/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* mutable hash table

   the implementation is influenced by CPython's dictobject implementation.
   See https://github.com/python/cpython/blob/master/Objects/dictobject.c
*/

val HASH_EMPTY: hash_t = 0UL
val HASH_DELETED: hash_t = 1UL
val PERTURB_SHIFT = 5

type ('k, 'd) hashentry_t = {hv: hash_t; key: 'k; data: 'd}

object type ('k, 'd) t =
{
    hash_f: 'k -> hash_t
    default_entry: ('k, 'd) hashentry_t
    var nelems: int
    var nremoved: int
    var table: ('k, 'd) hashentry_t []
}

fun empty(size0: int, k0: 'k, d0: 'd, f: 'k->hash_t): ('k, 'd) Hashmap.t
{
    var size = 8
    while size < size0 { size *= 2 }
    val entry0 = hashentry_t {hv=HASH_EMPTY, key=k0, data=d0}
    Hashmap.t {
        hash_f=f, default_entry=entry0,
        nelems=0, nremoved=0,
        table=array(size, entry0) }
}

fun empty(ht: ('k, 'd) Hashmap.t): bool = ht.nelems == 0
fun size(ht: ('k, 'd) Hashmap.t) = ht.nelems

fun clear(ht: 'k Hashmap.t) {
    val entry0 = ht.default_entry
    val table = ht.table
    for i <- 0:size(table) {
        table[i] = entry0
    }
    ht.nelems = 0
    ht.nremoved = 0
}

fun copy(ht: ('k, 'd) Hashmap.t): ('k, 'd) Hashmap.t =
    Hashmap.t {
        hash_f=ht.hash_f, default_entry=ht.default_entry,
        nelems=ht.nelems, nremoved=ht.nremoved,
        table=copy(ht.table) }

@private fun add_(ht: ('k, 'd) Hashmap.t, ht_table: ('k, 'd) hashentry_t [],
                 entry: ('k, 'd) hashentry_t): (int, int) {
    val tabsz = size(ht_table)
    val hv = entry.hv
    var perturb = hv, delta_nelems = -1, delta_nremoved = 0
    var j = int(hv) & (tabsz - 1), insert_idx = -1

    for i <- 0:tabsz+14 {
        val {hv=hvj, key=kj} as entry_j = ht_table[j]
        if hvj == hv {
            if kj == entry.key {
                if insert_idx >= 0 {
                    ht_table[insert_idx] = entry
                    ht_table[j] = ht.default_entry
                } else {
                    ht_table[j].data = entry.data
                }
                delta_nelems = 0
                break
            }
        } else if hvj == HASH_EMPTY {
            val idx = if insert_idx >= 0 {insert_idx} else {j}
            ht_table[idx] = entry
            delta_nelems = 1
            break
        } else if hvj == HASH_DELETED && insert_idx < 0 {
            insert_idx = j
            delta_nremoved = -1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if delta_nelems < 0 {
        if insert_idx >= 0 {
            ht_table[insert_idx] = entry
            delta_nelems = 1
        } else {
            throw Fail("can-not insert element into half-empty Hashtable (?!)")
        }
    }
    (delta_nelems, delta_nremoved)
}

@private fun grow(ht: ('k, 'd) Hashmap.t, new_size: int): void
{
    val ht_table = ht.table
    val curr_size = size(ht_table)
    val new_ht_table = array(new_size, ht.default_entry)
    for j <- 0:curr_size {
        if ht_table[j].hv > HASH_DELETED {
            ignore(add_(ht, new_ht_table, ht_table[j]))
        }
    }
    ht.table = new_ht_table
    ht.nremoved = 0
}

fun find_idx(ht: ('k, 'd) Hashmap.t, k: 'k): int
{
    var hv = ht.hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    val tabsz = size(ht.table)
    var perturb = hv, found = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val entry = ht.table[j]
        if entry.hv == hv {
            if entry.key == k {
                found = j
                break
            }
        } else if entry.hv == HASH_EMPTY { break }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    found
}

fun mem(ht: ('k, 'd) Hashmap.t, k: 'k): bool = find_idx(ht, k) >= 0
fun find_opt(ht: ('k, 'd) Hashmap.t, k: 'k): 'd?
{
    val j = find_idx(ht, k)
    if j >= 0 { Some(ht.table[j].data) } else { None }
}

fun find_idx_or_insert(ht: ('k, 'd) Hashmap.t, k: 'k): int
{
    var hv = ht.hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    var tabsz = size(ht.table)

    if ht.nelems + ht.nremoved >= (tabsz >> 1) {
        while tabsz <= (ht.nelems + ht.nremoved)*2 { tabsz *= 2 }
        grow(ht, tabsz)
    }

    var perturb = hv, found = -1, insert_idx = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val entry = ht.table[j]
        if entry.hv == hv {
            if entry.key == k {
                found = j
                break
            }
        } else if entry.hv == HASH_EMPTY {
            if insert_idx < 0 { insert_idx = j }
            break
        } else if entry.hv == HASH_DELETED && insert_idx < 0 {
            insert_idx = j
            ht.nremoved -= 1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if found >= 0 {
        if insert_idx < 0 {
            found
        } else {
            ht.table[insert_idx] = ht.table[found]
            ht.table[found] = ht.default_entry
            insert_idx
        }
    }
    else if insert_idx >= 0 {
        ht.table[insert_idx] = hashentry_t {hv=hv, key=k, data=ht.default_entry.data}
        ht.nelems += 1
        insert_idx
    } else {
        throw Fail("can-not insert element into half-empty Hashtable (?!)")
    }
}

fun add(ht: ('k, 'd) Hashmap.t, k: 'k, d: 'd): void
{
    val idx = find_idx_or_insert(ht, k)
    ht.table[idx].data = d
}

fun remove(ht: ('k, 'd) Hashmap.t, k: 'k) {
    val idx = find_idx(ht, k)
    if idx >= 0 {
        ht.table[idx] = ht.default_entry
        ht.nelems -= 1
        ht.nremoved += 1
    }
}

fun list(ht: ('k, 'd) Hashmap.t): ('k, 'd) list =
    [: for j <- 0:size(ht.table) {
        if ht.table[j].hv <= HASH_DELETED { continue }
        val entry = ht.table[j]
        (entry.key, entry.data)
    } :]

fun add_list(ht: ('k, 'd) Hashmap.t, data: ('k, 'd) list)
{
    var datasz = ht.nelems + ht.nremoved + data.length()
    var curr_size = size(ht.table), new_size = curr_size
    while new_size <= datasz*2 { new_size *= 2 }
    if new_size > curr_size { grow(ht, new_size) }
    for (k, d) <- data { add(ht, k, d) }
}

fun from_list(k0: 'k, d0: 'd, hash_f: 'k->hash_t, data: ('k, 'd) list): ('k, 'd) Hashmap.t
{
    val ht = empty(data.length()*2, k0, d0, hash_f)
    add_list(ht, data)
    ht
}

fun app(ht: ('k, 'd) Hashmap.t, f: ('k, 'd)->void) {
    val table = ht.table
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            val entry = table[j]
            f(entry.key, entry.data)
        }
    }
}

fun foldl(ht: ('k, 'd) Hashmap.t, f: ('k, 'd, 'r)->'r, res0: 'r): 'r {
    val table = ht.table
    var res = res0
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            val entry = table[j]
            res = f(entry.key, entry.data, res)
        }
    }
    res
}
