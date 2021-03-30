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

type ('k, 'd) hashmap_t =
{
    hash_f: 'k -> hash_t
    default_entry: ('k, 'd) hashentry_t
    nelems: int
    nremoved: int
    table: ('k, 'd) hashentry_t []
}

object type ('k, 'd) t =
{
    r: ('k, 'd) hashmap_t ref
}

fun empty(size0: int, k0: 'k, d0: 'd, f: 'k->hash_t): ('k, 'd) Hashmap.t
{
    var size = 8
    while size < size0 { size *= 2 }
    val entry0 = hashentry_t {hv=HASH_EMPTY, key=k0, data=d0}
    val state = hashmap_t {
        hash_f=f, default_entry=entry0,
        nelems=0, nremoved=0,
        table=array(size, entry0) }
    Hashmap.t { r = ref state }
}

fun empty(ht: ('k, 'd) Hashmap.t): bool = ht.r->nkeys == 0
fun size(ht: ('k, 'd) Hashmap.t) = ht.r->nelems

fun clear(ht: 'k Hashmap.t) {
    val entry0 = ht.r->default_entry
    val table = ht.r->table
    for i <- 0:size(table) {
        table[i] = entry0
    }
    ht.r->nelems = 0
    ht.r->nremoved = 0
}

fun copy(ht: ('k, 'd) Hashmap.t): ('k, 'd) Hashmap.t =
    t { r=ref (hashmap_t {
        hash_f=ht.r->hash_f, default_entry=ht.r->default_entry,
        nelems=ht.r->nelems, nremoved=ht.r->nremoved,
        table=copy(ht.r->table)})}

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
                    ht_table[j] = ht.r->default_entry
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
    val ht_table = ht.r->table
    val curr_size = size(ht_table)
    val new_ht_table = array(new_size, ht.r->default_entry)
    for j <- 0:curr_size {
        if ht_table[j].hv > HASH_DELETED {
            ignore(add_(ht, new_ht_table, ht_table[j]))
        }
    }
    ht.r->table = new_ht_table
    ht.r->nremoved = 0
}

fun find_idx(ht: ('k, 'd) Hashmap.t, k: 'k): int
{
    var hv = ht.r->hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    val tabsz = size(ht.r->table)
    var perturb = hv, found = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val entry = ht.r->table[j]
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
    if j >= 0 { Some(ht.r->table[j].data) } else { None }
}

fun find_idx_or_insert(ht: ('k, 'd) Hashmap.t, k: 'k): int
{
    var hv = ht.r->hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    var tabsz = size(ht.r->table)

    if ht.r->nelems + ht.r->nremoved >= (tabsz >> 1) {
        while tabsz <= (ht.r->nelems + ht.r->nremoved)*2 { tabsz *= 2 }
        grow(ht, tabsz)
    }

    var perturb = hv, found = -1, insert_idx = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val entry = ht.r->table[j]
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
            ht.r->nremoved -= 1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if found >= 0 {
        if insert_idx < 0 {
            found
        } else {
            ht.r->table[insert_idx] = ht.r->table[found]
            ht.r->table[found] = ht.r->default_entry
            insert_idx
        }
    }
    else if insert_idx >= 0 {
        ht.r->table[insert_idx] = hashentry_t {hv=hv, key=k, data=ht.r->default_entry.data}
        ht.r->nelems += 1
        insert_idx
    } else {
        throw Fail("can-not insert element into half-empty Hashtable (?!)")
    }
}

fun add(ht: ('k, 'd) Hashmap.t, k: 'k, d: 'd): void
{
    val idx = find_idx_or_insert(ht, k)
    ht.r->table[idx].data = d
}

fun remove(ht: ('k, 'd) Hashmap.t, k: 'k) {
    val idx = find_idx(ht, k)
    if idx >= 0 {
        ht.r->table[idx] = ht.r->default_entry
        ht.r->nelems -= 1
        ht.r->nremoved += 1
    }
}

fun list(ht: ('k, 'd) Hashmap.t): ('k, 'd) list =
    [: for j <- 0:size(ht.r->table) {
        if ht.r->table[j].hv <= HASH_DELETED { continue }
        val entry = ht.r->table[j]
        (entry.key, entry.data)
    } :]

fun add_list(ht: ('k, 'd) Hashmap.t, data: ('k, 'd) list)
{
    var datasz = ht.r->nelems + ht.r->nremoved + data.length()
    var curr_size = size(ht.r->table), new_size = curr_size
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
    val table = ht.r->table
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            val entry = table[j]
            f(entry.key, entry.data)
        }
    }
}

fun foldl(ht: ('k, 'd) Hashmap.t, f: ('k, 'd, 'r)->'r, res0: 'r): 'r {
    val table = ht.r->table
    var res = res0
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            val entry = table[j]
            res = f(entry.key, entry.data, res)
        }
    }
    res
}
