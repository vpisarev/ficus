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

type 'k hashset_entry_t = {hv: hash_t; key: 'k}

type 'k hashset_t =
{
    hash_f: 'k -> hash_t
    default_entry: 'k hashset_entry_t
    nelems: int
    nremoved: int
    table: 'k hashset_entry_t []
}

object type 'k t =
{
    r: 'k hashset_t ref
}

fun empty(size0: int, k0: 'k, f: 'k->hash_t ): 'k Hashset.t
{
    var size = 8
    while size < size0 { size *= 2 }
    val entry0 = hashset_entry_t {hv=HASH_EMPTY, key=k0}
    Hashset.t { r = ref (hashset_t {
        hash_f=f, default_entry=entry0,
        nelems=0, nremoved=0,
        table=array(size, entry0) })}
}

fun empty(hs: 'k Hashset.t): bool = hs.r->nelems == 0

fun size(hs: 'k Hashset.t) = hs.r->nelems

fun clear(hs: 'k Hashset.t) {
    val table = hs.r->table
    val entry0 = hs.r->default_entry
    for i <- 0:size(table) {
        table[i] = entry0
    }
    hs.r->nelems = 0
    hs.r->nremoved = 0
}

fun copy(hs: 'k Hashset.t): 'k Hashset.t =
    t { r = ref (hashset_t {
        hash_f=hs.r->hash_f, default_entry=hs.r->default_entry,
        nelems=hs.r->nelems, nremoved=hs.r->nremoved,
        table=copy(hs.r->table)})}

@private fun add_(hs: 'k Hashset.t, hs_table: 'k hashset_entry_t [], entry: 'k hashset_entry_t): (int, int)
{
    val tabsz = size(hs_table)
    val hv = entry.hv
    var perturb = hv, delta_nelems = -1, delta_nremoved = 0
    var j = int(hv) & (tabsz - 1), insert_pos = -1

    for i <- 0:tabsz+14 {
        val {hv=hvj, key=kj} = hs_table[j]
        if hvj == hv {
            if kj == entry.key {
                if insert_pos >= 0 {
                    hs_table[insert_pos] = entry
                    hs_table[j] = hs.r->default_entry
                }
                delta_nelems = 0
                break
            }
        } else if hvj == HASH_EMPTY {
            hs_table[(if insert_pos >= 0 {insert_pos} else {j})] = entry
            delta_nelems = 1
            break
        } else if hvj == HASH_DELETED && insert_pos < 0 {
            insert_pos = j
            delta_nremoved = -1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if delta_nelems < 0 {
        if insert_pos >= 0 {
            hs_table[insert_pos] = entry
            delta_nelems = 1
        } else {
            throw Fail("can-not insert element into half-empty Hashtable (?!)")
        }
    }
    (delta_nelems, delta_nremoved)
}

@private fun grow(hs: 'k Hashset.t, new_size: int): void
{
    val hs_table = hs.r->table
    val curr_size = size(hs_table)
    val new_ht_table = array(new_size, hs.r->default_entry)
    for j <- 0:curr_size {
        if hs_table[j].hv > HASH_DELETED {
            ignore(add_(hs, new_ht_table, hs_table[j]))
        }
    }
    hs.r->table = new_ht_table
    hs.r->nremoved = 0
}

fun find_idx(hs: 'k Hashset.t, k: 'k): int
{
    var hv = hs.r->hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    val tabsz = size(hs.r->table)
    var perturb = hv, found = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val {hv=hvj, key=kj} = hs.r->table[j]
        if hvj == hv {
            if kj == k {
                found = j
                break
            }
        } else if hvj == HASH_EMPTY { break }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    found
}

fun mem(hs: 'k Hashset.t, k: 'k): bool = find_idx(hs, k) >= 0

fun add(hs: 'k Hashset.t, k: 'k)
{
    var hv = hs.r->hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    var tabsz = size(hs.r->table)

    if hs.r->nelems + hs.r->nremoved >= (tabsz >> 1) {
        while tabsz <= (hs.r->nelems + hs.r->nremoved)*2 { tabsz *= 2 }
        grow(hs, tabsz)
    }

    var perturb = hv, found = -1, insert_pos = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val {hv=hvj, key=kj} = hs.r->table[j]
        if hvj == hv {
            if kj == k {
                found = j
                break
            }
        } else if hvj == HASH_EMPTY {
            if insert_pos < 0 { insert_pos = j }
            break
        } else if hvj == HASH_DELETED && insert_pos < 0 {
            insert_pos = j
            hs.r->nremoved -= 1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if found >= 0 {
        if insert_pos >= 0 {
            hs.r->table[insert_pos] = hs.r->table[found]
            hs.r->table[found] = hs.r->default_entry
        }
    }
    else if insert_pos >= 0 {
        hs.r->table[insert_pos] = hashset_entry_t {hv=hv, key=k}
        hs.r->nelems += 1
    } else {
        throw Fail("can-not insert element into half-empty Hashtable (?!)")
    }
}

fun remove(hs: 'k Hashset.t, k: 'k) {
    val idx = find_idx(hs, k)
    if idx >= 0 {
        hs.r->table[idx] = hs.r->default_entry
        hs.r->nelems -= 1
        hs.r->nremoved += 1
    }
}

fun list(hs: 'k Hashset.t): 'k list =
    [: for j <- 0:size(hs.r->table) {
        if hs.r->table[j].hv <= HASH_DELETED { continue }
        hs.r->table[j].key
    } :]

fun add_list(hs: 'k Hashset.t, data: 'k list)
{
    var datasz = hs.r->nelems + hs.r->nremoved + data.length()
    var curr_size = size(hs.r->table), new_size = curr_size
    while new_size <= datasz*2 { new_size *= 2 }
    if new_size > curr_size { grow(hs, new_size) }
    for k <- data { add(hs, k) }
}

fun from_list(k0: 'k, hash_f: 'k->hash_t, data: 'k list): 'k Hashset.t
{
    val hs = empty(data.length()*2, k0, hash_f)
    add_list(hs, data)
    hs
}

fun app(hs: 'k Hashset.t, f: 'k->void)
{
    val table = hs.r->table
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            f(table[j].key)
        }
    }
}

fun foldl(hs: 'k Hashset.t, f: ('k, 'r)->'r, res0: 'r): 'r
{
    val table = hs.r->table
    var res = res0
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            res = f(table[j].key, res)
        }
    }
    res
}

fun union(a: 'k Hashset.t, b: 'k Hashset.t): void
{
    val table = b.r->table
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            // [TODO] can reuse already computed hash value
            a.add(table[j].key)
        }
    }
}

fun all(hs: 'k Hashset.t, f: 'k->bool): bool
{
    val table = hs.r->table
    var ok = true
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED && !f(table[j].key) {
            ok = false; break
        }
    }
    ok
}

fun exists(hs: 'k Hashset.t, f: 'k->bool): bool
{
    val table = hs.r->table
    var ok = false
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED && f(table[j].key) {
            ok = true; break
        }
    }
    ok
}