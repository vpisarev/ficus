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

object type 'k t =
{
    hash_f: 'k -> hash_t
    k0: 'k
    _state: 'k hashset_state_t ref
}

type 'k hashset_state_t =
{
    nelems: int
    nremoved: int
    table: (hash_t, 'k) []
}

fun empty(size0: int, k0: 'k, f: 'k->hash_t ): 'k Hashset.t
{
    var size = 8
    while size < size0 { size *= 2 }
    val state = hashset_state_t {
        nelems=0, nremoved=0, table=array(size, (HASH_EMPTY, k0)) }
    Hashset.t { hash_f=f, k0=k0, _state=ref state }
}

fun empty(ht: 'k Hashset.t): bool = ht._state->nelems == 0

fun size(ht: 'k Hashset.t) = ht._state->nelems

fun clear(ht: 'k Hashset.t) {
    val k0 = ht.k0
    val table = ht._state->table
    for i <- 0:size(table) {
        table[i] = (HASH_EMPTY, k0)
    }
    ht._state->nelems = 0
    ht._state->nremoved = 0
}

fun copy(ht: 'k Hashset.t): 'k Hashset.t =
    t {hash_f=ht.hash_f, k0=ht.k0, _state=ref (hashset_state_t {
        nelems=ht._state->nelems, nremoved=ht._state->nremoved,
        table=copy(ht._state->table)})}

@private fun add_(ht: 'k Hashset.t, ht_table: (hash_t, 'k) [], (hv: hash_t, k: 'k)): (int, int) {
    val tabsz = size(ht_table)
    var perturb = hv, delta_nelems = -1, delta_nremoved = 0
    var j = int(hv) & (tabsz - 1), insert_pos = -1

    for i <- 0:tabsz+14 {
        val (hvj, kj) = ht_table[j]
        if hvj == hv {
            if kj == k {
                if insert_pos >= 0 {
                    ht_table[insert_pos] = (hv, k)
                    ht_table[j] = (HASH_EMPTY, ht.k0)
                }
                delta_nelems = 0
                break
            }
        } else if hvj == HASH_EMPTY {
            ht_table[(if insert_pos >= 0 {insert_pos} else {j})] = (hv, k)
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
            ht_table[insert_pos] = (hv, k)
            delta_nelems = 1
        } else {
            throw Fail("can-not insert element into half-empty Hashtable (?!)")
        }
    }
    (delta_nelems, delta_nremoved)
}

@private fun grow(ht: 'k Hashset.t, new_size: int): void
{
    val ht_table = ht._state->table
    val curr_size = size(ht_table)
    val new_ht_table = array(new_size, (HASH_EMPTY, ht.k0))
    for j <- 0:curr_size {
        if ht_table[j].0 > HASH_DELETED {
            ignore(add_(ht, new_ht_table, ht_table[j]))
        }
    }
    ht._state->table = new_ht_table
    ht._state->nremoved = 0
}

fun find_idx(ht: 'k Hashset.t, k: 'k): int
{
    var hv = ht.hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    val tabsz = size(ht._state->table)
    var perturb = hv, found = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val (hvj, kj) = ht._state->table[j]
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

fun mem(ht: 'k Hashset.t, k: 'k): bool = find_idx(ht, k) >= 0

fun add(ht: 'k Hashset.t, k: 'k)
{
    var hv = ht.hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    var tabsz = size(ht._state->table)

    if ht._state->nelems + ht._state->nremoved >= (tabsz >> 1) {
        while tabsz <= (ht._state->nelems + ht._state->nremoved)*2 { tabsz *= 2 }
        grow(ht, tabsz)
    }

    var perturb = hv, found = -1, insert_pos = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val (hvj, kj) = ht._state->table[j]
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
            ht._state->nremoved -= 1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if found >= 0 {
        if insert_pos >= 0 {
            ht._state->table[insert_pos] = ht._state->table[found]
            ht._state->table[found] = (HASH_EMPTY, ht.k0)
        }
    }
    else if insert_pos >= 0 {
        ht._state->table[insert_pos] = (hv, k)
        ht._state->nelems += 1
    } else {
        throw Fail("can-not insert element into half-empty Hashtable (?!)")
    }
}

fun remove(ht: 'k Hashset.t, k: 'k) {
    val idx = find_idx(ht, k)
    if idx >= 0 {
        ht._state->table[idx] = (HASH_DELETED, ht.k0)
        ht._state->nelems -= 1
        ht._state->nremoved += 1
    }
}

fun list(ht: 'k Hashset.t): 'k list =
    [: for j <- 0:size(ht._state->table) {
        if ht._state->table[j].0 <= HASH_DELETED { continue }
        ht._state->table[j].1
    } :]

fun add_list(ht: 'k Hashset.t, data: 'k list)
{
    var datasz = ht._state->nelems + ht._state->nremoved + data.length()
    var curr_size = size(ht._state->table), new_size = curr_size
    while new_size <= datasz*2 { new_size *= 2 }
    if new_size > curr_size { grow(ht, new_size) }
    for k <- data { add(ht, k) }
}

fun from_list(k0: 'k, hash_f: 'k->hash_t, data: 'k list): 'k Hashset.t
{
    val ht = empty(data.length()*2, k0, hash_f)
    add_list(ht, data)
    ht
}

fun app(ht: 'k Hashset.t, f: 'k->void)
{
    val table = ht._state->table
    for j <- 0:size(table) {
        if table[j].0 > HASH_DELETED {
            f(table[j].1)
        }
    }
}

fun foldl(ht: 'k Hashset.t, f: ('k, 'r)->'r, res0: 'r): 'r
{
    val table = ht._state->table
    var res = res0
    for j <- 0:size(table) {
        if table[j].0 > HASH_DELETED {
            res = f(table[j].1, res)
        }
    }
    res
}

fun union(a: 'k Hashset.t, b: 'k Hashset.t): void
{
    val table = b._state->table
    for j <- 0:size(table) {
        if table[j].0 > HASH_DELETED {
            // [TODO] can reuse already computed hash value
            a.add(table[j].1)
        }
    }
}

fun all(ht: 'k Hashset.t, f: 'k->bool): bool
{
    val table = ht._state->table
    var ok = true
    for j <- 0:size(table) {
        if table[j].0 > HASH_DELETED && !f(table[j].1) {
            ok = false; break
        }
    }
    ok
}

fun exists(ht: 'k Hashset.t, f: 'k->bool): bool
{
    val table = ht._state->table
    var ok = false
    for j <- 0:size(table) {
        if table[j].0 > HASH_DELETED && f(table[j].1) {
            ok = true; break
        }
    }
    ok
}