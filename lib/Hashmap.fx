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

object type ('k, 'd) t =
{
    hash_f: 'k -> hash_t
    k0: 'k
    d0: 'd
    _state: ('k, 'd) hash_state_t ref
}

type ('k, 'd) hash_state_t =
{
    nelems: int
    nremoved: int
    table: (hash_t, 'k, 'd) []
}

fun empty(size0: int, k0: 'k, d0: 'd, f: 'k->hash_t): ('k, 'd) Hashmap.t
{
    var size = 8
    while size < size0 { size *= 2 }
    val state = hash_state_t {
        nelems=0, nremoved=0, table=array(size, (HASH_EMPTY, k0, d0)) }
    Hashmap.t { hash_f=f, k0=k0, d0=d0, _state=ref state }
}

fun empty(ht: ('k, 'd) Hashmap.t): bool = ht._state->nkeys == 0
fun size(ht: ('k, 'd) Hashmap.t) = ht._state->nelems

fun clear(ht: 'k Hashmap.t) {
    val k0 = ht.k0, d0 = ht.d0
    val table = ht._state->table
    for i <- 0:size(table) {
        table[i] = (HASH_EMPTY, k0, d0)
    }
    ht._state->nelems = 0
    ht._state->nremoved = 0
}

fun copy(ht: ('k, 'd) Hashmap.t): ('k, 'd) Hashmap.t =
    t {hash_f=ht.hash_f, k0=ht.k0, d0=ht.d0, _state=ref (hash_state_t {
        nelems=ht._state->nelems, nremoved=ht._state->nremoved,
        table=copy(ht._state->table)})}

@private fun add_(ht: ('k, 'd) Hashmap.t, ht_table: (hash_t, 'k, 'd) [], (hv: hash_t, k: 'k, d: 'd)): (int, int) {
    val tabsz = size(ht_table)
    var perturb = hv, delta_nelems = -1, delta_nremoved = 0
    var j = int(hv) & (tabsz - 1), insert_pos = -1

    for i <- 0:tabsz+14 {
        val (hvj, kj, _) = ht_table[j]
        if hvj == hv {
            if kj == k {
                if insert_pos >= 0 {
                    ht_table[insert_pos] = (hv, k, d)
                    ht_table[j] = (HASH_EMPTY, ht.k0, ht.d0)
                } else {
                    ht_table[j].2 = d
                }
                delta_nelems = 0
                break
            }
        } else if hvj == HASH_EMPTY {
            ht_table[(if insert_pos >= 0 {insert_pos} else {j})] = (hv, k, d)
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
            ht_table[insert_pos] = (hv, k, d)
            delta_nelems = 1
        } else {
            throw Fail("can-not insert element into half-empty Hashtable (?!)")
        }
    }
    (delta_nelems, delta_nremoved)
}

@private fun grow(ht: ('k, 'd) Hashmap.t, new_size: int): void
{
    val ht_table = ht._state->table
    val curr_size = size(ht_table)
    val new_ht_table = array(new_size, (HASH_EMPTY, ht.k0, ht.d0))
    for j <- 0:curr_size {
        if ht_table[j].0 > HASH_DELETED {
            ignore(add_(ht, new_ht_table, ht_table[j]))
        }
    }
    ht._state->table = new_ht_table
    ht._state->nremoved = 0
}

fun find_idx(ht: ('k, 'd) Hashmap.t, k: 'k): int
{
    var hv = ht.hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    val tabsz = size(ht._state->table)
    var perturb = hv, found = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val (hvj, kj, _) = ht._state->table[j]
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

fun mem(ht: ('k, 'd) Hashmap.t, k: 'k): bool = find_idx(ht, k) >= 0
fun find_opt(ht: ('k, 'd) Hashmap.t, k: 'k): 'd?
{
    val j = find_idx(ht, k)
    if j >= 0 { Some(ht._state->table[j].2) } else { None }
}

fun find_idx_or_insert(ht: ('k, 'd) Hashmap.t, k: 'k): int
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
        val (hvj, kj, _) = ht._state->table[j]
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
        if insert_pos < 0 {
            found
        } else {
            ht._state->table[insert_pos] = ht._state->table[found]
            ht._state->table[found] = (HASH_EMPTY, ht.k0, ht.d0)
            insert_pos
        }
    }
    else if insert_pos >= 0 {
        ht._state->table[insert_pos] = (hv, k, ht.d0)
        ht._state->nelems += 1
        insert_pos
    } else {
        throw Fail("can-not insert element into half-empty Hashtable (?!)")
    }
}

fun add(ht: ('k, 'd) Hashmap.t, k: 'k, d: 'd): void
{
    val idx = find_idx_or_insert(ht, k)
    ht._state->table[idx].2 = d
}

fun remove(ht: ('k, 'd) Hashmap.t, k: 'k) {
    val idx = find_idx(ht, k)
    if idx >= 0 {
        ht._state->table[idx] = (HASH_DELETED, ht.k0, ht.d0)
        ht._state->nelems -= 1
        ht._state->nremoved += 1
    }
}

fun list(ht: ('k, 'd) Hashmap.t): ('k, 'd) list =
    [: for j <- 0:size(ht._state->table) {
        if ht._state->table[j].0 <= HASH_DELETED { continue }
        val (_, kj, dj) = ht._state->table[j]
        (kj, dj)
    } :]

fun add_list(ht: ('k, 'd) Hashmap.t, data: ('k, 'd) list)
{
    var datasz = ht._state->nelems + ht._state->nremoved + data.length()
    var curr_size = size(ht._state->table), new_size = curr_size
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
    val table = ht._state->table
    for j <- 0:size(table) {
        if table[j].0 > HASH_DELETED {
            val (_, kj, dj) = table[j]
            f(kj, dj)
        }
    }
}

fun foldl(ht: ('k, 'd) Hashmap.t, f: ('k, 'd, 'r)->'r, res0: 'r): 'r {
    val table = ht._state->table
    var res = res0
    for j <- 0:size(table) {
        if table[j].0 > HASH_DELETED {
            val (_, kj, dj) = table[j]
            res = f(kj, dj, res)
        }
    }
    res
}
