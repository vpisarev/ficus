/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* mutable hash table

   the implementation is influenced by CPython's dictobject implementation.
   See https://github.com/python/cpython/blob/master/Objects/dictobject.c
*/

val HASH_EMPTY = 0
val HASH_DELETED = 1
val HASH_ALIVE = 2
val HASH_SIGN_MASK: hash_t = 9223372036854775808u64
val PERTURB_SHIFT = 5

type hashset_entry_t[K] = {hv: hash_t; key: K}
type index_t = int []

class t[K]
{
    default_entry: hashset_entry_t[K]
    var nactive: int
    var tabsz: int
    var free: int
    var index: index_t
    var table: hashset_entry_t[K] []
}

fun makeindex(size: int): int [] = array(size, 0)

fun empty[K](size0: int, k0: K): Hashset.t[K]
{
    var size = 8
    while size < size0 { size *= 2 }
    val idxsize = size*2
    val entry0 = hashset_entry_t {hv=0u64, key=k0}
    Hashset.t {
        default_entry=entry0,
        nactive=0, tabsz=0, free=0,
        index = makeindex(idxsize),
        table=array(size, entry0) }
}

fun empty[K](hs: Hashset.t[K]): bool = hs.nactive == 0
fun size[K](hs: Hashset.t[K]): int = hs.nactive

fun clear[K](hs: Hashset.t[K]): void {
    val entry0 = hs.default_entry
    val table = hs.table
    for i <- 0:size(table) {
        table[i] = entry0
    }
    hs.nactive = 0
    hs.tabsz = 0
    hs.free = 0
    hs.index = makeindex(size(table)*2)
}

fun copy[K](hs: Hashset.t[K]): Hashset.t[K] =
    Hashset.t {
        default_entry=hs.default_entry, nactive=hs.nactive,
        tabsz=hs.tabsz, free = hs.free, index=copy(hs.index),
        table=copy(hs.table) }

fun compress[K](hs: Hashset.t[K]): Hashset.t[K]
{
    val nactive = hs.nactive
    val result = empty(nactive, hs.default_entry.key)
    val table = hs.table
    for i <- 0:hs.tabsz {
        if table[i].hv < HASH_SIGN_MASK {
            result.add_(table[i].key, table[i].hv)
        }
    }
    result
}

@private fun add_fast_[K](tabsz: int, ht_index: index_t,
                    ht_table: hashset_entry_t[K] [],
                    entry: hashset_entry_t[K]): int
{
    val idxsz = size(ht_index)
    val hv = entry.hv
    var perturb = hv, j = int(hv) & (idxsz - 1)
    var found_free_slot = false

    for i <- 0:idxsz+14 {
        val tidx = ht_index[j]
        if tidx == HASH_EMPTY {
            ht_index[j] = tabsz+HASH_ALIVE
            ht_table[tabsz] = entry
            found_free_slot = true
            break
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (idxsz - 1)
    }
    if !found_free_slot {
        throw Fail("cannot insert element (full Hashtable?!)")
    }
    tabsz + 1
}

@private fun grow[K](hs: Hashset.t[K], new_size: int): void
{
    //println(f"started growing to {new_size}")
    val ht_table = hs.table
    val new_ht_table = array(new_size, hs.default_entry)
    val new_ht_index = makeindex(new_size*2)
    var tabsz = 0

    for j <- 0:hs.tabsz {
        if ht_table[j].hv < HASH_SIGN_MASK {
            tabsz = add_fast_(tabsz, new_ht_index, new_ht_table, ht_table[j])
        }
    }
    hs.index = new_ht_index
    hs.table = new_ht_table
    hs.tabsz = tabsz
    hs.free = 0
    //println(f"finished growing to {new_size}")
}

@private fun find_idx_[K](hs: Hashset.t[K], k: K, hv: hash_t): (int, int)
{
    val idxsz = size(hs.index)
    var perturb = hv, found = -1
    var j = int(hv) & (idxsz - 1)
    for i <- 0:idxsz+14 {
        val tidx = hs.index[j]
        if tidx >= HASH_ALIVE {
            val entry = hs.table[tidx - HASH_ALIVE]
            if entry.hv == hv && entry.key == k {
                found = tidx - HASH_ALIVE
                break
            }
        } else if tidx == HASH_EMPTY { break }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (idxsz - 1)
    }
    (j, found)
}

fun find_idx[K](hs: Hashset.t[K], k: K): int = hs.find_idx_(k, hash(k) & ~HASH_SIGN_MASK).1
fun mem[K](hs: Hashset.t[K], k: K): bool = hs.find_idx_(k, hash(k) & ~HASH_SIGN_MASK).1 >= 0

fun check_free[K](hs: Hashset.t[K]): void
{
    var free = hs.free
    var count = 0
    print("free list: [")
    while free > 0 {
        count += 1
        val tidx = free - 1
        print(f" {tidx}")
        assert(0 <= tidx < hs.tabsz)
        free = int(hs.table[tidx].hv & ~HASH_SIGN_MASK)
    }
    println(f"]\nfree list is ok, {count} elements")
}

@private fun add_[K](hs: Hashset.t[K], k: K, hv: hash_t): void
{
    var idxsz = size(hs.index)

    if hs.nactive + 1 > (idxsz >> 1) {
        while idxsz < (hs.nactive + 1)*2 { idxsz *= 2 }
        hs.grow(max(idxsz/2, hs.nactive + 1))
    }

    //hs.check_free()
    var perturb = hv, found = -1, insert_idx = -1
    var j = int(hv) & (idxsz - 1)
    for i <- 0:idxsz+14 {
        val tidx = hs.index[j]
        if tidx >= HASH_ALIVE {
            val entry = hs.table[tidx - HASH_ALIVE]
            if entry.hv == hv && entry.key == k {
                found = tidx - HASH_ALIVE
                break
            }
        } else if tidx == HASH_EMPTY {
            if insert_idx < 0 {insert_idx = j}
            break
        } else if tidx == HASH_DELETED && insert_idx < 0 {
            insert_idx = j
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (idxsz - 1)
    }
    if found >= 0 {
        if insert_idx >= 0 && insert_idx != j {
            hs.index[insert_idx] = found + HASH_ALIVE
            hs.index[j] = HASH_DELETED
        }
    } else if insert_idx >= 0 {
        found = hs.free-1
        if found >= 0 {
            hs.free = int(hs.table[found].hv & ~HASH_SIGN_MASK)
        } else {
            found = hs.tabsz
            hs.tabsz += 1
            assert(found < size(hs.table))
        }
        hs.table[found] = hashset_entry_t {hv=hv, key=k}
        hs.index[insert_idx] = found + HASH_ALIVE
        hs.nactive += 1
    } else {
        throw Fail("cannot insert element (full Hashtable?!)")
    }
}

fun add[K](hs: Hashset.t[K], k: K): void = hs.add_(k, hash(k) & ~HASH_SIGN_MASK)

fun remove[K](hs: Hashset.t[K], k: K): void {
    val (j, tidx) = hs.find_idx_(k, hash(k) & ~HASH_SIGN_MASK)
    if tidx >= 0 {
        hs.index[j] = HASH_DELETED
        hs.table[tidx].hv = uint64(hs.free) | HASH_SIGN_MASK
        hs.free = tidx+1
        hs.nactive -= 1
    }
}

fun list[K](hs: Hashset.t[K]): list[K] =
    [:: for j <- 0:hs.tabsz {
        if hs.table[j].hv >= HASH_SIGN_MASK { continue }
        hs.table[j].key
    }]

fun array[K](hs: Hashset.t[K]): K []
{
    val result = array(hs.nactive, hs.default_entry.key)
    var k = 0
    for j <- 0:hs.tabsz {
        if hs.table[j].hv >= HASH_SIGN_MASK { continue }
        result[k] = hs.table[j].key
        k += 1
    }
    result
}

fun add_list[K](hs: Hashset.t[K], data: list[K]): void
{
    var datasz = hs.nactive + data.length()
    var curr_size = size(hs.table), new_size = curr_size
    while new_size < datasz { new_size *= 2 }
    if new_size > curr_size { hs.grow(new_size) }
    for k <- data { hs.add(k) }
}

fun from_list[K](k0: K, data: list[K]): Hashset.t[K]
{
    val ht = empty(data.length(), k0)
    ht.add_list(data)
    ht
}

fun app[K](hs: Hashset.t[K], f: K->void): void {
    val table = hs.table
    for j <- 0:hs.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            f(table[j].key)
        }
    }
}

fun foldl[K, Tr](hs: Hashset.t[K], f: (K, Tr)->Tr, res0: Tr): Tr {
    val table = hs.table
    var res = res0
    for j <- 0:hs.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            res = f(table[j].key, res)
        }
    }
    res
}

fun union[K](hs: Hashset.t[K], b: Hashset.t[K]): void
{
    val table = b.table
    for j <- 0:b.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            hs.add_(table[j].key, table[j].hv)
        }
    }
}

fun intersect[K](hs: Hashset.t[K], b: Hashset.t[K]): void
{
    val table = hs.table
    val tabsz = hs.tabsz
    val tmp = empty(min(size(hs.table), size(b.table)), hs.default_entry.key)
    hs.table = tmp.table
    hs.index = tmp.index
    hs.nactive = 0
    hs.tabsz = 0
    hs.free = 0
    for j <- 0:tabsz {
        if table[j].hv < HASH_SIGN_MASK && b.find_idx_(table[j].key, table[j].hv).1 >= 0 {
            hs.add_(table[j].key, table[j].hv)
        }
    }
}

fun diff[K](hs: Hashset.t[K], b: Hashset.t[K]): void
{
    val table = b.table
    for j <- 0:b.tabsz {
        val {hv, key} = table[j]
        if hv < HASH_SIGN_MASK {
            val (j, tidx) = hs.find_idx_(key, hv)
            if tidx >= 0 {
                hs.index[j] = HASH_DELETED
                hs.table[tidx].hv = uint64(hs.free) | HASH_SIGN_MASK
                hs.free = tidx+1
                hs.nactive -= 1
            }
        }
    }
}

fun all[K](hs: Hashset.t[K], f: K->bool): bool
{
    val table = hs.table
    var ok = true
    for j <- 0:hs.tabsz {
        if table[j].hv < HASH_SIGN_MASK && !f(table[j].key) {
            ok = false; break
        }
    }
    ok
}

fun exists[K](hs: Hashset.t[K], f: K->bool): bool
{
    val table = hs.table
    var ok = false
    for j <- 0:hs.tabsz {
        if table[j].hv < HASH_SIGN_MASK && f(table[j].key) {
            ok = true; break
        }
    }
    ok
}

fun filter[K](hs: Hashset.t[K], f: K->bool): void
{
    val idxsz = size(hs.index)
    val table = hs.table
    val key0 = hs.default_entry.key
    for j <- 0:idxsz {
        val tidx = hs.index[j]
        if tidx >= HASH_ALIVE && !f(table[tidx - HASH_ALIVE].key) {
            hs.index[j] = HASH_DELETED
            table[tidx - HASH_ALIVE] = hashset_entry_t {
                hv = uint64(hs.free) | HASH_SIGN_MASK, key=key0}
            hs.free = tidx + 1
            hs.nactive -= 1
        }
    }
}
