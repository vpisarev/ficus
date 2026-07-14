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

type hashentry_t[K, D] = {hv: hash_t; key: K; data: D}
type index_t = int []

class t[K, D]
{
    default_entry: hashentry_t[K, D]
    var nactive: int
    var tabsz: int
    var free: int
    var index: index_t
    var table: hashentry_t[K, D] []
}

fun makeindex(size: int): int [] = array(size, 0)

fun empty[K, D](size0: int, k0: K, d0: D): Hashmap.t[K, D]
{
    var size = 8
    while size < size0 { size *= 2 }
    val idxsize = size*2
    val entry0 = hashentry_t {hv=0u64, key=k0, data=d0}
    Hashmap.t {
        default_entry=entry0,
        nactive=0, tabsz=0, free=0,
        index = makeindex(idxsize),
        table=array(size, entry0) }
}

fun empty[K, D](hm: Hashmap.t[K, D]): bool = hm.nactive == 0
fun size[K, D](hm: Hashmap.t[K, D]): int = hm.nactive

fun clear[K, D](hm: Hashmap.t[K, D]): void {
    val entry0 = hm.default_entry
    val table = hm.table
    for i <- 0:size(table) {
        table[i] = entry0
    }
    hm.nactive = 0
    hm.tabsz = 0
    hm.free = 0
    hm.index = makeindex(size(table)*2)
}

fun copy[K, D](hm: Hashmap.t[K, D]): Hashmap.t[K, D] =
    Hashmap.t {
        default_entry=hm.default_entry, nactive=hm.nactive,
        tabsz=hm.tabsz, free = hm.free, index=copy(hm.index),
        table=copy(hm.table) }

@private fun add_fast_[K, D](tabsz: int, ht_index: index_t,
                    ht_table: hashentry_t[K, D] [],
                    entry: hashentry_t[K, D]): int
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

@private fun grow[K, D](hm: Hashmap.t[K, D], new_size: int): void
{
    //println(f"started growing to {new_size}")
    val ht_table = hm.table
    val new_ht_table = array(new_size, hm.default_entry)
    val new_ht_index = makeindex(new_size*2)
    var tabsz = 0

    for j <- 0:hm.tabsz {
        if ht_table[j].hv < HASH_SIGN_MASK {
            tabsz = add_fast_(tabsz, new_ht_index, new_ht_table, ht_table[j])
        }
    }
    hm.index = new_ht_index
    hm.table = new_ht_table
    hm.tabsz = tabsz
    hm.free = 0
    //println(f"finished growing to {new_size}")
}

@private fun find_idx_[K, D](hm: Hashmap.t[K, D], k: K): (int, int)
{
    val hv = hash(k) & ~HASH_SIGN_MASK
    val idxsz = size(hm.index)
    var perturb = hv, found = -1
    var j = int(hv) & (idxsz - 1)
    for i <- 0:idxsz+14 {
        val tidx = hm.index[j]
        if tidx >= HASH_ALIVE {
            val entry = hm.table[tidx - HASH_ALIVE]
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

fun find_idx[K, D](hm: Hashmap.t[K, D], k: K): int = hm.find_idx_(k).1
fun mem[K, D](hm: Hashmap.t[K, D], k: K): bool = hm.find_idx_(k).1 >= 0
fun find_opt[K, D](hm: Hashmap.t[K, D], k: K): D?
{
    val j = hm.find_idx_(k).1
    if j >= 0 { Some(hm.table[j].data) } else { None }
}

fun check_free[K, D](hm: Hashmap.t[K, D]): void
{
    var free = hm.free
    var count = 0
    print("free list: [")
    while free > 0 {
        count += 1
        val tidx = free - 1
        print(f" {tidx}")
        assert(0 <= tidx < hm.tabsz)
        free = int(hm.table[tidx].hv & ~HASH_SIGN_MASK)
    }
    println(f"]\nfree list is ok, {count} elements")
}

/*var find_idx_stat = 0
var find_idx_stat_ncalls = 0
fun print_and_reset()
{
    println(f"avg iters={double(find_idx_stat)/find_idx_stat_ncalls}")
    find_idx_stat = 0
    find_idx_stat_ncalls = 0
}*/

fun find_idx_or_insert[K, D](hm: Hashmap.t[K, D], k: K): int
{
    //find_idx_stat_ncalls += 1
    val hv = hash(k) & ~HASH_SIGN_MASK
    var idxsz = size(hm.index)

    //println("============================================================")
    //println(f"new_nelems: {hm.nactive+1}, free: {hm.free-1}, tabsz: {hm.tabsz}, idxsize: {size(hm.index)}")
    if hm.nactive + 1 > (idxsz >> 1) {
        while idxsz < (hm.nactive + 1)*2 { idxsz *= 2 }
        hm.grow(max(idxsz/2, hm.nactive + 1))
    }

    //hm.check_free()
    var perturb = hv, found = -1, insert_idx = -1
    var j = int(hv) & (idxsz - 1)
    //var hash_iters = 0

    for i <- 0:idxsz+14 {
        //hash_iters += 1
        val tidx = hm.index[j]
        //println(f"j={j}, tidx={tidx}")
        if tidx >= HASH_ALIVE {
            val entry = hm.table[tidx - HASH_ALIVE]
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
    //find_idx_stat += hash_iters
    if found >= 0 {
        //println("got here (found)")
        if insert_idx >= 0 && insert_idx != j {
            hm.index[insert_idx] = found + HASH_ALIVE
            hm.index[j] = HASH_DELETED
        }
    } else if insert_idx >= 0 {
        found = hm.free-1
        //println(f"got here (not found); insert_idx={insert_idx}, found={hm.free-1}")
        if found >= 0 {
            //println(f"before & ~HASH_SIGN_MASK: {hm.table[found].hv}")
            hm.free = int(hm.table[found].hv & ~HASH_SIGN_MASK)
            //println(f"new_free: {hm.free-1}")
        } else {
            found = hm.tabsz
            hm.tabsz += 1
            assert(found < size(hm.table))
        }
        hm.table[found] = hashentry_t {hv=hv, key=k, data=hm.default_entry.data}
        hm.index[insert_idx] = found + HASH_ALIVE
        hm.nactive += 1
    } else {
        throw Fail("cannot insert element (full Hashtable?!)")
    }
    found
}

fun add[K, D](hm: Hashmap.t[K, D], k: K, d: D): void
{
    val idx = hm.find_idx_or_insert(k)
    hm.table[idx].data = d
}

fun remove[K, D](hm: Hashmap.t[K, D], k: K): void {
    val (j, tidx) = hm.find_idx_(k)
    if tidx >= 0 {
        hm.index[j] = HASH_DELETED
        hm.table[tidx].hv = uint64(hm.free) | HASH_SIGN_MASK
        hm.free = tidx+1
        //hm.check_free()
        hm.nactive -= 1
    }
}

fun list[K, D](hm: Hashmap.t[K, D]): list[K, D] =
    [:: for j <- 0:hm.tabsz {
        if hm.table[j].hv >= HASH_SIGN_MASK { continue }
        val entry = hm.table[j]
        (entry.key, entry.data)
    }]

fun add_list[K, D](hm: Hashmap.t[K, D], data: list[K, D]): void
{
    var datasz = hm.nactive + data.length()
    var curr_size = size(hm.table), new_size = curr_size
    while new_size < datasz { new_size *= 2 }
    if new_size > curr_size { hm.grow(new_size) }
    for (k, d) <- data { hm.add(k, d) }
}

fun from_list[K, D](k0: K, d0: D, data: list[K, D]): Hashmap.t[K, D]
{
    val ht = empty(data.length(), k0, d0)
    ht.add_list(data)
    ht
}

fun app[K, D](hm: Hashmap.t[K, D], f: (K, D)->void): void {
    val table = hm.table
    for j <- 0:hm.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            val entry = table[j]
            f(entry.key, entry.data)
        }
    }
}

fun foldl[K, D, Tr](hm: Hashmap.t[K, D], f: (K, D, Tr)->Tr, res0: Tr): Tr {
    val table = hm.table
    var res = res0
    for j <- 0:hm.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            val entry = table[j]
            res = f(entry.key, entry.data, res)
        }
    }
    res
}

fun filter[K, D](hm: Hashmap.t[K, D], f: (K, D)->bool): void
{
    val idxsz = size(hm.index)
    val table = hm.table
    val key0 = hm.default_entry.key
    val data0 = hm.default_entry.data
    for j <- 0:idxsz {
        val tidx = hm.index[j]
        if tidx < HASH_ALIVE { continue }
        val (key,value) = table[tidx - HASH_ALIVE].key
        if !f(key,value) {
            hm.index[j] = HASH_DELETED
            table[tidx - HASH_ALIVE] = hashentry_t {
                hv = uint64(hm.free) | HASH_SIGN_MASK, key=key0, data = data0}
            hm.free = tidx + 1
            hm.nactive -= 1
        }
    }
}
