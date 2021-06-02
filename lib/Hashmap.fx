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
val HASH_SIGN_MASK: hash_t = 9223372036854775808UL
val PERTURB_SHIFT = 5

type ('k, 'd) hashentry_t = {hv: hash_t; key: 'k; data: 'd}
class index_t = IndexByte: uint8 [] | IndexWord: uint16 [] | IndexLarge: int []

class ('k, 'd) t
{
    default_entry: ('k, 'd) hashentry_t
    var nactive: int
    var tabsz: int
    var free: int
    var index: index_t
    var table: ('k, 'd) hashentry_t []
}

fun makeindex(size: int) =
    if size <= 256 {IndexByte(array(size, 0u8))}
    else if size <= 65536 {IndexWord(array(size, 0u16))}
    else {IndexLarge(array(size, 0))}

fun size(idx: index_t): int
{
    | IndexByte(tab) => size(tab)
    | IndexWord(tab) => size(tab)
    | IndexLarge(tab) => size(tab)
}

fun get(idx: index_t, i: int) =
    match idx {
    | IndexByte(tab) => int(tab[i])
    | IndexWord(tab) => int(tab[i])
    | IndexLarge(tab) => tab[i]
    }

fun set(idx: index_t, i: int, newval: int) =
    match idx {
    | IndexByte(tab) => tab[i] = uint8(newval)
    | IndexWord(tab) => tab[i] = uint16(newval)
    | IndexLarge(tab) => tab[i] = newval
    }

fun copy(idx: index_t)
{
    | IndexByte(tab) => IndexByte(copy(tab))
    | IndexWord(tab) => IndexWord(copy(tab))
    | IndexLarge(tab) => IndexLarge(copy(tab))
}

fun empty(size0: int, k0: 'k, d0: 'd): ('k, 'd) Hashmap.t
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

fun t.empty(): bool = self.nactive == 0
fun t.size() = self.nactive

fun t.clear() {
    val entry0 = self.default_entry
    val table = self.table
    for i <- 0:size(table) {
        table[i] = entry0
    }
    self.nactive = 0
    self.tabsz = 0
    self.free = 0
    self.index = makeindex(size(table)*2)
}

fun t.copy(): ('k, 'd) Hashmap.t =
    Hashmap.t {
        default_entry=self.default_entry, nactive=self.nactive,
        tabsz=self.tabsz, free = self.free, index=copy(self.index),
        table=copy(self.table) }

@private fun add_fast_(tabsz: int, ht_index: index_t,
                    ht_table: ('k, 'd) hashentry_t [],
                    entry: ('k, 'd) hashentry_t)
{
    val idxsz = size(ht_index)
    val hv = entry.hv
    var perturb = hv, j = int(hv) & (idxsz - 1)
    var found_free_slot = false

    for i <- 0:idxsz+14 {
        val tidx = ht_index.get(j)
        if tidx == HASH_EMPTY {
            ht_index.set(j, tabsz+HASH_ALIVE)
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

@private fun t.grow(new_size: int): void
{
    //println(f"started growing to {new_size}")
    val ht_table = self.table
    val new_ht_table = array(new_size, self.default_entry)
    val new_ht_index = makeindex(new_size*2)
    var tabsz = 0

    for j <- 0:self.tabsz {
        if ht_table[j].hv < HASH_SIGN_MASK {
            tabsz = add_fast_(tabsz, new_ht_index, new_ht_table, ht_table[j])
        }
    }
    self.index = new_ht_index
    self.table = new_ht_table
    self.tabsz = tabsz
    self.free = 0
    //println(f"finished growing to {new_size}")
}

@private fun t.find_idx_(k: 'k): (int, int)
{
    val hv = hash(k) & ~HASH_SIGN_MASK
    val idxsz = size(self.index)
    var perturb = hv, found = -1
    var j = int(hv) & (idxsz - 1)
    for i <- 0:idxsz+14 {
        val tidx = self.index.get(j)
        if tidx >= HASH_ALIVE {
            val entry = self.table[tidx - HASH_ALIVE]
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

fun t.find_idx(k: 'k) = self.find_idx_(k).1
fun t.mem(k: 'k): bool = self.find_idx_(k).1 >= 0
fun t.find_opt(k: 'k): 'd?
{
    val j = self.find_idx_(k).1
    if j >= 0 { Some(self.table[j].data) } else { None }
}

fun t.check_free()
{
    var free = self.free
    var count = 0
    print("free list: [")
    while free > 0 {
        count += 1
        val tidx = free - 1
        print(f" {tidx}")
        assert(0 <= tidx < self.tabsz)
        free = int(self.table[tidx].hv & ~HASH_SIGN_MASK)
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

fun t.find_idx_or_insert(k: 'k): int
{
    //find_idx_stat_ncalls += 1
    val hv = hash(k) & ~HASH_SIGN_MASK
    var idxsz = size(self.index)

    //println("============================================================")
    //println(f"new_nelems: {self.nactive+1}, free: {self.free-1}, tabsz: {self.tabsz}, idxsize: {size(self.index)}")
    if self.nactive + 1 > (idxsz >> 1) {
        while idxsz < (self.nactive + 1)*2 { idxsz *= 2 }
        self.grow(max(idxsz/2, self.nactive + 1))
    }

    //self.check_free()
    var perturb = hv, found = -1, insert_idx = -1
    var j = int(hv) & (idxsz - 1)
    //var hash_iters = 0

    for i <- 0:idxsz+14 {
        //hash_iters += 1
        val tidx = self.index.get(j)
        //println(f"j={j}, tidx={tidx}")
        if tidx >= HASH_ALIVE {
            val entry = self.table[tidx - HASH_ALIVE]
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
            self.index.set(insert_idx, found + HASH_ALIVE)
            self.index.set(j, HASH_DELETED)
        }
    } else if insert_idx >= 0 {
        found = self.free-1
        //println(f"got here (not found); insert_idx={insert_idx}, found={self.free-1}")
        if found >= 0 {
            //println(f"before & ~HASH_SIGN_MASK: {self.table[found].hv}")
            self.free = int(self.table[found].hv & ~HASH_SIGN_MASK)
            //println(f"new_free: {self.free-1}")
        } else {
            found = self.tabsz
            self.tabsz += 1
            assert(found < size(self.table))
        }
        self.table[found] = hashentry_t {hv=hv, key=k, data=self.default_entry.data}
        self.index.set(insert_idx, found + HASH_ALIVE)
        self.nactive += 1
    } else {
        throw Fail("cannot insert element (full Hashtable?!)")
    }
    found
}

fun t.add(k: 'k, d: 'd): void
{
    val idx = self.find_idx_or_insert(k)
    self.table[idx].data = d
}

fun t.remove(k: 'k) {
    val (j, tidx) = self.find_idx_(k)
    if tidx >= 0 {
        self.index.set(j, HASH_DELETED)
        self.table[tidx].hv = uint64(self.free) | HASH_SIGN_MASK
        self.free = tidx+1
        //self.check_free()
        self.nactive -= 1
    }
}

fun t.list(): ('k, 'd) list =
    [: for j <- 0:self.tabsz {
        if self.table[j].hv >= HASH_SIGN_MASK { continue }
        val entry = self.table[j]
        (entry.key, entry.data)
    } :]

fun t.add_list(data: ('k, 'd) list)
{
    var datasz = self.nactive + data.length()
    var curr_size = size(self.table), new_size = curr_size
    while new_size < datasz { new_size *= 2 }
    if new_size > curr_size { self.grow(new_size) }
    for (k, d) <- data { self.add(k, d) }
}

fun from_list(k0: 'k, d0: 'd, data: ('k, 'd) list): ('k, 'd) Hashmap.t
{
    val ht = empty(data.length(), k0, d0)
    ht.add_list(data)
    ht
}

fun t.app(f: ('k, 'd)->void) {
    val table = self.table
    for j <- 0:self.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            val entry = table[j]
            f(entry.key, entry.data)
        }
    }
}

fun t.foldl(f: ('k, 'd, 'r)->'r, res0: 'r): 'r {
    val table = self.table
    var res = res0
    for j <- 0:self.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            val entry = table[j]
            res = f(entry.key, entry.data, res)
        }
    }
    res
}

fun t.filter(f: ('k, 'd)->bool): void
{
    val idxsz = size(self.index)
    val table = self.table
    val key0 = self.default_entry.key
    val data0 = self.default_entry.data
    for j <- 0:idxsz {
        val tidx = self.index.get(j)
        if tidx < HASH_ALIVE { continue }
        val (key,value) = table[tidx - HASH_ALIVE].key
        if !f(key,value) {
            self.index.set(j, HASH_DELETED)
            table[tidx - HASH_ALIVE] = hashentry_t {
                hv = uint64(self.free) | HASH_SIGN_MASK, key=key0, data = data0}
            self.free = tidx + 1
            self.nactive -= 1
        }
    }
}
