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

type 'k hashset_entry_t = {hv: hash_t; key: 'k}
type index_t = int []

class 'k t
{
    default_entry: 'k hashset_entry_t
    var nactive: int
    var tabsz: int
    var free: int
    var index: index_t
    var table: 'k hashset_entry_t []
}

fun makeindex(size: int) = array(size, 0)

fun empty(size0: int, k0: 'k): 'k Hashset.t
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

fun t.copy(): 'k Hashset.t =
    Hashset.t {
        default_entry=self.default_entry, nactive=self.nactive,
        tabsz=self.tabsz, free = self.free, index=copy(self.index),
        table=copy(self.table) }

fun t.compress(): 'k Hashset.t
{
    val nactive = self.nactive
    val result = empty(nactive, self.default_entry.key)
    val table = self.table
    for i <- 0:self.tabsz {
        if table[i].hv < HASH_SIGN_MASK {
            result.add_(table[i].key, table[i].hv)
        }
    }
    result
}

@private fun add_fast_(tabsz: int, ht_index: index_t,
                    ht_table: 'k hashset_entry_t [],
                    entry: 'k hashset_entry_t)
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

@private fun t.find_idx_(k: 'k, hv: hash_t): (int, int)
{
    val idxsz = size(self.index)
    var perturb = hv, found = -1
    var j = int(hv) & (idxsz - 1)
    for i <- 0:idxsz+14 {
        val tidx = self.index[j]
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

fun t.find_idx(k: 'k) = self.find_idx_(k, hash(k) & ~HASH_SIGN_MASK).1
fun t.mem(k: 'k): bool = self.find_idx_(k, hash(k) & ~HASH_SIGN_MASK).1 >= 0
fun t.find_opt(k: 'k): 'd?
{
    val j = self.find_idx_(k, hash(k) & ~HASH_SIGN_MASK).1
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

@private fun t.add_(k: 'k, hv: hash_t)
{
    var idxsz = size(self.index)

    if self.nactive + 1 > (idxsz >> 1) {
        while idxsz < (self.nactive + 1)*2 { idxsz *= 2 }
        self.grow(max(idxsz/2, self.nactive + 1))
    }

    //self.check_free()
    var perturb = hv, found = -1, insert_idx = -1
    var j = int(hv) & (idxsz - 1)
    for i <- 0:idxsz+14 {
        val tidx = self.index[j]
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
    if found >= 0 {
        if insert_idx >= 0 && insert_idx != j {
            self.index[insert_idx] = found + HASH_ALIVE
            self.index[j] = HASH_DELETED
        }
    } else if insert_idx >= 0 {
        found = self.free-1
        if found >= 0 {
            self.free = int(self.table[found].hv & ~HASH_SIGN_MASK)
        } else {
            found = self.tabsz
            self.tabsz += 1
            assert(found < size(self.table))
        }
        self.table[found] = hashset_entry_t {hv=hv, key=k}
        self.index[insert_idx] = found + HASH_ALIVE
        self.nactive += 1
    } else {
        throw Fail("cannot insert element (full Hashtable?!)")
    }
}

fun t.add(k: 'k) = self.add_(k, hash(k) & ~HASH_SIGN_MASK)

fun t.remove(k: 'k) {
    val (j, tidx) = self.find_idx_(k, hash(k) & ~HASH_SIGN_MASK)
    if tidx >= 0 {
        self.index[j] = HASH_DELETED
        self.table[tidx].hv = uint64(self.free) | HASH_SIGN_MASK
        self.free = tidx+1
        self.nactive -= 1
    }
}

fun t.list(): 'k list =
    [:: for j <- 0:self.tabsz {
        if self.table[j].hv >= HASH_SIGN_MASK { continue }
        self.table[j].key
    }]

fun t.array(): 'k []
{
    val result = array(self.nactive, self.default_entry.key)
    var k = 0
    for j <- 0:self.tabsz {
        if self.table[j].hv >= HASH_SIGN_MASK { continue }
        result[k] = self.table[j].key
        k += 1
    }
    result
}

fun t.add_list(data: 'k list)
{
    var datasz = self.nactive + data.length()
    var curr_size = size(self.table), new_size = curr_size
    while new_size < datasz { new_size *= 2 }
    if new_size > curr_size { self.grow(new_size) }
    for k <- data { self.add(k) }
}

fun from_list(k0: 'k, data: 'k list): 'k Hashset.t
{
    val ht = empty(data.length(), k0)
    ht.add_list(data)
    ht
}

fun t.app(f: 'k->void) {
    val table = self.table
    for j <- 0:self.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            f(table[j].key)
        }
    }
}

fun t.foldl(f: ('k, 'r)->'r, res0: 'r): 'r {
    val table = self.table
    var res = res0
    for j <- 0:self.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            res = f(table[j].key, res)
        }
    }
    res
}

fun t.union(b: 'k Hashset.t): void
{
    val table = b.table
    for j <- 0:b.tabsz {
        if table[j].hv < HASH_SIGN_MASK {
            self.add_(table[j].key, table[j].hv)
        }
    }
}

fun t.intersect(b: 'k Hashset.t): void
{
    val table = self.table
    val tabsz = self.tabsz
    val tmp = empty(min(size(self.table), size(b.table)), self.default_entry.key)
    self.table = tmp.table
    self.index = tmp.index
    self.nactive = 0
    self.tabsz = 0
    self.free = 0
    for j <- 0:tabsz {
        if table[j].hv < HASH_SIGN_MASK && b.find_idx_(table[j].key, table[j].hv).1 >= 0 {
            self.add_(table[j].key, table[j].hv)
        }
    }
}

fun t.diff(b: 'k Hashset.t): void
{
    val table = b.table
    for j <- 0:b.tabsz {
        val {hv, key} = table[j]
        if hv < HASH_SIGN_MASK {
            val (j, tidx) = self.find_idx_(key, hv)
            if tidx >= 0 {
                self.index[j] = HASH_DELETED
                self.table[tidx].hv = uint64(self.free) | HASH_SIGN_MASK
                self.free = tidx+1
                self.nactive -= 1
            }
        }
    }
}

fun t.all(f: 'k->bool): bool
{
    val table = self.table
    var ok = true
    for j <- 0:self.tabsz {
        if table[j].hv < HASH_SIGN_MASK && !f(table[j].key) {
            ok = false; break
        }
    }
    ok
}

fun t.exists(f: 'k->bool): bool
{
    val table = self.table
    var ok = false
    for j <- 0:self.tabsz {
        if table[j].hv < HASH_SIGN_MASK && f(table[j].key) {
            ok = true; break
        }
    }
    ok
}

fun t.filter(f: 'k->bool): void
{
    val idxsz = size(self.index)
    val table = self.table
    val key0 = self.default_entry.key
    for j <- 0:idxsz {
        val tidx = self.index[j]
        if tidx >= HASH_ALIVE && !f(table[tidx - HASH_ALIVE].key) {
            self.index[j] = HASH_DELETED
            table[tidx - HASH_ALIVE] = hashset_entry_t {
                hv = uint64(self.free) | HASH_SIGN_MASK, key=key0}
            self.free = tidx + 1
            self.nactive -= 1
        }
    }
}
