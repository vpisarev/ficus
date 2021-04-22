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

class ('k, 'd) t
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

fun t.empty(): bool = self.nelems == 0
fun t.size() = self.nelems

fun t.clear() {
    val entry0 = self.default_entry
    val table = self.table
    for i <- 0:size(table) {
        table[i] = entry0
    }
    self.nelems = 0
    self.nremoved = 0
}

fun t.copy(): ('k, 'd) Hashmap.t =
    Hashmap.t {
        hash_f=self.hash_f, default_entry=self.default_entry,
        nelems=self.nelems, nremoved=self.nremoved,
        table=copy(self.table) }

@private fun t.add_(ht_table: ('k, 'd) hashentry_t [],
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
                    ht_table[j] = self.default_entry
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

@private fun t.grow(new_size: int): void
{
    val ht_table = self.table
    val curr_size = size(ht_table)
    val new_ht_table = array(new_size, self.default_entry)
    for j <- 0:curr_size {
        if ht_table[j].hv > HASH_DELETED {
            ignore(self.add_(new_ht_table, ht_table[j]))
        }
    }
    self.table = new_ht_table
    self.nremoved = 0
}

fun t.find_idx(k: 'k): int
{
    var hv = self.hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    val tabsz = size(self.table)
    var perturb = hv, found = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val entry = self.table[j]
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

fun t.mem(k: 'k): bool = self.find_idx(k) >= 0
fun t.find_opt(k: 'k): 'd?
{
    val j = self.find_idx(k)
    if j >= 0 { Some(self.table[j].data) } else { None }
}

fun t.find_idx_or_insert(k: 'k): int
{
    var hv = self.hash_f(k)
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    var tabsz = size(self.table)

    if self.nelems + self.nremoved >= (tabsz >> 1) {
        while tabsz <= (self.nelems + self.nremoved)*2 { tabsz *= 2 }
        self.grow(tabsz)
    }

    var perturb = hv, found = -1, insert_idx = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val entry = self.table[j]
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
            self.nremoved -= 1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if found >= 0 {
        if insert_idx < 0 {
            found
        } else {
            self.table[insert_idx] = self.table[found]
            self.table[found] = self.default_entry
            insert_idx
        }
    }
    else if insert_idx >= 0 {
        self.table[insert_idx] = hashentry_t {hv=hv, key=k, data=self.default_entry.data}
        self.nelems += 1
        insert_idx
    } else {
        throw Fail("can-not insert element into half-empty Hashtable (?!)")
    }
}

fun t.add(k: 'k, d: 'd): void
{
    val idx = self.find_idx_or_insert(k)
    self.table[idx].data = d
}

fun t.remove(k: 'k) {
    val idx = self.find_idx(k)
    if idx >= 0 {
        self.table[idx] = self.default_entry.{hv=HASH_DELETED}
        self.nelems -= 1
        self.nremoved += 1
    }
}

fun t.list(): ('k, 'd) list =
    [: for j <- 0:size(self.table) {
        if self.table[j].hv <= HASH_DELETED { continue }
        val entry = self.table[j]
        (entry.key, entry.data)
    } :]

fun t.add_list(data: ('k, 'd) list)
{
    var datasz = self.nelems + self.nremoved + data.length()
    var curr_size = size(self.table), new_size = curr_size
    while new_size <= datasz*2 { new_size *= 2 }
    if new_size > curr_size { self.grow(new_size) }
    for (k, d) <- data { self.add(k, d) }
}

fun from_list(k0: 'k, d0: 'd, hash_f: 'k->hash_t, data: ('k, 'd) list): ('k, 'd) Hashmap.t
{
    val ht = empty(data.length()*2, k0, d0, hash_f)
    ht.add_list(data)
    ht
}

fun t.app(f: ('k, 'd)->void) {
    val table = self.table
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            val entry = table[j]
            f(entry.key, entry.data)
        }
    }
}

fun t.foldl(f: ('k, 'd, 'r)->'r, res0: 'r): 'r {
    val table = self.table
    var res = res0
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            val entry = table[j]
            res = f(entry.key, entry.data, res)
        }
    }
    res
}
