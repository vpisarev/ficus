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

class 'k t
{
    hash_f: 'k -> hash_t
    default_entry: 'k hashset_entry_t
    var nelems: int
    var nremoved: int
    var table: 'k hashset_entry_t []
}

fun empty(size0: int, k0: 'k, f: 'k->hash_t ): 'k Hashset.t
{
    var size = 8
    while size < size0 { size *= 2 }
    val entry0 = hashset_entry_t {hv=HASH_EMPTY, key=k0}
    Hashset.t {
        hash_f=f, default_entry=entry0,
        nelems=0, nremoved=0,
        table=array(size, entry0) }
}

fun t.empty(): bool = self.nelems == 0
fun t.size() = self.nelems

fun t.clear() {
    val table = self.table
    val entry0 = self.default_entry
    for i <- 0:size(table) {
        table[i] = entry0
    }
    self.nelems = 0
    self.nremoved = 0
}

fun t.copy(): 'k Hashset.t =
    Hashset.t { hash_f=self.hash_f, default_entry=self.default_entry,
        nelems=self.nelems, nremoved=self.nremoved,
        table=copy(self.table) }

fun t.compress(): 'k Hashset.t
{
    val nelems = self.nelems
    val result = empty(nelems*2, self.default_entry.key, self.hash_f)
    val table = self.table
    for i <- 0:size(table) {
        if table[i].hv > HASH_DELETED {
            result.add_(table[i].key, table[i].hv)
        }
    }
    result
}

@private fun t.add_(hs_table: 'k hashset_entry_t [], entry: 'k hashset_entry_t): (int, int)
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
                    hs_table[j] = self.default_entry
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

@private fun t.grow(new_size: int): void
{
    val hs_table = self.table
    val curr_size = size(hs_table)
    val new_ht_table = array(new_size, self.default_entry)
    for j <- 0:curr_size {
        if hs_table[j].hv > HASH_DELETED {
            ignore(self.add_(new_ht_table, hs_table[j]))
        }
    }
    self.table = new_ht_table
    self.nremoved = 0
}

fun t.find_idx_(k: 'k, hv_: hash_t): int
{
    var hv = hv_
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    val tabsz = size(self.table)
    var perturb = hv, found = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val {hv=hvj, key=kj} = self.table[j]
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

fun t.find_idx(k: 'k): int = self.find_idx_(k, self.hash_f(k))
fun t.mem(k: 'k): bool = self.find_idx(k) >= 0

fun t.add_(k: 'k, hv_: hash_t)
{
    var hv = hv_
    if hv <= HASH_DELETED { hv ^= FNV_1A_OFFSET }
    var tabsz = size(self.table)

    if self.nelems + self.nremoved >= (tabsz >> 1) {
        while tabsz <= (self.nelems + self.nremoved)*2 { tabsz *= 2 }
        self.grow(tabsz)
    }

    var perturb = hv, found = -1, insert_pos = -1
    var j = int(hv) & (tabsz - 1)
    for i <- 0:tabsz+14 {
        val {hv=hvj, key=kj} = self.table[j]
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
            self.nremoved -= 1
        }
        perturb >>= PERTURB_SHIFT
        j = int(uint64(j*5 + 1) + perturb) & (tabsz - 1)
    }
    if found >= 0 {
        if insert_pos >= 0 {
            self.table[insert_pos] = self.table[found]
            self.table[found] = self.default_entry
        }
    }
    else if insert_pos >= 0 {
        self.table[insert_pos] = hashset_entry_t {hv=hv, key=k}
        self.nelems += 1
    } else {
        throw Fail("can-not insert element into half-empty Hashtable (?!)")
    }
}

fun t.add(k: 'k) = self.add_(k, self.hash_f(k))

fun t.remove(k: 'k) {
    val idx = self.find_idx(k)
    if idx >= 0 {
        self.table[idx] = self.default_entry.{hv=HASH_DELETED}
        self.nelems -= 1
        self.nremoved += 1
    }
}

fun t.list(): 'k list =
    [: for j <- 0:size(self.table) {
        if self.table[j].hv <= HASH_DELETED { continue }
        self.table[j].key
    } :]

fun t.add_list(data: 'k list)
{
    var datasz = self.nelems + self.nremoved + data.length()
    var curr_size = size(self.table), new_size = curr_size
    while new_size <= datasz*2 { new_size *= 2 }
    if new_size > curr_size { self.grow(new_size) }
    for k <- data { self.add(k) }
}

fun from_list(k0: 'k, hash_f: 'k->hash_t, data: 'k list): 'k Hashset.t
{
    val hs = empty(data.length()*2, k0, hash_f)
    hs.add_list(data)
    hs
}

fun t.app(f: 'k->void)
{
    val table = self.table
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            f(table[j].key)
        }
    }
}

fun t.foldl(f: ('k, 'r)->'r, res0: 'r): 'r
{
    val table = self.table
    var res = res0
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            res = f(table[j].key, res)
        }
    }
    res
}

fun t.union(b: 'k Hashset.t): void
{
    val table = b.table
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED {
            self.add_(table[j].key, table[j].hv)
        }
    }
}

fun t.intersect(b: 'k Hashset.t): void
{
    val table = self.table
    self.table = array(min(size(self.table), size(b.table)), self.default_entry)
    self.nelems = 0
    self.nremoved = 0
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED && b.find_idx_(table[j].key, table[j].hv) >= 0 {
            self.add_(table[j].key, table[j].hv)
        }
    }
}

fun t.all(f: 'k->bool): bool
{
    val table = self.table
    var ok = true
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED && !f(table[j].key) {
            ok = false; break
        }
    }
    ok
}

fun t.exists(f: 'k->bool): bool
{
    val table = self.table
    var ok = false
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED && f(table[j].key) {
            ok = true; break
        }
    }
    ok
}

fun t.filter(f: 'k->bool): void
{
    val table = self.table
    val deleted_entry = self.default_entry.{hv=HASH_DELETED}
    for j <- 0:size(table) {
        if table[j].hv > HASH_DELETED && !f(table[j].key) {
            table[j] = deleted_entry
            self.nelems -= 1
            self.nremoved += 1
        }
    }
}
