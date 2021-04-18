/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    This is implementation of Relaxed Radix Balanced Trees (RRB Trees)
    and immutable vectors based on it.

    The implementation is derived from https://github.com/hypirion/c-rrb.
    Below is the original copyright.
*/

/*
 * Copyright (c) 2013-2014 Jean Niklas L'orange. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 */

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef __FICUS_RRBVEC_IMPL_H__
#define __FICUS_RRBVEC_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

#undef RRB_MIN
#undef RRB_MAX
#define RRB_MIN(a, b) ((a) <= (b) ? (a) : (b))
#define RRB_MAX(a, b) ((a) >= (b) ? (a) : (b))

enum
{
    FX_RRB_RADIX_BITS = 5,
    FX_RRB_RADIX = 1 << FX_RRB_RADIX_BITS,
    FX_RRB_SHIFT_MASK = 255,
    FX_RRB_DEPTH_MASK = 255,
    FX_RRB_DEPTH_SHIFT = 8,
    FX_RRB_FLAG_NODE = 0 << 16,
    FX_RRB_FLAG_LEAF = 1 << 16,
    FX_RRB_FLAG_SIZES = 2 << 16
};

typedef struct fx_rrbleaf_t
{
    int_ rc;
    int flags, nelems;
} fx_rrbleaf_t;

typedef struct fx_rrbnode_t
{
    int_ rc;
    int flags, nelems;
    int_ total;
    fx_rrbnode_t* child[FX_RRB_RADIX];
} fx_rrbnode_t;

#define FX_RRB_LEAF_HDR_SIZE        (sizeof(int)*4)
#define FX_RRB_SHIFT(flags)         ((flags) & FX_RRB_SHIFT_MASK)
#define FX_RRB_DEPTH(flags)         (((flags) >> FX_RRB_DEPTH_SHIFT) & FX_RRB_DEPTH_MASK)
#define FX_RRB_HAVE_SIZES(flags)    (((flags) & FX_RRB_FLAG_SIZES) != 0)
#define FX_RRB_IS_LEAF(flags)       (((flags) & FX_RRB_FLAG_LEAF) != 0)
#define FX_RRB_SIZES(node)          (int_*)((node)+1)
#define FX_RRB_DATA(leaf)           ((char*)(leaf)+FX_RRB_LEAF_HDR_SIZE)
#define FX_RRB_TOTAL(node)          (FX_RRB_IS_LEAF((node)->flags) ? (int_)(node)->nelems : (node)->total)
#define FX_RRB_MAX_SIZE             (int_)((size_t)(int_)-1 >> 1)
#define FX_RRB_EXTRA                2

char* fx_rrb_find(const fx_rrbvec_t* vec, int_ index)
{
    int_ idx = index, size = vec->size;
    const fx_rrbnode_t* tail = vec->tail;
    int tsize = tail ? tail->nelems : 0;
    int_ elemsize = vec->info.elemsize, tail_fst = size - tsize;
    fx_rrbnode_t* node;
    if (idx >= tail_fst) return FX_RRB_DATA(tail) + elemsize*(idx - tail_fst);
    node = vec->root;
    for(;;) {
        int flags = node->flags, nidx;
        if (!FX_RRB_HAVE_SIZES(flags)) {
            // the node and all its children are balanced nodes, use the fast path
            int sh = FX_RRB_SHIFT(flags);
            for(;; sh -= FX_RRB_RADIX_BITS) {
                if (FX_RRB_IS_LEAF(flags)) return FX_RRB_DATA(node) + elemsize*idx;
                nidx = (int)(idx >> sh) & (FX_RRB_RADIX-1);
                idx -= nidx << sh;
                node = node->child[nidx];
                flags = node->flags;
            }
        } else {
            FX_STATIC_ASSERT(FX_RRB_RADIX == 32);
            const int_* sizes = FX_RRB_SIZES(node);
            nidx = idx >= sizes[FX_RRB_RADIX/2-1] ? FX_RRB_RADIX/2 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/4-1] ? FX_RRB_RADIX/4 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/8-1] ? FX_RRB_RADIX/8 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/16-1] ? FX_RRB_RADIX/16 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/32-1] ? FX_RRB_RADIX/32 : 0;
            idx -= nidx > 0 ? sizes[nidx-1] : 0;
            node = node->child[nidx];
        }
    }
}

char* fx_rrb_find_border(const fx_rrbvec_t* vec, int_ idx, int border)
{
    int_ size = vec->size;
    if ((size_t)idx >= (size_t)size) {
        if (border == 'z' || size == 0) return (char*)fx_zerobuf;
        if (border == 'w')
            idx = (idx % size) + (idx < 0 ? size : 0);
        else
            idx = idx < 0 ? 0 : size-1;
    }
    return fx_rrb_find(vec, idx);
}

static int fx_rrb_findpath(const fx_rrbnode_t* root, int_* idx0, fx_rrbnode_t** path, int* path_idx) {
    fx_rrbnode_t* node = (fx_rrbnode_t*)root;
    int_ idx = *idx0;
    int i = 0;
    for(;i < FX_RRB_MAX_DEPTH;i++) {
        int flags = node->flags, nidx;

        if (!FX_RRB_HAVE_SIZES(flags)) {
            // the node and its children are all perfectly balanced, use the fast path
            int sh = FX_RRB_SHIFT(flags);
            for(;; sh -= FX_RRB_RADIX_BITS, i++) {
                path[i] = node;
                if (FX_RRB_IS_LEAF(flags)) {
                    *idx0 = idx;
                    return i;
                }
                nidx = (int)(idx >> sh) & (FX_RRB_RADIX-1);
                idx -= nidx << sh;
                path_idx[i] = nidx;
                node = node->child[nidx];
                flags = node->flags;
            }
        } else {
            const int_* sizes = FX_RRB_SIZES(node);
            nidx = idx >= sizes[FX_RRB_RADIX/2-1] ? FX_RRB_RADIX/2 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/4-1] ? FX_RRB_RADIX/4 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/8-1] ? FX_RRB_RADIX/8 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/16-1] ? FX_RRB_RADIX/16 : 0;
            nidx += idx >= sizes[nidx + FX_RRB_RADIX/32-1] ? FX_RRB_RADIX/32 : 0;
            idx -= nidx > 0 ? sizes[nidx-1] : 0;
            path[i] = node;
            path_idx[i] = nidx;
            node = node->child[nidx];
        }
    }
    return -1;
}

char* fx_rrb_start_read(const fx_rrbvec_t* vec, fx_rrbiter_t* iter, int_ startidx, int dir)
{
    const fx_rrbinfo_t* info = &vec->info;
    fx_rrbnode_t* root = vec->root;
    int_ size = vec->size;
    int_ elemsize = info->elemsize;
    const fx_rrbnode_t* tail = vec->tail;
    int i = 0, tsize = tail ? tail->nelems : 0;
    int_ tail_fst = size - tsize;
    char* ptr;

    assert(dir == 1 || dir == -1);
    iter->vec = (fx_rrbvec_t*)vec;
    iter->dir = dir;
    if ((size_t)startidx >= (size_t)vec->size) {
        iter->depth = 0;
        iter->blockend = iter->blockstart = 0;
        return 0;
    }
    if (startidx >= tail_fst) {
        ptr = FX_RRB_DATA(tail);
        iter->depth = 0;
        iter->blockstart = ptr;
        iter->blockend = ptr + tail->nelems*elemsize;
        if (dir < 0 && root != 0) {
            iter->depth = 1;
            iter->nstack[0] = root;
            iter->istack[0] = root->nelems;
        }
        return ptr + (startidx - tail_fst)*elemsize;
    } else {
        int_ idx = startidx;
        int depth = fx_rrb_findpath(vec->root, &idx, iter->nstack, iter->istack);
        iter->depth = depth;
        if (depth < 0) {
            iter->blockend = iter->blockstart = 0;
            return 0;
        } else {
            fx_rrbnode_t* leaf = iter->nstack[depth];
            ptr = FX_RRB_DATA(leaf);
            iter->blockstart = ptr;
            iter->blockend = ptr + leaf->nelems*elemsize;
            return ptr + idx*elemsize;
        }
    }
}

char* fx_rrb_next(fx_rrbiter_t* iter)
{
    int i, depth = iter->depth, dir = iter->dir, elemsize = iter->vec->info.elemsize;
    if (iter->blockend == 0) return 0;

    for(i = depth-1; i >= 0; i--) {
        fx_rrbnode_t* node = iter->nstack[i];
        int nidx = iter->istack[i] + dir;
        if (nidx < 0 || nidx >= node->nelems)
            continue;
        for(;;) {
            iter->istack[i++] = nidx;
            node = node->child[nidx];
            if (FX_RRB_IS_LEAF(node->flags)) {
                char* ptr = FX_RRB_DATA(node);
                iter->blockstart = ptr;
                iter->blockend = ptr + node->nelems*elemsize;
                iter->depth = i;
                if(dir < 0) ptr = iter->blockend - elemsize;
                return ptr;
            }
            iter->nstack[i] = node;
            nidx = dir > 0 ? 0 : node->nelems-1;
        }
    }
    {
    fx_rrbnode_t* tail = iter->vec->tail;
    char* ptr = FX_RRB_DATA(tail);
    iter->blockend = ptr + tail->nelems*elemsize;
    iter->depth = 0;
    return ptr;
    }
}

static fx_rrbnode_t* fx_rrb_newleaf(const fx_rrbinfo_t* info, int_ nelems)
{
    size_t datasize = nelems*info->elemsize;
    fx_rrbnode_t* leaf = (fx_rrbnode_t*)fx_malloc(FX_RRB_LEAF_HDR_SIZE + datasize);
    if (leaf) {
        leaf->rc = 1;
        leaf->flags = FX_RRB_FLAG_LEAF;
        leaf->nelems = (int)nelems;
        if (info->free_f)
            memset(FX_RRB_DATA(leaf), 0, info->elemsize*nelems);
    }
    return leaf;
}

static fx_rrbnode_t* fx_rrb_newnode(const fx_rrbinfo_t* info, int flags, bool init_sizes)
{
    bool have_sizes = FX_RRB_HAVE_SIZES(flags);
    size_t datasize = have_sizes ? sizeof(int_)*FX_RRB_RADIX : 0;
    fx_rrbnode_t* node = (fx_rrbnode_t*)fx_malloc(sizeof(fx_rrbnode_t) + datasize);
    if (node) {
        node->rc = 1;
        node->flags = flags;
        node->nelems = 0;
        node->total = 0;
        if (have_sizes & init_sizes) {
            int_* sizes = FX_RRB_SIZES(node);
            // by default set all the sizes to some maximum value
            // to make the binary search work properly
            // (i.e. inside only the occupied part of the node)
            for(int i=0; i < FX_RRB_RADIX; i++)
                sizes[i] = FX_RRB_MAX_SIZE;
        }
        //if (info->free_f)
        //    memset(node->child, 0, sizeof(node->child));
    }
    return node;
}

static void fx_rrb_copy_elems(const char* src, char* dst, int_ sz, const fx_rrbinfo_t* info)
{
    fx_copy_t copy_f = info->copy_f;
    if(!copy_f) {
        if (sz > 0) memcpy(dst, src, sz);
    } else if(copy_f == fx_copy_ptr) {
        fx_ref_simple_t *src_ = (fx_ref_simple_t*)src, *dst_ = (fx_ref_simple_t*)dst;
        int_ sz_ = sz/sizeof(src_[0]);
        for(int_ i = 0; i < sz_; i++)
            FX_COPY_PTR(src_[i], dst_+i);
    } else {
        int elemsize = info->elemsize;
        for(int_ i = 0; i < sz; i += elemsize) {
            copy_f(src + i, dst + i);
        }
    }
}

// Add more elements to the vector; modify the vector in-place.
// That is, the function treats vector as a mutable structure.
// The function is intended to be used in vector comprehension
// or vector literal construction operations.
int fx_rrb_write(fx_rrbiter_t* iter, void* pptr, const char* elems, int_ nelems)
{
    char* ptr = *(char**)pptr;
    const fx_rrbinfo_t* info = &iter->vec->info;
    char* blockend = iter->blockend;
    int elemsize = info->elemsize;
    int_ i, sz0 = nelems*elemsize;;
    while (sz0 > 0) {
        int_ sz = blockend - ptr;
        sz = sz < sz0 ? sz : sz0;
        sz0 -= sz;
        fx_rrb_copy_elems(elems, ptr, sz, info);
        elems += sz;
        ptr += sz;
        if (ptr >= blockend && sz0 > 0) {
            fx_rrbvec_t* vec = iter->vec;
            int nleafelems = 1 << info->nleafelems_log;
            if(vec->tail) {
                // add the existing "tail" to the tree
                fx_rrbnode_t* child = vec->tail;
                int shift = info->nleafelems_log, depth = 1;
                child->nelems = nleafelems;
                vec->size += nleafelems;
                i = iter->depth-1;
                for(; i >= 0; i--, shift += FX_RRB_RADIX_BITS, depth++) {
                    fx_rrbnode_t* node = iter->nstack[i];
                    // Let's assume we always have regular nodes (without sizes) for simplicity.
                    // If we construct a new vector from scratch, which is always the case
                    // in vector comprehensions or vector literals.
                    assert(!FX_RRB_HAVE_SIZES(node->flags));
                    if (node->nelems < FX_RRB_RADIX) {
                        node->child[node->nelems++] = child;
                        int_ child_total = FX_RRB_TOTAL(child);
                        for(int j = i; j >= 0; j--) {
                            iter->nstack[j]->total += child_total;
                        }
                        break;
                    }
                    node = fx_rrb_newnode(info, (depth << FX_RRB_DEPTH_SHIFT) + shift, true);
                    if(!node) FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
                    node->child[0] = child;
                    node->nelems = 1;
                    node->total = FX_RRB_TOTAL(child);
                    iter->nstack[i] = node;
                    child = node;
                }
                if (i < 0) {
                    // need to form a new root, because the current one does not fit all the needed data
                    fx_rrbnode_t* root = fx_rrb_newnode(info, (depth << FX_RRB_DEPTH_SHIFT) + shift, true);
                    int nelems = 0;
                    int_ total = FX_RRB_TOTAL(child);
                    if(!root) FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
                    if (vec->root) {
                        root->child[nelems++] = vec->root;
                        total += FX_RRB_TOTAL(vec->root);
                    }
                    root->child[nelems++] = child;
                    root->nelems = nelems;
                    root->total = total;
                    vec->root = root;
                    for (i = ++iter->depth - 1; i > 0; i--) {
                        iter->nstack[i] = iter->nstack[i-1];
                    }
                    iter->nstack[0] = root;
                }
                assert(vec->root->total == vec->size);
            }
            vec->tail = fx_rrb_newleaf(info, nleafelems);
            if(!vec->tail)
                FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
            ptr = FX_RRB_DATA(vec->tail);
            iter->blockstart = ptr;
            iter->blockend = blockend = ptr + nleafelems*elemsize;
        }
    }
    *(char**)pptr = ptr;
    return FX_OK;
}

void fx_rrb_make_empty(size_t elemsize, fx_free_t free_f, fx_copy_t copy_f, fx_rrbvec_t* vec)
{
    static const int8_t tabsz[] = {
        FX_RRB_RADIX_BITS+3, FX_RRB_RADIX_BITS+3, FX_RRB_RADIX_BITS+2, FX_RRB_RADIX_BITS+1,
        FX_RRB_RADIX_BITS+1, FX_RRB_RADIX_BITS, FX_RRB_RADIX_BITS, FX_RRB_RADIX_BITS,
        FX_RRB_RADIX_BITS, FX_RRB_RADIX_BITS-1, FX_RRB_RADIX_BITS-1, FX_RRB_RADIX_BITS-1,
        FX_RRB_RADIX_BITS-1, FX_RRB_RADIX_BITS-1, FX_RRB_RADIX_BITS-1, FX_RRB_RADIX_BITS-1,
        FX_RRB_RADIX_BITS-1, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2,
        FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2,
        FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2,
        FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2, FX_RRB_RADIX_BITS-2,
        FX_RRB_RADIX_BITS-2
    };
    vec->size = 0;
    vec->info.elemsize = (int)elemsize;
    vec->info.free_f = free_f;
    vec->info.copy_f = copy_f;
    vec->info.nleafelems_log = elemsize <= 32 ? (int)tabsz[elemsize] : 2;
    vec->root = 0;
    vec->tail = 0;
}

static void fx_rrb_make_empty_(const fx_rrbinfo_t* info, fx_rrbvec_t* vec)
{
    vec->size = 0;
    vec->info = *info;
    vec->root = 0;
    vec->tail = 0;
}

void fx_rrb_copy(const fx_rrbvec_t* src, fx_rrbvec_t* dst)
{
    if (src->root) FX_INCREF(src->root->rc);
    if (src->tail) FX_INCREF(src->tail->rc);
    *dst = *src;
}

static void fx_rrb_freenode_(const fx_rrbinfo_t* info, fx_rrbnode_t* node)
{
    if (!FX_RRB_IS_LEAF(node->flags)) {
        int i, nelems = node->nelems;
        for(i = 0; i < nelems; i++) {
            fx_rrbnode_t* child = node->child[i];
            if (FX_DECREF(child->rc) == 1) {
                fx_rrb_freenode_(info, child);
            }
        }
    } else {
        if (info->free_f) {
            int i, nelems = node->nelems;
            int elemsize = info->elemsize;
            fx_free_t free_f = info->free_f;
            char* ptr = FX_RRB_DATA(node);
            for(i = 0; i < nelems; i++) {
                free_f(ptr + i*elemsize);
            }
        }
    }
    node->flags = 0;
    node->nelems = 0;
    fx_free(node);
}

static inline void fx_rrb_freenode(const fx_rrbinfo_t* info, fx_rrbnode_t** node)
{
    if(*node) {
        if(FX_DECREF((*node)->rc) == 1) fx_rrb_freenode_(info, *node);
        *node = 0;
    }
}

void fx_rrb_free(fx_rrbvec_t* vec)
{
    fx_rrb_freenode(&vec->info, &vec->root);
    fx_rrb_freenode(&vec->info, &vec->tail);
    vec->size = 0;
}

// in theory, 0 elements can be written to a vector,
// so we don't allocate anything until the first element is
// written to the vector
char* fx_rrb_start_write(size_t elemsize, fx_free_t free_elem,
                       fx_copy_t copy_elem, fx_rrbvec_t* vec,
                       fx_rrbiter_t* iter)
{
    fx_rrb_make_empty(elemsize, free_elem, copy_elem, vec);
    iter->vec = vec;
    iter->depth = 0;
    iter->blockend = 0;
    iter->blockstart = 0;
    return 0;
}

void fx_rrb_end_write(fx_rrbiter_t* iter, char* ptr)
{
    if(iter->vec->tail) {
        char* ptr0 = iter->blockstart;
        int_ nelems = (ptr - ptr0)/iter->vec->info.elemsize;
        iter->vec->tail->nelems = nelems;
        iter->vec->size += nelems;
    }
}

static fx_rrbnode_t* fx_rrb_copynode(const fx_rrbvec_t* vec, const fx_rrbnode_t* node)
{
    int i, nelems = node->nelems, flags = node->flags;
    fx_rrbnode_t* newnode = fx_rrb_newnode(&vec->info, flags, false);
    if (!newnode) return 0;
    newnode->nelems = nelems;
    newnode->total = node->total;
    for (i = 0; i < nelems; i++) {
        newnode->child[i] = node->child[i];
        FX_INCREF(newnode->child[i]->rc);
    }
    if (FX_RRB_HAVE_SIZES(flags)) {
        const int_* srcsizes = FX_RRB_SIZES(node);
        int_* dstsizes = FX_RRB_SIZES(newnode);
        memcpy(dstsizes, srcsizes, FX_RRB_RADIX*sizeof(srcsizes[0]));
    }
    return newnode;
}

static int_ fx_rrb_check_tree(const fx_rrbnode_t* node, int_ startidx, bool allref1)
{
    if (!node) return 0;
    {
    int flags = node->flags, i, nelems = node->nelems, errs = 0, depth_errs = 0;
    int depth = FX_RRB_DEPTH(flags), depth1;
    int_ total = 0, node_total;
    if (FX_RRB_IS_LEAF(flags)) {
        total = node_total = nelems;
        depth1 = 0;
    } else if (FX_RRB_HAVE_SIZES(flags)) {
        const int_* sizes = FX_RRB_SIZES(node);
        for(i = 0; i < nelems; i++) {
            int_ total_i = fx_rrb_check_tree(node->child[i], startidx + total, allref1);
            total += total_i;
            if(total != sizes[i]) {
                errs++;
            }
            depth_errs += depth != FX_RRB_DEPTH(node->child[i]->flags) + 1;
        }
        node_total = node->total;
        depth1 = FX_RRB_DEPTH(node->child[0]->flags) + 1;
    } else {
        for(i = 0; i < nelems; i++) {
            int_ total_i = fx_rrb_check_tree(node->child[i], startidx + total, allref1);
            total += total_i;
            depth_errs += depth != FX_RRB_DEPTH(node->child[i]->flags) + 1;
        }
        node_total = node->total;
        depth1 = FX_RRB_DEPTH(node->child[0]->flags) + 1;
    }
    errs += total != node_total;
    errs += (allref1 && node->rc != 1);
    if(errs > 0 || depth_errs > 0) {
        printf("node (rc=%d) (depth=%d) [%d..%d]: incorrect rc or array of sizes or total or depth: calculated total %d, stored %d; calculated depth: %d, stored: %d\n",
            (int)node->rc, depth, (int)startidx, (int)(startidx + total-1),
            (int)total, (int)node_total, depth1, depth);
        assert(0);
    }
    return total;
    }
}

static fx_rrbnode_t* fx_rrb_push_tail(fx_rrbvec_t* vec, bool inplace)
{
    const fx_rrbinfo_t* info = &vec->info;
    int i, nleaf_elems_log = info->nleafelems_log, nleaf_elems = 1 << nleaf_elems_log;
    int grow = 1, depth = 1, shift = nleaf_elems_log;
    int_ childsize, leafsize;
    fx_rrbnode_t* stack[FX_RRB_MAX_DEPTH];
    fx_rrbnode_t* root = vec->root, *node = root, *child;
    fx_rrbnode_t* tail = vec->tail;
    if (!root) {
        root = fx_rrb_newnode(info, (1 << FX_RRB_DEPTH_SHIFT) + nleaf_elems_log, true);
        root->nelems = 1;
        root->total = tail->nelems;
        root->child[0] = tail;
        return root;
    }
    for(i = 0;; i++) {
        int flags = node->flags;
        if(FX_RRB_IS_LEAF(flags)) break;
        stack[i] = node;
        node = node->child[node->nelems-1];
    }
    stack[i] = (fx_rrbnode_t*)tail;
    childsize = leafsize = tail->nelems;
    for(--i; i >= 0; i--, depth++, shift += FX_RRB_RADIX_BITS) {
        node = stack[i];
        child = stack[i+1];
        int nelems = node->nelems;
        if(nelems + grow <= FX_RRB_RADIX) {
            if (!inplace) {
                node = fx_rrb_copynode(vec, node);
                if(!node) return 0;
                if (!grow) {
                    fx_rrbnode_t* rightmost = node->child[nelems-1];
                    //node->total -= FX_RRB_TOTAL(rightmost);
                    fx_rrb_freenode(info, &rightmost);
                    node->child[nelems-1] = 0;
                } else {
                    node->nelems = ++nelems;
                    grow = 0;
                }
            } else if (grow) {
                node->nelems = ++nelems;
                grow = 0;
            } else {
                //node->total -= FX_RRB_TOTAL(node->child[nelems-1]);
            }
        } else {
            node = fx_rrb_newnode(info, (depth << FX_RRB_DEPTH_SHIFT) + shift, true);
            node->nelems = nelems = 1;
        }
        node->child[nelems-1] = child;
        //node->total += FX_RRB_TOTAL(child);
        if (FX_RRB_HAVE_SIZES(node->flags)) {
            int_* sizes = FX_RRB_SIZES(node);
            int_ prefixsize = nelems == 1 ? 0 : sizes[nelems-2];
            childsize += prefixsize;
            sizes[nelems-1] = childsize;
        } else {
            int_ prefixsize = nelems == 1 ? 0 : (1 << FX_RRB_RADIX_BITS*(depth-1))*nleaf_elems*(nelems-1);
            childsize += prefixsize;
        }
        node->total = childsize;
        stack[i] = node;
    }

    if (!grow) {
        root = stack[0];
    } else {
        int have_sizes = root->flags & FX_RRB_FLAG_SIZES;
        node = fx_rrb_newnode(info, (depth << FX_RRB_DEPTH_SHIFT) + shift + have_sizes, true);
        node->nelems = 2;
        FX_INCREF(root->rc);
        node->child[0] = root;
        node->child[1] = stack[0];
        node->total = FX_RRB_TOTAL(root) + FX_RRB_TOTAL(stack[0]);
        if (have_sizes) {
            int_* sizes = FX_RRB_SIZES(node);
            int_* prev_root_sizes = FX_RRB_SIZES(root);
            sizes[0] = prev_root_sizes[root->nelems-1];
            sizes[1] = sizes[0] + leafsize;
        }
        root = node;
    }
    //fx_rrb_check_tree(root, 0);
    return root;
}

int fx_rrb_append(const fx_rrbvec_t* vec, const char* elems, int_ nelems, fx_rrbvec_t* result)
{
    const fx_rrbinfo_t* info = &vec->info;
    const fx_rrbnode_t* root0 = vec->root;
    int_ nmax_leafelems = 1 << info->nleafelems_log, delta = 0;
    //int_ size0 = vec->size;
    //const char* elems0 = elems;
    int elemsize = info->elemsize;
    fx_rrb_copy(vec, result);
    for (;nelems > 0; nelems -= delta) {
        fx_rrbnode_t* tail = result->tail;
        int ntail_elems = tail ? tail->nelems : 0;
        delta = RRB_MIN(nmax_leafelems - ntail_elems, nelems);
        int_ newtail_elems = delta + ntail_elems;
        if (delta == 0) {
            if (tail != 0) {
                fx_rrbnode_t* root = fx_rrb_push_tail(result, result->root != root0);
                if (root != result->root) {
                    fx_rrb_freenode(info, &result->root);
                    result->root = root;
                }
                result->tail = 0;
            }
            continue;
        }
        {
        fx_rrbnode_t* new_tail = fx_rrb_newleaf(info, newtail_elems);
        char* new_data = FX_RRB_DATA(new_tail);
        fx_rrb_copy_elems(tail ? FX_RRB_DATA(tail) : 0, new_data, ntail_elems*elemsize, info);
        fx_rrb_copy_elems(elems, new_data + elemsize*ntail_elems, delta*elemsize, info);
        elems += delta*elemsize;
        result->size += delta;
        fx_rrb_freenode(info, &result->tail);
        result->tail = new_tail;
        //int_ nelems_added = (elems - elems0)/elemsize;
        //assert(nelems_added + size0 == new_tail->nelems + (result->root ? FX_RRB_TOTAL(result->root) : 0));
        }
    }
    return FX_OK;
}

static int fx_rrb_create_concat_plan(const fx_rrbinfo_t* info, fx_rrbnode_t** all, int nall, int* node_count)
{
    int flags = all[0]->flags;
    int radix = FX_RRB_IS_LEAF(flags) ? 1 << info->nleafelems_log : FX_RRB_RADIX;
    int i, total_nodes = 0;
    for (i = 0; i < nall; i++) {
        int size = all[i]->nelems;
        node_count[i] = size;
        total_nodes += size;
    }

    int optimal_slots = (total_nodes + radix - 1) / radix;
    int shuffled_len = nall, extra = radix >= 4 ? FX_RRB_EXTRA : 1;

    for (i = 0; optimal_slots + extra < shuffled_len; ) {
        while (node_count[i] >= radix - extra/2)
            i++;
        int remaining_nodes = node_count[i];
        for (;remaining_nodes > 0 && i < shuffled_len-1; i++) {
            int min_size = RRB_MIN(remaining_nodes + node_count[i+1], radix);
            node_count[i] = min_size;
            remaining_nodes = remaining_nodes + node_count[i+1] - min_size;
        }
        if (remaining_nodes > 0) {
            node_count[i] = remaining_nodes;
            break;
        }
        for (int j = i; j < shuffled_len - 1; j++)
            node_count[j] = node_count[j+1];
        shuffled_len--;
        i--;
    }
    /*int total_nodes_after = 0;
    for( i = 0; i < shuffled_len; i++ )
    {
        total_nodes_after += node_count[i];
    }
    assert(total_nodes_after == total_nodes);*/

    return shuffled_len;
}

static void fx_rrb_calc_sizes(fx_rrbnode_t* node)
{
    int_* sizes = FX_RRB_SIZES(node);
    int shift = FX_RRB_SHIFT(node->flags);
    int i, nelems = node->nelems;
    int_ total = 0;
    for(i = 0; i < nelems; i++) {
        int_ total_i = FX_RRB_TOTAL(node->child[i]);
        total += total_i;
        sizes[i] = total;
    }
    node->total = total;
    if(node->total == 1 << (shift + FX_RRB_RADIX_BITS))
        node->flags &= ~FX_RRB_FLAG_SIZES;
}

static bool fx_rrb_execute_concat_plan(const fx_rrbinfo_t* info, fx_rrbnode_t** all, int nall,
                                       const int* node_count, fx_rrbnode_t** new_all,
                                       int new_len, int flags)
{
    int i, idx = 0, offset = 0;
    if (FX_RRB_IS_LEAF(flags)) {
        int_ elemsize = info->elemsize;
        for (i = 0; i < new_len; i++) {
            int new_size = node_count[i];
            fx_rrbnode_t *old = all[idx];
            if (offset == 0 && new_size == old->nelems) {
                idx++;
                new_all[i] = (fx_rrbnode_t*)old;
                FX_INCREF(old->rc);
            }
            else {
                fx_rrbnode_t* new_node = fx_rrb_newleaf(info, new_size);
                char* new_data = FX_RRB_DATA(new_node);
                int curr_size = 0;
                if(!new_node) return false;
                while(curr_size < new_size) {
                    const fx_rrbnode_t *old_node = (const fx_rrbnode_t*)all[idx];
                    const char* old_data = FX_RRB_DATA(old_node);
                    int old_remaining = old_node->nelems - offset;
                    int new_needed = new_size - curr_size;
                    int copied = RRB_MIN(old_remaining, new_needed);
                    fx_rrb_copy_elems(old_data + offset*elemsize,
                                      new_data + curr_size*elemsize,
                                      copied*elemsize, info);
                    curr_size += copied;
                    offset += copied;
                    if (offset == old_node->nelems) {
                        idx++;
                        offset = 0;
                    }
                }
                new_all[i] = new_node;
            }
        }
    } else {
        for (i = 0; i < new_len; i++) {
            int new_size = node_count[i];
            fx_rrbnode_t *old = all[idx];
            if (offset == 0 && new_size == old->nelems) {
                idx++;
                new_all[i] = old;
                FX_INCREF(old->rc);
            }
            else {
                fx_rrbnode_t* new_node = fx_rrb_newnode(info, flags | FX_RRB_FLAG_SIZES, true);
                if(!new_node) return false;
                int curr_size = 0;
                while(curr_size < new_size) {
                    const fx_rrbnode_t *old_node = all[idx];
                    int old_remaining = old_node->nelems - offset;
                    int new_needed = new_size - curr_size;
                    int j, copied = RRB_MIN(old_remaining, new_needed);

                    for(j = 0; j < copied; j++) {
                        fx_rrbnode_t* n = old_node->child[offset+j];
                        new_node->child[curr_size+j] = n;
                        FX_INCREF(n->rc);
                    }
                    curr_size += copied;
                    offset += copied;
                    if (offset == old_node->nelems) {
                        idx++;
                        offset = 0;
                    }
                }
                new_node->nelems = new_size;
                fx_rrb_calc_sizes(new_node);
                new_all[i] = new_node;
            }
        }
    }
    //for( i = 0; i < new_len; i++ )
    //    assert(new_all[i]->nelems == node_count[i]);

    return true;
}

static fx_rrbnode_t* fx_rrb_rebalance(const fx_rrbinfo_t* info, const fx_rrbnode_t* left,
                                      fx_rrbnode_t* center, const fx_rrbnode_t* right,
                                      bool is_top)
{
    int nleft = left ? left->nelems-1 : 0, ncenter = center->nelems, nright = right ? right->nelems-1 : 0;
    int i, nall = nleft + ncenter + nright;
    assert(nall <= FX_RRB_RADIX*2);
    fx_rrbnode_t* all[FX_RRB_RADIX*2];
    fx_rrbnode_t* new_all[FX_RRB_RADIX*2];
    int node_count[FX_RRB_RADIX*2];
    if (nleft > 0) {
        memcpy(all, left->child, nleft*sizeof(all[0]));
    }
    memcpy(&all[nleft], center->child, ncenter*sizeof(all[0]));
    if (nright > 0) {
        memcpy(&all[nleft+ncenter], &right->child[1], nright*sizeof(all[0]));
    }
    int flags = all[0]->flags;
    int center_flags = center->flags;
    int new_len = fx_rrb_create_concat_plan(info, all, nall, node_count);
    //memset(new_all, 0, sizeof(new_all));
    bool ok = fx_rrb_execute_concat_plan(info, all, nall, node_count, new_all, new_len, flags);
    /*
    int total = 0;
    for(i = 0; i < nall; i++) {
        total += all[i]->total;
    }
    int total_after = 0;
    for(i = 0; i < new_len; i++) {
        total_after += new_all[i]->total;
    }
    assert(total == total_after);*/
    fx_rrbnode_t* center_ = center;
    fx_rrb_freenode(info, &center_);
    if (ok) {
        int depth = FX_RRB_DEPTH(center_flags), shift = FX_RRB_SHIFT(center_flags);
        int result_flags = ((depth+1) << FX_RRB_DEPTH_SHIFT) + shift + FX_RRB_RADIX_BITS + FX_RRB_FLAG_SIZES;
        fx_rrbnode_t* new_left = fx_rrb_newnode(info, center_flags | FX_RRB_FLAG_SIZES, true);
        fx_rrbnode_t* new_right = 0, *result;
        int new_nleft = RRB_MIN(new_len, FX_RRB_RADIX);
        for(i = 0; i < new_nleft; i++) {
            new_left->child[i] = new_all[i];
        }
        new_left->nelems = new_nleft;
        fx_rrb_calc_sizes(new_left);
        if(new_len > new_nleft) {
            new_right = fx_rrb_newnode(info, center_flags | FX_RRB_FLAG_SIZES, true);
            int new_nright = new_len - new_nleft;
            assert(new_nright <= FX_RRB_RADIX);
            for(i = 0; i < new_nright; i++) {
                new_right->child[i] = new_all[i+new_nleft];
            }
            new_right->nelems = new_nright;
            fx_rrb_calc_sizes(new_right);
        }
        if (!new_right && is_top) {
            //fx_rrb_check_tree(new_left, 0);
            return new_left;
        }
        result = fx_rrb_newnode(info, result_flags, true);
        result->child[0] = new_left;
        result->nelems = 1;
        if(new_right) {
            result->child[result->nelems++] = new_right;
        }
        fx_rrb_calc_sizes(result);
        //fx_rrb_check_tree(result, 0);
        return result;
    }

    return 0;
}

static fx_rrbnode_t* fx_rrb_concat_trees(const fx_rrbinfo_t* info, fx_rrbnode_t* left,
                                         fx_rrbnode_t* right, bool is_top)
{
    assert(left != 0 && right != 0);
    int lflags = left->flags, rflags = right->flags;
    int ldepth = FX_RRB_DEPTH(lflags), rdepth = FX_RRB_DEPTH(rflags);
    if (ldepth > rdepth) {
        fx_rrbnode_t* center = fx_rrb_concat_trees(info, left->child[left->nelems-1], right, false);
        fx_rrbnode_t* result = fx_rrb_rebalance(info, left, center, 0, is_top);
        assert(result->total == left->total + right->total);
        return result;
    } else if(ldepth < rdepth) {
        fx_rrbnode_t* center = fx_rrb_concat_trees(info, left, right->child[0], false);
        fx_rrbnode_t* result = fx_rrb_rebalance(info, 0, center, right, is_top);
        assert(result->total == left->total + right->total);
        return result;
    } else if (FX_RRB_IS_LEAF(lflags)) {
        int_ nleafelems = 1 << info->nleafelems_log;
        fx_rrbnode_t* node = fx_rrb_newnode(info, (1 << FX_RRB_DEPTH_SHIFT) + info->nleafelems_log, true);
        int_ total = left->nelems + right->nelems;
        if (is_top && total <= nleafelems) {
            fx_rrbnode_t* merged = fx_rrb_newleaf(info, total);
            char* mdata = FX_RRB_DATA(merged);
            int_ leftsize = left->nelems*info->elemsize, rightsize = right->nelems*info->elemsize;
            fx_rrb_copy_elems(FX_RRB_DATA(left), mdata, leftsize, info);
            fx_rrb_copy_elems(FX_RRB_DATA(right), mdata + leftsize, rightsize, info);
            node->nelems = 1;
            node->child[0] = merged;
        } else {
            node->nelems = 2;
            node->child[0] = left;
            node->child[1] = right;
            FX_INCREF(left->rc);
            FX_INCREF(right->rc);
        }
        node->total = total;
        //fx_rrb_check_tree(node, 0);
        return node;
    } else {
        fx_rrbnode_t* center = fx_rrb_concat_trees(info, left->child[left->nelems-1], right->child[0], false);
        fx_rrbnode_t* result = fx_rrb_rebalance(info, left, center, right, is_top);
        assert(result->total == left->total + right->total);
        return result;
    }
}

int fx_rrb_concat(const fx_rrbvec_t* leftvec, const fx_rrbvec_t* rightvec, fx_rrbvec_t* vec)
{
    int status = FX_OK;
    if (leftvec->size == 0) {
        fx_rrb_copy(rightvec, vec);
        return FX_OK;
    }
    if (rightvec->root == 0) {
        fx_rrbnode_t* right_tail = rightvec->tail;

        if (!right_tail) {
            fx_rrb_copy(leftvec, vec);
        } else {
            status = fx_rrb_append(leftvec, FX_RRB_DATA(right_tail), right_tail->nelems, vec);
            //fx_rrb_check_tree(vec->root, 0);
        }
        return status;
    }
    {
        const fx_rrbinfo_t* info = &leftvec->info;
        int nleafelems_log = info->nleafelems_log;
        fx_rrbnode_t* left_root;
        if (leftvec->tail) {
            left_root = fx_rrb_push_tail((fx_rrbvec_t*)leftvec, false);
            if (!left_root)
                FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
            FX_INCREF(leftvec->tail->rc);
        } else {
            left_root = leftvec->root;
            FX_INCREF(leftvec->root->rc);
        }
        fx_rrb_make_empty_(info, vec);
        vec->root = fx_rrb_concat_trees(info, left_root, (fx_rrbnode_t*)rightvec->root, true);
        if (!vec->root)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
        vec->size = leftvec->size + rightvec->size;
        vec->tail = rightvec->tail;
        if (vec->tail) FX_INCREF(vec->tail->rc);
        fx_rrb_freenode(info, &left_root);
        return FX_OK;
    }
}

enum {
    FX_RRB_LOCAL_BUFSIZE = 2048
};

static int fx_rrb_inverse(const fx_rrbvec_t* src, int_ start, int_ end, fx_rrbvec_t* dst)
{
    int fx_status = FX_OK;
    const fx_rrbinfo_t* info = &src->info;
    fx_rrbiter_t srciter, dstiter;
    int_ size = start - end;
    int_ i, j, blocksize = 0;
    char* srcptr = fx_rrb_start_read(src, &srciter, start, -1);
    char* dstptr = fx_rrb_start_write(src->info.elemsize, src->info.free_f,
                                      src->info.copy_f, dst, &dstiter);
    int elemsize = info->elemsize;
    char localbuf[FX_RRB_LOCAL_BUFSIZE], *buf = localbuf;
    int_ max_bufelems = FX_RRB_LOCAL_BUFSIZE/elemsize;
    max_bufelems = RRB_MIN(RRB_MAX(max_bufelems, 1), size);
    int_ bufsize = elemsize*max_bufelems;
    if (bufsize > FX_RRB_LOCAL_BUFSIZE) {
        buf = (char*)fx_malloc(bufsize);
        if(!buf)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    for(i = 0; i < size; i += blocksize) {
        blocksize = RRB_MIN(size - i, max_bufelems);
        for(j = 0; j < blocksize; j++) {
            memcpy(buf + j*elemsize, srcptr, elemsize);
            srcptr -= elemsize;
            if(srcptr < srciter.blockstart)
                srcptr = fx_rrb_next(&srciter);
        }
        if((fx_status = fx_rrb_write(&dstiter, &dstptr, buf, blocksize)) < 0)
            break;
    }
    fx_rrb_end_write(&dstiter, dstptr);
    if(buf != localbuf)
        fx_free(buf);
    return fx_status;
}

int fx_rrb_slice(const fx_rrbvec_t* vec0, int_ left, int_ right, int delta, int mask, fx_rrbvec_t* vec)
{
    const fx_rrbinfo_t* info = &vec0->info;
    int_ size0 = vec0->size;
    int status = FX_OK;
    left = !(mask & 1) ? left : delta > 0 ? 0 : size0-1;
    right = !(mask & 2) ? right : delta > 0 ? size0 : -1;
    if (delta == 0)
        FX_FAST_THROW_RET(FX_EXN_ZeroStepError);
    if (delta != 1 && delta != -1)
        FX_FAST_THROW_RET(FX_EXN_BadArgError);
    if (left == right) {
        fx_rrb_make_empty_(info, vec);
        return FX_OK;
    }
    if ((size_t)left >= (size_t)size0 || (size_t)(right - delta) >= (size_t)size0)
        FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    if (delta < 0)
        return fx_rrb_inverse(vec0, left, right, vec);

    {
    const fx_rrbnode_t* tail = vec0->tail;
    int ntail_elems = tail ? tail->nelems : 0;
    int_ tail_fst = size0 - ntail_elems;
    fx_rrbvec_t tempvec;
    fx_rrb_make_empty_(info, &tempvec);
    if (left < tail_fst) {
        // extract a part of the tree first and then append a part of the tail (if any)
        fx_rrbnode_t* root = vec0->root;
        int right_ = RRB_MIN(right, tail_fst)-1;
        int left_idx[FX_RRB_MAX_DEPTH], right_idx[FX_RRB_MAX_DEPTH];
        fx_rrbnode_t *left_path[FX_RRB_MAX_DEPTH], *right_path[FX_RRB_MAX_DEPTH];

        int_ lidx = left, ridx = right_;
        int i, j, k, depth = fx_rrb_findpath(root, &lidx, left_path, left_idx);
        int rdepth = fx_rrb_findpath(root, &ridx, right_path, right_idx);
        assert(depth >= 0 && depth == rdepth);

        int new_root_idx = -1;
        for(i = 0; i <= depth; i++) {
            if (left_path[i] != right_path[i]) break;
            new_root_idx = i;
        }
        if (new_root_idx == depth) {
            // both left and _right are inside the same leaf;
            // just copy the correponding part of the leaf to the new tree
            fx_rrbvec_t tempvec1;
            fx_rrb_make_empty_(info, &tempvec1);
            fx_rrbnode_t* leaf = left_path[new_root_idx];
            char* ptr = FX_RRB_DATA(leaf);
            fx_rrb_append(&tempvec1, ptr + lidx*info->elemsize, ridx - lidx + 1, &tempvec);
        } else {
            assert(new_root_idx >= 0);
            int nleaf_elems = 1 << info->nleafelems_log;
            int_ elemsize = info->elemsize;
            // process the left and the right branches,
            // truncate them on the left and on the right, respectively
            fx_rrbnode_t* leaf = left_path[depth];
            bool lupdated = false, rupdated = false;
            if (lidx == 0) {
                FX_INCREF(leaf->rc);
            } else {
                fx_rrbnode_t* newleaf = fx_rrb_newleaf(info, leaf->nelems - lidx);
                fx_rrb_copy_elems(FX_RRB_DATA(leaf) + lidx*elemsize,
                                  FX_RRB_DATA(newleaf), newleaf->nelems*elemsize, info);
                left_path[depth] = newleaf;
                lupdated = true;
            }
            leaf = right_path[depth];
            if (ridx == leaf->nelems-1) {
                FX_INCREF(leaf->rc);
            } else {
                fx_rrbnode_t* newleaf = fx_rrb_newleaf(info, ridx+1);
                fx_rrb_copy_elems(FX_RRB_DATA(leaf), FX_RRB_DATA(newleaf),
                                  newleaf->nelems*elemsize, info);
                right_path[depth] = newleaf;
                rupdated = true;
            }
            for(i = depth-1; i > new_root_idx; i--) {
                fx_rrbnode_t* node = left_path[i];
                fx_rrbnode_t* child = left_path[i+1];
                lidx = left_idx[i];
                if (lidx == 0 && !lupdated) {
                    // preserve the part of the tree until it does not have to be truncated
                    FX_DECREF(child->rc);
                    FX_INCREF(node->rc);
                } else {
                    fx_rrbnode_t* newnode = fx_rrb_newnode(info, node->flags | FX_RRB_FLAG_SIZES, true);
                    newnode->nelems = node->nelems - lidx;
                    newnode->child[0] = child;
                    for(j = 1; j < newnode->nelems; j++) {
                        child = node->child[lidx + j];
                        FX_INCREF(child->rc);
                        newnode->child[j] = child;
                    }
                    fx_rrb_calc_sizes(newnode);
                    left_path[i] = newnode;
                    lupdated = true;
                }

                node = right_path[i];
                child = right_path[i+1];
                ridx = right_idx[i];
                if (ridx == node->nelems-1 && !rupdated) {
                    // preserve the part of the tree until it does not have to be truncated
                    FX_DECREF(child->rc);
                    FX_INCREF(node->rc);
                } else {
                    fx_rrbnode_t* newnode = fx_rrb_newnode(info, node->flags | FX_RRB_FLAG_SIZES, true);
                    newnode->nelems = ridx + 1;
                    newnode->child[ridx] = child;
                    for(j = 0; j < newnode->nelems-1; j++) {
                        child = node->child[j];
                        FX_INCREF(child->rc);
                        newnode->child[j] = child;
                    }
                    fx_rrb_calc_sizes(newnode);
                    right_path[i] = newnode;
                    rupdated = true;
                }
            }
            // ok, we reached the "new root" level, i.e. the internal node
            // left_path[new_root_idx] == right_path[new_root_idx]
            // (it can also be a root of the original tree), where the
            // extracted slice is represented by subnodes
            // left_idx[new_root_idx] <= j <= right_idx[new_root_idx], where
            // left_idx[new_root_idx] < right_idx[new_root_idx].
            // we try to reshuffle data between the subnodes using a procedure similar to
            // fx_rrb_rebalance().
            fx_rrbnode_t* all[FX_RRB_RADIX*2];
            int node_count[FX_RRB_RADIX*2];
            fx_rrbnode_t* new_all[FX_RRB_RADIX*2];
            fx_rrbnode_t* root = left_path[new_root_idx];
            fx_rrbnode_t* lnode = left_path[new_root_idx+1];
            fx_rrbnode_t* rnode = right_path[new_root_idx+1];
            assert(root == right_path[new_root_idx]);
            lidx = left_idx[new_root_idx];
            ridx = right_idx[new_root_idx];
            assert(lidx < ridx);
            bool two_level_rebalance = !FX_RRB_IS_LEAF(lnode->flags) &&
                (lidx + 1 == ridx || (lidx + 2 == ridx &&
                lnode->nelems + root->child[lidx+1]->nelems + rnode->nelems < FX_RRB_RADIX*2));
            if (two_level_rebalance) {
                for(i = lidx, k = 0; i <= ridx; i++) {
                    fx_rrbnode_t* child = i == lidx ? lnode : i == ridx ? rnode : root->child[i];
                    for(j = 0; j < child->nelems; j++, k++)
                        all[k] = child->child[j];
                }
            } else {
                for(i = lidx, k = 0; i <= ridx; i++, k++) {
                    fx_rrbnode_t* child = i == lidx ? lnode : i == ridx ? rnode : root->child[i];
                    all[k] = child;
                }
            }
            int nall = k, flags = all[0]->flags;
            int new_len = fx_rrb_create_concat_plan(info, all, nall, node_count);
            bool ok = fx_rrb_execute_concat_plan(info, all, nall, node_count, new_all, new_len, flags);

            int depth = FX_RRB_DEPTH(flags), shift = FX_RRB_SHIFT(flags);
            int nleft_flags = ((depth+1) << FX_RRB_DEPTH_SHIFT) + shift + FX_RRB_RADIX_BITS;
            fx_rrbnode_t* new_left = fx_rrb_newnode(info, nleft_flags | FX_RRB_FLAG_SIZES, true);
            fx_rrbnode_t* new_right = 0, *result;
            int new_nleft = RRB_MIN(new_len, FX_RRB_RADIX);
            for(i = 0; i < new_nleft; i++) {
                new_left->child[i] = new_all[i];
            }
            new_left->nelems = new_nleft;
            fx_rrb_calc_sizes(new_left);
            if(new_len > new_nleft) {
                new_right = fx_rrb_newnode(info, nleft_flags | FX_RRB_FLAG_SIZES, true);
                int new_nright = new_len - new_nleft;
                assert(new_nright <= FX_RRB_RADIX);
                for(i = 0; i < new_nright; i++) {
                    new_right->child[i] = new_all[i+new_nleft];
                }
                new_right->nelems = new_nright;
                fx_rrb_calc_sizes(new_right);
            }
            if (!new_right) {
                root = new_left;
            } else {
                int result_flags = ((depth+2) << FX_RRB_DEPTH_SHIFT) + shift + FX_RRB_RADIX_BITS*2;
                root = fx_rrb_newnode(info, result_flags | FX_RRB_FLAG_SIZES, true);
                root->nelems = 2;
                root->child[0] = new_left;
                root->child[1] = new_right;
                fx_rrb_calc_sizes(root);
            }
            fx_rrb_freenode(info, &lnode);
            fx_rrb_freenode(info, &rnode);
            tempvec.root = root;
            tempvec.size = root->total;
        }
    }
    if (right > tail_fst) {
        int left_ = RRB_MAX(tail_fst, left);
        char* data = FX_RRB_DATA(tail) + (left_ - tail_fst)*info->elemsize;
        status = fx_rrb_append(&tempvec, data, right - left_, vec);
        fx_rrb_free(&tempvec);
    } else {
        *vec = tempvec;
    }
    }
    return status;
}

int fx_rrb_make(int_ size, size_t elemsize, fx_free_t free_elem,
                fx_copy_t copy_elem, const void* data, fx_rrbvec_t* vec)
{
    fx_rrbiter_t iter;
    char* ptr = fx_rrb_start_write(elemsize, free_elem, copy_elem, vec, &iter);
    int fx_status = fx_rrb_write(&iter, &ptr, (const char*)data, size);
    fx_rrb_end_write(&iter, ptr);
    return fx_status;
}

#ifdef __cplusplus
}
#endif

#endif
