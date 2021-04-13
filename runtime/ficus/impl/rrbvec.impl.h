/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    This is implementation of Relaxed Radix Balanced Trees (RRB Trees)
    and the immutable vector based on it.

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
#include <vector>

typedef intptr_t int_;
#define FX_INCREF(rc) ((rc)++)
#define FX_DECREF(rc) ((rc)--)

#undef MIN
#undef MAX
#define MIN(a, b) ((a) <= (b) ? (a) : (b))
#define MAX(a, b) ((a) >= (b) ? (a) : (b))

enum
{
    FX_RRB_RADIX_BITS = 5,
    FX_RRB_RADIX = 1 << FX_RRB_RADIX_BITS,
    FX_RRB_MAX_DEPTH = 14,
    FX_RRB_SHIFT_MASK = 255,
    FX_RRB_DEPTH_MASK = 255,
    FX_RRB_DEPTH_SHIFT = 8,
    FX_RRB_FLAG_NODE = 0 << 16,
    FX_RRB_FLAG_LEAF = 1 << 16,
    FX_RRB_FLAG_SIZES = 2 << 16
};

typedef void (*fx_copy_t)(const void* src, void* dst);
typedef void (*fx_free_t)(void* dst);

typedef struct fx_rrbinfo_t
{
    int elemsize, nleafelems_log;
    fx_copy_t copy_f;
    fx_free_t free_f;
} fx_rrbinfo_t;

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

typedef struct fx_rrbvec_t
{
    int_ size;
    const fx_rrbinfo_t* info;
    struct fx_rrbnode_t* root;
    struct fx_rrbleaf_t* tail;
} fx_rrbvec_t;

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

void* fx_malloc(size_t sz)
{
    return malloc(sz);
}

void fx_free(void* ptr)
{
    free(ptr);
}

char* fx_rrb_find(const fx_rrbvec_t* vec, int_ index)
{
    int_ idx = index, size = vec->size;
    if ((size_t)idx >= (size_t)size) return 0;
    {
    const fx_rrbleaf_t* tail = vec->tail;
    int tsize = tail ? tail->nelems : 0;
    int_ elemsize = vec->info->elemsize, tail_fst = size - tsize;
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
}

typedef struct fx_rrbiter_t {
    int depth, elemsize;
    char* blockend;
    fx_rrbvec_t* vec;
    fx_copy_t copy_f;
    int istack[FX_RRB_MAX_DEPTH];
    fx_rrbnode_t* nstack[FX_RRB_MAX_DEPTH];
} fx_rrbiter_t;

#define FX_RRB_NEXT(typ, iter, ptr) \
{ \
    if(++(ptr) >= (typ*)(iter).blockend) { \
        (ptr) = (typ*)fx_rrb_next(&(iter)); \
    } \
}

char* fx_rrb_start_read(const fx_rrbvec_t* vec, fx_rrbiter_t* iter, int_ startidx)
{
    fx_rrbnode_t* root = vec->root;
    int_ idx = startidx, size = vec->size;
    int_ elemsize = vec->info->elemsize;
    const fx_rrbleaf_t* tail = vec->tail;
    int i = 0, tsize = tail ? tail->nelems : 0;
    int_ tail_fst = size - tsize;
    char* ptr;

    iter->vec = (fx_rrbvec_t*)vec;
    iter->elemsize = vec->info->elemsize;
    iter->copy_f = vec->info->copy_f;
    if (idx >= tail_fst) {
        ptr = FX_RRB_DATA(tail);
        iter->depth = 0;
        iter->blockend = ptr + tail->nelems*elemsize;
        return ptr + (idx - tail_fst)*elemsize;
    } else {
        fx_rrbnode_t* node = vec->root;
        for(;;i++) {
            int flags = node->flags, nidx;
            iter->nstack[i] = node;
            if (!FX_RRB_HAVE_SIZES(flags)) {
                // the node and its children are all perfectly balanced, use the fast path
                int sh = FX_RRB_SHIFT(flags);
                for(;; sh -= FX_RRB_RADIX_BITS, i++) {
                    if (FX_RRB_IS_LEAF(flags)) {
                        ptr = FX_RRB_DATA(node);
                        iter->blockend = ptr + node->nelems*elemsize;
                        iter->depth = i;
                        ptr += idx*elemsize;
                        return ptr;
                    }
                    iter->nstack[i] = node;
                    nidx = (int)(idx >> sh) & (FX_RRB_RADIX-1);
                    idx -= nidx << sh;
                    iter->istack[i] = nidx;
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
                iter->istack[i] = nidx;
                node = node->child[nidx];
            }
        }
    }
}

char* fx_rrb_next(fx_rrbiter_t* iter)
{
    int i, depth = iter->depth;
    if (depth == 0) return 0;

    for(i = depth-1; i >= 0; i--) {
        fx_rrbnode_t* node = iter->nstack[i];
        int nidx = iter->istack[i] + 1;
        if (nidx >= node->nelems)
            continue;
        for(;;) {
            iter->istack[i++] = nidx;
            node = node->child[nidx];
            if (FX_RRB_IS_LEAF(node->flags)) {
                char* ptr = FX_RRB_DATA(node);
                iter->blockend = ptr + node->nelems*iter->elemsize;
                iter->depth = i;
                return ptr;
            }
            iter->nstack[i] = node;
            nidx = 0;
        }
    }
    {
    fx_rrbleaf_t* tail = iter->vec->tail;
    char* ptr = FX_RRB_DATA(tail);
    iter->blockend = ptr + tail->nelems*iter->elemsize;
    iter->depth = 0;
    return ptr;
    }
}

static fx_rrbleaf_t* fx_rrb_newleaf(const fx_rrbinfo_t* info, int_ maxelems)
{
    size_t datasize = maxelems*info->elemsize;
    fx_rrbleaf_t* leaf = (fx_rrbleaf_t*)malloc(FX_RRB_LEAF_HDR_SIZE + datasize);
    if (leaf) {
        leaf->rc = 1;
        leaf->flags = FX_RRB_FLAG_LEAF;
        leaf->nelems = 0;
        if (info->free_f)
            memset(FX_RRB_DATA(leaf), 0, info->elemsize*maxelems);
    }
    return leaf;
}

static fx_rrbnode_t* fx_rrb_newnode(const fx_rrbinfo_t* info, int flags, bool init_sizes)
{
    bool have_sizes = FX_RRB_HAVE_SIZES(flags);
    size_t datasize = have_sizes ? sizeof(int_)*FX_RRB_RADIX : 0;
    fx_rrbnode_t* node = (fx_rrbnode_t*)malloc(sizeof(fx_rrbnode_t) + datasize);
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
    } else {
        int elemsize = info->elemsize;
        for(int i = 0; i < sz; i += elemsize) {
            copy_f(src + i, dst + i);
        }
    }
}

// Add more elements to the vector; modify the vector in-place.
// That is, the function treats vector as a mutable structure.
// The function is intended to be used in vector comprehension
// or vector literal construction operations.
char* fx_rrbiter_push_back(fx_rrbiter_t* iter, char* ptr, const char* elems, int_ nelems)
{
    const fx_rrbinfo_t* info = iter->vec->info;
    char* blockend = iter->blockend;
    int elemsize = iter->elemsize;
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
            int nleafelems = 1 << vec->info->nleafelems_log;
            if(vec->tail) {
                // add the existing "tail" to the tree
                fx_rrbnode_t* child = (fx_rrbnode_t*)vec->tail;
                int shift = vec->info->nleafelems_log, depth = 1;
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
                        break;
                    }
                    node = fx_rrb_newnode(info, (depth << FX_RRB_DEPTH_SHIFT) + shift, true);
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
            }
            vec->tail = fx_rrb_newleaf(info, 1 << info->nleafelems_log);
            ptr = FX_RRB_DATA(vec->tail);
            iter->blockend = blockend = ptr + nleafelems*elemsize;
        }
    }
    return ptr;
}

void fx_rrb_empty_vec(const fx_rrbinfo_t* info, fx_rrbvec_t* vec)
{
    vec->size = 0;
    vec->info = info;
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
            int i, nleafelems = 1 << info->nleafelems_log;
            int elemsize = info->elemsize;
            fx_free_t free_f = info->free_f;
            char* ptr = FX_RRB_DATA(node);
            for(i = 0; i < nleafelems; i++) {
                free_f(ptr + i*elemsize);
            }
        }
    }
    node->flags = 0;
    node->nelems = 0;
    fx_free(node);
}

void fx_rrb_free(fx_rrbvec_t* vec)
{
    if(vec->root && FX_DECREF(vec->root->rc) == 1) {
        fx_rrb_freenode_(vec->info, vec->root);
    }
    if(vec->tail && FX_DECREF(vec->tail->rc) == 1) {
        fx_rrb_freenode_(vec->info, (fx_rrbnode_t*)vec->tail);
    }
    vec->root = 0;
    vec->tail = 0;
    vec->size = 0;
}

// in theory, 0 elements can be written to a vector,
// so we don't allocate anything until the first element is
// written to the vector
char* fx_rrb_start_write(fx_rrbvec_t* vec, fx_rrbiter_t* iter)
{
    assert(vec->size == 0 && vec->root == 0 && vec->tail == 0);

    iter->vec = vec;
    iter->depth = 0;
    iter->elemsize = vec->info->elemsize;
    iter->blockend = 0;
    iter->copy_f = vec->info->copy_f;
    return 0;
}

void fx_rrb_end_write(fx_rrbiter_t* iter, char* ptr)
{
    if(iter->vec->tail) {
        char* ptr0 = FX_RRB_DATA(iter->vec->tail);
        int_ nelems = (ptr - ptr0)/iter->elemsize;
        iter->vec->tail->nelems = nelems;
        iter->vec->size += nelems;
    }
}

void fx_rrb_init_info(fx_rrbinfo_t* info, int elemsize,
                      fx_copy_t copy_f, fx_free_t free_f)
{
    info->copy_f = copy_f;
    info->free_f = free_f;
    info->elemsize = elemsize;
    double nleafelems = (double)(FX_RRB_RADIX*sizeof(int_))/elemsize;
    int nleafelems_log = 0;
    while ((1 << nleafelems_log) < nleafelems)
        nleafelems_log++;
    info->nleafelems_log = nleafelems_log;
}

static fx_rrbnode_t* fx_rrb_copynode(const fx_rrbvec_t* vec, const fx_rrbnode_t* node)
{
    int i, nelems = node->nelems, flags = node->flags;
    fx_rrbnode_t* newnode = fx_rrb_newnode(vec->info, flags, false);
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

static int_ fx_rrb_check_tree(const fx_rrbnode_t* node, int_ startidx, bool allref1=false)
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
            int_ total_i = fx_rrb_check_tree(node->child[i], startidx + total);
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
            int_ total_i = fx_rrb_check_tree(node->child[i], startidx + total);
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

static fx_rrbnode_t* fx_rrb_push_leaf(const fx_rrbvec_t* vec, const fx_rrbleaf_t* leaf, bool inplace)
{
    int i, nleaf_elems_log = vec->info->nleafelems_log, nleaf_elems = 1 << nleaf_elems_log;
    int grow = 1, depth = 1, shift = nleaf_elems_log;
    int_ childsize, leafsize;
    fx_rrbnode_t* stack[FX_RRB_MAX_DEPTH];
    fx_rrbnode_t* root = vec->root, *node = root, *child;
    if (!root) {
        root = fx_rrb_newnode(vec->info, (1 << FX_RRB_DEPTH_SHIFT) + nleaf_elems_log, true);
        root->nelems = 1;
        root->total = leaf->nelems;
        root->child[0] = (fx_rrbnode_t*)leaf;
        return root;
    }
    for(i = 0;; i++) {
        int flags = node->flags;
        if(FX_RRB_IS_LEAF(flags)) break;
        stack[i] = node;
        node = node->child[node->nelems-1];
    }
    stack[i] = (fx_rrbnode_t*)leaf;
    childsize = leafsize = leaf->nelems;
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
                    if (FX_DECREF(rightmost->rc) == 1) {
                        fx_rrb_freenode_(vec->info, rightmost);
                    }
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
            node = fx_rrb_newnode(vec->info, (depth << FX_RRB_DEPTH_SHIFT) + shift, true);
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
        node = fx_rrb_newnode(vec->info, (depth << FX_RRB_DEPTH_SHIFT) + shift + have_sizes, true);
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

bool fx_rrb_append(const fx_rrbvec_t* vec, const char* elems, int_ nelems, fx_rrbvec_t* result)
{
    const fx_rrbinfo_t* info = vec->info;
    const fx_rrbnode_t* root0 = vec->root;
    int_ nmax_leafelems = 1 << info->nleafelems_log, delta = 0;
    //int_ size0 = vec->size;
    //const char* elems0 = elems;
    int elemsize = info->elemsize;
    fx_rrb_copy(vec, result);
    for (;nelems > 0; nelems -= delta) {
        fx_rrbleaf_t* tail = result->tail;
        int ntail_elems = tail ? tail->nelems : 0;
        delta = MIN(nmax_leafelems - ntail_elems, nelems);
        int_ newtail_elems = delta + ntail_elems;
        if (delta == 0) {
            if (tail != 0) {
                fx_rrbnode_t* root = fx_rrb_push_leaf(result, tail, result->root != root0);
                if (root != result->root) {
                    if (result->root && FX_DECREF(result->root->rc) == 1)
                        fx_rrb_freenode_(info, result->root);
                    result->root = root;
                }
                result->tail = 0;
            }
            continue;
        }
        {
        fx_rrbleaf_t* new_tail = fx_rrb_newleaf(info, newtail_elems);
        new_tail->nelems = (int)newtail_elems;
        char* new_data = FX_RRB_DATA(new_tail);
        fx_rrb_copy_elems(tail ? FX_RRB_DATA(tail) : 0, new_data, ntail_elems*elemsize, result->info);
        fx_rrb_copy_elems(elems, new_data + elemsize*ntail_elems, delta*elemsize, result->info);
        elems += delta*elemsize;
        result->size += delta;
        if(result->tail && FX_DECREF(result->tail->rc) == 1)
            fx_rrb_freenode_(info, (fx_rrbnode_t*)result->tail);
        result->tail = new_tail;
        //int_ nelems_added = (elems - elems0)/elemsize;
        //assert(nelems_added + size0 == new_tail->nelems + (result->root ? FX_RRB_TOTAL(result->root) : 0));
        }
    }
    return true;
}

static int fx_rrb_create_concat_plan(fx_rrbnode_t** all, int nall,
                                     int* node_count, int radix)
{
    int i, total_nodes = 0;
    for (i = 0; i < nall; i++) {
        int size = all[i]->nelems;
        node_count[i] = size;
        total_nodes += size;
    }

    int optimal_slots = (total_nodes + radix - 1) / radix;
    int shuffled_len = nall, extra = radix >= 4 ? FX_RRB_EXTRA : 0;

    for (i = 0; optimal_slots + extra < shuffled_len; ) {
        while (node_count[i] > radix - extra/2)
            i++;
        int remaining_nodes = node_count[i];
        while (remaining_nodes > 0 && i < shuffled_len-1) {
            int min_size = MIN(remaining_nodes + node_count[i+1], FX_RRB_RADIX);
            node_count[i] = min_size;
            remaining_nodes = remaining_nodes + node_count[i+1] - min_size;
            i++;
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
    if(node->total == 1 << shift)
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
            fx_rrbleaf_t *old = (fx_rrbleaf_t*)all[idx];
            if (offset == 0 && new_size == old->nelems) {
                idx++;
                new_all[i] = (fx_rrbnode_t*)old;
                FX_INCREF(old->rc);
            }
            else {
                fx_rrbleaf_t* new_node = fx_rrb_newleaf(info, new_size);
                new_node->nelems = new_size;
                char* new_data = FX_RRB_DATA(new_node);
                int curr_size = 0;
                if(!new_node) return false;
                while(curr_size < new_size) {
                    const fx_rrbleaf_t *old_node = (const fx_rrbleaf_t*)all[idx];
                    const char* old_data = FX_RRB_DATA(old_node);
                    int old_remaining = old_node->nelems - offset;
                    int new_needed = new_size - curr_size;
                    int copied = MIN(old_remaining, new_needed);
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
                new_all[i] = (fx_rrbnode_t*)new_node;
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
                    int j, copied = MIN(old_remaining, new_needed);

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
    int radix = FX_RRB_IS_LEAF(flags) ? 1 << info->nleafelems_log : FX_RRB_RADIX;
    int new_len = fx_rrb_create_concat_plan(all, nall, node_count, radix);
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
    if (FX_DECREF(center->rc) == 1) fx_rrb_freenode_(info, center);
    if (ok) {
        int depth = FX_RRB_DEPTH(center_flags), shift = FX_RRB_SHIFT(center_flags);
        int result_flags = ((depth+1) << FX_RRB_DEPTH_SHIFT) + shift + FX_RRB_RADIX_BITS + FX_RRB_FLAG_SIZES;
        fx_rrbnode_t* new_left = fx_rrb_newnode(info, center_flags | FX_RRB_FLAG_SIZES, true);
        fx_rrbnode_t* new_right = 0, *result;
        int new_nleft = MIN(new_len, FX_RRB_RADIX);
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
        return fx_rrb_rebalance(info, left, center, 0, is_top);
    } else if(ldepth < rdepth) {
        fx_rrbnode_t* center = fx_rrb_concat_trees(info, left, right->child[0], false);
        return fx_rrb_rebalance(info, 0, center, right, is_top);
    } else if (FX_RRB_IS_LEAF(lflags)) {
        int_ nleafelems = 1 << info->nleafelems_log;
        fx_rrbnode_t* node = fx_rrb_newnode(info, (1 << FX_RRB_DEPTH_SHIFT) + info->nleafelems_log, true);
        int_ total = left->nelems + right->nelems;
        if (is_top && total < nleafelems) {
            fx_rrbleaf_t* merged = fx_rrb_newleaf(info, nleafelems);
            char* mdata = FX_RRB_DATA(merged);
            int_ leftsize = left->nelems*info->elemsize, rightsize = right->nelems*info->elemsize;
            fx_rrb_copy_elems(FX_RRB_DATA(left), mdata, leftsize, info);
            fx_rrb_copy_elems(FX_RRB_DATA(right), mdata + leftsize, rightsize, info);
            node->nelems = 1;
            node->child[0] = (fx_rrbnode_t*)merged;
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
        return fx_rrb_rebalance(info, left, center, right, is_top);
    }
}

bool fx_rrb_concat(const fx_rrbvec_t* leftvec, const fx_rrbvec_t* rightvec, fx_rrbvec_t* vec)
{
    if (leftvec->size == 0) {
        fx_rrb_copy(rightvec, vec);
        return true;
    }
    if (rightvec->root == 0) {
        fx_rrbleaf_t* right_tail = rightvec->tail;
        bool ok = true;
        if (!right_tail) {
            fx_rrb_copy(leftvec, vec);
        } else {
            ok = fx_rrb_append(leftvec, FX_RRB_DATA(right_tail), right_tail->nelems, vec);
            //fx_rrb_check_tree(vec->root, 0);
        }
        return ok;
    }
    {
        const fx_rrbinfo_t* info = leftvec->info;
        int nleafelems_log = info->nleafelems_log;
        int nleafelems = 1 << nleafelems_log;

        fx_rrbnode_t* left_root;
        if (leftvec->tail) {
            left_root = fx_rrb_push_leaf(leftvec, leftvec->tail, false);
            if (!left_root) return false;
            FX_INCREF(leftvec->tail->rc);
        } else {
            left_root = leftvec->root;
            FX_INCREF(leftvec->root->rc);
        }
        fx_rrb_empty_vec(info, vec);
        vec->root = fx_rrb_concat_trees(info, left_root, (fx_rrbnode_t*)rightvec->root, true);
        if (!vec->root) return false;
        vec->size = leftvec->size + rightvec->size;
        vec->tail = rightvec->tail;
        if (vec->tail) FX_INCREF(vec->tail->rc);
        if (FX_DECREF(left_root->rc) == 1) fx_rrb_freenode_(info, left_root);
        return true;
    }
}

#if 0
typedef float T;

int main(int, char**)
{
    int i, k = 0, n = 10000003;
    fx_rrbvec_t vec;
    fx_rrbinfo_t info;
    fx_rrb_init_info(&info, sizeof(T), 0, 0);
    fx_rrb_empty_vec(&info, &vec);
    fx_rrbiter_t iter;
    std::vector<T> src(n);
    char* ptr;

    for(i = 0; i < n; i++) {
        src[i] = (float)i;//((rand()%10000)/10000.);
    }

    /*ptr = fx_rrb_start_write(&vec, &iter);
    i = 0;
    for(; i < n; ) {
        int delta0 = n - i; if (delta0 > 50) delta0 = 50;
        int delta = rand() % (delta0 + 1);
        ptr = fx_rrbiter_push_back(&iter, ptr, (char*)&src[i], delta);
        i += delta;
    }
    fx_rrb_end_write(&iter, ptr);*/

    i = 0;
    for(; i < n; k++) {
        int delta0 = n - i; if (delta0 > 10000) delta0 = 10000;
        int delta = rand() % (delta0 + 1);
        fx_rrbvec_t temp, temp1, temp2;
        fx_rrb_empty_vec(&info, &temp);
        fx_rrb_append(&temp, (char*)&src[i], delta, &temp1);
        fx_rrb_free(&temp);
        //fx_rrb_check_tree(temp1.root, 0, true);
        fx_rrb_concat(&vec, &temp1, &temp2);
        fx_rrb_free(&temp1);
        fx_rrb_free(&vec);
        vec = temp2;
        fx_rrb_check_tree(vec.root, 0, true);
        i += delta;
        int_ ntail = vec.tail ? vec.tail->nelems : 0;
        int_ ntree = vec.root ? vec.root->total : 0;
        assert(vec.size == i && ntree + ntail == i);
    }
    assert(vec.size == n);
    memset(&iter, 0, sizeof(iter));
    float* fptr = (float*)fx_rrb_start_read(&vec, &iter, 0);
    for(i = 0; i < n; i++) {
        float f = *fptr;
        assert(f == src[i]);
        FX_RRB_NEXT(float, iter, fptr);
    }

    for(i = 0; i < 100000; i++) {
        int k = rand() % n;
        ptr = fx_rrb_find(&vec, k);
        float f = *(float*)ptr;
        float f0 = src[k];
        if (f != f0) {
            ptr = fx_rrb_find(&vec, k);
            assert(*(float*)ptr == src[k]);
        }
    }
    fx_rrb_free(&vec);
    printf("ok\n");
    return 0;
}
#endif
