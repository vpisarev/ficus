/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// This regexp engine is the modified version of re1 by Russ Cox:
// https://code.google.com/archive/p/re1/
//
// Copyright 2007-2009 Russ Cox. All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the regex_LICENSE.txt file.

#ifndef __FICUS_REGEX_IMPL_H__
#define __FICUS_REGEX_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

typedef struct fx_re_astnode_t
{
	int type;
	int n;
	int c;
	int left;
	int right;
} fx_re_astnode_t;

typedef struct fx_re_ast_t
{
    fx_re_astnode_t* buf;
    int root;
    int size, capacity;
    int grp;
    bool allocated;
} fx_re_ast_t;

enum	/* fx_re_ast_t.type */
{
	FX_RE_ALT = 1,
	FX_RE_CAT,
	FX_RE_LIT,
	FX_RE_DOT,
	FX_RE_PAREN,
	FX_RE_QUEST,
	FX_RE_STAR,
	FX_RE_PLUS,
    FX_RE_RANGE,
    FX_RE_SUBRANGE,
    FX_RE_CLASS,
    FX_RE_ANCHOR_START,
    FX_RE_ANCHOR_END,
    FX_RE_ANCHOR_WBOUND,
};

enum
{
    FX_RE_CLASS_LIT = 0,
    FX_RE_CLASS_SPACE = 1,
    FX_RE_CLASS_NON_SPACE,
    FX_RE_CLASS_ALPHA,
    FX_RE_CLASS_IDENT,
    FX_RE_CLASS_DIGIT,
    FX_RE_ERROR = -100
};

static fx_re_ast_t *fx_re_parse(fx_str_t*);
static void fx_re_printre_(fx_re_ast_t*);
static void fx_re_fatal_(const char* msg)
{
    printf("error: [regexp] %s\n", msg);
    exit(1);
}

static void fx_re_free_regexp_(fx_re_ast_t* regexp)
{
    if(regexp == 0)
        return;
    fx_free(regexp);
}

typedef struct fx_re_inst_t
{
	int opcode;
	int c;
	int n;
    int x;
    int y;
	int gen;	// global state, oooh!
} fx_re_inst_t;

typedef struct fx_regex_t
{
    fx_re_inst_t* prog;
    int len, nsub;
} fx_regex_t;

enum	/* fx_re_inst_t.opcode */
{
	FX_RE_OP_CHAR = 1,
    FX_RE_OP_RANGE,
    FX_RE_OP_CLASS_SPACE,
    FX_RE_OP_CLASS_NON_SPACE,
    FX_RE_OP_CLASS_ALPHA,
    FX_RE_OP_CLASS_IDENT,
    FX_RE_OP_CLASS_DIGIT,
    FX_RE_OP_WBOUND,
	FX_RE_OP_MATCH,
	FX_RE_OP_JMP,
	FX_RE_OP_SPLIT,
	FX_RE_OP_ANY,
	FX_RE_OP_SAVE,
};

enum {
    // Subgroup # can be 0 (the whole match), 1 (1st submatch), 2, ... 9,
    // and for each subgroup we need 2 positions within a string: [start, end)
    // So, in total we may need to store up to 20 numbers for each regexp match.
	FX_RE_MAXSUB = 20,
    FX_RE_AST_BUFSIZE = 256,
    FX_RE_SUB_BUFSIZE = 256,
    FX_RE_NS_BUFSIZE = 256
};

typedef struct fx_re_sub_t
{
    union {
        struct fx_re_sub_t* next;
        int_ ref;
    } u;
	int_ sub[FX_RE_MAXSUB];
} fx_re_sub_t;

typedef struct fx_re_thread_t
{
    int pc;
    fx_re_sub_t *sub;
} fx_re_thread_t;

typedef struct fx_re_threads_t
{
    int n;
    fx_re_thread_t* t;
} fx_re_threads_t;

typedef struct fx_re_matcher_t
{
    const fx_str_t* input;
    fx_re_inst_t* prog;
    int proglen, nsub, subsize;
    int* pcgen;
    fx_re_sub_t* freesub;
    char* subbuf;
    int subbuf_size, subbuf_capacity;
    fx_re_thread_t* pool;
    char* globalbuf;
    int gen;
} fx_re_matcher_t;

static fx_re_sub_t *fx_re_newsub_(fx_re_matcher_t* matcher);
static fx_re_sub_t *fx_re_incref_(fx_re_sub_t*);
static fx_re_sub_t *fx_re_copysub_(fx_re_sub_t*);
static fx_re_sub_t *fx_re_update_(fx_re_matcher_t* matcher, fx_re_sub_t*, int, int_);
static void fx_re_decref_(fx_re_matcher_t* matcher, fx_re_sub_t*);

static fx_re_sub_t*
fx_re_newsub_(fx_re_matcher_t* matcher)
{
	fx_re_sub_t *s = matcher->freesub;
	if(s != 0)
        matcher->freesub = s->u.next;
	else
    {
        int subbuf_size = matcher->subbuf_size, subsize = matcher->subsize;
        if (subbuf_size >= matcher->subbuf_capacity)
            return 0;
        s = (fx_re_sub_t*)(matcher->subbuf + subbuf_size);
        memset(s, 0, subsize);
        matcher->subbuf_size = subbuf_size + subsize;
    }
    s->u.ref = 1;
	return s;
}

static fx_re_sub_t*
fx_re_incref_(fx_re_sub_t *s)
{
	s->u.ref++;
	return s;
}

static fx_re_sub_t*
fx_re_update_(fx_re_matcher_t* matcher, fx_re_sub_t *s, int i, int_ p)
{
	fx_re_sub_t *s1 = s;

	if(s->u.ref > 1) {
        int nsub = matcher->nsub;
		s1 = fx_re_newsub_(matcher);
		for(int j = 0; j < nsub; j++)
			s1->sub[j] = s->sub[j];
		s->u.ref--;
	};
	s1->sub[i] = p;
	return s1;
}

static void
fx_re_decref_(fx_re_matcher_t* matcher, fx_re_sub_t *s)
{
	if(--s->u.ref == 0) {
		s->u.next = matcher->freesub;
        matcher->freesub = s;
	}
}

static int fx_re_count_(fx_re_ast_t*, int);
static int fx_re_emit_(fx_re_inst_t* re, int, int*, fx_re_ast_t*, int);

////////////////////// compile & interpret regexps /////////////////////////////

static fx_regex_t* fx_re_compile_(fx_re_ast_t *ast)
{
    int nsub = ast->grp*2;
	int len = fx_re_count_(ast, ast->root) + 1, pc = 0;
    size_t progsize = len*sizeof(fx_re_inst_t);
    if(nsub > FX_RE_MAXSUB)
        return 0;
    {
    fx_regex_t *re = (fx_regex_t*)fx_malloc(sizeof(*re) + progsize);
    if(!re) return 0;
    re->prog = (fx_re_inst_t*)(re + 1);
    re->len = len;
    re->nsub = nsub;
    memset(re->prog, 0, progsize);
	int status = fx_re_emit_(re->prog, len, &pc, ast, ast->root);
    if (status >= 0) {
        if (++pc != len) {
            fx_free(re);
            return 0;
        }
        re->prog[pc-1].opcode = FX_RE_OP_MATCH;
    }
    if (status < 0 && re) {
        fx_free(re);
        re = 0;
    }
	return re;
    }
}

// Inside findall or global replace functions,
// when we found one match and want to move to the next one,
// we need to reset the state.
// While this is probably not the fastest way to
// "reset" it, it should be quite robust.
static void fx_re_reset_matcher(fx_re_matcher_t* matcher)
{
    size_t pcgen_size = matcher->proglen*sizeof(matcher->pcgen[0]);
    matcher->freesub = 0;
    matcher->subbuf_size = 0;
    memset(matcher->pcgen, 0, pcgen_size);
    matcher->gen = 0;
}

static int fx_re_init_matcher(fx_re_matcher_t* matcher, const fx_str_t* input, const fx_cptr_t regex_)
{
    memset(matcher, 0, sizeof(*matcher));

    size_t total, pcgen_size, subsize, subbuf_capacity, threadbuf_size;
    char* bufptr;
    fx_regex_t* regex;
    if (!regex_ || !regex_->ptr)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    regex = (fx_regex_t*)(regex_->ptr);
    matcher->input = input;
    matcher->prog = regex->prog;
    matcher->proglen = regex->len;
    matcher->nsub = regex->nsub;
    pcgen_size = regex->len*sizeof(matcher->pcgen[0]);
    threadbuf_size = matcher->proglen*2*sizeof(matcher->pool[0]);
    assert(sizeof(int_) == sizeof(void*));
    subsize = (matcher->nsub+1)*sizeof(int_);
    subbuf_capacity = (matcher->proglen*2 + 2)*subsize;
    total = pcgen_size + subbuf_capacity + threadbuf_size;
    //printf("total regexp matcher buf size=%d\n", (int)total);
    matcher->globalbuf = bufptr = (char*)fx_malloc(total);
    if(!matcher->globalbuf)
        return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
    matcher->subbuf = bufptr;
    bufptr += subbuf_capacity;
    matcher->subsize = subsize;
    matcher->subbuf_capacity = subbuf_capacity;
    matcher->pool = (fx_re_thread_t*)bufptr;
    bufptr += threadbuf_size;
    matcher->pcgen = (int*)bufptr;
    fx_re_reset_matcher(matcher);
    return FX_OK;
}

static void fx_re_free_matcher(fx_re_matcher_t* matcher)
{
    fx_free(matcher->globalbuf);
    memset(matcher, 0, sizeof(*matcher));
}

// how many instructions does r need?
static int fx_re_count_(fx_re_ast_t *ast, int ofs)
{
    fx_re_astnode_t* r = ast->buf + ofs;
	switch(r->type) {
	case FX_RE_ALT:
		return 2 + fx_re_count_(ast, r->left) + fx_re_count_(ast, r->right);
	case FX_RE_CAT:
		return fx_re_count_(ast, r->left) + fx_re_count_(ast, r->right);
	case FX_RE_LIT:
	case FX_RE_DOT:
    case FX_RE_CLASS:
		return 1;
    case FX_RE_RANGE:
        return r->n + 1;
    case FX_RE_SUBRANGE:
        return 1;
	case FX_RE_PAREN:
		return 2 + fx_re_count_(ast, r->left);
    case FX_RE_ANCHOR_WBOUND:
        return 1;
	case FX_RE_QUEST:
		return 1 + fx_re_count_(ast, r->left);
	case FX_RE_STAR:
		return 2 + fx_re_count_(ast, r->left);
	case FX_RE_PLUS:
		return 1 + fx_re_count_(ast, r->left);
    default:
        fx_re_fatal_("invalid opcode");
        return 0;
	}
}

static int fx_re_emit_(fx_re_inst_t* prog, int len, int* pc, fx_re_ast_t *ast, int ofs)
{
	fx_re_inst_t *p1, *p2;
    fx_re_astnode_t* r = ast->buf + ofs;
    int status = 0;
    if (*pc >= len)
        return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);

	switch(r->type) {
	case FX_RE_ALT:
		p1 = prog + (*pc)++;
        p1->opcode = FX_RE_OP_SPLIT;
		p1->x = *pc;
		status = fx_re_emit_(prog, len, pc, ast, r->left);
        if(status < 0) return status;
        p2 = prog + (*pc)++;
        p2->opcode = FX_RE_OP_JMP;
		p1->y = *pc;
		status = fx_re_emit_(prog, len, pc, ast, r->right);
        if(status < 0) return status;
		p2->x = *pc;
		break;

	case FX_RE_CAT:
		status = fx_re_emit_(prog, len, pc, ast, r->left);
        return status < 0 ? status :
            fx_re_emit_(prog, len, pc, ast, r->right);

	case FX_RE_LIT:
        p1 = prog + (*pc)++;
		p1->opcode = FX_RE_OP_CHAR;
		p1->c = r->c;
        p1->x = fx_tolower(r->c);
        p1->y = fx_toupper(r->c);
		break;

	case FX_RE_DOT:
        p1 = prog + (*pc)++;
        p1->opcode = FX_RE_OP_ANY;
		break;

    case FX_RE_CLASS:
        {
        p1 = prog + (*pc)++;
        int c = r->c;
        p1->opcode =
            c == FX_RE_CLASS_ALPHA ? FX_RE_OP_CLASS_ALPHA :
            c == FX_RE_CLASS_IDENT ? FX_RE_OP_CLASS_IDENT :
            c == FX_RE_CLASS_DIGIT ? FX_RE_OP_CLASS_DIGIT :
            c == FX_RE_CLASS_SPACE ? FX_RE_OP_CLASS_SPACE :
            c == FX_RE_CLASS_NON_SPACE ? FX_RE_OP_CLASS_NON_SPACE : -1;
        if (p1->opcode < 0)
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        }
        break;

    case FX_RE_RANGE:
        {
        int i, nranges = r->n;
        for(i = 0; i <= nranges; i++) {
            fx_re_inst_t* prog_i = prog + (*pc + i);
            int type = r[i].type;
            int c = r[i].c;
            prog_i->opcode = type == FX_RE_RANGE || type == FX_RE_SUBRANGE ? FX_RE_OP_RANGE :
                             type == FX_RE_LIT ? FX_RE_OP_CHAR :
                             type == FX_RE_CLASS ?
                                (c == FX_RE_CLASS_ALPHA ? FX_RE_OP_CLASS_ALPHA :
                                 c == FX_RE_CLASS_IDENT ? FX_RE_OP_CLASS_IDENT :
                                 c == FX_RE_CLASS_DIGIT ? FX_RE_OP_CLASS_DIGIT :
                                 c == FX_RE_CLASS_SPACE ? FX_RE_OP_CLASS_SPACE :
                                 c == FX_RE_CLASS_NON_SPACE ? FX_RE_OP_CLASS_NON_SPACE : -1) :
                             -1;
            if (prog_i->opcode < 0)
                return FX_SET_EXN_FAST(FX_EXN_BadArgError);
            prog_i->n = r[i].n;
            prog_i->c = r[i].c;
            prog_i->x = r[i].left;
            prog_i->y = r[i].right;
        }
        *pc += nranges + 1;
        }
        break;

	case FX_RE_PAREN:
        p1 = prog + (*pc)++;
		p1->opcode = FX_RE_OP_SAVE;
		p1->n = 2*r->n;
		status = fx_re_emit_(prog, len, pc, ast, r->left);
        if(status < 0) return status;
        p2 = prog + (*pc)++;
        p2->opcode = FX_RE_OP_SAVE;
		p2->n = 2*r->n + 1;
		break;

	case FX_RE_QUEST:
        p1 = prog + (*pc)++;
		p1->opcode = FX_RE_OP_SPLIT;
		p1->x = *pc;
		status = fx_re_emit_(prog, len, pc, ast, r->left);
        if(status < 0) return status;
		p1->y = *pc;
		if(r->n) {	// non-greedy
			int t = p1->x;
			p1->x = p1->y;
			p1->y = t;
		}
		break;

	case FX_RE_STAR:
        p1 = prog + (*pc)++;
		p1->opcode = FX_RE_OP_SPLIT;
		p1->x = *pc;
		status = fx_re_emit_(prog, len, pc, ast, r->left);
        if(status < 0) return status;
        p2 = prog + (*pc)++;
		p2->opcode = FX_RE_OP_JMP;
		p2->x = (int)(p1 - prog);
		p1->y = *pc;
		if(r->n) {	// non-greedy
			int t = p1->x;
			p1->x = p1->y;
			p1->y = t;
		}
		break;

	case FX_RE_PLUS:
        p1 = prog + (*pc);
		status = fx_re_emit_(prog, len, pc, ast, r->left);
        if(status < 0) return status;
        p2 = prog + (*pc)++;
		p2->opcode = FX_RE_OP_SPLIT;
		p2->x = (int)(p1 - prog);
		p2->y = *pc;
		if(r->n) {	// non-greedy
			int t = p2->x;
			p2->x = p2->y;
			p2->y = t;
		}
		break;

    case FX_RE_ANCHOR_WBOUND:
        p1 = prog + (*pc)++;
        p1->opcode = FX_RE_OP_WBOUND;
        p1->n = p1->c = p1->x = p1->y = 0;
        break;

    default:
        fx_re_fatal_("bad fx_re_emit_");
	}
    return status;
}

static fx_re_thread_t fx_re_thread_(int pc, fx_re_sub_t *sub)
{
	fx_re_thread_t t = {pc, sub};
	return t;
}

static void
fx_re_addthread_(fx_re_matcher_t* matcher, fx_re_threads_t *tl, fx_re_thread_t t, int_ charpos)
{
    int gen = matcher->gen;
	if(matcher->pcgen[t.pc] == gen) {
		fx_re_decref_(matcher, t.sub);
    } else {
        fx_re_inst_t* r = matcher->prog + t.pc;
        matcher->pcgen[t.pc] = gen;
        switch(r->opcode) {
        case FX_RE_OP_JMP:
            fx_re_addthread_(matcher, tl, fx_re_thread_(r->x, t.sub), charpos);
            break;
        case FX_RE_OP_SPLIT:
            fx_re_addthread_(matcher, tl, fx_re_thread_(r->x, fx_re_incref_(t.sub)), charpos);
            fx_re_addthread_(matcher, tl, fx_re_thread_(r->y, t.sub), charpos);
            break;
        case FX_RE_OP_SAVE:
            //printf("save at charpos=%d, r->n=%d\n", (int)charpos, r->n);
            fx_re_addthread_(matcher, tl, fx_re_thread_(t.pc+1, fx_re_update_(matcher, t.sub, r->n, charpos)), charpos);
            break;
        case FX_RE_OP_WBOUND:
            {
            const fx_str_t* input = matcher->input;
            char_ c0 = charpos > 0 ? input->data[charpos-1] : (char_)0;
            char_ c1 = charpos < input->length ? input->data[charpos] : (char_)0;
            bool i0 = fx_isalnum(c0) || c0 == '_';
            bool i1 = fx_isalnum(c1) || c1 == '_';
            if (i0 ^ i1)
                fx_re_addthread_(matcher, tl, fx_re_thread_(t.pc+1, t.sub), charpos);
            }
            break;
        default:
            assert(tl->n < matcher->proglen);
            tl->t[tl->n] = t;
            tl->n++;

            break;
        }
    }
}

static const char* fx_re_op2str(int opcode)
{
    return
    opcode == FX_RE_OP_ANY ? "any" :
    opcode == FX_RE_OP_CHAR ? "char" :
    opcode == FX_RE_OP_RANGE ? "range" :
    opcode == FX_RE_OP_CLASS_SPACE ? "isspace" :
    opcode == FX_RE_OP_CLASS_NON_SPACE ? "isnonspace" :
    opcode == FX_RE_OP_CLASS_ALPHA ? "isalpha" :
    opcode == FX_RE_OP_CLASS_IDENT ? "isident" :
    opcode == FX_RE_OP_CLASS_DIGIT ? "isdigit" :
    opcode == FX_RE_OP_WBOUND ? "word_boundary" :
    opcode == FX_RE_OP_MATCH ? "match!" :
    opcode == FX_RE_OP_JMP ? "jmp" :
    opcode == FX_RE_OP_SPLIT ? "split" :
    opcode == FX_RE_OP_SAVE ? "save" : "???";
}

static void
fx_re_printprog(const fx_cptr_t regex_)
{
    fx_regex_t* re = (fx_regex_t*)(regex_->ptr);
    if(re) {
        int i, len = re->len;
        for(i = 0; i < len; i++) {
            fx_re_inst_t* r = &re->prog[i];
            const char* opstr = fx_re_op2str(r->opcode);
            printf("%2d. %s n=%d, c=%d, x=%d, y=%d\n", i, opstr, r->n, r->c, r->x, r->y);
        }
    }
}

static int
fx_re_pikevm_(const fx_cptr_t regex_, const fx_str_t* input, int flags,
              int_** outsub_, int outsub_capacity0, int* outnsub)
{
    fx_re_matcher_t matcher;
    int i, len, nsub;
    fx_re_threads_t clist, nlist;
    char_* str = input->data;
    int_ j = 0, strlen = input->length;
    fx_re_sub_t *sub, *matched = 0;
    fx_re_inst_t* prog;
    bool ignore_case = (flags & FX_RE_IGNORECASE) != 0;
    bool multiline_mode = (flags & FX_RE_MULTILINE) != 0;
    int_* outsub_buf0 = *outsub_, *outsub_buf = outsub_buf0;
    int_* outsub = 0;
    int outsub_capacity = outsub_capacity0;
    int nmatches = 0, pc0 = 0;
    int mode = flags & FX_RE_MODE_MASK_;

    int status = fx_re_init_matcher(&matcher, input, regex_);
    if(status < 0) return status;

    nsub = matcher.nsub;
    len = matcher.proglen;
    prog = matcher.prog;
    // if we are in the "match mode", not "find mode", then
    // we need to skip the '.*?' part of the regexp, which
    // corresponds to the following bytecode:
    // ---
    //    SPLIT x, y
    // y: ANY
    //    JMP x
    // x: ...
    // ---
    // (i.e. need to skip 3 instructions and jump right to "x")
    pc0 = mode == FX_RE_MATCH_MODE_ ? 3 : 0;
    *outnsub = nsub;

    while(j < strlen) {
        //printf("start/resume from j=%d\n", (int)j);
        if(j > 0) {
            fx_re_reset_matcher(&matcher);
            matched = 0;
        }
        if ((nmatches+1)*nsub > outsub_capacity) {
            int new_capacity = outsub_capacity*2;
            int_* new_outsub = (int_*)fx_malloc(new_capacity*sizeof(outsub[0]));
            memcpy(new_outsub, outsub_buf, outsub_capacity*sizeof(outsub[0]));
            if(outsub_buf != outsub_buf0)
                fx_free(outsub_buf);
            *outsub_ = outsub_buf = new_outsub;
            outsub_capacity = new_capacity;
        }
        outsub = outsub_buf + nmatches*nsub;
        memset(outsub, 0, nsub*sizeof(outsub[0]));
        sub = fx_re_newsub_(&matcher);

        clist.t = matcher.pool;
        nlist.t = clist.t + len;
        clist.n = nlist.n = 0;

        matcher.gen++;
        fx_re_addthread_(&matcher, &clist, fx_re_thread_(pc0, sub), j);
        for(; j <= strlen; j++)
        {
            char_ c = j < strlen ? str[j] : 0;
            if(clist.n == 0)
                break;
            //printf("j=%d. char=%c\n", (int)j, c);
            // printf("%d(%02x).", (int)(sp - input), *sp & 0xFF);
            matcher.gen++;
            for(i = 0; i < clist.n; i++) {
                int pc = clist.t[i].pc;
                sub = clist.t[i].sub;
                fx_re_inst_t* r = prog + pc;
                //printf("thread %d. op #%d (%s)\n", i, pc, fx_re_op2str(r->opcode));
                switch(r->opcode) {
                case FX_RE_OP_ANY:
                    if (!multiline_mode || (c != '\n' && c != '\r'))
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(pc+1, sub), j+1);
                    else
                        fx_re_decref_(&matcher, sub);
                    break;
                case FX_RE_OP_CHAR:
                    {
                    bool ok = !ignore_case ? c == r->c : (c == r->x || c == r->y);
                    if(ok)
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(pc+1, sub), j+1);
                    else
                        fx_re_decref_(&matcher, sub);
                    }
                    break;
                case FX_RE_OP_RANGE:
                    {
                    char_ c0 = c, c1 = c;
                    bool inv = r->c != 0;
                    int k, n = r->n;
                    int next_pc = pc + 1 + n;
                    if(ignore_case) {
                        c0 = fx_tolower(c);
                        c1 = fx_toupper(c);
                    }
                    for(k = 0; k < n; k++) {
                        fx_re_inst_t* rk = prog + pc + 1 + k;
                        if(rk->opcode == FX_RE_OP_CHAR) {
                            if(c0 == rk->c || c1 == rk->c) break;
                        } else if(rk->opcode == FX_RE_OP_RANGE) {
                            if((rk->x <= c0 && c0 <= rk->y) ||
                               (rk->x <= c1 && c1 <= rk->y)) break;
                        } else if(rk->opcode == FX_RE_OP_CLASS_ALPHA) {
                            if(fx_isalpha(c)) break;
                        } else if(rk->opcode == FX_RE_OP_CLASS_IDENT) {
                            if(fx_isalnum(c) || c == '_') break;
                        } else if(rk->opcode == FX_RE_OP_CLASS_DIGIT) {
                            if(fx_isdigit(c)) break;
                        } else if(rk->opcode == FX_RE_OP_CLASS_SPACE) {
                            if(fx_isspace(c)) break;
                        } else if(rk->opcode == FX_RE_OP_CLASS_NON_SPACE) {
                            if(!fx_isspace(c)) break;
                        } else {
                            //printf("error: unexpected opcode '%d' inside the regexp range processing\n", rk->opcode);
                            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                        }
                    }
                    {
                    bool ok = inv ^ (k < n);
                    if(!ok)
                        fx_re_decref_(&matcher, sub);
                    else
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(next_pc, sub), j+1);
                    }
                    }
                    break;
                case FX_RE_OP_CLASS_ALPHA:
                    if(fx_isalpha(c))
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(pc+1, sub), j+1);
                    else
                        fx_re_decref_(&matcher, sub);
                    break;
                case FX_RE_OP_CLASS_IDENT:
                    if(fx_isalnum(c) || c == '_')
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(pc+1, sub), j+1);
                    else
                        fx_re_decref_(&matcher, sub);
                    break;
                case FX_RE_OP_CLASS_DIGIT:
                    if(fx_isdigit(c))
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(pc+1, sub), j+1);
                    else
                        fx_re_decref_(&matcher, sub);
                    break;
                case FX_RE_OP_CLASS_SPACE:
                    if(fx_isspace(c) && (!multiline_mode || (c != '\n' && c != '\r')))
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(pc+1, sub), j+1);
                    else
                        fx_re_decref_(&matcher, sub);
                    break;
                case FX_RE_OP_CLASS_NON_SPACE:
                    if(fx_isspace(c) && (!multiline_mode || (c != '\n' && c != '\r')))
                        fx_re_decref_(&matcher, sub);
                    else
                        fx_re_addthread_(&matcher, &nlist, fx_re_thread_(pc+1, sub), j+1);
                    break;
                case FX_RE_OP_MATCH:
                    if(matched)
                        fx_re_decref_(&matcher, matched);
                    matched = sub;
                    //printf("updated matched: (%d, %d)\n", (int)sub->sub[0], (int)sub->sub[1]);
                    for(++i; i < clist.n; i++)
                        fx_re_decref_(&matcher, clist.t[i].sub);
                    break;
                default:
                    //printf("unexpected instruction %d at pc=%d, j=%d\n", r->opcode, pc, (int)j);
                    return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                // FX_RE_OP_JMP, FX_RE_OP_SPLIT, FX_RE_OP_SAVE handled in fx_re_addthread_, so that
                // machine execution matches what a backtracker would do.
                // This is discussed (but not shown as code) in
                // Regular Expression Matching: the Virtual Machine Approach.
                }
            }
            // printf("\n");
            {
            fx_re_threads_t tmp = clist;
            clist = nlist;
            nlist = tmp;
            }
            nlist.n = 0;
            if(j >= strlen)
                break;
        }

        if(matched) {
            //printf("found (%d, %d)\n", (int)matched->sub[0], (int)matched->sub[1]);
            for(i = 0; i < nsub; i++)
                outsub[i] = matched->sub[i];
            fx_re_decref_(&matcher, matched);
            nmatches++;
        }
        if(mode != FX_RE_FINDALL_MODE_)
            break;
    }

    fx_re_free_matcher(&matcher);
	return nmatches;
}

// alt -> seq -> repeat -> single.
static int fx_re_astnode(fx_re_ast_t* ast, int type, int n, int c, int left, int right)
{
    if (ast->size >= ast->capacity)
    {
        int new_capacity = ast->capacity*2;
        fx_re_astnode_t* new_buf = (fx_re_astnode_t*)fx_malloc(new_capacity*sizeof(new_buf[0]));
        if(!new_buf) return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
        memcpy(new_buf, ast->buf, ast->capacity*sizeof(new_buf[0]));
        if(ast->allocated) fx_free(ast->buf);
        ast->buf = new_buf;
        ast->capacity = new_capacity;
        ast->allocated = true;
    }
    {
    int idx = ast->size++;
    fx_re_astnode_t* node = ast->buf + idx;
    node->type = type;
    node->n = n;
    node->c = c;
    node->left = left;
    node->right = right;
    return idx;
    }
}

static int fx_re_parse_special_(const fx_str_t* str, int_* pos, fx_re_ast_t* ast, int* c_special_)
{
    char_ c;
    int c_special;
    if (*pos >= str->length)
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
    c = str->data[*pos];
    c_special = c == 'n' ? '\n' : c == 'r' ? '\r' : c == 't' ? '\t' :
                c == '(' ? '(' : c == ')' ? ')' : c == '[' ? '[' : c == ']' ? ']' :
                c == '{' ? '{' : c == '}' ? '}' :
                c == '|' ? '|' : c == '.' ? '.' : c == '\\' ? '\\' :
                c == '*' ? '*' : c == '+' ? '+' : c == '-' ? '-' :
                c == '^' ? '^' : c == '$' ? '$' :
                c == 'a' ? -FX_RE_CLASS_ALPHA :
                c == 'w' ? -FX_RE_CLASS_IDENT :
                c == 'd' ? -FX_RE_CLASS_DIGIT :
                c == 's' ? -FX_RE_CLASS_SPACE :
                c == 'S' ? -FX_RE_CLASS_NON_SPACE :
                c == 'b' ? -FX_RE_ANCHOR_WBOUND :
                c == 'U' || c == 'u' || c == 'x' || c == '0' ? FX_RE_CLASS_LIT : FX_RE_ERROR;
    if (c_special == FX_RE_ERROR)
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
    ++*pos;
    if(c_special == FX_RE_CLASS_LIT) {
        int code = 0;
        int i, ndigits = str->length - *pos;
        if(c == '0') {
            ndigits = ndigits < 3 ? ndigits : 3;
            for(i = 0; i < ndigits; i++) {
                int d = str->data[*pos + i] - '0';
                if (d < 0 || d >= 8) break;
                code = code*8 + d;
            }
            *pos += i;
        } else {
            int max_ndigits = c == 'x' ? 2 : c == 'u' ? 4 : 8;
            ndigits = ndigits < max_ndigits ? ndigits : max_ndigits;
            for(i = 0; i < ndigits; i++) {
                int d = str->data[*pos + i];
                d = d > 'a' ? d - 'a' + 10 : d > 'A' ? d - 'A' + 10 : d <= '9' ? d - '0' : -1;
                if(d < 0 || d >= 16) break;
                code = code*16 + d;
            }
            *pos += i;
            if(i == 0)
                return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        }
        c_special = code;
    };

    {
    int type = c_special >= 0 ? FX_RE_LIT : c_special == -FX_RE_ANCHOR_WBOUND ? FX_RE_ANCHOR_WBOUND : FX_RE_CLASS;
    *c_special_ = c_special >= 0 ? c_special : -c_special;
    return type;
    }
}

static int fx_re_parse_alt_(const fx_str_t* str, int_* pos, fx_re_ast_t* ast);

static int fx_re_parse_range_(const fx_str_t* str, int_* pos, fx_re_ast_t* ast)
{
    bool inv = false;
    char_ c;
    int range_idx = fx_re_astnode(ast, FX_RE_RANGE, 0, 0, 0, 0);
    int type, c_special = 0, nranges = 0;
    if (*pos < str->length && str->data[*pos] == '^') {
        inv = true;
        ++*pos;
    }
    for(;; nranges++) {
        int left = 0;
        if (*pos >= str->length)
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        c = str->data[(*pos)++];
        if(c == ']')
            break;
        if(c == '\\') {
            type = fx_re_parse_special_(str, pos, ast, &c_special);
            if(type < 0)
                return type;
            if(type != FX_RE_LIT) {
                if(type != FX_RE_CLASS)
                    return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                fx_re_astnode(ast, type, 1, c_special, 0, 0);
                continue;
            }
            left = c_special;
        } else {
            left = c;
        }
        if (*pos >= str->length)
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        c = str->data[*pos];
        if(c != '-') {
            fx_re_astnode(ast, FX_RE_LIT, 1, left, 0, 0);
        } else {
            int right;
            if (++*pos >= str->length)
                return FX_SET_EXN_FAST(FX_EXN_BadArgError);
            c = str->data[(*pos)++];
            if(c == '\\') {
                type = fx_re_parse_special_(str, pos, ast, &c_special);
                if(type < 0)
                    return type;
                if(type != FX_RE_LIT)
                    // a character class cannot be used as a limiter of range
                    return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                right = c_special;
            } else {
                right = c;
            }
            fx_re_astnode(ast, FX_RE_SUBRANGE, 0, 0, left, right);
        }
    }

    if (nranges == 0)
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
    {
    fx_re_astnode_t* r = ast->buf + range_idx;
    r->c = (int)inv;
    r->n = nranges;
    }
    return range_idx;
}

static int fx_re_parse_single_(const fx_str_t* str, int_* pos, fx_re_ast_t* ast)
{
    char_ c;
    if(*pos >= str->length)
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
    c = str->data[*pos];
    if (c == '.') {
        ++*pos;
        return fx_re_astnode(ast, FX_RE_DOT, 0, 0, 0, 0);
    }
    if (c == '\\') {
        ++*pos;
        int c_special = 0;
        int type = fx_re_parse_special_(str, pos, ast, &c_special);
        return type < 0 ? type : fx_re_astnode(ast, type, 1, c_special, 0, 0);
    }
    if (c == '[') {
        ++*pos;
        return fx_re_parse_range_(str, pos, ast);
    }
    if (c == '(') {
        ++*pos;
        bool unnamed_group = *pos+2 < str->length && str->data[*pos] == '?' && str->data[*pos+1] == ':';
        if(unnamed_group) {
            *pos += 2;
        }
        int grp = fx_re_parse_alt_(str, pos, ast);
        if(!unnamed_group)
            grp = fx_re_astnode(ast, FX_RE_PAREN, ast->grp++, 0, grp, 0);
        if(*pos >= str->length || str->data[*pos] != ')')
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        ++*pos;
        return grp;
    }
    ++*pos;
    return fx_re_astnode(ast, FX_RE_LIT, 1, c, 0, 0);
}

static int fx_re_parse_repeat_(const fx_str_t* str, int_* pos, fx_re_ast_t* ast)
{
    int fst = fx_re_parse_single_(str, pos, ast);
    char_ c;
    if(fst < 0 || *pos >= str->length) return fst;
    c = str->data[*pos];
    if(c == '*' || c == '+' || c == '?') {
        int n = 0;
        if (++*pos < str->length && str->data[*pos] == '?') {
            n = 1;
            ++*pos;
        }
        fst = fx_re_astnode(ast, (c == '*' ? FX_RE_STAR : c == '+' ? FX_RE_PLUS : FX_RE_QUEST), n, 0, fst, 0);
    }
    return fst;
}

static int fx_re_parse_concat_(const fx_str_t* str, int_* pos, fx_re_ast_t* ast)
{
    int fst = -1;
    for(;;) {
        int next = fx_re_parse_repeat_(str, pos, ast);
        if(next < 0) return next;
        if (fst < 0) fst = next;
        else {
            fst = fx_re_astnode(ast, FX_RE_CAT, 0, 0, fst, next);
            if(fst < 0)
                return fst;
        }
        if (*pos >= str->length || str->data[*pos] == ')' || str->data[*pos] == '|') break;
    }
    return fst;
}

static int fx_re_parse_alt_(const fx_str_t* str, int_* pos, fx_re_ast_t* ast)
{
    int fst = -1;
    for(;;) {
        int next = fx_re_parse_concat_(str, pos, ast);
        if(next < 0) return next;
        if (fst < 0) fst = next;
        else {
            fst = fx_re_astnode(ast, FX_RE_ALT, 0, 0, fst, next);
            if(fst < 0)
                return fst;
        }
        if (*pos >= str->length || str->data[*pos] != '|') break;
        *pos += 1;
    }
    return fst;
}

static int fx_re_parse_(const fx_str_t* str, fx_re_ast_t* ast)
{
    int root_idx = fx_re_astnode(ast, FX_RE_CAT, 0, 0, 0, 0);
    int parsed_root, prefix;
    assert(root_idx == 0);
    int_ pos = 0;
    if (str->length == 0)
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
    parsed_root = fx_re_parse_alt_(str, &pos, ast);
    if (parsed_root < 0) return parsed_root;
    if (pos != str->length) FX_SET_EXN_FAST(FX_EXN_BadArgError);
    parsed_root = fx_re_astnode(ast, FX_RE_PAREN, 0, 0, parsed_root, 0);
    prefix = fx_re_astnode(ast, FX_RE_DOT, 0, 0, 0, 0);
    prefix = fx_re_astnode(ast, FX_RE_STAR, 1, 0, prefix, 0);
    {
    fx_re_astnode_t* root = ast->buf + root_idx;
    root->left = prefix;
    root->right = parsed_root;
    }
    ast->root = root_idx;
    return FX_OK;
}

int fx_re_compile(const fx_str_t* str, fx_cptr_t* fx_result)
{
    fx_re_astnode_t buf[FX_RE_AST_BUFSIZE];
    fx_re_ast_t ast = {buf, 0, 0, FX_RE_AST_BUFSIZE, 1, false};
    int status = fx_re_parse_(str, &ast);
    if(status >= 0)
    {
        fx_regex_t* re = fx_re_compile_(&ast);
        if(re != 0)
            status = fx_make_cptr(re, fx_free, fx_result);
        else
            status = FX_SET_EXN_FAST(FX_EXN_BadArgError);
    }
    //if(status >= 0)
    //    fx_re_printprog(*fx_result);
    if(ast.allocated) {fx_free(ast.buf);}
    return status;
}

int fx_re_match(const fx_cptr_t regex, const fx_str_t* str,
                int flags, fx_arr_t* fx_result)
{
    int_ subbuf[FX_RE_SUB_BUFSIZE], *sub = subbuf;
    int nsub = 0, status;
    status = fx_re_pikevm_(regex, str, flags, &sub, FX_RE_SUB_BUFSIZE, &nsub);

    if(status > 0) {
        int ndims = (flags & FX_RE_MODE_MASK_) == FX_RE_FINDALL_MODE_ ? 2 : 1;
        int_ sz[2] = {(ndims > 1 ? status : (nsub/2)), (nsub/2)};
        status = fx_make_arr(ndims, sz, sizeof(sub[0])*2, 0, 0, sub, fx_result);
    }
    if(sub != subbuf) fx_free(sub);
    return status;
}

int fx_re_fullmatch(const fx_cptr_t regex, const fx_str_t* str, int flags)
{
    int_ subbuf[FX_RE_SUB_BUFSIZE], *sub = subbuf;
    int nsub = 0, status;
    status = fx_re_pikevm_(regex, str, flags | FX_RE_MATCH_MODE_, &sub, FX_RE_SUB_BUFSIZE, &nsub);
    if(status > 0) {
        status = sub[1] == str->length;
    }
    if(sub != subbuf) fx_free(sub);
    return status;
}

int fx_re_replace(const fx_cptr_t regex, const fx_str_t* str,
                  const fx_str_t* subst, int flags, fx_str_t* result)
{
    int_ subbuf[FX_RE_SUB_BUFSIZE], *sub = subbuf;
    int nsub = 0, status;
    status = fx_re_pikevm_(regex, str, (flags | FX_RE_FINDALL_MODE_),
                           &sub, FX_RE_SUB_BUFSIZE, &nsub);
    if(status < 0) return status;
    if(status == 0) {
        fx_copy_str(str, result);
        return FX_OK;
    }

    /*
        Preprocess 'subst'; convert it to
           [a0, b0), \group_j0, [a1, b2), \group_j1 \group_j2 etc.
        That is, we find references to the original string in the form
        \0, \1 etc. and represent the substring as a sequence of
        clusters, where the literal fragments are interleaved with
        subgroup indices. Subgroups are represented by (-1, subgroup_idx) pairs..

        There are at maximum 10 subgroups (the whole match + up to 9 its submatches),
        but in the subst the same submatch can be referenced many times,
        and some submatches may be not referenced at all, e.g.

        re.compile(r"\bfor\s+(\w+)\s*<-\s*(\w+)\s*:\s*(\w+)").replace(
                "for i <- 0:100",
                r"for(int \1 = \2; \1 < \3; \1++)")
        here the whole match (\0) is not referenced, but the first submatch
        is referenced several times.
        The subst in this case is preprocessed into
        'for(int ', (-1, 1), ' = ', (-1, 2) ...
        where each literal fragment is represented by the pair of indices.
    */
    {
        const char_ *data0 = str->data, *data1 = subst->data;
        int_ len0 = str->length, len1 = subst->length;
        int i, j, nmatches = status, match_nsegments = 0;
        int_ matchsegm[FX_RE_NS_BUFSIZE], start = 0;
        int_ total = 0, k = 0, prev;
        char_* outdata;

        for(i = 0; i < len1; i++) {
            if (data1[i] == '\\' && i+1 < len1 && '0' <= data1[i+1] && data1[i+1] <= '9') {
                int idx = data1[i+1] - '0';
                if ((idx+1)*2 > nsub) {
                    if(sub != subbuf) fx_free(sub);
                    return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);
                }
                if ((match_nsegments+2)*2 > FX_RE_NS_BUFSIZE) {
                    if(sub != subbuf) fx_free(sub);
                    return FX_SET_EXN_FAST(FX_EXN_OverflowError);
                }
                if (i > start) {
                    matchsegm[match_nsegments*2] = start;
                    matchsegm[match_nsegments*2+1] = i;
                    match_nsegments++;
                }
                matchsegm[match_nsegments*2] = -1;
                matchsegm[match_nsegments*2+1] = idx;
                match_nsegments++;
                start = i+2;
                ++i;
            }
        }
        if (i > start) {
            matchsegm[match_nsegments*2] = start;
            matchsegm[match_nsegments*2+1] = i;
            match_nsegments++;
        }

        // pass 1. compute the total length of the output string
        for(i = 0; i < nmatches; i++) {
            prev = i > 0 ? sub[(i-1)*nsub+1] : 0;
            int_ a = sub[i*nsub], b = sub[i*nsub+1];
            total += a - prev;
            for(j = 0; j < match_nsegments; j++) {
                a = matchsegm[j*2];
                b = matchsegm[j*2+1];
                if(a < 0) {
                    a = sub[i*nsub + b*2];
                    b = sub[i*nsub + b*2+1];
                }
                total += b - a;
            }
        }

        total += len0 - sub[(nmatches-1)*nsub+1];
        status = fx_make_str(0, total, result);
        if(status < 0) {
            if(sub != subbuf) fx_free(sub);
            return status;
        }
        outdata = result->data;

        // pass 2. actually form the output string
        for(i = 0; i < nmatches; i++) {
            prev = i > 0 ? sub[(i-1)*nsub+1] : 0;
            int_ a = sub[i*nsub], b = sub[i*nsub+1];
            if (a > prev) {
                assert(k + a - prev <= total);
                memcpy(outdata + k, data0 + prev, (a - prev)*sizeof(outdata[0]));
                k += a - prev;
            }
            for(j = 0; j < match_nsegments; j++) {
                const char_* ptr = data1;
                a = matchsegm[j*2];
                b = matchsegm[j*2+1];
                if(a < 0) {
                    ptr = data0;
                    a = sub[i*nsub + b*2];
                    b = sub[i*nsub + b*2+1];
                }
                assert(k + b - a <= total);
                memcpy(outdata + k, ptr + a, (b - a)*sizeof(outdata[0]));
                k += b - a;
            }
        }
        prev = sub[(nmatches-1)*nsub+1];
        if(len0 > prev) {
            assert(k + len0 - prev <= total);
            memcpy(outdata + k, data0 + prev, (len0 - prev)*sizeof(outdata[0]));
            k += len0 - prev;
        }
        assert(k == total);
    }
    if(sub != subbuf) fx_free(sub);
    return status;
}

#ifdef __cplusplus
}
#endif

#endif
