/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// This regexp engine is the modified version of re1 by Russ Cox:
// https://code.google.com/archive/p/re1/
//
// Copyright 2007-2009 Russ Cox.  All Rights Reserved.
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

#define fx_re_nelem_(x) (sizeof(x)/sizeof((x)[0]))

typedef struct fx_Regexp fx_Regexp;
typedef struct fx_Prog fx_Prog;
typedef struct fx_Inst fx_Inst;

struct fx_Regexp
{
	int type;
	int n;
	int ch;
	fx_Regexp *left;
	fx_Regexp *right;
};

enum	/* fx_Regexp.type */
{
	FX_RE_ALT = 1,
	FX_RE_CAT,
	FX_RE_LIT,
	FX_RE_DOT,
	FX_RE_PAREN,
	FX_RE_QUEST,
	FX_RE_STAR,
	FX_RE_PLUS
};

static fx_Regexp *fx_re_parse(char*);
static fx_Regexp *reg(int type, fx_Regexp *left, fx_Regexp *right);
static void fx_re_printre_(fx_Regexp*);
static void fx_re_fatal_(const char*, ...);

static void fx_re_free_regexp_(fx_Regexp* regexp)
{
    if(regexp == 0)
        return;
    fx_re_free_regexp_(regexp->left);
    fx_re_free_regexp_(regexp->right);
    fx_free(regexp);
}

struct fx_Prog
{
	fx_Inst *start;
	int len;
};

struct fx_Inst
{
	int opcode;
	int c;
	int n;
	fx_Inst *x;
	fx_Inst *y;
	int gen;	// global state, oooh!
};

enum	/* fx_Inst.opcode */
{
	FX_RE_OP_CHAR = 1,
	FX_RE_OP_MATCH,
	FX_RE_OP_JMP,
	FX_RE_OP_SPLIT,
	FX_RE_OP_ANY,
	FX_RE_OP_SAVE,
};

enum {
	MAXSUB = 20
};

typedef struct fx_re_Sub fx_re_Sub;

struct fx_re_Sub
{
	int ref;
	int nsub;
	char *sub[MAXSUB];
};

static fx_re_Sub *fx_re_newsub_(int n);
static fx_re_Sub *fx_re_incref_(fx_re_Sub*);
static fx_re_Sub *copy(fx_re_Sub*);
static fx_re_Sub *fx_re_update_(fx_re_Sub*, int, char*);
static void fx_re_decref_(fx_re_Sub*);

static fx_re_Sub *freesub = 0;

static fx_re_Sub*
fx_re_newsub_(int n)
{
	fx_re_Sub *s;

	s = freesub;
	if(s != 0)
		freesub = (fx_re_Sub*)s->sub[0];
	else
    {
		s = (fx_re_Sub*)fx_malloc(sizeof *s);
        memset(s, 0, sizeof *s);
    }
	s->nsub = n;
	s->ref = 1;
	return s;
}

static fx_re_Sub*
fx_re_incref_(fx_re_Sub *s)
{
	s->ref++;
	return s;
}

static fx_re_Sub*
fx_re_update_(fx_re_Sub *s, int i, char *p)
{
	fx_re_Sub *s1;
	int j;

	if(s->ref > 1) {
		s1 = fx_re_newsub_(s->nsub);
		for(j=0; j<s->nsub; j++)
			s1->sub[j] = s->sub[j];
		s->ref--;
		s = s1;
	}
	s->sub[i] = p;
	return s;
}

static void
fx_re_decref_(fx_re_Sub *s)
{
	if(--s->ref == 0) {
		s->sub[0] = (char*)freesub;
		freesub = s;
	}
}

static fx_Inst *pc = 0;
static int fx_re_count_(fx_Regexp*);
static void fx_re_emit_(fx_Regexp*);

////////////////////////////// autogenerate files //////////////////////////////

#include "regex_parse.c"

////////////////////// compile & interpret regexps /////////////////////////////

static fx_Prog*
fx_re_compile_(fx_Regexp *r)
{
	int n;
	fx_Prog *p;

	n = fx_re_count_(r) + 1;
	p = (fx_Prog*)fx_malloc(sizeof *p + n*sizeof p->start[0]);
	p->start = (fx_Inst*)(p+1);
	pc = p->start;
	fx_re_emit_(r);
	pc->opcode = FX_RE_OP_MATCH;
	pc++;
	p->len = pc - p->start;
	return p;
}

// how many instructions does r need?
static int
fx_re_count_(fx_Regexp *r)
{
	switch(r->type) {
	default:
		fx_re_fatal_("bad fx_re_count_");
	case FX_RE_ALT:
		return 2 + fx_re_count_(r->left) + fx_re_count_(r->right);
	case FX_RE_CAT:
		return fx_re_count_(r->left) + fx_re_count_(r->right);
	case FX_RE_LIT:
	case FX_RE_DOT:
		return 1;
	case FX_RE_PAREN:
		return 2 + fx_re_count_(r->left);
	case FX_RE_QUEST:
		return 1 + fx_re_count_(r->left);
	case FX_RE_STAR:
		return 2 + fx_re_count_(r->left);
	case FX_RE_PLUS:
		return 1 +  fx_re_count_(r->left);
	}
}

static void
fx_re_emit_(fx_Regexp *r)
{
	fx_Inst *p1, *p2, *t;

	switch(r->type) {
	default:
		fx_re_fatal_("bad fx_re_emit_");

	case FX_RE_ALT:
		pc->opcode = FX_RE_OP_SPLIT;
		p1 = pc++;
		p1->x = pc;
		fx_re_emit_(r->left);
		pc->opcode = FX_RE_OP_JMP;
		p2 = pc++;
		p1->y = pc;
		fx_re_emit_(r->right);
		p2->x = pc;
		break;

	case FX_RE_CAT:
		fx_re_emit_(r->left);
		fx_re_emit_(r->right);
		break;

	case FX_RE_LIT:
		pc->opcode = FX_RE_OP_CHAR;
		pc->c = r->ch;
		pc++;
		break;

	case FX_RE_DOT:
		pc++->opcode = FX_RE_OP_ANY;
		break;

	case FX_RE_PAREN:
		pc->opcode = FX_RE_OP_SAVE;
		pc->n = 2*r->n;
		pc++;
		fx_re_emit_(r->left);
		pc->opcode = FX_RE_OP_SAVE;
		pc->n = 2*r->n + 1;
		pc++;
		break;

	case FX_RE_QUEST:
		pc->opcode = FX_RE_OP_SPLIT;
		p1 = pc++;
		p1->x = pc;
		fx_re_emit_(r->left);
		p1->y = pc;
		if(r->n) {	// non-greedy
			t = p1->x;
			p1->x = p1->y;
			p1->y = t;
		}
		break;

	case FX_RE_STAR:
		pc->opcode = FX_RE_OP_SPLIT;
		p1 = pc++;
		p1->x = pc;
		fx_re_emit_(r->left);
		pc->opcode = FX_RE_OP_JMP;
		pc->x = p1;
		pc++;
		p1->y = pc;
		if(r->n) {	// non-greedy
			t = p1->x;
			p1->x = p1->y;
			p1->y = t;
		}
		break;

	case FX_RE_PLUS:
		p1 = pc;
		fx_re_emit_(r->left);
		pc->opcode = FX_RE_OP_SPLIT;
		pc->x = p1;
		p2 = pc;
		pc++;
		p2->y = pc;
		if(r->n) {	// non-greedy
			t = p2->x;
			p2->x = p2->y;
			p2->y = t;
		}
		break;
	}
}

static void
fx_re_printprog_(fx_Prog *p)
{
	fx_Inst *pc, *e;

	pc = p->start;
	e = p->start + p->len;

	for(; pc < e; pc++) {
		switch(pc->opcode) {
		default:
			fx_re_fatal_("printprog");
		case FX_RE_OP_SPLIT:
			printf("%2d. split %d, %d\n", (int)(pc-p->start), (int)(pc->x-p->start), (int)(pc->y-p->start));
			break;
		case FX_RE_OP_JMP:
			printf("%2d. jmp %d\n", (int)(pc-p->start), (int)(pc->x-p->start));
			break;
		case FX_RE_OP_CHAR:
			printf("%2d. char %c\n", (int)(pc-p->start), pc->c);
			break;
		case FX_RE_OP_ANY:
			printf("%2d. any\n", (int)(pc-p->start));
			break;
		case FX_RE_OP_MATCH:
			printf("%2d. match\n", (int)(pc-p->start));
			break;
		case FX_RE_OP_SAVE:
			printf("%2d. save %d\n", (int)(pc-p->start), pc->n);
		}
	}
}

typedef struct fx_re_Thread fx_re_Thread;
struct fx_re_Thread
{
	fx_Inst *pc;
	fx_re_Sub *sub;
};

typedef struct fx_re_ThreadList fx_re_ThreadList;
struct fx_re_ThreadList
{
	int n;
	fx_re_Thread t[1];
};

static fx_re_Thread
fx_re_thread_(fx_Inst *pc, fx_re_Sub *sub)
{
	fx_re_Thread t = {pc, sub};
	return t;
}

static fx_re_ThreadList*
threadlist(int n)
{
    size_t size = sizeof(fx_re_ThreadList)+n*sizeof(fx_re_Thread);
	fx_re_ThreadList* tl = (fx_re_ThreadList*)fx_malloc(size);
    if(tl)
        memset(tl, 0, size);
    return tl;
}

static void
fx_re_addthread_(fx_re_ThreadList *l, fx_re_Thread t, char *sp)
{
	if(t.pc->gen == gen) {
		fx_re_decref_(t.sub);
		return;	// already on list
	}
	t.pc->gen = gen;

	switch(t.pc->opcode) {
	default:
		l->t[l->n] = t;
		l->n++;
		break;
	case FX_RE_OP_JMP:
		fx_re_addthread_(l, fx_re_thread_(t.pc->x, t.sub), sp);
		break;
	case FX_RE_OP_SPLIT:
		fx_re_addthread_(l, fx_re_thread_(t.pc->x, fx_re_incref_(t.sub)), sp);
		fx_re_addthread_(l, fx_re_thread_(t.pc->y, t.sub), sp);
		break;
	case FX_RE_OP_SAVE:
		fx_re_addthread_(l, fx_re_thread_(t.pc+1, fx_re_update_(t.sub, t.pc->n, sp)), sp);
		break;
	}
}

static int
fx_re_pikevm_(fx_Prog *prog, char *input, char **subp, int nsubp)
{
	int i, len;
	fx_re_ThreadList *clist, *nlist, *tmp;
	fx_Inst *pc;
	char *sp;
	fx_re_Sub *sub, *matched;

	matched = 0;
	for(i=0; i<nsubp; i++)
		subp[i] = 0;
	sub = fx_re_newsub_(nsubp);
	for(i=0; i<nsubp; i++)
		sub->sub[i] = 0;

	len = prog->len;
	clist = threadlist(len);
	nlist = threadlist(len);

	gen++;
	fx_re_addthread_(clist, fx_re_thread_(prog->start, sub), input);
	matched = 0;
	for(sp=input;; sp++) {
		if(clist->n == 0)
			break;
		// printf("%d(%02x).", (int)(sp - input), *sp & 0xFF);
		gen++;
		for(i=0; i<clist->n; i++) {
			pc = clist->t[i].pc;
			sub = clist->t[i].sub;
			// printf(" %d", (int)(pc - prog->start));
			switch(pc->opcode) {
			case FX_RE_OP_CHAR:
				if(*sp != pc->c) {
					fx_re_decref_(sub);
					break;
				}
			case FX_RE_OP_ANY:
				if(*sp == 0) {
					fx_re_decref_(sub);
					break;
				}
				fx_re_addthread_(nlist, fx_re_thread_(pc+1, sub), sp+1);
				break;
			case FX_RE_OP_MATCH:
				if(matched)
					fx_re_decref_(matched);
				matched = sub;
				for(i++; i < clist->n; i++)
					fx_re_decref_(clist->t[i].sub);
				goto BreakFor;
			// FX_RE_OP_JMP, FX_RE_OP_SPLIT, FX_RE_OP_SAVE handled in fx_re_addthread_, so that
			// machine execution matches what a backtracker would do.
			// This is discussed (but not shown as code) in
			// Regular Expression Matching: the Virtual Machine Approach.
			}
		}
	BreakFor:
		// printf("\n");
		tmp = clist;
		clist = nlist;
		nlist = tmp;
		nlist->n = 0;
		if(*sp == '\0')
			break;
	}
	fx_free(clist);
	fx_free(nlist);

	if(matched) {
		for(i=0; i<nsubp; i++)
			subp[i] = matched->sub[i];
		fx_re_decref_(matched);
		return 1;
	}
	return 0;
}

int fx_re_compile(const fx_str_t* str, fx_regex_t* fx_result)
{
    fx_cstr_t cstr;
    int fx_status = fx_str2cstr(str, &cstr, 0, 0);
    if(fx_status>=0)
    {
        fx_Regexp* regexp = fx_re_parse(cstr.data);
        fx_free_cstr(&cstr);
        if(regexp!=0)
        {
            fx_Prog* prog = fx_re_compile_(regexp);
            fx_re_free_regexp_(regexp);
            if(prog!=0)
                fx_status = fx_make_cptr(prog, fx_free, fx_result);
            else
                fx_status = FX_SET_EXN_FAST(FX_EXN_BadArgError);
        }
        else
            fx_status = FX_SET_EXN_FAST(FX_EXN_BadArgError);
    }
    return fx_status;
}

int fx_re_match(const fx_regex_t fx_regexp, const fx_str_t* str, bool* fx_result/*, fx_arr_t* fx_result_subs*/)
{
    fx_cstr_t cstr;
    int fx_status = fx_str2cstr(str, &cstr, 0, 0);
    if(fx_status>=0)
    {
        char *sub[MAXSUB]; //TODO: Change maximum amount;
        memset(sub, 0, sizeof sub);
        fx_Prog* prog = (fx_Prog*)(fx_regexp->ptr);
		*fx_result = fx_re_pikevm_(prog, cstr.data, sub, fx_re_nelem_(sub));
        if (sub[0] != cstr.data || sub[1] != cstr.data + cstr.length)
            *fx_result = false;
        fx_free_cstr(&cstr);
    }
    return fx_status;
}

#ifdef __cplusplus
}
#endif

#endif
