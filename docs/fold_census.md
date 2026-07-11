# fold census (fold-1 Phase 0.2)

Every `fold` KEYWORD site in `lib/ compiler/ test/ examples/`, classified by the
shape of the body **tail** (the last expression, folded into the accumulator
each iteration) to drive the Phase-2 automigrator. This does NOT cover the
`name(for ...)` sugar family (`all`/`exists`/`count`/`find`/`find_opt`/`filter`/
`vector`) — those are not `fold` keyword sites; they are re-expressed through
`__fold__` inside the compiler in Phase 1.

Method: `grep -nE '\bfold\b'` per file, excluding `__fold_result__`, `unfold`,
`manifold`, identifier names (`fold_x`/`x_fold`), the `FOLD` token defs, and
"fold" in comments/strings. Each real site read for its accumulator arity and
body tail. Classification done by four parallel readers, aggregated here.

## Categories

- **A** `single-op=` — single accumulator `s`; tail `s <op> rhs` with `s` the
  LEFT operand and `<op>` ∈ {`+ - * / % & | ^ << >> .* ./ .%`}. Migrates to
  `s <op>= rhs`.
- **B** `single-expr` — single accumulator; tail is any other expression (a
  call, another var, a whole `match`/`if` value, a `::` cons). Migrates to
  `s = E`.
- **C** `tuple-ctor` — tuple accumulator; tail is a literal tuple `(e1,…,en)`.
  Migrates to per-component assignments (with `op=` beautification) when no
  component reads a DIFFERENT accumulator, else to a simultaneous tuple
  assignment.
- **D** `tuple-nonctor` — tuple accumulator; tail is NOT a literal tuple (a
  `match`/`if` whose arms are tuples, or a tuple-returning call). Migrates to
  the simultaneous tuple assignment `(a,…,z) = E` — Phase 0.1 exists for
  exactly this, and simultaneity matters (the arms read the OLD accumulators).
- **E** `bare-acc` — tail is the bare accumulator (degenerate; drop the tail).
- **W** `closure-capture` — the body contains a lambda that references the
  accumulator. **Semantic-change watchlist** (old fold captured a per-iteration
  snapshot; new fold captures the var).
- **M** `manual` — unclear / fits none.

## Headline findings

- **W is EMPTY across the entire corpus.** No `fold` body anywhere closes over
  its own accumulator, so the new `__fold__` var-capture semantics cannot
  change behavior at any migrated site — the reform's single largest semantic
  risk is dissolved by the data.
- **No true E (bare no-op) sites**, and **no M (manual)** — every site fits a
  mechanical rule. Two sites *return* the bare accumulator but only after an
  inner `val` rebinds it (C_gen_code:1304, :2969) — those are ordinary B.
- **A is dominated by lib/test math** (norms, dot products, hashing); the
  compiler is almost all B (accumulator-list building `x :: acc`) and C/D
  (folds returning tuples of `(list, code)` accumulators).
- **~30 D sites** (tuple accumulator, `match`/`if` tail) are the ones that
  consume the Phase-0.1 simultaneous tuple assignment; several read the other
  accumulators in the `else` arm, so simultaneity is load-bearing.
- `valfold` = the `val fold acc = init …` sugar (accumulator names are the val
  bindings); `fold` = a bare fold expression (incl. `val (pat) = fold …`
  destructuring and the one assignment `match_var_cases = fold …`).

## Tally (261 sites)

| cat | meaning | count | migration |
|-----|---------|------:|-----------|
| A | single-acc, `s <op> rhs` (s left) | 47 | `s <op>= rhs` |
| B | single-acc, other expr | 115 | `s = E` |
| C | tuple-acc, literal-tuple tail | 60 | per-component (or simultaneous) |
| D | tuple-acc, non-literal tail (match/if/call) | 38 | `(a,…,z) = E` |
| E | bare accumulator (degenerate) | 1 | drop tail |
| W | closure captures accumulator | **0** | — (no semantic risk) |
| M | manual | **0** | — |

Accumulator arity: **163 single / 98 tuple**. Per directory: compiler ~171,
lib 62, test+examples 28. The lone E (K_fast_idx.fx:240) returns the bare
accumulator; the migrator drops its tail.

---

## test/ + examples/

### test/test_nn_quant.fx
test/test_nn_quant.fx:98 | valfold | 1 | A | tail: s + (x[n, c, i, j] - xzp)*xsc

### test/myops.fx
test/myops.fx:19 | fold | 1 | A | tail: s+x

### test/test_resolve.fx
test/test_resolve.fx:104 | valfold | 1 | B | tail: [:: x*x] + prog

### test/ir/fold.fx
test/ir/fold.fx:2 | valfold | 1 | A | tail: acc + x

### test/test_ds.fx
test/test_ds.fx:42 | valfold | 1 | A | tail: sum0 + i
test/test_ds.fx:90 | valfold | 1 | B | tail: wcounter.add(w, wcounter.find(w, 0)+1)
test/test_ds.fx:111 | valfold | 1 | B | tail: wcounter2.update(w, fun (_, ci_opt) {...})
test/test_ds.fx:122 | valfold | 1 | A | tail: c+ci
test/test_ds.fx:127 | valfold | N | D | tail: if c%2==0 {(wcounter_odd.remove(w), ll_odd)} else {(wcounter_odd, (w,c)::ll_odd)}
test/test_ds.fx:204 | valfold | 1 | A | tail: nbad + (if m.find_opt((i,i+1)).value_or(-1)==i*1000 {0} else {1})

### test/test_nbody.fx
test/test_nbody.fx:78 | fold | 1 | A | tail: p - body.vel * (body.mass / SolarMass)
test/test_nbody.fx:86 | fold | 1 | B | tail: e + 0.5*bi.mass*dot(bi.vel,bi.vel) - (fold e1 = 0. for j <- ...)
test/test_nbody.fx:90 | fold | 1 | A | tail: e1 + (bi.mass * bj.mass) / distance

### test/test_btree.fx
test/test_btree.fx:36 | valfold | 1 | A | tail: c + check(make(d))

### test/test_spectralnorm.fx
test/test_spectralnorm.fx:24 | fold | 1 | A | tail: t + A(i, j) * u[j]
test/test_spectralnorm.fx:35 | fold | 1 | A | tail: t + A(j, i) * u[j]
test/test_spectralnorm.fx:45 | valfold | N | C | tail: (AtAu(v), v)
test/test_spectralnorm.fx:47 | valfold | N | C | tail: (vBv + ui*vi, vv + vi*vi)

### test/test_array.fx
test/test_array.fx:145 | valfold | N | D | tail: if pix != 0 {(min(minx,x), max(maxx,x), min(miny,y), max(maxy,y))} else {(minx,maxx,miny,maxy)}

### test/rand/test_rand_array.fx
test/rand/test_rand_array.fx:149 | valfold | 1 | A | tail: acc + x

### examples/knucleotide.fx
examples/knucleotide.fx:74 | valfold | 1 | B | tail: key*4 + encrypt_char(c)

### examples/spectralnorm.fx
examples/spectralnorm.fx:16 | fold | 1 | A | tail: t + uj / A(i, j)
examples/spectralnorm.fx:27 | fold | 1 | A | tail: t + uj / A(j, i)
examples/spectralnorm.fx:43 | valfold | N | C | tail: (vBv + ui*vi, vv + vi*vi)

### examples/btree.fx
examples/btree.fx:45 | valfold | 1 | A | tail: c + check(make(d))

### examples/nbody.fx
examples/nbody.fx:72 | fold | 1 | A | tail: p - body.vel * (body.mass / SolarMass)
examples/nbody.fx:80 | fold | 1 | B | tail: e + 0.5*bi.mass*dot(bi.vel,bi.vel) - (fold e1 = 0. for j <- ...)
examples/nbody.fx:84 | fold | 1 | A | tail: e1 + (bi.mass * bj.mass) / distance

### examples/fst.fx
examples/fst.fx:188 | valfold | N | C | tail: (min(minv, y), max(maxv, y))

---

## lib/

### lib/Sys.fx
lib/Sys.fx:100 | valfold | N | D | tail: if iterations>1 && i==0 {(gmean, new_mintime)} else {(gmean+log_t, new_mintime)}

### lib/List.fx
lib/List.fx:31 | fold | 1 | B | tail: a :: r
lib/List.fx:34 | fold | 1 | B | tail: f(a, res)
lib/List.fx:70 | fold | 1 | B | tail: l + s   (s is RIGHT operand -> not op=)

### lib/Builtins.fx
lib/Builtins.fx:577 | fold | 1 | B | tail: if aj == bj {f} else {false}
lib/Builtins.fx:579 | fold | 1 | B | tail: if d != 0 {d} else {aj <=> bj}
lib/Builtins.fx:581 | fold | 1 | B | tail: if aj == bj {f} else {false}
lib/Builtins.fx:583 | fold | 1 | B | tail: if d != 0 {d} else {aj <=> bj}
lib/Builtins.fx:669 | fold | 1 | A | tail: s + aj*bj
lib/Builtins.fx:685 | fold | 1 | B | tail: max(s, normInf(aj))
lib/Builtins.fx:687 | fold | 1 | A | tail: s + normL1(aj)
lib/Builtins.fx:689 | fold | 1 | A | tail: s + normL2sqr(aj)
lib/Builtins.fx:692 | fold | 1 | B | tail: max(s, normInf(aj, bj))
lib/Builtins.fx:694 | fold | 1 | A | tail: s + normL1(aj, bj)
lib/Builtins.fx:696 | fold | 1 | A | tail: s + normL2sqr(aj, bj)
lib/Builtins.fx:736 | fold | 1 | A | tail: f & x
lib/Builtins.fx:737 | fold | 1 | A | tail: f | x
lib/Builtins.fx:775 | fold | 1 | A | tail: f & (aj === bj)
lib/Builtins.fx:777 | fold | 1 | A | tail: f & (aj === bj)
lib/Builtins.fx:1169 | fold | 1 | A | tail: h * FNV_1A_PRIME
lib/Builtins.fx:1175 | fold | 1 | A | tail: h * FNV_1A_PRIME

### lib/NN/Inference.fx
lib/NN/Inference.fx:367 | valfold | 1 | B | tail: max(maxk, ik)

### lib/Set.fx
lib/Set.fx:338 | valfold | N | C | tail: (new_root, size+dsz)

### lib/Json.fx
lib/Json.fx:210 | valfold | 1 | B | tail: if i < n-1 {..; l_newind|ofs+2} else {printf(" "); ofs}

### lib/String.fx
lib/String.fx:264 | valfold | N | D | tail: if f(c) {(.., start, true)} else {(sl, .., false)}
lib/String.fx:276 | valfold | N | D | tail: if ci==c {(.., i+1, true)} else {(sl, .., false)}
lib/String.fx:331 | valfold | N | D | tail: if code>=40 && code!=92 {(ll, verb)} else {..}

### lib/Array.fx
lib/Array.fx:23 | fold | 1 | A | tail: p*szj
lib/Array.fx:234 | fold | 1 | A | tail: s + aj
lib/Array.fx:237 | fold | 1 | A | tail: s + aj
lib/Array.fx:240 | fold | 1 | A | tail: p * aj
lib/Array.fx:245 | fold | 1 | B | tail: max(s, normInf(aj))
lib/Array.fx:248 | fold | 1 | A | tail: s + normL1(aj)
lib/Array.fx:251 | fold | 1 | A | tail: s + normL2sqr(aj)
lib/Array.fx:256 | fold | 1 | B | tail: max(s, normInf(aj, bj))
lib/Array.fx:259 | fold | 1 | A | tail: s + normL1(aj, bj)
lib/Array.fx:262 | fold | 1 | A | tail: s + normL2sqr(aj, bj)
lib/Array.fx:270 | fold | N | D | tail: if x < minv {(x, i)} else {(minv, mini)}
lib/Array.fx:279 | fold | N | D | tail: if x > maxv {(x, i)} else {(maxv, maxi)}
lib/Array.fx:408 | fold | 1 | B | tail: if lt(b, p) {..; i0+1} else {i0}
lib/Array.fx:417 | fold | 1 | B | tail: if lt(arr[j], p) {_swap_..; i0+1} else {i0}
lib/Array.fx:663 | fold | 1 | A | tail: s + a[i, i]

### lib/Deque.fx
lib/Deque.fx:81 | valfold | 1 | B | tail: f(x, res)
lib/Deque.fx:82 | fold | 1 | B | tail: f(x, res)
lib/Deque.fx:87 | valfold | 1 | B | tail: f(x, res)
lib/Deque.fx:88 | fold | 1 | B | tail: f(x, res)

### lib/UTest.fx
lib/UTest.fx:391 | valfold | 1 | B | tail: if (!matches ^ inverse_test) {failed} else {..}

### lib/Re2.fx
lib/Re2.fx:340 | fold | 1 | B | tail: if (ok) {piece::filtered} else {filtered}
lib/Re2.fx:351 | valfold | 1 | B | tail: match piece { RPString => max_sub | RPInt => max(max_sub, sub_num) }
lib/Re2.fx:972 | valfold | 1 | B | tail: match piece { RPString => ..::rlist | RPInt => ..::rlist }

### lib/NN/Ast.fx
lib/NN/Ast.fx:1277 | fold | 1 | A | tail: p*sz

### lib/NN/OpQuantized.fx
lib/NN/OpQuantized.fx:924 | valfold | 1 | A | tail: plane_size*inp_shape[i]

### lib/NN/FromOnnx.fx
lib/NN/FromOnnx.fx:308 | valfold | 1 | B | tail: rev_more_ops + prog   (prog is RIGHT operand -> not op=)

### lib/NN/OpDetect.fx
lib/NN/OpDetect.fx:59 | valfold | N | D | tail: if p > pmax {(k, p)} else {(best_class, pmax)}
lib/NN/OpDetect.fx:135 | valfold | N | D | tail: if ..p > pmax {(i, p)} else {(ibest, pmax)}

### lib/Map.fx
lib/Map.fx:335 | valfold | 1 | B | tail: add_(new_root, xk, xd, cmp)

### lib/NN/InferShapes.fx
lib/NN/InferShapes.fx:150 | valfold | 1 | A | tail: out_shape_a + shape_i.shape[axis]
lib/NN/InferShapes.fx:268 | valfold | 1 | B | tail: if enable_broadcast {out_shape.broadcast(shape_i)} else {..layout coerce}
lib/NN/InferShapes.fx:308 | valfold | N | D | tail: if i < axis {(sz1*sz, sz2)} else {(sz1, sz2*sz)}
lib/NN/InferShapes.fx:640 | valfold | 1 | A | tail: old_total*sz
lib/NN/InferShapes.fx:641 | valfold | N | D | tail: if sz==-1 {(new_total,true)} else if sz==0 {..} else {(new_total*sz, havem1)}
lib/NN/InferShapes.fx:825 | valfold | 1 | B | tail: if shape.shape[axis]==1 {..; out_ndims-1} else {out_ndims}

---

## compiler/ (non big-4)

### compiler/Ast.fx
compiler/Ast.fx:74 | fold | 1 | B | tail: if m_idx != loci_m_idx { if m_idx<=0 {loci} else {loc} } else { loc_t{...} }
compiler/Ast.fx:1022 | fold | 1 | B | tail: if szj < 0 || sz < 0 {-1} else {sz + szj}

### compiler/C_gen_types.fx
compiler/C_gen_types.fx:469 | fold | N | C | tail: (free_code, copy_code, make_code, (ni,cti)::relems, (ni,cti_arg,cti_arg_flags)::make_args)
compiler/C_gen_types.fx:518 | valfold | N | C | tail: (free_code, copy_code, make_code, (ni,cti)::relems, (arg_ni,cti_arg,cti_arg_flags)::make_args)
compiler/C_gen_types.fx:705 | valfold | N | D | tail: match kt { KTypVoid => (free_cases,copy_cases,uelems) | _ => (...,(ni_clean,ti)::uelems) }
compiler/C_gen_types.fx:1005 | valfold | 1 | B | tail: if is_used_decl(s, used_ids, true) { s::ctypes_ccode } else { ctypes_ccode }

### compiler/C_gen_fdecls.fx
compiler/C_gen_fdecls.fx:32 | valfold | 1 | B | tail: (arg, ctyp, arg_flags) :: args
compiler/C_gen_fdecls.fx:92 | valfold | N | C | tail: ((c_id, ctyp) :: relems, free_ccode)
compiler/C_gen_fdecls.fx:308 | valfold | N | C | tail: (init_ccode, iface->ki_id :: ids, pair :: pairs, all_ids.add(iface->ki_id))

### compiler/Compiler.fx
compiler/Compiler.fx:122 | fold | N | D | tail: if found {(preamble,found)} else if bare_name==mname {...} else if from_import {...} else {...}
compiler/Compiler.fx:141 | fold | 1 | B | tail: Lexer.PP_DEFINE :: Lexer.IDENT(true,n) :: Lexer.LITERAL(v) :: p
compiler/Compiler.fx:619 | valfold | N | C | tail: (any_cpp|is_cpp, any_recompiled|is_recompiled, clibs_j+all_clibs, ok&ok_j, obj::objs)
compiler/Compiler.fx:665 | fold | 1 | B | tail: if key!="" && seen.mem(key) {acc} else { if key!="" {seen.add(key)}; e::acc }

### compiler/K_flatten.fx
compiler/K_flatten.fx:85 | fold | 1 | B | tail: match new_e { KExpSeq(nested_elist,_) => nested_elist.rev()+code | _ => new_e::code }

### compiler/K_fuse_loops.fx
compiler/K_fuse_loops.fx:32 | valfold | N | D | tail: match e { KDefVal(_,KExpMap _,_)=>(nmaps+1,nfors) | ... | _ =>(nmaps,nfors) }
compiler/K_fuse_loops.fx:105 | valfold | N | D | tail: match dom { DomainElem(AtomId arr)=>...(arr_fuse_map,...::a2f) | _ =>(arr_fuse_map,a2f) }
compiler/K_fuse_loops.fx:147 | fold | N | D | tail: match dom { DomainElem(AtomId arr)=>... | _ => ((i,dom)::new_idl,pbody,arr_fuse_map) }
compiler/K_fuse_loops.fx:153 | valfold | N | D | tail: match nested_dom { DomainElem(AtomId nested_arr)=>... | _ => ((nested_i,nested_dom)::new_idl2,pbody2,new_fuse_map) }

### compiler/K_fast_idx.fx
compiler/K_fast_idx.fx:240 | valfold | 1 | E | tail: loop_idx (degenerate; but see note)
compiler/K_fast_idx.fx:242 | valfold | N | D | tail: match dom { DomainRange(...)when...=>(arr_id,loop_idx.add(...)) | ... | _ =>(arr_id,loop_idx) }
compiler/K_fast_idx.fx:264 | fold | 1 | B | tail: loop_idx.add(idx, LoopOverArr(arr_id, i))
compiler/K_fast_idx.fx:413 | valfold | N | D | tail: match idx { DomainRange _ =>(true,have_slow) | DomainElem _ =>(have_ranges,true) | _ =>(...) }
compiler/K_fast_idx.fx:579 | fold | 1 | B | tail: match aa_class { IdxSimple(i,scale,shift)=>... | _ => pre_for_code }
compiler/K_fast_idx.fx:696 | valfold | N | D | tail: match idx { DomainRange _ =>(true,have_slow,have_non_invariants,idx_atoms) | ... }

### compiler/K_inline.fx
compiler/K_inline.fx:133 | fold | 1 | B | tail: fold s = s for (f, a) <- al { s + (if f {10} else {1}) }  (nested fold)
compiler/K_inline.fx:134 | fold | 1 | A | tail: s + (if f { 10 } else { 1 })
compiler/K_inline.fx:138 | fold | 1 | A | tail: s + (if f { 10 } else { 1 })
compiler/K_inline.fx:147 | fold | 1 | A | tail: total + checks.length()
compiler/K_inline.fx:152 | fold | 1 | B | tail: 10 + total + idl.length()

### compiler/K_freevars.fx
compiler/K_freevars.fx:243 | valfold | 1 | B | tail: create_kdefval(fv_proxy, t, kv_flags, Some(deref_exp), prologue, kf_loc)

### compiler/K_tailrec.fx
compiler/K_tailrec.fx:104 | valfold | N | C | tail: (a1i::new_kf_params, (a2i,ti)::trec_args, f_init_code, loop_init_code)
compiler/K_tailrec.fx:158 | valfold | 1 | B | tail: set_new :: tcall_rcode

### compiler/K_form.fx
compiler/K_form.fx:699 | valfold | 1 | B | tail: (f, walk_atom_(a, loc)) :: new_row

### compiler/K_remove_unused.fx
compiler/K_remove_unused.fx:412 | valfold | 1 | B | tail: match e { KDefVal(n,_,_)=> if all_main_deps.mem(n){e::new_top} else{new_top} | ... }

### compiler/K_cfold_dealias.fx
compiler/K_cfold_dealias.fx:421 | valfold | 1 | B | tail: match a { AtomId n => ... | _ => try_cfold_str_concat(a, res_al) }

### compiler/K_mangle.fx
compiler/K_mangle.fx:132 | valfold | 1 | B | tail: mangle_ktyp_(targ, result)
compiler/K_mangle.fx:230 | fold | 1 | B | tail: mangle_ktyp_(a, result)
compiler/K_mangle.fx:241 | fold | 1 | B | tail: mangle_ktyp_(t, result)

### compiler/K_lift.fx
compiler/K_lift.fx:288 | valfold | 1 | B | tail: (new_fv, kv_typ) :: fvars_wt
compiler/K_lift.fx:527 | valfold | 1 | B | tail: create_kdefval(fv_proxy, t, new_kv_flags, Some(e), prologue, kf_loc)
compiler/K_lift.fx:613 | valfold | 1 | B | tail: (i, dom_i) :: idom_l

### compiler/K_annotate.fx
compiler/K_annotate.fx:33 | fold | 1 | B | tail: get_ktyp_deps_(t, deps)
compiler/K_annotate.fx:35 | fold | 1 | B | tail: get_ktyp_deps_(t, deps)
compiler/K_annotate.fx:37 | fold | 1 | B | tail: get_ktyp_deps_(ti, deps)
compiler/K_annotate.fx:47 | valfold | 1 | B | tail: get_ktyp_deps_(ti, deps)
compiler/K_annotate.fx:48 | fold | 1 | B | tail: deps.add(iname)
compiler/K_annotate.fx:51 | valfold | 1 | B | tail: get_ktyp_deps_(ti, deps)

### compiler/K_loop_inv.fx
compiler/K_loop_inv.fx:64 | fold | N | C | tail: (new_elist, new_e_idl_l, new_body)

---

## compiler/ big-4

### compiler/C_gen_code.fx
compiler/C_gen_code.fx:265 | fold | N | C | tail: (if idx==0 {fwd_decls} else {..::fwd_decls}, ..::init_calls, ..::deinit_calls)
compiler/C_gen_code.fx:822 | valfold | 1 | B | tail: CExpBinary(COpLogicAnd, check_exp, check_i, (CTypBool, loc))  (ctor call, not op=)
compiler/C_gen_code.fx:835 | valfold | 1 | B | tail: CExpBinary(COpAdd, sum_checks, check_i, (CTypCInt, loc))  (ctor call, not op=)
compiler/C_gen_code.fx:864 | fold | 1 | B | tail: ndims_i
compiler/C_gen_code.fx:987 | valfold | N | C | tail: (lists_i + list_exps, i_exps, n_exps, for_checks, incr_exps, init_checks, ...)
compiler/C_gen_code.fx:1158 | valfold | N | C | tail: (i_exp :: i_exps, n_exp :: n_exps, init_ccode)
compiler/C_gen_code.fx:1169 | valfold | 1 | B | tail: init_check_k :: init_checks
compiler/C_gen_code.fx:1271 | valfold | N | C | tail: (k_final + 1, (Some(CTypInt), init_exps, Some(check_exp), incr_exps_i) :: for_headers)
compiler/C_gen_code.fx:1280 | valfold | 1 | B | tail: CExpBinary(COpLogicAnd, check_exp, e, (CTypBool, ifor_loc))  (ctor call)
compiler/C_gen_code.fx:1292 | valfold | 1 | B | tail: Some(match check_exp_opt { | Some e => ... | _ => check_i })
compiler/C_gen_code.fx:1304 | fold | 1 | B | tail: body_ccode (rebound via inner val)
compiler/C_gen_code.fx:1320 | valfold | N | C | tail: (new_have_default, em_label_used || em_label_used_i, have_epilogues || ..., ...)
compiler/C_gen_code.fx:1324 | fold | N | C | tail: (ccheck_ij :: checks_i, ccode_ij :: pre_checks_i)
compiler/C_gen_code.fx:1379 | fold | 1 | B | tail: if_stmt :: pre_check_ij
compiler/C_gen_code.fx:1405 | valfold | 1 | B | tail: match s_i { | [:: CStmtIf(...)] => make_if(c_i, then_i, complex_if, loc_i) | _ => throw }
compiler/C_gen_code.fx:1758 | valfold | N | C | tail: (s_exp :: strs, ccode)
compiler/C_gen_code.fx:1862 | valfold | N | C | tail: (c_exp :: cargs, ccode)
compiler/C_gen_code.fx:1900 | valfold | N | C | tail: (i_exp :: i_exps, ccode)
compiler/C_gen_code.fx:1958 | valfold | N | C | tail: (carg :: args, ccode)
compiler/C_gen_code.fx:2025 | valfold | N | C | tail: (carg :: args, ccode)
compiler/C_gen_code.fx:2058 | valfold | N | C | tail: (ca :: cargs, ccode)
compiler/C_gen_code.fx:2083 | valfold | N | C | tail: (ca :: cargs, ccode)
compiler/C_gen_code.fx:2105 | fold | N | C | tail: (nscalars, scalars_data, make_int_exp(127, kloc) :: tags_data, arr_data, ccode)
compiler/C_gen_code.fx:2107 | valfold | N | D | tail: if f {(nscalars, scalars_data, ..::tags_data, elem_ptr::arr_data, ccode)} else {...}
compiler/C_gen_code.fx:2164 | valfold | N | D | tail: fold data = data, ccode = ccode for (_, a) <- arow { (e :: data, ccode) }  (nested fold)
compiler/C_gen_code.fx:2165 | fold | N | C | tail: (e :: data, ccode)
compiler/C_gen_code.fx:2207 | fold | N | D | tail: if f {(nscalars, scalars_data, ..::tags_data, elem_ptr::vec_data, ccode)} else {...}
compiler/C_gen_code.fx:2257 | valfold | N | C | tail: (e :: data, ccode)
compiler/C_gen_code.fx:2409 | valfold | N | D | tail: match d { DomainElem/DomainFast => (..), DomainRange => (..) }
compiler/C_gen_code.fx:2445 | valfold | N | D | tail: match d { DomainFast => (..), DomainElem => (..) }
compiler/C_gen_code.fx:2701 | valfold | 1 | A | tail: ndims + ndims_i
compiler/C_gen_code.fx:2725 | valfold | N | D | tail: match (for_flag_make, coll_ctyp, deref_ktyp(coll_typ, kloc)) { ... }
compiler/C_gen_code.fx:2830 | fold | N | C | tail: (k + 1, cmp_size_i :: cmp_size_list)
compiler/C_gen_code.fx:2837 | valfold | 1 | B | tail: if is_parallel_map {then_ccode} else {CExp(set_dstptr) :: then_ccode}
compiler/C_gen_code.fx:2882 | valfold | N | C | tail: ((coll_ctyp, elemtyp, dst_exp, dst_ptr, iter) :: dst_data, decl_dstptr_ccode + decl_dstptr_ccode_all)
compiler/C_gen_code.fx:2926 | valfold | 1 | B | tail: match for_flag_make { ForMakeArray => gen_copy_code(...) | ForMakeVector => ... }
compiler/C_gen_code.fx:2969 | valfold | 1 | B | tail: for_ccode (rebound via inner vals)
compiler/C_gen_code.fx:2988 | fold | 1 | B | tail: match e { CExpBinary(COpAssign,...) => CDefVal(...) :: for_ccode | e => throw }
compiler/C_gen_code.fx:3065 | valfold | 1 | B | tail: if k>0 || pre_body_ccode==[] {for_stmt} else {rccode2stmt(for_stmt :: pre_body_ccode, for_loc)}
compiler/C_gen_code.fx:3489 | valfold | 1 | B | tail: C_gen_types.gen_copy_code(src_exp, dst_exp, t, ccode, kf_loc)
compiler/C_gen_code.fx:3522 | valfold | 1 | B | tail: C_gen_types.gen_copy_code(src_exp, dst_exp, t, ccode, kf_loc)
compiler/C_gen_code.fx:3557 | valfold | 1 | B | tail: C_gen_types.gen_copy_code(src_exp, dst_exp, t, ccode, kf_loc)
compiler/C_gen_code.fx:3625 | valfold | N | D | tail: if is_global {(s :: global_vars, temp_init_vals)} else {(global_vars, s :: temp_init_vals)}
compiler/C_gen_code.fx:3684 | fold | N | C | tail: ((km, c_fdecls, mod_init_calls, all_exn_data_decls.rev()) :: kmods_plus, mod_exn_data_decls.rev() + ...)
compiler/C_gen_code.fx:3692 | valfold | 1 | B | tail: (cmodule_t {...}) :: cmods

### compiler/K_normalize.fx
compiler/K_normalize.fx:129 | valfold | N | C | tail: ((i, di) :: idom_list, code, body_code)
compiler/K_normalize.fx:167 | valfold | N | D | tail: match-arm form: (_, TypInt) => (i :: at_ids, body_code) | _ => throw
compiler/K_normalize.fx:177 | valfold | N | D | tail: match-arm form: (TypInt, _) => (i :: at_ids, KTypInt :: ktl) | _ => throw
compiler/K_normalize.fx:293 | valfold | N | C | tail: (ai :: args, code)
compiler/K_normalize.fx:310 | valfold | N | C | tail: (ai :: args, code)
compiler/K_normalize.fx:319 | valfold | N | C | tail: (krow.rev() :: krows, code, all_literals)
compiler/K_normalize.fx:320 | valfold | N | C | tail: ((f, a) :: krow, code, all_literals & islit)
compiler/K_normalize.fx:337 | valfold | N | C | tail: ((f, a) :: elems, code)
compiler/K_normalize.fx:356 | valfold | N | C | tail: (a :: ratoms, code)
compiler/K_normalize.fx:376 | valfold | N | C | tail: (a :: ratoms, code)
compiler/K_normalize.fx:398 | valfold | N | C | tail: (ai :: args, code)
compiler/K_normalize.fx:478 | fold | N | C | tail: ((pre_exp, idom_list, at_ids) :: pre_idom_ll, body_code)
compiler/K_normalize.fx:509 | fold | N | C | tail: (DomainElem(d)::dlist, code)
compiler/K_normalize.fx:515 | fold | N | C | tail: (d :: dlist, code)
compiler/K_normalize.fx:536 | fold | N | D | tail: if elem_id == ni { (j, j + 1) } else { (i, j + 1) }
compiler/K_normalize.fx:809 | valfold | 1 | B | tail: (ni, pi, found_t, found_idx) :: typed_rec_pl
compiler/K_normalize.fx:811 | valfold | N | D | tail: if get_orig_id(nj) == ni_orig { (idx, tj) } else { (found_idx, found_t) }
compiler/K_normalize.fx:966 | fold | 1 | B | tail: pat_simple_unpack(pi, ti, Some(ei), code, temp_prefix, flags, sc).1
compiler/K_normalize.fx:982 | fold | 1 | B | tail: pat_simple_unpack(pi, ti, Some(ei), code, temp_prefix, flags, sc).1
compiler/K_normalize.fx:1006 | fold | 1 | B | tail: pat_simple_unpack(pi, ti, Some(ei), code, temp_prefix, flags, sc).1
compiler/K_normalize.fx:1105 | valfold | 1 | B | tail: (pinfo_i :: plists_delta)
compiler/K_normalize.fx:1120 | valfold | 1 | B | tail: dispatch_pat(pinfo, plists)
compiler/K_normalize.fx:1245 | valfold | 1 | B | tail: (pi, ti, idx) :: pti_l
compiler/K_normalize.fx:1255 | valfold | 1 | B | tail: (pi, ti, idx) :: pti_l
compiler/K_normalize.fx:1304 | valfold | 1 | B | tail: (checks_.rev(), e) :: alt_cases
compiler/K_normalize.fx:1329 | fold | 1 | B | tail: match_var_cases.add(get_orig_id(n))
compiler/K_normalize.fx:1399 | fold | 1 | B | tail: match id_info(inst, df_loc) { IdFun(inst_df) => ... }
compiler/K_normalize.fx:1427 | valfold | N | C | tail: (pi :: inst_args, ti :: argtyps)
compiler/K_normalize.fx:1454 | valfold | N | C | tail: (i :: params, body_code)
compiler/K_normalize.fx:1487 | fold | 1 | B | tail: match-arm form on e: DefVariant(...) => ...
compiler/K_normalize.fx:1501 | fold | 1 | B | tail: match id_info(inst, dvar_loc) { IdVariant(...) => ... }
compiler/K_normalize.fx:1541 | fold | 1 | B | tail: match id_info(ctor, dvar_loc) { IdFun(...) => ... }
compiler/K_normalize.fx:1686 | valfold | 1 | B | tail: (minfo, kcode_typedefs) :: modules_plus

### compiler/Ast_typecheck.fx
compiler/Ast_typecheck.fx:749 | valfold | 1 | B | tail: match-arm form on e: EnvId(i) => (e::extra_entries | extra_entries) | _ => extra_entries
compiler/Ast_typecheck.fx:1506 | valfold | 1 | B | tail: add_typ_to_env(nt, t, env1)
compiler/Ast_typecheck.fx:1565 | fold | 1 | B | tail: add_typ_to_env(n, t, env)
compiler/Ast_typecheck.fx:1666 | valfold | N | C | tail: (trszj, (pi, ei) :: for_clauses, code_i + code, dims_i, env, idset)
compiler/Ast_typecheck.fx:1706 | valfold | N | D | tail: match ttrj { TypTuple => (def_pj :: code, env, idset) | _ => ... }
compiler/Ast_typecheck.fx:2404 | valfold | 1 | A | tail: possible_matches + new_matches
compiler/Ast_typecheck.fx:2556 | valfold | N | D | tail: match new_idx { ExpRange => (..) | _ => (new_idx::new_idxs, ndims+dim_inc, ...) }
compiler/Ast_typecheck.fx:2717 | fold | N | C | tail: (trsz + trsz_k, pre_code_k + pre_code, (for_clauses, idx_pat) :: map_clauses, total_dims + dims, env, idset)
compiler/Ast_typecheck.fx:2776 | fold | 1 | B | tail: ExpBinary(OpCons, ej, l_exp, (ltyp, eloc))
compiler/Ast_typecheck.fx:2848 | fold | N | C | tail: (if have_expanded_i {ncols_i} else {ncols}, arow.rev() :: arows, have_expanded, dims)
compiler/Ast_typecheck.fx:2850 | valfold | N | C | tail: (have_expanded_i || is_expanded, row_dims, elem1 :: arow)
compiler/Ast_typecheck.fx:2910 | valfold | 1 | B | tail: match elem { ExpUnary(OpExpand,...) => ... | _ => ... }
compiler/Ast_typecheck.fx:3224 | valfold | 1 | B | tail: match e { DefFun(df) => reg_deffun(df, env, sc) | DefExn(de) => check_defexn(de,env,sc) | _ => env }
compiler/Ast_typecheck.fx:3235 | valfold | N | C | tail: (eseq1, env1)
compiler/Ast_typecheck.fx:3397 | fold | 1 | B | tail: match i { EnvId(i) => ... }
compiler/Ast_typecheck.fx:3448 | fold | 1 | B | tail: import_entries(env, m_idx, op_name, entries, loc)
compiler/Ast_typecheck.fx:3454 | fold | N | D | tail: match e { DirImport => (fold ..., mlist) | DirImportFrom => (env, mlist) | ... }  (nested fold)
compiler/Ast_typecheck.fx:3457 | fold | 1 | B | tail: try { import_mod(env, alias, m, true, eloc) } catch { ... env }
compiler/Ast_typecheck.fx:3471 | valfold | 1 | B | tail: try { ... import_entries(env, m, k, entries, eloc) } catch { ... env }
compiler/Ast_typecheck.fx:3501 | fold | 1 | B | tail: match e { DefTyp(dt) => ... | ... }
compiler/Ast_typecheck.fx:3577 | fold | 1 | B | tail: match e { DefTyp dt => ... | DefVariant dvar => ... | DefInterface di => ... | _ => env }
compiler/Ast_typecheck.fx:3581 | valfold | 1 | B | tail: add_typ_to_env(t_arg, TypApp([], t_arg), env1)
compiler/Ast_typecheck.fx:3591 | fold | 1 | B | tail: add_id_to_env_check(n, ctor_name, env, check_for_duplicate_fun(t, env, sc, dvar_loc))
compiler/Ast_typecheck.fx:3616 | valfold | N | C | tail: (env1, (f, t, flags) :: base_members)
compiler/Ast_typecheck.fx:3622 | fold | N | C | tail: (env1, (f1, t, flags) :: all_members)
compiler/Ast_typecheck.fx:3695 | valfold | N | C | tail: (arg1 :: args1, t :: argtyps1, temp_env, idset1, templ_args1, all_typed & typed)
compiler/Ast_typecheck.fx:4023 | fold | N | C | tail: (df_inst_arg :: df_inst_args, inst_env, tmp_idset)
compiler/Ast_typecheck.fx:4170 | valfold | 1 | B | tail: match t { TypVoid => complex_cases | TypRecord => (..)::complex_cases | _ => (..)::complex_cases }
compiler/Ast_typecheck.fx:4176 | valfold | N | C | tail: ((rn, PatIdent(ai, body_loc)) :: al, (rn, PatIdent(bi, body_loc)) :: bl, cmp_code)
compiler/Ast_typecheck.fx:4197 | valfold | N | C | tail: (PatIdent(ai, body_loc) :: al, PatIdent(bi, body_loc) :: bl, cmp_code)
compiler/Ast_typecheck.fx:4257 | valfold | 1 | B | tail: add_typ_to_env(tn, TypApp([], tn), env)

### compiler/Parser.fx
compiler/Parser.fx:36 | fold | N | D | tail: (outer) fold (bd, b) = (thresh+1, "") for d <- inc_dirs { fold (bd,b)=(bd,b) for path <- glob ... }
compiler/Parser.fx:37 | fold | N | D | tail: if dist < bd || (dist == bd && nm < b) { (dist, nm) } else { (bd, b) }
compiler/Parser.fx:130 | valfold | N | C | tail: (p_ :: plist, e_ :: elist)
compiler/Parser.fx:223 | valfold | 1 | B | tail: ExpFor(pe_l, idxp, for_exp, flags.{for_flag_fold=true}, loc)
compiler/Parser.fx:228 | valfold | 1 | B | tail: (pe_l, idxp) :: nd_map
compiler/Parser.fx:366 | fold | 1 | B | tail: ExpBinary(OpCons, e, mklist_e, make_new_ctx(get_exp_loc(e)))
compiler/Parser.fx:676 | fold | N | C | tail: (result, next_e, code)
compiler/Parser.fx:1023 | valfold | N | C | tail: (glob_el, (for_cl_.rev(), idx_pat, when_e, loc) :: nested_fors)
compiler/Parser.fx:1024 | valfold | N | D | tail: match (idxp, idx_pat) { (PatAny,_) => (..) | (_,PatAny) => (..) | _ => throw }
compiler/Parser.fx:1052 | valfold | N | C | tail: ((pe_l, idxp) :: pel_i_l, glob_when_e, loc)
compiler/Parser.fx:1083 | fold | 1 | B | tail: ExpFor(pe_l, idxp, body, flags, loc)
compiler/Parser.fx:1496 | valfold | 1 | A | tail: stores + store  (own new make_tuple_assign helper)
compiler/Parser.fx:1673 | fold | 1 | B | tail: PatCons(p, tail, loclist2loc([::get_pat_loc(p), get_pat_loc(tail)], noloc))

---

## Migrator watch-notes

- **Nested folds** (a `fold` whose body/init contains another `fold`):
  K_inline.fx:133, C_gen_code.fx:2164, Ast_typecheck.fx:3454, Parser.fx:36→37.
  The migrator must rewrite the inner site independently (span-based, innermost
  first) — no site is only correct in combination with its parent.
- **C with cross-referencing components** — a tuple-ctor tail where component
  `e_i` reads a DIFFERENT accumulator — must NOT become sequential per-component
  stores; emit the simultaneous tuple assignment. Candidates to check by eye:
  C_gen_code.fx:2830 `(k+1, ...)`, :3684, Ast_typecheck.fx:2717/2848. Most C
  sites are cross-ref-free (`(x :: own_acc, other_acc)` shape).
- **`match`-arm fold bodies** (`fold (...) = (...) for x <- xs { | pat => ... }`)
  at K_normalize.fx:167/177: the whole match is the tail; migrate as
  `(a,…) = match x { | pat => (…) }`.
- **`op=` beautification is safe only when `s` is the LEFT operand.** Flagged
  right-operand sites that stay `s = E`: List.fx:70 `l + s`,
  NN/FromOnnx.fx:308 `rev_more_ops + prog`.
- **`::` cons tails are ordinary B** (`s = x :: s`), NOT the future writer
  accumulator sugar (`s += x`); the latter is a separate wave.
