
// this is autogenerated file, do not edit it.
#include "ficus/ficus.h"

typedef struct _fx_LS_data_t {
   int_ rc;
   struct _fx_LS_data_t* tl;
   fx_str_t hd;
} _fx_LS_data_t, *_fx_LS;

typedef struct _fx_N17Options__optval_t {
   int tag;
   union {
      bool OptBool;
      int_ OptInt;
      fx_str_t OptString;
   } u;
} _fx_N17Options__optval_t;

typedef struct _fx_T2SN17Options__optval_t {
   fx_str_t t0;
   struct _fx_N17Options__optval_t t1;
} _fx_T2SN17Options__optval_t;

typedef struct _fx_LT2SN17Options__optval_t_data_t {
   int_ rc;
   struct _fx_LT2SN17Options__optval_t_data_t* tl;
   struct _fx_T2SN17Options__optval_t hd;
} _fx_LT2SN17Options__optval_t_data_t, *_fx_LT2SN17Options__optval_t;

typedef struct _fx_R18Options__options_t {
   struct _fx_LS_data_t* app_args;
   fx_str_t app_filename;
   bool arch64;
   bool force_rebuild;
   fx_str_t build_dir;
   fx_str_t build_rootdir;
   fx_str_t cflags;
   fx_str_t clibs;
   bool compile_by_cpp;
   fx_str_t filename;
   bool gen_c;
   struct _fx_LS_data_t* include_path;
   bool debug;
   struct _fx_LT2SN17Options__optval_t_data_t* defines;
   int_ optim_iters;
   int_ inline_thresh;
   bool enable_openmp;
   bool relax;
   bool use_preamble;
   bool make_app;
   int_ optimize_level;
   fx_str_t output_name;
   bool print_ast0;
   bool print_ast;
   bool print_k0;
   bool print_k;
   bool print_tokens;
   bool run_app;
   bool verbose;
   bool W_unused;
} _fx_R18Options__options_t;

typedef struct _fx_Ta2i {
   int_ t0;
   int_ t1;
} _fx_Ta2i;

typedef struct _fx_T2Ta2iS {
   struct _fx_Ta2i t0;
   fx_str_t t1;
} _fx_T2Ta2iS;

typedef struct _fx_R10Ast__loc_t {
   int_ m_idx;
   int_ line0;
   int_ col0;
   int_ line1;
   int_ col1;
} _fx_R10Ast__loc_t;

typedef struct _fx_T2R10Ast__loc_tS {
   struct _fx_R10Ast__loc_t t0;
   fx_str_t t1;
} _fx_T2R10Ast__loc_tS;

typedef struct {
   int_ rc;
   int_ data;
} _fx_E4Exit_data_t;

typedef struct {
   int_ rc;
   fx_str_t data;
} _fx_E4Fail_data_t;

typedef struct {
   int_ rc;
   struct _fx_T2Ta2iS data;
} _fx_E22LexerUtils__LexerError_data_t;

typedef struct {
   int_ rc;
   struct _fx_T2R10Ast__loc_tS data;
} _fx_E17Ast__CompileError_data_t;

typedef struct {
   int_ rc;
   struct _fx_T2R10Ast__loc_tS data;
} _fx_E18Parser__ParseError_data_t;

static void _fx_free_LS(struct _fx_LS_data_t** dst)
{
   FX_FREE_LIST_IMPL(_fx_LS, fx_free_str);
}

static int _fx_cons_LS(fx_str_t* hd, struct _fx_LS_data_t* tl, bool addref_tl, struct _fx_LS_data_t** fx_result)
{
   FX_MAKE_LIST_IMPL(_fx_LS, fx_copy_str);
}

static void _fx_free_N17Options__optval_t(struct _fx_N17Options__optval_t* dst)
{
   switch (dst->tag) {
   case 3:
      fx_free_str(&dst->u.OptString); break;
   default:
      ;
   }
   dst->tag = 0;
}

static void _fx_copy_N17Options__optval_t(struct _fx_N17Options__optval_t* src, struct _fx_N17Options__optval_t* dst)
{
   dst->tag = src->tag;
   switch (src->tag) {
   case 3:
      fx_copy_str(&src->u.OptString, &dst->u.OptString); break;
   default:
      dst->u = src->u;
   }
}

static void _fx_free_T2SN17Options__optval_t(struct _fx_T2SN17Options__optval_t* dst)
{
   fx_free_str(&dst->t0);
   _fx_free_N17Options__optval_t(&dst->t1);
}

static void _fx_copy_T2SN17Options__optval_t(struct _fx_T2SN17Options__optval_t* src, struct _fx_T2SN17Options__optval_t* dst)
{
   fx_copy_str(&src->t0, &dst->t0);
   _fx_copy_N17Options__optval_t(&src->t1, &dst->t1);
}

static void _fx_make_T2SN17Options__optval_t(
   fx_str_t* t0,
   struct _fx_N17Options__optval_t* t1,
   struct _fx_T2SN17Options__optval_t* fx_result)
{
   fx_copy_str(t0, &fx_result->t0);
   _fx_copy_N17Options__optval_t(t1, &fx_result->t1);
}

static void _fx_free_LT2SN17Options__optval_t(struct _fx_LT2SN17Options__optval_t_data_t** dst)
{
   FX_FREE_LIST_IMPL(_fx_LT2SN17Options__optval_t, _fx_free_T2SN17Options__optval_t);
}

static int _fx_cons_LT2SN17Options__optval_t(
   struct _fx_T2SN17Options__optval_t* hd,
   struct _fx_LT2SN17Options__optval_t_data_t* tl,
   bool addref_tl,
   struct _fx_LT2SN17Options__optval_t_data_t** fx_result)
{
   FX_MAKE_LIST_IMPL(_fx_LT2SN17Options__optval_t, _fx_copy_T2SN17Options__optval_t);
}

static void _fx_free_R18Options__options_t(struct _fx_R18Options__options_t* dst)
{
   _fx_free_LS(&dst->app_args);
   fx_free_str(&dst->app_filename);
   fx_free_str(&dst->build_dir);
   fx_free_str(&dst->build_rootdir);
   fx_free_str(&dst->cflags);
   fx_free_str(&dst->clibs);
   fx_free_str(&dst->filename);
   _fx_free_LS(&dst->include_path);
   _fx_free_LT2SN17Options__optval_t(&dst->defines);
   fx_free_str(&dst->output_name);
}

static void _fx_copy_R18Options__options_t(struct _fx_R18Options__options_t* src, struct _fx_R18Options__options_t* dst)
{
   FX_COPY_PTR(src->app_args, &dst->app_args);
   fx_copy_str(&src->app_filename, &dst->app_filename);
   dst->arch64 = src->arch64;
   dst->force_rebuild = src->force_rebuild;
   fx_copy_str(&src->build_dir, &dst->build_dir);
   fx_copy_str(&src->build_rootdir, &dst->build_rootdir);
   fx_copy_str(&src->cflags, &dst->cflags);
   fx_copy_str(&src->clibs, &dst->clibs);
   dst->compile_by_cpp = src->compile_by_cpp;
   fx_copy_str(&src->filename, &dst->filename);
   dst->gen_c = src->gen_c;
   FX_COPY_PTR(src->include_path, &dst->include_path);
   dst->debug = src->debug;
   FX_COPY_PTR(src->defines, &dst->defines);
   dst->optim_iters = src->optim_iters;
   dst->inline_thresh = src->inline_thresh;
   dst->enable_openmp = src->enable_openmp;
   dst->relax = src->relax;
   dst->use_preamble = src->use_preamble;
   dst->make_app = src->make_app;
   dst->optimize_level = src->optimize_level;
   fx_copy_str(&src->output_name, &dst->output_name);
   dst->print_ast0 = src->print_ast0;
   dst->print_ast = src->print_ast;
   dst->print_k0 = src->print_k0;
   dst->print_k = src->print_k;
   dst->print_tokens = src->print_tokens;
   dst->run_app = src->run_app;
   dst->verbose = src->verbose;
   dst->W_unused = src->W_unused;
}

static void _fx_make_R18Options__options_t(
   struct _fx_LS_data_t* r_app_args,
   fx_str_t* r_app_filename,
   bool r_arch64,
   bool r_force_rebuild,
   fx_str_t* r_build_dir,
   fx_str_t* r_build_rootdir,
   fx_str_t* r_cflags,
   fx_str_t* r_clibs,
   bool r_compile_by_cpp,
   fx_str_t* r_filename,
   bool r_gen_c,
   struct _fx_LS_data_t* r_include_path,
   bool r_debug,
   struct _fx_LT2SN17Options__optval_t_data_t* r_defines,
   int_ r_optim_iters,
   int_ r_inline_thresh,
   bool r_enable_openmp,
   bool r_relax,
   bool r_use_preamble,
   bool r_make_app,
   int_ r_optimize_level,
   fx_str_t* r_output_name,
   bool r_print_ast0,
   bool r_print_ast,
   bool r_print_k0,
   bool r_print_k,
   bool r_print_tokens,
   bool r_run_app,
   bool r_verbose,
   bool r_W_unused,
   struct _fx_R18Options__options_t* fx_result)
{
   FX_COPY_PTR(r_app_args, &fx_result->app_args);
   fx_copy_str(r_app_filename, &fx_result->app_filename);
   fx_result->arch64 = r_arch64;
   fx_result->force_rebuild = r_force_rebuild;
   fx_copy_str(r_build_dir, &fx_result->build_dir);
   fx_copy_str(r_build_rootdir, &fx_result->build_rootdir);
   fx_copy_str(r_cflags, &fx_result->cflags);
   fx_copy_str(r_clibs, &fx_result->clibs);
   fx_result->compile_by_cpp = r_compile_by_cpp;
   fx_copy_str(r_filename, &fx_result->filename);
   fx_result->gen_c = r_gen_c;
   FX_COPY_PTR(r_include_path, &fx_result->include_path);
   fx_result->debug = r_debug;
   FX_COPY_PTR(r_defines, &fx_result->defines);
   fx_result->optim_iters = r_optim_iters;
   fx_result->inline_thresh = r_inline_thresh;
   fx_result->enable_openmp = r_enable_openmp;
   fx_result->relax = r_relax;
   fx_result->use_preamble = r_use_preamble;
   fx_result->make_app = r_make_app;
   fx_result->optimize_level = r_optimize_level;
   fx_copy_str(r_output_name, &fx_result->output_name);
   fx_result->print_ast0 = r_print_ast0;
   fx_result->print_ast = r_print_ast;
   fx_result->print_k0 = r_print_k0;
   fx_result->print_k = r_print_k;
   fx_result->print_tokens = r_print_tokens;
   fx_result->run_app = r_run_app;
   fx_result->verbose = r_verbose;
   fx_result->W_unused = r_W_unused;
}

static void _fx_free_T2Ta2iS(struct _fx_T2Ta2iS* dst)
{
   fx_free_str(&dst->t1);
}

static void _fx_copy_T2Ta2iS(struct _fx_T2Ta2iS* src, struct _fx_T2Ta2iS* dst)
{
   dst->t0 = src->t0;
   fx_copy_str(&src->t1, &dst->t1);
}

static void _fx_make_T2Ta2iS(struct _fx_Ta2i* t0, fx_str_t* t1, struct _fx_T2Ta2iS* fx_result)
{
   fx_result->t0 = *t0;
   fx_copy_str(t1, &fx_result->t1);
}

static void _fx_free_T2R10Ast__loc_tS(struct _fx_T2R10Ast__loc_tS* dst)
{
   fx_free_str(&dst->t1);
}

static void _fx_copy_T2R10Ast__loc_tS(struct _fx_T2R10Ast__loc_tS* src, struct _fx_T2R10Ast__loc_tS* dst)
{
   dst->t0 = src->t0;
   fx_copy_str(&src->t1, &dst->t1);
}

static void _fx_make_T2R10Ast__loc_tS(struct _fx_R10Ast__loc_t* t0, fx_str_t* t1, struct _fx_T2R10Ast__loc_tS* fx_result)
{
   fx_result->t0 = *t0;
   fx_copy_str(t1, &fx_result->t1);
}

bool _fx_g6fx__ok;
bool _fx_g8fx__ok1_;
FX_EXTERN_C int _fx_M7OptionsFM13parse_optionsB0(bool* fx_result, void* fx_fv);

FX_EXTERN_C_VAL(struct _fx_R18Options__options_t _fx_g12Options__opt)
FX_EXTERN_C int _fx_M8CompilerFM11process_allB1S(fx_str_t* fname0_0, bool* fx_result, void* fx_fv);

FX_EXTERN_C int _fx_F9make_ExitE1i(int_ arg0, fx_exn_t* fx_result);

FX_EXTERN_C int fx_init_fx(void)
{
   fx_str_t v_0 = {0};
   fx_exn_t v_1 = {0};
   int fx_status = 0;
   FX_CALL(_fx_M7OptionsFM13parse_optionsB0(&_fx_g6fx__ok, 0), _fx_cleanup);
   if (_fx_g6fx__ok) {
      fx_copy_str(&_fx_g12Options__opt.filename, &v_0);
      FX_CALL(_fx_M8CompilerFM11process_allB1S(&v_0, &_fx_g8fx__ok1_, 0), _fx_cleanup);
   }
   else {
      _fx_g8fx__ok1_ = false;
   }
   if (!_fx_g8fx__ok1_) {
      FX_CALL(_fx_F9make_ExitE1i(1, &v_1), _fx_cleanup); FX_THROW(&v_1, true, _fx_cleanup);
   }

_fx_cleanup: ;
   FX_FREE_STR(&v_0);
   fx_free_exn(&v_1);
   return fx_status;
}

FX_EXTERN_C void fx_deinit_fx(void)
{

}


FX_EXTERN_C int fx_init_Builtins();
FX_EXTERN_C void fx_deinit_Builtins();
FX_EXTERN_C int fx_init_Math();
FX_EXTERN_C void fx_deinit_Math();
FX_EXTERN_C int fx_init_Complex();
FX_EXTERN_C void fx_deinit_Complex();
FX_EXTERN_C int fx_init_Array();
FX_EXTERN_C void fx_deinit_Array();
FX_EXTERN_C int fx_init_List();
FX_EXTERN_C void fx_deinit_List();
FX_EXTERN_C int fx_init_Vector();
FX_EXTERN_C void fx_deinit_Vector();
FX_EXTERN_C int fx_init_Char();
FX_EXTERN_C void fx_deinit_Char();
FX_EXTERN_C int fx_init_String();
FX_EXTERN_C void fx_deinit_String();
FX_EXTERN_C int fx_init_Filename();
FX_EXTERN_C void fx_deinit_Filename();
FX_EXTERN_C int fx_init_File();
FX_EXTERN_C void fx_deinit_File();
FX_EXTERN_C int fx_init_Sys();
FX_EXTERN_C void fx_deinit_Sys();
FX_EXTERN_C int fx_init_Options();
FX_EXTERN_C void fx_deinit_Options();
FX_EXTERN_C int fx_init_Hashmap();
FX_EXTERN_C void fx_deinit_Hashmap();
FX_EXTERN_C int fx_init_Hashset();
FX_EXTERN_C void fx_deinit_Hashset();
FX_EXTERN_C int fx_init_LexerUtils();
FX_EXTERN_C void fx_deinit_LexerUtils();
FX_EXTERN_C int fx_init_Dynvec();
FX_EXTERN_C void fx_deinit_Dynvec();
FX_EXTERN_C int fx_init_Map();
FX_EXTERN_C void fx_deinit_Map();
FX_EXTERN_C int fx_init_Set();
FX_EXTERN_C void fx_deinit_Set();
FX_EXTERN_C int fx_init_Ast();
FX_EXTERN_C void fx_deinit_Ast();
FX_EXTERN_C int fx_init_PP();
FX_EXTERN_C void fx_deinit_PP();
FX_EXTERN_C int fx_init_Ast_pp();
FX_EXTERN_C void fx_deinit_Ast_pp();
FX_EXTERN_C int fx_init_Lexer();
FX_EXTERN_C void fx_deinit_Lexer();
FX_EXTERN_C int fx_init_Parser();
FX_EXTERN_C void fx_deinit_Parser();
FX_EXTERN_C int fx_init_Ast_typecheck();
FX_EXTERN_C void fx_deinit_Ast_typecheck();
FX_EXTERN_C int fx_init_K_form();
FX_EXTERN_C void fx_deinit_K_form();
FX_EXTERN_C int fx_init_K_pp();
FX_EXTERN_C void fx_deinit_K_pp();
FX_EXTERN_C int fx_init_K_normalize();
FX_EXTERN_C void fx_deinit_K_normalize();
FX_EXTERN_C int fx_init_K_annotate();
FX_EXTERN_C void fx_deinit_K_annotate();
FX_EXTERN_C int fx_init_K_mangle();
FX_EXTERN_C void fx_deinit_K_mangle();
FX_EXTERN_C int fx_init_K_remove_unused();
FX_EXTERN_C void fx_deinit_K_remove_unused();
FX_EXTERN_C int fx_init_K_lift_simple();
FX_EXTERN_C void fx_deinit_K_lift_simple();
FX_EXTERN_C int fx_init_K_flatten();
FX_EXTERN_C void fx_deinit_K_flatten();
FX_EXTERN_C int fx_init_K_tailrec();
FX_EXTERN_C void fx_deinit_K_tailrec();
FX_EXTERN_C int fx_init_K_inline();
FX_EXTERN_C void fx_deinit_K_inline();
FX_EXTERN_C int fx_init_K_copy_n_skip();
FX_EXTERN_C void fx_deinit_K_copy_n_skip();
FX_EXTERN_C int fx_init_K_cfold_dealias();
FX_EXTERN_C void fx_deinit_K_cfold_dealias();
FX_EXTERN_C int fx_init_K_fast_idx();
FX_EXTERN_C void fx_deinit_K_fast_idx();
FX_EXTERN_C int fx_init_K_loop_inv();
FX_EXTERN_C void fx_deinit_K_loop_inv();
FX_EXTERN_C int fx_init_K_fuse_loops();
FX_EXTERN_C void fx_deinit_K_fuse_loops();
FX_EXTERN_C int fx_init_K_optim_matop();
FX_EXTERN_C void fx_deinit_K_optim_matop();
FX_EXTERN_C int fx_init_K_nothrow_wrappers();
FX_EXTERN_C void fx_deinit_K_nothrow_wrappers();
FX_EXTERN_C int fx_init_K_freevars();
FX_EXTERN_C void fx_deinit_K_freevars();
FX_EXTERN_C int fx_init_K_declosure();
FX_EXTERN_C void fx_deinit_K_declosure();
FX_EXTERN_C int fx_init_K_lift();
FX_EXTERN_C void fx_deinit_K_lift();
FX_EXTERN_C int fx_init_C_form();
FX_EXTERN_C void fx_deinit_C_form();
FX_EXTERN_C int fx_init_C_gen_std();
FX_EXTERN_C void fx_deinit_C_gen_std();
FX_EXTERN_C int fx_init_C_gen_types();
FX_EXTERN_C void fx_deinit_C_gen_types();
FX_EXTERN_C int fx_init_C_gen_fdecls();
FX_EXTERN_C void fx_deinit_C_gen_fdecls();
FX_EXTERN_C int fx_init_C_pp();
FX_EXTERN_C void fx_deinit_C_pp();
FX_EXTERN_C int fx_init_C_gen_code();
FX_EXTERN_C void fx_deinit_C_gen_code();
FX_EXTERN_C int fx_init_C_post_rename_locals();
FX_EXTERN_C void fx_deinit_C_post_rename_locals();
FX_EXTERN_C int fx_init_C_post_adjust_decls();
FX_EXTERN_C void fx_deinit_C_post_adjust_decls();
FX_EXTERN_C int fx_init_Compiler();
FX_EXTERN_C void fx_deinit_Compiler();

int main(int argc, char** argv)
{
   fx_init(argc, argv);
   int fx_status = FX_OK;
  if (fx_status >= 0) fx_status = fx_init_Builtins();
  if (fx_status >= 0) fx_status = fx_init_Math();
  if (fx_status >= 0) fx_status = fx_init_Complex();
  if (fx_status >= 0) fx_status = fx_init_Array();
  if (fx_status >= 0) fx_status = fx_init_List();
  if (fx_status >= 0) fx_status = fx_init_Vector();
  if (fx_status >= 0) fx_status = fx_init_Char();
  if (fx_status >= 0) fx_status = fx_init_String();
  if (fx_status >= 0) fx_status = fx_init_Filename();
  if (fx_status >= 0) fx_status = fx_init_File();
  if (fx_status >= 0) fx_status = fx_init_Sys();
  if (fx_status >= 0) fx_status = fx_init_Options();
  if (fx_status >= 0) fx_status = fx_init_Hashmap();
  if (fx_status >= 0) fx_status = fx_init_Hashset();
  if (fx_status >= 0) fx_status = fx_init_LexerUtils();
  if (fx_status >= 0) fx_status = fx_init_Dynvec();
  if (fx_status >= 0) fx_status = fx_init_Map();
  if (fx_status >= 0) fx_status = fx_init_Set();
  if (fx_status >= 0) fx_status = fx_init_Ast();
  if (fx_status >= 0) fx_status = fx_init_PP();
  if (fx_status >= 0) fx_status = fx_init_Ast_pp();
  if (fx_status >= 0) fx_status = fx_init_Lexer();
  if (fx_status >= 0) fx_status = fx_init_Parser();
  if (fx_status >= 0) fx_status = fx_init_Ast_typecheck();
  if (fx_status >= 0) fx_status = fx_init_K_form();
  if (fx_status >= 0) fx_status = fx_init_K_pp();
  if (fx_status >= 0) fx_status = fx_init_K_normalize();
  if (fx_status >= 0) fx_status = fx_init_K_annotate();
  if (fx_status >= 0) fx_status = fx_init_K_mangle();
  if (fx_status >= 0) fx_status = fx_init_K_remove_unused();
  if (fx_status >= 0) fx_status = fx_init_K_lift_simple();
  if (fx_status >= 0) fx_status = fx_init_K_flatten();
  if (fx_status >= 0) fx_status = fx_init_K_tailrec();
  if (fx_status >= 0) fx_status = fx_init_K_inline();
  if (fx_status >= 0) fx_status = fx_init_K_copy_n_skip();
  if (fx_status >= 0) fx_status = fx_init_K_cfold_dealias();
  if (fx_status >= 0) fx_status = fx_init_K_fast_idx();
  if (fx_status >= 0) fx_status = fx_init_K_loop_inv();
  if (fx_status >= 0) fx_status = fx_init_K_fuse_loops();
  if (fx_status >= 0) fx_status = fx_init_K_optim_matop();
  if (fx_status >= 0) fx_status = fx_init_K_nothrow_wrappers();
  if (fx_status >= 0) fx_status = fx_init_K_freevars();
  if (fx_status >= 0) fx_status = fx_init_K_declosure();
  if (fx_status >= 0) fx_status = fx_init_K_lift();
  if (fx_status >= 0) fx_status = fx_init_C_form();
  if (fx_status >= 0) fx_status = fx_init_C_gen_std();
  if (fx_status >= 0) fx_status = fx_init_C_gen_types();
  if (fx_status >= 0) fx_status = fx_init_C_gen_fdecls();
  if (fx_status >= 0) fx_status = fx_init_C_pp();
  if (fx_status >= 0) fx_status = fx_init_C_gen_code();
  if (fx_status >= 0) fx_status = fx_init_C_post_rename_locals();
  if (fx_status >= 0) fx_status = fx_init_C_post_adjust_decls();
  if (fx_status >= 0) fx_status = fx_init_Compiler();
  if (fx_status >= 0) fx_status = fx_init_fx();
  if (fx_status < 0) fx_status = fx_print_bt();
  fx_deinit_fx();
  fx_deinit_Compiler();
  fx_deinit_C_post_adjust_decls();
  fx_deinit_C_post_rename_locals();
  fx_deinit_C_gen_code();
  fx_deinit_C_pp();
  fx_deinit_C_gen_fdecls();
  fx_deinit_C_gen_types();
  fx_deinit_C_gen_std();
  fx_deinit_C_form();
  fx_deinit_K_lift();
  fx_deinit_K_declosure();
  fx_deinit_K_freevars();
  fx_deinit_K_nothrow_wrappers();
  fx_deinit_K_optim_matop();
  fx_deinit_K_fuse_loops();
  fx_deinit_K_loop_inv();
  fx_deinit_K_fast_idx();
  fx_deinit_K_cfold_dealias();
  fx_deinit_K_copy_n_skip();
  fx_deinit_K_inline();
  fx_deinit_K_tailrec();
  fx_deinit_K_flatten();
  fx_deinit_K_lift_simple();
  fx_deinit_K_remove_unused();
  fx_deinit_K_mangle();
  fx_deinit_K_annotate();
  fx_deinit_K_normalize();
  fx_deinit_K_pp();
  fx_deinit_K_form();
  fx_deinit_Ast_typecheck();
  fx_deinit_Parser();
  fx_deinit_Lexer();
  fx_deinit_Ast_pp();
  fx_deinit_PP();
  fx_deinit_Ast();
  fx_deinit_Set();
  fx_deinit_Map();
  fx_deinit_Dynvec();
  fx_deinit_LexerUtils();
  fx_deinit_Hashset();
  fx_deinit_Hashmap();
  fx_deinit_Options();
  fx_deinit_Sys();
  fx_deinit_File();
  fx_deinit_Filename();
  fx_deinit_String();
  fx_deinit_Char();
  fx_deinit_Vector();
  fx_deinit_List();
  fx_deinit_Array();
  fx_deinit_Complex();
  fx_deinit_Math();
  fx_deinit_Builtins();
  return fx_deinit(fx_status);
}

