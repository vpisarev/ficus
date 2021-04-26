EXTRA_INCLUDE = .\runtime
RUNTIME_DIR = .\runtime\ficus
BUILD_DIR = .\__fxbuild__
O = $(BUILD_DIR)\bootstrap
BS = .\compiler\bootstrap
S = .\compiler
R = .\runtime\ficus
L = .\lib
FICUS0 = $(O)\ficus0.exe
FICUS_BIN = .\bin
FICUS = $(FICUS_BIN)\ficus.exe

CL = cl /nologo /MT /Ob2 /D WIN32 /D _WIN32 /I$(EXTRA_INCLUDE)
LDLIBS = kernel32.lib advapi32.lib

OBJS = $(O)/Ast.obj $(O)/Ast_pp.obj $(O)/Ast_typecheck.obj $(O)/Builtins.obj \
	$(O)/Char.obj $(O)/Compiler.obj $(O)/C_form.obj $(O)/C_gen_code.obj \
	$(O)/C_gen_fdecls.obj $(O)/C_gen_std.obj $(O)/C_gen_types.obj \
	$(O)/C_post_adjust_decls.obj $(O)/C_post_rename_locals.obj \
	$(O)/C_pp.obj $(O)/Dynvec.obj $(O)/File.obj $(O)/Filename.obj \
	$(O)/fx.obj $(O)/Hashmap.obj $(O)/Hashset.obj $(O)/K_annotate.obj \
	$(O)/K_cfold_dealias.obj $(O)/K_copy_n_skip.obj $(O)/K_fast_idx.obj \
	$(O)/K_flatten.obj $(O)/K_form.obj $(O)/K_fuse_loops.obj $(O)/K_inline.obj \
	$(O)/K_lift.obj $(O)/K_lift_simple.obj $(O)/K_loop_inv.obj $(O)/K_mangle.obj \
	$(O)/K_normalize.obj $(O)/K_pp.obj $(O)/K_remove_unused.obj $(O)/K_tailrec.obj \
	$(O)/Lexer.obj $(O)/List.obj $(O)/Map.obj $(O)/Math.obj $(O)/Options.obj \
	$(O)/Parser.obj $(O)/PP.obj $(O)/Set.obj $(O)/String.obj $(O)/Sys.obj

RUNTIME_API = $(R)/ficus.h $(R)/version.h
RUNTIME_IMPL = $(R)/impl/ficus.impl.h $(R)/impl/array.impl.h \
		$(R)/impl/file.impl.h $(R)/impl/regex.impl.h $(R)/impl/regex_parse.c \
		$(R)/impl/rpmalloc.c $(R)/impl/rpmalloc.h $(R)/impl/rrbvec.impl.h \
		$(R)/impl/string.impl.h $(R)/impl/system.impl.h $(R)/impl/_fx_unicode_data.gen.h

SRC0 = $(BS)/Ast.c $(BS)/Ast_pp.c $(BS)/Ast_typecheck.c $(BS)/Builtins.c \
	$(BS)/Char.c $(BS)/Compiler.c $(BS)/C_form.c $(BS)/C_gen_code.c \
	$(BS)/C_gen_fdecls.c $(BS)/C_gen_std.c $(BS)/C_gen_types.c \
	$(BS)/C_post_adjust_decls.c $(BS)/C_post_rename_locals.c \
	$(BS)/C_pp.c $(BS)/Dynvec.c $(BS)/File.c $(BS)/Filename.c \
	$(BS)/fx.c $(BS)/Hashmap.c $(BS)/Hashset.c $(BS)/K_annotate.c \
	$(BS)/K_cfold_dealias.c $(BS)/K_copy_n_skip.c $(BS)/K_fast_idx.c \
	$(BS)/K_flatten.c $(BS)/K_form.c $(BS)/K_fuse_loops.c $(BS)/K_inline.c \
	$(BS)/K_lift.c $(BS)/K_lift_simple.c $(BS)/K_loop_inv.c $(BS)/K_mangle.c \
	$(BS)/K_normalize.c $(BS)/K_pp.c $(BS)/K_remove_unused.c $(BS)/K_tailrec.c \
	$(BS)/Lexer.c $(BS)/List.c $(BS)/Map.c $(BS)/Math.c $(BS)/Options.c \
	$(BS)/Parser.c $(BS)/PP.c $(BS)/Set.c $(BS)/String.c $(BS)/Sys.c

SRC1 = $(S)/Ast.c $(S)/Ast_pp.c $(S)/Ast_typecheck.c $(L)/Builtins.c \
	$(L)/Char.c $(S)/Compiler.c $(S)/C_form.c $(S)/C_gen_code.c \
	$(S)/C_gen_fdecls.c $(S)/C_gen_std.c $(S)/C_gen_types.c \
	$(S)/C_post_adjust_decls.c $(S)/C_post_rename_locals.c \
	$(S)/C_pp.c $(L)/Dynvec.c $(L)/File.c $(L)/Filename.c \
	$(S)/fx.c $(L)/Hashmap.c $(L)/Hashset.c $(S)/K_annotate.c \
	$(S)/K_cfold_dealias.c $(S)/K_copy_n_skip.c $(S)/K_fast_idx.c \
	$(S)/K_flatten.c $(S)/K_form.c $(S)/K_fuse_loops.c $(S)/K_inline.c \
	$(S)/K_lift.c $(S)/K_lift_simple.c $(S)/K_loop_inv.c $(S)/K_mangle.c \
	$(S)/K_normalize.c $(S)/K_pp.c $(S)/K_remove_unused.c $(S)/K_tailrec.c \
	$(S)/Lexer.c $(L)/List.c $(L)/Map.c $(L)/Math.c $(S)/Options.c \
	$(S)/Parser.c $(L)/PP.c $(L)/Set.c $(L)/String.c $(L)/Sys.c

FICUS_FLAGS = -verbose -O3

all: create_dirs $(FICUS0) $(FICUS) final_note

clean:
	@if exist "$(O)/" del /S /Q "$(O)"
	@if exist "$(BUILD_DIR)/ficus/" del /S /Q "$(BUILD_DIR)/ficus"
	@if exist "$(FICUS)" del /S /Q "$(FICUS)"

create_dirs:
	@if not exist "$(BUILD_DIR)/" mkdir "$(BUILD_DIR)"
	@if not exist "$(O)/" mkdir "$(O)"
	@if not exist "$(FICUS_BIN)/" mkdir "$(FICUS_BIN)"

$(FICUS0): $(OBJS)
	$(CL) $(LDFLAGS) /Fe$(FICUS0) $(OBJS) $(LDLIBS)

{$(BS)}.c{$(O)}.obj:
	@$(CL) $(CFLAGS) /c /Fo$@ $<

$(FICUS): $(FICUS0) $(SRCS1)
	@$(FICUS0) $(FICUS_FLAGS) -o $(FICUS_BIN)/ficus $(S)/fx.fx

final_note:
	@echo ""
	@echo "./bin/ficus was built successfully."
	@echo "Now setup 'FICUS_PATH=<path_to_stdlib>' (lib subdirectory of the root directory)"
	@echo "and optionally 'FICUS_CFLAGS' and 'FICUS_LINK_LIBRARIES' to pass to C compiler."
	@echo "After that you can copy ficus to whatever directory you want and use it."
	@echo "When compiling an application, ficus creates __fxbuild__/<appname> subdirectory"
	@echo "in the current directory for the intermediate .obj and .o files,"
	@echo "as well as the produced application".
