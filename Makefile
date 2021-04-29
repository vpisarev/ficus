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

OBJS = $(O)/Array.obj $(O)/Ast.obj $(O)/Ast_pp.obj $(O)/Ast_typecheck.obj \
	$(O)/Builtins.obj $(O)/Char.obj $(O)/Compiler.obj $(O)/C_form.obj \
	$(O)/C_gen_code.obj $(O)/C_gen_fdecls.obj $(O)/C_gen_std.obj \
	$(O)/C_gen_types.obj $(O)/C_post_adjust_decls.obj \
	$(O)/C_post_rename_locals.obj $(O)/C_pp.obj $(O)/Dynvec.obj \
	$(O)/File.obj $(O)/Filename.obj $(O)/fx.obj $(O)/Hashmap.obj \
	$(O)/Hashset.obj $(O)/K_annotate.obj $(O)/K_cfold_dealias.obj \
	$(O)/K_copy_n_skip.obj $(O)/K_fast_idx.obj $(O)/K_flatten.obj \
	$(O)/K_form.obj $(O)/K_fuse_loops.obj $(O)/K_inline.obj \
	$(O)/K_lift.obj $(O)/K_lift_simple.obj $(O)/K_loop_inv.obj $(O)/K_mangle.obj \
	$(O)/K_normalize.obj $(O)/K_pp.obj $(O)/K_remove_unused.obj $(O)/K_tailrec.obj \
	$(O)/Lexer.obj $(O)/LexerUtils.obj $(O)/List.obj $(O)/Map.obj $(O)/Math.obj \
	$(O)/Options.obj $(O)/Parser.obj $(O)/PP.obj $(O)/Set.obj $(O)/String.obj \
	$(O)/Sys.obj $(O)/Vector.obj

RUNTIME_API = $(R)/ficus.h $(R)/version.h
RUNTIME_IMPL = $(R)/impl/ficus.impl.h $(R)/impl/array.impl.h \
		$(R)/impl/file.impl.h $(R)/impl/regex.impl.h $(R)/impl/regex_parse.c \
		$(R)/impl/rpmalloc.c $(R)/impl/rpmalloc.h $(R)/impl/rrbvec.impl.h \
		$(R)/impl/string.impl.h $(R)/impl/system.impl.h $(R)/impl/_fx_unicode_data.gen.h

SRC1 = $(L)/Array.fx $(S)/Ast.fx $(S)/Ast_pp.fx $(S)/Ast_typecheck.fx \
    $(L)/Builtins.fx $(L)/Char.fx $(S)/Compiler.fx $(S)/C_form.fx \
	$(S)/C_gen_code.fx $(S)/C_gen_fdecls.fx $(S)/C_gen_std.fx \
	$(S)/C_gen_types.fx $(S)/C_post_adjust_decls.fx \
	$(S)/C_post_rename_locals.fx $(S)/C_pp.fx $(L)/Dynvec.fx \
	$(L)/File.fx $(L)/Filename.fx $(S)/fx.fx $(L)/Hashmap.fx \
	$(L)/Hashset.fx $(S)/K_annotate.fx $(S)/K_cfold_dealias.fx \
	$(S)/K_copy_n_skip.fx $(S)/K_fast_idx.fx $(S)/K_flatten.fx \
	$(S)/K_form.fx $(S)/K_fuse_loops.fx $(S)/K_inline.fx $(S)/K_lift.fx \
	$(S)/K_lift_simple.fx $(S)/K_loop_inv.fx $(S)/K_mangle.fx \
	$(S)/K_normalize.fx $(S)/K_pp.fx $(S)/K_remove_unused.fx $(S)/K_tailrec.fx \
	$(L)/LexerUtils.fx $(S)/Lexer.fx $(L)/List.fx $(L)/Map.fx $(L)/Math.fx \
	$(S)/Options.fx $(S)/Parser.fx $(L)/PP.fx $(L)/Set.fx $(L)/String.fx \
	$(L)/Sys.fx $(L)/Vector.fx

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

$(FICUS): $(FICUS0) $(SRC1)
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
