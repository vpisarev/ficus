EXTRA_INCLUDE = ./runtime
BUILD_DIR = ./__fxbuild__
BOOTSTRAP_BUILD_DIR = $(BUILD_DIR)/bootstrap
BOOTSTRAP_SRC = ./compiler/bootstrap
FICUS_SRC = ./compiler
RUNTIME_DIR = ./runtime/ficus
FICUS0 = $(BOOTSTRAP_BUILD_DIR)/ficus0.exe
FICUS_BIN = ./bin
FICUS = $(FICUS_BIN)/ficus.exe

CL = cl /nologo /MT /Ob2 /D WIN32 /D _WIN32 /I$(EXTRA_INCLUDE)
LDLIBS = kernel32.lib advapi32.lib

FICUS_FLAGS = -verbose -O3

all: create_dirs $(FICUS0) $(FICUS) final_note

clean:
	@if exist "$(BOOTSTRAP_BUILD_DIR)/" del /S /Q "$(BOOTSTRAP_BUILD_DIR)"
	@if exist "$(BUILD_DIR)/ficus/" del /S /Q "$(BUILD_DIR)/ficus"
	@if exist "$(FICUS)" del /S /Q "$(FICUS)"

create_dirs:
	@if not exist "$(BUILD_DIR)/" mkdir "$(BUILD_DIR)"
	@if not exist "$(BOOTSTRAP_BUILD_DIR)/" mkdir "$(BOOTSTRAP_BUILD_DIR)"
	@if not exist "$(FICUS_BIN)/" mkdir "$(FICUS_BIN)"

$(FICUS0): $(BOOTSTRAP_SRC)/*.c
	@SET OBJS=$(**:.c=.obj)%
	@nmake $(BOOTSTRAP_BUILD_DIR)/libficus.obj %OBJS:$(BOOTSTRAP_SRC)=$(BOOTSTRAP_BUILD_DIR)%%
	$(CL) $(LDFLAGS) /F10485760 /Fe$(FICUS0) $(LDLIBS) %OBJS:$(BOOTSTRAP_SRC)=$(BOOTSTRAP_BUILD_DIR)% $(BOOTSTRAP_BUILD_DIR)/libficus.obj

{$(BOOTSTRAP_SRC)}.c{$(BOOTSTRAP_BUILD_DIR)}.obj:
	@$(CL) $(CFLAGS) /c /Fo$@ $<

{$(RUNTIME_DIR)/impl}.c{$(BOOTSTRAP_BUILD_DIR)}.obj:
	@$(CL) $(CFLAGS) /c /Fo$@ $<

$(FICUS): $(FICUS0) $(SRC1)
	@$(FICUS0:/=\) $(FICUS_FLAGS) -o $(FICUS_BIN)/ficus $(FICUS_SRC)/fx.fx

final_note:
	@echo ""
	@echo "./bin/ficus was built successfully."
	@echo "Now setup 'FICUS_PATH=<path_to_stdlib>' (lib subdirectory of the root directory)"
	@echo "and optionally 'FICUS_CFLAGS' and 'FICUS_LINK_LIBRARIES' to pass to C compiler."
	@echo "After that you can copy ficus to whatever directory you want and use it."
	@echo "When compiling an application, ficus creates __fxbuild__/<appname> subdirectory"
	@echo "in the current directory for the intermediate .obj and .o files,"
	@echo "as well as the produced application".
