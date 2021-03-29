MKDIR   := mkdir -p
RMDIR   := rm -rf
RM      := rm -rf
CC      := cc -Wno-unknown-warning-option -Wno-dangling-else -O3
INCLUDE := ./runtime
RUNTIME_DIR := ./runtime/ficus
RUNTIME_DEPS := $(wildcard $(RUNTIME_DIR)/*.h)
RUNTIME_IMPL_DEPS := $(wildcard $(RUNTIME_DIR)/impl/*.h)
BUILD_DIR := __build__
BOOTSTRAP_BUILD_DIR := $(BUILD_DIR)/bootstrap
BOOTSTRAP_SRC := ./compiler/bootstrap
BOOTSTRAP_SRCS := $(wildcard $(BOOTSTRAP_SRC)/*.c)
BOOTSTRAP_OBJS := $(patsubst $(BOOTSTRAP_SRC)/%.c,$(BOOTSTRAP_BUILD_DIR)/%.o,$(BOOTSTRAP_SRCS))
FICUS0  := $(BOOTSTRAP_BUILD_DIR)/ficus0
CFLAGS  := -I$(INCLUDE)
LDLIBS  := -lm

FICUS_SRC := ./compiler
FICUS_SRCS := $(wildcard $(FICUS_SRC)/*.fx)
FICUS_STDLIB := ./lib
FICUS_STDLIB_SRCS := $(wildcard $(FICUS_STDLIB)/*.fx)
FICUS := ./ficus
FICUS_FLAGS := -verbose -I $(FICUS_STDLIB) -O3 -cflags $(CFLAGS)

.PHONY: all clean make_dirs pre_ficus final_note

all: make_dirs $(FICUS0) pre_ficus $(FICUS) final_note

$(FICUS0): $(BOOTSTRAP_OBJS)
	@$(CC) $(LDFLAGS) $^ -o $@ $(LDLIBS)

$(BOOTSTRAP_BUILD_DIR)/ficus.o: $(BOOTSTRAP_SRC)/ficus.c $(RUNTIME_DEPS) $(RUNTIME_IMPL_DEPS)
	@echo "CC ficus.c"
	@$(CC) $(CFLAGS) -c $< -o $@

$(BOOTSTRAP_BUILD_DIR)/%.o: $(BOOTSTRAP_SRC)/%.c
	@echo "CC $<"
	@$(CC) $(CFLAGS) -c $< -o $@

$(FICUS): $(FICUS_SRCS) $(FICUS_STDLIB_SRCS)
	@$(RM) __build__/ficus/ficus
	@$(FICUS0) $(FICUS_FLAGS) -o $(FICUS) $(FICUS_SRC)/ficus.fx

make_dirs:
	@echo "\033[34;1mBuilding reference compiler from the pre-generated .c sources\033[0m"
	@$(MKDIR) $(BUILD_DIR)
	@$(MKDIR) $(BOOTSTRAP_BUILD_DIR)

pre_ficus:
	@echo "\033[34;1mBuilding fresh compiler from the actual ficus sources\033[0m"

final_note:
	@echo ""
	@echo "\033[32;1m./ficus has been built successfully.\033[0m"
	@echo "Now setup 'FICUS_PATH=<path_to_stdlib>' (lib subdirectory of the root directory),"
	@echo "'FICUS_CFLAGS=-I<path_to_runtime>' (runtime subdirectory of the root directory)"
	@echo "And optionally 'FICUS_LINKED_LIBRARIES=...'."
	@echo "It's recommended to build ficus with -lmimalloc or another efficient memory allocator."
	@echo "Then you can copy ficus to whatever directory you want and use it."
	@echo "When compiling an application, ficus creates __build__/<appname> subdirectory"
	@echo "in the current directory for the intermediate .c and .o files,"
	@echo "as well as the produced application".

clean:
	@$(RMDIR) $(BUILD_DIR)
	@$(RM) $(FICUS)
