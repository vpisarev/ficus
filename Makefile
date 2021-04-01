MKDIR   := mkdir -p
RMDIR   := rm -rf
RM      := rm -rf
CC      := cc -Wno-unknown-warning-option -Wno-dangling-else -Wno-static-in-inline -O3
INCLUDE := ./runtime
RUNTIME_DIR := ./runtime/ficus
RUNTIME_DEPS := $(wildcard $(RUNTIME_DIR)/*.h)
RUNTIME_IMPL_DEPS := $(wildcard $(RUNTIME_DIR)/impl/*.h)
BUILD_DIR := __fxbuild__
BOOTSTRAP_BUILD_DIR := $(BUILD_DIR)/bootstrap
BOOTSTRAP_SRC := ./compiler/bootstrap
BOOTSTRAP_SRCS := $(wildcard $(BOOTSTRAP_SRC)/*.c)
BOOTSTRAP_OBJS := $(patsubst $(BOOTSTRAP_SRC)/%.c,$(BOOTSTRAP_BUILD_DIR)/%.o,$(BOOTSTRAP_SRCS))
FICUS0  := $(BOOTSTRAP_BUILD_DIR)/ficus0
CFLAGS  := -I$(INCLUDE)
LDLIBS  := -lm
LDFLAGS :=

FICUS_SRC := ./compiler
FICUS_SRCS := $(wildcard $(FICUS_SRC)/*.fx)
FICUS_STDLIB := ./lib
FICUS_STDLIB_SRCS := $(wildcard $(FICUS_STDLIB)/*.fx)
FICUS_BIN := ./bin
FICUS := $(FICUS_BIN)/ficus

ifeq ($(OS),Windows_NT)
    CFLAGS += -D WIN32
else
    UNAME_S := $(shell uname -s)
    UNAME_P := $(shell uname -m)
	ifeq ($(UNAME_S),Darwin)
		ifeq ($(UNAME_P),x86_64)
			CFLAGS += -Xclang -fopenmp
			LDFLAGS += -Xclang -fopenmp
        	LDLIBS += -L./runtime/lib/macos_x64/ -lomp
		endif
		#ifeq ($(UNAME_P),arm64)
		#	CFLAGS += -Xclang -fopenmp
        #	LDLIBS += -L./runtime/lib/macos/x64/ -lmimalloc -lomp
		#endif
    endif
endif

FICUS_FLAGS := -verbose -O3

.PHONY: all clean final_note

all: $(FICUS0) $(FICUS) final_note

$(FICUS0): $(BOOTSTRAP_OBJS)
	@$(CC) $(LDFLAGS) $^ -o $@ $(LDLIBS)

$(BOOTSTRAP_BUILD_DIR)/ficus.o: $(BOOTSTRAP_SRC)/ficus.c $(RUNTIME_DEPS) $(RUNTIME_IMPL_DEPS)
	@echo "CC ficus.c"
	@$(CC) $(CFLAGS) -c $< -o $@

$(BOOTSTRAP_BUILD_DIR)/%.o: $(BOOTSTRAP_SRC)/%.c | $(BOOTSTRAP_BUILD_DIR)
	@echo "CC $<"
	@$(CC) $(CFLAGS) -c $< -o $@

$(FICUS): $(FICUS0) $(FICUS_SRCS) $(FICUS_STDLIB_SRCS)
	@echo "\033[34;1mBuilding fresh compiler from the actual ficus sources\033[0m"
	@$(MKDIR) $(FICUS_BIN)
	@$(FICUS0) $(FICUS_FLAGS) -o $(FICUS) $(FICUS_SRC)/fx.fx

$(BOOTSTRAP_BUILD_DIR):
	@echo "\033[34;1mBuilding reference compiler from the pre-generated .c sources\033[0m"
	@$(MKDIR) $(BUILD_DIR)
	@$(MKDIR) $(BOOTSTRAP_BUILD_DIR)

final_note: | $(FICUS)
	@echo ""
	@echo "\033[32;1m./ficus was built successfully.\033[0m"
	@echo "Now setup 'FICUS_PATH=<path_to_stdlib>' (lib subdirectory of the root directory)"
	@echo "and optionally 'FICUS_CFLAGS' and 'FICUS_LINK_LIBRARIES' to pass to C compiler."
	@echo "After that you can copy ficus to whatever directory you want and use it."
	@echo "When compiling an application, ficus creates __fxbuild__/<appname> subdirectory"
	@echo "in the current directory for the intermediate .c and .o files,"
	@echo "as well as the produced application".

clean:
	@$(RMDIR) $(BOOTSTRAP_BUILD_DIR)
	@$(RMDIR) $(BUILD_DIR)/ficus
	@$(RM) $(FICUS)
