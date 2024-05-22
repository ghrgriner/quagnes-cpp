#-----------------------------------------------------------------------------
# File: makefile
# Author: Ray Griner
# Date: 2019
# Purpose: makefile to build executable
# Changes: [Changes before v1.0.0 not logged here]
# [20240411] Make subdirectories src/, obj/ and bin/ and rename inc to include
# [20240521] Compile with static linkage to use Docker image "FROM scratch"
#-----------------------------------------------------------------------------
SRC_DIR     := src
OBJ_DIR     := obj
BIN_DIR     := bin

EXE         := $(BIN_DIR)/quagnes
SOURCE      := $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS     := $(SOURCE:$(SRC_DIR)/%.cpp=$(OBJ_DIR)/%.o)
CC          := g++
INCLUDES    := -Iinclude

#CFLAGS = -g $(INCLUDES) -Wall -Wpedantic -Wextra
#CFLAGS = $(INCLUDES) -Wall -Wpedantic -Wextra -O2
#CFLAGS = $(INCLUDES) -DNDEBUG -Wall -Wpedantic -Wextra -O2
CFLAGS = $(INCLUDES) -DNDEBUG -Wall -Wpedantic -Wextra -O2
#$(CC) -static -static-libstdc++ -static-libgcc $^ -o $@

all: $(EXE)

$(EXE): $(OBJECTS) | $(BIN_DIR)
	$(CC) $^ -o $@ -static

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BIN_DIR) $(OBJ_DIR):
	mkdir -p $@

clean:
	rm -rv $(BIN_DIR) $(OBJ_DIR)
