# Makefile for InCollege — tidy build layout
SRC_DIR   := src
BUILD_DIR := build
BIN_DIR   := bin
BIN       := $(BIN_DIR)/InCollegeApp

# Subprograms (have USING)
SUBS := AccountManagement.cob AddConnection.cob ConnectionRequest.cob CreateEditProfile.cob IO.cob \
        login.cob Navigation.cob NetworkDisplay.cob PendingRequests.cob SearchUser.cob SkillMenu.cob \
        Utilities.cob ViewProfile.cob JobMenu.cob PostJob.cob

SUB_OBJS := $(addprefix $(BUILD_DIR)/,$(SUBS:.cob=.o))

.PHONY: all clean run dirs

all: dirs $(BIN)

dirs:
	mkdir -p $(BUILD_DIR) $(BIN_DIR)

# Link the main with all subprogram objects
$(BIN): $(SUB_OBJS) $(SRC_DIR)/InCollege.cob
	cobc -free -x -o $@ $(SRC_DIR)/InCollege.cob $(SUB_OBJS)

# Compile each subprogram to build/*.o
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cob | dirs
	cobc -free -c -o $@ $<

run: all
	./$(BIN)

clean:
	rm -rf $(BUILD_DIR) $(BIN) $(BIN).exe
