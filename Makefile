PROGRAM=bin/InCollege
SRC=src/InCollege.cob src/IO.cob src/AccountManagement.cob src/Utilities.cob src/login.cob src/Navigation.cob src/SkillMenu.cob src/CreateEditProfile.cob src/ViewProfile.cob

all: $(PROGRAM)

$(PROGRAM): $(SRC)
	mkdir -p bin
	cobc -x -free -o $@ $(SRC)

run: all
	-./$(PROGRAM)

clean:
	rm -f bin/*