IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE-NAV.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD         PIC X(80).
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD        PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-CHOICE            PIC 9 VALUE 0.
       01 WS-SKILL-CHOICE      PIC 9 VALUE 0.
       01 WS-EOF-FLAG          PIC X VALUE 'N'.
       01 WS-USERNAME          PIC X(20) VALUE "TestUser".
       01 WS-OUTPUT-LINE       PIC X(80).
       01 WS-INPUT-CHOICE      PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           PERFORM DISPLAY-WELCOME
           PERFORM MAIN-MENU UNTIL WS-CHOICE = 9
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.
       
       DISPLAY-WELCOME.
           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Welcome, " WS-USERNAME "!" INTO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE.
       
       MAIN-MENU.
           PERFORM DISPLAY-MAIN-OPTIONS
           PERFORM READ-CHOICE
           
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM JOB-SEARCH-OPTION
               WHEN 2
                   PERFORM FIND-SOMEONE-OPTION
               WHEN 3
                   PERFORM LEARN-SKILL-MENU
               WHEN 4
                   MOVE 9 TO WS-CHOICE
               WHEN OTHER
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-OUTPUT
                   PERFORM WRITE-TO-FILE
           END-EVALUATE.
       
       DISPLAY-MAIN-OPTIONS.
           MOVE "Search for a job" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "Find someone you know" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "Learn a new skill" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "Exit" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE.
       
       JOB-SEARCH-OPTION.
           MOVE "Job search/internship is under construction." TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE.
       
       FIND-SOMEONE-OPTION.
           MOVE "Find someone you know is under construction." TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE.
       
       LEARN-SKILL-MENU.
           PERFORM DISPLAY-SKILLS
           PERFORM READ-SKILL-CHOICE
           
           EVALUATE WS-SKILL-CHOICE
               WHEN 1 THRU 5
                   MOVE "This skill is under construction." TO WS-OUTPUT-LINE
                   PERFORM WRITE-OUTPUT
                   PERFORM WRITE-TO-FILE
                   PERFORM LEARN-SKILL-MENU
               WHEN 6
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-OUTPUT
                   PERFORM WRITE-TO-FILE
                   PERFORM LEARN-SKILL-MENU
           END-EVALUATE.
       
       DISPLAY-SKILLS.
           MOVE "Learn a New Skill:" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "1. Python Programming" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "2. Data Analysis" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "3. Web Development" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "4. Digital Marketing" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "5. Project Management" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "6. Go Back" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE
           
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-OUTPUT
           PERFORM WRITE-TO-FILE.
       
       READ-CHOICE.
           READ INPUT-FILE INTO WS-INPUT-CHOICE
               AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ
           
           IF WS-EOF-FLAG = 'N'
               MOVE FUNCTION NUMVAL(WS-INPUT-CHOICE) TO WS-CHOICE
           ELSE
               MOVE 9 TO WS-CHOICE
           END-IF.
       
       READ-SKILL-CHOICE.
           READ INPUT-FILE INTO WS-INPUT-CHOICE
               AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ
           
           IF WS-EOF-FLAG = 'N'
               MOVE FUNCTION NUMVAL(WS-INPUT-CHOICE) TO WS-SKILL-CHOICE
           ELSE
               MOVE 6 TO WS-SKILL-CHOICE
           END-IF.
       
       WRITE-OUTPUT.
           DISPLAY WS-OUTPUT-LINE.
       
       WRITE-TO-FILE.
           MOVE WS-OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
