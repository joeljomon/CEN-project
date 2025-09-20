       IDENTIFICATION DIVISION.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE-NAV.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "data/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "data/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD         PIC X(10).


       FD OUTPUT-FILE.
       01 OUTPUT-RECORD        PIC X(80).


       WORKING-STORAGE SECTION.
       01 WS-CHOICE            PIC 9.
       01 WS-OUTPUT-LINE       PIC X(80).


       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           MOVE "Main Menu: " TO WS-OUTPUT-LINE

           MOVE "Main Menu: " TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH


           PERFORM UNTIL WS-CHOICE = 4
               PERFORM SHOW-MENU
               PERFORM GET-VALID-MAIN-CHOICE
               PERFORM GET-VALID-MAIN-CHOICE
               PERFORM PROCESS-CHOICE
           END-PERFORM


           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.


       SHOW-MENU.
           MOVE "Search for a job" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Find someone you know" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Learn a new skill" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Exit" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH.


       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE "Job search/internship is under construction."
                   MOVE "Job search/internship is under construction."
                   TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               WHEN 2
                   MOVE "Find someone you know is under construction."
                   MOVE "Find someone you know is under construction."
                   TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               WHEN 3
                   PERFORM SKILL-MENU
           END-EVALUATE.


       SKILL-MENU.
           MOVE "Learn a New Skill:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Python Programming" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Data Analysis" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Web Development" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Digital Marketing" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Project Management" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Go Back" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH

           PERFORM GET-VALID-SKILL-CHOICE

           PERFORM GET-VALID-SKILL-CHOICE
           IF WS-CHOICE >= 1 AND WS-CHOICE <= 5
              MOVE "This skill is under construction." TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               PERFORM SKILL-MENU
           END-IF.

       GET-VALID-MAIN-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 4
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 4
              MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.

       GET-VALID-SKILL-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 6
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 6
              MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.

       GET-CHOICE.
           READ INPUT-FILE INTO INPUT-RECORD
           MOVE FUNCTION NUMVAL(INPUT-RECORD) TO WS-CHOICE.


       WRITE-BOTH.
           DISPLAY FUNCTION TRIM(WS-OUTPUT-LINE)
           DISPLAY FUNCTION TRIM(WS-OUTPUT-LINE)
           MOVE WS-OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
