       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE-NAV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CHOICE      PIC 9 VALUE 0.
       01 WS-OUTPUT-LINE PIC X(80).
       01 WS-COMMAND     PIC X(20).
       01 WS-LINE        PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM UNTIL WS-CHOICE = 4
               PERFORM SHOW-MENU
               PERFORM GET-VALID-MAIN-CHOICE
               PERFORM PROCESS-CHOICE
           END-PERFORM
           GOBACK.

       SHOW-MENU.
           MOVE "Main Menu:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "1. Search for a job" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "2. Find someone you know" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "3. Learn a new skill" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "4. Return to Main Menu" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE "Job search/internship is under construction."
                       TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               WHEN 2
                   MOVE "Find someone you know is under construction."
                       TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               WHEN 3
                   PERFORM SKILL-MENU
           END-EVALUATE.

       SKILL-MENU.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = 6
               MOVE "Learn a New Skill:" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "1. Python Programming" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "2. Data Analysis" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "3. Web Development" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "4. Digital Marketing" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "5. Project Management" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "6. Go Back" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "Enter your choice:" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH

               PERFORM GET-VALID-SKILL-CHOICE

               IF WS-CHOICE >= 1 AND WS-CHOICE <= 5
                   MOVE "This skill is under construction."
                       TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.

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
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           IF WS-LINE = HIGH-VALUES
               MOVE 4 TO WS-CHOICE
           ELSE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
           END-IF.

       WRITE-BOTH.
           MOVE WS-OUTPUT-LINE TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.
