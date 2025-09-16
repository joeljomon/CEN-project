       IDENTIFICATION DIVISION.
       PROGRAM-ID. SKILL-MENU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CHOICE      PIC 9 VALUE 0.
       01 WS-OUTPUT-LINE PIC X(80).
       01 WS-COMMAND     PIC X(20).
       01 WS-LINE        PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = 6
               MOVE "===============Choose to learn a New Skill===================" TO WS-OUTPUT-LINE
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
           END-PERFORM
           GOBACK.

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
               MOVE 6 TO WS-CHOICE
           ELSE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
           END-IF.

       WRITE-BOTH.
           MOVE WS-OUTPUT-LINE TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.
       END PROGRAM SKILL-MENU.
