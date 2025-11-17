IDENTIFICATION DIVISION.
       PROGRAM-ID. JOB-MENU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CHOICE      PIC 9 VALUE 0.
       01 WS-OUTPUT-LINE PIC X(80).
       01 WS-COMMAND     PIC X(20).
       01 WS-LINE        PIC X(500).

       LINKAGE SECTION.
       01 JM-USERNAME PIC X(20).

       PROCEDURE DIVISION USING JM-USERNAME.
       MAIN-PROGRAM.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = 4
               PERFORM SHOW-MENU
               PERFORM GET-VALID-CHOICE
               PERFORM PROCESS-CHOICE
           END-PERFORM
           GOBACK.

       SHOW-MENU.
           MOVE "--- Job Search/Internship Menu ---" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "1. Post a Job/Internship" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "2. Browse Jobs/Internships" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "3. View My Applications" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "4. Back to Main Menu" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   CALL "POST-JOB" USING JM-USERNAME
               WHEN 2
                   CALL "BROWSE-APPLY-JOBS" USING JM-USERNAME
               WHEN 3
                   CALL "VIEW-MY-APPLICATIONS" USING JM-USERNAME
               WHEN 4
                   MOVE "Returning to Main Menu..." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       GET-VALID-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 4
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 4
                   MOVE "Invalid choice. Please enter a number between 1 and 4."
                       TO WS-OUTPUT-LINE
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

       END PROGRAM JOB-MENU.
      