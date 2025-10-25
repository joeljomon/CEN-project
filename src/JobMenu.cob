       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOB-MENU.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WS-CHOICE      PIC 9 VALUE 0.
       01 WS-OUTPUT-LINE PIC X(80).
       01 WS-COMMAND     PIC X(20).
       01 WS-LINE        PIC X(80).

       LINKAGE SECTION.
       01 JM-USERNAME PIC X(20).

       PROCEDURE DIVISION USING JM-USERNAME.
       MAIN-PROGRAM.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = 3
               PERFORM SHOW-MENU
               PERFORM GET-VALID-MAIN-CHOICE
               PERFORM PROCESS-CHOICE
           END-PERFORM
           GOBACK.

       SHOW-MENU.
           MOVE "================== Jobs/Internships Menu ==================" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "1. Post a Job/Internship" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "2. Browse Jobs/Internships" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "3. Return to Main Menu"  TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE "You chose to post a job/internship." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   MOVE "================== Post Job/Internship ==================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "POST-JOB" USING JM-USERNAME

               WHEN 2
                   MOVE "You chose to browse jobs/internships." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   MOVE "================== Browse Jobs/Internships ==================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "BROWSE-APPLY-JOBS" USING JM-USERNAME

           WHEN OTHER
               CONTINUE
       END-EVALUATE.


       GET-VALID-MAIN-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 3
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 3
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.

       GET-CHOICE.
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           IF WS-LINE = HIGH-VALUES
               MOVE 3 TO WS-CHOICE
           ELSE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
           END-IF.

       WRITE-BOTH.
           MOVE WS-OUTPUT-LINE TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.
           
       END PROGRAM JOB-MENU.
       