       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-COMMAND  PIC X(20).
       77 WS-LINE     PIC X(80).
       77 WS-USERNAME PIC X(20).
       77 WS-PASSWORD PIC X(20).
       77 WS-MESSAGE  PIC X(80).
       77 WS-CHOICE   PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE "OPEN" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           PERFORM UNTIL WS-CHOICE = 9
               MOVE " ================ Welcome to InCollege! ===================== " TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND

               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               MOVE "1. Log In" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               MOVE "2. Create New Account" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               MOVE "9. Exit" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               *> Read main menu choice
               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               IF WS-LINE = HIGH-VALUES
                   MOVE 9 TO WS-CHOICE
                   EXIT PERFORM
               ELSE
                   MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
               END-IF

               EVALUATE WS-CHOICE
                  WHEN 1
                       *> Username
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-USERNAME
                       END-IF

                       *> Password
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-PASSWORD
                       END-IF

                       MOVE "LOGIN" TO WS-COMMAND
                       CALL "ACCOUNT-MGMT" USING WS-COMMAND WS-USERNAME WS-PASSWORD WS-MESSAGE

                       MOVE WS-MESSAGE TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE

                       IF WS-MESSAGE = "================ You have successfully logged in. ==================="
                           CALL "INCOLLEGE-NAV" USING WS-USERNAME
                       END-IF

                  WHEN 2
                       *> Username
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-USERNAME
                       END-IF

                       *> Password
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-PASSWORD
                       END-IF

                       MOVE "CREATE" TO WS-COMMAND
                       CALL "ACCOUNT-MGMT" USING WS-COMMAND WS-USERNAME WS-PASSWORD WS-MESSAGE

                       MOVE WS-MESSAGE TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE

                  WHEN 9
                       MOVE " =============== Thank you for using InCollege! ================= " TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       CONTINUE

                  WHEN OTHER
                       MOVE " =============== Invalid choice. Please try again. ================= " TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               END-EVALUATE
           END-PERFORM

           MOVE "CLOSE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           GOBACK.
       END PROGRAM INCOLLEGE.
       