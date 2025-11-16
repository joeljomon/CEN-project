       IDENTIFICATION DIVISION.
       PROGRAM-ID. MESSAGE-MENU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CHOICE      PIC 9 VALUE 0.
       01 WS-OUTPUT-LINE PIC X(80).
       01 WS-COMMAND     PIC X(20).
    01 WS-LINE        PIC X(500).

       LINKAGE SECTION.
       01 MSG-USERNAME PIC X(20).

       PROCEDURE DIVISION USING MSG-USERNAME.
       MAIN-PROGRAM.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = 3
               PERFORM SHOW-MENU
               PERFORM GET-VALID-CHOICE
               PERFORM PROCESS-CHOICE
           END-PERFORM
           GOBACK.

       SHOW-MENU.
           MOVE "================== Messaging Menu ==================" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "1. Send a New Message" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "2. View My Messages" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "3. Back to Main Menu" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE "================== Send New Message ==================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "SEND-MESSAGE" USING MSG-USERNAME
                   CANCEL "SEND-MESSAGE"

               WHEN 2
                   MOVE "================== View My Messages ==================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "VIEW-MESSAGES" USING MSG-USERNAME

               WHEN 3
                   CONTINUE  *> Simply exit the loop to return to main menu

               WHEN OTHER
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
           END-EVALUATE.

       GET-VALID-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 3
               MOVE "Enter your choice: " TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               IF WS-LINE = HIGH-VALUES
                   MOVE 3 TO WS-CHOICE
               ELSE
                   MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
                   IF WS-CHOICE < 1 OR WS-CHOICE > 3
                       MOVE "Invalid choice. Please enter 1, 2, or 3." TO WS-OUTPUT-LINE
                       PERFORM WRITE-BOTH
                   END-IF
               END-IF
           END-PERFORM.

       WRITE-BOTH.
           MOVE WS-OUTPUT-LINE TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM MESSAGE-MENU.
