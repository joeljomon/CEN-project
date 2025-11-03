       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-MESSAGES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MESSAGE-FILE ASSIGN TO "data/messages.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MSG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD MESSAGE-FILE.
       01 MESSAGE-RECORD.
           05 MSG-SENDER    PIC X(20).
           05 MSG-RECIPIENT PIC X(20).
           05 MSG-TEXT      PIC X(200).

       WORKING-STORAGE SECTION.
       77 WS-MSG-STATUS     PIC XX.
       77 WS-EOF             PIC X VALUE "N".
       77 WS-MSG-COUNT       PIC 9(3) VALUE 0.
       77 WS-COMMAND         PIC X(20).
       77 WS-LINE            PIC X(200).
       77 WS-LOOP-IDX        PIC 9(3) VALUE 0.
       77 WS-COUNT-STR       PIC Z9.

       01 WS-MESSAGE-TABLE.
           05 WS-MSG-ENTRY OCCURS 50 TIMES.
              10 WS-MSG-SENDER   PIC X(20).
              10 WS-MSG-TEXT     PIC X(200).

       LINKAGE SECTION.
       01 MSG-USERNAME   PIC X(20).

       PROCEDURE DIVISION USING MSG-USERNAME.
       MAIN-PROGRAM.
           PERFORM LOAD-MESSAGES
           PERFORM DISPLAY-MESSAGES
           GOBACK.

       LOAD-MESSAGES.
           MOVE 0 TO WS-MSG-COUNT
           MOVE "N" TO WS-EOF

           OPEN INPUT MESSAGE-FILE

           IF WS-MSG-STATUS = "00"
              READ MESSAGE-FILE
                  AT END MOVE 'Y' TO WS-EOF
              END-READ
              PERFORM UNTIL WS-EOF = 'Y' OR WS-MSG-COUNT >= 50
                  IF FUNCTION TRIM(MSG-RECIPIENT) = 
                     FUNCTION TRIM(MSG-USERNAME)
                     ADD 1 TO WS-MSG-COUNT
                     MOVE MSG-SENDER TO 
                          WS-MSG-SENDER(WS-MSG-COUNT)
                     MOVE MSG-TEXT TO 
                          WS-MSG-TEXT(WS-MSG-COUNT)
                  END-IF
                  READ MESSAGE-FILE
                      AT END MOVE 'Y' TO WS-EOF
                  END-READ
              END-PERFORM
              CLOSE MESSAGE-FILE
           ELSE
              MOVE "Error opening messages file." TO WS-LINE
              PERFORM WRITE-LINE
           END-IF.

       DISPLAY-MESSAGES.
           MOVE "--- Your Messages ---" TO WS-LINE
           PERFORM WRITE-LINE
           
           MOVE SPACES TO WS-LINE
           STRING "Messages for " 
                  FUNCTION TRIM(MSG-USERNAME)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM WRITE-LINE
           
           MOVE "------------------------------" TO WS-LINE
           PERFORM WRITE-LINE

           IF WS-MSG-COUNT = 0
              MOVE " " TO WS-LINE
              PERFORM WRITE-LINE
              MOVE "You have no messages at this time." TO WS-LINE
              PERFORM WRITE-LINE
           ELSE
              PERFORM DISPLAY-EACH-MESSAGE
              PERFORM DISPLAY-TOTAL-COUNT
           END-IF

           MOVE "------------------------------" TO WS-LINE
           PERFORM WRITE-LINE
           MOVE " " TO WS-LINE
           PERFORM WRITE-LINE.

       DISPLAY-EACH-MESSAGE.
           PERFORM VARYING WS-LOOP-IDX FROM 1 BY 1 
                   UNTIL WS-LOOP-IDX > WS-MSG-COUNT
              
              MOVE SPACES TO WS-LINE
              STRING "From: " 
                     FUNCTION TRIM(WS-MSG-SENDER(WS-LOOP-IDX))
                     DELIMITED BY SIZE
                     INTO WS-LINE
              END-STRING
              PERFORM WRITE-LINE

              MOVE SPACES TO WS-LINE
              STRING "Message: " 
                     FUNCTION TRIM(WS-MSG-TEXT(WS-LOOP-IDX))
                     DELIMITED BY SIZE
                     INTO WS-LINE
              END-STRING
              PERFORM WRITE-LINE
              
              MOVE "---" TO WS-LINE
              PERFORM WRITE-LINE
           END-PERFORM.

       DISPLAY-TOTAL-COUNT.
           MOVE WS-MSG-COUNT TO WS-COUNT-STR
           MOVE SPACES TO WS-LINE
           STRING "Total Messages: " 
                  FUNCTION TRIM(WS-COUNT-STR)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM WRITE-LINE.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND, WS-LINE.

       END PROGRAM VIEW-MESSAGES.
