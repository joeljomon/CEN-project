IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-PENDING-REQUESTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PENDING-FILE ASSIGN TO "data/pending.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT TEMP-FILE ASSIGN TO "data/pending.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TEMP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD PENDING-FILE.
       01 PENDING-REC PIC X(200).

       FD TEMP-FILE.
       01 TEMP-REC PIC X(200).

       WORKING-STORAGE SECTION.
       77 WS-STATUS       PIC XX.
       77 WS-TEMP-STATUS  PIC XX.
       77 WS-LINE         PIC X(200).
       77 EOF-FLAG        PIC X VALUE "N".
       77 WS-FOUND        PIC X VALUE "N".
       77 SENDER          PIC X(20).
       77 RECEIVER        PIC X(20).
       77 WS-COMMAND      PIC X(20).
       77 WS-CHOICE       PIC X(80).
       77 WS-CURRENT-REQ  PIC X(200).
       77 WS-REQUEST-COUNT PIC 99 VALUE 0.
       77 WS-CONTINUE     PIC X VALUE "Y".
       77 WS-SELECTED-USER PIC X(20).
       77 WS-LOOP-COUNTER PIC 99 VALUE 0.
       77 WS-SELECTED-NUM PIC 99 VALUE 0.

       01 WS-REQUEST-TABLE.
          05 WS-REQUEST-ENTRY OCCURS 20 TIMES.
             10 WS-REQ-SENDER    PIC X(20).
             10 WS-REQ-FULL-LINE PIC X(200).

       LINKAGE SECTION.
       01 L-USERNAME PIC X(20).

       PROCEDURE DIVISION USING L-USERNAME.
       MAIN-PROGRAM.
           MOVE 0 TO WS-REQUEST-COUNT
           MOVE "N" TO WS-FOUND
           
           PERFORM LOAD-PENDING-REQUESTS
           
           IF WS-REQUEST-COUNT = 0
              MOVE "You have no pending connection requests at this time." 
                   TO WS-LINE
              PERFORM OUT
           ELSE
              PERFORM DISPLAY-ALL-REQUESTS
              PERFORM PROCESS-SELECTED-REQUEST
           END-IF
           
           GOBACK.
           
       LOAD-PENDING-REQUESTS.
           MOVE "---- Pending Connection Requests ----" TO WS-LINE
           PERFORM OUT
           
           OPEN INPUT PENDING-FILE
           IF WS-STATUS = "00"
              MOVE "N" TO EOF-FLAG
              PERFORM UNTIL EOF-FLAG = "Y" OR WS-REQUEST-COUNT >= 20
                 READ PENDING-FILE INTO WS-LINE
                    AT END MOVE "Y" TO EOF-FLAG
                    NOT AT END
                       MOVE SPACES TO SENDER
                       MOVE SPACES TO RECEIVER
                       UNSTRING WS-LINE DELIMITED BY "->"
                          INTO SENDER RECEIVER
                       END-UNSTRING
                       
                       IF FUNCTION TRIM(RECEIVER) = FUNCTION TRIM(L-USERNAME)
                          ADD 1 TO WS-REQUEST-COUNT
                          MOVE SENDER TO WS-REQ-SENDER(WS-REQUEST-COUNT)
                          MOVE WS-LINE TO WS-REQ-FULL-LINE(WS-REQUEST-COUNT)
                          MOVE "Y" TO WS-FOUND
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE PENDING-FILE.

       DISPLAY-ALL-REQUESTS.
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
                   UNTIL WS-LOOP-COUNTER > WS-REQUEST-COUNT
              MOVE SPACES TO WS-LINE
              STRING WS-LOOP-COUNTER ". Request from: " 
                     FUNCTION TRIM(WS-REQ-SENDER(WS-LOOP-COUNTER))
                     DELIMITED BY SIZE
                     INTO WS-LINE
              END-STRING
              PERFORM OUT
           END-PERFORM
           
           MOVE "-----------------------------------" TO WS-LINE
           PERFORM OUT.

       PROCESS-SELECTED-REQUEST.
           MOVE "Enter number to process (or 0 to go back):" TO WS-LINE
           PERFORM OUT
           
           MOVE SPACES TO WS-CHOICE
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-CHOICE
           
           MOVE FUNCTION NUMVAL(WS-CHOICE) TO WS-SELECTED-NUM
           
           IF WS-SELECTED-NUM = 0
              MOVE "N" TO WS-CONTINUE
           ELSE IF WS-SELECTED-NUM >= 1 AND 
                   WS-SELECTED-NUM <= WS-REQUEST-COUNT
              MOVE WS-REQ-SENDER(WS-SELECTED-NUM) 
                   TO WS-SELECTED-USER
              MOVE WS-REQ-FULL-LINE(WS-SELECTED-NUM) 
                   TO WS-CURRENT-REQ
              PERFORM SHOW-ACCEPT-REJECT-OPTIONS
           ELSE
              MOVE "Invalid selection." TO WS-LINE
              PERFORM OUT
           END-IF.

       SHOW-ACCEPT-REJECT-OPTIONS.
           MOVE SPACES TO WS-LINE
           STRING "Process request from " 
                  FUNCTION TRIM(WS-SELECTED-USER) ":"
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT
           
           MOVE "1. Accept" TO WS-LINE
           PERFORM OUT
           MOVE "2. Reject" TO WS-LINE
           PERFORM OUT
           
           MOVE SPACES TO WS-CHOICE
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-CHOICE
           
           EVALUATE FUNCTION TRIM(WS-CHOICE)
              WHEN "1"
                   PERFORM ACCEPT-REQUEST
              WHEN "2"
                   PERFORM REJECT-REQUEST
              WHEN OTHER
                   MOVE "Invalid choice." TO WS-LINE
                   PERFORM OUT
           END-EVALUATE
           
           MOVE "-----------------------------------" TO WS-LINE
           PERFORM OUT.

       ACCEPT-REQUEST.
           CALL "ADD-CONNECTION" USING L-USERNAME WS-SELECTED-USER
           PERFORM REMOVE-FROM-PENDING
           
           MOVE SPACES TO WS-LINE
           STRING "Connection request from " 
                  FUNCTION TRIM(WS-SELECTED-USER)
                  " accepted!"
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT.

       REJECT-REQUEST.
           PERFORM REMOVE-FROM-PENDING
           
           MOVE SPACES TO WS-LINE
           STRING "Connection request from " 
                  FUNCTION TRIM(WS-SELECTED-USER)
                  " rejected."
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT.

       REMOVE-FROM-PENDING.
           OPEN INPUT PENDING-FILE
           OPEN OUTPUT TEMP-FILE
           
           MOVE "N" TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = "Y"
              READ PENDING-FILE INTO WS-LINE
                 AT END MOVE "Y" TO EOF-FLAG
                 NOT AT END
                    IF WS-LINE NOT = WS-CURRENT-REQ
                       WRITE TEMP-REC FROM WS-LINE
                    END-IF
              END-READ
           END-PERFORM
           
           CLOSE PENDING-FILE
           CLOSE TEMP-FILE
           
           CALL "SYSTEM" USING "mv data/pending.tmp data/pending.dat".

       OUT.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM VIEW-PENDING-REQUESTS.