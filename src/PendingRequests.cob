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

       LINKAGE SECTION.
       01 L-USERNAME PIC X(20).

       PROCEDURE DIVISION USING L-USERNAME.
       MAIN-PROGRAM.
           MOVE "---- Pending Connection Requests ----" TO WS-LINE
           PERFORM OUT

           OPEN INPUT PENDING-FILE
           IF WS-STATUS = "00"
              MOVE "N" TO EOF-FLAG
              PERFORM UNTIL EOF-FLAG = "Y"
                 READ PENDING-FILE INTO WS-LINE
                    AT END MOVE "Y" TO EOF-FLAG
                    NOT AT END
                       MOVE SPACES TO SENDER
                       MOVE SPACES TO RECEIVER
                       UNSTRING WS-LINE DELIMITED BY "->"
                          INTO SENDER RECEIVER
                       END-UNSTRING
                       
                       IF FUNCTION TRIM(RECEIVER) = FUNCTION TRIM(L-USERNAME)
                          MOVE "Y" TO WS-FOUND
                          MOVE WS-LINE TO WS-CURRENT-REQ
                          PERFORM PROCESS-REQUEST
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE PENDING-FILE

           IF WS-FOUND NOT = "Y"
              MOVE "You have no pending connection requests at this time." 
                   TO WS-LINE
              PERFORM OUT
           END-IF

           GOBACK.

       PROCESS-REQUEST.
           MOVE SPACES TO WS-LINE
           STRING "Request from: " FUNCTION TRIM(SENDER)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT

           MOVE "1. Accept" TO WS-LINE
           PERFORM OUT
           MOVE "2. Reject" TO WS-LINE
           PERFORM OUT

           MOVE SPACES TO WS-LINE
           STRING "Enter your choice for " FUNCTION TRIM(SENDER) ":"
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
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
                   MOVE "Invalid choice. Request skipped." TO WS-LINE
                   PERFORM OUT
           END-EVALUATE

           MOVE "-----------------------------------" TO WS-LINE
           PERFORM OUT.

       ACCEPT-REQUEST.
           CALL "ADD-CONNECTION" USING L-USERNAME SENDER
           PERFORM REMOVE-FROM-PENDING

           MOVE SPACES TO WS-LINE
           STRING "Connection request from " FUNCTION TRIM(SENDER)
                  " accepted!"
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT.

       REJECT-REQUEST.
           PERFORM REMOVE-FROM-PENDING

           MOVE SPACES TO WS-LINE
           STRING "Connection request from " FUNCTION TRIM(SENDER)
                  " rejected."
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT.

       REMOVE-FROM-PENDING.
           CLOSE PENDING-FILE
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
           
           CALL "SYSTEM" USING "mv data/pending.tmp data/pending.dat"
           
           OPEN INPUT PENDING-FILE.

       OUT.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM VIEW-PENDING-REQUESTS.