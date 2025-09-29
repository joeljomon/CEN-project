IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-PENDING-REQUESTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PENDING-FILE ASSIGN TO "data/pending.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD PENDING-FILE.
       01 PENDING-REC PIC X(200).

       WORKING-STORAGE SECTION.
       77 WS-STATUS  PIC XX.
       77 WS-LINE    PIC X(200).
       77 EOF-FLAG   PIC X VALUE "N".
       77 WS-FOUND   PIC X VALUE "N".
       77 SENDER     PIC X(20).
       77 RECEIVER   PIC X(20).
       77 WS-COMMAND PIC X(20).

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
                       UNSTRING WS-LINE DELIMITED BY "->"
                          INTO SENDER RECEIVER
                       END-UNSTRING
                       IF FUNCTION TRIM(RECEIVER) = FUNCTION TRIM(L-USERNAME)
                          MOVE SPACES TO WS-LINE
                          STRING "- " FUNCTION TRIM(SENDER)
                             DELIMITED BY SIZE
                             INTO WS-LINE
                          END-STRING
                          PERFORM OUT
                          MOVE "Y" TO WS-FOUND
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

           MOVE "-----------------------------------" TO WS-LINE
           PERFORM OUT

           GOBACK.

       OUT.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM VIEW-PENDING-REQUESTS.