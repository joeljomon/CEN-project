IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECTION-REQUEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PENDING-FILE ASSIGN TO "data/pending.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PENDING-FILE.
       01 PENDING-REC                PIC X(200).

       WORKING-STORAGE SECTION.
       77  WS-LINE                   PIC X(200).
       77  WS-ALR-CONN               PIC X(1) VALUE "N".
       77  WS-OUT-DUP                PIC X(1) VALUE "N".
       77  WS-IN-DUP                 PIC X(1) VALUE "N".
       77  EOF-FLAG                  PIC X(1) VALUE "N".
       77  WS-COMMAND                PIC X(20).
       77  FILE-SENDER               PIC X(20).
       77  FILE-RECEIVER             PIC X(20).

       LINKAGE SECTION.
       01  WS-USERNAME               PIC X(20).
       01  WS-RECEIVER               PIC X(20).

       PROCEDURE DIVISION USING WS-USERNAME WS-RECEIVER.
       MAIN-PROGRAM.
           PERFORM INIT-FLAGS
           PERFORM CHECK-PENDING-DUPLICATES
           
           IF WS-OUT-DUP = "Y"
              MOVE "You already sent a connection request to this user." 
                   TO WS-LINE
              PERFORM OUT
              GOBACK
           END-IF
           
           IF WS-IN-DUP = "Y"
              MOVE "This user has already sent you a connection request." 
                   TO WS-LINE
              PERFORM OUT
              GOBACK
           END-IF

           *> No issues: save new request
           OPEN EXTEND PENDING-FILE
           MOVE SPACES TO WS-LINE
           STRING FUNCTION TRIM(WS-USERNAME) DELIMITED BY SIZE
                  "->" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-RECEIVER) DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           WRITE PENDING-REC FROM WS-LINE
           CLOSE PENDING-FILE

           MOVE SPACES TO WS-LINE
           STRING "Connection request sent successfully to " 
                  FUNCTION TRIM(WS-RECEIVER)
                  "!"
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT

           GOBACK.

       INIT-FLAGS.
           MOVE "N" TO WS-ALR-CONN
           MOVE "N" TO WS-OUT-DUP
           MOVE "N" TO WS-IN-DUP
           MOVE "N" TO EOF-FLAG.

       CHECK-PENDING-DUPLICATES.
           OPEN INPUT PENDING-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ PENDING-FILE INTO WS-LINE
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE SPACES TO FILE-SENDER
                       MOVE SPACES TO FILE-RECEIVER
                       UNSTRING WS-LINE DELIMITED BY "->"
                          INTO FILE-SENDER FILE-RECEIVER
                       END-UNSTRING
                       
                       *> Outgoing duplicate: I already sent to them
                       IF FUNCTION TRIM(FILE-SENDER) = 
                          FUNCTION TRIM(WS-USERNAME)
                          AND FUNCTION TRIM(FILE-RECEIVER) = 
                          FUNCTION TRIM(WS-RECEIVER)
                          MOVE "Y" TO WS-OUT-DUP
                       END-IF

                       *> Incoming duplicate: They already sent to me
                       IF FUNCTION TRIM(FILE-SENDER) = 
                          FUNCTION TRIM(WS-RECEIVER)
                          AND FUNCTION TRIM(FILE-RECEIVER) = 
                          FUNCTION TRIM(WS-USERNAME)
                          MOVE "Y" TO WS-IN-DUP
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PENDING-FILE
           MOVE "N" TO EOF-FLAG.

       OUT.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM CONNECTION-REQUEST.
       