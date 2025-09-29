       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECTION-REQUEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PENDING-FILE ASSIGN TO "data/pending.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PEND-STATUS.
           SELECT CONNECTIONS-FILE ASSIGN TO "data/connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONN-STATUS.
           SELECT ACCOUNTS-FILE ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACC-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  PENDING-FILE.
       01  PENDING-REC.
           05 P-SENDER         PIC X(20).
           05 FILLER           PIC X VALUE '|'.
           05 P-RECIP          PIC X(20).
           05 FILLER           PIC X VALUE '|'.
           05 P-TS             PIC X(19).

       FD  CONNECTIONS-FILE.
       01  CONN-LINE          PIC X(200).

       FD  ACCOUNTS-FILE.
       01  ACC-REC.
           05 ACC-USER        PIC X(20).
           05 ACC-PASS        PIC X(20).

       WORKING-STORAGE SECTION.
       77 WS-PEND-STATUS      PIC XX.
       77 WS-CONN-STATUS      PIC XX.
       77 WS-ACC-STATUS       PIC XX.

       77 WS-LINE             PIC X(200).
       77 WS-CMD              PIC X(20).

       *> Validation flags
       77 WS-SELF             PIC X  VALUE "N".
       77 WS-RECIP-EXIST      PIC X  VALUE "N".
       77 WS-ALR-CONN         PIC X  VALUE "N".
       77 WS-OUT-DUP          PIC X  VALUE "N".
       77 WS-IN-DUP           PIC X  VALUE "N".

       *> Temp variables for parsing connections
       77 WS-TMP-A            PIC X(20).
       77 WS-TMP-B            PIC X(20).

       *> Simple placeholder timestamp
       77 WS-TS               PIC X(19) VALUE "2025-01-01T00:00:00".

       LINKAGE SECTION.
       01 L-SENDER-USERNAME     PIC X(20).
       01 L-RECEIVER-USERNAME   PIC X(20).
       01 L-RECEIVER-FULLNAME   PIC X(40).

       PROCEDURE DIVISION USING
            L-SENDER-USERNAME
            L-RECEIVER-USERNAME
            L-RECEIVER-FULLNAME.

       MAIN-PROGRAM.
           PERFORM INIT-FLAGS
           PERFORM CHECK-SELF
           IF WS-SELF = "Y"
               MOVE "You cannot connect with yourself." TO WS-LINE
               PERFORM OUT
               GOBACK
           END-IF

           PERFORM CHECK-RECIP-EXISTS
           IF WS-RECIP-EXIST NOT = "Y"
               MOVE "User does not exist." TO WS-LINE
               PERFORM OUT
               GOBACK
           END-IF

           PERFORM CHECK-ALREADY-CONNECTED
           IF WS-ALR-CONN = "Y"
               MOVE "You are already connected with this user." TO WS-LINE
               PERFORM OUT
               GOBACK
           END-IF

           PERFORM CHECK-PENDING-DUPLICATES
           IF WS-OUT-DUP = "Y"
               MOVE "You already sent a connection request to this user." TO WS-LINE
               PERFORM OUT
               GOBACK
           END-IF
           IF WS-IN-DUP = "Y"
               MOVE "This user has already sent you a connection request." TO WS-LINE
               PERFORM OUT
               GOBACK
           END-IF

           PERFORM WRITE-PENDING

           STRING "Connection request sent to "
                  FUNCTION TRIM(L-RECEIVER-FULLNAME)
                  INTO WS-LINE
           END-STRING
           PERFORM OUT
           GOBACK.

       INIT-FLAGS.
           MOVE "N" TO WS-SELF WS-RECIP-EXIST WS-ALR-CONN WS-OUT-DUP WS-IN-DUP.

       CHECK-SELF.
           IF FUNCTION TRIM(L-SENDER-USERNAME) =
              FUNCTION TRIM(L-RECEIVER-USERNAME)
              MOVE "Y" TO WS-SELF
           END-IF.

       CHECK-RECIP-EXISTS.
           MOVE "N" TO WS-RECIP-EXIST
           OPEN INPUT ACCOUNTS-FILE
           IF WS-ACC-STATUS = "00"
              PERFORM UNTIL WS-ACC-STATUS NOT = "00"
                 READ ACCOUNTS-FILE
                    AT END EXIT PERFORM
                    NOT AT END
                       IF FUNCTION TRIM(ACC-USER) =
                          FUNCTION TRIM(L-RECEIVER-USERNAME)
                          MOVE "Y" TO WS-RECIP-EXIST
                          EXIT PERFORM
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE ACCOUNTS-FILE.

       CHECK-ALREADY-CONNECTED.
           MOVE "N" TO WS-ALR-CONN
           OPEN INPUT CONNECTIONS-FILE
           IF WS-CONN-STATUS = "00"
              PERFORM UNTIL WS-CONN-STATUS NOT = "00"
                 READ CONNECTIONS-FILE INTO WS-LINE
                    AT END EXIT PERFORM
                    NOT AT END
                       MOVE SPACES TO WS-TMP-A WS-TMP-B
                       UNSTRING FUNCTION TRIM(WS-LINE)
                         DELIMITED BY "|"
                         INTO WS-TMP-A WS-TMP-B
                         ON OVERFLOW
                           MOVE WS-LINE(1:20)  TO WS-TMP-A
                           MOVE WS-LINE(21:20) TO WS-TMP-B
                       END-UNSTRING
                       IF (FUNCTION TRIM(WS-TMP-A) = FUNCTION TRIM(L-SENDER-USERNAME)
                           AND FUNCTION TRIM(WS-TMP-B) = FUNCTION TRIM(L-RECEIVER-USERNAME))
                        OR (FUNCTION TRIM(WS-TMP-A) = FUNCTION TRIM(L-RECEIVER-USERNAME)
                           AND FUNCTION TRIM(WS-TMP-B) = FUNCTION TRIM(L-SENDER-USERNAME))
                          MOVE "Y" TO WS-ALR-CONN
                          EXIT PERFORM
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE CONNECTIONS-FILE.

       CHECK-PENDING-DUPLICATES.
           MOVE "N" TO WS-OUT-DUP WS-IN-DUP
           OPEN INPUT PENDING-FILE
           IF WS-PEND-STATUS = "00"
              PERFORM UNTIL WS-PEND-STATUS NOT = "00"
                 READ PENDING-FILE
                    AT END EXIT PERFORM
                    NOT AT END
                       IF FUNCTION TRIM(P-SENDER) =
                          FUNCTION TRIM(L-SENDER-USERNAME)
                          AND FUNCTION TRIM(P-RECIP) =
                          FUNCTION TRIM(L-RECEIVER-USERNAME)
                          MOVE "Y" TO WS-OUT-DUP
                       END-IF
                       IF FUNCTION TRIM(P-SENDER) =
                          FUNCTION TRIM(L-RECEIVER-USERNAME)
                          AND FUNCTION TRIM(P-RECIP) =
                          FUNCTION TRIM(L-SENDER-USERNAME)
                          MOVE "Y" TO WS-IN-DUP
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE PENDING-FILE.

       WRITE-PENDING.
           OPEN EXTEND PENDING-FILE
           IF WS-PEND-STATUS = "00"
              MOVE FUNCTION TRIM(L-SENDER-USERNAME)   TO P-SENDER
              MOVE FUNCTION TRIM(L-RECEIVER-USERNAME) TO P-RECIP
              MOVE WS-TS                               TO P-TS
              WRITE PENDING-REC
              CLOSE PENDING-FILE
           ELSE
              MOVE "Error: Cannot open pending file." TO WS-LINE
              PERFORM OUT
           END-IF.

       OUT.
           MOVE "WRITE" TO WS-CMD
           CALL "IO-MODULE" USING WS-CMD WS-LINE.
