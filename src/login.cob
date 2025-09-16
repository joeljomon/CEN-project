       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN.
       AUTHOR. Lakshmi Prakash.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-ACCOUNTS-FILE ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCOUNT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-ACCOUNTS-FILE
           LABEL RECORDS ARE STANDARD.
       01  USER-ACCOUNT-RECORD.
           05 USERNAME-F            PIC X(20).
           05 PASSWORD-F            PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-FILE-STATUS   PIC XX.
       01  WS-DISPLAY-LINE          PIC X(80).

       01  WS-USER-INPUT.
           05 WS-USERNAME           PIC X(20).
           05 WS-PASSWORD           PIC X(20).

       01  WS-FLAGS.
           05 WS-LOGIN-SUCCESS      PIC A(1) VALUE 'N'.

       01  WS-USER-ACCOUNTS.
           05 WS-USER-COUNT         PIC 9(3) VALUE 0.
           05 USER-TABLE.
              10 USER-ACCOUNT OCCURS 100 TIMES INDEXED BY I.
                 15 STORED-USERNAME PIC X(20).
                 15 STORED-PASSWORD PIC X(20).

       01  WS-ATTEMPTS              PIC 9 VALUE 0.
       01  WS-MAX-ATTEMPTS          PIC 9 VALUE 3.

       LINKAGE SECTION.
       01 LOGIN-USERNAME PIC X(20).
       01 LOGIN-PASSWORD PIC X(20).
       01 LOGIN-MESSAGE  PIC X(80).

       PROCEDURE DIVISION USING LOGIN-USERNAME
                                LOGIN-PASSWORD LOGIN-MESSAGE.

      MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-LOGIN-ROUTINE.
           GOBACK.

       1000-INITIALIZE.
           MOVE LOGIN-USERNAME TO WS-USERNAME
           MOVE LOGIN-PASSWORD TO WS-PASSWORD
           OPEN INPUT USER-ACCOUNTS-FILE

           IF WS-ACCOUNT-FILE-STATUS = "00"
              PERFORM UNTIL WS-ACCOUNT-FILE-STATUS NOT = "00"
                  READ USER-ACCOUNTS-FILE
                      AT END EXIT PERFORM
                      NOT AT END
                          ADD 1 TO WS-USER-COUNT
                          MOVE USERNAME-F TO STORED-USERNAME(WS-USER-COUNT)
                          MOVE PASSWORD-F TO STORED-PASSWORD(WS-USER-COUNT)
                  END-READ
              END-PERFORM
           END-IF

           CLOSE USER-ACCOUNTS-FILE.

       2000-LOGIN-ROUTINE.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           MOVE 0 TO WS-ATTEMPTS

           PERFORM UNTIL WS-LOGIN-SUCCESS = 'Y'
                   OR WS-ATTEMPTS >= WS-MAX-ATTEMPTS
               ADD 1 TO WS-ATTEMPTS
               PERFORM 2100-VALIDATE-CREDENTIALS
               IF WS-LOGIN-SUCCESS = 'Y'
                   MOVE "================ You have successfully logged in. ===================" TO WS-DISPLAY-LINE
               ELSE
                   MOVE " ==================== Incorrect username/password, please try again ====================" TO WS-DISPLAY-LINE
               END-IF
               PERFORM 9000-DISPLAY-LINE
           END-PERFORM.

       2100-VALIDATE-CREDENTIALS.
           MOVE 'N' TO WS-LOGIN-SUCCESS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-USER-COUNT
               IF FUNCTION TRIM(STORED-USERNAME(I)) = FUNCTION TRIM(WS-USERNAME)
                  AND FUNCTION TRIM(STORED-PASSWORD(I)) = FUNCTION TRIM(WS-PASSWORD)
                   MOVE 'Y' TO WS-LOGIN-SUCCESS
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       9000-DISPLAY-LINE.
           MOVE WS-DISPLAY-LINE TO LOGIN-MESSAGE
           DISPLAY WS-DISPLAY-LINE.
