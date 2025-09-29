       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-MGMT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE.
       01 ACCOUNT-REC.
          05 ACC-USERNAME PIC X(20).
          05 ACC-PASSWORD PIC X(20).

       WORKING-STORAGE SECTION.
       77 WS-VALID-FLAG     PIC X.
       77 WS-DUPLICATE-FLAG PIC X VALUE "N".
       77 WS-END-OF-FILE    PIC X VALUE "N".
       77 WS-COUNT          PIC 9(01) VALUE 0.
       77 WS-LIMIT          PIC 9(01) VALUE 5.

       LINKAGE SECTION.
       01 AM-COMMAND  PIC X(20).
       01 AM-USERNAME PIC X(20).
       01 AM-PASSWORD PIC X(20).
       01 AM-MESSAGE  PIC X(80).

       PROCEDURE DIVISION USING AM-COMMAND AM-USERNAME AM-PASSWORD AM-MESSAGE.
           EVALUATE AM-COMMAND
              WHEN "CREATE"
                   *> Reset flags
                   MOVE "N" TO WS-DUPLICATE-FLAG
                   MOVE "N" TO WS-END-OF-FILE
                   MOVE 0   TO WS-COUNT

                   *> Count how many accounts exist already
                   OPEN INPUT ACCOUNT-FILE
                   PERFORM UNTIL WS-END-OF-FILE = "Y"
                      READ ACCOUNT-FILE
                          AT END MOVE "Y" TO WS-END-OF-FILE
                          NOT AT END
                              ADD 1 TO WS-COUNT
                      END-READ
                   END-PERFORM
                   CLOSE ACCOUNT-FILE

                   IF WS-COUNT >= WS-LIMIT
                      MOVE "All permitted accounts have been created, please come back later"
                         TO AM-MESSAGE
                      GOBACK
                   END-IF

                   *> Validate password
                   CALL "UTILITIES" USING AM-PASSWORD WS-VALID-FLAG
                   IF WS-VALID-FLAG NOT = "Y"
                      MOVE "Invalid password. Must be 8-12 chars, 1 uppercase, 1 digit, 1 special."
                         TO AM-MESSAGE
                      GOBACK
                   END-IF

                   *> Check for duplicate username
                   MOVE "N" TO WS-END-OF-FILE
                   OPEN INPUT ACCOUNT-FILE
                   PERFORM UNTIL WS-END-OF-FILE = "Y"
                      READ ACCOUNT-FILE
                          AT END MOVE "Y" TO WS-END-OF-FILE
                          NOT AT END
                              IF FUNCTION TRIM(ACC-USERNAME) = FUNCTION TRIM(AM-USERNAME)
                                 MOVE "Y" TO WS-DUPLICATE-FLAG
                              END-IF
                      END-READ
                   END-PERFORM
                   CLOSE ACCOUNT-FILE

                   IF WS-DUPLICATE-FLAG = "Y"
                      MOVE "Username already exists." TO AM-MESSAGE
                      GOBACK
                   END-IF

                   *> Create the account
                   OPEN EXTEND ACCOUNT-FILE
                   MOVE AM-USERNAME TO ACC-USERNAME
                   MOVE AM-PASSWORD TO ACC-PASSWORD
                   WRITE ACCOUNT-REC
                   CLOSE ACCOUNT-FILE

                   MOVE "Account created successfully." TO AM-MESSAGE

              WHEN "LOGIN"
                   CALL "LOGIN" USING AM-USERNAME AM-PASSWORD AM-MESSAGE
           END-EVALUATE
           GOBACK.
       END PROGRAM ACCOUNT-MGMT.
