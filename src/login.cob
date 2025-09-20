       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN.
      *> This program handles the login logic of the InCollege
      *> application. It prompts the user to enter their credentials,
      *> validates them using the accounts file, then returns the 
      *> appropriate success or failure message.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-ACCOUNTS-FILE ASSIGN TO "accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCOUNT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-ACCOUNTS-FILE
           LABEL RECORDS ARE STANDARD.
       01  USER-ACCOUNT-RECORD.
           05 USERNAME-F            PIC X(20).
           05 PASSWORD-F            PIC X(12).

       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-FILE-STATUS   PIC XX.
       01  WS-DISPLAY-LINE          PIC X(80).

       01  WS-IO-COMMUNICATION.
           05 WS-IO-COMMAND         PIC X(20) VALUE SPACES.
           05 WS-IO-LINE            PIC X(80) VALUE SPACES.

       01  WS-USER-INPUT.
           05 WS-USERNAME           PIC X(20).
           05 WS-PASSWORD           PIC X(12).
      
      *> Flags to control program flow
       01  WS-FLAGS.
           05 WS-INPUT-EOF-FLAG     PIC A(1) VALUE 'N'.
           05 WS-USERS-EOF-FLAG     PIC A(1) VALUE 'N'.
           05 WS-LOGIN-SUCCESS      PIC A(1) VALUE 'N'.

       01  WS-USER-ACCOUNTS.
           05 WS-USER-COUNT         PIC 9(1) VALUE 0.
           05 USER-TABLE.
              10 USER-ACCOUNT OCCURS 100 TIMES INDEXED BY I.
                 15 STORED-USERNAME PIC X(20).
                 15 STORED-PASSWORD PIC X(12).

       LINKAGE SECTION.
       
       01 LOGIN-USERNAME PIC X(20).
       01 LOGIN-PASSWORD PIC X(20).
       01 LOGIN-MESSAGE  PIC X(80).

       PROCEDURE DIVISION USING LOGIN-USERNAME 
                                LOGIN-PASSWORD LOGIN-MESSAGE.
      
           MAIN-PROCEDURE.
      *     display 'Inside Login Module'
      *     display 'user' login-username
      *     display 'pass' login-password
           
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-LOGIN-ROUTINE. 
           

           CALL "IO-MODULE" USING 'CLOSE' WS-IO-LINE.
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT USER-ACCOUNTS-FILE.

      *>Check if the file opened successfully
           IF WS-ACCOUNT-FILE-STATUS NOT = "00"
              MOVE "Y" TO WS-USERS-EOF-FLAG
           ELSE
              PERFORM UNTIL WS-USERS-EOF-FLAG = 'Y'
                  READ USER-ACCOUNTS-FILE
                      AT END MOVE 'Y' TO WS-USERS-EOF-FLAG
                      NOT AT END
                           ADD 1 TO WS-USER-COUNT
                           MOVE USERNAME-F TO
                                STORED-USERNAME(WS-USER-COUNT)
                           MOVE PASSWORD-F TO
                                STORED-PASSWORD(WS-USER-COUNT)
                  END-READ
              END-PERFORM
           END-IF.

           CLOSE USER-ACCOUNTS-FILE.
      *     MOVE 'OPEN' TO WS-IO-COMMAND
      *     CALL "IO-MODULE" USING WS-IO-COMMAND
      *                         WS-IO-LINE.

       
      *> Main loop for login attempts
       2000-LOGIN-ROUTINE.
      *     display 'Inside 2000-LOGIN-ROUTINE'
      
              
           MOVE 'N' TO WS-LOGIN-SUCCESS
           MOVE LOGIN-USERNAME TO WS-USERNAME
           MOVE LOGIN-PASSWORD TO WS-PASSWORD
                   PERFORM 2100-VALIDATE-CREDENTIALS
                   IF WS-LOGIN-SUCCESS = 'Y'
              MOVE "You have successfully logged in."
                         TO WS-DISPLAY-LINE
                       PERFORM 9000-DISPLAY-AND-WRITE-LINE
                   ELSE
             MOVE WS-USERNAME "Incorrect username/password, try again" 
                             TO WS-DISPLAY-LINE
                       PERFORM 9000-DISPLAY-AND-WRITE-LINE
                   END-IF
            .

            
      *> VALIDATE THE CREDENTIALS
      *>Searches the stored credentials for a matching username
      *> and password
       2100-VALIDATE-CREDENTIALS.
           MOVE 'N' TO WS-LOGIN-SUCCESS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-USER-COUNT
               IF STORED-USERNAME(I) = WS-USERNAME AND
                  STORED-PASSWORD(I) = WS-PASSWORD
      *> If a match is found, set the success flag and exit
                   MOVE 'Y' TO WS-LOGIN-SUCCESS
                   EXIT PERFORM
               END-IF
           END-PERFORM.
    

      *> Displays the message then sends to the calling program
       9000-DISPLAY-AND-WRITE-LINE.
           MOVE WS-DISPLAY-LINE TO LOGIN-MESSAGE.
      *     display WS-DISPLAY-LINE.
      *     CALL "IO-MODULE" USING 'WRITE' WS-DISPLAY-LINE.


