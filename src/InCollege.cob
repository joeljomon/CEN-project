       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-COMMAND  PIC X(20).
       77 WS-LINE     PIC X(80).
       77 WS-USERNAME PIC X(20).
       77 WS-PASSWORD PIC X(20).
       77 WS-MESSAGE  PIC X(80).
       77 WS-CHOICE   PIC 9.
       77 WS-EOF     PIC X VALUE 'N'.
       77 WS-input-error    PIC X VALUE 'N'.
       01  WS-USERNAME2.
           05 part1 pic x(12).
           05 part2 pic x(8).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE "OPEN" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

<<<<<<< HEAD
           PERFORM UNTIL WS-CHOICE = 9
               MOVE "Welcome to InCollege!" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
=======
           Perform Until WS-EOF = 'Y'

           MOVE "Welcome to InCollege!" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
>>>>>>> 502ab5ff803384ab8c56f0762403ee243db0e5af

               MOVE "1. Log In" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               MOVE "2. Create New Account" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

<<<<<<< HEAD
               MOVE "9. Exit" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               *> Read main menu choice
               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               IF WS-LINE = HIGH-VALUES
                   MOVE 9 TO WS-CHOICE
                   EXIT PERFORM
               ELSE
                   MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
               END-IF

               EVALUATE WS-CHOICE
                  WHEN 1
                       *> Username
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-USERNAME
                       END-IF

                       *> Password
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-PASSWORD
                       END-IF

                       MOVE "LOGIN" TO WS-COMMAND
                       CALL "ACCOUNT-MGMT" USING WS-COMMAND WS-USERNAME 
                       WS-PASSWORD WS-MESSAGE

                       MOVE WS-MESSAGE TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE

                      IF WS-MESSAGE = "You have successfully logged in."
                           CALL "USER-PROFILE" USING WS-USERNAME 
                           WS-PASSWORD
                       END-IF

                  WHEN 2
                       *> Username
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-USERNAME
                       END-IF

                       *> Password
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = HIGH-VALUES
                           MOVE 9 TO WS-CHOICE
                           EXIT PERFORM
                       ELSE
                           MOVE WS-LINE(1:20) TO WS-PASSWORD
                       END-IF

                       MOVE "CREATE" TO WS-COMMAND
                       CALL "ACCOUNT-MGMT" USING WS-COMMAND WS-USERNAME 
                       WS-PASSWORD WS-MESSAGE

                       MOVE WS-MESSAGE TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE

                  WHEN 9
                       CONTINUE

                  WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO 
                       WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               END-EVALUATE
           END-PERFORM
=======
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           if WS-LINE = 'EOF'
              MOVE 'Y' TO WS-EOF
              EXIT PERFORM 
           end-if
           MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE

           EVALUATE WS-CHOICE
              WHEN 1
      *Read the username from the file        
              MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                if WS-LINE = 'EOF'
              MOVE 'Y' TO WS-EOF
              EXIT PERFORM 
           end-if
           move ws-line to ws-username

      *Read the password from the file
              CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   if WS-LINE = 'EOF'
              MOVE 'Y' TO WS-EOF
              EXIT PERFORM 
           end-if
               MOVE WS-LINE TO WS-PASSWORD
     
              MOVE "LOGIN" TO WS-COMMAND
                   CALL "ACCOUNT-MGMT" USING WS-COMMAND WS-USERNAME 
                   WS-PASSWORD WS-MESSAGE
                   MOVE "WRITE" TO WS-COMMAND
                   CALL "IO-MODULE" USING WS-COMMAND WS-MESSAGE
              WHEN 2
                   move 'N' to WS-input-error
                   MOVE "READ" TO WS-COMMAND
                   CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   move ws-line to ws-username
                   if ws-username = spaces 
                      MOVE "Username cannot be blank" TO WS-LINE
                      MOVE "WRITE" TO WS-COMMAND
                      CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                      move 'Y' to WS-input-error

                      end-if
                      move WS-USERNAME to ws-username2
                      if part2 > spaces
                 MOVE "Username cannot exceed 12 characters" TO WS-LINE
                         MOVE "WRITE" TO WS-COMMAND
                         CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                        move 'Y' to WS-input-error
                      end-if
                     
                   MOVE "READ" TO WS-COMMAND
                   CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   move ws-line to WS-PASSWORD
      
                  IF WS-input-error = 'N'             
                   MOVE "CREATE" TO WS-COMMAND
                   CALL "ACCOUNT-MGMT" USING WS-COMMAND WS-USERNAME 
                           WS-PASSWORD WS-MESSAGE

                   MOVE "WRITE" TO WS-COMMAND
                   CALL "IO-MODULE" USING WS-COMMAND WS-MESSAGE
                    END-IF
              WHEN OTHER
                   MOVE "Invalid choice" TO WS-LINE
                   MOVE "WRITE" TO WS-COMMAND
                   CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           END-EVALUATE
           END-PERFORM.
>>>>>>> 502ab5ff803384ab8c56f0762403ee243db0e5af

           MOVE "CLOSE" TO WS-COMMAND.
           CALL "IO-MODULE" USING WS-COMMAND.

           STOP RUN.
<<<<<<< HEAD
       END PROGRAM INCOLLEGE.
=======
           END PROGRAM INCOLLEGE.       
    
>>>>>>> 502ab5ff803384ab8c56f0762403ee243db0e5af
