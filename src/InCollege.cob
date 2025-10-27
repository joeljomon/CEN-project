       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-COMMAND      PIC X(20).
       77 WS-LINE         PIC X(80).
       77 WS-USERNAME     PIC X(20).
       77 WS-PASSWORD     PIC X(20).
       77 WS-MESSAGE      PIC X(80).
       77 WS-CHOICE       PIC 9.
       77 WS-MENU-CHOICE  PIC 9.
       77 WS-JOB-CHOICE   PIC 9.
       77 WS-SELECTION    PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           MOVE "OPEN" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           PERFORM LOGIN-SCREEN
           
           IF WS-CHOICE = 1 AND WS-USERNAME NOT = SPACES
              PERFORM MAIN-MENU
           END-IF

           MOVE "CLOSE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           STOP RUN.

       LOGIN-SCREEN.
           MOVE "Welcome to InCollege!" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           MOVE "1. Log In" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           MOVE "2. Create New Account" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           MOVE "Enter your choice:" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE

           IF WS-CHOICE = 1
              PERFORM DO-LOGIN
           ELSE IF WS-CHOICE = 2
              PERFORM CREATE-ACCOUNT
           ELSE
              MOVE "Invalid choice" TO WS-LINE
              MOVE "WRITE" TO WS-COMMAND
              CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           END-IF.

       DO-LOGIN.
           MOVE "Please enter your username:" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-USERNAME

           MOVE "Please enter your password:" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-PASSWORD

           *> Simplified login - just accept any credentials for now
           MOVE "You have successfully logged in." TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           MOVE SPACES TO WS-LINE
           STRING "Welcome, " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-USERNAME) DELIMITED BY SIZE
                  "!" DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       CREATE-ACCOUNT.
           MOVE "Please enter your username:" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-USERNAME

           MOVE "Please enter your password:" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-PASSWORD

           CALL "ACCOUNT-MGMT" USING "CREATE" WS-USERNAME 
                                     WS-PASSWORD WS-MESSAGE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-MESSAGE.

       MAIN-MENU.
           MOVE 0 TO WS-MENU-CHOICE
           PERFORM UNTIL WS-MENU-CHOICE = 6
               MOVE "1. Search for a job" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "2. Find someone you know" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "3. Learn a new skill" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "4. View My Pending Connection Requests" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "5. View My Network" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "6. Exit" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "Enter your choice:" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-MENU-CHOICE

               EVALUATE WS-MENU-CHOICE
                   WHEN 1
                       PERFORM JOB-SEARCH-MENU
                   WHEN 2
                       MOVE "Find someone you know is under construction."
                            TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   WHEN 3
                       PERFORM SKILL-MENU
                   WHEN 4
                       MOVE "Pending Connection Requests is under construction."
                            TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   WHEN 5
                       MOVE "View My Network is under construction."
                            TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   WHEN 6
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       MOVE 0 TO WS-MENU-CHOICE
               END-EVALUATE
           END-PERFORM.

       SKILL-MENU.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = 6
               MOVE "Learn a New Skill:" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "1. Python Programming" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "2. Data Analysis" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "3. Web Development" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "4. Digital Marketing" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "5. Project Management" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "6. Go Back" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "Enter your choice:" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE

               IF WS-CHOICE >= 1 AND WS-CHOICE <= 5
                   MOVE "This skill is under construction." TO WS-LINE
                   MOVE "WRITE" TO WS-COMMAND
                   CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   MOVE 0 TO WS-CHOICE
               ELSE IF WS-CHOICE NOT = 6
                   MOVE "Invalid choice. Please try again." TO WS-LINE
                   MOVE "WRITE" TO WS-COMMAND
                   CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                   MOVE 0 TO WS-CHOICE
               END-IF
           END-PERFORM.

       JOB-SEARCH-MENU.
           MOVE 0 TO WS-JOB-CHOICE
           PERFORM UNTIL WS-JOB-CHOICE = 4
               MOVE "--- Job Search/Internship Menu ---" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "1. Post a Job/Internship" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "2. Browse Jobs/Internships" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "3. View My Applications" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "4. Back to Main Menu" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE "Enter your choice:" TO WS-LINE
               MOVE "WRITE" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE

               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-JOB-CHOICE

               EVALUATE WS-JOB-CHOICE
                   WHEN 1
                       MOVE "POST-JOB" TO WS-COMMAND
                       CALL "JOB-MGMT" USING WS-COMMAND WS-USERNAME
                                             WS-SELECTION WS-MESSAGE
                   WHEN 2
                       PERFORM BROWSE-JOBS
                   WHEN 3
                       MOVE "VIEW-APPS" TO WS-COMMAND
                       CALL "JOB-MGMT" USING WS-COMMAND WS-USERNAME
                                             WS-SELECTION WS-MESSAGE
                   WHEN 4
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-LINE
                       MOVE "WRITE" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       MOVE 0 TO WS-JOB-CHOICE
               END-EVALUATE
           END-PERFORM.

       BROWSE-JOBS.
           MOVE "BROWSE" TO WS-COMMAND
           CALL "JOB-MGMT" USING WS-COMMAND WS-USERNAME
                                 WS-SELECTION WS-MESSAGE

           IF WS-MESSAGE = "SUCCESS"
               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE FUNCTION NUMVAL(WS-LINE) TO WS-JOB-CHOICE
               
               IF WS-JOB-CHOICE NOT = 0
                   MOVE WS-JOB-CHOICE TO WS-SELECTION
                   MOVE "VIEW-DETAILS" TO WS-COMMAND
                   CALL "JOB-MGMT" USING WS-COMMAND WS-USERNAME
                                         WS-SELECTION WS-MESSAGE
                   
                   IF WS-MESSAGE = "SUCCESS"
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
                       
                       IF WS-CHOICE = 1
                           MOVE WS-JOB-CHOICE TO WS-SELECTION
                           MOVE "APPLY" TO WS-COMMAND
                           CALL "JOB-MGMT" USING WS-COMMAND WS-USERNAME
                                                 WS-SELECTION WS-MESSAGE
                           PERFORM BROWSE-JOBS
                       ELSE IF WS-CHOICE = 2
                           PERFORM BROWSE-JOBS
                       END-IF
                   END-IF
               END-IF
           END-IF.

       END PROGRAM INCOLLEGE.
