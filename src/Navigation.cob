       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE-NAV.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WS-CHOICE            PIC 9.
       01 WS-OUTPUT-LINE       PIC X(80).
       01 WS-SELECTION         PIC X(10).
       01 WS-MESSAGE           PIC X(80).
       01 WS-JOB-CHOICE        PIC 9.

       LINKAGE SECTION.
       01 NAV-USERNAME         PIC X(20).

       PROCEDURE DIVISION USING NAV-USERNAME.
       MAIN-PROGRAM.
           DISPLAY "DEBUG: Entered Navigation"
           MOVE 0 TO WS-CHOICE
           DISPLAY "DEBUG: Initialized WS-CHOICE"
           MOVE 0 TO WS-JOB-CHOICE
           DISPLAY "DEBUG: Initialized WS-JOB-CHOICE"
           MOVE SPACES TO WS-OUTPUT-LINE
           DISPLAY "DEBUG: Initialized WS-OUTPUT-LINE"
           MOVE SPACES TO WS-SELECTION
           MOVE SPACES TO WS-MESSAGE
           DISPLAY "DEBUG: About to enter loop"
           PERFORM UNTIL WS-CHOICE = 6
               DISPLAY "DEBUG: In loop"
               PERFORM SHOW-MENU
               PERFORM GET-VALID-MAIN-CHOICE
               PERFORM PROCESS-CHOICE
           END-PERFORM
           GOBACK.

       SHOW-MENU.
           MOVE "1. Search for a job" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "2. Find someone you know" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "3. Learn a new skill" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "4. View My Pending Connection Requests" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "5. View My Network" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "6. Exit" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM JOB-SEARCH-MENU
               WHEN 2
                   MOVE "Find someone you know is under construction."
                   TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               WHEN 3
                   PERFORM SKILL-MENU
               WHEN 4
                   MOVE "Pending Connection Requests is under construction."
                   TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               WHEN 5
                   MOVE "View My Network is under construction."
                   TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
           END-EVALUATE.

       SKILL-MENU.
           MOVE "Learn a New Skill:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Python Programming" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Data Analysis" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Web Development" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Digital Marketing" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Project Management" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Go Back" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH

           PERFORM GET-VALID-SKILL-CHOICE
           IF WS-CHOICE >= 1 AND WS-CHOICE <= 5
               MOVE "This skill is under construction." TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               PERFORM SKILL-MENU
           END-IF.

       GET-VALID-MAIN-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 6
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 6
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.

       GET-VALID-SKILL-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 6
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 6
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.

       GET-CHOICE.
           CALL "IO-MODULE" USING "READ" WS-OUTPUT-LINE
           MOVE FUNCTION NUMVAL(WS-OUTPUT-LINE(1:1)) TO WS-CHOICE.

       JOB-SEARCH-MENU.
           MOVE 0 TO WS-JOB-CHOICE
           PERFORM UNTIL WS-JOB-CHOICE = 4
               MOVE "--- Job Search/Internship Menu ---" 
                    TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "1. Post a Job/Internship" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "2. Browse Jobs/Internships" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "3. View My Applications" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "4. Back to Main Menu" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH
               MOVE "Enter your choice:" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH

               PERFORM GET-CHOICE
               MOVE WS-CHOICE TO WS-JOB-CHOICE

               EVALUATE WS-JOB-CHOICE
                   WHEN 1
                       CALL "JOB-MGMT" USING "POST-JOB" NAV-USERNAME
                                             WS-SELECTION WS-MESSAGE
                   WHEN 2
                       PERFORM BROWSE-JOBS-FLOW
                   WHEN 3
                       CALL "JOB-MGMT" USING "VIEW-APPS" NAV-USERNAME
                                             WS-SELECTION WS-MESSAGE
                   WHEN 4
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." 
                            TO WS-OUTPUT-LINE
                       PERFORM WRITE-BOTH
                       MOVE 0 TO WS-JOB-CHOICE
               END-EVALUATE
           END-PERFORM.

       BROWSE-JOBS-FLOW.
           CALL "JOB-MGMT" USING "BROWSE" NAV-USERNAME
                                 WS-SELECTION WS-MESSAGE

           IF WS-MESSAGE = "SUCCESS"
               PERFORM GET-CHOICE
               MOVE WS-CHOICE TO WS-JOB-CHOICE
               
               IF WS-JOB-CHOICE NOT = 0
                   MOVE WS-JOB-CHOICE TO WS-SELECTION
                   CALL "JOB-MGMT" USING "VIEW-DETAILS" NAV-USERNAME
                                         WS-SELECTION WS-MESSAGE
                   
                   IF WS-MESSAGE = "SUCCESS"
                       PERFORM GET-CHOICE
                       
                       IF WS-CHOICE = 1
                           MOVE WS-JOB-CHOICE TO WS-SELECTION
                           CALL "JOB-MGMT" USING "APPLY" NAV-USERNAME
                                                 WS-SELECTION WS-MESSAGE
                           PERFORM BROWSE-JOBS-FLOW
                       ELSE IF WS-CHOICE = 2
                           PERFORM BROWSE-JOBS-FLOW
                       END-IF
                   END-IF
               END-IF
           END-IF.

       WRITE-BOTH.
           DISPLAY WS-OUTPUT-LINE
           CALL "IO-MODULE" USING "WRITE" WS-OUTPUT-LINE.
      


