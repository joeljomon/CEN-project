IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE-NAV.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WS-CHOICE      PIC 9 VALUE 0.
       01 WS-OUTPUT-LINE PIC X(80).
       01 WS-COMMAND     PIC X(20).
       01 WS-LINE        PIC X(80).
       01 WS-USERNAME    PIC X(20).

       LINKAGE SECTION.
       01 NAV-USERNAME PIC X(20).

       PROCEDURE DIVISION USING NAV-USERNAME.
       MAIN-PROGRAM.
           MOVE NAV-USERNAME TO WS-USERNAME
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = 7
               PERFORM SHOW-MENU
               PERFORM GET-VALID-MAIN-CHOICE
               PERFORM PROCESS-CHOICE
           END-PERFORM
           GOBACK.

       SHOW-MENU.
           MOVE "Main Menu:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "1. Create/Edit User Profile" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "2. View User Profile" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "3. Search for User"  TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "4. Learn a new skill" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "5. View My Pending Connection Requests" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "6. View My Network" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "7. Log Out" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH
           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM WRITE-BOTH.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE "You chose to create or edit your profile." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH

                   MOVE "================== Create/Edit Profile ==================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH

                   CALL "CREATE-EDIT-PROFILE" USING WS-USERNAME

               WHEN 2
                   MOVE "You Chose to view your profile." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "VIEW-PROFILE" USING WS-USERNAME

               WHEN 3
                   MOVE "You chose to search for a user." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   MOVE "================== Search for User ===================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH

                   CALL "SEARCH-USER" USING WS-USERNAME

               WHEN 4
                   MOVE "You chose to learn a new skill." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   MOVE "================== Learn a New Skill ===================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "SKILL-MENU"

               WHEN 5
                   MOVE "You chose to view pending requests." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   MOVE "================= Pending Requests =================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "VIEW-PENDING-REQUESTS" USING WS-USERNAME
               
               WHEN 6
                   MOVE "You chose to view your network." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   MOVE "================= My Network =================" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   CALL "NetworkDisplay" USING WS-USERNAME

               WHEN 7
                   MOVE "Logging out. Goodbye!" TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH

           WHEN OTHER
               CONTINUE
       END-EVALUATE.


       GET-VALID-MAIN-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 7
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 7
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.

       GET-CHOICE.
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           IF WS-LINE = HIGH-VALUES
               MOVE 7 TO WS-CHOICE
           ELSE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
           END-IF.

       WRITE-BOTH.
           MOVE WS-OUTPUT-LINE TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.
