       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-USER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROFILE-FILE ASSIGN TO "data/profiles.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PROFILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD PROFILE-FILE.
       01 PROFILE-REC.
          05 PROF-USERNAME        PIC X(20).
          05 PROF-FIRST-NAME      PIC X(20).
          05 PROF-LAST-NAME       PIC X(20).
          05 PROF-UNIVERSITY      PIC X(50).
          05 PROF-MAJOR           PIC X(50).
          05 PROF-GRAD-YEAR       PIC 9(4).
          05 PROF-ABOUT           PIC X(200).
          05 PROF-EXPERIENCE      OCCURS 3 TIMES.
             10 PROF-EXP-TITLE    PIC X(30).
             10 PROF-EXP-COMPANY  PIC X(30).
             10 PROF-EXP-DATES    PIC X(30).
             10 PROF-EXP-DESC     PIC X(100).
          05 PROF-EDUCATION       OCCURS 3 TIMES.
             10 PROF-EDU-DEGREE   PIC X(30).
             10 PROF-EDU-SCHOOL   PIC X(50).
             10 PROF-EDU-YEARS    PIC X(20).

       WORKING-STORAGE SECTION.
       77 WS-COMMAND        PIC X(20).
       77 WS-LINE           PIC X(200).
       77 WS-PROFILE-STATUS PIC XX.
       77 WS-END-FILE       PIC X VALUE "N".
       77 WS-FOUND          PIC X VALUE "N".
       77 WS-INPUT          PIC X(80).
       77 WS-MATCH-USERNAME PIC X(20).

       77 WS-SEARCH-FIRSTNAME PIC X(20).
       77 WS-SEARCH-LASTNAME  PIC X(20).
       77 WS-TMP-FIRST-NAME   PIC X(20).
       77 WS-TMP-LAST-NAME    PIC X(20).

       77 WS-LINK-RECEIVER-NAME PIC X(40).

       LINKAGE SECTION.
       01 L-SENDER-USERNAME PIC X(20).

       PROCEDURE DIVISION USING L-SENDER-USERNAME.

       MAIN-PROGRAM.
           MOVE SPACES TO WS-LINE
           MOVE "Enter the user's first name:" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-INPUT
           MOVE FUNCTION TRIM(WS-INPUT) TO WS-SEARCH-FIRSTNAME

           MOVE SPACES TO WS-LINE
           MOVE "Enter the user's last name:" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-INPUT
           MOVE FUNCTION TRIM(WS-INPUT) TO WS-SEARCH-LASTNAME

           MOVE "N" TO WS-END-FILE
           MOVE "N" TO WS-FOUND

           OPEN INPUT PROFILE-FILE

           IF WS-PROFILE-STATUS = "00"
              PERFORM UNTIL WS-END-FILE = "Y"
                  READ PROFILE-FILE
                      AT END
                          MOVE "Y" TO WS-END-FILE
                      NOT AT END
                          MOVE FUNCTION TRIM(PROF-FIRST-NAME) TO WS-TMP-FIRST-NAME
                          MOVE FUNCTION TRIM(PROF-LAST-NAME) TO WS-TMP-LAST-NAME
                          IF FUNCTION UPPER-CASE(WS-TMP-FIRST-NAME) = FUNCTION UPPER-CASE(WS-SEARCH-FIRSTNAME)
                             AND FUNCTION UPPER-CASE(WS-TMP-LAST-NAME) = FUNCTION UPPER-CASE(WS-SEARCH-LASTNAME)
                              MOVE "Y" TO WS-FOUND
                              MOVE PROF-USERNAME TO WS-MATCH-USERNAME
                              MOVE "Y" TO WS-END-FILE  *> Stop after first match
                          END-IF
                  END-READ
              END-PERFORM
              CLOSE PROFILE-FILE
           END-IF

           IF WS-FOUND = "Y"
               CALL "VIEW-PROFILE" USING WS-MATCH-USERNAME

               *> Build receiver full name for request
               STRING PROF-FIRST-NAME DELIMITED BY SPACE
                      " " DELIMITED BY SIZE
                      PROF-LAST-NAME  DELIMITED BY SIZE
                      INTO WS-LINK-RECEIVER-NAME
               END-STRING

               CALL "CONNECTION-REQUEST"
                   USING L-SENDER-USERNAME   *> sender from login
                         WS-MATCH-USERNAME   *> receiver username
                         WS-LINK-RECEIVER-NAME *> receiver full name
           ELSE
              MOVE SPACES TO WS-LINE
              MOVE "! No profile found for this name." TO WS-LINE
              PERFORM WRITE-LINE
           END-IF

           GOBACK.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM SEARCH-USER.
