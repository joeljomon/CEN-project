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
       77 WS-LINE           PIC X(500).
       77 WS-PROFILE-STATUS PIC XX.
       77 WS-END            PIC X VALUE "N".
       77 WS-FOUND          PIC X VALUE "N".
       77 WS-FIRST-NAME     PIC X(500) VALUE SPACES.
       77 WS-LAST-NAME      PIC X(500) VALUE SPACES.
       77 WS-TMP            PIC X(500) VALUE SPACES.
       77 WS-MATCH-USER     PIC X(20)  VALUE SPACES.

       LINKAGE SECTION.
       01 L-USERNAME        PIC X(20).

       PROCEDURE DIVISION USING L-USERNAME.
       MAIN-PROGRAM.
           MOVE "N" TO WS-FOUND.

           *> --- Ask for First Name ---
           MOVE "================== Search for User ===================" 
               TO WS-LINE
           PERFORM OUT

           MOVE "Enter First Name:" TO WS-LINE
           PERFORM OUT
           MOVE SPACES TO WS-FIRST-NAME
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-FIRST-NAME

           *> --- Ask for Last Name ---
           MOVE "Enter Last Name:" TO WS-LINE
           PERFORM OUT
           MOVE SPACES TO WS-LAST-NAME
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LAST-NAME

           *> --- Search profiles file ---
           OPEN INPUT PROFILE-FILE
           IF WS-PROFILE-STATUS = "00"
              MOVE "N" TO WS-END
              PERFORM UNTIL WS-END = "Y"
                 READ PROFILE-FILE
                    AT END
                       MOVE "Y" TO WS-END
                    NOT AT END
                       IF FUNCTION UPPER-CASE(
                              FUNCTION TRIM(PROF-FIRST-NAME))
                          = FUNCTION UPPER-CASE(
                              FUNCTION TRIM(WS-FIRST-NAME))
                          AND FUNCTION UPPER-CASE(
                              FUNCTION TRIM(PROF-LAST-NAME))
                          = FUNCTION UPPER-CASE(
                              FUNCTION TRIM(WS-LAST-NAME))
                          MOVE "Y" TO WS-FOUND
                          MOVE PROF-USERNAME TO WS-MATCH-USER
                          MOVE "Y" TO WS-END
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE PROFILE-FILE

           IF WS-FOUND = "Y"
              PERFORM SHOW-PROFILE
              PERFORM PROMPT-CONNECTION
           ELSE
              MOVE "! No profile found for this name." TO WS-LINE
              PERFORM OUT
           END-IF

           GOBACK.

       SHOW-PROFILE.
           MOVE SPACES TO WS-LINE
           STRING "========== Profile for: "
                  FUNCTION TRIM(PROF-FIRST-NAME) " "
                  FUNCTION TRIM(PROF-LAST-NAME)
                  " ==========" DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT

           MOVE SPACES TO WS-LINE
           STRING "> Name: "
                  FUNCTION TRIM(PROF-FIRST-NAME) " "
                  FUNCTION TRIM(PROF-LAST-NAME)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT

           MOVE SPACES TO WS-LINE
           STRING "> University: "
                  FUNCTION TRIM(PROF-UNIVERSITY)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT

           MOVE SPACES TO WS-LINE
           STRING "> Major: "
                  FUNCTION TRIM(PROF-MAJOR)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT

           MOVE SPACES TO WS-LINE
           STRING "> Graduation Year: "
                  PROF-GRAD-YEAR
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING
           PERFORM OUT

           MOVE "> About:" TO WS-LINE
           PERFORM OUT
           MOVE PROF-ABOUT TO WS-LINE
           PERFORM OUT

           MOVE "==================================" TO WS-LINE
           PERFORM OUT.

       PROMPT-CONNECTION.
           MOVE "1. Send Connection Request" TO WS-LINE
           PERFORM OUT
           MOVE "2. Back to Main Menu" TO WS-LINE
           PERFORM OUT

           MOVE SPACES TO WS-TMP
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-TMP

           IF FUNCTION TRIM(WS-TMP) = "1"
              CALL "CONNECTION-REQUEST" USING L-USERNAME WS-MATCH-USER
           END-IF.

       OUT.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM SEARCH-USER.
