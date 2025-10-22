       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-EDIT-PROFILE.

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
       77 WS-COMMAND         PIC X(20).
       77 WS-LINE            PIC X(200).
       77 WS-PROFILE-STATUS  PIC XX.
       77 WS-END-FILE        PIC X VALUE "N".
       77 WS-FOUND           PIC X VALUE "N".
       77 WS-COUNT           PIC 9(4) VALUE 0.

       77 VALID-FIELD        PIC X VALUE "N".
       77 VALID-GRAD-YEAR    PIC X VALUE "N".
       77 TMP-GRAD-YEAR-STR  PIC X(4).

       77 WS-LINE-LEN        PIC 9(3) VALUE 0.
       77 WS-ROOM            PIC 9(3) VALUE 0.
       77 WS-TMP-SPACE       PIC 9 VALUE 0.
       77 WS-LINE-PART       PIC X(200).

       01 WS-PROFILES.
          05 WS-PROFILE OCCURS 100 TIMES
             INDEXED BY IDX.
             10 WS-USERNAME        PIC X(20).
             10 WS-FIRST-NAME      PIC X(20).
             10 WS-LAST-NAME       PIC X(20).
             10 WS-UNIVERSITY      PIC X(50).
             10 WS-MAJOR           PIC X(50).
             10 WS-GRAD-YEAR       PIC 9(4).
             10 WS-ABOUT           PIC X(200).
             10 WS-EXP-TITLE       OCCURS 3 TIMES PIC X(30).
             10 WS-EXP-COMPANY     OCCURS 3 TIMES PIC X(30).
             10 WS-EXP-DATES       OCCURS 3 TIMES PIC X(30).
             10 WS-EXP-DESC        OCCURS 3 TIMES PIC X(100).
             10 WS-EDU-DEGREE      OCCURS 3 TIMES PIC X(30).
             10 WS-EDU-SCHOOL      OCCURS 3 TIMES PIC X(50).
             10 WS-EDU-YEARS       OCCURS 3 TIMES PIC X(20).

       01 WS-TEMP-PROFILE.
          05 TMP-USERNAME        PIC X(20).
          05 TMP-FIRST-NAME      PIC X(20).
          05 TMP-LAST-NAME       PIC X(20).
          05 TMP-UNIVERSITY      PIC X(50).
          05 TMP-MAJOR           PIC X(50).
          05 TMP-GRAD-YEAR       PIC 9(4).
          05 TMP-ABOUT           PIC X(200).
          05 TMP-EXP-TITLE       OCCURS 3 TIMES PIC X(30).
          05 TMP-EXP-COMPANY     OCCURS 3 TIMES PIC X(30).
          05 TMP-EXP-DATES       OCCURS 3 TIMES PIC X(30).
          05 TMP-EXP-DESC        OCCURS 3 TIMES PIC X(100).
          05 TMP-EDU-DEGREE      OCCURS 3 TIMES PIC X(30).
          05 TMP-EDU-SCHOOL      OCCURS 3 TIMES PIC X(50).
          05 TMP-EDU-YEARS       OCCURS 3 TIMES PIC X(20).

       77 EXP-COUNT             PIC 9 VALUE 0.
       77 EDU-COUNT             PIC 9 VALUE 0.
       77 IDX2                  PIC 9 VALUE 0.

       LINKAGE SECTION.
       01 CP-USERNAME PIC X(20).   *> passed in from logged-in session

       PROCEDURE DIVISION USING CP-USERNAME.
       MAIN-PROGRAM.
           MOVE CP-USERNAME TO TMP-USERNAME
           MOVE SPACES TO TMP-FIRST-NAME
           MOVE SPACES TO TMP-LAST-NAME
           MOVE SPACES TO TMP-UNIVERSITY
           MOVE SPACES TO TMP-MAJOR
           MOVE 0      TO TMP-GRAD-YEAR
           MOVE SPACES TO TMP-ABOUT

           PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
               MOVE SPACES TO TMP-EXP-TITLE(IDX2)
               MOVE SPACES TO TMP-EXP-COMPANY(IDX2)
               MOVE SPACES TO TMP-EXP-DATES(IDX2)
               MOVE SPACES TO TMP-EXP-DESC(IDX2)
               MOVE SPACES TO TMP-EDU-DEGREE(IDX2)
               MOVE SPACES TO TMP-EDU-SCHOOL(IDX2)
               MOVE SPACES TO TMP-EDU-YEARS(IDX2)
           END-PERFORM


           *> First Name (validation)
           PERFORM UNTIL VALID-FIELD = "Y"
               MOVE "Enter First Name:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               MOVE WS-LINE(1:20) TO TMP-FIRST-NAME
               IF FUNCTION LENGTH(FUNCTION TRIM(TMP-FIRST-NAME)) = 0
                   MOVE "First name cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               ELSE
                   MOVE "Y" TO VALID-FIELD
               END-IF
           END-PERFORM
           MOVE "N" TO VALID-FIELD

           *> Last Name (validation)
           PERFORM UNTIL VALID-FIELD = "Y"
               MOVE "Enter Last Name:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               MOVE WS-LINE(1:20) TO TMP-LAST-NAME
               IF FUNCTION LENGTH(FUNCTION TRIM(TMP-LAST-NAME)) = 0
                   MOVE "Last name cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               ELSE
                   MOVE "Y" TO VALID-FIELD
               END-IF
           END-PERFORM
           MOVE "N" TO VALID-FIELD

           *> University (validation)
           PERFORM UNTIL VALID-FIELD = "Y"
               MOVE "Enter University/College Attended:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               MOVE WS-LINE(1:50) TO TMP-UNIVERSITY
               IF FUNCTION LENGTH(FUNCTION TRIM(TMP-UNIVERSITY)) = 0
                   MOVE "University/College cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               ELSE
                   MOVE "Y" TO VALID-FIELD
               END-IF
           END-PERFORM
           MOVE "N" TO VALID-FIELD

           *> Major (validation)
           PERFORM UNTIL VALID-FIELD = "Y"
               MOVE "Enter Major:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               MOVE WS-LINE(1:50) TO TMP-MAJOR
               IF FUNCTION LENGTH(FUNCTION TRIM(TMP-MAJOR)) = 0
                   MOVE "Major cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               ELSE
                   MOVE "Y" TO VALID-FIELD
               END-IF
           END-PERFORM
           MOVE "N" TO VALID-FIELD

           *> Graduation Year (validation: numeric, 4-digits, 1900-2100)
           PERFORM UNTIL VALID-GRAD-YEAR = "Y"
               MOVE "Enter Graduation Year (4 digits, e.g., 2025):" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               MOVE WS-LINE(1:4) TO TMP-GRAD-YEAR-STR
               IF TMP-GRAD-YEAR-STR NUMERIC
                  AND LENGTH OF FUNCTION TRIM(TMP-GRAD-YEAR-STR) = 4
                  AND FUNCTION NUMVAL(TMP-GRAD-YEAR-STR) >= 1900
                  AND FUNCTION NUMVAL(TMP-GRAD-YEAR-STR) <= 2100
                   MOVE FUNCTION NUMVAL(TMP-GRAD-YEAR-STR) TO TMP-GRAD-YEAR
                   MOVE "Y" TO VALID-GRAD-YEAR
               ELSE
                   MOVE "Graduation year must be a 4-digit number between 1900 and 2100." TO WS-LINE
                   PERFORM WRITE-LINE
               END-IF
           END-PERFORM
           MOVE "N" TO VALID-GRAD-YEAR

           *> About Me (multi-line, up to 200 chars, terminated by DONE, blank lines skipped)
           MOVE SPACES TO TMP-ABOUT
           MOVE 0 TO WS-COUNT
           MOVE "Enter About Me. Type 'DONE' on a new line to finish (max 200 chars):" TO WS-LINE
           PERFORM WRITE-LINE

           PERFORM UNTIL WS-COUNT >= 200
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-LINE)) = "DONE"
                   EXIT PERFORM
               END-IF
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) = 0
                   CONTINUE
               ELSE
                   IF WS-COUNT > 0
                       MOVE 1 TO WS-TMP-SPACE
                   ELSE
                       MOVE 0 TO WS-TMP-SPACE
                   END-IF
                   COMPUTE WS-LINE-LEN = LENGTH OF FUNCTION TRIM(WS-LINE)
                   IF WS-COUNT + WS-TMP-SPACE + WS-LINE-LEN > 200
                       COMPUTE WS-ROOM = 200 - WS-COUNT - WS-TMP-SPACE
                       IF WS-ROOM > 0
                           IF WS-ROOM > 0
                               IF WS-COUNT > 0
                                   MOVE SPACES TO WS-LINE-PART
                                   IF WS-ROOM > 0
                                       MOVE WS-LINE(1:WS-ROOM) TO WS-LINE-PART
                                   ELSE
                                       MOVE SPACES TO WS-LINE-PART
                                   END-IF
                                   STRING
                                       TMP-ABOUT DELIMITED BY SIZE
                                       " "      DELIMITED BY SIZE
                                       WS-LINE-PART DELIMITED BY SIZE
                                       INTO TMP-ABOUT
                                   END-STRING
                               ELSE
                                   MOVE SPACES TO WS-LINE-PART
                                   IF WS-ROOM > 0
                                       MOVE WS-LINE(1:WS-ROOM) TO WS-LINE-PART
                                   ELSE
                                       MOVE SPACES TO WS-LINE-PART
                                   END-IF
                                   STRING
                                       WS-LINE-PART DELIMITED BY SIZE
                                       INTO TMP-ABOUT
                                   END-STRING
                               END-IF
                               COMPUTE WS-COUNT = 200
                           END-IF
                       END-IF
                       MOVE "About Me field reached maximum length (200 characters)." TO WS-LINE
                       PERFORM WRITE-LINE
                       EXIT PERFORM
                   ELSE
                       IF WS-COUNT > 0
                           STRING
                               TMP-ABOUT DELIMITED BY SIZE
                               " "      DELIMITED BY SIZE
                               WS-LINE  DELIMITED BY SIZE
                               INTO TMP-ABOUT
                           END-STRING
                           COMPUTE WS-COUNT = WS-COUNT + 1 + WS-LINE-LEN
                       ELSE
                           STRING
                               WS-LINE  DELIMITED BY SIZE
                               INTO TMP-ABOUT
                           END-STRING
                           COMPUTE WS-COUNT = WS-LINE-LEN
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           MOVE TMP-ABOUT TO PROF-ABOUT

           *> Experience (Optional, up to 3)
           MOVE 0 TO EXP-COUNT
           PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
               MOVE "Do you want to add an Experience entry? (Y/N):" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF WS-LINE(1:1) NOT = "Y" AND WS-LINE(1:1) NOT = "y"
                   EXIT PERFORM
               END-IF
               ADD 1 TO EXP-COUNT

               MOVE "  > Experience Title:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE(1:30))) = 0
                   MOVE SPACES TO TMP-EXP-TITLE(IDX2)
               ELSE
                   MOVE WS-LINE(1:30) TO TMP-EXP-TITLE(IDX2)
               END-IF

               MOVE "  > Company/Organization:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE(1:30))) = 0
                   MOVE SPACES TO TMP-EXP-COMPANY(IDX2)
               ELSE
                   MOVE WS-LINE(1:30) TO TMP-EXP-COMPANY(IDX2)
               END-IF

               MOVE "  > Dates (e.g., Summer 2024):" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE(1:30))) = 0
                   MOVE SPACES TO TMP-EXP-DATES(IDX2)
               ELSE
                   MOVE WS-LINE(1:30) TO TMP-EXP-DATES(IDX2)
               END-IF

               MOVE "  > Description (optional):" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE(1:100))) = 0
                   MOVE SPACES TO TMP-EXP-DESC(IDX2)
               ELSE
                   MOVE WS-LINE(1:100) TO TMP-EXP-DESC(IDX2)
               END-IF
           END-PERFORM

           *> Education (Optional, up to 3)
           MOVE 0 TO EDU-COUNT
           PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
               MOVE "Do you want to add an Education entry? (Y/N):" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF WS-LINE(1:1) NOT = "Y" AND WS-LINE(1:1) NOT = "y"
                   EXIT PERFORM
               END-IF
               ADD 1 TO EDU-COUNT

               MOVE "  > Degree:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE(1:30))) = 0
                   MOVE SPACES TO TMP-EDU-DEGREE(IDX2)
               ELSE
                   MOVE WS-LINE(1:30) TO TMP-EDU-DEGREE(IDX2)
               END-IF

               MOVE "  > University/College:" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE(1:50))) = 0
                   MOVE SPACES TO TMP-EDU-SCHOOL(IDX2)
               ELSE
                   MOVE WS-LINE(1:50) TO TMP-EDU-SCHOOL(IDX2)
               END-IF

               MOVE "  > Years Attended (e.g., 2023-2025):" TO WS-LINE
               PERFORM WRITE-LINE
               MOVE SPACES TO WS-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE(1:20))) = 0
                   MOVE SPACES TO TMP-EDU-YEARS(IDX2)
               ELSE
                   MOVE WS-LINE(1:20) TO TMP-EDU-YEARS(IDX2)
               END-IF
           END-PERFORM

           *> Load all profiles
           MOVE 0 TO WS-COUNT
           MOVE "N" TO WS-END-FILE
           MOVE "N" TO WS-FOUND
           OPEN INPUT PROFILE-FILE
           IF WS-PROFILE-STATUS = "00"
              PERFORM UNTIL WS-END-FILE = "Y"
                  READ PROFILE-FILE
                      AT END
                          MOVE "Y" TO WS-END-FILE
                      NOT AT END
                          ADD 1 TO WS-COUNT
                          MOVE PROF-USERNAME   TO WS-USERNAME(WS-COUNT)
                          MOVE PROF-FIRST-NAME TO WS-FIRST-NAME(WS-COUNT)
                          MOVE PROF-LAST-NAME  TO WS-LAST-NAME(WS-COUNT)
                          MOVE PROF-UNIVERSITY TO WS-UNIVERSITY(WS-COUNT)
                          MOVE PROF-MAJOR      TO WS-MAJOR(WS-COUNT)
                          MOVE PROF-GRAD-YEAR  TO WS-GRAD-YEAR(WS-COUNT)
                          MOVE PROF-ABOUT      TO WS-ABOUT(WS-COUNT)
                          PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                              MOVE PROF-EXP-TITLE(IDX2)    TO WS-EXP-TITLE(IDX2, WS-COUNT)
                              MOVE PROF-EXP-COMPANY(IDX2)  TO WS-EXP-COMPANY(IDX2, WS-COUNT)
                              MOVE PROF-EXP-DATES(IDX2)    TO WS-EXP-DATES(IDX2, WS-COUNT)
                              MOVE PROF-EXP-DESC(IDX2)     TO WS-EXP-DESC(IDX2, WS-COUNT)
                          END-PERFORM
                          PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                              MOVE PROF-EDU-DEGREE(IDX2)   TO WS-EDU-DEGREE(IDX2, WS-COUNT)
                              MOVE PROF-EDU-SCHOOL(IDX2)   TO WS-EDU-SCHOOL(IDX2, WS-COUNT)
                              MOVE PROF-EDU-YEARS(IDX2)    TO WS-EDU-YEARS(IDX2, WS-COUNT)
                          END-PERFORM
                  END-READ
              END-PERFORM
              CLOSE PROFILE-FILE
           END-IF

           *> Update existing or add new
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > WS-COUNT OR WS-FOUND = "Y"
               IF FUNCTION TRIM(WS-USERNAME(IDX)) =
                  FUNCTION TRIM(CP-USERNAME)
                  MOVE TMP-USERNAME   TO WS-USERNAME(IDX)
                  MOVE TMP-FIRST-NAME TO WS-FIRST-NAME(IDX)
                  MOVE TMP-LAST-NAME  TO WS-LAST-NAME(IDX)
                  MOVE TMP-UNIVERSITY TO WS-UNIVERSITY(IDX)
                  MOVE TMP-MAJOR      TO WS-MAJOR(IDX)
                  MOVE TMP-GRAD-YEAR  TO WS-GRAD-YEAR(IDX)
                  MOVE TMP-ABOUT      TO WS-ABOUT(IDX)
                  PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                      MOVE TMP-EXP-TITLE(IDX2)   TO WS-EXP-TITLE(IDX2, IDX)
                      MOVE TMP-EXP-COMPANY(IDX2) TO WS-EXP-COMPANY(IDX2, IDX)
                      MOVE TMP-EXP-DATES(IDX2)   TO WS-EXP-DATES(IDX2, IDX)
                      MOVE TMP-EXP-DESC(IDX2)    TO WS-EXP-DESC(IDX2, IDX)
                  END-PERFORM
                  PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                      MOVE TMP-EDU-DEGREE(IDX2)  TO WS-EDU-DEGREE(IDX2, IDX)
                      MOVE TMP-EDU-SCHOOL(IDX2)  TO WS-EDU-SCHOOL(IDX2, IDX)
                      MOVE TMP-EDU-YEARS(IDX2)   TO WS-EDU-YEARS(IDX2, IDX)
                  END-PERFORM
                  MOVE "Y" TO WS-FOUND
               END-IF
           END-PERFORM

           IF WS-FOUND = "N"
              ADD 1 TO WS-COUNT
              MOVE TMP-USERNAME   TO WS-USERNAME(WS-COUNT)
              MOVE TMP-FIRST-NAME TO WS-FIRST-NAME(WS-COUNT)
              MOVE TMP-LAST-NAME  TO WS-LAST-NAME(WS-COUNT)
              MOVE TMP-UNIVERSITY TO WS-UNIVERSITY(WS-COUNT)
              MOVE TMP-MAJOR      TO WS-MAJOR(WS-COUNT)
              MOVE TMP-GRAD-YEAR  TO WS-GRAD-YEAR(WS-COUNT)
              MOVE TMP-ABOUT      TO WS-ABOUT(WS-COUNT)
              PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                  MOVE TMP-EXP-TITLE(IDX2)   TO WS-EXP-TITLE(IDX2, WS-COUNT)
                  MOVE TMP-EXP-COMPANY(IDX2) TO WS-EXP-COMPANY(IDX2, WS-COUNT)
                  MOVE TMP-EXP-DATES(IDX2)   TO WS-EXP-DATES(IDX2, WS-COUNT)
                  MOVE TMP-EXP-DESC(IDX2)    TO WS-EXP-DESC(IDX2, WS-COUNT)
              END-PERFORM
              PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                  MOVE TMP-EDU-DEGREE(IDX2)  TO WS-EDU-DEGREE(IDX2, WS-COUNT)
                  MOVE TMP-EDU-SCHOOL(IDX2)  TO WS-EDU-SCHOOL(IDX2, WS-COUNT)
                  MOVE TMP-EDU-YEARS(IDX2)   TO WS-EDU-YEARS(IDX2, WS-COUNT)
              END-PERFORM
           END-IF

           *> Rewrite file with fixed-width records
           OPEN OUTPUT PROFILE-FILE
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > WS-COUNT
               MOVE SPACES TO PROFILE-REC
               MOVE WS-USERNAME(IDX)   TO PROF-USERNAME
               MOVE WS-FIRST-NAME(IDX) TO PROF-FIRST-NAME
               MOVE WS-LAST-NAME(IDX)  TO PROF-LAST-NAME
               MOVE WS-UNIVERSITY(IDX) TO PROF-UNIVERSITY
               MOVE WS-MAJOR(IDX)      TO PROF-MAJOR
               MOVE WS-GRAD-YEAR(IDX)  TO PROF-GRAD-YEAR
               MOVE WS-ABOUT(IDX)      TO PROF-ABOUT
               PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                   MOVE WS-EXP-TITLE(IDX2, IDX)   TO PROF-EXP-TITLE(IDX2)
                   MOVE WS-EXP-COMPANY(IDX2, IDX) TO PROF-EXP-COMPANY(IDX2)
                   MOVE WS-EXP-DATES(IDX2, IDX)   TO PROF-EXP-DATES(IDX2)
                   MOVE WS-EXP-DESC(IDX2, IDX)    TO PROF-EXP-DESC(IDX2)
               END-PERFORM
               PERFORM VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 3
                   MOVE WS-EDU-DEGREE(IDX2, IDX)  TO PROF-EDU-DEGREE(IDX2)
                   MOVE WS-EDU-SCHOOL(IDX2, IDX)  TO PROF-EDU-SCHOOL(IDX2)
                   MOVE WS-EDU-YEARS(IDX2, IDX)   TO PROF-EDU-YEARS(IDX2)
               END-PERFORM
               WRITE PROFILE-REC
           END-PERFORM
           CLOSE PROFILE-FILE

           MOVE "============== Profile saved successfully. ===================" TO WS-LINE
           PERFORM WRITE-LINE

           GOBACK.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       READ-LINE.
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM CREATE-EDIT-PROFILE.
       