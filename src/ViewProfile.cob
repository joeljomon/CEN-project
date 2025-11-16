 IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-PROFILE.

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
       77 WS-END-FILE       PIC X VALUE "N".
       77 WS-FOUND          PIC X VALUE "N".
       77 WS-IDX            PIC 9 VALUE 0.

       77 WS-TMP-FIRST-NAME      PIC X(20).
       77 WS-TMP-LAST-NAME       PIC X(20).
       77 WS-TMP-UNIVERSITY      PIC X(50).
       77 WS-TMP-MAJOR           PIC X(50).
       77 WS-TMP-GRAD-YEAR       PIC 9(4).
       77 WS-TMP-ABOUT           PIC X(200).
       77 WS-TMP-EXP-TITLE       PIC X(30).
       77 WS-TMP-EXP-COMPANY     PIC X(30).
       77 WS-TMP-EXP-DATES       PIC X(30).
       77 WS-TMP-EXP-DESC        PIC X(100).
       77 WS-TMP-EDU-DEGREE      PIC X(30).
       77 WS-TMP-EDU-SCHOOL      PIC X(50).
       77 WS-TMP-EDU-YEARS       PIC X(20).

       LINKAGE SECTION.
       01 VP-USERNAME PIC X(20).   *> passed in from logged-in session

       PROCEDURE DIVISION USING VP-USERNAME.
       MAIN-PROGRAM.
           MOVE "N" TO WS-END-FILE
           MOVE "N" TO WS-FOUND
           OPEN INPUT PROFILE-FILE

           IF WS-PROFILE-STATUS = "00"
              PERFORM UNTIL WS-END-FILE = "Y"
                  READ PROFILE-FILE
                      AT END
                          MOVE "Y" TO WS-END-FILE
                      NOT AT END
                          IF FUNCTION TRIM(PROF-USERNAME) = FUNCTION TRIM(VP-USERNAME)
                              MOVE "Y" TO WS-FOUND

                              MOVE SPACES TO WS-LINE
                              STRING "========== Profile for: " DELIMITED BY SIZE
                                     FUNCTION TRIM(PROF-FIRST-NAME) DELIMITED BY SIZE
                                     " " DELIMITED BY SIZE
                                     FUNCTION TRIM(PROF-LAST-NAME) DELIMITED BY SIZE
                                     " ==========" DELIMITED BY SIZE
                                     INTO WS-LINE
                              END-STRING
                              PERFORM WRITE-LINE

                              MOVE PROF-FIRST-NAME TO WS-TMP-FIRST-NAME
                              MOVE PROF-LAST-NAME TO WS-TMP-LAST-NAME
                              MOVE PROF-UNIVERSITY TO WS-TMP-UNIVERSITY
                              MOVE PROF-MAJOR TO WS-TMP-MAJOR
                              MOVE PROF-GRAD-YEAR TO WS-TMP-GRAD-YEAR
                              MOVE PROF-ABOUT TO WS-TMP-ABOUT

                              MOVE SPACES TO WS-LINE
                              STRING "> Name: " DELIMITED BY SIZE
                                     FUNCTION TRIM(WS-TMP-FIRST-NAME) DELIMITED BY SIZE
                                     " " DELIMITED BY SIZE
                                     FUNCTION TRIM(WS-TMP-LAST-NAME) DELIMITED BY SIZE
                                     INTO WS-LINE
                              END-STRING
                              PERFORM WRITE-LINE

                              MOVE SPACES TO WS-LINE
                              STRING "> University: " DELIMITED BY SIZE
                                     FUNCTION TRIM(WS-TMP-UNIVERSITY) DELIMITED BY SIZE
                                     INTO WS-LINE
                              END-STRING
                              PERFORM WRITE-LINE

                              MOVE SPACES TO WS-LINE
                              STRING "> Major: " DELIMITED BY SIZE
                                     FUNCTION TRIM(WS-TMP-MAJOR) DELIMITED BY SIZE
                                     INTO WS-LINE
                              END-STRING
                              PERFORM WRITE-LINE

                              MOVE SPACES TO WS-LINE
                              STRING "> Graduation Year: " DELIMITED BY SIZE
                                     WS-TMP-GRAD-YEAR DELIMITED BY SIZE
                                     INTO WS-LINE
                              END-STRING
                              PERFORM WRITE-LINE

                              MOVE SPACES TO WS-LINE
                              MOVE "> About: " TO WS-LINE
                              PERFORM WRITE-LINE

                              MOVE WS-TMP-ABOUT TO WS-LINE
                              PERFORM WRITE-LINE

                              MOVE "> Experience:" TO WS-LINE
                              PERFORM WRITE-LINE
                              PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 3
                                  MOVE PROF-EXP-TITLE(WS-IDX) TO WS-TMP-EXP-TITLE
                                  IF FUNCTION LENGTH(FUNCTION TRIM(WS-TMP-EXP-TITLE)) > 0
                                     MOVE PROF-EXP-COMPANY(WS-IDX) TO WS-TMP-EXP-COMPANY
                                     MOVE PROF-EXP-DATES(WS-IDX)   TO WS-TMP-EXP-DATES
                                     MOVE PROF-EXP-DESC(WS-IDX)    TO WS-TMP-EXP-DESC

                                     MOVE SPACES TO WS-LINE
                                     STRING ">>    Title: " DELIMITED BY SIZE
                                            FUNCTION TRIM(WS-TMP-EXP-TITLE) DELIMITED BY SIZE
                                            INTO WS-LINE
                                     END-STRING
                                     PERFORM WRITE-LINE

                                     MOVE SPACES TO WS-LINE
                                     STRING ">>       Company: " DELIMITED BY SIZE
                                            FUNCTION TRIM(WS-TMP-EXP-COMPANY) DELIMITED BY SIZE
                                            INTO WS-LINE
                                     END-STRING
                                     PERFORM WRITE-LINE

                                     MOVE SPACES TO WS-LINE
                                     STRING ">>       Dates: " DELIMITED BY SIZE
                                            FUNCTION TRIM(WS-TMP-EXP-DATES) DELIMITED BY SIZE
                                            INTO WS-LINE
                                     END-STRING
                                     PERFORM WRITE-LINE

                                     MOVE SPACES TO WS-LINE
                                     STRING ">>       Description: " DELIMITED BY SIZE
                                            FUNCTION TRIM(WS-TMP-EXP-DESC) DELIMITED BY SIZE
                                            INTO WS-LINE
                                     END-STRING
                                     PERFORM WRITE-LINE
                                  END-IF
                              END-PERFORM

                              MOVE "> Education:" TO WS-LINE
                              PERFORM WRITE-LINE
                              PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 3
                                  MOVE PROF-EDU-DEGREE(WS-IDX) TO WS-TMP-EDU-DEGREE
                                  IF FUNCTION LENGTH(FUNCTION TRIM(WS-TMP-EDU-DEGREE)) > 0
                                     MOVE PROF-EDU-SCHOOL(WS-IDX) TO WS-TMP-EDU-SCHOOL
                                     MOVE PROF-EDU-YEARS(WS-IDX)  TO WS-TMP-EDU-YEARS

                                     MOVE SPACES TO WS-LINE
                                     STRING ">>    Degree: " DELIMITED BY SIZE
                                            FUNCTION TRIM(WS-TMP-EDU-DEGREE) DELIMITED BY SIZE
                                            INTO WS-LINE
                                     END-STRING
                                     PERFORM WRITE-LINE

                                     MOVE SPACES TO WS-LINE
                                     STRING ">>       School: " DELIMITED BY SIZE
                                            FUNCTION TRIM(WS-TMP-EDU-SCHOOL) DELIMITED BY SIZE
                                            INTO WS-LINE
                                     END-STRING
                                     PERFORM WRITE-LINE

                                     MOVE SPACES TO WS-LINE
                                     STRING ">>       Years: " DELIMITED BY SIZE
                                            FUNCTION TRIM(WS-TMP-EDU-YEARS) DELIMITED BY SIZE
                                            INTO WS-LINE
                                     END-STRING
                                     PERFORM WRITE-LINE
                                  END-IF
                              END-PERFORM

                              MOVE SPACES TO WS-LINE
                              MOVE "==================================" TO WS-LINE
                              PERFORM WRITE-LINE

                              MOVE "Y" TO WS-END-FILE  *> stop after first match
                          END-IF
                  END-READ
              END-PERFORM
              CLOSE PROFILE-FILE
           END-IF

           IF WS-FOUND = "N"
              MOVE SPACES TO WS-LINE
              MOVE "! No profile found for this user." TO WS-LINE
              PERFORM WRITE-LINE
           END-IF

           GOBACK.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.
       END PROGRAM VIEW-PROFILE.

