       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOB-MGMT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JOB-FILE ASSIGN TO "data/jobs.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOB-FILE-STATUS.
           SELECT APPLICATION-FILE ASSIGN TO "data/applications.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APP-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD JOB-FILE.
       01 JOB-REC.
          05 JOB-ID            PIC 9(03).
          05 JOB-TITLE         PIC X(40).
          05 JOB-DESCRIPTION   PIC X(100).
          05 JOB-EMPLOYER      PIC X(40).
          05 JOB-LOCATION      PIC X(40).
          05 JOB-SALARY        PIC X(20).

       FD APPLICATION-FILE.
       01 APPLICATION-REC.
          05 APP-USERNAME      PIC X(20).
          05 APP-JOB-ID        PIC 9(03).
          05 APP-JOB-TITLE     PIC X(40).
          05 APP-EMPLOYER      PIC X(40).
          05 APP-LOCATION      PIC X(40).

       WORKING-STORAGE SECTION.
       77 WS-JOB-FILE-STATUS   PIC XX.
       77 WS-APP-FILE-STATUS   PIC XX.
       77 WS-END-OF-FILE       PIC X VALUE "N".
       77 WS-JOB-COUNT         PIC 9(03) VALUE 0.
       77 WS-APP-COUNT         PIC 9(03) VALUE 0.
       77 WS-DISPLAY-LINE      PIC X(80).
       77 WS-TEMP-NUM          PIC 9(03).
       77 I                    PIC 9(03).

       01 WS-JOB-TABLE.
          05 WS-JOB OCCURS 100 TIMES.
             10 WS-JOB-ID          PIC 9(03).
             10 WS-JOB-TITLE       PIC X(40).
             10 WS-JOB-DESCRIPTION PIC X(100).
             10 WS-JOB-EMPLOYER    PIC X(40).
             10 WS-JOB-LOCATION    PIC X(40).
             10 WS-JOB-SALARY      PIC X(20).

       01 WS-IO-VARS.
          05 WS-IO-COMMAND      PIC X(20).
          05 WS-IO-LINE         PIC X(80).

       LINKAGE SECTION.
       01 JM-COMMAND           PIC X(20).
       01 JM-USERNAME          PIC X(20).
       01 JM-SELECTION         PIC X(10).
       01 JM-MESSAGE           PIC X(80).

       PROCEDURE DIVISION USING JM-COMMAND JM-USERNAME 
                                JM-SELECTION JM-MESSAGE.
           EVALUATE JM-COMMAND
              WHEN "BROWSE"
                   PERFORM BROWSE-JOBS
              WHEN "VIEW-DETAILS"
                   PERFORM VIEW-JOB-DETAILS
              WHEN "APPLY"
                   PERFORM APPLY-TO-JOB
              WHEN "VIEW-APPS"
                   PERFORM VIEW-MY-APPLICATIONS
              WHEN "POST-JOB"
                   PERFORM POST-JOB
           END-EVALUATE
           GOBACK.

       BROWSE-JOBS.
           MOVE "N" TO WS-END-OF-FILE
           MOVE 0 TO WS-JOB-COUNT

           OPEN INPUT JOB-FILE
           IF WS-JOB-FILE-STATUS NOT = "00"
              MOVE "No jobs available at this time." TO WS-DISPLAY-LINE
              MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
              CLOSE JOB-FILE
              GOBACK
           END-IF

           PERFORM UNTIL WS-END-OF-FILE = "Y"
              READ JOB-FILE
                  AT END MOVE "Y" TO WS-END-OF-FILE
                  NOT AT END
                      ADD 1 TO WS-JOB-COUNT
                      MOVE JOB-ID TO WS-JOB-ID(WS-JOB-COUNT)
                      MOVE JOB-TITLE TO WS-JOB-TITLE(WS-JOB-COUNT)
                      MOVE JOB-DESCRIPTION TO 
                           WS-JOB-DESCRIPTION(WS-JOB-COUNT)
                      MOVE JOB-EMPLOYER TO WS-JOB-EMPLOYER(WS-JOB-COUNT)
                      MOVE JOB-LOCATION TO WS-JOB-LOCATION(WS-JOB-COUNT)
                      MOVE JOB-SALARY TO WS-JOB-SALARY(WS-JOB-COUNT)
              END-READ
           END-PERFORM
           CLOSE JOB-FILE

           IF WS-JOB-COUNT = 0
              MOVE "No jobs available at this time." TO WS-DISPLAY-LINE
              MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
              GOBACK
           END-IF

           MOVE "--- Available Job Listings ---" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
           CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-JOB-COUNT
              STRING I DELIMITED BY SIZE
                     ". " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-JOB-TITLE(I)) DELIMITED BY SIZE
                     " at " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-JOB-EMPLOYER(I)) DELIMITED BY SIZE
                     " (" DELIMITED BY SIZE
                     FUNCTION TRIM(WS-JOB-LOCATION(I)) DELIMITED BY SIZE
                     ")" DELIMITED BY SIZE
                     INTO WS-DISPLAY-LINE
              END-STRING
              MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           END-PERFORM

           MOVE "-----------------------------" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
           CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           MOVE "Enter job number to view details, or 0 to go back:"
                TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
           CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           MOVE "SUCCESS" TO JM-MESSAGE.

       VIEW-JOB-DETAILS.
           MOVE FUNCTION NUMVAL(JM-SELECTION) TO WS-TEMP-NUM
           
           IF WS-TEMP-NUM < 1 OR WS-TEMP-NUM > WS-JOB-COUNT
              MOVE "Invalid job selection." TO JM-MESSAGE
              GOBACK
           END-IF

           MOVE "--- Job Details ---" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           STRING "Title: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-TEMP-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           STRING "Description: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-DESCRIPTION(WS-TEMP-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           STRING "Employer: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-TEMP-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           STRING "Location: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION(WS-TEMP-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           IF WS-JOB-SALARY(WS-TEMP-NUM) NOT = SPACES
              STRING "Salary: " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-JOB-SALARY(WS-TEMP-NUM)) 
                     DELIMITED BY SIZE
                     INTO WS-DISPLAY-LINE
              END-STRING
              MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           END-IF

           MOVE "-------------------" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           MOVE "1. Apply for this Job" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           MOVE "2. Back to Job List" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           MOVE "Enter your choice:" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           MOVE "SUCCESS" TO JM-MESSAGE.

       APPLY-TO-JOB.
           MOVE FUNCTION NUMVAL(JM-SELECTION) TO WS-TEMP-NUM
           
           IF WS-TEMP-NUM < 1 OR WS-TEMP-NUM > WS-JOB-COUNT
              MOVE "Invalid job selection." TO JM-MESSAGE
              GOBACK
           END-IF

           OPEN EXTEND APPLICATION-FILE
           MOVE JM-USERNAME TO APP-USERNAME
           MOVE WS-JOB-ID(WS-TEMP-NUM) TO APP-JOB-ID
           MOVE WS-JOB-TITLE(WS-TEMP-NUM) TO APP-JOB-TITLE
           MOVE WS-JOB-EMPLOYER(WS-TEMP-NUM) TO APP-EMPLOYER
           MOVE WS-JOB-LOCATION(WS-TEMP-NUM) TO APP-LOCATION
           WRITE APPLICATION-REC
           CLOSE APPLICATION-FILE

           STRING "Your application for " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-TEMP-NUM)) 
                  DELIMITED BY SIZE
                  " at " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-TEMP-NUM)) 
                  DELIMITED BY SIZE
                  " has been submitted." DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           MOVE "SUCCESS" TO JM-MESSAGE.

       VIEW-MY-APPLICATIONS.
           MOVE "N" TO WS-END-OF-FILE
           MOVE 0 TO WS-APP-COUNT

           MOVE "--- Your Job Applications ---" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           STRING "Application Summary for " DELIMITED BY SIZE
                  FUNCTION TRIM(JM-USERNAME) DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           MOVE "------------------------------" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           OPEN INPUT APPLICATION-FILE
           IF WS-APP-FILE-STATUS NOT = "00"
              MOVE "You have not applied to any jobs yet." 
                   TO WS-DISPLAY-LINE
              MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
              MOVE "------------------------------" TO WS-DISPLAY-LINE
              MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
              CLOSE APPLICATION-FILE
              MOVE "SUCCESS" TO JM-MESSAGE
              GOBACK
           END-IF

           PERFORM UNTIL WS-END-OF-FILE = "Y"
              READ APPLICATION-FILE
                  AT END MOVE "Y" TO WS-END-OF-FILE
                  NOT AT END
                      IF APP-USERNAME = JM-USERNAME
                         ADD 1 TO WS-APP-COUNT
                         STRING "Job Title: " DELIMITED BY SIZE
                                FUNCTION TRIM(APP-JOB-TITLE) 
                                DELIMITED BY SIZE
                                INTO WS-DISPLAY-LINE
                         END-STRING
                         MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

                         STRING "Employer: " DELIMITED BY SIZE
                                FUNCTION TRIM(APP-EMPLOYER) 
                                DELIMITED BY SIZE
                                INTO WS-DISPLAY-LINE
                         END-STRING
                         MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

                         STRING "Location: " DELIMITED BY SIZE
                                FUNCTION TRIM(APP-LOCATION) 
                                DELIMITED BY SIZE
                                INTO WS-DISPLAY-LINE
                         END-STRING
                         MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

                         MOVE "---" TO WS-DISPLAY-LINE
                         MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
                      END-IF
              END-READ
           END-PERFORM
           CLOSE APPLICATION-FILE

           IF WS-APP-COUNT = 0
              MOVE "You have not applied to any jobs yet." 
                   TO WS-DISPLAY-LINE
              MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           END-IF

           MOVE "------------------------------" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           STRING "Total Applications: " DELIMITED BY SIZE
                  WS-APP-COUNT DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           MOVE "------------------------------" TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE

           MOVE "SUCCESS" TO JM-MESSAGE.

       POST-JOB.
           MOVE "Job posting is under construction." TO WS-DISPLAY-LINE
           MOVE "WRITE" TO WS-IO-COMMAND
              CALL "IO-MODULE" USING WS-IO-COMMAND WS-DISPLAY-LINE
           MOVE "SUCCESS" TO JM-MESSAGE.

       END PROGRAM JOB-MGMT.

