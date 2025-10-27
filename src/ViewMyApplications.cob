       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-MY-APPLICATIONS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APPLICATIONS-FILE ASSIGN TO "data/applications.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  APPLICATIONS-FILE.
       01  APP-RECORD.
           05 JOB-APPLIER          PIC X(20).
           05 APP-JOB-TITLE        PIC X(50).
           05 APP-EMPLOYER         PIC X(50).
           05 APP-LOCATION         PIC X(50).

       WORKING-STORAGE SECTION.
       77  WS-APP-STATUS           PIC XX.
       77  WS-EOF                  PIC X VALUE "N".
       77  WS-APP-COUNT            PIC 9(3) VALUE 0.
       77  WS-COMMAND              PIC X(20).
       77  WS-LINE                 PIC X(200).
       77  WS-COUNT-STR            PIC Z9.
       77  WS-LOOP-IDX             PIC 9(3) VALUE 0.

       01  WS-APP-TABLE.
           05 WS-APP-ENTRY OCCURS 50 TIMES.
              10 WS-APP-JOB-TITLE   PIC X(50).
              10 WS-APP-EMPLOYER    PIC X(50).
              10 WS-APP-LOCATION    PIC X(50).

       LINKAGE SECTION.
       01  LS-USERNAME             PIC X(20).

       PROCEDURE DIVISION USING LS-USERNAME.
       MAIN-PROGRAM.
           PERFORM LOAD-APPLICATIONS.
           PERFORM DISPLAY-APPLICATION-REPORT.
           GOBACK.

       LOAD-APPLICATIONS.
           MOVE 0 TO WS-APP-COUNT.
           MOVE "N" TO WS-EOF.

           OPEN INPUT APPLICATIONS-FILE.

           IF WS-APP-STATUS = "00"
              READ APPLICATIONS-FILE
                  AT END MOVE 'Y' TO WS-EOF
              END-READ
              PERFORM UNTIL WS-EOF = 'Y' OR WS-APP-COUNT >= 50
                  IF FUNCTION TRIM(JOB-APPLIER) = 
                     FUNCTION TRIM(LS-USERNAME)
                     ADD 1 TO WS-APP-COUNT
                     MOVE APP-JOB-TITLE TO 
                          WS-APP-JOB-TITLE(WS-APP-COUNT)
                     MOVE APP-EMPLOYER TO 
                          WS-APP-EMPLOYER(WS-APP-COUNT)
                     MOVE APP-LOCATION TO 
                          WS-APP-LOCATION(WS-APP-COUNT)
                  END-IF
                  READ APPLICATIONS-FILE
                      AT END MOVE 'Y' TO WS-EOF
                  END-READ
              END-PERFORM
              CLOSE APPLICATIONS-FILE
           ELSE
              MOVE "Error opening applications file." TO WS-LINE
              PERFORM WRITE-LINE
           END-IF.

       DISPLAY-APPLICATION-REPORT.
           MOVE "--- Your Job Applications ---" TO WS-LINE.
           PERFORM WRITE-LINE.
           
           MOVE SPACES TO WS-LINE.
           STRING "Application Summary for " 
                  FUNCTION TRIM(LS-USERNAME)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING.
           PERFORM WRITE-LINE.
           
           MOVE "------------------------------" TO WS-LINE.
           PERFORM WRITE-LINE.

           IF WS-APP-COUNT = 0
              MOVE " " TO WS-LINE
              PERFORM WRITE-LINE
              MOVE "You have not applied to any jobs yet." TO WS-LINE
              PERFORM WRITE-LINE
           ELSE
              PERFORM DISPLAY-EACH-APPLICATION
              PERFORM DISPLAY-TOTAL-COUNT
           END-IF.

           MOVE "------------------------------" TO WS-LINE.
           PERFORM WRITE-LINE.
           MOVE " " TO WS-LINE.
           PERFORM WRITE-LINE.

       DISPLAY-EACH-APPLICATION.
           PERFORM VARYING WS-LOOP-IDX FROM 1 BY 1 
                   UNTIL WS-LOOP-IDX > WS-APP-COUNT
              
              MOVE SPACES TO WS-LINE
              STRING "Job Title: " 
                     FUNCTION TRIM(WS-APP-JOB-TITLE(WS-LOOP-IDX))
                     DELIMITED BY SIZE
                     INTO WS-LINE
              END-STRING
              PERFORM WRITE-LINE

              MOVE SPACES TO WS-LINE
              STRING "Employer: " 
                     FUNCTION TRIM(WS-APP-EMPLOYER(WS-LOOP-IDX))
                     DELIMITED BY SIZE
                     INTO WS-LINE
              END-STRING
              PERFORM WRITE-LINE

              MOVE SPACES TO WS-LINE
              STRING "Location: " 
                     FUNCTION TRIM(WS-APP-LOCATION(WS-LOOP-IDX))
                     DELIMITED BY SIZE
                     INTO WS-LINE
              END-STRING
              PERFORM WRITE-LINE
              
              MOVE "---" TO WS-LINE
              PERFORM WRITE-LINE
           END-PERFORM.

       DISPLAY-TOTAL-COUNT.
           MOVE WS-APP-COUNT TO WS-COUNT-STR.
           MOVE SPACES TO WS-LINE.
           STRING "Total Applications: " 
                  FUNCTION TRIM(WS-COUNT-STR)
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING.
           PERFORM WRITE-LINE.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND.
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM VIEW-MY-APPLICATIONS.

