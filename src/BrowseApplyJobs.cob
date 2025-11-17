IDENTIFICATION DIVISION.
       PROGRAM-ID. BROWSE-APPLY-JOBS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JOBS-FILE ASSIGN TO "data/jobs.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOBS-STATUS.

           SELECT APPLICATIONS-FILE ASSIGN TO "data/applications.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APPLY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  JOBS-FILE.
       01  JOBS-RECORD.
           05 JOB-POSTER            PIC X(20).
           05 JOB-TITLE             PIC X(50).
           05 JOB-DESCRIPTION       PIC X(200).
           05 JOB-EMPLOYER          PIC X(50).
           05 JOB-LOCATION          PIC X(50).
           05 JOB-SALARY            PIC X(20).

       FD  APPLICATIONS-FILE.
       01  APP-RECORD.
           05 JOB-APPLIER          PIC X(20).
           05 APP-JOB-TITLE         PIC X(50).
           05 APP-EMPLOYER          PIC X(50).
           05 APP-LOCATION          PIC X(50).

       WORKING-STORAGE SECTION.
       77  WS-JOBS-STATUS           PIC XX.
       77  WS-APPLY-STATUS          PIC XX. 
       77  WS-EOF                   PIC X VALUE "N".
       77  WS-JOB-COUNT             PIC 9(3) VALUE 0.
       77  WS-JOB-NUM-DISPLAY       PIC Z9.
       77  WS-COMMAND               PIC X(20).
       77  WS-LINE                  PIC X(200).
       77  WS-LOOP-COUNTER          PIC 9(3).
       77  WS-SELECTED-NUM          PIC 9(3) VALUE 0.
       77  WS-CONTINUE              PIC X VALUE 'Y'.
       77  WS-CURR-POS           PIC 9(3).
       77  WS-LINE-LENGTH            PIC 9(2) VALUE 60.
       77  WS-REMAINING-LEN         PIC 9(3).

       01  WS-JOB-TABLE.
           05 WS-JOB-ENTRY OCCURS 50 TIMES.
              10 WS-JOB-POSTER        PIC X(20).
              10 WS-JOB-TITLE         PIC X(50).
              10 WS-JOB-DESCRIPTION   PIC X(200).
              10 WS-JOB-EMPLOYER      PIC X(50).
              10 WS-JOB-LOCATION      PIC X(50).
              10 WS-JOB-SALARY        PIC X(20).

       LINKAGE SECTION.
       01  LS-USERNAME              PIC X(20).

       PROCEDURE DIVISION USING LS-USERNAME.
       MAIN-PROGRAM.
           
           PERFORM LOAD-JOBS.
           IF WS-JOB-COUNT > 0
              MOVE 'Y' TO WS-CONTINUE
              PERFORM UNTIL WS-CONTINUE = 'N'
                 MOVE "------------------ Available Jobs/Internships ------------------" TO WS-LINE
                 PERFORM WRITE-LINE
                 PERFORM DISPLAY-ALL-JOBS
                 PERFORM PROCESS-CHOICE
              END-PERFORM
           ELSE
              PERFORM PROCESS-CHOICE
           END-IF.
           GOBACK.

       LOAD-JOBS.
           MOVE 0 TO WS-JOB-COUNT.
           MOVE "N" TO WS-EOF.

           OPEN INPUT JOBS-FILE.

           IF WS-JOBS-STATUS = "00"
              READ JOBS-FILE
                  AT END MOVE 'Y' TO WS-EOF
              END-READ
              PERFORM UNTIL WS-EOF = 'Y' OR WS-JOB-COUNT >= 50
                  PERFORM CHECK-JOB
                  READ JOBS-FILE
                      AT END MOVE 'Y' TO WS-EOF
                  END-READ
              END-PERFORM
              CLOSE JOBS-FILE
           ELSE
              MOVE "Error opening jobs file." TO WS-LINE
              PERFORM WRITE-LINE
           END-IF.

           IF WS-JOB-COUNT = 0
              MOVE " " TO WS-LINE
              PERFORM WRITE-LINE
              MOVE "There are no jobs available at this time."
                   TO WS-LINE
              PERFORM WRITE-LINE
           ELSE
               CONTINUE
           END-IF.

       CHECK-JOB.
           ADD 1 TO WS-JOB-COUNT.
           MOVE JOBS-RECORD TO WS-JOB-ENTRY(WS-JOB-COUNT).

       DISPLAY-ALL-JOBS.
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 
                   UNTIL WS-LOOP-COUNTER > WS-JOB-COUNT

              MOVE WS-LOOP-COUNTER TO WS-JOB-NUM-DISPLAY
              MOVE SPACES TO WS-LINE
              STRING
                  FUNCTION TRIM(WS-JOB-NUM-DISPLAY) DELIMITED BY SIZE
                  ". " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-LOOP-COUNTER)) 
                      DELIMITED BY SIZE
                  " at " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-LOOP-COUNTER)) 
                      DELIMITED BY SIZE
                  ", (" DELIMITED BY SIZE 
                  FUNCTION TRIM(WS-JOB-LOCATION(WS-LOOP-COUNTER)) 
                      DELIMITED BY SIZE
                  ")" DELIMITED BY SIZE 
                  INTO WS-LINE
              END-STRING
              PERFORM WRITE-LINE
           END-PERFORM.

       PROCESS-CHOICE.
           MOVE " " TO WS-LINE.
           PERFORM WRITE-LINE.
           
           IF WS-JOB-COUNT > 0
               MOVE "Enter job number to view full details (or 0 to go back):"
                    TO WS-LINE
           END-IF.
           PERFORM WRITE-LINE.

           MOVE "READ" TO WS-COMMAND.
           CALL "IO-MODULE" USING WS-COMMAND, WS-LINE.

           MOVE FUNCTION NUMVAL(WS-LINE) TO WS-SELECTED-NUM.

           IF WS-SELECTED-NUM = 0
              MOVE 'N' TO WS-CONTINUE
           ELSE IF WS-SELECTED-NUM >= 1 AND WS-SELECTED-NUM <= WS-JOB-COUNT
              PERFORM DISPLAY-FULL-JOB-DETAILS
           ELSE
              MOVE "Invalid choice." TO WS-LINE
              PERFORM WRITE-LINE
           END-IF.

       DISPLAY-FULL-JOB-DETAILS.
           MOVE " " TO WS-LINE.
           PERFORM WRITE-LINE.
           MOVE "------------------ Job Details ------------------"
                TO WS-LINE.
           PERFORM WRITE-LINE.

      *> Display selected job title     
           MOVE SPACES TO WS-LINE.
           STRING "> Title: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-SELECTED-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING.
           PERFORM WRITE-LINE.
           
      *> Display selected job description, multi-line if needed
           MOVE SPACES TO WS-LINE.
           MOVE "> Description: " TO WS-LINE.
           PERFORM WRITE-LINE.
           
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-JOB-DESCRIPTION(WS-SELECTED-NUM))) 
               TO WS-REMAINING-LEN.
           MOVE 1 TO WS-CURR-POS.

           PERFORM UNTIL WS-REMAINING-LEN <= 0
              MOVE SPACES TO WS-LINE
              
              IF WS-REMAINING-LEN < WS-LINE-LENGTH
                  STRING "  " DELIMITED BY SIZE
                         WS-JOB-DESCRIPTION(WS-SELECTED-NUM)
                            (WS-CURR-POS : WS-REMAINING-LEN)
                         DELIMITED BY SIZE
                         INTO WS-LINE
                  END-STRING
                  MOVE 0 TO WS-REMAINING-LEN
              ELSE
                  STRING "  " DELIMITED BY SIZE
                         WS-JOB-DESCRIPTION(WS-SELECTED-NUM)
                            (WS-CURR-POS : WS-LINE-LENGTH)
                         DELIMITED BY SIZE
                         INTO WS-LINE
                  END-STRING
                  ADD WS-LINE-LENGTH TO WS-CURR-POS
                  SUBTRACT WS-LINE-LENGTH FROM WS-REMAINING-LEN
              END-IF
              
              PERFORM WRITE-LINE
           END-PERFORM.
           
      *> Display selected job employer
           MOVE SPACES TO WS-LINE.
           STRING "> Employer: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-SELECTED-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING.
           PERFORM WRITE-LINE.

      *> Display selected job location     
           MOVE SPACES TO WS-LINE.
           STRING "> Location: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION(WS-SELECTED-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING.
           PERFORM WRITE-LINE.

      *> Display selected job salary
           MOVE SPACES TO WS-LINE.
           STRING "> Salary: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-SALARY(WS-SELECTED-NUM)) 
                  DELIMITED BY SIZE
                  INTO WS-LINE
           END-STRING.
           PERFORM WRITE-LINE.
           
           PERFORM PROCESS-APPLICATION.


       PROCESS-APPLICATION.
           MOVE " " TO WS-LINE.   
           PERFORM WRITE-LINE.
           MOVE "Would you like to apply to this job? (Y/N):" TO WS-LINE.
           PERFORM WRITE-LINE.
           
           MOVE "READ" TO WS-COMMAND.
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.
           
           IF WS-LINE(1:1) = 'Y'
           PERFORM STORE-APPLICATION   
              MOVE SPACES TO WS-LINE
              STRING "Your application for " 
                     FUNCTION TRIM(WS-JOB-TITLE(WS-SELECTED-NUM))
                     " at "
                     FUNCTION TRIM(WS-JOB-EMPLOYER(WS-SELECTED-NUM))
                     " has been submitted successfully!"
                     DELIMITED BY SIZE
                     INTO WS-LINE
              END-STRING
              PERFORM WRITE-LINE
           END-IF.

       STORE-APPLICATION.
           OPEN EXTEND APPLICATIONS-FILE.

           IF WS-APPLY-STATUS = "00" 
              MOVE SPACES TO APP-RECORD
              MOVE LS-USERNAME TO JOB-APPLIER
              MOVE WS-JOB-TITLE(WS-SELECTED-NUM) TO APP-JOB-TITLE
              MOVE WS-JOB-EMPLOYER(WS-SELECTED-NUM) TO APP-EMPLOYER
              MOVE WS-JOB-LOCATION(WS-SELECTED-NUM) TO APP-LOCATION
              WRITE APP-RECORD
           END-IF.
           CLOSE APPLICATIONS-FILE.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND.
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM BROWSE-APPLY-JOBS.
      