       IDENTIFICATION DIVISION.
       PROGRAM-ID. POST-JOB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JOBS-FILE ASSIGN TO "data/jobs.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOBS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  JOBS-FILE.
       01  JOBS-RECORD.
           05 JOB-POSTER             PIC X(20).
           05 JOB-TITLE              PIC X(50).
           05 JOB-DESCRIPTION        PIC X(200).
           05 JOB-EMPLOYER           PIC X(50).
           05 JOB-LOCATION           PIC X(50).
           05 JOB-SALARY             PIC X(20).

       WORKING-STORAGE SECTION.
       77  WS-JOBS-STATUS           PIC XX.
       77  WS-COMMAND               PIC X(20).
       77  WS-LINE                  PIC X(200).
       77  WS-IS-VALID              PIC X VALUE 'N'.

       77  WS-TEMP-DESC             PIC X(200).
       77  WS-COUNT                 PIC 9(3) VALUE 0.
       77  WS-TMP-SPACE             PIC 9 VALUE 0.
       77  WS-LINE-LEN              PIC 9(3).
       77  WS-ROOM                  PIC 9(3).

       LINKAGE SECTION.
       01  PJ-USERNAME              PIC X(20).

       PROCEDURE DIVISION USING PJ-USERNAME.
       MAIN-LOGIC.
           PERFORM GET-JOB-DETAILS.
           PERFORM SAVE-JOB-DETAILS.
           GOBACK.

       GET-JOB-DETAILS.
      
      *> Job Title validation
           MOVE "N" TO WS-IS-VALID.
           PERFORM UNTIL WS-IS-VALID = "Y"
               MOVE "Enter Job Title: " TO WS-LINE
               PERFORM WRITE-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) > 0
                   MOVE WS-LINE TO JOB-TITLE
                   MOVE "Y" TO WS-IS-VALID
               ELSE
                   MOVE "Job Title cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               END-IF
           END-PERFORM.

      *> Job Description validation, multi-line input terminated by 'DONE'
           MOVE "N" TO WS-IS-VALID.
           PERFORM UNTIL WS-IS-VALID = "Y"
               MOVE SPACES TO WS-TEMP-DESC
               MOVE 0 TO WS-COUNT
               MOVE "Enter Description. Type 'DONE' on a new line to finish (max 200 chars):"
                   TO WS-LINE
               PERFORM WRITE-LINE

               PERFORM UNTIL WS-COUNT >= 200
                   MOVE SPACES TO WS-LINE
                   PERFORM READ-LINE
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-LINE)) = "DONE"
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) > 0
                       IF WS-COUNT > 0
                           MOVE 1 TO WS-TMP-SPACE
                       ELSE
                           MOVE 0 TO WS-TMP-SPACE
                       END-IF
                       COMPUTE WS-LINE-LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-LINE))
                       IF WS-COUNT + WS-TMP-SPACE + WS-LINE-LEN > 200
                           COMPUTE WS-ROOM = 200 - WS-COUNT - WS-TMP-SPACE
                           IF WS-ROOM > 0
                               IF WS-COUNT > 0
                                   STRING WS-TEMP-DESC DELIMITED BY "  "
                                          " " DELIMITED BY SIZE
                                          WS-LINE(1:WS-ROOM) DELIMITED BY SIZE
                                          INTO WS-TEMP-DESC
                                   END-STRING
                               ELSE
                                   MOVE WS-LINE(1:WS-ROOM) TO WS-TEMP-DESC
                               END-IF
                           END-IF
                           MOVE "Description field reached maximum length."
                               TO WS-LINE
                           PERFORM WRITE-LINE
                           EXIT PERFORM
                       ELSE
                           IF WS-COUNT > 0
                               STRING WS-TEMP-DESC DELIMITED BY "  "
                                      " " DELIMITED BY SIZE
                                      FUNCTION TRIM(WS-LINE) DELIMITED BY SIZE
                                      INTO WS-TEMP-DESC
                               END-STRING
                           ELSE
                               MOVE FUNCTION TRIM(WS-LINE) TO WS-TEMP-DESC
                           END-IF
                           COMPUTE WS-COUNT =
                               FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-DESC))
                       END-IF
                   END-IF
               END-PERFORM

               IF FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-DESC)) > 0
                   MOVE "Y" TO WS-IS-VALID
               ELSE
                   MOVE "Description cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               END-IF
           END-PERFORM.
           MOVE WS-TEMP-DESC TO JOB-DESCRIPTION.

      *> Employer Name validation
              MOVE "N" TO WS-IS-VALID.
              PERFORM UNTIL WS-IS-VALID = "Y"
               MOVE "Enter Employer Name: " TO WS-LINE
               PERFORM WRITE-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) > 0
                   MOVE WS-LINE TO JOB-EMPLOYER
                   MOVE "Y" TO WS-IS-VALID
               ELSE
                   MOVE "Employer Name cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               END-IF
           END-PERFORM.

           MOVE "N" TO WS-IS-VALID.
           PERFORM UNTIL WS-IS-VALID = "Y"
               MOVE "Enter Location: " TO WS-LINE
               PERFORM WRITE-LINE
               PERFORM READ-LINE
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) > 0
                   MOVE WS-LINE TO JOB-LOCATION
                   MOVE "Y" TO WS-IS-VALID
               ELSE
                   MOVE "Location cannot be blank." TO WS-LINE
                   PERFORM WRITE-LINE
               END-IF
           END-PERFORM.

      *> Get salary, optional
           MOVE "Enter Salary (optional): "
               TO WS-LINE.
           PERFORM WRITE-LINE.
           PERFORM READ-LINE.
           MOVE WS-LINE TO JOB-SALARY.

       SAVE-JOB-DETAILS.
           OPEN EXTEND JOBS-FILE.
           MOVE PJ-USERNAME TO JOB-POSTER.
           WRITE JOBS-RECORD.
           CLOSE JOBS-FILE.
           MOVE "Your job has been posted." TO WS-LINE.
           PERFORM WRITE-LINE.

       READ-LINE.
           MOVE "READ" TO WS-COMMAND.
           CALL "IO-MODULE" USING WS-COMMAND, WS-LINE.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND.
           CALL "IO-MODULE" USING WS-COMMAND, WS-LINE.

       END PROGRAM POST-JOB.
