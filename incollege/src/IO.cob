       IDENTIFICATION DIVISION.
       PROGRAM-ID. IO-MODULE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "data/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "data/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-REC PIC X(80).
       FD  OUTPUT-FILE.
       01  OUTPUT-REC PIC X(80).

       LINKAGE SECTION.
       01 IO-COMMAND PIC X(20).
       01 IO-LINE    PIC X(80).

       PROCEDURE DIVISION USING IO-COMMAND IO-LINE.
           EVALUATE FUNCTION TRIM(IO-COMMAND)
              WHEN "OPEN"
                   OPEN INPUT INPUT-FILE
                   OPEN OUTPUT OUTPUT-FILE

              WHEN "CLOSE"
                   CLOSE INPUT-FILE
                   CLOSE OUTPUT-FILE

              WHEN "READ"
                   READ INPUT-FILE INTO IO-LINE
                       AT END
                           MOVE HIGH-VALUES TO IO-LINE
                   END-READ

              WHEN "WRITE"
                   DISPLAY FUNCTION TRIM(IO-LINE)
                   MOVE IO-LINE TO OUTPUT-REC
                   WRITE OUTPUT-REC
           END-EVALUATE

           GOBACK.
       END PROGRAM IO-MODULE.
