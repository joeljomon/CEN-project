       IDENTIFICATION DIVISION.
       PROGRAM-ID. IO-MODULE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
<<<<<<< HEAD
           SELECT INPUT-FILE ASSIGN TO "data/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "data/InCollege-Output.txt"
=======
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
>>>>>>> 502ab5ff803384ab8c56f0762403ee243db0e5af
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
<<<<<<< HEAD
                       AT END
                           MOVE HIGH-VALUES TO IO-LINE
=======
                       AT END MOVE "EOF" TO IO-LINE
>>>>>>> 502ab5ff803384ab8c56f0762403ee243db0e5af
                   END-READ

              WHEN "WRITE"
                   DISPLAY FUNCTION TRIM(IO-LINE)
                   MOVE IO-LINE TO OUTPUT-REC
                   WRITE OUTPUT-REC
           END-EVALUATE

           GOBACK.
<<<<<<< HEAD
       END PROGRAM IO-MODULE.
=======
           END PROGRAM IO-MODULE.
>>>>>>> 502ab5ff803384ab8c56f0762403ee243db0e5af
