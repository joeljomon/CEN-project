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
    01  INPUT-REC PIC X(500).
    FD  OUTPUT-FILE.
    01  OUTPUT-REC PIC X(500).

       WORKING-STORAGE SECTION.
       77 IO-INPUT-OPENED   PIC X VALUE "N".
       77 IO-OUTPUT-OPENED  PIC X VALUE "N".

    LINKAGE SECTION.
    01 IO-COMMAND PIC X(20).
    01 IO-LINE    PIC X(500).

       PROCEDURE DIVISION USING IO-COMMAND IO-LINE.

           EVALUATE FUNCTION TRIM(IO-COMMAND)
              WHEN "OPEN"
                   IF IO-INPUT-OPENED = "N"
                       OPEN INPUT INPUT-FILE
                       MOVE "Y" TO IO-INPUT-OPENED
                   END-IF
                   IF IO-OUTPUT-OPENED = "N"
                       OPEN OUTPUT OUTPUT-FILE
                       MOVE "Y" TO IO-OUTPUT-OPENED
                   END-IF

              WHEN "CLOSE"
                   IF IO-INPUT-OPENED = "Y"
                       CLOSE INPUT-FILE
                       MOVE "N" TO IO-INPUT-OPENED
                   END-IF
                   IF IO-OUTPUT-OPENED = "Y"
                       CLOSE OUTPUT-FILE
                       MOVE "N" TO IO-OUTPUT-OPENED
                   END-IF

              WHEN "READ"
                   IF IO-INPUT-OPENED = "Y"
                       READ INPUT-FILE INTO IO-LINE
                           AT END
                               MOVE HIGH-VALUES TO IO-LINE
                       END-READ
                   ELSE
                       MOVE HIGH-VALUES TO IO-LINE
                   END-IF

              WHEN "WRITE"
                   DISPLAY FUNCTION TRIM(IO-LINE)
                   IF IO-OUTPUT-OPENED = "Y"
                       MOVE IO-LINE TO OUTPUT-REC
                       WRITE OUTPUT-REC
                   END-IF
           END-EVALUATE

           GOBACK.
       END PROGRAM IO-MODULE.
       
