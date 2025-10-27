       IDENTIFICATION DIVISION.
       PROGRAM-ID. NAV-TEST.

       DATA DIVISION.
       LINKAGE SECTION.
       01 TEST-USERNAME PIC X(20).

       PROCEDURE DIVISION USING TEST-USERNAME.
           DISPLAY "Navigation module called successfully"
           DISPLAY "Username: " TEST-USERNAME
           GOBACK.
       END PROGRAM NAV-TEST.

