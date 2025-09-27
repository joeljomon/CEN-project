       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECTION-REQUEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONNECTION-FILE ASSIGN TO "data/connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONN-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD CONNECTION-FILE.
       01 CONNECTION-REC.
           05 CONN-SENDER-USERNAME      PIC X(15).
           05 CONN-RECEIVER-USERNAME    PIC X(15).
           05 CONN-RECEIVER-FULLNAME    PIC X(40).

       WORKING-STORAGE SECTION.
       77 WS-CONN-STATUS       PIC XX.
       77 WS-LINE              PIC X(200).
       77 WS-COMMAND           PIC X(20).
       77 WS-INPUT             PIC X(80).
       77 WS-CHOICE            PIC 9.

       LINKAGE SECTION.
       01 L-SENDER-USERNAME        PIC X(20).
       01 L-RECEIVER-USERNAME      PIC X(20).
       01 L-RECEIVER-FULLNAME      PIC X(30).

       PROCEDURE DIVISION USING
            L-SENDER-USERNAME
            L-RECEIVER-USERNAME
            L-RECEIVER-FULLNAME.

       MAIN-PROGRAM.
           PERFORM DISPLAY-MENU
           GOBACK.

       DISPLAY-MENU.
           MOVE "1. Send Connection Request" TO WS-LINE
           PERFORM WRITE-LINE
           MOVE "2. Back to Main Menu" TO WS-LINE
           PERFORM WRITE-LINE
           MOVE "Enter your choice:" TO WS-LINE
           PERFORM WRITE-LINE

           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-INPUT
           MOVE FUNCTION NUMVAL(WS-INPUT) TO WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM SEND-REQUEST
                   PERFORM DISPLAY-MENU
               WHEN 2
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice." TO WS-LINE
                   PERFORM WRITE-LINE
           END-EVALUATE.

       SEND-REQUEST.
           OPEN EXTEND CONNECTION-FILE
           IF WS-CONN-STATUS = "00"
               MOVE L-SENDER-USERNAME   TO CONN-SENDER-USERNAME
               MOVE L-RECEIVER-USERNAME TO CONN-RECEIVER-USERNAME
               MOVE L-RECEIVER-FULLNAME TO CONN-RECEIVER-FULLNAME
               WRITE CONNECTION-REC
               CLOSE CONNECTION-FILE

               STRING "Connection request sent to "
                      FUNCTION TRIM(L-RECEIVER-FULLNAME)
                      INTO WS-LINE
               PERFORM WRITE-LINE
           ELSE
               MOVE "Error: Cannot open connection file."
                    TO WS-LINE
               PERFORM WRITE-LINE
           END-IF.

       WRITE-LINE.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM CONNECTION-REQUEST.
