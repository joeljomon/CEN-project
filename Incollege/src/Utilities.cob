       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTILITIES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-CAPITAL PIC X VALUE "N".
       77 WS-DIGIT   PIC X VALUE "N".
       77 WS-SPECIAL PIC X VALUE "N".
       77 I          PIC 9(02).
       77 LEN        PIC 9(02).

       LINKAGE SECTION.
       01 U-PASSWORD   PIC X(20).
       01 U-VALID-FLAG PIC X(1).

       PROCEDURE DIVISION USING U-PASSWORD U-VALID-FLAG.
           MOVE "N" TO U-VALID-FLAG
           MOVE "N" TO WS-CAPITAL
           MOVE "N" TO WS-DIGIT
           MOVE "N" TO WS-SPECIAL

           MOVE FUNCTION STORED-CHAR-LENGTH(U-PASSWORD) TO LEN

           *> Length check
           IF LEN < 8 OR LEN > 12
              GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
              IF U-PASSWORD(I:1) IS ALPHABETIC-UPPER
                 MOVE "Y" TO WS-CAPITAL
              ELSE IF U-PASSWORD(I:1) >= "0" AND U-PASSWORD(I:1) <= "9"
                 MOVE "Y" TO WS-DIGIT
              ELSE IF U-PASSWORD(I:1) NOT ALPHABETIC
                   AND U-PASSWORD(I:1) NOT NUMERIC
                 MOVE "Y" TO WS-SPECIAL
              END-IF
           END-PERFORM

           IF WS-CAPITAL = "Y" AND WS-DIGIT = "Y" AND WS-SPECIAL = "Y"
              MOVE "Y" TO U-VALID-FLAG
           END-IF

           GOBACK.
       END PROGRAM UTILITIES.
       