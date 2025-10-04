       IDENTIFICATION DIVISION.
       PROGRAM-ID. NETWORKDISPLAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONNECTIONS-FILE ASSIGN TO "connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROFILES-FILE ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CONNECTIONS-FILE.
       01 CONNECTION-REC.
           05 USERNAME-ONE        PIC X(20).
           05 USERNAME-TWO        PIC X(20).

       FD PROFILES-FILE.
       01 PROFILE-REC.
           05 PROFILE-USERNAME    PIC X(20).
           05 PROFILE-NAME        PIC X(40).
           05 PROFILE-UNIVERSITY  PIC X(40).
           05 PROFILE-MAJOR       PIC X(40).

       WORKING-STORAGE SECTION.
       77 WS-COMMAND         PIC X(20).
       77 WS-LINE            PIC X(120).
       77 WS-CURRENT-USER    PIC X(20).
       77 WS-CONNECTED-USER  PIC X(20).
       77 WS-CONNECTED-NAME  PIC X(40).
       77 WS-CONNECTED-UNIV  PIC X(40).
       77 WS-CONNECTED-MAJOR PIC X(40).
       77 FOUND-FLAG         PIC X VALUE 'N'.
       77 WS-EOF             PIC X VALUE 'N'.

       LINKAGE SECTION.
       01 LS-USERNAME        PIC X(20).

       PROCEDURE DIVISION USING LS-USERNAME.
           MOVE LS-USERNAME TO WS-CURRENT-USER.

           MOVE "OPEN" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           MOVE "================== View My Network ==================" TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           OPEN INPUT CONNECTIONS-FILE.
           MOVE 'N' TO FOUND-FLAG.

           PERFORM UNTIL WS-EOF = 'Y'
              READ CONNECTIONS-FILE
                 AT END
                    MOVE 'Y' TO WS-EOF
                 NOT AT END
                    IF USERNAME-ONE = WS-CURRENT-USER
                       MOVE USERNAME-TWO TO WS-CONNECTED-USER
                       PERFORM DISPLAY-CONNECTION
                    ELSE
                       IF USERNAME-TWO = WS-CURRENT-USER
                          MOVE USERNAME-ONE TO WS-CONNECTED-USER
                          PERFORM DISPLAY-CONNECTION
                       END-IF
                    END-IF
              END-READ
           END-PERFORM.

           IF FOUND-FLAG = 'N'
              MOVE "You have no established connections." TO WS-LINE
              MOVE "WRITE" TO WS-COMMAND
              CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           END-IF.

           CLOSE CONNECTIONS-FILE.
           MOVE "----------------------------------------------------" TO WS-LINE
           CALL "IO-MODULE" USING "WRITE" WS-LINE

           MOVE "CLOSE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE

           GOBACK.

       DISPLAY-CONNECTION.
           MOVE 'Y' TO FOUND-FLAG.
           MOVE SPACES TO WS-CONNECTED-NAME WS-CONNECTED-UNIV WS-CONNECTED-MAJOR.
           OPEN INPUT PROFILES-FILE.
           PERFORM UNTIL EOF-PROFILE
              READ PROFILES-FILE
                 AT END
                    EXIT PERFORM
                 NOT AT END
                    IF PROFILE-USERNAME = WS-CONNECTED-USER
                       MOVE PROFILE-NAME       TO WS-CONNECTED-NAME
                       MOVE PROFILE-UNIVERSITY TO WS-CONNECTED-UNIV
                       MOVE PROFILE-MAJOR      TO WS-CONNECTED-MAJOR
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM.
           CLOSE PROFILES-FILE.

           STRING
              "Connected with: " DELIMITED BY SIZE
              WS-CONNECTED-NAME DELIMITED BY SPACE
              " (University: " DELIMITED BY SIZE
              WS-CONNECTED-UNIV DELIMITED BY SPACE
              ", Major: " DELIMITED BY SIZE
              WS-CONNECTED-MAJOR DELIMITED BY SPACE
              ")" DELIMITED BY SIZE
              INTO WS-LINE
           END-STRING

           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           EXIT.
