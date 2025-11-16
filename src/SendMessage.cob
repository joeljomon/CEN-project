       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEND-MESSAGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONNECTIONS-FILE ASSIGN TO "data/connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MESSAGE-FILE ASSIGN TO "data/messages.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CONNECTIONS-FILE.
       01 CONNECTION-RECORD.
           05 CONN-USER1 PIC X(20).
           05 CONN-USER2 PIC X(20).

       FD MESSAGE-FILE.
       01 MESSAGE-RECORD.
           05 MSG-SENDER    PIC X(20).
           05 MSG-RECIPIENT PIC X(20).
           05 MSG-TEXT      PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-RECIPIENT   PIC X(20) VALUE SPACES.
       01 WS-MESSAGE     PIC X(500) VALUE SPACES.
       01 WS-CONNECTED   PIC X VALUE 'N'.
       01 WS-END-FILE    PIC X VALUE 'N'.
       01 WS-LINE        PIC X(500).
       01 WS-OUTPUT-LINE PIC X(80).
       01 WS-COMMAND     PIC X(20).
       01 WS-MSG-LEN     PIC 9(3) VALUE 0.

       LINKAGE SECTION.
       01 MSG-USERNAME PIC X(20).

       PROCEDURE DIVISION USING MSG-USERNAME.

       MAIN-PROGRAM.
           *> --- Reset all fields before each run ---
           MOVE SPACES TO WS-RECIPIENT WS-MESSAGE WS-LINE WS-OUTPUT-LINE
           MOVE 'N' TO WS-CONNECTED WS-END-FILE
           MOVE 0 TO WS-MSG-LEN

           *> --- Ask repeatedly for valid recipient ---
           PERFORM UNTIL WS-CONNECTED = 'Y'
               MOVE "Enter recipient's username (must be a connection):"
                   TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH

               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE FUNCTION TRIM(WS-LINE) TO WS-RECIPIENT

               *> --- Check if recipient is a valid connection ---
               OPEN INPUT CONNECTIONS-FILE
               MOVE 'N' TO WS-END-FILE
               MOVE 'N' TO WS-CONNECTED

               PERFORM UNTIL WS-END-FILE = 'Y'
                   READ CONNECTIONS-FILE INTO CONNECTION-RECORD
                       AT END
                           MOVE 'Y' TO WS-END-FILE
                       NOT AT END
                           IF (FUNCTION TRIM(CONN-USER1) = FUNCTION TRIM(MSG-USERNAME)
                               AND FUNCTION TRIM(CONN-USER2) = FUNCTION TRIM(WS-RECIPIENT))
                            OR (FUNCTION TRIM(CONN-USER2) = FUNCTION TRIM(MSG-USERNAME)
                               AND FUNCTION TRIM(CONN-USER1) = FUNCTION TRIM(WS-RECIPIENT))
                               MOVE 'Y' TO WS-CONNECTED
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CONNECTIONS-FILE

               IF WS-CONNECTED NOT = 'Y'
                   MOVE "User not found in your network."
                       TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM

           *> --- Ask for the message text ---
           PERFORM UNTIL WS-MSG-LEN > 0 AND WS-MSG-LEN <= 200
               MOVE "Enter your message (max 200 chars):" TO WS-OUTPUT-LINE
               PERFORM WRITE-BOTH

               MOVE SPACES TO WS-LINE
               MOVE SPACES TO WS-MESSAGE

               MOVE "READ" TO WS-COMMAND
               CALL "IO-MODULE" USING WS-COMMAND WS-LINE
               MOVE WS-LINE TO WS-MESSAGE

               *> Robust length check: determine trimmed length and also
               *> check whether any non-space character exists beyond 200th
               *> position. This prevents accepting inputs that may be
               *> truncated or mis-measured by the environment.
               COMPUTE WS-MSG-LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-MESSAGE))

               IF WS-MSG-LEN > 200 OR WS-MESSAGE(201:1) NOT = SPACE
                   MOVE "Message length exceeded. Please re-enter under 200 characters."
                       TO WS-OUTPUT-LINE
                   PERFORM WRITE-BOTH
                   MOVE 0 TO WS-MSG-LEN
               END-IF
           END-PERFORM

           *> --- Save message ---
           OPEN EXTEND MESSAGE-FILE
           MOVE MSG-USERNAME TO MSG-SENDER
           MOVE WS-RECIPIENT TO MSG-RECIPIENT
           MOVE WS-MESSAGE(1:200) TO MSG-TEXT
           WRITE MESSAGE-RECORD
           CLOSE MESSAGE-FILE

           STRING
               "Message sent to "
               FUNCTION TRIM(WS-RECIPIENT)
               " successfully!"
               DELIMITED BY SIZE
               INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM WRITE-BOTH

           GOBACK.

       WRITE-BOTH.
           MOVE WS-OUTPUT-LINE TO WS-LINE
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE.

       END PROGRAM SEND-MESSAGE.
