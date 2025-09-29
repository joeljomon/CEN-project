       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-PENDING-REQUESTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PENDING-FILE ASSIGN TO "data/pending.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PEND-STATUS.
           SELECT PROFILE-FILE ASSIGN TO "data/profiles.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PROF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PENDING-FILE.
       01  PENDING-REC.
           05 P-SENDER         PIC X(20).
           05 FILLER           PIC X VALUE '|'.
           05 P-RECIP          PIC X(20).
           05 FILLER           PIC X VALUE '|'.
           05 P-TS             PIC X(19).

       FD  PROFILE-FILE.
       01  PROFILE-REC.
           05 PROF-USERNAME        PIC X(20).
           05 PROF-FIRST-NAME      PIC X(20).
           05 PROF-LAST-NAME       PIC X(20).
           05 PROF-UNIVERSITY      PIC X(50).
           05 PROF-MAJOR           PIC X(50).
           05 PROF-GRAD-YEAR       PIC 9(4).
           05 PROF-ABOUT           PIC X(200).
           05 PROF-EXPERIENCE      OCCURS 3 TIMES.
              10 PROF-EXP-TITLE    PIC X(30).
              10 PROF-EXP-COMPANY  PIC X(30).
              10 PROF-EXP-DATES    PIC X(30).
              10 PROF-EXP-DESC     PIC X(100).
           05 PROF-EDUCATION       OCCURS 3 TIMES.
              10 PROF-EDU-DEGREE   PIC X(30).
              10 PROF-EDU-SCHOOL   PIC X(50).
              10 PROF-EDU-YEARS    PIC X(20).

       WORKING-STORAGE SECTION.
       77 WS-PEND-STATUS      PIC XX.
       77 WS-PROF-STATUS      PIC XX.

       77 WS-CMD              PIC X(20).
       77 WS-LINE             PIC X(200).

       77 WS-END              PIC X VALUE "N".
       77 WS-FOUND            PIC X VALUE "N".

       *> Small in-memory table of profile names for quick lookup
       01  WS-PROFILE-TABLE.
           05 WS-PROFILE-COUNT      PIC 9(3) VALUE 0.
           05 WS-PROFILE-ROW OCCURS 200 TIMES
                INDEXED BY PX.
              10 WS-P-USERNAME      PIC X(20).
              10 WS-P-FIRST         PIC X(20).
              10 WS-P-LAST          PIC X(20).

       77  LOOK-USER           PIC X(20).
       77  LOOK-FIRST          PIC X(20).
       77  LOOK-LAST           PIC X(20).

       LINKAGE SECTION.
       01 VR-USERNAME PIC X(20).  *> currently logged-in user (recipient)

       PROCEDURE DIVISION USING VR-USERNAME.

       MAIN-PROGRAM.
           PERFORM WRITE-HEADER
           PERFORM LOAD-PROFILES
           PERFORM LIST-PENDING-FOR-USER
           GOBACK.

       WRITE-HEADER.
           MOVE "---- Pending Connection Requests ----" TO WS-LINE
           PERFORM OUT.

       LOAD-PROFILES.
           MOVE 0 TO WS-PROFILE-COUNT
           OPEN INPUT PROFILE-FILE
           IF WS-PROF-STATUS = "00"
              MOVE "N" TO WS-END
              PERFORM UNTIL WS-END = "Y"
                 READ PROFILE-FILE
                    AT END
                       MOVE "Y" TO WS-END
                    NOT AT END
                       IF WS-PROFILE-COUNT < 200
                          ADD 1 TO WS-PROFILE-COUNT
                          MOVE PROF-USERNAME   TO WS-P-USERNAME(WS-PROFILE-COUNT)
                          MOVE PROF-FIRST-NAME TO WS-P-FIRST   (WS-PROFILE-COUNT)
                          MOVE PROF-LAST-NAME  TO WS-P-LAST    (WS-PROFILE-COUNT)
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE PROFILE-FILE.

       LIST-PENDING-FOR-USER.
           MOVE "N" TO WS-FOUND
           OPEN INPUT PENDING-FILE
           IF WS-PEND-STATUS = "00"
              MOVE "N" TO WS-END
              PERFORM UNTIL WS-END = "Y"
                 READ PENDING-FILE
                    AT END
                       MOVE "Y" TO WS-END
                    NOT AT END
                       IF FUNCTION TRIM(P-RECIP) =
                          FUNCTION TRIM(VR-USERNAME)
                          MOVE "Y" TO WS-FOUND
                          MOVE P-SENDER TO LOOK-USER
                          PERFORM LOOKUP-NAME
                          IF FUNCTION LENGTH(FUNCTION TRIM(LOOK-FIRST)) > 0
                               STRING "- "
                                      FUNCTION TRIM(LOOK-FIRST) DELIMITED BY SIZE
                                      " " DELIMITED BY SIZE
                                      FUNCTION TRIM(LOOK-LAST)  DELIMITED BY SIZE
                                      "  (" DELIMITED BY SIZE
                                      FUNCTION TRIM(LOOK-USER)  DELIMITED BY SIZE
                                      ")" DELIMITED BY SIZE
                                      INTO WS-LINE
                               END-STRING
                          ELSE
                               STRING "- " FUNCTION TRIM(LOOK-USER)
                                      INTO WS-LINE
                               END-STRING
                          END-IF
                          PERFORM OUT
                       END-IF
                 END-READ
              END-PERFORM
           END-IF
           CLOSE PENDING-FILE

           IF WS-FOUND NOT = "Y"
              MOVE "You have no pending connection requests at this time." TO WS-LINE
              PERFORM OUT
           END-IF.

       LOOKUP-NAME.
           MOVE SPACES TO LOOK-FIRST LOOK-LAST
           IF WS-PROFILE-COUNT > 0
              PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > WS-PROFILE-COUNT
                 IF FUNCTION TRIM(WS-P-USERNAME(PX)) =
                    FUNCTION TRIM(LOOK-USER)
                    MOVE WS-P-FIRST(PX) TO LOOK-FIRST
                    MOVE WS-P-LAST(PX)  TO LOOK-LAST
                    EXIT PERFORM
                 END-IF
              END-PERFORM
           END-IF.

       OUT.
           MOVE "WRITE" TO WS-CMD
           CALL "IO-MODULE" USING WS-CMD WS-LINE.
