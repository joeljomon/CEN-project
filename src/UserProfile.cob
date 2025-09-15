       IDENTIFICATION DIVISION.
       PROGRAM-ID. USER-PROFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "data/temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE.
       01 ACCOUNT-REC.
          05 ACC-USERNAME PIC X(20).
          05 ACC-PASSWORD PIC X(20).
          05 ACC-OTHER-INFO PIC X(5000).
       FD TEMP-FILE.
       01 TEMP-REC.
            05 TEMP-USERNAME PIC X(20).
            05 TEMP-PASSWORD PIC X(20).
            05 WS-TEMP-OTHER-INFO PIC X(5000).

       WORKING-STORAGE SECTION.
       01 WS-COPY-OF-FILE.
          05 WS-USERNAME PIC X(20).
          05 WS-PASSWORD PIC X(20).
          05 WS-OTHER-INFO.
           10 WS-FD-FIRSTNAME PIC X(20).
           10 WS-FD-LASTNAME PIC X(20).
           10 WS-FD-UNIVERSITY PIC X(20).
           10 WS-FD-MAJOR PIC X(20).
           10 WS-FD-GRADYEAR PIC X(4).
           10 WS-FD-ABOUT-ME PIC X(80).
           10 WS-FD-EXPERIENCE-1.
                 15 WS-FD-TITLE-1 PIC X(20).
                 15 WS-FD-COMPANY-1 PIC X(20).
                 15 WS-FD-DATES-1 PIC X(20).
                 15 WS-FD-DESC-1 PIC X(80).
           10 WS-FD-EXPERIENCE-2.
               15 WS-FD-TITLE-2 PIC X(20).
               15 WS-FD-COMPANY-2 PIC X(20).
               15 WS-FD-DATES-2 PIC X(20).
               15 WS-FD-DESC-2 PIC X(80).
           10 WS-FD-EXPERIENCE-3.
               15 WS-FD-TITLE-3 PIC X(20).
               15 WS-FD-COMPANY-3 PIC X(20).
               15 WS-FD-DATES-3 PIC X(20).
               15 WS-FD-DESC-3 PIC X(80).       

           10 WS-FD-EDUCATION-1.
               15 WS-FD-DEGREE-1 PIC X(20).
               15 WS-FD-UNIV-1 PIC X(20).
               15 WS-FD-YEARS-1 PIC X(20).
           10 WS-FD-EDUCATION-2.
               15 WS-FD-DEGREE-2 PIC X(20).
               15 WS-FD-UNIV-2 PIC X(20).
               15 WS-FD-YEARS-2 PIC X(20).
           10 WS-FD-EDUCATION-3.
               15 WS-FD-DEGREE-3 PIC X(20).
               15 WS-FD-UNIV-3 PIC X(20).
               15 WS-FD-YEARS-3 PIC X(20).



       77 WS-END-OF-FILE PIC X.
       77 WS-COUNT PIC 9 VALUE 0.
       77 WS-REC-COUNT PIC 9 VALUE 0.
       77 WS-COMMAND  PIC X(20).
       77 WS-LINE     PIC X(80).
       77 WS-CHOICE   PIC 9.
       77 WS-FIELD-VALID PIC X VALUE 'N'.
       77 WS-FIRSTNAME PIC X(20).
       77 WS-LASTNAME PIC X(20).
       77 WS-UNIVERSITY PIC X(20).
       77 WS-MAJOR PIC X(20).
       77 WS-GRADYEAR PIC X(4).
       77 WS-ABOUT-ME PIC X(80).
       77 WS-TOTAL-EXP PIC 9.
       77 WS-TITLE PIC X(20).
       77 WS-COMPANY PIC X(20).
       77 WS-DATES PIC X(20).
       77 WS-DESC PIC X(80).
       01 WS-EXPERIENCE-1.
           10 WS-TITLE-1 PIC X(20).
           10 WS-COMPANY-1 PIC X(20).
           10 WS-DATES-1 PIC X(20).
           10 WS-DESC-1 PIC X(80).
       01 WS-EXPERIENCE-2.
           10 WS-TITLE-2 PIC X(20).
           10 WS-COMPANY-2 PIC X(20).
           10 WS-DATES-2 PIC X(20).
           10 WS-DESC-2 PIC X(80).
       01 WS-EXPERIENCE-3.
           10 WS-TITLE-3 PIC X(20).
           10 WS-COMPANY-3 PIC X(20).
           10 WS-DATES-3 PIC X(20).
           10 WS-DESC-3 PIC X(80).       

       01 WS-EDUCATION-1.
           10 WS-DEGREE-1 PIC X(20).
           10 WS-UNIV-1 PIC X(20).
           10 WS-YEARS-1 PIC X(20).
       01 WS-EDUCATION-2.
           10 WS-DEGREE-2 PIC X(20).
           10 WS-UNIV-2 PIC X(20).
           10 WS-YEARS-2 PIC X(20).
       01 WS-EDUCATION-3.
           10 WS-DEGREE-3 PIC X(20).
           10 WS-UNIV-3 PIC X(20).
           10 WS-YEARS-3 PIC X(20).
       77 WS-TOTAL-EDU PIC 9.
       77 WS-DEGREE PIC X(20).
       77 WS-UNIV PIC X(20).
       77 WS-YEARS PIC X(20).  
       

       LINKAGE SECTION.
       01 LINKAGE-USERNAME PIC X(20).
       01 LINKAGE-PASSWORD PIC X(20). 

       PROCEDURE DIVISION USING LINKAGE-USERNAME LINKAGE-PASSWORD.
       MAIN-PROGRAM.

           OPEN OUTPUT TEMP-FILE
           PERFORM UNTIL WS-CHOICE = 4
           OPEN INPUT ACCOUNT-FILE
               PERFORM SHOW-MENU
               PERFORM GET-VALID-MAIN-CHOICE
               IF WS-CHOICE <> 4
                  PERFORM PROCESS-CHOICE
               END-IF
               PERFORM UPDATE-FILE
           END-PERFORM
    *> CALL UPDATE-FILE AFTER THE LOOP, THEN CLOSE AND GOBACK.
           CLOSE TEMP-FILE
           GOBACK.

       SHOW-MENU.
    
               MOVE "User Profile Menu" TO WS-LINE
               PERFORM WRITE-BOTH
        
               MOVE "1. Create Profile" TO WS-LINE
               PERFORM WRITE-BOTH

               MOVE "2. Edit Profile" TO WS-LINE
               PERFORM WRITE-BOTH

               MOVE "3. View Profile" TO WS-LINE
               PERFORM WRITE-BOTH

               MOVE "4. Return to Main Menu" TO WS-LINE
               PERFORM WRITE-BOTH.

      
       GET-VALID-MAIN-CHOICE.
           MOVE 0 TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE >= 1 AND WS-CHOICE <= 4
               PERFORM GET-CHOICE
               IF WS-CHOICE < 1 OR WS-CHOICE > 4
                   MOVE "Invalid choice. Please try again." TO 
                   WS-LINE
                   PERFORM WRITE-BOTH
               END-IF
           END-PERFORM.
       GET-CHOICE.
           MOVE "READ" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           IF WS-LINE = HIGH-VALUES
               MOVE 4 TO WS-CHOICE
           ELSE
               MOVE FUNCTION NUMVAL(WS-LINE(1:1)) TO WS-CHOICE
           END-IF.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN 1
               PERFORM UNTIL WS-FIELD-VALID = 'Y'
                   MOVE "First Name (Required): " TO WS-LINE
                   PERFORM WRITE-BOTH
                   *> First name
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           IF WS-LINE IS ALPHABETIC
                           MOVE WS-LINE(1:20) TO WS-FIRSTNAME
                            WS-FD-FIRSTNAME
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                           ELSE 
                               MOVE "Invalid entry." TO WS-LINE
                               PERFORM WRITE-BOTH
                           END-IF
                       END-IF
               END-PERFORM

               MOVE 'N' TO WS-FIELD-VALID
               PERFORM UNTIL WS-FIELD-VALID = 'Y'
                   MOVE "Last Name (Required): " TO WS-LINE
                   PERFORM WRITE-BOTH
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           IF WS-LINE IS ALPHABETIC
                           MOVE WS-LINE(1:20) TO WS-LASTNAME
                            WS-FD-LASTNAME
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                           ELSE 
                               MOVE "Invalid entry." TO WS-LINE
                               PERFORM WRITE-BOTH
                           END-IF
                       END-IF
                 END-PERFORM
                  
               MOVE 'N' TO WS-FIELD-VALID
               PERFORM UNTIL WS-FIELD-VALID = 'Y'
                   MOVE "University/College Attended (Required): " 
                   TO WS-LINE
                   PERFORM WRITE-BOTH
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           IF WS-LINE IS ALPHABETIC
                           MOVE WS-LINE(1:20) TO WS-UNIVERSITY
                           WS-FD-UNIVERSITY
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                           ELSE 
                               MOVE "Invalid entry." TO WS-LINE
                               PERFORM WRITE-BOTH
                           END-IF
                       END-IF
               END-PERFORM

               MOVE 'N' TO WS-FIELD-VALID
               PERFORM UNTIL WS-FIELD-VALID = 'Y'                
                   MOVE "Major (Required): " TO WS-LINE
                   PERFORM WRITE-BOTH
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           IF WS-LINE IS ALPHABETIC
                           MOVE WS-LINE(1:20) TO WS-MAJOR
                           WS-FD-MAJOR
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                           ELSE 
                               MOVE "Invalid entry." TO WS-LINE
                               PERFORM WRITE-BOTH
                           END-IF
                       END-IF
                END-PERFORM

               MOVE 'N' TO WS-FIELD-VALID
               PERFORM UNTIL WS-FIELD-VALID = 'Y'  
                   MOVE "Graduation Year (Required): " TO WS-LINE
                   PERFORM WRITE-BOTH
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                           ELSE 
                        IF FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) not=4
                           
             MOVE "Invalid entry. Enter a valid 4 digit year" TO WS-LINE
                           PERFORM WRITE-BOTH
                           ELSE
                           IF WS-LINE(1:4) IS not NUMERIC
                               MOVE "Invalid. Enter a numeric value." 
                               TO WS-LINE
                               PERFORM WRITE-BOTH
                           ELSE
                               MOVE WS-LINE TO WS-GRADYEAR 
                               WS-FD-GRADYEAR
                               MOVE 'Y' TO WS-FIELD-VALID
                               EXIT PERFORM
                           END-IF
                         END-IF
                       END-IF
               END-PERFORM                               

                   
                   MOVE "About Me (Optional): " TO WS-LINE
                   PERFORM WRITE-BOTH
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE   
           
           
           MOVE 'N' TO WS-FIELD-VALID

               PERFORM UNTIL WS-FIELD-VALID = 'Y' 
              MOVE "Enter the number of job experiences you would like"& 
                    " to enter (0-3)" TO WS-LINE
                    PERFORM WRITE-BOTH
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                           ELSE 
                    if ws-line(1:1) not numeric  
                     MOVE "Entry is not numeric" TO WS-LINE
                           PERFORM WRITE-BOTH
                    else
                       move ws-line(1:1) to WS-TOTAL-EXP
                       if WS-TOTAL-EXP > 3
                         MOVE "Enter a value between 0 and 3" TO WS-LINE
                           PERFORM WRITE-BOTH
                       else
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM                   
                            END-IF
                         END-IF
                       END-IF
               END-PERFORM   
               if WS-TOTAL-EXP > 0
           
           

           STRING "Experience: add up to "       DELIMITED BY SIZE
               WS-TOTAL-EXP                   DELIMITED BY SIZE
               " entries in the following format " DELIMITED BY SIZE
                  INTO WS-LINE
                   PERFORM WRITE-BOTH
                   
                   MOVE "Title (e.g., Software Intern)" TO WS-LINE
                   PERFORM WRITE-BOTH
                   MOVE "Company/Organization (e.g., Tech" & 
                   "Solutions Inc.)" TO WS-LINE
                   PERFORM WRITE-BOTH
                   MOVE "Dates (e.g., Summer 2024 or Jan 2023" &
                   "- May 2024)" TO WS-LINE
                   PERFORM WRITE-BOTH
                   MOVE "Description (Optional, responsibilities"&
                   "/achievements)" TO WS-LINE
                   PERFORM WRITE-BOTH
                  
           PERFORM VARYING WS-COUNT FROM 1 BY 1
           UNTIL ws-count > WS-TOTAL-EXP
           
           MOVE 'N' TO WS-FIELD-VALID
              PERFORM UNTIL WS-FIELD-VALID = 'Y'
                       MOVE "Enter Job Title:" TO WS-LINE
                       PERFORM WRITE-BOTH            
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           MOVE WS-LINE(1:20) TO WS-TITLE
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                      END-IF
                END-PERFORM
                
                MOVE 'N' TO WS-FIELD-VALID
                PERFORM UNTIL WS-FIELD-VALID = 'Y' 
                       MOVE "Enter Company:" TO WS-LINE
                       PERFORM WRITE-BOTH            
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "Company name is required" TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           MOVE WS-LINE(1:20) TO WS-COMPANY
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                      END-IF
                END-PERFORM
               MOVE 'N' TO WS-FIELD-VALID
            PERFORM UNTIL WS-FIELD-VALID = 'Y'
                       MOVE "Enter Dates:" TO WS-LINE
                       PERFORM WRITE-BOTH        
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                           ELSE 
                               MOVE WS-LINE TO WS-DATES
                               MOVE 'Y' TO WS-FIELD-VALID
                               EXIT PERFORM
                       END-IF
               END-PERFORM  

     
                       MOVE "Enter Job Description (Optional):"  
                       TO WS-LINE
                       PERFORM WRITE-BOTH             
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       MOVE WS-LINE(1:20) TO WS-DESC

           EVALUATE WS-COUNT
               WHEN 1
                   MOVE WS-TITLE TO WS-TITLE-1 WS-FD-TITLE-1
                   MOVE WS-COMPANY TO WS-COMPANY-1 WS-FD-COMPANY-1
                   MOVE WS-DATES TO WS-DATES-1 WS-FD-DATES-1
                   MOVE WS-DESC TO WS-DESC-1 WS-FD-DESC-1
               WHEN 2
                   MOVE WS-TITLE TO WS-TITLE-2 WS-FD-TITLE-2
                   MOVE WS-COMPANY TO WS-COMPANY-2 WS-FD-COMPANY-2
                   MOVE WS-DATES TO WS-DATES-2 WS-FD-DATES-2
                   MOVE WS-DESC TO WS-DESC-2 WS-FD-DESC-2
               WHEN 3
                   MOVE WS-TITLE TO WS-TITLE-3 WS-FD-TITLE-3
                   MOVE WS-COMPANY TO WS-COMPANY-3 WS-FD-COMPANY-3
                   MOVE WS-DATES TO WS-DATES-3 WS-FD-DATES-3
                   MOVE WS-DESC TO WS-DESC-3 WS-FD-DESC-3
               end-evaluate
               
               END-PERFORM
               end-if   

           MOVE 'N' TO WS-FIELD-VALID

               PERFORM UNTIL WS-FIELD-VALID = 'Y'
              MOVE "Enter the number of educational background "&
              "entries you would like to enter (0-3)" TO WS-LINE
                       PERFORM WRITE-BOTH
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                           ELSE 
                    if ws-line(1:1) not numeric  
                     MOVE "Entry is not numeric" TO WS-LINE
                           PERFORM WRITE-BOTH
                    else
                       move ws-line(1:1) to WS-TOTAL-EDU
                       if WS-TOTAL-EDU > 3
                         MOVE "Enter a value between 0 and 3" TO WS-LINE
                           PERFORM WRITE-BOTH
                       else
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM                   
                            END-IF
                         END-IF
                       END-IF
               END-PERFORM   
               if WS-TOTAL-EDU > 0
           
           

           STRING "Experience: add up to "       DELIMITED BY SIZE
               WS-TOTAL-EDU                   DELIMITED BY SIZE
           " entries in the following format " DELIMITED BY SIZE
            INTO WS-LINE
                   PERFORM WRITE-BOTH
                   
                   MOVE "Degree (e.g., Master of Science)" TO WS-LINE
                   PERFORM WRITE-BOTH
                   MOVE "University/College (e.g., State University)" 
                   TO WS-LINE
                   PERFORM WRITE-BOTH
                   MOVE "Years Attended (e.g., 2023-2025)" TO WS-LINE
                   PERFORM WRITE-BOTH
                  
           PERFORM VARYING WS-COUNT FROM 1 BY 1
           UNTIL WS-COUNT > WS-TOTAL-EDU
           
           MOVE 'N' TO WS-FIELD-VALID
              PERFORM UNTIL WS-FIELD-VALID = 'Y'
                       MOVE "Enter Degree:" TO WS-LINE
                       PERFORM WRITE-BOTH              
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           MOVE WS-LINE(1:20) TO WS-DEGREE
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                      END-IF
                END-PERFORM
                
                MOVE 'N' TO WS-FIELD-VALID
                PERFORM UNTIL WS-FIELD-VALID = 'Y' 
                       MOVE "Enter University/College:" TO WS-LINE
                       PERFORM WRITE-BOTH            
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required" TO WS-LINE
                           PERFORM WRITE-BOTH
                       ELSE 
                           MOVE WS-LINE(1:20) TO WS-UNIV
                           MOVE 'Y' TO WS-FIELD-VALID
                           EXIT PERFORM
                      END-IF
                END-PERFORM

            MOVE 'N' TO WS-FIELD-VALID
            PERFORM UNTIL WS-FIELD-VALID = 'Y'  
                       MOVE "Enter Years Attended:" TO WS-LINE
                       PERFORM WRITE-BOTH              
                       MOVE "READ" TO WS-COMMAND
                       CALL "IO-MODULE" USING WS-COMMAND WS-LINE
                       IF WS-LINE = SPACES
                           MOVE "This field is required." TO WS-LINE
                           PERFORM WRITE-BOTH
                           ELSE 
                               MOVE WS-LINE TO WS-YEARS
                               MOVE 'Y' TO WS-FIELD-VALID
                               EXIT PERFORM
                       END-IF
               END-PERFORM  

      
           EVALUATE WS-COUNT
               WHEN 1
                   MOVE WS-DEGREE TO WS-DEGREE-1 WS-FD-DEGREE-1
                   MOVE WS-UNIV TO WS-UNIV-1 WS-FD-UNIV-1
                   MOVE WS-YEARS TO WS-YEARS-1 WS-FD-YEARS-1
               WHEN 2
                   MOVE WS-DEGREE TO WS-DEGREE-2 WS-FD-DEGREE-2
                   MOVE WS-UNIV TO WS-UNIV-2 WS-FD-UNIV-2
                   MOVE WS-YEARS TO WS-YEARS-2 WS-FD-YEARS-2
               WHEN 3
                   MOVE WS-DEGREE TO WS-DEGREE-3 WS-FD-DEGREE-3
                   MOVE WS-UNIV TO WS-UNIV-3 WS-FD-UNIV-3
                   MOVE WS-YEARS TO WS-YEARS-3 WS-FD-YEARS-3
               end-evaluate
               
               END-PERFORM
               end-if                  
               WHEN 2
                   MOVE "Find someone you know is under construction."
                       TO WS-LINE
                   PERFORM WRITE-BOTH
      *>         WHEN 3
      *>             PERFORM SKILL-MENU
           END-EVALUATE.
       WRITE-BOTH.
           MOVE "WRITE" TO WS-COMMAND
           CALL "IO-MODULE" USING WS-COMMAND WS-LINE
           MOVE SPACES TO WS-LINE.

       UPDATE-FILE.
           MOVE 'N' TO WS-END-OF-FILE
           MOVE 0 TO WS-REC-COUNT
           PERFORM UNTIL WS-END-OF-FILE = "Y"
                      READ ACCOUNT-FILE
                          AT END MOVE "Y" TO WS-END-OF-FILE
                          NOT AT END
                              MOVE ACC-USERNAME TO TEMP-USERNAME
                              MOVE ACC-PASSWORD TO TEMP-PASSWORD
                              ADD 1 TO WS-REC-COUNT

      *> CHECK IF THE CURRENT RECORD IS THE LOGGED-IN USER[cite: 124].
                       IF ACC-USERNAME = LINKAGE-USERNAME AND
                          ACC-PASSWORD = LINKAGE-PASSWORD
      *> IF IT MATCHES, WRITE THE NEW PROFILE DATA FROM WORKING-STORAGE.
                           MOVE WS-COPY-OF-FILE TO ACC-OTHER-INFO
                           WRITE TEMP-REC FROM ACCOUNT-REC
                       ELSE
      *> OTHERWISE, WRITE THE ORIGINAL RECORD UNCHANGED.
                           WRITE TEMP-REC FROM ACCOUNT-REC
                       END-IF
                              
                      END-READ
           END-PERFORM.
              CLOSE ACCOUNT-FILE.
              CLOSE TEMP-FILE.
      *> THIS SECTION REPLACES THE OLD FILE WITH THE NEWLY CREATED ONE.
           OPEN OUTPUT ACCOUNT-FILE
           OPEN INPUT TEMP-FILE

           MOVE 'N' TO WS-END-OF-FILE
           PERFORM UNTIL WS-END-OF-FILE = 'Y'
               READ TEMP-FILE INTO ACCOUNT-REC
                   AT END MOVE 'Y' TO WS-END-OF-FILE
                   NOT AT END
                       WRITE ACCOUNT-REC
               END-READ
           END-PERFORM

           CLOSE TEMP-FILE.
              CLOSE ACCOUNT-FILE.


       
       