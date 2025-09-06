IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD         PIC X(10).
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD        PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-CHOICE            PIC 9.
       01 WS-OUTPUT-LINE       PIC X(80).
       
