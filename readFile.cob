       IDENTIFICATION DIVISION.
       PROGRAM-ID. READFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  IN-REC PIC X(80).

       WORKING-STORAGE SECTION.
       01  EOF-FLAG   PIC X VALUE 'N'.
           88  END-OF-FILE VALUE 'Y'.
           88  NOT-END-OF-FILE VALUE 'N'.

       PROCEDURE DIVISION.
           OPEN INPUT INFILE
           PERFORM UNTIL END-OF-FILE
               READ INFILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       DISPLAY IN-REC
               END-READ
           END-PERFORM
           CLOSE INFILE
           STOP RUN.
           