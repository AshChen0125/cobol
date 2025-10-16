       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHANGEFILENAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
       01 IN-REC        PIC X(100).

       FD OUT-FILE.
       01 OUT-REC       PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-LINE       PIC X(100).
       01 WS-OUT        PIC X(200).
       01 WS-CHAR       PIC X(1).
       01 WS-LEN        PIC 9(4).
       01 I             PIC 9(4).
       01 POS           PIC 9(4) VALUE 1.
       01 EOF-SW        PIC X VALUE "N".
          88 END-OF-FILE      VALUE "Y".
          88 NOT-END-OF-FILE  VALUE "N".

       PROCEDURE DIVISION.
           OPEN INPUT  IN-FILE
                OUTPUT OUT-FILE

           PERFORM UNTIL END-OF-FILE
              READ IN-FILE INTO WS-LINE
                 AT END SET END-OF-FILE TO TRUE
              NOT AT END
                 MOVE SPACES TO WS-OUT
                 MOVE 1 TO POS
                 MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE TRAILING))
                      TO WS-LEN
                 PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LEN
                    MOVE WS-LINE(I:1) TO WS-CHAR
                    STRING WS-CHAR DELIMITED BY SIZE
                           INTO WS-OUT WITH POINTER POS
                    IF I < WS-LEN
                       STRING " " DELIMITED BY SIZE
                              INTO WS-OUT WITH POINTER POS
                    END-IF
                 END-PERFORM
                 WRITE OUT-REC FROM WS-OUT
                 DISPLAY WS-OUT
              END-READ
           END-PERFORM

           CLOSE IN-FILE OUT-FILE
           STOP RUN.
