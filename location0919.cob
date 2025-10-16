       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDRESS-SPLITTER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "input_test.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "location_test.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LOCFILE ASSIGN TO "location.txt"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 IN-REC               PIC X(200).
       FD OUTFILE.
       01 OUT-REC              PIC X(500).
       FD LOCFILE.
       01 LOC-REC              PIC X(50).

       WORKING-STORAGE SECTION.
       01 WS-LINE              PIC X(200).
       01 WS-OUT               PIC X(500).
       01 TMP-WORD             PIC X(50).
       01 LOC-TABLE.
          05 LOC-ENTRY OCCURS 100 TIMES.
             10 LOC-NAME       PIC X(50).
       01 LOC-COUNT            PIC 9(4) VALUE 0.

       01 IDX                  PIC 9(4).
       01 JDX                  PIC 9(4).
       01 ADDR-IDX             PIC 9(4).
       01 ADDR-LEN             PIC 9(4).
       01 EOF                  PIC X VALUE "N".
       01 EOF-LOC              PIC X VALUE "N".
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM LOAD-LOCATIONS.
           OPEN INPUT INFILE
                OUTPUT OUTFILE
           PERFORM UNTIL EOF = "Y"
              READ INFILE INTO WS-LINE
                 AT END MOVE "Y" TO EOF
              NOT AT END
                 PERFORM PROCESS-LINE
                 WRITE OUT-REC FROM WS-OUT
              END-READ
           END-PERFORM
           CLOSE INFILE OUTFILE
           STOP RUN.

       LOAD-LOCATIONS.
           OPEN INPUT LOCFILE
           PERFORM UNTIL EOF-LOC = "Y"
              READ LOCFILE INTO LOC-REC
                 AT END MOVE "Y" TO EOF-LOC
              NOT AT END
                 ADD 1 TO LOC-COUNT
                 MOVE FUNCTION TRIM(LOC-REC)
                   TO LOC-NAME(LOC-COUNT)
              END-READ
           END-PERFORM
           CLOSE LOCFILE.

       PROCESS-LINE.
           MOVE WS-LINE TO WS-OUT

           *> === Location 完全比對 ===
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) TO ADDR-LEN
           MOVE 1 TO ADDR-IDX

           PERFORM UNTIL ADDR-IDX > ADDR-LEN
              UNSTRING WS-LINE
                 DELIMITED BY SPACE OR ","
                 INTO TMP-WORD
                 WITH POINTER ADDR-IDX
              END-UNSTRING

              MOVE FUNCTION TRIM(TMP-WORD) TO TMP-WORD

              PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > LOC-COUNT
                 IF TMP-WORD = LOC-NAME(JDX)
                    STRING
                       TMP-WORD DELIMITED BY SIZE
                       "|" DELIMITED BY SIZE
                       INTO WS-OUT
                    END-STRING
                 END-IF
              END-PERFORM
           END-PERFORM.
