       *>LOCATION跟KEYWORD會重覆比對一個字段
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitterFinal.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO 'input_test.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'test_0922.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KEY-FILE ASSIGN TO 'keyword.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LOC-FILE ASSIGN TO 'location.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC      PIC X(200).

       FD  OUT-FILE.
       01  OUT-REC     PIC X(1000).

       FD  KEY-FILE.
       01  KEY-REC     PIC X(50).

       FD  LOC-FILE.
       01  LOC-REC     PIC X(100).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG       PIC X VALUE 'N'.
       01 KEY-FILE-EOF   PIC X VALUE 'N'.
       01 LOC-FILE-EOF   PIC X VALUE 'N'.

       01 ADDR-CLEAN     PIC X(200).
       01 REMAINING      PIC X(200).
       01 ADDR-OUTPUT    PIC X(1000) VALUE SPACES.
       01 DST-POINTER    PIC 9(4) VALUE 1.

       01 IDX            PIC 9(4).
       01 KEY-COUNT      PIC 9(4) VALUE 0.
       01 LOC-COUNT      PIC 9(4) VALUE 0.

       01 TOKEN          PIC X(200) VALUE SPACES.
       01 TOKEN-LEN      PIC 9(4).
       01 REM-LEN        PIC 9(4).
       01 KEY-POS        PIC 9(4).
       01 KEY-LEN        PIC 9(4).
       01 TMP-POS        PIC 9(4).
       01 MIN-POS        PIC 9(4).
       01 MIN-LEN        PIC 9(4).
       01 FOUND          PIC X VALUE 'N'.

       01 LOC-POS        PIC 9(4).
       01 LOC-LEN        PIC 9(4).
       01 LOC-FOUND      PIC X VALUE 'N'.
       01 EARLIEST-POS   PIC 9(4).
       01 EARLIEST-LEN   PIC 9(4).
       01 EARLIEST-IDX   PIC 9(4).

       01 PART           PIC X(200).
       01 PART-LEN       PIC 9(4).
       01 FIELD-FILL     PIC X(35) VALUE SPACES.
       01 AFTER-CHAR      PIC X VALUE SPACE.

       01 KEYWORDS.
           05 KEY-ENTRY OCCURS 200 TIMES PIC X(50) VALUE SPACES.

       01 LOCATIONS.
           05 LOC-ITEM OCCURS 500 TIMES PIC X(100) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARA.
           *> 讀 KEYWORD
           OPEN INPUT KEY-FILE
           PERFORM UNTIL KEY-FILE-EOF = 'Y'
               READ KEY-FILE
                   AT END MOVE 'Y' TO KEY-FILE-EOF
                   NOT AT END
                       ADD 1 TO KEY-COUNT
                       MOVE FUNCTION TRIM(KEY-REC) TO KEY-ENTRY(
                        KEY-COUNT)
               END-READ
           END-PERFORM
           CLOSE KEY-FILE

           *> 讀 LOCATION
           OPEN INPUT LOC-FILE
           PERFORM UNTIL LOC-FILE-EOF = 'Y'
               READ LOC-FILE
                   AT END MOVE 'Y' TO LOC-FILE-EOF
                   NOT AT END
                       ADD 1 TO LOC-COUNT
                       MOVE FUNCTION TRIM(LOC-REC) TO LOC-ITEM(
                        LOC-COUNT)
               END-READ
           END-PERFORM
           CLOSE LOC-FILE

           OPEN INPUT IN-FILE OUTPUT OUT-FILE

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ IN-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       MOVE IN-REC TO ADDR-CLEAN
                       INSPECT ADDR-CLEAN REPLACING ALL ',' BY ' '
                       MOVE ADDR-CLEAN TO REMAINING
                       MOVE SPACES TO ADDR-OUTPUT
                       MOVE 1 TO DST-POINTER

                       *> KEYWORD 拆段
                       PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(
                        REMAINING)) = 0
                           MOVE 9999 TO MIN-POS
                           MOVE 0 TO MIN-LEN
                           MOVE 'N' TO FOUND
                           MOVE FUNCTION LENGTH(FUNCTION TRIM(REMAINING
                           )) TO REM-LEN

                           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
                           KEY-COUNT
                               IF FUNCTION LENGTH(FUNCTION TRIM(
                                KEY-ENTRY(IDX))) > 0
                                   MOVE FUNCTION LENGTH(FUNCTION TRIM(
                                    KEY-ENTRY(IDX))) TO KEY-LEN
                                   PERFORM VARYING TMP-POS FROM 1 BY 1
                                       UNTIL TMP-POS + KEY-LEN - 1 > 
                                       REM-LEN
                                   
                                       IF FUNCTION UPPER-CASE(REMAINING
                                       (TMP-POS:KEY-LEN)) =
                                          FUNCTION UPPER-CASE(FUNCTION 
                                          TRIM(KEY-ENTRY(IDX)))
                                   
                                           *> 安全抓 keyword 後一個字元
                                           IF TMP-POS + KEY-LEN <= 
                                           REM-LEN
                                               MOVE REMAINING(TMP-POS + 
                                               KEY-LEN:1) TO AFTER-CHAR
                                           ELSE
                                               MOVE SPACE TO AFTER-CHAR
                                           END-IF
                                   
                                           *> 判斷是否符合條件
                                           IF AFTER-CHAR = ' ' OR 
                                           AFTER-CHAR = SPACES
                                              OR TMP-POS + KEY-LEN - 1 
                                              = REM-LEN
                                              OR AFTER-CHAR = '.'
                                              OR AFTER-CHAR = ','
                                               IF TMP-POS < MIN-POS
                                                   MOVE TMP-POS TO 
                                                   MIN-POS
                                                   COMPUTE MIN-LEN = 
                                                   KEY-LEN
                                                   IF AFTER-CHAR = '.'
                                                       ADD 1 TO MIN-LEN
                                                   END-IF
                                                   MOVE 'Y' TO FOUND
                                               END-IF
                                           END-IF
                                       END-IF
                                   END-PERFORM
                               END-IF
                           END-PERFORM

                           IF FOUND = 'Y'
                               COMPUTE KEY-POS = MIN-POS + MIN-LEN - 1
                               MOVE FUNCTION TRIM(REMAINING(1:KEY-POS))
                                TO TOKEN
                               IF KEY-POS < REM-LEN
                                   MOVE FUNCTION TRIM(REMAINING(KEY-POS 
                                   + 1:)) TO REMAINING
                               ELSE
                                   MOVE SPACES TO REMAINING
                               END-IF
                           ELSE
                               MOVE FUNCTION TRIM(REMAINING) TO TOKEN
                               MOVE SPACES TO REMAINING
                           END-IF

                           *> TOKEN 依 LOCATION 拆段（僅對未含 keyword 的部分）
                           MOVE FUNCTION TRIM(TOKEN) TO PART
                           MOVE FUNCTION LENGTH(FUNCTION TRIM(PART)) TO 
                           TOKEN-LEN
                           MOVE 1 TO TMP-POS

                           PERFORM UNTIL TMP-POS > TOKEN-LEN
                               MOVE 9999 TO EARLIEST-POS
                               MOVE 0 TO EARLIEST-LEN
                               MOVE 0 TO EARLIEST-IDX
                               MOVE 'N' TO LOC-FOUND

                               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX
                                > LOC-COUNT
                                   IF FUNCTION LENGTH(FUNCTION TRIM(
                                    LOC-ITEM(IDX))) > 0
                                       MOVE FUNCTION LENGTH(FUNCTION 
                                       TRIM(LOC-ITEM(IDX))) TO LOC-LEN
                                       PERFORM VARYING LOC-POS FROM 
                                       TMP-POS BY 1 UNTIL LOC-POS + 
                                       LOC-LEN - 1 > TOKEN-LEN
                                           IF FUNCTION UPPER-CASE(PART(
                                            LOC-POS:LOC-LEN)) = FUNCTION 
                                            UPPER-CASE(FUNCTION TRIM(
                                                LOC-ITEM(IDX)))
                                              AND (LOC-POS = 1 OR PART(
                                                LOC-POS - 1:1)=SPACE)
                                              AND (LOC-POS + LOC-LEN - 1
                                               = TOKEN-LEN OR PART(
                                                LOC-POS + LOC-LEN:1)=
                                                SPACE)
                                               IF LOC-POS < EARLIEST-POS
                                                   MOVE LOC-POS TO 
                                                   EARLIEST-POS
                                                   MOVE LOC-LEN TO 
                                                   EARLIEST-LEN
                                                   MOVE IDX TO 
                                                   EARLIEST-IDX
                                                   MOVE 'Y' TO LOC-FOUND
                                                   EXIT PERFORM
                                               END-IF
                                           END-IF
                                       END-PERFORM
                                   END-IF
                               END-PERFORM

                               IF LOC-FOUND = 'Y'
                                   IF EARLIEST-POS > TMP-POS
                                       COMPUTE PART-LEN = EARLIEST-POS 
                                       - TMP-POS
                                       IF PART-LEN > 0
                                           MOVE PART(TMP-POS:PART-LEN) 
                                           TO FIELD-FILL
                                           IF FUNCTION TRIM(FIELD-FILL) 
                                           NOT = SPACES
                                               STRING FIELD-FILL 
                                               DELIMITED BY SIZE '|'
                                                DELIMITED BY SIZE
                                                      INTO ADDR-OUTPUT
                                                      WITH POINTER 
                                                      DST-POINTER
                                               END-STRING
                                           END-IF
                                       END-IF
                                   END-IF

                                   MOVE PART(EARLIEST-POS:EARLIEST-LEN)
                                    TO FIELD-FILL
                                   IF FUNCTION TRIM(FIELD-FILL) NOT = 
                                   SPACES
                                       STRING FIELD-FILL DELIMITED BY 
                                       SIZE '|' DELIMITED BY SIZE
                                              INTO ADDR-OUTPUT
                                              WITH POINTER DST-POINTER
                                       END-STRING
                                   END-IF

                                   COMPUTE TMP-POS = EARLIEST-POS + 
                                   EARLIEST-LEN
                               ELSE
                                   IF TMP-POS <= TOKEN-LEN
                                       COMPUTE PART-LEN = TOKEN-LEN - 
                                       TMP-POS + 1
                                       MOVE PART(TMP-POS:PART-LEN) TO 
                                       FIELD-FILL
                                       IF FUNCTION TRIM(FIELD-FILL) 
                                       NOT = SPACES
                                           STRING FIELD-FILL DELIMITED 
                                           BY SIZE '|' DELIMITED BY SIZE
                                                  INTO ADDR-OUTPUT
                                                  WITH POINTER 
                                                  DST-POINTER
                                           END-STRING
                                       END-IF
                                   END-IF
                                   EXIT PERFORM
                               END-IF
                           END-PERFORM
                       END-PERFORM

                       MOVE ADDR-OUTPUT TO OUT-REC
                       WRITE OUT-REC
               END-READ
           END-PERFORM

           CLOSE IN-FILE OUT-FILE
           STOP RUN.

