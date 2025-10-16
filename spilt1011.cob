       *>新增輸出不符合的資料
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitterFinal.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO 'input_test.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'test_1011.csv' 
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KEY-FILE ASSIGN TO 'keyword.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LOC-FILE ASSIGN TO 'location.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNMATCH-FILE ASSIGN TO 'unmatched.txt'
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

       FD  UNMATCH-FILE.
       01  UNMATCH-REC PIC X(200).

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
       01 AFTER-CHAR     PIC X VALUE SPACE.

       01 LOC-POS        PIC 9(4).
       01 LOC-LEN        PIC 9(4).
       01 LOC-FOUND      PIC X VALUE 'N'.
       01 EARLIEST-POS   PIC 9(4).
       01 EARLIEST-LEN   PIC 9(4).
       01 EARLIEST-IDX   PIC 9(4).
       01 TEMP-FIELD     PIC X(35).

       01 PART           PIC X(200).
       01 PART-LEN       PIC 9(4).
       01 FIELD-FILL     PIC X(200).

       01 PREFIX-LEN     PIC 9(4) VALUE 0.
       01 SUB-LEN        PIC 9(4) VALUE 0.
       01 REMAIN-LEN     PIC 9(4) VALUE 0.
       01 FILL-START-IDX PIC 9(4) VALUE 0.

       01 HAS-MATCH      PIC X VALUE 'N'.

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
                       MOVE FUNCTION TRIM(KEY-REC) TO 
                       KEY-ENTRY(KEY-COUNT)
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
                       MOVE FUNCTION TRIM(LOC-REC) TO 
                       LOC-ITEM(LOC-COUNT)
               END-READ
           END-PERFORM
           CLOSE LOC-FILE

           OPEN INPUT IN-FILE OUTPUT OUT-FILE UNMATCH-FILE
            *> CSV 首行
           MOVE SPACES TO OUT-REC
           STRING "RM;F;ALY;LN/Lane;Sec;Street/St;Avenue/Ave;Way;"
                  "Boulevard/Blvd;Road/Rd;Drive/Dr;Town;"
                  "Dist/District;City;State;Province;County;District"
                  ";Zip;"
                  "Location1;Location2;Location3;Location4;"
                  "Location5;Location6;Other" DELIMITED BY SIZE
                  INTO OUT-REC
           WRITE OUT-REC

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ IN-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       MOVE 'N' TO HAS-MATCH
                       MOVE IN-REC TO REMAINING
                       INSPECT REMAINING REPLACING ALL ',' BY ' '
                       *> 清理多空白
                       MOVE SPACES TO ADDR-CLEAN
                       MOVE 1 TO TMP-POS
                       PERFORM VARYING IDX FROM 1 BY 1 UNTIL 
                       IDX > FUNCTION LENGTH(REMAINING)
                           IF REMAINING(IDX:1) NOT = SPACE
                               MOVE REMAINING(IDX:1) TO 
                               ADDR-CLEAN(TMP-POS:1)
                               ADD 1 TO TMP-POS
                           ELSE
                               IF TMP-POS > 1 AND ADDR-CLEAN(
                               TMP-POS - 1:1) NOT = SPACE
                                   MOVE SPACE TO ADDR-CLEAN(TMP-POS:1)
                                   ADD 1 TO TMP-POS
                               END-IF
                           END-IF
                       END-PERFORM
                       MOVE ADDR-CLEAN TO REMAINING
                       MOVE SPACES TO ADDR-OUTPUT
                       MOVE 1 TO DST-POINTER

                       *> KEYWORD 拆段
                       PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM
                       (REMAINING)) = 0
                           MOVE 9999 TO MIN-POS
                           MOVE 0 TO MIN-LEN
                           MOVE 'N' TO FOUND
                           MOVE FUNCTION LENGTH(FUNCTION TRIM(
                           REMAINING)) TO REM-LEN

                           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
                           KEY-COUNT
                               IF FUNCTION LENGTH(FUNCTION TRIM(
                               KEY-ENTRY(IDX))) > 0
                                   MOVE FUNCTION LENGTH(FUNCTION TRIM(
                                   KEY-ENTRY(IDX))) TO KEY-LEN
                                   PERFORM VARYING TMP-POS FROM 1 BY 1 
                                   UNTIL TMP-POS + KEY-LEN - 1 > REM-LEN
                                       IF FUNCTION UPPER-CASE(
                                       REMAINING(TMP-POS:KEY-LEN)) =
                                          FUNCTION UPPER-CASE(FUNCTION 
                                          TRIM(KEY-ENTRY(IDX)))
                                           IF TMP-POS + KEY-LEN <= 
                                           REM-LEN
                                               MOVE REMAINING(TMP-POS + 
                                               KEY-LEN:1) TO AFTER-CHAR
                                           ELSE
                                               MOVE SPACE TO AFTER-CHAR
                                           END-IF
                                           IF AFTER-CHAR = ' ' OR 
                                           AFTER-CHAR = SPACES
                                              OR TMP-POS + KEY-LEN - 1 
                                              = REM-LEN
                                              OR AFTER-CHAR = '.' OR 
                                              AFTER-CHAR = ','
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
                               MOVE 'Y' TO HAS-MATCH
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

                           *> 含 KEYWORD → 直接輸出
                           IF  FOUND = 'Y'
                               MOVE TOKEN TO FIELD-FILL
                               MOVE FUNCTION TRIM(FIELD-FILL) TO 
                               TEMP-FIELD
                               STRING FUNCTION TRIM(TEMP-FIELD)
                                DELIMITED BY SIZE ';' 
                               DELIMITED BY SIZE
                                   INTO ADDR-OUTPUT WITH POINTER 
                                   DST-POINTER
                               END-STRING
                           ELSE
                               *> TOKEN > 35 或剩餘不含 KEYWORD → 交給 LOCATION 拆分
                               MOVE FUNCTION TRIM(TOKEN) TO PART
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(PART))
                                TO PART-LEN
                               MOVE 1 TO TMP-POS

                               *> --- BEGIN: LOCATION 拆分迴圈 ---
                               PERFORM UNTIL TMP-POS > PART-LEN
                                   MOVE 9999 TO EARLIEST-POS
                                   MOVE 0 TO EARLIEST-LEN
                                   MOVE 'N' TO LOC-FOUND

                                   PERFORM VARYING IDX FROM 1 BY 1 UNTIL
                                    IDX > LOC-COUNT
                                       IF FUNCTION LENGTH(FUNCTION TRIM
                                       (LOC-ITEM(IDX))) > 0
                                           MOVE FUNCTION LENGTH(FUNCTION
                                            TRIM(LOC-ITEM(IDX))) TO 
                                            LOC-LEN
                                           PERFORM VARYING LOC-POS FROM 
                                           TMP-POS BY 1 UNTIL LOC-POS + 
                                           LOC-LEN - 1 > PART-LEN
                                               IF FUNCTION UPPER-CASE(
                                               PART(LOC-POS:LOC-LEN)) =
                                                  FUNCTION UPPER-CASE(
                                                  FUNCTION TRIM(
                                                  LOC-ITEM(IDX)))
                                                   IF (LOC-POS = 1 OR 
                                                   PART(LOC-POS - 1:1) 
                                                   = SPACE)
                                                      AND (LOC-POS + 
                                                      LOC-LEN - 1 = 
                                                      PART-LEN OR PART
                                                      (LOC-POS + LOC-LEN
                                                      :1) = SPACE)
                                                       IF LOC-POS < 
                                                       EARLIEST-POS
                                                           MOVE LOC-POS
                                                            TO 
                                                            EARLIEST-POS
                                                           MOVE LOC-LEN 
                                                           TO 
                                                           EARLIEST-LEN
                                                           MOVE 'Y' TO 
                                                           LOC-FOUND
                                                       END-IF
                                                   END-IF
                                               END-IF
                                           END-PERFORM
                                       END-IF
                                   END-PERFORM

                                   IF LOC-FOUND = 'Y'
                                       MOVE 'Y' TO HAS-MATCH
                                       *> 輸出 LOCATION 前的 prefix
                                       IF EARLIEST-POS > TMP-POS
                                           COMPUTE PREFIX-LEN = 
                                           EARLIEST-POS - TMP-POS
                                           PERFORM UNTIL PREFIX-LEN <= 0
                                               
                                                   MOVE PART(TMP-POS:
                                                   PREFIX-LEN) TO 
                                                   FIELD-FILL
                                                   ADD PREFIX-LEN TO 
                                                   TMP-POS
                                                   SUBTRACT PREFIX-LEN 
                                                   FROM PREFIX-LEN
                                               

                                               COMPUTE FILL-START-IDX = 
                                               FUNCTION LENGTH(FUNCTION 
                                               TRIM(FIELD-FILL)) + 1
                                               
                                               STRING FUNCTION TRIM
                                               (FIELD-FILL) 
                                               DELIMITED BY SIZE ';' 
                                               DELIMITED BY SIZE
                                                   INTO ADDR-OUTPUT 
                                                   WITH POINTER 
                                                   DST-POINTER
                                               END-STRING
                                           END-PERFORM
                                       END-IF

                                       *> 輸出 LOCATION 本身
                                       MOVE PART(EARLIEST-POS:
                                       EARLIEST-LEN) TO FIELD-FILL
                                       COMPUTE FILL-START-IDX = FUNCTION
                                        LENGTH(FUNCTION TRIM(FIELD-FILL
                                        )) + 1
                                       
                                       STRING FUNCTION TRIM(FIELD-FILL)
                                        DELIMITED BY 
                                       SIZE ';' DELIMITED BY SIZE
                                           INTO ADDR-OUTPUT WITH POINTER
                                            DST-POINTER
                                       END-STRING

                                       *> 前進 TMP-POS
                                       COMPUTE TMP-POS = EARLIEST-POS + 
                                       EARLIEST-LEN
                                       PERFORM UNTIL TMP-POS > PART-LEN 
                                       OR PART(TMP-POS:1) NOT = SPACE
                                           ADD 1 TO TMP-POS
                                       END-PERFORM

                                   ELSE
                                       *> 沒找到 LOCATION → 拆 35
                                       COMPUTE REMAIN-LEN = PART-LEN - 
                                       TMP-POS + 1
                                       PERFORM UNTIL REMAIN-LEN <= 0
                                           
                                               MOVE PART(TMP-POS:
                                               REMAIN-LEN) TO FIELD-FILL
                                               ADD REMAIN-LEN TO TMP-POS
                                           

                                           COMPUTE FILL-START-IDX = 
                                           FUNCTION LENGTH(FUNCTION TRIM
                                           (FIELD-FILL)) + 1   
                                           STRING FUNCTION 
                                           TRIM(FIELD-FILL) DELIMITED 
                                           BY SIZE ';' DELIMITED BY SIZE
                                               INTO ADDR-OUTPUT WITH 
                                               POINTER DST-POINTER
                                           END-STRING
                                           COMPUTE REMAIN-LEN = PART-LEN
                                            - TMP-POS + 1
                                       END-PERFORM

                                       EXIT PERFORM
                                   END-IF
                               END-PERFORM
                               *> --- END: LOCATION 拆分迴圈 ---
                           END-IF
                       END-PERFORM

                       IF HAS-MATCH = 'Y'
                           MOVE ADDR-OUTPUT TO OUT-REC
                           WRITE OUT-REC
                       ELSE
                           MOVE IN-REC TO UNMATCH-REC
                           WRITE UNMATCH-REC
                       END-IF
               END-READ
           END-PERFORM

           CLOSE IN-FILE OUT-FILE UNMATCH-FILE
           STOP RUN.
