       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitterFinal.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO 'input_test.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'test_0929.txt'
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
       01 TEMP-FIELD     PIC X(35).

       01 PART           PIC X(200).
       01 PART-LEN       PIC 9(4).
       01 FIELD-FILL     PIC X(35) VALUE SPACES.

       01 PREFIX-LEN     PIC 9(4) VALUE 0.
       01 SUB-LEN        PIC 9(4) VALUE 0.
       01 REMAIN-LEN     PIC 9(4) VALUE 0.
       01 FILL-START-IDX PIC 9(4) VALUE 0.
       01  TMP-LEN-CALC  PIC 9(4).

       01 HAS-MATCH      PIC X VALUE 'N'.

       01 KEYWORDS.
           05 KEY-ENTRY OCCURS 200 TIMES PIC X(50) VALUE SPACES.

       01 LOCATIONS.
           05 LOC-ITEM OCCURS 500 TIMES PIC X(100) VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PARA.
           PERFORM LOAD-KEYWORDS
           PERFORM LOAD-LOCATIONS

           OPEN INPUT IN-FILE OUTPUT OUT-FILE UNMATCH-FILE

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ IN-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       MOVE 'N' TO HAS-MATCH
                       MOVE IN-REC TO REMAINING
                       PERFORM CLEAN-INPUT
                       MOVE SPACES TO ADDR-OUTPUT
                       MOVE 1 TO DST-POINTER
                       PERFORM SPLIT-BY-KEYWORD-AND-LOCATION

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

       *>------------------- 子程式: 讀 KEYWORDS -------------------
       LOAD-KEYWORDS.
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
           .

       *>------------------- 子程式: 讀 LOCATIONS -------------------
       LOAD-LOCATIONS.
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
           .

       *>------------------- 子程式: 清理輸入 -------------------
       CLEAN-INPUT.
           INSPECT REMAINING REPLACING ALL ',' BY ' '
           MOVE SPACES TO ADDR-CLEAN
           MOVE 1 TO TMP-POS
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FUNCTION LENGTH
           (REMAINING)
               IF REMAINING(IDX:1) NOT = SPACE
                   MOVE REMAINING(IDX:1) TO ADDR-CLEAN(TMP-POS:1)
                   ADD 1 TO TMP-POS
               ELSE
                   IF TMP-POS > 1 AND ADDR-CLEAN(TMP-POS - 1:1)
                    NOT = SPACE
                       MOVE SPACE TO ADDR-CLEAN(TMP-POS:1)
                       ADD 1 TO TMP-POS
                   END-IF
               END-IF
           END-PERFORM
           MOVE ADDR-CLEAN TO REMAINING
           .

       *>------------------- 子程式: 拆分 keyword 與 location -------------------
       SPLIT-BY-KEYWORD-AND-LOCATION.
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(REMAINING)) = 0
               PERFORM FIND-NEXT-KEYWORD
               IF FOUND = 'Y'
                   MOVE 'Y' TO HAS-MATCH
                   COMPUTE KEY-POS = MIN-POS + MIN-LEN - 1
                   MOVE FUNCTION TRIM(REMAINING(1:KEY-POS)) TO TOKEN
                   IF KEY-POS < REM-LEN
                       MOVE FUNCTION TRIM(REMAINING(KEY-POS + 1:)) 
                       TO REMAINING
                   ELSE
                       MOVE SPACES TO REMAINING
                   END-IF
               ELSE
                   MOVE FUNCTION TRIM(REMAINING) TO TOKEN
                   MOVE SPACES TO REMAINING
               END-IF

               PERFORM PROCESS-TOKEN
           END-PERFORM
       .

       *>------------------- 子程式: 找下一個 keyword -------------------
       FIND-NEXT-KEYWORD.
           MOVE 9999 TO MIN-POS
           MOVE 0 TO MIN-LEN
           MOVE 'N' TO FOUND
           MOVE FUNCTION LENGTH(FUNCTION TRIM(REMAINING)) TO REM-LEN
       
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > KEY-COUNT
               IF FUNCTION LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX))) > 0
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX)))
                    TO KEY-LEN
                   PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL TMP-POS + 
                   KEY-LEN - 1 > REM-LEN
                       IF FUNCTION UPPER-CASE(REMAINING(TMP-POS:
                       KEY-LEN)) =
                          FUNCTION UPPER-CASE(FUNCTION TRIM(
                            KEY-ENTRY(IDX)))
                          
                          *> 取 keyword 後面字符判斷
                          IF TMP-POS + KEY-LEN <= REM-LEN
                              MOVE REMAINING(TMP-POS + KEY-LEN:1) TO
                               AFTER-CHAR
                          ELSE
                              MOVE SPACE TO AFTER-CHAR
                          END-IF
       
                          *> keyword 後面為空格、句號、逗號、數字或結尾 → 可拆
                          IF AFTER-CHAR = ' ' OR AFTER-CHAR = SPACES
                             OR TMP-POS + KEY-LEN - 1 = REM-LEN
                             OR AFTER-CHAR = '.' OR AFTER-CHAR = ','
                             OR AFTER-CHAR IS NUMERIC
       
                              IF TMP-POS < MIN-POS
                                  MOVE TMP-POS TO MIN-POS
                                  COMPUTE MIN-LEN = KEY-LEN
                                  MOVE 'Y' TO FOUND
                              END-IF
                          END-IF
                       END-IF
                   END-PERFORM
               END-IF
               IF FOUND = 'Y'
                   EXIT PERFORM
               END-IF
           END-PERFORM
       .
       

       *>------------------- 子程式: 處理 TOKEN (keyword 或 location) ----
       PROCESS-TOKEN.
           *> 若已找到 keyword (FOUND = 'Y') 且整段小於等於 35，直接輸出整段
           IF FOUND = 'Y' AND FUNCTION LENGTH(FUNCTION TRIM(TOKEN)) 
           <= 35
               MOVE FUNCTION TRIM(TOKEN) TO FIELD-FILL
               PERFORM FILL-FIELD-FILL
               GOBACK
           END-IF

           *> 否則：把 TOKEN 放到 PART，改用 location/35 拆法（保持單字完整）
           MOVE FUNCTION TRIM(TOKEN) TO PART
           MOVE FUNCTION LENGTH(FUNCTION TRIM(PART)) TO PART-LEN
           MOVE 1 TO TMP-POS

           PERFORM UNTIL TMP-POS > PART-LEN
               *> 嘗試找下一個 location（注意：FIND-NEXT-LOCATION 預期 PART, TMP-POS, PART-LEN 已設定）
               PERFORM FIND-NEXT-LOCATION

               IF LOC-FOUND = 'Y'
                   MOVE 'Y' TO HAS-MATCH
                   *> 輸出 EARLIEST-POS 之前的 prefix（若有），以不破字方式拆 35
                   IF EARLIEST-POS > TMP-POS
                       COMPUTE PREFIX-LEN = EARLIEST-POS - TMP-POS
                       PERFORM UNTIL PREFIX-LEN <= 0
                           IF PREFIX-LEN > 35
                               MOVE 35 TO SUB-LEN
                               *> 往回找最近空白以免切半字
                               PERFORM UNTIL SUB-LEN = 0 OR PART(TMP-POS
                                + SUB-LEN - 1:1) = SPACE
                                   SUBTRACT 1 FROM SUB-LEN
                               END-PERFORM
                               IF SUB-LEN = 0
                                   MOVE 35 TO SUB-LEN
                               END-IF
                           ELSE
                               MOVE PREFIX-LEN TO SUB-LEN
                           END-IF

                           MOVE PART(TMP-POS:SUB-LEN) TO FIELD-FILL
                           PERFORM FILL-FIELD-FILL

                           ADD SUB-LEN TO TMP-POS
                           SUBTRACT SUB-LEN FROM PREFIX-LEN
                       END-PERFORM
                   END-IF

                   *> 輸出 location 本身（整組 location 當一欄）
                   MOVE PART(EARLIEST-POS:EARLIEST-LEN) TO FIELD-FILL
                   PERFORM FILL-FIELD-FILL

                   *> 前進 TMP-POS 至 location 後並跳過空白
                   COMPUTE TMP-POS = EARLIEST-POS + EARLIEST-LEN
                   PERFORM UNTIL TMP-POS > PART-LEN OR PART(TMP-POS:1) 
                   NOT = SPACE
                       ADD 1 TO TMP-POS
                   END-PERFORM
               ELSE
                   *> 沒找到 location → 把剩下整段用不破字方式拆成 35（loop until done）
                   COMPUTE REMAIN-LEN = PART-LEN - TMP-POS + 1
                   PERFORM UNTIL REMAIN-LEN <= 0
                       IF REMAIN-LEN > 35
                           MOVE 35 TO SUB-LEN
                           PERFORM UNTIL SUB-LEN = 0 OR PART(TMP-POS + 
                           SUB-LEN - 1:1) = SPACE
                               SUBTRACT 1 FROM SUB-LEN
                           END-PERFORM
                           IF SUB-LEN = 0
                               MOVE 35 TO SUB-LEN
                           END-IF
                       ELSE
                           MOVE REMAIN-LEN TO SUB-LEN
                       END-IF

                       MOVE PART(TMP-POS:SUB-LEN) TO FIELD-FILL
                       PERFORM FILL-FIELD-FILL

                       ADD SUB-LEN TO TMP-POS
                       COMPUTE REMAIN-LEN = PART-LEN - TMP-POS + 1
                   END-PERFORM

                   *> 處理完剩餘就跳出主迴圈
                   EXIT PERFORM
               END-IF
           END-PERFORM
       .

      
       *>------------------- 子程式: 找下一個 location 並拆 35 -------------------
       FIND-NEXT-LOCATION.
           MOVE 9999 TO EARLIEST-POS
           MOVE 0 TO EARLIEST-LEN
           MOVE 'N' TO LOC-FOUND

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LOC-COUNT
               IF FUNCTION LENGTH(FUNCTION TRIM(LOC-ITEM(IDX))) > 0
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(LOC-ITEM(IDX))) 
                   TO LOC-LEN
                   PERFORM VARYING LOC-POS FROM TMP-POS BY 1 UNTIL 
                   LOC-POS + LOC-LEN - 1 > PART-LEN
                       IF FUNCTION UPPER-CASE(PART(LOC-POS:LOC-LEN)) =
                          FUNCTION UPPER-CASE(FUNCTION TRIM(LOC-ITEM
                          (IDX)))
                           IF (LOC-POS = 1 OR PART(LOC-POS - 1:1) = 
                           SPACE)
                              AND (LOC-POS + LOC-LEN - 1 = PART-LEN OR 
                              PART(LOC-POS + LOC-LEN:1) = SPACE)
                               IF LOC-POS < EARLIEST-POS
                                   MOVE LOC-POS TO EARLIEST-POS
                                   MOVE LOC-LEN TO EARLIEST-LEN
                                   MOVE 'Y' TO LOC-FOUND
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
                   COMPUTE PREFIX-LEN = EARLIEST-POS - TMP-POS
                   PERFORM UNTIL PREFIX-LEN <= 0
                       IF PREFIX-LEN > 35
                           MOVE 35 TO SUB-LEN
                           PERFORM UNTIL SUB-LEN = 0 OR PART(TMP-POS + 
                           SUB-LEN - 1:1) = SPACE
                               SUBTRACT 1 FROM SUB-LEN
                           END-PERFORM
                           IF SUB-LEN = 0
                               MOVE 35 TO SUB-LEN
                           END-IF
                           MOVE PART(TMP-POS:SUB-LEN) TO FIELD-FILL
                           ADD SUB-LEN TO TMP-POS
                           SUBTRACT SUB-LEN FROM PREFIX-LEN
                       ELSE
                           MOVE PART(TMP-POS:PREFIX-LEN) TO FIELD-FILL
                           ADD PREFIX-LEN TO TMP-POS
                           SUBTRACT PREFIX-LEN FROM PREFIX-LEN
                       END-IF
                       PERFORM FILL-FIELD-FILL
                   END-PERFORM
               END-IF

               *> 輸出 LOCATION 本身
               MOVE PART(EARLIEST-POS:EARLIEST-LEN) TO FIELD-FILL
               PERFORM FILL-FIELD-FILL
               COMPUTE TMP-POS = EARLIEST-POS + EARLIEST-LEN
               PERFORM UNTIL TMP-POS > PART-LEN OR PART(TMP-POS:1) 
               NOT = SPACE
                   ADD 1 TO TMP-POS
               END-PERFORM
           ELSE
               *> 沒找到 LOCATION → 拆 35
               COMPUTE REMAIN-LEN = PART-LEN - TMP-POS + 1
               PERFORM UNTIL REMAIN-LEN <= 0
                   IF REMAIN-LEN > 35
                       MOVE 35 TO SUB-LEN
                       PERFORM UNTIL SUB-LEN = 0 OR PART(TMP-POS + 
                       SUB-LEN - 1:1) = SPACE
                           SUBTRACT 1 FROM SUB-LEN
                       END-PERFORM
                       IF SUB-LEN = 0
                           MOVE 35 TO SUB-LEN
                       END-IF
                       MOVE PART(TMP-POS:SUB-LEN) TO FIELD-FILL
                       ADD SUB-LEN TO TMP-POS
                   ELSE
                       MOVE PART(TMP-POS:REMAIN-LEN) TO FIELD-FILL
                       ADD REMAIN-LEN TO TMP-POS
                   END-IF
                   PERFORM FILL-FIELD-FILL
                   COMPUTE REMAIN-LEN = PART-LEN - TMP-POS + 1
               END-PERFORM
           END-IF
           .

       *>------------------- 子程式: 填充 FIELD-FILL 並加 '|', 35 位 -------------------
       FILL-FIELD-FILL.
           COMPUTE FILL-START-IDX = FUNCTION LENGTH(FUNCTION TRIM(
            FIELD-FILL)) + 1
           PERFORM VARYING IDX FROM FILL-START-IDX BY 1 UNTIL IDX > 35
               MOVE SPACE TO FIELD-FILL(IDX:1)
           END-PERFORM
           STRING FIELD-FILL DELIMITED BY SIZE '|' DELIMITED BY SIZE
               INTO ADDR-OUTPUT WITH POINTER DST-POINTER
           END-STRING
           .
