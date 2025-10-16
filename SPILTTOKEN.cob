       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitter.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> 原始輸入字串
       01 ADDR-INPUT      PIC X(200) VALUE SPACES.
       01 ADDR-CLEAN      PIC X(200).
       01 ADDR-OUTPUT     PIC X(1000) VALUE SPACES.
       01 TOKEN           PIC X(200) VALUE SPACES.
       01 DST-POINTER     PIC 9(4) VALUE 1.
       01 IDX             PIC 9.
       01 KEY-POS         PIC 9(4).
       01 TMP-POS         PIC 9(4).
       01 REMAINING       PIC X(200).
       01 FOUND           PIC X VALUE 'N'.

       *> 關鍵字列表
       01 KEYWORDS.
           05 KEY-ENTRY OCCURS 8 TIMES PIC X(10) VALUE SPACES.

       *> 智能切段變數
       01 SUBSTR-START    PIC 9(4).
       01 SUBSTR-END      PIC 9(4).
       01 SUBSTR-LEN      PIC 9(4).
       01 TOKEN-FILL      PIC X(35).

       PROCEDURE DIVISION.

           *> ------------------------------
           *> 初始化 KEYWORDS
           *> ------------------------------
           MOVE 'Street' TO KEY-ENTRY(1)
           MOVE 'St'     TO KEY-ENTRY(2)
           MOVE 'Ave'    TO KEY-ENTRY(3)
           MOVE 'Blvd'   TO KEY-ENTRY(4)
           MOVE 'City'   TO KEY-ENTRY(5)
           MOVE 'State'  TO KEY-ENTRY(6)
           MOVE 'Zip'    TO KEY-ENTRY(7)
           MOVE 'town'   TO KEY-ENTRY(8)

           *> 原始輸入字串
           STRING
           '3989 Highland Ave asdfghjklqwer Rivertown tyuASDFGHJizxcvb,'
           ',Greenville City, NY 18837'
           DELIMITED BY SIZE
           INTO ADDR-INPUT

           *> 將逗號改成空格
           MOVE ADDR-INPUT TO ADDR-CLEAN
           INSPECT ADDR-CLEAN REPLACING ALL ',' BY ' '

           MOVE ADDR-CLEAN TO REMAINING

           *> ------------------------------
           *> 主迴圈處理 REMAINING
           *> ------------------------------
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(REMAINING)) = 0

               MOVE 9999 TO KEY-POS
               MOVE 'N' TO FOUND

               *> ------------------------------
               *> 找到最前面的 KEYWORD
               *> ------------------------------
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 8 OR FOUND 
               = 'Y'
                   MOVE 1 TO TMP-POS
                   PERFORM UNTIL TMP-POS > FUNCTION LENGTH(REMAINING) 
                   OR FOUND = 'Y'
                       IF FUNCTION UPPER-CASE(REMAINING(TMP-POS:
                              FUNCTION LENGTH(FUNCTION 
                              TRIM(KEY-ENTRY(IDX))))) =
                          FUNCTION UPPER-CASE(FUNCTION 
                          TRIM(KEY-ENTRY(IDX)))
                          *> 檢查字界，避免拆單字
                          IF (TMP-POS = 1 OR NOT (REMAINING(TMP-POS - 1
                          :1) >= 'A'
                               AND REMAINING(TMP-POS - 1:1) <= 'Z'
                               OR REMAINING(TMP-POS - 1:1) >= 'a'
                               AND REMAINING(TMP-POS - 1:1) <= 'z'))
                             MOVE TMP-POS TO KEY-POS
                             MOVE 'Y' TO FOUND
                          END-IF
                       END-IF
                       ADD 1 TO TMP-POS
                   END-PERFORM
               END-PERFORM

               *> ------------------------------
               *> TOKEN = 從開頭到關鍵字結束
               *> 若沒找到，TOKEN = 剩餘字串
               *> ------------------------------
               IF FOUND = 'Y'
                   MOVE FUNCTION TRIM(REMAINING(1:KEY-POS + FUNCTION 
                   LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX))) - 1)) TO TOKEN
               ELSE
                   MOVE FUNCTION TRIM(REMAINING TRAILING) TO TOKEN
               END-IF

               *> ------------------------------
               *> 智能切段，每段 ≤ 35，補空白到 35
               *> ------------------------------
               MOVE 1 TO SUBSTR-START
               PERFORM UNTIL SUBSTR-START > FUNCTION LENGTH(TOKEN)
                   COMPUTE SUBSTR-END = SUBSTR-START + 34
                   IF SUBSTR-END > FUNCTION LENGTH(TOKEN)
                       MOVE FUNCTION LENGTH(TOKEN) TO SUBSTR-END
                   END-IF

                   *> 向前找最後一個空格，避免拆單字
                   PERFORM UNTIL SUBSTR-END < SUBSTR-START OR 
                   TOKEN(SUBSTR-END:1) = ' '
                       SUBTRACT 1 FROM SUBSTR-END
                   END-PERFORM

                   IF SUBSTR-END < SUBSTR-START
                       COMPUTE SUBSTR-END = SUBSTR-START + 34
                       IF SUBSTR-END > FUNCTION LENGTH(TOKEN)
                           MOVE FUNCTION LENGTH(TOKEN) TO SUBSTR-END
                       END-IF
                   END-IF

                   COMPUTE SUBSTR-LEN = SUBSTR-END - SUBSTR-START + 1

                   *> 將段落放入 TOKEN-FILL，補空白到 35
                   MOVE SPACES TO TOKEN-FILL
                   MOVE FUNCTION TRIM(TOKEN(SUBSTR-START:SUBSTR-LEN)) 
                   TO TOKEN-FILL(1:SUBSTR-LEN)

                   *> 串入 ADDR-OUTPUT
                   STRING TOKEN-FILL
                          DELIMITED BY SIZE
                          '|' DELIMITED BY SIZE
                          INTO ADDR-OUTPUT
                          WITH POINTER DST-POINTER
                   END-STRING

                   COMPUTE SUBSTR-START = SUBSTR-END + 1
                   *> 跳過空格
                   PERFORM UNTIL SUBSTR-START > FUNCTION LENGTH(TOKEN) 
                   OR TOKEN(SUBSTR-START:1) NOT = ' '
                       ADD 1 TO SUBSTR-START
                   END-PERFORM
               END-PERFORM

               *> ------------------------------
               *> 移除已處理部分
               *> ------------------------------
               IF FOUND = 'Y'
                   IF KEY-POS + FUNCTION LENGTH(FUNCTION TRIM
                   (KEY-ENTRY(IDX))) - 1 <
                      FUNCTION LENGTH(REMAINING)
                       MOVE REMAINING(KEY-POS + FUNCTION LENGTH
                       (FUNCTION TRIM(KEY-ENTRY(IDX))):)
                            TO REMAINING
                   ELSE
                       MOVE SPACES TO REMAINING
                   END-IF
               ELSE
                   MOVE SPACES TO REMAINING
               END-IF

           END-PERFORM

           DISPLAY 'Result: ' ADDR-OUTPUT

           STOP RUN.
