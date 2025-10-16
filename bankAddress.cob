       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKADDRESS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE  ASSIGN TO "fake_addresses.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "result_address.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
       01 IN-REC        PIC X(350).

       FD OUT-FILE.
       01 OUT-REC       PIC X(255).

       WORKING-STORAGE SECTION.
       01 WS-LINE       PIC X(350).
       01 WS-PARTS.
          05 WS-PART OCCURS 10 TIMES PIC X(50).
       01 NUM-PARTS     PIC 9(2) VALUE 0.
       01 OUT-TEMP      PIC X(255).

       01 KEYWORDS.
          05 KEY-ENTRY OCCURS 7 TIMES PIC X(10).
      

       01 IDX           PIC 9(2).
       01 POS           PIC 9(4).
       01 CUR-POS       PIC 9(4).
       01 POS-CUR-POS       PIC 9(4).
       01 FOUND-FLAG    PIC X VALUE "N".
       01 CUR-KEY       PIC X(10).
       01 EOF-FLAG      PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT IN-FILE
           OPEN OUTPUT OUT-FILE

           PERFORM UNTIL EOF-FLAG = "Y"
               READ IN-FILE INTO WS-LINE
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM

           CLOSE IN-FILE
           CLOSE OUT-FILE
           STOP RUN.
       PROCESS-LINE.
           *> 初始化
           MOVE 0 TO NUM-PARTS
           MOVE 1 TO CUR-POS
       
           MOVE "Street" TO KEY-ENTRY(1)
           MOVE "St"     TO KEY-ENTRY(2)
           MOVE "Ave"    TO KEY-ENTRY(3)
           MOVE "Blvd"   TO KEY-ENTRY(4)
           MOVE "City"   TO KEY-ENTRY(5)
           MOVE "State"  TO KEY-ENTRY(6)
           MOVE "Zip"    TO KEY-ENTRY(7)
       
           *> 從頭掃描整行
           PERFORM UNTIL CUR-POS > FUNCTION LENGTH(WS-LINE)
               MOVE "N" TO FOUND-FLAG
       
               *> 逐個關鍵字檢查
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 7 
               OR FOUND-FLAG = "Y"
                   MOVE FUNCTION TRIM(KEY-ENTRY(IDX)) TO CUR-KEY
                   MOVE CUR-POS TO POS
       
                   PERFORM UNTIL POS > FUNCTION LENGTH(WS-LINE) 
                   OR FOUND-FLAG = "Y"
                       IF WS-LINE(POS:FUNCTION LENGTH(CUR-KEY)) 
                       = CUR-KEY
                           ADD 1 TO NUM-PARTS
                           MOVE WS-LINE(CUR-POS:POS-CUR-POS + 
                           FUNCTION LENGTH(CUR-KEY))
                               TO WS-PARTS(NUM-PARTS)
                           ADD FUNCTION LENGTH(CUR-KEY) TO POS
                           MOVE POS TO CUR-POS
                           MOVE "Y" TO FOUND-FLAG
                       ELSE
                           ADD 1 TO POS
                       END-IF
                   END-PERFORM
               END-PERFORM
       
               *> 如果沒有找到關鍵字，把剩下字串放下一段
               IF FOUND-FLAG = "N"
                   ADD 1 TO NUM-PARTS
                   MOVE WS-LINE(CUR-POS:
                   FUNCTION LENGTH(WS-LINE) - CUR-POS + 1)
                       TO WS-PARTS(NUM-PARTS)
                   MOVE FUNCTION LENGTH(WS-LINE) + 1 TO CUR-POS
               END-IF
           END-PERFORM
       
           *> 輸出結果，用 |
           MOVE SPACES TO OUT-REC
           IF NUM-PARTS > 0
               MOVE FUNCTION TRIM(WS-PARTS(1)) TO OUT-REC
               PERFORM VARYING IDX FROM 2 BY 1 UNTIL IDX > NUM-PARTS
                   STRING OUT-REC DELIMITED BY SIZE
                          "|" DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PARTS(IDX)) DELIMITED BY SIZE
                      INTO OUT-REC
               END-PERFORM
           END-IF
           DISPLAY OUT-REC.
           WRITE OUT-REC.
       