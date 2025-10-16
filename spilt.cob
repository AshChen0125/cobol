       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitterFile.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE  ASSIGN TO 'fake_addresses.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'result_address.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
       01 IN-REC  PIC X(200).

       FD OUT-FILE.
       01 OUT-REC PIC X(500).

       WORKING-STORAGE SECTION.
       01 ADDR-CLEAN      PIC X(200).
       01 ADDR-OUTPUT     PIC X(500) VALUE SPACES.
       01 DST-POINTER     PIC 9(4) VALUE 1.
       01 IDX             PIC 9.
       01 KEY-POS         PIC 9(4).
       01 TMP-POS         PIC 9(4).
       01 REMAINING       PIC X(200).
       01 FOUND           PIC X VALUE 'N'.
       01 EOF-IN          PIC X VALUE 'N'.
       01 KEYWORDS.
           05 KEY-ENTRY OCCURS 7 TIMES PIC X(10) VALUE SPACES.
       01 TMP-STR   PIC X(200).
       01 TMP-CHUNK PIC X(35).
       01 LEN-TMP   PIC 9(4).
       01 CHUNK-LEN PIC 9(4).
       01 TMP-TEXT  PIC X(35).

       PROCEDURE DIVISION.
       BEGIN.

           *> 初始化 KEYWORDS
           MOVE 'Street' TO KEY-ENTRY(1)
           MOVE 'St'     TO KEY-ENTRY(2)
           MOVE 'Ave'    TO KEY-ENTRY(3)
           MOVE 'Blvd'   TO KEY-ENTRY(4)
           MOVE 'City'   TO KEY-ENTRY(5)
           MOVE 'State'  TO KEY-ENTRY(6)
           MOVE 'Zip'    TO KEY-ENTRY(7)
           

           OPEN INPUT IN-FILE
           OPEN OUTPUT OUT-FILE

           PERFORM UNTIL EOF-IN = 'Y'
               READ IN-FILE
                   AT END MOVE 'Y' TO EOF-IN
                   NOT AT END
                       MOVE IN-REC TO ADDR-CLEAN
                       INSPECT ADDR-CLEAN REPLACING ALL ',' BY ' '
                       MOVE ADDR-CLEAN TO REMAINING
                       MOVE 1 TO DST-POINTER
                       MOVE SPACES TO ADDR-OUTPUT

                       *> 處理 REMAINING
                       PERFORM UNTIL FUNCTION LENGTH(FUNCTION 
                       TRIM(REMAINING)) = 0
                           MOVE 9999 TO KEY-POS
                           MOVE 'N' TO FOUND

                           *> 找到最前面的 KEYWORD
                           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 7
                            OR FOUND = 'Y'
                               MOVE 1 TO TMP-POS
                               PERFORM UNTIL TMP-POS > FUNCTION 
                               LENGTH(REMAINING) OR FOUND = 'Y'
                                   IF REMAINING(TMP-POS:FUNCTION 
                                   LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX))
                                   )) =
                                      FUNCTION TRIM(KEY-ENTRY(IDX))
                                       MOVE TMP-POS TO KEY-POS
                                       MOVE 'Y' TO FOUND
                                   ELSE
                                       ADD 1 TO TMP-POS
                                   END-IF
                               END-PERFORM
                           END-PERFORM

                           *> TMP-STR 存從開頭到關鍵字
                           IF FOUND = 'Y'
                               MOVE REMAINING(1:KEY-POS + FUNCTION 
                               LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX))) 
                                )
                                    TO TMP-STR
                           ELSE
                               MOVE FUNCTION TRIM(REMAINING TRAILING) 
                               TO TMP-STR
                           END-IF

                           *> TMP-STR 每 35 位分段
                           MOVE FUNCTION LENGTH(FUNCTION TRIM(TMP-STR)) 
                           TO LEN-TMP
                           MOVE 1 TO TMP-POS

                           PERFORM UNTIL TMP-POS > LEN-TMP
                               COMPUTE CHUNK-LEN = LEN-TMP - TMP-POS + 1
                               IF CHUNK-LEN > 35
                                   MOVE 35 TO CHUNK-LEN
                               END-IF

                               *> 將段文字移到 TMP-TEXT，右側補空格到 35 位
                               MOVE SPACES TO TMP-TEXT
                               MOVE TMP-STR(TMP-POS:CHUNK-LEN) TO 
                               TMP-TEXT(1:CHUNK-LEN)

                              *>加分隔記號方便閱讀
                               STRING TMP-TEXT DELIMITED BY SIZE
                                      '|' DELIMITED BY SIZE
                                      INTO ADDR-OUTPUT
                                      WITH POINTER DST-POINTER

                               ADD CHUNK-LEN TO TMP-POS
                           END-PERFORM

                           *> 移除已處理部分
                           IF FOUND = 'Y' AND KEY-POS + FUNCTION 
                           LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX))) <=
                              FUNCTION LENGTH(REMAINING)
                               MOVE REMAINING(KEY-POS + FUNCTION 
                               LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX)))
                                + 1 : )
                                   TO REMAINING
                           ELSE
                               MOVE SPACES TO REMAINING
                           END-IF

                       END-PERFORM

                       MOVE ADDR-OUTPUT TO OUT-REC
                       WRITE OUT-REC

               END-READ
           END-PERFORM

           CLOSE IN-FILE
           CLOSE OUT-FILE

           STOP RUN.
