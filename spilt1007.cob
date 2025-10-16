       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitterFinal.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO 'input_test.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'test_1007.csv'
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
       01  OUT-REC     PIC X(2000).

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

       01 REMAINING      PIC X(200).
       01 ADDR-OUTPUT    PIC X(2000) VALUE SPACES.
       01 DST-POINTER    PIC 9(4) VALUE 1.

       01 IDX            PIC 9(4).
       01 KEY-COUNT      PIC 9(4) VALUE 0.
       01 LOC-COUNT      PIC 9(4) VALUE 0.
       01 REM-LEN      PIC 9(4) VALUE 0.
       

       01 TMP-POS        PIC 9(4).
       01 TMP-KEY-LEN    PIC 9(4).
       01 MIN-POS        PIC 9(4).
       01 MIN-LEN        PIC 9(4).
       01 FOUND          PIC X VALUE 'N'.

       01 TMP-KEY        PIC X(50).
       01 HAS-MATCH      PIC X VALUE 'N'.
       01 AFTER-CHAR     PIC X.

       01 KEYWORDS.
           05 KEY-ENTRY OCCURS 200 TIMES PIC X(50) VALUE SPACES.

       01 LOCATIONS.
           05 LOC-ITEM OCCURS 500 TIMES PIC X(100) VALUE SPACES.

       01 RM             PIC X(50) VALUE SPACES.
       01 F              PIC X(50) VALUE SPACES.
       01 ALY            PIC X(50) VALUE SPACES.
       01 LN_LANE        PIC X(50) VALUE SPACES.
       01 SEC            PIC X(50) VALUE SPACES.
       01 STREET_ST      PIC X(50) VALUE SPACES.
       01 AVENUE_AVE     PIC X(50) VALUE SPACES.
       01 WAY            PIC X(50) VALUE SPACES.
       01 BOULEVARD_BLVD PIC X(50) VALUE SPACES.
       01 ROAD_RD        PIC X(50) VALUE SPACES.
       01 DRIVE_DR       PIC X(50) VALUE SPACES.
       01 TOWN           PIC X(50) VALUE SPACES.
       01 DISTRICT       PIC X(50) VALUE SPACES.
       01 CITY           PIC X(50) VALUE SPACES.
       01 STATE          PIC X(50) VALUE SPACES.
       01 PROVINCE       PIC X(50) VALUE SPACES.
       01 COUNTY         PIC X(50) VALUE SPACES.
       01 ZIP            PIC X(50) VALUE SPACES.
       01 LOCATION1      PIC X(50) VALUE SPACES.
       01 LOCATION2      PIC X(50) VALUE SPACES.
       01 LOCATION3      PIC X(50) VALUE SPACES.
       01 LOCATION4      PIC X(50) VALUE SPACES.
       01 LOCATION5      PIC X(50) VALUE SPACES.
       01 LOCATION6      PIC X(50) VALUE SPACES.
       01 OTHER-FIELD    PIC X(200) VALUE SPACES.
       01 TMP-MERGED    PIC X(200) VALUE SPACES.

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
                       MOVE IN-REC TO REMAINING
                       MOVE SPACES TO ADDR-OUTPUT
                       MOVE 1 TO DST-POINTER
                       MOVE 'N' TO HAS-MATCH

                       *> 清空所有欄位
                       MOVE SPACES TO RM F ALY LN_LANE SEC STREET_ST 
                       AVENUE_AVE WAY BOULEVARD_BLVD ROAD_RD DRIVE_DR 
                       TOWN DISTRICT
                       CITY STATE PROVINCE COUNTY ZIP LOCATION1 
                       LOCATION2 LOCATION3
                       LOCATION4 LOCATION5 LOCATION6 OTHER-FIELD

                       *> Keyword 拆分迴圈（保留完整檢查邏輯）
                       MOVE FUNCTION LENGTH(FUNCTION TRIM(REMAINING)) 
                       TO REM-LEN
                       MOVE 9999 TO MIN-POS
                       MOVE 0 TO MIN-LEN
                       MOVE 'N' TO FOUND

                       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
                       KEY-COUNT
                           IF FUNCTION LENGTH(FUNCTION TRIM(KEY-ENTRY
                           (IDX))) > 0
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(
                               KEY-ENTRY(IDX))) TO TMP-KEY-LEN
                               PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL
                                TMP-POS + TMP-KEY-LEN - 1 > REM-LEN
                                   IF FUNCTION UPPER-CASE(REMAINING
                                   (TMP-POS:TMP-KEY-LEN)) = FUNCTION 
                                   UPPER-CASE(FUNCTION TRIM(
                                   KEY-ENTRY(IDX)))
                                       
                                       IF TMP-POS + TMP-KEY-LEN - 1 = 
                                       REM-LEN
                                          OR REMAINING(TMP-POS + 
                                          TMP-KEY-LEN:1) = ' '
                                          OR REMAINING(TMP-POS + 
                                          TMP-KEY-LEN:1) = SPACES
                                          OR REMAINING(TMP-POS + 
                                          TMP-KEY-LEN:1) = '.'
                                          OR REMAINING(TMP-POS + 
                                          TMP-KEY-LEN:1) = ','
                                           
                                           IF TMP-POS < MIN-POS
                                               MOVE TMP-POS TO MIN-POS
                                               MOVE TMP-KEY-LEN TO 
                                               MIN-LEN
                                               MOVE 'Y' TO FOUND
                                           END-IF
                                       END-IF
                                   END-IF
                               END-PERFORM
                           END-IF
                       END-PERFORM

                       IF FOUND = 'Y'
                           *> Keyword 前文字放 OTHER-FIELD
                           IF MIN-POS > 1
                               MOVE FUNCTION TRIM(REMAINING(1:MIN-POS - 
                               1)) TO OTHER-FIELD
                           ELSE
                               MOVE SPACES TO OTHER-FIELD
                           END-IF
                           
                           MOVE FUNCTION TRIM(REMAINING(MIN-POS:MIN-LEN)
                           ) TO TMP-KEY
                           
                           *> 合併前置文字 + Keyword
                           STRING FUNCTION TRIM(OTHER-FIELD) DELIMITED 
                           BY SIZE
                                  FUNCTION TRIM(TMP-KEY) DELIMITED BY 
                                  SIZE
                                  INTO TMP-MERGED
                           END-STRING
                           
                           EVALUATE FUNCTION UPPER-CASE(TMP-KEY)
                              WHEN 'RM' MOVE TMP-MERGED TO RM
                              WHEN 'F' MOVE TMP-MERGED TO F
                              WHEN 'ALY' MOVE TMP-MERGED TO ALY
                              WHEN 'LN' MOVE TMP-MERGED TO LN_LANE
                              WHEN 'LANE' MOVE TMP-MERGED TO LN_LANE
                              WHEN 'SEC' MOVE TMP-MERGED TO SEC
                              WHEN 'STREET' MOVE TMP-MERGED TO STREET_ST
                              WHEN 'ST' MOVE TMP-MERGED TO STREET_ST
                              WHEN 'AVENUE' MOVE TMP-MERGED TO 
                              AVENUE_AVE
                              WHEN 'AVE' MOVE TMP-MERGED TO AVENUE_AVE
                              WHEN 'WAY' MOVE TMP-MERGED TO WAY
                              WHEN 'BOULEVARD' MOVE TMP-MERGED TO
                               BOULEVARD_BLVD
                              WHEN 'BLVD' MOVE TMP-MERGED TO 
                              BOULEVARD_BLVD
                              WHEN 'ROAD' MOVE TMP-MERGED TO ROAD_RD
                              WHEN 'RD' MOVE TMP-MERGED TO ROAD_RD
                              WHEN 'DRIVE' MOVE TMP-MERGED TO DRIVE_DR
                              WHEN 'DR' MOVE TMP-MERGED TO DRIVE_DR
                              WHEN 'TOWN' MOVE TMP-MERGED TO TOWN
                              WHEN 'DIST' MOVE TMP-MERGED TO DISTRICT
                              WHEN 'DISTRICT' MOVE TMP-MERGED TO 
                              DISTRICT
                              WHEN 'CITY' MOVE TMP-MERGED TO CITY
                              WHEN 'STATE' MOVE TMP-MERGED TO STATE
                              WHEN 'PROVINCE' MOVE TMP-MERGED TO 
                              PROVINCE
                              WHEN 'COUNTY' MOVE TMP-MERGED TO COUNTY
                              WHEN 'ZIP' MOVE TMP-MERGED TO ZIP
                              WHEN 'POSTAL' MOVE TMP-MERGED TO ZIP
                              WHEN OTHER CONTINUE
                           END-EVALUATE

                           *> 剩餘文字放 REMAINING 給 Location 拆分
                           IF MIN-POS + MIN-LEN <= REM-LEN
                               MOVE REMAINING(MIN-POS + MIN-LEN:) TO 
                               REMAINING
                               MOVE SPACES TO OTHER-FIELD
                           ELSE
                               MOVE SPACES TO REMAINING
                               MOVE SPACES TO OTHER-FIELD
                           END-IF
                       ELSE
                           MOVE REMAINING TO OTHER-FIELD
                           MOVE SPACES TO REMAINING
                       END-IF

                       *> Location 拆分
                       MOVE 1 TO IDX
                         PERFORM UNTIL IDX > LOC-COUNT
                             IF FUNCTION LENGTH(FUNCTION TRIM(LOC-ITEM
                             (IDX))) > 0
                                 PERFORM VARYING TMP-POS FROM 1 BY 1
                                     UNTIL TMP-POS > FUNCTION LENGTH(
                                     REMAINING)
                                     IF FUNCTION UPPER-CASE(
                                         REMAINING(TMP-POS:FUNCTION
                                          LENGTH(FUNCTION TRIM(LOC-ITEM
                                          (IDX)))))
                                        = FUNCTION UPPER-CASE(FUNCTION 
                                        TRIM(LOC-ITEM(IDX)))
                                         
                                         *> 放到 LOCATION1~6
                                         EVALUATE TRUE
                                             WHEN LOCATION1 = SPACES
                                                 MOVE LOC-ITEM(IDX) TO 
                                                 LOCATION1
                                             WHEN LOCATION2 = SPACES
                                                 MOVE LOC-ITEM(IDX) TO 
                                                 LOCATION2
                                             WHEN LOCATION3 = SPACES
                                                 MOVE LOC-ITEM(IDX) TO 
                                                 LOCATION3
                                             WHEN LOCATION4 = SPACES
                                                 MOVE LOC-ITEM(IDX) TO 
                                                 LOCATION4
                                             WHEN LOCATION5 = SPACES
                                                 MOVE LOC-ITEM(IDX) TO 
                                                 LOCATION5
                                             WHEN LOCATION6 = SPACES
                                                 MOVE LOC-ITEM(IDX) TO 
                                                 LOCATION6
                                         END-EVALUATE
              
                                         *> 刪掉 REMAINING 中已匹配文字
                                         MOVE SPACES TO REMAINING(
                                         TMP-POS:FUNCTION LENGTH(
                                         FUNCTION TRIM(LOC-ITEM(IDX))))
              
                                         EXIT PERFORM
                                     END-IF
                                 END-PERFORM
                             END-IF
                             ADD 1 TO IDX
                         END-PERFORM

                       *> 輸出 CSV
                       MOVE SPACES TO OUT-REC
                       STRING FUNCTION TRIM(RM) ";" FUNCTION TRIM(F) 
                       ";" FUNCTION TRIM(ALY) ";"
                              FUNCTION TRIM(LN_LANE) ";" FUNCTION 
                              TRIM(SEC) ";" FUNCTION TRIM(STREET_ST) ";"
                              FUNCTION TRIM(AVENUE_AVE) ";" FUNCTION 
                              TRIM(WAY) ";" FUNCTION 
                              TRIM(BOULEVARD_BLVD) ";"
                              FUNCTION TRIM(ROAD_RD) ";" FUNCTION 
                              TRIM(DRIVE_DR) ";" FUNCTION TRIM(TOWN) ";"
                              FUNCTION TRIM(DISTRICT) ";" FUNCTION 
                              TRIM(CITY) ";" FUNCTION TRIM(STATE) ";"
                              FUNCTION TRIM(PROVINCE) ";" FUNCTION 
                              TRIM(COUNTY) ";" FUNCTION TRIM(ZIP) ";"
                              FUNCTION TRIM(LOCATION1) ";" FUNCTION 
                              TRIM(LOCATION2) ";" FUNCTION 
                              TRIM(LOCATION3) ";"
                              FUNCTION TRIM(LOCATION4) ";" FUNCTION 
                              TRIM(LOCATION5) ";" FUNCTION 
                              TRIM(LOCATION6) ";"
                              FUNCTION TRIM(OTHER-FIELD) DELIMITED BY
                               SIZE INTO OUT-REC
                       WRITE OUT-REC
               END-READ
           END-PERFORM

           CLOSE IN-FILE OUT-FILE UNMATCH-FILE
           STOP RUN.
