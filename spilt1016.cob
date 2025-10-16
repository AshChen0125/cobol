       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitterFinal.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO 'input_ce.csv'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'test_1016.csv' 
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KEY-FILE ASSIGN TO 'keywords.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LOC-FILE ASSIGN TO 'location.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNMATCH-FILE ASSIGN TO 'unmatched.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COUNTRY-FILE ASSIGN TO 'country.txt'
               ORGANIZATION IS LINE SEQUENTIAL. 
           SELECT CITY-FILE ASSIGN TO 'city.txt'
               ORGANIZATION IS LINE SEQUENTIAL. 
           SELECT STATE-FILE ASSIGN TO 'state.txt'
               ORGANIZATION IS LINE SEQUENTIAL. 

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC      PIC X(500).

       FD  OUT-FILE.
       01  OUT-REC     PIC X(5000).

       FD  KEY-FILE.
       01  KEY-REC     PIC X(50).

       FD  LOC-FILE.
       01  LOC-REC     PIC X(100).

       FD  UNMATCH-FILE.
       01  UNMATCH-REC PIC X(200).

       FD  COUNTRY-FILE.
       01  COUNTRY-REC     PIC X(100).

       FD  CITY-FILE.
       01  CITY-REC     PIC X(100).

       FD  STATE-FILE.
       01  STATE-REC     PIC X(100).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG       PIC X VALUE 'N'.
       01 KEY-FILE-EOF   PIC X VALUE 'N'.
       01 LOC-FILE-EOF   PIC X VALUE 'N'.
       01 CITY-FILE-EOF   PIC X VALUE 'N'.
       01 COUNTRY-FILE-EOF   PIC X VALUE 'N'.
       01 STATE-FILE-EOF   PIC X VALUE 'N'.
       01 HAS-MATCH      PIC X VALUE 'N'.
       01 FOUND          PIC X VALUE 'N'.
       01 LOC-FOUND      PIC X VALUE 'N'.

       01 REMAINING      PIC X(500).
       01 FIELD-FILL     PIC X(100).
       01 TEMP-FIELD     PIC X(100).
       01 TMP-POS        PIC 9(4).
       01 TMP-POS2       PIC 9(4).
       01  PTR          PIC 9(4).
       01  IDX          PIC 9(4).
       01  POS          PIC 9(4).
       01 FIELD-LEN       PIC 9(4).
       01 TEMP-LEN       PIC 9(4).
       01 KEY-LEN       PIC 9(4).
       01 TEST-FIELD     PIC X(50).

       01 RM             PIC X(50) .
       01 F              PIC X(50) .
       01 BUILDING         PIC X(50) .
       01 ALY            PIC X(50) .
       01 LN_LANE        PIC X(50) .
       01 SEC            PIC X(50) .
       01 STREET_ST      PIC X(50) .
       01 AVENUE_AVE     PIC X(50) .
       01 WAY            PIC X(50) .
       01 BOULEVARD_BLVD PIC X(50) .
       01 ROAD_RD        PIC X(50) .
       01 DRIVE_DR       PIC X(50) .
       01 TOWN           PIC X(50) .
       01 DISTRICT       PIC X(50) .
       01 CITY           PIC X(50) .
       01 STATE          PIC X(50) .
       01 PROVINCE       PIC X(50) .
       01 COUNTY         PIC X(50) .
       01 ZIP            PIC X(50) .
       01 COUNTRY         PIC X(50) .
       01 NUMBER-FILED   PIC X(50) .
       01 LOCATION1      PIC X(50) .
       01 LOCATION2      PIC X(50) .
       01 LOCATION3      PIC X(50) .
       01 LOCATION4      PIC X(50) .
       01 LOCATION5      PIC X(50) .
       01 LOCATION6      PIC X(50) .
       01 OTHER-FIELD    PIC X(200) .
       01 CHINESE        PIC X(500).
       01 ORIGINAL       PIC X(500).
       01 CONCAT       PIC X(500).

       01 KEY-COUNT      PIC 9(4) VALUE 0.
       01 LOC-COUNT      PIC 9(4) VALUE 0.
       01 CITY-COUNT      PIC 9(4) VALUE 0.
       01 COUNTRY-COUNT      PIC 9(4) VALUE 0.
       01 STATE-COUNT      PIC 9(4) VALUE 0.

       01 KEYWORDS.
           05 KEY-ENTRY OCCURS 200 TIMES PIC X(50) .
       01 LOCATIONS.
           05 LOC-ITEM OCCURS 500 TIMES PIC X(100) .
       01 COUNTRYS.
           05 COUNTRY-ITEM OCCURS 500 TIMES PIC X(100) .    
       01 CITYS.
           05 CITY-ITEM OCCURS 500 TIMES PIC X(100) . 
       01 STATES.
           05 STATE-ITEM OCCURS 500 TIMES PIC X(100) .
       *> 拆分後的欄位陣列
       01 FIELD-ARRAY.
           05 FIELD-ENTRY OCCURS 200 TIMES PIC X(100).
       01 FIELD-COUNT PIC 9(4) VALUE 0.

       01 NO-CHECK-LEN    PIC 9(4).
       01 FIRST-CHAR   PIC X.
       01 LAST-CHAR    PIC X.
       01 NUM-PREFIX   PIC 9(10) VALUE ZEROS.
       01 NUM-SUFFIX   PIC 9(10) VALUE ZEROS.
       01 NUM-END-POS  PIC 9(4).
       01 NO-CHECK-RESULT    PIC X(10).
       01 NO-CHECK-TOKEN      PIC X(50) .
       01 NO-IDX          PIC 9(4).
       01 NO-PTR          PIC 9(4).
       01 NO-CHECK-IDX          PIC 9(4).
       01 NO-IDX2            PIC 9(4) VALUE 1.
       01 NO-ARRAY.
           05 NO-FIELD OCCURS 100 TIMES PIC X(20).
       01 IS-NUM             PIC X VALUE 'N'.
       01 STATE-IDX          PIC 9(4).
       01 TMP-START   PIC 9(4).
       01 IS-STATE             PIC X VALUE 'N'.
       01 WS-TEMP PIC X(200) .
       77 WS-FIRST-FIELD PIC X VALUE 'Y'.
       


       PROCEDURE DIVISION.
       MAIN-PARA.
           *> 讀 KEYWORD
           OPEN INPUT KEY-FILE
           PERFORM UNTIL KEY-FILE-EOF = 'Y'
               READ KEY-FILE
                   AT END MOVE 'Y' TO KEY-FILE-EOF
                   NOT AT END
                       ADD 1 TO KEY-COUNT
                       MOVE FUNCTION TRIM(KEY-REC) TO KEY-ENTRY
                       (KEY-COUNT)
               END-READ
           END-PERFORM
           CLOSE KEY-FILE

           *> 讀 CITY
           OPEN INPUT CITY-FILE
           PERFORM UNTIL CITY-FILE-EOF = 'Y'
               READ CITY-FILE
                   AT END MOVE 'Y' TO CITY-FILE-EOF
                   NOT AT END
                       ADD 1 TO CITY-COUNT
                       MOVE FUNCTION TRIM(CITY-REC) TO CITY-ITEM(
                        CITY-COUNT)
               END-READ
           END-PERFORM
           CLOSE CITY-FILE

           *> 讀 COUNTRY
           OPEN INPUT COUNTRY-FILE
           PERFORM UNTIL COUNTRY-FILE-EOF = 'Y'
               READ COUNTRY-FILE
                   AT END MOVE 'Y' TO COUNTRY-FILE-EOF
                   NOT AT END
                       ADD 1 TO COUNTRY-COUNT
                       MOVE FUNCTION TRIM(COUNTRY-REC) TO COUNTRY-ITEM(
                        COUNTRY-COUNT)
               END-READ
           END-PERFORM
           CLOSE COUNTRY-FILE

           *> 讀 STATE
           OPEN INPUT STATE-FILE
           PERFORM UNTIL STATE-FILE-EOF = 'Y'
               READ STATE-FILE
                   AT END MOVE 'Y' TO STATE-FILE-EOF
                   NOT AT END
                       ADD 1 TO STATE-COUNT
                       MOVE FUNCTION TRIM(STATE-REC) TO STATE-ITEM(
                        STATE-COUNT)
               END-READ
           END-PERFORM
           CLOSE STATE-FILE

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
           STRING 
               "Chinese;Original;Room/RM;Floor/F;Building;NO;ALY;"
               "LN/Lane;Sec;"
               "Street/St;Avenue/Ave;Way;Boulevard/Blvd;Road/Rd;"
               "Drive/Dr;Town;Dist/District;City;County;Province;State;"
               "Zip;Country;"
               *>"Location1;Location2;Location3;Location4"
               *>";Location5;Location6;"
               "Other;Concat"
               DELIMITED BY SIZE INTO OUT-REC
           WRITE OUT-REC

           *> 跳過首行
           READ IN-FILE AT END MOVE 'Y' TO EOF-FLAG END-READ

           *> 處理每一行
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ IN-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM

           CLOSE IN-FILE OUT-FILE UNMATCH-FILE
           STOP RUN.

       PROCESS-RECORD.
           MOVE 'N' TO HAS-MATCH
           MOVE SPACES TO RM F ALY LN_LANE SEC STREET_ST AVENUE_AVE
               WAY BOULEVARD_BLVD ROAD_RD DRIVE_DR TOWN DISTRICT CITY
               STATE PROVINCE COUNTY ZIP NUMBER-FILED
               LOCATION1 LOCATION2 LOCATION3 LOCATION4 LOCATION5 
               LOCATION6 COUNTRY BUILDING NUMBER-FILED
               OTHER-FIELD CHINESE ORIGINAL CONCAT
       
           UNSTRING IN-REC DELIMITED BY ';'
               INTO CHINESE, ORIGINAL
       
           MOVE ORIGINAL TO REMAINING
       
           *> 以逗號拆段落
           MOVE 1 TO PTR
           MOVE 1 TO IDX
           PERFORM UNTIL PTR > FUNCTION LENGTH(REMAINING) OR IDX > 30
               UNSTRING REMAINING DELIMITED BY "," 
                   INTO FIELD-ENTRY(IDX)
                   WITH POINTER PTR
               END-UNSTRING           
               ADD 1 TO IDX
           END-PERFORM
           
           SUBTRACT 1 FROM IDX GIVING FIELD-COUNT

           *> Keyword 比對
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FIELD-COUNT
               MOVE FUNCTION LENGTH(FIELD-ENTRY(IDX)) TO FIELD-LEN
               MOVE FUNCTION TRIM(FIELD-ENTRY(IDX)) TO TEMP-FIELD
               MOVE FUNCTION LENGTH(TEMP-FIELD) TO TEMP-LEN


               IF TEMP-FIELD NOT = SPACES AND TEMP-LEN >= 1
                   PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > TEMP-LEN
                       PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL 
                       TMP-POS > KEY-COUNT
                       MOVE FUNCTION LENGTH(FUNCTION TRIM
                              (KEY-ENTRY(TMP-POS))) TO KEY-LEN
                           IF FUNCTION UPPER-CASE(TEMP-FIELD
                           (POS: KEY-LEN)
                           ) = FUNCTION UPPER-CASE(FUNCTION TRIM
                              (KEY-ENTRY(TMP-POS)))
                               MOVE 'Y' TO HAS-MATCH
                               PERFORM OUTPUT-KEYWORD
                               *> 把已匹配的 Keyword 從欄位中刪掉
                               MOVE SPACE TO 
                               FIELD-ENTRY(IDX)
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-PERFORM

           *> Location 比對
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FIELD-COUNT
               IF FIELD-ENTRY(IDX) NOT = SPACES

                  *>CITY
                   MOVE FUNCTION LENGTH(FIELD-ENTRY(IDX)) TO FIELD-LEN
                   MOVE FUNCTION TRIM(FIELD-ENTRY(IDX)) TO TEMP-FIELD
                   MOVE FUNCTION LENGTH(TEMP-FIELD) TO TEMP-LEN

                   IF TEMP-FIELD NOT = SPACES AND TEMP-LEN >= 1
                    PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > TEMP-LEN
                      PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL TMP-POS 
                      > CITY-COUNT
                      MOVE FUNCTION LENGTH(FUNCTION TRIM
                              (CITY-ITEM(TMP-POS))) TO KEY-LEN
                       IF FUNCTION UPPER-CASE(TEMP-FIELD
                           (POS: KEY-LEN)
                           ) = FUNCTION UPPER-CASE(FUNCTION TRIM
                              (CITY-ITEM(TMP-POS)))
                           MOVE FIELD-ENTRY(IDX) TO FIELD-FILL
                           PERFORM CHECK-ZIP
                           MOVE TEMP-FIELD TO CITY
                           MOVE SPACES TO FIELD-ENTRY(IDX)
                           MOVE 'Y' TO HAS-MATCH

                           MOVE 0 TO FIELD-LEN
                           MOVE SPACE TO TEMP-FIELD
                           MOVE 0 TO TEMP-LEN

                           EXIT PERFORM
                       END-IF
                       END-PERFORM
                     END-PERFORM
                    END-IF

                   *>STATE
                   PERFORM CHECK-STATE


                   *>COUNTRY
                   PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL TMP-POS > 
                   COUNTRY-COUNT
                       IF FUNCTION UPPER-CASE(FUNCTION TRIM(FIELD-ENTRY
                       (IDX))) =
                          FUNCTION UPPER-CASE(FUNCTION TRIM(
                           COUNTRY-ITEM(TMP-POS)))
                           MOVE FUNCTION TRIM(FIELD-ENTRY(IDX)) 
                           TO COUNTRY
                           MOVE SPACES TO FIELD-ENTRY(IDX)
                           MOVE 'Y' TO HAS-MATCH
                           EXIT PERFORM
                       END-IF
                   END-PERFORM

                   PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL TMP-POS >
                    LOC-COUNT
                       IF FUNCTION TRIM(FIELD-ENTRY(IDX)) =
                          FUNCTION TRIM(LOC-ITEM(TMP-POS))
                           MOVE FIELD-ENTRY(IDX) TO FIELD-FILL
                           PERFORM OUTPUT-LOCATION
                           MOVE SPACES TO FIELD-ENTRY(IDX)
                           MOVE 'Y' TO HAS-MATCH
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

           *> 其他未匹配欄位
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FIELD-COUNT
               IF FUNCTION TRIM(FIELD-ENTRY(IDX)) NOT = SPACES
                   IF FUNCTION TRIM(OTHER-FIELD) NOT = SPACES
                       STRING FUNCTION TRIM(OTHER-FIELD) "," FUNCTION 
                       TRIM(FIELD-ENTRY(IDX))
                              DELIMITED BY SIZE
                              INTO OTHER-FIELD
                       END-STRING
                   ELSE
                       MOVE FUNCTION TRIM(FIELD-ENTRY(IDX)) TO 
                       OTHER-FIELD
                   END-IF
               END-IF
           END-PERFORM

           *> 判斷 unmatched
           IF HAS-MATCH = 'Y'
               PERFORM OUTPUT-COLUMN
           ELSE
               MOVE IN-REC TO UNMATCH-REC
               WRITE UNMATCH-REC
           END-IF.

       OUTPUT-KEYWORD.
           EVALUATE FUNCTION UPPER-CASE(KEY-ENTRY(TMP-POS))
               WHEN 'RM.' MOVE TEMP-FIELD TO RM
               WHEN 'ROOM' MOVE TEMP-FIELD TO RM
               WHEN 'TOWER' MOVE TEMP-FIELD TO BUILDING
               WHEN 'F.' MOVE TEMP-FIELD TO F
               WHEN 'FLOOR' MOVE TEMP-FIELD TO F
               WHEN 'NO.' MOVE TEMP-FIELD TO NUMBER-FILED
               WHEN 'ALY.' MOVE TEMP-FIELD TO ALY
               WHEN 'LN.' MOVE TEMP-FIELD TO LN_LANE
               WHEN 'LANE' MOVE TEMP-FIELD TO LN_LANE
               WHEN 'SEC.' MOVE TEMP-FIELD TO SEC
               WHEN 'SECTION' MOVE TEMP-FIELD TO SEC
               WHEN 'STREET' 
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO STREET_ST
               WHEN 'ST.' 
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO STREET_ST
               WHEN 'AVENUE' 
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO AVENUE_AVE
               WHEN 'AVE.'
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO AVENUE_AVE
               WHEN 'WAY' 
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO WAY
               WHEN 'BOULEVARD'
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO BOULEVARD_BLVD
               WHEN 'BLVD.'
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO BOULEVARD_BLVD
               WHEN 'ROAD' 
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO ROAD_RD
               WHEN 'RD.' 
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO ROAD_RD
               WHEN 'DRIVE' 
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO DRIVE_DR
               WHEN 'DR.'
                 PERFORM CHECK-NO
                 MOVE TEMP-FIELD TO DRIVE_DR
               WHEN 'DIST.' MOVE TEMP-FIELD TO DISTRICT
               WHEN 'DISTRICT' MOVE TEMP-FIELD TO DISTRICT
               WHEN 'PARK' MOVE TEMP-FIELD TO DISTRICT
               WHEN 'AREA' MOVE TEMP-FIELD TO DISTRICT
               WHEN 'TOWN' 
                 PERFORM CHECK-ZIP
                 MOVE TEMP-FIELD TO TOWN
               WHEN 'TOWNSHIP'
                 PERFORM CHECK-ZIP
                 MOVE TEMP-FIELD TO TOWN
               WHEN 'CITY'
                 PERFORM CHECK-ZIP
                 MOVE TEMP-FIELD TO CITY
               WHEN 'STATE' 
                 PERFORM CHECK-ZIP
                 MOVE TEMP-FIELD TO STATE
               WHEN 'PROVINCE' 
                 PERFORM CHECK-ZIP
                 MOVE TEMP-FIELD TO PROVINCE
               WHEN 'COUNTY' MOVE TEMP-FIELD TO COUNTY
                 PERFORM CHECK-ZIP
                 MOVE TEMP-FIELD TO COUNTY
               WHEN 'ZIP' MOVE TEMP-FIELD TO ZIP
               WHEN 'POSTAL' MOVE TEMP-FIELD TO ZIP
               WHEN OTHER CONTINUE
           END-EVALUATE.

       OUTPUT-LOCATION.
           EVALUATE TRUE
               WHEN LOCATION1 = SPACES
                   MOVE FUNCTION TRIM(FIELD-FILL) TO LOCATION1
               WHEN LOCATION2 = SPACES
                   MOVE FUNCTION TRIM(FIELD-FILL) TO LOCATION2
               WHEN LOCATION3 = SPACES
                   MOVE FUNCTION TRIM(FIELD-FILL) TO LOCATION3
               WHEN LOCATION4 = SPACES
                   MOVE FUNCTION TRIM(FIELD-FILL) TO LOCATION4
               WHEN LOCATION5 = SPACES
                   MOVE FUNCTION TRIM(FIELD-FILL) TO LOCATION5
               WHEN LOCATION6 = SPACES
                   MOVE FUNCTION TRIM(FIELD-FILL) TO LOCATION6
           END-EVALUATE.

       OUTPUT-COLUMN.
           MOVE SPACES TO OUT-REC
           MOVE "Y" TO WS-FIRST-FIELD

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 28
                      EVALUATE IDX
                          WHEN 1
                              MOVE RM TO WS-TEMP
                          WHEN 2
                              MOVE F TO WS-TEMP
                          WHEN 3
                              MOVE BUILDING TO WS-TEMP
                          WHEN 4
                              MOVE NUMBER-FILED TO WS-TEMP
                          WHEN 5
                              MOVE ALY TO WS-TEMP
                          WHEN 6
                              MOVE LN_LANE TO WS-TEMP
                          WHEN 7
                              MOVE SEC TO WS-TEMP
                          WHEN 8
                              MOVE STREET_ST TO WS-TEMP
                          WHEN 9
                              MOVE AVENUE_AVE TO WS-TEMP
                          WHEN 10
                              MOVE WAY TO WS-TEMP
                          WHEN 11
                              MOVE BOULEVARD_BLVD TO WS-TEMP
                          WHEN 12
                              MOVE ROAD_RD TO WS-TEMP
                          WHEN 13
                              MOVE DRIVE_DR TO WS-TEMP
                          WHEN 14
                              MOVE TOWN TO WS-TEMP
                          WHEN 15
                              MOVE DISTRICT TO WS-TEMP
                          WHEN 16
                              MOVE CITY TO WS-TEMP
                          WHEN 17
                              MOVE COUNTY TO WS-TEMP
                          WHEN 18
                              MOVE PROVINCE TO WS-TEMP
                          WHEN 19
                              MOVE STATE TO WS-TEMP
                          WHEN 20
                              MOVE ZIP TO WS-TEMP
                          WHEN 21
                              MOVE COUNTRY TO WS-TEMP
                          WHEN 22
                              MOVE LOCATION1 TO WS-TEMP
                          WHEN 23
                              MOVE LOCATION2 TO WS-TEMP
                          WHEN 24
                              MOVE LOCATION3 TO WS-TEMP
                          WHEN 25
                              MOVE LOCATION4 TO WS-TEMP
                          WHEN 26
                              MOVE LOCATION5 TO WS-TEMP
                          WHEN 27
                              MOVE LOCATION6 TO WS-TEMP
                          WHEN 28
                              MOVE OTHER-FIELD TO WS-TEMP
                      END-EVALUATE
           
                      IF FUNCTION TRIM(WS-TEMP) NOT = SPACES
                          IF WS-FIRST-FIELD = 'Y'
                              MOVE FUNCTION TRIM(WS-TEMP) TO CONCAT
                              MOVE 'N' TO WS-FIRST-FIELD
                          ELSE
                              STRING FUNCTION TRIM(CONCAT) "," 
                              FUNCTION TRIM(WS-TEMP)
                                  DELIMITED BY SIZE
                                  INTO CONCAT
                              END-STRING
                          END-IF
                      END-IF
           
           END-PERFORM

           STRING 
           FUNCTION TRIM(CHINESE) ";"FUNCTION TRIM(ORIGINAL) ";"
           FUNCTION TRIM(RM) ";" FUNCTION TRIM(F)";" 
           FUNCTION TRIM(BUILDING)";" 
           FUNCTION TRIM(NUMBER-FILED) ";"
           FUNCTION TRIM(ALY) ";"
                  FUNCTION TRIM(LN_LANE) ";" FUNCTION 
                  TRIM(SEC) ";" FUNCTION TRIM(STREET_ST) ";"
                  FUNCTION TRIM(AVENUE_AVE) ";" FUNCTION 
                  TRIM(WAY) ";" FUNCTION 
                  TRIM(BOULEVARD_BLVD) ";"
                  FUNCTION TRIM(ROAD_RD) ";" FUNCTION 
                  TRIM(DRIVE_DR) ";" FUNCTION TRIM(TOWN) ";"
                  FUNCTION TRIM(DISTRICT) ";" FUNCTION 
                  TRIM(CITY) ";"FUNCTION 
                  TRIM(COUNTY) ";" FUNCTION TRIM(PROVINCE) ";"
                  FUNCTION TRIM(STATE) ";"
                  FUNCTION TRIM(ZIP) ";"FUNCTION TRIM(COUNTRY) ";" 
                  *>FUNCTION TRIM(LOCATION1) ";" FUNCTION 
                  *>TRIM(LOCATION2) ";" FUNCTION 
                  *>TRIM(LOCATION3) ";"
                  *>FUNCTION TRIM(LOCATION4) ";" FUNCTION 
                  *>TRIM(LOCATION5) ";" FUNCTION 
                  *>TRIM(LOCATION6) ";"
                  FUNCTION TRIM(OTHER-FIELD) ";"
                  FUNCTION TRIM(CONCAT) DELIMITED BY
                   SIZE INTO OUT-REC
           WRITE OUT-REC.
       

       CHECK-ZIP.
           IF TEMP-FIELD NOT = SPACES AND ZIP = SPACES
              
               MOVE SPACE TO NO-CHECK-RESULT
       
               MOVE 1 TO NO-PTR
               MOVE 1 TO NO-IDX
       
               *> 用空格拆段落
               PERFORM UNTIL NO-IDX > FUNCTION LENGTH(TEMP-FIELD)
                   UNSTRING TEMP-FIELD DELIMITED BY " "
                       INTO NO-FIELD(NO-IDX)
                       WITH POINTER NO-PTR
                   END-UNSTRING
                   ADD 1 TO NO-IDX
               END-PERFORM

               *> 計算有效段落數量
               MOVE 0 TO NO-CHECK-IDX
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 10
                   IF FUNCTION TRIM(NO-FIELD(NO-IDX)) NOT = SPACES
                       ADD 1 TO NO-CHECK-IDX
                   END-IF
               END-PERFORM

               *> 檢查哪個段落全是數字
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 
               NO-CHECK-IDX 
                   MOVE 'Y' TO IS-NUM
                  PERFORM VARYING NO-IDX2 FROM 1 BY 1
                       UNTIL NO-IDX2 > FUNCTION LENGTH(
                        FUNCTION TRIM(NO-FIELD(NO-IDX)))
                       IF FUNCTION TRIM(NO-FIELD(NO-IDX))(NO-IDX2:1) 
                       IS NOT NUMERIC
                           MOVE 'N' TO IS-NUM
                       END-IF
                   END-PERFORM
                   IF IS-NUM = 'Y' 
                       MOVE FUNCTION TRIM(NO-FIELD(NO-IDX)) TO 
                       ZIP
                       *> 不立即清空陣列，重組時會排除 ZIP
                       MOVE SPACE TO NO-FIELD(NO-IDX)
                   END-IF
               END-PERFORM
       
               *> 把剩下的不是純數字段落重新組回 TEMP-FIELD
               MOVE SPACES TO TEMP-FIELD
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 
               NO-CHECK-IDX
                   IF  FUNCTION TRIM(NO-FIELD(NO-IDX))  NOT = SPACES
                       IF TEMP-FIELD = SPACES
                           MOVE FUNCTION TRIM(NO-FIELD(NO-IDX)) 
                           TO TEMP-FIELD
                       ELSE
                           STRING FUNCTION TRIM(TEMP-FIELD)
                            " " FUNCTION TRIM (NO-FIELD(NO-IDX)) 
                               DELIMITED BY SIZE
                               INTO TEMP-FIELD
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
       
           END-IF.

       CHECK-NO.
           IF TEMP-FIELD NOT = SPACES AND NUMBER-FILED = SPACES
               
               MOVE SPACE TO NO-CHECK-RESULT
       
               MOVE 1 TO NO-PTR
               MOVE 1 TO NO-IDX
       
               *> 用空格拆段落
               PERFORM UNTIL NO-IDX > FUNCTION LENGTH(TEMP-FIELD)
                   UNSTRING TEMP-FIELD DELIMITED BY " "
                       INTO NO-FIELD(NO-IDX)
                       WITH POINTER NO-PTR
                   END-UNSTRING
                   ADD 1 TO NO-IDX
               END-PERFORM

               *> 計算有效段落數量
               MOVE 0 TO NO-CHECK-IDX
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 10
                   IF FUNCTION TRIM(NO-FIELD(NO-IDX)) NOT = SPACES
                       ADD 1 TO NO-CHECK-IDX
                   END-IF
               END-PERFORM

               *> 檢查哪個段落全是數字
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 
               NO-CHECK-IDX
                   MOVE 'Y' TO IS-NUM
                  PERFORM VARYING NO-IDX2 FROM 1 BY 1
                       UNTIL NO-IDX2 > FUNCTION LENGTH(
                        FUNCTION TRIM(NO-FIELD(NO-IDX)))
                       IF FUNCTION TRIM(NO-FIELD(NO-IDX))(NO-IDX2:1) 
                       IS NOT NUMERIC
                           MOVE 'N' TO IS-NUM
                       END-IF
                   END-PERFORM
                   IF IS-NUM = 'Y' 
                       MOVE FUNCTION TRIM(NO-FIELD(NO-IDX)) TO 
                       NUMBER-FILED
                       *> 不立即清空陣列，重組時會排除 ZIP
                       MOVE SPACE TO NO-FIELD(NO-IDX)
                   END-IF
               END-PERFORM
       
               *> 把剩下的不是純數字段落重新組回 TEMP-FIELD
               MOVE SPACES TO TEMP-FIELD
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 
               NO-CHECK-IDX
                   IF  FUNCTION TRIM(NO-FIELD(NO-IDX))  NOT = SPACES
                       IF TEMP-FIELD = SPACES
                           MOVE FUNCTION TRIM(NO-FIELD(NO-IDX)) 
                           TO TEMP-FIELD
                       ELSE
                           STRING FUNCTION TRIM(TEMP-FIELD)
                            " " FUNCTION TRIM (NO-FIELD(NO-IDX)) 
                               DELIMITED BY SIZE
                               INTO TEMP-FIELD
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
       
           END-IF.

       CHECK-STATE.
           IF TEMP-FIELD NOT = SPACES AND STATE = SPACES

               MOVE SPACE TO NO-CHECK-RESULT
       
               MOVE 1 TO NO-PTR
               MOVE 1 TO NO-IDX

               MOVE 'N' TO IS-STATE
       
               *> 用空格拆段落
               PERFORM UNTIL NO-IDX > FUNCTION LENGTH(TEMP-FIELD)
                   UNSTRING TEMP-FIELD DELIMITED BY " "
                       INTO NO-FIELD(NO-IDX)
                       WITH POINTER NO-PTR
                   END-UNSTRING
                   ADD 1 TO NO-IDX
               END-PERFORM

               *> 計算有效段落數量
               MOVE 0 TO NO-CHECK-IDX
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 10
                   IF FUNCTION TRIM(NO-FIELD(NO-IDX)) NOT = SPACES
                       ADD 1 TO NO-CHECK-IDX
                   END-IF
               END-PERFORM

               *> 檢查哪個段落符合STATE
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 
               NO-CHECK-IDX OR IS-STATE = 'Y'

                   PERFORM VARYING NO-IDX2 FROM NO-IDX BY 1 UNTIL 
                   NO-IDX2 > NO-CHECK-IDX 
                   MOVE FUNCTION TRIM(NO-FIELD(NO-IDX)) TO TEMP-FIELD
           
                   *> 組合多段詞
                   IF NO-IDX2 > NO-IDX
                       COMPUTE TMP-START = NO-IDX + 1
                       PERFORM VARYING TMP-POS FROM TMP-START BY 1
                           UNTIL TMP-POS > NO-IDX2
                           STRING FUNCTION TRIM(TEMP-FIELD)
                            " " FUNCTION TRIM(NO-FIELD(TMP-POS))
                               DELIMITED BY SIZE
                               INTO TEMP-FIELD
                           END-STRING
                       END-PERFORM
                   END-IF
                  
                      *> 檢查 TEMP-FIELD 是否等於任何州名稱
                      PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL
                       TMP-POS > STATE-COUNT OR IS-STATE = 'Y'
                          IF FUNCTION UPPER-CASE(FUNCTION TRIM
                          (TEMP-FIELD))
                              = FUNCTION UPPER-CASE(FUNCTION TRIM
                              (STATE-ITEM(TMP-POS)))
                              MOVE FUNCTION TRIM(STATE-ITEM(TMP-POS)) 
                              TO STATE
                              MOVE 'Y' TO HAS-MATCH
                              MOVE 'Y' TO IS-STATE
                              MOVE SPACES TO NO-FIELD(NO-IDX)
                              PERFORM VARYING STATE-IDX FROM TMP-START 
                              BY 1 
                              UNTIL STATE-IDX > NO-IDX2
                                  MOVE SPACES TO NO-FIELD(STATE-IDX)
                              END-PERFORM
                          END-IF
                      END-PERFORM
                  END-PERFORM

               END-PERFORM
       
               *> 把剩下的非STATE的單字組成 TEMP-FIELD
               MOVE SPACES TO TEMP-FIELD
               PERFORM VARYING NO-IDX FROM 1 BY 1 UNTIL NO-IDX > 
               NO-CHECK-IDX
                   IF  FUNCTION TRIM(NO-FIELD(NO-IDX))  NOT = SPACES
                       IF TEMP-FIELD = SPACES
                           MOVE FUNCTION TRIM(NO-FIELD(NO-IDX)) 
                           TO TEMP-FIELD
                       ELSE
                           STRING FUNCTION TRIM(TEMP-FIELD)
                            " " FUNCTION TRIM (NO-FIELD(NO-IDX)) 
                               DELIMITED BY SIZE
                               INTO TEMP-FIELD
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
               
               IF IS-STATE = 'Y'
                     MOVE SPACE TO FIELD-ENTRY(IDX)
                     PERFORM CHECK-ZIP
               END-IF
       
           END-IF.

