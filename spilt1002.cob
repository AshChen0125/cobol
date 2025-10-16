       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddressSplitterFinal.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO 'input_test.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO 'test_1002.csv'
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
       01  IN-REC      PIC X(500).

       FD  OUT-FILE.
       01  OUT-REC     PIC X(1000).

       FD  KEY-FILE.
       01  KEY-REC     PIC X(50).

       FD  LOC-FILE.
       01  LOC-REC     PIC X(100).

       FD  UNMATCH-FILE.
       01  UNMATCH-REC PIC X(500).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG       PIC X VALUE 'N'.
       01 KEY-FILE-EOF   PIC X VALUE 'N'.
       01 LOC-FILE-EOF   PIC X VALUE 'N'.

       01 REMAINING      PIC X(500).
       01 ADDR-OUTPUT    PIC X(1000) VALUE SPACES.
       01 DST-POINTER    PIC 9(4) VALUE 1.

       01 IDX            PIC 9(4).
       01 KEY-COUNT      PIC 9(4) VALUE 0.
       01 LOC-COUNT      PIC 9(4) VALUE 0.

       01 TOKEN          PIC X(500) VALUE SPACES.
       01 TEMP-TOKEN     PIC X(500) VALUE SPACES.

       01 KEY-LEN        PIC 9(4).
       01 LOC-LEN        PIC 9(4).
       01 LOC-POS        PIC 9(4).
       01 MIN-POS        PIC 9(4).
       01 MIN-LEN        PIC 9(4).
       01 FOUND          PIC X VALUE 'N'.
       01 TMP-POS        PIC 9(4).
       01 REM-LEN        PIC 9(4).

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
                       MOVE FUNCTION TRIM(LOC-REC) TO LOC-ITEM
                       (LOC-COUNT)
               END-READ
           END-PERFORM
           CLOSE LOC-FILE

           OPEN INPUT IN-FILE OUTPUT OUT-FILE UNMATCH-FILE

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ IN-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       MOVE IN-REC TO REMAINING
                       *> 清理逗號並合併多空白
                       INSPECT REMAINING REPLACING ALL ',' BY ' '
                       PERFORM VARYING IDX FROM FUNCTION LENGTH(
                        REMAINING) BY -1
                           UNTIL IDX < 1
                           IF REMAINING(IDX:1) = SPACE
                               IF IDX > 1 AND REMAINING(IDX - 1:1
                               ) = SPACE
                                   MOVE SPACES TO REMAINING(IDX:1)
                               END-IF
                           END-IF
                       END-PERFORM

                       MOVE SPACES TO ADDR-OUTPUT
                       MOVE 1 TO DST-POINTER

                       *> 拆分 KEYWORD + LOCATION
                       PERFORM SPLIT-ADDR

                       *> 輸出結果
                       IF DST-POINTER > 1
                           MOVE ADDR-OUTPUT(1:DST-POINTER - 1) TO 
                           OUT-REC
                           WRITE OUT-REC
                       END-IF
               END-READ
           CLOSE IN-FILE OUT-FILE UNMATCH-FILE
           STOP RUN
           END-PERFORM.
       *>-----------------------------------
       *> KEYWORD + LOCATION 拆分段
       *>-----------------------------------
       SPLIT-ADDR.
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(REMAINING)) = 0
               MOVE 'N' TO FOUND
               MOVE 9999 TO MIN-POS
               MOVE 0 TO MIN-LEN
               MOVE FUNCTION LENGTH(FUNCTION TRIM(REMAINING)) TO REM-LEN
       
               *> -----------------------------
               *> 找 KEYWORD
               *> -----------------------------
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > KEY-COUNT
                   IF FUNCTION LENGTH(FUNCTION TRIM(KEY-ENTRY(IDX))) > 0
                       MOVE FUNCTION LENGTH(FUNCTION TRIM(KEY-ENTRY
                       (IDX))) TO KEY-LEN
                       PERFORM VARYING TMP-POS FROM 1 BY 1 UNTIL TMP-POS
                        + KEY-LEN - 1 > REM-LEN
                           IF FUNCTION UPPER-CASE(REMAINING(TMP-POS:
                           KEY-LEN)) =
                              FUNCTION UPPER-CASE(FUNCTION TRIM(
                                KEY-ENTRY(IDX)))
                               IF TMP-POS < MIN-POS
                                   MOVE TMP-POS TO MIN-POS
                                   MOVE KEY-LEN TO MIN-LEN
                                   MOVE 'Y' TO FOUND
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
       
               IF FOUND = 'Y'
                   *> 整段 KEYWORD 拆出，加分號
                   MOVE FUNCTION TRIM(REMAINING(MIN-POS:MIN-LEN)) TO
                    TOKEN
                   STRING
                       TOKEN DELIMITED BY SIZE
                       ";" DELIMITED BY SIZE
                       INTO ADDR-OUTPUT
                       WITH POINTER DST-POINTER
                   END-STRING
       
                   *> 剩餘文字更新（保留 KEYWORD 前後文字）
                   IF MIN-POS > 1
                       MOVE FUNCTION TRIM(REMAINING(1:MIN-POS - 1)) TO 
                       TEMP-TOKEN
                       IF FUNCTION LENGTH(TEMP-TOKEN) > 0
                           STRING
                               TEMP-TOKEN DELIMITED BY SIZE
                               ";" DELIMITED BY SIZE
                               INTO ADDR-OUTPUT
                               WITH POINTER DST-POINTER
                           END-STRING
                       END-IF
                   END-IF
       
                   IF MIN-POS + MIN-LEN <= REM-LEN
                       MOVE FUNCTION TRIM(REMAINING(MIN-POS + MIN-LEN:))
                        TO REMAINING
                   ELSE
                       MOVE SPACES TO REMAINING
                   END-IF
       
               ELSE
                   *> -----------------------------
                   *> 找 LOCATION
                   *> -----------------------------
                   MOVE FUNCTION TRIM(REMAINING) TO TOKEN
                   MOVE FUNCTION LENGTH(TOKEN) TO REM-LEN
                   MOVE 1 TO TMP-POS
       
                   PERFORM UNTIL TMP-POS > REM-LEN
                       MOVE 9999 TO MIN-POS
                       MOVE 0 TO MIN-LEN
                       MOVE 'N' TO FOUND
       
                       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
                       LOC-COUNT
                           IF FUNCTION LENGTH(FUNCTION TRIM(
                            LOC-ITEM(IDX))) > 0
                               MOVE FUNCTION LENGTH(FUNCTION TRIM(
                                LOC-ITEM(IDX))) TO LOC-LEN
                               PERFORM VARYING LOC-POS FROM TMP-POS BY 
                               1 UNTIL LOC-POS + LOC-LEN - 1 > REM-LEN
                                   IF FUNCTION UPPER-CASE(TOKEN(LOC-POS:
                                   LOC-LEN)) =
                                      FUNCTION UPPER-CASE(FUNCTION TRIM(
                                        LOC-ITEM(IDX)))
                                       IF LOC-POS < MIN-POS
                                           MOVE LOC-POS TO MIN-POS
                                           MOVE LOC-LEN TO MIN-LEN
                                           MOVE 'Y' TO FOUND
                                       END-IF
                                   END-IF
                               END-PERFORM
                           END-IF
                       END-PERFORM
       
                       IF FOUND = 'Y'
                           *> LOCATION 前面文字（非空格）拆出
                           IF MIN-POS > TMP-POS
                               MOVE FUNCTION TRIM(TOKEN(TMP-POS:MIN-POS 
                               - TMP-POS)) TO TEMP-TOKEN
                               IF FUNCTION LENGTH(TEMP-TOKEN) > 0
                                   STRING
                                       TEMP-TOKEN DELIMITED BY SIZE
                                       ";" DELIMITED BY SIZE
                                       INTO ADDR-OUTPUT
                                       WITH POINTER DST-POINTER
                                   END-STRING
                               END-IF
                           END-IF
       
                           *> LOCATION 本身拆出
                           MOVE FUNCTION TRIM(TOKEN(MIN-POS:MIN-LEN)) 
                           TO TEMP-TOKEN
                           STRING
                               TEMP-TOKEN DELIMITED BY SIZE
                               ";" DELIMITED BY SIZE
                               INTO ADDR-OUTPUT
                               WITH POINTER DST-POINTER
                           END-STRING
       
                           COMPUTE TMP-POS = MIN-POS + MIN-LEN
                       ELSE
                           *> 找不到 KEYWORD/LOCATION → unmatched
                           MOVE FUNCTION TRIM(TOKEN(TMP-POS:)) TO 
                           UNMATCH-REC
                           WRITE UNMATCH-REC
                           MOVE SPACES TO REMAINING
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
       
                   MOVE SPACES TO REMAINING
               END-IF
           END-PERFORM.

           
