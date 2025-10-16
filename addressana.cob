       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDRESSANA.

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
       01 IN-REC        PIC X(200).

       FD OUT-FILE.
       01 OUT-REC       PIC X(400).

       WORKING-STORAGE SECTION.
       01 WS-LINE       PIC X(500).
       01 TAB           PIC X VALUE "|".
       01 CR-LF         PIC X(2) VALUE X'0D0A'.
       01 IDX           PIC 9(1).

       *> 暫存原始拆分結果
       01 WS-STREET-RAW   PIC X(120).
       01 WS-AVE-RAW      PIC X(120).
       01 WS-CITY-RAW     PIC X(80).
       01 WS-STATE-RAW    PIC X(70).
       01 WS-ZIP-RAW      PIC X(70).
       01 WS-COUNTRY-RAW  PIC X(120).

       *> 輸出 OCCURS，每段最多 35，最多 2 段
       01 OUT-STREET-TABLE.
           05 OUT-STREET   OCCURS 2 TIMES PIC X(35).
       01 OUT-AVE-TABLE.
           05 OUT-AVE      OCCURS 2 TIMES PIC X(35).
       01 OUT-CITY-TABLE.
           05 OUT-CITY     OCCURS 2 TIMES PIC X(35).
       01 OUT-STATE-TABLE.
           05 OUT-STATE    OCCURS 2 TIMES PIC X(35).
       01 OUT-ZIP-TABLE.
           05 OUT-ZIP      OCCURS 2 TIMES PIC X(35).
       01 OUT-COUNTRY-TABLE.
           05 OUT-COUNTRY  OCCURS 2 TIMES PIC X(35).

       01 EOF-SW          PIC X VALUE "N".
           88 END-OF-FILE      VALUE "Y".
           88 NOT-END-OF-FILE  VALUE "N".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT IN-FILE
                OUTPUT OUT-FILE

           PERFORM UNTIL END-OF-FILE
               READ IN-FILE INTO WS-LINE
                   AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM CLEAN-LINE
                   PERFORM SPLIT-BY-KEYWORD
                   PERFORM CUT-FIELDS
                   PERFORM FORMAT-AND-WRITE
               END-READ
           END-PERFORM

           CLOSE IN-FILE OUT-FILE
           STOP RUN.

       CLEAN-LINE.
           INSPECT WS-LINE REPLACING ALL "," BY SPACE.

       SPLIT-BY-KEYWORD.
           MOVE SPACES TO WS-STREET-RAW WS-AVE-RAW WS-CITY-RAW
                          WS-STATE-RAW WS-ZIP-RAW WS-COUNTRY-RAW

           UNSTRING WS-LINE
               DELIMITED BY ALL "Ave" OR ALL "Avenue" OR ALL "AVE"
               OR ALL "ST" OR ALL "St" OR ALL "Street" OR ALL "street"
               INTO WS-STREET-RAW WS-AVE-RAW
           END-UNSTRING

           UNSTRING WS-AVE-RAW
               DELIMITED BY ALL "City" OR ALL "CITY" OR ALL "city"
               INTO WS-AVE-RAW WS-CITY-RAW
           END-UNSTRING

           UNSTRING WS-CITY-RAW
               DELIMITED BY ALL "State" OR ALL "ST" OR ALL "St"
               OR ALL "STATE"
               INTO WS-CITY-RAW WS-STATE-RAW
           END-UNSTRING

           UNSTRING WS-STATE-RAW
               DELIMITED BY ALL "Zip" OR ALL "ZIP" OR ALL "zip"
               INTO WS-STATE-RAW WS-ZIP-RAW
           END-UNSTRING.

       CUT-FIELDS.
           *> 清空 OUT-TABLE
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
               MOVE SPACES TO OUT-STREET(IDX)
               MOVE SPACES TO OUT-AVE(IDX)
               MOVE SPACES TO OUT-CITY(IDX)
               MOVE SPACES TO OUT-STATE(IDX)
               MOVE SPACES TO OUT-ZIP(IDX)
               MOVE SPACES TO OUT-COUNTRY(IDX)
           END-PERFORM
       
           *> 自動切段函數
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
               IF FUNCTION LENGTH(WS-STREET-RAW) >= ((IDX - 1) * 35) + 1
                   MOVE WS-STREET-RAW(((IDX - 1) * 35) + 1 :
                       FUNCTION MIN(35, FUNCTION LENGTH(WS-STREET-RAW)
                        - ((IDX - 1) * 35)))
                       TO OUT-STREET(IDX)
               END-IF
               IF FUNCTION LENGTH(WS-AVE-RAW) >= ((IDX - 1) * 35) + 1
                   MOVE WS-AVE-RAW(((IDX - 1) * 35) + 1 :
                       FUNCTION MIN(35, FUNCTION LENGTH(WS-AVE-RAW)
                        - ((IDX - 1) * 35)))
                       TO OUT-AVE(IDX)
               END-IF
               IF FUNCTION LENGTH(WS-CITY-RAW) >= ((IDX - 1) * 35) + 1
                   MOVE WS-CITY-RAW(((IDX - 1) * 35) + 1 :
                       FUNCTION MIN(35, FUNCTION LENGTH(WS-CITY-RAW)
                        - ((IDX - 1) * 35)))
                       TO OUT-CITY(IDX)
               END-IF
               IF FUNCTION LENGTH(WS-STATE-RAW) >= ((IDX - 1) * 35) + 1
                   MOVE WS-STATE-RAW(((IDX - 1) * 35) + 1 :
                       FUNCTION MIN(35, FUNCTION LENGTH(WS-STATE-RAW)
                        - ((IDX - 1) * 35)))
                       TO OUT-STATE(IDX)
               END-IF
               IF FUNCTION LENGTH(WS-ZIP-RAW) >= ((IDX - 1) * 35) + 1
                   MOVE WS-ZIP-RAW(((IDX - 1) * 35) + 1 :
                       FUNCTION MIN(35, FUNCTION LENGTH(WS-ZIP-RAW)
                        - ((IDX - 1) * 35)))
                       TO OUT-ZIP(IDX)
               END-IF
               IF FUNCTION LENGTH(WS-COUNTRY-RAW) >= 
               ((IDX - 1) * 35) + 1
                   MOVE WS-COUNTRY-RAW(((IDX - 1) * 35) + 1 :
                       FUNCTION MIN(35, FUNCTION LENGTH(WS-COUNTRY-RAW)
                        - ((IDX - 1) * 35)))
                       TO OUT-COUNTRY(IDX)
               END-IF
           END-PERFORM.
       
       FORMAT-AND-WRITE.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
               IF OUT-STREET(IDX) NOT = SPACES
                   MOVE SPACES TO OUT-REC
                   STRING OUT-STREET(IDX) DELIMITED BY SIZE
                          TAB
                          OUT-AVE(IDX) DELIMITED BY SIZE
                          TAB
                          OUT-CITY(IDX) DELIMITED BY SIZE
                          TAB
                          OUT-STATE(IDX) DELIMITED BY SIZE
                          TAB
                          OUT-ZIP(IDX) DELIMITED BY SIZE
                          TAB
                          OUT-COUNTRY(IDX) DELIMITED BY SIZE
                      INTO OUT-REC
                   WRITE OUT-REC
                   DISPLAY OUT-REC
               END-IF
           END-PERFORM.
