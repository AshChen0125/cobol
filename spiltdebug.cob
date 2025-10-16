       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECKZIPTEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 TEMP-FIELD         PIC X(100).
       77 NO-CHECK-TOKEN     PIC X(100).
       77 ZIP                PIC X(10).
       01 NO-ARRAY.
           05 NO-FIELD OCCURS 10 TIMES PIC X(20).
       77 NO-PTR             PIC 9(4) VALUE 1.
       77 NO-IDX             PIC 9(4) VALUE 1.
       77 NO-CHECK-IDX       PIC 9(4) VALUE 0.
       77 NO-IDX2            PIC 9(4) VALUE 1.
       01 FIELD-TRIMMED      PIC X(20) .
       01 IS-NUM             PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           *> 測試輸入
           MOVE "Main St 10087" TO TEMP-FIELD
           PERFORM CHECK-ZIP
           STOP RUN.

       CHECK-ZIP.
           IF TEMP-FIELD NOT = SPACES AND ZIP = SPACES

               *> 保存原始文字
               MOVE TEMP-FIELD TO NO-CHECK-TOKEN
               DISPLAY "NO-CHECK-TOKEN: " NO-CHECK-TOKEN

               *> 初始化
               MOVE 1 TO NO-PTR
               MOVE 1 TO NO-IDX

               *> 拆段落到陣列
               PERFORM UNTIL NO-IDX > FUNCTION LENGTH(NO-CHECK-TOKEN)
                   UNSTRING NO-CHECK-TOKEN DELIMITED BY " "
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
               DISPLAY "NO-CHECK-IDX: " NO-CHECK-IDX

               *> 逐字檢查 ZIP
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
                       MOVE FUNCTION TRIM(NO-FIELD(NO-IDX)) TO ZIP
                       *> 不立即清空陣列，重組時會排除 ZIP
                       MOVE SPACE TO NO-FIELD(NO-IDX)
                   END-IF
               END-PERFORM

               *> 重組剩餘地址
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

               DISPLAY "AFTER TEMP-FIELD: " TEMP-FIELD
               DISPLAY "AFTER ZIP: " ZIP

           END-IF.
