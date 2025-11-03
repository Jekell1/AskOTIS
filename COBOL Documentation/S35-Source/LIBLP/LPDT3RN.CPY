      * COPYMEMBER: LIBLP/LPDT3RN
       LOAD-DT3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DT-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DT-ALL-BASE
              MOVE FILPATH-MACHINE         TO DT-ALL-MACHINE
              MOVE FILPATH-DATA            TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH.

      *-----------------------------------------------------------------
       OPEN-DT3-FILE.
           PERFORM LOAD-DT3-FILE.
           PERFORM OPEN-IT.
           MOVE DT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DT3-FILE.
           PERFORM START-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE DT3-KEY        TO E-KEYX.

           IF ( DT3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DT3-FILE.

           IF ( QDT3-WEND-SEQ = ZEROES    ) OR
              ( QDT3-WEND-SEQ NOT NUMERIC )
              MOVE DT-STATUS            TO QDT3-WBEG-STATUS
              MOVE ALL "Z"              TO QDT3-WEND-STATUS
              MOVE DT-BRNO              TO QDT3-WBEG-BRNO
              MOVE DT-BRNO              TO QDT3-WEND-BRNO
              MOVE DT-NUM               TO QDT3-WBEG-NUM 
              MOVE ALL "9"              TO QDT3-WEND-NUM 
              MOVE DT-CLASS             TO QDT3-WBEG-CLASS
              MOVE ALL "9"              TO QDT3-WEND-CLASS
              MOVE DT-ACCTNO            TO QDT3-WBEG-ACCTNO
              MOVE ALL "9"              TO QDT3-WEND-ACCTNO
              MOVE DT-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT3-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT3-WEND-DATE
              MOVE DT-SEQ               TO QDT3-WBEG-SEQ
              MOVE ALL "9"              TO QDT3-WEND-SEQ.

           EXEC SQL
            DECLARE DT3_CURSOR CURSOR FOR
            SELECT DTFILE.DT_STATUS,
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   CAST(DTFILE.DT_DATE AS VARCHAR(10)),
                   DTFILE.DT_SEQ,
                   CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DTFILE.DT_TYPE,
                   DTFILE.DT_LPSEQNO,
                   DTFILE.DT_TRCD,
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   DTFILE.DT_PAID,
                   DTFILE.DT_REVERSE,
                   DTFILE.DT_REVERSE_TRCD,
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   DTFILE.DT_SELECTED,
                   DTFILE.DT_P4P5_TRCD,
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   DTFILE.DT_CHECK_VOID,
                   DTFILE.DT_UNPAID,
                   DTFILE.DT_MEMO,
                   DTFILE.DT_EVENT_CD,
                   DTFILE.DT_PART_HELD_STATUS
            FROM DBO.DTFILE
            WHERE DTFILE.DT_STATUS >= :QDT3-WBEG-STATUS
              AND DTFILE.DT_STATUS <= :QDT3-WEND-STATUS
              AND DTFILE.DT_BRNO = :QDT3-WBEG-BRNO
              AND DTFILE.DT_NUM >= :QDT3-WBEG-NUM 
              AND DTFILE.DT_NUM <= :QDT3-WEND-NUM 
              AND DTFILE.DT_CLASS >= :QDT3-WBEG-CLASS
              AND DTFILE.DT_CLASS <= :QDT3-WEND-CLASS
              AND DTFILE.DT_ACCTNO >= :QDT3-WBEG-ACCTNO
              AND DTFILE.DT_ACCTNO <= :QDT3-WEND-ACCTNO
              AND DTFILE.DT_DATE >= :QDT3-WBEG-DATE 
              AND DTFILE.DT_DATE <= :QDT3-WEND-DATE
              AND DTFILE.DT_SEQ >= :QDT3-WBEG-SEQ
              AND DTFILE.DT_SEQ <= :QDT3-WEND-SEQ
            ORDER BY DTFILE.DT_STATUS,
                     DTFILE.DT_BRNO,
                     DTFILE.DT_NUM,
                     DTFILE.DT_CLASS,
                     DTFILE.DT_ACCTNO,
                     DTFILE.DT_DATE,
                     DTFILE.DT_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DT3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDT3-WBEG-BRNO 
                          QDT3-WEND-BRNO
                          QDT3-WBEG-NUM
                          QDT3-WEND-NUM
                          QDT3-WBEG-CLASS
                          QDT3-WEND-CLASS
                          QDT3-WBEG-ACCTNO
                          QDT3-WEND-ACCTNO
                          QDT3-WBEG-SEQ
                          QDT3-WEND-SEQ.
 
           MOVE SPACES TO QDT3-WBEG-DATE
                          QDT3-WEND-DATE
                          QDT3-WBEG-STATUS
                          QDT3-WEND-STATUS.

           MOVE STAT---GOOD TO DT3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DT3-FILE.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO. 
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE DT3-KEY        TO E-KEYX.
           INITIALIZE QDT-REC.

           MOVE DT-STATUS TO QDT-STATUS.
           MOVE DT-BRNO   TO QDT-BRNO.
           MOVE DT-NUM    TO QDT-NUM.
           MOVE DT-CLASS  TO QDT-CLASS.
           MOVE DT-ACCTNO TO QDT-ACCTNO.
           MOVE DT-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QDT-DATE.
           MOVE DT-SEQ    TO QDT-SEQ.

           EXEC SQL
            SELECT DTFILE.DT_STATUS,
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   CAST(DTFILE.DT_DATE AS VARCHAR(10)),
                   DTFILE.DT_SEQ,
                   CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   DTFILE.DT_TYPE,
                   DTFILE.DT_LPSEQNO,
                   DTFILE.DT_TRCD,
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   DTFILE.DT_PAID,
                   DTFILE.DT_REVERSE,
                   DTFILE.DT_REVERSE_TRCD,
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   DTFILE.DT_SELECTED,
                   DTFILE.DT_P4P5_TRCD,
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   DTFILE.DT_CHECK_VOID,
                   DTFILE.DT_UNPAID,
                   DTFILE.DT_MEMO,
                   DTFILE.DT_EVENT_CD,
                   DTFILE.DT_PART_HELD_STATUS
            INTO :QDT-REC
            FROM DBO.DTFILE
            WHERE DTFILE.DT_STATUS = :QDT-STATUS
              AND DTFILE.DT_BRNO = :QDT-BRNO
              AND DTFILE.DT_NUM = :QDT-NUM
              AND DTFILE.DT_CLASS = :QDT-CLASS
              AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
              AND DTFILE.DT_DATE = :QDT-DATE
              AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       READ-DT3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE DT3-KEY        TO E-KEYX.
           INITIALIZE QDT-REC.

           IF ( DT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DT3_CURSOR INTO :QDT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-DT3-FILE.

           PERFORM CLOSE-IT.
           IF ( DT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DT3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DT3-CURSOR-STAT.

