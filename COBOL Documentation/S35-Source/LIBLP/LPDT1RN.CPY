      * COPYMEMBER: LIBLP/LPDT1RN
       LOAD-DT1-FILE.
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
       OPEN-DT1-FILE.
           PERFORM LOAD-DT1-FILE.
           PERFORM OPEN-IT.
           MOVE DT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DT1-FILE.
           PERFORM START-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.

           IF ( DT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DT1-FILE.

           IF ( QDT1-WEND-SEQ = ZEROES    ) OR
              ( QDT1-WEND-SEQ NOT NUMERIC )
              MOVE DT-BRNO              TO QDT1-WBEG-BRNO
              MOVE DT-BRNO              TO QDT1-WEND-BRNO
              MOVE DT-NUM               TO QDT1-WBEG-NUM 
              MOVE ALL "9"              TO QDT1-WEND-NUM 
              MOVE DT-CLASS             TO QDT1-WBEG-CLASS
              MOVE ALL "9"              TO QDT1-WEND-CLASS
              MOVE DT-ACCTNO            TO QDT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QDT1-WEND-ACCTNO
              MOVE DT-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE
              MOVE DT-SEQ               TO QDT1-WBEG-SEQ
              MOVE ALL "9"              TO QDT1-WEND-SEQ.

           EXEC SQL
            DECLARE DT1_CURSOR CURSOR FOR
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
            WHERE DTFILE.DT_BRNO = :QDT1-WBEG-BRNO
              AND DTFILE.DT_NUM >= :QDT1-WBEG-NUM 
              AND DTFILE.DT_NUM <= :QDT1-WEND-NUM 
              AND DTFILE.DT_CLASS >= :QDT1-WBEG-CLASS
              AND DTFILE.DT_CLASS <= :QDT1-WEND-CLASS
              AND DTFILE.DT_ACCTNO >= :QDT1-WBEG-ACCTNO
              AND DTFILE.DT_ACCTNO <= :QDT1-WEND-ACCTNO
              AND DTFILE.DT_DATE >= :QDT1-WBEG-DATE 
              AND DTFILE.DT_DATE <= :QDT1-WEND-DATE
              AND DTFILE.DT_SEQ >= :QDT1-WBEG-SEQ
              AND DTFILE.DT_SEQ <= :QDT1-WEND-SEQ
            ORDER BY DTFILE.DT_BRNO,
                     DTFILE.DT_NUM,
                     DTFILE.DT_CLASS,
                     DTFILE.DT_ACCTNO,
                     DTFILE.DT_DATE,
                     DTFILE.DT_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDT1-WBEG-BRNO 
                          QDT1-WEND-BRNO
                          QDT1-WBEG-NUM
                          QDT1-WEND-NUM
                          QDT1-WBEG-CLASS
                          QDT1-WEND-CLASS
                          QDT1-WBEG-ACCTNO
                          QDT1-WEND-ACCTNO
                          QDT1-WBEG-SEQ
                          QDT1-WEND-SEQ.
 
           MOVE SPACES TO QDT1-WBEG-DATE
                          QDT1-WEND-DATE.

           MOVE STAT---GOOD TO DT1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DT1-FILE.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO. 
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.
           INITIALIZE QDT-REC.

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
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
              AND DTFILE.DT_NUM = :QDT-NUM
              AND DTFILE.DT_CLASS = :QDT-CLASS
              AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
              AND DTFILE.DT_DATE = :QDT-DATE
              AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       READ-DT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.
           INITIALIZE QDT-REC.

           IF ( DT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DT1_CURSOR INTO :QDT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-DT1-FILE.

           PERFORM CLOSE-IT.
           IF ( DT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DT1-CURSOR-STAT.

