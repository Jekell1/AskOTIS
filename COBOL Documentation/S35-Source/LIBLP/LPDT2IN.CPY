      * COPYMEMBER: LIBLP/LPDT2IN
       LOAD-DT2-FILE.
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
       OPEN-DT2-FILE.
           PERFORM LOAD-DT2-FILE.
           PERFORM OPEN-IT.
           MOVE DT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DT2-FILE.
           PERFORM START-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.

           IF ( DT2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DT2-FILE.

           IF ( QDT2-WEND-BATCHID-SEQ = ZEROES    ) OR
              ( QDT2-WEND-BATCHID-SEQ NOT NUMERIC )
              MOVE DT-BRNO              TO QDT2-WBEG-BRNO
              MOVE DT-BRNO              TO QDT2-WEND-BRNO
              MOVE DT-BATCHID           TO QDT2-WBEG-BATCHID
              MOVE ALL "9"              TO QDT2-WEND-BATCHID
              MOVE DT-BATCHID-SEQ       TO QDT2-WBEG-BATCHID-SEQ
              MOVE ALL "9"              TO QDT2-WEND-BATCHID-SEQ.

           EXEC SQL
            DECLARE DT2_CURSOR CURSOR FOR
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
            WHERE DTFILE.DT_BRNO = :QDT2-WBEG-BRNO
              AND DTFILE.DT_BATCHID >= :QDT2-WBEG-BATCHID
              AND DTFILE.DT_BATCHID <= :QDT2-WEND-BATCHID
              AND DTFILE.DT_BATCHID_SEQ >= :QDT2-WBEG-BATCHID-SEQ
              AND DTFILE.DT_BATCHID_SEQ <= :QDT2-WEND-BATCHID-SEQ
            ORDER BY DTFILE.DT_BRNO,
                     DTFILE.DT_BATCHID,
                     DTFILE.DT_BATCHID_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DT2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDT2-WBEG-BRNO 
                          QDT2-WEND-BRNO
                          QDT2-WBEG-BATCHID
                          QDT2-WEND-BATCHID
                          QDT2-WBEG-BATCHID-SEQ
                          QDT2-WEND-BATCHID-SEQ.

           MOVE STAT---GOOD TO DT2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DT2-FILE.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO. 
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QDT-REC.

           MOVE DT-BRNO         TO QDT-BRNO.
           MOVE DT-BATCHID      TO QDT-BATCHID.
           MOVE DT-BATCHID-SEQ  TO QDT-BATCHID-SEQ.

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
              AND DTFILE.DT_BATCHID = :QDT-BATCHID
              AND DTFILE.DT_BATCHID_SEQ = :QDT-BATCHID-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       READ-DT2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QDT-REC.

           IF ( DT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DT2_CURSOR INTO :QDT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-DT2-FILE.

           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           ACCEPT DT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DT-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.
           PERFORM SET-DT-FIELDS.

           EXEC SQL
            UPDATE DBO.DTFILE
             SET
              DTFILE.DT_STATUS = :QDT-STATUS,
              DTFILE.DT_BRNO = :QDT-BRNO,
              DTFILE.DT_NUM = :QDT-NUM,
              DTFILE.DT_CLASS = :QDT-CLASS,
              DTFILE.DT_ACCTNO = :QDT-ACCTNO,
              DTFILE.DT_DATE = :QDT-DATE,
              DTFILE.DT_SEQ = :QDT-SEQ,
              DTFILE.DT_LTOUCH_DATE = :QDT-LTOUCH-DATE,
              DTFILE.DT_TYPE = :QDT-TYPE,
              DTFILE.DT_LPSEQNO = :QDT-LPSEQNO,
              DTFILE.DT_TRCD = :QDT-TRCD,
              DTFILE.DT_DLRVPRIN_1 = :QDT-DLRVPRIN-1,
              DTFILE.DT_DLRVPRIN_2 = :QDT-DLRVPRIN-2,
              DTFILE.DT_DLRVPRIN_3 = :QDT-DLRVPRIN-3,
              DTFILE.DT_DLRVCHGS_1 = :QDT-DLRVCHGS-1,
              DTFILE.DT_DLRVCHGS_2 = :QDT-DLRVCHGS-2,
              DTFILE.DT_DLRVCHGS_3 = :QDT-DLRVCHGS-3,
              DTFILE.DT_DLPARVHELD = :QDT-DLPARVHELD,
              DTFILE.DT_DLPARVPAID = :QDT-DLPARVPAID,
              DTFILE.DT_PROCEEDS = :QDT-PROCEEDS,
              DTFILE.DT_RELEASED = :QDT-RELEASED,
              DTFILE.DT_PAYMENTS = :QDT-PAYMENTS,
              DTFILE.DT_CKNO = :QDT-CKNO,
              DTFILE.DT_CKDATE = :QDT-CKDATE,
              DTFILE.DT_BATCHID = :QDT-BATCHID,
              DTFILE.DT_BATCHID_SEQ = :QDT-BATCHID-SEQ,
              DTFILE.DT_PAID = :QDT-PAID,
              DTFILE.DT_REVERSE = :QDT-REVERSE,
              DTFILE.DT_REVERSE_TRCD = :QDT-REVERSE-TRCD,
              DTFILE.DT_PURBAL = :QDT-PURBAL,
              DTFILE.DT_APCUR = :QDT-APCUR,
              DTFILE.DT_DLRVNO = :QDT-DLRVNO,
              DTFILE.DT_SELECTED = :QDT-SELECTED,
              DTFILE.DT_P4P5_TRCD = :QDT-P4P5-TRCD,
              DTFILE.DT_DLRVPRIN_PAY = :QDT-DLRVPRIN-PAY,
              DTFILE.DT_DLRVCHGS_PAY = :QDT-DLRVCHGS-PAY,
              DTFILE.DT_DLPARVHELD_PAY = :QDT-DLPARVHELD-PAY,
              DTFILE.DT_CHECK_VOID = :QDT-CHECK-VOID,
              DTFILE.DT_UNPAID = :QDT-UNPAID,
              DTFILE.DT_MEMO = :QDT-MEMO,
              DTFILE.DT_EVENT_CD = :QDT-EVENT-CD,
              DTFILE.DT_PART_HELD_STATUS = :QDT-PART-HELD-STATUS
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
               AND DTFILE.DT_NUM = :QDT-NUM
               AND DTFILE.DT_CLASS = :QDT-CLASS
               AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
               AND DTFILE.DT_DATE = :QDT-DATE
               AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DT2-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       CLOSE-DT2-FILE.

           PERFORM CLOSE-IT.

           IF ( DT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DT2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DT1-CURSOR-STAT.

