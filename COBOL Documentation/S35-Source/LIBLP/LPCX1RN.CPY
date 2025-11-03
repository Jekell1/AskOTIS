      * COPYMEMBER: LIBLP/LPCX1RN
       LOAD-CX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CX-ALL-DATA
              MOVE CX-ALL-PATH             TO CX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CX-ALL-BASE
              MOVE FILPATH-MACHINE         TO CX-ALL-MACHINE
              MOVE FILPATH-DATA            TO CX-ALL-DATA
              MOVE CX-ALL-PATH             TO CX-PATH.

      *-----------------------------------------------------------------
       OPEN-CX1-FILE.
           PERFORM LOAD-CX1-FILE.
           PERFORM OPEN-IT.
           MOVE CX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CX1-FILE.
           PERFORM START-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO.
           MOVE CX-PATH-SQL    TO E-FILE.
           MOVE CX1-KEY        TO E-KEYX.

           IF ( CX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CX1-FILE.

           IF ( QCX1-WEND-SEQNO = ZEROES    ) OR
              ( QCX1-WEND-SEQNO NOT NUMERIC )
              MOVE CX-PATH-OWNBR        TO QCX1-WBEG-BRNO
              MOVE CX-PATH-OWNBR        TO QCX1-WEND-BRNO
              MOVE CX-TYPE              TO QCX1-WBEG-TYPE
              MOVE ALL "Z"              TO QCX1-WEND-TYPE
              MOVE CX-SEQNO             TO QCX1-WBEG-SEQNO
              MOVE ALL "9"              TO QCX1-WEND-SEQNO.

           EXEC SQL
            DECLARE CX1_CURSOR CURSOR FOR
            SELECT CXFILE.CX_BRNO,
                   CXFILE.CX_TYPE,
                   CXFILE.CX_SEQNO,
                   CXFILE.CX_PAYEE,
                   CAST(CXFILE.CX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CXFILE.CX_ADR1,
                   CXFILE.CX_ADR2,
                   CXFILE.CX_CITY,
                   CXFILE.CX_STATE,
                   CXFILE.CX_ZIP,
                   CXFILE.CX_AMT,
                   CXFILE.CX_REF,
                   CXFILE.CX_REASON,
                   CXFILE.CX_GLNO_1,
                   CXFILE.CX_GLAMT_1,
                   CXFILE.CX_GLNO_2,
                   CXFILE.CX_GLAMT_2,
                   CXFILE.CX_GLNO_3,
                   CXFILE.CX_GLAMT_3,
                   CXFILE.CX_GLNO_4,
                   CXFILE.CX_GLAMT_4,
                   CXFILE.CX_GLNO_5,
                   CXFILE.CX_GLAMT_5,
                   CXFILE.CX_FREE1,
                   CXFILE.CX_FREE2,
                   CXFILE.CX_VOID,
                   CXFILE.CX_CLASS,
                   CXFILE.CX_CKNO,
                   CXFILE.CX_DLNO,
                   CAST(CXFILE.CX_DATE AS VARCHAR(10)),
                   CXFILE.CX_PAID,
                   CXFILE.CX_PGM,
                   CXFILE.CX_BATCHID,
                   CXFILE.CX_BWPROC,
                   CXFILE.CX_CKNO_NEW,
                   CXFILE.CX_DLRVNO,
                   CXFILE.CX_DT_SEQ,
                   CXFILE.CX_RESCISSION,
                   CXFILE.CX_TILLNO,
                   CXFILE.CX_ACCTNO,
                   CXFILE.CX_SERVACCT,
                   CAST(CXFILE.CX_DT_DATE AS VARCHAR(10)),
                   CXFILE.CX_SSNO,
                   CXFILE.CX_DT_PAID,
                   CXFILE.CX_DT_UNPAID,
                   CXFILE.CX_CKPAID_NEW,
                   CXFILE.CX_EVENT_CD,
                   CXFILE.CX_PAYEE_NO,
                   CXFILE.CX_DEPOSIT_WITHDR
            FROM DBO.CXFILE
            WHERE CXFILE.CX_BRNO  = :QCX1-WBEG-BRNO
              AND CXFILE.CX_TYPE >= :QCX1-WBEG-TYPE
              AND CXFILE.CX_TYPE <= :QCX1-WEND-TYPE
              AND CXFILE.CX_SEQNO >= :QCX1-WBEG-SEQNO
              AND CXFILE.CX_SEQNO <= :QCX1-WEND-SEQNO
            ORDER BY CXFILE.CX_BRNO,
                     CXFILE.CX_TYPE,
                     CXFILE.CX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCX1-WBEG-BRNO 
                          QCX1-WEND-BRNO
                          QCX1-WBEG-SEQNO
                          QCX1-WEND-SEQNO.
           MOVE SPACES TO QCX1-WBEG-TYPE
                          QCX1-WEND-TYPE.

           MOVE STAT---GOOD TO CX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CX1-FILE.
           PERFORM READ-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO. 
           MOVE CX-PATH-SQL    TO E-FILE.
           MOVE CX1-KEY        TO E-KEYX.
           INITIALIZE QCX-REC.

           MOVE CX-BRNO   TO QCX-BRNO.
           MOVE CX-TYPE   TO QCX-TYPE.
           MOVE CX-SEQNO  TO QCX-SEQNO.

           EXEC SQL
            SELECT CXFILE.CX_BRNO,
                   CXFILE.CX_TYPE,
                   CXFILE.CX_SEQNO,
                   CXFILE.CX_PAYEE,
                   CAST(CXFILE.CX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CXFILE.CX_ADR1,
                   CXFILE.CX_ADR2,
                   CXFILE.CX_CITY,
                   CXFILE.CX_STATE,
                   CXFILE.CX_ZIP,
                   CXFILE.CX_AMT,
                   CXFILE.CX_REF,
                   CXFILE.CX_REASON,
                   CXFILE.CX_GLNO_1,
                   CXFILE.CX_GLAMT_1,
                   CXFILE.CX_GLNO_2,
                   CXFILE.CX_GLAMT_2,
                   CXFILE.CX_GLNO_3,
                   CXFILE.CX_GLAMT_3,
                   CXFILE.CX_GLNO_4,
                   CXFILE.CX_GLAMT_4,
                   CXFILE.CX_GLNO_5,
                   CXFILE.CX_GLAMT_5,
                   CXFILE.CX_FREE1,
                   CXFILE.CX_FREE2,
                   CXFILE.CX_VOID,
                   CXFILE.CX_CLASS,
                   CXFILE.CX_CKNO,
                   CXFILE.CX_DLNO,
                   CAST(CXFILE.CX_DATE AS VARCHAR(10)),
                   CXFILE.CX_PAID,
                   CXFILE.CX_PGM,
                   CXFILE.CX_BATCHID,
                   CXFILE.CX_BWPROC,
                   CXFILE.CX_CKNO_NEW,
                   CXFILE.CX_DLRVNO,
                   CXFILE.CX_DT_SEQ,
                   CXFILE.CX_RESCISSION,
                   CXFILE.CX_TILLNO,
                   CXFILE.CX_ACCTNO,
                   CXFILE.CX_SERVACCT,
                   CAST(CXFILE.CX_DT_DATE AS VARCHAR(10)),
                   CXFILE.CX_SSNO,
                   CXFILE.CX_DT_PAID,
                   CXFILE.CX_DT_UNPAID,
                   CXFILE.CX_CKPAID_NEW,
                   CXFILE.CX_EVENT_CD,
                   CXFILE.CX_PAYEE_NO,
                   CXFILE.CX_DEPOSIT_WITHDR
            INTO :QCX-REC
            FROM DBO.CXFILE
            WHERE CXFILE.CX_BRNO = :QCX-BRNO
              AND CXFILE.CX_TYPE = :QCX-TYPE
              AND CXFILE.CX_SEQNO = :QCX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CX-FIELDS.

      *-----------------------------------------------------------------
       READ-CX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO.
           MOVE CX-PATH-SQL    TO E-FILE.
           MOVE CX1-KEY        TO E-KEYX.
           INITIALIZE QCX-REC.

           IF ( CX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CX1_CURSOR INTO :QCX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CX1-FILE.

           PERFORM CLOSE-IT.
           IF ( CX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CX1-CURSOR-STAT.

