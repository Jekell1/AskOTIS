      * COPYMEMBER: LIBLP/LPCK1RN
       LOAD-CK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CK-ALL-BASE
              MOVE FILPATH-MACHINE         TO CK-ALL-MACHINE
              MOVE FILPATH-DATA            TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH.

      *-----------------------------------------------------------------
       OPEN-CK1-FILE.
           PERFORM LOAD-CK1-FILE.
           PERFORM OPEN-IT.
           MOVE CK-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CK1-FILE.
           PERFORM START-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           IF ( CK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CK1-FILE.

           IF ( QCK1-WEND-BRNO = ZEROES    ) OR
              ( QCK1-WEND-BRNO NOT NUMERIC )
              MOVE CK-PATH-OWNBR        TO QCK1-WBEG-BRNO
              MOVE CK-PATH-OWNBR        TO QCK1-WEND-BRNO
              MOVE CK-NO                TO QCK1-WBEG-CKNO
              MOVE ALL "9"              TO QCK1-WEND-CKNO.

           EXEC SQL
            DECLARE CK1_CURSOR CURSOR FOR
            SELECT CKFILE.CK_BRNO,
                   CKFILE.CK_NO,
                   CAST(CKFILE.CK_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TYPE,
                   CAST(CKFILE.CK_DATE AS VARCHAR(10)),
                   CKFILE.CK_AMT,
                   CKFILE.CK_PAYEE,
                   CKFILE.CK_ADR1,
                   CKFILE.CK_ADR2,
                   CKFILE.CK_CITY,
                   CKFILE.CK_STATE,
                   CKFILE.CK_ZIP,
                   CKFILE.CK_REF,
                   CKFILE.CK_REASON,
                   CKFILE.CK_GLNO_1,
                   CKFILE.CK_GLAMT_1,
                   CKFILE.CK_GLNO_2,
                   CKFILE.CK_GLAMT_2,
                   CKFILE.CK_GLNO_3,
                   CKFILE.CK_GLAMT_3,
                   CKFILE.CK_GLNO_4,
                   CKFILE.CK_GLAMT_4,
                   CKFILE.CK_GLNO_5,
                   CKFILE.CK_GLAMT_5,
                   CKFILE.CK_VOID,
                   CKFILE.CK_PAID,
                   CKFILE.CK_CLASS,
                   CKFILE.CK_DLNO,
                   CKFILE.CK_PGM,
                   CKFILE.CK_NC_SEQNO,
                   CAST(CKFILE.CK_VOID_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_ACCTNO,
                   CKFILE.CK_DLRVNO,
                   CKFILE.CK_CLEARED_FG,
                   CKFILE.CK_SSNO,
                   CKFILE.CK_BATCHID,
                   CKFILE.CK_UNPAID,
                   CAST(CKFILE.CK_CLEARED_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TILLNO,
                   CKFILE.CK_EVENT_CD,
                   CKFILE.CK_PAYEE_NO,
                   CKFILE.CK_FREE1,
                   CKFILE.CK_FREE2,
                   CKFILE.CK_BWPROC
            FROM DBO.CKFILE
            WHERE CKFILE.CK_BRNO = :QCK1-WBEG-BRNO
              AND CKFILE.CK_NO >= :QCK1-WBEG-CKNO
              AND CKFILE.CK_NO <= :QCK1-WEND-CKNO
              ORDER BY CKFILE.CK_BRNO,
                       CKFILE.CK_NO
           END-EXEC.

           EXEC SQL
               OPEN CK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCK1-WBEG-BRNO 
                          QCK1-WEND-BRNO
                          QCK1-WBEG-CKNO
                          QCK1-WEND-CKNO.

           MOVE STAT---GOOD TO CK1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CK1-FILE.
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           MOVE CK-BRNO   TO QCK-BRNO.
           MOVE CK-NO   TO QCK-NO.

           EXEC SQL
            SELECT CKFILE.CK_BRNO,
                   CKFILE.CK_NO,
                   CAST(CKFILE.CK_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TYPE,
                   CAST(CKFILE.CK_DATE AS VARCHAR(10)),
                   CKFILE.CK_AMT,
                   CKFILE.CK_PAYEE,
                   CKFILE.CK_ADR1,
                   CKFILE.CK_ADR2,
                   CKFILE.CK_CITY,
                   CKFILE.CK_STATE,
                   CKFILE.CK_ZIP,
                   CKFILE.CK_REF,
                   CKFILE.CK_REASON,
                   CKFILE.CK_GLNO_1,
                   CKFILE.CK_GLAMT_1,
                   CKFILE.CK_GLNO_2,
                   CKFILE.CK_GLAMT_2,
                   CKFILE.CK_GLNO_3,
                   CKFILE.CK_GLAMT_3,
                   CKFILE.CK_GLNO_4,
                   CKFILE.CK_GLAMT_4,
                   CKFILE.CK_GLNO_5,
                   CKFILE.CK_GLAMT_5,
                   CKFILE.CK_VOID,
                   CKFILE.CK_PAID,
                   CKFILE.CK_CLASS,
                   CKFILE.CK_DLNO,
                   CKFILE.CK_PGM,
                   CKFILE.CK_NC_SEQNO,
                   CAST(CKFILE.CK_VOID_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_ACCTNO,
                   CKFILE.CK_DLRVNO,
                   CKFILE.CK_CLEARED_FG,
                   CKFILE.CK_SSNO,
                   CKFILE.CK_BATCHID,
                   CKFILE.CK_UNPAID,
                   CAST(CKFILE.CK_CLEARED_DATE
                                                  AS VARCHAR(10)),
                   CKFILE.CK_TILLNO,
                   CKFILE.CK_EVENT_CD,
                   CKFILE.CK_PAYEE_NO,
                   CKFILE.CK_FREE1,
                   CKFILE.CK_FREE2,
                   CKFILE.CK_BWPROC
            INTO :QCK-REC
            FROM DBO.CKFILE
            WHERE CKFILE.CK_BRNO = :QCK-BRNO
              AND CKFILE.CK_NO = :QCK-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

      *-----------------------------------------------------------------
       READ-CK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CK1_CURSOR INTO :QCK-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CK1-FILE.

           PERFORM CLOSE-IT.
           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CK1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CK1-CURSOR-STAT.

