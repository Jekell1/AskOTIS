      * COPYMEMBER: LIBLP/LPLM1RN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LM1-FILE.
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

           IF ( QLM1-WEND-BRNO = ZEROES    ) OR
              ( QLM1-WEND-BRNO NOT NUMERIC )
              MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
              MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
              MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLM1-WEND-ACCTNO
              MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QLM1-WEND-DATE-REV
              MOVE LM-SEQ               TO QLM1-WBEG-SEQ
              MOVE ALL "9"              TO QLM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LM1_CURSOR CURSOR FOR
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            FROM DBO.LMFILE
            WHERE    LMFILE.LM_BRNO           = :QLM1-WBEG-BRNO
              AND (  LMFILE.LM_ACCTNO        >= :QLM1-WBEG-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED >= :QLM1-WBEG-DATE-REV
              AND    LMFILE.LM_SEQ           >= :QLM1-WBEG-SEQ       )
              AND (  LMFILE.LM_ACCTNO        <= :QLM1-WEND-ACCTNO
              AND    LMFILE.LM_DATE_REVERSED <= :QLM1-WEND-DATE-REV
              AND    LMFILE.LM_SEQ           <= :QLM1-WEND-SEQ       )
            ORDER BY LMFILE.LM_BRNO,
                     LMFILE.LM_ACCTNO,
                     LMFILE.LM_DATE_REVERSED,
                     LMFILE.LM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ.

           MOVE STAT---GOOD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LM1-FILE.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO. 
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           MOVE LM-BRNO          TO QLM-BRNO.
           MOVE LM-ACCTNO        TO QLM-ACCTNO.
           MOVE LM-DATE-REVERSED TO QLM-DATE-REVERSED.
           MOVE LM-SEQ           TO QLM-SEQ.

           EXEC SQL
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            INTO :QLM-REC
            FROM DBO.LMFILE
            WHERE LMFILE.LM_BRNO = :QLM-BRNO
              AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
              AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
              AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       READ-LM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LM1_CURSOR INTO :QLM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.
           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-STAT.

