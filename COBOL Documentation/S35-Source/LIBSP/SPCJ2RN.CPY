      * COPYMEMBER: LIBSP/SPCJ2RN
       LOAD-CJ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH.

      *-----------------------------------------------------------------
       OPEN-CJ2-FILE.
           PERFORM LOAD-CJ2-FILE.
           PERFORM OPEN-IT.
           MOVE CJ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CJ2-FILE.
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.

           IF ( CJ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ2-FILE.

           IF ( QCJ2-WEND-START-TIME = ZEROES    ) OR
              ( QCJ2-WEND-START-TIME NOT NUMERIC )
              MOVE CJ-BRNO              TO QCJ2-WBEG-BRNO
              MOVE CJ-BRNO              TO QCJ2-WEND-BRNO
              MOVE CJ-ACCTNO            TO QCJ2-WBEG-ACCTNO 
              MOVE ALL "9"              TO QCJ2-WEND-ACCTNO 
              MOVE CJ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WEND-DATE
              MOVE CJ-START-TIME        TO QCJ2-WBEG-START-TIME
              MOVE ALL "9"              TO QCJ2-WEND-START-TIME.

           EXEC SQL
            DECLARE CJ2_CURSOR CURSOR FOR
            SELECT CJFILE.CJ_BRNO,
                   CJFILE.CJ_COLLID,
                   CAST(CJFILE.CJ_DATE AS VARCHAR(10)),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   CAST(CJFILE.CJ_LTOUCH_DATE AS VARCHAR(10)),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   CJFILE.CJ_ACTIVITY_CD,
                   CJFILE.CJ_RESULT_CD,
                   CJFILE.CJ_ACTION_CD,
                   CJFILE.CJ_NEW_COLLID,
                   CJFILE.CJ_CON_STAT,
                   CJFILE.CJ_REC_STAT,
                   CJFILE.CJ_FORM_NO,
                   CAST(CJFILE.CJ_PROMISE_DATE1 AS VARCHAR(10)),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   CAST(CJFILE.CJ_PROMISE_DATE2 AS VARCHAR(10)),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   CJFILE.CJ_QUEUE
            FROM DBO.CJFILE
            WHERE CJFILE.CJ_BRNO        = :QCJ2-WBEG-BRNO 
              AND CJFILE.CJ_ACCTNO     >= :QCJ2-WBEG-ACCTNO
              AND CJFILE.CJ_ACCTNO     <= :QCJ2-WEND-ACCTNO
              AND CJFILE.CJ_DATE       >= :QCJ2-WBEG-DATE
              AND CJFILE.CJ_DATE       <= :QCJ2-WEND-DATE 
              AND CJFILE.CJ_START_TIME >= :QCJ2-WBEG-START-TIME
              AND CJFILE.CJ_START_TIME <= :QCJ2-WEND-START-TIME
              ORDER BY CJFILE.CJ_BRNO,
                       CJFILE.CJ_ACCTNO,
                       CJFILE.CJ_DATE,
                       CJFILE.CJ_START_TIME,
                       CJFILE.CJ_COLLID,
                       CJFILE.CJ_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CJ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCJ2-WBEG-BRNO 
                          QCJ2-WEND-BRNO
                          QCJ2-WBEG-ACCTNO
                          QCJ2-WEND-ACCTNO
                          QCJ2-WBEG-START-TIME
                          QCJ2-WEND-START-TIME.
 
           MOVE SPACES TO QCJ2-WBEG-DATE
                          QCJ2-WEND-DATE.

           MOVE STAT---GOOD TO CJ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-CJ2-FILE-GREATER.
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.

           IF ( CJ2-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-CJ2-FILE.

           IF ( QCJ2-WEND-START-TIME = ZEROES    ) OR
              ( QCJ2-WEND-START-TIME NOT NUMERIC )
              MOVE CJ-BRNO              TO QCJ2-WBEG-BRNO
              MOVE CJ-BRNO              TO QCJ2-WEND-BRNO
              MOVE CJ-ACCTNO            TO QCJ2-WBEG-ACCTNO 
              MOVE ALL "9"              TO QCJ2-WEND-ACCTNO 
              MOVE CJ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WEND-DATE
              MOVE CJ-START-TIME        TO QCJ2-WBEG-START-TIME
              MOVE ALL "9"              TO QCJ2-WEND-START-TIME.

           EXEC SQL
            DECLARE CJ2_CURSOR_GR CURSOR FOR
            SELECT CJFILE.CJ_BRNO,
                   CJFILE.CJ_COLLID,
                   CAST(CJFILE.CJ_DATE AS VARCHAR(10)),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   CAST(CJFILE.CJ_LTOUCH_DATE AS VARCHAR(10)),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   CJFILE.CJ_ACTIVITY_CD,
                   CJFILE.CJ_RESULT_CD,
                   CJFILE.CJ_ACTION_CD,
                   CJFILE.CJ_NEW_COLLID,
                   CJFILE.CJ_CON_STAT,
                   CJFILE.CJ_REC_STAT,
                   CJFILE.CJ_FORM_NO,
                   CAST(CJFILE.CJ_PROMISE_DATE1 AS VARCHAR(10)),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   CAST(CJFILE.CJ_PROMISE_DATE2 AS VARCHAR(10)),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   CJFILE.CJ_QUEUE
            FROM DBO.CJFILE
      * NEED ACCTNO >= TO GET NEXT DATE WORKED FOR THIS ACCTNO, DATE STAYS >
            WHERE CJFILE.CJ_BRNO        = :QCJ2-WBEG-BRNO 
              AND CJFILE.CJ_ACCTNO     >= :QCJ2-WBEG-ACCTNO
              AND CJFILE.CJ_ACCTNO     <= :QCJ2-WEND-ACCTNO
              AND CJFILE.CJ_DATE        > :QCJ2-WBEG-DATE 
              AND CJFILE.CJ_DATE       <= :QCJ2-WEND-DATE 
              AND CJFILE.CJ_START_TIME  > :QCJ2-WBEG-START-TIME
              AND CJFILE.CJ_START_TIME <= :QCJ2-WEND-START-TIME
              ORDER BY CJFILE.CJ_BRNO,
                       CJFILE.CJ_ACCTNO,
                       CJFILE.CJ_DATE,
                       CJFILE.CJ_START_TIME,
                       CJFILE.CJ_COLLID,
                       CJFILE.CJ_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CJ2_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCJ2-WBEG-BRNO 
                          QCJ2-WEND-BRNO
                          QCJ2-WBEG-ACCTNO
                          QCJ2-WEND-ACCTNO
                          QCJ2-WBEG-START-TIME
                          QCJ2-WEND-START-TIME.
 
           MOVE SPACES TO QCJ2-WBEG-DATE
                          QCJ2-WEND-DATE.

           MOVE STAT---GOOD TO CJ2-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-CJ2-FILE.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO. 
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QCJ-REC.

           MOVE CJ-BRNO             TO QCJ-BRNO.
           MOVE CJ-ACCTNO TO QCJ-ACCTNO.
           MOVE CJ-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ-DATE.
           MOVE CJ-START-TIME       TO QCJ-START-TIME.

           EXEC SQL
            SELECT CJFILE.CJ_BRNO,
                   CJFILE.CJ_COLLID,
                   CAST(CJFILE.CJ_DATE AS VARCHAR(10)),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   CAST(CJFILE.CJ_LTOUCH_DATE AS VARCHAR(10)),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   CJFILE.CJ_ACTIVITY_CD,
                   CJFILE.CJ_RESULT_CD,
                   CJFILE.CJ_ACTION_CD,
                   CJFILE.CJ_NEW_COLLID,
                   CJFILE.CJ_CON_STAT,
                   CJFILE.CJ_REC_STAT,
                   CJFILE.CJ_FORM_NO,
                   CAST(CJFILE.CJ_PROMISE_DATE1 AS VARCHAR(10)),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   CAST(CJFILE.CJ_PROMISE_DATE2 AS VARCHAR(10)),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   CJFILE.CJ_QUEUE
            INTO :QCJ-REC
            FROM DBO.CJFILE
            WHERE CJFILE.CJ_BRNO       = :QCJ-BRNO
              AND CJFILE.CJ_ACCTNO     = :QCJ-ACCTNO
              AND CJFILE.CJ_DATE       = :QCJ-DATE
              AND CJFILE.CJ_START_TIME = :QCJ-START-TIME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       READ-CJ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QCJ-REC.

           IF ( CJ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ2_CURSOR INTO :QCJ-REC
              END-EXEC
           ELSE
           IF ( CJ2-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ2_CURSOR_GR INTO :QCJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CJ2-FILE.

           PERFORM CLOSE-IT.
           IF ( CJ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CJ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CJ2-CURSOR-STAT.

           IF ( CJ2-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE CJ2_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO CJ2-CURSOR-GR-STAT.

