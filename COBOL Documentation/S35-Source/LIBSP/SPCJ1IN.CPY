      * COPYMEMBER: LIBSP/SPCJ1IN
       LOAD-CJ1-FILE.
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
       OPEN-CJ1-FILE.
           PERFORM LOAD-CJ1-FILE.
           PERFORM OPEN-IT.
           MOVE CJ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CJ1-FILE.
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.

           IF ( CJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ1-FILE.

           IF ( QCJ1-WEND-SEQNO = ZEROES    ) OR
              ( QCJ1-WEND-SEQNO NOT NUMERIC )
              MOVE CJ-BRNO              TO QCJ1-WBEG-BRNO
              MOVE CJ-BRNO              TO QCJ1-WEND-BRNO
              MOVE CJ-COLLID            TO QCJ1-WBEG-COLLID
              MOVE ALL "Z"              TO QCJ1-WEND-COLLID
              MOVE CJ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WEND-DATE
              MOVE CJ-START-TIME        TO QCJ1-WBEG-START-TIME
              MOVE ALL "9"              TO QCJ1-WEND-START-TIME
              MOVE CJ-SEQNO             TO QCJ1-WBEG-SEQNO
              MOVE ALL "9"              TO QCJ1-WEND-SEQNO
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CJ-COLLID            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCJ1-WSBEG-COLLID

              MOVE CJ-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WSBEG-DATE

              COMPUTE QCJ1-WSBEG-START-TIME = CJ-START-TIME + 1
              IF ( CJ-SEQNO = 99 )
                 MOVE CJ-SEQNO          TO QCJ1-WSBEG-SEQNO
              ELSE
                 COMPUTE QCJ1-WSBEG-SEQNO = CJ-SEQNO + 1.


           EXEC SQL
            DECLARE CJ1_CURSOR CURSOR FOR
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
      *     WHERE (( CJFILE.CJ_BRNO       >= :QCJ1-WBEG-BRNO
      *       AND    CJFILE.CJ_COLLID     >= :QCJ1-WBEG-COLLID 
      *       AND    CAST(CJFILE.CJ_DATE AS VARCHAR(10))
      *                                   >= :QCJ1-WBEG-DATE
      *       AND    CJFILE.CJ_START_TIME >= :QCJ1-WBEG-START-TIME
      *       AND    CJFILE.CJ_SEQNO      >= :QCJ1-WBEG-SEQNO )
      *        OR  ( CJFILE.CJ_BRNO       >= :QCJ1-WBEG-BRNO
      *       AND    CJFILE.CJ_COLLID     >= :QCJ1-WBEG-COLLID 
      *       AND    CAST(CJFILE.CJ_DATE AS VARCHAR(10))
      *                                   >= :QCJ1-WBEG-DATE
      *       AND    CJFILE.CJ_START_TIME >= :QCJ1-WSBEG-START-TIME
      *       AND    CJFILE.CJ_SEQNO      >= 0 )
      *        OR  ( CJFILE.CJ_BRNO       >= :QCJ1-WBEG-BRNO
      *       AND    CJFILE.CJ_COLLID     >= :QCJ1-WBEG-COLLID 
      *       AND    CAST(CJFILE.CJ_DATE AS VARCHAR(10))
      *                                   >= :QCJ1-WSBEG-DATE
      *       AND    CJFILE.CJ_START_TIME >= 0
      *       AND    CJFILE.CJ_SEQNO      >= 0 )
      *        OR  ( CJFILE.CJ_BRNO       >= :QCJ1-WBEG-BRNO
      *       AND    CJFILE.CJ_COLLID     >= :QCJ1-WSBEG-COLLID 
      *       AND    CAST(CJFILE.CJ_DATE AS VARCHAR(10))
      *                                   >= '1900-01-01'
      *       AND    CJFILE.CJ_START_TIME >= 0
      *       AND    CJFILE.CJ_SEQNO      >= 0 ))
      *       AND (  CJFILE.CJ_BRNO       <= :QCJ1-WEND-BRNO
      *       AND    CJFILE.CJ_COLLID     <= :QCJ1-WEND-COLLID 
      *       AND    CAST(CJFILE.CJ_DATE AS VARCHAR(10))
      *                                   <= :QCJ1-WEND-DATE
      *       AND    CJFILE.CJ_START_TIME <= :QCJ1-WEND-START-TIME
      *       AND    CJFILE.CJ_SEQNO      <= :QCJ1-WEND-SEQNO )
            WHERE (  CJFILE.CJ_BRNO        = :QCJ1-WBEG-BRNO  )
              AND (( CJFILE.CJ_COLLID     >= :QCJ1-WBEG-COLLID 
              AND    CJFILE.CJ_DATE       >= :QCJ1-WBEG-DATE 
              AND    CJFILE.CJ_START_TIME >= :QCJ1-WBEG-START-TIME
              AND    CJFILE.CJ_SEQNO      >= :QCJ1-WBEG-SEQNO )
               OR  ( CJFILE.CJ_COLLID     >= :QCJ1-WBEG-COLLID 
              AND    CJFILE.CJ_DATE       >= :QCJ1-WBEG-DATE
              AND    CJFILE.CJ_START_TIME >= :QCJ1-WSBEG-START-TIME
              AND    CJFILE.CJ_SEQNO      >= 0 )
               OR  ( CJFILE.CJ_COLLID     >= :QCJ1-WBEG-COLLID 
              AND    CJFILE.CJ_DATE       >= :QCJ1-WSBEG-DATE 
              AND    CJFILE.CJ_START_TIME >= 0
              AND    CJFILE.CJ_SEQNO      >= 0 )
               OR  ( CJFILE.CJ_COLLID     >= :QCJ1-WSBEG-COLLID 
              AND    CJFILE.CJ_DATE       >= '1900-01-01' 
              AND    CJFILE.CJ_START_TIME >= 0
              AND    CJFILE.CJ_SEQNO      >= 0 ))
              AND (  CJFILE.CJ_COLLID     <= :QCJ1-WEND-COLLID 
              AND    CJFILE.CJ_DATE       <= :QCJ1-WEND-DATE
              AND    CJFILE.CJ_START_TIME <= :QCJ1-WEND-START-TIME
              AND    CJFILE.CJ_SEQNO      <= :QCJ1-WEND-SEQNO )
            ORDER BY CJFILE.CJ_BRNO,
                     CJFILE.CJ_COLLID,
                     CJFILE.CJ_DATE,
                     CJFILE.CJ_START_TIME,
                     CJFILE.CJ_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCJ1-WBEG-BRNO 
                          QCJ1-WEND-BRNO
                          QCJ1-WBEG-START-TIME
                          QCJ1-WEND-START-TIME
                          QCJ1-WBEG-SEQNO
                          QCJ1-WEND-SEQNO
                          QCJ1-WSBEG-START-TIME
                          QCJ1-WSBEG-SEQNO.
 
           MOVE SPACES TO QCJ1-WBEG-COLLID
                          QCJ1-WEND-COLLID
                          QCJ1-WBEG-DATE
                          QCJ1-WEND-DATE
                          QCJ1-WSBEG-COLLID
                          QCJ1-WSBEG-DATE.

           MOVE STAT---GOOD TO CJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CJ1-FILE.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO. 
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           INITIALIZE QCJ-REC.

           MOVE CJ-BRNO             TO QCJ-BRNO.
           MOVE CJ-COLLID           TO QCJ-COLLID.
           MOVE CJ-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ-DATE.
           MOVE CJ-START-TIME       TO QCJ-START-TIME.
           MOVE CJ-SEQNO            TO QCJ-SEQNO.

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
              AND CJFILE.CJ_COLLID     = :QCJ-COLLID
              AND CJFILE.CJ_DATE       = :QCJ-DATE
              AND CJFILE.CJ_START_TIME = :QCJ-START-TIME
              AND CJFILE.CJ_SEQNO      = :QCJ-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       READ-CJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           INITIALIZE QCJ-REC.

           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ1_CURSOR INTO :QCJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       WRITE-CJ1-FILE.

           MOVE CJ-PATH-OWNBR  TO CJ-BRNO. 
           ACCEPT CJ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           PERFORM SET-CJ-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CJFILE
             (CJFILE.CJ_BRNO,
              CJFILE.CJ_COLLID,
              CJFILE.CJ_DATE,
              CJFILE.CJ_START_TIME,
              CJFILE.CJ_SEQNO,
              CJFILE.CJ_LTOUCH_DATE,
              CJFILE.CJ_ACCTNO,
              CJFILE.CJ_END_TIME,
              CJFILE.CJ_ACTIVITY_CD,
              CJFILE.CJ_RESULT_CD,
              CJFILE.CJ_ACTION_CD,
              CJFILE.CJ_NEW_COLLID,
              CJFILE.CJ_CON_STAT,
              CJFILE.CJ_REC_STAT,
              CJFILE.CJ_FORM_NO,
              CJFILE.CJ_PROMISE_DATE1,
              CJFILE.CJ_PROMISE_AMT,
              CJFILE.CJ_BEG_LM_SEQ,
              CJFILE.CJ_END_LM_SEQ,
              CJFILE.CJ_PROMISE_DATE2,
              CJFILE.CJ_CUR_BAL,
              CJFILE.CJ_PAST_DUE,
              CJFILE.CJ_POSTING_BR,
              CJFILE.CJ_QUEUE)
            VALUES
             (:QCJ-BRNO,
              :QCJ-COLLID,
              :QCJ-DATE,
              :QCJ-START-TIME,
              :QCJ-SEQNO,
              :QCJ-LTOUCH-DATE,
              :QCJ-ACCTNO,
              :QCJ-END-TIME,
              :QCJ-ACTIVITY-CD,
              :QCJ-RESULT-CD,
              :QCJ-ACTION-CD,
              :QCJ-NEW-COLLID,
              :QCJ-CON-STAT,
              :QCJ-REC-STAT,
              :QCJ-FORM-NO,
              :QCJ-PROMISE-DATE1,
              :QCJ-PROMISE-AMT,
              :QCJ-BEG-LM-SEQ,
              :QCJ-END-LM-SEQ,
              :QCJ-PROMISE-DATE2,
              :QCJ-CUR-BAL,
              :QCJ-PAST-DUE,
              :QCJ-POSTING-BR,
              :QCJ-QUEUE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CJ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CJ1-FILE.

           MOVE CJ-PATH-OWNBR TO CJ-BRNO. 
           ACCEPT CJ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           PERFORM SET-CJ-FIELDS.

           EXEC SQL
            UPDATE DBO.CJFILE
             SET
              CJFILE.CJ_BRNO = :QCJ-BRNO,
              CJFILE.CJ_COLLID = :QCJ-COLLID,
              CJFILE.CJ_DATE = :QCJ-DATE,
              CJFILE.CJ_START_TIME = :QCJ-START-TIME,
              CJFILE.CJ_SEQNO = :QCJ-SEQNO,
              CJFILE.CJ_LTOUCH_DATE = :QCJ-LTOUCH-DATE,
              CJFILE.CJ_ACCTNO = :QCJ-ACCTNO,
              CJFILE.CJ_END_TIME = :QCJ-END-TIME,
              CJFILE.CJ_ACTIVITY_CD = :QCJ-ACTIVITY-CD,
              CJFILE.CJ_RESULT_CD = :QCJ-RESULT-CD,
              CJFILE.CJ_ACTION_CD = :QCJ-ACTION-CD,
              CJFILE.CJ_NEW_COLLID = :QCJ-NEW-COLLID,
              CJFILE.CJ_CON_STAT = :QCJ-CON-STAT,
              CJFILE.CJ_REC_STAT = :QCJ-REC-STAT,
              CJFILE.CJ_FORM_NO = :QCJ-FORM-NO,
              CJFILE.CJ_PROMISE_DATE1 = :QCJ-PROMISE-DATE1,
              CJFILE.CJ_PROMISE_AMT = :QCJ-PROMISE-AMT,
              CJFILE.CJ_BEG_LM_SEQ = :QCJ-BEG-LM-SEQ,
              CJFILE.CJ_END_LM_SEQ = :QCJ-END-LM-SEQ,
              CJFILE.CJ_PROMISE_DATE2 = :QCJ-PROMISE-DATE2,
              CJFILE.CJ_CUR_BAL = :QCJ-CUR-BAL,
              CJFILE.CJ_PAST_DUE = :QCJ-PAST-DUE,
              CJFILE.CJ_POSTING_BR = :QCJ-POSTING-BR,
              CJFILE.CJ_QUEUE = :QCJ-QUEUE
             WHERE CJFILE.CJ_BRNO       = :QCJ-BRNO
               AND CJFILE.CJ_COLLID     = :QCJ-COLLID
               AND CJFILE.CJ_DATE       = :QCJ-DATE
               AND CJFILE.CJ_START_TIME = :QCJ-START-TIME
               AND CJFILE.CJ_SEQNO      = :QCJ-SEQNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CJ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CJ1-FILE.

           MOVE CJ-PATH-OWNBR TO CJ-BRNO. 
           PERFORM DELETE-IT.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           PERFORM SET-CJ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CJFILE
             WHERE CJFILE.CJ_BRNO       = :QCJ-BRNO
               AND CJFILE.CJ_COLLID     = :QCJ-COLLID
               AND CJFILE.CJ_DATE       = :QCJ-DATE
               AND CJFILE.CJ_START_TIME = :QCJ-START-TIME
               AND CJFILE.CJ_SEQNO      = :QCJ-SEQNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CJ1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CJ1-FILE.

           PERFORM CLOSE-IT.
           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CJ1-CURSOR-STAT.

