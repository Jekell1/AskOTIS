      * COPYMEMBER: LIBSP/SPAQ1IN
      ************************************************
      *                (SPAQ1IN)
      * REV:
      * 012319 AMM AQFILE
      * 2021.0927 ADDED BRNO TO WRITE/REWRITE/DELETE
      ************************************************
      *-----------------------------------------------------------------
       LOAD-AQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( AQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( AQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO AQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO AQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO AQ-ALL-DATA
              MOVE AQ-ALL-PATH             TO AQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( AQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO AQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO AQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO AQ-ALL-DATA
              MOVE AQ-ALL-PATH             TO AQ-PATH.

      *-----------------------------------------------------------------
       OPEN-AQ1-FILE.
           PERFORM LOAD-AQ1-FILE.
           PERFORM OPEN-IT.
           MOVE AQ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-AQ1-FILE.
           PERFORM START-IT.
           MOVE AQ-PATH-OWNBR  TO AQ-BRNO.
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE AQ1-KEY        TO E-KEYX.

           IF ( AQ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-AQ1-FILE.

           IF ( QAQ1-WEND-BRNO = ZEROES    ) OR
              ( QAQ1-WEND-BRNO NOT NUMERIC )
              MOVE AQ-BRNO              TO QAQ1-WBEG-BRNO
              MOVE AQ-BRNO              TO QAQ1-WEND-BRNO
              MOVE AQ-ACCTNO            TO QAQ1-WBEG-ACCTNO
              MOVE ALL "9"              TO QAQ1-WEND-ACCTNO.

           EXEC SQL
            DECLARE AQ1_CURSOR CURSOR FOR
            SELECT AQFILE.AQ_BRNO,
                   AQFILE.AQ_ACCTNO,
                   CAST(AQFILE.AQ_LOANDATE AS VARCHAR(10)),
                   AQFILE.AQ_SEQNO,
                   AQFILE.AQ_WORKED_FG,
                   CAST(AQFILE.AQ_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.AQFILE
            WHERE AQFILE.AQ_BRNO    = :QAQ1-WBEG-BRNO
              AND AQFILE.AQ_ACCTNO >= :QAQ1-WBEG-ACCTNO
              AND AQFILE.AQ_ACCTNO <= :QAQ1-WEND-ACCTNO
            ORDER BY AQFILE.AQ_BRNO,
                     AQFILE.AQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN AQ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QAQ1-WBEG-BRNO 
                          QAQ1-WEND-BRNO
                          QAQ1-WBEG-ACCTNO
                          QAQ1-WEND-ACCTNO.

           MOVE STAT---GOOD TO AQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-AQ1-FILE.
           PERFORM READ-IT.
           MOVE AQ-PATH-OWNBR  TO AQ-BRNO. 
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE AQ1-KEY        TO E-KEYX.
           INITIALIZE QAQ-REC.

           MOVE AQ-BRNO   TO QAQ-BRNO.
           MOVE AQ-ACCTNO TO QAQ-ACCTNO.

           EXEC SQL
            SELECT AQFILE.AQ_BRNO,
                   AQFILE.AQ_ACCTNO,
                   CAST(AQFILE.AQ_LOANDATE AS VARCHAR(10)),
                   AQFILE.AQ_SEQNO,
                   AQFILE.AQ_WORKED_FG,
                   CAST(AQFILE.AQ_LTOUCH_DATE AS VARCHAR(10))
            INTO :QAQ-REC
            FROM DBO.AQFILE
            WHERE AQFILE.AQ_BRNO   = :QAQ-BRNO
              AND AQFILE.AQ_ACCTNO = :QAQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AQ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-AQ-FIELDS.

      *-----------------------------------------------------------------
       READ-AQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE AQ-PATH-OWNBR  TO AQ-BRNO.
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE AQ1-KEY        TO E-KEYX.
           INITIALIZE QAQ-REC.

           IF ( AQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH AQ1_CURSOR INTO :QAQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AQ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-AQ-FIELDS.

      *-----------------------------------------------------------------
       WRITE-AQ1-FILE.

           MOVE AQ-PATH-OWNBR TO AQ-BRNO. 
           ACCEPT AQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE AQ1-KEY        TO E-KEYX.
           PERFORM SET-AQ-FIELDS.

           EXEC SQL
            INSERT INTO DBO.AQFILE
             (AQFILE.AQ_BRNO,
              AQFILE.AQ_ACCTNO,
              AQFILE.AQ_LOANDATE,
              AQFILE.AQ_SEQNO,
              AQFILE.AQ_WORKED_FG,
              AQFILE.AQ_LTOUCH_DATE)
            VALUES
             (:QAQ-BRNO,
              :QAQ-ACCTNO,
              :QAQ-LOANDATE,
              :QAQ-SEQNO,
              :QAQ-WORKED-FG,
              :QAQ-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-AQ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-AQ1-FILE.

           MOVE AQ-PATH-OWNBR TO AQ-BRNO. 
           ACCEPT AQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE AQ1-KEY        TO E-KEYX.
           PERFORM SET-AQ-FIELDS.

           EXEC SQL
            UPDATE DBO.AQFILE
             SET
              AQFILE.AQ_BRNO = :QAQ-BRNO,
              AQFILE.AQ_ACCTNO = :QAQ-ACCTNO,
              AQFILE.AQ_LOANDATE = :QAQ-LOANDATE,
              AQFILE.AQ_SEQNO = :QAQ-SEQNO,
              AQFILE.AQ_WORKED_FG = :QAQ-WORKED-FG,
              AQFILE.AQ_LTOUCH_DATE = :QAQ-LTOUCH-DATE
            WHERE AQFILE.AQ_BRNO   = :QAQ-BRNO
              AND AQFILE.AQ_ACCTNO = :QAQ-ACCTNO
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-AQ1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-AQ1-FILE.

           MOVE AQ-PATH-OWNBR TO AQ-BRNO. 
           PERFORM DELETE-IT.
           MOVE AQ-PATH-SQL    TO E-FILE.
           MOVE AQ1-KEY        TO E-KEYX.
           PERFORM SET-AQ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.AQFILE
            WHERE AQFILE.AQ_BRNO   = :QAQ-BRNO
              AND AQFILE.AQ_ACCTNO = :QAQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-AQ1-FILE.

      *-----------------------------------------------------------------
       CLOSE-AQ1-FILE.

           PERFORM CLOSE-IT.

           IF ( AQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE AQ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO AQ1-CURSOR-STAT.

