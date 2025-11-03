      * COPYMEMBER: LIBSP/SPCQ1RN
       LOAD-CQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH.

      *-----------------------------------------------------------------
       OPEN-CQ1-FILE.
           PERFORM LOAD-CQ1-FILE.
           PERFORM OPEN-IT.
           MOVE CQ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CQ1-FILE.
           PERFORM START-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-PATH-SQL    TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.

           IF ( CQ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQ1-FILE.

           IF ( QCQ1-WEND-ACCTNO = ZEROES    ) OR
              ( QCQ1-WEND-ACCTNO NOT NUMERIC )
              MOVE CQ-PATH-OWNBR        TO QCQ1-WBEG-BRNO
              MOVE CQ-PATH-OWNBR        TO QCQ1-WEND-BRNO
              MOVE CQ-ACCTNO            TO QCQ1-WBEG-ACCTNO
              MOVE ALL "9"              TO QCQ1-WEND-ACCTNO.

           EXEC SQL
            DECLARE CQ1_CURSOR CURSOR FOR
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   CQFILE.CQ_COLLID,
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   CQFILE.CQ_SEQ_FG,
                   CAST(CQFILE.CQ_DATE AS VARCHAR(10)),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   CQFILE.CQ_MISC_KEY_FLD,
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   CAST(CQFILE.CQ_LTOUCH_DATE AS VARCHAR(10)),
                   CQFILE.CQ_WORK_COLLID,
                   CQFILE.CQ_WORKED_FG,
                   CQFILE.CQ_IN_PROCESS,
                   CAST(CQFILE.CQ_DATE_QUEUED AS VARCHAR(10)),
                   CQFILE.CQ_TIME_ZONE_ERROR
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO = :QCQ1-WBEG-BRNO
              AND CQFILE.CQ_ACCTNO >= :QCQ1-WBEG-ACCTNO
              AND CQFILE.CQ_ACCTNO <= :QCQ1-WEND-ACCTNO
            ORDER BY CQFILE.CQ_BRNO,
                     CQFILE.CQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CQ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQ1-WBEG-BRNO 
                          QCQ1-WEND-BRNO
                          QCQ1-WBEG-ACCTNO
                          QCQ1-WEND-ACCTNO.
 
           MOVE STAT---GOOD TO CQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CQ1-FILE.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO. 
           MOVE CQ-PATH-SQL    TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.
           INITIALIZE QCQ-REC.

           MOVE CQ-BRNO   TO QCQ-BRNO.
           MOVE CQ-ACCTNO TO QCQ-ACCTNO.

           EXEC SQL
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   CQFILE.CQ_COLLID,
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   CQFILE.CQ_SEQ_FG,
                   CAST(CQFILE.CQ_DATE AS VARCHAR(10)),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   CQFILE.CQ_MISC_KEY_FLD,
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   CAST(CQFILE.CQ_LTOUCH_DATE AS VARCHAR(10)),
                   CQFILE.CQ_WORK_COLLID,
                   CQFILE.CQ_WORKED_FG,
                   CQFILE.CQ_IN_PROCESS,
                   CAST(CQFILE.CQ_DATE_QUEUED AS VARCHAR(10)),
                   CQFILE.CQ_TIME_ZONE_ERROR
            INTO :QCQ-REC
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO = :QCQ-BRNO
              AND CQFILE.CQ_ACCTNO = :QCQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       READ-CQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-PATH-SQL    TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.
           INITIALIZE QCQ-REC.

           IF ( CQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CQ1_CURSOR INTO :QCQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CQ1-FILE.

           PERFORM CLOSE-IT.
           IF ( CQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQ1-CURSOR-STAT.

