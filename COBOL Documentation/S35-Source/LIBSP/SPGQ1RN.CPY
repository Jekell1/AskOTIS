      * COPYMEMBER: LIBSP/SPGQ1RN
       LOAD-GQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH.

      *-----------------------------------------------------------------
       OPEN-GQ1-FILE.
           PERFORM LOAD-GQ1-FILE.
           PERFORM OPEN-IT.
           MOVE GQ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GQ1-FILE.
           PERFORM START-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.

           IF ( GQ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQ1-FILE.

           IF ( QGQ1-WEND-ACCTNO = ZEROES    ) OR
              ( QGQ1-WEND-ACCTNO NOT NUMERIC )
              MOVE GQ-BRNO              TO QGQ1-WBEG-BRNO
              MOVE ALL "9"              TO QGQ1-WEND-BRNO
              MOVE GQ-ACCTNO            TO QGQ1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGQ1-WEND-ACCTNO.

           EXEC SQL
            DECLARE GQ1_CURSOR CURSOR FOR
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   GQFILE.GQ_COLLID,
                   GQFILE.GQ_SEQ_FG,
                   CAST(GQFILE.GQ_DATE AS VARCHAR(10)),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   GQFILE.GQ_MISC_KEY_FLD,
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   CAST(GQFILE.GQ_LTOUCH_DATE AS VARCHAR(10)),
                   GQFILE.GQ_WORK_COLLID,
                   GQFILE.GQ_WORKED_FG,
                   GQFILE.GQ_IN_PROCESS,
                   CAST(GQFILE.GQ_DATE_QUEUED AS VARCHAR(10)),
                   GQFILE.GQ_TIME_ZONE_ERROR
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_BRNO >= :QGQ1-WBEG-BRNO
              AND GQFILE.GQ_BRNO <= :QGQ1-WEND-BRNO
              AND GQFILE.GQ_ACCTNO >= :QGQ1-WBEG-ACCTNO
              AND GQFILE.GQ_ACCTNO <= :QGQ1-WEND-ACCTNO
            ORDER BY GQFILE.GQ_BRNO,
                     GQFILE.GQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN GQ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGQ1-WBEG-BRNO
                          QGQ1-WEND-BRNO
                          QGQ1-WBEG-ACCTNO
                          QGQ1-WEND-ACCTNO.

           MOVE STAT---GOOD TO GQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GQ1-FILE.
           PERFORM READ-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.
           INITIALIZE QGQ-REC.

           MOVE GQ-BRNO       TO QGQ-BRNO.  
           MOVE GQ-ACCTNO     TO QGQ-ACCTNO. 

           EXEC SQL
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   GQFILE.GQ_COLLID,
                   GQFILE.GQ_SEQ_FG,
                   CAST(GQFILE.GQ_DATE AS VARCHAR(10)),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   GQFILE.GQ_MISC_KEY_FLD,
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   CAST(GQFILE.GQ_LTOUCH_DATE AS VARCHAR(10)),
                   GQFILE.GQ_WORK_COLLID,
                   GQFILE.GQ_WORKED_FG,
                   GQFILE.GQ_IN_PROCESS,
                   CAST(GQFILE.GQ_DATE_QUEUED AS VARCHAR(10)),
                   GQFILE.GQ_TIME_ZONE_ERROR
            INTO :QGQ-REC
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_BRNO = :QGQ-BRNO
              AND GQFILE.GQ_ACCTNO = :QGQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GQ-FIELDS.

      *-----------------------------------------------------------------
       READ-GQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.
           INITIALIZE QGQ-REC.

           IF ( GQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GQ1_CURSOR INTO :QGQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GQ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GQ1-FILE.
           PERFORM CLOSE-IT.
           IF ( GQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQ1-CURSOR-STAT.

