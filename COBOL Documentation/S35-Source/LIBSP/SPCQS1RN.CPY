      * COPYMEMBER: LIBSP/SPCQS1RN
      * 190214  BREAK OUT THE HEADER RECORD IN EXISTING CQFILE, ITS NOW A
      * SUMMARY (CQS)
       LOAD-CQS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQS-ALL-DATA
              MOVE CQS-ALL-PATH             TO CQS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQS-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQS-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQS-ALL-DATA
              MOVE CQS-ALL-PATH             TO CQS-PATH.

      *-----------------------------------------------------------------
       OPEN-CQS1-FILE.
           PERFORM LOAD-CQS1-FILE.
           PERFORM OPEN-IT.
           MOVE CQS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CQS1-FILE.
           PERFORM START-IT.
           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           MOVE CQS-PATH-SQL    TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.

           IF ( CQS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQS1-FILE.

           IF ( QCQS1-WEND-COLLID = SPACES )
              MOVE CQS-PATH-OWNBR        TO QCQS1-WBEG-BRNO
              MOVE CQS-PATH-OWNBR        TO QCQS1-WEND-BRNO
              MOVE CQS-COLLID            TO QCQS1-WBEG-COLLID
              MOVE ALL "Z"               TO QCQS1-WEND-COLLID
              CALL "C$TOLOWER" USING QCQS1-WEND-COLLID, VALUE 3.

           EXEC SQL
            DECLARE CQS1_CURSOR CURSOR FOR
            SELECT CQSFILE.CQS_BRNO,
                   CQSFILE.CQS_COLLID,
                   CAST(CQSFILE.CQS_LTOUCH_DATE AS VARCHAR(10)),
                   CQSFILE.CQS_TOT_REC,
                   CQSFILE.CQS_WORKED_REC,
                   CQSFILE.CQS_TIMED_REC,
                   CQSFILE.CQS_CQ_BRNO,
                   CQSFILE.CQS_CQ_COLLID,
                   CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK,
                   CQSFILE.CQS_CQ_SEQ_FG,
                   CAST(CQSFILE.CQS_CQ_DATE AS VARCHAR(10)),
                   CQSFILE.CQS_CQ_START_TIME,
                   CQSFILE.CQS_CQ_REV_DEL,
                   CQSFILE.CQS_CQ_MISC_KEY_FLD,
                   CQSFILE.CQS_CQ_REV_PDUE_DAYS,
                   CQSFILE.CQS_CQ_REV_BAL,
                   CQSFILE.CQS_CQ_SEQ
            FROM DBO.CQSFILE
            WHERE CQSFILE.CQS_BRNO = :QCQS1-WBEG-BRNO
              AND CQSFILE.CQS_COLLID >= :QCQS1-WBEG-COLLID
              AND CQSFILE.CQS_COLLID <= :QCQS1-WEND-COLLID
            ORDER BY CQSFILE.CQS_BRNO,
                     CQSFILE.CQS_COLLID
           END-EXEC.

           EXEC SQL
               OPEN CQS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQS1-WBEG-BRNO 
                          QCQS1-WEND-BRNO.
 
           MOVE SPACES TO QCQS1-WBEG-COLLID
                          QCQS1-WEND-COLLID.

           MOVE STAT---GOOD TO CQS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CQS1-FILE.
           PERFORM READ-IT.
           MOVE CQS-PATH-OWNBR  TO CQS-BRNO. 
           MOVE CQS-PATH-SQL    TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           INITIALIZE QCQS-REC.

           MOVE CQS-BRNO   TO QCQS-BRNO.
           MOVE CQS-COLLID TO QCQS-COLLID.

           EXEC SQL
            SELECT CQSFILE.CQS_BRNO,
                   CQSFILE.CQS_COLLID,
                   CAST(CQSFILE.CQS_LTOUCH_DATE AS VARCHAR(10)),
                   CQSFILE.CQS_TOT_REC,
                   CQSFILE.CQS_WORKED_REC,
                   CQSFILE.CQS_TIMED_REC,
                   CQSFILE.CQS_CQ_BRNO,
                   CQSFILE.CQS_CQ_COLLID,
                   CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK,
                   CQSFILE.CQS_CQ_SEQ_FG,
                   CAST(CQSFILE.CQS_CQ_DATE AS VARCHAR(10)),
                   CQSFILE.CQS_CQ_START_TIME,
                   CQSFILE.CQS_CQ_REV_DEL,
                   CQSFILE.CQS_CQ_MISC_KEY_FLD,
                   CQSFILE.CQS_CQ_REV_PDUE_DAYS,
                   CQSFILE.CQS_CQ_REV_BAL,
                   CQSFILE.CQS_CQ_SEQ
            INTO :QCQS-REC
            FROM DBO.CQSFILE
            WHERE CQSFILE.CQS_BRNO = :QCQS-BRNO
              AND CQSFILE.CQS_COLLID = :QCQS-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CQS-FIELDS.

      *-----------------------------------------------------------------
       READ-CQS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           MOVE CQS-PATH-SQL    TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           INITIALIZE QCQS-REC.

           IF ( CQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CQS1_CURSOR INTO :QCQS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CQS1-FILE.

           PERFORM CLOSE-IT.
           IF ( CQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQS1-CURSOR-STAT.

