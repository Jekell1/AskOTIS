      * COPYMEMBER: LIBSP/SPGQS1RN
      * 190214  BREAK OUT THE HEADER RECORD IN EXISTING CQFILE, ITS NOW A
      * SUMMARY (GQS)
       LOAD-GQS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GQS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GQS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GQS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GQS-ALL-MACHINE
              MOVE GQS-ALL-PATH            TO GQS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GQS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GQS-ALL-BASE
              MOVE FILPATH-MACHINE         TO GQS-ALL-MACHINE
              MOVE GQS-ALL-PATH            TO GQS-PATH.

      *-----------------------------------------------------------------
       OPEN-GQS1-FILE.
           PERFORM LOAD-GQS1-FILE.
           PERFORM OPEN-IT.
           MOVE GQS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GQS1-FILE.
           PERFORM START-IT.
           MOVE GQS-PATH-SQL TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.

           IF ( GQS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQS1-FILE.

           IF ( QGQS1-WEND-COLLID = SPACES )
              MOVE GQS-COLLID           TO QGQS1-WBEG-COLLID
              MOVE ALL "X"              TO QGQS1-WEND-COLLID.

           EXEC SQL
            DECLARE GQS1_CURSOR CURSOR FOR
            SELECT GQSFILE.GQS_COLLID,
                   CAST(GQSFILE.GQS_LTOUCH_DATE AS VARCHAR(10)),
                   GQSFILE.GQS_TOT_REC,
                   GQSFILE.GQS_WORKED_REC,
                   GQSFILE.GQS_TIMED_REC,
                   GQSFILE.GQS_GROUP_ID,
                   GQSFILE.GQS_GQ_COLLID,
                   GQSFILE.GQS_GQ_SEQ_FG,
                   CAST(GQSFILE.GQS_GQ_DATE AS VARCHAR(10)),
                   GQSFILE.GQS_GQ_START_TIME,
                   GQSFILE.GQS_GQ_REV_DEL,
                   GQSFILE.GQS_GQ_MISC_KEY_FLD,
                   GQSFILE.GQS_GQ_REV_PDUE_DAYS,
                   GQSFILE.GQS_GQ_REV_BAL,
                   GQSFILE.GQS_GQ_SEQ
            FROM DBO.GQSFILE
            WHERE GQSFILE.GQS_COLLID >= :QGQS1-WBEG-COLLID
              AND GQSFILE.GQS_COLLID <= :QGQS1-WEND-COLLID
            ORDER BY GQSFILE.GQS_COLLID
           END-EXEC.

           EXEC SQL
               OPEN GQS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGQS1-WBEG-COLLID
                          QGQS1-WEND-COLLID.

           MOVE STAT---GOOD TO GQS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GQS1-FILE.
           PERFORM READ-IT.
           MOVE GQS-PATH-SQL TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.
           INITIALIZE QGQS-REC.

           MOVE GQS-GQ-COLLID     TO QGQS-GQ-COLLID. 

           EXEC SQL
            SELECT GQSFILE.GQS_COLLID,
                   CAST(GQSFILE.GQS_LTOUCH_DATE AS VARCHAR(10)),
                   GQSFILE.GQS_TOT_REC,
                   GQSFILE.GQS_WORKED_REC,
                   GQSFILE.GQS_TIMED_REC,
                   GQSFILE.GQS_GROUP_ID,
                   GQSFILE.GQS_GQ_COLLID,
                   GQSFILE.GQS_GQ_SEQ_FG,
                   CAST(GQSFILE.GQS_GQ_DATE AS VARCHAR(10)),
                   GQSFILE.GQS_GQ_START_TIME,
                   GQSFILE.GQS_GQ_REV_DEL,
                   GQSFILE.GQS_GQ_MISC_KEY_FLD,
                   GQSFILE.GQS_GQ_REV_PDUE_DAYS,
                   GQSFILE.GQS_GQ_REV_BAL,
                   GQSFILE.GQS_GQ_SEQ
            INTO :QGQS-REC
            FROM DBO.GQSFILE
            WHERE GQSFILE.GQS_GQ_COLLID = :QGQS-GQ-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GQS-FIELDS.

      *-----------------------------------------------------------------
       READ-GQS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GQS-PATH-SQL TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.
           INITIALIZE QGQS-REC.

           IF ( GQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GQS1_CURSOR INTO :QGQS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GQS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GQS1-FILE.
           PERFORM CLOSE-IT.
           IF ( GQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQS1-CURSOR-STAT.

