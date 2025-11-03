      * COPYMEMBER: LIBSP/SPGQS1IN
      *=================================================================
      *  NOTE!!! MUST SET GQS-GQ-BRNO IN THE WRITE & REWRITE BUT NOT
      *          IN THE DELETE, READ, READ-NEXT AND START.
      *          JUST NEED TO MAKE SURE THAT THE GQS-GQ2-KEY-FIELDS
      *          ARE MATCHING THE SIZE & POSITION FOR GQ2-KEY FIELDS.
      *          CS & KC 2019-0307
      *  20190327 GQS-GQ-BRNO IS NO LONGER NEEDED, THIS IS A COPY OF 
      *           GQ2-KEY, BUT GQ-BRNO IS NO LONGER PART OF GQ2-KEY
      *=================================================================
      * 190214  BREAK OUT THE HEADER RECORD IN EXISTING GQFILE, ITS NOW A 
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
       WRITE-GQS1-FILE.

           ACCEPT GQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GQS-PATH-SQL TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.
           PERFORM SET-GQS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GQSFILE
             (GQSFILE.GQS_COLLID,
              GQSFILE.GQS_LTOUCH_DATE,
              GQSFILE.GQS_TOT_REC,
              GQSFILE.GQS_WORKED_REC,
              GQSFILE.GQS_TIMED_REC,
              GQSFILE.GQS_GROUP_ID,
              GQSFILE.GQS_GQ_COLLID,
              GQSFILE.GQS_GQ_SEQ_FG,
              GQSFILE.GQS_GQ_DATE,
              GQSFILE.GQS_GQ_START_TIME,
              GQSFILE.GQS_GQ_REV_DEL,
              GQSFILE.GQS_GQ_MISC_KEY_FLD,
              GQSFILE.GQS_GQ_REV_PDUE_DAYS,
              GQSFILE.GQS_GQ_REV_BAL,
              GQSFILE.GQS_GQ_SEQ)
            VALUES
             (:QGQS-COLLID,
              :QGQS-LTOUCH-DATE,
              :QGQS-TOT-REC,
              :QGQS-WORKED-REC,
              :QGQS-TIMED-REC,
              :QGQS-GROUP-ID,
              :QGQS-GQ-COLLID,
              :QGQS-GQ-SEQ-FG,
              :QGQS-GQ-DATE,
              :QGQS-GQ-START-TIME,
              :QGQS-GQ-REV-DEL,
              :QGQS-GQ-MISC-KEY-FLD,
              :QGQS-GQ-REV-PDUE-DAYS,
              :QGQS-GQ-REV-BAL,
              :QGQS-GQ-SEQ)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GQS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GQS1-FILE.

           ACCEPT GQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GQS-PATH-SQL TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.
           PERFORM SET-GQS-FIELDS.

           EXEC SQL
            UPDATE DBO.GQSFILE
             SET
              GQSFILE.GQS_COLLID = :QGQS-COLLID,
              GQSFILE.GQS_LTOUCH_DATE =
                 :QGQS-LTOUCH-DATE,
              GQSFILE.GQS_TOT_REC = :QGQS-TOT-REC,
              GQSFILE.GQS_WORKED_REC = :QGQS-WORKED-REC,
              GQSFILE.GQS_TIMED_REC = :QGQS-TIMED-REC,
              GQSFILE.GQS_GROUP_ID = :QGQS-GROUP-ID,
              GQSFILE.GQS_GQ_COLLID = :QGQS-GQ-COLLID,
              GQSFILE.GQS_GQ_SEQ_FG = :QGQS-GQ-SEQ-FG,
              GQSFILE.GQS_GQ_DATE = :QGQS-GQ-DATE,
              GQSFILE.GQS_GQ_START_TIME = :QGQS-GQ-START-TIME,
              GQSFILE.GQS_GQ_REV_DEL = :QGQS-GQ-REV-DEL,
              GQSFILE.GQS_GQ_MISC_KEY_FLD = :QGQS-GQ-MISC-KEY-FLD,
              GQSFILE.GQS_GQ_REV_PDUE_DAYS =
                   :QGQS-GQ-REV-PDUE-DAYS,
              GQSFILE.GQS_GQ_REV_BAL = :QGQS-GQ-REV-BAL,
              GQSFILE.GQS_GQ_SEQ = :QGQS-GQ-SEQ
            WHERE GQSFILE.GQS_GQ_COLLID = :QGQS-GQ-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GQS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GQS1-FILE.

           PERFORM DELETE-IT.
           MOVE GQS-PATH-SQL TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.
           PERFORM SET-GQS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GQSFILE
            WHERE GQSFILE.GQS_GQ_COLLID = :QGQS-GQ-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GQS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GQS1-FILE.

           PERFORM CLOSE-IT.

           IF ( GQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQS1-CURSOR-STAT.

