      * COPYMEMBER: LIBSP/SPCQS1IN
      *=================================================================
      *  NOTE!!! MUST SET CQS-CQ-BRNO IN THE WRITE & REWRITE BUT NOT
      *          IN THE DELETE, READ, READ-NEXT AND START.
      *          JUST NEED TO MAKE SURE THAT THE CQS-CQ2-KEY-FIELDS
      *          ARE MATCHING THE SIZE & POSITION FOR CQ2-KEY FIELDS.
      *          CS & KC 2019-0307
      *=================================================================
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

           IF ( QCQS1-WEND-COLLID = SPACES    ) 
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
       WRITE-CQS1-FILE.

           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           ACCEPT CQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CQS-PATH-SQL    TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           PERFORM SET-CQS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CQSFILE
             (CQSFILE.CQS_BRNO,
              CQSFILE.CQS_COLLID,
              CQSFILE.CQS_LTOUCH_DATE,
              CQSFILE.CQS_TOT_REC,
              CQSFILE.CQS_WORKED_REC,
              CQSFILE.CQS_TIMED_REC,
              CQSFILE.CQS_CQ_BRNO,
              CQSFILE.CQS_CQ_COLLID,
              CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK,
              CQSFILE.CQS_CQ_SEQ_FG,
              CQSFILE.CQS_CQ_DATE,
              CQSFILE.CQS_CQ_START_TIME,
              CQSFILE.CQS_CQ_REV_DEL,
              CQSFILE.CQS_CQ_MISC_KEY_FLD,
              CQSFILE.CQS_CQ_REV_PDUE_DAYS,
              CQSFILE.CQS_CQ_REV_BAL,
              CQSFILE.CQS_CQ_SEQ)
            VALUES
             (:QCQS-BRNO,
              :QCQS-COLLID,
              :QCQS-LTOUCH-DATE,
              :QCQS-TOT-REC,
              :QCQS-WORKED-REC,
              :QCQS-TIMED-REC,
              :QCQS-CQ-BRNO,
              :QCQS-CQ-COLLID,
              :QCQS-CQ-REV-BEHAVIOR-RISK,
              :QCQS-CQ-SEQ-FG,
              :QCQS-CQ-DATE,
              :QCQS-CQ-START-TIME,
              :QCQS-CQ-REV-DEL,
              :QCQS-CQ-MISC-KEY-FLD,
              :QCQS-CQ-REV-PDUE-DAYS,
              :QCQS-CQ-REV-BAL,
              :QCQS-CQ-SEQ)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CQS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CQS1-FILE.

           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           ACCEPT CQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CQS-PATH-SQL    TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           PERFORM SET-CQS-FIELDS.

           EXEC SQL
            UPDATE DBO.CQSFILE
             SET
              CQSFILE.CQS_BRNO = :QCQS-BRNO,
              CQSFILE.CQS_COLLID = :QCQS-COLLID,
              CQSFILE.CQS_LTOUCH_DATE =
                 :QCQS-LTOUCH-DATE,
              CQSFILE.CQS_TOT_REC = :QCQS-TOT-REC,
              CQSFILE.CQS_WORKED_REC = :QCQS-WORKED-REC,
              CQSFILE.CQS_TIMED_REC = :QCQS-TIMED-REC,
              CQSFILE.CQS_CQ_BRNO = :QCQS-CQ-BRNO,
              CQSFILE.CQS_CQ_COLLID = :QCQS-CQ-COLLID,
              CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK =
                   :QCQS-CQ-REV-BEHAVIOR-RISK,
              CQSFILE.CQS_CQ_SEQ_FG = :QCQS-CQ-SEQ-FG,
              CQSFILE.CQS_CQ_DATE = :QCQS-CQ-DATE,
              CQSFILE.CQS_CQ_START_TIME = :QCQS-CQ-START-TIME,
              CQSFILE.CQS_CQ_REV_DEL = :QCQS-CQ-REV-DEL,
              CQSFILE.CQS_CQ_MISC_KEY_FLD = :QCQS-CQ-MISC-KEY-FLD,
              CQSFILE.CQS_CQ_REV_PDUE_DAYS =
                   :QCQS-CQ-REV-PDUE-DAYS,
              CQSFILE.CQS_CQ_REV_BAL = :QCQS-CQ-REV-BAL,
              CQSFILE.CQS_CQ_SEQ = :QCQS-CQ-SEQ
             WHERE CQSFILE.CQS_BRNO = :QCQS-BRNO
               AND CQSFILE.CQS_COLLID = :QCQS-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CQS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CQS1-FILE.

           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           PERFORM DELETE-IT.
           MOVE CQS-PATH-SQL    TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           PERFORM SET-CQS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CQSFILE
             WHERE CQSFILE.CQS_BRNO = :QCQS-BRNO
               AND CQSFILE.CQS_COLLID = :QCQS-COLLID
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CQS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CQS1-FILE.

           PERFORM CLOSE-IT.

           IF ( CQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQS1-CURSOR-STAT.
