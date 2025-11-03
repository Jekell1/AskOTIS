      * COPYMEMBER: LIBGL/GLGT2RN
      *============================================================
       LOAD-GT2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT2-FILE.
           PERFORM LOAD-GT2-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT2-FILE.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           IF ( GT2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT2-FILE.

           IF ( QGT2-WEND-DATE1 = SPACES )
              MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1

              MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           EXEC SQL
            DECLARE GT2_CURSOR CURSOR FOR
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE GTFILE.GT_DATE1 >= :QGT2-WBEG-DATE1 
              AND GTFILE.GT_DATE1 <= :QGT2-WEND-DATE1
            ORDER BY GTFILE.GT_DATE1,
                     GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT2-WBEG-DATE1
                          QGT2-WEND-DATE1.

           MOVE STAT---GOOD TO GT2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT2-FILE.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE1.

           EXEC SQL
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_DATE1 = :QGT-DATE1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT2_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GT2-FILE.
           PERFORM CLOSE-IT.
           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT2-CURSOR-STAT.

