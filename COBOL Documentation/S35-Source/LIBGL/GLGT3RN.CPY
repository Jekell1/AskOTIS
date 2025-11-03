      * COPYMEMBER: LIBGL/GLGT3RN
      ******************************************************************
      *
      * DESCRIPTION: IO ROUTINE FOR GT3-KEY
      *
      *=================================================================
      * REV:
      * KEC 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED GT3-KEY (A15) AND MAKE GT4-KEY (A15) TO
      *         NOW BE THE NEW GT3-KEY (A30).
      *
      ******************************************************************
       LOAD-GT3-FILE.
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
       OPEN-GT3-FILE.
           PERFORM LOAD-GT3-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT3-FILE.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT3-KEY TO E-KEYX.
           IF ( GT3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT3-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT3-WEND-LINE = ZEROES    ) OR
              ( QGT3-WEND-LINE NOT NUMERIC )
              MOVE GT-BRANCH            TO QGT3-WBEG-BRANCH
              MOVE ALL "9"              TO QGT3-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT3-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT3-WEND-ACCTNO

              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3

              MOVE GT-DRCRFG            TO QGT3-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT3-WEND-DRCRFG
              MOVE GT-LINE              TO QGT3-WBEG-LINE
              MOVE ALL "9"              TO QGT3-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-BRANCH = 9999 )
                 MOVE GT-BRANCH         TO QGT3-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1
              END-IF

              IF ( GT-ACCTNO = 99999999 )
                 MOVE GT-ACCTNO         TO QGT3-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT3_CURSOR CURSOR FOR
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
            WHERE (( GTFILE.GT_BRANCH     >= :QGT3-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT3-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT3-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT3-WBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= :QGT3-WBEG-LINE )
               OR  ( GTFILE.GT_BRANCH     >= :QGT3-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT3-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT3-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT3-WSBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_BRANCH     >= :QGT3-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT3-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT3-WSBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_BRANCH     >= :QGT3-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT3-WSBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_BRANCH     >= :QGT3-WSBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 ))
              AND (  GTFILE.GT_BRANCH     <= :QGT3-WEND-BRANCH
              AND    GTFILE.GT_ACCTNO     <= :QGT3-WEND-ACCTNO
              AND    GTFILE.GT_DATE3      <= :QGT3-WEND-DATE3 
              AND    GTFILE.GT_DRCRFG     <= :QGT3-WEND-DRCRFG
              AND    GTFILE.GT_LINE       <= :QGT3-WEND-LINE )
            ORDER BY GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE,
                     GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2
           END-EXEC.

           EXEC SQL
               OPEN GT3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT3-WBEG-DRCRFG
                          QGT3-WEND-DRCRFG
                          QGT3-WBEG-DATE3
                          QGT3-WEND-DATE3
                          QGT3-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT3-WBEG-BRANCH
                          QGT3-WEND-BRANCH
                          QGT3-WBEG-ACCTNO
                          QGT3-WEND-ACCTNO
                          QGT3-WBEG-LINE
                          QGT3-WEND-LINE
                          QGT3-WSBEG-BRANCH
                          QGT3-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT3-FILE.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT3-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-BRANCH           TO QGT-BRANCH.
           MOVE GT-ACCTNO           TO QGT-ACCTNO.
           MOVE GT-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.
           MOVE GT-DRCRFG           TO QGT-DRCRFG.
           MOVE GT-LINE             TO QGT-LINE.

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
            WHERE GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT3-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT3_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GT3-FILE.
           PERFORM CLOSE-IT.
           IF ( GT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT3-CURSOR-STAT.

