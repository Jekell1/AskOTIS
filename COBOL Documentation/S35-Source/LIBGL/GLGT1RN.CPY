      * COPYMEMBER: LIBGL/GLGT1R
       LOAD-GT1-FILE.
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
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT1-FILE.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
              MOVE 9                    TO QGT1-WEND-SOURCE

              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
                                           QGT1-WEND-DATE3

              MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
              MOVE ALL "9"              TO QGT1-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT1-WEND-ACCTNO
              MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
              MOVE GT-LINE              TO QGT1-WBEG-LINE
              MOVE ALL "9"              TO QGT1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-SOURCE = 9 )
                 MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
              ELSE
                 COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1
              END-IF

              MOVE GT-DATE2             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

              IF ( GT-BRANCH = 9999 )
                 MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1
              END-IF

              IF ( GT-ACCTNO = 99999999 )
                 MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
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
            WHERE (( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= :QGT1-WBEG-LINE )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WSBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WSBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WSBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2 
                                          >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WSBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WSBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WSBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= '1900-01-01'
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 ))
              AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
              AND    GTFILE.GT_DATE2      <= :QGT1-WEND-DATE2 
              AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
              AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
              AND    GTFILE.GT_DATE3      <= :QGT1-WEND-DATE3 
              AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
              AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )
            ORDER BY GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3
                          QGT1-WSBEG-DATE2
                          QGT1-WSBEG-DATE3
                          QGT1-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE
                          QGT1-WSBEG-SOURCE
                          QGT1-WSBEG-BRANCH
                          QGT1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT1-FILE.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-SOURCE           TO QGT-SOURCE.
           MOVE GT-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.
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
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.
           PERFORM CLOSE-IT.
           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT.

