      * COPYMEMBER: LIBGL/GLLOGSIN
       LOAD-GLLOGS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GLLOGS-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GLLOGS-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GLLOGS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GLLOGS-ALL-MACHINE
              MOVE GLLOGS-ALL-PATH             TO GLLOGS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GLLOGS-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GLLOGS-ALL-BASE
              MOVE FILPATH-MACHINE         TO GLLOGS-ALL-MACHINE
              MOVE GLLOGS-ALL-PATH             TO GLLOGS-PATH.

      *-----------------------------------------------------------------
       OPEN-GLLOGS1-FILE.
           PERFORM LOAD-GLLOGS1-FILE.
           PERFORM OPEN-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GLLOGS1-FILE.
           PERFORM START-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.
           MOVE GLLOGS1-KEY TO E-KEYX.

           IF ( GLLOGS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GLLOGS1-FILE.

           IF ( QGLLOGS1-WEND-SEQNO = ZEROES OR
                QGLLOGS1-WEND-SEQNO NOT NUMERIC    ) 
              MOVE GLLOGS-ID            TO QGLLOGS1-WBEG-ID
              MOVE ALL "Z"              TO QGLLOGS1-WEND-ID
              MOVE GLLOGS-SYSTEM-DATE   TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS1-WBEG-SYSTEM-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS1-WEND-SYSTEM-DATE
              MOVE GLLOGS-SEQNO         TO QGLLOGS1-WBEG-SEQNO
              MOVE ALL "9"              TO QGLLOGS1-WEND-SEQNO.

           EXEC SQL
            DECLARE GLLOGS1_CURSOR CURSOR FOR
            SELECT ISNULL(GLLOGSFILE.GLLOGS_ID,''),
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_SYSTEM_DATE
                                                  AS VARCHAR(10)),'0'),
                   GLLOGSFILE.GLLOGS_SEQNO,
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_BEG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_END_DATE
                                                  AS VARCHAR(10)),'0'),
                   GLLOGSFILE.GLLOGS_BRNO_RANGE,
                   GLLOGSFILE.GLLOGS_TIME,
                   GLLOGSFILE.GLLOGS_RECS_PURGED,
                   ISNULL(GLLOGSFILE.GLLOGS_USERID,''),
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.GLLOGSFILE
            WHERE GLLOGSFILE.GLLOGS_ID >= :QGLLOGS1-WBEG-ID
              AND GLLOGSFILE.GLLOGS_ID <= :QGLLOGS1-WEND-ID
              AND CAST(GLLOGSFILE.GLLOGS_SYSTEM_DATE AS VARCHAR(10))
                           >= :QGLLOGS1-WBEG-SYSTEM-DATE
              AND CAST(GLLOGSFILE.GLLOGS_SYSTEM_DATE AS VARCHAR(10))
                           <= :QGLLOGS1-WEND-SYSTEM-DATE
              AND GLLOGSFILE.GLLOGS_SEQNO >= :QGLLOGS1-WBEG-SEQNO
              AND GLLOGSFILE.GLLOGS_SEQNO <= :QGLLOGS1-WEND-SEQNO
            ORDER BY GLLOGSFILE.GLLOGS_ID,
                     GLLOGSFILE.GLLOGS_SYSTEM_DATE,
                     GLLOGSFILE.GLLOGS_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN GLLOGS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGLLOGS1-WBEG-ID
                          QGLLOGS1-WEND-ID
                          QGLLOGS1-WBEG-SYSTEM-DATE
                          QGLLOGS1-WEND-SYSTEM-DATE.

           MOVE ZEROES TO QGLLOGS1-WBEG-SEQNO
                          QGLLOGS1-WEND-SEQNO.

           MOVE STAT---GOOD TO GLLOGS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GLLOGS1-FILE.
           PERFORM READ-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.
           MOVE GLLOGS1-KEY TO E-KEYX.

           MOVE GLLOGS-ID            TO QGLLOGS-ID.  
           MOVE GLLOGS-SYSTEM-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS-SYSTEM-DATE.
           MOVE GLLOGS-SEQNO         TO QGLLOGS-SEQNO.

           EXEC SQL
            SELECT ISNULL(GLLOGSFILE.GLLOGS_ID,''),
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_SYSTEM_DATE
                                                  AS VARCHAR(10)),'0'),
                   GLLOGSFILE.GLLOGS_SEQNO,
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_BEG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_END_DATE
                                                  AS VARCHAR(10)),'0'),
                   GLLOGSFILE.GLLOGS_BRNO_RANGE,
                   GLLOGSFILE.GLLOGS_TIME,
                   GLLOGSFILE.GLLOGS_RECS_PURGED,
                   ISNULL(GLLOGSFILE.GLLOGS_USERID,''),
                   ISNULL(CAST(GLLOGSFILE.GLLOGS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QGLLOGS-REC
            FROM DBO.GLLOGSFILE
            WHERE GLLOGSFILE.GLLOGS_ID = :QGLLOGS-ID
              AND GLLOGSFILE.GLLOGS_SYSTEM_DATE = :QGLLOGS-SYSTEM-DATE
              AND GLLOGSFILE.GLLOGS_SEQNO = :QGLLOGS-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GLLOGS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GLLOGS-FIELDS.

      *-----------------------------------------------------------------
       READ-GLLOGS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.
           MOVE GLLOGS1-KEY TO E-KEYX.
           INITIALIZE QGLLOGS-REC.

           IF ( GLLOGS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GLLOGS1_CURSOR INTO :QGLLOGS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GLLOGS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GLLOGS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GLLOGS1-FILE.

           PERFORM CLOSE-IT.

           IF ( GLLOGS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GLLOGS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GLLOGS1-CURSOR-STAT.

