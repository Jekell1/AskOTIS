      * COPYMEMBER: LIBGL/GLLOGS2RN
       LOAD-GLLOGS2-FILE.
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
       OPEN-GLLOGS2-FILE.
           PERFORM LOAD-GLLOGS2-FILE.
           PERFORM OPEN-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GLLOGS2-FILE.
           PERFORM START-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.
           STRING GLLOGS-ID, GLLOGS-BEG-DATE, GLLOGS-END-DATE
                    DELIMITED BY SIZE INTO E-KEYX.

           IF ( GLLOGS2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GLLOGS2-FILE.

           IF ( QGLLOGS2-WEND-ID = SPACES    ) 
              MOVE GLLOGS-ID            TO QGLLOGS2-WBEG-ID
              MOVE ALL "Z"              TO QGLLOGS2-WEND-ID
              MOVE GLLOGS-BEG-DATE      TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WBEG-BEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WEND-BEG-DATE
              MOVE GLLOGS-END-DATE      TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WBEG-END-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WEND-END-DATE.

           EXEC SQL
            DECLARE GLLOGS2_CURSOR CURSOR FOR
            SELECT GLLOGSFILE.GLLOGS_ID,
                   CAST(GLLOGSFILE.GLLOGS_SYSTEM_DATE AS VARCHAR(10)),
                   GLLOGSFILE.GLLOGS_SEQNO,
                   CAST(GLLOGSFILE.GLLOGS_BEG_DATE AS VARCHAR(10)),
                   CAST(GLLOGSFILE.GLLOGS_END_DATE AS VARCHAR(10)),
                   GLLOGSFILE.GLLOGS_BRNO_RANGE,
                   GLLOGSFILE.GLLOGS_TIME,
                   GLLOGSFILE.GLLOGS_RECS_PURGED,
                   GLLOGSFILE.GLLOGS_USERID,
                   CAST(GLLOGSFILE.GLLOGS_LTOUCH_DATE AS VARCHAR(10))
            FROM DBO.GLLOGSFILE
            WHERE GLLOGSFILE.GLLOGS_ID >= :QGLLOGS2-WBEG-ID
              AND GLLOGSFILE.GLLOGS_ID <= :QGLLOGS2-WEND-ID
              AND GLLOGSFILE.GLLOGS_BEG_DATE 
                                       >= :QGLLOGS2-WBEG-BEG-DATE
              AND GLLOGSFILE.GLLOGS_BEG_DATE
                                       <= :QGLLOGS2-WEND-BEG-DATE
              AND GLLOGSFILE.GLLOGS_END_DATE 
                                       >= :QGLLOGS2-WBEG-END-DATE
              AND GLLOGSFILE.GLLOGS_END_DATE 
                                       <= :QGLLOGS2-WEND-END-DATE
            ORDER BY GLLOGSFILE.GLLOGS_ID,
                     GLLOGSFILE.GLLOGS_BEG_DATE,
                     GLLOGSFILE.GLLOGS_END_DATE,
                     GLLOGSFILE.GLLOGS_SYSTEM_DATE,
                     GLLOGSFILE.GLLOGS_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN GLLOGS2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGLLOGS2-WBEG-ID
                          QGLLOGS2-WEND-ID
                          QGLLOGS2-WBEG-BEG-DATE
                          QGLLOGS2-WBEG-END-DATE
                          QGLLOGS2-WEND-BEG-DATE
                          QGLLOGS2-WEND-END-DATE.

           MOVE STAT---GOOD TO GLLOGS2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GLLOGS2-FILE.
           PERFORM READ-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.
           STRING GLLOGS-ID, GLLOGS-BEG-DATE, GLLOGS-END-DATE
                    DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QGLLOGS-REC.

           MOVE GLLOGS-ID           TO QGLLOGS-ID.  
           MOVE GLLOGS-BEG-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS-BEG-DATE.
           MOVE GLLOGS-END-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS-END-DATE.

           EXEC SQL
            SELECT GLLOGSFILE.GLLOGS_ID,
                   CAST(GLLOGSFILE.GLLOGS_SYSTEM_DATE AS VARCHAR(10)),
                   GLLOGSFILE.GLLOGS_SEQNO,
                   CAST(GLLOGSFILE.GLLOGS_BEG_DATE AS VARCHAR(10)),
                   CAST(GLLOGSFILE.GLLOGS_END_DATE AS VARCHAR(10)),
                   GLLOGSFILE.GLLOGS_BRNO_RANGE,
                   GLLOGSFILE.GLLOGS_TIME,
                   GLLOGSFILE.GLLOGS_RECS_PURGED,
                   GLLOGSFILE.GLLOGS_USERID,
                   CAST(GLLOGSFILE.GLLOGS_LTOUCH_DATE AS VARCHAR(10))
            INTO :QGLLOGS-REC
            FROM DBO.GLLOGSFILE
            WHERE GLLOGSFILE.GLLOGS_ID = :QGLLOGS-ID
              AND GLLOGSFILE.GLLOGS_BEG_DATE = :QGLLOGS-BEG-DATE
              AND GLLOGSFILE.GLLOGS_END_DATE = :QGLLOGS-END-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GLLOGS2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GLLOGS-FIELDS.

      *-----------------------------------------------------------------
       READ-GLLOGS2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GLLOGS-PATH-SQL TO E-FILE.
           STRING GLLOGS-ID, GLLOGS-BEG-DATE, GLLOGS-END-DATE
                    DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QGLLOGS-REC.

           IF ( GLLOGS2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GLLOGS2_CURSOR INTO :QGLLOGS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GLLOGS2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GLLOGS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GLLOGS2-FILE.
           PERFORM CLOSE-IT.
           IF ( GLLOGS2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GLLOGS2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GLLOGS2-CURSOR-STAT.

