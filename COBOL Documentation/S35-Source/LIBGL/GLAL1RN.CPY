      * COPYMEMBER: LIBGL/GLAL1RN
       LOAD-AL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( AL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( AL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO AL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO AL-ALL-MACHINE
              MOVE AL-ALL-PATH             TO AL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( AL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO AL-ALL-BASE
              MOVE FILPATH-MACHINE         TO AL-ALL-MACHINE
              MOVE AL-ALL-PATH             TO AL-PATH.

      *-----------------------------------------------------------------
       OPEN-AL1-FILE.
           PERFORM LOAD-AL1-FILE.
           PERFORM OPEN-IT.
           MOVE AL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-AL1-FILE.
           PERFORM START-IT.
           MOVE AL-PATH-SQL TO E-FILE.
           MOVE AL1-KEY TO E-KEYX.

           IF ( AL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-AL1-FILE.

           IF ( QAL1-WEND-ID = SPACES )
              MOVE AL-ID          TO QAL1-WBEG-ID
              MOVE ALL "Z"        TO QAL1-WEND-ID.

           EXEC SQL
            DECLARE AL1_CURSOR CURSOR FOR
            SELECT ALFILE.AL_ID,
                   CAST(ALFILE.AL_LTOUCH_DATE AS VARCHAR(10)),
                   ALFILE.AL_DESC,
                   ALFILE.AL_NO_ACCTS,
                   ALFILE.AL_ACCT_1,
                   ALFILE.AL_PCT_1,
                   ALFILE.AL_ACCT_2,
                   ALFILE.AL_PCT_2,
                   ALFILE.AL_ACCT_3,
                   ALFILE.AL_PCT_3,
                   ALFILE.AL_ACCT_4,
                   ALFILE.AL_PCT_4,
                   ALFILE.AL_ACCT_5,
                   ALFILE.AL_PCT_5,
                   ALFILE.AL_ACCT_6,
                   ALFILE.AL_PCT_6,
                   ALFILE.AL_ACCT_7,
                   ALFILE.AL_PCT_7,
                   ALFILE.AL_ACCT_8,
                   ALFILE.AL_PCT_8,
                   ALFILE.AL_ACCT_9,
                   ALFILE.AL_PCT_9,
                   ALFILE.AL_ACCT_10,
                   ALFILE.AL_PCT_10,
                   ALFILE.AL_TYPE
            FROM DBO.ALFILE
            WHERE ALFILE.AL_ID >= :QAL1-WBEG-ID
              AND ALFILE.AL_ID <= :QAL1-WEND-ID
            ORDER BY ALFILE.AL_ID
           END-EXEC.

           EXEC SQL
               OPEN AL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QAL1-WBEG-ID
                          QAL1-WEND-ID.

           MOVE STAT---GOOD TO AL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-AL1-FILE.
           PERFORM READ-IT.
           MOVE AL-PATH-SQL TO E-FILE.
           MOVE AL1-KEY TO E-KEYX.
           INITIALIZE QAL-REC.

           MOVE AL-ID     TO QAL-ID.

           EXEC SQL
            SELECT ALFILE.AL_ID,
                   CAST(ALFILE.AL_LTOUCH_DATE AS VARCHAR(10)),
                   ALFILE.AL_DESC,
                   ALFILE.AL_NO_ACCTS,
                   ALFILE.AL_ACCT_1,
                   ALFILE.AL_PCT_1,
                   ALFILE.AL_ACCT_2,
                   ALFILE.AL_PCT_2,
                   ALFILE.AL_ACCT_3,
                   ALFILE.AL_PCT_3,
                   ALFILE.AL_ACCT_4,
                   ALFILE.AL_PCT_4,
                   ALFILE.AL_ACCT_5,
                   ALFILE.AL_PCT_5,
                   ALFILE.AL_ACCT_6,
                   ALFILE.AL_PCT_6,
                   ALFILE.AL_ACCT_7,
                   ALFILE.AL_PCT_7,
                   ALFILE.AL_ACCT_8,
                   ALFILE.AL_PCT_8,
                   ALFILE.AL_ACCT_9,
                   ALFILE.AL_PCT_9,
                   ALFILE.AL_ACCT_10,
                   ALFILE.AL_PCT_10,
                   ALFILE.AL_TYPE
            INTO :QAL-REC
            FROM DBO.ALFILE
            WHERE ALFILE.AL_ID = :QAL-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-AL-FIELDS.

      *-----------------------------------------------------------------
       READ-AL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE AL-PATH-SQL TO E-FILE.
           MOVE AL1-KEY TO E-KEYX.
           INITIALIZE QAL-REC.

           IF ( AL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH AL1_CURSOR INTO :QAL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-AL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-AL1-FILE.
           PERFORM CLOSE-IT.
           IF ( AL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE AL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO AL1-CURSOR-STAT.

