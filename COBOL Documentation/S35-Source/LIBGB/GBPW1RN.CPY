      * COPYMEMBER: LIBGB/GBPW1RN
       LOAD-PW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PW-ALL-MACHINE
              MOVE PW-ALL-PATH             TO PW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PW-ALL-BASE
              MOVE FILPATH-MACHINE         TO PW-ALL-MACHINE
              MOVE PW-ALL-PATH             TO PW-PATH.

      *-----------------------------------------------------------------
       OPEN-PW1-FILE.
           PERFORM LOAD-PW1-FILE.
           PERFORM OPEN-IT.
           MOVE PW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PW1-FILE.
           PERFORM START-IT.
           MOVE PW-PATH-SQL TO E-FILE.
           MOVE PW1-KEY TO E-KEYX.

           IF ( PW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PW1-FILE.

           IF ( QPW1-WEND-PGM = SPACES    ) OR
              ( QPW1-WEND-SEQ = 0 )
               MOVE PW-PGM    TO QPW1-WBEG-PGM
               MOVE ALL "Z"   TO QPW1-WEND-PGM
               MOVE PW-SEQ    TO QPW1-WBEG-SEQ
               MOVE 999       TO QPW1-WEND-SEQ.

           EXEC SQL
            DECLARE PW1_CURSOR CURSOR FOR
            SELECT PWFILE.PW_PGM,
                   PWFILE.PW_SEQ,
                   CAST(PWFILE.PW_LTOUCH_DATE AS VARCHAR(10)),
                   PWFILE.PW_PASSYN,
                   PWFILE.PW_PASSWORD,
                   PWFILE.PW_DESC,
                   PWFILE.PW_USER_PROFILE,
                   PWFILE.PW_ASK_FG,
                   PWFILE.PW_EXCEPTION_FG
            FROM DBO.PWFILE
            WHERE PWFILE.PW_PGM >= :QPW1-WBEG-PGM
              AND PWFILE.PW_PGM <= :QPW1-WEND-PGM
              AND PWFILE.PW_SEQ >= :QPW1-WBEG-SEQ
              AND PWFILE.PW_SEQ <= :QPW1-WEND-SEQ
            ORDER BY PWFILE.PW_PGM,
                     PWFILE.PW_SEQ
           END-EXEC.

           EXEC SQL
               OPEN PW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QPW1-WBEG-PGM
                          QPW1-WEND-PGM.
           MOVE ZEROES TO QPW1-WBEG-SEQ
                          QPW1-WEND-SEQ.

           MOVE STAT---GOOD TO PW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PW1-FILE.
           PERFORM READ-IT.
           MOVE PW-PATH-SQL TO E-FILE.
           MOVE PW1-KEY TO E-KEYX.
           INITIALIZE QPW-REC.

           MOVE PW-PGM    TO QPW-PGM.
           MOVE PW-SEQ    TO QPW-SEQ.

           EXEC SQL
            SELECT PWFILE.PW_PGM,
                   PWFILE.PW_SEQ,
                   CAST(PWFILE.PW_LTOUCH_DATE AS VARCHAR(10)),
                   PWFILE.PW_PASSYN,
                   PWFILE.PW_PASSWORD,
                   PWFILE.PW_DESC,
                   PWFILE.PW_USER_PROFILE,
                   PWFILE.PW_ASK_FG,
                   PWFILE.PW_EXCEPTION_FG
            INTO :QPW-REC
            FROM DBO.PWFILE
            WHERE PWFILE.PW_PGM = :QPW-PGM
              AND PWFILE.PW_SEQ = :QPW-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PW-FIELDS.

      *-----------------------------------------------------------------
       READ-PW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PW-PATH-SQL TO E-FILE.
           MOVE PW1-KEY TO E-KEYX.
           INITIALIZE QPW-REC.

           IF ( PW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PW1_CURSOR INTO :QPW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PW1-FILE.
           PERFORM CLOSE-IT.
           IF ( PW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PW1-CURSOR-STAT.

