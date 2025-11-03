      * COPYMEMBER: LIBLP/LPBX1RN
       LOAD-BX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BX-ALL-DATA
              MOVE BX-ALL-PATH             TO BX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BX-ALL-BASE
              MOVE FILPATH-MACHINE         TO BX-ALL-MACHINE
              MOVE FILPATH-DATA            TO BX-ALL-DATA
              MOVE BX-ALL-PATH             TO BX-PATH.

      *-----------------------------------------------------------------
       OPEN-BX1-FILE.
           PERFORM LOAD-BX1-FILE.
           PERFORM OPEN-IT.
           MOVE BX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BX1-FILE.
           PERFORM START-IT.
           MOVE BX-PATH-OWNBR  TO BX-BRNO.
           MOVE BX-PATH-SQL    TO E-FILE.
           MOVE BX1-KEY        TO E-KEYX.

           IF ( BX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BX1-FILE.

           IF ( QBX1-WEND-BRNO = ZEROES    ) OR
              ( QBX1-WEND-BRNO NOT NUMERIC )
              MOVE BX-PATH-OWNBR        TO QBX1-WBEG-BRNO
              MOVE BX-PATH-OWNBR        TO QBX1-WEND-BRNO
              MOVE BX-SSNO              TO QBX1-WBEG-SSNO
              MOVE ALL "9"              TO QBX1-WEND-SSNO.

           EXEC SQL
            DECLARE BX1_CURSOR CURSOR FOR
            SELECT BXFILE.BX_BRNO,
                   BXFILE.BX_SSNO,
                   CAST(BXFILE.BX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BXFILE.BX_ALT_ADR1,
                   BXFILE.BX_ALT_ADR2,
                   BXFILE.BX_ALT_CITY,
                   BXFILE.BX_ALT_STATE,
                   BXFILE.BX_ALT_ZIP,
                   BXFILE.BX_PREV_ADR1,
                   BXFILE.BX_PREV_ADR2,
                   BXFILE.BX_PREV_CITY,
                   BXFILE.BX_PREV_STATE,
                   BXFILE.BX_PREV_ZIP,
                   BXFILE.BX_KYC_ADR1,
                   BXFILE.BX_KYC_ADR2,
                   BXFILE.BX_KYC_CITY,
                   BXFILE.BX_KYC_STATE,
                   BXFILE.BX_KYC_ZIP,
                   BXFILE.BX_KYC_IDTYPE,
                   BXFILE.BX_KYC_ISSAUTH,
                   BXFILE.BX_KYC_NUMBER,
                   CAST(BXFILE.BX_KYC_ISSDATE
                                                  AS VARCHAR(10)),
                   CAST(BXFILE.BX_KYC_EXPDATE
                                                  AS VARCHAR(10)),
                   CAST(BXFILE.BX_KYC_LAST_VERDATE
                                                  AS VARCHAR(10))
            FROM DBO.BXFILE
            WHERE BXFILE.BX_BRNO  = :QBX1-WBEG-BRNO
              AND BXFILE.BX_SSNO >= :QBX1-WBEG-SSNO
              AND BXFILE.BX_SSNO <= :QBX1-WEND-SSNO
            ORDER BY BXFILE.BX_BRNO, BXFILE.BX_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBX1-WBEG-BRNO 
                          QBX1-WEND-BRNO
                          QBX1-WBEG-SSNO
                          QBX1-WEND-SSNO.

           MOVE STAT---GOOD TO BX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BX1-FILE.
           PERFORM READ-IT.
           MOVE BX-PATH-OWNBR  TO BX-BRNO. 
           MOVE BX-PATH-SQL    TO E-FILE.
           MOVE BX1-KEY        TO E-KEYX.
           INITIALIZE QBX-REC.

           MOVE BX-BRNO   TO QBX-BRNO.
           MOVE BX-SSNO   TO QBX-SSNO.

           EXEC SQL
            SELECT BXFILE.BX_BRNO,
                   BXFILE.BX_SSNO,
                   CAST(BXFILE.BX_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BXFILE.BX_ALT_ADR1,
                   BXFILE.BX_ALT_ADR2,
                   BXFILE.BX_ALT_CITY,
                   BXFILE.BX_ALT_STATE,
                   BXFILE.BX_ALT_ZIP,
                   BXFILE.BX_PREV_ADR1,
                   BXFILE.BX_PREV_ADR2,
                   BXFILE.BX_PREV_CITY,
                   BXFILE.BX_PREV_STATE,
                   BXFILE.BX_PREV_ZIP,
                   BXFILE.BX_KYC_ADR1,
                   BXFILE.BX_KYC_ADR2,
                   BXFILE.BX_KYC_CITY,
                   BXFILE.BX_KYC_STATE,
                   BXFILE.BX_KYC_ZIP,
                   BXFILE.BX_KYC_IDTYPE,
                   BXFILE.BX_KYC_ISSAUTH,
                   BXFILE.BX_KYC_NUMBER,
                   CAST(BXFILE.BX_KYC_ISSDATE
                                                  AS VARCHAR(10)),
                   CAST(BXFILE.BX_KYC_EXPDATE
                                                  AS VARCHAR(10)),
                   CAST(BXFILE.BX_KYC_LAST_VERDATE
                                                  AS VARCHAR(10))
            INTO :QBX-REC
            FROM DBO.BXFILE
            WHERE BXFILE.BX_BRNO = :QBX-BRNO
              AND BXFILE.BX_SSNO = :QBX-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BX-FIELDS.

      *-----------------------------------------------------------------
       READ-BX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BX-PATH-OWNBR  TO BX-BRNO.
           MOVE BX-PATH-SQL    TO E-FILE.
           MOVE BX1-KEY        TO E-KEYX.
           INITIALIZE QBX-REC.

           IF ( BX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BX1_CURSOR INTO :QBX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BX1-FILE.

           PERFORM CLOSE-IT.
           IF ( BX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BX1-CURSOR-STAT.

