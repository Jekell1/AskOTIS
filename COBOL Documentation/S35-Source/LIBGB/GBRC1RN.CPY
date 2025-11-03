      * COPYMEMBER: LIBGB/GBRC1RN
       LOAD-RC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *-----------------------------------------------------------------
       OPEN-RC1-FILE.
           PERFORM LOAD-RC1-FILE.
           PERFORM OPEN-IT.
           MOVE RC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RC1-FILE.
           PERFORM START-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           IF ( RC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RC1-FILE.

           IF ( QRC1-WEND-BRNO = ZEROES    ) OR
              ( QRC1-WEND-BRNO NOT NUMERIC )
              MOVE RC-BRNO              TO QRC1-WBEG-BRNO
              MOVE ALL "9"              TO QRC1-WEND-BRNO

              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WEND-TRANS-DATE.

           EXEC SQL
            DECLARE RC1_CURSOR CURSOR FOR
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO >= :QRC1-WBEG-BRNO
              AND RCFILE.RC_BRNO <= :QRC1-WEND-BRNO
              AND RCFILE.RC_TRANS_DATE >= :QRC1-WBEG-TRANS-DATE 
              AND RCFILE.RC_TRANS_DATE <= :QRC1-WEND-TRANS-DATE
            ORDER BY RCFILE.RC_BRNO,
                     RCFILE.RC_TRANS_DATE
           END-EXEC.

           EXEC SQL
               OPEN RC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRC1-WBEG-BRNO
                          QRC1-WEND-BRNO.

           MOVE SPACES TO QRC1-WBEG-TRANS-DATE
                          QRC1-WEND-TRANS-DATE.

           MOVE STAT---GOOD TO RC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RC1-FILE.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           MOVE RC-BRNO          TO QRC-BRNO.  
           MOVE RC-TRANS-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC-TRANS-DATE.
           MOVE RC-STATUS        TO QRC-STATUS.

           EXEC SQL
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE
                                                  AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            INTO :QRC-REC
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       READ-RC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC1_CURSOR INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RC1-FILE.
           PERFORM CLOSE-IT.
           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RC1-CURSOR-STAT.

