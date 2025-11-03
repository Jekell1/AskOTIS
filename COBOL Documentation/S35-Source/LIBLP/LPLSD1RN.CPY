      * COPYMEMBER: LIBLP/LPLSD1R
       LOAD-LSD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSD-ALL-DATA
              MOVE LSD-ALL-PATH             TO LSD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSD-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSD-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSD-ALL-DATA
              MOVE LSD-ALL-PATH             TO LSD-PATH.

      *-----------------------------------------------------------------
       OPEN-LSD1-FILE.
           PERFORM LOAD-LSD1-FILE.
           PERFORM OPEN-IT.
           MOVE LSD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSD1-FILE.
           PERFORM START-IT.
           MOVE LSD-PATH-OWNBR  TO LSD-BRNO.
           MOVE LSD-PATH-SQL    TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.

           IF ( LSD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSD1-FILE.

           IF ( QLSD1-WEND-BRNO = ZEROES    ) OR
              ( QLSD1-WEND-BRNO NOT NUMERIC )
              MOVE LSD-PATH-OWNBR        TO QLSD1-WBEG-BRNO
              MOVE LSD-PATH-OWNBR        TO QLSD1-WEND-BRNO
              MOVE LSD-ACCTNO            TO QLSD1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSD1-WEND-ACCTNO
              MOVE LSD-SEQNO             TO QLSD1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSD1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSD1_CURSOR CURSOR FOR
            SELECT LSDFILE.LSD_BRNO,
                   LSDFILE.LSD_ACCTNO,
                   LSDFILE.LSD_SEQNO,
                   CAST(LSDFILE.LSD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSDFILE.LSD_STOLEN_DATE
                                                  AS VARCHAR(10)),
                   LSDFILE.LSD_GAP_INS_PAID,
                   LSDFILE.LSD_OTHER_EXPENSE,
                   LSDFILE.LSD_MISC_AMT_EXPENSE,
                   LSDFILE.LSD_MISC_DESC_EXPENSE,
                   LSDFILE.LSD_TOT_EXPENSE,
                   LSDFILE.LSD_INT_REB,
                   LSDFILE.LSD_CPP_REB,
                   LSDFILE.LSD_GAP_REB,
                   LSDFILE.LSD_WARRANTY_REB,
                   LSDFILE.LSD_MISC_AMT_CREDIT,
                   LSDFILE.LSD_MISC_DESC_CREDIT,
                   LSDFILE.LSD_TOT_CREDITS,
                   LSDFILE.LSD_PAID_TO_OTHER,
                   LSDFILE.LSD_AMT_FROM_OWING,
                   LSDFILE.LSD_AMT_OF_SURPLUS,
                   LSDFILE.LSD_AMT_OF_DEFICIENCY,
                   LSDFILE.LSD_SEND_FMNO,
                   LSDFILE.LSD_INSURANCE_PROCEEDS,
                   CAST(LSDFILE.LSD_DATE_CHECK_RECV
                                                  AS VARCHAR(10)),
                   LSDFILE.LSD_INSURANCE_CHECK,
                   LSDFILE.LSD_WHO_ADJUSTED,
                   LSDFILE.LSD_INS_COMPANY,
                   CAST(LSDFILE.LSD_DEF_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSDFILE.LSD_SALES_DATE
                                                  AS VARCHAR(10)),
                   LSDFILE.LSD_AUCTION,
                   LSDFILE.LSD_AUCTN_TIME,
                   LSDFILE.LSD_AUCTN_ADR,
                   LSDFILE.LSD_AUCTN_CITY,
                   LSDFILE.LSD_AUCTN_STATE,
                   LSDFILE.LSD_AUCTN_ZIP,
                   LSDFILE.LSD_AUCTN_PHONE,
                   LSDFILE.LSD_REPO_CHARGE,
                   LSDFILE.LSD_STORAGE_CHARGE,
                   LSDFILE.LSD_REPAIR_CHARGE,
                   LSDFILE.LSD_OTHER_CHARGE,
                   LSDFILE.LSD_STORAGE_RATE,
                   LSDFILE.LSD_EST_REBATE,
                   LSDFILE.LSD_MINIMUM_BID,
                   LSDFILE.LSD_MAY_REINSTATE,
                   LSDFILE.LSD_MAY_NOT_REINSTATE,
                   LSDFILE.LSD_IN_PAST_12MONTHS,
                   LSDFILE.LSD_2X_DURING_CONTRACT,
                   LSDFILE.LSD_OTHER,
                   LSDFILE.LSD_OTHER_COMMENTS1,
                   LSDFILE.LSD_OTHER_COMMENTS2,
                   LSDFILE.LSD_CA_PYMTS_AS_AGREED,
                   LSDFILE.LSD_CA_KEEP_INS,
                   LSDFILE.LSD_CA_KEEP_LIEN_FREE,
                   LSDFILE.LSD_CA_OTHER,
                   LSDFILE.LSD_CA_OTHER_DESC,
                   LSDFILE.LSD_CURE
            FROM DBO.LSDFILE
            WHERE LSDFILE.LSD_BRNO  = :QLSD1-WBEG-BRNO
              AND LSDFILE.LSD_ACCTNO >= :QLSD1-WBEG-ACCTNO
              AND LSDFILE.LSD_ACCTNO <= :QLSD1-WEND-ACCTNO
              AND LSDFILE.LSD_SEQNO >= :QLSD1-WBEG-SEQNO
              AND LSDFILE.LSD_SEQNO <= :QLSD1-WEND-SEQNO
            ORDER BY LSDFILE.LSD_BRNO,
                     LSDFILE.LSD_ACCTNO,
                     LSDFILE.LSD_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSD1-WBEG-BRNO
                          QLSD1-WEND-BRNO
                          QLSD1-WBEG-ACCTNO
                          QLSD1-WEND-ACCTNO
                          QLSD1-WBEG-SEQNO
                          QLSD1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSD1-FILE.
           PERFORM READ-IT.
           MOVE LSD-PATH-OWNBR  TO LSD-BRNO. 
           MOVE LSD-PATH-SQL    TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.
           INITIALIZE QLSD-REC.

           MOVE LSD-BRNO   TO QLSD-BRNO.
           MOVE LSD-ACCTNO TO QLSD-ACCTNO.
           MOVE LSD-SEQNO  TO QLSD-SEQNO.

           EXEC SQL
            SELECT LSDFILE.LSD_BRNO,
                   LSDFILE.LSD_ACCTNO,
                   LSDFILE.LSD_SEQNO,
                   CAST(LSDFILE.LSD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSDFILE.LSD_STOLEN_DATE
                                                  AS VARCHAR(10)),
                   LSDFILE.LSD_GAP_INS_PAID,
                   LSDFILE.LSD_OTHER_EXPENSE,
                   LSDFILE.LSD_MISC_AMT_EXPENSE,
                   LSDFILE.LSD_MISC_DESC_EXPENSE,
                   LSDFILE.LSD_TOT_EXPENSE,
                   LSDFILE.LSD_INT_REB,
                   LSDFILE.LSD_CPP_REB,
                   LSDFILE.LSD_GAP_REB,
                   LSDFILE.LSD_WARRANTY_REB,
                   LSDFILE.LSD_MISC_AMT_CREDIT,
                   LSDFILE.LSD_MISC_DESC_CREDIT,
                   LSDFILE.LSD_TOT_CREDITS,
                   LSDFILE.LSD_PAID_TO_OTHER,
                   LSDFILE.LSD_AMT_FROM_OWING,
                   LSDFILE.LSD_AMT_OF_SURPLUS,
                   LSDFILE.LSD_AMT_OF_DEFICIENCY,
                   LSDFILE.LSD_SEND_FMNO,
                   LSDFILE.LSD_INSURANCE_PROCEEDS,
                   CAST(LSDFILE.LSD_DATE_CHECK_RECV
                                                  AS VARCHAR(10)),
                   LSDFILE.LSD_INSURANCE_CHECK,
                   LSDFILE.LSD_WHO_ADJUSTED,
                   LSDFILE.LSD_INS_COMPANY,
                   CAST(LSDFILE.LSD_DEF_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),
                   CAST(LSDFILE.LSD_SALES_DATE
                                                  AS VARCHAR(10)),
                   LSDFILE.LSD_AUCTION,
                   LSDFILE.LSD_AUCTN_TIME,
                   LSDFILE.LSD_AUCTN_ADR,
                   LSDFILE.LSD_AUCTN_CITY,
                   LSDFILE.LSD_AUCTN_STATE,
                   LSDFILE.LSD_AUCTN_ZIP,
                   LSDFILE.LSD_AUCTN_PHONE,
                   LSDFILE.LSD_REPO_CHARGE,
                   LSDFILE.LSD_STORAGE_CHARGE,
                   LSDFILE.LSD_REPAIR_CHARGE,
                   LSDFILE.LSD_OTHER_CHARGE,
                   LSDFILE.LSD_STORAGE_RATE,
                   LSDFILE.LSD_EST_REBATE,
                   LSDFILE.LSD_MINIMUM_BID,
                   LSDFILE.LSD_MAY_REINSTATE,
                   LSDFILE.LSD_MAY_NOT_REINSTATE,
                   LSDFILE.LSD_IN_PAST_12MONTHS,
                   LSDFILE.LSD_2X_DURING_CONTRACT,
                   LSDFILE.LSD_OTHER,
                   LSDFILE.LSD_OTHER_COMMENTS1,
                   LSDFILE.LSD_OTHER_COMMENTS2,
                   LSDFILE.LSD_CA_PYMTS_AS_AGREED,
                   LSDFILE.LSD_CA_KEEP_INS,
                   LSDFILE.LSD_CA_KEEP_LIEN_FREE,
                   LSDFILE.LSD_CA_OTHER,
                   LSDFILE.LSD_CA_OTHER_DESC,
                   LSDFILE.LSD_CURE
            INTO :QLSD-REC
            FROM DBO.LSDFILE
            WHERE LSDFILE.LSD_BRNO = :QLSD-BRNO
              AND LSDFILE.LSD_ACCTNO = :QLSD-ACCTNO
              AND LSDFILE.LSD_SEQNO = :QLSD-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSD-FIELDS.

      *-----------------------------------------------------------------
       READ-LSD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSD-PATH-OWNBR  TO LSD-BRNO.
           MOVE LSD-PATH-SQL    TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.
           INITIALIZE QLSD-REC.

           IF ( LSD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSD1_CURSOR INTO :QLSD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSD1-FILE.

           PERFORM CLOSE-IT.
           IF ( LSD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSD1-CURSOR-STAT.

