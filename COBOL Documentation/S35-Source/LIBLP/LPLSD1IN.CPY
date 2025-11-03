      * COPYMEMBER: LIBLP/LPLSD1IN
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
            WHERE LSDFILE.LSD_BRNO = :QLSD1-WBEG-BRNO
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
       WRITE-LSD1-FILE.

           MOVE LSD-PATH-OWNBR  TO LSD-BRNO.
           ACCEPT LSD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSD-PATH-SQL    TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.
           PERFORM SET-LSD-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSDFILE
             (LSDFILE.LSD_BRNO,
              LSDFILE.LSD_ACCTNO,
              LSDFILE.LSD_SEQNO,
              LSDFILE.LSD_LTOUCH_DATE,
              LSDFILE.LSD_STOLEN_DATE,
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
              LSDFILE.LSD_DATE_CHECK_RECV,
              LSDFILE.LSD_INSURANCE_CHECK,
              LSDFILE.LSD_WHO_ADJUSTED,
              LSDFILE.LSD_INS_COMPANY,
              LSDFILE.LSD_DEF_LETTER_SENT_DATE,
              LSDFILE.LSD_SALES_DATE,
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
              LSDFILE.LSD_CURE)
            VALUES
             (:QLSD-BRNO,
              :QLSD-ACCTNO,
              :QLSD-SEQNO,
              :QLSD-LTOUCH-DATE,
              :QLSD-STOLEN-DATE,
              :QLSD-GAP-INS-PAID,
              :QLSD-OTHER-EXPENSE,
              :QLSD-MISC-AMT-EXPENSE,
              :QLSD-MISC-DESC-EXPENSE,
              :QLSD-TOT-EXPENSE,
              :QLSD-INT-REB,
              :QLSD-CPP-REB,
              :QLSD-GAP-REB,
              :QLSD-WARRANTY-REB,
              :QLSD-MISC-AMT-CREDIT,
              :QLSD-MISC-DESC-CREDIT,
              :QLSD-TOT-CREDITS,
              :QLSD-PAID-TO-OTHER,
              :QLSD-AMT-FROM-OWING,
              :QLSD-AMT-OF-SURPLUS,
              :QLSD-AMT-OF-DEFICIENCY,
              :QLSD-SEND-FMNO,
              :QLSD-INSURANCE-PROCEEDS,
              :QLSD-DATE-CHECK-RECV,
              :QLSD-INSURANCE-CHECK,
              :QLSD-WHO-ADJUSTED,
              :QLSD-INS-COMPANY,
              :QLSD-DEF-LETTER-SENT-DATE,
              :QLSD-SALES-DATE,
              :QLSD-AUCTION,
              :QLSD-AUCTN-TIME,
              :QLSD-AUCTN-ADR,
              :QLSD-AUCTN-CITY,
              :QLSD-AUCTN-STATE,
              :QLSD-AUCTN-ZIP,
              :QLSD-AUCTN-PHONE,
              :QLSD-REPO-CHARGE,
              :QLSD-STORAGE-CHARGE,
              :QLSD-REPAIR-CHARGE,
              :QLSD-OTHER-CHARGE,
              :QLSD-STORAGE-RATE,
              :QLSD-EST-REBATE,
              :QLSD-MINIMUM-BID,
              :QLSD-MAY-REINSTATE,
              :QLSD-MAY-NOT-REINSTATE,
              :QLSD-IN-PAST-12MONTHS,
              :QLSD-2X-DURING-CONTRACT,
              :QLSD-OTHER,
              :QLSD-OTHER-COMMENTS1,
              :QLSD-OTHER-COMMENTS2,
              :QLSD-CA-PYMTS-AS-AGREED,
              :QLSD-CA-KEEP-INS,
              :QLSD-CA-KEEP-LIEN-FREE,
              :QLSD-CA-OTHER,
              :QLSD-CA-OTHER-DESC,
              :QLSD-CURE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSD1-FILE.

           MOVE LSD-PATH-OWNBR  TO LSD-BRNO.
           ACCEPT LSD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSD-PATH-SQL    TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.
           PERFORM SET-LSD-FIELDS.

           EXEC SQL
            UPDATE DBO.LSDFILE
             SET
              LSDFILE.LSD_BRNO = :QLSD-BRNO,
              LSDFILE.LSD_ACCTNO = :QLSD-ACCTNO,
              LSDFILE.LSD_SEQNO = :QLSD-SEQNO,
              LSDFILE.LSD_LTOUCH_DATE =
                 :QLSD-LTOUCH-DATE,
              LSDFILE.LSD_STOLEN_DATE =
                 :QLSD-STOLEN-DATE,
              LSDFILE.LSD_GAP_INS_PAID = :QLSD-GAP-INS-PAID,
              LSDFILE.LSD_OTHER_EXPENSE = :QLSD-OTHER-EXPENSE,
              LSDFILE.LSD_MISC_AMT_EXPENSE =
                   :QLSD-MISC-AMT-EXPENSE,
              LSDFILE.LSD_MISC_DESC_EXPENSE =
                   :QLSD-MISC-DESC-EXPENSE,
              LSDFILE.LSD_TOT_EXPENSE = :QLSD-TOT-EXPENSE,
              LSDFILE.LSD_INT_REB = :QLSD-INT-REB,
              LSDFILE.LSD_CPP_REB = :QLSD-CPP-REB,
              LSDFILE.LSD_GAP_REB = :QLSD-GAP-REB,
              LSDFILE.LSD_WARRANTY_REB = :QLSD-WARRANTY-REB,
              LSDFILE.LSD_MISC_AMT_CREDIT = :QLSD-MISC-AMT-CREDIT,
              LSDFILE.LSD_MISC_DESC_CREDIT =
                   :QLSD-MISC-DESC-CREDIT,
              LSDFILE.LSD_TOT_CREDITS = :QLSD-TOT-CREDITS,
              LSDFILE.LSD_PAID_TO_OTHER = :QLSD-PAID-TO-OTHER,
              LSDFILE.LSD_AMT_FROM_OWING = :QLSD-AMT-FROM-OWING,
              LSDFILE.LSD_AMT_OF_SURPLUS = :QLSD-AMT-OF-SURPLUS,
              LSDFILE.LSD_AMT_OF_DEFICIENCY =
                   :QLSD-AMT-OF-DEFICIENCY,
              LSDFILE.LSD_SEND_FMNO = :QLSD-SEND-FMNO,
              LSDFILE.LSD_INSURANCE_PROCEEDS =
                   :QLSD-INSURANCE-PROCEEDS,
              LSDFILE.LSD_DATE_CHECK_RECV =
                 :QLSD-DATE-CHECK-RECV,
              LSDFILE.LSD_INSURANCE_CHECK = :QLSD-INSURANCE-CHECK,
              LSDFILE.LSD_WHO_ADJUSTED = :QLSD-WHO-ADJUSTED,
              LSDFILE.LSD_INS_COMPANY = :QLSD-INS-COMPANY,
              LSDFILE.LSD_DEF_LETTER_SENT_DATE =
                 :QLSD-DEF-LETTER-SENT-DATE,
              LSDFILE.LSD_SALES_DATE = :QLSD-SALES-DATE,
              LSDFILE.LSD_AUCTION = :QLSD-AUCTION,
              LSDFILE.LSD_AUCTN_TIME = :QLSD-AUCTN-TIME,
              LSDFILE.LSD_AUCTN_ADR = :QLSD-AUCTN-ADR,
              LSDFILE.LSD_AUCTN_CITY = :QLSD-AUCTN-CITY,
              LSDFILE.LSD_AUCTN_STATE = :QLSD-AUCTN-STATE,
              LSDFILE.LSD_AUCTN_ZIP = :QLSD-AUCTN-ZIP,
              LSDFILE.LSD_AUCTN_PHONE = :QLSD-AUCTN-PHONE,
              LSDFILE.LSD_REPO_CHARGE = :QLSD-REPO-CHARGE,
              LSDFILE.LSD_STORAGE_CHARGE = :QLSD-STORAGE-CHARGE,
              LSDFILE.LSD_REPAIR_CHARGE = :QLSD-REPAIR-CHARGE,
              LSDFILE.LSD_OTHER_CHARGE = :QLSD-OTHER-CHARGE,
              LSDFILE.LSD_STORAGE_RATE = :QLSD-STORAGE-RATE,
              LSDFILE.LSD_EST_REBATE = :QLSD-EST-REBATE,
              LSDFILE.LSD_MINIMUM_BID = :QLSD-MINIMUM-BID,
              LSDFILE.LSD_MAY_REINSTATE = :QLSD-MAY-REINSTATE,
              LSDFILE.LSD_MAY_NOT_REINSTATE =
                   :QLSD-MAY-NOT-REINSTATE,
              LSDFILE.LSD_IN_PAST_12MONTHS =
                   :QLSD-IN-PAST-12MONTHS,
              LSDFILE.LSD_2X_DURING_CONTRACT =
                   :QLSD-2X-DURING-CONTRACT,
              LSDFILE.LSD_OTHER = :QLSD-OTHER,
              LSDFILE.LSD_OTHER_COMMENTS1 = :QLSD-OTHER-COMMENTS1,
              LSDFILE.LSD_OTHER_COMMENTS2 = :QLSD-OTHER-COMMENTS2,
              LSDFILE.LSD_CA_PYMTS_AS_AGREED =
                   :QLSD-CA-PYMTS-AS-AGREED,
              LSDFILE.LSD_CA_KEEP_INS = :QLSD-CA-KEEP-INS,
              LSDFILE.LSD_CA_KEEP_LIEN_FREE =
                   :QLSD-CA-KEEP-LIEN-FREE,
              LSDFILE.LSD_CA_OTHER = :QLSD-CA-OTHER,
              LSDFILE.LSD_CA_OTHER_DESC = :QLSD-CA-OTHER-DESC,
              LSDFILE.LSD_CURE = :QLSD-CURE
             WHERE LSDFILE.LSD_BRNO = :QLSD-BRNO
               AND LSDFILE.LSD_ACCTNO = :QLSD-ACCTNO
               AND LSDFILE.LSD_SEQNO = :QLSD-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSD1-FILE.

           MOVE LSD-PATH-OWNBR  TO LSD-BRNO.
           PERFORM DELETE-IT.
           MOVE LSD-PATH-SQL    TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.
           PERFORM SET-LSD-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSDFILE
             WHERE LSDFILE.LSD_BRNO = :QLSD-BRNO
               AND LSDFILE.LSD_ACCTNO = :QLSD-ACCTNO
               AND LSDFILE.LSD_SEQNO = :QLSD-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSD1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSD1-FILE.

           PERFORM CLOSE-IT.

           IF ( LSD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSD1-CURSOR-STAT.

