      * COPYMEMBER: LIBLP/LPTRP1RN
      * 20220923 BAH ADDED TP-LCDUE *1570
      * 20240212 BAH ADDED TP-REPAY-TRANS-ID #1641
      * 20250116 BAH ADDED TP-CLOSFEE-REB-AMT #1697 S35Q-157
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRP1-FILE.
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE 99991231         TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE
              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE TP-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE

              IF ( TP-CLASS = 999 )
                 MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1
              END-IF

              IF ( TP-ACCTNO = 99999999 )
                 MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1
              END-IF

              IF ( TP-LINE = 999 )
                 MOVE TP-LINE           TO QTRP1-WSBEG-LINE
              ELSE
                 COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.


           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
            SELECT TRPFILE.TP_BRNO,
                   CAST(TRPFILE.TP_DATE AS VARCHAR(10)),
                   TRPFILE.TP_CLASS,
                   TRPFILE.TP_ACCTNO,
                   TRPFILE.TP_LINE,
                   CAST(TRPFILE.TP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_LAWCODE,
                   TRPFILE.TP_JDCD,
                   TRPFILE.TP_PLCD,
                   TRPFILE.TP_SSNO,
                   TRPFILE.TP_MAKER,
                   TRPFILE.TP_IBPC,
                   TRPFILE.TP_TRCD,
                   TRPFILE.TP_REFNO,
                   CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),
                   CAST(TRPFILE.TP_DUEDATE AS VARCHAR(10)),
                   TRPFILE.TP_TRAMT,
                   TRPFILE.TP_INTDUE,
                   TRPFILE.TP_APOTH,
                   TRPFILE.TP_APOT2,
                   TRPFILE.TP_APINT,
                   TRPFILE.TP_APLC,
                   TRPFILE.TP_APCUR,
                   TRPFILE.TP_GLNO_1,
                   TRPFILE.TP_GLAMT_1,
                   TRPFILE.TP_GLNO_2,
                   TRPFILE.TP_GLAMT_2,
                   TRPFILE.TP_GLNO_3,
                   TRPFILE.TP_GLAMT_3,
                   TRPFILE.TP_EARNINGS_1,
                   TRPFILE.TP_EARNINGS_2,
                   TRPFILE.TP_EARNINGS_3,
                   TRPFILE.TP_EARNINGS_4,
                   TRPFILE.TP_EARNINGS_5,
                   TRPFILE.TP_EARNINGS_6,
                   TRPFILE.TP_EARNINGS_7,
                   TRPFILE.TP_EARNINGS_8,
                   TRPFILE.TP_EARNINGS_9,
                   TRPFILE.TP_EARNINGS_10,
                   TRPFILE.TP_WORKAMT_1,
                   TRPFILE.TP_WORKAMT_2,
                   TRPFILE.TP_WORKAMT_3,
                   TRPFILE.TP_WORKAMT_4,
                   TRPFILE.TP_WORKAMT_5,
                   TRPFILE.TP_WORKAMT_6,
                   TRPFILE.TP_WORKAMT_7,
                   TRPFILE.TP_WORKAMT_8,
                   TRPFILE.TP_WORKAMT_9,
                   TRPFILE.TP_WORKAMT_10,
                   TRPFILE.TP_OTHBAL,
                   TRPFILE.TP_OT2BAL,
                   TRPFILE.TP_INTBAL,
                   TRPFILE.TP_LCBAL,
                   TRPFILE.TP_CURBAL,
                   CAST(TRPFILE.TP_PDTH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_DLNO,
                   TRPFILE.TP_DLPROC,
                   TRPFILE.TP_SERVACCT,
                   TRPFILE.TP_SALEFIN,
                   TRPFILE.TP_INSURED,
                   TRPFILE.TP_INSOURS,
                   TRPFILE.TP_LOANTYPE,
                   TRPFILE.TP_TILLNO,
                   CAST(TRPFILE.TP_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_POSTING_BR,
                   TRPFILE.TP_OTHBR_FG,
                   TRPFILE.TP_REV,
                   TRPFILE.TP_REVTRCD,
                   TRPFILE.TP_REVSEQNO,
                   TRPFILE.TP_STATUSFG,
                   TRPFILE.TP_KEEPAPINT,
                   TRPFILE.TP_INTPAID,
                   TRPFILE.TP_PRIOR_POOLID,
                   TRPFILE.TP_INSTYPES,
                   TRPFILE.TP_INSEARN_1,
                   TRPFILE.TP_INSEARN_2,
                   TRPFILE.TP_INSEARN_3,
                   TRPFILE.TP_INSEARN_4,
                   TRPFILE.TP_INSEARN_5,
                   TRPFILE.TP_INSEARN_6,
                   TRPFILE.TP_INSEARN_7,
                   TRPFILE.TP_INSEARN_8,
                   TRPFILE.TP_INSEARN_9,
                   TRPFILE.TP_INSEARN_10,
                   TRPFILE.TP_INSWORKAMT_1,
                   TRPFILE.TP_INSWORKAMT_2,
                   TRPFILE.TP_INSWORKAMT_3,
                   TRPFILE.TP_INSWORKAMT_4,
                   TRPFILE.TP_INSWORKAMT_5,
                   TRPFILE.TP_INSWORKAMT_6,
                   TRPFILE.TP_INSWORKAMT_7,
                   TRPFILE.TP_INSWORKAMT_8,
                   TRPFILE.TP_INSWORKAMT_9,
                   TRPFILE.TP_INSWORKAMT_10,
                   TRPFILE.TP_DLRVCHGS_ADJ_1,
                   TRPFILE.TP_DLRVCHGS_ADJ_2,
                   TRPFILE.TP_DLRVCHGS_ADJ_3,
                   TRPFILE.TP_DLPARVHELD_ADJ,
                   TRPFILE.TP_DLPARVPAID_ADJ,
                   TRPFILE.TP_NEWSTAT_RATE,
                   TRPFILE.TP_POCD,
                   CAST(TRPFILE.TP_PODATE AS VARCHAR(10)),
                   TRPFILE.TP_DL_RVCHGS_CODE_1,
                   TRPFILE.TP_DL_RVCHGS_CODE_2,
                   TRPFILE.TP_DL_RVCHGS_CODE_3,
                   TRPFILE.TP_DL_PARVHELD_CODE,
                   TRPFILE.TP_DL_PARVPAID_CODE,
                   TRPFILE.TP_TOTNODEF,
                   TRPFILE.TP_LNAMT,
                   CAST(TRPFILE.TP_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),
                   CAST(TRPFILE.TP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),
                   CAST(TRPFILE.TP_1STPYDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_ORGTERM,
                   TRPFILE.TP_INSCOMM,
                   TRPFILE.TP_MANUAL_RB,
                   TRPFILE.TP_USERID,
                   TRPFILE.TP_POSTTIME,
                   TRPFILE.TP_PLTYPE,
                   TRPFILE.TP_PL_OTH_AR,
                   TRPFILE.TP_LP_DLNO,
                   TRPFILE.TP_REPOCD,
                   TRPFILE.TP_SPRCLASS,
                   TRPFILE.TP_MAX_DEFER_FG,
                   CAST(TRPFILE.TP_POSTDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_UNITPER_CD,
                   TRPFILE.TP_UNITPER_FREQ,
                   TRPFILE.TP_SP_DEFFRMLA,
                   TRPFILE.TP_COLLECTOR,
                   TRPFILE.TP_PENALTY_INT,
                   TRPFILE.TP_INSCOMP,
                   CAST(TRPFILE.TP_PROMDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_PROMAMT,
                   TRPFILE.TP_PROMID,
                   TRPFILE.TP_INS_TAX_RB,
                   TRPFILE.TP_PROMDAYS_PASTDUE,
                   TRPFILE.TP_MIP_FG,
                   TRPFILE.TP_MIP_CLPAID,
                   TRPFILE.TP_MIP_CLCOMM,
                   TRPFILE.TP_MIP_AHPAID,
                   TRPFILE.TP_MIP_AHCOMM,
                   CAST(TRPFILE.TP_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(TRPFILE.TP_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_INS_TAX_COUNTY_CD,
                   TRPFILE.TP_INS_TAX_CITY_CD,
                   TRPFILE.TP_APPLIED_TO_ESCROW,
                   TRPFILE.TP_ESCROW_AMT,
                   TRPFILE.TP_ESCROW_PREPAID,
                   TRPFILE.TP_ESCROW_DISBURSED,
                   TRPFILE.TP_LP_SEQNO,
                   TRPFILE.TP_PRIOR_DF_CODE,
                   CAST(TRPFILE.TP_PRIOR_DF_POSTDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_CANCEL_REASON,
                   TRPFILE.TP_FROMCL,
                   TRPFILE.TP_TOCL,
                   TRPFILE.TP_ADDON_TXT,
                   TRPFILE.TP_ADDON_TYP,
                   TRPFILE.TP_DLRVNUM,
                   TRPFILE.TP_P4P5_TRANCD,
                   TRPFILE.TP_P4P5_DLRVNUM,
                   TRPFILE.TP_POOLID,
                   TRPFILE.TP_ACTIONCD2,
                   TRPFILE.TP_LCDUE,
                   TRPFILE.TP_REPAY_TRANS_ID,
                   TRPFILE.TP_CLOSFEE_REB_AMT
            FROM DBO.TRPFILE
            WHERE  ( TRPFILE.TP_BRNO    = :QTRP1-WBEG-BRNO )
              AND (( TRPFILE.TP_DATE   >= :QTRP1-WBEG-DATE 
              AND    TRPFILE.TP_CLASS  >= :QTRP1-WBEG-CLASS
              AND    TRPFILE.TP_ACCTNO >= :QTRP1-WBEG-ACCTNO
              AND    TRPFILE.TP_LINE   >= :QTRP1-WBEG-LINE )
               OR  ( TRPFILE.TP_DATE   >= :QTRP1-WBEG-DATE 
              AND    TRPFILE.TP_CLASS  >= :QTRP1-WBEG-CLASS
              AND    TRPFILE.TP_ACCTNO >= :QTRP1-WSBEG-ACCTNO
              AND    TRPFILE.TP_LINE   >= 0                )
               OR  ( TRPFILE.TP_DATE   >= :QTRP1-WBEG-DATE
              AND    TRPFILE.TP_CLASS  >= :QTRP1-WSBEG-CLASS
              AND    TRPFILE.TP_ACCTNO >= 0
              AND    TRPFILE.TP_LINE   >= 0                )
               OR  ( TRPFILE.TP_DATE   >= :QTRP1-WSBEG-DATE 
              AND    TRPFILE.TP_CLASS  >= 0
              AND    TRPFILE.TP_ACCTNO >= 0
              AND    TRPFILE.TP_LINE   >= 0                ))
              AND ( TRPFILE.TP_DATE   <= :QTRP1-WEND-DATE 
              AND   TRPFILE.TP_CLASS  <= :QTRP1-WEND-CLASS
              AND   TRPFILE.TP_ACCTNO <= :QTRP1-WEND-ACCTNO
              AND   TRPFILE.TP_LINE   <= :QTRP1-WEND-LINE )
            ORDER BY TRPFILE.TP_BRNO,
                     TRPFILE.TP_DATE,
                     TRPFILE.TP_CLASS,
                     TRPFILE.TP_ACCTNO,
                     TRPFILE.TP_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE
                          QTRP1-WSBEG-CLASS
                          QTRP1-WSBEG-ACCTNO
                          QTRP1-WSBEG-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRP1-FILE.
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO. 
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.

           MOVE TP-BRNO             TO QTP-BRNO.
           MOVE TP-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTP-DATE.
           MOVE TP-CLASS            TO QTP-CLASS.
           MOVE TP-ACCTNO           TO QTP-ACCTNO.
           MOVE TP-LINE             TO QTP-LINE.

           EXEC SQL
            SELECT TRPFILE.TP_BRNO,
                   CAST(TRPFILE.TP_DATE AS VARCHAR(10)),
                   TRPFILE.TP_CLASS,
                   TRPFILE.TP_ACCTNO,
                   TRPFILE.TP_LINE,
                   CAST(TRPFILE.TP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_LAWCODE,
                   TRPFILE.TP_JDCD,
                   TRPFILE.TP_PLCD,
                   TRPFILE.TP_SSNO,
                   TRPFILE.TP_MAKER,
                   TRPFILE.TP_IBPC,
                   TRPFILE.TP_TRCD,
                   TRPFILE.TP_REFNO,
                   CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),
                   CAST(TRPFILE.TP_DUEDATE AS VARCHAR(10)),
                   TRPFILE.TP_TRAMT,
                   TRPFILE.TP_INTDUE,
                   TRPFILE.TP_APOTH,
                   TRPFILE.TP_APOT2,
                   TRPFILE.TP_APINT,
                   TRPFILE.TP_APLC,
                   TRPFILE.TP_APCUR,
                   TRPFILE.TP_GLNO_1,
                   TRPFILE.TP_GLAMT_1,
                   TRPFILE.TP_GLNO_2,
                   TRPFILE.TP_GLAMT_2,
                   TRPFILE.TP_GLNO_3,
                   TRPFILE.TP_GLAMT_3,
                   TRPFILE.TP_EARNINGS_1,
                   TRPFILE.TP_EARNINGS_2,
                   TRPFILE.TP_EARNINGS_3,
                   TRPFILE.TP_EARNINGS_4,
                   TRPFILE.TP_EARNINGS_5,
                   TRPFILE.TP_EARNINGS_6,
                   TRPFILE.TP_EARNINGS_7,
                   TRPFILE.TP_EARNINGS_8,
                   TRPFILE.TP_EARNINGS_9,
                   TRPFILE.TP_EARNINGS_10,
                   TRPFILE.TP_WORKAMT_1,
                   TRPFILE.TP_WORKAMT_2,
                   TRPFILE.TP_WORKAMT_3,
                   TRPFILE.TP_WORKAMT_4,
                   TRPFILE.TP_WORKAMT_5,
                   TRPFILE.TP_WORKAMT_6,
                   TRPFILE.TP_WORKAMT_7,
                   TRPFILE.TP_WORKAMT_8,
                   TRPFILE.TP_WORKAMT_9,
                   TRPFILE.TP_WORKAMT_10,
                   TRPFILE.TP_OTHBAL,
                   TRPFILE.TP_OT2BAL,
                   TRPFILE.TP_INTBAL,
                   TRPFILE.TP_LCBAL,
                   TRPFILE.TP_CURBAL,
                   CAST(TRPFILE.TP_PDTH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_DLNO,
                   TRPFILE.TP_DLPROC,
                   TRPFILE.TP_SERVACCT,
                   TRPFILE.TP_SALEFIN,
                   TRPFILE.TP_INSURED,
                   TRPFILE.TP_INSOURS,
                   TRPFILE.TP_LOANTYPE,
                   TRPFILE.TP_TILLNO,
                   CAST(TRPFILE.TP_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_POSTING_BR,
                   TRPFILE.TP_OTHBR_FG,
                   TRPFILE.TP_REV,
                   TRPFILE.TP_REVTRCD,
                   TRPFILE.TP_REVSEQNO,
                   TRPFILE.TP_STATUSFG,
                   TRPFILE.TP_KEEPAPINT,
                   TRPFILE.TP_INTPAID,
                   TRPFILE.TP_PRIOR_POOLID,
                   TRPFILE.TP_INSTYPES,
                   TRPFILE.TP_INSEARN_1,
                   TRPFILE.TP_INSEARN_2,
                   TRPFILE.TP_INSEARN_3,
                   TRPFILE.TP_INSEARN_4,
                   TRPFILE.TP_INSEARN_5,
                   TRPFILE.TP_INSEARN_6,
                   TRPFILE.TP_INSEARN_7,
                   TRPFILE.TP_INSEARN_8,
                   TRPFILE.TP_INSEARN_9,
                   TRPFILE.TP_INSEARN_10,
                   TRPFILE.TP_INSWORKAMT_1,
                   TRPFILE.TP_INSWORKAMT_2,
                   TRPFILE.TP_INSWORKAMT_3,
                   TRPFILE.TP_INSWORKAMT_4,
                   TRPFILE.TP_INSWORKAMT_5,
                   TRPFILE.TP_INSWORKAMT_6,
                   TRPFILE.TP_INSWORKAMT_7,
                   TRPFILE.TP_INSWORKAMT_8,
                   TRPFILE.TP_INSWORKAMT_9,
                   TRPFILE.TP_INSWORKAMT_10,
                   TRPFILE.TP_DLRVCHGS_ADJ_1,
                   TRPFILE.TP_DLRVCHGS_ADJ_2,
                   TRPFILE.TP_DLRVCHGS_ADJ_3,
                   TRPFILE.TP_DLPARVHELD_ADJ,
                   TRPFILE.TP_DLPARVPAID_ADJ,
                   TRPFILE.TP_NEWSTAT_RATE,
                   TRPFILE.TP_POCD,
                   CAST(TRPFILE.TP_PODATE AS VARCHAR(10)),
                   TRPFILE.TP_DL_RVCHGS_CODE_1,
                   TRPFILE.TP_DL_RVCHGS_CODE_2,
                   TRPFILE.TP_DL_RVCHGS_CODE_3,
                   TRPFILE.TP_DL_PARVHELD_CODE,
                   TRPFILE.TP_DL_PARVPAID_CODE,
                   TRPFILE.TP_TOTNODEF,
                   TRPFILE.TP_LNAMT,
                   CAST(TRPFILE.TP_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),
                   CAST(TRPFILE.TP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),
                   CAST(TRPFILE.TP_1STPYDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_ORGTERM,
                   TRPFILE.TP_INSCOMM,
                   TRPFILE.TP_MANUAL_RB,
                   TRPFILE.TP_USERID,
                   TRPFILE.TP_POSTTIME,
                   TRPFILE.TP_PLTYPE,
                   TRPFILE.TP_PL_OTH_AR,
                   TRPFILE.TP_LP_DLNO,
                   TRPFILE.TP_REPOCD,
                   TRPFILE.TP_SPRCLASS,
                   TRPFILE.TP_MAX_DEFER_FG,
                   CAST(TRPFILE.TP_POSTDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_UNITPER_CD,
                   TRPFILE.TP_UNITPER_FREQ,
                   TRPFILE.TP_SP_DEFFRMLA,
                   TRPFILE.TP_COLLECTOR,
                   TRPFILE.TP_PENALTY_INT,
                   TRPFILE.TP_INSCOMP,
                   CAST(TRPFILE.TP_PROMDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_PROMAMT,
                   TRPFILE.TP_PROMID,
                   TRPFILE.TP_INS_TAX_RB,
                   TRPFILE.TP_PROMDAYS_PASTDUE,
                   TRPFILE.TP_MIP_FG,
                   TRPFILE.TP_MIP_CLPAID,
                   TRPFILE.TP_MIP_CLCOMM,
                   TRPFILE.TP_MIP_AHPAID,
                   TRPFILE.TP_MIP_AHCOMM,
                   CAST(TRPFILE.TP_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(TRPFILE.TP_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_INS_TAX_COUNTY_CD,
                   TRPFILE.TP_INS_TAX_CITY_CD,
                   TRPFILE.TP_APPLIED_TO_ESCROW,
                   TRPFILE.TP_ESCROW_AMT,
                   TRPFILE.TP_ESCROW_PREPAID,
                   TRPFILE.TP_ESCROW_DISBURSED,
                   TRPFILE.TP_LP_SEQNO,
                   TRPFILE.TP_PRIOR_DF_CODE,
                   CAST(TRPFILE.TP_PRIOR_DF_POSTDATE
                                                  AS VARCHAR(10)),
                   TRPFILE.TP_CANCEL_REASON,
                   TRPFILE.TP_FROMCL,
                   TRPFILE.TP_TOCL,
                   TRPFILE.TP_ADDON_TXT,
                   TRPFILE.TP_ADDON_TYP,
                   TRPFILE.TP_DLRVNUM,
                   TRPFILE.TP_P4P5_TRANCD,
                   TRPFILE.TP_P4P5_DLRVNUM,
                   TRPFILE.TP_POOLID,
                   TRPFILE.TP_ACTIONCD2,
                   TRPFILE.TP_LCDUE,
                   TRPFILE.TP_REPAY_TRANS_ID,
                   TRPFILE.TP_CLOSFEE_REB_AMT
            INTO :QTRP-REC
            FROM DBO.TRPFILE
            WHERE TRPFILE.TP_BRNO = :QTP-BRNO
              AND TRPFILE.TP_DATE = :QTP-DATE
              AND TRPFILE.TP_CLASS = :QTP-CLASS
              AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
              AND TRPFILE.TP_LINE = :QTP-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRP1_CURSOR INTO :QTRP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.

