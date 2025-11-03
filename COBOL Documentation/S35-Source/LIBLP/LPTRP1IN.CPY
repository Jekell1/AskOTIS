      * COPYMEMBER: LIBLP/LPTRP1IN
      * 20220923 BAH ADDED TRP-LCDUE #1570
      * 20240212 BAH ADDED TRP-REPAY-TRANS-ID #1641
      * 20250116 BAH ADDED TRP-CLOSFEE-REB-AMT #1697 S35Q-157
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
            WHERE (  TRPFILE.TP_BRNO    = :QTRP1-WBEG-BRNO  )
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
              AND (  TRPFILE.TP_DATE   <= :QTRP1-WEND-DATE
              AND    TRPFILE.TP_CLASS  <= :QTRP1-WEND-CLASS
              AND    TRPFILE.TP_ACCTNO <= :QTRP1-WEND-ACCTNO
              AND    TRPFILE.TP_LINE   <= :QTRP1-WEND-LINE  )
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
       WRITE-TRP1-FILE.

           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           ACCEPT TP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           PERFORM SET-TRP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRPFILE
             (TRPFILE.TP_BRNO,
              TRPFILE.TP_DATE,
              TRPFILE.TP_CLASS,
              TRPFILE.TP_ACCTNO,
              TRPFILE.TP_LINE,
              TRPFILE.TP_LTOUCH_DATE,
              TRPFILE.TP_LAWCODE,
              TRPFILE.TP_JDCD,
              TRPFILE.TP_PLCD,
              TRPFILE.TP_SSNO,
              TRPFILE.TP_MAKER,
              TRPFILE.TP_IBPC,
              TRPFILE.TP_TRCD,
              TRPFILE.TP_REFNO,
              TRPFILE.TP_PAYDATE,
              TRPFILE.TP_DUEDATE,
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
              TRPFILE.TP_PDTH_DATE,
              TRPFILE.TP_DLNO,
              TRPFILE.TP_DLPROC,
              TRPFILE.TP_SERVACCT,
              TRPFILE.TP_SALEFIN,
              TRPFILE.TP_INSURED,
              TRPFILE.TP_INSOURS,
              TRPFILE.TP_LOANTYPE,
              TRPFILE.TP_TILLNO,
              TRPFILE.TP_LCPDTH_DATE,
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
              TRPFILE.TP_PODATE,
              TRPFILE.TP_DL_RVCHGS_CODE_1,
              TRPFILE.TP_DL_RVCHGS_CODE_2,
              TRPFILE.TP_DL_RVCHGS_CODE_3,
              TRPFILE.TP_DL_PARVHELD_CODE,
              TRPFILE.TP_DL_PARVPAID_CODE,
              TRPFILE.TP_TOTNODEF,
              TRPFILE.TP_LNAMT,
              TRPFILE.TP_ORIG_1STPYDATE,
              TRPFILE.TP_OLD1ST_PYDATE,
              TRPFILE.TP_1STPYDATE,
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
              TRPFILE.TP_POSTDATE,
              TRPFILE.TP_UNITPER_CD,
              TRPFILE.TP_UNITPER_FREQ,
              TRPFILE.TP_SP_DEFFRMLA,
              TRPFILE.TP_COLLECTOR,
              TRPFILE.TP_PENALTY_INT,
              TRPFILE.TP_INSCOMP,
              TRPFILE.TP_PROMDATE,
              TRPFILE.TP_PROMAMT,
              TRPFILE.TP_PROMID,
              TRPFILE.TP_INS_TAX_RB,
              TRPFILE.TP_PROMDAYS_PASTDUE,
              TRPFILE.TP_MIP_FG,
              TRPFILE.TP_MIP_CLPAID,
              TRPFILE.TP_MIP_CLCOMM,
              TRPFILE.TP_MIP_AHPAID,
              TRPFILE.TP_MIP_AHCOMM,
              TRPFILE.TP_MIP_CLPDTH_DATE,
              TRPFILE.TP_MIP_AHPDTH_DATE,
              TRPFILE.TP_INS_TAX_COUNTY_CD,
              TRPFILE.TP_INS_TAX_CITY_CD,
              TRPFILE.TP_APPLIED_TO_ESCROW,
              TRPFILE.TP_ESCROW_AMT,
              TRPFILE.TP_ESCROW_PREPAID,
              TRPFILE.TP_ESCROW_DISBURSED,
              TRPFILE.TP_LP_SEQNO,
              TRPFILE.TP_PRIOR_DF_CODE,
              TRPFILE.TP_PRIOR_DF_POSTDATE,
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
              TRPFILE.TP_CLOSFEE_REB_AMT)
            VALUES
             (:QTP-BRNO,
              :QTP-DATE,
              :QTP-CLASS,
              :QTP-ACCTNO,
              :QTP-LINE,
              :QTP-LTOUCH-DATE,
              :QTP-LAWCODE,
              :QTP-JDCD,
              :QTP-PLCD,
              :QTP-SSNO,
              :QTP-MAKER,
              :QTP-IBPC,
              :QTP-TRCD,
              :QTP-REFNO,
              :QTP-PAYDATE,
              :QTP-DUEDATE,
              :QTP-TRAMT,
              :QTP-INTDUE,
              :QTP-APOTH,
              :QTP-APOT2,
              :QTP-APINT,
              :QTP-APLC,
              :QTP-APCUR,
              :QTP-GLNO-1,
              :QTP-GLAMT-1,
              :QTP-GLNO-2,
              :QTP-GLAMT-2,
              :QTP-GLNO-3,
              :QTP-GLAMT-3,
              :QTP-EARNINGS-1,
              :QTP-EARNINGS-2,
              :QTP-EARNINGS-3,
              :QTP-EARNINGS-4,
              :QTP-EARNINGS-5,
              :QTP-EARNINGS-6,
              :QTP-EARNINGS-7,
              :QTP-EARNINGS-8,
              :QTP-EARNINGS-9,
              :QTP-EARNINGS-10,
              :QTP-WORKAMT-1,
              :QTP-WORKAMT-2,
              :QTP-WORKAMT-3,
              :QTP-WORKAMT-4,
              :QTP-WORKAMT-5,
              :QTP-WORKAMT-6,
              :QTP-WORKAMT-7,
              :QTP-WORKAMT-8,
              :QTP-WORKAMT-9,
              :QTP-WORKAMT-10,
              :QTP-OTHBAL,
              :QTP-OT2BAL,
              :QTP-INTBAL,
              :QTP-LCBAL,
              :QTP-CURBAL,
              :QTP-PDTH-DATE,
              :QTP-DLNO,
              :QTP-DLPROC,
              :QTP-SERVACCT,
              :QTP-SALEFIN,
              :QTP-INSURED,
              :QTP-INSOURS,
              :QTP-LOANTYPE,
              :QTP-TILLNO,
              :QTP-LCPDTH-DATE,
              :QTP-POSTING-BR,
              :QTP-OTHBR-FG,
              :QTP-REV,
              :QTP-REVTRCD,
              :QTP-REVSEQNO,
              :QTP-STATUSFG,
              :QTP-KEEPAPINT,
              :QTP-INTPAID,
              :QTP-PRIOR-POOLID,
              :QTP-INSTYPES,
              :QTP-INSEARN-1,
              :QTP-INSEARN-2,
              :QTP-INSEARN-3,
              :QTP-INSEARN-4,
              :QTP-INSEARN-5,
              :QTP-INSEARN-6,
              :QTP-INSEARN-7,
              :QTP-INSEARN-8,
              :QTP-INSEARN-9,
              :QTP-INSEARN-10,
              :QTP-INSWORKAMT-1,
              :QTP-INSWORKAMT-2,
              :QTP-INSWORKAMT-3,
              :QTP-INSWORKAMT-4,
              :QTP-INSWORKAMT-5,
              :QTP-INSWORKAMT-6,
              :QTP-INSWORKAMT-7,
              :QTP-INSWORKAMT-8,
              :QTP-INSWORKAMT-9,
              :QTP-INSWORKAMT-10,
              :QTP-DLRVCHGS-ADJ-1,
              :QTP-DLRVCHGS-ADJ-2,
              :QTP-DLRVCHGS-ADJ-3,
              :QTP-DLPARVHELD-ADJ,
              :QTP-DLPARVPAID-ADJ,
              :QTP-NEWSTAT-RATE,
              :QTP-POCD,
              :QTP-PODATE,
              :QTP-DL-RVCHGS-CODE-1,
              :QTP-DL-RVCHGS-CODE-2,
              :QTP-DL-RVCHGS-CODE-3,
              :QTP-DL-PARVHELD-CODE,
              :QTP-DL-PARVPAID-CODE,
              :QTP-TOTNODEF,
              :QTP-LNAMT,
              :QTP-ORIG-1STPYDATE,
              :QTP-OLD1ST-PYDATE,
              :QTP-1STPYDATE,
              :QTP-ORGTERM,
              :QTP-INSCOMM,
              :QTP-MANUAL-RB,
              :QTP-USERID,
              :QTP-POSTTIME,
              :QTP-PLTYPE,
              :QTP-PL-OTH-AR,
              :QTP-LP-DLNO,
              :QTP-REPOCD,
              :QTP-SPRCLASS,
              :QTP-MAX-DEFER-FG,
              :QTP-POSTDATE,
              :QTP-UNITPER-CD,
              :QTP-UNITPER-FREQ,
              :QTP-SP-DEFFRMLA,
              :QTP-COLLECTOR,
              :QTP-PENALTY-INT,
              :QTP-INSCOMP,
              :QTP-PROMDATE,
              :QTP-PROMAMT,
              :QTP-PROMID,
              :QTP-INS-TAX-RB,
              :QTP-PROMDAYS-PASTDUE,
              :QTP-MIP-FG,
              :QTP-MIP-CLPAID,
              :QTP-MIP-CLCOMM,
              :QTP-MIP-AHPAID,
              :QTP-MIP-AHCOMM,
              :QTP-MIP-CLPDTH-DATE,
              :QTP-MIP-AHPDTH-DATE,
              :QTP-INS-TAX-COUNTY-CD,
              :QTP-INS-TAX-CITY-CD,
              :QTP-APPLIED-TO-ESCROW,
              :QTP-ESCROW-AMT,
              :QTP-ESCROW-PREPAID,
              :QTP-ESCROW-DISBURSED,
              :QTP-LP-SEQNO,
              :QTP-PRIOR-DF-CODE,
              :QTP-PRIOR-DF-POSTDATE,
              :QTP-CANCEL-REASON,
              :QTP-FROMCL,
              :QTP-TOCL,
              :QTP-ADDON-TXT,
              :QTP-ADDON-TYP,
              :QTP-DLRVNUM,
              :QTP-P4P5-TRANCD,
              :QTP-P4P5-DLRVNUM,
              :QTP-POOLID,
              :QTP-ACTIONCD2,
              :QTP-LCDUE,
              :QTP-REPAY-TRANS-ID,
              :QTP-CLOSFEE-REB-AMT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRP1-FILE.

           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           ACCEPT TP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           PERFORM SET-TRP-FIELDS.

           EXEC SQL
            UPDATE DBO.TRPFILE
             SET
              TRPFILE.TP_BRNO = :QTP-BRNO,
              TRPFILE.TP_DATE = :QTP-DATE,
              TRPFILE.TP_CLASS = :QTP-CLASS,
              TRPFILE.TP_ACCTNO = :QTP-ACCTNO,
              TRPFILE.TP_LINE = :QTP-LINE,
              TRPFILE.TP_LTOUCH_DATE = :QTP-LTOUCH-DATE,
              TRPFILE.TP_LAWCODE = :QTP-LAWCODE,
              TRPFILE.TP_JDCD = :QTP-JDCD,
              TRPFILE.TP_PLCD = :QTP-PLCD,
              TRPFILE.TP_SSNO = :QTP-SSNO,
              TRPFILE.TP_MAKER = :QTP-MAKER,
              TRPFILE.TP_IBPC = :QTP-IBPC,
              TRPFILE.TP_TRCD = :QTP-TRCD,
              TRPFILE.TP_REFNO = :QTP-REFNO,
              TRPFILE.TP_PAYDATE = :QTP-PAYDATE,
              TRPFILE.TP_DUEDATE = :QTP-DUEDATE,
              TRPFILE.TP_TRAMT = :QTP-TRAMT,
              TRPFILE.TP_INTDUE = :QTP-INTDUE,
              TRPFILE.TP_APOTH = :QTP-APOTH,
              TRPFILE.TP_APOT2 = :QTP-APOT2,
              TRPFILE.TP_APINT = :QTP-APINT,
              TRPFILE.TP_APLC = :QTP-APLC,
              TRPFILE.TP_APCUR = :QTP-APCUR,
              TRPFILE.TP_GLNO_1 = :QTP-GLNO-1,
              TRPFILE.TP_GLAMT_1 = :QTP-GLAMT-1,
              TRPFILE.TP_GLNO_2 = :QTP-GLNO-2,
              TRPFILE.TP_GLAMT_2 = :QTP-GLAMT-2,
              TRPFILE.TP_GLNO_3 = :QTP-GLNO-3,
              TRPFILE.TP_GLAMT_3 = :QTP-GLAMT-3,
              TRPFILE.TP_EARNINGS_1 = :QTP-EARNINGS-1,
              TRPFILE.TP_EARNINGS_2 = :QTP-EARNINGS-2,
              TRPFILE.TP_EARNINGS_3 = :QTP-EARNINGS-3,
              TRPFILE.TP_EARNINGS_4 = :QTP-EARNINGS-4,
              TRPFILE.TP_EARNINGS_5 = :QTP-EARNINGS-5,
              TRPFILE.TP_EARNINGS_6 = :QTP-EARNINGS-6,
              TRPFILE.TP_EARNINGS_7 = :QTP-EARNINGS-7,
              TRPFILE.TP_EARNINGS_8 = :QTP-EARNINGS-8,
              TRPFILE.TP_EARNINGS_9 = :QTP-EARNINGS-9,
              TRPFILE.TP_EARNINGS_10 = :QTP-EARNINGS-10,
              TRPFILE.TP_WORKAMT_1 = :QTP-WORKAMT-1,
              TRPFILE.TP_WORKAMT_2 = :QTP-WORKAMT-2,
              TRPFILE.TP_WORKAMT_3 = :QTP-WORKAMT-3,
              TRPFILE.TP_WORKAMT_4 = :QTP-WORKAMT-4,
              TRPFILE.TP_WORKAMT_5 = :QTP-WORKAMT-5,
              TRPFILE.TP_WORKAMT_6 = :QTP-WORKAMT-6,
              TRPFILE.TP_WORKAMT_7 = :QTP-WORKAMT-7,
              TRPFILE.TP_WORKAMT_8 = :QTP-WORKAMT-8,
              TRPFILE.TP_WORKAMT_9 = :QTP-WORKAMT-9,
              TRPFILE.TP_WORKAMT_10 = :QTP-WORKAMT-10,
              TRPFILE.TP_OTHBAL = :QTP-OTHBAL,
              TRPFILE.TP_OT2BAL = :QTP-OT2BAL,
              TRPFILE.TP_INTBAL = :QTP-INTBAL,
              TRPFILE.TP_LCBAL = :QTP-LCBAL,
              TRPFILE.TP_CURBAL = :QTP-CURBAL,
              TRPFILE.TP_PDTH_DATE = :QTP-PDTH-DATE,
              TRPFILE.TP_DLNO = :QTP-DLNO,
              TRPFILE.TP_DLPROC = :QTP-DLPROC,
              TRPFILE.TP_SERVACCT = :QTP-SERVACCT,
              TRPFILE.TP_SALEFIN = :QTP-SALEFIN,
              TRPFILE.TP_INSURED = :QTP-INSURED,
              TRPFILE.TP_INSOURS = :QTP-INSOURS,
              TRPFILE.TP_LOANTYPE = :QTP-LOANTYPE,
              TRPFILE.TP_TILLNO = :QTP-TILLNO,
              TRPFILE.TP_LCPDTH_DATE = :QTP-LCPDTH-DATE,
              TRPFILE.TP_POSTING_BR = :QTP-POSTING-BR,
              TRPFILE.TP_OTHBR_FG = :QTP-OTHBR-FG,
              TRPFILE.TP_REV = :QTP-REV,
              TRPFILE.TP_REVTRCD = :QTP-REVTRCD,
              TRPFILE.TP_REVSEQNO = :QTP-REVSEQNO,
              TRPFILE.TP_STATUSFG = :QTP-STATUSFG,
              TRPFILE.TP_KEEPAPINT = :QTP-KEEPAPINT,
              TRPFILE.TP_INTPAID = :QTP-INTPAID,
              TRPFILE.TP_PRIOR_POOLID = :QTP-PRIOR-POOLID,
              TRPFILE.TP_INSTYPES = :QTP-INSTYPES,
              TRPFILE.TP_INSEARN_1 = :QTP-INSEARN-1,
              TRPFILE.TP_INSEARN_2 = :QTP-INSEARN-2,
              TRPFILE.TP_INSEARN_3 = :QTP-INSEARN-3,
              TRPFILE.TP_INSEARN_4 = :QTP-INSEARN-4,
              TRPFILE.TP_INSEARN_5 = :QTP-INSEARN-5,
              TRPFILE.TP_INSEARN_6 = :QTP-INSEARN-6,
              TRPFILE.TP_INSEARN_7 = :QTP-INSEARN-7,
              TRPFILE.TP_INSEARN_8 = :QTP-INSEARN-8,
              TRPFILE.TP_INSEARN_9 = :QTP-INSEARN-9,
              TRPFILE.TP_INSEARN_10 = :QTP-INSEARN-10,
              TRPFILE.TP_INSWORKAMT_1 = :QTP-INSWORKAMT-1,
              TRPFILE.TP_INSWORKAMT_2 = :QTP-INSWORKAMT-2,
              TRPFILE.TP_INSWORKAMT_3 = :QTP-INSWORKAMT-3,
              TRPFILE.TP_INSWORKAMT_4 = :QTP-INSWORKAMT-4,
              TRPFILE.TP_INSWORKAMT_5 = :QTP-INSWORKAMT-5,
              TRPFILE.TP_INSWORKAMT_6 = :QTP-INSWORKAMT-6,
              TRPFILE.TP_INSWORKAMT_7 = :QTP-INSWORKAMT-7,
              TRPFILE.TP_INSWORKAMT_8 = :QTP-INSWORKAMT-8,
              TRPFILE.TP_INSWORKAMT_9 = :QTP-INSWORKAMT-9,
              TRPFILE.TP_INSWORKAMT_10 = :QTP-INSWORKAMT-10,
              TRPFILE.TP_DLRVCHGS_ADJ_1 = :QTP-DLRVCHGS-ADJ-1,
              TRPFILE.TP_DLRVCHGS_ADJ_2 = :QTP-DLRVCHGS-ADJ-2,
              TRPFILE.TP_DLRVCHGS_ADJ_3 = :QTP-DLRVCHGS-ADJ-3,
              TRPFILE.TP_DLPARVHELD_ADJ = :QTP-DLPARVHELD-ADJ,
              TRPFILE.TP_DLPARVPAID_ADJ = :QTP-DLPARVPAID-ADJ,
              TRPFILE.TP_NEWSTAT_RATE = :QTP-NEWSTAT-RATE,
              TRPFILE.TP_POCD = :QTP-POCD,
              TRPFILE.TP_PODATE = :QTP-PODATE,
              TRPFILE.TP_DL_RVCHGS_CODE_1 = :QTP-DL-RVCHGS-CODE-1,
              TRPFILE.TP_DL_RVCHGS_CODE_2 = :QTP-DL-RVCHGS-CODE-2,
              TRPFILE.TP_DL_RVCHGS_CODE_3 = :QTP-DL-RVCHGS-CODE-3,
              TRPFILE.TP_DL_PARVHELD_CODE = :QTP-DL-PARVHELD-CODE,
              TRPFILE.TP_DL_PARVPAID_CODE = :QTP-DL-PARVPAID-CODE,
              TRPFILE.TP_TOTNODEF = :QTP-TOTNODEF,
              TRPFILE.TP_LNAMT = :QTP-LNAMT,
              TRPFILE.TP_ORIG_1STPYDATE = :QTP-ORIG-1STPYDATE,
              TRPFILE.TP_OLD1ST_PYDATE = :QTP-OLD1ST-PYDATE,
              TRPFILE.TP_1STPYDATE = :QTP-1STPYDATE,
              TRPFILE.TP_ORGTERM = :QTP-ORGTERM,
              TRPFILE.TP_INSCOMM = :QTP-INSCOMM,
              TRPFILE.TP_MANUAL_RB = :QTP-MANUAL-RB,
              TRPFILE.TP_USERID = :QTP-USERID,
              TRPFILE.TP_POSTTIME = :QTP-POSTTIME,
              TRPFILE.TP_PLTYPE = :QTP-PLTYPE,
              TRPFILE.TP_PL_OTH_AR = :QTP-PL-OTH-AR,
              TRPFILE.TP_LP_DLNO = :QTP-LP-DLNO,
              TRPFILE.TP_REPOCD = :QTP-REPOCD,
              TRPFILE.TP_SPRCLASS = :QTP-SPRCLASS,
              TRPFILE.TP_MAX_DEFER_FG = :QTP-MAX-DEFER-FG,
              TRPFILE.TP_POSTDATE = :QTP-POSTDATE,
              TRPFILE.TP_UNITPER_CD = :QTP-UNITPER-CD,
              TRPFILE.TP_UNITPER_FREQ = :QTP-UNITPER-FREQ,
              TRPFILE.TP_SP_DEFFRMLA = :QTP-SP-DEFFRMLA,
              TRPFILE.TP_COLLECTOR = :QTP-COLLECTOR,
              TRPFILE.TP_PENALTY_INT = :QTP-PENALTY-INT,
              TRPFILE.TP_INSCOMP = :QTP-INSCOMP,
              TRPFILE.TP_PROMDATE = :QTP-PROMDATE,
              TRPFILE.TP_PROMAMT = :QTP-PROMAMT,
              TRPFILE.TP_PROMID = :QTP-PROMID,
              TRPFILE.TP_INS_TAX_RB = :QTP-INS-TAX-RB,
              TRPFILE.TP_PROMDAYS_PASTDUE = :QTP-PROMDAYS-PASTDUE,
              TRPFILE.TP_MIP_FG = :QTP-MIP-FG,
              TRPFILE.TP_MIP_CLPAID = :QTP-MIP-CLPAID,
              TRPFILE.TP_MIP_CLCOMM = :QTP-MIP-CLCOMM,
              TRPFILE.TP_MIP_AHPAID = :QTP-MIP-AHPAID,
              TRPFILE.TP_MIP_AHCOMM = :QTP-MIP-AHCOMM,
              TRPFILE.TP_MIP_CLPDTH_DATE = :QTP-MIP-CLPDTH-DATE,
              TRPFILE.TP_MIP_AHPDTH_DATE = :QTP-MIP-AHPDTH-DATE,
              TRPFILE.TP_INS_TAX_COUNTY_CD =
                   :QTP-INS-TAX-COUNTY-CD,
              TRPFILE.TP_INS_TAX_CITY_CD = :QTP-INS-TAX-CITY-CD,
              TRPFILE.TP_APPLIED_TO_ESCROW =
                   :QTP-APPLIED-TO-ESCROW,
              TRPFILE.TP_ESCROW_AMT = :QTP-ESCROW-AMT,
              TRPFILE.TP_ESCROW_PREPAID = :QTP-ESCROW-PREPAID,
              TRPFILE.TP_ESCROW_DISBURSED = :QTP-ESCROW-DISBURSED,
              TRPFILE.TP_LP_SEQNO = :QTP-LP-SEQNO,
              TRPFILE.TP_PRIOR_DF_CODE = :QTP-PRIOR-DF-CODE,
              TRPFILE.TP_PRIOR_DF_POSTDATE = :QTP-PRIOR-DF-POSTDATE,
              TRPFILE.TP_CANCEL_REASON = :QTP-CANCEL-REASON,
              TRPFILE.TP_FROMCL = :QTP-FROMCL,
              TRPFILE.TP_TOCL = :QTP-TOCL,
              TRPFILE.TP_ADDON_TXT = :QTP-ADDON-TXT,
              TRPFILE.TP_ADDON_TYP = :QTP-ADDON-TYP,
              TRPFILE.TP_DLRVNUM = :QTP-DLRVNUM,
              TRPFILE.TP_P4P5_TRANCD = :QTP-P4P5-TRANCD,
              TRPFILE.TP_P4P5_DLRVNUM = :QTP-P4P5-DLRVNUM,
              TRPFILE.TP_POOLID = :QTP-POOLID,
              TRPFILE.TP_ACTIONCD2 = :QTP-ACTIONCD2,
              TRPFILE.TP_LCDUE = :QTP-LCDUE,
              TRPFILE.TP_REPAY_TRANS_ID = :QTP-REPAY-TRANS-ID,
              TRPFILE.TP_CLOSFEE_REB_AMT = :QTP-CLOSFEE-REB-AMT
            WHERE TRPFILE.TP_BRNO = :QTP-BRNO
              AND TRPFILE.TP_DATE = :QTP-DATE
              AND TRPFILE.TP_CLASS = :QTP-CLASS
              AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
              AND TRPFILE.TP_LINE = :QTP-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRP1-FILE.

           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           PERFORM DELETE-IT.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           PERFORM SET-TRP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRPFILE
            WHERE TRPFILE.TP_BRNO = :QTP-BRNO
              AND TRPFILE.TP_DATE = :QTP-DATE
              AND TRPFILE.TP_CLASS = :QTP-CLASS
              AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
              AND TRPFILE.TP_LINE = :QTP-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.

