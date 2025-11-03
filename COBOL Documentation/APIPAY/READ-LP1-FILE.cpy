      *-----------------------------------------------------------------
       READ-LP1-FILE.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO. 
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           MOVE LP-BRNO   TO QLP-BRNO.
           MOVE LP-ACCTNO TO QLP-ACCTNO.
           MOVE LP-SEQNO  TO QLP-SEQNO.

           EXEC SQL
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_USERID,
                   CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),
                   LPFILE.LP_POSTTIME,
                   CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),
                   CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),
                   LPFILE.LP_TRCD,
                   LPFILE.LP_REFNO,
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   LPFILE.LP_IBPC,
                   LPFILE.LP_MANUAL_RB,
                   LPFILE.LP_INSTYPES,
                   LPFILE.LP_REV,
                   LPFILE.LP_REVTRCD,
                   LPFILE.LP_REVSEQNO,
                   LPFILE.LP_STATUSFG,
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),
                   CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_LCPAID,
                   CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   LPFILE.LP_COLLECTOR,
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   LPFILE.LP_ALT_PREPAY_FG,
                   LPFILE.LP_REPAY_TRANS_ID
            INTO :QLP-REC
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO = :QLP-BRNO
              AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
              AND LPFILE.LP_SEQNO = :QLP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
