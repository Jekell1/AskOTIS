      * COPYMEMBER: LIBLP/LPMP1IN
       LOAD-MP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( MP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( MP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO MP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO MP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO MP-ALL-DATA
              MOVE MP-ALL-PATH             TO MP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( MP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO MP-ALL-BASE
              MOVE FILPATH-MACHINE         TO MP-ALL-MACHINE
              MOVE FILPATH-DATA            TO MP-ALL-DATA
              MOVE MP-ALL-PATH             TO MP-PATH.

      *-----------------------------------------------------------------
       OPEN-MP1-FILE.
           PERFORM LOAD-MP1-FILE.
           PERFORM OPEN-IT.
           MOVE MP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-MP1-FILE.
           PERFORM START-IT.
           MOVE MP-PATH-OWNBR  TO MP-BRNO.
           MOVE MP-PATH-SQL    TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.

           IF ( MP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-MP1-FILE.

           IF ( QMP1-WEND-SEQNO = ZEROES    ) OR
              ( QMP1-WEND-SEQNO NOT NUMERIC )
              MOVE MP-PATH-OWNBR        TO QMP1-WBEG-BRNO
              MOVE MP-PATH-OWNBR        TO QMP1-WEND-BRNO
              MOVE MP-ACCTNO            TO QMP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QMP1-WEND-ACCTNO
              MOVE MP-SEQNO             TO QMP1-WBEG-SEQNO
              MOVE ALL "9"              TO QMP1-WEND-SEQNO.

           EXEC SQL
            DECLARE MP1_CURSOR CURSOR FOR
            SELECT MPFILE.MP_BRNO,
                   MPFILE.MP_ACCTNO,
                   MPFILE.MP_SEQNO,
                   CAST(MPFILE.MP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_USERID,
                   CAST(MPFILE.MP_POSTDATE AS VARCHAR(10)),
                   MPFILE.MP_POSTTIME,
                   CAST(MPFILE.MP_TRDATE AS VARCHAR(10)),
                   CAST(MPFILE.MP_PAYDATE AS VARCHAR(10)),
                   MPFILE.MP_TRCD,
                   MPFILE.MP_REFNO,
                   MPFILE.MP_TRAMT,
                   MPFILE.MP_APOTH,
                   MPFILE.MP_APOT2,
                   MPFILE.MP_APINT,
                   MPFILE.MP_APLC,
                   MPFILE.MP_APCUR,
                   MPFILE.MP_OTHBAL,
                   MPFILE.MP_OT2BAL,
                   MPFILE.MP_INTBAL,
                   MPFILE.MP_LCBAL,
                   MPFILE.MP_CURBAL,
                   MPFILE.MP_INTDUE,
                   MPFILE.MP_DLPROC,
                   MPFILE.MP_EARNED_1,
                   MPFILE.MP_EARNED_2,
                   MPFILE.MP_EARNED_3,
                   MPFILE.MP_INTPAID,
                   MPFILE.MP_SSNO,
                   CAST(MPFILE.MP_PDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(MPFILE.MP_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(MPFILE.MP_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_TILLNO,
                   MPFILE.MP_APINTOWE,
                   MPFILE.MP_DLPARVHELD_ADJ,
                   MPFILE.MP_LT_SEQNO,
                   MPFILE.MP_IBPC,
                   MPFILE.MP_MANUAL_RB,
                   MPFILE.MP_INSTYPES,
                   MPFILE.MP_REV,
                   MPFILE.MP_REVTRCD,
                   MPFILE.MP_REVSEQNO,
                   MPFILE.MP_STATUSFG,
                   MPFILE.MP_NEWSTAT_RATE,
                   MPFILE.MP_LCPARTIALS,
                   MPFILE.MP_ADDON_ORIG_REGPY,
                   CAST(MPFILE.MP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),
                   CAST(MPFILE.MP_DEF_DUEDATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_LCPAID,
                   CAST(MPFILE.MP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_DEF_TERM,
                   MPFILE.MP_ADDON_LAST_PYAMT,
                   MPFILE.MP_DLRVCHGS_ADJ_1,
                   MPFILE.MP_DLRVCHGS_ADJ_2,
                   MPFILE.MP_DLRVCHGS_ADJ_3,
                   MPFILE.MP_DLPARVPAID_ADJ,
                   MPFILE.MP_DLNO,
                   MPFILE.MP_PREPAY_PENALTY,
                   MPFILE.MP_PRORATED_INS_REB,
                   MPFILE.MP_POSTING_BR,
                   MPFILE.MP_NOLCHG,
                   MPFILE.MP_COLLECTOR,
                   MPFILE.MP_EXTRA_PRIN_LFP,
                   MPFILE.MP_REMAIN_INT_SINCE_LFP,
                   MPFILE.MP_REMAIN_PRIN_SINCE_LFP,
                   MPFILE.MP_APPLIED_TO_ESCROW,
                   MPFILE.MP_ESCROW_PREPAID,
                   MPFILE.MP_PRIOR_POOLID,
                   CAST(MPFILE.MP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_FROMCL,
                   MPFILE.MP_TOCL,
                   MPFILE.MP_ALT_PREPAY_FG
            FROM DBO.MPFILE
            WHERE MPFILE.MP_BRNO  = :QMP1-WBEG-BRNO
              AND MPFILE.MP_ACCTNO >= :QMP1-WBEG-ACCTNO
              AND MPFILE.MP_ACCTNO <= :QMP1-WEND-ACCTNO
              AND MPFILE.MP_SEQNO >= :QMP1-WBEG-SEQNO
              AND MPFILE.MP_SEQNO <= :QMP1-WEND-SEQNO
            ORDER BY MPFILE.MP_BRNO,
                     MPFILE.MP_ACCTNO,
                     MPFILE.MP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN MP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QMP1-WBEG-BRNO
                          QMP1-WEND-BRNO
                          QMP1-WBEG-ACCTNO
                          QMP1-WEND-ACCTNO
                          QMP1-WBEG-SEQNO
                          QMP1-WEND-SEQNO.

           MOVE STAT---GOOD TO MP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-MP1-FILE.
           PERFORM READ-IT.
           MOVE MP-PATH-OWNBR  TO MP-BRNO. 
           MOVE MP-PATH-SQL    TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.
           INITIALIZE QMP-REC.

           MOVE MP-BRNO   TO QMP-BRNO.
           MOVE MP-ACCTNO TO QMP-ACCTNO.
           MOVE MP-SEQNO  TO QMP-SEQNO.

           EXEC SQL
            SELECT MPFILE.MP_BRNO,
                   MPFILE.MP_ACCTNO,
                   MPFILE.MP_SEQNO,
                   CAST(MPFILE.MP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_USERID,
                   CAST(MPFILE.MP_POSTDATE AS VARCHAR(10)),
                   MPFILE.MP_POSTTIME,
                   CAST(MPFILE.MP_TRDATE AS VARCHAR(10)),
                   CAST(MPFILE.MP_PAYDATE AS VARCHAR(10)),
                   MPFILE.MP_TRCD,
                   MPFILE.MP_REFNO,
                   MPFILE.MP_TRAMT,
                   MPFILE.MP_APOTH,
                   MPFILE.MP_APOT2,
                   MPFILE.MP_APINT,
                   MPFILE.MP_APLC,
                   MPFILE.MP_APCUR,
                   MPFILE.MP_OTHBAL,
                   MPFILE.MP_OT2BAL,
                   MPFILE.MP_INTBAL,
                   MPFILE.MP_LCBAL,
                   MPFILE.MP_CURBAL,
                   MPFILE.MP_INTDUE,
                   MPFILE.MP_DLPROC,
                   MPFILE.MP_EARNED_1,
                   MPFILE.MP_EARNED_2,
                   MPFILE.MP_EARNED_3,
                   MPFILE.MP_INTPAID,
                   MPFILE.MP_SSNO,
                   CAST(MPFILE.MP_PDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(MPFILE.MP_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(MPFILE.MP_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_TILLNO,
                   MPFILE.MP_APINTOWE,
                   MPFILE.MP_DLPARVHELD_ADJ,
                   MPFILE.MP_LT_SEQNO,
                   MPFILE.MP_IBPC,
                   MPFILE.MP_MANUAL_RB,
                   MPFILE.MP_INSTYPES,
                   MPFILE.MP_REV,
                   MPFILE.MP_REVTRCD,
                   MPFILE.MP_REVSEQNO,
                   MPFILE.MP_STATUSFG,
                   MPFILE.MP_NEWSTAT_RATE,
                   MPFILE.MP_LCPARTIALS,
                   MPFILE.MP_ADDON_ORIG_REGPY,
                   CAST(MPFILE.MP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),
                   CAST(MPFILE.MP_DEF_DUEDATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_LCPAID,
                   CAST(MPFILE.MP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_DEF_TERM,
                   MPFILE.MP_ADDON_LAST_PYAMT,
                   MPFILE.MP_DLRVCHGS_ADJ_1,
                   MPFILE.MP_DLRVCHGS_ADJ_2,
                   MPFILE.MP_DLRVCHGS_ADJ_3,
                   MPFILE.MP_DLPARVPAID_ADJ,
                   MPFILE.MP_DLNO,
                   MPFILE.MP_PREPAY_PENALTY,
                   MPFILE.MP_PRORATED_INS_REB,
                   MPFILE.MP_POSTING_BR,
                   MPFILE.MP_NOLCHG,
                   MPFILE.MP_COLLECTOR,
                   MPFILE.MP_EXTRA_PRIN_LFP,
                   MPFILE.MP_REMAIN_INT_SINCE_LFP,
                   MPFILE.MP_REMAIN_PRIN_SINCE_LFP,
                   MPFILE.MP_APPLIED_TO_ESCROW,
                   MPFILE.MP_ESCROW_PREPAID,
                   MPFILE.MP_PRIOR_POOLID,
                   CAST(MPFILE.MP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),
                   MPFILE.MP_FROMCL,
                   MPFILE.MP_TOCL,
                   MPFILE.MP_ALT_PREPAY_FG
            INTO :QMP-REC
            FROM DBO.MPFILE
            WHERE MPFILE.MP_BRNO = :QMP-BRNO
              AND MPFILE.MP_ACCTNO = :QMP-ACCTNO
              AND MPFILE.MP_SEQNO = :QMP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-MP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-MP-FIELDS.

      *-----------------------------------------------------------------
       READ-MP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE MP-PATH-OWNBR  TO MP-BRNO.
           MOVE MP-PATH-SQL    TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.
           INITIALIZE QMP-REC.

           IF ( MP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH MP1_CURSOR INTO :QMP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-MP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-MP-FIELDS.

      *-----------------------------------------------------------------
       WRITE-MP1-FILE.

           MOVE MP-PATH-OWNBR  TO MP-BRNO.
           ACCEPT MP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE MP-PATH-SQL    TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.
           PERFORM SET-MP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.MPFILE
             (MPFILE.MP_BRNO,
              MPFILE.MP_ACCTNO,
              MPFILE.MP_SEQNO,
              MPFILE.MP_LTOUCH_DATE,
              MPFILE.MP_USERID,
              MPFILE.MP_POSTDATE,
              MPFILE.MP_POSTTIME,
              MPFILE.MP_TRDATE,
              MPFILE.MP_PAYDATE,
              MPFILE.MP_TRCD,
              MPFILE.MP_REFNO,
              MPFILE.MP_TRAMT,
              MPFILE.MP_APOTH,
              MPFILE.MP_APOT2,
              MPFILE.MP_APINT,
              MPFILE.MP_APLC,
              MPFILE.MP_APCUR,
              MPFILE.MP_OTHBAL,
              MPFILE.MP_OT2BAL,
              MPFILE.MP_INTBAL,
              MPFILE.MP_LCBAL,
              MPFILE.MP_CURBAL,
              MPFILE.MP_INTDUE,
              MPFILE.MP_DLPROC,
              MPFILE.MP_EARNED_1,
              MPFILE.MP_EARNED_2,
              MPFILE.MP_EARNED_3,
              MPFILE.MP_INTPAID,
              MPFILE.MP_SSNO,
              MPFILE.MP_PDTH_DATE,
              MPFILE.MP_LCPDTH_DATE,
              MPFILE.MP_INTPDTH_DATE,
              MPFILE.MP_TILLNO,
              MPFILE.MP_APINTOWE,
              MPFILE.MP_DLPARVHELD_ADJ,
              MPFILE.MP_LT_SEQNO,
              MPFILE.MP_IBPC,
              MPFILE.MP_MANUAL_RB,
              MPFILE.MP_INSTYPES,
              MPFILE.MP_REV,
              MPFILE.MP_REVTRCD,
              MPFILE.MP_REVSEQNO,
              MPFILE.MP_STATUSFG,
              MPFILE.MP_NEWSTAT_RATE,
              MPFILE.MP_LCPARTIALS,
              MPFILE.MP_ADDON_ORIG_REGPY,
              MPFILE.MP_NEW1ST_PYDATE,
              MPFILE.MP_DEF_DUEDATE,
              MPFILE.MP_LCPAID,
              MPFILE.MP_OLD1ST_PYDATE,
              MPFILE.MP_DEF_TERM,
              MPFILE.MP_ADDON_LAST_PYAMT,
              MPFILE.MP_DLRVCHGS_ADJ_1,
              MPFILE.MP_DLRVCHGS_ADJ_2,
              MPFILE.MP_DLRVCHGS_ADJ_3,
              MPFILE.MP_DLPARVPAID_ADJ,
              MPFILE.MP_DLNO,
              MPFILE.MP_PREPAY_PENALTY,
              MPFILE.MP_PRORATED_INS_REB,
              MPFILE.MP_POSTING_BR,
              MPFILE.MP_NOLCHG,
              MPFILE.MP_COLLECTOR,
              MPFILE.MP_EXTRA_PRIN_LFP,
              MPFILE.MP_REMAIN_INT_SINCE_LFP,
              MPFILE.MP_REMAIN_PRIN_SINCE_LFP,
              MPFILE.MP_APPLIED_TO_ESCROW,
              MPFILE.MP_ESCROW_PREPAID,
              MPFILE.MP_PRIOR_POOLID,
              MPFILE.MP_ADDON_INTPDTH_DATE,
              MPFILE.MP_FROMCL,
              MPFILE.MP_TOCL,
              MPFILE.MP_ALT_PREPAY_FG)
            VALUES
             (:QMP-BRNO,
              :QMP-ACCTNO,
              :QMP-SEQNO,
              :QMP-LTOUCH-DATE,
              :QMP-USERID,
              :QMP-POSTDATE,
              :QMP-POSTTIME,
              :QMP-TRDATE,
              :QMP-PAYDATE,
              :QMP-TRCD,
              :QMP-REFNO,
              :QMP-TRAMT,
              :QMP-APOTH,
              :QMP-APOT2,
              :QMP-APINT,
              :QMP-APLC,
              :QMP-APCUR,
              :QMP-OTHBAL,
              :QMP-OT2BAL,
              :QMP-INTBAL,
              :QMP-LCBAL,
              :QMP-CURBAL,
              :QMP-INTDUE,
              :QMP-DLPROC,
              :QMP-EARNED-1,
              :QMP-EARNED-2,
              :QMP-EARNED-3,
              :QMP-INTPAID,
              :QMP-SSNO,
              :QMP-PDTH-DATE,
              :QMP-LCPDTH-DATE,
              :QMP-INTPDTH-DATE,
              :QMP-TILLNO,
              :QMP-APINTOWE,
              :QMP-DLPARVHELD-ADJ,
              :QMP-LT-SEQNO,
              :QMP-IBPC,
              :QMP-MANUAL-RB,
              :QMP-INSTYPES,
              :QMP-REV,
              :QMP-REVTRCD,
              :QMP-REVSEQNO,
              :QMP-STATUSFG,
              :QMP-NEWSTAT-RATE,
              :QMP-LCPARTIALS,
              :QMP-ADDON-ORIG-REGPY,
              :QMP-NEW1ST-PYDATE,
              :QMP-DEF-DUEDATE,
              :QMP-LCPAID,
              :QMP-OLD1ST-PYDATE,
              :QMP-DEF-TERM,
              :QMP-ADDON-LAST-PYAMT,
              :QMP-DLRVCHGS-ADJ-1,
              :QMP-DLRVCHGS-ADJ-2,
              :QMP-DLRVCHGS-ADJ-3,
              :QMP-DLPARVPAID-ADJ,
              :QMP-DLNO,
              :QMP-PREPAY-PENALTY,
              :QMP-PRORATED-INS-REB,
              :QMP-POSTING-BR,
              :QMP-NOLCHG,
              :QMP-COLLECTOR,
              :QMP-EXTRA-PRIN-LFP,
              :QMP-REMAIN-INT-SINCE-LFP,
              :QMP-REMAIN-PRIN-SINCE-LFP,
              :QMP-APPLIED-TO-ESCROW,
              :QMP-ESCROW-PREPAID,
              :QMP-PRIOR-POOLID,
              :QMP-ADDON-INTPDTH-DATE,
              :QMP-FROMCL,
              :QMP-TOCL,
              :QMP-ALT-PREPAY-FG)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-MP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-MP1-FILE.

           MOVE MP-PATH-OWNBR  TO MP-BRNO.
           ACCEPT MP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE MP-PATH-SQL    TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.
           PERFORM SET-MP-FIELDS.

           EXEC SQL
            UPDATE DBO.MPFILE
             SET
              MPFILE.MP_BRNO = :QMP-BRNO,
              MPFILE.MP_ACCTNO = :QMP-ACCTNO,
              MPFILE.MP_SEQNO = :QMP-SEQNO,
              MPFILE.MP_LTOUCH_DATE =
                 :QMP-LTOUCH-DATE,
              MPFILE.MP_USERID = :QMP-USERID,
              MPFILE.MP_POSTDATE =
                 :QMP-POSTDATE,
              MPFILE.MP_POSTTIME = :QMP-POSTTIME,
              MPFILE.MP_TRDATE =
                 :QMP-TRDATE,
              MPFILE.MP_PAYDATE =
                 :QMP-PAYDATE,
              MPFILE.MP_TRCD = :QMP-TRCD,
              MPFILE.MP_REFNO = :QMP-REFNO,
              MPFILE.MP_TRAMT = :QMP-TRAMT,
              MPFILE.MP_APOTH = :QMP-APOTH,
              MPFILE.MP_APOT2 = :QMP-APOT2,
              MPFILE.MP_APINT = :QMP-APINT,
              MPFILE.MP_APLC = :QMP-APLC,
              MPFILE.MP_APCUR = :QMP-APCUR,
              MPFILE.MP_OTHBAL = :QMP-OTHBAL,
              MPFILE.MP_OT2BAL = :QMP-OT2BAL,
              MPFILE.MP_INTBAL = :QMP-INTBAL,
              MPFILE.MP_LCBAL = :QMP-LCBAL,
              MPFILE.MP_CURBAL = :QMP-CURBAL,
              MPFILE.MP_INTDUE = :QMP-INTDUE,
              MPFILE.MP_DLPROC = :QMP-DLPROC,
              MPFILE.MP_EARNED_1 = :QMP-EARNED-1,
              MPFILE.MP_EARNED_2 = :QMP-EARNED-2,
              MPFILE.MP_EARNED_3 = :QMP-EARNED-3,
              MPFILE.MP_INTPAID = :QMP-INTPAID,
              MPFILE.MP_SSNO = :QMP-SSNO,
              MPFILE.MP_PDTH_DATE =
                 :QMP-PDTH-DATE,
              MPFILE.MP_LCPDTH_DATE =
                 :QMP-LCPDTH-DATE,
              MPFILE.MP_INTPDTH_DATE =
                 :QMP-INTPDTH-DATE,
              MPFILE.MP_TILLNO = :QMP-TILLNO,
              MPFILE.MP_APINTOWE = :QMP-APINTOWE,
              MPFILE.MP_DLPARVHELD_ADJ = :QMP-DLPARVHELD-ADJ,
              MPFILE.MP_LT_SEQNO = :QMP-LT-SEQNO,
              MPFILE.MP_IBPC = :QMP-IBPC,
              MPFILE.MP_MANUAL_RB = :QMP-MANUAL-RB,
              MPFILE.MP_INSTYPES = :QMP-INSTYPES,
              MPFILE.MP_REV = :QMP-REV,
              MPFILE.MP_REVTRCD = :QMP-REVTRCD,
              MPFILE.MP_REVSEQNO = :QMP-REVSEQNO,
              MPFILE.MP_STATUSFG = :QMP-STATUSFG,
              MPFILE.MP_NEWSTAT_RATE = :QMP-NEWSTAT-RATE,
              MPFILE.MP_LCPARTIALS = :QMP-LCPARTIALS,
              MPFILE.MP_ADDON_ORIG_REGPY = :QMP-ADDON-ORIG-REGPY,
              MPFILE.MP_NEW1ST_PYDATE =
                 :QMP-NEW1ST-PYDATE,
              MPFILE.MP_DEF_DUEDATE =
                 :QMP-DEF-DUEDATE,
              MPFILE.MP_LCPAID = :QMP-LCPAID,
              MPFILE.MP_OLD1ST_PYDATE =
                 :QMP-OLD1ST-PYDATE,
              MPFILE.MP_DEF_TERM = :QMP-DEF-TERM,
              MPFILE.MP_ADDON_LAST_PYAMT = :QMP-ADDON-LAST-PYAMT,
              MPFILE.MP_DLRVCHGS_ADJ_1 = :QMP-DLRVCHGS-ADJ-1,
              MPFILE.MP_DLRVCHGS_ADJ_2 = :QMP-DLRVCHGS-ADJ-2,
              MPFILE.MP_DLRVCHGS_ADJ_3 = :QMP-DLRVCHGS-ADJ-3,
              MPFILE.MP_DLPARVPAID_ADJ = :QMP-DLPARVPAID-ADJ,
              MPFILE.MP_DLNO = :QMP-DLNO,
              MPFILE.MP_PREPAY_PENALTY = :QMP-PREPAY-PENALTY,
              MPFILE.MP_PRORATED_INS_REB = :QMP-PRORATED-INS-REB,
              MPFILE.MP_POSTING_BR = :QMP-POSTING-BR,
              MPFILE.MP_NOLCHG = :QMP-NOLCHG,
              MPFILE.MP_COLLECTOR = :QMP-COLLECTOR,
              MPFILE.MP_EXTRA_PRIN_LFP = :QMP-EXTRA-PRIN-LFP,
              MPFILE.MP_REMAIN_INT_SINCE_LFP =
                   :QMP-REMAIN-INT-SINCE-LFP,
              MPFILE.MP_REMAIN_PRIN_SINCE_LFP =
                   :QMP-REMAIN-PRIN-SINCE-LFP,
              MPFILE.MP_APPLIED_TO_ESCROW =
                   :QMP-APPLIED-TO-ESCROW,
              MPFILE.MP_ESCROW_PREPAID = :QMP-ESCROW-PREPAID,
              MPFILE.MP_PRIOR_POOLID = :QMP-PRIOR-POOLID,
              MPFILE.MP_ADDON_INTPDTH_DATE = :QMP-ADDON-INTPDTH-DATE,
              MPFILE.MP_FROMCL = :QMP-FROMCL,
              MPFILE.MP_TOCL = :QMP-TOCL,
              MPFILE.MP_ALT_PREPAY_FG = :QMP-ALT-PREPAY-FG
            WHERE MPFILE.MP_BRNO = :QMP-BRNO
              AND MPFILE.MP_ACCTNO = :QMP-ACCTNO
              AND MPFILE.MP_SEQNO = :QMP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-MP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-MP1-FILE.

           MOVE MP-PATH-OWNBR  TO MP-BRNO.
           PERFORM DELETE-IT.
           MOVE MP-PATH-SQL    TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.
           PERFORM SET-MP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.MPFILE
            WHERE MPFILE.MP_BRNO = :QMP-BRNO
              AND MPFILE.MP_ACCTNO = :QMP-ACCTNO
              AND MPFILE.MP_SEQNO = :QMP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-MP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-MP1-FILE.

           PERFORM CLOSE-IT.

           IF ( MP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE MP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO MP1-CURSOR-STAT.

