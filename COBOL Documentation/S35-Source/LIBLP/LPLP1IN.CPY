      * COPYMEMBER: LIBLP/LPLP1IN
      * BAH 20240212 ADDED LP-REPAY-TRANS-ID #1641
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
      * ***NOTE*** THIS 'START' IS NOT FOR DISPLAYING MULTIPLE PAGES.
      * ONLY SHOULD BE USED ONCE PER PROGRAM/BRANCH/ACCOUNT LIKE IN
      * A STANDARD REPORT.  USE START-LP1-FILE-SCAN FOR DISPLAYING
      * MULTIPLE PAGES.
      *-----------------------------------------------------------------
       START-LP1-FILE.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.

           IF ( LP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLP1-WEND-ACCTNO
              MOVE LP-SEQNO             TO QLP1-WBEG-SEQNO
              MOVE ALL "9"              TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LP1_CURSOR CURSOR FOR
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
            FROM DBO.LPFILE
            WHERE   LPFILE.LP_BRNO    = :QLP1-WBEG-BRNO
              AND ( LPFILE.LP_ACCTNO >= :QLP1-WBEG-ACCTNO
              AND   LPFILE.LP_SEQNO  >= :QLP1-WBEG-SEQNO )
              AND ( LPFILE.LP_ACCTNO <= :QLP1-WEND-ACCTNO
              AND   LPFILE.LP_SEQNO  <= :QLP1-WEND-SEQNO )
            ORDER BY LPFILE.LP_BRNO,
                     LPFILE.LP_ACCTNO,
                     LPFILE.LP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-STAT.

      *-----------------------------------------------------------------
      * ***NOTE*** THIS 'START' IS FOR USE IN DISPLAYING MULTIPLE PAGES.
      * WHEN GOING UP & DOWN DISPLAYING PAGES, NEED TO USE THIS 'START'.
      * IF FIRST USED A START-LP1-FILE-NOT-GREATER, YOU NEED TO ADD
      * ONE TO LP-SEQNO IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      *-----------------------------------------------------------------
       START-LP1-FILE-SCAN.

           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.

           IF ( LP1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLP1-WEND-ACCTNO
              MOVE LP-SEQNO             TO QLP1-WBEG-SEQNO
              MOVE ALL "9"              TO QLP1-WEND-SEQNO
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( LP-BRNO = 9999 )
                 MOVE LP-BRNO           TO QLP1-WSBEG-BRNO
              ELSE
                 COMPUTE QLP1-WSBEG-BRNO = LP-BRNO + 1
              END-IF

              IF ( LP-ACCTNO = 99999999 )
                 MOVE LP-ACCTNO         TO QLP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QLP1-WSBEG-ACCTNO = LP-ACCTNO + 1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LP1_CURSOR_SC CURSOR FOR
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
            FROM DBO.LPFILE
            WHERE  ( LPFILE.LP_BRNO        = :QLP1-WBEG-BRNO )
              AND (( LPFILE.LP_ACCTNO     >= :QLP1-WBEG-ACCTNO
              AND    LPFILE.LP_SEQNO      >= :QLP1-WBEG-SEQNO )
               OR  ( LPFILE.LP_ACCTNO     >= :QLP1-WSBEG-ACCTNO
              AND    LPFILE.LP_SEQNO      >= 0 ))
      *        OR  ( LPFILE.LP_ACCTNO     >= 0
      *       AND    LPFILE.LP_SEQNO      >= 0 ))
              AND (  LPFILE.LP_ACCTNO     <= :QLP1-WEND-ACCTNO
              AND    LPFILE.LP_SEQNO      <= :QLP1-WEND-SEQNO )
            ORDER BY LPFILE.LP_BRNO,
                     LPFILE.LP_ACCTNO,
                     LPFILE.LP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO
                          QLP1-WSBEG-BRNO
                          QLP1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO LP1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
      *  ***NOTE*** ONLY USE START-LP1-FILE-SCAN WHEN USING A 
      *  START-LP1-FILE-NOT-GREATER.  THIS IS FOR DISPLAYING MULTIPLE
      *  PAGE IN A UP & DOWN SCENARIO.
      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE LP-ACCTNO            TO QLP1-WEND-ACCTNO
              MOVE ALL "0"              TO QLP1-WBEG-SEQNO
              MOVE LP-SEQNO             TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LP1_CURSOR_NG CURSOR FOR
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
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO    = :QLP1-WEND-BRNO
              AND LPFILE.LP_ACCTNO  = :QLP1-WEND-ACCTNO
              AND LPFILE.LP_SEQNO  <= :QLP1-WEND-SEQNO 
            ORDER BY LPFILE.LP_BRNO DESC,
                     LPFILE.LP_ACCTNO DESC,
                     LPFILE.LP_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-NG-STAT.

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
       READ-LP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR INTO :QLP-REC
              END-EXEC
           ELSE
           IF ( LP1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR_SC INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR_NG INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LP1-FILE.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           ACCEPT LP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
      *    MOVE LP1-KEY        TO E-KEYX LP-VISION-KEY1.
           PERFORM SET-LP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LPFILE
             (LPFILE.LP_BRNO,
              LPFILE.LP_ACCTNO,
              LPFILE.LP_SEQNO,
              LPFILE.LP_LTOUCH_DATE,
              LPFILE.LP_USERID,
              LPFILE.LP_POSTDATE,
              LPFILE.LP_POSTTIME,
              LPFILE.LP_TRDATE,
              LPFILE.LP_PAYDATE,
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
              LPFILE.LP_PDTH_DATE,
              LPFILE.LP_LCPDTH_DATE,
              LPFILE.LP_INTPDTH_DATE,
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
              LPFILE.LP_NEW1ST_PYDATE,
              LPFILE.LP_DEF_DUEDATE,
              LPFILE.LP_LCPAID,
              LPFILE.LP_OLD1ST_PYDATE,
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
              LPFILE.LP_ADDON_INTPDTH_DATE,
              LPFILE.LP_FROMCL,
              LPFILE.LP_TOCL,
              LPFILE.LP_ALT_PREPAY_FG,
              LPFILE.LP_REPAY_TRANS_ID)
            VALUES
             (:QLP-BRNO,
              :QLP-ACCTNO,
              :QLP-SEQNO,
              :QLP-LTOUCH-DATE,
              :QLP-USERID,
              :QLP-POSTDATE,
              :QLP-POSTTIME,
              :QLP-TRDATE,
              :QLP-PAYDATE,
              :QLP-TRCD,
              :QLP-REFNO,
              :QLP-TRAMT,
              :QLP-APOTH,
              :QLP-APOT2,
              :QLP-APINT,
              :QLP-APLC,
              :QLP-APCUR,
              :QLP-OTHBAL,
              :QLP-OT2BAL,
              :QLP-INTBAL,
              :QLP-LCBAL,
              :QLP-CURBAL,
              :QLP-INTDUE,
              :QLP-DLPROC,
              :QLP-EARNED-1,
              :QLP-EARNED-2,
              :QLP-EARNED-3,
              :QLP-INTPAID,
              :QLP-SSNO,
              :QLP-PDTH-DATE,
              :QLP-LCPDTH-DATE,
              :QLP-INTPDTH-DATE,
              :QLP-TILLNO,
              :QLP-APINTOWE,
              :QLP-DLPARVHELD-ADJ,
              :QLP-LT-SEQNO,
              :QLP-IBPC,
              :QLP-MANUAL-RB,
              :QLP-INSTYPES,
              :QLP-REV,
              :QLP-REVTRCD,
              :QLP-REVSEQNO,
              :QLP-STATUSFG,
              :QLP-NEWSTAT-RATE,
              :QLP-LCPARTIALS,
              :QLP-ADDON-ORIG-REGPY,
              :QLP-NEW1ST-PYDATE,
              :QLP-DEF-DUEDATE,
              :QLP-LCPAID,
              :QLP-OLD1ST-PYDATE,
              :QLP-DEF-TERM,
              :QLP-ADDON-LAST-PYAMT,
              :QLP-DLRVCHGS-ADJ-1,
              :QLP-DLRVCHGS-ADJ-2,
              :QLP-DLRVCHGS-ADJ-3,
              :QLP-DLPARVPAID-ADJ,
              :QLP-DLNO,
              :QLP-PREPAY-PENALTY,
              :QLP-PRORATED-INS-REB,
              :QLP-POSTING-BR,
              :QLP-NOLCHG,
              :QLP-COLLECTOR,
              :QLP-EXTRA-PRIN-LFP,
              :QLP-REMAIN-INT-SINCE-LFP,
              :QLP-REMAIN-PRIN-SINCE-LFP,
              :QLP-APPLIED-TO-ESCROW,
              :QLP-ESCROW-PREPAID,
              :QLP-PRIOR-POOLID,
              :QLP-ADDON-INTPDTH-DATE,
              :QLP-FROMCL,
              :QLP-TOCL,
              :QLP-ALT-PREPAY-FG,
              :QLP-REPAY-TRANS-ID)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LP1-FILE.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           ACCEPT LP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           PERFORM SET-LP-FIELDS.

           EXEC SQL
            UPDATE DBO.LPFILE
             SET
              LPFILE.LP_BRNO = :QLP-BRNO,
              LPFILE.LP_ACCTNO = :QLP-ACCTNO,
              LPFILE.LP_SEQNO = :QLP-SEQNO,
              LPFILE.LP_LTOUCH_DATE =
                 :QLP-LTOUCH-DATE,
              LPFILE.LP_USERID = :QLP-USERID,
              LPFILE.LP_POSTDATE =
                 :QLP-POSTDATE,
              LPFILE.LP_POSTTIME = :QLP-POSTTIME,
              LPFILE.LP_TRDATE =
                 :QLP-TRDATE,
              LPFILE.LP_PAYDATE =
                 :QLP-PAYDATE,
              LPFILE.LP_TRCD = :QLP-TRCD,
              LPFILE.LP_REFNO = :QLP-REFNO,
              LPFILE.LP_TRAMT = :QLP-TRAMT,
              LPFILE.LP_APOTH = :QLP-APOTH,
              LPFILE.LP_APOT2 = :QLP-APOT2,
              LPFILE.LP_APINT = :QLP-APINT,
              LPFILE.LP_APLC = :QLP-APLC,
              LPFILE.LP_APCUR = :QLP-APCUR,
              LPFILE.LP_OTHBAL = :QLP-OTHBAL,
              LPFILE.LP_OT2BAL = :QLP-OT2BAL,
              LPFILE.LP_INTBAL = :QLP-INTBAL,
              LPFILE.LP_LCBAL = :QLP-LCBAL,
              LPFILE.LP_CURBAL = :QLP-CURBAL,
              LPFILE.LP_INTDUE = :QLP-INTDUE,
              LPFILE.LP_DLPROC = :QLP-DLPROC,
              LPFILE.LP_EARNED_1 = :QLP-EARNED-1,
              LPFILE.LP_EARNED_2 = :QLP-EARNED-2,
              LPFILE.LP_EARNED_3 = :QLP-EARNED-3,
              LPFILE.LP_INTPAID = :QLP-INTPAID,
              LPFILE.LP_SSNO = :QLP-SSNO,
              LPFILE.LP_PDTH_DATE =
                 :QLP-PDTH-DATE,
              LPFILE.LP_LCPDTH_DATE =
                 :QLP-LCPDTH-DATE,
              LPFILE.LP_INTPDTH_DATE =
                 :QLP-INTPDTH-DATE,
              LPFILE.LP_TILLNO = :QLP-TILLNO,
              LPFILE.LP_APINTOWE = :QLP-APINTOWE,
              LPFILE.LP_DLPARVHELD_ADJ = :QLP-DLPARVHELD-ADJ,
              LPFILE.LP_LT_SEQNO = :QLP-LT-SEQNO,
              LPFILE.LP_IBPC = :QLP-IBPC,
              LPFILE.LP_MANUAL_RB = :QLP-MANUAL-RB,
              LPFILE.LP_INSTYPES = :QLP-INSTYPES,
              LPFILE.LP_REV = :QLP-REV,
              LPFILE.LP_REVTRCD = :QLP-REVTRCD,
              LPFILE.LP_REVSEQNO = :QLP-REVSEQNO,
              LPFILE.LP_STATUSFG = :QLP-STATUSFG,
              LPFILE.LP_NEWSTAT_RATE = :QLP-NEWSTAT-RATE,
              LPFILE.LP_LCPARTIALS = :QLP-LCPARTIALS,
              LPFILE.LP_ADDON_ORIG_REGPY = :QLP-ADDON-ORIG-REGPY,
              LPFILE.LP_NEW1ST_PYDATE =
                 :QLP-NEW1ST-PYDATE,
              LPFILE.LP_DEF_DUEDATE =
                 :QLP-DEF-DUEDATE,
              LPFILE.LP_LCPAID = :QLP-LCPAID,
              LPFILE.LP_OLD1ST_PYDATE =
                 :QLP-OLD1ST-PYDATE,
              LPFILE.LP_DEF_TERM = :QLP-DEF-TERM,
              LPFILE.LP_ADDON_LAST_PYAMT = :QLP-ADDON-LAST-PYAMT,
              LPFILE.LP_DLRVCHGS_ADJ_1 = :QLP-DLRVCHGS-ADJ-1,
              LPFILE.LP_DLRVCHGS_ADJ_2 = :QLP-DLRVCHGS-ADJ-2,
              LPFILE.LP_DLRVCHGS_ADJ_3 = :QLP-DLRVCHGS-ADJ-3,
              LPFILE.LP_DLPARVPAID_ADJ = :QLP-DLPARVPAID-ADJ,
              LPFILE.LP_DLNO = :QLP-DLNO,
              LPFILE.LP_PREPAY_PENALTY = :QLP-PREPAY-PENALTY,
              LPFILE.LP_PRORATED_INS_REB = :QLP-PRORATED-INS-REB,
              LPFILE.LP_POSTING_BR = :QLP-POSTING-BR,
              LPFILE.LP_NOLCHG = :QLP-NOLCHG,
              LPFILE.LP_COLLECTOR = :QLP-COLLECTOR,
              LPFILE.LP_EXTRA_PRIN_LFP = :QLP-EXTRA-PRIN-LFP,
              LPFILE.LP_REMAIN_INT_SINCE_LFP =
                   :QLP-REMAIN-INT-SINCE-LFP,
              LPFILE.LP_REMAIN_PRIN_SINCE_LFP =
                   :QLP-REMAIN-PRIN-SINCE-LFP,
              LPFILE.LP_APPLIED_TO_ESCROW =
                   :QLP-APPLIED-TO-ESCROW,
              LPFILE.LP_ESCROW_PREPAID = :QLP-ESCROW-PREPAID,
              LPFILE.LP_PRIOR_POOLID = :QLP-PRIOR-POOLID,
              LPFILE.LP_ADDON_INTPDTH_DATE =
                 :QLP-ADDON-INTPDTH-DATE,
              LPFILE.LP_FROMCL = :QLP-FROMCL,
              LPFILE.LP_TOCL = :QLP-TOCL,
              LPFILE.LP_ALT_PREPAY_FG = :QLP-ALT-PREPAY-FG,
              LPFILE.LP_REPAY_TRANS_ID = :QLP-REPAY-TRANS-ID
             WHERE LPFILE.LP_BRNO = :QLP-BRNO
               AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
               AND LPFILE.LP_SEQNO = :QLP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LP1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LP1-FILE.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           PERFORM DELETE-IT.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           PERFORM SET-LP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LPFILE
             WHERE LPFILE.LP_BRNO = :QLP-BRNO
               AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
               AND LPFILE.LP_SEQNO = :QLP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-STAT.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-NG-STAT.

           IF ( LP1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-SC-STAT.

