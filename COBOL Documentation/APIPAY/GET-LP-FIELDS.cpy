      *================================================================*
      * END COPYBOOK: LIBLP\LPLTIGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLPGS_SQL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLPGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN PAYMENT FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      * BAH 20240212 ADDED LP-REPAY-TRANS-ID #1641
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LP-REC FROM QLP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LP-FIELDS.
           MOVE QLP-BRNO TO LP-BRNO.
           MOVE QLP-ACCTNO TO LP-ACCTNO.
           MOVE QLP-SEQNO TO LP-SEQNO.

           IF ( QLP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-LTOUCH-DATE
           ELSE
              MOVE QLP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-LTOUCH-DATE.

           MOVE QLP-USERID TO LP-USERID.

           IF ( QLP-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO LP-POSTDATE
           ELSE
              MOVE QLP-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-POSTDATE.

           MOVE QLP-POSTTIME TO LP-POSTTIME.

           IF ( QLP-TRDATE = "1900-01-01" )
              MOVE ZEROES TO LP-TRDATE
           ELSE
              MOVE QLP-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-TRDATE.


           IF ( QLP-PAYDATE = "1900-01-01" )
              MOVE ZEROES TO LP-PAYDATE
           ELSE
              MOVE QLP-PAYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-PAYDATE.

           MOVE QLP-TRCD TO LP-TRCD.
           MOVE QLP-REFNO TO LP-REFNO.
           MOVE QLP-TRAMT TO LP-TRAMT.
           MOVE QLP-APOTH TO LP-APOTH.
           MOVE QLP-APOT2 TO LP-APOT2.
           MOVE QLP-APINT TO LP-APINT.
           MOVE QLP-APLC TO LP-APLC.
           MOVE QLP-APCUR TO LP-APCUR.
           MOVE QLP-OTHBAL TO LP-OTHBAL.
           MOVE QLP-OT2BAL TO LP-OT2BAL.
           MOVE QLP-INTBAL TO LP-INTBAL.
           MOVE QLP-LCBAL TO LP-LCBAL.
           MOVE QLP-CURBAL TO LP-CURBAL.
           MOVE QLP-INTDUE TO LP-INTDUE.
           MOVE QLP-DLPROC TO LP-DLPROC.
           MOVE QLP-EARNED-1 TO LP-EARNED(1).
           MOVE QLP-EARNED-2 TO LP-EARNED(2).
           MOVE QLP-EARNED-3 TO LP-EARNED(3).
           MOVE QLP-INTPAID TO LP-INTPAID.
           MOVE QLP-SSNO TO LP-SSNO.

           IF ( QLP-PDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-PDTH-DATE
           ELSE
              MOVE QLP-PDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-PDTH-DATE.


           IF ( QLP-LCPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-LCPDTH-DATE
           ELSE
              MOVE QLP-LCPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-LCPDTH-DATE.


           IF ( QLP-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-INTPDTH-DATE
           ELSE
              MOVE QLP-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-INTPDTH-DATE.

           MOVE QLP-TILLNO TO LP-TILLNO.
           MOVE QLP-APINTOWE TO LP-APINTOWE.
           MOVE QLP-DLPARVHELD-ADJ TO LP-DLPARVHELD-ADJ.
           MOVE QLP-LT-SEQNO TO LP-LT-SEQNO.
           MOVE QLP-IBPC TO LP-IBPC.
           MOVE QLP-MANUAL-RB TO LP-MANUAL-RB.
           MOVE QLP-INSTYPES TO LP-INSTYPES.
           MOVE QLP-REV TO LP-REV.
           MOVE QLP-REVTRCD TO LP-REVTRCD.
           MOVE QLP-REVSEQNO TO LP-REVSEQNO.
           MOVE QLP-STATUSFG TO LP-STATUSFG.
           MOVE QLP-NEWSTAT-RATE TO LP-NEWSTAT-RATE.
           MOVE QLP-LCPARTIALS TO LP-LCPARTIALS.
           MOVE QLP-ADDON-ORIG-REGPY TO LP-ADDON-ORIG-REGPY.

           IF ( QLP-NEW1ST-PYDATE = "1900-01-01" )
              MOVE ZEROES TO LP-NEW1ST-PYDATE
           ELSE
              MOVE QLP-NEW1ST-PYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-NEW1ST-PYDATE.


           IF ( QLP-DEF-DUEDATE = "1900-01-01" )
              MOVE ZEROES TO LP-DEF-DUEDATE
           ELSE
              MOVE QLP-DEF-DUEDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-DEF-DUEDATE.

           MOVE QLP-LCPAID TO LP-LCPAID.

           IF ( QLP-OLD1ST-PYDATE = "1900-01-01" )
              MOVE ZEROES TO LP-OLD1ST-PYDATE
           ELSE
              MOVE QLP-OLD1ST-PYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-OLD1ST-PYDATE.

           MOVE QLP-DEF-TERM TO LP-DEF-TERM.
           MOVE QLP-ADDON-LAST-PYAMT TO LP-ADDON-LAST-PYAMT.
           MOVE QLP-DLRVCHGS-ADJ-1 TO LP-DLRVCHGS-ADJ(1).
           MOVE QLP-DLRVCHGS-ADJ-2 TO LP-DLRVCHGS-ADJ(2).
           MOVE QLP-DLRVCHGS-ADJ-3 TO LP-DLRVCHGS-ADJ(3).
           MOVE QLP-DLPARVPAID-ADJ TO LP-DLPARVPAID-ADJ.
           MOVE QLP-DLNO TO LP-DLNO.
           MOVE QLP-PREPAY-PENALTY TO LP-PREPAY-PENALTY.
           MOVE QLP-PRORATED-INS-REB TO LP-PRORATED-INS-REB.
           MOVE QLP-POSTING-BR TO LP-POSTING-BR.
           MOVE QLP-NOLCHG TO LP-NOLCHG.
           MOVE QLP-COLLECTOR TO LP-COLLECTOR.
           MOVE QLP-EXTRA-PRIN-LFP TO LP-EXTRA-PRIN-LFP.
           MOVE QLP-REMAIN-INT-SINCE-LFP TO LP-REMAIN-INT-SINCE-LFP.
           MOVE QLP-REMAIN-PRIN-SINCE-LFP TO LP-REMAIN-PRIN-SINCE-LFP.
           MOVE QLP-APPLIED-TO-ESCROW TO LP-APPLIED-TO-ESCROW.
           MOVE QLP-ESCROW-PREPAID TO LP-ESCROW-PREPAID.
           MOVE QLP-PRIOR-POOLID TO LP-PRIOR-POOLID.

           IF ( QLP-ADDON-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-ADDON-INTPDTH-DATE
           ELSE
              MOVE QLP-ADDON-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-ADDON-INTPDTH-DATE.

           MOVE QLP-FROMCL TO LP-FROMCL.
           MOVE QLP-TOCL TO LP-TOCL.
           MOVE QLP-ALT-PREPAY-FG TO LP-ALT-PREPAY-FG.
           MOVE QLP-REPAY-TRANS-ID TO LP-REPAY-TRANS-ID.
 
      *-----------------------------------------------------------------
      *    LOAD QLP-REC FROM LP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
