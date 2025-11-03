      *================================================================*
      * END COPYBOOK: LIBLP\LPLTPGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLTIGS_SQL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLTIGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN TRAILER - INSURANCE
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LTI-REC FROM QLTI-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTI-FIELDS.
           MOVE QLTI-BRNO TO LTI-BRNO.
           MOVE QLTI-ACCTNO TO LTI-ACCTNO.
           MOVE QLTI-SEQNO TO LTI-SEQNO.

           IF ( QLTI-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-LTOUCH-DATE
           ELSE
              MOVE QLTI-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-LTOUCH-DATE.

           MOVE QLTI-INS-TYPE TO LTI-INS-TYPE.

           IF ( QLTI-ORIG-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO LTI-ORIG-1STPYDATE
           ELSE
              MOVE QLTI-ORIG-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-ORIG-1STPYDATE.

           MOVE QLTI-PRIOR-NODEF TO LTI-PRIOR-NODEF.
           MOVE QLTI-INSFEECD-1 TO LTI-INSFEECD(1).
           MOVE QLTI-INSFEE-1 TO LTI-INSFEE(1).
           MOVE QLTI-INSFEECD-2 TO LTI-INSFEECD(2).
           MOVE QLTI-INSFEE-2 TO LTI-INSFEE(2).
           MOVE QLTI-TRCD TO LTI-TRCD.
           MOVE QLTI-INSCOMP TO LTI-INSCOMP.
           MOVE QLTI-INSOURS TO LTI-INSOURS.

           IF ( QLTI-INSEFF-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-INSEFF-DATE
           ELSE
              MOVE QLTI-INSEFF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-INSEFF-DATE.


           IF ( QLTI-INSEXP-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-INSEXP-DATE
           ELSE
              MOVE QLTI-INSEXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-INSEXP-DATE.

           MOVE QLTI-INSPREM TO LTI-INSPREM.
           MOVE QLTI-INSCOVR TO LTI-INSCOVR.
           MOVE QLTI-INSCOMM TO LTI-INSCOMM.
           MOVE QLTI-ANTICTERM TO LTI-ANTICTERM.
           MOVE QLTI-ANTICERN-1 TO LTI-ANTICERN(1).
           MOVE QLTI-ANTICADJ-1 TO LTI-ANTICADJ(1).
           MOVE QLTI-UFERN-1 TO LTI-UFERN(1).
           MOVE QLTI-ACCERN-1 TO LTI-ACCERN(1).
           MOVE QLTI-ANTICERN-2 TO LTI-ANTICERN(2).
           MOVE QLTI-ANTICADJ-2 TO LTI-ANTICADJ(2).
           MOVE QLTI-UFERN-2 TO LTI-UFERN(2).
           MOVE QLTI-ACCERN-2 TO LTI-ACCERN(2).
           MOVE QLTI-ANTICERN-3 TO LTI-ANTICERN(3).
           MOVE QLTI-ANTICADJ-3 TO LTI-ANTICADJ(3).
           MOVE QLTI-UFERN-3 TO LTI-UFERN(3).
           MOVE QLTI-ACCERN-3 TO LTI-ACCERN(3).
           MOVE QLTI-INT-REBATE TO LTI-INT-REBATE.
           MOVE QLTI-INS-PAYMNT-INCR TO LTI-INS-PAYMNT-INCR.
           MOVE QLTI-INS-TOT-ADDON TO LTI-INS-TOT-ADDON.
           MOVE QLTI-INT-LAST-EARNED TO LTI-INT-LAST-EARNED.
           MOVE QLTI-INS-REBATE TO LTI-INS-REBATE.
           MOVE QLTI-RB-WORKER TO LTI-RB-WORKER.

           IF ( QLTI-ANTIC-START-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-ANTIC-START-DATE
           ELSE
              MOVE QLTI-ANTIC-START-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-ANTIC-START-DATE.

           MOVE QLTI-CANCEL-REASON-CD TO LTI-CANCEL-REASON-CD.
 
      *-----------------------------------------------------------------
      *    LOAD QLTI-REC FROM LTI-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
