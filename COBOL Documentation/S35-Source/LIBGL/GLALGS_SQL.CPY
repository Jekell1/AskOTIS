      * COPYMEMBER: LIBGL/GLALGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - ALFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD AL-REC FROM QAL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-AL-FIELDS.
           MOVE QAL-ID TO AL-ID.

           IF ( QAL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO AL-LTOUCH-DATE
           ELSE
              MOVE QAL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO AL-LTOUCH-DATE.

           MOVE QAL-DESC TO AL-DESC.
           MOVE QAL-NO-ACCTS TO AL-NO-ACCTS.
           MOVE QAL-ACCT-1 TO AL-ACCT(1).
           MOVE QAL-PCT-1 TO AL-PCT(1).
           MOVE QAL-ACCT-2 TO AL-ACCT(2).
           MOVE QAL-PCT-2 TO AL-PCT(2).
           MOVE QAL-ACCT-3 TO AL-ACCT(3).
           MOVE QAL-PCT-3 TO AL-PCT(3).
           MOVE QAL-ACCT-4 TO AL-ACCT(4).
           MOVE QAL-PCT-4 TO AL-PCT(4).
           MOVE QAL-ACCT-5 TO AL-ACCT(5).
           MOVE QAL-PCT-5 TO AL-PCT(5).
           MOVE QAL-ACCT-6 TO AL-ACCT(6).
           MOVE QAL-PCT-6 TO AL-PCT(6).
           MOVE QAL-ACCT-7 TO AL-ACCT(7).
           MOVE QAL-PCT-7 TO AL-PCT(7).
           MOVE QAL-ACCT-8 TO AL-ACCT(8).
           MOVE QAL-PCT-8 TO AL-PCT(8).
           MOVE QAL-ACCT-9 TO AL-ACCT(9).
           MOVE QAL-PCT-9 TO AL-PCT(9).
           MOVE QAL-ACCT-10 TO AL-ACCT(10).
           MOVE QAL-PCT-10 TO AL-PCT(10).
           MOVE QAL-TYPE TO AL-TYPE.
 
      *-----------------------------------------------------------------
      *    LOAD QAL-REC FROM AL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-AL-FIELDS.
           MOVE AL-ID TO QAL-ID.

           IF ( AL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QAL-LTOUCH-DATE
           ELSE
              MOVE AL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAL-LTOUCH-DATE.

           MOVE AL-DESC TO QAL-DESC.
           MOVE AL-NO-ACCTS TO QAL-NO-ACCTS.
           MOVE AL-ACCT(1) TO QAL-ACCT-1.
           MOVE AL-PCT(1) TO QAL-PCT-1.
           MOVE AL-ACCT(2) TO QAL-ACCT-2.
           MOVE AL-PCT(2) TO QAL-PCT-2.
           MOVE AL-ACCT(3) TO QAL-ACCT-3.
           MOVE AL-PCT(3) TO QAL-PCT-3.
           MOVE AL-ACCT(4) TO QAL-ACCT-4.
           MOVE AL-PCT(4) TO QAL-PCT-4.
           MOVE AL-ACCT(5) TO QAL-ACCT-5.
           MOVE AL-PCT(5) TO QAL-PCT-5.
           MOVE AL-ACCT(6) TO QAL-ACCT-6.
           MOVE AL-PCT(6) TO QAL-PCT-6.
           MOVE AL-ACCT(7) TO QAL-ACCT-7.
           MOVE AL-PCT(7) TO QAL-PCT-7.
           MOVE AL-ACCT(8) TO QAL-ACCT-8.
           MOVE AL-PCT(8) TO QAL-PCT-8.
           MOVE AL-ACCT(9) TO QAL-ACCT-9.
           MOVE AL-PCT(9) TO QAL-PCT-9.
           MOVE AL-ACCT(10) TO QAL-ACCT-10.
           MOVE AL-PCT(10) TO QAL-PCT-10.
           MOVE AL-TYPE TO QAL-TYPE.
 
