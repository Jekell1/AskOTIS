      * COPYMEMBER: LIBGB/GBSEGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - SEFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD SE-REC FROM QSE-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SE-FIELDS.
           MOVE QSE-FORM TO SE-FORM.

           IF ( QSE-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SE-LTOUCH-DATE
           ELSE
              MOVE QSE-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SE-LTOUCH-DATE.


           IF ( QSE-EXP-DATE = "1900-01-01" )
              MOVE ZEROES TO SE-EXP-DATE
           ELSE
              MOVE QSE-EXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SE-EXP-DATE.

           MOVE QSE-PURCHASABLE TO SE-PURCHASABLE.
           MOVE QSE-PROF-1 TO SE-PROF(1).
           MOVE QSE-PROF-2 TO SE-PROF(2).
           MOVE QSE-PROF-3 TO SE-PROF(3).
           MOVE QSE-PROF-4 TO SE-PROF(4).
           MOVE QSE-PROF-5 TO SE-PROF(5).
           MOVE QSE-PROF-6 TO SE-PROF(6).
           MOVE QSE-PROF-7 TO SE-PROF(7).
           MOVE QSE-PROF-8 TO SE-PROF(8).
           MOVE QSE-PROF-9 TO SE-PROF(9).
           MOVE QSE-PROF-10 TO SE-PROF(10).
           MOVE QSE-PROF-11 TO SE-PROF(11).
           MOVE QSE-PROF-12 TO SE-PROF(12).
           MOVE QSE-PROF-13 TO SE-PROF(13).
           MOVE QSE-PROF-14 TO SE-PROF(14).
           MOVE QSE-PROF-15 TO SE-PROF(15).
           MOVE QSE-PROF-16 TO SE-PROF(16).
           MOVE QSE-PROF-17 TO SE-PROF(17).
           MOVE QSE-PROF-18 TO SE-PROF(18).
           MOVE QSE-PROF-19 TO SE-PROF(19).
           MOVE QSE-PROF-20 TO SE-PROF(20).
           MOVE QSE-PROF-21 TO SE-PROF(21).
           MOVE QSE-PROF-22 TO SE-PROF(22).
           MOVE QSE-PROF-23 TO SE-PROF(23).
           MOVE QSE-PROF-24 TO SE-PROF(24).
           MOVE QSE-PROF-25 TO SE-PROF(25).
           MOVE QSE-PROF-26 TO SE-PROF(26).
 
      *-----------------------------------------------------------------
      *    LOAD QSE-REC FROM SE-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SE-FIELDS.
           MOVE SE-FORM TO QSE-FORM.

           IF ( SE-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSE-LTOUCH-DATE
           ELSE
              MOVE SE-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSE-LTOUCH-DATE.


           IF ( SE-EXP-DATE = ZEROES )
              MOVE "1900-01-01" TO QSE-EXP-DATE
           ELSE
              MOVE SE-EXP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSE-EXP-DATE.

           MOVE SE-PURCHASABLE TO QSE-PURCHASABLE.
           MOVE SE-PROF(1) TO QSE-PROF-1.
           MOVE SE-PROF(2) TO QSE-PROF-2.
           MOVE SE-PROF(3) TO QSE-PROF-3.
           MOVE SE-PROF(4) TO QSE-PROF-4.
           MOVE SE-PROF(5) TO QSE-PROF-5.
           MOVE SE-PROF(6) TO QSE-PROF-6.
           MOVE SE-PROF(7) TO QSE-PROF-7.
           MOVE SE-PROF(8) TO QSE-PROF-8.
           MOVE SE-PROF(9) TO QSE-PROF-9.
           MOVE SE-PROF(10) TO QSE-PROF-10.
           MOVE SE-PROF(11) TO QSE-PROF-11.
           MOVE SE-PROF(12) TO QSE-PROF-12.
           MOVE SE-PROF(13) TO QSE-PROF-13.
           MOVE SE-PROF(14) TO QSE-PROF-14.
           MOVE SE-PROF(15) TO QSE-PROF-15.
           MOVE SE-PROF(16) TO QSE-PROF-16.
           MOVE SE-PROF(17) TO QSE-PROF-17.
           MOVE SE-PROF(18) TO QSE-PROF-18.
           MOVE SE-PROF(19) TO QSE-PROF-19.
           MOVE SE-PROF(20) TO QSE-PROF-20.
           MOVE SE-PROF(21) TO QSE-PROF-21.
           MOVE SE-PROF(22) TO QSE-PROF-22.
           MOVE SE-PROF(23) TO QSE-PROF-23.
           MOVE SE-PROF(24) TO QSE-PROF-24.
           MOVE SE-PROF(25) TO QSE-PROF-25.
           MOVE SE-PROF(26) TO QSE-PROF-26.
 
