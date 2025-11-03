      * COPYMEMBER: LIBSP/SPBBGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - BBFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD BB-REC FROM QBB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BB-FIELDS.
           MOVE QBB-CATAGORY TO BB-CATAGORY.
           MOVE QBB-SUBCATAGORY TO BB-SUBCATAGORY.
           MOVE QBB-USERID TO BB-USERID.
           MOVE QBB-SEQNO TO BB-SEQNO.
           MOVE QBB-NAME TO BB-NAME.
           MOVE QBB-LOGNAME TO BB-LOGNAME.
           MOVE QBB-POSTNAME TO BB-POSTNAME.
           MOVE QBB-OTHERBR TO BB-OTHERBR.

           IF ( QBB-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO BB-POSTDATE
           ELSE
              MOVE QBB-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BB-POSTDATE.

           MOVE QBB-POSTTIME TO BB-POSTTIME.
           MOVE QBB-PASSWORD TO BB-PASSWORD.
           MOVE QBB-GROUP TO BB-GROUP.
           MOVE QBB-BRNO TO BB-BRNO.
           MOVE QBB-ACTION TO BB-ACTION.
           MOVE QBB-MEMO-LINE-1 TO BB-MEMO-LINE(1).
           MOVE QBB-MEMO-LINE-2 TO BB-MEMO-LINE(2).
           MOVE QBB-MEMO-LINE-3 TO BB-MEMO-LINE(3).
           MOVE QBB-MEMO-LINE-4 TO BB-MEMO-LINE(4).
           MOVE QBB-MEMO-LINE-5 TO BB-MEMO-LINE(5).
           MOVE QBB-MEMO-LINE-6 TO BB-MEMO-LINE(6).
           MOVE QBB-MEMO-LINE-7 TO BB-MEMO-LINE(7).
           MOVE QBB-MEMO-LINE-8 TO BB-MEMO-LINE(8).
           MOVE QBB-MEMO-LINE-9 TO BB-MEMO-LINE(9).
           MOVE QBB-MEMO-LINE-10 TO BB-MEMO-LINE(10).

           IF ( QBB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BB-LTOUCH-DATE
           ELSE
              MOVE QBB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BB-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QBB-REC FROM BB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BB-FIELDS.
           MOVE BB-CATAGORY TO QBB-CATAGORY.
           MOVE BB-SUBCATAGORY TO QBB-SUBCATAGORY.
           MOVE BB-USERID TO QBB-USERID.
           MOVE BB-SEQNO TO QBB-SEQNO.
           MOVE BB-NAME TO QBB-NAME.
           MOVE BB-LOGNAME TO QBB-LOGNAME.
           MOVE BB-POSTNAME TO QBB-POSTNAME.
           MOVE BB-OTHERBR TO QBB-OTHERBR.

           IF ( BB-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QBB-POSTDATE
           ELSE
              MOVE BB-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBB-POSTDATE.

           MOVE BB-POSTTIME TO QBB-POSTTIME.
           MOVE BB-PASSWORD TO QBB-PASSWORD.
           MOVE BB-GROUP TO QBB-GROUP.
           MOVE BB-BRNO TO QBB-BRNO.
           MOVE BB-ACTION TO QBB-ACTION.
           MOVE BB-MEMO-LINE(1) TO QBB-MEMO-LINE-1.
           MOVE BB-MEMO-LINE(2) TO QBB-MEMO-LINE-2.
           MOVE BB-MEMO-LINE(3) TO QBB-MEMO-LINE-3.
           MOVE BB-MEMO-LINE(4) TO QBB-MEMO-LINE-4.
           MOVE BB-MEMO-LINE(5) TO QBB-MEMO-LINE-5.
           MOVE BB-MEMO-LINE(6) TO QBB-MEMO-LINE-6.
           MOVE BB-MEMO-LINE(7) TO QBB-MEMO-LINE-7.
           MOVE BB-MEMO-LINE(8) TO QBB-MEMO-LINE-8.
           MOVE BB-MEMO-LINE(9) TO QBB-MEMO-LINE-9.
           MOVE BB-MEMO-LINE(10) TO QBB-MEMO-LINE-10.

           IF ( BB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBB-LTOUCH-DATE
           ELSE
              MOVE BB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBB-LTOUCH-DATE.

 
