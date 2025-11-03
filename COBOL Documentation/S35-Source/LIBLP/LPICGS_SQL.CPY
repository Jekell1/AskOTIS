      * COPYMEMBER: LIBLP/LPICGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - INSURANCE COMPANY
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD IC-REC FROM QIC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-IC-FIELDS.
           MOVE QIC-BRNO TO IC-BRNO.
           MOVE QIC-CODE TO IC-CODE.

           IF ( QIC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO IC-LTOUCH-DATE
           ELSE
              MOVE QIC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO IC-LTOUCH-DATE.

           MOVE QIC-NAME TO IC-NAME.
           MOVE QIC-ADDR1 TO IC-ADDR1.
           MOVE QIC-ADDR2 TO IC-ADDR2.
           MOVE QIC-CITY TO IC-CITY.
           MOVE QIC-STATE TO IC-STATE.
           MOVE QIC-ZIP TO IC-ZIP.
           MOVE QIC-PHONE TO IC-PHONE.
           MOVE QIC-COMMRATE-1 TO IC-COMMRATE(1).
           MOVE QIC-COMMRATE-2 TO IC-COMMRATE(2).
           MOVE QIC-COMMRATE-3 TO IC-COMMRATE(3).
           MOVE QIC-COMMRATE-4 TO IC-COMMRATE(4).
           MOVE QIC-COMMRATE-5 TO IC-COMMRATE(5).
           MOVE QIC-COMMRATE-6 TO IC-COMMRATE(6).
           MOVE QIC-COMMRATE-7 TO IC-COMMRATE(7).
           MOVE QIC-COMMRATE-8 TO IC-COMMRATE(8).
           MOVE QIC-COMMRATE-9 TO IC-COMMRATE(9).
           MOVE QIC-COMMRATE-10 TO IC-COMMRATE(10).
           MOVE QIC-COMMRATE-11 TO IC-COMMRATE(11).
           MOVE QIC-COMMRATE-12 TO IC-COMMRATE(12).
           MOVE QIC-COMMRATE-13 TO IC-COMMRATE(13).
           MOVE QIC-COMMRATE-14 TO IC-COMMRATE(14).
 
      *-----------------------------------------------------------------
      *    LOAD QIC-REC FROM IC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-IC-FIELDS.
           MOVE IC-BRNO TO QIC-BRNO.
           MOVE IC-CODE TO QIC-CODE.

           IF ( IC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QIC-LTOUCH-DATE
           ELSE
              MOVE IC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QIC-LTOUCH-DATE.

           MOVE IC-NAME TO QIC-NAME.
           MOVE IC-ADDR1 TO QIC-ADDR1.
           MOVE IC-ADDR2 TO QIC-ADDR2.
           MOVE IC-CITY TO QIC-CITY.
           MOVE IC-STATE TO QIC-STATE.
           MOVE IC-ZIP TO QIC-ZIP.
           MOVE IC-PHONE TO QIC-PHONE.
           MOVE IC-COMMRATE(1) TO QIC-COMMRATE-1.
           MOVE IC-COMMRATE(2) TO QIC-COMMRATE-2.
           MOVE IC-COMMRATE(3) TO QIC-COMMRATE-3.
           MOVE IC-COMMRATE(4) TO QIC-COMMRATE-4.
           MOVE IC-COMMRATE(5) TO QIC-COMMRATE-5.
           MOVE IC-COMMRATE(6) TO QIC-COMMRATE-6.
           MOVE IC-COMMRATE(7) TO QIC-COMMRATE-7.
           MOVE IC-COMMRATE(8) TO QIC-COMMRATE-8.
           MOVE IC-COMMRATE(9) TO QIC-COMMRATE-9.
           MOVE IC-COMMRATE(10) TO QIC-COMMRATE-10.
           MOVE IC-COMMRATE(11) TO QIC-COMMRATE-11.
           MOVE IC-COMMRATE(12) TO QIC-COMMRATE-12.
           MOVE IC-COMMRATE(13) TO QIC-COMMRATE-13.
           MOVE IC-COMMRATE(14) TO QIC-COMMRATE-14.
 
