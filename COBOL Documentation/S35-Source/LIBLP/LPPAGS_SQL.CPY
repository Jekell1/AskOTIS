      * COPYMEMBER: LIBLP/LPPAGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - PAYEE FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD PA-REC FROM QPA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-PA-FIELDS.
           MOVE QPA-BRNO TO PA-BRNO.
           MOVE QPA-NUMBER TO PA-NUMBER.
           MOVE QPA-NAME TO PA-NAME.
           MOVE QPA-ADR1 TO PA-ADR1.
           MOVE QPA-ADR2 TO PA-ADR2.
           MOVE QPA-CITY TO PA-CITY.
           MOVE QPA-STATE TO PA-STATE.
           MOVE QPA-ZIP TO PA-ZIP.
           MOVE QPA-LCHECK TO PA-LCHECK.

           IF ( QPA-LCDATE = "1900-01-01" )
              MOVE ZEROES TO PA-LCDATE
           ELSE
              MOVE QPA-LCDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PA-LCDATE.

           MOVE QPA-LAMT TO PA-LAMT.
           MOVE QPA-YTD-PAID TO PA-YTD-PAID.
           MOVE QPA-LYTD-PAID TO PA-LYTD-PAID.
           MOVE QPA-SORTKEY TO PA-SORTKEY.
           MOVE QPA-CUSNO TO PA-CUSNO.
           MOVE QPA-CONTACT TO PA-CONTACT.
           MOVE QPA-PHONE TO PA-PHONE.
           MOVE QPA-FAX TO PA-FAX.
           MOVE QPA-1099-FLAG TO PA-1099-FLAG.
           MOVE QPA-1099-ID TO PA-1099-ID.
           MOVE QPA-LEGAL-NAME1 TO PA-LEGAL-NAME1.
           MOVE QPA-LEGAL-NAME2 TO PA-LEGAL-NAME2.
           MOVE QPA-VAT-PERC TO PA-VAT-PERC.
           MOVE QPA-RFC TO PA-RFC.
           MOVE QPA-CURP TO PA-CURP.

           IF ( QPA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PA-LTOUCH-DATE
           ELSE
              MOVE QPA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PA-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QPA-REC FROM PA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-PA-FIELDS.
           MOVE PA-BRNO TO QPA-BRNO.
           MOVE PA-NUMBER TO QPA-NUMBER.
           MOVE PA-NAME TO QPA-NAME.
           MOVE PA-ADR1 TO QPA-ADR1.
           MOVE PA-ADR2 TO QPA-ADR2.
           MOVE PA-CITY TO QPA-CITY.
           MOVE PA-STATE TO QPA-STATE.
           MOVE PA-ZIP TO QPA-ZIP.
           MOVE PA-LCHECK TO QPA-LCHECK.

           IF ( PA-LCDATE = ZEROES )
              MOVE "1900-01-01" TO QPA-LCDATE
           ELSE
              MOVE PA-LCDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPA-LCDATE.

           MOVE PA-LAMT TO QPA-LAMT.
           MOVE PA-YTD-PAID TO QPA-YTD-PAID.
           MOVE PA-LYTD-PAID TO QPA-LYTD-PAID.
           MOVE PA-SORTKEY TO QPA-SORTKEY.
           MOVE PA-CUSNO TO QPA-CUSNO.
           MOVE PA-CONTACT TO QPA-CONTACT.
           MOVE PA-PHONE TO QPA-PHONE.
           MOVE PA-FAX TO QPA-FAX.
           MOVE PA-1099-FLAG TO QPA-1099-FLAG.
           MOVE PA-1099-ID TO QPA-1099-ID.
           MOVE PA-LEGAL-NAME1 TO QPA-LEGAL-NAME1.
           MOVE PA-LEGAL-NAME2 TO QPA-LEGAL-NAME2.
           MOVE PA-VAT-PERC TO QPA-VAT-PERC.
           MOVE PA-RFC TO QPA-RFC.
           MOVE PA-CURP TO QPA-CURP.

           IF ( PA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPA-LTOUCH-DATE
           ELSE
              MOVE PA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPA-LTOUCH-DATE.

 
