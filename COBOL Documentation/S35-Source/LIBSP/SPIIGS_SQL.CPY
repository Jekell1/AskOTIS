      * COPYMEMBER: LIBSP/SPIIGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - IIFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD II-REC FROM QII-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-II-FIELDS.
           MOVE QII-KEY TO II-KEY.

           IF ( QII-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO II-LTOUCH-DATE
           ELSE
              MOVE QII-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO II-LTOUCH-DATE.

           MOVE QII-EMPLOYER-ID TO II-EMPLOYER-ID.
           MOVE QII-NAME-CONTROL-NO TO II-NAME-CONTROL-NO.
           MOVE QII-TRANS-CONTROL-CODE TO II-TRANS-CONTROL-CODE.
           MOVE QII-FOREIGN-CORP TO II-FOREIGN-CORP.
           MOVE QII-TRANSFER-AGENT-FG TO II-TRANSFER-AGENT-FG.
           MOVE QII-PAYER-NAME TO II-PAYER-NAME.
           MOVE QII-SECOND-PAYER TO II-SECOND-PAYER.
           MOVE QII-PAYER-ADDRESS TO II-PAYER-ADDRESS.
           MOVE QII-PAYER-CITY TO II-PAYER-CITY.
           MOVE QII-PAYER-STATE TO II-PAYER-STATE.
           MOVE QII-PAYER-ZIP TO II-PAYER-ZIP.
           MOVE QII-PAYER-PHONE TO II-PAYER-PHONE.
           MOVE QII-PAYER-EXT TO II-PAYER-EXT.
           MOVE QII-CONTACT TO II-CONTACT.
           MOVE QII-CONTACT-EMAIL TO II-CONTACT-EMAIL.
           MOVE QII-TAX-YEAR TO II-TAX-YEAR.
           MOVE QII-CURRENT-LAST TO II-CURRENT-LAST.
           MOVE QII-LASER-FORM TO II-LASER-FORM.
 
      *-----------------------------------------------------------------
      *    LOAD QII-REC FROM II-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-II-FIELDS.
           MOVE II-KEY TO QII-KEY.

           IF ( II-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QII-LTOUCH-DATE
           ELSE
              MOVE II-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QII-LTOUCH-DATE.

           MOVE II-EMPLOYER-ID TO QII-EMPLOYER-ID.
           MOVE II-NAME-CONTROL-NO TO QII-NAME-CONTROL-NO.
           MOVE II-TRANS-CONTROL-CODE TO QII-TRANS-CONTROL-CODE.
           MOVE II-FOREIGN-CORP TO QII-FOREIGN-CORP.
           MOVE II-TRANSFER-AGENT-FG TO QII-TRANSFER-AGENT-FG.
           MOVE II-PAYER-NAME TO QII-PAYER-NAME.
           MOVE II-SECOND-PAYER TO QII-SECOND-PAYER.
           MOVE II-PAYER-ADDRESS TO QII-PAYER-ADDRESS.
           MOVE II-PAYER-CITY TO QII-PAYER-CITY.
           MOVE II-PAYER-STATE TO QII-PAYER-STATE.
           MOVE II-PAYER-ZIP TO QII-PAYER-ZIP.
           MOVE II-PAYER-PHONE TO QII-PAYER-PHONE.
           MOVE II-PAYER-EXT TO QII-PAYER-EXT.
           MOVE II-CONTACT TO QII-CONTACT.
           MOVE II-CONTACT-EMAIL TO QII-CONTACT-EMAIL.
           MOVE II-TAX-YEAR TO QII-TAX-YEAR.
           MOVE II-CURRENT-LAST TO QII-CURRENT-LAST.
           MOVE II-LASER-FORM TO QII-LASER-FORM.
 
