      * COPYMEMBER: LIBLP/LPTRIGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - TRIFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD TRI-REC FROM QTRI-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRI-FIELDS.
           MOVE QTI-BRNO TO TI-BRNO.

           IF ( QTI-KEY-DATE = "1900-01-01" )
              MOVE ZEROES TO TI-KEY-DATE
           ELSE
              MOVE QTI-KEY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TI-KEY-DATE.

           MOVE QTI-SEQ TO TI-SEQ.

           IF ( QTI-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TI-LTOUCH-DATE
           ELSE
              MOVE QTI-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TI-LTOUCH-DATE.

           MOVE QTI-WAIS TO TI-WAIS.
           MOVE QTI-FILLER TO TI-FILLER.
           MOVE QTI-TYPE TO TI-TYPE.
           MOVE QTI-AMOUNT TO TI-AMOUNT.
           MOVE QTI-CLAIM-NUMBER TO TI-CLAIM-NUMBER.
           MOVE QTI-FILLER2 TO TI-FILLER2.
           MOVE QTI-LNAME TO TI-LNAME.
           MOVE QTI-FNAME TO TI-FNAME.

           IF ( QTI-DATE = "1900-01-01" )
              MOVE ZEROES TO TI-DATE
           ELSE
              MOVE QTI-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TI-DATE.

           MOVE QTI-DRAFT TO TI-DRAFT.
           MOVE QTI-FILLER3 TO TI-FILLER3.
           MOVE QTI-PRODUCER TO TI-PRODUCER.
           MOVE QTI-USERID TO TI-USERID.
           MOVE QTI-POSTTIME TO TI-POSTTIME.

           IF ( QTI-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TI-POSTDATE
           ELSE
              MOVE QTI-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TI-POSTDATE.

           MOVE QTI-INSCOMP TO TI-INSCOMP.
           MOVE QTI-DAYS-PAID TO TI-DAYS-PAID.
           MOVE QTI-PMT-TYPE TO TI-PMT-TYPE.

           IF ( QTI-FROM-DATE = "1900-01-01" )
              MOVE ZEROES TO TI-FROM-DATE
           ELSE
              MOVE QTI-FROM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TI-FROM-DATE.


           IF ( QTI-TO-DATE = "1900-01-01" )
              MOVE ZEROES TO TI-TO-DATE
           ELSE
              MOVE QTI-TO-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TI-TO-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QTRI-REC FROM TRI-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRI-FIELDS.
           MOVE TI-BRNO TO QTI-BRNO.

           IF ( TI-KEY-DATE = ZEROES )
              MOVE "1900-01-01" TO QTI-KEY-DATE
           ELSE
              MOVE TI-KEY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTI-KEY-DATE.

           MOVE TI-SEQ TO QTI-SEQ.

           IF ( TI-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTI-LTOUCH-DATE
           ELSE
              MOVE TI-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTI-LTOUCH-DATE.

           MOVE TI-WAIS TO QTI-WAIS.
           MOVE TI-FILLER TO QTI-FILLER.
           MOVE TI-TYPE TO QTI-TYPE.
           MOVE TI-AMOUNT TO QTI-AMOUNT.
           MOVE TI-CLAIM-NUMBER TO QTI-CLAIM-NUMBER.
           MOVE TI-FILLER2 TO QTI-FILLER2.
           MOVE TI-LNAME TO QTI-LNAME.
           MOVE TI-FNAME TO QTI-FNAME.

           IF ( TI-DATE = ZEROES )
              MOVE "1900-01-01" TO QTI-DATE
           ELSE
              MOVE TI-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTI-DATE.

           MOVE TI-DRAFT TO QTI-DRAFT.
           MOVE TI-FILLER3 TO QTI-FILLER3.
           MOVE TI-PRODUCER TO QTI-PRODUCER.
           MOVE TI-USERID TO QTI-USERID.
           MOVE TI-POSTTIME TO QTI-POSTTIME.

           IF ( TI-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTI-POSTDATE
           ELSE
              MOVE TI-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTI-POSTDATE.

           MOVE TI-INSCOMP TO QTI-INSCOMP.
           MOVE TI-DAYS-PAID TO QTI-DAYS-PAID.
           MOVE TI-PMT-TYPE TO QTI-PMT-TYPE.

           IF ( TI-FROM-DATE = ZEROES )
              MOVE "1900-01-01" TO QTI-FROM-DATE
           ELSE
              MOVE TI-FROM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTI-FROM-DATE.


           IF ( TI-TO-DATE = ZEROES )
              MOVE "1900-01-01" TO QTI-TO-DATE
           ELSE
              MOVE TI-TO-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTI-TO-DATE.

 
