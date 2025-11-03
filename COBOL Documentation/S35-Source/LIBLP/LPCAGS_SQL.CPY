      * COPYMEMBER: LIBLP/LPCAGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CAFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CA-REC FROM QCA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CA-FIELDS.
           MOVE QCA-BRNO TO CA-BRNO.

           IF ( QCA-DATE = "1900-01-01" )
              MOVE ZEROES TO CA-DATE
           ELSE
              MOVE QCA-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CA-DATE.

           MOVE QCA-TILLNO TO CA-TILLNO.

           IF ( QCA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CA-LTOUCH-DATE
           ELSE
              MOVE QCA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CA-LTOUCH-DATE.

           MOVE QCA-DESC TO CA-DESC.
           MOVE QCA-BEGBAL TO CA-BEGBAL.
           MOVE QCA-OPENED-HHMM TO CA-OPENED-HHMM.
           MOVE QCA-CLOSED-HHMM TO CA-CLOSED-HHMM.
           MOVE QCA-ENDBAL TO CA-ENDBAL.
           MOVE QCA-BALBY TO CA-BALBY.
           MOVE QCA-RUNNING TO CA-RUNNING.
           MOVE QCA-DEPOSIT TO CA-DEPOSIT.
           MOVE QCA-OPEN-USERID TO CA-OPEN-USERID.
           MOVE QCA-NET-CASH-NOTES TO CA-NET-CASH-NOTES.
           MOVE QCA-PASSWORD TO CA-PASSWORD.
           MOVE QCA-CHECK-DEPOSIT TO CA-CHECK-DEPOSIT.
 
      *-----------------------------------------------------------------
      *    LOAD QCA-REC FROM CA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CA-FIELDS.
           MOVE CA-BRNO TO QCA-BRNO.

           IF ( CA-DATE = ZEROES )
              MOVE "1900-01-01" TO QCA-DATE
           ELSE
              MOVE CA-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCA-DATE.

           MOVE CA-TILLNO TO QCA-TILLNO.

           IF ( CA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCA-LTOUCH-DATE
           ELSE
              MOVE CA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCA-LTOUCH-DATE.

           MOVE CA-DESC TO QCA-DESC.
           MOVE CA-BEGBAL TO QCA-BEGBAL.
           MOVE CA-OPENED-HHMM TO QCA-OPENED-HHMM.
           MOVE CA-CLOSED-HHMM TO QCA-CLOSED-HHMM.
           MOVE CA-ENDBAL TO QCA-ENDBAL.
           MOVE CA-BALBY TO QCA-BALBY.
           MOVE CA-RUNNING TO QCA-RUNNING.
           MOVE CA-DEPOSIT TO QCA-DEPOSIT.
           MOVE CA-OPEN-USERID TO QCA-OPEN-USERID.
           MOVE CA-NET-CASH-NOTES TO QCA-NET-CASH-NOTES.
           MOVE CA-PASSWORD TO QCA-PASSWORD.
           MOVE CA-CHECK-DEPOSIT TO QCA-CHECK-DEPOSIT.
