      *================================================================*
      * END COPYBOOK: LIBGB\GBBRGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\GBGBGS_SQL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/GBGBGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - GBFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD GB-REC FROM QGB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GB-FIELDS.
           MOVE QGB-BRNO TO GB-BRNO.
           MOVE QGB-NO TO GB-NO.

           IF ( QGB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-LTOUCH-DATE
           ELSE
              MOVE QGB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-LTOUCH-DATE.

           MOVE QGB-COMPANY-NAME TO GB-COMPANY-NAME.
           MOVE QGB-ADDR-1 TO GB-ADDR-1.
           MOVE QGB-ADDR-2 TO GB-ADDR-2.
           MOVE QGB-CITY TO GB-CITY.
           MOVE QGB-STATE TO GB-STATE.
           MOVE QGB-ZIP TO GB-ZIP.
           MOVE QGB-PHONE TO GB-PHONE.
           MOVE QGB-NEXT-SF TO GB-NEXT-SF.
           MOVE QGB-NEXT-SMALL TO GB-NEXT-SMALL.
           MOVE QGB-NEXT-OTHER TO GB-NEXT-OTHER.
           MOVE QGB-HOLIDAY-1 TO GB-HOLIDAY(1).
           MOVE QGB-HOLIDAY-2 TO GB-HOLIDAY(2).
           MOVE QGB-HOLIDAY-3 TO GB-HOLIDAY(3).
           MOVE QGB-HOLIDAY-4 TO GB-HOLIDAY(4).
           MOVE QGB-HOLIDAY-5 TO GB-HOLIDAY(5).
           MOVE QGB-HOLIDAY-6 TO GB-HOLIDAY(6).
           MOVE QGB-HOLIDAY-7 TO GB-HOLIDAY(7).
           MOVE QGB-HOLIDAY-8 TO GB-HOLIDAY(8).
           MOVE QGB-HOLIDAY-9 TO GB-HOLIDAY(9).
           MOVE QGB-HOLIDAY-10 TO GB-HOLIDAY(10).
           MOVE QGB-HOLIDAY-11 TO GB-HOLIDAY(11).
           MOVE QGB-HOLIDAY-12 TO GB-HOLIDAY(12).
           MOVE QGB-HOLIDAY-13 TO GB-HOLIDAY(13).
           MOVE QGB-HOLIDAY-14 TO GB-HOLIDAY(14).
           MOVE QGB-HOLIDAY-15 TO GB-HOLIDAY(15).
           MOVE QGB-HOLIDAY-16 TO GB-HOLIDAY(16).
           MOVE QGB-RPT-LINES TO GB-RPT-LINES.
           MOVE QGB-CHECK-PROG TO GB-CHECK-PROG.
           MOVE QGB-OTHER-BRANCH-POST TO GB-OTHER-BRANCH-POST.
           MOVE QGB-LAST-ACCRUE TO GB-LAST-ACCRUE.
           MOVE QGB-AUTO-DEPOSIT TO GB-AUTO-DEPOSIT.
           MOVE QGB-UUCP-HOME TO GB-UUCP-HOME.
           MOVE QGB-EOM-TAG-CCYYMM TO GB-EOM-TAG-CCYYMM.

           IF ( QGB-EOM-UPDATE-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-EOM-UPDATE-DATE
           ELSE
              MOVE QGB-EOM-UPDATE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-EOM-UPDATE-DATE.

           MOVE QGB-EOM-UPDATE-TIME TO GB-EOM-UPDATE-TIME.
           MOVE QGB-EOM-UPDATE-USERID TO GB-EOM-UPDATE-USERID.

           MOVE QGB-FISCAL-EOY-TAG-CCYYMM TO GB-FISCAL-EOY-TAG-CCYYMM.

           IF ( QGB-EOYF-UPDATE-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-EOYF-UPDATE-DATE
           ELSE
              MOVE QGB-EOYF-UPDATE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-EOYF-UPDATE-DATE.

           MOVE QGB-EOYF-UPDATE-TIME TO GB-EOYF-UPDATE-TIME.
           MOVE QGB-EOYF-UPDATE-USERID TO GB-EOYF-UPDATE-USERID.

           MOVE QGB-CALENDAR-EOY-TAG-CCYYMM
              TO GB-CALENDAR-EOY-TAG-CCYYMM.

           IF ( QGB-EOYC-UPDATE-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-EOYC-UPDATE-DATE
           ELSE
              MOVE QGB-EOYC-UPDATE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-EOYC-UPDATE-DATE.

           MOVE QGB-EOYC-UPDATE-TIME TO GB-EOYC-UPDATE-TIME.
           MOVE QGB-EOYC-UPDATE-USERID TO GB-EOYC-UPDATE-USERID.

      *-----------------------------------------------------------------
      *    LOAD QGB-REC FROM GB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
