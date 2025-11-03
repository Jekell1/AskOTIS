      * COPYMEMBER: LIBLP/LPDMGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - DMFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD DM-REC FROM QDM-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-DM-FIELDS.
           MOVE QDM-BRNO TO DM-BRNO.
           MOVE QDM-DL-DLNO TO DM-DL-DLNO.

           MOVE QDM-DATE-REVERSED TO DM-DATE-REVERSED.
           MOVE QDM-SEQ TO DM-SEQ.

           IF ( QDM-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO DM-LTOUCH-DATE
           ELSE
              MOVE QDM-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DM-LTOUCH-DATE.

           MOVE QDM-LTOUCH-TIME-HHMM TO DM-LTOUCH-TIME-HHMM.
           MOVE QDM-LTOUCH-USERID TO DM-LTOUCH-USERID.
           MOVE QDM-MEMO TO DM-MEMO.
           MOVE QDM-MEMO-TYPE TO DM-MEMO-TYPE.
 
      *-----------------------------------------------------------------
      *    LOAD QDM-REC FROM DM-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-DM-FIELDS.
           MOVE DM-BRNO TO QDM-BRNO.
           MOVE DM-DL-DLNO TO QDM-DL-DLNO.

           MOVE DM-DATE-REVERSED TO QDM-DATE-REVERSED.
           MOVE DM-SEQ TO QDM-SEQ.

           IF ( DM-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QDM-LTOUCH-DATE
           ELSE
              MOVE DM-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDM-LTOUCH-DATE.

           MOVE DM-LTOUCH-TIME-HHMM TO QDM-LTOUCH-TIME-HHMM.
           MOVE DM-LTOUCH-USERID TO QDM-LTOUCH-USERID.
           MOVE DM-MEMO TO QDM-MEMO.
           MOVE DM-MEMO-TYPE TO QDM-MEMO-TYPE.
 
