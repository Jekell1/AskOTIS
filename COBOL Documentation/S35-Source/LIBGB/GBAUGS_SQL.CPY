      * COPYMEMBER: LIBGB/GBAUGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - AUFILE
      *
      *=================================================================
      * REV:
      * BAH 2022-0218 ***NEW*** #1542
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD AU-REC FROM QAU-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-AU-FIELDS.
           MOVE QAU-FILE-NAME TO AU-FILE-NAME.

           IF ( QAU-DATE = "1900-01-01" )
              MOVE ZEROES TO AU-DATE
           ELSE
              MOVE QAU-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO AU-DATE.

           MOVE QAU-SEQ           TO AU-SEQ.
           MOVE QAU-ACTION        TO AU-ACTION.
           MOVE QAU-FILE-TYPE     TO AU-FILE-TYPE.
           MOVE QAU-BRANCH        TO AU-BRANCH.
           MOVE QAU-FILE-KEY      TO AU-FILE-KEY.
           MOVE QAU-FIELD-DESC    TO AU-FIELD-DESC.
           MOVE QAU-ORIG-VALUE    TO AU-ORIG-VALUE.
           MOVE QAU-NEW-VALUE     TO AU-NEW-VALUE.
           MOVE QAU-TICKET        TO AU-TICKET.
           MOVE QAU-PROGRAM       TO AU-PROGRAM.
           MOVE QAU-TIME          TO AU-TIME.
           MOVE QAU-USERID        TO AU-USERID.

           IF ( QAU-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO AU-LTOUCH-DATE
           ELSE
              MOVE QAU-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO AU-LTOUCH-DATE.

      *-----------------------------------------------------------------
      *    LOAD QAU-REC FROM AU-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-AU-FIELDS.
           MOVE AU-FILE-NAME TO QAU-FILE-NAME.

           IF ( AU-DATE = ZEROES )
              MOVE "1900-01-01" TO QAU-DATE
           ELSE
              MOVE AU-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAU-DATE.

           MOVE AU-SEQ           TO QAU-SEQ.
           MOVE AU-ACTION        TO QAU-ACTION.
           MOVE AU-FILE-TYPE     TO QAU-FILE-TYPE.
           MOVE AU-BRANCH        TO QAU-BRANCH.
           MOVE AU-FILE-KEY      TO QAU-FILE-KEY.
           MOVE AU-FIELD-DESC    TO QAU-FIELD-DESC.
           MOVE AU-ORIG-VALUE    TO QAU-ORIG-VALUE.
           MOVE AU-NEW-VALUE     TO QAU-NEW-VALUE.
           MOVE AU-TICKET        TO QAU-TICKET.
           MOVE AU-PROGRAM       TO QAU-PROGRAM.
           MOVE AU-TIME          TO QAU-TIME.
           MOVE AU-USERID        TO QAU-USERID.

           IF ( AU-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QAU-LTOUCH-DATE
           ELSE
              MOVE AU-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAU-LTOUCH-DATE.

