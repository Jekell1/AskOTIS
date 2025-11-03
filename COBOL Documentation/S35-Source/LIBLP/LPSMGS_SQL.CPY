      * COPYMEMBER: LIBLP/LPSMGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - SMFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD SM-REC FROM QSM-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SM-FIELDS.
           MOVE QSM-SP-ORGST TO SM-SP-ORGST.
           MOVE QSM-SP-SPRCLASS TO SM-SP-SPRCLASS.
           MOVE QSM-SP-SUBCLASS TO SM-SP-SUBCLASS.
           MOVE QSM-SP-LAWCODE TO SM-SP-LAWCODE.

           MOVE QSM-DATE-REVERSED TO SM-DATE-REVERSED.
           MOVE QSM-SEQ TO SM-SEQ.

           IF ( QSM-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SM-LTOUCH-DATE
           ELSE
              MOVE QSM-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SM-LTOUCH-DATE.

           MOVE QSM-LTOUCH-TIME-HHMM TO SM-LTOUCH-TIME-HHMM.
           MOVE QSM-LTOUCH-USERID TO SM-LTOUCH-USERID.
           MOVE QSM-MEMO1 TO SM-MEMO1.
           MOVE QSM-MEMO2 TO SM-MEMO2.
           MOVE QSM-MEMO-TYPE TO SM-MEMO-TYPE.
 
      *-----------------------------------------------------------------
      *    LOAD QSM-REC FROM SM-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SM-FIELDS.
           MOVE SM-SP-ORGST TO QSM-SP-ORGST.
           MOVE SM-SP-SPRCLASS TO QSM-SP-SPRCLASS.
           MOVE SM-SP-SUBCLASS TO QSM-SP-SUBCLASS.
           MOVE SM-SP-LAWCODE TO QSM-SP-LAWCODE.

           MOVE SM-DATE-REVERSED TO QSM-DATE-REVERSED.
           MOVE SM-SEQ TO QSM-SEQ.

           IF ( SM-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSM-LTOUCH-DATE
           ELSE
              MOVE SM-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSM-LTOUCH-DATE.

           MOVE SM-LTOUCH-TIME-HHMM TO QSM-LTOUCH-TIME-HHMM.
           MOVE SM-LTOUCH-USERID TO QSM-LTOUCH-USERID.
           MOVE SM-MEMO1 TO QSM-MEMO1.
           MOVE SM-MEMO2 TO QSM-MEMO2.
           MOVE SM-MEMO-TYPE TO QSM-MEMO-TYPE.
 
