      * COPYMEMBER: LIBLP/LPSTGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - STFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0527 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD ST-REC FROM QST-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-ST-FIELDS.
           MOVE QST-CODE TO ST-CODE.

           MOVE QST-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO ST-LTOUCH-DATE.

           MOVE QST-DESC TO ST-DESC.
           MOVE QST-REINSTATE-WINDOW TO ST-REINSTATE-WINDOW.
           MOVE QST-REPO-RENOTICE TO ST-REPO-RENOTICE.
           MOVE QST-TITLE-RELEASE-TYPE TO ST-TITLE-RELEASE-TYPE.
           MOVE QST-AUCTION-DATE-REQ TO ST-AUCTION-DATE-REQ.
           MOVE QST-GEOGRAPHIC-CODE TO ST-GEOGRAPHIC-CODE.
           MOVE QST-MAX-LOAN-AVAIL TO ST-MAX-LOAN-AVAIL.
           MOVE QST-MAX-TAX-LOAN-AMT TO ST-MAX-TAX-LOAN-AMT.
           MOVE QST-NSF-FEE-AMT TO ST-NSF-FEE-AMT.
 
      *-----------------------------------------------------------------
      *    LOAD QST-REC FROM ST-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-ST-FIELDS.
           MOVE ST-CODE TO QST-CODE.

           MOVE ST-LTOUCH-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QST-LTOUCH-DATE.

           MOVE ST-DESC TO QST-DESC.
           MOVE ST-REINSTATE-WINDOW TO QST-REINSTATE-WINDOW.
           MOVE ST-REPO-RENOTICE TO QST-REPO-RENOTICE.
           MOVE ST-TITLE-RELEASE-TYPE TO QST-TITLE-RELEASE-TYPE.
           MOVE ST-AUCTION-DATE-REQ TO QST-AUCTION-DATE-REQ.
           MOVE ST-GEOGRAPHIC-CODE TO QST-GEOGRAPHIC-CODE.
           MOVE ST-MAX-LOAN-AVAIL TO QST-MAX-LOAN-AVAIL.
           MOVE ST-MAX-TAX-LOAN-AMT TO QST-MAX-TAX-LOAN-AMT.
           MOVE ST-NSF-FEE-AMT TO QST-NSF-FEE-AMT.

