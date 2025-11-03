      * COPYMEMBER: LIBUT/UTSTGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - ZMFILE
      *
      *=================================================================
      * REV:
      * BAH 2023-06-23 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD ZMB-REC FROM QZMB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-ZMB-FIELDS.
           MOVE QZMB-CODE               TO ZMB-CODE.
           MOVE QZMB-DESC               TO ZMB-DESC.
           MOVE QZMB-REINSTATE-WINDOW   TO ZMB-REINSTATE-WINDOW.
           MOVE QZMB-REPO-RENOTICE      TO ZMB-REPO-RENOTICE.
           MOVE QZMB-TITLE-RELEASE-TYPE TO ZMB-TITLE-RELEASE-TYPE.
           MOVE QZMB-AUCTION-DATE-REQ   TO ZMB-AUCTION-DATE-REQ.
           MOVE QZMB-GEOGRAPHIC-CODE    TO ZMB-GEOGRAPHIC-CODE.
           MOVE QZMB-MAX-LOAN-AVAIL     TO ZMB-MAX-LOAN-AVAIL.
           MOVE QZMB-MAX-TAX-LOAN-AMT   TO ZMB-MAX-TAX-LOAN-AMT.
           MOVE QZMB-NSF-FEE-AMT        TO ZMB-NSF-FEE-AMT.
           MOVE QZMB-TEST-AMOUNT        TO ZMB-TEST-AMOUNT.     
           MOVE QZMB-TEST-CODE          TO ZMB-TEST-CODE.      
           MOVE QZMB-TEST-DATE          TO ZMB-TEST-DATE.           
           MOVE QZMB-TEST-NUM           TO ZMB-TEST-NUM.          
           MOVE QZMB-TEST-RATE          TO ZMB-TEST-RATE.         
           MOVE QZMB-TEST-SSNO          TO ZMB-TEST-SSNO.
 
      *-----------------------------------------------------------------
      *    LOAD QZMB-REC FROM ZMB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-ZMB-FIELDS.
           MOVE ZMB-CODE                TO QZMB-CODE.
           MOVE ZMB-DESC                TO QZMB-DESC.
           MOVE ZMB-REINSTATE-WINDOW    TO QZMB-REINSTATE-WINDOW.
           MOVE ZMB-REPO-RENOTICE       TO QZMB-REPO-RENOTICE.
           MOVE ZMB-TITLE-RELEASE-TYPE  TO QZMB-TITLE-RELEASE-TYPE.
           MOVE ZMB-AUCTION-DATE-REQ    TO QZMB-AUCTION-DATE-REQ.
           MOVE ZMB-GEOGRAPHIC-CODE     TO QZMB-GEOGRAPHIC-CODE.
           MOVE ZMB-MAX-LOAN-AVAIL      TO QZMB-MAX-LOAN-AVAIL.
           MOVE ZMB-MAX-TAX-LOAN-AMT    TO QZMB-MAX-TAX-LOAN-AMT.
           MOVE ZMB-NSF-FEE-AMT         TO QZMB-NSF-FEE-AMT.
           MOVE ZMB-TEST-AMOUNT         TO QZMB-TEST-AMOUNT.
           MOVE ZMB-TEST-CODE           TO QZMB-TEST-CODE.
           MOVE ZMB-TEST-DATE           TO QZMB-TEST-DATE.
           MOVE ZMB-TEST-NUM            TO QZMB-TEST-NUM.
           MOVE ZMB-TEST-RATE           TO QZMB-TEST-RATE.
           MOVE ZMB-TEST-SSNO           TO QZMB-TEST-SSNO.
