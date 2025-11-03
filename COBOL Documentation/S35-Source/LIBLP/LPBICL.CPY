      * COPYMEMBER: LIBLP/LPBICL
      ***********************************************************************
      *    LPBICL    MASTER BORROWER INDEX FILE CLEAR ROUTINE
      *
      * REV:
      *  JTG 081694 ADDED LOGIC FOR BI-ENTDATE
      *  BAH 180316 ADDED BI-PLDATE, WORLD PR#1308
      *   CS 190412 ADD CLEAR OF FUTURE FIELDS
      *  AMM 210426 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-BI-FILE SECTION.

           MOVE SPACES TO BI-REC.

           MOVE 0 TO BI-SSNO BI-ACCTNO BI-OWNBR
                     BI-POFFDATE BI-ENTDATE BI-PLDATE
                     BI-LTOUCH-DATE.
      
       CLEAR-BI-FILE-EXIT.
           EXIT.
