      * COPYMEMBER: LIBLP/LPLSCLJ

      ***********************************************************************
      *    LPLSCL    LOAN STATUS INFORMATION CLEAR ROUTINE. JUDGMENT
      *
      * REV:
      *  BAH 120403 ADDED CLEAR OF "J" RECORD CITIZENS PR# 3990
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190506 CLEAR FUTURE FIELDS
      * AMM 20210429 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-LSJ-FILE SECTION.

           MOVE SPACES TO LSJ-REC.
           MOVE 0 TO LSJ-BRNO LSJ-ACCTNO LSJ-SEQNO LSJ-LTOUCH-DATE.
           MOVE 0 TO LSJ-FILEDATE LSJ-AMT LSJ-CLERK-PHONE
                     LSJ-SHERIFF-FEE LSJ-COURT-FEE LSJ-ATTY-FEE
                     LSJ-DISMISS-DATE LSJ-GARN-DATE LSJ-ATTY-PHONE.
      


