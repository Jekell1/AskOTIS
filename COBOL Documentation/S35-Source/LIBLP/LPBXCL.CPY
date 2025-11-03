      * COPYMEMBER: LIBLP/LPBXCL
      ********************************************************
      *    LPBXCL    BORROWER TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  030401 BAH NEW LENDMARK PR# 2044
      *  061114 MJD ADDED NEW KYC ("KNOW YOUR CUSTOMER") FIELDS  REGACC PR#2856
      *  190415  CS CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ********************************************************
       CLEAR-BX-FILE SECTION.
           MOVE SPACES TO BX-REC.

           MOVE 0 TO BX-KYC-ISSDATE
                     BX-KYC-EXPDATE
                     BX-KYC-LAST-VERDATE.

