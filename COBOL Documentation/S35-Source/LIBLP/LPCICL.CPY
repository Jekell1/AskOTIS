      * COPYMEMBER: LIBLP/LPCICL

      ***********************************************************************
      *    LPLCCI    CLEAR CUSTOMER INFOMATION FILE
      *
      * REV:
      *  BAH 110131 NEW WORLD PR# 739
      *   CS 190412 CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-CI-FILE SECTION.
           MOVE SPACES TO CI-REC.
           MOVE 0 TO CI-ACCTNO              CI-SSNO CI-BRNO
                     CI-SSNO-2              CI-DATE-UPDATED
                     CI-AMT-REQUESTED       CI-YEARS-IN-AREA
                     CI-DATE-AT-PRESENT-ADR CI-MONTHLY-RENT-MTG
                     CI-FAIR-MARKET-VALUE   CI-MTG-BALANCE
                     CI-TOTAL-MO-INCOME     CI-LESS-MO-PAY
                     CI-DATE-HIRE-1         CI-GROSS-MO-INCOME-1
                     CI-DATE-HIRE-2         CI-GROSS-MO-INCOME-2.
