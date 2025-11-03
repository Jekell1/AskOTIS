      * COPYMEMBER: LIBLP/LPTRDCL

      ***********************************************************************
      *    LPTRDCL    DEPOSIT TRANSACTION FILE CLEAR ROUTINE
      *
      * REV:
      *   MJD 062895 ADDED TD-POSTTIME, TD-POSTDATE.
      *   BLM 160929 ADDED TD-CHECK-DEPOSIT, WORLD PR# 1188
      *   BAH 20210426 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-TRD-FILE SECTION.
           MOVE SPACES TO TRD-REC.

           MOVE 0 TO TD-DATE TD-BRNO
                     TD-DEPOSIT TD-BANK TD-CASH
                     TD-GLOVERSHORT TD-OVERSHORT
                     TD-POSTTIME TD-POSTDATE
                     TD-CHECK-DEPOSIT TD-LTOUCH-DATE.

