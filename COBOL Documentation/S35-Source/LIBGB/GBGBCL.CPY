      * COPYMEMBER: LIBGB/GBGBCL

      ******************************************************************
      *    GBGBCL    GBFILE CLEAR ROUTINE
      *
      * REV:
      * KEC 051997 ADDED GB-NEXT-APPNO.
      * JTG 082097 ADDED GB-NEXT-GIFTNET-PO
      * BLF 051014 CHANGED GB-NEXT-GIFTNET-PO TO GB-NEXT-RECEIPT
      *  CS 190619 CLEAR GB-EOM-TAG-CCYYMM, GB-FISCAL-EOY-TAG-CCYYMM
      *              GB-CALENDAR-EOY-TAG-CCYYMM.
      * BAH 20190705 REMOVED GB-NEXT-NOTE, GB-NEXT-APPNO, GB-NEXT-RECEIPT
      *  CS 20190709 CLEAR GB-EOM-RUN-DATE,GB-EOM-RUN-TIME,
      *              GB-EOYF-RUN-DATE,GB-EOYF-RUN-TIME,
      *              GB-EOYC-RUN-DATE,GB-EOYC-RUN-TIME
      * BAH 20200519 REMOVED GB-PROGRP-1ST-LN-CLASS AND 2ND-LN-CLASS
      ******************************************************************
       CLEAR-GB-FILE SECTION.
           MOVE SPACES TO GB-REC.
           MOVE 0   TO GB-PHONE GB-NEXT-SF GB-NEXT-SMALL GB-NEXT-OTHER
                       GB-RPT-LINES GB-LAST-ACCRUE.

           PERFORM VARYING GBHOIX FROM 1 BY 1 UNTIL GBHOIX > 16
               MOVE 0 TO GB-HOLIDAY(GBHOIX)
           END-PERFORM.

           MOVE 0 TO GB-EOM-TAG-CCYYMM
                     GB-EOM-UPDATE-DATE
                     GB-EOM-UPDATE-TIME
                     GB-FISCAL-EOY-TAG-CCYYMM
                     GB-EOYF-UPDATE-DATE
                     GB-EOYF-UPDATE-TIME
                     GB-CALENDAR-EOY-TAG-CCYYMM
                     GB-EOYC-UPDATE-DATE
                     GB-EOYC-UPDATE-TIME.

