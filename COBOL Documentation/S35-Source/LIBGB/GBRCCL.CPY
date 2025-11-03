      * COPYMEMBER: LIBGB/GBRCCL
      ***********************************************************************
      *    GBRCCL    GLOBAL RECAP (DAILY) FILE CLEAR ROUTINE
      *
      * REV:
      *  JTG 022194 ADDED RC-EOM....    FIELDS
      *  BLF 050923 ADDED RC-BANK-BAL
      *  BLF 060216 ADDED CLEAR OF RC-LAST-ACCRUE
      *  BLM 160920 ADDED CLEAR OF RC-CHECK-DEPOSIT, WORLD PR# 1188
      *   CS 180411 ADD CLEAR OF FUTURE-FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-RC-FILE SECTION.
           MOVE SPACES TO RC-REC.

           MOVE 0 TO RC-BRNO RC-TRANS-DATE.

           MOVE 0 TO RC-DEPOSIT RC-OVER-SHORT RC-CHECKS.

           MOVE 0 TO RC-OPENED-DATE
                     RC-OPENED-TIME
                     RC-CLOSED-DATE
                     RC-CLOSED-TIME
                     RC-UPDATED-DATE
                     RC-UPDATED-TIME
                     RC-EOD-BATCH-DATE
                     RC-EOD-BATCH-TIME
                     RC-FUNDS-DATE
                     RC-FUNDS-TIME
                     RC-CHECK-DATE
                     RC-CHECK-TIME
                     RC-EARNINGS-DATE
                     RC-EARNINGS-TIME
                     RC-EOM-UPDATED-DATE
                     RC-EOM-UPDATED-TIME
                     RC-EOM2-BATCH-DATE
                     RC-EOM2-BATCH-TIME
                     RC-BANK-BAL
                     RC-LAST-ACCRUE
                     RC-CHECK-DEPOSIT.

       CLEAR-RC-FILE-EXIT.
           EXIT.
