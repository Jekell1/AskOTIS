      * COPYMEMBER: LIBSP/SPMEMO
      ******************************************************************
      *    M E M O    M O D U L E
      *
      *    COPIED BY SP/SPINQ1.CX, WI/MEMOSC
      *    COMPILE SP/SPINQ1, SP/XPINQ1, WI/MEMOSC
      *    NOTE!!!!!!!!!!!!!!!!!!!
      *    SPINQ, XPINQ USE FORM SP/SPINQ
      *    MEMOSC USES FORM MEMOSC
      * REV:
      *  BLV 110496 ADD OPTION FOR ENTER PRIORITY MEMOS
      *  BLV 121196 ADD BORROWER MEMOS
      *  BLV 012097 NON-NUMERIC BUG IN NEXT MEMO SEQ
      *  BLV 012297 DON'T LOCK LM-FILE; RE-READ BEFORE REWRITE
      *  MJD 012797 ADDED PASSWORD TO AUDIT/DELETE MEMOS OTHER THAN TODAY
      *  GLF 102098 ADDED NOTES AND INVESTOR MEMOS
      *  BLV 991015 NEED LOGIC TO TOGGLE TO DEALER MEMOS BASED ON
      *             GP FLAG, TFC PR# 40
      *  MJD 001003 ADD3ED A NEW MEMO TYPE "R" FOR REVERSALS
      *             (BASED ON GP-RV-MEMO) (WORLD PR#???)
      *  BAH 021024 ADDED "T" FOR COLLATERAL TITLE MEMOS (LTSCAN)
      *  BLM 120713 STARRED OFF SEND-HEAD-3 ROUTINE.  THIS ROUTINE SENDS
      *             "LOAN MEMOS" OR "BORROWER MEMOS" OR WHATEVER TO THE
      *             FIELD TITLE.  THIS WAS A NON-DISPLAY IN COLLECTIONS.
      *             SO JUST PUT DUMMY PARAGRAPH "SEND-HEAD-3" IN SPINQ0,
      *             WILL NEED TO ADD REAL ROUTINE TO WI/MEMOSC.
      ******************************************************************
       MEMO-MODULE SECTION.
       MEMO-RESET.
           PERFORM LM-MEMO-RESET-LINE1-KEY.
           MOVE EXT-WI-CL-TRANS-DATE TO MEMO-HOLD-SYS.
           PERFORM LM-MEMO-SCAN.
           PERFORM LM-MEMO-MODULE.
           IF VDUSTATUS NOT = "1<"
              GO TO EXIT-MEMO-MODULE.
           PERFORM BM-MEMO-RESET-LINE1-KEY.
           PERFORM BM-MEMO-SCAN.
           PERFORM BM-MEMO-MODULE.
           IF VDUSTATUS NOT = "1<"
              GO TO EXIT-MEMO-MODULE.
           IF GP-DL-MEMO-TOGGLE NOT = "Y"
              GO TO MEMO-RESET.
           PERFORM DM-MEMO-RESET-LINE1-KEY.
           PERFORM DM-MEMO-SCAN.
           PERFORM DM-MEMO-MODULE.
           IF VDUSTATUS NOT = "1<"
              GO TO EXIT-MEMO-MODULE
           ELSE
              GO TO MEMO-RESET.
       EXIT-MEMO-MODULE.
           EXIT.

      *MMDDYY-TO-YYMMDD SECTION.
      *    MOVE NUM-YR TO DTE-HOLD.
      *    MOVE NUM-DA TO NUM-YR.
      *    MOVE NUM-MO TO NUM-DA.
      *    MOVE DTE-HOLD TO NUM-MO.
      *YYMMDD-TO-MMDDYY SECTION.
      *    MOVE NUM-MO TO DTE-HOLD.
      *    MOVE NUM-DA TO NUM-MO.
      *    MOVE NUM-YR TO NUM-DA.
      *    MOVE DTE-HOLD TO NUM-YR.
       SET-MEMO-WHICH SECTION.
           IF MEMOSC-MEMO-TYPE = " "
              MOVE "       LOAN MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "J"
              MOVE "    JUDGEMENT MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "X"
              MOVE "   REPOSSESSION MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "L"
              MOVE "      LEGAL MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "D"
              MOVE "    DEBIT AID MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "B"
              MOVE "    BANKRUPTCY MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "T"
              MOVE "    COLLATERAL MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "A"
              MOVE "AUTO INSURANCE MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "N"
              MOVE "         NOTES MEMO" TO MEMO-WHICH
           ELSE
           IF MEMOSC-MEMO-TYPE = "R"
              MOVE "      REVERSAL MEMO" TO MEMO-WHICH.
      *SEND-HEAD-3 SECTION.
      *    MOVE "S  A220TITLE." TO VDU.
      *    MOVE MEMO-WHICH TO VDUBUF.
      *    PERFORM SCREENIO.
      *    MOVE "S  A660HEAD:S." TO VDU.
      *    MOVE HEAD-3 TO VDUBUF.
      *    PERFORM SCREENIO.
