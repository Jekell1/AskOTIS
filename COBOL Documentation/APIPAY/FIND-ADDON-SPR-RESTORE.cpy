      ********************************************************
      *    TEST FOR RESTORE OF LOANS SPR RECORD
      ********************************************************
       FIND-ADDON-SPR-RESTORE SECTION.
           IF LN-ADDON-FG = "Y"
              IF ADDONSPR-SP1-KEY NOT = " "
                 IF SP1-KEY NOT = ADDONSPR-SP1-KEY
                    MOVE ADDONSPR-SP1-KEY TO SP1-KEY
                    PERFORM READ-SP1-FILE
                    IF IO-FG = 9
                       GO TO IO-ERROR
                    ELSE
      * IF YOU FIND THE ORIGINAL SPR, MAKE SURE THE SAVED CONTRFRMLA
      * IS RESTORED, CRNO COULD HAVE CHANGED IT SO WE HAVE TO SAVE IT
                       MOVE ADDONSPR-SP-CONTRFRMLA TO SP-CONTRFRMLA.
      *================================================================*
      * END COPYBOOK: LIBLP\ADDONSPR.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLPCL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLPCL
      ******************************************************************
      *
      * DESCRIPTION: LOAN PAYMENT FILE CLEAR ROUTINE
      *
      * ***NOTE*** : ANY CHANGES TO THE CLEAR ROUTINE NEEDS TO BE DONE
      *              TO THE CLEAR ROUTINE FROM THE MP-REC [LIBLP/LPMPCL]
      *
      *=================================================================
      * REV:
      * 021898 BAH ADDED LP-PREPAY-PENALTY LP-PRORATED-INS-REB
      * BAH 062598 CHANGED LP-TILLNO TO ALPHA, REMOVED THE MOVE 0
      *  CS 081998 ADDED LP-POSTING-BR
      * MJD 000119 ADDED LP-NOLCHG (WORLD PR#218)
      * JTG 010227 ADDED LP-EXTRA-PRIN-LFP LP-REMAIN-INT-SINCE-LFP
      *            LP-REMAIN-PRIN-SINCE-LFP  (LFP=LAST-FULL-PAYMENT)
      *            LENDMARK, PR#1045 & PR#1380
      * BLM 100305 ADDED LP-APPLIED-TO-ESCROW, REGWAN PR# 3506
      *  CS 151208 ADD LP-ADDON-INTPDTH, LP-BRNO(DISPLAY ONLY)
      *  CS 160201 ADD LP-FROMCL, LP-TOCL USED IN CLASS TRANSFER
      *
      * KEC 2017.0120 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED NOTE OF NEEDING TO UPDATE THE CLEAR ROUTINE
      *         FOR THE MP-REC [LIBLP/LPMPCL] IF ADDING TO HERE.
      *  CS 190502 CLEAR FUTURE FIELDS
      * BAH 20190628 REMOVED LP-RT-SEQNO REVOLVING
      * BAH 20210427 REMOVED FUTURE FIELDS
      * JKC 20220803 RESTORED MOVING ZEROES TO LP-TILLNO DUE TO GETTING
      *         SQL ERROR "NUMERIC VALUE OUT OF RANGE".
      * BAH 20240212 ADDED LP-REPAY-TRANS-ID #1641
      * BAH 2025.0501 WAS MISSING LP-ESCROW-PREPAID S35Q-205
      ******************************************************************
