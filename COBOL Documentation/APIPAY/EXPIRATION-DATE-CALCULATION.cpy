      *================================================================*
      * END COPYBOOK: LIBLP\LPRATE.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPXDTE.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPXDTE
      **************************************************************
      *         EXPIRATION DATE CALCULATION
      *
      *   NAME: LPXDTE
      *   DESC: COMPUTES THE EXPIRATION DATE OF THE LOAN.
      *   IN  :
      *   OUT : XDTE-DATE
      *   USED: SP-EXREDFAC, SP-EXPD-FRMLA
      *   COPY: LPXDTEW, LPNDTE, LPNDTEW, LPMDTE, LPMDTEW.
      *
      * REV:
      *  JTG 032188 ADDED LOGIC FOR OPTIONAL EXPIRATION
      *             DATES VIA. SP-EXPD-FRMLA
      *  SLC 042888 TOOK OUT OPTION FOR SP-EXPD-FRMLA = "B".
      *             ADDED LOGIC TO INCLUDE EXP. DAYS.
      *  JTG 071896 CHANGED TO WORK WITH NEW DATE ROUTINES
      *  BAH 050218 ADDED SP-EXPD-FRMLA = "B", LENDMARK PL# 463
      *             DONT ALLOW DUE DATE CHANGES TO AFFECT MATURITY
      **************************************************************
       EXPIRATION-DATE-CALCULATION SECTION.
           IF SP-EXPD-FRMLA = "A" OR "B"
              MOVE "Y" TO MDTE-FLAG.
           IF SP-EXPD-FRMLA = "B"
              MOVE "Y" TO MDTE-DC-FLAG.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO NDTE-DATE.
           MOVE SP-EXPD-MONTHS TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

           IF SP-EXPD-DAYS NOT = 0
              MOVE SP-EXPD-DAYS TO NDTE-HOLD
              PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO XDTE-DATE.
      *================================================================*
      * END COPYBOOK: LIBLP\LPXDTE.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLCAS.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLCAS
      **********************************************************************
      *         LATE CHARGE ASSESS ROUTINE
      *
      *   NAME: LPLCAS
      *   DESC: THIS ROUTINE TESTS, BASED ON THE PAYDATE,
      *         IF LATE CHARGES SHOULD BE ASSESSED. IT
      *         CONSIDERS GRACE DAYS AND CURRENT LATE
      *         CHARGE PAID THRU.
      *   IN  : LCAS-PAYDATE       (LP-PAYDATE)
      *         LCAS-1STPYDATE     (LN-1STPYDATE)
      *         LCAS-LCPDTH-DATE        (LN-LCPDTH)
      *   OUT : LCAS-ASSESS        ("Y" OR " ")
      *         LCAS-ELAPSED-DAYS
      *         LCAS-GRACE-DATE
      *   USED: LN-ORGTERM, LN-DEFMNTS
      *   COPY: LPMDTEW, LPNDTE, LPNDTEW, TIMALL, JUL, CJUL.
      * REV:
      *  SLC 090889 EXTENDED LAST GRACE DAY TO NEXT BUSINESS DAY
      *             IF IT FALLS ON SATURDAY, SUNDAY, OR A HOLIDAY
      *             FROM THE HOLIDAY TABLE IN THE GLOBAL.
      *  SLC 121289 ADDED ON OVERFLOW CHECK WHEN CALLING GLOBRD.
      *  JTG 120590 ADDED OUTPUT OF LCAS-ELAPSED-DAYS
      *  JTG 060592 ADDED LOGIC FOR LN-LC-CODE (ROSE SHANIS)
      *  JTG 080795 CORRECTED FOR HOLIDAY/WEEKEND TEST NEVER ENDING LOOP
      *  JTG 071696 ADDED LOGIC FOR UNIT PERIODS
      *  JTG 103096 CHANGED TEST ON LN-LC-CODEX TO TEST FOR "00", WHICH
      *             SHOULD ELIMINATE UNNECESSARY IO ON EL-FILE
      *  JTG 030497 CHANGED TO CORRECT, DUE DAY = 30TH & LCPDTH = 02289X
      *             CAUSING NEXT DUE TO BE 032897 AND NOT 033097
      *  JTG 122198 ADDED LOGIC TO RETURN LCAS-GRACE-DATE, REGENCY REVOLVING
      *  JTG 990121 CORRECTED LOGIC TO RETURN LCAS-GRACE-DATE, REGENCY REVOLVING
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      * BAH 20220818 REMOVED CALL TO GLOBRD, EACH PROGRAM WILL READ GBFILE NOW
      **************************************************************************
      *******************************************
