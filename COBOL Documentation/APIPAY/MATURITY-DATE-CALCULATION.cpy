      *================================================================*
      * END COPYBOOK: LIBLP\LPLCAP.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPMDTE.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPMDTE
      ********************************************************************
      *         MATURITY DATE CALCULATION
      *
      *   NAME: LPMDTE
      *   DESC: COMPUTES THE MATURITY DATE OF THE LOAN.
      *
      *         N O T E:
      *                 REMEMBER TO CHANGE LPMDTEPD
      *
      *   IN  : MDTE-FLAG
      *         MDTE-ORGTERM-FG
      *
      *         IF MDTE-FLAG         ' '   - UPDATED MATURITY  IS RETURNED
      *                              'Y'   - ORIGINAL MATURITY IS RETURNED
      *                                         (NO DEFERMENTS ADDDED IN)
      *
      *         IF MDTE-ORGTERM-FG   'Y'   - THE LOANS ORIGINAL TERM
      *                                      IS USED IN THE CALCULATIONS
      *                                      INSTEAD OF THE EXTENDED
      *                                      ADDON TERM
      *
      *         IF MDTE-DC-FLAG      ' '   - DUE DATE CHANGES AFFECT MATURITY
      *                                       (USES LN-1STPYDATE)
      *                              'Y'   - DUE DATE CHANGES HAVE NO AFFECT
      *                                       (USED LN-ORIG-1STPYDATE)
      *                 SET TO 'Y' USING SP-EXPD-FRMLA "B"
      *
      *      NOTE:
      *           MDTE-FLAG, MDTE-ORGTERM-FG, AND MDTE-DC-FLAG ARE
      *           CLEARED TO SPACES, UPON EXIT.
      *
      *   OUT : MDTE-DATE
      *   USED: LN-ORGTERM, LN-DEFMNTS, LN-1STPYDATE, LN-SUM-ORGTERM,
      *                                 LN-ORIG-1STPYDATE
      *   COPY: LPMDTEW, LPNDTE, LPNDTEW, INCRMM.
      *
      * REV :
      *  JTG 032188 ADDED LOGIC FOR COMPUTATION OF UPDATED MATURITY
      *             OR ORIGINAL MATURITY
      *  JTG 080990 ADDED USAGE OF LN-SUM-ORGTERM
      *  JTG 071896 ADDED LOGIC FOR UNIT PERIODS
      *  BAH 050218 ADDED MDTE-DC-FLAG SO DUE DATE CHANGES DONT AFFECT
      *             MATURITY DATE BY USING LN-ORIG-1STPYDATE, LENDMARK PL# 463
      ********************************************************************
       MATURITY-DATE-CALCULATION SECTION.

           MOVE LN-1STPYDATE TO NDTE-DATE.

      * SO DUE DATE CHANGES DONT AFFECT MATURITY, LENDMARK PL# 463
           IF MDTE-DC-FLAG = "Y"
              MOVE LN-ORIG-1STPYDATE TO NDTE-DATE.

           MOVE 0 TO NDTE-HOLD.
           IF MDTE-FLAG = " "
              MOVE LN-TOTNODEF TO NDTE-HOLD.

           IF MDTE-ORGTERM-FG = "Y"
              COMPUTE NDTE-HOLD = NDTE-HOLD + (LN-ORGTERM - 1)
           ELSE
              COMPUTE NDTE-HOLD = NDTE-HOLD + (LN-SUM-ORGTERM - 1).
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO MDTE-DATE.
           MOVE " " TO MDTE-FLAG
                       MDTE-ORGTERM-FG
                       MDTE-DC-FLAG.
      *================================================================*
      * END COPYBOOK: LIBLP\LPMDTE.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPITRM.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPITRM
      *************************************************************
      *          INSURANCE TERM CALCULATION
      *                     AND
      *      TEST FOR THE INSURANCE IN EFFECT TODAY
      *
      *   NAME:  LPITRM
      *   DESC:  THIS ROUTINE COMPUTES THE TERM OF THE INSURANCE
      *          FROM THE EFFDATE THRU EXPDATE.
      *          ALSO DETERMINES IF THE INSURANCE IS INEFFECT TODAY.
      *
      *   IN  :  ITRM-SUB         - LN-INSURANCE LEVEL
      *          ITRM-TODAY       - TODAYS DATE
      *             NOTE: IF ITRM-TODAY = 0
      *                   INSURANCE INEFFECT IS NOT TESTED.
      *
      *   OUT :  ITRM-INSTERM     - TERM OF INSURANCE
      *          ITRM-INEFFECT-FG - "Y" INSURANCE INEFFECT
      *
      *   USED:  LN-INSEFF-DATE(ITRM-SUB), LN-INSEXP-DATE(ITRM-SUB)
      *          LN-REBATE(ITRM-SUB1)
      *   COPY:  LPITRMW
      * REV:
      *  JTG 082890 CHANGED TO TEST PREMIUM NOT COVERAGE
      *  BAH 012792 INSURANCE EXPIRES AT 12:01AM THE DAY OF EXPIRATION!
      *  JTG 071295 CHANGED TO CALCULATE ON ALL INSURANCES
      *  JTG 071696 CHANGED TO WORK WITH UNIT PERIODS
      *  BAH 110397 CHANGED TO USE TIM360(LIKE A48) FOR "M" MONTHLY ACCOUNTS,
      *             BECAUSE 10/31/97 TERM 18, 1STPYDATE 11/30/97, MDTE
      *             4/30/99 WAS CAUSING INSURANCE TERM OF 17....
      *  BAH 990720 ADDED SP-RBSPOPT2 = 8, INSURANCE ORGTERM SWING ON 16 DAYS
      *             FOR REGACC, PR# 911
      ******************************************************************
