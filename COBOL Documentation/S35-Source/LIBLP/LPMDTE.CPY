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
