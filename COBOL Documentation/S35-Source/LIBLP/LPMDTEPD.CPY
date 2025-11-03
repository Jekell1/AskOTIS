      * COPYMEMBER: LIBLP/LPMDTEPD
      ********************************************************************
      *                    (LPMDTEPD)
      *         MATURITY DATE CALCULATION
      *
      *   NAME: LPMDTE
      *   DESC: COMPUTES THE MATURITY DATE OF THE LOAN.
      *         N O T E:
      *                 REMEMBER TO CHANGE LPMDTE
      *
      *   IN  : MDTE-FLAG
      *         MDTE-ORGTERM-FG
      *
      *         IF MDTE-ORGTERM-FG = 'Y' - THE LOANS ORIGINAL TERM
      *                                    IS USED IN THE CALCULATIONS
      *                                    INSTEAD OF THE EXTENDED
      *                                    ADDON TERM
      *         IF MDTE-FLAG     = 'SP'  - UPDATED MATURITY IS RETURNED
      *         IF MDTE-FLAG     = 'Y'   - ORIGINAL MATURITY S RETURNED
      *
      *      NOTE:
      *           MDTE-FLAG AND MDTE-ORGTERM-FG ARE CLEARED TO SPACES,
      *           UPON EXIT.
      *
      *   OUT : MDTE-DATE
      *   USED: PD-ORGTERM, PD-DEFMNTS, PD-1STPYDATE, PD-SUM-ORGTERM
      *   COPY: LPMDTEW, LPNDTE, LPNDTEW, INCRMM.
      *
      * REV :
      *  JTG 032188 ADDED LOGIC FOR COMPUTATION OF UPDATED MATURITY
      *             OR ORIGINAL MATURITY
      *  JTG 080990 ADDED USAGE OF PD-SUM-ORGTERM
      *  JTG 071896 ADDED LOGIC FOR UNIT PERIODS
      ********************************************************************
       MATURITY-DATE-CALCULATION SECTION.
           MOVE PD-1STPYDATE TO NDTE-DATE.
           MOVE 0 TO NDTE-HOLD.
           IF MDTE-FLAG = " "
              MOVE PD-TOTNODEF TO NDTE-HOLD.

           IF MDTE-ORGTERM-FG = "Y"
              COMPUTE NDTE-HOLD = NDTE-HOLD + (PD-ORGTERM - 1)
           ELSE
              COMPUTE NDTE-HOLD = NDTE-HOLD + (PD-SUM-ORGTERM - 1).

           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO MDTE-DATE.
           MOVE " " TO MDTE-FLAG
                       MDTE-ORGTERM-FG.
