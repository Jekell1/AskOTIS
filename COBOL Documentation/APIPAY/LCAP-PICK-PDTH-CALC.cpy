      ****************************************************
      * DETERMINE WHICH PAID THRU CALCULATION TO PERFORM
      ****************************************************
       LCAP-PICK-PDTH-CALC SECTION.
           IF LN-PAY-SCHLD-FG = "Y"
              PERFORM LCAP-CALC-PDTH-ADDON
           ELSE
              PERFORM LCAP-CALC-PDTH.
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
