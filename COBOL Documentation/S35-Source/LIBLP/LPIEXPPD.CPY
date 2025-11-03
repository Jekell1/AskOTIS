      * COPYMEMBER: LIBLP/LPIEXPPD
      *************************************************************
      *          INSURANCE EFFECTIVE/EXPIRATION DATES
      *            AND COMMISSION CALCULATION
      *             ( PD-REC VERSION USED FOR ADDONS )
      *
      *   ************************************************************
      *   NOTE: CHANGES MADE TO THIS ROUTINE ALSO HAVE TO BE MADE IN
      *         LIBLP/LPIEXP.
      *
      *         . ALSO . THIS ROUTINE IS THE SAME AS LPIEXP EXCEPT FOR
      *         ........ THE IF ON SP-ADDON-EXCEPT-FG #REGACC PR#1297
      *
      *   ************************************************************
      *
      *   NAME:  LPIEXPPD
      *   DESC:  THIS ROUTINE COMPUTES INSURANCE:
      *            EFFECTIVE DATE
      *            EXPIRATION DATE
      *            INSURANCE COMMISSION
      *
      *   IN  :  IEXP-PD          - PD-INSURANCE LEVEL
      *
      *   OUT :  IEXP-EFFDATE     - INSURACE EFFECTIVE DATE
      *          IEXP-EXPDATE     - INSURACE EXPIRATION DATE
      *          IEXP-INSCOMM     - INSURANCE COMMISSION
      *
      *   USED:  LN-REC, PD-REC, SP-REC AND IC-REC
      *
      *   COPY:  LPIEXPW
      * REV:
      *  MJD 011597 CREATED NEW VERSION FOR USE WITH PD-REC.
      *             FIXED BUG; NOT COMPUTING O1 INS EXP DATE.
      *  MJD 052397 FIXED PROBLEM WITH INSCOMM FOR O2 AND O3.
      *  MJD 991001 ADDED LOGIC TO COMPUTE COMMISSION AS EITHER % OR
      *             FIXED AMOUNT  (TFC PR#41)
      *  MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *             ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  JTG 000919 ADDED SP-ADDON-EXCEPT-FG REGACC #71225 PR#1297
      *  JTG 010525 ADDED TEST FOR GP-I-AM-REGACC WHICH WAS OVER LOOKED
      *             BY MJD ON 991109
      *             FORCE INPUT OR EXP DATE ON O3 BY DAFAULTING TO 00/00/00
      *             INSTEAD OF MATURITY DATE (REGACC PR #985)
      *   CS 050427 DONT CALCULATE A COMMISSION, EXPDATE,EFFDATE IF THE
      *             PREMIUM IS ZERO.TROUBLE WHEN USING A FLAT RATE! [PR#479 NSA]
      *  BAH 20200625 REMOVED OPEN-IC1-FILE AND CLOSE-IC1-FILE #TST001
      ******************************************************************
       IEXP-INS-CALCULATIONS-PD SECTION.
           MOVE 0 TO IEXP-EXPDATE
                     IEXP-EFFDATE
                     IEXP-INSCOMM.

           IF IEXP-PD = 0
                 OR IEXP-PD > 8
                   OR PD-INSCOMP(IEXP-PD) = 0
                     OR PD-INSPREM(IEXP-PD) = ZEROS
              GO TO IEXP-INS-CALCULATIONS-PD-EXIT.

      *---------------------------------------------------
      * SET INSURANCE EFFECTIVE:
           IF SP-ADDON-EXCEPT-FG = "T"
              MOVE PD-LOANDATE TO IEXP-EFFDATE
           ELSE
              MOVE PD-INTDATE  TO IEXP-EFFDATE.

      *---------------------------------------------------
      * OT2 - OT5  SET INSURANCE EXPIRATION:
      *    IF IEXP-PD > 4 AND GP-OT-INS-TERM NOT = "Y"
      *       MOVE PD-INSEXP-DATE(IEXP-PD) TO IEXP-EXPDATE
      *       ADD 4 IEXP-PD GIVING IEXP-IC
      *       GO TO IEXP-INS-CALCULATIONS-PD-COMM.
      *------------------------------------------------------------------
      * OT2 - OT5  SET INSURANCE EFFECTIVE:
      *------------------------------------------------------------------
      * JTG 010525 ADDED TEST FOR GP-I-AM-REGACC WHICH WAS OVER LOOKED
      *            BY MJD ON 991109
      *            FORCE INPUT OR EXP DATE ON O3 BY DAFAULTING TO 00/00/00
      *            INSTEAD OF MATURITY DATE (REGACC PR #985)
      *    IF IEXP-PD > 4
      *       IF (GP-OT-INS-TERM NOT = "Y"      ) OR
      *          (GP-I-AM-REGACC AND IEXP-PD = 6)
      *          MOVE PD-INSEXP-DATE(IEXP-PD) TO IEXP-EXPDATE
      *          ADD 4 IEXP-PD GIVING IEXP-IC
      *          GO TO IEXP-INS-CALCULATIONS-PD-COMM.
           IF IEXP-PD > 4
              IF (GP-OT-INS-TERM NOT = "Y"      )
                 MOVE PD-INSEXP-DATE(IEXP-PD) TO IEXP-EXPDATE
                 ADD 4 IEXP-PD GIVING IEXP-IC
                 GO TO IEXP-INS-CALCULATIONS-PD-COMM.

      * CL DECREASING
           IF PD-INSURED = "J"
              MOVE 2 TO IEXP-IC
           ELSE
              MOVE 1 TO IEXP-IC.
      * CL LEVEL
           IF PD-INSTYPES-LL
              ADD 2 TO IEXP-IC.
      * A&H
           IF IEXP-PD = 2
              MOVE 5 TO IEXP-IC
           ELSE
      * PP
           IF IEXP-PD = 3
              IF PD-INSTYPES-PP2
                 MOVE 7 TO IEXP-IC
              ELSE
                 MOVE 6 TO IEXP-IC
              END-IF
           ELSE
      * O1
           IF IEXP-PD = 4
              MOVE 8 TO IEXP-IC.

           IF GP-OT-INS-TERM = "Y"
      * O2
              IF IEXP-PD = 5
                 MOVE 9 TO IEXP-IC
              ELSE
      * O3
                 IF IEXP-PD = 6
                    MOVE 10 TO IEXP-IC.

      *---------------------------------------------------
      * SET INSURANCE EXPIRATION:
      *    USE LN-XXX BECAUSE WE ARE TESTING THE SCENARIO WHEN INSURANCE
      *    IS EXTENDED WITH TERM.
           IF SP-EXTEND-EXPIRE(IEXP-IC)
              MOVE LN-INTDATE TO NUM-DATE
              MOVE LN-1STPYDATE TO SYS-DATE
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM TIMUPER.

           IF SP-EXTEND-INSURANCE(IEXP-IC)
                  OR (SP-EXTEND-EXPIRE(IEXP-IC)
                                  AND ELAPSED-UNITPER > 0)
              MOVE PD-1STPYDATE TO NDTE-DATE
              COMPUTE NDTE-HOLD = PD-INSTRM(IEXP-PD) - 1
           ELSE
              MOVE IEXP-EFFDATE TO NDTE-DATE
              MOVE PD-INSTRM(IEXP-PD) TO NDTE-HOLD.

           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO IEXP-EXPDATE.

      * COMPUTE COMMISSION:
       IEXP-INS-CALCULATIONS-PD-COMM.
           MOVE PD-INSCOMP(IEXP-PD) TO IC-CODE.
           IF IC-CODE NOT = IEXP-IC-CODE
              PERFORM READ-IC1-FILE
              IF IO-FG NOT = 0
                 MOVE 100 TO IC-COMMRATE(IEXP-IC)
              ELSE
                 MOVE IC-CODE TO IEXP-IC-CODE.
           IF PD-INSOURS(IEXP-PD) = "Y"
              IF IC-COMMRATE(IEXP-IC) >= 0
                 COMPUTE IEXP-INSCOMM ROUNDED = IC-COMMRATE(IEXP-IC)
                             / 100 * PD-INSPREM(IEXP-PD)
              ELSE
                 COMPUTE IEXP-INSCOMM = IC-COMMRATE(IEXP-IC) * -1
              END-IF.

       IEXP-INS-CALCULATIONS-PD-EXIT.
           EXIT.

