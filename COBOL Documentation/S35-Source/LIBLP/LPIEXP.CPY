      * COPYMEMBER: LIBLP/LPIEXP
      *************************************************************
      *          INSURANCE EFFECTIVE/EXPIRATION DATES
      *            AND COMMISSION CALCULATION
      *
      *   ************************************************************
      *   NOTE: CHANGES MADE TO THIS ROUTINE ALSO HAVE TO BE MADE IN
      *         LIBLP/LPIEXPPD.
      **********************************************************************
      *
      *   NAME:  LPIEXP
      *   DESC:  THIS ROUTINE COMPUTES INSURANCE:
      *            EFFECTIVE DATE
      *            EXPIRATION DATE
      *            INSURANCE COMMISSION
      *
      *   IN  :  IEXP-LN          - LN-INSURANCE LEVEL (1-8)
      *
      *   OUT :  IEXP-EFFDATE     - INSURACE EFFECTIVE DATE
      *          IEXP-EXPDATE     - INSURACE EXPIRATION DATE
      *          IEXP-INSCPMM     - INSURANCE COMMISSION
      *
      *   USED:  LN-REC, SP-REC AND IC-REC
      *
      *   COPY:  LPIEXPW
      * REV:
      *  MJD 011597 FIXED BUG: NOT CALCULATING EXP DATE FOR 01 INS.
      *  JTG 012797 CORRECTED PROBLEM WITH INSCOMM FOR O2 & O3
      *  MJD 991001 ADDED LOGIC TO COMPUTE COMMISSION AS EITHER % OR
      *             FIXED AMOUNT  (TFC PR#41)
      *  MJD 000207 ADDED TEST OF GP-OT-INS-TERM (TFC PR#48) IF "Y" THEN
      *             FORCED ENTRY OF TERM INSTEAD OF EXPIRATION DATE
      *  JTG 010525 ADDED TEST FOR GP-I-AM-REGACC WHICH WAS OVER LOOKED
      *             BY MJD ON 991109
      *             FORCE INPUT OR EXP DATE ON O3 BY DAFAULTING TO 00/00/00
      *             INSTEAD OF MATURITY DATE (REGACC PR #985)
      *   CS 050426 DONT COMPUTE IF INSURANCE PREMIUM = ZEROS [PR#479 NSA]
      *  BAH 20200625 REMOVED OPEN-IC1-FILE AND CLOSE-IC1-FILE #TST001
      **********************************************************************
       IEXP-INS-CALCULATIONS SECTION.
           MOVE 0 TO IEXP-EXPDATE
                     IEXP-EFFDATE
                     IEXP-INSCOMM.

           IF IEXP-LN = 0
                 OR IEXP-LN > 8
                   OR LN-INSCOMP(IEXP-LN) = 0
                      OR LN-INSPREM(IEXP-LN) = ZERO
              GO TO IEXP-INS-CALCULATIONS-EXIT.

      *---------------------------------------------------
      * SET INSURANCE EFFECTIVE:
           MOVE LN-INTDATE TO IEXP-EFFDATE.

      *---------------------------------------------------
      * OT2 - OT5  SET INSURANCE EXPIRATION:
      *    IF IEXP-LN > 4 AND GP-OT-INS-TERM NOT = "Y"
      *       MOVE LN-INSEXP-DATE(IEXP-LN) TO IEXP-EXPDATE
      *       ADD 4 IEXP-LN GIVING IEXP-IC
      *       GO TO IEXP-INS-CALCULATIONS-COMM.
      *------------------------------------------------------------------
      * OT2 - OT5  SET INSURANCE EFFECTIVE:
      *------------------------------------------------------------------
      * JTG 010525 ADDED TEST FOR GP-I-AM-REGACC WHICH WAS OVER LOOKED
      *            BY MJD ON 991109
      *            FORCE INPUT OR EXP DATE ON O3 BY DAFAULTING TO 00/00/00
      *            INSTEAD OF MATURITY DATE (REGACC PR #985)
           IF IEXP-LN > 4
              IF (GP-OT-INS-TERM NOT = "Y"      ) 
      *         OR (GP-I-AM-REGACC AND IEXP-LN = 6)
                 MOVE LN-INSEXP-DATE(IEXP-LN) TO IEXP-EXPDATE
                 ADD 4 IEXP-LN GIVING IEXP-IC
                 GO TO IEXP-INS-CALCULATIONS-COMM.

      * CL DECREASING
           IF LN-INSURED = "J"
              MOVE 2 TO IEXP-IC
           ELSE
              MOVE 1 TO IEXP-IC.
      * CL LEVEL
           IF LN-INSTYPES-LL
              ADD 2 TO IEXP-IC.

      * A&H
           IF IEXP-LN = 2
              MOVE 5 TO IEXP-IC
           ELSE
      * PP
           IF IEXP-LN = 3
              IF LN-INSTYPES-PP2
                 MOVE 7 TO IEXP-IC
              ELSE
                 MOVE 6 TO IEXP-IC
              END-IF
           ELSE
      * O1
           IF IEXP-LN = 4
              MOVE 8 TO IEXP-IC.

           IF GP-OT-INS-TERM = "Y"
      * O2
              IF IEXP-LN = 5
                 MOVE 9 TO IEXP-IC
              ELSE
      * O3
                 IF IEXP-LN = 6
                    MOVE 10 TO IEXP-IC.

      *---------------------------------------------------
      * SET INSURANCE EXPIRATION:
           IF SP-EXTEND-EXPIRE(IEXP-IC)
              MOVE LN-INTDATE TO NUM-DATE
              MOVE LN-1STPYDATE TO SYS-DATE
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM TIMUPER.

           IF SP-EXTEND-INSURANCE(IEXP-IC)
                  OR (SP-EXTEND-EXPIRE(IEXP-IC)
                                  AND ELAPSED-UNITPER > 0)
              MOVE LN-1STPYDATE TO NDTE-DATE
              COMPUTE NDTE-HOLD = LN-INSTRM(IEXP-LN) - 1
           ELSE
              MOVE IEXP-EFFDATE TO NDTE-DATE
              MOVE LN-INSTRM(IEXP-LN) TO NDTE-HOLD.

           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO IEXP-EXPDATE.

      * COMPUTE COMMISSION:
       IEXP-INS-CALCULATIONS-COMM.
           MOVE LN-INSCOMP(IEXP-LN) TO IC-CODE.
           IF IC-CODE NOT = IEXP-IC-CODE
              PERFORM READ-IC1-FILE
              IF IO-FG NOT = 0
                 MOVE 100 TO IC-COMMRATE(IEXP-IC)
              ELSE
                 MOVE IC-CODE TO IEXP-IC-CODE.
           IF LN-INSOURS(IEXP-LN) = "Y"
              IF IC-COMMRATE(IEXP-IC) >= 0
                 COMPUTE IEXP-INSCOMM ROUNDED = IC-COMMRATE(IEXP-IC)
                             / 100 * LN-INSPREM(IEXP-LN)
              ELSE
                 COMPUTE IEXP-INSCOMM = IC-COMMRATE(IEXP-IC) * -1
              END-IF.

       IEXP-INS-CALCULATIONS-EXIT.
           EXIT.

