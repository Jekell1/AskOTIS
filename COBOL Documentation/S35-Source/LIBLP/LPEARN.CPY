      * COPYMEMBER: LIBLP/LPEARN
      **************************************************************************
      *         COMPUTE ANTICIPATED EARNINGS/UNEARNED NUMBERS
      *
      *   NAME: LPEARN
      *   DESC: THIS ROUTINE COMPUTES (TOTAL ANTICIPATED NET INCOME),
      *         (TOTAL UNEARNED + COMPONENTS) AND (TOTAL ACCERN).
      *
      *   IN  :
      *   OUT : EARN-TOT-ANTICERN
      *         EARN-TOT-ANTICADJ
      *         EARN-TOT-ACCERN
      *         EARN-TOT-UFERN
      *         EARN-TOT-UNEARN AND EARN-UNEARN(1 TO 6)
      *   USED:
      *   COPY: LPEARNW
      *
      * REV:
      *  JTG 030790 ADDED EARN-TOT-ANTICADJ
      *             DISALLOWED UNEARNED ON SERVICE ACCTS & PL
      *  JTG 050290 TESTED LN-REBATE() TO SKIP COMPONENT UNEARN
      *  MJD 990730 IF EARN = 6 (OTHER) TEST GP-FLAG TO DETERMOINE
      *             WHETHER OR NOT TO INCLUDE IN ACCUMULATED TOTALS
      *  JTG 000501 MODIFIED TO RETURN 0 UNEARNED WHEN ACCOUNT IS PAID OFF
      *             RE: MERCURY UPGRADE FROM A10 TO A15 SINCE MERCURY
      *             HAD BOGUS ACCOUNTS WITH LN-CURBAL 0 AND STILL HAD
      *             NEGATIVE UNEARNED!!!!!!!!!!!!!!!!!!!!
      *  JTG 000802 CORRECTED TEST ON LN-POCD-OPEN TO INCLUDE TEST ON
      *             LN-POCD-PB-RESC, RE: REGENCY REGARDING BUG MADE IN
      *             LIBLP/AC1DVR WHICH WAS SKIPPING THE COMPUTE OF EARNINGS
      *             WHEN LN-ACCERN() = LN-UFERN()
      *   CS 081105 ADDED TEST ON GP-INCL-OTH-IN-EARNED-R, IF 'R' DONT INCLUDE
      *             OTHER(6) OR MAINTFEE(4) IN UNEARED/EARNED. REGACC UPDATES
      *             MAINTFEE WITH PARTICIPATION HELD TO AMORTIZE THE EXPENSE
      *             BUT DOESNT WANT TO UPDATE EARNED/UNEARNED REGACC PR#210
      **************************************************************************
       COMPUTE-EARNINGS SECTION.
           MOVE 0 TO EARN-TOT-ANTICERN
                     EARN-TOT-ANTICADJ
                     EARN-TOT-ACCERN
                     EARN-TOT-UFERN
                     EARN-TOT-UNEARN
                     EARN-UNEARN(1) EARN-UNEARN(2)
                     EARN-UNEARN(3) EARN-UNEARN(4)
                     EARN-UNEARN(5) EARN-UNEARN(6).

           MOVE 1 TO EARN.
           PERFORM COMPUTE-EARNINGS-ACCUM 6 TIMES.

       COMPUTE-EARNINGS-EXIT.
           EXIT.

      ************************************************************
      *    ACCUMULATE EARNINGS NUMBERS:
      ************************************************************
       COMPUTE-EARNINGS-ACCUM SECTION.
           IF EARN < 6 OR
               (EARN = 6 AND NOT (GP-INCL-OTH-IN-EARNED-N OR
                                   GP-INCL-OTH-IN-EARNED-R))
              IF NOT (EARN = 4 AND GP-INCL-OTH-IN-EARNED-R)
                 ADD LN-ANTICERN(EARN) TO EARN-TOT-ANTICERN
                 ADD LN-ANTICADJ(EARN) TO EARN-TOT-ANTICADJ
                 ADD LN-ACCERN(EARN) TO EARN-TOT-ACCERN
                 ADD LN-UFERN(EARN) TO EARN-TOT-UFERN.

           IF LN-CURBAL NOT = 0
      *JTG  IF LN-POCD-OPEN
            IF LN-POCD-OPEN OR LN-POCD-PB-RESC
             IF LN-PLCD = " "
               IF LN-SERVACCT NOT = "Y"
                IF LN-ANTICADJ(EARN) = 0
                   COMPUTE EARN-UNEARN(EARN) =
                                 LN-ANTICERN(EARN) - LN-ACCERN(EARN)
                   PERFORM COMPUTE-EARNINGS-ACCUM-RBTEST
                   IF EARN < 6 OR
                       (EARN = 6 AND NOT (GP-INCL-OTH-IN-EARNED-N OR
                                          GP-INCL-OTH-IN-EARNED-R))
                     IF NOT (EARN = 4 AND GP-INCL-OTH-IN-EARNED-R)
                        ADD EARN-UNEARN(EARN) TO EARN-TOT-UNEARN.

           ADD 1 TO EARN.

      *****************************
      * NOTE: WHAT ABOUT LEVEL 2 INS. ADDONS RE: REBATE
       COMPUTE-EARNINGS-ACCUM-RBTEST SECTION.
           IF EARN = 1 AND (NOT (LN-REBATE(4) = " " OR "C"))
              MOVE 0 TO EARN-UNEARN(EARN)
           ELSE
           IF EARN = 3 AND (NOT (LN-REBATE(5) = " " OR "C"))
              MOVE 0 TO EARN-UNEARN(EARN)
           ELSE
           IF EARN = 4 AND (NOT (LN-REBATE(6) = " " OR "C"))
              MOVE 0 TO EARN-UNEARN(EARN).
