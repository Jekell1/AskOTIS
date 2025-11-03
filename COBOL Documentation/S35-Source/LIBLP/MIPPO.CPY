      * COPYMEMBER: LIBLP/MIPPO
      *************************************************************************
      *          DETERMINE-MIP-REQUIRED
      *
      *    NAME: MIPPO
      *    DESC: THESE ROUTINES WILL DETERMINE IF MIP INSURANCE IS
      *          DO TO BE COLLECTED BASED ON:
      *                   LN-MIP-CLPDTH-DATE
      *                   LN-MIP-AHPDTH-DATE
      *                   LN-CL-REBATE FLAG
      *                   LN-AH-REBATE FLAG
      *                   POFF-PAYDATE
      *
      *          FOR PAYMENT POSTING WHEN LP-TRAMT IS ENOUGH MONIES PAID
      *          ( TO ) COVER THE PREMIUMS DUE, THEY WILL BE COLLECTED.
      *          FOR PAYOFFS
      *
      *    N O T E:
      *             A SIMILAR ROUTINE EXISTS IN LP/LONPFC.C FOR PAYMENTS
      *
      *    IN  : POFF-PAYDATE
      *          POFF-LPTRCD
      *    OUT : POFF-LXM-MIP-CLPAID
      *          POFF-LXM-MIP-AHPAID
      *          POFF-LXM-MIP-CLPDTH
      *          POFF-LXM-MIP-AHPDTH
      *    USED:
      *    COPY:
      * REV:
      * JTG 020731 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      * JTG 020924 CHANGED TO TEST POFF-LPTRCD = "PO" AND IN THE COMPARE
      *            USE FULL 6 DIGIT DATE                          REGENCY #1801
      *  JTG 030213 CHANGED MIP LOGIC PER DIRK:
      *             IF TODAY MM/YY LE MIPPDTH DON'T COLLECT PREMIUM
      *             IF TODAY MM/YY GT MIPPDTH COLLECT PREMIUM       REGENCY #392
      * BAH 050714 CALCULATE MIP ON RENEWALS, REGWAN PL# 499
      **************************************************************************

      * LOGIC TO DETERMINE PAYMENT REQUIREMENTS FOR MIP INSURANCE
      
       DETERMINE-MIP-REQUIRED SECTION.
           IF LN-MIP-FG = "Y"
              MOVE 0 TO POFF-LXM-MIP-CLPAID
                        POFF-LXM-MIP-AHPAID
              MOVE LN-MIP-CLPDTH-DATE TO POFF-LXM-MIP-CLPDTH-DATE
              MOVE LN-MIP-AHPDTH-DATE TO POFF-LXM-MIP-AHPDTH-DATE

              MOVE "N" TO POFF-CLPREM-CHG-FG
                          POFF-AHPREM-CHG-FG

              PERFORM UNTIL (
                              POFF-CLPREM-CHG-FG = " " AND
                              POFF-AHPREM-CHG-FG = " "
                            )
                 PERFORM TEST-MIP-CL-DUE
                 PERFORM TEST-MIP-AH-DUE
                 IF POFF-CLPREM-CHG-FG = "Y" OR
                    POFF-AHPREM-CHG-FG = "Y"
                    IF POFF-CLPREM-CHG-FG = "Y"
                       ADD LN-INSPREM(1) TO POFF-LXM-MIP-CLPAID
                       MOVE POFF-LXM-MIP-CLPDTH-DATE TO NDTE-DATE
                       MOVE 1                    TO NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE      TO POFF-LXM-MIP-CLPDTH-DATE

                       MOVE POFF-LXM-MIP-CLPDTH-DATE TO NUM-DATE
                       MOVE POFF-PAYDATE        TO SYS-DATE
                       MOVE 05                  TO NUM-DA S-DD
                       IF SYS-DATE = NUM-DATE
                          MOVE " " TO POFF-CLPREM-CHG-FG
                       END-IF
                    END-IF
                    IF POFF-AHPREM-CHG-FG = "Y"
                       ADD LN-INSPREM(2) TO POFF-LXM-MIP-AHPAID
                       MOVE POFF-LXM-MIP-AHPDTH-DATE TO NDTE-DATE
                       MOVE 1                   TO NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE    TO POFF-LXM-MIP-AHPDTH-DATE

                       MOVE POFF-LXM-MIP-AHPDTH-DATE TO NUM-DATE
                       MOVE POFF-PAYDATE        TO SYS-DATE
                       MOVE 05                  TO NUM-DA S-DD
                       IF SYS-DATE = NUM-DATE
                          MOVE " " TO POFF-AHPREM-CHG-FG
                       END-IF
                    END-IF
                 ELSE
                    MOVE " " TO POFF-CLPREM-CHG-FG
                                POFF-AHPREM-CHG-FG
                 END-IF
              END-PERFORM
           END-IF.

      ********************************************************************
      *    TEST MIP CL PAYMENT DUE
      *      THIS ROUTINE DETERMINES WHETHER A
      *      MIP CLPREM IS DUE TODAY, BASED ON
      *      -  LN-CL-REBATE = ' '     - HAS NOT BEEN CANCELLED
      *      -  MIP-CLPDTH HAS NOT REACHED LN-INSEXP-DATE(1)
      *      -  LPTRCD = 'PO'
      *             WHEN THE EFFECTIVE DATE OF THE PAYOFF IS BEFORE
      *             OR EQUAL TO MIP PDTH DO NOT COLLECT A PREMIUM
      *      -  LPTRCD NOT = 'PO'
      *             WHEN THE EFFECTIVE MONTH & YEAR OF THE PAYMENT
      *             IS PRIOR TO THE MONTH AND YEAR OF MIP PDTH
      *             DO NOT COLLECT A PREMIUM
      ********************************************************************
       TEST-MIP-CL-DUE SECTION.
           MOVE " " TO POFF-CLPREM-CHG-FG.
           IF (LN-CL-REBATE NOT = " ") OR
              (LN-INSPREM(1) = 0)
              GO TO TEST-MIP-CL-DUE-EXIT.

      * SEE IF COLLECTION OF MIP-CL HAS EXPIRED:
           MOVE LN-INSEXP-DATE(1)        TO NUM-DATE.
           MOVE POFF-LXM-MIP-CLPDTH-DATE TO SYS-DATE.
           MOVE 05 TO NUM-DA S-DD.
           IF SYS-DATE NOT < NUM-DATE
              GO TO TEST-MIP-CL-DUE-EXIT.

      * SEE IF COLLECTION OF MIP-CLPREM IS DUE:
           MOVE POFF-LXM-MIP-CLPDTH-DATE TO NUM-DATE.
           MOVE POFF-PAYDATE        TO SYS-DATE.
           IF POFF-LPTRCD = "PO" OR "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              IF SYS-DATE NOT > NUM-DATE
                 GO TO TEST-MIP-CL-DUE-EXIT
              END-IF
           ELSE
              MOVE 05 TO NUM-DA S-DD
              IF SYS-DATE NOT > NUM-DATE
                 GO TO TEST-MIP-CL-DUE-EXIT.

           MOVE "Y" TO POFF-CLPREM-CHG-FG.

       TEST-MIP-CL-DUE-EXIT.
           EXIT.

      ********************************************************************
      *    TEST MIP AH PAYMENT DUE
      *      THIS ROUTINE DETERMINES WHETHER A
      *      MIP AHPREM IS DUE TODAY, BASED ON
      *      -  LN-AH-REBATE = ' '     - HAS NOT BEEN CANCELLED
      *      -  MIP-AHPDTH HAS NOT REACHED LN-INSEXP-DATE(1)
      *      -  LPTRCD = 'PO'
      *             WHEN THE EFFECTIVE DATE OF THE PAYOFF IS BEFORE
      *             OR EQUAL TO MIP PDTH DO NOT COLLECT A PREMIUM
      *      -  LPTRCD NOT = 'PO'
      *             WHEN THE EFFECTIVE MONTH & YEAR OF THE PAYMENT
      *             IS PRIOR TO THE MONTH AND YEAR OF MIP PDTH
      *             DO NOT COLLECT A PREMIUM
      ********************************************************************
       TEST-MIP-AH-DUE SECTION.
           MOVE " " TO POFF-AHPREM-CHG-FG.
           IF (LN-AH-REBATE NOT = " ") OR
              (LN-INSPREM(2) = 0)
              GO TO TEST-MIP-AH-DUE-EXIT.

      * SEE IF COLLECTION OF MIP-AH HAS EXPIRED:
           MOVE LN-INSEXP-DATE(2)        TO NUM-DATE.
           MOVE POFF-LXM-MIP-AHPDTH-DATE TO SYS-DATE.
           MOVE 05 TO NUM-DA S-DD.
           IF SYS-DATE NOT < NUM-DATE
              GO TO TEST-MIP-AH-DUE-EXIT.

      * SEE IF COLLECTION OF MIP-AH-PREM IS DUE:
           MOVE POFF-LXM-MIP-AHPDTH-DATE TO NUM-DATE.
           MOVE POFF-PAYDATE        TO SYS-DATE.
           IF POFF-LPTRCD = "PO" OR "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              IF SYS-DATE NOT > NUM-DATE
                 GO TO TEST-MIP-AH-DUE-EXIT
              END-IF
           ELSE
              MOVE 05 TO NUM-DA S-DD
              IF SYS-DATE NOT > NUM-DATE
                 GO TO TEST-MIP-AH-DUE-EXIT.

           MOVE "Y" TO POFF-AHPREM-CHG-FG.

       TEST-MIP-AH-DUE-EXIT.
           EXIT.

