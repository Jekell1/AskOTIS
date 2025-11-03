      *************************************************
      * REBATE AFTER REDUCTION AND %AGE COULD BE < 0
      *************************************************
       REBATE-ADJUST-REB-REBATE SECTION.
      * TEST FOR EARLY PROV 7, PRORATA EXTENSION CHARGE:
           IF SP-RBEARLY(REB-SUB) = 7
              IF LN-EXTCHG NOT = 0
                 IF PODATE-B4-1STPYDATE
                    PERFORM REBATE-EP7
      * ADD BACK EXTCHG FOR MIN REB TEST:
                    ADD LN-EXTCHG TO REB-TOTCHG.

      * TEST FOR MISSISSIPPI 20 DAY RULE;
      *  IF PREPAYMENT OCCURS BEFORE THE FIRST PAYMENT DATE, FULLY
      *  REBATE THE EXTENSION CHARGE, USED TO INCLUDE IT IN CALC
           IF SP-RBSPOPT1(REB-SUB) = 3
              IF PODATE-B4-1STPYDATE
                 ADD LN-EXTCHG TO REB-REBATE.

      * REDUCE COMPUTED REBATE BY REDUCTION AMOUNT:
           IF SP-RBREDUC(REB-SUB) > 9999
              PERFORM REBATE-SPEC-RBREDUC
           ELSE
           IF SP-RBSPOPT1(REB-SUB) = 5
              PERFORM REBATE-RBREDUC-NJ
           ELSE
           IF SP-RBSPOPT1(REB-SUB) = 16
              PERFORM REBATE-RBREDUC-OH
           ELSE
           IF SP-RBSPOPT1(REB-SUB) = 25
              PERFORM REBATE-RBREDUC-OH-25
           ELSE
           IF SP-RBSPOPT1(REB-SUB) = 21
              PERFORM REBATE-RBREDUC-NJ-21
           ELSE
           IF SP-RBSPOPT1(REB-SUB) = 28
              PERFORM REBATE-RBREDUC-VYBA-28
           ELSE
              COMPUTE REB-REBATE ROUNDED =
                          REB-REBATE - SP-RBREDUC(REB-SUB).

      * TAKE A PERCENT OF COMPUTED REBATE:
           COMPUTE REB-REBATE ROUNDED =
                         REB-REBATE * (SP-RBPCT(REB-SUB) / 100).

      * WE MUST EARN AT LEAST RBMINRET
           MOVE SP-RBMINRET(REB-SUB) TO REB-WORKER.
      * TEST FOR IOWA MINIMUM RETENTION:
           IF REB-WORKER = 9999.01
              IF REB-LPTRCD = "PB" OR "RN" OR "SC"
                                OR "RB" OR "RO"
                 MOVE 0 TO REB-WORKER
              ELSE
                 ADD LN-SERCHG TO REB-TOTCHG
                 PERFORM LOAN-CALCULATIONS
                 IF DISCLOSED-FINAMT > 75.00
                    MOVE 7.50 TO REB-WORKER
                 ELSE
                    MOVE 5.00 TO REB-WORKER
                 END-IF
              END-IF
           ELSE
      * TEST FOR SC, WORLD MINIMUM RETENTION:
           IF REB-WORKER = 9999.02
              IF REB-LPTRCD = "PB" OR "RN" OR "SC"
                                 OR "RB" OR "RO"
                 MOVE 0 TO REB-WORKER
              ELSE
                 MOVE 15.00 TO REB-WORKER.

           IF REB-ADDON-FG = "Y" AND REB-SUB = 7
              GO TO REBATE-ADJUST-REB-REBATE-FLOOR.

           COMPUTE REB-WORK2 = REB-TOTCHG - REB-REBATE.
           IF REB-ADDON-FG NOT = "Y" AND REB-SUB = 7
              IF LN-ADDON-FG = "Y"
                 IF LN-CONVERCD = "X"
                    COMPUTE REB-WORK2 = LN-ANTICERN(1) - REB-REBATE.

           IF REB-WORK2 < REB-WORKER
              COMPUTE REB-REBATE =
                        REB-WORK2 + REB-REBATE - REB-WORKER.

       REBATE-ADJUST-REB-REBATE-FLOOR.
      * NEXT STMT NECESSARY, AS < SP-RBMIN CHECK IS CONDITIONAL
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.

      * NO NEED TO REBATE WHEN BELOW THIS FLOOR
           IF SP-RBSPOPT1(REB-SUB) = 2 AND
              (REB-LPTRCD = "PB" OR "RN" OR "SC"
                              OR "RB" OR "RO")
              NEXT SENTENCE
           ELSE
              IF SP-RBMIN(REB-SUB) NOT = 999.01
                 IF SP-RBMIN(REB-SUB) NOT = 999.02
                    IF REB-REBATE < SP-RBMIN(REB-SUB)
                       MOVE 0 TO REB-REBATE
                    ELSE
                       NEXT SENTENCE
                 ELSE
      * 999.02 MERCURY "OK" PERSONAL PROPERTY
      * IF RENEWED, MINIMUM IS $1.00, IF PAID OUT, MINIMUM IS $5.00
                    IF (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                        OR "RB" OR "RO")
                       IF REB-REBATE < 1.00
                          MOVE 0 TO REB-REBATE
                       ELSE
                          NEXT SENTENCE
                    ELSE
                       IF REB-REBATE < 5.00
                          MOVE 0 TO REB-REBATE.

      * SPECIAL OPTION 2 = 11:
      * THE FINAL REFUND IS ROUNDED UP OR DOWN TO WHOLE DOLLARS
           IF SP-RBSPOPT2(REB-SUB) = 11
              IF REB-REBATE-CENTS < 51
                 MOVE 0 TO REB-REBATE-CENTS
              ELSE
                 COMPUTE REB-UPR-WHOLE ROUNDED = REB-REBATE + 0.50
                 MOVE REB-UPR-WHOLE TO REB-REBATE.

      **************************************
      *   SPECIAL REBATE REDUCTION ROUTINES
      **************************************
