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
       INS-TERM-CALCULATION SECTION.
           MOVE 0 TO ITRM-INSTERM.
           MOVE SPACES TO ITRM-INEFFECT-FG.

           IF ITRM-SUB = 0
            OR ITRM-SUB > 10
             OR LN-INSCOMP(ITRM-SUB) = 0
               OR LN-INSCOVR(ITRM-SUB) = 0
                  GO TO ITRM-EXIT.
      *******************************************
      * SETUP SPR SUBSCRIPT
      *******************************************
           IF ITRM-SUB = 1
              IF LN-INSURED = "J"
                 MOVE 2 TO ITRM-SUB1
              ELSE
                 MOVE 1 TO ITRM-SUB1
              END-IF
              IF LN-INSTYPES-LL
                 ADD 2 TO ITRM-SUB1
              END-IF
           ELSE
      * A&H
              IF ITRM-SUB = 2
                 MOVE 5 TO ITRM-SUB1
              ELSE
      * PROPERTY
                 IF ITRM-SUB = 3
                    MOVE 6 TO ITRM-SUB1
                 ELSE
      * OTHER 1
                    IF ITRM-SUB = 4
                       MOVE 12 TO ITRM-SUB1
                    ELSE
      * OTHER 2
                       IF ITRM-SUB = 5
                          MOVE 13 TO ITRM-SUB1
                       ELSE
      * OTHER 3
                          MOVE 14 TO ITRM-SUB1.


      *******************************************
      *    COMPUTE INSURED TERM:
      *******************************************
           MOVE LN-INSEFF-DATE(ITRM-SUB) TO NUM-DATE.
           MOVE LN-INSEXP-DATE(ITRM-SUB) TO SYS-DATE.
           IF LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1
              PERFORM TIM360
              MOVE ELAPSED-MONTHS TO ITRM-INSTERM
           ELSE
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM TIMUPER
              MOVE ELAPSED-UNITPER TO ITRM-INSTERM.
      *******************************************
      * TEST FOR SPECIAL SWING DAYS OPTION
      *******************************************
           IF SP-RBSPOPT2(ITRM-SUB1) = 8
              IF ELAPSED-REM NOT < SP-RBDAYS(ITRM-SUB1 2)
                 ADD 1 TO ELAPSED-MONTHS ITRM-INSTERM.

      *******************************************
      *    TEST FOR INEFFECT TODAY:
      *******************************************
      * SET LN-REBATE-TAB SUBSCRIPT:
      *    1- CL    9- O1  12- O4  15- O7
      *    2- AH   10- O2  13- O5
      *    3- PP   11- O3  14- O6

           IF ITRM-TODAY = 0
              GO TO ITRM-EXIT.
           MOVE ITRM-SUB TO ITRM-SUB1.
           IF ITRM-SUB1 > 3
              ADD 5 TO ITRM-SUB1.

           IF LN-REBATE(ITRM-SUB1) = " "
              IF LN-INSEXP-DATE(ITRM-SUB) > ITRM-TODAY
                 MOVE "Y" TO ITRM-INEFFECT-FG.

       ITRM-EXIT.
           EXIT.
