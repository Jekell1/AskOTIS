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
      *================================================================*
      * END COPYBOOK: LIBLP\LPITRM.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPAMTS.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPAMTS
      ********************************************************************
      *          LOAN CALCULATIONS   (DUPLICATE OF LPDAMTS)
      *
      *   NAME:  LPAMTS
      *          N O T E: LPDAMTS NEEDS TO BE CHANGED IF
      *                   THIS ROUTINE CHANGES.
      *
      *   DESC:  GENERALIZED LOAN CALCULATIONS USING LN-RECORD
      *
      *   IN  :  SPR MUST BE READ FOR INT-CHARGEABLE
      *   OUT :  TOTAL-REPAY      - TOTAL REPAYABLE
      *          TOTAL-NOTE       - TOTAL REPAYABE LESS EXTCHG
      *                                        LESS MAINT.FEES
      *          PRINCIPLE        - LOAN AMOUNT
      *          ADJ-PRINCIPAL    - TOTAL-NOTE - INTEREST CHARGE
      *          CASH-ADVANCE     - TOTAL-NOTE - INTCHG - SERCHG
      *                                        - ALL INSPREMS
      *          SPCASH-ADVANCE   - CASH-ADVANCE + SERCHG
      *          FINANCED-AMOUNT  - AMOUNT APR IS COMPUTED ON
      *          DISCLOSED-FINAMT - AMOUNT DISCLOSED AS FINANCED
      *          INT-CHARGEABLE   - AMOUNT INTEREST IS CALC'D ON
      *          TOTAL-INSPREM-NONMEMO - TOTAL INS PREMIUMS THAT ARE NON MEMO
      *          TOTAL-INSFINCHG  - TOTAL INS PREMIUMS THAT ARE FINANCE CHARGES
      *          TOTAL-FEEFINCHG  - TOTAL FEES THAT ARE FINANCE CHARGES
      *          TOTAL-INS-TAX    - TOTAL FEES THAT ARE INSURANCE SALES TAX
      *          HIGH-CREDIT      - CASH-ADVANCE - ALL FEES
      *          CREDIT-LIMIT     - LOANS SET CREDIT-LIMIT RE: REVOLVING
      *          NONMEMO-INS      - NON MEMO INSURANCES
      *          NONMEMO-FEES     - NON MEMO FEES
      *          INSUR-OURS       - OUR INSURANCES
      *          FEES-OURS        - OUR FEES
      *          LIVE-CHECK-FEES  - FEES ON LIVE CHECK LOAN
      *
      * REV:
      *  071586 JTG ADDED CASH ADVANCE ROUTINE
      *  090586 JTG ADDED FINANCED-AMOUNT ROUTINE
      *  030287 JTG ADDED DISCLOSED-FINAMT ROUTINE
      *  062987 JTG CHANGED P.C. FINANCED-AMOUNT TO USE
      *             TOTAL-REPAY INSTEAD OF LN-LNAMT
      *  030888 BAW ADDED ADJ-PRINCIPAL
      *  102688 SLC NOW CALCULATES HIGH CREDIT.
      *  081689 BAH ADDED INT-CHARGEABLE
      *  041392 JTG CORRECTED (IB) INT-CHARGEABLE
      *  033195 JTG REMOVED REF: NOODDPY
      *  061995 JTG CHANGED TO ADD A 10 INSURANCES
      *  020896 JTG CHANGED TO PERFORM VARYING LOGIC TO GET NON MEMO FEE
      *             AND NON MEMO INSURANCE
      *  111296 JTG CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  073197 JTG ADDED LOGIC FOR TOTAL-FEEFINCHG AND TOTAL-INSFINCHG
      *  022398 BAH ADDED INSUR-OURS AND FEES-OURS FOR DISC FRMLA (5)
      *  JTG 990727 ADDED CREDIT-LIMIT FOR LIBWI/LNAPPROVAL RE: REVOLVING
      *  JTG 020620 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED TOTAL-INSPREM-NONMEMO FOR LPAPRS WHICH
      *             MUST EXCLUDE MEMO INSURANCE PREMIUMS
      *  MJD 090905 IF MFFRMLA-05 THEN LOOK AT ANTICERN(4)
      *             INSTEAD OF LN-MAINTFEE * TERM.
      *             MAINT FEE MAY BE LAGER THEN LN-MAINTFEE
      *             FIELD AND GET TRUNCATED.                        WORLD #651
      *  MJD 160823 ADDED GET-LIVECHECK-FEES TO CALCULATE THE AMOUNT OF FEES
      *             FROM LIVE CHECKS THAT NEED TO BE INCLUDED IN
      *             HIGH-CREDIT/LN-CREDITLIM                          61000/1180
      *  BAH 190318 ADDED LPAMTSW-FEECODE-CH1, MOVED IT FROM LN-REC REDEFINE
      *  BAH 20221121 DONT ALLOW CASH-ADVANCE, SPCASH-ADVANCE OR HIGH-CREDIT TO
      *               RESULT IN NEGATIVE, CINDY FOUND IN TESTING
      **************************************************************************
