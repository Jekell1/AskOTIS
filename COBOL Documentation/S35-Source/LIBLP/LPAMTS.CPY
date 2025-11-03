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
       LOAN-CALCULATIONS SECTION.
           MOVE LN-CREDITLIM TO CREDIT-LIMIT.
           COMPUTE TOTAL-REPAY = LN-1STPYAMT
                     + LN-NOREGPY * LN-REGPYAMT + LN-LASTPYAMT.
           IF SP-MFFRMLA-05
              COMPUTE TOTAL-NOTE = TOTAL-REPAY - LN-EXTCHG
                                 - LN-ANTICERN(4)
           ELSE
              COMPUTE TOTAL-NOTE = TOTAL-REPAY - LN-EXTCHG
                                 - LN-ORGTERM * LN-MAINTFEE.
           COMPUTE ADJ-PRINCIPAL = TOTAL-NOTE - LN-INTCHG.

           MOVE 0 TO TOTAL-INSFINCHG TOTAL-FEEFINCHG TOTAL-INS-TAX
                     TOTAL-INSPREM-NONMEMO.
           PERFORM VARYING LPAMTS-SUB FROM 1 BY 1
                                UNTIL LPAMTS-SUB > 10
              IF ( (LPAMTS-SUB) < 9                  ) AND
                 ( LN-INSOURS(LPAMTS-SUB) NOT = "M"  )
                 ADD LN-INSPREM(LPAMTS-SUB) TO TOTAL-INSPREM-NONMEMO
                 IF LN-INSFINCHG(LPAMTS-SUB) = "Y"
                    ADD LN-INSPREM(LPAMTS-SUB) TO TOTAL-INSFINCHG
                 END-IF
              END-IF
              IF LN-FEEOURS(LPAMTS-SUB) NOT = "M"
                 IF LN-FEEFINCHG(LPAMTS-SUB) = "Y"
                    ADD LN-FEEAMT(LPAMTS-SUB) TO TOTAL-FEEFINCHG
                 END-IF
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    IF LN-FEECODE(LPAMTS-SUB) = "TC" OR "TA" OR "TP"
                                             OR "T1" OR "T2" OR "T3"
                       ADD LN-FEEAMT(LPAMTS-SUB) TO TOTAL-INS-TAX
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

           IF LN-LOANTYPE = "I"
              COMPUTE PRINCIPLE = TOTAL-NOTE - LN-INTCHG
              MOVE PRINCIPLE TO FINANCED-AMOUNT INT-CHARGEABLE
              SUBTRACT TOTAL-INSFINCHG TOTAL-FEEFINCHG
                                         FROM FINANCED-AMOUNT
              SUBTRACT LN-SERCHG FROM FINANCED-AMOUNT
                                         GIVING DISCLOSED-FINAMT
           ELSE
              MOVE TOTAL-REPAY TO PRINCIPLE
              IF SP-MFFRMLA-05
                 COMPUTE FINANCED-AMOUNT =
                      TOTAL-REPAY - LN-INTCHG - LN-SERCHG - LN-EXTCHG
                         - LN-ANTICERN(4)
                          - TOTAL-INSFINCHG - TOTAL-FEEFINCHG
              ELSE
                 COMPUTE FINANCED-AMOUNT =
                      TOTAL-REPAY - LN-INTCHG - LN-SERCHG - LN-EXTCHG
                         - (LN-MAINTFEE * LN-ORGTERM)
                          - TOTAL-INSFINCHG - TOTAL-FEEFINCHG
              END-IF
              MOVE FINANCED-AMOUNT TO DISCLOSED-FINAMT
              COMPUTE INT-CHARGEABLE = TOTAL-NOTE - LN-INTCHG.

      * GET NON MEMO FEES AND NON MEMO INSURANCES:
      * GET OUR FEES AND OUR INSURANCES:
           MOVE 0 TO NONMEMO-INS NONMEMO-FEES
                     LIVE-CHECK-FEES.
           MOVE 0 TO INSUR-OURS FEES-OURS.
           PERFORM VARYING LPAMTS-SUB FROM 1 BY 1
                                   UNTIL LPAMTS-SUB > 10
              IF LPAMTS-SUB < 9
                 IF LN-INSOURS(LPAMTS-SUB) NOT = "M"
                    ADD LN-INSPREM(LPAMTS-SUB) TO NONMEMO-INS
                    IF LN-INSOURS(LPAMTS-SUB) = "Y"
                       ADD LN-INSPREM(LPAMTS-SUB) TO INSUR-OURS
                    END-IF
                 END-IF
              END-IF
              IF LN-FEEOURS(LPAMTS-SUB) NOT = "M"
                 ADD LN-FEEAMT(LPAMTS-SUB) TO NONMEMO-FEES
                 MOVE LN-FEECODE(LPAMTS-SUB) TO LPAMTSW-FEECODE
                 IF NOT (LPAMTSW-FEECODE-CH1-NUMERIC)
                    ADD LN-FEEAMT(LPAMTS-SUB) TO LIVE-CHECK-FEES
                 END-IF
                 IF LN-FEEOURS(LPAMTS-SUB) = "Y"
                    ADD LN-FEEAMT(LPAMTS-SUB) TO FEES-OURS
                 END-IF
              END-IF
           END-PERFORM.

           IF (LN-INTCHG + LN-SERCHG + NONMEMO-INS) > TOTAL-NOTE
              MOVE 0 TO CASH-ADVANCE
                        SPCASH-ADVANCE
           ELSE
              COMPUTE CASH-ADVANCE = TOTAL-NOTE - LN-INTCHG
                                   - LN-SERCHG - NONMEMO-INS
              COMPUTE SPCASH-ADVANCE = CASH-ADVANCE + LN-SERCHG.

           IF LIVE-CHECK-FEES NOT = 0
              MOVE "CL/LCFLD" TO FORM-NAM
              MOVE "LC-CALC-HI-CREDIT" TO MESS
              CALL FORM-PROGX USING FORM-PATH STAT LN-CLASS MESS
              CANCEL FORM-PROGX
              IF STAT = "00"
                 IF MESS = "L"
                    SUBTRACT LIVE-CHECK-FEES FROM NONMEMO-FEES.

           IF NONMEMO-FEES > CASH-ADVANCE
              MOVE 0 TO HIGH-CREDIT
           ELSE
              COMPUTE HIGH-CREDIT = CASH-ADVANCE - NONMEMO-FEES.

           IF SP-CAL-CHGABLE(1) = "P" OR "I"
              SUBTRACT LN-SERCHG FROM INT-CHARGEABLE.

           IF SP-CAL-CHGABLE(1) = "P" OR "S"
              SUBTRACT NONMEMO-INS FROM INT-CHARGEABLE.

