      * COPYMEMBER: LIBLP/LPDAMTS
      ********************************************************************
      *          LOAN CALCULATIONS   (DUPLICATE OF LPAMTS)
      *
      *   NAME:  LPDAMTS
      *          N O T E: LPAMTS NEEDS TO BE CHANGED IF
      *                   THIS ROUTINE CHANGES.
      *
      *   DESC:  GENERALIZED LOAN CALCULATIONS USING PD-RECORD
      *
      *   IN  :  SPR MUST BE READ FOR INT-CHARGEABLE
      *   OUT :  TOTAL-REPAY      - TOTAL REPAYABLE
      *          TOTAL-NOTE       - TOTAL REPAYABE LESS EXTCHG
      *                                             LESS MAINT.FEES
      *          PRINCIPLE        - LOAN AMOUNT
      *          ADJ-PRINCIPAL    - TOTAL-NOTE - INTEREST CHARGE
      *          CASH-ADVANCE     - TOTAL-NOTE - INTCHG - SERCHG
      *                                             - ALL INSPREMS
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
      *REV:
      * 071586 JTG ADDED CASH ADVANCE ROUTINE
      * 090586 JTG ADDED FINANCED-AMOUNT ROUTINE
      * 030287 JTG ADDED DISCLOSED-FINAMT ROUTINE
      * 062987 JTG CHANGED P.C. FINANCED-AMOUNT TO USE
      *            TOTAL-REPAY INSTEAD OF PD-LNAMT
      * 030888 BAW ADDED ADJ-PRINCIPAL
      * 102688 SLC NOW CALCULATES HIGH CREDIT.
      * 081689 BAH ADDED INT-CHARGEABLE
      * 041392 JTG CORRECTED (IB) INT-CHARGEABLE
      * 033195 JTG REMOVED REF: NOODDPY
      * 061995 JTG CHANGED TO ADD A 10 INSURANCES
      * 020896 JTG CHANGED TO PERFORM VARYING LOGIC TO GET NON MEMO FEE
      *            AND NON MEMO INSURANCE
      * 111296 JTG CHANGED PD-INSTBL (LEVEL'S 9 & 10) TO FILLER
      * 073197 JTG ADDED LOGIC FOR TOTAL-FEEFINCHG AND TOTAL-INSFINCHG
      * 022398 BAH ADDED INSUR-OURS, FEES-OURS FOR DISC FRMLA (5) FFG
      * JTG 990727 ADDED CREDIT-LIMIT FOR LIBWI/LNAPPROVAL RE: REVOLVING
      * JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            ADDED TOTAL-INS-TAX                                WORLD #288
      * JTG 020620 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS    REGENCY #1801
      *            ADDED TOTAL-INSPREM-NONMEMO FOR LPAPRS WHICH
      *            MUST EXCLUDE MEMO INSURANCE PREMIUMS
      *  MJD 100812 IF MFFRMLA-05 THEN LOOK AT ANTICERN(4)
      *             INSTEAD OF PD-MAINTFEE * TERM.
      *             MAINT FEE MAY BE LAGER THEN PD-MAINTFEE
      *             FIELD AND GET TRUNCATED.                        WORLD #692
      *  MJD 160823 ADDED GET-LIVECHECK-FEES TO CALCULATE THE AMOUNT OF FEES
      *             FROM LIVE CHECKS THAT NEED TO BE INCLUDED IN
      *             HIGH-CREDIT/PD-CREDITLIM                          61000/1180
      *  BAH 190318 ADDED LPAMTSW-FEECODE-CH1, MOVED FROM PD-REC
      ********************************************************************
       LOAN-CALCULATIONS SECTION.
           MOVE PD-CREDITLIM TO CREDIT-LIMIT.
           COMPUTE TOTAL-REPAY = PD-1STPYAMT
                     + PD-NOREGPY * PD-REGPYAMT + PD-LASTPYAMT.
           IF SP-MFFRMLA-05
              COMPUTE TOTAL-NOTE = TOTAL-REPAY - PD-EXTCHG
                                 - PD-ANTICERN(4)
           ELSE
              COMPUTE TOTAL-NOTE = TOTAL-REPAY - PD-EXTCHG
                                 - PD-ORGTERM * PD-MAINTFEE.
           COMPUTE ADJ-PRINCIPAL = TOTAL-NOTE - PD-INTCHG.

           MOVE 0 TO TOTAL-INSFINCHG TOTAL-FEEFINCHG TOTAL-INS-TAX
                     TOTAL-INSPREM-NONMEMO.
           PERFORM VARYING LPAMTS-SUB FROM 1 BY 1
                                UNTIL LPAMTS-SUB > 10
              IF ( (LPAMTS-SUB) < 9                  ) AND
                 ( PD-INSOURS(LPAMTS-SUB) NOT = "M"  )
                 ADD PD-INSPREM(LPAMTS-SUB) TO TOTAL-INSPREM-NONMEMO
                 IF PD-INSFINCHG(LPAMTS-SUB) = "Y"
                    ADD PD-INSPREM(LPAMTS-SUB) TO TOTAL-INSFINCHG
                 END-IF
              END-IF
              IF PD-FEEOURS(LPAMTS-SUB) NOT = "M"
                 IF PD-FEEFINCHG(LPAMTS-SUB) = "Y"
                    ADD PD-FEEAMT(LPAMTS-SUB) TO TOTAL-FEEFINCHG
                 END-IF
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    IF PD-FEECODE(LPAMTS-SUB) = "TC" OR "TA" OR "TP"
                                             OR "T1" OR "T2" OR "T3"
                       ADD PD-FEEAMT(LPAMTS-SUB) TO TOTAL-INS-TAX
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

           IF PD-LOANTYPE = "I"
              COMPUTE PRINCIPLE = TOTAL-NOTE - PD-INTCHG
              MOVE PRINCIPLE TO FINANCED-AMOUNT INT-CHARGEABLE
              SUBTRACT TOTAL-INSFINCHG TOTAL-FEEFINCHG
                                         FROM FINANCED-AMOUNT
              SUBTRACT PD-SERCHG FROM FINANCED-AMOUNT
                                         GIVING DISCLOSED-FINAMT
           ELSE
              MOVE TOTAL-REPAY TO PRINCIPLE
              IF SP-MFFRMLA-05
                 COMPUTE FINANCED-AMOUNT =
                      TOTAL-REPAY - PD-INTCHG - PD-SERCHG - PD-EXTCHG
                         - PD-ANTICERN(4)
                          - TOTAL-INSFINCHG - TOTAL-FEEFINCHG
              ELSE
                 COMPUTE FINANCED-AMOUNT =
                      TOTAL-REPAY - PD-INTCHG - PD-SERCHG - PD-EXTCHG
                         - (PD-MAINTFEE * PD-ORGTERM)
                          - TOTAL-INSFINCHG - TOTAL-FEEFINCHG
              END-IF
              MOVE FINANCED-AMOUNT TO DISCLOSED-FINAMT
              COMPUTE INT-CHARGEABLE = TOTAL-NOTE - PD-INTCHG.

      * GET NON MEMO FEES AND NON MEMO INSURANCES:
      * GET OUR FEES AND OUR INSURANCES:
           MOVE 0 TO NONMEMO-INS NONMEMO-FEES
                     LIVE-CHECK-FEES.
           MOVE 0 TO INSUR-OURS FEES-OURS.
           PERFORM VARYING LPAMTS-SUB FROM 1 BY 1
                                   UNTIL LPAMTS-SUB > 10
              IF LPAMTS-SUB < 9
                 IF PD-INSOURS(LPAMTS-SUB) NOT = "M"
                    ADD PD-INSPREM(LPAMTS-SUB) TO NONMEMO-INS
                    IF PD-INSOURS(LPAMTS-SUB) = "Y"
                       ADD PD-INSPREM(LPAMTS-SUB) TO INSUR-OURS
                    END-IF
                 END-IF
              END-IF
              IF PD-FEEOURS(LPAMTS-SUB) NOT = "M"
                 ADD PD-FEEAMT(LPAMTS-SUB) TO NONMEMO-FEES
                 MOVE PD-FEECODE(LPAMTS-SUB) TO LPAMTSW-FEECODE
                 IF NOT (LPAMTSW-FEECODE-CH1-NUMERIC)
                    ADD PD-FEEAMT(LPAMTS-SUB) TO LIVE-CHECK-FEES
                 END-IF
                 IF PD-FEEOURS(LPAMTS-SUB) = "Y"
                    ADD PD-FEEAMT(LPAMTS-SUB) TO FEES-OURS
                 END-IF
              END-IF
           END-PERFORM.

           COMPUTE CASH-ADVANCE = TOTAL-NOTE - PD-INTCHG
                                - PD-SERCHG - NONMEMO-INS.
           COMPUTE SPCASH-ADVANCE = CASH-ADVANCE + PD-SERCHG.

           IF LIVE-CHECK-FEES NOT = 0
              MOVE "CL/LCFLD" TO FORM-NAM
              MOVE "LC-CALC-HI-CREDIT" TO MESS
              CALL FORM-PROGX USING FORM-PATH STAT PD-CLASS MESS
              CANCEL FORM-PROGX
              IF STAT = "00"
                 IF MESS = "L"
                    SUBTRACT LIVE-CHECK-FEES FROM NONMEMO-FEES.

           COMPUTE HIGH-CREDIT = CASH-ADVANCE - NONMEMO-FEES.

           IF SP-CAL-CHGABLE(1) = "P" OR "I"
              SUBTRACT PD-SERCHG FROM INT-CHARGEABLE.

           IF SP-CAL-CHGABLE(1) = "P" OR "S"
              SUBTRACT NONMEMO-INS FROM INT-CHARGEABLE.

