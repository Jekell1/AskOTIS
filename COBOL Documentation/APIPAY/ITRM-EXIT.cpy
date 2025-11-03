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
