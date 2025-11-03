      * COPYMEMBER: LIBEX/EX01XCD
      **************************************************************************
      *
      *   DESCRIPTION:  CODE DESCRIPTION FILE RSZ 74
      *
      *-------------------------------------------------------------------------
      *   KEY - TYPE, CODE              
      *
      *       TYPE:
      *        {  -  ACTION CODES
      *        AS -  ADVERTISING SOURCE CODE
      *        AT -  ACTIVITY CODES
      *        BR -  BATCH (PAYMENT) REFERENCE CODES
      *        BS -  REFERENCE  (LP-REFNO)
      *        BW -  BORROWER HOUSING CODES
      *        CH -  CHARGE OFF (P&L) REASON CODE
      *        CL -  CAN LOAN GROUP ID
      *        CN -  COLLECTOR ID
      *        CO -  COLLATERAL CODES
      *        CT -  COUNTY TAX PERCENTS CODES (KENTUCKY)
      *        CY -  CITY   TAX PERCENTS CODES (KENTUCKY)
      *        DF -  DEFERMENT REASON CODES
      *        EA -  EXTRACTION/ACTION GROUP ID
      *        EL -  EXTENDED LATE CHARGE INFO
      *        FC -  FORM CHARGES
      *        FU -  FOLLOW-UP CODES
      *        FZ -  FROZEN REASON CODES
      *        GC -  FEE CODES
      *        ID -  ID TYPE (KNOW YOUR CUSTOMER SCREENS)
      *        JT -  JOB TYPE
      *        LO -  LPO (TFC)
      *        LR -  TRIAD SCORING
      *        LS -  REPO REASON CODE
      *        LV -  GROUP LEVELS
      *        ML -  MAIL LIST SOURCE CODES
      *        PA -  PAYROLL CODES (TIME SHEET PROCESSING)
      *        PC -  PROCESS CODE   (TFC)
      *        PH -  PHONE NUMBER DESCRIPTIONS
      *        PL -  POOL ID CODES
      *        PO -  PAYOFF CODES
      *        PR -  PRODUCTS CODES (REVOLVING)
      *        PS -  PAYMENT RESCHEDULE CODE (USED THIS TO MATCH POSTING CODE)
      *        PU -  PURPOSE CODES
      *        PX -  P&L CODES
      *        RC -  REP CODE  (TFC)
      *        RE -  COLLECTION RESULT CODE
      *        RF -  TURN DOWN (REFUSAL CODES/LOAN APPLICATION STATUS)
      *        RL -  RELATION CODES
      *        RO -  REVOLVING OTHER CHARGE CODES
      *        SR -  SOURCE CODES
      *        TC -  TRAN CODE REFERENCE PAYMENTS
      *        TS -  TRW SPECIAL CODES
      *        UW -  UNWIND SALES FINANCE VOID
      *
      *       CODE:  TYPE DEPENDENT
      *
      *-----------------------------------------------------------------
      *   NOTE:  ON ACTION CODES CD-TYPE WILL BEGIN WITH {
      *          AND THE 2ND CHARACTER OF CD-TYPE WILL
      *          BE PART OF CD-CODE
      *
      *-----------------------------------------------------------------
      * REV:
      **************************************************************************
      *
      *    STANDARD KEY FORMAT
      *
      *-------------------------------------------------------------------------
      *        AT -  ACTIVITY CODES
      *        CO -  COLLATERAL CODES
      *        EV -  EVENT CODES
      *        ML -  MAIL LIST SOURCE CODES
      *        PH -  PHONE NUMBER DESCRIPTIONS
      *        PU -  PURPOSE CODES
      *        RL -  RELATION CODES
      *        SR -  SOURCE CODES
      ******************************************************************
       78  XCD-SIZE                              VALUE 74.
       01  XCD-REC.
           03  XCD-TYPE                       PIC X(2).
           03  XCD-TYPE-DESC                  PIC X(25).

           03  XCD-CODE                       PIC X(6).
           03  XCD-DESCRIPTION                PIC X(35).

           03  XCD-BRANCH                     PIC 9(4).

           03  XCD-CR                         PIC X.
           03  XCD-LF                         PIC X.
