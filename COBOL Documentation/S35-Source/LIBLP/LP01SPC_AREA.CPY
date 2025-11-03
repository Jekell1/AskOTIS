      *                     SIZE = 2542
      *
      * REDUCED SPC-INSEARN-ORDER TO 12 FROM 14 TO MATCH INSTBL
      * REMOVED ALL SPC-REVOLVING FIELDS
      * BAH 2020.0128 ADDED SP-RBEARLY 28, IL, WORLD #1417
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210428 REMOVED FUTURE FIELDS, RESIZED FILE
      * BAH 2021.0617 ADDED SP-RBEARLY 97, TN, WORLD #1500
      * BAH 2023.0301 SP-INS-O1-EDIT ADDED "I" IUI AND "C" CPI/AUTO, #1594
      * BAH 2023.0517 DISABLED CALIFORNIA IB RECASTING FORMULA 2  KM
      * BAH 20230614 ADDED FILLER
      * BAH 2023.0801 ADDED SP-RBFRMLA2 "Q", "AL" CLOSING FEE REFUND #1619

           03  SPC-AREA-C.

      *NEW, FOR STORING INSURANCE EARNINGS.
               05  SPC-INSEARN-ORDER        PIC 9(2) OCCURS 12.
      *--------------------------------------------------------
      *    SPC-INSAH-JOINT-FG   PAGE 9 OF SPRS
      *        Y - A&H IS JOINT
      *        N - A&H IS NOT JOINT
      *--------------------------------------------------------
               05  SPC-INSAH-JOINT-FG            PIC X.

      * OTHER 1 & 2 INSURANCE EDITS:
               05  SPC-INS-O1-EDIT               PIC X.
                   88 SPC-INS-O1-EDIT-VALID          VALUE " " "A"
                                                           "I" "C".
               05  SPC-INS-O2-EDIT               PIC X.
                   88 SPC-INS-O2-EDIT-VALID          VALUE " " "A".

      *----------------------------------------------------
      *    INSURANCE AND CHARGES REBATE TABLE
      *
      *  N O T E:
      *          IF THE PICTURE OF SPC-REBTBL CHANGES
      *          YOU MUST CHANGE: LPCERN & LPAERN
      *
      *    LEVELS: 1 - CL DECL  SINGLE   10 - DEFERMENTS   19 - NEW UNEARN
      *            2 - CL  "    JOINT    11 - PP 2         20 - NEW UNEARN
      *            3 - CL WHOLE SINGLE   12 - OTH 1        21 - NEW UNEARN
      *            4 - CL  "    JOINT    13 - OTH 2        22 - NEW UNEARN
      *            5 - AH                14 - OTH 3        
      *            6 - PP                15 - OTH 4        
      *            7 - INTEREST          16 - OTH 5
      *            8 - SERVICE CHG       17 - NEW INS  
      *            9 - MAINT. FEE        18 - NEW INS   
      *----------------------------------------------------
      *ADD 2 NEW INSURANCES(15/16 NOT CURRENTLY USED) AND 4 NEW UNEARNEDS
               05  SPC-REBTBL                           OCCURS 22.
      *CHANGED FROM PIC X TO PIC X(2).
                   07  SPC-RBFRMLA               PIC X(2).
                       88  SPC-RBFRMLA-VALID         VALUE " "
                                                         "0" THRU "Z".
                   07  SPC-RBTBL                        OCCURS 2.
                       09  SPC-RBDTE             PIC X.
                           88  SPC-RBDTE-VALID       VALUE "A" "B"
                                                         "C" "D".
                       09  SPC-RBDAYS            PIC 9(2)    COMP-3.
                       09  SPC-RBYRTYPE          PIC 9(4)    COMP-3.
                           88  SPC-RBYRTYPE-VALID    VALUE 358 360
                                                         365 367.
      *MINIMUM RETENTION DO WE NEED TO INCREASE
                   07  SPC-RBMINRET              PIC 9999V99 COMP-3.
      *NUMBER OF MONTHS REMAINING TO MATURITY THAT NEGATES REBATES
                   07  SPC-RBNOMON               PIC 9(2)    COMP-3.
                   07  SPC-RBDEDCHG              PIC 9999V99 COMP-3.
                   07  SPC-RBRECAST              PIC 9(2)    COMP-3.
                       88  SPC-RBRECAST-VALID        VALUE 0 THRU 1.
                   07  SPC-RBEARLY               PIC 9(2)    COMP-3.
                       88  SPC-RBEARLY-VALID         VALUE 0 THRU 28
                                                         97 98 99.
                   07  SPC-RBDEFER               PIC 9(2)    COMP-3.
                       88  SPC-RBDEFER-VALID         VALUE 0 1 2.
                   07  SPC-RBPCT                 PIC 9(3)V9  COMP-3.
                   07  SPC-RBREDUC               PIC 9(4)V99 COMP-3.
                   07  SPC-RBMIN                 PIC 9(4)V99 COMP-3.
                   07  SPC-RBSPOPT1              PIC 9(2)    COMP-3.
                       88  SPC-RBSPOPT1-VALID        VALUE 0 THRU 28.
                       88  SPC-RBSPOPT1-FIRST-30     VALUE 13 14.
                   07  SPC-RBSPOPT2              PIC 9(2)    COMP-3.
                       88  SPC-RBSPOPT2-VALID        VALUE 0 THRU 13.
                   07  SPC-RBMANAGER             PIC 9(2)    COMP-3.
                   07  SPC-RBUNITPER-CD          PIC X.
                       88  SPC-RBUNITPERCD-CD-VAILD  VALUE " " "A" "B".
      *CHANGED FROM PIC X TO PIC X(2)
                   07  SPC-RBFRMLA2              PIC X(2).
                       88  SPC-RBFRMLA2-VALID        VALUE "1" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                       "9" "A" "B" "C"
                                                       "D" "E" "F" "G"
                                                       "H" "I" "J" "K"
                                                       "L" "M" "N" "O"
                                                       "P" "Q".

                   07  FILLER                    PIC X(15).

               05  SPC-RBCOMCD                   PIC X.
                   88  SPC-RBCOMCD-VALID             VALUE " " "A" "B"
                                                         "C".
                   88  SPC-RBCOMCD-1                 VALUE "A".
                   88  SPC-RBCOMCD-10                VALUE "B".
                   88  SPC-RBCOMCD-5                 VALUE "C".
      *--------------------------------------------------------
      * ADDON RELATED FIELDS:
      *--------------------------------------------------------
               05  SPC-ADDON-DISFRMLA           PIC X.
                   88  SPC-ADDON-DISFRMLA-VALID     VALUES "A" "C".
               05  SPC-ADDON-WHOLE-FG           PIC X.
                   88  SPC-ADDON-WHOLE-FG-VALID     VALUES " " "A".
               05  SPC-ADDON-ORG-MATURITY       PIC X.
                   88  SPC-ADDON-ORG-MATURITY-VALID VALUES "O" "U".
               05  SPC-CONSOLIDATE-RT-FG        PIC X.
                   88  SPC-CONSOLIDATE-RT-FG-VALID  VALUES " " "C".
               05  SPC-1STPY-ADDON-DAYS         PIC 9(3) COMP-3.
               05  SPC-1STPY-ADDON-YRTYPE       PIC 9(3) COMP-3.
                   88  SPC-1STPY-ADDON-YRTYPE-VALID VALUE 358 360 365
                                                        367 999.
               05  SPC-ADDON-RATE-FG            PIC X.
                   88  SPC-ADDON-RATE-FG-VALID      VALUES "A" "F" "E".
               05  SPC-ADDON-RATE               PIC 999V999 COMP-3.
      *--------------------------------------------------------
      * FEE FORMULAS AND INFO
      *--------------------------------------------------------
      *CALCULATED FEES AT LOAN INPUT FOR DOC STAMPS & SALES TAX AT 6%
      *DOES THIS NEED INCREASED TO ACCOMODATE ADDITIONAL FEES
               05  SPC-FEETABLE                    OCCURS 2.
                   07  SPC-FEEFRMLA         PIC X.
                       88  SPC-FEEFRMLA-VALID   VALUE " " "A" "B" "C".
                   07  SPC-FEECD            PIC XX.
                   07  SPC-FEEID            PIC X(7).

      *--------------------------------------------------------
      * MISC FIELDS INFO
      *--------------------------------------------------------
               05  SPC-ROUND-PAY-FG             PIC X.
               05  SPC-IRREGULAR-PAY-FG         PIC X.
               05  SPC-DLRVCHGS-RB-CD                 OCCURS 3
                                           PIC X.
                   88  SPC-DLRVCHGS-RB-CD-VALID     VALUE " " "A" "Z".
               05  SPC-PARVHELD-RB-CD           PIC X.
                   88  SPC-PARVHELD-RB-CD-VALID     VALUE " " "A" "Z".
               05  SPC-PARVPAID-RB-CD           PIC X.
                   88  SPC-PARVPAID-RB-CD-VALID     VALUE " " "A" "Z".
               05  SPC-CONTR-CREDIT             PIC 99V99 COMP-3.
               05  SPC-CONTR-CREDIT-CD          PIC X.
                   88  SPC-CONTR-CREDIT-CD-VALID    VALUES " " "A" "B".
               05  SPC-CONTR-CREDIT-MIN-DEL-PER PIC 999V9 COMP-3.
               05  SPC-OPT-CAL-CHGABLE          PIC X.
                   88  SPC-OPT-CAL-CHGABLE-VALID    VALUE " " "P" "I"
                                                        "S" "B" "3".
                   88  SPC-OPT-CAL-NONE             VALUE " ".
                   88  SPC-OPT-CAL-PROCEEDS         VALUE "P".
                   88  SPC-OPT-CAL-PRO-INS          VALUE "I".
                   88  SPC-OPT-CAL-PRO-SC           VALUE "S".
                   88  SPC-OPT-CAL-PRO-INS-SC       VALUE "B".
                   88  SPC-OPT-CAL-PRO-INS-SC-NO-O3 VALUE "3".
               05  SPC-PBDEL-DAYS               PIC S999   COMP-3.
               05  SPC-PBINCR-PERCENT           PIC 999    COMP-3.
               05  SPC-CPOT30-CD            PIC X.
                   88  SPC-CPOT30-CD-VALID      VALUE " " "A".
               05  SPC-CPOTCHGOFF-CD        PIC X.
                   88  SPC-CPOTCHGOFF-CD-VALID  VALUE " " "A" "B" "C".
               05  SPC-RDPOT30-CD           PIC X.
                   88  SPC-RDPOT30-CD-VALID     VALUE " ".
               05  SPC-RDPOTCHGOFF-CD       PIC X.
                   88  SPC-RDPOTCHGOFF-CD-VALID VALUE " " "A".
      ***************************************************************
      * SPC-MAX-ALLOW-DELINQ 9(2) MATCH TO LN-TOTNODEFACT
               05  SPC-MAX-ALLOW-DELINQ         PIC 99.
               05  SPC-RESCIND-BUS-DAYS         PIC X.
                   88  SPC-RESCIND-BUS-DAYS-VALID   VALUE "0" THRU "8".
               05  SPC-RESCIND-SATURDAY         PIC X.
                   88  SPC-RESCIND-SATURDAY-VALID   VALUE "Y" "N".
                   88  SPC-RESCIND-SATURDAY-YES     VALUE "Y".

      *--------------------------------------------------------
      *    MISC FIELDS
      *--------------------------------------------------------
               05  SPC-MIN-CREDIT-SCORE        PIC 9(4) COMP-3.

               05  SPC-DUAL-IN-BRANCH          PIC X.
               05  SPC-DUAL-IN-STATE           PIC X.
               05  SPC-PACESETTER-FG           PIC X.
               05  SPC-WAGE-ASSIGN-DAYS        PIC 99  COMP-3.
               05  SPC-CAL-PAYMENT-ROUND-FG    PIC X.
               05  SPC-CAN-LOANS-FG            PIC X.
                   88  SPC-CAN-LOANS-YES           VALUE "Y" " ".
                   88  SPC-CAN-LOANS-NO            VALUE "N".
                   88  SPC-CAN-LOANS-ONLY          VALUE "O".
                   88  SPC-CAN-LOANS-VALID         VALUE "Y"
                                                     "N" "O".
               05  SPC-ALLOW-DELINQ-POLCD      PIC X.
                   88  SPC-ALLOW-DELINQ-POLCD-VALID VALUE " " "A".
               05  SPC-YTD-MAX-ALLOW-DELINQ    PIC 99.

      *=================================================================
               05  SPC-LNCOST-FRMLA               PIC X.
                   88  SPC-LNCOST-FRMLA-VALID     VALUE " " "A" "D"
                                                        "F" "P" "3"
                                                        "R" "M" "4"
                                                        "5".
               05  SPC-LNCOST-AMT                 PIC 9(7)V99 COMP-3.
               05  SPC-DLDISC-RVPRIN1-PCNT        PIC 9(3)V99 COMP-3.

               05  SPC-RENEWABLE-PAYS             PIC S9(4) COMP-3.
               05  SPC-RENEWABLE-DAYS             PIC S9(4) COMP-3.

               05  SPC-ALLOW-LN-BACK-DATE         PIC X.
               05  SPC-DLDISC-RVPRIN1-AF-PCNTX.
                   07  SPC-DLDISC-RVPRIN1-AF-PCNT PIC 9(3)V99 COMP-3.
               05  SPC-CAL-MAX-AT-ESTIMATE        PIC X.
      *------------------------------------------------------------
      *    SPC-INSAH-RETRO       PAGE 9 OF SPRS
      *         Y - A&H IS RETRO
      *         N - A&H IS NOT RETRO
      *    SPC-INSAH-DAYS-WAIT   PAGE 9 OF SPRS
      *            09 -  7 DAY
      *        14 - 14 DAY
      *------------------------------------------------------------
               05  SPC-INSAH-RETRO                PIC X.
               05  SPC-INSAH-DAYS-WAIT            PIC XX.
               05  SPC-ADDON-EXCEPT-FG            PIC X.

      *-----------------------------------------------------------------
      *    REPO INFORMATION (BASED ON THE TRCD AND FMNO FOR 4 BORROWERS
      *-----------------------------------------------------------------
               05  SPC-REPO-TRCD-FMNO-TABLE              OCCURS 3 TIMES.
                   07  SPC-REPO-TRCD              PIC X(02).
                   07  SPC-REPO-FMNO              PIC 9(03) OCCURS 4.

      *-----------------------------------------------------------------
               05  SPC-CANCEL-ADDON-FG            PIC X.
               05  SPC-DEFAULT-O2-INS-COMP        PIC 9(2).
               05  SPC-LC-ON-JUDGEMENTS           PIC X.
               05  SPC-DELQ-IB-AGE-ALP.
                   07  SPC-DELQ-IB-AGE            PIC 999      COMP-3.
               05  SPC-DELQ-IB-MAX-DAYS           PIC 999      COMP-3.
               05  SPC-INSURANCE-WRITTEN          PIC X.
               05  SPC-INTERIM-COUPON             PIC X.
               05  SPC-AUTO-ASSESS-LCHG           PIC X.
      * CHANGED FROM PIC 9(6)V99 TO 9(7) TO MATCH LN-PROCASH
               05  SPC-MAX-CASH-PROCEEDS          PIC 9(6)V99 COMP-3.
               05  SPC-ALLOW-ZERO-PROC            PIC X.
               05  SPC-BANKRULE-RATE              PIC 9(4)    COMP-3.
                   88  SPC-BANKRULE-RATE-VALID        VALUE 0 THRU 100.

      * PR# 245 REGACC 5/22/2009  (28 BYTES)
      *DOES THIS NEED INCREASED
               05  SPC-AUTO-SEND-TABLE              OCCURS 2 TIMES.
                   07  SPC-AUTO-SEND-TRCD         PIC X(02).
                   07  SPC-AUTO-SEND-FMNO         PIC 9(03) OCCURS 4.

               05  SPC-DLDISC-CONV-BF-PCNT        PIC 999V999 COMP-3.
               05  SPC-UNITPER-CD                 PIC X.
                   88  SPC-UNITPER-CD-VALID          VALUE " " "M" "S"
                                                         "B" "W".
               05  SPC-RENEWAL-EDIT               PIC X.
                   88  SPC-RENEWAL-EDIT-VALID     VALUES " " "A" "B" "C"
                                                       "D" "E".
               05  SPC-ROUND-MFEE                 PIC X.
                   88  SPC-ROUND-MFEE-VALID       VALUE " " "Y" "N".
                   88  SPC-ROUND-MFEE-YES         VALUE "Y".
               05  SPC-ADV-TO-1ST-NO-CORR         PIC X.
               05  SPC-LC-INT-ON-REPO-NON-PL      PIC X.
               05  SPC-CONTRACTUAL-SET-NONACCRUAL PIC 9(3) COMP-3.
               05  SPC-CONTRACTUAL-RESET-ACCRUAL  PIC 9(3) COMP-3.
               05  SPC-MONTHS-PAY-NAPP-COUNTER    PIC 99   COMP-3.
               05  SPC-SHORT-PAY-THRESHOLD        PIC 9(3).
               05  SPC-CASH-AVAIL-PERCENT         PIC 99.
               05  SPC-O1-JOINT-FG                PIC X.
      * PROCEEDS OF NEW LOAN MUST BE THIS PERCENT
               05  SPC-NET-BAL-REQ                PIC S9(3)    COMP-3.
               05  SPC-AMT-PROCEEDS-REQ           PIC S9(7)    COMP-3.
               05  SPC-ALT-PREPAY-MONTHS          PIC 9(2).    
               05  SPC-PERCENT-REGPAY-AVAIL       PIC 9(3).
      * THESE ALT-DEF FIELDS WERE ADDED FOR REGACC ONLY. 
               05  SPC-ALT-DEFFRMLA               PIC 99 COMP-3.
                   88  SPC-ALT-DEFFRMLA-VALID      VALUE 0 THRU 10
                                                    95 96 97 98 99.
                   88  SPC-ALT-DEFFRMLA-IB-FORMULAS VALUE 95 96 97
                                                          98 99.
               05  SPC-ALT-DEFMIN                 PIC 99V99 COMP-3.
               05  SPC-ALT-DEFMAX                 PIC 9999V99 COMP-3.
               05  SPC-NON-LOAN-FLAG              PIC X.
               05  SPC-SKIP-TRW                   PIC X.

               05  FILLER                         PIC X(854).
