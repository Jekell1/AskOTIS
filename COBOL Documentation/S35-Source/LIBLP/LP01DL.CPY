      *COPYMEMBER: LIBLP/LP01DL
      ******************************************************************
      *
      *            ( DEALER MASTER FILE )
      *
      *    DL1-KEY - DL-BRNO, DL-NUM
      *    DL2-KEY - NON-CONTIGUOUS, DL-BRNO, DL-SHORT      
      *
      *=================================================================
      * REV:
      * JTG 032596 ADDED DL-RVCHGS-FRMLA = 1, RELIANCE 
      *            TAKE A PERCENTAGE OF INTCHG
      * MJD 040896 ADDED DL-AFP-DEALER.
      * BAH 030797 ADDED DL-RECOURSE-FRMLA (FAIRCO, FL)
      * BAH 061397 ADDED DL-RVPRIN-FRMLA 01 FOR FEDERATED PR#508
      * JTG 062797 ADDED "A" AND "C" OPTIONS FOR DL-PARVPAID-CODE RE: #515
      * BAH 022398 ADDED DISC-FRMLA 5 FFG, PR# 653, AMTFIN - OWNED INS & FEES
      * BAH 040198 ADDED DL-RVPRIN-FRMLA 02 FOR FUTURE PR#677
      * KEC 100698 ADDED DL-FAX-NUMBER. REGENCY REVOLVING
      * JTG 102098 ADDED DL-PARVPAID-FRMLA-IB & DL-PARVPAID-CODE-IB "C"
      *            INSTANT AUTO #744
      * KEC 111198 ADDED DL-REVOLVING; REGENCY REVOLVING
      * MJD 990126 ----- UPDATED IN FLEXGEN -----
      * CS  990324 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 990701 ADDED DL-LPO FOR TFC REQ#31
      * MJD 990722 ADDED 88'S TO AFP-DEALER
      * KEC 990805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * JTG 000217 ADDED DL-DISC-CODE-NO-PL-EXP                      ACE, #980
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLB 020104 ADD FIELDS: DL-ACH-FG, DL-BANK-ROUTING-NO,
      *            DL-BANK-ACCTNO, REGACC PR# 1691
      * BLV 020111 ADDED DL-ACH-PRENOTE-FG, REGACC PR# 1691
      * JTG 030613 ADDED LOGIC FOR SALES FINANCE AMORTIZATED PARTICIPATION 
      *            RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE 
      *            AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      * CS 060222 ADD DL-RSV-REQD-STD-PCT, DL-RSV-REQD-60-PCT
      *                                                [SAC PR#2503, #2512]
      * BAH 060407 ADDED REDEFINES OF DL-SVACCT-LCHGPER, DL-SVACCT-INTPER AND
      *            DL-SVACCT-DEFPER TO HAVE DECIMAL, UNION PR# 2756
      *  CS 060411 CHANGED DL-RSV-REQD-STD-PCT,DL-RSV-REQD-60-PCT TO MISC 
      *            PERCENTS TO BE USED BY SAL FOR DEALER PLANS 1,2,3
      * BAH 061110 REDEFINED DL-SIGN3 AS DL-DBA-NAME FOR REGACC PR# 155
      *  CS 090203 ADD DL-USE-PARTIC-FLAT-TABLE Y/N   REGIONAL 237
      * BAH 100917 ADDED DL-WAIVE-ACH-FEE FOR MIDATL PR# 3650
      * BAH 140310 ADDED DL-TRACK-CHANGE-FLAG, REGACC PR# 350
      *  CS 160224 RENAMED DL-BRANCH TO DL-BRNO, MADE DL-DBA-NAME ITS OWN
      *             FIELD AND INCREASED SIZE FROM 25 TO 40
      *  CS 190422 ADD FUTURE FIELDS, REMOVE ENDING FILLER
      * BAH 20190627 REMOVED DL-REVOLVING
      * BAH 20190702 REMOVED DL-USE-PARTIC-FLAT-TABLE PT0051
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      * BAH 20230600 ADDED FILLER
      ******************************************************************
       78  DL-SIZE                        VALUE  965.
       01  DL-REC.
           03  DL1-KEY.
               05  DL-BRNO                PIC 9(4).              
               05  DL-NUM                 PIC 9(6).
               05  DL-NUM-RED            REDEFINES DL-NUM.
                   07  DL-NO              PIC 9(4).
                   07  DL-BR              PIC 9(2).
               05  DL-DLNO               REDEFINES DL-NUM
                                          PIC 9(4)V99.
      $XFD DATE=YYYYMMDD
           03  DL-LTOUCH-DATE             PIC 9(8) COMP-3.
           03  DL-NAME                    PIC X(25).
           03  DL-ADR1                    PIC X(25).
           03  DL-ADR2                    PIC X(25).
           03  DL-CITY                    PIC X(15).
           03  DL-STATE                   PIC XX.
           03  DL-ZIP                     PIC X(10).
           03  DL-SHORT                   PIC X(16).
      *-------------------------------------------------------------
           03  DL-PHONE                   PIC 9(10)    COMP-3.
           03  DL-PURPOSE                 PIC X(3).
           03  DL-MFGCODE                 PIC X(3).

      $XFD DATE=YYYYMMDD
           03  DL-ORIGDATE                PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  DL-TERMDATE                PIC 9(8)     COMP-3.

           03  DL-LC-CODE                 PIC 99.
      *-------------------------------------------------------------
      * RECOURSE ACCOUNT PARAMETERS:
           03  DL-FULLCEIL                PIC S9(7)V99 COMP-3.
           03  DL-PARTCEIL                PIC S9(7)V99 COMP-3.
           03  DL-NONCEIL                 PIC S9(7)V99 COMP-3.
      *-------------------------------------------------------------
      * AUTHORIZED SIGNATURES:
           03  DL-SIGN1                   PIC X(25).
           03  DL-SIGN2                   PIC X(25).
           03  DL-SIGN3                   PIC X(25).
      *-------------------------------------------------------------
      * EARNINGS CODES:
           03  DL-MAXPURTRM               PIC 9(4)  COMP-3.
           03  DL-MAXPURPER               PIC 9(4)  COMP-3.
           03  DL-EARNCD                  PIC 99    COMP-3.
               88  DL-EARNCD-VALID        VALUE 0 1.
           03  DL-EARNFAC                 PIC 9(4)  COMP-3.
      *-------------------------------------------------------------
      * DEALER DISCOUNT PARAMETERS:
           03  DL-DISC-FRMLA              PIC 99       COMP-3.
               88  DL-DISC-FRMLA-VALID    VALUE 0 1 2 3 4 5.
           03  DL-DISC-CODE               PIC X.
               88  DL-DISC-CODE-VALID     VALUE " " "N".
               88  DL-DISC-CODE-NO-PL-EXP VALUE "N".
           03  DL-DISC-TAB                OCCURS 6.
               05  DL-DISC-TERM           PIC 9(3).
               05  DL-DISC-RATE           PIC 99V9(4)  COMP-3.
      *-------------------------------------------------------------
      * RESERVE FROM PRINCIPAL PARAMETERS:
           03  DL-RVPRIN-TBL              OCCURS 3.
               05  DL-RVPRIN-FRMLA        PIC 99       COMP-3.
                   88  DL-RVPRIN-FRMLA-VALID    VALUE 0 01 02 99.
               05  DL-RVPRIN-CODE         PIC X.
                   88  DL-RVPRIN-CODE-VALID         VALUE " ".
               05  DL-RVPRIN-ADDON        PIC S9(5)V99 COMP-3.
               05  DL-RVPRIN-TAB          OCCURS 6.
                   07  DL-RVPRIN-TERM     PIC 9(3).
                   07  DL-RVPRIN-RATE     PIC 99V9(4)  COMP-3.
      *-------------------------------------------------------------
      * RESERVE FROM CHARGES PARAMETERS:
           03  DL-RVCHGS-TBL              OCCURS 3.
               05  DL-RVCHGS-FRMLA        PIC 99       COMP-3.
                   88  DL-RVCHGS-FRMLA-VALID    VALUE 0 1 99.
               05  DL-RVCHGS-CODE         PIC X.
                   88  DL-RVCHGS-CODE-VALID     VALUE  " ".
               05  DL-RVCHGS-ADDON        PIC S9(5)V99 COMP-3.
               05  DL-RVCHGS-TAB          OCCURS 6.
                   07  DL-RVCHGS-TERM     PIC 9(3).
                   07  DL-RVCHGS-RATE     PIC 99V9(4)  COMP-3.
      *-------------------------------------------------------------
      * PARTICIPATING RESERVE WITHHELD PARAMETERS:
           03  DL-PARVHELD-FRMLA          PIC 99       COMP-3.
               88  DL-PARVHELD-FRMLA-VALID    VALUE 0 1 2.
           03  DL-PARVHELD-CODE           PIC X.
               88  DL-PARVHELD-CODE-VALID     VALUE " " "A" "B".
           03  DL-PARVHELD-ADDON          PIC S9(5)V99 COMP-3.
           03  DL-PARVHELD-TAB            OCCURS 6.
               05  DL-PARVHELD-TERM       PIC 9(3).
               05  DL-PARVHELD-RATE       PIC 99V9(4)  COMP-3.
      *-------------------------------------------------------------
      * PARTICIPATING RESERVE PAID PARAMETERS:
           03  DL-PARVPAID-FRMLA          PIC 99       COMP-3.
               88  DL-PARVPAID-FRMLA-VALID    VALUE 0 1 2 3.
           03  DL-PARVPAID-CODE           PIC X.
               88  DL-PARVPAID-CODE-VALID     VALUE " " "A" "C".
           03  DL-PARVPAID-ADDON          PIC S9(5)V99 COMP-3.
           03  DL-PARVPAID-TAB            OCCURS 6.
               05  DL-PARVPAID-TERM       PIC 9(3).
               05  DL-PARVPAID-RATE       PIC 99V9(4)  COMP-3.
      *-------------------------------------------------------------
      * SERVICE ACCOUNT PARAMETERS:
           03  DL-SVACCT-FRMLA            PIC 99       COMP-3.
               88  DL-SVACCT-FRMLA-VALID  VALUE 0 1.
           03  DL-SVCODE                  PIC X.
               88  DL-SVCODE-VALID        VALUE " ".
           03  DL-SVACCT-AMT              PIC S9(5)V99 COMP-3.
           03  DL-SVACCT-RATE             PIC 999V99   COMP-3.
           03  DL-SVACCT-LCHGPER          PIC 999      COMP-3.
           03  DL-SVACCT-LCHGPER-DEC REDEFINES DL-SVACCT-LCHGPER
                                          PIC 99V9     COMP-3.
           03  DL-SVACCT-INTPER           PIC 999      COMP-3.
           03  DL-SVACCT-INTPER-DEC REDEFINES DL-SVACCT-INTPER
                                          PIC 99V9     COMP-3.
           03  DL-SVACCT-DEFPER           PIC 999      COMP-3.
           03  DL-SVACCT-DEFPER-DEC REDEFINES DL-SVACCT-DEFPER
                                          PIC 99V9     COMP-3.
      *-------------------------------------------------------------
      * RECOURSE ACCOUNT BALANCES:
           03  DL-FULLCEIL-BAL            PIC S9(9)V99 COMP-3.
           03  DL-PARTCEIL-BAL            PIC S9(9)V99 COMP-3.
           03  DL-NONCEIL-BAL             PIC S9(9)V99 COMP-3.
      *-------------------------------------------------------------
      * AFP FLAG
      *-----------------------------------------------------------------
      * AFP FLAG: ONLY ALLOW ENTRY OF "Y" OR "N" IN MAINT.
      *           INTERNALLY LET " " = "N" WHEN TESTING.
      *-----------------------------------------------------------------
           03  DL-AFP-DEALER              PIC X.
               88 DL-AFP-DEALER-N         VALUE " " "N".
               88 DL-AFP-DEALER-Y         VALUE "Y".
               88 DL-AFP-DEALER-VALID     VALUE "Y" "N".
      *-------------------------------------------------------------
           03  DL-RECOURSE-FRMLA          PIC X.
           03  DL-FAX-NUMBER              PIC 9(10)    COMP-3.
      *-------------------------------------------------------------
      * PARTICIPATING RESERVE PAID (IB) PARAMETERS:
           03  DL-PARVPAID-FRMLA-IB       PIC X.
               88  DL-PARVPAID-FRMLA-IB-VALID    VALUE " " "A" "3".
      * NOTE: ' ' 'C' AND 'D' DON'T PROVIDE FOR RECAPTURE AND THE 
      *       CURRENT LOAN MANAGER LOGIC ASSUMES THAT AND DOSN'T TEST FOR IT
           03  DL-PARVPAID-CODE-IB        PIC X.
               88  DL-PARVPAID-CODE-IB-VALID     VALUE " " "C".
      *-------------------------------------------------------------
           03  DL-LPO                     PIC X(4).
      * GLB 01/04/02 NEW FIELDS
           03  DL-ACH-FG                  PIC X.
           03  DL-BANK-ROUTING-NO         PIC 9(9) COMP-3.
           03  DL-BANK-ACCTNO             PIC X(17).
           03  DL-ACH-PRENOTE-FG          PIC X.
      * DL-MIN-RSV-REQD REQUIRED RESERVE BALANCE AS A % TO THAT DEALER'S
      *                 PORTFOLIO
      * DL-RSV-REQD-60 ADDITIONAL RESERVE % ON PORTFOLIO 60+
       
           03  DL-SAL-MISC-PCT            PIC 99V99 COMP-3.
           03  DL-SAL-MISC-PCT2           PIC 99V99 COMP-3.
           03  DL-WAIVE-ACH-FEE           PIC X.
           03  DL-TRACK-CHANGE-FLAG       PIC X.
           03  DL-DBA-NAME                PIC X(40).
           03  FILLER                     PIC X(150).
