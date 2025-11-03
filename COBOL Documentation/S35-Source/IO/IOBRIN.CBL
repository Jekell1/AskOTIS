       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOBRIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOBRIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : BRFILE 
      * TABLE DESC    : BRANCH MASTER FILE
      * OUTPUT FILE   : XPC/IOBRIN.PRE
      * USED FOR      : LIBGB/GBWSGR.CPY --- QBR-REC
      *                 LIBGB/GBBRRN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0511 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOBRIN.CBL S34 05/17/21-16:08:31 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QBR-WHERE-BEG-KEY.
           03  QBR-WBEG-NO                            PIC 9(4).
 
       01  QBR-WHERE-END-KEY.
           03  QBR-WEND-NO                            PIC 9(4).
 
       01  QBR-REC.
           03  QBR-NO                                 PIC 9(4).
           03  QBR-LTOUCH-DATE                        PIC X(10).
           03  QBR-NAME                               PIC X(40).
           03  QBR-ADR1                               PIC X(24).
           03  QBR-ADR2                               PIC X(24).
           03  QBR-CITY                               PIC X(15).
           03  QBR-STATE                              PIC XX.
           03  QBR-ZIP                                PIC X(10).
           03  QBR-PHONE                              PIC 9(10).
           03  QBR-RETAIN                             PIC 9(11).
           03  QBR-NEXT-CHECK                         PIC 9(6).
           03  QBR-PRINTER-1                          PIC 99.
           03  QBR-PRINTER-2                          PIC 99.
           03  QBR-PRINTER-3                          PIC 99.
           03  QBR-PRINTER-4                          PIC 99.
           03  QBR-FEE-GL-BRNO                        PIC 9(4).
           03  QBR-REMIT1                             PIC X(30).
           03  QBR-REMIT2                             PIC X(30).
           03  QBR-REMIT3                             PIC X(30).
           03  QBR-STMNT1                             PIC X(60).
           03  QBR-STMNT2                             PIC X(60).
           03  QBR-INS-PRODUCER-NUM                   PIC 9(8).
           03  QBR-DLRCHK                             PIC X.
           03  QBR-FRONT-END-FG                       PIC X.
           03  QBR-EMLNRG-PHONE                       PIC X.
           03  QBR-POST-GL                            PIC X.
           03  QBR-ALLOTMNTS                          PIC X.
           03  QBR-EXTRACT-AUTO-UPD                   PIC X.
           03  QBR-PCNI-LEDGER                        PIC X.
           03  QBR-PRINTERFG-1                        PIC X.
           03  QBR-PRINTERFG-2                        PIC X.
           03  QBR-PRINTERFG-3                        PIC X.
           03  QBR-PRINTERFG-4                        PIC X.
           03  QBR-NEGDLRSV                           PIC X.
           03  QBR-CPCORP-ORDER                       PIC X.
           03  QBR-PRIO-MEMO-FLAG                     PIC X.
           03  QBR-PREBILL-PL-JD-OPT                  PIC X.
           03  QBR-LOAN-NUM-FLAG                      PIC X.
           03  QBR-DOWNPY-FLAG                        PIC X.
           03  QBR-AUTO-INS-EARN                      PIC X.
           03  QBR-OTH-INS-LEVELS                     PIC X.
           03  QBR-ALLOW-INTERBR                      PIC X.
           03  QBR-UNER-DLANRP                        PIC X.
           03  QBR-SECOND-PROMISE                     PIC X.
           03  QBR-AVAIL-CREDIT                       PIC X.
           03  QBR-RECP1                              PIC X(30).
           03  QBR-RECP2                              PIC X(30).
           03  QBR-INC-WAIVERS                        PIC X.
           03  QBR-INC-INS                            PIC X.
           03  QBR-BP-NEXT-BATCH                      PIC 9(2).
           03  QBR-BP-NOPAYOFF                        PIC X.
           03  QBR-BP-ODDPAY                          PIC X.
           03  QBR-BP-LC                              PIC X.
           03  QBR-BP-NOPRIN                          PIC X.
           03  QBR-BP-FLASH                           PIC X.
           03  QBR-BP-ACTIONCD                        PIC X.
           03  QBR-BP-MAXCON                          PIC 9(3).
           03  QBR-BP-MAXREC                          PIC 9(3).
           03  QBR-BP-PL                              PIC X.
           03  QBR-BP-JD                              PIC X.
           03  QBR-BP-BRANCH                          PIC 9(5).
           03  QBR-AGE-ERN                            PIC X.
           03  QBR-OTHER-BRANCH-GL                    PIC X.
           03  QBR-SALES-SMALL                        PIC X.
           03  QBR-NEW-COLL-INQUIRY                   PIC X.
           03  QBR-CHANGE-MEMO                        PIC X.
           03  QBR-INS-ADDON                          PIC X.
           03  QBR-TRAILER-REC                        PIC X.
           03  QBR-ALDEL-FLAG                         PIC X.
           03  QBR-IB-LCHG-MESS                       PIC X.
           03  QBR-COL-QUEUE-PAST-DUE                 PIC X.
           03  QBR-LI-DLCOMM-CMSI-PP-MAX              PIC 9(4)V99.
           03  QBR-ADDON-FEECD-O2-1                   PIC XX.
           03  QBR-ADDON-FEECD-O2-2                   PIC XX.
           03  QBR-AUTO-CHECK                         PIC X.
           03  QBR-RECEIPT-PROMDATE                   PIC X.
           03  QBR-RECEIPT-INTPD                      PIC X.
           03  QBR-COLL-ID-SCAN                       PIC X.
           03  QBR-STOP-ON-COLL                       PIC X.
           03  QBR-DUEDATE-CHANGE-CD                  PIC X.
           03  QBR-CHECK-EOM-FG                       PIC X.
           03  QBR-VOLUME-DLANRP                      PIC X.
           03  QBR-PDTH-NXDUE                         PIC X.
           03  QBR-LC-CODE                            PIC X.
           03  QBR-VALIDATE-PRUS                      PIC X.
           03  QBR-AUTO-OPEN-BAL                      PIC X.
           03  QBR-OPEN-BAL-1                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-2                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-3                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-4                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-5                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-6                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-7                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-8                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-9                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-10                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-11                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-12                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-13                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-14                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-15                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-16                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-17                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-18                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-19                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-20                        PIC S9(7)V99.
           03  QBR-TENDER-CD-1                        PIC X(5).
           03  QBR-TENDER-CD-2                        PIC X(5).
           03  QBR-TENDER-CD-3                        PIC X(5).
           03  QBR-DLPARV-ADJ-FG                      PIC X.
           03  QBR-OVERRIDE-BR-STATE                  PIC X.
           03  QBR-DEFAULT-SC-CLASS                   PIC 9(3).
           03  QBR-RECDEL-NOT-REC                     PIC X.
           03  QBR-POST-AHEAD-FG                      PIC X.
           03  QBR-CAN-LOANS-FG                       PIC X.
           03  QBR-NON-MONTHLY-CAN-LOANS              PIC X.
           03  QBR-BP-ALLOW-PMT-ACCT-OTHBAL           PIC X.
           03  QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL         PIC X.
           03  QBR-REGION-NUMBER                      PIC 9(2).
           03  QBR-OUR-FEE                            PIC X.
           03  QBR-REB-METHOD-1                       PIC 99.
           03  QBR-REB-METHOD-2                       PIC 99.
           03  QBR-REB-METHOD-3                       PIC 99.
           03  QBR-REB-METHOD-4                       PIC 99.
           03  QBR-REB-METHOD-5                       PIC 99.
           03  QBR-REB-METHOD-6                       PIC 99.
           03  QBR-REB-METHOD-7                       PIC 99.
           03  QBR-REB-METHOD-8                       PIC 99.
           03  QBR-REB-METHOD-9                       PIC 99.
           03  QBR-REB-METHOD-10                      PIC 99.
           03  QBR-REB-METHOD-11                      PIC 99.
           03  QBR-REBDAYS-MONTH-1                    PIC 99.
           03  QBR-REBDAYS-MONTH-2                    PIC 99.
           03  QBR-REBDAYS-MONTH-3                    PIC 99.
           03  QBR-REBDAYS-MONTH-4                    PIC 99.
           03  QBR-REBDAYS-MONTH-5                    PIC 99.
           03  QBR-REBDAYS-MONTH-6                    PIC 99.
           03  QBR-REBDAYS-MONTH-7                    PIC 99.
           03  QBR-REBDAYS-MONTH-8                    PIC 99.
           03  QBR-REBDAYS-MONTH-9                    PIC 99.
           03  QBR-REBDAYS-MONTH-10                   PIC 99.
           03  QBR-REBDAYS-MONTH-11                   PIC 99.
           03  QBR-ACCRUAL-MONTHS-1                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-2                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-3                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-4                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-5                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-6                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-7                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-8                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-9                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-10                  PIC 999.
           03  QBR-ACCRUAL-MONTHS-11                  PIC 999.
           03  QBR-GLOVERSHORT                        PIC 9(11).
           03  QBR-GLBANK                             PIC 9(11).
           03  QBR-GLONHAND                           PIC 9(11).
           03  QBR-BANKBAL                            PIC S9(9)V99.
           03  QBR-RETAIN-AMT                         PIC S9(9)V99.
           03  QBR-LCARDFG                            PIC X.
           03  QBR-DATA-PATH                          PIC X(6).
           03  QBR-MACHINE                            PIC XX.
           03  QBR-MANAGER                            PIC X(25).
           03  QBR-EOD-PROCEDURE                      PIC XXX.
           03  QBR-BP-PAYOFF-NONCASH                  PIC X.
           03  QBR-BP-PAYOFF-OV                       PIC X.
           03  QBR-LI-DLCOMM-CMSI-O3-MAX              PIC 9(4)V99.
           03  QBR-LR-GROUP-COLLID                    PIC X.
           03  QBR-UNITPER-FG                         PIC X.
           03  QBR-UNITPER-Y-D-FG                     PIC X.
           03  QBR-NO-PROMPT-FOR-OTHBR                PIC X.
           03  QBR-WORLD-BRANCH-TYPE                  PIC X.
           03  QBR-COL-QUEUE-BEHAVIOR-FG              PIC X.
           03  QBR-CONVERT-AUTO-ACCTNO                PIC X.
           03  QBR-TS-REPO-INS-FG                     PIC X.
           03  QBR-BOOK-SF-BR-ONLY                    PIC X.
           03  QBR-FORCE-VALID-CITY-COUNTY            PIC X.
           03  QBR-SET-COLLCD-DL-MFGCD                PIC X.
           03  QBR-DEFAULT-SF-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-SF-PURCD                   PIC X(3).
           03  QBR-DEFAULT-LN-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-LN-PURCD                   PIC X(3).
           03  QBR-TAP-OTHER-DL                       PIC X.
           03  QBR-AH-AND-CL                          PIC X.
           03  QBR-NO-CASH-PROCEEDS                   PIC X.
           03  QBR-RI-BEFORE-REPO                     PIC X.
           03  QBR-RI-BEFORE-P4P5                     PIC X.
           03  QBR-NOT-OURS-REFUND                    PIC X.
           03  QBR-SFDLO3-WINDOW-FG                   PIC X.
           03  QBR-CALC-INS-ADJ                       PIC X.
           03  QBR-AC-BEFORE-REPO                     PIC X.
           03  QBR-AC-BEFORE-P4P5                     PIC X.
           03  QBR-BANK-ROUTING-NO                    PIC 9(9).
           03  QBR-BANK-CLEARING-GL                   PIC 9(11).
           03  QBR-GLBANK-DEPOSIT                     PIC 9(11).
           03  QBR-BANK-ACCTNO                        PIC X(17).
           03  QBR-DEFAULT-SC-SUBCLASS                PIC 9(3).
           03  QBR-NO-OUT-DLR-RSV                     PIC X.
           03  QBR-COLLECTOR-QUEUE                    PIC X.
           03  QBR-COLL-QUEUE-CON-REC                 PIC X.
           03  QBR-JE-ALLOCATION-SKIP                 PIC X.
           03  QBR-EXPAND-APP-INPUT                   PIC X.
           03  QBR-A50-CPCORP                         PIC X.
           03  QBR-INBR-BILNSC-FG                     PIC X.
           03  QBR-INBR-BRIEF-FG                      PIC X.
           03  QBR-LEDGER-JUST-INTCHG                 PIC X.
           03  QBR-BYINQ-START-PAGE                   PIC 99.
           03  QBR-DSINQ-START-PAGE                   PIC 99.
           03  QBR-APP-REQUIRED-FOR-INPUT             PIC X.
           03  QBR-CREDIT-SCORING-FG                  PIC X.
           03  QBR-COL-QUEUE-LOW-TO-HI                PIC X.
           03  QBR-CAN-GROUP-ID                       PIC X(4).
           03  QBR-AUTO-LNSTAT-WINDOW                 PIC X.
           03  QBR-TIME-ZONE-FACTOR                   PIC S9.
           03  QBR-LONPF-OPT-DISPLAY                  PIC X.
           03  QBR-GIFTNET                            PIC X.
           03  QBR-GIFTNET-LEDGER-LINE                PIC X.
           03  QBR-NO-FT-EMP                          PIC 9(3).
           03  QBR-NO-PT-EMP                          PIC 99V99.
           03  QBR-BALLOON-LOANS-FG                   PIC X.
           03  QBR-GL-TYPE-ALLOWED                    PIC X.
           03  QBR-GL-ENTRY-DEFAULT                   PIC X.
           03  QBR-MIDSTREAM-1STPAY-FG                PIC X.
           03  QBR-NODEF-1ST-X-MONTHS                 PIC 9.
           03  QBR-ADD-INS-FEECODE                    PIC XX.
           03  QBR-ROAD-G-SGL-FEECODE                 PIC XX.
           03  QBR-ROAD-G-FAM-FEECODE                 PIC XX.
           03  QBR-NODEF-IF-ZERO-AMT                  PIC X.
           03  QBR-ASSESS-LATE-CHGS                   PIC X.
           03  QBR-NEXT-SOLICIT-NO                    PIC 9(6).
           03  QBR-ADDON-FEECD-O3-1                   PIC XX.
           03  QBR-ADDON-FEECD-O3-2                   PIC XX.
           03  QBR-IMMEDIATE-RECEIPT                  PIC X.
           03  QBR-IMMEDIATE-DOC-PRINT                PIC X.
           03  QBR-AUTO-PL-REF-AC                     PIC X.
           03  QBR-FAX-NUMBER                         PIC 9(10).
           03  QBR-ALPHA-DRAWERS                      PIC X.
           03  QBR-LATECHG-ON-ALLOTMENTS              PIC X.
           03  QBR-ALLOW-SYS-AS-EFFDATE               PIC X(01).
           03  QBR-TAX-PREBOOK-CUST                   PIC 9(4).
           03  QBR-TAX-PREBOOK-NONCUST                PIC 9(4).
           03  QBR-PASSWORD-DRAWER                    PIC X.
           03  QBR-ONESTEP-RN-FG                      PIC X.
           03  QBR-LI-LISCAN-FG                       PIC X.
           03  QBR-HIDE-END-BAL                       PIC X(01).
           03  QBR-LBOX-NOPAYOFF                      PIC X.
           03  QBR-LBOX-ODDPAY                        PIC X.
           03  QBR-LBOX-LC                            PIC X.
           03  QBR-LBOX-NOPRIN                        PIC X.
           03  QBR-LBOX-FLASH                         PIC X.
           03  QBR-LBOX-ACTIONCD                      PIC X.
           03  QBR-LBOX-MAXCON                        PIC 9(3).
           03  QBR-LBOX-MAXREC                        PIC 9(3).
           03  QBR-LBOX-PL                            PIC X.
           03  QBR-LBOX-JD                            PIC X.
           03  QBR-LBOX                               PIC X(01).
           03  QBR-LATECHG-ON-LBOX                    PIC X(01).
           03  QBR-ALLOW-OV-TRCD                      PIC X.
           03  QBR-GLOBAL-QUEUE                       PIC X(01).
           03  QBR-LBOX-PAYOFF-NONCASH                PIC X.
           03  QBR-LBOX-PAYOFF-OV                     PIC X.
           03  QBR-COLLECT-MANAGER                    PIC X(25).
           03  QBR-COLLECT-PHONE                      PIC 9(10).
           03  QBR-COLLECT-EXT                        PIC 9(4).
           03  QBR-ALT-INTO-THIS-BRANCH               PIC X.
           03  QBR-POFF-TITLE-WINDOW                  PIC X.
           03  QBR-COUPON-STATEMENT                   PIC X.
           03  QBR-SUPERVISOR                         PIC X(25).
           03  QBR-AREA-MANAGER                       PIC X(25).
           03  QBR-MIN-MONTHS-DEF                     PIC 99.
           03  QBR-CC-FLAG                            PIC X.
           03  QBR-CC-ADR1                            PIC X(24).
           03  QBR-CC-CITY                            PIC X(15).
           03  QBR-CC-STATE                           PIC XX.
           03  QBR-CC-ZIP                             PIC X(10).
           03  QBR-MIN-MONTHS-D2                      PIC 99.
           03  QBR-SKIP-MIDSTREAM-TRW                 PIC X.
           03  QBR-GL-ESCROW-BALANCE                  PIC 9(11).
           03  QBR-GL-ESCROW-PAYABLE                  PIC 9(11).
           03  QBR-ALLOW-FORCE-OPEN-FG                PIC X.
           03  QBR-ACH-BANK-NO                        PIC 9.
           03  QBR-WIMENU-CHECK-WRITING               PIC X.
           03  QBR-ADP-PAY-GROUP                      PIC X(3).
           03  QBR-ALLOW-SCANNED-CKS                  PIC X.
           03  QBR-CHECK-DEPOSIT-GL                   PIC 9(11).
           03  QBR-OPTION-PST-LASER                   PIC X.
           03  QBR-USE-DEBIT-CARDS                    PIC X.
           03  QBR-GL-DEBIT-CARD-PROCEEDS             PIC 9(11).
           03  QBR-WKDAY-BANKID                       PIC 9(4).
           03  QBR-PURGE-NOW-FLAG                     PIC X.
           03  QBR-EMAIL                              PIC X(50).
           03  QBR-MANAGER-EMAIL                      PIC X(50).
       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH     (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH     ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "BRNO LTOUCH   NAME            ".
           03  FILLER PIC X(30) VALUE "                         ADR2".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(06) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BR-NO                      PIC 9(4).
           03  FILLER                         PIC X(01).
           03  D01-BR-LTOUCH-DATE             PIC 9(8).
           03  FILLER                         PIC X(01).
           03  D01-BR-NAME                    PIC X(40).
           03  FILLER                         PIC X(01).
           03  D01-BR-ADR2                    PIC X(24).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOBRIN_DEF.CPY".
       COPY "LIBIO/IOBRIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOBRIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                        TO BR-PATH-OVERRIDE.

           MOVE BEG-BRANCH                 TO BR-NO.

           PERFORM OPEN-BR-FILE.
           PERFORM START-BR-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BR-NO < BEG-BRANCH )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BR-NO                      TO D01-BR-NO.
           MOVE BR-LTOUCH-DATE             TO D01-BR-LTOUCH-DATE.
           MOVE BR-NAME                    TO D01-BR-NAME.
           MOVE BR-ADR2                    TO D01-BR-ADR2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOBRIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOBRIN-QUEUE-POS  TO Q-POS.
           MOVE IOBRIN-COPIES-POS TO C-POS.
           MOVE IOBRIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
      *KC  IF (        F6-KEY ) PERFORM SCAN-XX1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )     
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           MOVE "SNNN040BEG-BRANCH."   TO VDU.
           MOVE BEG-BRANCH             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH."   TO VDU.
           MOVE END-BRANCH             TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE BR-NO       TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BRANCH              : "    TO R01-PROMPT.
           MOVE BEG-BRANCH                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRANCH              : "    TO R01-PROMPT.
           MOVE END-BRANCH                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - GRFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOBRIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOBRIN-SYSDATE.
           DISPLAY IOBRIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOBRIN-FORM-FIELDS TO WR-BUF.
           MOVE IOBRIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOBRIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOBRIN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOBRIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD BR-REC FROM QBR-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BR-FIELDS.
           MOVE QBR-NO TO BR-NO.

           IF ( QBR-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BR-LTOUCH-DATE
           ELSE
              MOVE QBR-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BR-LTOUCH-DATE.

           MOVE QBR-NAME TO BR-NAME.
           MOVE QBR-ADR1 TO BR-ADR1.
           MOVE QBR-ADR2 TO BR-ADR2.
           MOVE QBR-CITY TO BR-CITY.
           MOVE QBR-STATE TO BR-STATE.
           MOVE QBR-ZIP TO BR-ZIP.
           MOVE QBR-PHONE TO BR-PHONE.
           MOVE QBR-RETAIN TO BR-RETAIN.
           MOVE QBR-NEXT-CHECK TO BR-NEXT-CHECK.
           MOVE QBR-PRINTER-1 TO BR-PRINTER(1).
           MOVE QBR-PRINTER-2 TO BR-PRINTER(2).
           MOVE QBR-PRINTER-3 TO BR-PRINTER(3).
           MOVE QBR-PRINTER-4 TO BR-PRINTER(4).
           MOVE QBR-FEE-GL-BRNO TO BR-FEE-GL-BRNO.
           MOVE QBR-REMIT1 TO BR-REMIT1.
           MOVE QBR-REMIT2 TO BR-REMIT2.
           MOVE QBR-REMIT3 TO BR-REMIT3.
           MOVE QBR-STMNT1 TO BR-STMNT1.
           MOVE QBR-STMNT2 TO BR-STMNT2.
           MOVE QBR-INS-PRODUCER-NUM TO BR-INS-PRODUCER-NUM.
           MOVE QBR-DLRCHK TO BR-DLRCHK.
           MOVE QBR-FRONT-END-FG TO BR-FRONT-END-FG.
           MOVE QBR-EMLNRG-PHONE TO BR-EMLNRG-PHONE.
           MOVE QBR-POST-GL TO BR-POST-GL.
           MOVE QBR-ALLOTMNTS TO BR-ALLOTMNTS.
           MOVE QBR-EXTRACT-AUTO-UPD TO BR-EXTRACT-AUTO-UPD.
           MOVE QBR-PCNI-LEDGER TO BR-PCNI-LEDGER.
           MOVE QBR-PRINTERFG-1 TO BR-PRINTERFG(1).
           MOVE QBR-PRINTERFG-2 TO BR-PRINTERFG(2).
           MOVE QBR-PRINTERFG-3 TO BR-PRINTERFG(3).
           MOVE QBR-PRINTERFG-4 TO BR-PRINTERFG(4).
           MOVE QBR-NEGDLRSV TO BR-NEGDLRSV.
           MOVE QBR-CPCORP-ORDER TO BR-CPCORP-ORDER.
           MOVE QBR-PRIO-MEMO-FLAG TO BR-PRIO-MEMO-FLAG.
           MOVE QBR-PREBILL-PL-JD-OPT TO BR-PREBILL-PL-JD-OPT.
           MOVE QBR-LOAN-NUM-FLAG TO BR-LOAN-NUM-FLAG.
           MOVE QBR-DOWNPY-FLAG TO BR-DOWNPY-FLAG.
           MOVE QBR-AUTO-INS-EARN TO BR-AUTO-INS-EARN.
           MOVE QBR-OTH-INS-LEVELS TO BR-OTH-INS-LEVELS.
           MOVE QBR-ALLOW-INTERBR TO BR-ALLOW-INTERBR.
           MOVE QBR-UNER-DLANRP TO BR-UNER-DLANRP.
           MOVE QBR-SECOND-PROMISE TO BR-SECOND-PROMISE.
           MOVE QBR-AVAIL-CREDIT TO BR-AVAIL-CREDIT.
           MOVE QBR-RECP1 TO BR-RECP1.
           MOVE QBR-RECP2 TO BR-RECP2.
           MOVE QBR-INC-WAIVERS TO BR-INC-WAIVERS.
           MOVE QBR-INC-INS TO BR-INC-INS.
           MOVE QBR-BP-NEXT-BATCH TO BR-BP-NEXT-BATCH.
           MOVE QBR-BP-NOPAYOFF TO BR-BP-NOPAYOFF.
           MOVE QBR-BP-ODDPAY TO BR-BP-ODDPAY.
           MOVE QBR-BP-LC TO BR-BP-LC.
           MOVE QBR-BP-NOPRIN TO BR-BP-NOPRIN.
           MOVE QBR-BP-FLASH TO BR-BP-FLASH.
           MOVE QBR-BP-ACTIONCD TO BR-BP-ACTIONCD.
           MOVE QBR-BP-MAXCON TO BR-BP-MAXCON.
           MOVE QBR-BP-MAXREC TO BR-BP-MAXREC.
           MOVE QBR-BP-PL TO BR-BP-PL.
           MOVE QBR-BP-JD TO BR-BP-JD.
           MOVE QBR-BP-BRANCH TO BR-BP-BRANCH.
           MOVE QBR-AGE-ERN TO BR-AGE-ERN.
           MOVE QBR-OTHER-BRANCH-GL TO BR-OTHER-BRANCH-GL.
           MOVE QBR-SALES-SMALL TO BR-SALES-SMALL.
           MOVE QBR-NEW-COLL-INQUIRY TO BR-NEW-COLL-INQUIRY.
           MOVE QBR-CHANGE-MEMO TO BR-CHANGE-MEMO.
           MOVE QBR-INS-ADDON TO BR-INS-ADDON.
           MOVE QBR-TRAILER-REC TO BR-TRAILER-REC.
           MOVE QBR-ALDEL-FLAG TO BR-ALDEL-FLAG.
           MOVE QBR-IB-LCHG-MESS TO BR-IB-LCHG-MESS.
           MOVE QBR-COL-QUEUE-PAST-DUE TO BR-COL-QUEUE-PAST-DUE.
           MOVE QBR-LI-DLCOMM-CMSI-PP-MAX TO BR-LI-DLCOMM-CMSI-PP-MAX.
           MOVE QBR-ADDON-FEECD-O2-1 TO BR-ADDON-FEECD-O2(1).
           MOVE QBR-ADDON-FEECD-O2-2 TO BR-ADDON-FEECD-O2(2).
           MOVE QBR-AUTO-CHECK TO BR-AUTO-CHECK.
           MOVE QBR-RECEIPT-PROMDATE TO BR-RECEIPT-PROMDATE.
           MOVE QBR-RECEIPT-INTPD TO BR-RECEIPT-INTPD.
           MOVE QBR-COLL-ID-SCAN TO BR-COLL-ID-SCAN.
           MOVE QBR-STOP-ON-COLL TO BR-STOP-ON-COLL.
           MOVE QBR-DUEDATE-CHANGE-CD TO BR-DUEDATE-CHANGE-CD.
           MOVE QBR-CHECK-EOM-FG TO BR-CHECK-EOM-FG.
           MOVE QBR-VOLUME-DLANRP TO BR-VOLUME-DLANRP.
           MOVE QBR-PDTH-NXDUE TO BR-PDTH-NXDUE.
           MOVE QBR-LC-CODE TO BR-LC-CODE.
           MOVE QBR-VALIDATE-PRUS TO BR-VALIDATE-PRUS.
           MOVE QBR-AUTO-OPEN-BAL TO BR-AUTO-OPEN-BAL.
           MOVE QBR-OPEN-BAL-1 TO BR-OPEN-BAL(1).
           MOVE QBR-OPEN-BAL-2 TO BR-OPEN-BAL(2).
           MOVE QBR-OPEN-BAL-3 TO BR-OPEN-BAL(3).
           MOVE QBR-OPEN-BAL-4 TO BR-OPEN-BAL(4).
           MOVE QBR-OPEN-BAL-5 TO BR-OPEN-BAL(5).
           MOVE QBR-OPEN-BAL-6 TO BR-OPEN-BAL(6).
           MOVE QBR-OPEN-BAL-7 TO BR-OPEN-BAL(7).
           MOVE QBR-OPEN-BAL-8 TO BR-OPEN-BAL(8).
           MOVE QBR-OPEN-BAL-9 TO BR-OPEN-BAL(9).
           MOVE QBR-OPEN-BAL-10 TO BR-OPEN-BAL(10).
           MOVE QBR-OPEN-BAL-11 TO BR-OPEN-BAL(11).
           MOVE QBR-OPEN-BAL-12 TO BR-OPEN-BAL(12).
           MOVE QBR-OPEN-BAL-13 TO BR-OPEN-BAL(13).
           MOVE QBR-OPEN-BAL-14 TO BR-OPEN-BAL(14).
           MOVE QBR-OPEN-BAL-15 TO BR-OPEN-BAL(15).
           MOVE QBR-OPEN-BAL-16 TO BR-OPEN-BAL(16).
           MOVE QBR-OPEN-BAL-17 TO BR-OPEN-BAL(17).
           MOVE QBR-OPEN-BAL-18 TO BR-OPEN-BAL(18).
           MOVE QBR-OPEN-BAL-19 TO BR-OPEN-BAL(19).
           MOVE QBR-OPEN-BAL-20 TO BR-OPEN-BAL(20).
           MOVE QBR-TENDER-CD-1 TO BR-TENDER-CD(1).
           MOVE QBR-TENDER-CD-2 TO BR-TENDER-CD(2).
           MOVE QBR-TENDER-CD-3 TO BR-TENDER-CD(3).
           MOVE QBR-DLPARV-ADJ-FG TO BR-DLPARV-ADJ-FG.
           MOVE QBR-OVERRIDE-BR-STATE TO BR-OVERRIDE-BR-STATE.
           MOVE QBR-DEFAULT-SC-CLASS TO BR-DEFAULT-SC-CLASS.
           MOVE QBR-RECDEL-NOT-REC TO BR-RECDEL-NOT-REC.
           MOVE QBR-POST-AHEAD-FG TO BR-POST-AHEAD-FG.
           MOVE QBR-CAN-LOANS-FG TO BR-CAN-LOANS-FG.
           MOVE QBR-NON-MONTHLY-CAN-LOANS TO BR-NON-MONTHLY-CAN-LOANS.
           MOVE QBR-BP-ALLOW-PMT-ACCT-OTHBAL
              TO BR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL
              TO BR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           MOVE QBR-REGION-NUMBER TO BR-REGION-NUMBER.
           MOVE QBR-OUR-FEE TO BR-OUR-FEE.
           MOVE QBR-REB-METHOD-1 TO BR-REB-METHOD(1).
           MOVE QBR-REB-METHOD-2 TO BR-REB-METHOD(2).
           MOVE QBR-REB-METHOD-3 TO BR-REB-METHOD(3).
           MOVE QBR-REB-METHOD-4 TO BR-REB-METHOD(4).
           MOVE QBR-REB-METHOD-5 TO BR-REB-METHOD(5).
           MOVE QBR-REB-METHOD-6 TO BR-REB-METHOD(6).
           MOVE QBR-REB-METHOD-7 TO BR-REB-METHOD(7).
           MOVE QBR-REB-METHOD-8 TO BR-REB-METHOD(8).
           MOVE QBR-REB-METHOD-9 TO BR-REB-METHOD(9).
           MOVE QBR-REB-METHOD-10 TO BR-REB-METHOD(10).
           MOVE QBR-REB-METHOD-11 TO BR-REB-METHOD(11).
           MOVE QBR-REBDAYS-MONTH-1 TO BR-REBDAYS-MONTH(1).
           MOVE QBR-REBDAYS-MONTH-2 TO BR-REBDAYS-MONTH(2).
           MOVE QBR-REBDAYS-MONTH-3 TO BR-REBDAYS-MONTH(3).
           MOVE QBR-REBDAYS-MONTH-4 TO BR-REBDAYS-MONTH(4).
           MOVE QBR-REBDAYS-MONTH-5 TO BR-REBDAYS-MONTH(5).
           MOVE QBR-REBDAYS-MONTH-6 TO BR-REBDAYS-MONTH(6).
           MOVE QBR-REBDAYS-MONTH-7 TO BR-REBDAYS-MONTH(7).
           MOVE QBR-REBDAYS-MONTH-8 TO BR-REBDAYS-MONTH(8).
           MOVE QBR-REBDAYS-MONTH-9 TO BR-REBDAYS-MONTH(9).
           MOVE QBR-REBDAYS-MONTH-10 TO BR-REBDAYS-MONTH(10).
           MOVE QBR-REBDAYS-MONTH-11 TO BR-REBDAYS-MONTH(11).
           MOVE QBR-ACCRUAL-MONTHS-1 TO BR-ACCRUAL-MONTHS(1).
           MOVE QBR-ACCRUAL-MONTHS-2 TO BR-ACCRUAL-MONTHS(2).
           MOVE QBR-ACCRUAL-MONTHS-3 TO BR-ACCRUAL-MONTHS(3).
           MOVE QBR-ACCRUAL-MONTHS-4 TO BR-ACCRUAL-MONTHS(4).
           MOVE QBR-ACCRUAL-MONTHS-5 TO BR-ACCRUAL-MONTHS(5).
           MOVE QBR-ACCRUAL-MONTHS-6 TO BR-ACCRUAL-MONTHS(6).
           MOVE QBR-ACCRUAL-MONTHS-7 TO BR-ACCRUAL-MONTHS(7).
           MOVE QBR-ACCRUAL-MONTHS-8 TO BR-ACCRUAL-MONTHS(8).
           MOVE QBR-ACCRUAL-MONTHS-9 TO BR-ACCRUAL-MONTHS(9).
           MOVE QBR-ACCRUAL-MONTHS-10 TO BR-ACCRUAL-MONTHS(10).
           MOVE QBR-ACCRUAL-MONTHS-11 TO BR-ACCRUAL-MONTHS(11).
           MOVE QBR-GLOVERSHORT TO BR-GLOVERSHORT.
           MOVE QBR-GLBANK TO BR-GLBANK.
           MOVE QBR-GLONHAND TO BR-GLONHAND.
           MOVE QBR-BANKBAL TO BR-BANKBAL.
           MOVE QBR-RETAIN-AMT TO BR-RETAIN-AMT.
           MOVE QBR-LCARDFG TO BR-LCARDFG.
           MOVE QBR-DATA-PATH TO BR-DATA-PATH.
           MOVE QBR-MACHINE TO BR-MACHINE.
           MOVE QBR-MANAGER TO BR-MANAGER.
           MOVE QBR-EOD-PROCEDURE TO BR-EOD-PROCEDURE.
           MOVE QBR-BP-PAYOFF-NONCASH TO BR-BP-PAYOFF-NONCASH.
           MOVE QBR-BP-PAYOFF-OV TO BR-BP-PAYOFF-OV.
           MOVE QBR-LI-DLCOMM-CMSI-O3-MAX TO BR-LI-DLCOMM-CMSI-O3-MAX.
           MOVE QBR-LR-GROUP-COLLID TO BR-LR-GROUP-COLLID.
           MOVE QBR-UNITPER-FG TO BR-UNITPER-FG.
           MOVE QBR-UNITPER-Y-D-FG TO BR-UNITPER-Y-D-FG.
           MOVE QBR-NO-PROMPT-FOR-OTHBR TO BR-NO-PROMPT-FOR-OTHBR.
           MOVE QBR-WORLD-BRANCH-TYPE TO BR-WORLD-BRANCH-TYPE.
           MOVE QBR-COL-QUEUE-BEHAVIOR-FG TO BR-COL-QUEUE-BEHAVIOR-FG.
           MOVE QBR-CONVERT-AUTO-ACCTNO TO BR-CONVERT-AUTO-ACCTNO.
           MOVE QBR-TS-REPO-INS-FG TO BR-TS-REPO-INS-FG.
           MOVE QBR-BOOK-SF-BR-ONLY TO BR-BOOK-SF-BR-ONLY.
           MOVE QBR-FORCE-VALID-CITY-COUNTY
              TO BR-FORCE-VALID-CITY-COUNTY.
           MOVE QBR-SET-COLLCD-DL-MFGCD TO BR-SET-COLLCD-DL-MFGCD.
           MOVE QBR-DEFAULT-SF-UNITPER-CD TO BR-DEFAULT-SF-UNITPER-CD.
           MOVE QBR-DEFAULT-SF-PURCD TO BR-DEFAULT-SF-PURCD.
           MOVE QBR-DEFAULT-LN-UNITPER-CD TO BR-DEFAULT-LN-UNITPER-CD.
           MOVE QBR-DEFAULT-LN-PURCD TO BR-DEFAULT-LN-PURCD.
           MOVE QBR-TAP-OTHER-DL TO BR-TAP-OTHER-DL.
           MOVE QBR-AH-AND-CL TO BR-AH-AND-CL.
           MOVE QBR-NO-CASH-PROCEEDS TO BR-NO-CASH-PROCEEDS.
           MOVE QBR-RI-BEFORE-REPO TO BR-RI-BEFORE-REPO.
           MOVE QBR-RI-BEFORE-P4P5 TO BR-RI-BEFORE-P4P5.
           MOVE QBR-NOT-OURS-REFUND TO BR-NOT-OURS-REFUND.
           MOVE QBR-SFDLO3-WINDOW-FG TO BR-SFDLO3-WINDOW-FG.
           MOVE QBR-CALC-INS-ADJ TO BR-CALC-INS-ADJ.
           MOVE QBR-AC-BEFORE-REPO TO BR-AC-BEFORE-REPO.
           MOVE QBR-AC-BEFORE-P4P5 TO BR-AC-BEFORE-P4P5.
           MOVE QBR-BANK-ROUTING-NO TO BR-BANK-ROUTING-NO.
           MOVE QBR-BANK-CLEARING-GL TO BR-BANK-CLEARING-GL.
           MOVE QBR-GLBANK-DEPOSIT TO BR-GLBANK-DEPOSIT.
           MOVE QBR-BANK-ACCTNO TO BR-BANK-ACCTNO.
           MOVE QBR-DEFAULT-SC-SUBCLASS TO BR-DEFAULT-SC-SUBCLASS.
           MOVE QBR-NO-OUT-DLR-RSV TO BR-NO-OUT-DLR-RSV.
           MOVE QBR-COLLECTOR-QUEUE TO BR-COLLECTOR-QUEUE.
           MOVE QBR-COLL-QUEUE-CON-REC TO BR-COLL-QUEUE-CON-REC.
           MOVE QBR-JE-ALLOCATION-SKIP TO BR-JE-ALLOCATION-SKIP.
           MOVE QBR-EXPAND-APP-INPUT TO BR-EXPAND-APP-INPUT.
           MOVE QBR-A50-CPCORP TO BR-A50-CPCORP.
           MOVE QBR-INBR-BILNSC-FG TO BR-INBR-BILNSC-FG.
           MOVE QBR-INBR-BRIEF-FG TO BR-INBR-BRIEF-FG.
           MOVE QBR-LEDGER-JUST-INTCHG TO BR-LEDGER-JUST-INTCHG.
           MOVE QBR-BYINQ-START-PAGE TO BR-BYINQ-START-PAGE.
           MOVE QBR-DSINQ-START-PAGE TO BR-DSINQ-START-PAGE.
           MOVE QBR-APP-REQUIRED-FOR-INPUT
              TO BR-APP-REQUIRED-FOR-INPUT.
           MOVE QBR-CREDIT-SCORING-FG TO BR-CREDIT-SCORING-FG.
           MOVE QBR-COL-QUEUE-LOW-TO-HI TO BR-COL-QUEUE-LOW-TO-HI.
           MOVE QBR-CAN-GROUP-ID TO BR-CAN-GROUP-ID.
           MOVE QBR-AUTO-LNSTAT-WINDOW TO BR-AUTO-LNSTAT-WINDOW.
           MOVE QBR-TIME-ZONE-FACTOR TO BR-TIME-ZONE-FACTOR.
           MOVE QBR-LONPF-OPT-DISPLAY TO BR-LONPF-OPT-DISPLAY.
           MOVE QBR-GIFTNET TO BR-GIFTNET.
           MOVE QBR-GIFTNET-LEDGER-LINE TO BR-GIFTNET-LEDGER-LINE.
           MOVE QBR-NO-FT-EMP TO BR-NO-FT-EMP.
           MOVE QBR-NO-PT-EMP TO BR-NO-PT-EMP.
           MOVE QBR-BALLOON-LOANS-FG TO BR-BALLOON-LOANS-FG.
           MOVE QBR-GL-TYPE-ALLOWED TO BR-GL-TYPE-ALLOWED.
           MOVE QBR-GL-ENTRY-DEFAULT TO BR-GL-ENTRY-DEFAULT.
           MOVE QBR-MIDSTREAM-1STPAY-FG TO BR-MIDSTREAM-1STPAY-FG.
           MOVE QBR-NODEF-1ST-X-MONTHS TO BR-NODEF-1ST-X-MONTHS.
           MOVE QBR-ADD-INS-FEECODE TO BR-ADD-INS-FEECODE.
           MOVE QBR-ROAD-G-SGL-FEECODE TO BR-ROAD-G-SGL-FEECODE.
           MOVE QBR-ROAD-G-FAM-FEECODE TO BR-ROAD-G-FAM-FEECODE.
           MOVE QBR-NODEF-IF-ZERO-AMT TO BR-NODEF-IF-ZERO-AMT.
           MOVE QBR-ASSESS-LATE-CHGS TO BR-ASSESS-LATE-CHGS.
           MOVE QBR-NEXT-SOLICIT-NO TO BR-NEXT-SOLICIT-NO.
           MOVE QBR-ADDON-FEECD-O3-1 TO BR-ADDON-FEECD-O3(1).
           MOVE QBR-ADDON-FEECD-O3-2 TO BR-ADDON-FEECD-O3(2).
           MOVE QBR-IMMEDIATE-RECEIPT TO BR-IMMEDIATE-RECEIPT.
           MOVE QBR-IMMEDIATE-DOC-PRINT TO BR-IMMEDIATE-DOC-PRINT.
           MOVE QBR-AUTO-PL-REF-AC TO BR-AUTO-PL-REF-AC.
           MOVE QBR-FAX-NUMBER TO BR-FAX-NUMBER.
           MOVE QBR-ALPHA-DRAWERS TO BR-ALPHA-DRAWERS.
           MOVE QBR-LATECHG-ON-ALLOTMENTS TO BR-LATECHG-ON-ALLOTMENTS.
           MOVE QBR-ALLOW-SYS-AS-EFFDATE TO BR-ALLOW-SYS-AS-EFFDATE.
           MOVE QBR-TAX-PREBOOK-CUST TO BR-TAX-PREBOOK-CUST.
           MOVE QBR-TAX-PREBOOK-NONCUST TO BR-TAX-PREBOOK-NONCUST.
           MOVE QBR-PASSWORD-DRAWER TO BR-PASSWORD-DRAWER.
           MOVE QBR-ONESTEP-RN-FG TO BR-ONESTEP-RN-FG.
           MOVE QBR-LI-LISCAN-FG TO BR-LI-LISCAN-FG.
           MOVE QBR-HIDE-END-BAL TO BR-HIDE-END-BAL.
           MOVE QBR-LBOX-NOPAYOFF TO BR-LBOX-NOPAYOFF.
           MOVE QBR-LBOX-ODDPAY TO BR-LBOX-ODDPAY.
           MOVE QBR-LBOX-LC TO BR-LBOX-LC.
           MOVE QBR-LBOX-NOPRIN TO BR-LBOX-NOPRIN.
           MOVE QBR-LBOX-FLASH TO BR-LBOX-FLASH.
           MOVE QBR-LBOX-ACTIONCD TO BR-LBOX-ACTIONCD.
           MOVE QBR-LBOX-MAXCON TO BR-LBOX-MAXCON.
           MOVE QBR-LBOX-MAXREC TO BR-LBOX-MAXREC.
           MOVE QBR-LBOX-PL TO BR-LBOX-PL.
           MOVE QBR-LBOX-JD TO BR-LBOX-JD.
           MOVE QBR-LBOX TO BR-LBOX.
           MOVE QBR-LATECHG-ON-LBOX TO BR-LATECHG-ON-LBOX.
           MOVE QBR-ALLOW-OV-TRCD TO BR-ALLOW-OV-TRCD.
           MOVE QBR-GLOBAL-QUEUE TO BR-GLOBAL-QUEUE.
           MOVE QBR-LBOX-PAYOFF-NONCASH TO BR-LBOX-PAYOFF-NONCASH.
           MOVE QBR-LBOX-PAYOFF-OV TO BR-LBOX-PAYOFF-OV.
           MOVE QBR-COLLECT-MANAGER TO BR-COLLECT-MANAGER.
           MOVE QBR-COLLECT-PHONE TO BR-COLLECT-PHONE.
           MOVE QBR-COLLECT-EXT TO BR-COLLECT-EXT.
           MOVE QBR-ALT-INTO-THIS-BRANCH TO BR-ALT-INTO-THIS-BRANCH.
           MOVE QBR-POFF-TITLE-WINDOW TO BR-POFF-TITLE-WINDOW.
           MOVE QBR-COUPON-STATEMENT TO BR-COUPON-STATEMENT.
           MOVE QBR-SUPERVISOR TO BR-SUPERVISOR.
           MOVE QBR-AREA-MANAGER TO BR-AREA-MANAGER.
           MOVE QBR-MIN-MONTHS-DEF TO BR-MIN-MONTHS-DEF.
           MOVE QBR-CC-FLAG TO BR-CC-FLAG.
           MOVE QBR-CC-ADR1 TO BR-CC-ADR1.
           MOVE QBR-CC-CITY TO BR-CC-CITY.
           MOVE QBR-CC-STATE TO BR-CC-STATE.
           MOVE QBR-CC-ZIP TO BR-CC-ZIP.
           MOVE QBR-MIN-MONTHS-D2 TO BR-MIN-MONTHS-D2.
           MOVE QBR-SKIP-MIDSTREAM-TRW TO BR-SKIP-MIDSTREAM-TRW.
           MOVE QBR-GL-ESCROW-BALANCE TO BR-GL-ESCROW-BALANCE.
           MOVE QBR-GL-ESCROW-PAYABLE TO BR-GL-ESCROW-PAYABLE.
           MOVE QBR-ALLOW-FORCE-OPEN-FG TO BR-ALLOW-FORCE-OPEN-FG.
           MOVE QBR-ACH-BANK-NO TO BR-ACH-BANK-NO.
           MOVE QBR-WIMENU-CHECK-WRITING TO BR-WIMENU-CHECK-WRITING.
           MOVE QBR-ADP-PAY-GROUP TO BR-ADP-PAY-GROUP.
           MOVE QBR-ALLOW-SCANNED-CKS TO BR-ALLOW-SCANNED-CKS.
           MOVE QBR-CHECK-DEPOSIT-GL TO BR-CHECK-DEPOSIT-GL.
           MOVE QBR-OPTION-PST-LASER TO BR-OPTION-PST-LASER.
           MOVE QBR-USE-DEBIT-CARDS TO BR-USE-DEBIT-CARDS.
           MOVE QBR-GL-DEBIT-CARD-PROCEEDS
              TO BR-GL-DEBIT-CARD-PROCEEDS.
           MOVE QBR-WKDAY-BANKID TO BR-WKDAY-BANKID.
           MOVE QBR-PURGE-NOW-FLAG TO BR-PURGE-NOW-FLAG.
           MOVE QBR-EMAIL TO BR-EMAIL.
           MOVE QBR-MANAGER-EMAIL TO BR-MANAGER-EMAIL.
 
      *-----------------------------------------------------------------
      *    LOAD QBR-REC FROM BR-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BR-FIELDS.
           MOVE BR-NO TO QBR-NO.

           IF ( BR-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBR-LTOUCH-DATE
           ELSE
              MOVE BR-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBR-LTOUCH-DATE.

           MOVE BR-NAME TO QBR-NAME.
           MOVE BR-ADR1 TO QBR-ADR1.
           MOVE BR-ADR2 TO QBR-ADR2.
           MOVE BR-CITY TO QBR-CITY.
           MOVE BR-STATE TO QBR-STATE.
           MOVE BR-ZIP TO QBR-ZIP.
           MOVE BR-PHONE TO QBR-PHONE.
           MOVE BR-RETAIN TO QBR-RETAIN.
           MOVE BR-NEXT-CHECK TO QBR-NEXT-CHECK.
           MOVE BR-PRINTER(1) TO QBR-PRINTER-1.
           MOVE BR-PRINTER(2) TO QBR-PRINTER-2.
           MOVE BR-PRINTER(3) TO QBR-PRINTER-3.
           MOVE BR-PRINTER(4) TO QBR-PRINTER-4.
           MOVE BR-FEE-GL-BRNO TO QBR-FEE-GL-BRNO.
           MOVE BR-REMIT1 TO QBR-REMIT1.
           MOVE BR-REMIT2 TO QBR-REMIT2.
           MOVE BR-REMIT3 TO QBR-REMIT3.
           MOVE BR-STMNT1 TO QBR-STMNT1.
           MOVE BR-STMNT2 TO QBR-STMNT2.
           MOVE BR-INS-PRODUCER-NUM TO QBR-INS-PRODUCER-NUM.
           MOVE BR-DLRCHK TO QBR-DLRCHK.
           MOVE BR-FRONT-END-FG TO QBR-FRONT-END-FG.
           MOVE BR-EMLNRG-PHONE TO QBR-EMLNRG-PHONE.
           MOVE BR-POST-GL TO QBR-POST-GL.
           MOVE BR-ALLOTMNTS TO QBR-ALLOTMNTS.
           MOVE BR-EXTRACT-AUTO-UPD TO QBR-EXTRACT-AUTO-UPD.
           MOVE BR-PCNI-LEDGER TO QBR-PCNI-LEDGER.
           MOVE BR-PRINTERFG(1) TO QBR-PRINTERFG-1.
           MOVE BR-PRINTERFG(2) TO QBR-PRINTERFG-2.
           MOVE BR-PRINTERFG(3) TO QBR-PRINTERFG-3.
           MOVE BR-PRINTERFG(4) TO QBR-PRINTERFG-4.
           MOVE BR-NEGDLRSV TO QBR-NEGDLRSV.
           MOVE BR-CPCORP-ORDER TO QBR-CPCORP-ORDER.
           MOVE BR-PRIO-MEMO-FLAG TO QBR-PRIO-MEMO-FLAG.
           MOVE BR-PREBILL-PL-JD-OPT TO QBR-PREBILL-PL-JD-OPT.
           MOVE BR-LOAN-NUM-FLAG TO QBR-LOAN-NUM-FLAG.
           MOVE BR-DOWNPY-FLAG TO QBR-DOWNPY-FLAG.
           MOVE BR-AUTO-INS-EARN TO QBR-AUTO-INS-EARN.
           MOVE BR-OTH-INS-LEVELS TO QBR-OTH-INS-LEVELS.
           MOVE BR-ALLOW-INTERBR TO QBR-ALLOW-INTERBR.
           MOVE BR-UNER-DLANRP TO QBR-UNER-DLANRP.
           MOVE BR-SECOND-PROMISE TO QBR-SECOND-PROMISE.
           MOVE BR-AVAIL-CREDIT TO QBR-AVAIL-CREDIT.
           MOVE BR-RECP1 TO QBR-RECP1.
           MOVE BR-RECP2 TO QBR-RECP2.
           MOVE BR-INC-WAIVERS TO QBR-INC-WAIVERS.
           MOVE BR-INC-INS TO QBR-INC-INS.
           MOVE BR-BP-NEXT-BATCH TO QBR-BP-NEXT-BATCH.
           MOVE BR-BP-NOPAYOFF TO QBR-BP-NOPAYOFF.
           MOVE BR-BP-ODDPAY TO QBR-BP-ODDPAY.
           MOVE BR-BP-LC TO QBR-BP-LC.
           MOVE BR-BP-NOPRIN TO QBR-BP-NOPRIN.
           MOVE BR-BP-FLASH TO QBR-BP-FLASH.
           MOVE BR-BP-ACTIONCD TO QBR-BP-ACTIONCD.
           MOVE BR-BP-MAXCON TO QBR-BP-MAXCON.
           MOVE BR-BP-MAXREC TO QBR-BP-MAXREC.
           MOVE BR-BP-PL TO QBR-BP-PL.
           MOVE BR-BP-JD TO QBR-BP-JD.
           MOVE BR-BP-BRANCH TO QBR-BP-BRANCH.
           MOVE BR-AGE-ERN TO QBR-AGE-ERN.
           MOVE BR-OTHER-BRANCH-GL TO QBR-OTHER-BRANCH-GL.
           MOVE BR-SALES-SMALL TO QBR-SALES-SMALL.
           MOVE BR-NEW-COLL-INQUIRY TO QBR-NEW-COLL-INQUIRY.
           MOVE BR-CHANGE-MEMO TO QBR-CHANGE-MEMO.
           MOVE BR-INS-ADDON TO QBR-INS-ADDON.
           MOVE BR-TRAILER-REC TO QBR-TRAILER-REC.
           MOVE BR-ALDEL-FLAG TO QBR-ALDEL-FLAG.
           MOVE BR-IB-LCHG-MESS TO QBR-IB-LCHG-MESS.
           MOVE BR-COL-QUEUE-PAST-DUE TO QBR-COL-QUEUE-PAST-DUE.
           MOVE BR-LI-DLCOMM-CMSI-PP-MAX TO QBR-LI-DLCOMM-CMSI-PP-MAX.
           MOVE BR-ADDON-FEECD-O2(1) TO QBR-ADDON-FEECD-O2-1.
           MOVE BR-ADDON-FEECD-O2(2) TO QBR-ADDON-FEECD-O2-2.
           MOVE BR-AUTO-CHECK TO QBR-AUTO-CHECK.
           MOVE BR-RECEIPT-PROMDATE TO QBR-RECEIPT-PROMDATE.
           MOVE BR-RECEIPT-INTPD TO QBR-RECEIPT-INTPD.
           MOVE BR-COLL-ID-SCAN TO QBR-COLL-ID-SCAN.
           MOVE BR-STOP-ON-COLL TO QBR-STOP-ON-COLL.
           MOVE BR-DUEDATE-CHANGE-CD TO QBR-DUEDATE-CHANGE-CD.
           MOVE BR-CHECK-EOM-FG TO QBR-CHECK-EOM-FG.
           MOVE BR-VOLUME-DLANRP TO QBR-VOLUME-DLANRP.
           MOVE BR-PDTH-NXDUE TO QBR-PDTH-NXDUE.
           MOVE BR-LC-CODE TO QBR-LC-CODE.
           MOVE BR-VALIDATE-PRUS TO QBR-VALIDATE-PRUS.
           MOVE BR-AUTO-OPEN-BAL TO QBR-AUTO-OPEN-BAL.
           MOVE BR-OPEN-BAL(1) TO QBR-OPEN-BAL-1.
           MOVE BR-OPEN-BAL(2) TO QBR-OPEN-BAL-2.
           MOVE BR-OPEN-BAL(3) TO QBR-OPEN-BAL-3.
           MOVE BR-OPEN-BAL(4) TO QBR-OPEN-BAL-4.
           MOVE BR-OPEN-BAL(5) TO QBR-OPEN-BAL-5.
           MOVE BR-OPEN-BAL(6) TO QBR-OPEN-BAL-6.
           MOVE BR-OPEN-BAL(7) TO QBR-OPEN-BAL-7.
           MOVE BR-OPEN-BAL(8) TO QBR-OPEN-BAL-8.
           MOVE BR-OPEN-BAL(9) TO QBR-OPEN-BAL-9.
           MOVE BR-OPEN-BAL(10) TO QBR-OPEN-BAL-10.
           MOVE BR-OPEN-BAL(11) TO QBR-OPEN-BAL-11.
           MOVE BR-OPEN-BAL(12) TO QBR-OPEN-BAL-12.
           MOVE BR-OPEN-BAL(13) TO QBR-OPEN-BAL-13.
           MOVE BR-OPEN-BAL(14) TO QBR-OPEN-BAL-14.
           MOVE BR-OPEN-BAL(15) TO QBR-OPEN-BAL-15.
           MOVE BR-OPEN-BAL(16) TO QBR-OPEN-BAL-16.
           MOVE BR-OPEN-BAL(17) TO QBR-OPEN-BAL-17.
           MOVE BR-OPEN-BAL(18) TO QBR-OPEN-BAL-18.
           MOVE BR-OPEN-BAL(19) TO QBR-OPEN-BAL-19.
           MOVE BR-OPEN-BAL(20) TO QBR-OPEN-BAL-20.
           MOVE BR-TENDER-CD(1) TO QBR-TENDER-CD-1.
           MOVE BR-TENDER-CD(2) TO QBR-TENDER-CD-2.
           MOVE BR-TENDER-CD(3) TO QBR-TENDER-CD-3.
           MOVE BR-DLPARV-ADJ-FG TO QBR-DLPARV-ADJ-FG.
           MOVE BR-OVERRIDE-BR-STATE TO QBR-OVERRIDE-BR-STATE.
           MOVE BR-DEFAULT-SC-CLASS TO QBR-DEFAULT-SC-CLASS.
           MOVE BR-RECDEL-NOT-REC TO QBR-RECDEL-NOT-REC.
           MOVE BR-POST-AHEAD-FG TO QBR-POST-AHEAD-FG.
           MOVE BR-CAN-LOANS-FG TO QBR-CAN-LOANS-FG.
           MOVE BR-NON-MONTHLY-CAN-LOANS TO QBR-NON-MONTHLY-CAN-LOANS.
           MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL
            TO QBR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL
            TO QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BR-REGION-NUMBER TO QBR-REGION-NUMBER.
           MOVE BR-OUR-FEE TO QBR-OUR-FEE.
           MOVE BR-REB-METHOD(1) TO QBR-REB-METHOD-1.
           MOVE BR-REB-METHOD(2) TO QBR-REB-METHOD-2.
           MOVE BR-REB-METHOD(3) TO QBR-REB-METHOD-3.
           MOVE BR-REB-METHOD(4) TO QBR-REB-METHOD-4.
           MOVE BR-REB-METHOD(5) TO QBR-REB-METHOD-5.
           MOVE BR-REB-METHOD(6) TO QBR-REB-METHOD-6.
           MOVE BR-REB-METHOD(7) TO QBR-REB-METHOD-7.
           MOVE BR-REB-METHOD(8) TO QBR-REB-METHOD-8.
           MOVE BR-REB-METHOD(9) TO QBR-REB-METHOD-9.
           MOVE BR-REB-METHOD(10) TO QBR-REB-METHOD-10.
           MOVE BR-REB-METHOD(11) TO QBR-REB-METHOD-11.
           MOVE BR-REBDAYS-MONTH(1) TO QBR-REBDAYS-MONTH-1.
           MOVE BR-REBDAYS-MONTH(2) TO QBR-REBDAYS-MONTH-2.
           MOVE BR-REBDAYS-MONTH(3) TO QBR-REBDAYS-MONTH-3.
           MOVE BR-REBDAYS-MONTH(4) TO QBR-REBDAYS-MONTH-4.
           MOVE BR-REBDAYS-MONTH(5) TO QBR-REBDAYS-MONTH-5.
           MOVE BR-REBDAYS-MONTH(6) TO QBR-REBDAYS-MONTH-6.
           MOVE BR-REBDAYS-MONTH(7) TO QBR-REBDAYS-MONTH-7.
           MOVE BR-REBDAYS-MONTH(8) TO QBR-REBDAYS-MONTH-8.
           MOVE BR-REBDAYS-MONTH(9) TO QBR-REBDAYS-MONTH-9.
           MOVE BR-REBDAYS-MONTH(10) TO QBR-REBDAYS-MONTH-10.
           MOVE BR-REBDAYS-MONTH(11) TO QBR-REBDAYS-MONTH-11.
           MOVE BR-ACCRUAL-MONTHS(1) TO QBR-ACCRUAL-MONTHS-1.
           MOVE BR-ACCRUAL-MONTHS(2) TO QBR-ACCRUAL-MONTHS-2.
           MOVE BR-ACCRUAL-MONTHS(3) TO QBR-ACCRUAL-MONTHS-3.
           MOVE BR-ACCRUAL-MONTHS(4) TO QBR-ACCRUAL-MONTHS-4.
           MOVE BR-ACCRUAL-MONTHS(5) TO QBR-ACCRUAL-MONTHS-5.
           MOVE BR-ACCRUAL-MONTHS(6) TO QBR-ACCRUAL-MONTHS-6.
           MOVE BR-ACCRUAL-MONTHS(7) TO QBR-ACCRUAL-MONTHS-7.
           MOVE BR-ACCRUAL-MONTHS(8) TO QBR-ACCRUAL-MONTHS-8.
           MOVE BR-ACCRUAL-MONTHS(9) TO QBR-ACCRUAL-MONTHS-9.
           MOVE BR-ACCRUAL-MONTHS(10) TO QBR-ACCRUAL-MONTHS-10.
           MOVE BR-ACCRUAL-MONTHS(11) TO QBR-ACCRUAL-MONTHS-11.
           MOVE BR-GLOVERSHORT TO QBR-GLOVERSHORT.
           MOVE BR-GLBANK TO QBR-GLBANK.
           MOVE BR-GLONHAND TO QBR-GLONHAND.
           MOVE BR-BANKBAL TO QBR-BANKBAL.
           MOVE BR-RETAIN-AMT TO QBR-RETAIN-AMT.
           MOVE BR-LCARDFG TO QBR-LCARDFG.
           MOVE BR-DATA-PATH TO QBR-DATA-PATH.
           MOVE BR-MACHINE TO QBR-MACHINE.
           MOVE BR-MANAGER TO QBR-MANAGER.
           MOVE BR-EOD-PROCEDURE TO QBR-EOD-PROCEDURE.
           MOVE BR-BP-PAYOFF-NONCASH TO QBR-BP-PAYOFF-NONCASH.
           MOVE BR-BP-PAYOFF-OV TO QBR-BP-PAYOFF-OV.
           MOVE BR-LI-DLCOMM-CMSI-O3-MAX TO QBR-LI-DLCOMM-CMSI-O3-MAX.
           MOVE BR-LR-GROUP-COLLID TO QBR-LR-GROUP-COLLID.
           MOVE BR-UNITPER-FG TO QBR-UNITPER-FG.
           MOVE BR-UNITPER-Y-D-FG TO QBR-UNITPER-Y-D-FG.
           MOVE BR-NO-PROMPT-FOR-OTHBR TO QBR-NO-PROMPT-FOR-OTHBR.
           MOVE BR-WORLD-BRANCH-TYPE TO QBR-WORLD-BRANCH-TYPE.
           MOVE BR-COL-QUEUE-BEHAVIOR-FG TO QBR-COL-QUEUE-BEHAVIOR-FG.
           MOVE BR-CONVERT-AUTO-ACCTNO TO QBR-CONVERT-AUTO-ACCTNO.
           MOVE BR-TS-REPO-INS-FG TO QBR-TS-REPO-INS-FG.
           MOVE BR-BOOK-SF-BR-ONLY TO QBR-BOOK-SF-BR-ONLY.
           MOVE BR-FORCE-VALID-CITY-COUNTY
            TO QBR-FORCE-VALID-CITY-COUNTY.
           MOVE BR-SET-COLLCD-DL-MFGCD TO QBR-SET-COLLCD-DL-MFGCD.
           MOVE BR-DEFAULT-SF-UNITPER-CD TO QBR-DEFAULT-SF-UNITPER-CD.
           MOVE BR-DEFAULT-SF-PURCD TO QBR-DEFAULT-SF-PURCD.
           MOVE BR-DEFAULT-LN-UNITPER-CD TO QBR-DEFAULT-LN-UNITPER-CD.
           MOVE BR-DEFAULT-LN-PURCD TO QBR-DEFAULT-LN-PURCD.
           MOVE BR-TAP-OTHER-DL TO QBR-TAP-OTHER-DL.
           MOVE BR-AH-AND-CL TO QBR-AH-AND-CL.
           MOVE BR-NO-CASH-PROCEEDS TO QBR-NO-CASH-PROCEEDS.
           MOVE BR-RI-BEFORE-REPO TO QBR-RI-BEFORE-REPO.
           MOVE BR-RI-BEFORE-P4P5 TO QBR-RI-BEFORE-P4P5.
           MOVE BR-NOT-OURS-REFUND TO QBR-NOT-OURS-REFUND.
           MOVE BR-SFDLO3-WINDOW-FG TO QBR-SFDLO3-WINDOW-FG.
           MOVE BR-CALC-INS-ADJ TO QBR-CALC-INS-ADJ.
           MOVE BR-AC-BEFORE-REPO TO QBR-AC-BEFORE-REPO.
           MOVE BR-AC-BEFORE-P4P5 TO QBR-AC-BEFORE-P4P5.
           MOVE BR-BANK-ROUTING-NO TO QBR-BANK-ROUTING-NO.
           MOVE BR-BANK-CLEARING-GL TO QBR-BANK-CLEARING-GL.
           MOVE BR-GLBANK-DEPOSIT TO QBR-GLBANK-DEPOSIT.
           MOVE BR-BANK-ACCTNO TO QBR-BANK-ACCTNO.
           MOVE BR-DEFAULT-SC-SUBCLASS TO QBR-DEFAULT-SC-SUBCLASS.
           MOVE BR-NO-OUT-DLR-RSV TO QBR-NO-OUT-DLR-RSV.
           MOVE BR-COLLECTOR-QUEUE TO QBR-COLLECTOR-QUEUE.
           MOVE BR-COLL-QUEUE-CON-REC TO QBR-COLL-QUEUE-CON-REC.
           MOVE BR-JE-ALLOCATION-SKIP TO QBR-JE-ALLOCATION-SKIP.
           MOVE BR-EXPAND-APP-INPUT TO QBR-EXPAND-APP-INPUT.
           MOVE BR-A50-CPCORP TO QBR-A50-CPCORP.
           MOVE BR-INBR-BILNSC-FG TO QBR-INBR-BILNSC-FG.
           MOVE BR-INBR-BRIEF-FG TO QBR-INBR-BRIEF-FG.
           MOVE BR-LEDGER-JUST-INTCHG TO QBR-LEDGER-JUST-INTCHG.
           MOVE BR-BYINQ-START-PAGE TO QBR-BYINQ-START-PAGE.
           MOVE BR-DSINQ-START-PAGE TO QBR-DSINQ-START-PAGE.
           MOVE BR-APP-REQUIRED-FOR-INPUT
            TO QBR-APP-REQUIRED-FOR-INPUT.
           MOVE BR-CREDIT-SCORING-FG TO QBR-CREDIT-SCORING-FG.
           MOVE BR-COL-QUEUE-LOW-TO-HI TO QBR-COL-QUEUE-LOW-TO-HI.
           MOVE BR-CAN-GROUP-ID TO QBR-CAN-GROUP-ID.
           MOVE BR-AUTO-LNSTAT-WINDOW TO QBR-AUTO-LNSTAT-WINDOW.
           MOVE BR-TIME-ZONE-FACTOR TO QBR-TIME-ZONE-FACTOR.
           MOVE BR-LONPF-OPT-DISPLAY TO QBR-LONPF-OPT-DISPLAY.
           MOVE BR-GIFTNET TO QBR-GIFTNET.
           MOVE BR-GIFTNET-LEDGER-LINE TO QBR-GIFTNET-LEDGER-LINE.
           MOVE BR-NO-FT-EMP TO QBR-NO-FT-EMP.
           MOVE BR-NO-PT-EMP TO QBR-NO-PT-EMP.
           MOVE BR-BALLOON-LOANS-FG TO QBR-BALLOON-LOANS-FG.
           MOVE BR-GL-TYPE-ALLOWED TO QBR-GL-TYPE-ALLOWED.
           MOVE BR-GL-ENTRY-DEFAULT TO QBR-GL-ENTRY-DEFAULT.
           MOVE BR-MIDSTREAM-1STPAY-FG TO QBR-MIDSTREAM-1STPAY-FG.
           MOVE BR-NODEF-1ST-X-MONTHS TO QBR-NODEF-1ST-X-MONTHS.
           MOVE BR-ADD-INS-FEECODE TO QBR-ADD-INS-FEECODE.
           MOVE BR-ROAD-G-SGL-FEECODE TO QBR-ROAD-G-SGL-FEECODE.
           MOVE BR-ROAD-G-FAM-FEECODE TO QBR-ROAD-G-FAM-FEECODE.
           MOVE BR-NODEF-IF-ZERO-AMT TO QBR-NODEF-IF-ZERO-AMT.
           MOVE BR-ASSESS-LATE-CHGS TO QBR-ASSESS-LATE-CHGS.
           MOVE BR-NEXT-SOLICIT-NO TO QBR-NEXT-SOLICIT-NO.
           MOVE BR-ADDON-FEECD-O3(1) TO QBR-ADDON-FEECD-O3-1.
           MOVE BR-ADDON-FEECD-O3(2) TO QBR-ADDON-FEECD-O3-2.
           MOVE BR-IMMEDIATE-RECEIPT TO QBR-IMMEDIATE-RECEIPT.
           MOVE BR-IMMEDIATE-DOC-PRINT TO QBR-IMMEDIATE-DOC-PRINT.
           MOVE BR-AUTO-PL-REF-AC TO QBR-AUTO-PL-REF-AC.
           MOVE BR-FAX-NUMBER TO QBR-FAX-NUMBER.
           MOVE BR-ALPHA-DRAWERS TO QBR-ALPHA-DRAWERS.
           MOVE BR-LATECHG-ON-ALLOTMENTS TO QBR-LATECHG-ON-ALLOTMENTS.
           MOVE BR-ALLOW-SYS-AS-EFFDATE TO QBR-ALLOW-SYS-AS-EFFDATE.
           MOVE BR-TAX-PREBOOK-CUST TO QBR-TAX-PREBOOK-CUST.
           MOVE BR-TAX-PREBOOK-NONCUST TO QBR-TAX-PREBOOK-NONCUST.
           MOVE BR-PASSWORD-DRAWER TO QBR-PASSWORD-DRAWER.
           MOVE BR-ONESTEP-RN-FG TO QBR-ONESTEP-RN-FG.
           MOVE BR-LI-LISCAN-FG TO QBR-LI-LISCAN-FG.
           MOVE BR-HIDE-END-BAL TO QBR-HIDE-END-BAL.
           MOVE BR-LBOX-NOPAYOFF TO QBR-LBOX-NOPAYOFF.
           MOVE BR-LBOX-ODDPAY TO QBR-LBOX-ODDPAY.
           MOVE BR-LBOX-LC TO QBR-LBOX-LC.
           MOVE BR-LBOX-NOPRIN TO QBR-LBOX-NOPRIN.
           MOVE BR-LBOX-FLASH TO QBR-LBOX-FLASH.
           MOVE BR-LBOX-ACTIONCD TO QBR-LBOX-ACTIONCD.
           MOVE BR-LBOX-MAXCON TO QBR-LBOX-MAXCON.
           MOVE BR-LBOX-MAXREC TO QBR-LBOX-MAXREC.
           MOVE BR-LBOX-PL TO QBR-LBOX-PL.
           MOVE BR-LBOX-JD TO QBR-LBOX-JD.
           MOVE BR-LBOX TO QBR-LBOX.
           MOVE BR-LATECHG-ON-LBOX TO QBR-LATECHG-ON-LBOX.
           MOVE BR-ALLOW-OV-TRCD TO QBR-ALLOW-OV-TRCD.
           MOVE BR-GLOBAL-QUEUE TO QBR-GLOBAL-QUEUE.
           MOVE BR-LBOX-PAYOFF-NONCASH TO QBR-LBOX-PAYOFF-NONCASH.
           MOVE BR-LBOX-PAYOFF-OV TO QBR-LBOX-PAYOFF-OV.
           MOVE BR-COLLECT-MANAGER TO QBR-COLLECT-MANAGER.
           MOVE BR-COLLECT-PHONE TO QBR-COLLECT-PHONE.
           MOVE BR-COLLECT-EXT TO QBR-COLLECT-EXT.
           MOVE BR-ALT-INTO-THIS-BRANCH TO QBR-ALT-INTO-THIS-BRANCH.
           MOVE BR-POFF-TITLE-WINDOW TO QBR-POFF-TITLE-WINDOW.
           MOVE BR-COUPON-STATEMENT TO QBR-COUPON-STATEMENT.
           MOVE BR-SUPERVISOR TO QBR-SUPERVISOR.
           MOVE BR-AREA-MANAGER TO QBR-AREA-MANAGER.
           MOVE BR-MIN-MONTHS-DEF TO QBR-MIN-MONTHS-DEF.
           MOVE BR-CC-FLAG TO QBR-CC-FLAG.
           MOVE BR-CC-ADR1 TO QBR-CC-ADR1.
           MOVE BR-CC-CITY TO QBR-CC-CITY.
           MOVE BR-CC-STATE TO QBR-CC-STATE.
           MOVE BR-CC-ZIP TO QBR-CC-ZIP.
           MOVE BR-MIN-MONTHS-D2 TO QBR-MIN-MONTHS-D2.
           MOVE BR-SKIP-MIDSTREAM-TRW TO QBR-SKIP-MIDSTREAM-TRW.
           MOVE BR-GL-ESCROW-BALANCE TO QBR-GL-ESCROW-BALANCE.
           MOVE BR-GL-ESCROW-PAYABLE TO QBR-GL-ESCROW-PAYABLE.
           MOVE BR-ALLOW-FORCE-OPEN-FG TO QBR-ALLOW-FORCE-OPEN-FG.
           MOVE BR-ACH-BANK-NO TO QBR-ACH-BANK-NO.
           MOVE BR-WIMENU-CHECK-WRITING TO QBR-WIMENU-CHECK-WRITING.
           MOVE BR-ADP-PAY-GROUP TO QBR-ADP-PAY-GROUP.
           MOVE BR-ALLOW-SCANNED-CKS TO QBR-ALLOW-SCANNED-CKS.
           MOVE BR-CHECK-DEPOSIT-GL TO QBR-CHECK-DEPOSIT-GL.
           MOVE BR-OPTION-PST-LASER TO QBR-OPTION-PST-LASER.
           MOVE BR-USE-DEBIT-CARDS TO QBR-USE-DEBIT-CARDS.
           MOVE BR-GL-DEBIT-CARD-PROCEEDS
            TO QBR-GL-DEBIT-CARD-PROCEEDS.
           MOVE BR-WKDAY-BANKID TO QBR-WKDAY-BANKID.
           MOVE BR-PURGE-NOW-FLAG TO QBR-PURGE-NOW-FLAG.
           MOVE BR-EMAIL TO QBR-EMAIL.
           MOVE BR-MANAGER-EMAIL TO QBR-MANAGER-EMAIL.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBBRRN
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   ISNULL(CAST(BRFILE.BR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_ADR1,''),
                   ISNULL(BRFILE.BR_ADR2,''),
                   ISNULL(BRFILE.BR_CITY,''),
                   ISNULL(BRFILE.BR_STATE,''),
                   ISNULL(BRFILE.BR_ZIP,''),
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   ISNULL(BRFILE.BR_REMIT1,''),
                   ISNULL(BRFILE.BR_REMIT2,''),
                   ISNULL(BRFILE.BR_REMIT3,''),
                   ISNULL(BRFILE.BR_STMNT1,''),
                   ISNULL(BRFILE.BR_STMNT2,''),
                   BRFILE.BR_INS_PRODUCER_NUM,
                   ISNULL(BRFILE.BR_DLRCHK,''),
                   ISNULL(BRFILE.BR_FRONT_END_FG,''),
                   ISNULL(BRFILE.BR_EMLNRG_PHONE,''),
                   ISNULL(BRFILE.BR_POST_GL,''),
                   ISNULL(BRFILE.BR_ALLOTMNTS,''),
                   ISNULL(BRFILE.BR_EXTRACT_AUTO_UPD,''),
                   ISNULL(BRFILE.BR_PCNI_LEDGER,''),
                   ISNULL(BRFILE.BR_PRINTERFG_1,''),
                   ISNULL(BRFILE.BR_PRINTERFG_2,''),
                   ISNULL(BRFILE.BR_PRINTERFG_3,''),
                   ISNULL(BRFILE.BR_PRINTERFG_4,''),
                   ISNULL(BRFILE.BR_NEGDLRSV,''),
                   ISNULL(BRFILE.BR_CPCORP_ORDER,''),
                   ISNULL(BRFILE.BR_PRIO_MEMO_FLAG,''),
                   ISNULL(BRFILE.BR_PREBILL_PL_JD_OPT,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DOWNPY_FLAG,''),
                   ISNULL(BRFILE.BR_AUTO_INS_EARN,''),
                   ISNULL(BRFILE.BR_OTH_INS_LEVELS,''),
                   ISNULL(BRFILE.BR_ALLOW_INTERBR,''),
                   ISNULL(BRFILE.BR_UNER_DLANRP,''),
                   ISNULL(BRFILE.BR_SECOND_PROMISE,''),
                   ISNULL(BRFILE.BR_AVAIL_CREDIT,''),
                   ISNULL(BRFILE.BR_RECP1,''),
                   ISNULL(BRFILE.BR_RECP2,''),
                   ISNULL(BRFILE.BR_INC_WAIVERS,''),
                   ISNULL(BRFILE.BR_INC_INS,''),
                   BRFILE.BR_BP_NEXT_BATCH,
                   ISNULL(BRFILE.BR_BP_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_BP_ODDPAY,''),
                   ISNULL(BRFILE.BR_BP_LC,''),
                   ISNULL(BRFILE.BR_BP_NOPRIN,''),
                   ISNULL(BRFILE.BR_BP_FLASH,''),
                   ISNULL(BRFILE.BR_BP_ACTIONCD,''),
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   ISNULL(BRFILE.BR_BP_PL,''),
                   ISNULL(BRFILE.BR_BP_JD,''),
                   BRFILE.BR_BP_BRANCH,
                   ISNULL(BRFILE.BR_AGE_ERN,''),
                   ISNULL(BRFILE.BR_OTHER_BRANCH_GL,''),
                   ISNULL(BRFILE.BR_SALES_SMALL,''),
                   ISNULL(BRFILE.BR_NEW_COLL_INQUIRY,''),
                   ISNULL(BRFILE.BR_CHANGE_MEMO,''),
                   ISNULL(BRFILE.BR_INS_ADDON,''),
                   ISNULL(BRFILE.BR_TRAILER_REC,''),
                   ISNULL(BRFILE.BR_ALDEL_FLAG,''),
                   ISNULL(BRFILE.BR_IB_LCHG_MESS,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_PAST_DUE,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_2,''),
                   ISNULL(BRFILE.BR_AUTO_CHECK,''),
                   ISNULL(BRFILE.BR_RECEIPT_PROMDATE,''),
                   ISNULL(BRFILE.BR_RECEIPT_INTPD,''),
                   ISNULL(BRFILE.BR_COLL_ID_SCAN,''),
                   ISNULL(BRFILE.BR_STOP_ON_COLL,''),
                   ISNULL(BRFILE.BR_DUEDATE_CHANGE_CD,''),
                   ISNULL(BRFILE.BR_CHECK_EOM_FG,''),
                   ISNULL(BRFILE.BR_VOLUME_DLANRP,''),
                   ISNULL(BRFILE.BR_PDTH_NXDUE,''),
                   ISNULL(BRFILE.BR_LC_CODE,''),
                   ISNULL(BRFILE.BR_VALIDATE_PRUS,''),
                   ISNULL(BRFILE.BR_AUTO_OPEN_BAL,''),
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   ISNULL(BRFILE.BR_TENDER_CD_1,''),
                   ISNULL(BRFILE.BR_TENDER_CD_2,''),
                   ISNULL(BRFILE.BR_TENDER_CD_3,''),
                   ISNULL(BRFILE.BR_DLPARV_ADJ_FG,''),
                   ISNULL(BRFILE.BR_OVERRIDE_BR_STATE,''),
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   ISNULL(BRFILE.BR_RECDEL_NOT_REC,''),
                   ISNULL(BRFILE.BR_POST_AHEAD_FG,''),
                   ISNULL(BRFILE.BR_CAN_LOANS_FG,''),
                   ISNULL(BRFILE.BR_NON_MONTHLY_CAN_LOANS,''),
                   ISNULL(BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,''),
                   ISNULL(BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_OUR_FEE,''),
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   ISNULL(BRFILE.BR_LCARDFG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_MANAGER,''),
                   ISNULL(BRFILE.BR_EOD_PROCEDURE,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_OV,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   ISNULL(BRFILE.BR_LR_GROUP_COLLID,''),
                   ISNULL(BRFILE.BR_UNITPER_FG,''),
                   ISNULL(BRFILE.BR_UNITPER_Y_D_FG,''),
                   ISNULL(BRFILE.BR_NO_PROMPT_FOR_OTHBR,''),
                   ISNULL(BRFILE.BR_WORLD_BRANCH_TYPE,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,''),
                   ISNULL(BRFILE.BR_CONVERT_AUTO_ACCTNO,''),
                   ISNULL(BRFILE.BR_TS_REPO_INS_FG,''),
                   ISNULL(BRFILE.BR_BOOK_SF_BR_ONLY,''),
                   ISNULL(BRFILE.BR_FORCE_VALID_CITY_COUNTY,''),
                   ISNULL(BRFILE.BR_SET_COLLCD_DL_MFGCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_PURCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_PURCD,''),
                   ISNULL(BRFILE.BR_TAP_OTHER_DL,''),
                   ISNULL(BRFILE.BR_AH_AND_CL,''),
                   ISNULL(BRFILE.BR_NO_CASH_PROCEEDS,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_P4P5,''),
                   ISNULL(BRFILE.BR_NOT_OURS_REFUND,''),
                   ISNULL(BRFILE.BR_SFDLO3_WINDOW_FG,''),
                   ISNULL(BRFILE.BR_CALC_INS_ADJ,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_P4P5,''),
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   ISNULL(BRFILE.BR_BANK_ACCTNO,''),
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   ISNULL(BRFILE.BR_NO_OUT_DLR_RSV,''),
                   ISNULL(BRFILE.BR_COLLECTOR_QUEUE,''),
                   ISNULL(BRFILE.BR_COLL_QUEUE_CON_REC,''),
                   ISNULL(BRFILE.BR_JE_ALLOCATION_SKIP,''),
                   ISNULL(BRFILE.BR_EXPAND_APP_INPUT,''),
                   ISNULL(BRFILE.BR_A50_CPCORP,''),
                   ISNULL(BRFILE.BR_INBR_BILNSC_FG,''),
                   ISNULL(BRFILE.BR_INBR_BRIEF_FG,''),
                   ISNULL(BRFILE.BR_LEDGER_JUST_INTCHG,''),
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   ISNULL(BRFILE.BR_APP_REQUIRED_FOR_INPUT,''),
                   ISNULL(BRFILE.BR_CREDIT_SCORING_FG,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_LOW_TO_HI,''),
                   ISNULL(BRFILE.BR_CAN_GROUP_ID,''),
                   ISNULL(BRFILE.BR_AUTO_LNSTAT_WINDOW,''),
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   ISNULL(BRFILE.BR_LONPF_OPT_DISPLAY,''),
                   ISNULL(BRFILE.BR_GIFTNET,''),
                   ISNULL(BRFILE.BR_GIFTNET_LEDGER_LINE,''),
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   ISNULL(BRFILE.BR_BALLOON_LOANS_FG,''),
                   ISNULL(BRFILE.BR_GL_TYPE_ALLOWED,''),
                   ISNULL(BRFILE.BR_GL_ENTRY_DEFAULT,''),
                   ISNULL(BRFILE.BR_MIDSTREAM_1STPAY_FG,''),
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   ISNULL(BRFILE.BR_ADD_INS_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_SGL_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_FAM_FEECODE,''),
                   ISNULL(BRFILE.BR_NODEF_IF_ZERO_AMT,''),
                   ISNULL(BRFILE.BR_ASSESS_LATE_CHGS,''),
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_2,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_RECEIPT,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_DOC_PRINT,''),
                   ISNULL(BRFILE.BR_AUTO_PL_REF_AC,''),
                   BRFILE.BR_FAX_NUMBER,
                   ISNULL(BRFILE.BR_ALPHA_DRAWERS,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_ALLOTMENTS,''),
                   ISNULL(BRFILE.BR_ALLOW_SYS_AS_EFFDATE,''),
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   ISNULL(BRFILE.BR_PASSWORD_DRAWER,''),
                   ISNULL(BRFILE.BR_ONESTEP_RN_FG,''),
                   ISNULL(BRFILE.BR_LI_LISCAN_FG,''),
                   ISNULL(BRFILE.BR_HIDE_END_BAL,''),
                   ISNULL(BRFILE.BR_LBOX_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_LBOX_ODDPAY,''),
                   ISNULL(BRFILE.BR_LBOX_LC,''),
                   ISNULL(BRFILE.BR_LBOX_NOPRIN,''),
                   ISNULL(BRFILE.BR_LBOX_FLASH,''),
                   ISNULL(BRFILE.BR_LBOX_ACTIONCD,''),
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   ISNULL(BRFILE.BR_LBOX_PL,''),
                   ISNULL(BRFILE.BR_LBOX_JD,''),
                   ISNULL(BRFILE.BR_LBOX,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_LBOX,''),
                   ISNULL(BRFILE.BR_ALLOW_OV_TRCD,''),
                   ISNULL(BRFILE.BR_GLOBAL_QUEUE,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_OV,''),
                   ISNULL(BRFILE.BR_COLLECT_MANAGER,''),
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   ISNULL(BRFILE.BR_ALT_INTO_THIS_BRANCH,''),
                   ISNULL(BRFILE.BR_POFF_TITLE_WINDOW,''),
                   ISNULL(BRFILE.BR_COUPON_STATEMENT,''),
                   ISNULL(BRFILE.BR_SUPERVISOR,''),
                   ISNULL(BRFILE.BR_AREA_MANAGER,''),
                   BRFILE.BR_MIN_MONTHS_DEF,
                   ISNULL(BRFILE.BR_CC_FLAG,''),
                   ISNULL(BRFILE.BR_CC_ADR1,''),
                   ISNULL(BRFILE.BR_CC_CITY,''),
                   ISNULL(BRFILE.BR_CC_STATE,''),
                   ISNULL(BRFILE.BR_CC_ZIP,''),
                   BRFILE.BR_MIN_MONTHS_D2,
                   ISNULL(BRFILE.BR_SKIP_MIDSTREAM_TRW,''),
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   ISNULL(BRFILE.BR_ALLOW_FORCE_OPEN_FG,''),
                   BRFILE.BR_ACH_BANK_NO,
                   ISNULL(BRFILE.BR_WIMENU_CHECK_WRITING,''),
                   ISNULL(BRFILE.BR_ADP_PAY_GROUP,''),
                   ISNULL(BRFILE.BR_ALLOW_SCANNED_CKS,''),
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   ISNULL(BRFILE.BR_OPTION_PST_LASER,''),
                   ISNULL(BRFILE.BR_USE_DEBIT_CARDS,''),
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   ISNULL(BRFILE.BR_PURGE_NOW_FLAG,''),
                   ISNULL(BRFILE.BR_EMAIL,''),
                   ISNULL(BRFILE.BR_MANAGER_EMAIL,'')
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   ISNULL(CAST(BRFILE.BR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_ADR1,''),
                   ISNULL(BRFILE.BR_ADR2,''),
                   ISNULL(BRFILE.BR_CITY,''),
                   ISNULL(BRFILE.BR_STATE,''),
                   ISNULL(BRFILE.BR_ZIP,''),
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   ISNULL(BRFILE.BR_REMIT1,''),
                   ISNULL(BRFILE.BR_REMIT2,''),
                   ISNULL(BRFILE.BR_REMIT3,''),
                   ISNULL(BRFILE.BR_STMNT1,''),
                   ISNULL(BRFILE.BR_STMNT2,''),
                   BRFILE.BR_INS_PRODUCER_NUM,
                   ISNULL(BRFILE.BR_DLRCHK,''),
                   ISNULL(BRFILE.BR_FRONT_END_FG,''),
                   ISNULL(BRFILE.BR_EMLNRG_PHONE,''),
                   ISNULL(BRFILE.BR_POST_GL,''),
                   ISNULL(BRFILE.BR_ALLOTMNTS,''),
                   ISNULL(BRFILE.BR_EXTRACT_AUTO_UPD,''),
                   ISNULL(BRFILE.BR_PCNI_LEDGER,''),
                   ISNULL(BRFILE.BR_PRINTERFG_1,''),
                   ISNULL(BRFILE.BR_PRINTERFG_2,''),
                   ISNULL(BRFILE.BR_PRINTERFG_3,''),
                   ISNULL(BRFILE.BR_PRINTERFG_4,''),
                   ISNULL(BRFILE.BR_NEGDLRSV,''),
                   ISNULL(BRFILE.BR_CPCORP_ORDER,''),
                   ISNULL(BRFILE.BR_PRIO_MEMO_FLAG,''),
                   ISNULL(BRFILE.BR_PREBILL_PL_JD_OPT,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DOWNPY_FLAG,''),
                   ISNULL(BRFILE.BR_AUTO_INS_EARN,''),
                   ISNULL(BRFILE.BR_OTH_INS_LEVELS,''),
                   ISNULL(BRFILE.BR_ALLOW_INTERBR,''),
                   ISNULL(BRFILE.BR_UNER_DLANRP,''),
                   ISNULL(BRFILE.BR_SECOND_PROMISE,''),
                   ISNULL(BRFILE.BR_AVAIL_CREDIT,''),
                   ISNULL(BRFILE.BR_RECP1,''),
                   ISNULL(BRFILE.BR_RECP2,''),
                   ISNULL(BRFILE.BR_INC_WAIVERS,''),
                   ISNULL(BRFILE.BR_INC_INS,''),
                   BRFILE.BR_BP_NEXT_BATCH,
                   ISNULL(BRFILE.BR_BP_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_BP_ODDPAY,''),
                   ISNULL(BRFILE.BR_BP_LC,''),
                   ISNULL(BRFILE.BR_BP_NOPRIN,''),
                   ISNULL(BRFILE.BR_BP_FLASH,''),
                   ISNULL(BRFILE.BR_BP_ACTIONCD,''),
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   ISNULL(BRFILE.BR_BP_PL,''),
                   ISNULL(BRFILE.BR_BP_JD,''),
                   BRFILE.BR_BP_BRANCH,
                   ISNULL(BRFILE.BR_AGE_ERN,''),
                   ISNULL(BRFILE.BR_OTHER_BRANCH_GL,''),
                   ISNULL(BRFILE.BR_SALES_SMALL,''),
                   ISNULL(BRFILE.BR_NEW_COLL_INQUIRY,''),
                   ISNULL(BRFILE.BR_CHANGE_MEMO,''),
                   ISNULL(BRFILE.BR_INS_ADDON,''),
                   ISNULL(BRFILE.BR_TRAILER_REC,''),
                   ISNULL(BRFILE.BR_ALDEL_FLAG,''),
                   ISNULL(BRFILE.BR_IB_LCHG_MESS,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_PAST_DUE,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_2,''),
                   ISNULL(BRFILE.BR_AUTO_CHECK,''),
                   ISNULL(BRFILE.BR_RECEIPT_PROMDATE,''),
                   ISNULL(BRFILE.BR_RECEIPT_INTPD,''),
                   ISNULL(BRFILE.BR_COLL_ID_SCAN,''),
                   ISNULL(BRFILE.BR_STOP_ON_COLL,''),
                   ISNULL(BRFILE.BR_DUEDATE_CHANGE_CD,''),
                   ISNULL(BRFILE.BR_CHECK_EOM_FG,''),
                   ISNULL(BRFILE.BR_VOLUME_DLANRP,''),
                   ISNULL(BRFILE.BR_PDTH_NXDUE,''),
                   ISNULL(BRFILE.BR_LC_CODE,''),
                   ISNULL(BRFILE.BR_VALIDATE_PRUS,''),
                   ISNULL(BRFILE.BR_AUTO_OPEN_BAL,''),
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   ISNULL(BRFILE.BR_TENDER_CD_1,''),
                   ISNULL(BRFILE.BR_TENDER_CD_2,''),
                   ISNULL(BRFILE.BR_TENDER_CD_3,''),
                   ISNULL(BRFILE.BR_DLPARV_ADJ_FG,''),
                   ISNULL(BRFILE.BR_OVERRIDE_BR_STATE,''),
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   ISNULL(BRFILE.BR_RECDEL_NOT_REC,''),
                   ISNULL(BRFILE.BR_POST_AHEAD_FG,''),
                   ISNULL(BRFILE.BR_CAN_LOANS_FG,''),
                   ISNULL(BRFILE.BR_NON_MONTHLY_CAN_LOANS,''),
                   ISNULL(BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,''),
                   ISNULL(BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_OUR_FEE,''),
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   ISNULL(BRFILE.BR_LCARDFG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_MANAGER,''),
                   ISNULL(BRFILE.BR_EOD_PROCEDURE,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_OV,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   ISNULL(BRFILE.BR_LR_GROUP_COLLID,''),
                   ISNULL(BRFILE.BR_UNITPER_FG,''),
                   ISNULL(BRFILE.BR_UNITPER_Y_D_FG,''),
                   ISNULL(BRFILE.BR_NO_PROMPT_FOR_OTHBR,''),
                   ISNULL(BRFILE.BR_WORLD_BRANCH_TYPE,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,''),
                   ISNULL(BRFILE.BR_CONVERT_AUTO_ACCTNO,''),
                   ISNULL(BRFILE.BR_TS_REPO_INS_FG,''),
                   ISNULL(BRFILE.BR_BOOK_SF_BR_ONLY,''),
                   ISNULL(BRFILE.BR_FORCE_VALID_CITY_COUNTY,''),
                   ISNULL(BRFILE.BR_SET_COLLCD_DL_MFGCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_PURCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_PURCD,''),
                   ISNULL(BRFILE.BR_TAP_OTHER_DL,''),
                   ISNULL(BRFILE.BR_AH_AND_CL,''),
                   ISNULL(BRFILE.BR_NO_CASH_PROCEEDS,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_P4P5,''),
                   ISNULL(BRFILE.BR_NOT_OURS_REFUND,''),
                   ISNULL(BRFILE.BR_SFDLO3_WINDOW_FG,''),
                   ISNULL(BRFILE.BR_CALC_INS_ADJ,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_P4P5,''),
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   ISNULL(BRFILE.BR_BANK_ACCTNO,''),
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   ISNULL(BRFILE.BR_NO_OUT_DLR_RSV,''),
                   ISNULL(BRFILE.BR_COLLECTOR_QUEUE,''),
                   ISNULL(BRFILE.BR_COLL_QUEUE_CON_REC,''),
                   ISNULL(BRFILE.BR_JE_ALLOCATION_SKIP,''),
                   ISNULL(BRFILE.BR_EXPAND_APP_INPUT,''),
                   ISNULL(BRFILE.BR_A50_CPCORP,''),
                   ISNULL(BRFILE.BR_INBR_BILNSC_FG,''),
                   ISNULL(BRFILE.BR_INBR_BRIEF_FG,''),
                   ISNULL(BRFILE.BR_LEDGER_JUST_INTCHG,''),
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   ISNULL(BRFILE.BR_APP_REQUIRED_FOR_INPUT,''),
                   ISNULL(BRFILE.BR_CREDIT_SCORING_FG,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_LOW_TO_HI,''),
                   ISNULL(BRFILE.BR_CAN_GROUP_ID,''),
                   ISNULL(BRFILE.BR_AUTO_LNSTAT_WINDOW,''),
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   ISNULL(BRFILE.BR_LONPF_OPT_DISPLAY,''),
                   ISNULL(BRFILE.BR_GIFTNET,''),
                   ISNULL(BRFILE.BR_GIFTNET_LEDGER_LINE,''),
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   ISNULL(BRFILE.BR_BALLOON_LOANS_FG,''),
                   ISNULL(BRFILE.BR_GL_TYPE_ALLOWED,''),
                   ISNULL(BRFILE.BR_GL_ENTRY_DEFAULT,''),
                   ISNULL(BRFILE.BR_MIDSTREAM_1STPAY_FG,''),
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   ISNULL(BRFILE.BR_ADD_INS_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_SGL_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_FAM_FEECODE,''),
                   ISNULL(BRFILE.BR_NODEF_IF_ZERO_AMT,''),
                   ISNULL(BRFILE.BR_ASSESS_LATE_CHGS,''),
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_2,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_RECEIPT,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_DOC_PRINT,''),
                   ISNULL(BRFILE.BR_AUTO_PL_REF_AC,''),
                   BRFILE.BR_FAX_NUMBER,
                   ISNULL(BRFILE.BR_ALPHA_DRAWERS,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_ALLOTMENTS,''),
                   ISNULL(BRFILE.BR_ALLOW_SYS_AS_EFFDATE,''),
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   ISNULL(BRFILE.BR_PASSWORD_DRAWER,''),
                   ISNULL(BRFILE.BR_ONESTEP_RN_FG,''),
                   ISNULL(BRFILE.BR_LI_LISCAN_FG,''),
                   ISNULL(BRFILE.BR_HIDE_END_BAL,''),
                   ISNULL(BRFILE.BR_LBOX_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_LBOX_ODDPAY,''),
                   ISNULL(BRFILE.BR_LBOX_LC,''),
                   ISNULL(BRFILE.BR_LBOX_NOPRIN,''),
                   ISNULL(BRFILE.BR_LBOX_FLASH,''),
                   ISNULL(BRFILE.BR_LBOX_ACTIONCD,''),
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   ISNULL(BRFILE.BR_LBOX_PL,''),
                   ISNULL(BRFILE.BR_LBOX_JD,''),
                   ISNULL(BRFILE.BR_LBOX,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_LBOX,''),
                   ISNULL(BRFILE.BR_ALLOW_OV_TRCD,''),
                   ISNULL(BRFILE.BR_GLOBAL_QUEUE,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_OV,''),
                   ISNULL(BRFILE.BR_COLLECT_MANAGER,''),
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   ISNULL(BRFILE.BR_ALT_INTO_THIS_BRANCH,''),
                   ISNULL(BRFILE.BR_POFF_TITLE_WINDOW,''),
                   ISNULL(BRFILE.BR_COUPON_STATEMENT,''),
                   ISNULL(BRFILE.BR_SUPERVISOR,''),
                   ISNULL(BRFILE.BR_AREA_MANAGER,''),
                   BRFILE.BR_MIN_MONTHS_DEF,
                   ISNULL(BRFILE.BR_CC_FLAG,''),
                   ISNULL(BRFILE.BR_CC_ADR1,''),
                   ISNULL(BRFILE.BR_CC_CITY,''),
                   ISNULL(BRFILE.BR_CC_STATE,''),
                   ISNULL(BRFILE.BR_CC_ZIP,''),
                   BRFILE.BR_MIN_MONTHS_D2,
                   ISNULL(BRFILE.BR_SKIP_MIDSTREAM_TRW,''),
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   ISNULL(BRFILE.BR_ALLOW_FORCE_OPEN_FG,''),
                   BRFILE.BR_ACH_BANK_NO,
                   ISNULL(BRFILE.BR_WIMENU_CHECK_WRITING,''),
                   ISNULL(BRFILE.BR_ADP_PAY_GROUP,''),
                   ISNULL(BRFILE.BR_ALLOW_SCANNED_CKS,''),
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   ISNULL(BRFILE.BR_OPTION_PST_LASER,''),
                   ISNULL(BRFILE.BR_USE_DEBIT_CARDS,''),
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   ISNULL(BRFILE.BR_PURGE_NOW_FLAG,''),
                   ISNULL(BRFILE.BR_EMAIL,''),
                   ISNULL(BRFILE.BR_MANAGER_EMAIL,'')
            INTO :QBR-REC
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO :QBR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BR-FILE.

           ACCEPT BR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           PERFORM SET-BR-FIELDS.

           EXEC SQL
            INSERT INTO DBO.BRFILE
             (BRFILE.BR_NO,
              BRFILE.BR_LTOUCH_DATE,
              BRFILE.BR_NAME,
              BRFILE.BR_ADR1,
              BRFILE.BR_ADR2,
              BRFILE.BR_CITY,
              BRFILE.BR_STATE,
              BRFILE.BR_ZIP,
              BRFILE.BR_PHONE,
              BRFILE.BR_RETAIN,
              BRFILE.BR_NEXT_CHECK,
              BRFILE.BR_PRINTER_1,
              BRFILE.BR_PRINTER_2,
              BRFILE.BR_PRINTER_3,
              BRFILE.BR_PRINTER_4,
              BRFILE.BR_FEE_GL_BRNO,
              BRFILE.BR_REMIT1,
              BRFILE.BR_REMIT2,
              BRFILE.BR_REMIT3,
              BRFILE.BR_STMNT1,
              BRFILE.BR_STMNT2,
              BRFILE.BR_INS_PRODUCER_NUM,
              BRFILE.BR_DLRCHK,
              BRFILE.BR_FRONT_END_FG,
              BRFILE.BR_EMLNRG_PHONE,
              BRFILE.BR_POST_GL,
              BRFILE.BR_ALLOTMNTS,
              BRFILE.BR_EXTRACT_AUTO_UPD,
              BRFILE.BR_PCNI_LEDGER,
              BRFILE.BR_PRINTERFG_1,
              BRFILE.BR_PRINTERFG_2,
              BRFILE.BR_PRINTERFG_3,
              BRFILE.BR_PRINTERFG_4,
              BRFILE.BR_NEGDLRSV,
              BRFILE.BR_CPCORP_ORDER,
              BRFILE.BR_PRIO_MEMO_FLAG,
              BRFILE.BR_PREBILL_PL_JD_OPT,
              BRFILE.BR_LOAN_NUM_FLAG,
              BRFILE.BR_DOWNPY_FLAG,
              BRFILE.BR_AUTO_INS_EARN,
              BRFILE.BR_OTH_INS_LEVELS,
              BRFILE.BR_ALLOW_INTERBR,
              BRFILE.BR_UNER_DLANRP,
              BRFILE.BR_SECOND_PROMISE,
              BRFILE.BR_AVAIL_CREDIT,
              BRFILE.BR_RECP1,
              BRFILE.BR_RECP2,
              BRFILE.BR_INC_WAIVERS,
              BRFILE.BR_INC_INS,
              BRFILE.BR_BP_NEXT_BATCH,
              BRFILE.BR_BP_NOPAYOFF,
              BRFILE.BR_BP_ODDPAY,
              BRFILE.BR_BP_LC,
              BRFILE.BR_BP_NOPRIN,
              BRFILE.BR_BP_FLASH,
              BRFILE.BR_BP_ACTIONCD,
              BRFILE.BR_BP_MAXCON,
              BRFILE.BR_BP_MAXREC,
              BRFILE.BR_BP_PL,
              BRFILE.BR_BP_JD,
              BRFILE.BR_BP_BRANCH,
              BRFILE.BR_AGE_ERN,
              BRFILE.BR_OTHER_BRANCH_GL,
              BRFILE.BR_SALES_SMALL,
              BRFILE.BR_NEW_COLL_INQUIRY,
              BRFILE.BR_CHANGE_MEMO,
              BRFILE.BR_INS_ADDON,
              BRFILE.BR_TRAILER_REC,
              BRFILE.BR_ALDEL_FLAG,
              BRFILE.BR_IB_LCHG_MESS,
              BRFILE.BR_COL_QUEUE_PAST_DUE,
              BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
              BRFILE.BR_ADDON_FEECD_O2_1,
              BRFILE.BR_ADDON_FEECD_O2_2,
              BRFILE.BR_AUTO_CHECK,
              BRFILE.BR_RECEIPT_PROMDATE,
              BRFILE.BR_RECEIPT_INTPD,
              BRFILE.BR_COLL_ID_SCAN,
              BRFILE.BR_STOP_ON_COLL,
              BRFILE.BR_DUEDATE_CHANGE_CD,
              BRFILE.BR_CHECK_EOM_FG,
              BRFILE.BR_VOLUME_DLANRP,
              BRFILE.BR_PDTH_NXDUE,
              BRFILE.BR_LC_CODE,
              BRFILE.BR_VALIDATE_PRUS,
              BRFILE.BR_AUTO_OPEN_BAL,
              BRFILE.BR_OPEN_BAL_1,
              BRFILE.BR_OPEN_BAL_2,
              BRFILE.BR_OPEN_BAL_3,
              BRFILE.BR_OPEN_BAL_4,
              BRFILE.BR_OPEN_BAL_5,
              BRFILE.BR_OPEN_BAL_6,
              BRFILE.BR_OPEN_BAL_7,
              BRFILE.BR_OPEN_BAL_8,
              BRFILE.BR_OPEN_BAL_9,
              BRFILE.BR_OPEN_BAL_10,
              BRFILE.BR_OPEN_BAL_11,
              BRFILE.BR_OPEN_BAL_12,
              BRFILE.BR_OPEN_BAL_13,
              BRFILE.BR_OPEN_BAL_14,
              BRFILE.BR_OPEN_BAL_15,
              BRFILE.BR_OPEN_BAL_16,
              BRFILE.BR_OPEN_BAL_17,
              BRFILE.BR_OPEN_BAL_18,
              BRFILE.BR_OPEN_BAL_19,
              BRFILE.BR_OPEN_BAL_20,
              BRFILE.BR_TENDER_CD_1,
              BRFILE.BR_TENDER_CD_2,
              BRFILE.BR_TENDER_CD_3,
              BRFILE.BR_DLPARV_ADJ_FG,
              BRFILE.BR_OVERRIDE_BR_STATE,
              BRFILE.BR_DEFAULT_SC_CLASS,
              BRFILE.BR_RECDEL_NOT_REC,
              BRFILE.BR_POST_AHEAD_FG,
              BRFILE.BR_CAN_LOANS_FG,
              BRFILE.BR_NON_MONTHLY_CAN_LOANS,
              BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,
              BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,
              BRFILE.BR_REGION_NUMBER,
              BRFILE.BR_OUR_FEE,
              BRFILE.BR_REB_METHOD_1,
              BRFILE.BR_REB_METHOD_2,
              BRFILE.BR_REB_METHOD_3,
              BRFILE.BR_REB_METHOD_4,
              BRFILE.BR_REB_METHOD_5,
              BRFILE.BR_REB_METHOD_6,
              BRFILE.BR_REB_METHOD_7,
              BRFILE.BR_REB_METHOD_8,
              BRFILE.BR_REB_METHOD_9,
              BRFILE.BR_REB_METHOD_10,
              BRFILE.BR_REB_METHOD_11,
              BRFILE.BR_REBDAYS_MONTH_1,
              BRFILE.BR_REBDAYS_MONTH_2,
              BRFILE.BR_REBDAYS_MONTH_3,
              BRFILE.BR_REBDAYS_MONTH_4,
              BRFILE.BR_REBDAYS_MONTH_5,
              BRFILE.BR_REBDAYS_MONTH_6,
              BRFILE.BR_REBDAYS_MONTH_7,
              BRFILE.BR_REBDAYS_MONTH_8,
              BRFILE.BR_REBDAYS_MONTH_9,
              BRFILE.BR_REBDAYS_MONTH_10,
              BRFILE.BR_REBDAYS_MONTH_11,
              BRFILE.BR_ACCRUAL_MONTHS_1,
              BRFILE.BR_ACCRUAL_MONTHS_2,
              BRFILE.BR_ACCRUAL_MONTHS_3,
              BRFILE.BR_ACCRUAL_MONTHS_4,
              BRFILE.BR_ACCRUAL_MONTHS_5,
              BRFILE.BR_ACCRUAL_MONTHS_6,
              BRFILE.BR_ACCRUAL_MONTHS_7,
              BRFILE.BR_ACCRUAL_MONTHS_8,
              BRFILE.BR_ACCRUAL_MONTHS_9,
              BRFILE.BR_ACCRUAL_MONTHS_10,
              BRFILE.BR_ACCRUAL_MONTHS_11,
              BRFILE.BR_GLOVERSHORT,
              BRFILE.BR_GLBANK,
              BRFILE.BR_GLONHAND,
              BRFILE.BR_BANKBAL,
              BRFILE.BR_RETAIN_AMT,
              BRFILE.BR_LCARDFG,
              BRFILE.BR_DATA_PATH,
              BRFILE.BR_MACHINE,
              BRFILE.BR_MANAGER,
              BRFILE.BR_EOD_PROCEDURE,
              BRFILE.BR_BP_PAYOFF_NONCASH,
              BRFILE.BR_BP_PAYOFF_OV,
              BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
              BRFILE.BR_LR_GROUP_COLLID,
              BRFILE.BR_UNITPER_FG,
              BRFILE.BR_UNITPER_Y_D_FG,
              BRFILE.BR_NO_PROMPT_FOR_OTHBR,
              BRFILE.BR_WORLD_BRANCH_TYPE,
              BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,
              BRFILE.BR_CONVERT_AUTO_ACCTNO,
              BRFILE.BR_TS_REPO_INS_FG,
              BRFILE.BR_BOOK_SF_BR_ONLY,
              BRFILE.BR_FORCE_VALID_CITY_COUNTY,
              BRFILE.BR_SET_COLLCD_DL_MFGCD,
              BRFILE.BR_DEFAULT_SF_UNITPER_CD,
              BRFILE.BR_DEFAULT_SF_PURCD,
              BRFILE.BR_DEFAULT_LN_UNITPER_CD,
              BRFILE.BR_DEFAULT_LN_PURCD,
              BRFILE.BR_TAP_OTHER_DL,
              BRFILE.BR_AH_AND_CL,
              BRFILE.BR_NO_CASH_PROCEEDS,
              BRFILE.BR_RI_BEFORE_REPO,
              BRFILE.BR_RI_BEFORE_P4P5,
              BRFILE.BR_NOT_OURS_REFUND,
              BRFILE.BR_SFDLO3_WINDOW_FG,
              BRFILE.BR_CALC_INS_ADJ,
              BRFILE.BR_AC_BEFORE_REPO,
              BRFILE.BR_AC_BEFORE_P4P5,
              BRFILE.BR_BANK_ROUTING_NO,
              BRFILE.BR_BANK_CLEARING_GL,
              BRFILE.BR_GLBANK_DEPOSIT,
              BRFILE.BR_BANK_ACCTNO,
              BRFILE.BR_DEFAULT_SC_SUBCLASS,
              BRFILE.BR_NO_OUT_DLR_RSV,
              BRFILE.BR_COLLECTOR_QUEUE,
              BRFILE.BR_COLL_QUEUE_CON_REC,
              BRFILE.BR_JE_ALLOCATION_SKIP,
              BRFILE.BR_EXPAND_APP_INPUT,
              BRFILE.BR_A50_CPCORP,
              BRFILE.BR_INBR_BILNSC_FG,
              BRFILE.BR_INBR_BRIEF_FG,
              BRFILE.BR_LEDGER_JUST_INTCHG,
              BRFILE.BR_BYINQ_START_PAGE,
              BRFILE.BR_DSINQ_START_PAGE,
              BRFILE.BR_APP_REQUIRED_FOR_INPUT,
              BRFILE.BR_CREDIT_SCORING_FG,
              BRFILE.BR_COL_QUEUE_LOW_TO_HI,
              BRFILE.BR_CAN_GROUP_ID,
              BRFILE.BR_AUTO_LNSTAT_WINDOW,
              BRFILE.BR_TIME_ZONE_FACTOR,
              BRFILE.BR_LONPF_OPT_DISPLAY,
              BRFILE.BR_GIFTNET,
              BRFILE.BR_GIFTNET_LEDGER_LINE,
              BRFILE.BR_NO_FT_EMP,
              BRFILE.BR_NO_PT_EMP,
              BRFILE.BR_BALLOON_LOANS_FG,
              BRFILE.BR_GL_TYPE_ALLOWED,
              BRFILE.BR_GL_ENTRY_DEFAULT,
              BRFILE.BR_MIDSTREAM_1STPAY_FG,
              BRFILE.BR_NODEF_1ST_X_MONTHS,
              BRFILE.BR_ADD_INS_FEECODE,
              BRFILE.BR_ROAD_G_SGL_FEECODE,
              BRFILE.BR_ROAD_G_FAM_FEECODE,
              BRFILE.BR_NODEF_IF_ZERO_AMT,
              BRFILE.BR_ASSESS_LATE_CHGS,
              BRFILE.BR_NEXT_SOLICIT_NO,
              BRFILE.BR_ADDON_FEECD_O3_1,
              BRFILE.BR_ADDON_FEECD_O3_2,
              BRFILE.BR_IMMEDIATE_RECEIPT,
              BRFILE.BR_IMMEDIATE_DOC_PRINT,
              BRFILE.BR_AUTO_PL_REF_AC,
              BRFILE.BR_FAX_NUMBER,
              BRFILE.BR_ALPHA_DRAWERS,
              BRFILE.BR_LATECHG_ON_ALLOTMENTS,
              BRFILE.BR_ALLOW_SYS_AS_EFFDATE,
              BRFILE.BR_TAX_PREBOOK_CUST,
              BRFILE.BR_TAX_PREBOOK_NONCUST,
              BRFILE.BR_PASSWORD_DRAWER,
              BRFILE.BR_ONESTEP_RN_FG,
              BRFILE.BR_LI_LISCAN_FG,
              BRFILE.BR_HIDE_END_BAL,
              BRFILE.BR_LBOX_NOPAYOFF,
              BRFILE.BR_LBOX_ODDPAY,
              BRFILE.BR_LBOX_LC,
              BRFILE.BR_LBOX_NOPRIN,
              BRFILE.BR_LBOX_FLASH,
              BRFILE.BR_LBOX_ACTIONCD,
              BRFILE.BR_LBOX_MAXCON,
              BRFILE.BR_LBOX_MAXREC,
              BRFILE.BR_LBOX_PL,
              BRFILE.BR_LBOX_JD,
              BRFILE.BR_LBOX,
              BRFILE.BR_LATECHG_ON_LBOX,
              BRFILE.BR_ALLOW_OV_TRCD,
              BRFILE.BR_GLOBAL_QUEUE,
              BRFILE.BR_LBOX_PAYOFF_NONCASH,
              BRFILE.BR_LBOX_PAYOFF_OV,
              BRFILE.BR_COLLECT_MANAGER,
              BRFILE.BR_COLLECT_PHONE,
              BRFILE.BR_COLLECT_EXT,
              BRFILE.BR_ALT_INTO_THIS_BRANCH,
              BRFILE.BR_POFF_TITLE_WINDOW,
              BRFILE.BR_COUPON_STATEMENT,
              BRFILE.BR_SUPERVISOR,
              BRFILE.BR_AREA_MANAGER,
              BRFILE.BR_MIN_MONTHS_DEF,
              BRFILE.BR_CC_FLAG,
              BRFILE.BR_CC_ADR1,
              BRFILE.BR_CC_CITY,
              BRFILE.BR_CC_STATE,
              BRFILE.BR_CC_ZIP,
              BRFILE.BR_MIN_MONTHS_D2,
              BRFILE.BR_SKIP_MIDSTREAM_TRW,
              BRFILE.BR_GL_ESCROW_BALANCE,
              BRFILE.BR_GL_ESCROW_PAYABLE,
              BRFILE.BR_ALLOW_FORCE_OPEN_FG,
              BRFILE.BR_ACH_BANK_NO,
              BRFILE.BR_WIMENU_CHECK_WRITING,
              BRFILE.BR_ADP_PAY_GROUP,
              BRFILE.BR_ALLOW_SCANNED_CKS,
              BRFILE.BR_CHECK_DEPOSIT_GL,
              BRFILE.BR_OPTION_PST_LASER,
              BRFILE.BR_USE_DEBIT_CARDS,
              BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
              BRFILE.BR_WKDAY_BANKID,
              BRFILE.BR_PURGE_NOW_FLAG,
              BRFILE.BR_EMAIL,
              BRFILE.BR_MANAGER_EMAIL)
            VALUES
             (:QBR-NO,
              NULLIF(:QBR-LTOUCH-DATE,'1900-01-01'),
              :QBR-NAME,
              :QBR-ADR1,
              :QBR-ADR2,
              :QBR-CITY,
              :QBR-STATE,
              :QBR-ZIP,
              :QBR-PHONE,
              :QBR-RETAIN,
              :QBR-NEXT-CHECK,
              :QBR-PRINTER-1,
              :QBR-PRINTER-2,
              :QBR-PRINTER-3,
              :QBR-PRINTER-4,
              :QBR-FEE-GL-BRNO,
              :QBR-REMIT1,
              :QBR-REMIT2,
              :QBR-REMIT3,
              :QBR-STMNT1,
              :QBR-STMNT2,
              :QBR-INS-PRODUCER-NUM,
              :QBR-DLRCHK,
              :QBR-FRONT-END-FG,
              :QBR-EMLNRG-PHONE,
              :QBR-POST-GL,
              :QBR-ALLOTMNTS,
              :QBR-EXTRACT-AUTO-UPD,
              :QBR-PCNI-LEDGER,
              :QBR-PRINTERFG-1,
              :QBR-PRINTERFG-2,
              :QBR-PRINTERFG-3,
              :QBR-PRINTERFG-4,
              :QBR-NEGDLRSV,
              :QBR-CPCORP-ORDER,
              :QBR-PRIO-MEMO-FLAG,
              :QBR-PREBILL-PL-JD-OPT,
              :QBR-LOAN-NUM-FLAG,
              :QBR-DOWNPY-FLAG,
              :QBR-AUTO-INS-EARN,
              :QBR-OTH-INS-LEVELS,
              :QBR-ALLOW-INTERBR,
              :QBR-UNER-DLANRP,
              :QBR-SECOND-PROMISE,
              :QBR-AVAIL-CREDIT,
              :QBR-RECP1,
              :QBR-RECP2,
              :QBR-INC-WAIVERS,
              :QBR-INC-INS,
              :QBR-BP-NEXT-BATCH,
              :QBR-BP-NOPAYOFF,
              :QBR-BP-ODDPAY,
              :QBR-BP-LC,
              :QBR-BP-NOPRIN,
              :QBR-BP-FLASH,
              :QBR-BP-ACTIONCD,
              :QBR-BP-MAXCON,
              :QBR-BP-MAXREC,
              :QBR-BP-PL,
              :QBR-BP-JD,
              :QBR-BP-BRANCH,
              :QBR-AGE-ERN,
              :QBR-OTHER-BRANCH-GL,
              :QBR-SALES-SMALL,
              :QBR-NEW-COLL-INQUIRY,
              :QBR-CHANGE-MEMO,
              :QBR-INS-ADDON,
              :QBR-TRAILER-REC,
              :QBR-ALDEL-FLAG,
              :QBR-IB-LCHG-MESS,
              :QBR-COL-QUEUE-PAST-DUE,
              :QBR-LI-DLCOMM-CMSI-PP-MAX,
              :QBR-ADDON-FEECD-O2-1,
              :QBR-ADDON-FEECD-O2-2,
              :QBR-AUTO-CHECK,
              :QBR-RECEIPT-PROMDATE,
              :QBR-RECEIPT-INTPD,
              :QBR-COLL-ID-SCAN,
              :QBR-STOP-ON-COLL,
              :QBR-DUEDATE-CHANGE-CD,
              :QBR-CHECK-EOM-FG,
              :QBR-VOLUME-DLANRP,
              :QBR-PDTH-NXDUE,
              :QBR-LC-CODE,
              :QBR-VALIDATE-PRUS,
              :QBR-AUTO-OPEN-BAL,
              :QBR-OPEN-BAL-1,
              :QBR-OPEN-BAL-2,
              :QBR-OPEN-BAL-3,
              :QBR-OPEN-BAL-4,
              :QBR-OPEN-BAL-5,
              :QBR-OPEN-BAL-6,
              :QBR-OPEN-BAL-7,
              :QBR-OPEN-BAL-8,
              :QBR-OPEN-BAL-9,
              :QBR-OPEN-BAL-10,
              :QBR-OPEN-BAL-11,
              :QBR-OPEN-BAL-12,
              :QBR-OPEN-BAL-13,
              :QBR-OPEN-BAL-14,
              :QBR-OPEN-BAL-15,
              :QBR-OPEN-BAL-16,
              :QBR-OPEN-BAL-17,
              :QBR-OPEN-BAL-18,
              :QBR-OPEN-BAL-19,
              :QBR-OPEN-BAL-20,
              :QBR-TENDER-CD-1,
              :QBR-TENDER-CD-2,
              :QBR-TENDER-CD-3,
              :QBR-DLPARV-ADJ-FG,
              :QBR-OVERRIDE-BR-STATE,
              :QBR-DEFAULT-SC-CLASS,
              :QBR-RECDEL-NOT-REC,
              :QBR-POST-AHEAD-FG,
              :QBR-CAN-LOANS-FG,
              :QBR-NON-MONTHLY-CAN-LOANS,
              :QBR-BP-ALLOW-PMT-ACCT-OTHBAL,
              :QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL,
              :QBR-REGION-NUMBER,
              :QBR-OUR-FEE,
              :QBR-REB-METHOD-1,
              :QBR-REB-METHOD-2,
              :QBR-REB-METHOD-3,
              :QBR-REB-METHOD-4,
              :QBR-REB-METHOD-5,
              :QBR-REB-METHOD-6,
              :QBR-REB-METHOD-7,
              :QBR-REB-METHOD-8,
              :QBR-REB-METHOD-9,
              :QBR-REB-METHOD-10,
              :QBR-REB-METHOD-11,
              :QBR-REBDAYS-MONTH-1,
              :QBR-REBDAYS-MONTH-2,
              :QBR-REBDAYS-MONTH-3,
              :QBR-REBDAYS-MONTH-4,
              :QBR-REBDAYS-MONTH-5,
              :QBR-REBDAYS-MONTH-6,
              :QBR-REBDAYS-MONTH-7,
              :QBR-REBDAYS-MONTH-8,
              :QBR-REBDAYS-MONTH-9,
              :QBR-REBDAYS-MONTH-10,
              :QBR-REBDAYS-MONTH-11,
              :QBR-ACCRUAL-MONTHS-1,
              :QBR-ACCRUAL-MONTHS-2,
              :QBR-ACCRUAL-MONTHS-3,
              :QBR-ACCRUAL-MONTHS-4,
              :QBR-ACCRUAL-MONTHS-5,
              :QBR-ACCRUAL-MONTHS-6,
              :QBR-ACCRUAL-MONTHS-7,
              :QBR-ACCRUAL-MONTHS-8,
              :QBR-ACCRUAL-MONTHS-9,
              :QBR-ACCRUAL-MONTHS-10,
              :QBR-ACCRUAL-MONTHS-11,
              :QBR-GLOVERSHORT,
              :QBR-GLBANK,
              :QBR-GLONHAND,
              :QBR-BANKBAL,
              :QBR-RETAIN-AMT,
              :QBR-LCARDFG,
              :QBR-DATA-PATH,
              :QBR-MACHINE,
              :QBR-MANAGER,
              :QBR-EOD-PROCEDURE,
              :QBR-BP-PAYOFF-NONCASH,
              :QBR-BP-PAYOFF-OV,
              :QBR-LI-DLCOMM-CMSI-O3-MAX,
              :QBR-LR-GROUP-COLLID,
              :QBR-UNITPER-FG,
              :QBR-UNITPER-Y-D-FG,
              :QBR-NO-PROMPT-FOR-OTHBR,
              :QBR-WORLD-BRANCH-TYPE,
              :QBR-COL-QUEUE-BEHAVIOR-FG,
              :QBR-CONVERT-AUTO-ACCTNO,
              :QBR-TS-REPO-INS-FG,
              :QBR-BOOK-SF-BR-ONLY,
              :QBR-FORCE-VALID-CITY-COUNTY,
              :QBR-SET-COLLCD-DL-MFGCD,
              :QBR-DEFAULT-SF-UNITPER-CD,
              :QBR-DEFAULT-SF-PURCD,
              :QBR-DEFAULT-LN-UNITPER-CD,
              :QBR-DEFAULT-LN-PURCD,
              :QBR-TAP-OTHER-DL,
              :QBR-AH-AND-CL,
              :QBR-NO-CASH-PROCEEDS,
              :QBR-RI-BEFORE-REPO,
              :QBR-RI-BEFORE-P4P5,
              :QBR-NOT-OURS-REFUND,
              :QBR-SFDLO3-WINDOW-FG,
              :QBR-CALC-INS-ADJ,
              :QBR-AC-BEFORE-REPO,
              :QBR-AC-BEFORE-P4P5,
              :QBR-BANK-ROUTING-NO,
              :QBR-BANK-CLEARING-GL,
              :QBR-GLBANK-DEPOSIT,
              :QBR-BANK-ACCTNO,
              :QBR-DEFAULT-SC-SUBCLASS,
              :QBR-NO-OUT-DLR-RSV,
              :QBR-COLLECTOR-QUEUE,
              :QBR-COLL-QUEUE-CON-REC,
              :QBR-JE-ALLOCATION-SKIP,
              :QBR-EXPAND-APP-INPUT,
              :QBR-A50-CPCORP,
              :QBR-INBR-BILNSC-FG,
              :QBR-INBR-BRIEF-FG,
              :QBR-LEDGER-JUST-INTCHG,
              :QBR-BYINQ-START-PAGE,
              :QBR-DSINQ-START-PAGE,
              :QBR-APP-REQUIRED-FOR-INPUT,
              :QBR-CREDIT-SCORING-FG,
              :QBR-COL-QUEUE-LOW-TO-HI,
              :QBR-CAN-GROUP-ID,
              :QBR-AUTO-LNSTAT-WINDOW,
              :QBR-TIME-ZONE-FACTOR,
              :QBR-LONPF-OPT-DISPLAY,
              :QBR-GIFTNET,
              :QBR-GIFTNET-LEDGER-LINE,
              :QBR-NO-FT-EMP,
              :QBR-NO-PT-EMP,
              :QBR-BALLOON-LOANS-FG,
              :QBR-GL-TYPE-ALLOWED,
              :QBR-GL-ENTRY-DEFAULT,
              :QBR-MIDSTREAM-1STPAY-FG,
              :QBR-NODEF-1ST-X-MONTHS,
              :QBR-ADD-INS-FEECODE,
              :QBR-ROAD-G-SGL-FEECODE,
              :QBR-ROAD-G-FAM-FEECODE,
              :QBR-NODEF-IF-ZERO-AMT,
              :QBR-ASSESS-LATE-CHGS,
              :QBR-NEXT-SOLICIT-NO,
              :QBR-ADDON-FEECD-O3-1,
              :QBR-ADDON-FEECD-O3-2,
              :QBR-IMMEDIATE-RECEIPT,
              :QBR-IMMEDIATE-DOC-PRINT,
              :QBR-AUTO-PL-REF-AC,
              :QBR-FAX-NUMBER,
              :QBR-ALPHA-DRAWERS,
              :QBR-LATECHG-ON-ALLOTMENTS,
              :QBR-ALLOW-SYS-AS-EFFDATE,
              :QBR-TAX-PREBOOK-CUST,
              :QBR-TAX-PREBOOK-NONCUST,
              :QBR-PASSWORD-DRAWER,
              :QBR-ONESTEP-RN-FG,
              :QBR-LI-LISCAN-FG,
              :QBR-HIDE-END-BAL,
              :QBR-LBOX-NOPAYOFF,
              :QBR-LBOX-ODDPAY,
              :QBR-LBOX-LC,
              :QBR-LBOX-NOPRIN,
              :QBR-LBOX-FLASH,
              :QBR-LBOX-ACTIONCD,
              :QBR-LBOX-MAXCON,
              :QBR-LBOX-MAXREC,
              :QBR-LBOX-PL,
              :QBR-LBOX-JD,
              :QBR-LBOX,
              :QBR-LATECHG-ON-LBOX,
              :QBR-ALLOW-OV-TRCD,
              :QBR-GLOBAL-QUEUE,
              :QBR-LBOX-PAYOFF-NONCASH,
              :QBR-LBOX-PAYOFF-OV,
              :QBR-COLLECT-MANAGER,
              :QBR-COLLECT-PHONE,
              :QBR-COLLECT-EXT,
              :QBR-ALT-INTO-THIS-BRANCH,
              :QBR-POFF-TITLE-WINDOW,
              :QBR-COUPON-STATEMENT,
              :QBR-SUPERVISOR,
              :QBR-AREA-MANAGER,
              :QBR-MIN-MONTHS-DEF,
              :QBR-CC-FLAG,
              :QBR-CC-ADR1,
              :QBR-CC-CITY,
              :QBR-CC-STATE,
              :QBR-CC-ZIP,
              :QBR-MIN-MONTHS-D2,
              :QBR-SKIP-MIDSTREAM-TRW,
              :QBR-GL-ESCROW-BALANCE,
              :QBR-GL-ESCROW-PAYABLE,
              :QBR-ALLOW-FORCE-OPEN-FG,
              :QBR-ACH-BANK-NO,
              :QBR-WIMENU-CHECK-WRITING,
              :QBR-ADP-PAY-GROUP,
              :QBR-ALLOW-SCANNED-CKS,
              :QBR-CHECK-DEPOSIT-GL,
              :QBR-OPTION-PST-LASER,
              :QBR-USE-DEBIT-CARDS,
              :QBR-GL-DEBIT-CARD-PROCEEDS,
              :QBR-WKDAY-BANKID,
              :QBR-PURGE-NOW-FLAG,
              :QBR-EMAIL,
              :QBR-MANAGER-EMAIL)
           END-EXEC.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BR-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-BR-FILE.

           ACCEPT BR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           PERFORM SET-BR-FIELDS.

           EXEC SQL
            UPDATE DBO.BRFILE
             SET
              BRFILE.BR_NO = :QBR-NO,
              BRFILE.BR_LTOUCH_DATE =
                 NULLIF(:QBR-LTOUCH-DATE,'1900-01-01'),
              BRFILE.BR_NAME = :QBR-NAME,
              BRFILE.BR_ADR1 = :QBR-ADR1,
              BRFILE.BR_ADR2 = :QBR-ADR2,
              BRFILE.BR_CITY = :QBR-CITY,
              BRFILE.BR_STATE = :QBR-STATE,
              BRFILE.BR_ZIP = :QBR-ZIP,
              BRFILE.BR_PHONE = :QBR-PHONE,
              BRFILE.BR_RETAIN = :QBR-RETAIN,
              BRFILE.BR_NEXT_CHECK = :QBR-NEXT-CHECK,
              BRFILE.BR_PRINTER_1 = :QBR-PRINTER-1,
              BRFILE.BR_PRINTER_2 = :QBR-PRINTER-2,
              BRFILE.BR_PRINTER_3 = :QBR-PRINTER-3,
              BRFILE.BR_PRINTER_4 = :QBR-PRINTER-4,
              BRFILE.BR_FEE_GL_BRNO = :QBR-FEE-GL-BRNO,
              BRFILE.BR_REMIT1 = :QBR-REMIT1,
              BRFILE.BR_REMIT2 = :QBR-REMIT2,
              BRFILE.BR_REMIT3 = :QBR-REMIT3,
              BRFILE.BR_STMNT1 = :QBR-STMNT1,
              BRFILE.BR_STMNT2 = :QBR-STMNT2,
              BRFILE.BR_INS_PRODUCER_NUM = :QBR-INS-PRODUCER-NUM,
              BRFILE.BR_DLRCHK = :QBR-DLRCHK,
              BRFILE.BR_FRONT_END_FG = :QBR-FRONT-END-FG,
              BRFILE.BR_EMLNRG_PHONE = :QBR-EMLNRG-PHONE,
              BRFILE.BR_POST_GL = :QBR-POST-GL,
              BRFILE.BR_ALLOTMNTS = :QBR-ALLOTMNTS,
              BRFILE.BR_EXTRACT_AUTO_UPD = :QBR-EXTRACT-AUTO-UPD,
              BRFILE.BR_PCNI_LEDGER = :QBR-PCNI-LEDGER,
              BRFILE.BR_PRINTERFG_1 = :QBR-PRINTERFG-1,
              BRFILE.BR_PRINTERFG_2 = :QBR-PRINTERFG-2,
              BRFILE.BR_PRINTERFG_3 = :QBR-PRINTERFG-3,
              BRFILE.BR_PRINTERFG_4 = :QBR-PRINTERFG-4,
              BRFILE.BR_NEGDLRSV = :QBR-NEGDLRSV,
              BRFILE.BR_CPCORP_ORDER = :QBR-CPCORP-ORDER,
              BRFILE.BR_PRIO_MEMO_FLAG = :QBR-PRIO-MEMO-FLAG,
              BRFILE.BR_PREBILL_PL_JD_OPT =
                   :QBR-PREBILL-PL-JD-OPT,
              BRFILE.BR_LOAN_NUM_FLAG = :QBR-LOAN-NUM-FLAG,
              BRFILE.BR_DOWNPY_FLAG = :QBR-DOWNPY-FLAG,
              BRFILE.BR_AUTO_INS_EARN = :QBR-AUTO-INS-EARN,
              BRFILE.BR_OTH_INS_LEVELS = :QBR-OTH-INS-LEVELS,
              BRFILE.BR_ALLOW_INTERBR = :QBR-ALLOW-INTERBR,
              BRFILE.BR_UNER_DLANRP = :QBR-UNER-DLANRP,
              BRFILE.BR_SECOND_PROMISE = :QBR-SECOND-PROMISE,
              BRFILE.BR_AVAIL_CREDIT = :QBR-AVAIL-CREDIT,
              BRFILE.BR_RECP1 = :QBR-RECP1,
              BRFILE.BR_RECP2 = :QBR-RECP2,
              BRFILE.BR_INC_WAIVERS = :QBR-INC-WAIVERS,
              BRFILE.BR_INC_INS = :QBR-INC-INS,
              BRFILE.BR_BP_NEXT_BATCH = :QBR-BP-NEXT-BATCH,
              BRFILE.BR_BP_NOPAYOFF = :QBR-BP-NOPAYOFF,
              BRFILE.BR_BP_ODDPAY = :QBR-BP-ODDPAY,
              BRFILE.BR_BP_LC = :QBR-BP-LC,
              BRFILE.BR_BP_NOPRIN = :QBR-BP-NOPRIN,
              BRFILE.BR_BP_FLASH = :QBR-BP-FLASH,
              BRFILE.BR_BP_ACTIONCD = :QBR-BP-ACTIONCD,
              BRFILE.BR_BP_MAXCON = :QBR-BP-MAXCON,
              BRFILE.BR_BP_MAXREC = :QBR-BP-MAXREC,
              BRFILE.BR_BP_PL = :QBR-BP-PL,
              BRFILE.BR_BP_JD = :QBR-BP-JD,
              BRFILE.BR_BP_BRANCH = :QBR-BP-BRANCH,
              BRFILE.BR_AGE_ERN = :QBR-AGE-ERN,
              BRFILE.BR_OTHER_BRANCH_GL = :QBR-OTHER-BRANCH-GL,
              BRFILE.BR_SALES_SMALL = :QBR-SALES-SMALL,
              BRFILE.BR_NEW_COLL_INQUIRY = :QBR-NEW-COLL-INQUIRY,
              BRFILE.BR_CHANGE_MEMO = :QBR-CHANGE-MEMO,
              BRFILE.BR_INS_ADDON = :QBR-INS-ADDON,
              BRFILE.BR_TRAILER_REC = :QBR-TRAILER-REC,
              BRFILE.BR_ALDEL_FLAG = :QBR-ALDEL-FLAG,
              BRFILE.BR_IB_LCHG_MESS = :QBR-IB-LCHG-MESS,
              BRFILE.BR_COL_QUEUE_PAST_DUE =
                   :QBR-COL-QUEUE-PAST-DUE,
              BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX =
                   :QBR-LI-DLCOMM-CMSI-PP-MAX,
              BRFILE.BR_ADDON_FEECD_O2_1 = :QBR-ADDON-FEECD-O2-1,
              BRFILE.BR_ADDON_FEECD_O2_2 = :QBR-ADDON-FEECD-O2-2,
              BRFILE.BR_AUTO_CHECK = :QBR-AUTO-CHECK,
              BRFILE.BR_RECEIPT_PROMDATE = :QBR-RECEIPT-PROMDATE,
              BRFILE.BR_RECEIPT_INTPD = :QBR-RECEIPT-INTPD,
              BRFILE.BR_COLL_ID_SCAN = :QBR-COLL-ID-SCAN,
              BRFILE.BR_STOP_ON_COLL = :QBR-STOP-ON-COLL,
              BRFILE.BR_DUEDATE_CHANGE_CD =
                   :QBR-DUEDATE-CHANGE-CD,
              BRFILE.BR_CHECK_EOM_FG = :QBR-CHECK-EOM-FG,
              BRFILE.BR_VOLUME_DLANRP = :QBR-VOLUME-DLANRP,
              BRFILE.BR_PDTH_NXDUE = :QBR-PDTH-NXDUE,
              BRFILE.BR_LC_CODE = :QBR-LC-CODE,
              BRFILE.BR_VALIDATE_PRUS = :QBR-VALIDATE-PRUS,
              BRFILE.BR_AUTO_OPEN_BAL = :QBR-AUTO-OPEN-BAL,
              BRFILE.BR_OPEN_BAL_1 = :QBR-OPEN-BAL-1,
              BRFILE.BR_OPEN_BAL_2 = :QBR-OPEN-BAL-2,
              BRFILE.BR_OPEN_BAL_3 = :QBR-OPEN-BAL-3,
              BRFILE.BR_OPEN_BAL_4 = :QBR-OPEN-BAL-4,
              BRFILE.BR_OPEN_BAL_5 = :QBR-OPEN-BAL-5,
              BRFILE.BR_OPEN_BAL_6 = :QBR-OPEN-BAL-6,
              BRFILE.BR_OPEN_BAL_7 = :QBR-OPEN-BAL-7,
              BRFILE.BR_OPEN_BAL_8 = :QBR-OPEN-BAL-8,
              BRFILE.BR_OPEN_BAL_9 = :QBR-OPEN-BAL-9,
              BRFILE.BR_OPEN_BAL_10 = :QBR-OPEN-BAL-10,
              BRFILE.BR_OPEN_BAL_11 = :QBR-OPEN-BAL-11,
              BRFILE.BR_OPEN_BAL_12 = :QBR-OPEN-BAL-12,
              BRFILE.BR_OPEN_BAL_13 = :QBR-OPEN-BAL-13,
              BRFILE.BR_OPEN_BAL_14 = :QBR-OPEN-BAL-14,
              BRFILE.BR_OPEN_BAL_15 = :QBR-OPEN-BAL-15,
              BRFILE.BR_OPEN_BAL_16 = :QBR-OPEN-BAL-16,
              BRFILE.BR_OPEN_BAL_17 = :QBR-OPEN-BAL-17,
              BRFILE.BR_OPEN_BAL_18 = :QBR-OPEN-BAL-18,
              BRFILE.BR_OPEN_BAL_19 = :QBR-OPEN-BAL-19,
              BRFILE.BR_OPEN_BAL_20 = :QBR-OPEN-BAL-20,
              BRFILE.BR_TENDER_CD_1 = :QBR-TENDER-CD-1,
              BRFILE.BR_TENDER_CD_2 = :QBR-TENDER-CD-2,
              BRFILE.BR_TENDER_CD_3 = :QBR-TENDER-CD-3,
              BRFILE.BR_DLPARV_ADJ_FG = :QBR-DLPARV-ADJ-FG,
              BRFILE.BR_OVERRIDE_BR_STATE =
                   :QBR-OVERRIDE-BR-STATE,
              BRFILE.BR_DEFAULT_SC_CLASS = :QBR-DEFAULT-SC-CLASS,
              BRFILE.BR_RECDEL_NOT_REC = :QBR-RECDEL-NOT-REC,
              BRFILE.BR_POST_AHEAD_FG = :QBR-POST-AHEAD-FG,
              BRFILE.BR_CAN_LOANS_FG = :QBR-CAN-LOANS-FG,
              BRFILE.BR_NON_MONTHLY_CAN_LOANS =
                   :QBR-NON-MONTHLY-CAN-LOANS,
              BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL =
                   :QBR-BP-ALLOW-PMT-ACCT-OTHBAL,
              BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL =
                   :QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL,
              BRFILE.BR_REGION_NUMBER = :QBR-REGION-NUMBER,
              BRFILE.BR_OUR_FEE = :QBR-OUR-FEE,
              BRFILE.BR_REB_METHOD_1 = :QBR-REB-METHOD-1,
              BRFILE.BR_REB_METHOD_2 = :QBR-REB-METHOD-2,
              BRFILE.BR_REB_METHOD_3 = :QBR-REB-METHOD-3,
              BRFILE.BR_REB_METHOD_4 = :QBR-REB-METHOD-4,
              BRFILE.BR_REB_METHOD_5 = :QBR-REB-METHOD-5,
              BRFILE.BR_REB_METHOD_6 = :QBR-REB-METHOD-6,
              BRFILE.BR_REB_METHOD_7 = :QBR-REB-METHOD-7,
              BRFILE.BR_REB_METHOD_8 = :QBR-REB-METHOD-8,
              BRFILE.BR_REB_METHOD_9 = :QBR-REB-METHOD-9,
              BRFILE.BR_REB_METHOD_10 = :QBR-REB-METHOD-10,
              BRFILE.BR_REB_METHOD_11 = :QBR-REB-METHOD-11,
              BRFILE.BR_REBDAYS_MONTH_1 = :QBR-REBDAYS-MONTH-1,
              BRFILE.BR_REBDAYS_MONTH_2 = :QBR-REBDAYS-MONTH-2,
              BRFILE.BR_REBDAYS_MONTH_3 = :QBR-REBDAYS-MONTH-3,
              BRFILE.BR_REBDAYS_MONTH_4 = :QBR-REBDAYS-MONTH-4,
              BRFILE.BR_REBDAYS_MONTH_5 = :QBR-REBDAYS-MONTH-5,
              BRFILE.BR_REBDAYS_MONTH_6 = :QBR-REBDAYS-MONTH-6,
              BRFILE.BR_REBDAYS_MONTH_7 = :QBR-REBDAYS-MONTH-7,
              BRFILE.BR_REBDAYS_MONTH_8 = :QBR-REBDAYS-MONTH-8,
              BRFILE.BR_REBDAYS_MONTH_9 = :QBR-REBDAYS-MONTH-9,
              BRFILE.BR_REBDAYS_MONTH_10 = :QBR-REBDAYS-MONTH-10,
              BRFILE.BR_REBDAYS_MONTH_11 = :QBR-REBDAYS-MONTH-11,
              BRFILE.BR_ACCRUAL_MONTHS_1 = :QBR-ACCRUAL-MONTHS-1,
              BRFILE.BR_ACCRUAL_MONTHS_2 = :QBR-ACCRUAL-MONTHS-2,
              BRFILE.BR_ACCRUAL_MONTHS_3 = :QBR-ACCRUAL-MONTHS-3,
              BRFILE.BR_ACCRUAL_MONTHS_4 = :QBR-ACCRUAL-MONTHS-4,
              BRFILE.BR_ACCRUAL_MONTHS_5 = :QBR-ACCRUAL-MONTHS-5,
              BRFILE.BR_ACCRUAL_MONTHS_6 = :QBR-ACCRUAL-MONTHS-6,
              BRFILE.BR_ACCRUAL_MONTHS_7 = :QBR-ACCRUAL-MONTHS-7,
              BRFILE.BR_ACCRUAL_MONTHS_8 = :QBR-ACCRUAL-MONTHS-8,
              BRFILE.BR_ACCRUAL_MONTHS_9 = :QBR-ACCRUAL-MONTHS-9,
              BRFILE.BR_ACCRUAL_MONTHS_10 =
                   :QBR-ACCRUAL-MONTHS-10,
              BRFILE.BR_ACCRUAL_MONTHS_11 =
                   :QBR-ACCRUAL-MONTHS-11,
              BRFILE.BR_GLOVERSHORT = :QBR-GLOVERSHORT,
              BRFILE.BR_GLBANK = :QBR-GLBANK,
              BRFILE.BR_GLONHAND = :QBR-GLONHAND,
              BRFILE.BR_BANKBAL = :QBR-BANKBAL,
              BRFILE.BR_RETAIN_AMT = :QBR-RETAIN-AMT,
              BRFILE.BR_LCARDFG = :QBR-LCARDFG,
              BRFILE.BR_DATA_PATH = :QBR-DATA-PATH,
              BRFILE.BR_MACHINE = :QBR-MACHINE,
              BRFILE.BR_MANAGER = :QBR-MANAGER,
              BRFILE.BR_EOD_PROCEDURE = :QBR-EOD-PROCEDURE,
              BRFILE.BR_BP_PAYOFF_NONCASH =
                   :QBR-BP-PAYOFF-NONCASH,
              BRFILE.BR_BP_PAYOFF_OV = :QBR-BP-PAYOFF-OV,
              BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX =
                   :QBR-LI-DLCOMM-CMSI-O3-MAX,
              BRFILE.BR_LR_GROUP_COLLID = :QBR-LR-GROUP-COLLID,
              BRFILE.BR_UNITPER_FG = :QBR-UNITPER-FG,
              BRFILE.BR_UNITPER_Y_D_FG = :QBR-UNITPER-Y-D-FG,
              BRFILE.BR_NO_PROMPT_FOR_OTHBR =
                   :QBR-NO-PROMPT-FOR-OTHBR,
              BRFILE.BR_WORLD_BRANCH_TYPE =
                   :QBR-WORLD-BRANCH-TYPE,
              BRFILE.BR_COL_QUEUE_BEHAVIOR_FG =
                   :QBR-COL-QUEUE-BEHAVIOR-FG,
              BRFILE.BR_CONVERT_AUTO_ACCTNO =
                   :QBR-CONVERT-AUTO-ACCTNO,
              BRFILE.BR_TS_REPO_INS_FG = :QBR-TS-REPO-INS-FG,
              BRFILE.BR_BOOK_SF_BR_ONLY = :QBR-BOOK-SF-BR-ONLY,
              BRFILE.BR_FORCE_VALID_CITY_COUNTY =
                   :QBR-FORCE-VALID-CITY-COUNTY,
              BRFILE.BR_SET_COLLCD_DL_MFGCD =
                   :QBR-SET-COLLCD-DL-MFGCD,
              BRFILE.BR_DEFAULT_SF_UNITPER_CD =
                   :QBR-DEFAULT-SF-UNITPER-CD,
              BRFILE.BR_DEFAULT_SF_PURCD = :QBR-DEFAULT-SF-PURCD,
              BRFILE.BR_DEFAULT_LN_UNITPER_CD =
                   :QBR-DEFAULT-LN-UNITPER-CD,
              BRFILE.BR_DEFAULT_LN_PURCD = :QBR-DEFAULT-LN-PURCD,
              BRFILE.BR_TAP_OTHER_DL = :QBR-TAP-OTHER-DL,
              BRFILE.BR_AH_AND_CL = :QBR-AH-AND-CL,
              BRFILE.BR_NO_CASH_PROCEEDS = :QBR-NO-CASH-PROCEEDS,
              BRFILE.BR_RI_BEFORE_REPO = :QBR-RI-BEFORE-REPO,
              BRFILE.BR_RI_BEFORE_P4P5 = :QBR-RI-BEFORE-P4P5,
              BRFILE.BR_NOT_OURS_REFUND = :QBR-NOT-OURS-REFUND,
              BRFILE.BR_SFDLO3_WINDOW_FG = :QBR-SFDLO3-WINDOW-FG,
              BRFILE.BR_CALC_INS_ADJ = :QBR-CALC-INS-ADJ,
              BRFILE.BR_AC_BEFORE_REPO = :QBR-AC-BEFORE-REPO,
              BRFILE.BR_AC_BEFORE_P4P5 = :QBR-AC-BEFORE-P4P5,
              BRFILE.BR_BANK_ROUTING_NO = :QBR-BANK-ROUTING-NO,
              BRFILE.BR_BANK_CLEARING_GL = :QBR-BANK-CLEARING-GL,
              BRFILE.BR_GLBANK_DEPOSIT = :QBR-GLBANK-DEPOSIT,
              BRFILE.BR_BANK_ACCTNO = :QBR-BANK-ACCTNO,
              BRFILE.BR_DEFAULT_SC_SUBCLASS =
                   :QBR-DEFAULT-SC-SUBCLASS,
              BRFILE.BR_NO_OUT_DLR_RSV = :QBR-NO-OUT-DLR-RSV,
              BRFILE.BR_COLLECTOR_QUEUE = :QBR-COLLECTOR-QUEUE,
              BRFILE.BR_COLL_QUEUE_CON_REC =
                   :QBR-COLL-QUEUE-CON-REC,
              BRFILE.BR_JE_ALLOCATION_SKIP =
                   :QBR-JE-ALLOCATION-SKIP,
              BRFILE.BR_EXPAND_APP_INPUT = :QBR-EXPAND-APP-INPUT,
              BRFILE.BR_A50_CPCORP = :QBR-A50-CPCORP,
              BRFILE.BR_INBR_BILNSC_FG = :QBR-INBR-BILNSC-FG,
              BRFILE.BR_INBR_BRIEF_FG = :QBR-INBR-BRIEF-FG,
              BRFILE.BR_LEDGER_JUST_INTCHG =
                   :QBR-LEDGER-JUST-INTCHG,
              BRFILE.BR_BYINQ_START_PAGE = :QBR-BYINQ-START-PAGE,
              BRFILE.BR_DSINQ_START_PAGE = :QBR-DSINQ-START-PAGE,
              BRFILE.BR_APP_REQUIRED_FOR_INPUT =
                   :QBR-APP-REQUIRED-FOR-INPUT,
              BRFILE.BR_CREDIT_SCORING_FG =
                   :QBR-CREDIT-SCORING-FG,
              BRFILE.BR_COL_QUEUE_LOW_TO_HI =
                   :QBR-COL-QUEUE-LOW-TO-HI,
              BRFILE.BR_CAN_GROUP_ID = :QBR-CAN-GROUP-ID,
              BRFILE.BR_AUTO_LNSTAT_WINDOW =
                   :QBR-AUTO-LNSTAT-WINDOW,
              BRFILE.BR_TIME_ZONE_FACTOR = :QBR-TIME-ZONE-FACTOR,
              BRFILE.BR_LONPF_OPT_DISPLAY =
                   :QBR-LONPF-OPT-DISPLAY,
              BRFILE.BR_GIFTNET = :QBR-GIFTNET,
              BRFILE.BR_GIFTNET_LEDGER_LINE =
                   :QBR-GIFTNET-LEDGER-LINE,
              BRFILE.BR_NO_FT_EMP = :QBR-NO-FT-EMP,
              BRFILE.BR_NO_PT_EMP = :QBR-NO-PT-EMP,
              BRFILE.BR_BALLOON_LOANS_FG = :QBR-BALLOON-LOANS-FG,
              BRFILE.BR_GL_TYPE_ALLOWED = :QBR-GL-TYPE-ALLOWED,
              BRFILE.BR_GL_ENTRY_DEFAULT = :QBR-GL-ENTRY-DEFAULT,
              BRFILE.BR_MIDSTREAM_1STPAY_FG =
                   :QBR-MIDSTREAM-1STPAY-FG,
              BRFILE.BR_NODEF_1ST_X_MONTHS =
                   :QBR-NODEF-1ST-X-MONTHS,
              BRFILE.BR_ADD_INS_FEECODE = :QBR-ADD-INS-FEECODE,
              BRFILE.BR_ROAD_G_SGL_FEECODE =
                   :QBR-ROAD-G-SGL-FEECODE,
              BRFILE.BR_ROAD_G_FAM_FEECODE =
                   :QBR-ROAD-G-FAM-FEECODE,
              BRFILE.BR_NODEF_IF_ZERO_AMT =
                   :QBR-NODEF-IF-ZERO-AMT,
              BRFILE.BR_ASSESS_LATE_CHGS = :QBR-ASSESS-LATE-CHGS,
              BRFILE.BR_NEXT_SOLICIT_NO = :QBR-NEXT-SOLICIT-NO,
              BRFILE.BR_ADDON_FEECD_O3_1 = :QBR-ADDON-FEECD-O3-1,
              BRFILE.BR_ADDON_FEECD_O3_2 = :QBR-ADDON-FEECD-O3-2,
              BRFILE.BR_IMMEDIATE_RECEIPT =
                   :QBR-IMMEDIATE-RECEIPT,
              BRFILE.BR_IMMEDIATE_DOC_PRINT =
                   :QBR-IMMEDIATE-DOC-PRINT,
              BRFILE.BR_AUTO_PL_REF_AC = :QBR-AUTO-PL-REF-AC,
              BRFILE.BR_FAX_NUMBER = :QBR-FAX-NUMBER,
              BRFILE.BR_ALPHA_DRAWERS = :QBR-ALPHA-DRAWERS,
              BRFILE.BR_LATECHG_ON_ALLOTMENTS =
                   :QBR-LATECHG-ON-ALLOTMENTS,
              BRFILE.BR_ALLOW_SYS_AS_EFFDATE =
                   :QBR-ALLOW-SYS-AS-EFFDATE,
              BRFILE.BR_TAX_PREBOOK_CUST = :QBR-TAX-PREBOOK-CUST,
              BRFILE.BR_TAX_PREBOOK_NONCUST =
                   :QBR-TAX-PREBOOK-NONCUST,
              BRFILE.BR_PASSWORD_DRAWER = :QBR-PASSWORD-DRAWER,
              BRFILE.BR_ONESTEP_RN_FG = :QBR-ONESTEP-RN-FG,
              BRFILE.BR_LI_LISCAN_FG = :QBR-LI-LISCAN-FG,
              BRFILE.BR_HIDE_END_BAL = :QBR-HIDE-END-BAL,
              BRFILE.BR_LBOX_NOPAYOFF = :QBR-LBOX-NOPAYOFF,
              BRFILE.BR_LBOX_ODDPAY = :QBR-LBOX-ODDPAY,
              BRFILE.BR_LBOX_LC = :QBR-LBOX-LC,
              BRFILE.BR_LBOX_NOPRIN = :QBR-LBOX-NOPRIN,
              BRFILE.BR_LBOX_FLASH = :QBR-LBOX-FLASH,
              BRFILE.BR_LBOX_ACTIONCD = :QBR-LBOX-ACTIONCD,
              BRFILE.BR_LBOX_MAXCON = :QBR-LBOX-MAXCON,
              BRFILE.BR_LBOX_MAXREC = :QBR-LBOX-MAXREC,
              BRFILE.BR_LBOX_PL = :QBR-LBOX-PL,
              BRFILE.BR_LBOX_JD = :QBR-LBOX-JD,
              BRFILE.BR_LBOX = :QBR-LBOX,
              BRFILE.BR_LATECHG_ON_LBOX = :QBR-LATECHG-ON-LBOX,
              BRFILE.BR_ALLOW_OV_TRCD = :QBR-ALLOW-OV-TRCD,
              BRFILE.BR_GLOBAL_QUEUE = :QBR-GLOBAL-QUEUE,
              BRFILE.BR_LBOX_PAYOFF_NONCASH =
                   :QBR-LBOX-PAYOFF-NONCASH,
              BRFILE.BR_LBOX_PAYOFF_OV = :QBR-LBOX-PAYOFF-OV,
              BRFILE.BR_COLLECT_MANAGER = :QBR-COLLECT-MANAGER,
              BRFILE.BR_COLLECT_PHONE = :QBR-COLLECT-PHONE,
              BRFILE.BR_COLLECT_EXT = :QBR-COLLECT-EXT,
              BRFILE.BR_ALT_INTO_THIS_BRANCH =
                   :QBR-ALT-INTO-THIS-BRANCH,
              BRFILE.BR_POFF_TITLE_WINDOW =
                   :QBR-POFF-TITLE-WINDOW,
              BRFILE.BR_COUPON_STATEMENT = :QBR-COUPON-STATEMENT,
              BRFILE.BR_SUPERVISOR = :QBR-SUPERVISOR,
              BRFILE.BR_AREA_MANAGER = :QBR-AREA-MANAGER,
              BRFILE.BR_MIN_MONTHS_DEF = :QBR-MIN-MONTHS-DEF,
              BRFILE.BR_CC_FLAG = :QBR-CC-FLAG,
              BRFILE.BR_CC_ADR1 = :QBR-CC-ADR1,
              BRFILE.BR_CC_CITY = :QBR-CC-CITY,
              BRFILE.BR_CC_STATE = :QBR-CC-STATE,
              BRFILE.BR_CC_ZIP = :QBR-CC-ZIP,
              BRFILE.BR_MIN_MONTHS_D2 = :QBR-MIN-MONTHS-D2,
              BRFILE.BR_SKIP_MIDSTREAM_TRW =
                   :QBR-SKIP-MIDSTREAM-TRW,
              BRFILE.BR_GL_ESCROW_BALANCE =
                   :QBR-GL-ESCROW-BALANCE,
              BRFILE.BR_GL_ESCROW_PAYABLE =
                   :QBR-GL-ESCROW-PAYABLE,
              BRFILE.BR_ALLOW_FORCE_OPEN_FG =
                   :QBR-ALLOW-FORCE-OPEN-FG,
              BRFILE.BR_ACH_BANK_NO = :QBR-ACH-BANK-NO,
              BRFILE.BR_WIMENU_CHECK_WRITING =
                   :QBR-WIMENU-CHECK-WRITING,
              BRFILE.BR_ADP_PAY_GROUP = :QBR-ADP-PAY-GROUP,
              BRFILE.BR_ALLOW_SCANNED_CKS =
                   :QBR-ALLOW-SCANNED-CKS,
              BRFILE.BR_CHECK_DEPOSIT_GL = :QBR-CHECK-DEPOSIT-GL,
              BRFILE.BR_OPTION_PST_LASER = :QBR-OPTION-PST-LASER,
              BRFILE.BR_USE_DEBIT_CARDS = :QBR-USE-DEBIT-CARDS,
              BRFILE.BR_GL_DEBIT_CARD_PROCEEDS =
                   :QBR-GL-DEBIT-CARD-PROCEEDS,
              BRFILE.BR_WKDAY_BANKID = :QBR-WKDAY-BANKID,
              BRFILE.BR_PURGE_NOW_FLAG = :QBR-PURGE-NOW-FLAG,
              BRFILE.BR_EMAIL = :QBR-EMAIL,
              BRFILE.BR_MANAGER_EMAIL = :QBR-MANAGER-EMAIL
             WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BR-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-BR-FILE.

           PERFORM DELETE-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           PERFORM SET-BR-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BRFILE
             WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BR-FILE.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.
 
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################

      *=================================================================
      *=================================================================
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOBRIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
