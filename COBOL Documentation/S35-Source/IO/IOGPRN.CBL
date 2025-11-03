       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGPRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-09-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGPRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GPFILE 
      * TABLE DESC    : GLOBAL PARAMETER FILE
      * OUTPUT FILE   : XPC/IOGPRN.PRE
      * USED FOR      : LIBGB/GBWSGP.CPY  --- QGB-REC
      *                 LIBGB/GBGPRN.CPY --- READ
      *
      *=================================================================
      * REV:
      *  BAH 2021-0309 ***NEW***
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
           "@(#)IO/IOGPRN.CBL S34 05/18/21-08:20:17 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GBWSGP.CPY".

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
       01  QGP-WHERE-BEG-KEY.
           03  QGP-WBEG-KEY            PIC 99.

       01  QGP-WHERE-END-KEY.
           03  QGP-WEND-KEY            PIC 99.

       01  QGP-REC.
           03  QGP-KEY                        PIC 99.
           03  QGP-LTOUCH-DATE                PIC X(10).
           03  QGP-BEG-FISCAL-DATE            PIC X(10).
           03  QGP-END-FISCAL-DATE-1          PIC X(10).
           03  QGP-END-FISCAL-DATE-2          PIC X(10).
           03  QGP-END-FISCAL-DATE-3          PIC X(10).
           03  QGP-END-FISCAL-DATE-4          PIC X(10).
           03  QGP-END-FISCAL-DATE-5          PIC X(10).
           03  QGP-END-FISCAL-DATE-6          PIC X(10).
           03  QGP-END-FISCAL-DATE-7          PIC X(10).
           03  QGP-END-FISCAL-DATE-8          PIC X(10).
           03  QGP-END-FISCAL-DATE-9          PIC X(10).
           03  QGP-END-FISCAL-DATE-10         PIC X(10).
           03  QGP-END-FISCAL-DATE-11         PIC X(10).
           03  QGP-END-FISCAL-DATE-12         PIC X(10).
           03  QGP-BANK-GL                    PIC 9(11).
           03  QGP-BANK-CLEARING-GL           PIC 9(11).
           03  QGP-BANK-ROUTING-NO            PIC 9(9).
           03  QGP-BANK-ACCTNO                PIC 9(15).
           03  QGP-TR-RETAIN-MONTHS           PIC 99.
           03  QGP-FED-TAX-ID                 PIC 9(9).
           03  QGP-CORP-NAME                  PIC X(25).
           03  QGP-SHORT-NAME                 PIC X(3).
           03  QGP-BANK-NAME                  PIC X(23).
           03  QGP-BANK-DEPOSIT-GL            PIC 9(11).
           03  QGP-AGE-BUCKET-CD              PIC X.
           03  QGP-THE-ONLY-SF-SRCD           PIC XX.
           03  QGP-ALLOW-BATCH-CHECKS         PIC X.
           03  QGP-DLRVPRIN-3-FG              PIC X.
           03  QGP-DLRVCHGS-3-FG              PIC X.
           03  QGP-CASH-METHOD-ONLY           PIC X.
           03  QGP-IB-ACCRUALS-DONE           PIC X.
           03  QGP-EOD-RETAIN-MONTHS          PIC 99.
           03  QGP-NO-LC-IN-ARREARAGE-FG      PIC X.
           03  QGP-DEFAULT-O3-INS-COMPANY     PIC 99.
           03  QGP-STOP-ON-PRIOR-FG           PIC X.
           03  QGP-APPLICATION-FG             PIC X.
           03  QGP-DEFAULT-ACCRATE            PIC 999V999.
           03  QGP-DLR-DISP-TYPE              PIC X.
           03  QGP-ALLOW-RLOGINS              PIC X.
           03  QGP-AUTO-REPO-WINDOW           PIC X.
           03  QGP-ALLOW-PI-WITH-SE-BAL       PIC X.
           03  QGP-ALLOW-MORE-COLLECT         PIC X.
           03  QGP-BATCH-MORE-THAN-DAY        PIC X.
           03  QGP-PRINT-GL-RECAP             PIC X.
           03  QGP-LOAN-CONVERSION-UPD-FG     PIC X.
           03  QGP-FORM-CHG-CD                PIC X.
           03  QGP-PROGRP-PRINT-REDEEM        PIC X.
           03  QGP-BW-HOUSING-CODE            PIC X.
           03  QGP-BW-CREDIT-SCORE            PIC X.
           03  QGP-DISPLAY-BANK-BAL           PIC X.
           03  QGP-PROGRP-DL-CONT             PIC X.
           03  QGP-DLRVPRIN-2-FG              PIC X.
           03  QGP-DLRVPRIN-1-FG              PIC X.
           03  QGP-ADV-TO-1ST-NO-CORR         PIC X.
           03  QGP-COMPANY-FLAG               PIC X(4).
           03  QGP-INTERBR-AR-AP-FULLGL       PIC X.
           03  QGP-SKIP-BI-UPDATE             PIC X.
           03  QGP-ASK-HARD-COPY-MEMOS        PIC X.
           03  QGP-RLOGIN-LOCAL-PROFILE       PIC X.
           03  QGP-RLOGIN-REMOTE-PROFILE      PIC X.
           03  QGP-REPODATE-PAYDATE           PIC X.
           03  QGP-POST-INTO-FUTURE           PIC X.
           03  QGP-RENEWAL-COLLATERAL         PIC X.
           03  QGP-PROGRP-LN-CLASS-1          PIC 9(3).
           03  QGP-PROGRP-LN-CLASS-2          PIC 9(3).
           03  QGP-PROGRP-LN-CLASS-3          PIC 9(3).
           03  QGP-PROGRP-SF-CLASS-1          PIC 9(3).
           03  QGP-PROGRP-SF-CLASS-2          PIC 9(3).
           03  QGP-PROGRP-SF-CLASS-3          PIC 9(3).
           03  QGP-PROGRP-SF-CLASS-4          PIC 9(3).
           03  QGP-RENEW-DISC-UNERN           PIC X.
           03  QGP-NOTES-EOD-BATCH-ID         PIC X(3).
           03  QGP-NEXT-NOTE                  PIC 9(6).
           03  QGP-NEXT-BATCH-ID              PIC 9(5).
           03  QGP-REFNO-VERIFY               PIC X.
           03  QGP-ACTIONCD-VERIFY            PIC X.
           03  QGP-BP-NEXT-BATCH              PIC 9(3).
           03  QGP-IB-ACCRUE-EARN-ON-REPO     PIC X.
           03  QGP-DELINQ-WITH-O2             PIC X.
           03  QGP-ALLOW-REPO-ON-PL           PIC X.
           03  QGP-BK-PRIOR-TO-POSTING        PIC X.
           03  QGP-AMT-CASH-PROCEEDS          PIC 9(6)V99.
           03  QGP-INCL-OTH-IN-EARNED         PIC X.
           03  QGP-VALID-BDATE                PIC X.
           03  QGP-ARCHIVE-DIRECTORY          PIC X(4).
           03  QGP-DL-MEMO-TOGGLE             PIC X.
           03  QGP-RN-FOR10-LN                PIC X.
           03  QGP-LI-APPLICATION-FG          PIC X.
           03  QGP-OT-INS-TERM                PIC X.
           03  QGP-UNEARNED-STATS             PIC X.
           03  QGP-PDTH-GT-SCHED-PAY          PIC X.
           03  QGP-LB-NEXT-BATCH              PIC 9(3).
           03  QGP-RECDEL-REVERSAL-FG         PIC X.
           03  QGP-REF-PHONE                  PIC X.
           03  QGP-ALLOW-PL-ON-SX             PIC X.
           03  QGP-USE-CONRATE                PIC X.
           03  QGP-CPCORP-CURBAL              PIC X.
           03  QGP-TIME-ZONE-Q                PIC X.
           03  QGP-TIME-ZONE-FACTOR           PIC S9(2).
           03  QGP-TIME-ZONE-DLS              PIC X.
           03  QGP-CALL-START-TIME            PIC 9(4).
           03  QGP-CALL-END-TIME              PIC 9(4).
           03  QGP-RV-MEMO                    PIC X.
           03  QGP-PENALTY-INT                PIC X.
           03  QGP-ALLOW-SET-FOR-NONCASH      PIC X.
           03  QGP-IGNORE-LCHG-NXDUE          PIC X.
           03  QGP-ALLOW-ENTRY-TRADEIN        PIC X.
           03  QGP-TRUE-NEXT-BATCH            PIC 9(3).
           03  QGP-REPO-CODE                  PIC X.
           03  QGP-ALLOW-MORTGAGE-INT-Z       PIC X.
           03  QGP-PROM-AMT-MIN-PCT           PIC 9(3).
           03  QGP-BW-PRIVACY-OPT-OUT         PIC X.
           03  QGP-NO-GL-ON-O2-WHEN-PL        PIC X.
           03  QGP-INTERIM-COUPONS            PIC X.
           03  QGP-MGRAM-NEXT-BATCH           PIC 9(3).
           03  QGP-RESCHEDULE-MEMO            PIC X.
           03  QGP-DONT-COLL-INT-ON-SS        PIC X.
           03  QGP-DL-ACH-FG                  PIC X.
           03  QGP-GL-NEW-JE                  PIC X.
           03  QGP-CLEAR-PROM-PDTH            PIC X.
           03  QGP-CREATE-TP-FORMS            PIC X.
           03  QGP-CHECK-RECONCILIATION       PIC X.
           03  QGP-CORP-BRNO                  PIC 9(4).
           03  QGP-PL-REPO-REASON             PIC X.
           03  QGP-ALLOW-MIP-INSURANCE        PIC X.
           03  QGP-ENTER-LPO-FG               PIC X.
           03  QGP-DLCOMM-CMSI-FG             PIC X.
           03  QGP-REFCD-NEXT-BATCH           PIC 9(3).
           03  QGP-EARLIEST-CLOSE-TIME        PIC 9(4).
           03  QGP-GROUP-APPROVAL-FG          PIC X.
           03  QGP-APPROVAL-MEMO              PIC X.
           03  QGP-FORCE-INPUT-PP-COVERAGE    PIC X.
           03  QGP-NO-SC-USE-DM               PIC X.
           03  QGP-DEFAULT-O3-INSOURS         PIC X.
           03  QGP-USE-BW-BAD-FLAGS           PIC X.
           03  QGP-NON-FILE-FEECODE           PIC XX.
           03  QGP-CHBORR-PAST-DUE-DAYS       PIC 99.
           03  QGP-CLOSED-FISCAL-FG-1         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-2         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-3         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-4         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-5         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-6         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-7         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-8         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-9         PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-10        PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-11        PIC X(1).
           03  QGP-CLOSED-FISCAL-FG-12        PIC X(1).
           03  QGP-BACKDATE-MEMO              PIC X.
           03  QGP-OTREF-NEXT-BATCH           PIC 9(3).
           03  QGP-NEW-REPO-UNEARNED-ACCTS    PIC X.
           03  QGP-BK-FLAG-CONFIDENTIAL       PIC X.
           03  QGP-AMORTIZE-ACQUIST-FEE       PIC X.
           03  QGP-DENY-FORM-NO-ADDRESS       PIC X.
           03  QGP-HIDE-FIRST-5-SSNO          PIC X.
           03  QGP-DEFERMENT-APPROVAL         PIC X.
           03  QGP-INCLUDE-O2-IN-BAL          PIC X.
           03  QGP-LOAN-ACH-PROCESSING        PIC X.
           03  QGP-PAYEE-PROCESSING-FG        PIC X.
           03  QGP-AUTO-BANKRUPT-WINDOW       PIC X.
           03  QGP-BW-SCORES-QUARTER-SUB      PIC 9.
           03  QGP-POST-XX-BF-PAYMENTS        PIC X.
           03  QGP-VALIDATE-ENDORSER-AGE      PIC X.
           03  QGP-ALLOW-MANUAL-CHECKS        PIC X.
           03  QGP-EXPANDED-FOLLOW-UP         PIC X.
           03  QGP-NEXT-EMPLOYEE-NO           PIC 9(6).
           03  QGP-USE-RESULT-WORKED-FG       PIC X.
           03  QGP-MAX-PROMISE-DAYS           PIC 9(2).
           03  QGP-PURGE-TO-ARCHIVE-FG        PIC X.
           03  QGP-EDIT-BORROWER-NAME-FG      PIC X.
           03  QGP-RECAP-CLASS-TOTALS         PIC X.
           03  QGP-BRING-AUTO-INS-ON-RENEWAL  PIC X.
           03  QGP-ALLOW-MF-ON-IB             PIC X.
           03  QGP-USE-MIN-FORM-SENT-DAYS     PIC X.
           03  QGP-DISPLAY-BAD-ADR-FLAG       PIC X.
           03  QGP-DAYS-IDENT-NEW-PAPER       PIC 9(3).
           03  QGP-ESCROW-FG                  PIC X.
           03  QGP-SECURITY-FG                PIC X.
           03  QGP-GPS-INFORMATION            PIC X.
           03  QGP-CUST-INFO-FG               PIC X.
           03  QGP-USE-ADDRESS-BW-SCAN        PIC X.
           03  QGP-EXPANDED-LEGAL-WINDOW      PIC X.
           03  QGP-USE-SCRA                   PIC X.
           03  QGP-USE-ACCRUAL-EARN-FILE      PIC X.
           03  QGP-MESSAGE-ATTR               PIC X.
           03  QGP-JUDGMENT-WINDOW         PIC X.
           03  QGP-DUE-29-30-31            PIC X.
           03  QGP-DEFAULT-SKIP-TRW        PIC X.
           03  QGP-EARLIEST-OPEN-TIME      PIC 9(4).
           03  QGP-TRANSFER-CONPROFILE     PIC X.
           03  QGP-BATCH-REFCD-DUP-DAYS    PIC 9(3).

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

       01  LEGEND-BEG-KEY. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING KEY      (F3-ALL) ".

       01  LEGEND-END-KEY. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING KEY      ".

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
           03  BEG-KEY                 PIC 9(02).
           03  END-KEY                 PIC 9(02).

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
           03  FILLER PIC X(32) VALUE "KEY  FISCAL DTE      BANK GL".
           03  FILLER PIC X(34) VALUE "TAX ID  CORP NAME".
           03  FILLER PIC X(30) VALUE "COMP   OPEN".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GP-KEY                     PIC 9.
           03  FILLER                         PIC X(04).
           03  D01-GP-BEG-FISCAL-DATE         PIC 9999/99/99.
           03  FILLER                         PIC X(02).
           03  D01-GP-BANK-GL                 PIC 9(11).
           03  FILLER                         PIC X(01).
           03  D01-GP-FED-TAX-ID              PIC 9(9).
           03  FILLER                         PIC X(02).
           03  D01-GP-CORP-NAME               PIC X(25).
           03  FILLER                         PIC X(01).
           03  D01-GP-COMPANY-FLAG            PIC X(4).
           03  FILLER                         PIC X(01).
           03  D01-GP-ALLOW-MANUAL-CHECKS     PIC X.
           03  FILLER                         PIC X(01).
           03  D01-GP-EARLIEST-OPEN-TIME      PIC 9(4).

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
      *COPY "LIBGB/GPENVW.CPY".
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
       COPY "LIBIO/IOGPRN_DEF.CPY".
       COPY "LIBIO/IOGPRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGPRN_SCN.CPY".

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
           MOVE "B"                        TO GP-PATH-OVERRIDE.

           MOVE BEG-KEY                    TO QGP-WBEG-KEY.

           PERFORM OPEN-GP-FILE.

           MOVE 0 TO GP-KEY.
       MAIN-PROGRAM-NEXT.

           ADD 1 TO GP-KEY.

           ADD 01 TO RECORDS-READ.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *    IF ( PROCESS-KEY = 1 )
      *       PERFORM READ-%%1-FILE-NEXT
      *    ELSE
      *       PERFORM READ-%%#-FILE-NEXT.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           PERFORM READ-GP-FILE.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GP-KEY < BEG-KEY )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( GP-KEY > END-KEY )
              GO TO MAIN-PROGRAM-END.

           MOVE GP-KEY                     TO D01-GP-KEY.
           MOVE GP-BEG-FISCAL-DATE         TO D01-GP-BEG-FISCAL-DATE.
           MOVE GP-BANK-GL                 TO D01-GP-BANK-GL.
           MOVE GP-FED-TAX-ID              TO D01-GP-FED-TAX-ID.
           MOVE GP-CORP-NAME               TO D01-GP-CORP-NAME.
           MOVE GP-COMPANY-FLAG            TO D01-GP-COMPANY-FLAG.
           MOVE GP-ALLOW-MANUAL-CHECKS     TO D01-GP-ALLOW-MANUAL-CHECKS.
           MOVE GP-EARLIEST-OPEN-TIME      TO D01-GP-EARLIEST-OPEN-TIME.
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
           MOVE "IOGPRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGPRN-QUEUE-POS  TO Q-POS.
           MOVE IOGPRN-COPIES-POS TO C-POS.
           MOVE IOGPRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-GROUP.

           MOVE LEGEND-BEG-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N020BEG-GROUP." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-GROUP.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-GROUP.

           MOVE VDUNUM0 TO BEG-KEY.

      *-----------------------------------------------------------------
       ENTER-END-GROUP.

           MOVE LEGEND-END-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N020END-GROUP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-GROUP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-KEY.

           IF ( BEG-KEY > END-KEY )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-GROUP.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-GROUP.

           MOVE "SN N020BEG-GROUP." TO VDU.
           MOVE 0                   TO BEG-KEY VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-GROUP." TO VDU.
           MOVE 99                  TO END-KEY VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-GROUP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )      *> %%%%%% SET FOR MAX # OF KEYS
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

           MOVE "S  N000BEG-GROUP."    TO VDU.
           MOVE BEG-KEY                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N000END-GROUP."    TO VDU.
           MOVE END-KEY                TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
           PERFORM SCREENIO.

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

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *    IF ( PROCESS-KEY = 1 )
      *       MOVE %%1-KEY TO PACE-DETAIL-KEY
      *    ELSE
      *    IF ( PROCESS-KEY = 2 )
      *       MOVE %%2-KEY TO PACE-DETAIL-KEY
      *    ELSE
      *       MOVE %%3-KEY TO PACE-DETAIL-KEY.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

           MOVE GP-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING KEY                 : "    TO R01-PROMPT.
           MOVE BEG-KEY                               TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    KEY                 : "    TO R01-PROMPT.
           MOVE END-KEY                               TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - GPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOGPRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGPRN-SYSDATE.
           DISPLAY IOGPRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGPRN-FORM-FIELDS TO WR-BUF.
           MOVE IOGPRN-FORM-LIST TO WR-LIST.
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

           MOVE IOGPRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGPRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOGPRN_EVA.CPY".
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
      *    LOAD GP-REC FROM QGP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GP-FIELDS.
           MOVE QGP-KEY TO GP-KEY.

           IF ( QGP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GP-LTOUCH-DATE
           ELSE
              MOVE QGP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-LTOUCH-DATE.


           IF ( QGP-BEG-FISCAL-DATE = "1900-01-01" )
              MOVE ZEROES TO GP-BEG-FISCAL-DATE
           ELSE
              MOVE QGP-BEG-FISCAL-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-BEG-FISCAL-DATE.


           IF ( QGP-END-FISCAL-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(1)
           ELSE
              MOVE QGP-END-FISCAL-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(1).


           IF ( QGP-END-FISCAL-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(2)
           ELSE
              MOVE QGP-END-FISCAL-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(2).


           IF ( QGP-END-FISCAL-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(3)
           ELSE
              MOVE QGP-END-FISCAL-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(3).


           IF ( QGP-END-FISCAL-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(4)
           ELSE
              MOVE QGP-END-FISCAL-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(4).


           IF ( QGP-END-FISCAL-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(5)
           ELSE
              MOVE QGP-END-FISCAL-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(5).


           IF ( QGP-END-FISCAL-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(6)
           ELSE
              MOVE QGP-END-FISCAL-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(6).


           IF ( QGP-END-FISCAL-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(7)
           ELSE
              MOVE QGP-END-FISCAL-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(7).


           IF ( QGP-END-FISCAL-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(8)
           ELSE
              MOVE QGP-END-FISCAL-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(8).


           IF ( QGP-END-FISCAL-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(9)
           ELSE
              MOVE QGP-END-FISCAL-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(9).


           IF ( QGP-END-FISCAL-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(10)
           ELSE
              MOVE QGP-END-FISCAL-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(10).


           IF ( QGP-END-FISCAL-DATE-11 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(11)
           ELSE
              MOVE QGP-END-FISCAL-DATE-11 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(11).


           IF ( QGP-END-FISCAL-DATE-12 = "1900-01-01" )
              MOVE ZEROES TO GP-END-FISCAL-DATE(12)
           ELSE
              MOVE QGP-END-FISCAL-DATE-12 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(12).

           MOVE QGP-BANK-GL TO GP-BANK-GL.
           MOVE QGP-BANK-CLEARING-GL TO GP-BANK-CLEARING-GL.
           MOVE QGP-BANK-ROUTING-NO TO GP-BANK-ROUTING-NO.
           MOVE QGP-BANK-ACCTNO TO GP-BANK-ACCTNO.
           MOVE QGP-TR-RETAIN-MONTHS TO GP-TR-RETAIN-MONTHS.
           MOVE QGP-FED-TAX-ID TO GP-FED-TAX-ID.
           MOVE QGP-CORP-NAME TO GP-CORP-NAME.
           MOVE QGP-SHORT-NAME TO GP-SHORT-NAME.
           MOVE QGP-BANK-NAME TO GP-BANK-NAME.
           MOVE QGP-BANK-DEPOSIT-GL TO GP-BANK-DEPOSIT-GL.
           MOVE QGP-AGE-BUCKET-CD TO GP-AGE-BUCKET-CD.
           MOVE QGP-THE-ONLY-SF-SRCD TO GP-THE-ONLY-SF-SRCD.
           MOVE QGP-ALLOW-BATCH-CHECKS TO GP-ALLOW-BATCH-CHECKS.
           MOVE QGP-DLRVPRIN-3-FG TO GP-DLRVPRIN-3-FG.
           MOVE QGP-DLRVCHGS-3-FG TO GP-DLRVCHGS-3-FG.
           MOVE QGP-CASH-METHOD-ONLY TO GP-CASH-METHOD-ONLY.
           MOVE QGP-IB-ACCRUALS-DONE TO GP-IB-ACCRUALS-DONE.
           MOVE QGP-EOD-RETAIN-MONTHS TO GP-EOD-RETAIN-MONTHS.
           MOVE QGP-NO-LC-IN-ARREARAGE-FG TO GP-NO-LC-IN-ARREARAGE-FG.
           MOVE QGP-DEFAULT-O3-INS-COMPANY
              TO GP-DEFAULT-O3-INS-COMPANY.
           MOVE QGP-STOP-ON-PRIOR-FG TO GP-STOP-ON-PRIOR-FG.
           MOVE QGP-APPLICATION-FG TO GP-APPLICATION-FG.
           MOVE QGP-DEFAULT-ACCRATE TO GP-DEFAULT-ACCRATE.
           MOVE QGP-DLR-DISP-TYPE TO GP-DLR-DISP-TYPE.
           MOVE QGP-ALLOW-RLOGINS TO GP-ALLOW-RLOGINS.
           MOVE QGP-AUTO-REPO-WINDOW TO GP-AUTO-REPO-WINDOW.
           MOVE QGP-ALLOW-PI-WITH-SE-BAL TO GP-ALLOW-PI-WITH-SE-BAL.
           MOVE QGP-ALLOW-MORE-COLLECT TO GP-ALLOW-MORE-COLLECT.
           MOVE QGP-BATCH-MORE-THAN-DAY TO GP-BATCH-MORE-THAN-DAY.
           MOVE QGP-PRINT-GL-RECAP TO GP-PRINT-GL-RECAP.
           MOVE QGP-LOAN-CONVERSION-UPD-FG
              TO GP-LOAN-CONVERSION-UPD-FG.
           MOVE QGP-FORM-CHG-CD TO GP-FORM-CHG-CD.
           MOVE QGP-PROGRP-PRINT-REDEEM TO GP-PROGRP-PRINT-REDEEM.
           MOVE QGP-BW-HOUSING-CODE TO GP-BW-HOUSING-CODE.
           MOVE QGP-BW-CREDIT-SCORE TO GP-BW-CREDIT-SCORE.
           MOVE QGP-DISPLAY-BANK-BAL TO GP-DISPLAY-BANK-BAL.
           MOVE QGP-PROGRP-DL-CONT TO GP-PROGRP-DL-CONT.
           MOVE QGP-DLRVPRIN-2-FG TO GP-DLRVPRIN-2-FG.
           MOVE QGP-DLRVPRIN-1-FG TO GP-DLRVPRIN-1-FG.
           MOVE QGP-ADV-TO-1ST-NO-CORR TO GP-ADV-TO-1ST-NO-CORR.
           MOVE QGP-COMPANY-FLAG TO GP-COMPANY-FLAG.
           MOVE QGP-INTERBR-AR-AP-FULLGL TO GP-INTERBR-AR-AP-FULLGL.
           MOVE QGP-SKIP-BI-UPDATE TO GP-SKIP-BI-UPDATE.
           MOVE QGP-ASK-HARD-COPY-MEMOS TO GP-ASK-HARD-COPY-MEMOS.
           MOVE QGP-RLOGIN-LOCAL-PROFILE TO GP-RLOGIN-LOCAL-PROFILE.
           MOVE QGP-RLOGIN-REMOTE-PROFILE TO GP-RLOGIN-REMOTE-PROFILE.
           MOVE QGP-REPODATE-PAYDATE TO GP-REPODATE-PAYDATE.
           MOVE QGP-POST-INTO-FUTURE TO GP-POST-INTO-FUTURE.
           MOVE QGP-RENEWAL-COLLATERAL TO GP-RENEWAL-COLLATERAL.
           MOVE QGP-PROGRP-LN-CLASS-1 TO GP-PROGRP-LN-CLASS(1).
           MOVE QGP-PROGRP-LN-CLASS-2 TO GP-PROGRP-LN-CLASS(2).
           MOVE QGP-PROGRP-LN-CLASS-3 TO GP-PROGRP-LN-CLASS(3).
           MOVE QGP-PROGRP-SF-CLASS-1 TO GP-PROGRP-SF-CLASS(1).
           MOVE QGP-PROGRP-SF-CLASS-2 TO GP-PROGRP-SF-CLASS(2).
           MOVE QGP-PROGRP-SF-CLASS-3 TO GP-PROGRP-SF-CLASS(3).
           MOVE QGP-PROGRP-SF-CLASS-4 TO GP-PROGRP-SF-CLASS(4).
           MOVE QGP-RENEW-DISC-UNERN TO GP-RENEW-DISC-UNERN.
           MOVE QGP-NOTES-EOD-BATCH-ID TO GP-NOTES-EOD-BATCH-ID.
           MOVE QGP-NEXT-NOTE TO GP-NEXT-NOTE.
           MOVE QGP-NEXT-BATCH-ID TO GP-NEXT-BATCH-ID.
           MOVE QGP-REFNO-VERIFY TO GP-REFNO-VERIFY.
           MOVE QGP-ACTIONCD-VERIFY TO GP-ACTIONCD-VERIFY.
           MOVE QGP-BP-NEXT-BATCH TO GP-BP-NEXT-BATCH.
           MOVE QGP-IB-ACCRUE-EARN-ON-REPO
              TO GP-IB-ACCRUE-EARN-ON-REPO.
           MOVE QGP-DELINQ-WITH-O2 TO GP-DELINQ-WITH-O2.
           MOVE QGP-ALLOW-REPO-ON-PL TO GP-ALLOW-REPO-ON-PL.
           MOVE QGP-BK-PRIOR-TO-POSTING TO GP-BK-PRIOR-TO-POSTING.
           MOVE QGP-AMT-CASH-PROCEEDS TO GP-AMT-CASH-PROCEEDS.
           MOVE QGP-INCL-OTH-IN-EARNED TO GP-INCL-OTH-IN-EARNED.
           MOVE QGP-VALID-BDATE TO GP-VALID-BDATE.
           MOVE QGP-ARCHIVE-DIRECTORY TO GP-ARCHIVE-DIRECTORY.
           MOVE QGP-DL-MEMO-TOGGLE TO GP-DL-MEMO-TOGGLE.
           MOVE QGP-RN-FOR10-LN TO GP-RN-FOR10-LN.
           MOVE QGP-LI-APPLICATION-FG TO GP-LI-APPLICATION-FG.
           MOVE QGP-OT-INS-TERM TO GP-OT-INS-TERM.
           MOVE QGP-UNEARNED-STATS TO GP-UNEARNED-STATS.
           MOVE QGP-PDTH-GT-SCHED-PAY TO GP-PDTH-GT-SCHED-PAY.
           MOVE QGP-LB-NEXT-BATCH TO GP-LB-NEXT-BATCH.
           MOVE QGP-RECDEL-REVERSAL-FG TO GP-RECDEL-REVERSAL-FG.
           MOVE QGP-REF-PHONE TO GP-REF-PHONE.
           MOVE QGP-ALLOW-PL-ON-SX TO GP-ALLOW-PL-ON-SX.
           MOVE QGP-USE-CONRATE TO GP-USE-CONRATE.
           MOVE QGP-CPCORP-CURBAL TO GP-CPCORP-CURBAL.
           MOVE QGP-TIME-ZONE-Q TO GP-TIME-ZONE-Q.
           MOVE QGP-TIME-ZONE-FACTOR TO GP-TIME-ZONE-FACTOR.
           MOVE QGP-TIME-ZONE-DLS TO GP-TIME-ZONE-DLS.
           MOVE QGP-CALL-START-TIME TO GP-CALL-START-TIME.
           MOVE QGP-CALL-END-TIME TO GP-CALL-END-TIME.
           MOVE QGP-RV-MEMO TO GP-RV-MEMO.
           MOVE QGP-PENALTY-INT TO GP-PENALTY-INT.
           MOVE QGP-ALLOW-SET-FOR-NONCASH TO GP-ALLOW-SET-FOR-NONCASH.
           MOVE QGP-IGNORE-LCHG-NXDUE TO GP-IGNORE-LCHG-NXDUE.
           MOVE QGP-ALLOW-ENTRY-TRADEIN TO GP-ALLOW-ENTRY-TRADEIN.
           MOVE QGP-TRUE-NEXT-BATCH TO GP-TRUE-NEXT-BATCH.
           MOVE QGP-REPO-CODE TO GP-REPO-CODE.
           MOVE QGP-ALLOW-MORTGAGE-INT-Z TO GP-ALLOW-MORTGAGE-INT-Z.
           MOVE QGP-PROM-AMT-MIN-PCT TO GP-PROM-AMT-MIN-PCT.
           MOVE QGP-BW-PRIVACY-OPT-OUT TO GP-BW-PRIVACY-OPT-OUT.
           MOVE QGP-NO-GL-ON-O2-WHEN-PL TO GP-NO-GL-ON-O2-WHEN-PL.
           MOVE QGP-INTERIM-COUPONS TO GP-INTERIM-COUPONS.
           MOVE QGP-MGRAM-NEXT-BATCH TO GP-MGRAM-NEXT-BATCH.
           MOVE QGP-RESCHEDULE-MEMO TO GP-RESCHEDULE-MEMO.
           MOVE QGP-DONT-COLL-INT-ON-SS TO GP-DONT-COLL-INT-ON-SS.
           MOVE QGP-DL-ACH-FG TO GP-DL-ACH-FG.
           MOVE QGP-GL-NEW-JE TO GP-GL-NEW-JE.
           MOVE QGP-CLEAR-PROM-PDTH TO GP-CLEAR-PROM-PDTH.
           MOVE QGP-CREATE-TP-FORMS TO GP-CREATE-TP-FORMS.
           MOVE QGP-CHECK-RECONCILIATION TO GP-CHECK-RECONCILIATION.
           MOVE QGP-CORP-BRNO TO GP-CORP-BRNO.
           MOVE QGP-PL-REPO-REASON TO GP-PL-REPO-REASON.
           MOVE QGP-ALLOW-MIP-INSURANCE TO GP-ALLOW-MIP-INSURANCE.
           MOVE QGP-ENTER-LPO-FG TO GP-ENTER-LPO-FG.
           MOVE QGP-DLCOMM-CMSI-FG TO GP-DLCOMM-CMSI-FG.
           MOVE QGP-REFCD-NEXT-BATCH TO GP-REFCD-NEXT-BATCH.
           MOVE QGP-EARLIEST-CLOSE-TIME TO GP-EARLIEST-CLOSE-TIME.
           MOVE QGP-GROUP-APPROVAL-FG TO GP-GROUP-APPROVAL-FG.
           MOVE QGP-APPROVAL-MEMO TO GP-APPROVAL-MEMO.
           MOVE QGP-FORCE-INPUT-PP-COVERAGE
              TO GP-FORCE-INPUT-PP-COVERAGE.
           MOVE QGP-NO-SC-USE-DM TO GP-NO-SC-USE-DM.
           MOVE QGP-DEFAULT-O3-INSOURS TO GP-DEFAULT-O3-INSOURS.
           MOVE QGP-USE-BW-BAD-FLAGS TO GP-USE-BW-BAD-FLAGS.
           MOVE QGP-NON-FILE-FEECODE TO GP-NON-FILE-FEECODE.
           MOVE QGP-CHBORR-PAST-DUE-DAYS TO GP-CHBORR-PAST-DUE-DAYS.
           MOVE QGP-CLOSED-FISCAL-FG-1 TO GP-CLOSED-FISCAL-FG(1).
           MOVE QGP-CLOSED-FISCAL-FG-2 TO GP-CLOSED-FISCAL-FG(2).
           MOVE QGP-CLOSED-FISCAL-FG-3 TO GP-CLOSED-FISCAL-FG(3).
           MOVE QGP-CLOSED-FISCAL-FG-4 TO GP-CLOSED-FISCAL-FG(4).
           MOVE QGP-CLOSED-FISCAL-FG-5 TO GP-CLOSED-FISCAL-FG(5).
           MOVE QGP-CLOSED-FISCAL-FG-6 TO GP-CLOSED-FISCAL-FG(6).
           MOVE QGP-CLOSED-FISCAL-FG-7 TO GP-CLOSED-FISCAL-FG(7).
           MOVE QGP-CLOSED-FISCAL-FG-8 TO GP-CLOSED-FISCAL-FG(8).
           MOVE QGP-CLOSED-FISCAL-FG-9 TO GP-CLOSED-FISCAL-FG(9).
           MOVE QGP-CLOSED-FISCAL-FG-10 TO GP-CLOSED-FISCAL-FG(10).
           MOVE QGP-CLOSED-FISCAL-FG-11 TO GP-CLOSED-FISCAL-FG(11).
           MOVE QGP-CLOSED-FISCAL-FG-12 TO GP-CLOSED-FISCAL-FG(12).
           MOVE QGP-BACKDATE-MEMO TO GP-BACKDATE-MEMO.
           MOVE QGP-OTREF-NEXT-BATCH TO GP-OTREF-NEXT-BATCH.
           MOVE QGP-NEW-REPO-UNEARNED-ACCTS
              TO GP-NEW-REPO-UNEARNED-ACCTS.
           MOVE QGP-BK-FLAG-CONFIDENTIAL TO GP-BK-FLAG-CONFIDENTIAL.
           MOVE QGP-AMORTIZE-ACQUIST-FEE TO GP-AMORTIZE-ACQUIST-FEE.
           MOVE QGP-DENY-FORM-NO-ADDRESS TO GP-DENY-FORM-NO-ADDRESS.
           MOVE QGP-HIDE-FIRST-5-SSNO TO GP-HIDE-FIRST-5-SSNO.
           MOVE QGP-DEFERMENT-APPROVAL TO GP-DEFERMENT-APPROVAL.
           MOVE QGP-INCLUDE-O2-IN-BAL TO GP-INCLUDE-O2-IN-BAL.
           MOVE QGP-LOAN-ACH-PROCESSING TO GP-LOAN-ACH-PROCESSING.
           MOVE QGP-PAYEE-PROCESSING-FG TO GP-PAYEE-PROCESSING-FG.
           MOVE QGP-AUTO-BANKRUPT-WINDOW TO GP-AUTO-BANKRUPT-WINDOW.
           MOVE QGP-BW-SCORES-QUARTER-SUB TO GP-BW-SCORES-QUARTER-SUB.
           MOVE QGP-POST-XX-BF-PAYMENTS TO GP-POST-XX-BF-PAYMENTS.
           MOVE QGP-VALIDATE-ENDORSER-AGE TO GP-VALIDATE-ENDORSER-AGE.
           MOVE QGP-ALLOW-MANUAL-CHECKS TO GP-ALLOW-MANUAL-CHECKS.
           MOVE QGP-EXPANDED-FOLLOW-UP TO GP-EXPANDED-FOLLOW-UP.
           MOVE QGP-NEXT-EMPLOYEE-NO TO GP-NEXT-EMPLOYEE-NO.
           MOVE QGP-USE-RESULT-WORKED-FG TO GP-USE-RESULT-WORKED-FG.
           MOVE QGP-MAX-PROMISE-DAYS TO GP-MAX-PROMISE-DAYS.
           MOVE QGP-PURGE-TO-ARCHIVE-FG TO GP-PURGE-TO-ARCHIVE-FG.
           MOVE QGP-EDIT-BORROWER-NAME-FG TO GP-EDIT-BORROWER-NAME-FG.
           MOVE QGP-RECAP-CLASS-TOTALS TO GP-RECAP-CLASS-TOTALS.
           MOVE QGP-BRING-AUTO-INS-ON-RENEWAL
              TO GP-BRING-AUTO-INS-ON-RENEWAL.
           MOVE QGP-ALLOW-MF-ON-IB TO GP-ALLOW-MF-ON-IB.
           MOVE QGP-USE-MIN-FORM-SENT-DAYS
              TO GP-USE-MIN-FORM-SENT-DAYS.
           MOVE QGP-DISPLAY-BAD-ADR-FLAG TO GP-DISPLAY-BAD-ADR-FLAG.
           MOVE QGP-DAYS-IDENT-NEW-PAPER TO GP-DAYS-IDENT-NEW-PAPER.
           MOVE QGP-ESCROW-FG TO GP-ESCROW-FG.
           MOVE QGP-SECURITY-FG TO GP-SECURITY-FG.
           MOVE QGP-GPS-INFORMATION TO GP-GPS-INFORMATION.
           MOVE QGP-CUST-INFO-FG TO GP-CUST-INFO-FG.
           MOVE QGP-USE-ADDRESS-BW-SCAN TO GP-USE-ADDRESS-BW-SCAN.
           MOVE QGP-EXPANDED-LEGAL-WINDOW TO GP-EXPANDED-LEGAL-WINDOW.
           MOVE QGP-USE-SCRA TO GP-USE-SCRA.
           MOVE QGP-USE-ACCRUAL-EARN-FILE TO GP-USE-ACCRUAL-EARN-FILE.
           MOVE QGP-MESSAGE-ATTR TO GP-MESSAGE-ATTR.
           MOVE QGP-JUDGMENT-WINDOW TO GP-JUDGMENT-WINDOW.
           MOVE QGP-DUE-29-30-31 TO GP-DUE-29-30-31.
           MOVE QGP-DEFAULT-SKIP-TRW TO GP-DEFAULT-SKIP-TRW.
           MOVE QGP-EARLIEST-OPEN-TIME TO GP-EARLIEST-OPEN-TIME.
           MOVE QGP-TRANSFER-CONPROFILE TO GP-TRANSFER-CONPROFILE.
           MOVE QGP-BATCH-REFCD-DUP-DAYS TO GP-BATCH-REFCD-DUP-DAYS.
 
      *-----------------------------------------------------------------
      *    LOAD QGP-REC FROM GP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GP-FIELDS.
           MOVE GP-KEY TO QGP-KEY.

           IF ( GP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGP-LTOUCH-DATE
           ELSE
              MOVE GP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-LTOUCH-DATE.


           IF ( GP-BEG-FISCAL-DATE = ZEROES )
              MOVE "1900-01-01" TO QGP-BEG-FISCAL-DATE
           ELSE
              MOVE GP-BEG-FISCAL-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-BEG-FISCAL-DATE.


           IF ( GP-END-FISCAL-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-1
           ELSE
              MOVE GP-END-FISCAL-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-1.


           IF ( GP-END-FISCAL-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-2
           ELSE
              MOVE GP-END-FISCAL-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-2.


           IF ( GP-END-FISCAL-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-3
           ELSE
              MOVE GP-END-FISCAL-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-3.


           IF ( GP-END-FISCAL-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-4
           ELSE
              MOVE GP-END-FISCAL-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-4.


           IF ( GP-END-FISCAL-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-5
           ELSE
              MOVE GP-END-FISCAL-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-5.


           IF ( GP-END-FISCAL-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-6
           ELSE
              MOVE GP-END-FISCAL-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-6.


           IF ( GP-END-FISCAL-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-7
           ELSE
              MOVE GP-END-FISCAL-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-7.


           IF ( GP-END-FISCAL-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-8
           ELSE
              MOVE GP-END-FISCAL-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-8.


           IF ( GP-END-FISCAL-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-9
           ELSE
              MOVE GP-END-FISCAL-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-9.


           IF ( GP-END-FISCAL-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-10
           ELSE
              MOVE GP-END-FISCAL-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-10.


           IF ( GP-END-FISCAL-DATE(11) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-11
           ELSE
              MOVE GP-END-FISCAL-DATE(11) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-11.


           IF ( GP-END-FISCAL-DATE(12) = ZEROES )
              MOVE "1900-01-01" TO QGP-END-FISCAL-DATE-12
           ELSE
              MOVE GP-END-FISCAL-DATE(12) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGP-END-FISCAL-DATE-12.

           MOVE GP-BANK-GL TO QGP-BANK-GL.
           MOVE GP-BANK-CLEARING-GL TO QGP-BANK-CLEARING-GL.
           MOVE GP-BANK-ROUTING-NO TO QGP-BANK-ROUTING-NO.
           MOVE GP-BANK-ACCTNO TO QGP-BANK-ACCTNO.
           MOVE GP-TR-RETAIN-MONTHS TO QGP-TR-RETAIN-MONTHS.
           MOVE GP-FED-TAX-ID TO QGP-FED-TAX-ID.
           MOVE GP-CORP-NAME TO QGP-CORP-NAME.
           MOVE GP-SHORT-NAME TO QGP-SHORT-NAME.
           MOVE GP-BANK-NAME TO QGP-BANK-NAME.
           MOVE GP-BANK-DEPOSIT-GL TO QGP-BANK-DEPOSIT-GL.
           MOVE GP-AGE-BUCKET-CD TO QGP-AGE-BUCKET-CD.
           MOVE GP-THE-ONLY-SF-SRCD TO QGP-THE-ONLY-SF-SRCD.
           MOVE GP-ALLOW-BATCH-CHECKS TO QGP-ALLOW-BATCH-CHECKS.
           MOVE GP-DLRVPRIN-3-FG TO QGP-DLRVPRIN-3-FG.
           MOVE GP-DLRVCHGS-3-FG TO QGP-DLRVCHGS-3-FG.
           MOVE GP-CASH-METHOD-ONLY TO QGP-CASH-METHOD-ONLY.
           MOVE GP-IB-ACCRUALS-DONE TO QGP-IB-ACCRUALS-DONE.
           MOVE GP-EOD-RETAIN-MONTHS TO QGP-EOD-RETAIN-MONTHS.
           MOVE GP-NO-LC-IN-ARREARAGE-FG TO QGP-NO-LC-IN-ARREARAGE-FG.
           MOVE GP-DEFAULT-O3-INS-COMPANY
            TO QGP-DEFAULT-O3-INS-COMPANY.
           MOVE GP-STOP-ON-PRIOR-FG TO QGP-STOP-ON-PRIOR-FG.
           MOVE GP-APPLICATION-FG TO QGP-APPLICATION-FG.
           MOVE GP-DEFAULT-ACCRATE TO QGP-DEFAULT-ACCRATE.
           MOVE GP-DLR-DISP-TYPE TO QGP-DLR-DISP-TYPE.
           MOVE GP-ALLOW-RLOGINS TO QGP-ALLOW-RLOGINS.
           MOVE GP-AUTO-REPO-WINDOW TO QGP-AUTO-REPO-WINDOW.
           MOVE GP-ALLOW-PI-WITH-SE-BAL TO QGP-ALLOW-PI-WITH-SE-BAL.
           MOVE GP-ALLOW-MORE-COLLECT TO QGP-ALLOW-MORE-COLLECT.
           MOVE GP-BATCH-MORE-THAN-DAY TO QGP-BATCH-MORE-THAN-DAY.
           MOVE GP-PRINT-GL-RECAP TO QGP-PRINT-GL-RECAP.
           MOVE GP-LOAN-CONVERSION-UPD-FG
            TO QGP-LOAN-CONVERSION-UPD-FG.
           MOVE GP-FORM-CHG-CD TO QGP-FORM-CHG-CD.
           MOVE GP-PROGRP-PRINT-REDEEM TO QGP-PROGRP-PRINT-REDEEM.
           MOVE GP-BW-HOUSING-CODE TO QGP-BW-HOUSING-CODE.
           MOVE GP-BW-CREDIT-SCORE TO QGP-BW-CREDIT-SCORE.
           MOVE GP-DISPLAY-BANK-BAL TO QGP-DISPLAY-BANK-BAL.
           MOVE GP-PROGRP-DL-CONT TO QGP-PROGRP-DL-CONT.
           MOVE GP-DLRVPRIN-2-FG TO QGP-DLRVPRIN-2-FG.
           MOVE GP-DLRVPRIN-1-FG TO QGP-DLRVPRIN-1-FG.
           MOVE GP-ADV-TO-1ST-NO-CORR TO QGP-ADV-TO-1ST-NO-CORR.
           MOVE GP-COMPANY-FLAG TO QGP-COMPANY-FLAG.
           MOVE GP-INTERBR-AR-AP-FULLGL TO QGP-INTERBR-AR-AP-FULLGL.
           MOVE GP-SKIP-BI-UPDATE TO QGP-SKIP-BI-UPDATE.
           MOVE GP-ASK-HARD-COPY-MEMOS TO QGP-ASK-HARD-COPY-MEMOS.
           MOVE GP-RLOGIN-LOCAL-PROFILE TO QGP-RLOGIN-LOCAL-PROFILE.
           MOVE GP-RLOGIN-REMOTE-PROFILE TO QGP-RLOGIN-REMOTE-PROFILE.
           MOVE GP-REPODATE-PAYDATE TO QGP-REPODATE-PAYDATE.
           MOVE GP-POST-INTO-FUTURE TO QGP-POST-INTO-FUTURE.
           MOVE GP-RENEWAL-COLLATERAL TO QGP-RENEWAL-COLLATERAL.
           MOVE GP-PROGRP-LN-CLASS(1) TO QGP-PROGRP-LN-CLASS-1.
           MOVE GP-PROGRP-LN-CLASS(2) TO QGP-PROGRP-LN-CLASS-2.
           MOVE GP-PROGRP-LN-CLASS(3) TO QGP-PROGRP-LN-CLASS-3.
           MOVE GP-PROGRP-SF-CLASS(1) TO QGP-PROGRP-SF-CLASS-1.
           MOVE GP-PROGRP-SF-CLASS(2) TO QGP-PROGRP-SF-CLASS-2.
           MOVE GP-PROGRP-SF-CLASS(3) TO QGP-PROGRP-SF-CLASS-3.
           MOVE GP-PROGRP-SF-CLASS(4) TO QGP-PROGRP-SF-CLASS-4.
           MOVE GP-RENEW-DISC-UNERN TO QGP-RENEW-DISC-UNERN.
           MOVE GP-NOTES-EOD-BATCH-ID TO QGP-NOTES-EOD-BATCH-ID.
           MOVE GP-NEXT-NOTE TO QGP-NEXT-NOTE.
           MOVE GP-NEXT-BATCH-ID TO QGP-NEXT-BATCH-ID.
           MOVE GP-REFNO-VERIFY TO QGP-REFNO-VERIFY.
           MOVE GP-ACTIONCD-VERIFY TO QGP-ACTIONCD-VERIFY.
           MOVE GP-BP-NEXT-BATCH TO QGP-BP-NEXT-BATCH.
           MOVE GP-IB-ACCRUE-EARN-ON-REPO
            TO QGP-IB-ACCRUE-EARN-ON-REPO.
           MOVE GP-DELINQ-WITH-O2 TO QGP-DELINQ-WITH-O2.
           MOVE GP-ALLOW-REPO-ON-PL TO QGP-ALLOW-REPO-ON-PL.
           MOVE GP-BK-PRIOR-TO-POSTING TO QGP-BK-PRIOR-TO-POSTING.
           MOVE GP-AMT-CASH-PROCEEDS TO QGP-AMT-CASH-PROCEEDS.
           MOVE GP-INCL-OTH-IN-EARNED TO QGP-INCL-OTH-IN-EARNED.
           MOVE GP-VALID-BDATE TO QGP-VALID-BDATE.
           MOVE GP-ARCHIVE-DIRECTORY TO QGP-ARCHIVE-DIRECTORY.
           MOVE GP-DL-MEMO-TOGGLE TO QGP-DL-MEMO-TOGGLE.
           MOVE GP-RN-FOR10-LN TO QGP-RN-FOR10-LN.
           MOVE GP-LI-APPLICATION-FG TO QGP-LI-APPLICATION-FG.
           MOVE GP-OT-INS-TERM TO QGP-OT-INS-TERM.
           MOVE GP-UNEARNED-STATS TO QGP-UNEARNED-STATS.
           MOVE GP-PDTH-GT-SCHED-PAY TO QGP-PDTH-GT-SCHED-PAY.
           MOVE GP-LB-NEXT-BATCH TO QGP-LB-NEXT-BATCH.
           MOVE GP-RECDEL-REVERSAL-FG TO QGP-RECDEL-REVERSAL-FG.
           MOVE GP-REF-PHONE TO QGP-REF-PHONE.
           MOVE GP-ALLOW-PL-ON-SX TO QGP-ALLOW-PL-ON-SX.
           MOVE GP-USE-CONRATE TO QGP-USE-CONRATE.
           MOVE GP-CPCORP-CURBAL TO QGP-CPCORP-CURBAL.
           MOVE GP-TIME-ZONE-Q TO QGP-TIME-ZONE-Q.
           MOVE GP-TIME-ZONE-FACTOR TO QGP-TIME-ZONE-FACTOR.
           MOVE GP-TIME-ZONE-DLS TO QGP-TIME-ZONE-DLS.
           MOVE GP-CALL-START-TIME TO QGP-CALL-START-TIME.
           MOVE GP-CALL-END-TIME TO QGP-CALL-END-TIME.
           MOVE GP-RV-MEMO TO QGP-RV-MEMO.
           MOVE GP-PENALTY-INT TO QGP-PENALTY-INT.
           MOVE GP-ALLOW-SET-FOR-NONCASH TO QGP-ALLOW-SET-FOR-NONCASH.
           MOVE GP-IGNORE-LCHG-NXDUE TO QGP-IGNORE-LCHG-NXDUE.
           MOVE GP-ALLOW-ENTRY-TRADEIN TO QGP-ALLOW-ENTRY-TRADEIN.
           MOVE GP-TRUE-NEXT-BATCH TO QGP-TRUE-NEXT-BATCH.
           MOVE GP-REPO-CODE TO QGP-REPO-CODE.
           MOVE GP-ALLOW-MORTGAGE-INT-Z TO QGP-ALLOW-MORTGAGE-INT-Z.
           MOVE GP-PROM-AMT-MIN-PCT TO QGP-PROM-AMT-MIN-PCT.
           MOVE GP-BW-PRIVACY-OPT-OUT TO QGP-BW-PRIVACY-OPT-OUT.
           MOVE GP-NO-GL-ON-O2-WHEN-PL TO QGP-NO-GL-ON-O2-WHEN-PL.
           MOVE GP-INTERIM-COUPONS TO QGP-INTERIM-COUPONS.
           MOVE GP-MGRAM-NEXT-BATCH TO QGP-MGRAM-NEXT-BATCH.
           MOVE GP-RESCHEDULE-MEMO TO QGP-RESCHEDULE-MEMO.
           MOVE GP-DONT-COLL-INT-ON-SS TO QGP-DONT-COLL-INT-ON-SS.
           MOVE GP-DL-ACH-FG TO QGP-DL-ACH-FG.
           MOVE GP-GL-NEW-JE TO QGP-GL-NEW-JE.
           MOVE GP-CLEAR-PROM-PDTH TO QGP-CLEAR-PROM-PDTH.
           MOVE GP-CREATE-TP-FORMS TO QGP-CREATE-TP-FORMS.
           MOVE GP-CHECK-RECONCILIATION TO QGP-CHECK-RECONCILIATION.
           MOVE GP-CORP-BRNO TO QGP-CORP-BRNO.
           MOVE GP-PL-REPO-REASON TO QGP-PL-REPO-REASON.
           MOVE GP-ALLOW-MIP-INSURANCE TO QGP-ALLOW-MIP-INSURANCE.
           MOVE GP-ENTER-LPO-FG TO QGP-ENTER-LPO-FG.
           MOVE GP-DLCOMM-CMSI-FG TO QGP-DLCOMM-CMSI-FG.
           MOVE GP-REFCD-NEXT-BATCH TO QGP-REFCD-NEXT-BATCH.
           MOVE GP-EARLIEST-CLOSE-TIME TO QGP-EARLIEST-CLOSE-TIME.
           MOVE GP-GROUP-APPROVAL-FG TO QGP-GROUP-APPROVAL-FG.
           MOVE GP-APPROVAL-MEMO TO QGP-APPROVAL-MEMO.
           MOVE GP-FORCE-INPUT-PP-COVERAGE
            TO QGP-FORCE-INPUT-PP-COVERAGE.
           MOVE GP-NO-SC-USE-DM TO QGP-NO-SC-USE-DM.
           MOVE GP-DEFAULT-O3-INSOURS TO QGP-DEFAULT-O3-INSOURS.
           MOVE GP-USE-BW-BAD-FLAGS TO QGP-USE-BW-BAD-FLAGS.
           MOVE GP-NON-FILE-FEECODE TO QGP-NON-FILE-FEECODE.
           MOVE GP-CHBORR-PAST-DUE-DAYS TO QGP-CHBORR-PAST-DUE-DAYS.
           MOVE GP-CLOSED-FISCAL-FG(1) TO QGP-CLOSED-FISCAL-FG-1.
           MOVE GP-CLOSED-FISCAL-FG(2) TO QGP-CLOSED-FISCAL-FG-2.
           MOVE GP-CLOSED-FISCAL-FG(3) TO QGP-CLOSED-FISCAL-FG-3.
           MOVE GP-CLOSED-FISCAL-FG(4) TO QGP-CLOSED-FISCAL-FG-4.
           MOVE GP-CLOSED-FISCAL-FG(5) TO QGP-CLOSED-FISCAL-FG-5.
           MOVE GP-CLOSED-FISCAL-FG(6) TO QGP-CLOSED-FISCAL-FG-6.
           MOVE GP-CLOSED-FISCAL-FG(7) TO QGP-CLOSED-FISCAL-FG-7.
           MOVE GP-CLOSED-FISCAL-FG(8) TO QGP-CLOSED-FISCAL-FG-8.
           MOVE GP-CLOSED-FISCAL-FG(9) TO QGP-CLOSED-FISCAL-FG-9.
           MOVE GP-CLOSED-FISCAL-FG(10) TO QGP-CLOSED-FISCAL-FG-10.
           MOVE GP-CLOSED-FISCAL-FG(11) TO QGP-CLOSED-FISCAL-FG-11.
           MOVE GP-CLOSED-FISCAL-FG(12) TO QGP-CLOSED-FISCAL-FG-12.
           MOVE GP-BACKDATE-MEMO TO QGP-BACKDATE-MEMO.
           MOVE GP-OTREF-NEXT-BATCH TO QGP-OTREF-NEXT-BATCH.
           MOVE GP-NEW-REPO-UNEARNED-ACCTS
            TO QGP-NEW-REPO-UNEARNED-ACCTS.
           MOVE GP-BK-FLAG-CONFIDENTIAL TO QGP-BK-FLAG-CONFIDENTIAL.
           MOVE GP-AMORTIZE-ACQUIST-FEE TO QGP-AMORTIZE-ACQUIST-FEE.
           MOVE GP-DENY-FORM-NO-ADDRESS TO QGP-DENY-FORM-NO-ADDRESS.
           MOVE GP-HIDE-FIRST-5-SSNO TO QGP-HIDE-FIRST-5-SSNO.
           MOVE GP-DEFERMENT-APPROVAL TO QGP-DEFERMENT-APPROVAL.
           MOVE GP-INCLUDE-O2-IN-BAL TO QGP-INCLUDE-O2-IN-BAL.
           MOVE GP-LOAN-ACH-PROCESSING TO QGP-LOAN-ACH-PROCESSING.
           MOVE GP-PAYEE-PROCESSING-FG TO QGP-PAYEE-PROCESSING-FG.
           MOVE GP-AUTO-BANKRUPT-WINDOW TO QGP-AUTO-BANKRUPT-WINDOW.
           MOVE GP-BW-SCORES-QUARTER-SUB TO QGP-BW-SCORES-QUARTER-SUB.
           MOVE GP-POST-XX-BF-PAYMENTS TO QGP-POST-XX-BF-PAYMENTS.
           MOVE GP-VALIDATE-ENDORSER-AGE TO QGP-VALIDATE-ENDORSER-AGE.
           MOVE GP-ALLOW-MANUAL-CHECKS TO QGP-ALLOW-MANUAL-CHECKS.
           MOVE GP-EXPANDED-FOLLOW-UP TO QGP-EXPANDED-FOLLOW-UP.
           MOVE GP-NEXT-EMPLOYEE-NO TO QGP-NEXT-EMPLOYEE-NO.
           MOVE GP-USE-RESULT-WORKED-FG TO QGP-USE-RESULT-WORKED-FG.
           MOVE GP-MAX-PROMISE-DAYS TO QGP-MAX-PROMISE-DAYS.
           MOVE GP-PURGE-TO-ARCHIVE-FG TO QGP-PURGE-TO-ARCHIVE-FG.
           MOVE GP-EDIT-BORROWER-NAME-FG TO QGP-EDIT-BORROWER-NAME-FG.
           MOVE GP-RECAP-CLASS-TOTALS TO QGP-RECAP-CLASS-TOTALS.
           MOVE GP-BRING-AUTO-INS-ON-RENEWAL
            TO QGP-BRING-AUTO-INS-ON-RENEWAL.
           MOVE GP-ALLOW-MF-ON-IB TO QGP-ALLOW-MF-ON-IB.
           MOVE GP-USE-MIN-FORM-SENT-DAYS
            TO QGP-USE-MIN-FORM-SENT-DAYS.
           MOVE GP-DISPLAY-BAD-ADR-FLAG TO QGP-DISPLAY-BAD-ADR-FLAG.
           MOVE GP-DAYS-IDENT-NEW-PAPER TO QGP-DAYS-IDENT-NEW-PAPER.
           MOVE GP-ESCROW-FG TO QGP-ESCROW-FG.
           MOVE GP-SECURITY-FG TO QGP-SECURITY-FG.
           MOVE GP-GPS-INFORMATION TO QGP-GPS-INFORMATION.
           MOVE GP-CUST-INFO-FG TO QGP-CUST-INFO-FG.
           MOVE GP-USE-ADDRESS-BW-SCAN TO QGP-USE-ADDRESS-BW-SCAN.
           MOVE GP-EXPANDED-LEGAL-WINDOW TO QGP-EXPANDED-LEGAL-WINDOW.
           MOVE GP-USE-SCRA TO QGP-USE-SCRA.
           MOVE GP-USE-ACCRUAL-EARN-FILE TO QGP-USE-ACCRUAL-EARN-FILE.
           MOVE GP-MESSAGE-ATTR TO QGP-MESSAGE-ATTR.
           MOVE GP-JUDGMENT-WINDOW TO QGP-JUDGMENT-WINDOW.
           MOVE GP-DUE-29-30-31 TO QGP-DUE-29-30-31.
           MOVE GP-DEFAULT-SKIP-TRW TO QGP-DEFAULT-SKIP-TRW.
           MOVE GP-EARLIEST-OPEN-TIME TO QGP-EARLIEST-OPEN-TIME.
           MOVE GP-TRANSFER-CONPROFILE TO QGP-TRANSFER-CONPROFILE.
           MOVE GP-BATCH-REFCD-DUP-DAYS TO QGP-BATCH-REFCD-DUP-DAYS.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GB%%1RN
       LOAD-GP-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GP-ALL-MACHINE
              MOVE GP-ALL-PATH             TO GP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GP-ALL-BASE
              MOVE FILPATH-MACHINE         TO GP-ALL-MACHINE
              MOVE GP-ALL-PATH             TO GP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GPFILE NOT = "S" )
              MOVE GP-PATH-BEG             TO GP-SELECT-PATH-BEG
              MOVE GP-PATH-FILE            TO GP-SELECT-PATH-FILE
           ELSE
              MOVE GP-PATH-SQL             TO GP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GP-FILE.
           PERFORM LOAD-GP-FILE.
           PERFORM OPEN-IT.
           MOVE GP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GP-FILE.
           PERFORM START-IT.
           MOVE GP-SELECT-PATH TO E-FILE.
           MOVE GP-KEY       TO E-KEYX.

           IF ( GP-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GP-FILE.

           IF ( QGP-WEND-KEY = ZEROES    ) OR
              ( QGP-WEND-KEY NOT NUMERIC )
              MOVE GP-KEY              TO QGP-WBEG-KEY
              MOVE 99                  TO QGP-WEND-KEY.

           EXEC SQL
            DECLARE GP_CURSOR CURSOR FOR
            SELECT GPFILE.GP_KEY,
                   ISNULL(CAST(GPFILE.GP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_BEG_FISCAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_11
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_12
                                                  AS VARCHAR(10)),'0'),
                   GPFILE.GP_BANK_GL,
                   GPFILE.GP_BANK_CLEARING_GL,
                   GPFILE.GP_BANK_ROUTING_NO,
                   GPFILE.GP_BANK_ACCTNO,
                   GPFILE.GP_TR_RETAIN_MONTHS,
                   GPFILE.GP_FED_TAX_ID,
                   ISNULL(GPFILE.GP_CORP_NAME,''),
                   ISNULL(GPFILE.GP_SHORT_NAME,''),
                   ISNULL(GPFILE.GP_BANK_NAME,''),
                   GPFILE.GP_BANK_DEPOSIT_GL,
                   ISNULL(GPFILE.GP_AGE_BUCKET_CD,''),
                   ISNULL(GPFILE.GP_THE_ONLY_SF_SRCD,''),
                   ISNULL(GPFILE.GP_ALLOW_BATCH_CHECKS,''),
                   ISNULL(GPFILE.GP_DLRVPRIN_3_FG,''),
                   ISNULL(GPFILE.GP_DLRVCHGS_3_FG,''),
                   ISNULL(GPFILE.GP_CASH_METHOD_ONLY,''),
                   ISNULL(GPFILE.GP_IB_ACCRUALS_DONE,''),
                   GPFILE.GP_EOD_RETAIN_MONTHS,
                   ISNULL(GPFILE.GP_NO_LC_IN_ARREARAGE_FG,''),
                   GPFILE.GP_DEFAULT_O3_INS_COMPANY,
                   ISNULL(GPFILE.GP_STOP_ON_PRIOR_FG,''),
                   ISNULL(GPFILE.GP_APPLICATION_FG,''),
                   GPFILE.GP_DEFAULT_ACCRATE,
                   ISNULL(GPFILE.GP_DLR_DISP_TYPE,''),
                   ISNULL(GPFILE.GP_ALLOW_RLOGINS,''),
                   ISNULL(GPFILE.GP_AUTO_REPO_WINDOW,''),
                   ISNULL(GPFILE.GP_ALLOW_PI_WITH_SE_BAL,''),
                   ISNULL(GPFILE.GP_ALLOW_MORE_COLLECT,''),
                   ISNULL(GPFILE.GP_BATCH_MORE_THAN_DAY,''),
                   ISNULL(GPFILE.GP_PRINT_GL_RECAP,''),
                   ISNULL(GPFILE.GP_LOAN_CONVERSION_UPD_FG,''),
                   ISNULL(GPFILE.GP_FORM_CHG_CD,''),
                   ISNULL(GPFILE.GP_PROGRP_PRINT_REDEEM,''),
                   ISNULL(GPFILE.GP_BW_HOUSING_CODE,''),
                   ISNULL(GPFILE.GP_BW_CREDIT_SCORE,''),
                   ISNULL(GPFILE.GP_DISPLAY_BANK_BAL,''),
                   ISNULL(GPFILE.GP_PROGRP_DL_CONT,''),
                   ISNULL(GPFILE.GP_DLRVPRIN_2_FG,''),
                   ISNULL(GPFILE.GP_DLRVPRIN_1_FG,''),
                   ISNULL(GPFILE.GP_ADV_TO_1ST_NO_CORR,''),
                   ISNULL(GPFILE.GP_COMPANY_FLAG,''),
                   ISNULL(GPFILE.GP_INTERBR_AR_AP_FULLGL,''),
                   ISNULL(GPFILE.GP_SKIP_BI_UPDATE,''),
                   ISNULL(GPFILE.GP_ASK_HARD_COPY_MEMOS,''),
                   ISNULL(GPFILE.GP_RLOGIN_LOCAL_PROFILE,''),
                   ISNULL(GPFILE.GP_RLOGIN_REMOTE_PROFILE,''),
                   ISNULL(GPFILE.GP_REPODATE_PAYDATE,''),
                   ISNULL(GPFILE.GP_POST_INTO_FUTURE,''),
                   ISNULL(GPFILE.GP_RENEWAL_COLLATERAL,''),
                   GPFILE.GP_PROGRP_LN_CLASS_1,
                   GPFILE.GP_PROGRP_LN_CLASS_2,
                   GPFILE.GP_PROGRP_LN_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_1,
                   GPFILE.GP_PROGRP_SF_CLASS_2,
                   GPFILE.GP_PROGRP_SF_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_4,
                   ISNULL(GPFILE.GP_RENEW_DISC_UNERN,''),
                   ISNULL(GPFILE.GP_NOTES_EOD_BATCH_ID,''),
                   GPFILE.GP_NEXT_NOTE,
                   GPFILE.GP_NEXT_BATCH_ID,
                   ISNULL(GPFILE.GP_REFNO_VERIFY,''),
                   ISNULL(GPFILE.GP_ACTIONCD_VERIFY,''),
                   GPFILE.GP_BP_NEXT_BATCH,
                   ISNULL(GPFILE.GP_IB_ACCRUE_EARN_ON_REPO,''),
                   ISNULL(GPFILE.GP_DELINQ_WITH_O2,''),
                   ISNULL(GPFILE.GP_ALLOW_REPO_ON_PL,''),
                   ISNULL(GPFILE.GP_BK_PRIOR_TO_POSTING,''),
                   GPFILE.GP_AMT_CASH_PROCEEDS,
                   ISNULL(GPFILE.GP_INCL_OTH_IN_EARNED,''),
                   ISNULL(GPFILE.GP_VALID_BDATE,''),
                   ISNULL(GPFILE.GP_ARCHIVE_DIRECTORY,''),
                   ISNULL(GPFILE.GP_DL_MEMO_TOGGLE,''),
                   ISNULL(GPFILE.GP_RN_FOR10_LN,''),
                   ISNULL(GPFILE.GP_LI_APPLICATION_FG,''),
                   ISNULL(GPFILE.GP_OT_INS_TERM,''),
                   ISNULL(GPFILE.GP_UNEARNED_STATS,''),
                   ISNULL(GPFILE.GP_PDTH_GT_SCHED_PAY,''),
                   GPFILE.GP_LB_NEXT_BATCH,
                   ISNULL(GPFILE.GP_RECDEL_REVERSAL_FG,''),
                   ISNULL(GPFILE.GP_REF_PHONE,''),
                   ISNULL(GPFILE.GP_ALLOW_PL_ON_SX,''),
                   ISNULL(GPFILE.GP_USE_CONRATE,''),
                   ISNULL(GPFILE.GP_CPCORP_CURBAL,''),
                   ISNULL(GPFILE.GP_TIME_ZONE_Q,''),
                   GPFILE.GP_TIME_ZONE_FACTOR,
                   ISNULL(GPFILE.GP_TIME_ZONE_DLS,''),
                   GPFILE.GP_CALL_START_TIME,
                   GPFILE.GP_CALL_END_TIME,
                   ISNULL(GPFILE.GP_RV_MEMO,''),
                   ISNULL(GPFILE.GP_PENALTY_INT,''),
                   ISNULL(GPFILE.GP_ALLOW_SET_FOR_NONCASH,''),
                   ISNULL(GPFILE.GP_IGNORE_LCHG_NXDUE,''),
                   ISNULL(GPFILE.GP_ALLOW_ENTRY_TRADEIN,''),
                   GPFILE.GP_TRUE_NEXT_BATCH,
                   ISNULL(GPFILE.GP_REPO_CODE,''),
                   ISNULL(GPFILE.GP_ALLOW_MORTGAGE_INT_Z,''),
                   GPFILE.GP_PROM_AMT_MIN_PCT,
                   ISNULL(GPFILE.GP_BW_PRIVACY_OPT_OUT,''),
                   ISNULL(GPFILE.GP_NO_GL_ON_O2_WHEN_PL,''),
                   ISNULL(GPFILE.GP_INTERIM_COUPONS,''),
                   GPFILE.GP_MGRAM_NEXT_BATCH,
                   ISNULL(GPFILE.GP_RESCHEDULE_MEMO,''),
                   ISNULL(GPFILE.GP_DONT_COLL_INT_ON_SS,''),
                   ISNULL(GPFILE.GP_DL_ACH_FG,''),
                   ISNULL(GPFILE.GP_GL_NEW_JE,''),
                   ISNULL(GPFILE.GP_CLEAR_PROM_PDTH,''),
                   ISNULL(GPFILE.GP_CREATE_TP_FORMS,''),
                   ISNULL(GPFILE.GP_CHECK_RECONCILIATION,''),
                   GPFILE.GP_CORP_BRNO,
                   ISNULL(GPFILE.GP_PL_REPO_REASON,''),
                   ISNULL(GPFILE.GP_ALLOW_MIP_INSURANCE,''),
                   ISNULL(GPFILE.GP_ENTER_LPO_FG,''),
                   ISNULL(GPFILE.GP_DLCOMM_CMSI_FG,''),
                   GPFILE.GP_REFCD_NEXT_BATCH,
                   GPFILE.GP_EARLIEST_CLOSE_TIME,
                   ISNULL(GPFILE.GP_GROUP_APPROVAL_FG,''),
                   ISNULL(GPFILE.GP_APPROVAL_MEMO,''),
                   ISNULL(GPFILE.GP_FORCE_INPUT_PP_COVERAGE,''),
                   ISNULL(GPFILE.GP_NO_SC_USE_DM,''),
                   ISNULL(GPFILE.GP_DEFAULT_O3_INSOURS,''),
                   ISNULL(GPFILE.GP_USE_BW_BAD_FLAGS,''),
                   ISNULL(GPFILE.GP_NON_FILE_FEECODE,''),
                   GPFILE.GP_CHBORR_PAST_DUE_DAYS,
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_1,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_2,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_3,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_4,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_5,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_6,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_7,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_8,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_9,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_10,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_11,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_12,''),
                   ISNULL(GPFILE.GP_BACKDATE_MEMO,''),
                   GPFILE.GP_OTREF_NEXT_BATCH,
                   ISNULL(GPFILE.GP_NEW_REPO_UNEARNED_ACCTS,''),
                   ISNULL(GPFILE.GP_BK_FLAG_CONFIDENTIAL,''),
                   ISNULL(GPFILE.GP_AMORTIZE_ACQUIST_FEE,''),
                   ISNULL(GPFILE.GP_DENY_FORM_NO_ADDRESS,''),
                   ISNULL(GPFILE.GP_HIDE_FIRST_5_SSNO,''),
                   ISNULL(GPFILE.GP_DEFERMENT_APPROVAL,''),
                   ISNULL(GPFILE.GP_INCLUDE_O2_IN_BAL,''),
                   ISNULL(GPFILE.GP_LOAN_ACH_PROCESSING,''),
                   ISNULL(GPFILE.GP_PAYEE_PROCESSING_FG,''),
                   ISNULL(GPFILE.GP_AUTO_BANKRUPT_WINDOW,''),
                   GPFILE.GP_BW_SCORES_QUARTER_SUB,
                   ISNULL(GPFILE.GP_POST_XX_BF_PAYMENTS,''),
                   ISNULL(GPFILE.GP_VALIDATE_ENDORSER_AGE,''),
                   ISNULL(GPFILE.GP_ALLOW_MANUAL_CHECKS,''),
                   ISNULL(GPFILE.GP_EXPANDED_FOLLOW_UP,''),
                   GPFILE.GP_NEXT_EMPLOYEE_NO,
                   ISNULL(GPFILE.GP_USE_RESULT_WORKED_FG,''),
                   GPFILE.GP_MAX_PROMISE_DAYS,
                   ISNULL(GPFILE.GP_PURGE_TO_ARCHIVE_FG,''),
                   ISNULL(GPFILE.GP_EDIT_BORROWER_NAME_FG,''),
                   ISNULL(GPFILE.GP_RECAP_CLASS_TOTALS,''),
                   ISNULL(GPFILE.GP_BRING_AUTO_INS_ON_RENEWAL,''),
                   ISNULL(GPFILE.GP_ALLOW_MF_ON_IB,''),
                   ISNULL(GPFILE.GP_USE_MIN_FORM_SENT_DAYS,''),
                   ISNULL(GPFILE.GP_DISPLAY_BAD_ADR_FLAG,''),
                   GPFILE.GP_DAYS_IDENT_NEW_PAPER,
                   ISNULL(GPFILE.GP_ESCROW_FG,''),
                   ISNULL(GPFILE.GP_SECURITY_FG,''),
                   ISNULL(GPFILE.GP_GPS_INFORMATION,''),
                   ISNULL(GPFILE.GP_CUST_INFO_FG,''),
                   ISNULL(GPFILE.GP_USE_ADDRESS_BW_SCAN,''),
                   ISNULL(GPFILE.GP_EXPANDED_LEGAL_WINDOW,''),
                   ISNULL(GPFILE.GP_USE_SCRA,''),
                   ISNULL(GPFILE.GP_USE_ACCRUAL_EARN_FILE,''),
                   ISNULL(GPFILE.GP_MESSAGE_ATTR,''),
                   ISNULL(GPFILE.GP_JUDGMENT_WINDOW,''),
                   ISNULL(GPFILE.GP_DUE_29_30_31,''),
                   ISNULL(GPFILE.GP_DEFAULT_SKIP_TRW,''),
                   GPFILE.GP_EARLIEST_OPEN_TIME,
                   ISNULL(GPFILE.GP_TRANSFER_CONPROFILE,''),
                   GPFILE.GP_BATCH_REFCD_DUP_DAYS
            FROM DBO.GPFILE
            WHERE GPFILE.GP_KEY >= :QGP-WBEG-KEY
             AND GPFILE.GP_KEY <= :QGP-WEND-KEY
            ORDER BY GPFILE.GP_KEY
           END-EXEC.

           EXEC SQL
               OPEN GP_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE 0 TO QGP-WBEG-KEY
                     QGP-WEND-KEY.

           MOVE STAT---GOOD TO GP-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GP-FILE.
           PERFORM READ-IT.
           MOVE GP-SELECT-PATH TO E-FILE.
           MOVE GP-KEY TO E-KEYX.

           MOVE GP-KEY    TO QGP-KEY. 

           EXEC SQL
            SELECT GPFILE.GP_KEY,
                   ISNULL(CAST(GPFILE.GP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_BEG_FISCAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_11
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(GPFILE.GP_END_FISCAL_DATE_12
                                                  AS VARCHAR(10)),'0'),
                   GPFILE.GP_BANK_GL,
                   GPFILE.GP_BANK_CLEARING_GL,
                   GPFILE.GP_BANK_ROUTING_NO,
                   GPFILE.GP_BANK_ACCTNO,
                   GPFILE.GP_TR_RETAIN_MONTHS,
                   GPFILE.GP_FED_TAX_ID,
                   ISNULL(GPFILE.GP_CORP_NAME,''),
                   ISNULL(GPFILE.GP_SHORT_NAME,''),
                   ISNULL(GPFILE.GP_BANK_NAME,''),
                   GPFILE.GP_BANK_DEPOSIT_GL,
                   ISNULL(GPFILE.GP_AGE_BUCKET_CD,''),
                   ISNULL(GPFILE.GP_THE_ONLY_SF_SRCD,''),
                   ISNULL(GPFILE.GP_ALLOW_BATCH_CHECKS,''),
                   ISNULL(GPFILE.GP_DLRVPRIN_3_FG,''),
                   ISNULL(GPFILE.GP_DLRVCHGS_3_FG,''),
                   ISNULL(GPFILE.GP_CASH_METHOD_ONLY,''),
                   ISNULL(GPFILE.GP_IB_ACCRUALS_DONE,''),
                   GPFILE.GP_EOD_RETAIN_MONTHS,
                   ISNULL(GPFILE.GP_NO_LC_IN_ARREARAGE_FG,''),
                   GPFILE.GP_DEFAULT_O3_INS_COMPANY,
                   ISNULL(GPFILE.GP_STOP_ON_PRIOR_FG,''),
                   ISNULL(GPFILE.GP_APPLICATION_FG,''),
                   GPFILE.GP_DEFAULT_ACCRATE,
                   ISNULL(GPFILE.GP_DLR_DISP_TYPE,''),
                   ISNULL(GPFILE.GP_ALLOW_RLOGINS,''),
                   ISNULL(GPFILE.GP_AUTO_REPO_WINDOW,''),
                   ISNULL(GPFILE.GP_ALLOW_PI_WITH_SE_BAL,''),
                   ISNULL(GPFILE.GP_ALLOW_MORE_COLLECT,''),
                   ISNULL(GPFILE.GP_BATCH_MORE_THAN_DAY,''),
                   ISNULL(GPFILE.GP_PRINT_GL_RECAP,''),
                   ISNULL(GPFILE.GP_LOAN_CONVERSION_UPD_FG,''),
                   ISNULL(GPFILE.GP_FORM_CHG_CD,''),
                   ISNULL(GPFILE.GP_PROGRP_PRINT_REDEEM,''),
                   ISNULL(GPFILE.GP_BW_HOUSING_CODE,''),
                   ISNULL(GPFILE.GP_BW_CREDIT_SCORE,''),
                   ISNULL(GPFILE.GP_DISPLAY_BANK_BAL,''),
                   ISNULL(GPFILE.GP_PROGRP_DL_CONT,''),
                   ISNULL(GPFILE.GP_DLRVPRIN_2_FG,''),
                   ISNULL(GPFILE.GP_DLRVPRIN_1_FG,''),
                   ISNULL(GPFILE.GP_ADV_TO_1ST_NO_CORR,''),
                   ISNULL(GPFILE.GP_COMPANY_FLAG,''),
                   ISNULL(GPFILE.GP_INTERBR_AR_AP_FULLGL,''),
                   ISNULL(GPFILE.GP_SKIP_BI_UPDATE,''),
                   ISNULL(GPFILE.GP_ASK_HARD_COPY_MEMOS,''),
                   ISNULL(GPFILE.GP_RLOGIN_LOCAL_PROFILE,''),
                   ISNULL(GPFILE.GP_RLOGIN_REMOTE_PROFILE,''),
                   ISNULL(GPFILE.GP_REPODATE_PAYDATE,''),
                   ISNULL(GPFILE.GP_POST_INTO_FUTURE,''),
                   ISNULL(GPFILE.GP_RENEWAL_COLLATERAL,''),
                   GPFILE.GP_PROGRP_LN_CLASS_1,
                   GPFILE.GP_PROGRP_LN_CLASS_2,
                   GPFILE.GP_PROGRP_LN_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_1,
                   GPFILE.GP_PROGRP_SF_CLASS_2,
                   GPFILE.GP_PROGRP_SF_CLASS_3,
                   GPFILE.GP_PROGRP_SF_CLASS_4,
                   ISNULL(GPFILE.GP_RENEW_DISC_UNERN,''),
                   ISNULL(GPFILE.GP_NOTES_EOD_BATCH_ID,''),
                   GPFILE.GP_NEXT_NOTE,
                   GPFILE.GP_NEXT_BATCH_ID,
                   ISNULL(GPFILE.GP_REFNO_VERIFY,''),
                   ISNULL(GPFILE.GP_ACTIONCD_VERIFY,''),
                   GPFILE.GP_BP_NEXT_BATCH,
                   ISNULL(GPFILE.GP_IB_ACCRUE_EARN_ON_REPO,''),
                   ISNULL(GPFILE.GP_DELINQ_WITH_O2,''),
                   ISNULL(GPFILE.GP_ALLOW_REPO_ON_PL,''),
                   ISNULL(GPFILE.GP_BK_PRIOR_TO_POSTING,''),
                   GPFILE.GP_AMT_CASH_PROCEEDS,
                   ISNULL(GPFILE.GP_INCL_OTH_IN_EARNED,''),
                   ISNULL(GPFILE.GP_VALID_BDATE,''),
                   ISNULL(GPFILE.GP_ARCHIVE_DIRECTORY,''),
                   ISNULL(GPFILE.GP_DL_MEMO_TOGGLE,''),
                   ISNULL(GPFILE.GP_RN_FOR10_LN,''),
                   ISNULL(GPFILE.GP_LI_APPLICATION_FG,''),
                   ISNULL(GPFILE.GP_OT_INS_TERM,''),
                   ISNULL(GPFILE.GP_UNEARNED_STATS,''),
                   ISNULL(GPFILE.GP_PDTH_GT_SCHED_PAY,''),
                   GPFILE.GP_LB_NEXT_BATCH,
                   ISNULL(GPFILE.GP_RECDEL_REVERSAL_FG,''),
                   ISNULL(GPFILE.GP_REF_PHONE,''),
                   ISNULL(GPFILE.GP_ALLOW_PL_ON_SX,''),
                   ISNULL(GPFILE.GP_USE_CONRATE,''),
                   ISNULL(GPFILE.GP_CPCORP_CURBAL,''),
                   ISNULL(GPFILE.GP_TIME_ZONE_Q,''),
                   GPFILE.GP_TIME_ZONE_FACTOR,
                   ISNULL(GPFILE.GP_TIME_ZONE_DLS,''),
                   GPFILE.GP_CALL_START_TIME,
                   GPFILE.GP_CALL_END_TIME,
                   ISNULL(GPFILE.GP_RV_MEMO,''),
                   ISNULL(GPFILE.GP_PENALTY_INT,''),
                   ISNULL(GPFILE.GP_ALLOW_SET_FOR_NONCASH,''),
                   ISNULL(GPFILE.GP_IGNORE_LCHG_NXDUE,''),
                   ISNULL(GPFILE.GP_ALLOW_ENTRY_TRADEIN,''),
                   GPFILE.GP_TRUE_NEXT_BATCH,
                   ISNULL(GPFILE.GP_REPO_CODE,''),
                   ISNULL(GPFILE.GP_ALLOW_MORTGAGE_INT_Z,''),
                   GPFILE.GP_PROM_AMT_MIN_PCT,
                   ISNULL(GPFILE.GP_BW_PRIVACY_OPT_OUT,''),
                   ISNULL(GPFILE.GP_NO_GL_ON_O2_WHEN_PL,''),
                   ISNULL(GPFILE.GP_INTERIM_COUPONS,''),
                   GPFILE.GP_MGRAM_NEXT_BATCH,
                   ISNULL(GPFILE.GP_RESCHEDULE_MEMO,''),
                   ISNULL(GPFILE.GP_DONT_COLL_INT_ON_SS,''),
                   ISNULL(GPFILE.GP_DL_ACH_FG,''),
                   ISNULL(GPFILE.GP_GL_NEW_JE,''),
                   ISNULL(GPFILE.GP_CLEAR_PROM_PDTH,''),
                   ISNULL(GPFILE.GP_CREATE_TP_FORMS,''),
                   ISNULL(GPFILE.GP_CHECK_RECONCILIATION,''),
                   GPFILE.GP_CORP_BRNO,
                   ISNULL(GPFILE.GP_PL_REPO_REASON,''),
                   ISNULL(GPFILE.GP_ALLOW_MIP_INSURANCE,''),
                   ISNULL(GPFILE.GP_ENTER_LPO_FG,''),
                   ISNULL(GPFILE.GP_DLCOMM_CMSI_FG,''),
                   GPFILE.GP_REFCD_NEXT_BATCH,
                   GPFILE.GP_EARLIEST_CLOSE_TIME,
                   ISNULL(GPFILE.GP_GROUP_APPROVAL_FG,''),
                   ISNULL(GPFILE.GP_APPROVAL_MEMO,''),
                   ISNULL(GPFILE.GP_FORCE_INPUT_PP_COVERAGE,''),
                   ISNULL(GPFILE.GP_NO_SC_USE_DM,''),
                   ISNULL(GPFILE.GP_DEFAULT_O3_INSOURS,''),
                   ISNULL(GPFILE.GP_USE_BW_BAD_FLAGS,''),
                   ISNULL(GPFILE.GP_NON_FILE_FEECODE,''),
                   GPFILE.GP_CHBORR_PAST_DUE_DAYS,
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_1,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_2,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_3,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_4,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_5,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_6,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_7,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_8,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_9,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_10,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_11,''),
                   ISNULL(GPFILE.GP_CLOSED_FISCAL_FG_12,''),
                   ISNULL(GPFILE.GP_BACKDATE_MEMO,''),
                   GPFILE.GP_OTREF_NEXT_BATCH,
                   ISNULL(GPFILE.GP_NEW_REPO_UNEARNED_ACCTS,''),
                   ISNULL(GPFILE.GP_BK_FLAG_CONFIDENTIAL,''),
                   ISNULL(GPFILE.GP_AMORTIZE_ACQUIST_FEE,''),
                   ISNULL(GPFILE.GP_DENY_FORM_NO_ADDRESS,''),
                   ISNULL(GPFILE.GP_HIDE_FIRST_5_SSNO,''),
                   ISNULL(GPFILE.GP_DEFERMENT_APPROVAL,''),
                   ISNULL(GPFILE.GP_INCLUDE_O2_IN_BAL,''),
                   ISNULL(GPFILE.GP_LOAN_ACH_PROCESSING,''),
                   ISNULL(GPFILE.GP_PAYEE_PROCESSING_FG,''),
                   ISNULL(GPFILE.GP_AUTO_BANKRUPT_WINDOW,''),
                   GPFILE.GP_BW_SCORES_QUARTER_SUB,
                   ISNULL(GPFILE.GP_POST_XX_BF_PAYMENTS,''),
                   ISNULL(GPFILE.GP_VALIDATE_ENDORSER_AGE,''),
                   ISNULL(GPFILE.GP_ALLOW_MANUAL_CHECKS,''),
                   ISNULL(GPFILE.GP_EXPANDED_FOLLOW_UP,''),
                   GPFILE.GP_NEXT_EMPLOYEE_NO,
                   ISNULL(GPFILE.GP_USE_RESULT_WORKED_FG,''),
                   GPFILE.GP_MAX_PROMISE_DAYS,
                   ISNULL(GPFILE.GP_PURGE_TO_ARCHIVE_FG,''),
                   ISNULL(GPFILE.GP_EDIT_BORROWER_NAME_FG,''),
                   ISNULL(GPFILE.GP_RECAP_CLASS_TOTALS,''),
                   ISNULL(GPFILE.GP_BRING_AUTO_INS_ON_RENEWAL,''),
                   ISNULL(GPFILE.GP_ALLOW_MF_ON_IB,''),
                   ISNULL(GPFILE.GP_USE_MIN_FORM_SENT_DAYS,''),
                   ISNULL(GPFILE.GP_DISPLAY_BAD_ADR_FLAG,''),
                   GPFILE.GP_DAYS_IDENT_NEW_PAPER,
                   ISNULL(GPFILE.GP_ESCROW_FG,''),
                   ISNULL(GPFILE.GP_SECURITY_FG,''),
                   ISNULL(GPFILE.GP_GPS_INFORMATION,''),
                   ISNULL(GPFILE.GP_CUST_INFO_FG,''),
                   ISNULL(GPFILE.GP_USE_ADDRESS_BW_SCAN,''),
                   ISNULL(GPFILE.GP_EXPANDED_LEGAL_WINDOW,''),
                   ISNULL(GPFILE.GP_USE_SCRA,''),
                   ISNULL(GPFILE.GP_USE_ACCRUAL_EARN_FILE,''),
                   ISNULL(GPFILE.GP_MESSAGE_ATTR,''),
                   ISNULL(GPFILE.GP_JUDGMENT_WINDOW,''),
                   ISNULL(GPFILE.GP_DUE_29_30_31,''),
                   ISNULL(GPFILE.GP_DEFAULT_SKIP_TRW,''),
                   GPFILE.GP_EARLIEST_OPEN_TIME,
                   ISNULL(GPFILE.GP_TRANSFER_CONPROFILE,''),
                   GPFILE.GP_BATCH_REFCD_DUP_DAYS
            INTO :QGP-REC
            FROM DBO.GPFILE
            WHERE GPFILE.GP_KEY = :QGP-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GP-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GP-FILE.
           IF ( GP-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GP_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GP-CURSOR-STAT.
 
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
              DESTROY IOGPRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
