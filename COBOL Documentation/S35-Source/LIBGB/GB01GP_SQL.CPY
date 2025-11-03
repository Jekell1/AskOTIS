      * COPYMEMBER: LIBGB/GB01GP_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - GLOBAL PARAMETERS FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      * BAH 2023-0302 ADDED GP-TEST-PP-AUTO-INSCOVR #1594
      * BAH 2023-0419 REMOVED GP-TEST-PP-AUTO-INSCOVR #1594A
      * BAH 2023-1019 ADDED GP-CREATE-PAY-XTRP-XNO #1629
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  GP-CURSOR-STAT               PIC X(2)  VALUE SPACES.
           88  GP-CURSOR-STAT-GOOD                VALUE "00".
           88  GP-CURSOR-STAT-BAD                 VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GP1-RETURN-CODE             PIC S9(4)   COMP-5.

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
           03  QGP-CREATE-PAY-XTRP-XNO     PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
