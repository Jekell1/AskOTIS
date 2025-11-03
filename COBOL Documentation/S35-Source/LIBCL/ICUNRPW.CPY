      * COPYMEMBER: LIBCL/ICUNRPW
      ******************************************************************
      *
      * ICUNRP SCREEN USER RANGES
      *
      *=================================================================
      * REVS:
      * 112189 SLC USE TO BE ICUNRP.CMD
      * 091499 GLF ADD INCPL FOR INCLUDE, SKIP, ONLY OF P&L ACCOUNTS
      *
      *    SCREEN FIELDS AND WORKERS
      *
      *    THESE FIELDS ARE PASSED TO:
      *                       ICUNR1.C
      *                       ICUNR2.C
      *
      * 010320 BAH CHANGED BEG-BR TO BEG-BRANCH FOR BRANCH RANGE SECURITY
      * 050721 BLF ADD OPTION TO SUPPRESS PRINTING BRANCH TOTALS,
      *            WORLD, PR# 413
      * BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * JKC 2022-0809 PR#1565 SECURITIZATION - ADDED FILE EXTRACTION
      *               ADDED EXPORT-FILE-YNO
      *
      ******************************************************************
           03  WH-INS             PIC 9.
           03  BEG-BRANCH         PIC 9(4).
           03  END-BRANCH         PIC 9(4).
           03  BEG-IC             PIC 99.
           03  END-IC             PIC 99.
           03  BEG-LC             PIC 9(3).
           03  END-LC             PIC 9(3).
           03  BEG-ST             PIC XX.
           03  END-ST             PIC XX.
           03  MMYY               PIC 9(4).
           03  MM-YY              REDEFINES MMYY.
               05  MM             PIC 9(2).
               05  YY             PIC 9(2).
           03  ZBAL-ONLY          PIC X.
           03  ACCOUNT-DETAIL     PIC X.
           03  SUMMARY-ONLY       PIC X.
           03  RBDAYS-MONTH       PIC 99.
           03  REBATE-METHOD9.
               05  REBATE-METHOD   PIC 9 OCCURS 5.
      * REBATE-METHOD(1) FOR CREDIT LIFE
      * REBATE-METHOD(2) FOR A & H
      * REBATE-METHOD(3) FOR OTHER/FIRE
      *       OR
      * REBATE-METHOD(1) FOR OTHER 1 INS
      * REBATE-METHOD(2) FOR OTHER 2 INS
      *        REBATE-METHOD = 1, RULE OF 78THS
      *        REBATE-METHOD = 2, PRO RATA
      *        REBATE-METHOD = 3, REFUND
           03  REBATE-METHODX REDEFINES REBATE-METHOD9.
               05  REBATE-M1       PIC X.
               05  REBATE-M2       PIC X.
               05  REBATE-M3       PIC X.
           03  REB1X               PIC X(20).
           03  REB2X               PIC X(20).
           03  REB3X               PIC X(20).
           03  ZERONLY             PIC X.
           03  GROUP-FLAG          PIC X(1).
           03  ENT-GROUP           PIC X(4).
      *-----------------------------------------------------
           03  REPORT-ENDING-DATE  PIC 9(8) COMP-3.
           03  PRIOR-ENDING-DATE   PIC 9(8) COMP-3.
           03  PL                  PIC X.
           03  PRINT-BRANCH-TOTS   PIC X.
           03  EXPORT-FILE-YNO     PIC X.
      *=================================================================
       01  SAVE-BRANCH-NUMBERS.
           03  SV-BEG-BR           PIC 9(4)  VALUE 9999.
           03  SV-END-BR           PIC 9(4)  VALUE 9999.
           03  SV-GROUP-FLAG       PIC X     VALUE SPACES.
           03  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
           03  CASE-BUF            PIC X(4)  VALUE SPACES.
