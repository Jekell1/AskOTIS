      * COPYMEMBER: LIBCL/FEUNRPW
      ************************************************************************
      *
      *    SCREEN FIELDS AND WORKERS
      *
      *    THESE FIELDS ARE PASSED TO:
      *                       FEUNR1.C
      *
      * REVS:
      *  SLC 891121 SLC USE TO BE ICUNRP.CMD
      *  GLF 990914 GLF ADD INCPL FOR INCLUDE, SKIP, ONLY OF P&L ACCOUNTS
      *  BAH 010320 BAH CHANGED BEG-BR TO BEG-BRANCH FOR BRANCH RANGE SECURITY
      *  JTG 041110 *NEW*
      *             PROGRAMMED UNEARNED FEE CODE REPORT            LENDMARK 2387
      *  JTG 050113 ADDED 'LOAN ENTERED START DATE' TO REPORT      LENDMARK 2444
      *  BAH 170323 CHANGED DATES TO 9(8)
      **************************************************************************
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-LC              PIC 9(3).
           03  END-LC              PIC 9(3).
           03  MMYY                PIC 9(4).
           03  MM-YY              REDEFINES MMYY.
               05  MM              PIC 9(2).
               05  YY              PIC 9(2).
           03  ENTERED-START-DATE  PIC 9(8).
           03  ZBAL-ONLY           PIC X.
           03  ACCOUNT-DETAIL      PIC X.
           03  SUMMARY-ONLY        PIC X.
           03  RBDAYS-MONTH        PIC 99.
           03  FEE-CODE            PIC X(2) OCCURS 3.
           03  REBATE-METHOD9.
               05  REBATE-METHOD   PIC 9(2) OCCURS 3.
      *              1-  RULE OF 78THS
      *              2-  PRORATA
      *              3-  REFUND METHOD (SPR)
      *              4-  RULE 78THS CASH-7
      *              5-  RULE 78THS CASH-8
      *              6-  78THS/PRORATA AVG
      *              7-  ACTUARIAL ACCRUAL
      *              9-  SECURITY FINANCIAL
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
      *=================================================================
       01  SAVE-BRANCH-NUMBERS.
           03  SV-BEG-BR           PIC 9(4)  VALUE 9999.
           03  SV-END-BR           PIC 9(4)  VALUE 9999.
           03  SV-GROUP-FLAG       PIC X     VALUE SPACES.
           03  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
           03  CASE-BUF            PIC X(4)  VALUE SPACES.
