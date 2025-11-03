      * COPYMEMBER: LIBLP/LP01CI
      **************************************************************************
      *            ( CUSTOMER INFORMATION FILE )
      *                     LP01CI
      *                 LOAN TRAILER WITH BORROWER INFO
      *  RSZ 500
      *
      * REV:
      *  BAH 110126 NEW, WORLD PR# 739
      *  BLM 110224 REMOVED CENTS FROM ALL AMOUNTS.  REMOVE FIELD CI-LESS-RENT, 
      *             THIS IS THE SAME AS CI-MONTHLY-RENT-MTG.  REMOVED FIELD
      *             CI-FREE-INCOME, THIS IS JUST A FIELDS COMPUTED & DISPLAYED
      *             ON THE SCREEN.  WORLD PR# 739 
      **************************************************************************
       78  CI-SIZE                     VALUE 500.
       01  CI-REC.
           03  CI1-KEY.
               05  CI-ACCTNO           PIC 9(8).
               05  CI-ACCTNOX REDEFINES CI-ACCTNO.
                   07 CI-NUMBER        PIC 9(6).
                   07 CI-SEQNO         PIC 99.
           03  CI2-KEY.
               05  CI-SSNO             PIC 9(9).
           03  CI3-KEY.
               05  CI-SSNO-2           PIC 9(9).
      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           03  CI-DATE-UPDATED         PIC 9(8)  COMP-6.
           03  CI-AMT-REQUESTED        PIC S9(7) COMP-3.
           03  CI-MARITAL-STAT         PIC X.
           03  CI-YEARS-IN-AREA        PIC 99.
      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           03  CI-DATE-AT-PRESENT-ADR  PIC 9(8)  COMP-6.
           03  CI-MONTHLY-RENT-MTG     PIC S9(7) COMP-3.
           03  CI-FAIR-MARKET-VALUE    PIC S9(7) COMP-3.
           03  CI-MTG-BALANCE          PIC S9(7) COMP-3.
           03  CI-CHECKING-ACCOUNT     PIC X.
           03  CI-SAVINGS-ACCOUNT      PIC X.
           03  CI-TOTAL-MO-INCOME      PIC S9(7) COMP-3.
           03  CI-LESS-MO-PAY          PIC S9(7) COMP-3.

      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           03  CI-DATE-HIRE-1          PIC 9(8)  COMP-6.
           03  CI-POSITION-CODE-1      PIC X(5).
           03  CI-GROSS-MO-INCOME-1    PIC S9(7) COMP-3.

      * MMDDYYYY TRULY STORED AS 8 DIGIT DATE
           03  CI-DATE-HIRE-2          PIC 9(8)  COMP-6.
           03  CI-POSITION-CODE-2      PIC X(5).
           03  CI-GROSS-MO-INCOME-2    PIC S9(7) COMP-3.

           03  FILLER                  PIC X(411).
