      * COPYMEMBER: LIBLP/LP01BX
      **************************************************************************
      *            ( BORROWER TRAILER FILE )
      *                     LP01BX
      *  RSZ 300
      *
      * REV:
      *  BAH 030331 NEW, LENDMARK PR# 2044
      *  BAH 030519 ADDED BX-ALT ADDRESS FIELDS REGACC PR# 27
      *  MJD 061107 ADDED BX-KYC-??? FIELDS (113 BYTES TOTAL)
      *             REGENCY "KNOW YOUR CUSTOMER" PROGRAM SCREEN FIELDS PR#2856
      **************************************************************************
       78  BX-SIZE                     VALUE 300.
       01  BX-REC.
           03  BX1-KEY.
               05  BX-SSNO             PIC 9(9).

           03  BX-EMAIL                PIC X(40).

           03  BX-SCORE-TABLE.
               05  BX-CREDIT-SCORE     PIC 9(4) OCCURS 4.
               05  BX-BNKRPT-SCORE     PIC 9(4) OCCURS 4.

           03  BX-ALT-ADDRESS.
               05  BX-ALT-ADR1             PIC X(24).
               05  BX-ALT-ADR2             PIC X(24).
               05  BX-ALT-CITY             PIC X(15).
               05  BX-ALT-STATE            PIC X(2).
               05  BX-ALT-ZIP              PIC X(10).

           03  BX-KNOW-YOUR-CUSTOMER.
               05  BX-KYC-ADR1         PIC X(24).
               05  BX-KYC-ADR2         PIC X(24).
               05  BX-KYC-CITY         PIC X(15).
               05  BX-KYC-STATE        PIC X(2).
               05  BX-KYC-ZIP          PIC X(10).
               05  BX-KYC-IDTYPE       PIC X(2).
                   88  BX-KYC-IDTYPE-VALID VALUE "A" "C" "D" "O"
                                                 "P" "R" "T".
               05  BX-KYC-ISSAUTH      PIC X(2).
               05  BX-KYC-NUMBER       PIC X(25).
               05  BX-KYC-ISSDTE       PIC 9(6) COMP.
               05  BX-KYC-EXPDTE       PIC 9(6) COMP.
               05  BX-KYC-LST-VER-DTE  PIC 9(6) COMP.

           03  FILLER                  PIC X(31).
