      * COPYMEMBER: LIBNP/NP01IT
      **************************************************
      *       IT-FILE - INVESTOR TRAILER FILE
      *
      * ****** IF CHANGING THIS FILE ALSO CHAGE LIBNP/NPCHIT ******
      *
      * REV:
      * MJD 070110 NEW COPY MEMBER/FILE      REGENCY PR#2856
      * BAH 130520 ADDED IT-KYC-ASK-BUSINESS AND IT-KYC-ASK-DATE
      *                                      REGENCY PR#4265
      *  CS 160307 ADDED IT-KYC-3RD-PARTY-PAY, IT-KYC-2RD-PARTY-PAY-DATE
      *            IT-KYC-PRIVATE-ATM, IT-KYC-PRIVATE-AMT-DATE PR#50002
      **************************************************
       78  IT-SIZE                     VALUE 219.
       01  IT-REC.
           03  IT1-KEY.
               05  IT-SSNO             PIC 9(10)     COMP-3.
           03  IT-KNOW-YOUR-CUSTOMER.
               05  IT-KYC-ADR1         PIC X(24).
               05  IT-KYC-ADR2         PIC X(24).
               05  IT-KYC-CITY         PIC X(15).
               05  IT-KYC-STATE        PIC X(2).
               05  IT-KYC-ZIP          PIC X(10).
               05  IT-KYC-IDTYPE       PIC X(2).
                   88  IT-KYC-IDTYPE-VALID VALUE "A" "C" "D" "O"
                                                 "P" "R" "T".
               05  IT-KYC-ISSAUTH      PIC X(2).
               05  IT-KYC-NUMBER       PIC X(25).
               05  IT-KYC-ISSDTE       PIC 9(6) COMP.
               05  IT-KYC-EXPDTE       PIC 9(6) COMP.
               05  IT-KYC-LST-VER-DTE  PIC 9(6) COMP.
               05  IT-KYC-ASK-BUSINESS PIC X.
               05  IT-KYC-ASK-DATE     PIC 9(6) COMP.
               05  IT-KYC-3RD-PARTY-PAY-PROC PIC X.
               05  IT-KYC-3RD-PARTY-PAY-PC-DATE PIC 9(6) COMP.
               05  IT-KYC-PRIVATE-ATM  PIC X.
               05  IT-KYC-PRIVATE-ATM-DATE PIC 9(6) COMP.
           03  FILLER PIC X(88).
      *      03  FILLER                  PIC X(96).
