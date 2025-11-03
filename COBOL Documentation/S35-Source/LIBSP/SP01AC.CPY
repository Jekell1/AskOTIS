      * COPYMEMBER: LIBSP/SP01AC
      *****************************************************************
      *             ACH DEPOSIT (DAILY) FILE 
      *
      *                     (SP)
      *
      * REV:
      * MJD 082897 ADDED AC-SETTLEMENT-DATE FOR WORLD
      *            (DID NOT CHANGE RECORD SIZE)
      * BLF 060316 ADDED AC-PAYMENT-AMOUNT, REDEF OF AC-DEPOSIT-AMOUNT
      *            LENDMARK PR# 2711
      * BLM 120111 ADDED ALPHA REDEFINITION OF AC-COMPANY-PREFIX &
      *            AC-COMPANY-PREFIX2, TRIST PR# 3944
      *****************************************************************
       01  AC-REC.
           03  AC-RECORD-AREA                  PIC X(94).
      * TRANSMITTER CONTROL-HEADER FOR COMMUNICATIONS
           03  AC-REC-TRANSMITTER-CONTROL REDEFINES AC-RECORD-AREA.
               05  AC-LOGIN-INFO               PIC X(94).
           03  AC-REC-01-AREA REDEFINES AC-RECORD-AREA.
      * RECORD TYPE = 1  (FILE HEADER)
               05  AC-REC-TYPE                 PIC 9.
      * PRIORITY CODE = 01
               05  AC-PRIORITY-CODE            PIC 99.
      * IMMEDIATE DEST & ORIGIN - BOTH CORPORATE ROUTING #
               05  FILLER                      PIC X.
               05  AC-DESTINATION              PIC 9(9).
               05  AC-ORIGIN-ROUTING.
                   07  FILLER                      PIC X.
                   07  AC-ORIGIN                   PIC 9(9).
      * BANKONE WANTS TAXID IN ORIGIN, NOT ROUTING #
               05  AC-ORIGIN-TAXID REDEFINES AC-ORIGIN-ROUTING.
                   07  AC-ORIGIN-PREFIX        PIC 9.
                   07  AC-ORIGIN-TAX-ID        PIC 9(9).
               05  AC-CREATE-DATE-YYMMDD       PIC 9(6).
               05  AC-CREATE-TIME-HHMM         PIC 9(4).
      * MODIFIER - BLANK (UNLESS 2 FILES ON SAME DATE?)
               05  AC-FILE-MODIFIER            PIC X.
      * RECORD SIZE = 94
               05  AC-REC-SIZE                 PIC 999.
      * BLOCKING FACTOR = 10
               05  AC-BLOCKING                 PIC 99.
      * FORMAT CODE = 1
               05  AC-FORMAT-CODE              PIC 9.
      * DEST & ORIGIN NAME - CORP. BANK NAME
               05  AC-DESTINATION-NAME         PIC X(23).
               05  AC-ORIGIN-NAME              PIC X(23).
               05  FILLER                      PIC X(8).

           03  AC-REC-05-AREA REDEFINES AC-RECORD-AREA.
      * RECORD TYPE = 5  (COMPANY/BATCH HEADER)
               05  FILLER                      PIC X.
      * SERVICE CLASS = 200 (MIXED DEBITS & CREDITS)
               05  AC-SERVICE-CLASS            PIC 999.
      * COMPANY-NAME = CORPORATE NAME
               05  AC-COMPANY-NAME             PIC X(16).
      * DISCRETIONARY DATE - OPTIONAL
               05  AC-DISCRETIONARY-DATA       PIC X(20).
      * COMPANY ID - FEDERAL TAX ID WITH A 1 IN LEFTMOST DIGIT
               05  AC-COMPANY-ID.
                   07  AC-COMPANY-PREFIX           PIC 9.
                   07  AC-COMPANY-PREFIX-ALP REDEFINES
                       AC-COMPANY-PREFIX           PIC X.
                   07  AC-FED-TAX-ID               PIC 9(9).
      * STANDARD ENTRY CLASS CODE = CCD
               05  AC-ENTRY-CLASS              PIC X(3).
      * COMPANY ENTRY DESC = CASH C&D
               05  AC-COMPANY-ENTRY-DESC       PIC X(10).
      * COMPANY DESC DATE - OPTIONAL
               05  AC-COMPANY-DESC-DATE        PIC 9(6).
      * EFFECTIVE ENTRY DATE - DAY AFTER TRANSMISSION
      * EG. THURS BUSINESS, XMIT FRIDAY W/DATE OF MONDAY
               05  AC-EFFECTIVE-ENTRY-DATE     PIC 9(6).
      * WORLD: FIRST UNION - MANDATORY ZEROS IN SETTLEMENT DATE
               05  AC-SETTLEMENT-DATE          PIC 9(3).
      * ORIGINATOR STATUS CODE = 1
               05  AC-ORIG-STATUS              PIC 9.
      * ORIGINING DFI - CORPORATE BANK ROUTING
               05  AC-BANK-IDENTIFICATION      PIC 9(8).
      * BATCH NUMBER = 1
               05  AC-BATCH-NUMBER             PIC 9(7).

           03  AC-REC-06-AREA REDEFINES AC-RECORD-AREA.
      * RECORD TYPE = 6  (ENTRY DETAIL RECORD)
               05  FILLER                      PIC X.
      * TRANSACTION CODE:  27 FOR DEBITS FROM BRANCHES
      *                    22 FOR OFFSETTING CREDIT
               05  AC-TRAN-CODE                PIC 99.
      * ROUTING & BANK ACCT # FOR BRANCHES FOR TYPE 27'S
               05  AC-BANK-RT.
                   07  AC-BANK-RT-NO           PIC 9(8).
                   07  AC-BANK-RT-CD           PIC 9.
               05  AC-BANK-RT-NUMBER REDEFINES AC-BANK-RT
                                               PIC 9(9).
               05  AC-BANK-ACCOUNT-NUMBER      PIC X(17).
               05  AC-DEPOSIT-AMOUNT           PIC 9(8)V99.
               05  AC-PAYMENT-AMOUNT REDEFINES AC-DEPOSIT-AMOUNT
                                               PIC 9(8)V99.
      * IDENTIFICATION NUMBER - OPTIONAL
               05  AC-ID-NUMBER                PIC X(15).
               05  AC-NAME                     PIC X(22).
      * DISCRETIONARY DATE - OPTIONAL
               05  AC-DISC-DATA                PIC XX.
      * ADDENDA INDICATOR: 0 FOR NO ADDENDA
               05  AC-ADDENDA-REC-IND          PIC 9.
      * TRACE NUMBER - OPTIONAL
               05  AC-TRACE-PREFIX             PIC 9(8).
               05  AC-TRACE-NUMBER             PIC 9(7).

           03  AC-REC-08-AREA REDEFINES AC-RECORD-AREA.
               05  FILLER                      PIC X(4).
               05  AC-ENTRY-COUNT              PIC 9(6).
               05  AC-ENTRY-HASH               PIC 9(10).
               05  AC-TOTAL-DEBIT              PIC 9(10)V99.
               05  AC-TOTAL-CREDIT             PIC 9(10)V99.
      * COMPANY ID - FEDERAL TAX ID WITH A 1 IN LEFTMOST DIGIT
               05  AC-COMPANY-ID2.
                   07  AC-COMPANY-PREFIX2      PIC 9.
                   07  AC-COMPANY-PREFIX2-ALP REDEFINES
                       AC-COMPANY-PREFIX2      PIC X.
                   07  AC-FED-TAX-ID2          PIC 9(9).
               05  FILLER                      PIC X(40).

           03  AC-REC-09-AREA REDEFINES AC-RECORD-AREA.
               05  FILLER                      PIC X.
               05  AC-BATCH-COUNT              PIC 9(6).
               05  AC-BLOCK-COUNT              PIC 9(6).
               05  AC-09-ENTRY-COUNT           PIC 9(8).
               05  AC-09-ENTRY-HASH            PIC 9(10).
               05  AC-09-TOTAL-DEBIT           PIC 9(10)V99.
               05  AC-09-TOTAL-CREDIT          PIC 9(10)V99.
               05  FILLER                      PIC X(39).
           03  AC-CR                           PIC X.
           03  AC-LF                           PIC X.
