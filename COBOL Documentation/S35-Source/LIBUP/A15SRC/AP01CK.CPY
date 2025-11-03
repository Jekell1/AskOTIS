      * COPYMEMBER: LIBAP/AP01CK
      ******************************************************************
      *           ACCOUNTS PAYABLE CHECK FILE
      *           KEY1 = CHECK NUMBER
      * NOTE: THERE IS A COPY OF THIS FILE DESC IN NOTES
      *       PAYABLE - NP01APCK - IF YOU CHANGE THIS RECORD
      *       YOU NEED TO UPDATE THE FILE IN LIBNP!!!!!
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 1999.0312 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *
      ******************************************************************
       01  CK-REC.
           03  CK-KEY-AREA.
               05  CK-LOC             PIC 9(4).
               05  CK-CHECKNO         PIC 9(6).
           03  CK1-KEY-AREA REDEFINES CK-KEY-AREA.
               05  CK1-KEY            PIC X(10).
           03  CK2-KEY-AREA REDEFINES CK-KEY-AREA.
               05  FILLER             PIC X(4).
               05  CK2-KEY            PIC X(6).
      * GLB    EXPAND FROM 6 - 9   11/98
           03  CK-VENDOR              PIC 9(9)     COMP-3.
      * GLF    REDEFINE CK-VENDOR WITH CK-INVSTOR
           03  CK-INVESTOR REDEFINES CK-VENDOR PIC 9(9)     COMP-3.
           03  CK-NAME                PIC X(25).
      * GLB    MMDDYY
           03  CK-CHECKDATE           PIC 9(6)     COMP-3.
      * GLB    MMDDYY
           03  CK-TRANSDATE           PIC 9(8)     COMP-3.
           03  CK-TYPE                PIC X.
      *           S = SYSTEM CHECK
      *           M = MANUAL CHECK
      *           A = ALIGNMENT CHECK
      *           C = CONTINUATION CHECK
           03  CK-AMT                 PIC S9(9)V99 COMP-3.
           03  CK-CON-CHECK           PIC 9(6)     COMP-3.
           03  CK-VOID                PIC XX.
      *           V  = VOID
      *           VA = VOIDED THRU AUTO PROCESS
      *           R  = REPRINTED
           03  CK-CLEARED-FG          PIC X.
      * GLB    STILL MMDDYY           EXPAND FROM 6 - 8   11/98
           03  CK-CLEARED-DT-ALP.
               05  CK-CLEARED-DT      PIC 9(8).
           03  CK-VOID-DATE           PIC 9(8).
      * GLB    NEW FIELDS             12/98
           03  CK-CLASS               PIC 9(4).
           03  CK-ACCTNO              PIC 9(6).
           03  CK-SOURCE-CODE         PIC XX.
           03  CK-BLOC                PIC 9(4).
           03  CK-UNUSED              PIC X(22).
