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
      * KEC 2016.0404 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASED CK-REC SIZE; CK-SIZE = 150.
      *         ADDED CK-LTOUCH-DATE.
      *         CHANGED DATES TO CCYYMMDD FORMAT; PIC 9(8) COMP-3.
      *         DATE FIELDS CK-CLEARED-DATE, CK-VOID-DATE AND
      *          CK-TRANSDATE ALREADY PIC 9(8) BUT IN 00MMDDYY FORMAT.
      *         REMOVED USE OF CK-CLEARED-DATE-ALP.
      *
      ******************************************************************
       78  CK-SIZE                    VALUE 150.
       01  CK-REC.
           03  CK-KEY-AREA.
               05  CK-LOC             PIC 9(4).
               05  CK-CKNO         PIC 9(6).
           03  CK1-KEY-AREA REDEFINES CK-KEY-AREA.
               05  CK1-KEY            PIC X(10).
           03  CK2-KEY-AREA REDEFINES CK-KEY-AREA.
               05  FILLER             PIC X(4).
               05  CK2-KEY            PIC X(6).
      *>>XDD DATE=YYYYMMDD
           03  CK-LTOUCH-DATE         PIC 9(8)     COMP-3.
           03  CK-VENDOR              PIC 9(9)     COMP-3.
      * GLF    REDEFINE CK-VENDOR WITH CK-INVSTOR
           03  CK-INVESTOR REDEFINES CK-VENDOR PIC 9(9)     COMP-3.
           03  CK-NAME                PIC X(25).
      *>>XDD DATE=YYYYMMDD
           03  CK-CHECKDATE           PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
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
      *>>XDD DATE=YYYYMMDD
           03  CK-CLEARED-DATE        PIC 9(8).
      *>>XDD DATE=YYYYMMDD
           03  CK-VOID-DATE           PIC 9(8).
           03  CK-CLASS               PIC 9(4).
           03  CK-ACCTNO              PIC 9(6).
           03  CK-SOURCE-CODE         PIC XX.
           03  CK-BLOC                PIC 9(4).
           03  FILLER                 PIC X(49).
