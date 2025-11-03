      * COPYMEMBER: LIBNP/NPFDTRA
      *************************************************
      *                                                
      *     (DAILY NOTES PAYABLE TRANSACTION RECORD)   
      *
      *           CODE = "A"
      *
      *            --- RECORD LENGTH 218 ---           
      *                                                
      *  REV:
      *  MJD 091698 ADDED POSTTIME, POSTDATE, POST-BR AND USERID.
      *  BLV 980923 ADDED TRA-CASH-DRAWER-NO
      *  BLV 980929 ADDED TRA-OTHBR-FG
      **************************************************
       01  TRA-REC.
           03  TRA-KEY.
               05  TRA-DAY              PIC 99.
               05  TRA-BRNO             PIC 9(4).
               05  TRA-CODE             PIC X.
               05  TRA-CLASS            PIC 99.
               05  TRA-ACCTNO           PIC 9(6).
               05  TRA-LINE             PIC 9(6).
               05  FILLER               PIC X(6).
           03  TRA-UPD-FLAG             PIC X.     
           03  TRA-DATA.
               05  TRA-TRDATE          PIC 9(6)      COMP-3.
               05  TRA-TRCD            PIC X(2).
               05  TRA-REFNO           PIC X(5).
               05  TRA-TRAMT           PIC S9(9)V99  COMP-3.
               05  TRA-NOTERATE        PIC 99V99     COMP-3.
               05  TRA-CURFG           PIC X.
               05  TRA-PREVBAL         PIC S9(9)V99  COMP-3.
               05  TRA-ACCRUAL         PIC S9(9)V99  COMP-3.
               05  TRA-ACCRDATE        PIC 9(6)      COMP-3.
               05  TRA-LASTPMT         PIC 9(6)      COMP-3.
               05  TRA-NOTEBAL         PIC S9(9)V99  COMP-3.
               05  TRA-GLCASH          PIC S9(9)V99  COMP-3.
               05  TRA-GLACCRUE        PIC S9(9)V99  COMP-3.
               05  TRA-GLEXPENSE       PIC S9(9)V99  COMP-3.
               05  TRA-GLNTPAY         PIC S9(9)V99  COMP-3.
               05  TRA-GLMISC          OCCURS 2.
                   07  TRA-GLNO        PIC 9(11)     COMP-3.
                   07  TRA-GLAMT       PIC S9(9)V99  COMP-3.
               05  TRA-REALTRCD        PIC X(2).
               05  TRA-PAYDATE         PIC 9(6)      COMP-3.
               05  TRA-CASH-DRAWER-NO  PIC X.
               05  TRA-USERID          PIC X(10).
               05  TRA-POSTDATE        PIC 9(8)      COMP-3.
               05  TRA-POSTTIME        PIC 9(6)      COMP-3.
               05  TRA-POST-BRNO       PIC 9(4)      COMP-3.
               05  TRA-OTHBR-FG        PIC X.
               05  FILLER              PIC X(65).
