      * COPYMEMBER: LIBNP/NPFDTRN
      *************************************************
      *                                                
      *     (DAILY NOTES PAYABLE TRANSACTION RECORD)   
      *
      *           CODE = "N"
      *
      *            --- RECORD LENGTH 218 ---           
      *                                                
      *  REV:
      *  MJD 091698 ADDED POSTTIME, POSTDATE, POST-BR AND USERID.
      *  BLV 980929 ADDED TRA-OTHBR-FG
      *  GLF 981102 ADDED TRN-CASH-DRAWER-NO
      *  GLB 981221 ADDED TRN-NC-SEQNO, ADJUSTED REC SIZE
      **************************************************
       01  TRN-REC.
           03  TRN-KEY.
               05  TRN-DAY              PIC 99.
               05  TRN-BRNO             PIC 9(4).
               05  TRN-COMMON-KEY.
                   07  TRN-CODE             PIC X.
                   07  TRN-CLASS            PIC 99.
                   07  TRN-NC-SEQNO         PIC 99.
                   07  TRN-ACCTNO           PIC 9(6).
                   07  TRN-LINE             PIC 9(6).
                   07  FILLER               PIC X(6).
           03  TRN-UPD-FLAG             PIC X.
           03  TRN-DATA.
               05  TRN-IVTBL           OCCURS 4.
                   07  TRN-SSNO        PIC 9(9)     COMP-3.
               05  TRN-ISSDATE         PIC 9(6)      COMP-3.
               05  TRN-MATUDATE        PIC 9(6)      COMP-3.
               05  TRN-LASTPMT         PIC 9(6)      COMP-3.
               05  TRN-ACCRDATE        PIC 9(6)      COMP-3.
               05  TRN-NOSHARES        PIC 9(8)      COMP-3.
               05  TRN-NOTEAMT         PIC S9(9)V99  COMP-3.
               05  TRN-NOTEBAL         PIC S9(9)V99  COMP-3.
               05  TRN-PREVBAL         PIC S9(9)V99  COMP-3.
               05  TRN-ACCRUAL         PIC S9(9)V99  COMP-3.
               05  TRN-EXPYTD          PIC S9(9)V99  COMP-3.
               05  TRN-INTWHELD        PIC S9(9)V99  COMP-3.
               05  TRN-NOTERATE        PIC 99V99     COMP-3.
               05  TRN-FACERATE        PIC 99V99     COMP-3.
               05  TRN-YRLYPMTS        PIC 9(2)      COMP-3.
      *        HOLD INTEREST CHECK ?
               05  TRN-INTCKCD         PIC X.
      *        OLD NOTE RETURNED ?
               05  TRN-NTRETFG         PIC X.
      *        F-FULL, P-PARTIAL, N-NONE
               05  TRN-INTPYCD         PIC X.
      *        PARTIAL INTEREST PMTS
               05  TRN-PARTAMT         PIC 9(6)V99   COMP-3.
               05  TRN-PARTPCT         PIC 99V99     COMP-3.
               05  TRN-REALTRCD        PIC X(2).
               05  TRN-USERID          PIC X(10).
               05  TRN-POSTDATE        PIC 9(8)      COMP-3.
               05  TRN-POSTTIME        PIC 9(6)      COMP-3.
               05  TRN-POST-BR         PIC 9(4)      COMP-3.
               05  TRN-CASH-DRAWER-NO  PIC X.
               05  FILLER              PIC X(67).

