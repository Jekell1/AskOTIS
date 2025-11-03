      * COPYMEMBER: LIBLP/LPFDTRG
      **************************************************************************
      *               TR-CODE  =  "G"                  
      *                  PAYMENTS
      * REV:
      *  MJD 010115 NEW COPY MEMBER (PR#1375)
      **************************************************************************
       01  TG-REC.
           03  TG-TRP-KEY.
               05  TG-DAY              PIC 99     COMP.
               05  TG-BRNO             PIC 9(4)   COMP.
               05  TG-CODE             PIC X.
               05  TG-CLASS            PIC 99     COMP.
               05  TG-ACCT.
                   07  TG-NUMBER       PIC 9(6)   COMP.
                   07  TG-SEQNO        PIC 99     COMP.
               05  TG-LINE             PIC 99     COMP.
               05  TG-FILLER2          PIC X(4).
           03  TG-TRF-KEY  REDEFINES TG-TRP-KEY.
               05  FILLER              PIC X(4).
               05  TGF-POOLID          PIC 9(5)   COMP.
               05  TGF-CLASS           PIC 99     COMP.
               05  TGF-NUMBER          PIC 9(6)   COMP.
               05  TGF-SEQNO           PIC 99     COMP.
               05  TGF-LINE            PIC 99     COMP.
               05  TGF-SIGN            PIC S9     COMP-3.
      *-------------------------------------------------
           03  TG-TRCD             PIC  X(02).
           03  TG-TRDATE           PIC  9(08)    COMP-6.
           03  TG-INTPDDT          PIC  9(08)    COMP-6.
           03  TG-CURBAL           PIC S9(07)V99 COMP-3.
           03  TG-DLNO             PIC  9(04)V99 COMP-6.
           03  TG-SUBCLASS         PIC  9(02)    COMP-6.
           03  TG-LAWCODE          PIC  9(02)    COMP-6.
           03  TG-SSNO             PIC  9(09)    COMP-6.
      *    03  TG-DELINQ           PIC  X(04).
           03  TG-DELINQ           PIC S9(07)    COMP-3.
           03  TG-INTEARN          PIC S9(07)V99 COMP-3.
           03  TG-ADDEARN          PIC S9(07)V99 COMP-3.
           03  TG-SVCEARN          PIC S9(07)V99 COMP-3.
           03  TG-MFEARN           PIC S9(07)V99 COMP-3.
           03  TG-DISEARN          PIC S9(07)V99 COMP-3.
           03  TG-OTHEARN          PIC S9(07)V99 COMP-3.
           03  TG-INT-TM           PIC S9(07)V99 COMP-3.
           03  TG-ADD-TM           PIC S9(07)V99 COMP-3.
           03  TG-SVC-TM           PIC S9(07)V99 COMP-3.
           03  TG-MF-TM            PIC S9(07)V99 COMP-3.
           03  TG-DIS-TM           PIC S9(07)V99 COMP-3.
           03  TG-OTH-TM           PIC S9(07)V99 COMP-3.
           03  TG-LAST-ACCRU-AMT   PIC S9(07)V99 COMP-3.
           03  TG-INTOWE           PIC S9(07)V99 COMP-3.
           03  TG-TRUE-ASSESS      PIC S9(07)V99 COMP-3.
           03  TG-ACC-AMT          PIC S9(07)V99 COMP-3.
           03  TG-BEGINNING        PIC S9(07)V99 COMP-3.
           03  TG-NONACCRU         PIC S9(07)V99 COMP-3.
           03  TG-ENDING           PIC S9(07)V99 COMP-3.
           03  TG-MAX              PIC S9(07)V99 COMP-3.
           03  TG-STOP-FG          PIC X(01).
      *    -------------------------------------------------------------
           03  TG-POSTDATE         PIC  9(08)    COMP-6.
           03  TG-POSTTIME         PIC  9(08)    COMP-6.
           03  TG-USERID           PIC  X(10).
           03  FILLER              PIC X(388).
