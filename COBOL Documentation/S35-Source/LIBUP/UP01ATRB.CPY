      * COPYMEMBER: LIBLP/LPFDTRB.CPY
      ******************************************************************
      *
      * DESCRIPTION:  TRANSACTION FILE [BRANCH (BULK) TRANSFERS]
      *
      * 2020.05.08 BAH REMOVED ATB-REFNO AND PUT TAB BETWEEN TB-REF-FRTO
      *                AND TB-REFBR
      * 20220524 BAH ADDED ATB-ACTIONCD2
      ******************************************************************
       78  ATRB-SIZE                    VALUE 668.
       01  ATRB-REC.
           03  ATB-BRNO             PIC 9(4).
           03  FILLER               PIC X.
           03  ATB-DATE             PIC 9(8).
           03  FILLER               PIC X.
           03  ATB-CLASS            PIC 9(3). 
           03  FILLER               PIC X.
           03  ATB-ACCTNO           PIC 9(8).
           03  FILLER               PIC X.
           03  ATB-LAWCODE          PIC 99.
           03  FILLER               PIC X.
           03  ATB-LINE             PIC 9(3).
           03  FILLER               PIC X.
           03  ATB-LTOUCH-DATE      PIC 9(8).
           03  FILLER               PIC X.
           03  ATB-BORRTBL        OCCURS 4 TIMES.
               05  ATB-SSNO         PIC 9(9).
               05  FILLER           PIC X.
               05  ATB-MAKERCD      PIC X.
               05  FILLER           PIC X.
           03  ATB-REF-FRTO         PIC X.
           03  FILLER               PIC X.
           03  ATB-REFBR            PIC 9999.
           03  FILLER               PIC X.
           03  ATB-PURCD            PIC XXX.
           03  FILLER               PIC X.
           03  ATB-SRCD             PIC XX.
           03  FILLER               PIC X.
           03  ATB-COLLCD           PIC XX.
           03  FILLER               PIC X.
           03  ATB-SERVACCT         PIC X.
           03  FILLER               PIC X.
           03  ATB-LOANTYPE         PIC X.
           03  FILLER               PIC X.
           03  ATB-SUBCLASS         PIC 99.
           03  FILLER               PIC X.
           03  ATB-LOANDATE         PIC 9(8).
           03  FILLER               PIC X.
           03  ATB-PROCEEDS         PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-LNAMT            PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-FINCHG           PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-INTCHG           PIC 9(7).99.
           03  FILLER               PIC X.
           03  ATB-SERCHG           PIC 9(5).99.
           03  FILLER               PIC X.
           03  ATB-EXTCHG           PIC 9(5).99.
           03  FILLER               PIC X.
           03  ATB-MAINTFEE         PIC 9(5).99.
           03  FILLER               PIC X.
           03  ATB-ORGTERM          PIC 9(3).
           03  FILLER               PIC X.
           03  ATB-INSURED          PIC X.
           03  FILLER               PIC X.
           03  ATB-INSUNEARN-TABLE OCCURS 10.
               05  ATB-INSUNEARN    PIC +9(7).99.
               05  FILLER           PIC X.
           03  ATB-DLNO             PIC 9(4).99.
           03  FILLER               PIC X.
           03  ATB-DLRV             PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-DLSPRV           PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-DLDISC           PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-DLPARV           PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-DLRECOURSE       PIC X.
           03  FILLER               PIC X.
           03  ATB-DLPARTRCAMT      PIC 9(7).99.
           03  FILLER               PIC X.
           03  ATB-PURBAL           PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-UNEARN-TABLE    OCCURS 10 TIMES.
               05  ATB-UNEARN       PIC +9(7).99.
               05  FILLER           PIC X.
           03  ATB-REF-ACCTNO       PIC 9(8).
           03  FILLER               PIC X.
           03  ATB-DLRVPRIN-TABLE  OCCURS 3 TIMES.
               05  ATB-DLRVPRIN     PIC +9(8).99.
               05  FILLER           PIC X.
           03  ATB-DLRVCHGS-TABLE  OCCURS 3 TIMES.
               05  ATB-DLRVCHGS     PIC +9(7).99.
               05  FILLER           PIC X.
           03  ATB-DLPARVPAID       PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-DLPARVHELD       PIC +9(7).99.
           03  FILLER               PIC X.
           03  ATB-USERID           PIC X(10).
           03  FILLER               PIC X.
           03  ATB-POSTDATE         PIC 9(8).
           03  FILLER               PIC X.
           03  ATB-POSTTIME         PIC 9(6).
           03  FILLER               PIC X.
           03  ATB-PLCD             PIC X.
           03  FILLER               PIC X.
           03  ATB-PLTYPE           PIC X.
           03  FILLER               PIC X.
           03  ATB-REPOCD           PIC X.
           03  FILLER               PIC X.
           03  ATB-ACTIONCD2        PIC X(4).
           03  FILLER               PIC X.
           03  ATB-CR              PIC X.
           03  ATB-LF              PIC X.
