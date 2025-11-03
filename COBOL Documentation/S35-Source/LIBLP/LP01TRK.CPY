      * COPYMEMBER: LIBLP/LPFDTRK.CPY
      ******************************************************************
      *
      * DESCRIPTION:  TRANSACTION FILE [CONVERSIONS]
      *
      * FILE INFO  :  CONVERSIONS "K"
      *               LP/TRKFILE
      *
      * TRK1-KEY = TK-BRNO,TK-DATE,TK-CLASS,TK-ACCTNO,TK-LAWCODE,TK-LINE
      * TRK2-KEY = TK-DATE,TK-BRNO,TK-CLASS,TK-ACCTNO,TK-LAWCODE,TK-LINE
      *
      *
      *=================================================================
      * REV:
      * MJD 062895 ADDED TK-USERID, TK-POSTDATE, AND TK-POSTTIME.
      * JTG 092096 ADDED TK-PLCD & TK-PLTYPE, FOR BULK TRANSFER OF P&L
      * JTG 080129 CHANGED FOR JUDY/CS/JOHN TO ALLOW BULK-CONVERSION-FG
      *             TO TAKE UPFRONT EARNINGS 'ADDED TK-EARNED'
      *            FINAUTO    #3141
      *  CS 160916 ADDED TK-ACCTNO 9(8) AND MADE TK-NUMBER  REDEFINE
      *            REMOVED TK-SEQNO
      *            CHANGED  TK-LOANDATE 9(6) TO 9(8)
      *                     TK-MAINTFEE 999V99 TO 9(5)V99 
      *            ADD  TK-INSEARNED        OCCURS 10 PIC S9(7)V99.
      *                 TK-INSUNEARN        OCCURS 10 PIC S9(7)V99.
      *            CHANGED TK-DLRVPRIN, TK-DLRVCHGS, TK-DLPARVPAID
      *             TK-DLPARVHELD   FROM S9(8)V99 TO S9(7)V99
      *             TO MATCH LN
      *            CHANGED TK-FILLER2 X(3) TO X(5)
      *            SAVED VERSION LIBLP/LPFDTRK.160921
      * CS 161117 ADD TK-SUBCLASS
      *  CS 170713 CHANGED TK-LINE TO PIC 9(3) TO MATCH WK2-LINE, IN A15
      *            WAS COMP 9(2) WHICH ALLOWED A VALUE OF 248, I DONT THINK
      *            WE ALLOWED LINE >99 BUT CHANGED TO 999. FILLER TO 4
      *            SAVED LPFDTRL.170713
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      *  CS 180524 ADDED REDEFINE OF TK-ACCTNO AS TK-NUMBER
      * BAH 180628 REMOVED TK-NUMBER REDEFINE
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20200508 REMOVED TK-INSCODES AND MOVED TK-INSURED TO 03
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  TRK-SIZE                    VALUE 850.
       01  TRK-REC.
           03  TRK1-KEY.
               05  TK-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TK-DATE             PIC 9(8). 
               05  TK-CLASS            PIC 9(3).
               05  TK-ACCTNO           PIC 9(8).
               05  TK-LAWCODE          PIC 99.
               05  TK-LINE             PIC 9(3).
      *-----------------------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  TK-LTOUCH-DATE          PIC 9(8)       COMP-3.
           03  TK-BORRTBL          OCCURS 4.
               05  TK-SSNO             PIC 9(9)       COMP-3.
               05  TK-MAKERCD          PIC X.
      *-----------------------------------------------------------------
      *   TK-REF-FRTO --> T : TRANSFERRED TO ANOTHER BRANCH.
      *                   F : TRANSFERRED FROM ANOTHER BRANCH.
      *-----------------------------------------------------------------
           03  TK-REFNO.
               05  TK-REF-FRTO         PIC X.
               05  TK-REFBR            PIC 9999.
           03  TK-PURCD                PIC XXX.
           03  TK-SRCD                 PIC XX.
           03  TK-COLLCD               PIC XX.
           03  TK-SERVACCT             PIC X.
           03  TK-LOANTYPE             PIC X.
           03  TK-SUBCLASS             PIC 99.
           03  TK-REPOCD               PIC X.
           03  TK-PLCD                 PIC X.
           03  TK-PLTYPE               PIC X.
      $XFD DATE=YYYYMMDD
           03  TK-LOANDATE             PIC 9(8)       COMP-3.
           03  TK-PROCEEDS             PIC S9(7)V99   COMP-3.
           03  TK-LNAMT                PIC S9(7)V99   COMP-3.
           03  TK-FINCHG               PIC S9(7)V99   COMP-3.
           03  TK-INTCHG               PIC 9(7)V99    COMP-3.
           03  TK-SERCHG               PIC 9(5)V99    COMP-3.
           03  TK-EXTCHG               PIC 9(5)V99    COMP-3.
           03  TK-MAINTFEE             PIC 9(5)V99    COMP-3.
           03  TK-ORGTERM              PIC 9(3)       COMP-3.

           03  TK-INSURED              PIC X.

      *NEW PARALLEL THE FIELDS REGARDING NORMAL EARNINGS
           03  TK-INSUNEARN            PIC S9(7)V99 OCCURS 10.
           03  TK-INSEARNED            PIC S9(7)V99 OCCURS 10.

      *-----------------------------------------------------------------
           03  TK-DLNO                 PIC 9(4)V99    COMP-3.
           03  TK-DLRV                 PIC S9(7)V99   COMP-3.
           03  TK-DLSPRV               PIC S9(7)V99   COMP-3.
           03  TK-DLDISC               PIC S9(7)V99   COMP-3.
           03  TK-DLPARV               PIC S9(7)V99   COMP-3.
           03  TK-DLRECOURSE           PIC X.
           03  TK-DLPARTRCAMT          PIC 9(6)V99    COMP-3.
           03  TK-PURBAL               PIC S9(7)V99   COMP-3.
           03  TK-UNEARN               PIC S9(7)V99   COMP-3 OCCURS 10.
           03  TK-PRLNNO               PIC 9(8)       COMP-3.
           03  TK-DLRVPRIN             PIC S9(7)V99   COMP-3 OCCURS 3.
           03  TK-DLRVCHGS             PIC S9(7)V99   COMP-3 OCCURS 3.
           03  TK-DLPARVPAID           PIC S9(7)V99   COMP-3.
           03  TK-DLPARVHELD           PIC S9(7)V99   COMP-3.
           03  TK-USERID               PIC X(10).
      $XFD DATE=YYYYMMDD
           03  TK-POSTDATE             PIC 9(8)       COMP-3.
           03  TK-POSTTIME             PIC 9(6)       COMP-3.
           03  TK-EARNED               PIC S9(7)V99   COMP-3  OCCURS 10.
           03  FILLER                  PIC X(355).
