      * COPYMEMBER: LIBLP/LPFDTRJ.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [CLASS TRANSFERS WITHIN BRANCH]
      *
      * FILE INFO  :  CLASS TRANSFER WITHIN A BRANCH
      *               LP/TRJFILE
      *
      * TRJ1-KEY = TJ-BRNO,TJ-DATE,TJ-CLASS,TJ-ACCTNO,TJ-LAWCODE,TJ-LINE
      * TRJ2-KEY = TJ-DATE,TJ-BRNO,TJ-CLASS,TJ-ACCTNO,TJ-LAWCODE,TJ-LINE
      *
      *
      *=================================================================
      * REV:
      * MJD 062895 ADDED TJ-USERID, TJ-POSTDATE, AND TJ-POSTTIME.
      * JTG 092096 ADDED TJ-PLCD & TJ-PLTYPE, FOR CLASS TRANSFER OF P&L
      * JTG 030797 ADDED TJ-REPOCD, FOR CLASS TRANSFER OF REPO
      * BAH 052897 FIXED TJ-REFNO
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS 160914 REMOVED TJ-SEQNO
      *  CS 160915 CHANGED TJ-CLASS 9(2) TO 9(3), TJ-NUMBER IS REDEFINE
      *             OF TJ-ACCTNO 9(8), MADE TJ-REF-FRCL/TJ-REF-TOLC
      *             SEPARATE FIELDS CLASS 9(3) NOW
      *            TJ-LOANDATE 9(6) TO 9(8)
      *            TJ-MAINTFEE 9(3)V99 TO 9(5)V99
      *            REMOVED TJ-FEETBL 
      *            REMOVED TJ-INSTYPES,TJ-INSELIBIBLE AND TJ-INSTBL, ITS
      *             NOT UPDATED ON CLASS TRANSFER
      *            ADDED TJ-INSUEARN OCCURS 10 S9(7)V99 WHEN WE UPDATE
      *             INSURANCE EARNINGS TO LOAN RECORD
      *            CHANGED TJ-UNEARN OCCURS 6 TO 10 FOR NEW EARNINGS
      *            CHANGED TJ-DLRVPRIN,TJ-DLRVCHGS,TJ-DLAPRVPAID,
      *             TRJ-DLRVCHGS FROM 9(8)V99 TO 9(7)V99
      *            CHANGED TJ-POSTDATE 9(6) TO 9(8)
      *            REMOVE COMP FIELDS FROM KEY
      *            SAVED VERSION LIBLP/LPFDTRJ.160921
      *  CS 161117 ADD TJ-SUBCLASS
      *  CS 170713 CHANGED TJ-LINE TO PIC 9(3) TO MATCH WK2-LINE, IN A15
      *            WAS COMP 9(2) WHICH ALLOWED A VALUE OF 248, I DONT THINK
      *            WE ALLOWED LINE >99 BUT CHANGED TO 999. FILLER TO 4
      *            SAVED LPFDTRJ.170713
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  TRJ-SIZE                    VALUE 850.
       01  TRJ-REC.
           03  TRJ1-KEY.
               05  TJ-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TJ-DATE             PIC 9(8).
               05  TJ-CLASS            PIC 9(3).
               05  TJ-ACCTNO           PIC 9(8).
               05  TJ-LAWCODE          PIC 99.
               05  TJ-LINE             PIC 9(3).
      *-----------------------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  TJ-LTOUCH-DATE      PIC 9(8) COMP-3.

           03  TJ-BORRTBL          OCCURS 4 TIMES.
               05  TJ-SSNO         PIC 9(9)       COMP-3.
               05  TJ-MAKERCD      PIC X.
           03  TJ-REFNO.                            
               05  TJ-REF-FRLC     PIC 99.
               05  TJ-REF-X        PIC X.
               05  TJ-REF-TOLC     PIC 99.

      * NEEDS TO BE SEPARATE FIELDS SINCE CLASS IS 9(3)
           03  TJ-FRLC         PIC 9(3) COMP-3.
           03  TJ-TOLC         PIC 9(3) COMP-3.

           03  TJ-PURCD            PIC XXX.
           03  TJ-SRCD             PIC XX.
           03  TJ-COLLCD           PIC XX.
           03  TJ-SERVACCT         PIC X.
           03  TJ-LOANTYPE         PIC X.
           03  TJ-SUBCLASS         PIC 99.
      *-----------------------------------------------------------------

      $XFD DATE=YYYYMMDD
           03  TJ-LOANDATE         PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------
           03  TJ-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TJ-LNAMT            PIC S9(7)V99   COMP-3.
           03  TJ-FINCHG           PIC S9(7)V99   COMP-3.
           03  TJ-INTCHG           PIC 9(7)V99    COMP-3.
           03  TJ-SERCHG           PIC 9(5)V99    COMP-3.
           03  TJ-EXTCHG           PIC 9(5)V99    COMP-3.

           03  TJ-MAINTFEE         PIC 9(5)V99    COMP-3.
           03  TJ-ORGTERM          PIC 9(3)       COMP-3.
      *-----------------------------------------------------------------

           03  TJ-INSCODES.
               05  TJ-INSURED      PIC X.

      *NEW WHEN WE STORE INSURANCE UNEARNED
           03  TJ-INSUNEARN         PIC S9(7)V99 COMP-3 OCCURS 10.

      *-----------------------------------------------------------------

           03  TJ-DLNO             PIC 9(4)V99    COMP-3.
           03  TJ-DLRV             PIC S9(7)V99   COMP-3.
           03  TJ-DLSPRV           PIC S9(7)V99   COMP-3.
           03  TJ-DLDISC           PIC S9(7)V99   COMP-3.
           03  TJ-DLPARV           PIC S9(7)V99   COMP-3.
           03  TJ-DLRECOURSE       PIC X.
           03  TJ-DLPARTRCAMT      PIC 9(6)V99    COMP-3.
           03  TJ-PURBAL           PIC S9(7)V99   COMP-3.

           03  TJ-UNEARN           PIC S9(7)V99   COMP-3 OCCURS 10.
           03  TJ-PRLNNO           PIC 9(8)       COMP-3.
           03  TJ-DLRVPRIN         PIC S9(7)V99    COMP-3 OCCURS 3.
           03  TJ-DLRVCHGS         PIC S9(7)V99    COMP-3 OCCURS 3.
           03  TJ-DLPARVPAID       PIC S9(7)V99    COMP-3.
           03  TJ-DLPARVHELD       PIC S9(7)V99    COMP-3.
           03  TJ-USERID           PIC X(10).

      $XFD DATE=YYYYMMDD
           03  TJ-POSTDATE         PIC 9(8)        COMP-3.
           03  TJ-POSTTIME         PIC 9(6)        COMP-3.
           03  TJ-PLCD             PIC X.
           03  TJ-PLTYPE           PIC X.
           03  TJ-REPOCD           PIC X.
           03  FILLER              PIC X(531).
