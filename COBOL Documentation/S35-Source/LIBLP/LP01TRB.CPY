      * COPYMEMBER: LIBLP/LPFDTRB.CPY
      ******************************************************************
      *
      * DESCRIPTION:  TRANSACTION FILE [BRANCH (BULK) TRANSFERS]
      *
      * FILE INFO  :  BRANCH TRANSFERS (BULKT2, BULKG2)
      *               LP/TRBFILE
      *
      * TRB1-KEY = TB-BRNO,TB-DATE,TB-CLASS,TB-ACCTNO,TB-LAWCODE,TB-LINE
      * TRB2-KEY = TB-DATE,TB-BRNO,TB-CLASS,TB-ACCTNO,TB-LAWCODE,TB-LINE
      *
      *=================================================================
      * REV:
      * MJD 062895 ADDED TB-USERID, TB-POSTDATE, AND TB-POSTTIME.
      * JTG 092096 ADDED TB-PLCD & TB-PLTYPE, FOR BULK TRANSFER OF P&L
      * JTG 030797 ADDED TB-REPOCD, FOR BULK TRANSFER OF REPO ACCOUNTS
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS 160913 REMOVED TB-SEQNO,CHANGED TB-CLASS TO 9(3),REDDEFINED 
      *            TB-NUMBER TO TB-ACCTNO, TB-LOANDATE 9(6) TO 9(8), 
      *            TB-MAINTFEE 9(3)V99 TO 9(5)V99,CHANGED FEE TABLE
      *            OCCURS 6 TO 20,TB-INSEXP 9(6) TO 9(8),TB-INSCOMM 9(6) TO
      *            9(5), ADDED TB-INSUNEARN OCCURS 10 TO STORE INSURANCE EARN
      *            CHANGED TB-DLPARTRCAMT 9(6) TO 9(7), TB-UNEARN OCCURS 6
      *            TO 10, TB-DLRVCHGS,TB-DLPARVPAID,TB-DLPARVHELD 9(8) TO
      *            9(7) TO MATCH LN
      *  CS 160914 COMMENTED OUT INSURANCE AND FEE CODES NOT UPDATED
      *  CS 160916 SAVED VERSION LPFDTRB.160920
      *  CS 161117 ADD TB-SUBCLASS PER KM
      *  CS 170713 CHANGED TB-LINE TO PIC 9(3) TO MATCH WK2-LINE, IN A15
      *            WAS COMP 9(2) WHICH ALLOWED A VALUE OF 248, I DONT THINK
      *            WE ALLOWED LINE >99 BUT CHANGED TO 999. FILLER TO 4
      *            SAVED LPFDTRB.170713
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210423 REMOVED FUTURE FIELDS
      * BAH 20220420 ADD TB-ACTIONCD, SECURITIZATION #1551
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  TRB-SIZE                    VALUE 850.
       01  TRB-REC.
           03  TRB1-KEY.
               05  TB-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TB-DATE             PIC 9(8).
               05  TB-CLASS            PIC 9(3). 
               05  TB-ACCTNO           PIC 9(8).
               05  TB-LAWCODE          PIC 99.
               05  TB-LINE             PIC 9(3).

      *-----------------------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  TB-LTOUCH-DATE      PIC 9(8) COMP-3.
           03  TB-BORRTBL          OCCURS 4 TIMES.
               05  TB-SSNO         PIC 9(9)       COMP-3.
               05  TB-MAKERCD      PIC X.
      *-----------------------------------------------------------------
      *   TB-REF-FRTO --> T : TRANSFERRED TO ANOTHER BRANCH.
      *                   F : TRANSFERRED FROM ANOTHER BRANCH.
      *-----------------------------------------------------------------
           03  TB-REFNO.
               05  TB-REF-FRTO     PIC X.
               05  TB-REFBR        PIC 9999.
           03  TB-PURCD            PIC XXX.
           03  TB-SRCD             PIC XX.
           03  TB-COLLCD           PIC XX.
           03  TB-SERVACCT         PIC X.
           03  TB-LOANTYPE         PIC X.
           03  TB-SUBCLASS         PIC 99.
      *-----------------------------------------------------------------

      $XFD DATE=YYYYMMDD
           03  TB-LOANDATE         PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------
           03  TB-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TB-LNAMT            PIC S9(7)V99   COMP-3.
           03  TB-FINCHG           PIC S9(7)V99   COMP-3.
           03  TB-INTCHG           PIC 9(7)V99    COMP-3.
           03  TB-SERCHG           PIC 9(5)V99    COMP-3.
           03  TB-EXTCHG           PIC 9(5)V99    COMP-3.

           03  TB-MAINTFEE         PIC 9(5)V99    COMP-3.
           03  TB-ORGTERM          PIC 9(3)       COMP-3.
      *-----------------------------------------------------------------
           03  TB-INSCODES.
               05  TB-INSURED      PIC X.

           03  TB-INSUNEARN       OCCURS 10
                                   PIC S9(7)V99  COMP-3.
      *-----------------------------------------------------------------
           03  TB-DLNO             PIC 9(4)V99    COMP-3.
           03  TB-DLRV             PIC S9(7)V99   COMP-3.
           03  TB-DLSPRV           PIC S9(7)V99   COMP-3.
           03  TB-DLDISC           PIC S9(7)V99   COMP-3.
           03  TB-DLPARV           PIC S9(7)V99   COMP-3.
           03  TB-DLRECOURSE       PIC X.

           03  TB-DLPARTRCAMT      PIC 9(7)V99    COMP-3.
           03  TB-PURBAL           PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------

           03  TB-UNEARN           OCCURS 10 TIMES
                                   PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TB-REF-ACCTNO       PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------

           03  TB-DLRVPRIN         OCCURS 3 TIMES
                                   PIC S9(8)V99    COMP-3.

           03  TB-DLRVCHGS         OCCURS 3 TIMES
                                   PIC S9(7)V99    COMP-3.
           03  TB-DLPARVPAID       PIC S9(7)V99    COMP-3.
           03  TB-DLPARVHELD       PIC S9(7)V99    COMP-3.
           03  TB-USERID           PIC X(10).

      $XFD DATE=YYYYMMDD
           03  TB-POSTDATE         PIC 9(8)        COMP-3.
           03  TB-POSTTIME         PIC 9(6)        COMP-3.
           03  TB-PLCD             PIC X.
           03  TB-PLTYPE           PIC X.
           03  TB-REPOCD           PIC X.
           03  TB-ACTIONCD2        PIC X(4).
           03  FILLER              PIC X(528).
