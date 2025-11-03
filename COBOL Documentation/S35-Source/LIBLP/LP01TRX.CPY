      * COPYMEMBER: LIBLP/LPFDTRX.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [CLASS TRANSFERS]
      *
      * FILE INFO  :  CLASS TRANSFER
      *               LP/TRXFILE
      *
      * TRX1-KEY = TX-BRNO,TX-DATE,TX-CLASS,TX-ACCTNO,TX-LAWCODE,TX-LINE
      * TRX2-KEY = TX-DATE,TX-BRNO,TX-CLASS,TX-ACCTNO,TX-LAWCODE,TX-LINE
      *
      *
      *=================================================================
      * REV:
      * MJD 062795 ADDED TX-USERID, TX-POSTTIME, TX-POSTDATE
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS 160915 REMOVED TX-SEQNO
      *            REMOVED  TX-FEETBL,NOT UPDATED 
      *            ADDED TX-ACCTNO 9(8) AND MADE TX-NUMBER REDEFINE
      *            CHANGED  TX-LOANDATE/ TX-POSTDATE FROM 9(6) TO 9(8)
      *            CHANGED TX-MAINTFEE FROM 9(3)V99 TO 9(5)V99 
      *            REMOVED TX-INSTBL OCCURS 10, NOT UPDATED    
      *            CHANGED TX-DLPARTRCAMT 9(6)V99 TO 9(7)V99
      *            CHANGED TX-DLRVCHGS 9(6)V99 TO 9(7)V99
      *            CHANGED TX-DLPARVPAID 9(6)V99 TO 9(7)V99
      *            CHANGED  TX-SPRCLASS 99 TO 999
      *            CHANGED  TX-ANTICERN /TX-EARNINGS OCCURS 6 TO 10  
      *            ADDED TX-INSANTICERN/TX-INSEARNINGS OCCURS 10
      * CS 161117 ADD TX-SUBCLASS
      *  CS 170713 CHANGED TX-LINE TO PIC 9(3) TO MATCH WK2-LINE, IN A15
      *            WAS COMP 9(2) WHICH ALLOWED A VALUE OF 248, I DONT THINK
      *            WE ALLOWED LINE >99 BUT CHANGED TO 999. FILLER TO 4
      *            SAVED LPFDTRX.170713
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      * BAH 20200325 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  TRX-SIZE                    VALUE 750.
       01  TRX-REC.
           03  TRX1-KEY.
               05  TX-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TX-DATE             PIC 9(8).
               05  TX-CLASS            PIC 9(3).
               05  TX-ACCTNO           PIC 9(8).
               05  TX-LAWCODE          PIC 99.
               05  TX-LINE             PIC 9(3).
      *----------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  TX-LTOUCH-DATE      PIC 9(8) COMP-3.

           03  TX-BORRTBL        OCCURS 4.
               05  TX-SSNO         PIC 9(9)       COMP-3.
               05  TX-MAKERCD      PIC X.
           03  TX-PURCD            PIC XXX.
           03  TX-SRCD             PIC XX.
           03  TX-COLLCD           PIC XX.
           03  TX-SERVACCT         PIC X.
           03  TX-LOANTYPE         PIC X.
           03  TX-SUBCLASS         PIC 99.
      $XFD DATE=YYYYMMDD
           03  TX-LOANDATE         PIC 9(8)       COMP-3.
           03  TX-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TX-LNAMT            PIC S9(7)V99   COMP-3.
           03  TX-FINCHG           PIC S9(7)V99   COMP-3.
           03  TX-INTCHG           PIC 9(7)V99    COMP-3.
           03  TX-SERCHG           PIC 9(5)V99    COMP-3.
           03  TX-EXTCHG           PIC 9(5)V99    COMP-3.
           03  TX-MAINTFEE         PIC 9(5)V99    COMP-3.
           03  TX-ORGTERM          PIC 9(3)       COMP-3.
           03  TX-INSCODES.
               05  TX-INSURED      PIC X.

      *%ADDED FOR WHEN WE STORE INSURANCE EARNINGS,A TP FROM OUR
      *CLASS TO SERVICE CLASS GENERATES AND 'X' RECORD FOR OUR CLASS
           03  TX-INSANTICERN      PIC S9(7)V99   COMP-3 OCCURS 10.
           03  TX-INSEARNINGS      PIC S9(7)V99   COMP-3 OCCURS 10.
      *----------------------------------------------------
           03  TX-DLNO             PIC 9(4)V99    COMP-3.
           03  TX-DLRECOURSE       PIC X.
           03  TX-DLPARTRCAMT      PIC 9(7)V99    COMP-3.
           03  TX-PURBAL           PIC S9(7)V99   COMP-3.
           03  TX-DLRVPRIN         PIC S9(7)V99   COMP-3 OCCURS 3.
           03  TX-DLRVCHGS         PIC S9(7)V99   COMP-3 OCCURS 3.
           03  TX-DLPARVHELD       PIC S9(7)V99   COMP-3.
           03  TX-DLPARVPAID       PIC S9(7)V99   COMP-3.
           03  TX-DLDISC           PIC S9(7)V99   COMP-3.
           03  TX-ANTICTERM        PIC 9(3)       COMP-3.
           03  TX-ANTICERN         PIC S9(7)V99   COMP-3 OCCURS 10.
           03  TX-EARNINGS         PIC S9(7)V99   COMP-3 OCCURS 10.
           03  TX-PRLNNO           PIC 9(8)       COMP-3.
           03  TX-PRBAL            PIC S9(7)V99   COMP-3.
           03  TX-TFRBAL           PIC S9(7)V99   COMP-3.
           03  TX-SPRCLASS         PIC 999        COMP-3.
           03  TX-USERID           PIC X(10).
      $XFD DATE=YYYYMMDD
           03  TX-POSTDATE         PIC 9(8)       COMP-3.
           03  TX-POSTTIME         PIC 9(6)       COMP-3.
           03  FILLER              PIC X(344).
