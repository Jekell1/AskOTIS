      * COPYMEMBER: LIBLP/LP01CL
      ******************************************************************
      *
      * DESCRIPTION: CAN LOAN FILE (CL)
      *
      *    CL1-KEY =  CL-GROUP-ID, CL-CAN-NO
      *    CL2-KEY =  CL-GROUP-ID, CL-DESC WITH DUPLICATES
      *
      * ***NOTE*** : CL-PD-REC MUST ALWAYS MATCH PD-REC
      *
      *
      *=================================================================
      * REV:
      * JTG 060597 CREATED FILE
      * JTG 062397 CHANGED TO 4 DIGIT GROUP
      * JTG 082097 INCREASED CL-PD-REC FROM 1830 TO 1889
      * JTG 112097 EXPANDED CL-LN-REC BY 230 BYTES
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLM 160615 INCREASED CL-PD-REC FROM 2289 TO 3308
      * KEC 161004 ADDED CL-LTOUCH-DATE.
      *            INCREASED FILLER TO X(55).
      *            CHANGED   CL-PD-REC FROM 3308 TO 3200 (CL-PD-SIZE).
      *            CHANGED   CL-REC    FROM 3368 TO 3300 (CL-SIZE).
      *
      * KEC 2017.0615 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          IN REVIEW OF A30-TO-A15 CHANGES, NOTICED THAT THE
      *          CL-PD-REC WAS SMALLER THAN PD-REC.
      *          CHANGED CL-PD-REC FROM 3200 TO 3350 (CL-PD-SIZE).
      *          CHANGED CL-REC    FROM 3300 TO 3450 (CL-SIZE).
      *
      *  CS 190517 ADD FUTURE FIELDS
      *
      * JKC 191206 IN A34 TESTING OF UP1534, GOT A 9D,25 ERROR WHICH IS
      *            THE DATABASE REJECTING NON-NUMERIC DATA WITHIN A
      *            PRIMARY KEY NUMERIC FIELD.  SO NEEDED TO MAKE
      *            CL-GROUP-ID A ELEMENTARY FIELD BEING PIC X(4) THEN
      *            REDEFINES ON THAT FIELD MAKING CL-GROUP-LOC PIC 9(4).
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20200617 ADDED CL-PD-RECORD LAYOUT, NOT JUST GROUP
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 2024.0404 ADDED CL-LOS-APP-ID PIC 9(18) COMP-3
      *
      ******************************************************************
      *
      * ***NOTE*** : CL-PD-REC MUST ALWAYS MATCH PD-REC
      *
      ******************************************************************
       78  CL-SIZE                         VALUE 3450.
       78  CL-PD-SIZE                      VALUE 3350.
       01  CL-REC.
           03  CL1-KEY.
               05  CL-GROUP-ID         PIC X(4).
               05  CL-GROUP-LOC        REDEFINES CL-GROUP-ID
                                       PIC 9(4).
               05  CL-CAN-NO           PIC XX.
      $XFD DATE=YYYYMMDD
           03  CL-LTOUCH-DATE          PIC 9(08)     COMP-3.
           03  CL-DESC                 PIC X(33).

           03  CL-GIFTNET              PIC X.
 
           03  FILLER                  PIC X(55).
 
      *    --------------------------------------------------
           03  CL-PD-REC.
               05  CL-PD1-KEY.
                   07  CL-PD-ID               PIC X.
                   07  CL-PD-BRANCH           PIC 9(4).
                   07  CL-PD-SOC-SEC          PIC 9(10).
               05  CL-PD-RECORD.
                   07  CL-PD-OWNBR            PIC 9(4).
                   07  CL-PD-ACCTNO           PIC 9(8).
                   07  CL-PD-CLASS            PIC 9(3).
                   07  CL-PD-SUBCLASS         PIC 99.
                   07  CL-PD-LAWCODE          PIC 99.
                   07  CL-PD-BWTABLEX.
                       09  CL-PD-SSNO1        PIC 9(9).
                       09  CL-PD-MAKERCD1     PIC X.
                       09  CL-PD-SSNO2        PIC 9(9).
                       09  CL-PD-MAKERCD2     PIC X.
                       09  CL-PD-SSNO3        PIC 9(9).
                       09  CL-PD-MAKERCD3     PIC X.
                       09  CL-PD-SSNO4        PIC 9(9).
                       09  CL-PD-MAKERCD4     PIC X.
                   07  CL-PD-BWTABLE REDEFINES CL-PD-BWTABLEX.
                       09  CL-PD-BORRTBL      OCCURS 4 TIMES.
                           11  CL-PD-SSNO     PIC 9(9).
                           11  CL-PD-MAKERCD  PIC X.
                   07  CL-PD-ORGST            PIC XX.
                   07  CL-PD-ORGBR            PIC 9(4)   COMP-3.
                   07  CL-PD-OWNST            PIC XX.
                   07  CL-PD-SPRCLASS         PIC 9(3).
                   07  CL-PD-CONVERCD         PIC X.
                   07  CL-PD-MIDSTREAM        PIC X.
                   07  CL-PD-PURCD            PIC XXX.
                   07  CL-PD-SRCD             PIC XX.
                   07  CL-PD-COLLCD           PIC XX.
                   07  CL-PD-COLLID           PIC X(20).
                   07  CL-PD-POCD             PIC XX.
                   07  CL-PD-PLCD             PIC X.
                   07  CL-PD-PLTYPE           PIC X.
                   07  CL-PD-CONFIDENTCD      PIC X.
                   07  CL-PD-ALLOTCD          PIC X.
                   07  CL-PD-RECASTCD         PIC X.
                   07  CL-PD-SKIP-TRW         PIC X.
                   07  CL-PD-STATUSFG         PIC X.
                   07  CL-PD-SMALLOAN         PIC X.
                   07  CL-PD-SALEFIN          PIC X.
                   07  CL-PD-SERVACCT         PIC X.
                   07  CL-PD-LOANTYPE         PIC X.
                   07  CL-PD-ADDON-FG         PIC X.
                   07  CL-PD-PAY-SCHLD-FG     PIC X.
                   07  CL-PD-LC-CODE          PIC 99.
                   07  CL-PD-CAN-LOAN-NO      PIC XX.
                   07  CL-PD-STATFLAGS        PIC X(20).
      $XFD DATE=YYYYMMDD
                   07  CL-PD-ENTDATE          PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LOANDATE         PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PURDATE          PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-INTDATE          PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-CASHDATE         PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-FILEDATE         PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-RESCISSION-DATE  PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PART-RECOURSE-DATE PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-ORIG-1STPYDATE   PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-1STPYDATE        PIC 9(8).
                   07  CL-PD-1STPYDATEX REDEFINES CL-PD-1STPYDATE.
                       09  CL-PD-1STPY-YYYYMM PIC 9(6).
                       09  CL-PD-1STPYDA      PIC 99.

      $XFD DATE=YYYYMMDD
                   07  CL-PD-JDDATE           PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PLDATE           PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PL-TRDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-ACCELDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-REDUDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-POFFDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PO-TRDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LEGALDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-BNKRPTDATE       PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-BNKRPT-DISMISS-DATE PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-BNKRPT-DISCHARGE-DATE PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-BNKRPT-REAFFIRM-DATE  PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-DEBTAIDATE       PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-REPODATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-REDEEMDATE       PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-SOLDDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-INS-CONFIRM-DATE PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PRIOR-RN-FOR10-DATE PIC 9(8)       COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LAST-RN-FOR10-DATE  PIC 9(8)       COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-TRW-DOFD-DATE       PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LAST-PYPA-DATE   PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-BT-TRDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-METRO2-LAST-PMT-DATE PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-FOLLOWUP-DATE    PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LAST-ACCRUAL-DATE PIC 9(8)     COMP-3.
                   07  CL-PD-APRATE           PIC 999V999   COMP-3.
                   07  CL-PD-EFFRATE          PIC 999V999   COMP-3.
                   07  CL-PD-SMPRATE          PIC 999V999   COMP-3.
                   07  CL-PD-YLDRATE          PIC 999V999   COMP-3.
                   07  CL-PD-OVRDRATE         PIC 999V999   COMP-3.
                   07  CL-PD-ENDORATE         PIC 999V999   COMP-3.
                   07  CL-PD-JDRATE           PIC 999V999   COMP-3.
                   07  CL-PD-ACCRATE          PIC 999V999   COMP-3.
                   07  CL-PD-REDURATE         PIC 999V999   COMP-3.
                   07  CL-PD-CONRATE          PIC 999V999 COMP-3.
                   07  CL-PD-SERVPER          PIC 9(3)V99   COMP-3.
                   07  CL-PD-SELLPR           PIC 9(7)V99   COMP-3.
                   07  CL-PD-DOWNPY           PIC 9(7)V99   COMP-3.
                   07  CL-PD-TRADEIN          PIC S9(7)V99  COMP-3.
                   07  CL-PD-LNAMT            PIC 9(7)V99   COMP-3.
                   07  CL-PD-FINCHG           PIC 9(7)V99   COMP-3.
                   07  CL-PD-INTCHG           PIC 9(7)V99   COMP-3.
                   07  CL-PD-SERCHG           PIC 9(5)V99   COMP-3.
                   07  CL-PD-EXTCHG           PIC 9(5)V99   COMP-3.
                   07  CL-PD-MAINTFEE         PIC 9(5)V99   COMP-3.
                   07  CL-PD-POINTS           PIC 99V99     COMP-3.
                   07  CL-PD-PROCEEDS         PIC S9(7)V99  COMP-3.
                   07  CL-PD-ORGTERM          PIC 9(3)      COMP-3.
                   07  CL-PD-1STPYAMT         PIC 9(7)V99   COMP-3.
                   07  CL-PD-NOREGPY          PIC 9(3)      COMP-3.
                   07  CL-PD-REGPYAMT         PIC 9(7)V99   COMP-3.
                   07  CL-PD-LASTPYAMT        PIC S9(7)V99  COMP-3.
                   07  CL-PD-PROCASH          PIC S9(7)V99  COMP-3.
                   07  CL-PD-TILLNO           PIC 9(2).
                   07  CL-PD-STATE-FLAG       PIC X.
                   07  CL-PD-STATE-WORKER     PIC S9(7)V99 COMP-3.
                   07  CL-PD-STATE-WORKER2    PIC S9(5)V99 COMP-3.
                   07  CL-PD-FEETBL           OCCURS 20 TIMES.
                       09  CL-PD-FEECODE      PIC XX.
                       09  CL-PD-FEEAMT       PIC 9(5)V99   COMP-3.
                       09  CL-PD-FEEOURS      PIC X.
                       09  CL-PD-FEEFINCHG    PIC X.
                   07  CL-PD-INSCODES.
                       09  CL-PD-INSURED      PIC X.
                       09  CL-PD-INSTYPES     PIC X.
                       09  CL-PD-INSELIGIBLE  PIC X.
                   07  CL-PD-INSTBL             OCCURS 10 TIMES.
                       09  CL-PD-INSCOMP         PIC 99.
                       09  CL-PD-INSOURS         PIC X.
      * THIS IS A TERM FOR QUOTES AND SAVED LOANS
                       09  CL-PD-INSEFF-DATE     PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                       09  CL-PD-INSEXP-DATE     PIC 9(8)      COMP-3.
                       09  CL-PD-INSPREM         PIC 9(5)V99   COMP-3.
                       09  CL-PD-INSCOVR         PIC 9(7)V99   COMP-3.
                       09  CL-PD-INSCOMM         PIC 9(5)V99   COMP-3.
                       09  CL-PD-INSANTICERN     PIC S9(7)V99  COMP-3.
                       09  CL-PD-INSANTICADJ     PIC S9(7)V99  COMP-3.
                       09  CL-PD-INSUFERN        PIC S9(7)V99  COMP-3.
                       09  CL-PD-INSACCERN       PIC S9(7)V99  COMP-3.
                       09  CL-PD-INSEARN-ORDER   PIC 99.
                       09  CL-PD-INSFINCHG       PIC X.


                   07  CL-PD-PY-YYYYMM-RECDEL    PIC 9(6)      COMP-3.
                   07  CL-PD-MTD-PAYS-RECDEL     PIC S9(7)V99  COMP-3.
                   07  CL-PD-GARNISH-EXTR        PIC X.
                   07  CL-PD-SHIP-SAME-FG        PIC X.
                   07  CL-PD-AD-D-INS-COVR       PIC 9(7)      COMP-3.
                   07  CL-PD-TOT-PARTIAL-CHGOFF  PIC S9(7)V99  COMP-3.
                   07  CL-PD-LPO                 PIC X(4).
                   07  CL-PD-REP-CD              PIC X(8).
                   07  CL-PD-EMP-ID              PIC 9(8)      COMP-3.
                   07  CL-PD-PROCESS-CD          PIC X.
                   07  CL-PD-WCBC-CREDITLIM      PIC S9(5)     COMP-3.
                   07  CL-PD-FOLLOWUP-CD         PIC X(4).
                   07  CL-PD-MIP-FG              PIC X.
                   07  CL-PD-MIP-MTD-CLAHPAID    PIC S9(5)V99  COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-MIP-CLPDTH-DATE     PIC 9(8)      COMP-3.
                   07  CL-PD-MIP-TOT-CLPAID      PIC S9(7)V99  COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-MIP-AHPDTH-DATE     PIC 9(8)      COMP-3.
                   07  CL-PD-MIP-TOT-AHPAID      PIC S9(7)V99  COMP-3.
                   07  CL-PD-IL-GROSS-INCOME     PIC S9(7)V99  COMP-3.
                   07  CL-PD-IL-GROSS-INCOME2    PIC S9(7)V99  COMP-3.
                   07  CL-PD-RESCISSION          PIC X(1).
                   07  CL-PD-INTERIM-TRW         PIC X(1).
                   07  CL-PD-DLNO             PIC 9(4)V99   COMP-3.
                   07  CL-PD-DLPOOL           PIC 9(5)      COMP-3.
                   07  CL-PD-DLRECOURSE       PIC X.
                   07  CL-PD-DLPARTRCAMT      PIC 9(7)V99   COMP-3.
                   07  CL-PD-ORIGPURAMT       PIC 9(7)V99   COMP-3.
                   07  CL-PD-PURBAL           PIC S9(7)V99  COMP-3.
                   07  CL-PD-DLRVPRIN         OCCURS 3
                                       PIC 9(7)V99   COMP-3.
                   07  CL-PD-DLRVCHGS         OCCURS 3
                                       PIC 9(7)V99   COMP-3.
                   07  CL-PD-DLPARVHELD       PIC 9(7)V99   COMP-3.
                   07  CL-PD-DLPARVPAID       PIC 9(7)V99   COMP-3.
                   07  CL-PD-DLDISC           PIC 9(7)V99   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LCPDTH-DATE           PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-INTPDTH-DATE          PIC 9(8)      COMP-3.
                   07  CL-PD-CURBAL           PIC S9(7)V99  COMP-3.
                   07  CL-PD-INTBAL           PIC S9(7)V99  COMP-3.
                   07  CL-PD-LCBAL            PIC S9(7)V99  COMP-3.
                   07  CL-PD-OTHBAL           PIC S9(7)V99  COMP-3.
                   07  CL-PD-OT2BAL           PIC S9(7)V99  COMP-3.
                   07  CL-PD-LCPAID           PIC S9(7)V99  COMP-3.
                   07  CL-PD-LCPARTIALS       PIC S9(7)V99  COMP-3.
                   07  CL-PD-STMT-COUNTER     PIC 9(5)      COMP-3.
                   07  CL-PD-PRIOR-STMT-COUNTER PIC 9(5)  COMP-3.
                   07  CL-PD-FROZEN-REASON    PIC XX.
                   07  CL-PD-COUPON-STATEMENT PIC X.
                   07  CL-PD-MONTHEND-BAL     PIC 9(7)V99   COMP-3.
                   07  CL-PD-ANTICTERM        PIC 9(3)      COMP-3.
                   07  CL-PD-ANTICTBL         OCCURS 10 TIMES.
                       09  CL-PD-ANTICERN     PIC S9(7)V99  COMP-3.
                       09  CL-PD-ANTICADJ     PIC S9(7)V99  COMP-3.
                       09  CL-PD-UFERN        PIC S9(7)V99  COMP-3.
                       09  CL-PD-ACCERN       PIC S9(7)V99  COMP-3.
                       09  CL-PD-EARN-ORDER   PIC 99.


                   07  CL-PD-LMO-PRIOR-INTEREST PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRIOR-UNEARNED   PIC S9(7)V99  COMP-3.
                   07  CL-PD-ERNDELQ-STOP     PIC X.
                   07  CL-PD-NONACRERN        PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRIOR-NONACRERN  PIC S9(7)V99  COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-ERN-IB-PDTH-DATE      PIC 9(8)      COMP-3.
                   07  CL-PD-ERN-IB-PRINBAL   PIC S9(7)V99  COMP-3.
                   07  CL-PD-ERN-IB-INTOWE    PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTERN           PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTOTH           PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTINT           PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTLCHG          PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTDEF           PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTSERACCT       PIC S9(7)V99  COMP-3.
                   07  CL-PD-LCWAIVERS        PIC S9(5)V99  COMP-3.
                   07  CL-PD-INTWAIVERS       PIC S9(5)V99  COMP-3.
                   07  CL-PD-TOTPAYMNTD       PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTEXCPAYMNTD    PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTNODEF         PIC S99       COMP-3.
                   07  CL-PD-YTDNODEF         PIC S99       COMP-3.
                   07  CL-PD-TOTNODEFACT      PIC S99   COMP-3.
                   07  CL-PD-YTDNODEFACT      PIC S99   COMP-3.
                   07  CL-PD-TOTNOALDEL       PIC S99       COMP-3.
                   07  CL-PD-YTDNOALDEL       PIC S99       COMP-3.
                   07  CL-PD-YTDINTPD         PIC S9(7)V99  COMP-3.
                   07  CL-PD-LYRINTPD         PIC S9(7)V99  COMP-3.
                   07  CL-PD-TOTINTPD         PIC S9(7)V99  COMP-3.
                   07  CL-PD-RECASTINT        PIC 9(7)V99   COMP-3.
                   07  CL-PD-RECASTPRIN       PIC 9(7)V99   COMP-3.

                   07  CL-PD-REBATE-TAB.
                       09  CL-PD-CL-REBATE    PIC X.
                       09  CL-PD-AH-REBATE    PIC X.
                       09  CL-PD-PP-REBATE    PIC X.
                       09  CL-PD-INT-REBATE   PIC X.
                       09  CL-PD-SC-REBATE    PIC X.
                       09  CL-PD-MF-REBATE    PIC X.
                       09  CL-PD-DF-REBATE    PIC X.
                       09  CL-PD-EX-REBATE    PIC X.
                       09  CL-PD-O1-REBATE    PIC X.
                       09  CL-PD-O2-REBATE    PIC X.
                       09  CL-PD-O3-REBATE    PIC X.
                       09  CL-PD-O4-REBATE    PIC X.
                       09  CL-PD-O5-REBATE    PIC X.
                       09  CL-PD-O6-REBATE    PIC X.
                       09  CL-PD-O7-REBATE    PIC X.
                       09  CL-PD-UN-O7-REB    PIC X.
                       09  CL-PD-UN-O8-REB    PIC X.
                       09  CL-PD-UN-O9-REB    PIC X.
                       09  CL-PD-UN-O10-REB   PIC X.
                   07  FILLER         REDEFINES CL-PD-REBATE-TAB.
                       09  CL-PD-REBATE       PIC X OCCURS 19.

                   07  CL-PD-PLREASON         PIC XX.
                   07  CL-PD-COLLECTOR        PIC X(3).
                   07  CL-PD-ACTIONCD         PIC X(4).
                   07  CL-PD-ACTIONCD2        PIC X(4).
                   07  CL-PD-ATCODE           PIC X(3).
                   07  CL-PD-RECODE           PIC X(3).
                   07  CL-PD-ESCROW-FG        PIC X.
                   07  CL-PD-PROMDAYS-PASTDUE PIC S9(3)     COMP-3.
                   07  CL-PD-PROMID           PIC X(3).
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PROMDATE         PIC 9(8)      COMP-3.
                   07  CL-PD-PROMAMT          PIC 9(7)V99   COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PROMDATE2        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-DATE-PAID-LAST   PIC 9(8)      COMP-3.
                   07  CL-PD-AMT-PAID-LAST    PIC 9(7)V99   COMP-3.
                   07  CL-PD-LASTCFORMNO      PIC 9(5)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LASTCFORMDATE    PIC 9(8)      COMP-3.
                   07  CL-PD-DELCNT           PIC 99.
                   07  CL-PD-DELCNT2          PIC 99.
                   07  CL-PD-CREDITLIM        PIC 9(7)      COMP-3.
                   07  CL-PD-CRSCORE          PIC S9(3)     COMP-3.

                   07  CL-PD-LASTSFORMNO       PIC 9(5)      COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LASTSFORMDATE     PIC 9(8)      COMP-3.
                   07  CL-PD-SOLCNT           PIC 99.
                   07  CL-PD-MESSAGE          PIC X(30).
                   07  CL-PD-OLDACCTNO        PIC 9(8)      COMP-3.
                   07  CL-PD-PRLNNO           PIC 9(8)      COMP-3.
                   07  CL-PD-PRBAL            PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRINTDUE         PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRFINCHGRB       PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRINSRB          PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRPOCD           PIC XX.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-PRPODATE         PIC 9(8)      COMP-3.
                   07  CL-PD-PRDELINQAMT      PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRLNAMT          PIC 9(7)V99   COMP-3.
                   07  CL-PD-PRPDUE-DAYS      PIC S9(5)     COMP-3.
                   07  CL-PD-PRDLDISC-UNEARN    PIC S9(7)V99  COMP-3.
                   07  CL-PD-PRCONPROFILE     PIC 9(12)     COMP-3.
                   07  CL-PD-SUM-ORIGPURAMT   PIC 9(7)V99   COMP-3.
                   07  CL-PD-SUM-LNAMT        PIC 9(7)V99   COMP-3.
                   07  CL-PD-SUM-FINCHG       PIC 9(7)V99   COMP-3.
                   07  CL-PD-SUM-INTCHG       PIC 9(7)V99   COMP-3.
                   07  CL-PD-SUM-ORGTERM      PIC 9(3)      COMP-3.
                   07  CL-PD-SUM-REGPYAMT     PIC 9(7)V99   COMP-3.
                   07  CL-PD-SUM-EXTCHG       PIC 9(5)V99   COMP-3.
                   07  CL-PD-SUM-SERCHG       PIC 9(5)V99   COMP-3.
                   07  CL-PD-SUM-MAINTFEE     PIC 9(7)V99   COMP-3.
                   07  CL-PD-UNITPER-INFO.
                       09  CL-PD-UNITPER-CD    PIC X.
                       09  CL-PD-UNITPER-FREQ  PIC 999        COMP-3.

                   07  CL-PD-DAYS-IN-UNITPER   PIC 9(3)       COMP-3.
                   07  CL-PD-UNITPER-PER-YEAR  PIC 9(3)V9(12) COMP-3.
                   07  CL-PD-REPOCD            PIC X.
                   07  CL-PD-BR-TFR-FG         PIC X.
                   07  CL-PD-O3INS-DLPROC      PIC 9(7)V99   COMP-3.
                   07  CL-PD-INSPREM-DLADJ     PIC S9(5)V99  COMP-3.
                   07  CL-PD-PLAMT             PIC 9(7)V99   COMP-3.
                   07  CL-PD-MTD-PRIN          PIC S9(7)V99  COMP-3.
                   07  CL-PD-MTD-LC            PIC S9(5)V99  COMP-3.
                   07  CL-PD-MTD-INT           PIC S9(5)V99  COMP-3.
                   07  CL-PD-MTD-OT            PIC S9(5)V99  COMP-3.
                   07  CL-PD-MTD-EX            PIC S9(7)V99  COMP-3.
                   07  CL-PD-BNKRPT-CHAPTER    PIC 9(2).
                   07  CL-PD-BMO-CON-STATUS    PIC X.
                   07  CL-PD-BMO-REC-STATUS    PIC X.
                   07  CL-PD-APP-NUMBER        PIC 9(09)    COMP-3.
                   07  CL-PD-GIFTNET           PIC X.
                   07  CL-PD-GIFTNET-OUR-PO    PIC 9(7)     COMP-3.
                   07  CL-PD-PROC-DISBURSED    PIC X.
                   07  CL-PD-NOODDPY           PIC 9.
                   07  CL-PD-CONPROFILE        OCCURS 5
                                        PIC 9(12)     COMP-3.
                   07  CL-PD-RECPROFILE        OCCURS 5
                                        PIC 9(12)     COMP-3.
                   07  CL-PD-TRW-STATUS            PIC XX.
                   07  CL-PD-ORIGIN                PIC X(4).
                   07  CL-PD-PO-TRCD               PIC XX.
                   07  CL-PD-NEXIS-CRSCORE         PIC 9(3)     COMP-3.
                   07  CL-PD-TRW-B-CONSUMER-IND    PIC X(2).
                   07  CL-PD-PARVHELD-PAID-FG      PIC X.
                   07  CL-PD-ACCT-FROZEN           PIC X.
                   07  CL-PD-TRW-SPECIAL-CODE      PIC X(2).
                   07  CL-PD-CREATION-STATUS       PIC X(1).
                   07  CL-PD-TOTNOLCHG             PIC S9(3)    COMP-3.
                   07  CL-PD-LBOX                  PIC X(01).
                   07  CL-PD-EXTRA-PRIN-LFP        PIC S9(7)V99 COMP-3.
                   07  CL-PD-REMAIN-INT-SINCE-LFP  PIC S9(7)V99 COMP-3.
                   07  CL-PD-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99 COMP-3.
                   07  CL-PD-INTERIM-COUPON        PIC X.
                   07  CL-PD-DLCOMM-O3-CMSI        PIC 9(5)V99  COMP-3.
                   07  CL-PD-DLCOMM-PP-CMSI        PIC 9(5)V99  COMP-3.
                   07  CL-PD-TRW-J-CONSUMER-IND    PIC X(2).
                   07  CL-PD-ACH                   PIC X.
                   07  CL-PD-1099-C-FG             PIC X.
                   07  CL-PD-TRW-COMPLIANCE-CODE   PIC XX.
                   07  CL-PD-UNWIND-REASON-CODE    PIC X(2).
                   07  CL-PD-PBI-PBD-FG            PIC X.
                   07  CL-PD-OLD-ACCT-FORMAT-FG    PIC X.
                   07  CL-PD-APPROVED-BY-USER      PIC X(10).
                   07  CL-PD-APPROVED-BY-CODE      PIC X(3).
      $XFD DATE=YYYYMMDD
                   07  CL-PD-APPROVED-BY-DATE      PIC 9(8)     COMP-3.
                   07  CL-PD-APPROVED-BY-TIME      PIC 9(6)     COMP-3.
      $XFD DATE=YYYYMMDD
                   07  CL-PD-LTOUCH-DATE           PIC 9(8)     COMP-3.
                   07  CL-PD-LTOUCH-TIME           PIC 9(6)     COMP-3.
      * NEXUS APP ID
                   07  CL-PD-LOS-APP-ID            PIC 9(18)    COMP-3.
                   07  FILLER                      PIC X(690).

                   07  CL-PD-REST-OF-REC.
                       09  CL-PD-HOLD-COLLATERAL   PIC X(60).
                       09  CL-PD-HOLD-COLL REDEFINES
                                        CL-PD-HOLD-COLLATERAL.
                           11  CL-PD-MSG1          PIC X(30).
                           11  CL-PD-MSG2          PIC X(30).
                       09  CL-PD-DLPAID            PIC X.
                       09  CL-PD-CKNO              PIC 9(6)    COMP-3.
                       09  CL-PD-FREE1             PIC X(70).
                       09  CL-PD-FREE2             PIC X(70).
      * TO MATCH LP01WK RECORD, THESE MUST STAY IN SYNC!
                       09  CL-PD-FREE-SPACE-FILLER PIC X(44).

                       09  CL-PD-POSTING-BR        PIC 9(4)    COMP-3.
                       09  CL-PD-OTHBR-FG          PIC X.
                       09  CL-PD-APPROVAL-LIMIT    PIC 9(7)    COMP-3.
                       09  CL-PD-APPROVAL-USERID   PIC X(3).
                       09  CL-PD-MIN-CRED-SCORE    PIC 9(4)    COMP-3.
                       09  CL-PD-CRED-SCORE-USERID PIC X(3).

                       09  CL-PD-NEW-FIELDS.
                           11  CL-PD-FINAMT        PIC 9(7)V99 COMP-3.
                           11  CL-PD-PROCEEDS-FG   PIC X.
                           11  CL-PD-POINTS-FG     PIC X.
                           11  CL-PD-OVRDRATE-FG   PIC X.
                           11  CL-PD-INS-FLAGS    OCCURS 10.
                               13  CL-PD-INS-FG    PIC X.
                           11  CL-PD-SERCHG-FG     PIC X.
                           11  CL-PD-MAINTFEE-FG   PIC X.
                           11  CL-PD-EXTCHG-FG     PIC X.
                           11  CL-PD-REGPYAMT-FG   PIC X.
                           11  CL-PD-FEECD        OCCURS 20
                                                   PIC XX.
                           11  CL-PD-PRPAID-AHEAD  PIC S9(7)V99 COMP-3.
                           11  CL-PD-RESTRICT-ADJ  PIC S9(7)V99 COMP-3.
                           11  CL-PD-DUAL-LOAN-ADJ PIC S9(7)V99 COMP-3.
                           11  CL-PD-CAN-LOAN-MAX  PIC S9(7)V99 COMP-3.
                           11  CL-PD-PERDOWN       PIC 999V99   COMP-3.
                           11  CL-PD-SELLPR-FG     PIC X.
                           11  CL-PD-DOWNPY-FG     PIC X.
                           11  CL-PD-PERDOWN-FG    PIC X.
                           11  CL-PD-SELL-DOWN-FG  PIC X.
                           11  CL-PD-BALLOON-FG    PIC X.
                           11  CL-PD-BALLOON-LAST-FG  PIC X.
                           11  CL-PD-FEECD-FG     OCCURS 2
                                                   PIC X.
                           11  CL-PD-INS-SWITCHES OCCURS 10.
                               13  CL-PD-INS-TERM-SW  PIC X.
                               13  CL-PD-COVERAGE-SW  PIC X.
                               13  CL-PD-PREMIUM-SW   PIC X.
                           11  CL-PD-DLDISC-FG        PIC X.
                           11  CL-PD-DLRVPRIN-FG     OCCURS 3
                                                      PIC X.
                           11  CL-PD-DLRVCHGS-FG     OCCURS 3
                                                      PIC X.
                           11  CL-PD-DLPARVHELD-FG    PIC X.
                           11  CL-PD-DLPARVPAID-FG    PIC X.
                           11  CL-PD-ONE-MTH-NO-DAY-EXT-SW  PIC X.
                           11  CL-PD-ASSIGN-RN-ACCTNO       PIC X.
                           11  CL-PD-APR-FINAMT  PIC S9(7)V99 COMP-3.
                           11  CL-PD-DLCOMM-PP-CMSI-FG      PIC X.
                           11  CL-PD-DLCOMM-O3-CMSI-FG      PIC X.

                       09  CL-PD-FILLER-TO-MATCH-LN   PIC X(90).

               05  CL-PD2-KEY.
                   07  CL-PD2-ID                      PIC X.
                   07  CL-PD2-BRANCH                  PIC 9(4).
                   07  CL-PD2-BWNAME                  PIC X(33).
