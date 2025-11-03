      * COPYMEMBER: LIBEX/EX01XTRP
      **************************************************************************
      * PAYMENT TRANSACTION EXTRACTION FILE      1056 BYTES
      *
      *******
      * NOTE: MAKE SURE TO ADJUST LPFDREPAYTRP AND UPDATEREPAYTRP WHEN CHANGING
      *******
      *
      *  DATA WAREHOUSE EXTRACTION
      *
      * REV:
      * BLV 021015 UPDATE FIELDS, INS-TAX-RB, PROMDAYS-PASTDUE, MIP FIELDS
      *  CS 040907 ADD XTRP-REGION-NUMBER [PR#76 REGACC]
      * BAH 060620 ADDED XTRP-LINE-3 TO HOLD TRP-LINE, ITS COMP IN THE LIVE
      *            COPY MEMBER WHICH ALLOWS IT TO HOLD UP TO 255 (FF)
      *            WAS TRUNCATING IN HERE WHEN MOVING TO XTRP-LINE 9(2)
      *            CAUSING DUP KEYS IN THE WAREHOUSE, REGACC PL# 541
      * BAH 110809 ADDED XTRP-LP-SEQNO REGACC PR# 295
      *  CS 120807 ADDED XTRP-PRIOR-DF-CODE & XTRP-PRIOR-DF-POSTDATE REGACC
      *                                                        PR#318
      * BAH  20220421 ADD XTRP-ACTIONCD2, SECURITIZATION #1551
      * BAH  20220923 ADDED XTRP-LCDUE #1570 VERITEC
      * BAH  20240209 ADD XTRP-REPAY-TRANS-ID AND FILLER #1641
      * BAH  20250109 ADD XTRP-CLOSFEE-REB-AMT #1697 S35Q-157
      **************************************************************************
       01  XTRP-REC.
           03  XTRP-BRACCTNO             PIC 9(10).
           03  XTRP-BRNO                 PIC 9(04).
           03  XTRP-ACCTNO               PIC 9(06).
           03  XTRP-TRANS-DATE           PIC 9(08).
           03  XTRP-CODE                 PIC X(01).
           03  XTRP-CLASS                PIC 9(02).
           03  XTRP-LINE                 PIC 9(02).
           03  XTRP-LAWCODE              PIC 9(02).
           03  XTRP-JDCD                 PIC X(01).
           03  XTRP-PLCD                 PIC X(01).
           03  XTRP-MORELN               PIC X(01).
           03  XTRP-SSNO                 PIC 9(10).
           03  XTRP-MAKER                PIC X(01).
           03  XTRP-IBPC                 PIC X(01).
           03  XTRP-TRCD                 PIC X(02).
           03  XTRP-REFNO                PIC X(5).
           03  XTRP-PAYDATE              PIC 9(08).
           03  XTRP-DUEDATE              PIC 9(08).
           03  XTRP-TRAMT                PIC +9(07).99.
           03  XTRP-INTDUE               PIC +9(07).99.
           03  XTRP-APOTH                PIC +9(07).99.
           03  XTRP-APOT2                PIC +9(07).99.
           03  XTRP-APINT                PIC +9(07).99.
           03  XTRP-APLC                 PIC +9(07).99.
           03  XTRP-APCUR                PIC +9(07).99.
           03  XTRP-GLTAB.
               05  XTRP-GLNO1            PIC 9(11).
               05  XTRP-GLAMT1           PIC +9(05).99.
               05  XTRP-GLNO2            PIC 9(11).
               05  XTRP-GLAMT2           PIC +9(05).99.
               05  XTRP-GLNO3            PIC 9(11).
               05  XTRP-GLAMT3           PIC +9(05).99.
           03  XTRP-GLTABLE REDEFINES XTRP-GLTAB OCCURS 3.
               05  XTRP-GLNO             PIC 9(11).
               05  XTRP-GLAMT            PIC +9(05).99.
      *-------------------------------------------------
           03  XTRP-EARNINGS-TAB.
               05  XTRP-EARNINGS1        PIC +9(07).99.
               05  XTRP-EARNINGS2        PIC +9(07).99.
               05  XTRP-EARNINGS3        PIC +9(07).99.
               05  XTRP-EARNINGS4        PIC +9(07).99.
               05  XTRP-EARNINGS5        PIC +9(07).99.
               05  XTRP-EARNINGS6        PIC +9(07).99.
           03  XTRP-EARNINGS-TABLE REDEFINES XTRP-EARNINGS-TAB.
               05  XTRP-EARNINGS         PIC +9(07).99 OCCURS 6.
           03  XTRP-OTHBAL               PIC +9(07).99.
           03  XTRP-OT2BAL               PIC +9(07).99.
           03  XTRP-INTBAL               PIC +9(07).99.
           03  XTRP-LCBAL                PIC +9(07).99.
           03  XTRP-CURBAL               PIC +9(07).99.
           03  XTRP-PDTHRU               PIC 9(08).
           03  XTRP-DLNO                 PIC 9(4).99.
           03  XTRP-DLPROC               PIC +9(7).99.
           03  XTRP-SERVACCT             PIC X(01).
           03  XTRP-SALEFIN              PIC X(01).
           03  XTRP-INSURED              PIC X(01).
           03  XTRP-INSOURS              PIC X(01).
           03  XTRP-LOANTYPE             PIC X(01).
           03  XTRP-TILLNO               PIC X(01).
           03  XTRP-LCPD                 PIC 9(08).
           03  XTRP-WORKAMT1             PIC +9(07).99.
           03  XTRP-WORKAMT2             PIC +9(07).99.
           03  XTRP-WORKAMT3             PIC +9(07).99.
           03  XTRP-WORKAMT4             PIC +9(07).99.
           03  XTRP-WORKAMT5             PIC +9(07).99.
           03  XTRP-WORKAMT6             PIC +9(07).99.
           03  XTRP-POSTING-BR           PIC 9(04).
           03  XTRP-OTHBR-FG             PIC X(01).
           03  XTRP-REV                  PIC X(01).
           03  XTRP-REVTRCD              PIC X(02).
           03  XTRP-REVSEQNO             PIC 9(04).
           03  XTRP-STATUSFG             PIC X(01).
           03  XTRP-KEEPAPINT            PIC X(01).
           03  XTRP-INTPAID              PIC +9(07).99.
           03  XTRP-PRIOR-POOLID         PIC 9(05).
           03  XTRP-INSTYPES             PIC X(01).
           03  XTRP-DLRVPRIN1            PIC +9(07).99.
           03  XTRP-DLPARVPAID           PIC +9(07).99.
           03  XTRP-DLPARVHELD           PIC +9(07).99.
           03  XTRP-DLPARVHELD-ADJ       PIC +9(07).99.
           03  XTRP-DLPARVPAID-ADJ       PIC +9(07).99.
           03  XTRP-NEWSTAT-RATE         PIC 9(03).999.
           03  XTRP-POCD                 PIC X(02).
           03  XTRP-PODATE               PIC 9(08).
           03  XTRP-DL-RVCHGS-CODE1      PIC X(01).
           03  XTRP-DL-RVCHGS-CODE2      PIC X(01).
           03  XTRP-DL-RVCHGS-CODE3      PIC X(01).
           03  XTRP-DL-PARVHELD-CODE     PIC X(01).
           03  XTRP-DL-PARVPAID-CODE     PIC X(01).
           03  XTRP-TOTNODEF             PIC +9(03).
           03  XTRP-LNAMT                PIC +9(06).99.
           03  XTRP-ORIG-1STPAY          PIC 9(08).
           03  XTRP-OLD1ST-PYDATE        PIC 9(08).
           03  XTRP-1STPYDATE            PIC 9(08).
           03  XTRP-ORGTERM              PIC 9(03).
           03  XTRP-INSCOMM              PIC +9(07).99.
           03  XTRP-MANUAL-RB            PIC X(01).
           03  XTRP-USERID               PIC X(10).
           03  XTRP-POSTTIME             PIC 9(06).
           03  XTRP-PLTYPE               PIC X(01).
           03  XTRP-PL-OTH-AR            PIC +9(07).99.
           03  XTRP-LP-DLNO              PIC 9(04).99.
           03  XTRP-REPOCD               PIC X(01).
           03  XTRP-SPRCLASS             PIC 9(02).
           03  XTRP-MAX-DEFER-FG         PIC X(01).
           03  XTRP-POSTDATE             PIC 9(08).
           03  XTRP-UNITPER-CD           PIC X(01).
           03  XTRP-UNITPER-FREQ         PIC 9(03).
           03  XTRP-REVOLVING            PIC X(01).
           03  XTRP-RINS-TAB.
               05  XTRP-RINSPREM1        PIC +9(05).99.
               05  XTRP-RINSCOMM1        PIC +9(05).99.
               05  XTRP-RINSPREM2        PIC +9(05).99.
               05  XTRP-RINSCOMM2        PIC +9(05).99.
               05  XTRP-RINSPREM3        PIC +9(05).99.
               05  XTRP-RINSCOMM3        PIC +9(05).99.
               05  XTRP-RINSPREM4        PIC +9(05).99.
               05  XTRP-RINSCOMM4        PIC +9(05).99.
           03  XTRP-RINS REDEFINES XTRP-RINS-TAB OCCURS 4.
               05  XTRP-RINSPREM         PIC +9(05).99.
               05  XTRP-RINSCOMM         PIC +9(05).99.
           03  XTRP-APOTH-MEMBERSHIP     PIC +9(05).99.
           03  XTRP-APOTH-NSF            PIC +9(05).99.
           03  XTRP-APOTH-MISC1          PIC +9(05).99.
           03  XTRP-APOTH-MISC2          PIC +9(05).99.
           03  XTRP-APOTH-MISC3          PIC +9(05).99.
           03  XTRP-APOTH-MISC4          PIC +9(05).99.
           03  XTRP-APOTH-MISC5          PIC +9(05).99.
           03  XTRP-APOTH-MISC6          PIC +9(05).99.
           03  XTRP-SP-DEFFRMLA          PIC 9(02).
           03  XTRP-COLLECTOR            PIC X(03).
           03  XTRP-PENALTY-INT          PIC +9(07).99.
           03  XTRP-INSCOMP              PIC 9(02).
           03  XTRP-PROMDATE             PIC 9(08).
           03  XTRP-PROMAMT              PIC +9(07).99.
           03  XTRP-PROMID               PIC X(03).

           03  XTRP-INS-TAX-RB           PIC +9(7).99.
           03  XTRP-PROMDAYS-PASTDUE     PIC +9(3).
           03  XTRP-MIP-FG               PIC X.
           03  XTRP-MIP-CLPAID           PIC +9(7).99.
           03  XTRP-MIP-CLCOMM           PIC +9(7).99.
           03  XTRP-MIP-AHPAID           PIC +9(7).99.
           03  XTRP-MIP-AHCOMM           PIC +9(7).99.
           03  XTRP-MIP-CLPDTH           PIC 9(8).
           03  XTRP-MIP-AHPDTH           PIC 9(8).
           03  XTRP-REGION-NUMBER        PIC 9(2).
           03  XTRP-LINE-3               PIC 9(3).
           03  XTRP-LP-SEQNO             PIC 9(4).
           03  XTRP-PRIOR-DF-CODE        PIC 9(2).
           03  XTRP-PRIOR-DF-POSTDATE    PIC 9(8).
           03  XTRP-ACTIONCD2            PIC X(4).
           03  XTRP-LCDUE                PIC +9(07).99.
           03  XTRP-REPAY-TRANS-ID       PIC 9(10).
           03  XTRP-CLOSFEE-REB-AMT      PIC +9(07).99.
           03  FILLER                    PIC X(89).
           03  XTRP-CR                   PIC X(01).
           03  XTRP-LF                   PIC X(01).

