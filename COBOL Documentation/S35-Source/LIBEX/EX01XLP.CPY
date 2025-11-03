      * COPYMEMBER: LIBEX/EX01XLP
      **************************************************************************
      * PAYEMNT EXTRACTION FILE          623 BYTES
      *
      * DATA WAREHOUSE EXTRACTION
      *
      * REV:
      * BLV 2002.0214 ADDED 3 GL NUMBERS & AMTS FROM LX
      *  CS 2004.0903 ADDED XLP-REGION-NUMBER   PR#76  REGACC
      * BAH 2024.0311 ADDED XLP-REPAY-TRANS-ID #1641
      * BAH 2025.0115 ADDED XLP-CLOSFEE-REB-AMT #1697 S35Q-157
      **************************************************************************
       78  XLP-SIZE                      VALUE 623.
       01  XLP-REC.
           03  XLP-BRACCTNO              PIC 9(10).
           03  XLP-ACCTNO                PIC 9(08).
           03  XLP-SEQNO                 PIC 9(04).
           03  XLP-USERID                PIC X(10).
           03  XLP-POSTDATE              PIC 9(08).
           03  XLP-POSTTIME              PIC 9(06).
           03  XLP-TRDATE                PIC 9(08).
           03  XLP-PAYDATE               PIC 9(08).
           03  XLP-TRCD                  PIC X(02).
           03  XLP-REFNO                 PIC X(05).
           03  XLP-TRAMT                 PIC +9(07).99.
           03  XLP-APOTH                 PIC +9(07).99.
           03  XLP-APOT2                 PIC +9(07).99.
           03  XLP-APINT                 PIC +9(07).99.
           03  XLP-APLC                  PIC +9(07).99.
           03  XLP-APCUR                 PIC +9(07).99.
           03  XLP-OTHBAL                PIC +9(07).99.
           03  XLP-OT2BAL                PIC +9(07).99.
           03  XLP-INTBAL                PIC +9(07).99.
           03  XLP-LCBAL                 PIC +9(07).99.
           03  XLP-CURBAL                PIC +9(07).99.
           03  XLP-INTDUE                PIC +9(07).99.
           03  XLP-DLPROC                PIC +9(07).99.
           03  XLP-EARNED1               PIC +9(07).99.
           03  XLP-EARNED2               PIC +9(07).99.
           03  XLP-EARNED3               PIC +9(07).99.
           03  XLP-INTPAID               PIC +9(07).99.
           03  XLP-SSNO                  PIC  9(10).
           03  XLP-PDTH                  PIC  9(08).
           03  XLP-LCPDTH                PIC  9(08).
           03  XLP-TILLNO                PIC  X(01).
           03  XLP-APINTOWE              PIC +9(07).99.
           03  XLP-DLPARVHELD-ADJ        PIC +9(07).99.
           03  XLP-IBPC                  PIC X(01).
           03  XLP-MANUAL-RB             PIC X(01).
           03  XLP-REV                   PIC X(01).
           03  XLP-REVTRCD               PIC X(02).
           03  XLP-REVSEQNO              PIC 9(04).
           03  XLP-STATUSFG              PIC X(01).
           03  XLP-NEWSTAT-RATE          PIC 9(03).999.
           03  XLP-LCPARTIALS            PIC +9(07).99.
           03  XLP-LCPAID                PIC +9(07).99.
           03  XLP-DLRVCHGS-ADJ1         PIC +9(07).99.
           03  XLP-DLRVCHGS-ADJ2         PIC +9(07).99.
           03  XLP-DLRVCHGS-ADJ3         PIC +9(07).99.
           03  XLP-DLPARVPAID-ADJ        PIC +9(07).99.
           03  XLP-DLNO                  PIC 9(04).99.
           03  XLP-PREPAY-PENALTY        PIC +9(07).99.
           03  XLP-PRORATED-INS-REB      PIC +9(07).99.
           03  XLP-POSTING-BR            PIC 9(04).
           03  XLP-NOLCHG                PIC +9(03).
           03  XLP-COLLECTOR             PIC X(03).
           03  XLP-EXTRA-PRIN-LFP        PIC +9(07).99.
           03  XLP-REMAIN-INT-SINCE-LFP  PIC +9(07).99.
           03  XLP-REMAIN-PRIN-SINCE-LFP PIC +9(07).99.
           03  XLP-GLNO1                 PIC 9(11).
           03  XLP-GLAMT1                PIC +9(05).99.
           03  XLP-GLNO2                 PIC 9(11).
           03  XLP-GLAMT2                PIC +9(05).99.
           03  XLP-GLNO3                 PIC 9(11).
           03  XLP-GLAMT3                PIC +9(05).99.
           03  XLP-REGION-NUMBER         PIC 9(2).
           03  XLP-REPAY-TRANS-ID        PIC 9(10).
           03  XLP-CLOSFEE-REB-AMT       PIC +9(07).99.
           03  XLP-UNUSED                PIC X(77).
           03  XLP-CR                    PIC X(01).
           03  XLP-LF                    PIC X(01).
