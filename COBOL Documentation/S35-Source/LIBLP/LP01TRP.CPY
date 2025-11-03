      * COPYMEMBER: LIBLP/LPFDTRP.CPY
      ******************************************************************
      *
      * DESCRIPTION:  TRANSACTION FILE [PAYMENTS] "P", "F", "G"
      *
      *   REMOVED TP-CODE (8/10/2021) AND MOVED POOLID OUT OF KEY
      *   IF A PAYMENT WITH POOLID, TP-POOLID WILL HAVE A VALUE (WAS "F")
      *   IF A "SP" TRCD, TP-POOLID WILL BE SET                 (WAS "G")
      *
      * FILE INFO  :  PAYMENT RECORDS
      *               LP-TRPFILE
      *
      * TRP1-KEY = TP-BRNO,TP-DATE,TP-CLASS,TP-ACCTNO,TP-LINE
      * TRP2-KEY = TP-DATE,TP-BRNO,TP-CLASS,TP-ACCTNO,TP-LINE
      *
      *=================================================================
      *
      * REV:
      * BLL 061595 ADD TP-MAX-DEFER-FG
      * MJD 062795 ADDED TP-POSTDATE
      * BAH 083095 ADDED TP-INSTYPES-VALID AND ALL O3 CODES
      * MJD 071896 ADDED TP-UNITPER-CD, TP-UNITPER-FREQ
      * BAH 072596 CHANGED THE FILLER AT TOP OF TP-TRF-KEY TO 4 FROM 3
      *             WHEN WE EXPANDED BRNO, WE DIDNT FIX THE POOL KEY !!
      * BAH 060498 EXPANDED S9(5) FIELDS TO S9(7) FIELDS LIKE LP FILE,
      *             REDUCED FILLER BY 22 BYTES
      * BAH 062498 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X.
      * JTG 082698 CHANGED TP-PRINBAL TO TP-CURBAL, REGENCY REVOLVING
      * JTG 093098 ALLOWED LN-CURBAL TO GO NEGATIVE; REGENCY REVOL
      * JTG 110998 ADDED TP-REVOLVING, REGENCY REVOLVING
      *             AND 03  TP-DLRSV-REVOL-TBL.
      *                     05  TP-DLRVPRIN1    PIC S9(7)V99 COMP-3.
      *                     05  TP-DLPARVPAID   PIC S9(7)V99 COMP-3.
      *                     05  TP-DLPARVHELD   PIC S9(7)V99 COMP-3.
      *                 REDEFINITION OF TP-DLRSV-REVOL-TBL
      * JTG 120398 ADDED TP-RINSPREM(4) TP-RINSCOMM(4) REVOLVING REGENCY
      * JTG 120798 ADDED TP-APOTH-AMT   OCCURS 8
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      * JTG 000417 ADDED TP-SP-DEFFRMLA, RE: NEW DEFERMENT FORMULA 97 OF
      *             INTDUE OR 1/3 OF REGPAY MERCURY #224
      * MJD 001016 ADDED TP-COLLECTOR (BAY PR#1317)
      * MJD 001113 ADDED TP-PENALTY-INT : INTEREST COLLECTED ON
      *             PENALTIES (NEWS PR#1264)
      * BAH 011108 ADDED TP-INSCOMP FOR THE ADDONS IN THE MORP'S.
      *            PUT ON ADDON, REVERSE, ADDON WITH DIFFERENT COMPANY,
      *             THEY ALL REPORT WITH THE LAST COMPANY
      *            PR#1617 REGACC
      * BAH 011210 ADDED TP-PROMDATE, TP-PROMAMT, TP-PROMID TO TRACK
      *             WHEN THESE GOT CLEARED IN THE LOAN, REGACC PR# 1662
      * JTG 020322 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            ADDED TP-INS-TAX-RB                        WORLD #288
      * BAH 020502 ADDED TP-PROMDAYS-PASTDUE FOR "DAYS PAST DUE AT TIME
      *             PROMISE TAKEN", REGACC PR# 1783
      * JTG 020722 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS
      *             TP-MIP-FG          PIC X.
      *             TP-MIP-CLPAID      PIC S9(7)V99  COMP-3.
      *             TP-MIP-CLCOMM      PIC S9(7)V99  COMP-3.
      *             TP-MIP-AHPAID      PIC S9(7)V99  COMP-3.
      *             TP-MIP-AHCOMM      PIC S9(7)V99  COMP-3.
      *             TP-MIP-CLPDTH-DATE      PIC 9(6)      COMP-6.
      *             TP-MIP-AHPDTH-DATE      PIC 9(6)      COMP-6.
      *            REGENCY #1801
      *  CS 021125 ADDED TP-INS-TAX-COUNTY-CD, TP-INS-TAX-TITY-CD FOR
      *             TAXING AUTHORITY ON INSURANCES WRITTEN
      *            WORLD PR#288
      * BLM 100304 ADDED TP-APPLIED-TO-ESCROW, TP-ESCROW-AMT,
      *             TP-ESCROW-PREPAID, TP-ESCROW-DISBURSED
      *            REGWAN PR# 3506
      * BAH 110801 ADDED TP-LP-SEQNO FOR REGACC ONLY , PR# 295
      *  CS 120806 ADDED 88 LEVELS FOR TP-MAX-DEFER-FG.
      *            REGACC WANTS DIFFERENT FLAGS TO DISTINGUISH WHEN
      *             POLICY WAS OVERRIDDEN, PREVIOUSLY WE ONLY FLAGGED
      *             IF YOU EXCEEDED MAXIMUM DEFERMENTS, NOW
      *             '1' IS DF POSTED WITHIN 1ST XMONTHS, "2" IS 'D2'
      *             POSTED WITHIN MONTHS BETWEENS D2, 'D' IS DF POSTED
      *             WITHIN MONTHS BETWEEN                 REGACC PR#318
      *  CS 120806 ADDED TP-PRIOR-DF-CODE(LAST UNREVERESED DF - D9,
      *             TP-PRIOR-DF-POSTDATE DATEOF LAST UNREVERSED DF-D9,
      *             FIELD IS ONLY UPDATED WHEN A 'DF' THRU 'D9' IS
      *             POSTED                                REGACC PR#318
      *  CS 150728 ADD TP-CANCEL-REASON, FILLER FROM 56 TO 53
      *             KSTREET #4770
      *  CS 160916 REMOVE TP-SEQNO
      *             TP-CLASS 9(2) TO 9(3)
      *             NEW TP-ACCTNO, TP-NUMBER IS REDEFINE
      *             CHANGED TP-LINE 9(2) TO 9(3)
      *             CHANGED TP-PAYDATE 9(6) TO 9(8)
      *                     TP-DUEDATE 9(6) TO 9(8)                  
      *                     TP-EARNINGS OCCURS 6 TO 10  
      *                     TP-WORKAMT OCCURS 6 TO 10 
      *             ADD TP-INSEARN          OCCURS 10 TIMES
      *                 TP-INSWORKAMT       OCCURS 10 TIMES
      *             CHANGED TP-PDTHRU  9(6) TO 9(8)                 
      *             CHANGED TP-LCPD 9(6) TO 9(8)  
      *             CHANGEDD TP-REVSEQNO 9(4) TO 9(5) 
      *             CHANGED TP-TILLNO FROM X TO 9(2)
      *             TP-ORIG-1STPAY 9(6) TO  9(8) 
      *             TP-OLD1ST-PYDATE 9(6) TO 9(8)          
      *             TP-INSCOMM  9(7)V99 TO  S9(5)V99 
      *             TP-1STPYDATE 9(6) TO 9(8)               
      *             CHANGED TP-SPRCLASS 9(2) TO 9(3)
      *             CHANGED TP-POSTDATE 9(6) TO 9(8)                    
      *             CHANGED TP-RINS OCCURS 4 TO   OCCURS 10
      *                TP-RINSPREM         PIC S9(7)V99
      *                TP-RINSCOMM         PIC S9(7)V99 
      *             CHANGED TP-APOTH-AMT-TABLE AMOUNTS TO S9(7)V99
      *             CHANGED  TP-PROMDATE 9(6) TO 9(8)
      *             CHANGED TP-MIP-CLPDTH-DATE 9(6) TO 9(8)                  
      *             CHANGED TP-APPLIED-TO-ESCROW TO  PIC S9(7)V99 
      *             CHANGED TP-MIP-AHPDTH-DATE 9(6) TO 9(8)                  
      *             CHANGED TP-MIP-AHPDTH-DATE 9(6) TO 9(8)                  
      *             CHANGED TP-LP-SEQNO 9(4) TO 9(5)
      *             CHANGED TP-PRIOR-DF-POSTDATE 9(6) TO 9(8)
      *             *IS IN TP-REFNO BUT REFNO IS ONLY X(5)
      *             ADDED TP-FROMCL  PIC 9(3)  TP-REFNO ONLY 5
      *                   TP-TOCL    PIC 9(3)                     
      *             SAVED VERSION LIBLP/LPFDTRP.160921
      *  CS 160923 CHANGED TP-DLRVCHGS-ADJ(OCCURS 3) FROM A REDEFINE OF
      *             TP-DLRSV-REVOL-TBLE TO SEPARATE FIELDS
      *
      * KEC 2016.1028 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED TP-ORIG-1STPAY TO TP-ORIG-1STPYDATE.
      * BAH 20190625 REMOVED TP-REVOLVING, PT0153
      * BAH 20200325 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20200508 REMOVED TP-UNITPER-INFO 03 LEVEL
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 20210811 REMOVED TP-CODE, MOVE TP-POOLID OUT OF KEY
      * BAH 20220420 ADD TP-ACTIONCD2, SECURITIZATION #1551
      * BAH 20220912 ADD TP-LCDUE FOR VERITEC "IL", LATE CHARGE OWING WERE
      *              NEVER SEPARATED #1570
      *
      * JKC 2023.0516 REVISED THE 'OCCURS' AFTER THE 'PIC' ON 
      *          TP-EARNINGS AND TP-WORKAMT; NEED TO DO THAT FOR
      *          THE SQL-PGM-COL.SH SCRIPT TO FIND THE FIELD NAME.
      * BAH 20230612 ADDED FILLER
      * BAH 20240212 ADDED TP-REPAY-TRANS-ID FOR APIPAY #1641
      * BAH 20250115 ADDED TP-CLOSFEE-REB-AMT FOR THE 4% GA CLOSING FEE
      *              PORTION REFUND FOR GATXEX #1697 S35Q-157
      ******************************************************************
       78  TRP-SIZE                    VALUE 850.
       01  TRP-REC.
           03  TRP1-KEY.
               05  TP-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TP-DATE             PIC 9(8).
               05  TP-CLASS            PIC 999.
               05  TP-ACCTNO           PIC 9(8).
               05  TP-LINE             PIC 999.   
      *-------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  TP-LTOUCH-DATE      PIC 9(8) COMP-3.
           03  TP-LAWCODE          PIC 99.
           03  TP-JDCD             PIC X.
           03  TP-PLCD             PIC X.

           03  TP-SSNO             PIC 9(10)    COMP-3.
           03  TP-MAKER            PIC X.
           03  TP-IBPC             PIC X.
           03  TP-TRCD             PIC XX.
           03  TP-TRCDX REDEFINES TP-TRCD.
               05  TP-TRCD1        PIC X.
               05  TP-TRCD2        PIC X.

           03  TP-REFNO            PIC X(5).
           03  TP-ADDON           REDEFINES TP-REFNO.
               05  TP-ADDON-TEXT   PIC X(3).
               05  TP-ADDON-TYPE   PIC X(2).
           03  FILLER             REDEFINES TP-REFNO.
               05  TP-DLRVNO       PIC 9.
               05  FILLER          PIC X(4).
           03  FILLER             REDEFINES TP-REFNO.
               05  TP-P4P5-TRCD    PIC XX.
               05  TP-P4P5-DLRVNO  PIC 9.
               05  FILLER          PIC XX.

      $XFD DATE=YYYYMMDD
           03  TP-PAYDATE          PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  TP-DUEDATE          PIC 9(8)     COMP-3.
      *-------------------------------------------------
           03  TP-TRAMT            PIC S9(7)V99 COMP-3.
           03  TP-INTDUE           PIC S9(7)V99 COMP-3.
           03  TP-APOTH            PIC S9(7)V99 COMP-3.
           03  TP-APOT2            PIC S9(7)V99 COMP-3.
           03  TP-APINT            PIC S9(7)V99 COMP-3.
           03  TP-APLC             PIC S9(7)V99 COMP-3.
           03  TP-APCUR            PIC S9(7)V99 COMP-3.
      *-------------------------------------------------
           03  TP-GLTABLE          OCCURS 3.
               05  TP-GLNO         PIC 9(11)    COMP-3.
               05  TP-GLAMT        PIC S9(7)V99 COMP-3.
      *-------------------------------------------------

           03  TP-EARNINGS         PIC S9(7)V99 COMP-3
                                   OCCURS 10.

           03  TP-WORKAMT          PIC S9(7)V99 COMP-3
                                   OCCURS 10.

           03  TP-OTHBAL           PIC S9(7)V99 COMP-3.
           03  TP-OT2BAL           PIC S9(7)V99 COMP-3.
           03  TP-INTBAL           PIC S9(7)V99 COMP-3.
           03  TP-LCBAL            PIC S9(7)V99 COMP-3.
           03  TP-CURBAL           PIC S9(7)V99 COMP-3.

      $XFD DATE=YYYYMMDD
           03  TP-PDTH-DATE      PIC 9(8)     COMP-3.
      *-------------------------------------------------
           03  TP-DLNO             PIC 9(4)V99  COMP-3.
           03  TP-DLPROC           PIC S9(7)V99 COMP-3.
      *-------------------------------------------------
           03  TP-SERVACCT         PIC X.
           03  TP-SALEFIN          PIC X.
           03  TP-INSURED          PIC X.
           03  TP-INSOURS          PIC X.
           03  TP-LOANTYPE         PIC X.
           03  TP-TILLNO           PIC 9(2).

      $XFD DATE=YYYYMMDD
           03  TP-LCPDTH-DATE      PIC 9(8)     COMP-3.
           03  TP-POSTING-BR       PIC 9(4)     COMP-3.
      * TP-OTHBR-FG "G" OWNING BRANCH
      * TP-OTHBR-FG "C" POSTING BRANCH
           03  TP-OTHBR-FG         PIC X.
           03  TP-REV              PIC X.
           03  TP-REVTRCD          PIC XX.
      *%%CHANGE TP-REVSEQNO FROM 9(4) TO 9(5)
           03  TP-REVSEQNO         PIC 9(5)     COMP-3.
           03  TP-STATUSFG         PIC X.
           03  TP-KEEPAPINT        PIC X.
           03  TP-INTPAID          PIC S9(7)V99 COMP-3.
           03  TP-PRIOR-POOLID     PIC 9(5)     COMP-3.
           03  TP-INSTYPES         PIC X.
               88  TP-INSTYPES-DL      VALUE "C"
                                             "G" "I" "J" "M" "P"
                                             "T" "X" "Y" "5" "6"
                                             "7".
               88  TP-INSTYPES-LL      VALUE "B"
                                             "A" "F" "H" "L" "O"
                                             "S" "U" "W" "2" "3"
                                             "4".
               88  TP-INSTYPES-AH      VALUE "D"
                                             "F" "G" "K" "L" "M"
                                             "O" "P" "Q" "V" "W"
                                             "X" "3" "4" "6" "7"
                                             "9" "0".
               88  TP-INSTYPES-PP      VALUE "E"
                                             "H" "J" "K" "L" "M"
                                             "U" "Y" "Z" "3" "6"
                                             "9".
               88  TP-INSTYPES-PP2     VALUE "R"
                                             "S" "T" "V" "W" "X"
                                             "2" "4" "5" "7" "8"
                                             "0".
               88  TP-INSTYPES-O3      VALUE "1"
                                             "A" "I" "O" "P" "Q"
                                             "U" "Y" "Z" "2" "3"
                                             "4" "5" "6" "7" "8"
                                             "9" "0".
               88  TP-INSTYPES-VALID   VALUE "A" THRU "Z"
                                             "0" "1" "2" "3" "4"
                                             "5" "6" "7" "8" "9".
      *% NEW INSURANCE EARNINGS, EVEN THOUGH INSURANCE EARNINGS ARE COMING FROM
      *% AN ACCRUAL REPORT AT MONTH END, THEY NEED TO WORK THE SAME AS
      * EARNINGS ON REBATES OF COMPONENTS. BELIEVE WE NEED THE WORK AMOUNT
      *SAME AS EARNINGS, %% WORK AMOUNT IS REBATE, AND THEN EARNINGS
           03  TP-INSEARN          PIC S9(7)V99 COMP-3 OCCURS 10.
           03  TP-INSWORKAMT       PIC S9(7)V99 COMP-3 OCCURS 10.

      *REMOVE REDEFINE OF TP-DLRSV-REVOL-TBL AND MADE SEPARATE
           03  TP-DLRVCHGS-ADJ     PIC S9(7)V99 COMP-3 OCCURS 3.
           03  TP-DLPARVHELD-ADJ   PIC S9(7)V99 COMP-3.
           03  TP-DLPARVPAID-ADJ   PIC S9(7)V99 COMP-3.
           03  TP-NEWSTAT-RATE     PIC 999V999  COMP-3.
           03  TP-POCD             PIC XX.

      $XFD DATE=YYYYMMDD
           03  TP-PODATE           PIC 9(8)     COMP-3.
           03  TP-DL-RVCHGS-CODE   OCCURS 3
                                   PIC X.
           03  TP-DL-PARVHELD-CODE PIC X.
           03  TP-DL-PARVPAID-CODE PIC X.
           03  TP-TOTNODEF         PIC S999     COMP-3.
           03  TP-LNAMT            PIC S9(7)V99 COMP-3.
      $XFD DATE=YYYYMMDD
           03  TP-ORIG-1STPYDATE   PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
           03  TP-OLD1ST-PYDATE    PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
           03  TP-1STPYDATE        PIC 9(8) COMP-3.
           03  TP-ORGTERM          PIC 9(3) COMP-3.

           03  TP-INSCOMM          PIC S9(5)V99 COMP-3.
           03  TP-MANUAL-RB        PIC X.
           03  TP-USERID           PIC X(10).
           03  TP-POSTTIME         PIC 9(6) COMP-3.
           03  TP-PLTYPE           PIC X.
           03  TP-PL-OTH-AR        PIC S9(7)V99 COMP-3.
           03  TP-LP-DLNO          PIC 9(4)V99  COMP-3.
           03  TP-REPOCD           PIC X.

           03  TP-SPRCLASS         PIC 999      COMP-3.
           03  TP-MAX-DEFER-FG     PIC X.
               88  TP-1ST-XMONTHS         VALUE "X".
               88  TP-MONTHS-D2           VALUE "2".
               88  TP-MONTHS-DF           VALUE "D".
               88  TP-EXCEEDS-DF-POL      VALUE "Y".

      $XFD DATE=YYYYMMDD
           03  TP-POSTDATE             PIC 9(8)     COMP-3.
           03  TP-UNITPER-CD           PIC X.
               88  TP-UNITPER-CD-DAYS    VALUE "D" "S" "W" "B".
               88  TP-UNITPER-CD-MONTHS  VALUE "M" "Y".
           03  TP-UNITPER-FREQ         PIC 9(3)     COMP-3.
           03  TP-SP-DEFFRMLA          PIC 99.                
           03  TP-COLLECTOR            PIC X(3).
      *%THIS I WHERE LP-PREPAYMENT-PENALTY IS STORED, MS PENALTY (CL/REBA01)
           03  TP-PENALTY-INT          PIC S9(7)V99 COMP-3.
           03  TP-INSCOMP              PIC 99.

      $XFD DATE=YYYYMMDD
           03  TP-PROMDATE             PIC 9(8)     COMP-3.
           03  TP-PROMAMT              PIC S9(7)V99 COMP-3.
           03  TP-PROMID               PIC X(3).
           03  TP-INS-TAX-RB           PIC S9(7)V99 COMP-3.
           03  TP-PROMDAYS-PASTDUE     PIC S9(3)    COMP-3.
           03  TP-MIP-FG               PIC X.
           03  TP-MIP-CLPAID           PIC S9(7)V99 COMP-3.
           03  TP-MIP-CLCOMM           PIC S9(7)V99 COMP-3.
           03  TP-MIP-AHPAID           PIC S9(7)V99 COMP-3.
           03  TP-MIP-AHCOMM           PIC S9(7)V99 COMP-3.
      $XFD DATE=YYYYMMDD
           03  TP-MIP-CLPDTH-DATE      PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  TP-MIP-AHPDTH-DATE      PIC 9(8)     COMP-3.
           03  TP-INS-TAX-COUNTY-CD    PIC X(3).
           03  TP-INS-TAX-CITY-CD      PIC X(3).

           03  TP-APPLIED-TO-ESCROW    PIC S9(7)V99  COMP-3.
           03  TP-ESCROW-AMT           PIC S9(7)V99  COMP-3.
           03  TP-ESCROW-PREPAID       PIC S9(7)V99  COMP-3.
           03  TP-ESCROW-DISBURSED     PIC S9(7)V99  COMP-3.

           03  TP-LP-SEQNO             PIC 9(5)      COMP-3.

           03  TP-PRIOR-DF-CODE        PIC X(2).
      $XFD DATE=YYYYMMDD
           03  TP-PRIOR-DF-POSTDATE    PIC 9(8) COMP-3.
           03  TP-CANCEL-REASON        PIC X(3).
           03  TP-FROMCL               PIC 9(3) COMP-3.
           03  TP-TOCL                 PIC 9(3) COMP-3.
      
           03  TP-ADDON-TXT       PIC X(3).
           03  TP-ADDON-TYP       PIC X(2).

           03  TP-DLRVNUM         PIC 9.
           03  TP-P4P5-TRANCD     PIC XX.
           03  TP-P4P5-DLRVNUM    PIC 9.
           03  TP-POOLID          PIC 9(5).
           03  TP-ACTIONCD2       PIC X(4).
           03  TP-LCDUE           PIC S9(7)V99 COMP-3.
           03  TP-REPAY-TRANS-ID  PIC 9(10)    COMP-3.
           03  TP-CLOSFEE-REB-AMT PIC S9(7)V99 COMP-3.
           03  FILLER             PIC X(204).
