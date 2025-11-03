      * COPYMEMBER: LIBLP/LPPFBUF
      ********************************************************************
      *                   (LIBLP/LPPFBUF)
      *    LONPF'N'      INTERPROGRAM MEMORY BUFFER
      *    WHEN ADDING TO A NEW PROGRAM, UPDATE THIS HEADER
      *    USED IN LONPF1,
      *
      *
      * REV:
      *  JTG 050289 ADDED BP-TRCD FOR BATCH PAYMENTS
      *  SLC 051289 ADDED HOLD-NEW-1ST-DUE FOR "DC".
      *  JTG 051589 ADDED HOLD-DL-PARVHELD-CODE + MORE FILLER
      *  JTG 102489 ADDED HOLD-PRDELINQAMT
      *  JTG 081090 ADDED ADDON-INT-REBATE & ADDON-INT-EARNED
      *             FOR MULLEN
      *  JTG 073192 ADDED INTERRUPT-FG
      *  BAH 030293 REMOVED PRINT-RECEIPT
      *  JTG 071995 ADDED HOLD-PRPDUE-DAYS
      *  SLC 072195 ADDED CONV-REV-FLAG FOR REVERSING CONVERTED TRANSACTIONS.
      *  JTG 081695 ADDED CONV-AMT-PAID-LAST CONV-DATE-PAID-LAST
      *                   CONV-LCPDTH-DATE CONV-INTPDTH
      *  JTG 042996 ADDED HOLD-AFPDLLP RAC----AFP ACCOUNTS)
      *  JTG 101196 ADDED HOLD-EARNINGS-IB-OWING
      *                   HOLD-EARNINGS-IB-ERNINT
      *                   HOLD-EARNINGS-IB
      *               FOR SP-ERNFRMLA 15/16 REAL INTEREST BEARING METHOD
      *                                      - INSTANT AUTO, KY
      *                                      - FAIRCO, FL
      *  JTG 060397 EXPANDED THE FOLLOWING FOR 03,04,05
      *             03  REB-AMOUNT        OCCURS 10  -> 13
      *             03  U-INSOURS         OCCURS 5   -> 8
      *             03  INS-RB-TBL        OCCURS 5   -> 8
      *  BAH 100997 CHANGED REB-AMOUNT FROM S9(5)V99 TO S9(7)V99 FOR LARGE
      *             REBATES OF INTEREST, STORAGE SIZE STILL 4 BYTES..
      *  BAH 021798 EXPANDED HOLD-LP1-SEQNO FROM 9(3) TO 9(4).
      *  JTG 110598 EXPANDED BUFFER TO 500 BYTES FOR REVOLVING AND
      *             ADDED HOLD-RADDON FIELDS AND ADDED "UP5"
      *  JTG 120198 ADDED HOLD-APOTH-AMT FIELDS FOR REVOLVING PAYMENTS
      *             AND EXPANDED TO 720 BYTES
      *  MJD 990201 ADDED 12 BYTES TO FILLER, SIZE WAS 708, SCREEN
      *             BUFFERS WERE 720.
      *  JTG 990701 ADDED HOLD-PAYEE HOLD-REASON REVOLVING ADVANCE
      *  JTG 991123 ADDED HOLD-ONESTEP-RN-BUF FOR ONE STEP RENEWALS
      *  JTG 001121 ADDED HOLD-OVPAID-CHECK  (AMOUNT TO PAY RE: OVERPAID PAYOFF)
      *  BAH 010619 ADDED HOLD-REPO-AUCTION-EXP FOR "SS", REGACC PR# 1478
      *  JTG 011004 ADDED HOLD-LXZ(FIELDS) FOR UPDATE LX-Z RECORD
      *             ADDED NEW RATE TYPE 'Z' AMORTIZED WHERE RUNTIME COMPUTES
      *             INTEREST ON A 30 DAY METHOD (REAL ESTATE) AND LOAN CALCS
      *             ARE DONE SIMPLE, REMOVED TIE TO CTBL, FORCING TESTS ON (1)
      *                                             LENDMARK PR#1045 & PR#1380
      *  JTG 020308 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *             ADDED HOLD-INS-TAX-RB() OCCURS 8 S9(7)V99 REFUNDS WORLD #288
      *  JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED HOLD-LXM-MIP-CLPAID HOLD-LXM-MIP-CLPDTH
      *                   HOLD-LXM-MIP-AHPAID HOLD-LXM-MIP-AHPDTH
      *  BAH 020626 ADDED HOLD-PLREASON FOR REGACC , PR# 1775
      *  CS  020809 REDEFINED HOLD-RADDON-FIELDS TO HOLD TYPE 'PU' THAT ARE
      *             SPECIAL REVOLVING PROGRAMS (HOLD-PU-RVPROG OCCURS 4)
      *             [PR# 1877 LENDMARK]
      *  CS  020819 ADDED ANOTHER TABLE FOR REVLVING PROGRAMS TO HOLD NEW
      *             PRGRAMS
      *  CS  020820 CHANGED TO INCORPORATE ORIGINAL CALCULATED DEFAULTS &
      *             USER INPUTS IN THE SAME TABLE
      *  CS  030409 ADDED HOLD-NFE-CK-NUMBER CASHCO PR# 2034.
      *  JTG 050316 ADDED LOGIC TO SET 'LP-REFNO' IN THE PAYMENT RECORD TO
      *             THE VALUE THE CUSTOMER TYPED WHEN REVERSING. WILL CHANGE
      *             LONPF2.C TO SET 'TP-REFNO' TO THE VALUE OF LP-REFNO
      *             BEFORE LP-REFNO WAS ALTERED TO VALUE CUSTOMER TYPED.
      *                                                            CENTRIX #2466
      *   CS 070411 ADDED HOLD-REPO-AUCTION-NAME REGACC PR#168
      *   CS 120806 ADD HOLD-PRIOR-DF-CODE, HOLD-PRIOR-DF-POSTDATE PR#318
      *                                                     REGACC
      *  BAH 121114 ADDED PATH-BUF TO THIS TO PASS BETWEEN PROGRAMS
      *  BAH 130107 ADDED TP-BUF TO THIS TO PASS BETWEEN PROGRAMS
      *   CS 130429 ADDED HOLD-TDR1, HOLD-TDR2, HOLD-TDR2-ERR PR#338
      *   CS 151207 CHANGED HOLD-LP1-SEQ TO 9(5) A30 ONLY
      *   CS 151208 CHANGED HOLD1-KEY-DOWN, HOLD2-KEY-UP TO X(13) FROM X(12)
      *             TO ACCOMODATE LP-SEQNO IS NOW 9(5)
      *   CS 160302 ADD HOLD-ALT-PREPAY-FG, SET TO "Y" WHEN THE CUSTOMER IS
      *             PREPAID BY SP-ALT-PREPAY-MONTHS WORLD 1140
      *  BLM 160429 ADDED HOLD-BNKRPT-CASE-NO, HOLD-BNKRPT-STATUS,
      *             WORLD PR# 1151
      *  MJD 160613 ADDED HOLD-LXR-REB-AMT: AMOUNT OF THE SERVICE CHARGE THAT
      *             IS THE GEORGIA 4% CLOSING FEE EARLY PROVISION CALCULATED 
      *             IN REBATE-EP54 IN LIBGB/REBATE                  REGMGT #4915
      *  BAH 160701 CHANGED DATES TO 9(8)
      *  BLM 160728 CHANGED HOLD-SP1-KEY FROM 8 TO 9
      *  BAH 170417 REMOVED CONV-INTPDTH, NOT USED ANYWHERE
      *  BAH 181219 GLOBAL LPFILE, HOLD1-KEY-DOWN, HOLD2-KEY-UP, 
      *             HOLD-LP1-KEY FROM 13 TO 17
      * BAH 20190627 REMOVED REVOLVING HOLD-RADDON* HOLD-PU* HOLD-AP*, PT0153
      * BAH 20190701 REMOVED U-INSOURS,AVAILABLE-CREDIT FROM LPPFBUF
      * BAH 20201029 CHANGED HOLD-PARTIALS AND HOLD-LCPAID FROM S9(5) TO S9(7)
      * BAH 20240410 REMOVED HOLD-TDR1, HOLD-TDR2, HOLD-TDR2-ERR,
      *              HOLD-NFE-CK-NUMBER
      **************************************************************************
       01  UPDATE-BUF.
           03  ENTRY-FLAG                PIC 9.
           03  BYPASS-AMT-ENT-FG         PIC 9.
           03  FULL-DISPLAY              PIC X.
           03  REV-REBATE                PIC X.
           03  MAKER-CD                  PIC X.
           03  PD1-EXISTS                PIC X.
           03  FULL-PAGE-FG              PIC X.
           03  ENTER-AMT-FG              PIC X.
           03  INTERRUPT-FG              PIC X.
      * HOLD-DISPLAY-ERN HOLDS LC-DISPLAY-ERN, SO JUST READ THE LCFILE
           03  HOLD-DISPLAY-ERN          PIC X.
           03  HOLD-POCD                 PIC XX.
           03  HOLD1-KEY-DOWN            PIC X(17).
           03  HOLD2-KEY-UP              PIC X(17).
           03  HOLD-LP-REFNO             PIC X(5).
           03  FILLER                   REDEFINES HOLD-LP-REFNO.
               05  FILLER                PIC X.
               05  HOLD-LP-REFNO-BA      PIC 9(4).
           03  HOLD-SP1-KEY              PIC X(9).
           03  HOLD-LP1-KEY.
               05  HOLD-LP1-BRNO         PIC 9(4).
               05  HOLD-LP1-ACCTNO       PIC 9(8).
               05  HOLD-LP1-SEQNO        PIC 9(5).

           03  HOLD-PARTIALS             PIC S9(7)V99 COMP.
           03  HOLD-LCPAID               PIC S9(7)V99 COMP.
           03  HOLD-DLRVCHGS-ADJ      OCCURS 3
                                         PIC S9(5)V99 COMP.
           03  HOLD-DLPARVHELD-ADJ       PIC S9(5)V99 COMP.
           03  HOLD-DLPARVPAID-ADJ       PIC S9(5)V99 COMP.
           03  HOLD-LN-ACCERN         OCCURS 6
                                         PIC S9(7)V99 COMP.
           03  HOLD-LN-TOTINT            PIC S9(7)V99 COMP.
           03  HOLD-LN-TOTERN            PIC S9(7)V99 COMP.
           03  HOLD-LN-TOTDFMNTS         PIC S9(7)V99 COMP.
           03  HOLD-LN-TOTLCHG           PIC S9(7)V99 COMP.
           03  HOLD-LN-TOTSERACCT        PIC S9(6)V99 COMP.
           03  HOLD-LN-JDRATE            PIC 999V999  COMP.
           03  HOLD-NEW-1ST-DUE          PIC 9(8)     COMP.

      *-------------------------------------------------------
      *                REB-AMOUNT()
      *        1 -  CL             8 -  N/A
      *        2 -  AH             9 -  O1
      *        3 -  PP            10 -  O2
      *        4 -  INT           11 -  O3
      *        5 -  SC            12 -  O4
      *        6 -  MF            13 -  O5
      *        7 -  DF
      *-------------------------------------------------------
           03  REB-AMOUNT               OCCURS 13
                                         PIC S9(7)V99 COMP.

      *-------------------------------------------------------
      *                   INS-RB-TBL()
      *
      *        1 -  CL             5 -  O2
      *        2 -  AH             6 -  O3
      *        3 -  PP             7 -  O4
      *        4 -  O1             8 -  O5
      *-------------------------------------------------------
           03  INS-RB-TBL           OCCURS 8
                                         PIC X.
      *-------------------------------------------------------

           03  HOLD-BP-TRCD              PIC XX.
           03  HOLD-DL-RVCHGS-CODE      OCCURS 3
                                         PIC X.
           03  HOLD-DL-PARVHELD-CODE     PIC X.
           03  HOLD-DL-PARVPAID-CODE     PIC X.
           03  HOLD-PRDELINQAMT          PIC S9(7)V99 COMP.
           03  ADDON-INT-REBATE          PIC S9(6)V99 COMP.
           03  ADDON-INT-EARNED          PIC S9(6)V99 COMP.
           03  MAX-DEFER-FG              PIC X.
           03  HOLD-PRPDUE-DAYS          PIC S9(5)    COMP.
           03  CONV-REV-FLAG             PIC X.
           03  CONV-AMT-PAID-LAST        PIC 9(6)V99  COMP.
           03  CONV-DATE-PAID-LAST       PIC 9(8)     COMP.
           03  CONV-LCPDTH-DATE          PIC 9(8)     COMP.
           03  HOLD-AFPDLLP              PIC S9(6)V99 COMP.
           03  HOLD-EARNINGS-IB-OWING    PIC S9(6)V99 COMP.
           03  HOLD-EARNINGS-IB-ERNINT   PIC S9(6)V99 COMP.
           03  HOLD-EARNINGS-IB          PIC S9(6)V99 COMP.
      *-----------------------------------------------------------------
           03  HOLD-ONESTEP-RN-BUF.
               05  HOLD-ONESTEP-RN-FG    PIC X.
           03  HOLD-OVPAID-CHECK         PIC S9(7)V99 COMP.
           03  HOLD-REPO-AUCTION-EXP     PIC S9(7)V99 COMP.

      *-------------------------------------------------------
      *                HOLD-INS-TAX-RB()
      *        1 -  CL             5 -  O2
      *        2 -  AH             6 -  O3
      *        3 -  PP             7 -  O4
      *        4 -  O1             8 -  O5
      *-------------------------------------------------------
           03  HOLD-INS-TAX-RB          OCCURS 8
                                         PIC S9(7)V99 COMP.

           03  HOLD-PLREASON             PIC XX.
           03  HOLD-REV-LP-REFNO         PIC X(5).
           03  HOLD-PRIOR-DF-CODE        PIC X(2).
           03  HOLD-PRIOR-DF-POSTDATE    PIC 9(8)     COMP.
           03  HOLD-BNKRPT-CASE-NO       PIC X(9).
           03  HOLD-BNKRPT-STATUS        PIC X.
           03  HOLD-LXR-REB-AMT          PIC S9(7)V99 COMP.

      *-----------------------------------------------------------------
      *    WORKERS FOR MONTHLY INSURANCE PREMIUM PAYMENT INFO
      *-----------------------------------------------------------------
           03  HOLD-LXM-MIP-CLPAID       PIC S9(7)V99 COMP.
           03  HOLD-LXM-MIP-CLPDTH       PIC 9(8)     COMP.
           03  HOLD-LXM-MIP-AHPAID       PIC S9(7)V99 COMP.
           03  HOLD-LXM-MIP-AHPDTH       PIC 9(8)     COMP.

      *-----------------------------------------------------------------
      *    WORKERS FOR AMORTIZED INTEREST (Z) PAYMENT INFO
      *-----------------------------------------------------------------
           03  HOLD-LXZ-POST-REGPY       PIC S9(7)V99 COMP.
           03  HOLD-LXZ-POST-EXTRA-PRIN  PIC S9(7)V99 COMP.
           03  HOLD-LXZ-POST-OTH         PIC S9(7)V99 COMP.
           03  HOLD-LXZ-POST-LCHG        PIC S9(7)V99 COMP.

           03  HOLD-LXZ-PDUE-APINT       PIC S9(7)V99 COMP.
           03  HOLD-LXZ-PDUE-APPRIN      PIC S9(7)V99 COMP.
           03  HOLD-LXZ-CUR-APINT        PIC S9(7)V99 COMP.
           03  HOLD-LXZ-CUR-APPRIN       PIC S9(7)V99 COMP.

           03  HOLD-REPO-AUCTION-NAME        PIC X(30).
           03  HOLD-ALT-PREPAY-FG            PIC X. 

      * THESE WERE PART OF PATH-BUF AND WERE XFER/FLDVAL IN LONPF'S AND
      * INTERBRANCH
      * LP/INBRTR MOVES POSTING BRANCHES DATA PATH INTO HERE
           03  PATH-MACHINE                  PIC X(2).
           03  PATH-DATA                     PIC X(6).
           03  PATH-DATAX REDEFINES PATH-DATA.
               05  PATH-STATE                PIC XX.
               05  PATH-BRANCH               PIC 9(4).
      * ADDED FROM LONPFA, LONPFB AND LONPF2 FLDVAL
           03  MS-PENALTY                    PIC S9(5)V99.

           03  TP-BUF.
               05  HOLD-LN-DLRVPRIN    OCCURS 3
                                       PIC S9(7)V99  VALUE 0.
               05  HOLD-LN-ANTIC-DISC  PIC S9(7)V99  VALUE 0.
               05  HOLD-LN-ANTIC-INT   PIC S9(7)V99  VALUE 0.
               05  HOLD-LN-ANTIC-SC    PIC S9(7)V99  VALUE 0.
               05  HOLD-LN-ANTIC-MF    PIC S9(7)V99  VALUE 0.
               05  HOLD-LN-PROCEEDS    PIC S9(7)V99  VALUE 0.
               05  HOLD-LN-PARTRCAMT   PIC S9(7)V99  VALUE 0.
               05  HOLD-LN-RECOURSE    PIC X         VALUE " ".
               05  HOLD-CKNO           PIC 9(6)      VALUE 0.
               05  HOLD-CKCODE         PIC X         VALUE " ".
               05  HOLD-FREE1          PIC X(70)     VALUE " ".
               05  HOLD-FREE2          PIC X(70)     VALUE " ".

           03  TS-BUF.
               05  HOLD-NEW-DLNO       PIC S9(4)V99 VALUE +0.

           03  ADDON-PRIOR-NODEF       PIC S9(3)     VALUE 0.
