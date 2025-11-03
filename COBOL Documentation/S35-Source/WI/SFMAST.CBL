       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFMAST.
       DATE-WRITTEN.    071495.
      *****************************************************
      *                 (WI)
      *  DESCRIPTION:   SALES FINANCE MASTER FINANCIALS
      *                 ONLY CALLED IN F9 
      *  REV:
      *  MJD 091895 ADDES DATE TO CALL FOR DLBAL ( NEEDED FOR DS-DATE)
      *  MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 062097 ALLOWED CTRL N
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 101797 ADDED MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER
      *  BAH 022299 INSURANCE TERM WAS NOT BEING CALCULATED, IT WAS
      *             MOVING INSTRM(), WHICH IS REALLY EFFECTIVE DATE !!
      *  GLF 080699 ADD LPO, REP CODE, AND PROCESS CODE DISPLAY IF GP-I-AM-TFC
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  BAH 991022 PERCENT DOWN WAS DISPLAYING AS .99 INSTEAD OF MULTIPLYING
      *             BY 100 AND DISPLAYING AS 99.99, CINDY
      *  MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *             ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  MJD 000626 IF GP-USE-CONRATE = "Y" DISPLAY CONTRACT RATE, ELSE DISPLAY
      *             THE EFFECTIVE RATE OF THE LOAN.
      *  JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE:SALE PRICE-DP-TI PAACO PR#1467
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *   CS 081121 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTIC6) AND MAINTFEE(ANTIC4) FROM ANTICIPATED
      *             EARNINGS  REGACC PR#210
      *  BAH 100127 DISPLAY RVPRIN2 AND RVPRIN3 IN THE O2 AND O3 LINES FOR
      *             UNITED CREDIT ONLY PR# 3513
      *  BAH 140310 DISPLAY LOAN COST: LN-ANTICERN(6) FOR GP-I-AM-KSTREET
      *             ONLY, PR# 4492 KSTREET
      *  BAH 151216 LOAN CLASS WAS GETTING CLOBBERED BY SP-FEEFRMLA(1) BECAUSE
      *             THE DISPLAY BUFFER IS REDEFINED AND IT WAS GETTING FILLED
      *             IN TOO SOON. I MOVED FIELDS 200 AND 210 TO BE IN
      *             DISPLAY-BUF1 SO ALL FEES CAN BE SETUP AT SAME TIME. PL 915
      * KEC 2016.0509 CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      * BAH 2017.0424 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018.0823 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/SFMAST.CBL S35 04/16/24-14:35:03 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  SFMAST-LINES               PIC 99  VALUE 22.
       01  SFMAST-COLUMNS             PIC 99  VALUE 78.
       01  SFMAST-BEGIN-Y             PIC 99  VALUE 2.
       01  SFMAST-BEGIN-X             PIC 99  VALUE 2.
       01  ERRCD           PIC XX   VALUE " ".
       01  ELE             PIC 9(6).
       01  SUB             PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
       01  ANTIC           PIC S9(6)V99 COMP.
       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

       01  COUNTER         PIC 99.

       01  HOLD-WORKER       PIC S9(7)V99.
       01  HOLD-PERCENT    PIC S999V99.

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************

       01  PROMPT-FEE1         PIC X(13)  VALUE "FEE1        :".
       01  PROMPT-FEE2         PIC X(8)   VALUE "FEE2   :".

       01  DISPLAY-BUF1        VALUE SPACES.
           03  D-LCNO          PIC Z(8)9.
           03  D-LC-DESC       PIC X(16).
           03  D-010-040       OCCURS 2.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-LOANAMT       PIC Z(5)9.99  BLANK ZERO.
           03  D-DLCK          PIC Z(5)9.99  BLANK ZERO.
           03  D-ANTIC         PIC Z(5)9.99  BLANK ZERO.
           03  D-050           PIC XX.
           03  D-051           PIC 9(4)      BLANK ZERO.
           03  D-052           PIC Z9.
           03  D-053           PIC X(12).
           03  D-060           PIC 99/99/99  BLANK ZERO.
           03  D-061           PIC 99/99/99  BLANK ZERO.
           03  D-ENT-DATE      PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC X(3).
           03  D-071           PIC X(15).
           03  D-080           PIC X(2).
           03  D-081           PIC X(15).
           03  D-090           PIC 9(4).99   BLANK ZERO.
           03  D-091           PIC X(15).
           03  D-100           PIC X(2).
           03  D-101           PIC X(15).
           03  D-110           PIC X.
           03  D-111           PIC 999.
           03  D-120           PIC ZZ9.
           03  D-130           PIC 99/99/99  BLANK ZERO.
           03  D-140           PIC Z(5)9.99- BLANK ZERO.
           03  D-150           PIC Z(5)9.99- BLANK ZERO.
           03  D-151           PIC ZZ9.99    BLANK ZERO.
           03  D-152           PIC X.
           03  D-160           PIC Z(5)9.99- BLANK ZERO.
           03  D-161           PIC ZZ9.99    BLANK ZERO.
           03  D-162           PIC X.
           03  D-180           PIC X(3).
           03  D-181           PIC Z9.99     BLANK ZERO.
           03  D-190           PIC Z(3)9.99  BLANK ZERO.
           03  D-200           PIC Z(3)9.99  BLANK ZERO.
           03  D-210           PIC Z(3)9.99  BLANK ZERO.

       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-DLRVPRIN      PIC Z(4)9.99  BLANK ZERO.
           03  D-DLRVCHGS      PIC Z(4)9.99  BLANK ZERO.
           03  D-DLPARV        PIC Z(4)9.99  BLANK ZERO.
           03  D-DLDISC        PIC Z(4)9.99  BLANK ZERO.
           03  D-260           PIC ZZ9.999   BLANK ZERO.
           03  D-262P          PIC X(03).
           03  D-262           PIC ZZ9.999   BLANK ZERO.
           03  D-263           PIC ZZ9.999   BLANK ZERO.
           03  D-264           PIC ZZ9.999   BLANK ZERO.
           03  D-270-300      OCCURS 4.
               05  D-1COMP     PIC 99        BLANK ZERO.
               05  D-1OUR      PIC X.
               05  D-1TERM     PIC ZZ9       BLANK ZERO.
               05  D-1COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM     PIC Z(3)9.99  BLANK ZERO.
           03  D-310-320      OCCURS 2.
               05  D-2COMP     PIC 99        BLANK ZERO.
               05  D-2OUR      PIC X.
               05  D-2EXP      PIC 9(6)      BLANK ZERO.
               05  D-2TERM REDEFINES D-2EXP
                               PIC Z(6)      BLANK ZERO.
               05  D-2COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM     PIC Z(3)9.99  BLANK ZERO.
           03  D-340           PIC Z(5)9.99  BLANK ZERO.
           03  D-350           PIC Z(5)9.99  BLANK ZERO.
           03  D-360           PIC Z(5)9.99  BLANK ZERO.
           03  D-370           PIC Z(5)9.99  BLANK ZERO.
           03  D-380           PIC Z(5)9.99  BLANK ZERO.
           03  D-390           PIC Z(6)9.99  BLANK ZERO.
           03  D-400           PIC Z(5)9.99  BLANK ZERO.
           03  D-410           PIC Z(5)9.99- BLANK ZERO.
           03  D-420           PIC Z(5)9.99  BLANK ZERO.
           03  FILLER          PIC X(81).

       01  DISPLAY-LIST1.
           03  FILLER             PIC X(37)  VALUE
           "LNKEY LCDESC 010 011 012 020 021 022 ".
           03  FILLER             PIC X(35)  VALUE
           "LOANAMT DLCK ANTIC 050 051 052 053 ".
           03  FILLER             PIC X(37)  VALUE
           "060 061 ENT-DATE 070 071 080 081 090 ".
           03  FILLER             PIC X(40)  VALUE
           "091 100 101 110 111 120 130 140 150 151 ".
           03  FILLER             PIC X(28)  VALUE
           "160 161 180 181 190 200 210.".

       01  DISPLAY-LIST2.
           03  FILLER          PIC X(20)  VALUE
           "220 230 240 250 260 ".
           03  FILLER          PIC X(39)  VALUE
           "262P:P 262 263 264 270 271 272 273 274 ".
           03  FILLER          PIC X(44)  VALUE
           "280 281 282 283 284 290 291 292 293 294 300 ".
           03  FILLER          PIC X(52)  VALUE
           "301 302 303 304 310 311 312 313 314 320 321 322 323 ".
           03  FILLER          PIC X(40)  VALUE
           "324 340 350 360 370 380 390 400 410 420.".

       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBWI/SFINFOW.CPY".
       COPY "LIBWI/DLPOFFW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBWI/GAINQW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".

       01  SFMAST-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SFMAST_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/SFMAST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SFMAST_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SFMAST" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

      * ALLOW CTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

           PERFORM GET-GPENV.


           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE SFMAST-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  SFMAST-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    SFMAST-COLUMNS + VDU-WIN-W-MOD
                   LINES   SFMAST-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS SFMAST-WINDOW-HANDLE.

           PERFORM FORM-RESET.


           IF GP-USE-CONRATE = "Y"
              MOVE "CNT" TO VDUBUF
           ELSE
              MOVE "EFF" TO VDUBUF.
           MOVE "S  A030262P:P." TO VDU.
           PERFORM SCREENIO.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMMIS" TO VDUBUF
           ELSE
              MOVE "DISCNT" TO VDUBUF.
           MOVE "S  A03025P:P." TO VDU.
           PERFORM SCREENIO.

           MOVE "S  A080SYSDATE." TO VDU.
           MOVE EXT-CONAME-SYSDATE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200TITLE." TO VDU.
           MOVE "SALES FINANCE MASTER" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO END-ROUTINE.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           PERFORM DISPLAY-FIELDS.

       ENTER-AGAIN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "12"
              PERFORM MEMO-SCAN.
           IF VDUSTATUS = "16"
              PERFORM DLPOFF-INQUIRY.
      *    IF VDUSTATUS = "18"
      *       PERFORM DLRSRV-BALANCES.
           IF VDUSTATUS = "1:"
              PERFORM SFINFO-SCAN.
           IF VDUBUF = "M"
              PERFORM GAINQ-SCAN.
           GO TO ENTER-AGAIN.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE SPACES TO DISPLAY-BUF1.

           PERFORM GET-SP.
           MOVE LN-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO ELE.
       DISP-NEXT-BW.
           MOVE LN-SSNO(ELE) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(ELE)
              MOVE INSPECT-W TO D-011(ELE)
              MOVE LN-MAKERCD(ELE) TO D-012(ELE).
           IF ELE < 2
              ADD 1 TO ELE
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-LNAMT TO D-LOANAMT.
           MOVE LN-PROCEEDS TO D-DLCK.
           COMPUTE ANTIC = LN-ANTICERN(1) + LN-ANTICERN(2)
                         + LN-ANTICERN(3)
                         + LN-ANTICERN(5).
           IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                                    GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(6) TO ANTIC.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(4) TO ANTIC.

           MOVE ANTIC TO D-ANTIC.
           MOVE LN-ORGST TO D-050.
           MOVE LN-ORGBR TO D-051.
           MOVE LN-SUBCLASS TO D-052.
           MOVE SP-DESC TO D-053.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-061.
           MOVE LN-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-ENT-DATE.

           MOVE LN-PURCD TO D-070 CD-CODE.
           MOVE "PU" TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-071.

           MOVE LN-SRCD TO D-080 CD-CODE.
           MOVE "SR" TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.

           MOVE LN-DLNO TO D-090
                           DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-091.

           MOVE LN-COLLCD TO D-100 CD-CODE.
           MOVE "CO" TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-101.

           MOVE LN-UNITPER-CD TO D-110.
           MOVE LN-UNITPER-FREQ TO D-111.

           MOVE LN-ORGTERM TO D-120.
           MOVE LN-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-130.
           MOVE LN-SELLPR TO D-140.

           IF LN-DOWNPY NOT = 0
              MOVE LN-DOWNPY TO D-150
              COMPUTE HOLD-PERCENT ROUNDED =
                           LN-DOWNPY / LN-SELLPR * 100
              IF HOLD-PERCENT NOT = 0
                 MOVE HOLD-PERCENT TO D-151
                 MOVE "%"          TO D-152.

           IF LN-TRADEIN NOT = 0
              MOVE LN-TRADEIN TO D-160
              COMPUTE HOLD-PERCENT ROUNDED =
                           LN-TRADEIN / LN-SELLPR * 100
              IF HOLD-PERCENT NOT = 0
                 MOVE HOLD-PERCENT TO D-161
                 MOVE "%"          TO D-162.

           MOVE LN-INSCODES TO D-180.
           MOVE LN-POINTS TO D-181.
           PERFORM SETUP-FEES-FOR-DISPLAY.

           IF SP-FEEFRMLA(1) = " "
              MOVE SPACES TO PROMPT-FEE1
           ELSE
              STRING SP-FEEID(1) "     :" DELIMITED BY SIZE
                         INTO PROMPT-FEE1.
           MOVE "S  A130FEE1." TO VDU.
           MOVE PROMPT-FEE1 TO VDUBUF.
           PERFORM SCREENIO.

           IF SP-FEEFRMLA(2) = " "
              MOVE SPACES TO PROMPT-FEE2
           ELSE
              STRING SP-FEEID(2) ":" DELIMITED BY SIZE
                          INTO PROMPT-FEE2.

           MOVE "S  A130FEE2." TO VDU.
           MOVE PROMPT-FEE2 TO VDUBUF.
           PERFORM SCREENIO.

           IF SP-FEEFRMLA(1) NOT = " "
              MOVE LN-FEEAMT(1) TO D-200.

           IF SP-FEEFRMLA(2) NOT = " "
              MOVE LN-FEEAMT(2) TO D-210.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO COUNTER.

       DISP-INTERMEDIATE-FIELDS.
           MOVE SPACES TO DISPLAY-BUF2.

           MOVE LN-OVRDRATE TO D-260.
           IF GP-USE-CONRATE = "Y"
              MOVE "CNT" TO D-262P
              MOVE LN-CONRATE TO D-262
           ELSE
              MOVE "EFF" TO D-262P
              MOVE LN-EFFRATE TO D-262.
           MOVE LN-APRATE TO D-263.
           MOVE LN-YLDRATE TO D-264.

           MOVE 1 TO ELE.
       DISP-NEXT-IC.
           MOVE LN-INSCOMP(ELE) TO D-1COMP(ELE).
           MOVE LN-INSPREM(ELE) TO D-1PREM(ELE).
           MOVE ELE TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-1TERM(ELE).
           MOVE LN-INSCOVR(ELE) TO D-1COVR(ELE).
           MOVE LN-INSOURS(ELE) TO D-1OUR(ELE).
           IF ELE < 4
              ADD 1 TO ELE
              GO TO DISP-NEXT-IC.

           MOVE 1 TO SUB.
           MOVE 5 TO ELE.
       DISP-NEXT-2IC.
           MOVE LN-INSCOMP(ELE) TO D-2COMP(SUB).
           MOVE LN-INSPREM(ELE) TO D-2PREM(SUB).
           IF GP-OT-INS-TERM = "Y"
              MOVE ELE TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO D-2TERM(SUB)
           ELSE
              MOVE LN-INSEXP-DATE(ELE) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-2EXP(SUB).
           MOVE LN-INSCOVR(ELE) TO D-2COVR(SUB).
           MOVE LN-INSOURS(ELE) TO D-2OUR(SUB).
           IF ELE < 6
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-2IC.

           MOVE 1 TO ELE.

           MOVE LN-SERCHG TO D-340.
           MULTIPLY LN-MAINTFEE BY LN-ORGTERM GIVING D-350 ROUNDED.
           MOVE LN-EXTCHG TO D-360.
           MOVE LN-INTCHG TO D-370.

           PERFORM LOAN-CALCULATIONS.
           MOVE DISCLOSED-FINAMT TO D-380.
           IF LN-LOANTYPE = "P"
              MOVE LN-LNAMT TO D-390
           ELSE
              MOVE TOTAL-REPAY TO D-390.

      * SETUP 1ST PAYMENT AMOUNT:
           MOVE LN-1STPYAMT TO D-400.

      * SETUP LAST PAYMENT AMOUNT:
           MOVE LN-LASTPYAMT TO D-410.

      * SETUP REGUALR PAYMENT AMOUNT:
           MOVE LN-REGPYAMT TO D-420.

           IF LN-DLRVPRIN(2) = 0
                   AND LN-DLRVPRIN(3) = 0
              MOVE LN-DLRVPRIN(1) TO D-DLRVPRIN
           ELSE
              ADD LN-DLRVPRIN(1) LN-DLRVPRIN(2) LN-DLRVPRIN(3)
                                         GIVING D-DLRVPRIN
              INSPECT D-DLRVPRIN REPLACING LEADING " " BY "*".

           IF LN-DLRVCHGS(2) = 0
                   AND LN-DLRVCHGS(3) = 0
              MOVE LN-DLRVCHGS(1) TO D-DLRVCHGS
           ELSE
              ADD LN-DLRVCHGS(1) LN-DLRVCHGS(2) LN-DLRVCHGS(3)
                                         GIVING D-DLRVCHGS
              INSPECT D-DLRVCHGS REPLACING LEADING " " BY "*".

           IF LN-DLPARVPAID = 0
              MOVE LN-DLPARVHELD TO D-DLPARV
           ELSE
              ADD LN-DLPARVHELD LN-DLPARVPAID GIVING D-DLPARV
              INSPECT D-DLPARV REPLACING LEADING " " BY "*".

           MOVE LN-DLDISC TO D-DLDISC.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

      ************************************
       SETUP-FEES-FOR-DISPLAY SECTION.
      ************************************
           ADD LN-FEEAMT(1) LN-FEEAMT(2)
               LN-FEEAMT(3) LN-FEEAMT(4)
               LN-FEEAMT(5) LN-FEEAMT(6)
               LN-FEEAMT(7) LN-FEEAMT(8)
               LN-FEEAMT(9) LN-FEEAMT(10)
                                  GIVING HOLD-WORKER.
           IF SP-FEEFRMLA(1) NOT = " "
              SUBTRACT LN-FEEAMT(1) FROM HOLD-WORKER
              MOVE LN-FEEAMT(1) TO D-200.
           IF SP-FEEFRMLA(2) NOT = " "
              SUBTRACT LN-FEEAMT(2) FROM HOLD-WORKER
              MOVE LN-FEEAMT(2) TO D-210.
           MOVE HOLD-WORKER TO D-190.

      ************************************
      *DLRSRV-BALANCES SECTION.
      ************************************
      *    MOVE EXT-WI-CL-ACCTNO     TO DLBAL-ACCTNO.
      *    MOVE EXT-WI-CL-TRANS-DATE TO DLBAL-TRANS-DATE.
      *    MOVE "WI/DLBAL" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH DLBAL-BUF EXIT-PATHNAME.
      *    CANCEL FORM-PROGX.

      ************************************
       DLPOFF-INQUIRY SECTION.
      ************************************
           MOVE "Y"           TO DLPOFF-SFINFO-FG.
           MOVE "WI/DLPOFF" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLPOFF-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ************************************
       SFINFO-SCAN SECTION.
      ************************************
           MOVE "Y" TO SFINFO-DLPOFF-FG.
           MOVE "WI/SFINFO" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SFINFO-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ************************************
       MEMO-SCAN SECTION.
      ************************************
           MOVE "WI/MEMOSC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ************************************
       GAINQ-SCAN SECTION.
      ************************************
           MOVE EXT-WI-CL-ACCTNO TO GAINQ-ACCTNO.
           MOVE "WI/GAINQY" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GAINQ-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON SFMAST-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY SFMAST-SCREEN UPON SFMAST-WINDOW-HANDLE.
           MOVE SFMAST-FORM-FIELDS TO WR-BUF.
           MOVE SFMAST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE SFMAST-HELP--FILE TO HELP-LINK-FILE.
           MOVE SFMAST-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SFMAST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE "STATE PARA DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO CD-DESC.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE " INVALID DEALER" TO DL-SHORT.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       ADD-IT2.
           PERFORM ADD-IT.
           MOVE "/" TO ADD-ON.
           PERFORM ADD-IT.
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW.
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-MEMO.
           MOVE "S  A060M1." TO VDU.
           MOVE "MEMO: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE LN-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/SCREEN.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY SFMAST-SCREEN.
           DESTROY SFMAST-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
