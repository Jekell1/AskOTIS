       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPJ0.
       DATE-WRITTEN.    012484.
      **************************************************************
      *                 (LP)
      *  DESCRIPTION:   VOID SALES FINANCE LOAN
      *
      * REV:
      *  061395 KEC ADDED SECURITY PASSWORD ON VOIDING A CONTRACT
      *             WHEN THE SYSDATE IS NOT EQUALING THE ENTERING
      *             DATE
      *  080695 BLL FIX DATE CK PASSWD; ASKING PASSWD EVEN IF TODAY
      *  042496 JTG ADDED LOGIC TO STOP VOID OF SEQUENCE NO. 1 LOANS
      *             WITH LN-MORELN = "Y"
      *  MJD 042596 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      *  MJD 071996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *             FOR WEEKLY UNIT PERIODS.
      *  KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 121096 MOVED PERFORM OF LPSTAT-DISPLAY TO AFTER READ-BW1.
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE:SALE PRICE-DP-TI PAACO PR#1467
      *  BAH 020712 SIGNED LN-TRADEIN, REGACC PR# 1808
      *  JTG 050922 ADDED LN-POCD-UNWIND "*U" A LN-POCD-NONLOAN
      *                   AND LN-UNWIND-REASON-CODE                 REGACC #0105
      *  BAH 051020 RENAMED VOID-SEQ TO VOID-DIFF-DATE-SEQ, WHICH IS WHAT
      *             IS WAS REALLY DOING, AND ADDED VOID-SEQ FOR ANY VOID
      *              REGACC PR# 105
      *   CS 081117 ADD TEST GP-INCL-OTH-IN-EARNED ='R'. IF 'R' DONT INCLUDE
      *             OTHER(ANTIC(6) OR MAINTFEE(ANTIC(4) IN EARNINGS REGACC
      *                                                          PR#210
      *  BAH 121130 PASSWORD SEQ 0 GOT REMOVED SOMEHOW WHEN I ADDED SEQ 2
      *             PUT BACK PASSWORD FOR VOIDING ON A DIFFERENT DATE, PL# 785
      *   CS 130507 WOULDNT ALLOW VOID, KEPT GOING TO MODIFY-ROUTINE, MISSING
      *             A PERFORM VOID IF YOU PASSED THE PASSWORD CHECK & YOU WERE
      *             VOIDING ON SAME DAY AS ENTRY DATE. MULLEN
      *   CS 130710 A30 CONVERSION COMPLETED
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 141121 FIXED DISPLAY OF UNWIND CODES (CDSCAN), STARTED AT LINE
      *            0 AND NOT DISPLAYING TITLE
      * BAH 2016.0118 SPLIT OUT LTFILE, PD0003
      * BAH 2016.0304 CHANGED TILLNO TO NUMERIC AND CHANGED SCREEN INPUT TO 2
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2017.0530 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 2017.0629 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.1001 GLOBAL BWFILE
      * BAH 2018.1210 GLOBAL LNFILE
      * BAH 2020.0707 REMOVED CLOSE-BR-FILE #TST001
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2023.0713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPJ0.CBL S35 02/20/24-15:27:50 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       01  WK-IFN                    PIC X(4) VALUE "B5WK".
       01  WK-KEY                    PIC 9(2).
       01  HOLD-WK-UNWIND-REASON-CD  PIC X(2).
       01  HOLD-WK-UNWIND-DRAWER     PIC 9(2).

       01  ELE                 PIC 9(6)    VALUE 0.
       01  ACTION              PIC X(6).
       01  IO-TYPE             PIC X(3).
       01  NEW-FLAG            PIC 9       VALUE 0.
       01  FIRST-TIME          PIC 9       VALUE 0.
       01  ANTIC               PIC S9(6)V99  COMP-3.
       01  LAST-READ           PIC X(4).
       01  SAVE-BW-KEY         PIC X(32).
       01  SV-PASSWORD-DRAWER  PIC X.
       01  VALID-LOAN          PIC X.
       01  SAVE-SSNO           PIC 9(10) COMP-3 VALUE 0.
       01  IX1                 PIC 99    COMP-3.
       01  IX2                 PIC 99    COMP-3.

       01  SUB             PIC 9(6) COMP-3.
       01  SUB2            PIC 9(6) COMP-3.
       01  OVER-20         PIC 99.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).

       01  UNWIND-RTN        PIC X(3) VALUE " ".

       01  DL-CK-PRINTED-FG  PIC X    VALUE " ".

       01  FIRST-SCAN        PIC 9     VALUE 0.
       01  SCAN-FLAG         PIC 9     VALUE 0.
       01  SCAN              PIC X(2)  VALUE SPACES.
           88  SCAN-ON       VALUE "ON".
       01  TYP               PIC 9     VALUE 0.

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
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.

       01  COLLATERAL-BREAKOUT.
           03 COLLATERAL-30-1    PIC X(30).
           03 COLLATERAL-30-2    PIC X(30).
           03 FILLER             PIC X(20).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************

       01  DLR-TYP-LIST      PIC X(6)   VALUE "38P:P.".
       01  DLR-TYP-BUF       PIC X(4).

       01  DISPLAY-BUF1        VALUE " ".
           03  D-LNNO          PIC X(14).
           03  D-LC-DESC       PIC X(16).
           03  D-LNAMT         PIC ZZZ,ZZ9.99.
           03  D-PROCEEDS      PIC ZZZ,ZZ9.99.
           03  D-ANTIC         PIC ZZZ,ZZ9.99.
           03  D-010-040       OCCURS 4.
               05  D-010       PIC X(11).
               05  D-011       PIC X(24).
               05  D-012       PIC X(1).
           03  D-050           PIC XX.
           03  D-051           PIC 9(4)      BLANK ZERO.
           03  D-060           PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC X(3).
           03  D-071           PIC X(15).
           03  D-080           PIC X(3).
           03  D-081           PIC X(15).
           03  D-090           PIC X(3).
           03  D-091           PIC X(15).
           03  D-100           PIC X(20).
           03  D-110           PIC X(30).
           03  D-111           PIC X(30).
       01  DISPLAY-BUF2 REDEFINES DISPLAY-BUF1.
           03  D-120           PIC 9(4).99   BLANK ZERO.
           03  D-121           PIC X(16).
           03  D-130           PIC X.
           03  D-131           PIC X(9).
           03  D-132           PIC Z(5)9.99   BLANK ZERO.
           03  D-140           PIC ZZZ.
           03  D-150           PIC ZZ.
           03  D-160           PIC 99/99/99  BLANK ZERO.
      *--->03  D-170           PIC 99/99/99  BLANK ZERO.
           03  D-170           PIC X(8).
           03  D-180           PIC 99/99/99  BLANK ZERO.
           03  D-190           PIC ZZ9.999   BLANK ZERO.
           03  D-200           PIC ZZ9.999   BLANK ZERO.
           03  D-210           PIC ZZ9.999   BLANK ZERO.
           03  D-220           PIC Z(5)9.99  BLANK ZERO.
           03  D-230           PIC Z(5)9.99  BLANK ZERO.
           03  D-231           PIC Z(5)9.99-  BLANK ZERO.
           03  D-240           PIC Z(5)9.99  BLANK ZERO.
           03  D-241           PIC ZZ9.99     BLANK ZERO.
           03  D-250           PIC Z(5)9.99  BLANK ZERO.
           03  D-251           PIC Z(3)9.99  BLANK ZERO.
           03  D-260           PIC XXX.
           03  D-270-290       OCCURS 3.
               05  D-1COMP      PIC 99        BLANK ZERO.
               05  D-1IOURS     PIC X.
               05  D-1TERM      PIC Z(3).
               05  D-1COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-300-310       OCCURS 2.
               05  D-2COMP      PIC 99        BLANK ZERO.
               05  D-2IOURS     PIC X.
               05  D-2EXP       PIC 9(6)      BLANK ZERO.
               05  D-2COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-320-340       OCCURS 3.
               05  D-1FEECD     PIC XX.
               05  D-1FEEAMT    PIC Z(3)9.99  BLANK ZERO.
               05  D-1FOURS     PIC X.
               05  D-2FEECD     PIC XX.
               05  D-2FEEAMT    PIC Z(3)9.99  BLANK ZERO.
               05  D-2FOURS     PIC X.
           03  D-350           PIC Z(5)9.99  BLANK ZERO.
           03  D-351           PIC Z(5)9.99  BLANK ZERO.
           03  D-360           PIC 99/99/99  BLANK ZERO.
           03  D-370           PIC Z(5)9.99  BLANK ZERO.
           03  D-371           PIC Z(5)9.99  BLANK ZERO.
           03  D-380           PIC Z(5)9.99  BLANK ZERO.
           03  D-381           PIC Z(5)9.99  BLANK ZERO.
       01  DISPLAY-BUF3 REDEFINES DISPLAY-BUF1.
           03  D-X010-X040      OCCURS 4.
               05  D-X010       PIC X(11).
               05  D-X011       PIC X(24).
       01  D-BW-WK.
           03  FILLER          PIC X(14).
           03  D-BW-PROMPT     PIC X(8).
           03  D-BW-LOANS      PIC 99.

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(28)  VALUE
           "LNNO LCDESC PRIN DLCK ANTIC ".
           03  FILLER          PIC X(40)  VALUE
           "010 011 012 020 021 022 030 031 032 040 ".
           03  FILLER          PIC X(56)  VALUE
           "041 042 050 051 060 070 071 080 081 090 091 100 110 111.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(52)  VALUE
           "120 121 130 131 132 140 150 160 170 180 190 200 210 ".
           03  FILLER          PIC X(56)  VALUE
           "220 230 231 240 241 250 251 260 270 271 272 273 274 280 ".
           03  FILLER          PIC X(52)  VALUE
           "281 282 283 284 290 291 292 293 294 300 301 302 303 ".
           03  FILLER          PIC X(52)  VALUE
           "304 310 311 312 313 314 320 321 322 323 324 325 330 ".
           03  FILLER          PIC X(52)  VALUE
           "331 332 333 334 335 340 341 342 343 344 345 350 351 ".
           03  FILLER          PIC X(20)  VALUE
           "360 370 371 380 381.".
       01  DISPLAY-LIST3.
           03  FILLER          PIC X(32) VALUE
           "010 011 020 021 030 031 040 041.".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPMENUW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBLP/LONPJ_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBWI/UNWIND_DEF.CPY".

       COPY "LIBLP/LONPJ_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBWI/UNWIND_WKS.CPY".

       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  VOID-DIFF-DATE-SEQ PIC 99 VALUE 0.
       01  UNWIND-SEQ         PIC 99 VALUE 1.
       01  VOID-SEQ           PIC 99 VALUE 2.


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPJ_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".
       COPY "LIBWI/UNWIND_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPJ" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

           PERFORM RESTORE-SCREEN.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.

           PERFORM GET-GPENV.

      * ROUTE-CD = 1 IF J1 FOUND ERRORS DURING VERIFY TO CORRECT
           IF ROUTE-CD = "1"
              MOVE " " TO ROUTE-CD
              MOVE 2 TO NEW-FLAG
              MOVE 1 TO FIRST-TIME
              GO TO ENTER-LOAN.

           PERFORM FORM-RESET.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM" TO DLR-TYP-BUF
           ELSE
              MOVE "DISC" TO DLR-TYP-BUF.
           MOVE DLR-TYP-LIST TO WR-LIST.
           MOVE DLR-TYP-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

           IF NEW-FLAG = 1
              PERFORM DISPLAY-FIELDS
              MOVE 1 TO FIRST-TIME
              GO TO ENTER-LOAN.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE " " TO MESS
           PERFORM MSG-LINE.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
       ENTER-LOAN.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       LOAN-GET-NUMBER.
           MOVE 0 TO ELE.
           MOVE 0 TO NEW-FLAG.
           MOVE "Y" TO LC-SALEFIN.
           MOVE SPACES TO IO-TYPE.
           IF LOAN-KEY NOT = 0
      * ROUTE-CD 5 ONLY PASSED FROM PAGE 2 AND 3
              IF ROUTE-CD = "5"
                 MOVE " " TO ROUTE-CD
                 MOVE LOAN-KEY TO LN-ACCTNO
                 MOVE 0 TO LOAN-KEY
                 GO TO START-LN-LOOKUP.
           MOVE "F7-MENU, F5 OR F6-SCAN, ENTER LOAN NO:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DESTROY ALL
              GO TO MENU-KEY.

           IF VDUSTATUS = "1:"
              PERFORM ENTER-SCANKEY
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 MOVE " " TO WK-SAVE-RECORD
                 MOVE 0 TO LOAN-KEY
                 GO TO LOAN-GET-NUMBER
              ELSE
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO LOAN-GET-NUMBER.
           IF VDUNUM0 = 0
              GO TO LOAN-GET-NUMBER.
           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
       START-LN-LOOKUP.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN VOIDED" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-UNWIND )
              MOVE "ACCOUNT HAS BEEN UNWOUND" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

      * CLEAR UNWIND FIELDS FOR POSSIBLE UNWIND INSTEAD OF VOID:

           MOVE " " TO HOLD-WK-UNWIND-REASON-CD.
           MOVE 0   TO HOLD-WK-UNWIND-DRAWER.

           PERFORM DISPLAY-FIELDS.

           GO TO EXIT-ENTER-KEYS.
       MENU-KEY.
           IF LN-ACCTNO NUMERIC
              MOVE LN-ACCTNO TO LOAN-KEY.

           PERFORM MENU-SELECTION.

           MOVE 0 TO ELE.
           PERFORM SAVE-SCREEN.
           IF MENU-FG = 99
              GO TO LOAN-GET-NUMBER.

           MOVE "END" TO IO-TYPE.
       EXIT-ENTER-KEYS.
           IF LC-SALEFIN NOT = "Y"
              PERFORM NOT-SALES-FINANCE
              MOVE "CLR" TO IO-TYPE.

      ***********************************
       ENTER-SCANKEY SECTION.
      ***********************************
           MOVE 2 TO TYP.
           MOVE 1 TO SCAN-FLAG.
           MOVE "NEXT" TO LAST-READ.
           MOVE "N" TO VALID-LOAN.
           MOVE "S  A050SORTALP." TO VDU.

           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE "SCAN:" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A200SORTKEY." TO VDU.
           MOVE "____________________" TO VDUBUF.
           PERFORM SCREENIO.

           GO TO DISPLAY-TYPE.

       ENTER-SCAN.
           MOVE 1 TO SCAN-FLAG.
           MOVE "NEXT" TO LAST-READ.
           MOVE "N" TO VALID-LOAN.

           PERFORM ALARM.

       ENTER-TYPE.
           MOVE
            "ENTER SCAN TYPE: 1-SS#, 2-NAME, 3-SP SS#, 4-SP-NAME:"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N010TYP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              MOVE "S" TO VDUACTION
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM DISPLAY-TYPE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.
       DISPLAY-TYPE.

           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
              MOVE "^-TYPE, F5-SCAN LN, F6-SCAN SS#, ENTER SS#:"
                                                         TO MESS
           ELSE
           IF TYP = 2
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE
              MOVE
              "^-TYPE, F5-SCAN LN, F6-SCAN NAME, ENTER LAST NAME:"
                                                        TO MESS
           ELSE
           IF TYP = 3
              MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE
              MOVE
              "^-TYPE, F5-SCAN LN, F6-SCAN SP SS#, ENTER SP SS#:"
                                                         TO MESS
           ELSE
           IF TYP = 4
              MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE
              MOVE
           "^-TYPE, F5-SCAN LN, F6-SCAN SP NAME, ENTER SP LAST NAME:"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-KEY.
           MOVE "E RA000SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              GO TO ENTER-SCAN.
           IF VDUSTATUS = "1:"
              IF FIRST-SCAN = 0
                 MOVE " ENTER SORT KEY FIRST!!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1:"
              PERFORM SCAN-LOANS
              IF IO-TYPE = "CLR"
                 MOVE SPACES TO IO-TYPE
                 GO TO ENTER-KEY
              ELSE
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1<"
              IF LAST-READ = "NEXT"
                 GO TO READ-FILES-NEXT
              ELSE
                 MOVE SAVE-BW-KEY TO VDUBUF
                 GO TO START-FILES.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF VALID-LOAN = "Y"
                 MOVE "   " TO IO-TYPE
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
       START-FILES.
           IF TYP = 1 OR 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO
           ELSE
              IF TYP = 2 OR 4
                 PERFORM ARRANGE-BW-KEY
                 MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.

           IF TYP = 1
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
           MOVE 0 TO SCAN-FLAG.
       READ-FILES-NEXT.
           MOVE 0 TO FIRST-SCAN.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           MOVE BW-SSNO TO SAVE-SSNO.
           PERFORM DISPLAY-BORROWER.

           MOVE "NEXT" TO LAST-READ.
           MOVE "N" TO VALID-LOAN.
           IF TYP = 1
              MOVE BW-SSNO TO SAVE-BW-KEY
           ELSE
           IF TYP = 2
              MOVE BW-NAME TO SAVE-BW-KEY
           ELSE
           IF TYP = 3
              MOVE BW-SPSSNO TO SAVE-BW-KEY
           ELSE
           IF TYP = 4
              MOVE BW-SPNAME TO SAVE-BW-KEY.
           MOVE 1 TO SCAN-FLAG.
           MOVE 1 TO FIRST-SCAN.
           MOVE " " TO SCAN.
           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A010TYP." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A050SORTALP." TO VDU.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY.
           EXIT.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
            ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      **************************************
       SCAN-LOANS SECTION.
      **************************************
      *    GO TO START-LN4 START-LN5 START-LN6 START-LN7
           GO TO START-LN4 START-LN5 
                 DEPENDING ON SCAN-FLAG.
       START-LN4.
           IF SCAN-ON
              GO TO READ-LN4.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE SAVE-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE.
           MOVE "ON" TO SCAN.
       READ-LN4.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-FG = 0
              IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(1) = SAVE-SSNO)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN4
                 ELSE
                    PERFORM DISPLAY-FIELDS
                    GO TO END-SCAN-LOANS.
           ADD 1 TO SCAN-FLAG.
           MOVE SPACES TO SCAN.
       START-LN5.
           IF SCAN-ON
              GO TO READ-LN5.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE SAVE-SSNO       TO LN-SSNO(2) 
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
           MOVE "ON" TO SCAN.
       READ-LN5.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-FG = 0
              IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(2) = SAVE-SSNO)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN5
                 ELSE
                    PERFORM DISPLAY-FIELDS
                    GO TO END-SCAN-LOANS.

      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN6.
      *    IF SCAN-ON
      *       GO TO READ-LN6.

      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE SAVE-SSNO       TO LN-SSNO(3) 
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(3) = SAVE-SSNO)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN6
      *          ELSE
      *             PERFORM DISPLAY-FIELDS
      *             GO TO END-SCAN-LOANS.
      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN7.
      *    IF SCAN-ON
      *       GO TO READ-LN7.

      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE SAVE-SSNO       TO LN-SSNO(4) 
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(4) = SAVE-SSNO)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN7
      *          ELSE
      *             PERFORM DISPLAY-FIELDS
                    GO TO END-SCAN-LOANS.

           MOVE "CLR" TO IO-TYPE.
           MOVE 1 TO SCAN-FLAG.
           MOVE SPACES TO SCAN LN-REC.
       END-SCAN-LOANS.
           EXIT.

      **************************************
       BWLN-SCAN SECTION.
      **************************************
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO BWLNSC-SYSDATE.
           MOVE 0        TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BWLN-SCAN.
           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           MOVE 0 TO IO-FG.
       EXIT-BWLN-SCAN.
           EXIT.

      *****************************************************
       UNWIND-SCAN SECTION.
      *****************************************************

           MOVE 30     TO CDSCAN-BEGIN-X.
           MOVE 04     TO CDSCAN-BEGIN-Y.
           MOVE "UW"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" 
              MOVE 9 TO IO-FG
              GO TO EXIT-UNWIND-SCAN.

           MOVE "S  A020REASON."        TO VDU.
           MOVE CDSCAN-CODE             TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.

       EXIT-UNWIND-SCAN.
           EXIT.

      ************************************
       DISPLAY-BORROWER SECTION.
      ************************************
           MOVE " " TO DISPLAY-BUF3.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-X010(1).
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-BW-WK.
           MOVE "# LOANS:" TO D-BW-PROMPT.
           MOVE BW-LOANS TO D-BW-LOANS.
           MOVE D-BW-WK TO D-X011(1).
           MOVE BW-ADR1 TO INSPECT-W.
           MOVE BW-ADR2 TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-X011(2).
           MOVE BW-CITY TO INSPECT-W.
           MOVE BW-STATE TO ADD-ON.
           PERFORM ADD-IT.
           MOVE BW-ZIP TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-X011(3).
           MOVE BW-SPSSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-X010(4).
           MOVE BW-SPLNAME TO INSPECT-W.
           MOVE BW-SPFNAME TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-BW-WK.
           MOVE "(SPOUSE)" TO D-BW-PROMPT.
           MOVE D-BW-WK TO D-X011(4).
      *    CALL XFER USING STAT DISPLAY-LIST3 DISPLAY-BUF3.
      *    CALL WRFORM USING STAT DISPLAY-LIST3.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           MOVE DISPLAY-BUF3  TO WR-BUF.
           PERFORM CALL-WRFORM.


      *************************************************
      *     ENTRY-MODULE
      *     NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                1 - INITIAL NEW RECORD ENTRY
      *                2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

      * VERIFY THAT AN UNWIND CAN OCCUR,
      * SEE IF DEALER PAYABLE CHECK HAS BEEN PRINTED (CUT)

           MOVE " " TO DL-CK-PRINTED-FG.
           IF DL-CK-PRINTED-FG = "Y"
              MOVE "F1-CLEAR, F7-MENU, VOID, UNWIND, P2, P3:" TO MESS
           ELSE
              MOVE "F1-CLEAR, F7-MENU, VOID, P2, P3 :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              IF LN-ACCTNO NUMERIC
                 MOVE LN-ACCTNO TO LOAN-KEY.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPJ-SCREEN
              PERFORM MENU-SELECTION
              PERFORM SAVE-SCREEN
              IF MENU-FG = 99
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "    P2"
              PERFORM SAVE-SCREEN
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE "K0K" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "    P3"
              PERFORM SAVE-SCREEN
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE "U0U" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "UNWIND"
              IF DL-CK-PRINTED-FG NOT = "Y"
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE UNWIND-SEQ TO PASSWDW-SEQ
                 MOVE "TO UNWIND THIS SALES CONTRACT" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE
                 END-IF
                 PERFORM VERIFY-VOID
                 PERFORM UNWIND-WINDOW
                 IF UNWIND-RTN NOT = " "
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE 0 TO LOAN-KEY
                    PERFORM SAVE-SCREEN
                    MOVE "J1J" TO NEW-PROG-FORM
                    MOVE "1" TO NEW-PROG-SUB
                    MOVE "P" TO ERRCD
                    MOVE "END" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.

           IF ACTION = "  VOID"
      * TO VOID ANY SALES CONTRACT
              MOVE VOID-SEQ TO PASSWDW-SEQ
              MOVE "TO VOID THIS SALES CONTRACT" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF ( PASSWDW-STAT NOT = "OK" )
                 GO TO MODIFY-ROUTINE
              ELSE
      * TO VOID ON A DIFFERENT DATE
                 IF ( LN-ENTDATE NOT = TRANS-DATE )
                    MOVE VOID-DIFF-DATE-SEQ TO PASSWDW-SEQ
                    PERFORM VERIFY-PASSWORD
                    IF ( PASSWDW-STAT NOT = "OK" )
                       GO TO MODIFY-ROUTINE
                    ELSE
                       PERFORM VERIFY-VOID
                 ELSE
                    PERFORM VERIFY-VOID.

           IF IO-TYPE = "END"
              GO TO END-ENTRY-MODULE.

           GO TO MODIFY-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE 0 TO LOAN-KEY.
           MOVE " " TO DISPLAY-BUF1.
           MOVE "Y" TO VALID-LOAN.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.

           MOVE LC-DESC TO D-LC-DESC.
           MOVE LN-SUM-LNAMT TO D-LNAMT.
           MOVE LN-PROCEEDS TO D-PROCEEDS.
           COMPUTE ANTIC =   LN-ANTICERN(1) + LN-ANTICERN(2)
                           + LN-ANTICERN(3) + LN-ANTICERN(5).
           IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                          GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(6) TO ANTIC.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(4) TO ANTIC.

           MOVE ANTIC TO D-ANTIC.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO SUB.
       DISP-NEXT-BW.
           MOVE LN-SSNO(SUB) TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF NUM-BW NOT = 0
              MOVE NUM-BW TO BW-SSNO
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(SUB)
              MOVE INSPECT-W TO D-011(SUB)
              MOVE LN-MAKERCD(SUB) TO D-012(SUB).
           IF SUB = 1
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LNNO.
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-ORGST TO D-050.
           MOVE LN-ORGBR TO D-051.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.
           MOVE "PU"     TO CD-TYPE.
           MOVE LN-PURCD TO CD-CODE D-070.
           PERFORM GET-CD.
           MOVE CD-DESC  TO D-071.
           MOVE "SR"    TO CD-TYPE.
           MOVE LN-SRCD TO CD-CODE D-080.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.
           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE D-090.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-091.
           MOVE LN-COLLID TO D-100.

           PERFORM GET-COLLATERAL.

           MOVE LTC-HHGS(1) TO COLLATERAL-BREAKOUT.
           MOVE COLLATERAL-30-1 TO D-110.
           MOVE COLLATERAL-30-2 TO D-111.

           MOVE DISPLAY-LIST1 TO WR-LIST.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE " " TO DISPLAY-BUF2.
           MOVE LN-DLNO TO D-120 DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-121.
           MOVE LN-DLRECOURSE TO D-130.
           PERFORM GET-RECOURSE-DESC.
           MOVE VDUBUF TO D-131.
           IF LN-DLRECOURSE = "P"
               MOVE LN-DLPARTRCAMT TO D-132
           ELSE
               MOVE 0 TO D-132.
           MOVE LN-SUM-ORGTERM TO D-140.
           MOVE 1 TO D-150.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-160.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-170.
           MOVE LN-CASHDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-180.
           MOVE LN-APRATE    TO D-190.
           MOVE LN-OVRDRATE  TO D-200.
           MOVE LN-ENDORATE  TO D-210.
           MOVE LN-SELLPR    TO D-220.
           MOVE LN-DOWNPY    TO D-230.
           MOVE LN-TRADEIN   TO D-231.
           MOVE LN-SUM-SERCHG TO D-240.
           MOVE LN-SUM-MAINTFEE TO D-241.
           MOVE LN-SUM-INTCHG TO D-250.
           MOVE LN-SUM-EXTCHG TO D-251.
           MOVE LN-INSCODES TO D-260.

           MOVE 1 TO SUB OVER-20 SUB2.
       DISP-NEXT-IC.
      * FIRST 3 INSURANCES
           IF SUB < 4
              MOVE LN-INSCOMP(SUB) TO D-1COMP(SUB)
              MOVE LN-INSPREM(SUB) TO D-1PREM(SUB)
              MOVE SUB TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO D-1TERM(SUB)
              MOVE LN-INSCOVR(SUB) TO D-1COVR(SUB)
              MOVE LN-INSOURS(SUB) TO D-1IOURS(SUB).
      * OTHER 2 INSURANCES
           IF SUB > 3 AND SUB < 6
              MOVE LN-INSCOMP(SUB) TO D-2COMP(OVER-20)
              MOVE LN-INSPREM(SUB) TO D-2PREM(OVER-20)
              MOVE LN-INSEXP-DATE(SUB) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY     TO D-2EXP(OVER-20)
              MOVE LN-INSCOVR(SUB) TO D-2COVR(OVER-20)
              MOVE LN-INSOURS(SUB) TO D-2IOURS(OVER-20)
              ADD 1 TO OVER-20.

           IF SUB < 4
              MOVE LN-FEECODE(SUB2) TO D-1FEECD(SUB)
              MOVE LN-FEEAMT(SUB2) TO D-1FEEAMT(SUB)
              MOVE LN-FEEOURS(SUB2) TO D-1FOURS(SUB)
              ADD 1 TO SUB2
              MOVE LN-FEECODE(SUB2) TO D-2FEECD(SUB)
              MOVE LN-FEEAMT(SUB2) TO D-2FEEAMT(SUB)
              MOVE LN-FEEOURS(SUB2) TO D-2FOURS(SUB).

           IF SUB < 5
              ADD 1 TO SUB SUB2
              GO TO DISP-NEXT-IC.

           MOVE LN-REGPYAMT TO D-350.
           MOVE LN-1STPYAMT TO D-351.
           MOVE LN-PURDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-360.
           IF LN-DLRVPRIN(2) = 0
                   AND LN-DLRVPRIN(3) = 0
              MOVE LN-DLRVPRIN(1) TO D-370
           ELSE
              ADD LN-DLRVPRIN(1) LN-DLRVPRIN(2) LN-DLRVPRIN(3)
                                         GIVING D-370
              INSPECT D-370 REPLACING LEADING " " BY "*".

           IF LN-DLRVCHGS(2) = 0
                   AND LN-DLRVCHGS(3) = 0
              MOVE LN-DLRVCHGS(1) TO D-371
           ELSE
              ADD LN-DLRVCHGS(1) LN-DLRVCHGS(2) LN-DLRVCHGS(3)
                                         GIVING D-371
              INSPECT D-371 REPLACING LEADING " " BY "*".

           IF LN-DLPARVPAID = 0
              MOVE LN-DLPARVHELD TO D-380
           ELSE
              ADD LN-DLPARVHELD LN-DLPARVPAID GIVING D-380
              INSPECT D-380 REPLACING LEADING " " BY "*".

           MOVE LN-DLDISC TO D-381.
      *    CALL XFER USING STAT DISPLAY-LIST2 DISPLAY-BUF2.
      *    CALL WRFORM USING STAT DISPLAY-LIST2.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           MOVE DISPLAY-BUF2  TO WR-BUF.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

      *********************************************************
       UNWIND-WINDOW SECTION.
      *********************************************************
           MOVE 7 TO WINDOW-LINES.
           MOVE 40 TO WINDOW-COLUMNS.
           MOVE 10 TO WINDOW-BEGIN-Y.
           MOVE 21 TO WINDOW-BEGIN-X.
           MOVE "WI/UNWIND" TO WINDOW-FORM-NAM.
           MOVE "UNWIND"    TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY UNWIND-SCREEN.

           MOVE UNWIND-FORM-FIELDS TO WR-BUF.
           MOVE UNWIND-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

       UNWIND-WINDOW-REASON.
           MOVE " " TO UNWIND-RTN.
           MOVE " F1, F6-SCAN, ENTER REASON CODE:" TO MESS.
           PERFORM SEND-LEGEND-W.
           MOVE "E  A020REASON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO UNWIND-RTN
              GO TO UNWIND-WINDOW-EXIT.
           IF VDUSTATUS = "1<"
              PERFORM UNWIND-SCAN
              IF IO-FG = 9
                 GO TO UNWIND-WINDOW-REASON.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO UNWIND-WINDOW-REASON.
           MOVE "UW"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE
                          HOLD-WK-UNWIND-REASON-CD.
           PERFORM GET-CD.
           IF IO-FG = 9
              MOVE "INVALID REASON CODE" TO MESS
              PERFORM SEND-MESS
              GO TO UNWIND-WINDOW-REASON.
           MOVE "S  A035DESC." TO VDU.
           MOVE CD-DESC     TO VDUBUF.
           PERFORM SCREENIO.

       UNWIND-WINDOW-DRAWER.
           MOVE " F1, ENTER CASH DRAWER ENTRY:" TO MESS.

      * GLB 3/21/03 (WORLD) HEY, WE REALLY NEED TO TEST THE PASSWORD
      * DRAWER FLAG OF THE POSTING BRANCH IF DOING INTERBRANCH,
      * SO READ POSTING BRANCH THEN RESTORE OWNING BRANCH:

           IF PATH-MACHINE NOT = " "
              MOVE BRANCH TO BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-PASSWORD-DRAWER TO SV-PASSWORD-DRAWER
              MOVE LN-OWNBR TO BR-NO
              PERFORM READ-BR-FILE
           ELSE
              MOVE BR-PASSWORD-DRAWER TO SV-PASSWORD-DRAWER.

           IF SV-PASSWORD-DRAWER = "Y"
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "PASSWORD:"
           ELSE
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "NUMBER  :".
           PERFORM SEND-LEGEND-W.
      * ---
       UNWIND-ACCEPT-ENTER-TILLNO.
           MOVE TRANS-DATE TO CA-DATE.
           MOVE BRANCH     TO CA-BRNO.
           MOVE 0          TO CA-TILLNO.
           MOVE CA1-KEY    TO HOLD-CA1-KEY.
           IF SV-PASSWORD-DRAWER = "Y"
              IF PATH-MACHINE NOT = " "
                 MOVE EXT-FILPATH-BASE TO CA-ALL-BASE
                 MOVE PATH-MACHINE TO CA-ALL-MACHINE
                 MOVE PATH-DATA TO CA-ALL-DATA
                 MOVE CA-ALL-PATH TO CA-PATH
                 MOVE "Y" TO CA-PATH-OVERRIDE
              END-IF
              PERFORM ENTER-CASH-DRAWER-PASSWORD
              IF CAPASSWD-OKAY = "V"
                 GO TO UNWIND-WINDOW-REASON
              END-IF
              IF CAPASSWD-OKAY = "N"
                 GO TO UNWIND-ACCEPT-ENTER-TILLNO
              END-IF
              IF CAPASSWD-OKAY = "Y"
                 MOVE "SN N020DRAWER." TO VDU
                 MOVE CA-TILLNO TO VDUNUM0
                                   HOLD-WK-UNWIND-DRAWER
                 PERFORM SCREENIO
              END-IF
           ELSE
              MOVE "EN N020DRAWER." TO VDU
              PERFORM SCREENIO
              MOVE VDUNUM0 TO HOLD-WK-UNWIND-DRAWER.

       UNWIND-ACCEPT-CHECK-VDUSTATUS.
           IF VDUSTATUS = "1U"
              GO TO UNWIND-WINDOW-REASON.
           IF VDUSTATUS NOT = "00"
              GO TO UNWIND-ACCEPT-ENTER-TILLNO.

       UNWIND-WINDOW-ACCEPT.
           MOVE " F1, ^-BACK, F7-ACCEPT:" TO MESS.
           PERFORM SEND-LEGEND-W.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO UNWIND-RTN
              GO TO UNWIND-WINDOW-EXIT.
           IF VDUSTATUS = "1U"
              GO TO UNWIND-WINDOW-REASON.
           IF VDUSTATUS NOT = "1>"
              GO TO UNWIND-WINDOW-ACCEPT.

       UNWIND-WINDOW-EXIT.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY UNWIND-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "LONPJ" TO VDU-FORM-NAME.



      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE LPLOAN-SYSDATE TO LONPJ-SYSDATE.
      * NEED TO CLEAR SCREEN ON F1
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPJ-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPJ-FORM-FIELDS TO WR-BUF.
           MOVE LONPJ-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPJ-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPJ-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPJ_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              COPY "LIBWI/UNWIND_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 5 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF MENU-RESUME = "Y"
              MOVE "N" TO MENU-RESUME
              IF WK-OLD-PROG-FORM = "  "
                 MOVE 99 TO MENU-FG
                 GO TO EXIT-SAVE-SCREEN
              ELSE
                 MOVE WK-OLD-PROG-FORM TO NEW-PROG-FORM OLD-PROG-FORM
                 MOVE " " TO WK-OLD-PROG-FORM.
           MOVE ELE TO WK-ELE.
           MOVE LN-REC TO WK-SAVE-REC.
           MOVE HOLD-WK-UNWIND-REASON-CD TO WK-UNWIND-REASON-CD.
           MOVE HOLD-WK-UNWIND-DRAWER    TO WK-UNWIND-DRAWER.
           PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       RESTORE-SCREEN SECTION.
      *************************************
           MOVE 0 TO LN-ACCTNO.
           MOVE 5 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-PROG-FORM NOT = OLD-PROG-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM.
           MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-FG = 0
              GO TO RESTORE-CLEAR.
           MOVE " " TO WK-SAVE-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE 0 TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           IF LN-ACCTNO = LOAN-KEY
              MOVE 0 TO NEW-FLAG.
           PERFORM CLOSE-WK-FILE.

       COPY "LIBLP/LPMENU.CPY".
           IF LN-ACCTNO NUMERIC AND MENU-FG NOT = 99
              MOVE LN-ACCTNO TO LOAN-KEY.

      ******************************************************************
       VERIFY-VOID SECTION.
      ******************************************************************
           IF (BRANCH NOT = LN-OWNBR )
              MOVE "USER BRANCH MUST BE THE SAME AS LOAN BRANCH"
                    TO MESS
              PERFORM SEND-MESS
              GO TO END-VERIFY-VOID.

           MOVE 0 TO LOAN-KEY.
           PERFORM SAVE-SCREEN.
           MOVE "J1J" TO NEW-PROG-FORM.
           MOVE "1" TO NEW-PROG-SUB.
           MOVE "P" TO ERRCD.
           MOVE "END" TO IO-TYPE.

       END-VERIFY-VOID.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
            MOVE " " TO PASSWDW-STAT.
            MOVE "LONPJ0" TO PASSWDW-PGM.
            MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
            MOVE "CL/PASSWD" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
            CANCEL FORM-PROGX.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/CAPASSWD.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/SYSTEM.CPY".
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
       GET-COLLATERAL.
           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO LTC-HHGS(1) LTC-HHGS(2).
           PERFORM CLOSE-LTC1-FILE.
       GET-RECOURSE-DESC.
           MOVE " INVALID" TO VDUBUF.
           IF LN-DLRECOURSE = "F"
              MOVE "ULL" TO VDUBUF.
           IF LN-DLRECOURSE = "P"
              MOVE "ARTIAL" TO VDUBUF.
           IF LN-DLRECOURSE = "N"
              MOVE "ONE" TO VDUBUF.
       SEND-CD-DESC.
           PERFORM GET-CD.
           MOVE "S  A000  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
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
           IF NUM-BW NOT = 0
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
       NOT-SALES-FINANCE.
           MOVE "INVALID FOR SALES FINANCE" TO MESS.
           PERFORM SEND-MESS.
       MSG-LINE.
      *    CALL TMMSG USING MESS.
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       SEND-LEGEND-W.
           MOVE "S  A400WSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-LEGEND.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.

       STOP-RUN.
           IF NEW-PROG-SUB = 1 AND ERRCD = "P"
               NEXT SENTENCE
           ELSE
               DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
               DESTROY LONPJ-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           EXIT PROGRAM.
