       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAST.
       DATE-WRITTEN.    012484.
      *****************************************************************
      *                 (WI)
      *  DESCRIPTION:   LOAN MASTER FINANCIALS
      *                   CALLED FROM F9
      * REV:
      *  MJD 071495 ADDED 03 AND ANTIC EARNINGS, REMOVED BORROWER 4.
      *  MJD 071795 ADDED MEMO OPTION.
      *  JTG 090695 CORRECTED DISPLAY OF AMOUNT FINANCED
      *  MJD 072996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *             FOR WEEKLY UNIT PERIODS.
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      *  MJD 013097 ADDED SUBCLASS.
      *  JTG 062097 CHANGED TO DISPLAY LN-INSCODES & ALLOW CTRL N
      *  GLF 101797 ADDED MOVE LN-APPNO TO EXT-WI-CL-APPNO
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  BAH 990901 LN-MESSAGE WAS NOT STAYING DISPLAYED, BECAUSE THE
      *             ELSE ON THE IF HAD A . IN THE MIDDLE OF IT.
      *  JTG 991129 ADDED DISPLAY OF CAN LOAN NO IF LOANS MADE VIA. CANS, WORLD
      *  MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *             ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *  JTG 050819 CHANGED SAVE LOAN, ETC TO USE PD-ANTICERN(4) FOR DISPLAY
      *             OF MAINTENANCE FEES INSTEAD OF THE STORED TRUNCTATED
      *             PD-MAINTFEE * PD-ORGTERM WHICH DOSEN'T EQUAL THE COMPUTED
      *             'VAT' VALUE ADDED TAX IN PD-ANTICERN(4)          WORLD #0408
      *  BLF 070815 CHECK LNMAST-ARCHIVE-FG & SET UP ARCHIVE PATHS FOR LN & LT
      *             IF LOAN IS FROM ARCHIVE, WORLD PR# 547
      *  CS 081118  ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM
      *             ANTICIPATED     REGACC PR#210
      *  MJD 091009 ADDED LOGIC TO USE LN-ANTICERN(4) IF SP-MFFRMLA-05
      *             FOR MEXICO VAT TAX.  LOGIC WAS ALREADY TESTING IF
      *             ORGST WAS "MX" BUT ADDED THIS TO CATCH IN CASE OTHER
      *             STATES USE IT AT A LATER DATE.                   WORLD #651
      *  BAH 110301 WORLD "IL" NEEDS GROSS INCOME, ADDED LN-IL-GROSS-INCOME
      *             REDEFINE LN-MIP-TOT-CLPAID WORLD PR# 748
      *  BAH 120830 ADDED END: ENDORSER RATE FOR MEXICO TO DISCLOSE
      *             THEIR APR, ONLY FOR GP-I-AM-WORLD AND "MX", WORLD PR# 818
      *  BAH 140310 DISPLAY LOAN COST: LN-ANTICERN(6) FOR GP-I-AM-KSTREET
      *             ONLY, PR# 4492 KSTREET
      *  BAH 140710 ADD LN-IL-GROSS-INCOME2 TO LN-IL-GROSS-INCOME FOR DISPLAY
      *                                          WORLD PR# 1002
      *  BLM 141021 IF ARCHIVE, MOVE "Y" TO LN-OVERRIDE-PATH, LT-OVERRIDE-PATH,
      *             NEW LOAD ROUTINES
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      *  CS 170217 CHANGED W-ACCOUNT TO 9(8) FROM 9(6)
      *  CS 181206 GLOBAL LTCFILE
      *  BAH 2024.0214 DISPLAY CLOSING FEE IF LN-POINTS NOT 0, SUBTRACT IT
      *                FROM SERVICE CHG #1644
      *  BAH 2024.0311 REMOVED LNMAST-ARCHIVE-FG, EXT-WI-CL-APP-NUMBER
      *  BAH 2024.0816 DEFAULT SSNO TO XXX-XX-9999, ADD F4 TO CHANGE IT TO
      *                DISPLAY FULL SSNO, #1673 S35Q-118
      *  BAH 2024.1010 TOGGLE BACK TO SSNO XXX-XX-9999 #1673A S35Q-139(FM ALSO)
      *  BAH 2025.0410 ALLOW TO VIEW A VOIDED ACCOUNT, YOU CAN ONLY GET HERE
      *                WITH F9 FROM INQUIRIES WHICH HAVE VOIDS PASSWORDED OR
      *                LOCKED OUT. #1708 S35Q-196
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
           "@(#)WI/LNMAST.CBL S35 02/22/24-12:49:46 >".
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


      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LNFILE".

      *********************************************************
      *   LTCFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTC-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE "/LP/LTCFILE".

       01  LNMAST-LINES               PIC 99  VALUE 22.
       01  LNMAST-COLUMNS             PIC 99  VALUE 78.
       01  LNMAST-BEGIN-Y             PIC 99  VALUE 2.
       01  LNMAST-BEGIN-X             PIC 99  VALUE 2.

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
       01  SSNO-DISPLAY    PIC X VALUE "X".

       01  DISPLAY-BUF-SSNO     VALUE " ".
           03  D-010-SSNO       PIC X(11) OCCURS 4.
       01  DISPLAY-LIST-SSNO    PIC X(16) VALUE "010 020 030 040.".

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LNNO          PIC X(16).
           03  D-LC-DESC       PIC X(15).
           03  D-010-040       OCCURS 3.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-LOANAMT       PIC Z(5)9.99  BLANK ZERO.
           03  D-ANTIC         PIC Z(5)9.99  BLANK ZERO.
           03  D-050           PIC XX.
           03  D-051           PIC 9999      BLANK ZERO.
           03  D-052           PIC 99        BLANK ZERO.
           03  D-053           PIC X(12).
           03  D-060           PIC 99/99/99  BLANK ZERO.
           03  D-061           PIC 99/99/99  BLANK ZERO.
           03  D-062           PIC 99/99/99  BLANK ZERO.
      *--->03  D-070           PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC X(8).
           03  D-071           PIC ZZZ.
           03  D-CAN-NOP       PIC X(7).
           03  D-CAN-NO        PIC XX.
           03  D-080           PIC X(3).
           03  D-081           PIC X(15).
           03  D-090           PIC X(2).
           03  D-091           PIC X(15).
           03  D-100           PIC X(2).
           03  D-101           PIC X(15).
           03  D-110           PIC 9(6)      BLANK ZERO.
           03  D-111           PIC Z(5)9.99  BLANK ZERO.
           03  D-120           PIC Z(5)9.99  BLANK ZERO.
           03  D-120I          PIC XXX.
           03  D-140-180       OCCURS 5.
               05  D-1FEECD     PIC XX.
               05  D-1FEEAMT    PIC Z(3)9.99  BLANK ZERO.
               05  D-2FEECD     PIC XX.
               05  D-2FEEAMT    PIC Z(3)9.99  BLANK ZERO.
           03  D-190           PIC 9(4).99   BLANK ZERO.
           03  D-191           PIC X(15).
      * CLOSING FEE
           03  D-CLSFEE        PIC Z9.99.

       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-200           PIC X(20).
           03  D-201           PIC X(20).
           03  D-202           PIC X(20).
           03  D-203           PIC X(20).
           03  D-203X REDEFINES D-203.
               05  D-203X-P    PIC X(5).
               05  D-203X-F    PIC ZZ9.999   BLANK ZERO.
               05  FILLER      PIC X(8).
           03  D-210           PIC ZZ9.999   BLANK ZERO.
           03  D-220           PIC ZZ9.999   BLANK ZERO.
           03  D-230           PIC ZZ9.999   BLANK ZERO.
           03  D-250-270       OCCURS 3.
               05  D-1COMP      PIC 99        BLANK ZERO.
               05  D-1OUR       PIC X.
               05  D-1TERM      PIC ZZ9       BLANK ZERO.
               05  D-1COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-280-290       OCCURS 3.
               05  D-2COMP      PIC 99        BLANK ZERO.
               05  D-2OUR       PIC X.
               05  D-2EXP       PIC 9(6)      BLANK ZERO.
               05  D-2TERM  REDEFINES D-2EXP
                                PIC Z(6)      BLANK ZERO.
               05  D-2COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-300           PIC Z(5)9.99  BLANK ZERO.
           03  D-310           PIC Z(5)9.99  BLANK ZERO.
           03  D-320           PIC Z(5)9.99  BLANK ZERO.
           03  D-330           PIC Z(5)9.99  BLANK ZERO.
           03  D-340           PIC Z(5)9.99  BLANK ZERO.
           03  D-350           PIC Z(6)9.99  BLANK ZERO.
           03  D-360           PIC Z(5)9.99  BLANK ZERO.
           03  D-370           PIC Z(5)9.99  BLANK ZERO.
           03  D-GROSS-H       PIC X(13).
           03  D-GROSS-F       PIC Z(5)9.99  BLANK ZERO.
           03  FILLER          PIC X(195).

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(17)  VALUE
           "LNNO LNSQ LCDESC ".
           03  FILLER          PIC X(36)  VALUE
           "010 011 012 020 021 022 030 031 032 ".
           03  FILLER          PIC X(50)  VALUE
           "LOANAMT ANTIC 050 051 052 053 060 061 062 070 071 ".
           03  FILLER          PIC X(11)  VALUE
           "CANP:S CAN ".
           03  FILLER          PIC X(53)  VALUE
           "080 081 090 091 100 101 110 111 120 120I 140 141 142 ".
           03  FILLER          PIC X(52)  VALUE
           "143 150 151 152 153 160 161 162 163 170 171 172 173 ".
           03  FILLER          PIC X(31)  VALUE
           "180 181 182 183 190 191 CLSFEE.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(48)  VALUE
           "200 201 202 203 210 220 230 250 251 252 253 254 ".
           03  FILLER          PIC X(44)  VALUE
           "260 261 262 263 264 270 271 272 273 274 280 ".
           03  FILLER          PIC X(56)  VALUE
           "281 282 283 284 290 291 292 293 294 295 296 297 298 299 ".
           03  FILLER          PIC X(48)  VALUE
           "300 310 320 330 340 350 360 370 GROSS-H GROSS-F.".


       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".

       01  LNMAST-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LNMAST_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/LNMAST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LNMAST_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
       MAIN-PROGRAM SECTION.
      ***********************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAST" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

      * ALLOW CTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

           PERFORM GET-GPENV.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE LNMAST-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  LNMAST-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    LNMAST-COLUMNS + VDU-WIN-W-MOD
                   LINES   LNMAST-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LNMAST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A080SYSDATE." TO VDU.
           MOVE EXT-CONAME-SYSDATE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200TITLE." TO VDU.
           MOVE " DIRECT LOAN MASTER " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO END-ROUTINE.

      * REMOVED CHECK FOR VOIDS, CAN ONLY GET HERE WITH F9 FROM INQUIRIES
      * AND THOSE ARE EITHER PASSWORDED OR LOCKED OUT #1708
      *    IF ( LN-POCD-VOID )
      *       MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO END-ROUTINE.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.

           PERFORM DISPLAY-FIELDS.

           MOVE " F7 - EXIT, F2 - MEMO, F4 - SSNO:" TO MESS.
           PERFORM SEND-LEGEND.

       ENTER-AGAIN.
           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "12"
              PERFORM MEMO-SCAN.
           IF VDUSTATUS = "18"
              PERFORM DISPLAY-SSNO.
           GO TO ENTER-AGAIN.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "S  A200CONAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO ELE.
       DISPLAY-FIELDS-BW.
           MOVE LN-SSNO(ELE) TO NUM-BW BW-SSNO.
           PERFORM DISPLAY-LAST-4.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(ELE)
              MOVE INSPECT-W TO D-011(ELE)
              MOVE LN-MAKERCD(ELE) TO D-012(ELE).
           IF ELE = 1
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LNNO.
           IF ELE < 3
              ADD 1 TO ELE
              GO TO DISPLAY-FIELDS-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-ORGST TO D-050.
           MOVE LN-ORGBR TO D-051.
           MOVE LN-SUBCLASS TO D-052.
           MOVE SP-DESC TO D-053.
           MOVE LN-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-062.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-061.

           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-070.

           MOVE LN-ORGTERM TO D-071.

           MOVE LN-PURCD TO D-080 CD-CODE.
           MOVE "PU" TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.

           MOVE LN-COLLCD TO D-090 CD-CODE.
           MOVE "CO" TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-091.

           MOVE LN-SRCD TO D-100 CD-CODE.
           MOVE "SR" TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-101.

           MOVE LN-PRLNNO TO D-110.
           IF LN-PRLNNO NOT  = 0
              COMPUTE D-111 ROUNDED = (LN-PRBAL + LN-PRINTDUE)
                                        - (LN-PRFINCHGRB + LN-PRINSRB).

           MOVE LN-PROCEEDS TO D-120.
           MOVE LN-INSCODES TO D-120I.

           MOVE LN-LNAMT TO D-LOANAMT.
           COMPUTE ANTIC = LN-ANTICERN(1) + LN-ANTICERN(2)
                         + LN-ANTICERN(3)
                         + LN-ANTICERN(5).
           IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                       GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(6) TO ANTIC.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(4) TO ANTIC.

           MOVE ANTIC TO D-ANTIC.

           MOVE 1 TO ELE.
           MOVE 1 TO SUB.
       DISPLAY-FIELDS-NEXT-GL.
           MOVE LN-FEECODE(SUB) TO D-1FEECD(ELE).
           MOVE LN-FEEAMT(SUB) TO D-1FEEAMT(ELE).
           ADD 1 TO SUB.
           MOVE LN-FEECODE(SUB) TO D-2FEECD(ELE).
           MOVE LN-FEEAMT(SUB) TO D-2FEEAMT(ELE).
           IF SUB < 10
              ADD 1 TO ELE SUB
              GO TO DISPLAY-FIELDS-NEXT-GL.

           MOVE LN-DLNO TO D-190 DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-191.

      * CLOSING FEE
           MOVE LN-POINTS TO D-CLSFEE.

           MOVE LN-LNAMT TO D-LOANAMT.

           IF LN-CAN-LOAN-NO NOT = " "
              MOVE "CAN NO:"      TO D-CAN-NOP
              MOVE LN-CAN-LOAN-NO TO D-CAN-NO.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO DISPLAY-BUF2.
           MOVE LN-COLLID TO D-200.

           PERFORM GET-COLLATERAL.
           IF LTC-HHGS(1) NOT = " "
              MOVE LTC-COLL-INFO1(1) TO D-201
              MOVE LTC-COLL-INFO2(1) TO D-202
              MOVE LTC-COLL-INFO3(1) TO D-203.

           IF LN-ORGST = "MX"
              MOVE SPACES TO D-203
              MOVE "END: " TO D-203X-P
              MOVE LN-ENDORATE TO D-203X-F.

           MOVE LN-EFFRATE TO D-210.
           MOVE LN-SMPRATE TO D-220.
           MOVE LN-APRATE TO D-230.

           IF LN-INSCOMP(1) NOT = 0
              MOVE 1 TO ELE
              PERFORM SET-INS.
           IF LN-INSCOMP(2) NOT = 0
              MOVE 2 TO ELE
              PERFORM SET-INS.
           IF LN-INSCOMP(3) NOT = 0
              MOVE 3 TO ELE
              PERFORM SET-INS.

           IF LN-INSCOMP(4) NOT = 0
              MOVE 1 TO SUB
              MOVE 4 TO ELE
              PERFORM SET-OTHER-INS.

           IF LN-INSCOMP(5) NOT = 0
              MOVE 2 TO SUB
              MOVE 5 TO ELE
              PERFORM SET-OTHER-INS.

           IF LN-INSCOMP(6) NOT = 0
              MOVE 3 TO SUB
              MOVE 6 TO ELE
              PERFORM SET-OTHER-INS.

           MOVE 1 TO ELE.

       DISP-FIELDS-OUTPUT.
      * CLOSING FEE
           IF LN-POINTS NOT = 0
              COMPUTE D-300 = LN-SERCHG - LN-POINTS
           ELSE
              MOVE LN-SERCHG TO D-300.
           IF LN-ORGST = "MX" OR SP-MFFRMLA-05
              MOVE LN-ANTICERN(4) TO D-310
           ELSE
              MULTIPLY LN-MAINTFEE BY LN-ORGTERM GIVING D-310.
           MOVE LN-EXTCHG TO D-320.
           MOVE LN-INTCHG TO D-330.
           MOVE LN-1STPYAMT TO D-360.
           MOVE LN-REGPYAMT TO D-370.

           PERFORM LOAN-CALCULATIONS.
           MOVE DISCLOSED-FINAMT TO D-340.
           IF LN-LOANTYPE = "P"
              MOVE LN-LNAMT TO D-350
           ELSE
              MOVE TOTAL-REPAY TO D-350.

      * WORLD PR# 748 
           IF LN-ORGST = "IL"
              IF LN-CLASS = 1 OR 5
                 IF LN-SUBCLASS = 1
                    MOVE "GROSS INCOME:" TO D-GROSS-H
                    IF (LN-IL-GROSS-INCOME2 NOT = 202020202 AND
                                            NOT = 020202020)
                       IF (LN-IL-GROSS-INCOME NOT = 202020202 AND
                                              NOT = 020202020)
                          COMPUTE D-GROSS-F = LN-IL-GROSS-INCOME +
                                              LN-IL-GROSS-INCOME2
                       ELSE
                          MOVE LN-IL-GROSS-INCOME2 TO D-GROSS-F
                    ELSE
                       IF (LN-IL-GROSS-INCOME NOT = 202020202 AND
                                              NOT = 020202020)
                          MOVE LN-IL-GROSS-INCOME TO D-GROSS-F.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MESSAGE
              PERFORM ALARM
           ELSE
              MOVE "S  A300M0." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A060M1." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A300M2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      *************************************
       DISPLAY-SSNO SECTION.
      *************************************
           MOVE 1 TO ELE.
       DISPLAY-SSNO-BW.
           MOVE LN-SSNO(ELE) TO NUM-BW.
           MOVE SPACES       TO ALP-BW.
           IF NUM-BW NOT = 0
              IF SSNO-DISPLAY = "X"
                 MOVE NUM-BW123  TO ALP-BW123
                 MOVE NUM-BW45   TO ALP-BW45
                 MOVE NUM-BW6789 TO ALP-BW6789
                 MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2
              ELSE
                 MOVE "XXX"      TO ALP-BW123
                 MOVE "XX"       TO ALP-BW45
                 MOVE NUM-BW6789 TO ALP-BW6789
                 MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
           IF LN-SSNO(ELE) NOT = 0
              MOVE ALP-BW TO D-010-SSNO(ELE).
           IF ELE < 3
              ADD 1 TO ELE
              GO TO DISPLAY-SSNO-BW.

           MOVE DISPLAY-BUF-SSNO TO WR-BUF.
           MOVE DISPLAY-LIST-SSNO TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF SSNO-DISPLAY = "X"
              MOVE "9" TO SSNO-DISPLAY
            ELSE
              MOVE "X" TO SSNO-DISPLAY.


       EXIT-DISPLAY-SSNO.
           EXIT.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON LNMAST-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY LNMAST-SCREEN UPON LNMAST-WINDOW-HANDLE.
           MOVE LNMAST-FORM-FIELDS TO WR-BUF.
           MOVE LNMAST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE LNMAST-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAST-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LNMAST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       MEMO-SCAN.
           MOVE "WI/MEMOSC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       SET-INS.
           MOVE LN-INSCOMP(ELE) TO D-1COMP(ELE).
           MOVE LN-INSPREM(ELE) TO D-1PREM(ELE).
           MOVE ELE TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-1TERM(ELE).
           MOVE LN-INSCOVR(ELE) TO D-1COVR(ELE).
           MOVE LN-INSOURS(ELE) TO D-1OUR(ELE).
       SET-OTHER-INS.
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
           MOVE LTC-PATH-OWNBR TO LTC-BRNO.
           MOVE LN-ACCTNO      TO LTC-ACCTNO.
           MOVE 1              TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO LTC-COLLATERAL-INFO.
           PERFORM CLOSE-LTC1-FILE.
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
       DISPLAY-LAST-4.
           MOVE " " TO ALP-BW.
           IF NUM-BW NOT = 0
              MOVE "XXX"      TO ALP-BW123
              MOVE "XX"       TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-MESSAGE.
           MOVE "S  A060M1." TO VDU.
           MOVE "MESS: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE LN-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A600MSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
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
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAST-SCREEN.
           DESTROY LNMAST-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
