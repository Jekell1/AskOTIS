       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONIK0.
       DATE-WRITTEN.    012484.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   SALES FINANCE LOAN
      *                 CONVERSION - PAGE 2
      *
      *=================================================================
      * REV:
      * BAH 080390 REPLACED LN-LNAMT WITH LN-SUM-LNAMT FOR ADDONS
      * JTG 092091 ALLOWED LN-PRBAL LN-PRINTDUE LN-PRFINCHGRB
      *            LN-PRINSRB TO BE NEGATIVE
      * JTG 032593 REVISED TO PASS BR-REC AND GB-REC
      * JTG 042293 REVISED TO WORK WITH NEW DATE ROUTINES
      * JTG 091093 ADDED LOGIC FOR LN-PLTYPE
      * KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      * MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *            EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *            OTHERWISE EXCLUDE BUCKET 6.
      *  CS 991018 ADD TEST OF BULK-CONVERSION-FG TO DETERMINE IF SCREEN
      *            SHOULD DISPLAY CONVERSION OR BULK PURCHASE
      *  CS 991020 ADDED BULK-CONVERSION-FG MUST MATCH LN-CONVERCD
      *  CS 991130 REMOVED TEST ON LN-REPO AND LN-PLDATE BELONGS IN LONIF0
      * BAH 001017 ADDED 51. O1 REBATE AND 52. O2 REBATE
      *  CS 081112 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE OTHER
      *            AND MAINTFEE(ANTICERN(4) FROM EARNED/UNEARNED REGACC PR#210
      * MJD 160706 REMOVED MORELN, REMOVED LN-SEQNO, CORRECTED SCREEN
      *            ENTRY OT ACTNO AND REMOVED MORELN FROM SCREEN DISPLAY.
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 170609 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181210 GLOBAL LNFILE
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
           "@(#)LP/LONIK0.CBL S35 02/20/24-15:03:42 >".
      **********************************
      *       F I L E   P A T H S      *
      **********************************
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  ONE                PIC S9.
       01  ELE                PIC 9(6).
       01  ACTION.
           03  CHKEY          PIC X(4).
           03  CHNO           PIC 9(2).
       01  IO-TYPE            PIC X(3).
       01  NEW-FLAG           PIC 9       VALUE 0.
       01  FIRST-ELE          PIC 99 COMP-3 VALUE 0.
       01  ORIGINAL-LN-REC    PIC X(LN-SIZE).
       01  CHANGED-LN-REC     PIC X(LN-SIZE).

       01  SUB             PIC 9(6) COMP-3.
       01  OVER-20         PIC 99.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
       01  PROFILE-NUM     PIC 9(12).
       01  PROFILE-ALP REDEFINES PROFILE-NUM
                           PIC X(12).

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

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LNNO          PIC 9(8).
           03  D-P             PIC X(2).
           03  D-PTYPE         PIC X(2).
           03  D-J             PIC X(3).
           03  D-LCNO          PIC 99.
           03  D-LC-DESC       PIC X(15).
           03  D-LNAMT         PIC ZZZZZ9.99.
           03  D-PROCEEDS      PIC ZZZZZ9.99.
           03  D-010-040       OCCURS 4.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-050           PIC Z(5)9.99- BLANK ZERO.
           03  D-060           PIC 9(8)      BLANK ZERO.
           03  D-070           PIC Z(3)9.99- BLANK ZERO.
           03  D-080           PIC Z(3)9.99- BLANK ZERO.
           03  D-090           PIC Z(3)9.99- BLANK ZERO.
           03  D-100           PIC Z(3)9.99  BLANK ZERO.
           03  D-110           PIC Z(3)9.99  BLANK ZERO.
       01  DISPLAY-BUF2 REDEFINES DISPLAY-BUF1.
           03  D-120           PIC Z(4).
           03  D-121           PIC XX.
           03  D-130           PIC ZZ.
           03  D-140           PIC XX.
           03  D-150           PIC X.
           03  D-160           PIC ZZZZ.
           03  D-170           PIC Z9        BLANK ZERO.
           03  D-180           PIC X(12).
           03  D-190           PIC X(12).
           03  D-200           PIC Z(5)9.99  BLANK ZERO.
           03  D-210           PIC Z(5)9.99  BLANK ZERO.
           03  D-220           PIC Z(5)9.99  BLANK ZERO.
           03  D-230           PIC Z(5)9.99  BLANK ZERO.
           03  D-240           PIC Z(5)9.99  BLANK ZERO.
           03  D-250           PIC Z(4)9.99-  BLANK ZERO.
           03  D-260           PIC Z(4)9.99-  BLANK ZERO.
           03  D-270           PIC Z9        BLANK ZERO.
           03  D-280           PIC Z(3)9.99  BLANK ZERO.
           03  D-290           PIC ZZ9.999   BLANK ZERO.
           03  D-300           PIC ZZ9.99    BLANK ZERO.
           03  D-310           PIC Z(8)      BLANK ZERO.
           03  D-320           PIC Z(5)9.99  BLANK ZERO.
           03  D-330           PIC 99/99/99  BLANK ZERO.
           03  D-340           PIC 99/99/99  BLANK ZERO.
           03  D-350           PIC 99/99/99  BLANK ZERO.
           03  D-360           PIC 99/99/99  BLANK ZERO.
           03  D-370           PIC 99/99/99  BLANK ZERO.
           03  D-380           PIC 99/99/99  BLANK ZERO.
           03  D-390           PIC 99/99/99  BLANK ZERO.
           03  D-400           PIC 99/99/99  BLANK ZERO.
           03  D-410           PIC ZZZ9.
           03  D-420           PIC 99/99/99  BLANK ZERO.
           03  D-430           PIC X.
           03  D-440           PIC X.
           03  D-450           PIC X.
           03  D-460           PIC X.
           03  D-470           PIC X.
           03  D-480           PIC X.
           03  D-490           PIC X.
           03  D-500           PIC X.
           03  D-510           PIC X.
           03  D-520           PIC X.

       01  DISPLAY-LISTS1.
           03  FILLER          PIC X(31)  VALUE
           "LNNO PJ LNKEY LCDESC PRIN DLCK ".
           03  FILLER          PIC X(48)  VALUE
           "010 011 012 020 021 022 030 031 032 040 041 042 ".
           03  FILLER          PIC X(28)  VALUE
           "050 060 070 080 090 100 110.".
       01  DISPLAY-LIST1 REDEFINES DISPLAY-LISTS1
                               PIC X(112).
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(56)  VALUE
           "120 121 130 140 150 160 170 180 190 200 210 220 230 240 ".
           03  FILLER          PIC X(52)  VALUE
           "250 260 270 280 290 300 310 320 330 340 350 360 370 ".
           03  FILLER          PIC X(52)  VALUE
           "380 390 400 410 420 430 440 450 460 470 480 490 500 ".
           03  FILLER          PIC X(8)  VALUE
           "510 520.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 52.
       01  SPEC-TABLE.
      * 1  LN-SSNO(1)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 2  LN-SSNO(2)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 3  LN-SSNO(3)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 4  LN-SSNO(4)
           03  FILLER      PIC X(8) VALUE "E  A110N".
      * 5  LN-PRBAL
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 6  LN-PRLNNO
           03  FILLER      PIC X(8) VALUE "ENNN080 ".
      * 7  LN-PRINTDUE
           03  FILLER      PIC X(8) VALUE "ENNS042 ".
      * 8  LN-PRFINCHGRB
           03  FILLER      PIC X(8) VALUE "ENNS042 ".
      * 9  LN-PRCLRB
           03  FILLER      PIC X(8) VALUE "ENNS042 ".
      * 10 LM-PRAHRB
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 11 LM-PROTRB
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 12 LN-OWNBR
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 13 LN-PREBR
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 14 LN-POCD
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 15 LN-MORELN
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 16 LN-DLPOOL
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 17 LN-DELCNT
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 18 LN-CONPROFILE
           03  FILLER      PIC X(8) VALUE "ENNN120 ".
      * 19 LN-RECPROFILE
           03  FILLER      PIC X(8) VALUE "ENNN120 ".
      * 20 LN-CURBAL
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 21 LN-INTBAL
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 22 LN-OTHBAL
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 23 LN-PURBAL
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 24 LN-ACCERN(1)
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 25 LN-YTDINTPD
           03  FILLER      PIC X(8) VALUE "ENNS052 ".
      * 26 LN-LYRINTPD
           03  FILLER      PIC X(8) VALUE "ENNS052 ".
      * 27 LN-TOTNODEF
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 28 LN-LCPAID
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 29 LN-JDRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 30 LN-SERVPER
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      * 31 LN-OLDACCTNO
           03  FILLER      PIC X(8) VALUE "ENNN080 ".
      * 32 LN-TOTPAYMNTD
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 33 LN-ENTDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 34 LN-FILEDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 35 LN-DATE-PAID-LAST
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 36 LN-INTREDU
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 37 LN-POFFDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 38 LN-JDDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 39 LN-PLDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 40 LN-PROMDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 41 LN-ANTICTERM
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 42 LN-INTPDTH-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 43 LN-CL-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 44 LN-AH-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 45 LN-PP-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 46 LN-INT-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 47 LN-SC-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 48 LN-MF-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 49 LN-DF-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 50 LN-EX-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 51 LN-O1-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 52 LN-O2-REBATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 52 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONIK_DEF.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONIK_WKS.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONIK_SCN.CPY".
       COPY "LIBLP/LPCMNU_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF.

      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONIK" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

           PERFORM GET-GPENV.

           IF ROUTE-CD = "1"
              MOVE " " TO ROUTE-CD
              MOVE 2 TO NEW-FLAG
              GO TO ENTER-LOAN.

           IF NEW-FLAG = 1
              PERFORM DISPLAY-FIELDS
              GO TO ENTER-LOAN.

       MAIN-MODULE.

           PERFORM FORM-RESET.
           PERFORM DISPLAY-BULK-CONVERSION.

           MOVE " " TO MESS.
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
           MOVE 0 TO NEW-FLAG ELE.
           MOVE SPACES TO IO-TYPE.
           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              MOVE 0 TO LOAN-KEY
              MOVE "SN N060LNNO." TO VDU
              MOVE LN-ACCTNO TO VDUNUM0
              PERFORM SCREENIO
              GO TO START-LN-LOOKUP.
           MOVE "F2-MENU" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "12"
              GO TO MENU-KEY.
           IF VDUSTATUS NOT = "00"
              GO TO LOAN-GET-NUMBER.
           IF VDUNUM0 = 0
              GO TO LOAN-GET-NUMBER.
           MOVE VDUNUM0 TO LN-ACCTNO.
      * VVVVV REMOVED AS PART OF REMOVING MORELN PD0003
      * LOAN-GET-SEQ.
      *     MOVE "F2-MENU" TO MESS.
      *     PERFORM SEND-LEGEND.
      *     MOVE "EN N020LNSQ." TO VDU.
      *     PERFORM SCREENIO.
      *     IF VDUSTATUS = "12"
      *        GO TO MENU-KEY.
      *     IF VDUSTATUS = "1U"
      *        GO TO LOAN-GET-NUMBER.
      *     IF VDUSTATUS NOT = "00"
      *        GO TO LOAN-GET-SEQ.
      *     IF VDUNUM0 = 0
      *        MOVE "SN N020LNSQ." TO VDU
      *        MOVE 1 TO VDUNUM0
      *        PERFORM SCREENIO.
      *     MOVE VDUNUM0 TO LN-SEQNO.
      * ^^^^^ REMOVED AS PART OF REMOVING MORELN PD0003
       START-LN-LOOKUP.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
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

      *---{CS}----
           IF BULK-CONVERSION-FG = "B"
              IF LN-CONVERCD NOT = "B"
                 MOVE "ACCOUNT NOT INPUT AS BULK PURCHASE ACCOUNT"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO LOAN-GET-NUMBER.

           IF BULK-CONVERSION-FG = "C"
              IF LN-CONVERCD NOT = "Y"
                 MOVE "ACCOUNT NOT INPUT AS CONVERSION ACCOUNT"
                                               TO MESS
                 PERFORM SEND-MESS
                 GO TO LOAN-GET-NUMBER.
      *--{CS}---
           MOVE LN-REC TO ORIGINAL-LN-REC.
           PERFORM DISPLAY-FIELDS.
           IF LN-SALEFIN NOT = "Y"
              PERFORM NOT-SALES-FINANCE
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           MOVE 2 TO NEW-FLAG.
           GO TO EXIT-ENTER-KEYS.
       MENU-KEY.
           IF LN-ACCTNO NUMERIC
              MOVE LN-ACCTNO TO LOAN-KEY.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIK-SCREEN.

           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO LOAN-GET-NUMBER.
           MOVE "END" TO IO-TYPE.
       EXIT-ENTER-KEYS.
           EXIT.

      *************************************************
      *     ENTRY-MODULE
      *     NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                1 - INITIAL NEW RECORD ENTRY
      *                2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
       BEGIN-ENTRY.
           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "F2-MENU,F4-MEMO,FIELD#,P1,P3,U-UPDATE" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              IF LN-ACCTNO NUMERIC
                 MOVE LN-ACCTNO TO LOAN-KEY.
           IF VDUSTATUS = "12"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONIK-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "18"
              PERFORM MEMO-ENTRY
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "    P1"
              MOVE LN-ACCTNO TO LOAN-KEY
              PERFORM WRITE-MODULE
              MOVE "J0J" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "    P3"
              MOVE LN-ACCTNO TO LOAN-KEY
              PERFORM WRITE-MODULE
              MOVE "U0U" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "     U"
              PERFORM WRITE-MODULE
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF LN-CURBAL NOT = 0
              OR LN-LCBAL NOT = 0
              OR LN-INTBAL NOT = 0
              OR LN-OTHBAL NOT = 0
              MOVE "CURRENT BAL, INT BAL, AND OTH BAL MUST ALL BE O.OO"
                TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F2.
           IF VDUSTATUS NOT = "12"
              GO TO TEST-F4.
           IF LN-ACCTNO NUMERIC
              MOVE LN-ACCTNO TO LOAN-KEY.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIK-SCREEN.
           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO ENTER-ELE.
           MOVE "END" TO IO-TYPE.
           GO TO END-ROUTINE.
       TEST-F4.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F5.
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-00.
       TEST-00.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE 1 TO ONE.
           IF ELE > 40
              GO TO GOTO-FIELD-40.
           IF ELE > 20
              GO TO GOTO-FIELD-20.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 DEPENDING ON ELE.
       GOTO-FIELD-20.
           SUBTRACT 20 FROM ELE GIVING OVER-20.
           GO TO F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 F-38 F-39 F-40
                 DEPENDING ON OVER-20.
       GOTO-FIELD-40.
           SUBTRACT 40 FROM ELE GIVING OVER-20.
           GO TO F-41 F-42 F-43 F-44 F-45 F-46 F-47 F-48 F-49 F-50
                 F-51 F-52
                 DEPENDING ON OVER-20.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    GO TO CREATE-ROUTINE.
       F-02.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    GO TO CREATE-ROUTINE.
       F-03.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    GO TO CREATE-ROUTINE.
       F-04.
      *    PERFORM BORROWER-ENTRY.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    PERFORM BW-CHECK-ENDORSER.
      *    GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO LN-PRBAL.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM0 TO LN-PRLNNO.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO LN-PRINTDUE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO LN-PRFINCHGRB.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO LN-PRINSRB.
           GO TO CREATE-ROUTINE.
       F-10.
      *    MOVE VDUNUM2 TO LN-PRAHRB.
           GO TO CREATE-ROUTINE.
       F-11.
      *    MOVE VDUNUM2 TO LN-PROTRB.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM0 TO LN-OWNBR.
       F-121.
           MOVE "E  A020121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
               GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
               GO TO F-121.
           MOVE VDUBUF TO LN-OWNST.
           GO TO CREATE-ROUTINE.
       F-13.
      *    MOVE VDUNUM0 TO LN-PREBR.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO LN-POCD.
           GO TO CREATE-ROUTINE.
       F-15.
      *--->MOVE VDUBUF TO LN-MORELN.
      *--->GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM0 TO LN-DLPOOL.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM0 TO LN-DELCNT.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM0 TO LN-CONPROFILE(1).
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM0 TO LN-RECPROFILE(1).
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM2 TO LN-CURBAL.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM2 TO LN-INTBAL.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM2 TO LN-OTHBAL.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM2 TO LN-PURBAL.
           GO TO CREATE-ROUTINE.
       F-24.
      *    MOVE VDUNUM2 TO LN-ACCERN(1).
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUNUM2 TO LN-YTDINTPD.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUNUM2 TO LN-LYRINTPD.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE VDUNUM0 TO LN-TOTNODEF.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE VDUNUM2 TO LN-LCPAID.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDUNUM3 TO LN-JDRATE.
           GO TO CREATE-ROUTINE.
       F-30.
           MOVE VDUNUM2 TO LN-SERVPER.
           GO TO CREATE-ROUTINE.
       F-31.
           MOVE VDUNUM0 TO LN-OLDACCTNO.
           GO TO CREATE-ROUTINE.
       F-32.
           MOVE VDUNUM2 TO LN-TOTPAYMNTD.
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE VDU-DATE-YYYYMMDD TO LN-ENTDATE.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE VDU-DATE-YYYYMMDD TO LN-FILEDATE.
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE VDU-DATE-YYYYMMDD TO LN-DATE-PAID-LAST.
           GO TO CREATE-ROUTINE.
       F-36.
      *    MOVE VDUNUM0 TO LN-INTREDU.
           GO TO CREATE-ROUTINE.
       F-37.
           MOVE VDU-DATE-YYYYMMDD TO LN-POFFDATE.
           GO TO CREATE-ROUTINE.
       F-38.
           MOVE VDU-DATE-YYYYMMDD TO LN-JDDATE.
           GO TO CREATE-ROUTINE.
       F-39.
           MOVE VDU-DATE-YYYYMMDD TO LN-PLDATE.
           IF LN-PLDATE = 0
              MOVE " " TO LN-PLCD
                          LN-PLTYPE
           ELSE
              MOVE "P" TO LN-PLCD
                          LN-PLTYPE.
           GO TO CREATE-ROUTINE.
       F-40.
           MOVE VDU-DATE-YYYYMMDD TO LN-PROMDATE.
           GO TO CREATE-ROUTINE.
       F-41.
           IF VDUNUM0 > LN-ORGTERM
              MOVE " EXCEEDS ORIGINAL TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO LN-ANTICTERM.
           GO TO CREATE-ROUTINE.
      * F-42.
      *     MOVE VDUNUM0 TO LN-LCPDTH-DATE SYS-DATE MMYY-WK.
      *     MOVE "S  A000420." TO VDU.
      *     MOVE MMYY-WK TO VDUBUF.
      *     PERFORM SCREENIO.
      *     IF S-DD < 1 OR S-DD > 12
      *        MOVE "INVALID MONTH" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO ENTER-ELE.
      *     GO TO CREATE-ROUTINE.
       F-42.
           MOVE VDU-DATE-YYYYMMDD TO LN-INTPDTH-DATE.
           GO TO CREATE-ROUTINE.
       F-43.
           MOVE VDUBUF TO LN-CL-REBATE.
           GO TO CREATE-ROUTINE.
       F-44.
           MOVE VDUBUF TO LN-AH-REBATE.
           GO TO CREATE-ROUTINE.
       F-45.
           MOVE VDUBUF TO LN-PP-REBATE.
           GO TO CREATE-ROUTINE.
       F-46.
           MOVE VDUBUF TO LN-INT-REBATE.
           GO TO CREATE-ROUTINE.
       F-47.
           MOVE VDUBUF TO LN-SC-REBATE.
           GO TO CREATE-ROUTINE.
       F-48.
           MOVE VDUBUF TO LN-MF-REBATE.
           GO TO CREATE-ROUTINE.
       F-49.
           MOVE VDUBUF TO LN-DF-REBATE.
           GO TO CREATE-ROUTINE.
       F-50.
           MOVE VDUBUF TO LN-EX-REBATE.
           GO TO CREATE-ROUTINE.
       F-51.
           MOVE VDUBUF TO LN-O1-REBATE.
           GO TO CREATE-ROUTINE.
       F-52.
           MOVE VDUBUF TO LN-O2-REBATE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *********************************
       MEMO-ENTRY SECTION.
      *********************************

           PERFORM SEND-MEMO.

       ENTER-MEMO.
           MOVE "E  A300M2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MEMO.
           MOVE VDUBUF TO LN-MESSAGE.
           IF LN-MESSAGE = " "
              MOVE "M1 M2." TO MESS
              PERFORM RESET-MESS.

      *******************************
       WRITE-MODULE SECTION.
      *******************************
           PERFORM OPEN-LN1-FILE.
           MOVE LN-REC TO CHANGED-LN-REC.
           IF ORIGINAL-LN-REC = CHANGED-LN-REC
              GO TO END-WRITE-MODULE.
           MOVE "UPDATING LOAN MASTER........." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-LN-REC TO LN-REC.
           PERFORM REWRITE-LN1-FILE.
       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.
           MOVE LN-ACCTNO TO D-LNNO.
           IF LN-PLCD = "P"
              MOVE "PL" TO D-P
              IF LN-PLTYPE NOT = "P"
                 MOVE LN-PLTYPE TO D-PTYPE.
           IF LN-JDDATE NOT = 0
              MOVE "JDG" TO D-J.
           MOVE LN-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.
           MOVE LN-SUM-LNAMT TO D-LNAMT.
           MOVE LN-PROCEEDS TO D-PROCEEDS.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO SUB.
       DISP-NEXT-BW.
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(SUB)
              MOVE INSPECT-W TO D-011(SUB)
              MOVE LN-MAKERCD(SUB) TO D-012(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-PRBAL TO D-050.
           MOVE LN-PRLNNO TO D-060.
           MOVE LN-PRINTDUE TO D-070.
           MOVE LN-PRFINCHGRB TO D-080.
           MOVE LN-PRINSRB TO D-090.
      *    MOVE LN-PRAHRB TO D-100.
      *    MOVE LN-PROTRB TO D-110.
           MOVE 0 TO D-100 D-110.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO DISPLAY-BUF2.
           MOVE LN-OWNBR TO D-120.
           MOVE LN-OWNST TO D-121.
      *    MOVE LN-PREBR TO D-130.
           MOVE 0 TO D-130.
           MOVE LN-POCD TO D-140.
      *--->MOVE LN-MORELN TO D-150.
           MOVE SPACE TO D-150.
           MOVE LN-DLPOOL TO D-160.
           MOVE LN-DELCNT TO D-170.
           MOVE LN-CONPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-ALP TO D-180.
           MOVE LN-RECPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-ALP TO D-190.
           MOVE LN-CURBAL TO D-200.
      *    MOVE LN-INTBAL TO D-210.
           ADD LN-LCBAL LN-INTBAL GIVING D-210.
           MOVE LN-OTHBAL TO D-220.
           MOVE LN-PURBAL TO D-230.
      *    MOVE LN-ACCERN(1) TO D-240.

           IF GP-INCL-OTH-IN-EARNED-N
              ADD LN-ACCERN(1) LN-ACCERN(2) LN-ACCERN(3)
                  LN-ACCERN(4) LN-ACCERN(5) GIVING D-240
           ELSE
           IF GP-INCL-OTH-IN-EARNED-R
              ADD LN-ACCERN(1) LN-ACCERN(2) LN-ACCERN(3)
                  LN-ACCERN(5)              GIVING D-240

           ELSE
              ADD LN-ACCERN(1) LN-ACCERN(2) LN-ACCERN(3)
                  LN-ACCERN(4) LN-ACCERN(5) LN-ACCERN(6)
                                            GIVING D-240.

           MOVE LN-YTDINTPD TO D-250.
           MOVE LN-LYRINTPD TO D-260.
           MOVE LN-TOTNODEF TO D-270.
           MOVE LN-LCPAID TO D-280.
           MOVE LN-JDRATE TO D-290.
           MOVE LN-SERVPER TO D-300.
           MOVE LN-OLDACCTNO TO D-310.
           MOVE LN-TOTPAYMNTD TO D-320.
           MOVE LN-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-330.
           MOVE LN-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-340.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-350.
      *    MOVE LN-INTREDU TO D-360.
           MOVE 0 TO D-360.
           MOVE LN-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-370.
           MOVE LN-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-380.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-390.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-400.
           MOVE LN-ANTICTERM TO D-410.
      *    MOVE LN-LCPDTH-DATE TO NUM-DATE.
      *    MOVE NUM-DA TO D-420L.
      *    MOVE "/" TO D-420M.
      *    MOVE NUM-YR TO D-420R.
           MOVE LN-INTPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-420.
           MOVE LN-CL-REBATE TO D-430.
           MOVE LN-AH-REBATE TO D-440.
           MOVE LN-PP-REBATE TO D-450.
           MOVE LN-INT-REBATE TO D-460.
           MOVE LN-SC-REBATE TO D-470.
           MOVE LN-MF-REBATE TO D-480.
           MOVE LN-DF-REBATE TO D-490.
           MOVE LN-EX-REBATE TO D-500.
           MOVE LN-O1-REBATE TO D-510.
           MOVE LN-O2-REBATE TO D-520.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPMNUI.CPY".
           IF LN-ACCTNO NUMERIC AND MENU-FG NOT = 99
              MOVE LN-ACCTNO TO LOAN-KEY.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE LPLONI-SYSDATE TO LONIK-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONIK-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONIK-FORM-FIELDS TO WR-BUF.
           MOVE LONIK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE LONIK-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONIK-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONIK_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
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
       DISPLAY-BULK-CONVERSION.
      *--{CS}--
           MOVE "S  A100TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "BULK PURCH" TO VDUBUF
           ELSE
              MOVE "CONVERSION" TO VDUBUF.
           PERFORM SCREENIO.

       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW
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
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-LEGEND.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       PROFILE-DASH.
           INSPECT PROFILE-ALP REPLACING ALL "9" BY "-".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIK-SCREEN.
           EXIT PROGRAM.
