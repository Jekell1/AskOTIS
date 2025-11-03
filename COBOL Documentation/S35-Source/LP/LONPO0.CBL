       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPO0.
       DATE-WRITTEN.    012484.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   DIRECT LOANS (NON-SALES FINANCE)
      *                 VOID/DELETE PAGE 2
      *
      *=================================================================
      * REV:
      * MJD 042496 ADDED LOGIC FOR BWLNSC AND MORELN WINDOWS.
      * KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *            READING BW-FILE. ADDED BRFILE.
      * MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *
      * KEC 2017.0530 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *  CS 170531 ACTIVATE 8 DIGIT DATES
      * BAH 181211 GLOBAL LNFILE
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
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
           "@(#)LP/LONPO0.CBL S35 02/20/24-14:05:51 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       01  WK-IFN          PIC X(4) VALUE "B5WK".
       01  WK-KEY          PIC 9(2).

       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9       VALUE 0.
       01  FIRST-TIME      PIC 9       VALUE 0.

       01  SUB             PIC 9(6) COMP-3.
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
           03  D-LNNO          PIC X(14).
           03  D-LCNO          PIC 99.
           03  D-LC-DESC       PIC X(15).
           03  D-LNAMT         PIC ZZZ,ZZ9.99.
           03  D-PROCEEDS      PIC ZZZ,ZZ9.99.
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
           03  D-160           PIC Z(5)9.99  BLANK ZERO.
           03  D-170           PIC ZZ.
           03  D-180           PIC X(12).
           03  D-190           PIC X(12).
           03  D-200           PIC Z(5)9.99  BLANK ZERO.
           03  D-210           PIC Z(5)9.99  BLANK ZERO.
           03  D-220           PIC Z(5)9.99  BLANK ZERO.
           03  D-230           PIC Z(5)9.99  BLANK ZERO.
           03  D-240           PIC Z(5)9.99  BLANK ZERO.
           03  D-250           PIC Z(4)9.99-  BLANK ZERO.
           03  D-260           PIC Z(4)9.99-  BLANK ZERO.
           03  D-270           PIC ZZ.
           03  D-280           PIC Z(3)9.99  BLANK ZERO.
           03  D-290           PIC ZZ9.999   BLANK ZERO.
           03  D-300           PIC Z9.99     BLANK ZERO.
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

       01  DISPLAY-LISTS1.
           03  FILLER          PIC X(27)  VALUE
           "LNNO LNKEY LCDESC PRIN FIN ".
           03  FILLER          PIC X(48)  VALUE
           "010 011 012 020 021 022 030 031 032 040 041 042 ".
           03  FILLER          PIC X(28)  VALUE
           "050 060 070 080 090 100 110.".
       01  DISPLAY-LIST1 REDEFINES DISPLAY-LISTS1
                               PIC X(111).
       01  DISPLAY-LISTS2.
           03  FILLER          PIC X(56)  VALUE
           "120 121 130 140 150 160 170 180 190 200 210 220 230 240 ".
           03  FILLER          PIC X(52)  VALUE
           "250 260 270 280 290 300 310 320 330 340 350 360 370 ".
           03  FILLER          PIC X(52)  VALUE
           "380 390 400 410 420 430 440 450 460 470 480 490 500.".
       01  DISPLAY-LIST2 REDEFINES DISPLAY-LISTS2
                               PIC X(160).

       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPO_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBLP/LONPO_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPO_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".


       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPO" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           MOVE " " TO ERRCD.
           MOVE USER-PID TO TEMP-PID.

           PERFORM GET-GPENV.

           PERFORM RESTORE-SCREEN.

           IF ROUTE-CD = "1"
              MOVE " " TO ROUTE-CD
              MOVE 2 TO NEW-FLAG
              GO TO ENTER-LOAN.

           IF NEW-FLAG = 1
              PERFORM DISPLAY-FIELDS
              MOVE 1 TO FIRST-TIME
              GO TO ENTER-LOAN.

       MAIN-MODULE.
           PERFORM FORM-RESET.

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
              GO TO START-LN-LOOKUP.

           MOVE "F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 MOVE SPACES TO WK-SAVE-RECORD
                 MOVE 0 TO LOAN-KEY
                 GO TO LOAN-GET-NUMBER
              ELSE
                 GO TO START-LN-LOOKUP.
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

           PERFORM DISPLAY-FIELDS.
           IF LC-SALEFIN = "Y"
              PERFORM NOT-DIRECT
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           MOVE 2 TO NEW-FLAG.
           GO TO EXIT-ENTER-KEYS.
       MENU-KEY.
           IF LN-ACCTNO NUMERIC
              MOVE LN-ACCTNO TO LOAN-KEY.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPO-SCREEN.

           PERFORM MENU-SELECTION.
           MOVE 0 TO ELE.
           PERFORM SAVE-SCREEN.
           IF MENU-FG = 99
              GO TO LOAN-GET-NUMBER.
           MOVE "END" TO IO-TYPE.
       EXIT-ENTER-KEYS.
           EXIT.

      *************************************************
      *      ENTRY-MODULE
      *      NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                 1 - INITIAL NEW RECORD ENTRY
      *                 2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
      ************************************************
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "F1-CLEAR, F7-MENU, P1, P3 :" TO MESS.
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
              DESTROY LONPO-SCREEN
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
           IF ACTION = "    P1"
              PERFORM SAVE-SCREEN
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE "5" TO ROUTE-CD
              MOVE "N0N" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "    P3"
              PERFORM SAVE-SCREEN
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE "V0V" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           GO TO MODIFY-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
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
           IF SUB = 1
              MOVE BRANCH TO BR-NO
              PERFORM GET-BR
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LNNO.
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-PRBAL TO D-050.
           MOVE LN-PRLNNO TO D-060.
           MOVE LN-PRINTDUE TO D-070.
           MOVE LN-PRFINCHGRB TO D-080.
           MOVE LN-PRINSRB TO D-090.
           MOVE 0 TO D-100 D-110.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.


           MOVE SPACES TO DISPLAY-BUF2.
           MOVE LN-OWNBR TO D-120.
           MOVE LN-OWNST TO D-121.
           MOVE 0 TO D-130.
           MOVE LN-POCD TO D-140.
           MOVE SPACES TO D-150.
           MOVE LN-SELLPR TO D-160.
           MOVE LN-DOWNPY TO D-170.
           MOVE LN-CONPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-ALP TO D-180.
           MOVE LN-RECPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-ALP TO D-190.
           MOVE LN-CURBAL TO D-200.
           ADD LN-INTBAL LN-LCBAL GIVING D-210.
           MOVE LN-OTHBAL TO D-220.
           MOVE LN-PURBAL TO D-230.
           MOVE LN-ACCERN(1) TO D-240.
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
           MOVE DATE-MMDDYY       TO D-350.
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
           MOVE LN-INTPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-420.
           MOVE LN-CL-REBATE TO D-430.
           MOVE LN-AH-REBATE TO D-440.
           MOVE LN-PP-REBATE TO D-450.
           MOVE LN-INT-REBATE TO D-460.
           MOVE LN-SC-REBATE TO D-470.
           MOVE LN-MF-REBATE TO D-480.
           MOVE LN-DF-REBATE TO D-490.
           MOVE LN-EX-REBATE TO D-500.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 7 TO WK-KEY.
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
           MOVE LN-REC TO WK-SAVE-RECORD.
           PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       RESTORE-SCREEN SECTION.
      *************************************
           MOVE 0 TO LN-ACCTNO.
           MOVE 7 TO WK-KEY.
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


      ***************************************
       BWLN-SCAN SECTION.
      ***************************************
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
           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE 0             TO IO-FG.
       EXIT-BWLN-SCAN.
           EXIT.


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE LPLOAN-SYSDATE TO LONPO-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPO-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPO-FORM-FIELDS TO WR-BUF.
           MOVE LONPO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPO-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPO-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPO_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMENU.CPY".
           IF LN-ACCTNO NUMERIC AND MENU-FG NOT = 99
              MOVE LN-ACCTNO TO LOAN-KEY.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/SYSTEM.CPY".
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
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
       SP-INVALID.
           MOVE "ENTRY INCONSISTENT WITH STATE PARAMETERS" TO MESS.
           PERFORM SEND-MESS.
       NOT-DIRECT.
           MOVE "INVALID FOR DIRECT LOANS" TO MESS.
           PERFORM SEND-MESS.
       MSG-LINE.
      *    CALL TMMSG USING MESS.
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-LEGEND.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       PROFILE-DASH.
           INSPECT PROFILE-ALP REPLACING ALL "9" BY "-".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPO-SCREEN.
           EXIT PROGRAM.
