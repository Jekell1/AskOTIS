       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFINFO.
       DATE-WRITTEN.    012484.
      *****************************************************
      *                 (WI)
      *  DESCRIPTION:   SALES FINANCE INFORMATION
      *                      CALLED FROM F9 CLMENU AND
      *                             WI/DLPOFF, WI/SFMAST
      *  REV:
      *  MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 020397 ADDED LN-PURDATE
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 080699 ADD LPO, REP CODE, AND PROCESS CODE DISPLAY IF GP-I-AM-TFC
      *  BAH 060328 REMOVED TFC HARD CODING AND ADDED LN-LPO AND
      *             LN-REPCD FOR EVERYONE, PR# 125 REGACC
      *  BAH 070417 ADDED LN-PROCEEDS AND LN-TOTPAYMNTD FOR BARNETT PR# 2958
      *  BAH 070821 ADDED LN-TOTEXCPAYMNTD AND RISK % FOR BARNETT PR# 2965
      *   CS 160225 CHANGED DL-BRANCH TO DL-BRNO
      *  BAH 170424 ACTIVATE 8 DIGIT DATES, PD0006
      *****************************************************
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
           "@(#)WI/SFINFO.CBL S35 04/22/24-10:49:01 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  SFINFO-LINES                PIC 99  VALUE 21.
       01  SFINFO-COLUMNS              PIC 99  VALUE 78.
       01  SFINFO-BEGIN-Y              PIC 99  VALUE 2.
       01  SFINFO-BEGIN-X              PIC 99  VALUE 2.

       01  ERRCD           PIC XX   VALUE " ".

       01  WK-RISK         PIC S9(5)V99 COMP-3 VALUE 0.
      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************

       01  DISPLAY-BUF1        VALUE " ".
           03  D-DLNO          PIC ZZZ9.99.
           03  D-010           PIC X(25).
           03  D-020           PIC X(25).
           03  D-030           PIC X(25).
           03  D-040           PIC X(29).
           03  D-050           PIC Z/999/999/9999.
           03  D-060           PIC X(16).
           03  D-070           PIC ZZ99.
           03  D-071           PIC X(20).
           03  D-080           PIC XXX.
           03  D-090           PIC 99/99/99  BLANK ZERO.
           03  D-100           PIC 99/99/99  BLANK ZERO.
           03  D-110           PIC XXX.
           03  D-300           PIC Z(5)9.99   BLANK ZERO.
           03  D-310           PIC Z(5)9.99   BLANK ZERO.
           03  D-320           PIC Z(5)9.99   BLANK ZERO.
           03  D-330           PIC Z(5)9.99   BLANK ZERO.
           03  D-340           PIC X.
           03  D-341           PIC X(6).
           03  D-350           PIC Z(5)9.99   BLANK ZERO.
           03  D-360           PIC 9(5)       BLANK ZERO.
           03  D-370           PIC 99/99/99   BLANK ZERO.
           03  D-PURDATE       PIC 99/99/99   BLANK ZERO.
           03  D-380           PIC ZZ9.
           03  D-390           PIC Z(5)9.99.
           03  D-400           PIC Z(5)9.99.
           03  D-410           PIC Z(5)9.99.
           03  D-420           PIC X(4).
           03  D-430           PIC X(8).
           03  D-CHECK         PIC Z(5)9.99.
           03  D-TOTPAYMNTD    PIC Z(5)9.99.
           03  D-TOTEXCPAYMNTD PIC Z(5)9.99.
           03  D-RISK          PIC Z(5)9.99.
           03  D-SIGN          PIC X.
           03  D-PROF          PIC Z(3).99.

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(5)  VALUE
           "DLNO ".
           03  FILLER          PIC X(48)  VALUE
           "010 020 030 040 050 060 070 071 080 090 100 110 ".
           03  FILLER          PIC X(48)  VALUE
           "300 310 320 330 340 341 350 360 370 PURDATE 380 ".
           03  FILLER          PIC X(53)  VALUE
           "390 400 410 420 430 CHECK PAYMNTD NON RISK SIGN PROF.".

       COPY "LIBWI/DLPOFFW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       01  SFINFO-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SFINFO_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/SFINFO_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/SFINFOW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SFINFO_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME SFINFO-BUF EXIT-PATHNAME.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SFINFO" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.

           PERFORM GET-GPENV.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE SFINFO-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  SFINFO-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    SFINFO-COLUMNS + VDU-WIN-W-MOD
                   LINES   SFINFO-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SFINFO-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM" TO VDUBUF
           ELSE
              MOVE "DISC" TO VDUBUF.
           MOVE "S  A04033P:P." TO VDU.
           PERFORM SCREENIO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

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

           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE "INVALID DEALER" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           PERFORM DISPLAY-FIELDS.

           IF SFINFO-DLPOFF-FG NOT = "N"
              MOVE " F7-EXIT, F2-MEMO, F3-DEALER PAYOFF INQUIRY:"
                                                             TO MESS
           ELSE
              MOVE " F7-EXIT, F2-MEMO:" TO MESS.
           PERFORM SEND-LEGEND.

       ENTER-AGAIN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "12"
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO ENTER-AGAIN.

           IF SFINFO-DLPOFF-FG NOT = "N"
              IF VDUSTATUS = "16"
                 MOVE "N" TO DLPOFF-SFINFO-FG
                 MOVE "WI/DLPOFF" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH DLPOFF-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 GO TO ENTER-AGAIN.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           GO TO ENTER-AGAIN.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.
           MOVE DL-DLNO TO D-DLNO.
           MOVE DL-NAME TO D-010.
           MOVE DL-ADR1 TO D-020.
           MOVE DL-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "      " BY ", ++ +".
           INSPECT MESS REPLACING FIRST "++" BY DL-STATE.
           INSPECT MESS REPLACING FIRST "+         " BY DL-ZIP.
           IF DL-ADR2 = " "
              MOVE MESS TO D-030
           ELSE
              MOVE DL-ADR2 TO D-030
              MOVE MESS TO D-040.

           IF DL-PHONE NOT = 0
              MOVE DL-PHONE TO D-050
              INSPECT D-050 REPLACING LEADING "/" BY " ".
              INSPECT D-050 REPLACING ALL "/" BY "-".

           MOVE DL-SHORT TO D-060.
           MOVE DL-BRNO   TO D-070.
           MOVE DL-BRNO   TO BR-NO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 0
              MOVE BR-NAME  TO D-071.
           MOVE DL-PURPOSE  TO D-080.
           MOVE DL-ORIGDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-090.
           MOVE DL-TERMDATE TO D-100.
           MOVE DL-MFGCODE  TO D-110.

           COMPUTE D-300 =
              LN-DLRVPRIN(1) + LN-DLRVPRIN(2) + LN-DLRVPRIN(3).
           COMPUTE D-310 =
              LN-DLRVCHGS(1) + LN-DLRVCHGS(2) + LN-DLRVCHGS(3).
           COMPUTE D-320 = LN-DLPARVHELD + LN-DLPARVPAID.
           MOVE LN-DLDISC TO D-330.
           MOVE LN-DLRECOURSE TO D-340.
           IF LN-DLRECOURSE = "F"
              MOVE "ULL" TO D-341
           ELSE
           IF LN-DLRECOURSE = "P"
              MOVE "ARTIAL" TO D-341
           ELSE
           IF LN-DLRECOURSE = "N"
              MOVE "ONE" TO D-341.
           MOVE LN-DLPARTRCAMT TO D-350.
           MOVE LN-DLPOOL   TO D-360.
           MOVE LN-CASHDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-370.
           MOVE LN-PURDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PURDATE.
           MOVE LN-ANTICTERM TO D-380.
           MOVE LN-ORIGPURAMT TO D-390.
           MOVE LN-PURBAL TO D-400.
           MOVE LN-CURBAL TO D-410.
           MOVE LN-LPO TO D-420.
           MOVE LN-REP-CD TO D-430.
           COMPUTE D-CHECK = LN-PROCEEDS + LN-OTHBAL.
           MOVE LN-TOTPAYMNTD TO D-TOTPAYMNTD.
           COMPUTE D-TOTEXCPAYMNTD = LN-TOTEXCPAYMNTD + LN-TOTDEF.
           COMPUTE WK-RISK ROUNDED = LN-PROCEEDS + LN-OTHBAL
                    - LN-TOTPAYMNTD - LN-TOTEXCPAYMNTD - LN-TOTDEF.
           MOVE WK-RISK TO D-RISK.
           IF WK-RISK > 0
              MOVE "+" TO D-SIGN
           ELSE
              MOVE "-" TO D-SIGN.
           IF (LN-PROCEEDS + LN-OTHBAL) > 0
              COMPUTE WK-RISK ROUNDED =
                  ( (LN-TOTPAYMNTD + LN-TOTEXCPAYMNTD + LN-TOTDEF)
                   / (LN-PROCEEDS + LN-OTHBAL) ) * 100
              MOVE WK-RISK TO D-PROF.


           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON SFINFO-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY SFINFO-SCREEN UPON SFINFO-WINDOW-HANDLE.
           MOVE SFINFO-FORM-FIELDS TO WR-BUF.
           MOVE SFINFO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE SFINFO-HELP--FILE TO HELP-LINK-FILE.
           MOVE SFINFO-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SFINFO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A670WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SFINFO-SCREEN.
           DESTROY SFINFO-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
