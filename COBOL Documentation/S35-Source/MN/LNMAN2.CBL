       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN2.
      ******************************************************************
      *
      * DESC: LOAN MASTER FILE MAINTENANCE   PAGE 2
      *
      *=================================================================
      * REV.
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * JTG 072095 ADDED LN-PLAMT, CURBAL AT TIME OF P&L
      * GLF 082096 ADDED LNSTAT LINE TO BOTTOM OF PAGE.
      * GLF 082996 ADDED ORIG-MATURITY AND UPDATED-MATURITY DISPLAYS
      * CS  102796 ADDED LN-YLDRATE
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * JTG 020397 CORRECTED ORIG-MATURITY AND UPDATED-MATURITY DISPLAYS
      * MJD 082197 CHANGED RECORD SIZE TO 1570
      *            ADD LOGIC FOR PAGE 10.
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * BAH 102297 PLAMT WAS LOSING DECIMAL BECAUSE IT WAS MOVING VDUNUM0,#152
      * MJD 000616 ADDED LN-CONRATE (REGACC PR#1220)
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * BAH 061127 CHANGED 8. N/A TO 8. BANKRUPT DISMISSAL, JIM R. IT
      *             WAS NOT DISPLAYING ANYWHERE !
      * MJD 121018 A30 CONVERSION COMPLETE
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *  CS 160711 LN-OWNBR NEEDS REMOVED FROM ENTRY-MODULE
      *  CS 170327 01  SPEC-TABLE.
      *            WAS 03  FILLER     PIC X(15) VALUE "E NM060 ".
      *            CHANGED TO PIC X(8) AND ADDED BACK THE CHFG N   
      *  CS 170424 ACTIVATE 8 DIGIT DATES
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
      * BAH 181212 GLOBAL LNFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/LNMAN2 S35 06/12/24-13:38:13 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".

      *=================================================================
      *   LNCH-REC FROM 'REPLACING LEADING' ON LN-REC. [CHANGED-REC]
      *   LNOR-REC FROM 'REPLACING LEADING' ON LN-REC. [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNCH".
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNOR".

      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1     REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.
       01  FORM-NUM2      REDEFINES ACTION.
           03  FORM-KEY2     PIC X(3).
           03  FORM-P2       PIC X.
           03  FORM-SEL2     PIC 99.
               88  FORM-2-DIGITS  VALUE 10 11.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  LNMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(8).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).
       01  WS-ORGBR          PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC ZZ/ZZ/ZZ.
           03  F02        PIC ZZ/ZZ/ZZ.
           03  F03        PIC ZZ/ZZ/ZZ.
           03  F04        PIC ZZ/ZZ/ZZ.
           03  F05        PIC ZZ/ZZ/ZZ.
           03  F06        PIC ZZ/ZZ/ZZ.
           03  F07        PIC ZZ/ZZ/ZZ.
           03  F08        PIC ZZ/ZZ/ZZ.
           03  FLNORGMA   PIC ZZ/ZZ/ZZ.
           03  FLNUPDTMA  PIC ZZ/ZZ/ZZ.
           03  F09        PIC ZZ/ZZ/ZZ.
           03  F10        PIC ZZ/ZZ/ZZ.
           03  F11        PIC ZZ/ZZ/ZZ.
           03  F111       PIC ZZZZZZ.ZZ.
           03  F12        PIC ZZ/ZZ/ZZ.
           03  F13        PIC ZZ/ZZ/ZZ.
           03  F14        PIC ZZ/ZZ/ZZ.
           03  F15        PIC ZZ/ZZ/ZZ.
           03  F16        PIC ZZ/ZZ/ZZ.
           03  F17        PIC ZZ/ZZ/ZZ.
           03  F171       PIC ZZ/ZZ/ZZ.
           03  F172       PIC ZZ/ZZ/ZZ.
           03  F18        PIC ZZZ.ZZZ.
           03  F19        PIC ZZZ.ZZZ.
           03  F20        PIC ZZZ.ZZZ.
           03  F21        PIC ZZZ.ZZZ.
           03  F22        PIC ZZZ.ZZZ.
           03  F23        PIC ZZZ.ZZZ.
           03  F24        PIC ZZZ.ZZZ.
           03  F25        PIC ZZZ.ZZZ.
           03  F26        PIC ZZZ.ZZZ.
           03  F27        PIC ZZZ.ZZZ.
           03  F28        PIC ZZZ.ZZ.
           03  FLNSTAT    PIC X(30).
       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(38) VALUE
               "060 070 080 LNORGMA LNUPDTMA 090 100 ".
           03  FILLER     PIC X(24) VALUE
               "110 111 120 130 140 150 ".
           03  FILLER     PIC X(28) VALUE
               "160 170 171 172 180 190 200 ".
           03  FILLER     PIC X(39) VALUE
               "210 220 230 240 250 260 270 280 LNSTAT.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       78  MAX                     VALUE 28.
       01  SPEC-TABLE.
           03  FILLER     PIC X(08) VALUE "E NM060N".
           03  FILLER     PIC X(08) VALUE "E NM060N".
           03  FILLER     PIC X(08) VALUE "E NM060N".
           03  FILLER     PIC X(08) VALUE "E NM060N".
           03  FILLER     PIC X(08) VALUE "E NM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "ERNM060N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N033N".
           03  FILLER     PIC X(08) VALUE "EN N032N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN2_DEF.CPY".
       COPY "LIBMN/LNMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMAN2_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE FORM-LINK-AREA TO LNMAIN-LINK.
           MOVE "LNMAN2" TO VDU-FORM-NAME.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           MOVE " " TO VDU-PGMENU-SELECTION.
           PERFORM REC-LN-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 3 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *******************************************
       REC-LN-MODULE SECTION.
      *******************************************
       REC-LN-MODULE-NO.
           MOVE 0 TO LN-ACCTNO LN-OWNBR FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LN-ACCTNO
              MOVE SPACES TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-LNFILE.

           MOVE "N" TO PAGE-CHANGE-FLAG.
           MOVE 0 TO NEW-FLAG.

       REC-LN-ACCTNO.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LN-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LN-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LN-ACCTNO.
           MOVE VDUNUM0 TO LN-ACCTNO.
           IF LN-ACCTNO = 0
              GO TO REC-LN-ACCTNO.

       READ-LNFILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
              MOVE LN-REC TO LNOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              IF NEW-REC = "Y"
                 MOVE 1 TO NEW-FLAG
              END-IF
              IF ( LN-POCD-INRESCISSION ) AND ( PAGE-CHANGE-NO )
                 MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                 PERFORM SEND-MESS
              END-IF.

       END-REC-LN-MODULE.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.
       ENTER-KEY.
           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LN-REC TO LNOR-REC
                 IF ( LN-POCD-INRESCISSION )
                    MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE VDUNUM0       TO LN-ACCTNO.

           PERFORM START-LN1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF ( LN-POCD-VOID ) OR ( LN-POCD-RESCIND )
              GO TO READ-FILES-NEXT.

      *    UNLOCK LN-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N060LNNO." TO VDU.
           MOVE LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LN1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 2 OR FORM-SEL1 > 9
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                   GO TO MODIFY-ROUTINE.
           MOVE SPACES TO VDU.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
                 A-26 A-27 A-28
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDU-DATE-YYYYMMDD TO LN-ENTDATE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDU-DATE-YYYYMMDD TO LN-PURDATE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDU-DATE-YYYYMMDD TO LN-LOANDATE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDU-DATE-YYYYMMDD TO LN-ORIG-1STPYDATE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDU-DATE-YYYYMMDD TO LN-INTDATE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD TO LN-FILEDATE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDU-DATE-YYYYMMDD TO LN-CASHDATE.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDU-DATE-YYYYMMDD TO LN-BNKRPT-DISMISS-DATE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDU-DATE-YYYYMMDD TO LN-POFFDATE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDU-DATE-YYYYMMDD TO LN-JDDATE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDU-DATE-YYYYMMDD TO LN-PLDATE.
       A-111.
           MOVE "ENNN062111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-111.
           MOVE VDUNUM2 TO LN-PLAMT.
           IF LN-PLDATE = 0
              IF (LN-PLAMT NOT = 0)
                 MOVE "P&&L AMOUNT MUST BE ZERO!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-111
              ELSE
                 NEXT SENTENCE
           ELSE
              IF LN-PLAMT = 0
                 MOVE "P&&L AMOUNT CAN'T BE ZERO!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-111.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDU-DATE-YYYYMMDD TO LN-ACCELDATE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDU-DATE-YYYYMMDD TO LN-REDUDATE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDU-DATE-YYYYMMDD TO LN-LEGALDATE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDU-DATE-YYYYMMDD TO LN-BNKRPTDATE.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDU-DATE-YYYYMMDD TO LN-DEBTAIDATE.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDU-DATE-YYYYMMDD TO LN-REPODATE.
           IF LN-REPODATE = 0
              IF LN-REPOCD = "X"
                 MOVE "LOAN REPO CODE = 'X', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
              IF LN-REPOCD = "V"
                 MOVE "LOAN REPO CODE = 'V', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
              IF LN-REPOCD = "S"
                 MOVE "LOAN REPO CODE = 'S', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       A-171.
           MOVE "ERNM060171." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-171.
           MOVE VDU-DATE-YYYYMMDD TO LN-REDEEMDATE.
           IF LN-REDEEMDATE = 0
                    AND LN-REPOCD = "V"
              MOVE "LOAN REPO CODE = 'V', MUST ENTER!" TO MESS
              PERFORM SEND-MESS
              GO TO A-171.
       A-172.
           MOVE "ERNM060172." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-172.
           MOVE VDU-DATE-YYYYMMDD TO LN-SOLDDATE.
           IF LN-SOLDDATE = 0
              IF LN-REPOCD = "S"
                 MOVE "LOAN REPO CODE = 'S', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-172.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM3 TO LN-APRATE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM3 TO LN-EFFRATE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM3 TO LN-SMPRATE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM3 TO LN-OVRDRATE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM3 TO LN-ENDORATE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM3 TO LN-YLDRATE.
           GO TO CREATE-ROUTINE.

       A-24.
           MOVE VDUNUM3 TO LN-CONRATE.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUNUM3 TO LN-JDRATE.
           GO TO CREATE-ROUTINE.
       A-26.
           IF VDUNUM3 > 999.998
              MOVE "RATE MUST BE LESS THAN 999.999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM3 TO LN-ACCRATE.
           GO TO CREATE-ROUTINE.
       A-27.
           IF VDUNUM3 > 999.996
              MOVE "RATE MUST BE LESS THAN 999.997" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM3 TO LN-REDURATE.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUNUM2 TO LN-SERVPER.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************
       WRITE-MODULE SECTION.
      ************************************
           PERFORM OPEN-LN1-FILE.

           MOVE LN-REC TO LNCH-REC.

           IF LNOR-REC = LNCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LN-REC NOT = LNOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LNCH-REC TO LN-REC.

           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      *************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *************************************************
           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-ENTDATE NOT = LNCH-ENTDATE
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "ENT DATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ENTDATE            TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-ENTDATE            TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PURDATE NOT = LNCH-PURDATE
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "PUR DATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PURDATE            TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PURDATE            TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LOANDATE NOT = LNCH-LOANDATE
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "LOANDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LOANDATE           TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY TO             WORK-ERRLOG-ORG-N0
             MOVE LNCH-LOANDATE           TO DATE-YYYYMMDD           
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ORIG-1STPYDATE NOT = LNCH-ORIG-1STPYDATE
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "OR 1STPY"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ORIG-1STPYDATE     TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-ORIG-1STPYDATE     TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTDATE NOT = LNCH-INTDATE
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "INT DATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INTDATE            TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INTDATE            TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FILEDATE NOT = LNCH-FILEDATE
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "FILEDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FILEDATE           TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FILEDATE           TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CASHDATE NOT = LNCH-CASHDATE
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "CASHDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-CASHDATE           TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CASHDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-BNKRPT-DISMISS-DATE NOT = LNCH-BNKRPT-DISMISS-DATE
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "BNK DISM"              TO WORK-ERRLOG-DESC
             MOVE LNOR-BNKRPT-DISMISS-DATE   TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY               TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-BNKRPT-DISMISS-DATE   TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-POFFDATE NOT = LNCH-POFFDATE
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "POFFDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-POFFDATE           TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-POFFDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-JDDATE NOT = LNCH-JDDATE
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "JUD DATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-JDDATE             TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-JDDATE             TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PLDATE NOT = LNCH-PLDATE
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "PL DATE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PLDATE             TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PLDATE             TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ACCELDATE NOT = LNCH-ACCELDATE
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "ACCELDAY"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCELDATE          TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ACCELDATE          TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-REDUDATE NOT = LNCH-REDUDATE
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "REDUDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-REDUDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-REDUDATE           TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LEGALDATE NOT = LNCH-LEGALDATE
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "LEG DATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LEGALDATE          TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LEGALDATE          TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-BNKRPTDATE NOT = LNCH-BNKRPTDATE
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "BANKRUPT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-BNKRPTDATE         TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-BNKRPTDATE         TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DEBTAIDATE NOT = LNCH-DEBTAIDATE
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "DEBT AID"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DEBTAIDATE         TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-DEBTAIDATE         TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-REPODATE NOT = LNCH-REPODATE
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "REPODATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-REPODATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-REPODATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-REDEEMDATE NOT = LNCH-REDEEMDATE
             MOVE "171"                   TO WORK-ERRLOG-FIELD
             MOVE "REDEEM  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-REDEEMDATE         TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-REDEEMDATE         TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SOLDDATE NOT = LNCH-SOLDDATE
             MOVE "172"                   TO WORK-ERRLOG-FIELD
             MOVE "SOLDDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SOLDDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-SOLDDATE           TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-APRATE NOT = LNCH-APRATE
             MOVE "18 "                  TO WORK-ERRLOG-FIELD
             MOVE "APR RATE"             TO WORK-ERRLOG-DESC
             MOVE LNOR-APRATE            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-APRATE            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-EFFRATE NOT = LNCH-EFFRATE
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "EFF RATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-EFFRATE            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-EFFRATE            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SMPRATE NOT = LNCH-SMPRATE
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "SMP RATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SMPRATE            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SMPRATE            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-OVRDRATE NOT = LNCH-OVRDRATE
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "OVRDRATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-OVRDRATE           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-OVRDRATE           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ENDORATE NOT = LNCH-ENDORATE
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "ENDORATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ENDORATE           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ENDORATE           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-YLDRATE NOT = LNCH-YLDRATE
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "YLD RATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-YLDRATE            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-YLDRATE            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CONRATE NOT = LNCH-CONRATE
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "CON RATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-CONRATE            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-CONRATE            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-JDRATE NOT = LNCH-JDRATE
             MOVE "25 "                   TO WORK-ERRLOG-FIELD
             MOVE "JD RATE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-JDRATE             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-JDRATE             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ACCRATE NOT = LNCH-ACCRATE
             MOVE "26 "                   TO WORK-ERRLOG-FIELD
             MOVE "ACC RATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCRATE            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ACCRATE            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-REDURATE NOT = LNCH-REDURATE
             MOVE "27 "                   TO WORK-ERRLOG-FIELD
             MOVE "REDURATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-REDURATE           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-REDURATE           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SERVPER NOT = LNCH-SERVPER
             MOVE "28 "                   TO WORK-ERRLOG-FIELD
             MOVE "SERVPER "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SERVPER            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SERVPER            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

      ********************************************
       LOG-FIELD-CHANGE SECTION.
      ********************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           ACCEPT CRNOR2-SYSDATE FROM CENTURY-DATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE LN-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F01.

           MOVE LN-PURDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO F02.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.

           MOVE LN-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO   F04.

           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F05.

           MOVE LN-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F06.

           MOVE LN-CASHDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F07.

           MOVE LN-BNKRPT-DISMISS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F08.


      *-----------------------ORIGINAL MATURITY---------------------
           MOVE "Y" TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO FLNORGMA.

      *----------------------UPDATED MATURITY-----------------------
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO FLNUPDTMA.

           MOVE LN-POFFDATE TO DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F09.

           MOVE LN-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F10.

           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F11.

           MOVE LN-PLAMT TO F111.

           MOVE LN-ACCELDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F12.

           MOVE LN-REDUDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F13.

           MOVE LN-LEGALDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F14.

           MOVE LN-BNKRPTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F15.

           MOVE LN-DEBTAIDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F16.

           MOVE LN-REPODATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F17.

           MOVE LN-REDEEMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F171.

           MOVE LN-SOLDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F172.

           MOVE LN-APRATE TO F18.
           MOVE LN-EFFRATE TO F19.
           MOVE LN-SMPRATE TO F20.
           MOVE LN-OVRDRATE TO F21.
           MOVE LN-ENDORATE TO F22.
           MOVE LN-YLDRATE TO F23.
           MOVE LN-CONRATE TO F24.
           MOVE LN-JDRATE TO F25.
           MOVE LN-ACCRATE TO F26.
           MOVE LN-REDURATE TO F27.
           MOVE LN-SERVPER TO F28.
           MOVE LN-ORGBR TO WS-ORGBR.

      D    STOP MESS.
           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           MOVE EXT-CONAME-CONAME TO LNMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN2-SYSDATE.
           DISPLAY LNMAN2-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN2-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE LNMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN2-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************************
      *  C O P Y - L I B R A R Y - S E C T I O N S
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
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
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.

