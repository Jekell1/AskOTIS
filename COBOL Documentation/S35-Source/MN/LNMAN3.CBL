       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN3.
      ******************************************************************
      *
      *   DIR :  MN
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 3
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * GLF 082196 ADDED LNSTAT TO BOTTOM OF PAGE.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * JTG 082097 ADDED LN-PROC-DISBURSED, A90 WORLD
      * MJD 082197 CHANGED RECORD SIZE FROM 1500 TO 1570
      *            ADD LOGIC FOR PAGE 10.
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * BAH 062498 CHANGED LN-TILLNO TO ALPHANUMERIC
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 101598 ALLOWED ZERO PAYMENT SCHEDULE FOR REVOLVING ACCOUNT
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * BAH 990908 REMOVED IF BR-LC-CODE = "Y" TO DISPLAY LN-LC-CODE,
      *            YOU DIDNT KNOW IF SOMETHING WAS REALLY IN THERE...
      * JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI PAACO PR#1467
      * BAH 020712 SIGNED LN-TRADEIN, REGACC PR# 1808
      * CS  021121 ADDED DISPLAY OF INSURANCE TAX CODES,LXX-INS-TAX-COUNTY-CD,
      *            LXX-INS-TAX-CITY-CD. PER JTG DO NOT ALLOW ALTER OF THE TAX
      *            CODES [WORLD PR#288]
      *  AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *             /USR/DATA/R1/LG/CHG.YYMMDD
      *  MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      *  MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *
      * KEC 2016.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      *  CS 160616 REMOVED LN-SEQNO
      *  CS 160620 UNCOMMENTED IF LN-REVOLVING = "Y"GO TO SET-TEST-NOREGPY-EXIT.
      *  CS 160711 ADD LN-OWNBR PRIOR TO READ OF LN
      *  CS 161109 CHANGED LNMAN3_SCN FOR INS & FEES TO INCLUDE 2 &&
      * KEC 2017.0526 REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *               LNOR-REC AND LNCH-REC.
      *  CS 171121 CHANGE THE ENTRY OF LN-TILLNO TO BE FOR NUMERIC, ALSO
      *            CHANGED THE FORM TO HOLD 2 DIGITS
      * BAH 181212 GLOBAL LNFILE
      * BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 190130 SPLIT CDFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 2024.0131 CALL CRNOR2 AND WRITE NOTICE RECORD
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LNMAN3.CBL S35 02/21/24-12:58:21 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".

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
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  NOREG-FG          PIC X      VALUE " ".
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
       01  TOTAL-FEE-INS-FINCHG   PIC S9(7)V99.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-TIME        PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  AMT-HOLDER        PIC 9(7)V99 COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  LNMAIN-LINK  VALUE SPACES.
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
           03  F01        PIC Z(6).ZZ.
           03  F02        PIC Z(6).ZZ.
           03  F03        PIC Z(6).ZZ-.
           03  F04        PIC Z(6).ZZ.
           03  F05        PIC ZZZ.
           03  F06        PIC Z(6).ZZ.
           03  F07        PIC Z(6).ZZ.
           03  F071       PIC ZZ9.
           03  F08        PIC Z(6).ZZ.
           03  F09        PIC Z(7).ZZ-.
           03  F10        PIC Z(7).ZZ-.
           03  F101       PIC 9(2).
           03  F11        PIC X.
           03  F12        PIC X.
           03  F13        PIC ZZ9.
           03  FUNITPER-BUF.
               05  F131   PIC ZZ9.999999999999.
               05  F132   PIC ZZ9.
           03  F133       PIC X(30).
           03  F14        PIC Z(6).ZZ.
           03  F15        PIC Z(6).ZZ.
           03  F16        PIC Z(4).ZZ.
           03  F17        PIC Z(5).ZZ.
           03  F18        PIC ZZZ.ZZ.
           03  F181       PIC Z(6).ZZ.
           03  F19        PIC ZZ.ZZ.
           03  F20        PIC ZZZ-.
           03  F21        PIC X.
           03  F22        PIC Z(7).ZZ-.
           03  F23        PIC Z(7).ZZ-.
           03  F24        PIC ZZ.
           03  F25        PIC XXX.
           03  F251       PIC XXX.
           03  FLNSTAT    PIC X(30).
       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(32) VALUE
               "060 070 071 080 090 100 101 110 ".
           03  FILLER     PIC X(32) VALUE
               "120 130 131 132 133 140 150 160 ".
           03  FILLER     PIC X(51) VALUE
               "170 180 181 190 200 210 220 230 240 250 251 LNSTAT.".
       01  UNITPER-LIST   PIC X(8)  VALUE "131 132.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 25.
       01  SPEC-TABLE.
      *01.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *02.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *03 TRADEIN.
           03  FILLER     PIC X(8) VALUE "ENNS062N".
      *04.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *05.
           03  FILLER     PIC X(8) VALUE "ENNN030N".
      *06.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *07.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *08.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *09.
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      *10.
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      *11. LN-PROC-DISBURSED
           03  FILLER     PIC X(8) VALUE "E  A010N".
      *12.
           03  FILLER     PIC X(8) VALUE "E  A010N".
      *13.
           03  FILLER     PIC X(8) VALUE "ENNN030N".
      *14.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *15.
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      *16.
           03  FILLER     PIC X(8) VALUE "ENNN042N".
      *17.
           03  FILLER     PIC X(8) VALUE "ENNN052N".
      *18.
           03  FILLER     PIC X(8) VALUE "ENNN032N".
      *19.
           03  FILLER     PIC X(8) VALUE "ENNN022N".
      *20.
           03  FILLER     PIC X(8) VALUE "ENNN020N".
      *21.
           03  FILLER     PIC X(8) VALUE "E  A010N".
      *22.
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      *23.
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      *24.
           03  FILLER     PIC X(8) VALUE "ENNN020N".
      *25
           03  FILLER     PIC X(8) VALUE "E NA030N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 25 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN3_DEF.CPY".
       COPY "LIBMN/LNMAN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/UPERTSTW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBWI/ELSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMAN3_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE EXT-CONAME-USER-LOC TO BR-NO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE 1 TO BR-NO
              PERFORM READ-BR-FILE
              IF IO-FG = 9
                 MOVE "BRANCH RECORD MISSING" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.
           PERFORM CLOSE-BR-FILE.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN3" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.
           PERFORM FORM-RESET.
           MOVE " " TO VDU-PGMENU-SELECTION.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

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
              MOVE 4 TO FORM-SEL1.
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

      ************************************************
       REC-LN-MODULE SECTION.
      ************************************************
       REC-LN-MODULE-NO.
           MOVE 0 TO LN-ACCTNO FIRST-SCAN.
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
           EXIT.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
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
           MOVE VDUNUM0          TO LN-ACCTNO.
           MOVE EXT-FILPATH-BRNO TO LN-OWNBR.

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
           IF LN-SALEFIN = "Y" AND ELE = 9
              GO TO CREATE-ROUTINE.
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
              PERFORM SET-TEST-NOREGPY
              IF NOREG-FG = " "
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 GO TO MODIFY-ROUTINE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 3 OR FORM-SEL1 > 9
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
           PERFORM SET-TEST-NOREGPY.
           IF NOREG-FG = " "
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE
           ELSE
              GO TO MODIFY-ROUTINE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           IF (CHNO = 9 AND LN-SALEFIN = "Y")
               GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF ELE = 11 OR 20 OR 25
              GO TO CREATE-ROUTINE.
           IF ELE = 22
              IF BR-LC-CODE NOT = "Y"
                 GO TO CREATE-ROUTINE.
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
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF ELE = 24
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
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
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM2 TO LN-SELLPR.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO LN-DOWNPY.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO LN-TRADEIN.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO LN-LNAMT.
           IF LN-LNAMT = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           IF VDUNUM0 < LN-ANTICTERM
              MOVE " LESS THAN ANTICERN(1) TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO LN-ORGTERM.
           IF NEW-FLAG NOT = 1
              PERFORM ALARM
              PERFORM SET-TEST-NOREGPY.
           GO TO CREATE-ROUTINE.
       A-06.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO LN-1STPYAMT.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM2 TO LN-REGPYAMT.
           IF NEW-FLAG NOT = 1
              PERFORM ALARM
              PERFORM SET-TEST-NOREGPY.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM2 TO LN-LASTPYAMT.
           PERFORM ALARM.
           PERFORM SET-TEST-NOREGPY.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM2 TO LN-PROCEEDS.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO LN-PROCASH.
       A-101.
           MOVE "ENNN010101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-101.
           MOVE VDUNUM0 TO LN-TILLNO.
      *    IF LN-TILLNO = SPACES
      *       GO TO A-101.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO LN-PROC-DISBURSED.
           IF NOT (LN-PROC-DISBURSED = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO LN-UNITPER-CD
                          UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-CODE.
           IF UPERTST-ERRCD NOT = " "
              GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              MOVE 13 TO ELE
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM0 TO UPERTST-UNITPER-FREQ LN-UNITPER-FREQ.
           MOVE LN-UNITPER-CD TO UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-FREQUENCY.
           IF UPERTST-ERRCD NOT = " "
              GO TO ENTER-ELE.

           MOVE LN-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE LN-INTDATE TO UPER-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO UPER-1STPYDATE.
           MOVE LN-ORGTERM TO UPER-TERM.
           PERFORM UPER-CALCULATION.
           IF UPER-ERRCD NOT = " "
              GO TO ENTER-ELE.

           MOVE UPER-UNITPER-PER-YEAR TO LN-UNITPER-PER-YEAR
                                         F131.
           MOVE UPER-DAYS-IN-UNITPER TO LN-DAYS-IN-UNITPER
                                        F132.
           MOVE FUNITPER-BUF TO WR-BUF.
           MOVE UNITPER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *    CALL XFER USING STAT UNITPER-LIST F131.

           PERFORM UPERTST-SET-MESS.
           MOVE "S  A300133." TO VDU.
           MOVE UPERTST-MESS TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO LN-FINCHG.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO LN-INTCHG.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM2 TO LN-EXTCHG.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM2 TO LN-SERCHG.
           GO TO CREATE-ROUTINE.
       A-18.
           DIVIDE VDUNUM2 BY LN-ORGTERM GIVING LN-MAINTFEE
                                          REMAINDER AMT-HOLDER.
           IF AMT-HOLDER NOT = 0
              MOVE "FEE MUST BE EVENLY DIVISABLE BY TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO LN-POINTS.
           GO TO CREATE-ROUTINE.
       A-20.
           GO TO CREATE-ROUTINE.
       A-21.
           IF VDUBUF NOT = "A"
                  AND NOT = " "
              MOVE "ENTER: 'A' OR SPACE ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-STATE-FLAG.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO LN-STATE-WORKER.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO LN-STATE-WORKER2.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM0 TO LN-LC-CODE.
           PERFORM VERIFY-EL-CODE.
           IF IO-FG NOT = 0 AND LN-LC-CODE NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-25.
      *AT PRESENT DON'T ALLOW ALTER OF THE TAX CODES (UPDATE LX-X) WRITTEN WHEN
      *LOAN WAS BOOKED
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1
                     FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       CODE-SCAN SECTION.
      ************************************************

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.
           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00") OR (ELSCAN-CODE = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNN020220." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ZEROES        TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
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

      ***********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-SELLPR NOT = LNCH-SELLPR
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "SELL PR "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SELLPR             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SELLPR             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DOWNPY NOT = LNCH-DOWNPY
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "DOWN PAY"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DOWNPY             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DOWNPY             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TRADEIN NOT = LNCH-TRADEIN
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRADE IN"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TRADEIN            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-TRADEIN            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LNAMT NOT = LNCH-LNAMT
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "LOAN AMT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LNAMT              TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-LNAMT              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ORGTERM NOT = LNCH-ORGTERM
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "ORG TERM"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ORGTERM            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-ORGTERM            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-1STPYAMT NOT = LNCH-1STPYAMT
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "1STPYAMT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-1STPYAMT           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-1STPYAMT           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-REGPYAMT NOT = LNCH-REGPYAMT
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "REGPYAMT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-REGPYAMT           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-REGPYAMT           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-NOREGPY NOT = LNCH-NOREGPY
             MOVE "071"                   TO WORK-ERRLOG-FIELD
             MOVE "NOREGPAY"              TO WORK-ERRLOG-DESC
             MOVE LNOR-NOREGPY            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-NOREGPY            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LASTPYAMT NOT = LNCH-LASTPYAMT
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "LASTPAY "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LASTPYAMT          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-LASTPYAMT          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROCEEDS NOT = LNCH-PROCEEDS
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROCEEDS"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROCEEDS           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PROCEEDS           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROCASH NOT = LNCH-PROCASH
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROCASH "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROCASH            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PROCASH            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TILLNO NOT = LNCH-TILLNO
             MOVE "101"                   TO WORK-ERRLOG-FIELD
             MOVE "TILLNO  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-TILLNO             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TILLNO             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROC-DISBURSED NOT = LNCH-PROC-DISBURSED
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROCDISB"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROC-DISBURSED     TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PROC-DISBURSED     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-UNITPER-CD NOT = LNCH-UNITPER-CD
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "UNIT CD "              TO WORK-ERRLOG-DESC
             MOVE LNOR-UNITPER-CD         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-UNITPER-CD         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-UNITPER-FREQ NOT = LNCH-UNITPER-FREQ
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "UNITFREQ"              TO WORK-ERRLOG-DESC
             MOVE LNOR-UNITPER-FREQ       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-UNITPER-FREQ       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-UNITPER-PER-YEAR NOT = LNCH-UNITPER-PER-YEAR
             MOVE "131"                   TO WORK-ERRLOG-FIELD
             MOVE "UNITPPYR"              TO WORK-ERRLOG-DESC
             MOVE LNOR-UNITPER-PER-YEAR   TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-UNITPER-PER-YEAR   TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DAYS-IN-UNITPER NOT = LNCH-DAYS-IN-UNITPER
             MOVE "132"                   TO WORK-ERRLOG-FIELD
             MOVE "DAY UNIT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DAYS-IN-UNITPER    TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-DAYS-IN-UNITPER    TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FINCHG NOT = LNCH-FINCHG
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "FINCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-FINCHG             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FINCHG             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTCHG NOT = LNCH-INTCHG
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-INTCHG             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INTCHG             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-EXTCHG NOT = LNCH-EXTCHG
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "EXTCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-EXTCHG             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-EXTCHG             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SERCHG NOT = LNCH-SERCHG
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "SERCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SERCHG             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SERCHG             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MAINTFEE NOT = LNCH-MAINTFEE
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "MAINTFEE  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MAINTFEE             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MAINTFEE             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-POINTS NOT = LNCH-POINTS
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "POINTS  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-POINTS             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-POINTS             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-STATE-FLAG NOT = LNCH-STATE-FLAG
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "ST FLAG "              TO WORK-ERRLOG-DESC
             MOVE LNOR-STATE-FLAG         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-STATE-FLAG         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-STATE-WORKER NOT = LNCH-STATE-WORKER
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "STWORKER"              TO WORK-ERRLOG-DESC
             MOVE LNOR-STATE-WORKER       TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-STATE-WORKER       TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-STATE-WORKER2 NOT = LNCH-STATE-WORKER2
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "ST WORK2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-STATE-WORKER2      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-STATE-WORKER2      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LC-CODE NOT = LNCH-LC-CODE
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "LC CODE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LC-CODE            TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LC-CODE            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      ***********************************************
       LOG-FIELD-CHANGE SECTION.
      ***********************************************
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

      ***********************************************
       DISPLAY-FIELDS SECTION.
      ***********************************************
           MOVE LN-SELLPR           TO F01.
           MOVE LN-DOWNPY           TO F02.
           MOVE LN-TRADEIN          TO F03.
           MOVE LN-LNAMT            TO F04.
           MOVE LN-ORGTERM          TO F05.
           MOVE LN-1STPYAMT         TO F06.
           MOVE LN-REGPYAMT         TO F07.
           MOVE LN-NOREGPY          TO F071.
           MOVE LN-LASTPYAMT        TO F08.
           MOVE LN-PROCEEDS         TO F09.
           MOVE LN-PROCASH          TO F10.
           MOVE LN-TILLNO           TO F101.
           MOVE LN-PROC-DISBURSED   TO F11.
           MOVE LN-UNITPER-CD       TO F12.
           MOVE LN-UNITPER-FREQ     TO F13.
           MOVE LN-UNITPER-PER-YEAR TO F131.
           MOVE LN-DAYS-IN-UNITPER  TO F132.
           MOVE LN-FINCHG TO F14.
           MOVE LN-INTCHG TO F15.
           MOVE LN-EXTCHG TO F16.
           MOVE LN-SERCHG TO F17.
           MULTIPLY LN-MAINTFEE BY LN-ORGTERM GIVING F18.
           MOVE 0 TO TOTAL-FEE-INS-FINCHG.
           PERFORM GET-FEE-INS-FINCHG
                VARYING SUB FROM 1 BY 1
                  UNTIL SUB > 10.
           MOVE TOTAL-FEE-INS-FINCHG TO F181.
           MOVE LN-POINTS TO F19.
           MOVE LN-STATE-FLAG TO F21.
           MOVE LN-STATE-WORKER TO F22.
           MOVE LN-STATE-WORKER2 TO F23.
           MOVE LN-LC-CODE TO F24.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              PERFORM GET-LXX-TAX
              MOVE LXX-INS-TAX-COUNTY-CD TO F25
              MOVE LXX-INS-TAX-CITY-CD   TO F251.

           MOVE LN-ORGBR TO WS-ORGBR.

           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE LN-UNITPER-CD TO UPERTST-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO UPERTST-UNITPER-FREQ.
           PERFORM UPERTST-SET-MESS.
           MOVE UPERTST-MESS TO F133.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ***********************************************
       GET-FEE-INS-FINCHG SECTION.
      ***********************************************
           IF SUB < 9
              IF LN-INSOURS(SUB) NOT = "M"
                 IF LN-INSFINCHG(SUB) = "Y"
                    ADD LN-INSPREM(SUB) TO TOTAL-FEE-INS-FINCHG.
           IF LN-FEEOURS(SUB) NOT = "M"
              IF LN-FEEFINCHG(SUB) = "Y"
                 ADD LN-FEEAMT(SUB) TO TOTAL-FEE-INS-FINCHG.

      ***********************************************
       SET-TEST-NOREGPY SECTION.
      ***********************************************
           MOVE " " TO NOREG-FG.

           SUBTRACT 1 FROM LN-ORGTERM GIVING LN-NOREGPY.
           IF LN-LASTPYAMT NOT = 0
              SUBTRACT 1 FROM LN-NOREGPY.
           MOVE "SNNN030071." TO VDU.
           MOVE LN-NOREGPY TO VDUNUM0.
           PERFORM SCREENIO.
           IF LN-ORGTERM = 1
              IF LN-LASTPYAMT NOT = 0
                 MOVE "E" TO NOREG-FG
                 MOVE "LAST PAYMENT MUST BE ZERO" TO MESS
                 PERFORM SEND-MESS.
           IF LN-ORGTERM > 1
              IF NOT (LN-1STPYAMT > 0
                          AND LN-REGPYAMT > 0
                            AND LN-LASTPYAMT > 0
                     )
                 MOVE "E" TO NOREG-FG
                 MOVE "FIRST, REGULAR AND LAST PAYMENTS CAN'T BE ZERO"
                                                               TO MESS
                 PERFORM SEND-MESS.

       SET-TEST-NOREGPY-EXIT.
           EXIT.

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO LNMAN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN3-SYSDATE.
           DISPLAY LNMAN3-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN3-FORM-FIELDS TO WR-BUF. 
           MOVE LNMAN3-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE LNMAN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN3-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/UPERTST.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************
       MISC-PARAGRAPHS SECTION.
      *******************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

       GET-LXX-TAX.
           PERFORM OPEN-LX1-FILE.
           MOVE LN-OWNBR  TO LX-BRNO.
           MOVE LN-ACCTNO TO LX-ACCTNO.
           MOVE 1         TO LX-SEQNO.
           PERFORM READ-LX1-FILE.
           IF IO-BAD
              MOVE SPACES TO LXX-INS-TAX-COUNTY-CD
              MOVE SPACES TO LXX-INS-TAX-CITY-CD.
           PERFORM CLOSE-LX1-FILE.
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
       VERIFY-EL-CODE.
           PERFORM OPEN-CD1-FILE.
           MOVE "EL"       TO CD-TYPE.
           MOVE LN-LC-CODE TO CD-EL-CODE.
           MOVE SPACES     TO CD-EL-FILLER.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

      **************************************************
       IO-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLX1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************************************
       END-ROUTINE SECTION.
      **************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN3-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
