       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN8.
      ******************************************************************
      *
      *   DIR :  MN
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 8
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * JTG 072095 ADDED LN-PRLNAMT & LN-PRPDUE-DAYS
      * GLF 082296 ADDED LNSTAT FIELD TO BOTTOM OF PAGE.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 082297 ADDED LOGIC FOR PAGE 10. REMOVED PRIOR LOAN INFO,
      *            ADDED LN-RECPROFILE(1..5) AND LN-CONPROFILE(1..5)
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      *  CS 033198 ADDED LN-FOLLOWUP-DATE LN-ORIGIN
      * MJD 072498 FIXED BUG, SETTING NEW FLAG TO "N" BEFORE GOING
      *            TO PAGE 9. RESULT: PAGE 9 DOES NOT WALK THROUGH
      *            FIRELDS ON NEW RECORD ENTRY.
      * BAH 081098 ADDED LN-YTDNOALDEL FOR MERCURY'S NEW EDIT ON ALLOWABLE
      *            DELINQUENCY, #193
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * GLF 020199 ADDED LN-LPO,LN-REP-CD,LN-PROCESS-CD FOR TFC, WILL BE USED
      *            AS RANGES FOR ACCESS OF AGED TRIAL BALANCE AND COLLECTION
      *            REPORTS
      * BAH 011210 ADDED LN-PROMID FOR LAST 3 CHAR OF LOGIN WHO SET THE
      *            PROMISE TO PAY, REGACC PR# 1662
      * BLV 011211 ADDED LN-FOLLOWUP-CD, DEVELOPMENT PR# 1487
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99   LENDMARK PR#J021
      * BAH 020218 ADDED LN-LAST-PYPA-DATE FOR REGACC PR# 1721
      * BAH 020502 ADDED LN-PROMDAYS-PASTDUE FOR "DAYS PAST DUE AT TIME
      *            PROMISE TAKEN", REGACC PR# 1783
      * BLF 051011 ADDED LN-ACTIONCD2 (FIELD2 PART 2), LN-ATCODE & LN-RECODE,
      *            FIELD 34, OLD FIELDS NEVER IN MAINTENANCE.
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * MJD 121025 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *  CS 160617 ADDED MOVE OF EXT-FILEPATH-BRNO PRIOR TO READ
      *  CS 170425 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
      *
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 02.10.2020 ONLY ALLOW VALID CODES FOR ORIGIN #1421
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/LNMAN8.CBL S34 08/16/21-11:21:27 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

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
       01  FIRST-TIME        PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  LNMAIN-LINK.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(8).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).
       01  WS-ORGBR      PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC XXX.
           03  F02        PIC XXXX.
           03  F021       PIC XXXX.
           03  F03        PIC 99/99/99 BLANK ZERO.
           03  F031       PIC X(3).
           03  F04        PIC Z(6).ZZ.
           03  F041       PIC ZZZ.
           03  F05        PIC 99/99/99 BLANK ZERO.
           03  F06        PIC 99/99/99 BLANK ZERO.
           03  F07        PIC Z(7).ZZ.
           03  F08        PIC 99/99/99 BLANK ZERO.
           03  F09        PIC ZZZ.
           03  F091       PIC 99/99/99 BLANK ZERO.
           03  F10        PIC ZZ.
           03  F11        PIC ZZ.
           03  F12        PIC ZZ.
           03  F121       PIC ZZ.
           03  F13        PIC 99/99/99 BLANK ZERO.
           03  F131       PIC X(4).
           03  F14        PIC X(4).
           03  F15        PIC X(30).
           03  F16        PIC Z(6).
           03  F17        PIC ZZZ-.
           03  F18        PIC ZZZ.
           03  F19        PIC 99/99/99 BLANK ZERO.
           03  F20        PIC ZZ.
           03  F21        PIC 9(12).
           03  F21-ALP REDEFINES F21
                          PIC X(12).
           03  F22        PIC 9(12).
           03  F22-ALP REDEFINES F22
                          PIC X(12).
           03  F23        PIC 9(12).
           03  F23-ALP REDEFINES F23
                          PIC X(12).
           03  F24        PIC 9(12).
           03  F24-ALP REDEFINES F24
                          PIC X(12).
           03  F25        PIC 9(12).
           03  F25-ALP REDEFINES F25
                          PIC X(12).
           03  F26        PIC 9(12).
           03  F26-ALP REDEFINES F26
                          PIC X(12).
           03  F27        PIC 9(12).
           03  F27-ALP REDEFINES F27
                          PIC X(12).
           03  F28        PIC 9(12).
           03  F28-ALP REDEFINES F28
                          PIC X(12).
           03  F29        PIC 9(12).
           03  F29-ALP REDEFINES F29
                          PIC X(12).
           03  F30        PIC 9(12).
           03  F30-ALP REDEFINES F30
                          PIC X(12).
           03  F31        PIC X(4).
           03  F32        PIC X(8).
           03  F33        PIC X.
           03  F34        PIC X(3).
           03  F341       PIC X(3).
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(32) VALUE
               "010 020 021 030 031 040 041 050 ".
           03  FILLER     PIC X(24) VALUE
               "060 070 080 090 091 100 ".
           03  FILLER     PIC X(28) VALUE
               "110 120 121 130 131 140 150 ".
           03  FILLER     PIC X(20) VALUE
               "160 170 180 190 200 ".
           03  FILLER     PIC X(20) VALUE
               "210 220 230 240 250 ".
           03  FILLER     PIC X(20) VALUE
               "260 270 280 290 300 ".
           03  FILLER     PIC X(27) VALUE
               "310 320 330 340 341 LNSTAT.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       78  MAX                     VALUE 34.
       01  SPEC-TABLE.
      * COLLECTION INFORMATION:
      * 1. COLLECTOR ID
           03  FILLER     PIC X(8) VALUE "EN A030N".
      * 2. ACTION CODE
           03  FILLER     PIC X(8) VALUE "EN A040N".
      * 3. PROMISED DATE/ID
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 4. PROMISED AMT
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 5. PROMISED DATE 2
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 6. LAST PAYMENT DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 7. LAST PAYMENT AMOUNT
           03  FILLER     PIC X(8) VALUE "ENNN072N".
      * 8. LAST PY/PA DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 9. LAST COLC FORM SET
           03  FILLER     PIC X(8) VALUE "ENNN030N".
      * 10. DEL EXTRACT COUNTER
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "EN A040N".

      * 15. BEEP MESSAG
           03  FILLER     PIC X(8) VALUE "EN A300N".

      * SOLICITATION INFORMATION:
           03  FILLER     PIC X(8) VALUE "ENNN060N".
           03  FILLER     PIC X(8) VALUE "ENNS030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".

      * PROFILES
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".

           03  FILLER     PIC X(8) VALUE "EN A040N".
           03  FILLER     PIC X(8) VALUE "EN A080N".
           03  FILLER     PIC X(8) VALUE "EN A010N".

      * 34. ACTIVITY CODE
           03  FILLER     PIC X(8) VALUE "EN A030N".
      * 341. RESULT CODE
           03  FILLER     PIC X(8) VALUE "EN A030N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN8_DEF.CPY".
       COPY "LIBMN/LNMAN8_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMAN8_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN8" TO VDU-FORM-NAME.
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
              MOVE 9 TO FORM-SEL1
              MOVE " " TO SAVEKEY.
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

      ****************************************
       REC-LN-MODULE SECTION.
      ****************************************
       REC-LN-MODULE-NO.
           MOVE 0 TO LN-ACCTNO LN-OWNBR  FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LN-ACCTNO
              MOVE " " TO SAVEKEY
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

      ****************************************
       ENTER-SCANKEY SECTION.
      ****************************************
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
           MOVE VDUNUM0       TO LN-ACCTNO.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.

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
              IF FORM-SEL1 < 1 OR FORM-SEL1 > 9
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
                 A-26 A-27 A-28 A-29 A-30
                 A-31 A-32 A-33 A-34
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUBUF TO LN-COLLECTOR.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO LN-ACTIONCD.
       A-021.
           MOVE "E  A040021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-021.
           MOVE VDUBUF TO LN-ACTIONCD2.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDU-DATE-YYYYMMDD TO LN-PROMDATE.
       A-031.
           MOVE "E  A030031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-031.
           MOVE VDUBUF TO LN-PROMID.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO LN-PROMAMT.
       A-041.
           MOVE "EN N030041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-041.
           MOVE VDUNUM0 TO LN-PROMDAYS-PASTDUE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDU-DATE-YYYYMMDD TO LN-PROMDATE2.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD TO LN-DATE-PAID-LAST.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM2 TO LN-AMT-PAID-LAST.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDU-DATE-YYYYMMDD TO LN-LAST-PYPA-DATE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM0 TO LN-LASTCFORMNO.

       A-091.
           MOVE "ERNM060091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-091.
           MOVE VDU-DATE-YYYYMMDD TO LN-LASTCFORMDATE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM0 TO LN-DELCNT.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM0 TO LN-DELCNT2.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM0 TO LN-TOTNOALDEL.
       A-121.
           MOVE "ENNN020121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-121.
           MOVE VDUNUM0 TO LN-YTDNOALDEL.
           GO TO CREATE-ROUTINE.

       A-13.
           MOVE VDU-DATE-YYYYMMDD TO LN-FOLLOWUP-DATE.
       A-131.
           MOVE "E  A040131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-131.
           IF VDUBUF NOT = SPACES
              MOVE "FU"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM GET-CD
              IF IO-FG NOT = 0
                 MOVE "INVALID FOLLOWUP CD; ENTER VALID OR SPACES"
                    TO MESS
              PERFORM SEND-MESS
              GO TO A-131.
           MOVE VDUBUF TO LN-FOLLOWUP-CD.
           GO TO CREATE-ROUTINE.

       A-14.
           MOVE VDUBUF TO LN-ORIGIN CD-CODE.
           IF LN-ORIGIN NOT = SPACES
              MOVE "OR"   TO CD-TYPE
              PERFORM GET-CD
              IF IO-FG NOT = 0
                 MOVE "INVALID CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-15.
           MOVE VDUBUF TO LN-MESSAGE.
           GO TO CREATE-ROUTINE.

       A-16.
           MOVE VDUNUM0 TO LN-CREDITLIM.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM0 TO LN-CRSCORE.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM0 TO LN-LASTSFORMNO.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDU-DATE-YYYYMMDD TO LN-LASTSFORMDATE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM0 TO LN-SOLCNT.
           GO TO CREATE-ROUTINE.

       A-21.
           MOVE VDUNUM0 TO LN-CONPROFILE(1) F21.
           INSPECT F21-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120210." TO VDU.
           MOVE F21-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM0 TO LN-CONPROFILE(2) F22.
           INSPECT F22-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120220." TO VDU.
           MOVE F22-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM0 TO LN-CONPROFILE(3) F23.
           INSPECT F23-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120230." TO VDU.
           MOVE F23-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM0 TO LN-CONPROFILE(4) F24.
           INSPECT F24-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120240." TO VDU.
           MOVE F24-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUNUM0 TO LN-CONPROFILE(5) F25.
           INSPECT F25-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120250." TO VDU.
           MOVE F25-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUNUM0 TO LN-RECPROFILE(1) F26.
           INSPECT F26-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120260." TO VDU.
           MOVE F26-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-27.
           MOVE VDUNUM0 TO LN-RECPROFILE(2) F27.
           INSPECT F27-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120270." TO VDU.
           MOVE F27-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUNUM0 TO LN-RECPROFILE(3) F28.
           INSPECT F28-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120280." TO VDU.
           MOVE F28-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUNUM0 TO LN-RECPROFILE(4) F29.
           INSPECT F29-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120290." TO VDU.
           MOVE F29-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-30.
           MOVE VDUNUM0 TO LN-RECPROFILE(5) F30.
           INSPECT F30-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120300." TO VDU.
           MOVE F30-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-31.
           MOVE VDUBUF TO LN-LPO.
           GO TO CREATE-ROUTINE.
       A-32.
           MOVE VDUBUF TO LN-REP-CD.
           GO TO CREATE-ROUTINE.
       A-33.
           MOVE VDUBUF TO LN-PROCESS-CD.
           GO TO CREATE-ROUTINE.
       A-34.
           IF VDUBUF NOT = SPACES
              MOVE "AT"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM GET-CD
              IF IO-FG NOT = 0
                 MOVE "INVALID ACTIVITY CD; ENTER VALID OR SPACES"
                    TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-ATCODE.
       A-341.
           MOVE "E  A030341." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-341.
           IF VDUBUF NOT = SPACES
              MOVE "RE"   TO CD-TYPE
              MOVE VDUBUF TO CD-CODE
              PERFORM GET-CD
              IF IO-FG NOT = 0
                 MOVE "INVALID RESULT CD; ENTER VALID OR SPACES"
                    TO MESS
              PERFORM SEND-MESS
              GO TO A-341.
           MOVE VDUBUF TO LN-RECODE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ****************************************
       WRITE-MODULE SECTION.
      ****************************************
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

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      *********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-COLLECTOR NOT = LNCH-COLLECTOR
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "COLLECT "              TO WORK-ERRLOG-DESC
             MOVE LNOR-COLLECTOR          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-COLLECTOR          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ACTIONCD NOT = LNCH-ACTIONCD
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "ACTIONCD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ACTIONCD           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ACTIONCD           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ACTIONCD2 NOT = LNCH-ACTIONCD2
             MOVE "021"                   TO WORK-ERRLOG-FIELD
             MOVE "ACTION 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ACTIONCD2          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ACTIONCD2          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROMDATE NOT = LNCH-PROMDATE
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROMDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROMDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PROMDATE           TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROMID NOT = LNCH-PROMID
             MOVE "031"                   TO WORK-ERRLOG-FIELD
             MOVE "PROMID  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROMID             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PROMID             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROMAMT NOT = LNCH-PROMAMT
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROMAMT "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROMAMT            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PROMAMT            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROMDAYS-PASTDUE NOT = LNCH-PROMDAYS-PASTDUE
             MOVE "041"                   TO WORK-ERRLOG-FIELD
             MOVE "PAST DUE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROMDAYS-PASTDUE   TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-PROMDAYS-PASTDUE   TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROMDATE2 NOT = LNCH-PROMDATE2
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRODATE2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROMDATE2          TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-PROMDATE2          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DATE-PAID-LAST NOT = LNCH-DATE-PAID-LAST
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "PAIDLAST"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DATE-PAID-LAST     TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-DATE-PAID-LAST     TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-AMT-PAID-LAST NOT = LNCH-AMT-PAID-LAST
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "LAST AMT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-AMT-PAID-LAST      TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-AMT-PAID-LAST      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LAST-PYPA-DATE NOT = LNCH-LAST-PYPA-DATE
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "LASTPYPA"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LAST-PYPA-DATE     TO DATE-YYYYMMDD           
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LAST-PYPA-DATE     TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LASTCFORMNO NOT = LNCH-LASTCFORMNO
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "CFORM NO"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LASTCFORMNO        TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LASTCFORMNO        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LASTCFORMDATE NOT = LNCH-LASTCFORMDATE
             MOVE "091"                   TO WORK-ERRLOG-FIELD
             MOVE "CFORMDAY"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LASTCFORMDATE      TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LASTCFORMDATE      TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DELCNT NOT = LNCH-DELCNT
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "DELCNT  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-DELCNT             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-DELCNT             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DELCNT2 NOT = LNCH-DELCNT2
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "DELCNT 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DELCNT2            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-DELCNT2            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTNOALDEL NOT = LNCH-TOTNOALDEL
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTNO   "              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTNOALDEL         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTNOALDEL         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-YTDNOALDEL NOT = LNCH-YTDNOALDEL
             MOVE "121"                   TO WORK-ERRLOG-FIELD
             MOVE "YTDNO   "              TO WORK-ERRLOG-DESC
             MOVE LNOR-YTDNOALDEL         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-YTDNOALDEL         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FOLLOWUP-DATE NOT = LNCH-FOLLOWUP-DATE
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "FLUPDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FOLLOWUP-DATE      TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FOLLOWUP-DATE      TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FOLLOWUP-CD NOT = LNCH-FOLLOWUP-CD
             MOVE "131"                   TO WORK-ERRLOG-FIELD
             MOVE "FOLLUPCD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FOLLOWUP-CD        TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FOLLOWUP-CD        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ORIGIN NOT = LNCH-ORIGIN
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "ORIGIN  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-ORIGIN             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ORIGIN             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MESSAGE NOT = LNCH-MESSAGE
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "MESSAGE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MESSAGE            TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-MESSAGE            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CREDITLIM NOT = LNCH-CREDITLIM
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "CRED LIM"              TO WORK-ERRLOG-DESC
             MOVE LNOR-CREDITLIM          TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CREDITLIM          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CRSCORE NOT = LNCH-CRSCORE
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "CRSCORE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CRSCORE            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CRSCORE            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LASTSFORMNO NOT = LNCH-LASTSFORMNO
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "SFORMNO "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LASTSFORMNO        TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LASTSFORMNO        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LASTSFORMDATE NOT = LNCH-LASTSFORMDATE
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "FORMDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LASTSFORMDATE      TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LASTSFORMDATE      TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SOLCNT NOT = LNCH-SOLCNT
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "SOLCNT  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SOLCNT             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SOLCNT             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CONPROFILE(1) NOT = LNCH-CONPROFILE(1)
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "CPROF 1 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CONPROFILE(1)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CONPROFILE(1)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-CONPROFILE(2) NOT = LNCH-CONPROFILE(2)
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "CPROF 2 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CONPROFILE(2)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CONPROFILE(2)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-CONPROFILE(3) NOT = LNCH-CONPROFILE(3)
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "CPROF 3 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CONPROFILE(3)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CONPROFILE(3)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-CONPROFILE(4) NOT = LNCH-CONPROFILE(4)
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "CPROF 4 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CONPROFILE(4)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CONPROFILE(4)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-CONPROFILE(5) NOT = LNCH-CONPROFILE(5)
             MOVE "25 "                   TO WORK-ERRLOG-FIELD
             MOVE "CPROF 5 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CONPROFILE(5)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CONPROFILE(5)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-RECPROFILE(1) NOT = LNCH-RECPROFILE(1)
             MOVE "211"                   TO WORK-ERRLOG-FIELD
             MOVE "RPROF 1 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECPROFILE(1)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-RECPROFILE(1)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-RECPROFILE(2) NOT = LNCH-RECPROFILE(2)
             MOVE "221"                   TO WORK-ERRLOG-FIELD
             MOVE "RPROF 2 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECPROFILE(2)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-RECPROFILE(2)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-RECPROFILE(3) NOT = LNCH-RECPROFILE(3)
             MOVE "231"                   TO WORK-ERRLOG-FIELD
             MOVE "RPROF 3 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECPROFILE(3)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-RECPROFILE(3)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-RECPROFILE(4) NOT = LNCH-RECPROFILE(4)
             MOVE "241"                   TO WORK-ERRLOG-FIELD
             MOVE "RPROF 4 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECPROFILE(4)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-RECPROFILE(4)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-RECPROFILE(5) NOT = LNCH-RECPROFILE(5)
             MOVE "251"                   TO WORK-ERRLOG-FIELD
             MOVE "RPROF 5 "              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECPROFILE(5)      TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-RECPROFILE(5)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-LPO NOT = LNCH-LPO
             MOVE "31 "                   TO WORK-ERRLOG-FIELD
             MOVE "LPO     "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LPO                TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LPO                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-REP-CD NOT = LNCH-REP-CD
             MOVE "32 "                   TO WORK-ERRLOG-FIELD
             MOVE "REP-CD  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-REP-CD                TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-REP-CD                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-PROCESS-CD NOT = LNCH-PROCESS-CD
             MOVE "33 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROCESS "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROCESS-CD                TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PROCESS-CD                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ATCODE NOT = LNCH-ATCODE
             MOVE "34 "                   TO WORK-ERRLOG-FIELD
             MOVE "ACTIVITY"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ATCODE                TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ATCODE                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-RECODE NOT = LNCH-RECODE
             MOVE "341"                   TO WORK-ERRLOG-FIELD
             MOVE "RESULT  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECODE                TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-RECODE                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      *********************************************
       LOG-FIELD-CHANGE SECTION.
      *********************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *********************************************
       DISPLAY-FIELDS SECTION.
      *********************************************
           MOVE LN-COLLECTOR TO F01.
           MOVE LN-ACTIONCD TO F02.
           MOVE LN-ACTIONCD2 TO F021.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.
           MOVE LN-PROMID TO F031.
           MOVE LN-PROMAMT TO F04.
           MOVE LN-PROMDAYS-PASTDUE TO F041.
           MOVE LN-PROMDATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F05.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F06.
           MOVE LN-AMT-PAID-LAST TO F07.
           MOVE LN-LAST-PYPA-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F08.
           MOVE LN-LASTCFORMNO TO F09.
           MOVE LN-LASTCFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F091.
           MOVE LN-DELCNT TO F10.
           MOVE LN-DELCNT2 TO F11.
           MOVE LN-TOTNOALDEL TO F12.
           MOVE LN-YTDNOALDEL TO F121.
           MOVE LN-FOLLOWUP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F13.
           MOVE LN-FOLLOWUP-CD TO F131.
           MOVE LN-ORIGIN TO F14.
           MOVE LN-MESSAGE TO F15.
           MOVE LN-CREDITLIM TO F16.
           MOVE LN-CRSCORE TO F17.
           MOVE LN-LASTSFORMNO TO F18.
           MOVE LN-LASTSFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F19.
           MOVE LN-SOLCNT TO F20.
           MOVE LN-ORGBR TO WS-ORGBR.

           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE LN-CONPROFILE(1) TO F21.
           INSPECT F21-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-CONPROFILE(2) TO F22.
           INSPECT F22-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-CONPROFILE(3) TO F23.
           INSPECT F23-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-CONPROFILE(4) TO F24.
           INSPECT F24-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-CONPROFILE(5) TO F25.
           INSPECT F25-ALP REPLACING ALL '9' BY '-'.

           MOVE LN-RECPROFILE(1) TO F26.
           INSPECT F26-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(2) TO F27.
           INSPECT F27-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(3) TO F28.
           INSPECT F28-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(4) TO F29.
           INSPECT F29-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(5) TO F30.
           INSPECT F30-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-LPO TO F31.
           MOVE LN-REP-CD TO F32.
           MOVE LN-PROCESS-CD TO F33.
           MOVE LN-ATCODE TO F34.
           MOVE LN-RECODE TO F341.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE EXT-CONAME-CONAME TO LNMAN8-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN8-SYSDATE.
           DISPLAY LNMAN8-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN8-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN8-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE LNMAN8-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN8-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN8_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
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

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **********************************************
       END-ROUTINE SECTION.
      **********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN8-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

