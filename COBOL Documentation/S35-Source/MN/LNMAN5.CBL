       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN5.
      ******************************************************************
      *
      *   DIR :  MN
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 5
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * MJD 071296 MADE DISPLAY OF LN-LCPDTH-DATE 99/99/99.
      * JTG 071696 PUT AUDIT ON LP-PDTH-DATE & LP-LCPDTH, TO VERIFY
      *            DATES ARE ON A CYCLIC PAY FREQUENCY
      * GLF 082296 GLF ADDED LNSTAT TO BOTTOM OF PAGE
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 082197 CHANGED RECORD SIZE FROM 1500 TO 1570
      *            ADD LOGIC FOR PAGE 10.
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 093098 ALLOWED LN-CURBAL & LN-PURBAL TO GO NEGATIVE; REGENCY REVOL
      * JTG 113098 ADDED VERIFY THAT LN-OTHBAL = LN-OTHBAL-AMT() FOR REVOLVING
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * MJD 990810 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *            EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *            OTHERWISE EXCLUDE BUCKET 6.
      * MJD 991220 ADDED LN-TOTNODEFACT AND LN-YTDNODEFACT (WORLD PR#216)
      * MJD 000119 ADDED LN-TOTNOLCHG (WORLD PR#218)
      * MJD 000310 ADDED LN-PRIOR-STMT-COUNTER (MERC/NEWS PR#????)
      * JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * BAH 051025 RENAMED LN-LEDGERCNT TO FILLER
      * BAH 060221 MOVE SPACES TO F12 F13 SO 202 DOES NOT DISPLAY
      * BAH 061205 ADDED 12. COUPONS/STATEMENTS : FOR REGACC PR# 158
      * BAH 081029 ALLOW "E" FOR LN-COUPON-STATEMENT TO BE ELECTRONIC
      *                                                   REGACC PR# 224
      *  081117 CS ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *            OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM
      *            ANTICIPATED     REGACC PR#210
      * MJD 121025 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *  CS 160616 REMOVED LN-SEQNO
      *  CS 170425 ACTIVATE 8 DIGIT DATES
      * KEC 2017.0526 REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *               LNOR-REC AND LNCH-REC.
      * BAH 181212 GLOBAL LNFILE
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
           "@(#)MN/LNMAN5.CBL S35 02/21/24-13:00:41 >".
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
       01  ACCERN            PIC S9(6)V99 COMP.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-TIME        PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  CHAN          PIC X(4).
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
           03  F01        PIC Z(6).ZZ-.
           03  F02        PIC Z(5).ZZ-.
           03  F03        PIC Z(5).ZZ-.
           03  F04        PIC Z(5).ZZ-.
           03  F05        PIC Z(5).ZZ-.
           03  F06        PIC 99/99/99 BLANK ZERO.
           03  F07        PIC 99/99/99 BLANK ZERO.
           03  F08        PIC 99/99/99 BLANK ZERO.
           03  F09        PIC Z(5).ZZ-.
           03  F10        PIC Z(5).ZZ-.
           03  F11        PIC ZZZZ.
           03  F111       PIC ZZZZ.
           03  F12        PIC X.
           03  F13        PIC XXX.
           03  F14        PIC Z(6).ZZ.
           03  F15        PIC Z(6).ZZ.
           03  F16        PIC Z(7).ZZ-.
           03  F161       PIC Z(7).ZZ-.
           03  F17        PIC Z(7).ZZ-.
           03  F18        PIC Z(7).ZZ-.
           03  F19        PIC Z(6).ZZ-.
           03  F20        PIC Z(03).
           03  F201       PIC Z(6).ZZ-.
           03  F21        PIC Z(6).ZZ-.
           03  F22        PIC Z(6).ZZ-.
           03  F23        PIC Z(5).ZZ-.
           03  F24        PIC Z(5).ZZ-.
           03  F25        PIC Z(6).ZZ-.
           03  F26        PIC ZZ-.
           03  F261       PIC ZZ-.
           03  F27        PIC ZZ-.
           03  F271       PIC ZZ-.
           03  F28        PIC Z(5).ZZ-.
           03  F29        PIC Z(5).ZZ-.
           03  F30        PIC Z(5).ZZ-.
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(20) VALUE
               "060 070 080 090 100 ".
           03  FILLER     PIC X(24) VALUE
               "110 111 120 130 140 150 ".
           03  FILLER     PIC X(28) VALUE
               "160 161 170 180 190 200 201 ".
           03  FILLER     PIC X(20) VALUE
               "210 220 230 240 250 ".
           03  FILLER     PIC X(35) VALUE
               "260 261 270 271 280 290 300 LNSTAT.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       78  MAX                     VALUE 30.
       01  SPEC-TABLE.
      * 1.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      * 2.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 3.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 4. OTHER   BALANCE
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 5. OTHER 2 BALANCE
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 6. LATE CHARGE PDTH
           03 FILLER      PIC X(8) VALUE "ERNM060N".
      * 7.
           03 FILLER      PIC X(8) VALUE "ERNM060N".
      * 8.
           03 FILLER      PIC X(8) VALUE "ERNM060N".
      * 9.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *10.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *11.
           03 FILLER      PIC X(8) VALUE "ENNN040N".
      *12.  LN-COUPON-STATEMENT
           03 FILLER      PIC X(8) VALUE "ENNA010N".
      *13.
           03 FILLER      PIC X(8) VALUE "ENNN030N".
      *14.
           03 FILLER      PIC X(8) VALUE "ENNN062N".
      *15.
           03 FILLER      PIC X(8) VALUE "ENNN062N".
      *16.
           03 FILLER      PIC X(8) VALUE "ENNN072N".
      *17.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *18.
           03 FILLER      PIC X(8) VALUE "ENNS072N".
      *19.
           03 FILLER      PIC X(8) VALUE "ENNN072N".
      *20.  LN-TOTNOLCHG
           03 FILLER      PIC X(8) VALUE "ENNS030N".
      *21.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *22.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *23.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *24.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *25.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *26.
           03 FILLER      PIC X(8) VALUE "ENNS020N".
      *27.
           03 FILLER      PIC X(8) VALUE "ENNS020N".
      *28.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *29.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *30.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *31.
           03 FILLER      PIC X(8) VALUE "ENNS052N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN5_DEF.CPY".
       COPY "LIBMN/LNMAN5_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMAN5_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      *********************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN5" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.
           MOVE " " TO VDU-PGMENU-SELECTION.
           PERFORM FORM-RESET.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM GET-GPENV.

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
              MOVE 6 TO FORM-SEL1.
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

      ********************************************
       REC-LN-MODULE SECTION.
      ********************************************
       REC-LN-MODULE-NO.
           MOVE 0 TO LN-ACCTNO LN-OWNBR FIRST-SCAN.
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

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
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
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 8
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 5 OR FORMSEL > 9
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
           IF CHNO = 18
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF ELE = 13
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                GO TO MODIFY-ROUTINE.

           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = CHAN
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
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           IF VDUNUM2 < 0
              MOVE "BELOW ZERO ONLY ALLOWED FOR REVOLVING ACCOUNTS"
                                                            TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO LN-CURBAL.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO LN-INTBAL.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO LN-LCBAL.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO LN-OTHBAL.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO LN-OT2BAL.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE
                                     LN-LCPDTH-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO ENTER-ELE.
      *    IF LN-1STPYDATE = 0
      *       GO TO CREATE-ROUTINE.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE LN-LCPDTH-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER-REM NOT = 0
              MOVE "DATE IS NOT ON A PAY PERIOD CYCLE DATE!!!!"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDU-DATE-YYYYMMDD TO LN-INTPDTH-DATE.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDU-DATE-YYYYMMDD TO LN-1STPYDATE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM2 TO LN-LCPAID.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO LN-LCPARTIALS.
           GO TO CREATE-ROUTINE.
       A-11.
           IF VDUNUM0 NOT > 0
              MOVE "MUST BE > 0" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO LN-STMT-COUNTER.
       A-111.
           MOVE "ENNN040111." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-111.
           IF VDUNUM0 NOT > 0
              MOVE "COUNTER MUST BE GREATER THAN ZERO!" TO MESS
              PERFORM SEND-MESS
              GO TO A-111.
           IF VDUNUM0 > LN-STMT-COUNTER
              MOVE "PRIOR CANNOT EXCEED THE CURRENT COUNTER" TO MESS
              PERFORM SEND-MESS
              GO TO A-111.
           MOVE VDUNUM0 TO LN-PRIOR-STMT-COUNTER.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO LN-COUPON-STATEMENT.
           IF NOT (LN-COUPON-STATEMENT = " " OR "C" OR "S" OR "E")
              MOVE "ENTER ' ', 'C', 'S' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-13.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO LN-RECASTINT.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO LN-RECASTPRIN.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM2 TO LN-TOTPAYMNTD.
       A-161.
           MOVE "ENNS072161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-161.
           MOVE VDUNUM2 TO LN-TOTEXCPAYMNTD.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM2 TO LN-TOTERN.
           GO TO CREATE-ROUTINE.
       A-18.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO LN-TOTINT.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM0 TO LN-TOTNOLCHG.
       A-201.
           MOVE "ENNS052201." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-201.
           MOVE VDUNUM2 TO LN-TOTLCHG.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM2 TO LN-TOTDEF.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO LN-TOTSERACCT.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO LN-INTWAIVERS.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM2 TO LN-LCWAIVERS.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUNUM2 TO LN-TOTOTH.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUNUM0 TO LN-TOTNODEF.
       A-261.
           MOVE "ENNN030261." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-261.
           MOVE VDUNUM0 TO LN-TOTNODEFACT.
           GO TO CREATE-ROUTINE.
       A-27.
           MOVE VDUNUM0 TO LN-YTDNODEF.
       A-271.
           MOVE "ENNN030271." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-271.
           MOVE VDUNUM0 TO LN-YTDNODEFACT.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUNUM2 TO LN-TOTINTPD.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUNUM2 TO LN-YTDINTPD.
           GO TO CREATE-ROUTINE.
       A-30.
           MOVE VDUNUM2 TO LN-LYRINTPD.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ********************************************
       WRITE-MODULE SECTION.
      ********************************************
       REWRITE-RECORD.
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

      *********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-CURBAL NOT = LNCH-CURBAL
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "CURBAL  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CURBAL             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-CURBAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTBAL NOT = LNCH-INTBAL
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTBAL  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-INTBAL             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INTBAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LCBAL NOT = LNCH-LCBAL
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "LCBAL  "               TO WORK-ERRLOG-DESC
             MOVE LNOR-LCBAL              TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-LCBAL              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-OTHBAL NOT = LNCH-OTHBAL
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "OTHBAL  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-OTHBAL             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-OTHBAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-OT2BAL NOT = LNCH-OT2BAL
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "OT2BAL  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-OT2BAL             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-OT2BAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LCPDTH-DATE NOT = LNCH-LCPDTH-DATE
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "LCPDTH-DATE  "         TO WORK-ERRLOG-DESC
             MOVE LNOR-LCPDTH-DATE        TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LCPDTH-DATE        TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTPDTH-DATE NOT = LNCH-INTPDTH-DATE
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTPDTH-DATE "         TO WORK-ERRLOG-DESC
             MOVE LNOR-INTPDTH-DATE       TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INTPDTH-DATE       TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-1STPYDATE NOT = LNCH-1STPYDATE
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "1STPYDAT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-1STPYDATE          TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-1STPYDATE          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LCPAID NOT = LNCH-LCPAID
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "LCPAID  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LCPAID             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LCPAID             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LCPARTIALS NOT = LNCH-LCPARTIALS
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "PARTIALS"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LCPARTIALS         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LCPARTIALS         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-STMT-COUNTER NOT = LNCH-STMT-COUNTER
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "STMT CNT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-STMT-COUNTER       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-STMT-COUNTER       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRIOR-STMT-COUNTER NOT = LNCH-PRIOR-STMT-COUNTER
             MOVE "111"                   TO WORK-ERRLOG-FIELD
             MOVE "PSTMTCNT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRIOR-STMT-COUNTER TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PRIOR-STMT-COUNTER TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-COUPON-STATEMENT NOT = LNCH-COUPON-STATEMENT
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "COUP/STM"              TO WORK-ERRLOG-DESC
             MOVE LNOR-COUPON-STATEMENT   TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-COUPON-STATEMENT   TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-RECASTINT NOT = LNCH-RECASTINT
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "RECASTIN"              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECASTINT          TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-RECASTINT          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-RECASTPRIN NOT = LNCH-RECASTPRIN
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "RECASTPR"              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECASTPRIN         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-RECASTPRIN         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTPAYMNTD NOT = LNCH-TOTPAYMNTD
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTPAYMN"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTPAYMNTD         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTPAYMNTD         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-TOTEXCPAYMNTD NOT = LNCH-TOTEXCPAYMNTD
             MOVE "161"                   TO WORK-ERRLOG-FIELD
             MOVE "NON-CON PMTD"            TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTEXCPAYMNTD         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTEXCPAYMNTD         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTERN NOT = LNCH-TOTERN
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTERN  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTERN             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTERN             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTINT NOT = LNCH-TOTINT
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTINT  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTINT             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTINT             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTNOLCHG NOT = LNCH-TOTNOLCHG
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTNLCHG"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTNOLCHG          TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTNOLCHG          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTLCHG NOT = LNCH-TOTLCHG
             MOVE "201"                   TO WORK-ERRLOG-FIELD
             MOVE "TOTLCHG "              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTLCHG            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTLCHG            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTDEF NOT = LNCH-TOTDEF
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTDEF  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTDEF             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTDEF             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTSERACCT NOT = LNCH-TOTSERACCT
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOT ACCT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTSERACCT            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTSERACCT            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTWAIVERS NOT = LNCH-INTWAIVERS
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "INT WAIV"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INTWAIVERS         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INTWAIVERS         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LCWAIVERS NOT = LNCH-LCWAIVERS
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "LC WAIV "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LCWAIVERS          TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LCWAIVERS          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTOTH NOT = LNCH-TOTOTH
             MOVE "25 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOT OTH "              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTOTH             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTOTH             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTNODEF NOT = LNCH-TOTNODEF
             MOVE "26 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTNODEF"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTNODEF           TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTNODEF           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTNODEFACT NOT = LNCH-TOTNODEFACT
             MOVE "261"                   TO WORK-ERRLOG-FIELD
             MOVE "NODEFACT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTNODEFACT        TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTNODEFACT        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-YTDNODEF NOT = LNCH-YTDNODEF
             MOVE "27 "                   TO WORK-ERRLOG-FIELD
             MOVE "YTDNODEF"              TO WORK-ERRLOG-DESC
             MOVE LNOR-YTDNODEF           TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-YTDNODEF           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-YTDNODEFACT NOT = LNCH-YTDNODEFACT
             MOVE "271"                   TO WORK-ERRLOG-FIELD
             MOVE "YTDEFACT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-YTDNODEFACT        TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-YTDNODEFACT        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOTINTPD NOT = LNCH-TOTINTPD
             MOVE "28 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTINTPD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOTINTPD           TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TOTINTPD           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-YTDINTPD NOT = LNCH-YTDINTPD
             MOVE "29 "                   TO WORK-ERRLOG-FIELD
             MOVE "YTDINTPD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-YTDINTPD           TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-YTDINTPD           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LYRINTPD NOT = LNCH-LYRINTPD
             MOVE "30 "                   TO WORK-ERRLOG-FIELD
             MOVE "LYRINTPD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LYRINTPD           TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LYRINTPD           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

      *************************************
       LOG-FIELD-CHANGE SECTION.
      *************************************
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

      *************************************
       DISPLAY-FIELDS SECTION.
      *************************************
           MOVE LN-CURBAL TO F01.
           MOVE LN-INTBAL TO F02.
           MOVE LN-LCBAL TO F03.
           MOVE LN-OTHBAL TO F04.
           MOVE LN-OT2BAL TO F05.
           MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F06. 

           MOVE LN-INTPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F07.

           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F08.

           MOVE LN-LCPAID TO F09.
           MOVE LN-LCPARTIALS TO F10.
           MOVE LN-STMT-COUNTER TO F11.
           MOVE LN-PRIOR-STMT-COUNTER TO F111.
           MOVE LN-COUPON-STATEMENT TO F12.
           MOVE SPACES TO F13.
           MOVE LN-RECASTINT TO F14.
           MOVE LN-RECASTPRIN TO F15.
           MOVE LN-TOTPAYMNTD TO F16.
           MOVE LN-TOTEXCPAYMNTD TO F161.
           MOVE LN-TOTERN TO F17.
           COMPUTE ACCERN = LN-ACCERN(1) + LN-ACCERN(2)
                          + LN-ACCERN(3) + LN-ACCERN(5).

           IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                                     GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ACCERN(6) TO ACCERN.


           IF NOT(GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ACCERN(4) TO ACCERN.

           MOVE ACCERN TO F18.
           MOVE LN-TOTINT TO F19.
           MOVE LN-TOTNOLCHG TO F20.
           MOVE LN-TOTLCHG TO F201.
           MOVE LN-TOTDEF TO F21.
           MOVE LN-TOTSERACCT TO F22.
           MOVE LN-INTWAIVERS TO F23.
           MOVE LN-LCWAIVERS TO F24.
           MOVE LN-TOTOTH TO F25.
           MOVE LN-TOTNODEF TO F26.
           MOVE LN-TOTNODEFACT TO F261.
           MOVE LN-YTDNODEF TO F27.
           MOVE LN-YTDNODEFACT TO F271.
           MOVE LN-TOTINTPD TO F28.
           MOVE LN-YTDINTPD TO F29.
           MOVE LN-LYRINTPD TO F30.
           MOVE LN-ORGBR TO WS-ORGBR.

           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           MOVE EXT-CONAME-CONAME TO LNMAN5-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN5-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY LNMAN5-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN5-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN5-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE LNMAN5-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN5-HELP--DIR TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN5_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
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

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **********************************************
       END-ROUTINE SECTION.
      **********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN5-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

