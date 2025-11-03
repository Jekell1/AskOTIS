       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SSINQ.
       DATE-WRITTEN.    122010.
      ******************************************************************
      *                 (LP)
      *         CREDIT SCORE STATISTICS FILE INQUIRY
      *         WORLD PR# 726
      * REV:
      * BLM 130924 THERE WAS LEFT OVER LOGIC IN HERE FROM BYINQ TO GO TO
      *            PAGE 2 IF YOU TYPED P2, BUT THERE IS NO PAGE 2 TO IT
      *            BOMBED WITH MISSING FORM.  REMOVED CHECK ON "P2" &
      *            SWITCH-FORM PARAGRAPH
      *  CS 170713 ACTIVATE 8 DIGIT DATES
      *  CS 181016 GLOBAL SS SS-SELECT-PATH            
      * BAH 20190919 REMOVED ACCESS-CALL ON SSFILE
      **************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/SSINQ.CBL S35 02/23/24-10:15:42 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SS.CPY".
       COPY "LIBLP/LP01SS_SQL.CPY".
       COPY "LIBLP/LPWSSS.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  ERRCD           PIC X     VALUE SPACES.
       01  FIRST-READ      PIC X     VALUE "Y".
       01  IO-TYPE         PIC X(3)  VALUE " ".

       01  GROUP-FLAG      PIC X.
       01  ENT-GROUP       PIC X(4).
       01  ENT-GROUPX REDEFINES ENT-GROUP.
           03  ENT-GROUP-1ST-3 PIC X(3).
           03  FILLER      PIC X.
       01  SAVE-LEVEL      PIC 9.
       01  HOLD-GR-NAME PIC X(20).

       01  ALL-BRANCHES-FLAG        PIC X    VALUE "N".
           88  NOT-ALL-BRANCHES              VALUE "N".
           88  ALL-BRANCHES                  VALUE "Y".
       01  ALL-CLASSES-FLAG         PIC X    VALUE "N".
           88  ONE-CLASS                     VALUE "1".
           88  ALL-CLASSES                   VALUE "Y".
           88  SF-CLASSES                    VALUE "S".
           88  LN-CLASSES                    VALUE "L".
           88  WORLD-CLASS123                VALUE "A".
           88  WCBC-CLASSES                  VALUE "W".
           88  WORLD-OTHER-CLASSES           VALUE "O".
           88  WORLD-SF-CLASSES              VALUE "F".
           88  SMALL-LN-CLASSES              VALUE "M".
           88  NON-SMALL-LN-CLASSES          VALUE "R".

       01  YYES                     PIC X    VALUE "Y".
       01  NNO                      PIC X    VALUE "N".
       01  STORE-BRNO               PIC 9(4) VALUE 0.
       01  STORE-CLASS              PIC 9(3) VALUE 0.
       01  ENTER-CLASS-WK           PIC 99.
       01  ENTER-CLASS-ALPHA       REDEFINES ENTER-CLASS-WK.
           03  CLASS-WK1            PIC X.
           03  CLASS-WK2            PIC X.
       01  CORRECT-CLASS            PIC X    VALUE "N".
       01  STORE-DATE               PIC 9(4).
       01  STORE-DATEX              REDEFINES STORE-DATE.
           03  STORE-MO             PIC 9(2).
           03  STORE-YR             PIC 9(2).
       01  SAVE-SS-DATE             PIC 9(6).
       01  SAVE-SS-DATEX REDEFINES SAVE-SS-DATE.
           03  SAVE-SS-CC           PIC 99.
           03  SAVE-SS-YY           PIC 99.
           03  SAVE-SS-MM           PIC 99.
       01  PERCENT-WK               PIC S999V9  VALUE 0.
       01  PERCENT-WK2              PIC S999    VALUE 0.


       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".
      **************************************************
      *    SUMMARY-TYPES:
      *
      *    BR-CL        0 = ONE BRANCH   / ONE CLASS
      *
      *    BR-ALLCL     1 = ONE BRANCH   / ALL CLASSES
      *    ALLBR-CL     2 = ALL BRANCHES / ONE CLASS
      *    ALLBR-ALLCL  3 = ALL BRANCHES / ALL CLASSES
      *    RESOLVED     1 OR 2 OR 3
      **************************************************
       01  SUMMARY-TYPE        PIC 9 VALUE 0.
           88  BR-CL           VALUE 0.
           88  BR-ALLCL        VALUE 1.
           88  ALLBR-CL        VALUE 2.
           88  ALLBR-ALLCL     VALUE 3.
           88  RESOLVED        VALUE 1 2 3.

       01  READ-ONCE-FG    PIC X VALUE "N".
       01  DEL-SUB2        PIC 9(6) COMP VALUE 1.
       01  TYPE-SUB        PIC 9         VALUE 1.

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 1
      *----------------------------------------------------------------
       01  PAGE1-DEL-LEGEND.
           03  FILLER           PIC X(39) VALUE
               "F1-CLEAR, F3-SHIFT CURRENT/ORIG SCORE, ".
           03  FILLER           PIC X(27) VALUE
               "F6-SHIFT DELINQ, 'P'-P&L'S:".
       01  PAGE1-PL-LEGEND.
           03  FILLER           PIC X(39) VALUE
               "F1-CLEAR, F3-SHIFT CURRENT/ORIG SCORE, ".
           03  FILLER           PIC X(16) VALUE
               "'D'-DELINQUENCY:".
       01  LEGEND               PIC X(70).
       01  LEGEND-LIST          PIC X(7) VALUE "LEGEND.".
       01  PAGE1-SCORE-TYPE     PIC 9 VALUE 1.
           88  PAGE1-CUR-SCORE  VALUE 1.
           88  PAGE1-ORIG-SCORE VALUE 2.
       01  P1-SCORE-TYPE-DESC.
           03  FILLER     PIC X(10) VALUE
           "CUR SCORE ".
           03  FILLER     PIC X(10) VALUE
           "ORIG SCORE".
       01  PAGE1-SCORE-TYPE-DESC REDEFINES P1-SCORE-TYPE-DESC
                          PIC X(10) OCCURS 2.

       01  PAGE1-COLUMNS    PIC 9 VALUE 1.
           88  PAGE1-CUR-POT-30 VALUE 1.
           88  PAGE1-30-60-90   VALUE 2.

       01  PAGE1-DISPLAY-TYPE    PIC X VALUE "D".
           88  PAGE1-DEL-DISPLAY VALUE "D".
           88  PAGE1-PL-DISPLAY  VALUE "P".

      *=================================================================
      *
      *    S C R E E N - 1 - F I E L D S
      *
      *=================================================================
       01  DISPLAY-BUF1    VALUE " ".
           03  P1-TYPE          PIC X(10).
           03  P1-HEADER        PIC X(70).
           03  SCORE-RANGE      OCCURS 9 TIMES.
               05  DEL-AMT      OCCURS 4 TIMES.
                   07  D-AMT    PIC ZZZZ,ZZZ,ZZ9- BLANK ZERO.
                   07  D-PCT    PIC Z9.9- BLANK ZERO.
                   07  FILLER REDEFINES D-PCT.
                       09  FILLER   PIC X(4).
                       09  D-PCTS   PIC X.
                   07  FILLER REDEFINES D-PCT.
                       09  FILLER   PIC X.
                       09  D-PCT2   PIC ZZ9- BLANK ZERO.
               05  DEL-NO-PCT   OCCURS 4 TIMES.
                   07  D-NO     PIC ZZZZ,ZZZ,ZZ9- BLANK ZERO.
                   07  FILLER   PIC X(5).
       01  PAGE1-HEADERS.
           03  PAGE1-HEAD1.
               05  FILLER      PIC X(36) VALUE
               "   CURRENT              POTS      ".
               05  FILLER       PIC X(36) VALUE
               "       DEL             TOTAL      ".
           03  PAGE1-HEAD2.
               05  FILLER       PIC X(36) VALUE
               "      POTS              30'S      ".
               05  FILLER       PIC X(36) VALUE
               "      60'S               90+      ".
           03  PAGE1-PL-HEAD.
               05  FILLER       PIC X(36) VALUE
               " MTD # PL       MTD PL AMT   PERC   ".
               05  FILLER       PIC X(36) VALUE
               " YTD # PL       YTD PL AMT   PERC   ".

       01  DISPLAY-BUF1-PL      VALUE " ".
           03  P1-PL-TYPE       PIC X(10).
           03  P1-PL-HEADER     PIC X(70).
           03  SCORE-RANGE      OCCURS 9 TIMES.
               05  MTD-YTD-PL   OCCURS 2 TIMES.
                   07  D-PL-NO      PIC ZZZ,ZZZ,ZZ9- BLANK ZERO.
                   07  D-PL-AMT     PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
                   07  D-PL-PCT     PIC ZZZ9.9- BLANK ZERO.
                   07  FILLER REDEFINES D-PL-PCT.
                       09  FILLER   PIC X(6).
                       09  D-PL-PCTS PIC X.
               05  FILLER           PIC X(72).

       01  DISPLAY-LIST1.
           03  FILLER       PIC X(7)  VALUE "HEADER ".
           03  FILLER       PIC X(52) VALUE
           "LINE1 LINE1A LINE2 LINE2A LINE3 LINE3A LINE4 LINE4A ".
           03  FILLER       PIC X(52) VALUE
           "LINE5 LINE5A LINE6 LINE6A LINE7 LINE7A LINE8 LINE8A ".
           03  FILLER       PIC X(13) VALUE
           "LINE9 LINE9A.".

      **************************************************
      *    PAGE 1 TOTALS
      **************************************************
       01  PAGE-1-TOTALS              VALUE ZERO.
           03  SCORE-RANGE            OCCURS 9 TIMES.
               05  SCORE-CUR-ORIG     OCCURS 2 TIMES.
                   07  DEL-TABLE      OCCURS 7 TIMES.
                       09  SUM-AMT    PIC S9(10)V99.
                       09  SUM-NO     PIC S9(8).
                   07  PL-TABLE       OCCURS 2.
                       09  SUM-AMT-PL PIC S9(10)V99.
                       09  SUM-NO-PL  PIC S9(8).


       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/BYINQ_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPSSWK.CPY".

       01  SSINQ1-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/SSINQ1_DEF.CPY".
       COPY "LIBLP/SSINQ1_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/SSINQ1_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************************************
      *
      *           MAIN PROGRAM MODULE
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "SSINQ1" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SSINQ1-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-SS-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-SS-MODULE SECTION.
      ******************************************************************

           MOVE NNO TO ALL-BRANCHES-FLAG.
           MOVE "1" TO ALL-CLASSES-FLAG.
           MOVE " " TO IO-TYPE.

           PERFORM GET-TODAYS-DATE.

           GO TO SETUP-SS-DATE.
       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-SS-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.
           MOVE VDUNUM0 TO STORE-DATE DISPLAY-DATE.
           IF STORE-MO < 1 OR STORE-MO > 12
              GO TO ENTER-DATE.
       SETUP-SS-DATE.
           MOVE 20       TO SAVE-SS-CC.
           MOVE STORE-YR TO SAVE-SS-YY.
           MOVE STORE-MO TO SAVE-SS-MM.
           MOVE SAVE-SS-DATE TO SS-DATE-YYYYMM.
       ENTER-BRANCH.
           MOVE "E  A040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-SS-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BRANCH
              ELSE
                 MOVE "S  A000BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 GO TO READ-GROUP-FILE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

       READ-GROUP-FILE.
           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP.
           IF WGR-WORD = SPACES
              GO TO ENTER-BRANCH.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
                   AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.
           MOVE WGR-LEVEL TO SAVE-LEVEL.

           PERFORM OPEN-BR-FILE.

           IF WGR-TYPE NOT = "A"
              GO TO BRANCH-NUMBER-ENTERED.

           MOVE WGR-WORD TO ENT-GROUP.
           MOVE GR-NAME TO BR-NAME HOLD-GR-NAME.

           MOVE EXT-CONAME-USER-GROUP TO WGR-WORD.
           CALL "C$TOUPPER" USING WGR-WORD, VALUE 3.
      * IF GROUP SELECTED & IT'S = LOGIN GROUP, OKAY
           IF WGR-WORD = ENT-GROUP-1ST-3
              GO TO GROUP-OKAY.

      * IF LAST 3 CHARACTERS OF LOGIN NOT A VALID GROU, ENTER PASSWORD
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED.

           PERFORM OPEN-GR1-FILE.
           MOVE EXT-CONAME-USER-GROUP TO GR-ID.
           CALL "C$TOUPPER" USING GR-ID, VALUE 3.
           PERFORM SEARCH-GR-INIT.

           MOVE ENT-GROUP TO WGR-TEST-GROUP.
           PERFORM VERIFY-GROUP-MEMBER.
      * GROUP RANGE, LOGIN HAS VALID GROUP; IS GROUP WITHIN THAT GROUP?
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED.

           GO TO GROUP-OKAY.

       PASSWORD-REQUIRED.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "VIEWING THIS GROUP IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

       GROUP-OKAY.
           MOVE "Y" TO GROUP-FLAG.
           MOVE 0 TO STORE-BRNO.
           PERFORM SEND-BR-NAME.
           GO TO GET-CLASS.

       BRANCH-NUMBER-ENTERED.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO STORE-BRNO
                               BR-NO
                               SS-BRNO.
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE STORE-BRNO TO VDUNUM0.
           PERFORM SCREENIO.
           IF STORE-BRNO = 0
              GO TO ENTER-BRANCH.

           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO SS-PATH-OVERRIDE.

           PERFORM SEND-BR-NAME.
           IF STORE-BRNO = EXT-CONAME-USER-LOC
              GO TO BRANCH-OKAY.

           MOVE EXT-CONAME-USER-GROUP TO WGR-WORD.
           CALL "C$TOUPPER" USING WGR-WORD, VALUE 3.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED-BR.

           PERFORM OPEN-GR1-FILE.
           MOVE EXT-CONAME-USER-GROUP TO GR-ID.
           CALL "C$TOUPPER" USING GR-ID, VALUE 3.
           PERFORM SEARCH-GR-INIT.
           MOVE STORE-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED-BR.

           GO TO BRANCH-OKAY.

       PASSWORD-REQUIRED-BR.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "VIEWING THIS BRANCH IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

       BRANCH-OKAY.
           PERFORM OPEN-SS1-FILE.

       GET-CLASS.
           MOVE "S  A020LCX." TO VDU.
           PERFORM SCREENIO.

           MOVE "E  A020LC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-SS-MODULE.
           IF VDUSTATUS = "16"
              MOVE YYES TO ALL-CLASSES-FLAG
      * AS PER JR, MAKE WORLD ENTER CLASS MANUALLY.
              MOVE 01 TO SS-CLASS
              MOVE "S  A020LCX." TO VDU
              MOVE "ALL CLASSES" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF NOT IO-GOOD
                 GO TO GET-CLASS.
           IF VDUSTATUS NOT = "00"
              GO TO GET-CLASS.
           MOVE VDUBUF TO ENTER-CLASS-ALPHA.
           IF ENTER-CLASS-ALPHA NUMERIC
              GO TO ONE-CLASS-ENTERED.
           IF CLASS-WK1 = "S"
              MOVE "S" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "SALES FINANCE" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "L"
              MOVE "L" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "DIRECT LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "A"
              MOVE "A" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "CLASS 1, 2 & 3 LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "W"
              MOVE "W" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "WCBC LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "M"
              MOVE "M" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "SMALL LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "R"
              MOVE "R" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "REAL ESTATE LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "O"
              MOVE "O" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "CLASS 5 - 21 LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "F"
              MOVE "F" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "WCBC & CLASS 30" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           MOVE CLASS-WK1 TO CLASS-WK2.
           MOVE 0 TO CLASS-WK1.
           IF ENTER-CLASS-ALPHA NUMERIC
              MOVE "SN N020LC." TO VDU
              MOVE ENTER-CLASS-WK TO VDUNUM0
              PERFORM SCREENIO
              GO TO ONE-CLASS-ENTERED.
           GO TO GET-CLASS.

       ONE-CLASS-ENTERED.
           MOVE ENTER-CLASS-WK TO SS-CLASS
                                  STORE-CLASS
                                  LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CLASS.
           MOVE "S  A020LCX." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "1" TO ALL-CLASSES-FLAG.

       TEST-ALL.
           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.
           PERFORM SUMMARY-MODULE.
           PERFORM DISPLAY-PAGE.

       END-REC-SS-MODULE.
           EXIT.

      **************************************************
      *    SUMMARY-TYPES:
      *
      *    BR-CL        0 = ONE BRANCH   / ONE CLASS
      *    BR-ALLCL     1 = ONE BRANCH   / ALL CLASSES
      *    ALLBR-CL     2 = ALL BRANCHES / ONE CLASS
      *    ALLBR-ALLCL  3 = ALL BRANCHES / ALL CLASSES
      *    RESOLVED     1 OR 2 OR 3
      *
      **************************************************
       SUMMARY-MODULE SECTION.
           MOVE ZEROS TO PAGE-1-TOTALS.
           IF READ-ONCE-FG = "Y"
              IF GROUP-FLAG = "Y"
                 MOVE HOLD-GR-NAME TO BR-NAME
              END-IF
              GO TO EXIT-SUMMARY-MODULE.
           IF RESOLVED
              GO TO INC-NEXT-RECORD-KEY.

           MOVE "CALCULATING SUMMARY INFO - STANDBY" TO MESS.
           PERFORM CALL-TMMSG.

           IF GROUP-FLAG = "Y"
              GO TO INC-NEXT-RECORD-KEY.

           IF NOT-ALL-BRANCHES AND NOT ONE-CLASS
              MOVE 1 TO SUMMARY-TYPE
           ELSE
           IF ALL-BRANCHES AND ONE-CLASS
              MOVE 2 TO SUMMARY-TYPE
           ELSE
           IF ALL-BRANCHES AND NOT ONE-CLASS
              MOVE 3 TO SUMMARY-TYPE
              GO TO READ-NEXT-REC.

           IF ALL-BRANCHES
              MOVE 1 TO SS-BRNO
           ELSE
              MOVE STORE-BRNO TO SS-BRNO.
           IF ONE-CLASS
              MOVE STORE-CLASS TO SS-CLASS
           ELSE
              MOVE 1 TO SS-CLASS.

           IF NOT ALL-BRANCHES
                   AND ONE-CLASS
              PERFORM READ-SS1-FILE
              GO TO TEST-TYPE.

           GO TO START-FILE.

       INC-NEXT-RECORD-KEY.
           IF GROUP-FLAG NOT = "Y"
              GO TO SKIP-GROUP-LOGIC.

           IF FIRST-READ = "N"
              GO TO NEXT-GROUP-SS.
           MOVE "N" TO FIRST-READ.
       NEXT-GROUP-LOC.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO EXIT-SUMMARY-MODULE.

           MOVE SAVE-SS-DATE TO SS-DATE-YYYYMM.

           MOVE WGR-SRESULT TO SS-BRNO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO SS-PATH-OVERRIDE.

           PERFORM OPEN-SS1-FILE.
           IF NOT ONE-CLASS
              MOVE 1           TO SS-CLASS
                                  QSS1-WBEG-CLASS
              MOVE ALL "9"     TO QSS1-WEND-CLASS
 
           ELSE
              MOVE STORE-CLASS TO SS-CLASS
                                  QSS1-WBEG-CLASS
                                  QSS1-WEND-CLASS.


           MOVE SS-DATE-YYYYMM TO QSS1-WBEG-DATE
                                  QSS1-WEND-DATE.
           MOVE SS-BRNO        TO QSS1-WBEG-BRNO
                                  QSS1-WEND-BRNO.

           PERFORM START-SS1-FILE.
       NEXT-GROUP-SS.
           PERFORM READ-SS1-FILE-NEXT.
           IF NOT (IO-GOOD OR SS-BRNO NOT = WGR-SRESULT)
              PERFORM CLOSE-SS1-FILE
              GO TO NEXT-GROUP-LOC.

           IF SS-DATE-YYYYMM NOT = SAVE-SS-DATE
              PERFORM CLOSE-SS1-FILE
              GO TO NEXT-GROUP-LOC.

           IF ONE-CLASS
              IF SS-CLASS NOT = STORE-CLASS
                 PERFORM CLOSE-SS1-FILE
                 GO TO NEXT-GROUP-LOC
              ELSE
                 GO TO ACCUM-SS-TOTALS.

           PERFORM TEST-CLASS.
           IF CORRECT-CLASS = "N"
              GO TO NEXT-GROUP-SS.

           GO TO ACCUM-SS-TOTALS.

       SKIP-GROUP-LOGIC.
           MOVE SAVE-SS-DATE TO SS-DATE-YYYYMM.
           IF BR-ALLCL
              GO TO READ-NEXT-REC.
           IF ALLBR-CL
              ADD 1 TO SS-BRNO.
       START-FILE.
           MOVE SAVE-SS-DATE TO SS-DATE-YYYYMM
                                QSS1-WBEG-DATE
                                QSS1-WEND-DATE.
           MOVE SS-BRNO      TO QSS1-WBEG-BRNO.
           MOVE ALL "9"      TO QSS1-WEND-BRNO.
           MOVE SS-CLASS     TO QSS1-WBEG-CLASS.
           MOVE ALL "9"      TO QSS1-WEND-CLASS.

           PERFORM START-SS1-FILE.
       READ-NEXT-REC.
           PERFORM READ-SS1-FILE-NEXT.
       TEST-TYPE.
           IF IO-BAD
              IF E-CODE NOT = "00"
                 GO TO EXIT-SUMMARY-MODULE.

           IF SS-DATE-YYYYMM NOT = SAVE-SS-DATE
              GO TO EXIT-SUMMARY-MODULE.

           IF BR-ALLCL
              IF SS-BRNO > STORE-BRNO
                 GO TO EXIT-SUMMARY-MODULE.

           IF ONE-CLASS
              IF SS-CLASS NOT = STORE-CLASS
                 GO TO READ-NEXT-REC
              ELSE
                 GO TO ACCUM-SS-TOTALS.

           PERFORM TEST-CLASS.
           IF CORRECT-CLASS = "N"
              GO TO READ-NEXT-REC.

       ACCUM-SS-TOTALS.
           PERFORM SUM-FIELDS-1-SS.

           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-SS.
           IF BR-ALLCL OR ALLBR-CL
              GO TO INC-NEXT-RECORD-KEY.
           IF ALLBR-ALLCL
              GO TO READ-NEXT-REC.

       EXIT-SUMMARY-MODULE.
           EXIT.

      ************************************************
       SUM-FIELDS-1-SS SECTION.
      ************************************************

      * SUM FIELDS FROM CREDIT SCORE STATS PAGE 1
           IF PAGE1-CUR-SCORE
              MOVE 1 TO SSSUB2
           ELSE
              MOVE 2 TO SSSUB2.
           PERFORM VARYING
               SSSUB3 FROM 1 BY 1 UNTIL SSSUB3 > 5
                  AFTER SSSUB2 FROM 1 BY 1 UNTIL SSSUB2 > 2
                    AFTER SSSUB1 FROM 1 BY 1 UNTIL SSSUB1 > 8
                       ADD SS-UNITS(SSSUB1, SSSUB2, SSSUB3)
                          TO SUM-NO(SSSUB1, SSSUB2, SSSUB3)
                             SUM-NO(SSSUB1, SSSUB2, 7)
                             SUM-NO(     9, SSSUB2, SSSUB3)
                             SUM-NO(     9, SSSUB2, 7)
                       IF SSSUB3 > 2
                          ADD SS-UNITS(SSSUB1, SSSUB2, SSSUB3)
                             TO SUM-NO(SSSUB1, SSSUB2, 6)
                                SUM-NO(     9, SSSUB2, 6)
                       END-IF

                       ADD SS-OUTBAL(SSSUB1, SSSUB2, SSSUB3)
                          TO SUM-AMT(SSSUB1, SSSUB2, SSSUB3)
                             SUM-AMT(SSSUB1, SSSUB2, 7)
                             SUM-AMT(    9 , SSSUB2, SSSUB3)
                             SUM-AMT(    9 , SSSUB2, 7)
                       IF SSSUB3 > 2
                          ADD SS-OUTBAL(SSSUB1, SSSUB2, SSSUB3)
                             TO SUM-AMT(SSSUB1, SSSUB2, 6)
                                SUM-AMT(     9, SSSUB2, 6)
                       END-IF

                       IF SSSUB3 < 3
                          ADD SS-NO-PL(SSSUB1, SSSUB2, SSSUB3)
                             TO SUM-NO-PL(SSSUB1, SSSUB2, SSSUB3)
                                SUM-NO-PL(     9, SSSUB2, SSSUB3)
                          ADD SS-AMT-PL(SSSUB1, SSSUB2, SSSUB3)
                             TO SUM-AMT-PL(SSSUB1, SSSUB2, SSSUB3)
                                SUM-AMT-PL(     9, SSSUB2, SSSUB3)
                       END-IF
           END-PERFORM.

       SUM-FIELDS-1-SS-EXIT.
           EXIT.

      *************************************
       ENTRY-MODULE SECTION.
      *************************************

       ENTER-SELECTION.

           IF PAGE1-DEL-DISPLAY
              MOVE PAGE1-DEL-LEGEND TO LEGEND
           ELSE
              MOVE PAGE1-PL-LEGEND TO LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO READ-ONCE-FG
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS = "16"
              IF PAGE1-CUR-SCORE
                 MOVE 2 TO PAGE1-SCORE-TYPE
                 GO TO ACTIVITY-OPTION
              ELSE
                 MOVE 1 TO PAGE1-SCORE-TYPE
                 GO TO ACTIVITY-OPTION.

           IF PAGE1-DEL-DISPLAY
              IF VDUSTATUS = "1<"
                 IF PAGE1-COLUMNS = 1
                    MOVE 2 TO PAGE1-COLUMNS
                 ELSE
                    MOVE 1 TO PAGE1-COLUMNS
                 END-IF
                 GO TO ACTIVITY-OPTION.

           IF PAGE1-DEL-DISPLAY
              IF VDUBUF = "     P"
                 MOVE "P" TO PAGE1-DISPLAY-TYPE
                 GO TO ACTIVITY-OPTION
              END-IF
           ELSE
              IF VDUBUF = "     D"
                 MOVE "D" TO PAGE1-DISPLAY-TYPE
                 GO TO ACTIVITY-OPTION.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SELECTION.

           GO TO ENTER-SELECTION.

       ACTIVITY-OPTION.
           PERFORM DISPLAY-PAGE.
           GO TO ENTER-SELECTION.
       END-ENTRY-MODULE.
           EXIT.

      *****************************************************************
       DISPLAY-PAGE SECTION.
      *****************************************************************
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PAGE1-CUR-SCORE
              MOVE 1 TO TYPE-SUB
           ELSE
              MOVE 2 TO TYPE-SUB.

           PERFORM DISPLAY-FIELDS-1.

       END-DISPLAY-PAGE.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-1 SECTION.
      *****************************************************************
           MOVE " " TO DISPLAY-BUF1 DISPLAY-BUF1-PL
                       MESS.
           PERFORM CALL-TMMSG.
           IF PAGE1-CUR-SCORE
              MOVE 1 TO TYPE-SUB
           ELSE
              MOVE 2 TO TYPE-SUB.
           MOVE PAGE1-SCORE-TYPE-DESC(TYPE-SUB) TO P1-TYPE
                                                   P1-PL-TYPE.
           IF PAGE1-PL-DISPLAY
              MOVE PAGE1-PL-HEAD TO P1-PL-HEADER
              PERFORM MOVE-1-TOTALS-PL
                  VARYING MTD-SUB FROM 1 BY 1 UNTIL MTD-SUB > 2
                    AFTER SCORE-SUB FROM 1 BY 1 UNTIL SCORE-SUB > 9
              MOVE DISPLAY-BUF1-PL TO WR-BUF
              MOVE DISPLAY-LIST1 TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              IF PAGE1-CUR-POT-30
                 MOVE PAGE1-HEAD1 TO P1-HEADER
              ELSE
                 MOVE PAGE1-HEAD2 TO P1-HEADER
              END-IF
              PERFORM MOVE-1-TOTALS
                  VARYING DEL-SUB FROM 1 BY 1 UNTIL DEL-SUB > 4
                    AFTER SCORE-SUB FROM 1 BY 1 UNTIL SCORE-SUB > 9
              MOVE DISPLAY-BUF1 TO WR-BUF
              MOVE DISPLAY-LIST1 TO WR-LIST
              PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT-1.
           EXIT.

      *****************************************************************
       MOVE-1-TOTALS SECTION.
      *****************************************************************
           IF PAGE1-30-60-90
              COMPUTE DEL-SUB2 = DEL-SUB + 1
           ELSE
              IF DEL-SUB < 3
                 MOVE DEL-SUB TO DEL-SUB2
              ELSE
                 COMPUTE DEL-SUB2 = DEL-SUB + 3
              END-IF
           END-IF.

           COMPUTE D-AMT(SCORE-SUB DEL-SUB) ROUNDED =
              SUM-AMT(SCORE-SUB, TYPE-SUB, DEL-SUB2) * 1.
           COMPUTE D-NO(SCORE-SUB DEL-SUB) ROUNDED =
              SUM-NO(SCORE-SUB, TYPE-SUB, DEL-SUB2).

           IF SUM-AMT(SCORE-SUB, TYPE-SUB, 7) > 0
              COMPUTE PERCENT-WK ROUNDED =
                 SUM-AMT(SCORE-SUB, TYPE-SUB, DEL-SUB2) /
                 SUM-AMT(SCORE-SUB, TYPE-SUB, 7) * 100
           ELSE
              MOVE 0 TO PERCENT-WK.

           IF PERCENT-WK < 100
              MOVE PERCENT-WK TO D-PCT(SCORE-SUB DEL-SUB)
              IF PERCENT-WK > 0
                 MOVE "%" TO D-PCTS(SCORE-SUB DEL-SUB)
           ELSE
              COMPUTE PERCENT-WK2 ROUNDED = PERCENT-WK * 1
              MOVE PERCENT-WK2 TO D-PCT2(SCORE-SUB DEL-SUB)
              IF PERCENT-WK2 > 0
                 MOVE "%" TO D-PCTS(SCORE-SUB DEL-SUB).
       EXIT-MOVE-1.
           EXIT.

      *****************************************************************
       MOVE-1-TOTALS-PL SECTION.
      *****************************************************************
           MOVE SUM-AMT-PL(SCORE-SUB, TYPE-SUB, MTD-SUB) TO
                     D-PL-AMT(SCORE-SUB, MTD-SUB)
              MOVE SUM-NO-PL(SCORE-SUB, TYPE-SUB, MTD-SUB) TO
                     D-PL-NO(SCORE-SUB MTD-SUB)
              IF SUM-AMT-PL(9, TYPE-SUB, MTD-SUB) > 0
                 COMPUTE PERCENT-WK ROUNDED =
                    SUM-AMT-PL(SCORE-SUB, TYPE-SUB, MTD-SUB) /
                    SUM-AMT-PL(9, TYPE-SUB, MTD-SUB) * 100
              ELSE
                 MOVE 0 TO PERCENT-WK
              END-IF
              MOVE PERCENT-WK TO D-PL-PCT(SCORE-SUB MTD-SUB)
              IF PERCENT-WK > 0
                 MOVE "%" TO D-PL-PCTS(SCORE-SUB MTD-SUB).
       EXIT-MOVE-1.
           EXIT.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE "S  A020LC." TO VDU.
           MOVE LCSCAN-KEY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ***************************************************
       TEST-CLASS SECTION.
      ***************************************************
           MOVE "N" TO CORRECT-CLASS.

           MOVE SS-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              GO TO EXIT-TEST-CLASS.

           IF SF-CLASSES
              IF LC-SALEFIN = "Y"
                 IF LC-AFP-CLASS NOT = "Y"
                    MOVE "Y" TO CORRECT-CLASS.
           IF WORLD-CLASS123
              IF LC-CODE < 4
                 MOVE "Y" TO CORRECT-CLASS.
           IF LN-CLASSES
              IF LC-SALEFIN = "N" AND LC-GIFTNET NOT = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF SMALL-LN-CLASSES
              IF LC-SMALLOAN = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF NON-SMALL-LN-CLASSES
              IF LC-SMALLOAN = "N" AND LC-SALEFIN = "N"
                 MOVE "Y" TO CORRECT-CLASS.
           IF WCBC-CLASSES
              IF LC-GIFTNET = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF WORLD-OTHER-CLASSES
              IF LC-CODE > 4
                 IF LC-CODE < 22
                    MOVE "Y" TO CORRECT-CLASS.
           IF WORLD-SF-CLASSES
              IF LC-GIFTNET = "Y" OR LC-CODE = 30
                 MOVE "Y" TO CORRECT-CLASS.
           IF ALL-CLASSES
              MOVE "Y" TO CORRECT-CLASS.
           IF LC-SERVACCT = "Y"
              MOVE "N" TO CORRECT-CLASS.
       EXIT-TEST-CLASS.
           EXIT.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "STATS" TO PASSWDW-PGM.
           MOVE SAVE-LEVEL TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO VIEW THESE BRANCHES:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SSINQ1-SCREEN UPON SSINQ1-WINDOW-HANDLE.
           MOVE SSINQ1-FORM-FIELDS TO WR-BUF.
           MOVE SSINQ1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE SSINQ1-HELP--FILE TO HELP-LINK-FILE.
           MOVE SSINQ1-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/SSINQ1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       MISC-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE -1 TO NDTE-HOLD
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-MM   TO STORE-MO DISPLAY-MO.
           MOVE NDTE-YY   TO STORE-YR DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SEND-BR-NAME.
           MOVE "S  A000BRX." TO VDU.
           IF GROUP-FLAG = "Y"
              MOVE HOLD-GR-NAME TO VDUBUF
           ELSE
              MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SS1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSSGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSS1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SSINQ1-SCREEN.
           DESTROY SSINQ1-WINDOW-HANDLE.
           EXIT PROGRAM.
