      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      NONFLE.
      ******************************************************************
      *
      *                      (SP)
      *    DESCRIPTION:  NON-FILE INSURANCE REPORT
      *
      *     570 RESERVE AT END OF PREVIOUS MONTH
      *     571 PREMIUMS WRITTEN
      *     572 WORLDS PORTION 90%
      *     573 CLAIMS PAID
      *     574 RECOVERIES
      *     575 NET CLAIMS
      *     576 RESERVE AT END OF CURRENT PERIOD
      *
      * REV:
      *  040317 BAH NEW FOR WORLD
      * GLB 040406 REMOVE DEFAULT OF SYSTEM DATE (MMYY)
      * BLV 040430 CHANGE HEADING, GET RID OF WORD "RESERVE"
      * CS  040624 SKIP BRANCHES WITH ZERO IN EACH OF THE 7 COMPONENTS
      * BLF 041213 CHANGE REPORT TO PRINT & COMPUTE WITH LIFE OF THE
      *            SOUTH PREMIUMS, CLAIMS, RECOVERIES
      * BLF 061115 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *            EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      * 20171010 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * 20180328 BLM FIX INCORRECT WRFILE KEY FOR PRIOR MONTH
      * BAH 20200831 REMOVED RESET ON BR1 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      /
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPRL.CPY".
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/NONFLE.CBL J35 12/21/23-09:43:52 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999   COMP   VALUE 0.
           03  LN-COUNT         PIC 999   COMP   VALUE 99.
           03  LN-WK            PIC 999   COMP   VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)  COMP   VALUE 0.

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  SUB                  PIC 99.
       01  TOT-SUB              PIC 99.
       01  LEVEL-SUB            PIC 9999.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.

       01  WS-BRNAMEX           PIC X(15).
       01  WS-BRNAME REDEFINES WS-BRNAMEX PIC X OCCURS 15 TIMES.
       01  OUT-SUB              PIC 99.

       01  CUR-MO-WR-DATE       PIC 9(6).
       01  FILLER              REDEFINES CUR-MO-WR-DATE.
           03  CURRENT-YEAR     PIC 9(4).
           03  CURRENT-MONTH    PIC 99.

       01  LAST-MO-YYYYMM      PIC 9(6).
       01  FILLER REDEFINES LAST-MO-YYYYMM.
           03  LAST-MO-YYYY    PIC 9(4).
           03  LAST-MO-MM      PIC 99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LAST MONTH RATING SHEET RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRLM".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS MONTH RATING SHEET RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRTM".

      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB          OCCURS 10.
                07  T-PREV      PIC S9(13)V99 COMP.
                07  T-PREM      PIC S9(13)V99 COMP.
                07  T-WORLD     PIC S9(13)V99 COMP.
                07  T-CLAIMS    PIC S9(13)V99 COMP.
                07  T-RECOVER   PIC S9(13)V99 COMP.
                07  T-NET       PIC S9(13)V99 COMP.
                07  T-CURRENT   PIC S9(13)V99 COMP.

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       OCCURS 15
                                PIC 9999     COMP.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".

       01  STAR-LINE.
           03  FILLER          PIC X(46) VALUE
           "______________________________________________".
           03  FILLER          PIC X(45) VALUE
           "_____________________________________________".
           03  FILLER          PIC X(45) VALUE
           "_____________________________________________".

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X OCCURS 10.
           03  STORE-DATE      PIC 9(4).
           03  STORE-DATEX     REDEFINES STORE-DATE.
               05  STORE-MO    PIC 9(2).
               05  STORE-YR    PIC 9(2).
           03  LOCAL-PRINT-FG  PIC X.

       01  VERSION-CONTROL     PIC X     VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZZ9.

      *----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(8) VALUE "USERID: ".
           03  H-USERID         PIC X(8).
           03  FILLER           PIC X(35) VALUE " ".
           03  FILLER           PIC X(20) VALUE
               "NONFILE INSURANCE".
           03  FILLER           PIC X(5) VALUE " ".
           03  H-MMYY           PIC 99/99.

      *----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(28) VALUE  SPACES.
           03  FILLER           PIC X(23) VALUE
           "END OF         PREMIUMS".
           03  FILLER           PIC X(24) VALUE
           "      WORLDS".
           03  FILLER           PIC X(40) VALUE SPACES.
           03  FILLER           PIC X(15) VALUE
           "END OF        ".


       01  HEAD4.
           03  FILLER           PIC X(26) VALUE "BRNO ".
           03  FILLER           PIC X(29) VALUE
           "PREV MONTH        WRITTEN    ".
           03  FILLER           PIC X(26) VALUE
           "PORTION 90%    CLAIMS PAID".
           03  FILLER           PIC X(48) VALUE
           "     RECOVERIES     NET CLAIMS    CURRENT PERIOD".


      *----------------------------------------------------------------

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE            PIC X(132) VALUE " ".
       01  DETAIL1               REDEFINES DETAIL-LINE.
           03  D-BRNO             PIC Z(4).
           03  FILLER             PIC X.
           03  D-BRNAME           PIC X(15).
           03  D-BRNAMEX REDEFINES D-BRNAME PIC X OCCURS 15 TIMES.
           03  FILLER             PIC XX.

           03  D-PREVIOUS-MONTH   PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-PREM-WRITTEN     PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-WORLD-PORTION    PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-CLAIMS-PAID      PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-RECOVERIES       PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-NET-CLAIMS       PIC ZZZ,ZZZ,ZZZ.99-.
           03  FILLER             PIC XXX.
           03  D-CURRENT-PERIOD   PIC ZZZ,ZZZ,ZZZ.99-.

      *----------------------------------------------------------------
       01  RANGE-LINE            REDEFINES DETAIL-LINE.
           03  FILLER             PIC X(30).
           03  RANGE-SEL          PIC X(6).
           03  FILLER             PIC X(100).

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/NONFLE_DEF.CPY".
       COPY "LIBSP/NONFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/NONFLE_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "NONFLE" TO VDU-FORM-NAME.

           MOVE LOW-VALUES TO TOTAL-TABLE.

           PERFORM INITIALIZATION.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * AT END OF FILE, FORCE WGR-SCURR -TABLE & TPLUS TO CHANGE FROM
      * PREVIOUS SO GRAND TOTALS WILL PRINT - MUST FORCE BECAUSE ALL
      * LEVELS START AT THE SAME POINT (THE SELECTED PARENT GROUP)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM TOTAL-LEVEL-CHECK
              GO TO END-ROUTINE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE
           ELSE
              PERFORM TOTAL-LEVEL-CHECK.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              PERFORM PRINT-ONE-BRANCH-GRAND
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           PERFORM OPEN-WR1-FILE.

           MOVE CUR-MO-WR-DATE TO WR-YYYYMM
                                  QWR1-WBEG-YYYYMM
                                  QWR1-WEND-YYYYMM.
           IF GROUP-FLAG = "Y"
              MOVE WGR-SRESULT TO WR-BRNO
           ELSE
              MOVE BR-NO       TO WR-BRNO.
           MOVE WR-BRNO        TO QWR1-WBEG-BRNO
                                  QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.

       NEXT-WR1.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-FG NOT = 0
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF WR-YYYYMM NOT = CUR-MO-WR-DATE
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF GROUP-FLAG = "Y"
              IF WR-BRNO NOT = WGR-SRESULT
                 GO TO END-OF-GROUP.

           IF GROUP-FLAG NOT = "Y"
              IF WR-BRNO NOT = BR-NO
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

      *----------------------------------------------------------------
      *    OK-TO-PROCESS
      *----------------------------------------------------------------
           MOVE "N" TO FIRST-TIME.

           PERFORM CHECK-ADD-TOTALS.

           GO TO NEXT-WR1.

       END-OF-GROUP.
           PERFORM CLOSE-WR1-FILE.
           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-LOC
           ELSE
              GO TO NEXT-BRANCH-NO.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "N"
              MOVE 9 TO TOT-SUB
              PERFORM ADD-TOTALS
              MOVE 1 TO TOT-SUB
              PERFORM ADD-TOTALS
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-TPLUS TO GROUP-SUB.

       CHECK-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.

           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              MOVE 9 TO TOT-SUB
              GO TO CHECK-TOTAL-FLAG.

           MOVE WGR-SID(LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO TOT-SUB.

       CHECK-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

       CONTINUE-CHECK-TOTALS.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.

       EXIT-CHECK-TOTALS.
           EXIT.

      ******************************************************************
       ADD-TOTALS SECTION.
      ******************************************************************

      * MOVE CURRENT WR-REC TO THIS MONTH WRTM RECORD
      *   - AND READ LAST MONTHS  WRLM-REC
      *   - PROCESS
      * RESTORE THIS MONTH WRTM RECORD AND RESTART FILE:

      * MOVE CURRENT WR-REC TO THIS MONTH WRTM RECORD
           MOVE WR-REC TO WRTM-REC.

      * GET PREVIOUS MONTH
           MOVE LAST-MO-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-FG = 9
              PERFORM CLEAR-WR-FILE.

      * SAVE LAST MONTHS RECORD
           MOVE WR-REC TO WRLM-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     570 RESERVE AT END OF PREVIOUS MONTH
      *     571 PREMIUMS WRITTEN
      *     572 WORLDS PORTION 90%
      *     573 CLAIMS PAID
      *     574 RECOVERIES
      *     575 NET CLAIMS
      *     576 RESERVE AT END OF CURRENT MONTH
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD WRLM-NONFILE-ENDING-BALANCE TO T-PREV(TOT-SUB).
           ADD WRTM-LOTS-NON-FILE-PREMS(1) TO T-PREM(TOT-SUB).
           COMPUTE T-WORLD(TOT-SUB) = T-WORLD(TOT-SUB) +
                     ( WRTM-LOTS-NON-FILE-PREMS(1) * .90 )
           ADD WRTM-LOTS-NF-CLAIMS(1)     TO T-CLAIMS(TOT-SUB).
           ADD WRTM-LOTS-NF-RECOVERIES(1) TO T-RECOVER(TOT-SUB).
      * NET CLAIMS = CLAIMS PAID MINUS RECOVERIES
           COMPUTE T-NET(TOT-SUB) = T-NET(TOT-SUB) +
               WRTM-LOTS-NF-CLAIMS(1) - WRTM-LOTS-NF-RECOVERIES(1).
           ADD WRTM-NONFILE-ENDING-BALANCE TO T-CURRENT(TOT-SUB).

       EXIT-ADD-TOTALS.
      * RESTORE THIS MONTH RECORD AND RESTART FILE:
           MOVE WRTM-REC TO WR-REC.

           PERFORM START-WR1-FILE.

           PERFORM READ-WR1-FILE-NEXT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE NONFLE-QUEUE-POS TO Q-POS.
           MOVE NONFLE-COPIES-POS TO C-POS.
           MOVE NONFLE-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH     TO SV-BEG-BR
              MOVE END-BRANCH     TO SV-END-BR
              MOVE GROUP-FLAG     TO SV-GROUP-FLAG
              MOVE ENT-GROUP      TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
              IF EXT-EOBUF-BRNO NUMERIC
                 MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
                 MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              ELSE
                 MOVE EXT-EOBUF-GROUP TO ENT-GROUP
                 MOVE 0 TO BEG-BRANCH
                 MOVE 9999 TO END-BRANCH
                 MOVE "Y" TO GROUP-FLAG
                 MOVE "N" TO LOCAL-PRINT-FG
              END-IF
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE STORE-DATE TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           IF EXT-ROUTE-BUF = "00"
              GO TO EXIT-INITIALIZE.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE " "        TO TOTAL-LEV-TABLE
              MOVE "Y"        TO TOTAL-LEVEL-TABLE(9).

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

      * SETUP CURRENT MONTH DATE:
           MOVE EXT-EOBUF-DATE2 TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO STORE-MO CURRENT-MONTH.
           MOVE DATE-YYYYMMDD-YY TO STORE-YR.
           MOVE DATE-YYYYMMDD-YYYY TO CURRENT-YEAR.
           MOVE CUR-MO-WR-DATE TO LAST-MO-YYYYMM.
           IF LAST-MO-MM > 1
              SUBTRACT 1 FROM LAST-MO-MM
           ELSE
              SUBTRACT 1 FROM LAST-MO-YYYY
              MOVE 12 TO LAST-MO-MM.
           
       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           IF VDUSTATUS = "1U" GO TO BR-01.

           PERFORM GET-TODAYS-DATE.

       ENTER-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           IF VDUNUM0 = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              MOVE ALP-MO TO STORE-MO DISPLAY-MO
              MOVE ALP-YR TO STORE-YR DISPLAY-YR
              MOVE "SN N040MMYY." TO VDU
              MOVE DISPLAY-DATE TO VDUNUM0
              PERFORM SCREENIO
              GO TO SETUP-WR-DATE.

           MOVE VDUNUM0 TO STORE-DATE
                           DISPLAY-DATE.

      *----------------------------------------------------------------
       SETUP-WR-DATE.
           MOVE STORE-MO TO NUM-MO.
           MOVE STORE-YR TO NUM-YR.
           IF NUM-YR < EXT-JULIAN-CC
              ADD 2000 NUM-YR GIVING CURRENT-YEAR
           ELSE
              ADD 1900 NUM-YR GIVING CURRENT-YEAR.
           MOVE NUM-MO TO CURRENT-MONTH.

           MOVE CUR-MO-WR-DATE TO LAST-MO-YYYYMM.
           IF LAST-MO-MM > 1
              SUBTRACT 1 FROM LAST-MO-MM
           ELSE
              SUBTRACT 1 FROM LAST-MO-YYYY
              MOVE 12 TO LAST-MO-MM.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CHECK-LEVEL-PRINTS.

           IF ( GROUP-FLAG = "Y" )
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

           IF GROUP-FLAG = "Y"
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.
      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CHECK-LEVEL-PRINTS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE SPACES TO VDUBUF.
           MOVE "S  A040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
           ELSE
              MOVE "SN N040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SN N040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *   CHECK FOR LEVEL SUMMARY TOTALS
      *   WGR-SCURR IS TABLE OF POINTERS FOR CURRENT GROUP
      *   WHICH POINT TO WGR-SID TABLE TO SHOW CURRENT
      *   GROUP HIERARCHY:  WGR-SCURR(1) POINTS TO WGR-SID
      *   OF LOW LEVEL (SELECTED GROUP); WGR-SCURR(WGR-TPLUS)
      *   POINTS TO THE WGR-SID OF THE HIGHEST LEVEL WHICH
      *   IS THE BRANCH NUMBER CURRENTLY BEING PROCESSED.
      *   THIS ROUTINE COMPARES THE WGR-SCURR TABLE TO THE
      *   TABLE SAVED FROM THE PREVIOUS GROUP, PRINTING &
      *   ZEROING TOTALS FOR EACH LEVEL WHICH HAS CHANGED
      *   STARTING WITH THE HIGHEST (BRANCH) LEVEL & WORKING
      *   THROUGH HIERARCHY.  GRAND TOTALS ARE FORCED BY
      *   MOVING 0 TO WGR-SCURR TABLE & 15 TO WGR-TPLUS.
      ******************************************************************
      *
      *     TOTAL-LEVEL-CHECK SECTION.
      *
      ******************************************************************
       TOTAL-LEVEL-CHECK SECTION.

           MOVE 15 TO GROUP-SUB.

       LEVEL-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           IF WGR-SCURR(GROUP-SUB) = PREV-SCURR(GROUP-SUB)
              GO TO CONTINUE-LOOP.

           MOVE PREV-SCURR(GROUP-SUB) TO PREV-SUB.

           IF PREV-SUB = 0
              GO TO CONTINUE-LOOP.

           IF WGR-SID(PREV-SUB) NUMERIC
              GO TO CHECK-BRANCH-SID.

           MOVE WGR-SID(PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           IF TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE GR-LEVEL TO TOT-SUB.

           STRING "GROUP:"," ",GR-ID DELIMITED BY "  " INTO D-BRNAME.
           PERFORM PRINT-TOTALS.
           GO TO CONTINUE-LOOP.

       CHECK-BRANCH-SID.
           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE 9 TO TOT-SUB.
           MOVE BR-NO TO D-BRNO.
           PERFORM SHIFT-BRNAME.
           PERFORM PRINT-TOTALS.

       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.


      ***********************
       SHIFT-BRNAME SECTION.
      ***********************
           MOVE 1 TO SUB OUT-SUB.
           MOVE BR-NAME TO WS-BRNAMEX.
       NEXT-BRNAME.
           IF WS-BRNAME(SUB) NOT = SPACES
              MOVE WS-BRNAME(SUB) TO D-BRNAMEX(OUT-SUB)
              ADD 1 TO OUT-SUB.
           ADD 1 TO SUB.
           IF SUB NOT > 15
              GO TO NEXT-BRNAME.

      ******************************************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      ******************************************************************

           IF LOCAL-PRINT-FG = "Y"
              MOVE 0 TO PAGE-NUMBER
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO EXIT-PRINT-ONE-BRANCH.

           IF FIRST-TIME = "Y"
              GO TO EXIT-PRINT-ONE-BRANCH.

           MOVE 9 TO TOT-SUB.
           PERFORM SHIFT-BRNAME.
           MOVE BR-NO TO D-BRNO.
           PERFORM PRINT-TOTALS.
           MOVE "N" TO FIRST-TIME.

       EXIT-PRINT-ONE-BRANCH.
           EXIT.

      ******************************************************************
       PRINT-ONE-BRANCH-GRAND SECTION.
      ******************************************************************

           IF BEG-BRANCH = END-BRANCH
              GO TO EXIT-PRINT-ONE-GRAND.

           IF LOCAL-PRINT-FG = "Y"
              GO TO EXIT-PRINT-ONE-GRAND.

           MOVE 1 TO TOT-SUB.
           MOVE 0 TO D-BRNO.
           MOVE "GRAND TOTALS:" TO D-BRNAME.
           PERFORM PRINT-TOTALS.

       EXIT-PRINT-ONE-GRAND.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************

           IF ( T-PREV(TOT-SUB) =  ZEROS AND
                T-PREM(TOT-SUB) =  ZEROS AND
                 T-WORLD(TOT-SUB)  = ZEROS AND
                  T-CLAIMS(TOT-SUB)  = ZEROS AND
                   T-RECOVER(TOT-SUB) = ZEROS AND
                    T-NET(TOT-SUB) = ZEROS AND
                     T-CURRENT(TOT-SUB) = ZEROS)
               MOVE ZEROS TO D-BRNO
               MOVE SPACES TO D-BRNAME
               GO TO EXIT-PRINT-TOTALS.

           MOVE T-PREV(TOT-SUB) TO D-PREVIOUS-MONTH.
           MOVE T-PREM(TOT-SUB) TO D-PREM-WRITTEN.
           MOVE T-WORLD(TOT-SUB) TO D-WORLD-PORTION.
           MOVE T-CLAIMS(TOT-SUB) TO D-CLAIMS-PAID.
           MOVE T-RECOVER(TOT-SUB) TO D-RECOVERIES.
           MOVE T-NET(TOT-SUB) TO D-NET-CLAIMS.
           MOVE T-CURRENT(TOT-SUB) TO D-CURRENT-PERIOD.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-TOTALS.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > 63
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-EXIT.
           EXIT.

      **********************
       HEAD-PAGE SECTION.
      **********************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 7 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           MOVE 99 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH         TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH" TO RANGE-LINE
              MOVE END-BRANCH      TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBSP/SPWRCL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO NONFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO NONFLE-SYSDATE.
           DISPLAY NONFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE NONFLE-FORM-FIELDS TO WR-BUF.
           MOVE NONFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE NONFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE NONFLE-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/NONFLE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".

      *-----------------------------------------------------------------
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO STORE-MO DISPLAY-MO.
           MOVE ALP-YR TO STORE-YR DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPWR1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      *----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-EOBUF-GROUP-FG = "Y"
              IF SV-BEG-BR NOT = 9999
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY NONFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
