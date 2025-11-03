       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ISTATS.
      ******************************************************************
      *
      *                      (SP)
      *    DESCRIPTION:  INSURANCE STATISTICS LISTING
      *
      *       WORLD : BEN HOLLOWAY (ACCOUNTING MGR) USES FOR INSURANCE
      *               RECON AND REMIT.  5/9/2023
      *
      * REV:
      *  031112 BAH NEW FOR WORLD INSURANCE STATISTICS PR# 331
      *  040127 BLV FIX DATE IN HEADING, WAS PRINTING GARBAGE WHEN
      *             RUN IN BATCH
      * GLB 040406 REMOVE DEFAULT OF SYSTEM DATE (MMYY)
      * GLB 040525 FIX HEADINGS; PRINT BRANCH NO / NAME NOT SHORT-NAME
      * BLF 041221 FIX FOR LOTS INSURANCE, PRINT LOTS INFO FOR NONFILE
      *            PREMIUMS.  COMBINE LOTS RECOVERIES & CLAIMS
      *            WITH ASSURANCE FIELDS FOR NONFILE CLAIMS AND
      *            RECOVERIES.
      * BLF 050111 INCLUDE ASSURANT NON-FILE PREMIUMS IN TOTALS, WERE
      *            MISSING FROM QUARTER-TO-DATE & YTD TOTALS FOR 12/04.
      * BLF 061115 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *            EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      * BLF 070717 WORLD WAS TRYING TO RUN FOR PERIODS IN PRIOR YEAR & THE
      *            QUARTER-TO-DATE NUMBERS WERE WRONG.  THESE NUMBERS WERE
      *            COMPUTED BY SUBTRACTING THE PRIOR QUARTER'S YTD FROM
      *            THE SELECTED PERIOD'S YTD.  THE PROBLEM IS PROGRAM
      *            IS READING PRIOR QUARTER WR BASED ON THE MO/YR IN THE
      *            CURRENT GP-REC FOR THAT QUARTERS PERIOD. CHANGED TO
      *            USE THE YEAR OF THE SELECTED PERIOD, OR SUBTRACTING
      *            1 FROM THAT YEAR IF PRIOR QUARTER DATE IS DECEMBER.
      *            WORLD PR#546
      *  20170926 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2019.0307 REMOVED REDEFINES OF GP-END-FISCAL-DATE, WAS USING
      *               GP-END-FISCAL-DATE-MM, USE DATE WORKERS INSTEAD
      * BAH 20200831 REMOVED RESET ON BR1 
      * MB  20231004 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
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
           "@(#)SP/ISTATS.CBL S35 12/22/23-11:52:49 >".

      ******************************************************************
      *     F I L E   I F N
      ******************************************************************
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

       01  WR1-CONN-OPEN-SW     PIC X        VALUE "N".

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

       01  CUR-PERIOD          PIC 99.
       01  LAST-QTR-WR-DATE    PIC 9(6).
       01  FILLER               REDEFINES LAST-QTR-WR-DATE.
           03  LAST-QTR-YEAR    PIC 9(4).
           03  LAST-QTR-MONTH   PIC 99.

       01  CUR-MO-WR-DATE       PIC 9(6).
       01  FILLER              REDEFINES CUR-MO-WR-DATE.
           03  CURRENT-CC       PIC 99.
           03  CURRENT-YY       PIC 99.
           03  CURRENT-MM       PIC 99.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LAST YEAR RATING SHEET RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRLQ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS MONTH RATING SHEET RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRTM".

      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB          OCCURS 10.
               05  T-LINE         OCCURS 67.
                   07  T-MTD       PIC S9(13)V99 COMP.
                   07  T-QTD       PIC S9(13)V99 COMP.
                   07  T-YTD       PIC S9(13)V99 COMP.

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR          OCCURS 15
                                   PIC 9999  COMP.

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
           03  FILLER           PIC X(2)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(10)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZZ9.

      *----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(8) VALUE "USERID: ".
           03  H-USERID         PIC X(8).
           03  FILLER           PIC X(12) VALUE " ".
           03  FILLER           PIC X(26) VALUE
           "-- INSURANCE STATISTICS --".
           03  FILLER           PIC X(2) VALUE " ".
           03  H-MMYY           PIC 99/99.
           03  FILLER           PIC X(19) VALUE " ".

      *----------------------------------------------------------------
       01  HEAD3.
           03  HEAD3-TYPE       PIC X(8)    VALUE "BRANCH:".
           03  H-GRID           PIC X(4).
           03  H-BRNO           REDEFINES H-GRID PIC Z(4).
           03  FILLER           PIC X     VALUE " ".
           03  H-GRNAME         PIC X(30) VALUE " ".
           03  FILLER           PIC X(18)  VALUE " ".

      *----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(2)  VALUE " ".
           03  FILLER           PIC X(1)  VALUE " ".
           03  FILLER           PIC X(37) VALUE SPACES.
           03  FILLER           PIC X(51) VALUE
               "MONTH     QRTR-TO-DATE      YEAR-TO-DATE".

      *----------------------------------------------------------------

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE            PIC X(80) VALUE " ".
       01  DETAIL1               REDEFINES DETAIL-LINE.
           03  D-LINENO           PIC 999.
           03  FILLER             PIC X.
           03  D-DESC             PIC X(28).
           03  FILLER             PIC X.

           03  D-MTD              PIC Z,ZZZ,ZZZ.99-.
           03  D-MTD-NO REDEFINES D-MTD
                                  PIC ZZZZZZZZ,ZZ9-.
           03  D-MTD-P            PIC X.

           03  FILLER             PIC X(1).

           03  D-QTD              PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-QTD-NO REDEFINES D-QTD
                                  PIC ZZZZZZZZZZ,ZZ9-.
           03  D-QTD-P            PIC X.

           03  FILLER             PIC X(2).

           03  D-YTD              PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-YTD-NO  REDEFINES D-YTD
                                  PIC ZZZZZZZZZZ,ZZ9-.
           03  D-YTD-P            PIC X.

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
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ISTATS_DEF.CPY".
       COPY "LIBSP/ISTATS_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ISTATS_SCN.CPY".

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
           PERFORM CLOSE-WR1-CONNECTION.
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
           MOVE "ISTATS" TO VDU-FORM-NAME.

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

           IF EXT-ROUTE-BUF = "00"
               MOVE SPACES TO WR-BUF
               STRING "REPORT IN PROGRESS...............", WR-BRNO
                      DELIMITED "  " INTO WR-BUF
               MOVE MESS-LIST TO WR-LIST
               PERFORM CALL-WRFORM
           END-IF.

           PERFORM START-WR1-FILE.  *> SSP WR

       NEXT-WR1.
           PERFORM READ-WR1-FILE-NEXT.  *> EMBEDDED WR
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

      * OK-TO-PROCESS

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

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       CHECK-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

      *----------------------------------------------------------------
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
      *     - AND READ LAST QUARTER  WRLQ-REC
      *     - PROCESS
      * RESTORE THIS MONTH WRTM RECORD AND RESTART FILE:

      * MOVE CURRENT WR-REC TO THIS MONTH WRTM RECORD

           MOVE WR-REC TO WRTM-REC.

      * READ LAST QUARTER WRLY-REC:
           MOVE LAST-QTR-WR-DATE TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.   *> EMBEDDED WR
           IF IO-FG = 9 OR CUR-PERIOD NOT > 3
              PERFORM CLEAR-WR-FILE.
      * YTD IN 1 THRU 3 IS QUARTER TO DATE, SO CLEAR THE RECORD SO I
      * CAN STILL USE THE SUBTRACT

           MOVE WR-REC TO WRLQ-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 01: 500 LIFE PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO SUB.
           ADD WRTM-LIFE-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                     ( WRTM-LIFE-PREMS(2) - WRLQ-LIFE-PREMS(2)).
           ADD WRTM-LIFE-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 02: 501 A AND H PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 02 TO SUB.
           ADD WRTM-AH-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                       ( WRTM-AH-PREMS(2) - WRLQ-AH-PREMS(2)).
           ADD WRTM-AH-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 03: 502 TOTAL LIFE,A-H PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 03 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   ( T-MTD(TOT-SUB,1) + T-MTD(TOT-SUB,2) ).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   ( T-QTD(TOT-SUB,1) + T-QTD(TOT-SUB,2) ).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   ( T-YTD(TOT-SUB,1) + T-YTD(TOT-SUB,2) ).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 04: 503 LIFE REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 04 TO SUB.
           ADD WRTM-LIFE-REFS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                       ( WRTM-LIFE-REFS(2) - WRLQ-LIFE-REFS(2)).
           ADD WRTM-LIFE-REFS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 05: 504 A AND H REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 05 TO SUB.
           ADD WRTM-AH-REFS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                       ( WRTM-AH-REFS(2) - WRLQ-AH-REFS(2)).
           ADD WRTM-AH-REFS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 06: 505 MISC REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 06 TO SUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 07: 506 TOTAL REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 07 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) = T-MTD(TOT-SUB,4) +
                   T-MTD(TOT-SUB,5) + T-MTD(TOT-SUB,6).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,4) +
                   T-QTD(TOT-SUB,5) + T-QTD(TOT-SUB,6).
           COMPUTE T-YTD(TOT-SUB,SUB) = T-YTD(TOT-SUB,4) +
                   T-YTD(TOT-SUB,5) + T-YTD(TOT-SUB,6).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 08:  507 NET LIFE,A-H PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 08 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,7).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,3) - T-QTD(TOT-SUB,7).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,7).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 09: 508 NBR LIFE POLICIES WRITTEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 09 TO SUB.
           ADD WRTM-NO-LIFE-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-LIFE-PREMS(2) - WRLQ-NO-LIFE-PREMS(2)).
           ADD WRTM-NO-LIFE-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 10: 509 NBR A-H POLICIES WRITTEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 10 TO SUB.
           ADD WRTM-NO-AH-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-AH-PREMS(2) - WRLQ-NO-AH-PREMS(2)).
           ADD WRTM-NO-AH-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 11: 510 LIFE CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 11 TO SUB.
           ADD WRTM-LIFE-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-LIFE-CLAIMS(2) - WRLQ-LIFE-CLAIMS(2)).
           ADD WRTM-LIFE-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 12: 511 A-H CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 12 TO SUB.
           ADD WRTM-AH-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-AH-CLAIMS(2) - WRLQ-AH-CLAIMS(2)).
           ADD WRTM-AH-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 13: 512 TOTAL LIFE, A-H CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 13 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,11) + T-MTD(TOT-SUB,12).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,11) + T-QTD(TOT-SUB,12).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,11) + T-YTD(TOT-SUB,12).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 14: 513 PERCENT LIFE CLAIMS
      *              LIFE CLAIMS (510) / LIFE PREMIUMS (500)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 14 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
               (T-MTD(TOT-SUB,11) / T-MTD(TOT-SUB,1) ) * 100.
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED  =
               (T-QTD(TOT-SUB,11) / T-QTD(TOT-SUB,1) ) * 100.
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
               (T-YTD(TOT-SUB,11) / T-YTD(TOT-SUB,1) ) * 100.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 15: 514 PERCENT A-H CLAIMS
      *              A-H CLAIMS (511) / A-H PREMIUMS (501)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 15 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
               (T-MTD(TOT-SUB,12) / T-MTD(TOT-SUB,2) ) * 100.
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED =
               (T-QTD(TOT-SUB,12) / T-QTD(TOT-SUB,2) ) * 100.
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
               (T-YTD(TOT-SUB,12) / T-YTD(TOT-SUB,2) ) * 100.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 16: 515 NBR OF LIFE CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 16 TO SUB.
           ADD WRTM-NO-LIFE-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-LIFE-CLAIMS(2) - WRLQ-NO-LIFE-CLAIMS(2)).
           ADD WRTM-NO-LIFE-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 17: 516 NBR OF A-H CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 17 TO SUB.
           ADD WRTM-NO-AH-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-AH-CLAIMS(2) - WRLQ-NO-AH-CLAIMS(2)).
           ADD WRTM-NO-AH-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 18:  517 PROPERTY PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 18 TO SUB.
           ADD WRTM-FIRE-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-FIRE-PREMS(2) - WRLQ-FIRE-PREMS(2)).
           ADD WRTM-FIRE-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 19: 518 AUTO PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 19 TO SUB.
           ADD WRTM-AUTO-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-AUTO-PREMS(2) - WRLQ-AUTO-PREMS(2)).
           ADD WRTM-AUTO-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 20: 519 TOTAL PROP AND AUTO PREM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 20 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,18) + T-MTD(TOT-SUB,19).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,18) + T-QTD(TOT-SUB,19).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,18) + T-YTD(TOT-SUB,19).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 21: 520 PROPERTY REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 21 TO SUB.
           ADD WRTM-FIRE-REFS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                       ( WRTM-FIRE-REFS(2) - WRLQ-FIRE-REFS(2)).
           ADD WRTM-FIRE-REFS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 22: 521 AUTO REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 22 TO SUB.
           ADD WRTM-AUTO-REFS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                       ( WRTM-AUTO-REFS(2) - WRLQ-AUTO-REFS(2)).
           ADD WRTM-AUTO-REFS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 23: 522 MISC REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 23 TO SUB.
           ADD 0 TO T-MTD(TOT-SUB,SUB).
           ADD 0 TO T-QTD(TOT-SUB,SUB).
           ADD 0 TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 24:  523 TOTAL REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 24 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                        T-MTD(TOT-SUB,21) + T-MTD(TOT-SUB,22) +
                        T-MTD(TOT-SUB,23).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                        T-QTD(TOT-SUB,21) + T-QTD(TOT-SUB,22) +
                        T-QTD(TOT-SUB,23).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                        T-YTD(TOT-SUB,21) + T-YTD(TOT-SUB,22) +
                        T-YTD(TOT-SUB,23).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 25: 524 NET PROP AND AUTO PREM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 25 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,20) - T-MTD(TOT-SUB,24).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,20) - T-QTD(TOT-SUB,24).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,20) - T-YTD(TOT-SUB,24).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 26: 525 NBR OF PROPERTY POLICIES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 26 TO SUB.
           ADD WRTM-NO-FIRE-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-FIRE-PREMS(2) - WRLQ-NO-FIRE-PREMS(2)).
           ADD WRTM-NO-FIRE-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 27: 526 NBR OF AUTO POLICIES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 27 TO SUB.
           ADD WRTM-NO-AUTO-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-AUTO-PREMS(2) - WRLQ-NO-AUTO-PREMS(2)).
           ADD WRTM-NO-AUTO-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 28: 527 PROPERTY CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 28 TO SUB.
           ADD WRTM-FIRE-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-FIRE-CLAIMS(2) - WRLQ-FIRE-CLAIMS(2)).
           ADD WRTM-FIRE-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 29: 528 AUTO CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 29 TO SUB.
           ADD WRTM-AUTO-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-AUTO-CLAIMS(2) - WRLQ-AUTO-CLAIMS(2)).
           ADD WRTM-AUTO-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 30: 529 PERCENT PROP CLAIMS
      *              PROP CLAIMS (527) / PROP PREMIUMS (517)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 30 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                (T-MTD(TOT-SUB,28) / T-MTD(TOT-SUB,18) ) * 100.
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED =
                (T-QTD(TOT-SUB,28) / T-QTD(TOT-SUB,18) ) * 100.
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
                (T-YTD(TOT-SUB,28) / T-YTD(TOT-SUB,18) ) * 100.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 31: 530 PERCENT AUTO CLAIMS
      *              AUTO CLAIMS (528) / AUTO PREMIUMS (518)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 31 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
               (T-MTD(TOT-SUB,29) / T-MTD(TOT-SUB,19) ) * 100.
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED =
               (T-QTD(TOT-SUB,29) / T-QTD(TOT-SUB,19) ) * 100.
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
               (T-YTD(TOT-SUB,29) / T-YTD(TOT-SUB,19) ) * 100.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 32: 531 NBR PROPERTY CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 32 TO SUB.
           ADD WRTM-NO-FIRE-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-FIRE-CLAIMS(2) - WRLQ-NO-FIRE-CLAIMS(2)).
           ADD WRTM-NO-FIRE-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 33: 532 NBR AUTO CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 33 TO SUB
           ADD WRTM-NO-AUTO-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-AUTO-CLAIMS(2) - WRLQ-NO-AUTO-CLAIMS(2)).
           ADD WRTM-NO-AUTO-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 34: 533 NON-FILE WRITTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 34 TO SUB.
           ADD WRTM-NON-FILE-PREMS(1)      TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NON-FILE-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
            (WRTM-NON-FILE-PREMS(2) - WRLQ-NON-FILE-PREMS(2)) +
            (WRTM-LOTS-NON-FILE-PREMS(2) - WRLQ-LOTS-NON-FILE-PREMS(2)).
           ADD WRTM-NON-FILE-PREMS(2)      TO T-YTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NON-FILE-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 35: 534 NON-FILE RECOVERIES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 35 TO SUB.
           ADD WRTM-NF-RECOVERIES(1)      TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NF-RECOVERIES(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
             (WRTM-NF-RECOVERIES(2) - WRLQ-NF-RECOVERIES(2)) +
             (WRTM-LOTS-NF-RECOVERIES(2) - WRLQ-LOTS-NF-RECOVERIES(2)).
           ADD WRTM-NF-RECOVERIES(2)      TO T-YTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NF-RECOVERIES(2) TO T-YTD(TOT-SUB,SUB).


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 36: 535 TOTAL NON-FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 36 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,34) + T-MTD(TOT-SUB,35).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,34) + T-QTD(TOT-SUB,35).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,34) + T-YTD(TOT-SUB,35).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 37: 536 NF CLAIMS-BANKRUPT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 37 TO SUB
           ADD WRTM-NF-BK-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NF-BK-CLAIMS(2)
                - WRLQ-NF-BK-CLAIMS(2)).
           ADD WRTM-NF-BK-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 38: 537 NBR NF CLAIMS-BR
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 38 TO SUB.
           ADD WRTM-NO-NF-BK-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-NF-BK-CLAIMS(2) - WRLQ-NO-NF-BK-CLAIMS(2)).
           ADD WRTM-NO-NF-BK-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 39: 538 NF CLAIMS-NON BANKRUPT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 39 TO SUB.
           ADD WRTM-NF-NON-BK-CLAIMS(1)      TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NF-CLAIMS(1)        TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                (WRTM-NF-NON-BK-CLAIMS(2) - WRLQ-NF-NON-BK-CLAIMS(2)) +
                (WRTM-LOTS-NF-CLAIMS(2) - WRLQ-LOTS-NF-CLAIMS(2)).
           ADD WRTM-NF-NON-BK-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NF-CLAIMS(2)   TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 40: 539 NBR NF CLAIMS-NON BR
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 40 TO SUB.
           ADD WRTM-NO-NF-NON-BK-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NO-NF-CLAIMS(1)   TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
           (WRTM-NO-NF-NON-BK-CLAIMS(2) - WRLQ-NO-NF-NON-BK-CLAIMS(2)) +
           (WRTM-LOTS-NO-NF-CLAIMS(2) - WRLQ-LOTS-NO-NF-CLAIMS(2)).
           ADD WRTM-NO-NF-NON-BK-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NO-NF-CLAIMS(2)   TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 41: 540 TOTAL NF CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 41 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,37) + T-MTD(TOT-SUB,39).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,37) + T-QTD(TOT-SUB,39).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,37) + T-YTD(TOT-SUB,39).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 42: 541 TOTAL NO NF CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 42 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,38) + T-MTD(TOT-SUB,40).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,38) + T-QTD(TOT-SUB,40).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,38) + T-YTD(TOT-SUB,40).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 43: 542 % LIFE CLAIMS-NET PREM
      *           CLAIMS (510) / (PREMS - REFS)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 43 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED = ( T-MTD(TOT-SUB,11) /
                  (T-MTD(TOT-SUB,1) - T-MTD(TOT-SUB,4) ) * 100).
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED = ( T-QTD(TOT-SUB,11) /
                  (T-QTD(TOT-SUB,1) - T-QTD(TOT-SUB,4) ) * 100).
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED = ( T-YTD(TOT-SUB,11) /
                  (T-YTD(TOT-SUB,1) - T-YTD(TOT-SUB,4) ) * 100).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 44: 543 % A-H CLAIMS-NET PREM
      *           CLAIMS  / (PREMS - REFS)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 44 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED = ( T-MTD(TOT-SUB,12) /
                  (T-MTD(TOT-SUB,2) - T-MTD(TOT-SUB,5) ) * 100).
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED = ( T-QTD(TOT-SUB,12) /
                  (T-QTD(TOT-SUB,2) - T-QTD(TOT-SUB,5) ) * 100).
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED = ( T-YTD(TOT-SUB,12) /
                  (T-YTD(TOT-SUB,2) - T-YTD(TOT-SUB,5) ) * 100).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 45: 544 % PROP CLAIMS-NET-PREM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 45 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED = ( T-MTD(TOT-SUB,28) /
                  (T-MTD(TOT-SUB,18) - T-MTD(TOT-SUB,21) ) * 100).
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED = ( T-QTD(TOT-SUB,28) /
                  (T-QTD(TOT-SUB,18) - T-QTD(TOT-SUB,21) ) * 100).
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED = ( T-YTD(TOT-SUB,28) /
                  (T-YTD(TOT-SUB,18) - T-YTD(TOT-SUB,21) ) * 100).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 46: 450 % AUTO CLAIMS-NET PREM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 46 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED = ( T-MTD(TOT-SUB,29) /
                  (T-MTD(TOT-SUB,19) - T-MTD(TOT-SUB,22) ) * 100).
           COMPUTE T-QTD(TOT-SUB,SUB) ROUNDED = ( T-QTD(TOT-SUB,29) /
                  (T-QTD(TOT-SUB,19) - T-QTD(TOT-SUB,22) ) * 100).
           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED = ( T-YTD(TOT-SUB,29) /
                  (T-YTD(TOT-SUB,19) - T-YTD(TOT-SUB,22) ) * 100).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 47: 451 UNEMPLOYMENT PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 47 TO SUB.
           ADD WRTM-UNEMP-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                     ( WRTM-UNEMP-PREMS(2) - WRLQ-UNEMP-PREMS(2)).
           ADD WRTM-UNEMP-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 48: 452 UNEMPLOYMENT REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 48 TO SUB.
           ADD WRTM-UNEMP-REFS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                       ( WRTM-UNEMP-REFS(2) - WRLQ-UNEMP-REFS(2)).
           ADD WRTM-UNEMP-REFS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 49: 453 NET UNEMPL PREMIUMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 49 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,47) - T-MTD(TOT-SUB,48).
           COMPUTE T-QTD(TOT-SUB,SUB) =
                   T-QTD(TOT-SUB,47) - T-QTD(TOT-SUB,48).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,47) - T-YTD(TOT-SUB,48).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 50: 454 UNEMPLOYMENT CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 50 TO SUB.
           ADD WRTM-UNEMP-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-UNEMP-CLAIMS(2) - WRLQ-UNEMP-CLAIMS(2)).
           ADD WRTM-UNEMP-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 51: 455 NBR UNEMPL POLICIES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 51 TO SUB.
           ADD WRTM-NO-UNEMP-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-UNEMP-PREMS(2) - WRLQ-NO-UNEMP-PREMS(2)).
           ADD WRTM-NO-UNEMP-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 52: 456 NBR UNEMPL CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 52 TO SUB.
           ADD WRTM-NO-UNEMP-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-UNEMP-CLAIMS(2) - WRLQ-NO-UNEMP-CLAIMS(2)).
           ADD WRTM-NO-UNEMP-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 53: 457 NBR A D & D POLICIES WRITTEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 53 TO SUB.
           ADD WRTM-NO-AD-D-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-AD-D-PREMS(2) - WRLQ-NO-AD-D-PREMS(2)).
           ADD WRTM-NO-AD-D-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 54: 458 AD & D PREMIUMS WRITTEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 54 TO SUB.
           ADD WRTM-AD-D-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                     ( WRTM-AD-D-PREMS(2) - WRLQ-AD-D-PREMS(2)).
           ADD WRTM-AD-D-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 55: 459 A D & D CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 55 TO SUB.
           ADD WRTM-AD-D-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-AD-D-CLAIMS(2) - WRLQ-AD-D-CLAIMS(2)).
           ADD WRTM-AD-D-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 56: 460 NBR A D & D CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 56 TO SUB.
           ADD WRTM-NO-AD-D-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-AD-D-CLAIMS(2) - WRLQ-NO-AD-D-CLAIMS(2)).
           ADD WRTM-NO-AD-D-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 57: 461 MEMBERSHIP INCOME $
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 57 TO SUB.
           ADD WRTM-ROAD-GARD-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-ROAD-GARD-PREMS(2) - WRLQ-ROAD-GARD-PREMS(2)).
           ADD WRTM-ROAD-GARD-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 58: 462 NBR MEMBERSHIPS SOLD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 58 TO SUB.
           ADD WRTM-NO-ROAD-GARD-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
              ( WRTM-NO-ROAD-GARD-PREMS(2) - WRLQ-NO-ROAD-GARD-PREMS(2)).
           ADD WRTM-NO-ROAD-GARD-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 59: 463 MEMBERSHIP CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 59 TO SUB.
           ADD WRTM-ROAD-GARD-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-ROAD-GARD-CLAIMS(2) - WRLQ-ROAD-GARD-CLAIMS(2)).
           ADD WRTM-ROAD-GARD-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 60: 464 NBR MEMBERSHIP CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 60 TO SUB.
           ADD WRTM-NO-ROAD-GARD-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
            (WRTM-NO-ROAD-GARD-CLAIMS(2) - WRLQ-NO-ROAD-GARD-CLAIMS(2)).
           ADD WRTM-NO-ROAD-GARD-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 61: 465 MEMBERSHIP REFUNDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 61 TO SUB.
           ADD WRTM-ROAD-GARD-REFS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                   ( WRTM-ROAD-GARD-REFS(2) - WRLQ-ROAD-GARD-REFS(2)).
           ADD WRTM-ROAD-GARD-REFS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 62: 466 NBR MEMBERSHIP REFUNDS ??????? DONT CARE ???
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 62 TO SUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 63: 467 LP PREMIUMS WRITTEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 63 TO SUB.
           ADD WRTM-L-PROT-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                     ( WRTM-L-PROT-PREMS(2) - WRLQ-L-PROT-PREMS(2)).
           ADD WRTM-L-PROT-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 64: 468 LP REFUNDS WRITTEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 64 TO SUB.
           ADD WRTM-L-PROT-REFS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                   ( WRTM-L-PROT-REFS(2) - WRLQ-L-PROT-REFS(2)).
           ADD WRTM-L-PROT-REFS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 65: 469 NBR LP POLICIES WRITTEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 65 TO SUB.
           ADD WRTM-NO-L-PROT-PREMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-NO-L-PROT-PREMS(2) - WRLQ-NO-L-PROT-PREMS(2)).
           ADD WRTM-NO-L-PROT-PREMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 66: 470 LP CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 66 TO SUB.
           ADD WRTM-L-PROT-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
                ( WRTM-L-PROT-CLAIMS(2) - WRLQ-L-PROT-CLAIMS(2)).
           ADD WRTM-L-PROT-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 67: 471 NBR LP CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 67 TO SUB.
           ADD WRTM-NO-L-PROT-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           COMPUTE T-QTD(TOT-SUB,SUB) = T-QTD(TOT-SUB,SUB) +
             ( WRTM-NO-L-PROT-CLAIMS(2) - WRLQ-NO-L-PROT-CLAIMS(2)).
           ADD WRTM-NO-L-PROT-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).

       EXIT-ADD-TOTALS.

      * RESTORE THIS MONTH RECORD AND RESTART FILE:
           MOVE WRTM-REC TO WR-REC.
           MOVE WR-YYYYMM  TO QWR1-WBEG-YYYYMM
                              QWR1-WEND-YYYYMM.
           MOVE WR-BRNO    TO QWR1-WBEG-BRNO
                              QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.  *> SSP WR
           PERFORM READ-WR1-FILE-NEXT.  *> EMBEDDED WR

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE ISTATS-QUEUE-POS TO Q-POS.
           MOVE ISTATS-COPIES-POS TO C-POS.
           MOVE ISTATS-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
           PERFORM OPEN-BR-FILE.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.

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

           PERFORM FIND-CURRENT-PERIOD.

           PERFORM FIND-LAST-QUARTER-DATE.

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
           MOVE EXT-EOBUF-DATE2 TO NUM-DATE.
           MOVE NUM-MO TO STORE-MO CURRENT-MM.
           MOVE NUM-YR TO STORE-YR CURRENT-YY.
           MOVE NUM-CC TO CURRENT-CC.

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
           MOVE STORE-MO TO NUM-MO CURRENT-MM.
           MOVE STORE-YR TO NUM-YR CURRENT-YY.
           IF NUM-YR < EXT-JULIAN-CC
              MOVE 20 TO CURRENT-CC
           ELSE
              MOVE 19 TO CURRENT-CC.

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
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO MESS.
           MOVE MESS TO WR-BUF.
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
       TOTAL-LEVEL-CHECK SECTION.
      ******************************************************************

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

       DO-GROUP-TOTALS.
           MOVE "GROUP: " TO HEAD3-TYPE.
           MOVE GR-ID     TO H-GRID.
           MOVE GR-NAME   TO H-GRNAME.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.
           GO TO CONTINUE-LOOP.

       CHECK-BRANCH-SID.
           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE 9 TO TOT-SUB.
           MOVE "BRANCH: " TO HEAD3-TYPE.
           MOVE BR-NO      TO H-BRNO.
           MOVE BR-NAME    TO H-GRNAME.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      ******************************************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      ******************************************************************
           IF LOCAL-PRINT-FG = "Y"
              MOVE 0 TO PAGE-NUMBER
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO EXIT-PRINT-ONE-BRANCH.

           MOVE 9 TO TOT-SUB.
           MOVE "BRANCH: " TO HEAD3-TYPE.
           MOVE BR-NO      TO H-BRNO.
           MOVE BR-NAME    TO H-GRNAME
           PERFORM HEAD-PAGE.
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
           MOVE " "             TO HEAD3-TYPE.
           MOVE 0               TO H-BRNO.
           MOVE "GRAND TOTALS:" TO H-GRNAME.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

       EXIT-PRINT-ONE-GRAND.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************

           MOVE 01               TO SUB.
           MOVE 500 TO D-LINENO.
           MOVE "LIFE PREMIUMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 02               TO SUB
           MOVE 501 TO D-LINENO.
           MOVE "A&H  PREMIUMS " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 03               TO SUB.
           MOVE 502 TO D-LINENO.
           MOVE "  TOTAL LIFE, A&H PREMIUMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 04            TO SUB.
           MOVE 503 TO D-LINENO.
           MOVE "LIFE REFUNDS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 05                  TO SUB.
           MOVE 504 TO D-LINENO.
           MOVE "A&H  REFUNDS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 06                     TO SUB.
           MOVE 505 TO D-LINENO.
           MOVE "MISC REFUNDS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 07                  TO SUB.
           MOVE 506 TO D-LINENO.
           MOVE "  TOTAL REFUNDS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 08                   TO SUB.
           MOVE 507 TO D-LINENO.
           MOVE "  NET LIFE, A&H PREMIUMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 09                  TO SUB.
           MOVE 508 TO D-LINENO.
           MOVE "NBR LIFE POLICIES WRITTEN" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 10                 TO SUB.
           MOVE 509 TO D-LINENO.
           MOVE "NBR A&H  POLICIES WRITTEN" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 11                   TO SUB.
           MOVE 510 TO D-LINENO.
           MOVE "LIFE CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 12               TO SUB.
           MOVE 511 TO D-LINENO.
           MOVE "A&H  CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 13            TO SUB.
           MOVE 512 TO D-LINENO.
           MOVE "  TOTAL LIFE, A&H CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 14                    TO SUB.
           MOVE 513 TO D-LINENO.
           MOVE "PERCENT LIFE CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 15            TO SUB.
           MOVE 514 TO D-LINENO.
           MOVE "PERCENT A&H  CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 16                    TO SUB.
           MOVE 515 TO D-LINENO.
           MOVE "NBR OF LIFE CLAIMS" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 17                        TO SUB.
           MOVE 516 TO D-LINENO.
           MOVE "NBR OF A&H  CLAIMS" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 18          TO SUB.
           MOVE 517 TO D-LINENO.
           MOVE "PROPERTY PREMIUMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 19                  TO SUB.
           MOVE 518 TO D-LINENO.
           MOVE "AUTO PREMIUMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 20                      TO SUB.
           MOVE 519 TO D-LINENO.
           MOVE "  TOTAL PROP AND AUTO PREM" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 21                    TO SUB.
           MOVE 520 TO D-LINENO.
           MOVE "PROPERTY REFUNDS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 22                      TO SUB.
           MOVE 521 TO D-LINENO.
           MOVE "AUTO REFUNDS " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 23                      TO SUB.
           MOVE 522 TO D-LINENO.
           MOVE "MISC REFUNDS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 24                        TO SUB.
           MOVE 523 TO D-LINENO.
           MOVE "  TOTAL REFUNDS     " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 25                        TO SUB.
           MOVE 524 TO D-LINENO.
           MOVE "  NET PROP AND AUTO PREM" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 26                      TO SUB.
           MOVE 525 TO D-LINENO.
           MOVE "NBR OF PROPERTY POLICIES" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 27                        TO SUB.
           MOVE 526 TO D-LINENO.
           MOVE "NBR OF AUTO POLICIES  " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 28                        TO SUB.
           MOVE 527 TO D-LINENO.
           MOVE "PROPERTY CLAIMS  " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 29                      TO SUB.
           MOVE 528 TO D-LINENO.
           MOVE "AUTO CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 30                        TO SUB.
           MOVE 529 TO D-LINENO.
           MOVE "PERCENT PROP CLAIMS " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 31                        TO SUB.
           MOVE 530 TO D-LINENO.
           MOVE "PERCENT AUTO CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 32                        TO SUB.
           MOVE 531 TO D-LINENO.
           MOVE "NBR PROPERTY CLAIMS" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 33                        TO SUB.
           MOVE 532 TO D-LINENO.
           MOVE "NBR AUTO CLAIMS     " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 34                        TO SUB.
           MOVE 533 TO D-LINENO.
           MOVE "NON-FILE WRITTEN  " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 35                        TO SUB.
           MOVE 534 TO D-LINENO.
           MOVE "NON-FILE RECOVERIES   " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 36                        TO SUB.
           MOVE 535 TO D-LINENO.
           MOVE "  TOTAL NON-FILE      " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 37                        TO SUB.
           MOVE 536 TO D-LINENO.
           MOVE "NF CLAIMS-BANKRUPT" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 38                        TO SUB.
           MOVE 537 TO D-LINENO.
           MOVE "NBR NF CLAIMS-BANKRUPT " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 39                        TO SUB.
           MOVE 538 TO D-LINENO.
           MOVE "NF CLAIMS-NON BANKRUPT" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 40                        TO SUB.
           MOVE 539 TO D-LINENO.
           MOVE "NBR NF CLAIMS-NON BANKRUPT" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 41                        TO SUB.
           MOVE 540 TO D-LINENO.
           MOVE "  TOTAL NF CLAIMS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 42                        TO SUB.
           MOVE 541 TO D-LINENO.
           MOVE "  TOTAL NO NF CLAIMS   " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 43                        TO SUB.
           MOVE 542 TO D-LINENO.
           MOVE "% LIFE CLAIMS-NET PREMIUM  " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 44                        TO SUB.
           MOVE 543 TO D-LINENO.
           MOVE "% A&H  CLAIMS-NET PREMIUM " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 45                        TO SUB.
           MOVE 544 TO D-LINENO.
           MOVE "% PROP CLAIMS-NET PREMIUM    " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 46                        TO SUB.
           MOVE 450 TO D-LINENO.
           MOVE "% AUTO CLAIMS-NET PREMIUM " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE "%" TO D-MTD-P D-QTD-P D-YTD-P.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 47                        TO SUB.
           MOVE 451 TO D-LINENO.
           MOVE "UNEMPLOYMENT PREMIUMS " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 48                        TO SUB.
           MOVE 452 TO D-LINENO.
           MOVE "UNEMPLOYMENT REFUNDS" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 49                        TO SUB.
           MOVE 453 TO D-LINENO.
           MOVE "NET UNEMPL PREMIUMS " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 50                        TO SUB.
           MOVE 454 TO D-LINENO.
           MOVE "UNEMPLOYMENT CLAIMS " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 51                        TO SUB.
           MOVE 455 TO D-LINENO.
           MOVE "NBR UNEMPL POLICIES      " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 52                        TO SUB.
           MOVE 456 TO D-LINENO.
           MOVE "NBR UNEMPL CLAIMS     " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 53                        TO SUB.
           MOVE 457 TO D-LINENO.
           MOVE "NBR A D & D POLICIES WRITTEN" TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 54                        TO SUB.
           MOVE 458 TO D-LINENO.
           MOVE "A D & D PREMIUMS WRITTEN" TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 55                        TO SUB.
           MOVE 459 TO D-LINENO.
           MOVE "A D & D CLAIMS      " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 56                        TO SUB.
           MOVE 460 TO D-LINENO.
           MOVE "NBR A D & D CLAIMS      " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 57                        TO SUB.
           MOVE 461 TO D-LINENO.
           MOVE "MEMBERSHIP INCOME $      " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 58                        TO SUB.
           MOVE 462 TO D-LINENO.
           MOVE "NBR MEMBERSHIPS SOLD  " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 59                        TO SUB.
           MOVE 463 TO D-LINENO.
           MOVE "MEMBERSHIP CLAIMS     " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 60                        TO SUB.
           MOVE 464 TO D-LINENO.
           MOVE "NBR MEMBERSHIP CLAIMS     " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 61                        TO SUB.
           MOVE 465 TO D-LINENO.
           MOVE "MEMBERSHIP REFUNDS    " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 62                        TO SUB.
           MOVE 466 TO D-LINENO.
           MOVE "NBR MEMBERSHIP REFUNDS     " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 63                        TO SUB.
           MOVE 467 TO D-LINENO.
           MOVE "LP PREMIUMS WRITTEN     " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 64                        TO SUB.
           MOVE 468 TO D-LINENO.
           MOVE "LP REFUNDS     " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 65                        TO SUB.
           MOVE 469 TO D-LINENO.
           MOVE "NBR LP POLICIES WRITTEN " TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 66                        TO SUB.
           MOVE 470 TO D-LINENO.
           MOVE "LP CLAIMS     " TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 67                        TO SUB.
           MOVE 471 TO D-LINENO.
           MOVE "NBR LP CLAIMS " TO D-DESC.
           PERFORM MOVE-NUMBERS.
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

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
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
           MOVE 2 TO LN-FEED.
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
           MOVE "REPORT SELECTION:" TO H-GRNAME.

           MOVE "GROUP "  TO RANGE-LINE.
           MOVE ENT-GROUP TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BRANCH         TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING BRANCH" TO RANGE-LINE.
           MOVE END-BRANCH      TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBSP/SPWRCL.CPY".

      *****************************
       FIND-CURRENT-PERIOD SECTION.
      *****************************
           MOVE 0 TO CUR-PERIOD SUB.
       NEXT-CURRENT-PERIOD.
           ADD 1 TO SUB.
           IF SUB > 12
              GO TO EXIT-FIND-CURRENT-PERIOD.

           MOVE GP-END-FISCAL-DATE(SUB) TO DATE-YYYYMMDD.
           IF STORE-MO = DATE-YYYYMMDD-MM
              MOVE SUB TO CUR-PERIOD
              GO TO EXIT-FIND-CURRENT-PERIOD.

           GO TO NEXT-CURRENT-PERIOD.
       EXIT-FIND-CURRENT-PERIOD.
           EXIT.

      ***********************************
       FIND-LAST-QUARTER-DATE SECTION.
      ***********************************
           IF CUR-PERIOD > 9
              MOVE 9 TO SUB
           ELSE
           IF CUR-PERIOD > 6
              MOVE 6 TO SUB
           ELSE
           IF CUR-PERIOD > 3
              MOVE 3 TO SUB
           ELSE
              MOVE 12 TO SUB.

           MOVE GP-END-FISCAL-DATE(SUB) TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM        TO LAST-QTR-MONTH.
           MOVE STORE-YR TO LAST-QTR-YEAR.
           IF LAST-QTR-MONTH > 9
              SUBTRACT 1 FROM LAST-QTR-YEAR.
           IF LAST-QTR-YEAR < EXT-JULIAN-CC
              ADD 2000 TO LAST-QTR-YEAR
           ELSE
              ADD 1900 TO LAST-QTR-YEAR.



      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO ISTATS-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ISTATS-SYSDATE.
           DISPLAY ISTATS-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ISTATS-FORM-FIELDS TO WR-BUF.
           MOVE ISTATS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE ISTATS-HELP--FILE TO HELP-LINK-FILE.
           MOVE ISTATS-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ISTATS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       MOVE-NUMBERS.
           MOVE T-MTD(TOT-SUB,SUB) TO D-MTD-NO.
           MOVE T-QTD(TOT-SUB,SUB) TO D-QTD-NO.
           MOVE T-YTD(TOT-SUB,SUB) TO D-YTD-NO.
       MOVE-DOLLARS.
           MOVE T-MTD(TOT-SUB,SUB) TO D-MTD.
           MOVE T-QTD(TOT-SUB,SUB) TO D-QTD.
           MOVE T-YTD(TOT-SUB,SUB) TO D-YTD.
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
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".


      * COPY "LIBSP/SPWR1RN.CPY".
      * COPYMEMBER: LIBSP/SPWR1RN
       LOAD-WR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( WR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( WR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO WR-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO WR-PATH-MACHINE
              MOVE WR-PATH-FORMAT          TO WR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( WR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO WR-PATH-BASE
              MOVE FILPATH-MACHINE         TO WR-PATH-MACHINE
              MOVE WR-PATH-FORMAT          TO WR-PATH.

      *-----------------------------------------------------------------
       OPEN-WR1-FILE.
           PERFORM LOAD-WR1-FILE.
           PERFORM OPEN-IT.
           MOVE WR-PATH-SQL TO E-FILE.

           IF WR1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS WR1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO WR1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-WR1-FILE.  *> SSP WR
           PERFORM START-IT.
           MOVE WR-PATH-SQL TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.

           IF ( WR1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-WR1-FILE.

           EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE WR1_CURSOR CURSOR FOR
                :WR1-RETURN-CODE = 
                 EXEC SSP_ISTATS_WRFILE_SELECT
                  (:QWR1-WBEG-YYYYMM, 
                    :QWR1-WBEG-BRNO) 
                END-EXEC.

      *     SELECT WRFILE.WR_YYYYMM,
      *            WRFILE.WR_BRNO,
      *            CAST(WRFILE.WR_LTOUCH_DATE AS VARCHAR(10)), 
      *            WRFILE.WR_CUR_NO_CUST,
      *            WRFILE.WR_ELM_NO_CUST,
      *            WRFILE.WR_ELY_NO_CUST,
      *            WRFILE.WR_OUTS_BALANCE,
      *            WRFILE.WR_ELM_OUTS_BALANCE,
      *            WRFILE.WR_ELY_OUTS_BALANCE,
      *            WRFILE.WR_NO_NB_1,
      *            WRFILE.WR_NO_NB_2,
      *            WRFILE.WR_NO_FB_1,
      *            WRFILE.WR_NO_FB_2,
      *            WRFILE.WR_NO_PB_1,
      *            WRFILE.WR_NO_PB_2,
      *            WRFILE.WR_GROSS_VOLUME_1,
      *            WRFILE.WR_GROSS_VOLUME_2,
      *            WRFILE.WR_BSR_BONUS_1,
      *            WRFILE.WR_BSR_BONUS_2,
      *            WRFILE.WR_INTEREST_ALLOCATION_1,
      *            WRFILE.WR_INTEREST_ALLOCATION_2,
      *            WRFILE.WR_GEN_RESERVE_ADJ_1,
      *            WRFILE.WR_GEN_RESERVE_ADJ_2,
      *            WRFILE.WR_INTANGIBLE_AMORT_1,
      *            WRFILE.WR_INTANGIBLE_AMORT_2,
      *            WRFILE.WR_ROBBERIES_1,
      *            WRFILE.WR_ROBBERIES_2,
      *            WRFILE.WR_RATING_SHEET_INCOME_1,
      *            WRFILE.WR_RATING_SHEET_INCOME_2,
      *            WRFILE.WR_INT_FEE_INCOME_1,
      *            WRFILE.WR_INT_FEE_INCOME_2,
      *            WRFILE.WR_INSURANCE_INCOME_1,
      *            WRFILE.WR_INSURANCE_INCOME_2,
      *            WRFILE.WR_RATING_SHEET_EXPENSES_1,
      *            WRFILE.WR_RATING_SHEET_EXPENSES_2,
      *            WRFILE.WR_PERSONNEL_EXPENSE_1,
      *            WRFILE.WR_PERSONNEL_EXPENSE_2,
      *            WRFILE.WR_CHARGEOFF_EXPENSE_1,
      *            WRFILE.WR_CHARGEOFF_EXPENSE_2,
      *            WRFILE.WR_NO_CHARGEOFFS_1,
      *            WRFILE.WR_NO_CHARGEOFFS_2,
      *            WRFILE.WR_RECOVERY_INCOME_1,
      *            WRFILE.WR_RECOVERY_INCOME_2,
      *            WRFILE.WR_BAD_DEBT_INCREASE_1,
      *            WRFILE.WR_BAD_DEBT_INCREASE_2,
      *            WRFILE.WR_NO_30_DAY_ACCTS,
      *            WRFILE.WR_AMT_30_DAY_ACCTS,
      *            WRFILE.WR_NO_60_DAY_ACCTS,
      *            WRFILE.WR_AMT_60_DAY_ACCTS,
      *            WRFILE.WR_NO_90_DAY_ACCTS,
      *            WRFILE.WR_AMT_90_DAY_ACCTS,
      *            WRFILE.WR_ELM_AMT_90_DAY_ACCTS,
      *            WRFILE.WR_ELY_AMT_90_DAY_ACCTS,
      *            WRFILE.WR_YTD_PCT_TURNOVER,
      *            WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT,
      *            WRFILE.WR_YTD_PCT_RS_INC_LEDGER,
      *            WRFILE.WR_YTD_PCT_INT_FEE_LEDGER,
      *            WRFILE.WR_YTD_PCT_INS_INC_LEDGER,
      *            WRFILE.WR_YTD_PCT_EXPENSE_LEDGER,
      *            WRFILE.WR_YTD_PCT_BAD_DEBT_LEDGER,
      *            WRFILE.WR_YTD_PCT_NF_BNK_LEDGER,
      *            WRFILE.WR_YTD_PCT_PRETAX_LEDGER,
      *            WRFILE.WR_YTD_PCT_DEBT_NF_LEDGER,
      *            WRFILE.WR_WCBC_CUR_NO_CUST,
      *            WRFILE.WR_WCBC_ELM_NO_CUST,
      *            WRFILE.WR_WCBC_ELY_NO_CUST,
      *            WRFILE.WR_WCBC_OUTS_BALANCE,
      *            WRFILE.WR_WCBC_ELM_OUTS_BAL,
      *            WRFILE.WR_WCBC_ELY_OUTS_BAL,
      *            WRFILE.WR_WCBC_NO_NB_1,
      *            WRFILE.WR_WCBC_NO_NB_2,
      *            WRFILE.WR_WCBC_NO_FB_1,
      *            WRFILE.WR_WCBC_NO_FB_2,
      *            WRFILE.WR_WCBC_NO_PB_1,
      *            WRFILE.WR_WCBC_NO_PB_2,
      *            WRFILE.WR_WCBC_GROSS_VOLUME_1,
      *            WRFILE.WR_WCBC_GROSS_VOLUME_2,
      *            WRFILE.WR_WCBC_LATE_CHGS_1,
      *            WRFILE.WR_WCBC_LATE_CHGS_2,
      *            WRFILE.WR_WCBC_NET_FIN_CHGS_1,
      *            WRFILE.WR_WCBC_NET_FIN_CHGS_2,
      *            WRFILE.WR_WCBC_SALES_1,
      *            WRFILE.WR_WCBC_SALES_2,
      *            WRFILE.WR_WCBC_COST_OF_SALES_1,
      *            WRFILE.WR_WCBC_COST_OF_SALES_2,
      *            WRFILE.WR_WCBC_EXPENSES_1,
      *            WRFILE.WR_WCBC_EXPENSES_2,
      *            WRFILE.WR_WCBC_RECOVERY_INCOME_1,
      *            WRFILE.WR_WCBC_RECOVERY_INCOME_2,
      *            WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_1,
      *            WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_2,
      *            WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_1,
      *            WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_2,
      *            WRFILE.WR_WCBC_NO_30_DAY_ACCTS,
      *            WRFILE.WR_WCBC_AMT_30_DAY_ACCTS,
      *            WRFILE.WR_WCBC_NO_60_DAY_ACCTS,
      *            WRFILE.WR_WCBC_AMT_60_DAY_ACCTS,
      *            WRFILE.WR_WCBC_NO_90_DAY_ACCTS,
      *            WRFILE.WR_WCBC_AMT_90_DAY_ACCTS,
      *            WRFILE.WR_WCBC_ELM_AMT_90_DAYS,
      *            WRFILE.WR_WCBC_ELY_AMT_90_DAYS,
      *            WRFILE.WR_WCBC_PCT_VOLUME,
      *            WRFILE.WR_WCBC_GROSS_PROF_PCT,
      *            WRFILE.WR_LARGE_CUR_NO_CUST,
      *            WRFILE.WR_LARGE_ELM_NO_CUST,
      *            WRFILE.WR_LARGE_ELY_NO_CUST,
      *            WRFILE.WR_LARGE_OUTS_BALANCE,
      *            WRFILE.WR_LARGE_ELM_OUTS_BAL,
      *            WRFILE.WR_LARGE_ELY_OUTS_BAL,
      *            WRFILE.WR_LARGE_NO_NB_1,
      *            WRFILE.WR_LARGE_NO_NB_2,
      *            WRFILE.WR_LARGE_NO_FB_1,
      *            WRFILE.WR_LARGE_NO_FB_2,
      *            WRFILE.WR_LARGE_NO_PB_1,
      *            WRFILE.WR_LARGE_NO_PB_2,
      *            WRFILE.WR_LARGE_GROSS_VOLUME_1,
      *            WRFILE.WR_LARGE_GROSS_VOLUME_2,
      *            WRFILE.WR_LARGE_NO_30_DAY_ACCTS,
      *            WRFILE.WR_LARGE_AMT_30_DAY_ACCTS,
      *            WRFILE.WR_LARGE_NO_60_DAY_ACCTS,
      *            WRFILE.WR_LARGE_AMT_60_DAY_ACCTS,
      *            WRFILE.WR_LARGE_NO_90_DAY_ACCTS,
      *            WRFILE.WR_LARGE_AMT_90_DAY_ACCTS,
      *            WRFILE.WR_LARGE_ELM_AMT_90_DAYS,
      *            WRFILE.WR_LARGE_ELY_AMT_90_DAYS,
      *            WRFILE.WR_LIFE_CLAIMS_1,
      *            WRFILE.WR_LIFE_CLAIMS_2,
      *            WRFILE.WR_AH_CLAIMS_1,
      *            WRFILE.WR_AH_CLAIMS_2,
      *            WRFILE.WR_FIRE_CLAIMS_1,
      *            WRFILE.WR_FIRE_CLAIMS_2,
      *            WRFILE.WR_AUTO_CLAIMS_1,
      *            WRFILE.WR_AUTO_CLAIMS_2,
      *            WRFILE.WR_UNEMP_CLAIMS_1,
      *            WRFILE.WR_UNEMP_CLAIMS_2,
      *            WRFILE.WR_AD_D_CLAIMS_1,
      *            WRFILE.WR_AD_D_CLAIMS_2,
      *            WRFILE.WR_L_PROT_CLAIMS_1,
      *            WRFILE.WR_L_PROT_CLAIMS_2,
      *            WRFILE.WR_NF_NON_BK_CLAIMS_1,
      *            WRFILE.WR_NF_NON_BK_CLAIMS_2,
      *            WRFILE.WR_NF_BK_CLAIMS_1,
      *            WRFILE.WR_NF_BK_CLAIMS_2,
      *            WRFILE.WR_ROAD_GARD_CLAIMS_1,
      *            WRFILE.WR_ROAD_GARD_CLAIMS_2,
      *            WRFILE.WR_NO_LIFE_CLAIMS_1,
      *            WRFILE.WR_NO_LIFE_CLAIMS_2,
      *            WRFILE.WR_NO_AH_CLAIMS_1,
      *            WRFILE.WR_NO_AH_CLAIMS_2,
      *            WRFILE.WR_NO_FIRE_CLAIMS_1,
      *            WRFILE.WR_NO_FIRE_CLAIMS_2,
      *            WRFILE.WR_NO_AUTO_CLAIMS_1,
      *            WRFILE.WR_NO_AUTO_CLAIMS_2,
      *            WRFILE.WR_NO_UNEMP_CLAIMS_1,
      *            WRFILE.WR_NO_UNEMP_CLAIMS_2,
      *            WRFILE.WR_NO_AD_D_CLAIMS_1,
      *            WRFILE.WR_NO_AD_D_CLAIMS_2,
      *            WRFILE.WR_NO_L_PROT_CLAIMS_1,
      *            WRFILE.WR_NO_L_PROT_CLAIMS_2,
      *            WRFILE.WR_NO_NF_NON_BK_CLAIMS_1,
      *            WRFILE.WR_NO_NF_NON_BK_CLAIMS_2,
      *            WRFILE.WR_NO_NF_BK_CLAIMS_1,
      *            WRFILE.WR_NO_NF_BK_CLAIMS_2,
      *            WRFILE.WR_NO_ROAD_GARD_CLAIMS_1,
      *            WRFILE.WR_NO_ROAD_GARD_CLAIMS_2,
      *            WRFILE.WR_LIFE_PREMS_1,
      *            WRFILE.WR_LIFE_PREMS_2,
      *            WRFILE.WR_AH_PREMS_1,
      *            WRFILE.WR_AH_PREMS_2,
      *            WRFILE.WR_FIRE_PREMS_1,
      *            WRFILE.WR_FIRE_PREMS_2,
      *            WRFILE.WR_AUTO_PREMS_1,
      *            WRFILE.WR_AUTO_PREMS_2,
      *            WRFILE.WR_UNEMP_PREMS_1,
      *            WRFILE.WR_UNEMP_PREMS_2,
      *            WRFILE.WR_AD_D_PREMS_1,
      *            WRFILE.WR_AD_D_PREMS_2,
      *            WRFILE.WR_L_PROT_PREMS_1,
      *            WRFILE.WR_L_PROT_PREMS_2,
      *            WRFILE.WR_NON_FILE_PREMS_1,
      *            WRFILE.WR_NON_FILE_PREMS_2,
      *            WRFILE.WR_ROAD_GARD_PREMS_1,
      *            WRFILE.WR_ROAD_GARD_PREMS_2,
      *            WRFILE.WR_NO_LIFE_PREMS_1,
      *            WRFILE.WR_NO_LIFE_PREMS_2,
      *            WRFILE.WR_NO_AH_PREMS_1,
      *            WRFILE.WR_NO_AH_PREMS_2,
      *            WRFILE.WR_NO_FIRE_PREMS_1,
      *            WRFILE.WR_NO_FIRE_PREMS_2,
      *            WRFILE.WR_NO_AUTO_PREMS_1,
      *            WRFILE.WR_NO_AUTO_PREMS_2,
      *            WRFILE.WR_NO_UNEMP_PREMS_1,
      *            WRFILE.WR_NO_UNEMP_PREMS_2,
      *            WRFILE.WR_NO_AD_D_PREMS_1,
      *            WRFILE.WR_NO_AD_D_PREMS_2,
      *            WRFILE.WR_NO_L_PROT_PREMS_1,
      *            WRFILE.WR_NO_L_PROT_PREMS_2,
      *            WRFILE.WR_NO_NON_FILE_PREMS_1,
      *            WRFILE.WR_NO_NON_FILE_PREMS_2,
      *            WRFILE.WR_NO_ROAD_GARD_PREMS_1,
      *            WRFILE.WR_NO_ROAD_GARD_PREMS_2,
      *            WRFILE.WR_LIFE_REFS_1,
      *            WRFILE.WR_LIFE_REFS_2,
      *            WRFILE.WR_AH_REFS_1,
      *            WRFILE.WR_AH_REFS_2,
      *            WRFILE.WR_FIRE_REFS_1,
      *            WRFILE.WR_FIRE_REFS_2,
      *            WRFILE.WR_AUTO_REFS_1,
      *            WRFILE.WR_AUTO_REFS_2,
      *            WRFILE.WR_UNEMP_REFS_1,
      *            WRFILE.WR_UNEMP_REFS_2,
      *            WRFILE.WR_AD_D_REFS_1,
      *            WRFILE.WR_AD_D_REFS_2,
      *            WRFILE.WR_L_PROT_REFS_1,
      *            WRFILE.WR_L_PROT_REFS_2,
      *            WRFILE.WR_NON_FILE_REFS_1,
      *            WRFILE.WR_NON_FILE_REFS_2,
      *            WRFILE.WR_ROAD_GARD_REFS_1,
      *            WRFILE.WR_ROAD_GARD_REFS_2,
      *            WRFILE.WR_NF_RECOVERIES_1,
      *            WRFILE.WR_NF_RECOVERIES_2,
      *            WRFILE.WR_NONFILE_ENDING_BALANCE,
      *            WRFILE.WR_NO_CUST_1_YR_AGO,
      *            WRFILE.WR_OUTS_BAL_1_YR_AGO,
      *            WRFILE.WR_WCBC_NO_CUST_1_YR_AGO,
      *            WRFILE.WR_WCBC_OUTS_BAL_1_YR_AGO,
      *            WRFILE.WR_LARGE_NO_CUST_1_YR_AGO,
      *            WRFILE.WR_LARGE_OUTS_BAL_1_YR_AGO,
      *            WRFILE.WR_CUR_NO_CONT_SLOW,
      *            WRFILE.WR_WCBC_CUR_NO_CONT_SLOW,
      *            WRFILE.WR_LARGE_CUR_NO_CONT_SLOW,
      *            WRFILE.WR_CASHIER_BONUS,
      *            WRFILE.WR_ASST_MGR_BONUS,
      *            WRFILE.WR_MGR_BONUS,
      *            WRFILE.WR_LOTS_NF_CLAIMS_1,
      *            WRFILE.WR_LOTS_NF_CLAIMS_2,
      *            WRFILE.WR_LOTS_NO_NF_CLAIMS_1,
      *            WRFILE.WR_LOTS_NO_NF_CLAIMS_2,
      *            WRFILE.WR_LOTS_NON_FILE_PREMS_1,
      *            WRFILE.WR_LOTS_NON_FILE_PREMS_2,
      *            WRFILE.WR_LOTS_NO_NON_FILE_PREMS_1,
      *            WRFILE.WR_LOTS_NO_NON_FILE_PREMS_2,
      *            WRFILE.WR_LOTS_NF_RECOVERIES_1,
      *            WRFILE.WR_LOTS_NF_RECOVERIES_2,
      *            WRFILE.WR_PRETAX_PROFIT_1,
      *            WRFILE.WR_PRETAX_PROFIT_2,
      *            WRFILE.WR_SUPERVISION_1,
      *            WRFILE.WR_SUPERVISION_2
      *     FROM DBO.WRFILE WRFILE  
      *     WHERE WRFILE.WR_YYYYMM = @QWR1_WBEG_YYYYMM
      *      AND WRFILE.WR_BRNO   = @QWR1_WBEG_BRNO
      *      ORDER BY WRFILE.WR_YYYYMM,
      *               WRFILE.WR_BRNO

           EXEC SQL
               OPEN WR1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QWR1-WBEG-YYYYMM
                          QWR1-WEND-YYYYMM
                          QWR1-WEND-BRNO
                          QWR1-WEND-BRNO.

           MOVE STAT---GOOD TO WR1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-WR1-FILE.  *> EMBEDDED WR
           PERFORM READ-IT.
           MOVE WR-PATH-SQL TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           INITIALIZE QWR-REC.

           MOVE WR-YYYYMM   TO QWR-YYYYMM.
           MOVE WR-BRNO     TO QWR-BRNO.

           EXEC SQL
            SELECT WRFILE.WR_YYYYMM,
                   WRFILE.WR_BRNO,
                   CAST(WRFILE.WR_LTOUCH_DATE AS VARCHAR(10)),
                   WRFILE.WR_CUR_NO_CUST,
                   WRFILE.WR_ELM_NO_CUST,
                   WRFILE.WR_ELY_NO_CUST,
                   WRFILE.WR_OUTS_BALANCE,
                   WRFILE.WR_ELM_OUTS_BALANCE,
                   WRFILE.WR_ELY_OUTS_BALANCE,
                   WRFILE.WR_NO_NB_1,
                   WRFILE.WR_NO_FB_1,
                   WRFILE.WR_NO_PB_1,
                   WRFILE.WR_GROSS_VOLUME_1,
                   WRFILE.WR_BSR_BONUS_1,
                   WRFILE.WR_INTEREST_ALLOCATION_1,
                   WRFILE.WR_OLD_SUPERVISION_1,
                   WRFILE.WR_GEN_RESERVE_ADJ_1,
                   WRFILE.WR_INTANGIBLE_AMORT_1,
                   WRFILE.WR_ROBBERIES_1,
                   WRFILE.WR_RATING_SHEET_INCOME_1,
                   WRFILE.WR_INT_FEE_INCOME_1,
                   WRFILE.WR_INSURANCE_INCOME_1,
                   WRFILE.WR_RATING_SHEET_EXPENSES_1,
                   WRFILE.WR_PERSONNEL_EXPENSE_1,
                   WRFILE.WR_CHARGEOFF_EXPENSE_1,
                   WRFILE.WR_NO_CHARGEOFFS_1,
                   WRFILE.WR_RECOVERY_INCOME_1,
                   WRFILE.WR_BAD_DEBT_INCREASE_1,
                   WRFILE.WR_NO_NB_2,
                   WRFILE.WR_NO_FB_2,
                   WRFILE.WR_NO_PB_2,
                   WRFILE.WR_GROSS_VOLUME_2,
                   WRFILE.WR_BSR_BONUS_2,
                   WRFILE.WR_INTEREST_ALLOCATION_2,
                   WRFILE.WR_OLD_SUPERVISION_2,
                   WRFILE.WR_GEN_RESERVE_ADJ_2,
                   WRFILE.WR_INTANGIBLE_AMORT_2,
                   WRFILE.WR_ROBBERIES_2,
                   WRFILE.WR_RATING_SHEET_INCOME_2,
                   WRFILE.WR_INT_FEE_INCOME_2,
                   WRFILE.WR_INSURANCE_INCOME_2,
                   WRFILE.WR_RATING_SHEET_EXPENSES_2,
                   WRFILE.WR_PERSONNEL_EXPENSE_2,
                   WRFILE.WR_CHARGEOFF_EXPENSE_2,
                   WRFILE.WR_NO_CHARGEOFFS_2,
                   WRFILE.WR_RECOVERY_INCOME_2,
                   WRFILE.WR_BAD_DEBT_INCREASE_2,
                   WRFILE.WR_NO_30_DAY_ACCTS,
                   WRFILE.WR_AMT_30_DAY_ACCTS,
                   WRFILE.WR_NO_60_DAY_ACCTS,
                   WRFILE.WR_AMT_60_DAY_ACCTS,
                   WRFILE.WR_NO_90_DAY_ACCTS,
                   WRFILE.WR_AMT_90_DAY_ACCTS,
                   WRFILE.WR_ELM_AMT_90_DAY_ACCTS,
                   WRFILE.WR_ELY_AMT_90_DAY_ACCTS,
                   WRFILE.WR_YTD_PCT_TURNOVER,
                   WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT,
                   WRFILE.WR_YTD_PCT_RS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_INT_FEE_LEDGER,
                   WRFILE.WR_YTD_PCT_INS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_EXPENSE_LEDGER,
                   WRFILE.WR_YTD_PCT_BAD_DEBT_LEDGER,
                   WRFILE.WR_YTD_PCT_NF_BNK_LEDGER,
                   WRFILE.WR_YTD_PCT_PRETAX_LEDGER,
                   WRFILE.WR_YTD_PCT_DEBT_NF_LEDGER,
                   WRFILE.WR_WCBC_CUR_NO_CUST,
                   WRFILE.WR_WCBC_ELM_NO_CUST,
                   WRFILE.WR_WCBC_ELY_NO_CUST,
                   WRFILE.WR_WCBC_OUTS_BALANCE,
                   WRFILE.WR_WCBC_ELM_OUTS_BAL,
                   WRFILE.WR_WCBC_ELY_OUTS_BAL,
                   WRFILE.WR_WCBC_NO_NB_1,
                   WRFILE.WR_WCBC_NO_FB_1,
                   WRFILE.WR_WCBC_NO_PB_1,
                   WRFILE.WR_WCBC_GROSS_VOLUME_1,
                   WRFILE.WR_WCBC_LATE_CHGS_1,
                   WRFILE.WR_WCBC_NET_FIN_CHGS_1,
                   WRFILE.WR_WCBC_SALES_1,
                   WRFILE.WR_WCBC_COST_OF_SALES_1,
                   WRFILE.WR_WCBC_EXPENSES_1,
                   WRFILE.WR_WCBC_RECOVERY_INCOME_1,
                   WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_1,
                   WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_1,
                   WRFILE.WR_WCBC_NO_NB_2,
                   WRFILE.WR_WCBC_NO_FB_2,
                   WRFILE.WR_WCBC_NO_PB_2,
                   WRFILE.WR_WCBC_GROSS_VOLUME_2,
                   WRFILE.WR_WCBC_LATE_CHGS_2,
                   WRFILE.WR_WCBC_NET_FIN_CHGS_2,
                   WRFILE.WR_WCBC_SALES_2,
                   WRFILE.WR_WCBC_COST_OF_SALES_2,
                   WRFILE.WR_WCBC_EXPENSES_2,
                   WRFILE.WR_WCBC_RECOVERY_INCOME_2,
                   WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_2,
                   WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_2,
                   WRFILE.WR_WCBC_NO_30_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_30_DAY_ACCTS,
                   WRFILE.WR_WCBC_NO_60_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_60_DAY_ACCTS,
                   WRFILE.WR_WCBC_NO_90_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_90_DAY_ACCTS,
                   WRFILE.WR_WCBC_ELM_AMT_90_DAYS,
                   WRFILE.WR_WCBC_ELY_AMT_90_DAYS,
                   WRFILE.WR_WCBC_PCT_VOLUME,
                   WRFILE.WR_WCBC_GROSS_PROF_PCT,
                   WRFILE.WR_LARGE_CUR_NO_CUST,
                   WRFILE.WR_LARGE_ELM_NO_CUST,
                   WRFILE.WR_LARGE_ELY_NO_CUST,
                   WRFILE.WR_LARGE_OUTS_BALANCE,
                   WRFILE.WR_LARGE_ELM_OUTS_BAL,
                   WRFILE.WR_LARGE_ELY_OUTS_BAL,
                   WRFILE.WR_LARGE_NO_NB_1,
                   WRFILE.WR_LARGE_NO_FB_1,
                   WRFILE.WR_LARGE_NO_PB_1,
                   WRFILE.WR_LARGE_GROSS_VOLUME_1,
                   WRFILE.WR_LARGE_NO_NB_2,
                   WRFILE.WR_LARGE_NO_FB_2,
                   WRFILE.WR_LARGE_NO_PB_2,
                   WRFILE.WR_LARGE_GROSS_VOLUME_2,
                   WRFILE.WR_LARGE_NO_30_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_30_DAY_ACCTS,
                   WRFILE.WR_LARGE_NO_60_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_60_DAY_ACCTS,
                   WRFILE.WR_LARGE_NO_90_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_90_DAY_ACCTS,
                   WRFILE.WR_LARGE_ELM_AMT_90_DAYS,
                   WRFILE.WR_LARGE_ELY_AMT_90_DAYS,
                   WRFILE.WR_LIFE_CLAIMS_1,
                   WRFILE.WR_AH_CLAIMS_1,
                   WRFILE.WR_FIRE_CLAIMS_1,
                   WRFILE.WR_AUTO_CLAIMS_1,
                   WRFILE.WR_UNEMP_CLAIMS_1,
                   WRFILE.WR_AD_D_CLAIMS_1,
                   WRFILE.WR_L_PROT_CLAIMS_1,
                   WRFILE.WR_NF_NON_BK_CLAIMS_1,
                   WRFILE.WR_NF_BK_CLAIMS_1,
                   WRFILE.WR_ROAD_GARD_CLAIMS_1,
                   WRFILE.WR_NO_LIFE_CLAIMS_1,
                   WRFILE.WR_NO_AH_CLAIMS_1,
                   WRFILE.WR_NO_FIRE_CLAIMS_1,
                   WRFILE.WR_NO_AUTO_CLAIMS_1,
                   WRFILE.WR_NO_UNEMP_CLAIMS_1,
                   WRFILE.WR_NO_AD_D_CLAIMS_1,
                   WRFILE.WR_NO_L_PROT_CLAIMS_1,
                   WRFILE.WR_NO_NF_NON_BK_CLAIMS_1,
                   WRFILE.WR_NO_NF_BK_CLAIMS_1,
                   WRFILE.WR_NO_ROAD_GARD_CLAIMS_1,
                   WRFILE.WR_LIFE_PREMS_1,
                   WRFILE.WR_AH_PREMS_1,
                   WRFILE.WR_FIRE_PREMS_1,
                   WRFILE.WR_AUTO_PREMS_1,
                   WRFILE.WR_UNEMP_PREMS_1,
                   WRFILE.WR_AD_D_PREMS_1,
                   WRFILE.WR_L_PROT_PREMS_1,
                   WRFILE.WR_NON_FILE_PREMS_1,
                   WRFILE.WR_ROAD_GARD_PREMS_1,
                   WRFILE.WR_NO_LIFE_PREMS_1,
                   WRFILE.WR_NO_AH_PREMS_1,
                   WRFILE.WR_NO_FIRE_PREMS_1,
                   WRFILE.WR_NO_AUTO_PREMS_1,
                   WRFILE.WR_NO_UNEMP_PREMS_1,
                   WRFILE.WR_NO_AD_D_PREMS_1,
                   WRFILE.WR_NO_L_PROT_PREMS_1,
                   WRFILE.WR_NO_NON_FILE_PREMS_1,
                   WRFILE.WR_NO_ROAD_GARD_PREMS_1,
                   WRFILE.WR_LIFE_REFS_1,
                   WRFILE.WR_AH_REFS_1,
                   WRFILE.WR_FIRE_REFS_1,
                   WRFILE.WR_AUTO_REFS_1,
                   WRFILE.WR_UNEMP_REFS_1,
                   WRFILE.WR_AD_D_REFS_1,
                   WRFILE.WR_L_PROT_REFS_1,
                   WRFILE.WR_NON_FILE_REFS_1,
                   WRFILE.WR_ROAD_GARD_REFS_1,
                   WRFILE.WR_NF_RECOVERIES_1,
                   WRFILE.WR_LIFE_CLAIMS_2,
                   WRFILE.WR_AH_CLAIMS_2,
                   WRFILE.WR_FIRE_CLAIMS_2,
                   WRFILE.WR_AUTO_CLAIMS_2,
                   WRFILE.WR_UNEMP_CLAIMS_2,
                   WRFILE.WR_AD_D_CLAIMS_2,
                   WRFILE.WR_L_PROT_CLAIMS_2,
                   WRFILE.WR_NF_NON_BK_CLAIMS_2,
                   WRFILE.WR_NF_BK_CLAIMS_2,
                   WRFILE.WR_ROAD_GARD_CLAIMS_2,
                   WRFILE.WR_NO_LIFE_CLAIMS_2,
                   WRFILE.WR_NO_AH_CLAIMS_2,
                   WRFILE.WR_NO_FIRE_CLAIMS_2,
                   WRFILE.WR_NO_AUTO_CLAIMS_2,
                   WRFILE.WR_NO_UNEMP_CLAIMS_2,
                   WRFILE.WR_NO_AD_D_CLAIMS_2,
                   WRFILE.WR_NO_L_PROT_CLAIMS_2,
                   WRFILE.WR_NO_NF_NON_BK_CLAIMS_2,
                   WRFILE.WR_NO_NF_BK_CLAIMS_2,
                   WRFILE.WR_NO_ROAD_GARD_CLAIMS_2,
                   WRFILE.WR_LIFE_PREMS_2,
                   WRFILE.WR_AH_PREMS_2,
                   WRFILE.WR_FIRE_PREMS_2,
                   WRFILE.WR_AUTO_PREMS_2,
                   WRFILE.WR_UNEMP_PREMS_2,
                   WRFILE.WR_AD_D_PREMS_2,
                   WRFILE.WR_L_PROT_PREMS_2,
                   WRFILE.WR_NON_FILE_PREMS_2,
                   WRFILE.WR_ROAD_GARD_PREMS_2,
                   WRFILE.WR_NO_LIFE_PREMS_2,
                   WRFILE.WR_NO_AH_PREMS_2,
                   WRFILE.WR_NO_FIRE_PREMS_2,
                   WRFILE.WR_NO_AUTO_PREMS_2,
                   WRFILE.WR_NO_UNEMP_PREMS_2,
                   WRFILE.WR_NO_AD_D_PREMS_2,
                   WRFILE.WR_NO_L_PROT_PREMS_2,
                   WRFILE.WR_NO_NON_FILE_PREMS_2,
                   WRFILE.WR_NO_ROAD_GARD_PREMS_2,
                   WRFILE.WR_LIFE_REFS_2,
                   WRFILE.WR_AH_REFS_2,
                   WRFILE.WR_FIRE_REFS_2,
                   WRFILE.WR_AUTO_REFS_2,
                   WRFILE.WR_UNEMP_REFS_2,
                   WRFILE.WR_AD_D_REFS_2,
                   WRFILE.WR_L_PROT_REFS_2,
                   WRFILE.WR_NON_FILE_REFS_2,
                   WRFILE.WR_ROAD_GARD_REFS_2,
                   WRFILE.WR_NF_RECOVERIES_2,
                   WRFILE.WR_NONFILE_ENDING_BALANCE,
                   WRFILE.WR_NO_CUST_1_YR_AGO,
                   WRFILE.WR_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_WCBC_NO_CUST_1_YR_AGO,
                   WRFILE.WR_WCBC_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_LARGE_NO_CUST_1_YR_AGO,
                   WRFILE.WR_LARGE_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_CUR_NO_CONT_SLOW,
                   WRFILE.WR_WCBC_CUR_NO_CONT_SLOW,
                   WRFILE.WR_LARGE_CUR_NO_CONT_SLOW,
                   WRFILE.WR_CASHIER_BONUS,
                   WRFILE.WR_ASST_MGR_BONUS,
                   WRFILE.WR_MGR_BONUS,
                   WRFILE.WR_LOTS_NF_CLAIMS_1,
                   WRFILE.WR_LOTS_NO_NF_CLAIMS_1,
                   WRFILE.WR_LOTS_NON_FILE_PREMS_1,
                   WRFILE.WR_LOTS_NO_NON_FILE_PREMS_1,
                   WRFILE.WR_LOTS_NF_RECOVERIES_1,
                   WRFILE.WR_LOTS_NF_CLAIMS_2,
                   WRFILE.WR_LOTS_NO_NF_CLAIMS_2,
                   WRFILE.WR_LOTS_NON_FILE_PREMS_2,
                   WRFILE.WR_LOTS_NO_NON_FILE_PREMS_2,
                   WRFILE.WR_LOTS_NF_RECOVERIES_2,
                   WRFILE.WR_PRETAX_PROFIT_1,
                   WRFILE.WR_SUPERVISION_1,
                   WRFILE.WR_PRETAX_PROFIT_2,
                   WRFILE.WR_SUPERVISION_2,
                   WRFILE.WR_MGR_PENALTY_FG,
                   WRFILE.WR_NO_TAX_RETURNS_1,
                   WRFILE.WR_NO_TAX_RETURNS_2
            INTO :QWR-REC
            FROM DBO.WRFILE
            WHERE WRFILE.WR_YYYYMM = :QWR-YYYYMM
              AND WRFILE.WR_BRNO = :QWR-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-WR1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-WR-FIELDS.

      *-----------------------------------------------------------------
       READ-WR1-FILE-NEXT.  *> EMBEDDED WR
           PERFORM READ-IT.
           MOVE WR-PATH-SQL TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           INITIALIZE QWR-REC.

           EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC.

           IF ( WR1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH WR1_CURSOR INTO 
                   :QWR-YYYYMM,
                   :QWR-BRNO,
                   :QWR-LTOUCH-DATE, 
                   :QWR-CUR-NO-CUST,
                   :QWR-ELM-NO-CUST,
                   :QWR-ELY-NO-CUST,
                   :QWR-OUTS-BALANCE,
                   :QWR-ELM-OUTS-BALANCE,
                   :QWR-ELY-OUTS-BALANCE,
                   :QWR-NO-NB-1,
                   :QWR-NO-NB-2,
                   :QWR-NO-FB-1,
                   :QWR-NO-FB-2,
                   :QWR-NO-PB-1,
                   :QWR-NO-PB-2,
                   :QWR-GROSS-VOLUME-1,
                   :QWR-GROSS-VOLUME-2,
                   :QWR-BSR-BONUS-1,
                   :QWR-BSR-BONUS-2,
                   :QWR-INTEREST-ALLOCATION-1,
                   :QWR-INTEREST-ALLOCATION-2,
                   :QWR-GEN-RESERVE-ADJ-1,
                   :QWR-GEN-RESERVE-ADJ-2,
                   :QWR-INTANGIBLE-AMORT-1,
                   :QWR-INTANGIBLE-AMORT-2,
                   :QWR-ROBBERIES-1,
                   :QWR-ROBBERIES-2,
                   :QWR-RATING-SHEET-INCOME-1,
                   :QWR-RATING-SHEET-INCOME-2,
                   :QWR-INT-FEE-INCOME-1,
                   :QWR-INT-FEE-INCOME-2,
                   :QWR-INSURANCE-INCOME-1,
                   :QWR-INSURANCE-INCOME-2,
                   :QWR-RATING-SHEET-EXPENSES-1,
                   :QWR-RATING-SHEET-EXPENSES-2,
                   :QWR-PERSONNEL-EXPENSE-1,
                   :QWR-PERSONNEL-EXPENSE-2,
                   :QWR-CHARGEOFF-EXPENSE-1,
                   :QWR-CHARGEOFF-EXPENSE-2,
                   :QWR-NO-CHARGEOFFS-1,
                   :QWR-NO-CHARGEOFFS-2,
                   :QWR-RECOVERY-INCOME-1,
                   :QWR-RECOVERY-INCOME-2,
                   :QWR-BAD-DEBT-INCREASE-1,
                   :QWR-BAD-DEBT-INCREASE-2,
                   :QWR-NO-30-DAY-ACCTS,
                   :QWR-AMT-30-DAY-ACCTS,
                   :QWR-NO-60-DAY-ACCTS,
                   :QWR-AMT-60-DAY-ACCTS,
                   :QWR-NO-90-DAY-ACCTS,
                   :QWR-AMT-90-DAY-ACCTS,
                   :QWR-ELM-AMT-90-DAY-ACCTS,
                   :QWR-ELY-AMT-90-DAY-ACCTS,
                   :QWR-YTD-PCT-TURNOVER,
                   :QWR-YTD-PCT-RATE-SHT-PROFIT,
                   :QWR-YTD-PCT-RS-INC-LEDGER,
                   :QWR-YTD-PCT-INT-FEE-LEDGER,
                   :QWR-YTD-PCT-INS-INC-LEDGER,
                   :QWR-YTD-PCT-EXPENSE-LEDGER,
                   :QWR-YTD-PCT-BAD-DEBT-LEDGER,
                   :QWR-YTD-PCT-NF-BNK-LEDGER,
                   :QWR-YTD-PCT-PRETAX-LEDGER,
                   :QWR-YTD-PCT-DEBT-NF-LEDGER,
                   :QWR-WCBC-CUR-NO-CUST,
                   :QWR-WCBC-ELM-NO-CUST,
                   :QWR-WCBC-ELY-NO-CUST,
                   :QWR-WCBC-OUTS-BALANCE,
                   :QWR-WCBC-ELM-OUTS-BAL,
                   :QWR-WCBC-ELY-OUTS-BAL,
                   :QWR-WCBC-NO-NB-1,
                   :QWR-WCBC-NO-NB-2,
                   :QWR-WCBC-NO-FB-1,
                   :QWR-WCBC-NO-FB-2,
                   :QWR-WCBC-NO-PB-1,
                   :QWR-WCBC-NO-PB-2,
                   :QWR-WCBC-GROSS-VOLUME-1,
                   :QWR-WCBC-GROSS-VOLUME-2,
                   :QWR-WCBC-LATE-CHGS-1,
                   :QWR-WCBC-LATE-CHGS-2,
                   :QWR-WCBC-NET-FIN-CHGS-1,
                   :QWR-WCBC-NET-FIN-CHGS-2,
                   :QWR-WCBC-SALES-1,
                   :QWR-WCBC-SALES-2,
                   :QWR-WCBC-COST-OF-SALES-1,
                   :QWR-WCBC-COST-OF-SALES-2,
                   :QWR-WCBC-EXPENSES-1,
                   :QWR-WCBC-EXPENSES-2,
                   :QWR-WCBC-RECOVERY-INCOME-1,
                   :QWR-WCBC-RECOVERY-INCOME-2,
                   :QWR-WCBC-CHARGEOFF-EXPENSE-1,
                   :QWR-WCBC-CHARGEOFF-EXPENSE-2,
                   :QWR-WCBC-BAD-DEBT-BNKRPCY-1,
                   :QWR-WCBC-BAD-DEBT-BNKRPCY-2,
                   :QWR-WCBC-NO-30-DAY-ACCTS,
                   :QWR-WCBC-AMT-30-DAY-ACCTS,
                   :QWR-WCBC-NO-60-DAY-ACCTS,
                   :QWR-WCBC-AMT-60-DAY-ACCTS,
                   :QWR-WCBC-NO-90-DAY-ACCTS,
                   :QWR-WCBC-AMT-90-DAY-ACCTS,
                   :QWR-WCBC-ELM-AMT-90-DAYS,
                   :QWR-WCBC-ELY-AMT-90-DAYS,
                   :QWR-WCBC-PCT-VOLUME,
                   :QWR-WCBC-GROSS-PROF-PCT,
                   :QWR-LARGE-CUR-NO-CUST,
                   :QWR-LARGE-ELM-NO-CUST,
                   :QWR-LARGE-ELY-NO-CUST,
                   :QWR-LARGE-OUTS-BALANCE,
                   :QWR-LARGE-ELM-OUTS-BAL,
                   :QWR-LARGE-ELY-OUTS-BAL,
                   :QWR-LARGE-NO-NB-1,
                   :QWR-LARGE-NO-NB-2,
                   :QWR-LARGE-NO-FB-1,
                   :QWR-LARGE-NO-FB-2,
                   :QWR-LARGE-NO-PB-1,
                   :QWR-LARGE-NO-PB-2,
                   :QWR-LARGE-GROSS-VOLUME-1,
                   :QWR-LARGE-GROSS-VOLUME-2,
                   :QWR-LARGE-NO-30-DAY-ACCTS,
                   :QWR-LARGE-AMT-30-DAY-ACCTS,
                   :QWR-LARGE-NO-60-DAY-ACCTS,
                   :QWR-LARGE-AMT-60-DAY-ACCTS,
                   :QWR-LARGE-NO-90-DAY-ACCTS,
                   :QWR-LARGE-AMT-90-DAY-ACCTS,
                   :QWR-LARGE-ELM-AMT-90-DAYS,
                   :QWR-LARGE-ELY-AMT-90-DAYS,
                   :QWR-LIFE-CLAIMS-1,
                   :QWR-LIFE-CLAIMS-2,
                   :QWR-AH-CLAIMS-1,
                   :QWR-AH-CLAIMS-2,
                   :QWR-FIRE-CLAIMS-1,
                   :QWR-FIRE-CLAIMS-2,
                   :QWR-AUTO-CLAIMS-1,
                   :QWR-AUTO-CLAIMS-2,
                   :QWR-UNEMP-CLAIMS-1,
                   :QWR-UNEMP-CLAIMS-2,
                   :QWR-AD-D-CLAIMS-1,
                   :QWR-AD-D-CLAIMS-2,
                   :QWR-L-PROT-CLAIMS-1,
                   :QWR-L-PROT-CLAIMS-2,
                   :QWR-NF-NON-BK-CLAIMS-1,
                   :QWR-NF-NON-BK-CLAIMS-2,
                   :QWR-NF-BK-CLAIMS-1,
                   :QWR-NF-BK-CLAIMS-2,
                   :QWR-ROAD-GARD-CLAIMS-1,
                   :QWR-ROAD-GARD-CLAIMS-2,
                   :QWR-NO-LIFE-CLAIMS-1,
                   :QWR-NO-LIFE-CLAIMS-2,
                   :QWR-NO-AH-CLAIMS-1,
                   :QWR-NO-AH-CLAIMS-2,
                   :QWR-NO-FIRE-CLAIMS-1,
                   :QWR-NO-FIRE-CLAIMS-2,
                   :QWR-NO-AUTO-CLAIMS-1,
                   :QWR-NO-AUTO-CLAIMS-2,
                   :QWR-NO-UNEMP-CLAIMS-1,
                   :QWR-NO-UNEMP-CLAIMS-2,
                   :QWR-NO-AD-D-CLAIMS-1,
                   :QWR-NO-AD-D-CLAIMS-2,
                   :QWR-NO-L-PROT-CLAIMS-1,
                   :QWR-NO-L-PROT-CLAIMS-2,
                   :QWR-NO-NF-NON-BK-CLAIMS-1,
                   :QWR-NO-NF-NON-BK-CLAIMS-2,
                   :QWR-NO-NF-BK-CLAIMS-1,
                   :QWR-NO-NF-BK-CLAIMS-2,
                   :QWR-NO-ROAD-GARD-CLAIMS-1,
                   :QWR-NO-ROAD-GARD-CLAIMS-2,
                   :QWR-LIFE-PREMS-1,
                   :QWR-LIFE-PREMS-2,
                   :QWR-AH-PREMS-1,
                   :QWR-AH-PREMS-2,
                   :QWR-FIRE-PREMS-1,
                   :QWR-FIRE-PREMS-2,
                   :QWR-AUTO-PREMS-1,
                   :QWR-AUTO-PREMS-2,
                   :QWR-UNEMP-PREMS-1,
                   :QWR-UNEMP-PREMS-2,
                   :QWR-AD-D-PREMS-1,
                   :QWR-AD-D-PREMS-2,
                   :QWR-L-PROT-PREMS-1,
                   :QWR-L-PROT-PREMS-2,
                   :QWR-NON-FILE-PREMS-1,
                   :QWR-NON-FILE-PREMS-2,
                   :QWR-ROAD-GARD-PREMS-1,
                   :QWR-ROAD-GARD-PREMS-2,
                   :QWR-NO-LIFE-PREMS-1,
                   :QWR-NO-LIFE-PREMS-2,
                   :QWR-NO-AH-PREMS-1,
                   :QWR-NO-AH-PREMS-2,
                   :QWR-NO-FIRE-PREMS-1,
                   :QWR-NO-FIRE-PREMS-2,
                   :QWR-NO-AUTO-PREMS-1,
                   :QWR-NO-AUTO-PREMS-2,
                   :QWR-NO-UNEMP-PREMS-1,
                   :QWR-NO-UNEMP-PREMS-2,
                   :QWR-NO-AD-D-PREMS-1,
                   :QWR-NO-AD-D-PREMS-2,
                   :QWR-NO-L-PROT-PREMS-1,
                   :QWR-NO-L-PROT-PREMS-2,
                   :QWR-NO-NON-FILE-PREMS-1,
                   :QWR-NO-NON-FILE-PREMS-2,
                   :QWR-NO-ROAD-GARD-PREMS-1,
                   :QWR-NO-ROAD-GARD-PREMS-2,
                   :QWR-LIFE-REFS-1,
                   :QWR-LIFE-REFS-2,
                   :QWR-AH-REFS-1,
                   :QWR-AH-REFS-2,
                   :QWR-FIRE-REFS-1,
                   :QWR-FIRE-REFS-2,
                   :QWR-AUTO-REFS-1,
                   :QWR-AUTO-REFS-2,
                   :QWR-UNEMP-REFS-1,
                   :QWR-UNEMP-REFS-2,
                   :QWR-AD-D-REFS-1,
                   :QWR-AD-D-REFS-2,
                   :QWR-L-PROT-REFS-1,
                   :QWR-L-PROT-REFS-2,
                   :QWR-NON-FILE-REFS-1,
                   :QWR-NON-FILE-REFS-2,
                   :QWR-ROAD-GARD-REFS-1,
                   :QWR-ROAD-GARD-REFS-2,
                   :QWR-NF-RECOVERIES-1,
                   :QWR-NF-RECOVERIES-2,
                   :QWR-NONFILE-ENDING-BALANCE,
                   :QWR-NO-CUST-1-YR-AGO,
                   :QWR-OUTS-BAL-1-YR-AGO,
                   :QWR-WCBC-NO-CUST-1-YR-AGO,
                   :QWR-WCBC-OUTS-BAL-1-YR-AGO,
                   :QWR-LARGE-NO-CUST-1-YR-AGO,
                   :QWR-LARGE-OUTS-BAL-1-YR-AGO,
                   :QWR-CUR-NO-CONT-SLOW,
                   :QWR-WCBC-CUR-NO-CONT-SLOW,
                   :QWR-LARGE-CUR-NO-CONT-SLOW,
                   :QWR-CASHIER-BONUS,
                   :QWR-ASST-MGR-BONUS,
                   :QWR-MGR-BONUS,
                   :QWR-LOTS-NF-CLAIMS-1,
                   :QWR-LOTS-NF-CLAIMS-2,
                   :QWR-LOTS-NO-NF-CLAIMS-1,
                   :QWR-LOTS-NO-NF-CLAIMS-2,
                   :QWR-LOTS-NON-FILE-PREMS-1,
                   :QWR-LOTS-NON-FILE-PREMS-2,
                   :QWR-LOTS-NO-NON-FILE-PREMS-1,
                   :QWR-LOTS-NO-NON-FILE-PREMS-2,
                   :QWR-LOTS-NF-RECOVERIES-1,
                   :QWR-LOTS-NF-RECOVERIES-2,
                   :QWR-PRETAX-PROFIT-1,
                   :QWR-PRETAX-PROFIT-2,
                   :QWR-SUPERVISION-1,
                   :QWR-SUPERVISION-2
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-WR1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-WR-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-WR1-FILE.
           PERFORM CLOSE-IT.

           IF ( WR1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE WR1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO WR1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-WR1-CONNECTION.

           IF WR1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT WR1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO WR1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


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
              DESTROY ISTATS-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
