       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WREOM1.
      ******************************************************************
      *
      *                      (SP)
      *    DESCRIPTION:  EOM RATING SHEET FILE (WR-FILE)
      *                  READ LAST MONTH WR-REC; WRITE NEW MONTH WR-REC
      *                  03/02/04 COPIED FROM SP/WRLIST.C
      * REV:
      * BLV 041220 CLEAR MTD LOTS FIELDS
      * BLF 050426 CLEAR YTD LOTS FIELDS
      * BLM 160412 CLEAR MTD/YTD TAX-RETURNS, ALSO CLEAR MGR-PENALTY-FG,
      *            WORLD PR# 1149
      * BLM 160706 CLEAR WR-BSR-BONUS(1), WORLD PR# 1163
      * MJD 170912 ACTIVATE 8 DIGIT DATES, PD0006
      * BLM 180201 FIX TEST ON INVALID DATE, PROGRAM COULD NOT BE RUN
      * BAH 20200831 REMOVED RESET ON BR1 
      *****************************************************************
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

       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/WREOM1.CBL S34 06/21/21-11:37:41 >".

      ******************************************************************
      *     F I L E   I F N
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RATING SHEET RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRTM".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LAST YEAR'S RATING SHEET RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRLY".


       01  PR-IFN               PIC X(2)     VALUE "PR".

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  FIRST-TIME           PIC X  VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  HOLD-WR1-KEY.
           03  HOLD-WR-YYYYMM                 PIC 9(6).
           03  HOLD-WR-BRNO                   PIC 9(4).

      *----------- D A T E   W O R K E R S -------
      *    HARD CODED FIRST FISCAL MONTH IS APRIL (04)
      *    LINE YTDS WILL BE CALCULATED FOR SUBSCRIPTS
      *    BEGINNING WITH THAT OF APRIL
       01  FIRST-FISCAL-MO             PIC 99 VALUE 4.

       01  CURRENT-YYYYMM          PIC 9(6).
       01  FILLER                  REDEFINES CURRENT-YYYYMM.
           03  CURRENT-YYYY        PIC 9(4).
           03  CURRENT-MM          PIC 99.
               88  END-OF-YEAR-MM  VALUE 03.

       01  NEXT-YYYYMM             PIC 9(6).
       01  FILLER                  REDEFINES NEXT-YYYYMM.
           03  NEXT-YYYY           PIC 9(4).
           03  NEXT-MM             PIC 99.

       01  LAST-YYYYMM             PIC 9(6).
       01  FILLER                  REDEFINES LAST-YYYYMM.
           03  LAST-YYYY           PIC 9(4).
           03  LAST-MM             PIC 99.
      *------------------------------------------------------
       01  MESS-LIST               PIC X(5)     VALUE "MESS.".
       01  MSG-LIST                PIC X(35) VALUE
           "MSG1 MSG2 MSG3 MSG4 MSG5 MSG6 MSG7.".
       01  MSG-BUF.
           03  MSG1                PIC X(65) VALUE SPACES.
           03  MSG2                PIC X(65) VALUE SPACES.
           03  MSG3                PIC X(65) VALUE SPACES.
           03  MSG4                PIC X(65) VALUE SPACES.
           03  MSG5                PIC X(65) VALUE SPACES.
           03  MSG6                PIC X(65) VALUE SPACES.
           03  MSG7                PIC X(65) VALUE SPACES.
       01  MONTH-WORKER            PIC X(9).
       01  MONTH-DESC-TABLE.
           03  MONTH-FILLER.
               05  FILLER          PIC X(9) VALUE "JANUARY  ".
               05  FILLER          PIC X(9) VALUE "FEBRUARY ".
               05  FILLER          PIC X(9) VALUE "MARCH    ".
               05  FILLER          PIC X(9) VALUE "APRIL    ".
               05  FILLER          PIC X(9) VALUE "MAY      ".
               05  FILLER          PIC X(9) VALUE "JUNE     ".
               05  FILLER          PIC X(9) VALUE "JULY     ".
               05  FILLER          PIC X(9) VALUE "AUGUST   ".
               05  FILLER          PIC X(9) VALUE "SEPTEMBER".
               05  FILLER          PIC X(9) VALUE "OCTOBER  ".
               05  FILLER          PIC X(9) VALUE "NOVEMBER ".
               05  FILLER          PIC X(9) VALUE "DECEMBER ".
       01  MONTH-DESC-TABLE2 REDEFINES MONTH-DESC-TABLE.
           03  MONTH-DESC       PIC X(9) OCCURS 12.



       01  LEGEND                  PIC X(70) VALUE SPACES.
       01  PROGRESS-MESS REDEFINES LEGEND.
           03  PROGRESS-1          PIC X(20).
           03  PROGRESS-YYYYMM     PIC 9(6).
           03  PROGRESS-DASH1      PIC X(3).
           03  PROGRESS-BRNO       PIC 9(4).
           03  FILLER              PIC X(37).

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  LOCAL-PRINT-FG  PIC X.

       01  VERSION-CONTROL     PIC X     VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WREOM1_DEF.CPY".
       COPY "LIBSP/WREOM1_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/WREOM1_SCN.CPY".

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
           CLOSE
               PR-FILE.
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
           MOVE "WREOM1" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           MOVE "RATING SHEET EOM/EOY  B E G I N" TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.

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
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           PERFORM OPEN-WR1-FILE.

           MOVE CURRENT-YYYYMM TO WR-YYYYMM
                                  QWR1-WBEG-YYYYMM
                                  QWR1-WEND-YYYYMM.
           MOVE BR-NO          TO WR-BRNO
                                  QWR1-WBEG-BRNO
                                  QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.

       NEXT-WR1.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-OF-GROUP.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO LEGEND
              MOVE "PROCESSING DATE/BRANCH # " TO PROGRESS-1
              MOVE WR-YYYYMM TO PROGRESS-YYYYMM
              MOVE " - "     TO PROGRESS-DASH1
              MOVE WR-BRNO   TO PROGRESS-BRNO
              MOVE LEGEND TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF WR-YYYYMM NOT = CURRENT-YYYYMM
              GO TO END-OF-GROUP.

           IF WR-BRNO NOT = BR-NO
              GO TO END-OF-GROUP.

      * OK-TO-PROCESS

           MOVE "N" TO FIRST-TIME.

           MOVE WR1-KEY TO HOLD-WR1-KEY.

      * GLB 04/07/04
      * READ LAST YEAR WRLY-REC:
           MOVE LAST-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-FG = 9
              PERFORM CLEAR-WR-FILE.
           MOVE WR-REC TO WRLY-REC.

      * RE-READ CURRENT MONTH WR-REC:
           MOVE HOLD-WR1-KEY TO WR1-KEY.
           PERFORM READ-WR1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           PERFORM UPDATE-WR-EOM.

           PERFORM UPDATE-WR-EOY.

           PERFORM UPDATE-WR-LAST-EOY.
           MOVE WR-REC    TO WRTM-REC.

      *-----------------------------------------------------------
      *           CREATE NEW WR-REC FOR NEXT YYYYMM
      *-----------------------------------------------------------
       CREATE-NEW-WR-MONTH.
           MOVE NEXT-YYYYMM TO WR-YYYYMM.
           PERFORM WRITE-WR1-FILE.
           IF IO-BAD
              IF E-CODE = "22"
                 MOVE "ALREADY WRITTEN" TO MESS
                 PERFORM SEND-MESS
                 PERFORM REWRITE-WR1-FILE
              ELSE
                 GO TO IO-ERROR.

      *-----------------------------------------------------------
      * MOVE ORIGINAL EOM RECORD BACK (RESET WR1-KEY)
      *-----------------------------------------------------------
           MOVE WRTM-REC  TO WR-REC.
           MOVE WR-YYYYMM TO QWR1-WBEG-YYYYMM
                             QWR1-WEND-YYYYMM.
           MOVE WR-BRNO   TO QWR1-WBEG-BRNO.
           MOVE ALL "9"   TO QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.
           PERFORM READ-WR1-FILE-NEXT.

           GO TO NEXT-WR1.

      *----------------------------------------------------------------
       END-OF-GROUP.
           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-LOC
           ELSE
              GO TO NEXT-BRANCH-NO.
      *----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "RATING SHEET EOM/EOY COMPLETED"
              TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.

           GO TO END-PROGRAM.

      *****************************************************************
       UPDATE-WR-EOM SECTION.
      *****************************************************************

      * UPDATE WR EOM CREATE NEXT MONTH RECORD AND SET & CLEAR

      * MOVE CURRENT TO ELM

      * ELM FIELDS FOR 90+ BALANCES FOR DIRECT, WCBC,

           MOVE WR-CUR-NO-CUST               TO WR-ELM-NO-CUST.
           MOVE WR-OUTS-BALANCE              TO WR-ELM-OUTS-BALANCE.
           MOVE WR-AMT-90-DAY-ACCTS          TO WR-ELM-AMT-90-DAY-ACCTS.
           MOVE WR-WCBC-CUR-NO-CUST          TO WR-WCBC-ELM-NO-CUST.
           MOVE WR-WCBC-OUTS-BALANCE         TO WR-WCBC-ELM-OUTS-BAL.
           MOVE WR-WCBC-AMT-90-DAY-ACCTS     TO WR-WCBC-ELM-AMT-90-DAYS.
           MOVE WR-LARGE-CUR-NO-CUST         TO WR-LARGE-ELM-NO-CUST.
           MOVE WR-LARGE-OUTS-BALANCE        TO WR-LARGE-ELM-OUTS-BAL.
           MOVE WR-LARGE-AMT-90-DAY-ACCTS
              TO WR-LARGE-ELM-AMT-90-DAYS.

      * CLEAR  M-T-D

           MOVE 0 TO  WR-NO-NB                  (1).
           MOVE 0 TO  WR-NO-FB                  (1).
           MOVE 0 TO  WR-NO-PB                  (1).
           MOVE 0 TO  WR-GROSS-VOLUME           (1).
           MOVE 0 TO  WR-PRETAX-PROFIT          (1).
           MOVE 0 TO  WR-INTEREST-ALLOCATION    (1).
           MOVE 0 TO  WR-SUPERVISION            (1).
           MOVE 0 TO  WR-GEN-RESERVE-ADJ        (1).
           MOVE 0 TO  WR-INTANGIBLE-AMORT       (1).
           MOVE 0 TO  WR-ROBBERIES              (1).
           MOVE 0 TO  WR-RATING-SHEET-INCOME    (1).
           MOVE 0 TO  WR-INT-FEE-INCOME         (1).
           MOVE 0 TO  WR-INSURANCE-INCOME       (1).
           MOVE 0 TO  WR-RATING-SHEET-EXPENSES  (1).
           MOVE 0 TO  WR-PERSONNEL-EXPENSE      (1).
           MOVE 0 TO  WR-CHARGEOFF-EXPENSE      (1).
           MOVE 0 TO  WR-NO-CHARGEOFFS          (1).
           MOVE 0 TO  WR-RECOVERY-INCOME        (1).
           MOVE 0 TO  WR-BAD-DEBT-INCREASE      (1).

           MOVE 0 TO   WR-WCBC-NO-NB             (1).
           MOVE 0 TO   WR-WCBC-NO-FB             (1).
           MOVE 0 TO   WR-WCBC-NO-PB             (1).
           MOVE 0 TO   WR-WCBC-GROSS-VOLUME      (1).
           MOVE 0 TO   WR-WCBC-LATE-CHGS         (1).
           MOVE 0 TO   WR-WCBC-NET-FIN-CHGS      (1).
           MOVE 0 TO   WR-WCBC-SALES             (1).
           MOVE 0 TO   WR-WCBC-COST-OF-SALES     (1).
           MOVE 0 TO   WR-WCBC-EXPENSES          (1).
           MOVE 0 TO   WR-WCBC-RECOVERY-INCOME   (1).
           MOVE 0 TO   WR-WCBC-CHARGEOFF-EXPENSE (1).
           MOVE 0 TO   WR-WCBC-BAD-DEBT-BNKRPCY  (1).

      * WCBC YTD PERCENTS - MTD IS CALCULATED BUT YTD IS ACCUMULATED
           MOVE 0 TO  WR-LARGE-NO-NB             (1).
           MOVE 0 TO  WR-LARGE-NO-FB             (1).
           MOVE 0 TO  WR-LARGE-NO-PB             (1).
           MOVE 0 TO  WR-LARGE-GROSS-VOLUME      (1).

           MOVE 0 TO  WR-LIFE-CLAIMS            (1).
           MOVE 0 TO  WR-AH-CLAIMS              (1).
           MOVE 0 TO  WR-FIRE-CLAIMS            (1).
           MOVE 0 TO  WR-AUTO-CLAIMS            (1).
           MOVE 0 TO  WR-UNEMP-CLAIMS           (1).
           MOVE 0 TO  WR-AD-D-CLAIMS            (1).
           MOVE 0 TO  WR-L-PROT-CLAIMS          (1).
           MOVE 0 TO  WR-NF-NON-BK-CLAIMS       (1).
           MOVE 0 TO  WR-NF-BK-CLAIMS           (1).
           MOVE 0 TO  WR-ROAD-GARD-CLAIMS       (1).

           MOVE 0 TO  WR-NO-LIFE-CLAIMS         (1).
           MOVE 0 TO  WR-NO-AH-CLAIMS           (1).
           MOVE 0 TO  WR-NO-FIRE-CLAIMS         (1).
           MOVE 0 TO  WR-NO-AUTO-CLAIMS         (1).
           MOVE 0 TO  WR-NO-UNEMP-CLAIMS        (1).
           MOVE 0 TO  WR-NO-AD-D-CLAIMS         (1).
           MOVE 0 TO  WR-NO-L-PROT-CLAIMS       (1).
           MOVE 0 TO  WR-NO-NF-NON-BK-CLAIMS    (1).
           MOVE 0 TO  WR-NO-NF-BK-CLAIMS        (1).
           MOVE 0 TO  WR-NO-ROAD-GARD-CLAIMS    (1).

           MOVE 0 TO  WR-LIFE-PREMS             (1).
           MOVE 0 TO  WR-AH-PREMS               (1).
           MOVE 0 TO  WR-FIRE-PREMS             (1).
           MOVE 0 TO  WR-AUTO-PREMS             (1).
           MOVE 0 TO  WR-UNEMP-PREMS            (1).
           MOVE 0 TO  WR-AD-D-PREMS             (1).
           MOVE 0 TO  WR-L-PROT-PREMS           (1).
           MOVE 0 TO  WR-NON-FILE-PREMS         (1).
           MOVE 0 TO  WR-ROAD-GARD-PREMS        (1).

           MOVE 0 TO  WR-NO-LIFE-PREMS          (1).
           MOVE 0 TO  WR-NO-AH-PREMS            (1).
           MOVE 0 TO  WR-NO-FIRE-PREMS          (1).
           MOVE 0 TO  WR-NO-AUTO-PREMS          (1).
           MOVE 0 TO  WR-NO-UNEMP-PREMS         (1).
           MOVE 0 TO  WR-NO-AD-D-PREMS          (1).
           MOVE 0 TO  WR-NO-L-PROT-PREMS        (1).
           MOVE 0 TO  WR-NO-NON-FILE-PREMS      (1).
           MOVE 0 TO  WR-NO-ROAD-GARD-PREMS     (1).

           MOVE 0 TO  WR-LIFE-REFS              (1).
           MOVE 0 TO  WR-AH-REFS                (1).
           MOVE 0 TO  WR-FIRE-REFS              (1).
           MOVE 0 TO  WR-AUTO-REFS              (1).
           MOVE 0 TO  WR-UNEMP-REFS             (1).
           MOVE 0 TO  WR-AD-D-REFS              (1).
           MOVE 0 TO  WR-L-PROT-REFS            (1).
           MOVE 0 TO  WR-NON-FILE-REFS          (1).
           MOVE 0 TO  WR-ROAD-GARD-REFS         (1).
           MOVE 0 TO  WR-NF-RECOVERIES          (1).
           MOVE 0 TO  WR-LOTS-NF-CLAIMS(1).
           MOVE 0 TO  WR-LOTS-NO-NF-CLAIMS(1).
           MOVE 0 TO  WR-LOTS-NON-FILE-PREMS(1).
           MOVE 0 TO  WR-LOTS-NO-NON-FILE-PREMS(1).
           MOVE 0 TO  WR-LOTS-NF-RECOVERIES(1).
           MOVE 0 TO WR-CASHIER-BONUS.
           MOVE 0 TO WR-ASST-MGR-BONUS.
           MOVE 0 TO WR-MGR-BONUS.
           MOVE 0 TO WR-BSR-BONUS(1).
           MOVE 0 TO WR-NO-TAX-RETURNS(1).
           MOVE " " TO WR-MGR-PENALTY-FG.
       EXIT-UPDATE-WR-EOM.
            EXIT.

      *****************************************************************
       UPDATE-WR-EOY SECTION.
      *****************************************************************

      * UPDATE WR EOY CLEAR YEAR END RECORD

           IF NOT (END-OF-YEAR-MM)
              GO TO EXIT-UPDATE-WR-EOY.

      * END OF YEAR

      * MOVE CURRENT TO ELY

           MOVE WR-CUR-NO-CUST            TO WR-ELY-NO-CUST.
           MOVE WR-OUTS-BALANCE           TO WR-ELY-OUTS-BALANCE.
           MOVE WR-AMT-90-DAY-ACCTS       TO WR-ELY-AMT-90-DAY-ACCTS.
           MOVE WR-WCBC-CUR-NO-CUST       TO WR-WCBC-ELY-NO-CUST.
           MOVE WR-WCBC-OUTS-BALANCE      TO WR-WCBC-ELY-OUTS-BAL.
           MOVE WR-WCBC-AMT-90-DAY-ACCTS  TO WR-WCBC-ELY-AMT-90-DAYS.
           MOVE WR-LARGE-CUR-NO-CUST      TO WR-LARGE-ELY-NO-CUST.
           MOVE WR-LARGE-OUTS-BALANCE     TO WR-LARGE-ELY-OUTS-BAL.
           MOVE WR-LARGE-AMT-90-DAY-ACCTS TO WR-LARGE-ELY-AMT-90-DAYS.

      * CLEAR Y-T-D

           MOVE 0 TO WR-YTD-PCT-TURNOVER.
           MOVE 0 TO WR-YTD-PCT-RATE-SHT-PROFIT.
           MOVE 0 TO WR-YTD-PCT-RS-INC-LEDGER.
           MOVE 0 TO WR-YTD-PCT-INT-FEE-LEDGER.
           MOVE 0 TO WR-YTD-PCT-INS-INC-LEDGER.
           MOVE 0 TO WR-YTD-PCT-EXPENSE-LEDGER.
           MOVE 0 TO WR-YTD-PCT-BAD-DEBT-LEDGER.
           MOVE 0 TO WR-YTD-PCT-NF-BNK-LEDGER.
           MOVE 0 TO WR-YTD-PCT-PRETAX-LEDGER.
           MOVE 0 TO WR-YTD-PCT-DEBT-NF-LEDGER.

           MOVE 0 TO WR-WCBC-PCT-VOLUME.
           MOVE 0 TO WR-WCBC-GROSS-PROF-PCT.

           MOVE 0 TO  WR-NO-NB                  (2).
           MOVE 0 TO  WR-NO-FB                  (2).
           MOVE 0 TO  WR-NO-PB                  (2).
           MOVE 0 TO  WR-GROSS-VOLUME           (2).
           MOVE 0 TO  WR-PRETAX-PROFIT          (2).
           MOVE 0 TO  WR-INTEREST-ALLOCATION    (2).
           MOVE 0 TO  WR-SUPERVISION            (2).
           MOVE 0 TO  WR-GEN-RESERVE-ADJ        (2).
           MOVE 0 TO  WR-INTANGIBLE-AMORT       (2).
           MOVE 0 TO  WR-ROBBERIES              (2).
           MOVE 0 TO  WR-RATING-SHEET-INCOME    (2).
           MOVE 0 TO  WR-INT-FEE-INCOME         (2).
           MOVE 0 TO  WR-INSURANCE-INCOME       (2).
           MOVE 0 TO  WR-RATING-SHEET-EXPENSES  (2).
           MOVE 0 TO  WR-PERSONNEL-EXPENSE      (2).
           MOVE 0 TO  WR-CHARGEOFF-EXPENSE      (2).
           MOVE 0 TO  WR-NO-CHARGEOFFS          (2).
           MOVE 0 TO  WR-RECOVERY-INCOME        (2).
           MOVE 0 TO  WR-BAD-DEBT-INCREASE      (2).

           MOVE 0 TO   WR-WCBC-NO-NB             (2).
           MOVE 0 TO   WR-WCBC-NO-FB             (2).
           MOVE 0 TO   WR-WCBC-NO-PB             (2).
           MOVE 0 TO   WR-WCBC-GROSS-VOLUME      (2).
           MOVE 0 TO   WR-WCBC-LATE-CHGS         (2).
           MOVE 0 TO   WR-WCBC-NET-FIN-CHGS      (2).
           MOVE 0 TO   WR-WCBC-SALES             (2).
           MOVE 0 TO   WR-WCBC-COST-OF-SALES     (2).
           MOVE 0 TO   WR-WCBC-EXPENSES          (2).
           MOVE 0 TO   WR-WCBC-RECOVERY-INCOME   (2).
           MOVE 0 TO   WR-WCBC-CHARGEOFF-EXPENSE (2).
           MOVE 0 TO   WR-WCBC-BAD-DEBT-BNKRPCY  (2).

           MOVE 0 TO  WR-LARGE-NO-NB             (2).
           MOVE 0 TO  WR-LARGE-NO-FB             (2).
           MOVE 0 TO  WR-LARGE-NO-PB             (2).
           MOVE 0 TO  WR-LARGE-GROSS-VOLUME      (2).

           MOVE 0 TO  WR-LIFE-CLAIMS             (2).
           MOVE 0 TO  WR-AH-CLAIMS               (2).
           MOVE 0 TO  WR-FIRE-CLAIMS             (2).
           MOVE 0 TO  WR-AUTO-CLAIMS             (2).
           MOVE 0 TO  WR-UNEMP-CLAIMS            (2).
           MOVE 0 TO  WR-AD-D-CLAIMS             (2).
           MOVE 0 TO  WR-L-PROT-CLAIMS           (2).
           MOVE 0 TO  WR-NF-NON-BK-CLAIMS        (2).
           MOVE 0 TO  WR-NF-BK-CLAIMS            (2).
           MOVE 0 TO  WR-ROAD-GARD-CLAIMS        (2).

           MOVE 0 TO  WR-NO-LIFE-CLAIMS          (2).
           MOVE 0 TO  WR-NO-AH-CLAIMS            (2).
           MOVE 0 TO  WR-NO-FIRE-CLAIMS          (2).
           MOVE 0 TO  WR-NO-AUTO-CLAIMS          (2).
           MOVE 0 TO  WR-NO-UNEMP-CLAIMS         (2).
           MOVE 0 TO  WR-NO-AD-D-CLAIMS          (2).
           MOVE 0 TO  WR-NO-L-PROT-CLAIMS        (2).
           MOVE 0 TO  WR-NO-NF-NON-BK-CLAIMS     (2).
           MOVE 0 TO  WR-NO-NF-BK-CLAIMS         (2).
           MOVE 0 TO  WR-NO-ROAD-GARD-CLAIMS     (2).

           MOVE 0 TO  WR-LIFE-PREMS              (2).
           MOVE 0 TO  WR-AH-PREMS                (2).
           MOVE 0 TO  WR-FIRE-PREMS              (2).
           MOVE 0 TO  WR-AUTO-PREMS              (2).
           MOVE 0 TO  WR-UNEMP-PREMS             (2).
           MOVE 0 TO  WR-AD-D-PREMS              (2).
           MOVE 0 TO  WR-L-PROT-PREMS            (2).
           MOVE 0 TO  WR-NON-FILE-PREMS          (2).
           MOVE 0 TO  WR-ROAD-GARD-PREMS         (2).

           MOVE 0 TO  WR-NO-LIFE-PREMS           (2).
           MOVE 0 TO  WR-NO-AH-PREMS             (2).
           MOVE 0 TO  WR-NO-FIRE-PREMS           (2).
           MOVE 0 TO  WR-NO-AUTO-PREMS           (2).
           MOVE 0 TO  WR-NO-UNEMP-PREMS          (2).
           MOVE 0 TO  WR-NO-AD-D-PREMS           (2).
           MOVE 0 TO  WR-NO-L-PROT-PREMS         (2).
           MOVE 0 TO  WR-NO-NON-FILE-PREMS       (2).
           MOVE 0 TO  WR-NO-ROAD-GARD-PREMS      (2).

           MOVE 0 TO  WR-LIFE-REFS               (2).
           MOVE 0 TO  WR-AH-REFS                 (2).
           MOVE 0 TO  WR-FIRE-REFS               (2).
           MOVE 0 TO  WR-AUTO-REFS               (2).
           MOVE 0 TO  WR-UNEMP-REFS              (2).
           MOVE 0 TO  WR-AD-D-REFS               (2).
           MOVE 0 TO  WR-L-PROT-REFS             (2).
           MOVE 0 TO  WR-NON-FILE-REFS           (2).
           MOVE 0 TO  WR-ROAD-GARD-REFS          (2).
           MOVE 0 TO  WR-NF-RECOVERIES           (2).
           MOVE 0 TO  WR-LOTS-NF-CLAIMS(2).
           MOVE 0 TO  WR-LOTS-NO-NF-CLAIMS(2).
           MOVE 0 TO  WR-LOTS-NON-FILE-PREMS(2).
           MOVE 0 TO  WR-LOTS-NO-NON-FILE-PREMS(2).
           MOVE 0 TO  WR-LOTS-NF-RECOVERIES(2).
           MOVE 0 TO WR-NO-TAX-RETURNS(2).
       EXIT-UPDATE-WR-EOY.
            EXIT.


      *****************************************************************
       UPDATE-WR-LAST-EOY SECTION.
      *****************************************************************

      * UPDATE NEW MONTH WR WITH LAST YEAR DATA

      * MOVE CURRENT WR-REC TO THIS MONTH WRTM RECORD
      *   - AND READ LAST YEAR  WRLY-REC
      *   - PROCESS
      * RESTORE THIS MONTH WRTM RECORD AND RESTART FILE:

      * MOVE LAST YEAR TO NEW  MONTH
           MOVE 0                         TO WR-NONFILE-ENDING-BALANCE.
           MOVE WRLY-CUR-NO-CUST          TO WR-NO-CUST-1-YR-AGO.
           MOVE WRLY-OUTS-BALANCE         TO WR-OUTS-BAL-1-YR-AGO.
           MOVE WRLY-WCBC-CUR-NO-CUST     TO WR-WCBC-NO-CUST-1-YR-AGO.
           MOVE WRLY-WCBC-OUTS-BALANCE    TO WR-WCBC-OUTS-BAL-1-YR-AGO.
           MOVE WRLY-LARGE-CUR-NO-CUST    TO WR-LARGE-NO-CUST-1-YR-AGO.
           MOVE WRLY-LARGE-OUTS-BALANCE   TO WR-LARGE-OUTS-BAL-1-YR-AGO.
           MOVE 0                         TO WR-CASHIER-BONUS.
           MOVE 0                         TO WR-ASST-MGR-BONUS.
           MOVE 0                         TO WR-MGR-BONUS.

       EXIT-UPD-LAST-EOY.
           EXIT.

      ***************************************************
       LOG-ERROR SECTION.
      ***************************************************

      * LOG ERROR IN GLOBAL LG DIRECTORY
           MOVE "M" TO ERRLOGW-ACTION.
           MOVE BR-NO TO ERRLOGW-BRNO.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE WREOM1-QUEUE-POS TO Q-POS.
           MOVE WREOM1-COPIES-POS TO C-POS.
           MOVE WREOM1-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH     TO SV-BEG-BR
              MOVE END-BRANCH     TO SV-END-BR
              MOVE GROUP-FLAG     TO SV-GROUP-FLAG
              MOVE ENT-GROUP      TO SV-ENT-GROUP
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

           IF EXT-ROUTE-BUF = "00"
              MOVE "EOM RATING SHEET IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BR-FILE.

           IF EXT-ROUTE-BUF = "00"
              GO TO EXIT-INITIALIZE.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO NUM-DATE.
           MOVE NUM-MO      TO DISPLAY-MO.
           MOVE NUM-YR      TO DISPLAY-YR.
           PERFORM SETUP-EOM-WR-DATE.
           PERFORM SETUP-NEXT-EOM-WR-DATE.
           PERFORM SETUP-LAST-EOY-WR-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           IF VDUSTATUS = "1U" GO TO BR-01.

      *----------------------------------------------------------------
       SEND-DEFAULT-DATE.
           PERFORM GET-TODAYS-DATE.

      *----------------------------------------------------------------
       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           IF VDUNUM0 = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              MOVE ALP-MO TO DISPLAY-MO
              MOVE ALP-YR TO DISPLAY-YR
              MOVE "SN N040MMYY." TO VDU
              MOVE DISPLAY-DATE TO VDUNUM0
              PERFORM SCREENIO.

           MOVE VDUNUM0       TO DISPLAY-DATE.
           IF DISPLAY-MO < 1 OR DISPLAY-MO > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETUP CURRENT MONTH/YEAR DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SETUP-EOM-WR-DATE.

           MOVE "SN N060YYYYMM."   TO VDU.
           MOVE CURRENT-YYYYMM TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETUP NEXT MONTH DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SETUP-NEXT-EOM-WR-DATE.

           MOVE "SN N060NYYYYMM."   TO VDU.
           MOVE NEXT-YYYYMM TO VDUNUM0.
           PERFORM SCREENIO.

      *  GLB 04/07/04
           PERFORM OPEN-WR1-FILE.

           MOVE NEXT-YYYYMM  TO WR-YYYYMM
                                QWR1-WBEG-YYYYMM
                                QWR1-WEND-YYYYMM.
           MOVE 0            TO WR-BRNO
                                QWR1-WBEG-BRNO.
           MOVE ALL "9"      TO QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-FG = 0
              IF WR-YYYYMM = NEXT-YYYYMM
                 MOVE SPACES TO MESS
                 STRING "WARNING! RECORDS EXIST FOR " NEXT-YYYYMM
                        " IN BRANCH: " WR-BRNO ".  "
                 DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS.

           MOVE CURRENT-YYYYMM     TO WR-YYYYMM
                                      QWR1-WBEG-YYYYMM
                                      QWR1-WEND-YYYYMM.
           MOVE 0                  TO WR-BRNO
                                      QWR1-WBEG-BRNO.
           MOVE ALL "9"            TO QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-FG = 9 OR (WR-YYYYMM NOT = CURRENT-YYYYMM)
              MOVE SPACES TO MESS
              STRING "NO EXISTING RECORDS FOR " CURRENT-YYYYMM
              DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-WR1-FILE
              GO TO ENTER-DATE.

           PERFORM CLOSE-WR1-FILE.

           IF NEXT-MM = FIRST-FISCAL-MO
              MOVE "END OF YEAR WILL BE PROCESSED "
              TO MESS PERFORM SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETUP LAST YEAR/MONTH DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SETUP-LAST-EOY-WR-DATE.

           MOVE "SN N060LYYYYMM."   TO VDU.
           MOVE LAST-YYYYMM TO VDUNUM0.
           PERFORM SCREENIO.


           MOVE "THE FOLLOWING ACTION WILL BE TAKEN:" TO MSG1.

           MOVE SPACES TO MESS.
           MOVE MONTH-DESC(CURRENT-MM) TO MONTH-WORKER.
           STRING
           "     "
           "RATING SHEET RECORDS FOR " MONTH-WORKER " "
            CURRENT-YYYY " WILL BE READ."
             DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO MSG2.

           MOVE SPACES TO MESS.
           MOVE MONTH-DESC(NEXT-MM) TO MONTH-WORKER.
           STRING
           "     "
           "NEW RATING SHEET RECS FOR " MONTH-WORKER
            NEXT-YYYY " WILL BE CREATED."
             DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO MSG3.

           MOVE SPACES TO MESS.
           STRING
           "     "
           "YEAR-TO-DATE WILL BE BROUGHT FORWARD AND MONTH-TO-DATE"
             DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO MSG4.

           MOVE SPACES TO MESS.
           STRING
           "     "
           "CLEARED IN THE NEW RECORD."
             DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO MSG5.

           MOVE SPACES TO MESS.
           STRING
           "     "
           "IF CREATING RATING SHEETS FOR APRIL, YEAR-TO-DATE"
             DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO MSG6.

           MOVE SPACES TO MESS.
           STRING "     "
                  "WILL BE CLEARED IN THE NEW RECORD."
                                DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO MSG7.

           MOVE MSG-BUF TO WR-BUF.
           MOVE MSG-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *----------------------------------------------------------------
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
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO WREOM1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WREOM1-SYSDATE.
           DISPLAY WREOM1-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WREOM1-FORM-FIELDS TO WR-BUF.
           MOVE WREOM1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE WREOM1-HELP--FILE TO HELP-LINK-FILE.
           MOVE WREOM1-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WREOM1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       PRU-IN.

       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".

       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO DISPLAY-MO.
           MOVE ALP-YR TO DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SETUP-EOM-WR-DATE.
      * SETUP CURRENT MONTH/YEAR DATE:
           MOVE DISPLAY-MO         TO DATE-YYMMDD-MM.
           MOVE 1                  TO DATE-YYMMDD-DD.
           MOVE DISPLAY-YR         TO DATE-YYMMDD-YY.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE -1                 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-MM            TO DATE-YYYYMM-MM.
           MOVE NDTE-CCYY            TO DATE-YYYYMM-YYYY.
           MOVE DATE-YYYYMM        TO CURRENT-YYYYMM.
       SETUP-NEXT-EOM-WR-DATE.
      * SETUP NEXT MONTH DATE:
           MOVE DISPLAY-MO        TO DATE-MMDDYY-MM.
           MOVE 1                 TO DATE-MMDDYY-DD.
           MOVE DISPLAY-YR        TO DATE-MMDDYY-YY.
           PERFORM   CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM       TO NEXT-YYYYMM.
       SETUP-LAST-EOY-WR-DATE.
      * SETUP LAST YEAR/MONTH DATE:
           MOVE DISPLAY-MO         TO DATE-YYMMDD-MM.
           MOVE 1                  TO DATE-YYMMDD-DD.
           MOVE DISPLAY-YR         TO DATE-YYMMDD-YY.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE -1                 TO NDTE-HOLD.
           PERFORM INCREMENT-YEARS.
           MOVE NDTE-MM            TO DATE-MMDDYY-MM.
           MOVE NDTE-CCYY          TO DATE-YYYYMM-YYYY.
           MOVE DATE-YYYYMM        TO LAST-YYYYMM.

       COPY "LIBSP/SPWRCL.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPWR1IN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

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
              DESTROY WREOM1-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
