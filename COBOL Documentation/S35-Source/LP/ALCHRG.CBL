       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ALCHRG.
       DATE-WRITTEN.    021607.
      **************************************************************************
      * NIGHTLY PROGRMA TO CREATE "AL" TRANSACTIONS
      *             THIS PROGRAM IS CALLED TO BRING LATE CHARGES CURRENT
      *               BATCH ONLY
      *
      *  070216 CLS CREATE 'AL' TRANSACTIONS, LP RECORD, TR RECORD. PROGRAM
      *             TO BE RUN NIGHTLY.  REGACC PR# 152
      *  070314 CLS REMOVED FIELDS FROM DISPLAY-FIELDS SECTION
      *  070405  CS FIXED THE BRANCH HEADINGS PER KEVIN
      *  071001  CS CINDY & BARB - DO A CHMOD ON TRFILE, THEY ARE RUNNING IN
      *             EOCRON BATCH & ACCESS ON TRFILES WAS MESSED UP ON THE
      *             FIRST OF THE MONTH, REGACC.
      *  140508  GET RID ALCHRG.CMD, DO CHMOD WITH COBOL SYSTEM CALL &
      *          CHANGE ACCESS ON CHMOD TO 770
      *
      *  150924 {PL#00904} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *         ADDED CLEAR ROUTINE FOR LR-FILE SO WHEN CREATING
      *         NEW LR-REC.
      *  151222 SET LP-BRNO FROM LN-OWNBR
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      * KEC 2016.0914 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         SET LR-BRNO USING LN-OWNBR.
      *
      * KEC 2016.1025 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING SAVE-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *
      * BAH 170316 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0808 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED "MOVE LN-OWNBR TO LP-BRNO." AFTER CLEARING LP-REC
      *         AND BEFORE WRITING A NEW LP RECORD.
      *
      * BAH 181203 GLOBAL LNFILE
      * BAH 181221 GLOBAL LPFILE
      * BAH 20190625 REMOVED LN-REVOLVING, TP-REVOLVING PT0153
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 20220422 UPDATE TP-ACTIONCD2, SECURITIZATION #1551
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220912 UPDATE TP-LCDUE WITH LATE CHARGE ASSESSED, VERITEC #1570
      *              IS STILL ADDED IN TP-INTDUE
      *  BAH 20221108 USE TODAYS-DATE FOR TRANSACTION FILE AND POST DATE,
      *               NOT AGING DATE (E-COMPLISH DELAY NEEDS TO BE 2 DAYS
      *               RETRO). WE CANT BE CREATING TRANS RECORDS IN A PRIOR
      *               TRANSACTION MONTH FOR DATA INTEGRITY PURPOSES. LATE
      *               CHARGES WILL STILL BE CALCULATED AND ASSESSED USING
      *               EFFECTIVE DATE ON THE SCREEN. #1577
      *  BAH 20221214 REMOVE THE CHMOD #1583
      *  BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/ALCHRG.CBL S35 04/22/25-12:15:51 >".

       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
      *----------------------------------------------------------------
       01  PR-IFN        PIC XX       VALUE "PR".

       01  SAVE-LP1-KEY  PIC 9(17)    VALUE ZERO.

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  TODAYS-DATE          PIC 9(8).
       01  TOTAL-FG             PIC X        VALUE "N".
       01  EXITPROG             PIC X(9)     VALUE "LP/LPR2MU".
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGE-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  BEG-CLASS              PIC 9(3).
           03  END-CLASS              PIC 9(3).
           03  REPORT-DETAIL          PIC X.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  TOTALS-FG              PIC X.
               88  TOTALS-FG-VALID    VALUE  "Y" "N" "T" "R".
               88  TOTALS-YES         VALUE  "Y" "T".
               88  RANGES-YES         VALUE  "Y" "R".

       01  VERSION-CONTROL     PIC X    VALUE "B".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBGB/PASSWDW.CPY".

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(35)    VALUE
           " ASSESS LATE CHARGE REPORT         ".
           03  FILLER           PIC X(6)     VALUE " FOR: ".
           03  H-AGEDATE        PIC X(10).

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER      PIC X(10)    VALUE "BRANCH   :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER      PIC X(28)    VALUE
           "BRNO  ACCOUNT #        NAME ".
           03  FILLER       PIC X(24) VALUE SPACES.
           03  FILLER      PIC X(48)    VALUE
           "AL DATE            AL AMOUNT         LC OWING  ".

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  RUN-DISPLAY-MESS.
           03  FILLER           PIC X(24)    VALUE
           "REPORT IN PROGRESS......".
           03  RUN-MESS-BR      PIC 9(4).

       01  TOTALS-TAB.
           03  TOTAL-TAB OCCURS 2.
               05  T-NO            PIC S9(7).
               05  T-AL-AMOUNT     PIC S9(7)V99.
               05  T-LC-OWING      PIC S9(7)V99.

       01  LEV                    PIC 9(6)  COMP.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-OWNBR     PIC 9(4).
           03  FILLER      PIC X(2).
           03  D-LOAN-NUM  PIC X(16).
           03  FILLER      PIC X.
           03  D-NAME      PIC X(27).
           03  FILLER      PIC X.
           03  D-AL-DATE   PIC 99/99/99.
           03  FILLER      PIC X(9).
           03  D-AL-AMOUNT PIC Z,ZZZ,ZZ9.99-.
           03  FILLER      PIC X(4).
           03  D-LC-OWING  PIC Z,ZZZ,ZZ9.99-.


       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME           PIC X(20).
           03  FILLER              PIC X(112).
       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER              PIC X(46).
           03  S-SIGN1             PIC X.
           03  FILLER              PIC X.
           03  S-NO                PIC ZZZZ9-.
           03  FILLER              PIC X(08).
           03  S-AL-AMOUNT         PIC Z,ZZZ,ZZ9.99-.
           03  FILLER              PIC X(4).
           03  S-LC-OWING          PIC Z,ZZZ,ZZ9.99-.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(20).
           03  RANGE-SELECTION   PIC X(112).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ALCHRG_DEF.CPY".
       COPY "LIBLP/ALCHRG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ALCHRG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ALCHRG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      * NEEDED FOR GP-NO-LC-IN-ARREARAGE IN LIBLP/CPCORP DETAIL-LINE

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-NO1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE BR-NO TO RUN-MESS-BR
              MOVE RUN-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF (IO-FG = 9) OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

      *  SKIP PAID OUTS, P&L'S
           IF NOT LN-POCD-OPEN
              GO TO NEXT-LN.

           PERFORM GET-SP.

           IF SP-AUTO-ASSESS-LCHG NOT = "Y"
              GO TO NEXT-LN.

           MOVE AGE-DATE   TO IBPC-DATE.
           PERFORM IBPC-TEST.
           IF NOT (IBPC-FG = "P" OR LN-LOANTYPE = "I")
              GO TO NEXT-LN.

           MOVE "AL"         TO LCAP-LPTRCD.
           MOVE AGE-DATE     TO LCAP-PAYDATE.
           MOVE LN-1STPYDATE TO LCAP-1STPYDATE.
           MOVE 0            TO LCAP-LPAPLC.
           MOVE 0            TO LCAP-LPAPINT.
           MOVE 0            TO LCAP-TRAMT.
           PERFORM LATE-CHARGE-APPLY.
           IF LCAP-OWE = ZERO
              GO TO SKIP-LATE-CHARGE-ASSESS.

           PERFORM CREATE-AL-LP-RECORD.

           PERFORM UPDATE-LN1-FILE.

           PERFORM CREATE-AL-TR-RECORD.

           PERFORM UPDATE-NOTICE-FILE.

           MOVE "Y" TO RECORDS-FOUND.
           PERFORM SET-BRANCH-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.

       SKIP-LATE-CHARGE-ASSESS.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-TRP1-FILE.

           MOVE "Y" TO FIRST-TIME.

           MOVE 1 TO LEV.
           PERFORM ZERO-TOTAL-TBL.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF TOTALS-YES
              PERFORM GRAND-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE ALCHRG-QUEUE-POS TO Q-POS.
           MOVE ALCHRG-COPIES-POS TO C-POS.
           MOVE ALCHRG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE AGE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO H-AGEDATE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           MOVE 1 TO LEV.
           PERFORM ZERO-TOTAL-TBL.

           MOVE 2 TO LEV.
           PERFORM ZERO-TOTAL-TBL.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXT-CONAME-USER-LOC TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      *----------------------------------------------------------------
       POSTD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO POSTD.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-EDIT AGE-DATE
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO POSTD.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S NA000BR1." TO VDU
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
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO PRINT-DET.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       PRINT-DET.
           MOVE "E  A010DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINT-DET.

           MOVE VDUBUF TO REPORT-DETAIL.
           IF NOT (REPORT-DETAIL = "Y" OR "N")
              MOVE "ENTER Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO PRINT-DET.
      *----------------------------------------------------------------
       TOTALS-YN.
           MOVE "E  A010TOT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PRINT-DET.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TOTALS-YN.

           IF NOT (VDUBUF = "Y" OR "N")
                 MOVE "ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO TOTALS-YN
           ELSE
                 MOVE VDUBUF TO TOTALS-FG.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DETAIL." TO VDU.
           MOVE REPORT-DETAIL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TOT." TO VDU.
           MOVE TOTALS-FG TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           IF REPORT-DETAIL NOT = "Y"
              GO TO DETAIL-LN-ACCTOT.

           MOVE LN-OWNBR  TO D-OWNBR.
           MOVE LN-ACCTNO TO D-LOAN-NUM.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = ZERO
              MOVE "NOT FOUND"  TO D-NAME
           ELSE
              STRING BW-LNAME, ", ", BW-FNAME DELIMITED BY "   "
                  INTO D-NAME.

           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-AL-DATE.
           MOVE LP-APINTOWE      TO D-AL-AMOUNT.
           MOVE LP-LCBAL         TO D-LC-OWING.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       DETAIL-LN-ACCTOT.
           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           GO TO END-DETAIL-LINE-PROCESS.

       DETAIL-LN-ACCTBL.
           ADD 1 TO T-NO(LEV).
           ADD LP-APINTOWE        TO T-AL-AMOUNT(LEV).
           ADD LP-LCBAL           TO T-LC-OWING(LEV).

       END-DETAIL-LINE-PROCESS.
           EXIT.

      ********************************
       UPDATE-LN1-FILE SECTION.
      ********************************

           ADD LCAP-PARTIALS     TO LN-LCPARTIALS.
           MOVE LCAP-LCPAID      TO LN-LCPAID.
           ADD LCAP-NOLCHG       TO LN-TOTNOLCHG.
           MOVE LCAP-LCPDTH-DATE TO LN-LCPDTH-DATE.
           ADD LCAP-OWE          TO LN-LCBAL.
           PERFORM REWRITE-LN1-FILE.

       UPDATE-LN1-FILE-EXIT.
           EXIT.

      ********************************
       CREATE-AL-LP-RECORD SECTION.
      ********************************

           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO.
           MOVE 0             TO LP-SEQNO.

       CREATE-AL-LP-NEXT-SEQ.
           ADD 1        TO LP-SEQNO.
           MOVE LP1-KEY TO SAVE-LP1-KEY.
           PERFORM READ-LP1-FILE.
           IF IO-GOOD
      *       UNLOCK LP-FILE
              IF LP-SEQNO NOT = 99999
                 GO TO CREATE-AL-LP-NEXT-SEQ.

           PERFORM CLEAR-LP-FILE.

           MOVE SAVE-LP1-KEY       TO LP1-KEY.
           MOVE "AL"               TO LP-TRCD.
           MOVE TODAYS-DATE        TO LP-TRDATE.
           MOVE AGE-DATE           TO LP-PAYDATE.
           MOVE EXT-CONAME-USER-ID TO LP-USERID.
           MOVE LN-OWNBR           TO LP-POSTING-BR.
           MOVE "ALCHG"            TO LP-REFNO.
           MOVE IBPC-FG            TO LP-IBPC.
           MOVE LN-1STPYDATE       TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL     TO LP-PDTH-DATE.
           MOVE LCAP-LCPDTH-DATE   TO LP-LCPDTH-DATE.
           MOVE LCAP-NOLCHG        TO LP-NOLCHG.
           MOVE LCAP-OWE           TO LP-APINTOWE.

           ADD LCAP-OWE TO LN-LCBAL GIVING LP-LCBAL.

           MOVE LN-SSNO(1)    TO LP-SSNO.
           MOVE LN-OTHBAL     TO LP-OTHBAL.
           MOVE LN-OT2BAL     TO LP-OT2BAL.
           MOVE LN-INTBAL     TO LP-INTBAL.
           MOVE LN-CURBAL     TO LP-CURBAL.
           MOVE LN-LCPARTIALS TO LP-LCPARTIALS.
           MOVE LN-LCPAID     TO LP-LCPAID.

           MOVE 0          TO LP-INTDUE.

           PERFORM SET-POSTDATE.
           MOVE POST-DATE     TO LP-POSTDATE.
           MOVE POST-TIME     TO LP-POSTTIME.

           PERFORM WRITE-LP1-FILE.
           IF IO-BAD
              PERFORM CLOSE-LP1-FILE.

       CREATE-AL-LP-RECORD-EXIT.
           EXIT.

      **************************************************
       CREATE-AL-TR-RECORD SECTION.
      **************************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE LN-OWNBR     TO TP-BRNO.
           MOVE TODAYS-DATE  TO TP-DATE.
           MOVE LN-CLASS     TO TP-CLASS.
           MOVE LN-ACCTNO    TO TP-ACCTNO.

       CREATE-AL-TR-NEXT.
           ADD 1             TO TP-LINE.
           MOVE TRP1-KEY     TO LPWSTRP-TRP1-KEY.

           PERFORM READ-TRP1-FILE.
           IF IO-FG = 0
      *       UNLOCK TRP-FILE
              GO TO CREATE-AL-TR-NEXT.

           PERFORM CLEAR-TRP-FILE.

           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.

           MOVE "AL"        TO TP-TRCD.
           MOVE LP-PAYDATE  TO TP-PAYDATE.
           MOVE "ALCHG"     TO TP-REFNO.
           MOVE LP-LCBAL    TO TP-LCBAL.
           MOVE LN-CURBAL   TO TP-CURBAL.
           MOVE LN-SSNO(1)  TO TP-SSNO.
           MOVE LN-ORGTERM  TO TP-ORGTERM.
           MOVE LN-DLNO     TO TP-DLNO.
           MOVE LN-LNAMT    TO TP-LNAMT.
           MOVE LN-1STPYDATE TO TP-ORIG-1STPYDATE.
           MOVE LP-OTHBAL   TO TP-OTHBAL.
           MOVE LP-INTBAL   TO TP-INTBAL.
           MOVE TP-OT2BAL   TO TP-OT2BAL.
           ADD  LP-INTDUE LP-APINTOWE GIVING TP-INTDUE.
           ADD LP-APINTOWE  TO TP-LCDUE.

           MOVE LP-PDTH-DATE     TO TP-PDTH-DATE.
           MOVE LP-LCPDTH-DATE   TO TP-LCPDTH-DATE.
           MOVE LP-POSTING-BR    TO TP-POSTING-BR.
           MOVE LP-USERID        TO TP-USERID.
           PERFORM SET-POSTDATE.
      *PAGE 2
           MOVE POST-DATE     TO TP-POSTDATE.
           MOVE POST-TIME     TO TP-POSTTIME.
           MOVE LN-SALEFIN    TO TP-SALEFIN.
           MOVE LN-INSURED    TO TP-INSURED.
      *    MOVE LN-INSOURS    TO TP-INSOURS.
           MOVE LN-LOANTYPE   TO TP-LOANTYPE.
      * OTHER BRANCH FLAG
           MOVE LN-SERVACCT   TO TP-SERVACCT.
      *    MOVE LP-REV        TO TP-REV.
      *                       TO TP-STATUSFG.
      *                       TO TP-OTHBR-FG.
           MOVE LN-INSTYPES   TO TP-INSTYPES.
           MOVE LN-LAWCODE    TO TP-LAWCODE.
      *    MOVE LN-POCD       TO TP-POCD.
      *                       TO TP-MAX-DEFER-FG.

           MOVE LN-PLCD       TO TP-PLCD.
      *    MOVE LN-MORELN     TO TP-MORELN.
           MOVE LN-MAKERCD(1) TO TP-MAKER.
           MOVE LP-IBPC       TO TP-IBPC.
           IF LN-JDDATE NOT = ZERO
              MOVE "J"        TO TP-JDCD.
      *PAGE 3
           MOVE LN-SPRCLASS   TO TP-SPRCLASS.
           MOVE LN-PLTYPE     TO TP-PLTYPE.
           MOVE LN-UNITPER-CD TO TP-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO TP-UNITPER-FREQ.
           MOVE LN-ACTIONCD2    TO TP-ACTIONCD2.

           PERFORM WRITE-TRP1-FILE.

       CREATE-AL-TR-RECORD-EXIT.
           EXIT.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE TODAYS-DATE    TO CRNOR2-SYSDATE.
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

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING CLASS       :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TOTALS / RANGES    :" TO RANGE-LINE.
           MOVE TOTALS-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPORT TYPE        :" TO RANGE-LINE.
           MOVE REPORT-DETAIL           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       ZERO-TOTAL-TBL SECTION.
      ******************************************************************
           MOVE 0 TO T-NO(LEV)
                     T-AL-AMOUNT(LEV)
                     T-LC-OWING(LEV).

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC
           MOVE 2 TO LN-FEED
           PERFORM WRITE-PR-REC.

      *----------------------------------------------------------------
       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************

           MOVE "Y" TO TOTAL-FG.
           MOVE SPACES TO H-BRNO.
           MOVE "** ALL BRANCHES **" TO H-BRDESC.
           MOVE 1 TO LN-FEED.
           MOVE "GRAND TOTALS:" TO S-SUMNAME.
           MOVE 2 TO LEV.
           MOVE T-NO(LEV) TO S-NO.
           MOVE T-AL-AMOUNT(LEV) TO S-AL-AMOUNT.
           MOVE T-LC-OWING(LEV)  TO S-LC-OWING.
           MOVE "#"              TO S-SIGN1.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-GRAND-TOTALS.
           EXIT.

      *************************************************************
       CLASS-SCAN SECTION.
      *************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPTRPCL.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO ALCHRG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ALCHRG-SYSDATE.
           DISPLAY ALCHRG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ALCHRG-FORM-FIELDS TO WR-BUF.
           MOVE ALCHRG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE ALCHRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE ALCHRG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ALCHRG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       GET-SP.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
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
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF  = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY ALCHRG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
