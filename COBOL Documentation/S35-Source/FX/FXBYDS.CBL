       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXBYDS.
       DATE-WRITTEN.    090898.
      ******************************************************************
      *
      *                 (FX)
      *
      *  DESCRIPTION:  FIX STATISTICS (BY/DS)
      *
      *                RECONSTRUCTS
      *                ------------
      *
      *                    BY:  PAGE 2  -  NET GAIN/LOSS
      *                                    WEIGHTED AVERAGES
      *                         PAGE 3  -  DELINQUENCY/BANKRUPT DELINQUENCY
      *                         PAGE 4  -  REPO NET GAIN/LOSS
      *                         PAGE 7  -  P&L  NET GAIN/LOSS
      *                         PAGE 15 -  CURRENT UNEARNED
      *
      *                    DS:  PAGE 1  -  WEIGHTED AVERAGES
      *                                    RECOURSE AMOUNTS
      *                         PAGE 2  -  P&L  NET GAIN/LOSS
      *                                    JUDGEMENT NET GAIN/LOSS
      *                         PAGE 3  -  DELINQUENCY/BANKRUPT DELINQUENCY
      *                         PAGE 4  -  REPO NET GAIN/LOSS
      *                         PAGE 6  -  CURRENT UNEARNED
      *
      *-----------------------------------------------------------------
      * REV:
      *  JTG 091598 CORRECTED UPDATE OF PAGE 4 REPO P&L FOR REDEMED REPOS
      *             WHICH SHOULD GO ON PAGE 2
      *  BLV 990106 CORRECTED F3 ALL BRANCHES GAVE "ERR READING GR"
      *  KEC 021299 ADDED CHECK TO SKIP REVOLVING ACCOUNTS.
      *  MJD 990420 CHANGED TO INCLUDE/EXCLUDE O2 IN DELINQ BASED
      *             ON GP-DELINQ-WITH-O2
      *  JTG 990716 UPDATED LOGIC TO UPDATE DS & TY FILES FOR REVOLVING
      *  JTG 990720 ADDED LOGIC FOR LN-ACTIVE-FG     REGENCY REVOLVING #45
      *  JTG 990801 CORRECTED BUG IN UPDATE OF NORMAL ACCOUNTS
      *  BLV 990818 CHANGE P&L UPDATE FOR BY & DS TO UPDATE SLOTS 4 & 5 OF
      *             REPO TABLE IF REPO DATE > PL DATE, ALWAYS UPDATE
      *             DS-PL OUTSTANDING WHETHER OR NOT REPO
      *  KEC 991005 ADDED 'EXITPROG' AND 'PROGID' LOGIC.
      *  JTG 000412 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *  BLV 000713 MODIFIED TO UPDATE REPO/PL BUCKETS 4 & 5 REGARDLESS OF
      *             GP FLAG OR PL & REPO DATES, TFC PR# 77
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *  BAM 160511 FIX DS-DATE FORMAT, BY-DATE FORMAT, DS-UNEARNED, PD0003
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * KEC 2017.0103 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      * BLM 180101 FIX TODAYS-DATE, TODAYS-BY-DATE-CCYYMM, TODAYS-DS-DATE-CCYYMM
      *            TODAYS-DATE WAS MMDDYY & MESSING UP CONTRACTUAL
      * BAH 181203 GLOBAL LNFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 20190625 REMOVED LN-ZERO-BAL-CNT AND LN-REVOLVING PT0153
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXBYDS.CBL S34 05/21/21-08:34:09 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *01  HOLD-BY1-KEY      PIC X(13).
      *01  HOLD-DS1-KEY      PIC X(19).
       01  SAVE-OUTBAL       PIC S9(7)V99 VALUE 0.

       01  ONE               PIC S9.
       01  SUB1              PIC 99.
       01  SUB2              PIC 99.
       01  REC-EXISTS        PIC X     VALUE "N".

       01  ERRCD             PIC X     VALUE " ".

       01  ACCEPT-FG         PIC X(7)  VALUE " ".

       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  TODAYS-BY-DATE-CCYYMM       PIC 9(6)      VALUE 0.
       01  TODAYS-DS-DATE-CCYYMM       PIC 9(6)      VALUE 0.
       01  BEG-MONTH-DATE       PIC 9(8)      VALUE 0.

       01  TODAYS-DATE          PIC 9(8)      VALUE 0.

      *01 ELM-FIX-ONE           PIC S9        VALUE 0.
      *01 ELM-MEMO.
      *   03  ELM-M1            PIC X(9).
      *   03  FILLER            PIC X(27).
      *   03  ELM-M2            PIC X(5).


      *-----------------------------------------------------------------
       01  BEG-BRANCH        PIC 9(4)      VALUE 0.
       01  END-BRANCH        PIC 9(4)      VALUE 0.
       01  BR1               PIC 9999      VALUE 0.
       01  BR2               PIC 9999      VALUE 0.
       01  CL1               PIC 99        VALUE 0.
       01  CL2               PIC 99        VALUE 0.
      *01  BMO-DEL           PIC X         VALUE " ".
       01  GROUP-FLAG        PIC X.
       01  ENT-GROUP         PIC X(4).

      *=================================================================
      *
      *    P R O C E S S I N G - W O R K E R S
      *
      *=================================================================
       01  PROCESSING-FLAG             PIC X(01)     VALUE SPACES.
           88  PROCESSING-BRANCH-CHANGE              VALUE SPACES.
           88  PROCESSING-CLEAR-BY                   VALUE "B".
           88  PROCESSING-CLEAR-DS                   VALUE "D".
           88  PROCESSING-UPDATING                   VALUE "U".

      *-----------------------------------------------------------------
       01  PROCESSING-TEXT-CLEAR-BY.
           03  FILLER PIC X(30) VALUE "CLEARING THE 'BY' FIELDS......".
           03  FILLER PIC X(30) VALUE "..............................".

       01  PROCESSING-TEXT-CLEAR-DS.
           03  FILLER PIC X(30) VALUE "CLEARING THE 'DS' FIELDS......".
           03  FILLER PIC X(30) VALUE "..............................".

       01  PROCESSING-TEXT-UPDATING.
           03  FILLER PIC X(30) VALUE "UPDATING 'BY' AND 'DS' FILES..".
           03  FILLER PIC X(30) VALUE "..............................".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPBYCLW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       01  FXBYDS-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXBYDS_DEF.CPY".
       COPY "LIBFX/FXBYDS_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXBYDS_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - C O N T R O L
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "FXBYDS" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXBYDS-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM INITIAL-ROUTINE.
           IF ( ERRCD NOT = " " )
              GO TO END-ROUTINE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM GET-GPENV.

           IF ( GROUP-FLAG = "N" )
              MOVE BR1 TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = 0 )
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( BR-NO > BR2 )
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              DS-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
       GLOBAL-UPDATE SECTION.
      ******************************************************************

           MOVE SPACES TO PROCESSING-FLAG.
           MOVE 9999   TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM CLEAR-BY.
           PERFORM CLEAR-DS.
           PERFORM UPDATE-BY-DS.

       GLOBAL-UPDATE-EXIT.
           EXIT.

      ******************************************************************
       CLEAR-BY SECTION.
      ******************************************************************

           PERFORM OPEN-BY1-FILE.

           MOVE TODAYS-BY-DATE-CCYYMM TO BY-DATE-CCYYMM
                                         QBY1-WBEG-DATE-CCYYMM
                                         QBY1-WEND-DATE-CCYYMM.
           MOVE BR-NO                 TO BY-BRNO
                                         QBY1-WBEG-BRNO
                                         QBY1-WEND-BRNO.
           MOVE CL1                   TO BY-CLASS
                                         QBY1-WBEG-CLASS.
           MOVE CL2                   TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE.
       CLEAR-BY-NEXT.
           PERFORM READ-BY1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CLEAR-BY-EXIT.

           MOVE "B" TO PROCESSING-FLAG.
           PERFORM SEND-PROGRESS-INDICATOR.

           IF BY-DATE-CCYYMM NOT = TODAYS-BY-DATE-CCYYMM
              GO TO CLEAR-BY-EXIT.

           IF BY-BRNO NOT = BR-NO
              GO TO CLEAR-BY-EXIT.

           IF BY-CLASS > CL2
              GO TO CLEAR-BY-EXIT.

      * CLEAR CURRENT BY-PERIOD-TBL:
           PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 11
              MOVE 0 TO BY-NO(BYCUR BYNET BYSUB3)
                        BY-AMT(BYCUR BYNET BYSUB3)
                        BY-O2(BYCUR BYNET BYSUB3)
                        BY-INT(BYCUR BYNET BYSUB3)
           END-PERFORM.

      * CLEAR CURRENT BY-REPO-ACTIVITY-TBL:
           PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 1
              MOVE 0 TO BY-NO-REPO(BYCUR BYNET BYSUB3)
                        BY-AMT-REPO(BYCUR BYNET BYSUB3)
                        BY-O2-REPO(BYCUR BYNET BYSUB3)
                        BY-INT-REPO(BYCUR BYNET BYSUB3)
           END-PERFORM.

           PERFORM VARYING BYSUB3 FROM 3 BY 1 UNTIL BYSUB3 > 5
              MOVE 0 TO BY-NO-REPO(BYCUR BYNET BYSUB3)
                        BY-AMT-REPO(BYCUR BYNET BYSUB3)
                        BY-O2-REPO(BYCUR BYNET BYSUB3)
                        BY-INT-REPO(BYCUR BYNET BYSUB3)
           END-PERFORM.

      * CLEAR DELINQUENCY TABLE TODATE & BEGINNING OF MONTH:
           PERFORM VARYING BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                      AFTER BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
      *      TODATE:
              MOVE 0 TO BY-UNITS(BYNOW BYSUB2 BYSUB3)
                        BY-OUTBAL(BYNOW BYSUB2 BYSUB3)
           END-PERFORM.

      * CLEAR BANKRUPT DELINQUENCY TABLE TODATE & BEGINNING OF MONTH:
           PERFORM VARYING BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                      AFTER BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
      *      TODATE:
              MOVE 0 TO BY-UNITS-BNKRPT(BYNOW BYSUB2 BYSUB3)
                        BY-OUTBAL-BNKRPT(BYNOW BYSUB2 BYSUB3)
           END-PERFORM.

      * CLEAR P&L TABLE TODATE:
           PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
              MOVE 0 TO BY-NO-PL(BYCUR BYNET BYSUB3)
                        BY-AMT-PL(BYCUR BYNET BYSUB3)
                        BY-O2-PL(BYCUR BYNET BYSUB3)
                        BY-INT-PL(BYCUR BYNET BYSUB3)
           END-PERFORM.

      * CLEAR CURRENT BY-UNEARNED TABLE TODATE:
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 6
                 MOVE 0 TO BY-UNEARNED-AMT(BYCUR BYNET BYSUB3)
              END-PERFORM.

      * MISC:
           MOVE 0 TO BY-SUMWGTTM
                     BY-SUMWGTAV
                     BY-SUMWGTTA.

           PERFORM REWRITE-BY1-FILE.

           GO TO CLEAR-BY-NEXT.

       CLEAR-BY-EXIT.
           PERFORM CLOSE-BY1-FILE.

      ********************************************
       CLEAR-DS SECTION.
      ********************************************

           PERFORM OPEN-DS1-FILE.

           MOVE TODAYS-DS-DATE-CCYYMM TO DS-DATE-CCYYMM
                                         QDS1-WBEG-DATE-CCYYMM
                                         QDS1-WEND-DATE-CCYYMM.
           MOVE BR-NO                 TO DS-BRANCH
                                         QDS1-WBEG-BRANCH
                                         QDS1-WEND-BRANCH.
           MOVE 0                     TO DS-NUM
                                         DS-CLASS
                                         QDS1-WBEG-NUM
                                         QDS1-WBEG-CLASS.
           MOVE ALL "9"               TO QDS1-WEND-NUM
                                         QDS1-WEND-CLASS.

           PERFORM START-DS1-FILE.
       CLEAR-DS-NEXT.
           PERFORM READ-DS1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CLEAR-DS-EXIT.

           MOVE "D" TO PROCESSING-FLAG.
           PERFORM SEND-PROGRESS-INDICATOR.

           IF DS-DATE-CCYYMM NOT = TODAYS-DS-DATE-CCYYMM
              GO TO CLEAR-DS-EXIT.

           IF DS-BRANCH NOT = BR-NO
              GO TO CLEAR-DS-EXIT.

           IF DS-CLASS < CL1 OR DS-CLASS > CL2
              GO TO CLEAR-DS-NEXT.

      * CLEAR DELINQUENCY TABLE TODATE & BEGINNING OF MONTH:
           PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 10
                      AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2

      * CURRENT:
               MOVE 0 TO DS-UNITS(DSNOW DSSUB2 DSSUB3)
                         DS-OUTBAL(DSNOW DSSUB2 DSSUB3)
           END-PERFORM.

      * CLEAR BANKRUPT DELINQUENCY TABLE TODATE & BEGINNING OF MONTH:
           PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 10
                      AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2

      * CURRENT:
               MOVE 0 TO DS-UNITS-BNKRPT(DSNOW DSSUB2 DSSUB3)
                         DS-OUTBAL-BNKRPT(DSNOW DSSUB2 DSSUB3)
           END-PERFORM.

      * CLEAR CURRENT DS-REPO-ACTIVITY-TBL:
           PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 1
              MOVE 0 TO DS-NO-REPO(DSCUR DSNET DSSUB3)
                        DS-AMT-REPO(DSCUR DSNET DSSUB3)
           END-PERFORM.

           PERFORM VARYING DSSUB3 FROM 3 BY 1 UNTIL DSSUB3 > 5
              MOVE 0 TO DS-NO-REPO(DSCUR DSNET DSSUB3)
                        DS-AMT-REPO(DSCUR DSNET DSSUB3)
           END-PERFORM.


      * CLEAR RECOURSE:
           PERFORM VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3
              MOVE 0 TO DS-RCOURSE-UNITS(DSSUB1)
                        DS-RCOURSE-AMT(DSSUB1)
           END-PERFORM.

      * CLEAR CURRENT DS-UNEARNED TABLE TODATE:
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 6
                 MOVE 0 TO DS-UNEARNED-AMT(DSNOW DSCUR DSSUB3)
              END-PERFORM.

      * CLEAR MISC:
           MOVE 0 TO DS-SUMWGTTM
                     DS-SUMWGTAV
                     DS-SUMWGTTA.

           MOVE 0 TO DS-OUTSUNIT(1) DS-OUTSAMT(1).
           MOVE 0 TO DS-JDOSUNIT(1) DS-JDOSAMT(1).
           MOVE 0 TO DS-PLOSUNIT(1) DS-PLOSAMT(1).

           PERFORM REWRITE-DS1-FILE.

           GO TO CLEAR-DS-NEXT.

       CLEAR-DS-EXIT.
           PERFORM CLOSE-DS1-FILE.


      ********************************************
       UPDATE-BY-DS SECTION.
      ********************************************
           PERFORM OPEN-LN3-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE CL1           TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE CL2           TO QLN3-WEND-CLASS.
           MOVE 0             TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.
       UPDATE-BY-DS-NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                         OR (LN-CLASS > CL2)
              GO TO UPDATE-BY-DS-EXIT.

           MOVE "U" TO PROCESSING-FLAG.
           PERFORM SEND-PROGRESS-INDICATOR.

           IF LN-POCD-NONLOAN
              GO TO UPDATE-BY-DS-NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
              GO TO UPDATE-BY-DS-NEXT-LN.


      * FIX LAST MONTH COUNT (MERCURY ONE TIME FIX)
      *    IF ACCEPT-FG = "MERCFIX"
      *       PERFORM TEST-FOR-ELM-FIX.


      * GET DELINQUENCY FOR TODAY

           MOVE TODAYS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           IF LN-CURBAL = 0
              MOVE 0 TO ONE
           ELSE
              MOVE +1 TO ONE.

           PERFORM UPDATE-BY1.
           PERFORM UPDATE-DS1.

       UPDATE-BY-DS-REWRITE-TEST.
           GO TO UPDATE-BY-DS-NEXT-LN.

       UPDATE-BY-DS-EXIT.
           PERFORM CLOSE-LN3-FILE.

      ******************************************************************
       INITIAL-ROUTINE SECTION.
      ******************************************************************
           MOVE " " TO ERRCD.

       INIT-ACCEPT-TODAY.
           MOVE "ERNM060TODAYS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-TODAY.

           MOVE VDU-DATE-YYYYMMDD TO TODAYS-DATE.
           IF TODAYS-DATE = 0
              GO TO INIT-ACCEPT-TODAY.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR1.
           MOVE "   A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO INITIALIZE-ENDIT.

           IF ( VDUSTATUS = "16" )
              GO TO INIT-ACCEPT-BR-ALL.

           IF ( VDUSTATUS = "1U" )
              GO TO INIT-ACCEPT-TODAY.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO INIT-ACCEPT-BR1
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR1.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO INIT-ACCEPT-BR1.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BR1 BEG-BRANCH
              MOVE 9999 TO BR2 END-BRANCH
              GO TO INIT-ACCEPT-CL1.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BR1 BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BR1 TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO INITIALIZE-ENDIT.

           IF ( VDUSTATUS = "1U" )
              GO TO INIT-ACCEPT-BR1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR2.

           MOVE VDUNUM0 TO BR2 END-BRANCH.

           IF ( BR1 > BR2 )
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-BR1.

           GO TO INIT-ACCEPT-CL1.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR-ALL.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BR1 BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO BR2 END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       INIT-ACCEPT-CL1.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "16"
              GO TO INIT-ACCEPT-CL-ALL.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-BR2.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-CL1.

           MOVE VDUNUM0 TO CL1.

      *-----------------------------------------------------------------
       INIT-ACCEPT-CL2.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-CL1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-CL2.

           MOVE VDUNUM0 TO CL2.

           IF CL1 > CL2
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-CL1.

           GO TO INIT-BMO-DEL.

      *-----------------------------------------------------------------
       INIT-ACCEPT-CL-ALL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO CL1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO CL2 VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       INIT-BMO-DEL.
      *    MOVE "E  A010BMODELINQ." TO VDU.
      *    PERFORM SCREENIO.

      *    IF VDUSTATUS = "1>"
      *       GO TO INITIALIZE-ENDIT.

      *    IF VDUSTATUS = "1U"
      *       GO TO INIT-ACCEPT-CL2.

      *    IF NOT (VDUSTATUS = "00" OR "1D")
      *       GO TO INIT-BMO-DEL.

      *    MOVE VDUBUF TO BMO-DEL.

      *    IF NOT (BMO-DEL = "Y" OR "N")
      *       GO TO INIT-BMO-DEL.

           GO TO INIT-ACCEPT-ACCEPT.

      *-----------------------------------------------------------------
      *    Y - RUN TO COMPLETION
      *    N - CONVERT OPTION
      *-----------------------------------------------------------------
       INIT-ACCEPT-ACCEPT.
           MOVE "S  A040MESS2." TO VDU.
           PERFORM SCREENIO.
           MOVE "E  A070ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-CL2.

           IF VDUSTATUS NOT = "00"
              GO TO INIT-ACCEPT-ACCEPT.

           MOVE VDUBUF TO ACCEPT-FG.

           IF ACCEPT-FG = "N"
              GO TO INITIALIZE-ENDIT.

           IF NOT (ACCEPT-FG = "Y" )
      *    IF NOT (ACCEPT-FG = "Y" OR "MERCFIX")
              GO TO INIT-ACCEPT-ACCEPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SET BEGINNING OF MONTH & SET TODAYS-BY-DATE-CCYYMM:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TODAYS-DATE TO NUM-DATE.
           MOVE 1 TO NUM-DA.
           MOVE NUM-DATE TO BEG-MONTH-DATE.

           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO TODAYS-BY-DATE-CCYYMM
                               TODAYS-DS-DATE-CCYYMM.

           MOVE "PROGRAM IN PROGRESS........................" TO LEGEND.
           PERFORM SEND-LEGEND.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-BY1 SECTION.
      ******************************************************************
           MOVE "N" TO REC-EXISTS.
           PERFORM OPEN-BY1-FILE.

       UPDATE-BY1-EXISTS.
           PERFORM SET-BY1-KEY.
           PERFORM READ-BY1-FILE.
           IF IO-FG = 9
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 MOVE "Y" TO REC-EXISTS
                 PERFORM CLEAR-BY-FILE
                 PERFORM SET-BY1-KEY
                 PERFORM WRITE-BY1-FILE
                 IF IO-FG NOT = 0
                    GO TO IO-ERROR
                 ELSE
                    GO TO UPDATE-BY1-EXISTS.

           IF LN-PLDATE NOT = 0
              PERFORM PL-BY1-UPD
           ELSE
              IF LN-REPOCD = "X" OR "S"
                 PERFORM REPO-BY1-UPD
              ELSE
                 PERFORM NORMAL-BY1-UPD.

           IF GP-UNEARNED-STATS = "Y"
              PERFORM COMPUTE-EARNINGS
              ADD EARN-UNEARN(1) TO BY-UNEARNED-AMT(BYCUR BYNET 1)
              ADD EARN-UNEARN(2) TO BY-UNEARNED-AMT(BYCUR BYNET 2)
              ADD EARN-UNEARN(3) TO BY-UNEARNED-AMT(BYCUR BYNET 3)
              ADD EARN-UNEARN(4) TO BY-UNEARNED-AMT(BYCUR BYNET 4)
              ADD EARN-UNEARN(5) TO BY-UNEARNED-AMT(BYCUR BYNET 5)
              IF NOT GP-INCL-OTH-IN-EARNED-N
                 ADD EARN-UNEARN(6) TO BY-UNEARNED-AMT(BYCUR BYNET 6).

           PERFORM REWRITE-BY1-FILE.
       UPDATE-BY1-EXIT.
           PERFORM CLOSE-BY1-FILE.

      ****************************************************
       NORMAL-BY1-UPD SECTION.
      ****************************************************

      * SETUP OUTBAL WORKER FOR UPDATE OF STATS

           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL GIVING SAVE-OUTBAL.

           PERFORM GET-SRCD-SUB.
           ADD ONE       TO BY-NO(BYCUR BYNET SRCD).
           ADD LN-CURBAL TO BY-AMT(BYCUR BYNET SRCD).
           ADD LN-OT2BAL TO BY-O2(BYCUR BYNET SRCD).

      * ONE-TIME FIX FOR WORLD; THEY DIDN'T UPDATE THE GP-FISCAL TABLE
      * ON RISC2 UNTIL THE EVENING OF 4/30/09.  SOME BRANCHES UPDATED
      * THE DAY & SINCE THE FISCAL TABLE SAID 4/30/08, LP/DAILY GOT
      * RUN EVEN THOUGH IT WAS THE LAST DAY OF THE MONTH.  SO THE
      * CURRENT DELINQUENCY FOR 4/09 WAS AS OF MAY 1ST, AND THIS GOT
      * UPDATED TO THE END-OF-LAST-MONTH DELINQUENCY FOR 5/09.
      * OUR SOLUTION WAS TO GO TO BEOM & RUN THE STAT FIX FOR APRIL,
      * THEN I WROTE A FIX (FX/BY0409) WHICH TAKES THE CURRENT STATS
      * FROM BEOM & MOVES THEM TO THE CURRENT IN DATA & THE ELM IN
      * THE NEXT MONTH OF DATA.  ONLY PROBLEM WAS THAT IN BEOM THE
      * MTD RECDEL IN THE LOAN RECORD HAD BEEN CLEARED BY EOM, SO
      * I PUT THIS STATEMENT IN THE STATS FIX TO TAKE CARE OF THE
      * POTS DELINQUENCY WHICH WAS MESSED UP.  BARB
      *
      *    IF RDPOT30 = "Y"
      *       IF LN-PY-YYYYMM-RECDEL = 0409
      *          MOVE 10 TO RDSUB.

           ADD ONE TO BY-UNITS(BYNOW BYREC RDSUB)
                      BY-UNITS(BYNOW BYCON CSUB).
           ADD SAVE-OUTBAL TO BY-OUTBAL(BYNOW BYREC RDSUB)
                              BY-OUTBAL(BYNOW BYCON CSUB).

      * UPDATE END OF LAST MONTH DELINQUENCY TABLE:
      *    IF BMO-DEL = "Y"
      *       ADD ONE TO BY-UNITS(BYBMO BYREC RDSUB)
      *                  BY-UNITS(BYBMO BYCON CSUB)
      *       ADD SAVE-OUTBAL TO BY-OUTBAL(BYBMO BYREC RDSUB)
      *                          BY-OUTBAL(BYBMO BYCON CSUB).

      * UPDATE CURRENT BANKRUPT DELINQUENCY TABLE:
           IF LN-BNKRPTDATE NOT = 0
              ADD ONE TO BY-UNITS-BNKRPT(BYNOW BYREC RDSUB)
                         BY-UNITS-BNKRPT(BYNOW BYCON CSUB)
              ADD SAVE-OUTBAL TO BY-OUTBAL-BNKRPT(BYNOW BYREC RDSUB)
                                 BY-OUTBAL-BNKRPT(BYNOW BYCON CSUB)

      * UPDATE END OF LAST MONTH BANKRUPT DELINQUENCY TABLE:
      *       IF BMO-DEL = "Y"
      *          ADD ONE TO BY-UNITS-BNKRPT(BYBMO BYREC RDSUB)
      *                     BY-UNITS-BNKRPT(BYBMO BYCON CSUB)
      *          ADD SAVE-OUTBAL
      *                  TO BY-OUTBAL-BNKRPT(BYBMO BYREC RDSUB)
      *                     BY-OUTBAL-BNKRPT(BYBMO BYCON CSUB)
      *       END-IF
           END-IF.

      * SUM WEIGHTED FIELDS:
           COMPUTE BY-SUMWGTTM = BY-SUMWGTTM + LN-ORGTERM * ONE.
           COMPUTE BY-SUMWGTAV = BY-SUMWGTAV + LN-LNAMT * ONE.
           COMPUTE BY-SUMWGTTA =
                          BY-SUMWGTTA + (LN-ORGTERM * LN-LNAMT * ONE).

       NORMAL-BY1-UPD-EXIT.
           EXIT.

      ****************************************************
       REPO-BY1-UPD SECTION.
      ****************************************************

           IF LN-REPOCD = "X"
                MOVE 1 TO SRCD
             ELSE
                MOVE 3 TO SRCD.

           ADD ONE       TO BY-NO-REPO(BYCUR BYNET SRCD).
           ADD LN-CURBAL TO BY-AMT-REPO(BYCUR BYNET SRCD).
           ADD LN-OT2BAL TO BY-O2-REPO(BYCUR BYNET SRCD).

      * SUM WEIGHTED FIELDS:
           COMPUTE BY-SUMWGTTM = BY-SUMWGTTM + LN-ORGTERM * ONE.
           COMPUTE BY-SUMWGTAV = BY-SUMWGTAV + LN-LNAMT * ONE.
           COMPUTE BY-SUMWGTTA =
                          BY-SUMWGTTA + (LN-ORGTERM * LN-LNAMT * ONE).

      * MERCFIX
      *       ADD ELM-FIX-ONE TO BY-NO-REPO(BYELM BYNET SRCD).

       REPO-BY1-UPD-EXIT.
           EXIT.

      ****************************************************
      *    P&L ACCOUNT BY1 UPDATE
      ****************************************************
       PL-BY1-UPD SECTION.

           PERFORM GET-SRCD-SUB.

           ADD ONE       TO BY-NO-PL(BYCUR BYNET SRCD).
           ADD LN-CURBAL TO BY-AMT-PL(BYCUR BYNET SRCD).
           ADD LN-OT2BAL TO BY-O2-PL(BYCUR BYNET SRCD).

      * MERCFIX
      *    ADD ELM-FIX-ONE TO BY-NO-PL(BYELM BYNET SRCD).

           IF NOT (LN-REPOCD = "X" OR "S")
              GO TO PL-BY1-UPD-EXIT.

           IF LN-REPOCD = "X"
                MOVE 4 TO SRCD
             ELSE
                MOVE 5 TO SRCD.

           ADD ONE TO BY-NO-REPO(BYCUR BYNET SRCD).
           ADD LN-CURBAL TO BY-AMT-REPO(BYCUR BYNET SRCD).
           ADD LN-OT2BAL TO BY-O2-REPO(BYCUR BYNET SRCD).

      * MERCFIX
      *    ADD ELM-FIX-ONE TO BY-NO-REPO(BYELM BYNET SRCD).

       PL-BY1-UPD-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-DS1 SECTION.
      ******************************************************************
           MOVE "N" TO REC-EXISTS.
           PERFORM OPEN-DS1-FILE.

       UPDATE-DS1-EXISTS.
           PERFORM SET-DS1-KEY.
           PERFORM READ-DS1-FILE.
           IF IO-FG = 9
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 MOVE "Y" TO REC-EXISTS
                 PERFORM CLEAR-DS-FILE
                 PERFORM SET-DS1-KEY
                 PERFORM WRITE-DS1-FILE
                 IF IO-FG NOT = 0
                    GO TO IO-ERROR
                 ELSE
                    GO TO UPDATE-DS1-EXISTS.

           IF LN-PLDATE NOT = 0
              PERFORM PL-DS1-UPD
           ELSE
              IF LN-REPOCD = "X" OR "S"
                 PERFORM REPO-DS1-UPD
              ELSE
                 PERFORM NORMAL-DS1-UPD.

           IF GP-UNEARNED-STATS = "Y"
              PERFORM COMPUTE-EARNINGS
              ADD EARN-UNEARN(1) TO DS-UNEARNED-AMT(DSNOW DSCUR 1)
              ADD EARN-UNEARN(2) TO DS-UNEARNED-AMT(DSNOW DSCUR 2)
              ADD EARN-UNEARN(3) TO DS-UNEARNED-AMT(DSNOW DSCUR 3)
              ADD EARN-UNEARN(4) TO DS-UNEARNED-AMT(DSNOW DSCUR 4)
              ADD EARN-UNEARN(5) TO DS-UNEARNED-AMT(DSNOW DSCUR 5)
              IF NOT GP-INCL-OTH-IN-EARNED-N
                 ADD EARN-UNEARN(6) TO DS-UNEARNED-AMT(DSNOW DSCUR 6).

           PERFORM REWRITE-DS1-FILE.
       UPDATE-DS1-EXIT.
           PERFORM CLOSE-DS1-FILE.

      ****************************************************
       NORMAL-DS1-UPD SECTION.
      ****************************************************

      * SETUP OUTBAL WORKER FOR UPDATE OF STATS

           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL GIVING SAVE-OUTBAL.

           ADD ONE       TO DS-OUTSUNIT(1).
           ADD LN-CURBAL TO DS-OUTSAMT(1).

      * UPDATE JUDGEMENT ACCOUNTS:
           IF LN-JDDATE NOT = 0
              ADD ONE       TO DS-JDOSUNIT(1)
              ADD LN-CURBAL TO DS-JDOSAMT(1).

      * UPDATE CURRENT DELINQUENCY TABLE:
           ADD ONE         TO DS-UNITS(DSNOW DSREC RDSUB)
                              DS-UNITS(DSNOW DSCON CSUB).
           ADD SAVE-OUTBAL TO DS-OUTBAL(DSNOW DSREC RDSUB)
                              DS-OUTBAL(DSNOW DSCON CSUB).

      * UPDATE END OF LAST MONTH DELINQUENCY TABLE:
      *    IF BMO-DEL = "Y"
      *       ADD ONE TO DS-UNITS(DSBMO DSREC RDSUB)
      *                  DS-UNITS(DSBMO DSCON CSUB)
      *       ADD SAVE-OUTBAL TO DS-OUTBAL(DSBMO DSREC RDSUB)
      *                          DS-OUTBAL(DSBMO DSCON CSUB).

      * UPDATE CURRENT & END OF LAST MONTH BANKRUPT DELINQUENCY TABLE:
           IF LN-BNKRPTDATE NOT = 0
              ADD ONE         TO DS-UNITS-BNKRPT(DSNOW DSREC RDSUB)
                                 DS-UNITS-BNKRPT(DSNOW DSCON CSUB)
              ADD SAVE-OUTBAL TO DS-OUTBAL-BNKRPT(DSNOW DSREC RDSUB)
                                 DS-OUTBAL-BNKRPT(DSNOW DSCON CSUB)
      *       IF BMO-DEL = "Y"
      *          ADD ONE TO DS-UNITS-BNKRPT(DSBMO DSREC RDSUB)
      *                     DS-UNITS-BNKRPT(DSBMO DSCON CSUB)
      *          ADD SAVE-OUTBAL
      *             TO DS-OUTBAL-BNKRPT(DSBMO DSREC RDSUB)
      *                DS-OUTBAL-BNKRPT(DSBMO DSCON CSUB)
      *        END-IF
           END-IF.

      * UPDATE RECOURSE:
           IF ( LN-SALEFIN = "Y"      ) AND
              ( LN-SERVACCT NOT = "Y" ) 
              IF LN-DLRECOURSE = "F"
                 ADD ONE       TO DS-RCOURSE-UNITS(1)
                 ADD LN-CURBAL TO DS-RCOURSE-AMT(1)
              ELSE
              IF LN-DLRECOURSE = "N"
                 ADD ONE       TO DS-RCOURSE-UNITS(3)
                 ADD LN-CURBAL TO DS-RCOURSE-AMT(3)
              ELSE
              IF LN-DLRECOURSE = "P"
                 ADD ONE TO DS-RCOURSE-UNITS(2)
                 IF LN-CURBAL < LN-DLPARTRCAMT
                    ADD LN-CURBAL      TO DS-RCOURSE-AMT(2)
                 ELSE
                    ADD LN-DLPARTRCAMT TO DS-RCOURSE-AMT(2).

      * SUM WEIGHTED FIELDS:
           COMPUTE DS-SUMWGTTM = DS-SUMWGTTM + LN-ORGTERM * ONE.
           COMPUTE DS-SUMWGTAV = DS-SUMWGTAV + LN-LNAMT * ONE.
           COMPUTE DS-SUMWGTTA =
                          DS-SUMWGTTA + (LN-ORGTERM * LN-LNAMT * ONE).

       NORMAL-DS1-UPD-EXIT.
           EXIT.

      ****************************************************
       REPO-DS1-UPD SECTION.
      ****************************************************

           IF LN-REPOCD = "X"
              MOVE 1 TO SRCD
           ELSE
              MOVE 3 TO SRCD.

           ADD ONE       TO DS-NO-REPO(DSCUR DSNET SRCD).
           ADD LN-CURBAL TO DS-AMT-REPO(DSCUR DSNET SRCD).

      * UPDATE JUDGEMENT ACCOUNTS:
           IF LN-JDDATE NOT = 0
              ADD ONE       TO DS-JDOSUNIT(1)
              ADD LN-CURBAL TO DS-JDOSAMT(1).

      * UPDATE RECOURSE:
           IF ( LN-SALEFIN = "Y"      ) AND
              ( LN-SERVACCT NOT = "Y" ) 
              IF LN-DLRECOURSE = "F"
                 ADD ONE       TO DS-RCOURSE-UNITS(1)
                 ADD LN-CURBAL TO DS-RCOURSE-AMT(1)
              ELSE
              IF LN-DLRECOURSE = "N"
                 ADD ONE       TO DS-RCOURSE-UNITS(3)
                 ADD LN-CURBAL TO DS-RCOURSE-AMT(3)
              ELSE
              IF LN-DLRECOURSE = "P"
                 ADD ONE          TO DS-RCOURSE-UNITS(2)
                 IF LN-CURBAL < LN-DLPARTRCAMT
                    ADD LN-CURBAL TO DS-RCOURSE-AMT(2)
                 ELSE
                    ADD LN-DLPARTRCAMT TO DS-RCOURSE-AMT(2).

      * SUM WEIGHTED FIELDS:
           COMPUTE DS-SUMWGTTM = DS-SUMWGTTM + LN-ORGTERM * ONE.
           COMPUTE DS-SUMWGTAV = DS-SUMWGTAV + LN-LNAMT * ONE.
           COMPUTE DS-SUMWGTTA =
                          DS-SUMWGTTA + (LN-ORGTERM * LN-LNAMT * ONE).

      * MERCFIX
      *    IF LN-REPOCD = "X"
      *       MOVE 1 TO SRCD
      *    ELSE
      *       MOVE 3 TO SRCD
      *    END-IF.
      *    ADD ELM-FIX-ONE TO DS-NO-REPO(DSELM DSNET SRCD).
      *    IF LN-JDDATE NOT = 0
      *       ADD ELM-FIX-ONE TO DS-JDOSUNIT(2).

       REPO-DS1-UPD-EXIT.
           EXIT.

      ****************************************************
       PL-DS1-UPD SECTION.
      ****************************************************

      * UPDATE JUDGEMENT ACCOUNTS:
           IF LN-JDDATE NOT = 0
              ADD ONE       TO DS-JDOSUNIT(1)
              ADD LN-CURBAL TO DS-JDOSAMT(1).

      * UPDATE P&L ACTIVITY:
           ADD ONE       TO DS-PLOSUNIT(1).
           ADD LN-CURBAL TO DS-PLOSAMT(1).

      * MERCFIX
      *    IF LN-JDDATE NOT = 0
      *       ADD ELM-FIX-ONE TO DS-JDOSUNIT(2).
      *    IF NOT (LN-REPOCD = "X" OR "S")
      *       ADD ELM-FIX-ONE TO DS-PLOSUNIT(2)
      *    ELSE
      *    IF LN-PLCD = "I"
      *       ADD ELM-FIX-ONE TO DS-NO-REPO(DSELM DSNET 5)
      *    ELSE
      *       ADD ELM-FIX-ONE TO DS-NO-REPO(DSELM DSNET 4).

           IF NOT (LN-REPOCD = "X" OR "S")
              GO TO PL-DS1-UPD-EXIT.

           IF LN-REPOCD = "X"
                MOVE 4 TO SRCD
             ELSE
                MOVE 5 TO SRCD.

           ADD 1 TO DS-NO-REPO(DSCUR DSNET SRCD).
           ADD LN-CURBAL TO DS-AMT-REPO(DSCUR DSNET SRCD).
      *    ADD ELM-FIX-ONE TO DS-NO-REPO(DSELM DSNET SRCD).

       PL-DS1-UPD-EXIT.
           EXIT.

      ******************************************************************
      *    TEST FOR LAST MONTH FIX
      *    THIS ROUTINE TEST TO SEE IF THIS ACCOUNT WAS TRANSFERED
      *    LAST MONTH WHEN IT HAD A ZERO BALANCE AND PER A BUG IN
      *    BULKT2.C STATISTICS WAS UPDATE WITH ZERO DOLLARS AND 1 UNIT
      ******************************************************************
      *TEST-FOR-ELM-FIX SECTION.
      *    MOVE 0 TO ELM-FIX-ONE.
      *    IF LN-POCD = "BT"
      *       MOVE LN-POFFDATE TO NUM-DATE
      *       IF NUM-MO = 8 AND NUM-YR = 98
      *          PERFORM VARYING SUB FROM 1 BY 1
      *                  UNTIL SUB > 10
      *             IF LN-STATFG(SUB) = X"74"
      *                PERFORM TEST-LM-FOR-ELM
      *             END-IF
      *          END-PERFORM
      *       END-IF
      *       GO TO TEST-FOR-ELM-FIX-EXIT.
      *
      *    IF LN-BR-TFR-FG = "Y"
      *       PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *          IF LN-STATFG(SUB) = "T"
      *             PERFORM TEST-LM-FOR-ELM
      *          END-IF
      *       END-PERFORM.

      *TEST-FOR-ELM-FIX-EXIT.
      *    EXIT.

      *TEST-LM-FOR-ELM SECTION.
      *    PERFORM OPEN-LM1-FILE.
      *    MOVE LN-ACCTNO TO LM-ACCTNO.
      *    MOVE 0 TO LM-DATE
      *              LM-SEQ.
      *    PERFORM START-LM1-FILE.
      *TEST-LM-FOR-ELM-AGAIN.
      *    PERFORM READ-LM1-FILE-NEXT.
      *    IF (IO-FG = 9) OR (LM-ACCTNO NOT = LN-ACCTNO)
      *       GO TO TEST-LM-FOR-ELM-EXIT.

      *    MOVE LM-MEMO TO ELM-MEMO.
      *    IF ELM-M1 = "TFR TO BR" AND ELM-M2 = " 0.00"
      *       SUBTRACT LM-DATE FROM 999999 GIVING NUM-DATE
      *       PERFORM YYMMDD-TO-MMDDYY
      *       IF NUM-MO = 98 AND NUM-DA = 08
      *          MOVE +1 TO ELM-FIX-ONE
      *          GO TO TEST-LM-FOR-ELM-EXIT.

      *    IF ELM-M1 = "TFR FR BR" AND ELM-M2 = " 0.00"
      *       SUBTRACT LM-DATE FROM 999999 GIVING NUM-DATE
      *       PERFORM YYMMDD-TO-MMDDYY
      *       IF NUM-MO = 98 AND NUM-DA = 08
      *          MOVE -1 TO ELM-FIX-ONE
      *          GO TO TEST-LM-FOR-ELM-EXIT.

      *    GO TO TEST-LM-FOR-ELM-AGAIN.

      *    PERFORM CLOSE-LM1-FILE.

      *TEST-LM-FOR-ELM-EXIT.
      *    EXIT.

      ******************************************************************
       SEND-PROGRESS-INDICATOR SECTION.
      ******************************************************************

           MOVE BR-NO                  TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE SPACES                 TO PROGID-PRU-LP.
           MOVE SPACES                 TO PROGID-ROUTE-BUF.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.
           MOVE SPACES                 TO PROGID-TIME.
           MOVE "B"                    TO PROGID-TYPE.
           MOVE SPACES                 TO PROGID-TEXT.

           IF ( PROCESSING-CLEAR-BY )
              MOVE PROCESSING-TEXT-CLEAR-BY TO PROGID-TEXT
           ELSE
           IF ( PROCESSING-CLEAR-DS )
              MOVE PROCESSING-TEXT-CLEAR-DS TO PROGID-TEXT
           ELSE
           IF ( PROCESSING-UPDATING )
              MOVE PROCESSING-TEXT-UPDATING TO PROGID-TEXT.

           PERFORM PROGID-DISPLAY.

       SEND-PROGID-INDICATOR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBLP/LPBYCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXBYDS-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXBYDS-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXBYDS-SCREEN UPON FXBYDS-WINDOW-HANDLE.
           MOVE FXBYDS-FORM-FIELDS TO WR-BUF.
           MOVE FXBYDS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXBYDS-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXBYDS-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXBYDS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARA SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SET-BY1-KEY.
           MOVE TODAYS-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE LN-OWNBR TO BY-BRNO.
           MOVE LN-CLASS TO BY-CLASS.
       SET-DS1-KEY.
           MOVE TODAYS-DS-DATE-CCYYMM TO DS-DATE-CCYYMM.
           MOVE LN-OWNBR TO DS-BRANCH.
           MOVE LN-DLNO TO DS-DLNO.
           MOVE LN-CLASS TO DS-CLASS.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.


      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN3IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPBY1IN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-PROGRAM SECTION.
      ***************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXBYDS-SCREEN.
           DESTROY FXBYDS-WINDOW-HANDLE.
           EXIT PROGRAM.
