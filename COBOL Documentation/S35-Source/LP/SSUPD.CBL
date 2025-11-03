       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SSUPD.
       DATE-WRITTEN.    121410.
      ******************************************************************
      *                 (LP)
      *
      * DESCRIPTION:  GENERATE CREDIT SCORE STATISTICS (SSFILE)
      *               WILL READ LNFILE FOR OPEN LOANS, & GENERATE
      *               STATS BASED ON BORROWER #1 ORIG & CURRENT
      *               CREDIT SCORES & BEG. OF THE MONTH DELINQUENCY
      *               WORLD PR# 726
      *
      * WORLD USES 4/27/2023
      *
      *               NOTE:  WILL RUN IN DATA, PROBABLY A FEW DAYS AFTER
      *                      MONTH-END.  SO SKIP LOANS BOOKED AFTER THE
      *                      MONTH-END DATE.  ALSO, IF LOAN WAS PAID OFF
      *                      AFTER MONTH-END DATE, INCLUDE EVEN THO
      *                      BALANCE IS ZERO.
      *
      *=================================================================
      * REV:
      * BLM 110203 CHANGE LOGIC FOR CHECKING PAIDOUTS.  NOW JUST SKIP IF
      *            LN-MONTHEND-BAL = 0.
      * BLM 110212 FIX LOGIC FOR COMPUTING BEG-FISCAL-DATE
      * BLM 110224 IF LOAN WAS TRANSFERED TO A BRANCH AFTER PREVIOUS
      *            END-OF-MONTH, IT WAS ADDED TO SCORE STATS IN BOTH BRANCHES.
      *            SO IF ACCT IS TRANSFERED, CHECK MEMO TO SEE IF IT WAS
      *            TRANSFER SINCE MONTH-END & SKIP IT.
      * BLM 110225 SCORE STATS ARE HIGH BY LOANS ENTERED IN PRIOR PERIOD &
      *            VOIDED THIS MONTH.  CHECK LN-ENTDATE ON VOIDED LOANS &
      *            ONLY SKIP VOIDS ON LOANS FROM CURRENT MONTH.
      * BLM 110315 FIXED MISSING INTERRUPT LOGIC, COULDN'T BREAK OUT.
      * BLM 140112 FIXED ROUTINE TO GET POSTING DATE OF CHARGE-OFF. WHEN
      *            READING LP FOR PL POST DATE, IT WAS CORRECT FOR P2-P5
      *            BUF NOT FOR PL, SO PL'S WOULD HAVE BEEN USING PLDATE
      *            FROM LNFILE,  WORLD PR# 1045
      * BAH 160125 REMOVED LTFILE, PD0003
      *
      * KEC 2017.0613 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED LIBLP/TFRMEMOW; USED TO CREATE/CHECK MEMOS (BM/LM)
      *  CS 170711 ACTIVATE 8 DIGIT DATES       
      * BAH 181004 GLOBAL BWFILE
      * BAH 181128 GLOBAL LMFILE
      * BAH 181211 GLOBAL LNFILE
      * BAH 20190930 REMOVED ACCESS-CALL SSFILE
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * BAH 20220803 HARD CODED START
      * RMZ 20231116 PERFORMANCE TUNING - ADDED LNFILE & LMFILE
      *              STORED PROCEDURES.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *COPY "LIBGB/GBFSWKI.CPY".
 
           SELECT WK-FILE ASSIGN WK-PATH   
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.

           SELECT VSS-FILE ASSIGN VSS-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY VSS1-KEY
                  FILE STATUS FILE-STAT. 
 

       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
      *****************************************************************
      * INDEXED WORK FILE, SORT SELECTED ACCTS BY SCORE (EITHER CURRENT
      * OR ORIGINAL AS SELECTED), BY DELINQUENCY.  WILL USE TO
      * GERNERATE AUDIT FILE
      *****************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO             PIC 9(4)      COMP-6.
               05  WK-PL-FG            PIC X.
               05  WK-CLASS            PIC 9(3).
               05  WK-SCORE-BUCKET     PIC 9.
               05  WK-DEL-BUCKET       PIC 9.
               05  WK-ACCT             PIC 9(8)      COMP-6.
           03  WK-SCORE                PIC 9(4)      COMP-6.
           03  WK-SCORE2               PIC 9(4)      COMP-6.
           03  WK-SCORE-BUCKET2        PIC 9.
           03  WK-BALANCE              PIC S9(7)V99  COMP-3.
           03  WK-PLDATE               PIC 9(8)      COMP-6.


      ******************************************************************
      *    FOR WRITING / RE-WRITING NEW SSFILE ROWS
      ******************************************************************
       FD  VSS-FILE
           LABEL RECORDS ARE STANDARD.
 
       78  VSS-SS-SIZE                         VALUE 1240.

       01  VSS-REC.
           03  VSS1-KEY.
               05  VSS-SS-DATE-YYYYMM          PIC 9(6).
               05  VSS-SS-BRNO                 PIC 9(4).
               05  VSS-SS-CLASS                PIC 9(3).
           03  VSS-SS-REC                      PIC X(VSS-SS-SIZE).
 
      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)LP/SSUPD S35 02/27/24-12:01:01 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SS.CPY".
       COPY "LIBLP/LP01SS_SQL.CPY".
       COPY "LIBLP/LPWSSS.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-MONTH-END-DATE-YYYYMM  PIC 9(6) VALUE 0.
       01  QWS-MONTH-END-DATE         PIC X(10).
       01  QWS-MEMO-CUT-OFF-DATE      PIC X(10).
       01  QWS-LM-CLASS               PIC 9(03).
       01  QWS-TEST-LN-PLCD           PIC X(02).
       01  QWS-TEST-LN-PLCD2 REDEFINES QWS-TEST-LN-PLCD.
           03  QWS-TEST-PLCD          PIC X.
           03  QWS-TEST-PLTYPE        PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW           PIC X        VALUE "N".
       01  LM1-CONN-OPEN-SW           PIC X        VALUE "N".

       01  WK-IFN                     PIC X(2)     VALUE "WK".
       01  VSS-IFN                    PIC X(3)     VALUE "VSS".
       01  WK-PATH                    PIC X(35)    VALUE SPACES.
       01  VSS-PATH                   PIC X(35)    VALUE SPACES.

       01  WS-LN-KEY.
           03 WS-LN-OWNBR             PIC 9(04).
           03 WS-LN-CLASS             PIC 9(03).    
           03 WS-LN-ACCTNO            PIC 9(08).
          
       01  WS-LM-KEY.                 
           03 WS-LM-OWNBR             PIC 9(04).
           03 WS-LM-CLASS             PIC 9(03).
           03 WS-LM-ACCTNO            PIC 9(08).

       01  HOLD-PL-FG                 PIC X.
       01  PL-MTD-FG                  PIC X.
       01  HOLD-CLASS                 PIC 9(3).
       01  HOLD-SCORE-BUCKET          PIC 9.
       01  FIRST-TIME                 PIC X.
     
       01  MEMO-CUT-OFF-DATE          PIC 9(8).
       01  ACCT-XFERED-AFTER-EOM      PIC X VALUE "N".
      
       01  TOT-SUB                    PIC 99.
       01  REC-EXISTS                 PIC X     VALUE "N".
      
       01  LEGEND                     PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                PIC X(07)     VALUE "LEGEND.".
       01  MESS-LIST                  PIC X(5)      VALUE "MESS.".
      
       01  MONTH-END-DATE-YYYYMM      PIC 9(6)      VALUE 0.
       01  BEG-MONTH-DATE             PIC 9(8)      VALUE 0.
      
       01  BEG-FISCAL-DATE            PIC 9(8)      VALUE 0.
       01  FILLER REDEFINES BEG-FISCAL-DATE.
           03  BEG-FISCAL-CCYR        PIC 9(4).
           03  BEG-FISCAL-MO          PIC 9(2).
           03  BEG-FISCAL-DA          PIC 9(2).
								     
       01  MONTH-END-DATE             PIC 9(8)      VALUE 0.
       01  FILLER                     REDEFINES MONTH-END-DATE.
           03  MONTH-END-CCYR         PIC 9(4).
           03  MONTH-END-MO           PIC 99.
           03  MONTH-END-DA           PIC 99.

      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH        PIC 9(4).
           03  END-BRANCH        PIC 9(4).
           03  CL1               PIC 99.
           03  CL2               PIC 99.
           03  REPORT-FG         PIC X.
           03  SCORE-TYPE-FG     PIC X.
           03  GROUP-FLAG        PIC X.
           03  ENT-GROUP         PIC X(4).

       01  VERSION-CONTROL       PIC X VALUE "A".

       01  HOLD-PL-TRDATE        PIC 9(8).
       01  TEST-LN-PLCD.        
           03  TEST-PLCD         PIC X.
           03  TEST-PLTYPE       PIC X.

       01  TOTAL-TABLE.
           03  TOTAL-TAB OCCURS 4.
               05  TOT-NO-DEL      PIC 9(6)      COMP  OCCURS 5.
               05  TOT-DEL-AMT     PIC S9(10)V99 COMP  OCCURS 5.
               05  TOT-NO-MTD-PL   PIC 9(6)      COMP.
               05  TOT-PL-MTD-AMT  PIC S9(10)V99 COMP.
               05  TOT-NO-YTD-PL   PIC 9(6)      COMP.
               05  TOT-PL-YTD-AMT  PIC S9(10)V99 COMP.
       01  TEST-TOTAL              PIC S9(10)V99 COMP.

      *=================================================================
      *
      *    P R O C E S S I N G - W O R K E R S
      *
      *=================================================================
       01  PROCESSING-FLAG             PIC X(01)     VALUE SPACES.
           88  PROCESSING-BRANCH-CHANGE              VALUE SPACES.
           88  PROCESSING-CLEAR-SS                   VALUE "B".
           88  PROCESSING-UPDATING                   VALUE "U".

      *-----------------------------------------------------------------
       01  PROCESSING-TEXT-CLEAR-SS.
           03  FILLER PIC X(30) VALUE "CLEARING THE 'SS' FIELDS......".
           03  FILLER PIC X(30) VALUE "..............................".

       01  PROCESSING-TEXT-UPDATING.
           03  FILLER PIC X(30) VALUE "UPDATING 'SS' FILE............".
           03  FILLER PIC X(30) VALUE "..............................".

      ******************************************************************
      *    L I N E    W O R K E R S
      ******************************************************************
       01  LN-FEED          PIC 99    VALUE 0.
       01  LN-COUNT         PIC 999   VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      ******************************************************************
      *     P R I N T   B U F F E R S
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

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(30)    VALUE
           "CREDIT SCORE STATISTICS UPDATE".
           03  FILLER           PIC X(3).

       01  HEAD3.
           03  FILLER      PIC X(15)    VALUE
               "BR#  CL  ACCT# ".
           03  HEAD3-SCORE-TYPE  PIC X.
           03  FILLER      PIC X(7)     VALUE
               " SCORE ".
           03  FILLER      PIC X(39)    VALUE
               "     CURRENT         POTS       30 DAY ".
           03  FILLER      PIC X(26)    VALUE
               "      60 DAY     90+ DAYS ".
           03  FILLER      PIC X(10)    VALUE
               " PL DATE  ".
           03  FILLER      PIC X(26)    VALUE
               "     MTD P&L      YTD P&L ".
           03  HEAD3-SCORE-TYPE2 PIC X.
           03  FILLER      PIC X(7)     VALUE
               " SCORE ".

       01  DETAIL-LINE          PIC X(132).
       01  DETAIL1              REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC 9(4).
           03  FILLER           PIC X.
           03  D-CLASS          PIC 99.
           03  FILLER           PIC X.
           03  D-ACCT           PIC ZZZZZ9.
           03  FILLER           PIC X.
           03  D-SCORE          PIC ZZZ9.
           03  FILLER           PIC XX.
           03  D-SCORE-BUCKET   PIC 9.
           03  FILLER           PIC X.
           03  D-BALANCE        OCCURS 5 PIC ZZZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER REDEFINES D-BALANCE.
               05  D-COUNT OCCURS 5 PIC ZZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  FILLER           PIC X.
           03  D-PLDATE         PIC X(8).
           03  FILLER           PIC X.
           03  D-MTD-PLAMT      PIC ZZZZZ,ZZ9.99- BLANK ZERO.
           03  D-MTD-PL-COUNT REDEFINES D-MTD-PLAMT
                                PIC ZZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  D-YTD-PLAMT      PIC ZZZZZ,ZZ9.99- BLANK ZERO.
           03  D-YTD-PL-COUNT REDEFINES D-YTD-PLAMT
                                PIC ZZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  FILLER           PIC X.
           03  D-SCORE2         PIC ZZZ9.
           03  FILLER           PIC XX.
           03  D-SCORE-BUCKET2  PIC 9.
       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).

      ******************************************************************
      *    C O P Y - L I B R A R Y - W O R K E R S
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBLP/LPSSWK.CPY".
       COPY "LIBLP/TFRMEMOW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/SSUPD_DEF.CPY".
       COPY "LIBLP/SSUPD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/SSUPD_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE VSS-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SS1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LM1-CONNECTION.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           CLOSE PR-FILE WK-FILE VSS-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - C O N T R O L
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE "SSUPD" TO VDU-FORM-NAME.
           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = " " )
              GO TO END-ROUTINE.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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
           IF ( IO-FG = 9 ) OR ( BR-NO > END-BRANCH )
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "PROGRAM IN PROGRESS.............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

       END-OF-GROUP.
           IF REPORT-FG NOT = "N"
              PERFORM PREPARE-REPORT.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       GLOBAL-UPDATE SECTION.
      ******************************************************************

           MOVE SPACES TO PROCESSING-FLAG.
           MOVE 9999   TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM CLEAR-SS.

           PERFORM UPDATE-SS.

       GLOBAL-UPDATE-EXIT.
           EXIT.

      ******************************************************************
       CLEAR-SS SECTION.
      ******************************************************************

           MOVE MONTH-END-DATE-YYYYMM TO SS-DATE-YYYYMM
                                         QSS1-WBEG-DATE
                                         QSS1-WEND-DATE.
           MOVE BR-NO                 TO SS-BRNO
                                         QSS1-WBEG-BRNO
                                         QSS1-WEND-BRNO.
           MOVE CL1                   TO SS-CLASS
                                         QSS1-WBEG-CLASS.
           MOVE CL2                   TO QSS1-WEND-CLASS.

           PERFORM START-SS1-FILE.
       CLEAR-SS-NEXT.
           PERFORM READ-SS1-FILE-NEXT.
           IF IO-FG = 9 OR (SS-BRNO NOT = BR-NO)
              GO TO CLEAR-SS-EXIT.

           MOVE "B" TO PROCESSING-FLAG.
           PERFORM SEND-PROGRESS-INDICATOR.

           IF SS-DATE-YYYYMM NOT = MONTH-END-DATE-YYYYMM
              GO TO CLEAR-SS-EXIT.
           IF SS-CLASS > CL2
              GO TO CLEAR-SS-EXIT.

           PERFORM DELETE-SS1-FILE.

           GO TO CLEAR-SS-NEXT.

       CLEAR-SS-EXIT.
           EXIT.

      ********************************************
       UPDATE-SS SECTION.
      ********************************************
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LM1-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE CL1       TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE CL2       TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           MOVE MONTH-END-DATE-YYYYMM TO QWS-MONTH-END-DATE-YYYYMM.
           MOVE MONTH-END-DATE        TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD   TO QWS-MONTH-END-DATE.
           MOVE MEMO-CUT-OFF-DATE     TO QWS-MEMO-CUT-OFF-DATE.
           MOVE ALL "0"               TO WS-LM-OWNBR
                                         WS-LM-CLASS   
                                         WS-LM-ACCTNO.

           PERFORM START-LN3-FILE. *> SSP - LN,BW
           PERFORM START-LM1-FILE. *> SSP - LM,LN

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-CLASS > CL2)
                 GO TO UPDATE-SS-EXIT.

           MOVE "U" TO PROCESSING-FLAG.
           PERFORM SEND-PROGRESS-INDICATOR.

      * IF LOAN FROM PRIOR MONTH WAS VOIDED THIS MONTH, INCLUDE IT.
      * SO SKIP VOIDED LOANS ENTERED & VOIDED THIS MONTH
      * AND SKIP LOANS VOIDED PRIOR TO MONTH-END
      * SO, MONTH-END DATE IS 1/31/11
      * LOAN IS ENTERED AFTER 1/31/11 & VOIDED, SKIP IT.
      * LOAN ENTERED & VOIDED ON OR BEFORE 1/31/11, SKIP IT
      * SHOULD JUST BE LEFT WITH LOANS ENTERED BEFORE 1/31 & VOIDED AFTER

           IF LN-POCD = "**"
              IF LN-ENTDATE > MONTH-END-DATE
                 GO TO NEXT-LN
              ELSE
                 IF LN-POFFDATE NOT > MONTH-END-DATE
                    GO TO NEXT-LN.

      * IF LOAN WAS BOOKED AFTER MONTH-END DATE, DON'T INCLUDE:

           IF LN-LOANDATE > MONTH-END-DATE
              GO TO NEXT-LN.

           IF LN-MONTHEND-BAL = 0
              GO TO NEXT-LN.

           IF LN-BR-TFR-FG = "Y"
              PERFORM CHECK-LM-TRANSFER
              IF ACCT-XFERED-AFTER-EOM = "Y"
                 GO TO NEXT-LN.

           PERFORM UPDATE-SS1.

           GO TO NEXT-LN.

       UPDATE-SS-EXIT.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LM1-FILE.
   
           PERFORM CLOSE-VSS-FILE.
           PERFORM OPEN-VSS-FILE.
           PERFORM PROCESS-VSS-FILE.
           PERFORM CLOSE-VSS-FILE.

           PERFORM OPEN-VSS-FILE-OUTPUT.
           PERFORM CLOSE-VSS-FILE.
           PERFORM OPEN-VSS-FILE.

      *******************************************
       CHECK-LM-TRANSFER SECTION.
      *******************************************

      * IF LOAN WAS TRANSFERED TO THIS BRANCH AFTER PREVIOUS END-OF-MONTH,
      * IT WILL BE UPDATED IN BRANCH IT WAS TRANSFERED TO, SO SKIP IT
      * IN THE TRANSFER-TO BRANCH

           MOVE "N" TO ACCT-XFERED-AFTER-EOM.

      *    MOVE LM-PATH-OWNBR     TO LM-BRNO
      *                              QLM1-WBEG-BRNO
      *                              QLM1-WEND-BRNO.
      *    MOVE LN-ACCTNO         TO LM-ACCTNO
      *                              QLM1-WBEG-ACCTNO
      *                              QLM1-WEND-ACCTNO.
      *    MOVE 0                 TO LM-DATE LM-SEQ
      *                              QLM1-WBEG-DATE-REV
      *                              QLM1-WBEG-SEQ.
      *    MOVE ALL "9"           TO QLM1-WEND-DATE-REV
      *                              QLM1-WEND-SEQ.
      *    MOVE MEMO-CUT-OFF-DATE TO QWS-MEMO-CUT-OFF-DATE. 

      *    PERFORM START-LM1-FILE. *> SSP - LMFILE, LNFILE

       CHECK-LM-NEXT.

           IF WS-LN-KEY < WS-LM-KEY
              GO TO EXIT-CHECK-LM-TRANSFER.

           IF WS-LM-KEY < WS-LN-KEY
              PERFORM READ-LM1-FILE-NEXT *> EMBEDDED - LMFILE, LNFILE
              IF IO-FG = 9  
                 MOVE ALL "9"  TO WS-LM-OWNBR
                                  WS-LM-CLASS   
                                  WS-LM-ACCTNO
                 GO TO EXIT-CHECK-LM-TRANSFER
              ELSE
                 GO TO CHECK-LM-NEXT.

           IF LM-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-CHECK-LM-TRANSFER.

           IF LM-DATE >= MEMO-CUT-OFF-DATE
              GO TO EXIT-CHECK-LM-TRANSFER.
           IF LM-MEMO-TYPE NOT = "9"
              GO TO CHECK-LM-NEXT.

      * TFR TO BR #### FR BR ####, FOR $ZZZZZ9.99 ---> 12345678     [LM]
      * TFR FR BR #### TO BR ####, FOR $ZZZZZ9.99 <--- 12345678     [LM]

           MOVE LM-MEMO                    TO TFRMEMO-MEMO.

           IF ( TFRMEMO-PRIMARY NOT = "TFR FR BR" )
              GO TO CHECK-LM-NEXT.

           MOVE "Y" TO ACCT-XFERED-AFTER-EOM.

       EXIT-CHECK-LM-TRANSFER.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE SSUPD-QUEUE-POS TO Q-POS.
           MOVE SSUPD-COPIES-POS TO C-POS.
           MOVE SSUPD-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

      * SET BEGINNING MONTH DATE, BEG FISCAL DATE & SS FORMAT DATE
           MOVE MONTH-END-DATE TO NUM-DATE.
           MOVE 1              TO NUM-DA.
           MOVE NUM-DATE       TO BEG-MONTH-DATE.

           MOVE 4 TO BEG-FISCAL-MO.
           MOVE 1 TO BEG-FISCAL-DA.
           IF MONTH-END-MO < 4
              SUBTRACT 1 FROM MONTH-END-CCYR GIVING BEG-FISCAL-CCYR
           ELSE
              MOVE MONTH-END-CCYR TO BEG-FISCAL-CCYR.

           MOVE MONTH-END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO MONTH-END-DATE-YYYYMM.

           COMPUTE MEMO-CUT-OFF-DATE = 99999999
                                     - MONTH-END-DATE.

           MOVE EXT-CONAME-CONAME   TO H-NAME.
           MOVE EXT-CONAME-SYSDATE  TO H-DATE.
           MOVE EXT-CONAME-USER-ID   TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROGRAM IN PROGRESS..................." TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SS1-FILE.
           PERFORM OPEN-PR-FILE.

           IF REPORT-FG NOT = "N"
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE MKTEMP-BUF TO WK-PATH
              PERFORM OPEN-WK-FILE-OUTPUT
              PERFORM CLOSE-WK-FILE
              PERFORM OPEN-WK-FILE.


           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO VSS-PATH.
           PERFORM OPEN-VSS-FILE-OUTPUT.
           PERFORM CLOSE-VSS-FILE.
           PERFORM OPEN-VSS-FILE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION. 

           MOVE LOW-VALUES TO TOTAL-TABLE.

           GO TO INITIALIZE-EXIT.
       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO MONTH-END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              IF REPORT-FG NOT = "N"
                 GO TO ENTER-SCORE-TYPE
              ELSE
                 GO TO ENTER-REPORT-YN.
       INIT-ACCEPT-BR1.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO EXIT-PARM.
           IF ( VDUSTATUS = "16" )
              GO TO INIT-ACCEPT-BR-ALL.
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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO INIT-ACCEPT-CL1.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" )
              GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )
              GO TO INIT-ACCEPT-BR1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR2.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-BR1.

           GO TO INIT-ACCEPT-CL1.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR-ALL.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       INIT-ACCEPT-CL1.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PARM.
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

           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-CL1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-CL2.

           MOVE VDUNUM0 TO CL2.

           IF CL1 > CL2
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-CL1.

           GO TO INIT-ACCEPT-DATE.

      *-----------------------------------------------------------------
       INIT-ACCEPT-CL-ALL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO CL1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO CL2 VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       INIT-ACCEPT-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-REPORT-YN.

           MOVE "ERNM060DATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-DATE.

           MOVE VDU-DATE-YYYYMMDD TO MONTH-END-DATE.
           IF MONTH-END-DATE = 0
              GO TO INIT-ACCEPT-DATE.

      *-----------------------------------------------------------------
       ENTER-REPORT-YN.
           MOVE "E  A010REPORT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO INIT-ACCEPT-CL2
              ELSE
                 GO TO INIT-ACCEPT-DATE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-REPORT-YN.

           MOVE VDUBUF TO REPORT-FG.

           IF REPORT-FG NOT = "Y"
              IF REPORT-FG NOT = "N"
                 IF REPORT-FG NOT = "O"
                    MOVE
                  "PLEASE ENTER 'Y', 'N', OR 'O'" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-REPORT-YN.
           IF REPORT-FG = "N"
              GO TO EXIT-PARM.
      *-----------------------------------------------------------------
       ENTER-SCORE-TYPE.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-REPORT-YN.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-REPORT-YN.

           MOVE VDUBUF TO SCORE-TYPE-FG.

           IF SCORE-TYPE-FG NOT = "C"
              IF SCORE-TYPE-FG NOT = "O"
                 MOVE "PLEASE ENTER 'C'(CURRENT) OR 'O'(ORIG)" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-SCORE-TYPE.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE CL1 TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE CL2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010REPORT." TO VDU.
           MOVE REPORT-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010TYPE." TO VDU.
           MOVE SCORE-TYPE-FG TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       UPDATE-SS1 SECTION.
      ******************************************************************
           MOVE "N" TO REC-EXISTS.

       UPDATE-SS1-EXISTS.
           PERFORM SET-SS1-KEY.
           PERFORM READ-VSS-FILE. *> EMBEDDED - SSFILE
      *    PERFORM READ-SS1-FILE. *> EMBEDDED - SSFILE
           IF IO-FG = 9
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 MOVE "Y" TO REC-EXISTS
      *          PERFORM CLEAR-SS-FILE
                 INITIALIZE SS-REC 
                 PERFORM SET-SS1-KEY
                 MOVE SS-REC TO VSS-SS-REC
                 PERFORM WRITE-VSS-FILE *> EMBEDDED - VSS WORK FILE
      *          PERFORM WRITE-SS1-FILE *> EMBEDDED - SSFILE
      *          IF IO-FG NOT = 0
      *             GO TO IO-ERROR
      *          ELSE
      *             GO TO UPDATE-SS1-EXISTS.
                 IF IO-FG NOT = 0
                    GO TO IO-ERROR
                 ELSE
                    NEXT SENTENCE    
            ELSE
              MOVE VSS-SS-REC TO SS-REC.

      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF IO-BAD

           IF BW-SSNO = 0
              MOVE "INVALID SSNO, BR# &&&&, LOAN# %%%%%%%%!!" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              INSPECT MESS REPLACING FIRST "%%%%%%%%" BY LN-ACCTNO
              PERFORM SEND-MESS
              GO TO UPDATE-SS1-EXIT.

           MOVE BW-CREDIT-SCORE TO CREDIT-SCORE-BUCKET.
           IF SCORE-BUCKET1      MOVE 1 TO CUR-SUB
           ELSE IF SCORE-BUCKET2 MOVE 2 TO CUR-SUB
           ELSE IF SCORE-BUCKET3 MOVE 3 TO CUR-SUB
           ELSE IF SCORE-BUCKET4 MOVE 4 TO CUR-SUB
           ELSE IF SCORE-BUCKET5 MOVE 5 TO CUR-SUB
           ELSE IF SCORE-BUCKET6 MOVE 6 TO CUR-SUB
           ELSE IF SCORE-BUCKET7 MOVE 7 TO CUR-SUB
           ELSE IF SCORE-BUCKET8 MOVE 8 TO CUR-SUB.

           MOVE BW-BNKRPT-SCORE(1) TO CREDIT-SCORE-BUCKET.
           IF SCORE-BUCKET1      MOVE 1 TO ORIG-SUB
           ELSE IF SCORE-BUCKET2 MOVE 2 TO ORIG-SUB
           ELSE IF SCORE-BUCKET3 MOVE 3 TO ORIG-SUB
           ELSE IF SCORE-BUCKET4 MOVE 4 TO ORIG-SUB
           ELSE IF SCORE-BUCKET5 MOVE 5 TO ORIG-SUB
           ELSE IF SCORE-BUCKET6 MOVE 6 TO ORIG-SUB
           ELSE IF SCORE-BUCKET7 MOVE 7 TO ORIG-SUB
           ELSE IF SCORE-BUCKET8 MOVE 8 TO ORIG-SUB.

           IF LN-PLDATE = 0
              PERFORM NORMAL-SS1-UPD
              GO TO REWRITE-SS-FILE.

      * IF ACCT WAS CHARGED OFF AFTER MONTH-END, PROCESS AS
      * NORMAL OPEN ACCT, OTHERWISE PROCESS AS P&L.  NOTE:
      * WILL HAVE TO READ LP TO GET TRANS DATE SINCE PL'S ARE
      * BACK-DATED, AT LEAST P2'S:

           MOVE LN-PLDATE TO HOLD-PL-TRDATE.
           MOVE "PL" TO TEST-LN-PLCD
                        QWS-TEST-LN-PLCD.
           IF LN-PLTYPE = "2" OR "3" OR "4" OR "5"
              MOVE LN-PLTYPE TO TEST-PLTYPE
                                QWS-TEST-PLTYPE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
      *     PERFORM START-LP1-FILE-NOT-GREATER. *> EMBEDDED - LPFILE
       NEXT-LP.
      *    PERFORM READ-LP1-FILE-PREVIOUS. *> EMBEDDED - LPFILE
           PERFORM READ-LP1-FILE. *> EMBEDDED - LPFILE
           IF IO-FG NOT = 0
              GO TO EXIT-GET-LP-INFO.
       
           IF (LP-BRNO NOT = LN-OWNBR) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-GET-LP-INFO.
       
      *    IF LP-TRCD NOT = TEST-LN-PLCD
      *       GO TO NEXT-LP.
       
           MOVE LP-TRDATE TO HOLD-PL-TRDATE.
       EXIT-GET-LP-INFO.

           IF HOLD-PL-TRDATE > MONTH-END-DATE
              PERFORM NORMAL-SS1-UPD
           ELSE
              PERFORM PL-SS1-UPD.

       REWRITE-SS-FILE.

           MOVE SS-REC TO VSS-SS-REC.
           PERFORM REWRITE-VSS-FILE. *> EMBEDDED - VSS WORK FILE
      *    PERFORM REWRITE-SS1-FILE. *> EMBEDDED - SSFILE

       UPDATE-SS1-EXIT.
           EXIT.

      ****************************************************
       NORMAL-SS1-UPD SECTION.
      ****************************************************

           IF LN-BMO-CON-STATUS = "0"
              MOVE 1 TO DEL-SUB
           ELSE IF LN-BMO-CON-STATUS = "Y"
              MOVE 2 TO DEL-SUB
           ELSE IF LN-BMO-CON-STATUS = "1"
              MOVE 3 TO DEL-SUB
           ELSE IF LN-BMO-CON-STATUS = "2"
              MOVE 4 TO DEL-SUB
           ELSE
              MOVE 5 TO DEL-SUB.

           ADD 1               TO SS-UNITS(CUR-SUB 1 DEL-SUB).
           ADD LN-MONTHEND-BAL TO SS-OUTBAL(CUR-SUB 1 DEL-SUB).
           ADD 1               TO SS-UNITS(ORIG-SUB 2 DEL-SUB).
           ADD LN-MONTHEND-BAL TO SS-OUTBAL(ORIG-SUB 2 DEL-SUB).

           IF REPORT-FG = "N"
              GO TO NORMAL-SS1-UPD-EXIT.

           MOVE " " TO HOLD-PL-FG.

           PERFORM CREATE-WK-FILE.
           IF SCORE-TYPE-FG = "C"
              MOVE BW-CREDIT-SCORE TO WK-SCORE
              MOVE BW-BNKRPT-SCORE(1) TO WK-SCORE2
              MOVE ORIG-SUB TO WK-SCORE-BUCKET2
           ELSE
              MOVE BW-BNKRPT-SCORE(1) TO WK-SCORE
              MOVE BW-CREDIT-SCORE TO WK-SCORE2
              MOVE CUR-SUB  TO WK-SCORE-BUCKET2.
           MOVE LN-MONTHEND-BAL TO WK-BALANCE.
           MOVE 0 TO WK-PLDATE.
           PERFORM REWRITE-WK-FILE.

       NORMAL-SS1-UPD-EXIT.
           EXIT.

      ****************************************************
       PL-SS1-UPD SECTION.
      ****************************************************

           IF HOLD-PL-TRDATE < BEG-FISCAL-DATE
              GO TO PL-SS1-UPD-EXIT.

           ADD 1         TO SS-NO-PL(CUR-SUB 1 SSYTD)
           ADD LN-PLAMT  TO SS-AMT-PL(CUR-SUB 1 SSYTD).
           ADD 1         TO SS-NO-PL(ORIG-SUB 2 SSYTD)
           ADD LN-PLAMT  TO SS-AMT-PL(ORIG-SUB 2 SSYTD).

           IF HOLD-PL-TRDATE NOT < BEG-MONTH-DATE
              ADD 1         TO SS-NO-PL(CUR-SUB 1 SSMTD)
              ADD LN-PLAMT  TO SS-AMT-PL(CUR-SUB 1 SSMTD)
              ADD 1         TO SS-NO-PL(ORIG-SUB 2 SSMTD)
              ADD LN-PLAMT  TO SS-AMT-PL(ORIG-SUB 2 SSMTD).

           IF REPORT-FG = "N"
              GO TO PL-SS1-UPD-EXIT.

           MOVE "P" TO HOLD-PL-FG.

           PERFORM CREATE-WK-FILE.
           IF SCORE-TYPE-FG = "C"
              MOVE BW-CREDIT-SCORE TO WK-SCORE
              MOVE BW-BNKRPT-SCORE(1) TO WK-SCORE2
              MOVE ORIG-SUB TO WK-SCORE-BUCKET2
           ELSE
              MOVE BW-BNKRPT-SCORE(1) TO WK-SCORE
              MOVE BW-CREDIT-SCORE TO WK-SCORE2
              MOVE CUR-SUB  TO WK-SCORE-BUCKET2.
           MOVE LN-PLAMT              TO WK-BALANCE.
           MOVE HOLD-PL-TRDATE        TO WK-PLDATE.
           PERFORM REWRITE-WK-FILE.

       PL-SS1-UPD-EXIT.
           EXIT.

      **********************************************************
      * WRITE ALL SS VSAM RECORDS TO SSFILE TABLE FOR A BRANCH.
      **********************************************************
       PROCESS-VSS-FILE SECTION.

           PERFORM READ-VSS-FILE-NEXT. *> EMBEDDED - VSSFILE
  
           IF IO-FG = 9
              GO TO PROCESS-VSS-FILE-EXIT.
 
           MOVE VSS-SS-REC TO SS-REC.
           PERFORM WRITE-SS1-FILE *> EMBEDDED - SSFILE
           GO TO PROCESS-VSS-FILE.

       PROCESS-VSS-FILE-EXIT.
           EXIT.

      ****************************************************
       CREATE-WK-FILE SECTION.
      ****************************************************
           MOVE "N" TO REC-EXISTS.
       CREATE-WK-FILE-EXISTS.
           MOVE BR-NO       TO WK-BRNO.
           MOVE HOLD-PL-FG  TO WK-PL-FG.
           MOVE LN-CLASS    TO WK-CLASS.
           MOVE LN-ACCTNO   TO WK-ACCT.
           IF SCORE-TYPE-FG = "C"
              MOVE CUR-SUB  TO WK-SCORE-BUCKET
           ELSE
              MOVE ORIG-SUB TO WK-SCORE-BUCKET.
           IF HOLD-PL-FG = "P"
              MOVE 1           TO WK-DEL-BUCKET
           ELSE
              MOVE DEL-SUB     TO WK-DEL-BUCKET.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 MOVE SPACES      TO WK-REC
                 MOVE BR-NO       TO WK-BRNO
                 MOVE HOLD-PL-FG  TO WK-PL-FG
                 MOVE LN-CLASS    TO WK-CLASS
                 MOVE LN-ACCTNO   TO WK-ACCT
                 IF SCORE-TYPE-FG = "C"
                    MOVE CUR-SUB  TO WK-SCORE-BUCKET
                 ELSE
                    MOVE ORIG-SUB TO WK-SCORE-BUCKET
                 END-IF
                 IF HOLD-PL-FG = "P"
                    MOVE 1           TO WK-DEL-BUCKET
                 ELSE
                    MOVE DEL-SUB     TO WK-DEL-BUCKET
                 END-IF
                 PERFORM WRITE-WK-FILE
                 IF IO-BAD
                    GO TO IO-ERROR
                 ELSE
                    GO TO CREATE-WK-FILE-EXISTS.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED       TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO LN-FEED.
           MOVE SPACES       TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
     
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
    
           IF SCORE-TYPE-FG = "C"
              MOVE "C " TO HEAD3-SCORE-TYPE
              MOVE "O " TO HEAD3-SCORE-TYPE2
           ELSE
              MOVE "O " TO HEAD3-SCORE-TYPE
              MOVE "C " TO HEAD3-SCORE-TYPE2.
           MOVE HEAD3        TO PR-REC.
           PERFORM WRITE-PR-REC.
   
           MOVE 4      TO LN-COUNT.
           MOVE 2      TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE 4                      TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE  CL1                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
     
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE CL2                    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
    
           MOVE "MONTH END DATE     :" TO RANGE-LINE.
           MOVE  MONTH-END-DATE        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
   
           MOVE "REPORT (Y/N/O)     :" TO RANGE-LINE.
           MOVE REPORT-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
  
           MOVE "SCORE TYPE (C/O)   :" TO RANGE-LINE.
           MOVE SCORE-TYPE-FG          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       PREPARE-REPORT SECTION.
      *****************************************************************
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE "Y" TO FIRST-TIME.
       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PREPARE-REPORT.

           IF FIRST-TIME = "Y"
              MOVE WK-SCORE-BUCKET TO HOLD-SCORE-BUCKET
              MOVE WK-CLASS TO HOLD-CLASS
              MOVE "N" TO FIRST-TIME.

           IF WK-CLASS NOT = HOLD-CLASS
              PERFORM PRINT-SCORE-TOTALS
              PERFORM PRINT-CLASS-TOTALS.

           IF WK-SCORE-BUCKET NOT = HOLD-SCORE-BUCKET
              PERFORM PRINT-SCORE-TOTALS.

           IF WK-PLDATE = 0
              ADD 1             TO TOT-NO-DEL(1 WK-DEL-BUCKET)
                                   TOT-NO-DEL(2 WK-DEL-BUCKET)
                                   TOT-NO-DEL(3 WK-DEL-BUCKET)
                                   TOT-NO-DEL(4 WK-DEL-BUCKET)
              ADD WK-BALANCE    TO TOT-DEL-AMT(1 WK-DEL-BUCKET)
                                   TOT-DEL-AMT(2 WK-DEL-BUCKET)
                                   TOT-DEL-AMT(3 WK-DEL-BUCKET)
                                   TOT-DEL-AMT(4 WK-DEL-BUCKET)
           ELSE
              ADD 1             TO TOT-NO-YTD-PL(1)
                                   TOT-NO-YTD-PL(2)
                                   TOT-NO-YTD-PL(3)
                                   TOT-NO-YTD-PL(4)
              ADD WK-BALANCE    TO TOT-PL-YTD-AMT(1)
                                   TOT-PL-YTD-AMT(2)
                                   TOT-PL-YTD-AMT(3)
                                   TOT-PL-YTD-AMT(4)
              MOVE "N"          TO PL-MTD-FG
              IF WK-PLDATE NOT < BEG-MONTH-DATE
                 MOVE "Y"          TO PL-MTD-FG
                 ADD 1             TO TOT-NO-MTD-PL(1)
                                      TOT-NO-MTD-PL(2)
                                      TOT-NO-MTD-PL(3)
                                      TOT-NO-MTD-PL(4)
                 ADD WK-BALANCE    TO TOT-PL-MTD-AMT(1)
                                      TOT-PL-MTD-AMT(2)
                                      TOT-PL-MTD-AMT(3)
                                      TOT-PL-MTD-AMT(4).

           MOVE SPACES           TO DETAIL-LINE.
           MOVE WK-BRNO          TO D-BRNO.
           MOVE WK-CLASS         TO D-CLASS.
           MOVE WK-ACCT          TO D-ACCT.
           MOVE WK-SCORE-BUCKET  TO D-SCORE-BUCKET.
           MOVE WK-SCORE         TO D-SCORE.
           MOVE WK-SCORE-BUCKET2 TO D-SCORE-BUCKET2.
           MOVE WK-SCORE2        TO D-SCORE2.
           IF WK-PLDATE = 0
              MOVE WK-BALANCE    TO D-BALANCE(WK-DEL-BUCKET)
           ELSE
              MOVE WK-PLDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE      TO D-PLDATE
              MOVE WK-BALANCE    TO D-YTD-PLAMT
              IF PL-MTD-FG = "Y"
                 MOVE WK-BALANCE    TO D-MTD-PLAMT.

           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-WK.
       EXIT-PREPARE-REPORT.
           PERFORM PRINT-BRANCH-TOTALS.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      ******************************************************************
       PRINT-SCORE-TOTALS SECTION.
      ******************************************************************
           MOVE 1 TO TOT-SUB.
           MOVE "SCORE TOTALS BUCKET &:" TO DETAIL1.
           INSPECT DETAIL1 REPLACING FIRST "&" BY HOLD-SCORE-BUCKET.
           PERFORM PRINT-TOTALS.
           MOVE WK-SCORE-BUCKET TO HOLD-SCORE-BUCKET.

      ******************************************************************
       PRINT-CLASS-TOTALS SECTION.
      ******************************************************************
           MOVE 2 TO TOT-SUB.
           MOVE "CLASS TOTALS CLASS &&&:" TO DETAIL1.
           INSPECT DETAIL1 REPLACING FIRST "&&&" BY HOLD-CLASS.
           PERFORM PRINT-TOTALS.
           MOVE WK-CLASS TO HOLD-CLASS.

      ******************************************************************
       PRINT-BRANCH-TOTALS SECTION.
      ******************************************************************
           MOVE 3 TO TOT-SUB.
           MOVE "BRANCH TOTALS:" TO DETAIL1.
           PERFORM PRINT-TOTALS.

      ******************************************************************
       PRINT-GRAND-TOTALS SECTION.
      ******************************************************************
           MOVE 4 TO TOT-SUB.
           MOVE "GRAND TOTALS :" TO DETAIL1.
           PERFORM PRINT-TOTALS.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE 0 TO TEST-TOTAL.
           PERFORM VARYING DEL-SUB FROM 1 BY 1 UNTIL DEL-SUB > 5
              MOVE TOT-DEL-AMT(TOT-SUB DEL-SUB) TO D-BALANCE(DEL-SUB)
              ADD TOT-DEL-AMT(TOT-SUB DEL-SUB) TO TEST-TOTAL
           END-PERFORM.
           MOVE TOT-PL-MTD-AMT(TOT-SUB) TO D-MTD-PLAMT.
           MOVE TOT-PL-YTD-AMT(TOT-SUB) TO D-YTD-PLAMT.
           ADD TOT-PL-YTD-AMT(TOT-SUB) TO TEST-TOTAL.
           IF TEST-TOTAL = 0
              GO TO EXIT-PRINT-TOTALS.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM VARYING DEL-SUB FROM 1 BY 1 UNTIL DEL-SUB > 5
              MOVE TOT-NO-DEL(TOT-SUB DEL-SUB) TO D-COUNT(DEL-SUB)
           END-PERFORM.
           MOVE TOT-NO-MTD-PL(TOT-SUB) TO D-MTD-PL-COUNT.
           MOVE TOT-NO-YTD-PL(TOT-SUB) TO D-YTD-PL-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).
       EXIT-PRINT-TOTALS.
           EXIT.

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

           IF ( PROCESSING-CLEAR-SS )
              MOVE PROCESSING-TEXT-CLEAR-SS TO PROGID-TEXT
           ELSE
           IF ( PROCESSING-UPDATING )
              MOVE PROCESSING-TEXT-UPDATING TO PROGID-TEXT.

           IF EXT-ROUTE-BUF = "00"
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBLP/LPSSCL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SSUPD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SSUPD-SYSDATE.
           DISPLAY SSUPD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SSUPD-FORM-FIELDS TO WR-BUF.
           MOVE SSUPD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SSUPD-HELP--FILE TO HELP-LINK-FILE.
           MOVE SSUPD-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/SSUPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARA SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SET-SS1-KEY.
           MOVE MONTH-END-DATE-YYYYMM TO SS-DATE-YYYYMM
                                         VSS-SS-DATE-YYYYMM.
           MOVE LN-OWNBR              TO SS-BRNO
                                         VSS-SS-BRNO.
           MOVE LN-CLASS              TO SS-CLASS
                                         VSS-SS-CLASS.

       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSSGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> SSP - LN,BW
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

      *  STORED PROCEDURE:
      *
      *   SELECT LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SSNO1,
      *          LNFILE.LN_MAKERCD1,
      *          LNFILE.LN_SSNO2,
      *          LNFILE.LN_MAKERCD2,
      *          LNFILE.LN_SSNO3,
      *          LNFILE.LN_MAKERCD3,
      *          LNFILE.LN_SSNO4,
      *          LNFILE.LN_MAKERCD4,
      *          LNFILE.LN_POCD,
      *          LNFILE.LN_PLTYPE,
      *          CAST(LNFILE.LN_ENTDATE  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_PLDATE   AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)), 
      *          LNFILE.LN_MONTHEND_BAL,
      *          LNFILE.LN_BR_TFR_FG,
      *          LNFILE.LN_PLAMT,
      *          LNFILE.LN_BMO_CON_STATUS, 
      *          ISNULL(BWFILE.BW_OWNBR, 0),
      *          ISNULL(BWFILE.BW_SSNO, 0),
      *          ISNULL(BWFILE.BW_CREDIT_SCORE, 0),
      *          ISNULL(BWFILE.BW_BNKRPT_SCORE_1, 0),
      *          ISNULL(BWFILE.BW_BNKRPT_SCORE_2, 0),
      *          ISNULL(BWFILE.BW_BNKRPT_SCORE_3, 0),
      *          ISNULL(BWFILE.BW_BNKRPT_SCORE_4, 0)
      * 
      *     FROM DBO.LNFILE  LNFILE
      * 
      *     LEFT OUTER JOIN 
      *          DBO.BWFILE  BWFILE 
      *       ON (  LNFILE.LN_OWNBR         = BWFILE.BW_OWNBR  
      *      AND    LNFILE.LN_SSNO1         = BWFILE.BW_SSNO  )
      * 
      *    WHERE (( LNFILE.LN_OWNBR         = @QLN3_WBEG_OWNBR
      *      AND    LNFILE.LN_CLASS   BETWEEN @QLN3_WBEG_CLASS
      *                                   AND @QLN3_WEND_CLASS )
      *      AND (  LNFILE.LN_POCD         != '**'
      *       OR  ( LNFILE.LN_POCD          = '**'           
      *      AND    LNFILE.LN_ENTDATE      <= @QWS_MONTH_END_DATE 
      *      AND    LNFILE.LN_POFFDATE      > @QWS_MONTH_END_DATE ))
      *      AND (  LNFILE.LN_LOANDATE     <= @QWS_MONTH_END_DATE )
      *      AND (  LNFILE.LN_MONTHEND_BAL != 0 ))
      * 
      *    ORDER BY LNFILE.LN_OWNBR,
      *             LNFILE.LN_CLASS,
      *             LNFILE.LN_ACCTNO; 

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_SSUPD_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR,          
                :QLN3-WBEG-CLASS,          
                :QLN3-WEND-CLASS,          
                :QWS-MONTH-END-DATE-YYYYMM,
                :QWS-MONTH-END-DATE )
           END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.
 
           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> EMBEDDED - LN,BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LN3_CURSOR 
                INTO 
                 :QLN-OWNBR,
                 :QLN-ACCTNO,
                 :QLN-CLASS,
                 :QLN-SSNO1,
                 :QLN-MAKERCD1,
                 :QLN-SSNO2,
                 :QLN-MAKERCD2,
                 :QLN-SSNO3,
                 :QLN-MAKERCD3,
                 :QLN-SSNO4,
                 :QLN-MAKERCD4,
                 :QLN-POCD,
                 :QLN-PLTYPE,
                 :QLN-ENTDATE, 
                 :QLN-LOANDATE, 
                 :QLN-PLDATE, 
                 :QLN-POFFDATE, 
                 :QLN-MONTHEND-BAL,
                 :QLN-BR-TFR-FG,
                 :QLN-PLAMT,
                 :QLN-BMO-CON-STATUS, 
                 :QBW-OWNBR, 
                 :QBW-SSNO,  
                 :QBW-CREDIT-SCORE,  
                 :QBW-BNKRPT-SCORE-1,  
                 :QBW-BNKRPT-SCORE-2,  
                 :QBW-BNKRPT-SCORE-3,  
                 :QBW-BNKRPT-SCORE-4
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-SS-FIELDS
              PERFORM GET-BW-FIELDS
              MOVE LN-OWNBR  TO WS-LN-OWNBR
              MOVE LN-CLASS  TO WS-LN-CLASS   
              MOVE LN-ACCTNO TO WS-LN-ACCTNO.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.
  
      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.


      * COPY "LIBLP/LPLP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLP1RN
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
      *  ***NOTE*** ONLY USE START-LP1-FILE-SCAN WHEN USING A 
      *  START-LP1-FILE-NOT-GREATER.  THIS IS FOR DISPLAYING MULTIPLE
      *  PAGE IN A UP & DOWN SCENARIO.
      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER. *> EMBEDDED - LPFILE
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE LP-ACCTNO            TO QLP1-WEND-ACCTNO
              MOVE ALL "0"              TO QLP1-WBEG-SEQNO
              MOVE LP-SEQNO             TO QLP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LP1_CURSOR_NG CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),
                   LPFILE.LP_TRCD
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO    = :QLP1-WEND-BRNO
              AND LPFILE.LP_ACCTNO  = :QLP1-WEND-ACCTNO
              AND LPFILE.LP_SEQNO  <= :QLP1-WEND-SEQNO 
            ORDER BY LPFILE.LP_BRNO DESC,
                     LPFILE.LP_ACCTNO DESC,
                     LPFILE.LP_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LP1-FILE.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO. 
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

            MOVE LN-OWNBR  TO QLP-BRNO.
            MOVE LN-ACCTNO TO QLP-ACCTNO.
      *     MOVE LP-SEQNO  TO QLP-SEQNO.

           EXEC SQL
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),
                   LPFILE.LP_TRCD
              INTO
                  :QLP-BRNO,
                  :QLP-ACCTNO,
                  :QLP-SEQNO,
                  :QLP-TRDATE,
                  :QLP-TRCD
              FROM DBO.LPFILE 
             WHERE LPFILE.LP_BRNO   = :QLP-BRNO                     
               AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
               AND LPFILE.LP_SEQNO  = 
                  (SELECT MAX(LPFILE.LP_SEQNO) 
                     FROM DBO.LPFILE
                    WHERE LPFILE.LP_BRNO   = :QLP-BRNO
                      AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
                      AND LPFILE.LP_TRCD   = :QWS-TEST-LN-PLCD)    
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS. *> EMBEDDED - LPFILE
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
               FETCH LP1_CURSOR_NG 
                INTO 
                 :QLP-BRNO,
                 :QLP-ACCTNO,
                 :QLP-SEQNO,
                 :QLP-TRDATE,
                 :QLP-TRCD 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.
            IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-NG-STAT.

       COPY "LIBLP/LPBW1RN.CPY".

      * COPY "LIBLP/LPSS1IN.CPY".
      * COPYMEMBER: LIBLP/LPSS1IN
       LOAD-SS1-FILE.

      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO SS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO SS-ALL-BASE
              MOVE FILPATH-MACHINE         TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH.

      *-----------------------------------------------------------------
       OPEN-SS1-FILE.
           PERFORM LOAD-SS1-FILE.
           PERFORM OPEN-IT.
           MOVE SS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SS1-FILE. *> EMBEDDED - SSFILE
           PERFORM START-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.

           IF ( SS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SS1-FILE.



           IF ( QSS1-WEND-CLASS = ZEROES    ) OR
              ( QSS1-WEND-CLASS NOT NUMERIC )
              MOVE SS-DATE-YYYYMM TO QSS1-WBEG-DATE
              MOVE ALL "9"        TO QSS1-WEND-DATE
              MOVE SS-BRNO        TO QSS1-WBEG-BRNO
              MOVE ALL "9"        TO QSS1-WEND-BRNO
              MOVE SS-CLASS       TO QSS1-WBEG-CLASS
              MOVE ALL "9"        TO QSS1-WEND-CLASS.

           EXEC SQL
            DECLARE SS1_CURSOR CURSOR FOR
            SELECT SSFILE.SS_DATE_YYYYMM,
                   SSFILE.SS_BRNO,
                   SSFILE.SS_CLASS,
                   CAST(SSFILE.SS_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SSFILE.SS_UNITS_1_1_1,
                   SSFILE.SS_OUTBAL_1_1_1,
                   SSFILE.SS_UNITS_1_1_2,
                   SSFILE.SS_OUTBAL_1_1_2,
                   SSFILE.SS_UNITS_1_1_3,
                   SSFILE.SS_OUTBAL_1_1_3,
                   SSFILE.SS_UNITS_1_1_4,
                   SSFILE.SS_OUTBAL_1_1_4,
                   SSFILE.SS_UNITS_1_1_5,
                   SSFILE.SS_OUTBAL_1_1_5,
                   SSFILE.SS_NO_PL_1_1_1,
                   SSFILE.SS_AMT_PL_1_1_1,
                   SSFILE.SS_NO_PL_1_1_2,
                   SSFILE.SS_AMT_PL_1_1_2,
                   SSFILE.SS_UNITS_1_2_1,
                   SSFILE.SS_OUTBAL_1_2_1,
                   SSFILE.SS_UNITS_1_2_2,
                   SSFILE.SS_OUTBAL_1_2_2,
                   SSFILE.SS_UNITS_1_2_3,
                   SSFILE.SS_OUTBAL_1_2_3,
                   SSFILE.SS_UNITS_1_2_4,
                   SSFILE.SS_OUTBAL_1_2_4,
                   SSFILE.SS_UNITS_1_2_5,
                   SSFILE.SS_OUTBAL_1_2_5,
                   SSFILE.SS_NO_PL_1_2_1,
                   SSFILE.SS_AMT_PL_1_2_1,
                   SSFILE.SS_NO_PL_1_2_2,
                   SSFILE.SS_AMT_PL_1_2_2,
                   SSFILE.SS_UNITS_2_1_1,
                   SSFILE.SS_OUTBAL_2_1_1,
                   SSFILE.SS_UNITS_2_1_2,
                   SSFILE.SS_OUTBAL_2_1_2,
                   SSFILE.SS_UNITS_2_1_3,
                   SSFILE.SS_OUTBAL_2_1_3,
                   SSFILE.SS_UNITS_2_1_4,
                   SSFILE.SS_OUTBAL_2_1_4,
                   SSFILE.SS_UNITS_2_1_5,
                   SSFILE.SS_OUTBAL_2_1_5,
                   SSFILE.SS_NO_PL_2_1_1,
                   SSFILE.SS_AMT_PL_2_1_1,
                   SSFILE.SS_NO_PL_2_1_2,
                   SSFILE.SS_AMT_PL_2_1_2,
                   SSFILE.SS_UNITS_2_2_1,
                   SSFILE.SS_OUTBAL_2_2_1,
                   SSFILE.SS_UNITS_2_2_2,
                   SSFILE.SS_OUTBAL_2_2_2,
                   SSFILE.SS_UNITS_2_2_3,
                   SSFILE.SS_OUTBAL_2_2_3,
                   SSFILE.SS_UNITS_2_2_4,
                   SSFILE.SS_OUTBAL_2_2_4,
                   SSFILE.SS_UNITS_2_2_5,
                   SSFILE.SS_OUTBAL_2_2_5,
                   SSFILE.SS_NO_PL_2_2_1,
                   SSFILE.SS_AMT_PL_2_2_1,
                   SSFILE.SS_NO_PL_2_2_2,
                   SSFILE.SS_AMT_PL_2_2_2,
                   SSFILE.SS_UNITS_3_1_1,
                   SSFILE.SS_OUTBAL_3_1_1,
                   SSFILE.SS_UNITS_3_1_2,
                   SSFILE.SS_OUTBAL_3_1_2,
                   SSFILE.SS_UNITS_3_1_3,
                   SSFILE.SS_OUTBAL_3_1_3,
                   SSFILE.SS_UNITS_3_1_4,
                   SSFILE.SS_OUTBAL_3_1_4,
                   SSFILE.SS_UNITS_3_1_5,
                   SSFILE.SS_OUTBAL_3_1_5,
                   SSFILE.SS_NO_PL_3_1_1,
                   SSFILE.SS_AMT_PL_3_1_1,
                   SSFILE.SS_NO_PL_3_1_2,
                   SSFILE.SS_AMT_PL_3_1_2,
                   SSFILE.SS_UNITS_3_2_1,
                   SSFILE.SS_OUTBAL_3_2_1,
                   SSFILE.SS_UNITS_3_2_2,
                   SSFILE.SS_OUTBAL_3_2_2,
                   SSFILE.SS_UNITS_3_2_3,
                   SSFILE.SS_OUTBAL_3_2_3,
                   SSFILE.SS_UNITS_3_2_4,
                   SSFILE.SS_OUTBAL_3_2_4,
                   SSFILE.SS_UNITS_3_2_5,
                   SSFILE.SS_OUTBAL_3_2_5,
                   SSFILE.SS_NO_PL_3_2_1,
                   SSFILE.SS_AMT_PL_3_2_1,
                   SSFILE.SS_NO_PL_3_2_2,
                   SSFILE.SS_AMT_PL_3_2_2,
                   SSFILE.SS_UNITS_4_1_1,
                   SSFILE.SS_OUTBAL_4_1_1,
                   SSFILE.SS_UNITS_4_1_2,
                   SSFILE.SS_OUTBAL_4_1_2,
                   SSFILE.SS_UNITS_4_1_3,
                   SSFILE.SS_OUTBAL_4_1_3,
                   SSFILE.SS_UNITS_4_1_4,
                   SSFILE.SS_OUTBAL_4_1_4,
                   SSFILE.SS_UNITS_4_1_5,
                   SSFILE.SS_OUTBAL_4_1_5,
                   SSFILE.SS_NO_PL_4_1_1,
                   SSFILE.SS_AMT_PL_4_1_1,
                   SSFILE.SS_NO_PL_4_1_2,
                   SSFILE.SS_AMT_PL_4_1_2,
                   SSFILE.SS_UNITS_4_2_1,
                   SSFILE.SS_OUTBAL_4_2_1,
                   SSFILE.SS_UNITS_4_2_2,
                   SSFILE.SS_OUTBAL_4_2_2,
                   SSFILE.SS_UNITS_4_2_3,
                   SSFILE.SS_OUTBAL_4_2_3,
                   SSFILE.SS_UNITS_4_2_4,
                   SSFILE.SS_OUTBAL_4_2_4,
                   SSFILE.SS_UNITS_4_2_5,
                   SSFILE.SS_OUTBAL_4_2_5,
                   SSFILE.SS_NO_PL_4_2_1,
                   SSFILE.SS_AMT_PL_4_2_1,
                   SSFILE.SS_NO_PL_4_2_2,
                   SSFILE.SS_AMT_PL_4_2_2,
                   SSFILE.SS_UNITS_5_1_1,
                   SSFILE.SS_OUTBAL_5_1_1,
                   SSFILE.SS_UNITS_5_1_2,
                   SSFILE.SS_OUTBAL_5_1_2,
                   SSFILE.SS_UNITS_5_1_3,
                   SSFILE.SS_OUTBAL_5_1_3,
                   SSFILE.SS_UNITS_5_1_4,
                   SSFILE.SS_OUTBAL_5_1_4,
                   SSFILE.SS_UNITS_5_1_5,
                   SSFILE.SS_OUTBAL_5_1_5,
                   SSFILE.SS_NO_PL_5_1_1,
                   SSFILE.SS_AMT_PL_5_1_1,
                   SSFILE.SS_NO_PL_5_1_2,
                   SSFILE.SS_AMT_PL_5_1_2,
                   SSFILE.SS_UNITS_5_2_1,
                   SSFILE.SS_OUTBAL_5_2_1,
                   SSFILE.SS_UNITS_5_2_2,
                   SSFILE.SS_OUTBAL_5_2_2,
                   SSFILE.SS_UNITS_5_2_3,
                   SSFILE.SS_OUTBAL_5_2_3,
                   SSFILE.SS_UNITS_5_2_4,
                   SSFILE.SS_OUTBAL_5_2_4,
                   SSFILE.SS_UNITS_5_2_5,
                   SSFILE.SS_OUTBAL_5_2_5,
                   SSFILE.SS_NO_PL_5_2_1,
                   SSFILE.SS_AMT_PL_5_2_1,
                   SSFILE.SS_NO_PL_5_2_2,
                   SSFILE.SS_AMT_PL_5_2_2,
                   SSFILE.SS_UNITS_6_1_1,
                   SSFILE.SS_OUTBAL_6_1_1,
                   SSFILE.SS_UNITS_6_1_2,
                   SSFILE.SS_OUTBAL_6_1_2,
                   SSFILE.SS_UNITS_6_1_3,
                   SSFILE.SS_OUTBAL_6_1_3,
                   SSFILE.SS_UNITS_6_1_4,
                   SSFILE.SS_OUTBAL_6_1_4,
                   SSFILE.SS_UNITS_6_1_5,
                   SSFILE.SS_OUTBAL_6_1_5,
                   SSFILE.SS_NO_PL_6_1_1,
                   SSFILE.SS_AMT_PL_6_1_1,
                   SSFILE.SS_NO_PL_6_1_2,
                   SSFILE.SS_AMT_PL_6_1_2,
                   SSFILE.SS_UNITS_6_2_1,
                   SSFILE.SS_OUTBAL_6_2_1,
                   SSFILE.SS_UNITS_6_2_2,
                   SSFILE.SS_OUTBAL_6_2_2,
                   SSFILE.SS_UNITS_6_2_3,
                   SSFILE.SS_OUTBAL_6_2_3,
                   SSFILE.SS_UNITS_6_2_4,
                   SSFILE.SS_OUTBAL_6_2_4,
                   SSFILE.SS_UNITS_6_2_5,
                   SSFILE.SS_OUTBAL_6_2_5,
                   SSFILE.SS_NO_PL_6_2_1,
                   SSFILE.SS_AMT_PL_6_2_1,
                   SSFILE.SS_NO_PL_6_2_2,
                   SSFILE.SS_AMT_PL_6_2_2,
                   SSFILE.SS_UNITS_7_1_1,
                   SSFILE.SS_OUTBAL_7_1_1,
                   SSFILE.SS_UNITS_7_1_2,
                   SSFILE.SS_OUTBAL_7_1_2,
                   SSFILE.SS_UNITS_7_1_3,
                   SSFILE.SS_OUTBAL_7_1_3,
                   SSFILE.SS_UNITS_7_1_4,
                   SSFILE.SS_OUTBAL_7_1_4,
                   SSFILE.SS_UNITS_7_1_5,
                   SSFILE.SS_OUTBAL_7_1_5,
                   SSFILE.SS_NO_PL_7_1_1,
                   SSFILE.SS_AMT_PL_7_1_1,
                   SSFILE.SS_NO_PL_7_1_2,
                   SSFILE.SS_AMT_PL_7_1_2,
                   SSFILE.SS_UNITS_7_2_1,
                   SSFILE.SS_OUTBAL_7_2_1,
                   SSFILE.SS_UNITS_7_2_2,
                   SSFILE.SS_OUTBAL_7_2_2,
                   SSFILE.SS_UNITS_7_2_3,
                   SSFILE.SS_OUTBAL_7_2_3,
                   SSFILE.SS_UNITS_7_2_4,
                   SSFILE.SS_OUTBAL_7_2_4,
                   SSFILE.SS_UNITS_7_2_5,
                   SSFILE.SS_OUTBAL_7_2_5,
                   SSFILE.SS_NO_PL_7_2_1,
                   SSFILE.SS_AMT_PL_7_2_1,
                   SSFILE.SS_NO_PL_7_2_2,
                   SSFILE.SS_AMT_PL_7_2_2,
                   SSFILE.SS_UNITS_8_1_1,
                   SSFILE.SS_OUTBAL_8_1_1,
                   SSFILE.SS_UNITS_8_1_2,
                   SSFILE.SS_OUTBAL_8_1_2,
                   SSFILE.SS_UNITS_8_1_3,
                   SSFILE.SS_OUTBAL_8_1_3,
                   SSFILE.SS_UNITS_8_1_4,
                   SSFILE.SS_OUTBAL_8_1_4,
                   SSFILE.SS_UNITS_8_1_5,
                   SSFILE.SS_OUTBAL_8_1_5,
                   SSFILE.SS_NO_PL_8_1_1,
                   SSFILE.SS_AMT_PL_8_1_1,
                   SSFILE.SS_NO_PL_8_1_2,
                   SSFILE.SS_AMT_PL_8_1_2,
                   SSFILE.SS_UNITS_8_2_1,
                   SSFILE.SS_OUTBAL_8_2_1,
                   SSFILE.SS_UNITS_8_2_2,
                   SSFILE.SS_OUTBAL_8_2_2,
                   SSFILE.SS_UNITS_8_2_3,
                   SSFILE.SS_OUTBAL_8_2_3,
                   SSFILE.SS_UNITS_8_2_4,
                   SSFILE.SS_OUTBAL_8_2_4,
                   SSFILE.SS_UNITS_8_2_5,
                   SSFILE.SS_OUTBAL_8_2_5,
                   SSFILE.SS_NO_PL_8_2_1,
                   SSFILE.SS_AMT_PL_8_2_1,
                   SSFILE.SS_NO_PL_8_2_2,
                   SSFILE.SS_AMT_PL_8_2_2
            FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM >= :QSS1-WBEG-DATE
              AND SSFILE.SS_DATE_YYYYMM <= :QSS1-WEND-DATE
              AND SSFILE.SS_BRNO >= :QSS1-WBEG-BRNO
              AND SSFILE.SS_BRNO <= :QSS1-WEND-BRNO
              AND SSFILE.SS_CLASS >= :QSS1-WBEG-CLASS
              AND SSFILE.SS_CLASS <= :QSS1-WEND-CLASS
            ORDER BY SSFILE.SS_DATE_YYYYMM,
                     SSFILE.SS_BRNO,
                     SSFILE.SS_CLASS
           END-EXEC.

           EXEC SQL
               OPEN SS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QSS1-WBEG-DATE
                          QSS1-WEND-DATE
                          QSS1-WBEG-BRNO
                          QSS1-WEND-BRNO
                          QSS1-WBEG-CLASS
                          QSS1-WEND-CLASS.

           MOVE STAT---GOOD TO SS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SS1-FILE. *> EMBEDDED - SSFILE
           PERFORM READ-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           INITIALIZE QSS-REC.

           MOVE SS-DATE-YYYYMM TO QSS-DATE-YYYYMM.
           MOVE SS-BRNO        TO QSS-BRNO.
           MOVE SS-CLASS       TO QSS-CLASS.

           EXEC SQL
            SELECT SSFILE.SS_DATE_YYYYMM,
                   SSFILE.SS_BRNO,
                   SSFILE.SS_CLASS,
                   CAST(SSFILE.SS_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   SSFILE.SS_UNITS_1_1_1,
                   SSFILE.SS_OUTBAL_1_1_1,
                   SSFILE.SS_UNITS_1_1_2,
                   SSFILE.SS_OUTBAL_1_1_2,
                   SSFILE.SS_UNITS_1_1_3,
                   SSFILE.SS_OUTBAL_1_1_3,
                   SSFILE.SS_UNITS_1_1_4,
                   SSFILE.SS_OUTBAL_1_1_4,
                   SSFILE.SS_UNITS_1_1_5,
                   SSFILE.SS_OUTBAL_1_1_5,
                   SSFILE.SS_NO_PL_1_1_1,
                   SSFILE.SS_AMT_PL_1_1_1,
                   SSFILE.SS_NO_PL_1_1_2,
                   SSFILE.SS_AMT_PL_1_1_2,
                   SSFILE.SS_UNITS_1_2_1,
                   SSFILE.SS_OUTBAL_1_2_1,
                   SSFILE.SS_UNITS_1_2_2,
                   SSFILE.SS_OUTBAL_1_2_2,
                   SSFILE.SS_UNITS_1_2_3,
                   SSFILE.SS_OUTBAL_1_2_3,
                   SSFILE.SS_UNITS_1_2_4,
                   SSFILE.SS_OUTBAL_1_2_4,
                   SSFILE.SS_UNITS_1_2_5,
                   SSFILE.SS_OUTBAL_1_2_5,
                   SSFILE.SS_NO_PL_1_2_1,
                   SSFILE.SS_AMT_PL_1_2_1,
                   SSFILE.SS_NO_PL_1_2_2,
                   SSFILE.SS_AMT_PL_1_2_2,
                   SSFILE.SS_UNITS_2_1_1,
                   SSFILE.SS_OUTBAL_2_1_1,
                   SSFILE.SS_UNITS_2_1_2,
                   SSFILE.SS_OUTBAL_2_1_2,
                   SSFILE.SS_UNITS_2_1_3,
                   SSFILE.SS_OUTBAL_2_1_3,
                   SSFILE.SS_UNITS_2_1_4,
                   SSFILE.SS_OUTBAL_2_1_4,
                   SSFILE.SS_UNITS_2_1_5,
                   SSFILE.SS_OUTBAL_2_1_5,
                   SSFILE.SS_NO_PL_2_1_1,
                   SSFILE.SS_AMT_PL_2_1_1,
                   SSFILE.SS_NO_PL_2_1_2,
                   SSFILE.SS_AMT_PL_2_1_2,
                   SSFILE.SS_UNITS_2_2_1,
                   SSFILE.SS_OUTBAL_2_2_1,
                   SSFILE.SS_UNITS_2_2_2,
                   SSFILE.SS_OUTBAL_2_2_2,
                   SSFILE.SS_UNITS_2_2_3,
                   SSFILE.SS_OUTBAL_2_2_3,
                   SSFILE.SS_UNITS_2_2_4,
                   SSFILE.SS_OUTBAL_2_2_4,
                   SSFILE.SS_UNITS_2_2_5,
                   SSFILE.SS_OUTBAL_2_2_5,
                   SSFILE.SS_NO_PL_2_2_1,
                   SSFILE.SS_AMT_PL_2_2_1,
                   SSFILE.SS_NO_PL_2_2_2,
                   SSFILE.SS_AMT_PL_2_2_2,
                   SSFILE.SS_UNITS_3_1_1,
                   SSFILE.SS_OUTBAL_3_1_1,
                   SSFILE.SS_UNITS_3_1_2,
                   SSFILE.SS_OUTBAL_3_1_2,
                   SSFILE.SS_UNITS_3_1_3,
                   SSFILE.SS_OUTBAL_3_1_3,
                   SSFILE.SS_UNITS_3_1_4,
                   SSFILE.SS_OUTBAL_3_1_4,
                   SSFILE.SS_UNITS_3_1_5,
                   SSFILE.SS_OUTBAL_3_1_5,
                   SSFILE.SS_NO_PL_3_1_1,
                   SSFILE.SS_AMT_PL_3_1_1,
                   SSFILE.SS_NO_PL_3_1_2,
                   SSFILE.SS_AMT_PL_3_1_2,
                   SSFILE.SS_UNITS_3_2_1,
                   SSFILE.SS_OUTBAL_3_2_1,
                   SSFILE.SS_UNITS_3_2_2,
                   SSFILE.SS_OUTBAL_3_2_2,
                   SSFILE.SS_UNITS_3_2_3,
                   SSFILE.SS_OUTBAL_3_2_3,
                   SSFILE.SS_UNITS_3_2_4,
                   SSFILE.SS_OUTBAL_3_2_4,
                   SSFILE.SS_UNITS_3_2_5,
                   SSFILE.SS_OUTBAL_3_2_5,
                   SSFILE.SS_NO_PL_3_2_1,
                   SSFILE.SS_AMT_PL_3_2_1,
                   SSFILE.SS_NO_PL_3_2_2,
                   SSFILE.SS_AMT_PL_3_2_2,
                   SSFILE.SS_UNITS_4_1_1,
                   SSFILE.SS_OUTBAL_4_1_1,
                   SSFILE.SS_UNITS_4_1_2,
                   SSFILE.SS_OUTBAL_4_1_2,
                   SSFILE.SS_UNITS_4_1_3,
                   SSFILE.SS_OUTBAL_4_1_3,
                   SSFILE.SS_UNITS_4_1_4,
                   SSFILE.SS_OUTBAL_4_1_4,
                   SSFILE.SS_UNITS_4_1_5,
                   SSFILE.SS_OUTBAL_4_1_5,
                   SSFILE.SS_NO_PL_4_1_1,
                   SSFILE.SS_AMT_PL_4_1_1,
                   SSFILE.SS_NO_PL_4_1_2,
                   SSFILE.SS_AMT_PL_4_1_2,
                   SSFILE.SS_UNITS_4_2_1,
                   SSFILE.SS_OUTBAL_4_2_1,
                   SSFILE.SS_UNITS_4_2_2,
                   SSFILE.SS_OUTBAL_4_2_2,
                   SSFILE.SS_UNITS_4_2_3,
                   SSFILE.SS_OUTBAL_4_2_3,
                   SSFILE.SS_UNITS_4_2_4,
                   SSFILE.SS_OUTBAL_4_2_4,
                   SSFILE.SS_UNITS_4_2_5,
                   SSFILE.SS_OUTBAL_4_2_5,
                   SSFILE.SS_NO_PL_4_2_1,
                   SSFILE.SS_AMT_PL_4_2_1,
                   SSFILE.SS_NO_PL_4_2_2,
                   SSFILE.SS_AMT_PL_4_2_2,
                   SSFILE.SS_UNITS_5_1_1,
                   SSFILE.SS_OUTBAL_5_1_1,
                   SSFILE.SS_UNITS_5_1_2,
                   SSFILE.SS_OUTBAL_5_1_2,
                   SSFILE.SS_UNITS_5_1_3,
                   SSFILE.SS_OUTBAL_5_1_3,
                   SSFILE.SS_UNITS_5_1_4,
                   SSFILE.SS_OUTBAL_5_1_4,
                   SSFILE.SS_UNITS_5_1_5,
                   SSFILE.SS_OUTBAL_5_1_5,
                   SSFILE.SS_NO_PL_5_1_1,
                   SSFILE.SS_AMT_PL_5_1_1,
                   SSFILE.SS_NO_PL_5_1_2,
                   SSFILE.SS_AMT_PL_5_1_2,
                   SSFILE.SS_UNITS_5_2_1,
                   SSFILE.SS_OUTBAL_5_2_1,
                   SSFILE.SS_UNITS_5_2_2,
                   SSFILE.SS_OUTBAL_5_2_2,
                   SSFILE.SS_UNITS_5_2_3,
                   SSFILE.SS_OUTBAL_5_2_3,
                   SSFILE.SS_UNITS_5_2_4,
                   SSFILE.SS_OUTBAL_5_2_4,
                   SSFILE.SS_UNITS_5_2_5,
                   SSFILE.SS_OUTBAL_5_2_5,
                   SSFILE.SS_NO_PL_5_2_1,
                   SSFILE.SS_AMT_PL_5_2_1,
                   SSFILE.SS_NO_PL_5_2_2,
                   SSFILE.SS_AMT_PL_5_2_2,
                   SSFILE.SS_UNITS_6_1_1,
                   SSFILE.SS_OUTBAL_6_1_1,
                   SSFILE.SS_UNITS_6_1_2,
                   SSFILE.SS_OUTBAL_6_1_2,
                   SSFILE.SS_UNITS_6_1_3,
                   SSFILE.SS_OUTBAL_6_1_3,
                   SSFILE.SS_UNITS_6_1_4,
                   SSFILE.SS_OUTBAL_6_1_4,
                   SSFILE.SS_UNITS_6_1_5,
                   SSFILE.SS_OUTBAL_6_1_5,
                   SSFILE.SS_NO_PL_6_1_1,
                   SSFILE.SS_AMT_PL_6_1_1,
                   SSFILE.SS_NO_PL_6_1_2,
                   SSFILE.SS_AMT_PL_6_1_2,
                   SSFILE.SS_UNITS_6_2_1,
                   SSFILE.SS_OUTBAL_6_2_1,
                   SSFILE.SS_UNITS_6_2_2,
                   SSFILE.SS_OUTBAL_6_2_2,
                   SSFILE.SS_UNITS_6_2_3,
                   SSFILE.SS_OUTBAL_6_2_3,
                   SSFILE.SS_UNITS_6_2_4,
                   SSFILE.SS_OUTBAL_6_2_4,
                   SSFILE.SS_UNITS_6_2_5,
                   SSFILE.SS_OUTBAL_6_2_5,
                   SSFILE.SS_NO_PL_6_2_1,
                   SSFILE.SS_AMT_PL_6_2_1,
                   SSFILE.SS_NO_PL_6_2_2,
                   SSFILE.SS_AMT_PL_6_2_2,
                   SSFILE.SS_UNITS_7_1_1,
                   SSFILE.SS_OUTBAL_7_1_1,
                   SSFILE.SS_UNITS_7_1_2,
                   SSFILE.SS_OUTBAL_7_1_2,
                   SSFILE.SS_UNITS_7_1_3,
                   SSFILE.SS_OUTBAL_7_1_3,
                   SSFILE.SS_UNITS_7_1_4,
                   SSFILE.SS_OUTBAL_7_1_4,
                   SSFILE.SS_UNITS_7_1_5,
                   SSFILE.SS_OUTBAL_7_1_5,
                   SSFILE.SS_NO_PL_7_1_1,
                   SSFILE.SS_AMT_PL_7_1_1,
                   SSFILE.SS_NO_PL_7_1_2,
                   SSFILE.SS_AMT_PL_7_1_2,
                   SSFILE.SS_UNITS_7_2_1,
                   SSFILE.SS_OUTBAL_7_2_1,
                   SSFILE.SS_UNITS_7_2_2,
                   SSFILE.SS_OUTBAL_7_2_2,
                   SSFILE.SS_UNITS_7_2_3,
                   SSFILE.SS_OUTBAL_7_2_3,
                   SSFILE.SS_UNITS_7_2_4,
                   SSFILE.SS_OUTBAL_7_2_4,
                   SSFILE.SS_UNITS_7_2_5,
                   SSFILE.SS_OUTBAL_7_2_5,
                   SSFILE.SS_NO_PL_7_2_1,
                   SSFILE.SS_AMT_PL_7_2_1,
                   SSFILE.SS_NO_PL_7_2_2,
                   SSFILE.SS_AMT_PL_7_2_2,
                   SSFILE.SS_UNITS_8_1_1,
                   SSFILE.SS_OUTBAL_8_1_1,
                   SSFILE.SS_UNITS_8_1_2,
                   SSFILE.SS_OUTBAL_8_1_2,
                   SSFILE.SS_UNITS_8_1_3,
                   SSFILE.SS_OUTBAL_8_1_3,
                   SSFILE.SS_UNITS_8_1_4,
                   SSFILE.SS_OUTBAL_8_1_4,
                   SSFILE.SS_UNITS_8_1_5,
                   SSFILE.SS_OUTBAL_8_1_5,
                   SSFILE.SS_NO_PL_8_1_1,
                   SSFILE.SS_AMT_PL_8_1_1,
                   SSFILE.SS_NO_PL_8_1_2,
                   SSFILE.SS_AMT_PL_8_1_2,
                   SSFILE.SS_UNITS_8_2_1,
                   SSFILE.SS_OUTBAL_8_2_1,
                   SSFILE.SS_UNITS_8_2_2,
                   SSFILE.SS_OUTBAL_8_2_2,
                   SSFILE.SS_UNITS_8_2_3,
                   SSFILE.SS_OUTBAL_8_2_3,
                   SSFILE.SS_UNITS_8_2_4,
                   SSFILE.SS_OUTBAL_8_2_4,
                   SSFILE.SS_UNITS_8_2_5,
                   SSFILE.SS_OUTBAL_8_2_5,
                   SSFILE.SS_NO_PL_8_2_1,
                   SSFILE.SS_AMT_PL_8_2_1,
                   SSFILE.SS_NO_PL_8_2_2,
                   SSFILE.SS_AMT_PL_8_2_2
            INTO :QSS-REC
            FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM
              AND SSFILE.SS_BRNO = :QSS-BRNO
              AND SSFILE.SS_CLASS = :QSS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SS-FIELDS.

      *-----------------------------------------------------------------
       READ-SS1-FILE-NEXT. *> EMBEDDED - SSFILE
           PERFORM READ-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           INITIALIZE QSS-REC.

           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SS1_CURSOR INTO :QSS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-SS1-FILE. *> EMBEDDED - SSFILE

           ACCEPT SS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           PERFORM SET-SS-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.SSFILE
             (SSFILE.SS_DATE_YYYYMM,
              SSFILE.SS_BRNO,
              SSFILE.SS_CLASS,
              SSFILE.SS_LTOUCH_DATE,
              SSFILE.SS_UNITS_1_1_1,
              SSFILE.SS_OUTBAL_1_1_1,
              SSFILE.SS_UNITS_1_1_2,
              SSFILE.SS_OUTBAL_1_1_2,
              SSFILE.SS_UNITS_1_1_3,
              SSFILE.SS_OUTBAL_1_1_3,
              SSFILE.SS_UNITS_1_1_4,
              SSFILE.SS_OUTBAL_1_1_4,
              SSFILE.SS_UNITS_1_1_5,
              SSFILE.SS_OUTBAL_1_1_5,
              SSFILE.SS_NO_PL_1_1_1,
              SSFILE.SS_AMT_PL_1_1_1,
              SSFILE.SS_NO_PL_1_1_2,
              SSFILE.SS_AMT_PL_1_1_2,
              SSFILE.SS_UNITS_1_2_1,
              SSFILE.SS_OUTBAL_1_2_1,
              SSFILE.SS_UNITS_1_2_2,
              SSFILE.SS_OUTBAL_1_2_2,
              SSFILE.SS_UNITS_1_2_3,
              SSFILE.SS_OUTBAL_1_2_3,
              SSFILE.SS_UNITS_1_2_4,
              SSFILE.SS_OUTBAL_1_2_4,
              SSFILE.SS_UNITS_1_2_5,
              SSFILE.SS_OUTBAL_1_2_5,
              SSFILE.SS_NO_PL_1_2_1,
              SSFILE.SS_AMT_PL_1_2_1,
              SSFILE.SS_NO_PL_1_2_2,
              SSFILE.SS_AMT_PL_1_2_2,
              SSFILE.SS_UNITS_2_1_1,
              SSFILE.SS_OUTBAL_2_1_1,
              SSFILE.SS_UNITS_2_1_2,
              SSFILE.SS_OUTBAL_2_1_2,
              SSFILE.SS_UNITS_2_1_3,
              SSFILE.SS_OUTBAL_2_1_3,
              SSFILE.SS_UNITS_2_1_4,
              SSFILE.SS_OUTBAL_2_1_4,
              SSFILE.SS_UNITS_2_1_5,
              SSFILE.SS_OUTBAL_2_1_5,
              SSFILE.SS_NO_PL_2_1_1,
              SSFILE.SS_AMT_PL_2_1_1,
              SSFILE.SS_NO_PL_2_1_2,
              SSFILE.SS_AMT_PL_2_1_2,
              SSFILE.SS_UNITS_2_2_1,
              SSFILE.SS_OUTBAL_2_2_1,
              SSFILE.SS_UNITS_2_2_2,
              SSFILE.SS_OUTBAL_2_2_2,
              SSFILE.SS_UNITS_2_2_3,
              SSFILE.SS_OUTBAL_2_2_3,
              SSFILE.SS_UNITS_2_2_4,
              SSFILE.SS_OUTBAL_2_2_4,
              SSFILE.SS_UNITS_2_2_5,
              SSFILE.SS_OUTBAL_2_2_5,
              SSFILE.SS_NO_PL_2_2_1,
              SSFILE.SS_AMT_PL_2_2_1,
              SSFILE.SS_NO_PL_2_2_2,
              SSFILE.SS_AMT_PL_2_2_2,
              SSFILE.SS_UNITS_3_1_1,
              SSFILE.SS_OUTBAL_3_1_1,
              SSFILE.SS_UNITS_3_1_2,
              SSFILE.SS_OUTBAL_3_1_2,
              SSFILE.SS_UNITS_3_1_3,
              SSFILE.SS_OUTBAL_3_1_3,
              SSFILE.SS_UNITS_3_1_4,
              SSFILE.SS_OUTBAL_3_1_4,
              SSFILE.SS_UNITS_3_1_5,
              SSFILE.SS_OUTBAL_3_1_5,
              SSFILE.SS_NO_PL_3_1_1,
              SSFILE.SS_AMT_PL_3_1_1,
              SSFILE.SS_NO_PL_3_1_2,
              SSFILE.SS_AMT_PL_3_1_2,
              SSFILE.SS_UNITS_3_2_1,
              SSFILE.SS_OUTBAL_3_2_1,
              SSFILE.SS_UNITS_3_2_2,
              SSFILE.SS_OUTBAL_3_2_2,
              SSFILE.SS_UNITS_3_2_3,
              SSFILE.SS_OUTBAL_3_2_3,
              SSFILE.SS_UNITS_3_2_4,
              SSFILE.SS_OUTBAL_3_2_4,
              SSFILE.SS_UNITS_3_2_5,
              SSFILE.SS_OUTBAL_3_2_5,
              SSFILE.SS_NO_PL_3_2_1,
              SSFILE.SS_AMT_PL_3_2_1,
              SSFILE.SS_NO_PL_3_2_2,
              SSFILE.SS_AMT_PL_3_2_2,
              SSFILE.SS_UNITS_4_1_1,
              SSFILE.SS_OUTBAL_4_1_1,
              SSFILE.SS_UNITS_4_1_2,
              SSFILE.SS_OUTBAL_4_1_2,
              SSFILE.SS_UNITS_4_1_3,
              SSFILE.SS_OUTBAL_4_1_3,
              SSFILE.SS_UNITS_4_1_4,
              SSFILE.SS_OUTBAL_4_1_4,
              SSFILE.SS_UNITS_4_1_5,
              SSFILE.SS_OUTBAL_4_1_5,
              SSFILE.SS_NO_PL_4_1_1,
              SSFILE.SS_AMT_PL_4_1_1,
              SSFILE.SS_NO_PL_4_1_2,
              SSFILE.SS_AMT_PL_4_1_2,
              SSFILE.SS_UNITS_4_2_1,
              SSFILE.SS_OUTBAL_4_2_1,
              SSFILE.SS_UNITS_4_2_2,
              SSFILE.SS_OUTBAL_4_2_2,
              SSFILE.SS_UNITS_4_2_3,
              SSFILE.SS_OUTBAL_4_2_3,
              SSFILE.SS_UNITS_4_2_4,
              SSFILE.SS_OUTBAL_4_2_4,
              SSFILE.SS_UNITS_4_2_5,
              SSFILE.SS_OUTBAL_4_2_5,
              SSFILE.SS_NO_PL_4_2_1,
              SSFILE.SS_AMT_PL_4_2_1,
              SSFILE.SS_NO_PL_4_2_2,
              SSFILE.SS_AMT_PL_4_2_2,
              SSFILE.SS_UNITS_5_1_1,
              SSFILE.SS_OUTBAL_5_1_1,
              SSFILE.SS_UNITS_5_1_2,
              SSFILE.SS_OUTBAL_5_1_2,
              SSFILE.SS_UNITS_5_1_3,
              SSFILE.SS_OUTBAL_5_1_3,
              SSFILE.SS_UNITS_5_1_4,
              SSFILE.SS_OUTBAL_5_1_4,
              SSFILE.SS_UNITS_5_1_5,
              SSFILE.SS_OUTBAL_5_1_5,
              SSFILE.SS_NO_PL_5_1_1,
              SSFILE.SS_AMT_PL_5_1_1,
              SSFILE.SS_NO_PL_5_1_2,
              SSFILE.SS_AMT_PL_5_1_2,
              SSFILE.SS_UNITS_5_2_1,
              SSFILE.SS_OUTBAL_5_2_1,
              SSFILE.SS_UNITS_5_2_2,
              SSFILE.SS_OUTBAL_5_2_2,
              SSFILE.SS_UNITS_5_2_3,
              SSFILE.SS_OUTBAL_5_2_3,
              SSFILE.SS_UNITS_5_2_4,
              SSFILE.SS_OUTBAL_5_2_4,
              SSFILE.SS_UNITS_5_2_5,
              SSFILE.SS_OUTBAL_5_2_5,
              SSFILE.SS_NO_PL_5_2_1,
              SSFILE.SS_AMT_PL_5_2_1,
              SSFILE.SS_NO_PL_5_2_2,
              SSFILE.SS_AMT_PL_5_2_2,
              SSFILE.SS_UNITS_6_1_1,
              SSFILE.SS_OUTBAL_6_1_1,
              SSFILE.SS_UNITS_6_1_2,
              SSFILE.SS_OUTBAL_6_1_2,
              SSFILE.SS_UNITS_6_1_3,
              SSFILE.SS_OUTBAL_6_1_3,
              SSFILE.SS_UNITS_6_1_4,
              SSFILE.SS_OUTBAL_6_1_4,
              SSFILE.SS_UNITS_6_1_5,
              SSFILE.SS_OUTBAL_6_1_5,
              SSFILE.SS_NO_PL_6_1_1,
              SSFILE.SS_AMT_PL_6_1_1,
              SSFILE.SS_NO_PL_6_1_2,
              SSFILE.SS_AMT_PL_6_1_2,
              SSFILE.SS_UNITS_6_2_1,
              SSFILE.SS_OUTBAL_6_2_1,
              SSFILE.SS_UNITS_6_2_2,
              SSFILE.SS_OUTBAL_6_2_2,
              SSFILE.SS_UNITS_6_2_3,
              SSFILE.SS_OUTBAL_6_2_3,
              SSFILE.SS_UNITS_6_2_4,
              SSFILE.SS_OUTBAL_6_2_4,
              SSFILE.SS_UNITS_6_2_5,
              SSFILE.SS_OUTBAL_6_2_5,
              SSFILE.SS_NO_PL_6_2_1,
              SSFILE.SS_AMT_PL_6_2_1,
              SSFILE.SS_NO_PL_6_2_2,
              SSFILE.SS_AMT_PL_6_2_2,
              SSFILE.SS_UNITS_7_1_1,
              SSFILE.SS_OUTBAL_7_1_1,
              SSFILE.SS_UNITS_7_1_2,
              SSFILE.SS_OUTBAL_7_1_2,
              SSFILE.SS_UNITS_7_1_3,
              SSFILE.SS_OUTBAL_7_1_3,
              SSFILE.SS_UNITS_7_1_4,
              SSFILE.SS_OUTBAL_7_1_4,
              SSFILE.SS_UNITS_7_1_5,
              SSFILE.SS_OUTBAL_7_1_5,
              SSFILE.SS_NO_PL_7_1_1,
              SSFILE.SS_AMT_PL_7_1_1,
              SSFILE.SS_NO_PL_7_1_2,
              SSFILE.SS_AMT_PL_7_1_2,
              SSFILE.SS_UNITS_7_2_1,
              SSFILE.SS_OUTBAL_7_2_1,
              SSFILE.SS_UNITS_7_2_2,
              SSFILE.SS_OUTBAL_7_2_2,
              SSFILE.SS_UNITS_7_2_3,
              SSFILE.SS_OUTBAL_7_2_3,
              SSFILE.SS_UNITS_7_2_4,
              SSFILE.SS_OUTBAL_7_2_4,
              SSFILE.SS_UNITS_7_2_5,
              SSFILE.SS_OUTBAL_7_2_5,
              SSFILE.SS_NO_PL_7_2_1,
              SSFILE.SS_AMT_PL_7_2_1,
              SSFILE.SS_NO_PL_7_2_2,
              SSFILE.SS_AMT_PL_7_2_2,
              SSFILE.SS_UNITS_8_1_1,
              SSFILE.SS_OUTBAL_8_1_1,
              SSFILE.SS_UNITS_8_1_2,
              SSFILE.SS_OUTBAL_8_1_2,
              SSFILE.SS_UNITS_8_1_3,
              SSFILE.SS_OUTBAL_8_1_3,
              SSFILE.SS_UNITS_8_1_4,
              SSFILE.SS_OUTBAL_8_1_4,
              SSFILE.SS_UNITS_8_1_5,
              SSFILE.SS_OUTBAL_8_1_5,
              SSFILE.SS_NO_PL_8_1_1,
              SSFILE.SS_AMT_PL_8_1_1,
              SSFILE.SS_NO_PL_8_1_2,
              SSFILE.SS_AMT_PL_8_1_2,
              SSFILE.SS_UNITS_8_2_1,
              SSFILE.SS_OUTBAL_8_2_1,
              SSFILE.SS_UNITS_8_2_2,
              SSFILE.SS_OUTBAL_8_2_2,
              SSFILE.SS_UNITS_8_2_3,
              SSFILE.SS_OUTBAL_8_2_3,
              SSFILE.SS_UNITS_8_2_4,
              SSFILE.SS_OUTBAL_8_2_4,
              SSFILE.SS_UNITS_8_2_5,
              SSFILE.SS_OUTBAL_8_2_5,
              SSFILE.SS_NO_PL_8_2_1,
              SSFILE.SS_AMT_PL_8_2_1,
              SSFILE.SS_NO_PL_8_2_2,
              SSFILE.SS_AMT_PL_8_2_2)
            VALUES
             (:QSS-DATE-YYYYMM,
              :QSS-BRNO,
              :QSS-CLASS,
              :QSS-LTOUCH-DATE,
              :QSS-UNITS-1-1-1,
              :QSS-OUTBAL-1-1-1,
              :QSS-UNITS-1-1-2,
              :QSS-OUTBAL-1-1-2,
              :QSS-UNITS-1-1-3,
              :QSS-OUTBAL-1-1-3,
              :QSS-UNITS-1-1-4,
              :QSS-OUTBAL-1-1-4,
              :QSS-UNITS-1-1-5,
              :QSS-OUTBAL-1-1-5,
              :QSS-NO-PL-1-1-1,
              :QSS-AMT-PL-1-1-1,
              :QSS-NO-PL-1-1-2,
              :QSS-AMT-PL-1-1-2,
              :QSS-UNITS-1-2-1,
              :QSS-OUTBAL-1-2-1,
              :QSS-UNITS-1-2-2,
              :QSS-OUTBAL-1-2-2,
              :QSS-UNITS-1-2-3,
              :QSS-OUTBAL-1-2-3,
              :QSS-UNITS-1-2-4,
              :QSS-OUTBAL-1-2-4,
              :QSS-UNITS-1-2-5,
              :QSS-OUTBAL-1-2-5,
              :QSS-NO-PL-1-2-1,
              :QSS-AMT-PL-1-2-1,
              :QSS-NO-PL-1-2-2,
              :QSS-AMT-PL-1-2-2,
              :QSS-UNITS-2-1-1,
              :QSS-OUTBAL-2-1-1,
              :QSS-UNITS-2-1-2,
              :QSS-OUTBAL-2-1-2,
              :QSS-UNITS-2-1-3,
              :QSS-OUTBAL-2-1-3,
              :QSS-UNITS-2-1-4,
              :QSS-OUTBAL-2-1-4,
              :QSS-UNITS-2-1-5,
              :QSS-OUTBAL-2-1-5,
              :QSS-NO-PL-2-1-1,
              :QSS-AMT-PL-2-1-1,
              :QSS-NO-PL-2-1-2,
              :QSS-AMT-PL-2-1-2,
              :QSS-UNITS-2-2-1,
              :QSS-OUTBAL-2-2-1,
              :QSS-UNITS-2-2-2,
              :QSS-OUTBAL-2-2-2,
              :QSS-UNITS-2-2-3,
              :QSS-OUTBAL-2-2-3,
              :QSS-UNITS-2-2-4,
              :QSS-OUTBAL-2-2-4,
              :QSS-UNITS-2-2-5,
              :QSS-OUTBAL-2-2-5,
              :QSS-NO-PL-2-2-1,
              :QSS-AMT-PL-2-2-1,
              :QSS-NO-PL-2-2-2,
              :QSS-AMT-PL-2-2-2,
              :QSS-UNITS-3-1-1,
              :QSS-OUTBAL-3-1-1,
              :QSS-UNITS-3-1-2,
              :QSS-OUTBAL-3-1-2,
              :QSS-UNITS-3-1-3,
              :QSS-OUTBAL-3-1-3,
              :QSS-UNITS-3-1-4,
              :QSS-OUTBAL-3-1-4,
              :QSS-UNITS-3-1-5,
              :QSS-OUTBAL-3-1-5,
              :QSS-NO-PL-3-1-1,
              :QSS-AMT-PL-3-1-1,
              :QSS-NO-PL-3-1-2,
              :QSS-AMT-PL-3-1-2,
              :QSS-UNITS-3-2-1,
              :QSS-OUTBAL-3-2-1,
              :QSS-UNITS-3-2-2,
              :QSS-OUTBAL-3-2-2,
              :QSS-UNITS-3-2-3,
              :QSS-OUTBAL-3-2-3,
              :QSS-UNITS-3-2-4,
              :QSS-OUTBAL-3-2-4,
              :QSS-UNITS-3-2-5,
              :QSS-OUTBAL-3-2-5,
              :QSS-NO-PL-3-2-1,
              :QSS-AMT-PL-3-2-1,
              :QSS-NO-PL-3-2-2,
              :QSS-AMT-PL-3-2-2,
              :QSS-UNITS-4-1-1,
              :QSS-OUTBAL-4-1-1,
              :QSS-UNITS-4-1-2,
              :QSS-OUTBAL-4-1-2,
              :QSS-UNITS-4-1-3,
              :QSS-OUTBAL-4-1-3,
              :QSS-UNITS-4-1-4,
              :QSS-OUTBAL-4-1-4,
              :QSS-UNITS-4-1-5,
              :QSS-OUTBAL-4-1-5,
              :QSS-NO-PL-4-1-1,
              :QSS-AMT-PL-4-1-1,
              :QSS-NO-PL-4-1-2,
              :QSS-AMT-PL-4-1-2,
              :QSS-UNITS-4-2-1,
              :QSS-OUTBAL-4-2-1,
              :QSS-UNITS-4-2-2,
              :QSS-OUTBAL-4-2-2,
              :QSS-UNITS-4-2-3,
              :QSS-OUTBAL-4-2-3,
              :QSS-UNITS-4-2-4,
              :QSS-OUTBAL-4-2-4,
              :QSS-UNITS-4-2-5,
              :QSS-OUTBAL-4-2-5,
              :QSS-NO-PL-4-2-1,
              :QSS-AMT-PL-4-2-1,
              :QSS-NO-PL-4-2-2,
              :QSS-AMT-PL-4-2-2,
              :QSS-UNITS-5-1-1,
              :QSS-OUTBAL-5-1-1,
              :QSS-UNITS-5-1-2,
              :QSS-OUTBAL-5-1-2,
              :QSS-UNITS-5-1-3,
              :QSS-OUTBAL-5-1-3,
              :QSS-UNITS-5-1-4,
              :QSS-OUTBAL-5-1-4,
              :QSS-UNITS-5-1-5,
              :QSS-OUTBAL-5-1-5,
              :QSS-NO-PL-5-1-1,
              :QSS-AMT-PL-5-1-1,
              :QSS-NO-PL-5-1-2,
              :QSS-AMT-PL-5-1-2,
              :QSS-UNITS-5-2-1,
              :QSS-OUTBAL-5-2-1,
              :QSS-UNITS-5-2-2,
              :QSS-OUTBAL-5-2-2,
              :QSS-UNITS-5-2-3,
              :QSS-OUTBAL-5-2-3,
              :QSS-UNITS-5-2-4,
              :QSS-OUTBAL-5-2-4,
              :QSS-UNITS-5-2-5,
              :QSS-OUTBAL-5-2-5,
              :QSS-NO-PL-5-2-1,
              :QSS-AMT-PL-5-2-1,
              :QSS-NO-PL-5-2-2,
              :QSS-AMT-PL-5-2-2,
              :QSS-UNITS-6-1-1,
              :QSS-OUTBAL-6-1-1,
              :QSS-UNITS-6-1-2,
              :QSS-OUTBAL-6-1-2,
              :QSS-UNITS-6-1-3,
              :QSS-OUTBAL-6-1-3,
              :QSS-UNITS-6-1-4,
              :QSS-OUTBAL-6-1-4,
              :QSS-UNITS-6-1-5,
              :QSS-OUTBAL-6-1-5,
              :QSS-NO-PL-6-1-1,
              :QSS-AMT-PL-6-1-1,
              :QSS-NO-PL-6-1-2,
              :QSS-AMT-PL-6-1-2,
              :QSS-UNITS-6-2-1,
              :QSS-OUTBAL-6-2-1,
              :QSS-UNITS-6-2-2,
              :QSS-OUTBAL-6-2-2,
              :QSS-UNITS-6-2-3,
              :QSS-OUTBAL-6-2-3,
              :QSS-UNITS-6-2-4,
              :QSS-OUTBAL-6-2-4,
              :QSS-UNITS-6-2-5,
              :QSS-OUTBAL-6-2-5,
              :QSS-NO-PL-6-2-1,
              :QSS-AMT-PL-6-2-1,
              :QSS-NO-PL-6-2-2,
              :QSS-AMT-PL-6-2-2,
              :QSS-UNITS-7-1-1,
              :QSS-OUTBAL-7-1-1,
              :QSS-UNITS-7-1-2,
              :QSS-OUTBAL-7-1-2,
              :QSS-UNITS-7-1-3,
              :QSS-OUTBAL-7-1-3,
              :QSS-UNITS-7-1-4,
              :QSS-OUTBAL-7-1-4,
              :QSS-UNITS-7-1-5,
              :QSS-OUTBAL-7-1-5,
              :QSS-NO-PL-7-1-1,
              :QSS-AMT-PL-7-1-1,
              :QSS-NO-PL-7-1-2,
              :QSS-AMT-PL-7-1-2,
              :QSS-UNITS-7-2-1,
              :QSS-OUTBAL-7-2-1,
              :QSS-UNITS-7-2-2,
              :QSS-OUTBAL-7-2-2,
              :QSS-UNITS-7-2-3,
              :QSS-OUTBAL-7-2-3,
              :QSS-UNITS-7-2-4,
              :QSS-OUTBAL-7-2-4,
              :QSS-UNITS-7-2-5,
              :QSS-OUTBAL-7-2-5,
              :QSS-NO-PL-7-2-1,
              :QSS-AMT-PL-7-2-1,
              :QSS-NO-PL-7-2-2,
              :QSS-AMT-PL-7-2-2,
              :QSS-UNITS-8-1-1,
              :QSS-OUTBAL-8-1-1,
              :QSS-UNITS-8-1-2,
              :QSS-OUTBAL-8-1-2,
              :QSS-UNITS-8-1-3,
              :QSS-OUTBAL-8-1-3,
              :QSS-UNITS-8-1-4,
              :QSS-OUTBAL-8-1-4,
              :QSS-UNITS-8-1-5,
              :QSS-OUTBAL-8-1-5,
              :QSS-NO-PL-8-1-1,
              :QSS-AMT-PL-8-1-1,
              :QSS-NO-PL-8-1-2,
              :QSS-AMT-PL-8-1-2,
              :QSS-UNITS-8-2-1,
              :QSS-OUTBAL-8-2-1,
              :QSS-UNITS-8-2-2,
              :QSS-OUTBAL-8-2-2,
              :QSS-UNITS-8-2-3,
              :QSS-OUTBAL-8-2-3,
              :QSS-UNITS-8-2-4,
              :QSS-OUTBAL-8-2-4,
              :QSS-UNITS-8-2-5,
              :QSS-OUTBAL-8-2-5,
              :QSS-NO-PL-8-2-1,
              :QSS-AMT-PL-8-2-1,
              :QSS-NO-PL-8-2-2,
              :QSS-AMT-PL-8-2-2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-SS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-SS1-FILE. *> EMBEDDED - SSFILE

           ACCEPT SS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           PERFORM SET-SS-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.SSFILE
             SET
              SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM,
              SSFILE.SS_BRNO = :QSS-BRNO,
              SSFILE.SS_CLASS = :QSS-CLASS,
              SSFILE.SS_LTOUCH_DATE = :QSS-LTOUCH-DATE,
              SSFILE.SS_UNITS_1_1_1 = :QSS-UNITS-1-1-1,
              SSFILE.SS_OUTBAL_1_1_1 = :QSS-OUTBAL-1-1-1,
              SSFILE.SS_UNITS_1_1_2 = :QSS-UNITS-1-1-2,
              SSFILE.SS_OUTBAL_1_1_2 = :QSS-OUTBAL-1-1-2,
              SSFILE.SS_UNITS_1_1_3 = :QSS-UNITS-1-1-3,
              SSFILE.SS_OUTBAL_1_1_3 = :QSS-OUTBAL-1-1-3,
              SSFILE.SS_UNITS_1_1_4 = :QSS-UNITS-1-1-4,
              SSFILE.SS_OUTBAL_1_1_4 = :QSS-OUTBAL-1-1-4,
              SSFILE.SS_UNITS_1_1_5 = :QSS-UNITS-1-1-5,
              SSFILE.SS_OUTBAL_1_1_5 = :QSS-OUTBAL-1-1-5,
              SSFILE.SS_NO_PL_1_1_1 = :QSS-NO-PL-1-1-1,
              SSFILE.SS_AMT_PL_1_1_1 = :QSS-AMT-PL-1-1-1,
              SSFILE.SS_NO_PL_1_1_2 = :QSS-NO-PL-1-1-2,
              SSFILE.SS_AMT_PL_1_1_2 = :QSS-AMT-PL-1-1-2,
              SSFILE.SS_UNITS_1_2_1 = :QSS-UNITS-1-2-1,
              SSFILE.SS_OUTBAL_1_2_1 = :QSS-OUTBAL-1-2-1,
              SSFILE.SS_UNITS_1_2_2 = :QSS-UNITS-1-2-2,
              SSFILE.SS_OUTBAL_1_2_2 = :QSS-OUTBAL-1-2-2,
              SSFILE.SS_UNITS_1_2_3 = :QSS-UNITS-1-2-3,
              SSFILE.SS_OUTBAL_1_2_3 = :QSS-OUTBAL-1-2-3,
              SSFILE.SS_UNITS_1_2_4 = :QSS-UNITS-1-2-4,
              SSFILE.SS_OUTBAL_1_2_4 = :QSS-OUTBAL-1-2-4,
              SSFILE.SS_UNITS_1_2_5 = :QSS-UNITS-1-2-5,
              SSFILE.SS_OUTBAL_1_2_5 = :QSS-OUTBAL-1-2-5,
              SSFILE.SS_NO_PL_1_2_1 = :QSS-NO-PL-1-2-1,
              SSFILE.SS_AMT_PL_1_2_1 = :QSS-AMT-PL-1-2-1,
              SSFILE.SS_NO_PL_1_2_2 = :QSS-NO-PL-1-2-2,
              SSFILE.SS_AMT_PL_1_2_2 = :QSS-AMT-PL-1-2-2,
              SSFILE.SS_UNITS_2_1_1 = :QSS-UNITS-2-1-1,
              SSFILE.SS_OUTBAL_2_1_1 = :QSS-OUTBAL-2-1-1,
              SSFILE.SS_UNITS_2_1_2 = :QSS-UNITS-2-1-2,
              SSFILE.SS_OUTBAL_2_1_2 = :QSS-OUTBAL-2-1-2,
              SSFILE.SS_UNITS_2_1_3 = :QSS-UNITS-2-1-3,
              SSFILE.SS_OUTBAL_2_1_3 = :QSS-OUTBAL-2-1-3,
              SSFILE.SS_UNITS_2_1_4 = :QSS-UNITS-2-1-4,
              SSFILE.SS_OUTBAL_2_1_4 = :QSS-OUTBAL-2-1-4,
              SSFILE.SS_UNITS_2_1_5 = :QSS-UNITS-2-1-5,
              SSFILE.SS_OUTBAL_2_1_5 = :QSS-OUTBAL-2-1-5,
              SSFILE.SS_NO_PL_2_1_1 = :QSS-NO-PL-2-1-1,
              SSFILE.SS_AMT_PL_2_1_1 = :QSS-AMT-PL-2-1-1,
              SSFILE.SS_NO_PL_2_1_2 = :QSS-NO-PL-2-1-2,
              SSFILE.SS_AMT_PL_2_1_2 = :QSS-AMT-PL-2-1-2,
              SSFILE.SS_UNITS_2_2_1 = :QSS-UNITS-2-2-1,
              SSFILE.SS_OUTBAL_2_2_1 = :QSS-OUTBAL-2-2-1,
              SSFILE.SS_UNITS_2_2_2 = :QSS-UNITS-2-2-2,
              SSFILE.SS_OUTBAL_2_2_2 = :QSS-OUTBAL-2-2-2,
              SSFILE.SS_UNITS_2_2_3 = :QSS-UNITS-2-2-3,
              SSFILE.SS_OUTBAL_2_2_3 = :QSS-OUTBAL-2-2-3,
              SSFILE.SS_UNITS_2_2_4 = :QSS-UNITS-2-2-4,
              SSFILE.SS_OUTBAL_2_2_4 = :QSS-OUTBAL-2-2-4,
              SSFILE.SS_UNITS_2_2_5 = :QSS-UNITS-2-2-5,
              SSFILE.SS_OUTBAL_2_2_5 = :QSS-OUTBAL-2-2-5,
              SSFILE.SS_NO_PL_2_2_1 = :QSS-NO-PL-2-2-1,
              SSFILE.SS_AMT_PL_2_2_1 = :QSS-AMT-PL-2-2-1,
              SSFILE.SS_NO_PL_2_2_2 = :QSS-NO-PL-2-2-2,
              SSFILE.SS_AMT_PL_2_2_2 = :QSS-AMT-PL-2-2-2,
              SSFILE.SS_UNITS_3_1_1 = :QSS-UNITS-3-1-1,
              SSFILE.SS_OUTBAL_3_1_1 = :QSS-OUTBAL-3-1-1,
              SSFILE.SS_UNITS_3_1_2 = :QSS-UNITS-3-1-2,
              SSFILE.SS_OUTBAL_3_1_2 = :QSS-OUTBAL-3-1-2,
              SSFILE.SS_UNITS_3_1_3 = :QSS-UNITS-3-1-3,
              SSFILE.SS_OUTBAL_3_1_3 = :QSS-OUTBAL-3-1-3,
              SSFILE.SS_UNITS_3_1_4 = :QSS-UNITS-3-1-4,
              SSFILE.SS_OUTBAL_3_1_4 = :QSS-OUTBAL-3-1-4,
              SSFILE.SS_UNITS_3_1_5 = :QSS-UNITS-3-1-5,
              SSFILE.SS_OUTBAL_3_1_5 = :QSS-OUTBAL-3-1-5,
              SSFILE.SS_NO_PL_3_1_1 = :QSS-NO-PL-3-1-1,
              SSFILE.SS_AMT_PL_3_1_1 = :QSS-AMT-PL-3-1-1,
              SSFILE.SS_NO_PL_3_1_2 = :QSS-NO-PL-3-1-2,
              SSFILE.SS_AMT_PL_3_1_2 = :QSS-AMT-PL-3-1-2,
              SSFILE.SS_UNITS_3_2_1 = :QSS-UNITS-3-2-1,
              SSFILE.SS_OUTBAL_3_2_1 = :QSS-OUTBAL-3-2-1,
              SSFILE.SS_UNITS_3_2_2 = :QSS-UNITS-3-2-2,
              SSFILE.SS_OUTBAL_3_2_2 = :QSS-OUTBAL-3-2-2,
              SSFILE.SS_UNITS_3_2_3 = :QSS-UNITS-3-2-3,
              SSFILE.SS_OUTBAL_3_2_3 = :QSS-OUTBAL-3-2-3,
              SSFILE.SS_UNITS_3_2_4 = :QSS-UNITS-3-2-4,
              SSFILE.SS_OUTBAL_3_2_4 = :QSS-OUTBAL-3-2-4,
              SSFILE.SS_UNITS_3_2_5 = :QSS-UNITS-3-2-5,
              SSFILE.SS_OUTBAL_3_2_5 = :QSS-OUTBAL-3-2-5,
              SSFILE.SS_NO_PL_3_2_1 = :QSS-NO-PL-3-2-1,
              SSFILE.SS_AMT_PL_3_2_1 = :QSS-AMT-PL-3-2-1,
              SSFILE.SS_NO_PL_3_2_2 = :QSS-NO-PL-3-2-2,
              SSFILE.SS_AMT_PL_3_2_2 = :QSS-AMT-PL-3-2-2,
              SSFILE.SS_UNITS_4_1_1 = :QSS-UNITS-4-1-1,
              SSFILE.SS_OUTBAL_4_1_1 = :QSS-OUTBAL-4-1-1,
              SSFILE.SS_UNITS_4_1_2 = :QSS-UNITS-4-1-2,
              SSFILE.SS_OUTBAL_4_1_2 = :QSS-OUTBAL-4-1-2,
              SSFILE.SS_UNITS_4_1_3 = :QSS-UNITS-4-1-3,
              SSFILE.SS_OUTBAL_4_1_3 = :QSS-OUTBAL-4-1-3,
              SSFILE.SS_UNITS_4_1_4 = :QSS-UNITS-4-1-4,
              SSFILE.SS_OUTBAL_4_1_4 = :QSS-OUTBAL-4-1-4,
              SSFILE.SS_UNITS_4_1_5 = :QSS-UNITS-4-1-5,
              SSFILE.SS_OUTBAL_4_1_5 = :QSS-OUTBAL-4-1-5,
              SSFILE.SS_NO_PL_4_1_1 = :QSS-NO-PL-4-1-1,
              SSFILE.SS_AMT_PL_4_1_1 = :QSS-AMT-PL-4-1-1,
              SSFILE.SS_NO_PL_4_1_2 = :QSS-NO-PL-4-1-2,
              SSFILE.SS_AMT_PL_4_1_2 = :QSS-AMT-PL-4-1-2,
              SSFILE.SS_UNITS_4_2_1 = :QSS-UNITS-4-2-1,
              SSFILE.SS_OUTBAL_4_2_1 = :QSS-OUTBAL-4-2-1,
              SSFILE.SS_UNITS_4_2_2 = :QSS-UNITS-4-2-2,
              SSFILE.SS_OUTBAL_4_2_2 = :QSS-OUTBAL-4-2-2,
              SSFILE.SS_UNITS_4_2_3 = :QSS-UNITS-4-2-3,
              SSFILE.SS_OUTBAL_4_2_3 = :QSS-OUTBAL-4-2-3,
              SSFILE.SS_UNITS_4_2_4 = :QSS-UNITS-4-2-4,
              SSFILE.SS_OUTBAL_4_2_4 = :QSS-OUTBAL-4-2-4,
              SSFILE.SS_UNITS_4_2_5 = :QSS-UNITS-4-2-5,
              SSFILE.SS_OUTBAL_4_2_5 = :QSS-OUTBAL-4-2-5,
              SSFILE.SS_NO_PL_4_2_1 = :QSS-NO-PL-4-2-1,
              SSFILE.SS_AMT_PL_4_2_1 = :QSS-AMT-PL-4-2-1,
              SSFILE.SS_NO_PL_4_2_2 = :QSS-NO-PL-4-2-2,
              SSFILE.SS_AMT_PL_4_2_2 = :QSS-AMT-PL-4-2-2,
              SSFILE.SS_UNITS_5_1_1 = :QSS-UNITS-5-1-1,
              SSFILE.SS_OUTBAL_5_1_1 = :QSS-OUTBAL-5-1-1,
              SSFILE.SS_UNITS_5_1_2 = :QSS-UNITS-5-1-2,
              SSFILE.SS_OUTBAL_5_1_2 = :QSS-OUTBAL-5-1-2,
              SSFILE.SS_UNITS_5_1_3 = :QSS-UNITS-5-1-3,
              SSFILE.SS_OUTBAL_5_1_3 = :QSS-OUTBAL-5-1-3,
              SSFILE.SS_UNITS_5_1_4 = :QSS-UNITS-5-1-4,
              SSFILE.SS_OUTBAL_5_1_4 = :QSS-OUTBAL-5-1-4,
              SSFILE.SS_UNITS_5_1_5 = :QSS-UNITS-5-1-5,
              SSFILE.SS_OUTBAL_5_1_5 = :QSS-OUTBAL-5-1-5,
              SSFILE.SS_NO_PL_5_1_1 = :QSS-NO-PL-5-1-1,
              SSFILE.SS_AMT_PL_5_1_1 = :QSS-AMT-PL-5-1-1,
              SSFILE.SS_NO_PL_5_1_2 = :QSS-NO-PL-5-1-2,
              SSFILE.SS_AMT_PL_5_1_2 = :QSS-AMT-PL-5-1-2,
              SSFILE.SS_UNITS_5_2_1 = :QSS-UNITS-5-2-1,
              SSFILE.SS_OUTBAL_5_2_1 = :QSS-OUTBAL-5-2-1,
              SSFILE.SS_UNITS_5_2_2 = :QSS-UNITS-5-2-2,
              SSFILE.SS_OUTBAL_5_2_2 = :QSS-OUTBAL-5-2-2,
              SSFILE.SS_UNITS_5_2_3 = :QSS-UNITS-5-2-3,
              SSFILE.SS_OUTBAL_5_2_3 = :QSS-OUTBAL-5-2-3,
              SSFILE.SS_UNITS_5_2_4 = :QSS-UNITS-5-2-4,
              SSFILE.SS_OUTBAL_5_2_4 = :QSS-OUTBAL-5-2-4,
              SSFILE.SS_UNITS_5_2_5 = :QSS-UNITS-5-2-5,
              SSFILE.SS_OUTBAL_5_2_5 = :QSS-OUTBAL-5-2-5,
              SSFILE.SS_NO_PL_5_2_1 = :QSS-NO-PL-5-2-1,
              SSFILE.SS_AMT_PL_5_2_1 = :QSS-AMT-PL-5-2-1,
              SSFILE.SS_NO_PL_5_2_2 = :QSS-NO-PL-5-2-2,
              SSFILE.SS_AMT_PL_5_2_2 = :QSS-AMT-PL-5-2-2,
              SSFILE.SS_UNITS_6_1_1 = :QSS-UNITS-6-1-1,
              SSFILE.SS_OUTBAL_6_1_1 = :QSS-OUTBAL-6-1-1,
              SSFILE.SS_UNITS_6_1_2 = :QSS-UNITS-6-1-2,
              SSFILE.SS_OUTBAL_6_1_2 = :QSS-OUTBAL-6-1-2,
              SSFILE.SS_UNITS_6_1_3 = :QSS-UNITS-6-1-3,
              SSFILE.SS_OUTBAL_6_1_3 = :QSS-OUTBAL-6-1-3,
              SSFILE.SS_UNITS_6_1_4 = :QSS-UNITS-6-1-4,
              SSFILE.SS_OUTBAL_6_1_4 = :QSS-OUTBAL-6-1-4,
              SSFILE.SS_UNITS_6_1_5 = :QSS-UNITS-6-1-5,
              SSFILE.SS_OUTBAL_6_1_5 = :QSS-OUTBAL-6-1-5,
              SSFILE.SS_NO_PL_6_1_1 = :QSS-NO-PL-6-1-1,
              SSFILE.SS_AMT_PL_6_1_1 = :QSS-AMT-PL-6-1-1,
              SSFILE.SS_NO_PL_6_1_2 = :QSS-NO-PL-6-1-2,
              SSFILE.SS_AMT_PL_6_1_2 = :QSS-AMT-PL-6-1-2,
              SSFILE.SS_UNITS_6_2_1 = :QSS-UNITS-6-2-1,
              SSFILE.SS_OUTBAL_6_2_1 = :QSS-OUTBAL-6-2-1,
              SSFILE.SS_UNITS_6_2_2 = :QSS-UNITS-6-2-2,
              SSFILE.SS_OUTBAL_6_2_2 = :QSS-OUTBAL-6-2-2,
              SSFILE.SS_UNITS_6_2_3 = :QSS-UNITS-6-2-3,
              SSFILE.SS_OUTBAL_6_2_3 = :QSS-OUTBAL-6-2-3,
              SSFILE.SS_UNITS_6_2_4 = :QSS-UNITS-6-2-4,
              SSFILE.SS_OUTBAL_6_2_4 = :QSS-OUTBAL-6-2-4,
              SSFILE.SS_UNITS_6_2_5 = :QSS-UNITS-6-2-5,
              SSFILE.SS_OUTBAL_6_2_5 = :QSS-OUTBAL-6-2-5,
              SSFILE.SS_NO_PL_6_2_1 = :QSS-NO-PL-6-2-1,
              SSFILE.SS_AMT_PL_6_2_1 = :QSS-AMT-PL-6-2-1,
              SSFILE.SS_NO_PL_6_2_2 = :QSS-NO-PL-6-2-2,
              SSFILE.SS_AMT_PL_6_2_2 = :QSS-AMT-PL-6-2-2,
              SSFILE.SS_UNITS_7_1_1 = :QSS-UNITS-7-1-1,
              SSFILE.SS_OUTBAL_7_1_1 = :QSS-OUTBAL-7-1-1,
              SSFILE.SS_UNITS_7_1_2 = :QSS-UNITS-7-1-2,
              SSFILE.SS_OUTBAL_7_1_2 = :QSS-OUTBAL-7-1-2,
              SSFILE.SS_UNITS_7_1_3 = :QSS-UNITS-7-1-3,
              SSFILE.SS_OUTBAL_7_1_3 = :QSS-OUTBAL-7-1-3,
              SSFILE.SS_UNITS_7_1_4 = :QSS-UNITS-7-1-4,
              SSFILE.SS_OUTBAL_7_1_4 = :QSS-OUTBAL-7-1-4,
              SSFILE.SS_UNITS_7_1_5 = :QSS-UNITS-7-1-5,
              SSFILE.SS_OUTBAL_7_1_5 = :QSS-OUTBAL-7-1-5,
              SSFILE.SS_NO_PL_7_1_1 = :QSS-NO-PL-7-1-1,
              SSFILE.SS_AMT_PL_7_1_1 = :QSS-AMT-PL-7-1-1,
              SSFILE.SS_NO_PL_7_1_2 = :QSS-NO-PL-7-1-2,
              SSFILE.SS_AMT_PL_7_1_2 = :QSS-AMT-PL-7-1-2,
              SSFILE.SS_UNITS_7_2_1 = :QSS-UNITS-7-2-1,
              SSFILE.SS_OUTBAL_7_2_1 = :QSS-OUTBAL-7-2-1,
              SSFILE.SS_UNITS_7_2_2 = :QSS-UNITS-7-2-2,
              SSFILE.SS_OUTBAL_7_2_2 = :QSS-OUTBAL-7-2-2,
              SSFILE.SS_UNITS_7_2_3 = :QSS-UNITS-7-2-3,
              SSFILE.SS_OUTBAL_7_2_3 = :QSS-OUTBAL-7-2-3,
              SSFILE.SS_UNITS_7_2_4 = :QSS-UNITS-7-2-4,
              SSFILE.SS_OUTBAL_7_2_4 = :QSS-OUTBAL-7-2-4,
              SSFILE.SS_UNITS_7_2_5 = :QSS-UNITS-7-2-5,
              SSFILE.SS_OUTBAL_7_2_5 = :QSS-OUTBAL-7-2-5,
              SSFILE.SS_NO_PL_7_2_1 = :QSS-NO-PL-7-2-1,
              SSFILE.SS_AMT_PL_7_2_1 = :QSS-AMT-PL-7-2-1,
              SSFILE.SS_NO_PL_7_2_2 = :QSS-NO-PL-7-2-2,
              SSFILE.SS_AMT_PL_7_2_2 = :QSS-AMT-PL-7-2-2,
              SSFILE.SS_UNITS_8_1_1 = :QSS-UNITS-8-1-1,
              SSFILE.SS_OUTBAL_8_1_1 = :QSS-OUTBAL-8-1-1,
              SSFILE.SS_UNITS_8_1_2 = :QSS-UNITS-8-1-2,
              SSFILE.SS_OUTBAL_8_1_2 = :QSS-OUTBAL-8-1-2,
              SSFILE.SS_UNITS_8_1_3 = :QSS-UNITS-8-1-3,
              SSFILE.SS_OUTBAL_8_1_3 = :QSS-OUTBAL-8-1-3,
              SSFILE.SS_UNITS_8_1_4 = :QSS-UNITS-8-1-4,
              SSFILE.SS_OUTBAL_8_1_4 = :QSS-OUTBAL-8-1-4,
              SSFILE.SS_UNITS_8_1_5 = :QSS-UNITS-8-1-5,
              SSFILE.SS_OUTBAL_8_1_5 = :QSS-OUTBAL-8-1-5,
              SSFILE.SS_NO_PL_8_1_1 = :QSS-NO-PL-8-1-1,
              SSFILE.SS_AMT_PL_8_1_1 = :QSS-AMT-PL-8-1-1,
              SSFILE.SS_NO_PL_8_1_2 = :QSS-NO-PL-8-1-2,
              SSFILE.SS_AMT_PL_8_1_2 = :QSS-AMT-PL-8-1-2,
              SSFILE.SS_UNITS_8_2_1 = :QSS-UNITS-8-2-1,
              SSFILE.SS_OUTBAL_8_2_1 = :QSS-OUTBAL-8-2-1,
              SSFILE.SS_UNITS_8_2_2 = :QSS-UNITS-8-2-2,
              SSFILE.SS_OUTBAL_8_2_2 = :QSS-OUTBAL-8-2-2,
              SSFILE.SS_UNITS_8_2_3 = :QSS-UNITS-8-2-3,
              SSFILE.SS_OUTBAL_8_2_3 = :QSS-OUTBAL-8-2-3,
              SSFILE.SS_UNITS_8_2_4 = :QSS-UNITS-8-2-4,
              SSFILE.SS_OUTBAL_8_2_4 = :QSS-OUTBAL-8-2-4,
              SSFILE.SS_UNITS_8_2_5 = :QSS-UNITS-8-2-5,
              SSFILE.SS_OUTBAL_8_2_5 = :QSS-OUTBAL-8-2-5,
              SSFILE.SS_NO_PL_8_2_1 = :QSS-NO-PL-8-2-1,
              SSFILE.SS_AMT_PL_8_2_1 = :QSS-AMT-PL-8-2-1,
              SSFILE.SS_NO_PL_8_2_2 = :QSS-NO-PL-8-2-2,
              SSFILE.SS_AMT_PL_8_2_2 = :QSS-AMT-PL-8-2-2
            WHERE SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM
              AND SSFILE.SS_BRNO = :QSS-BRNO
              AND SSFILE.SS_CLASS = :QSS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-SS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-SS1-FILE. *> EMBEDDED - SSFILE

           PERFORM DELETE-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           PERFORM SET-SS-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM
              AND SSFILE.SS_BRNO = :QSS-BRNO
              AND SSFILE.SS_CLASS = :QSS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-SS1-FILE.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-SS1-FILE.

           PERFORM CLOSE-IT.

           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SS1-CURSOR-STAT.

      * COPY "LIBLP/LPLM1RN.CPY".
      * COPYMEMBER: LIBLP/LPLM1RN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL    TO E-FILE.

           IF LM1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LM1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LM1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LM1-FILE. *> SSP - LMFILE
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

           IF ( QLM1-WEND-BRNO = ZEROES    ) OR
              ( QLM1-WEND-BRNO NOT NUMERIC )
              MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
              MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
              MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLM1-WEND-ACCTNO
              MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QLM1-WEND-DATE-REV
              MOVE LM-SEQ               TO QLM1-WBEG-SEQ
              MOVE ALL "9"              TO QLM1-WEND-SEQ.

      
      *  STORED PROCEDURE:
      *
      *   SELECT LMFILE.LM_BRNO,
      *          LMFILE.LM_ACCTNO,
      *          CAST(LMFILE.LM_DATE_REVERSED AS VARCHAR(10)), 
      *          LMFILE.LM_SEQ,
      *          LMFILE.LM_MEMO, 
      *          LMFILE.LM_MEMO_TYPE 
      * 
      *     FROM DBO.LMFILE  LMFILE
      *  
      *    WHERE  ( LMFILE.LM_BRNO             = @QLM1_WBEG_BRNO 
      *      AND    LMFILE.LM_ACCTNO           = @QLM1_WBEG_ACCTNO 
      *      AND    LMFILE.LM_DATE_REVERSED    < @QWS_MEMO_CUT_OFF_DATE
      *      AND    LMFILE.LM_MEMO_TYPE        = '9'
      *      AND    SUBSTRING(LMFILE.LM_MEMO, 1, 9) = 'TFR FR BR' )
      * 
      *    ORDER BY LMFILE.LM_BRNO, 
      *             LMFILE.LM_ACCTNO,
      *             LMFILE.LM_DATE_REVERSED; 


           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LM1_CURSOR CURSOR FOR
                :LM1-RETURN-CODE = EXEC SSP_SSUPD_LMFILE_SELECT 
              ( :QLN3-WBEG-OWNBR,
                :QLN3-WBEG-CLASS,
                :QLN3-WEND-CLASS,
                :QWS-MEMO-CUT-OFF-DATE,
                :QWS-MONTH-END-DATE )      
           END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ.

           MOVE STAT---GOOD TO LM1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LM1-FILE-NEXT. *> EMBEDDED - LMFILE
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LM1_CURSOR 
                    INTO 
                     :QLM-BRNO,
                     :QWS-LM-CLASS,
                     :QLM-ACCTNO,
                     :QLM-DATE-REVERSED, 
                     :QLM-SEQ,
                     :QLM-MEMO, 
                     :QLM-MEMO-TYPE 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS
              MOVE LM-BRNO       TO WS-LM-OWNBR
              MOVE QWS-LM-CLASS  TO WS-LM-CLASS   
              MOVE LM-ACCTNO     TO WS-LM-ACCTNO.
 
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.
           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LM1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LM1-CONNECTION.

           IF LM1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LM1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LM1-CONN-OPEN-SW.


       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       OPEN-VSS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE "VSS" TO E-FILE.
           OPEN OUTPUT VSS-FILE.
           IF IO-FG = 8
              GO TO OPEN-VSS-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE VSS-FILE
              GO TO OPEN-VSS-FILE-OUTPUT.
           UNLOCK VSS-FILE.
       OPEN-VSS-FILE.
           PERFORM OPEN-IT.
           MOVE "VSS" TO E-FILE.
           OPEN I-O VSS-FILE.
           IF IO-FG = 8
              GO TO OPEN-VSS-FILE.
           IF IO-FG = 7
              CLOSE VSS-FILE
              GO TO OPEN-VSS-FILE.
           UNLOCK VSS-FILE.
       READ-VSS-FILE.
           PERFORM READ-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           READ VSS-FILE.
           IF IO-FG = 8
              GO TO READ-VSS-FILE.
       READ-VSS-FILE-NEXT.
           PERFORM READ-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           READ VSS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VSS-FILE-NEXT.
       READ-VSS-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           READ VSS-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-VSS-FILE-PREVIOUS.
       START-VSS-FILE-LE.
           PERFORM START-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           START VSS-FILE KEY <= VSS1-KEY.
           UNLOCK VSS-FILE.
       START-VSS-FILE-GREATER.
           PERFORM START-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           START VSS-FILE KEY > VSS1-KEY.
           UNLOCK VSS-FILE.
       START-VSS-FILE.
           PERFORM START-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           START VSS-FILE KEY NOT < VSS1-KEY.
           UNLOCK VSS-FILE.
       WRITE-VSS-FILE.
           PERFORM WRITE-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           WRITE VSS-REC 
             INVALID KEY
                MOVE 22 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
              GO TO WRITE-VSS-FILE.
           UNLOCK VSS-FILE.
       REWRITE-VSS-FILE.
           PERFORM REWRITE-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           REWRITE VSS-REC.
           IF IO-FG = 8
              GO TO REWRITE-VSS-FILE.
           UNLOCK VSS-FILE.
       DELETE-VSS-FILE.
           PERFORM DELETE-IT.
           MOVE "VSS" TO E-FILE.
           MOVE VSS1-KEY TO E-KEYX.
           DELETE VSS-FILE.
           IF IO-FG = 8
              GO TO DELETE-VSS-FILE.
           UNLOCK VSS-FILE.
       CLOSE-VSS-FILE.
           PERFORM CLOSE-IT.
           CLOSE VSS-FILE.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           MOVE VSS-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK-PATH  TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SSUPD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
