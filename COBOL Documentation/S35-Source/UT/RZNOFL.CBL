       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RZNOFL.
       DATE-WRITTEN.    01/16/2024.
      ******************************************************************
      *                      (SP)
      *    DESCRIPTION:  CREATE NOTICE FILE FOR ALL NON 0 BALANCE ACCOUNTS
      *                  WORLD WILL RUN NIGHTLY TO CREATE NEW NOTICE RECORDS
      *
      * CREATE NOFILE RECORDS FOR EVERY LOAN WITH A BALANCE
      * AS OF 2024-09-10 CHANGE: SKIPS PLS.
      *
      * RUNCBL -C /$REL/SH/XCBLCONFIG +E /$FIL/../LG/ACU_TCLP /$REL/CO/EOCRON 
      *     NOF +00000 +00000 0001 0001
      *
      * LOG OF BRANCHES RAN - LG/CRNOFL.YYYYMMDD
      *      1 LINE PER BRANCH. IF NO RECORDS READ OR NO RECORDS
      *      WRITTEN/REWRITTEN, THE FIRST 4 BYTES WILL SAY "ZERO"
      *      OTHERWISE, WILL SAY "PASS"
      * STATUS    BRNO   START        READ      REWRITE      WRITE
      * "ZERO"    0010  04:56 PM         0          0          0
      * "PASS"    0011  04:56 PM      7537       7537          0
      *
      *****************************************************************************
      ***    HOW NOFILE UPDATES/INSERTS ARE APPLIED TO DBO.NOFILE  2024-09-10  ****
      * ORIGINAL PROCESSING NOW CREATES A FILE CONTAINING THE NOFILE RECORDS
      *  TO BE APPLIED TO DBO.NOFILE.
      * AFTER FILE CREATION IS COMPLETED, CRNOFL CALLS A SCRIPT TO RUN A BCP 
      *  (SQL BATCH LOAD) LOADING THE CRNOFL OUTPUT RECORDS INTO A WORK TABLE.
      * ONCE COMPLETED, A SCRIPT IS CALLED TO USE SQLCMD TO EXECUTE UPDATE (REWRITE)
      *  AND INSERT (WRITE) QUERIES UPDATE DBO.NOFILE FROM DATA LOADED INTO THE 
      *  WORK TABLE.
      *=================================================================
      * REV:
      * 2024.0314 (RMZ) - ADDED STORED PROCEDURE TO IMPROVE RUN-TIME. 
      * 2024.0607 BAH ADDED MOVE AGE-DATE TO LP-PAYDATE
      *            THIS IS NEEDED FOR DEFERMENT FORMULA 8 WHICH CALLS DEFI01
      *            AND IT PASSES LP-PAYDATE IN DEF-POST-DATE TO CALC ELAPSED
      *            PERIODS. WE WERE GETTING NON-NUMERIC MESSAGES IN ACU_TCLP
      *            AS IT WAS 20202020
      * 2024.0625 (RMZ) - BYPASS ROWS WHERE LN_ACCTNO = 0 (S35Q-74) 
      * 2024-0709 JKC - CHANGED FOR SSP TO USE A TEMP TABLE TO AVOID
      *      TABLE LOCKS.  ALSO ADDED LOGIC TO PRODUCE A REPORT SHOWING
      *      TOTALS OF ACCOUNTS READ, SKIP, WRITE, REWRITE PER BRANCH.
      * 2024-0719 BAH ADDED LG/CRNOFL.YYYYMMDD LOG FILE WITH 1 LINE
      *               PER BRANCH. S35Q-94
      * 2024-0723 BAH REMOVE REPORT-PATH WORKFILE UPON EXIT S35Q-94
      * 2024-0724 MB  CHANGED SSP TO NOT RETURN NOFILE VALUES S35Q-94
      *                 ADDED Y/N INDICATOR FOR NOFILE REC EXISTENCE.
      * 2024-0910 MB  S35Q-125 CHANGED TO CREATE FILE FOR LNFILE UPDATES 
      *                AT END OF PROGRAM CALL SCRIPT (BCP) TO LOAD UPDATE RECORDS
      *                INTO WORK TABLE AND THEN APPLY UPDATES/INSERTS USING
      *                SQLCMD.  BCP/SQLCMD SCRIPTS AND UPDATE/INSERT SQL
      *                CURRENTLY IN USR/S35/SH.
      *                IF INCLUDE PAYOUTS = 'Y' UPDATES FILL BE APPLIED
      *                EVERY 200,000 RECORDS.
      *                ONLY PROCESS WHERE LN_PLDATE = 19000101 (SKIP PLS)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSSQLLOG.CPY".

           SELECT REPORT-FILE ASSIGN REPORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY REPORT-KEY
                  FILE STATUS FILE-STAT.

           SELECT LOG-FILE ASSIGN LOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.


           SELECT WKNO-FILE ASSIGN WKNO-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT WKNO-RESULT-FILE ASSIGN WKNO-PATH-RESULT-FILE
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBGB/GBFDSQLLOG.CPY".

      ******************************************************************
      *
      *    WKNO-FILE - FILE TO BE LOADED TO DBO.WKNOFILE SQL TABLE 
      *
      ******************************************************************
       FD  WKNO-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WKNO-REC.
           03  WKNO-BYTE               PIC X OCCURS 2000 TIMES.

       FD  WKNO-RESULT-FILE
           LABEL RECORDS ARE STANDARD.
       01  WKNO-RESULT-REC                    PIC X(200).

      ******************************************************************
      *
      *    R E P O R T - F I L E
      *
      *-----------------------------------------------------------------
      *    FILE USED TO PRINT A REPORT
      ******************************************************************
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.

      *=================================================================
       01  REPORT-REC.
           03  REPORT-KEY.
               05  REPORT-BRNO             PIC 9(10).
           03  REPORT-RECS-READ            PIC 9(10).
           03  REPORT-RECS-REWRITE         PIC 9(10).
           03  REPORT-RECS-SKIP            PIC 9(10).
           03  REPORT-RECS-WRITE           PIC 9(10).

      **************************************************
      *  CRNOFL LOG FILE                      *
      *   LG/CRNOFL.YYYYMMDD
      **************************************************
       FD  LOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  LOG-REC.
           03  LOG-STATUS        PIC X(4).
           03  FILLER            PIC X.
           03  LOG-BRNO          PIC 9(4).
           03  FILLER            PIC X.
           03  LOG-TIME          PIC X(8).
           03  FILLER            PIC X.
           03  LOG-READ          PIC Z(7)9.
           03  FILLER            PIC X.
           03  LOG-REWRITE       PIC Z(7)9.
           03  FILLER            PIC X.
           03  LOG-WRITE         PIC Z(7)9.

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZNOFL S35 10/16/24-15:35:43 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
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
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GBWSSQLLOG.CPY".

       01  LOG-PATH.
           03  LOG-BASE           PIC  X(09).
           03  FILLER             PIC X(01)   VALUE "/".
           03  LOG-MACHINE        PIC X(02).
           03  FILLER             PIC X(11)  VALUE "/LG/CRNOFL.".
           03  LOG-DATE           PIC 9(8).

       01  WKNO-PATH.
           03  WK-PATH-BASE            PIC X(9).
           03  FILLER                  PIC X        VALUE "/".
           03  WK-PATH-MACHINE         PIC X(2).
           03  FILLER                  PIC X(4)     VALUE "/WK/".
           03  WK-PATH-PGM-TBL         PIC X(10)    VALUE "CRNOFL-NO-".
           03  WK-PATH-PROC-ID         PIC X(6).
           03  FILLER                  PIC X        VALUE "-".
           03  WKNO-PATH-BEGBRNO       PIC X(4).
           03  WKNO-PATH-DAT           PIC X(4)    VALUE ".DAT".

       01  WKNO-PATH-RESULT-FILE.
           03  WKNO-PATH-RESULT-DIR    PIC X(37).
           03  WKNO-PATH-RESULT-RES    PIC X(4).

       01  WKNO-KEY.
           05  WKNO-KEY-YMD            PIC X(8).
           05  WKNO-KEY-PROC-ID        PIC X(6).
           05  WKNO-KEY-BEGBRNO        PIC X(4).

       01  PIPE-1                      PIC X         VALUE X"7C".
       01  LN3-CONN-OPEN-SW            PIC X(1)      VALUE "N".
       01  ECHO-CMD                    PIC X(8)      VALUE "ECHO -E ".
       01  CURRENT-DATE-YYMMDD         PIC 9(06).
       01  CURRENT-DATE-YYYY-MM-DD     PIC 9999/99/99.
       01  TOTAL-ACCOUNTS-PROC-CTR     PIC 9(9)      VALUE 0.
       01  BCP-FAIL-SW                 PIC X         VALUE "N".

      *==================================[ EXTRACT DISPLAY FIELD (EDF) ]
       01  EDF-WORKERS.
           03  EDF-SQLCABC             PIC 9(9)-.
           03  EDF-SQLCODE             PIC 9(9)-.
           03  EDF-SQLERRML            PIC 9(4)-.
           03  EDF-SQLERRD             PIC 9(9)- OCCURS 6 TIMES.
           03  EDF-SQLERRPL            PIC 9(4)-.
           03  EDF-TIME-HH-MM-SS       PIC 99/99/99.


       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-INCLUDE-PAIDOUTS        PIC X         VALUE SPACE.
       01  WS-WKNO-KEY                 PIC X(18).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  REPORT-FILE-WORKERS.
           03  HOLD-REPORT-BRNO        PIC 9(04)     VALUE ZEROES.
           03  REPORT-IFN              PIC X(06)     VALUE "REPORT".
           03  REPORT-PATH             PIC X(35)     VALUE SPACES.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  MESS-LIST                   PIC X(5)      VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER                  PIC X(29)     VALUE
                                           "BUILDING NOTICE FILE....".
           03  DISPLAY-MESS-BR         PIC 9(4).
           03  FILLER                  PIC X(10).

       01  RECS-COUNT-WORKERS.
           03  RECS-COUNT-BRNO-READ    PIC 9(9)      VALUE ZEROES.
           03  RECS-COUNT-BRNO-REWRITE PIC 9(9)      VALUE ZEROES.
           03  RECS-COUNT-BRNO-SKIP    PIC 9(9)      VALUE ZEROES.
           03  RECS-COUNT-BRNO-WRITE   PIC 9(9)      VALUE ZEROES.

       01  WS-TRACE-OPTION             PIC 9(2)      VALUE ZEROES.
       01  WS-TRACE-REC-COUNT          PIC 9(9)      VALUE ZEROES.
       01  WS-TRACE-REC-TOTAL          PIC 9(9)      VALUE ZEROES.
       01  WS-TRACE-REC-SHOW           PIC ZZZ,ZZZ,ZZ9.

       01  SUB                         PIC 9(9)      VALUE ZEROES.
       01  LAST-BRACKET-POS            PIC 9(3)      VALUE ZEROES.
       01  UPDATE-CTR                  PIC 9(9)      VALUE ZEROES.
       01  STARTING-TIME               PIC X(8)      VALUE SPACES.
       01  ENDING-TIME                 PIC X(8)      VALUE SPACES.
       01  SCRIPT-CALL-ERROR-SW        PIC X         VALUE "N".
       01  SCRIPT-ERROR-MSG            PIC X(100)    VALUE SPACES.
       01  SCRIPT-MISC                 PIC X(30)     VALUE SPACES.
       01  SCRIPT-TYPE                 PIC X(3)      VALUE SPACES.
       01  SCRIPT-ERROR-DETAIL-MSG     PIC X(200)    VALUE SPACES.
       01  MISC-MSG                    PIC X(300)    VALUE SPACES.
       01  FOUND-SW                    PIC X         VALUE "N".

       01  TCLP-CRNOFL-SHELL-COMMAND. 
           03  TCLP-CRNOFL-SHELL-PGM.
              05  FILLER               PIC X(09)   VALUE "/$REL/SH/".
              05  TASC-EC              PIC X(03)   VALUE "EC ".
              05  FILLER               PIC X(09)   VALUE "/$REL/SH/".
              05  TASC-SCRIPT          PIC X(20)   VALUE SPACES.
          03  TCLP-CRNOFL-SHELL-PARMS  PIC X(200)  VALUE SPACES.

       01  WS-QNO-FIELDS.
           03  WS-QNO-TDUELC             PIC  +9(7).99.
           03  WS-QNO-TDUEAMT            PIC  +9(7).99.
           03  WS-QNO-TDUEPRIN           PIC  +9(7).99.
           03  WS-QNO-TDUETBAL           PIC  +9(7).99.
           03  WS-QNO-TDUEPOFF           PIC  +9(7).99. 
           03  WS-QNO-LDUELC             PIC  +9(7).99.
           03  WS-QNO-LDUEAMT            PIC  +9(7).99. 
           03  WS-QNO-AMT-PAID-LAST      PIC  +9(7).99. 
           03  WS-QNO-NXDUEAMT           PIC  +9(7).99.
           03  WS-QNO-NXDUELC            PIC  +9(7).99. 
           03  WS-QNO-LCHG-INTDUE        PIC  +9(7).99.
           03  WS-QNO-AVAILCREDIT        PIC  +9(7).99.
           03  WS-QNO-TDUEDELINQ         PIC  +9(7).99.
           03  WS-QNO-DEFERMENT          PIC  +9(7).99. 
           03  WS-QNO-SCHED-PAYMENT      PIC  +9(7).99.
           03  WS-QNO-TDUECONTR          PIC  +9(7).99.
           03  WS-QNO-LDUECONTR          PIC  +9(7).99.
           03  WS-QNO-NXDUECONTR         PIC  +9(7).99.
           03  WS-QNO-CURRENT-AVAIL      PIC  +9(7).99.
           03  WS-QNO-LATE-CHG           PIC  +9(7).99. 
           03  WS-QNO-INDU-PENALTY       PIC  +9(7).99.
           03  WS-NO-TDUE-DATE           PIC X(08).
           03  WS-NO-LDUE-DATE           PIC X(08).
           03  WS-NO-DATE-PAID-LAST      PIC X(08).
           03  WS-NO-NXDUE-DATE          PIC X(08).
           03  WS-NO-PDTH-DATE           PIC X(08).
           03  WS-NO-LCPDTH-DATE         PIC X(08).
           03  WS-NO-TRUE-DUE-DATE       PIC X(08).

      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGE-DATE                PIC 9(8).
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).
           03  INCLUDE-PAIDOUTS        PIC X(1).
           03  GROUP-FLAG              PIC X(1).
           03  ENT-GROUP               PIC X(4).

       01  VERSION-CONTROL             PIC X(1)      VALUE "A".

       01  SV-BEG-BR                   PIC 9999 VALUE 9999.
       01  SV-END-BR                   PIC 9999 VALUE 9999.
       01  CH-BEG-BR                   PIC X(04).
       01  CH-END-BR                   PIC X(04).
       01  SV-ENT-GROUP                PIC X(4)  VALUE SPACES.
       01  SV-GROUP-FLAG               PIC X     VALUE SPACES.

      ******************************************************************
      *
      *    P R I N T - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED                 PIC 99        VALUE 0.
           03  LN-COUNT                PIC 99        VALUE 61.
           03  LN-WK                   PIC 99        VALUE 0.
       01  PAGE-NUMBER                 PIC 9(4)      VALUE 0.

      *=================================================================
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

       01  D01-TOTAL-TOTALS            REDEFINES DETAIL-LINE.
           03  D01-TOTAL-BRNO          PIC 9(4).
           03  FILLER                  PIC X(1).
           03  D01-TOTAL-READ          PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                  PIC X(1).
           03  D01-TOTAL-SKIP          PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                  PIC X(1).
           03  D01-TOTAL-REWRITE       PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                  PIC X(1).
           03  D01-TOTAL-WRITE         PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                  PIC X(80).

       01  RANGE-LN          REDEFINES DETAIL-LINE.
           03  RANGE-LINE              PIC X(31).
           03  RANGE-SELECTION         PIC X(101).

      *=================================================================
       01  HEAD1.
           03  H-DATE                  PIC X(8)      VALUE " ".
           03  FILLER                  PIC XX        VALUE " ".
           03  H-HR                    PIC 99        VALUE 0.
           03  FILLER                  PIC X         VALUE ":".
           03  H-MIN                   PIC 99        VALUE 0.
           03  FILLER                  PIC X(31)     VALUE " ".
           03  H-NAME                  PIC X(40)     VALUE " ".
           03  FILLER                  PIC X(37)     VALUE " ".
           03  FILLER                  PIC X(5)      VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(28)     VALUE " ".
           03  H-TITLE                 PIC X(20)     VALUE
                                                   "NOTICE FILE CREATE".
           03  FILLER                  PIC X(6)      VALUE " FOR: ".
           03  H-AGEDATE               PIC X(10).

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER PIC X(30) VALUE "BRANCH      READ        SKIP  ".
           03  FILLER PIC X(30) VALUE "   REWRITE       WRITE        ".
           03  FILLER PIC X(72) VALUE SPACES.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/UNAMEW.CPY".
       COPY "LIBGB/HEX_CMD.CPY".

       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPCRNOW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CRNOFL_DEF.CPY".
       COPY "LIBSP/CRNOFL_WKS.CPY".
       COPY "LIBWI/GRSCANW.CPY".

      *=================================================================
      *    L I N K A G E - S E C T I O N 
      *=================================================================
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/CRNOFL_SCN.CPY".

      *****************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      *****************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE REPORT-FILE LOG-FILE WKNO-FILE.
       COPY "LIBGB/DECLRP_ASCII.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-REPORT-FILE.
           CLOSE LOG-FILE WKNO-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CRNOFL" TO FORM-PROG VDU-FORM-NAME.

           ACCEPT CURRENT-DATE-YYMMDD FROM DATE.
           MOVE CURRENT-DATE-YYMMDD TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO CURRENT-DATE-YYYY-MM-DD.
           INSPECT CURRENT-DATE-YYYY-MM-DD REPLACING ALL "/" BY "-".

           MOVE "Y"      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACE    TO TIME-MILITARY-FG.
           MOVE HH-MM-SS TO EDF-TIME-HH-MM-SS.
           INSPECT EDF-TIME-HH-MM-SS REPLACING ALL "/" BY ":".

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           PERFORM TCLP-TRACE-MODULE.
           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           MOVE 08 TO WS-TRACE-OPTION.               *> EXT-EOBUF-ERRCD
           PERFORM UPDATE-TRACE-MODULE.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

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

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE 09 TO WS-TRACE-OPTION.                 *> BRANCH CHANGE
           PERFORM UPDATE-TRACE-MODULE.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "PROGRAM IN PROGRESS................", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.   

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              NO-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              GB-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-NO1-FILE.
           PERFORM OPEN-LP1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE WK-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           MOVE BR-NO            TO LN-OWNBR
                                    QLN3-WBEG-OWNBR
                                    QLN3-WEND-OWNBR
                                    QLP1-WBEG-BRNO
           MOVE 0                TO LN-ACCTNO
                                    LN-CLASS
                                    QLN3-WBEG-CLASS
                                    QLN3-WBEG-ACCTNO.
           MOVE ALL "9"          TO QLN3-WEND-ACCTNO
                                    QLN3-WEND-CLASS.
           MOVE INCLUDE-PAIDOUTS TO QWS-INCLUDE-PAIDOUTS.


           MOVE 10 TO WS-TRACE-OPTION.              *> BEFORE START-LN3
           PERFORM UPDATE-TRACE-MODULE.

           PERFORM START-LN3-FILE.  *> SSP - LN,BW,NO

           MOVE 11 TO WS-TRACE-OPTION.              *> AFTER START-LN3
           PERFORM UPDATE-TRACE-MODULE.

      *=================================================================
       NEXT-LN.

           PERFORM READ-LN3-FILE-NEXT. *> SSP - LN,BW

           IF IO-FG = 9   *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           MOVE 12 TO WS-TRACE-OPTION.              *> PER 5000 RECORDS
           PERFORM UPDATE-TRACE-MODULE.

           ADD 1 TO RECS-COUNT-BRNO-READ.

           IF LN-ACCTNO = 0
              ADD 1 TO RECS-COUNT-BRNO-SKIP
              GO TO NEXT-LN.

           IF INCLUDE-PAIDOUTS = "N"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          ADD 1 TO RECS-COUNT-BRNO-SKIP
                          GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      * DO NOT USE A HOLD-SP1-KEY, MUST RE-READ BECAUSE OF LN-LC-CODE
      * IN THE LATE CHARGE ROUTINES, WIPES OUT IMPORTANT SPR FIELDS !
      *    PERFORM READ-SP1-FILE.
           PERFORM ARRAYSP-READ.
           IF IO-FG NOT = 0
              ADD 1 TO RECS-COUNT-BRNO-SKIP
              GO TO NEXT-LN.

      * AVAILABLE CREDIT NEEDS THE BORROWER, INSIDE CREATE-NOREC
      *    MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
      *    IF IO-FG NOT = 0
           IF BW-SSNO = 0
              ADD 1 TO RECS-COUNT-BRNO-SKIP
              GO TO NEXT-LN.

           IF LN-1STPYDATE = 0
              MOVE LN-LOANDATE TO LN-1STPYDATE.
           IF LN-ORIG-1STPYDATE = 0
              MOVE LN-LOANDATE TO LN-ORIG-1STPYDATE.

           MOVE LN-MAKERCD(1)   TO CRNO-MAKERCD.
           MOVE AGE-DATE        TO CRNO-PAYDATE.
           MOVE " "             TO CRNO-LPREC-FG.

      * THIS IS NEEDED FOR DEFERMENT FORMULA 8 WHICH CALLS DEFI01 AND IT
      * PASSES LP-PAYDATE IN DEF-POST-DATE TO CALC ELAPSED PERIODS.
      * WE WERE GETTING NON-NUMERIC MESSAGES IN ACU_TCLP AS IT WAS 20202020

           MOVE AGE-DATE        TO LP-PAYDATE.

      *    MOVE LN-OWNBR  TO NO-BRNO.
      *    MOVE LN-ACCTNO TO NO-ACCTNO.

      * CREATE-NOREC HAS A CLEAR-NO-FILE 

      *    PERFORM READ-NO1-FILE.
      *    IF IO-FG = 0
      *     IF NO-ACCTNO > 0 

           PERFORM CREATE-NOREC.
           MOVE SPACES TO WKNO-REC.
           PERFORM SET-NO-FIELDS.

           MOVE QNO-TDUELC              TO WS-QNO-TDUELC.      
           INSPECT WS-QNO-TDUELC        REPLACING LEADING "+" BY " ".   
           MOVE QNO-TDUEAMT             TO WS-QNO-TDUEAMT. 
           INSPECT WS-QNO-TDUEAMT       REPLACING LEADING "+" BY " ".      
           MOVE QNO-TDUEPRIN            TO WS-QNO-TDUEPRIN.     
           INSPECT WS-QNO-TDUEPRIN      REPLACING LEADING "+" BY " ".   
           MOVE QNO-TDUETBAL            TO WS-QNO-TDUETBAL.     
           INSPECT WS-QNO-TDUETBAL      REPLACING LEADING "+" BY " ".   
           MOVE QNO-TDUEPOFF            TO WS-QNO-TDUEPOFF.     
           INSPECT WS-QNO-TDUEPOFF      REPLACING LEADING "+" BY " ". 
           MOVE QNO-LDUELC              TO WS-QNO-LDUELC.       
           INSPECT WS-QNO-LDUELC        REPLACING LEADING "+" BY " ". 
           MOVE QNO-LDUEAMT             TO WS-QNO-LDUEAMT.      
           INSPECT WS-QNO-LDUEAMT       REPLACING LEADING "+" BY " ". 
           MOVE QNO-AMT-PAID-LAST       TO WS-QNO-AMT-PAID-LAST.
           INSPECT WS-QNO-AMT-PAID-LAST REPLACING LEADING "+" BY " ".
           MOVE QNO-NXDUEAMT            TO WS-QNO-NXDUEAMT. 
           INSPECT WS-QNO-NXDUEAMT      REPLACING LEADING "+" BY " ".  
           MOVE QNO-NXDUELC             TO WS-QNO-NXDUELC.
           INSPECT WS-QNO-NXDUELC       REPLACING LEADING "+" BY " ".   
           MOVE QNO-LCHG-INTDUE         TO WS-QNO-LCHG-INTDUE.  
           INSPECT WS-QNO-LCHG-INTDUE   REPLACING LEADING "+" BY " ".   
           MOVE QNO-AVAILCREDIT         TO WS-QNO-AVAILCREDIT. 
           INSPECT WS-QNO-AVAILCREDIT   REPLACING LEADING "+" BY " ".   
           MOVE QNO-TDUEDELINQ          TO WS-QNO-TDUEDELINQ.   
           INSPECT WS-QNO-TDUEDELINQ    REPLACING LEADING "+" BY " ".   
           MOVE QNO-DEFERMENT           TO WS-QNO-DEFERMENT.    
           INSPECT WS-QNO-DEFERMENT     REPLACING LEADING "+" BY " ". 
           MOVE QNO-SCHED-PAYMENT       TO WS-QNO-SCHED-PAYMENT.
           INSPECT WS-QNO-SCHED-PAYMENT REPLACING LEADING "+" BY " ".
           MOVE QNO-TDUECONTR           TO WS-QNO-TDUECONTR.    
           INSPECT WS-QNO-TDUECONTR     REPLACING LEADING "+" BY " ".
           MOVE QNO-LDUECONTR           TO WS-QNO-LDUECONTR.    
           INSPECT WS-QNO-LDUECONTR     REPLACING LEADING "+" BY " ".
           MOVE QNO-NXDUECONTR          TO WS-QNO-NXDUECONTR.   
           INSPECT WS-QNO-NXDUECONTR    REPLACING LEADING "+" BY " ".
           MOVE QNO-CURRENT-AVAIL       TO WS-QNO-CURRENT-AVAIL.
           INSPECT WS-QNO-CURRENT-AVAIL REPLACING LEADING "+" BY " ".
           MOVE QNO-LATE-CHG            TO WS-QNO-LATE-CHG.     
           INSPECT WS-QNO-LATE-CHG      REPLACING LEADING "+" BY " ".
           MOVE QNO-INDU-PENALTY        TO WS-QNO-INDU-PENALTY.
           INSPECT WS-QNO-INDU-PENALTY  REPLACING LEADING "+" BY " ".

           IF NO-TDUE-DATE               = 0
              MOVE 19000101             TO NO-TDUE-DATE.
           MOVE NO-TDUE-DATE            TO WS-NO-TDUE-DATE.

           IF NO-LDUE-DATE               = 0
              MOVE 19000101             TO NO-LDUE-DATE.
           MOVE NO-LDUE-DATE            TO WS-NO-LDUE-DATE.

           IF NO-DATE-PAID-LAST          = 0
              MOVE 19000101             TO NO-DATE-PAID-LAST.
           MOVE NO-DATE-PAID-LAST       TO WS-NO-DATE-PAID-LAST.

           IF NO-NXDUE-DATE              = 0
              MOVE 19000101             TO NO-NXDUE-DATE.
           MOVE NO-NXDUE-DATE           TO WS-NO-NXDUE-DATE.

           IF NO-PDTH-DATE               = 0
              MOVE 19000101             TO NO-PDTH-DATE.
           MOVE NO-PDTH-DATE            TO WS-NO-PDTH-DATE.

           IF NO-LCPDTH-DATE             = 0
              MOVE 19000101             TO NO-LCPDTH-DATE.
           MOVE NO-LCPDTH-DATE          TO WS-NO-LCPDTH-DATE.

           IF NO-TRUE-DUE-DATE             = 0
              MOVE 19000101             TO NO-TRUE-DUE-DATE.
           MOVE NO-TRUE-DUE-DATE        TO WS-NO-TRUE-DUE-DATE.

           STRING WKNO-KEY,PIPE-1,
                  QNO-BRNO,PIPE-1,
                  QNO-ACCTNO,PIPE-1,
                  WS-NO-TDUE-DATE,PIPE-1,
                  WS-QNO-TDUELC,PIPE-1,
                  WS-QNO-TDUEAMT,PIPE-1,
                  WS-QNO-TDUEPRIN,PIPE-1,
                  WS-QNO-TDUETBAL,PIPE-1,
                  WS-QNO-TDUEPOFF,PIPE-1,
                  WS-NO-LDUE-DATE,PIPE-1,
                  WS-QNO-LDUELC,PIPE-1,
                  WS-QNO-LDUEAMT,PIPE-1,
                  WS-NO-DATE-PAID-LAST,PIPE-1,
                  WS-QNO-AMT-PAID-LAST,PIPE-1,
                  QNO-LSTLPKEY,PIPE-1,
                  WS-NO-NXDUE-DATE,PIPE-1,
                  WS-QNO-NXDUEAMT,PIPE-1,
                  WS-QNO-NXDUELC,PIPE-1,
                  QNO-TRUCONTR,PIPE-1,
                  QNO-RECENCY,PIPE-1,
                  WS-NO-PDTH-DATE,PIPE-1,
                  WS-NO-LCPDTH-DATE,PIPE-1,
                  WS-QNO-LCHG-INTDUE,PIPE-1,
                  WS-QNO-AVAILCREDIT,PIPE-1,
                  WS-QNO-TDUEDELINQ,PIPE-1,
                  WS-QNO-DEFERMENT,PIPE-1,
                  WS-NO-TRUE-DUE-DATE,PIPE-1,
                  WS-QNO-SCHED-PAYMENT,PIPE-1,
                  WS-QNO-TDUECONTR,PIPE-1,
                  WS-QNO-LDUECONTR,PIPE-1,
                  WS-QNO-NXDUECONTR,PIPE-1,
                  WS-QNO-CURRENT-AVAIL,PIPE-1,
                  WS-QNO-LATE-CHG,PIPE-1,
                  QNO-PAST-DUE-DAYS,PIPE-1,
                  QNO-RECDEL,PIPE-1,
                  QNO-AVCR-RESTRICT-FG,PIPE-1,
                  WS-QNO-INDU-PENALTY,PIPE-1,
                  QNO-CPOT30,PIPE-1,
                  QNO-RDPOT30,PIPE-1,
                  QNO-CPOTCHGOFF,PIPE-1,
                  QNO-RDPOTCHGOFF,PIPE-1,
                  QNO-CSUB,PIPE-1,
                  QNO-RSUB 
           DELIMITED BY "  " INTO WKNO-REC. 

           IF INCLUDE-PAIDOUTS = "Y"
              IF UPDATE-CTR = 200000
                 PERFORM CLOSE-WKNO-FILE 
                 PERFORM CALL-BCP-SCRIPT
                 IF SCRIPT-CALL-ERROR-SW = "Y"
                    GO TO END-OF-GROUP
                 ELSE 
                    PERFORM OPEN-WKNO-FILE-OUTPUT.

           PERFORM WRITE-WKNO-FILE. 
           ADD 1 TO RECS-COUNT-BRNO-REWRITE 
                    TOTAL-ACCOUNTS-PROC-CTR
                    UPDATE-CTR.

           GO TO NEXT-LN.

      *----------------------------------------------------------------
       END-OF-GROUP.

           PERFORM CLOSE-LN3-FILE.

           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO END-ROUTINE.

           PERFORM UPDATE-LOG.
           PERFORM UPDATE-REPORT-FILE.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           PERFORM CLOSE-WKNO-FILE.

           IF SCRIPT-CALL-ERROR-SW = "Y"
              GO TO END-PROGRAM.

           IF UPDATE-CTR > 0
              PERFORM CALL-BCP-SCRIPT
              IF SCRIPT-CALL-ERROR-SW = "Y"
                 GO TO END-PROGRAM.

           PERFORM PRINT-REPORT-FILE.
           PERFORM PRINT-RANGE-LINES.

           IF ( EXT-ROUTE-BUF = "00" ) OR
              ( EXT-ROUTE-BUF = "70" )
              PERFORM FINAL-PASS-LOG-MSG.

           MOVE WKNO-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           GO TO END-PROGRAM.

      ******************************************************************
      *
      *   I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE 01 TO WS-TRACE-OPTION.                    *> INITIALIZE
           PERFORM UPDATE-TRACE-MODULE.

           MOVE CRNOFL-QUEUE-POS TO Q-POS.
           MOVE CRNOFL-COPIES-POS TO C-POS.
           MOVE CRNOFL-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           CALL "C$TOLOWER"  USING ECHO-CMD, VALUE 8.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE AGE-DATE           TO H-AGEDATE.

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

           IF EXT-ROUTE-BUF = "00"
              STRING "PROGRAM IN PROGRESS................", "...."
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.   

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 02 TO WS-TRACE-OPTION.              *> BEFORE OPEN PR
           PERFORM UPDATE-TRACE-MODULE.

           PERFORM OPEN-PR-FILE.

           MOVE 03 TO WS-TRACE-OPTION.              *> AFTER OPEN PR
           PERFORM UPDATE-TRACE-MODULE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 04 TO WS-TRACE-OPTION.              *> BEFORE SQL CONN
           PERFORM UPDATE-TRACE-MODULE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION. 

           MOVE 05 TO WS-TRACE-OPTION.              *> AFTER  SQL CONN
           PERFORM UPDATE-TRACE-MODULE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 06 TO WS-TRACE-OPTION.              *> BEFORE MKTEMP
           PERFORM UPDATE-TRACE-MODULE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.

           MOVE 07 TO WS-TRACE-OPTION.              *> AFTER  MKTEMP
           PERFORM UPDATE-TRACE-MODULE.

      D    STOP MESS.

           ACCEPT DATE-YYYYMMDD       FROM CENTURY-DATE.
      * CALL MKTEMP TO GET RANDOM PROCESS-ID - DELETE FILE - NOT NEEDED
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT = "00"
              MOVE MKTEMP-BUF TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE
           ELSE
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE EXT-FILPATH-BASE        TO WK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE     TO WK-PATH-MACHINE.
           CALL "C$TOLOWER" USING WK-PATH-PGM-TBL VALUE 10.

           MOVE PROCESS-ID-BUF          TO WK-PATH-PROC-ID
                                           WKNO-KEY-PROC-ID.

           INSPECT WKNO-KEY-PROC-ID REPLACING ALL " " BY "X". 

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP  TO WKNO-PATH-BEGBRNO
                                 WKNO-KEY-BEGBRNO
           ELSE
              MOVE BEG-BRANCH  TO WKNO-PATH-BEGBRNO
                                  WKNO-KEY-BEGBRNO.

           MOVE DATE-YYYYMMDD   TO WKNO-KEY-YMD. 
           MOVE WKNO-KEY        TO WS-WKNO-KEY.

           INSPECT WKNO-PATH REPLACING ALL " " BY "X".
           CALL "C$TOLOWER" USING WKNO-PATH-DAT VALUE 4.
           MOVE WKNO-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT  = "00"
              MOVE "WKNO FILE ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WKNO-FILE-OUTPUT.
           MOVE 0 TO UPDATE-CTR. 

           MOVE WKNO-PATH              TO WKNO-PATH-RESULT-FILE.
           MOVE ".RES"                 TO WKNO-PATH-RESULT-RES.
           CALL "C$TOLOWER" USING WKNO-PATH-RESULT-RES VALUE 4.
           CALL "C$TOLOWER" USING TASC-EC VALUE 3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *    EO-EXEC-AUDIT SECTION.
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
      *
      *    ENTER-PARAMETERS SECTION.
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO BR-01.

       AGEDT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD AGE-DATE 
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO AGEDT.
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

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       ENTER-PAIDOUTS.
           MOVE "E  A010PAID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           MOVE VDUBUF TO INCLUDE-PAIDOUTS.
           IF INCLUDE-PAIDOUTS NOT = "Y" AND NOT = "N"
              GO TO ENTER-PAIDOUTS.

       EXIT-PARM.
           EXIT.

      ******************************************************************
      *
      *     DISPLAY-PARAMETERS SECTION.
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.

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

           MOVE "S  A010PAID." TO VDU.
           MOVE INCLUDE-PAIDOUTS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       UPDATE-LOG SECTION.
      ******************************************************************

           MOVE EXT-FILPATH-BASE    TO LOG-BASE.
           MOVE EXT-FILPATH-MACHINE TO LOG-MACHINE.
           MOVE AGE-DATE            TO LOG-DATE.
           MOVE LOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-LOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-LOG-FILE-OUTPUT.

           MOVE BR-NO                   TO LOG-BRNO.
           MOVE STARTING-TIME           TO LOG-TIME.
           MOVE RECS-COUNT-BRNO-READ    TO LOG-READ.
           MOVE RECS-COUNT-BRNO-REWRITE TO LOG-REWRITE.
           MOVE RECS-COUNT-BRNO-WRITE   TO LOG-WRITE.

           IF RECS-COUNT-BRNO-READ = 0 OR
              (RECS-COUNT-BRNO-REWRITE = 0 AND 
                RECS-COUNT-BRNO-WRITE = 0)
              MOVE "ZERO"               TO LOG-STATUS
           ELSE
              MOVE "PASS"               TO LOG-STATUS.

           PERFORM WRITE-LOG-FILE.
           PERFORM CLOSE-LOG-FILE.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - F I L E
      *
      ******************************************************************
       UPDATE-REPORT-FILE SECTION.

           IF ( RECS-COUNT-BRNO-READ = ZEROES )
              GO TO UPDATE-REPORT-FILE-EXIT.

           MOVE BR-NO TO REPORT-BRNO  HOLD-REPORT-BRNO.
           PERFORM READ-REPORT-FILE.

           IF ( IO-FG NOT = ZEROES )
              PERFORM UPDATE-REPORT-WRITE.

           PERFORM UPDATE-REPORT-REWRITE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 9999 TO REPORT-BRNO  HOLD-REPORT-BRNO.
           PERFORM READ-REPORT-FILE.

           IF ( IO-FG NOT = ZEROES )
              PERFORM UPDATE-REPORT-WRITE.

           PERFORM UPDATE-REPORT-REWRITE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO RECS-COUNT-BRNO-READ
                          RECS-COUNT-BRNO-SKIP
                          RECS-COUNT-BRNO-REWRITE
                          RECS-COUNT-BRNO-WRITE.

      *=================================================================
       UPDATE-REPORT-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E W R I T E
      *
      ******************************************************************
       UPDATE-REPORT-REWRITE SECTION.

           ADD RECS-COUNT-BRNO-READ       TO REPORT-RECS-READ.
           ADD RECS-COUNT-BRNO-REWRITE    TO REPORT-RECS-REWRITE.
           ADD RECS-COUNT-BRNO-SKIP       TO REPORT-RECS-SKIP.
           ADD RECS-COUNT-BRNO-WRITE      TO REPORT-RECS-WRITE.

           PERFORM REWRITE-REPORT-FILE.

           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

      *=================================================================
       UPDATE-REPORT-REWRITE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - W R I T E
      *
      ******************************************************************
       UPDATE-REPORT-WRITE SECTION.

           MOVE SPACES           TO REPORT-REC.
           MOVE HOLD-REPORT-BRNO TO REPORT-BRNO.
           MOVE ZEROES           TO REPORT-RECS-READ
                                    REPORT-RECS-REWRITE
                                    REPORT-RECS-SKIP
                                    REPORT-RECS-WRITE.

           PERFORM WRITE-REPORT-FILE.

           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

      *=================================================================
       UPDATE-REPORT-WRITE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - F I L E
      *
      ******************************************************************
       PRINT-REPORT-FILE SECTION.

           PERFORM CLOSE-REPORT-FILE.

           MOVE ZEROES TO REPORT-BRNO.

           PERFORM OPEN-REPORT-FILE.
           PERFORM START-REPORT-FILE.

      *=================================================================
       PRINT-REPORT-NEXT.

           PERFORM READ-REPORT-FILE-NEXT.

           IF ( IO-FG NOT = ZEROES )
              GO TO PRINT-REPORT-FILE-END.

           MOVE REPORT-BRNO                TO D01-TOTAL-BRNO.
           MOVE REPORT-RECS-READ           TO D01-TOTAL-READ.
           MOVE REPORT-RECS-SKIP           TO D01-TOTAL-SKIP.
           MOVE REPORT-RECS-REWRITE        TO D01-TOTAL-REWRITE.
           MOVE REPORT-RECS-WRITE          TO D01-TOTAL-WRITE.

           IF ( REPORT-BRNO = 9999 )
              MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-NEXT.

      *=================================================================
       PRINT-REPORT-FILE-END.

           PERFORM CLOSE-REPORT-FILE.

      *-----------------------------------------------------------------
       PRINT-REPORT-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R A N G E - L I N E S
      *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.

           MOVE 4 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :"  TO RANGE-LINE
              MOVE ENT-GROUP               TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :"  TO RANGE-LINE
              MOVE SPACES                  TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME      :"    TO RANGE-LINE.
           MOVE STARTING-TIME             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME      :"    TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                 TO ENDING-TIME..
           MOVE ENDING-TIME               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM WRITE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - H E A D E R
      *
      ******************************************************************
       WRITE-HEADER SECTION.

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "="  TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *=================================================================
       WRITE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T C L P - T R A C E - M O D U L E 
      *
      *=================================================================
      * IN  : FORM-REL
      * OUT : EXT-TCLP-TRACE-WORKERS
      * DESC: TCLP/OTIS TRACE LOGGING.  CHECK IF `TCLP_TRACE`
      *       ENVIRONMENT VARIABLE IS "Y" (ENABLE TRACE WITHOUT BRANCH)
      *       OR IS "B" (ENABLE TRACE WITH READ OF BRANCH FILE; BRFILE).
      *       IF TRACE IS ENABLE THEN SEND TO LOG THE LAUNCH OF OTIS.
      ******************************************************************
       TCLP-TRACE-MODULE SECTION.

           MOVE ZEROES TO EXT-TCLP-TRACE-LEVEL.
           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

      * SKIP EVENT

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN. 

           MOVE "TCLP_TRACE"  TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-BAD )
      * DISABLE TRACE
              MOVE "N" TO EXT-TCLP-TRACE
              GO TO TCLP-TRACE-MODULE-EXIT.

           MOVE GETENV-BUF TO EXT-TCLP-TRACE.

           IF ( NOT EXT-TCLP-TRACE-VALID   ) OR
              (     EXT-TCLP-TRACE-DISABLE )
      * DISABLE TRACE
              MOVE "N" TO EXT-TCLP-TRACE 
              GO TO TCLP-TRACE-MODULE-EXIT.

           MOVE "ROOT" TO MESS.
           CALL "C$TOLOWER" USING MESS, VALUE 10.
      * GET USERID
           PERFORM UNAME-CALL.

      *KC * DISABLE FOR ROOT
      *KC      IF ( UNAME-BUF = MESS )
      *KC * DISABLE TRACE
      *KC         MOVE "N" TO EXT-TCLP-TRACE
      *KC         GO TO TCLP-TRACE-MODULE-EXIT.

           MOVE ALL "="                    TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           STRING "OTIS SESSION STARTED /", FORM-REL
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

      *=================================================================
       TCLP-TRACE-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - T R A C E - M O D U L E
      *
      *=================================================================
      * IN  : WS-TRACE-OPTION
      * OUT : NOTHING
      * DESC: UPDATE THE TRACE LOG BASED ON THE NEED OPTION.
      ******************************************************************
       UPDATE-TRACE-MODULE SECTION.

           IF ( EXT-TCLP-TRACE = "B" )
              GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-01.

           IF ( WS-TRACE-OPTION NOT = 01 )
              GO TO UPDATE-TRACE-OPTION-02.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.

           STRING "EXECUTION SYSTEM INFO | /", FORM-REL,
                  " | ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "DATABASE NAME = ", EXT-ACUSQL-DB-NAME
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "SQL SERVER    = ", EXT-ACUSQL-SERVER-NAME
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "BATCH CODE    = ", EXT-EOBUF-PCODE
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "EXT-ROUTE-BUF = ", EXT-ROUTE-BUF
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           MOVE EXT-EOBUF-DATE1            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING "BEG DATE      = ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           MOVE EXT-EOBUF-DATE2            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING "END DATE      = ", DATE-MMSDDSYY
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "BEG BRANCH    = ", BEG-BRANCH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "END BRANCH    = ", END-BRANCH
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "GROUP FLAG    = ", GROUP-FLAG
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           STRING "ENT GROUP     = ", ENT-GROUP
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-02.

           IF ( WS-TRACE-OPTION NOT = 02 )
              GO TO UPDATE-TRACE-OPTION-03.

           MOVE "BEFORE OPEN-PR-FILE"      TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-03.

           IF ( WS-TRACE-OPTION NOT = 03 )
              GO TO UPDATE-TRACE-OPTION-04.

           MOVE "AFTER  OPEN-PR-FILE"      TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-04.

           IF ( WS-TRACE-OPTION NOT = 04 )
              GO TO UPDATE-TRACE-OPTION-05.

           MOVE "BEFORE SQL-OPEN-UPDATE-CONNECTION"
                                           TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           STRING "SQL-UPDATE-CONN-OPEN-SW", SQL-UPDATE-CONN-OPEN-SW
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-05.

           IF ( WS-TRACE-OPTION NOT = 05 )
              GO TO UPDATE-TRACE-OPTION-06.

           MOVE "AFTER  SQL-OPEN-UPDATE-CONNECTION"
                                           TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-06.

           IF ( WS-TRACE-OPTION NOT = 06 )
              GO TO UPDATE-TRACE-OPTION-07.

           MOVE "BEFORE MKTEMP"            TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-07.

           IF ( WS-TRACE-OPTION NOT = 07 )
              GO TO UPDATE-TRACE-OPTION-08.

           STRING "AFTER  MKTEMP: ", MKTEMP-BUF
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-08.

           IF ( WS-TRACE-OPTION NOT = 08 )
              GO TO UPDATE-TRACE-OPTION-09.

           STRING "EXT-EOBUF-ERRCD = ", EXT-EOBUF-ERRCD
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CRNOFL"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-SEND-MESS.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-09.

           IF ( WS-TRACE-OPTION NOT = 09 )
              GO TO UPDATE-TRACE-OPTION-10.

           MOVE ZEROES TO WS-TRACE-REC-COUNT WS-TRACE-REC-TOTAL.
           STRING "PROCESSING BRANCH: ", BR-NO
                  DELIMITED BY "  "      INTO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-10.

           IF ( WS-TRACE-OPTION NOT = 10 )
              GO TO UPDATE-TRACE-OPTION-11.

           MOVE "BEFORE START-LN3-FILE"    TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-11.

           IF ( WS-TRACE-OPTION NOT = 11 )
              GO TO UPDATE-TRACE-OPTION-12.

           MOVE "AFTER  START-LN3-FILE"    TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-12.

           IF ( WS-TRACE-OPTION NOT = 12 )
              GO TO UPDATE-TRACE-OPTION-13.

           ADD 01 TO WS-TRACE-REC-COUNT   WS-TRACE-REC-TOTAL.

           IF ( WS-TRACE-REC-COUNT > 4999 )
              MOVE ZEROES                  TO WS-TRACE-REC-COUNT
              MOVE WS-TRACE-REC-TOTAL      TO WS-TRACE-REC-SHOW
              STRING " --- RECORD COUNT = ", WS-TRACE-REC-SHOW
                  DELIMITED BY SIZE      INTO EXT-TCLP-TRACE-MESS
              MOVE FORM-PATHNAME           TO EXT-TCLP-TRACE-FORM-PATH
              PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-13.

           IF ( WS-TRACE-OPTION NOT = 13 )
              GO TO UPDATE-TRACE-OPTION-14.

           MOVE "BEFORE REWRITE-NO1-FILE" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME             TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-14.

           IF ( WS-TRACE-OPTION NOT = 14 )
              GO TO UPDATE-TRACE-OPTION-15.

           MOVE "AFTER  REWRITE-NO1-FILE" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME             TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-15.

           IF ( WS-TRACE-OPTION NOT = 15 )
              GO TO UPDATE-TRACE-OPTION-16.

           MOVE "BEFORE WRITE-NO1-FILE" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME           TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-16.

           IF ( WS-TRACE-OPTION NOT = 16 )
              GO TO UPDATE-TRACE-OPTION-99.

           MOVE "AFTER  WRITE-NO1-FILE" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME           TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

           GO TO UPDATE-TRACE-MODULE-EXIT.

      *-----------------------------------------------------------------
       UPDATE-TRACE-OPTION-99.

           MOVE "UNKNOWN ERROR" TO EXT-TCLP-TRACE-MESS.
           MOVE FORM-PATHNAME   TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM TRACE-SEND-MESS.

      *=================================================================
       UPDATE-TRACE-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBCL/TRACE.CPY".

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/DEFPOL.CPY".

       COPY "LIBLP/LPCRNO.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".

       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/GBFRSQLLOG.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CRNOFL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CRNOFL-SYSDATE.
           DISPLAY CRNOFL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CRNOFL-FORM-FIELDS TO WR-BUF.
           MOVE CRNOFL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CRNOFL-HELP--FILE TO HELP-LINK-FILE.
           MOVE CRNOFL-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CRNOFL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       CALL-BCP-SCRIPT SECTION.

      D    STOP MESS.

           MOVE SPACE  TO SCRIPT-TYPE.
           PERFORM CLEAR-NOFILE-WORK-TABLE.

      * RUN BCP TO LOAD WORK TABLE
           MOVE "BCP"  TO SCRIPT-TYPE.
           MOVE SPACES TO TCLP-CRNOFL-SHELL-PARMS.
           STRING EXT-ACUSQL-DSN-NAME, " ",
                  EXT-ACUSQL-DB-NAME, " ",
                  EXT-ACUSQL-LOGIN, " ",
                  EXT-ACUSQL-PSWD, " ",
                  "DBO.CRNOFL_NOFILE", " ", 
                  WKNO-PATH, " ", 
                  WKNO-PATH-RESULT-FILE
             DELIMITED BY "  " INTO TCLP-CRNOFL-SHELL-PARMS.
           MOVE "TCLP_BCP.SH "  TO TASC-SCRIPT.
           CALL "C$TOLOWER" USING TASC-SCRIPT VALUE 20.
           MOVE TCLP-CRNOFL-SHELL-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF (STAT-BAD)
              STRING "ERROR CALLING SCRIPT ",TASC-SCRIPT 
               INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

           MOVE "RES"                 TO SCRIPT-TYPE.
           MOVE WKNO-PATH-RESULT-FILE TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              STRING TASC-SCRIPT, " RESULT FILE NOT FOUND" 
               INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

      * RETRIEVE RESULT FILE COMPLETION MESSAGE
      * IF MESSAGE NOT FOUND, ASSUME ERROR IN BCP
           MOVE "BCP"  TO SCRIPT-TYPE.
           PERFORM RETR-RESULT-MSG.
           IF FOUND-SW = "N"
              STRING "ERROR CALLING SCRIPT ",TASC-SCRIPT 
               INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

           MOVE WKNO-PATH-RESULT-FILE TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.


      * RUN SQLCMD TO EXECUTE REWRITE QUERY
           MOVE "CMD"            TO SCRIPT-TYPE.
           MOVE SPACES           TO TCLP-CRNOFL-SHELL-PARMS.
           MOVE "CRNOFL_REWRITE" TO SCRIPT-MISC. 
           STRING EXT-ACUSQL-DSN-NAME, " ",
                  EXT-ACUSQL-DB-NAME, " ",
                  EXT-ACUSQL-LOGIN, " ",
                  EXT-ACUSQL-PSWD, " ",
                  WKNO-KEY, " ", 
                  SCRIPT-MISC, " ", 
                  WKNO-PATH-RESULT-FILE, " ",
                  WKNO-KEY-PROC-ID
             DELIMITED BY "  " INTO TCLP-CRNOFL-SHELL-PARMS.
           MOVE "CRNOFL_SQLCMD.SH " TO TASC-SCRIPT.
           CALL "C$TOLOWER" USING TASC-SCRIPT VALUE 20.
           MOVE TCLP-CRNOFL-SHELL-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF (STAT-BAD)
              STRING "ERROR CALLING SCRIPT ", TASC-SCRIPT,
                     " USING ", SCRIPT-MISC INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

           MOVE "RES"                 TO SCRIPT-TYPE.
           MOVE WKNO-PATH-RESULT-FILE TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              STRING TASC-SCRIPT, " RESULT FILE NOT FOUND" 
               INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

      * RETRIEVE RESULT FILE COMPLETION MESSAGE
      * IF NOT FOUND, ASSUME ERROR IN SQL
           MOVE "CMD"  TO SCRIPT-TYPE.
           PERFORM RETR-RESULT-MSG.
           IF FOUND-SW = "N"
              MOVE "CMD"  TO SCRIPT-TYPE
              STRING "ERROR CALLING SCRIPT ", TASC-SCRIPT,
                     " USING ", SCRIPT-MISC INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

           MOVE WKNO-PATH-RESULT-FILE TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.


      * RUN SQLCMD TO EXECUTE WRITE QUERY
           MOVE "CMD"            TO SCRIPT-TYPE.
           MOVE SPACES           TO TCLP-CRNOFL-SHELL-PARMS.
           MOVE "CRNOFL_WRITE"   TO SCRIPT-MISC. 
           STRING EXT-ACUSQL-DSN-NAME, " ",
                  EXT-ACUSQL-DB-NAME, " ",
                  EXT-ACUSQL-LOGIN, " ",
                  EXT-ACUSQL-PSWD, " ",
                  WKNO-KEY, " ", 
                  SCRIPT-MISC, " ", 
                  WKNO-PATH-RESULT-FILE, " ",
                  WKNO-KEY-PROC-ID
             DELIMITED BY "  " INTO TCLP-CRNOFL-SHELL-PARMS.
           MOVE "CRNOFL_SQLCMD.SH " TO TASC-SCRIPT.
           CALL "C$TOLOWER" USING TASC-SCRIPT VALUE 20.
           MOVE TCLP-CRNOFL-SHELL-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF (STAT-BAD)
              STRING "ERROR CALLING SCRIPT ", TASC-SCRIPT,
                     " USING ", SCRIPT-MISC INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

           MOVE "RES"                 TO SCRIPT-TYPE.
           MOVE WKNO-PATH-RESULT-FILE TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              STRING SCRIPT-MISC, " RESULT FILE NOT FOUND" 
               INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

      * RETRIEVE RESULT FILE COMPLETION MESSAGE
      * IF NOT FOUND, ASSUME ERROR IN SQL
           MOVE "CMD"  TO SCRIPT-TYPE.
           PERFORM RETR-RESULT-MSG.
           IF FOUND-SW = "N"
              MOVE "CMD"  TO SCRIPT-TYPE
              STRING "ERROR CALLING SCRIPT ", TASC-SCRIPT,
                     " USING ", SCRIPT-MISC INTO SCRIPT-ERROR-MSG
              MOVE "Y" TO SCRIPT-CALL-ERROR-SW
              PERFORM SCRIPT-ERROR-HANDLER
              GO TO CALL-BCP-SCRIPT-EXIT.

           PERFORM SCRIPT-CLEANUP.

           MOVE 0 TO UPDATE-CTR.

       CALL-BCP-SCRIPT-EXIT.
           EXIT.

      ******************************************************************
      *     M I S C E L L E N O U S    P A R A G R A P H S
      ******************************************************************
       MISC-PARA SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/UNAME.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       RETR-RESULT-MSG.
           MOVE "N" TO FOUND-SW.
           PERFORM OPEN-WKNO-RESULT-FILE.
           PERFORM READ-WKNO-RESULT-FILE-NEXT.
           PERFORM UNTIL IO-BAD
              CALL "C$TOUPPER"  USING WKNO-RESULT-REC, VALUE 200
              IF SCRIPT-TYPE = "BCP"
                 PERFORM VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 100
                    IF WKNO-RESULT-REC(SUB:6) = "COPIED"
                       MOVE "Y" TO FOUND-SW 
                       EXIT PERFORM
                    END-IF 
                 END-PERFORM
              ELSE
                 PERFORM VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 100
                    IF WKNO-RESULT-REC(SUB:8) = "AFFECTED"
                       MOVE "Y" TO FOUND-SW 
                       EXIT PERFORM
                    END-IF 
                 END-PERFORM
              END-IF 
              IF FOUND-SW = "Y"
                 EXIT PERFORM
              END-IF
              PERFORM READ-WKNO-RESULT-FILE-NEXT
           END-PERFORM.
           PERFORM CLOSE-WKNO-RESULT-FILE.

       SCRIPT-ERROR-HANDLER.

           IF SCRIPT-TYPE NOT = "RES"
              PERFORM FIND-SCRIPT-ERROR-DETAIL-MSG.

           MOVE SPACES TO MESS.
           IF ( GROUP-FLAG = "Y" )
              STRING EXT-EOBUF-PCODE, " ",
                     ENT-GROUP,       " ",
                     BR-NO
                     DELIMITED BY "  " INTO MESS
           ELSE
              STRING EXT-EOBUF-PCODE, " ",
                     BEG-BRANCH,      "~",
                     END-BRANCH,      " ",
                     BR-NO
                     DELIMITED BY "  " INTO MESS.

           STRING SCRIPT-ERROR-MSG, " | ",
                  SCRIPT-ERROR-DETAIL-MSG
                  DELIMITED BY "  " INTO MISC-MSG.

           MOVE FORM-PATHNAME  TO SQLLOG-PROGRAM.
           MOVE MESS           TO SQLLOG-RANGES.
           MOVE "FAIL"         TO SQLLOG-STATUS.
           MOVE MISC-MSG       TO SQLLOG-MESS.
           PERFORM UPDATE-SQLLOG-FILE.

       SCRIPT-CLEANUP.
      * DELETE WORK NOFILE UPDATE FILE AND SCRIPT RESULT FILE, CLEAR WORK TABLE
           MOVE WKNO-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE WKNO-PATH-RESULT-FILE TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           PERFORM CLEAR-NOFILE-WORK-TABLE.


       FIND-SCRIPT-ERROR-DETAIL-MSG.

           PERFORM OPEN-WKNO-RESULT-FILE.

           PERFORM READ-WKNO-RESULT-FILE-NEXT.
           PERFORM UNTIL IO-BAD
              CALL "C$TOUPPER"  USING WKNO-RESULT-REC, VALUE 200
              IF WKNO-RESULT-REC(1:6) = "ERROR"
                 MOVE 0 TO LAST-BRACKET-POS
                 PERFORM VARYING SUB FROM 1 BY 1 
                  UNTIL SUB > 200
                    IF WKNO-RESULT-REC(SUB:1) = "]"
                       MOVE SUB TO LAST-BRACKET-POS
                    END-IF 
                 END-PERFORM
                 EXIT PERFORM 
              ELSE
                 IF WKNO-RESULT-REC(1:7) = "INVALID"
                    MOVE WKNO-RESULT-REC TO SCRIPT-ERROR-DETAIL-MSG
                    MOVE 0               TO LAST-BRACKET-POS
                    EXIT PERFORM 
                 ELSE
                    PERFORM READ-WKNO-RESULT-FILE-NEXT
                 END-IF
              END-IF 
           END-PERFORM.

           PERFORM CLOSE-WKNO-RESULT-FILE.

           IF LAST-BRACKET-POS > 0 AND LAST-BRACKET-POS < 199
               ADD 1 TO LAST-BRACKET-POS
               COMPUTE SUB = 200 - LAST-BRACKET-POS
               MOVE WKNO-RESULT-REC(LAST-BRACKET-POS:SUB)
                  TO SCRIPT-ERROR-DETAIL-MSG
           END-IF.

       FINAL-PASS-LOG-MSG.

           MOVE SPACES TO SYSTEM-BUF.
           MOVE "Y"      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACE    TO TIME-MILITARY-FG.
           MOVE HH-MM-SS TO EDF-TIME-HH-MM-SS.
           INSPECT EDF-TIME-HH-MM-SS REPLACING ALL "/" BY ":".

           MOVE SPACES TO MESS.
           IF ( GROUP-FLAG = "Y" )
              STRING EXT-EOBUF-PCODE, " ",
                     ENT-GROUP,       " ",
                     BR-NO
                     DELIMITED BY "  " INTO MESS
           ELSE
              STRING EXT-EOBUF-PCODE, " ",
                     BEG-BRANCH,      "~",
                     END-BRANCH,      " ",
                     BR-NO
                     DELIMITED BY "  " INTO MESS.

           STRING "ACCOUNTS PROCESSED: ", TOTAL-ACCOUNTS-PROC-CTR,
                  " | ", EXT-ACUSQL-DB-NAME
                  DELIMITED BY "  " INTO MISC-MSG.

           MOVE FORM-PATHNAME     TO SQLLOG-PROGRAM.
           MOVE MESS              TO SQLLOG-RANGES.
           MOVE "PASS"            TO SQLLOG-STATUS.
           MOVE MISC-MSG          TO SQLLOG-MESS.
           PERFORM UPDATE-SQLLOG-FILE.

      ******************************************************************
      *
      *    I/O   R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".   
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBSQLLOGIN.CPY".

      * COPY "LIBLP/LPNO1IN.CPY".
      * COPYMEMBER: LIBLP/LPNO1IN
      *
      * BAH 2024.0201 REMOVED NO-FMNO, NO-PRINTED, NO-WAGE-ASSIGN-DELINQ
      *               ADDED FIELDS AT THE END OF RECORD
       LOAD-NO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( NO-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( NO-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO NO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO NO-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( NO-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO NO-ALL-BASE
              MOVE FILPATH-MACHINE         TO NO-ALL-MACHINE
              MOVE FILPATH-DATA            TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH.

      *-----------------------------------------------------------------
       OPEN-NO1-FILE.
           PERFORM LOAD-NO1-FILE.
           PERFORM OPEN-IT.
           MOVE NO-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
      *WRITE-NO1-FILE. *> EMBEDDED - NO
      *    PERFORM WRITE-IT.
      *    MOVE NO-PATH-SQL    TO E-FILE.
      *    MOVE NO1-KEY        TO E-KEYX.
      *
      *    MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.
      *
      *    EXEC SQL
      *      INSERT INTO DBO.NOFILE
      *      SELECT WRKNOFILE.WRKNO_BRNO,
      *             WRKNOFILE.WRKNO_ACCTNO,
      *             GETDATE(),
      *             CAST(WRKNOFILE.WRKNO_TDUE_DATE AS DATE),
      *             WRKNOFILE.WRKNO_TDUELC,
      *             WRKNOFILE.WRKNO_TDUEAMT,
      *             WRKNOFILE.WRKNO_TDUEPRIN,
      *             WRKNOFILE.WRKNO_TDUETBAL,
      *             WRKNOFILE.WRKNO_TDUEPOFF,
      *             CAST(WRKNOFILE.WRKNO_LDUE_DATE AS DATE),
      *             WRKNOFILE.WRKNO_LDUELC,
      *             WRKNOFILE.WRKNO_LDUEAMT,
      *             CAST(WRKNOFILE.WRKNO_DATE_PAID_LAST AS DATE),
      *             WRKNOFILE.WRKNO_AMT_PAID_LAST,
      *             WRKNOFILE.WRKNO_LSTLPKEY,
      *             CAST(WRKNOFILE.WRKNO_NXDUE_DATE AS DATE),
      *             WRKNOFILE.WRKNO_NXDUEAMT,
      *             WRKNOFILE.WRKNO_NXDUELC,
      *             WRKNOFILE.WRKNO_TRUCONTR,
      *             WRKNOFILE.WRKNO_RECENCY,
      *             CAST(WRKNOFILE.WRKNO_PDTH_DATE AS DATE),
      *             CAST(WRKNOFILE.WRKNO_LCPDTH_DATE AS DATE),
      *             WRKNOFILE.WRKNO_LCHG_INTDUE,
      *             WRKNOFILE.WRKNO_AVAILCREDIT,
      *             WRKNOFILE.WRKNO_TDUEDELINQ,
      *             WRKNOFILE.WRKNO_DEFERMENT,
      *             CAST(WRKNOFILE.WRKNO_TRUE_DUE_DATE AS DATE),
      *             WRKNOFILE.WRKNO_SCHED_PAYMENT,
      *             WRKNOFILE.WRKNO_TDUECONTR,
      *             WRKNOFILE.WRKNO_LDUECONTR,
      *             WRKNOFILE.WRKNO_NXDUECONTR,
      *             WRKNOFILE.WRKNO_CURRENT_AVAIL,
      *             WRKNOFILE.WRKNO_LATE_CHG,
      *             WRKNOFILE.WRKNO_PAST_DUE_DAYS,
      *             WRKNOFILE.WRKNO_RECDEL,
      *             WRKNOFILE.WRKNO_AVCR_RESTRICT_FG,
      *             WRKNOFILE.WRKNO_INDU_PENALTY,
      *             WRKNOFILE.WRKNO_CPOT30,
      *             WRKNOFILE.WRKNO_RDPOT30,
      *             WRKNOFILE.WRKNO_CPOTCHGOFF,
      *             WRKNOFILE.WRKNO_RDPOTCHGOFF,
      *             WRKNOFILE.WRKNO_CSUB,
      *             WRKNOFILE.WRKNO_RSUB
      *
      *       FROM DBO.WRKNOFILE WRKNOFILE
      *       LEFT OUTER JOIN DBO.NOFILE NOFILE
      *         ON NOFILE.NO_BRNO       = WRKNOFILE.WRKNO_BRNO
      *         AND NOFILE.NO_ACCTNO    = WRKNOFILE.WRKNO_ACCTNO
      *
      *       WHERE WRKNOFILE.WRKNO_KEY = :WS-WKNO-FILE-KEY 
      *         AND NOFILE.NO_BRNO IS NULL
      *    END-EXEC.
      *
      *    PERFORM SQL-IO-VALIDATION.
      *
      *    IF ( IO-FG = 8 )     *> LOCKED
      *       GO TO WRITE-NO1-FILE.
      *
      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.
      *
      *    MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.
      *
      *-----------------------------------------------------------------
      *REWRITE-NO1-FILE. *> EMBEDDED - NO
      *
      *    MOVE NO-PATH-OWNBR  TO NO-BRNO.
      *    PERFORM REWRITE-IT.
      *    MOVE NO-PATH-SQL    TO E-FILE.
      *    MOVE NO1-KEY        TO E-KEYX.
      *
      *    MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.
      *
      *     EXEC SQL
      *      UPDATE DBO.NOFILE
      *       SET
      *        NOFILE.NO_LTOUCH_DATE   = :QNO-LTOUCH-DATE,
      *        NOFILE.NO_TDUE_DATE     = WRKNOFILE.WRKNO_TDUE_DATE,
      *        NOFILE.NO_TDUELC        = WRKNOFILE.WRKNO_TDUELC,
      *        NOFILE.NO_TDUEAMT       = WRKNOFILE.WRKNO_TDUEAMT,
      *        NOFILE.NO_TDUEPRIN      = WRKNOFILE.WRKNO_TDUEPRIN,
      *        NOFILE.NO_TDUETBAL      = WRKNOFILE.WRKNO_TDUETBAL,
      *        NOFILE.NO_TDUEPOFF      = WRKNOFILE.WRKNO_TDUEPOFF,
      *        NOFILE.NO_LDUE_DATE     = WRKNOFILE.WRKNO_LDUE_DATE,
      *        NOFILE.NO_LDUELC        = WRKNOFILE.WRKNO_LDUELC,
      *        NOFILE.NO_LDUEAMT       = WRKNOFILE.WRKNO_LDUEAMT,
      *        NOFILE.NO_DATE_PAID_LAST 
      *               = WRKNOFILE.WRKNO_DATE_PAID_LAST,
      *        NOFILE.NO_AMT_PAID_LAST = WRKNOFILE.WRKNO_AMT_PAID_LAST,
      *        NOFILE.NO_LSTLPKEY      = WRKNOFILE.WRKNO_LSTLPKEY,
      *        NOFILE.NO_NXDUE_DATE    = WRKNOFILE.WRKNO_NXDUE_DATE,
      *        NOFILE.NO_NXDUEAMT      = WRKNOFILE.WRKNO_NXDUEAMT,
      *        NOFILE.NO_NXDUELC       = WRKNOFILE.WRKNO_NXDUELC,
      *        NOFILE.NO_TRUCONTR      = WRKNOFILE.WRKNO_TRUCONTR,
      *        NOFILE.NO_RECENCY       = WRKNOFILE.WRKNO_RECENCY,
      *        NOFILE.NO_PDTH_DATE     = WRKNOFILE.WRKNO_PDTH_DATE,
      *        NOFILE.NO_LCPDTH_DATE   = WRKNOFILE.WRKNO_LCPDTH_DATE,
      *        NOFILE.NO_LCHG_INTDUE   = WRKNOFILE.WRKNO_LCHG_INTDUE,
      *        NOFILE.NO_AVAILCREDIT   = WRKNOFILE.WRKNO_AVAILCREDIT,
      *        NOFILE.NO_TDUEDELINQ    = WRKNOFILE.WRKNO_TDUEDELINQ,
      *        NOFILE.NO_DEFERMENT     = WRKNOFILE.WRKNO_DEFERMENT,
      *        NOFILE.NO_TRUE_DUE_DATE = WRKNOFILE.WRKNO_TRUE_DUE_DATE,
      *        NOFILE.NO_SCHED_PAYMENT = WRKNOFILE.WRKNO_SCHED_PAYMENT,
      *        NOFILE.NO_TDUECONTR     = WRKNOFILE.WRKNO_TDUECONTR,
      *        NOFILE.NO_LDUECONTR     = WRKNOFILE.WRKNO_LDUECONTR,
      *        NOFILE.NO_NXDUECONTR    = WRKNOFILE.WRKNO_NXDUECONTR,
      *        NOFILE.NO_CURRENT_AVAIL = WRKNOFILE.WRKNO_CURRENT_AVAIL,
      *        NOFILE.NO_LATE_CHG      = WRKNOFILE.WRKNO_LATE_CHG,
      *        NOFILE.NO_PAST_DUE_DAYS = WRKNOFILE.WRKNO_PAST_DUE_DAYS,
      *        NOFILE.NO_RECDEL        = WRKNOFILE.WRKNO_RECDEL,
      *        NOFILE.NO_AVCR_RESTRICT_FG 
      *               = WRKNOFILE.WRKNO_AVCR_RESTRICT_FG,
      *        NOFILE.NO_INDU_PENALTY  = WRKNOFILE.WRKNO_INDU_PENALTY,
      *        NOFILE.NO_CPOT30        = WRKNOFILE.WRKNO_CPOT30,
      *        NOFILE.NO_RDPOT30       = WRKNOFILE.WRKNO_RDPOT30,
      *        NOFILE.NO_CPOTCHGOFF    = WRKNOFILE.WRKNO_CPOTCHGOFF,
      *        NOFILE.NO_RDPOTCHGOFF   = WRKNOFILE.WRKNO_RDPOTCHGOFF,
      *        NOFILE.NO_CSUB          = WRKNOFILE.WRKNO_CSUB,
      *        NOFILE.NO_RSUB          = WRKNOFILE.WRKNO_RSUB
      *         FROM DBO.WRKNOFILE, DBO.NOFILE        
      *        WHERE WRKNOFILE.WRKNO_KEY    = :WS-WKNO-FILE-KEY
      *          AND WRKNOFILE.WRKNO_BRNO   =  NOFILE.NO_BRNO 
      *          AND WRKNOFILE.WRKNO_ACCTNO =  NOFILE.NO_ACCTNO 
      *     END-EXEC.
      *
      *    PERFORM SQL-IO-VALIDATION.
      *
      *    IF ( IO-FG = 8 )     *> LOCKED
      *       GO TO REWRITE-NO1-FILE.
      *
      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.
      *
      *    MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
      * CLOSE-NO1-FILE.
      *
      *      PERFORM CLOSE-IT.

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
              MOVE "Y" TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> SSP - LN,BW,NO
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

      *     IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
      *        ( QLN3-WEND-ACCTNO NOT NUMERIC )
      *        MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
      *        MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
      *        MOVE LN-CLASS             TO QLN3-WBEG-CLASS
      *        MOVE ALL "9"              TO QLN3-WEND-CLASS
      *        MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
      *        MOVE ALL "9"              TO QLN3-WEND-ACCTNO.


      *  STORED PROCEDURE:
      *
      *   SELECT LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SUBCLASS,
      *          LNFILE.LN_LAWCODE,
      *          LNFILE.LN_SSNO1,
      *          LNFILE.LN_MAKERCD1,
      *          LNFILE.LN_SSNO2,
      *          LNFILE.LN_MAKERCD2,
      *          LNFILE.LN_SSNO3,
      *          LNFILE.LN_MAKERCD3,
      *          LNFILE.LN_SSNO4,
      *          LNFILE.LN_MAKERCD4,
      *          LNFILE.LN_ORGST,
      *          LNFILE.LN_ORGBR,
      *          LNFILE.LN_OWNST,
      *          LNFILE.LN_SPRCLASS,
      *          LNFILE.LN_CONVERCD,
      *          LNFILE.LN_MIDSTREAM,
      *          LNFILE.LN_PURCD,
      *          LNFILE.LN_SRCD,
      *          LNFILE.LN_COLLCD,
      *          LNFILE.LN_COLLID,
      *          LNFILE.LN_POCD,
      *          LNFILE.LN_PLCD,
      *          LNFILE.LN_PLTYPE,
      *          LNFILE.LN_CONFIDENTCD,
      *          LNFILE.LN_ALLOTCD,
      *          LNFILE.LN_RECASTCD,
      *          LNFILE.LN_SKIP_TRW,
      *          LNFILE.LN_STATUSFG,
      *          LNFILE.LN_SMALLOAN,
      *          LNFILE.LN_SALEFIN,
      *          LNFILE.LN_SERVACCT,
      *          LNFILE.LN_LOANTYPE,
      *          LNFILE.LN_ADDON_FG,
      *          LNFILE.LN_PAY_SCHLD_FG,
      *          LNFILE.LN_LC_CODE,
      *          LNFILE.LN_CAN_LOAN_NO,
      *          LNFILE.LN_STATFLAGS,
      *          CAST(LNFILE.LN_ENTDATE                AS VARCHAR(10)) AS LN_ENTDATE,
      *          CAST(LNFILE.LN_LOANDATE               AS VARCHAR(10)) AS LN_LOANDATE,
      *          CAST(LNFILE.LN_PURDATE                AS VARCHAR(10)) AS LN_PURDATE,
      *          CAST(LNFILE.LN_INTDATE                AS VARCHAR(10)) AS LN_INTDATE,
      *          CAST(LNFILE.LN_CASHDATE               AS VARCHAR(10)) AS LN_CASHDATE,
      *          CAST(LNFILE.LN_FILEDATE               AS VARCHAR(10)) AS LN_FILEDATE,
      *          CAST(LNFILE.LN_RESCISSION_DATE        AS VARCHAR(10)) AS LN_RESCISSION_DATE,
      *          CAST(LNFILE.LN_PART_RECOURSE_DATE     AS VARCHAR(10)) AS LN_PART_RECOURSE_DATE,
      *          CAST(LNFILE.LN_ORIG_1STPYDATE         AS VARCHAR(10)) AS LN_ORIG_1STPYDATE,
      *          CAST(LNFILE.LN_1STPYDATE              AS VARCHAR(10)) AS LN_1STPYDATE,
      *          CAST(LNFILE.LN_JDDATE                 AS VARCHAR(10)) AS LN_JDDATE,
      *          CAST(LNFILE.LN_PLDATE                 AS VARCHAR(10)) AS LN_PLDATE,
      *          CAST(LNFILE.LN_PL_TRDATE              AS VARCHAR(10)) AS LN_PL_TRDATE,
      *          CAST(LNFILE.LN_ACCELDATE              AS VARCHAR(10)) AS LN_ACCELDATE,
      *          CAST(LNFILE.LN_REDUDATE               AS VARCHAR(10)) AS LN_REDUDATE,
      *          CAST(LNFILE.LN_POFFDATE               AS VARCHAR(10)) AS LN_POFFDATE,
      *          CAST(LNFILE.LN_PO_TRDATE              AS VARCHAR(10)) AS LN_PO_TRDATE,
      *          CAST(LNFILE.LN_LEGALDATE              AS VARCHAR(10)) AS LN_LEGALDATE,
      *          CAST(LNFILE.LN_BNKRPTDATE             AS VARCHAR(10)) AS LN_BNKRPTDATE,
      *          CAST(LNFILE.LN_BNKRPT_DISMISS_DATE    AS VARCHAR(10)) AS LN_BNKRPT_DISMISS_DATE  ,
      *          CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE  AS VARCHAR(10)) AS LN_BNKRPT_DISCHARGE_DATE,
      *          CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE   AS VARCHAR(10)) AS LN_BNKRPT_REAFFIRM_DATE,
      *          CAST(LNFILE.LN_DEBTAIDATE             AS VARCHAR(10)) AS LN_DEBTAIDATE,
      *          CAST(LNFILE.LN_REPODATE               AS VARCHAR(10)) AS LN_REPODATE,
      *          CAST(LNFILE.LN_REDEEMDATE             AS VARCHAR(10)) AS LN_REDEEMDATE,
      *          CAST(LNFILE.LN_SOLDDATE               AS VARCHAR(10)) AS LN_SOLDDATE,
      *          CAST(LNFILE.LN_INS_CONFIRM_DATE       AS VARCHAR(10)) AS LN_INS_CONFIRM_DATE,
      *          CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE    AS VARCHAR(10)) AS LN_PRIOR_RN_FOR10_DATE,
      *          CAST(LNFILE.LN_LAST_RN_FOR10_DATE     AS VARCHAR(10)) AS LN_LAST_RN_FOR10_DATE ,
      *          CAST(LNFILE.LN_TRW_DOFD_DATE          AS VARCHAR(10)) AS LN_TRW_DOFD_DATE,
      *          CAST(LNFILE.LN_LAST_PYPA_DATE         AS VARCHAR(10)) AS LN_LAST_PYPA_DATE,
      *          CAST(LNFILE.LN_BT_TRDATE              AS VARCHAR(10)) AS LN_BT_TRDATE,
      *          CAST(LNFILE.LN_METRO2_LAST_PMT_DATE   AS VARCHAR(10)) AS LN_METRO2_LAST_PMT_DATE,
      *          CAST(LNFILE.LN_FOLLOWUP_DATE          AS VARCHAR(10)) AS LN_FOLLOWUP_DATE,
      *          CAST(LNFILE.LN_LAST_ACCRUAL_DATE      AS VARCHAR(10)) AS LN_LAST_ACCRUAL_DATE,
      *          LNFILE.LN_APRATE,
      *          LNFILE.LN_EFFRATE,
      *          LNFILE.LN_SMPRATE,
      *          LNFILE.LN_YLDRATE,
      *          LNFILE.LN_OVRDRATE,
      *          LNFILE.LN_ENDORATE,
      *          LNFILE.LN_JDRATE,
      *          LNFILE.LN_ACCRATE,
      *          LNFILE.LN_REDURATE,
      *          LNFILE.LN_CONRATE,
      *          LNFILE.LN_SERVPER,
      *          LNFILE.LN_SELLPR,
      *          LNFILE.LN_DOWNPY,
      *          LNFILE.LN_TRADEIN,
      *          LNFILE.LN_LNAMT,
      *          LNFILE.LN_FINCHG,
      *          LNFILE.LN_INTCHG,
      *          LNFILE.LN_SERCHG,
      *          LNFILE.LN_EXTCHG,
      *          LNFILE.LN_MAINTFEE,
      *          LNFILE.LN_POINTS,
      *          LNFILE.LN_PROCEEDS,
      *          LNFILE.LN_ORGTERM,
      *          LNFILE.LN_1STPYAMT,
      *          LNFILE.LN_NOREGPY,
      *          LNFILE.LN_REGPYAMT,
      *          LNFILE.LN_LASTPYAMT,
      *          LNFILE.LN_PROCASH,
      *          LNFILE.LN_TILLNO,
      *          LNFILE.LN_STATE_FLAG,
      *          LNFILE.LN_STATE_WORKER,
      *          LNFILE.LN_STATE_WORKER2,
      *          LNFILE.LN_FEECODE_1,
      *          LNFILE.LN_FEEAMT_1,
      *          LNFILE.LN_FEEOURS_1,
      *          LNFILE.LN_FEEFINCHG_1,
      *          LNFILE.LN_FEECODE_2,
      *          LNFILE.LN_FEEAMT_2,
      *          LNFILE.LN_FEEOURS_2,
      *          LNFILE.LN_FEEFINCHG_2,
      *          LNFILE.LN_FEECODE_3,
      *          LNFILE.LN_FEEAMT_3,
      *          LNFILE.LN_FEEOURS_3,
      *          LNFILE.LN_FEEFINCHG_3,
      *          LNFILE.LN_FEECODE_4,
      *          LNFILE.LN_FEEAMT_4,
      *          LNFILE.LN_FEEOURS_4,
      *          LNFILE.LN_FEEFINCHG_4,
      *          LNFILE.LN_FEECODE_5,
      *          LNFILE.LN_FEEAMT_5,
      *          LNFILE.LN_FEEOURS_5,
      *          LNFILE.LN_FEEFINCHG_5,
      *          LNFILE.LN_FEECODE_6,
      *          LNFILE.LN_FEEAMT_6,
      *          LNFILE.LN_FEEOURS_6,
      *          LNFILE.LN_FEEFINCHG_6,
      *          LNFILE.LN_FEECODE_7,
      *          LNFILE.LN_FEEAMT_7,
      *          LNFILE.LN_FEEOURS_7,
      *          LNFILE.LN_FEEFINCHG_7,
      *          LNFILE.LN_FEECODE_8,
      *          LNFILE.LN_FEEAMT_8,
      *          LNFILE.LN_FEEOURS_8,
      *          LNFILE.LN_FEEFINCHG_8,
      *          LNFILE.LN_FEECODE_9,
      *          LNFILE.LN_FEEAMT_9,
      *          LNFILE.LN_FEEOURS_9,
      *          LNFILE.LN_FEEFINCHG_9,
      *          LNFILE.LN_FEECODE_10,
      *          LNFILE.LN_FEEAMT_10,
      *          LNFILE.LN_FEEOURS_10,
      *          LNFILE.LN_FEEFINCHG_10,
      *          LNFILE.LN_FEECODE_11,
      *          LNFILE.LN_FEEAMT_11,
      *          LNFILE.LN_FEEOURS_11,
      *          LNFILE.LN_FEEFINCHG_11,
      *          LNFILE.LN_FEECODE_12,
      *          LNFILE.LN_FEEAMT_12,
      *          LNFILE.LN_FEEOURS_12,
      *          LNFILE.LN_FEEFINCHG_12,
      *          LNFILE.LN_FEECODE_13,
      *          LNFILE.LN_FEEAMT_13,
      *          LNFILE.LN_FEEOURS_13,
      *          LNFILE.LN_FEEFINCHG_13,
      *          LNFILE.LN_FEECODE_14,
      *          LNFILE.LN_FEEAMT_14,
      *          LNFILE.LN_FEEOURS_14,
      *          LNFILE.LN_FEEFINCHG_14,
      *          LNFILE.LN_FEECODE_15,
      *          LNFILE.LN_FEEAMT_15,
      *          LNFILE.LN_FEEOURS_15,
      *          LNFILE.LN_FEEFINCHG_15,
      *          LNFILE.LN_FEECODE_16,
      *          LNFILE.LN_FEEAMT_16,
      *          LNFILE.LN_FEEOURS_16,
      *          LNFILE.LN_FEEFINCHG_16,
      *          LNFILE.LN_FEECODE_17,
      *          LNFILE.LN_FEEAMT_17,
      *          LNFILE.LN_FEEOURS_17,
      *          LNFILE.LN_FEEFINCHG_17,
      *          LNFILE.LN_FEECODE_18,
      *          LNFILE.LN_FEEAMT_18,
      *          LNFILE.LN_FEEOURS_18,
      *          LNFILE.LN_FEEFINCHG_18,
      *          LNFILE.LN_FEECODE_19,
      *          LNFILE.LN_FEEAMT_19,
      *          LNFILE.LN_FEEOURS_19,
      *          LNFILE.LN_FEEFINCHG_19,
      *          LNFILE.LN_FEECODE_20,
      *          LNFILE.LN_FEEAMT_20,
      *          LNFILE.LN_FEEOURS_20,
      *          LNFILE.LN_FEEFINCHG_20,
      *          LNFILE.LN_INSURED,
      *          LNFILE.LN_INSTYPES,
      *          LNFILE.LN_INSELIGIBLE,
      *          LNFILE.LN_INSCOMP_1,
      *          LNFILE.LN_INSOURS_1,
      *          CAST(LNFILE.LN_INSEFF_DATE_1   AS VARCHAR(10)) AS LN_INSEFF_DATE_1,
      *          CAST(LNFILE.LN_INSEXP_DATE_1   AS VARCHAR(10)) AS LN_INSEXP_DATE_1,
      *          LNFILE.LN_INSPREM_1,
      *          LNFILE.LN_INSCOVR_1,
      *          LNFILE.LN_INSCOMM_1,
      *          LNFILE.LN_INSANTICERN_1,
      *          LNFILE.LN_INSANTICADJ_1,
      *          LNFILE.LN_INSUFERN_1,
      *          LNFILE.LN_INSACCERN_1,
      *          LNFILE.LN_INSEARN_ORDER_1,
      *          LNFILE.LN_INSFINCHG_1,
      *          LNFILE.LN_INSCOMP_2,
      *          LNFILE.LN_INSOURS_2,
      *          CAST(LNFILE.LN_INSEFF_DATE_2   AS VARCHAR(10)) AS LN_INSEFF_DATE_2,
      *          CAST(LNFILE.LN_INSEXP_DATE_2   AS VARCHAR(10)) AS LN_INSEXP_DATE_2,
      *          LNFILE.LN_INSPREM_2,
      *          LNFILE.LN_INSCOVR_2,
      *          LNFILE.LN_INSCOMM_2,
      *          LNFILE.LN_INSANTICERN_2,
      *          LNFILE.LN_INSANTICADJ_2,
      *          LNFILE.LN_INSUFERN_2,
      *          LNFILE.LN_INSACCERN_2,
      *          LNFILE.LN_INSEARN_ORDER_2,
      *          LNFILE.LN_INSFINCHG_2,
      *          LNFILE.LN_INSCOMP_3,
      *          LNFILE.LN_INSOURS_3,
      *          CAST(LNFILE.LN_INSEFF_DATE_3   AS VARCHAR(10)) AS LN_INSEFF_DATE_3,
      *          CAST(LNFILE.LN_INSEXP_DATE_3   AS VARCHAR(10)) AS LN_INSEXP_DATE_3,
      *          LNFILE.LN_INSPREM_3,
      *          LNFILE.LN_INSCOVR_3,
      *          LNFILE.LN_INSCOMM_3,
      *          LNFILE.LN_INSANTICERN_3,
      *          LNFILE.LN_INSANTICADJ_3,
      *          LNFILE.LN_INSUFERN_3,
      *          LNFILE.LN_INSACCERN_3,
      *          LNFILE.LN_INSEARN_ORDER_3,
      *          LNFILE.LN_INSFINCHG_3,
      *          LNFILE.LN_INSCOMP_4,
      *          LNFILE.LN_INSOURS_4,
      *          CAST(LNFILE.LN_INSEFF_DATE_4   AS VARCHAR(10)) AS LN_INSEFF_DATE_4,
      *          CAST(LNFILE.LN_INSEXP_DATE_4   AS VARCHAR(10)) AS LN_INSEXP_DATE_4,
      *          LNFILE.LN_INSPREM_4,
      *          LNFILE.LN_INSCOVR_4,
      *          LNFILE.LN_INSCOMM_4,
      *          LNFILE.LN_INSANTICERN_4,
      *          LNFILE.LN_INSANTICADJ_4,
      *          LNFILE.LN_INSUFERN_4,
      *          LNFILE.LN_INSACCERN_4,
      *          LNFILE.LN_INSEARN_ORDER_4,
      *          LNFILE.LN_INSFINCHG_4,
      *          LNFILE.LN_INSCOMP_5,
      *          LNFILE.LN_INSOURS_5,
      *          CAST(LNFILE.LN_INSEFF_DATE_5   AS VARCHAR(10)) AS LN_INSEFF_DATE_5,
      *          CAST(LNFILE.LN_INSEXP_DATE_5   AS VARCHAR(10)) AS LN_INSEXP_DATE_5,
      *          LNFILE.LN_INSPREM_5,
      *          LNFILE.LN_INSCOVR_5,
      *          LNFILE.LN_INSCOMM_5,
      *          LNFILE.LN_INSANTICERN_5,
      *          LNFILE.LN_INSANTICADJ_5,
      *          LNFILE.LN_INSUFERN_5,
      *          LNFILE.LN_INSACCERN_5,
      *          LNFILE.LN_INSEARN_ORDER_5,
      *          LNFILE.LN_INSFINCHG_5,
      *          LNFILE.LN_INSCOMP_6,
      *          LNFILE.LN_INSOURS_6,
      *          CAST(LNFILE.LN_INSEFF_DATE_6   AS VARCHAR(10)) AS LN_INSEFF_DATE_6,
      *          CAST(LNFILE.LN_INSEXP_DATE_6   AS VARCHAR(10)) AS LN_INSEXP_DATE_6,
      *          LNFILE.LN_INSPREM_6,
      *          LNFILE.LN_INSCOVR_6,
      *          LNFILE.LN_INSCOMM_6,
      *          LNFILE.LN_INSANTICERN_6,
      *          LNFILE.LN_INSANTICADJ_6,
      *          LNFILE.LN_INSUFERN_6,
      *          LNFILE.LN_INSACCERN_6,
      *          LNFILE.LN_INSEARN_ORDER_6,
      *          LNFILE.LN_INSFINCHG_6,
      *          LNFILE.LN_INSCOMP_7,
      *          LNFILE.LN_INSOURS_7,
      *          CAST(LNFILE.LN_INSEFF_DATE_7   AS VARCHAR(10)) AS LN_INSEFF_DATE_7,
      *          CAST(LNFILE.LN_INSEXP_DATE_7   AS VARCHAR(10)) AS LN_INSEXP_DATE_7,
      *          LNFILE.LN_INSPREM_7,
      *          LNFILE.LN_INSCOVR_7,
      *          LNFILE.LN_INSCOMM_7,
      *          LNFILE.LN_INSANTICERN_7,
      *          LNFILE.LN_INSANTICADJ_7,
      *          LNFILE.LN_INSUFERN_7,
      *          LNFILE.LN_INSACCERN_7,
      *          LNFILE.LN_INSEARN_ORDER_7,
      *          LNFILE.LN_INSFINCHG_7,
      *          LNFILE.LN_INSCOMP_8,
      *          LNFILE.LN_INSOURS_8,
      *          CAST(LNFILE.LN_INSEFF_DATE_8   AS VARCHAR(10)) AS LN_INSEFF_DATE_8,
      *          CAST(LNFILE.LN_INSEXP_DATE_8   AS VARCHAR(10)) AS LN_INSEXP_DATE_8,
      *          LNFILE.LN_INSPREM_8,
      *          LNFILE.LN_INSCOVR_8,
      *          LNFILE.LN_INSCOMM_8,
      *          LNFILE.LN_INSANTICERN_8,
      *          LNFILE.LN_INSANTICADJ_8,
      *          LNFILE.LN_INSUFERN_8,
      *          LNFILE.LN_INSACCERN_8,
      *          LNFILE.LN_INSEARN_ORDER_8,
      *          LNFILE.LN_INSFINCHG_8,
      *          LNFILE.LN_INSCOMP_9,
      *          LNFILE.LN_INSOURS_9,
      *          CAST(LNFILE.LN_INSEFF_DATE_9   AS VARCHAR(10)) AS LN_INSEFF_DATE_9,
      *          CAST(LNFILE.LN_INSEXP_DATE_9   AS VARCHAR(10)) AS LN_INSEXP_DATE_9,
      *          LNFILE.LN_INSPREM_9,
      *          LNFILE.LN_INSCOVR_9,
      *          LNFILE.LN_INSCOMM_9,
      *          LNFILE.LN_INSANTICERN_9,
      *          LNFILE.LN_INSANTICADJ_9,
      *          LNFILE.LN_INSUFERN_9,
      *          LNFILE.LN_INSACCERN_9,
      *          LNFILE.LN_INSEARN_ORDER_9,
      *          LNFILE.LN_INSFINCHG_9,
      *          LNFILE.LN_INSCOMP_10,
      *          LNFILE.LN_INSOURS_10,
      *          CAST(LNFILE.LN_INSEFF_DATE_10  AS VARCHAR(10)) AS LN_INSEFF_DATE_10,
      *          CAST(LNFILE.LN_INSEXP_DATE_10  AS VARCHAR(10)) AS LN_INSEXP_DATE_10,
      *          LNFILE.LN_INSPREM_10,
      *          LNFILE.LN_INSCOVR_10,
      *          LNFILE.LN_INSCOMM_10,
      *          LNFILE.LN_INSANTICERN_10,
      *          LNFILE.LN_INSANTICADJ_10,
      *          LNFILE.LN_INSUFERN_10,
      *          LNFILE.LN_INSACCERN_10,
      *          LNFILE.LN_INSEARN_ORDER_10,
      *          LNFILE.LN_INSFINCHG_10,
      *          LNFILE.LN_PY_YYYYMM_RECDEL,
      *          LNFILE.LN_MTD_PAYS_RECDEL,
      *          LNFILE.LN_GARNISH_EXTR,
      *          LNFILE.LN_SHIP_SAME_FG,
      *          LNFILE.LN_AD_D_INS_COVR,
      *          LNFILE.LN_TOT_PARTIAL_CHGOFF,
      *          LNFILE.LN_LPO,
      *          LNFILE.LN_REP_CD,
      *          LNFILE.LN_EMP_ID,
      *          LNFILE.LN_PROCESS_CD,
      *          LNFILE.LN_WCBC_CREDITLIM,
      *          LNFILE.LN_FOLLOWUP_CD,
      *          LNFILE.LN_MIP_FG,
      *          LNFILE.LN_MIP_MTD_CLAHPAID,
      *          CAST(LNFILE.LN_MIP_CLPDTH_DATE AS VARCHAR(10)) AS LN_MIP_CLPDTH_DATE,
      *          LNFILE.LN_MIP_TOT_CLPAID,
      *          CAST(LNFILE.LN_MIP_AHPDTH_DATE AS VARCHAR(10)) AS LN_MIP_AHPDTH_DATE,
      *          LNFILE.LN_MIP_TOT_AHPAID,
      *          LNFILE.LN_IL_GROSS_INCOME,
      *          LNFILE.LN_IL_GROSS_INCOME2,
      *          LNFILE.LN_RESCISSION,
      *          LNFILE.LN_INTERIM_TRW,
      *          LNFILE.LN_DLNO,
      *          LNFILE.LN_DLPOOL,
      *          LNFILE.LN_DLRECOURSE,
      *          LNFILE.LN_DLPARTRCAMT,
      *          LNFILE.LN_ORIGPURAMT,
      *          LNFILE.LN_PURBAL,
      *          LNFILE.LN_DLRVPRIN_1,
      *          LNFILE.LN_DLRVPRIN_2,
      *          LNFILE.LN_DLRVPRIN_3,
      *          LNFILE.LN_DLRVCHGS_1,
      *          LNFILE.LN_DLRVCHGS_2,
      *          LNFILE.LN_DLRVCHGS_3,
      *          LNFILE.LN_DLPARVHELD,
      *          LNFILE.LN_DLPARVPAID,
      *          LNFILE.LN_DLDISC,
      *          CAST(LNFILE.LN_LCPDTH_DATE     AS VARCHAR(10)) AS LN_LCPDTH_DATE,
      *          CAST(LNFILE.LN_INTPDTH_DATE    AS VARCHAR(10)) AS LN_INTPDTH_DATE,
      *          LNFILE.LN_CURBAL,
      *          LNFILE.LN_INTBAL,
      *          LNFILE.LN_LCBAL,
      *          LNFILE.LN_OTHBAL,
      *          LNFILE.LN_OT2BAL,
      *          LNFILE.LN_LCPAID,
      *          LNFILE.LN_LCPARTIALS,
      *          LNFILE.LN_STMT_COUNTER,
      *          LNFILE.LN_PRIOR_STMT_COUNTER,
      *          LNFILE.LN_FROZEN_REASON,
      *          LNFILE.LN_COUPON_STATEMENT,
      *          LNFILE.LN_MONTHEND_BAL,
      *          LNFILE.LN_ANTICTERM,
      *          LNFILE.LN_ANTICERN_1,
      *          LNFILE.LN_ANTICADJ_1,
      *          LNFILE.LN_UFERN_1,
      *          LNFILE.LN_ACCERN_1,
      *          LNFILE.LN_EARN_ORDER_1,
      *          LNFILE.LN_ANTICERN_2,
      *          LNFILE.LN_ANTICADJ_2,
      *          LNFILE.LN_UFERN_2,
      *          LNFILE.LN_ACCERN_2,
      *          LNFILE.LN_EARN_ORDER_2,
      *          LNFILE.LN_ANTICERN_3,
      *          LNFILE.LN_ANTICADJ_3,
      *          LNFILE.LN_UFERN_3,
      *          LNFILE.LN_ACCERN_3,
      *          LNFILE.LN_EARN_ORDER_3,
      *          LNFILE.LN_ANTICERN_4,
      *          LNFILE.LN_ANTICADJ_4,
      *          LNFILE.LN_UFERN_4,
      *          LNFILE.LN_ACCERN_4,
      *          LNFILE.LN_EARN_ORDER_4,
      *          LNFILE.LN_ANTICERN_5,
      *          LNFILE.LN_ANTICADJ_5,
      *          LNFILE.LN_UFERN_5,
      *          LNFILE.LN_ACCERN_5,
      *          LNFILE.LN_EARN_ORDER_5,
      *          LNFILE.LN_ANTICERN_6,
      *          LNFILE.LN_ANTICADJ_6,
      *          LNFILE.LN_UFERN_6,
      *          LNFILE.LN_ACCERN_6,
      *          LNFILE.LN_EARN_ORDER_6,
      *          LNFILE.LN_ANTICERN_7,
      *          LNFILE.LN_ANTICADJ_7,
      *          LNFILE.LN_UFERN_7,
      *          LNFILE.LN_ACCERN_7,
      *          LNFILE.LN_EARN_ORDER_7,
      *          LNFILE.LN_ANTICERN_8,
      *          LNFILE.LN_ANTICADJ_8,
      *          LNFILE.LN_UFERN_8,
      *          LNFILE.LN_ACCERN_8,
      *          LNFILE.LN_EARN_ORDER_8,
      *          LNFILE.LN_ANTICERN_9,
      *          LNFILE.LN_ANTICADJ_9,
      *          LNFILE.LN_UFERN_9,
      *          LNFILE.LN_ACCERN_9,
      *          LNFILE.LN_EARN_ORDER_9,
      *          LNFILE.LN_ANTICERN_10,
      *          LNFILE.LN_ANTICADJ_10,
      *          LNFILE.LN_UFERN_10,
      *          LNFILE.LN_ACCERN_10,
      *          LNFILE.LN_EARN_ORDER_10,
      *          LNFILE.LN_LMO_PRIOR_INTEREST,
      *          LNFILE.LN_PRIOR_UNEARNED,
      *          LNFILE.LN_ERNDELQ_STOP,
      *          LNFILE.LN_NONACRERN,
      *          LNFILE.LN_PRIOR_NONACRERN,
      *          CAST(LNFILE.LN_ERN_IB_PDTH_DATE AS VARCHAR(10)) AS LN_ERN_IB_PDTH_DATE,
      *          LNFILE.LN_ERN_IB_PRINBAL,
      *          LNFILE.LN_ERN_IB_INTOWE,
      *          LNFILE.LN_TOTERN,
      *          LNFILE.LN_TOTOTH,
      *          LNFILE.LN_TOTINT,
      *          LNFILE.LN_TOTLCHG,
      *          LNFILE.LN_TOTDEF,
      *          LNFILE.LN_TOTSERACCT,
      *          LNFILE.LN_LCWAIVERS,
      *          LNFILE.LN_INTWAIVERS,
      *          LNFILE.LN_TOTPAYMNTD,
      *          LNFILE.LN_TOTEXCPAYMNTD,
      *          LNFILE.LN_TOTNODEF,
      *          LNFILE.LN_YTDNODEF,
      *          LNFILE.LN_TOTNODEFACT,
      *          LNFILE.LN_YTDNODEFACT,
      *          LNFILE.LN_TOTNOALDEL,
      *          LNFILE.LN_YTDNOALDEL,
      *          LNFILE.LN_YTDINTPD,
      *          LNFILE.LN_LYRINTPD,
      *          LNFILE.LN_TOTINTPD,
      *          LNFILE.LN_RECASTINT,
      *          LNFILE.LN_RECASTPRIN,
      *          LNFILE.LN_CL_REBATE,
      *          LNFILE.LN_AH_REBATE,
      *          LNFILE.LN_PP_REBATE,
      *          LNFILE.LN_INT_REBATE,
      *          LNFILE.LN_SC_REBATE,
      *          LNFILE.LN_MF_REBATE,
      *          LNFILE.LN_DF_REBATE,
      *          LNFILE.LN_EX_REBATE,
      *          LNFILE.LN_O1_REBATE,
      *          LNFILE.LN_O2_REBATE,
      *          LNFILE.LN_O3_REBATE,
      *          LNFILE.LN_O4_REBATE,
      *          LNFILE.LN_O5_REBATE,
      *          LNFILE.LN_O6_REBATE,
      *          LNFILE.LN_O7_REBATE,
      *          LNFILE.LN_UN_O7_REB,
      *          LNFILE.LN_UN_O8_REB,
      *          LNFILE.LN_UN_O9_REB,
      *          LNFILE.LN_UN_O10_REB,
      *          LNFILE.LN_PLREASON,
      *          LNFILE.LN_COLLECTOR,
      *          LNFILE.LN_ACTIONCD,
      *          LNFILE.LN_ACTIONCD2,
      *          LNFILE.LN_ATCODE,
      *          LNFILE.LN_RECODE,
      *          LNFILE.LN_ESCROW_FG,
      *          LNFILE.LN_PROMDAYS_PASTDUE,
      *          LNFILE.LN_PROMID,
      *          CAST(LNFILE.LN_PROMDATE        AS VARCHAR(10)) AS LN_PROMDATE,
      *          LNFILE.LN_PROMAMT,
      *          CAST(LNFILE.LN_PROMDATE2       AS VARCHAR(10)) AS LN_PROMDATE2,
      *          CAST(LNFILE.LN_DATE_PAID_LAST  AS VARCHAR(10)) AS LN_DATE_PAID_LAST,
      *          LNFILE.LN_AMT_PAID_LAST,
      *          LNFILE.LN_LASTCFORMNO,
      *          CAST(LNFILE.LN_LASTCFORMDATE   AS VARCHAR(10)) AS LN_LASTCFORMDATE,
      *          LNFILE.LN_DELCNT,
      *          LNFILE.LN_DELCNT2,
      *          LNFILE.LN_CREDITLIM,
      *          LNFILE.LN_CRSCORE,
      *          LNFILE.LN_LASTSFORMNO,
      *          CAST(LNFILE.LN_LASTSFORMDATE   AS VARCHAR(10)) AS LN_LASTSFORMDATE,
      *          LNFILE.LN_SOLCNT,
      *          LNFILE.LN_MESSAGE,
      *          LNFILE.LN_OLDACCTNO,
      *          LNFILE.LN_PRLNNO,
      *          LNFILE.LN_PRBAL,
      *          LNFILE.LN_PRINTDUE,
      *          LNFILE.LN_PRFINCHGRB,
      *          LNFILE.LN_PRINSRB,
      *          LNFILE.LN_PRPOCD,
      *          CAST(LNFILE.LN_PRPODATE        AS VARCHAR(10)) AS LN_PRPODATE,
      *          LNFILE.LN_PRDELINQAMT,
      *          LNFILE.LN_PRLNAMT,
      *          LNFILE.LN_PRPDUE_DAYS,
      *          LNFILE.LN_PRDLDISC_UNEARN,
      *          LNFILE.LN_PRCONPROFILE,
      *          LNFILE.LN_SUM_ORIGPURAMT,
      *          LNFILE.LN_SUM_LNAMT,
      *          LNFILE.LN_SUM_FINCHG,
      *          LNFILE.LN_SUM_INTCHG,
      *          LNFILE.LN_SUM_ORGTERM,
      *          LNFILE.LN_SUM_REGPYAMT,
      *          LNFILE.LN_SUM_EXTCHG,
      *          LNFILE.LN_SUM_SERCHG,
      *          LNFILE.LN_SUM_MAINTFEE,
      *          LNFILE.LN_UNITPER_CD,
      *          LNFILE.LN_UNITPER_FREQ,
      *          LNFILE.LN_DAYS_IN_UNITPER,
      *          LNFILE.LN_UNITPER_PER_YEAR,
      *          LNFILE.LN_REPOCD,
      *          LNFILE.LN_BR_TFR_FG,
      *          LNFILE.LN_O3INS_DLPROC,
      *          LNFILE.LN_INSPREM_DLADJ,
      *          LNFILE.LN_PLAMT,
      *          LNFILE.LN_MTD_PRIN,
      *          LNFILE.LN_MTD_LC,
      *          LNFILE.LN_MTD_INT,
      *          LNFILE.LN_MTD_OT,
      *          LNFILE.LN_MTD_EX,
      *          LNFILE.LN_BNKRPT_CHAPTER,
      *          LNFILE.LN_BMO_CON_STATUS,
      *          LNFILE.LN_BMO_REC_STATUS,
      *          LNFILE.LN_APP_NUMBER,
      *          LNFILE.LN_GIFTNET,
      *          LNFILE.LN_GIFTNET_OUR_PO,
      *          LNFILE.LN_PROC_DISBURSED,
      *          LNFILE.LN_NOODDPY,
      *          LNFILE.LN_CONPROFILE_1,
      *          LNFILE.LN_CONPROFILE_2,
      *          LNFILE.LN_CONPROFILE_3,
      *          LNFILE.LN_CONPROFILE_4,
      *          LNFILE.LN_CONPROFILE_5,
      *          LNFILE.LN_RECPROFILE_1,
      *          LNFILE.LN_RECPROFILE_2,
      *          LNFILE.LN_RECPROFILE_3,
      *          LNFILE.LN_RECPROFILE_4,
      *          LNFILE.LN_RECPROFILE_5,
      *          LNFILE.LN_TRW_STATUS,
      *          LNFILE.LN_ORIGIN,
      *          LNFILE.LN_PO_TRCD,
      *          LNFILE.LN_NEXIS_CRSCORE,
      *          LNFILE.LN_TRW_B_CONSUMER_IND,
      *          LNFILE.LN_PARVHELD_PAID_FG,
      *          LNFILE.LN_ACCT_FROZEN,
      *          LNFILE.LN_TRW_SPECIAL_CODE,
      *          LNFILE.LN_CREATION_STATUS,
      *          LNFILE.LN_TOTNOLCHG,
      *          LNFILE.LN_LBOX,
      *          LNFILE.LN_EXTRA_PRIN_LFP,
      *          LNFILE.LN_REMAIN_INT_SINCE_LFP,
      *          LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
      *          LNFILE.LN_INTERIM_COUPON,
      *          LNFILE.LN_DLCOMM_O3_CMSI,
      *          LNFILE.LN_DLCOMM_PP_CMSI,
      *          LNFILE.LN_TRW_J_CONSUMER_IND,
      *          LNFILE.LN_ACH,
      *          LNFILE.LN_1099_C_FG,
      *          LNFILE.LN_TRW_COMPLIANCE_CODE,
      *          LNFILE.LN_UNWIND_REASON_CODE,
      *          LNFILE.LN_PBI_PBD_FG,
      *          LNFILE.LN_OLD_ACCT_FORMAT_FG,
      *          LNFILE.LN_APPROVED_BY_USER,
      *          LNFILE.LN_APPROVED_BY_CODE,
      *          CAST(LNFILE.LN_APPROVED_BY_DATE AS VARCHAR(10)) AS LN_APPROVED_BY_DATE,
      *          LNFILE.LN_APPROVED_BY_TIME,
      *          CAST(LNFILE.LN_LTOUCH_DATE      AS VARCHAR(10)) AS LN_LTOUCH_DATE,
      *          LNFILE.LN_LTOUCH_TIME,
      *          LNFILE.LN_LOS_APP_ID,
      *          ISNULL(BWFILE.BW_OWNBR, 0)             AS BW_OWNBR,
      *          ISNULL(BWFILE.BW_SSNO, 0)              AS BW_SSNO,
      *          ISNULL(BWFILE.BW_LNAME,' ')            AS BW_LNAME,
      *          ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
      *            AS VARCHAR(10)),'1900-01-01')        AS BW_LAST_RN_FOR10_DATE
      *     FROM DBO.LNFILE  LNFILE
      *     LEFT OUTER JOIN DBO.BWFILE  BWFILE
      *       ON (  LNFILE.LN_OWNBR     = BWFILE.BW_OWNBR
      *      AND    LNFILE.LN_SSNO1     = BWFILE.BW_SSNO )
      *     WHERE  ( LNFILE.LN_OWNBR        = @QLN3_WBEG_OWNBR )  
      *       AND  ( LNFILE.LN_ACCTNO      != 0 )  
      *       AND  ( LNFILE.LN_PLDATE       = '1900-01-01' 
      *        OR    LNFILE.LN_PLDATE       > (GETDATE() - 2 ))
      *       AND (( @QWS_INCLUDE_PAIDOUTS != 'N' )
      *        OR (  @QWS_INCLUDE_PAIDOUTS  = 'N'
      *       AND  ( LNFILE.LN_CURBAL      != 0
      *        OR    LNFILE.LN_INTBAL      != 0
      *        OR    LNFILE.LN_LCBAL       != 0 
      *        OR    LNFILE.LN_OTHBAL      != 0 )))

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_CRNOFL_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR,
                :QWS-INCLUDE-PAIDOUTS )
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> SSP - LN,BW 
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN3_CURSOR 
                 INTO 
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-SUBCLASS,
                  :QLN-LAWCODE,
                  :QLN-SSNO1,
                  :QLN-MAKERCD1,
                  :QLN-SSNO2,
                  :QLN-MAKERCD2,
                  :QLN-SSNO3,
                  :QLN-MAKERCD3,
                  :QLN-SSNO4,
                  :QLN-MAKERCD4,
                  :QLN-ORGST,
                  :QLN-ORGBR,
                  :QLN-OWNST,
                  :QLN-SPRCLASS,
                  :QLN-CONVERCD,
                  :QLN-MIDSTREAM,
                  :QLN-PURCD,
                  :QLN-SRCD,
                  :QLN-COLLCD,
                  :QLN-COLLID,
                  :QLN-POCD,
                  :QLN-PLCD,
                  :QLN-PLTYPE,
                  :QLN-CONFIDENTCD,
                  :QLN-ALLOTCD,
                  :QLN-RECASTCD,
                  :QLN-SKIP-TRW,
                  :QLN-STATUSFG,
                  :QLN-SMALLOAN,
                  :QLN-SALEFIN,
                  :QLN-SERVACCT,
                  :QLN-LOANTYPE,
                  :QLN-ADDON-FG,
                  :QLN-PAY-SCHLD-FG,
                  :QLN-LC-CODE,
                  :QLN-CAN-LOAN-NO,
                  :QLN-STATFLAGS,
                  :QLN-ENTDATE,                 
                  :QLN-LOANDATE,                
                  :QLN-PURDATE,                 
                  :QLN-INTDATE,                 
                  :QLN-CASHDATE,                
                  :QLN-FILEDATE,                
                  :QLN-RESCISSION-DATE,         
                  :QLN-PART-RECOURSE-DATE,      
                  :QLN-ORIG-1STPYDATE,          
                  :QLN-1STPYDATE,               
                  :QLN-JDDATE,                  
                  :QLN-PLDATE,                  
                  :QLN-PL-TRDATE,               
                  :QLN-ACCELDATE,               
                  :QLN-REDUDATE,                
                  :QLN-POFFDATE,                
                  :QLN-PO-TRDATE,               
                  :QLN-LEGALDATE,               
                  :QLN-BNKRPTDATE,              
                  :QLN-BNKRPT-DISMISS-DATE,     
                  :QLN-BNKRPT-DISCHARGE-DATE,   
                  :QLN-BNKRPT-REAFFIRM-DATE,    
                  :QLN-DEBTAIDATE,              
                  :QLN-REPODATE,                
                  :QLN-REDEEMDATE,              
                  :QLN-SOLDDATE,                
                  :QLN-INS-CONFIRM-DATE,        
                  :QLN-PRIOR-RN-FOR10-DATE,     
                  :QLN-LAST-RN-FOR10-DATE,      
                  :QLN-TRW-DOFD-DATE,           
                  :QLN-LAST-PYPA-DATE,          
                  :QLN-BT-TRDATE,               
                  :QLN-METRO2-LAST-PMT-DATE,    
                  :QLN-FOLLOWUP-DATE,           
                  :QLN-LAST-ACCRUAL-DATE,       
                  :QLN-APRATE,
                  :QLN-EFFRATE,
                  :QLN-SMPRATE,
                  :QLN-YLDRATE,
                  :QLN-OVRDRATE,
                  :QLN-ENDORATE,
                  :QLN-JDRATE,
                  :QLN-ACCRATE,
                  :QLN-REDURATE,
                  :QLN-CONRATE,
                  :QLN-SERVPER,
                  :QLN-SELLPR,
                  :QLN-DOWNPY,
                  :QLN-TRADEIN,
                  :QLN-LNAMT,
                  :QLN-FINCHG,
                  :QLN-INTCHG,
                  :QLN-SERCHG,
                  :QLN-EXTCHG,
                  :QLN-MAINTFEE,
                  :QLN-POINTS,
                  :QLN-PROCEEDS,
                  :QLN-ORGTERM,
                  :QLN-1STPYAMT,
                  :QLN-NOREGPY,
                  :QLN-REGPYAMT,
                  :QLN-LASTPYAMT,
                  :QLN-PROCASH,
                  :QLN-TILLNO,
                  :QLN-STATE-FLAG,
                  :QLN-STATE-WORKER,
                  :QLN-STATE-WORKER2,
                  :QLN-FEECODE-1,
                  :QLN-FEEAMT-1,
                  :QLN-FEEOURS-1,
                  :QLN-FEEFINCHG-1,
                  :QLN-FEECODE-2,
                  :QLN-FEEAMT-2,
                  :QLN-FEEOURS-2,
                  :QLN-FEEFINCHG-2,
                  :QLN-FEECODE-3,
                  :QLN-FEEAMT-3,
                  :QLN-FEEOURS-3,
                  :QLN-FEEFINCHG-3,
                  :QLN-FEECODE-4,
                  :QLN-FEEAMT-4,
                  :QLN-FEEOURS-4,
                  :QLN-FEEFINCHG-4,
                  :QLN-FEECODE-5,
                  :QLN-FEEAMT-5,
                  :QLN-FEEOURS-5,
                  :QLN-FEEFINCHG-5,
                  :QLN-FEECODE-6,
                  :QLN-FEEAMT-6,
                  :QLN-FEEOURS-6,
                  :QLN-FEEFINCHG-6,
                  :QLN-FEECODE-7,
                  :QLN-FEEAMT-7,
                  :QLN-FEEOURS-7,
                  :QLN-FEEFINCHG-7,
                  :QLN-FEECODE-8,
                  :QLN-FEEAMT-8,
                  :QLN-FEEOURS-8,
                  :QLN-FEEFINCHG-8,
                  :QLN-FEECODE-9,
                  :QLN-FEEAMT-9,
                  :QLN-FEEOURS-9,
                  :QLN-FEEFINCHG-9,
                  :QLN-FEECODE-10,
                  :QLN-FEEAMT-10,
                  :QLN-FEEOURS-10,
                  :QLN-FEEFINCHG-10,
                  :QLN-FEECODE-11,
                  :QLN-FEEAMT-11,
                  :QLN-FEEOURS-11,
                  :QLN-FEEFINCHG-11,
                  :QLN-FEECODE-12,
                  :QLN-FEEAMT-12,
                  :QLN-FEEOURS-12,
                  :QLN-FEEFINCHG-12,
                  :QLN-FEECODE-13,
                  :QLN-FEEAMT-13,
                  :QLN-FEEOURS-13,
                  :QLN-FEEFINCHG-13,
                  :QLN-FEECODE-14,
                  :QLN-FEEAMT-14,
                  :QLN-FEEOURS-14,
                  :QLN-FEEFINCHG-14,
                  :QLN-FEECODE-15,
                  :QLN-FEEAMT-15,
                  :QLN-FEEOURS-15,
                  :QLN-FEEFINCHG-15,
                  :QLN-FEECODE-16,
                  :QLN-FEEAMT-16,
                  :QLN-FEEOURS-16,
                  :QLN-FEEFINCHG-16,
                  :QLN-FEECODE-17,
                  :QLN-FEEAMT-17,
                  :QLN-FEEOURS-17,
                  :QLN-FEEFINCHG-17,
                  :QLN-FEECODE-18,
                  :QLN-FEEAMT-18,
                  :QLN-FEEOURS-18,
                  :QLN-FEEFINCHG-18,
                  :QLN-FEECODE-19,
                  :QLN-FEEAMT-19,
                  :QLN-FEEOURS-19,
                  :QLN-FEEFINCHG-19,
                  :QLN-FEECODE-20,
                  :QLN-FEEAMT-20,
                  :QLN-FEEOURS-20,
                  :QLN-FEEFINCHG-20,
                  :QLN-INSURED,
                  :QLN-INSTYPES,
                  :QLN-INSELIGIBLE,
                  :QLN-INSCOMP-1,
                  :QLN-INSOURS-1,
                  :QLN-INSEFF-DATE-1,
                  :QLN-INSEXP-DATE-1,
                  :QLN-INSPREM-1,
                  :QLN-INSCOVR-1,
                  :QLN-INSCOMM-1,
                  :QLN-INSANTICERN-1,
                  :QLN-INSANTICADJ-1,
                  :QLN-INSUFERN-1,
                  :QLN-INSACCERN-1,
                  :QLN-INSEARN-ORDER-1,
                  :QLN-INSFINCHG-1,
                  :QLN-INSCOMP-2,
                  :QLN-INSOURS-2,
                  :QLN-INSEFF-DATE-2,
                  :QLN-INSEXP-DATE-2,
                  :QLN-INSPREM-2,
                  :QLN-INSCOVR-2,
                  :QLN-INSCOMM-2,
                  :QLN-INSANTICERN-2,
                  :QLN-INSANTICADJ-2,
                  :QLN-INSUFERN-2,
                  :QLN-INSACCERN-2,
                  :QLN-INSEARN-ORDER-2,
                  :QLN-INSFINCHG-2,
                  :QLN-INSCOMP-3,
                  :QLN-INSOURS-3,
                  :QLN-INSEFF-DATE-3,
                  :QLN-INSEXP-DATE-3,
                  :QLN-INSPREM-3,
                  :QLN-INSCOVR-3,
                  :QLN-INSCOMM-3,
                  :QLN-INSANTICERN-3,
                  :QLN-INSANTICADJ-3,
                  :QLN-INSUFERN-3,
                  :QLN-INSACCERN-3,
                  :QLN-INSEARN-ORDER-3,
                  :QLN-INSFINCHG-3,
                  :QLN-INSCOMP-4,
                  :QLN-INSOURS-4,
                  :QLN-INSEFF-DATE-4,
                  :QLN-INSEXP-DATE-4,
                  :QLN-INSPREM-4,
                  :QLN-INSCOVR-4,
                  :QLN-INSCOMM-4,
                  :QLN-INSANTICERN-4,
                  :QLN-INSANTICADJ-4,
                  :QLN-INSUFERN-4,
                  :QLN-INSACCERN-4,
                  :QLN-INSEARN-ORDER-4,
                  :QLN-INSFINCHG-4,
                  :QLN-INSCOMP-5,
                  :QLN-INSOURS-5,
                  :QLN-INSEFF-DATE-5,
                  :QLN-INSEXP-DATE-5,
                  :QLN-INSPREM-5,
                  :QLN-INSCOVR-5,
                  :QLN-INSCOMM-5,
                  :QLN-INSANTICERN-5,
                  :QLN-INSANTICADJ-5,
                  :QLN-INSUFERN-5,
                  :QLN-INSACCERN-5,
                  :QLN-INSEARN-ORDER-5,
                  :QLN-INSFINCHG-5,
                  :QLN-INSCOMP-6,
                  :QLN-INSOURS-6,
                  :QLN-INSEFF-DATE-6,
                  :QLN-INSEXP-DATE-6,
                  :QLN-INSPREM-6,
                  :QLN-INSCOVR-6,
                  :QLN-INSCOMM-6,
                  :QLN-INSANTICERN-6,
                  :QLN-INSANTICADJ-6,
                  :QLN-INSUFERN-6,
                  :QLN-INSACCERN-6,
                  :QLN-INSEARN-ORDER-6,
                  :QLN-INSFINCHG-6,
                  :QLN-INSCOMP-7,
                  :QLN-INSOURS-7,
                  :QLN-INSEFF-DATE-7,
                  :QLN-INSEXP-DATE-7,
                  :QLN-INSPREM-7,
                  :QLN-INSCOVR-7,
                  :QLN-INSCOMM-7,
                  :QLN-INSANTICERN-7,
                  :QLN-INSANTICADJ-7,
                  :QLN-INSUFERN-7,
                  :QLN-INSACCERN-7,
                  :QLN-INSEARN-ORDER-7,
                  :QLN-INSFINCHG-7,
                  :QLN-INSCOMP-8,
                  :QLN-INSOURS-8,
                  :QLN-INSEFF-DATE-8,
                  :QLN-INSEXP-DATE-8,
                  :QLN-INSPREM-8,
                  :QLN-INSCOVR-8,
                  :QLN-INSCOMM-8,
                  :QLN-INSANTICERN-8,
                  :QLN-INSANTICADJ-8,
                  :QLN-INSUFERN-8,
                  :QLN-INSACCERN-8,
                  :QLN-INSEARN-ORDER-8,
                  :QLN-INSFINCHG-8,
                  :QLN-INSCOMP-9,
                  :QLN-INSOURS-9,
                  :QLN-INSEFF-DATE-9,
                  :QLN-INSEXP-DATE-9,
                  :QLN-INSPREM-9,
                  :QLN-INSCOVR-9,
                  :QLN-INSCOMM-9,
                  :QLN-INSANTICERN-9,
                  :QLN-INSANTICADJ-9,
                  :QLN-INSUFERN-9,
                  :QLN-INSACCERN-9,
                  :QLN-INSEARN-ORDER-9,
                  :QLN-INSFINCHG-9,
                  :QLN-INSCOMP-10,
                  :QLN-INSOURS-10,
                  :QLN-INSEFF-DATE-10,
                  :QLN-INSEXP-DATE-10,
                  :QLN-INSPREM-10,
                  :QLN-INSCOVR-10,
                  :QLN-INSCOMM-10,
                  :QLN-INSANTICERN-10,
                  :QLN-INSANTICADJ-10,
                  :QLN-INSUFERN-10,
                  :QLN-INSACCERN-10,
                  :QLN-INSEARN-ORDER-10,
                  :QLN-INSFINCHG-10,
                  :QLN-PY-YYYYMM-RECDEL,
                  :QLN-MTD-PAYS-RECDEL,
                  :QLN-GARNISH-EXTR,
                  :QLN-SHIP-SAME-FG,
                  :QLN-AD-D-INS-COVR,
                  :QLN-TOT-PARTIAL-CHGOFF,
                  :QLN-LPO,
                  :QLN-REP-CD,
                  :QLN-EMP-ID,
                  :QLN-PROCESS-CD,
                  :QLN-WCBC-CREDITLIM,
                  :QLN-FOLLOWUP-CD,
                  :QLN-MIP-FG,
                  :QLN-MIP-MTD-CLAHPAID,
                  :QLN-MIP-CLPDTH-DATE,
                  :QLN-MIP-TOT-CLPAID,
                  :QLN-MIP-AHPDTH-DATE,
                  :QLN-MIP-TOT-AHPAID,
                  :QLN-IL-GROSS-INCOME,
                  :QLN-IL-GROSS-INCOME2,
                  :QLN-RESCISSION,
                  :QLN-INTERIM-TRW,
                  :QLN-DLNO,
                  :QLN-DLPOOL,
                  :QLN-DLRECOURSE,
                  :QLN-DLPARTRCAMT,
                  :QLN-ORIGPURAMT,
                  :QLN-PURBAL,
                  :QLN-DLRVPRIN-1,
                  :QLN-DLRVPRIN-2,
                  :QLN-DLRVPRIN-3,
                  :QLN-DLRVCHGS-1,
                  :QLN-DLRVCHGS-2,
                  :QLN-DLRVCHGS-3,
                  :QLN-DLPARVHELD,
                  :QLN-DLPARVPAID,
                  :QLN-DLDISC,
                  :QLN-LCPDTH-DATE, 
                  :QLN-INTPDTH-DATE,
                  :QLN-CURBAL,
                  :QLN-INTBAL,
                  :QLN-LCBAL,
                  :QLN-OTHBAL,
                  :QLN-OT2BAL,
                  :QLN-LCPAID,
                  :QLN-LCPARTIALS,
                  :QLN-STMT-COUNTER,
                  :QLN-PRIOR-STMT-COUNTER,
                  :QLN-FROZEN-REASON,
                  :QLN-COUPON-STATEMENT,
                  :QLN-MONTHEND-BAL,
                  :QLN-ANTICTERM,
                  :QLN-ANTICERN-1,
                  :QLN-ANTICADJ-1,
                  :QLN-UFERN-1,
                  :QLN-ACCERN-1,
                  :QLN-EARN-ORDER-1,
                  :QLN-ANTICERN-2,
                  :QLN-ANTICADJ-2,
                  :QLN-UFERN-2,
                  :QLN-ACCERN-2,
                  :QLN-EARN-ORDER-2,
                  :QLN-ANTICERN-3,
                  :QLN-ANTICADJ-3,
                  :QLN-UFERN-3,
                  :QLN-ACCERN-3,
                  :QLN-EARN-ORDER-3,
                  :QLN-ANTICERN-4,
                  :QLN-ANTICADJ-4,
                  :QLN-UFERN-4,
                  :QLN-ACCERN-4,
                  :QLN-EARN-ORDER-4,
                  :QLN-ANTICERN-5,
                  :QLN-ANTICADJ-5,
                  :QLN-UFERN-5,
                  :QLN-ACCERN-5,
                  :QLN-EARN-ORDER-5,
                  :QLN-ANTICERN-6,
                  :QLN-ANTICADJ-6,
                  :QLN-UFERN-6,
                  :QLN-ACCERN-6,
                  :QLN-EARN-ORDER-6,
                  :QLN-ANTICERN-7,
                  :QLN-ANTICADJ-7,
                  :QLN-UFERN-7,
                  :QLN-ACCERN-7,
                  :QLN-EARN-ORDER-7,
                  :QLN-ANTICERN-8,
                  :QLN-ANTICADJ-8,
                  :QLN-UFERN-8,
                  :QLN-ACCERN-8,
                  :QLN-EARN-ORDER-8,
                  :QLN-ANTICERN-9,
                  :QLN-ANTICADJ-9,
                  :QLN-UFERN-9,
                  :QLN-ACCERN-9,
                  :QLN-EARN-ORDER-9,
                  :QLN-ANTICERN-10,
                  :QLN-ANTICADJ-10,
                  :QLN-UFERN-10,
                  :QLN-ACCERN-10,
                  :QLN-EARN-ORDER-10,
                  :QLN-LMO-PRIOR-INTEREST,
                  :QLN-PRIOR-UNEARNED,
                  :QLN-ERNDELQ-STOP,
                  :QLN-NONACRERN,
                  :QLN-PRIOR-NONACRERN,
                  :QLN-ERN-IB-PDTH-DATE,
                  :QLN-ERN-IB-PRINBAL,
                  :QLN-ERN-IB-INTOWE,
                  :QLN-TOTERN,
                  :QLN-TOTOTH,
                  :QLN-TOTINT,
                  :QLN-TOTLCHG,
                  :QLN-TOTDEF,
                  :QLN-TOTSERACCT,
                  :QLN-LCWAIVERS,
                  :QLN-INTWAIVERS,
                  :QLN-TOTPAYMNTD,
                  :QLN-TOTEXCPAYMNTD,
                  :QLN-TOTNODEF,
                  :QLN-YTDNODEF,
                  :QLN-TOTNODEFACT,
                  :QLN-YTDNODEFACT,
                  :QLN-TOTNOALDEL,
                  :QLN-YTDNOALDEL,
                  :QLN-YTDINTPD,
                  :QLN-LYRINTPD,
                  :QLN-TOTINTPD,
                  :QLN-RECASTINT,
                  :QLN-RECASTPRIN,
                  :QLN-CL-REBATE,
                  :QLN-AH-REBATE,
                  :QLN-PP-REBATE,
                  :QLN-INT-REBATE,
                  :QLN-SC-REBATE,
                  :QLN-MF-REBATE,
                  :QLN-DF-REBATE,
                  :QLN-EX-REBATE,
                  :QLN-O1-REBATE,
                  :QLN-O2-REBATE,
                  :QLN-O3-REBATE,
                  :QLN-O4-REBATE,
                  :QLN-O5-REBATE,
                  :QLN-O6-REBATE,
                  :QLN-O7-REBATE,
                  :QLN-UN-O7-REB,
                  :QLN-UN-O8-REB,
                  :QLN-UN-O9-REB,
                  :QLN-UN-O10-REB,
                  :QLN-PLREASON,
                  :QLN-COLLECTOR,
                  :QLN-ACTIONCD,
                  :QLN-ACTIONCD2,
                  :QLN-ATCODE,
                  :QLN-RECODE,
                  :QLN-ESCROW-FG,
                  :QLN-PROMDAYS-PASTDUE,
                  :QLN-PROMID,
                  :QLN-PROMDATE,
                  :QLN-PROMAMT,
                  :QLN-PROMDATE2,
                  :QLN-DATE-PAID-LAST,
                  :QLN-AMT-PAID-LAST,
                  :QLN-LASTCFORMNO,
                  :QLN-LASTCFORMDATE,
                  :QLN-DELCNT,
                  :QLN-DELCNT2,
                  :QLN-CREDITLIM,
                  :QLN-CRSCORE,
                  :QLN-LASTSFORMNO,
                  :QLN-LASTSFORMDATE,
                  :QLN-SOLCNT,
                  :QLN-MESSAGE,
                  :QLN-OLDACCTNO,
                  :QLN-PRLNNO,
                  :QLN-PRBAL,
                  :QLN-PRINTDUE,
                  :QLN-PRFINCHGRB,
                  :QLN-PRINSRB,
                  :QLN-PRPOCD,
                  :QLN-PRPODATE,
                  :QLN-PRDELINQAMT,
                  :QLN-PRLNAMT,
                  :QLN-PRPDUE-DAYS,
                  :QLN-PRDLDISC-UNEARN,
                  :QLN-PRCONPROFILE,
                  :QLN-SUM-ORIGPURAMT,
                  :QLN-SUM-LNAMT,
                  :QLN-SUM-FINCHG,
                  :QLN-SUM-INTCHG,
                  :QLN-SUM-ORGTERM,
                  :QLN-SUM-REGPYAMT,
                  :QLN-SUM-EXTCHG,
                  :QLN-SUM-SERCHG,
                  :QLN-SUM-MAINTFEE,
                  :QLN-UNITPER-CD,
                  :QLN-UNITPER-FREQ,
                  :QLN-DAYS-IN-UNITPER,
                  :QLN-UNITPER-PER-YEAR,
                  :QLN-REPOCD,
                  :QLN-BR-TFR-FG,
                  :QLN-O3INS-DLPROC,
                  :QLN-INSPREM-DLADJ,
                  :QLN-PLAMT,
                  :QLN-MTD-PRIN,
                  :QLN-MTD-LC,
                  :QLN-MTD-INT,
                  :QLN-MTD-OT,
                  :QLN-MTD-EX,
                  :QLN-BNKRPT-CHAPTER,
                  :QLN-BMO-CON-STATUS,
                  :QLN-BMO-REC-STATUS,
                  :QLN-APP-NUMBER,
                  :QLN-GIFTNET,
                  :QLN-GIFTNET-OUR-PO,
                  :QLN-PROC-DISBURSED,
                  :QLN-NOODDPY,
                  :QLN-CONPROFILE-1,
                  :QLN-CONPROFILE-2,
                  :QLN-CONPROFILE-3,
                  :QLN-CONPROFILE-4,
                  :QLN-CONPROFILE-5,
                  :QLN-RECPROFILE-1,
                  :QLN-RECPROFILE-2,
                  :QLN-RECPROFILE-3,
                  :QLN-RECPROFILE-4,
                  :QLN-RECPROFILE-5,
                  :QLN-TRW-STATUS,
                  :QLN-ORIGIN,
                  :QLN-PO-TRCD,
                  :QLN-NEXIS-CRSCORE,
                  :QLN-TRW-B-CONSUMER-IND,
                  :QLN-PARVHELD-PAID-FG,
                  :QLN-ACCT-FROZEN,
                  :QLN-TRW-SPECIAL-CODE,
                  :QLN-CREATION-STATUS,
                  :QLN-TOTNOLCHG,
                  :QLN-LBOX,
                  :QLN-EXTRA-PRIN-LFP,
                  :QLN-REMAIN-INT-SINCE-LFP,
                  :QLN-REMAIN-PRIN-SINCE-LFP,
                  :QLN-INTERIM-COUPON,
                  :QLN-DLCOMM-O3-CMSI,
                  :QLN-DLCOMM-PP-CMSI,
                  :QLN-TRW-J-CONSUMER-IND,
                  :QLN-ACH,
                  :QLN-1099-C-FG,
                  :QLN-TRW-COMPLIANCE-CODE,
                  :QLN-UNWIND-REASON-CODE,
                  :QLN-PBI-PBD-FG,
                  :QLN-OLD-ACCT-FORMAT-FG,
                  :QLN-APPROVED-BY-USER,
                  :QLN-APPROVED-BY-CODE,
                  :QLN-APPROVED-BY-DATE,
                  :QLN-APPROVED-BY-TIME,
                  :QLN-LTOUCH-DATE,
                  :QLN-LTOUCH-TIME,
                  :QLN-LOS-APP-ID,
                  :QBW-OWNBR, 
                  :QBW-SSNO,  
                  :QBW-LNAME, 
                  :QBW-LAST-RN-FOR10-DATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.
      *        PERFORM GET-NO-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       CLEAR-NOFILE-WORK-TABLE.    
           
           MOVE "2" TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.CRNOFL_NOFILE
            WHERE WRKNO_KEY = :WS-WKNO-KEY 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *=================================================================
       OPEN-REPORT-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE REPORT-IFN TO E-FILE.
           OPEN OUTPUT REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE-OUTPUT.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE-OUTPUT.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       OPEN-REPORT-FILE.

           PERFORM OPEN-IT.
           MOVE REPORT-IFN TO E-FILE.
           OPEN I-O REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE.

           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE.

           PERFORM READ-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           READ REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE-NEXT.

           PERFORM READ-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           READ REPORT-FILE NEXT.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE-NEXT.

      *-----------------------------------------------------------------
       START-REPORT-FILE.

           PERFORM START-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           START REPORT-FILE KEY NOT < REPORT-KEY.
           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       WRITE-REPORT-FILE.

           PERFORM WRITE-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           WRITE REPORT-REC.

           IF ( IO-LOCKED )
              GO TO WRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       REWRITE-REPORT-FILE.

           PERFORM REWRITE-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           REWRITE REPORT-REC.

           IF ( IO-LOCKED )
              GO TO REWRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       CLOSE-REPORT-FILE.

           PERFORM CLOSE-IT.
           CLOSE REPORT-FILE.

      *=================================================================
       OPEN-LOG-FILE-EXTEND.
           OPEN EXTEND LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-EXTEND.

       OPEN-LOG-FILE-OUTPUT.
           OPEN OUTPUT LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-OUTPUT.
       WRITE-LOG-FILE.
           WRITE LOG-REC.
       CLOSE-LOG-FILE.
           CLOSE LOG-FILE.

      *=================================================================

       OPEN-WKNO-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WKNO-PATH TO E-FILE.
           OPEN OUTPUT WKNO-FILE.
           IF IO-FG = 8
              GO TO OPEN-WKNO-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WKNO-FILE
              GO TO OPEN-WKNO-FILE-OUTPUT.
       WRITE-WKNO-FILE.
           PERFORM WRITE-IT.
           MOVE WKNO-PATH TO E-FILE.
           WRITE WKNO-REC.
           IF IO-FG = 8
             GO TO WRITE-WKNO-FILE.
       CLOSE-WKNO-FILE.
           PERFORM CLOSE-IT.
           CLOSE WKNO-FILE.

       OPEN-WKNO-RESULT-FILE.
           PERFORM OPEN-IT.
           MOVE WKNO-PATH-RESULT-FILE TO E-FILE.
           OPEN INPUT WKNO-RESULT-FILE.
       READ-WKNO-RESULT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WKNO-PATH-RESULT-FILE TO E-FILE.
           READ WKNO-RESULT-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-WKNO-RESULT-FILE-NEXT.
       CLOSE-WKNO-RESULT-FILE.
           PERFORM CLOSE-IT.
           CLOSE WKNO-RESULT-FILE.

      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE REPORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME
           ELSE
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE SPACES TO MESS
                 PERFORM SEND-LEGEND.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CRNOFL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
