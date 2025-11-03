       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RZ1DA4.
       AUTHOR.        KEC.
       DATE-WRITTEN.  04-14-1999.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/AC1DAY.C
      *
      * ALL INTERNAL SQL (NO SP) - LIMITED COLUMNS - NO ISNULL CODE 
      *
      * DESCRIPTION   :  MONTHLY EARNINGS ACCRUAL - IB ACCRUAL
      *                  DAILY REPORT WO/ UPDATES - BATCH ONLY ?
      *
      * NOTE: AD2DAY CREATES THE EXTRACT FILES, NOT THIS PROGRAM
      *
      * **** NOTE ****:  AS OF 03-12-2001 (KEC), THIS PROGRAM IS NO
      *                  LONGER TIED TO LIBLP/AC1DVR.  IT IS SEPERATE
      *                  FROM LP/AC1ERN.C AND CONTAINS ITS OWN LOGIC.
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 990414 {PR#00830} FRONTIER FINANCE [JUDY]
      *             ***NOTE*** TOOK 'LP/AC1ERN.C' AND CUT IT UP INTO
      *             COPY MEMBERS AND A NEW PROGRAM 'LP/AC1DAY.C' SO TO
      *             HAVE THE ACCURAL REPORTS BE GENERATED FROM A MONTHLY
      *             OR DAILY DRIVEN PROGRAMS.
      *
      *            'LIBLP/AC1DVR'   = AC1 DRIVER ROUTINES
      *            'LIBLP/AC1DVRW'  = AC1 DRIVER WORKERS
      *               'LP/AC1ERN.C' = MONTHLY W/  UPDATES
      *               'LP/AC1DAY.C' = DAILY   WO/ UPDATES
      *
      * GLF 990923 {PR#00000} PARADATA FINANCIAL [JOHN]
      *             REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER
      *             DEVELPOMENT MEETING (082599)
      *
      * MJD 000601 BROKE AC1DCRW INTO TWO COPY MEMBERS:
      *                AC1DVR.FIL - FILE SECTION
      *                AC1DVR.WKS - WORKING STORAGE SECTION
      *            THIS NEEDED TO BE DONE BECAUSE THE SCCS IDENTIFIER
      *            LINE NEEDS TO BE PHYSICALLY IN THE DOT C FOR IT TO
      *            BE UPDATED.
      *
      * KEC 010105 {PR#00000} PARADTA FINANCIAL [JIM ROSENAUER]
      *             WORKING ON A PROBLEM JIM FOUND WHERE EXT-CONAME-RPT-LINES,
      *             CONAME, SYSDATE AND USERID WERE HAVING ZEROES AND
      *             SPACES IN THEM WHEN RUNNING BATCH.  AFTER MUCH
      *             CONFUSION WITH HOW THE 'FLDVAL' OF SYSINFO-BUF
      *             WORKS IN LIBGB/EOINIT (HOW THINGS GET STORED),
      *             BETH AND I FOUND (IN DEBUG) THAT THE 'XFER' OF
      *             SYSINFO-BUF IN THE PROCEDURE DIVISION (BEFORE
      *             MAIN-PROGRAM SECTION) WAS CAUSING THE PROBLEM.
      *             I STILL DO NOT UNDERSTAND HOW ALL OF THIS BATCH
      *             STUFF WORKS BUT I AM JUST GOING TO ACCEPT IT.
      *             I COMMENTED OUT THE 'XFER' OF SYSINFO-BUF IN THE
      *             PROCEDURE-DIVISION FOR THIS WILL NOT EFFECT HOW
      *             THE ACCRUAL WORKS (DOES NOT TOUCH LP/AC1ERN.C).
      * CS  010223  FIRST-TIME FLAG WAS NOT RESET TO "Y" WITH EACH NEW
      *             BRANCH, ALL HEADINGS PRINTED THE FIRST BRANCH
      * CS  010227  ADDED A SUMMARY ONLY PRINT OPTION. ALL SUMMARY TOTALS
      *             BRANCH,SUMMARY,CORPORATE TOTALS ARE WITHIN AC1DAY.C,PR#1402
      *- - - - - - - - - - - - - - - - - - - - - - - -{ **** NOTE **** }
      * KEC 010312 {PR#00000} MERCURY FINANCE [KEVIN MULLEN]
      *             NEED TO HAVE A POOL ID RANGE (BEG/END) AND TOTALS.
      *             BECAUSE OF THE COMPLEXITY OF THIS PROGRAM, I HAVE
      *             SEPERATED LP/AC1DAY.C FROM LP/AC1ERN.C.  THIS MEANS
      *             I HAVE HARD CODED THE FOLLOWING INTO THIS:
      *
      *               LIBLP/AC1DVR
      *               LIBLP/AC1DVR.FIL
      *               LIBLP/AC1DVR.WKS
      *- - - - - - - - - - - - - - - - - - - - - - - -{ **** NOTE **** }
      * CS  010316 IB ACCOUNTS ARE NOW BEING PROCESSED.  ADDDED TOTAL OF
      *            IB'S.
      *
      * KEC 010402 {PR#00000} PARADATA FINANICAL [KEN]
      *             REVISED THE ENTIRE PROGRAM.  CLEANED IT UP SO TO
      *             MAKE IT EASIER TO UNDERSTAND AND CHANGE.  THE REPORT
      *             LAYOUT REMAIN THE SAME (EXCEPT FOR A SLIGHT CHANGE
      *             TO THE HEADER).
      *
      * KEC 081106 {PR#00000} PARADATA FINANICAL [KEN]
      *            HAD TO INCLUDE LOCAL-FILE-PATHS FOR THE LINKAGE TO RVCALC
      *            THAT GAIL B DID BACK IN 2001.
      *  CS 081106 ADDED TEST FOR GP-INCL-OTH-IN-EARNED-R. IF 'R' THEN
      *            DONT' INCLUDE IN EARNED/UNEARNED OTHER(6) OR MAINTFEE(4)
      *                                             REGACC PR#210
      * KEC 090330 {PR#00000} PARADATA FINANICAL [KEN]
      *            THIS RELATES BACK TO MY CHANGE ON '081106'.
      *            THERE WAS A PROBLEM OF GETTING NO DATA WHEN RUNNING
      *            THIS REPORT AT PFSIMO & CUSTOMERS MACHINE.
      *            I COMMENTED OUT THE SETTING OF A ERROR IN GETTING
      *            THE LOCAL FILE PATH FOR RS, RT AND WKRS IF THEY
      *            DID NOT EXIST.
      *
      **********   IF AN ACCOUNT IS BOTH A REPO AND A PL, WILL PRINT AS
      **********   P&L TOTALS, AND NOT AS REPO TOTALS
      * NOTE ON REVOLVING LOAN ACCRUAL
      * CS 090622  TO CALCULATE THE ACCRUAL AMOUNTS CL/RVCALC IS CALLED. RVCALC
      *            BUILDS THE WKRS???? FOR THE MONTH/YEAR. THESE FIELDS ARE
      *            THEN PASSED BACK TO THE CALLING PROGRAM VIA RVCALCW-WORKERS.
      *
      *            HERE'S THE PROBLEM, AC9DAY WONT RUN IF THE WKRS DOES NOT
      *            EXIT, AND IF IT EXISTS, IT ADDS TO THE AMOUNTS IN THE WKRS
      *            YIELDING BAD ACCRUAL NUMBERS. AC1DAY RUNS ACCURATELY IF THE
      *            WKRS DOES NOT EXIST, BUT ITS WRONG ON ANY RUN WHERE THE
      *            WKRS EXISTS.
      *
      *            RVSTUP(REVOLVING STATMENT RUN) OPENS TO OUTPUT, WHICH IS
      *            OK UNLESS YOU ARE ALSO RUNNING THE CAPITALIZATION(RVCAPT)
      *            PROGRAM AT THE SAME TIME WHICH ALSO OPENS THE WKRS FILE TO
      *            OUTPUT.
      *
      *            AC1ERN ALSO OPEN OUTPUT ON THE WKRS FILE.
      *
      *            SO, I AM USING A DIFFERENT PATH FOR LNAGIB,AC1DAY,AC9DAY,
      *            AND THE CAPITALIZATION(RVCAPT) PROGRAMS
      *            I AM PERFORMING MKTEMP AND MOVING MKTEMP-BUF TO WK-PATH
      *            AND WKRS-PATH   PR#3374
      *
      *  090622   ALSO FOUND A PROBLEM WITH 'INTEREST OWED' ON REVOVLING
      *           WAS USING LN-INTBAL WHICH NOW HAS THE LAST ACCRUAL AMOUNT
      *           INCLUDED SO MUST BE SUBTRACTED OUT
      * BLM 100122 REMOVED RM-CMD FROM ACRERN.CMD, CHANGE TO REMOVE-WORKFILE
      *            LOGIC IN LIBGB/RMFILE & RM.CMD, NEED TO REMOVE .VIX FILES
      *  CS 100527 JR CALLED RAN AC1DAY&AC9DAY AT REGIONAL & IMMEDIATELY GOT
      *            NO RECORDS, WAS CAUSED BY THE ACCESS CHECK ON RS & RT.  THE
      *            RS IS NOT USED AND IF REVOLVING CL/RVCALC OPENS RT TO IO
      *            SO I COMMENTED OUT THIS CHECK.  ALSO AC9DAY APPEARS TO BE
      *            THE SAME AS AC1DAY SO PER JR REMOVED CO-AC9DAY,FM-/US2/SRC
      *            A15/FM.OLD/LP/AC9DAY.100527 & MOVED SRC TO AC1DAY.NO   JR
      * KEC 130313 {PR#00000} PARADATA [JIM ROSENAUER]
      *            FIXED A PROBLEM WHERE PROGRAM WAS NOT RUNNING
      *            CORRECTLY FOR GROUP LOGIC DUE TO MISSING LINE:
      *               MOVE WGR-SRESULT TO BR-NO.
      * AMM 2014.0820 {PL#0857} FIXED RT AND RS PATHS NOT BEING SET
      *               PROPERLY
      *   CS 150721 CHANGED WKRS-KEY (RVCALC) TO INCLUDE A SEQUENCE, NECESSARY
      *             FOR DUPLCIATE SPECIAL PROGRAM WKRS-KEY, ALSO ADDED WKRS-
      *             RT-SEQ   REGENCY PL887
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0311 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-POOL???? TO CD-PL-????
      *
      * KEC 2017.0223 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED REPORT-CLASS FIELDS TO MATCH LN-CLASS/LC-CODE.
      * BAH 170316 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 170316 ADDED LTR-REC TO RVCALC LINKAGE LIKE AC1ERN
      *  CS 170913 FIXED DISPLAY-ACCRUAL-DATE WAS DEFAULTING ZEROS
      *     INSTEAD OF SYSTEM DATE
      *
      * KEC 2018.0424 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      * BAH 181010 GLOBAL BWFILE, L2G
      * BAH 181010 GLOBAL DLFILE, L2G
      * BAH 181203 GLOBAL LNFILE, L2G
      * BAH 181221 GLOBAL LPFILE, L2G
      *  CS 190103 COMMENT LOCAL-LTI, LOCAL-LTP, KEN AND I LOOKED AND
      *            DIDNT SEE WAS BEING USED
      *  CS 190103 GLOBAL LTIFILE
      * KEC 2019-0131 CC32 - REMOVED THE REVOLVING CREDIT USAGE.
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20190703 REMOVED LPWSWKRS, PT0153
      * BAH 20190703 REMOVED LTR PT0150
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE, BW, LTI,DL,LP
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT REPORT-FILE ASSIGN REPORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY REPORT-KEY
                  FILE STATUS FILE-STAT.

      *-----------------------------------------------------------------
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".


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
               05  REPORT-TYPE                       PIC X(03).
                   88  REPORT-TYPE-IB                    VALUE "IB ".
                   88  REPORT-TYPE-IBX                   VALUE "IBX".
                   88  REPORT-TYPE-PC                    VALUE "PC ".

               05  REPORT-BRANCH                     PIC 9(05).
                   88  REPORT-GRAND-TOTAL                VALUE 99999.

               05  REPORT-POOL                       PIC 9(06).
                   88  REPORT-BRANCH-TOTAL               VALUE 999999.

               05  REPORT-CLASS                      PIC 9(04).
                   88  REPORT-POOL-TOTAL                 VALUE 9999.

               05  REPORT-ACCTNO                     PIC 9(08).
                   88  REPORT-CLASS-TOTAL                VALUE 99999999.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  REPORT-REC-INFO                       PIC X(170).

      *-------------------------------------------------{ INT. BEARING }
           03  REPORT-IB-REC-INFO          REDEFINES REPORT-REC-INFO.
               05  REPORT-IB-DISPLAY-LNNO            PIC X(16).
               05  REPORT-IB-LNAME                   PIC X(16).
               05  REPORT-IB-FNAME                   PIC X(16).
               05  REPORT-IB-FROM-DATE               PIC 9(08).
               05  REPORT-IB-INT-SINCE-LSTPY         PIC S9(9)V99.
               05  REPORT-IB-INTBAL                  PIC S9(9)V99.
               05  REPORT-IB-TRUE-ACCRUE             PIC S9(9)V99.
               05  REPORT-IB-ACCRUED                 PIC S9(9)V99.
               05  REPORT-IB-MAX                     PIC X(01).
               05  REPORT-IB-NUMBER-ACCTS            PIC 9(07).
               05  FILLER                            PIC X(62).

      *-------------------------------------{ INT. BERARING EXCEPTIONS }
           03  REPORT-IBX-REC-INFO         REDEFINES REPORT-REC-INFO.
               05  REPORT-IBX-DISPLAY-LNNO           PIC X(16).
               05  REPORT-IBX-LNAME                  PIC X(16).
               05  REPORT-IBX-FNAME                  PIC X(16).
               05  REPORT-IBX-INT-PDTH-DATE          PIC 9(8).
               05  REPORT-IBX-INT-SINCE-LSTPY        PIC S9(9)V99.
               05  REPORT-IBX-INT-OWED               PIC S9(9)V99.
               05  REPORT-IBX-TRUE-ACCRUE            PIC S9(9)V99.
               05  REPORT-IBX-INCOME-ACCRUE          PIC S9(9)V99.
               05  REPORT-IBX-BALANCE                PIC S9(9)V99.
               05  REPORT-IBX-CONTR-PDTH-DATE        PIC 9(8).
               05  REPORT-IBX-CONTR-STATUS           PIC 9(03).
               05  REPORT-IBX-NUMBER-ACCTS           PIC 9(07).
               05  FILLER                            PIC X(41).

      *---------------------------------------------------{ PRECOMPUTE }
           03  REPORT-PC-REC-INFO          REDEFINES REPORT-REC-INFO.
               05  REPORT-PC-DISPLAY-LNNO            PIC X(16).
               05  REPORT-PC-LNAME                   PIC X(16).
               05  REPORT-PC-FNAME                   PIC X(16).
               05  REPORT-PC-ANTERM                  PIC 9(03).
               05  REPORT-PC-ANTIC                   PIC S9(9)V99.
               05  REPORT-PC-UNEARN                  PIC S9(9)V99.
               05  REPORT-PC-INT                     PIC S9(9)V99.
               05  REPORT-PC-ADDON                   PIC S9(9)V99.
               05  REPORT-PC-SRV                     PIC S9(9)V99.
               05  REPORT-PC-MFEE                    PIC S9(9)V99.
               05  REPORT-PC-DISC                    PIC S9(9)V99.
               05  REPORT-PC-OTHER                   PIC S9(9)V99.
               05  REPORT-PC-CASH-EARN               PIC S9(9)V99.
               05  REPORT-PC-ENDING                  PIC S9(9)V99.
               05  REPORT-PC-NUMBER-ACCTS            PIC 9(07).
               05  FILLER                            PIC X(02).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZ1DAY S35 06/14/23-06:09:35 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
      * COPY "LIBLP/LP01LN_SQL.CPY".
      * COPY "LIBLP/LP01LN_SQL.CPY".
      * COPYMEMBER: LIBLP/LP01LN_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN MASTER FILE
      *
      *=================================================================
      * REV:
      * JKC 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  LN1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN1-CURSOR-STAT-GOOD              VALUE "00".
           88  LN1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN3-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN3-CURSOR-STAT-GOOD              VALUE "00".
           88  LN3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN4-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN4-CURSOR-STAT-GOOD              VALUE "00".
           88  LN4-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN5-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN5-CURSOR-STAT-GOOD              VALUE "00".
           88  LN5-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".


      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LN1-CURSOR-GR-STAT        PIC X(2)    VALUE SPACES.
           88  LN1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  LN1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN3-CURSOR-GR-STAT        PIC X(2)    VALUE SPACES.
           88  LN3-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  LN3-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{NOT GREATER CURSOR FOR START-NOT-GREATER & READ-PREVIOUS}
       01  LN4-CURSOR-NG-STAT        PIC X(2)    VALUE SPACES.
           88  LN4-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LN4-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN5-CURSOR-NG-STAT        PIC X(2)    VALUE SPACES.
           88  LN5-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LN5-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".


       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LN-COMMAND                PIC X(8192) VALUE SPACES.

       01  RET-CODE                                  PIC S9(4) COMP-5.  

       01  QLN1-WHERE-BEG-KEY.
           03  QLN1-WBEG-OWNBR                       PIC 9(4).
           03  QLN1-WBEG-ACCTNO                      PIC 9(8).

       01  QLN1-WHERE-END-KEY.
           03  QLN1-WEND-OWNBR                       PIC 9(4).
           03  QLN1-WEND-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-BEG-KEY.
           03  QLN3-WBEG-OWNBR                       PIC 9(4).
           03  QLN3-WBEG-CLASS                       PIC 9(3).
           03  QLN3-WBEG-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-END-KEY.
           03  QLN3-WEND-OWNBR                       PIC 9(4).
           03  QLN3-WEND-CLASS                       PIC 9(3).
           03  QLN3-WEND-ACCTNO                      PIC 9(8).

       01  QLN4-WHERE-BEG-KEY.
           03  QLN4-WBEG-OWNBR                       PIC 9(4).
           03  QLN4-WBEG-SSNO1                       PIC 9(9).

       01  QLN4-WHERE-END-KEY.
           03  QLN4-WEND-OWNBR                       PIC 9(4).
           03  QLN4-WEND-SSNO1                       PIC 9(9).

       01  QLN5-WHERE-BEG-KEY.
           03  QLN5-WBEG-OWNBR                       PIC 9(4).
           03  QLN5-WBEG-SSNO2                       PIC 9(9).

       01  QLN5-WHERE-END-KEY.
           03  QLN5-WEND-OWNBR                       PIC 9(4).
           03  QLN5-WEND-SSNO2                       PIC 9(9).


       01  QLN-REC.
           03  QLN-OWNBR                             PIC 9(4).
           03  QLN-ACCTNO                            PIC 9(8).
           03  QLN-CLASS                             PIC 9(3).
           03  QLN-SUBCLASS                          PIC 99.
           03  QLN-LAWCODE                           PIC 99.
           03  QLN-SSNO1                             PIC 9(9).
           03  QLN-MAKERCD1                          PIC X.
           03  QLN-SSNO2                             PIC 9(9).
           03  QLN-MAKERCD2                          PIC X.
           03  QLN-SSNO3                             PIC 9(9).
           03  QLN-MAKERCD3                          PIC X.
           03  QLN-SSNO4                             PIC 9(9).
           03  QLN-MAKERCD4                          PIC X.
           03  QLN-ORGST                             PIC XX.
           03  QLN-ORGBR                             PIC 9(4).
           03  QLN-OWNST                             PIC XX.
           03  QLN-SPRCLASS                          PIC 9(3).
           03  QLN-CONVERCD                          PIC X.
           03  QLN-MIDSTREAM                         PIC X.
           03  QLN-PURCD                             PIC XXX.
           03  QLN-SRCD                              PIC XX.
           03  QLN-COLLCD                            PIC X(2).
           03  QLN-COLLID                            PIC X(20).
           03  QLN-POCD                              PIC XX.
           03  QLN-PLCD                              PIC X.
           03  QLN-PLTYPE                            PIC X.
           03  QLN-CONFIDENTCD                       PIC X.
           03  QLN-ALLOTCD                           PIC X.
           03  QLN-RECASTCD                          PIC X.
           03  QLN-SKIP-TRW                          PIC X.
           03  QLN-STATUSFG                          PIC X.
           03  QLN-SMALLOAN                          PIC X.
           03  QLN-SALEFIN                           PIC X.
           03  QLN-SERVACCT                          PIC X.
           03  QLN-LOANTYPE                          PIC X.
           03  QLN-ADDON-FG                          PIC X.
           03  QLN-PAY-SCHLD-FG                      PIC X.
           03  QLN-LC-CODE                           PIC 99.
           03  QLN-CAN-LOAN-NO                       PIC XX.
           03  QLN-STATFLAGS                         PIC X(20).
           03  QLN-ENTDATE                           PIC X(10).
           03  QLN-LOANDATE                          PIC X(10).
           03  QLN-PURDATE                           PIC X(10).
           03  QLN-INTDATE                           PIC X(10).
           03  QLN-CASHDATE                          PIC X(10).
           03  QLN-FILEDATE                          PIC X(10).
           03  QLN-RESCISSION-DATE                   PIC X(10).
           03  QLN-PART-RECOURSE-DATE                PIC X(10).
           03  QLN-ORIG-1STPYDATE                    PIC X(10).
           03  QLN-1STPYDATE                         PIC X(10).
           03  QLN-JDDATE                            PIC X(10).
           03  QLN-PLDATE                            PIC X(10).
           03  QLN-PL-TRDATE                         PIC X(10).
           03  QLN-ACCELDATE                         PIC X(10).
           03  QLN-REDUDATE                          PIC X(10).
           03  QLN-POFFDATE                          PIC X(10).
           03  QLN-PO-TRDATE                         PIC X(10).
           03  QLN-LEGALDATE                         PIC X(10).
           03  QLN-BNKRPTDATE                        PIC X(10).
           03  QLN-BNKRPT-DISMISS-DATE               PIC X(10).
           03  QLN-BNKRPT-DISCHARGE-DATE             PIC X(10).
           03  QLN-BNKRPT-REAFFIRM-DATE              PIC X(10).
           03  QLN-DEBTAIDATE                        PIC X(10).
           03  QLN-REPODATE                          PIC X(10).
           03  QLN-REDEEMDATE                        PIC X(10).
           03  QLN-SOLDDATE                          PIC X(10).
           03  QLN-INS-CONFIRM-DATE                  PIC X(10).
           03  QLN-PRIOR-RN-FOR10-DATE               PIC X(10).
           03  QLN-LAST-RN-FOR10-DATE                PIC X(10).
           03  QLN-TRW-DOFD-DATE                     PIC X(10).
           03  QLN-LAST-PYPA-DATE                    PIC X(10).
           03  QLN-BT-TRDATE                         PIC X(10).
           03  QLN-METRO2-LAST-PMT-DATE              PIC X(10).
           03  QLN-FOLLOWUP-DATE                     PIC X(10).
           03  QLN-LAST-ACCRUAL-DATE                 PIC X(10).
           03  QLN-APRATE                            PIC 999V999.
           03  QLN-EFFRATE                           PIC 999V999.
           03  QLN-SMPRATE                           PIC 999V999.
           03  QLN-YLDRATE                           PIC 999V999.
           03  QLN-OVRDRATE                          PIC 999V999.
           03  QLN-ENDORATE                          PIC 999V999.
           03  QLN-JDRATE                            PIC 999V999.
           03  QLN-ACCRATE                           PIC 999V999.
           03  QLN-REDURATE                          PIC 999V999.
           03  QLN-CONRATE                           PIC 999V999.
           03  QLN-SERVPER                           PIC 9(3)V99.
           03  QLN-SELLPR                            PIC 9(7)V99.
           03  QLN-DOWNPY                            PIC 9(7)V99.
           03  QLN-TRADEIN                           PIC S9(7)V99.
           03  QLN-LNAMT                             PIC 9(7)V99.
           03  QLN-FINCHG                            PIC 9(7)V99.
           03  QLN-INTCHG                            PIC 9(7)V99.
           03  QLN-SERCHG                            PIC 9(5)V99.
           03  QLN-EXTCHG                            PIC 9(5)V99.
           03  QLN-MAINTFEE                          PIC 9(5)V99.
           03  QLN-POINTS                            PIC 99V99.
           03  QLN-PROCEEDS                          PIC S9(7)V99.
           03  QLN-ORGTERM                           PIC 9(3).
           03  QLN-1STPYAMT                          PIC 9(7)V99.
           03  QLN-NOREGPY                           PIC 9(3).
           03  QLN-REGPYAMT                          PIC 9(7)V99 .
           03  QLN-LASTPYAMT                         PIC S9(7)V99.
           03  QLN-PROCASH                           PIC S9(7)V99.
           03  QLN-TILLNO                            PIC 9(2).
           03  QLN-STATE-FLAG                        PIC X.
           03  QLN-STATE-WORKER                      PIC S9(7)V99.
           03  QLN-STATE-WORKER2                     PIC S9(5)V99.
           03  QLN-FEECODE-1                         PIC XX.
           03  QLN-FEEAMT-1                          PIC 9(5)V99.
           03  QLN-FEEOURS-1                         PIC X.
           03  QLN-FEEFINCHG-1                       PIC X.
           03  QLN-FEECODE-2                         PIC XX.
           03  QLN-FEEAMT-2                          PIC 9(5)V99.
           03  QLN-FEEOURS-2                         PIC X.
           03  QLN-FEEFINCHG-2                       PIC X.
           03  QLN-FEECODE-3                         PIC XX.
           03  QLN-FEEAMT-3                          PIC 9(5)V99.
           03  QLN-FEEOURS-3                         PIC X.
           03  QLN-FEEFINCHG-3                       PIC X.
           03  QLN-FEECODE-4                         PIC XX.
           03  QLN-FEEAMT-4                          PIC 9(5)V99.
           03  QLN-FEEOURS-4                         PIC X.
           03  QLN-FEEFINCHG-4                       PIC X.
           03  QLN-FEECODE-5                         PIC XX.
           03  QLN-FEEAMT-5                          PIC 9(5)V99.
           03  QLN-FEEOURS-5                         PIC X.
           03  QLN-FEEFINCHG-5                       PIC X.
           03  QLN-FEECODE-6                         PIC XX.
           03  QLN-FEEAMT-6                          PIC 9(5)V99.
           03  QLN-FEEOURS-6                         PIC X.
           03  QLN-FEEFINCHG-6                       PIC X.
           03  QLN-FEECODE-7                         PIC XX.
           03  QLN-FEEAMT-7                          PIC 9(5)V99.
           03  QLN-FEEOURS-7                         PIC X.
           03  QLN-FEEFINCHG-7                       PIC X.
           03  QLN-FEECODE-8                         PIC XX.
           03  QLN-FEEAMT-8                          PIC 9(5)V99.
           03  QLN-FEEOURS-8                         PIC X.
           03  QLN-FEEFINCHG-8                       PIC X.
           03  QLN-FEECODE-9                         PIC XX.
           03  QLN-FEEAMT-9                          PIC 9(5)V99.
           03  QLN-FEEOURS-9                         PIC X.
           03  QLN-FEEFINCHG-9                       PIC X.
           03  QLN-FEECODE-10                        PIC XX.
           03  QLN-FEEAMT-10                         PIC 9(5)V99.
           03  QLN-FEEOURS-10                        PIC X.
           03  QLN-FEEFINCHG-10                      PIC X.
           03  QLN-FEECODE-11                        PIC XX.
           03  QLN-FEEAMT-11                         PIC 9(5)V99.
           03  QLN-FEEOURS-11                        PIC X.
           03  QLN-FEEFINCHG-11                      PIC X.
           03  QLN-FEECODE-12                        PIC XX.
           03  QLN-FEEAMT-12                         PIC 9(5)V99.
           03  QLN-FEEOURS-12                        PIC X.
           03  QLN-FEEFINCHG-12                      PIC X.
           03  QLN-FEECODE-13                        PIC XX.
           03  QLN-FEEAMT-13                         PIC 9(5)V99.
           03  QLN-FEEOURS-13                        PIC X.
           03  QLN-FEEFINCHG-13                      PIC X.
           03  QLN-FEECODE-14                        PIC XX.
           03  QLN-FEEAMT-14                         PIC 9(5)V99.
           03  QLN-FEEOURS-14                        PIC X.
           03  QLN-FEEFINCHG-14                      PIC X.
           03  QLN-FEECODE-15                        PIC XX.
           03  QLN-FEEAMT-15                         PIC 9(5)V99.
           03  QLN-FEEOURS-15                        PIC X.
           03  QLN-FEEFINCHG-15                      PIC X.
           03  QLN-FEECODE-16                        PIC XX.
           03  QLN-FEEAMT-16                         PIC 9(5)V99.
           03  QLN-FEEOURS-16                        PIC X.
           03  QLN-FEEFINCHG-16                      PIC X.
           03  QLN-FEECODE-17                        PIC XX.
           03  QLN-FEEAMT-17                         PIC 9(5)V99.
           03  QLN-FEEOURS-17                        PIC X.
           03  QLN-FEEFINCHG-17                      PIC X.
           03  QLN-FEECODE-18                        PIC XX.
           03  QLN-FEEAMT-18                         PIC 9(5)V99.
           03  QLN-FEEOURS-18                        PIC X.
           03  QLN-FEEFINCHG-18                      PIC X.
           03  QLN-FEECODE-19                        PIC XX.
           03  QLN-FEEAMT-19                         PIC 9(5)V99.
           03  QLN-FEEOURS-19                        PIC X.
           03  QLN-FEEFINCHG-19                      PIC X.
           03  QLN-FEECODE-20                        PIC XX.
           03  QLN-FEEAMT-20                         PIC 9(5)V99.
           03  QLN-FEEOURS-20                        PIC X.
           03  QLN-FEEFINCHG-20                      PIC X.
           03  QLN-INSURED                           PIC X.
           03  QLN-INSTYPES                          PIC X.
           03  QLN-INSELIGIBLE                       PIC X.
           03  QLN-INSCOMP-1                         PIC 99.
           03  QLN-INSOURS-1                         PIC X.
           03  QLN-INSEFF-DATE-1                     PIC X(10).
           03  QLN-INSEXP-DATE-1                     PIC X(10).
           03  QLN-INSPREM-1                         PIC 9(5)V99.
           03  QLN-INSCOVR-1                         PIC 9(7)V99.
           03  QLN-INSCOMM-1                         PIC 9(5)V99.
           03  QLN-INSANTICERN-1                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-1                     PIC S9(7)V99.
           03  QLN-INSUFERN-1                        PIC S9(7)V99.
           03  QLN-INSACCERN-1                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-1                   PIC 99.
           03  QLN-INSFINCHG-1                       PIC X.
           03  QLN-INSCOMP-2                         PIC 99.
           03  QLN-INSOURS-2                         PIC X.
           03  QLN-INSEFF-DATE-2                     PIC X(10).
           03  QLN-INSEXP-DATE-2                     PIC X(10).
           03  QLN-INSPREM-2                         PIC 9(5)V99.
           03  QLN-INSCOVR-2                         PIC 9(7)V99.
           03  QLN-INSCOMM-2                         PIC 9(5)V99.
           03  QLN-INSANTICERN-2                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-2                     PIC S9(7)V99.
           03  QLN-INSUFERN-2                        PIC S9(7)V99.
           03  QLN-INSACCERN-2                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-2                   PIC 99.
           03  QLN-INSFINCHG-2                       PIC X.
           03  QLN-INSCOMP-3                         PIC 99.
           03  QLN-INSOURS-3                         PIC X.
           03  QLN-INSEFF-DATE-3                     PIC X(10).
           03  QLN-INSEXP-DATE-3                     PIC X(10).
           03  QLN-INSPREM-3                         PIC 9(5)V99.
           03  QLN-INSCOVR-3                         PIC 9(7)V99.
           03  QLN-INSCOMM-3                         PIC 9(5)V99.
           03  QLN-INSANTICERN-3                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-3                     PIC S9(7)V99.
           03  QLN-INSUFERN-3                        PIC S9(7)V99.
           03  QLN-INSACCERN-3                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-3                   PIC 99.
           03  QLN-INSFINCHG-3                       PIC X.
           03  QLN-INSCOMP-4                         PIC 99.
           03  QLN-INSOURS-4                         PIC X.
           03  QLN-INSEFF-DATE-4                     PIC X(10).
           03  QLN-INSEXP-DATE-4                     PIC X(10).
           03  QLN-INSPREM-4                         PIC 9(5)V99.
           03  QLN-INSCOVR-4                         PIC 9(7)V99.
           03  QLN-INSCOMM-4                         PIC 9(5)V99.
           03  QLN-INSANTICERN-4                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-4                     PIC S9(7)V99.
           03  QLN-INSUFERN-4                        PIC S9(7)V99.
           03  QLN-INSACCERN-4                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-4                   PIC 99.
           03  QLN-INSFINCHG-4                       PIC X.
           03  QLN-INSCOMP-5                         PIC 99.
           03  QLN-INSOURS-5                         PIC X.
           03  QLN-INSEFF-DATE-5                     PIC X(10).
           03  QLN-INSEXP-DATE-5                     PIC X(10).
           03  QLN-INSPREM-5                         PIC 9(5)V99.
           03  QLN-INSCOVR-5                         PIC 9(7)V99.
           03  QLN-INSCOMM-5                         PIC 9(5)V99.
           03  QLN-INSANTICERN-5                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-5                     PIC S9(7)V99.
           03  QLN-INSUFERN-5                        PIC S9(7)V99.
           03  QLN-INSACCERN-5                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-5                   PIC 99.
           03  QLN-INSFINCHG-5                       PIC X.
           03  QLN-INSCOMP-6                         PIC 99.
           03  QLN-INSOURS-6                         PIC X.
           03  QLN-INSEFF-DATE-6                     PIC X(10).
           03  QLN-INSEXP-DATE-6                     PIC X(10).
           03  QLN-INSPREM-6                         PIC 9(5)V99.
           03  QLN-INSCOVR-6                         PIC 9(7)V99.
           03  QLN-INSCOMM-6                         PIC 9(5)V99.
           03  QLN-INSANTICERN-6                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-6                     PIC S9(7)V99.
           03  QLN-INSUFERN-6                        PIC S9(7)V99.
           03  QLN-INSACCERN-6                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-6                   PIC 99.
           03  QLN-INSFINCHG-6                       PIC X.
           03  QLN-INSCOMP-7                         PIC 99.
           03  QLN-INSOURS-7                         PIC X.
           03  QLN-INSEFF-DATE-7                     PIC X(10).
           03  QLN-INSEXP-DATE-7                     PIC X(10).
           03  QLN-INSPREM-7                         PIC 9(5)V99.
           03  QLN-INSCOVR-7                         PIC 9(7)V99.
           03  QLN-INSCOMM-7                         PIC 9(5)V99.
           03  QLN-INSANTICERN-7                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-7                     PIC S9(7)V99.
           03  QLN-INSUFERN-7                        PIC S9(7)V99.
           03  QLN-INSACCERN-7                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-7                   PIC 99.
           03  QLN-INSFINCHG-7                       PIC X.
           03  QLN-INSCOMP-8                         PIC 99.
           03  QLN-INSOURS-8                         PIC X.
           03  QLN-INSEFF-DATE-8                     PIC X(10).
           03  QLN-INSEXP-DATE-8                     PIC X(10).
           03  QLN-INSPREM-8                         PIC 9(5)V99.
           03  QLN-INSCOVR-8                         PIC 9(7)V99.
           03  QLN-INSCOMM-8                         PIC 9(5)V99.
           03  QLN-INSANTICERN-8                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-8                     PIC S9(7)V99.
           03  QLN-INSUFERN-8                        PIC S9(7)V99.
           03  QLN-INSACCERN-8                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-8                   PIC 99.
           03  QLN-INSFINCHG-8                       PIC X.
           03  QLN-INSCOMP-9                         PIC 99.
           03  QLN-INSOURS-9                         PIC X.
           03  QLN-INSEFF-DATE-9                     PIC X(10).
           03  QLN-INSEXP-DATE-9                     PIC X(10).
           03  QLN-INSPREM-9                         PIC 9(5)V99.
           03  QLN-INSCOVR-9                         PIC 9(7)V99.
           03  QLN-INSCOMM-9                         PIC 9(5)V99.
           03  QLN-INSANTICERN-9                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-9                     PIC S9(7)V99.
           03  QLN-INSUFERN-9                        PIC S9(7)V99.
           03  QLN-INSACCERN-9                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-9                   PIC 99.
           03  QLN-INSFINCHG-9                       PIC X.
           03  QLN-INSCOMP-10                        PIC 99.
           03  QLN-INSOURS-10                        PIC X.
           03  QLN-INSEFF-DATE-10                    PIC X(10).
           03  QLN-INSEXP-DATE-10                    PIC X(10).
           03  QLN-INSPREM-10                        PIC 9(5)V99.
           03  QLN-INSCOVR-10                        PIC 9(7)V99.
           03  QLN-INSCOMM-10                        PIC 9(5)V99.
           03  QLN-INSANTICERN-10                    PIC S9(7)V99.
           03  QLN-INSANTICADJ-10                    PIC S9(7)V99.
           03  QLN-INSUFERN-10                       PIC S9(7)V99.
           03  QLN-INSACCERN-10                      PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-10                  PIC 99.
           03  QLN-INSFINCHG-10                      PIC X.
           03  QLN-PY-YYYYMM-RECDEL                  PIC 9(6).
           03  QLN-MTD-PAYS-RECDEL                   PIC S9(7)V99.
           03  QLN-GARNISH-EXTR                      PIC X.
           03  QLN-SHIP-SAME-FG                      PIC X.
           03  QLN-AD-D-INS-COVR                     PIC 9(7).
           03  QLN-TOT-PARTIAL-CHGOFF                PIC S9(7)V99.
           03  QLN-LPO                               PIC X(4).
           03  QLN-REP-CD                            PIC X(8).
           03  QLN-EMP-ID                            PIC 9(8).
           03  QLN-PROCESS-CD                        PIC X.
           03  QLN-WCBC-CREDITLIM                    PIC S9(5).
           03  QLN-FOLLOWUP-CD                       PIC X(4).
           03  QLN-MIP-FG                            PIC X.
           03  QLN-MIP-MTD-CLAHPAID                  PIC S9(5)V99.
           03  QLN-MIP-CLPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-CLPAID                    PIC S9(7)V99.
           03  QLN-MIP-AHPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-AHPAID                    PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME                   PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME2                  PIC S9(7)V99.
           03  QLN-RESCISSION                        PIC X(1).
           03  QLN-INTERIM-TRW                       PIC X(1).
           03  QLN-DLNO                              PIC 9(4)V99.
           03  QLN-DLPOOL                            PIC 9(5).
           03  QLN-DLRECOURSE                        PIC X.
           03  QLN-DLPARTRCAMT                       PIC 9(7)V99.
           03  QLN-ORIGPURAMT                        PIC 9(7)V99.
           03  QLN-PURBAL                            PIC S9(7)V99.
           03  QLN-DLRVPRIN-1                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-2                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-3                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-1                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-2                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-3                        PIC 9(7)V99.
           03  QLN-DLPARVHELD                        PIC 9(7)V99.
           03  QLN-DLPARVPAID                        PIC 9(7)V99.
           03  QLN-DLDISC                            PIC 9(7)V99.
           03  QLN-LCPDTH-DATE                       PIC X(10).
           03  QLN-INTPDTH-DATE                      PIC X(10).
           03  QLN-CURBAL                            PIC S9(7)V99.
           03  QLN-INTBAL                            PIC S9(7)V99.
           03  QLN-LCBAL                             PIC S9(7)V99.
           03  QLN-OTHBAL                            PIC S9(7)V99.
           03  QLN-OT2BAL                            PIC S9(7)V99.
           03  QLN-LCPAID                            PIC S9(7)V99.
           03  QLN-LCPARTIALS                        PIC S9(7)V99.
           03  QLN-STMT-COUNTER                      PIC 9(5).
           03  QLN-PRIOR-STMT-COUNTER                PIC 9(5).
           03  QLN-FROZEN-REASON                     PIC X(2).
           03  QLN-COUPON-STATEMENT                  PIC X.
           03  QLN-MONTHEND-BAL                      PIC 9(7)V99.
           03  QLN-ANTICTERM                         PIC 9(3).
           03  QLN-ANTICERN-1                        PIC S9(7)V99.
           03  QLN-ANTICADJ-1                        PIC S9(7)V99.
           03  QLN-UFERN-1                           PIC S9(7)V99.
           03  QLN-ACCERN-1                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-1                      PIC 99.
           03  QLN-ANTICERN-2                        PIC S9(7)V99.
           03  QLN-ANTICADJ-2                        PIC S9(7)V99.
           03  QLN-UFERN-2                           PIC S9(7)V99.
           03  QLN-ACCERN-2                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-2                      PIC 99.
           03  QLN-ANTICERN-3                        PIC S9(7)V99.
           03  QLN-ANTICADJ-3                        PIC S9(7)V99.
           03  QLN-UFERN-3                           PIC S9(7)V99.
           03  QLN-ACCERN-3                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-3                      PIC 99.
           03  QLN-ANTICERN-4                        PIC S9(7)V99.
           03  QLN-ANTICADJ-4                        PIC S9(7)V99.
           03  QLN-UFERN-4                           PIC S9(7)V99.
           03  QLN-ACCERN-4                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-4                      PIC 99.
           03  QLN-ANTICERN-5                        PIC S9(7)V99.
           03  QLN-ANTICADJ-5                        PIC S9(7)V99.
           03  QLN-UFERN-5                           PIC S9(7)V99.
           03  QLN-ACCERN-5                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-5                      PIC 99.
           03  QLN-ANTICERN-6                        PIC S9(7)V99.
           03  QLN-ANTICADJ-6                        PIC S9(7)V99.
           03  QLN-UFERN-6                           PIC S9(7)V99.
           03  QLN-ACCERN-6                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-6                      PIC 99.
           03  QLN-ANTICERN-7                        PIC S9(7)V99.
           03  QLN-ANTICADJ-7                        PIC S9(7)V99.
           03  QLN-UFERN-7                           PIC S9(7)V99.
           03  QLN-ACCERN-7                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-7                      PIC 99.
           03  QLN-ANTICERN-8                        PIC S9(7)V99.
           03  QLN-ANTICADJ-8                        PIC S9(7)V99.
           03  QLN-UFERN-8                           PIC S9(7)V99.
           03  QLN-ACCERN-8                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-8                      PIC 99.
           03  QLN-ANTICERN-9                        PIC S9(7)V99.
           03  QLN-ANTICADJ-9                        PIC S9(7)V99.
           03  QLN-UFERN-9                           PIC S9(7)V99.
           03  QLN-ACCERN-9                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-9                      PIC 99.
           03  QLN-ANTICERN-10                       PIC S9(7)V99.
           03  QLN-ANTICADJ-10                       PIC S9(7)V99.
           03  QLN-UFERN-10                          PIC S9(7)V99.
           03  QLN-ACCERN-10                         PIC S9(7)V99.
           03  QLN-EARN-ORDER-10                     PIC 99.
           03  QLN-LMO-PRIOR-INTEREST                PIC S9(7)V99.
           03  QLN-PRIOR-UNEARNED                    PIC S9(7)V99.
           03  QLN-ERNDELQ-STOP                      PIC X.
           03  QLN-NONACRERN                         PIC S9(7)V99.
           03  QLN-PRIOR-NONACRERN                   PIC S9(7)V99.
           03  QLN-ERN-IB-PDTH-DATE                  PIC X(10).
           03  QLN-ERN-IB-PRINBAL                    PIC S9(7)V99.
           03  QLN-ERN-IB-INTOWE                     PIC S9(7)V99.
           03  QLN-TOTERN                            PIC S9(7)V99.
           03  QLN-TOTOTH                            PIC S9(7)V99.
           03  QLN-TOTINT                            PIC S9(7)V99.
           03  QLN-TOTLCHG                           PIC S9(7)V99.
           03  QLN-TOTDEF                            PIC S9(7)V99.
           03  QLN-TOTSERACCT                        PIC S9(7)V99.
           03  QLN-LCWAIVERS                         PIC S9(5)V99.
           03  QLN-INTWAIVERS                        PIC S9(5)V99.
           03  QLN-TOTPAYMNTD                        PIC S9(7)V99.
           03  QLN-TOTEXCPAYMNTD                     PIC S9(7)V99.
           03  QLN-TOTNODEF                          PIC S99.
           03  QLN-YTDNODEF                          PIC S99.
           03  QLN-TOTNODEFACT                       PIC S99.
           03  QLN-YTDNODEFACT                       PIC S99.
           03  QLN-TOTNOALDEL                        PIC S99.
           03  QLN-YTDNOALDEL                        PIC S99.
           03  QLN-YTDINTPD                          PIC S9(7)V99.
           03  QLN-LYRINTPD                          PIC S9(7)V99.
           03  QLN-TOTINTPD                          PIC S9(7)V99.
           03  QLN-RECASTINT                         PIC 9(7)V99.
           03  QLN-RECASTPRIN                        PIC 9(7)V99.
           03  QLN-CL-REBATE                         PIC X.
           03  QLN-AH-REBATE                         PIC X.
           03  QLN-PP-REBATE                         PIC X.
           03  QLN-INT-REBATE                        PIC X.
           03  QLN-SC-REBATE                         PIC X.
           03  QLN-MF-REBATE                         PIC X.
           03  QLN-DF-REBATE                         PIC X.
           03  QLN-EX-REBATE                         PIC X.
           03  QLN-O1-REBATE                         PIC X.
           03  QLN-O2-REBATE                         PIC X.
           03  QLN-O3-REBATE                         PIC X.
           03  QLN-O4-REBATE                         PIC X.
           03  QLN-O5-REBATE                         PIC X.
           03  QLN-O6-REBATE                         PIC X.
           03  QLN-O7-REBATE                         PIC X.
           03  QLN-UN-O7-REB                         PIC X.
           03  QLN-UN-O8-REB                         PIC X.
           03  QLN-UN-O9-REB                         PIC X.
           03  QLN-UN-O10-REB                        PIC X.
           03  QLN-PLREASON                          PIC XX.
           03  QLN-COLLECTOR                         PIC X(3).
           03  QLN-ACTIONCD                          PIC X(4).
           03  QLN-ACTIONCD2                         PIC X(4).
           03  QLN-ATCODE                            PIC X(3).
           03  QLN-RECODE                            PIC X(3).
           03  QLN-ESCROW-FG                         PIC X.
           03  QLN-PROMDAYS-PASTDUE                  PIC S9(3).
           03  QLN-PROMID                            PIC X(3).
           03  QLN-PROMDATE                          PIC X(10).
           03  QLN-PROMAMT                           PIC 9(7)V99.
           03  QLN-PROMDATE2                         PIC X(10).
           03  QLN-DATE-PAID-LAST                    PIC X(10).
           03  QLN-AMT-PAID-LAST                     PIC 9(7)V99.
           03  QLN-LASTCFORMNO                       PIC 9(5).
           03  QLN-LASTCFORMDATE                     PIC X(10).
           03  QLN-DELCNT                            PIC 99.
           03  QLN-DELCNT2                           PIC 99.
           03  QLN-CREDITLIM                         PIC 9(7).
           03  QLN-CRSCORE                           PIC S9(3).
           03  QLN-LASTSFORMNO                       PIC 9(5).
           03  QLN-LASTSFORMDATE                     PIC X(10).
           03  QLN-SOLCNT                            PIC 99.
           03  QLN-MESSAGE                           PIC X(30).
           03  QLN-OLDACCTNO                         PIC 9(8).
           03  QLN-PRLNNO                            PIC 9(8).
           03  QLN-PRBAL                             PIC S9(7)V99.
           03  QLN-PRINTDUE                          PIC S9(7)V99.
           03  QLN-PRFINCHGRB                        PIC S9(7)V99.
           03  QLN-PRINSRB                           PIC S9(7)V99.
           03  QLN-PRPOCD                            PIC XX.
           03  QLN-PRPODATE                          PIC X(10).
           03  QLN-PRDELINQAMT                       PIC S9(7)V99.
           03  QLN-PRLNAMT                           PIC 9(7)V99.
           03  QLN-PRPDUE-DAYS                       PIC S9(5).
           03  QLN-PRDLDISC-UNEARN                   PIC S9(7)V99.
           03  QLN-PRCONPROFILE                      PIC 9(12).
           03  QLN-SUM-ORIGPURAMT                    PIC 9(7)V99.
           03  QLN-SUM-LNAMT                         PIC 9(7)V99.
           03  QLN-SUM-FINCHG                        PIC 9(7)V99.
           03  QLN-SUM-INTCHG                        PIC 9(7)V99.
           03  QLN-SUM-ORGTERM                       PIC 9(3).
           03  QLN-SUM-REGPYAMT                      PIC 9(7)V99.
           03  QLN-SUM-EXTCHG                        PIC 9(5)V99.
           03  QLN-SUM-SERCHG                        PIC 9(5)V99.
           03  QLN-SUM-MAINTFEE                      PIC 9(7)V99.
           03  QLN-UNITPER-CD                        PIC X.
           03  QLN-UNITPER-FREQ                      PIC 999.
           03  QLN-DAYS-IN-UNITPER                   PIC 9(3).
           03  QLN-UNITPER-PER-YEAR                  PIC 9(3)V9(12).
           03  QLN-REPOCD                            PIC X.
           03  QLN-BR-TFR-FG                         PIC X.
           03  QLN-O3INS-DLPROC                      PIC 9(7)V99.
           03  QLN-INSPREM-DLADJ                     PIC S9(5)V99.
           03  QLN-PLAMT                             PIC 9(7)V99.
           03  QLN-MTD-PRIN                          PIC S9(7)V99.
           03  QLN-MTD-LC                            PIC S9(5)V99.
           03  QLN-MTD-INT                           PIC S9(5)V99.
           03  QLN-MTD-OT                            PIC S9(5)V99.
           03  QLN-MTD-EX                            PIC S9(7)V99.
           03  QLN-BNKRPT-CHAPTER                    PIC 9(2).
           03  QLN-BMO-CON-STATUS                    PIC X.
           03  QLN-BMO-REC-STATUS                    PIC X.
           03  QLN-APP-NUMBER                        PIC 9(09).
           03  QLN-GIFTNET                           PIC X.
           03  QLN-GIFTNET-OUR-PO                    PIC 9(7).
           03  QLN-PROC-DISBURSED                    PIC X.
           03  QLN-NOODDPY                           PIC X.
           03  QLN-CONPROFILE-1                      PIC 9(12).
           03  QLN-CONPROFILE-2                      PIC 9(12).
           03  QLN-CONPROFILE-3                      PIC 9(12).
           03  QLN-CONPROFILE-4                      PIC 9(12).
           03  QLN-CONPROFILE-5                      PIC 9(12).
           03  QLN-RECPROFILE-1                      PIC 9(12).
           03  QLN-RECPROFILE-2                      PIC 9(12).
           03  QLN-RECPROFILE-3                      PIC 9(12).
           03  QLN-RECPROFILE-4                      PIC 9(12).
           03  QLN-RECPROFILE-5                      PIC 9(12).
           03  QLN-TRW-STATUS                        PIC XX.
           03  QLN-ORIGIN                            PIC X(4).
           03  QLN-PO-TRCD                           PIC XX.
           03  QLN-NEXIS-CRSCORE                     PIC 9(3).
           03  QLN-TRW-B-CONSUMER-IND                PIC X(2).
           03  QLN-PARVHELD-PAID-FG                  PIC X.
           03  QLN-ACCT-FROZEN                       PIC X.
           03  QLN-TRW-SPECIAL-CODE                  PIC X(2).
           03  QLN-CREATION-STATUS                   PIC X(1).
           03  QLN-TOTNOLCHG                         PIC S9(3).
           03  QLN-LBOX                              PIC X(1).
           03  QLN-EXTRA-PRIN-LFP                    PIC S9(7)V99.
           03  QLN-REMAIN-INT-SINCE-LFP              PIC S9(7)V99.
           03  QLN-REMAIN-PRIN-SINCE-LFP             PIC S9(7)V99.
           03  QLN-INTERIM-COUPON                    PIC X.
           03  QLN-DLCOMM-O3-CMSI                    PIC 9(5)V99.
           03  QLN-DLCOMM-PP-CMSI                    PIC 9(5)V99.
           03  QLN-TRW-J-CONSUMER-IND                PIC X(2).
           03  QLN-ACH                               PIC X.
           03  QLN-1099-C-FG                         PIC X.
           03  QLN-TRW-COMPLIANCE-CODE               PIC XX.
           03  QLN-UNWIND-REASON-CODE                PIC X(2).
           03  QLN-PBI-PBD-FG                        PIC X.
           03  QLN-OLD-ACCT-FORMAT-FG                PIC X.
           03  QLN-APPROVED-BY-USER                  PIC X(10).
           03  QLN-APPROVED-BY-CODE                  PIC X(3).
           03  QLN-APPROVED-BY-DATE                  PIC X(10).
           03  QLN-APPROVED-BY-TIME                  PIC 9(6).
           03  QLN-LTOUCH-DATE                       PIC X(10).
           03  QLN-LTOUCH-TIME                       PIC 9(6).

       EXEC SQL END DECLARE SECTION END-EXEC.	  
	   

       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *-----------------------------------------------------------------
       01  REPORT-IFN                  PIC X(06)     VALUE "REPORT".
       01  REPORT-PATH                 PIC X(35)     VALUE SPACES.

      ******************************************************************
      *
      *    L O A D - W O R K E R S
      *
      *-----------------------------------------------------------------
      *    WORKERS USED TO LOAD THE REPORT-FILE
      ******************************************************************

      *-------------------------------------------------{ INT. BEARING }
       01  LOAD-REPORT-IB-WORKERS.
           03  LOAD-IB-DISPLAY-LNNO                  PIC X(16).
           03  LOAD-IB-LNAME                         PIC X(16).
           03  LOAD-IB-FNAME                         PIC X(16).
           03  LOAD-IB-FROM-DATE                     PIC 9(08).
           03  LOAD-IB-INT-SINCE-LSTPY               PIC S9(9)V99.
           03  LOAD-IB-INTBAL                        PIC S9(9)V99.
           03  LOAD-IB-TRUE-ACCRUE                   PIC S9(9)V99.
           03  LOAD-IB-ACCRUED                       PIC S9(9)V99.
           03  LOAD-IB-MAX                           PIC X(01).

      *-------------------------------------{ INT. BERARING EXCEPTIONS }
       01  LOAD-REPORT-IBX-WORKERS.
           03  LOAD-IBX-DISPLAY-LNNO                 PIC X(16).
           03  LOAD-IBX-LNAME                        PIC X(16).
           03  LOAD-IBX-FNAME                        PIC X(16).
           03  LOAD-IBX-INT-PDTH-DATE                PIC 9(8).
           03  LOAD-IBX-INT-SINCE-LSTPY              PIC S9(9)V99.
           03  LOAD-IBX-INT-OWED                     PIC S9(9)V99.
           03  LOAD-IBX-TRUE-ACCRUE                  PIC S9(9)V99.
           03  LOAD-IBX-INCOME-ACCRUE                PIC S9(9)V99.
           03  LOAD-IBX-BALANCE                      PIC S9(9)V99.
           03  LOAD-IBX-CONTR-PDTH-DATE              PIC 9(8).
           03  LOAD-IBX-CONTR-STATUS                 PIC 9(03).

      *---------------------------------------------------{ PRECOMPUTE }
       01  LOAD-REPORT-PC-WORKERS.
           03  LOAD-PC-DISPLAY-LNNO                  PIC X(16).
           03  LOAD-PC-LNAME                         PIC X(16).
           03  LOAD-PC-FNAME                         PIC X(16).
           03  LOAD-PC-ANTERM                        PIC 9(03).
           03  LOAD-PC-ANTIC                         PIC S9(9)V99.
           03  LOAD-PC-UNEARN                        PIC S9(9)V99.
           03  LOAD-PC-INT                           PIC S9(9)V99.
           03  LOAD-PC-ADDON                         PIC S9(9)V99.
           03  LOAD-PC-SRV                           PIC S9(9)V99.
           03  LOAD-PC-MFEE                          PIC S9(9)V99.
           03  LOAD-PC-DISC                          PIC S9(9)V99.
           03  LOAD-PC-OTHER                         PIC S9(9)V99.
           03  LOAD-PC-CASH-EARN                     PIC S9(9)V99.
           03  LOAD-PC-ENDING                        PIC S9(9)V99.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
      *01  ACCRUAL-DATE-MMYY           PIC 9(04)     VALUE ZEROES.
      *01  FILLER            REDEFINES ACCRUAL-DATE-MMYY.
      *    03  ACCRUAL-DATE-MMYY-MM    PIC 9(02).
      *    03  ACCRUAL-DATE-MMYY-YY    PIC 9(02).

       01  CALCUALTION-WORKERS.
           03  CALC-ADDON-EARNINGS     PIC S9(7)V99  COMP VALUE ZEROES.
           03  CALC-INTPDTH-DATE       PIC 9(8)     VALUE ZEROES.


       01  CURRENT-REPORT-TYPE         PIC X(03)     VALUE SPACES.
           88  CURRENT-REPORT-TYPE-IB                VALUE "IB ".
           88  CURRENT-REPORT-TYPE-IBX               VALUE "IBX".
           88  CURRENT-REPORT-TYPE-PC                VALUE "PC ".

       01  DISPLAY-ACCTNO.
           03  FILLER                  PIC X(03)     VALUE SPACES.
           03  DISPLAY-LNNO            PIC X(13)     VALUE SPACES.

       01  HOLD-BRANCH                 PIC 9(05)     VALUE ZEROES.
       01  HOLD-CLASS                  PIC 9(03)     VALUE ZEROES.
       01  HOLD-POOL                   PIC 9(06)     VALUE ZEROES.

       01  HOLD-LC1-KEY.
           03  HOLD-LC-CODE            PIC 9(3)      VALUE ZEROES.

       01  HOLD-SP1-KEY.
           03  HOLD-SP-ORGST           PIC XX        VALUE SPACES.
           03  HOLD-SP-SPRCLASS        PIC 999       VALUE ZEROES.
           03  HOLD-SP-SUBCLASS        PIC 99        VALUE ZEROES.
           03  HOLD-SP-LAWCODE         PIC 99        VALUE ZEROES.

       01  HOLD-REPORT-KEY.
           03  HOLD-REPORT-TYPE        PIC X(03).
               88  HOLD-REPORT-TYPE-IB               VALUE "IB ".
               88  HOLD-REPORT-TYPE-IBX              VALUE "IBX".
               88  HOLD-REPORT-TYPE-PC               VALUE "PC ".
           03  HOLD-REPORT-BRANCH      PIC 9(05).
               88  HOLD-REPORT-GRAND-TOTAL           VALUE 99999.
           03  HOLD-REPORT-POOL        PIC 9(06).
               88  HOLD-REPORT-BRANCH-TOTAL          VALUE 999999.
           03  HOLD-REPORT-CLASS       PIC 9(04).
               88  HOLD-REPORT-POOL-TOTAL            VALUE 9999.
           03  HOLD-REPORT-ACCTNO      PIC 9(08).
               88  HOLD-REPORT-CLASS-TOTAL           VALUE 99999999.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".
       01  MAX-IB-INTEREST             PIC S9(9)V99  COMP  VALUE ZEROES.
       01  ONE                         PIC S9(1)     VALUE ZEROES.

       01  PROCESS-ACCTNO-STAT         PIC X(02)     VALUE SPACES.
           88  PROCESS-ACCTNO-STAT-CONTINUE          VALUE SPACES.
           88  PROCESS-ACCTNO-STAT-COMPLETED         VALUE "XX".

       01  LN-COUNT                PIC 9(03)     VALUE 99.
       01  LN-FEED                 PIC 9(02)     VALUE ZEROES.
       01  PAGE-NUMBER-TOTAL       PIC 9(04)     VALUE ZEROES.
       01  PAGE-NUMBER-TYPE        PIC 9(04)     VALUE ZEROES.

       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  LN1-OPENED-SW               PIC X         VALUE "N".
       01  SP1-FOUND-SW                PIC X         VALUE "N".
	   
       01  SP1-TBL-CTR                 PIC 9(5)      VALUE ZEROES.
       01  SP1-SUB                     PIC 9(5)      COMP VALUE 0.
       01  SP1-TABLE                   OCCURS 10000 TIMES.
           03  SPA-TBL-REC             PIC X(984)    VALUE SPACES.
           03  SPA-TBL-REC-RDF         REDEFINES SPA-TBL-REC.
               05  SPA-TBL-ORGST       PIC XX.
               05  SPA-TBL-SPRCLASS    PIC 999.
               05  SPA-TBL-SUBCLASS    PIC 99.
               05  SPA-TBL-LAWCODE     PIC 99.
               05  SPA-TBL-DATA        PIC X(975).
           03  SPB-TBL-REC             PIC X(2553)    VALUE SPACES.
           03  SPB-TBL-REC-RDF         REDEFINES SPB-TBL-REC.
               05  SPB-TBL-ORGST       PIC XX.
               05  SPB-TBL-SPRCLASS    PIC 999.
               05  SPB-TBL-SUBCLASS    PIC 99.
               05  SPB-TBL-LAWCODE     PIC 99.
               05  SPB-TBL-DATA        PIC X(2544).
           03  SPC-TBL-REC             PIC X(1367)     VALUE SPACES.
           03  SPC-TBL-REC-RDF         REDEFINES SPC-TBL-REC.
               05  SPC-TBL-ORGST       PIC XX.
               05  SPC-TBL-SPRCLASS    PIC 999.
               05  SPC-TBL-SUBCLASS    PIC 99.
               05  SPC-TBL-LAWCODE     PIC 99.
               05  SPC-TBL-DATA        PIC X(1358).

      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH                         PIC 9(04).
           03  END-BRANCH                         PIC 9(04).

           03  ACCRUAL-DATE                       PIC 9(08).
           03  FILLER                   REDEFINES ACCRUAL-DATE.
               05  ACCRUAL-DATE-CC                PIC 9(02).
               05  ACCRUAL-DATE-YY                PIC 9(02).
               05  ACCRUAL-DATE-MM                PIC 9(02).
               05  ACCRUAL-DATE-DD                PIC 9(02).

           03  GROUP-OPTIONS.
               05  GROUP-FLAG                     PIC X(01).
               05  ENT-GROUP                      PIC X(04).

           03  FILLER                             PIC X(01).

           03  POOL-RANGE-WORKERS.
               05  BEG-POOL                       PIC 9(05).
               05  END-POOL                       PIC 9(05).

           03  REPORT-BREAKDOWN-OPTIONS.
               05  PRINT-BY-CLASS                 PIC X(01).
               05  PRINT-BY-POOL                  PIC X(01).

           03  REPORT-LOAN-TYPE-OPTIONS.
               05  PRINT-LOAN-TYPE-IB             PIC X(01).
               05  PRINT-LOAN-TYPE-IBX            PIC X(01).
               05  PRINT-LOAN-TYPE-PC             PIC X(01).

           03  REPORT-PRINT-OPTIONS.
               05  PRINT-BRANCH-TOTALS            PIC X(01).
               05  PRINT-CLASS-TOTALS             PIC X(01).
               05  PRINT-DETAIL-ANALYSIS          PIC X(01).
               05  PRINT-GRAND-TOTALS             PIC X(01).
               05  PRINT-POOL-TOTALS              PIC X(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "D".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.
           03  FILLER             PIC X(18) VALUE "PAGE (TYPE/TOTAL) ".
           03  H01-PAGE-TYPE      PIC 9(04).
           03  FILLER             PIC X(01) VALUE "/".
           03  H01-PAGE-TOTAL     PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  FILLER             PIC X(18) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  FILLER             PIC X(12) VALUE "DAY ENDING: ".
           03  H02-ACCRUAL-DATE   PIC 99/99/99.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER             PIC X(08) VALUE "BRANCH :".
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H03-BRANCH         PIC 9(04) VALUE ZEROES.
           03  H03-BRANCHX        REDEFINES H03-BRANCH
                                  PIC X(04).
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H03-BRANCH-DESC    PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(76) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-04.
           03  FILLER             PIC X(08) VALUE "POOL ID:".
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H04-POOL           PIC 9(05) VALUE ZEROES.
           03  H04-POOLX          REDEFINES H04-POOL
                                  PIC X(05).
           03  FILLER             PIC X(01) VALUE SPACES.
           03  H04-POOL-DESC      PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(76) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-05.
           03  FILLER             PIC X(08) VALUE "CLASS  :".
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H05-CLASS          PIC 9(02) VALUE ZEROES.
           03  H05-CLASSX         REDEFINES H05-CLASS
                                  PIC X(02).
           03  FILLER             PIC X(04) VALUE SPACES.
           03  H05-CLASS-DESC     PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(56) VALUE SPACES.
           03  H05-MTD-NON        PIC X(07) VALUE "MTD-NON".
           03  FILLER             PIC X(11) VALUE SPACES.

      *-------------------------------------------{ 'INTEREST BEARING' }
       01  HEADER-LINE-IB-01.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "              ACCRUED     ACCR".
           03  FILLER PIC X(30) VALUE "UED FROM    +    INTEREST     ".
           03  FILLER PIC X(30) VALUE "=       TRUE        ACCRUED   ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IB-02.
           03  FILLER PIC X(30) VALUE "ACCOUNT NO.   BORROWER        ".
           03  FILLER PIC X(30) VALUE "                 FROM    LAST ".
           03  FILLER PIC X(30) VALUE "ASSESSED             OWED     ".
           03  FILLER PIC X(30) VALUE "    ASSESSED         AMOUNT   ".
           03  FILLER PIC X(12) VALUE SPACES.

      *--------------------------------{ 'INTEREST BEARING EXCEPTIONS' }
       01  HEADER-LINE-IBX-01.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "        ACRRUED    ACCRD FROM ".
           03  FILLER PIC X(30) VALUE "    INTEREST         TRUE     ".
           03  FILLER PIC X(30) VALUE " ACCRUED                CONTRA".
           03  FILLER PIC X(12) VALUE "CT    CONT  ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-IBX-02.
           03  FILLER PIC X(30) VALUE "ACCOUNT NO.   BORROWER        ".
           03  FILLER PIC X(30) VALUE "        FROM        LAST ASSD ".
           03  FILLER PIC X(30) VALUE "        OWED         ASSD     ".
           03  FILLER PIC X(30) VALUE "  AMOUNT      BALANCE  PAID TH".
           03  FILLER PIC X(12) VALUE "RU  STATUS  ".

      *-------------------------------------------------{ 'PRECOMPUTE' }
       01  HEADER-LINE-PC-01.
           03  FILLER PIC X(30) VALUE "                             A".
           03  FILLER PIC X(30) VALUE "NTICIPATED      BEGIN   ------".
           03  FILLER PIC X(30) VALUE "---------- DAILY ACCRUAL EARNI".
           03  FILLER PIC X(30) VALUE "NGS ----------------    ACCRUA".
           03  FILLER PIC X(12) VALUE "L    ENDING ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-PC-02.
           03  FILLER PIC X(30) VALUE "ACCOUNT NO.   BORROWER      TE".
           03  FILLER PIC X(30) VALUE "RM EARNINGS  UNEARNED  INTERES".
           03  FILLER PIC X(30) VALUE "T    ADDONS    SERCHG  MAINTFE".
           03  FILLER PIC X(30) VALUE "E   DL DISC     OTHER  EARNING".
           03  FILLER PIC X(12) VALUE "S  UNEARNED ".

      *---------------------------------{ 'INTEREST BEARING' (SUMMARY) }
       01  HEADER-LINE-IB-01-SUMMARY.
           03  FILLER PIC X(30) VALUE "* SUMMARY *                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IB-02-SUMMARY.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(30) VALUE "        TOTAL ACCRUED         ".
           03  FILLER PIC X(30) VALUE "   TOTAL       TOTAL TRUE     ".
           03  FILLER PIC X(30) VALUE " TOTAL ACCRUED                ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IB-03-SUMMARY.
           03  FILLER PIC X(30) VALUE " BR# POOL CL        ACCTS     ".
           03  FILLER PIC X(30) VALUE "   FROM LAST ASSESSED         ".
           03  FILLER PIC X(30) VALUE "INT OWED         ASSESSED     ".
           03  FILLER PIC X(30) VALUE "        AMOUNT                ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------{ 'INTEREST BEARING EXCEPTION' (SUMMARY) }
       01  HEADER-LINE-IBX-01-SUMMARY.
           03  FILLER PIC X(30) VALUE "* SUMMARY *                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IBX-02-SUMMARY.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "                   ACCRD FROM ".
           03  FILLER PIC X(30) VALUE "    INTEREST         TRUE     ".
           03  FILLER PIC X(30) VALUE " ACCRUED                      ".
           03  FILLER PIC X(12) VALUE "            ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-IBX-03-SUMMARY.
           03  FILLER PIC X(30) VALUE " BR# POOL CL                  ".
           03  FILLER PIC X(30) VALUE "                    LAST ASSD ".
           03  FILLER PIC X(30) VALUE "        OWED         ASSD     ".
           03  FILLER PIC X(30) VALUE "  AMOUNT      BALANCE         ".
           03  FILLER PIC X(12) VALUE "            ".

      *---------------------------------------{ 'PRECOMPUTE' (SUMMARY) }
       01  HEADER-LINE-PC-01-SUMMARY.
           03  FILLER PIC X(30) VALUE "* SUMMARY *                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(21) VALUE SPACES.
           03  FILLER PIC X(07) VALUE "MTD-NON".
           03  FILLER PIC X(14) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-PC-02-SUMMARY.
           03  FILLER PIC X(30) VALUE "                   NO      BEG".
           03  FILLER PIC X(30) VALUE "IN       ---------------------".
           03  FILLER PIC X(30) VALUE " DAILY ACCRUAL EARNINGS ------".
           03  FILLER PIC X(30) VALUE "---------------       ACCRUAL ".
           03  FILLER PIC X(12) VALUE "     ENDING ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-PC-03-SUMMARY.
           03  FILLER PIC X(30) VALUE " BR# POOL CL    ACCTS   UNEARN".
           03  FILLER PIC X(30) VALUE "ED     INTEREST      ADDONS   ".
           03  FILLER PIC X(30) VALUE "   SERCHG    MAINTFEE      DL ".
           03  FILLER PIC X(30) VALUE "DISC       OTHER    EARNINGS  ".
           03  FILLER PIC X(12) VALUE "   UNEARNED ".

      *-----------------------------------------------------------------
       01  HEADER-TITLE-TYPE-IB.
           03  FILLER PIC X(31) VALUE "ACCRUED INTEREST FOR I/B LOANS ".
           03  FILLER PIC X(31) VALUE "  (* - EXCEEDS MAX ACCRUE DAYS)".
           03  FILLER PIC X(30) VALUE "                              ".

      *-----------------------------------------------------------------
       01  HEADER-TITLE-TYPE-IBX.
           03  FILLER PIC X(31) VALUE "ACCRUED INTEREST FOR I/B LOANS ".
           03  FILLER PIC X(31) VALUE "EXCEEDING MAX ACCRUE DAYS      ".
           03  FILLER PIC X(30) VALUE "                              ".

      *-----------------------------------------------------------------
       01  HEADER-TITLE-TYPE-PC.
           03  FILLER PIC X(31) VALUE "UNEARNED / EARNED ACCRUAL REPOR".
           03  FILLER PIC X(31) VALUE "T (PRECOMPUTE)                 ".
           03  FILLER PIC X(30) VALUE "                              ".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------{ 'INT. BEARING' }
       01  DETAIL-LINE-01-IB                  REDEFINES DETAIL-LINE.
           03  D01-IB-DISPLAY-LNNO            PIC X(13).
           03  FILLER                         PIC X(01).
           03  D01-IB-NAME                    PIC X(13).
           03  FILLER                         PIC X(16).
           03  D01-IB-FROM                    PIC 99/99/99.
           03  FILLER                         PIC X(05).
           03  D01-IB-INT-SINCE-LSTPY         PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  D01-IB-INTBAL                  PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  D01-IB-TRUE-ACCRUE             PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  D01-IB-ACCRUED                 PIC ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  D01-IB-MAX                     PIC X(01).

      *------------------------------------{ 'INT. BEARING EXCEPTIONS' }
       01  DETAIL-LINE-02-IBX                 REDEFINES DETAIL-LINE.
           03  D02-IBX-DISPLAY-LNNO           PIC X(13).
           03  FILLER                         PIC X(01).
           03  D02-IBX-NAME                   PIC X(13).
           03  FILLER                         PIC X(11).
           03  D02-IBX-INT-PDTH               PIC 99/99/99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-INT-SINCE-LSTPY        PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-INT-OWED               PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-TRUE-ACCRUE            PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  D02-IBX-INCOME-ACCRUE          PIC ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-BALANCE                PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  D02-IBX-CONT-PDTH              PIC 99/99/99.
           03  FILLER                         PIC X(05).
           03  D02-IBX-CONT-STATUS            PIC ZZ9.
           03  FILLER                         PIC X(02).

      *-------------------------------------------------{ 'PRECOMPUTE' }
       01  DETAIL-LINE-03-PC                  REDEFINES DETAIL-LINE.
           03  D03-PC-DISPLAY-LNNO            PIC X(13).
           03  FILLER                         PIC X(01).
           03  D03-PC-NAME                    PIC X(13).
           03  FILLER                         PIC X(01).
           03  D03-PC-ANTERM                  PIC ZZ9        BLANK ZERO.
           03  FILLER                         PIC X(01).
           03  D03-PC-ANTIC                   PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-UNEARN                  PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-INT                     PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-ADDON                   PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-SRV                     PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-MFEE                    PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-DISC                    PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-OTHER                   PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-CASH-EARN               PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-ENDING                  PIC Z(05)9.99- BLANK ZERO.

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                  REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC               REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                  REDEFINES R01-FIELD
                                              PIC X(100).

      *--------------------------------{ 'INT. BEARING' SUMMARY TOTALS }
       01  SUMMARY-LINE-01-IB                 REDEFINES DETAIL-LINE.
           03  S01-IB-BRANCH                  PIC Z(03)9.
           03  S01-IB-BRANCHX                 REDEFINES S01-IB-BRANCH
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S01-IB-POOL                    PIC Z(03)9.
           03  S01-IB-POOLX                   REDEFINES S01-IB-POOL
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S01-IB-CLASS                   PIC 9(02).
           03  S01-IB-CLASSX                  REDEFINES S01-IB-CLASS
                                              PIC X(02).
           03  FILLER                         PIC X(05).
           03  S01-IB-NUMBER-ACCTS            PIC Z(05)9.
           03  S01-IB-NUMBER-ACCTS-FG         PIC X(01).
           03  FILLER                         PIC X(13).
           03  S01-IB-INT-SINCE-LSTPY         PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  S01-IB-INTBAL                  PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  S01-IB-TRUE-ACCRUE             PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  S01-IB-ACCRUED                 PIC ZZZ,ZZZ,ZZ9.99.

      *-------------------------{ 'INT. BEARING EXECPT' SUMMARY TOTALS }
       01  SUMMARY-LINE-02-IBX                REDEFINES DETAIL-LINE.
           03  S02-IBX-BRANCH                 PIC Z(03)9.
           03  S02-IBX-BRANCHX                REDEFINES S02-IBX-BRANCH
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S02-IBX-POOL                   PIC Z(03)9.
           03  S02-IBX-POOLX                  REDEFINES S02-IBX-POOL
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S02-IBX-CLASS                  PIC 9(02).
           03  S02-IBX-CLASSX                 REDEFINES S02-IBX-CLASS
                                              PIC X(02).
           03  FILLER                         PIC X(05).
           03  S02-IBX-NUMBER-ACCTS           PIC Z(05)9.
           03  S02-IBX-NUMBER-ACCTS-FG        PIC X(01).
           03  FILLER                         PIC X(23).
           03  S02-IBX-INT-SINCE-LSTPY        PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  S02-IBX-INT-OWED               PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  S02-IBX-TRUE-ACCRUE            PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  S02-IBX-INCOME-ACCRUE          PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  S02-IBX-BALANCE                PIC Z,ZZZ,ZZ9.99.

      *----------------------------------{ 'PRECOMPUTE' SUMMARY TOTALS }
       01  SUMMARY-LINE-03-PC                 REDEFINES DETAIL-LINE.
           03  S03-PC-BRANCH                  PIC Z(03)9.
           03  S03-PC-BRANCHX                 REDEFINES S03-PC-BRANCH
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S03-PC-POOL                    PIC Z(03)9.
           03  S03-PC-POOLX                   REDEFINES S03-PC-POOL
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S03-PC-CLASS                   PIC 9(02).
           03  S03-PC-CLASSX                  REDEFINES S03-PC-CLASS
                                              PIC X(02).
           03  FILLER                         PIC X(01).
           03  S03-PC-NUMBER-ACCTS            PIC Z(05)9.
           03  S03-PC-NUMBER-ACCTS-FG         PIC X(01).
           03  S03-PC-UNEARN                  PIC Z(08)9.99- BLANK ZERO.
           03  S03-PC-INT                     PIC Z(08)9.99- BLANK ZERO.
           03  S03-PC-ADDON                   PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-SRV                     PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-MFEE                    PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-DISC                    PIC Z(08)9.99- BLANK ZERO.
           03  S03-PC-OTHER                   PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-CASH-EARN               PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-ENDING                  PIC Z(08)9.99- BLANK ZERO.

      *-------------------------------{ 'INT. BEARING' STANDARD TOTALS }
       01  TOTAL-LINE-01-IB                   REDEFINES DETAIL-LINE.
           03  T01-IB-PROMPT                  PIC X(25).
           03  T01-IB-NUMBER-ACCTS            PIC Z(05)9.
           03  T01-IB-NUMBER-ACCTS-FG         PIC X(01).
           03  FILLER                         PIC X(23).
           03  T01-IB-INT-SINCE-LSTPY         PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(04).
           03  T01-IB-INTBAL                  PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(04).
           03  T01-IB-TRUE-ACCRUE             PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  T01-IB-ACCRUED                 PIC ZZZ,ZZZ,ZZ9.99-.

      *------------------------{ 'INT. BEARING EXECPT' STANDARD TOTALS }
       01  TOTAL-LINE-02-IBX                  REDEFINES DETAIL-LINE.
           03  T02-IBX-PROMPT                 PIC X(25).
           03  T02-IBX-NUMBER-ACCTS           PIC Z(05)9.
           03  T02-IBX-NUMBER-ACCTS-FG        PIC X(01).
           03  FILLER                         PIC X(15).
           03  T02-IBX-INT-SINCE-LSTPY        PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  T02-IBX-INT-OWED               PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  T02-IBX-TRUE-ACCRUE            PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  T02-IBX-INCOME-ACCRUE          PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  T02-IBX-BALANCE                PIC Z,ZZZ,ZZ9.99.

      *---------------------------------{ 'PRECOMPUTE' STANDARD TOTALS }
       01  TOTAL-LINE-03-PC                   REDEFINES DETAIL-LINE.
           03  T03-PC-PROMPT                  PIC X(25).
           03  T03-PC-NUMBER-ACCTS            PIC Z(05)9.
           03  T03-PC-NUMBER-ACCTS-FG         PIC X(01).
           03  FILLER                         PIC X(07).
           03  T03-PC-UNEARN                  PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-ADDON                   PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-MFEE                    PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-OTHER                   PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-ENDING                  PIC Z(08)9.99- BLANK ZERO.

      *-----------------------------------------------------------------
       01  TOTAL-LINE-04-PC                   REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(49).
           03  T04-PC-INT                     PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PC-SRV                     PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PC-DISC                    PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PC-CASH-EARN               PIC Z(08)9.99- BLANK ZERO.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *COPY "LIBLP/AC1DAY.CMD.CPY". %%%%%%%%%%%%%%%%% REPLACE THIS WHEN DONE
       COPY "LIBLP/AC9DAY_CMD.CPY".
       COPY "LIBLP/LPAERNW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPCERNW.CPY".
       COPY "LIBLP/LPEADJW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
      *COPY "LIBLP/LPEDAYW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPUERNW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/ARRAYSPW.CPY". 

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AC1DAY_DEF.CPY".
       COPY "LIBLP/AC1DAY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/AC1DAY_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 REPORT-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
      *    PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CD1-FILE.
      *     PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE REPORT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE "X"           TO RECORDS-FOUND.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AC1DAY"      TO FORM-PROG VDU-FORM-NAME.

           MOVE ZEROES TO ACCRUAL-DATE.

           MOVE AC1DAY-QUEUE-POS TO Q-POS.
           MOVE AC1DAY-COPIES-POS TO C-POS.
           MOVE AC1DAY-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

       INITIALIZATION-CONTINUE.

           IF EXT-ROUTE-BUF = "00"
              MOVE PROGID-TEXT-PROCESSING TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ MAKE PATHS }
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-PR-FILE.
      *    PERFORM OPEN-SP1-FILE.

      *- - - - - - - - - - - - - - - - - - - -{ MISCELLANEOUS SETTINGS }
           PERFORM ENABLE-INTERRUPT.

           PERFORM INITIAL-PROGRESS-INDICATOR.
           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.
           MOVE "Y"       TO TSTINT-CONTINUE-ONLY.

      *    PERFORM LOAD-SP-TABLE.
           PERFORM ARRAYSP-LOAD. 

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-PRINT-GRAND-TOTALS.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF (( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES ))
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES                 TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-BEG-POOL.

           MOVE LEGEND-BEG-POOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN050BEG-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-POOL.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-POOL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-POOL.

      *-----------------------------------------------------------------
       ENTER-END-POOL.

           MOVE LEGEND-END-POOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN050END-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-POOL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-POOL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-POOL.

           IF ( BEG-POOL > END-POOL )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-POOL.

           GO TO ENTER-ACCRUAL-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-POOL.

           MOVE "SNNN050BEG-POOL." TO VDU.
           MOVE ZEROES             TO VDUNUM0 BEG-POOL.
           PERFORM SCREENIO.

           MOVE "SNNN050END-POOL." TO VDU.
           MOVE 99999              TO VDUNUM0 END-POOL.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCRUAL-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              IF ( UP-KEY )
                 GO TO ENTER-END-POOL
              ELSE
                 GO TO ENTER-PRINT-BY-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM DISPLAY-ACCRUAL-DATE.

           MOVE LEGEND-ACCRUAL-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060ACCRUAL-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-POOL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ACCRUAL-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDU-DATE-YYYYMMDD TO ACCRUAL-DATE.
      *    MOVE ACCRUAL-DATE-MM TO ACCRUAL-DATE-MMYY-MM.
      *    MOVE ACCRUAL-DATE-YY TO ACCRUAL-DATE-MMYY-YY.

      *=================================================================
       ENTER-PRINT-BY-CLASS.

           IF ( UP-KEY ) GO TO ENTER-ACCRUAL-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-CLASS." TO VDU.
           MOVE "Y"                 TO VDUBUF   PRINT-BY-CLASS.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-PRINT-BY-POOL.

           MOVE LEGEND-PRINT-BY-POOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-BY-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-BY-POOL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-BY-POOL.

           IF NOT ( PRINT-BY-POOL = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-BY-POOL.

      *-----------------------------------------------------------------
       ENTER-PRINT-LOAN-TYPE-IB.

           MOVE LEGEND-PRINT-LOAN-TYPE-IB TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-IB." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-BY-POOL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-LOAN-TYPE-IB.

           IF NOT ( PRINT-LOAN-TYPE-IB = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-LOAN-TYPE-IB.

      *-----------------------------------------------------------------
       ENTER-PRINT-LOAN-TYPE-IBX.

           MOVE LEGEND-PRINT-LOAN-TYPE-IBX TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-IBX." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IB.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IBX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-LOAN-TYPE-IBX.

           IF NOT ( PRINT-LOAN-TYPE-IBX = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-LOAN-TYPE-IBX.

      *-----------------------------------------------------------------
       ENTER-PRINT-LOAN-TYPE-PC.

           MOVE LEGEND-PRINT-LOAN-TYPE-PC TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-PC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IBX.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-PC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-LOAN-TYPE-PC.

           IF NOT ( PRINT-LOAN-TYPE-PC = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-LOAN-TYPE-PC.

      *-----------------------------------------------------------------
       ENTER-PRINT-DETAIL-ANALYSI.

           MOVE LEGEND-PRINT-DETAIL-ANALYSIS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-PC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-DETAIL-ANALYSI.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-DETAIL-ANALYSIS.

           IF NOT ( PRINT-DETAIL-ANALYSIS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-DETAIL-ANALYSI.

      *-----------------------------------------------------------------
       ENTER-PRINT-CLASS-TOTALS.

           MOVE LEGEND-PRINT-CLASS-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-CLASST." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-DETAIL-ANALYSI.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-CLASS-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-CLASS-TOTALS.

           IF NOT ( PRINT-CLASS-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-CLASS-TOTALS.

      *-----------------------------------------------------------------
       ENTER-PRINT-POOL-TOTALS.

           IF ( PRINT-BY-POOL = "N" )
              IF ( UP-KEY )
                 GO TO ENTER-PRINT-CLASS-TOTALS
              ELSE
                 MOVE "S  A010PRT-POOLT." TO VDU
                 MOVE "N"                 TO VDUBUF   PRINT-POOL-TOTALS
                 PERFORM SCREENIO
                 GO TO ENTER-PRINT-BRANCH-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-PRINT-POOL-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-POOLT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-CLASS-TOTALS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-POOL-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-POOL-TOTALS.

           IF NOT ( PRINT-POOL-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-POOL-TOTALS.

      *-----------------------------------------------------------------
       ENTER-PRINT-BRANCH-TOTALS.

           MOVE LEGEND-PRINT-BRANCH-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-BRANCHT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-POOL-TOTALS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-BRANCH-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-BRANCH-TOTALS.

           IF NOT ( PRINT-BRANCH-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-BRANCH-TOTALS.

      *-----------------------------------------------------------------
       ENTER-PRINT-GRAND-TOTALS.

           MOVE LEGEND-PRINT-GRAND-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-GRANDT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-BRANCH-TOTALS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-GRAND-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO PRINT-GRAND-TOTALS.

           IF NOT ( PRINT-GRAND-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-GRAND-TOTALS.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    D I S P L A Y - A C C R U A L - D A T E
      *
      ******************************************************************
       DISPLAY-ACCRUAL-DATE SECTION.

           IF ( ACCRUAL-DATE NOT = ZEROES )
              GO TO DISPLAY-ACCRUAL-DATE-EXIT.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO ACCRUAL-DATE.

           MOVE "S  8000ACCRUAL-DATE." TO VDU.
           MOVE ACCRUAL-DATE  TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       DISPLAY-ACCRUAL-DATE-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES ))
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN050BEG-POOL."        TO VDU.
           MOVE BEG-POOL                  TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN050END-POOL."        TO VDU.
           MOVE END-POOL                  TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-CLASS."       TO VDU.
           MOVE PRINT-BY-CLASS            TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-POOL."        TO VDU.
           MOVE PRINT-BY-POOL             TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-IB."          TO VDU.
           MOVE PRINT-LOAN-TYPE-IB        TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-IBX."         TO VDU.
           MOVE PRINT-LOAN-TYPE-IBX       TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-PC."          TO VDU.
           MOVE PRINT-LOAN-TYPE-PC        TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-DETAIL."      TO VDU.
           MOVE PRINT-DETAIL-ANALYSIS     TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-CLASST."      TO VDU.
           MOVE PRINT-CLASS-TOTALS        TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-POOLT."       TO VDU.
           MOVE PRINT-POOL-TOTALS         TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-BRANCHT."     TO VDU.
           MOVE PRINT-BRANCH-TOTALS       TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-GRANDT."      TO VDU.
           MOVE PRINT-GRAND-TOTALS        TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

           MOVE EXT-EOBUF-DATE2 TO ACCRUAL-DATE.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.

           MOVE GRSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO GRSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VALID SELECTION  }
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *LOAD-SP-TABLE SECTION.
      ******************************************************************

      *     MOVE ALL " "              TO QSP1-WBEG-ORGST
      *                                  SPA-ORGST.
      *     MOVE ALL "Z"              TO QSP1-WEND-ORGST.
      
      *     MOVE ALL "0"              TO QSP1-WBEG-SPRCLASS
      *                                  SPA-SPRCLASS
      *                                  QSP1-WBEG-SUBCLASS
      *                                  SPA-SUBCLASS
      *                                  QSP1-WBEG-LAWCODE
      *                                  SPA-LAWCODE.

      *     MOVE ALL "9"              TO QSP1-WEND-SPRCLASS 
      *                                  QSP1-WEND-SUBCLASS 
      *                                  QSP1-WEND-LAWCODE.

      *     PERFORM START-SP1-FILE.
      *     SET SP1-SUB TO 1.

      *LOAD-SP-TABLE-NEXT. 
       
      *    PERFORM READ-SP1-FILE-NEXT. 
      *    IF IO-FG = 9
      *       GO TO LOAD-SP-TABLE-NEXT-EXIT.

      *    MOVE SPA-REC TO SPA-TBL-REC(SP1-SUB).
      *    MOVE SPB-REC TO SPB-TBL-REC(SP1-SUB).
      *    MOVE SPC-REC TO SPC-TBL-REC(SP1-SUB).

      *    ADD 1        TO SP1-TBL-CTR.
      *    SET SP1-SUB  UP BY 1.
       
      *    GO TO LOAD-SP-TABLE-NEXT.

      *LOAD-SP-TABLE-NEXT-EXIT.
      *    EXIT.

      ******************************************************************
      *
      *    E X E C U T I O N - M O D U L E
      *
      ******************************************************************
       EXECUTION-MODULE SECTION.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
      D    STOP GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       EXECUTION-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO EXECUTION-MODULE-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXECUTION-PROCESS-BRANCH.

      *-----------------------------------------------------------------
       EXECUTION-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO EXECUTION-MODULE-END.

      *-----------------------------------------------------------------
       EXECUTION-PROCESS-BRANCH.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-END.

           PERFORM PRINT-REPORT-MODULE.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-PROGRESS-INDICATOR.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.
      ******************
      * NOTE: YOU DO NOT NEED TO MAKE ANY CHANGES FOR THESE PATHS FOR
      *       A32. THEY WILL GET ADJUSTED IN THEIR CORRESPONDING OPEN-XX
      ******************

      *     PERFORM OPEN-BW1-FILE.
      *     PERFORM OPEN-LN1-FILE.
           IF LN1-OPENED-SW = "N"
               PERFORM OPEN-LN1-FILE
               MOVE "Y" TO LN1-OPENED-SW.

           PERFORM OPEN-LTI1-FILE.

           MOVE ZEROES TO HOLD-LC-CODE.

           MOVE BR-NO         TO LN-PATH-OWNBR
                                 LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       PROCESS-BRANCH-NEXT-LN.

           PERFORM READ-LN1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-PROGRESS-INDICATOR.
           PERFORM TRAP-INTERRUPT.

           IF ( LN-DLPOOL < BEG-POOL ) OR ( LN-DLPOOL > END-POOL )
              GO TO PROCESS-BRANCH-NEXT-LN.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO DISPLAY-ACCTNO.

           MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF ( IO-BAD )
           IF BW-LNAME = ' '
              MOVE "*NOT ON FILE*" TO BW-LNAME
              MOVE SPACES          TO BW-FNAME.

           MOVE LN-CLASS TO LC-CODE.
           IF ( LC1-KEY NOT = HOLD-LC1-KEY )
               MOVE LC1-KEY TO HOLD-LC1-KEY
      *        PERFORM READ-LC1-FILE
      *        IF ( IO-BAD )
	           IF LC-DESC = ' ' 
                 GO TO IO-ERROR.

            MOVE LN-ORGST    TO SP-ORGST.
            MOVE LN-SPRCLASS TO SP-SPRCLASS.
            MOVE LN-SUBCLASS TO SP-SUBCLASS.
            MOVE LN-LAWCODE  TO SP-LAWCODE.

      *     PERFORM READ-SP1-FILE
      *     IF ( IO-BAD )
      *        GO TO IO-ERROR.

      *    IF ( LN-ORGST    = HOLD-SP-ORGST    AND
      *         LN-SPRCLASS = HOLD-SP-SPRCLASS AND
      *         LN-SUBCLASS = HOLD-SP-SUBCLASS AND
      *         LN-LAWCODE  = HOLD-SP-LAWCODE )
      *        NEXT SENTENCE
      *    ELSE       
      *        MOVE 'N'         TO SP1-FOUND-SW
      *        MOVE LN-ORGST    TO SP-ORGST
      *                            HOLD-SP-ORGST
      *        MOVE LN-SPRCLASS TO SP-SPRCLASS
      *                            HOLD-SP-SPRCLASS
      *        MOVE LN-SUBCLASS TO SP-SUBCLASS
      *                            HOLD-SP-SUBCLASS
      *        MOVE LN-LAWCODE  TO SP-LAWCODE
      *                            HOLD-SP-LAWCODE
      *        PERFORM VARYING SP1-SUB FROM 1 BY 1 
      *            UNTIL SP1-SUB > SP1-TBL-CTR
      *            IF ( LN-ORGST    = SPA-TBL-ORGST(SP1-SUB)    AND 
      *                 LN-SPRCLASS = SPA-TBL-SPRCLASS(SP1-SUB) AND
      *                 LN-SUBCLASS = SPA-TBL-SUBCLASS(SP1-SUB) AND
      *                 LN-LAWCODE  = SPA-TBL-LAWCODE(SP1-SUB) )
      *                MOVE SPA-TBL-REC(SP1-SUB) TO SPA-REC
      *                MOVE SPB-TBL-REC(SP1-SUB) TO SPB-REC 
      *                MOVE SPC-TBL-REC(SP1-SUB) TO SPC-REC
      *                PERFORM SET-ALL-TO-SP1 
      *                MOVE 'Y'                  TO SP1-FOUND-SW        
      *                MOVE SP1-TBL-CTR          TO SP1-SUB
      *            END-IF
      *        END-PERFORM.

 
      *    IF SP1-FOUND-SW = 'N'
      *        GO TO IO-ERROR.

           PERFORM ARRAYSP-READ.

           IF ( IO-BAD )
               GO TO IO-ERROR. 

      *         PERFORM READ-SP1-FILE
      *            IF ( IO-BAD )
      *                GO TO IO-ERROR
      *            ELSE
      *                ADD 3 TO SP-READ-CTR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO PROCESS-ACCTNO-STAT.

           PERFORM PROCESS-ACCTNO-PC.

           IF LN-ACCTNO = 078820
      D        STOP MESS
               MOVE SPACES TO MESS.          
 
           PERFORM PROCESS-ACCTNO-IB.

           GO TO PROCESS-BRANCH-NEXT-LN.

       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LTI1-FILE.

       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - A C C T N O - I B
      *
      ******************************************************************
       PROCESS-ACCTNO-IB SECTION.

           IF ( GP-IB-ACCRUALS-DONE NOT = "Y" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

           IF ( LC-SERVACCT = "Y" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SKIP LOANS PAIDOFF
      *    OR IN P&L
      *    OR IN RESCISSION
      *    OR LOANS THAT HAVE BEEN RESCINDED
      *    OR VOIDED LOANS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-CURBAL     = 0      ) OR
              ( LN-PLDATE NOT = 0      ) OR
              ( LN-PLCD NOT   = SPACES ) OR
              ( LN-POCD-INRESCISSION   ) OR
              ( LN-POCD-RESCIND        ) OR
              ( LN-POCD       = "**"   )
              GO TO PROCESS-ACCTNO-IB-EXIT.

           IF SP-MAX-IB-ACR-DAYS = 0   
              GO TO PROCESS-ACCTNO-IB-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    USED TO ALWAYS SKIP REPO ACCOUNTS ON IB ACCRUAL, FROM A60
      *    FORWARD.  AUTOMOTIVE CREDIT WANTED TO TAKE EARNINGS TILL
      *    ACCOUNT CHARGED OFF MERCURY, SKIP REPO ACCOUNTS:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GP-IB-ACCRUE-EARN-ON-REPO NOT = "Y"        ) AND
              ( LN-REPOCD                     = "X" OR "S" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

           MOVE ACCRUAL-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           IF (  IBPC-FG           = "I"  ) OR
              (( IBPC-FG           = "P" )  AND
               ( IBPC-EXPIRED-FLAG = "E" ))
              NEXT SENTENCE
           ELSE
              GO TO PROCESS-ACCTNO-IB-EXIT.

      * VALID RECORD TO PROCESS  

           MOVE "XX" TO PROCESS-ACCTNO-STAT.
           MOVE "IB" TO CURRENT-REPORT-TYPE.
           PERFORM LOAD-REPORT-IB-FIELDS.
           PERFORM UPDATE-REPORT-FILE.

      * CHECK FOR IB EXCEPTIONS  
           IF ( LOAD-IB-MAX NOT = SPACES )
              MOVE "IBX" TO CURRENT-REPORT-TYPE
              PERFORM LOAD-REPORT-IBX-FIELDS
              PERFORM UPDATE-REPORT-FILE.

       PROCESS-ACCTNO-IB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - A C C T N O - P C
      *
      ******************************************************************
       PROCESS-ACCTNO-PC SECTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SKIP LOANS IN RESCISSION AND LOANS THAT HAVE BEEN RESCINDED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-POCD-INRESCISSION ) OR
              ( LN-POCD-RESCIND      )
              GO TO PROCESS-ACCTNO-PC-EXIT.

      * DELETED ACCOUNT
           IF ( LN-POCD = "**" )
              MOVE LN-POFFDATE TO NUM-DATE
              IF ( LN-SALEFIN = "Y" )
                 MOVE LN-PURDATE  TO SYS-DATE
              ELSE
                 MOVE LN-LOANDATE TO SYS-DATE.

      * SKIP LOAN IF NOT DELETED THIS MONTH:
           IF (  LN-POCD = "**"                )  AND
              (( NUM-YR NOT = ACCRUAL-DATE-YY )  OR
               ( NUM-MO NOT = ACCRUAL-DATE-MM ))
              GO TO PROCESS-ACCTNO-PC-EXIT.

      * SKIP LOAN IF BOOKED AND DELETED THIS MONTH:
           IF ( LN-POCD = "**" ) AND
              ( NUM-YR = S-YY  ) AND
              ( NUM-MO = S-MM  )
              GO TO PROCESS-ACCTNO-PC-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-SERVACCT       = "Y"    ) AND
              ( LN-PRIOR-UNEARNED = ZEROES ) AND
              ( LN-NONACRERN      = ZEROES )
              GO TO PROCESS-ACCTNO-PC-EXIT.

           IF ( LN-PLDATE     NOT = ZEROES ) AND
              ( LN-PRIOR-UNEARNED = ZEROES ) AND
              ( LN-NONACRERN      = ZEROES )
              GO TO PROCESS-ACCTNO-PC-EXIT.

           IF ( LN-CURBAL         = ZEROES ) AND
              ( LN-PRIOR-UNEARNED = ZEROES ) AND
              ( LN-NONACRERN      = ZEROES )
              GO TO PROCESS-ACCTNO-PC-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JTG 000504 CORRECTED LOGIC ABOVE FOR GP-INCL-OTH-IN-EARNED FLAG.
      *            NEEDED ADDITIONAL TEST FOR WHEN ACCOUNT HAS
      *            LN-ANTICERN(6) AND GP-INCL-OTH-IN-EARNED = "N", STILL
      *            NEED TO TAKE AN ACCRUAL OF LOAN COSTS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM COMPUTE-EARNINGS.

           IF ( LN-LOANTYPE       = "I"  ) AND
              ( LN-POCD       NOT = "**" ) AND
              ( EARN-TOT-ANTICERN = 0    ) AND
              ( EARN-TOT-ANTICADJ = 0    )
              IF ( GP-INCL-OTH-IN-EARNED = "Y" )
                 GO TO PROCESS-ACCTNO-PC-EXIT
              ELSE
              IF ( LN-ANTICERN(6) = 0  AND LN-ANTICERN(4) = 0)
                 GO TO PROCESS-ACCTNO-PC-EXIT.

      * VALID RECORD TO PROCESS  

           MOVE "XX" TO PROCESS-ACCTNO-STAT.
           MOVE "PC" TO CURRENT-REPORT-TYPE.
           PERFORM LOAD-REPORT-PC-FIELDS.
           PERFORM UPDATE-REPORT-FILE.

       PROCESS-ACCTNO-PC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - C A L C - A D D O N - E A R N I N G S
      *
      *-----------------------------------------------------------------
      *    DESC:  COMPUTE NEW ACCRUAL EARNINGS NUMBERS
      *    OUT :  CALC-ADDON-EARNINGS.
      ******************************************************************
       LOAD-CALC-ADDON-EARNINGS SECTION.

           MOVE ZEROES TO CALC-ADDON-EARNINGS.

           MOVE LN-OWNBR        TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.

       LOAD-CALC-ADDON-EARN-NEXT.

           PERFORM READ-LTI1-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO LOAD-CALC-ADDON-EARN-EXIT.
        
           IF (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
              ( LTI-ACCTNO NOT = LN-ACCTNO )
              GO TO LOAD-CALC-ADDON-EARN-EXIT.

           IF NOT (LTI-INT-REBATE = SPACES OR "C")
              GO TO LOAD-CALC-ADDON-EARN-NEXT.

           IF ( LTI-INSPREM < 0      )
              MOVE -1 TO ONE
           ELSE
              MOVE +1 TO ONE.

           MOVE LTI-ORIG-1STPYDATE   TO AERN-LN-ORIG-1STPYDATE.
           MOVE LTI-ANTIC-START-DATE TO AERN-LN-LOANDATE
                                        AERN-LN-PURDATE
                                        AERN-LN-INTDATE.

           SUBTRACT LTI-PRIOR-NODEF FROM LN-TOTNODEF
                                 GIVING AERN-LN-TOTNODEF.

           MOVE LTI-ANTICTERM  TO AERN-LN-ANTICTERM.
           MOVE "N"           TO AERN-LN-SALEFIN.
           MOVE LTI-INT-REBATE TO AERN-LN-REBATE.

           MULTIPLY ONE BY LTI-ANTICERN(1) GIVING AERN-LN-ANTICERN.
           MULTIPLY ONE BY LTI-ANTICADJ(1) GIVING AERN-LN-ANTICADJ.
           MULTIPLY ONE BY LTI-UFERN   (1) GIVING AERN-LN-UFERN.
           MULTIPLY ONE BY LTI-ACCERN  (1) GIVING AERN-LN-ACCERN.

           MOVE ACCRUAL-DATE         TO AERN-ENDDATE.
           MOVE LN-LAST-ACCRUAL-DATE TO AERN-LAST-ACCRUAL-DATE.
           MOVE "Y"                  TO AERN-ADDON-FG.
           MOVE 2                    TO AERN.

           PERFORM COMPUTE-AERNINGS.


           MULTIPLY ONE BY AERN-EARNINGS(2).
           ADD AERN-EARNINGS(2) TO CALC-ADDON-EARNINGS
                                   LTI-ACCERN(1).

      *KC->MOVE AERN-EARNINGS(2) TO LTI-INT-LAST-EARNED.
      *KC->PERFORM REWRITE-LTI1-FILE.

           GO TO LOAD-CALC-ADDON-EARN-NEXT.

       LOAD-CALC-ADDON-EARN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - I B - F I E L D S
      *
      ******************************************************************
       LOAD-REPORT-IB-FIELDS SECTION.

           MOVE SPACES TO
                LOAD-IB-DISPLAY-LNNO        LOAD-IB-LNAME
                LOAD-IB-FNAME               LOAD-IB-MAX.

           MOVE ZEROES TO
                LOAD-IB-FROM-DATE           LOAD-IB-INT-SINCE-LSTPY
                LOAD-IB-INTBAL              LOAD-IB-TRUE-ACCRUE
                LOAD-IB-ACCRUED.

           MOVE ZEROES TO MAX-IB-INTEREST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DISPLAY-LNNO TO LOAD-IB-DISPLAY-LNNO.
           MOVE BW-LNAME     TO LOAD-IB-LNAME.
           MOVE BW-FNAME     TO LOAD-IB-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LOAD-REPORT-IB-FIELDS-STD.                  

       LOAD-REPORT-IB-FIELDS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - I B - F I E L D S - S T D
      *
      *-----------------------------------------------------------------
      *    NORMAL ACCOUNT (NON REVOLVING) IB SETUP FOR ACCRUAL
      ******************************************************************
       LOAD-REPORT-IB-FIELDS-STD SECTION.

           IF ( LN-INTPDTH-DATE = 0      )
              MOVE LN-INTDATE          TO LOAD-IB-FROM-DATE
           ELSE
              MOVE LN-INTPDTH-DATE     TO LOAD-IB-FROM-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS ROUTINE WILL ESTABLISH THE MAXIMUM INTEREST ASSESS
      *    FROM THE LAST ACCRUED DATE, PER SP-MAX-IB-ACR-DAYS:
      *
      *         RE: MOVE 1 TO INDU-MAX-ACCRUE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SP-MAX-IB-ACR-DAYS NOT = 0      )
              IF ( LN-INTPDTH-DATE = 0      )
                 MOVE LN-INTDATE      TO NDTE-DATE CALC-INTPDTH-DATE
              ELSE
                 MOVE LN-INTPDTH-DATE TO NDTE-DATE CALC-INTPDTH-DATE
              END-IF
              MOVE SP-MAX-IB-ACR-DAYS  TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE           TO RATE-DATE
              MOVE LN-MAKERCD(1)       TO RATE-MAKERCD
              MOVE "PY"                TO RATE-LPTRCD
              MOVE 1                   TO INDU-MAX-ACCRUE
              PERFORM SET-PY-RATE
              MOVE INDU-INTEREST       TO MAX-IB-INTEREST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS ROUTINE WILL ESTABLISH THE ACCRUED INTEREST ASSESS
      *    FROM THE LAST ACCRUED DATE
      *
      *         RE: MOVE 0 TO INDU-MAX-ACCRUE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ACCRUAL-DATE           TO RATE-DATE.
           MOVE LN-MAKERCD(1)          TO RATE-MAKERCD.
           MOVE "PY"                   TO RATE-LPTRCD.
           MOVE ZEROES                 TO INDU-MAX-ACCRUE.
           PERFORM SET-PY-RATE.
           MOVE INDU-INTEREST          TO LOAD-IB-INT-SINCE-LSTPY.
           MOVE LN-INTBAL              TO LOAD-IB-INTBAL.

           COMPUTE LOAD-IB-TRUE-ACCRUE = LN-INTBAL + INDU-INTEREST.

           IF ( SP-MAX-IB-ACR-DAYS NOT = 999             ) AND
              ( LOAD-IB-TRUE-ACCRUE    > MAX-IB-INTEREST )
              MOVE MAX-IB-INTEREST     TO LOAD-IB-ACCRUED
              MOVE "*"                 TO LOAD-IB-MAX
           ELSE
              MOVE LOAD-IB-TRUE-ACCRUE TO LOAD-IB-ACCRUED.

       LOAD-REPORT-IB-FD-STD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - I B X - F I E L D S
      *
      *-----------------------------------------------------------------
      *    NORMAL ACCOUNT (NON REVOLVING) IB EXCEPTION SETUP FOR ACCRUAL
      ******************************************************************
       LOAD-REPORT-IBX-FIELDS SECTION.

           MOVE SPACES TO
                LOAD-IBX-DISPLAY-LNNO       LOAD-IBX-LNAME
                LOAD-IBX-FNAME.

           MOVE SPACES TO
                LOAD-IBX-INT-PDTH-DATE      LOAD-IBX-INT-SINCE-LSTPY
                LOAD-IBX-INT-OWED           LOAD-IBX-TRUE-ACCRUE
                LOAD-IBX-INCOME-ACCRUE      LOAD-IBX-BALANCE
                LOAD-IBX-CONTR-PDTH-DATE    LOAD-IBX-CONTR-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DISPLAY-LNNO            TO LOAD-IBX-DISPLAY-LNNO.
           MOVE BW-LNAME                TO LOAD-IBX-LNAME.
           MOVE BW-FNAME                TO LOAD-IBX-FNAME.
           MOVE CALC-INTPDTH-DATE       TO LOAD-IBX-INT-PDTH-DATE.
           MOVE LOAD-IB-INT-SINCE-LSTPY TO LOAD-IBX-INT-SINCE-LSTPY.
           MOVE LOAD-IB-INTBAL          TO LOAD-IBX-INT-OWED.
           MOVE LOAD-IB-TRUE-ACCRUE     TO LOAD-IBX-TRUE-ACCRUE.
           MOVE LOAD-IB-ACCRUED         TO LOAD-IBX-INCOME-ACCRUE.
           MOVE LN-CURBAL               TO LOAD-IBX-BALANCE.

           MOVE LN-1STPYDATE            TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL          TO LOAD-IBX-CONTR-PDTH-DATE.

           MOVE ACCRUAL-DATE            TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE CONTRACTUAL             TO LOAD-IBX-CONTR-STATUS.

       LOAD-REPORT-IBX-FLDS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - P C - F I E L D S
      *
      ******************************************************************
       LOAD-REPORT-PC-FIELDS SECTION.

           MOVE SPACES TO
                LOAD-PC-DISPLAY-LNNO        LOAD-PC-LNAME
                LOAD-PC-FNAME.

           MOVE ZEROES TO
                LOAD-PC-ANTERM              LOAD-PC-ANTIC
                LOAD-PC-UNEARN              LOAD-PC-INT
                LOAD-PC-ADDON               LOAD-PC-SRV
                LOAD-PC-MFEE                LOAD-PC-DISC
                LOAD-PC-OTHER               LOAD-PC-CASH-EARN
                LOAD-PC-ENDING.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DISPLAY-LNNO            TO LOAD-PC-DISPLAY-LNNO.
           MOVE BW-LNAME                TO LOAD-PC-LNAME.
           MOVE BW-FNAME                TO LOAD-PC-FNAME.

      *- - - - - - - - - - - - - - - - - - -{ COMPUTE ACCRUAL EARNINGS }
           IF ( LN-POCD = "**" )
              MOVE ZEROES               TO EARN-TOT-UNEARN
           ELSE
              MOVE ACCRUAL-DATE         TO AERN-ENDDATE
              MOVE LN-LAST-ACCRUAL-DATE TO AERN-LAST-ACCRUAL-DATE
              PERFORM COMPUTE-AERNINGS
              MOVE AERN-EARNINGS(1)     TO LOAD-PC-INT
              MOVE AERN-EARNINGS(3)     TO LOAD-PC-SRV
              MOVE AERN-EARNINGS(4)     TO LOAD-PC-MFEE
              MOVE AERN-EARNINGS(5)     TO LOAD-PC-DISC
              MOVE AERN-EARNINGS(6)     TO LOAD-PC-OTHER
              MOVE 0                    TO LOAD-PC-ADDON
              IF ( LN-ADDON-FG   = "Y"           ) AND
                 ( LN-INT-REBATE = SPACES OR "C" )
                 PERFORM LOAD-CALC-ADDON-EARNINGS
                 MOVE CALC-ADDON-EARNINGS TO LOAD-PC-ADDON.

           MOVE LN-ANTICTERM            TO LOAD-PC-ANTERM.
           MOVE LN-NONACRERN            TO LOAD-PC-CASH-EARN.
           MOVE LN-PRIOR-UNEARNED       TO LOAD-PC-UNEARN.

           COMPUTE LOAD-PC-ANTIC         = EARN-TOT-ANTICADJ
                                         + EARN-TOT-ANTICERN.

           COMPUTE LOAD-PC-ENDING        = EARN-TOT-UNEARN
                                         - LOAD-PC-INT
                                         - LOAD-PC-ADDON
                                         - LOAD-PC-SRV
                                         - LOAD-PC-DISC.

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              SUBTRACT LOAD-PC-OTHER FROM LOAD-PC-ENDING.

      * DOES NOT WANT TO INCLUDE MFEE (THEY USE TO AMORTIZE
      * PARTICIPATION EXPENSE
           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              SUBTRACT LOAD-PC-MFEE FROM LOAD-PC-ENDING.

       LOAD-REPORT-PC-FIELDS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - M O D U L E
      *
      ******************************************************************
       PRINT-REPORT-MODULE SECTION.

           MOVE SPACES               TO CURRENT-REPORT-TYPE.
           MOVE ZEROES               TO HOLD-BRANCH.
           MOVE ZEROES               TO HOLD-CLASS.
           MOVE ZEROES               TO PAGE-NUMBER-TOTAL.
           MOVE ZEROES               TO PAGE-NUMBER-TYPE.
           MOVE PROGID-TEXT-PRINTING TO PROGID-TEXT.
           MOVE 9999                 TO PROGID-SLEEP-COUNTER.

      *- - - - - - - - - - - - - - - - - - - - -{ START 'REPORT' FILE  }
           MOVE SPACES TO REPORT-TYPE     HOLD-REPORT-TYPE.
           MOVE ZEROES TO REPORT-BRANCH   HOLD-REPORT-BRANCH.
           MOVE ZEROES TO REPORT-POOL     HOLD-REPORT-POOL.
           MOVE ZEROES TO REPORT-CLASS    HOLD-REPORT-CLASS.
           MOVE ZEROES TO REPORT-ACCTNO   HOLD-REPORT-ACCTNO.
           PERFORM START-REPORT-FILE.

      *=================================================================
       PRINT-REPORT-RECORD-NEXT.

           PERFORM READ-REPORT-FILE-NEXT.

           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-END.

           MOVE REPORT-BRANCH TO BR-NO.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-PROGRESS-INDICATOR.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - -{ CHECK REPORT TYPE  }
           IF (( PRINT-LOAN-TYPE-IB  = "N"  ) AND ( REPORT-TYPE-IB )) OR
              (( PRINT-LOAN-TYPE-IBX = "N"  ) AND ( REPORT-TYPE-IBX)) OR
              (( PRINT-LOAN-TYPE-PC  = "N"  ) AND ( REPORT-TYPE-PC ))
              GO TO PRINT-REPORT-RECORD-NEXT.

      *- - - - - - - - - - - - - - - - - - - - -{ CHECK REPORT OPTIONS }
           IF ( REPORT-GRAND-TOTAL )
              IF ( PRINT-GRAND-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( REPORT-BRANCH-TOTAL )
              IF ( PRINT-BRANCH-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( REPORT-POOL-TOTAL )
              IF ( PRINT-POOL-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( REPORT-CLASS-TOTAL )
              IF ( PRINT-CLASS-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( PRINT-DETAIL-ANALYSIS = "Y"  )
              GO TO PRINT-REPORT-RECORD-VALID.

           GO TO PRINT-REPORT-RECORD-NEXT.

      *-----------------------------------------------------------------
       PRINT-REPORT-RECORD-VALID.

           PERFORM PRINT-REPORT-PAGE-BREAK.

           IF ( REPORT-TYPE-IB  ) PERFORM PRINT-REPORT-IB.
           IF ( REPORT-TYPE-IBX ) PERFORM PRINT-REPORT-IBX.
           IF ( REPORT-TYPE-PC  ) PERFORM PRINT-REPORT-PC.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO PRINT-REPORT-RECORD-NEXT.

       PRINT-REPORT-MODULE-END.

           PERFORM PRINT-REPORT-RANGES.

       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - P A G E - B R E A K
      *
      *-----------------------------------------------------------------
      *    PAGE BREAK ONLY WHEN GOING TO PRINT A 'DETAIL' RECORD AFTER
      *    JUST PRINTING A 'TOTAL' RECORD; OR IF A CHANGE OF REPORT TYPE
      ******************************************************************
       PRINT-REPORT-PAGE-BREAK SECTION.

           IF (( NOT CURRENT-REPORT-TYPE-IB ) AND ( REPORT-TYPE-IB )) OR
              (( NOT CURRENT-REPORT-TYPE-IBX) AND ( REPORT-TYPE-IBX)) OR
              (( NOT CURRENT-REPORT-TYPE-PC ) AND ( REPORT-TYPE-PC ))
              MOVE REPORT-TYPE TO CURRENT-REPORT-TYPE
              MOVE 99          TO LN-COUNT
              MOVE ZEROES      TO PAGE-NUMBER-TYPE
              GO TO PRINT-REPORT-PG-BREAK-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO NOT CHANGE LN-COUNT IF PRINTING A 'TOTAL' RECORD.
      *    ALLOW TOTALS TO PRINT ON SAME PAGE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL   ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO PRINT-REPORT-PG-BREAK-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    AT THIS POINT THE CURRENT RECORD IS FOR A 'DETAIL' RECORD.
      *    IF THE PREVOIUS RECORD WAS A 'TOTAL' RECORD, DO A PAGE BREAK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( HOLD-REPORT-GRAND-TOTAL  ) OR
              ( HOLD-REPORT-BRANCH-TOTAL ) OR
              ( HOLD-REPORT-POOL-TOTAL   ) OR
              ( HOLD-REPORT-CLASS-TOTAL  )
              MOVE 99 TO LN-COUNT.

       PRINT-REPORT-PG-BREAK-END.

           MOVE REPORT-KEY TO HOLD-REPORT-KEY.

       PRINT-REPORT-PG-BREAK-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - R A N G E S
      *
      ******************************************************************
       PRINT-REPORT-RANGES SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO PRINT-REPORT-RANGES-EXIT.

           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING POOL ID             : "    TO R01-PROMPT.
           MOVE BEG-POOL                              TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    POOL ID             : "    TO R01-PROMPT.
           MOVE BEG-POOL                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACCRUAL DATE (MM/DD/YY)       : "    TO R01-PROMPT.
           MOVE ACCRUAL-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                           TO R01-DATE.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPORT BREAKDOWN BY CLASS     : "    TO R01-PROMPT.
           MOVE PRINT-BY-CLASS                        TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT BREAKDOWN BY POOL      : "    TO R01-PROMPT.
           MOVE PRINT-BY-POOL                         TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPORT INTEREST BEARING       : "    TO R01-PROMPT.
           MOVE PRINT-LOAN-TYPE-IB                    TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT INTEREST BEARING EXCEPT: "    TO R01-PROMPT.
           MOVE PRINT-LOAN-TYPE-IBX                   TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT PRECOMPUTE             : "    TO R01-PROMPT.
           MOVE PRINT-LOAN-TYPE-PC                    TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT DETAIL ANALYSIS         : "    TO R01-PROMPT.
           MOVE PRINT-DETAIL-ANALYSIS                 TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT CLASS  TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-CLASS-TOTALS                    TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT POOL   TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-POOL-TOTALS                     TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT BRANCH TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-BRANCH-TOTALS                   TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT GRAND  TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-GRAND-TOTALS                    TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-REPORT-RANGES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - I B
      *
      ******************************************************************
       PRINT-REPORT-IB SECTION.

           IF ( REPORT-GRAND-TOTAL  )  OR
              ( REPORT-BRANCH-TOTAL )  OR
              ( REPORT-POOL-TOTAL   )  OR
              ( REPORT-CLASS-TOTAL  )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 GO TO PRINT-REPORT-IB-TOTAL
              ELSE
                 GO TO PRINT-REPORT-IB-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-REPORT-IB-DETAIL.

           MOVE REPORT-IB-DISPLAY-LNNO     TO D01-IB-DISPLAY-LNNO.

           STRING REPORT-IB-LNAME, ", ", REPORT-IB-FNAME
                  DELIMITED BY "  "      INTO D01-IB-NAME.

           MOVE REPORT-IB-FROM-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D01-IB-FROM.
           MOVE REPORT-IB-INT-SINCE-LSTPY  TO D01-IB-INT-SINCE-LSTPY.
           MOVE REPORT-IB-INTBAL           TO D01-IB-INTBAL.
           MOVE REPORT-IB-TRUE-ACCRUE      TO D01-IB-TRUE-ACCRUE.
           MOVE REPORT-IB-ACCRUED          TO D01-IB-ACCRUED.
           MOVE REPORT-IB-MAX              TO D01-IB-MAX.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
           GO TO PRINT-REPORT-IB-EXIT.

      *===========================================[ TOTAL SUMMARY ONLY ]
       PRINT-REPORT-IB-SUMMARY.

           IF ( REPORT-GRAND-TOTAL )
              MOVE "****"                  TO S01-IB-BRANCHX
           ELSE
              MOVE REPORT-BRANCH           TO S01-IB-BRANCH.

           IF ( REPORT-BRANCH-TOTAL )
              MOVE "****"                  TO S01-IB-POOLX
           ELSE
              MOVE REPORT-POOL             TO S01-IB-POOL.

           IF ( REPORT-POOL-TOTAL )
              MOVE "**"                    TO S01-IB-CLASSX
           ELSE
              MOVE REPORT-CLASS            TO S01-IB-CLASS.

           MOVE REPORT-IB-NUMBER-ACCTS     TO S01-IB-NUMBER-ACCTS.
           MOVE "#"                        TO S01-IB-NUMBER-ACCTS-FG.
           MOVE REPORT-IB-INT-SINCE-LSTPY  TO S01-IB-INT-SINCE-LSTPY.
           MOVE REPORT-IB-INTBAL           TO S01-IB-INTBAL.
           MOVE REPORT-IB-TRUE-ACCRUE      TO S01-IB-TRUE-ACCRUE.
           MOVE REPORT-IB-ACCRUED          TO S01-IB-ACCRUED.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IB-EXIT.

       PRINT-REPORT-IB-TOTAL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE 01  }
           IF ( REPORT-GRAND-TOTAL )
              MOVE "GRAND TOTALS         " TO T01-IB-PROMPT
           ELSE
           IF ( REPORT-BRANCH-TOTAL )
              MOVE "BRANCH TOTALS        " TO T01-IB-PROMPT
           ELSE
           IF ( REPORT-POOL-TOTAL )
              MOVE "POOL TOTALS          " TO T01-IB-PROMPT
           ELSE
              MOVE "CLASS TOTALS         " TO T01-IB-PROMPT.

           MOVE REPORT-IB-NUMBER-ACCTS     TO T01-IB-NUMBER-ACCTS.
           MOVE "#"                        TO T01-IB-NUMBER-ACCTS-FG.
           MOVE REPORT-IB-INT-SINCE-LSTPY  TO T01-IB-INT-SINCE-LSTPY.
           MOVE REPORT-IB-INTBAL           TO T01-IB-INTBAL.
           MOVE REPORT-IB-TRUE-ACCRUE      TO T01-IB-TRUE-ACCRUE.
           MOVE REPORT-IB-ACCRUED          TO T01-IB-ACCRUED.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IB-EXIT.

       PRINT-REPORT-IB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - I B X
      *
      ******************************************************************
       PRINT-REPORT-IBX SECTION.

           IF ( REPORT-GRAND-TOTAL  )  OR
              ( REPORT-BRANCH-TOTAL )  OR
              ( REPORT-POOL-TOTAL   )  OR
              ( REPORT-CLASS-TOTAL  )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 GO TO PRINT-REPORT-IBX-TOTAL
              ELSE
                 GO TO PRINT-REPORT-IBX-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-REPORT-IBX-DETAIL.

           MOVE REPORT-IBX-DISPLAY-LNNO    TO D02-IBX-DISPLAY-LNNO.

           STRING REPORT-IBX-LNAME, ", ", REPORT-IBX-FNAME
                  DELIMITED BY "  "      INTO D02-IBX-NAME.

           MOVE REPORT-IBX-INT-PDTH-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D02-IBX-INT-PDTH.
           MOVE REPORT-IBX-INT-SINCE-LSTPY TO D02-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED        TO D02-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE     TO D02-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE   TO D02-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE         TO D02-IBX-BALANCE.
           MOVE REPORT-IBX-CONTR-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D02-IBX-CONT-PDTH.
           MOVE REPORT-IBX-CONTR-STATUS    TO D02-IBX-CONT-STATUS.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
           GO TO PRINT-REPORT-IBX-EXIT.

      *===========================================[ TOTAL SUMMARY ONLY ]
       PRINT-REPORT-IBX-SUMMARY.

           IF ( REPORT-GRAND-TOTAL )
              MOVE "****"                  TO S02-IBX-BRANCHX
           ELSE
              MOVE REPORT-BRANCH           TO S02-IBX-BRANCH.

           IF ( REPORT-BRANCH-TOTAL )
              MOVE "****"                  TO S02-IBX-POOLX
           ELSE
              MOVE REPORT-POOL             TO S02-IBX-POOL.

           IF ( REPORT-POOL-TOTAL )
              MOVE "**"                    TO S02-IBX-CLASSX
           ELSE
              MOVE REPORT-CLASS            TO S02-IBX-CLASS.

           MOVE REPORT-IBX-NUMBER-ACCTS    TO S02-IBX-NUMBER-ACCTS.
           MOVE "#"                        TO S02-IBX-NUMBER-ACCTS-FG.
           MOVE REPORT-IBX-INT-SINCE-LSTPY TO S02-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED        TO S02-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE     TO S02-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE   TO S02-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE         TO S02-IBX-BALANCE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IBX-EXIT.

      *=================================================================
       PRINT-REPORT-IBX-TOTAL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE 01  }
           IF ( REPORT-GRAND-TOTAL )
              MOVE "GRAND TOTALS          " TO T02-IBX-PROMPT
           ELSE
           IF ( REPORT-BRANCH-TOTAL )
              MOVE "BRANCH TOTALS         " TO T02-IBX-PROMPT
           ELSE
           IF ( REPORT-POOL-TOTAL )
              MOVE "POOL TOTALS           " TO T02-IBX-PROMPT
           ELSE
              MOVE "CLASS TOTALS          " TO T02-IBX-PROMPT.

           MOVE REPORT-IBX-NUMBER-ACCTS     TO T02-IBX-NUMBER-ACCTS.
           MOVE "#"                         TO T02-IBX-NUMBER-ACCTS-FG.
           MOVE REPORT-IBX-INT-SINCE-LSTPY  TO T02-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED         TO T02-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE      TO T02-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE    TO T02-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE          TO T02-IBX-BALANCE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IBX-EXIT.

       PRINT-REPORT-IBX-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - P C
      *
      ******************************************************************
       PRINT-REPORT-PC SECTION.

           IF ( REPORT-GRAND-TOTAL  )  OR
              ( REPORT-BRANCH-TOTAL )  OR
              ( REPORT-POOL-TOTAL   )  OR
              ( REPORT-CLASS-TOTAL  )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 GO TO PRINT-REPORT-PC-TOTAL
              ELSE
                 GO TO PRINT-REPORT-PC-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-REPORT-PC-DETAIL.

           MOVE REPORT-PC-DISPLAY-LNNO     TO D03-PC-DISPLAY-LNNO.

           STRING REPORT-PC-LNAME, ", ", REPORT-PC-FNAME
                  DELIMITED BY "  "      INTO D03-PC-NAME.

           MOVE REPORT-PC-ANTERM           TO D03-PC-ANTERM.
           MOVE REPORT-PC-ANTIC            TO D03-PC-ANTIC.
           MOVE REPORT-PC-UNEARN           TO D03-PC-UNEARN.
           MOVE REPORT-PC-INT              TO D03-PC-INT.
           MOVE REPORT-PC-ADDON            TO D03-PC-ADDON.
           MOVE REPORT-PC-SRV              TO D03-PC-SRV.
           MOVE REPORT-PC-MFEE             TO D03-PC-MFEE.
           MOVE REPORT-PC-DISC             TO D03-PC-DISC.
           MOVE REPORT-PC-OTHER            TO D03-PC-OTHER.
           MOVE REPORT-PC-CASH-EARN        TO D03-PC-CASH-EARN.
           MOVE REPORT-PC-ENDING           TO D03-PC-ENDING.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PC-EXIT.

      *===========================================[ TOTAL SUMMARY ONLY ]
       PRINT-REPORT-PC-SUMMARY.

           IF ( REPORT-GRAND-TOTAL )
              MOVE "****"                  TO S03-PC-BRANCHX
           ELSE
              MOVE REPORT-BRANCH           TO S03-PC-BRANCH.

           IF ( REPORT-BRANCH-TOTAL )
              MOVE "****"                  TO S03-PC-POOLX
           ELSE
              MOVE REPORT-POOL             TO S03-PC-POOL.

           IF ( REPORT-POOL-TOTAL )
              MOVE "**"                    TO S03-PC-CLASSX
           ELSE
              MOVE REPORT-CLASS            TO S03-PC-CLASS.

           MOVE REPORT-PC-NUMBER-ACCTS     TO S03-PC-NUMBER-ACCTS.
           MOVE "#"                        TO S03-PC-NUMBER-ACCTS-FG.
           MOVE REPORT-PC-UNEARN           TO S03-PC-UNEARN.
           MOVE REPORT-PC-INT              TO S03-PC-INT.
           MOVE REPORT-PC-ADDON            TO S03-PC-ADDON.
           MOVE REPORT-PC-SRV              TO S03-PC-SRV.
           MOVE REPORT-PC-MFEE             TO S03-PC-MFEE.
           MOVE REPORT-PC-DISC             TO S03-PC-DISC.
           MOVE REPORT-PC-OTHER            TO S03-PC-OTHER.
           MOVE REPORT-PC-CASH-EARN        TO S03-PC-CASH-EARN.
           MOVE REPORT-PC-ENDING           TO S03-PC-ENDING.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PC-EXIT.

       PRINT-REPORT-PC-TOTAL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE 01  }
           IF ( REPORT-GRAND-TOTAL )
              MOVE "GRAND TOTALS          " TO T03-PC-PROMPT
           ELSE
           IF ( REPORT-BRANCH-TOTAL )
              MOVE "BRANCH TOTALS         " TO T03-PC-PROMPT
           ELSE
           IF ( REPORT-POOL-TOTAL )
              MOVE "POOL TOTALS           " TO T03-PC-PROMPT
           ELSE
              MOVE "CLASS TOTALS          " TO T03-PC-PROMPT.

           MOVE REPORT-PC-NUMBER-ACCTS     TO T03-PC-NUMBER-ACCTS.
           MOVE "#"                        TO T03-PC-NUMBER-ACCTS-FG.
           MOVE REPORT-PC-UNEARN           TO T03-PC-UNEARN.
           MOVE REPORT-PC-ADDON            TO T03-PC-ADDON.
           MOVE REPORT-PC-MFEE             TO T03-PC-MFEE.
           MOVE REPORT-PC-OTHER            TO T03-PC-OTHER.
           MOVE REPORT-PC-ENDING           TO T03-PC-ENDING.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE 02  }
           MOVE REPORT-PC-INT              TO T04-PC-INT.
           MOVE REPORT-PC-SRV              TO T04-PC-SRV.
           MOVE REPORT-PC-DISC             TO T04-PC-DISC.
           MOVE REPORT-PC-CASH-EARN        TO T04-PC-CASH-EARN.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PC-EXIT.

       PRINT-REPORT-PC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - F I L E
      *
      ******************************************************************
       UPDATE-REPORT-FILE SECTION.

           MOVE CURRENT-REPORT-TYPE TO HOLD-REPORT-TYPE.
           MOVE LN-OWNBR            TO HOLD-REPORT-BRANCH.
           MOVE ZEROES              TO HOLD-REPORT-POOL.
           MOVE LN-CLASS            TO HOLD-REPORT-CLASS.
           MOVE LN-ACCTNO           TO HOLD-REPORT-ACCTNO.

           IF ( PRINT-BY-POOL = "Y" )
              MOVE LN-DLPOOL      TO HOLD-REPORT-POOL.

      * UPDATE: DETAIL 
           PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: CLASS 
           MOVE 99999999 TO HOLD-REPORT-ACCTNO.
           PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: POOL 
           MOVE 9999     TO HOLD-REPORT-CLASS.

           IF ( PRINT-BY-POOL = "Y" )
              PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: BRANCH
           MOVE 999999   TO HOLD-REPORT-POOL.
           PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: GRAND 
           MOVE 99999    TO HOLD-REPORT-BRANCH.
           PERFORM UPDATE-REPORT-RECORD.

       UPDATE-REPORT-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D
      *
      ******************************************************************
       UPDATE-REPORT-RECORD SECTION.

           MOVE HOLD-REPORT-KEY TO REPORT-KEY.
           PERFORM READ-REPORT-FILE.

           IF ( IO-BAD )
              PERFORM UPDATE-REPORT-RECORD-CLEAR
              PERFORM WRITE-REPORT-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( REPORT-TYPE-IB   ) PERFORM UPDATE-REPORT-RECORD-IB.
           IF ( REPORT-TYPE-IBX ) PERFORM UPDATE-REPORT-RECORD-IBX.
           IF ( REPORT-TYPE-PC   ) PERFORM UPDATE-REPORT-RECORD-PC.

       UPDATE-REPORT-RECORD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - C L E A R
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-CLEAR SECTION.

           MOVE SPACES          TO REPORT-REC.
           MOVE HOLD-REPORT-KEY TO REPORT-KEY.

           IF ( REPORT-TYPE-IB )
              MOVE ZEROES TO
                   REPORT-IB-FROM-DATE       REPORT-IB-INT-SINCE-LSTPY
                   REPORT-IB-INTBAL          REPORT-IB-TRUE-ACCRUE
                   REPORT-IB-ACCRUED         REPORT-IB-MAX
                   REPORT-IB-NUMBER-ACCTS.

           IF ( REPORT-TYPE-IBX )
              MOVE ZEROES TO
                   REPORT-IBX-INT-PDTH-DATE
                   REPORT-IBX-INT-SINCE-LSTPY
                   REPORT-IBX-INT-OWED
                   REPORT-IBX-TRUE-ACCRUE
                   REPORT-IBX-INCOME-ACCRUE
                   REPORT-IBX-BALANCE
                   REPORT-IBX-CONTR-PDTH-DATE
                   REPORT-IBX-CONTR-STATUS  REPORT-IBX-NUMBER-ACCTS.

           IF ( REPORT-TYPE-PC )
              MOVE ZEROES TO
                   REPORT-PC-ANTERM          REPORT-PC-ANTIC
                   REPORT-PC-UNEARN          REPORT-PC-INT
                   REPORT-PC-ADDON           REPORT-PC-SRV
                   REPORT-PC-MFEE            REPORT-PC-DISC
                   REPORT-PC-OTHER           REPORT-PC-CASH-EARN
                   REPORT-PC-ENDING          REPORT-PC-NUMBER-ACCTS.

       UPDATE-REPORT-REC-CLR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - I B
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-IB SECTION.

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-RECORD-IB-SKIP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LOAD-IB-DISPLAY-LNNO     TO REPORT-IB-DISPLAY-LNNO.
           MOVE LOAD-IB-LNAME            TO REPORT-IB-LNAME.
           MOVE LOAD-IB-FNAME            TO REPORT-IB-FNAME.
           MOVE LOAD-IB-FROM-DATE        TO REPORT-IB-FROM-DATE.
           MOVE LOAD-IB-MAX              TO REPORT-IB-MAX.

      *-----------------------------------------------------------------
       UPDATE-REPORT-RECORD-IB-SKIP.

           ADD  LOAD-IB-INT-SINCE-LSTPY  TO REPORT-IB-INT-SINCE-LSTPY.
           ADD  LOAD-IB-INTBAL           TO REPORT-IB-INTBAL.
           ADD  LOAD-IB-TRUE-ACCRUE      TO REPORT-IB-TRUE-ACCRUE.
           ADD  LOAD-IB-ACCRUED          TO REPORT-IB-ACCRUED.
           ADD  01                       TO REPORT-IB-NUMBER-ACCTS.

      *- - - - - - - - - - - - - - - - - -{ UPDATING TO 'REPORT' FILE  }
           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-RECORD-IB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - I B E X
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-IBX SECTION.

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-RC-IBX-SKIP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LOAD-IBX-DISPLAY-LNNO    TO REPORT-IBX-DISPLAY-LNNO.
           MOVE LOAD-IBX-LNAME           TO REPORT-IBX-LNAME.
           MOVE LOAD-IBX-FNAME           TO REPORT-IBX-FNAME.
           MOVE LOAD-IBX-INT-PDTH-DATE   TO REPORT-IBX-INT-PDTH-DATE.
           MOVE LOAD-IBX-CONTR-PDTH-DATE TO REPORT-IBX-CONTR-PDTH-DATE.
           MOVE LOAD-IBX-CONTR-STATUS    TO REPORT-IBX-CONTR-STATUS.

      *-----------------------------------------------------------------
       UPDATE-REPORT-RC-IBX-SKIP.

           ADD    LOAD-IBX-INT-SINCE-LSTPY      TO
                REPORT-IBX-INT-SINCE-LSTPY.

           ADD    LOAD-IBX-INT-OWED             TO
                REPORT-IBX-INT-OWED.

           ADD    LOAD-IBX-TRUE-ACCRUE          TO
                REPORT-IBX-TRUE-ACCRUE.

           ADD    LOAD-IBX-INCOME-ACCRUE        TO
                REPORT-IBX-INCOME-ACCRUE.

           ADD    LOAD-IBX-BALANCE              TO
                REPORT-IBX-BALANCE.

           ADD  01 TO REPORT-IBX-NUMBER-ACCTS.

      *- - - - - - - - - - - - - - - - - -{ UPDATING TO 'REPORT' FILE  }
           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-RC-IBX-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - P C
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-PC SECTION.

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL   ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-REC-PC-SKIP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LOAD-PC-DISPLAY-LNNO      TO REPORT-PC-DISPLAY-LNNO.
           MOVE LOAD-PC-LNAME             TO REPORT-PC-LNAME.
           MOVE LOAD-PC-LNAME             TO REPORT-PC-FNAME.
           MOVE LOAD-PC-ANTERM            TO REPORT-PC-ANTERM.

      *-----------------------------------------------------------------
       UPDATE-REPORT-REC-PC-SKIP.

           ADD  LOAD-PC-ANTIC             TO REPORT-PC-ANTIC.
           ADD  LOAD-PC-UNEARN            TO REPORT-PC-UNEARN.
           ADD  LOAD-PC-INT               TO REPORT-PC-INT.
           ADD  LOAD-PC-ADDON             TO REPORT-PC-ADDON.
           ADD  LOAD-PC-SRV               TO REPORT-PC-SRV.
           ADD  LOAD-PC-MFEE              TO REPORT-PC-MFEE.
           ADD  LOAD-PC-DISC              TO REPORT-PC-DISC.
           ADD  LOAD-PC-OTHER             TO REPORT-PC-OTHER.
           ADD  LOAD-PC-CASH-EARN         TO REPORT-PC-CASH-EARN.
           ADD  LOAD-PC-ENDING            TO REPORT-PC-ENDING.
           ADD  01                        TO REPORT-PC-NUMBER-ACCTS.

      *- - - - - - - - - - - - - - - - - -{ UPDATING TO 'REPORT' FILE  }
           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-REC-PC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER-LINES.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R - L I N E S
      *
      ******************************************************************
       WRITE-PAGE-HEADER-LINES SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER-TOTAL.
           ADD  01 TO PAGE-NUMBER-TYPE
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - -{ GET 'BR' RECORD  }
           IF ( PRINT-DETAIL-ANALYSIS = "N" )
              GO TO WRITE-PAGE-HEADER-CONTINUE.

           IF ( REPORT-BRANCH NOT = HOLD-BRANCH ) AND
              ( NOT REPORT-GRAND-TOTAL          )
              MOVE REPORT-BRANCH TO BR-NO HOLD-BRANCH
              PERFORM READ-BR-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - -{ GET 'CD1' RECORD }
           IF ( REPORT-POOL = ZEROES )
              MOVE "* NO POOL ID FOR LOANS *"    TO CD-DESC
           ELSE
           IF ( REPORT-POOL NOT = HOLD-POOL ) AND
              ( NOT REPORT-BRANCH-TOTAL     )
              MOVE "PL"                          TO CD-TYPE
              MOVE REPORT-POOL                   TO CD-PL-POOLID
              MOVE SPACES                        TO CD-PL-FILLER
              PERFORM READ-CD1-FILE
              IF ( IO-BAD )
                 MOVE "* NOT ON FILE *         " TO CD-DESC.

      *- - - - - - - - - - - - - - - - - - - - - - -{ GET 'LC1' RECORD }
           IF ( REPORT-CLASS NOT = HOLD-CLASS ) AND
              ( NOT REPORT-POOL-TOTAL         )
              MOVE REPORT-CLASS TO LC-CODE HOLD-CLASS
              PERFORM READ-LC1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

      *-------------------------------------------------------------{01}
       WRITE-PAGE-HEADER-CONTINUE.

           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER-TYPE                 TO H01-PAGE-TYPE.
           MOVE PAGE-NUMBER-TOTAL                TO H01-PAGE-TOTAL.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE ACCRUAL-DATE                     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                      TO H02-ACCRUAL-DATE.

           IF ( CURRENT-REPORT-TYPE-IB )
              MOVE HEADER-TITLE-TYPE-IB          TO H02-TITLE
           ELSE
           IF ( CURRENT-REPORT-TYPE-IBX )
              MOVE HEADER-TITLE-TYPE-IBX         TO H02-TITLE
           ELSE
           IF ( CURRENT-REPORT-TYPE-PC )
              MOVE HEADER-TITLE-TYPE-PC          TO H02-TITLE
           ELSE
              MOVE SPACES                        TO H02-TITLE.

           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           IF ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE BR-NO                         TO H03-BRANCH
              MOVE BR-NAME                       TO H03-BRANCH-DESC
           ELSE
              MOVE "****"                        TO H03-BRANCHX
              MOVE "*ALL BRANCHES*"              TO H03-BRANCH-DESC.

           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           IF ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE REPORT-POOL                   TO H04-POOL
              MOVE CD-DESC                       TO H04-POOL-DESC
           ELSE
              MOVE "*****"                       TO H04-POOLX
              MOVE "*ALL POOLS*"                 TO H04-POOL-DESC.

           IF ( PRINT-BY-POOL = "Y" )
               MOVE HEADER-LINE-04               TO PR-REC
               ADD  01                           TO LN-COUNT
               MOVE 01                           TO LN-FEED
               PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{05}
           IF ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE REPORT-CLASS                  TO H05-CLASS
              MOVE LC-DESC                       TO H05-CLASS-DESC
           ELSE
              MOVE "**"                          TO H05-CLASSX
              MOVE "*ALL CLASSES*"               TO H05-CLASS-DESC.

           MOVE SPACES                           TO H05-MTD-NON.

           IF ( CURRENT-REPORT-TYPE-PC      ) AND
              ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE "MTD-NON"                     TO H05-MTD-NON.

           MOVE HEADER-LINE-05                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{06}
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IB-01          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IB-01-SUMMARY  TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IBX-01         TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IBX-01-SUMMARY TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-PC-01          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PC-01-SUMMARY  TO PR-REC.

           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{07}
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IB-02          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IB-02-SUMMARY  TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IBX-02         TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IBX-02-SUMMARY TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-PC-02          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PC-02-SUMMARY  TO PR-REC.

           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{08}
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE ALL "="                    TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IB-03-SUMMARY  TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE ALL "="                    TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IBX-03-SUMMARY TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE ALL "="                    TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PC-03-SUMMARY  TO PR-REC.

           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{09}
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE SPACES                     TO PR-REC
              ELSE
                 MOVE ALL "="                    TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE SPACES                     TO PR-REC
              ELSE
                 MOVE ALL "="                    TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE SPACES                     TO PR-REC
              ELSE
                 MOVE ALL "="                    TO PR-REC.

           IF ( PR-REC NOT = SPACES )
              ADD  01 TO LN-COUNT
              MOVE 01 TO LN-FEED
              PERFORM WRITE-PR-REC.

       WRITE-PAGE-HEADER-LN-END.

           MOVE 01 TO LN-FEED.

       WRITE-PAGE-HEADER-LN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/ARRAYSP.CPY".

       COPY "LIBLP/LPAERN.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPCERN.CPY".
       COPY "LIBLP/LPEADJ.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPREGZ.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPUERN.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO AC1DAY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AC1DAY-SYSDATE.
           DISPLAY AC1DAY-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AC1DAY-FORM-FIELDS TO WR-BUF.
           MOVE AC1DAY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE AC1DAY-HELP--FILE TO HELP-LINK-FILE.
           MOVE AC1DAY-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AC1DAY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=======================================================[ PROGID ]
       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

           MOVE BR-NO     TO PROGID-BRANCH.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

      * COPY "LIBLP/LPCD1RN.CPY".
      * COPYMEMBER: LIBLP/LPCD1RN
       LOAD-CD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CD-ALL-BASE
              MOVE FILPATH-MACHINE         TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CDFILE NOT = "S" )
              MOVE CD-PATH-BEG             TO CD-SELECT-PATH-BEG
              MOVE CD-PATH-FILE            TO CD-SELECT-PATH-FILE
           ELSE
              MOVE CD-PATH-SQL             TO CD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CD1-FILE.
           PERFORM LOAD-CD1-FILE.
           PERFORM OPEN-IT.
           MOVE CD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CD1-FILE.
           PERFORM START-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.

           IF ( CD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CD1-FILE.

           IF ( QCD1-WEND-TYPE = SPACES    ) OR
              ( QCD1-WEND-CODE = SPACES    )
              MOVE CD-TYPE              TO QCD1-WBEG-TYPE
              MOVE ALL "Z"              TO QCD1-WEND-TYPE
              MOVE CD-CODE              TO QCD1-WBEG-CODE
              MOVE ALL "Z"              TO QCD1-WEND-CODE.

           EXEC SQL
            DECLARE CD1_CURSOR CURSOR FOR
            SELECT ISNULL(CDFILE.CD_TYPE,''),
                   ISNULL(CDFILE.CD_CODE,''),
                   ISNULL(CAST(CDFILE.CD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDFILE.CD_DESC,''),
                   ISNULL(CDFILE.CD_AT_ACTIONCD,''),
                   ISNULL(CDFILE.CD_AT_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_AT_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_AT_RESULT_REQUIRED,''),
                   ISNULL(CDFILE.CD_BR_FILE_TYPE,''),
                   ISNULL(CDFILE.CD_BR_FILE_NAME,''),
                   ISNULL(CDFILE.CD_BR_PAYOFF_FG,''),
                   ISNULL(CDFILE.CD_BR_POST_SS_TRCD,''),
                   CDFILE.CD_BR_GL_BRANCH,
                   CDFILE.CD_BR_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_ALLOT_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LBOX_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LC_FG,''),
                   CDFILE.CD_BR_OT_GL_BRANCH,
                   CDFILE.CD_BR_OT_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_MULTIPLE_REFCDS,''),
                   ISNULL(CDFILE.CD_BR_COLLECT_LC,''),
                   ISNULL(CDFILE.CD_CO_GROUP,''),
                   ISNULL(CDFILE.CD_CO_SECURED,''),
                   ISNULL(CDFILE.CD_CO_TRW,''),
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_1,''),
                   CDFILE.CD_CT_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_2,''),
                   CDFILE.CD_CT_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_3,''),
                   CDFILE.CD_CT_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_4,''),
                   CDFILE.CD_CT_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_5,''),
                   CDFILE.CD_CT_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_6,''),
                   CDFILE.CD_CT_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_7,''),
                   CDFILE.CD_CT_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_8,''),
                   CDFILE.CD_CT_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_9,''),
                   CDFILE.CD_CT_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_10,''),
                   CDFILE.CD_CT_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_11,''),
                   CDFILE.CD_CT_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_12,''),
                   CDFILE.CD_CT_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CT_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_1,''),
                   CDFILE.CD_CY_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_2,''),
                   CDFILE.CD_CY_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_3,''),
                   CDFILE.CD_CY_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_4,''),
                   CDFILE.CD_CY_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_5,''),
                   CDFILE.CD_CY_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_6,''),
                   CDFILE.CD_CY_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_7,''),
                   CDFILE.CD_CY_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_8,''),
                   CDFILE.CD_CY_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_9,''),
                   CDFILE.CD_CY_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_10,''),
                   CDFILE.CD_CY_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_11,''),
                   CDFILE.CD_CY_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_12,''),
                   CDFILE.CD_CY_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CY_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_EA_CQ_FG,''),
                   ISNULL(CDFILE.CD_EA_GQ_FG,''),
                   CDFILE.CD_ELGRACE,
                   ISNULL(CDFILE.CD_ELGRACECD,''),
                   ISNULL(CDFILE.CD_ELTYPE,''),
                   CDFILE.CD_ELRATE,
                   ISNULL(CDFILE.CD_ELFRMLA,''),
                   CDFILE.CD_ELMINCHG,
                   CDFILE.CD_ELMAXAMT,
                   CDFILE.CD_ELDELFAC,
                   CDFILE.CD_ELYRTYPE,
                   ISNULL(CDFILE.CD_ELUNITPER_CD,''),
                   CDFILE.CD_PO_TITLE_HOLD_DAYS,
                   ISNULL(CDFILE.CD_RE_ACTIONCD,''),
                   ISNULL(CDFILE.CD_RE_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_RE_ACTIVITY_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_RESCHEDULE_REQ,''),
                   CDFILE.CD_RE_RESCHEDULE_TIME,
                   ISNULL(CDFILE.CD_RE_WORKED_ACCOUNT,''),
                   CDFILE.CD_FC_FEEAMT,
                   ISNULL(CDFILE.CD_TC_REFNO_1,''),
                   ISNULL(CDFILE.CD_TC_REFNO_2,''),
                   ISNULL(CDFILE.CD_TC_REFNO_3,''),
                   ISNULL(CDFILE.CD_TC_REFNO_4,''),
                   ISNULL(CDFILE.CD_TC_REFNO_5,''),
                   ISNULL(CDFILE.CD_TC_REFNO_6,''),
                   ISNULL(CDFILE.CD_TC_REFNO_7,''),
                   ISNULL(CDFILE.CD_TC_REFNO_8,''),
                   ISNULL(CDFILE.CD_TC_REFNO_9,''),
                   ISNULL(CDFILE.CD_TC_REFNO_10,''),
                   ISNULL(CDFILE.CD_PS_TDR_FLAG,'')
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE >= :QCD1-WBEG-TYPE
              AND CDFILE.CD_TYPE <= :QCD1-WEND-TYPE
              AND CDFILE.CD_CODE >= :QCD1-WBEG-CODE
              AND CDFILE.CD_CODE <= :QCD1-WEND-CODE
              ORDER BY CDFILE.CD_TYPE,
                       CDFILE.CD_CODE
           END-EXEC.

           EXEC SQL
               OPEN CD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCD1-WBEG-TYPE
                          QCD1-WEND-TYPE
                          QCD1-WBEG-CODE
                          QCD1-WEND-CODE.

           MOVE STAT---GOOD TO CD1-CURSOR-STAT.

      *------------------------------------------------
       START-CD1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-CD1-FILE.

           IF ( QCD1-WEND-TYPE = SPACES    ) OR
              ( QCD1-WEND-CODE = SPACES    )
              MOVE SPACES               TO QCD1-WBEG-TYPE
              MOVE CD-TYPE              TO QCD1-WEND-TYPE
              MOVE SPACES               TO QCD1-WBEG-CODE
              MOVE CD-CODE              TO QCD1-WEND-CODE.

           EXEC SQL
            DECLARE CD1_CURSOR_NG CURSOR FOR
            SELECT ISNULL(CDFILE.CD_TYPE,''),
                   ISNULL(CDFILE.CD_CODE,''),
                   ISNULL(CAST(CDFILE.CD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDFILE.CD_DESC,''),
                   ISNULL(CDFILE.CD_AT_ACTIONCD,''),
                   ISNULL(CDFILE.CD_AT_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_AT_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_AT_RESULT_REQUIRED,''),
                   ISNULL(CDFILE.CD_BR_FILE_TYPE,''),
                   ISNULL(CDFILE.CD_BR_FILE_NAME,''),
                   ISNULL(CDFILE.CD_BR_PAYOFF_FG,''),
                   ISNULL(CDFILE.CD_BR_POST_SS_TRCD,''),
                   CDFILE.CD_BR_GL_BRANCH,
                   CDFILE.CD_BR_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_ALLOT_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LBOX_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LC_FG,''),
                   CDFILE.CD_BR_OT_GL_BRANCH,
                   CDFILE.CD_BR_OT_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_MULTIPLE_REFCDS,''),
                   ISNULL(CDFILE.CD_BR_COLLECT_LC,''),
                   ISNULL(CDFILE.CD_CO_GROUP,''),
                   ISNULL(CDFILE.CD_CO_SECURED,''),
                   ISNULL(CDFILE.CD_CO_TRW,''),
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_1,''),
                   CDFILE.CD_CT_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_2,''),
                   CDFILE.CD_CT_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_3,''),
                   CDFILE.CD_CT_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_4,''),
                   CDFILE.CD_CT_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_5,''),
                   CDFILE.CD_CT_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_6,''),
                   CDFILE.CD_CT_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_7,''),
                   CDFILE.CD_CT_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_8,''),
                   CDFILE.CD_CT_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_9,''),
                   CDFILE.CD_CT_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_10,''),
                   CDFILE.CD_CT_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_11,''),
                   CDFILE.CD_CT_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_12,''),
                   CDFILE.CD_CT_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CT_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_1,''),
                   CDFILE.CD_CY_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_2,''),
                   CDFILE.CD_CY_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_3,''),
                   CDFILE.CD_CY_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_4,''),
                   CDFILE.CD_CY_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_5,''),
                   CDFILE.CD_CY_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_6,''),
                   CDFILE.CD_CY_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_7,''),
                   CDFILE.CD_CY_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_8,''),
                   CDFILE.CD_CY_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_9,''),
                   CDFILE.CD_CY_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_10,''),
                   CDFILE.CD_CY_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_11,''),
                   CDFILE.CD_CY_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_12,''),
                   CDFILE.CD_CY_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CY_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_EA_CQ_FG,''),
                   ISNULL(CDFILE.CD_EA_GQ_FG,''),
                   CDFILE.CD_ELGRACE,
                   ISNULL(CDFILE.CD_ELGRACECD,''),
                   ISNULL(CDFILE.CD_ELTYPE,''),
                   CDFILE.CD_ELRATE,
                   ISNULL(CDFILE.CD_ELFRMLA,''),
                   CDFILE.CD_ELMINCHG,
                   CDFILE.CD_ELMAXAMT,
                   CDFILE.CD_ELDELFAC,
                   CDFILE.CD_ELYRTYPE,
                   ISNULL(CDFILE.CD_ELUNITPER_CD,''),
                   CDFILE.CD_PO_TITLE_HOLD_DAYS,
                   ISNULL(CDFILE.CD_RE_ACTIONCD,''),
                   ISNULL(CDFILE.CD_RE_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_RE_ACTIVITY_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_RESCHEDULE_REQ,''),
                   CDFILE.CD_RE_RESCHEDULE_TIME,
                   ISNULL(CDFILE.CD_RE_WORKED_ACCOUNT,''),
                   CDFILE.CD_FC_FEEAMT,
                   ISNULL(CDFILE.CD_TC_REFNO_1,''),
                   ISNULL(CDFILE.CD_TC_REFNO_2,''),
                   ISNULL(CDFILE.CD_TC_REFNO_3,''),
                   ISNULL(CDFILE.CD_TC_REFNO_4,''),
                   ISNULL(CDFILE.CD_TC_REFNO_5,''),
                   ISNULL(CDFILE.CD_TC_REFNO_6,''),
                   ISNULL(CDFILE.CD_TC_REFNO_7,''),
                   ISNULL(CDFILE.CD_TC_REFNO_8,''),
                   ISNULL(CDFILE.CD_TC_REFNO_9,''),
                   ISNULL(CDFILE.CD_TC_REFNO_10,''),
                   ISNULL(CDFILE.CD_PS_TDR_FLAG,'')
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE >= :QCD1-WBEG-TYPE
              AND CDFILE.CD_TYPE <= :QCD1-WEND-TYPE
              AND CDFILE.CD_CODE >= :QCD1-WBEG-CODE
              AND CDFILE.CD_CODE <= :QCD1-WEND-CODE
            ORDER BY CDFILE.CD_TYPE DESC,
                     CDFILE.CD_CODE DESC
           END-EXEC.

           EXEC SQL
               OPEN CD1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCD1-WBEG-TYPE
                          QCD1-WEND-TYPE
                          QCD1-WBEG-CODE
                          QCD1-WEND-CODE.

           MOVE STAT---GOOD TO CD1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-CD1-FILE.
           PERFORM READ-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.

           MOVE CD-TYPE    TO QCD-TYPE.
           MOVE CD-CODE    TO QCD-CODE.

           EXEC SQL
            SELECT CDFILE.CD_TYPE,
                   CDFILE.CD_CODE,
                   CDFILE.CD_DESC 
              INTO
				   :QCD-TYPE,
				   :QCD-CODE,
				   :QCD-DESC 	   
              FROM DBO.CDFILE
             WHERE CDFILE.CD_TYPE = :QCD-TYPE
               AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       READ-CD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CD1_CURSOR INTO :QCD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       READ-CD1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH CD1_CURSOR_NG INTO :QCD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.
           PERFORM CLOSE-IT.
           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-STAT.

           IF ( CD1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-NG-STAT.	  
	  
	  
      * COPY "LIBLP/LPLC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLC1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LCFILE NOT = "S" )
              MOVE LC-PATH-BEG             TO LC-SELECT-PATH-BEG
              MOVE LC-PATH-FILE            TO LC-SELECT-PATH-FILE
           ELSE
              MOVE LC-PATH-SQL             TO LC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LC1-FILE.
           PERFORM START-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >= :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR_GR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                   AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >  :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LC1-FILE.
           PERFORM READ-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   LCFILE.LC_DESC,
                   LCFILE.LC_SERVACCT 
              INTO 
                   :QLC-CODE,
                   :QLC-DESC,
                   :QLC-SERVACCT
              FROM DBO.LCFILE LCFILE
             WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
      *    STILL LOADING FILE WORKERS LIKE E-WORKERS
      *-----------------------------------------------------------------
       READ-LC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR INTO :QLC-REC
              END-EXEC
           ELSE
           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR_GR INTO :QLC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           PERFORM CLOSE-IT.
           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-STAT.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-GR-STAT.	  
	  
	  
      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN1_CONNECT
      *             USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
      *    END-EXEC.


      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *    EXEC SQL SET CONNECTION C1 END-EXEC.
 
      *-----------------------------------------------------------------
       START-LN1-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

      *    EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

      *    EXEC SQL
      *     DECLARE LN1_CURSOR CURSOR FOR
      *         :RET-CODE = EXEC SP_RZ1DAY_LNFILE_SELECT
      *       ( :QLN1-WBEG-OWNBR,  :QLN1-WEND-OWNBR, 
      *         :QLN1-WBEG-ACCTNO, :QLN1-WEND-ACCTNO )
      *    END-EXEC.

           EXEC SQL   
		    DECLARE LN1_CURSOR CURSOR FOR		   
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1,
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2,
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4, 
                   LNFILE.LN_ORGST, 
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_CONVERCD, 
                   LNFILE.LN_POCD, 
                   LNFILE.LN_PLCD, 
                   LNFILE.LN_STATUSFG, 
                   LNFILE.LN_SALEFIN, 
                   LNFILE.LN_SERVACCT, 
                   LNFILE.LN_LOANTYPE, 
                   LNFILE.LN_ADDON_FG, 
                   LNFILE.LN_PAY_SCHLD_FG, 
                   LNFILE.LN_STATFLAGS, 
                   CAST(LNFILE.LN_ENTDATE  AS VARCHAR(10)),
                   CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_PURDATE  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INTDATE  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_ORIG_1STPYDATE		    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_1STPYDATE				    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_JDDATE   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_PLDATE   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_ACCELDATE				    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_LAST_ACCRUAL_DATE		    
                                           AS VARCHAR(10)), 
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_FEECODE_1, 
                   LNFILE.LN_FEECODE_2, 
                   LNFILE.LN_FEECODE_3, 
                   LNFILE.LN_FEECODE_4, 
                   LNFILE.LN_FEECODE_5, 
                   LNFILE.LN_FEECODE_6, 
                   LNFILE.LN_FEECODE_7, 
                   LNFILE.LN_FEECODE_8, 
                   LNFILE.LN_FEECODE_9, 
                   LNFILE.LN_FEECODE_10, 
                   LNFILE.LN_FEECODE_11, 
                   LNFILE.LN_FEECODE_12, 
                   LNFILE.LN_FEECODE_13, 
                   LNFILE.LN_FEECODE_14, 
                   LNFILE.LN_FEECODE_15, 
                   LNFILE.LN_FEECODE_16, 
                   LNFILE.LN_FEECODE_17, 
                   LNFILE.LN_FEECODE_18, 
                   LNFILE.LN_FEECODE_19, 
                   LNFILE.LN_FEECODE_20, 
                   LNFILE.LN_FEEAMT_1,
                   LNFILE.LN_FEEAMT_2,
                   LNFILE.LN_FEEAMT_3,
                   LNFILE.LN_FEEAMT_4,
                   LNFILE.LN_FEEAMT_5,
                   LNFILE.LN_FEEAMT_6,
                   LNFILE.LN_FEEAMT_7,
                   LNFILE.LN_FEEAMT_8,
                   LNFILE.LN_FEEAMT_9,
                   LNFILE.LN_FEEAMT_10,
                   LNFILE.LN_FEEAMT_11,
                   LNFILE.LN_FEEAMT_12,
                   LNFILE.LN_FEEAMT_13,
                   LNFILE.LN_FEEAMT_14,
                   LNFILE.LN_FEEAMT_15,
                   LNFILE.LN_FEEAMT_16,
                   LNFILE.LN_FEEAMT_17,
                   LNFILE.LN_FEEAMT_18,
                   LNFILE.LN_FEEAMT_19,
                   LNFILE.LN_FEEAMT_20,
                   LNFILE.LN_FEEOURS_1, 
                   LNFILE.LN_FEEOURS_2, 
                   LNFILE.LN_FEEOURS_3, 
                   LNFILE.LN_FEEOURS_4, 
                   LNFILE.LN_FEEOURS_5, 
                   LNFILE.LN_FEEOURS_6, 
                   LNFILE.LN_FEEOURS_7, 
                   LNFILE.LN_FEEOURS_8, 
                   LNFILE.LN_FEEOURS_9, 
                   LNFILE.LN_FEEOURS_10, 
                   LNFILE.LN_FEEOURS_11, 
                   LNFILE.LN_FEEOURS_12, 
                   LNFILE.LN_FEEOURS_13, 
                   LNFILE.LN_FEEOURS_14, 
                   LNFILE.LN_FEEOURS_15, 
                   LNFILE.LN_FEEOURS_16, 
                   LNFILE.LN_FEEOURS_17, 
                   LNFILE.LN_FEEOURS_18, 
                   LNFILE.LN_FEEOURS_19, 
                   LNFILE.LN_FEEOURS_20, 
                   LNFILE.LN_FEEFINCHG_1, 
                   LNFILE.LN_FEEFINCHG_2, 
                   LNFILE.LN_FEEFINCHG_3, 
                   LNFILE.LN_FEEFINCHG_4, 
                   LNFILE.LN_FEEFINCHG_5, 
                   LNFILE.LN_FEEFINCHG_6, 
                   LNFILE.LN_FEEFINCHG_7, 
                   LNFILE.LN_FEEFINCHG_8, 
                   LNFILE.LN_FEEFINCHG_9, 
                   LNFILE.LN_FEEFINCHG_10, 
                   LNFILE.LN_FEEFINCHG_11, 
                   LNFILE.LN_FEEFINCHG_12, 
                   LNFILE.LN_FEEFINCHG_13, 
                   LNFILE.LN_FEEFINCHG_14, 
                   LNFILE.LN_FEEFINCHG_15, 
                   LNFILE.LN_FEEFINCHG_16, 
                   LNFILE.LN_FEEFINCHG_17, 
                   LNFILE.LN_FEEFINCHG_18, 
                   LNFILE.LN_FEEFINCHG_19, 
                   LNFILE.LN_FEEFINCHG_20, 
                   LNFILE.LN_INSURED, 
                   LNFILE.LN_INSTYPES, 
                   LNFILE.LN_INSCOMP_1,
                   LNFILE.LN_INSCOMP_2,
                   LNFILE.LN_INSCOMP_3,
                   LNFILE.LN_INSCOMP_4,
                   LNFILE.LN_INSCOMP_5,
                   LNFILE.LN_INSCOMP_6,
                   LNFILE.LN_INSCOMP_7,
                   LNFILE.LN_INSCOMP_8,
                   LNFILE.LN_INSCOMP_9,
                   LNFILE.LN_INSCOMP_10,
                   LNFILE.LN_INSOURS_1, 
                   LNFILE.LN_INSOURS_2, 
                   LNFILE.LN_INSOURS_3, 
                   LNFILE.LN_INSOURS_4, 
                   LNFILE.LN_INSOURS_5, 
                   LNFILE.LN_INSOURS_6, 
                   LNFILE.LN_INSOURS_7, 
                   LNFILE.LN_INSOURS_8, 
                   LNFILE.LN_INSOURS_9, 
                   LNFILE.LN_INSOURS_10, 
                   CAST(LNFILE.LN_INSEFF_DATE_1
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_2			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_3			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_4			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_5			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_6			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_7			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_8			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_9			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_10
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_1
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_2			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_3			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_4			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_5			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_6			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_7			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_8			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_9			    
                                           AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_10		    
                                           AS VARCHAR(10)), 
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSFINCHG_1, 
                   LNFILE.LN_INSFINCHG_2, 
                   LNFILE.LN_INSFINCHG_3, 
                   LNFILE.LN_INSFINCHG_4, 
                   LNFILE.LN_INSFINCHG_5, 
                   LNFILE.LN_INSFINCHG_6, 
                   LNFILE.LN_INSFINCHG_7, 
                   LNFILE.LN_INSFINCHG_8, 
                   LNFILE.LN_INSFINCHG_9, 
                   LNFILE.LN_INSFINCHG_10, 
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   LNFILE.LN_DLPOOL,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLDISC,
                   CAST(LNFILE.LN_INTPDTH_DATE
                                         AS VARCHAR(10)), 
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_PRIOR_UNEARNED,
                   LNFILE.LN_ERNDELQ_STOP, 
                   LNFILE.LN_NONACRERN,
                   CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                        AS VARCHAR(10)), 
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_CL_REBATE, 
                   LNFILE.LN_AH_REBATE, 
                   LNFILE.LN_PP_REBATE, 
                   LNFILE.LN_INT_REBATE, 
                   LNFILE.LN_SC_REBATE, 
                   LNFILE.LN_MF_REBATE, 
                   LNFILE.LN_DF_REBATE, 
                   LNFILE.LN_EX_REBATE, 
                   LNFILE.LN_O1_REBATE, 
                   LNFILE.LN_O2_REBATE, 
                   LNFILE.LN_O3_REBATE, 
                   LNFILE.LN_O4_REBATE, 
                   LNFILE.LN_O5_REBATE, 
                   LNFILE.LN_O6_REBATE, 
                   LNFILE.LN_O7_REBATE, 
                   LNFILE.LN_UN_O7_REB, 
                   LNFILE.LN_UN_O8_REB, 
                   LNFILE.LN_UN_O9_REB, 
                   LNFILE.LN_UN_O10_REB, 
                   LNFILE.LN_ACTIONCD2, 
                   CAST(LNFILE.LN_DATE_PAID_LAST
                                           AS VARCHAR(10)), 
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_UNITPER_CD,
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_REPOCD,
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,' '),
                   ISNULL(BWFILE.BW_FNAME,' '),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,' '),
                   ISNULL(LCFILE.LC_SERVACCT,'')
              FROM DBO.LNFILE LNFILE
                LEFT JOIN DBO.BWFILE BWFILE 
                  ON ( LNFILE.LN_OWNBR     = BWFILE.BW_OWNBR	
                 AND   LNFILE.LN_SSNO1     = BWFILE.BW_SSNO ) 
                LEFT JOIN DBO.LCFILE LCFILE 
                  ON ( LNFILE.LN_CLASS     = LCFILE.LC_CODE )
             WHERE ( LNFILE.LN_OWNBR      >= :QLN1-WBEG-OWNBR
               AND   LNFILE.LN_OWNBR      <= :QLN1-WEND-OWNBR
               AND   LNFILE.LN_ACCTNO     >= :QLN1-WBEG-ACCTNO
               AND   LNFILE.LN_ACCTNO     <= :QLN1-WEND-ACCTNO )
               AND ( LNFILE.LN_POCD   NOT IN ('*I', '*R') )
             ORDER BY LNFILE.LN_OWNBR,
                      LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.
		   
      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN1_CURSOR_GR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR = :QLN1-WBEG-OWNBR
              AND LNFILE.LN_ACCTNO > :QLN1-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN1-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LN1-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            INTO :QLN-REC
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.
		   
      *    EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.


           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR INTO 
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
                     :QLN-SPRCLASS,
                     :QLN-CONVERCD, 
                     :QLN-POCD,
                     :QLN-PLCD,
                     :QLN-STATUSFG,
                     :QLN-SALEFIN, 
                     :QLN-SERVACCT, 
                     :QLN-LOANTYPE,
                     :QLN-ADDON-FG,
                     :QLN-PAY-SCHLD-FG,
                     :QLN-STATFLAGS,
                     :QLN-ENTDATE,
                     :QLN-LOANDATE,
                     :QLN-PURDATE,
                     :QLN-INTDATE,
                     :QLN-CASHDATE,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-JDDATE,
                     :QLN-PLDATE,
                     :QLN-ACCELDATE,
                     :QLN-REDUDATE,
                     :QLN-POFFDATE,
                     :QLN-LAST-ACCRUAL-DATE,
                     :QLN-APRATE,
                     :QLN-EFFRATE,
                     :QLN-SMPRATE,
                     :QLN-OVRDRATE,
                     :QLN-ENDORATE,
                     :QLN-JDRATE,
                     :QLN-ACCRATE,
                     :QLN-REDURATE,
                     :QLN-DOWNPY,
                     :QLN-LNAMT,
                     :QLN-FINCHG,
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-POINTS,
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-FEECODE-1, 
                     :QLN-FEECODE-2, 
                     :QLN-FEECODE-3, 
                     :QLN-FEECODE-4, 
                     :QLN-FEECODE-5, 
                     :QLN-FEECODE-6, 
                     :QLN-FEECODE-7, 
                     :QLN-FEECODE-8, 
                     :QLN-FEECODE-9, 
                     :QLN-FEECODE-10, 
                     :QLN-FEECODE-11, 
                     :QLN-FEECODE-12, 
                     :QLN-FEECODE-13, 
                     :QLN-FEECODE-14, 
                     :QLN-FEECODE-15, 
                     :QLN-FEECODE-16, 
                     :QLN-FEECODE-17, 
                     :QLN-FEECODE-18, 
                     :QLN-FEECODE-19, 
                     :QLN-FEECODE-20, 
                     :QLN-FEEAMT-1,
                     :QLN-FEEAMT-2,
                     :QLN-FEEAMT-3,
                     :QLN-FEEAMT-4,
                     :QLN-FEEAMT-5,
                     :QLN-FEEAMT-6,
                     :QLN-FEEAMT-7,
                     :QLN-FEEAMT-8,
                     :QLN-FEEAMT-9,
                     :QLN-FEEAMT-10,
                     :QLN-FEEAMT-11,
                     :QLN-FEEAMT-12,
                     :QLN-FEEAMT-13,
                     :QLN-FEEAMT-14,
                     :QLN-FEEAMT-15,
                     :QLN-FEEAMT-16,
                     :QLN-FEEAMT-17,
                     :QLN-FEEAMT-18,
                     :QLN-FEEAMT-19,
                     :QLN-FEEAMT-20,
                     :QLN-FEEOURS-1, 
                     :QLN-FEEOURS-2, 
                     :QLN-FEEOURS-3, 
                     :QLN-FEEOURS-4, 
                     :QLN-FEEOURS-5, 
                     :QLN-FEEOURS-6, 
                     :QLN-FEEOURS-7, 
                     :QLN-FEEOURS-8, 
                     :QLN-FEEOURS-9, 
                     :QLN-FEEOURS-10, 
                     :QLN-FEEOURS-11, 
                     :QLN-FEEOURS-12, 
                     :QLN-FEEOURS-13, 
                     :QLN-FEEOURS-14, 
                     :QLN-FEEOURS-15, 
                     :QLN-FEEOURS-16, 
                     :QLN-FEEOURS-17, 
                     :QLN-FEEOURS-18, 
                     :QLN-FEEOURS-19, 
                     :QLN-FEEOURS-20, 
                     :QLN-FEEFINCHG-1, 
                     :QLN-FEEFINCHG-2, 
                     :QLN-FEEFINCHG-3, 
                     :QLN-FEEFINCHG-4, 
                     :QLN-FEEFINCHG-5, 
                     :QLN-FEEFINCHG-6, 
                     :QLN-FEEFINCHG-7, 
                     :QLN-FEEFINCHG-8, 
                     :QLN-FEEFINCHG-9, 
                     :QLN-FEEFINCHG-10, 
                     :QLN-FEEFINCHG-11, 
                     :QLN-FEEFINCHG-12, 
                     :QLN-FEEFINCHG-13, 
                     :QLN-FEEFINCHG-14, 
                     :QLN-FEEFINCHG-15, 
                     :QLN-FEEFINCHG-16, 
                     :QLN-FEEFINCHG-17, 
                     :QLN-FEEFINCHG-18, 
                     :QLN-FEEFINCHG-19, 
                     :QLN-FEEFINCHG-20, 
                     :QLN-INSURED,
                     :QLN-INSTYPES,
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSOURS-1, 
                     :QLN-INSOURS-2, 
                     :QLN-INSOURS-3, 
                     :QLN-INSOURS-4, 
                     :QLN-INSOURS-5, 
                     :QLN-INSOURS-6, 
                     :QLN-INSOURS-7, 
                     :QLN-INSOURS-8, 
                     :QLN-INSOURS-9, 
                     :QLN-INSOURS-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSPREM-1,
                     :QLN-INSPREM-2,
                     :QLN-INSPREM-3,
                     :QLN-INSPREM-4,
                     :QLN-INSPREM-5,
                     :QLN-INSPREM-6,
                     :QLN-INSPREM-7,
                     :QLN-INSPREM-8,
                     :QLN-INSPREM-9,
                     :QLN-INSPREM-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-INSFINCHG-1,
                     :QLN-INSFINCHG-2,
                     :QLN-INSFINCHG-3,
                     :QLN-INSFINCHG-4,
                     :QLN-INSFINCHG-5,
                     :QLN-INSFINCHG-6,
                     :QLN-INSFINCHG-7,
                     :QLN-INSFINCHG-8,
                     :QLN-INSFINCHG-9,
                     :QLN-INSFINCHG-10, 
                     :QLN-PY-YYYYMM-RECDEL,
                     :QLN-MTD-PAYS-RECDEL,
                     :QLN-DLPOOL,
                     :QLN-ORIGPURAMT,
                     :QLN-PURBAL,
                     :QLN-DLDISC,
                     :QLN-INTPDTH-DATE,
                     :QLN-CURBAL,
                     :QLN-INTBAL,
                     :QLN-OT2BAL,
                     :QLN-ANTICTERM,
                     :QLN-ANTICERN-1,
                     :QLN-ANTICERN-2,
                     :QLN-ANTICERN-3,
                     :QLN-ANTICERN-4,
                     :QLN-ANTICERN-5,
                     :QLN-ANTICERN-6,
                     :QLN-ANTICERN-7,
                     :QLN-ANTICERN-8,
                     :QLN-ANTICERN-9,
                     :QLN-ANTICERN-10,
                     :QLN-ANTICADJ-1,
                     :QLN-ANTICADJ-2,
                     :QLN-ANTICADJ-3,
                     :QLN-ANTICADJ-4,
                     :QLN-ANTICADJ-5,
                     :QLN-ANTICADJ-6,
                     :QLN-ANTICADJ-7,
                     :QLN-ANTICADJ-8,
                     :QLN-ANTICADJ-9,
                     :QLN-ANTICADJ-10,
                     :QLN-UFERN-1,
                     :QLN-UFERN-2,
                     :QLN-UFERN-3,
                     :QLN-UFERN-4,
                     :QLN-UFERN-5,
                     :QLN-UFERN-6,
                     :QLN-UFERN-7,
                     :QLN-UFERN-8,
                     :QLN-UFERN-9,
                     :QLN-UFERN-10,
                     :QLN-ACCERN-1,
                     :QLN-ACCERN-2,
                     :QLN-ACCERN-3,
                     :QLN-ACCERN-4,
                     :QLN-ACCERN-5,
                     :QLN-ACCERN-6,
                     :QLN-ACCERN-7,
                     :QLN-ACCERN-8,
                     :QLN-ACCERN-9,
                     :QLN-ACCERN-10,
                     :QLN-PRIOR-UNEARNED,
                     :QLN-ERNDELQ-STOP,
                     :QLN-NONACRERN,
                     :QLN-ERN-IB-PDTH-DATE,
                     :QLN-ERN-IB-PRINBAL,
                     :QLN-ERN-IB-INTOWE,
                     :QLN-TOTINT,
                     :QLN-TOTLCHG,
                     :QLN-TOTDEF,
                     :QLN-TOTPAYMNTD,
                     :QLN-TOTEXCPAYMNTD,
                     :QLN-TOTNODEF,
                     :QLN-TOTNOALDEL,
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
                     :QLN-ACTIONCD2, 
                     :QLN-DATE-PAID-LAST,
                     :QLN-CREDITLIM,
                     :QLN-OLDACCTNO,
                     :QLN-PRLNNO,
                     :QLN-SUM-LNAMT,
                     :QLN-SUM-FINCHG,
                     :QLN-SUM-INTCHG,
                     :QLN-SUM-ORGTERM,
                     :QLN-SUM-EXTCHG,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ,
                     :QLN-REPOCD,
                     :QLN-CONPROFILE-1,
                     :QLN-CONPROFILE-2,
                     :QLN-CONPROFILE-3,
                     :QLN-CONPROFILE-4,
                     :QLN-CONPROFILE-5,
                     :QBW-OWNBR,		
                     :QBW-SSNO,					 
                     :QBW-LNAME,					 
                     :QBW-FNAME,					 
                     :QBW-SOLICIT,					 
                     :QLC-CODE,					 
                     :QLC-DESC,					 
                     :QLC-SERVACCT					 
              END-EXEC
           ELSE
           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR_GR INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
			  PERFORM GET-BW-FIELDS
			  PERFORM GET-LC-FIELDS.
			
      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

      *    EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-GR-STAT.	  

      *    EXEC SQL SET CONNECTION C1 END-EXEC.


      * COPY "LIBLP/LPLTI1RN.CPY".
      * COPYMEMBER: LIBLP/LPLTI1RN
       LOAD-LTI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTI-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTI-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTIFILE NOT = "S" )
              MOVE LTI-PATH-BEG             TO LTI-SELECT-PATH-BEG
              MOVE LTI-PATH-FILE            TO LTI-SELECT-PATH-FILE
           ELSE
              MOVE LTI-PATH-SQL             TO LTI-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTI1-FILE.
           PERFORM LOAD-LTI1-FILE.
           PERFORM OPEN-IT.
           MOVE LTI-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTI1-FILE.
           PERFORM START-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.

           IF ( LTI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTI1-FILE.

           IF ( QLTI1-WEND-BRNO = ZEROES    ) OR
              ( QLTI1-WEND-BRNO NOT NUMERIC )
              MOVE LTI-PATH-OWNBR        TO QLTI1-WBEG-BRNO
              MOVE LTI-PATH-OWNBR        TO QLTI1-WEND-BRNO
              MOVE LTI-ACCTNO            TO QLTI1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTI1-WEND-ACCTNO
              MOVE LTI-SEQNO             TO QLTI1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTI1-WEND-SEQNO.


           EXEC SQL
            DECLARE LTI1_CURSOR CURSOR FOR
              SELECT LTIFILE.LTI_BRNO,
                     LTIFILE.LTI_ACCTNO,
                     LTIFILE.LTI_SEQNO,
                     LTIFILE.LTI_INS_TYPE,
                     CAST(LTIFILE.LTI_ORIG_1STPYDATE
                                       AS VARCHAR(10)),
                     LTIFILE.LTI_PRIOR_NODEF,
                     LTIFILE.LTI_INSPREM,
                     LTIFILE.LTI_ANTICTERM,
                     LTIFILE.LTI_ANTICERN_1,
                     LTIFILE.LTI_ANTICERN_2,
                     LTIFILE.LTI_ANTICERN_3,
                     LTIFILE.LTI_ANTICADJ_1,
                     LTIFILE.LTI_ANTICADJ_2,
                     LTIFILE.LTI_ANTICADJ_3,
                     LTIFILE.LTI_UFERN_1,
                     LTIFILE.LTI_UFERN_2,
                     LTIFILE.LTI_UFERN_3,
                     LTIFILE.LTI_ACCERN_1,
                     LTIFILE.LTI_ACCERN_2,
                     LTIFILE.LTI_ACCERN_3,            
                     LTIFILE.LTI_INT_REBATE,
                     CAST(LTIFILE.LTI_ANTIC_START_DATE
                                       AS VARCHAR(10))      
                FROM DBO.LTIFILE
               WHERE LTIFILE.LTI_BRNO   >= :QLTI1-WBEG-BRNO
                 AND LTIFILE.LTI_BRNO   <= :QLTI1-WEND-BRNO
                 AND LTIFILE.LTI_ACCTNO >= :QLTI1-WBEG-ACCTNO
                 AND LTIFILE.LTI_ACCTNO <= :QLTI1-WEND-ACCTNO
                 AND LTIFILE.LTI_SEQNO  >= :QLTI1-WBEG-SEQNO
                 AND LTIFILE.LTI_SEQNO  <= :QLTI1-WEND-SEQNO
               ORDER BY LTIFILE.LTI_BRNO,
                       LTIFILE.LTI_ACCTNO,
                       LTIFILE.LTI_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTI1-WBEG-BRNO
                          QLTI1-WEND-BRNO
                          QLTI1-WBEG-ACCTNO
                          QLTI1-WEND-ACCTNO
                          QLTI1-WBEG-SEQNO
                          QLTI1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTI1-FILE.
           PERFORM READ-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO. 
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.

           MOVE LTI-BRNO   TO QLTI-BRNO.
           MOVE LTI-ACCTNO TO QLTI-ACCTNO.
           MOVE LTI-SEQNO  TO QLTI-SEQNO.

           EXEC SQL
            SELECT LTIFILE.LTI_BRNO,
                   LTIFILE.LTI_ACCTNO,
                   LTIFILE.LTI_SEQNO,
                   CAST(LTIFILE.LTI_LTOUCH_DATE AS VARCHAR(10)),
                   LTIFILE.LTI_INS_TYPE,
                   CAST(LTIFILE.LTI_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),
                   LTIFILE.LTI_PRIOR_NODEF,
                   LTIFILE.LTI_INSFEECD_1,
                   LTIFILE.LTI_INSFEE_1,
                   LTIFILE.LTI_INSFEECD_2,
                   LTIFILE.LTI_INSFEE_2,
                   LTIFILE.LTI_TRCD,
                   LTIFILE.LTI_INSCOMP,
                   LTIFILE.LTI_INSOURS,
                   CAST(LTIFILE.LTI_INSEFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTIFILE.LTI_INSEXP_DATE
                                                  AS VARCHAR(10)),
                   LTIFILE.LTI_INSPREM,
                   LTIFILE.LTI_INSCOVR,
                   LTIFILE.LTI_INSCOMM,
                   LTIFILE.LTI_ANTICTERM,
                   LTIFILE.LTI_ANTICERN_1,
                   LTIFILE.LTI_ANTICADJ_1,
                   LTIFILE.LTI_UFERN_1,
                   LTIFILE.LTI_ACCERN_1,
                   LTIFILE.LTI_ANTICERN_2,
                   LTIFILE.LTI_ANTICADJ_2,
                   LTIFILE.LTI_UFERN_2,
                   LTIFILE.LTI_ACCERN_2,
                   LTIFILE.LTI_ANTICERN_3,
                   LTIFILE.LTI_ANTICADJ_3,
                   LTIFILE.LTI_UFERN_3,
                   LTIFILE.LTI_ACCERN_3,
                   LTIFILE.LTI_INT_REBATE,
                   LTIFILE.LTI_INS_PAYMNT_INCR,
                   LTIFILE.LTI_INS_TOT_ADDON,
                   LTIFILE.LTI_INT_LAST_EARNED,
                   LTIFILE.LTI_INS_REBATE,
                   LTIFILE.LTI_RB_WORKER,
                   CAST(LTIFILE.LTI_ANTIC_START_DATE
                                                  AS VARCHAR(10)),
                   LTIFILE.LTI_CANCEL_REASON_CD
            INTO :QLTI-REC
            FROM DBO.LTIFILE
            WHERE LTIFILE.LTI_BRNO = :QLTI-BRNO
              AND LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO
              AND LTIFILE.LTI_SEQNO = :QLTI-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTI-FIELDS.

      *-----------------------------------------------------------------
       READ-LTI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           INITIALIZE QLTI-REC.

           IF ( LTI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTI1_CURSOR 
                     INTO 
                        :QLTI-BRNO,
                        :QLTI-ACCTNO,
                        :QLTI-SEQNO,
                        :QLTI-INS-TYPE,
                        :QLTI-ORIG-1STPYDATE,                   
                        :QLTI-PRIOR-NODEF,
                        :QLTI-INSPREM,
                        :QLTI-ANTICTERM,
                        :QLTI-ANTICERN-1,
                        :QLTI-ANTICERN-2,
                        :QLTI-ANTICERN-3,
                        :QLTI-ANTICADJ-1,
                        :QLTI-ANTICADJ-2,
                        :QLTI-ANTICADJ-3,
                        :QLTI-UFERN-1,
                        :QLTI-UFERN-2,
                        :QLTI-UFERN-3,
                        :QLTI-ACCERN-1,
                        :QLTI-ACCERN-2,
                        :QLTI-ACCERN-3,            
                        :QLTI-INT-REBATE,
                        :QLTI-ANTIC-START-DATE				              
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTI1-FILE.

           PERFORM CLOSE-IT.
           IF ( LTI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTI1-CURSOR-STAT.


       COPY "LIBLP/LPLTP1RN.CPY".



      * COPY "LIBLP/LPSP1RN.CPY".
      * COPYMEMBER: LIBLP/LPSP1RN
      ******************************************************************
       SET-ALL-TO-SP1.
           MOVE SPA1-KEY   TO SP1-KEY.
           MOVE SPA-AREA-A TO SP-AREA-A.
           MOVE SPB-AREA-B TO SP-AREA-B.
           MOVE SPC-AREA-C TO SP-AREA-C.

       SET-SP1-TO-ALL.
           MOVE SP1-KEY   TO SPA1-KEY.
           MOVE SP-AREA-A TO SPA-AREA-A.
           MOVE SP1-KEY   TO SPB1-KEY.
           MOVE SP-AREA-B TO SPB-AREA-B.
           MOVE SP1-KEY   TO SPC1-KEY.
           MOVE SP-AREA-C TO SPC-AREA-C.

       LOAD-SP1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM LOAD-SPC1-FILE.

      *-----------------------------------------------------------------
       OPEN-SP1-FILE.
           PERFORM OPEN-SPA1-FILE.
           IF ( IO-FG = 0 )
               PERFORM OPEN-SPB1-FILE
               IF ( IO-FG = 0 )
                  PERFORM OPEN-SPC1-FILE
                  IF ( IO-FG = 0 )
                     MOVE " " TO IO-MULTI-FG
                  ELSE
                     MOVE "C" TO IO-MULTI-FG
               ELSE
                  MOVE "B" TO IO-MULTI-FG
            ELSE
               MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM START-SPA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM READ-SPB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM READ-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       READ-SP1-FILE-NEXT.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE SPA1-KEY TO SPB1-KEY
              PERFORM READ-SPB1-FILE
              IF ( IO-FG = 0 )
                 MOVE SPA1-KEY TO SPC1-KEY
                 PERFORM READ-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-SP1-FILE.
           PERFORM CLOSE-SPA1-FILE.
           PERFORM CLOSE-SPB1-FILE.
           PERFORM CLOSE-SPC1-FILE.

      *================================================================E
       LOAD-SPA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPA-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPAFILE NOT = "S" )
              MOVE SPA-PATH-BEG          TO SPA-SELECT-PATH-BEG
              MOVE SPA-PATH-FILE         TO SPA-SELECT-PATH-FILE
           ELSE
              MOVE SPA-PATH-SQL          TO SPA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPA1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM OPEN-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SPA1-FILE.
           PERFORM START-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SPA1-FILE.

           IF ( QSP1-WEND-LAWCODE = 0  ) OR
              ( QSP1-WEND-LAWCODE NOT NUMERIC)
              MOVE SPA-ORGST            TO QSP1-WBEG-ORGST 
              MOVE ALL "Z"              TO QSP1-WEND-ORGST 
              MOVE SPA-SPRCLASS         TO QSP1-WBEG-SPRCLASS
              MOVE ALL "9"              TO QSP1-WEND-SPRCLASS
              MOVE SPA-SUBCLASS         TO QSP1-WBEG-SUBCLASS
              MOVE ALL "9"              TO QSP1-WEND-SUBCLASS
              MOVE SPA-LAWCODE          TO QSP1-WBEG-LAWCODE
              MOVE ALL "9"              TO QSP1-WEND-LAWCODE.

           EXEC SQL
            DECLARE SPA1_CURSOR CURSOR FOR
            SELECT SPAFILE.SPA_ORGST,
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   CAST(SPAFILE.SPA_LTOUCH_DATE
                                             AS VARCHAR(10)),
                   SPAFILE.SPA_DESC,
                   SPAFILE.SPA_NOTES,
                   CAST(SPAFILE.SPA_EFFDATE
                                             AS VARCHAR(10)),
                   CAST(SPAFILE.SPA_EXPDATE
                                             AS VARCHAR(10)),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   SPAFILE.SPA_EXPD_FRMLA, 
                   SPAFILE.SPA_EXPD_EXCEPT, 
                   SPAFILE.SPA_EXPR_FRMLA, 
                   SPAFILE.SPA_EXPR_REDU_FRMLA, 
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   SPAFILE.SPA_EXPR_INTMETH_1,
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   SPAFILE.SPA_EXPR_INTMETH_2,
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   SPAFILE.SPA_EXPR_INTMETH_3,
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   SPAFILE.SPA_EXPR_INTMETH_4,
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   SPAFILE.SPA_EXPR_INTMETH_5,
                   SPAFILE.SPA_EXPR_LCHG,
                   SPAFILE.SPA_JUDGFRMLA,
                   SPAFILE.SPA_JUDGRATE,
                   SPAFILE.SPA_JUDGYRTYPE,
                   SPAFILE.SPA_JUDGINTMETH,
                   SPAFILE.SPA_ACC_FRMLA,
                   SPAFILE.SPA_ACC_YRTYPE,
                   SPAFILE.SPA_ACC_INTMETH,
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_5,
                   SPAFILE.SPA_1098FRMLA, 
                   SPAFILE.SPA_DISFRMLA, 
                   SPAFILE.SPA_CONTRFRMLA, 
                   SPAFILE.SPA_BANKRULE, 
                   SPAFILE.SPA_CONTPDTH,
                   SPAFILE.SPA_FINCHG,
                   SPAFILE.SPA_BANKUNITPER_CD,
                   SPAFILE.SPA_TRWBANKRULE,
                   SPAFILE.SPA_TRWCONTPDTH,
                   SPAFILE.SPA_TRWBANKUNITPER_CD, 
                   SPAFILE.SPA_RECFRMLA, 
                   SPAFILE.SPA_RECPAYCD,
                   SPAFILE.SPA_RECPDTH,
                   SPAFILE.SPA_DISFRMLA_SPAY_FG,
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNPERCT_1,
                   SPAFILE.SPA_ERNPERCD_1, 
                   SPAFILE.SPA_ERNACCRL_FG_1, 
                   SPAFILE.SPA_ERNACCRL_START_CD_1, 
                   SPAFILE.SPA_ERNACCRL_DFCD_1,
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_TERM_1,
                   SPAFILE.SPA_ERN_TYPE_1,
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNPERCT_2,
                   SPAFILE.SPA_ERNPERCD_2, 
                   SPAFILE.SPA_ERNACCRL_FG_2, 
                   SPAFILE.SPA_ERNACCRL_START_CD_2, 
                   SPAFILE.SPA_ERNACCRL_DFCD_2, 
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_TERM_2,
                   SPAFILE.SPA_ERN_TYPE_2, 
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNPERCT_3,
                   SPAFILE.SPA_ERNPERCD_3, 
                   SPAFILE.SPA_ERNACCRL_FG_3, 
                   SPAFILE.SPA_ERNACCRL_START_CD_3,
                   SPAFILE.SPA_ERNACCRL_DFCD_3, 
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_TERM_3,
                   SPAFILE.SPA_ERN_TYPE_3,
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNPERCT_4,
                   SPAFILE.SPA_ERNPERCD_4,
                   SPAFILE.SPA_ERNACCRL_FG_4,
                   SPAFILE.SPA_ERNACCRL_START_CD_4,
                   SPAFILE.SPA_ERNACCRL_DFCD_4,
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_TERM_4,
                   SPAFILE.SPA_ERN_TYPE_4,
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNPERCT_5,
                   SPAFILE.SPA_ERNPERCD_5,
                   SPAFILE.SPA_ERNACCRL_FG_5,
                   SPAFILE.SPA_ERNACCRL_START_CD_5,
                   SPAFILE.SPA_ERNACCRL_DFCD_5,
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_TERM_5,
                   SPAFILE.SPA_ERN_TYPE_5,
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNPERCT_6,
                   SPAFILE.SPA_ERNPERCD_6,
                   SPAFILE.SPA_ERNACCRL_FG_6,
                   SPAFILE.SPA_ERNACCRL_START_CD_6,
                   SPAFILE.SPA_ERNACCRL_DFCD_6,
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_TERM_6,
                   SPAFILE.SPA_ERN_TYPE_6,
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNPERCT_7,
                   SPAFILE.SPA_ERNPERCD_7,
                   SPAFILE.SPA_ERNACCRL_FG_7,
                   SPAFILE.SPA_ERNACCRL_START_CD_7,
                   SPAFILE.SPA_ERNACCRL_DFCD_7,
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_TERM_7,
                   SPAFILE.SPA_ERN_TYPE_7,
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNPERCT_8,
                   SPAFILE.SPA_ERNPERCD_8,
                   SPAFILE.SPA_ERNACCRL_FG_8,
                   SPAFILE.SPA_ERNACCRL_START_CD_8,
                   SPAFILE.SPA_ERNACCRL_DFCD_8,
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_TERM_8,
                   SPAFILE.SPA_ERN_TYPE_8,
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNPERCT_9,
                   SPAFILE.SPA_ERNPERCD_9,
                   SPAFILE.SPA_ERNACCRL_FG_9,
                   SPAFILE.SPA_ERNACCRL_START_CD_9,
                   SPAFILE.SPA_ERNACCRL_DFCD_9,
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_TERM_9,
                   SPAFILE.SPA_ERN_TYPE_9,
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_10,
                   SPAFILE.SPA_ERNPERCT_10,
                   SPAFILE.SPA_ERNPERCD_10,
                   SPAFILE.SPA_ERNACCRL_FG_10,
                   SPAFILE.SPA_ERNACCRL_START_CD_10,
                   SPAFILE.SPA_ERNACCRL_DFCD_10,
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_10,
                   SPAFILE.SPA_ERN_TYPE_10,
                   SPAFILE.SPA_ERNACCEL_CD,
                   SPAFILE.SPA_ERNJD_CD,
                   SPAFILE.SPA_MIDSTRERNCD,
                   SPAFILE.SPA_EARN_ORDER_1,
                   SPAFILE.SPA_EARN_ORDER_2,
                   SPAFILE.SPA_EARN_ORDER_3,
                   SPAFILE.SPA_EARN_ORDER_4,
                   SPAFILE.SPA_EARN_ORDER_5,
                   SPAFILE.SPA_EARN_ORDER_6,
                   SPAFILE.SPA_EARN_ORDER_7,
                   SPAFILE.SPA_EARN_ORDER_8,
                   SPAFILE.SPA_EARN_ORDER_9,
                   SPAFILE.SPA_EARN_ORDER_10,
                   SPAFILE.SPA_ERNDELQFRMLA,
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   SPAFILE.SPA_1098_INC_LC,
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   SPAFILE.SPA_LCGRACE,
                   SPAFILE.SPA_LCGRACECD,
                   SPAFILE.SPA_LCTYPE,
                   SPAFILE.SPA_LCRATE,
                   SPAFILE.SPA_LCFRMLA,
                   SPAFILE.SPA_LCMINCHG,
                   SPAFILE.SPA_LCMAXAMT,
                   SPAFILE.SPA_LCDELFAC,
                   SPAFILE.SPA_LCYRTYPE,
                   SPAFILE.SPA_LC_CODE,
                   SPAFILE.SPA_LCUNITPER_CD,
                   SPAFILE.SPA_DEFFRMLA,
                   SPAFILE.SPA_DEFEXCEPT,
                   SPAFILE.SPA_DEFWORKER,
                   SPAFILE.SPA_DEFPERCD,
                   SPAFILE.SPA_DEFBASE,
                   SPAFILE.SPA_DEFPOLCD,
                   SPAFILE.SPA_DEFPOLMX_1,
                   SPAFILE.SPA_DEFPOLMX_2,
                   SPAFILE.SPA_DEFPOLMX_3,
                   SPAFILE.SPA_DEFMIN,
                   SPAFILE.SPA_DEFMAX,
                   SPAFILE.SPA_DEFUNITPER_CD,
                   SPAFILE.SPA_PRFORM_1,
                   SPAFILE.SPA_PRFLAG_1,
                   SPAFILE.SPA_PRFORM_2,
                   SPAFILE.SPA_PRFLAG_2,
                   SPAFILE.SPA_PRFORM_3,
                   SPAFILE.SPA_PRFLAG_3,
                   SPAFILE.SPA_PRFORM_4,
                   SPAFILE.SPA_PRFLAG_4,
                   SPAFILE.SPA_PRFORM_5,
                   SPAFILE.SPA_PRFLAG_5,
                   SPAFILE.SPA_PRFORM_6,
                   SPAFILE.SPA_PRFLAG_6,
                   SPAFILE.SPA_PRFORM_7,
                   SPAFILE.SPA_PRFLAG_7,
                   SPAFILE.SPA_PRFORM_8,
                   SPAFILE.SPA_PRFLAG_8,
                   SPAFILE.SPA_PRFORM_9,
                   SPAFILE.SPA_PRFLAG_9,
                   SPAFILE.SPA_PRFORM_10,
                   SPAFILE.SPA_PRFLAG_10,
                   SPAFILE.SPA_PRFORM_11,
                   SPAFILE.SPA_PRFLAG_11,
                   SPAFILE.SPA_PRFORM_12,
                   SPAFILE.SPA_PRFLAG_12,
                   SPAFILE.SPA_PRFORM_13,
                   SPAFILE.SPA_PRFLAG_13,
                   SPAFILE.SPA_PRFORM_14,
                   SPAFILE.SPA_PRFLAG_14,
                   SPAFILE.SPA_PRFORM_15,
                   SPAFILE.SPA_PRFLAG_15,
                   SPAFILE.SPA_PRFORM_16,
                   SPAFILE.SPA_PRFLAG_16,
                   SPAFILE.SPA_PRFORM_17,
                   SPAFILE.SPA_PRFLAG_17,
                   SPAFILE.SPA_PRFORM_18,
                   SPAFILE.SPA_PRFLAG_18,
                   SPAFILE.SPA_PRFORM_19,
                   SPAFILE.SPA_PRFLAG_19,
                   SPAFILE.SPA_PRFORM_20,
                   SPAFILE.SPA_PRFLAG_20,
                   SPAFILE.SPA_IMED_REC,
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_SCRATE,
                   SPAFILE.SPA_SCMIN,
                   SPAFILE.SPA_SCMAX,
                   SPAFILE.SPA_MFFRMLA,
                   SPAFILE.SPA_MFRATE,
                   SPAFILE.SPA_MFMIN,
                   SPAFILE.SPA_MFMAX,
                   SPAFILE.SPA_INTMETHOD,
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   SPAFILE.SPA_PYEXFMDATE,
                   SPAFILE.SPA_PYEXBASECD,
                   SPAFILE.SPA_PYEXBASE,
                   SPAFILE.SPA_PYEXDAYS,
                   SPAFILE.SPA_CAL_SPFRMLA,
                   SPAFILE.SPA_CAL_RATETYPE_1,
                   SPAFILE.SPA_CAL_BREAKTYPE_1,
                   SPAFILE.SPA_CAL_CHGABLE_1,
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_TBFRMLA_1,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   SPAFILE.SPA_CAL_RATETYPE_2,
                   SPAFILE.SPA_CAL_BREAKTYPE_2,
                   SPAFILE.SPA_CAL_CHGABLE_2,
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_TBFRMLA_2,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_2_5,
                   SPAFILE.SPA_MINSF_FRMLA,
                   SPAFILE.SPA_MINSF_CHG,
                   SPAFILE.SPA_MAXAPR_FRMLA,
                   SPAFILE.SPA_EDT_NOEDITS_1,
                   SPAFILE.SPA_EDT_TYPE_1_1,
                   SPAFILE.SPA_EDT_HL_1_1,
                   SPAFILE.SPA_EDT_COLL_1_1,
                   SPAFILE.SPA_EDT_CD_1_1,
                   SPAFILE.SPA_EDT_RATE_1_1,
                   SPAFILE.SPA_EDT_RATETYPE_1_1,
                   SPAFILE.SPA_EDT_AMT_1_1,
                   SPAFILE.SPA_EDT_AMTYPE_1_1,
                   SPAFILE.SPA_EDT_TERM_1_1,
                   SPAFILE.SPA_EDT_TYPE_1_2,
                   SPAFILE.SPA_EDT_HL_1_2,
                   SPAFILE.SPA_EDT_COLL_1_2,
                   SPAFILE.SPA_EDT_CD_1_2,
                   SPAFILE.SPA_EDT_RATE_1_2,
                   SPAFILE.SPA_EDT_RATETYPE_1_2,
                   SPAFILE.SPA_EDT_AMT_1_2,
                   SPAFILE.SPA_EDT_AMTYPE_1_2,
                   SPAFILE.SPA_EDT_TERM_1_2,
                   SPAFILE.SPA_EDT_TYPE_1_3,
                   SPAFILE.SPA_EDT_HL_1_3,
                   SPAFILE.SPA_EDT_COLL_1_3,
                   SPAFILE.SPA_EDT_CD_1_3,
                   SPAFILE.SPA_EDT_RATE_1_3,
                   SPAFILE.SPA_EDT_RATETYPE_1_3,
                   SPAFILE.SPA_EDT_AMT_1_3,
                   SPAFILE.SPA_EDT_AMTYPE_1_3,
                   SPAFILE.SPA_EDT_TERM_1_3,
                   SPAFILE.SPA_EDT_TYPE_1_4,
                   SPAFILE.SPA_EDT_HL_1_4,
                   SPAFILE.SPA_EDT_COLL_1_4,
                   SPAFILE.SPA_EDT_CD_1_4,
                   SPAFILE.SPA_EDT_RATE_1_4,
                   SPAFILE.SPA_EDT_RATETYPE_1_4,
                   SPAFILE.SPA_EDT_AMT_1_4,
                   SPAFILE.SPA_EDT_AMTYPE_1_4,
                   SPAFILE.SPA_EDT_TERM_1_4,
                   SPAFILE.SPA_EDT_TYPE_1_5,
                   SPAFILE.SPA_EDT_HL_1_5,
                   SPAFILE.SPA_EDT_COLL_1_5,
                   SPAFILE.SPA_EDT_CD_1_5,
                   SPAFILE.SPA_EDT_RATE_1_5,
                   SPAFILE.SPA_EDT_RATETYPE_1_5,
                   SPAFILE.SPA_EDT_AMT_1_5,
                   SPAFILE.SPA_EDT_AMTYPE_1_5,
                   SPAFILE.SPA_EDT_TERM_1_5,
                   SPAFILE.SPA_EDT_TYPE_1_6,
                   SPAFILE.SPA_EDT_HL_1_6,
                   SPAFILE.SPA_EDT_COLL_1_6,
                   SPAFILE.SPA_EDT_CD_1_6,
                   SPAFILE.SPA_EDT_RATE_1_6,
                   SPAFILE.SPA_EDT_RATETYPE_1_6,
                   SPAFILE.SPA_EDT_AMT_1_6,
                   SPAFILE.SPA_EDT_AMTYPE_1_6,
                   SPAFILE.SPA_EDT_TERM_1_6,
                   SPAFILE.SPA_EDT_NOEDITS_2,
                   SPAFILE.SPA_EDT_TYPE_2_1,
                   SPAFILE.SPA_EDT_HL_2_1,
                   SPAFILE.SPA_EDT_COLL_2_1,
                   SPAFILE.SPA_EDT_CD_2_1,
                   SPAFILE.SPA_EDT_RATE_2_1,
                   SPAFILE.SPA_EDT_RATETYPE_2_1,
                   SPAFILE.SPA_EDT_AMT_2_1,
                   SPAFILE.SPA_EDT_AMTYPE_2_1,
                   SPAFILE.SPA_EDT_TERM_2_1,
                   SPAFILE.SPA_EDT_TYPE_2_2,
                   SPAFILE.SPA_EDT_HL_2_2,
                   SPAFILE.SPA_EDT_COLL_2_2,
                   SPAFILE.SPA_EDT_CD_2_2,
                   SPAFILE.SPA_EDT_RATE_2_2,
                   SPAFILE.SPA_EDT_RATETYPE_2_2,
                   SPAFILE.SPA_EDT_AMT_2_2,
                   SPAFILE.SPA_EDT_AMTYPE_2_2,
                   SPAFILE.SPA_EDT_TERM_2_2,
                   SPAFILE.SPA_EDT_TYPE_2_3,
                   SPAFILE.SPA_EDT_HL_2_3,
                   SPAFILE.SPA_EDT_COLL_2_3,
                   SPAFILE.SPA_EDT_CD_2_3,
                   SPAFILE.SPA_EDT_RATE_2_3,
                   SPAFILE.SPA_EDT_RATETYPE_2_3,
                   SPAFILE.SPA_EDT_AMT_2_3,
                   SPAFILE.SPA_EDT_AMTYPE_2_3,
                   SPAFILE.SPA_EDT_TERM_2_3,
                   SPAFILE.SPA_EDT_TYPE_2_4,
                   SPAFILE.SPA_EDT_HL_2_4,
                   SPAFILE.SPA_EDT_COLL_2_4,
                   SPAFILE.SPA_EDT_CD_2_4,
                   SPAFILE.SPA_EDT_RATE_2_4,
                   SPAFILE.SPA_EDT_RATETYPE_2_4,
                   SPAFILE.SPA_EDT_AMT_2_4,
                   SPAFILE.SPA_EDT_AMTYPE_2_4,
                   SPAFILE.SPA_EDT_TERM_2_4,
                   SPAFILE.SPA_EDT_TYPE_2_5,
                   SPAFILE.SPA_EDT_HL_2_5,
                   SPAFILE.SPA_EDT_COLL_2_5,
                   SPAFILE.SPA_EDT_CD_2_5,
                   SPAFILE.SPA_EDT_RATE_2_5,
                   SPAFILE.SPA_EDT_RATETYPE_2_5,
                   SPAFILE.SPA_EDT_AMT_2_5,
                   SPAFILE.SPA_EDT_AMTYPE_2_5,
                   SPAFILE.SPA_EDT_TERM_2_5,
                   SPAFILE.SPA_EDT_TYPE_2_6,
                   SPAFILE.SPA_EDT_HL_2_6,
                   SPAFILE.SPA_EDT_COLL_2_6,
                   SPAFILE.SPA_EDT_CD_2_6,
                   SPAFILE.SPA_EDT_RATE_2_6,
                   SPAFILE.SPA_EDT_RATETYPE_2_6,
                   SPAFILE.SPA_EDT_AMT_2_6,
                   SPAFILE.SPA_EDT_AMTYPE_2_6,
                   SPAFILE.SPA_EDT_TERM_2_6
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST >= :QSP1-WBEG-ORGST
              AND SPAFILE.SPA_ORGST <= :QSP1-WEND-ORGST
              AND SPAFILE.SPA_SPRCLASS >= :QSP1-WBEG-SPRCLASS
              AND SPAFILE.SPA_SPRCLASS <= :QSP1-WEND-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS >= :QSP1-WBEG-SUBCLASS
              AND SPAFILE.SPA_SUBCLASS <= :QSP1-WEND-SUBCLASS
              AND SPAFILE.SPA_LAWCODE >= :QSP1-WBEG-LAWCODE 
              AND SPAFILE.SPA_LAWCODE <= :QSP1-WEND-LAWCODE 
            ORDER BY SPAFILE.SPA_ORGST,
                     SPAFILE.SPA_SPRCLASS,
                     SPAFILE.SPA_SUBCLASS,
                     SPAFILE.SPA_LAWCODE
           END-EXEC.

           EXEC SQL
               OPEN SPA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSP1-WBEG-ORGST
                          QSP1-WEND-ORGST.

           MOVE ZEROES TO QSP1-WBEG-SPRCLASS
                          QSP1-WEND-SPRCLASS
                          QSP1-WBEG-SUBCLASS
                          QSP1-WEND-SUBCLASS
                          QSP1-WBEG-LAWCODE
                          QSP1-WEND-LAWCODE.

           MOVE STAT---GOOD TO SPA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SPA1-FILE.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           MOVE SPA-ORGST            TO QSPA-ORGST.
           MOVE SPA-SPRCLASS         TO QSPA-SPRCLASS.
           MOVE SPA-SUBCLASS         TO QSPA-SUBCLASS.
           MOVE SPA-LAWCODE          TO QSPA-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_1,''),
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_2,''),
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_3,''),
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_4,''),
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_5,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   SPAFILE.SPA_JUDGYRTYPE,
                   ISNULL(SPAFILE.SPA_JUDGINTMETH,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   SPAFILE.SPA_ACC_YRTYPE,
                   ISNULL(SPAFILE.SPA_ACC_INTMETH,''),
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_BREAK_5,
                   ISNULL(SPAFILE.SPA_DISFRMLA,''),
                   ISNULL(SPAFILE.SPA_CONTRFRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   SPAFILE.SPA_RECPDTH,
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNAMT_10,
                   ISNULL(SPAFILE.SPA_ERNPERCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNPERCD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_10,''),
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_1,
                   SPAFILE.SPA_ERN_TERM_2,
                   SPAFILE.SPA_ERN_TERM_3,
                   SPAFILE.SPA_ERN_TERM_4,
                   SPAFILE.SPA_ERN_TERM_5,
                   SPAFILE.SPA_ERN_TERM_6,
                   SPAFILE.SPA_ERN_TERM_7,
                   SPAFILE.SPA_ERN_TERM_8,
                   SPAFILE.SPA_ERN_TERM_9,
                   SPAFILE.SPA_ERN_TERM_10,
                   ISNULL(SPAFILE.SPA_ERNACCEL_CD,''),
                   ISNULL(SPAFILE.SPA_ERNJD_CD,''),
                   ISNULL(SPAFILE.SPA_ERNDELQFRMLA,''),
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_MFFRMLA,
                   ISNULL(SPAFILE.SPA_INTMETHOD,''),
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   ISNULL(SPAFILE.SPA_PYEXFMDATE,''),
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,''),
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_5
            INTO 
                  :QSPA-ORGST,
                  :QSPA-SPRCLASS,
                  :QSPA-SUBCLASS,
                  :QSPA-LAWCODE,
                  :QSPA-EXPD-MONTHS,
                  :QSPA-EXPD-DAYS,
                  :QSPA-EXPD-FRMLA,
                  :QSPA-EXPR-FRMLA,
                  :QSPA-EXPR-REDU-FRMLA,
                  :QSPA-EXPR-RATE-1,
                  :QSPA-EXPR-RATE-2,
                  :QSPA-EXPR-RATE-3,
                  :QSPA-EXPR-RATE-4,
                  :QSPA-EXPR-RATE-5,
                  :QSPA-EXPR-BREAK-1,
                  :QSPA-EXPR-BREAK-2,
                  :QSPA-EXPR-BREAK-3,
                  :QSPA-EXPR-BREAK-4,
                  :QSPA-EXPR-BREAK-5,
                  :QSPA-EXPR-YRTYPE-1,
                  :QSPA-EXPR-YRTYPE-2,
                  :QSPA-EXPR-YRTYPE-3,
                  :QSPA-EXPR-YRTYPE-4,
                  :QSPA-EXPR-YRTYPE-5,
                  :QSPA-EXPR-INTMETH-1, 
                  :QSPA-EXPR-INTMETH-2, 
                  :QSPA-EXPR-INTMETH-3, 
                  :QSPA-EXPR-INTMETH-4, 
                  :QSPA-EXPR-INTMETH-5, 
                  :QSPA-JUDGFRMLA,
                  :QSPA-JUDGYRTYPE,
                  :QSPA-JUDGINTMETH,
                  :QSPA-ACC-FRMLA,
                  :QSPA-ACC-YRTYPE,
                  :QSPA-ACC-INTMETH,
                  :QSPA-ACC-RATE-1,
                  :QSPA-ACC-RATE-2,
                  :QSPA-ACC-RATE-3,
                  :QSPA-ACC-RATE-4,
                  :QSPA-ACC-RATE-5,
                  :QSPA-ACC-BREAK-1,
                  :QSPA-ACC-BREAK-2,
                  :QSPA-ACC-BREAK-3,
                  :QSPA-ACC-BREAK-4,
                  :QSPA-ACC-BREAK-5,
                  :QSPA-DISFRMLA,
                  :QSPA-CONTRFRMLA,
                  :QSPA-BANKRULE,
                  :QSPA-CONTPDTH,
                  :QSPA-BANKUNITPER-CD,
                  :QSPA-RECFRMLA,
                  :QSPA-RECPDTH,
                  :QSPA-ERNFRMLA-1,
                  :QSPA-ERNFRMLA-2,
                  :QSPA-ERNFRMLA-3,
                  :QSPA-ERNFRMLA-4,
                  :QSPA-ERNFRMLA-5,
                  :QSPA-ERNFRMLA-6,
                  :QSPA-ERNFRMLA-7,
                  :QSPA-ERNFRMLA-8,
                  :QSPA-ERNFRMLA-9,
                  :QSPA-ERNFRMLA-10,
                  :QSPA-ERNAMT-1,
                  :QSPA-ERNAMT-2,
                  :QSPA-ERNAMT-3,
                  :QSPA-ERNAMT-4,
                  :QSPA-ERNAMT-5,
                  :QSPA-ERNAMT-6,
                  :QSPA-ERNAMT-7,
                  :QSPA-ERNAMT-8,
                  :QSPA-ERNAMT-9,
                  :QSPA-ERNAMT-10,
                  :QSPA-ERNPERCD-1, 
                  :QSPA-ERNPERCD-2, 
                  :QSPA-ERNPERCD-3, 
                  :QSPA-ERNPERCD-4, 
                  :QSPA-ERNPERCD-5, 
                  :QSPA-ERNPERCD-6, 
                  :QSPA-ERNPERCD-7, 
                  :QSPA-ERNPERCD-8, 
                  :QSPA-ERNPERCD-9, 
                  :QSPA-ERNPERCD-10, 
                  :QSPA-ERNACCRL-FG-1, 
                  :QSPA-ERNACCRL-FG-2, 
                  :QSPA-ERNACCRL-FG-3, 
                  :QSPA-ERNACCRL-FG-4, 
                  :QSPA-ERNACCRL-FG-5, 
                  :QSPA-ERNACCRL-FG-6, 
                  :QSPA-ERNACCRL-FG-7, 
                  :QSPA-ERNACCRL-FG-8, 
                  :QSPA-ERNACCRL-FG-9, 
                  :QSPA-ERNACCRL-FG-10,  
                  :QSPA-ERNACCRL-START-CD-1, 
                  :QSPA-ERNACCRL-START-CD-2, 
                  :QSPA-ERNACCRL-START-CD-3, 
                  :QSPA-ERNACCRL-START-CD-4, 
                  :QSPA-ERNACCRL-START-CD-5, 
                  :QSPA-ERNACCRL-START-CD-6, 
                  :QSPA-ERNACCRL-START-CD-7, 
                  :QSPA-ERNACCRL-START-CD-8, 
                  :QSPA-ERNACCRL-START-CD-9, 
                  :QSPA-ERNACCRL-START-CD-10, 
                  :QSPA-ERNACCRL-DFCD-1, 
                  :QSPA-ERNACCRL-DFCD-2, 
                  :QSPA-ERNACCRL-DFCD-3, 
                  :QSPA-ERNACCRL-DFCD-4, 
                  :QSPA-ERNACCRL-DFCD-5, 
                  :QSPA-ERNACCRL-DFCD-6, 
                  :QSPA-ERNACCRL-DFCD-7, 
                  :QSPA-ERNACCRL-DFCD-8, 
                  :QSPA-ERNACCRL-DFCD-9, 
                  :QSPA-ERNACCRL-DFCD-10, 
                  :QSPA-ERN-DAYS-1,
                  :QSPA-ERN-DAYS-2,
                  :QSPA-ERN-DAYS-3,
                  :QSPA-ERN-DAYS-4,
                  :QSPA-ERN-DAYS-5,
                  :QSPA-ERN-DAYS-6,
                  :QSPA-ERN-DAYS-7,
                  :QSPA-ERN-DAYS-8,
                  :QSPA-ERN-DAYS-9,
                  :QSPA-ERN-DAYS-10,
                  :QSPA-ERN-TERM-1,
                  :QSPA-ERN-TERM-2,
                  :QSPA-ERN-TERM-3,
                  :QSPA-ERN-TERM-4,
                  :QSPA-ERN-TERM-5,
                  :QSPA-ERN-TERM-6,
                  :QSPA-ERN-TERM-7,
                  :QSPA-ERN-TERM-8,
                  :QSPA-ERN-TERM-9,
                  :QSPA-ERN-TERM-10,
                  :QSPA-ERNACCEL-CD, 
                  :QSPA-ERNJD-CD, 
                  :QSPA-ERNDELQFRMLA, 
                  :QSPA-ERNDELQ-START,
                  :QSPA-ERNDELQ-STOP,
                  :QSPA-MAX-IB-ACR-DAYS,
                  :QSPA-LCUNITPER-CD, 
                  :QSPA-DEFUNITPER-CD, 
                  :QSPA-SCFRMLA,
                  :QSPA-MFFRMLA,
                  :QSPA-INTMETHOD, 
                  :QSPA-YEARTYPE,
                  :QSPA-PYEXFRMLA,
                  :QSPA-PYEXSPCODE,
                  :QSPA-PYEXYEARTP,
                  :QSPA-PYEXFMDATE, 
                  :QSPA-CAL-RATETYPE-1, 
                  :QSPA-CAL-RATETYPE-2, 
                  :QSPA-CAL-BREAKTYPE-1, 
                  :QSPA-CAL-BREAKTYPE-2, 
                  :QSPA-CAL-CHGABLE-1, 
                  :QSPA-CAL-CHGABLE-2, 
                  :QSPA-CAL-NORATES-1,
                  :QSPA-CAL-NORATES-2,
                  :QSPA-CAL-RATE-1-1,
                  :QSPA-CAL-RATE-1-2,
                  :QSPA-CAL-RATE-1-3,
                  :QSPA-CAL-RATE-1-4,
                  :QSPA-CAL-RATE-1-5,
                  :QSPA-CAL-RATE-2-1,
                  :QSPA-CAL-RATE-2-2,
                  :QSPA-CAL-RATE-2-3,
                  :QSPA-CAL-RATE-2-4,
                  :QSPA-CAL-RATE-2-5,
                  :QSPA-CAL-BREAK-1-1,
                  :QSPA-CAL-BREAK-1-2,
                  :QSPA-CAL-BREAK-1-3,
                  :QSPA-CAL-BREAK-1-4,
                  :QSPA-CAL-BREAK-1-5,
                  :QSPA-CAL-BREAK-2-1,
                  :QSPA-CAL-BREAK-2-2,
                  :QSPA-CAL-BREAK-2-3,
                  :QSPA-CAL-BREAK-2-4,
                  :QSPA-CAL-BREAK-2-5
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST    = :QSPA-ORGST
              AND SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
              AND SPAFILE.SPA_LAWCODE  = :QSPA-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       READ-SPA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.

           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SPA1_CURSOR INTO :QSPA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPA1-FILE.
           PERFORM CLOSE-IT.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-CURSOR-STAT.

      *=================================================================
       LOAD-SPB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPB-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPBFILE NOT = "S" )
              MOVE SPB-PATH-BEG          TO SPB-SELECT-PATH-BEG
              MOVE SPB-PATH-FILE         TO SPB-SELECT-PATH-FILE
           ELSE
              MOVE SPB-PATH-SQL          TO SPB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPB1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM OPEN-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPB1-FILE.
           PERFORM READ-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.
           MOVE SPB1-KEY TO E-KEYX.

           MOVE SPB-ORGST            TO QSPB-ORGST.
           MOVE SPB-SPRCLASS         TO QSPB-SPRCLASS.
           MOVE SPB-SUBCLASS         TO QSPB-SUBCLASS.
           MOVE SPB-LAWCODE          TO QSPB-LAWCODE.

           EXEC SQL
            SELECT SPBFILE.SPB_ORGST,
                   SPBFILE.SPB_SPRCLASS,
                   SPBFILE.SPB_SUBCLASS,
                   SPBFILE.SPB_LAWCODE,
                   SPBFILE.SPB_INSRATECD_1,
                   SPBFILE.SPB_INSRATECD_2,
                   SPBFILE.SPB_INSRATECD_3,
                   SPBFILE.SPB_INSRATECD_4,
                   SPBFILE.SPB_INSRATECD_5,
                   SPBFILE.SPB_INSRATECD_6,
                   SPBFILE.SPB_INSRATECD_7,
                   SPBFILE.SPB_INSRATECD_8,
                   SPBFILE.SPB_INSRATECD_9,
                   SPBFILE.SPB_INSRATECD_10,
                   SPBFILE.SPB_INSRATECD_11,
                   SPBFILE.SPB_INSRATECD_12,
                   SPBFILE.SPB_INSTERM_1_1,
                   SPBFILE.SPB_INSTERM_1_2,
                   SPBFILE.SPB_INSTERM_1_3,
                   SPBFILE.SPB_INSTERM_1_4,
                   SPBFILE.SPB_INSTERM_1_5,
                   SPBFILE.SPB_INSTERM_1_6,
                   SPBFILE.SPB_INSTERM_1_7,
                   SPBFILE.SPB_INSTERM_1_8,
                   SPBFILE.SPB_INSTERM_1_9,
                   SPBFILE.SPB_INSTERM_1_10,
                   SPBFILE.SPB_INSTERM_1_11,
                   SPBFILE.SPB_INSTERM_1_12,
                   SPBFILE.SPB_INSTERM_1_13,
                   SPBFILE.SPB_INSTERM_1_14,
                   SPBFILE.SPB_INSTERM_1_15,
                   SPBFILE.SPB_INSTERM_1_16,
                   SPBFILE.SPB_INSTERM_1_17,
                   SPBFILE.SPB_INSTERM_1_18,
                   SPBFILE.SPB_INSTERM_1_19,
                   SPBFILE.SPB_INSTERM_1_20,
                   SPBFILE.SPB_INSTERM_2_1,
                   SPBFILE.SPB_INSTERM_2_2,
                   SPBFILE.SPB_INSTERM_2_3,
                   SPBFILE.SPB_INSTERM_2_4,
                   SPBFILE.SPB_INSTERM_2_5,
                   SPBFILE.SPB_INSTERM_2_6,
                   SPBFILE.SPB_INSTERM_2_7,
                   SPBFILE.SPB_INSTERM_2_8,
                   SPBFILE.SPB_INSTERM_2_9,
                   SPBFILE.SPB_INSTERM_2_10,
                   SPBFILE.SPB_INSTERM_2_11,
                   SPBFILE.SPB_INSTERM_2_12,
                   SPBFILE.SPB_INSTERM_2_13,
                   SPBFILE.SPB_INSTERM_2_14,
                   SPBFILE.SPB_INSTERM_2_15,
                   SPBFILE.SPB_INSTERM_2_16,
                   SPBFILE.SPB_INSTERM_2_17,
                   SPBFILE.SPB_INSTERM_2_18,
                   SPBFILE.SPB_INSTERM_2_19,
                   SPBFILE.SPB_INSTERM_2_20,
                   SPBFILE.SPB_INSTERM_3_1,
                   SPBFILE.SPB_INSTERM_3_2,
                   SPBFILE.SPB_INSTERM_3_3,
                   SPBFILE.SPB_INSTERM_3_4,
                   SPBFILE.SPB_INSTERM_3_5,
                   SPBFILE.SPB_INSTERM_3_6,
                   SPBFILE.SPB_INSTERM_3_7,
                   SPBFILE.SPB_INSTERM_3_8,
                   SPBFILE.SPB_INSTERM_3_9,
                   SPBFILE.SPB_INSTERM_3_10,
                   SPBFILE.SPB_INSTERM_3_11,
                   SPBFILE.SPB_INSTERM_3_12,
                   SPBFILE.SPB_INSTERM_3_13,
                   SPBFILE.SPB_INSTERM_3_14,
                   SPBFILE.SPB_INSTERM_3_15,
                   SPBFILE.SPB_INSTERM_3_16,
                   SPBFILE.SPB_INSTERM_3_17,
                   SPBFILE.SPB_INSTERM_3_18,
                   SPBFILE.SPB_INSTERM_3_19,
                   SPBFILE.SPB_INSTERM_3_20,
                   SPBFILE.SPB_INSTERM_4_1,
                   SPBFILE.SPB_INSTERM_4_2,
                   SPBFILE.SPB_INSTERM_4_3,
                   SPBFILE.SPB_INSTERM_4_4,
                   SPBFILE.SPB_INSTERM_4_5,
                   SPBFILE.SPB_INSTERM_4_6,
                   SPBFILE.SPB_INSTERM_4_7,
                   SPBFILE.SPB_INSTERM_4_8,
                   SPBFILE.SPB_INSTERM_4_9,
                   SPBFILE.SPB_INSTERM_4_10,
                   SPBFILE.SPB_INSTERM_4_11,
                   SPBFILE.SPB_INSTERM_4_12,
                   SPBFILE.SPB_INSTERM_4_13,
                   SPBFILE.SPB_INSTERM_4_14,
                   SPBFILE.SPB_INSTERM_4_15,
                   SPBFILE.SPB_INSTERM_4_16,
                   SPBFILE.SPB_INSTERM_4_17,
                   SPBFILE.SPB_INSTERM_4_18,
                   SPBFILE.SPB_INSTERM_4_19,
                   SPBFILE.SPB_INSTERM_4_20,
                   SPBFILE.SPB_INSTERM_5_1,
                   SPBFILE.SPB_INSTERM_5_2,
                   SPBFILE.SPB_INSTERM_5_3,
                   SPBFILE.SPB_INSTERM_5_4,
                   SPBFILE.SPB_INSTERM_5_5,
                   SPBFILE.SPB_INSTERM_5_6,
                   SPBFILE.SPB_INSTERM_5_7,
                   SPBFILE.SPB_INSTERM_5_8,
                   SPBFILE.SPB_INSTERM_5_9,
                   SPBFILE.SPB_INSTERM_5_10,
                   SPBFILE.SPB_INSTERM_5_11,
                   SPBFILE.SPB_INSTERM_5_12,
                   SPBFILE.SPB_INSTERM_5_13,
                   SPBFILE.SPB_INSTERM_5_14,
                   SPBFILE.SPB_INSTERM_5_15,
                   SPBFILE.SPB_INSTERM_5_16,
                   SPBFILE.SPB_INSTERM_5_17,
                   SPBFILE.SPB_INSTERM_5_18,
                   SPBFILE.SPB_INSTERM_5_19,
                   SPBFILE.SPB_INSTERM_5_20,
                   SPBFILE.SPB_INSTERM_6_1,
                   SPBFILE.SPB_INSTERM_6_2,
                   SPBFILE.SPB_INSTERM_6_3,
                   SPBFILE.SPB_INSTERM_6_4,
                   SPBFILE.SPB_INSTERM_6_5,
                   SPBFILE.SPB_INSTERM_6_6,
                   SPBFILE.SPB_INSTERM_6_7,
                   SPBFILE.SPB_INSTERM_6_8,
                   SPBFILE.SPB_INSTERM_6_9,
                   SPBFILE.SPB_INSTERM_6_10,
                   SPBFILE.SPB_INSTERM_6_11,
                   SPBFILE.SPB_INSTERM_6_12,
                   SPBFILE.SPB_INSTERM_6_13,
                   SPBFILE.SPB_INSTERM_6_14,
                   SPBFILE.SPB_INSTERM_6_15,
                   SPBFILE.SPB_INSTERM_6_16,
                   SPBFILE.SPB_INSTERM_6_17,
                   SPBFILE.SPB_INSTERM_6_18,
                   SPBFILE.SPB_INSTERM_6_19,
                   SPBFILE.SPB_INSTERM_6_20,
                   SPBFILE.SPB_INSTERM_7_1,
                   SPBFILE.SPB_INSTERM_7_2,
                   SPBFILE.SPB_INSTERM_7_3,
                   SPBFILE.SPB_INSTERM_7_4,
                   SPBFILE.SPB_INSTERM_7_5,
                   SPBFILE.SPB_INSTERM_7_6,
                   SPBFILE.SPB_INSTERM_7_7,
                   SPBFILE.SPB_INSTERM_7_8,
                   SPBFILE.SPB_INSTERM_7_9,
                   SPBFILE.SPB_INSTERM_7_10,
                   SPBFILE.SPB_INSTERM_7_11,
                   SPBFILE.SPB_INSTERM_7_12,
                   SPBFILE.SPB_INSTERM_7_13,
                   SPBFILE.SPB_INSTERM_7_14,
                   SPBFILE.SPB_INSTERM_7_15,
                   SPBFILE.SPB_INSTERM_7_16,
                   SPBFILE.SPB_INSTERM_7_17,
                   SPBFILE.SPB_INSTERM_7_18,
                   SPBFILE.SPB_INSTERM_7_19,
                   SPBFILE.SPB_INSTERM_7_20,
                   SPBFILE.SPB_INSTERM_8_1,
                   SPBFILE.SPB_INSTERM_8_2,
                   SPBFILE.SPB_INSTERM_8_3,
                   SPBFILE.SPB_INSTERM_8_4,
                   SPBFILE.SPB_INSTERM_8_5,
                   SPBFILE.SPB_INSTERM_8_6,
                   SPBFILE.SPB_INSTERM_8_7,
                   SPBFILE.SPB_INSTERM_8_8,
                   SPBFILE.SPB_INSTERM_8_9,
                   SPBFILE.SPB_INSTERM_8_10,
                   SPBFILE.SPB_INSTERM_8_11,
                   SPBFILE.SPB_INSTERM_8_12,
                   SPBFILE.SPB_INSTERM_8_13,
                   SPBFILE.SPB_INSTERM_8_14,
                   SPBFILE.SPB_INSTERM_8_15,
                   SPBFILE.SPB_INSTERM_8_16,
                   SPBFILE.SPB_INSTERM_8_17,
                   SPBFILE.SPB_INSTERM_8_18,
                   SPBFILE.SPB_INSTERM_8_19,
                   SPBFILE.SPB_INSTERM_8_20,
                   SPBFILE.SPB_INSTERM_9_1,
                   SPBFILE.SPB_INSTERM_9_2,
                   SPBFILE.SPB_INSTERM_9_3,
                   SPBFILE.SPB_INSTERM_9_4,
                   SPBFILE.SPB_INSTERM_9_5,
                   SPBFILE.SPB_INSTERM_9_6,
                   SPBFILE.SPB_INSTERM_9_7,
                   SPBFILE.SPB_INSTERM_9_8,
                   SPBFILE.SPB_INSTERM_9_9,
                   SPBFILE.SPB_INSTERM_9_10,
                   SPBFILE.SPB_INSTERM_9_11,
                   SPBFILE.SPB_INSTERM_9_12,
                   SPBFILE.SPB_INSTERM_9_13,
                   SPBFILE.SPB_INSTERM_9_14,
                   SPBFILE.SPB_INSTERM_9_15,
                   SPBFILE.SPB_INSTERM_9_16,
                   SPBFILE.SPB_INSTERM_9_17,
                   SPBFILE.SPB_INSTERM_9_18,
                   SPBFILE.SPB_INSTERM_9_19,
                   SPBFILE.SPB_INSTERM_9_20,
                   SPBFILE.SPB_INSTERM_10_1,
                   SPBFILE.SPB_INSTERM_10_2,
                   SPBFILE.SPB_INSTERM_10_3,
                   SPBFILE.SPB_INSTERM_10_4,
                   SPBFILE.SPB_INSTERM_10_5,
                   SPBFILE.SPB_INSTERM_10_6,
                   SPBFILE.SPB_INSTERM_10_7,
                   SPBFILE.SPB_INSTERM_10_8,
                   SPBFILE.SPB_INSTERM_10_9,
                   SPBFILE.SPB_INSTERM_10_10,
                   SPBFILE.SPB_INSTERM_10_11,
                   SPBFILE.SPB_INSTERM_10_12,
                   SPBFILE.SPB_INSTERM_10_13,
                   SPBFILE.SPB_INSTERM_10_14,
                   SPBFILE.SPB_INSTERM_10_15,
                   SPBFILE.SPB_INSTERM_10_16,
                   SPBFILE.SPB_INSTERM_10_17,
                   SPBFILE.SPB_INSTERM_10_18,
                   SPBFILE.SPB_INSTERM_10_19,
                   SPBFILE.SPB_INSTERM_10_20,
                   SPBFILE.SPB_INSTERM_11_1,
                   SPBFILE.SPB_INSTERM_11_2,
                   SPBFILE.SPB_INSTERM_11_3,
                   SPBFILE.SPB_INSTERM_11_4,
                   SPBFILE.SPB_INSTERM_11_5,
                   SPBFILE.SPB_INSTERM_11_6,
                   SPBFILE.SPB_INSTERM_11_7,
                   SPBFILE.SPB_INSTERM_11_8,
                   SPBFILE.SPB_INSTERM_11_9,
                   SPBFILE.SPB_INSTERM_11_10,
                   SPBFILE.SPB_INSTERM_11_11,
                   SPBFILE.SPB_INSTERM_11_12,
                   SPBFILE.SPB_INSTERM_11_13,
                   SPBFILE.SPB_INSTERM_11_14,
                   SPBFILE.SPB_INSTERM_11_15,
                   SPBFILE.SPB_INSTERM_11_16,
                   SPBFILE.SPB_INSTERM_11_17,
                   SPBFILE.SPB_INSTERM_11_18,
                   SPBFILE.SPB_INSTERM_11_19,
                   SPBFILE.SPB_INSTERM_11_20,
                   SPBFILE.SPB_INSTERM_12_1,
                   SPBFILE.SPB_INSTERM_12_2,
                   SPBFILE.SPB_INSTERM_12_3,
                   SPBFILE.SPB_INSTERM_12_4,
                   SPBFILE.SPB_INSTERM_12_5,
                   SPBFILE.SPB_INSTERM_12_6,
                   SPBFILE.SPB_INSTERM_12_7,
                   SPBFILE.SPB_INSTERM_12_8,
                   SPBFILE.SPB_INSTERM_12_9,
                   SPBFILE.SPB_INSTERM_12_10,
                   SPBFILE.SPB_INSTERM_12_11,
                   SPBFILE.SPB_INSTERM_12_12,
                   SPBFILE.SPB_INSTERM_12_13,
                   SPBFILE.SPB_INSTERM_12_14,
                   SPBFILE.SPB_INSTERM_12_15,
                   SPBFILE.SPB_INSTERM_12_16,
                   SPBFILE.SPB_INSTERM_12_17,
                   SPBFILE.SPB_INSTERM_12_18,
                   SPBFILE.SPB_INSTERM_12_19,
                   SPBFILE.SPB_INSTERM_12_20,
                   SPBFILE.SPB_INSRATE_1_1,
                   SPBFILE.SPB_INSRATE_1_2,
                   SPBFILE.SPB_INSRATE_1_3,
                   SPBFILE.SPB_INSRATE_1_4,
                   SPBFILE.SPB_INSRATE_1_5,
                   SPBFILE.SPB_INSRATE_1_6,
                   SPBFILE.SPB_INSRATE_1_7,
                   SPBFILE.SPB_INSRATE_1_8,
                   SPBFILE.SPB_INSRATE_1_9,
                   SPBFILE.SPB_INSRATE_1_10,
                   SPBFILE.SPB_INSRATE_1_11,
                   SPBFILE.SPB_INSRATE_1_12,
                   SPBFILE.SPB_INSRATE_1_13,
                   SPBFILE.SPB_INSRATE_1_14,
                   SPBFILE.SPB_INSRATE_1_15,
                   SPBFILE.SPB_INSRATE_1_16,
                   SPBFILE.SPB_INSRATE_1_17,
                   SPBFILE.SPB_INSRATE_1_18,
                   SPBFILE.SPB_INSRATE_1_19,
                   SPBFILE.SPB_INSRATE_1_20,
                   SPBFILE.SPB_INSRATE_2_1,
                   SPBFILE.SPB_INSRATE_2_2,
                   SPBFILE.SPB_INSRATE_2_3,
                   SPBFILE.SPB_INSRATE_2_4,
                   SPBFILE.SPB_INSRATE_2_5,
                   SPBFILE.SPB_INSRATE_2_6,
                   SPBFILE.SPB_INSRATE_2_7,
                   SPBFILE.SPB_INSRATE_2_8,
                   SPBFILE.SPB_INSRATE_2_9,
                   SPBFILE.SPB_INSRATE_2_10,
                   SPBFILE.SPB_INSRATE_2_11,
                   SPBFILE.SPB_INSRATE_2_12,
                   SPBFILE.SPB_INSRATE_2_13,
                   SPBFILE.SPB_INSRATE_2_14,
                   SPBFILE.SPB_INSRATE_2_15,
                   SPBFILE.SPB_INSRATE_2_16,
                   SPBFILE.SPB_INSRATE_2_17,
                   SPBFILE.SPB_INSRATE_2_18,
                   SPBFILE.SPB_INSRATE_2_19,
                   SPBFILE.SPB_INSRATE_2_20,
                   SPBFILE.SPB_INSRATE_3_1,
                   SPBFILE.SPB_INSRATE_3_2,
                   SPBFILE.SPB_INSRATE_3_3,
                   SPBFILE.SPB_INSRATE_3_4,
                   SPBFILE.SPB_INSRATE_3_5,
                   SPBFILE.SPB_INSRATE_3_6,
                   SPBFILE.SPB_INSRATE_3_7,
                   SPBFILE.SPB_INSRATE_3_8,
                   SPBFILE.SPB_INSRATE_3_9,
                   SPBFILE.SPB_INSRATE_3_10,
                   SPBFILE.SPB_INSRATE_3_11,
                   SPBFILE.SPB_INSRATE_3_12,
                   SPBFILE.SPB_INSRATE_3_13,
                   SPBFILE.SPB_INSRATE_3_14,
                   SPBFILE.SPB_INSRATE_3_15,
                   SPBFILE.SPB_INSRATE_3_16,
                   SPBFILE.SPB_INSRATE_3_17,
                   SPBFILE.SPB_INSRATE_3_18,
                   SPBFILE.SPB_INSRATE_3_19,
                   SPBFILE.SPB_INSRATE_3_20,
                   SPBFILE.SPB_INSRATE_4_1,
                   SPBFILE.SPB_INSRATE_4_2,
                   SPBFILE.SPB_INSRATE_4_3,
                   SPBFILE.SPB_INSRATE_4_4,
                   SPBFILE.SPB_INSRATE_4_5,
                   SPBFILE.SPB_INSRATE_4_6,
                   SPBFILE.SPB_INSRATE_4_7,
                   SPBFILE.SPB_INSRATE_4_8,
                   SPBFILE.SPB_INSRATE_4_9,
                   SPBFILE.SPB_INSRATE_4_10,
                   SPBFILE.SPB_INSRATE_4_11,
                   SPBFILE.SPB_INSRATE_4_12,
                   SPBFILE.SPB_INSRATE_4_13,
                   SPBFILE.SPB_INSRATE_4_14,
                   SPBFILE.SPB_INSRATE_4_15,
                   SPBFILE.SPB_INSRATE_4_16,
                   SPBFILE.SPB_INSRATE_4_17,
                   SPBFILE.SPB_INSRATE_4_18,
                   SPBFILE.SPB_INSRATE_4_19,
                   SPBFILE.SPB_INSRATE_4_20,
                   SPBFILE.SPB_INSRATE_5_1,
                   SPBFILE.SPB_INSRATE_5_2,
                   SPBFILE.SPB_INSRATE_5_3,
                   SPBFILE.SPB_INSRATE_5_4,
                   SPBFILE.SPB_INSRATE_5_5,
                   SPBFILE.SPB_INSRATE_5_6,
                   SPBFILE.SPB_INSRATE_5_7,
                   SPBFILE.SPB_INSRATE_5_8,
                   SPBFILE.SPB_INSRATE_5_9,
                   SPBFILE.SPB_INSRATE_5_10,
                   SPBFILE.SPB_INSRATE_5_11,
                   SPBFILE.SPB_INSRATE_5_12,
                   SPBFILE.SPB_INSRATE_5_13,
                   SPBFILE.SPB_INSRATE_5_14,
                   SPBFILE.SPB_INSRATE_5_15,
                   SPBFILE.SPB_INSRATE_5_16,
                   SPBFILE.SPB_INSRATE_5_17,
                   SPBFILE.SPB_INSRATE_5_18,
                   SPBFILE.SPB_INSRATE_5_19,
                   SPBFILE.SPB_INSRATE_5_20,
                   SPBFILE.SPB_INSRATE_6_1,
                   SPBFILE.SPB_INSRATE_6_2,
                   SPBFILE.SPB_INSRATE_6_3,
                   SPBFILE.SPB_INSRATE_6_4,
                   SPBFILE.SPB_INSRATE_6_5,
                   SPBFILE.SPB_INSRATE_6_6,
                   SPBFILE.SPB_INSRATE_6_7,
                   SPBFILE.SPB_INSRATE_6_8,
                   SPBFILE.SPB_INSRATE_6_9,
                   SPBFILE.SPB_INSRATE_6_10,
                   SPBFILE.SPB_INSRATE_6_11,
                   SPBFILE.SPB_INSRATE_6_12,
                   SPBFILE.SPB_INSRATE_6_13,
                   SPBFILE.SPB_INSRATE_6_14,
                   SPBFILE.SPB_INSRATE_6_15,
                   SPBFILE.SPB_INSRATE_6_16,
                   SPBFILE.SPB_INSRATE_6_17,
                   SPBFILE.SPB_INSRATE_6_18,
                   SPBFILE.SPB_INSRATE_6_19,
                   SPBFILE.SPB_INSRATE_6_20,
                   SPBFILE.SPB_INSRATE_7_1,
                   SPBFILE.SPB_INSRATE_7_2,
                   SPBFILE.SPB_INSRATE_7_3,
                   SPBFILE.SPB_INSRATE_7_4,
                   SPBFILE.SPB_INSRATE_7_5,
                   SPBFILE.SPB_INSRATE_7_6,
                   SPBFILE.SPB_INSRATE_7_7,
                   SPBFILE.SPB_INSRATE_7_8,
                   SPBFILE.SPB_INSRATE_7_9,
                   SPBFILE.SPB_INSRATE_7_10,
                   SPBFILE.SPB_INSRATE_7_11,
                   SPBFILE.SPB_INSRATE_7_12,
                   SPBFILE.SPB_INSRATE_7_13,
                   SPBFILE.SPB_INSRATE_7_14,
                   SPBFILE.SPB_INSRATE_7_15,
                   SPBFILE.SPB_INSRATE_7_16,
                   SPBFILE.SPB_INSRATE_7_17,
                   SPBFILE.SPB_INSRATE_7_18,
                   SPBFILE.SPB_INSRATE_7_19,
                   SPBFILE.SPB_INSRATE_7_20,
                   SPBFILE.SPB_INSRATE_8_1,
                   SPBFILE.SPB_INSRATE_8_2,
                   SPBFILE.SPB_INSRATE_8_3,
                   SPBFILE.SPB_INSRATE_8_4,
                   SPBFILE.SPB_INSRATE_8_5,
                   SPBFILE.SPB_INSRATE_8_6,
                   SPBFILE.SPB_INSRATE_8_7,
                   SPBFILE.SPB_INSRATE_8_8,
                   SPBFILE.SPB_INSRATE_8_9,
                   SPBFILE.SPB_INSRATE_8_10,
                   SPBFILE.SPB_INSRATE_8_11,
                   SPBFILE.SPB_INSRATE_8_12,
                   SPBFILE.SPB_INSRATE_8_13,
                   SPBFILE.SPB_INSRATE_8_14,
                   SPBFILE.SPB_INSRATE_8_15,
                   SPBFILE.SPB_INSRATE_8_16,
                   SPBFILE.SPB_INSRATE_8_17,
                   SPBFILE.SPB_INSRATE_8_18,
                   SPBFILE.SPB_INSRATE_8_19,
                   SPBFILE.SPB_INSRATE_8_20,
                   SPBFILE.SPB_INSRATE_9_1,
                   SPBFILE.SPB_INSRATE_9_2,
                   SPBFILE.SPB_INSRATE_9_3,
                   SPBFILE.SPB_INSRATE_9_4,
                   SPBFILE.SPB_INSRATE_9_5,
                   SPBFILE.SPB_INSRATE_9_6,
                   SPBFILE.SPB_INSRATE_9_7,
                   SPBFILE.SPB_INSRATE_9_8,
                   SPBFILE.SPB_INSRATE_9_9,
                   SPBFILE.SPB_INSRATE_9_10,
                   SPBFILE.SPB_INSRATE_9_11,
                   SPBFILE.SPB_INSRATE_9_12,
                   SPBFILE.SPB_INSRATE_9_13,
                   SPBFILE.SPB_INSRATE_9_14,
                   SPBFILE.SPB_INSRATE_9_15,
                   SPBFILE.SPB_INSRATE_9_16,
                   SPBFILE.SPB_INSRATE_9_17,
                   SPBFILE.SPB_INSRATE_9_18,
                   SPBFILE.SPB_INSRATE_9_19,
                   SPBFILE.SPB_INSRATE_9_20,
                   SPBFILE.SPB_INSRATE_10_1,
                   SPBFILE.SPB_INSRATE_10_2,
                   SPBFILE.SPB_INSRATE_10_3,
                   SPBFILE.SPB_INSRATE_10_4,
                   SPBFILE.SPB_INSRATE_10_5,
                   SPBFILE.SPB_INSRATE_10_6,
                   SPBFILE.SPB_INSRATE_10_7,
                   SPBFILE.SPB_INSRATE_10_8,
                   SPBFILE.SPB_INSRATE_10_9,
                   SPBFILE.SPB_INSRATE_10_10,
                   SPBFILE.SPB_INSRATE_10_11,
                   SPBFILE.SPB_INSRATE_10_12,
                   SPBFILE.SPB_INSRATE_10_13,
                   SPBFILE.SPB_INSRATE_10_14,
                   SPBFILE.SPB_INSRATE_10_15,
                   SPBFILE.SPB_INSRATE_10_16,
                   SPBFILE.SPB_INSRATE_10_17,
                   SPBFILE.SPB_INSRATE_10_18,
                   SPBFILE.SPB_INSRATE_10_19,
                   SPBFILE.SPB_INSRATE_10_20,
                   SPBFILE.SPB_INSRATE_11_1,
                   SPBFILE.SPB_INSRATE_11_2,
                   SPBFILE.SPB_INSRATE_11_3,
                   SPBFILE.SPB_INSRATE_11_4,
                   SPBFILE.SPB_INSRATE_11_5,
                   SPBFILE.SPB_INSRATE_11_6,
                   SPBFILE.SPB_INSRATE_11_7,
                   SPBFILE.SPB_INSRATE_11_8,
                   SPBFILE.SPB_INSRATE_11_9,
                   SPBFILE.SPB_INSRATE_11_10,
                   SPBFILE.SPB_INSRATE_11_11,
                   SPBFILE.SPB_INSRATE_11_12,
                   SPBFILE.SPB_INSRATE_11_13,
                   SPBFILE.SPB_INSRATE_11_14,
                   SPBFILE.SPB_INSRATE_11_15,
                   SPBFILE.SPB_INSRATE_11_16,
                   SPBFILE.SPB_INSRATE_11_17,
                   SPBFILE.SPB_INSRATE_11_18,
                   SPBFILE.SPB_INSRATE_11_19,
                   SPBFILE.SPB_INSRATE_11_20,
                   SPBFILE.SPB_INSRATE_12_1,
                   SPBFILE.SPB_INSRATE_12_2,
                   SPBFILE.SPB_INSRATE_12_3,
                   SPBFILE.SPB_INSRATE_12_4,
                   SPBFILE.SPB_INSRATE_12_5,
                   SPBFILE.SPB_INSRATE_12_6,
                   SPBFILE.SPB_INSRATE_12_7,
                   SPBFILE.SPB_INSRATE_12_8,
                   SPBFILE.SPB_INSRATE_12_9,
                   SPBFILE.SPB_INSRATE_12_10,
                   SPBFILE.SPB_INSRATE_12_11,
                   SPBFILE.SPB_INSRATE_12_12,
                   SPBFILE.SPB_INSRATE_12_13,
                   SPBFILE.SPB_INSRATE_12_14,
                   SPBFILE.SPB_INSRATE_12_15,
                   SPBFILE.SPB_INSRATE_12_16,
                   SPBFILE.SPB_INSRATE_12_17,
                   SPBFILE.SPB_INSRATE_12_18,
                   SPBFILE.SPB_INSRATE_12_19,
                   SPBFILE.SPB_INSRATE_12_20 
            INTO  
                  :QSPB-ORGST,
                  :QSPB-SPRCLASS,
                  :QSPB-SUBCLASS,
                  :QSPB-LAWCODE,
                  :QSPB-INSRATECD-1,
                  :QSPB-INSRATECD-2,
                  :QSPB-INSRATECD-3,
                  :QSPB-INSRATECD-4,
                  :QSPB-INSRATECD-5,
                  :QSPB-INSRATECD-6,
                  :QSPB-INSRATECD-7,
                  :QSPB-INSRATECD-8,
                  :QSPB-INSRATECD-9,
                  :QSPB-INSRATECD-10,
                  :QSPB-INSRATECD-11,
                  :QSPB-INSRATECD-12,
                  :QSPB-INSTERM-1-1,
                  :QSPB-INSTERM-1-2,
                  :QSPB-INSTERM-1-3,
                  :QSPB-INSTERM-1-4,
                  :QSPB-INSTERM-1-5,
                  :QSPB-INSTERM-1-6,
                  :QSPB-INSTERM-1-7,
                  :QSPB-INSTERM-1-8,
                  :QSPB-INSTERM-1-9,
                  :QSPB-INSTERM-1-10,
                  :QSPB-INSTERM-1-11,
                  :QSPB-INSTERM-1-12,
                  :QSPB-INSTERM-1-13,
                  :QSPB-INSTERM-1-14,
                  :QSPB-INSTERM-1-15,
                  :QSPB-INSTERM-1-16,
                  :QSPB-INSTERM-1-17,
                  :QSPB-INSTERM-1-18,
                  :QSPB-INSTERM-1-19,
                  :QSPB-INSTERM-1-20,
                  :QSPB-INSTERM-2-1,
                  :QSPB-INSTERM-2-2,
                  :QSPB-INSTERM-2-3,
                  :QSPB-INSTERM-2-4,
                  :QSPB-INSTERM-2-5,
                  :QSPB-INSTERM-2-6,
                  :QSPB-INSTERM-2-7,
                  :QSPB-INSTERM-2-8,
                  :QSPB-INSTERM-2-9,
                  :QSPB-INSTERM-2-10,
                  :QSPB-INSTERM-2-11,
                  :QSPB-INSTERM-2-12,
                  :QSPB-INSTERM-2-13,
                  :QSPB-INSTERM-2-14,
                  :QSPB-INSTERM-2-15,
                  :QSPB-INSTERM-2-16,
                  :QSPB-INSTERM-2-17,
                  :QSPB-INSTERM-2-18,
                  :QSPB-INSTERM-2-19,
                  :QSPB-INSTERM-2-20,
                  :QSPB-INSTERM-3-1,
                  :QSPB-INSTERM-3-2,
                  :QSPB-INSTERM-3-3,
                  :QSPB-INSTERM-3-4,
                  :QSPB-INSTERM-3-5,
                  :QSPB-INSTERM-3-6,
                  :QSPB-INSTERM-3-7,
                  :QSPB-INSTERM-3-8,
                  :QSPB-INSTERM-3-9,
                  :QSPB-INSTERM-3-10,
                  :QSPB-INSTERM-3-11,
                  :QSPB-INSTERM-3-12,
                  :QSPB-INSTERM-3-13,
                  :QSPB-INSTERM-3-14,
                  :QSPB-INSTERM-3-15,
                  :QSPB-INSTERM-3-16,
                  :QSPB-INSTERM-3-17,
                  :QSPB-INSTERM-3-18,
                  :QSPB-INSTERM-3-19,
                  :QSPB-INSTERM-3-20,
                  :QSPB-INSTERM-4-1,
                  :QSPB-INSTERM-4-2,
                  :QSPB-INSTERM-4-3,
                  :QSPB-INSTERM-4-4,
                  :QSPB-INSTERM-4-5,
                  :QSPB-INSTERM-4-6,
                  :QSPB-INSTERM-4-7,
                  :QSPB-INSTERM-4-8,
                  :QSPB-INSTERM-4-9,
                  :QSPB-INSTERM-4-10,
                  :QSPB-INSTERM-4-11,
                  :QSPB-INSTERM-4-12,
                  :QSPB-INSTERM-4-13,
                  :QSPB-INSTERM-4-14,
                  :QSPB-INSTERM-4-15,
                  :QSPB-INSTERM-4-16,
                  :QSPB-INSTERM-4-17,
                  :QSPB-INSTERM-4-18,
                  :QSPB-INSTERM-4-19,
                  :QSPB-INSTERM-4-20,
                  :QSPB-INSTERM-5-1,
                  :QSPB-INSTERM-5-2,
                  :QSPB-INSTERM-5-3,
                  :QSPB-INSTERM-5-4,
                  :QSPB-INSTERM-5-5,
                  :QSPB-INSTERM-5-6,
                  :QSPB-INSTERM-5-7,
                  :QSPB-INSTERM-5-8,
                  :QSPB-INSTERM-5-9,
                  :QSPB-INSTERM-5-10,
                  :QSPB-INSTERM-5-11,
                  :QSPB-INSTERM-5-12,
                  :QSPB-INSTERM-5-13,
                  :QSPB-INSTERM-5-14,
                  :QSPB-INSTERM-5-15,
                  :QSPB-INSTERM-5-16,
                  :QSPB-INSTERM-5-17,
                  :QSPB-INSTERM-5-18,
                  :QSPB-INSTERM-5-19,
                  :QSPB-INSTERM-5-20,
                  :QSPB-INSTERM-6-1,
                  :QSPB-INSTERM-6-2,
                  :QSPB-INSTERM-6-3,
                  :QSPB-INSTERM-6-4,
                  :QSPB-INSTERM-6-5,
                  :QSPB-INSTERM-6-6,
                  :QSPB-INSTERM-6-7,
                  :QSPB-INSTERM-6-8,
                  :QSPB-INSTERM-6-9,
                  :QSPB-INSTERM-6-10,
                  :QSPB-INSTERM-6-11,
                  :QSPB-INSTERM-6-12,
                  :QSPB-INSTERM-6-13,
                  :QSPB-INSTERM-6-14,
                  :QSPB-INSTERM-6-15,
                  :QSPB-INSTERM-6-16,
                  :QSPB-INSTERM-6-17,
                  :QSPB-INSTERM-6-18,
                  :QSPB-INSTERM-6-19,
                  :QSPB-INSTERM-6-20,
                  :QSPB-INSTERM-7-1,
                  :QSPB-INSTERM-7-2,
                  :QSPB-INSTERM-7-3,
                  :QSPB-INSTERM-7-4,
                  :QSPB-INSTERM-7-5,
                  :QSPB-INSTERM-7-6,
                  :QSPB-INSTERM-7-7,
                  :QSPB-INSTERM-7-8,
                  :QSPB-INSTERM-7-9,
                  :QSPB-INSTERM-7-10,
                  :QSPB-INSTERM-7-11,
                  :QSPB-INSTERM-7-12,
                  :QSPB-INSTERM-7-13,
                  :QSPB-INSTERM-7-14,
                  :QSPB-INSTERM-7-15,
                  :QSPB-INSTERM-7-16,
                  :QSPB-INSTERM-7-17,
                  :QSPB-INSTERM-7-18,
                  :QSPB-INSTERM-7-19,
                  :QSPB-INSTERM-7-20,
                  :QSPB-INSTERM-8-1,
                  :QSPB-INSTERM-8-2,
                  :QSPB-INSTERM-8-3,
                  :QSPB-INSTERM-8-4,
                  :QSPB-INSTERM-8-5,
                  :QSPB-INSTERM-8-6,
                  :QSPB-INSTERM-8-7,
                  :QSPB-INSTERM-8-8,
                  :QSPB-INSTERM-8-9,
                  :QSPB-INSTERM-8-10,
                  :QSPB-INSTERM-8-11,
                  :QSPB-INSTERM-8-12,
                  :QSPB-INSTERM-8-13,
                  :QSPB-INSTERM-8-14,
                  :QSPB-INSTERM-8-15,
                  :QSPB-INSTERM-8-16,
                  :QSPB-INSTERM-8-17,
                  :QSPB-INSTERM-8-18,
                  :QSPB-INSTERM-8-19,
                  :QSPB-INSTERM-8-20,
                  :QSPB-INSTERM-9-1,
                  :QSPB-INSTERM-9-2,
                  :QSPB-INSTERM-9-3,
                  :QSPB-INSTERM-9-4,
                  :QSPB-INSTERM-9-5,
                  :QSPB-INSTERM-9-6,
                  :QSPB-INSTERM-9-7,
                  :QSPB-INSTERM-9-8,
                  :QSPB-INSTERM-9-9,
                  :QSPB-INSTERM-9-10,
                  :QSPB-INSTERM-9-11,
                  :QSPB-INSTERM-9-12,
                  :QSPB-INSTERM-9-13,
                  :QSPB-INSTERM-9-14,
                  :QSPB-INSTERM-9-15,
                  :QSPB-INSTERM-9-16,
                  :QSPB-INSTERM-9-17,
                  :QSPB-INSTERM-9-18,
                  :QSPB-INSTERM-9-19,
                  :QSPB-INSTERM-9-20,
                  :QSPB-INSTERM-10-1,
                  :QSPB-INSTERM-10-2,
                  :QSPB-INSTERM-10-3,
                  :QSPB-INSTERM-10-4,
                  :QSPB-INSTERM-10-5,
                  :QSPB-INSTERM-10-6,
                  :QSPB-INSTERM-10-7,
                  :QSPB-INSTERM-10-8,
                  :QSPB-INSTERM-10-9,
                  :QSPB-INSTERM-10-10,
                  :QSPB-INSTERM-10-11,
                  :QSPB-INSTERM-10-12,
                  :QSPB-INSTERM-10-13,
                  :QSPB-INSTERM-10-14,
                  :QSPB-INSTERM-10-15,
                  :QSPB-INSTERM-10-16,
                  :QSPB-INSTERM-10-17,
                  :QSPB-INSTERM-10-18,
                  :QSPB-INSTERM-10-19,
                  :QSPB-INSTERM-10-20,
                  :QSPB-INSTERM-11-1,
                  :QSPB-INSTERM-11-2,
                  :QSPB-INSTERM-11-3,
                  :QSPB-INSTERM-11-4,
                  :QSPB-INSTERM-11-5,
                  :QSPB-INSTERM-11-6,
                  :QSPB-INSTERM-11-7,
                  :QSPB-INSTERM-11-8,
                  :QSPB-INSTERM-11-9,
                  :QSPB-INSTERM-11-10,
                  :QSPB-INSTERM-11-11,
                  :QSPB-INSTERM-11-12,
                  :QSPB-INSTERM-11-13,
                  :QSPB-INSTERM-11-14,
                  :QSPB-INSTERM-11-15,
                  :QSPB-INSTERM-11-16,
                  :QSPB-INSTERM-11-17,
                  :QSPB-INSTERM-11-18,
                  :QSPB-INSTERM-11-19,
                  :QSPB-INSTERM-11-20,
                  :QSPB-INSTERM-12-1,
                  :QSPB-INSTERM-12-2,
                  :QSPB-INSTERM-12-3,
                  :QSPB-INSTERM-12-4,
                  :QSPB-INSTERM-12-5,
                  :QSPB-INSTERM-12-6,
                  :QSPB-INSTERM-12-7,
                  :QSPB-INSTERM-12-8,
                  :QSPB-INSTERM-12-9,
                  :QSPB-INSTERM-12-10,
                  :QSPB-INSTERM-12-11,
                  :QSPB-INSTERM-12-12,
                  :QSPB-INSTERM-12-13,
                  :QSPB-INSTERM-12-14,
                  :QSPB-INSTERM-12-15,
                  :QSPB-INSTERM-12-16,
                  :QSPB-INSTERM-12-17,
                  :QSPB-INSTERM-12-18,
                  :QSPB-INSTERM-12-19,
                  :QSPB-INSTERM-12-20,
                  :QSPB-INSRATE-1-1,
                  :QSPB-INSRATE-1-2,
                  :QSPB-INSRATE-1-3,
                  :QSPB-INSRATE-1-4,
                  :QSPB-INSRATE-1-5,
                  :QSPB-INSRATE-1-6,
                  :QSPB-INSRATE-1-7,
                  :QSPB-INSRATE-1-8,
                  :QSPB-INSRATE-1-9,
                  :QSPB-INSRATE-1-10,
                  :QSPB-INSRATE-1-11,
                  :QSPB-INSRATE-1-12,
                  :QSPB-INSRATE-1-13,
                  :QSPB-INSRATE-1-14,
                  :QSPB-INSRATE-1-15,
                  :QSPB-INSRATE-1-16,
                  :QSPB-INSRATE-1-17,
                  :QSPB-INSRATE-1-18,
                  :QSPB-INSRATE-1-19,
                  :QSPB-INSRATE-1-20,
                  :QSPB-INSRATE-2-1,
                  :QSPB-INSRATE-2-2,
                  :QSPB-INSRATE-2-3,
                  :QSPB-INSRATE-2-4,
                  :QSPB-INSRATE-2-5,
                  :QSPB-INSRATE-2-6,
                  :QSPB-INSRATE-2-7,
                  :QSPB-INSRATE-2-8,
                  :QSPB-INSRATE-2-9,
                  :QSPB-INSRATE-2-10,
                  :QSPB-INSRATE-2-11,
                  :QSPB-INSRATE-2-12,
                  :QSPB-INSRATE-2-13,
                  :QSPB-INSRATE-2-14,
                  :QSPB-INSRATE-2-15,
                  :QSPB-INSRATE-2-16,
                  :QSPB-INSRATE-2-17,
                  :QSPB-INSRATE-2-18,
                  :QSPB-INSRATE-2-19,
                  :QSPB-INSRATE-2-20,
                  :QSPB-INSRATE-3-1,
                  :QSPB-INSRATE-3-2,
                  :QSPB-INSRATE-3-3,
                  :QSPB-INSRATE-3-4,
                  :QSPB-INSRATE-3-5,
                  :QSPB-INSRATE-3-6,
                  :QSPB-INSRATE-3-7,
                  :QSPB-INSRATE-3-8,
                  :QSPB-INSRATE-3-9,
                  :QSPB-INSRATE-3-10,
                  :QSPB-INSRATE-3-11,
                  :QSPB-INSRATE-3-12,
                  :QSPB-INSRATE-3-13,
                  :QSPB-INSRATE-3-14,
                  :QSPB-INSRATE-3-15,
                  :QSPB-INSRATE-3-16,
                  :QSPB-INSRATE-3-17,
                  :QSPB-INSRATE-3-18,
                  :QSPB-INSRATE-3-19,
                  :QSPB-INSRATE-3-20,
                  :QSPB-INSRATE-4-1,
                  :QSPB-INSRATE-4-2,
                  :QSPB-INSRATE-4-3,
                  :QSPB-INSRATE-4-4,
                  :QSPB-INSRATE-4-5,
                  :QSPB-INSRATE-4-6,
                  :QSPB-INSRATE-4-7,
                  :QSPB-INSRATE-4-8,
                  :QSPB-INSRATE-4-9,
                  :QSPB-INSRATE-4-10,
                  :QSPB-INSRATE-4-11,
                  :QSPB-INSRATE-4-12,
                  :QSPB-INSRATE-4-13,
                  :QSPB-INSRATE-4-14,
                  :QSPB-INSRATE-4-15,
                  :QSPB-INSRATE-4-16,
                  :QSPB-INSRATE-4-17,
                  :QSPB-INSRATE-4-18,
                  :QSPB-INSRATE-4-19,
                  :QSPB-INSRATE-4-20,
                  :QSPB-INSRATE-5-1,
                  :QSPB-INSRATE-5-2,
                  :QSPB-INSRATE-5-3,
                  :QSPB-INSRATE-5-4,
                  :QSPB-INSRATE-5-5,
                  :QSPB-INSRATE-5-6,
                  :QSPB-INSRATE-5-7,
                  :QSPB-INSRATE-5-8,
                  :QSPB-INSRATE-5-9,
                  :QSPB-INSRATE-5-10,
                  :QSPB-INSRATE-5-11,
                  :QSPB-INSRATE-5-12,
                  :QSPB-INSRATE-5-13,
                  :QSPB-INSRATE-5-14,
                  :QSPB-INSRATE-5-15,
                  :QSPB-INSRATE-5-16,
                  :QSPB-INSRATE-5-17,
                  :QSPB-INSRATE-5-18,
                  :QSPB-INSRATE-5-19,
                  :QSPB-INSRATE-5-20,
                  :QSPB-INSRATE-6-1,
                  :QSPB-INSRATE-6-2,
                  :QSPB-INSRATE-6-3,
                  :QSPB-INSRATE-6-4,
                  :QSPB-INSRATE-6-5,
                  :QSPB-INSRATE-6-6,
                  :QSPB-INSRATE-6-7,
                  :QSPB-INSRATE-6-8,
                  :QSPB-INSRATE-6-9,
                  :QSPB-INSRATE-6-10,
                  :QSPB-INSRATE-6-11,
                  :QSPB-INSRATE-6-12,
                  :QSPB-INSRATE-6-13,
                  :QSPB-INSRATE-6-14,
                  :QSPB-INSRATE-6-15,
                  :QSPB-INSRATE-6-16,
                  :QSPB-INSRATE-6-17,
                  :QSPB-INSRATE-6-18,
                  :QSPB-INSRATE-6-19,
                  :QSPB-INSRATE-6-20,
                  :QSPB-INSRATE-7-1,
                  :QSPB-INSRATE-7-2,
                  :QSPB-INSRATE-7-3,
                  :QSPB-INSRATE-7-4,
                  :QSPB-INSRATE-7-5,
                  :QSPB-INSRATE-7-6,
                  :QSPB-INSRATE-7-7,
                  :QSPB-INSRATE-7-8,
                  :QSPB-INSRATE-7-9,
                  :QSPB-INSRATE-7-10,
                  :QSPB-INSRATE-7-11,
                  :QSPB-INSRATE-7-12,
                  :QSPB-INSRATE-7-13,
                  :QSPB-INSRATE-7-14,
                  :QSPB-INSRATE-7-15,
                  :QSPB-INSRATE-7-16,
                  :QSPB-INSRATE-7-17,
                  :QSPB-INSRATE-7-18,
                  :QSPB-INSRATE-7-19,
                  :QSPB-INSRATE-7-20,
                  :QSPB-INSRATE-8-1,
                  :QSPB-INSRATE-8-2,
                  :QSPB-INSRATE-8-3,
                  :QSPB-INSRATE-8-4,
                  :QSPB-INSRATE-8-5,
                  :QSPB-INSRATE-8-6,
                  :QSPB-INSRATE-8-7,
                  :QSPB-INSRATE-8-8,
                  :QSPB-INSRATE-8-9,
                  :QSPB-INSRATE-8-10,
                  :QSPB-INSRATE-8-11,
                  :QSPB-INSRATE-8-12,
                  :QSPB-INSRATE-8-13,
                  :QSPB-INSRATE-8-14,
                  :QSPB-INSRATE-8-15,
                  :QSPB-INSRATE-8-16,
                  :QSPB-INSRATE-8-17,
                  :QSPB-INSRATE-8-18,
                  :QSPB-INSRATE-8-19,
                  :QSPB-INSRATE-8-20,
                  :QSPB-INSRATE-9-1,
                  :QSPB-INSRATE-9-2,
                  :QSPB-INSRATE-9-3,
                  :QSPB-INSRATE-9-4,
                  :QSPB-INSRATE-9-5,
                  :QSPB-INSRATE-9-6,
                  :QSPB-INSRATE-9-7,
                  :QSPB-INSRATE-9-8,
                  :QSPB-INSRATE-9-9,
                  :QSPB-INSRATE-9-10,
                  :QSPB-INSRATE-9-11,
                  :QSPB-INSRATE-9-12,
                  :QSPB-INSRATE-9-13,
                  :QSPB-INSRATE-9-14,
                  :QSPB-INSRATE-9-15,
                  :QSPB-INSRATE-9-16,
                  :QSPB-INSRATE-9-17,
                  :QSPB-INSRATE-9-18,
                  :QSPB-INSRATE-9-19,
                  :QSPB-INSRATE-9-20,
                  :QSPB-INSRATE-10-1,
                  :QSPB-INSRATE-10-2,
                  :QSPB-INSRATE-10-3,
                  :QSPB-INSRATE-10-4,
                  :QSPB-INSRATE-10-5,
                  :QSPB-INSRATE-10-6,
                  :QSPB-INSRATE-10-7,
                  :QSPB-INSRATE-10-8,
                  :QSPB-INSRATE-10-9,
                  :QSPB-INSRATE-10-10,
                  :QSPB-INSRATE-10-11,
                  :QSPB-INSRATE-10-12,
                  :QSPB-INSRATE-10-13,
                  :QSPB-INSRATE-10-14,
                  :QSPB-INSRATE-10-15,
                  :QSPB-INSRATE-10-16,
                  :QSPB-INSRATE-10-17,
                  :QSPB-INSRATE-10-18,
                  :QSPB-INSRATE-10-19,
                  :QSPB-INSRATE-10-20,
                  :QSPB-INSRATE-11-1,
                  :QSPB-INSRATE-11-2,
                  :QSPB-INSRATE-11-3,
                  :QSPB-INSRATE-11-4,
                  :QSPB-INSRATE-11-5,
                  :QSPB-INSRATE-11-6,
                  :QSPB-INSRATE-11-7,
                  :QSPB-INSRATE-11-8,
                  :QSPB-INSRATE-11-9,
                  :QSPB-INSRATE-11-10,
                  :QSPB-INSRATE-11-11,
                  :QSPB-INSRATE-11-12,
                  :QSPB-INSRATE-11-13,
                  :QSPB-INSRATE-11-14,
                  :QSPB-INSRATE-11-15,
                  :QSPB-INSRATE-11-16,
                  :QSPB-INSRATE-11-17,
                  :QSPB-INSRATE-11-18,
                  :QSPB-INSRATE-11-19,
                  :QSPB-INSRATE-11-20,
                  :QSPB-INSRATE-12-1,
                  :QSPB-INSRATE-12-2,
                  :QSPB-INSRATE-12-3,
                  :QSPB-INSRATE-12-4,
                  :QSPB-INSRATE-12-5,
                  :QSPB-INSRATE-12-6,
                  :QSPB-INSRATE-12-7,
                  :QSPB-INSRATE-12-8,
                  :QSPB-INSRATE-12-9,
                  :QSPB-INSRATE-12-10,
                  :QSPB-INSRATE-12-11,
                  :QSPB-INSRATE-12-12,
                  :QSPB-INSRATE-12-13,
                  :QSPB-INSRATE-12-14,
                  :QSPB-INSRATE-12-15,
                  :QSPB-INSRATE-12-16,
                  :QSPB-INSRATE-12-17,
                  :QSPB-INSRATE-12-18,
                  :QSPB-INSRATE-12-19,
                  :QSPB-INSRATE-12-20
            FROM DBO.SPBFILE
            WHERE SPBFILE.SPB_ORGST    = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE  = :QSPB-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPB1-FILE.
           PERFORM CLOSE-IT.

      *=================================================================
       LOAD-SPC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPC-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPCFILE NOT = "S" )
              MOVE SPC-PATH-BEG          TO SPC-SELECT-PATH-BEG
              MOVE SPC-PATH-FILE         TO SPC-SELECT-PATH-FILE
           ELSE
              MOVE SPC-PATH-SQL          TO SPC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPC1-FILE.
           PERFORM LOAD-SPC1-FILE.
           PERFORM OPEN-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPC1-FILE.
           PERFORM READ-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.

           MOVE SPC-ORGST            TO QSPC-ORGST.
           MOVE SPC-SPRCLASS         TO QSPC-SPRCLASS.
           MOVE SPC-SUBCLASS         TO QSPC-SUBCLASS.
           MOVE SPC-LAWCODE          TO QSPC-LAWCODE.

           EXEC SQL
            SELECT SPCFILE.SPC_ORGST,
                   SPCFILE.SPC_SPRCLASS,
                   SPCFILE.SPC_SUBCLASS,
                   SPCFILE.SPC_LAWCODE,
                   SPCFILE.SPC_RBFRMLA_1,
                   SPCFILE.SPC_RBFRMLA_2,
                   SPCFILE.SPC_RBFRMLA_3,
                   SPCFILE.SPC_RBFRMLA_4,
                   SPCFILE.SPC_RBFRMLA_5,
                   SPCFILE.SPC_RBFRMLA_6,
                   SPCFILE.SPC_RBFRMLA_7,
                   SPCFILE.SPC_RBFRMLA_8,
                   SPCFILE.SPC_RBFRMLA_9,
                   SPCFILE.SPC_RBFRMLA_10,
                   SPCFILE.SPC_RBFRMLA_11,
                   SPCFILE.SPC_RBFRMLA_12,
                   SPCFILE.SPC_RBFRMLA_13,
                   SPCFILE.SPC_RBFRMLA_14,
                   SPCFILE.SPC_RBFRMLA_15,
                   SPCFILE.SPC_RBFRMLA_16,
                   SPCFILE.SPC_RBFRMLA_17,
                   SPCFILE.SPC_RBFRMLA_18,
                   SPCFILE.SPC_RBFRMLA_19,
                   SPCFILE.SPC_RBFRMLA_20,
                   SPCFILE.SPC_RBFRMLA_21,
                   SPCFILE.SPC_RBFRMLA_22,
                   SPCFILE.SPC_RBDTE_1_1,
                   SPCFILE.SPC_RBDTE_1_2,
                   SPCFILE.SPC_RBDTE_2_1,
                   SPCFILE.SPC_RBDTE_2_2,
                   SPCFILE.SPC_RBDTE_3_1,
                   SPCFILE.SPC_RBDTE_3_2,
                   SPCFILE.SPC_RBDTE_4_1,
                   SPCFILE.SPC_RBDTE_4_2,
                   SPCFILE.SPC_RBDTE_5_1,
                   SPCFILE.SPC_RBDTE_5_2,
                   SPCFILE.SPC_RBDTE_6_1,
                   SPCFILE.SPC_RBDTE_6_2,
                   SPCFILE.SPC_RBDTE_7_1,
                   SPCFILE.SPC_RBDTE_7_2,
                   SPCFILE.SPC_RBDTE_8_1,
                   SPCFILE.SPC_RBDTE_8_2,
                   SPCFILE.SPC_RBDTE_9_1,
                   SPCFILE.SPC_RBDTE_9_2,
                   SPCFILE.SPC_RBDTE_10_1,
                   SPCFILE.SPC_RBDTE_10_2,
                   SPCFILE.SPC_RBDTE_11_1,
                   SPCFILE.SPC_RBDTE_11_2,
                   SPCFILE.SPC_RBDTE_12_1,
                   SPCFILE.SPC_RBDTE_12_2,
                   SPCFILE.SPC_RBDTE_13_1,
                   SPCFILE.SPC_RBDTE_13_2,
                   SPCFILE.SPC_RBDTE_14_1,
                   SPCFILE.SPC_RBDTE_14_2,
                   SPCFILE.SPC_RBDTE_15_1,
                   SPCFILE.SPC_RBDTE_15_2,
                   SPCFILE.SPC_RBDTE_16_1,
                   SPCFILE.SPC_RBDTE_16_2,
                   SPCFILE.SPC_RBDTE_17_1,
                   SPCFILE.SPC_RBDTE_17_2,
                   SPCFILE.SPC_RBDTE_18_1,
                   SPCFILE.SPC_RBDTE_18_2,
                   SPCFILE.SPC_RBDTE_19_1,
                   SPCFILE.SPC_RBDTE_19_2,
                   SPCFILE.SPC_RBDTE_20_1,
                   SPCFILE.SPC_RBDTE_20_2,
                   SPCFILE.SPC_RBDTE_21_1,
                   SPCFILE.SPC_RBDTE_21_2,
                   SPCFILE.SPC_RBDTE_22_1,
                   SPCFILE.SPC_RBDTE_22_2,
                   SPCFILE.SPC_RBDAYS_1_1,
                   SPCFILE.SPC_RBDAYS_1_2,
                   SPCFILE.SPC_RBDAYS_2_1,
                   SPCFILE.SPC_RBDAYS_2_2,
                   SPCFILE.SPC_RBDAYS_3_1,
                   SPCFILE.SPC_RBDAYS_3_2,
                   SPCFILE.SPC_RBDAYS_4_1,
                   SPCFILE.SPC_RBDAYS_4_2,
                   SPCFILE.SPC_RBDAYS_5_1,
                   SPCFILE.SPC_RBDAYS_5_2,
                   SPCFILE.SPC_RBDAYS_6_1,
                   SPCFILE.SPC_RBDAYS_6_2,
                   SPCFILE.SPC_RBDAYS_7_1,
                   SPCFILE.SPC_RBDAYS_7_2,
                   SPCFILE.SPC_RBDAYS_8_1,
                   SPCFILE.SPC_RBDAYS_8_2,
                   SPCFILE.SPC_RBDAYS_9_1,
                   SPCFILE.SPC_RBDAYS_9_2,
                   SPCFILE.SPC_RBDAYS_10_1,
                   SPCFILE.SPC_RBDAYS_10_2,
                   SPCFILE.SPC_RBDAYS_11_1,
                   SPCFILE.SPC_RBDAYS_11_2,
                   SPCFILE.SPC_RBDAYS_12_1,
                   SPCFILE.SPC_RBDAYS_12_2,
                   SPCFILE.SPC_RBDAYS_13_1,
                   SPCFILE.SPC_RBDAYS_13_2,
                   SPCFILE.SPC_RBDAYS_14_1,
                   SPCFILE.SPC_RBDAYS_14_2,
                   SPCFILE.SPC_RBDAYS_15_1,
                   SPCFILE.SPC_RBDAYS_15_2,
                   SPCFILE.SPC_RBDAYS_16_1,
                   SPCFILE.SPC_RBDAYS_16_2,
                   SPCFILE.SPC_RBDAYS_17_1,
                   SPCFILE.SPC_RBDAYS_17_2,
                   SPCFILE.SPC_RBDAYS_18_1,
                   SPCFILE.SPC_RBDAYS_18_2,
                   SPCFILE.SPC_RBDAYS_19_1,
                   SPCFILE.SPC_RBDAYS_19_2,
                   SPCFILE.SPC_RBDAYS_20_1,
                   SPCFILE.SPC_RBDAYS_20_2,
                   SPCFILE.SPC_RBDAYS_21_1,
                   SPCFILE.SPC_RBDAYS_21_2,
                   SPCFILE.SPC_RBDAYS_22_1,
                   SPCFILE.SPC_RBDAYS_22_2,
                   SPCFILE.SPC_RBYRTYPE_1_1,
                   SPCFILE.SPC_RBYRTYPE_1_2,
                   SPCFILE.SPC_RBYRTYPE_2_1,
                   SPCFILE.SPC_RBYRTYPE_2_2,
                   SPCFILE.SPC_RBYRTYPE_3_1,
                   SPCFILE.SPC_RBYRTYPE_3_2,
                   SPCFILE.SPC_RBYRTYPE_4_1,
                   SPCFILE.SPC_RBYRTYPE_4_2,
                   SPCFILE.SPC_RBYRTYPE_5_1,
                   SPCFILE.SPC_RBYRTYPE_5_2,
                   SPCFILE.SPC_RBYRTYPE_6_1,
                   SPCFILE.SPC_RBYRTYPE_6_2,
                   SPCFILE.SPC_RBYRTYPE_7_1,
                   SPCFILE.SPC_RBYRTYPE_7_2,
                   SPCFILE.SPC_RBYRTYPE_8_1,
                   SPCFILE.SPC_RBYRTYPE_8_2,
                   SPCFILE.SPC_RBYRTYPE_9_1,
                   SPCFILE.SPC_RBYRTYPE_9_2,
                   SPCFILE.SPC_RBYRTYPE_10_1,
                   SPCFILE.SPC_RBYRTYPE_10_2,
                   SPCFILE.SPC_RBYRTYPE_11_1,
                   SPCFILE.SPC_RBYRTYPE_11_2,
                   SPCFILE.SPC_RBYRTYPE_12_1,
                   SPCFILE.SPC_RBYRTYPE_12_2,
                   SPCFILE.SPC_RBYRTYPE_13_1,
                   SPCFILE.SPC_RBYRTYPE_13_2,
                   SPCFILE.SPC_RBYRTYPE_14_1,
                   SPCFILE.SPC_RBYRTYPE_14_2,
                   SPCFILE.SPC_RBYRTYPE_15_1,
                   SPCFILE.SPC_RBYRTYPE_15_2,
                   SPCFILE.SPC_RBYRTYPE_16_1,
                   SPCFILE.SPC_RBYRTYPE_16_2,
                   SPCFILE.SPC_RBYRTYPE_17_1,
                   SPCFILE.SPC_RBYRTYPE_17_2,
                   SPCFILE.SPC_RBYRTYPE_18_1,
                   SPCFILE.SPC_RBYRTYPE_18_2,
                   SPCFILE.SPC_RBYRTYPE_19_1,
                   SPCFILE.SPC_RBYRTYPE_19_2,
                   SPCFILE.SPC_RBYRTYPE_20_1,
                   SPCFILE.SPC_RBYRTYPE_20_2,
                   SPCFILE.SPC_RBYRTYPE_21_1,
                   SPCFILE.SPC_RBYRTYPE_21_2,
                   SPCFILE.SPC_RBYRTYPE_22_1,
                   SPCFILE.SPC_RBYRTYPE_22_2,
                   SPCFILE.SPC_RBMINRET_1,
                   SPCFILE.SPC_RBMINRET_2,
                   SPCFILE.SPC_RBMINRET_3,
                   SPCFILE.SPC_RBMINRET_4,
                   SPCFILE.SPC_RBMINRET_5,
                   SPCFILE.SPC_RBMINRET_6,
                   SPCFILE.SPC_RBMINRET_7,
                   SPCFILE.SPC_RBMINRET_8,
                   SPCFILE.SPC_RBMINRET_9,
                   SPCFILE.SPC_RBMINRET_10,
                   SPCFILE.SPC_RBMINRET_11,
                   SPCFILE.SPC_RBMINRET_12,
                   SPCFILE.SPC_RBMINRET_13,
                   SPCFILE.SPC_RBMINRET_14,
                   SPCFILE.SPC_RBMINRET_15,
                   SPCFILE.SPC_RBMINRET_16,
                   SPCFILE.SPC_RBMINRET_17,
                   SPCFILE.SPC_RBMINRET_18,
                   SPCFILE.SPC_RBMINRET_19,
                   SPCFILE.SPC_RBMINRET_20,
                   SPCFILE.SPC_RBMINRET_21,
                   SPCFILE.SPC_RBMINRET_22,
                   SPCFILE.SPC_RBNOMON_1,
                   SPCFILE.SPC_RBNOMON_2,
                   SPCFILE.SPC_RBNOMON_3,
                   SPCFILE.SPC_RBNOMON_4,
                   SPCFILE.SPC_RBNOMON_5,
                   SPCFILE.SPC_RBNOMON_6,
                   SPCFILE.SPC_RBNOMON_7,
                   SPCFILE.SPC_RBNOMON_8,
                   SPCFILE.SPC_RBNOMON_9,
                   SPCFILE.SPC_RBNOMON_10,
                   SPCFILE.SPC_RBNOMON_11,
                   SPCFILE.SPC_RBNOMON_12,
                   SPCFILE.SPC_RBNOMON_13,
                   SPCFILE.SPC_RBNOMON_14,
                   SPCFILE.SPC_RBNOMON_15,
                   SPCFILE.SPC_RBNOMON_16,
                   SPCFILE.SPC_RBNOMON_17,
                   SPCFILE.SPC_RBNOMON_18,
                   SPCFILE.SPC_RBNOMON_19,
                   SPCFILE.SPC_RBNOMON_20,
                   SPCFILE.SPC_RBNOMON_21,
                   SPCFILE.SPC_RBNOMON_22,
                   SPCFILE.SPC_RBDEDCHG_1,
                   SPCFILE.SPC_RBDEDCHG_2,
                   SPCFILE.SPC_RBDEDCHG_3,
                   SPCFILE.SPC_RBDEDCHG_4,
                   SPCFILE.SPC_RBDEDCHG_5,
                   SPCFILE.SPC_RBDEDCHG_6,
                   SPCFILE.SPC_RBDEDCHG_7,
                   SPCFILE.SPC_RBDEDCHG_8,
                   SPCFILE.SPC_RBDEDCHG_9,
                   SPCFILE.SPC_RBDEDCHG_10,
                   SPCFILE.SPC_RBDEDCHG_11,
                   SPCFILE.SPC_RBDEDCHG_12,
                   SPCFILE.SPC_RBDEDCHG_13,
                   SPCFILE.SPC_RBDEDCHG_14,
                   SPCFILE.SPC_RBDEDCHG_15,
                   SPCFILE.SPC_RBDEDCHG_16,
                   SPCFILE.SPC_RBDEDCHG_17,
                   SPCFILE.SPC_RBDEDCHG_18,
                   SPCFILE.SPC_RBDEDCHG_19,
                   SPCFILE.SPC_RBDEDCHG_20,
                   SPCFILE.SPC_RBDEDCHG_21,
                   SPCFILE.SPC_RBDEDCHG_22,
                   SPCFILE.SPC_RBRECAST_1,
                   SPCFILE.SPC_RBRECAST_2,
                   SPCFILE.SPC_RBRECAST_3,
                   SPCFILE.SPC_RBRECAST_4,
                   SPCFILE.SPC_RBRECAST_5,
                   SPCFILE.SPC_RBRECAST_6,
                   SPCFILE.SPC_RBRECAST_7,
                   SPCFILE.SPC_RBRECAST_8,
                   SPCFILE.SPC_RBRECAST_9,
                   SPCFILE.SPC_RBRECAST_10,
                   SPCFILE.SPC_RBRECAST_11,
                   SPCFILE.SPC_RBRECAST_12,
                   SPCFILE.SPC_RBRECAST_13,
                   SPCFILE.SPC_RBRECAST_14,
                   SPCFILE.SPC_RBRECAST_15,
                   SPCFILE.SPC_RBRECAST_16,
                   SPCFILE.SPC_RBRECAST_17,
                   SPCFILE.SPC_RBRECAST_18,
                   SPCFILE.SPC_RBRECAST_19,
                   SPCFILE.SPC_RBRECAST_20,
                   SPCFILE.SPC_RBRECAST_21,
                   SPCFILE.SPC_RBRECAST_22,
                   SPCFILE.SPC_RBEARLY_1,
                   SPCFILE.SPC_RBEARLY_2,
                   SPCFILE.SPC_RBEARLY_3,
                   SPCFILE.SPC_RBEARLY_4,
                   SPCFILE.SPC_RBEARLY_5,
                   SPCFILE.SPC_RBEARLY_6,
                   SPCFILE.SPC_RBEARLY_7,
                   SPCFILE.SPC_RBEARLY_8,
                   SPCFILE.SPC_RBEARLY_9,
                   SPCFILE.SPC_RBEARLY_10,
                   SPCFILE.SPC_RBEARLY_11,
                   SPCFILE.SPC_RBEARLY_12,
                   SPCFILE.SPC_RBEARLY_13,
                   SPCFILE.SPC_RBEARLY_14,
                   SPCFILE.SPC_RBEARLY_15,
                   SPCFILE.SPC_RBEARLY_16,
                   SPCFILE.SPC_RBEARLY_17,
                   SPCFILE.SPC_RBEARLY_18,
                   SPCFILE.SPC_RBEARLY_19,
                   SPCFILE.SPC_RBEARLY_20,
                   SPCFILE.SPC_RBEARLY_21,
                   SPCFILE.SPC_RBEARLY_22,
                   SPCFILE.SPC_RBDEFER_1,
                   SPCFILE.SPC_RBDEFER_2,
                   SPCFILE.SPC_RBDEFER_3,
                   SPCFILE.SPC_RBDEFER_4,
                   SPCFILE.SPC_RBDEFER_5,
                   SPCFILE.SPC_RBDEFER_6,
                   SPCFILE.SPC_RBDEFER_7,
                   SPCFILE.SPC_RBDEFER_8,
                   SPCFILE.SPC_RBDEFER_9,
                   SPCFILE.SPC_RBDEFER_10,
                   SPCFILE.SPC_RBDEFER_11,
                   SPCFILE.SPC_RBDEFER_12,
                   SPCFILE.SPC_RBDEFER_13,
                   SPCFILE.SPC_RBDEFER_14,
                   SPCFILE.SPC_RBDEFER_15,
                   SPCFILE.SPC_RBDEFER_16,
                   SPCFILE.SPC_RBDEFER_17,
                   SPCFILE.SPC_RBDEFER_18,
                   SPCFILE.SPC_RBDEFER_19,
                   SPCFILE.SPC_RBDEFER_20,
                   SPCFILE.SPC_RBDEFER_21,
                   SPCFILE.SPC_RBDEFER_22,
                   SPCFILE.SPC_RBPCT_1,
                   SPCFILE.SPC_RBPCT_2,
                   SPCFILE.SPC_RBPCT_3,
                   SPCFILE.SPC_RBPCT_4,
                   SPCFILE.SPC_RBPCT_5,
                   SPCFILE.SPC_RBPCT_6,
                   SPCFILE.SPC_RBPCT_7,
                   SPCFILE.SPC_RBPCT_8,
                   SPCFILE.SPC_RBPCT_9,
                   SPCFILE.SPC_RBPCT_10,
                   SPCFILE.SPC_RBPCT_11,
                   SPCFILE.SPC_RBPCT_12,
                   SPCFILE.SPC_RBPCT_13,
                   SPCFILE.SPC_RBPCT_14,
                   SPCFILE.SPC_RBPCT_15,
                   SPCFILE.SPC_RBPCT_16,
                   SPCFILE.SPC_RBPCT_17,
                   SPCFILE.SPC_RBPCT_18,
                   SPCFILE.SPC_RBPCT_19,
                   SPCFILE.SPC_RBPCT_20,
                   SPCFILE.SPC_RBPCT_21,
                   SPCFILE.SPC_RBPCT_22,
                   SPCFILE.SPC_RBREDUC_1,
                   SPCFILE.SPC_RBREDUC_2,
                   SPCFILE.SPC_RBREDUC_3,
                   SPCFILE.SPC_RBREDUC_4,
                   SPCFILE.SPC_RBREDUC_5,
                   SPCFILE.SPC_RBREDUC_6,
                   SPCFILE.SPC_RBREDUC_7,
                   SPCFILE.SPC_RBREDUC_8,
                   SPCFILE.SPC_RBREDUC_9,
                   SPCFILE.SPC_RBREDUC_10,
                   SPCFILE.SPC_RBREDUC_11,
                   SPCFILE.SPC_RBREDUC_12,
                   SPCFILE.SPC_RBREDUC_13,
                   SPCFILE.SPC_RBREDUC_14,
                   SPCFILE.SPC_RBREDUC_15,
                   SPCFILE.SPC_RBREDUC_16,
                   SPCFILE.SPC_RBREDUC_17,
                   SPCFILE.SPC_RBREDUC_18,
                   SPCFILE.SPC_RBREDUC_19,
                   SPCFILE.SPC_RBREDUC_20,
                   SPCFILE.SPC_RBREDUC_21,
                   SPCFILE.SPC_RBREDUC_22,
                   SPCFILE.SPC_RBMIN_1,
                   SPCFILE.SPC_RBMIN_2,
                   SPCFILE.SPC_RBMIN_3,
                   SPCFILE.SPC_RBMIN_4,
                   SPCFILE.SPC_RBMIN_5,
                   SPCFILE.SPC_RBMIN_6,
                   SPCFILE.SPC_RBMIN_7,
                   SPCFILE.SPC_RBMIN_8,
                   SPCFILE.SPC_RBMIN_9,
                   SPCFILE.SPC_RBMIN_10,
                   SPCFILE.SPC_RBMIN_11,
                   SPCFILE.SPC_RBMIN_12,
                   SPCFILE.SPC_RBMIN_13,
                   SPCFILE.SPC_RBMIN_14,
                   SPCFILE.SPC_RBMIN_15,
                   SPCFILE.SPC_RBMIN_16,
                   SPCFILE.SPC_RBMIN_17,
                   SPCFILE.SPC_RBMIN_18,
                   SPCFILE.SPC_RBMIN_19,
                   SPCFILE.SPC_RBMIN_20,
                   SPCFILE.SPC_RBMIN_21,
                   SPCFILE.SPC_RBMIN_22,
                   SPCFILE.SPC_RBSPOPT1_1,
                   SPCFILE.SPC_RBSPOPT1_2,
                   SPCFILE.SPC_RBSPOPT1_3,
                   SPCFILE.SPC_RBSPOPT1_4,
                   SPCFILE.SPC_RBSPOPT1_5,
                   SPCFILE.SPC_RBSPOPT1_6,
                   SPCFILE.SPC_RBSPOPT1_7,
                   SPCFILE.SPC_RBSPOPT1_8,
                   SPCFILE.SPC_RBSPOPT1_9,
                   SPCFILE.SPC_RBSPOPT1_10,
                   SPCFILE.SPC_RBSPOPT1_11,
                   SPCFILE.SPC_RBSPOPT1_12,
                   SPCFILE.SPC_RBSPOPT1_13,
                   SPCFILE.SPC_RBSPOPT1_14,
                   SPCFILE.SPC_RBSPOPT1_15,
                   SPCFILE.SPC_RBSPOPT1_16,
                   SPCFILE.SPC_RBSPOPT1_17,
                   SPCFILE.SPC_RBSPOPT1_18,
                   SPCFILE.SPC_RBSPOPT1_19,
                   SPCFILE.SPC_RBSPOPT1_20,
                   SPCFILE.SPC_RBSPOPT1_21,
                   SPCFILE.SPC_RBSPOPT1_22,
                   SPCFILE.SPC_RBSPOPT2_1,
                   SPCFILE.SPC_RBSPOPT2_2,
                   SPCFILE.SPC_RBSPOPT2_3,
                   SPCFILE.SPC_RBSPOPT2_4,
                   SPCFILE.SPC_RBSPOPT2_5,
                   SPCFILE.SPC_RBSPOPT2_6,
                   SPCFILE.SPC_RBSPOPT2_7,
                   SPCFILE.SPC_RBSPOPT2_8,
                   SPCFILE.SPC_RBSPOPT2_9,
                   SPCFILE.SPC_RBSPOPT2_10,
                   SPCFILE.SPC_RBSPOPT2_11,
                   SPCFILE.SPC_RBSPOPT2_12,
                   SPCFILE.SPC_RBSPOPT2_13,
                   SPCFILE.SPC_RBSPOPT2_14,
                   SPCFILE.SPC_RBSPOPT2_15,
                   SPCFILE.SPC_RBSPOPT2_16,
                   SPCFILE.SPC_RBSPOPT2_17,
                   SPCFILE.SPC_RBSPOPT2_18,
                   SPCFILE.SPC_RBSPOPT2_19,
                   SPCFILE.SPC_RBSPOPT2_20,
                   SPCFILE.SPC_RBSPOPT2_21,
                   SPCFILE.SPC_RBSPOPT2_22,
                   SPCFILE.SPC_RBMANAGER_1,
                   SPCFILE.SPC_RBMANAGER_2,
                   SPCFILE.SPC_RBMANAGER_3,
                   SPCFILE.SPC_RBMANAGER_4,
                   SPCFILE.SPC_RBMANAGER_5,
                   SPCFILE.SPC_RBMANAGER_6,
                   SPCFILE.SPC_RBMANAGER_7,
                   SPCFILE.SPC_RBMANAGER_8,
                   SPCFILE.SPC_RBMANAGER_9,
                   SPCFILE.SPC_RBMANAGER_10,
                   SPCFILE.SPC_RBMANAGER_11,
                   SPCFILE.SPC_RBMANAGER_12,
                   SPCFILE.SPC_RBMANAGER_13,
                   SPCFILE.SPC_RBMANAGER_14,
                   SPCFILE.SPC_RBMANAGER_15,
                   SPCFILE.SPC_RBMANAGER_16,
                   SPCFILE.SPC_RBMANAGER_17,
                   SPCFILE.SPC_RBMANAGER_18,
                   SPCFILE.SPC_RBMANAGER_19,
                   SPCFILE.SPC_RBMANAGER_20,
                   SPCFILE.SPC_RBMANAGER_21,
                   SPCFILE.SPC_RBMANAGER_22,
                   SPCFILE.SPC_RBUNITPER_CD_1,
                   SPCFILE.SPC_RBUNITPER_CD_2,
                   SPCFILE.SPC_RBUNITPER_CD_3,
                   SPCFILE.SPC_RBUNITPER_CD_4,
                   SPCFILE.SPC_RBUNITPER_CD_5,
                   SPCFILE.SPC_RBUNITPER_CD_6,
                   SPCFILE.SPC_RBUNITPER_CD_7,
                   SPCFILE.SPC_RBUNITPER_CD_8,
                   SPCFILE.SPC_RBUNITPER_CD_9,
                   SPCFILE.SPC_RBUNITPER_CD_10,
                   SPCFILE.SPC_RBUNITPER_CD_11,
                   SPCFILE.SPC_RBUNITPER_CD_12,
                   SPCFILE.SPC_RBUNITPER_CD_13,
                   SPCFILE.SPC_RBUNITPER_CD_14,
                   SPCFILE.SPC_RBUNITPER_CD_15,
                   SPCFILE.SPC_RBUNITPER_CD_16,
                   SPCFILE.SPC_RBUNITPER_CD_17,
                   SPCFILE.SPC_RBUNITPER_CD_18,
                   SPCFILE.SPC_RBUNITPER_CD_19,
                   SPCFILE.SPC_RBUNITPER_CD_20,
                   SPCFILE.SPC_RBUNITPER_CD_21,
                   SPCFILE.SPC_RBUNITPER_CD_22,
                   SPCFILE.SPC_RBFRMLA2_1,
                   SPCFILE.SPC_RBFRMLA2_2,
                   SPCFILE.SPC_RBFRMLA2_3,
                   SPCFILE.SPC_RBFRMLA2_4,
                   SPCFILE.SPC_RBFRMLA2_5,
                   SPCFILE.SPC_RBFRMLA2_6,
                   SPCFILE.SPC_RBFRMLA2_7,
                   SPCFILE.SPC_RBFRMLA2_8,
                   SPCFILE.SPC_RBFRMLA2_9,
                   SPCFILE.SPC_RBFRMLA2_10,
                   SPCFILE.SPC_RBFRMLA2_11,
                   SPCFILE.SPC_RBFRMLA2_12,
                   SPCFILE.SPC_RBFRMLA2_13,
                   SPCFILE.SPC_RBFRMLA2_14,
                   SPCFILE.SPC_RBFRMLA2_15,
                   SPCFILE.SPC_RBFRMLA2_16,
                   SPCFILE.SPC_RBFRMLA2_17,
                   SPCFILE.SPC_RBFRMLA2_18,
                   SPCFILE.SPC_RBFRMLA2_19,
                   SPCFILE.SPC_RBFRMLA2_20,
                   SPCFILE.SPC_RBFRMLA2_21,
                   SPCFILE.SPC_RBFRMLA2_22,
                   SPCFILE.SPC_CONTR_CREDIT,
                   SPCFILE.SPC_CONTR_CREDIT_CD,
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   SPCFILE.SPC_OPT_CAL_CHGABLE,
                   SPCFILE.SPC_CPOT30_CD,
                   SPCFILE.SPC_CPOTCHGOFF_CD,
                   SPCFILE.SPC_RDPOTCHGOFF_CD,
                   SPCFILE.SPC_CAL_MAX_AT_ESTIMATE,
                   SPCFILE.SPC_DELQ_IB_AGE,
                   SPCFILE.SPC_DELQ_IB_MAX_DAYS,
                   SPCFILE.SPC_BANKRULE_RATE,
                   SPCFILE.SPC_LC_INT_ON_REPO_NON_PL
            INTO 
                  :QSPC-ORGST,
                  :QSPC-SPRCLASS,
                  :QSPC-SUBCLASS,
                  :QSPC-LAWCODE,
                  :QSPC-RBFRMLA-1, 
                  :QSPC-RBFRMLA-2, 
                  :QSPC-RBFRMLA-3, 
                  :QSPC-RBFRMLA-4, 
                  :QSPC-RBFRMLA-5, 
                  :QSPC-RBFRMLA-6, 
                  :QSPC-RBFRMLA-7, 
                  :QSPC-RBFRMLA-8, 
                  :QSPC-RBFRMLA-9, 
                  :QSPC-RBFRMLA-10, 
                  :QSPC-RBFRMLA-11, 
                  :QSPC-RBFRMLA-12, 
                  :QSPC-RBFRMLA-13, 
                  :QSPC-RBFRMLA-14, 
                  :QSPC-RBFRMLA-15, 
                  :QSPC-RBFRMLA-16, 
                  :QSPC-RBFRMLA-17, 
                  :QSPC-RBFRMLA-18, 
                  :QSPC-RBFRMLA-19, 
                  :QSPC-RBFRMLA-20, 
                  :QSPC-RBFRMLA-21, 
                  :QSPC-RBFRMLA-22, 
                  :QSPC-RBDTE-1-1, 
                  :QSPC-RBDTE-1-2, 
                  :QSPC-RBDTE-2-1, 
                  :QSPC-RBDTE-2-2, 
                  :QSPC-RBDTE-3-1, 
                  :QSPC-RBDTE-3-2, 
                  :QSPC-RBDTE-4-1, 
                  :QSPC-RBDTE-4-2, 
                  :QSPC-RBDTE-5-1, 
                  :QSPC-RBDTE-5-2, 
                  :QSPC-RBDTE-6-1, 
                  :QSPC-RBDTE-6-2, 
                  :QSPC-RBDTE-7-1, 
                  :QSPC-RBDTE-7-2, 
                  :QSPC-RBDTE-8-1, 
                  :QSPC-RBDTE-8-2, 
                  :QSPC-RBDTE-9-1, 
                  :QSPC-RBDTE-9-2, 
                  :QSPC-RBDTE-10-1, 
                  :QSPC-RBDTE-10-2, 
                  :QSPC-RBDTE-11-1, 
                  :QSPC-RBDTE-11-2, 
                  :QSPC-RBDTE-12-1, 
                  :QSPC-RBDTE-12-2, 
                  :QSPC-RBDTE-13-1, 
                  :QSPC-RBDTE-13-2, 
                  :QSPC-RBDTE-14-1, 
                  :QSPC-RBDTE-14-2, 
                  :QSPC-RBDTE-15-1, 
                  :QSPC-RBDTE-15-2, 
                  :QSPC-RBDTE-16-1, 
                  :QSPC-RBDTE-16-2, 
                  :QSPC-RBDTE-17-1, 
                  :QSPC-RBDTE-17-2, 
                  :QSPC-RBDTE-18-1, 
                  :QSPC-RBDTE-18-2, 
                  :QSPC-RBDTE-19-1, 
                  :QSPC-RBDTE-19-2, 
                  :QSPC-RBDTE-20-1, 
                  :QSPC-RBDTE-20-2, 
                  :QSPC-RBDTE-21-1, 
                  :QSPC-RBDTE-21-2, 
                  :QSPC-RBDTE-22-1, 
                  :QSPC-RBDTE-22-2, 
                  :QSPC-RBDAYS-1-1,
                  :QSPC-RBDAYS-1-2,
                  :QSPC-RBDAYS-2-1,
                  :QSPC-RBDAYS-2-2,
                  :QSPC-RBDAYS-3-1,
                  :QSPC-RBDAYS-3-2,
                  :QSPC-RBDAYS-4-1,
                  :QSPC-RBDAYS-4-2,
                  :QSPC-RBDAYS-5-1,
                  :QSPC-RBDAYS-5-2,
                  :QSPC-RBDAYS-6-1,
                  :QSPC-RBDAYS-6-2,
                  :QSPC-RBDAYS-7-1,
                  :QSPC-RBDAYS-7-2,
                  :QSPC-RBDAYS-8-1,
                  :QSPC-RBDAYS-8-2,
                  :QSPC-RBDAYS-9-1,
                  :QSPC-RBDAYS-9-2,
                  :QSPC-RBDAYS-10-1,
                  :QSPC-RBDAYS-10-2,
                  :QSPC-RBDAYS-11-1,
                  :QSPC-RBDAYS-11-2,
                  :QSPC-RBDAYS-12-1,
                  :QSPC-RBDAYS-12-2,
                  :QSPC-RBDAYS-13-1,
                  :QSPC-RBDAYS-13-2,
                  :QSPC-RBDAYS-14-1,
                  :QSPC-RBDAYS-14-2,
                  :QSPC-RBDAYS-15-1,
                  :QSPC-RBDAYS-15-2,
                  :QSPC-RBDAYS-16-1,
                  :QSPC-RBDAYS-16-2,
                  :QSPC-RBDAYS-17-1,
                  :QSPC-RBDAYS-17-2,
                  :QSPC-RBDAYS-18-1,
                  :QSPC-RBDAYS-18-2,
                  :QSPC-RBDAYS-19-1,
                  :QSPC-RBDAYS-19-2,
                  :QSPC-RBDAYS-20-1,
                  :QSPC-RBDAYS-20-2,
                  :QSPC-RBDAYS-21-1,
                  :QSPC-RBDAYS-21-2,
                  :QSPC-RBDAYS-22-1,
                  :QSPC-RBDAYS-22-2,
                  :QSPC-RBYRTYPE-1-1,
                  :QSPC-RBYRTYPE-1-2,
                  :QSPC-RBYRTYPE-2-1,
                  :QSPC-RBYRTYPE-2-2,
                  :QSPC-RBYRTYPE-3-1,
                  :QSPC-RBYRTYPE-3-2,
                  :QSPC-RBYRTYPE-4-1,
                  :QSPC-RBYRTYPE-4-2,
                  :QSPC-RBYRTYPE-5-1,
                  :QSPC-RBYRTYPE-5-2,
                  :QSPC-RBYRTYPE-6-1,
                  :QSPC-RBYRTYPE-6-2,
                  :QSPC-RBYRTYPE-7-1,
                  :QSPC-RBYRTYPE-7-2,
                  :QSPC-RBYRTYPE-8-1,
                  :QSPC-RBYRTYPE-8-2,
                  :QSPC-RBYRTYPE-9-1,
                  :QSPC-RBYRTYPE-9-2,
                  :QSPC-RBYRTYPE-10-1,
                  :QSPC-RBYRTYPE-10-2,
                  :QSPC-RBYRTYPE-11-1,
                  :QSPC-RBYRTYPE-11-2,
                  :QSPC-RBYRTYPE-12-1,
                  :QSPC-RBYRTYPE-12-2,
                  :QSPC-RBYRTYPE-13-1,
                  :QSPC-RBYRTYPE-13-2,
                  :QSPC-RBYRTYPE-14-1,
                  :QSPC-RBYRTYPE-14-2,
                  :QSPC-RBYRTYPE-15-1,
                  :QSPC-RBYRTYPE-15-2,
                  :QSPC-RBYRTYPE-16-1,
                  :QSPC-RBYRTYPE-16-2,
                  :QSPC-RBYRTYPE-17-1,
                  :QSPC-RBYRTYPE-17-2,
                  :QSPC-RBYRTYPE-18-1,
                  :QSPC-RBYRTYPE-18-2,
                  :QSPC-RBYRTYPE-19-1,
                  :QSPC-RBYRTYPE-19-2,
                  :QSPC-RBYRTYPE-20-1,
                  :QSPC-RBYRTYPE-20-2,
                  :QSPC-RBYRTYPE-21-1,
                  :QSPC-RBYRTYPE-21-2,
                  :QSPC-RBYRTYPE-22-1,
                  :QSPC-RBYRTYPE-22-2,
                  :QSPC-RBMINRET-1,
                  :QSPC-RBMINRET-2,
                  :QSPC-RBMINRET-3,
                  :QSPC-RBMINRET-4,
                  :QSPC-RBMINRET-5,
                  :QSPC-RBMINRET-6,
                  :QSPC-RBMINRET-7,
                  :QSPC-RBMINRET-8,
                  :QSPC-RBMINRET-9,
                  :QSPC-RBMINRET-10,
                  :QSPC-RBMINRET-11,
                  :QSPC-RBMINRET-12,
                  :QSPC-RBMINRET-13,
                  :QSPC-RBMINRET-14,
                  :QSPC-RBMINRET-15,
                  :QSPC-RBMINRET-16,
                  :QSPC-RBMINRET-17,
                  :QSPC-RBMINRET-18,
                  :QSPC-RBMINRET-19,
                  :QSPC-RBMINRET-20,
                  :QSPC-RBMINRET-21,
                  :QSPC-RBMINRET-22,
                  :QSPC-RBNOMON-1,
                  :QSPC-RBNOMON-2,
                  :QSPC-RBNOMON-3,
                  :QSPC-RBNOMON-4,
                  :QSPC-RBNOMON-5,
                  :QSPC-RBNOMON-6,
                  :QSPC-RBNOMON-7,
                  :QSPC-RBNOMON-8,
                  :QSPC-RBNOMON-9,
                  :QSPC-RBNOMON-10,
                  :QSPC-RBNOMON-11,
                  :QSPC-RBNOMON-12,
                  :QSPC-RBNOMON-13,
                  :QSPC-RBNOMON-14,
                  :QSPC-RBNOMON-15,
                  :QSPC-RBNOMON-16,
                  :QSPC-RBNOMON-17,
                  :QSPC-RBNOMON-18,
                  :QSPC-RBNOMON-19,
                  :QSPC-RBNOMON-20,
                  :QSPC-RBNOMON-21,
                  :QSPC-RBNOMON-22,
                  :QSPC-RBDEDCHG-1,
                  :QSPC-RBDEDCHG-2,
                  :QSPC-RBDEDCHG-3,
                  :QSPC-RBDEDCHG-4,
                  :QSPC-RBDEDCHG-5,
                  :QSPC-RBDEDCHG-6,
                  :QSPC-RBDEDCHG-7,
                  :QSPC-RBDEDCHG-8,
                  :QSPC-RBDEDCHG-9,
                  :QSPC-RBDEDCHG-10,
                  :QSPC-RBDEDCHG-11,
                  :QSPC-RBDEDCHG-12,
                  :QSPC-RBDEDCHG-13,
                  :QSPC-RBDEDCHG-14,
                  :QSPC-RBDEDCHG-15,
                  :QSPC-RBDEDCHG-16,
                  :QSPC-RBDEDCHG-17,
                  :QSPC-RBDEDCHG-18,
                  :QSPC-RBDEDCHG-19,
                  :QSPC-RBDEDCHG-20,
                  :QSPC-RBDEDCHG-21,
                  :QSPC-RBDEDCHG-22,
                  :QSPC-RBRECAST-1,
                  :QSPC-RBRECAST-2,
                  :QSPC-RBRECAST-3,
                  :QSPC-RBRECAST-4,
                  :QSPC-RBRECAST-5,
                  :QSPC-RBRECAST-6,
                  :QSPC-RBRECAST-7,
                  :QSPC-RBRECAST-8,
                  :QSPC-RBRECAST-9,
                  :QSPC-RBRECAST-10,
                  :QSPC-RBRECAST-11,
                  :QSPC-RBRECAST-12,
                  :QSPC-RBRECAST-13,
                  :QSPC-RBRECAST-14,
                  :QSPC-RBRECAST-15,
                  :QSPC-RBRECAST-16,
                  :QSPC-RBRECAST-17,
                  :QSPC-RBRECAST-18,
                  :QSPC-RBRECAST-19,
                  :QSPC-RBRECAST-20,
                  :QSPC-RBRECAST-21,
                  :QSPC-RBRECAST-22,
                  :QSPC-RBEARLY-1,
                  :QSPC-RBEARLY-2,
                  :QSPC-RBEARLY-3,
                  :QSPC-RBEARLY-4,
                  :QSPC-RBEARLY-5,
                  :QSPC-RBEARLY-6,
                  :QSPC-RBEARLY-7,
                  :QSPC-RBEARLY-8,
                  :QSPC-RBEARLY-9,
                  :QSPC-RBEARLY-10,
                  :QSPC-RBEARLY-11,
                  :QSPC-RBEARLY-12,
                  :QSPC-RBEARLY-13,
                  :QSPC-RBEARLY-14,
                  :QSPC-RBEARLY-15,
                  :QSPC-RBEARLY-16,
                  :QSPC-RBEARLY-17,
                  :QSPC-RBEARLY-18,
                  :QSPC-RBEARLY-19,
                  :QSPC-RBEARLY-20,
                  :QSPC-RBEARLY-21,
                  :QSPC-RBEARLY-22,
                  :QSPC-RBDEFER-1,
                  :QSPC-RBDEFER-2,
                  :QSPC-RBDEFER-3,
                  :QSPC-RBDEFER-4,
                  :QSPC-RBDEFER-5,
                  :QSPC-RBDEFER-6,
                  :QSPC-RBDEFER-7,
                  :QSPC-RBDEFER-8,
                  :QSPC-RBDEFER-9,
                  :QSPC-RBDEFER-10,
                  :QSPC-RBDEFER-11,
                  :QSPC-RBDEFER-12,
                  :QSPC-RBDEFER-13,
                  :QSPC-RBDEFER-14,
                  :QSPC-RBDEFER-15,
                  :QSPC-RBDEFER-16,
                  :QSPC-RBDEFER-17,
                  :QSPC-RBDEFER-18,
                  :QSPC-RBDEFER-19,
                  :QSPC-RBDEFER-20,
                  :QSPC-RBDEFER-21,
                  :QSPC-RBDEFER-22,
                  :QSPC-RBPCT-1,
                  :QSPC-RBPCT-2,
                  :QSPC-RBPCT-3,
                  :QSPC-RBPCT-4,
                  :QSPC-RBPCT-5,
                  :QSPC-RBPCT-6,
                  :QSPC-RBPCT-7,
                  :QSPC-RBPCT-8,
                  :QSPC-RBPCT-9,
                  :QSPC-RBPCT-10,
                  :QSPC-RBPCT-11,
                  :QSPC-RBPCT-12,
                  :QSPC-RBPCT-13,
                  :QSPC-RBPCT-14,
                  :QSPC-RBPCT-15,
                  :QSPC-RBPCT-16,
                  :QSPC-RBPCT-17,
                  :QSPC-RBPCT-18,
                  :QSPC-RBPCT-19,
                  :QSPC-RBPCT-20,
                  :QSPC-RBPCT-21,
                  :QSPC-RBPCT-22,
                  :QSPC-RBREDUC-1,
                  :QSPC-RBREDUC-2,
                  :QSPC-RBREDUC-3,
                  :QSPC-RBREDUC-4,
                  :QSPC-RBREDUC-5,
                  :QSPC-RBREDUC-6,
                  :QSPC-RBREDUC-7,
                  :QSPC-RBREDUC-8,
                  :QSPC-RBREDUC-9,
                  :QSPC-RBREDUC-10,
                  :QSPC-RBREDUC-11,
                  :QSPC-RBREDUC-12,
                  :QSPC-RBREDUC-13,
                  :QSPC-RBREDUC-14,
                  :QSPC-RBREDUC-15,
                  :QSPC-RBREDUC-16,
                  :QSPC-RBREDUC-17,
                  :QSPC-RBREDUC-18,
                  :QSPC-RBREDUC-19,
                  :QSPC-RBREDUC-20,
                  :QSPC-RBREDUC-21,
                  :QSPC-RBREDUC-22,
                  :QSPC-RBMIN-1,
                  :QSPC-RBMIN-2,
                  :QSPC-RBMIN-3,
                  :QSPC-RBMIN-4,
                  :QSPC-RBMIN-5,
                  :QSPC-RBMIN-6,
                  :QSPC-RBMIN-7,
                  :QSPC-RBMIN-8,
                  :QSPC-RBMIN-9,
                  :QSPC-RBMIN-10,
                  :QSPC-RBMIN-11,
                  :QSPC-RBMIN-12,
                  :QSPC-RBMIN-13,
                  :QSPC-RBMIN-14,
                  :QSPC-RBMIN-15,
                  :QSPC-RBMIN-16,
                  :QSPC-RBMIN-17,
                  :QSPC-RBMIN-18,
                  :QSPC-RBMIN-19,
                  :QSPC-RBMIN-20,
                  :QSPC-RBMIN-21,
                  :QSPC-RBMIN-22,
                  :QSPC-RBSPOPT1-1,
                  :QSPC-RBSPOPT1-2,
                  :QSPC-RBSPOPT1-3,
                  :QSPC-RBSPOPT1-4,
                  :QSPC-RBSPOPT1-5,
                  :QSPC-RBSPOPT1-6,
                  :QSPC-RBSPOPT1-7,
                  :QSPC-RBSPOPT1-8,
                  :QSPC-RBSPOPT1-9,
                  :QSPC-RBSPOPT1-10,
                  :QSPC-RBSPOPT1-11,
                  :QSPC-RBSPOPT1-12,
                  :QSPC-RBSPOPT1-13,
                  :QSPC-RBSPOPT1-14,
                  :QSPC-RBSPOPT1-15,
                  :QSPC-RBSPOPT1-16,
                  :QSPC-RBSPOPT1-17,
                  :QSPC-RBSPOPT1-18,
                  :QSPC-RBSPOPT1-19,
                  :QSPC-RBSPOPT1-20,
                  :QSPC-RBSPOPT1-21,
                  :QSPC-RBSPOPT1-22,
                  :QSPC-RBSPOPT2-1,
                  :QSPC-RBSPOPT2-2,
                  :QSPC-RBSPOPT2-3,
                  :QSPC-RBSPOPT2-4,
                  :QSPC-RBSPOPT2-5,
                  :QSPC-RBSPOPT2-6,
                  :QSPC-RBSPOPT2-7,
                  :QSPC-RBSPOPT2-8,
                  :QSPC-RBSPOPT2-9,
                  :QSPC-RBSPOPT2-10,
                  :QSPC-RBSPOPT2-11,
                  :QSPC-RBSPOPT2-12,
                  :QSPC-RBSPOPT2-13,
                  :QSPC-RBSPOPT2-14,
                  :QSPC-RBSPOPT2-15,
                  :QSPC-RBSPOPT2-16,
                  :QSPC-RBSPOPT2-17,
                  :QSPC-RBSPOPT2-18,
                  :QSPC-RBSPOPT2-19,
                  :QSPC-RBSPOPT2-20,
                  :QSPC-RBSPOPT2-21,
                  :QSPC-RBSPOPT2-22,
                  :QSPC-RBMANAGER-1,
                  :QSPC-RBMANAGER-2,
                  :QSPC-RBMANAGER-3,
                  :QSPC-RBMANAGER-4,
                  :QSPC-RBMANAGER-5,
                  :QSPC-RBMANAGER-6,
                  :QSPC-RBMANAGER-7,
                  :QSPC-RBMANAGER-8,
                  :QSPC-RBMANAGER-9,
                  :QSPC-RBMANAGER-10,
                  :QSPC-RBMANAGER-11,
                  :QSPC-RBMANAGER-12,
                  :QSPC-RBMANAGER-13,
                  :QSPC-RBMANAGER-14,
                  :QSPC-RBMANAGER-15,
                  :QSPC-RBMANAGER-16,
                  :QSPC-RBMANAGER-17,
                  :QSPC-RBMANAGER-18,
                  :QSPC-RBMANAGER-19,
                  :QSPC-RBMANAGER-20,
                  :QSPC-RBMANAGER-21,
                  :QSPC-RBMANAGER-22,
                  :QSPC-RBUNITPER-CD-1, 
                  :QSPC-RBUNITPER-CD-2, 
                  :QSPC-RBUNITPER-CD-3, 
                  :QSPC-RBUNITPER-CD-4, 
                  :QSPC-RBUNITPER-CD-5, 
                  :QSPC-RBUNITPER-CD-6, 
                  :QSPC-RBUNITPER-CD-7, 
                  :QSPC-RBUNITPER-CD-8, 
                  :QSPC-RBUNITPER-CD-9, 
                  :QSPC-RBUNITPER-CD-10, 
                  :QSPC-RBUNITPER-CD-11, 
                  :QSPC-RBUNITPER-CD-12, 
                  :QSPC-RBUNITPER-CD-13, 
                  :QSPC-RBUNITPER-CD-14, 
                  :QSPC-RBUNITPER-CD-15, 
                  :QSPC-RBUNITPER-CD-16, 
                  :QSPC-RBUNITPER-CD-17, 
                  :QSPC-RBUNITPER-CD-18, 
                  :QSPC-RBUNITPER-CD-19, 
                  :QSPC-RBUNITPER-CD-20, 
                  :QSPC-RBUNITPER-CD-21, 
                  :QSPC-RBUNITPER-CD-22, 
                  :QSPC-RBFRMLA2-1, 
                  :QSPC-RBFRMLA2-2, 
                  :QSPC-RBFRMLA2-3, 
                  :QSPC-RBFRMLA2-4, 
                  :QSPC-RBFRMLA2-5, 
                  :QSPC-RBFRMLA2-6, 
                  :QSPC-RBFRMLA2-7, 
                  :QSPC-RBFRMLA2-8, 
                  :QSPC-RBFRMLA2-9, 
                  :QSPC-RBFRMLA2-10, 
                  :QSPC-RBFRMLA2-11, 
                  :QSPC-RBFRMLA2-12, 
                  :QSPC-RBFRMLA2-13, 
                  :QSPC-RBFRMLA2-14, 
                  :QSPC-RBFRMLA2-15, 
                  :QSPC-RBFRMLA2-16, 
                  :QSPC-RBFRMLA2-17, 
                  :QSPC-RBFRMLA2-18, 
                  :QSPC-RBFRMLA2-19, 
                  :QSPC-RBFRMLA2-20, 
                  :QSPC-RBFRMLA2-21, 
                  :QSPC-RBFRMLA2-22, 
                  :QSPC-CONTR-CREDIT,
                  :QSPC-CONTR-CREDIT-CD,
                  :QSPC-CONTR-CREDIT-MIN-DEL-PER,
                  :QSPC-OPT-CAL-CHGABLE,
                  :QSPC-CPOT30-CD,
                  :QSPC-CPOTCHGOFF-CD,
                  :QSPC-RDPOTCHGOFF-CD,
                  :QSPC-CAL-MAX-AT-ESTIMATE,
                  :QSPC-DELQ-IB-AGE,
                  :QSPC-DELQ-IB-MAX-DAYS,
                  :QSPC-BANKRULE-RATE,
                  :QSPC-LC-INT-ON-REPO-NON-PL 
            FROM DBO.SPCFILE
            WHERE SPCFILE.SPC_ORGST    = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE  = :QSPC-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPC1-FILE.
           PERFORM CLOSE-IT.


      *=================================================================
      *    'IO' ROUTINES FOR "REPORT"
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

           MOVE REPORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              MOVE SV-BEG-BRANCH TO BEG-BRANCH
              MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y"  )
              MOVE SV-BEG-BRANCH TO BEG-BRANCH
              MOVE SV-END-BRANCH TO END-BRANCH
              MOVE SV-GROUP-FLAG TO GROUP-FLAG
              MOVE SV-ENT-GROUP  TO ENT-GROUP.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AC1DAY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
