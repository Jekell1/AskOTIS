       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AC1DAY.
       AUTHOR.        KEC.
       DATE-WRITTEN.  04-14-1999.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/AC1DAY.C
      *
      * DESCRIPTION   :  MONTHLY EARNINGS ACCRUAL - IB ACCRUAL
      *                  'MENU DRIVEN' DAILY REPORT WO/ UPDATES
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
      *             WORKING ON A PROBLEM JIM FOUND WHERE REPORT-LINES,
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
      *               MOVE WGR-SRESULT TO BR-KEY.
      *
      * AMM 2014.0820 {PL#0857} FIXED RT AND RS PATHS NOT BEING SET 
      *               PROPERLY
      *  CS 150721 CHANGED THE WKRS KEY TO HAVE A SEQUENCE, ALSO ADDED WKRS-
      *            RT-SEQNO   REGENCY PL887
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSBR".
       COPY "LIBGB/GBFSGR".
       COPY "LIBGB/GBFSPR".

       COPY "LIBLP/LPFSBW".
       COPY "LIBLP/LPFSCD".
       COPY "LIBLP/LPFSLC".
       COPY "LIBLP/LPFSLN".
       COPY "LIBLP/LPFSLT".
       COPY "LIBLP/LPFSSP".

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
       COPY "LIBGB/GBFDBR".
       COPY "LIBGB/GBFDGR".
       COPY "LIBGB/GBFDPR".

       COPY "LIBLP/LPFDBW".
       COPY "LIBLP/LPFDCD".
       COPY "LIBLP/LPFDLC".
       COPY "LIBLP/LPFDLN".
       COPY "LIBLP/LPFDLT".
       COPY "LIBLP/LPFDSP".

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

               05  REPORT-CLASS                      PIC 9(03).
                   88  REPORT-POOL-TOTAL                 VALUE 999.

               05  REPORT-ACCTNO                     PIC 9(08).
                   88  REPORT-CLASS-TOTAL                VALUE 99999999.

               05  FILLER                  REDEFINES REPORT-ACCTNO.
                   07  REPORT-LN-NUMBER              PIC 9(06).
                   07  REPORT-LN-SEQNO               PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  REPORT-REC-INFO                       PIC X(170).

      *-------------------------------------------------{ INT. BEARING }
           03  REPORT-IB-REC-INFO          REDEFINES REPORT-REC-INFO.
               05  REPORT-IB-DISPLAY-LNNO            PIC X(16). 
               05  REPORT-IB-LNAME                   PIC X(16). 
               05  REPORT-IB-FNAME                   PIC X(16). 
               05  REPORT-IB-FROM                    PIC 9(08).
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
               05  REPORT-IBX-INT-PDTH               PIC 9(06).      
               05  REPORT-IBX-INT-SINCE-LSTPY        PIC S9(9)V99.
               05  REPORT-IBX-INT-OWED               PIC S9(9)V99.
               05  REPORT-IBX-TRUE-ACCRUE            PIC S9(9)V99.
               05  REPORT-IBX-INCOME-ACCRUE          PIC S9(9)V99.
               05  REPORT-IBX-BALANCE                PIC S9(9)V99.
               05  REPORT-IBX-CONTR-PDTH             PIC 9(06).       
               05  REPORT-IBX-CONTR-STATUS           PIC 9(03).
               05  REPORT-IBX-NUMBER-ACCTS           PIC 9(07).
               05  FILLER                            PIC X(45).

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
           "@(#)UT/RZ1DAY S35 03/31/23-09:23:42 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR".
       COPY "LIBGB/GBWSGR".

       COPY "LIBLP/LPWSBW".
       COPY "LIBLP/LPWSCD".
       COPY "LIBLP/LPWSDL".
       COPY "LIBLP/LPWSLC".
       COPY "LIBLP/LPWSLN".
       COPY "LIBLP/LPWSLP".
       COPY "LIBLP/LPWSLT".
       COPY "LIBLP/LPWSRS".
       COPY "LIBLP/LPWSRT".
       COPY "LIBLP/LPWSSP".
      *COPY "LIBLP/LPWSWKRS".

      *-----------------------------------------------------------------
       01  BR-KEY                      PIC 9(04)     VALUE ZEROES.
       01  REPORT-IFN                  PIC X(06)     VALUE "REPORT".
       01  REPORT-PATH                 PIC X(35)     VALUE SPACES.
       01  WKRS-PATH                   PIC X(35)     VALUE SPACES.

      *=================================================================
       01  LOCAL-FILE-PATHS.
           03  LOCAL-WKRS-PATH            PIC X(37).
           03  LOCAL-RS-PATH              PIC X(35).
           03  LOCAL-BW-PATH              PIC X(35).
           03  LOCAL-DL-PATH              PIC X(35).
           03  LOCAL-LP-PATH              PIC X(35).
           03  LOCAL-LT-PATH              PIC X(35).
           03  LOCAL-RT-PATH              PIC X(35).

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
           03  LOAD-IB-FROM                          PIC 9(08).
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
           03  LOAD-IBX-INT-PDTH                     PIC 9(06).      
           03  LOAD-IBX-INT-SINCE-LSTPY              PIC S9(9)V99.
           03  LOAD-IBX-INT-OWED                     PIC S9(9)V99.
           03  LOAD-IBX-TRUE-ACCRUE                  PIC S9(9)V99.
           03  LOAD-IBX-INCOME-ACCRUE                PIC S9(9)V99.
           03  LOAD-IBX-BALANCE                      PIC S9(9)V99.
           03  LOAD-IBX-CONTR-PDTH                   PIC 9(06).       
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
       01  ACCRUAL-DATE-MMYY           PIC 9(04)     VALUE ZEROES.
       01  FILLER            REDEFINES ACCRUAL-DATE-MMYY.
           03  ACCRUAL-DATE-MMYY-MM    PIC 9(02).
           03  ACCRUAL-DATE-MMYY-YY    PIC 9(02).

       01  CALCUALTION-WORKERS.
           03  CALC-ADDON-EARNINGS     PIC S9(7)V99  COMP VALUE ZEROES.
           03  CALC-INTPDTH            PIC 9(06)     VALUE ZEROES.

       01  CASE-BUF                    PIC X(04)     VALUE SPACES.

       01  CURRENT-REPORT-TYPE         PIC X(03)     VALUE SPACES.
           88  CURRENT-REPORT-TYPE-IB                VALUE "IB ".
           88  CURRENT-REPORT-TYPE-IBX               VALUE "IBX".
           88  CURRENT-REPORT-TYPE-PC                VALUE "PC ".

       01  DISPLAY-ACCTNO.
           03  FILLER                  PIC X(03)     VALUE SPACES.
           03  DISPLAY-LNNO            PIC X(13)     VALUE SPACES.

       01  EXITPROG                    PIC X(09)     VALUE "LP/LPRPMU".
       01  EXITPROG-LIST               PIC X(09)     VALUE "EXITPROG.".
       01  FILE-ACCESS-CHECK-STAT      PIC X(02)     VALUE "00".
       01  HOLD-BRANCH                 PIC 9(05)     VALUE ZEROES.
       01  HOLD-CLASS                  PIC 9(03)     VALUE ZEROES.
       01  HOLD-POOL                   PIC 9(06)     VALUE ZEROES.

       01  HOLD-LC1-KEY.
           03  HOLD-LC-CODE            PIC 9(02)     VALUE ZEROES.

       01  HOLD-LN-COUNT               PIC 9(02)     VALUE ZEROES.
       01  HOLD-LT1-KEY                PIC X(08)     VALUE SPACES.

       01  HOLD-REPORT-KEY.
           03  HOLD-REPORT-TYPE        PIC X(03).
               88  HOLD-REPORT-TYPE-IB               VALUE "IB  ".
               88  HOLD-REPORT-TYPE-IBX              VALUE "IBX".
               88  HOLD-REPORT-TYPE-PC               VALUE "PC  ".
           03  HOLD-REPORT-BRANCH      PIC 9(05).
               88  HOLD-REPORT-GRAND-TOTAL           VALUE 99999.
           03  HOLD-REPORT-POOL        PIC 9(06).
               88  HOLD-REPORT-BRANCH-TOTAL          VALUE 999999.
           03  HOLD-REPORT-CLASS       PIC 9(03).
               88  HOLD-REPORT-POOL-TOTAL            VALUE 999.
           03  HOLD-REPORT-ACCTNO      PIC 9(08).
               88  HOLD-REPORT-CLASS-TOTAL           VALUE 99999999.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".
       01  MAX-IB-INTEREST             PIC S9(9)V99  COMP  VALUE ZEROES.
       01  ONE                         PIC S9(1)     VALUE ZEROES.

       01  PROCESS-ACCTNO-STAT         PIC X(02)     VALUE SPACES.
           88  PROCESS-ACCTNO-STAT-CONTINUE          VALUE SPACES.
           88  PROCESS-ACCTNO-STAT-COMPLETED         VALUE "XX".

       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER-TOTAL       PIC 9(04)     VALUE ZEROES.
           03  PAGE-NUMBER-TYPE        PIC 9(04)     VALUE ZEROES.
 
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(06)     COMP VALUE ZEROES.
 
      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF".
      *-----------------------------------------------------------------
           03  BEG-BRANCH                         PIC 9(04).
           03  END-BRANCH                         PIC 9(04).

           03  ACCRUAL-DATE                       PIC 9(06).
           03  FILLER                   REDEFINES ACCRUAL-DATE.
               05  ACCRUAL-DATE-MM                PIC 9(02).
               05  ACCRUAL-DATE-DD                PIC 9(02).
               05  ACCRUAL-DATE-YY                PIC 9(02).

           03  GROUP-OPTIONS.
               05  GROUP-FLAG                     PIC X(01).
               05  ENT-GROUP                      PIC X(04).
               05  LOCAL-PRINT-FG                 PIC X(01).

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
       01  VERSION-CONTROL             PIC X(01)     VALUE "C".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).
               05  SV-LOCAL-PRINT-FG   PIC X(01).

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
           03  FILLER                         PIC X(01).
           03  D01-IB-NUMBER-ACCTS            PIC ZZ9        BLANK ZERO.
           03  FILLER                         PIC X(12).
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
           03  FILLER                         PIC X(01).
           03  D02-IBX-NUMBER-ACCTS           PIC ZZ9        BLANK ZERO.
           03  FILLER                         PIC X(07).
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
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
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
       COPY "LIBCL/CREAIXW".
       COPY "LIBCL/RVCALCW".

       COPY "LIBGB/ACCESSW".
       COPY "LIBGB/AGEINGW".
       COPY "LIBGB/BRSECUREW".
       COPY "LIBGB/CALLID".
       COPY "LIBGB/DATERW".
       COPY "LIBGB/EOSPOOLW".
       COPY "LIBGB/GETENVW".
       COPY "LIBGB/GETPRW".
       COPY "LIBGB/GPENVW".
       COPY "LIBGB/GRWORKW".
       COPY "LIBGB/MKDIR.CMD".
       COPY "LIBGB/MKTEMPW".
       COPY "LIBGB/PROGIDW".
       COPY "LIBGB/RBACTW".
       COPY "LIBGB/REBATEW".
       COPY "LIBGB/PASSWDW".
       COPY "LIBGB/SCREENW".
       COPY "LIBGB/SPPDTHW".
       COPY "LIBGB/TIMEWK".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/FILEWK".
       COPY "LIBGB/MESSWK".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/RM.CMD".
      *COPY "LIBLP/ACRERN.CMD".
      *COPY "LIBLP/AC1DAY.CMD". %%%%%%%%%%%%%%%%% REPLACE THIS WHEN DONE
       COPY "LIBLP/AC9DAY.CMD".
       COPY "LIBLP/LPAERNW".
       COPY "LIBLP/LPAMTSW".
       COPY "LIBLP/LPAPRSW".
       COPY "LIBLP/LPAPRZW".
       COPY "LIBLP/LPCEPPW".
       COPY "LIBLP/LPCERNW".
       COPY "LIBLP/LPEADJW".
       COPY "LIBLP/LPEARNW".
       COPY "LIBLP/LPEDAYW".
       COPY "LIBLP/LPIBPCW".
       COPY "LIBLP/LPITRMW".
       COPY "LIBLP/LPMDTEW".
       COPY "LIBLP/LPPDUEW".
       COPY "LIBLP/LPRATEW".
       COPY "LIBLP/LPREGZW".
       COPY "LIBLP/LPSTATW".
       COPY "LIBLP/LPUERNW".
       COPY "LIBLP/LPUPERW".
       COPY "LIBLP/LPUPWKW".
       COPY "LIBLP/LPXDTEW".
       COPY "LIBLP/SETCTBLW".

       COPY "LIBWI/WINDOWSW".
       COPY "LIBWI/TSTINTW".
       COPY "LIBWI/DATESCW".
       COPY "LIBWI/GRSCANW".

      *=================================================================
      *    L I N K A G E - S E C T I O N
      *=================================================================
       COPY "LIBGB/GETFMW".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 BR-FILE BW-FILE CD-FILE GR-FILE LC-FILE LN-FILE
                 LT-FILE SP-FILE 
                 REPORT-FILE PR-FILE DPR-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/DECLRP".
      *-----------------------------------------------------------------
           PERFORM CLOSE-PR-FILE.
           CLOSE BR-FILE BW-FILE CD-FILE GR-FILE LC-FILE LN-FILE
                 LT-FILE SP-FILE 
                 REPORT-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/DECLRP2".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

      *=================================================================
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }
      D    STOP MESS.

           MOVE "X"           TO RECORDS-FOUND.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AC1DAY"      TO FORM-PROG.

           CALL FLDVAL USING STAT EXITPROG-LIST EXITPROG.
           MOVE ZEROES TO ACCRUAL-DATE.

      *-----------------------------------------------------------------
       COPY "LIBGB/EOINIT".
      *-----------------------------------------------------{ CONTINUE }
       INITIALIZATION-CONTINUE.

           MOVE PROGID-TEXT-PROCESSING TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE USERID-BR  TO BEG-BRANCH
                                 END-BRANCH.

           IF ( EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EOBUF-GROUP-PRPATH TO PRU-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ MAKE PATHS }
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WKRS-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ GET 'SYSINFO'  }
           CALL FLDVAL USING STAT SYSINFO-LIST SYSINFO-BUF.

           IF ( STAT-BAD )
              PERFORM FORM-IT
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN 'GLOBAL' FILE }
           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-SP1-FILE.

      *- - - - - - - - - - - - - - - - - - - -{ MISCELLANEOUS SETTINGS }
           PERFORM ENABLE-INTERRUPT.
           PERFORM INITIAL-PROGRESS-INDICATOR.
           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.
           MOVE "Y"       TO TSTINT-CONTINUE-ONLY.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( UP-KEY )
              GO TO ENTER-PRINT-GRAND-TOTALS.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
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

           IF ( ROUTE-BUF NOT = "00" )
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
           IF (        F6-KEY ) PERFORM DATESC-ROUTINE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ACCRUAL-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0         TO ACCRUAL-DATE.
           MOVE ACCRUAL-DATE-MM TO ACCRUAL-DATE-MMYY-MM.
           MOVE ACCRUAL-DATE-YY TO ACCRUAL-DATE-MMYY-YY.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( ACCRUAL-DATE NOT = ZEROES )
              GO TO DISPLAY-ACCRUAL-DATE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SYSDATE TO ALP-DATE.
           MOVE ALP-MO  TO ACCRUAL-DATE-MM.
           MOVE ALP-DA  TO ACCRUAL-DATE-DD.
           MOVE ALP-YR  TO ACCRUAL-DATE-YY.

           MOVE "S  A000ACCRUAL-DATE." TO VDU.
           MOVE ACCRUAL-DATE           TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-ACCRUAL-DATE-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE EOBUF-DATE2 TO ACCRUAL-DATE.

      *=================================================================
       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E X E C U T I O N - M O D U L E
      *
      ******************************************************************
       EXECUTION-MODULE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-KEY
              PERFORM START-BR-FILE
              GO TO EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       EXECUTION-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" ) 
              GO TO EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO EXECUTION-MODULE-END.

           MOVE WGR-SRESULT TO BR-KEY.
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

      *KC->IF ( LOCAL-PRINT-FG = "Y" )
      *       PERFORM CLOSE-PR-FILE
      *       PERFORM CREATE-SPOOL-DIR
      *       IF ( NOT STAT-VALID )
      *KC->      GO TO EXECUTION-NEXT-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM PROCESS-BRANCH-MODULE.
 
           GO TO EXECUTION-NEXT-GROUP.

      *=================================================================
       EXECUTION-MODULE-END.
     
           PERFORM PRINT-REPORT-MODULE.

      *-----------------------------------------------------------------
       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.
 
      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE "00" TO FILE-ACCESS-CHECK-STAT.

           PERFORM FILE-ACCESS-CHECK-BW.
           PERFORM FILE-ACCESS-CHECK-DL.
           PERFORM FILE-ACCESS-CHECK-LN.
           PERFORM FILE-ACCESS-CHECK-LP.
           PERFORM FILE-ACCESS-CHECK-LT.
           PERFORM FILE-ACCESS-CHECK-RS.
           PERFORM FILE-ACCESS-CHECK-RT.
      *    PERFORM FILE-ACCESS-CHECK-WKRS.

           MOVE FILE-ACCESS-CHECK-STAT TO STAT.

           IF ( STAT-BAD )
              GO TO PROCESS-BRANCH-MODULE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN 'LOCAL' FILES }
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ CLEAR FIELDS }
           MOVE ZEROES TO HOLD-LC-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ START 'LN1'  }
           MOVE ZEROES TO LN-ACCTNO.
           PERFORM START-LN1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT-LN.

           PERFORM READ-LN1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-MODULE-END.

           PERFORM SEND-PROGRESS-INDICATOR.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ CHECK: POOL  }
           IF ( LN-DLPOOL < BEG-POOL ) OR
              ( LN-DLPOOL > END-POOL )
              GO TO PROCESS-BRANCH-NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - -{ GET 'ACCTNO' DISPLAY }
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO DISPLAY-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ GET 'BW1'  }
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           IF ( IO-BAD )
              MOVE "*NOT ON FILE*" TO BW-LNAME
              MOVE SPACES          TO BW-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ GET 'LC1'  }
           MOVE LN-CLASS TO LC-CODE.

           IF ( LC1-KEY NOT = HOLD-LC1-KEY )
              MOVE LC1-KEY TO HOLD-LC1-KEY
              PERFORM READ-LC1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ GET 'SP1'  }
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF ( IO-BAD )
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO PROCESS-ACCTNO-STAT.
           PERFORM PROCESS-ACCTNO-PC.
           PERFORM PROCESS-ACCTNO-IB.

           GO TO PROCESS-BRANCH-NEXT-LN.
 
      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LT1-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - A C C T N O - I B
      *
      ******************************************************************
       PROCESS-ACCTNO-IB SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

      *KC->IF ( PROCESS-ACCTNO-STAT-COMPLETED ) OR
           IF ( GP-IB-ACCRUALS-DONE NOT = "Y" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
              
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-REVOLVING   NOT = "Y"    ) AND
              ( SP-MAX-IB-ACR-DAYS = 0      )
              GO TO PROCESS-ACCTNO-IB-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    USED TO ALWAYS SKIP REPO ACCOUNTS ON IB ACCRUAL, FROM A60
      *    FORWARD.  AUTOMOTIVE CREDIT WANTED TO TAKE EARNINGS TILL
      *    ACCOUNT CHARGED OFF MERCURY, SKIP REPO ACCOUNTS:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GP-IB-ACCRUE-EARN-ON-REPO NOT = "Y"        ) AND
              ( LN-REPOCD                     = "X" OR "S" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ CHECK 'IBPC  }
           MOVE ACCRUAL-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           IF (  IBPC-FG           = "I"  ) OR
              (( IBPC-FG           = "P" )  AND
               ( IBPC-EXPIRED-FLAG = "E" ))
              NEXT SENTENCE
           ELSE
              GO TO PROCESS-ACCTNO-IB-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ VALID RECORD TO PROCESS  }
           MOVE "XX" TO PROCESS-ACCTNO-STAT.
           MOVE "IB" TO CURRENT-REPORT-TYPE.
           PERFORM LOAD-REPORT-IB-FIELDS.
           PERFORM UPDATE-REPORT-FILE.

      *- - - - - - - - - - - - - - - - - - -{ CHECK FOR IB EXCEPTIONS  }
           IF ( LOAD-IB-MAX NOT = SPACES )
              MOVE "IBX" TO CURRENT-REPORT-TYPE
              PERFORM LOAD-REPORT-IBX-FIELDS
              PERFORM UPDATE-REPORT-FILE.

      *=================================================================
       PROCESS-ACCTNO-IB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - A C C T N O - P C
      *
      ******************************************************************
       PROCESS-ACCTNO-PC SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

      *KC->IF ( PROCESS-ACCTNO-STAT-COMPLETED )
      *KC->   GO TO PROCESS-ACCTNO-PC-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SKIP REVOLVING LOANS                              JTG: 990226
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-REVOLVING = "Y" )
              GO TO PROCESS-ACCTNO-PC-EXIT.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SKIP LOANS IN RESCISSION AND LOANS THAT HAVE BEEN RESCINDED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-POCD-INRESCISSION ) OR
              ( LN-POCD-RESCIND      )
              GO TO PROCESS-ACCTNO-PC-EXIT.
              
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    POCD - **  IS A DELETED CONTRACT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-POCD = "**" )
              MOVE LN-POFFDATE TO NUM-DATE 
              IF ( LN-SALEFIN = "Y" )
                 MOVE LN-PURDATE  TO SYS-DATE
              ELSE
                 MOVE LN-LOANDATE TO SYS-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SKIP LOAN IF NOT DELETED THIS MONTH:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (  LN-POCD = "**"                )  AND
              (( NUM-YR NOT = ACCRUAL-DATE-YY )  OR
               ( NUM-MO NOT = ACCRUAL-DATE-MM ))
              GO TO PROCESS-ACCTNO-PC-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SKIP LOAN IF BOOKED AND DELETED THIS MONTH:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      *- - - - - - - - - - - - - - - - - - -{ VALID RECORD TO PROCESS  }
           MOVE "XX" TO PROCESS-ACCTNO-STAT.
           MOVE "PC" TO CURRENT-REPORT-TYPE.
           PERFORM LOAD-REPORT-PC-FIELDS.
           PERFORM UPDATE-REPORT-FILE.

      *=================================================================
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE ZEROES TO CALC-ADDON-EARNINGS.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ START 'LT1'  }
           MOVE LN-ACCTNO TO LT-ACCTNO.
           MOVE "I"       TO LT-CODE.
           MOVE ZEROES    TO LT-SEQNO.
           PERFORM START-LT1-FILE.

      *=================================================================
       LOAD-CALC-ADDON-EARN-NEXT.

           PERFORM READ-LT1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO LOAD-CALC-ADDON-EARN-EXIT.

           IF ( LT-ACCTNO NOT = LN-ACCTNO )
              GO TO LOAD-CALC-ADDON-EARN-EXIT.

           IF ( LT-CODE NOT = "I" )
              GO TO LOAD-CALC-ADDON-EARN-EXIT.

           IF NOT (LT-INT-REBATE = SPACES OR "C")
              GO TO LOAD-CALC-ADDON-EARN-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * MUST HAVE, BECAUSE GET-LT-P RECORD OPENS AND CLOSES FILE IF THE
      * ADDON HAS A PAYMENT SCHEDULE, BECAUSE ERNDELQFRMLA COMPUTES
      * CONTRACTUAL ROUTINE FOR START AND STOP POINTS ! (COLTN, 12/2/96)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LT1-KEY TO HOLD-LT1-KEY.

           IF ( LT-INSPREM < 0      )
              MOVE -1 TO ONE
           ELSE
              MOVE +1 TO ONE.

           MOVE LT-ORIG-1STPAY      TO AERN-LN-ORIG-1STPAY.
           MOVE LT-ANTIC-START-DATE TO AERN-LN-LOANDATE
                                       AERN-LN-PURDATE
                                       AERN-LN-INTDATE.

           SUBTRACT LT-PRIOR-NODEF FROM LN-TOTNODEF
                                 GIVING AERN-LN-TOTNODEF.

           MOVE LT-ANTICTERM  TO AERN-LN-ANTICTERM.
           MOVE "N"           TO AERN-LN-SALEFIN.
           MOVE LT-INT-REBATE TO AERN-LN-REBATE.

           MULTIPLY ONE BY LT-ANTICERN(1) GIVING AERN-LN-ANTICERN.
           MULTIPLY ONE BY LT-ANTICADJ(1) GIVING AERN-LN-ANTICADJ.
           MULTIPLY ONE BY LT-UFERN   (1) GIVING AERN-LN-UFERN.
           MULTIPLY ONE BY LT-ACCERN  (1) GIVING AERN-LN-ACCERN.

           MOVE ACCRUAL-DATE         TO AERN-ENDDATE.
           MOVE LN-LAST-ACCRUAL-DATE TO AERN-LAST-ACCRUAL-DATE.
           MOVE "Y"                  TO AERN-ADDON-FG.
           MOVE 2                    TO AERN.

           PERFORM COMPUTE-AERNINGS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IF AN ADDON, AND HAVE A PAYMENT SCHEDULE, THE "P" RECORD WAS
      * READ AND THE LT FILE WAS CLOSED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LT1-KEY NOT = HOLD-LT1-KEY )
              PERFORM OPEN-LT1-FILE
              MOVE HOLD-LT1-KEY TO LT1-KEY
              PERFORM READ-LT1-FILE.

           MULTIPLY ONE BY AERN-EARNINGS(2).
           ADD AERN-EARNINGS(2) TO CALC-ADDON-EARNINGS
                                   LT-ACCERN(1).

      *KC->MOVE AERN-EARNINGS(2) TO LT-INT-LAST-EARNED.
      *KC->PERFORM REWRITE-LT1-FILE.

           GO TO LOAD-CALC-ADDON-EARN-NEXT.

      *=================================================================
       LOAD-CALC-ADDON-EARN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - I B - F I E L D S
      *
      ******************************************************************
       LOAD-REPORT-IB-FIELDS SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE SPACES TO
                LOAD-IB-DISPLAY-LNNO        LOAD-IB-LNAME
                LOAD-IB-FNAME               LOAD-IB-MAX.

           MOVE ZEROES TO
                LOAD-IB-FROM                LOAD-IB-INT-SINCE-LSTPY
                LOAD-IB-INTBAL              LOAD-IB-TRUE-ACCRUE
                LOAD-IB-ACCRUED.

           MOVE ZEROES TO MAX-IB-INTEREST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DISPLAY-LNNO TO LOAD-IB-DISPLAY-LNNO.
           MOVE BW-LNAME     TO LOAD-IB-LNAME.
           MOVE BW-FNAME     TO LOAD-IB-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-REVOLVING = "Y" )
              PERFORM LOAD-REPORT-IB-FIELDS-REV
           ELSE
              PERFORM LOAD-REPORT-IB-FIELDS-STD.

      *=================================================================
       LOAD-REPORT-IB-FIELDS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - I B - F I E L D S - R E V
      *
      *-----------------------------------------------------------------
      *    REVOLVING ACCOUNT IB SETUP FOR ACCRUAL
      ******************************************************************
       LOAD-REPORT-IB-FIELDS-REV SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

      *- - - - - - - - - - - - - - - - - - -[ SETUP LOCAL FILE PATHS ]
           MOVE WKRS-PATH TO LOCAL-WKRS-PATH.
           MOVE RS-PATH   TO LOCAL-RS-PATH.
           MOVE BW-PATH   TO LOCAL-BW-PATH.
           MOVE DL-PATH   TO LOCAL-DL-PATH.
           MOVE LP-PATH   TO LOCAL-LP-PATH.
           MOVE LT-PATH   TO LOCAL-LT-PATH.
           MOVE RT-PATH   TO LOCAL-RT-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: RVCALC-CYCLE-THRU-DATE IS NOT REQUIRED WHEN 
      *       RVCALC-UPDATE-FILES = "N" OR "A"
      *         AND IT MUST BE "A", EXCEPT FROM RVSTUP.C
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "A"          TO RVCALC-UPDATE-FILES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N"          TO RVCALC-PRINT-A-STATEMENT-FG.
           MOVE BR-KEY       TO RVCALC-BR-KEY.
           MOVE ACCRUAL-DATE TO RVCALC-SYSTEM-DATE.
           MOVE 0            TO RVCALC-CYCLE-THRU-DATE.
           UNLOCK LN-FILE.
           MOVE "CL/RVCALC"  TO FORM-NAM.

           CALL FORM-PATH USING FORM-PATH RVCALC-WORKERS LN-REC SP-REC 
                                LOCAL-FILE-PATHS.
           CANCEL FORM-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( STMT-PRIOR-STMTDATE = ZEROES )
              MOVE ZEROES              TO LOAD-IB-FROM
           ELSE
              MOVE STMT-PRIOR-STMTDATE TO LOAD-IB-FROM.

           MOVE STMT-CURDUE-INT TO LOAD-IB-INT-SINCE-LSTPY.
      *CS  MOVE LN-INTBAL       TO LOAD-IB-INTBAL.
      *CS  LN-INTBAL NOW INCLUDES THE ACCRUAL AMOUNT SO ITS NOT CORRECT
           COMPUTE LOAD-IB-INTBAL = 
              LN-INTBAL - LOAD-IB-INT-SINCE-LSTPY.

      *CS  COMPUTE LOAD-IB-TRUE-ACCRUE = LN-INTBAL + STMT-CURDUE-INT.
      *CS  STMT-CURDUE-INT ALREADY INCLUDES LN-INTBAL
           COMPUTE LOAD-IB-TRUE-ACCRUE = 
               LOAD-IB-INTBAL + STMT-CURDUE-INT.

      *--->IF ( SP-MAX-IB-ACR-DAYS NOT = 999             ) AND
      *       ( INDU-INTEREST          > MAX-IB-INTEREST )
      *       COMPUTE LOAD-IB-ACCRUED  = MAX-IB-INTEREST + LN-INTBAL
      *       MOVE "*"                 TO LOAD-IB-MAX
      *--->ELSE
              MOVE LOAD-IB-TRUE-ACCRUE TO LOAD-IB-ACCRUED.

      *=================================================================
       LOAD-REPORT-IB-FD-REV-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - I B - F I E L D S - S T D
      *
      *-----------------------------------------------------------------
      *    NORMAL ACCOUNT (NON REVOLVING) IB SETUP FOR ACCRUAL
      ******************************************************************
       LOAD-REPORT-IB-FIELDS-STD SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( LN-INTPDTH = 0      )
              MOVE LN-INTDATE          TO LOAD-IB-FROM
           ELSE
              MOVE LN-INTPDTH          TO LOAD-IB-FROM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS ROUTINE WILL ESTABLISH THE MAXIMUM INTEREST ASSESS
      *    FROM THE LAST ACCRUED DATE, PER SP-MAX-IB-ACR-DAYS: 
      *
      *         RE: MOVE 1 TO INDU-MAX-ACCRUE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SP-MAX-IB-ACR-DAYS NOT = 0      )
              IF ( LN-INTPDTH = 0      )
                 MOVE LN-INTDATE       TO NDTE-DATE CALC-INTPDTH
              ELSE
                 MOVE LN-INTPDTH       TO NDTE-DATE CALC-INTPDTH
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

      *=================================================================
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE SPACES TO
                LOAD-IBX-DISPLAY-LNNO       LOAD-IBX-LNAME
                LOAD-IBX-FNAME.

           MOVE SPACES TO
                LOAD-IBX-INT-PDTH           LOAD-IBX-INT-SINCE-LSTPY
                LOAD-IBX-INT-OWED           LOAD-IBX-TRUE-ACCRUE
                LOAD-IBX-INCOME-ACCRUE      LOAD-IBX-BALANCE
                LOAD-IBX-CONTR-PDTH         LOAD-IBX-CONTR-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DISPLAY-LNNO            TO LOAD-IBX-DISPLAY-LNNO.
           MOVE BW-LNAME                TO LOAD-IBX-LNAME.
           MOVE BW-FNAME                TO LOAD-IBX-FNAME.
           MOVE CALC-INTPDTH            TO LOAD-IBX-INT-PDTH.
           MOVE LOAD-IB-INT-SINCE-LSTPY TO LOAD-IBX-INT-SINCE-LSTPY.
           MOVE LOAD-IB-INTBAL          TO LOAD-IBX-INT-OWED.
           MOVE LOAD-IB-TRUE-ACCRUE     TO LOAD-IBX-TRUE-ACCRUE.
           MOVE LOAD-IB-ACCRUED         TO LOAD-IBX-INCOME-ACCRUE.
           MOVE LN-CURBAL               TO LOAD-IBX-BALANCE.

           MOVE LN-1STPYDATE            TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL          TO LOAD-IBX-CONTR-PDTH.

           MOVE ACCRUAL-DATE            TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE CONTRACTUAL             TO LOAD-IBX-CONTR-STATUS. 

      *=================================================================
       LOAD-REPORT-IBX-FLDS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - R E P O R T - P C - F I E L D S
      *
      ******************************************************************
       LOAD-REPORT-PC-FIELDS SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

      *REGACC DOES NOT WANT TO INCLUDE MFEE(THEY USE TO AMORTIZE
      *PARTICIPATION EXPENSE 
           IF NOT(GP-INCL-OTH-IN-EARNED-R)                         
              SUBTRACT LOAD-PC-MFEE FROM LOAD-PC-ENDING.


      *=================================================================
       LOAD-REPORT-PC-FIELDS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - M O D U L E
      *
      ******************************************************************
       PRINT-REPORT-MODULE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

      *=================================================================
       PRINT-REPORT-MODULE-END.

           PERFORM PRINT-REPORT-RANGES.

      *-----------------------------------------------------------------
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

      *=================================================================
       PRINT-REPORT-PG-BREAK-END.

           MOVE REPORT-KEY TO HOLD-REPORT-KEY.

      *-----------------------------------------------------------------
       PRINT-REPORT-PG-BREAK-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - R A N G E S
      *
      ******************************************************************
       PRINT-REPORT-RANGES SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( NO-RECORDS-FOUND )
              GO TO PRINT-REPORT-RANGES-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE ACCRUAL-DATE                          TO R01-DATE.
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

      *=================================================================
       PRINT-REPORT-RANGES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - I B
      *
      ******************************************************************
       PRINT-REPORT-IB SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

           MOVE REPORT-IB-FROM             TO D01-IB-FROM.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
           GO TO PRINT-REPORT-IB-EXIT.

      *=================================================================
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
           GO TO PRINT-REPORT-IB-EXIT.

      *=================================================================
       PRINT-REPORT-IB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - I B X
      *
      ******************************************************************
       PRINT-REPORT-IBX SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

           MOVE REPORT-IBX-INT-PDTH        TO D02-IBX-INT-PDTH.
           MOVE REPORT-IBX-INT-SINCE-LSTPY TO D02-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED        TO D02-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE     TO D02-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE   TO D02-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE         TO D02-IBX-BALANCE.
           MOVE REPORT-IBX-CONTR-PDTH      TO D02-IBX-CONT-PDTH.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
           GO TO PRINT-REPORT-IBX-EXIT.

      *=================================================================
       PRINT-REPORT-IBX-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - P C
      *
      ******************************************************************
       PRINT-REPORT-PC SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
           GO TO PRINT-REPORT-PC-EXIT.

      *=================================================================
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ EXIT }
           GO TO PRINT-REPORT-PC-EXIT.

      *=================================================================
       PRINT-REPORT-PC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - F I L E
      *
      ******************************************************************
       UPDATE-REPORT-FILE SECTION.
      *- - - - - - - - - - - - - - - - - - - -{ SET 'HOLD-REPORT-KEY'  }

           MOVE CURRENT-REPORT-TYPE TO HOLD-REPORT-TYPE.
           MOVE LN-OWNBR            TO HOLD-REPORT-BRANCH.
           MOVE ZEROES              TO HOLD-REPORT-POOL.
           MOVE LN-CLASS            TO HOLD-REPORT-CLASS.
           MOVE LN-ACCTNO           TO HOLD-REPORT-ACCTNO.

           IF ( PRINT-BY-POOL = "Y" )
              MOVE LN-DLPOOL      TO HOLD-REPORT-POOL.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ UPDATE: DETAIL }
           PERFORM UPDATE-REPORT-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ UPDATE: CLASS  }
           MOVE 99999999 TO HOLD-REPORT-ACCTNO.
           PERFORM UPDATE-REPORT-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ UPDATE: POOL }
           MOVE 999      TO HOLD-REPORT-CLASS.

           IF ( PRINT-BY-POOL = "Y" )
              PERFORM UPDATE-REPORT-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ UPDATE: BRANCH }
           MOVE 999999   TO HOLD-REPORT-POOL.
           PERFORM UPDATE-REPORT-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ UPDATE: GRAND  }
           MOVE 99999    TO HOLD-REPORT-BRANCH.
           PERFORM UPDATE-REPORT-RECORD.

      *=================================================================
       UPDATE-REPORT-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D
      *
      ******************************************************************
       UPDATE-REPORT-RECORD SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

      *=================================================================
       UPDATE-REPORT-RECORD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - C L E A R
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-CLEAR SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE SPACES          TO REPORT-REC.
           MOVE HOLD-REPORT-KEY TO REPORT-KEY.

           IF ( REPORT-TYPE-IB )
              MOVE ZEROES TO
                   REPORT-IB-FROM            REPORT-IB-INT-SINCE-LSTPY
                   REPORT-IB-INTBAL          REPORT-IB-TRUE-ACCRUE
                   REPORT-IB-ACCRUED         REPORT-IB-MAX
                   REPORT-IB-NUMBER-ACCTS.

           IF ( REPORT-TYPE-IBX )
              MOVE ZEROES TO 
                   REPORT-IBX-INT-PDTH
                   REPORT-IBX-INT-SINCE-LSTPY
                   REPORT-IBX-INT-OWED
                   REPORT-IBX-TRUE-ACCRUE       
                   REPORT-IBX-INCOME-ACCRUE       
                   REPORT-IBX-BALANCE
                   REPORT-IBX-CONTR-PDTH
                   REPORT-IBX-CONTR-STATUS  REPORT-IBX-NUMBER-ACCTS.

           IF ( REPORT-TYPE-PC )
              MOVE ZEROES TO 
                   REPORT-PC-ANTERM          REPORT-PC-ANTIC
                   REPORT-PC-UNEARN          REPORT-PC-INT
                   REPORT-PC-ADDON           REPORT-PC-SRV
                   REPORT-PC-MFEE            REPORT-PC-DISC
                   REPORT-PC-OTHER           REPORT-PC-CASH-EARN
                   REPORT-PC-ENDING          REPORT-PC-NUMBER-ACCTS.

      *=================================================================
       UPDATE-REPORT-REC-CLR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - I B
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-IB SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-RECORD-IB-SKIP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LOAD-IB-DISPLAY-LNNO     TO REPORT-IB-DISPLAY-LNNO.
           MOVE LOAD-IB-LNAME            TO REPORT-IB-LNAME.
           MOVE LOAD-IB-FNAME            TO REPORT-IB-FNAME.
           MOVE LOAD-IB-FROM             TO REPORT-IB-FROM.
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

      *=================================================================
       UPDATE-REPORT-RECORD-IB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - I B E X
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-IBX SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-RC-IBX-SKIP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LOAD-IBX-DISPLAY-LNNO TO REPORT-IBX-DISPLAY-LNNO.
           MOVE LOAD-IBX-LNAME        TO REPORT-IBX-LNAME.
           MOVE LOAD-IBX-FNAME        TO REPORT-IBX-FNAME.
           MOVE LOAD-IBX-INT-PDTH     TO REPORT-IBX-INT-PDTH.
           MOVE LOAD-IBX-CONTR-PDTH   TO REPORT-IBX-CONTR-PDTH.
           MOVE LOAD-IBX-CONTR-STATUS TO REPORT-IBX-CONTR-STATUS.

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

      *=================================================================
       UPDATE-REPORT-RC-IBX-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - R E P O R T - R E C O R D - P C
      *
      ******************************************************************
       UPDATE-REPORT-RECORD-PC SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

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

      *=================================================================
       UPDATE-REPORT-REC-PC-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > REPORT-LINES )
              PERFORM WRITE-PAGE-HEADER-LINES.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 
      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R - L I N E S
      *
      ******************************************************************
       WRITE-PAGE-HEADER-LINES SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER-TOTAL.
           ADD  01 TO PAGE-NUMBER-TYPE
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - -{ GET 'BR' RECORD  }
           IF ( PRINT-DETAIL-ANALYSIS = "N" )
              GO TO WRITE-PAGE-HEADER-CONTINUE.

           IF ( REPORT-BRANCH NOT = HOLD-BRANCH ) AND
              ( NOT REPORT-GRAND-TOTAL          )
              MOVE REPORT-BRANCH TO BR-KEY HOLD-BRANCH
              PERFORM READ-BR-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - -{ GET 'CD1' RECORD }
           IF ( REPORT-POOL = ZEROES )
              MOVE "* NO POOL ID FOR LOANS *"    TO CD-POOLDESC
           ELSE
           IF ( REPORT-POOL NOT = HOLD-POOL ) AND
              ( NOT REPORT-BRANCH-TOTAL     )
              MOVE "PL"                          TO CD-POOLID
              MOVE REPORT-POOL                   TO CD-POOLID
              MOVE SPACES                        TO CD-POOLFILLER
              PERFORM READ-CD1-FILE
              IF ( IO-BAD )
                 MOVE "* NOT ON FILE *         " TO CD-POOLDESC.

      *- - - - - - - - - - - - - - - - - - - - - - -{ GET 'LC1' RECORD }
           IF ( REPORT-CLASS NOT = HOLD-CLASS ) AND
              ( NOT REPORT-POOL-TOTAL         )
              MOVE REPORT-CLASS TO LC-CODE HOLD-CLASS
              PERFORM READ-LC1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

      *-------------------------------------------------------------{01}
       WRITE-PAGE-HEADER-CONTINUE.

           MOVE SYSDATE                          TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE CONAME                           TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER-TYPE                 TO H01-PAGE-TYPE.
           MOVE PAGE-NUMBER-TOTAL                TO H01-PAGE-TOTAL.

           MOVE HEADER-LINE-01                   TO PR-REC.
      *KC->ADD  01                               TO LN-COUNT.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE USERID                           TO H02-USERID.
           MOVE ACCRUAL-DATE                     TO H02-ACCRUAL-DATE.

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
              MOVE CD-POOLDESC                   TO H04-POOL-DESC
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

      *=================================================================
       WRITE-PAGE-HEADER-LN-END.

           MOVE 01 TO LN-FEED.

      *-----------------------------------------------------------------
       WRITE-PAGE-HEADER-LN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/AGEING".
       COPY "LIBGB/BRSECURE".
       COPY "LIBGB/DATER".
       COPY "LIBGB/EOSPOOL".
       COPY "LIBGB/GPENV".
       COPY "LIBGB/GRSERCH".
       COPY "LIBGB/GRWORK".
       COPY "LIBGB/PROGID".
       COPY "LIBGB/REBATE".
       COPY "LIBGB/SCREEN".
       COPY "LIBGB/SPPDTH".
       COPY "LIBGB/TIMEIO".

       COPY "LIBLP/LPAERN".
       COPY "LIBLP/LPAMTS".
       COPY "LIBLP/LPAPRS".
       COPY "LIBLP/LPAPRZ".
       COPY "LIBLP/LPCEPP".
       COPY "LIBLP/LPCERN".
       COPY "LIBLP/LPEADJ".
       COPY "LIBLP/LPEARN".
       COPY "LIBLP/LPEDAY".
       COPY "LIBLP/LPIBPC".
       COPY "LIBLP/LPITRM".
       COPY "LIBLP/LPMDTE".
       COPY "LIBLP/LPPDUE".
       COPY "LIBLP/LPRATE".
       COPY "LIBLP/LPREGZ".
       COPY "LIBLP/LPSTAT".
       COPY "LIBLP/LPUERN".
       COPY "LIBLP/LPUPER".
       COPY "LIBLP/LPUPWK".
       COPY "LIBLP/LPXDTE".
       COPY "LIBGB/RBACT".
       COPY "LIBLP/SETCTBL".

       COPY "LIBWI/DATESC".
       COPY "LIBWI/WINDOWS".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS".
       COPY "LIBGB/GETENV".
       COPY "LIBGB/GETPR".
       COPY "LIBGB/MKTEMP".
       COPY "LIBGB/SYSTEM".
       COPY "LIBGB/UPPER.CMD".
       COPY "LIBGB/RMFILE".
      *=================================================================
       FILE-ACCESS-CHECK-BW.

           MOVE BR-MACHINE   TO BW-ALL-MACHINE
           MOVE BR-DATA-PATH TO BW-ALL-DATA
           MOVE BW-ALL-PATH  TO BW-PATH.
           MOVE BW-PATH      TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *-----------------------------------------------------------------
       FILE-ACCESS-CHECK-DL.

           MOVE BR-MACHINE   TO DL-ALL-MACHINE
           MOVE BR-DATA-PATH TO DL-ALL-DATA
           MOVE DL-ALL-PATH  TO DL-PATH.
           MOVE DL-PATH      TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *-----------------------------------------------------------------
       FILE-ACCESS-CHECK-LN.

           MOVE BR-MACHINE   TO LN-ALL-MACHINE
           MOVE BR-DATA-PATH TO LN-ALL-DATA
           MOVE LN-ALL-PATH  TO LN-PATH.
           MOVE LN-PATH      TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *-----------------------------------------------------------------
       FILE-ACCESS-CHECK-LP.

           MOVE BR-MACHINE   TO LP-ALL-MACHINE
           MOVE BR-DATA-PATH TO LP-ALL-DATA
           MOVE LP-ALL-PATH  TO LP-PATH.
           MOVE LP-PATH      TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *-----------------------------------------------------------------
       FILE-ACCESS-CHECK-LT.

           MOVE BR-MACHINE   TO LT-ALL-MACHINE
           MOVE BR-DATA-PATH TO LT-ALL-DATA
           MOVE LT-ALL-PATH  TO LT-PATH.
           MOVE LT-PATH      TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *-----------------------------------------------------------------
       FILE-ACCESS-CHECK-RS.

           MOVE BR-MACHINE   TO RS-ALL-MACHINE
           MOVE BR-DATA-PATH TO RS-ALL-DATA
           MOVE SYSDATE      TO ALP-DATE.
           MOVE ALP-MO       TO RS-ALL-MONTH.
           MOVE ALP-YR       TO RS-ALL-YEAR
           MOVE RS-ALL-PATH  TO RS-PATH.
           MOVE RS-PATH      TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

      *KC->IF ( STAT-BAD )
      *KC->   MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *-----------------------------------------------------------------
       FILE-ACCESS-CHECK-RT.

           MOVE BR-MACHINE   TO RT-ALL-MACHINE
           MOVE BR-DATA-PATH TO RT-ALL-DATA
           MOVE RT-ALL-PATH  TO RT-PATH.
           MOVE RT-PATH      TO ACCESS-BUF.
           PERFORM ACCESS-CALL.

      *KC->IF ( STAT-BAD )
      *KC->   MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *-----------------------------------------------------------------
      *FILE-ACCESS-CHECK-WKRS.

      *CS  MOVE BR-MACHINE   TO WKRS-ALL-MACHINE
      *CS  MOVE BR-DATA-PATH TO WKRS-ALL-DATA
      *CS  MOVE SYSDATE      TO ALP-DATE.
      *CS  MOVE ALP-MO       TO WKRS-ALL-MONTH.
      *CS  MOVE ALP-YR       TO WKRS-ALL-YEAR.
      *CS  MOVE WKRS-ALL-PATH  TO WKRS-PATH.
      *CS  MOVE WKRS-PATH      TO ACCESS-BUF.
      *CS  PERFORM ACCESS-CALL.

      *KC->IF ( STAT-BAD )
      *KC->   MOVE STAT TO FILE-ACCESS-CHECK-STAT.

      *=================================================================
       SEND-LEGEND.

           CALL XFER   USING STAT LEGEND-LIST LEGEND.
           CALL WRFORM USING STAT LEGEND-LIST.

      *=======================================================[ PROGID ]
       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE ROUTE-BUF              TO PROGID-ROUTE-BUF.
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

           PERFORM PROGID-DISPLAY.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
      *-----------------------------------------------------------------
       COPY "LIBGB/GBPRINT".
       COPY "LIBGB/GBBRRN".
       COPY "LIBGB/GBGR1RN".

       COPY "LIBLP/LPBW1R".
       COPY "LIBLP/LPCD1R".
       COPY "LIBLP/LPLC1R".
       COPY "LIBLP/LPLN1RN".
       COPY "LIBLP/LPLT1RN".
       COPY "LIBLP/LPSP1R".

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
       COPY "LIBGB/FERRORS".
           MOVE "E" TO ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.
           CALL XFER USING STAT ERRCD-LIST ERRCD.

           MOVE REPORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE WKRS-PATH   TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              MOVE SV-BEG-BRANCH TO BEG-BRANCH
              MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EOBUF-GROUP-FG = "Y"  )
              MOVE SV-BEG-BRANCH TO BEG-BRANCH
              MOVE SV-END-BRANCH TO END-BRANCH
              MOVE SV-GROUP-FLAG TO GROUP-FLAG
              MOVE SV-ENT-GROUP  TO ENT-GROUP.

           IF ( ROUTE-BUF NOT = "00" )
              CALL XFER USING STAT EOBUF-LIST EOBUF-BUF
           ELSE
              MOVE EXITPROG  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           EXIT PROGRAM.

