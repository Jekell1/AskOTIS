       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ACRERN.
       DATE-WRITTEN.  08-30-95.
      ******************************************************************
      *                              (LP)
      * DESCRIPTION:    MONTHLY EARNINGS ACCRUAL AND NON ACCRUAL
      *                            (DRIVER)
      *
      *      THIS PROGRAM DRIVES OFF OF THE GROUP OR BRANCH FILE
      *   A  GROUP ID OR BRANCH RANGE OF ONE OR ALL BRANCHES
      *   CAN BE ENTERED.  IT CHECKS TO MAKE SURE ALL DAYS ARE
      *   UPDATED FOR THE GROUP OR BRANCH RANGE ENTERED.  THE
      *   RC FILE FOR EACH BRANCH IS UPDATED (OR CREATED) WITH
      *   THE STATUS THAT EARNINGS HAVE BEEN UPDATED.
      *
      *      SUMMARY AND CORPORATE TOTALS FROM AC1ERN.C AND AC2ERN.C
      *   ARE STORED IN TEMPORARY WORK FILES WK-FILE AND WT-FILE
      *   RESECTIVELY DURING RUN AND PRINTED BY THIS DRIVER AFER ALL
      *   BRANCHES HAVE BEEN PROCESSED. TEMP FILES ARE OPENED FOR OUTPUT
      *   HERE AND HAVE THEIR PATHS PASSED TO AC1ERN AND AC2ERN VIA
      *   AC1-WORKER (DEFINED IN LINKAGE SECTION.)
      *      THESE TOTALS ARE WRITTEN TO THE GLOBAL ../ED DIRECTORY.
      *              EX:  A20895S.UNEARNED     AR0895T.PLUNEARN
      *                   A20895T.IBACCRUE     AR0895S.NONACCRU
      *       IF RAN FOR GROUP, THEN THE GROUP ID WILL BE ADDED TO
      *       FILE NAME.  THIS IS TO ALLOW DIFFERENT GROUPS TO BE RUN
      *       FROM SEPERATE TERMINALS WITHOUT OVERWRITING FILES
      *       IN GLOBAL ED.
      *              EX:  A20895S.UNEARNED_D01     AR0895S.PLUNEARN_DR2
      *                   A20895T.IBACCRUE_D02     AR0895T.NONACCRU_DR1
      *
      *      NOTE: THIS APPLIES TO GROUP OPTIONS ONLY, NOT BRANCH
      *            RANGES.
      *
      *      ALL OTHER DETAIL LINE REPORTS ARE WRITTEN TO THEIR
      *      RESPECTIVE
      *   BRANCHES ../ED DIRECTORY, DETERMINED BY THE BRANCH FILE.
      *      TO HELP PREVENT BOMBIING OUT, ACCESS CALLS ARE MADE TO ALL
      *   USED FILES AT THE BEGINNING OF EACH PROGRAM. IF ANY ONE OF THE
      *   FILES IS MISSING THEN A MESSAGE IS SENT TO THE ERROR LOG
      *   (../LG/ACR.[DATE]). ALL BRANCHES THAT WERE ABORTED FOR FILE
      *   ERRORS ETC WILL APPEAR HERE ALSO.
      *      ALL WORK FILES ARE REMOVED AT THE END OF RUN.
      *
      * REV:
      * MJD 082295 IN THE PROCESS UF CHANGING FOR MERCURY. OLD SAVED AS
      *             ACRERN.0822
      * MJD 083195 CHANGED PRINT PATH FROM USERS ED TO GLOBAL ED.
      * MJD 083195 ADDED GT-FILE UPDATE.
      * MJD 091195 FIXED PAGE HEADINGS.
      * MJD 091295 ADDED EXTENDED FILE NAMES WITH GROUP IDENTIFIERS.
      * BLL 092295 DRIVE OFF GROUP OR BRANCH; IF RC EXISTS, REWRITE,
      *            OTHERWISE, CREATE RC TO REFLECT EARNINGS STATUS
      * MJD 100495 SET RC-ALL-PATH.
      * MJD 100595 ADDING GT AND BY UPDATE FOR IB.
      * MJD 111095 CHANGED IB-ACCRUAL GL UPDATE TO UPDATE ARRUED
      *             INTEREST INCOME (GI-ACR-INT-AR) AND NOT INTEREST INCOME
      * BLV 050697 DON'T ALLOW PIGGYBACK PRINTING
      * MJD 060997 FIXED TO GET CORRECT GI FOR EACH BRANCH/CLASS OF BYFILE.
      * GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * MJD 091097 MAKE WK- FIELDS S9(9)V99 TO ACCOMODATE GT-AMT CHANGE.
      * JTG 110697 INCREASED WORKERS FROM S9(7)V99 TO S9(9)V99
      *            AND DISPLAYS TO Z(8)9.99-
      * GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      * MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      * BAH 051498 ADDED HOLD-TEST-YEAR FOR YEAR 2000 TEST.
      * MJD 080598 FIXED BUG, BR-NO WAS NOT GETTING SET FOR FIRST
      *            BRANCH PRINTED IN AC2ERN TOTALS.
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELPOMENT
      *            MEETING (082599)
      * BLV 000222 MOVE "N" TO RCINQ-EARNINGS-FG INSTEAD OF "Y" BEFORE
      *            CALLING RC INQUIRY
      * BAH 000427 TEMPORARY : IF GP-CASH-METHOD-ONLY = "Y" AND
      *            GP-IB-ACCRUALS-DONE = "N"; GO TO END-PROGRAM (UNTIL AC3ERN
      *            IS WORKING, YOU LOCK UP !!)
      * JTG 020118 FORCE GT-DATE TO THE 16TH OF THE MONTH         REGWAN PR#1714
      * JTG 030721 ADDED A SUFFIX TO THE SUMMARY PRINT PATH FOR BOGUS RUNS
      *            WHICH KICK OUT BRANCH'S THAT HAVE RC STATUS GREATER THAN
      *            'D1' EOD BATCH DONE, HOWEVER THE SUMMARY REPORTS GOT
      *            OVERRUN                                  LENDMARK KEVIN  J031
      * BAH 040202 DONT GO TO IO-ERROR ON BAD READ-GB-FILE, SKIP IT AND LOG
      *            IT...
      * BLF 070119 ADD UPDATE OF GT-POST-USERID, GT-POST-DATE,
      *            GT-POST-TIME, SUPPORT PR# 2780
      * BLF 080220 FIX PROBLEM - IF NO ACTIVE LOANS IN BRANCH, THE GL IB
      *            ACCRUAL ENTIRES MADE AT PRIOR EOM ARE NOT REVERSED IN THE
      *            CURRENT MONTH ACCRUAL PROCESS.  PROBLEM WHEN ALL ACCOUNTS
      *            ARE TRANSFERRED OUT OF A BRANCH.  CHANGED UPDATE-BY-LAST
      *            ROUTINE.  INSTEAD OF UPDATING FOR EACH BRANCH IN THE
      *            CREATE-GT1-DETAIL ROUTINE, CHANGED TO READ ALL BY'S
      *            FOR LAST MONTH, IF BRANCH IS IN RANGE & BY-IB-ACCRUED
      *            NOT = 0, MAKE GT ENTRY.  WESTERN, PL #618
      * BAH 090721 SUMMARY TOTAL ACCRUED PRINT AMOUNT WAS NOT SIGNED SO TOTALS
      *            LOOKED LIKE THEY WERE ADDING WRONG EVEN THOUGH ARE CORRECT
      *            REGMGM PL# 660
      * BLM 100120 FIX BUG CAUSED WHEN PL# 618 DONE ON 2/20/08.  UPDATE-BY-LAST
      *            ROUTINE WAS PERFORMED AT THE END, HAD NO WAY OF KNOWING
      *            RC-STATUS WHEN READING LAST MONTHS BY, SO IF YOU RAN ACCRUAL
      *            TWICE WITH SAME BRANCH & DATE RANGE, IT WOULD DOUBLE-UPDATE
      *            REVERSAL OF IB ACCRUAL TO GTFILE.  CHANGED TO PERFORM
      *            UPDATE-BY-LAST WHEN EACH BRANCH'S PROCESSING IS COMPLETE,
      *            SUPPORT PL# 672
      * BLM 100122 REMOVED RM-CMD FROM ACRERN.CMD, CHANGE TO REMOVE-WORKFILE
      *            LOGIC IN LIBGB/RMFILE & RM.CMD, NEED TO REMOVE .VIX FILES
      *  CS 121017 IF GP-I-AM-REGACC, CALL RAC323 WHICH LOOPS THRU EACH BRANCH
      *            IN THE GROUP RANGE, AND UPDATES THE LRFILE & AEFILE NON-
      *            ACCRUAL INTERNAL REPORTING FIELDS  PR#363 REGACC
      * BLM 130206 ADD SKIP-INCLUDE-ONLY OPTION FOR GROUP TOTALS ON UNEARNED/
      *            EARNED ACCRUAL REPORT.  SKIP JUST PRINTS SUMMARY-TOTALS
      *            LIKE BEFORE, INCLUDE PRINTS GROUP & SUMMARY, ONLY IS JUST
      *            GROUP TOTALS.  GROUP TOTALS JUST ON THIS ONE REPORT, NOT
      *            ON IB OR ANYTHING ELSE.  NOTE, IF WE NEED TO ADD GROUP
      *            TOTS FOR OTHER REPORTS, PUT IN SEPARATE TOTAL TABLE SO
      *            WE DON'T EXCEED 64K LIMIT ON WS TABLES.  WORLD PR# 848
      * BLM 130208 WORLD DOES NOT WANT P&L UNEARNED OR NON-ACCRUAL REPORTS,
      *            SO IF I-AM-WORLD, SKIP PROCESSING AC2 TOTALS,
      *            WORLD PR# 853
      * BLM 130308 CHANGE SUMMARY REPORTS TO WRITE TO BATCH DIRECTORY ACR IN
      *            EO INSTEAD OF GLOBAL ED.  NAMES HAVE BEEN SHORTENED.  IF
      *            ACCRUAL IS RUN MORE THAN ONCE, STEP NUMBERS WILL BE
      *            INCREMENTED.  WORLD PR# 862
      *  CS 130508 REMOVED CALL TO RAC323, REGACC NO LONGER WANTS TO RUN THE
      *            PREPROCESSOR PROGRAM  REGACC PR#342
      * BLM 131113 CHANGED DECLARE & DECLARE2 TO DECLRP & DECLRP2,
      *            SINCE THIS COPIES EOBUF & USES EXT-EOBUF-ERRCD
      * BLM 131002 FIX PRINT-ID, WAS NOT GETTING SET & MADE IB CORPORATE
      *            REPORT MESSED UP WITHOUT TOTAL INFO ON LINES, PL# 818
      *  CS 131202 ADD TEST OF GP-USE-ACCRUAL-EARN-FILE, IF 'R' (REGACC) RUN
      *            THE PREPROCESSOR (CL/RAC347)  REGACC
      * BLM 140205 FIX TO RUN IN BATCH, REGACC PR# 349
      * BLM 140604 CHANGED MESSAGE IF GI NOT FOUND ON REVERSE OF IB IN
      *            LAST MONTHS GI TO DISTINGUISH IT FROM SAME ERROR 
      *            IN GL UPDATE.  (JUST DOCUMENTING)  CHANGED TO
      *            "GI-REC NOT FOUND (REV) FOR BR:&&&& CLASS:&&"
      * BLM 140611 LOG "PRE-PROCESSOR IN PROCESS", REGACC PR# 349B
      * BLM 141104 ADD STATUS CHECK TO CREATE-SPOOL-DIR
      *  CS 141217 ON CALL TO AC1ERN PASS EXIT-PATHNAME, GETTING LINKAGE ERROR
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  CS 160202 CHANGED WK-CLASS,WT-CLASS TO 9(3)
      *  CS 160203 REMOVED USE OF GI-ACCT(GISUB),REMOVED USE OF CREDIT-INDEX
      *            AND DEBIT-INDEX WHICH WAS USED TO FIND THE GL-ACCT IN THE  
      *            GIFILE. NOW THE GI ACCOUNTS FOR THE IB-ACCRUAL ARE 
      *            BY NAME & THE EARNINGS COMPONENTS 1-6 ARE NOW EARNED/
      *            UNEARNED 1-6
      * BLM 160314 FIX FOR NEW BY-DATE FORMAT, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * KEC 2016.1201 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      *
      * BAH 170316 ACTIVATE 8 DIGIT DATES, PD0006
      * BLM 180228 FIX DEFAULT DATE DISPLAYING AS YYMMDD
      *  CS 181105 PER DISCUSSION WITH KEN/KM EMAIL, WORLD DOESNT USE AT 
      *            PRESENT SO DONT CONVERT AEFILE.
      *            MOVED CL/RAC347 TO .NO, SPECIFIC TO REGIONAL ACCEPTANCE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 20190711 ADDED GLGTCL, FUTURE FIELDS WERE NOT CLEARED!
      * BAH 20190920 REMOVED ACCESS-CALL ON GBFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 20211021 COMMENTED OUT CALL TO AC3ERN. DOESNT EXIST.
      * RMZ 20231127 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *-----------------------------------------------------------------
           SELECT WK-FILE ASSIGN WK-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  ALTERNATE RECORD KEY WK2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
      *-----------------------------------------------------------------
           SELECT WT-FILE ASSIGN WT-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WT1-KEY
                  ALTERNATE RECORD KEY WT2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
      *-----------------------------------------------------------------
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *
      *    SUMMARY TOTALS WORK RECORD
      *
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK1-KEY.
               05  WK-BRANCH           PIC 9(4).
               05  WK2-KEY.
                   07  WK-CLASS        PIC 9(3).
           03  WK-NOACCTS OCCURS 2     PIC 9(6)      COMP-3.
           03  WK-UNEARN               PIC S9(9)V99  COMP-3.
           03  WK-GL-AMT-TABLE.
               05  WK-INT              PIC S9(9)V99  COMP-3.
               05  WK-ADDON            PIC S9(9)V99  COMP-3.
               05  WK-SRV              PIC S9(9)V99  COMP-3.
               05  WK-MFEE             PIC S9(9)V99  COMP-3.
               05  WK-DISC             PIC S9(9)V99  COMP-3.
               05  WK-OTHER            PIC S9(9)V99  COMP-3.
           03  WK-GL-AMT     REDEFINES WK-GL-AMT-TABLE
                          OCCURS 6     PIC S9(9)V99  COMP-3.
           03  WK-CASH-EARN            PIC S9(9)V99  COMP-3.
           03  WK-ENDING               PIC S9(9)V99  COMP-3.
           03  WK-ACCRUED              PIC S9(9)V99  COMP-3.
           03  WK-INT-SINCE-LSTPY      PIC S9(9)V99  COMP-3.
           03  WK-INTBAL               PIC S9(9)V99  COMP-3.
           03  WK-TRUE-ACCRUE          PIC S9(9)V99  COMP-3.

      ******************************************************************
      *
      *   SUMMARY TOTALS WT WORK RECORD
      *
      ******************************************************************
       FD  WT-FILE
           LABEL RECORDS ARE STANDARD.
       01  WT-REC.
           03  WT1-KEY.
               05  WT-BRANCH           PIC 9(4).
               05  WT2-KEY.
                   07  WT-CLASS        PIC 999.
           03  WT-DATA           OCCURS 2 TIMES.
               07  WT-NOACCTS          PIC 9(6)      COMP-3.
               07  WT-UNEARN           PIC S9(9)V99  COMP-3.
               07  WT-INT              PIC S9(9)V99  COMP-3.
               07  WT-INT-RB           PIC S9(9)V99  COMP-3.
               07  WT-ADDON            PIC S9(9)V99  COMP-3.
               07  WT-SRV              PIC S9(9)V99  COMP-3.
               07  WT-SRV-RB           PIC S9(9)V99  COMP-3.
               07  WT-MFEE             PIC S9(9)V99  COMP-3.
               07  WT-MFEE-RB          PIC S9(9)V99  COMP-3.
               07  WT-DISC             PIC S9(9)V99  COMP-3.
               07  WT-OTHER            PIC S9(9)V99  COMP-3.
               07  WT-ACCRUED          PIC S9(9)V99  COMP-3.
               07  WT-ENDING           PIC S9(9)V99  COMP-3.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/ACRERN S35 01/30/25-10:38:59 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(2)  VALUE "PR".
       01  WK1-IFN                     PIC X(3)  VALUE "WK1".
       01  WK2-IFN                     PIC X(3)  VALUE "WK2".
       01  WT1-IFN                     PIC X(3)  VALUE "WT1".
       01  WT2-IFN                     PIC X(3)  VALUE "WT2".

       01  BY1-CONN-OPEN-SW            PIC X     VALUE "N".

      ******************************************************************
      *
      *     S C R E E N - B U F  --- ( E O B U F )
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH               PIC 9(4).
           03  END-BRANCH               PIC 9(4).
           03  EOM-DATE                 PIC 9(8).
           03  DATE1                   REDEFINES EOM-DATE.
               05  EOM-CC               PIC 99.
               05  EOM-YR               PIC 99.
               05  EOM-MO               PIC 99.
               05  EOM-DA               PIC 99.
           03  GROUP-FLAG               PIC X.
           03  ENT-GROUP                PIC X(4).
           03  GTT-ISO                  PIC X.
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE     PIC X(1)     OCCURS 10.

       01  VERSION-CONTROL             PIC X     VALUE "B".

      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------

       01  END-DATE-MMYY               PIC 9999.
       01  FILLER            REDEFINES END-DATE-MMYY.
           03  END-MONTH               PIC 99.
           03  END-YEAR                PIC 99.

       01  LAST-DATE-MMYY              PIC 9999.
       01  FILLER            REDEFINES LAST-DATE-MMYY.
           03  LAST-MONTH              PIC 99.
           03  LAST-YEAR               PIC 99.
       01  HOLD-BY-DATE-CCYYMM         PIC 9(6)  VALUE 0.

       01  HOLD-TEST-YEAR              PIC 99.

       01  MONTH-END                   PIC 9(8)  VALUE 0.
       01  FILLER            REDEFINES MONTH-END.
           03 MONTH-CC                 PIC 99.
           03 MONTH-YY                 PIC 99.
           03 MONTH-MM                 PIC 99.
           03 MONTH-DD                 PIC 99.

       01  LEV                         PIC 9(6)  COMP.
       01  LEV1                        PIC 9(6)  COMP.
       01  FIRST-TIME                  PIC X     VALUE "Y".
       01  HOLD-BRNO                   PIC 9(4)  VALUE 0.
       01  HOLD-CLASS                  PIC 9(3)  VALUE 0.
       01  FIRST-DAY-OF-NEXT-MONTH     PIC 9(8).
       01  LAST-DAY-OF-THIS-MONTH      PIC 9(8).
       01  WARNING-LIST                PIC X(8)   VALUE "WARNING.".
       01  MSEL-LIST                   PIC X(5)   VALUE "MSEL.".
       01  LEGEND                      PIC X(70)  VALUE SPACES.
       01  RC-REC-EXISTS               PIC X.
       01  EOM-RC-TRANS-DATE           PIC 9(8).
       01  WORK-TIME          PIC 9(6).
       01  FILLER REDEFINES WORK-TIME.
           04  TIME-HHMM      PIC 9(4).
           04  FILLER         PIC 9(2).

       01  LAST-EOM-DATE               PIC 9(8)  VALUE 0.

       01  IB-ACCRUALS-DONE            PIC X  VALUE " ".
       01  SV-BEG-BR                   PIC 9(4) VALUE 9999.
       01  SV-END-BR                   PIC 9(4) VALUE 9999.

       01  PRINT-PATH.
           03  PRINT-ALL-BASE          PIC X(09).
           03  FILLER                  PIC X(01)    VALUE "/".
           03  PRINT-ALL-MACHINE       PIC XX.
           03  PRINT-ALL-PATH          PIC X(7) VALUE "/ED/ACR".
           03  PRINT-ENDDATE           PIC 9(4).
           03  PRINT-ID                PIC X.
           03  FILLER                  PIC X     VALUE ".".
           03  PRINT-DESC              PIC X(8)  VALUE " ".
           03  PRINT-SUF               PIC X     VALUE " ".
           03  PRINT-U-BAR             PIC X     VALUE " ".
           03  PRINT-GROUP             PIC X(4)  VALUE " ".

       01  PRINT-SPOOL-NAME            PIC X(6).
       01  STEP-COUNT                  PIC 9(4)  VALUE 1.

       01  AC1-WORKER.
           03  HOLD-BR                 PIC 9(4).
           03  HOLD-DATE                PIC 9(4).
      * THESE ARE FILE PATHS
           03  WK-BUF                  PIC X(35).
           03  WT-BUF                  PIC X(35).
           03  WKSP-PATH               PIC X(35).
           03  WX-RUNTYPE              PIC X.

      * MUST MATCH CL/RAC347
      *01  AE-LR-WORKER.
      *     03  BEG-BR                 PIC 9(4).
      *     03  END-BR                 PIC 9(4).
      *     03  MONTH-END-DATE         PIC 9(8).
      *     03  GROUP-FG               PIC X.
      *     03  ENTER-GROUP            PIC X(4).


      ******************************************************************
      *
      *   GT UPDATE WORKERS.
      *
      ******************************************************************
       01 SUB1                         PIC 9(6)  COMP.
       01 CLSUB                        PIC 9(3)  COMP.
       01 WORKER-BRANCH                PIC 9(4).
       01 WORKER-GL-AMT                PIC S9(9)V99 COMP.
       01 SAVE-GT-LINE                 PIC 9(6).

       01 HOLD-CREDIT-GL               PIC 9(11) VALUE ZEROS.
       01 HOLD-DEBIT-GL                PIC 9(11) VALUE ZEROS.
       01 HOLD-GL-BR-ACCTNO            PIC 9(11).
       01 FILLER             REDEFINES HOLD-GL-BR-ACCTNO.
          03  HOLD-GL-BRANCH           PIC 9(4).
          03  HOLD-GL-ACCTNO           PIC 9(7).

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-FEED                     PIC 99       VALUE 0.
       01  LN-COUNT                    PIC 999      VALUE 99.
       01  PAGE-NUMBER                 PIC 9(4)     VALUE 0.
       01  SUB                         PIC 9(6)  COMP.

      ******************************************************************
      *
      *   TOTALS SUB 1 = CLASS
      *              2 = BRANCH
      *
      ******************************************************************
       01  TOTALS-TABLE.
           03  TOTAL-TBL               OCCURS 2.
               05  T-NOACCTS           PIC 9(6)         COMP.
               05  T-UNEARN            PIC S9(9)V99     COMP.
               05  T-INT               PIC S9(9)V99     COMP.
               05  T-ADDON             PIC S9(9)V99     COMP.
               05  T-SRV               PIC S9(9)V99     COMP.
               05  T-MFEE              PIC S9(9)V99     COMP.
               05  T-DISC              PIC S9(9)V99     COMP.
               05  T-OTHER             PIC S9(9)V99     COMP.
               05  T-CASH-EARN         PIC S9(9)V99     COMP.
               05  T-ENDING            PIC S9(9)V99     COMP.
               05  T-ACCRUED           PIC S9(9)V99     COMP.
               05  T-INT-SINCE-LSTPY   PIC S9(9)V99     COMP.
               05  T-INT-RB  REDEFINES T-INT-SINCE-LSTPY
                                       PIC S9(9)V99     COMP.
               05  T-INTBAL            PIC S9(9)V99     COMP.
               05  T-SRV-RB  REDEFINES T-INTBAL
                                       PIC S9(9)V99     COMP.
               05  T-TRUE-ACCRUE       PIC S9(9)V99     COMP.
               05  T-MFEE-RB REDEFINES T-TRUE-ACCRUE
                                       PIC S9(9)V99     COMP.

      ******************************************************************
      *    W S - G T T - T A B L E
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL       OCCURS 10 TIMES.
               05  WS-GTT-CLASS-TBL           OCCURS 101.
                   07  WS-GTT-NOACCTS         PIC 9(7)     COMP.
                   07  WS-GTT-UNEARN          PIC S9(9)V99 COMP.
                   07  WS-GTT-INT             PIC S9(9)V99 COMP.
                   07  WS-GTT-ADDON           PIC S9(9)V99 COMP.
                   07  WS-GTT-SRV             PIC S9(9)V99 COMP.
                   07  WS-GTT-MFEE            PIC S9(9)V99 COMP.
                   07  WS-GTT-DISC            PIC S9(9)V99 COMP.
                   07  WS-GTT-OTHER           PIC S9(9)V99 COMP.
                   07  WS-GTT-CASH-EARN       PIC S9(9)V99 COMP.
                   07  WS-GTT-ENDING          PIC S9(9)V99 COMP.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       01  MESS-LIST                   PIC X(5) VALUE "MESS.".
       01  MESS-LIST2                  PIC X(6) VALUE "MESSX.".

       01  SCREEN-LIST.
           03  FILLER                  PIC X(6) VALUE
           "TITLE.".

       01  SCREEN-BUF.
           03  FILLER                  PIC X(35) VALUE
           "  MONTHLY ACCRUAL EARNINGS UPDATE".

       01  AGING-DATE-MESS.
           03  FILLER              PIC X(29) VALUE
               "END OF MONTH AGEING DATE IS: ".
           03  D-AGE-DATE          PIC 99/99/99.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE                  PIC X(8)     VALUE " ".
           03  FILLER                  PIC XX       VALUE " ".
           03  H-HR                    PIC 99       VALUE 0.
           03  FILLER                  PIC X        VALUE ":".
           03  H-MIN                   PIC 99       VALUE 0.
           03  FILLER                  PIC X(30)    VALUE " ".
           03  H-NAME                  PIC X(40)    VALUE " ".
           03  H-RMEMO                 PIC X(38)    VALUE " ".
           03  FILLER                  PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                  PIC X(10)    VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(17)    VALUE " ".
           03  H-TITLE                 PIC X(49)    VALUE
           "UNEARNED / EARNED ACCRUAL REPORT   MONTH ENDING: ".
           03  H-ENDDATE               PIC X(8).
           03  H-MESS                  PIC X(38)    VALUE SPACE.

      *-----------------------------------------------------------------
       01  UN-HEAD2.
           03  UN-TITLE                PIC X(49)    VALUE
           "UNEARNED / EARNED ACCRUAL REPORT   MONTH ENDING: ".
           03  UN-MESS                 PIC X(38)    VALUE  SPACES.

      *-----------------------------------------------------------------
       01  IB-HEAD2.
           03  IB-TITLE                PIC X(49)    VALUE
           "ACCRUED INTEREST FOR I/B LOANS     MONTH ENDING: ".
           03  IB-MESS                 PIC X(38)    VALUE
               "     * - EXCEEDS MAX ACCRUE DAYS      ".

      *-----------------------------------------------------------------
       01  NEW-BRANCH.
           03  FILLER                  PIC X(8) VALUE "BRANCH:".
           03  D-T-BRNO                PIC 9(4).
           03  FILLER                  PIC XX.
           03  D-T-BRDESC              PIC X(20).

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(132) VALUE SPACES.

       01  TOTAL-LINE                 REDEFINES DETAIL-LINE.
           03  D-T-DESC                PIC X(20).
           03  FILLER                 REDEFINES D-T-DESC.
               05  FILLER              PIC XXX.
               05  D-T-CLASS           PIC 99.
               05  FILLER              PIC X(8).
               05  D-T-NOACCTS         PIC ZZZZZ9.
               05  D-T-TAG             PIC X.
           03  D-T-ANT.
               05  D-T-UNEARN          PIC Z(8)9.99-      BLANK ZERO.
               05  D-T-INT             PIC Z(8)9.99-      BLANK ZERO.
               05  FILLER             REDEFINES D-T-INT.
                   07  FILLER          PIC X(12).
                   07  D-T-INT-R       PIC X.
               05  D-T-ADDON           PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-SRV             PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  FILLER             REDEFINES D-T-SRV.
                   07  FILLER          PIC X(11).
                   07  D-T-SRV-R       PIC X.
               05  D-T-MFEE            PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  FILLER             REDEFINES D-T-MFEE.
                   07  FILLER          PIC X(11).
                   07  D-T-MFEE-R      PIC X.
               05  D-T-DISC            PIC Z(8)9.99-      BLANK ZERO.
               05  D-T-OTHER           PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-CASH-EARN       PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-ENDING          PIC Z(8)9.99-      BLANK ZERO.
           03  D-T-INTR               REDEFINES D-T-ANT.
               05  FILLER              PIC X(13).
               05  D-T-INT-SINCE-LSTPY PIC ZZZ,ZZZ,ZZ9.99-.
               05  FILLER              PIC X(2).
               05  D-T-INTBAL          PIC ZZZ,ZZZ,ZZ9.99-.
               05  FILLER              PIC X(2).
               05  D-T-TRUE-ACCRUE     PIC ZZZ,ZZZ,ZZ9.99-.
               05  FILLER              PIC X(4).
               05  D-T-ACCRUED         PIC ZZZ,ZZZ,ZZ9.99-.
       01  BRANCH-GROUPID              REDEFINES DETAIL-LINE.
           03  D-TYPE                  PIC X(10).
           03  D-GRID.
               05  D-BRNO              PIC 9(4).
           03  FILLER                  PIC XX.
           03  D-BRDESC                PIC X(20).
           03  FILLER                  PIC X(96).


      ******************************************************************
      *
      *    S U M M A R Y - L I N E S
      *
      ******************************************************************
       01  SUMMARY1                    PIC X(132)  VALUE SPACES.
       01  SUMMARY2                    PIC X(132)  VALUE SPACES.
       01  SUMMARY3                    PIC X(132)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  WK-SUMMARY1.
           03  FILLER                  PIC X(11) VALUE
               "* SUMMARY *".
           03  FILLER                  PIC X(101) VALUE " ".
           03  S-CLRIT                 PIC X(7)   VALUE "MTD-NON".
           03  FILLER                  PIC X(13)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  WK-SUMMARY2.
           03  FILLER                  PIC X(19) VALUE SPACES.
           03  FILLER                  PIC X(14) VALUE
               "NO      BEGIN ".
           03  FILLER                  PIC X(44) VALUE
               "      -------------------- MONTHLY ACCRUAL E".
           03  FILLER                  PIC X(32) VALUE
               "ARNINGS -------------------- ".
           03  FILLER                  PIC X(22) VALUE
               "   ACCRUAL      ENDING".

      *-----------------------------------------------------------------
       01  IB-SUMMARY2.
           03  FILLER                  PIC X(32) VALUE SPACES.
           03  FILLER                  PIC X(36) VALUE
               "      TOTAL ACCRUED            TOTAL".
           03  FILLER                  PIC X(36) VALUE
               "       TOTAL TRUE      TOTAL ACCRUED".
           03  FILLER           PIC X(28) VALUE SPACES.

      *-----------------------------------------------------------------

       01  WK-SUMMARY3.
           03  FILLER                  PIC X(32) VALUE
               "CLASS           ACCTS   UNEARNED".
           03  FILLER                  PIC X(52) VALUE
               "     INTEREST      ADDONS      SERCHG    MAINTFEE   ".
           03  FILLER                  PIC X(47) VALUE
               "   DL DISC       OTHER    EARNINGS     UNEARNED".
      *-----------------------------------------------------------------
       01  IB-SUMMARY3.
           03  FILLER                  PIC X(32) VALUE
               "CLASS               ACCTS       ".
           03  FILLER                  PIC X(36) VALUE
               " FROM LAST ASSESSED         INT OWED".
           03  FILLER                  PIC X(36) VALUE
               "         ASSESSED             AMOUNT".
           03  FILLER                  PIC X(28) VALUE SPACES.

      *----------------------------------------------------------------
       01  WT-SUMMARY1.
           03  FILLER                  PIC X(11) VALUE
               "* SUMMARY *".
           03  FILLER                  PIC X(101) VALUE " ".
           03  WT-S-CLRIT          PIC X(7)   VALUE " ".
           03  FILLER                  PIC X(13) VALUE SPACES.

      *-----------------------------------------------------------------
       01  WT-SUMMARY2.
           03  FILLER                  PIC X(21) VALUE SPACES.
           03  FILLER                  PIC X(14) VALUE
               "NO       BEGIN".
           03  WT-SUMMARY2-2           PIC X(43) VALUE
               "     ------------------ MONTHLY NON ACCRUAL".
           03  WT-SUMMARY2-3           PIC X(32) VALUE
               " EARNINGS -------------------- ".
           03  WT-SUMMARY2-4           PIC X(22) VALUE
               "  ACCRUAL      ENDING ".

      *-----------------------------------------------------------------
       01  WT-SUMMARY2-2PL             PIC X(43) VALUE
               "     ----------------------  UNEARNED INCOM".

      *-----------------------------------------------------------------
       01  WT-SUMMARY2-3PL             PIC X(32) VALUE
               "E  -------------------------".

      *-----------------------------------------------------------------
       01  WT-SUMMARY3.
           03  FILLER                  PIC X(32) VALUE
               "CLASS           ACCTS   UNEARNED".
           03  FILLER                  PIC X(52) VALUE
               "     INTEREST      ADDONS      SERCHG    MAINTFEE   ".
           03  FILLER                  PIC X(22) VALUE
               "   DL DISC       OTHER".
           03  WT-SUMMARY3-1           PIC X(25) VALUE
               "    EARNINGS     UNEARNED".
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBCL/CKSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBWI/RCINQW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBLP/LPBYCLW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
      *-----------------------------------------------------------------

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ACRERN_DEF.CPY".
       COPY "LIBLP/ACRERN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ACRERN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           CLOSE
               PR-FILE WK-FILE WT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.
      * INITIAL FOR AC1ERN
           MOVE SPACES TO WKSP-PATH. 

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ACRERN" TO VDU-FORM-NAME.

       RETRY-INIT.
           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD = "X"
              GO TO END-PROGRAM.

           IF EXT-EOBUF-ERRCD NOT = SPACE
              GO TO END-PROGRAM.

      * TIMER ADDED TO CODE - REMOVE BETWEEN %%%

           MOVE "R"    TO P-TIMER-TYPE-SET.
           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.

           IF ( P-TIMER-TYPE-SET-VALID )
               MOVE P-TIMER-TYPE-SET TO P-TIMER-TYPE
           ELSE
               MOVE "R" TO P-TIMER-TYPE.     *> REPORT OPTION

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      * REMOVE TO HERE!!!


      * CL/RAC347 ONLY USED BY REGACC AND UPDATES REGACC SPECIFIC
      *   LR FIELDS.  MOVED CL/RAC347 TO QA DIRECTORY
      *   LOGIC FOR AE IS PRETTY SELF-CONTAINED, USED IN AEMAIN,RAC347,
      *   BULKTG2,BULKT2,LONPF2.
      *    IF GP-I-AM-REGACC
      *       IF GP-USE-ACCRUAL-EARN-FILE = "R"
      *          MOVE "PRE-PROCESSOR IN PROCESS" TO MESS
      *                                             ERRLOGW-EOM-MSG
      *          PERFORM SEND-PREPROCESSOR
      *          PERFORM LOG-ERROR
      *          MOVE BEG-BRANCH TO BEG-BR
      *          MOVE END-BRANCH TO END-BR
      *          MOVE MONTH-END  TO MONTH-END-DATE
      *          MOVE GROUP-FLAG TO GROUP-FG
      *          MOVE ENT-GROUP  TO ENTER-GROUP
      *          MOVE "CL/RAC347" TO FORM-NAM
      *          CALL FORM-PROGX USING FORM-PATH AE-LR-WORKER
      *          CANCEL FORM-PROGX.

           MOVE "C" TO EXT-EOBUF-ERRCD.

      * TEMPORARY UNTIL AC3ERN GETS FIXED !!  4/27/00 BAH
           IF GP-CASH-METHOD-ONLY = "Y" AND GP-IB-ACCRUALS-DONE = "N"
              GO TO END-PROGRAM.
      * TEMPORARY UNTIL AC3ERN GETS FIXED !!  4/27/00 BAH

           IF GP-CASH-METHOD-ONLY NOT = "Y"
              MOVE " " TO EXT-EOBUF-ERRCD.

           IF GP-IB-ACCRUALS-DONE = "Y"
              MOVE "Y" TO IB-ACCRUALS-DONE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-BUF.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WT-BUF.
           PERFORM OPEN-WT1-FILE-OUTPUT.
           PERFORM CLOSE-WT1-FILE.

      *    PERFORM SET-USER-PATH.

           PERFORM EOM-UPDATE.

           GO TO END-ROUTINE.
      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE ACRERN-QUEUE-POS TO Q-POS.
           MOVE ACRERN-COPIES-POS TO C-POS.
           MOVE ACRERN-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           MOVE " " TO MESS EXT-EOBUF-ERRCD.
           PERFORM SEND-LEGEND.

           PERFORM GET-GPENV.

       COPY "LIBGB/EOINIT.CPY".

           IF PRU-LP = "VDU" OR "PRU"
              MOVE "PLEASE DO NOT SELECT PIGGY-BACK PRINTER" TO MESS
              PERFORM SEND-MESS
              GO TO EOINIT-SETPR.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP TRUE LAST DAY OF THE MONTH END:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EOM-DATE TO NDTE-DATE.
           MOVE 31 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.

           MOVE "S  A000AGEDATE." TO VDU.
           MOVE NDTE-DATE TO LAST-DAY-OF-THIS-MONTH
                             MONTH-END
                             DATE-YYYYMMDD.
           MOVE NDTE-MM TO END-MONTH.
           MOVE NDTE-YY TO END-YEAR.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-AGE-DATE.

           MOVE AGING-DATE-MESS TO VDUBUF.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SCREENIO.

           MOVE MONTH-MM TO END-MONTH.
           MOVE MONTH-YY TO END-YEAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP FIRST DAY OF THE NEXT MONTH:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE 1 TO NDTE-DD.

           MOVE NDTE-DATE TO FIRST-DAY-OF-NEXT-MONTH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP LAST MONTH/YEAR FOR TAG REMOVAL:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE EOM-DATE TO NDTE-DATE.
           MOVE 50       TO NDTE-DD.
           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO LAST-EOM-DATE.

      *-----------------------------------------------------------------
       STATUS-CHECK.
      *-----------------------------------------------------------------

           IF EXT-ROUTE-BUF = "00"
              MOVE "CHECKING BRANCH STATUS, PLEASE WAIT...."
                    TO MESS
              PERFORM SEND-LEGEND.

           MOVE "C" TO CKSTAT-ACTION.

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP   TO CKSTAT-GROUP
           ELSE
              MOVE BEG-BRANCH  TO CKSTAT-BEG-BRANCH.

           MOVE END-BRANCH     TO CKSTAT-END-BRANCH.
           MOVE EXT-JULIAN-BEG TO CKSTAT-DATE1.
           MOVE EXT-JULIAN-END TO CKSTAT-DATE2.
           MOVE "C1"           TO CKSTAT-HIGH-STATUS.
           MOVE "CL/CKSTAT"    TO FORM-NAM.
           CALL FORM-PROGX  USING FORM-PATH CKSTAT-BUF.
           CANCEL FORM-PROGX.

           IF EXT-ROUTE-BUF = "00"
              MOVE "CHECKING BRANCH STATUS, PLEASE WAIT...."
                    TO MESS
              PERFORM SEND-LEGEND.

           IF CKSTAT-RETURN-STAT = "N"
              MOVE "RANGE INCLUDES BRANCHES AT AN INCORRECT STATUS!"
                 TO MESS
              PERFORM SEND-MESS
              IF EXT-ROUTE-BUF = "00"
                 PERFORM RC-INQUIRY
              END-IF
           ELSE
              MOVE
              "ALL BRANCHES ARE AT A CORRECT STATUS TO PROCEED!"
                                                        TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACCEPT-IT.
           IF EXT-ROUTE-BUF = "70"
              IF CKSTAT-RETURN-STAT = "N"
                 MOVE "RANGE INCLUDES BRANCHES AT AN INCORRECT STATUS!"
                    TO ERRLOGW-EOM-MSG
                 PERFORM LOG-ERROR
                 GO TO ENDIT.

           IF CKSTAT-RETURN-STAT = "N"
              STRING
                 "EOM CAN'T RUN UNTIL "
                    "ALL BRANCHES ARE AT THE CORRECT STATUS!"
                      DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENDIT
           ELSE
              MOVE SPACES TO LEGEND.

           IF EXT-ROUTE-BUF NOT = "00"
              GO TO BEGIN-EOM.

           MOVE LEGEND TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "F1-CANCEL, F6-SCAN STATUSES, F7-EXIT, 'Y' TO ACCEPT"
              TO LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO ENDIT.

           IF VDUSTATUS = "10" GO TO EOINIT-SETPR.
           IF VDUSTATUS = "1U" GO TO EOINIT-ENTER-MENU.

           IF VDUSTATUS = "1<"
              PERFORM RC-INQUIRY
              GO TO ACCEPT-IT.

           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF VDUBUF = "N" GO TO EOINIT-ENTER-MENU.

           IF VDUBUF NOT = "Y"
              GO TO ACCEPT-IT.

      *-----------------------------------------------------------------
       BEGIN-EOM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PERFORM OPEN-PR-FILE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM CREATE-SPOOL-DIR.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE SPACES TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EOM-DATE     TO EOM-RC-TRANS-DATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-RC1-FILE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.
 
           MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE.

           GO TO EXIT-INITIALIZE.

      *----------------------------------------------------------------
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

      *----------------------------------------------------------------
       EXIT-INITIALIZE.
           EXIT.
      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *----------------------------------------------------------------
       EO-MOVE-DATES.

           MOVE EXT-EOBUF-DATE2 TO EOM-DATE.
           PERFORM CHECK-EOM-DATE.
           IF SUB = 0
              MOVE "INVALID GLOBAL PARAMETER DATE: &&&&&&&&"
              TO ERRLOGW-EOM-MSG
              MOVE EOM-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-EDIT-DATE
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&&&&&"
                 BY VDU-EDIT-DATE
              PERFORM LOG-ERROR
              MOVE "X" TO EXT-EOBUF-ERRCD.

      *****************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *----------------------------------------------------------------
       ENTER-PARM.

           MOVE SCREEN-BUF TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * USED TO BE A NONDISPLAY FIELD, MUST NOW SET TO "Y" IF DONT
      * STOP ON ACCEPT
           MOVE "Y" TO NOACCEPT-BUF.

           IF VDUSTATUS = "1U" GO TO EOM-DT.

      *----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
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
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.

           MOVE LEGEND-BR-02 TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
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

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.

           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       EOM-DT.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO GTT-ISO-PARAGRAPH.

           PERFORM GET-DEFAULT-DATE.

           MOVE LEGEND-EOM-DTE TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "ERNM060EOMDT."    TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO EOM-DT.

           MOVE VDU-DATE-YYYYMMDD TO EOM-DATE.
           PERFORM CHECK-EOM-DATE.

           IF SUB = 0
              MOVE "NO SUCH EOM DATE IN GLOBAL PARAMETER FILE"
              TO MESS
              PERFORM SEND-MESS
              GO TO EOM-DT.

           MOVE EOM-YR TO END-YEAR.
           MOVE EOM-MO TO END-MONTH.

           MOVE EOM-DATE TO NDTE-DATE.
           MOVE 50       TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO NUM-DATE MONTH-END.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID MONTH END DATE" TO MESS
              PERFORM SEND-MESS
              GO TO EOM-DT.

           MOVE SPACES TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO EXIT-PARM.

           MOVE LEGEND-GTT-01 TO WR-BUF..
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO EOM-DT.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

      *-----------------------------------------------------------------
       EXIT-PARM.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *----------------------------------------------------------------
       DISPLAY-PARM.

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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
      *****************************************************************
      *
      *    R C - I N Q U I R Y
      *
      *****************************************************************
       RC-INQUIRY SECTION.

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO RCINQ-GROUP
           ELSE
              MOVE BEG-BRANCH TO RCINQ-BEG-BRANCH.

           MOVE END-BRANCH TO RCINQ-END-BRANCH.
           MOVE EXT-JULIAN-BEG TO RCINQW-DATE1.
           MOVE EXT-JULIAN-END TO RCINQW-DATE2.
           MOVE "N" TO RCINQ-ENTER-RANGES.
           MOVE "Y" TO RCINQ-OPEN-FG.
           MOVE "N" TO RCINQ-EARNINGS-FG.
           MOVE "N" TO RCINQ-UPDATED-FG.
           MOVE "N" TO RCINQ-EOMUPD-FG.
           MOVE "N" TO RCINQ-DEPOSIT-FG.
           MOVE "N" TO RCINQ-CHECKS-FG.
           MOVE "WI/RCINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH RCINQ-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ******************************************************************
      *
      *    G E T - D E F A U L T - E O M D A T E
      *
      ******************************************************************
       GET-DEFAULT-DATE SECTION.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           MOVE 1 TO SUB.

      *-----------------------------------------------------------------
       CHECK-TODAY-LOOP.

           IF SUB > 12
              MOVE 0 TO SUB
              GO TO EXIT-CHECK-TODAY
           ELSE
              IF GP-END-FISCAL-DATE(SUB) = NUM-DATE
                 GO TO EXIT-CHECK-TODAY
              ELSE
                 ADD 1 TO SUB
                 GO TO CHECK-TODAY-LOOP.

      *-----------------------------------------------------------------
       EXIT-CHECK-TODAY.

           MOVE "S  A000EOMDT." TO VDU.

           IF SUB NOT = 0
              IF NUM-DATE = GP-END-FISCAL-DATE(SUB)
              MOVE GP-END-FISCAL-DATE(SUB) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
           ELSE
              MOVE 0 TO VDU-DATE-EDIT.

           PERFORM SCREENIO.
      ******************************************************************
      *
      *    C H E C K - E O M - D A T E
      *
      ******************************************************************
       CHECK-EOM-DATE SECTION.

           MOVE 1 TO SUB.

      *-----------------------------------------------------------------
       CHECK-EOM-LOOP.

           IF SUB > 12
              MOVE 0 TO SUB
              GO TO EXIT-CHECK-EOM
           ELSE
              IF GP-END-FISCAL-DATE(SUB) = EOM-DATE
                 GO TO EXIT-CHECK-EOM
              ELSE
                 ADD 1 TO SUB
                 GO TO CHECK-EOM-LOOP.

      *-----------------------------------------------------------------
       EXIT-CHECK-EOM.
           EXIT.

      *****************************************************************
      *
      *    E O M - U P D A T E --- END OF MONTH  - UPDATE, CLEAR
      *
      *****************************************************************
       EOM-UPDATE SECTION.

           MOVE "BEGINNING END-OF-MONTH PROCESSING"
              TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.

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
              GO TO EXIT-EOM-UPDATE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO CHECK-EOM-RC.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.

           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO EXIT-EOM-UPDATE.

      *-----------------------------------------------------------------
       CHECK-EOM-RC.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "PROCESSING IN PROGRESS........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.   
   
           MOVE BR-NO TO RC-BRNO.
           MOVE EOM-RC-TRANS-DATE TO RC-TRANS-DATE.
           PERFORM READ-RC1-FILE. *> EMBEDDED - RCFILE
           IF IO-BAD
              MOVE "N" TO RC-REC-EXISTS
              GO TO GET-BRANCH-PATHS
           ELSE
              MOVE "Y" TO RC-REC-EXISTS.

           IF RC-STATUS NOT = "D1"
              IF RC-STATUS NOT = "C1"
                 MOVE "RC STATUS ERROR - BRANCH #&&&&"
                 TO ERRLOGW-EOM-MSG
                 INSPECT ERRLOGW-EOM-MSG
                    REPLACING FIRST "&&&&" BY BR-NO
                 PERFORM LOG-ERROR
                 GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       GET-BRANCH-PATHS.
           IF BR-DATA-PATH = SPACES OR BR-MACHINE = SPACES
              MOVE "BRANCH &&&& HAS UNDEFINED DATA PATH"
                 TO ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG
                 REPLACING FIRST "&&&&" BY BR-NO
              PERFORM LOG-ERROR
              GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO GB-PATH-OVERRIDE.

           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "GB-FILE ERROR :BRANCH &&&& ACCRUAL SKIPPED" TO
                     ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&" BY BR-NO
              PERFORM LOG-ERROR
              GO TO NEXT-GROUP-LOC.

           MOVE GB-LAST-ACCRUE TO LAST-DATE-MMYY.
           COMPUTE HOLD-TEST-YEAR = LAST-YEAR + 1.

           IF (END-YEAR = LAST-YEAR AND END-MONTH = (LAST-MONTH + 1)) OR
              (LAST-MONTH = 12 AND (END-YEAR = HOLD-TEST-YEAR) AND
               END-MONTH = 1)
              PERFORM COMPUTE-ACCRUALS
              PERFORM UPDATE-RC-STATUS
              PERFORM UPDATE-BY-LAST
           ELSE
              MOVE "DATE ERROR :BR &&&& ACCRUAL SKIPPED:LAST &&&&" TO
                                               MESS ERRLOGW-EOM-MSG
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              INSPECT MESS REPLACING FIRST "&&" BY LAST-MONTH
              INSPECT MESS REPLACING FIRST "&&" BY LAST-YEAR
              INSPECT ERRLOGW-EOM-MSG
                      REPLACING FIRST "&&&&" BY BR-NO
              INSPECT ERRLOGW-EOM-MSG
                      REPLACING FIRST "&&" BY LAST-MONTH
              INSPECT ERRLOGW-EOM-MSG
                      REPLACING FIRST "&&" BY LAST-YEAR
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *M        PERFORM SEND-MESS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              PERFORM LOG-ERROR.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       EXIT-EOM-UPDATE.
           EXIT.
      ******************************************************************
      *
      *    U P D A T E - R C - F I L E
      *
      ******************************************************************
       UPDATE-RC-STATUS SECTION.
      *-----------------------------------------------------------------
       UPDATE-RC.

           IF RC-REC-EXISTS = "N"
              PERFORM CLEAR-RC-FILE
              MOVE BR-NO TO RC-BRNO
              MOVE EOM-RC-TRANS-DATE TO RC-TRANS-DATE
              MOVE "CREATED BY EOM" TO RC-ERROR-MSG.

           MOVE "G1" TO RC-STATUS.
           MOVE END-DATE-MMYY TO RC-LAST-ACCRUE.

           PERFORM SET-POSTDATE.
           MOVE POST-TIME TO RC-EARNINGS-TIME.
           MOVE POST-DATE TO RC-EARNINGS-DATE.

           IF RC-REC-EXISTS = "Y"
              PERFORM REWRITE-RC1-FILE *> EMBEDDED - RCFILE
           ELSE
              PERFORM WRITE-RC1-FILE *> EMBEDDED - RCFILE
              IF IO-BAD
                 GO TO IO-ERROR.

      *-----------------------------------------------------------------
       EXIT-UPDATE-RC.
           EXIT.
      ******************************************************************
      *
      *    E N D - R O U T I N E
      *
      ******************************************************************
       END-ROUTINE SECTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  PROCESS AC1ERN.C TOTALS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "PROCESSING AC1ERN TOTALS" TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.

           MOVE 1 TO LEV.

           PERFORM CLOSE-PR-FILE.
           PERFORM SET-PRINT-S.

           PERFORM OPEN-PR-FILE.

           MOVE "D"        TO PRINT-ID.
           MOVE 99         TO LN-COUNT.
           MOVE 0          TO PAGE-NUMBER.
           MOVE MONTH-END  TO H-ENDDATE.
           MOVE EXT-CONAME-CONAME     TO H-NAME.
           MOVE EXT-CONAME-SYSDATE    TO H-DATE.
           MOVE EXT-CONAME-USER-ID    TO H-USERID.
           MOVE SPACES     TO S-CLRIT.

           PERFORM OPEN-WK1-FILE.

           IF (GTT-ISO = "I" OR GTT-ISO = "O")
              MOVE 99 TO LN-COUNT
              PERFORM GROUP-TOTAL-TABLE.

           IF GTT-ISO NOT = "O"
              PERFORM SUMMARY-TOTALS
              PERFORM CORPORATE-TOTALS.

           MOVE 0 TO T-NOACCTS(1) T-NOACCTS(2) T-UNEARN(1) T-UNEARN(2)
                     T-INT(1)     T-INT (2)    T-ADDON(1)  T-ADDON (2)
                     T-SRV(1)     T-SRV (2)    T-MFEE(1)   T-MFEE(2)
                     T-DISC(1)    T-DISC (2)   T-OTHER(1)  T-OTHER (2)
                     T-CASH-EARN(1) T-CASH-EARN(2)
                     T-ENDING(1)    T-ENDING(2).

           MOVE 0 TO PAGE-NUMBER.
           MOVE MONTH-END TO H-ENDDATE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE SPACES TO S-CLRIT.

           MOVE 99 TO LN-COUNT.

           MOVE 2 TO LEV.
           PERFORM CLOSE-PR-FILE.
           PERFORM SET-PRINT-IB-S.

           PERFORM OPEN-PR-FILE.

           PERFORM SUMMARY-TOTALS.
           PERFORM CORPORATE-TOTALS.

      *----------------------------------------------------------------
      * PROCESS AC2ERN.C TOTALS:
      *----------------------------------------------------------------
      *    IF GP-I-AM-WORLD
      *       GO TO GT-UPDATE.

      *    MOVE "D" TO PRINT-ID.
      *    MOVE 99 TO LN-COUNT.
      *    MOVE 0 TO PAGE-NUMBER.
      *    MOVE MONTH-END TO H-ENDDATE.
      *    MOVE EXT-CONAME-CONAME TO H-NAME.
      *    MOVE EXT-CONAME-SYSDATE TO H-DATE.
      *    MOVE EXT-CONAME-USER-ID TO H-USERID.
      *    MOVE SPACES TO WT-S-CLRIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NEED TO BE ABLE TO DETERMINE WHICH TOTALS TO RUN IN ACRERN.C.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


      *    IF WX-RUNTYPE = "B" OR "N" OR "P"
      *       MOVE "PROCESSING AC2ERN TOTALS" TO ERRLOGW-EOM-MSG
      *       PERFORM LOG-ERROR.


      *    IF WX-RUNTYPE = "B" OR "N"
      *       MOVE 1 TO LEV1
      *       PERFORM SET-PRINT-S-WT
      *       PERFORM SUMMARY-TOTALS-WT
      *       PERFORM CORPORATE-TOTALS-WT
      *       MOVE 99 TO LN-COUNT.

      *    MOVE "S" TO PRINT-ID.
      *    MOVE 0 TO PAGE-NUMBER.

      *    IF WX-RUNTYPE = "B" OR "P"
      *       MOVE 2 TO LEV1
      *       PERFORM SET-PRINT-PL-S-WT
      *       PERFORM SUMMARY-TOTALS-WT
      *       PERFORM CORPORATE-TOTALS-WT.

      *----------------------------------------------------------------
      * UPDATE GT FILE.
      *----------------------------------------------------------------
       GT-UPDATE.
           PERFORM CREATE-GT1-DETAIL.

           MOVE "ACRERN COMPLETED:" TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.

           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    B R A N C H - T O T A L S --- FOR AC1ERN.C
      *
      ******************************************************************
       BRANCH-TOTALS SECTION.

           MOVE 2 TO LN-FEED.
           MOVE 2 TO SUB.

              MOVE "   ---- TOTAL:" TO D-T-DESC
              PERFORM PRINT-SUM-BRANCH.

      *-----------------------------------------------------------------
       END-BRANCH-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2) T-MFEE(2) T-DISC(2) T-OTHER(2)
                     T-CASH-EARN(2) T-ENDING(2) T-ACCRUED(2)
                     T-TRUE-ACCRUE(2) T-INTBAL(2) T-INT-SINCE-LSTPY(2).

      ******************************************************************
      *
      *    P R I N T - B R A N C H - T O T A L S --- FOR AC1ERN.C
      *
      ******************************************************************
       PRINT-SUM-BRANCH SECTION.
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.

           IF PRINT-ID = "S" OR "D"
              MOVE T-UNEARN(SUB) TO D-T-UNEARN
              MOVE T-ADDON(SUB) TO D-T-ADDON
              MOVE T-MFEE(SUB) TO D-T-MFEE
              MOVE T-OTHER(SUB) TO D-T-OTHER
              MOVE T-INT(SUB) TO D-T-INT
              MOVE T-SRV(SUB) TO D-T-SRV
              MOVE T-DISC(SUB) TO D-T-DISC
              MOVE T-CASH-EARN(SUB) TO D-T-CASH-EARN
              MOVE T-ENDING(SUB) TO D-T-ENDING
           ELSE
              MOVE T-ACCRUED(SUB) TO D-T-ACCRUED
              MOVE T-TRUE-ACCRUE(SUB) TO D-T-TRUE-ACCRUE
              MOVE T-INTBAL(SUB) TO D-T-INTBAL
              MOVE T-INT-SINCE-LSTPY(SUB) TO D-T-INT-SINCE-LSTPY.

           PERFORM WRITE-DETAIL-LINE.
      ******************************************************************
      *
      *    S U M M A R Y - T O T A L S --- FOR AC1ERN.C
      *
      ******************************************************************
       SUMMARY-TOTALS SECTION.

           MOVE "Y" TO FIRST-TIME.

           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2) T-MFEE(2) T-DISC(2) T-OTHER(2)
                     T-CASH-EARN(2) T-ENDING(2) T-ACCRUED(2)
                     T-TRUE-ACCRUE(2) T-INTBAL(2) T-INT-SINCE-LSTPY(2).

           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.
           MOVE 0 TO WK-BRANCH WK-CLASS.

           PERFORM START-WK1-FILE.

      *-----------------------------------------------------------------
       SUMMARY-NEXT-WK.

           PERFORM READ-WK1-FILE-NEXT.

           IF IO-BAD
              PERFORM BRANCH-TOTALS
              GO TO END-SUMMARY-TOTALS.

           IF WK-NOACCTS(LEV) = 0
              GO TO SUMMARY-NEXT-WK.

           IF FIRST-TIME = "Y"
              MOVE WK-BRANCH TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE.

           IF WK-BRANCH NOT = HOLD-BRNO
              PERFORM BRANCH-TOTALS
              MOVE WK-BRANCH TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE
              MOVE 3 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED.

           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           MOVE WK-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE WK-NOACCTS(LEV) TO D-T-NOACCTS.

           IF PRINT-ID = "S" OR "D"
              MOVE WK-UNEARN TO D-T-UNEARN
              MOVE WK-INT TO D-T-INT
              MOVE WK-ADDON TO D-T-ADDON
              MOVE WK-SRV TO D-T-SRV
              MOVE WK-MFEE TO D-T-MFEE
              MOVE WK-DISC TO D-T-DISC
              MOVE WK-OTHER TO D-T-OTHER
              MOVE WK-CASH-EARN TO D-T-CASH-EARN
              MOVE WK-ENDING TO D-T-ENDING
           ELSE
              MOVE WK-ACCRUED TO D-T-ACCRUED
              MOVE WK-TRUE-ACCRUE TO D-T-TRUE-ACCRUE
              MOVE WK-INTBAL TO D-T-INTBAL
              MOVE WK-INT-SINCE-LSTPY TO D-T-INT-SINCE-LSTPY.

           PERFORM WRITE-DETAIL-LINE.

           ADD WK-NOACCTS(LEV) TO T-NOACCTS(2).
           IF PRINT-ID = "S" OR "D"
              ADD WK-UNEARN TO T-UNEARN(2)
              ADD WK-INT TO T-INT(2)
              ADD WK-ADDON TO T-ADDON(2)
              ADD WK-SRV TO T-SRV(2)
              ADD WK-MFEE TO T-MFEE(2)
              ADD WK-DISC TO T-DISC(2)
              ADD WK-OTHER TO T-OTHER(2)
              ADD WK-CASH-EARN TO T-CASH-EARN(2)
              ADD WK-ENDING TO T-ENDING(2)
           ELSE
              ADD WK-ACCRUED TO T-ACCRUED(2)
              ADD WK-TRUE-ACCRUE TO T-TRUE-ACCRUE(2)
              ADD WK-INTBAL TO T-INTBAL(2)
              ADD WK-INT-SINCE-LSTPY TO T-INT-SINCE-LSTPY(2).
           GO TO SUMMARY-NEXT-WK.

      *-----------------------------------------------------------------
       END-SUMMARY-TOTALS.
           EXIT.
      ******************************************************************
      *
      *    P R I N T - C O R P O R A T E - T O T A L S --- FOR AC1ERN.C
      *
      ******************************************************************
       PRINT-CORPORATE-TOTS SECTION.

           MOVE 2 TO LN-FEED.

           MOVE "   ---- TOTAL:" TO D-T-DESC.
           MOVE 1 TO SUB.
           PERFORM PRINT-SUM-BRANCH.

      *-----------------------------------------------------------------
       END-CORP-TOTALS.

           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2) T-MFEE(2) T-DISC(2) T-OTHER(2)
                     T-CASH-EARN(2) T-ENDING(2) T-ACCRUED(2)
                     T-TRUE-ACCRUE(2) T-INTBAL(2) T-INT-SINCE-LSTPY(2).
      ******************************************************************
      *
      *    C O R P O R A T E - T O T A L S --- FOR AC1ERN.C
      *
      ******************************************************************
       CORPORATE-TOTALS SECTION.

           MOVE 0 TO T-NOACCTS(1) T-UNEARN(1) T-INT(1) T-ADDON(1)
                     T-SRV(1) T-MFEE(1) T-DISC(1) T-OTHER(1)
                     T-CASH-EARN(1) T-ENDING(1) T-ACCRUED(1)
                     T-TRUE-ACCRUE(1) T-INTBAL(1) T-INT-SINCE-LSTPY(1).

           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2) T-MFEE(2) T-DISC(2) T-OTHER(2)
                     T-CASH-EARN(2) T-ENDING(2) T-ACCRUED(2)
                     T-TRUE-ACCRUE(2) T-INTBAL(2) T-INT-SINCE-LSTPY(2).

           MOVE "Y" TO FIRST-TIME.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK2-FILE.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK2-FILE.

      *-----------------------------------------------------------------
       CORPORATE-NEXT-WK.

           PERFORM READ-WK2-FILE-NEXT.

           IF IO-BAD
              PERFORM PRINT-CORPORATE
              PERFORM PRINT-CORPORATE-TOTS
              GO TO END-CORPORATE-TOTALS.

           IF WK-NOACCTS(LEV) = 0
              GO TO CORPORATE-NEXT-WK.

           IF FIRST-TIME = "Y"
              MOVE 3 TO LN-FEED
              MOVE "CORPORATE TOTALS:" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED
              MOVE WK-CLASS TO HOLD-CLASS
              MOVE "N" TO FIRST-TIME.

           IF WK-CLASS NOT = HOLD-CLASS
              PERFORM PRINT-CORPORATE
              MOVE WK-CLASS TO HOLD-CLASS.

           ADD WK-NOACCTS(LEV) TO T-NOACCTS(2).

           IF PRINT-ID = "S" OR "D"
              ADD WK-UNEARN TO T-UNEARN(2)
              ADD WK-INT TO T-INT(2)
              ADD WK-ADDON TO T-ADDON(2)
              ADD WK-SRV TO T-SRV(2)
              ADD WK-MFEE TO T-MFEE(2)
              ADD WK-DISC TO T-DISC(2)
              ADD WK-OTHER TO T-OTHER(2)
              ADD WK-CASH-EARN TO T-CASH-EARN(2)
              ADD WK-ENDING TO T-ENDING(2)
           ELSE
              ADD WK-ACCRUED TO T-ACCRUED(2)
              ADD WK-TRUE-ACCRUE TO T-TRUE-ACCRUE(2)
              ADD WK-INTBAL TO T-INTBAL(2)
              ADD WK-INT-SINCE-LSTPY TO T-INT-SINCE-LSTPY(2).

           GO TO CORPORATE-NEXT-WK.

      *-----------------------------------------------------------------
       END-CORPORATE-TOTALS.
           PERFORM CLOSE-WK2-FILE.
      ******************************************************************
      *
      *    P R I N T - C O R P O R A T E - T O T A L S --- FOR AC1ERN.C
      *
      ******************************************************************
       PRINT-CORPORATE SECTION.

           MOVE HOLD-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE T-NOACCTS(2) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.

           IF PRINT-ID = "S" OR "D"
              MOVE T-UNEARN(2) TO D-T-UNEARN
              MOVE T-INT(2) TO D-T-INT
              MOVE T-ADDON(2) TO D-T-ADDON
              MOVE T-SRV(2) TO D-T-SRV
              MOVE T-MFEE(2) TO D-T-MFEE
              MOVE T-DISC(2) TO D-T-DISC
              MOVE T-OTHER(2) TO D-T-OTHER
              MOVE T-CASH-EARN(2) TO D-T-CASH-EARN
              MOVE T-ENDING(2) TO D-T-ENDING
           ELSE
              MOVE T-ACCRUED(2) TO D-T-ACCRUED
              MOVE T-TRUE-ACCRUE(2) TO D-T-TRUE-ACCRUE
              MOVE T-INTBAL(2) TO D-T-INTBAL
              MOVE T-INT-SINCE-LSTPY(2) TO D-T-INT-SINCE-LSTPY.

           PERFORM WRITE-DETAIL-LINE.
           ADD T-NOACCTS(2) TO T-NOACCTS(1).

           IF PRINT-ID = "S" OR "D"
              ADD T-UNEARN(2) TO T-UNEARN(1)
              ADD T-INT(2) TO T-INT(1)
              ADD T-ADDON(2) TO T-ADDON(1)
              ADD T-SRV(2) TO T-SRV(1)
              ADD T-MFEE(2) TO T-MFEE(1)
              ADD T-DISC(2) TO T-DISC(1)
              ADD T-OTHER(2) TO T-OTHER(1)
              ADD T-CASH-EARN(2) TO T-CASH-EARN(1)
              ADD T-ENDING(2) TO T-ENDING(1)
           ELSE
              ADD T-ACCRUED(2) TO T-ACCRUED(1)
              ADD T-TRUE-ACCRUE(2) TO T-TRUE-ACCRUE(1)
              ADD T-INTBAL(2) TO T-INTBAL(1)
              ADD T-INT-SINCE-LSTPY(2) TO T-INT-SINCE-LSTPY(1).

           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2) T-MFEE(2) T-DISC(2) T-OTHER(2)
                     T-CASH-EARN(2) T-ENDING(2)
                     T-ACCRUED(2) T-TRUE-ACCRUE(2) T-INTBAL(2)
                     T-INT-SINCE-LSTPY(2).
      ******************************************************************
      *
      *    B R A N C H - T O T A L S --- FOR AC2ERN.C
      *
      ******************************************************************
      *BRANCH-TOTALS-WT SECTION.

      *    MOVE 2 TO LN-FEED.
      *    MOVE 2 TO SUB.

      *    MOVE "   ---- TOTAL:" TO D-T-DESC.
      *    PERFORM PRINT-BRANCH-SUM-WT.

      *-----------------------------------------------------------------
      *END-BRANCH-TOTALS-WT.

      *    MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
      *              T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
      *              T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
      *              T-ACCRUED(2) T-ENDING(2).

      ******************************************************************
      *
      *    P R I N T - C O R P O R A T E - T O T A L S --- FOR AC2ERN.C
      *
      ******************************************************************
      *PRINT-CORPORATE-TOTS-WT SECTION.

      *    MOVE 2 TO LN-FEED.

      *    MOVE "   ---- TOTAL:" TO D-T-DESC.
      *    MOVE 1 TO SUB.
      *    PERFORM PRINT-BRANCH-SUM-WT.

      *-----------------------------------------------------------------
      *END-CORP-TOTALS-WT.

      *    MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
      *              T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
      *              T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
      *              T-ACCRUED(2) T-ENDING(2).


      ******************************************************************
      *
      *    P R I N T - B R A N C H - S U M M A R Y - T O T A L S
      *        --- FOR AC2ERN.C
      *
      ******************************************************************
       PRINT-BRANCH-SUM-WT SECTION.
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.
           MOVE T-UNEARN(SUB) TO D-T-UNEARN.
           MOVE T-ADDON(SUB) TO D-T-ADDON.
           MOVE T-MFEE(SUB) TO D-T-MFEE.
           MOVE T-OTHER(SUB) TO D-T-OTHER.
           MOVE T-INT(SUB) TO D-T-INT.
           MOVE T-SRV(SUB) TO D-T-SRV.
           MOVE T-DISC(SUB) TO D-T-DISC.
           MOVE T-ACCRUED(SUB) TO D-T-CASH-EARN.
           MOVE T-ENDING(SUB) TO D-T-ENDING.
           PERFORM WRITE-DETAIL-LINE.

           IF T-INT-RB(SUB) = 0
                 AND T-SRV-RB(SUB) = 0
                  AND T-MFEE-RB(SUB) = 0
              NEXT SENTENCE
           ELSE
              MOVE T-INT-RB(SUB) TO D-T-INT
              MOVE T-SRV-RB(SUB) TO D-T-SRV
              MOVE T-MFEE-RB(SUB) TO D-T-MFEE
              PERFORM SET-D-T-R-WT
              PERFORM WRITE-DETAIL-LINE.
      ******************************************************************
      *
      *    S E T - D - T - R - W T
      *
      ******************************************************************
       SET-D-T-R-WT SECTION.

           IF D-T-INT NOT = " "
              IF D-T-INT-R = " "
                 MOVE "R" TO D-T-INT-R.

           IF D-T-SRV NOT = " "
              IF D-T-SRV-R = " "
                 MOVE "R" TO D-T-SRV-R.

           IF D-T-MFEE NOT = " "
              IF D-T-MFEE-R = " "
                 MOVE "R" TO D-T-MFEE-R.

      ******************************************************************
      *
      *    P R I N T - S U M M A R Y --- FOR AC2ERN.C
      *
      ******************************************************************
      *SUMMARY-TOTALS-WT SECTION.

      *    MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
      *              T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
      *              T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
      *              T-ACCRUED(2) T-ENDING(2).

      *    MOVE "Y" TO FIRST-TIME.
      *    PERFORM CLOSE-WT1-FILE.
      *    PERFORM OPEN-WT1-FILE.
      *    MOVE 0 TO WT-BRANCH WT-CLASS.
      *    PERFORM START-WT1-FILE.

      *-----------------------------------------------------------------
      *SUMMARY-NEXT-WT.

      *    PERFORM READ-WT1-FILE-NEXT.

      *    IF IO-BAD
      *       PERFORM BRANCH-TOTALS-WT
      *       GO TO EXIT-SUMMARY-TOTALS-WT.

      *    IF WT-NOACCTS(LEV1) = 0
      *       GO TO SUMMARY-NEXT-WT.

      *    IF FIRST-TIME = "Y"
      *       MOVE WT-BRANCH TO HOLD-BRNO BR-NO
      *       PERFORM READ-BR-FILE
      *       MOVE BR-NO TO D-T-BRNO
      *       MOVE BR-NAME TO D-T-BRDESC
      *       MOVE NEW-BRANCH TO DETAIL-LINE.

      *    IF WT-BRANCH NOT = HOLD-BRNO
      *       PERFORM BRANCH-TOTALS-WT
      *       MOVE 2 TO LN-FEED
      *       MOVE WT-BRANCH TO HOLD-BRNO BR-NO
      *       PERFORM READ-BR-FILE
      *       MOVE BR-NO TO D-T-BRNO
      *       MOVE BR-NAME TO D-T-BRDESC
      *       MOVE NEW-BRANCH TO DETAIL-LINE
      *       MOVE 3 TO LN-FEED
      *       PERFORM WRITE-DETAIL-LINE
      *       MOVE 2 TO LN-FEED.

      *    IF FIRST-TIME = "Y"
      *       MOVE 1 TO LN-FEED
      *       MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
      *       PERFORM WRITE-DETAIL-LINE
      *       MOVE 2 TO LN-FEED
      *       MOVE "N" TO FIRST-TIME.

      *    MOVE WT-CLASS TO LC-CODE.
      *    PERFORM GET-LC.
      *    MOVE LC-CODE TO D-T-CLASS.
      *    MOVE WT-NOACCTS(LEV1) TO D-T-NOACCTS.
      *    MOVE WT-UNEARN(LEV1) TO D-T-UNEARN.
      *    MOVE WT-INT(LEV1) TO D-T-INT.
      *    MOVE WT-ADDON(LEV1) TO D-T-ADDON.
      *    MOVE WT-SRV(LEV1) TO D-T-SRV.
      *    MOVE WT-MFEE(LEV1) TO D-T-MFEE.
      *    MOVE WT-DISC(LEV1) TO D-T-DISC.
      *    MOVE WT-OTHER(LEV1) TO D-T-OTHER.
      *    MOVE WT-ACCRUED(LEV1) TO D-T-CASH-EARN.
      *    MOVE WT-ENDING(LEV1) TO D-T-ENDING.
      *    PERFORM WRITE-DETAIL-LINE.

      *    IF WT-INT-RB(LEV1) = 0
      *          AND WT-SRV-RB(LEV1) = 0
      *           AND WT-MFEE-RB(LEV1)= 0
      *       MOVE 2 TO LN-FEED
      *    ELSE
      *       MOVE WT-INT-RB(LEV1) TO D-T-INT
      *       MOVE WT-SRV-RB(LEV1) TO D-T-SRV
      *       MOVE WT-MFEE-RB(LEV1) TO D-T-MFEE
      *       PERFORM SET-D-T-R-WT
      *       PERFORM WRITE-DETAIL-LINE
      *       MOVE 2 TO LN-FEED.

      *    ADD WT-NOACCTS(LEV1) TO T-NOACCTS(2).
      *    ADD WT-UNEARN(LEV1) TO T-UNEARN(2).
      *    ADD WT-INT(LEV1) TO T-INT(2).
      *    ADD WT-INT-RB(LEV1) TO T-INT-RB(2).
      *    ADD WT-ADDON(LEV1) TO T-ADDON(2).
      *    ADD WT-SRV(LEV1) TO T-SRV(2).
      *    ADD WT-SRV-RB(LEV1) TO T-SRV-RB(2).
      *    ADD WT-MFEE(LEV1) TO T-MFEE(2).
      *    ADD WT-MFEE-RB(LEV1) TO T-MFEE-RB(2).
      *    ADD WT-DISC(LEV1) TO T-DISC(2).
      *    ADD WT-OTHER(LEV1) TO T-OTHER(2).
      *    ADD WT-ACCRUED(LEV1) TO T-ACCRUED(2).
      *    ADD WT-ENDING(LEV1) TO T-ENDING(2).
      *    GO TO SUMMARY-NEXT-WT.

      *EXIT-SUMMARY-TOTALS-WT.
      *    EXIT.

      ******************************************************************
      *
      *    C O R P O R A T E - T O T A L S --- FOR AC2ERN.C
      *
      ******************************************************************
      *CORPORATE-TOTALS-WT SECTION.

      *    MOVE 0 TO T-NOACCTS(1) T-UNEARN(1) T-INT(1) T-INT-RB(1)
      *              T-ADDON(1) T-SRV(1) T-SRV-RB(1) T-MFEE(1)
      *              T-MFEE-RB(1) T-DISC(1) T-OTHER(1)
      *              T-ACCRUED(1) T-ENDING(1).
      *    MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
      *              T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
      *              T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
      *              T-ACCRUED(2) T-ENDING(2).

      *    MOVE "Y" TO FIRST-TIME.
      *    PERFORM CLOSE-WT1-FILE.
      *    PERFORM OPEN-WT2-FILE.
      *    MOVE 0 TO WT-CLASS.
      *    PERFORM START-WT2-FILE.

      *-----------------------------------------------------------------
      *CORPORATE-NEXT-WT.

      *    PERFORM READ-WT2-FILE-NEXT.

      *    IF IO-BAD
      *       PERFORM PRINT-CORPORATE-WT
      *       PERFORM PRINT-CORPORATE-TOTS-WT
      *       GO TO END-CORPORATE-TOTALS-WT.

      *    IF WT-NOACCTS(LEV1) = 0
      *       GO TO CORPORATE-NEXT-WT.

      *    IF FIRST-TIME = "Y"
      *       MOVE 3 TO LN-FEED
      *       MOVE "CORPORATE TOTALS:" TO DETAIL-LINE
      *       PERFORM WRITE-DETAIL-LINE
      *       MOVE 2 TO LN-FEED
      *       MOVE WT-CLASS TO HOLD-CLASS
      *       MOVE "N" TO FIRST-TIME.

      *    IF WT-CLASS NOT = HOLD-CLASS
      *       PERFORM PRINT-CORPORATE-WT
      *       MOVE WT-CLASS TO HOLD-CLASS.

      *    ADD WT-NOACCTS(LEV1) TO T-NOACCTS(2).
      *    ADD WT-UNEARN(LEV1) TO T-UNEARN(2).
      *    ADD WT-INT(LEV1) TO T-INT(2).
      *    ADD WT-INT-RB(LEV1) TO T-INT-RB(2).
      *    ADD WT-ADDON(LEV1) TO T-ADDON(2).
      *    ADD WT-SRV(LEV1) TO T-SRV(2).
      *    ADD WT-SRV-RB(LEV1) TO T-SRV-RB(2).
      *    ADD WT-MFEE(LEV1) TO T-MFEE(2).
      *    ADD WT-MFEE-RB(LEV1) TO T-MFEE-RB(2).
      *    ADD WT-DISC(LEV1) TO T-DISC(2).
      *    ADD WT-OTHER(LEV1) TO T-OTHER(2).
      *    ADD WT-ACCRUED(LEV1) TO T-ACCRUED(2).
      *    ADD WT-ENDING(LEV1) TO T-ENDING(2).
      *    GO TO CORPORATE-NEXT-WT.

      *-----------------------------------------------------------------
      *END-CORPORATE-TOTALS-WT.
      *    PERFORM CLOSE-WT2-FILE.

      ******************************************************************
      *
      *    P R I N T - C O R P O R A T E - T O T A L S --- FOR AC2ERN.C
      *
      ******************************************************************
      *PRINT-CORPORATE-WT SECTION.

      *    MOVE HOLD-CLASS TO LC-CODE.
      *    PERFORM GET-LC.
      *    MOVE LC-CODE TO D-T-CLASS.
      *    MOVE T-NOACCTS(2) TO D-T-NOACCTS.
      *    MOVE "#" TO D-T-TAG.
      *    MOVE T-UNEARN(2) TO D-T-UNEARN.
      *    MOVE T-INT(2) TO D-T-INT.
      *    MOVE T-ADDON(2) TO D-T-ADDON.
      *    MOVE T-SRV(2) TO D-T-SRV.
      *    MOVE T-MFEE(2) TO D-T-MFEE.
      *    MOVE T-DISC(2) TO D-T-DISC.
      *    MOVE T-OTHER(2) TO D-T-OTHER.
      *    MOVE T-ACCRUED(2) TO D-T-CASH-EARN.
      *    MOVE T-ENDING(2) TO D-T-ENDING.
      *    PERFORM WRITE-DETAIL-LINE.

      *    IF T-INT-RB(SUB) = 0
      *          AND T-SRV-RB(SUB) = 0
      *           AND T-MFEE-RB(SUB) = 0
      *       MOVE 2 TO LN-FEED
      *    ELSE
      *       MOVE T-INT-RB(SUB) TO D-T-INT
      *       MOVE T-SRV-RB(SUB) TO D-T-SRV
      *       MOVE T-MFEE-RB(SUB) TO D-T-MFEE
      *       PERFORM SET-D-T-R-WT
      *       PERFORM WRITE-DETAIL-LINE
      *       MOVE 2 TO LN-FEED.

      *    ADD T-NOACCTS(2) TO T-NOACCTS(1).
      *    ADD T-UNEARN(2) TO T-UNEARN(1).
      *    ADD T-INT(2) TO T-INT(1).
      *    ADD T-INT-RB(2) TO T-INT-RB(1).
      *    ADD T-ADDON(2) TO T-ADDON(1).
      *    ADD T-SRV(2) TO T-SRV(1).
      *    ADD T-SRV-RB(2) TO T-SRV-RB(1).
      *    ADD T-MFEE(2) TO T-MFEE(1).
      *    ADD T-MFEE-RB(2) TO T-MFEE-RB(1).
      *    ADD T-DISC(2) TO T-DISC(1).
      *    ADD T-OTHER(2) TO T-OTHER(1).
      *    ADD T-ACCRUED(2) TO T-ACCRUED(1).
      *    ADD T-ENDING(2) TO T-ENDING(1).

      *    MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
      *              T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
      *              T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
      *              T-ACCRUED(2) T-ENDING(2).

      ******************************************************************
      *
      *    G T T - C O M P I L E - G R O U P - T O T A L S
      *
      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      *-----------------------------------------------------------------
       GTT-START-WK1.
           MOVE 0 TO WK-CLASS.
           MOVE WGR-SRESULT TO WK-BRANCH.
           PERFORM START-WK1-FILE.

       GTT-NEXT-WK1.

           PERFORM READ-WK1-FILE-NEXT.

           IF IO-BAD
              GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.
           IF WK-BRANCH NOT = WGR-SRESULT
              GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ******************************************************************
      *    G T T - A D D - T O T A L S
      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      *-----------------------------------------------------------------
       GTT-ADD-TOTS.
           IF WK-CLASS = 0
              GO TO EXIT-GTT-ADD-TOTALS.
           MOVE WK-CLASS TO CLSUB.
           PERFORM GTT-ADD-UNEARNED.
           MOVE 100 TO CLSUB.
           PERFORM GTT-ADD-UNEARNED.
           GO TO EXIT-GTT-ADD-TOTALS.
       GTT-ADD-UNEARNED.
           ADD WK-NOACCTS(1) TO WS-GTT-NOACCTS(GTT-TOTAL-SUB CLSUB).
           ADD WK-UNEARN     TO WS-GTT-UNEARN(GTT-TOTAL-SUB CLSUB).
           ADD WK-INT        TO WS-GTT-INT(GTT-TOTAL-SUB CLSUB).
           ADD WK-ADDON      TO WS-GTT-ADDON(GTT-TOTAL-SUB CLSUB).
           ADD WK-SRV        TO WS-GTT-SRV(GTT-TOTAL-SUB CLSUB).
           ADD WK-MFEE       TO WS-GTT-MFEE(GTT-TOTAL-SUB CLSUB).
           ADD WK-DISC       TO WS-GTT-DISC(GTT-TOTAL-SUB CLSUB).
           ADD WK-OTHER      TO WS-GTT-OTHER(GTT-TOTAL-SUB CLSUB).
           ADD WK-CASH-EARN  TO WS-GTT-CASH-EARN(GTT-TOTAL-SUB CLSUB).
           ADD WK-ENDING     TO WS-GTT-ENDING(GTT-TOTAL-SUB CLSUB).
      *=================================================================
       EXIT-GTT-ADD-TOTALS.
           EXIT.
      ******************************************************************
      *
      *    G T T - P R I N T - G R O U P - B R - T O T A L S
      *
      ******************************************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      *-----------------------------------------------------------------
       GTT-PRT-GR-BR-TOTALS.

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES       TO DETAIL-LINE.
           MOVE 9            TO GTT-TOTAL-SUB.
           MOVE "BRANCH #: " TO D-TYPE.
           MOVE BR-NO        TO D-BRNO.
           MOVE BR-NAME      TO D-BRDESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES       TO DETAIL-LINE.

           PERFORM GTT-PRINT-TOTALS.

      *=================================================================
       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    G T T - P R I N T - G R O U P - T O T A L S
      *
      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      *-----------------------------------------------------------------
       GTT-PRT-GR-TOTALS.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP ID: " TO D-TYPE.
           MOVE GR-ID        TO D-GRID.
           MOVE GR-NAME      TO D-BRDESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES       TO DETAIL-LINE.

           PERFORM GTT-PRINT-TOTALS.

      *=================================================================
       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    G T T - P R I N T - T O T A L S
      *
      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      *-----------------------------------------------------------------
       GTT-PRT-TOTALS-NEXT.
           PERFORM VARYING CLSUB FROM 1 BY 1 UNTIL CLSUB > 100
             IF WS-GTT-NOACCTS(GTT-TOTAL-SUB, CLSUB) > 0
                 IF CLSUB < 100
                    MOVE CLSUB TO D-T-CLASS
                 ELSE
                    IF GTT-TOTAL-SUB = 9
                       MOVE "BRANCH TOTALS:" TO D-T-DESC
                    ELSE
                       MOVE "&&&& TOTALS:" TO D-T-DESC
                       INSPECT D-T-DESC REPLACING FIRST "&&&&" BY GR-ID
                    END-IF
                 END-IF
                 MOVE WS-GTT-NOACCTS(GTT-TOTAL-SUB CLSUB) TO D-T-NOACCTS
                 MOVE WS-GTT-UNEARN(GTT-TOTAL-SUB CLSUB)  TO D-T-UNEARN
                 MOVE WS-GTT-INT(GTT-TOTAL-SUB CLSUB)     TO D-T-INT
                 MOVE WS-GTT-ADDON(GTT-TOTAL-SUB CLSUB)   TO D-T-ADDON
                 MOVE WS-GTT-SRV(GTT-TOTAL-SUB CLSUB)     TO D-T-SRV
                 MOVE WS-GTT-MFEE(GTT-TOTAL-SUB CLSUB)    TO D-T-MFEE
                 MOVE WS-GTT-DISC(GTT-TOTAL-SUB CLSUB)    TO D-T-DISC
                 MOVE WS-GTT-OTHER(GTT-TOTAL-SUB CLSUB)   TO D-T-OTHER
                 MOVE WS-GTT-CASH-EARN(GTT-TOTAL-SUB CLSUB)
                                                       TO D-T-CASH-EARN
                 MOVE WS-GTT-ENDING(GTT-TOTAL-SUB, CLSUB) TO D-T-ENDING
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - CLEAR LEVEL IN TABLE  - - -
           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

      *=================================================================
       EXIT-GTT-PRINT-TOTALS.
           EXIT.
      ******************************************************************
      *
      *   C R E A T E - G T - F I L E
      *
      ******************************************************************
       CREATE-GT1-DETAIL SECTION.
           MOVE 1 TO LEV.

           PERFORM OPEN-GT1-FILE.

           MOVE 0 TO WK-BRANCH WT-CLASS HOLD-BRNO.
           PERFORM OPEN-WK1-FILE.
           PERFORM START-WK1-FILE.

           MOVE "UPDATING GL TRANSACTION FILE          " TO
                     ERRLOGW-EOM-MSG MESS.
           PERFORM LOG-ERROR.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND2.

      *-----------------------------------------------------------------
       NEXT-WK.

           PERFORM READ-WK1-FILE-NEXT.
           IF NOT IO-GOOD
              GO TO EXIT-CREATE-GT1-DETAIL.

           IF WK-NOACCTS(1) = 0 AND WK-NOACCTS(2) = 0
              GO TO NEXT-WK.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *---- REVERSE LAST MONTHS ESTIMATE OF IB EARNINGS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF WK-BRANCH NOT = HOLD-BRNO
      *       PERFORM UPDATE-BY-LAST
              MOVE WK-BRANCH TO HOLD-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * MUST GET NEW GI-REC HERE, CANNOT RELY ON NUMBER
      * COMMING OUT OF UPDATE-BY-LAST.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE WK-BRANCH TO GI-BRANCH.
           MOVE WK-CLASS TO GI-CLASS.
           PERFORM GET-GI.
           IF NOT IO-GOOD
              MOVE "GI-REC NOT FOUND (REV) FOR BR:&&&& CLASS:&&&"
                    TO ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG
                    REPLACING FIRST "&&&&" BY GI-BRANCH
              INSPECT ERRLOGW-EOM-MSG
                    REPLACING FIRST "&&&" BY GI-CLASS
              PERFORM LOG-ERROR
              GO TO NEXT-WK.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF WK-NOACCTS(1) NOT = 0
                 IF WK-GL-AMT(SUB1) NOT = 0
                    MOVE WK-BRANCH TO WORKER-BRANCH
                    MOVE WK-GL-AMT(SUB1) TO WORKER-GL-AMT
                    PERFORM UPDATE-GT-DETAIL
                 END-IF
              END-IF
           END-PERFORM.

           IF WK-NOACCTS(2) NOT = 0
              IF WK-ACCRUED NOT = 0
                 MOVE 0 TO SUB1
                 MOVE GI-ACR-INT-AR TO HOLD-CREDIT-GL
                 MOVE GI-ACR-INT-AR-AR TO HOLD-DEBIT-GL
                 MOVE WK-BRANCH TO WORKER-BRANCH
                 MOVE WK-ACCRUED TO WORKER-GL-AMT
                 PERFORM UPDATE-GT-DETAIL
                 PERFORM UPDATE-BY-DETAIL.

           GO TO NEXT-WK.

      *-----------------------------------------------------------------
       EXIT-CREATE-GT1-DETAIL.

           PERFORM CLOSE-GT1-FILE.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

      ******************************************************************
      *
      *    U P D A T E - G T - F I L E
      *
      ******************************************************************
       UPDATE-GT-DETAIL SECTION.

           PERFORM CLEAR-GT-FILE.

           MOVE 1 TO SAVE-GT-LINE.

           MOVE " "              TO GT-FLAG.
           MOVE "LP-&&&&&&-&&&&" TO GT-REF.

           MOVE MONTH-END TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           INSPECT GT-REF REPLACING FIRST "&&&&&&" BY DATE-YYMMDD.
           INSPECT GT-REF REPLACING FIRST "&&&&" BY WORKER-BRANCH.

           MOVE EOM-DATE          TO DATE-YYYYMMDD.

           MOVE DATE-YYYYMMDD     TO GT-DATE1 GT-DATE2 GT-DATE3.

           MOVE 3 TO GT-SOURCE.

      * IF SUB1 = 0, ITS THE IB ACCRUAL GL'S, ELSE ITS THE EARNED 1-6
      * EARNINGS COMPONENTS.  

           IF SUB1 = 0
              MOVE HOLD-CREDIT-GL    TO HOLD-GL-BR-ACCTNO
           ELSE
              MOVE GI-EARN(SUB1)     TO HOLD-GL-BR-ACCTNO.

           MOVE HOLD-GL-BRANCH TO GT-BRANCH.
           MOVE HOLD-GL-ACCTNO TO GT-ACCTNO.
           COMPUTE GT-AMT = -1 * WORKER-GL-AMT.

           IF GT-AMT > 0
              MOVE "1" TO GT-DRCRFG
           ELSE
              MOVE "2" TO GT-DRCRFG.

           MOVE "EOM - ACCRUALS" TO GT-EXPL.
           MOVE 0 TO GT-PERIOD.

       UPDATE-GT-CREDIT.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE    TO GT-POST-DATE.
           MOVE POST-TIME    TO WORK-TIME.
           MOVE TIME-HHMM    TO GT-POST-TIME.
           MOVE POST-USERID  TO GT-POST-USERID.
           MOVE SAVE-GT-LINE TO GT-LINE.
           PERFORM WRITE-GT1-FILE. *> EMBEDDED - GTFILE

           IF IO-GOOD
              GO TO  CONT-UPDATE-GT.

           IF GT-LINE < 999999
              ADD 1 TO SAVE-GT-LINE
              GO TO UPDATE-GT-CREDIT.

           MOVE "ERROR WRITING GT-FILE &&&&&&&&&&&" TO ERRLOGW-EOM-MSG.
           INSPECT ERRLOGW-EOM-MSG
              REPLACING FIRST "&&&&&&&&&&&" BY HOLD-GL-BR-ACCTNO.

           PERFORM LOG-ERROR.
           GO TO EXIT-UPDATE-GT-DETAIL.

       CONT-UPDATE-GT.

      * IF SUB1 = 0, ITS THE IB ACCRUAL GL'S, ELSE ITS THE UNEARNED 1-6
      * EARNINGS COMPONENTS.  

           MOVE 1 TO SAVE-GT-LINE.

           IF SUB1 = 0
              MOVE HOLD-DEBIT-GL   TO HOLD-GL-BR-ACCTNO
           ELSE
              MOVE GI-UNEARN(SUB1) TO HOLD-GL-BR-ACCTNO.
             
           MOVE HOLD-GL-ACCTNO    TO GT-ACCTNO.
           MOVE HOLD-GL-BRANCH TO GT-BRANCH.
           MOVE WORKER-GL-AMT TO GT-AMT.

           IF GT-AMT > 0
              MOVE "1" TO GT-DRCRFG
           ELSE
              MOVE "2" TO GT-DRCRFG.

       UPDATE-GT-DEBIT.

           PERFORM SET-POSTDATE.
           MOVE POST-DATE    TO GT-POST-DATE.
           MOVE POST-TIME    TO WORK-TIME.
           MOVE TIME-HHMM    TO GT-POST-TIME.
           MOVE POST-USERID  TO GT-POST-USERID.
           MOVE SAVE-GT-LINE TO GT-LINE.
           PERFORM WRITE-GT1-FILE. *> EMBEDDED - GTFILE
           IF IO-GOOD
              GO TO EXIT-UPDATE-GT-DETAIL.

           IF GT-LINE < 999999
              ADD 1 TO SAVE-GT-LINE
              GO TO UPDATE-GT-DEBIT.

           MOVE "ERROR WRITING GT-FILE &&&&&&&&&&&" TO ERRLOGW-EOM-MSG.

           INSPECT ERRLOGW-EOM-MSG
              REPLACING FIRST "&&&&&&&&&&&" BY HOLD-GL-BR-ACCTNO.

           PERFORM LOG-ERROR.

       EXIT-UPDATE-GT-DETAIL.
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - B Y - F I L E - L A S T
      *        --- REVERSE LAST MONTHS IB ESTIMATES IN GL.
      *
      ******************************************************************
       UPDATE-BY-LAST SECTION.

           PERFORM OPEN-GT1-FILE.
           PERFORM OPEN-BY1-FILE.

           MOVE LAST-EOM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM   TO HOLD-BY-DATE-CCYYMM BY-DATE-CCYYMM
                                 QBY1-WBEG-DATE-CCYYMM
                                 QBY1-WEND-DATE-CCYYMM.

           MOVE BR-NO   TO BY-BRNO
                           QBY1-WBEG-BRNO
                           QBY1-WEND-BRNO.

           MOVE 0       TO BY-CLASS
                           QBY1-WBEG-CLASS.
           MOVE ALL "9" TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE. *> EMBEDDED - BYFILE                  

      *-----------------------------------------------------------------
       NEXT-BY.

           PERFORM READ-BY1-FILE-NEXT. *> EMBEDDED - BYFILE                    
           IF IO-BAD
              GO TO EXIT-UPDATE-BY-LAST.

           IF BY-DATE-CCYYMM > HOLD-BY-DATE-CCYYMM
              GO TO EXIT-UPDATE-BY-LAST.

           IF BY-BRNO NOT = BR-NO
              GO TO NEXT-BY.

           IF BY-IB-ACCRUED = 0
              GO TO NEXT-BY.

      *       GET NEW GI RECORD FOR EACH BRANCH/CLASS.
           MOVE BY-BRNO TO GI-BRANCH.
           MOVE BY-CLASS TO GI-CLASS.
           PERFORM GET-GI.
           IF NOT IO-GOOD
              MOVE "GI-REC NOT FOUND FOR BR:&&&& CLASS:&&"
                    TO ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG
                    REPLACING FIRST "&&&&" BY GI-BRANCH
              INSPECT ERRLOGW-EOM-MSG
                    REPLACING FIRST "&&&" BY GI-CLASS
              PERFORM LOG-ERROR
              GO TO NEXT-BY.

           MOVE 0 TO SUB1.

      *REVERSING PRIOR MONTH I.B. ACCRUAL CREDIT ENTRY
      *GI-ACR-INT-AR-AR                     
           MOVE GI-ACR-INT-AR-AR TO HOLD-CREDIT-GL.

      *REVERSING PRIOR MONTH I.B. ACCRUAL DEBIT ENTRY
      *GI-ACR-INT-AR
           MOVE GI-ACR-INT-AR   TO HOLD-DEBIT-GL.
           MOVE BY-IB-ACCRUED   TO WORKER-GL-AMT.
           MOVE BY-BRNO         TO WORKER-BRANCH.

           PERFORM UPDATE-GT-DETAIL.

           GO TO NEXT-BY.

      *-----------------------------------------------------------------
       EXIT-UPDATE-BY-LAST.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-GT1-FILE.

      ******************************************************************
      *
      *    U P D A T E - B Y - D E T A I L
      *        REWRITE BY WITH CURRENT MONTHS IB/ACCRUALS.
      *        IF BY FILE DOES NOT EXIST, CREATE ONE AND CREATE
      *        ERROR LOG ENTRY.
      *
      ******************************************************************
       UPDATE-BY-DETAIL SECTION.

           MOVE WK-BRANCH TO BY-BRNO.
           MOVE WK-CLASS TO BY-CLASS.
           MOVE EOM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO HOLD-BY-DATE-CCYYMM BY-DATE-CCYYMM.

           PERFORM OPEN-BY1-FILE.
           PERFORM READ-BY1-FILE. *> EMBEDDED - BYFILE

           IF IO-BAD
              MOVE "ERROR: NO BY-REC FOR &&&&&&&&&&&&&: WRITING NEW"
                 TO ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG
                 REPLACING FIRST "&&&&&&" BY BY-DATE-CCYYMM
              INSPECT ERRLOGW-EOM-MSG
                 REPLACING FIRST "&&&&" BY BY-BRNO
              INSPECT ERRLOGW-EOM-MSG
                 REPLACING FIRST "&&&" BY BY-CLASS
              PERFORM LOG-ERROR
              PERFORM CLEAR-BY-FILE
              MOVE WK-BRANCH TO BY-BRNO
              MOVE WK-CLASS TO BY-CLASS
              MOVE HOLD-BY-DATE-CCYYMM TO BY-DATE-CCYYMM
              MOVE WK-ACCRUED TO BY-IB-ACCRUED
              PERFORM WRITE-BY1-FILE *> EMBEDDED - BYFILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR
              END-IF
              GO TO EXIT-UPDATE-BY-DETAIL.

           MOVE WK-ACCRUED TO BY-IB-ACCRUED.

           PERFORM REWRITE-BY1-FILE. *> EMBEDDED - BYFILE
           IF NOT IO-GOOD
              GO TO IO-ERROR.
      *-----------------------------------------------------------------
       EXIT-UPDATE-BY-DETAIL.
           PERFORM CLOSE-BY1-FILE.
      ******************************************************************
      *
      *    H E A D - P A G E
      *
      ******************************************************************
       HEAD-PAGE SECTION.
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

           MOVE SUMMARY1 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE SUMMARY2 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE SUMMARY3 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *-----------------------------------------------------------------
       END-HEAD-PAGE.
           EXIT.
      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
      *
      *    L O G - E R R O R --- IN GLOBAL LG DIRECTORY
      *
      ******************************************************************
       LOG-ERROR SECTION.

           MOVE "A" TO ERRLOGW-ACTION.
           MOVE BR-NO TO ERRLOGW-BRNO.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *****************************************************************
      *    CREATE SPOOL DIRECTORY
      *****************************************************************
       CREATE-SPOOL-DIR SECTION.
           MOVE "ACR"       TO EO-SPOOL-PCODE.
           MOVE EOM-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE.
           MOVE " "         TO EO-SPOOL-FILE.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              IF STAT NOT = "00"
                 GO TO EXIT-CREATE-SPOOL-DIR.

           PERFORM SETUP-REPORT-PATH.

       EXIT-CREATE-SPOOL-DIR.
           EXIT.

      *****************************************************************
      * THEY MAY RUN THE ACCRUAL MORE THAN ONCE FOR THE SAME MONTH,
      * SO CHECK TO SEE IF REPORT EXISTS, IF SO, INCREMENT THE STEP #
      * SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
       SETUP-REPORT-PATH SECTION.
           MOVE "/"        TO EO-SPOOL-SLASH.
           MOVE PRINT-SPOOL-NAME TO EO-SPOOL-NAME.
           MOVE 1 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EO-SPOOL-STEP.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "00" TO STAT
              GO TO EXIT-SETUP-REPORT-PATH.
           IF STEP-COUNT < 999
              ADD 1 TO STEP-COUNT
              GO TO TRY-NEXT-STEP-NO.
       EXIT-SETUP-REPORT-PATH.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GBRCCL.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPBYCL.CPY".
       COPY "LIBGL/GLGTCL.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO ACRERN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ACRERN-SYSDATE.
           DISPLAY ACRERN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ACRERN-FORM-FIELDS TO WR-BUF.
           MOVE ACRERN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE ACRERN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ACRERN-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ACRERN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *----------------------------------------------------------------
       COMPUTE-ACCRUALS.

      * 10/21/2021 COMMENTED OUT, AC3ERN DOES NOT EXIST ANYMORE
      *    IF EXT-EOBUF-ERRCD = "C" AND IB-ACCRUALS-DONE = " "
      *       MOVE "MONTHLY NON ACCRUAL EARNINGS UPDATE" TO TITLEX
      *       MOVE "LP/AC3ERN" TO FORM-NAM
      *    ELSE

      D    STOP MESS.

           MOVE "LP/AC1ERN" TO FORM-NAM.

           MOVE BR-NO TO HOLD-BR.
           MOVE END-DATE-MMYY TO HOLD-DATE.

           MOVE SCREEN-BUF TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           CALL FORM-PROGX USING FORM-PATH IB-ACCRUALS-DONE AC1-WORKER 
                                                EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       SET-PRINT-S.
           MOVE "UNERNS"     TO PRINT-SPOOL-NAME.
           PERFORM SETUP-REPORT-PATH.
           MOVE EO-SPOOL TO PRU-FILE.
           MOVE "S"          TO PRINT-ID.

           MOVE UN-TITLE     TO H-TITLE.
           MOVE UN-MESS      TO H-MESS.
           MOVE WK-SUMMARY1  TO SUMMARY1.
           MOVE WK-SUMMARY2  TO SUMMARY2.
           MOVE WK-SUMMARY3  TO SUMMARY3.

      *----------------------------------------------------------------
       SET-PRINT-IB-S.
           MOVE "IBACCS"     TO PRINT-SPOOL-NAME.
           PERFORM SETUP-REPORT-PATH.
           MOVE EO-SPOOL TO PRU-FILE.
           MOVE "T"          TO PRINT-ID.

           MOVE IB-SUMMARY2  TO SUMMARY2.
           MOVE IB-SUMMARY3  TO SUMMARY3.
           MOVE IB-TITLE     TO H-TITLE.
           MOVE IB-MESS      TO H-MESS.

      *----------------------------------------------------------------
       SET-PRINT-S-WT.

           PERFORM CLOSE-PR-FILE.
           MOVE "NONACS"    TO PRINT-SPOOL-NAME.
           PERFORM SETUP-REPORT-PATH.
           MOVE EO-SPOOL TO PRU-FILE.
           MOVE "S"          TO PRINT-ID.

           MOVE WT-SUMMARY2  TO SUMMARY2.
           MOVE WT-SUMMARY3  TO SUMMARY3.
           MOVE "          9.99R = AMOUNT OF REFUND" TO H-RMEMO.
           MOVE "    NON ACCRUAL EARNINGS REPORT    MONTH ENDING: "
                             TO H-TITLE.

           PERFORM OPEN-PR-FILE.

      *----------------------------------------------------------------
       SET-PRINT-PL-S-WT.

           PERFORM CLOSE-PR-FILE.
           MOVE "PLUNES"    TO PRINT-SPOOL-NAME.
           PERFORM SETUP-REPORT-PATH.
           MOVE EO-SPOOL TO PRU-FILE.
           MOVE "T"          TO PRINT-ID.

           MOVE
           "P&L UNEARNED INCOME REPORT          MONTH ENDING "
                                                  TO H-TITLE.
           MOVE SPACES       TO H-RMEMO.
           MOVE WT-SUMMARY2-2PL TO WT-SUMMARY2-2.
           MOVE WT-SUMMARY2-3PL TO WT-SUMMARY2-3.
           MOVE " " TO WT-SUMMARY2-4 WT-SUMMARY3-1.
           MOVE WT-SUMMARY1 TO SUMMARY1.
           MOVE WT-SUMMARY2 TO SUMMARY2.
           MOVE WT-SUMMARY3 TO SUMMARY3.

           PERFORM OPEN-PR-FILE.

      *----------------------------------------------------------------
       GET-LC.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.

           IF IO-BAD
              MOVE "*** NOT FOUND ***" TO LC-DESC.

           PERFORM CLOSE-LC1-FILE.

      *----------------------------------------------------------------
       GET-GI.

           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.

      *----------------------------------------------------------------
       SET-USER-PATH.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE TO PRINT-ALL-BASE.
           MOVE BR-MACHINE       TO PRINT-ALL-MACHINE.

      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
       SEND-LEGEND2.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
      *----------------------------------------------------------------
       SEND-PREPROCESSOR.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
      *----------------------------------------------------------------

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBGB/GBRC1IN.CPY".
      * COPYMEMBER: LIBGB/GBRC1IN
       LOAD-RC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *-----------------------------------------------------------------
       OPEN-RC1-FILE.
           PERFORM LOAD-RC1-FILE.
           PERFORM OPEN-IT.
           MOVE RC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RC1-FILE.
           PERFORM START-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           IF ( RC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RC1-FILE.

           IF ( QRC1-WEND-BRNO = ZEROES    ) OR
              ( QRC1-WEND-BRNO NOT NUMERIC )
              MOVE RC-BRNO              TO QRC1-WBEG-BRNO
              MOVE ALL "9"              TO QRC1-WEND-BRNO

              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WEND-TRANS-DATE.

           EXEC SQL
            DECLARE RC1_CURSOR CURSOR FOR
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO >= :QRC1-WBEG-BRNO
              AND RCFILE.RC_BRNO <= :QRC1-WEND-BRNO
              AND RCFILE.RC_TRANS_DATE >= :QRC1-WBEG-TRANS-DATE 
              AND RCFILE.RC_TRANS_DATE <= :QRC1-WEND-TRANS-DATE
            ORDER BY RCFILE.RC_BRNO,
                     RCFILE.RC_TRANS_DATE
           END-EXEC.

           EXEC SQL
               OPEN RC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRC1-WBEG-BRNO
                          QRC1-WEND-BRNO.

           MOVE SPACES TO QRC1-WBEG-TRANS-DATE
                          QRC1-WEND-TRANS-DATE.

           MOVE STAT---GOOD TO RC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-RC1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           IF ( RC1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-RC1-FILE.

           IF ( QRC1-WEND-BRNO = ZEROES    ) OR
              ( QRC1-WEND-BRNO NOT NUMERIC )
              MOVE ALL "0"              TO QRC1-WBEG-BRNO
              MOVE RC-BRNO              TO QRC1-WEND-BRNO

              MOVE EXT-JULIAN-BEG       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE
              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WEND-TRANS-DATE.

           EXEC SQL
            DECLARE RC1_CURSOR_NG CURSOR FOR
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)), 
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO >= :QRC1-WBEG-BRNO
              AND RCFILE.RC_BRNO <= :QRC1-WEND-BRNO
              AND RCFILE.RC_TRANS_DATE >= :QRC1-WBEG-TRANS-DATE 
              AND RCFILE.RC_TRANS_DATE <= :QRC1-WEND-TRANS-DATE
            ORDER BY RCFILE.RC_BRNO DESC,
                     RCFILE.RC_TRANS_DATE DESC
           END-EXEC.

           EXEC SQL
               OPEN RC1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRC1-WBEG-BRNO
                          QRC1-WEND-BRNO.

           MOVE SPACES TO QRC1-WBEG-TRANS-DATE
                          QRC1-WEND-TRANS-DATE.

           MOVE STAT---GOOD TO RC1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-RC1-FILE.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           MOVE RC-BRNO          TO QRC-BRNO.  
           MOVE RC-TRANS-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC-TRANS-DATE.
           MOVE RC-STATUS        TO QRC-STATUS.

           EXEC SQL
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            INTO :QRC-REC
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       READ-RC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC1_CURSOR INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       READ-RC1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH RC1_CURSOR_NG INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       WRITE-RC1-FILE.

           ACCEPT RC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           PERFORM SET-RC-FIELDS.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. 
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.RCFILE
             (RCFILE.RC_STATUS,
              RCFILE.RC_BRNO,
              RCFILE.RC_TRANS_DATE,
              RCFILE.RC_LTOUCH_DATE,
              RCFILE.RC_RECAP_FG,
              RCFILE.RC_GL_FG,
              RCFILE.RC_EOD_BATCH_FG,
              RCFILE.RC_NIGHTLY_FUNDS_FG,
              RCFILE.RC_EOM1_BATCH_FG,
              RCFILE.RC_EOM_UPDATE_FG,
              RCFILE.RC_EOM2_BATCH_FG,
              RCFILE.RC_BS_P2P3_INBAL_FG,
              RCFILE.RC_DEPOSIT,
              RCFILE.RC_OVER_SHORT,
              RCFILE.RC_CHECKS,
              RCFILE.RC_OPENED_DATE,
              RCFILE.RC_OPENED_TIME,
              RCFILE.RC_OP_USERID,
              RCFILE.RC_CLOSED_DATE,
              RCFILE.RC_CLOSED_TIME,
              RCFILE.RC_CL_USERID,
              RCFILE.RC_UPDATED_DATE,
              RCFILE.RC_UPDATED_TIME,
              RCFILE.RC_UPDATED_USERID,
              RCFILE.RC_EOD_BATCH_DATE,
              RCFILE.RC_EOD_BATCH_TIME,
              RCFILE.RC_FUNDS_DATE,
              RCFILE.RC_FUNDS_TIME,
              RCFILE.RC_EARNINGS_DATE,
              RCFILE.RC_EARNINGS_TIME,
              RCFILE.RC_EOM_UPDATED_DATE,
              RCFILE.RC_EOM_UPDATED_TIME,
              RCFILE.RC_EOM2_BATCH_DATE,
              RCFILE.RC_EOM2_BATCH_TIME,
              RCFILE.RC_CHECK_DATE,
              RCFILE.RC_CHECK_TIME,
              RCFILE.RC_LAST_ACCRUE,
              RCFILE.RC_ERROR_MSG,
              RCFILE.RC_BANK_BAL,
              RCFILE.RC_CHECK_DEPOSIT)
            VALUES
             (:QRC-STATUS,
              :QRC-BRNO,
              :QRC-TRANS-DATE,
              :QRC-LTOUCH-DATE,
              :QRC-RECAP-FG,
              :QRC-GL-FG,
              :QRC-EOD-BATCH-FG,
              :QRC-NIGHTLY-FUNDS-FG,
              :QRC-EOM1-BATCH-FG,
              :QRC-EOM-UPDATE-FG,
              :QRC-EOM2-BATCH-FG,
              :QRC-BS-P2P3-INBAL-FG,
              :QRC-DEPOSIT,
              :QRC-OVER-SHORT,
              :QRC-CHECKS,
              :QRC-OPENED-DATE,
              :QRC-OPENED-TIME,
              :QRC-OP-USERID,
              :QRC-CLOSED-DATE,
              :QRC-CLOSED-TIME,
              :QRC-CL-USERID,
              :QRC-UPDATED-DATE,
              :QRC-UPDATED-TIME,
              :QRC-UPDATED-USERID,
              :QRC-EOD-BATCH-DATE,
              :QRC-EOD-BATCH-TIME,
              :QRC-FUNDS-DATE,
              :QRC-FUNDS-TIME,
              :QRC-EARNINGS-DATE,
              :QRC-EARNINGS-TIME,
              :QRC-EOM-UPDATED-DATE,
              :QRC-EOM-UPDATED-TIME,
              :QRC-EOM2-BATCH-DATE,
              :QRC-EOM2-BATCH-TIME,
              :QRC-CHECK-DATE,
              :QRC-CHECK-TIME,
              :QRC-LAST-ACCRUE,
              :QRC-ERROR-MSG,
              :QRC-BANK-BAL,
              :QRC-CHECK-DEPOSIT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-RC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      * USE C1 CONNECT
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-RC1-FILE.

           ACCEPT RC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           PERFORM SET-RC-FIELDS.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.RCFILE
             SET
              RCFILE.RC_STATUS = :QRC-STATUS,
              RCFILE.RC_BRNO = :QRC-BRNO,
              RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE,
              RCFILE.RC_LTOUCH_DATE = :QRC-LTOUCH-DATE,
              RCFILE.RC_RECAP_FG = :QRC-RECAP-FG,
              RCFILE.RC_GL_FG = :QRC-GL-FG,
              RCFILE.RC_EOD_BATCH_FG = :QRC-EOD-BATCH-FG,
              RCFILE.RC_NIGHTLY_FUNDS_FG = :QRC-NIGHTLY-FUNDS-FG,
              RCFILE.RC_EOM1_BATCH_FG = :QRC-EOM1-BATCH-FG,
              RCFILE.RC_EOM_UPDATE_FG = :QRC-EOM-UPDATE-FG,
              RCFILE.RC_EOM2_BATCH_FG = :QRC-EOM2-BATCH-FG,
              RCFILE.RC_BS_P2P3_INBAL_FG = :QRC-BS-P2P3-INBAL-FG,
              RCFILE.RC_DEPOSIT = :QRC-DEPOSIT,
              RCFILE.RC_OVER_SHORT = :QRC-OVER-SHORT,
              RCFILE.RC_CHECKS = :QRC-CHECKS,
              RCFILE.RC_OPENED_DATE = :QRC-OPENED-DATE,
              RCFILE.RC_OPENED_TIME = :QRC-OPENED-TIME,
              RCFILE.RC_OP_USERID = :QRC-OP-USERID,
              RCFILE.RC_CLOSED_DATE = :QRC-CLOSED-DATE,
              RCFILE.RC_CLOSED_TIME = :QRC-CLOSED-TIME,
              RCFILE.RC_CL_USERID = :QRC-CL-USERID,
              RCFILE.RC_UPDATED_DATE = :QRC-UPDATED-DATE,
              RCFILE.RC_UPDATED_TIME = :QRC-UPDATED-TIME,
              RCFILE.RC_UPDATED_USERID = :QRC-UPDATED-USERID,
              RCFILE.RC_EOD_BATCH_DATE = :QRC-EOD-BATCH-DATE,
              RCFILE.RC_EOD_BATCH_TIME = :QRC-EOD-BATCH-TIME,
              RCFILE.RC_FUNDS_DATE = :QRC-FUNDS-DATE,
              RCFILE.RC_FUNDS_TIME = :QRC-FUNDS-TIME,
              RCFILE.RC_EARNINGS_DATE = :QRC-EARNINGS-DATE,
              RCFILE.RC_EARNINGS_TIME = :QRC-EARNINGS-TIME,
              RCFILE.RC_EOM_UPDATED_DATE = :QRC-EOM-UPDATED-DATE,
              RCFILE.RC_EOM_UPDATED_TIME = :QRC-EOM-UPDATED-TIME,
              RCFILE.RC_EOM2_BATCH_DATE = :QRC-EOM2-BATCH-DATE,
              RCFILE.RC_EOM2_BATCH_TIME = :QRC-EOM2-BATCH-TIME,
              RCFILE.RC_CHECK_DATE = :QRC-CHECK-DATE,
              RCFILE.RC_CHECK_TIME = :QRC-CHECK-TIME,
              RCFILE.RC_LAST_ACCRUE = :QRC-LAST-ACCRUE,
              RCFILE.RC_ERROR_MSG = :QRC-ERROR-MSG,
              RCFILE.RC_BANK_BAL = :QRC-BANK-BAL,
              RCFILE.RC_CHECK_DEPOSIT = :QRC-CHECK-DEPOSIT
            WHERE RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-RC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      * USE C1 CONNECT
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-RC1-FILE.
           PERFORM CLOSE-IT.

       COPY "LIBLP/LPLC1RN.CPY".
  
      * COPY "LIBLP/LPBY1IN.CPY".
      * COPYMEMBER: LIBLP/LPBY1IN
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - IO ROUTINES - A THRU B
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *              /USR/DATA/R#/LP/BYBFILE
      *
      * ATTENTION  : BY-PATH-OVERRIDE CONTROLS ALL FILES - A THRU B
      *              ^^^^^^^^^^^^^^^^
      *              THERE IS NO BYA-PATH-OVERRIDE OR BYB-PATH-OVERRIDE
      *
      *=================================================================
      * REV:
      * KEC 2019-0205 ***NEW*** TO A32; MODIFIED FROM A30
      * KEC 2021-0524 ADDED ACUSQL VERSION
      ******************************************************************
       SET-ALL-TO-BY1.
           MOVE BYA1-KEY   TO BY1-KEY.
           MOVE BYA-AREA-A TO BY-AREA-A.
           MOVE BYB-AREA-B TO BY-AREA-B.

      *-----------------------------------------------------------------
       SET-BY1-TO-ALL.
           MOVE BY1-KEY    TO BYA1-KEY BYB1-KEY.
           MOVE BY-AREA-A  TO BYA-AREA-A.
           MOVE BY-AREA-B  TO BYB-AREA-B.

      *-----------------------------------------------------------------
       LOAD-BY1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM LOAD-BYB1-FILE.

      *-----------------------------------------------------------------
       OPEN-BY1-FILE.
           PERFORM OPEN-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM OPEN-BYB1-FILE
              IF ( IO-FG = 0 )
                 MOVE " " TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM START-BYA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE BYA1-KEY TO BYB1-KEY
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       WRITE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM WRITE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM WRITE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       REWRITE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM REWRITE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM REWRITE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-BY1-FILE.

           PERFORM CLOSE-IT.
           PERFORM CLOSE-BYA1-FILE.
           PERFORM CLOSE-BYB1-FILE.

      *=================================================================
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BYA1-FILE. *> EMBEDDED - BYAFILE
           PERFORM START-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

           IF ( QBY1-WEND-DATE-CCYYMM = ZEROES ) OR
              ( QBY1-WEND-DATE-CCYYMM NOT NUMERIC )
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QBY1-WEND-DATE-CCYYMM 
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
              MOVE ALL "9"              TO QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE ALL "9"              TO QBY1-WEND-CLASS.

           EXEC SQL
            DECLARE BYA1_CURSOR CURSOR FOR
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   CAST(BYAFILE.BYA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM = :QBY1-WBEG-DATE-CCYYMM
      *       AND BYAFILE.BYA_DATE_CCYYMM <= :QBY1-WEND-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO        = :QBY1-WBEG-BRNO
      *       AND BYAFILE.BYA_BRNO       <= :QBY1-WEND-BRNO 
      *       AND BYAFILE.BYA_CLASS      >= :QBY1-WBEG-CLASS
      *       AND BYAFILE.BYA_CLASS      <= :QBY1-WEND-CLASS
            ORDER BY BYAFILE.BYA_DATE_CCYYMM,
                     BYAFILE.BYA_BRNO,
                     BYAFILE.BYA_CLASS
           END-EXEC.

           EXEC SQL
               OPEN BYA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBY1-WBEG-DATE-CCYYMM
                          QBY1-WEND-DATE-CCYYMM
                          QBY1-WBEG-BRNO
                          QBY1-WEND-BRNO
                          QBY1-WBEG-CLASS
                          QBY1-WEND-CLASS.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BYA1-FILE. *> EMBEDDED - BYAFILE
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           MOVE BYA-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BYA-BRNO             TO QBYA-BRNO.
           MOVE BYA-CLASS            TO QBYA-CLASS.

           EXEC SQL
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   CAST(BYAFILE.BYA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            INTO :QBYA-REC
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
              AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT. *> EMBEDDED - BYAFILE
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BYA1_CURSOR INTO :QBYA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BYA1-FILE. *> EMBEDDED - BYAFILE

           ACCEPT BYA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.BYAFILE
             (BYAFILE.BYA_DATE_CCYYMM,
              BYAFILE.BYA_BRNO,
              BYAFILE.BYA_CLASS,
              BYAFILE.BYA_LTOUCH_DATE,
              BYAFILE.BYA_NO_1_1_1,
              BYAFILE.BYA_AMT_1_1_1,
              BYAFILE.BYA_O2_1_1_1,
              BYAFILE.BYA_INT_1_1_1,
              BYAFILE.BYA_NO_1_1_2,
              BYAFILE.BYA_AMT_1_1_2,
              BYAFILE.BYA_O2_1_1_2,
              BYAFILE.BYA_INT_1_1_2,
              BYAFILE.BYA_NO_1_1_3,
              BYAFILE.BYA_AMT_1_1_3,
              BYAFILE.BYA_O2_1_1_3,
              BYAFILE.BYA_INT_1_1_3,
              BYAFILE.BYA_NO_1_1_4,
              BYAFILE.BYA_AMT_1_1_4,
              BYAFILE.BYA_O2_1_1_4,
              BYAFILE.BYA_INT_1_1_4,
              BYAFILE.BYA_NO_1_1_5,
              BYAFILE.BYA_AMT_1_1_5,
              BYAFILE.BYA_O2_1_1_5,
              BYAFILE.BYA_INT_1_1_5,
              BYAFILE.BYA_NO_1_1_6,
              BYAFILE.BYA_AMT_1_1_6,
              BYAFILE.BYA_O2_1_1_6,
              BYAFILE.BYA_INT_1_1_6,
              BYAFILE.BYA_NO_1_1_7,
              BYAFILE.BYA_AMT_1_1_7,
              BYAFILE.BYA_O2_1_1_7,
              BYAFILE.BYA_INT_1_1_7,
              BYAFILE.BYA_NO_1_1_8,
              BYAFILE.BYA_AMT_1_1_8,
              BYAFILE.BYA_O2_1_1_8,
              BYAFILE.BYA_INT_1_1_8,
              BYAFILE.BYA_NO_1_1_9,
              BYAFILE.BYA_AMT_1_1_9,
              BYAFILE.BYA_O2_1_1_9,
              BYAFILE.BYA_INT_1_1_9,
              BYAFILE.BYA_NO_1_1_10,
              BYAFILE.BYA_AMT_1_1_10,
              BYAFILE.BYA_O2_1_1_10,
              BYAFILE.BYA_INT_1_1_10,
              BYAFILE.BYA_NO_1_1_11,
              BYAFILE.BYA_AMT_1_1_11,
              BYAFILE.BYA_O2_1_1_11,
              BYAFILE.BYA_INT_1_1_11,
              BYAFILE.BYA_NO_1_2_1,
              BYAFILE.BYA_AMT_1_2_1,
              BYAFILE.BYA_O2_1_2_1,
              BYAFILE.BYA_INT_1_2_1,
              BYAFILE.BYA_NO_1_2_2,
              BYAFILE.BYA_AMT_1_2_2,
              BYAFILE.BYA_O2_1_2_2,
              BYAFILE.BYA_INT_1_2_2,
              BYAFILE.BYA_NO_1_2_3,
              BYAFILE.BYA_AMT_1_2_3,
              BYAFILE.BYA_O2_1_2_3,
              BYAFILE.BYA_INT_1_2_3,
              BYAFILE.BYA_NO_1_2_4,
              BYAFILE.BYA_AMT_1_2_4,
              BYAFILE.BYA_O2_1_2_4,
              BYAFILE.BYA_INT_1_2_4,
              BYAFILE.BYA_NO_1_2_5,
              BYAFILE.BYA_AMT_1_2_5,
              BYAFILE.BYA_O2_1_2_5,
              BYAFILE.BYA_INT_1_2_5,
              BYAFILE.BYA_NO_1_2_6,
              BYAFILE.BYA_AMT_1_2_6,
              BYAFILE.BYA_O2_1_2_6,
              BYAFILE.BYA_INT_1_2_6,
              BYAFILE.BYA_NO_1_2_7,
              BYAFILE.BYA_AMT_1_2_7,
              BYAFILE.BYA_O2_1_2_7,
              BYAFILE.BYA_INT_1_2_7,
              BYAFILE.BYA_NO_1_2_8,
              BYAFILE.BYA_AMT_1_2_8,
              BYAFILE.BYA_O2_1_2_8,
              BYAFILE.BYA_INT_1_2_8,
              BYAFILE.BYA_NO_1_2_9,
              BYAFILE.BYA_AMT_1_2_9,
              BYAFILE.BYA_O2_1_2_9,
              BYAFILE.BYA_INT_1_2_9,
              BYAFILE.BYA_NO_1_2_10,
              BYAFILE.BYA_AMT_1_2_10,
              BYAFILE.BYA_O2_1_2_10,
              BYAFILE.BYA_INT_1_2_10,
              BYAFILE.BYA_NO_1_2_11,
              BYAFILE.BYA_AMT_1_2_11,
              BYAFILE.BYA_O2_1_2_11,
              BYAFILE.BYA_INT_1_2_11,
              BYAFILE.BYA_NO_2_1_1,
              BYAFILE.BYA_AMT_2_1_1,
              BYAFILE.BYA_O2_2_1_1,
              BYAFILE.BYA_INT_2_1_1,
              BYAFILE.BYA_NO_2_1_2,
              BYAFILE.BYA_AMT_2_1_2,
              BYAFILE.BYA_O2_2_1_2,
              BYAFILE.BYA_INT_2_1_2,
              BYAFILE.BYA_NO_2_1_3,
              BYAFILE.BYA_AMT_2_1_3,
              BYAFILE.BYA_O2_2_1_3,
              BYAFILE.BYA_INT_2_1_3,
              BYAFILE.BYA_NO_2_1_4,
              BYAFILE.BYA_AMT_2_1_4,
              BYAFILE.BYA_O2_2_1_4,
              BYAFILE.BYA_INT_2_1_4,
              BYAFILE.BYA_NO_2_1_5,
              BYAFILE.BYA_AMT_2_1_5,
              BYAFILE.BYA_O2_2_1_5,
              BYAFILE.BYA_INT_2_1_5,
              BYAFILE.BYA_NO_2_1_6,
              BYAFILE.BYA_AMT_2_1_6,
              BYAFILE.BYA_O2_2_1_6,
              BYAFILE.BYA_INT_2_1_6,
              BYAFILE.BYA_NO_2_1_7,
              BYAFILE.BYA_AMT_2_1_7,
              BYAFILE.BYA_O2_2_1_7,
              BYAFILE.BYA_INT_2_1_7,
              BYAFILE.BYA_NO_2_1_8,
              BYAFILE.BYA_AMT_2_1_8,
              BYAFILE.BYA_O2_2_1_8,
              BYAFILE.BYA_INT_2_1_8,
              BYAFILE.BYA_NO_2_1_9,
              BYAFILE.BYA_AMT_2_1_9,
              BYAFILE.BYA_O2_2_1_9,
              BYAFILE.BYA_INT_2_1_9,
              BYAFILE.BYA_NO_2_1_10,
              BYAFILE.BYA_AMT_2_1_10,
              BYAFILE.BYA_O2_2_1_10,
              BYAFILE.BYA_INT_2_1_10,
              BYAFILE.BYA_NO_2_1_11,
              BYAFILE.BYA_AMT_2_1_11,
              BYAFILE.BYA_O2_2_1_11,
              BYAFILE.BYA_INT_2_1_11,
              BYAFILE.BYA_NO_2_2_1,
              BYAFILE.BYA_AMT_2_2_1,
              BYAFILE.BYA_O2_2_2_1,
              BYAFILE.BYA_INT_2_2_1,
              BYAFILE.BYA_NO_2_2_2,
              BYAFILE.BYA_AMT_2_2_2,
              BYAFILE.BYA_O2_2_2_2,
              BYAFILE.BYA_INT_2_2_2,
              BYAFILE.BYA_NO_2_2_3,
              BYAFILE.BYA_AMT_2_2_3,
              BYAFILE.BYA_O2_2_2_3,
              BYAFILE.BYA_INT_2_2_3,
              BYAFILE.BYA_NO_2_2_4,
              BYAFILE.BYA_AMT_2_2_4,
              BYAFILE.BYA_O2_2_2_4,
              BYAFILE.BYA_INT_2_2_4,
              BYAFILE.BYA_NO_2_2_5,
              BYAFILE.BYA_AMT_2_2_5,
              BYAFILE.BYA_O2_2_2_5,
              BYAFILE.BYA_INT_2_2_5,
              BYAFILE.BYA_NO_2_2_6,
              BYAFILE.BYA_AMT_2_2_6,
              BYAFILE.BYA_O2_2_2_6,
              BYAFILE.BYA_INT_2_2_6,
              BYAFILE.BYA_NO_2_2_7,
              BYAFILE.BYA_AMT_2_2_7,
              BYAFILE.BYA_O2_2_2_7,
              BYAFILE.BYA_INT_2_2_7,
              BYAFILE.BYA_NO_2_2_8,
              BYAFILE.BYA_AMT_2_2_8,
              BYAFILE.BYA_O2_2_2_8,
              BYAFILE.BYA_INT_2_2_8,
              BYAFILE.BYA_NO_2_2_9,
              BYAFILE.BYA_AMT_2_2_9,
              BYAFILE.BYA_O2_2_2_9,
              BYAFILE.BYA_INT_2_2_9,
              BYAFILE.BYA_NO_2_2_10,
              BYAFILE.BYA_AMT_2_2_10,
              BYAFILE.BYA_O2_2_2_10,
              BYAFILE.BYA_INT_2_2_10,
              BYAFILE.BYA_NO_2_2_11,
              BYAFILE.BYA_AMT_2_2_11,
              BYAFILE.BYA_O2_2_2_11,
              BYAFILE.BYA_INT_2_2_11,
              BYAFILE.BYA_NO_3_1_1,
              BYAFILE.BYA_AMT_3_1_1,
              BYAFILE.BYA_O2_3_1_1,
              BYAFILE.BYA_INT_3_1_1,
              BYAFILE.BYA_NO_3_1_2,
              BYAFILE.BYA_AMT_3_1_2,
              BYAFILE.BYA_O2_3_1_2,
              BYAFILE.BYA_INT_3_1_2,
              BYAFILE.BYA_NO_3_1_3,
              BYAFILE.BYA_AMT_3_1_3,
              BYAFILE.BYA_O2_3_1_3,
              BYAFILE.BYA_INT_3_1_3,
              BYAFILE.BYA_NO_3_1_4,
              BYAFILE.BYA_AMT_3_1_4,
              BYAFILE.BYA_O2_3_1_4,
              BYAFILE.BYA_INT_3_1_4,
              BYAFILE.BYA_NO_3_1_5,
              BYAFILE.BYA_AMT_3_1_5,
              BYAFILE.BYA_O2_3_1_5,
              BYAFILE.BYA_INT_3_1_5,
              BYAFILE.BYA_NO_3_1_6,
              BYAFILE.BYA_AMT_3_1_6,
              BYAFILE.BYA_O2_3_1_6,
              BYAFILE.BYA_INT_3_1_6,
              BYAFILE.BYA_NO_3_1_7,
              BYAFILE.BYA_AMT_3_1_7,
              BYAFILE.BYA_O2_3_1_7,
              BYAFILE.BYA_INT_3_1_7,
              BYAFILE.BYA_NO_3_1_8,
              BYAFILE.BYA_AMT_3_1_8,
              BYAFILE.BYA_O2_3_1_8,
              BYAFILE.BYA_INT_3_1_8,
              BYAFILE.BYA_NO_3_1_9,
              BYAFILE.BYA_AMT_3_1_9,
              BYAFILE.BYA_O2_3_1_9,
              BYAFILE.BYA_INT_3_1_9,
              BYAFILE.BYA_NO_3_1_10,
              BYAFILE.BYA_AMT_3_1_10,
              BYAFILE.BYA_O2_3_1_10,
              BYAFILE.BYA_INT_3_1_10,
              BYAFILE.BYA_NO_3_1_11,
              BYAFILE.BYA_AMT_3_1_11,
              BYAFILE.BYA_O2_3_1_11,
              BYAFILE.BYA_INT_3_1_11,
              BYAFILE.BYA_NO_3_2_1,
              BYAFILE.BYA_AMT_3_2_1,
              BYAFILE.BYA_O2_3_2_1,
              BYAFILE.BYA_INT_3_2_1,
              BYAFILE.BYA_NO_3_2_2,
              BYAFILE.BYA_AMT_3_2_2,
              BYAFILE.BYA_O2_3_2_2,
              BYAFILE.BYA_INT_3_2_2,
              BYAFILE.BYA_NO_3_2_3,
              BYAFILE.BYA_AMT_3_2_3,
              BYAFILE.BYA_O2_3_2_3,
              BYAFILE.BYA_INT_3_2_3,
              BYAFILE.BYA_NO_3_2_4,
              BYAFILE.BYA_AMT_3_2_4,
              BYAFILE.BYA_O2_3_2_4,
              BYAFILE.BYA_INT_3_2_4,
              BYAFILE.BYA_NO_3_2_5,
              BYAFILE.BYA_AMT_3_2_5,
              BYAFILE.BYA_O2_3_2_5,
              BYAFILE.BYA_INT_3_2_5,
              BYAFILE.BYA_NO_3_2_6,
              BYAFILE.BYA_AMT_3_2_6,
              BYAFILE.BYA_O2_3_2_6,
              BYAFILE.BYA_INT_3_2_6,
              BYAFILE.BYA_NO_3_2_7,
              BYAFILE.BYA_AMT_3_2_7,
              BYAFILE.BYA_O2_3_2_7,
              BYAFILE.BYA_INT_3_2_7,
              BYAFILE.BYA_NO_3_2_8,
              BYAFILE.BYA_AMT_3_2_8,
              BYAFILE.BYA_O2_3_2_8,
              BYAFILE.BYA_INT_3_2_8,
              BYAFILE.BYA_NO_3_2_9,
              BYAFILE.BYA_AMT_3_2_9,
              BYAFILE.BYA_O2_3_2_9,
              BYAFILE.BYA_INT_3_2_9,
              BYAFILE.BYA_NO_3_2_10,
              BYAFILE.BYA_AMT_3_2_10,
              BYAFILE.BYA_O2_3_2_10,
              BYAFILE.BYA_INT_3_2_10,
              BYAFILE.BYA_NO_3_2_11,
              BYAFILE.BYA_AMT_3_2_11,
              BYAFILE.BYA_O2_3_2_11,
              BYAFILE.BYA_INT_3_2_11,
              BYAFILE.BYA_NO_REPO_1_1_1,
              BYAFILE.BYA_AMT_REPO_1_1_1,
              BYAFILE.BYA_O2_REPO_1_1_1,
              BYAFILE.BYA_INT_REPO_1_1_1,
              BYAFILE.BYA_NO_REPO_1_1_2,
              BYAFILE.BYA_AMT_REPO_1_1_2,
              BYAFILE.BYA_O2_REPO_1_1_2,
              BYAFILE.BYA_INT_REPO_1_1_2,
              BYAFILE.BYA_NO_REPO_1_1_3,
              BYAFILE.BYA_AMT_REPO_1_1_3,
              BYAFILE.BYA_O2_REPO_1_1_3,
              BYAFILE.BYA_INT_REPO_1_1_3,
              BYAFILE.BYA_NO_REPO_1_1_4,
              BYAFILE.BYA_AMT_REPO_1_1_4,
              BYAFILE.BYA_O2_REPO_1_1_4,
              BYAFILE.BYA_INT_REPO_1_1_4,
              BYAFILE.BYA_NO_REPO_1_1_5,
              BYAFILE.BYA_AMT_REPO_1_1_5,
              BYAFILE.BYA_O2_REPO_1_1_5,
              BYAFILE.BYA_INT_REPO_1_1_5,
              BYAFILE.BYA_NO_REPO_1_2_1,
              BYAFILE.BYA_AMT_REPO_1_2_1,
              BYAFILE.BYA_O2_REPO_1_2_1,
              BYAFILE.BYA_INT_REPO_1_2_1,
              BYAFILE.BYA_NO_REPO_1_2_2,
              BYAFILE.BYA_AMT_REPO_1_2_2,
              BYAFILE.BYA_O2_REPO_1_2_2,
              BYAFILE.BYA_INT_REPO_1_2_2,
              BYAFILE.BYA_NO_REPO_1_2_3,
              BYAFILE.BYA_AMT_REPO_1_2_3,
              BYAFILE.BYA_O2_REPO_1_2_3,
              BYAFILE.BYA_INT_REPO_1_2_3,
              BYAFILE.BYA_NO_REPO_1_2_4,
              BYAFILE.BYA_AMT_REPO_1_2_4,
              BYAFILE.BYA_O2_REPO_1_2_4,
              BYAFILE.BYA_INT_REPO_1_2_4,
              BYAFILE.BYA_NO_REPO_1_2_5,
              BYAFILE.BYA_AMT_REPO_1_2_5,
              BYAFILE.BYA_O2_REPO_1_2_5,
              BYAFILE.BYA_INT_REPO_1_2_5,
              BYAFILE.BYA_NO_REPO_2_1_1,
              BYAFILE.BYA_AMT_REPO_2_1_1,
              BYAFILE.BYA_O2_REPO_2_1_1,
              BYAFILE.BYA_INT_REPO_2_1_1,
              BYAFILE.BYA_NO_REPO_2_1_2,
              BYAFILE.BYA_AMT_REPO_2_1_2,
              BYAFILE.BYA_O2_REPO_2_1_2,
              BYAFILE.BYA_INT_REPO_2_1_2,
              BYAFILE.BYA_NO_REPO_2_1_3,
              BYAFILE.BYA_AMT_REPO_2_1_3,
              BYAFILE.BYA_O2_REPO_2_1_3,
              BYAFILE.BYA_INT_REPO_2_1_3,
              BYAFILE.BYA_NO_REPO_2_1_4,
              BYAFILE.BYA_AMT_REPO_2_1_4,
              BYAFILE.BYA_O2_REPO_2_1_4,
              BYAFILE.BYA_INT_REPO_2_1_4,
              BYAFILE.BYA_NO_REPO_2_1_5,
              BYAFILE.BYA_AMT_REPO_2_1_5,
              BYAFILE.BYA_O2_REPO_2_1_5,
              BYAFILE.BYA_INT_REPO_2_1_5,
              BYAFILE.BYA_NO_REPO_2_2_1,
              BYAFILE.BYA_AMT_REPO_2_2_1,
              BYAFILE.BYA_O2_REPO_2_2_1,
              BYAFILE.BYA_INT_REPO_2_2_1,
              BYAFILE.BYA_NO_REPO_2_2_2,
              BYAFILE.BYA_AMT_REPO_2_2_2,
              BYAFILE.BYA_O2_REPO_2_2_2,
              BYAFILE.BYA_INT_REPO_2_2_2,
              BYAFILE.BYA_NO_REPO_2_2_3,
              BYAFILE.BYA_AMT_REPO_2_2_3,
              BYAFILE.BYA_O2_REPO_2_2_3,
              BYAFILE.BYA_INT_REPO_2_2_3,
              BYAFILE.BYA_NO_REPO_2_2_4,
              BYAFILE.BYA_AMT_REPO_2_2_4,
              BYAFILE.BYA_O2_REPO_2_2_4,
              BYAFILE.BYA_INT_REPO_2_2_4,
              BYAFILE.BYA_NO_REPO_2_2_5,
              BYAFILE.BYA_AMT_REPO_2_2_5,
              BYAFILE.BYA_O2_REPO_2_2_5,
              BYAFILE.BYA_INT_REPO_2_2_5,
              BYAFILE.BYA_NO_REPO_3_1_1,
              BYAFILE.BYA_AMT_REPO_3_1_1,
              BYAFILE.BYA_O2_REPO_3_1_1,
              BYAFILE.BYA_INT_REPO_3_1_1,
              BYAFILE.BYA_NO_REPO_3_1_2,
              BYAFILE.BYA_AMT_REPO_3_1_2,
              BYAFILE.BYA_O2_REPO_3_1_2,
              BYAFILE.BYA_INT_REPO_3_1_2,
              BYAFILE.BYA_NO_REPO_3_1_3,
              BYAFILE.BYA_AMT_REPO_3_1_3,
              BYAFILE.BYA_O2_REPO_3_1_3,
              BYAFILE.BYA_INT_REPO_3_1_3,
              BYAFILE.BYA_NO_REPO_3_1_4,
              BYAFILE.BYA_AMT_REPO_3_1_4,
              BYAFILE.BYA_O2_REPO_3_1_4,
              BYAFILE.BYA_INT_REPO_3_1_4,
              BYAFILE.BYA_NO_REPO_3_1_5,
              BYAFILE.BYA_AMT_REPO_3_1_5,
              BYAFILE.BYA_O2_REPO_3_1_5,
              BYAFILE.BYA_INT_REPO_3_1_5,
              BYAFILE.BYA_NO_REPO_3_2_1,
              BYAFILE.BYA_AMT_REPO_3_2_1,
              BYAFILE.BYA_O2_REPO_3_2_1,
              BYAFILE.BYA_INT_REPO_3_2_1,
              BYAFILE.BYA_NO_REPO_3_2_2,
              BYAFILE.BYA_AMT_REPO_3_2_2,
              BYAFILE.BYA_O2_REPO_3_2_2,
              BYAFILE.BYA_INT_REPO_3_2_2,
              BYAFILE.BYA_NO_REPO_3_2_3,
              BYAFILE.BYA_AMT_REPO_3_2_3,
              BYAFILE.BYA_O2_REPO_3_2_3,
              BYAFILE.BYA_INT_REPO_3_2_3,
              BYAFILE.BYA_NO_REPO_3_2_4,
              BYAFILE.BYA_AMT_REPO_3_2_4,
              BYAFILE.BYA_O2_REPO_3_2_4,
              BYAFILE.BYA_INT_REPO_3_2_4,
              BYAFILE.BYA_NO_REPO_3_2_5,
              BYAFILE.BYA_AMT_REPO_3_2_5,
              BYAFILE.BYA_O2_REPO_3_2_5,
              BYAFILE.BYA_INT_REPO_3_2_5,
              BYAFILE.BYA_NO_PL_1_1_1,
              BYAFILE.BYA_AMT_PL_1_1_1,
              BYAFILE.BYA_O2_PL_1_1_1,
              BYAFILE.BYA_INT_PL_1_1_1,
              BYAFILE.BYA_NO_PL_1_1_2,
              BYAFILE.BYA_AMT_PL_1_1_2,
              BYAFILE.BYA_O2_PL_1_1_2,
              BYAFILE.BYA_INT_PL_1_1_2,
              BYAFILE.BYA_NO_PL_1_1_3,
              BYAFILE.BYA_AMT_PL_1_1_3,
              BYAFILE.BYA_O2_PL_1_1_3,
              BYAFILE.BYA_INT_PL_1_1_3,
              BYAFILE.BYA_NO_PL_1_1_4,
              BYAFILE.BYA_AMT_PL_1_1_4,
              BYAFILE.BYA_O2_PL_1_1_4,
              BYAFILE.BYA_INT_PL_1_1_4,
              BYAFILE.BYA_NO_PL_1_1_5,
              BYAFILE.BYA_AMT_PL_1_1_5,
              BYAFILE.BYA_O2_PL_1_1_5,
              BYAFILE.BYA_INT_PL_1_1_5,
              BYAFILE.BYA_NO_PL_1_1_6,
              BYAFILE.BYA_AMT_PL_1_1_6,
              BYAFILE.BYA_O2_PL_1_1_6,
              BYAFILE.BYA_INT_PL_1_1_6,
              BYAFILE.BYA_NO_PL_1_1_7,
              BYAFILE.BYA_AMT_PL_1_1_7,
              BYAFILE.BYA_O2_PL_1_1_7,
              BYAFILE.BYA_INT_PL_1_1_7,
              BYAFILE.BYA_NO_PL_1_1_8,
              BYAFILE.BYA_AMT_PL_1_1_8,
              BYAFILE.BYA_O2_PL_1_1_8,
              BYAFILE.BYA_INT_PL_1_1_8,
              BYAFILE.BYA_NO_PL_1_1_9,
              BYAFILE.BYA_AMT_PL_1_1_9,
              BYAFILE.BYA_O2_PL_1_1_9,
              BYAFILE.BYA_INT_PL_1_1_9,
              BYAFILE.BYA_NO_PL_1_1_10,
              BYAFILE.BYA_AMT_PL_1_1_10,
              BYAFILE.BYA_O2_PL_1_1_10,
              BYAFILE.BYA_INT_PL_1_1_10,
              BYAFILE.BYA_NO_PL_1_2_1,
              BYAFILE.BYA_AMT_PL_1_2_1,
              BYAFILE.BYA_O2_PL_1_2_1,
              BYAFILE.BYA_INT_PL_1_2_1,
              BYAFILE.BYA_NO_PL_1_2_2,
              BYAFILE.BYA_AMT_PL_1_2_2,
              BYAFILE.BYA_O2_PL_1_2_2,
              BYAFILE.BYA_INT_PL_1_2_2,
              BYAFILE.BYA_NO_PL_1_2_3,
              BYAFILE.BYA_AMT_PL_1_2_3,
              BYAFILE.BYA_O2_PL_1_2_3,
              BYAFILE.BYA_INT_PL_1_2_3,
              BYAFILE.BYA_NO_PL_1_2_4,
              BYAFILE.BYA_AMT_PL_1_2_4,
              BYAFILE.BYA_O2_PL_1_2_4,
              BYAFILE.BYA_INT_PL_1_2_4,
              BYAFILE.BYA_NO_PL_1_2_5,
              BYAFILE.BYA_AMT_PL_1_2_5,
              BYAFILE.BYA_O2_PL_1_2_5,
              BYAFILE.BYA_INT_PL_1_2_5,
              BYAFILE.BYA_NO_PL_1_2_6,
              BYAFILE.BYA_AMT_PL_1_2_6,
              BYAFILE.BYA_O2_PL_1_2_6,
              BYAFILE.BYA_INT_PL_1_2_6,
              BYAFILE.BYA_NO_PL_1_2_7,
              BYAFILE.BYA_AMT_PL_1_2_7,
              BYAFILE.BYA_O2_PL_1_2_7,
              BYAFILE.BYA_INT_PL_1_2_7,
              BYAFILE.BYA_NO_PL_1_2_8,
              BYAFILE.BYA_AMT_PL_1_2_8,
              BYAFILE.BYA_O2_PL_1_2_8,
              BYAFILE.BYA_INT_PL_1_2_8,
              BYAFILE.BYA_NO_PL_1_2_9,
              BYAFILE.BYA_AMT_PL_1_2_9,
              BYAFILE.BYA_O2_PL_1_2_9,
              BYAFILE.BYA_INT_PL_1_2_9,
              BYAFILE.BYA_NO_PL_1_2_10,
              BYAFILE.BYA_AMT_PL_1_2_10,
              BYAFILE.BYA_O2_PL_1_2_10,
              BYAFILE.BYA_INT_PL_1_2_10,
              BYAFILE.BYA_NO_PL_2_1_1,
              BYAFILE.BYA_AMT_PL_2_1_1,
              BYAFILE.BYA_O2_PL_2_1_1,
              BYAFILE.BYA_INT_PL_2_1_1,
              BYAFILE.BYA_NO_PL_2_1_2,
              BYAFILE.BYA_AMT_PL_2_1_2,
              BYAFILE.BYA_O2_PL_2_1_2,
              BYAFILE.BYA_INT_PL_2_1_2,
              BYAFILE.BYA_NO_PL_2_1_3,
              BYAFILE.BYA_AMT_PL_2_1_3,
              BYAFILE.BYA_O2_PL_2_1_3,
              BYAFILE.BYA_INT_PL_2_1_3,
              BYAFILE.BYA_NO_PL_2_1_4,
              BYAFILE.BYA_AMT_PL_2_1_4,
              BYAFILE.BYA_O2_PL_2_1_4,
              BYAFILE.BYA_INT_PL_2_1_4,
              BYAFILE.BYA_NO_PL_2_1_5,
              BYAFILE.BYA_AMT_PL_2_1_5,
              BYAFILE.BYA_O2_PL_2_1_5,
              BYAFILE.BYA_INT_PL_2_1_5,
              BYAFILE.BYA_NO_PL_2_1_6,
              BYAFILE.BYA_AMT_PL_2_1_6,
              BYAFILE.BYA_O2_PL_2_1_6,
              BYAFILE.BYA_INT_PL_2_1_6,
              BYAFILE.BYA_NO_PL_2_1_7,
              BYAFILE.BYA_AMT_PL_2_1_7,
              BYAFILE.BYA_O2_PL_2_1_7,
              BYAFILE.BYA_INT_PL_2_1_7,
              BYAFILE.BYA_NO_PL_2_1_8,
              BYAFILE.BYA_AMT_PL_2_1_8,
              BYAFILE.BYA_O2_PL_2_1_8,
              BYAFILE.BYA_INT_PL_2_1_8,
              BYAFILE.BYA_NO_PL_2_1_9,
              BYAFILE.BYA_AMT_PL_2_1_9,
              BYAFILE.BYA_O2_PL_2_1_9,
              BYAFILE.BYA_INT_PL_2_1_9,
              BYAFILE.BYA_NO_PL_2_1_10,
              BYAFILE.BYA_AMT_PL_2_1_10,
              BYAFILE.BYA_O2_PL_2_1_10,
              BYAFILE.BYA_INT_PL_2_1_10,
              BYAFILE.BYA_NO_PL_2_2_1,
              BYAFILE.BYA_AMT_PL_2_2_1,
              BYAFILE.BYA_O2_PL_2_2_1,
              BYAFILE.BYA_INT_PL_2_2_1,
              BYAFILE.BYA_NO_PL_2_2_2,
              BYAFILE.BYA_AMT_PL_2_2_2,
              BYAFILE.BYA_O2_PL_2_2_2,
              BYAFILE.BYA_INT_PL_2_2_2,
              BYAFILE.BYA_NO_PL_2_2_3,
              BYAFILE.BYA_AMT_PL_2_2_3,
              BYAFILE.BYA_O2_PL_2_2_3,
              BYAFILE.BYA_INT_PL_2_2_3,
              BYAFILE.BYA_NO_PL_2_2_4,
              BYAFILE.BYA_AMT_PL_2_2_4,
              BYAFILE.BYA_O2_PL_2_2_4,
              BYAFILE.BYA_INT_PL_2_2_4,
              BYAFILE.BYA_NO_PL_2_2_5,
              BYAFILE.BYA_AMT_PL_2_2_5,
              BYAFILE.BYA_O2_PL_2_2_5,
              BYAFILE.BYA_INT_PL_2_2_5,
              BYAFILE.BYA_NO_PL_2_2_6,
              BYAFILE.BYA_AMT_PL_2_2_6,
              BYAFILE.BYA_O2_PL_2_2_6,
              BYAFILE.BYA_INT_PL_2_2_6,
              BYAFILE.BYA_NO_PL_2_2_7,
              BYAFILE.BYA_AMT_PL_2_2_7,
              BYAFILE.BYA_O2_PL_2_2_7,
              BYAFILE.BYA_INT_PL_2_2_7,
              BYAFILE.BYA_NO_PL_2_2_8,
              BYAFILE.BYA_AMT_PL_2_2_8,
              BYAFILE.BYA_O2_PL_2_2_8,
              BYAFILE.BYA_INT_PL_2_2_8,
              BYAFILE.BYA_NO_PL_2_2_9,
              BYAFILE.BYA_AMT_PL_2_2_9,
              BYAFILE.BYA_O2_PL_2_2_9,
              BYAFILE.BYA_INT_PL_2_2_9,
              BYAFILE.BYA_NO_PL_2_2_10,
              BYAFILE.BYA_AMT_PL_2_2_10,
              BYAFILE.BYA_O2_PL_2_2_10,
              BYAFILE.BYA_INT_PL_2_2_10,
              BYAFILE.BYA_NO_PL_3_1_1,
              BYAFILE.BYA_AMT_PL_3_1_1,
              BYAFILE.BYA_O2_PL_3_1_1,
              BYAFILE.BYA_INT_PL_3_1_1,
              BYAFILE.BYA_NO_PL_3_1_2,
              BYAFILE.BYA_AMT_PL_3_1_2,
              BYAFILE.BYA_O2_PL_3_1_2,
              BYAFILE.BYA_INT_PL_3_1_2,
              BYAFILE.BYA_NO_PL_3_1_3,
              BYAFILE.BYA_AMT_PL_3_1_3,
              BYAFILE.BYA_O2_PL_3_1_3,
              BYAFILE.BYA_INT_PL_3_1_3,
              BYAFILE.BYA_NO_PL_3_1_4,
              BYAFILE.BYA_AMT_PL_3_1_4,
              BYAFILE.BYA_O2_PL_3_1_4,
              BYAFILE.BYA_INT_PL_3_1_4,
              BYAFILE.BYA_NO_PL_3_1_5,
              BYAFILE.BYA_AMT_PL_3_1_5,
              BYAFILE.BYA_O2_PL_3_1_5,
              BYAFILE.BYA_INT_PL_3_1_5,
              BYAFILE.BYA_NO_PL_3_1_6,
              BYAFILE.BYA_AMT_PL_3_1_6,
              BYAFILE.BYA_O2_PL_3_1_6,
              BYAFILE.BYA_INT_PL_3_1_6,
              BYAFILE.BYA_NO_PL_3_1_7,
              BYAFILE.BYA_AMT_PL_3_1_7,
              BYAFILE.BYA_O2_PL_3_1_7,
              BYAFILE.BYA_INT_PL_3_1_7,
              BYAFILE.BYA_NO_PL_3_1_8,
              BYAFILE.BYA_AMT_PL_3_1_8,
              BYAFILE.BYA_O2_PL_3_1_8,
              BYAFILE.BYA_INT_PL_3_1_8,
              BYAFILE.BYA_NO_PL_3_1_9,
              BYAFILE.BYA_AMT_PL_3_1_9,
              BYAFILE.BYA_O2_PL_3_1_9,
              BYAFILE.BYA_INT_PL_3_1_9,
              BYAFILE.BYA_NO_PL_3_1_10,
              BYAFILE.BYA_AMT_PL_3_1_10,
              BYAFILE.BYA_O2_PL_3_1_10,
              BYAFILE.BYA_INT_PL_3_1_10,
              BYAFILE.BYA_NO_PL_3_2_1,
              BYAFILE.BYA_AMT_PL_3_2_1,
              BYAFILE.BYA_O2_PL_3_2_1,
              BYAFILE.BYA_INT_PL_3_2_1,
              BYAFILE.BYA_NO_PL_3_2_2,
              BYAFILE.BYA_AMT_PL_3_2_2,
              BYAFILE.BYA_O2_PL_3_2_2,
              BYAFILE.BYA_INT_PL_3_2_2,
              BYAFILE.BYA_NO_PL_3_2_3,
              BYAFILE.BYA_AMT_PL_3_2_3,
              BYAFILE.BYA_O2_PL_3_2_3,
              BYAFILE.BYA_INT_PL_3_2_3,
              BYAFILE.BYA_NO_PL_3_2_4,
              BYAFILE.BYA_AMT_PL_3_2_4,
              BYAFILE.BYA_O2_PL_3_2_4,
              BYAFILE.BYA_INT_PL_3_2_4,
              BYAFILE.BYA_NO_PL_3_2_5,
              BYAFILE.BYA_AMT_PL_3_2_5,
              BYAFILE.BYA_O2_PL_3_2_5,
              BYAFILE.BYA_INT_PL_3_2_5,
              BYAFILE.BYA_NO_PL_3_2_6,
              BYAFILE.BYA_AMT_PL_3_2_6,
              BYAFILE.BYA_O2_PL_3_2_6,
              BYAFILE.BYA_INT_PL_3_2_6,
              BYAFILE.BYA_NO_PL_3_2_7,
              BYAFILE.BYA_AMT_PL_3_2_7,
              BYAFILE.BYA_O2_PL_3_2_7,
              BYAFILE.BYA_INT_PL_3_2_7,
              BYAFILE.BYA_NO_PL_3_2_8,
              BYAFILE.BYA_AMT_PL_3_2_8,
              BYAFILE.BYA_O2_PL_3_2_8,
              BYAFILE.BYA_INT_PL_3_2_8,
              BYAFILE.BYA_NO_PL_3_2_9,
              BYAFILE.BYA_AMT_PL_3_2_9,
              BYAFILE.BYA_O2_PL_3_2_9,
              BYAFILE.BYA_INT_PL_3_2_9,
              BYAFILE.BYA_NO_PL_3_2_10,
              BYAFILE.BYA_AMT_PL_3_2_10,
              BYAFILE.BYA_O2_PL_3_2_10,
              BYAFILE.BYA_INT_PL_3_2_10,
              BYAFILE.BYA_DLRVPRIN_1_1_1,
              BYAFILE.BYA_DLRVPRIN_1_1_2,
              BYAFILE.BYA_DLRVPRIN_1_1_3,
              BYAFILE.BYA_DLRVCHGS_1_1_1,
              BYAFILE.BYA_DLRVCHGS_1_1_2,
              BYAFILE.BYA_DLRVCHGS_1_1_3,
              BYAFILE.BYA_DLPARVHELD_1_1,
              BYAFILE.BYA_DLRVPRIN_1_2_1,
              BYAFILE.BYA_DLRVPRIN_1_2_2,
              BYAFILE.BYA_DLRVPRIN_1_2_3,
              BYAFILE.BYA_DLRVCHGS_1_2_1,
              BYAFILE.BYA_DLRVCHGS_1_2_2,
              BYAFILE.BYA_DLRVCHGS_1_2_3,
              BYAFILE.BYA_DLPARVHELD_1_2,
              BYAFILE.BYA_DLRVPRIN_2_1_1,
              BYAFILE.BYA_DLRVPRIN_2_1_2,
              BYAFILE.BYA_DLRVPRIN_2_1_3,
              BYAFILE.BYA_DLRVCHGS_2_1_1,
              BYAFILE.BYA_DLRVCHGS_2_1_2,
              BYAFILE.BYA_DLRVCHGS_2_1_3,
              BYAFILE.BYA_DLPARVHELD_2_1,
              BYAFILE.BYA_DLRVPRIN_2_2_1,
              BYAFILE.BYA_DLRVPRIN_2_2_2,
              BYAFILE.BYA_DLRVPRIN_2_2_3,
              BYAFILE.BYA_DLRVCHGS_2_2_1,
              BYAFILE.BYA_DLRVCHGS_2_2_2,
              BYAFILE.BYA_DLRVCHGS_2_2_3,
              BYAFILE.BYA_DLPARVHELD_2_2,
              BYAFILE.BYA_DLRVPRIN_3_1_1,
              BYAFILE.BYA_DLRVPRIN_3_1_2,
              BYAFILE.BYA_DLRVPRIN_3_1_3,
              BYAFILE.BYA_DLRVCHGS_3_1_1,
              BYAFILE.BYA_DLRVCHGS_3_1_2,
              BYAFILE.BYA_DLRVCHGS_3_1_3,
              BYAFILE.BYA_DLPARVHELD_3_1,
              BYAFILE.BYA_DLRVPRIN_3_2_1,
              BYAFILE.BYA_DLRVPRIN_3_2_2,
              BYAFILE.BYA_DLRVPRIN_3_2_3,
              BYAFILE.BYA_DLRVCHGS_3_2_1,
              BYAFILE.BYA_DLRVCHGS_3_2_2,
              BYAFILE.BYA_DLRVCHGS_3_2_3,
              BYAFILE.BYA_DLPARVHELD_3_2)
            VALUES
             (:QBYA-DATE-CCYYMM,
              :QBYA-BRNO,
              :QBYA-CLASS,
              :QBYA-LTOUCH-DATE,
              :QBYA-NO-1-1-1,
              :QBYA-AMT-1-1-1,
              :QBYA-O2-1-1-1,
              :QBYA-INT-1-1-1,
              :QBYA-NO-1-1-2,
              :QBYA-AMT-1-1-2,
              :QBYA-O2-1-1-2,
              :QBYA-INT-1-1-2,
              :QBYA-NO-1-1-3,
              :QBYA-AMT-1-1-3,
              :QBYA-O2-1-1-3,
              :QBYA-INT-1-1-3,
              :QBYA-NO-1-1-4,
              :QBYA-AMT-1-1-4,
              :QBYA-O2-1-1-4,
              :QBYA-INT-1-1-4,
              :QBYA-NO-1-1-5,
              :QBYA-AMT-1-1-5,
              :QBYA-O2-1-1-5,
              :QBYA-INT-1-1-5,
              :QBYA-NO-1-1-6,
              :QBYA-AMT-1-1-6,
              :QBYA-O2-1-1-6,
              :QBYA-INT-1-1-6,
              :QBYA-NO-1-1-7,
              :QBYA-AMT-1-1-7,
              :QBYA-O2-1-1-7,
              :QBYA-INT-1-1-7,
              :QBYA-NO-1-1-8,
              :QBYA-AMT-1-1-8,
              :QBYA-O2-1-1-8,
              :QBYA-INT-1-1-8,
              :QBYA-NO-1-1-9,
              :QBYA-AMT-1-1-9,
              :QBYA-O2-1-1-9,
              :QBYA-INT-1-1-9,
              :QBYA-NO-1-1-10,
              :QBYA-AMT-1-1-10,
              :QBYA-O2-1-1-10,
              :QBYA-INT-1-1-10,
              :QBYA-NO-1-1-11,
              :QBYA-AMT-1-1-11,
              :QBYA-O2-1-1-11,
              :QBYA-INT-1-1-11,
              :QBYA-NO-1-2-1,
              :QBYA-AMT-1-2-1,
              :QBYA-O2-1-2-1,
              :QBYA-INT-1-2-1,
              :QBYA-NO-1-2-2,
              :QBYA-AMT-1-2-2,
              :QBYA-O2-1-2-2,
              :QBYA-INT-1-2-2,
              :QBYA-NO-1-2-3,
              :QBYA-AMT-1-2-3,
              :QBYA-O2-1-2-3,
              :QBYA-INT-1-2-3,
              :QBYA-NO-1-2-4,
              :QBYA-AMT-1-2-4,
              :QBYA-O2-1-2-4,
              :QBYA-INT-1-2-4,
              :QBYA-NO-1-2-5,
              :QBYA-AMT-1-2-5,
              :QBYA-O2-1-2-5,
              :QBYA-INT-1-2-5,
              :QBYA-NO-1-2-6,
              :QBYA-AMT-1-2-6,
              :QBYA-O2-1-2-6,
              :QBYA-INT-1-2-6,
              :QBYA-NO-1-2-7,
              :QBYA-AMT-1-2-7,
              :QBYA-O2-1-2-7,
              :QBYA-INT-1-2-7,
              :QBYA-NO-1-2-8,
              :QBYA-AMT-1-2-8,
              :QBYA-O2-1-2-8,
              :QBYA-INT-1-2-8,
              :QBYA-NO-1-2-9,
              :QBYA-AMT-1-2-9,
              :QBYA-O2-1-2-9,
              :QBYA-INT-1-2-9,
              :QBYA-NO-1-2-10,
              :QBYA-AMT-1-2-10,
              :QBYA-O2-1-2-10,
              :QBYA-INT-1-2-10,
              :QBYA-NO-1-2-11,
              :QBYA-AMT-1-2-11,
              :QBYA-O2-1-2-11,
              :QBYA-INT-1-2-11,
              :QBYA-NO-2-1-1,
              :QBYA-AMT-2-1-1,
              :QBYA-O2-2-1-1,
              :QBYA-INT-2-1-1,
              :QBYA-NO-2-1-2,
              :QBYA-AMT-2-1-2,
              :QBYA-O2-2-1-2,
              :QBYA-INT-2-1-2,
              :QBYA-NO-2-1-3,
              :QBYA-AMT-2-1-3,
              :QBYA-O2-2-1-3,
              :QBYA-INT-2-1-3,
              :QBYA-NO-2-1-4,
              :QBYA-AMT-2-1-4,
              :QBYA-O2-2-1-4,
              :QBYA-INT-2-1-4,
              :QBYA-NO-2-1-5,
              :QBYA-AMT-2-1-5,
              :QBYA-O2-2-1-5,
              :QBYA-INT-2-1-5,
              :QBYA-NO-2-1-6,
              :QBYA-AMT-2-1-6,
              :QBYA-O2-2-1-6,
              :QBYA-INT-2-1-6,
              :QBYA-NO-2-1-7,
              :QBYA-AMT-2-1-7,
              :QBYA-O2-2-1-7,
              :QBYA-INT-2-1-7,
              :QBYA-NO-2-1-8,
              :QBYA-AMT-2-1-8,
              :QBYA-O2-2-1-8,
              :QBYA-INT-2-1-8,
              :QBYA-NO-2-1-9,
              :QBYA-AMT-2-1-9,
              :QBYA-O2-2-1-9,
              :QBYA-INT-2-1-9,
              :QBYA-NO-2-1-10,
              :QBYA-AMT-2-1-10,
              :QBYA-O2-2-1-10,
              :QBYA-INT-2-1-10,
              :QBYA-NO-2-1-11,
              :QBYA-AMT-2-1-11,
              :QBYA-O2-2-1-11,
              :QBYA-INT-2-1-11,
              :QBYA-NO-2-2-1,
              :QBYA-AMT-2-2-1,
              :QBYA-O2-2-2-1,
              :QBYA-INT-2-2-1,
              :QBYA-NO-2-2-2,
              :QBYA-AMT-2-2-2,
              :QBYA-O2-2-2-2,
              :QBYA-INT-2-2-2,
              :QBYA-NO-2-2-3,
              :QBYA-AMT-2-2-3,
              :QBYA-O2-2-2-3,
              :QBYA-INT-2-2-3,
              :QBYA-NO-2-2-4,
              :QBYA-AMT-2-2-4,
              :QBYA-O2-2-2-4,
              :QBYA-INT-2-2-4,
              :QBYA-NO-2-2-5,
              :QBYA-AMT-2-2-5,
              :QBYA-O2-2-2-5,
              :QBYA-INT-2-2-5,
              :QBYA-NO-2-2-6,
              :QBYA-AMT-2-2-6,
              :QBYA-O2-2-2-6,
              :QBYA-INT-2-2-6,
              :QBYA-NO-2-2-7,
              :QBYA-AMT-2-2-7,
              :QBYA-O2-2-2-7,
              :QBYA-INT-2-2-7,
              :QBYA-NO-2-2-8,
              :QBYA-AMT-2-2-8,
              :QBYA-O2-2-2-8,
              :QBYA-INT-2-2-8,
              :QBYA-NO-2-2-9,
              :QBYA-AMT-2-2-9,
              :QBYA-O2-2-2-9,
              :QBYA-INT-2-2-9,
              :QBYA-NO-2-2-10,
              :QBYA-AMT-2-2-10,
              :QBYA-O2-2-2-10,
              :QBYA-INT-2-2-10,
              :QBYA-NO-2-2-11,
              :QBYA-AMT-2-2-11,
              :QBYA-O2-2-2-11,
              :QBYA-INT-2-2-11,
              :QBYA-NO-3-1-1,
              :QBYA-AMT-3-1-1,
              :QBYA-O2-3-1-1,
              :QBYA-INT-3-1-1,
              :QBYA-NO-3-1-2,
              :QBYA-AMT-3-1-2,
              :QBYA-O2-3-1-2,
              :QBYA-INT-3-1-2,
              :QBYA-NO-3-1-3,
              :QBYA-AMT-3-1-3,
              :QBYA-O2-3-1-3,
              :QBYA-INT-3-1-3,
              :QBYA-NO-3-1-4,
              :QBYA-AMT-3-1-4,
              :QBYA-O2-3-1-4,
              :QBYA-INT-3-1-4,
              :QBYA-NO-3-1-5,
              :QBYA-AMT-3-1-5,
              :QBYA-O2-3-1-5,
              :QBYA-INT-3-1-5,
              :QBYA-NO-3-1-6,
              :QBYA-AMT-3-1-6,
              :QBYA-O2-3-1-6,
              :QBYA-INT-3-1-6,
              :QBYA-NO-3-1-7,
              :QBYA-AMT-3-1-7,
              :QBYA-O2-3-1-7,
              :QBYA-INT-3-1-7,
              :QBYA-NO-3-1-8,
              :QBYA-AMT-3-1-8,
              :QBYA-O2-3-1-8,
              :QBYA-INT-3-1-8,
              :QBYA-NO-3-1-9,
              :QBYA-AMT-3-1-9,
              :QBYA-O2-3-1-9,
              :QBYA-INT-3-1-9,
              :QBYA-NO-3-1-10,
              :QBYA-AMT-3-1-10,
              :QBYA-O2-3-1-10,
              :QBYA-INT-3-1-10,
              :QBYA-NO-3-1-11,
              :QBYA-AMT-3-1-11,
              :QBYA-O2-3-1-11,
              :QBYA-INT-3-1-11,
              :QBYA-NO-3-2-1,
              :QBYA-AMT-3-2-1,
              :QBYA-O2-3-2-1,
              :QBYA-INT-3-2-1,
              :QBYA-NO-3-2-2,
              :QBYA-AMT-3-2-2,
              :QBYA-O2-3-2-2,
              :QBYA-INT-3-2-2,
              :QBYA-NO-3-2-3,
              :QBYA-AMT-3-2-3,
              :QBYA-O2-3-2-3,
              :QBYA-INT-3-2-3,
              :QBYA-NO-3-2-4,
              :QBYA-AMT-3-2-4,
              :QBYA-O2-3-2-4,
              :QBYA-INT-3-2-4,
              :QBYA-NO-3-2-5,
              :QBYA-AMT-3-2-5,
              :QBYA-O2-3-2-5,
              :QBYA-INT-3-2-5,
              :QBYA-NO-3-2-6,
              :QBYA-AMT-3-2-6,
              :QBYA-O2-3-2-6,
              :QBYA-INT-3-2-6,
              :QBYA-NO-3-2-7,
              :QBYA-AMT-3-2-7,
              :QBYA-O2-3-2-7,
              :QBYA-INT-3-2-7,
              :QBYA-NO-3-2-8,
              :QBYA-AMT-3-2-8,
              :QBYA-O2-3-2-8,
              :QBYA-INT-3-2-8,
              :QBYA-NO-3-2-9,
              :QBYA-AMT-3-2-9,
              :QBYA-O2-3-2-9,
              :QBYA-INT-3-2-9,
              :QBYA-NO-3-2-10,
              :QBYA-AMT-3-2-10,
              :QBYA-O2-3-2-10,
              :QBYA-INT-3-2-10,
              :QBYA-NO-3-2-11,
              :QBYA-AMT-3-2-11,
              :QBYA-O2-3-2-11,
              :QBYA-INT-3-2-11,
              :QBYA-NO-REPO-1-1-1,
              :QBYA-AMT-REPO-1-1-1,
              :QBYA-O2-REPO-1-1-1,
              :QBYA-INT-REPO-1-1-1,
              :QBYA-NO-REPO-1-1-2,
              :QBYA-AMT-REPO-1-1-2,
              :QBYA-O2-REPO-1-1-2,
              :QBYA-INT-REPO-1-1-2,
              :QBYA-NO-REPO-1-1-3,
              :QBYA-AMT-REPO-1-1-3,
              :QBYA-O2-REPO-1-1-3,
              :QBYA-INT-REPO-1-1-3,
              :QBYA-NO-REPO-1-1-4,
              :QBYA-AMT-REPO-1-1-4,
              :QBYA-O2-REPO-1-1-4,
              :QBYA-INT-REPO-1-1-4,
              :QBYA-NO-REPO-1-1-5,
              :QBYA-AMT-REPO-1-1-5,
              :QBYA-O2-REPO-1-1-5,
              :QBYA-INT-REPO-1-1-5,
              :QBYA-NO-REPO-1-2-1,
              :QBYA-AMT-REPO-1-2-1,
              :QBYA-O2-REPO-1-2-1,
              :QBYA-INT-REPO-1-2-1,
              :QBYA-NO-REPO-1-2-2,
              :QBYA-AMT-REPO-1-2-2,
              :QBYA-O2-REPO-1-2-2,
              :QBYA-INT-REPO-1-2-2,
              :QBYA-NO-REPO-1-2-3,
              :QBYA-AMT-REPO-1-2-3,
              :QBYA-O2-REPO-1-2-3,
              :QBYA-INT-REPO-1-2-3,
              :QBYA-NO-REPO-1-2-4,
              :QBYA-AMT-REPO-1-2-4,
              :QBYA-O2-REPO-1-2-4,
              :QBYA-INT-REPO-1-2-4,
              :QBYA-NO-REPO-1-2-5,
              :QBYA-AMT-REPO-1-2-5,
              :QBYA-O2-REPO-1-2-5,
              :QBYA-INT-REPO-1-2-5,
              :QBYA-NO-REPO-2-1-1,
              :QBYA-AMT-REPO-2-1-1,
              :QBYA-O2-REPO-2-1-1,
              :QBYA-INT-REPO-2-1-1,
              :QBYA-NO-REPO-2-1-2,
              :QBYA-AMT-REPO-2-1-2,
              :QBYA-O2-REPO-2-1-2,
              :QBYA-INT-REPO-2-1-2,
              :QBYA-NO-REPO-2-1-3,
              :QBYA-AMT-REPO-2-1-3,
              :QBYA-O2-REPO-2-1-3,
              :QBYA-INT-REPO-2-1-3,
              :QBYA-NO-REPO-2-1-4,
              :QBYA-AMT-REPO-2-1-4,
              :QBYA-O2-REPO-2-1-4,
              :QBYA-INT-REPO-2-1-4,
              :QBYA-NO-REPO-2-1-5,
              :QBYA-AMT-REPO-2-1-5,
              :QBYA-O2-REPO-2-1-5,
              :QBYA-INT-REPO-2-1-5,
              :QBYA-NO-REPO-2-2-1,
              :QBYA-AMT-REPO-2-2-1,
              :QBYA-O2-REPO-2-2-1,
              :QBYA-INT-REPO-2-2-1,
              :QBYA-NO-REPO-2-2-2,
              :QBYA-AMT-REPO-2-2-2,
              :QBYA-O2-REPO-2-2-2,
              :QBYA-INT-REPO-2-2-2,
              :QBYA-NO-REPO-2-2-3,
              :QBYA-AMT-REPO-2-2-3,
              :QBYA-O2-REPO-2-2-3,
              :QBYA-INT-REPO-2-2-3,
              :QBYA-NO-REPO-2-2-4,
              :QBYA-AMT-REPO-2-2-4,
              :QBYA-O2-REPO-2-2-4,
              :QBYA-INT-REPO-2-2-4,
              :QBYA-NO-REPO-2-2-5,
              :QBYA-AMT-REPO-2-2-5,
              :QBYA-O2-REPO-2-2-5,
              :QBYA-INT-REPO-2-2-5,
              :QBYA-NO-REPO-3-1-1,
              :QBYA-AMT-REPO-3-1-1,
              :QBYA-O2-REPO-3-1-1,
              :QBYA-INT-REPO-3-1-1,
              :QBYA-NO-REPO-3-1-2,
              :QBYA-AMT-REPO-3-1-2,
              :QBYA-O2-REPO-3-1-2,
              :QBYA-INT-REPO-3-1-2,
              :QBYA-NO-REPO-3-1-3,
              :QBYA-AMT-REPO-3-1-3,
              :QBYA-O2-REPO-3-1-3,
              :QBYA-INT-REPO-3-1-3,
              :QBYA-NO-REPO-3-1-4,
              :QBYA-AMT-REPO-3-1-4,
              :QBYA-O2-REPO-3-1-4,
              :QBYA-INT-REPO-3-1-4,
              :QBYA-NO-REPO-3-1-5,
              :QBYA-AMT-REPO-3-1-5,
              :QBYA-O2-REPO-3-1-5,
              :QBYA-INT-REPO-3-1-5,
              :QBYA-NO-REPO-3-2-1,
              :QBYA-AMT-REPO-3-2-1,
              :QBYA-O2-REPO-3-2-1,
              :QBYA-INT-REPO-3-2-1,
              :QBYA-NO-REPO-3-2-2,
              :QBYA-AMT-REPO-3-2-2,
              :QBYA-O2-REPO-3-2-2,
              :QBYA-INT-REPO-3-2-2,
              :QBYA-NO-REPO-3-2-3,
              :QBYA-AMT-REPO-3-2-3,
              :QBYA-O2-REPO-3-2-3,
              :QBYA-INT-REPO-3-2-3,
              :QBYA-NO-REPO-3-2-4,
              :QBYA-AMT-REPO-3-2-4,
              :QBYA-O2-REPO-3-2-4,
              :QBYA-INT-REPO-3-2-4,
              :QBYA-NO-REPO-3-2-5,
              :QBYA-AMT-REPO-3-2-5,
              :QBYA-O2-REPO-3-2-5,
              :QBYA-INT-REPO-3-2-5,
              :QBYA-NO-PL-1-1-1,
              :QBYA-AMT-PL-1-1-1,
              :QBYA-O2-PL-1-1-1,
              :QBYA-INT-PL-1-1-1,
              :QBYA-NO-PL-1-1-2,
              :QBYA-AMT-PL-1-1-2,
              :QBYA-O2-PL-1-1-2,
              :QBYA-INT-PL-1-1-2,
              :QBYA-NO-PL-1-1-3,
              :QBYA-AMT-PL-1-1-3,
              :QBYA-O2-PL-1-1-3,
              :QBYA-INT-PL-1-1-3,
              :QBYA-NO-PL-1-1-4,
              :QBYA-AMT-PL-1-1-4,
              :QBYA-O2-PL-1-1-4,
              :QBYA-INT-PL-1-1-4,
              :QBYA-NO-PL-1-1-5,
              :QBYA-AMT-PL-1-1-5,
              :QBYA-O2-PL-1-1-5,
              :QBYA-INT-PL-1-1-5,
              :QBYA-NO-PL-1-1-6,
              :QBYA-AMT-PL-1-1-6,
              :QBYA-O2-PL-1-1-6,
              :QBYA-INT-PL-1-1-6,
              :QBYA-NO-PL-1-1-7,
              :QBYA-AMT-PL-1-1-7,
              :QBYA-O2-PL-1-1-7,
              :QBYA-INT-PL-1-1-7,
              :QBYA-NO-PL-1-1-8,
              :QBYA-AMT-PL-1-1-8,
              :QBYA-O2-PL-1-1-8,
              :QBYA-INT-PL-1-1-8,
              :QBYA-NO-PL-1-1-9,
              :QBYA-AMT-PL-1-1-9,
              :QBYA-O2-PL-1-1-9,
              :QBYA-INT-PL-1-1-9,
              :QBYA-NO-PL-1-1-10,
              :QBYA-AMT-PL-1-1-10,
              :QBYA-O2-PL-1-1-10,
              :QBYA-INT-PL-1-1-10,
              :QBYA-NO-PL-1-2-1,
              :QBYA-AMT-PL-1-2-1,
              :QBYA-O2-PL-1-2-1,
              :QBYA-INT-PL-1-2-1,
              :QBYA-NO-PL-1-2-2,
              :QBYA-AMT-PL-1-2-2,
              :QBYA-O2-PL-1-2-2,
              :QBYA-INT-PL-1-2-2,
              :QBYA-NO-PL-1-2-3,
              :QBYA-AMT-PL-1-2-3,
              :QBYA-O2-PL-1-2-3,
              :QBYA-INT-PL-1-2-3,
              :QBYA-NO-PL-1-2-4,
              :QBYA-AMT-PL-1-2-4,
              :QBYA-O2-PL-1-2-4,
              :QBYA-INT-PL-1-2-4,
              :QBYA-NO-PL-1-2-5,
              :QBYA-AMT-PL-1-2-5,
              :QBYA-O2-PL-1-2-5,
              :QBYA-INT-PL-1-2-5,
              :QBYA-NO-PL-1-2-6,
              :QBYA-AMT-PL-1-2-6,
              :QBYA-O2-PL-1-2-6,
              :QBYA-INT-PL-1-2-6,
              :QBYA-NO-PL-1-2-7,
              :QBYA-AMT-PL-1-2-7,
              :QBYA-O2-PL-1-2-7,
              :QBYA-INT-PL-1-2-7,
              :QBYA-NO-PL-1-2-8,
              :QBYA-AMT-PL-1-2-8,
              :QBYA-O2-PL-1-2-8,
              :QBYA-INT-PL-1-2-8,
              :QBYA-NO-PL-1-2-9,
              :QBYA-AMT-PL-1-2-9,
              :QBYA-O2-PL-1-2-9,
              :QBYA-INT-PL-1-2-9,
              :QBYA-NO-PL-1-2-10,
              :QBYA-AMT-PL-1-2-10,
              :QBYA-O2-PL-1-2-10,
              :QBYA-INT-PL-1-2-10,
              :QBYA-NO-PL-2-1-1,
              :QBYA-AMT-PL-2-1-1,
              :QBYA-O2-PL-2-1-1,
              :QBYA-INT-PL-2-1-1,
              :QBYA-NO-PL-2-1-2,
              :QBYA-AMT-PL-2-1-2,
              :QBYA-O2-PL-2-1-2,
              :QBYA-INT-PL-2-1-2,
              :QBYA-NO-PL-2-1-3,
              :QBYA-AMT-PL-2-1-3,
              :QBYA-O2-PL-2-1-3,
              :QBYA-INT-PL-2-1-3,
              :QBYA-NO-PL-2-1-4,
              :QBYA-AMT-PL-2-1-4,
              :QBYA-O2-PL-2-1-4,
              :QBYA-INT-PL-2-1-4,
              :QBYA-NO-PL-2-1-5,
              :QBYA-AMT-PL-2-1-5,
              :QBYA-O2-PL-2-1-5,
              :QBYA-INT-PL-2-1-5,
              :QBYA-NO-PL-2-1-6,
              :QBYA-AMT-PL-2-1-6,
              :QBYA-O2-PL-2-1-6,
              :QBYA-INT-PL-2-1-6,
              :QBYA-NO-PL-2-1-7,
              :QBYA-AMT-PL-2-1-7,
              :QBYA-O2-PL-2-1-7,
              :QBYA-INT-PL-2-1-7,
              :QBYA-NO-PL-2-1-8,
              :QBYA-AMT-PL-2-1-8,
              :QBYA-O2-PL-2-1-8,
              :QBYA-INT-PL-2-1-8,
              :QBYA-NO-PL-2-1-9,
              :QBYA-AMT-PL-2-1-9,
              :QBYA-O2-PL-2-1-9,
              :QBYA-INT-PL-2-1-9,
              :QBYA-NO-PL-2-1-10,
              :QBYA-AMT-PL-2-1-10,
              :QBYA-O2-PL-2-1-10,
              :QBYA-INT-PL-2-1-10,
              :QBYA-NO-PL-2-2-1,
              :QBYA-AMT-PL-2-2-1,
              :QBYA-O2-PL-2-2-1,
              :QBYA-INT-PL-2-2-1,
              :QBYA-NO-PL-2-2-2,
              :QBYA-AMT-PL-2-2-2,
              :QBYA-O2-PL-2-2-2,
              :QBYA-INT-PL-2-2-2,
              :QBYA-NO-PL-2-2-3,
              :QBYA-AMT-PL-2-2-3,
              :QBYA-O2-PL-2-2-3,
              :QBYA-INT-PL-2-2-3,
              :QBYA-NO-PL-2-2-4,
              :QBYA-AMT-PL-2-2-4,
              :QBYA-O2-PL-2-2-4,
              :QBYA-INT-PL-2-2-4,
              :QBYA-NO-PL-2-2-5,
              :QBYA-AMT-PL-2-2-5,
              :QBYA-O2-PL-2-2-5,
              :QBYA-INT-PL-2-2-5,
              :QBYA-NO-PL-2-2-6,
              :QBYA-AMT-PL-2-2-6,
              :QBYA-O2-PL-2-2-6,
              :QBYA-INT-PL-2-2-6,
              :QBYA-NO-PL-2-2-7,
              :QBYA-AMT-PL-2-2-7,
              :QBYA-O2-PL-2-2-7,
              :QBYA-INT-PL-2-2-7,
              :QBYA-NO-PL-2-2-8,
              :QBYA-AMT-PL-2-2-8,
              :QBYA-O2-PL-2-2-8,
              :QBYA-INT-PL-2-2-8,
              :QBYA-NO-PL-2-2-9,
              :QBYA-AMT-PL-2-2-9,
              :QBYA-O2-PL-2-2-9,
              :QBYA-INT-PL-2-2-9,
              :QBYA-NO-PL-2-2-10,
              :QBYA-AMT-PL-2-2-10,
              :QBYA-O2-PL-2-2-10,
              :QBYA-INT-PL-2-2-10,
              :QBYA-NO-PL-3-1-1,
              :QBYA-AMT-PL-3-1-1,
              :QBYA-O2-PL-3-1-1,
              :QBYA-INT-PL-3-1-1,
              :QBYA-NO-PL-3-1-2,
              :QBYA-AMT-PL-3-1-2,
              :QBYA-O2-PL-3-1-2,
              :QBYA-INT-PL-3-1-2,
              :QBYA-NO-PL-3-1-3,
              :QBYA-AMT-PL-3-1-3,
              :QBYA-O2-PL-3-1-3,
              :QBYA-INT-PL-3-1-3,
              :QBYA-NO-PL-3-1-4,
              :QBYA-AMT-PL-3-1-4,
              :QBYA-O2-PL-3-1-4,
              :QBYA-INT-PL-3-1-4,
              :QBYA-NO-PL-3-1-5,
              :QBYA-AMT-PL-3-1-5,
              :QBYA-O2-PL-3-1-5,
              :QBYA-INT-PL-3-1-5,
              :QBYA-NO-PL-3-1-6,
              :QBYA-AMT-PL-3-1-6,
              :QBYA-O2-PL-3-1-6,
              :QBYA-INT-PL-3-1-6,
              :QBYA-NO-PL-3-1-7,
              :QBYA-AMT-PL-3-1-7,
              :QBYA-O2-PL-3-1-7,
              :QBYA-INT-PL-3-1-7,
              :QBYA-NO-PL-3-1-8,
              :QBYA-AMT-PL-3-1-8,
              :QBYA-O2-PL-3-1-8,
              :QBYA-INT-PL-3-1-8,
              :QBYA-NO-PL-3-1-9,
              :QBYA-AMT-PL-3-1-9,
              :QBYA-O2-PL-3-1-9,
              :QBYA-INT-PL-3-1-9,
              :QBYA-NO-PL-3-1-10,
              :QBYA-AMT-PL-3-1-10,
              :QBYA-O2-PL-3-1-10,
              :QBYA-INT-PL-3-1-10,
              :QBYA-NO-PL-3-2-1,
              :QBYA-AMT-PL-3-2-1,
              :QBYA-O2-PL-3-2-1,
              :QBYA-INT-PL-3-2-1,
              :QBYA-NO-PL-3-2-2,
              :QBYA-AMT-PL-3-2-2,
              :QBYA-O2-PL-3-2-2,
              :QBYA-INT-PL-3-2-2,
              :QBYA-NO-PL-3-2-3,
              :QBYA-AMT-PL-3-2-3,
              :QBYA-O2-PL-3-2-3,
              :QBYA-INT-PL-3-2-3,
              :QBYA-NO-PL-3-2-4,
              :QBYA-AMT-PL-3-2-4,
              :QBYA-O2-PL-3-2-4,
              :QBYA-INT-PL-3-2-4,
              :QBYA-NO-PL-3-2-5,
              :QBYA-AMT-PL-3-2-5,
              :QBYA-O2-PL-3-2-5,
              :QBYA-INT-PL-3-2-5,
              :QBYA-NO-PL-3-2-6,
              :QBYA-AMT-PL-3-2-6,
              :QBYA-O2-PL-3-2-6,
              :QBYA-INT-PL-3-2-6,
              :QBYA-NO-PL-3-2-7,
              :QBYA-AMT-PL-3-2-7,
              :QBYA-O2-PL-3-2-7,
              :QBYA-INT-PL-3-2-7,
              :QBYA-NO-PL-3-2-8,
              :QBYA-AMT-PL-3-2-8,
              :QBYA-O2-PL-3-2-8,
              :QBYA-INT-PL-3-2-8,
              :QBYA-NO-PL-3-2-9,
              :QBYA-AMT-PL-3-2-9,
              :QBYA-O2-PL-3-2-9,
              :QBYA-INT-PL-3-2-9,
              :QBYA-NO-PL-3-2-10,
              :QBYA-AMT-PL-3-2-10,
              :QBYA-O2-PL-3-2-10,
              :QBYA-INT-PL-3-2-10,
              :QBYA-DLRVPRIN-1-1-1,
              :QBYA-DLRVPRIN-1-1-2,
              :QBYA-DLRVPRIN-1-1-3,
              :QBYA-DLRVCHGS-1-1-1,
              :QBYA-DLRVCHGS-1-1-2,
              :QBYA-DLRVCHGS-1-1-3,
              :QBYA-DLPARVHELD-1-1,
              :QBYA-DLRVPRIN-1-2-1,
              :QBYA-DLRVPRIN-1-2-2,
              :QBYA-DLRVPRIN-1-2-3,
              :QBYA-DLRVCHGS-1-2-1,
              :QBYA-DLRVCHGS-1-2-2,
              :QBYA-DLRVCHGS-1-2-3,
              :QBYA-DLPARVHELD-1-2,
              :QBYA-DLRVPRIN-2-1-1,
              :QBYA-DLRVPRIN-2-1-2,
              :QBYA-DLRVPRIN-2-1-3,
              :QBYA-DLRVCHGS-2-1-1,
              :QBYA-DLRVCHGS-2-1-2,
              :QBYA-DLRVCHGS-2-1-3,
              :QBYA-DLPARVHELD-2-1,
              :QBYA-DLRVPRIN-2-2-1,
              :QBYA-DLRVPRIN-2-2-2,
              :QBYA-DLRVPRIN-2-2-3,
              :QBYA-DLRVCHGS-2-2-1,
              :QBYA-DLRVCHGS-2-2-2,
              :QBYA-DLRVCHGS-2-2-3,
              :QBYA-DLPARVHELD-2-2,
              :QBYA-DLRVPRIN-3-1-1,
              :QBYA-DLRVPRIN-3-1-2,
              :QBYA-DLRVPRIN-3-1-3,
              :QBYA-DLRVCHGS-3-1-1,
              :QBYA-DLRVCHGS-3-1-2,
              :QBYA-DLRVCHGS-3-1-3,
              :QBYA-DLPARVHELD-3-1,
              :QBYA-DLRVPRIN-3-2-1,
              :QBYA-DLRVPRIN-3-2-2,
              :QBYA-DLRVPRIN-3-2-3,
              :QBYA-DLRVCHGS-3-2-1,
              :QBYA-DLRVCHGS-3-2-2,
              :QBYA-DLRVCHGS-3-2-3,
              :QBYA-DLPARVHELD-3-2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BYA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      * USE C1 CONNECT
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYA1-FILE. *> EMBEDDED - BYAFILE

           ACCEPT BYA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYAFILE
             SET
              BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM,
              BYAFILE.BYA_BRNO = :QBYA-BRNO,
              BYAFILE.BYA_CLASS = :QBYA-CLASS,
              BYAFILE.BYA_LTOUCH_DATE = :QBYA-LTOUCH-DATE,
              BYAFILE.BYA_NO_1_1_1 = :QBYA-NO-1-1-1,
              BYAFILE.BYA_AMT_1_1_1 = :QBYA-AMT-1-1-1,
              BYAFILE.BYA_O2_1_1_1 = :QBYA-O2-1-1-1,
              BYAFILE.BYA_INT_1_1_1 = :QBYA-INT-1-1-1,
              BYAFILE.BYA_NO_1_1_2 = :QBYA-NO-1-1-2,
              BYAFILE.BYA_AMT_1_1_2 = :QBYA-AMT-1-1-2,
              BYAFILE.BYA_O2_1_1_2 = :QBYA-O2-1-1-2,
              BYAFILE.BYA_INT_1_1_2 = :QBYA-INT-1-1-2,
              BYAFILE.BYA_NO_1_1_3 = :QBYA-NO-1-1-3,
              BYAFILE.BYA_AMT_1_1_3 = :QBYA-AMT-1-1-3,
              BYAFILE.BYA_O2_1_1_3 = :QBYA-O2-1-1-3,
              BYAFILE.BYA_INT_1_1_3 = :QBYA-INT-1-1-3,
              BYAFILE.BYA_NO_1_1_4 = :QBYA-NO-1-1-4,
              BYAFILE.BYA_AMT_1_1_4 = :QBYA-AMT-1-1-4,
              BYAFILE.BYA_O2_1_1_4 = :QBYA-O2-1-1-4,
              BYAFILE.BYA_INT_1_1_4 = :QBYA-INT-1-1-4,
              BYAFILE.BYA_NO_1_1_5 = :QBYA-NO-1-1-5,
              BYAFILE.BYA_AMT_1_1_5 = :QBYA-AMT-1-1-5,
              BYAFILE.BYA_O2_1_1_5 = :QBYA-O2-1-1-5,
              BYAFILE.BYA_INT_1_1_5 = :QBYA-INT-1-1-5,
              BYAFILE.BYA_NO_1_1_6 = :QBYA-NO-1-1-6,
              BYAFILE.BYA_AMT_1_1_6 = :QBYA-AMT-1-1-6,
              BYAFILE.BYA_O2_1_1_6 = :QBYA-O2-1-1-6,
              BYAFILE.BYA_INT_1_1_6 = :QBYA-INT-1-1-6,
              BYAFILE.BYA_NO_1_1_7 = :QBYA-NO-1-1-7,
              BYAFILE.BYA_AMT_1_1_7 = :QBYA-AMT-1-1-7,
              BYAFILE.BYA_O2_1_1_7 = :QBYA-O2-1-1-7,
              BYAFILE.BYA_INT_1_1_7 = :QBYA-INT-1-1-7,
              BYAFILE.BYA_NO_1_1_8 = :QBYA-NO-1-1-8,
              BYAFILE.BYA_AMT_1_1_8 = :QBYA-AMT-1-1-8,
              BYAFILE.BYA_O2_1_1_8 = :QBYA-O2-1-1-8,
              BYAFILE.BYA_INT_1_1_8 = :QBYA-INT-1-1-8,
              BYAFILE.BYA_NO_1_1_9 = :QBYA-NO-1-1-9,
              BYAFILE.BYA_AMT_1_1_9 = :QBYA-AMT-1-1-9,
              BYAFILE.BYA_O2_1_1_9 = :QBYA-O2-1-1-9,
              BYAFILE.BYA_INT_1_1_9 = :QBYA-INT-1-1-9,
              BYAFILE.BYA_NO_1_1_10 = :QBYA-NO-1-1-10,
              BYAFILE.BYA_AMT_1_1_10 = :QBYA-AMT-1-1-10,
              BYAFILE.BYA_O2_1_1_10 = :QBYA-O2-1-1-10,
              BYAFILE.BYA_INT_1_1_10 = :QBYA-INT-1-1-10,
              BYAFILE.BYA_NO_1_1_11 = :QBYA-NO-1-1-11,
              BYAFILE.BYA_AMT_1_1_11 = :QBYA-AMT-1-1-11,
              BYAFILE.BYA_O2_1_1_11 = :QBYA-O2-1-1-11,
              BYAFILE.BYA_INT_1_1_11 = :QBYA-INT-1-1-11,
              BYAFILE.BYA_NO_1_2_1 = :QBYA-NO-1-2-1,
              BYAFILE.BYA_AMT_1_2_1 = :QBYA-AMT-1-2-1,
              BYAFILE.BYA_O2_1_2_1 = :QBYA-O2-1-2-1,
              BYAFILE.BYA_INT_1_2_1 = :QBYA-INT-1-2-1,
              BYAFILE.BYA_NO_1_2_2 = :QBYA-NO-1-2-2,
              BYAFILE.BYA_AMT_1_2_2 = :QBYA-AMT-1-2-2,
              BYAFILE.BYA_O2_1_2_2 = :QBYA-O2-1-2-2,
              BYAFILE.BYA_INT_1_2_2 = :QBYA-INT-1-2-2,
              BYAFILE.BYA_NO_1_2_3 = :QBYA-NO-1-2-3,
              BYAFILE.BYA_AMT_1_2_3 = :QBYA-AMT-1-2-3,
              BYAFILE.BYA_O2_1_2_3 = :QBYA-O2-1-2-3,
              BYAFILE.BYA_INT_1_2_3 = :QBYA-INT-1-2-3,
              BYAFILE.BYA_NO_1_2_4 = :QBYA-NO-1-2-4,
              BYAFILE.BYA_AMT_1_2_4 = :QBYA-AMT-1-2-4,
              BYAFILE.BYA_O2_1_2_4 = :QBYA-O2-1-2-4,
              BYAFILE.BYA_INT_1_2_4 = :QBYA-INT-1-2-4,
              BYAFILE.BYA_NO_1_2_5 = :QBYA-NO-1-2-5,
              BYAFILE.BYA_AMT_1_2_5 = :QBYA-AMT-1-2-5,
              BYAFILE.BYA_O2_1_2_5 = :QBYA-O2-1-2-5,
              BYAFILE.BYA_INT_1_2_5 = :QBYA-INT-1-2-5,
              BYAFILE.BYA_NO_1_2_6 = :QBYA-NO-1-2-6,
              BYAFILE.BYA_AMT_1_2_6 = :QBYA-AMT-1-2-6,
              BYAFILE.BYA_O2_1_2_6 = :QBYA-O2-1-2-6,
              BYAFILE.BYA_INT_1_2_6 = :QBYA-INT-1-2-6,
              BYAFILE.BYA_NO_1_2_7 = :QBYA-NO-1-2-7,
              BYAFILE.BYA_AMT_1_2_7 = :QBYA-AMT-1-2-7,
              BYAFILE.BYA_O2_1_2_7 = :QBYA-O2-1-2-7,
              BYAFILE.BYA_INT_1_2_7 = :QBYA-INT-1-2-7,
              BYAFILE.BYA_NO_1_2_8 = :QBYA-NO-1-2-8,
              BYAFILE.BYA_AMT_1_2_8 = :QBYA-AMT-1-2-8,
              BYAFILE.BYA_O2_1_2_8 = :QBYA-O2-1-2-8,
              BYAFILE.BYA_INT_1_2_8 = :QBYA-INT-1-2-8,
              BYAFILE.BYA_NO_1_2_9 = :QBYA-NO-1-2-9,
              BYAFILE.BYA_AMT_1_2_9 = :QBYA-AMT-1-2-9,
              BYAFILE.BYA_O2_1_2_9 = :QBYA-O2-1-2-9,
              BYAFILE.BYA_INT_1_2_9 = :QBYA-INT-1-2-9,
              BYAFILE.BYA_NO_1_2_10 = :QBYA-NO-1-2-10,
              BYAFILE.BYA_AMT_1_2_10 = :QBYA-AMT-1-2-10,
              BYAFILE.BYA_O2_1_2_10 = :QBYA-O2-1-2-10,
              BYAFILE.BYA_INT_1_2_10 = :QBYA-INT-1-2-10,
              BYAFILE.BYA_NO_1_2_11 = :QBYA-NO-1-2-11,
              BYAFILE.BYA_AMT_1_2_11 = :QBYA-AMT-1-2-11,
              BYAFILE.BYA_O2_1_2_11 = :QBYA-O2-1-2-11,
              BYAFILE.BYA_INT_1_2_11 = :QBYA-INT-1-2-11,
              BYAFILE.BYA_NO_2_1_1 = :QBYA-NO-2-1-1,
              BYAFILE.BYA_AMT_2_1_1 = :QBYA-AMT-2-1-1,
              BYAFILE.BYA_O2_2_1_1 = :QBYA-O2-2-1-1,
              BYAFILE.BYA_INT_2_1_1 = :QBYA-INT-2-1-1,
              BYAFILE.BYA_NO_2_1_2 = :QBYA-NO-2-1-2,
              BYAFILE.BYA_AMT_2_1_2 = :QBYA-AMT-2-1-2,
              BYAFILE.BYA_O2_2_1_2 = :QBYA-O2-2-1-2,
              BYAFILE.BYA_INT_2_1_2 = :QBYA-INT-2-1-2,
              BYAFILE.BYA_NO_2_1_3 = :QBYA-NO-2-1-3,
              BYAFILE.BYA_AMT_2_1_3 = :QBYA-AMT-2-1-3,
              BYAFILE.BYA_O2_2_1_3 = :QBYA-O2-2-1-3,
              BYAFILE.BYA_INT_2_1_3 = :QBYA-INT-2-1-3,
              BYAFILE.BYA_NO_2_1_4 = :QBYA-NO-2-1-4,
              BYAFILE.BYA_AMT_2_1_4 = :QBYA-AMT-2-1-4,
              BYAFILE.BYA_O2_2_1_4 = :QBYA-O2-2-1-4,
              BYAFILE.BYA_INT_2_1_4 = :QBYA-INT-2-1-4,
              BYAFILE.BYA_NO_2_1_5 = :QBYA-NO-2-1-5,
              BYAFILE.BYA_AMT_2_1_5 = :QBYA-AMT-2-1-5,
              BYAFILE.BYA_O2_2_1_5 = :QBYA-O2-2-1-5,
              BYAFILE.BYA_INT_2_1_5 = :QBYA-INT-2-1-5,
              BYAFILE.BYA_NO_2_1_6 = :QBYA-NO-2-1-6,
              BYAFILE.BYA_AMT_2_1_6 = :QBYA-AMT-2-1-6,
              BYAFILE.BYA_O2_2_1_6 = :QBYA-O2-2-1-6,
              BYAFILE.BYA_INT_2_1_6 = :QBYA-INT-2-1-6,
              BYAFILE.BYA_NO_2_1_7 = :QBYA-NO-2-1-7,
              BYAFILE.BYA_AMT_2_1_7 = :QBYA-AMT-2-1-7,
              BYAFILE.BYA_O2_2_1_7 = :QBYA-O2-2-1-7,
              BYAFILE.BYA_INT_2_1_7 = :QBYA-INT-2-1-7,
              BYAFILE.BYA_NO_2_1_8 = :QBYA-NO-2-1-8,
              BYAFILE.BYA_AMT_2_1_8 = :QBYA-AMT-2-1-8,
              BYAFILE.BYA_O2_2_1_8 = :QBYA-O2-2-1-8,
              BYAFILE.BYA_INT_2_1_8 = :QBYA-INT-2-1-8,
              BYAFILE.BYA_NO_2_1_9 = :QBYA-NO-2-1-9,
              BYAFILE.BYA_AMT_2_1_9 = :QBYA-AMT-2-1-9,
              BYAFILE.BYA_O2_2_1_9 = :QBYA-O2-2-1-9,
              BYAFILE.BYA_INT_2_1_9 = :QBYA-INT-2-1-9,
              BYAFILE.BYA_NO_2_1_10 = :QBYA-NO-2-1-10,
              BYAFILE.BYA_AMT_2_1_10 = :QBYA-AMT-2-1-10,
              BYAFILE.BYA_O2_2_1_10 = :QBYA-O2-2-1-10,
              BYAFILE.BYA_INT_2_1_10 = :QBYA-INT-2-1-10,
              BYAFILE.BYA_NO_2_1_11 = :QBYA-NO-2-1-11,
              BYAFILE.BYA_AMT_2_1_11 = :QBYA-AMT-2-1-11,
              BYAFILE.BYA_O2_2_1_11 = :QBYA-O2-2-1-11,
              BYAFILE.BYA_INT_2_1_11 = :QBYA-INT-2-1-11,
              BYAFILE.BYA_NO_2_2_1 = :QBYA-NO-2-2-1,
              BYAFILE.BYA_AMT_2_2_1 = :QBYA-AMT-2-2-1,
              BYAFILE.BYA_O2_2_2_1 = :QBYA-O2-2-2-1,
              BYAFILE.BYA_INT_2_2_1 = :QBYA-INT-2-2-1,
              BYAFILE.BYA_NO_2_2_2 = :QBYA-NO-2-2-2,
              BYAFILE.BYA_AMT_2_2_2 = :QBYA-AMT-2-2-2,
              BYAFILE.BYA_O2_2_2_2 = :QBYA-O2-2-2-2,
              BYAFILE.BYA_INT_2_2_2 = :QBYA-INT-2-2-2,
              BYAFILE.BYA_NO_2_2_3 = :QBYA-NO-2-2-3,
              BYAFILE.BYA_AMT_2_2_3 = :QBYA-AMT-2-2-3,
              BYAFILE.BYA_O2_2_2_3 = :QBYA-O2-2-2-3,
              BYAFILE.BYA_INT_2_2_3 = :QBYA-INT-2-2-3,
              BYAFILE.BYA_NO_2_2_4 = :QBYA-NO-2-2-4,
              BYAFILE.BYA_AMT_2_2_4 = :QBYA-AMT-2-2-4,
              BYAFILE.BYA_O2_2_2_4 = :QBYA-O2-2-2-4,
              BYAFILE.BYA_INT_2_2_4 = :QBYA-INT-2-2-4,
              BYAFILE.BYA_NO_2_2_5 = :QBYA-NO-2-2-5,
              BYAFILE.BYA_AMT_2_2_5 = :QBYA-AMT-2-2-5,
              BYAFILE.BYA_O2_2_2_5 = :QBYA-O2-2-2-5,
              BYAFILE.BYA_INT_2_2_5 = :QBYA-INT-2-2-5,
              BYAFILE.BYA_NO_2_2_6 = :QBYA-NO-2-2-6,
              BYAFILE.BYA_AMT_2_2_6 = :QBYA-AMT-2-2-6,
              BYAFILE.BYA_O2_2_2_6 = :QBYA-O2-2-2-6,
              BYAFILE.BYA_INT_2_2_6 = :QBYA-INT-2-2-6,
              BYAFILE.BYA_NO_2_2_7 = :QBYA-NO-2-2-7,
              BYAFILE.BYA_AMT_2_2_7 = :QBYA-AMT-2-2-7,
              BYAFILE.BYA_O2_2_2_7 = :QBYA-O2-2-2-7,
              BYAFILE.BYA_INT_2_2_7 = :QBYA-INT-2-2-7,
              BYAFILE.BYA_NO_2_2_8 = :QBYA-NO-2-2-8,
              BYAFILE.BYA_AMT_2_2_8 = :QBYA-AMT-2-2-8,
              BYAFILE.BYA_O2_2_2_8 = :QBYA-O2-2-2-8,
              BYAFILE.BYA_INT_2_2_8 = :QBYA-INT-2-2-8,
              BYAFILE.BYA_NO_2_2_9 = :QBYA-NO-2-2-9,
              BYAFILE.BYA_AMT_2_2_9 = :QBYA-AMT-2-2-9,
              BYAFILE.BYA_O2_2_2_9 = :QBYA-O2-2-2-9,
              BYAFILE.BYA_INT_2_2_9 = :QBYA-INT-2-2-9,
              BYAFILE.BYA_NO_2_2_10 = :QBYA-NO-2-2-10,
              BYAFILE.BYA_AMT_2_2_10 = :QBYA-AMT-2-2-10,
              BYAFILE.BYA_O2_2_2_10 = :QBYA-O2-2-2-10,
              BYAFILE.BYA_INT_2_2_10 = :QBYA-INT-2-2-10,
              BYAFILE.BYA_NO_2_2_11 = :QBYA-NO-2-2-11,
              BYAFILE.BYA_AMT_2_2_11 = :QBYA-AMT-2-2-11,
              BYAFILE.BYA_O2_2_2_11 = :QBYA-O2-2-2-11,
              BYAFILE.BYA_INT_2_2_11 = :QBYA-INT-2-2-11,
              BYAFILE.BYA_NO_3_1_1 = :QBYA-NO-3-1-1,
              BYAFILE.BYA_AMT_3_1_1 = :QBYA-AMT-3-1-1,
              BYAFILE.BYA_O2_3_1_1 = :QBYA-O2-3-1-1,
              BYAFILE.BYA_INT_3_1_1 = :QBYA-INT-3-1-1,
              BYAFILE.BYA_NO_3_1_2 = :QBYA-NO-3-1-2,
              BYAFILE.BYA_AMT_3_1_2 = :QBYA-AMT-3-1-2,
              BYAFILE.BYA_O2_3_1_2 = :QBYA-O2-3-1-2,
              BYAFILE.BYA_INT_3_1_2 = :QBYA-INT-3-1-2,
              BYAFILE.BYA_NO_3_1_3 = :QBYA-NO-3-1-3,
              BYAFILE.BYA_AMT_3_1_3 = :QBYA-AMT-3-1-3,
              BYAFILE.BYA_O2_3_1_3 = :QBYA-O2-3-1-3,
              BYAFILE.BYA_INT_3_1_3 = :QBYA-INT-3-1-3,
              BYAFILE.BYA_NO_3_1_4 = :QBYA-NO-3-1-4,
              BYAFILE.BYA_AMT_3_1_4 = :QBYA-AMT-3-1-4,
              BYAFILE.BYA_O2_3_1_4 = :QBYA-O2-3-1-4,
              BYAFILE.BYA_INT_3_1_4 = :QBYA-INT-3-1-4,
              BYAFILE.BYA_NO_3_1_5 = :QBYA-NO-3-1-5,
              BYAFILE.BYA_AMT_3_1_5 = :QBYA-AMT-3-1-5,
              BYAFILE.BYA_O2_3_1_5 = :QBYA-O2-3-1-5,
              BYAFILE.BYA_INT_3_1_5 = :QBYA-INT-3-1-5,
              BYAFILE.BYA_NO_3_1_6 = :QBYA-NO-3-1-6,
              BYAFILE.BYA_AMT_3_1_6 = :QBYA-AMT-3-1-6,
              BYAFILE.BYA_O2_3_1_6 = :QBYA-O2-3-1-6,
              BYAFILE.BYA_INT_3_1_6 = :QBYA-INT-3-1-6,
              BYAFILE.BYA_NO_3_1_7 = :QBYA-NO-3-1-7,
              BYAFILE.BYA_AMT_3_1_7 = :QBYA-AMT-3-1-7,
              BYAFILE.BYA_O2_3_1_7 = :QBYA-O2-3-1-7,
              BYAFILE.BYA_INT_3_1_7 = :QBYA-INT-3-1-7,
              BYAFILE.BYA_NO_3_1_8 = :QBYA-NO-3-1-8,
              BYAFILE.BYA_AMT_3_1_8 = :QBYA-AMT-3-1-8,
              BYAFILE.BYA_O2_3_1_8 = :QBYA-O2-3-1-8,
              BYAFILE.BYA_INT_3_1_8 = :QBYA-INT-3-1-8,
              BYAFILE.BYA_NO_3_1_9 = :QBYA-NO-3-1-9,
              BYAFILE.BYA_AMT_3_1_9 = :QBYA-AMT-3-1-9,
              BYAFILE.BYA_O2_3_1_9 = :QBYA-O2-3-1-9,
              BYAFILE.BYA_INT_3_1_9 = :QBYA-INT-3-1-9,
              BYAFILE.BYA_NO_3_1_10 = :QBYA-NO-3-1-10,
              BYAFILE.BYA_AMT_3_1_10 = :QBYA-AMT-3-1-10,
              BYAFILE.BYA_O2_3_1_10 = :QBYA-O2-3-1-10,
              BYAFILE.BYA_INT_3_1_10 = :QBYA-INT-3-1-10,
              BYAFILE.BYA_NO_3_1_11 = :QBYA-NO-3-1-11,
              BYAFILE.BYA_AMT_3_1_11 = :QBYA-AMT-3-1-11,
              BYAFILE.BYA_O2_3_1_11 = :QBYA-O2-3-1-11,
              BYAFILE.BYA_INT_3_1_11 = :QBYA-INT-3-1-11,
              BYAFILE.BYA_NO_3_2_1 = :QBYA-NO-3-2-1,
              BYAFILE.BYA_AMT_3_2_1 = :QBYA-AMT-3-2-1,
              BYAFILE.BYA_O2_3_2_1 = :QBYA-O2-3-2-1,
              BYAFILE.BYA_INT_3_2_1 = :QBYA-INT-3-2-1,
              BYAFILE.BYA_NO_3_2_2 = :QBYA-NO-3-2-2,
              BYAFILE.BYA_AMT_3_2_2 = :QBYA-AMT-3-2-2,
              BYAFILE.BYA_O2_3_2_2 = :QBYA-O2-3-2-2,
              BYAFILE.BYA_INT_3_2_2 = :QBYA-INT-3-2-2,
              BYAFILE.BYA_NO_3_2_3 = :QBYA-NO-3-2-3,
              BYAFILE.BYA_AMT_3_2_3 = :QBYA-AMT-3-2-3,
              BYAFILE.BYA_O2_3_2_3 = :QBYA-O2-3-2-3,
              BYAFILE.BYA_INT_3_2_3 = :QBYA-INT-3-2-3,
              BYAFILE.BYA_NO_3_2_4 = :QBYA-NO-3-2-4,
              BYAFILE.BYA_AMT_3_2_4 = :QBYA-AMT-3-2-4,
              BYAFILE.BYA_O2_3_2_4 = :QBYA-O2-3-2-4,
              BYAFILE.BYA_INT_3_2_4 = :QBYA-INT-3-2-4,
              BYAFILE.BYA_NO_3_2_5 = :QBYA-NO-3-2-5,
              BYAFILE.BYA_AMT_3_2_5 = :QBYA-AMT-3-2-5,
              BYAFILE.BYA_O2_3_2_5 = :QBYA-O2-3-2-5,
              BYAFILE.BYA_INT_3_2_5 = :QBYA-INT-3-2-5,
              BYAFILE.BYA_NO_3_2_6 = :QBYA-NO-3-2-6,
              BYAFILE.BYA_AMT_3_2_6 = :QBYA-AMT-3-2-6,
              BYAFILE.BYA_O2_3_2_6 = :QBYA-O2-3-2-6,
              BYAFILE.BYA_INT_3_2_6 = :QBYA-INT-3-2-6,
              BYAFILE.BYA_NO_3_2_7 = :QBYA-NO-3-2-7,
              BYAFILE.BYA_AMT_3_2_7 = :QBYA-AMT-3-2-7,
              BYAFILE.BYA_O2_3_2_7 = :QBYA-O2-3-2-7,
              BYAFILE.BYA_INT_3_2_7 = :QBYA-INT-3-2-7,
              BYAFILE.BYA_NO_3_2_8 = :QBYA-NO-3-2-8,
              BYAFILE.BYA_AMT_3_2_8 = :QBYA-AMT-3-2-8,
              BYAFILE.BYA_O2_3_2_8 = :QBYA-O2-3-2-8,
              BYAFILE.BYA_INT_3_2_8 = :QBYA-INT-3-2-8,
              BYAFILE.BYA_NO_3_2_9 = :QBYA-NO-3-2-9,
              BYAFILE.BYA_AMT_3_2_9 = :QBYA-AMT-3-2-9,
              BYAFILE.BYA_O2_3_2_9 = :QBYA-O2-3-2-9,
              BYAFILE.BYA_INT_3_2_9 = :QBYA-INT-3-2-9,
              BYAFILE.BYA_NO_3_2_10 = :QBYA-NO-3-2-10,
              BYAFILE.BYA_AMT_3_2_10 = :QBYA-AMT-3-2-10,
              BYAFILE.BYA_O2_3_2_10 = :QBYA-O2-3-2-10,
              BYAFILE.BYA_INT_3_2_10 = :QBYA-INT-3-2-10,
              BYAFILE.BYA_NO_3_2_11 = :QBYA-NO-3-2-11,
              BYAFILE.BYA_AMT_3_2_11 = :QBYA-AMT-3-2-11,
              BYAFILE.BYA_O2_3_2_11 = :QBYA-O2-3-2-11,
              BYAFILE.BYA_INT_3_2_11 = :QBYA-INT-3-2-11,
              BYAFILE.BYA_NO_REPO_1_1_1 = :QBYA-NO-REPO-1-1-1,
              BYAFILE.BYA_AMT_REPO_1_1_1 = :QBYA-AMT-REPO-1-1-1,
              BYAFILE.BYA_O2_REPO_1_1_1 = :QBYA-O2-REPO-1-1-1,
              BYAFILE.BYA_INT_REPO_1_1_1 = :QBYA-INT-REPO-1-1-1,
              BYAFILE.BYA_NO_REPO_1_1_2 = :QBYA-NO-REPO-1-1-2,
              BYAFILE.BYA_AMT_REPO_1_1_2 = :QBYA-AMT-REPO-1-1-2,
              BYAFILE.BYA_O2_REPO_1_1_2 = :QBYA-O2-REPO-1-1-2,
              BYAFILE.BYA_INT_REPO_1_1_2 = :QBYA-INT-REPO-1-1-2,
              BYAFILE.BYA_NO_REPO_1_1_3 = :QBYA-NO-REPO-1-1-3,
              BYAFILE.BYA_AMT_REPO_1_1_3 = :QBYA-AMT-REPO-1-1-3,
              BYAFILE.BYA_O2_REPO_1_1_3 = :QBYA-O2-REPO-1-1-3,
              BYAFILE.BYA_INT_REPO_1_1_3 = :QBYA-INT-REPO-1-1-3,
              BYAFILE.BYA_NO_REPO_1_1_4 = :QBYA-NO-REPO-1-1-4,
              BYAFILE.BYA_AMT_REPO_1_1_4 = :QBYA-AMT-REPO-1-1-4,
              BYAFILE.BYA_O2_REPO_1_1_4 = :QBYA-O2-REPO-1-1-4,
              BYAFILE.BYA_INT_REPO_1_1_4 = :QBYA-INT-REPO-1-1-4,
              BYAFILE.BYA_NO_REPO_1_1_5 = :QBYA-NO-REPO-1-1-5,
              BYAFILE.BYA_AMT_REPO_1_1_5 = :QBYA-AMT-REPO-1-1-5,
              BYAFILE.BYA_O2_REPO_1_1_5 = :QBYA-O2-REPO-1-1-5,
              BYAFILE.BYA_INT_REPO_1_1_5 = :QBYA-INT-REPO-1-1-5,
              BYAFILE.BYA_NO_REPO_1_2_1 = :QBYA-NO-REPO-1-2-1,
              BYAFILE.BYA_AMT_REPO_1_2_1 = :QBYA-AMT-REPO-1-2-1,
              BYAFILE.BYA_O2_REPO_1_2_1 = :QBYA-O2-REPO-1-2-1,
              BYAFILE.BYA_INT_REPO_1_2_1 = :QBYA-INT-REPO-1-2-1,
              BYAFILE.BYA_NO_REPO_1_2_2 = :QBYA-NO-REPO-1-2-2,
              BYAFILE.BYA_AMT_REPO_1_2_2 = :QBYA-AMT-REPO-1-2-2,
              BYAFILE.BYA_O2_REPO_1_2_2 = :QBYA-O2-REPO-1-2-2,
              BYAFILE.BYA_INT_REPO_1_2_2 = :QBYA-INT-REPO-1-2-2,
              BYAFILE.BYA_NO_REPO_1_2_3 = :QBYA-NO-REPO-1-2-3,
              BYAFILE.BYA_AMT_REPO_1_2_3 = :QBYA-AMT-REPO-1-2-3,
              BYAFILE.BYA_O2_REPO_1_2_3 = :QBYA-O2-REPO-1-2-3,
              BYAFILE.BYA_INT_REPO_1_2_3 = :QBYA-INT-REPO-1-2-3,
              BYAFILE.BYA_NO_REPO_1_2_4 = :QBYA-NO-REPO-1-2-4,
              BYAFILE.BYA_AMT_REPO_1_2_4 = :QBYA-AMT-REPO-1-2-4,
              BYAFILE.BYA_O2_REPO_1_2_4 = :QBYA-O2-REPO-1-2-4,
              BYAFILE.BYA_INT_REPO_1_2_4 = :QBYA-INT-REPO-1-2-4,
              BYAFILE.BYA_NO_REPO_1_2_5 = :QBYA-NO-REPO-1-2-5,
              BYAFILE.BYA_AMT_REPO_1_2_5 = :QBYA-AMT-REPO-1-2-5,
              BYAFILE.BYA_O2_REPO_1_2_5 = :QBYA-O2-REPO-1-2-5,
              BYAFILE.BYA_INT_REPO_1_2_5 = :QBYA-INT-REPO-1-2-5,
              BYAFILE.BYA_NO_REPO_2_1_1 = :QBYA-NO-REPO-2-1-1,
              BYAFILE.BYA_AMT_REPO_2_1_1 = :QBYA-AMT-REPO-2-1-1,
              BYAFILE.BYA_O2_REPO_2_1_1 = :QBYA-O2-REPO-2-1-1,
              BYAFILE.BYA_INT_REPO_2_1_1 = :QBYA-INT-REPO-2-1-1,
              BYAFILE.BYA_NO_REPO_2_1_2 = :QBYA-NO-REPO-2-1-2,
              BYAFILE.BYA_AMT_REPO_2_1_2 = :QBYA-AMT-REPO-2-1-2,
              BYAFILE.BYA_O2_REPO_2_1_2 = :QBYA-O2-REPO-2-1-2,
              BYAFILE.BYA_INT_REPO_2_1_2 = :QBYA-INT-REPO-2-1-2,
              BYAFILE.BYA_NO_REPO_2_1_3 = :QBYA-NO-REPO-2-1-3,
              BYAFILE.BYA_AMT_REPO_2_1_3 = :QBYA-AMT-REPO-2-1-3,
              BYAFILE.BYA_O2_REPO_2_1_3 = :QBYA-O2-REPO-2-1-3,
              BYAFILE.BYA_INT_REPO_2_1_3 = :QBYA-INT-REPO-2-1-3,
              BYAFILE.BYA_NO_REPO_2_1_4 = :QBYA-NO-REPO-2-1-4,
              BYAFILE.BYA_AMT_REPO_2_1_4 = :QBYA-AMT-REPO-2-1-4,
              BYAFILE.BYA_O2_REPO_2_1_4 = :QBYA-O2-REPO-2-1-4,
              BYAFILE.BYA_INT_REPO_2_1_4 = :QBYA-INT-REPO-2-1-4,
              BYAFILE.BYA_NO_REPO_2_1_5 = :QBYA-NO-REPO-2-1-5,
              BYAFILE.BYA_AMT_REPO_2_1_5 = :QBYA-AMT-REPO-2-1-5,
              BYAFILE.BYA_O2_REPO_2_1_5 = :QBYA-O2-REPO-2-1-5,
              BYAFILE.BYA_INT_REPO_2_1_5 = :QBYA-INT-REPO-2-1-5,
              BYAFILE.BYA_NO_REPO_2_2_1 = :QBYA-NO-REPO-2-2-1,
              BYAFILE.BYA_AMT_REPO_2_2_1 = :QBYA-AMT-REPO-2-2-1,
              BYAFILE.BYA_O2_REPO_2_2_1 = :QBYA-O2-REPO-2-2-1,
              BYAFILE.BYA_INT_REPO_2_2_1 = :QBYA-INT-REPO-2-2-1,
              BYAFILE.BYA_NO_REPO_2_2_2 = :QBYA-NO-REPO-2-2-2,
              BYAFILE.BYA_AMT_REPO_2_2_2 = :QBYA-AMT-REPO-2-2-2,
              BYAFILE.BYA_O2_REPO_2_2_2 = :QBYA-O2-REPO-2-2-2,
              BYAFILE.BYA_INT_REPO_2_2_2 = :QBYA-INT-REPO-2-2-2,
              BYAFILE.BYA_NO_REPO_2_2_3 = :QBYA-NO-REPO-2-2-3,
              BYAFILE.BYA_AMT_REPO_2_2_3 = :QBYA-AMT-REPO-2-2-3,
              BYAFILE.BYA_O2_REPO_2_2_3 = :QBYA-O2-REPO-2-2-3,
              BYAFILE.BYA_INT_REPO_2_2_3 = :QBYA-INT-REPO-2-2-3,
              BYAFILE.BYA_NO_REPO_2_2_4 = :QBYA-NO-REPO-2-2-4,
              BYAFILE.BYA_AMT_REPO_2_2_4 = :QBYA-AMT-REPO-2-2-4,
              BYAFILE.BYA_O2_REPO_2_2_4 = :QBYA-O2-REPO-2-2-4,
              BYAFILE.BYA_INT_REPO_2_2_4 = :QBYA-INT-REPO-2-2-4,
              BYAFILE.BYA_NO_REPO_2_2_5 = :QBYA-NO-REPO-2-2-5,
              BYAFILE.BYA_AMT_REPO_2_2_5 = :QBYA-AMT-REPO-2-2-5,
              BYAFILE.BYA_O2_REPO_2_2_5 = :QBYA-O2-REPO-2-2-5,
              BYAFILE.BYA_INT_REPO_2_2_5 = :QBYA-INT-REPO-2-2-5,
              BYAFILE.BYA_NO_REPO_3_1_1 = :QBYA-NO-REPO-3-1-1,
              BYAFILE.BYA_AMT_REPO_3_1_1 = :QBYA-AMT-REPO-3-1-1,
              BYAFILE.BYA_O2_REPO_3_1_1 = :QBYA-O2-REPO-3-1-1,
              BYAFILE.BYA_INT_REPO_3_1_1 = :QBYA-INT-REPO-3-1-1,
              BYAFILE.BYA_NO_REPO_3_1_2 = :QBYA-NO-REPO-3-1-2,
              BYAFILE.BYA_AMT_REPO_3_1_2 = :QBYA-AMT-REPO-3-1-2,
              BYAFILE.BYA_O2_REPO_3_1_2 = :QBYA-O2-REPO-3-1-2,
              BYAFILE.BYA_INT_REPO_3_1_2 = :QBYA-INT-REPO-3-1-2,
              BYAFILE.BYA_NO_REPO_3_1_3 = :QBYA-NO-REPO-3-1-3,
              BYAFILE.BYA_AMT_REPO_3_1_3 = :QBYA-AMT-REPO-3-1-3,
              BYAFILE.BYA_O2_REPO_3_1_3 = :QBYA-O2-REPO-3-1-3,
              BYAFILE.BYA_INT_REPO_3_1_3 = :QBYA-INT-REPO-3-1-3,
              BYAFILE.BYA_NO_REPO_3_1_4 = :QBYA-NO-REPO-3-1-4,
              BYAFILE.BYA_AMT_REPO_3_1_4 = :QBYA-AMT-REPO-3-1-4,
              BYAFILE.BYA_O2_REPO_3_1_4 = :QBYA-O2-REPO-3-1-4,
              BYAFILE.BYA_INT_REPO_3_1_4 = :QBYA-INT-REPO-3-1-4,
              BYAFILE.BYA_NO_REPO_3_1_5 = :QBYA-NO-REPO-3-1-5,
              BYAFILE.BYA_AMT_REPO_3_1_5 = :QBYA-AMT-REPO-3-1-5,
              BYAFILE.BYA_O2_REPO_3_1_5 = :QBYA-O2-REPO-3-1-5,
              BYAFILE.BYA_INT_REPO_3_1_5 = :QBYA-INT-REPO-3-1-5,
              BYAFILE.BYA_NO_REPO_3_2_1 = :QBYA-NO-REPO-3-2-1,
              BYAFILE.BYA_AMT_REPO_3_2_1 = :QBYA-AMT-REPO-3-2-1,
              BYAFILE.BYA_O2_REPO_3_2_1 = :QBYA-O2-REPO-3-2-1,
              BYAFILE.BYA_INT_REPO_3_2_1 = :QBYA-INT-REPO-3-2-1,
              BYAFILE.BYA_NO_REPO_3_2_2 = :QBYA-NO-REPO-3-2-2,
              BYAFILE.BYA_AMT_REPO_3_2_2 = :QBYA-AMT-REPO-3-2-2,
              BYAFILE.BYA_O2_REPO_3_2_2 = :QBYA-O2-REPO-3-2-2,
              BYAFILE.BYA_INT_REPO_3_2_2 = :QBYA-INT-REPO-3-2-2,
              BYAFILE.BYA_NO_REPO_3_2_3 = :QBYA-NO-REPO-3-2-3,
              BYAFILE.BYA_AMT_REPO_3_2_3 = :QBYA-AMT-REPO-3-2-3,
              BYAFILE.BYA_O2_REPO_3_2_3 = :QBYA-O2-REPO-3-2-3,
              BYAFILE.BYA_INT_REPO_3_2_3 = :QBYA-INT-REPO-3-2-3,
              BYAFILE.BYA_NO_REPO_3_2_4 = :QBYA-NO-REPO-3-2-4,
              BYAFILE.BYA_AMT_REPO_3_2_4 = :QBYA-AMT-REPO-3-2-4,
              BYAFILE.BYA_O2_REPO_3_2_4 = :QBYA-O2-REPO-3-2-4,
              BYAFILE.BYA_INT_REPO_3_2_4 = :QBYA-INT-REPO-3-2-4,
              BYAFILE.BYA_NO_REPO_3_2_5 = :QBYA-NO-REPO-3-2-5,
              BYAFILE.BYA_AMT_REPO_3_2_5 = :QBYA-AMT-REPO-3-2-5,
              BYAFILE.BYA_O2_REPO_3_2_5 = :QBYA-O2-REPO-3-2-5,
              BYAFILE.BYA_INT_REPO_3_2_5 = :QBYA-INT-REPO-3-2-5,
              BYAFILE.BYA_NO_PL_1_1_1 = :QBYA-NO-PL-1-1-1,
              BYAFILE.BYA_AMT_PL_1_1_1 = :QBYA-AMT-PL-1-1-1,
              BYAFILE.BYA_O2_PL_1_1_1 = :QBYA-O2-PL-1-1-1,
              BYAFILE.BYA_INT_PL_1_1_1 = :QBYA-INT-PL-1-1-1,
              BYAFILE.BYA_NO_PL_1_1_2 = :QBYA-NO-PL-1-1-2,
              BYAFILE.BYA_AMT_PL_1_1_2 = :QBYA-AMT-PL-1-1-2,
              BYAFILE.BYA_O2_PL_1_1_2 = :QBYA-O2-PL-1-1-2,
              BYAFILE.BYA_INT_PL_1_1_2 = :QBYA-INT-PL-1-1-2,
              BYAFILE.BYA_NO_PL_1_1_3 = :QBYA-NO-PL-1-1-3,
              BYAFILE.BYA_AMT_PL_1_1_3 = :QBYA-AMT-PL-1-1-3,
              BYAFILE.BYA_O2_PL_1_1_3 = :QBYA-O2-PL-1-1-3,
              BYAFILE.BYA_INT_PL_1_1_3 = :QBYA-INT-PL-1-1-3,
              BYAFILE.BYA_NO_PL_1_1_4 = :QBYA-NO-PL-1-1-4,
              BYAFILE.BYA_AMT_PL_1_1_4 = :QBYA-AMT-PL-1-1-4,
              BYAFILE.BYA_O2_PL_1_1_4 = :QBYA-O2-PL-1-1-4,
              BYAFILE.BYA_INT_PL_1_1_4 = :QBYA-INT-PL-1-1-4,
              BYAFILE.BYA_NO_PL_1_1_5 = :QBYA-NO-PL-1-1-5,
              BYAFILE.BYA_AMT_PL_1_1_5 = :QBYA-AMT-PL-1-1-5,
              BYAFILE.BYA_O2_PL_1_1_5 = :QBYA-O2-PL-1-1-5,
              BYAFILE.BYA_INT_PL_1_1_5 = :QBYA-INT-PL-1-1-5,
              BYAFILE.BYA_NO_PL_1_1_6 = :QBYA-NO-PL-1-1-6,
              BYAFILE.BYA_AMT_PL_1_1_6 = :QBYA-AMT-PL-1-1-6,
              BYAFILE.BYA_O2_PL_1_1_6 = :QBYA-O2-PL-1-1-6,
              BYAFILE.BYA_INT_PL_1_1_6 = :QBYA-INT-PL-1-1-6,
              BYAFILE.BYA_NO_PL_1_1_7 = :QBYA-NO-PL-1-1-7,
              BYAFILE.BYA_AMT_PL_1_1_7 = :QBYA-AMT-PL-1-1-7,
              BYAFILE.BYA_O2_PL_1_1_7 = :QBYA-O2-PL-1-1-7,
              BYAFILE.BYA_INT_PL_1_1_7 = :QBYA-INT-PL-1-1-7,
              BYAFILE.BYA_NO_PL_1_1_8 = :QBYA-NO-PL-1-1-8,
              BYAFILE.BYA_AMT_PL_1_1_8 = :QBYA-AMT-PL-1-1-8,
              BYAFILE.BYA_O2_PL_1_1_8 = :QBYA-O2-PL-1-1-8,
              BYAFILE.BYA_INT_PL_1_1_8 = :QBYA-INT-PL-1-1-8,
              BYAFILE.BYA_NO_PL_1_1_9 = :QBYA-NO-PL-1-1-9,
              BYAFILE.BYA_AMT_PL_1_1_9 = :QBYA-AMT-PL-1-1-9,
              BYAFILE.BYA_O2_PL_1_1_9 = :QBYA-O2-PL-1-1-9,
              BYAFILE.BYA_INT_PL_1_1_9 = :QBYA-INT-PL-1-1-9,
              BYAFILE.BYA_NO_PL_1_1_10 = :QBYA-NO-PL-1-1-10,
              BYAFILE.BYA_AMT_PL_1_1_10 = :QBYA-AMT-PL-1-1-10,
              BYAFILE.BYA_O2_PL_1_1_10 = :QBYA-O2-PL-1-1-10,
              BYAFILE.BYA_INT_PL_1_1_10 = :QBYA-INT-PL-1-1-10,
              BYAFILE.BYA_NO_PL_1_2_1 = :QBYA-NO-PL-1-2-1,
              BYAFILE.BYA_AMT_PL_1_2_1 = :QBYA-AMT-PL-1-2-1,
              BYAFILE.BYA_O2_PL_1_2_1 = :QBYA-O2-PL-1-2-1,
              BYAFILE.BYA_INT_PL_1_2_1 = :QBYA-INT-PL-1-2-1,
              BYAFILE.BYA_NO_PL_1_2_2 = :QBYA-NO-PL-1-2-2,
              BYAFILE.BYA_AMT_PL_1_2_2 = :QBYA-AMT-PL-1-2-2,
              BYAFILE.BYA_O2_PL_1_2_2 = :QBYA-O2-PL-1-2-2,
              BYAFILE.BYA_INT_PL_1_2_2 = :QBYA-INT-PL-1-2-2,
              BYAFILE.BYA_NO_PL_1_2_3 = :QBYA-NO-PL-1-2-3,
              BYAFILE.BYA_AMT_PL_1_2_3 = :QBYA-AMT-PL-1-2-3,
              BYAFILE.BYA_O2_PL_1_2_3 = :QBYA-O2-PL-1-2-3,
              BYAFILE.BYA_INT_PL_1_2_3 = :QBYA-INT-PL-1-2-3,
              BYAFILE.BYA_NO_PL_1_2_4 = :QBYA-NO-PL-1-2-4,
              BYAFILE.BYA_AMT_PL_1_2_4 = :QBYA-AMT-PL-1-2-4,
              BYAFILE.BYA_O2_PL_1_2_4 = :QBYA-O2-PL-1-2-4,
              BYAFILE.BYA_INT_PL_1_2_4 = :QBYA-INT-PL-1-2-4,
              BYAFILE.BYA_NO_PL_1_2_5 = :QBYA-NO-PL-1-2-5,
              BYAFILE.BYA_AMT_PL_1_2_5 = :QBYA-AMT-PL-1-2-5,
              BYAFILE.BYA_O2_PL_1_2_5 = :QBYA-O2-PL-1-2-5,
              BYAFILE.BYA_INT_PL_1_2_5 = :QBYA-INT-PL-1-2-5,
              BYAFILE.BYA_NO_PL_1_2_6 = :QBYA-NO-PL-1-2-6,
              BYAFILE.BYA_AMT_PL_1_2_6 = :QBYA-AMT-PL-1-2-6,
              BYAFILE.BYA_O2_PL_1_2_6 = :QBYA-O2-PL-1-2-6,
              BYAFILE.BYA_INT_PL_1_2_6 = :QBYA-INT-PL-1-2-6,
              BYAFILE.BYA_NO_PL_1_2_7 = :QBYA-NO-PL-1-2-7,
              BYAFILE.BYA_AMT_PL_1_2_7 = :QBYA-AMT-PL-1-2-7,
              BYAFILE.BYA_O2_PL_1_2_7 = :QBYA-O2-PL-1-2-7,
              BYAFILE.BYA_INT_PL_1_2_7 = :QBYA-INT-PL-1-2-7,
              BYAFILE.BYA_NO_PL_1_2_8 = :QBYA-NO-PL-1-2-8,
              BYAFILE.BYA_AMT_PL_1_2_8 = :QBYA-AMT-PL-1-2-8,
              BYAFILE.BYA_O2_PL_1_2_8 = :QBYA-O2-PL-1-2-8,
              BYAFILE.BYA_INT_PL_1_2_8 = :QBYA-INT-PL-1-2-8,
              BYAFILE.BYA_NO_PL_1_2_9 = :QBYA-NO-PL-1-2-9,
              BYAFILE.BYA_AMT_PL_1_2_9 = :QBYA-AMT-PL-1-2-9,
              BYAFILE.BYA_O2_PL_1_2_9 = :QBYA-O2-PL-1-2-9,
              BYAFILE.BYA_INT_PL_1_2_9 = :QBYA-INT-PL-1-2-9,
              BYAFILE.BYA_NO_PL_1_2_10 = :QBYA-NO-PL-1-2-10,
              BYAFILE.BYA_AMT_PL_1_2_10 = :QBYA-AMT-PL-1-2-10,
              BYAFILE.BYA_O2_PL_1_2_10 = :QBYA-O2-PL-1-2-10,
              BYAFILE.BYA_INT_PL_1_2_10 = :QBYA-INT-PL-1-2-10,
              BYAFILE.BYA_NO_PL_2_1_1 = :QBYA-NO-PL-2-1-1,
              BYAFILE.BYA_AMT_PL_2_1_1 = :QBYA-AMT-PL-2-1-1,
              BYAFILE.BYA_O2_PL_2_1_1 = :QBYA-O2-PL-2-1-1,
              BYAFILE.BYA_INT_PL_2_1_1 = :QBYA-INT-PL-2-1-1,
              BYAFILE.BYA_NO_PL_2_1_2 = :QBYA-NO-PL-2-1-2,
              BYAFILE.BYA_AMT_PL_2_1_2 = :QBYA-AMT-PL-2-1-2,
              BYAFILE.BYA_O2_PL_2_1_2 = :QBYA-O2-PL-2-1-2,
              BYAFILE.BYA_INT_PL_2_1_2 = :QBYA-INT-PL-2-1-2,
              BYAFILE.BYA_NO_PL_2_1_3 = :QBYA-NO-PL-2-1-3,
              BYAFILE.BYA_AMT_PL_2_1_3 = :QBYA-AMT-PL-2-1-3,
              BYAFILE.BYA_O2_PL_2_1_3 = :QBYA-O2-PL-2-1-3,
              BYAFILE.BYA_INT_PL_2_1_3 = :QBYA-INT-PL-2-1-3,
              BYAFILE.BYA_NO_PL_2_1_4 = :QBYA-NO-PL-2-1-4,
              BYAFILE.BYA_AMT_PL_2_1_4 = :QBYA-AMT-PL-2-1-4,
              BYAFILE.BYA_O2_PL_2_1_4 = :QBYA-O2-PL-2-1-4,
              BYAFILE.BYA_INT_PL_2_1_4 = :QBYA-INT-PL-2-1-4,
              BYAFILE.BYA_NO_PL_2_1_5 = :QBYA-NO-PL-2-1-5,
              BYAFILE.BYA_AMT_PL_2_1_5 = :QBYA-AMT-PL-2-1-5,
              BYAFILE.BYA_O2_PL_2_1_5 = :QBYA-O2-PL-2-1-5,
              BYAFILE.BYA_INT_PL_2_1_5 = :QBYA-INT-PL-2-1-5,
              BYAFILE.BYA_NO_PL_2_1_6 = :QBYA-NO-PL-2-1-6,
              BYAFILE.BYA_AMT_PL_2_1_6 = :QBYA-AMT-PL-2-1-6,
              BYAFILE.BYA_O2_PL_2_1_6 = :QBYA-O2-PL-2-1-6,
              BYAFILE.BYA_INT_PL_2_1_6 = :QBYA-INT-PL-2-1-6,
              BYAFILE.BYA_NO_PL_2_1_7 = :QBYA-NO-PL-2-1-7,
              BYAFILE.BYA_AMT_PL_2_1_7 = :QBYA-AMT-PL-2-1-7,
              BYAFILE.BYA_O2_PL_2_1_7 = :QBYA-O2-PL-2-1-7,
              BYAFILE.BYA_INT_PL_2_1_7 = :QBYA-INT-PL-2-1-7,
              BYAFILE.BYA_NO_PL_2_1_8 = :QBYA-NO-PL-2-1-8,
              BYAFILE.BYA_AMT_PL_2_1_8 = :QBYA-AMT-PL-2-1-8,
              BYAFILE.BYA_O2_PL_2_1_8 = :QBYA-O2-PL-2-1-8,
              BYAFILE.BYA_INT_PL_2_1_8 = :QBYA-INT-PL-2-1-8,
              BYAFILE.BYA_NO_PL_2_1_9 = :QBYA-NO-PL-2-1-9,
              BYAFILE.BYA_AMT_PL_2_1_9 = :QBYA-AMT-PL-2-1-9,
              BYAFILE.BYA_O2_PL_2_1_9 = :QBYA-O2-PL-2-1-9,
              BYAFILE.BYA_INT_PL_2_1_9 = :QBYA-INT-PL-2-1-9,
              BYAFILE.BYA_NO_PL_2_1_10 = :QBYA-NO-PL-2-1-10,
              BYAFILE.BYA_AMT_PL_2_1_10 = :QBYA-AMT-PL-2-1-10,
              BYAFILE.BYA_O2_PL_2_1_10 = :QBYA-O2-PL-2-1-10,
              BYAFILE.BYA_INT_PL_2_1_10 = :QBYA-INT-PL-2-1-10,
              BYAFILE.BYA_NO_PL_2_2_1 = :QBYA-NO-PL-2-2-1,
              BYAFILE.BYA_AMT_PL_2_2_1 = :QBYA-AMT-PL-2-2-1,
              BYAFILE.BYA_O2_PL_2_2_1 = :QBYA-O2-PL-2-2-1,
              BYAFILE.BYA_INT_PL_2_2_1 = :QBYA-INT-PL-2-2-1,
              BYAFILE.BYA_NO_PL_2_2_2 = :QBYA-NO-PL-2-2-2,
              BYAFILE.BYA_AMT_PL_2_2_2 = :QBYA-AMT-PL-2-2-2,
              BYAFILE.BYA_O2_PL_2_2_2 = :QBYA-O2-PL-2-2-2,
              BYAFILE.BYA_INT_PL_2_2_2 = :QBYA-INT-PL-2-2-2,
              BYAFILE.BYA_NO_PL_2_2_3 = :QBYA-NO-PL-2-2-3,
              BYAFILE.BYA_AMT_PL_2_2_3 = :QBYA-AMT-PL-2-2-3,
              BYAFILE.BYA_O2_PL_2_2_3 = :QBYA-O2-PL-2-2-3,
              BYAFILE.BYA_INT_PL_2_2_3 = :QBYA-INT-PL-2-2-3,
              BYAFILE.BYA_NO_PL_2_2_4 = :QBYA-NO-PL-2-2-4,
              BYAFILE.BYA_AMT_PL_2_2_4 = :QBYA-AMT-PL-2-2-4,
              BYAFILE.BYA_O2_PL_2_2_4 = :QBYA-O2-PL-2-2-4,
              BYAFILE.BYA_INT_PL_2_2_4 = :QBYA-INT-PL-2-2-4,
              BYAFILE.BYA_NO_PL_2_2_5 = :QBYA-NO-PL-2-2-5,
              BYAFILE.BYA_AMT_PL_2_2_5 = :QBYA-AMT-PL-2-2-5,
              BYAFILE.BYA_O2_PL_2_2_5 = :QBYA-O2-PL-2-2-5,
              BYAFILE.BYA_INT_PL_2_2_5 = :QBYA-INT-PL-2-2-5,
              BYAFILE.BYA_NO_PL_2_2_6 = :QBYA-NO-PL-2-2-6,
              BYAFILE.BYA_AMT_PL_2_2_6 = :QBYA-AMT-PL-2-2-6,
              BYAFILE.BYA_O2_PL_2_2_6 = :QBYA-O2-PL-2-2-6,
              BYAFILE.BYA_INT_PL_2_2_6 = :QBYA-INT-PL-2-2-6,
              BYAFILE.BYA_NO_PL_2_2_7 = :QBYA-NO-PL-2-2-7,
              BYAFILE.BYA_AMT_PL_2_2_7 = :QBYA-AMT-PL-2-2-7,
              BYAFILE.BYA_O2_PL_2_2_7 = :QBYA-O2-PL-2-2-7,
              BYAFILE.BYA_INT_PL_2_2_7 = :QBYA-INT-PL-2-2-7,
              BYAFILE.BYA_NO_PL_2_2_8 = :QBYA-NO-PL-2-2-8,
              BYAFILE.BYA_AMT_PL_2_2_8 = :QBYA-AMT-PL-2-2-8,
              BYAFILE.BYA_O2_PL_2_2_8 = :QBYA-O2-PL-2-2-8,
              BYAFILE.BYA_INT_PL_2_2_8 = :QBYA-INT-PL-2-2-8,
              BYAFILE.BYA_NO_PL_2_2_9 = :QBYA-NO-PL-2-2-9,
              BYAFILE.BYA_AMT_PL_2_2_9 = :QBYA-AMT-PL-2-2-9,
              BYAFILE.BYA_O2_PL_2_2_9 = :QBYA-O2-PL-2-2-9,
              BYAFILE.BYA_INT_PL_2_2_9 = :QBYA-INT-PL-2-2-9,
              BYAFILE.BYA_NO_PL_2_2_10 = :QBYA-NO-PL-2-2-10,
              BYAFILE.BYA_AMT_PL_2_2_10 = :QBYA-AMT-PL-2-2-10,
              BYAFILE.BYA_O2_PL_2_2_10 = :QBYA-O2-PL-2-2-10,
              BYAFILE.BYA_INT_PL_2_2_10 = :QBYA-INT-PL-2-2-10,
              BYAFILE.BYA_NO_PL_3_1_1 = :QBYA-NO-PL-3-1-1,
              BYAFILE.BYA_AMT_PL_3_1_1 = :QBYA-AMT-PL-3-1-1,
              BYAFILE.BYA_O2_PL_3_1_1 = :QBYA-O2-PL-3-1-1,
              BYAFILE.BYA_INT_PL_3_1_1 = :QBYA-INT-PL-3-1-1,
              BYAFILE.BYA_NO_PL_3_1_2 = :QBYA-NO-PL-3-1-2,
              BYAFILE.BYA_AMT_PL_3_1_2 = :QBYA-AMT-PL-3-1-2,
              BYAFILE.BYA_O2_PL_3_1_2 = :QBYA-O2-PL-3-1-2,
              BYAFILE.BYA_INT_PL_3_1_2 = :QBYA-INT-PL-3-1-2,
              BYAFILE.BYA_NO_PL_3_1_3 = :QBYA-NO-PL-3-1-3,
              BYAFILE.BYA_AMT_PL_3_1_3 = :QBYA-AMT-PL-3-1-3,
              BYAFILE.BYA_O2_PL_3_1_3 = :QBYA-O2-PL-3-1-3,
              BYAFILE.BYA_INT_PL_3_1_3 = :QBYA-INT-PL-3-1-3,
              BYAFILE.BYA_NO_PL_3_1_4 = :QBYA-NO-PL-3-1-4,
              BYAFILE.BYA_AMT_PL_3_1_4 = :QBYA-AMT-PL-3-1-4,
              BYAFILE.BYA_O2_PL_3_1_4 = :QBYA-O2-PL-3-1-4,
              BYAFILE.BYA_INT_PL_3_1_4 = :QBYA-INT-PL-3-1-4,
              BYAFILE.BYA_NO_PL_3_1_5 = :QBYA-NO-PL-3-1-5,
              BYAFILE.BYA_AMT_PL_3_1_5 = :QBYA-AMT-PL-3-1-5,
              BYAFILE.BYA_O2_PL_3_1_5 = :QBYA-O2-PL-3-1-5,
              BYAFILE.BYA_INT_PL_3_1_5 = :QBYA-INT-PL-3-1-5,
              BYAFILE.BYA_NO_PL_3_1_6 = :QBYA-NO-PL-3-1-6,
              BYAFILE.BYA_AMT_PL_3_1_6 = :QBYA-AMT-PL-3-1-6,
              BYAFILE.BYA_O2_PL_3_1_6 = :QBYA-O2-PL-3-1-6,
              BYAFILE.BYA_INT_PL_3_1_6 = :QBYA-INT-PL-3-1-6,
              BYAFILE.BYA_NO_PL_3_1_7 = :QBYA-NO-PL-3-1-7,
              BYAFILE.BYA_AMT_PL_3_1_7 = :QBYA-AMT-PL-3-1-7,
              BYAFILE.BYA_O2_PL_3_1_7 = :QBYA-O2-PL-3-1-7,
              BYAFILE.BYA_INT_PL_3_1_7 = :QBYA-INT-PL-3-1-7,
              BYAFILE.BYA_NO_PL_3_1_8 = :QBYA-NO-PL-3-1-8,
              BYAFILE.BYA_AMT_PL_3_1_8 = :QBYA-AMT-PL-3-1-8,
              BYAFILE.BYA_O2_PL_3_1_8 = :QBYA-O2-PL-3-1-8,
              BYAFILE.BYA_INT_PL_3_1_8 = :QBYA-INT-PL-3-1-8,
              BYAFILE.BYA_NO_PL_3_1_9 = :QBYA-NO-PL-3-1-9,
              BYAFILE.BYA_AMT_PL_3_1_9 = :QBYA-AMT-PL-3-1-9,
              BYAFILE.BYA_O2_PL_3_1_9 = :QBYA-O2-PL-3-1-9,
              BYAFILE.BYA_INT_PL_3_1_9 = :QBYA-INT-PL-3-1-9,
              BYAFILE.BYA_NO_PL_3_1_10 = :QBYA-NO-PL-3-1-10,
              BYAFILE.BYA_AMT_PL_3_1_10 = :QBYA-AMT-PL-3-1-10,
              BYAFILE.BYA_O2_PL_3_1_10 = :QBYA-O2-PL-3-1-10,
              BYAFILE.BYA_INT_PL_3_1_10 = :QBYA-INT-PL-3-1-10,
              BYAFILE.BYA_NO_PL_3_2_1 = :QBYA-NO-PL-3-2-1,
              BYAFILE.BYA_AMT_PL_3_2_1 = :QBYA-AMT-PL-3-2-1,
              BYAFILE.BYA_O2_PL_3_2_1 = :QBYA-O2-PL-3-2-1,
              BYAFILE.BYA_INT_PL_3_2_1 = :QBYA-INT-PL-3-2-1,
              BYAFILE.BYA_NO_PL_3_2_2 = :QBYA-NO-PL-3-2-2,
              BYAFILE.BYA_AMT_PL_3_2_2 = :QBYA-AMT-PL-3-2-2,
              BYAFILE.BYA_O2_PL_3_2_2 = :QBYA-O2-PL-3-2-2,
              BYAFILE.BYA_INT_PL_3_2_2 = :QBYA-INT-PL-3-2-2,
              BYAFILE.BYA_NO_PL_3_2_3 = :QBYA-NO-PL-3-2-3,
              BYAFILE.BYA_AMT_PL_3_2_3 = :QBYA-AMT-PL-3-2-3,
              BYAFILE.BYA_O2_PL_3_2_3 = :QBYA-O2-PL-3-2-3,
              BYAFILE.BYA_INT_PL_3_2_3 = :QBYA-INT-PL-3-2-3,
              BYAFILE.BYA_NO_PL_3_2_4 = :QBYA-NO-PL-3-2-4,
              BYAFILE.BYA_AMT_PL_3_2_4 = :QBYA-AMT-PL-3-2-4,
              BYAFILE.BYA_O2_PL_3_2_4 = :QBYA-O2-PL-3-2-4,
              BYAFILE.BYA_INT_PL_3_2_4 = :QBYA-INT-PL-3-2-4,
              BYAFILE.BYA_NO_PL_3_2_5 = :QBYA-NO-PL-3-2-5,
              BYAFILE.BYA_AMT_PL_3_2_5 = :QBYA-AMT-PL-3-2-5,
              BYAFILE.BYA_O2_PL_3_2_5 = :QBYA-O2-PL-3-2-5,
              BYAFILE.BYA_INT_PL_3_2_5 = :QBYA-INT-PL-3-2-5,
              BYAFILE.BYA_NO_PL_3_2_6 = :QBYA-NO-PL-3-2-6,
              BYAFILE.BYA_AMT_PL_3_2_6 = :QBYA-AMT-PL-3-2-6,
              BYAFILE.BYA_O2_PL_3_2_6 = :QBYA-O2-PL-3-2-6,
              BYAFILE.BYA_INT_PL_3_2_6 = :QBYA-INT-PL-3-2-6,
              BYAFILE.BYA_NO_PL_3_2_7 = :QBYA-NO-PL-3-2-7,
              BYAFILE.BYA_AMT_PL_3_2_7 = :QBYA-AMT-PL-3-2-7,
              BYAFILE.BYA_O2_PL_3_2_7 = :QBYA-O2-PL-3-2-7,
              BYAFILE.BYA_INT_PL_3_2_7 = :QBYA-INT-PL-3-2-7,
              BYAFILE.BYA_NO_PL_3_2_8 = :QBYA-NO-PL-3-2-8,
              BYAFILE.BYA_AMT_PL_3_2_8 = :QBYA-AMT-PL-3-2-8,
              BYAFILE.BYA_O2_PL_3_2_8 = :QBYA-O2-PL-3-2-8,
              BYAFILE.BYA_INT_PL_3_2_8 = :QBYA-INT-PL-3-2-8,
              BYAFILE.BYA_NO_PL_3_2_9 = :QBYA-NO-PL-3-2-9,
              BYAFILE.BYA_AMT_PL_3_2_9 = :QBYA-AMT-PL-3-2-9,
              BYAFILE.BYA_O2_PL_3_2_9 = :QBYA-O2-PL-3-2-9,
              BYAFILE.BYA_INT_PL_3_2_9 = :QBYA-INT-PL-3-2-9,
              BYAFILE.BYA_NO_PL_3_2_10 = :QBYA-NO-PL-3-2-10,
              BYAFILE.BYA_AMT_PL_3_2_10 = :QBYA-AMT-PL-3-2-10,
              BYAFILE.BYA_O2_PL_3_2_10 = :QBYA-O2-PL-3-2-10,
              BYAFILE.BYA_INT_PL_3_2_10 = :QBYA-INT-PL-3-2-10,
              BYAFILE.BYA_DLRVPRIN_1_1_1 = :QBYA-DLRVPRIN-1-1-1,
              BYAFILE.BYA_DLRVPRIN_1_1_2 = :QBYA-DLRVPRIN-1-1-2,
              BYAFILE.BYA_DLRVPRIN_1_1_3 = :QBYA-DLRVPRIN-1-1-3,
              BYAFILE.BYA_DLRVCHGS_1_1_1 = :QBYA-DLRVCHGS-1-1-1,
              BYAFILE.BYA_DLRVCHGS_1_1_2 = :QBYA-DLRVCHGS-1-1-2,
              BYAFILE.BYA_DLRVCHGS_1_1_3 = :QBYA-DLRVCHGS-1-1-3,
              BYAFILE.BYA_DLPARVHELD_1_1 = :QBYA-DLPARVHELD-1-1,
              BYAFILE.BYA_DLRVPRIN_1_2_1 = :QBYA-DLRVPRIN-1-2-1,
              BYAFILE.BYA_DLRVPRIN_1_2_2 = :QBYA-DLRVPRIN-1-2-2,
              BYAFILE.BYA_DLRVPRIN_1_2_3 = :QBYA-DLRVPRIN-1-2-3,
              BYAFILE.BYA_DLRVCHGS_1_2_1 = :QBYA-DLRVCHGS-1-2-1,
              BYAFILE.BYA_DLRVCHGS_1_2_2 = :QBYA-DLRVCHGS-1-2-2,
              BYAFILE.BYA_DLRVCHGS_1_2_3 = :QBYA-DLRVCHGS-1-2-3,
              BYAFILE.BYA_DLPARVHELD_1_2 = :QBYA-DLPARVHELD-1-2,
              BYAFILE.BYA_DLRVPRIN_2_1_1 = :QBYA-DLRVPRIN-2-1-1,
              BYAFILE.BYA_DLRVPRIN_2_1_2 = :QBYA-DLRVPRIN-2-1-2,
              BYAFILE.BYA_DLRVPRIN_2_1_3 = :QBYA-DLRVPRIN-2-1-3,
              BYAFILE.BYA_DLRVCHGS_2_1_1 = :QBYA-DLRVCHGS-2-1-1,
              BYAFILE.BYA_DLRVCHGS_2_1_2 = :QBYA-DLRVCHGS-2-1-2,
              BYAFILE.BYA_DLRVCHGS_2_1_3 = :QBYA-DLRVCHGS-2-1-3,
              BYAFILE.BYA_DLPARVHELD_2_1 = :QBYA-DLPARVHELD-2-1,
              BYAFILE.BYA_DLRVPRIN_2_2_1 = :QBYA-DLRVPRIN-2-2-1,
              BYAFILE.BYA_DLRVPRIN_2_2_2 = :QBYA-DLRVPRIN-2-2-2,
              BYAFILE.BYA_DLRVPRIN_2_2_3 = :QBYA-DLRVPRIN-2-2-3,
              BYAFILE.BYA_DLRVCHGS_2_2_1 = :QBYA-DLRVCHGS-2-2-1,
              BYAFILE.BYA_DLRVCHGS_2_2_2 = :QBYA-DLRVCHGS-2-2-2,
              BYAFILE.BYA_DLRVCHGS_2_2_3 = :QBYA-DLRVCHGS-2-2-3,
              BYAFILE.BYA_DLPARVHELD_2_2 = :QBYA-DLPARVHELD-2-2,
              BYAFILE.BYA_DLRVPRIN_3_1_1 = :QBYA-DLRVPRIN-3-1-1,
              BYAFILE.BYA_DLRVPRIN_3_1_2 = :QBYA-DLRVPRIN-3-1-2,
              BYAFILE.BYA_DLRVPRIN_3_1_3 = :QBYA-DLRVPRIN-3-1-3,
              BYAFILE.BYA_DLRVCHGS_3_1_1 = :QBYA-DLRVCHGS-3-1-1,
              BYAFILE.BYA_DLRVCHGS_3_1_2 = :QBYA-DLRVCHGS-3-1-2,
              BYAFILE.BYA_DLRVCHGS_3_1_3 = :QBYA-DLRVCHGS-3-1-3,
              BYAFILE.BYA_DLPARVHELD_3_1 = :QBYA-DLPARVHELD-3-1,
              BYAFILE.BYA_DLRVPRIN_3_2_1 = :QBYA-DLRVPRIN-3-2-1,
              BYAFILE.BYA_DLRVPRIN_3_2_2 = :QBYA-DLRVPRIN-3-2-2,
              BYAFILE.BYA_DLRVPRIN_3_2_3 = :QBYA-DLRVPRIN-3-2-3,
              BYAFILE.BYA_DLRVCHGS_3_2_1 = :QBYA-DLRVCHGS-3-2-1,
              BYAFILE.BYA_DLRVCHGS_3_2_2 = :QBYA-DLRVCHGS-3-2-2,
              BYAFILE.BYA_DLRVCHGS_3_2_3 = :QBYA-DLRVCHGS-3-2-3,
              BYAFILE.BYA_DLPARVHELD_3_2 = :QBYA-DLPARVHELD-3-2
             WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
               AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
               AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BYA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      * USE C1 CONNECT
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.
           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BYA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *=================================================================
       LOAD-BYB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYB-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH.

      *-----------------------------------------------------------------
       OPEN-BYB1-FILE.
           PERFORM LOAD-BYB1-FILE.
           PERFORM OPEN-IT.
           MOVE BYB-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
       READ-BYB1-FILE. *> EMBEDDED - BYBFILE
           PERFORM READ-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           INITIALIZE QBYB-REC.

           MOVE BYB-DATE-CCYYMM      TO QBYB-DATE-CCYYMM.
           MOVE BYB-BRNO             TO QBYB-BRNO.
           MOVE BYB-CLASS            TO QBYB-CLASS.

           EXEC SQL
            SELECT BYBFILE.BYB_DATE_CCYYMM,
                   BYBFILE.BYB_BRNO,
                   BYBFILE.BYB_CLASS,
                   BYBFILE.BYB_PL_PRINCOL_1_1,
                   BYBFILE.BYB_PL_INTCOL_1_1,
                   BYBFILE.BYB_PL_LATECOL_1_1,
                   BYBFILE.BYB_PL_O2COL_1_1,
                   BYBFILE.BYB_PL_PRINCOL_1_2,
                   BYBFILE.BYB_PL_INTCOL_1_2,
                   BYBFILE.BYB_PL_LATECOL_1_2,
                   BYBFILE.BYB_PL_O2COL_1_2,
                   BYBFILE.BYB_PL_PRINCOL_1_3,
                   BYBFILE.BYB_PL_INTCOL_1_3,
                   BYBFILE.BYB_PL_LATECOL_1_3,
                   BYBFILE.BYB_PL_O2COL_1_3,
                   BYBFILE.BYB_PL_PRINCOL_1_4,
                   BYBFILE.BYB_PL_INTCOL_1_4,
                   BYBFILE.BYB_PL_LATECOL_1_4,
                   BYBFILE.BYB_PL_O2COL_1_4,
                   BYBFILE.BYB_PL_PRINCOL_1_5,
                   BYBFILE.BYB_PL_INTCOL_1_5,
                   BYBFILE.BYB_PL_LATECOL_1_5,
                   BYBFILE.BYB_PL_O2COL_1_5,
                   BYBFILE.BYB_AMT_SOLD_1,
                   BYBFILE.BYB_AMT_TRANS_1,
                   BYBFILE.BYB_AMT_PURCH_1,
                   BYBFILE.BYB_NO_SOLD_1,
                   BYBFILE.BYB_NO_TRANS_1,
                   BYBFILE.BYB_NO_PURCH_1,
                   BYBFILE.BYB_OTHCOL_1,
                   BYBFILE.BYB_OT2COL_1,
                   BYBFILE.BYB_INTCOL_1,
                   BYBFILE.BYB_LATECOL_1,
                   BYBFILE.BYB_PRINCOL_1,
                   BYBFILE.BYB_DEFCOL_1,
                   BYBFILE.BYB_RNPRIN_1,
                   BYBFILE.BYB_INTREB_1,
                   BYBFILE.BYB_CLREB1_1,
                   BYBFILE.BYB_CLREB2_1,
                   BYBFILE.BYB_CLREB3_1,
                   BYBFILE.BYB_CLREB4_1,
                   BYBFILE.BYB_AHREB_1,
                   BYBFILE.BYB_PPREB_1,
                   BYBFILE.BYB_O1REB_1,
                   BYBFILE.BYB_O2REB_1,
                   BYBFILE.BYB_O3REB_1,
                   BYBFILE.BYB_O4REB_1,
                   BYBFILE.BYB_O5REB_1,
                   BYBFILE.BYB_O6REB_1,
                   BYBFILE.BYB_O7REB_1,
                   BYBFILE.BYB_SCREB_1,
                   BYBFILE.BYB_MFREB_1,
                   BYBFILE.BYB_DFREB_1,
                   BYBFILE.BYB_UN7_REB_1,
                   BYBFILE.BYB_UN8_REB_1,
                   BYBFILE.BYB_UN9_REB_1,
                   BYBFILE.BYB_UN10_REB_1,
                   BYBFILE.BYB_PART_CHGOFF_1,
                   BYBFILE.BYB_ALLOTMENTS_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_1,
                   BYBFILE.BYB_INS_NO_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_INS_AMT_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_2,
                   BYBFILE.BYB_INS_NO_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_INS_AMT_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_ELIG_1_3,
                   BYBFILE.BYB_INS_NO_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_INS_AMT_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_ELIG_1_4,
                   BYBFILE.BYB_INS_NO_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
                   BYBFILE.BYB_INS_AMT_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_ELIG_1_5,
                   BYBFILE.BYB_INS_NO_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
                   BYBFILE.BYB_INS_AMT_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_ELIG_1_6,
                   BYBFILE.BYB_INS_NO_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
                   BYBFILE.BYB_INS_AMT_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_ELIG_1_7,
                   BYBFILE.BYB_INS_NO_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
                   BYBFILE.BYB_INS_AMT_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_ELIG_1_8,
                   BYBFILE.BYB_INS_NO_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
                   BYBFILE.BYB_INS_AMT_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_ELIG_1_9,
                   BYBFILE.BYB_INS_NO_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
                   BYBFILE.BYB_INS_AMT_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_ELIG_1_10,
                   BYBFILE.BYB_INS_NO_WROTE_1_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
                   BYBFILE.BYB_INS_AMT_WROTE_1_10,
                   BYBFILE.BYB_FEE_NO_ELIG_1_1,
                   BYBFILE.BYB_FEE_NO_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_ELIG_1_2,
                   BYBFILE.BYB_FEE_NO_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_ELIG_1_3,
                   BYBFILE.BYB_FEE_NO_WROTE_1_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_3,
                   BYBFILE.BYB_OTH_NO_COL_1,
                   BYBFILE.BYB_OT2_NO_COL_1,
                   BYBFILE.BYB_INT_NO_COL_1,
                   BYBFILE.BYB_LATE_NO_COL_1,
                   BYBFILE.BYB_OTH_NO_WAIVED_1,
                   BYBFILE.BYB_OT2_NO_WAIVED_1,
                   BYBFILE.BYB_INT_NO_WAIVED_1,
                   BYBFILE.BYB_LATE_NO_WAIVED_1,
                   BYBFILE.BYB_OTH_AMT_WAIVED_1,
                   BYBFILE.BYB_OT2_AMT_WAIVED_1,
                   BYBFILE.BYB_INT_AMT_WAIVED_1,
                   BYBFILE.BYB_LATE_AMT_WAIVED_1,
                   BYBFILE.BYB_PL_PRINCOL_2_1,
                   BYBFILE.BYB_PL_INTCOL_2_1,
                   BYBFILE.BYB_PL_LATECOL_2_1,
                   BYBFILE.BYB_PL_O2COL_2_1,
                   BYBFILE.BYB_PL_PRINCOL_2_2,
                   BYBFILE.BYB_PL_INTCOL_2_2,
                   BYBFILE.BYB_PL_LATECOL_2_2,
                   BYBFILE.BYB_PL_O2COL_2_2,
                   BYBFILE.BYB_PL_PRINCOL_2_3,
                   BYBFILE.BYB_PL_INTCOL_2_3,
                   BYBFILE.BYB_PL_LATECOL_2_3,
                   BYBFILE.BYB_PL_O2COL_2_3,
                   BYBFILE.BYB_PL_PRINCOL_2_4,
                   BYBFILE.BYB_PL_INTCOL_2_4,
                   BYBFILE.BYB_PL_LATECOL_2_4,
                   BYBFILE.BYB_PL_O2COL_2_4,
                   BYBFILE.BYB_PL_PRINCOL_2_5,
                   BYBFILE.BYB_PL_INTCOL_2_5,
                   BYBFILE.BYB_PL_LATECOL_2_5,
                   BYBFILE.BYB_PL_O2COL_2_5,
                   BYBFILE.BYB_AMT_SOLD_2,
                   BYBFILE.BYB_AMT_TRANS_2,
                   BYBFILE.BYB_AMT_PURCH_2,
                   BYBFILE.BYB_NO_SOLD_2,
                   BYBFILE.BYB_NO_TRANS_2,
                   BYBFILE.BYB_NO_PURCH_2,
                   BYBFILE.BYB_OTHCOL_2,
                   BYBFILE.BYB_OT2COL_2,
                   BYBFILE.BYB_INTCOL_2,
                   BYBFILE.BYB_LATECOL_2,
                   BYBFILE.BYB_PRINCOL_2,
                   BYBFILE.BYB_DEFCOL_2,
                   BYBFILE.BYB_RNPRIN_2,
                   BYBFILE.BYB_INTREB_2,
                   BYBFILE.BYB_CLREB1_2,
                   BYBFILE.BYB_CLREB2_2,
                   BYBFILE.BYB_CLREB3_2,
                   BYBFILE.BYB_CLREB4_2,
                   BYBFILE.BYB_AHREB_2,
                   BYBFILE.BYB_PPREB_2,
                   BYBFILE.BYB_O1REB_2,
                   BYBFILE.BYB_O2REB_2,
                   BYBFILE.BYB_O3REB_2,
                   BYBFILE.BYB_O4REB_2,
                   BYBFILE.BYB_O5REB_2,
                   BYBFILE.BYB_O6REB_2,
                   BYBFILE.BYB_O7REB_2,
                   BYBFILE.BYB_SCREB_2,
                   BYBFILE.BYB_MFREB_2,
                   BYBFILE.BYB_DFREB_2,
                   BYBFILE.BYB_UN7_REB_2,
                   BYBFILE.BYB_UN8_REB_2,
                   BYBFILE.BYB_UN9_REB_2,
                   BYBFILE.BYB_UN10_REB_2,
                   BYBFILE.BYB_PART_CHGOFF_2,
                   BYBFILE.BYB_ALLOTMENTS_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_1,
                   BYBFILE.BYB_INS_NO_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_INS_AMT_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_ELIG_2_2,
                   BYBFILE.BYB_INS_NO_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_INS_AMT_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_3,
                   BYBFILE.BYB_INS_NO_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_INS_AMT_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_ELIG_2_4,
                   BYBFILE.BYB_INS_NO_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
                   BYBFILE.BYB_INS_AMT_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_ELIG_2_5,
                   BYBFILE.BYB_INS_NO_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
                   BYBFILE.BYB_INS_AMT_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_ELIG_2_6,
                   BYBFILE.BYB_INS_NO_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
                   BYBFILE.BYB_INS_AMT_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_ELIG_2_7,
                   BYBFILE.BYB_INS_NO_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
                   BYBFILE.BYB_INS_AMT_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_ELIG_2_8,
                   BYBFILE.BYB_INS_NO_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
                   BYBFILE.BYB_INS_AMT_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_ELIG_2_9,
                   BYBFILE.BYB_INS_NO_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
                   BYBFILE.BYB_INS_AMT_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_ELIG_2_10,
                   BYBFILE.BYB_INS_NO_WROTE_2_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
                   BYBFILE.BYB_INS_AMT_WROTE_2_10,
                   BYBFILE.BYB_FEE_NO_ELIG_2_1,
                   BYBFILE.BYB_FEE_NO_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_ELIG_2_2,
                   BYBFILE.BYB_FEE_NO_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_ELIG_2_3,
                   BYBFILE.BYB_FEE_NO_WROTE_2_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_3,
                   BYBFILE.BYB_OTH_NO_COL_2,
                   BYBFILE.BYB_OT2_NO_COL_2,
                   BYBFILE.BYB_INT_NO_COL_2,
                   BYBFILE.BYB_LATE_NO_COL_2,
                   BYBFILE.BYB_OTH_NO_WAIVED_2,
                   BYBFILE.BYB_OT2_NO_WAIVED_2,
                   BYBFILE.BYB_INT_NO_WAIVED_2,
                   BYBFILE.BYB_LATE_NO_WAIVED_2,
                   BYBFILE.BYB_OTH_AMT_WAIVED_2,
                   BYBFILE.BYB_OT2_AMT_WAIVED_2,
                   BYBFILE.BYB_INT_AMT_WAIVED_2,
                   BYBFILE.BYB_LATE_AMT_WAIVED_2,
                   BYBFILE.BYB_UNITS_1_1_1,
                   BYBFILE.BYB_OUTBAL_1_1_1,
                   BYBFILE.BYB_UNITS_1_1_2,
                   BYBFILE.BYB_OUTBAL_1_1_2,
                   BYBFILE.BYB_UNITS_1_1_3,
                   BYBFILE.BYB_OUTBAL_1_1_3,
                   BYBFILE.BYB_UNITS_1_1_4,
                   BYBFILE.BYB_OUTBAL_1_1_4,
                   BYBFILE.BYB_UNITS_1_1_5,
                   BYBFILE.BYB_OUTBAL_1_1_5,
                   BYBFILE.BYB_UNITS_1_1_6,
                   BYBFILE.BYB_OUTBAL_1_1_6,
                   BYBFILE.BYB_UNITS_1_1_7,
                   BYBFILE.BYB_OUTBAL_1_1_7,
                   BYBFILE.BYB_UNITS_1_1_8,
                   BYBFILE.BYB_OUTBAL_1_1_8,
                   BYBFILE.BYB_UNITS_1_1_9,
                   BYBFILE.BYB_OUTBAL_1_1_9,
                   BYBFILE.BYB_UNITS_1_1_10,
                   BYBFILE.BYB_OUTBAL_1_1_10,
                   BYBFILE.BYB_UNITS_1_2_1,
                   BYBFILE.BYB_OUTBAL_1_2_1,
                   BYBFILE.BYB_UNITS_1_2_2,
                   BYBFILE.BYB_OUTBAL_1_2_2,
                   BYBFILE.BYB_UNITS_1_2_3,
                   BYBFILE.BYB_OUTBAL_1_2_3,
                   BYBFILE.BYB_UNITS_1_2_4,
                   BYBFILE.BYB_OUTBAL_1_2_4,
                   BYBFILE.BYB_UNITS_1_2_5,
                   BYBFILE.BYB_OUTBAL_1_2_5,
                   BYBFILE.BYB_UNITS_1_2_6,
                   BYBFILE.BYB_OUTBAL_1_2_6,
                   BYBFILE.BYB_UNITS_1_2_7,
                   BYBFILE.BYB_OUTBAL_1_2_7,
                   BYBFILE.BYB_UNITS_1_2_8,
                   BYBFILE.BYB_OUTBAL_1_2_8,
                   BYBFILE.BYB_UNITS_1_2_9,
                   BYBFILE.BYB_OUTBAL_1_2_9,
                   BYBFILE.BYB_UNITS_1_2_10,
                   BYBFILE.BYB_OUTBAL_1_2_10,
                   BYBFILE.BYB_UNITS_2_1_1,
                   BYBFILE.BYB_OUTBAL_2_1_1,
                   BYBFILE.BYB_UNITS_2_1_2,
                   BYBFILE.BYB_OUTBAL_2_1_2,
                   BYBFILE.BYB_UNITS_2_1_3,
                   BYBFILE.BYB_OUTBAL_2_1_3,
                   BYBFILE.BYB_UNITS_2_1_4,
                   BYBFILE.BYB_OUTBAL_2_1_4,
                   BYBFILE.BYB_UNITS_2_1_5,
                   BYBFILE.BYB_OUTBAL_2_1_5,
                   BYBFILE.BYB_UNITS_2_1_6,
                   BYBFILE.BYB_OUTBAL_2_1_6,
                   BYBFILE.BYB_UNITS_2_1_7,
                   BYBFILE.BYB_OUTBAL_2_1_7,
                   BYBFILE.BYB_UNITS_2_1_8,
                   BYBFILE.BYB_OUTBAL_2_1_8,
                   BYBFILE.BYB_UNITS_2_1_9,
                   BYBFILE.BYB_OUTBAL_2_1_9,
                   BYBFILE.BYB_UNITS_2_1_10,
                   BYBFILE.BYB_OUTBAL_2_1_10,
                   BYBFILE.BYB_UNITS_2_2_1,
                   BYBFILE.BYB_OUTBAL_2_2_1,
                   BYBFILE.BYB_UNITS_2_2_2,
                   BYBFILE.BYB_OUTBAL_2_2_2,
                   BYBFILE.BYB_UNITS_2_2_3,
                   BYBFILE.BYB_OUTBAL_2_2_3,
                   BYBFILE.BYB_UNITS_2_2_4,
                   BYBFILE.BYB_OUTBAL_2_2_4,
                   BYBFILE.BYB_UNITS_2_2_5,
                   BYBFILE.BYB_OUTBAL_2_2_5,
                   BYBFILE.BYB_UNITS_2_2_6,
                   BYBFILE.BYB_OUTBAL_2_2_6,
                   BYBFILE.BYB_UNITS_2_2_7,
                   BYBFILE.BYB_OUTBAL_2_2_7,
                   BYBFILE.BYB_UNITS_2_2_8,
                   BYBFILE.BYB_OUTBAL_2_2_8,
                   BYBFILE.BYB_UNITS_2_2_9,
                   BYBFILE.BYB_OUTBAL_2_2_9,
                   BYBFILE.BYB_UNITS_2_2_10,
                   BYBFILE.BYB_OUTBAL_2_2_10,
                   BYBFILE.BYB_UNITS_3_1_1,
                   BYBFILE.BYB_OUTBAL_3_1_1,
                   BYBFILE.BYB_UNITS_3_1_2,
                   BYBFILE.BYB_OUTBAL_3_1_2,
                   BYBFILE.BYB_UNITS_3_1_3,
                   BYBFILE.BYB_OUTBAL_3_1_3,
                   BYBFILE.BYB_UNITS_3_1_4,
                   BYBFILE.BYB_OUTBAL_3_1_4,
                   BYBFILE.BYB_UNITS_3_1_5,
                   BYBFILE.BYB_OUTBAL_3_1_5,
                   BYBFILE.BYB_UNITS_3_1_6,
                   BYBFILE.BYB_OUTBAL_3_1_6,
                   BYBFILE.BYB_UNITS_3_1_7,
                   BYBFILE.BYB_OUTBAL_3_1_7,
                   BYBFILE.BYB_UNITS_3_1_8,
                   BYBFILE.BYB_OUTBAL_3_1_8,
                   BYBFILE.BYB_UNITS_3_1_9,
                   BYBFILE.BYB_OUTBAL_3_1_9,
                   BYBFILE.BYB_UNITS_3_1_10,
                   BYBFILE.BYB_OUTBAL_3_1_10,
                   BYBFILE.BYB_UNITS_3_2_1,
                   BYBFILE.BYB_OUTBAL_3_2_1,
                   BYBFILE.BYB_UNITS_3_2_2,
                   BYBFILE.BYB_OUTBAL_3_2_2,
                   BYBFILE.BYB_UNITS_3_2_3,
                   BYBFILE.BYB_OUTBAL_3_2_3,
                   BYBFILE.BYB_UNITS_3_2_4,
                   BYBFILE.BYB_OUTBAL_3_2_4,
                   BYBFILE.BYB_UNITS_3_2_5,
                   BYBFILE.BYB_OUTBAL_3_2_5,
                   BYBFILE.BYB_UNITS_3_2_6,
                   BYBFILE.BYB_OUTBAL_3_2_6,
                   BYBFILE.BYB_UNITS_3_2_7,
                   BYBFILE.BYB_OUTBAL_3_2_7,
                   BYBFILE.BYB_UNITS_3_2_8,
                   BYBFILE.BYB_OUTBAL_3_2_8,
                   BYBFILE.BYB_UNITS_3_2_9,
                   BYBFILE.BYB_OUTBAL_3_2_9,
                   BYBFILE.BYB_UNITS_3_2_10,
                   BYBFILE.BYB_OUTBAL_3_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
                   BYBFILE.BYB_SUMWGTTM,
                   BYBFILE.BYB_SUMWGTAV,
                   BYBFILE.BYB_SUMWGTTA,
                   BYBFILE.BYB_IB_ACCRUED,
                   BYBFILE.BYB_DLDISC_1,
                   BYBFILE.BYB_DLDISC_2,
                   BYBFILE.BYB_DLDISC_3,
                   BYBFILE.BYB_STD_POS,
                   BYBFILE.BYB_30_DAY_MOVED,
                   BYBFILE.BYB_PROJ_INCREASE,
                   BYBFILE.BYB_PROJ_PROFIT,
                   BYBFILE.BYB_PROCEEDS_1,
                   BYBFILE.BYB_PROCEEDS_2,
                   BYBFILE.BYB_LOCKBOX_1,
                   BYBFILE.BYB_LOCKBOX_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_10
            INTO :QBYB-REC
            FROM DBO.BYBFILE
            WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
              AND BYBFILE.BYB_BRNO = :QBYB-BRNO
              AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BYB1-FILE. *> EMBEDDED - BYBFILE

           PERFORM WRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.BYBFILE
             (BYBFILE.BYB_DATE_CCYYMM,
              BYBFILE.BYB_BRNO,
              BYBFILE.BYB_CLASS,
              BYBFILE.BYB_PL_PRINCOL_1_1,
              BYBFILE.BYB_PL_INTCOL_1_1,
              BYBFILE.BYB_PL_LATECOL_1_1,
              BYBFILE.BYB_PL_O2COL_1_1,
              BYBFILE.BYB_PL_PRINCOL_1_2,
              BYBFILE.BYB_PL_INTCOL_1_2,
              BYBFILE.BYB_PL_LATECOL_1_2,
              BYBFILE.BYB_PL_O2COL_1_2,
              BYBFILE.BYB_PL_PRINCOL_1_3,
              BYBFILE.BYB_PL_INTCOL_1_3,
              BYBFILE.BYB_PL_LATECOL_1_3,
              BYBFILE.BYB_PL_O2COL_1_3,
              BYBFILE.BYB_PL_PRINCOL_1_4,
              BYBFILE.BYB_PL_INTCOL_1_4,
              BYBFILE.BYB_PL_LATECOL_1_4,
              BYBFILE.BYB_PL_O2COL_1_4,
              BYBFILE.BYB_PL_PRINCOL_1_5,
              BYBFILE.BYB_PL_INTCOL_1_5,
              BYBFILE.BYB_PL_LATECOL_1_5,
              BYBFILE.BYB_PL_O2COL_1_5,
              BYBFILE.BYB_AMT_SOLD_1,
              BYBFILE.BYB_AMT_TRANS_1,
              BYBFILE.BYB_AMT_PURCH_1,
              BYBFILE.BYB_NO_SOLD_1,
              BYBFILE.BYB_NO_TRANS_1,
              BYBFILE.BYB_NO_PURCH_1,
              BYBFILE.BYB_OTHCOL_1,
              BYBFILE.BYB_OT2COL_1,
              BYBFILE.BYB_INTCOL_1,
              BYBFILE.BYB_LATECOL_1,
              BYBFILE.BYB_PRINCOL_1,
              BYBFILE.BYB_DEFCOL_1,
              BYBFILE.BYB_RNPRIN_1,
              BYBFILE.BYB_INTREB_1,
              BYBFILE.BYB_CLREB1_1,
              BYBFILE.BYB_CLREB2_1,
              BYBFILE.BYB_CLREB3_1,
              BYBFILE.BYB_CLREB4_1,
              BYBFILE.BYB_AHREB_1,
              BYBFILE.BYB_PPREB_1,
              BYBFILE.BYB_O1REB_1,
              BYBFILE.BYB_O2REB_1,
              BYBFILE.BYB_O3REB_1,
              BYBFILE.BYB_O4REB_1,
              BYBFILE.BYB_O5REB_1,
              BYBFILE.BYB_O6REB_1,
              BYBFILE.BYB_O7REB_1,
              BYBFILE.BYB_SCREB_1,
              BYBFILE.BYB_MFREB_1,
              BYBFILE.BYB_DFREB_1,
              BYBFILE.BYB_UN7_REB_1,
              BYBFILE.BYB_UN8_REB_1,
              BYBFILE.BYB_UN9_REB_1,
              BYBFILE.BYB_UN10_REB_1,
              BYBFILE.BYB_PART_CHGOFF_1,
              BYBFILE.BYB_ALLOTMENTS_1,
              BYBFILE.BYB_INS_NO_ELIG_1_1,
              BYBFILE.BYB_INS_NO_WROTE_1_1,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
              BYBFILE.BYB_INS_AMT_WROTE_1_1,
              BYBFILE.BYB_INS_NO_ELIG_1_2,
              BYBFILE.BYB_INS_NO_WROTE_1_2,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
              BYBFILE.BYB_INS_AMT_WROTE_1_2,
              BYBFILE.BYB_INS_NO_ELIG_1_3,
              BYBFILE.BYB_INS_NO_WROTE_1_3,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
              BYBFILE.BYB_INS_AMT_WROTE_1_3,
              BYBFILE.BYB_INS_NO_ELIG_1_4,
              BYBFILE.BYB_INS_NO_WROTE_1_4,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
              BYBFILE.BYB_INS_AMT_WROTE_1_4,
              BYBFILE.BYB_INS_NO_ELIG_1_5,
              BYBFILE.BYB_INS_NO_WROTE_1_5,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
              BYBFILE.BYB_INS_AMT_WROTE_1_5,
              BYBFILE.BYB_INS_NO_ELIG_1_6,
              BYBFILE.BYB_INS_NO_WROTE_1_6,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
              BYBFILE.BYB_INS_AMT_WROTE_1_6,
              BYBFILE.BYB_INS_NO_ELIG_1_7,
              BYBFILE.BYB_INS_NO_WROTE_1_7,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
              BYBFILE.BYB_INS_AMT_WROTE_1_7,
              BYBFILE.BYB_INS_NO_ELIG_1_8,
              BYBFILE.BYB_INS_NO_WROTE_1_8,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
              BYBFILE.BYB_INS_AMT_WROTE_1_8,
              BYBFILE.BYB_INS_NO_ELIG_1_9,
              BYBFILE.BYB_INS_NO_WROTE_1_9,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
              BYBFILE.BYB_INS_AMT_WROTE_1_9,
              BYBFILE.BYB_INS_NO_ELIG_1_10,
              BYBFILE.BYB_INS_NO_WROTE_1_10,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
              BYBFILE.BYB_INS_AMT_WROTE_1_10,
              BYBFILE.BYB_FEE_NO_ELIG_1_1,
              BYBFILE.BYB_FEE_NO_WROTE_1_1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
              BYBFILE.BYB_FEE_AMT_WROTE_1_1,
              BYBFILE.BYB_FEE_NO_ELIG_1_2,
              BYBFILE.BYB_FEE_NO_WROTE_1_2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
              BYBFILE.BYB_FEE_AMT_WROTE_1_2,
              BYBFILE.BYB_FEE_NO_ELIG_1_3,
              BYBFILE.BYB_FEE_NO_WROTE_1_3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
              BYBFILE.BYB_FEE_AMT_WROTE_1_3,
              BYBFILE.BYB_OTH_NO_COL_1,
              BYBFILE.BYB_OT2_NO_COL_1,
              BYBFILE.BYB_INT_NO_COL_1,
              BYBFILE.BYB_LATE_NO_COL_1,
              BYBFILE.BYB_OTH_NO_WAIVED_1,
              BYBFILE.BYB_OT2_NO_WAIVED_1,
              BYBFILE.BYB_INT_NO_WAIVED_1,
              BYBFILE.BYB_LATE_NO_WAIVED_1,
              BYBFILE.BYB_OTH_AMT_WAIVED_1,
              BYBFILE.BYB_OT2_AMT_WAIVED_1,
              BYBFILE.BYB_INT_AMT_WAIVED_1,
              BYBFILE.BYB_LATE_AMT_WAIVED_1,
              BYBFILE.BYB_PL_PRINCOL_2_1,
              BYBFILE.BYB_PL_INTCOL_2_1,
              BYBFILE.BYB_PL_LATECOL_2_1,
              BYBFILE.BYB_PL_O2COL_2_1,
              BYBFILE.BYB_PL_PRINCOL_2_2,
              BYBFILE.BYB_PL_INTCOL_2_2,
              BYBFILE.BYB_PL_LATECOL_2_2,
              BYBFILE.BYB_PL_O2COL_2_2,
              BYBFILE.BYB_PL_PRINCOL_2_3,
              BYBFILE.BYB_PL_INTCOL_2_3,
              BYBFILE.BYB_PL_LATECOL_2_3,
              BYBFILE.BYB_PL_O2COL_2_3,
              BYBFILE.BYB_PL_PRINCOL_2_4,
              BYBFILE.BYB_PL_INTCOL_2_4,
              BYBFILE.BYB_PL_LATECOL_2_4,
              BYBFILE.BYB_PL_O2COL_2_4,
              BYBFILE.BYB_PL_PRINCOL_2_5,
              BYBFILE.BYB_PL_INTCOL_2_5,
              BYBFILE.BYB_PL_LATECOL_2_5,
              BYBFILE.BYB_PL_O2COL_2_5,
              BYBFILE.BYB_AMT_SOLD_2,
              BYBFILE.BYB_AMT_TRANS_2,
              BYBFILE.BYB_AMT_PURCH_2,
              BYBFILE.BYB_NO_SOLD_2,
              BYBFILE.BYB_NO_TRANS_2,
              BYBFILE.BYB_NO_PURCH_2,
              BYBFILE.BYB_OTHCOL_2,
              BYBFILE.BYB_OT2COL_2,
              BYBFILE.BYB_INTCOL_2,
              BYBFILE.BYB_LATECOL_2,
              BYBFILE.BYB_PRINCOL_2,
              BYBFILE.BYB_DEFCOL_2,
              BYBFILE.BYB_RNPRIN_2,
              BYBFILE.BYB_INTREB_2,
              BYBFILE.BYB_CLREB1_2,
              BYBFILE.BYB_CLREB2_2,
              BYBFILE.BYB_CLREB3_2,
              BYBFILE.BYB_CLREB4_2,
              BYBFILE.BYB_AHREB_2,
              BYBFILE.BYB_PPREB_2,
              BYBFILE.BYB_O1REB_2,
              BYBFILE.BYB_O2REB_2,
              BYBFILE.BYB_O3REB_2,
              BYBFILE.BYB_O4REB_2,
              BYBFILE.BYB_O5REB_2,
              BYBFILE.BYB_O6REB_2,
              BYBFILE.BYB_O7REB_2,
              BYBFILE.BYB_SCREB_2,
              BYBFILE.BYB_MFREB_2,
              BYBFILE.BYB_DFREB_2,
              BYBFILE.BYB_UN7_REB_2,
              BYBFILE.BYB_UN8_REB_2,
              BYBFILE.BYB_UN9_REB_2,
              BYBFILE.BYB_UN10_REB_2,
              BYBFILE.BYB_PART_CHGOFF_2,
              BYBFILE.BYB_ALLOTMENTS_2,
              BYBFILE.BYB_INS_NO_ELIG_2_1,
              BYBFILE.BYB_INS_NO_WROTE_2_1,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
              BYBFILE.BYB_INS_AMT_WROTE_2_1,
              BYBFILE.BYB_INS_NO_ELIG_2_2,
              BYBFILE.BYB_INS_NO_WROTE_2_2,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
              BYBFILE.BYB_INS_AMT_WROTE_2_2,
              BYBFILE.BYB_INS_NO_ELIG_2_3,
              BYBFILE.BYB_INS_NO_WROTE_2_3,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
              BYBFILE.BYB_INS_AMT_WROTE_2_3,
              BYBFILE.BYB_INS_NO_ELIG_2_4,
              BYBFILE.BYB_INS_NO_WROTE_2_4,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
              BYBFILE.BYB_INS_AMT_WROTE_2_4,
              BYBFILE.BYB_INS_NO_ELIG_2_5,
              BYBFILE.BYB_INS_NO_WROTE_2_5,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
              BYBFILE.BYB_INS_AMT_WROTE_2_5,
              BYBFILE.BYB_INS_NO_ELIG_2_6,
              BYBFILE.BYB_INS_NO_WROTE_2_6,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
              BYBFILE.BYB_INS_AMT_WROTE_2_6,
              BYBFILE.BYB_INS_NO_ELIG_2_7,
              BYBFILE.BYB_INS_NO_WROTE_2_7,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
              BYBFILE.BYB_INS_AMT_WROTE_2_7,
              BYBFILE.BYB_INS_NO_ELIG_2_8,
              BYBFILE.BYB_INS_NO_WROTE_2_8,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
              BYBFILE.BYB_INS_AMT_WROTE_2_8,
              BYBFILE.BYB_INS_NO_ELIG_2_9,
              BYBFILE.BYB_INS_NO_WROTE_2_9,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
              BYBFILE.BYB_INS_AMT_WROTE_2_9,
              BYBFILE.BYB_INS_NO_ELIG_2_10,
              BYBFILE.BYB_INS_NO_WROTE_2_10,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
              BYBFILE.BYB_INS_AMT_WROTE_2_10,
              BYBFILE.BYB_FEE_NO_ELIG_2_1,
              BYBFILE.BYB_FEE_NO_WROTE_2_1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
              BYBFILE.BYB_FEE_AMT_WROTE_2_1,
              BYBFILE.BYB_FEE_NO_ELIG_2_2,
              BYBFILE.BYB_FEE_NO_WROTE_2_2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
              BYBFILE.BYB_FEE_AMT_WROTE_2_2,
              BYBFILE.BYB_FEE_NO_ELIG_2_3,
              BYBFILE.BYB_FEE_NO_WROTE_2_3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
              BYBFILE.BYB_FEE_AMT_WROTE_2_3,
              BYBFILE.BYB_OTH_NO_COL_2,
              BYBFILE.BYB_OT2_NO_COL_2,
              BYBFILE.BYB_INT_NO_COL_2,
              BYBFILE.BYB_LATE_NO_COL_2,
              BYBFILE.BYB_OTH_NO_WAIVED_2,
              BYBFILE.BYB_OT2_NO_WAIVED_2,
              BYBFILE.BYB_INT_NO_WAIVED_2,
              BYBFILE.BYB_LATE_NO_WAIVED_2,
              BYBFILE.BYB_OTH_AMT_WAIVED_2,
              BYBFILE.BYB_OT2_AMT_WAIVED_2,
              BYBFILE.BYB_INT_AMT_WAIVED_2,
              BYBFILE.BYB_LATE_AMT_WAIVED_2,
              BYBFILE.BYB_UNITS_1_1_1,
              BYBFILE.BYB_OUTBAL_1_1_1,
              BYBFILE.BYB_UNITS_1_1_2,
              BYBFILE.BYB_OUTBAL_1_1_2,
              BYBFILE.BYB_UNITS_1_1_3,
              BYBFILE.BYB_OUTBAL_1_1_3,
              BYBFILE.BYB_UNITS_1_1_4,
              BYBFILE.BYB_OUTBAL_1_1_4,
              BYBFILE.BYB_UNITS_1_1_5,
              BYBFILE.BYB_OUTBAL_1_1_5,
              BYBFILE.BYB_UNITS_1_1_6,
              BYBFILE.BYB_OUTBAL_1_1_6,
              BYBFILE.BYB_UNITS_1_1_7,
              BYBFILE.BYB_OUTBAL_1_1_7,
              BYBFILE.BYB_UNITS_1_1_8,
              BYBFILE.BYB_OUTBAL_1_1_8,
              BYBFILE.BYB_UNITS_1_1_9,
              BYBFILE.BYB_OUTBAL_1_1_9,
              BYBFILE.BYB_UNITS_1_1_10,
              BYBFILE.BYB_OUTBAL_1_1_10,
              BYBFILE.BYB_UNITS_1_2_1,
              BYBFILE.BYB_OUTBAL_1_2_1,
              BYBFILE.BYB_UNITS_1_2_2,
              BYBFILE.BYB_OUTBAL_1_2_2,
              BYBFILE.BYB_UNITS_1_2_3,
              BYBFILE.BYB_OUTBAL_1_2_3,
              BYBFILE.BYB_UNITS_1_2_4,
              BYBFILE.BYB_OUTBAL_1_2_4,
              BYBFILE.BYB_UNITS_1_2_5,
              BYBFILE.BYB_OUTBAL_1_2_5,
              BYBFILE.BYB_UNITS_1_2_6,
              BYBFILE.BYB_OUTBAL_1_2_6,
              BYBFILE.BYB_UNITS_1_2_7,
              BYBFILE.BYB_OUTBAL_1_2_7,
              BYBFILE.BYB_UNITS_1_2_8,
              BYBFILE.BYB_OUTBAL_1_2_8,
              BYBFILE.BYB_UNITS_1_2_9,
              BYBFILE.BYB_OUTBAL_1_2_9,
              BYBFILE.BYB_UNITS_1_2_10,
              BYBFILE.BYB_OUTBAL_1_2_10,
              BYBFILE.BYB_UNITS_2_1_1,
              BYBFILE.BYB_OUTBAL_2_1_1,
              BYBFILE.BYB_UNITS_2_1_2,
              BYBFILE.BYB_OUTBAL_2_1_2,
              BYBFILE.BYB_UNITS_2_1_3,
              BYBFILE.BYB_OUTBAL_2_1_3,
              BYBFILE.BYB_UNITS_2_1_4,
              BYBFILE.BYB_OUTBAL_2_1_4,
              BYBFILE.BYB_UNITS_2_1_5,
              BYBFILE.BYB_OUTBAL_2_1_5,
              BYBFILE.BYB_UNITS_2_1_6,
              BYBFILE.BYB_OUTBAL_2_1_6,
              BYBFILE.BYB_UNITS_2_1_7,
              BYBFILE.BYB_OUTBAL_2_1_7,
              BYBFILE.BYB_UNITS_2_1_8,
              BYBFILE.BYB_OUTBAL_2_1_8,
              BYBFILE.BYB_UNITS_2_1_9,
              BYBFILE.BYB_OUTBAL_2_1_9,
              BYBFILE.BYB_UNITS_2_1_10,
              BYBFILE.BYB_OUTBAL_2_1_10,
              BYBFILE.BYB_UNITS_2_2_1,
              BYBFILE.BYB_OUTBAL_2_2_1,
              BYBFILE.BYB_UNITS_2_2_2,
              BYBFILE.BYB_OUTBAL_2_2_2,
              BYBFILE.BYB_UNITS_2_2_3,
              BYBFILE.BYB_OUTBAL_2_2_3,
              BYBFILE.BYB_UNITS_2_2_4,
              BYBFILE.BYB_OUTBAL_2_2_4,
              BYBFILE.BYB_UNITS_2_2_5,
              BYBFILE.BYB_OUTBAL_2_2_5,
              BYBFILE.BYB_UNITS_2_2_6,
              BYBFILE.BYB_OUTBAL_2_2_6,
              BYBFILE.BYB_UNITS_2_2_7,
              BYBFILE.BYB_OUTBAL_2_2_7,
              BYBFILE.BYB_UNITS_2_2_8,
              BYBFILE.BYB_OUTBAL_2_2_8,
              BYBFILE.BYB_UNITS_2_2_9,
              BYBFILE.BYB_OUTBAL_2_2_9,
              BYBFILE.BYB_UNITS_2_2_10,
              BYBFILE.BYB_OUTBAL_2_2_10,
              BYBFILE.BYB_UNITS_3_1_1,
              BYBFILE.BYB_OUTBAL_3_1_1,
              BYBFILE.BYB_UNITS_3_1_2,
              BYBFILE.BYB_OUTBAL_3_1_2,
              BYBFILE.BYB_UNITS_3_1_3,
              BYBFILE.BYB_OUTBAL_3_1_3,
              BYBFILE.BYB_UNITS_3_1_4,
              BYBFILE.BYB_OUTBAL_3_1_4,
              BYBFILE.BYB_UNITS_3_1_5,
              BYBFILE.BYB_OUTBAL_3_1_5,
              BYBFILE.BYB_UNITS_3_1_6,
              BYBFILE.BYB_OUTBAL_3_1_6,
              BYBFILE.BYB_UNITS_3_1_7,
              BYBFILE.BYB_OUTBAL_3_1_7,
              BYBFILE.BYB_UNITS_3_1_8,
              BYBFILE.BYB_OUTBAL_3_1_8,
              BYBFILE.BYB_UNITS_3_1_9,
              BYBFILE.BYB_OUTBAL_3_1_9,
              BYBFILE.BYB_UNITS_3_1_10,
              BYBFILE.BYB_OUTBAL_3_1_10,
              BYBFILE.BYB_UNITS_3_2_1,
              BYBFILE.BYB_OUTBAL_3_2_1,
              BYBFILE.BYB_UNITS_3_2_2,
              BYBFILE.BYB_OUTBAL_3_2_2,
              BYBFILE.BYB_UNITS_3_2_3,
              BYBFILE.BYB_OUTBAL_3_2_3,
              BYBFILE.BYB_UNITS_3_2_4,
              BYBFILE.BYB_OUTBAL_3_2_4,
              BYBFILE.BYB_UNITS_3_2_5,
              BYBFILE.BYB_OUTBAL_3_2_5,
              BYBFILE.BYB_UNITS_3_2_6,
              BYBFILE.BYB_OUTBAL_3_2_6,
              BYBFILE.BYB_UNITS_3_2_7,
              BYBFILE.BYB_OUTBAL_3_2_7,
              BYBFILE.BYB_UNITS_3_2_8,
              BYBFILE.BYB_OUTBAL_3_2_8,
              BYBFILE.BYB_UNITS_3_2_9,
              BYBFILE.BYB_OUTBAL_3_2_9,
              BYBFILE.BYB_UNITS_3_2_10,
              BYBFILE.BYB_OUTBAL_3_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
              BYBFILE.BYB_SUMWGTTM,
              BYBFILE.BYB_SUMWGTAV,
              BYBFILE.BYB_SUMWGTTA,
              BYBFILE.BYB_IB_ACCRUED,
              BYBFILE.BYB_DLDISC_1,
              BYBFILE.BYB_DLDISC_2,
              BYBFILE.BYB_DLDISC_3,
              BYBFILE.BYB_STD_POS,
              BYBFILE.BYB_30_DAY_MOVED,
              BYBFILE.BYB_PROJ_INCREASE,
              BYBFILE.BYB_PROJ_PROFIT,
              BYBFILE.BYB_PROCEEDS_1,
              BYBFILE.BYB_PROCEEDS_2,
              BYBFILE.BYB_LOCKBOX_1,
              BYBFILE.BYB_LOCKBOX_2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_1,
              BYBFILE.BYB_UNEARNED_AMT_1_1_2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_3,
              BYBFILE.BYB_UNEARNED_AMT_1_1_4,
              BYBFILE.BYB_UNEARNED_AMT_1_1_5,
              BYBFILE.BYB_UNEARNED_AMT_1_1_6,
              BYBFILE.BYB_UNEARNED_AMT_1_1_7,
              BYBFILE.BYB_UNEARNED_AMT_1_1_8,
              BYBFILE.BYB_UNEARNED_AMT_1_1_9,
              BYBFILE.BYB_UNEARNED_AMT_1_1_10,
              BYBFILE.BYB_UNEARNED_AMT_1_2_1,
              BYBFILE.BYB_UNEARNED_AMT_1_2_2,
              BYBFILE.BYB_UNEARNED_AMT_1_2_3,
              BYBFILE.BYB_UNEARNED_AMT_1_2_4,
              BYBFILE.BYB_UNEARNED_AMT_1_2_5,
              BYBFILE.BYB_UNEARNED_AMT_1_2_6,
              BYBFILE.BYB_UNEARNED_AMT_1_2_7,
              BYBFILE.BYB_UNEARNED_AMT_1_2_8,
              BYBFILE.BYB_UNEARNED_AMT_1_2_9,
              BYBFILE.BYB_UNEARNED_AMT_1_2_10,
              BYBFILE.BYB_UNEARNED_AMT_2_1_1,
              BYBFILE.BYB_UNEARNED_AMT_2_1_2,
              BYBFILE.BYB_UNEARNED_AMT_2_1_3,
              BYBFILE.BYB_UNEARNED_AMT_2_1_4,
              BYBFILE.BYB_UNEARNED_AMT_2_1_5,
              BYBFILE.BYB_UNEARNED_AMT_2_1_6,
              BYBFILE.BYB_UNEARNED_AMT_2_1_7,
              BYBFILE.BYB_UNEARNED_AMT_2_1_8,
              BYBFILE.BYB_UNEARNED_AMT_2_1_9,
              BYBFILE.BYB_UNEARNED_AMT_2_1_10,
              BYBFILE.BYB_UNEARNED_AMT_2_2_1,
              BYBFILE.BYB_UNEARNED_AMT_2_2_2,
              BYBFILE.BYB_UNEARNED_AMT_2_2_3,
              BYBFILE.BYB_UNEARNED_AMT_2_2_4,
              BYBFILE.BYB_UNEARNED_AMT_2_2_5,
              BYBFILE.BYB_UNEARNED_AMT_2_2_6,
              BYBFILE.BYB_UNEARNED_AMT_2_2_7,
              BYBFILE.BYB_UNEARNED_AMT_2_2_8,
              BYBFILE.BYB_UNEARNED_AMT_2_2_9,
              BYBFILE.BYB_UNEARNED_AMT_2_2_10,
              BYBFILE.BYB_UNEARNED_AMT_3_1_1,
              BYBFILE.BYB_UNEARNED_AMT_3_1_2,
              BYBFILE.BYB_UNEARNED_AMT_3_1_3,
              BYBFILE.BYB_UNEARNED_AMT_3_1_4,
              BYBFILE.BYB_UNEARNED_AMT_3_1_5,
              BYBFILE.BYB_UNEARNED_AMT_3_1_6,
              BYBFILE.BYB_UNEARNED_AMT_3_1_7,
              BYBFILE.BYB_UNEARNED_AMT_3_1_8,
              BYBFILE.BYB_UNEARNED_AMT_3_1_9,
              BYBFILE.BYB_UNEARNED_AMT_3_1_10,
              BYBFILE.BYB_UNEARNED_AMT_3_2_1,
              BYBFILE.BYB_UNEARNED_AMT_3_2_2,
              BYBFILE.BYB_UNEARNED_AMT_3_2_3,
              BYBFILE.BYB_UNEARNED_AMT_3_2_4,
              BYBFILE.BYB_UNEARNED_AMT_3_2_5,
              BYBFILE.BYB_UNEARNED_AMT_3_2_6,
              BYBFILE.BYB_UNEARNED_AMT_3_2_7,
              BYBFILE.BYB_UNEARNED_AMT_3_2_8,
              BYBFILE.BYB_UNEARNED_AMT_3_2_9,
              BYBFILE.BYB_UNEARNED_AMT_3_2_10)
            VALUES
             (:QBYB-DATE-CCYYMM,
              :QBYB-BRNO,
              :QBYB-CLASS,
              :QBYB-PL-PRINCOL-1-1,
              :QBYB-PL-INTCOL-1-1,
              :QBYB-PL-LATECOL-1-1,
              :QBYB-PL-O2COL-1-1,
              :QBYB-PL-PRINCOL-1-2,
              :QBYB-PL-INTCOL-1-2,
              :QBYB-PL-LATECOL-1-2,
              :QBYB-PL-O2COL-1-2,
              :QBYB-PL-PRINCOL-1-3,
              :QBYB-PL-INTCOL-1-3,
              :QBYB-PL-LATECOL-1-3,
              :QBYB-PL-O2COL-1-3,
              :QBYB-PL-PRINCOL-1-4,
              :QBYB-PL-INTCOL-1-4,
              :QBYB-PL-LATECOL-1-4,
              :QBYB-PL-O2COL-1-4,
              :QBYB-PL-PRINCOL-1-5,
              :QBYB-PL-INTCOL-1-5,
              :QBYB-PL-LATECOL-1-5,
              :QBYB-PL-O2COL-1-5,
              :QBYB-AMT-SOLD-1,
              :QBYB-AMT-TRANS-1,
              :QBYB-AMT-PURCH-1,
              :QBYB-NO-SOLD-1,
              :QBYB-NO-TRANS-1,
              :QBYB-NO-PURCH-1,
              :QBYB-OTHCOL-1,
              :QBYB-OT2COL-1,
              :QBYB-INTCOL-1,
              :QBYB-LATECOL-1,
              :QBYB-PRINCOL-1,
              :QBYB-DEFCOL-1,
              :QBYB-RNPRIN-1,
              :QBYB-INTREB-1,
              :QBYB-CLREB1-1,
              :QBYB-CLREB2-1,
              :QBYB-CLREB3-1,
              :QBYB-CLREB4-1,
              :QBYB-AHREB-1,
              :QBYB-PPREB-1,
              :QBYB-O1REB-1,
              :QBYB-O2REB-1,
              :QBYB-O3REB-1,
              :QBYB-O4REB-1,
              :QBYB-O5REB-1,
              :QBYB-O6REB-1,
              :QBYB-O7REB-1,
              :QBYB-SCREB-1,
              :QBYB-MFREB-1,
              :QBYB-DFREB-1,
              :QBYB-UN7-REB-1,
              :QBYB-UN8-REB-1,
              :QBYB-UN9-REB-1,
              :QBYB-UN10-REB-1,
              :QBYB-PART-CHGOFF-1,
              :QBYB-ALLOTMENTS-1,
              :QBYB-INS-NO-ELIG-1-1,
              :QBYB-INS-NO-WROTE-1-1,
              :QBYB-INS-NO-NOTWROTE-1-1,
              :QBYB-INS-AMT-WROTE-1-1,
              :QBYB-INS-NO-ELIG-1-2,
              :QBYB-INS-NO-WROTE-1-2,
              :QBYB-INS-NO-NOTWROTE-1-2,
              :QBYB-INS-AMT-WROTE-1-2,
              :QBYB-INS-NO-ELIG-1-3,
              :QBYB-INS-NO-WROTE-1-3,
              :QBYB-INS-NO-NOTWROTE-1-3,
              :QBYB-INS-AMT-WROTE-1-3,
              :QBYB-INS-NO-ELIG-1-4,
              :QBYB-INS-NO-WROTE-1-4,
              :QBYB-INS-NO-NOTWROTE-1-4,
              :QBYB-INS-AMT-WROTE-1-4,
              :QBYB-INS-NO-ELIG-1-5,
              :QBYB-INS-NO-WROTE-1-5,
              :QBYB-INS-NO-NOTWROTE-1-5,
              :QBYB-INS-AMT-WROTE-1-5,
              :QBYB-INS-NO-ELIG-1-6,
              :QBYB-INS-NO-WROTE-1-6,
              :QBYB-INS-NO-NOTWROTE-1-6,
              :QBYB-INS-AMT-WROTE-1-6,
              :QBYB-INS-NO-ELIG-1-7,
              :QBYB-INS-NO-WROTE-1-7,
              :QBYB-INS-NO-NOTWROTE-1-7,
              :QBYB-INS-AMT-WROTE-1-7,
              :QBYB-INS-NO-ELIG-1-8,
              :QBYB-INS-NO-WROTE-1-8,
              :QBYB-INS-NO-NOTWROTE-1-8,
              :QBYB-INS-AMT-WROTE-1-8,
              :QBYB-INS-NO-ELIG-1-9,
              :QBYB-INS-NO-WROTE-1-9,
              :QBYB-INS-NO-NOTWROTE-1-9,
              :QBYB-INS-AMT-WROTE-1-9,
              :QBYB-INS-NO-ELIG-1-10,
              :QBYB-INS-NO-WROTE-1-10,
              :QBYB-INS-NO-NOTWROTE-1-10,
              :QBYB-INS-AMT-WROTE-1-10,
              :QBYB-FEE-NO-ELIG-1-1,
              :QBYB-FEE-NO-WROTE-1-1,
              :QBYB-FEE-NO-NOTWROTE-1-1,
              :QBYB-FEE-AMT-WROTE-1-1,
              :QBYB-FEE-NO-ELIG-1-2,
              :QBYB-FEE-NO-WROTE-1-2,
              :QBYB-FEE-NO-NOTWROTE-1-2,
              :QBYB-FEE-AMT-WROTE-1-2,
              :QBYB-FEE-NO-ELIG-1-3,
              :QBYB-FEE-NO-WROTE-1-3,
              :QBYB-FEE-NO-NOTWROTE-1-3,
              :QBYB-FEE-AMT-WROTE-1-3,
              :QBYB-OTH-NO-COL-1,
              :QBYB-OT2-NO-COL-1,
              :QBYB-INT-NO-COL-1,
              :QBYB-LATE-NO-COL-1,
              :QBYB-OTH-NO-WAIVED-1,
              :QBYB-OT2-NO-WAIVED-1,
              :QBYB-INT-NO-WAIVED-1,
              :QBYB-LATE-NO-WAIVED-1,
              :QBYB-OTH-AMT-WAIVED-1,
              :QBYB-OT2-AMT-WAIVED-1,
              :QBYB-INT-AMT-WAIVED-1,
              :QBYB-LATE-AMT-WAIVED-1,
              :QBYB-PL-PRINCOL-2-1,
              :QBYB-PL-INTCOL-2-1,
              :QBYB-PL-LATECOL-2-1,
              :QBYB-PL-O2COL-2-1,
              :QBYB-PL-PRINCOL-2-2,
              :QBYB-PL-INTCOL-2-2,
              :QBYB-PL-LATECOL-2-2,
              :QBYB-PL-O2COL-2-2,
              :QBYB-PL-PRINCOL-2-3,
              :QBYB-PL-INTCOL-2-3,
              :QBYB-PL-LATECOL-2-3,
              :QBYB-PL-O2COL-2-3,
              :QBYB-PL-PRINCOL-2-4,
              :QBYB-PL-INTCOL-2-4,
              :QBYB-PL-LATECOL-2-4,
              :QBYB-PL-O2COL-2-4,
              :QBYB-PL-PRINCOL-2-5,
              :QBYB-PL-INTCOL-2-5,
              :QBYB-PL-LATECOL-2-5,
              :QBYB-PL-O2COL-2-5,
              :QBYB-AMT-SOLD-2,
              :QBYB-AMT-TRANS-2,
              :QBYB-AMT-PURCH-2,
              :QBYB-NO-SOLD-2,
              :QBYB-NO-TRANS-2,
              :QBYB-NO-PURCH-2,
              :QBYB-OTHCOL-2,
              :QBYB-OT2COL-2,
              :QBYB-INTCOL-2,
              :QBYB-LATECOL-2,
              :QBYB-PRINCOL-2,
              :QBYB-DEFCOL-2,
              :QBYB-RNPRIN-2,
              :QBYB-INTREB-2,
              :QBYB-CLREB1-2,
              :QBYB-CLREB2-2,
              :QBYB-CLREB3-2,
              :QBYB-CLREB4-2,
              :QBYB-AHREB-2,
              :QBYB-PPREB-2,
              :QBYB-O1REB-2,
              :QBYB-O2REB-2,
              :QBYB-O3REB-2,
              :QBYB-O4REB-2,
              :QBYB-O5REB-2,
              :QBYB-O6REB-2,
              :QBYB-O7REB-2,
              :QBYB-SCREB-2,
              :QBYB-MFREB-2,
              :QBYB-DFREB-2,
              :QBYB-UN7-REB-2,
              :QBYB-UN8-REB-2,
              :QBYB-UN9-REB-2,
              :QBYB-UN10-REB-2,
              :QBYB-PART-CHGOFF-2,
              :QBYB-ALLOTMENTS-2,
              :QBYB-INS-NO-ELIG-2-1,
              :QBYB-INS-NO-WROTE-2-1,
              :QBYB-INS-NO-NOTWROTE-2-1,
              :QBYB-INS-AMT-WROTE-2-1,
              :QBYB-INS-NO-ELIG-2-2,
              :QBYB-INS-NO-WROTE-2-2,
              :QBYB-INS-NO-NOTWROTE-2-2,
              :QBYB-INS-AMT-WROTE-2-2,
              :QBYB-INS-NO-ELIG-2-3,
              :QBYB-INS-NO-WROTE-2-3,
              :QBYB-INS-NO-NOTWROTE-2-3,
              :QBYB-INS-AMT-WROTE-2-3,
              :QBYB-INS-NO-ELIG-2-4,
              :QBYB-INS-NO-WROTE-2-4,
              :QBYB-INS-NO-NOTWROTE-2-4,
              :QBYB-INS-AMT-WROTE-2-4,
              :QBYB-INS-NO-ELIG-2-5,
              :QBYB-INS-NO-WROTE-2-5,
              :QBYB-INS-NO-NOTWROTE-2-5,
              :QBYB-INS-AMT-WROTE-2-5,
              :QBYB-INS-NO-ELIG-2-6,
              :QBYB-INS-NO-WROTE-2-6,
              :QBYB-INS-NO-NOTWROTE-2-6,
              :QBYB-INS-AMT-WROTE-2-6,
              :QBYB-INS-NO-ELIG-2-7,
              :QBYB-INS-NO-WROTE-2-7,
              :QBYB-INS-NO-NOTWROTE-2-7,
              :QBYB-INS-AMT-WROTE-2-7,
              :QBYB-INS-NO-ELIG-2-8,
              :QBYB-INS-NO-WROTE-2-8,
              :QBYB-INS-NO-NOTWROTE-2-8,
              :QBYB-INS-AMT-WROTE-2-8,
              :QBYB-INS-NO-ELIG-2-9,
              :QBYB-INS-NO-WROTE-2-9,
              :QBYB-INS-NO-NOTWROTE-2-9,
              :QBYB-INS-AMT-WROTE-2-9,
              :QBYB-INS-NO-ELIG-2-10,
              :QBYB-INS-NO-WROTE-2-10,
              :QBYB-INS-NO-NOTWROTE-2-10,
              :QBYB-INS-AMT-WROTE-2-10,
              :QBYB-FEE-NO-ELIG-2-1,
              :QBYB-FEE-NO-WROTE-2-1,
              :QBYB-FEE-NO-NOTWROTE-2-1,
              :QBYB-FEE-AMT-WROTE-2-1,
              :QBYB-FEE-NO-ELIG-2-2,
              :QBYB-FEE-NO-WROTE-2-2,
              :QBYB-FEE-NO-NOTWROTE-2-2,
              :QBYB-FEE-AMT-WROTE-2-2,
              :QBYB-FEE-NO-ELIG-2-3,
              :QBYB-FEE-NO-WROTE-2-3,
              :QBYB-FEE-NO-NOTWROTE-2-3,
              :QBYB-FEE-AMT-WROTE-2-3,
              :QBYB-OTH-NO-COL-2,
              :QBYB-OT2-NO-COL-2,
              :QBYB-INT-NO-COL-2,
              :QBYB-LATE-NO-COL-2,
              :QBYB-OTH-NO-WAIVED-2,
              :QBYB-OT2-NO-WAIVED-2,
              :QBYB-INT-NO-WAIVED-2,
              :QBYB-LATE-NO-WAIVED-2,
              :QBYB-OTH-AMT-WAIVED-2,
              :QBYB-OT2-AMT-WAIVED-2,
              :QBYB-INT-AMT-WAIVED-2,
              :QBYB-LATE-AMT-WAIVED-2,
              :QBYB-UNITS-1-1-1,
              :QBYB-OUTBAL-1-1-1,
              :QBYB-UNITS-1-1-2,
              :QBYB-OUTBAL-1-1-2,
              :QBYB-UNITS-1-1-3,
              :QBYB-OUTBAL-1-1-3,
              :QBYB-UNITS-1-1-4,
              :QBYB-OUTBAL-1-1-4,
              :QBYB-UNITS-1-1-5,
              :QBYB-OUTBAL-1-1-5,
              :QBYB-UNITS-1-1-6,
              :QBYB-OUTBAL-1-1-6,
              :QBYB-UNITS-1-1-7,
              :QBYB-OUTBAL-1-1-7,
              :QBYB-UNITS-1-1-8,
              :QBYB-OUTBAL-1-1-8,
              :QBYB-UNITS-1-1-9,
              :QBYB-OUTBAL-1-1-9,
              :QBYB-UNITS-1-1-10,
              :QBYB-OUTBAL-1-1-10,
              :QBYB-UNITS-1-2-1,
              :QBYB-OUTBAL-1-2-1,
              :QBYB-UNITS-1-2-2,
              :QBYB-OUTBAL-1-2-2,
              :QBYB-UNITS-1-2-3,
              :QBYB-OUTBAL-1-2-3,
              :QBYB-UNITS-1-2-4,
              :QBYB-OUTBAL-1-2-4,
              :QBYB-UNITS-1-2-5,
              :QBYB-OUTBAL-1-2-5,
              :QBYB-UNITS-1-2-6,
              :QBYB-OUTBAL-1-2-6,
              :QBYB-UNITS-1-2-7,
              :QBYB-OUTBAL-1-2-7,
              :QBYB-UNITS-1-2-8,
              :QBYB-OUTBAL-1-2-8,
              :QBYB-UNITS-1-2-9,
              :QBYB-OUTBAL-1-2-9,
              :QBYB-UNITS-1-2-10,
              :QBYB-OUTBAL-1-2-10,
              :QBYB-UNITS-2-1-1,
              :QBYB-OUTBAL-2-1-1,
              :QBYB-UNITS-2-1-2,
              :QBYB-OUTBAL-2-1-2,
              :QBYB-UNITS-2-1-3,
              :QBYB-OUTBAL-2-1-3,
              :QBYB-UNITS-2-1-4,
              :QBYB-OUTBAL-2-1-4,
              :QBYB-UNITS-2-1-5,
              :QBYB-OUTBAL-2-1-5,
              :QBYB-UNITS-2-1-6,
              :QBYB-OUTBAL-2-1-6,
              :QBYB-UNITS-2-1-7,
              :QBYB-OUTBAL-2-1-7,
              :QBYB-UNITS-2-1-8,
              :QBYB-OUTBAL-2-1-8,
              :QBYB-UNITS-2-1-9,
              :QBYB-OUTBAL-2-1-9,
              :QBYB-UNITS-2-1-10,
              :QBYB-OUTBAL-2-1-10,
              :QBYB-UNITS-2-2-1,
              :QBYB-OUTBAL-2-2-1,
              :QBYB-UNITS-2-2-2,
              :QBYB-OUTBAL-2-2-2,
              :QBYB-UNITS-2-2-3,
              :QBYB-OUTBAL-2-2-3,
              :QBYB-UNITS-2-2-4,
              :QBYB-OUTBAL-2-2-4,
              :QBYB-UNITS-2-2-5,
              :QBYB-OUTBAL-2-2-5,
              :QBYB-UNITS-2-2-6,
              :QBYB-OUTBAL-2-2-6,
              :QBYB-UNITS-2-2-7,
              :QBYB-OUTBAL-2-2-7,
              :QBYB-UNITS-2-2-8,
              :QBYB-OUTBAL-2-2-8,
              :QBYB-UNITS-2-2-9,
              :QBYB-OUTBAL-2-2-9,
              :QBYB-UNITS-2-2-10,
              :QBYB-OUTBAL-2-2-10,
              :QBYB-UNITS-3-1-1,
              :QBYB-OUTBAL-3-1-1,
              :QBYB-UNITS-3-1-2,
              :QBYB-OUTBAL-3-1-2,
              :QBYB-UNITS-3-1-3,
              :QBYB-OUTBAL-3-1-3,
              :QBYB-UNITS-3-1-4,
              :QBYB-OUTBAL-3-1-4,
              :QBYB-UNITS-3-1-5,
              :QBYB-OUTBAL-3-1-5,
              :QBYB-UNITS-3-1-6,
              :QBYB-OUTBAL-3-1-6,
              :QBYB-UNITS-3-1-7,
              :QBYB-OUTBAL-3-1-7,
              :QBYB-UNITS-3-1-8,
              :QBYB-OUTBAL-3-1-8,
              :QBYB-UNITS-3-1-9,
              :QBYB-OUTBAL-3-1-9,
              :QBYB-UNITS-3-1-10,
              :QBYB-OUTBAL-3-1-10,
              :QBYB-UNITS-3-2-1,
              :QBYB-OUTBAL-3-2-1,
              :QBYB-UNITS-3-2-2,
              :QBYB-OUTBAL-3-2-2,
              :QBYB-UNITS-3-2-3,
              :QBYB-OUTBAL-3-2-3,
              :QBYB-UNITS-3-2-4,
              :QBYB-OUTBAL-3-2-4,
              :QBYB-UNITS-3-2-5,
              :QBYB-OUTBAL-3-2-5,
              :QBYB-UNITS-3-2-6,
              :QBYB-OUTBAL-3-2-6,
              :QBYB-UNITS-3-2-7,
              :QBYB-OUTBAL-3-2-7,
              :QBYB-UNITS-3-2-8,
              :QBYB-OUTBAL-3-2-8,
              :QBYB-UNITS-3-2-9,
              :QBYB-OUTBAL-3-2-9,
              :QBYB-UNITS-3-2-10,
              :QBYB-OUTBAL-3-2-10,
              :QBYB-UNITS-BNKRPT-1-1-1,
              :QBYB-OUTBAL-BNKRPT-1-1-1,
              :QBYB-UNITS-BNKRPT-1-1-2,
              :QBYB-OUTBAL-BNKRPT-1-1-2,
              :QBYB-UNITS-BNKRPT-1-1-3,
              :QBYB-OUTBAL-BNKRPT-1-1-3,
              :QBYB-UNITS-BNKRPT-1-1-4,
              :QBYB-OUTBAL-BNKRPT-1-1-4,
              :QBYB-UNITS-BNKRPT-1-1-5,
              :QBYB-OUTBAL-BNKRPT-1-1-5,
              :QBYB-UNITS-BNKRPT-1-1-6,
              :QBYB-OUTBAL-BNKRPT-1-1-6,
              :QBYB-UNITS-BNKRPT-1-1-7,
              :QBYB-OUTBAL-BNKRPT-1-1-7,
              :QBYB-UNITS-BNKRPT-1-1-8,
              :QBYB-OUTBAL-BNKRPT-1-1-8,
              :QBYB-UNITS-BNKRPT-1-1-9,
              :QBYB-OUTBAL-BNKRPT-1-1-9,
              :QBYB-UNITS-BNKRPT-1-1-10,
              :QBYB-OUTBAL-BNKRPT-1-1-10,
              :QBYB-UNITS-BNKRPT-1-2-1,
              :QBYB-OUTBAL-BNKRPT-1-2-1,
              :QBYB-UNITS-BNKRPT-1-2-2,
              :QBYB-OUTBAL-BNKRPT-1-2-2,
              :QBYB-UNITS-BNKRPT-1-2-3,
              :QBYB-OUTBAL-BNKRPT-1-2-3,
              :QBYB-UNITS-BNKRPT-1-2-4,
              :QBYB-OUTBAL-BNKRPT-1-2-4,
              :QBYB-UNITS-BNKRPT-1-2-5,
              :QBYB-OUTBAL-BNKRPT-1-2-5,
              :QBYB-UNITS-BNKRPT-1-2-6,
              :QBYB-OUTBAL-BNKRPT-1-2-6,
              :QBYB-UNITS-BNKRPT-1-2-7,
              :QBYB-OUTBAL-BNKRPT-1-2-7,
              :QBYB-UNITS-BNKRPT-1-2-8,
              :QBYB-OUTBAL-BNKRPT-1-2-8,
              :QBYB-UNITS-BNKRPT-1-2-9,
              :QBYB-OUTBAL-BNKRPT-1-2-9,
              :QBYB-UNITS-BNKRPT-1-2-10,
              :QBYB-OUTBAL-BNKRPT-1-2-10,
              :QBYB-UNITS-BNKRPT-2-1-1,
              :QBYB-OUTBAL-BNKRPT-2-1-1,
              :QBYB-UNITS-BNKRPT-2-1-2,
              :QBYB-OUTBAL-BNKRPT-2-1-2,
              :QBYB-UNITS-BNKRPT-2-1-3,
              :QBYB-OUTBAL-BNKRPT-2-1-3,
              :QBYB-UNITS-BNKRPT-2-1-4,
              :QBYB-OUTBAL-BNKRPT-2-1-4,
              :QBYB-UNITS-BNKRPT-2-1-5,
              :QBYB-OUTBAL-BNKRPT-2-1-5,
              :QBYB-UNITS-BNKRPT-2-1-6,
              :QBYB-OUTBAL-BNKRPT-2-1-6,
              :QBYB-UNITS-BNKRPT-2-1-7,
              :QBYB-OUTBAL-BNKRPT-2-1-7,
              :QBYB-UNITS-BNKRPT-2-1-8,
              :QBYB-OUTBAL-BNKRPT-2-1-8,
              :QBYB-UNITS-BNKRPT-2-1-9,
              :QBYB-OUTBAL-BNKRPT-2-1-9,
              :QBYB-UNITS-BNKRPT-2-1-10,
              :QBYB-OUTBAL-BNKRPT-2-1-10,
              :QBYB-UNITS-BNKRPT-2-2-1,
              :QBYB-OUTBAL-BNKRPT-2-2-1,
              :QBYB-UNITS-BNKRPT-2-2-2,
              :QBYB-OUTBAL-BNKRPT-2-2-2,
              :QBYB-UNITS-BNKRPT-2-2-3,
              :QBYB-OUTBAL-BNKRPT-2-2-3,
              :QBYB-UNITS-BNKRPT-2-2-4,
              :QBYB-OUTBAL-BNKRPT-2-2-4,
              :QBYB-UNITS-BNKRPT-2-2-5,
              :QBYB-OUTBAL-BNKRPT-2-2-5,
              :QBYB-UNITS-BNKRPT-2-2-6,
              :QBYB-OUTBAL-BNKRPT-2-2-6,
              :QBYB-UNITS-BNKRPT-2-2-7,
              :QBYB-OUTBAL-BNKRPT-2-2-7,
              :QBYB-UNITS-BNKRPT-2-2-8,
              :QBYB-OUTBAL-BNKRPT-2-2-8,
              :QBYB-UNITS-BNKRPT-2-2-9,
              :QBYB-OUTBAL-BNKRPT-2-2-9,
              :QBYB-UNITS-BNKRPT-2-2-10,
              :QBYB-OUTBAL-BNKRPT-2-2-10,
              :QBYB-UNITS-BNKRPT-3-1-1,
              :QBYB-OUTBAL-BNKRPT-3-1-1,
              :QBYB-UNITS-BNKRPT-3-1-2,
              :QBYB-OUTBAL-BNKRPT-3-1-2,
              :QBYB-UNITS-BNKRPT-3-1-3,
              :QBYB-OUTBAL-BNKRPT-3-1-3,
              :QBYB-UNITS-BNKRPT-3-1-4,
              :QBYB-OUTBAL-BNKRPT-3-1-4,
              :QBYB-UNITS-BNKRPT-3-1-5,
              :QBYB-OUTBAL-BNKRPT-3-1-5,
              :QBYB-UNITS-BNKRPT-3-1-6,
              :QBYB-OUTBAL-BNKRPT-3-1-6,
              :QBYB-UNITS-BNKRPT-3-1-7,
              :QBYB-OUTBAL-BNKRPT-3-1-7,
              :QBYB-UNITS-BNKRPT-3-1-8,
              :QBYB-OUTBAL-BNKRPT-3-1-8,
              :QBYB-UNITS-BNKRPT-3-1-9,
              :QBYB-OUTBAL-BNKRPT-3-1-9,
              :QBYB-UNITS-BNKRPT-3-1-10,
              :QBYB-OUTBAL-BNKRPT-3-1-10,
              :QBYB-UNITS-BNKRPT-3-2-1,
              :QBYB-OUTBAL-BNKRPT-3-2-1,
              :QBYB-UNITS-BNKRPT-3-2-2,
              :QBYB-OUTBAL-BNKRPT-3-2-2,
              :QBYB-UNITS-BNKRPT-3-2-3,
              :QBYB-OUTBAL-BNKRPT-3-2-3,
              :QBYB-UNITS-BNKRPT-3-2-4,
              :QBYB-OUTBAL-BNKRPT-3-2-4,
              :QBYB-UNITS-BNKRPT-3-2-5,
              :QBYB-OUTBAL-BNKRPT-3-2-5,
              :QBYB-UNITS-BNKRPT-3-2-6,
              :QBYB-OUTBAL-BNKRPT-3-2-6,
              :QBYB-UNITS-BNKRPT-3-2-7,
              :QBYB-OUTBAL-BNKRPT-3-2-7,
              :QBYB-UNITS-BNKRPT-3-2-8,
              :QBYB-OUTBAL-BNKRPT-3-2-8,
              :QBYB-UNITS-BNKRPT-3-2-9,
              :QBYB-OUTBAL-BNKRPT-3-2-9,
              :QBYB-UNITS-BNKRPT-3-2-10,
              :QBYB-OUTBAL-BNKRPT-3-2-10,
              :QBYB-SUMWGTTM,
              :QBYB-SUMWGTAV,
              :QBYB-SUMWGTTA,
              :QBYB-IB-ACCRUED,
              :QBYB-DLDISC-1,
              :QBYB-DLDISC-2,
              :QBYB-DLDISC-3,
              :QBYB-STD-POS,
              :QBYB-30-DAY-MOVED,
              :QBYB-PROJ-INCREASE,
              :QBYB-PROJ-PROFIT,
              :QBYB-PROCEEDS-1,
              :QBYB-PROCEEDS-2,
              :QBYB-LOCKBOX-1,
              :QBYB-LOCKBOX-2,
              :QBYB-UNEARNED-AMT-1-1-1,
              :QBYB-UNEARNED-AMT-1-1-2,
              :QBYB-UNEARNED-AMT-1-1-3,
              :QBYB-UNEARNED-AMT-1-1-4,
              :QBYB-UNEARNED-AMT-1-1-5,
              :QBYB-UNEARNED-AMT-1-1-6,
              :QBYB-UNEARNED-AMT-1-1-7,
              :QBYB-UNEARNED-AMT-1-1-8,
              :QBYB-UNEARNED-AMT-1-1-9,
              :QBYB-UNEARNED-AMT-1-1-10,
              :QBYB-UNEARNED-AMT-1-2-1,
              :QBYB-UNEARNED-AMT-1-2-2,
              :QBYB-UNEARNED-AMT-1-2-3,
              :QBYB-UNEARNED-AMT-1-2-4,
              :QBYB-UNEARNED-AMT-1-2-5,
              :QBYB-UNEARNED-AMT-1-2-6,
              :QBYB-UNEARNED-AMT-1-2-7,
              :QBYB-UNEARNED-AMT-1-2-8,
              :QBYB-UNEARNED-AMT-1-2-9,
              :QBYB-UNEARNED-AMT-1-2-10,
              :QBYB-UNEARNED-AMT-2-1-1,
              :QBYB-UNEARNED-AMT-2-1-2,
              :QBYB-UNEARNED-AMT-2-1-3,
              :QBYB-UNEARNED-AMT-2-1-4,
              :QBYB-UNEARNED-AMT-2-1-5,
              :QBYB-UNEARNED-AMT-2-1-6,
              :QBYB-UNEARNED-AMT-2-1-7,
              :QBYB-UNEARNED-AMT-2-1-8,
              :QBYB-UNEARNED-AMT-2-1-9,
              :QBYB-UNEARNED-AMT-2-1-10,
              :QBYB-UNEARNED-AMT-2-2-1,
              :QBYB-UNEARNED-AMT-2-2-2,
              :QBYB-UNEARNED-AMT-2-2-3,
              :QBYB-UNEARNED-AMT-2-2-4,
              :QBYB-UNEARNED-AMT-2-2-5,
              :QBYB-UNEARNED-AMT-2-2-6,
              :QBYB-UNEARNED-AMT-2-2-7,
              :QBYB-UNEARNED-AMT-2-2-8,
              :QBYB-UNEARNED-AMT-2-2-9,
              :QBYB-UNEARNED-AMT-2-2-10,
              :QBYB-UNEARNED-AMT-3-1-1,
              :QBYB-UNEARNED-AMT-3-1-2,
              :QBYB-UNEARNED-AMT-3-1-3,
              :QBYB-UNEARNED-AMT-3-1-4,
              :QBYB-UNEARNED-AMT-3-1-5,
              :QBYB-UNEARNED-AMT-3-1-6,
              :QBYB-UNEARNED-AMT-3-1-7,
              :QBYB-UNEARNED-AMT-3-1-8,
              :QBYB-UNEARNED-AMT-3-1-9,
              :QBYB-UNEARNED-AMT-3-1-10,
              :QBYB-UNEARNED-AMT-3-2-1,
              :QBYB-UNEARNED-AMT-3-2-2,
              :QBYB-UNEARNED-AMT-3-2-3,
              :QBYB-UNEARNED-AMT-3-2-4,
              :QBYB-UNEARNED-AMT-3-2-5,
              :QBYB-UNEARNED-AMT-3-2-6,
              :QBYB-UNEARNED-AMT-3-2-7,
              :QBYB-UNEARNED-AMT-3-2-8,
              :QBYB-UNEARNED-AMT-3-2-9,
              :QBYB-UNEARNED-AMT-3-2-10)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BYB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      * USE C1 CONNECT
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYB1-FILE. *> EMBEDDED - BYBFILE

           PERFORM REWRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYBFILE
             SET
              BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM,
              BYBFILE.BYB_BRNO = :QBYB-BRNO,
              BYBFILE.BYB_CLASS = :QBYB-CLASS,
              BYBFILE.BYB_PL_PRINCOL_1_1 = :QBYB-PL-PRINCOL-1-1,
              BYBFILE.BYB_PL_INTCOL_1_1 = :QBYB-PL-INTCOL-1-1,
              BYBFILE.BYB_PL_LATECOL_1_1 = :QBYB-PL-LATECOL-1-1,
              BYBFILE.BYB_PL_O2COL_1_1 = :QBYB-PL-O2COL-1-1,
              BYBFILE.BYB_PL_PRINCOL_1_2 = :QBYB-PL-PRINCOL-1-2,
              BYBFILE.BYB_PL_INTCOL_1_2 = :QBYB-PL-INTCOL-1-2,
              BYBFILE.BYB_PL_LATECOL_1_2 = :QBYB-PL-LATECOL-1-2,
              BYBFILE.BYB_PL_O2COL_1_2 = :QBYB-PL-O2COL-1-2,
              BYBFILE.BYB_PL_PRINCOL_1_3 = :QBYB-PL-PRINCOL-1-3,
              BYBFILE.BYB_PL_INTCOL_1_3 = :QBYB-PL-INTCOL-1-3,
              BYBFILE.BYB_PL_LATECOL_1_3 = :QBYB-PL-LATECOL-1-3,
              BYBFILE.BYB_PL_O2COL_1_3 = :QBYB-PL-O2COL-1-3,
              BYBFILE.BYB_PL_PRINCOL_1_4 = :QBYB-PL-PRINCOL-1-4,
              BYBFILE.BYB_PL_INTCOL_1_4 = :QBYB-PL-INTCOL-1-4,
              BYBFILE.BYB_PL_LATECOL_1_4 = :QBYB-PL-LATECOL-1-4,
              BYBFILE.BYB_PL_O2COL_1_4 = :QBYB-PL-O2COL-1-4,
              BYBFILE.BYB_PL_PRINCOL_1_5 = :QBYB-PL-PRINCOL-1-5,
              BYBFILE.BYB_PL_INTCOL_1_5 = :QBYB-PL-INTCOL-1-5,
              BYBFILE.BYB_PL_LATECOL_1_5 = :QBYB-PL-LATECOL-1-5,
              BYBFILE.BYB_PL_O2COL_1_5 = :QBYB-PL-O2COL-1-5,
              BYBFILE.BYB_AMT_SOLD_1 = :QBYB-AMT-SOLD-1,
              BYBFILE.BYB_AMT_TRANS_1 = :QBYB-AMT-TRANS-1,
              BYBFILE.BYB_AMT_PURCH_1 = :QBYB-AMT-PURCH-1,
              BYBFILE.BYB_NO_SOLD_1 = :QBYB-NO-SOLD-1,
              BYBFILE.BYB_NO_TRANS_1 = :QBYB-NO-TRANS-1,
              BYBFILE.BYB_NO_PURCH_1 = :QBYB-NO-PURCH-1,
              BYBFILE.BYB_OTHCOL_1 = :QBYB-OTHCOL-1,
              BYBFILE.BYB_OT2COL_1 = :QBYB-OT2COL-1,
              BYBFILE.BYB_INTCOL_1 = :QBYB-INTCOL-1,
              BYBFILE.BYB_LATECOL_1 = :QBYB-LATECOL-1,
              BYBFILE.BYB_PRINCOL_1 = :QBYB-PRINCOL-1,
              BYBFILE.BYB_DEFCOL_1 = :QBYB-DEFCOL-1,
              BYBFILE.BYB_RNPRIN_1 = :QBYB-RNPRIN-1,
              BYBFILE.BYB_INTREB_1 = :QBYB-INTREB-1,
              BYBFILE.BYB_CLREB1_1 = :QBYB-CLREB1-1,
              BYBFILE.BYB_CLREB2_1 = :QBYB-CLREB2-1,
              BYBFILE.BYB_CLREB3_1 = :QBYB-CLREB3-1,
              BYBFILE.BYB_CLREB4_1 = :QBYB-CLREB4-1,
              BYBFILE.BYB_AHREB_1 = :QBYB-AHREB-1,
              BYBFILE.BYB_PPREB_1 = :QBYB-PPREB-1,
              BYBFILE.BYB_O1REB_1 = :QBYB-O1REB-1,
              BYBFILE.BYB_O2REB_1 = :QBYB-O2REB-1,
              BYBFILE.BYB_O3REB_1 = :QBYB-O3REB-1,
              BYBFILE.BYB_O4REB_1 = :QBYB-O4REB-1,
              BYBFILE.BYB_O5REB_1 = :QBYB-O5REB-1,
              BYBFILE.BYB_O6REB_1 = :QBYB-O6REB-1,
              BYBFILE.BYB_O7REB_1 = :QBYB-O7REB-1,
              BYBFILE.BYB_SCREB_1 = :QBYB-SCREB-1,
              BYBFILE.BYB_MFREB_1 = :QBYB-MFREB-1,
              BYBFILE.BYB_DFREB_1 = :QBYB-DFREB-1,
              BYBFILE.BYB_UN7_REB_1 = :QBYB-UN7-REB-1,
              BYBFILE.BYB_UN8_REB_1 = :QBYB-UN8-REB-1,
              BYBFILE.BYB_UN9_REB_1 = :QBYB-UN9-REB-1,
              BYBFILE.BYB_UN10_REB_1 = :QBYB-UN10-REB-1,
              BYBFILE.BYB_PART_CHGOFF_1 = :QBYB-PART-CHGOFF-1,
              BYBFILE.BYB_ALLOTMENTS_1 = :QBYB-ALLOTMENTS-1,
              BYBFILE.BYB_INS_NO_ELIG_1_1 = :QBYB-INS-NO-ELIG-1-1,
              BYBFILE.BYB_INS_NO_WROTE_1_1 =
                   :QBYB-INS-NO-WROTE-1-1,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_1 =
                   :QBYB-INS-NO-NOTWROTE-1-1,
              BYBFILE.BYB_INS_AMT_WROTE_1_1 =
                   :QBYB-INS-AMT-WROTE-1-1,
              BYBFILE.BYB_INS_NO_ELIG_1_2 = :QBYB-INS-NO-ELIG-1-2,
              BYBFILE.BYB_INS_NO_WROTE_1_2 =
                   :QBYB-INS-NO-WROTE-1-2,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_2 =
                   :QBYB-INS-NO-NOTWROTE-1-2,
              BYBFILE.BYB_INS_AMT_WROTE_1_2 =
                   :QBYB-INS-AMT-WROTE-1-2,
              BYBFILE.BYB_INS_NO_ELIG_1_3 = :QBYB-INS-NO-ELIG-1-3,
              BYBFILE.BYB_INS_NO_WROTE_1_3 =
                   :QBYB-INS-NO-WROTE-1-3,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_3 =
                   :QBYB-INS-NO-NOTWROTE-1-3,
              BYBFILE.BYB_INS_AMT_WROTE_1_3 =
                   :QBYB-INS-AMT-WROTE-1-3,
              BYBFILE.BYB_INS_NO_ELIG_1_4 = :QBYB-INS-NO-ELIG-1-4,
              BYBFILE.BYB_INS_NO_WROTE_1_4 =
                   :QBYB-INS-NO-WROTE-1-4,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_4 =
                   :QBYB-INS-NO-NOTWROTE-1-4,
              BYBFILE.BYB_INS_AMT_WROTE_1_4 =
                   :QBYB-INS-AMT-WROTE-1-4,
              BYBFILE.BYB_INS_NO_ELIG_1_5 = :QBYB-INS-NO-ELIG-1-5,
              BYBFILE.BYB_INS_NO_WROTE_1_5 =
                   :QBYB-INS-NO-WROTE-1-5,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_5 =
                   :QBYB-INS-NO-NOTWROTE-1-5,
              BYBFILE.BYB_INS_AMT_WROTE_1_5 =
                   :QBYB-INS-AMT-WROTE-1-5,
              BYBFILE.BYB_INS_NO_ELIG_1_6 = :QBYB-INS-NO-ELIG-1-6,
              BYBFILE.BYB_INS_NO_WROTE_1_6 =
                   :QBYB-INS-NO-WROTE-1-6,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_6 =
                   :QBYB-INS-NO-NOTWROTE-1-6,
              BYBFILE.BYB_INS_AMT_WROTE_1_6 =
                   :QBYB-INS-AMT-WROTE-1-6,
              BYBFILE.BYB_INS_NO_ELIG_1_7 = :QBYB-INS-NO-ELIG-1-7,
              BYBFILE.BYB_INS_NO_WROTE_1_7 =
                   :QBYB-INS-NO-WROTE-1-7,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_7 =
                   :QBYB-INS-NO-NOTWROTE-1-7,
              BYBFILE.BYB_INS_AMT_WROTE_1_7 =
                   :QBYB-INS-AMT-WROTE-1-7,
              BYBFILE.BYB_INS_NO_ELIG_1_8 = :QBYB-INS-NO-ELIG-1-8,
              BYBFILE.BYB_INS_NO_WROTE_1_8 =
                   :QBYB-INS-NO-WROTE-1-8,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_8 =
                   :QBYB-INS-NO-NOTWROTE-1-8,
              BYBFILE.BYB_INS_AMT_WROTE_1_8 =
                   :QBYB-INS-AMT-WROTE-1-8,
              BYBFILE.BYB_INS_NO_ELIG_1_9 = :QBYB-INS-NO-ELIG-1-9,
              BYBFILE.BYB_INS_NO_WROTE_1_9 =
                   :QBYB-INS-NO-WROTE-1-9,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_9 =
                   :QBYB-INS-NO-NOTWROTE-1-9,
              BYBFILE.BYB_INS_AMT_WROTE_1_9 =
                   :QBYB-INS-AMT-WROTE-1-9,
              BYBFILE.BYB_INS_NO_ELIG_1_10 =
                   :QBYB-INS-NO-ELIG-1-10,
              BYBFILE.BYB_INS_NO_WROTE_1_10 =
                   :QBYB-INS-NO-WROTE-1-10,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_10 =
                   :QBYB-INS-NO-NOTWROTE-1-10,
              BYBFILE.BYB_INS_AMT_WROTE_1_10 =
                   :QBYB-INS-AMT-WROTE-1-10,
              BYBFILE.BYB_FEE_NO_ELIG_1_1 = :QBYB-FEE-NO-ELIG-1-1,
              BYBFILE.BYB_FEE_NO_WROTE_1_1 =
                   :QBYB-FEE-NO-WROTE-1-1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_1 =
                   :QBYB-FEE-NO-NOTWROTE-1-1,
              BYBFILE.BYB_FEE_AMT_WROTE_1_1 =
                   :QBYB-FEE-AMT-WROTE-1-1,
              BYBFILE.BYB_FEE_NO_ELIG_1_2 = :QBYB-FEE-NO-ELIG-1-2,
              BYBFILE.BYB_FEE_NO_WROTE_1_2 =
                   :QBYB-FEE-NO-WROTE-1-2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_2 =
                   :QBYB-FEE-NO-NOTWROTE-1-2,
              BYBFILE.BYB_FEE_AMT_WROTE_1_2 =
                   :QBYB-FEE-AMT-WROTE-1-2,
              BYBFILE.BYB_FEE_NO_ELIG_1_3 = :QBYB-FEE-NO-ELIG-1-3,
              BYBFILE.BYB_FEE_NO_WROTE_1_3 =
                   :QBYB-FEE-NO-WROTE-1-3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_3 =
                   :QBYB-FEE-NO-NOTWROTE-1-3,
              BYBFILE.BYB_FEE_AMT_WROTE_1_3 =
                   :QBYB-FEE-AMT-WROTE-1-3,
              BYBFILE.BYB_OTH_NO_COL_1 = :QBYB-OTH-NO-COL-1,
              BYBFILE.BYB_OT2_NO_COL_1 = :QBYB-OT2-NO-COL-1,
              BYBFILE.BYB_INT_NO_COL_1 = :QBYB-INT-NO-COL-1,
              BYBFILE.BYB_LATE_NO_COL_1 = :QBYB-LATE-NO-COL-1,
              BYBFILE.BYB_OTH_NO_WAIVED_1 = :QBYB-OTH-NO-WAIVED-1,
              BYBFILE.BYB_OT2_NO_WAIVED_1 = :QBYB-OT2-NO-WAIVED-1,
              BYBFILE.BYB_INT_NO_WAIVED_1 = :QBYB-INT-NO-WAIVED-1,
              BYBFILE.BYB_LATE_NO_WAIVED_1 =
                   :QBYB-LATE-NO-WAIVED-1,
              BYBFILE.BYB_OTH_AMT_WAIVED_1 =
                   :QBYB-OTH-AMT-WAIVED-1,
              BYBFILE.BYB_OT2_AMT_WAIVED_1 =
                   :QBYB-OT2-AMT-WAIVED-1,
              BYBFILE.BYB_INT_AMT_WAIVED_1 =
                   :QBYB-INT-AMT-WAIVED-1,
              BYBFILE.BYB_LATE_AMT_WAIVED_1 =
                   :QBYB-LATE-AMT-WAIVED-1,
              BYBFILE.BYB_PL_PRINCOL_2_1 = :QBYB-PL-PRINCOL-2-1,
              BYBFILE.BYB_PL_INTCOL_2_1 = :QBYB-PL-INTCOL-2-1,
              BYBFILE.BYB_PL_LATECOL_2_1 = :QBYB-PL-LATECOL-2-1,
              BYBFILE.BYB_PL_O2COL_2_1 = :QBYB-PL-O2COL-2-1,
              BYBFILE.BYB_PL_PRINCOL_2_2 = :QBYB-PL-PRINCOL-2-2,
              BYBFILE.BYB_PL_INTCOL_2_2 = :QBYB-PL-INTCOL-2-2,
              BYBFILE.BYB_PL_LATECOL_2_2 = :QBYB-PL-LATECOL-2-2,
              BYBFILE.BYB_PL_O2COL_2_2 = :QBYB-PL-O2COL-2-2,
              BYBFILE.BYB_PL_PRINCOL_2_3 = :QBYB-PL-PRINCOL-2-3,
              BYBFILE.BYB_PL_INTCOL_2_3 = :QBYB-PL-INTCOL-2-3,
              BYBFILE.BYB_PL_LATECOL_2_3 = :QBYB-PL-LATECOL-2-3,
              BYBFILE.BYB_PL_O2COL_2_3 = :QBYB-PL-O2COL-2-3,
              BYBFILE.BYB_PL_PRINCOL_2_4 = :QBYB-PL-PRINCOL-2-4,
              BYBFILE.BYB_PL_INTCOL_2_4 = :QBYB-PL-INTCOL-2-4,
              BYBFILE.BYB_PL_LATECOL_2_4 = :QBYB-PL-LATECOL-2-4,
              BYBFILE.BYB_PL_O2COL_2_4 = :QBYB-PL-O2COL-2-4,
              BYBFILE.BYB_PL_PRINCOL_2_5 = :QBYB-PL-PRINCOL-2-5,
              BYBFILE.BYB_PL_INTCOL_2_5 = :QBYB-PL-INTCOL-2-5,
              BYBFILE.BYB_PL_LATECOL_2_5 = :QBYB-PL-LATECOL-2-5,
              BYBFILE.BYB_PL_O2COL_2_5 = :QBYB-PL-O2COL-2-5,
              BYBFILE.BYB_AMT_SOLD_2 = :QBYB-AMT-SOLD-2,
              BYBFILE.BYB_AMT_TRANS_2 = :QBYB-AMT-TRANS-2,
              BYBFILE.BYB_AMT_PURCH_2 = :QBYB-AMT-PURCH-2,
              BYBFILE.BYB_NO_SOLD_2 = :QBYB-NO-SOLD-2,
              BYBFILE.BYB_NO_TRANS_2 = :QBYB-NO-TRANS-2,
              BYBFILE.BYB_NO_PURCH_2 = :QBYB-NO-PURCH-2,
              BYBFILE.BYB_OTHCOL_2 = :QBYB-OTHCOL-2,
              BYBFILE.BYB_OT2COL_2 = :QBYB-OT2COL-2,
              BYBFILE.BYB_INTCOL_2 = :QBYB-INTCOL-2,
              BYBFILE.BYB_LATECOL_2 = :QBYB-LATECOL-2,
              BYBFILE.BYB_PRINCOL_2 = :QBYB-PRINCOL-2,
              BYBFILE.BYB_DEFCOL_2 = :QBYB-DEFCOL-2,
              BYBFILE.BYB_RNPRIN_2 = :QBYB-RNPRIN-2,
              BYBFILE.BYB_INTREB_2 = :QBYB-INTREB-2,
              BYBFILE.BYB_CLREB1_2 = :QBYB-CLREB1-2,
              BYBFILE.BYB_CLREB2_2 = :QBYB-CLREB2-2,
              BYBFILE.BYB_CLREB3_2 = :QBYB-CLREB3-2,
              BYBFILE.BYB_CLREB4_2 = :QBYB-CLREB4-2,
              BYBFILE.BYB_AHREB_2 = :QBYB-AHREB-2,
              BYBFILE.BYB_PPREB_2 = :QBYB-PPREB-2,
              BYBFILE.BYB_O1REB_2 = :QBYB-O1REB-2,
              BYBFILE.BYB_O2REB_2 = :QBYB-O2REB-2,
              BYBFILE.BYB_O3REB_2 = :QBYB-O3REB-2,
              BYBFILE.BYB_O4REB_2 = :QBYB-O4REB-2,
              BYBFILE.BYB_O5REB_2 = :QBYB-O5REB-2,
              BYBFILE.BYB_O6REB_2 = :QBYB-O6REB-2,
              BYBFILE.BYB_O7REB_2 = :QBYB-O7REB-2,
              BYBFILE.BYB_SCREB_2 = :QBYB-SCREB-2,
              BYBFILE.BYB_MFREB_2 = :QBYB-MFREB-2,
              BYBFILE.BYB_DFREB_2 = :QBYB-DFREB-2,
              BYBFILE.BYB_UN7_REB_2 = :QBYB-UN7-REB-2,
              BYBFILE.BYB_UN8_REB_2 = :QBYB-UN8-REB-2,
              BYBFILE.BYB_UN9_REB_2 = :QBYB-UN9-REB-2,
              BYBFILE.BYB_UN10_REB_2 = :QBYB-UN10-REB-2,
              BYBFILE.BYB_PART_CHGOFF_2 = :QBYB-PART-CHGOFF-2,
              BYBFILE.BYB_ALLOTMENTS_2 = :QBYB-ALLOTMENTS-2,
              BYBFILE.BYB_INS_NO_ELIG_2_1 = :QBYB-INS-NO-ELIG-2-1,
              BYBFILE.BYB_INS_NO_WROTE_2_1 =
                   :QBYB-INS-NO-WROTE-2-1,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_1 =
                   :QBYB-INS-NO-NOTWROTE-2-1,
              BYBFILE.BYB_INS_AMT_WROTE_2_1 =
                   :QBYB-INS-AMT-WROTE-2-1,
              BYBFILE.BYB_INS_NO_ELIG_2_2 = :QBYB-INS-NO-ELIG-2-2,
              BYBFILE.BYB_INS_NO_WROTE_2_2 =
                   :QBYB-INS-NO-WROTE-2-2,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_2 =
                   :QBYB-INS-NO-NOTWROTE-2-2,
              BYBFILE.BYB_INS_AMT_WROTE_2_2 =
                   :QBYB-INS-AMT-WROTE-2-2,
              BYBFILE.BYB_INS_NO_ELIG_2_3 = :QBYB-INS-NO-ELIG-2-3,
              BYBFILE.BYB_INS_NO_WROTE_2_3 =
                   :QBYB-INS-NO-WROTE-2-3,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_3 =
                   :QBYB-INS-NO-NOTWROTE-2-3,
              BYBFILE.BYB_INS_AMT_WROTE_2_3 =
                   :QBYB-INS-AMT-WROTE-2-3,
              BYBFILE.BYB_INS_NO_ELIG_2_4 = :QBYB-INS-NO-ELIG-2-4,
              BYBFILE.BYB_INS_NO_WROTE_2_4 =
                   :QBYB-INS-NO-WROTE-2-4,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_4 =
                   :QBYB-INS-NO-NOTWROTE-2-4,
              BYBFILE.BYB_INS_AMT_WROTE_2_4 =
                   :QBYB-INS-AMT-WROTE-2-4,
              BYBFILE.BYB_INS_NO_ELIG_2_5 = :QBYB-INS-NO-ELIG-2-5,
              BYBFILE.BYB_INS_NO_WROTE_2_5 =
                   :QBYB-INS-NO-WROTE-2-5,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_5 =
                   :QBYB-INS-NO-NOTWROTE-2-5,
              BYBFILE.BYB_INS_AMT_WROTE_2_5 =
                   :QBYB-INS-AMT-WROTE-2-5,
              BYBFILE.BYB_INS_NO_ELIG_2_6 = :QBYB-INS-NO-ELIG-2-6,
              BYBFILE.BYB_INS_NO_WROTE_2_6 =
                   :QBYB-INS-NO-WROTE-2-6,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_6 =
                   :QBYB-INS-NO-NOTWROTE-2-6,
              BYBFILE.BYB_INS_AMT_WROTE_2_6 =
                   :QBYB-INS-AMT-WROTE-2-6,
              BYBFILE.BYB_INS_NO_ELIG_2_7 = :QBYB-INS-NO-ELIG-2-7,
              BYBFILE.BYB_INS_NO_WROTE_2_7 =
                   :QBYB-INS-NO-WROTE-2-7,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_7 =
                   :QBYB-INS-NO-NOTWROTE-2-7,
              BYBFILE.BYB_INS_AMT_WROTE_2_7 =
                   :QBYB-INS-AMT-WROTE-2-7,
              BYBFILE.BYB_INS_NO_ELIG_2_8 = :QBYB-INS-NO-ELIG-2-8,
              BYBFILE.BYB_INS_NO_WROTE_2_8 =
                   :QBYB-INS-NO-WROTE-2-8,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_8 =
                   :QBYB-INS-NO-NOTWROTE-2-8,
              BYBFILE.BYB_INS_AMT_WROTE_2_8 =
                   :QBYB-INS-AMT-WROTE-2-8,
              BYBFILE.BYB_INS_NO_ELIG_2_9 = :QBYB-INS-NO-ELIG-2-9,
              BYBFILE.BYB_INS_NO_WROTE_2_9 =
                   :QBYB-INS-NO-WROTE-2-9,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_9 =
                   :QBYB-INS-NO-NOTWROTE-2-9,
              BYBFILE.BYB_INS_AMT_WROTE_2_9 =
                   :QBYB-INS-AMT-WROTE-2-9,
              BYBFILE.BYB_INS_NO_ELIG_2_10 =
                   :QBYB-INS-NO-ELIG-2-10,
              BYBFILE.BYB_INS_NO_WROTE_2_10 =
                   :QBYB-INS-NO-WROTE-2-10,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_10 =
                   :QBYB-INS-NO-NOTWROTE-2-10,
              BYBFILE.BYB_INS_AMT_WROTE_2_10 =
                   :QBYB-INS-AMT-WROTE-2-10,
              BYBFILE.BYB_FEE_NO_ELIG_2_1 = :QBYB-FEE-NO-ELIG-2-1,
              BYBFILE.BYB_FEE_NO_WROTE_2_1 =
                   :QBYB-FEE-NO-WROTE-2-1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_1 =
                   :QBYB-FEE-NO-NOTWROTE-2-1,
              BYBFILE.BYB_FEE_AMT_WROTE_2_1 =
                   :QBYB-FEE-AMT-WROTE-2-1,
              BYBFILE.BYB_FEE_NO_ELIG_2_2 = :QBYB-FEE-NO-ELIG-2-2,
              BYBFILE.BYB_FEE_NO_WROTE_2_2 =
                   :QBYB-FEE-NO-WROTE-2-2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_2 =
                   :QBYB-FEE-NO-NOTWROTE-2-2,
              BYBFILE.BYB_FEE_AMT_WROTE_2_2 =
                   :QBYB-FEE-AMT-WROTE-2-2,
              BYBFILE.BYB_FEE_NO_ELIG_2_3 = :QBYB-FEE-NO-ELIG-2-3,
              BYBFILE.BYB_FEE_NO_WROTE_2_3 =
                   :QBYB-FEE-NO-WROTE-2-3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_3 =
                   :QBYB-FEE-NO-NOTWROTE-2-3,
              BYBFILE.BYB_FEE_AMT_WROTE_2_3 =
                   :QBYB-FEE-AMT-WROTE-2-3,
              BYBFILE.BYB_OTH_NO_COL_2 = :QBYB-OTH-NO-COL-2,
              BYBFILE.BYB_OT2_NO_COL_2 = :QBYB-OT2-NO-COL-2,
              BYBFILE.BYB_INT_NO_COL_2 = :QBYB-INT-NO-COL-2,
              BYBFILE.BYB_LATE_NO_COL_2 = :QBYB-LATE-NO-COL-2,
              BYBFILE.BYB_OTH_NO_WAIVED_2 = :QBYB-OTH-NO-WAIVED-2,
              BYBFILE.BYB_OT2_NO_WAIVED_2 = :QBYB-OT2-NO-WAIVED-2,
              BYBFILE.BYB_INT_NO_WAIVED_2 = :QBYB-INT-NO-WAIVED-2,
              BYBFILE.BYB_LATE_NO_WAIVED_2 =
                   :QBYB-LATE-NO-WAIVED-2,
              BYBFILE.BYB_OTH_AMT_WAIVED_2 =
                   :QBYB-OTH-AMT-WAIVED-2,
              BYBFILE.BYB_OT2_AMT_WAIVED_2 =
                   :QBYB-OT2-AMT-WAIVED-2,
              BYBFILE.BYB_INT_AMT_WAIVED_2 =
                   :QBYB-INT-AMT-WAIVED-2,
              BYBFILE.BYB_LATE_AMT_WAIVED_2 =
                   :QBYB-LATE-AMT-WAIVED-2,
              BYBFILE.BYB_UNITS_1_1_1 = :QBYB-UNITS-1-1-1,
              BYBFILE.BYB_OUTBAL_1_1_1 = :QBYB-OUTBAL-1-1-1,
              BYBFILE.BYB_UNITS_1_1_2 = :QBYB-UNITS-1-1-2,
              BYBFILE.BYB_OUTBAL_1_1_2 = :QBYB-OUTBAL-1-1-2,
              BYBFILE.BYB_UNITS_1_1_3 = :QBYB-UNITS-1-1-3,
              BYBFILE.BYB_OUTBAL_1_1_3 = :QBYB-OUTBAL-1-1-3,
              BYBFILE.BYB_UNITS_1_1_4 = :QBYB-UNITS-1-1-4,
              BYBFILE.BYB_OUTBAL_1_1_4 = :QBYB-OUTBAL-1-1-4,
              BYBFILE.BYB_UNITS_1_1_5 = :QBYB-UNITS-1-1-5,
              BYBFILE.BYB_OUTBAL_1_1_5 = :QBYB-OUTBAL-1-1-5,
              BYBFILE.BYB_UNITS_1_1_6 = :QBYB-UNITS-1-1-6,
              BYBFILE.BYB_OUTBAL_1_1_6 = :QBYB-OUTBAL-1-1-6,
              BYBFILE.BYB_UNITS_1_1_7 = :QBYB-UNITS-1-1-7,
              BYBFILE.BYB_OUTBAL_1_1_7 = :QBYB-OUTBAL-1-1-7,
              BYBFILE.BYB_UNITS_1_1_8 = :QBYB-UNITS-1-1-8,
              BYBFILE.BYB_OUTBAL_1_1_8 = :QBYB-OUTBAL-1-1-8,
              BYBFILE.BYB_UNITS_1_1_9 = :QBYB-UNITS-1-1-9,
              BYBFILE.BYB_OUTBAL_1_1_9 = :QBYB-OUTBAL-1-1-9,
              BYBFILE.BYB_UNITS_1_1_10 = :QBYB-UNITS-1-1-10,
              BYBFILE.BYB_OUTBAL_1_1_10 = :QBYB-OUTBAL-1-1-10,
              BYBFILE.BYB_UNITS_1_2_1 = :QBYB-UNITS-1-2-1,
              BYBFILE.BYB_OUTBAL_1_2_1 = :QBYB-OUTBAL-1-2-1,
              BYBFILE.BYB_UNITS_1_2_2 = :QBYB-UNITS-1-2-2,
              BYBFILE.BYB_OUTBAL_1_2_2 = :QBYB-OUTBAL-1-2-2,
              BYBFILE.BYB_UNITS_1_2_3 = :QBYB-UNITS-1-2-3,
              BYBFILE.BYB_OUTBAL_1_2_3 = :QBYB-OUTBAL-1-2-3,
              BYBFILE.BYB_UNITS_1_2_4 = :QBYB-UNITS-1-2-4,
              BYBFILE.BYB_OUTBAL_1_2_4 = :QBYB-OUTBAL-1-2-4,
              BYBFILE.BYB_UNITS_1_2_5 = :QBYB-UNITS-1-2-5,
              BYBFILE.BYB_OUTBAL_1_2_5 = :QBYB-OUTBAL-1-2-5,
              BYBFILE.BYB_UNITS_1_2_6 = :QBYB-UNITS-1-2-6,
              BYBFILE.BYB_OUTBAL_1_2_6 = :QBYB-OUTBAL-1-2-6,
              BYBFILE.BYB_UNITS_1_2_7 = :QBYB-UNITS-1-2-7,
              BYBFILE.BYB_OUTBAL_1_2_7 = :QBYB-OUTBAL-1-2-7,
              BYBFILE.BYB_UNITS_1_2_8 = :QBYB-UNITS-1-2-8,
              BYBFILE.BYB_OUTBAL_1_2_8 = :QBYB-OUTBAL-1-2-8,
              BYBFILE.BYB_UNITS_1_2_9 = :QBYB-UNITS-1-2-9,
              BYBFILE.BYB_OUTBAL_1_2_9 = :QBYB-OUTBAL-1-2-9,
              BYBFILE.BYB_UNITS_1_2_10 = :QBYB-UNITS-1-2-10,
              BYBFILE.BYB_OUTBAL_1_2_10 = :QBYB-OUTBAL-1-2-10,
              BYBFILE.BYB_UNITS_2_1_1 = :QBYB-UNITS-2-1-1,
              BYBFILE.BYB_OUTBAL_2_1_1 = :QBYB-OUTBAL-2-1-1,
              BYBFILE.BYB_UNITS_2_1_2 = :QBYB-UNITS-2-1-2,
              BYBFILE.BYB_OUTBAL_2_1_2 = :QBYB-OUTBAL-2-1-2,
              BYBFILE.BYB_UNITS_2_1_3 = :QBYB-UNITS-2-1-3,
              BYBFILE.BYB_OUTBAL_2_1_3 = :QBYB-OUTBAL-2-1-3,
              BYBFILE.BYB_UNITS_2_1_4 = :QBYB-UNITS-2-1-4,
              BYBFILE.BYB_OUTBAL_2_1_4 = :QBYB-OUTBAL-2-1-4,
              BYBFILE.BYB_UNITS_2_1_5 = :QBYB-UNITS-2-1-5,
              BYBFILE.BYB_OUTBAL_2_1_5 = :QBYB-OUTBAL-2-1-5,
              BYBFILE.BYB_UNITS_2_1_6 = :QBYB-UNITS-2-1-6,
              BYBFILE.BYB_OUTBAL_2_1_6 = :QBYB-OUTBAL-2-1-6,
              BYBFILE.BYB_UNITS_2_1_7 = :QBYB-UNITS-2-1-7,
              BYBFILE.BYB_OUTBAL_2_1_7 = :QBYB-OUTBAL-2-1-7,
              BYBFILE.BYB_UNITS_2_1_8 = :QBYB-UNITS-2-1-8,
              BYBFILE.BYB_OUTBAL_2_1_8 = :QBYB-OUTBAL-2-1-8,
              BYBFILE.BYB_UNITS_2_1_9 = :QBYB-UNITS-2-1-9,
              BYBFILE.BYB_OUTBAL_2_1_9 = :QBYB-OUTBAL-2-1-9,
              BYBFILE.BYB_UNITS_2_1_10 = :QBYB-UNITS-2-1-10,
              BYBFILE.BYB_OUTBAL_2_1_10 = :QBYB-OUTBAL-2-1-10,
              BYBFILE.BYB_UNITS_2_2_1 = :QBYB-UNITS-2-2-1,
              BYBFILE.BYB_OUTBAL_2_2_1 = :QBYB-OUTBAL-2-2-1,
              BYBFILE.BYB_UNITS_2_2_2 = :QBYB-UNITS-2-2-2,
              BYBFILE.BYB_OUTBAL_2_2_2 = :QBYB-OUTBAL-2-2-2,
              BYBFILE.BYB_UNITS_2_2_3 = :QBYB-UNITS-2-2-3,
              BYBFILE.BYB_OUTBAL_2_2_3 = :QBYB-OUTBAL-2-2-3,
              BYBFILE.BYB_UNITS_2_2_4 = :QBYB-UNITS-2-2-4,
              BYBFILE.BYB_OUTBAL_2_2_4 = :QBYB-OUTBAL-2-2-4,
              BYBFILE.BYB_UNITS_2_2_5 = :QBYB-UNITS-2-2-5,
              BYBFILE.BYB_OUTBAL_2_2_5 = :QBYB-OUTBAL-2-2-5,
              BYBFILE.BYB_UNITS_2_2_6 = :QBYB-UNITS-2-2-6,
              BYBFILE.BYB_OUTBAL_2_2_6 = :QBYB-OUTBAL-2-2-6,
              BYBFILE.BYB_UNITS_2_2_7 = :QBYB-UNITS-2-2-7,
              BYBFILE.BYB_OUTBAL_2_2_7 = :QBYB-OUTBAL-2-2-7,
              BYBFILE.BYB_UNITS_2_2_8 = :QBYB-UNITS-2-2-8,
              BYBFILE.BYB_OUTBAL_2_2_8 = :QBYB-OUTBAL-2-2-8,
              BYBFILE.BYB_UNITS_2_2_9 = :QBYB-UNITS-2-2-9,
              BYBFILE.BYB_OUTBAL_2_2_9 = :QBYB-OUTBAL-2-2-9,
              BYBFILE.BYB_UNITS_2_2_10 = :QBYB-UNITS-2-2-10,
              BYBFILE.BYB_OUTBAL_2_2_10 = :QBYB-OUTBAL-2-2-10,
              BYBFILE.BYB_UNITS_3_1_1 = :QBYB-UNITS-3-1-1,
              BYBFILE.BYB_OUTBAL_3_1_1 = :QBYB-OUTBAL-3-1-1,
              BYBFILE.BYB_UNITS_3_1_2 = :QBYB-UNITS-3-1-2,
              BYBFILE.BYB_OUTBAL_3_1_2 = :QBYB-OUTBAL-3-1-2,
              BYBFILE.BYB_UNITS_3_1_3 = :QBYB-UNITS-3-1-3,
              BYBFILE.BYB_OUTBAL_3_1_3 = :QBYB-OUTBAL-3-1-3,
              BYBFILE.BYB_UNITS_3_1_4 = :QBYB-UNITS-3-1-4,
              BYBFILE.BYB_OUTBAL_3_1_4 = :QBYB-OUTBAL-3-1-4,
              BYBFILE.BYB_UNITS_3_1_5 = :QBYB-UNITS-3-1-5,
              BYBFILE.BYB_OUTBAL_3_1_5 = :QBYB-OUTBAL-3-1-5,
              BYBFILE.BYB_UNITS_3_1_6 = :QBYB-UNITS-3-1-6,
              BYBFILE.BYB_OUTBAL_3_1_6 = :QBYB-OUTBAL-3-1-6,
              BYBFILE.BYB_UNITS_3_1_7 = :QBYB-UNITS-3-1-7,
              BYBFILE.BYB_OUTBAL_3_1_7 = :QBYB-OUTBAL-3-1-7,
              BYBFILE.BYB_UNITS_3_1_8 = :QBYB-UNITS-3-1-8,
              BYBFILE.BYB_OUTBAL_3_1_8 = :QBYB-OUTBAL-3-1-8,
              BYBFILE.BYB_UNITS_3_1_9 = :QBYB-UNITS-3-1-9,
              BYBFILE.BYB_OUTBAL_3_1_9 = :QBYB-OUTBAL-3-1-9,
              BYBFILE.BYB_UNITS_3_1_10 = :QBYB-UNITS-3-1-10,
              BYBFILE.BYB_OUTBAL_3_1_10 = :QBYB-OUTBAL-3-1-10,
              BYBFILE.BYB_UNITS_3_2_1 = :QBYB-UNITS-3-2-1,
              BYBFILE.BYB_OUTBAL_3_2_1 = :QBYB-OUTBAL-3-2-1,
              BYBFILE.BYB_UNITS_3_2_2 = :QBYB-UNITS-3-2-2,
              BYBFILE.BYB_OUTBAL_3_2_2 = :QBYB-OUTBAL-3-2-2,
              BYBFILE.BYB_UNITS_3_2_3 = :QBYB-UNITS-3-2-3,
              BYBFILE.BYB_OUTBAL_3_2_3 = :QBYB-OUTBAL-3-2-3,
              BYBFILE.BYB_UNITS_3_2_4 = :QBYB-UNITS-3-2-4,
              BYBFILE.BYB_OUTBAL_3_2_4 = :QBYB-OUTBAL-3-2-4,
              BYBFILE.BYB_UNITS_3_2_5 = :QBYB-UNITS-3-2-5,
              BYBFILE.BYB_OUTBAL_3_2_5 = :QBYB-OUTBAL-3-2-5,
              BYBFILE.BYB_UNITS_3_2_6 = :QBYB-UNITS-3-2-6,
              BYBFILE.BYB_OUTBAL_3_2_6 = :QBYB-OUTBAL-3-2-6,
              BYBFILE.BYB_UNITS_3_2_7 = :QBYB-UNITS-3-2-7,
              BYBFILE.BYB_OUTBAL_3_2_7 = :QBYB-OUTBAL-3-2-7,
              BYBFILE.BYB_UNITS_3_2_8 = :QBYB-UNITS-3-2-8,
              BYBFILE.BYB_OUTBAL_3_2_8 = :QBYB-OUTBAL-3-2-8,
              BYBFILE.BYB_UNITS_3_2_9 = :QBYB-UNITS-3-2-9,
              BYBFILE.BYB_OUTBAL_3_2_9 = :QBYB-OUTBAL-3-2-9,
              BYBFILE.BYB_UNITS_3_2_10 = :QBYB-UNITS-3-2-10,
              BYBFILE.BYB_OUTBAL_3_2_10 = :QBYB-OUTBAL-3-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_1 =
                   :QBYB-UNITS-BNKRPT-1-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1 =
                   :QBYB-OUTBAL-BNKRPT-1-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_2 =
                   :QBYB-UNITS-BNKRPT-1-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2 =
                   :QBYB-OUTBAL-BNKRPT-1-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_3 =
                   :QBYB-UNITS-BNKRPT-1-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3 =
                   :QBYB-OUTBAL-BNKRPT-1-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_4 =
                   :QBYB-UNITS-BNKRPT-1-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4 =
                   :QBYB-OUTBAL-BNKRPT-1-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_5 =
                   :QBYB-UNITS-BNKRPT-1-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5 =
                   :QBYB-OUTBAL-BNKRPT-1-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_6 =
                   :QBYB-UNITS-BNKRPT-1-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6 =
                   :QBYB-OUTBAL-BNKRPT-1-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_7 =
                   :QBYB-UNITS-BNKRPT-1-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7 =
                   :QBYB-OUTBAL-BNKRPT-1-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_8 =
                   :QBYB-UNITS-BNKRPT-1-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8 =
                   :QBYB-OUTBAL-BNKRPT-1-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_9 =
                   :QBYB-UNITS-BNKRPT-1-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9 =
                   :QBYB-OUTBAL-BNKRPT-1-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_10 =
                   :QBYB-UNITS-BNKRPT-1-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10 =
                   :QBYB-OUTBAL-BNKRPT-1-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_1 =
                   :QBYB-UNITS-BNKRPT-1-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1 =
                   :QBYB-OUTBAL-BNKRPT-1-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_2 =
                   :QBYB-UNITS-BNKRPT-1-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2 =
                   :QBYB-OUTBAL-BNKRPT-1-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_3 =
                   :QBYB-UNITS-BNKRPT-1-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3 =
                   :QBYB-OUTBAL-BNKRPT-1-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_4 =
                   :QBYB-UNITS-BNKRPT-1-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4 =
                   :QBYB-OUTBAL-BNKRPT-1-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_5 =
                   :QBYB-UNITS-BNKRPT-1-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5 =
                   :QBYB-OUTBAL-BNKRPT-1-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_6 =
                   :QBYB-UNITS-BNKRPT-1-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6 =
                   :QBYB-OUTBAL-BNKRPT-1-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_7 =
                   :QBYB-UNITS-BNKRPT-1-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7 =
                   :QBYB-OUTBAL-BNKRPT-1-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_8 =
                   :QBYB-UNITS-BNKRPT-1-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8 =
                   :QBYB-OUTBAL-BNKRPT-1-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_9 =
                   :QBYB-UNITS-BNKRPT-1-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9 =
                   :QBYB-OUTBAL-BNKRPT-1-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_10 =
                   :QBYB-UNITS-BNKRPT-1-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10 =
                   :QBYB-OUTBAL-BNKRPT-1-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_1 =
                   :QBYB-UNITS-BNKRPT-2-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1 =
                   :QBYB-OUTBAL-BNKRPT-2-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_2 =
                   :QBYB-UNITS-BNKRPT-2-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2 =
                   :QBYB-OUTBAL-BNKRPT-2-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_3 =
                   :QBYB-UNITS-BNKRPT-2-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3 =
                   :QBYB-OUTBAL-BNKRPT-2-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_4 =
                   :QBYB-UNITS-BNKRPT-2-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4 =
                   :QBYB-OUTBAL-BNKRPT-2-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_5 =
                   :QBYB-UNITS-BNKRPT-2-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5 =
                   :QBYB-OUTBAL-BNKRPT-2-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_6 =
                   :QBYB-UNITS-BNKRPT-2-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6 =
                   :QBYB-OUTBAL-BNKRPT-2-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_7 =
                   :QBYB-UNITS-BNKRPT-2-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7 =
                   :QBYB-OUTBAL-BNKRPT-2-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_8 =
                   :QBYB-UNITS-BNKRPT-2-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8 =
                   :QBYB-OUTBAL-BNKRPT-2-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_9 =
                   :QBYB-UNITS-BNKRPT-2-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9 =
                   :QBYB-OUTBAL-BNKRPT-2-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_10 =
                   :QBYB-UNITS-BNKRPT-2-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10 =
                   :QBYB-OUTBAL-BNKRPT-2-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_1 =
                   :QBYB-UNITS-BNKRPT-2-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1 =
                   :QBYB-OUTBAL-BNKRPT-2-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_2 =
                   :QBYB-UNITS-BNKRPT-2-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2 =
                   :QBYB-OUTBAL-BNKRPT-2-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_3 =
                   :QBYB-UNITS-BNKRPT-2-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3 =
                   :QBYB-OUTBAL-BNKRPT-2-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_4 =
                   :QBYB-UNITS-BNKRPT-2-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4 =
                   :QBYB-OUTBAL-BNKRPT-2-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_5 =
                   :QBYB-UNITS-BNKRPT-2-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5 =
                   :QBYB-OUTBAL-BNKRPT-2-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_6 =
                   :QBYB-UNITS-BNKRPT-2-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6 =
                   :QBYB-OUTBAL-BNKRPT-2-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_7 =
                   :QBYB-UNITS-BNKRPT-2-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7 =
                   :QBYB-OUTBAL-BNKRPT-2-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_8 =
                   :QBYB-UNITS-BNKRPT-2-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8 =
                   :QBYB-OUTBAL-BNKRPT-2-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_9 =
                   :QBYB-UNITS-BNKRPT-2-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9 =
                   :QBYB-OUTBAL-BNKRPT-2-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_10 =
                   :QBYB-UNITS-BNKRPT-2-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10 =
                   :QBYB-OUTBAL-BNKRPT-2-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_1 =
                   :QBYB-UNITS-BNKRPT-3-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1 =
                   :QBYB-OUTBAL-BNKRPT-3-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_2 =
                   :QBYB-UNITS-BNKRPT-3-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2 =
                   :QBYB-OUTBAL-BNKRPT-3-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_3 =
                   :QBYB-UNITS-BNKRPT-3-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3 =
                   :QBYB-OUTBAL-BNKRPT-3-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_4 =
                   :QBYB-UNITS-BNKRPT-3-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4 =
                   :QBYB-OUTBAL-BNKRPT-3-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_5 =
                   :QBYB-UNITS-BNKRPT-3-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5 =
                   :QBYB-OUTBAL-BNKRPT-3-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_6 =
                   :QBYB-UNITS-BNKRPT-3-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6 =
                   :QBYB-OUTBAL-BNKRPT-3-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_7 =
                   :QBYB-UNITS-BNKRPT-3-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7 =
                   :QBYB-OUTBAL-BNKRPT-3-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_8 =
                   :QBYB-UNITS-BNKRPT-3-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8 =
                   :QBYB-OUTBAL-BNKRPT-3-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_9 =
                   :QBYB-UNITS-BNKRPT-3-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9 =
                   :QBYB-OUTBAL-BNKRPT-3-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_10 =
                   :QBYB-UNITS-BNKRPT-3-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10 =
                   :QBYB-OUTBAL-BNKRPT-3-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_1 =
                   :QBYB-UNITS-BNKRPT-3-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1 =
                   :QBYB-OUTBAL-BNKRPT-3-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_2 =
                   :QBYB-UNITS-BNKRPT-3-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2 =
                   :QBYB-OUTBAL-BNKRPT-3-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_3 =
                   :QBYB-UNITS-BNKRPT-3-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3 =
                   :QBYB-OUTBAL-BNKRPT-3-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_4 =
                   :QBYB-UNITS-BNKRPT-3-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4 =
                   :QBYB-OUTBAL-BNKRPT-3-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_5 =
                   :QBYB-UNITS-BNKRPT-3-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5 =
                   :QBYB-OUTBAL-BNKRPT-3-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_6 =
                   :QBYB-UNITS-BNKRPT-3-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6 =
                   :QBYB-OUTBAL-BNKRPT-3-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_7 =
                   :QBYB-UNITS-BNKRPT-3-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7 =
                   :QBYB-OUTBAL-BNKRPT-3-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_8 =
                   :QBYB-UNITS-BNKRPT-3-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8 =
                   :QBYB-OUTBAL-BNKRPT-3-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_9 =
                   :QBYB-UNITS-BNKRPT-3-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9 =
                   :QBYB-OUTBAL-BNKRPT-3-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_10 =
                   :QBYB-UNITS-BNKRPT-3-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10 =
                   :QBYB-OUTBAL-BNKRPT-3-2-10,
              BYBFILE.BYB_SUMWGTTM = :QBYB-SUMWGTTM,
              BYBFILE.BYB_SUMWGTAV = :QBYB-SUMWGTAV,
              BYBFILE.BYB_SUMWGTTA = :QBYB-SUMWGTTA,
              BYBFILE.BYB_IB_ACCRUED = :QBYB-IB-ACCRUED,
              BYBFILE.BYB_DLDISC_1 = :QBYB-DLDISC-1,
              BYBFILE.BYB_DLDISC_2 = :QBYB-DLDISC-2,
              BYBFILE.BYB_DLDISC_3 = :QBYB-DLDISC-3,
              BYBFILE.BYB_STD_POS = :QBYB-STD-POS,
              BYBFILE.BYB_30_DAY_MOVED = :QBYB-30-DAY-MOVED,
              BYBFILE.BYB_PROJ_INCREASE = :QBYB-PROJ-INCREASE,
              BYBFILE.BYB_PROJ_PROFIT = :QBYB-PROJ-PROFIT,
              BYBFILE.BYB_PROCEEDS_1 = :QBYB-PROCEEDS-1,
              BYBFILE.BYB_PROCEEDS_2 = :QBYB-PROCEEDS-2,
              BYBFILE.BYB_LOCKBOX_1 = :QBYB-LOCKBOX-1,
              BYBFILE.BYB_LOCKBOX_2 = :QBYB-LOCKBOX-2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_1 =
                   :QBYB-UNEARNED-AMT-1-1-1,
              BYBFILE.BYB_UNEARNED_AMT_1_1_2 =
                   :QBYB-UNEARNED-AMT-1-1-2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_3 =
                   :QBYB-UNEARNED-AMT-1-1-3,
              BYBFILE.BYB_UNEARNED_AMT_1_1_4 =
                   :QBYB-UNEARNED-AMT-1-1-4,
              BYBFILE.BYB_UNEARNED_AMT_1_1_5 =
                   :QBYB-UNEARNED-AMT-1-1-5,
              BYBFILE.BYB_UNEARNED_AMT_1_1_6 =
                   :QBYB-UNEARNED-AMT-1-1-6,
              BYBFILE.BYB_UNEARNED_AMT_1_1_7 =
                   :QBYB-UNEARNED-AMT-1-1-7,
              BYBFILE.BYB_UNEARNED_AMT_1_1_8 =
                   :QBYB-UNEARNED-AMT-1-1-8,
              BYBFILE.BYB_UNEARNED_AMT_1_1_9 =
                   :QBYB-UNEARNED-AMT-1-1-9,
              BYBFILE.BYB_UNEARNED_AMT_1_1_10 =
                   :QBYB-UNEARNED-AMT-1-1-10,
              BYBFILE.BYB_UNEARNED_AMT_1_2_1 =
                   :QBYB-UNEARNED-AMT-1-2-1,
              BYBFILE.BYB_UNEARNED_AMT_1_2_2 =
                   :QBYB-UNEARNED-AMT-1-2-2,
              BYBFILE.BYB_UNEARNED_AMT_1_2_3 =
                   :QBYB-UNEARNED-AMT-1-2-3,
              BYBFILE.BYB_UNEARNED_AMT_1_2_4 =
                   :QBYB-UNEARNED-AMT-1-2-4,
              BYBFILE.BYB_UNEARNED_AMT_1_2_5 =
                   :QBYB-UNEARNED-AMT-1-2-5,
              BYBFILE.BYB_UNEARNED_AMT_1_2_6 =
                   :QBYB-UNEARNED-AMT-1-2-6,
              BYBFILE.BYB_UNEARNED_AMT_1_2_7 =
                   :QBYB-UNEARNED-AMT-1-2-7,
              BYBFILE.BYB_UNEARNED_AMT_1_2_8 =
                   :QBYB-UNEARNED-AMT-1-2-8,
              BYBFILE.BYB_UNEARNED_AMT_1_2_9 =
                   :QBYB-UNEARNED-AMT-1-2-9,
              BYBFILE.BYB_UNEARNED_AMT_1_2_10 =
                   :QBYB-UNEARNED-AMT-1-2-10,
              BYBFILE.BYB_UNEARNED_AMT_2_1_1 =
                   :QBYB-UNEARNED-AMT-2-1-1,
              BYBFILE.BYB_UNEARNED_AMT_2_1_2 =
                   :QBYB-UNEARNED-AMT-2-1-2,
              BYBFILE.BYB_UNEARNED_AMT_2_1_3 =
                   :QBYB-UNEARNED-AMT-2-1-3,
              BYBFILE.BYB_UNEARNED_AMT_2_1_4 =
                   :QBYB-UNEARNED-AMT-2-1-4,
              BYBFILE.BYB_UNEARNED_AMT_2_1_5 =
                   :QBYB-UNEARNED-AMT-2-1-5,
              BYBFILE.BYB_UNEARNED_AMT_2_1_6 =
                   :QBYB-UNEARNED-AMT-2-1-6,
              BYBFILE.BYB_UNEARNED_AMT_2_1_7 =
                   :QBYB-UNEARNED-AMT-2-1-7,
              BYBFILE.BYB_UNEARNED_AMT_2_1_8 =
                   :QBYB-UNEARNED-AMT-2-1-8,
              BYBFILE.BYB_UNEARNED_AMT_2_1_9 =
                   :QBYB-UNEARNED-AMT-2-1-9,
              BYBFILE.BYB_UNEARNED_AMT_2_1_10 =
                   :QBYB-UNEARNED-AMT-2-1-10,
              BYBFILE.BYB_UNEARNED_AMT_2_2_1 =
                   :QBYB-UNEARNED-AMT-2-2-1,
              BYBFILE.BYB_UNEARNED_AMT_2_2_2 =
                   :QBYB-UNEARNED-AMT-2-2-2,
              BYBFILE.BYB_UNEARNED_AMT_2_2_3 =
                   :QBYB-UNEARNED-AMT-2-2-3,
              BYBFILE.BYB_UNEARNED_AMT_2_2_4 =
                   :QBYB-UNEARNED-AMT-2-2-4,
              BYBFILE.BYB_UNEARNED_AMT_2_2_5 =
                   :QBYB-UNEARNED-AMT-2-2-5,
              BYBFILE.BYB_UNEARNED_AMT_2_2_6 =
                   :QBYB-UNEARNED-AMT-2-2-6,
              BYBFILE.BYB_UNEARNED_AMT_2_2_7 =
                   :QBYB-UNEARNED-AMT-2-2-7,
              BYBFILE.BYB_UNEARNED_AMT_2_2_8 =
                   :QBYB-UNEARNED-AMT-2-2-8,
              BYBFILE.BYB_UNEARNED_AMT_2_2_9 =
                   :QBYB-UNEARNED-AMT-2-2-9,
              BYBFILE.BYB_UNEARNED_AMT_2_2_10 =
                   :QBYB-UNEARNED-AMT-2-2-10,
              BYBFILE.BYB_UNEARNED_AMT_3_1_1 =
                   :QBYB-UNEARNED-AMT-3-1-1,
              BYBFILE.BYB_UNEARNED_AMT_3_1_2 =
                   :QBYB-UNEARNED-AMT-3-1-2,
              BYBFILE.BYB_UNEARNED_AMT_3_1_3 =
                   :QBYB-UNEARNED-AMT-3-1-3,
              BYBFILE.BYB_UNEARNED_AMT_3_1_4 =
                   :QBYB-UNEARNED-AMT-3-1-4,
              BYBFILE.BYB_UNEARNED_AMT_3_1_5 =
                   :QBYB-UNEARNED-AMT-3-1-5,
              BYBFILE.BYB_UNEARNED_AMT_3_1_6 =
                   :QBYB-UNEARNED-AMT-3-1-6,
              BYBFILE.BYB_UNEARNED_AMT_3_1_7 =
                   :QBYB-UNEARNED-AMT-3-1-7,
              BYBFILE.BYB_UNEARNED_AMT_3_1_8 =
                   :QBYB-UNEARNED-AMT-3-1-8,
              BYBFILE.BYB_UNEARNED_AMT_3_1_9 =
                   :QBYB-UNEARNED-AMT-3-1-9,
              BYBFILE.BYB_UNEARNED_AMT_3_1_10 =
                   :QBYB-UNEARNED-AMT-3-1-10,
              BYBFILE.BYB_UNEARNED_AMT_3_2_1 =
                   :QBYB-UNEARNED-AMT-3-2-1,
              BYBFILE.BYB_UNEARNED_AMT_3_2_2 =
                   :QBYB-UNEARNED-AMT-3-2-2,
              BYBFILE.BYB_UNEARNED_AMT_3_2_3 =
                   :QBYB-UNEARNED-AMT-3-2-3,
              BYBFILE.BYB_UNEARNED_AMT_3_2_4 =
                   :QBYB-UNEARNED-AMT-3-2-4,
              BYBFILE.BYB_UNEARNED_AMT_3_2_5 =
                   :QBYB-UNEARNED-AMT-3-2-5,
              BYBFILE.BYB_UNEARNED_AMT_3_2_6 =
                   :QBYB-UNEARNED-AMT-3-2-6,
              BYBFILE.BYB_UNEARNED_AMT_3_2_7 =
                   :QBYB-UNEARNED-AMT-3-2-7,
              BYBFILE.BYB_UNEARNED_AMT_3_2_8 =
                   :QBYB-UNEARNED-AMT-3-2-8,
              BYBFILE.BYB_UNEARNED_AMT_3_2_9 =
                   :QBYB-UNEARNED-AMT-3-2-9,
              BYBFILE.BYB_UNEARNED_AMT_3_2_10 =
                   :QBYB-UNEARNED-AMT-3-2-10
             WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO        = :QBYB-BRNO
               AND BYBFILE.BYB_CLASS       = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BYB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      * USE C1 CONNECT
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BYB1-FILE.



      * COPY "LIBGL/GLGT1IN.CPY".
      * COPYMEMBER: LIBGL/GLGT1IN
      ******************************************************************
      * REV:
      * JKC 2023-1019 COMMENTED OUT START-GT1-FILE-GREATER; ONE PROGRAM
      *          (GL/DELDUP) ONLY USES IT; DUE TO THE COMPLEX GT1-KEY I
      *          DO NOT BELIEVE IT IS PROPERLY SETUP FOR IT TO WORK
      *          CORRECTLY; AFTER DISCUSSING WITH KEVIN MULLEN, DELDUP
      *          WAS ONLY USED ONCE BUT WOULD BE NICE TO HAVE IN CASE
      *          NEEDED FOR AN EMERGENCY; RON ZAMISKA WAS ABLE TO
      *          ACCOMPLISH WHAT DELDUP DOES WITHIN A SQL CODE; WE
      *          ARCHIVED DELDUP FROM USE SO THE USE OF
      *          START-GT1-FILE-GREATER IS NO LONGER NEEDED.
      ******************************************************************
       LOAD-GT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT1-FILE. *> EMBEDDED - GTFILE
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
              MOVE 9                    TO QGT1-WEND-SOURCE

              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
                                           QGT1-WEND-DATE3

              MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
              MOVE ALL "9"              TO QGT1-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT1-WEND-ACCTNO
              MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
              MOVE GT-LINE              TO QGT1-WBEG-LINE
              MOVE ALL "9"              TO QGT1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-SOURCE = 9 )
                 MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
              ELSE
                 COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1
              END-IF

              MOVE GT-DATE2             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

              IF ( GT-BRANCH = 9999 )
                 MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1
              END-IF

              IF ( GT-ACCTNO = 99999999 )
                 MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE
                                               AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE (( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= :QGT1-WBEG-LINE )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WSBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WSBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WSBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WSBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2 
                                          >= :QGT1-WSBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WSBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= '1900-01-01' 
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 ))
              AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
              AND    GTFILE.GT_DATE2      <= :QGT1-WEND-DATE2 
              AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
              AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
              AND    GTFILE.GT_DATE3      <= :QGT1-WEND-DATE3 
              AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
              AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )

            ORDER BY GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3
                          QGT1-WSBEG-DATE2
                          QGT1-WSBEG-DATE3
                          QGT1-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE
                          QGT1-WSBEG-SOURCE
                          QGT1-WSBEG-BRANCH
                          QGT1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.

      *-----------------------------------------------------------------
      * START-GT1-FILE-GREATER.
      *     PERFORM START-IT.
      *     MOVE GT-PATH-SQL TO E-FILE.
      *     MOVE GT1-KEY TO E-KEYX.

      *     IF ( GT1-CURSOR-GR-STAT-GOOD )
      *        PERFORM CLOSE-GT1-FILE.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      **  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      **  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      **  ONE TO THE WHERE SCAN BEG (WSBEG).
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      *     IF ( QGT1-WEND-LINE = ZEROES    ) OR
      *        ( QGT1-WEND-LINE NOT NUMERIC )
      *        MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
      *        MOVE 9                    TO QGT1-WEND-SOURCE

      *        MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
      *        MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

      *        MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
      *                                     QGT1-WEND-DATE3

      *        MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
      *        MOVE ALL "9"              TO QGT1-WEND-BRANCH
      *        MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
      *        MOVE ALL "9"              TO QGT1-WEND-ACCTNO
      *        MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
      *        MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
      *        MOVE GT-LINE              TO QGT1-WBEG-LINE
      *        MOVE ALL "9"              TO QGT1-WEND-LINE
      **-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *        IF ( GT-SOURCE = 9 )
      *           MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
      *        ELSE
      *           COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1
      *        END-IF

      *        MOVE GT-DATE2             TO NDTE-DATE
      *        MOVE 1                    TO NDTE-HOLD
      *        PERFORM INCREMENT-DAYS
      *        MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

      *        IF ( GT-BRANCH = 9999 )
      *           MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
      *        ELSE
      *           COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1
      *        END-IF

      *        IF ( GT-ACCTNO = 99999999 )
      *           MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
      *        ELSE
      *           COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1
      *        END-IF

      *        MOVE GT-DATE3             TO NDTE-DATE
      *        MOVE 1                    TO NDTE-HOLD
      *        PERFORM INCREMENT-DAYS
      *        MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

      *        MOVE 01                   TO SQL-TEXT-SIZE
      *        MOVE GT-DRCRFG            TO SQL-TEXT
      *        PERFORM SQL-TEXT-INCREMENT
      *        MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      *     EXEC SQL
      *      DECLARE GT1_CURSOR_GR CURSOR FOR
      *      SELECT GTFILE.GT_FLAG,
      *             GTFILE.GT_REF,
      *             CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
      *             GTFILE.GT_SOURCE,
      *             CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
      *             GTFILE.GT_BRANCH,
      *             GTFILE.GT_ACCTNO,
      *             CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
      *             GTFILE.GT_DRCRFG,
      *             GTFILE.GT_LINE,
      *             CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
      *             GTFILE.GT_AMT,
      *             GTFILE.GT_EXPL,
      *             GTFILE.GT_PERIOD,
      *             GTFILE.GT_SMITH_SENT,
      *             GTFILE.GT_POST_USERID,
      *             CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
      *             GTFILE.GT_POST_TIME
      *      FROM DBO.GTFILE
      *      WHERE (( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3      >  :QGT1-WBEG-DATE3 
      *        AND    GTFILE.GT_DRCRFG     >  :QGT1-WBEG-DRCRFG
      *        AND    GTFILE.GT_LINE       >  :QGT1-WBEG-LINE )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3      >  :QGT1-WBEG-DATE3 
      *        AND    GTFILE.GT_DRCRFG     >  :QGT1-WSBEG-DRCRFG
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3 
      *                                    >  :QGT1-WSBEG-DATE3
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WSBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01' 
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WSBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  0
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01' 
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WSBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  0
      *        AND    GTFILE.GT_ACCTNO     >  0
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01' 
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WSBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  '1900-01-01'
      *        AND    GTFILE.GT_BRANCH     >  0
      *        AND    GTFILE.GT_ACCTNO     >  0
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01'
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 ))
      *        AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
      *        AND    GTFILE.GT_DATE2      <= :QGT1-WEND-DATE2
      *        AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
      *        AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
      *        AND    GTFILE.GT_DATE3      <= :QGT1-WEND-DATE3 
      *        AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
      *        AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )
      *      ORDER BY GTFILE.GT_SOURCE,
      *               GTFILE.GT_DATE2,
      *               GTFILE.GT_BRANCH,
      *               GTFILE.GT_ACCTNO,
      *               GTFILE.GT_DATE3,
      *               GTFILE.GT_DRCRFG,
      *               GTFILE.GT_LINE
      *     END-EXEC.

      *     EXEC SQL
      *         OPEN GT1_CURSOR_GR
      *     END-EXEC.

      *     PERFORM SQL-IO-VALIDATION.

      *     MOVE SPACES TO QGT1-WBEG-DRCRFG
      *                    QGT1-WEND-DRCRFG
      *                    QGT1-WBEG-DATE2
      *                    QGT1-WEND-DATE2
      *                    QGT1-WBEG-DATE3
      *                    QGT1-WEND-DATE3
      *                    QGT1-WSBEG-DATE2
      *                    QGT1-WSBEG-DATE3
      *                    QGT1-WSBEG-DRCRFG.

      *     MOVE ZEROES TO QGT1-WBEG-BRANCH
      *                    QGT1-WEND-BRANCH
      *                    QGT1-WBEG-ACCTNO
      *                    QGT1-WEND-ACCTNO
      *                    QGT1-WBEG-LINE
      *                    QGT1-WEND-LINE
      *                    QGT1-WBEG-SOURCE
      *                    QGT1-WEND-SOURCE
      *                    QGT1-WSBEG-SOURCE
      *                    QGT1-WSBEG-BRANCH
      *                    QGT1-WSBEG-ACCTNO.

      *     MOVE STAT---GOOD TO GT1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       START-GT1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.

           IF ( GT1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO 
      *  !!SUBTRACT!! ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE 0                    TO QGT1-WBEG-SOURCE
              MOVE GT-SOURCE            TO QGT1-WEND-SOURCE

              MOVE EXT-JULIAN-BEG       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
                                           QGT1-WBEG-DATE3
              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE3

              MOVE ALL "0"              TO QGT1-WBEG-BRANCH
              MOVE GT-BRANCH            TO QGT1-WEND-BRANCH
              MOVE ALL "0"              TO QGT1-WBEG-ACCTNO
              MOVE GT-ACCTNO            TO QGT1-WEND-ACCTNO
              MOVE ALL "A"              TO QGT1-WBEG-DRCRFG
              MOVE GT-DRCRFG            TO QGT1-WEND-DRCRFG
              MOVE ALL "0"              TO QGT1-WBEG-LINE
              MOVE GT-LINE              TO QGT1-WEND-LINE.
      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-SOURCE = 0 )
                 MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
              ELSE
                 COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE - 1
              END-IF

              MOVE GT-DATE2             TO NDTE-DATE
              MOVE -1                   TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

              IF ( GT-BRANCH = 0 )
                 MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH - 1
              END-IF

              IF ( GT-ACCTNO = 0 )
                 MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO - 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE -1                   TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-DECREMENT
              MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT1_CURSOR_NG CURSOR FOR
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE (( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= :QGT1-WBEG-LINE )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WSBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WSBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WSBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WSBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2 
                                          >= :QGT1-WSBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WSBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= '1900-01-01'
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 ))
              AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
              AND    GTFILE.GT_DATE2      <= :QGT1-WEND-DATE2 
              AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
              AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
              AND    GTFILE.GT_DATE3      <= :QGT1-WEND-DATE3 
              AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
              AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )
           ORDER BY GTFILE.GT_SOURCE DESC,
                    GTFILE.GT_DATE2 DESC,
                    GTFILE.GT_BRANCH DESC,
                    GTFILE.GT_ACCTNO DESC,
                    GTFILE.GT_DATE3 DESC,
                    GTFILE.GT_DRCRFG DESC,
                    GTFILE.GT_LINE DESC
           END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3
                          QGT1-WSBEG-DATE2
                          QGT1-WSBEG-DATE3
                          QGT1-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE
                          QGT1-WSBEG-SOURCE
                          QGT1-WSBEG-BRANCH
                          QGT1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-GT1-FILE. *> EMBEDDED - GTFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-SOURCE           TO QGT-SOURCE.
           MOVE GT-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.
           MOVE GT-BRANCH           TO QGT-BRANCH.
           MOVE GT-ACCTNO           TO QGT-ACCTNO.
           MOVE GT-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.
           MOVE GT-DRCRFG           TO QGT-DRCRFG.
           MOVE GT-LINE             TO QGT-LINE.

           EXEC SQL
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT. *> EMBEDDED - GTFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.


      *-----------------------------------------------------------------
       READ-GT1-FILE-PREVIOUS. *> EMBEDDED - GTFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR_NG INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GT1-FILE. *> EMBEDDED - GTFILE

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

      * USE UPDATE_CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. 
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.GTFILE
             (GTFILE.GT_FLAG,
              GTFILE.GT_REF,
              GTFILE.GT_DATE1,
              GTFILE.GT_SOURCE,
              GTFILE.GT_DATE2,
              GTFILE.GT_BRANCH,
              GTFILE.GT_ACCTNO,
              GTFILE.GT_DATE3,
              GTFILE.GT_DRCRFG,
              GTFILE.GT_LINE,
              GTFILE.GT_LTOUCH_DATE,
              GTFILE.GT_AMT,
              GTFILE.GT_EXPL,
              GTFILE.GT_PERIOD,
              GTFILE.GT_SMITH_SENT,
              GTFILE.GT_POST_USERID,
              GTFILE.GT_POST_DATE,
              GTFILE.GT_POST_TIME)
            VALUES
             (:QGT-FLAG,
              :QGT-REF,
              :QGT-DATE1,
              :QGT-SOURCE,
              :QGT-DATE2,
              :QGT-BRANCH,
              :QGT-ACCTNO,
              :QGT-DATE3,
              :QGT-DRCRFG,
              :QGT-LINE,
              :QGT-LTOUCH-DATE,
              :QGT-AMT,
              :QGT-EXPL,
              :QGT-PERIOD,
              :QGT-SMITH-SENT,
              :QGT-POST-USERID,
              :QGT-POST-DATE,
              :QGT-POST-TIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GT1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      * USE C1 CONNECT
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-GT1-FILE. *> EMBEDDED - GTFILE

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            UPDATE DBO.GTFILE
             SET
              GTFILE.GT_FLAG = :QGT-FLAG,
              GTFILE.GT_REF = :QGT-REF,
              GTFILE.GT_DATE1 = :QGT-DATE1,
              GTFILE.GT_SOURCE = :QGT-SOURCE,
              GTFILE.GT_DATE2 = :QGT-DATE2,
              GTFILE.GT_BRANCH = :QGT-BRANCH,
              GTFILE.GT_ACCTNO = :QGT-ACCTNO,
              GTFILE.GT_DATE3 = :QGT-DATE3,
              GTFILE.GT_DRCRFG = :QGT-DRCRFG,
              GTFILE.GT_LINE = :QGT-LINE,
              GTFILE.GT_LTOUCH_DATE = :QGT-LTOUCH-DATE,
              GTFILE.GT_AMT = :QGT-AMT,
              GTFILE.GT_EXPL = :QGT-EXPL,
              GTFILE.GT_PERIOD = :QGT-PERIOD,
              GTFILE.GT_SMITH_SENT = :QGT-SMITH-SENT,
              GTFILE.GT_POST_USERID = :QGT-POST-USERID,
              GTFILE.GT_POST_DATE = :QGT-POST-DATE,
              GTFILE.GT_POST_TIME = :QGT-POST-TIME
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GT1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GT1-FILE. *> EMBEDDED - GTFILE

           PERFORM DELETE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GT1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.

           PERFORM CLOSE-IT.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT.

           IF ( GT1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-NG-STAT.

      *    IF ( GT1-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE GT1_CURSOR_GR
      *       END-EXEC
      *       MOVE STAT---BAD TO GT1-CURSOR-GR-STAT.




       COPY "LIBGL/GLGI1RN.CPY".
      *-----------------------------------------------------------------
       OPEN-WK1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-LOCKED
             GO TO OPEN-WK1-FILE-OUTPUT.
           IF IO-ALREADY-OPEN
             CLOSE WK-FILE
             GO TO OPEN-WK1-FILE-OUTPUT.
           UNLOCK WK-FILE.
      *-----------------------------------------------------------------
       OPEN-WK1-FILE.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-LOCKED
             GO TO OPEN-WK1-FILE.
           IF IO-ALREADY-OPEN
             CLOSE WK-FILE
             GO TO OPEN-WK1-FILE.
           UNLOCK WK-FILE.
      *-----------------------------------------------------------------
       READ-WK1-FILE.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-LOCKED
             GO TO READ-WK1-FILE.
      *-----------------------------------------------------------------
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-LOCKED
             GO TO READ-WK1-FILE-NEXT.
      *-----------------------------------------------------------------
       START-WK1-FILE.
           PERFORM START-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK1-KEY.
           UNLOCK WK-FILE.
      *-----------------------------------------------------------------
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-LOCKED
             GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
      *-----------------------------------------------------------------
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-LOCKED
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
      *-----------------------------------------------------------------
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
      *-----------------------------------------------------------------
       OPEN-WK2-FILE.
           PERFORM OPEN-IT.
           MOVE WK2-IFN TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-LOCKED
             GO TO OPEN-WK2-FILE.
           IF IO-ALREADY-OPEN
             CLOSE WK-FILE
             GO TO OPEN-WK2-FILE.
           UNLOCK WK-FILE.
      *-----------------------------------------------------------------
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK2-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-LOCKED
             GO TO READ-WK2-FILE-NEXT.
      *-----------------------------------------------------------------
       START-WK2-FILE.
           PERFORM START-IT.
           MOVE WK2-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK2-KEY.
           UNLOCK WK-FILE.
      *-----------------------------------------------------------------
       CLOSE-WK2-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.

      *-----------------------------------------------------------------
       OPEN-WT1-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE WT1-IFN TO E-FILE.
           OPEN OUTPUT WT-FILE.

           IF IO-LOCKED
             GO TO OPEN-WT1-FILE-OUTPUT.

           IF IO-ALREADY-OPEN
             CLOSE WT-FILE
             GO TO OPEN-WT1-FILE-OUTPUT.

           UNLOCK WT-FILE.

      *-----------------------------------------------------------------
       OPEN-WT1-FILE.

           PERFORM OPEN-IT.
           MOVE WT1-IFN TO E-FILE.
           OPEN I-O WT-FILE.

           IF IO-LOCKED
             GO TO OPEN-WT1-FILE.

           IF IO-ALREADY-OPEN
             CLOSE WT-FILE
             GO TO OPEN-WT1-FILE.

           UNLOCK WT-FILE.

      *-----------------------------------------------------------------
       READ-WT1-FILE.

           PERFORM READ-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           READ WT-FILE.

           IF IO-LOCKED
             GO TO READ-WT1-FILE.

      *-----------------------------------------------------------------
       READ-WT1-FILE-NEXT.

           PERFORM READ-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           READ WT-FILE NEXT.

           IF IO-LOCKED
             GO TO READ-WT1-FILE-NEXT.

      *-----------------------------------------------------------------
       START-WT1-FILE.

           PERFORM START-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           START WT-FILE KEY NOT < WT1-KEY.
           UNLOCK WT-FILE.

      *-----------------------------------------------------------------
       WRITE-WT1-FILE.

           PERFORM WRITE-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           WRITE WT-REC.

           IF IO-LOCKED
             GO TO WRITE-WT1-FILE.

           UNLOCK WT-FILE.

      *-----------------------------------------------------------------
       REWRITE-WT1-FILE.

           PERFORM REWRITE-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           REWRITE WT-REC.

           IF IO-LOCKED
             GO TO REWRITE-WT1-FILE.

           UNLOCK WT-FILE.

      *-----------------------------------------------------------------
       CLOSE-WT1-FILE.

           PERFORM CLOSE-IT.
           CLOSE WT-FILE.

      *-----------------------------------------------------------------
       OPEN-WT2-FILE.

           PERFORM OPEN-IT.
           MOVE WT2-IFN TO E-FILE.
           OPEN I-O WT-FILE.

           IF IO-LOCKED
             GO TO OPEN-WT2-FILE.

           IF IO-ALREADY-OPEN
             CLOSE WT-FILE
             GO TO OPEN-WT2-FILE.

           UNLOCK WT-FILE.

      *-----------------------------------------------------------------
       READ-WT2-FILE-NEXT.

           PERFORM READ-IT.
           MOVE WT2-IFN TO E-FILE.
           MOVE WT2-KEY TO E-KEYX.
           READ WT-FILE NEXT.

           IF IO-LOCKED
             GO TO READ-WT2-FILE-NEXT.

      *-----------------------------------------------------------------
       START-WT2-FILE.

           PERFORM START-IT.
           MOVE WT2-IFN TO E-FILE.
           MOVE WT2-KEY TO E-KEYX.
           START WT-FILE KEY NOT < WT2-KEY.
           UNLOCK WT-FILE.

      *-----------------------------------------------------------------
       CLOSE-WT2-FILE.

           PERFORM CLOSE-IT.
           CLOSE WT-FILE.

      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

      *    TIMER ADDED HERE - REMOVE TO NEXT &&&

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      *    TIMER - DELETE TO HERE.


           PERFORM CLOSE-FILES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * REMOVE WK AND WT FILES FROM /USR/TMP/???????
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE WK-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WT-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( WKSP-PATH NOT = SPACES )
              MOVE WKSP-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPEMMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ACRERN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
