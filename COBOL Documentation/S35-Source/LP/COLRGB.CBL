       IDENTIFICATION DIVISION.
       PROGRAM-ID.   COLRGB.
       DATE-WRITTEN. 012301.
      ******************************************************************
      *
      *                      (LS)
      *    DESCRIPTION:  GLOBAL COLLECTOR JOURNAL SUMMARY
      *
      * NOTE: TOOK FROM COLRPT, BASICALLY SAME REPORT BUT ONLY FOR
      *       GLOBAL (CROSS BRANCH) QUEUING, WHICH ARE TAGGED BY A
      *       NON-ZERO VALUE IN CJ-POSTING-BR.  REPORT JUST TAKES
      *       CJ RECORDS & SORTS BY COLLECTOR (REGARDLESS OF CJ-BRNO
      *       WHICH IS THE BRANCH OF THE ACCOUNT)
      *
      * REV:
      * BLV 010829 CLEANED UP SOME PROBLEMS FOUND BY JIM R. - 1 BUG
      *            CAUSED A LOOP; TOTALS WERE MESSED UP; REMOVED
      *            BRANCH TOTALS OPTION & PRINT
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF CDSCAN; NOT USING CDSCAN WITHIN PROGRAM.
      *
      *-----------------------------------------------------------------
      * NOTE:   THE WK1-FILE IS USED TO KEEP TRACK IF A CERTAIN ACCOUNT
      *     WITHIN A CERTAIN BRANCH HAS OR HAS NOT BEEN WORKED.  THE
      *     WAY THE WK1-FILE IS SETUP, ONLY ONE COLLECTOR WILL BE
      *     CREDITED TO 'NUMBER ACCTS ATTEMPTED' ON 'COLLECTOR-TOTALS';
      *     'NUMBER OF ACTIVITIES' AND 'TIME SPENT' WILL BE CREDITED FOR
      *     EACH VALID CJ-RECORD.  ON 'BRANCH-TOTALS' AND GRAND-TOTALS',
      *     'NUMBER ACCTS ATTEMPTED' WILL TRULLY REFLECT THE RIGHT
      *     NUMBER OF ACCOUNTS ATTEMPTED.
      *         THE WK2-FILE IS USED TO KEEP A COLLECTOR/BRANCH/GRAND
      *     TOTALS BASED ON THE RESULT CODES (ONLY THOSE RESULT CODES
      *     THAT HAVE A "Y" IN CD-RE-WORKED-ACCOUNT).  THE 'TOTAL' LINE
      *     FOR COLLECTOR/BRANCH/GRAND TOTALS FOR THE RESULT CODES IS
      *     STORED IN A WORKING STORAGE TABLE (TT-RESULT-TOTALS).  THE
      *     WK2-FILE UPDATES COLLECTOR, BRANCH AND GRAND TOTALS FOR
      *     EVERY CJ RECORD READ.
      *         THE TOTALS FOR THE DELINQUENCY ARE STORED IN A WORKING
      *     STORAGE TABLE (TT-DELINQUENCY-TOTALS).
      *         'CDFILE-WORKED-ACCT' IS USED TO STORE THE VALUE OF A
      *     RESULT CODE'S WORKED ACCOUNT FLAG (CD-RE-WORKED-ACCOUNT).
      *         'ACCT-ALREADY-WORKED' IS USED TO KNOW IF A CERTAIN
      *     ACCOUNT HAS BEEN PREVIOUSLY WORKED BY ANY COLLECTOR WITHIN
      *     A CERTAIN BRANCH.
      *         'WS-CHANGE-OF-RESULT' IS USED TO KNOW HOW TO PROCESS THE
      *     CURRENT CJ-RECORD'S ACCOUNT NUMBER (CJ-ACCTNO) IN ADDING TO
      *     THE COLLECTOR/BRANCH/GRAND TOTALS.
      *
      * 160602 BAH CHAGED CJ-DATE FROM JULIAN TO YYYYMMDD
      * BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181127 GLOBAL LMFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 20190924 REMOVED ACCESS-CALL CJFILE
      * BAH 20220803 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT WK3-FILE ASSIGN WK3-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK3-KEY
                  FILE STATUS FILE-STAT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K - F I L E - 1
      *
      ******************************************************************
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK1-REC.
           03  WK1-KEY.
               05  WK-BRNO       PIC 9(4).
               05  WK-ACCTNO     PIC 9(8).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WK-RESULT-CODE IS USED TO HOLD THE CURRENT RESULT CODE.
      *    WK-WORKED-ACCOUNT IF THIS ACCOUNT NUMBER HAS ALREADY
      *    BEEN WORKED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  WK-RESULT-CODE    PIC X(3).
           03  WK-WORKED-ACCOUNT PIC X(1).

      ******************************************************************
      *
      *    W O R K - F I L E - 2
      *
      *-----------------------------------------------------------------
      *    COLLECTOR TOTALS:
      *         WK2-BRNO        = ZEROS
      *         WK2-COLLID      = HAS THE COLLECTOR ID
      *         WK2-RESULT-CODE = HAS A RESULT CODE
      *    GRAND TOTALS:
      *         WK2-BRNO        = 9999
      *         WK2-COLLID      = SPACES
      *         WK2-RESULT-CODE = HAS A RESULT CODE
      *-----------------------------------------------------------------
      *    WK2-RES-NUMBER OCCURS 7 TIMES.
      *       1 = POT/CURRENT  4 = 90        7 = TOTALS
      *       2 = 30           5 = 120
      *       3 = 60           6 = 150+
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRNO         PIC 9(4).
               05  WK2-COLLID       PIC X(3).
               05  WK2-RESULT-CODE  PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  WK2-RES-TOTAL-DEL    OCCURS 7.
               05  WK2-RES-NUMBER   PIC 9(6).
               05  WK2-RES-AMOUNT   PIC 9(6)V99.
      ******************************************************************
      *
      *    W O R K - F I L E - 3
      *
      ******************************************************************
       FD  WK3-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK3-REC.
           03  WK3-KEY.
               05  WK3-COLLID       PIC XXX.
               05  WK3-DATE         PIC 9(8)     COMP-3.
               05  WK3-START-TIME   PIC 9(4)     COMP-3.
               05  WK3-SEQNO        PIC 99       COMP-3.
               05  WK3-BRNO         PIC 9(4)     COMP-3.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/COLRGB.CBL S34 05/26/21-08:32:30 >".
      ******************************************************************
      *
      *   F I L E   P A T H S
      *
      ******************************************************************
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".

       01  WK1-PATH             PIC X(35).
       01  WK2-PATH             PIC X(35).
       01  WK3-PATH             PIC X(35).

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   M I S C - W O R K E R S
      *
      ******************************************************************
       01  MSEL-LIST            PIC X(5)            VALUE "MSEL.".
       01  SUB                  PIC 9(2)            COMP-3.
       01  SUB1                 PIC 9(2)            COMP-3.
       01  CDFILE-WORKED-ACCT   PIC X(1)            VALUE SPACES.
       01  ACCT-ALREADY-WORKED  PIC X(1)            VALUE SPACES.

       01  RECORDS-FOUND        PIC X               VALUE " ".
           88  NO-RECORDS-FOUND                     VALUE "N".

      *-----------------------------------------------------------------
      * COMPARE TIME WORKERS
      *-----------------------------------------------------------------
       01  TIME-WORKER          PIC 99/99.
       01  TIME-WORKERX REDEFINES TIME-WORKER.
           03  TW-HH            PIC 9(02).
           03  FILLER           PIC X(01).
           03  TW-MM            PIC 9(02).

       01  TIME1                PIC 9(04).
       01  TIME1X REDEFINES TIME1.
           03  TIME1-HH         PIC 9(02).
           03  TIME1-MM         PIC 9(02).

       01  TIME2                PIC 9(04).
       01  TIME2X REDEFINES TIME2.
           03  TIME2-HH         PIC 9(02).
           03  TIME2-MM         PIC 9(02).

       01  VERIFY-TIME-ERR      PIC 9(01)    VALUE 0.
       01  CMPTIM-STAT          PIC X(01).
           88  TIME1-IS-EQ      VALUE "=".
           88  TIME1-IS-GT      VALUE ">".
           88  TIME1-IS-LT      VALUE "<".
           88  TIME1-IS-GE      VALUE "=" ">".
           88  TIME1-IS-LE      VALUE "=" "<".
           88  TIME1-IS-NE      VALUE "<" ">".

       01  CMPTIM-HH-STAT       PIC X(01).
           88  HH-EQ            VALUE "=".
           88  HH-GT            VALUE ">".
           88  HH-LT            VALUE "<".

       01  CMPTIM-MM-STAT       PIC X(01).
           88  MM-EQ            VALUE "=".
           88  MM-GT            VALUE ">".
           88  MM-LT            VALUE "<".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      *-----------------------------------------------------------------
       01  HOLD-FIELDS.
           03  HOLD-CODE-TYPE                PIC X(7)  VALUE SPACES.
           03  HOLD-CNDESC                   PIC X(25) VALUE SPACES.
           03  HOLD-COLLID                   PIC X(3)  VALUE SPACES.
           03  HOLD-BRNO                     PIC 9(4)  VALUE ZEROES.
           03  HOLD-RESULT-CODE              PIC X(3)  VALUE SPACES.
           03  HOLD-WK1-KEY.
               05  HOLD-WK-BRNO              PIC 9(4).
               05  HOLD-WK-ACCTNO            PIC 9(8).
           03  HOLD-WK2-KEY.
               05  HOLD-WK2-BRNO             PIC 9(4).
               05  HOLD-WK2-COLLID           PIC X(3).
               05  HOLD-WK2-RESULT-CODE      PIC X(3).

      *-----------------------------------------------------------------
       01  TEST-MEMO1.
           03  FILLER                        PIC X(6).
           03  TEST-COLON                    PIC X.
           03  FILLER                        PIC X(3).
           03  TEST-AM-PM                    PIC XX.
           03  FILLER                        PIC X(18).

      *-----------------------------------------------------------------
       COPY "LIBSP/COLRPT_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *    L E V E L S
      *
      ******************************************************************
       01  LEV-DELINQUENCY      PIC 9(2)  COMP-3.
       01  LEV1                 PIC 9(2)  COMP-3.
       01  LEV2                 PIC 9(2)  COMP-3.
       01  LEV2-MAX             PIC 9(2)  COMP-3    VALUE  7.
       01  LEV-1                PIC 9(2)  COMP-3.
       01  LEV-2                PIC 9(2)  COMP-3.
       01  LEV-3                PIC 9(2)  COMP-3.
       01  LEV-2-MAX            PIC 9(2)  COMP-3    VALUE  7.

      ******************************************************************
      *
      *    W S - F I E L D S
      *
      ******************************************************************
      * MEMO DATE SUBTRACTED FROM 9'S
       01  WS-CONVERT-DATE               PIC 9(8)     VALUE ZERO.

      *-----------------------------------------------------------------
      *    WS-CHANGE-OF-RESULT IS TO FLAG THE TOTAL COMPUTATION SECTIONS
      *       "N" - NO CHANGE.
      *       "W" - NEW ACCOUNT.
      *       "Y" - THERE IS A CHANGE.
      *-----------------------------------------------------------------
       01  WS-CHANGE-OF-RESULT           PIC X(1)     VALUE SPACES.

       01  WS-HEAD-BRANCH            PIC X(25)    VALUE
           "B R A N C H - T O T A L S".
       01  WS-HEAD-COLLECTOR         PIC X(31)    VALUE
           "C O L L E C T O R - T O T A L S".
       01  WS-HEAD-CORPORATE         PIC X(31)    VALUE
           "C O R P O R A T E - T O T A L S".

       01  WS-HOLD-BW-NAME               PIC X(10)    VALUE SPACES.
       01  WS-INVALID-LM-FG              PIC X(1)     VALUE SPACES.
       01  WS-NEXT-CJ                    PIC X(1)     VALUE SPACES.
       01  WS-TIME-HH-MM                 PIC 9(4)     VALUE ZEROES.
       01  WS-TIME-HH-MM-RDF   REDEFINES WS-TIME-HH-MM.
           03  WS-TIME-HH                PIC 9(2).
           03  WS-TIME-MM                PIC 9(2).
       01  WS-COMPUTE-MIN                PIC 9(4)     VALUE ZEROES.
       01  WS-TOTAL-HEAD1-FG             PIC X(1)     VALUE "N".
       01  WS-TOTAL-HEAD2-FG             PIC X(1)     VALUE "N".

      ******************************************************************
      *
      *    T O T A L S - D E L I N Q U E N C Y  &  R E S U L T S
      *
      ******************************************************************
      *    TT-DEL-ATTEMPTED(  LEV1, LEV2 )
      *    TT-DEL-ACTIVITIES( LEV1, LEV2 )
      *    TT-DEL-TIME-SPENT( LEV1, LEV2 )
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LEV1 --->  1 = COLLECTOR TOTALS
      *               2 = CORPORATE TOTALS
      *
      *    LEV-2 --->  1 = POTENTIAL + CURRENT
      *                2 = 30
      *                3 = 60
      *                4 = 90
      *                5 = 120
      *                6 = 150+
      *                7 = TOTAL
      *-----------------------------------------------------------------
       01  TT-DELINQUENCY-TOTALS.
           03  TT-DEL-TOTAL-TYPE          OCCURS 2.
               05  TT-DEL-TOTAL-DELINQ    OCCURS 7.
                   07  TT-DEL-ATTEMPTED   PIC 9(9)    VALUE ZEROES.
                   07  TT-DEL-ACTIVITIES  PIC 9(9)    VALUE ZEROES.
                   07  TT-DEL-TIME-SPENT  PIC 9(9)    VALUE ZEROES.

      *-----------------------------------------------------------------
      *        TT-RES-NUMBER(LEV-1,LEV-2)
      *        TT-RES-AMOUNT(LEV-1,LEV-2)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LEV-1 --->  1 = COLLECTOR TOTALS
      *                2 = CORPORATE TOTALS
      *
      *    LEV-2 --->  1 = POT + CUR     4 = 90     7 = TOTAL
      *                2 = 30            5 = 120
      *                3 = 60            6 = 150+
      *-----------------------------------------------------------------
       01  TT-RESULT-TOTALS.
           03  TT-RES-TOTAL-TYPE         OCCURS 3.
               05  TT-RES-TOTAL-DEL      OCCURS 7.
                   07  TT-RES-NUMBER     PIC 9(6)     VALUE ZEROES.
                   07  TT-RES-AMOUNT     PIC 9(6)V99  VALUE ZEROES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-COLLID          PIC X(3).
           03  END-COLLID          PIC X(3).
           03  BEG-DATE            PIC 9(8).
           03  END-DATE            PIC 9(8).
           03  PRINT-DETAIL        PIC X(1).
           03  PRINT-MEMO          PIC X(1).
           03  PRINT-COLLID-TOTAL  PIC X(1).
           03  PRINT-CORP-TOTAL    PIC X(1).
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).
           03  LOCAL-PRINT-FG      PIC X(1).
           03  BEG-START-TIME-ALP.
               05  BEG-START-TIME  PIC 9(04).
           03  END-START-TIME-ALP.
               05  END-START-TIME  PIC 9(04).

       01  VERSION-CONTROL         PIC X VALUE "E".

       01  SV-BEG-BR               PIC 9999  VALUE 9999.
       01  SV-END-BR               PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG           PIC X     VALUE SPACES.
       01  SV-ENT-GROUP            PIC X(4)  VALUE SPACES.

      *****************************************************************
      *
      *  P R I N T  B U F F E R S
      *
      *****************************************************************
       01  HEAD1.
           03  H1-DATE       PIC X(8)    VALUE " ".
           03  FILLER        PIC XX      VALUE " ".
           03  H1-TIME       PIC X(8)    VALUE " ".
           03  FILLER        PIC X(28)   VALUE " ".
           03  H1-NAME       PIC X(40)   VALUE " ".
           03  FILLER        PIC X(37)   VALUE " ".
           03  FILLER        PIC X(5)    VALUE "PAGE".
           03  H1-PAGE-NO    PIC ZZZ9.

      *---------------------------------------------------------------
       01  HEAD2.
           03  FILLER        PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID     PIC X(10).
           03  FILLER        PIC X(33)   VALUE " ".
           03  FILLER        PIC X(24)   VALUE
               "GLOBAL COLLECTOR JOURNAL".
           03  FILLER        PIC X(54)   VALUE " ".

      *---------------------------------------------------------------
       01  HEAD3.
           03  FILLER        PIC X(11)   VALUE "COLLECTOR: ".
           03  H3-COLLID     PIC X(3).
           03  FILLER        PIC X(3)     VALUE " ".
           03  H3-CNDESC     PIC X(25)    VALUE " ".
           03  FILLER        PIC X(90).

      *---------------------------------------------------------------
       01  HEAD4.
           03  FILLER        PIC X(40)   VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "BORROWER".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "ATV".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "RST".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "PROM-1".
           03  FILLER        PIC X(13)   VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "PROM-2".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "FRM".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "ACTN".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "NEW".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "CON".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "REC".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "CURRENT".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "PAST-DUE".
           03  FILLER        PIC X(11)   VALUE SPACES.

      *---------------------------------------------------------------
       01  HEAD5.
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "DATE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "BEG-TIME".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "END-TIME".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE " BRNO ".
           03  FILLER        PIC X(6)    VALUE "ACCTNO".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "NAME".
           03  FILLER        PIC X(5)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "COD".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "COD".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "DATE".
           03  FILLER        PIC X(4)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "PROM-AMT".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "DATE".
           03  FILLER        PIC X(4)    VALUE SPACES.
           03  FILLER        PIC X(2)    VALUE "NO".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "CODE".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "COL".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "STA".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "STA".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "BALANCE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "BALANCE".
           03  FILLER        PIC X(11)   VALUE SPACES.

      *---------------------------------------------------------------
       01  HEAD6.
           03  FILLER        PIC X(15)    VALUE SPACES.
           03  FILLER        PIC X(12)   VALUE "NUMBER ACCTS".
           03  FILLER        PIC X(12)   VALUE SPACES.
           03  FILLER        PIC X(9)    VALUE "NUMBER OF".
           03  FILLER        PIC X(13)   VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "TIME".
           03  FILLER        PIC X(67)   VALUE SPACES.

      *---------------------------------------------------------------
       01  HEAD7.
           03  FILLER        PIC X(18)   VALUE SPACES.
           03  FILLER        PIC X(9)    VALUE "ATTEMPTED".
           03  FILLER        PIC X(11)   VALUE SPACES.
           03  FILLER        PIC X(10)   VALUE "ACTIVITIES".
           03  FILLER        PIC X(12)   VALUE SPACES.
           03  FILLER        PIC X(5)    VALUE "SPENT".
           03  FILLER        PIC X(67)   VALUE SPACES.

      *---------------------------------------------------------------
       01  HEAD8.
           03  FILLER        PIC X(9)    VALUE SPACES.
           03  FILLER        PIC X(19)   VALUE "-- CUR, POT, MIS --".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(19)   VALUE "--------30---------".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(18)   VALUE "--------60--------".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(15)   VALUE "------90-------".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(13)   VALUE "-----120-----".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(13)   VALUE "----150+-----".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(20)   VALUE "-------TOTAL--------".

      *---------------------------------------------------------------
       01  HEAD9.
           03  H9-CODE-TYPE  PIC X(8)    VALUE SPACES.
           03  FILLER        PIC X(2)    VALUE SPACES.
      *- - - POT
           03  FILLER        PIC X(6)    VALUE "NUMBER".
           03  FILLER        PIC X(6)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(2)    VALUE SPACES.
      *- - - 30
           03  FILLER        PIC X(6)    VALUE "NUMBER".
           03  FILLER        PIC X(6)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(1)    VALUE SPACES.
      *- - - 60
           03  FILLER        PIC X(6)    VALUE "NUMBER".
           03  FILLER        PIC X(6)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(1)    VALUE SPACES.
      *- - - 90
           03  FILLER        PIC X(6)    VALUE "NUMBER".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(2)    VALUE SPACES.
      *- - - 120
           03  FILLER        PIC X(4)    VALUE "NUM.".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(2)    VALUE SPACES.
      *- - - 150
           03  FILLER        PIC X(4)    VALUE "NUM.".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(2)    VALUE SPACES.
      *- - - TOTAL
           03  FILLER        PIC X(6)    VALUE "NUMBER".
           03  FILLER        PIC X(7)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".

      *****************************************************************
      *
      *    D E T A I L  L I N E
      *
      *****************************************************************
       01  DETAIL-LINE      PIC X(132)   VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-DATE              PIC 99/99/99.
           03  FILLER               PIC X(1).
           03  D1-START-TIME        PIC X(8).
           03  FILLER               PIC X(1).
           03  D1-END-TIME          PIC X(8).
           03  FILLER               PIC X(2).
           03  D1-BRNO              PIC Z(3)9.
           03  FILLER               PIC X(1).
           03  D1-ACCTNO            PIC Z(5)9.
           03  FILLER               PIC X(1).
           03  D1-BW-NAME           PIC X(10).
           03  FILLER               PIC X(1).
           03  D1-ACTIVITY-CD       PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-RESULT-CD         PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-PROMISE-DATE1     PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER               PIC X(1).
           03  D1-PROMISE-AMT       PIC ZZZZZ9.99 BLANK WHEN ZERO.
           03  FILLER               PIC X(1).
           03  D1-PROMISE-DATE2     PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER               PIC X(1).
           03  D1-FORM-NO           PIC ZZ9.
           03  FILLER               PIC X(1).
           03  D1-ACTION-CD         PIC X(4).
           03  FILLER               PIC X(1).
           03  D1-NEW-COLLID        PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-CON-STAT          PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-REC-STAT          PIC X(3).
           03  FILLER               PIC X(1).
           03  D1-CURRENT-BAL       PIC ZZZZZ9.99.
           03  FILLER               PIC X(1).
           03  D1-PASTDUE-BAL       PIC ZZZZZ9.99.
           03  FILLER               PIC X(6).

      *----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(6).
           03  D2-LM-MEMO1          PIC X(30).
           03  D2-LM-MEMO2          PIC X(30).
           03  FILLER               PIC X(6).
           03  D2-LM-MEMO3          PIC X(30).
           03  D2-LM-MEMO4          PIC X(30).

      *------------------------------------------------RESULT TOTALS----
       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(2).
           03  D3-RESULT-CODE       PIC X(5).
           03  FILLER               PIC X(2).
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  POT   -  -  -
           03  D3-POT-NO            PIC ZZZZZZ9.
           03  D3-POT-AMT           PIC ZZZZ,ZZZ,ZZ9.
           03  FILLER               PIC X(1).
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  30 -  -  -  -
           03  D3-30-NO             PIC ZZZZZZ9.
           03  D3-30-AMT            PIC ZZZZ,ZZZ,ZZ9.
           03  FILLER               PIC X(1).
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  60 -  -  -  -
           03  D3-60-NO             PIC ZZZZZ9.
           03  D3-60-AMT            PIC ZZZZ,ZZZ,ZZ9.
           03  FILLER               PIC X(1).
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  90 -  -  -  -
           03  D3-90-NO             PIC ZZZZ9.
           03  D3-90-AMT            PIC ZZ,ZZZ,ZZ9.
           03  FILLER               PIC X(1).
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  120   -  -  -
           03  D3-120-NO            PIC ZZZZ9.
           03  FILLER               PIC X(1).
           03  D3-120-AMT           PIC ZZZ,ZZ9.
           03  FILLER               PIC X(1).
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  150+  -  -  -
           03  D3-150-NO            PIC ZZZZ9.
           03  FILLER               PIC X(1).
           03  D3-150-AMT           PIC ZZZ,ZZ9.
           03  FILLER               PIC X(1).
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  TOTAL -  -  -
           03  D3-TOTAL-NO          PIC ZZZZZZ9.
           03  D3-TOT-AMT           PIC ZZZZZ,ZZZ,ZZ9.
           03  D3-TOT-AMTX          REDEFINES D3-TOT-AMT
                                    PIC X(13).

      *-------------------------------------------DELINQUENCY TOTALS----
       01  DETAIL4 REDEFINES DETAIL-LINE.
           03  D4-DELINQ-TYPE       PIC X(11).
           03  FILLER               PIC X(5).
           03  D4-NUM-ATTEMPT       PIC ZZZ,ZZZ,ZZ9.
           03  FILLER               PIC X(10).
           03  D4-NUM-ACTIVITY      PIC ZZZ,ZZZ,ZZ9.
           03  FILLER               PIC X(10).
           03  D4-TIME-SPENT-HH     PIC ZZZ9.
           03  D4-TIME-SPENT-COLON  PIC X(1).
           03  D4-TIME-SPENT-MM     PIC 9(2).
           03  FILLER               PIC X(68).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(7)9.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/COLRGB_DEF.CPY".
       COPY "LIBLP/COLRGB_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/COLRGB_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK2-FILE WK3-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK1-FILE WK2-FILE WK3-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "COLRGB" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM CJ-SORT-ROUTINE.

           PERFORM CLOSE-WK3-FILE.
           PERFORM OPEN-WK3-FILE.

       NEXT-SORT-REC.
           PERFORM READ-WK3-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-OF-REPORT.

           IF WK3-BRNO = HOLD-BRNO
              GO TO SKIP-READ-BRANCH.

           MOVE WK3-BRNO TO BR-NO HOLD-BRNO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *-----------------------------------------------------------------
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CJ-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-CJ1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           IF PRINT-MEMO = "Y"
              PERFORM OPEN-LM1-FILE.

       SKIP-READ-BRANCH.
           MOVE WK3-BRNO       TO CJ-BRNO.
           MOVE WK3-COLLID     TO CJ-COLLID.
           MOVE WK3-DATE       TO CJ-DATE.
           MOVE WK3-START-TIME TO CJ-START-TIME.
           MOVE WK3-SEQNO      TO CJ-SEQNO.
           PERFORM READ-CJ1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           PERFORM CHECK-SET-PARAMETERS.

      *-----------------------------------------------------------------
       PROCESS-RECORD.

           PERFORM CHECK-COLLECTOR.
           PERFORM VERIFY-WORK-RECORD.
           MOVE 1  TO LEV1 LEV-1.
           PERFORM COMPUTE-TOTALS-TABLE.
           PERFORM COMPUTE-TOTALS-WK2.

           IF ( FIRST-TIME = "Y" )
              MOVE "N"          TO FIRST-TIME
              MOVE "Y"          TO RECORDS-FOUND
              MOVE 1            TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           IF ( PRINT-DETAIL = "Y" )
              PERFORM PRINT-DETAIL-LINE.

           GO TO NEXT-SORT-REC.

      *=================================================================
      *
      *    END OF THE BRANCH AND/OR GROUP.
      *
      *=================================================================
       END-OF-REPORT.

      *- - - - - - - - - - - - - - - - - - - - PRINTING COLLECTOR TOTALS
           IF ( PRINT-COLLID-TOTAL = "Y" )
              MOVE 1 TO LEV1 LEV-1
              PERFORM PRINT-TOTALS.

      *- - - - - - - - - - - - - - - - - - -  COMPUTING CORPORATE TOTALS
           MOVE 2 TO LEV1 LEV-1.
           PERFORM COMPUTE-TOTALS-TABLE.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

      *- - - - - - - - - - - - - - - - - - - - PRINTING CORPORATE TOTALS
           IF ( PRINT-CORP-TOTAL = "Y" )
              MOVE 2 TO LEV1 LEV-1
              PERFORM PRINT-TOTALS.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
      *
      *   SORT ROUTINE, READ CJ, SORT BY GLOBAL COLLECTOR
      *
      ******************************************************************
       CJ-SORT-ROUTINE SECTION.

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
              GO TO END-SORT-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (BR-NO > END-BRANCH)
              GO TO END-SORT-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CJ-PATH-OVERRIDE.

           PERFORM OPEN-WK1-FILE.
           PERFORM OPEN-WK2-FILE.
           PERFORM OPEN-WK3-FILE.
           PERFORM OPEN-CJ1-FILE.

           MOVE BR-NO             TO CJ-BRNO
                                     QCJ1-WBEG-BRNO
                                     QCJ1-WEND-BRNO.
           MOVE BEG-COLLID        TO CJ-COLLID
                                     QCJ1-WBEG-COLLID.
           MOVE END-COLLID        TO QCJ1-WEND-COLLID.
           MOVE BEG-DATE          TO CJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WBEG-DATE.
           MOVE END-DATE          TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WEND-DATE.

           MOVE BEG-START-TIME    TO CJ-START-TIME
                                     QCJ1-WBEG-START-TIME.
           MOVE END-START-TIME    TO QCJ1-WEND-START-TIME.
           MOVE 0                 TO CJ-SEQNO
                                     QCJ1-WBEG-SEQNO.
           MOVE ALL "9"           TO QCJ1-WEND-SEQNO.

      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
           MOVE 03                TO SQL-TEXT-SIZE.
           MOVE CJ-COLLID         TO SQL-TEXT.
           PERFORM SQL-TEXT-INCREMENT.
           MOVE SQL-TEXT          TO QCJ1-WSBEG-COLLID.

           MOVE CJ-DATE           TO NDTE-DATE.
           MOVE 1                 TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WSBEG-DATE.

           COMPUTE QCJ1-WSBEG-START-TIME = CJ-START-TIME + 1.
           IF ( CJ-SEQNO = 99 )
              MOVE CJ-SEQNO       TO QCJ1-WSBEG-SEQNO
           ELSE
              COMPUTE QCJ1-WSBEG-SEQNO = CJ-SEQNO + 1.

           PERFORM START-CJ1-FILE.

      *=================================================================
      *
      *    PROCESSING CJ-FILE FOR THE CURRENT BRANCH.
      *
      *=================================================================
       NEXT-CJ-REPORT.

           PERFORM READ-CJ1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO END-OF-GROUP.

           IF (CJ-BRNO NOT = CJ-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF CJ-POSTING-BR = 0 OR 2020
              GO TO NEXT-CJ-REPORT.

           PERFORM CHECK-SET-PARAMETERS.

           IF ( WS-NEXT-CJ = "X" )
              GO TO END-OF-GROUP.

           IF ( WS-NEXT-CJ = "Y" )
              GO TO NEXT-CJ-REPORT.

      *-----------------------------------------------------------------
       PROCESS-VALID-RECORD.

           MOVE CJ-BRNO       TO WK3-BRNO.
           MOVE CJ-COLLID     TO WK3-COLLID.
           MOVE CJ-DATE       TO WK3-DATE.
           MOVE CJ-START-TIME TO WK3-START-TIME.
           MOVE CJ-SEQNO      TO WK3-SEQNO.
           PERFORM WRITE-WK3-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO NEXT-CJ-REPORT.

      *=================================================================
      *
      *    END OF THE BRANCH AND/OR GROUP.
      *
      *=================================================================
       END-OF-GROUP.
           PERFORM CLOSE-CJ1-FILE.
           GO TO NEXT-GROUP-LOC.
       END-SORT-ROUTINE.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE COLRGB-QUEUE-POS TO Q-POS.
           MOVE COLRGB-COPIES-POS TO C-POS.
           MOVE COLRGB-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE "N" TO RECORDS-FOUND.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK3-PATH.
           PERFORM OPEN-WK3-FILE-OUTPUT.
           PERFORM CLOSE-WK3-FILE.

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE TO H1-DATE.
           MOVE EXT-CONAME-USER-ID TO H2-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP TO ENT-GROUP
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              MOVE "Y" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "COLLECTOR JOURNAL ACTIVITY IN PROGRESS.." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      * ACCOUNT FOR PREVIOUSLY SET UP BATCHS
           IF BEG-START-TIME-ALP = SPACES AND
                              END-START-TIME-ALP = SPACES
              MOVE 0000 TO BEG-START-TIME
              MOVE 2400 TO END-START-TIME.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO PRINT-CORP-TOTAL-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - DEFAULT TO USER BRANCH  - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( GROUP-FLAG NOT = "Y" )
      *       MOVE "SNNN040BR1." TO VDU
      *       MOVE 0 TO BEG-BRANCH VDUNUM0
      *       PERFORM SCREENIO
      *       MOVE "SNNN040BR2." TO VDU
      *       MOVE 0 TO END-BRANCH VDUNUM0
      *------>PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-BRANCH.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BEGIN-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENDING-BRANCH.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BEGIN-BRANCH.

      *-----------------------------------------------------------------
       BEGIN-COLLECTOR.
           MOVE LEGEND-COLL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A030BEG-COLL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-COLLECTORS.
           IF VDUSTATUS = "1U" GO TO ENDING-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-COLLECTOR.
           MOVE VDUNUM0 TO BEG-COLLID.

      *-----------------------------------------------------------------
       ENDING-COLLECTOR.
           MOVE LEGEND-COLL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A030END-COLL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-COLLECTOR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-COLLECTOR.
           MOVE VDUNUM0 TO END-COLLID.

           IF BEG-COLLID > END-COLLID
              MOVE "INVALID COLLECTOR ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-COLLECTOR.

           GO TO BEGIN-DATE.

      *-----------------------------------------------------------------
       ALL-COLLECTORS.
           MOVE "S  A030BEG-COLL." TO VDU.
           MOVE SPACES             TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030END-COLL." TO VDU.
           MOVE BABY-ZS            TO END-COLLID VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-DATE.
           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO ENDING-COLLECTOR.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO BEGIN-TIME.

           MOVE LEGEND-DATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO ENDING-COLLECTOR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-DATE.

           IF ( VDUNUM0 = 0 )
              GO TO BEGIN-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO ENDING-COLLECTOR.

           MOVE LEGEND-DATE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENDING-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

      *-----------------------------------------------------------------
       BEGIN-TIME.
           MOVE LEGEND-TIME-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNA080BEG-TIME." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY  GO TO EXIT-PARM.
           IF F3-KEY  GO TO ALL-TIMES.
           IF UP-KEY  GO TO ENDING-DATE.
           IF F7-KEY  GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO BEGIN-TIME.

           MOVE VDUBUF TO TIME-WORKER.
           MOVE TW-HH TO TIME1-HH.
           MOVE TW-MM TO TIME1-MM.
           MOVE TIME1 TO BEG-START-TIME.
           PERFORM VERIFY-TIME.
           IF VERIFY-TIME-ERR = 1
              MOVE "PLEASE ENTER TIME IN HHMM FORMAT" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-TIME.

      *-----------------------------------------------------------------
       ENDING-TIME.
           MOVE LEGEND-TIME-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNA080END-TIME." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY  GO TO EXIT-PARM.
           IF UP-KEY  GO TO BEGIN-TIME.
           IF F7-KEY  GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO ENDING-TIME.

           MOVE VDUBUF TO TIME-WORKER.
           MOVE TW-HH TO TIME1-HH.
           MOVE TW-MM TO TIME1-MM.
           MOVE TIME1 TO END-START-TIME.
           PERFORM VERIFY-TIME.
           IF VERIFY-TIME-ERR = 1
              MOVE "PLEASE ENTER TIME IN HHMM FORMAT" TO MESS
              PERFORM SEND-MESS
              GO TO ENDING-TIME.

           MOVE BEG-START-TIME TO TIME1.
           MOVE END-START-TIME TO TIME2.
           PERFORM CMPTIME.
           IF TIME1-IS-GT
              MOVE "INVALID RANGE: BEGINNING TIME < ENDING TIME" TO MESS
              PERFORM SEND-MESS
              GO TO ENDING-TIME.

           GO TO PRINT-DETAIL-YN.

       ALL-TIMES.
           MOVE "S  A080BEG-TIME." TO VDU.
           MOVE 0000 TO TIME-WORKER BEG-START-TIME.
           INSPECT TIME-WORKER REPLACING ALL "/" BY ":".
           MOVE TIME-WORKER TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A080END-TIME." TO VDU.
           MOVE 2400 TO TIME-WORKER END-START-TIME.
           INSPECT TIME-WORKER REPLACING ALL "/" BY ":".
           MOVE TIME-WORKER TO VDUBUF.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       PRINT-DETAIL-YN.
           MOVE LEGEND-PRINT-DETAIL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRT-DET-ANAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDING-TIME.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT "1D" GO TO PRINT-DETAIL-YN.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO PRINT-DETAIL-YN.

           MOVE VDUBUF TO PRINT-DETAIL.

      *-----------------------------------------------------------------
       PRINT-MEMO-YN.
           IF ( VDUSTATUS = "1U" ) AND ( PRINT-DETAIL = "N" )
              GO TO PRINT-DETAIL-YN
           ELSE
           IF ( PRINT-DETAIL = "N" )
              MOVE "S  A010PRT-DET-MEMO." TO VDU
              MOVE "N" TO VDUBUF PRINT-MEMO
              PERFORM SCREENIO
              GO TO PRINT-COLLID-TOTAL-YN.

           MOVE LEGEND-PRINT-MEMO TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRT-DET-MEMO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT "1D" GO TO PRINT-MEMO-YN.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO PRINT-MEMO-YN.

           MOVE VDUBUF TO PRINT-MEMO.

      *-----------------------------------------------------------------
       PRINT-COLLID-TOTAL-YN.
           MOVE LEGEND-PRINT-COLLID-TOTAL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010COLLID-TOTAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-MEMO-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS NOT = "00" AND NOT "1D" )
              GO TO PRINT-COLLID-TOTAL-YN.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO PRINT-COLLID-TOTAL-YN.

           MOVE VDUBUF TO PRINT-COLLID-TOTAL.

      *-----------------------------------------------------------------
       PRINT-CORP-TOTAL-YN.
           MOVE LEGEND-PRINT-CORP-TOTAL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010CORP-TOTAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-COLLID-TOTAL-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS NOT = "00" AND NOT "1D" )
              GO TO PRINT-CORP-TOTAL-YN.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO PRINT-CORP-TOTAL-YN.

           MOVE VDUBUF TO PRINT-CORP-TOTAL.

      *----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A250SPOOL-PROMPT."         TO VDU.
           MOVE "SPOOL TO LOCAL?  (Y/N)  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-CORP-TOTAL-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

      *=================================================================
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

      *-----------------------------------------------------------------
      *    A CHECK TO SEE IF LOCAL-SPOOLING AND CORPORATE TOTALS ARE
      *    BOTH ON.  CORPORATE TOTALS CANNOT PRINT IF THERE IS LOCAL-SP.
      *-----------------------------------------------------------------
       CHECK-LOCAL-TO-CORP-TOTALS.
           IF ( LOCAL-PRINT-FG = "Y" ) AND
              ( PRINT-CORP-TOTAL = "Y" )
              MOVE "S  A010CORP-TOTAL." TO VDU
              MOVE "N" TO VDUBUF PRINT-CORP-TOTAL
              PERFORM SCREENIO
              MOVE LEGEND-NO-CORP-TOTAL TO MESS
              PERFORM SEND-MESS.

      *----------------------------------------------------------------
      *    CHECKING TO SEE IF THERE IS ANY TYPE OF PRINTING.
      *    IF NOT, THEY MUST RE-DO THE SETUP.
      *----------------------------------------------------------------
       CHECK-IF-PRINTING.
           IF ( PRINT-DETAIL       = "N" ) AND
              ( PRINT-MEMO         = "N" ) AND
              ( PRINT-COLLID-TOTAL = "N" ) AND
              ( PRINT-CORP-TOTAL   = "N" )
              MOVE LEGEND-NO-PRINTING TO MESS
              PERFORM SEND-MESS
              GO TO PRINT-DETAIL-YN.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030BEG-COLL." TO VDU.
           MOVE BEG-COLLID         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030END-COLL." TO VDU.
           MOVE END-COLLID         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A050BEG-TIME." TO VDU.
           MOVE BEG-START-TIME     TO TIME-WORKER.
           INSPECT TIME-WORKER     REPLACING ALL "/" BY ":".
           MOVE TIME-WORKER        TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A050END-TIME." TO VDU.
           MOVE END-START-TIME     TO TIME-WORKER.
           INSPECT TIME-WORKER     REPLACING ALL "/" BY ":".
           MOVE TIME-WORKER        TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-DET-ANAL." TO VDU.
           MOVE PRINT-DETAIL           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRT-DET-MEMO." TO VDU.
           MOVE PRINT-MEMO             TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010COLLID-TOTAL." TO VDU.
           MOVE PRINT-COLLID-TOTAL     TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010CORP-TOTAL." TO VDU.
           MOVE PRINT-CORP-TOTAL     TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A250SPOOL-PROMPT."         TO VDU.
           MOVE "SPOOL TO LOCAL?  (Y/N)  :"    TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       CHECK-SET-PARAMETERS SECTION.
      ******************************************************************

           MOVE "N" TO WS-NEXT-CJ.

      *- - - - - - - - - - - - - - - - - -  VERIFY COLLID IN RANGE - - -
           IF ( CJ-COLLID < BEG-COLLID )
              MOVE "Y" TO WS-NEXT-CJ
              GO TO EXIT-CHECK-SET-PARM.

           IF ( CJ-COLLID > END-COLLID )
              MOVE "X" TO WS-NEXT-CJ
              GO TO EXIT-CHECK-SET-PARM.

      *- - - - - - - - - - - - - - - - - - -  VERIFY DATE IN RANGE - - -

      *  IF THE CJ-DATE IS < THAN BEG-DATE, THEN START THE CJ-FILE
      *  TO THE BEG-DATE RECORD OF THE CURRENT COLLID.

           IF ( CJ-DATE < BEG-DATE )
              MOVE "Y"               TO WS-NEXT-CJ
      *       MOVE BEG-DATE          TO CJ-DATE
      *       MOVE BEG-START-TIME    TO CJ-START-TIME
      *       MOVE ZEROES            TO CJ-SEQNO
      *       PERFORM START-CJ1-FILE
              GO TO EXIT-CHECK-SET-PARM.

      *  IF THE CJ-DATE IS > THAN END-DATE, THEN START THE CJ-FILE
      *  TO THE LAST RECORD OF THE CURRENT COLLID.

           IF ( CJ-DATE > END-DATE )
              MOVE "Y"   TO WS-NEXT-CJ
      *       MOVE 99991231 TO CJ-DATE
      *       MOVE 9999     TO CJ-START-TIME
      *       MOVE 99       TO CJ-SEQNO
      *       PERFORM START-CJ1-FILE
              GO TO EXIT-CHECK-SET-PARM.

      *  IF THE CJ-START-TIME IS < THAN BEG-START-TIME, THEN START THE CJ-FILE
      *  AT THE BEG-START-TIME RECORD OF THE CURRENT COLLID/DATE.

           MOVE CJ-START-TIME TO TIME1.
           MOVE BEG-START-TIME TO TIME2.
           PERFORM CMPTIME.
           IF TIME1-IS-LT
              MOVE "Y"   TO WS-NEXT-CJ
      *       MOVE BEG-START-TIME TO CJ-START-TIME
      *       PERFORM START-CJ1-FILE
              GO TO EXIT-CHECK-SET-PARM.

      *  IF THE CJ-START-TIME IS > THAN END-START-TIME, THEN START THE CJ-FILE
      *  AT THE LAST RECORD OF THE CURRENT COLLID/DATE.

           MOVE CJ-START-TIME TO TIME1.
           MOVE END-START-TIME TO TIME2.
           PERFORM CMPTIME.
           IF TIME1-IS-GT
              MOVE "Y"   TO WS-NEXT-CJ
      *       MOVE 9999  TO CJ-START-TIME
      *       MOVE 99    TO CJ-SEQNO
      *       PERFORM START-CJ1-FILE
              GO TO EXIT-CHECK-SET-PARM.

       EXIT-CHECK-SET-PARM.
           EXIT.

      ******************************************************************
      *
      *    C H E C K - C O L L E C T O R
      *
      *-----------------------------------------------------------------
      *  CHECKING TO SEE IF THE COLLECTOR IS THE SAME.  IF NOT THE SAME:
      *       1) PRINT COLLECTOR TOTALS;
      *       2) UPDATE BRANCH TOTALS;
      *       3) CLEAR THE COLLECTOR TOTALS.
      ******************************************************************
       CHECK-COLLECTOR SECTION.

           IF ( CJ-COLLID NOT = HOLD-COLLID )
              IF ( FIRST-TIME NOT = "Y" )
                 IF ( PRINT-COLLID-TOTAL = "Y" )
                    MOVE 1 TO LEV1 LEV-1
                    PERFORM PRINT-TOTALS
                    MOVE 2 TO LEV1 LEV-1
                    PERFORM COMPUTE-TOTALS-TABLE
                 ELSE
                    MOVE 2 TO LEV1 LEV-1
                    PERFORM COMPUTE-TOTALS-TABLE.

           IF ( CJ-COLLID NOT = HOLD-COLLID )
              MOVE CJ-COLLID TO HOLD-COLLID
              MOVE 99        TO LN-COUNT.

       EXIT-CHECK-COLLECTOR.
           EXIT.

      ******************************************************************
       VERIFY-WORK-RECORD SECTION.
      ******************************************************************

           MOVE CJ-BRNO   TO HOLD-WK-BRNO.
           MOVE CJ-ACCTNO TO HOLD-WK-ACCTNO.
           MOVE "N"       TO WS-CHANGE-OF-RESULT.

       CREATE-WK-REC.

           MOVE HOLD-WK1-KEY TO WK1-KEY.
           PERFORM READ-WK1-FILE.
           IF ( IO-FG = 0 )
              GO TO CREATE-WK-UPDATE.

           MOVE SPACES       TO WK1-REC.
           MOVE HOLD-WK1-KEY TO WK1-KEY.

           PERFORM WRITE-WK1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "W" TO WS-CHANGE-OF-RESULT.
           GO TO CREATE-WK-REC.

      *===========================================UPDATING THE RECORD===
       CREATE-WK-UPDATE.

           MOVE WK-WORKED-ACCOUNT TO ACCT-ALREADY-WORKED.

           IF ( WK-WORKED-ACCOUNT = "Y" )
              GO TO EXIT-VERIFY-WORK-RECORD.

           MOVE "RE"         TO CD-TYPE.
           MOVE CJ-RESULT-CD TO CD-CODE HOLD-RESULT-CODE.
           PERFORM GET-CD.
           MOVE CD-RE-WORKED-ACCOUNT TO CDFILE-WORKED-ACCT.

           IF ( WS-CHANGE-OF-RESULT = "W" )
              MOVE CJ-RESULT-CD       TO WK-RESULT-CODE
              MOVE CDFILE-WORKED-ACCT TO WK-WORKED-ACCOUNT
           ELSE
              IF ( WK-WORKED-ACCOUNT = CDFILE-WORKED-ACCT )
                 MOVE "N" TO WS-CHANGE-OF-RESULT
                 GO TO EXIT-VERIFY-WORK-RECORD
              ELSE
                 MOVE "Y"                  TO WS-CHANGE-OF-RESULT
                 MOVE CJ-RESULT-CD         TO WK-RESULT-CODE
                 MOVE CDFILE-WORKED-ACCT   TO WK-WORKED-ACCOUNT.

           PERFORM REWRITE-WK1-FILE.

       EXIT-VERIFY-WORK-RECORD.
           EXIT.

      ******************************************************************
       COMPUTE-TOTALS-TABLE SECTION.
      ******************************************************************

           IF ( LEV-1 NOT = 1 )
              PERFORM UPDATING-TOTALS
              PERFORM CLEARING-TOTALS
              GO TO EXIT-COMPUTE-TOTALS-TABLE.

           MOVE 1 TO LEV2 LEV-2 LEV-3.

      * COMPUTE DELINQUENCY

           PERFORM FIND-DELINQUENCY.
           MOVE LEV-DELINQUENCY TO LEV2.

      * ADDING ONCE TO TT-DEL-ATTEMPTED FOR EACH ACCOUNT WORKED ON.

           IF ( WS-CHANGE-OF-RESULT = "W" )
              ADD  1 TO TT-DEL-ATTEMPTED( LEV1, LEV2     )
                        TT-DEL-ATTEMPTED( LEV1, LEV2-MAX ).

           PERFORM COMPUTE-HOUR-TO-MIN.

           ADD WS-COMPUTE-MIN TO TT-DEL-TIME-SPENT( LEV1, LEV2     )
                                 TT-DEL-TIME-SPENT( LEV1, LEV2-MAX ).

           ADD  1             TO TT-DEL-ACTIVITIES( LEV1, LEV2     )
                                 TT-DEL-ACTIVITIES( LEV1, LEV2-MAX ).

      * COMPUTE RESULT

      * IF THERE IS NO CHANGE TO THE RESULT TOTALS; EXIT THE SECTION.

           IF ( WS-CHANGE-OF-RESULT = "N" )
              GO TO EXIT-COMPUTE-TOTALS-TABLE.

           PERFORM FIND-DELINQUENCY.
           MOVE LEV-DELINQUENCY TO LEV-2.

      * ADDING RESULT 

      * DON'T RE-ADD TO TOTALS IF THERE WAS CHANGE TO WK-WORKED-ACCOUNT

           IF ( WS-CHANGE-OF-RESULT = "N" )
              GO TO EXIT-COMPUTE-TOTALS-TABLE.

           IF ( CDFILE-WORKED-ACCT NOT = "Y" )
              GO TO EXIT-COMPUTE-TOTALS-TABLE.

           ADD 1 TO TT-RES-NUMBER( LEV-1, LEV-2     )
                    TT-RES-NUMBER( LEV-1, LEV-2-MAX ).

           ADD CJ-CUR-BAL TO TT-RES-AMOUNT(LEV-1, LEV-2     )
                             TT-RES-AMOUNT(LEV-1, LEV-2-MAX ).

       EXIT-COMPUTE-TOTALS-TABLE.
           EXIT.

      ******************************************************************
      *    COMPUTE THE TOTALS FOR THE WK2-FILE.  TOTALS FOR COLLECTOR,
      *    BRANCH AND CORPORATE ARE UPDATE EVERY TIME THROUGH.
      ******************************************************************
       COMPUTE-TOTALS-WK2 SECTION.

      * IF THERE IS NO CHANGE TO THE RESULT TOTALS; EXIT THE SECTION.

           IF ( WS-CHANGE-OF-RESULT = "N" )
              GO TO EXIT-COMPUTE-TOTALS-WK2.

           IF ( CDFILE-WORKED-ACCT NOT = "Y" )
              GO TO EXIT-COMPUTE-TOTALS-WK2.

           PERFORM FIND-DELINQUENCY.
           MOVE LEV-DELINQUENCY TO LEV-2.
           MOVE CJ-COLLID       TO HOLD-COLLID.
           MOVE CJ-RESULT-CD    TO HOLD-RESULT-CODE.

      * ADDING RESULT

           PERFORM UPDATE-RESULT-CODE-WK2
                   VARYING LEV-1 FROM 1 BY 1 UNTIL LEV-1 > 2.

       EXIT-COMPUTE-TOTALS-WK2.
           EXIT.

      ******************************************************************
      *    UPDATING TO BRANCH/GRAND WORKING STORAGE TABLES ONLY!
      ******************************************************************
       UPDATING-TOTALS SECTION.

           MOVE 1 TO LEV2 LEV-2.
           COMPUTE SUB  = LEV-1 - 1.
           COMPUTE SUB1 = LEV1 - 1.

      * SUB IS USED TO REACH THE PREVIOUS LEVEL ( LEV-? )

       UPDATING-TOTALS-DELINQ.

           ADD TT-DEL-ATTEMPTED( SUB,  LEV2 ) TO
               TT-DEL-ATTEMPTED( LEV1, LEV2 ).

           ADD TT-DEL-ACTIVITIES( SUB,  LEV2 ) TO
               TT-DEL-ACTIVITIES( LEV1, LEV2 ).

           ADD TT-DEL-TIME-SPENT( SUB,  LEV2 ) TO
               TT-DEL-TIME-SPENT( LEV1, LEV2 ).

           IF ( LEV2 < LEV2-MAX )
              ADD 1 TO LEV2
              GO TO UPDATING-TOTALS-DELINQ.

      * SUB IS USED TO REACH THE PREVIOUS LEVEL ( LEV-? )
       UPDATING-TOT-RESULT-TABLE.

           ADD TT-RES-NUMBER( SUB,   LEV-2 ) TO
               TT-RES-NUMBER( LEV-1, LEV-2 ).

           ADD TT-RES-AMOUNT( SUB,   LEV-2 ) TO
               TT-RES-AMOUNT( LEV-1, LEV-2 ).

           IF ( LEV-2 < LEV-2-MAX )
              ADD  1 TO LEV-2
              GO TO UPDATING-TOT-RESULT-TABLE.

       EXIT-UPDATING-TOTALS.
           EXIT.

      ******************************************************************
      *    CLEARING TO BRANCH/GRAND WORKING STORAGE TABLES ONLY!
      ******************************************************************
       CLEARING-TOTALS SECTION.
           COMPUTE SUB  = LEV1 - 1.
           COMPUTE SUB1 = LEV-1 - 1.
           MOVE 1 TO LEV2 LEV-2 LEV-3.

      * SUB IS USED TO REACH THE PREVIOUS LEVEL ( LEV-? )

       CLEARING-TOTALS-DELINQ.

           MOVE ZEROES TO TT-DEL-ATTEMPTED( SUB, LEV2 )
                          TT-DEL-ACTIVITIES( SUB, LEV2 )
                          TT-DEL-TIME-SPENT( SUB, LEV2 ).

           IF ( LEV2 < LEV2-MAX )
              ADD  1 TO LEV2
              GO TO CLEARING-TOTALS-DELINQ.

      *-----------------------------------------------------------------
      *    SUB1 IS USED TO REACH THE PREVIOUS LEVEL ( LEV-? )
      *-----------------------------------------------------------------
       CLEARING-TOTALS-RESULT.

           MOVE ZEROES TO TT-RES-NUMBER( SUB1, LEV-2 )
                          TT-RES-AMOUNT( SUB1, LEV-2 ).

           IF ( LEV-2 < LEV-2-MAX )
              ADD  1 TO LEV-2
              GO TO CLEARING-TOTALS-RESULT.

       EXIT-CLEARING-TOTALS.
           EXIT.

      ******************************************************************
       UPDATE-RESULT-CODE-WK2 SECTION.
      ******************************************************************

           PERFORM OPEN-WK2-FILE.
           PERFORM SETUP-WK2-FILE-SEARCH.

      *-------------------------------------CHECKING IF REC. EXISTS-----
       CREATE-WK2-REC.

           MOVE HOLD-WK2-KEY TO WK2-KEY.
           PERFORM READ-WK2-FILE.
           IF ( IO-FG = 0 )
              GO TO UPDATE-WK2-REC.

           MOVE SPACES       TO WK2-REC.
           MOVE HOLD-WK2-KEY TO WK2-KEY.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
               MOVE 0 TO WK2-RES-NUMBER(SUB) WK2-RES-AMOUNT(SUB)
           END-PERFORM.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO CREATE-WK2-REC.

      *-----------------------------------------------------------------
       UPDATE-WK2-REC.

           ADD 1          TO WK2-RES-NUMBER( LEV-2     )
                             WK2-RES-NUMBER( LEV-2-MAX ).

           ADD CJ-CUR-BAL TO WK2-RES-AMOUNT( LEV-2     )
                             WK2-RES-AMOUNT( LEV-2-MAX ).

           PERFORM REWRITE-WK2-FILE.

       EXIT-UPDATE-RESULT-CODE-WK2.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-LINE SECTION.
      ******************************************************************

           MOVE CJ-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D1-DATE.

           MOVE CJ-START-TIME    TO H-AND-M.
           PERFORM FORMAT-TIME.
           MOVE TIME-EDIT        TO D1-START-TIME.

           MOVE CJ-END-TIME      TO H-AND-M.
           PERFORM FORMAT-TIME.
           MOVE TIME-EDIT        TO D1-END-TIME.

           MOVE CJ-BRNO          TO D1-BRNO.
           MOVE CJ-ACCTNO        TO D1-ACCTNO.

           PERFORM GET-BW-NAME.
           MOVE WS-HOLD-BW-NAME  TO D1-BW-NAME.

           MOVE CJ-ACTIVITY-CD   TO D1-ACTIVITY-CD.
           MOVE CJ-RESULT-CD     TO D1-RESULT-CD.
           MOVE CJ-PROMISE-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D1-PROMISE-DATE1.
           MOVE CJ-PROMISE-AMT   TO D1-PROMISE-AMT.
           MOVE CJ-PROMISE-DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D1-PROMISE-DATE2.
           MOVE CJ-FORM-NO       TO D1-FORM-NO.
           MOVE CJ-ACTION-CD     TO D1-ACTION-CD.
           MOVE CJ-NEW-COLLID    TO D1-NEW-COLLID.
           MOVE CJ-CON-STAT      TO D1-CON-STAT.
           MOVE CJ-REC-STAT      TO D1-REC-STAT.
           MOVE CJ-CUR-BAL       TO D1-CURRENT-BAL.
           MOVE CJ-PAST-DUE      TO D1-PASTDUE-BAL.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( PRINT-MEMO = "N" )
              GO TO EXIT-PRINT-DETAIL-LINE.

      * NEED WS-CONVERT-DATE TO COMPARE LM-DATE TO DURING READ LOOP
           COMPUTE WS-CONVERT-DATE = 99999999 - CJ-DATE.

           MOVE LM-PATH-OWNBR   TO LM-BRNO
                                   QLM1-WBEG-BRNO
                                   QLM1-WEND-BRNO.
           MOVE CJ-ACCTNO       TO LM-ACCTNO
                                   QLM1-WBEG-ACCTNO
                                   QLM1-WEND-ACCTNO.
           MOVE WS-CONVERT-DATE TO LM-DATE
                                   QLM1-WBEG-DATE-REV
                                   QLM1-WEND-DATE-REV.
           MOVE CJ-BEG-LM-SEQ   TO LM-SEQ
                                   QLM1-WBEG-SEQ.
           MOVE ALL "9"         TO QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

      *------------------------------------------------- L I N E - 2 ---
       PRINTING-LINE-2.
           PERFORM READ-LM1-FILE-NEXT.

           PERFORM CHECK-LM-RECORD.

           IF ( WS-INVALID-LM-FG = "Y" )
              PERFORM CLOSE-LM1-FILE
              GO TO EXIT-PRINT-DETAIL-LINE
           ELSE
           IF ( WS-INVALID-LM-FG = "S" )
              GO TO PRINTING-LINE-2
           ELSE
              MOVE LM-MEMO1 TO D2-LM-MEMO1
              MOVE LM-MEMO2 TO D2-LM-MEMO2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       READ-NEXT-LM.
           PERFORM READ-LM1-FILE-NEXT.

           PERFORM CHECK-LM-RECORD.

           IF ( WS-INVALID-LM-FG = "S" )
              GO TO READ-NEXT-LM.
           IF ( WS-INVALID-LM-FG = "Y" )
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              PERFORM CLOSE-LM1-FILE
              GO TO EXIT-PRINT-DETAIL-LINE
           ELSE
              MOVE LM-MEMO1 TO D2-LM-MEMO3
              MOVE LM-MEMO2 TO D2-LM-MEMO4.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINTING-LINE-2.

       EXIT-PRINT-DETAIL-LINE.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************

           IF ( LEV-1 = 1 )
              MOVE "C" TO WS-TOTAL-HEAD1-FG
           ELSE
           IF ( LEV-1 = 2 )
              MOVE "B" TO WS-TOTAL-HEAD1-FG
           ELSE
              MOVE "G" TO WS-TOTAL-HEAD1-FG.

      * SETUP FOR DELINQUENCY PRINT --- ALL FROM 'TT-DEL-' TABLE.

           MOVE "D"       TO WS-TOTAL-HEAD2-FG.
           MOVE 1         TO LEV2.
           MOVE 99        TO LN-COUNT.
           MOVE SPACES    TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM TOTALS-HEAD-PAGE.
           MOVE 1         TO LN-FEED.

       PRINT-DELINQ-TOTAL.

           IF ( LEV2 = 1 )
              MOVE "CUR,POT,MIS" TO D4-DELINQ-TYPE
           ELSE
           IF ( LEV2 = 2 )
              MOVE "         30" TO D4-DELINQ-TYPE
           ELSE
           IF ( LEV2 = 3 )
              MOVE "         60" TO D4-DELINQ-TYPE
           ELSE
           IF ( LEV2 = 4 )
              MOVE "         90" TO D4-DELINQ-TYPE
           ELSE
           IF ( LEV2 = 5 )
              MOVE "        120" TO D4-DELINQ-TYPE
           ELSE
           IF ( LEV2 = 6 )
              MOVE "       150+" TO D4-DELINQ-TYPE
           ELSE
           IF ( LEV2 = 7 )
              MOVE "      TOTAL" TO D4-DELINQ-TYPE.

           MOVE TT-DEL-ATTEMPTED(  LEV1, LEV2 ) TO D4-NUM-ATTEMPT.
           MOVE TT-DEL-ACTIVITIES( LEV1, LEV2 ) TO D4-NUM-ACTIVITY.

           PERFORM COMPUTE-MIN-TO-HOUR.
           MOVE WS-TIME-HH TO D4-TIME-SPENT-HH.
           MOVE ":"        TO D4-TIME-SPENT-COLON.
           MOVE WS-TIME-MM TO D4-TIME-SPENT-MM.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

           IF ( LEV2 < LEV2-MAX )
              ADD 1 TO LEV2
              GO TO PRINT-DELINQ-TOTAL.

      * SETUP FOR RESULT PRINT --- ALL RESULT CODES FROM WK2-FILE
      *                            "TOTAL" FROM 'TT-RES-' TABLE.

           MOVE "RESULT:" TO HOLD-CODE-TYPE.
           MOVE "R"       TO WS-TOTAL-HEAD2-FG.
           PERFORM TOTALS-HEAD-PAGE.
           MOVE 1         TO LEV-2 LEV-3.
           MOVE 1         TO LN-FEED.
           PERFORM OPEN-WK2-FILE.
           IF LEV-1 = 1
              MOVE 0           TO HOLD-WK2-BRNO
              MOVE HOLD-COLLID TO HOLD-WK2-COLLID
              MOVE SPACES      TO HOLD-WK2-RESULT-CODE
           ELSE
           IF LEV-1 = 2
              MOVE 9999        TO HOLD-WK2-BRNO
              MOVE SPACES      TO HOLD-WK2-COLLID
              MOVE SPACES      TO HOLD-WK2-RESULT-CODE.
           MOVE HOLD-WK2-KEY TO WK2-KEY.
           PERFORM START-WK2-FILE.

      * TOTALS FROM THE WK2-FILE.

       PRINT-RESULT-TOT-WK2.

           PERFORM READ-WK2-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO PRINT-RESULT-TOT-TABLE.

      * CHECK COLLECTOR 
           IF LEV-1 = 1
              IF WK2-COLLID NOT = HOLD-COLLID OR WK2-BRNO = 9999
                 GO TO PRINT-RESULT-TOT-TABLE.

      * CHECK GRAND 
           IF LEV-1 = 2
              IF WK2-BRNO NOT = 9999 OR WK2-COLLID NOT = SPACES
                 GO TO PRINT-RESULT-TOT-TABLE.

           MOVE WK2-RESULT-CODE     TO D3-RESULT-CODE.
           MOVE WK2-RES-NUMBER( 1 ) TO D3-POT-NO.
           MOVE WK2-RES-NUMBER( 2 ) TO D3-30-NO.
           MOVE WK2-RES-NUMBER( 3 ) TO D3-60-NO.
           MOVE WK2-RES-NUMBER( 4 ) TO D3-90-NO.
           MOVE WK2-RES-NUMBER( 5 ) TO D3-120-NO.
           MOVE WK2-RES-NUMBER( 6 ) TO D3-150-NO.
           MOVE WK2-RES-NUMBER( 7 ) TO D3-TOTAL-NO.

           COMPUTE D3-POT-AMT ROUNDED = WK2-RES-AMOUNT( 1 ) * 1.
           COMPUTE D3-30-AMT  ROUNDED = WK2-RES-AMOUNT( 2 ) * 1.
           COMPUTE D3-60-AMT  ROUNDED = WK2-RES-AMOUNT( 3 ) * 1.
           COMPUTE D3-90-AMT  ROUNDED = WK2-RES-AMOUNT( 4 ) * 1.
           COMPUTE D3-120-AMT ROUNDED = WK2-RES-AMOUNT( 5 ) * 1.
           COMPUTE D3-150-AMT ROUNDED = WK2-RES-AMOUNT( 6 ) * 1.
           COMPUTE D3-TOT-AMT ROUNDED = WK2-RES-AMOUNT( 7 ) * 1.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           GO TO PRINT-RESULT-TOT-WK2.

      * TOTALS FROM THE 'WS-RES-' TABLE.
       PRINT-RESULT-TOT-TABLE.

           MOVE "TOTAL" TO D3-RESULT-CODE.

           MOVE TT-RES-NUMBER( LEV-1, 1 ) TO D3-POT-NO.
           MOVE TT-RES-NUMBER( LEV-1, 2 ) TO D3-30-NO.
           MOVE TT-RES-NUMBER( LEV-1, 3 ) TO D3-60-NO.
           MOVE TT-RES-NUMBER( LEV-1, 4 ) TO D3-90-NO.
           MOVE TT-RES-NUMBER( LEV-1, 5 ) TO D3-120-NO.
           MOVE TT-RES-NUMBER( LEV-1, 6 ) TO D3-150-NO.
           MOVE TT-RES-NUMBER( LEV-1, 7 ) TO D3-TOTAL-NO.

           COMPUTE D3-POT-AMT ROUNDED = TT-RES-AMOUNT( LEV-1, 1 ) * 1.
           COMPUTE D3-30-AMT  ROUNDED = TT-RES-AMOUNT( LEV-1, 2 ) * 1.
           COMPUTE D3-60-AMT  ROUNDED = TT-RES-AMOUNT( LEV-1, 3 ) * 1.
           COMPUTE D3-90-AMT  ROUNDED = TT-RES-AMOUNT( LEV-1, 4 ) * 1.
           COMPUTE D3-120-AMT ROUNDED = TT-RES-AMOUNT( LEV-1, 5 ) * 1.
           COMPUTE D3-150-AMT ROUNDED = TT-RES-AMOUNT( LEV-1, 6 ) * 1.
           COMPUTE D3-TOT-AMT ROUNDED = TT-RES-AMOUNT( LEV-1, 7 ) * 1.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

       EXIT-PRINT-TOTALS.
           MOVE "N" TO WS-TOTAL-HEAD1-FG.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
      *    WS-TOTAL-HEAD1-FG = "B" ---> BRANCH TOTALS
      *                      = "C" ---> COLLECTOR TOTALS
      *                      = "G" ---> CORPORATE/GRAND TOTALS
      *                      = "N" ---> STANDARD PRINT
      *                      = "R" ---> RANGE LINE PRINT
      ******************************************************************
       HEAD-PAGE SECTION.
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H1-TIME.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WS-TOTAL-HEAD1-FG = "B" OR "G" OR "R" )
              MOVE "***"                      TO H3-COLLID
              MOVE "**** ALL COLLECTORS ****" TO H3-CNDESC
           ELSE
              PERFORM GET-COLLECTOR-ID-DESC
              MOVE HOLD-CNDESC TO H3-CNDESC
              MOVE HOLD-COLLID TO H3-COLLID.

           MOVE HEAD3 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WS-TOTAL-HEAD1-FG = "B" OR "C" OR "G" )
              MOVE 3 TO LN-COUNT
              MOVE 1 TO LN-FEED
              GO TO END-HEAD-PAGE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD5 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "-" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 7 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      *-----------------------------------------------------------------
       END-HEAD-PAGE.
           EXIT.
      ******************************************************************
      *
      *    T O T A L S - H E A D - P A G E
      *
      *-----------------------------------------------------------------
      *    WS-TOTAL-HEAD1-FG = "B" ---> BRANCH TOTALS
      *                      = "C" ---> COLLECTOR TOTALS
      *                      = "G" ---> CORPORATE/GRAND TOTALS
      *                      = "N" ---> STANDARD PRINT
      *                      = "R" ---> RANGE LINE PRINT
      *-----------------------------------------------------------------
      *    WS-TOTAL-HEAD2-FG = "D" ---> DELINQUENCY TOTALS
      *                      = "R" ---> RESULT CODE TOTALS
      ******************************************************************
       TOTALS-HEAD-PAGE SECTION.
           IF ( WS-TOTAL-HEAD2-FG = "R" )
              GO TO TOTALS-HEAD-RESULT.

           IF ( WS-TOTAL-HEAD1-FG = "B" )
              MOVE WS-HEAD-BRANCH TO DETAIL-LINE
           ELSE
           IF ( WS-TOTAL-HEAD1-FG = "C" )
              MOVE WS-HEAD-COLLECTOR TO DETAIL-LINE
           ELSE
           IF ( WS-TOTAL-HEAD1-FG = "G" )
              MOVE WS-HEAD-CORPORATE TO DETAIL-LINE.

           MOVE 2     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD6 TO DETAIL-LINE
           MOVE 2     TO LN-FEED
           PERFORM WRITE-DETAIL-LINE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD7 TO DETAIL-LINE
           MOVE 1     TO LN-FEED
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "-" TO DETAIL-LINE.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO EXIT-TOTALS-HEAD-PAGE.

      *-----------------------------------------------------------------
       TOTALS-HEAD-RESULT.
           MOVE HEAD8          TO DETAIL-LINE
           MOVE 2              TO LN-FEED
           PERFORM WRITE-DETAIL-LINE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HOLD-CODE-TYPE TO H9-CODE-TYPE
           MOVE HEAD9          TO DETAIL-LINE
           MOVE 1              TO LN-FEED
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "-"        TO DETAIL-LINE.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-TOTALS-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE "R"    TO WS-TOTAL-HEAD1-FG.
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP      TO RANGE-SELECTION
              MOVE 4  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH   TO R-NUM
              MOVE 4  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH   TO R-NUM
              MOVE 1  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG COLLECTOR" TO RANGE-LINE.
           MOVE BEG-COLLID      TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END COLLECTOR" TO RANGE-LINE.
           MOVE END-COLLID      TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG DATE"      TO RANGE-LINE.
           MOVE BEG-DATE        TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END DATE"      TO RANGE-LINE.
           MOVE END-DATE        TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG TIME"      TO RANGE-LINE.
           MOVE BEG-START-TIME  TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END TIME"      TO RANGE-LINE.
           MOVE END-START-TIME  TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT DETAIL?" TO RANGE-LINE.
           MOVE PRINT-DETAIL    TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT MEMO ?"  TO RANGE-LINE.
           MOVE PRINT-MEMO      TO R-NUM.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT COLLECTOR TOTALS" TO RANGE-LINE.
           MOVE PRINT-COLLID-TOTAL       TO R-NUM.
           MOVE 1                        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT CORPORATE TOTALS" TO RANGE-LINE.
           MOVE PRINT-CORP-TOTAL         TO R-NUM.
           MOVE 1                        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       VERIFY-TIME SECTION.
      ******************************************************************
           MOVE 0 TO VERIFY-TIME-ERR.
           IF (TIME1-HH > 24 OR TIME1-MM > 59     ) OR
              (TIME1-HH = 24 AND TIME1-MM NOT = 00)
              MOVE 1 TO VERIFY-TIME-ERR.

      ******************************************************************
       CMPTIME SECTION.
      ******************************************************************
           IF TIME1-HH = TIME2-HH  MOVE "=" TO CMPTIM-HH-STAT.
           IF TIME1-HH > TIME2-HH  MOVE ">" TO CMPTIM-HH-STAT.
           IF TIME1-HH < TIME2-HH  MOVE "<" TO CMPTIM-HH-STAT.

           IF TIME1-MM = TIME2-MM  MOVE "=" TO CMPTIM-MM-STAT.
           IF TIME1-MM > TIME2-MM  MOVE ">" TO CMPTIM-MM-STAT.
           IF TIME1-MM < TIME2-MM  MOVE "<" TO CMPTIM-MM-STAT.

      *    -------------------------------------------------------------
      *    TEST TIME1 = TIME2
      *    -------------------------------------------------------------
           IF HH-EQ AND MM-EQ               MOVE "=" TO CMPTIM-STAT.

      *    -------------------------------------------------------------
      *    TEST TIME1 > TIME2
      *    -------------------------------------------------------------
           IF  HH-GT OR (HH-EQ AND MM-GT)   MOVE ">" TO CMPTIM-STAT.

      *    -------------------------------------------------------------
      *    TEST TIME1 < TIME2
      *    -------------------------------------------------------------
           IF  HH-LT OR (HH-EQ AND MM-LT)   MOVE "<" TO CMPTIM-STAT.

       CMPTIME-EXIT.
           EXIT.
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO COLRGB-CONAME.
           MOVE EXT-CONAME-SYSDATE TO COLRGB-SYSDATE.
           DISPLAY COLRGB-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE COLRGB-FORM-FIELDS TO WR-BUF.
           MOVE COLRGB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE COLRGB-HELP--FILE TO HELP-LINK-FILE.
           MOVE COLRGB-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/COLRGB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *---------------------------------------------------------------
       FIND-DELINQUENCY.

           IF ( CJ-CON-STAT NOT NUMERIC )
              IF ( CJ-CON-STAT = "POT" )
                 MOVE 1 TO LEV-DELINQUENCY
              ELSE
              IF ( CJ-CON-STAT = "MIS" )
                 MOVE 1 TO LEV-DELINQUENCY
              ELSE
              IF ( CJ-CON-STAT = "PC" )
                 MOVE 6 TO LEV-DELINQUENCY
              ELSE
              IF ( CJ-CON-STAT = "PL" )
                 MOVE 6 TO LEV-DELINQUENCY
              ELSE
                 MOVE 6 TO LEV-DELINQUENCY.

           IF ( CJ-CON-STAT NUMERIC )
              IF ( CJ-CON-STAT = "000" )
                 MOVE 1 TO LEV-DELINQUENCY
              ELSE
              IF ( CJ-CON-STAT = "030" )
                 MOVE 2 TO LEV-DELINQUENCY
              ELSE
              IF ( CJ-CON-STAT = "060" )
                 MOVE 3 TO LEV-DELINQUENCY
              ELSE
              IF ( CJ-CON-STAT = "090" )
                 MOVE 4 TO LEV-DELINQUENCY
              ELSE
              IF ( CJ-CON-STAT = "120" )
                 MOVE 5 TO LEV-DELINQUENCY
              ELSE
                 MOVE 6 TO LEV-DELINQUENCY.

      *-----------------------------------------------------------------
       CHECK-LM-RECORD.
           IF ( IO-FG NOT = 0 ) OR ( LM-BRNO NOT = LM-PATH-OWNBR )
              MOVE "Y" TO WS-INVALID-LM-FG
           ELSE
           IF ( LM-ACCTNO NOT = CJ-ACCTNO )
              MOVE "Y" TO WS-INVALID-LM-FG
           ELSE
           IF ( LM-DATE NOT = WS-CONVERT-DATE)
              MOVE "Y" TO WS-INVALID-LM-FG
           ELSE
           IF ( LM-SEQ > CJ-END-LM-SEQ )
              MOVE "Y" TO WS-INVALID-LM-FG
           ELSE
              MOVE "N" TO WS-INVALID-LM-FG.

           IF WS-INVALID-LM-FG = "N"
              MOVE LM-MEMO1 TO TEST-MEMO1
              IF TEST-COLON = ":"
                 IF TEST-AM-PM = "AM" OR "PM"
                    MOVE "S" TO WS-INVALID-LM-FG.

      *-----------------------------------------------------------------
      *    FORMAT CLOCK TIME
      *-----------------------------------------------------------------
       FORMAT-TIME.
           IF ( H-AND-M NOT GREATER THAN 1159 )
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.

           IF ( H-AND-M IS NOT LESS THAN 1300 )
              SUBTRACT 1200 FROM H-AND-M.

           IF ( CUR-HH = 0 )
              MOVE 12 TO CUR-HH.

           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":"    TO T-COL.
           MOVE SPACE  TO T-SPACE.

      *-----------------------------------------------------------------
       GET-BW-NAME.
           MOVE CJ-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.

           IF ( IO-FG = 0 )
               MOVE LN-SSNO(1) TO BW-SSNO
               PERFORM READ-BW1-FILE
              IF ( IO-FG = 0 )
                 MOVE BW-LNAME TO WS-HOLD-BW-NAME
              ELSE
                 MOVE "** NONE **" TO WS-HOLD-BW-NAME
           ELSE
              MOVE "** NONE **" TO WS-HOLD-BW-NAME.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO CD-RE-WORKED-ACCOUNT
              MOVE "* NOT ON FILE *" TO CD-DESC.

      *----------------------------------------------------------------
       COMPUTE-HOUR-TO-MIN.

           MOVE CJ-END-TIME TO WS-TIME-HH-MM.
           COMPUTE WS-COMPUTE-MIN = ( WS-TIME-HH * 60 ) + WS-TIME-MM.

           MOVE CJ-START-TIME TO WS-TIME-HH-MM.
           COMPUTE WS-COMPUTE-MIN = WS-COMPUTE-MIN -
                                  ( ( WS-TIME-HH * 60 ) + WS-TIME-MM ).

      *----------------------------------------------------------------
       COMPUTE-MIN-TO-HOUR.

           MOVE TT-DEL-TIME-SPENT( LEV1, LEV2 ) TO WS-COMPUTE-MIN.
           DIVIDE WS-COMPUTE-MIN BY 60
                  GIVING    WS-TIME-HH
                  REMAINDER WS-TIME-MM.

      *----------------------------------------------------------------
       SETUP-WK2-FILE-SEARCH.

           IF LEV-1 = 1
              MOVE 0                TO HOLD-WK2-BRNO
              MOVE HOLD-COLLID      TO HOLD-WK2-COLLID
              MOVE HOLD-RESULT-CODE TO HOLD-WK2-RESULT-CODE
           ELSE
           IF ( LEV-1 = 2 )
              MOVE 9999             TO HOLD-WK2-BRNO
              MOVE SPACES           TO HOLD-WK2-COLLID
              MOVE HOLD-RESULT-CODE TO HOLD-WK2-RESULT-CODE.

      *----------------------------------------------------------------
       GET-COLLECTOR-ID-DESC.

           PERFORM OPEN-CDB1-FILE.
           MOVE "CN"        TO CDB-TYPE.
           MOVE 0           TO CDB-BRANCH.
           MOVE HOLD-COLLID TO CDB-CODE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO CDB-DESC.

           MOVE CDB-DESC TO HOLD-CNDESC.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/GBWK3IN.CPY".

       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPCJ1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
             MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              IF ( EXT-ROUTE-BUF = "00" OR "70" )
                 MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
                 PERFORM SEND-MESS.

           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK3-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.


      *-----------------------------------------------------------------
       EXIT-PROG.
           IF ( SV-BEG-BR NOT = 9999 )
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
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
              DESTROY COLRGB-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
