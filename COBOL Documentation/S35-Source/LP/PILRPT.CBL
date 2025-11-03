       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PILRPT.
       AUTHOR.        KEC.
       DATE-WRITTEN.  10-14-2008.
      ******************************************************************
      *
      * IDENTIFICATION: LP/PILRPT.C
      *
      * DESCRIPTION   : WORLD ACCEPTANCE POTENTIAL INCREASED LOAN REPORT
      *
      *                 REPORT FOR SOLICITING CUSTOMERS THAT COULD
      *                 INCREASE THEIR LOANS.
      *
      * CREATED FROM  : LP/CPCORP.C
      *
      * PROG. REQUEST : PR#00605  10/07/2008
      *                 AWHILE BACK I CREATED A BATCH FOR THE BRANCHES
      *                 CALLED "INC" TO ALLOW THEM TO NARROW DOWN THE
      *                 LIST OF CUSTOMERS THAT COULD POTENTIALLY BE
      *                 SOLICITED FOR AN INCREASED LOAN.  SINCE WE
      *                 DIDN'T HAVE ANYTHING THAT WOULD DO EXACTLY WHAT
      *                 WE WANTED, I JUST USED WHAT WE HAD.
      *                 BASICALLY, I TOOK A COLLECTION REPORT AND
      *                 EXCLUDED LOANS THAT WERE OVER 10 DAYS PAST DUE,
      *                 AND ANY BANKRUPT, LEGAL, JUDGEMENT, OR LOANS
      *                 THAT HASN'T MADE THEIR FIRST PAYMENT YET.  THIS
      *                 GAVE THEM SOMEWHAT TO START BUT I WOULD LIKE TO
      *                 CREATE A REPORT THAT IS MORE OF WHAT THEY
      *                 ACTUALLY NEED.  HERE ARE THE CRITERIA:
      *
      *                 - CLASS 1 ONLY
      *                 - LESS THAN 10 DAYS PAST DUE (RIGHT NOW)
      *                 - NEVER MORE THAN 30 DAYS (ON THIS LOAN)
      *                 - ON SAME GROSS NOTE TWICE (COUNTING THIS LOAN)
      *                 - NO LEGALS
      *                 - NO BANKRUPTS
      *                 - NO JUDGEMENTS
      *                 - SKIP NO 1ST PAYMENT
      *
      *                 GIVE BRANCH A RANGE OF LOAN AMOUNTS AND THE
      *                 OPTION TO INCLUDE A FORM OR JUST GET A REPORT.
      *                 ALSO, PLEASE MAKE THIS WORK THE BATCH PROCESSING
      *                 W/LOCAL SPOOLING OPTION.
      *
      * REV:
      * KEC 2008.1014 {PR#00605} WORLD ACCEPTANCE <A15> [ROSENAUER]
      *               ***NEW***
      * BLM 090217 STRIP OUT OLD LOCAL SPOOL LOGIC
      * BLM 092267 STRIP OUT OLD LOCAL SPOOL LOGIC
      *
      * KEC 091014 {PR#00605} WORLD ACCEPTANCE <A15> [ROSENAUER]
      *            PER JOSH, THE PROGRAM WILL NOT STOP ON ERROR WITH
      *            BAD BW-SSNO.  WILL JUST FILL BW-LNAME WITH ERROR
      *            MESSAGE.
      * BAH 101104 CHANGE SORT TO BE BY BRNO,LNAME,FNAME,SSNO INSTEAD
      *            OF BY SSNO, WORLD PR# 717
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0205 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *
      * BAH 170711 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181015 GLOBAL BWFILE
      * BAH 181115 GLOBAL FMFILE
      * BAH 181211 GLOBAL LNFILE
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190920 REMOVED ACCESS-CALL ON BWFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSSORT.CPY".

      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    S O R T - F I L E
      *
      ******************************************************************
       FD  SORT-FILE
           LABEL RECORDS ARE STANDARD.

       01  SORT-REC.

           03  SORT-KEY.
               05  SORT-BRNO                PIC 9(04).
               05  SORT-BW-LNAME            PIC X(16).
               05  SORT-BW-FNAME            PIC X(16).
               05  SORT-BW-SSNO             PIC 9(09).

      *-----------------------------------------------------------------
           03  SORT-BW-ZIP                           PIC X(10).

      *-----------------------------------------------------------------
      *    SORT-ELE --- 01 = OPEN ACCOUNT      (SORT-ELE---OPEN)
      *                 02 = PREVIOUS ACCOUNT  (SORT-ELE---PREV)
      *-----------------------------------------------------------------
       78  SORT---MAX                                VALUE 02.
           03  SORT-INFO-TABLE                OCCURS SORT---MAX.

               05  SORT-LN-ACCTNO                    PIC 9(08).

               05  SORT-LN-LNAMT                     PIC 9(06)V99.

               05  SORT-LN-LOANDATE                  PIC 9(08).
               05  SORT-LN-POCD                      PIC X(02).
               05  SORT-LN-POFFDATE                  PIC 9(08).
      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/PILRPT.CBL S34 05/21/21-09:00:26 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
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
       COPY "LIBSP/SPWSSORT.CPY".

       78  SORT-ELE---OPEN                           VALUE 01.
       78  SORT-ELE---PREV                           VALUE 02.
       77  SORT-ELE                    PIC 9(02)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  SAVE-SORT-KEY.
           03  SAVE-SORT-BRNO          PIC 9(04).
           03  SAVE-SORT-LNAME         PIC X(16).
           03  SAVE-SORT-FNAME         PIC X(16).
           03  SAVE-SORT-BW-SSNO       PIC 9(09).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  RECORDS-PROCESSED           REDEFINES PROCESS-STAT
                                       PIC 9(15)    COMP-6.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

      ******************************************************************
      *
      *     A V W - W O R K E R S
      *
      *     DESC:  ACCOUNT VERIFICATION WORKERS (AVW)
      ******************************************************************
       01  AVW-WORKERS.

           03  AVW-STATUS-OPEN         PIC X(01)     VALUE SPACES.
               88  AVW-STATUS-OPEN-YES               VALUE "Y".
               88  AVW-STATUS-OPEN-NO                VALUE "N".

           03  AVW-STATUS-PAID         PIC X(01)     VALUE SPACES.
               88  AVW-STATUS-PAID-YES               VALUE "Y".
               88  AVW-STATUS-PAID-NO                VALUE "N".

           03  AVW-STATUS-PL           PIC X(01)     VALUE SPACES.
               88  AVW-STATUS-PL-YES                 VALUE "Y".
               88  AVW-STATUS-PL-NO                  VALUE "N".

           03  AVW-STATUS-PL-TYPE      PIC X(02)     VALUE SPACES.
               88  AVW-STATUS-PL-P1                  VALUE "P1".
               88  AVW-STATUS-PL-P2                  VALUE "P2".
               88  AVW-STATUS-PL-P3                  VALUE "P3".
               88  AVW-STATUS-PL-P4                  VALUE "P4".
               88  AVW-STATUS-PL-P5                  VALUE "P5".
               88  AVW-STATUS-PL-NONE                VALUE SPACES.

           03  AVW-STATUS-PL-ACTIVITY  PIC X(01)     VALUE SPACES.
               88  AVW-STATUS-PL-ACTIVE              VALUE "A".
               88  AVW-STATUS-PL-INACTIVE            VALUE "I".
               88  AVW-STATUS-PL-NONE                VALUE SPACES.

      ******************************************************************
      *
      *     L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING BRANCH/G".
           03  FILLER PIC X(19) VALUE "ROUP               ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G BRANCH NUMBER               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-CLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING CLASS NU".
           03  FILLER PIC X(19) VALUE "MBER               ".

       01  LEGEND-END-CLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "DING CLASS NUMBER             ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING ACCOUNT NUMBER   ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G ACCOUNT NUMBER              ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-LNAMT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING LOAN AMOUNT      ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-LNAMT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G LOAN AMOUNT                 ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-PDUE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING PAST DUE DAYS    ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-PDUE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G PAST DUE DAYS               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-PL-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR P&".
           03  FILLER PIC X(19) VALUE "L                  ".

       01  LEGEND-PL-TYPE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (A)LL".
           03  FILLER PIC X(30) VALUE ", (P)L OR P(2/3/4/5)          ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-PL-ACTIVITY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NA".
           03  FILLER PIC X(30) VALUE "CTIVE, (A)CTIVE OR (B)OTH     ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-BANKRUPT-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR BA".
           03  FILLER PIC X(19) VALUE "NKRUPTCY           ".

       01  LEGEND-JUDGE-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR JU".
           03  FILLER PIC X(19) VALUE "DGEMENTS           ".

       01  LEGEND-LEGAL-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR LE".
           03  FILLER PIC X(19) VALUE "GALS               ".

       01  LEGEND-NO1STPY-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR NO".
           03  FILLER PIC X(19) VALUE " 1ST PAYMENTS      ".

       01  LEGEND-REPORT-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR RE".
           03  FILLER PIC X(19) VALUE "PORT               ".

      *-----------------------------------------------------------------

       01  LEGEND-SRW-FD-FMNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER FORM NUMBER               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

      ******************************************************************
      *
      *     S C R E E N - R A N G E - W O R K E R S                (SRW)
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  BEG-CLASS               PIC 9(02).
           03  END-CLASS               PIC 9(02).
           03  BEG-ACCTNO              PIC 9(06).
           03  END-ACCTNO              PIC 9(06).
           03  BEG-LNAMT               PIC 9(06).
           03  END-LNAMT               PIC 9(06).
           03  BEG-PDUE                PIC 9(04).
           03  END-PDUE                PIC 9(04).

           03  PL-ISO                         PIC X(1).
               88  PL-ISO-INCLUDE                 VALUE "I".
               88  PL-ISO-SKIP                    VALUE "S".
               88  PL-ISO-ONLY                    VALUE "O".

           03  PL-TYPE                        PIC X(1).
               88  PL-TYPE-ALL                    VALUE "A".
               88  PL-TYPE-P1                     VALUE "P".
               88  PL-TYPE-P2                     VALUE "2".
               88  PL-TYPE-P3                     VALUE "3".
               88  PL-TYPE-P4                     VALUE "4".
               88  PL-TYPE-P5                     VALUE "5".

           03  PL-ACTIVITY                    PIC X(1).
               88  PL-ACTIVITY-ACTIVE             VALUE "A".
               88  PL-ACTIVITY-INACTIVE           VALUE "I".
               88  PL-ACTIVITY-BOTH               VALUE "B".

           03  BANKRUPT-ISO            PIC X(01).
               88  BANKRUPT-ISO-INCLUDE              VALUE "I".
               88  BANKRUPT-ISO-SKIP                 VALUE "S".
               88  BANKRUPT-ISO-ONLY                 VALUE "O".
               88  BANKRUPT-ISO-VALID                VALUE "I" "S" "O".

           03  JUDGE-ISO               PIC X(01).
               88  JUDGE-ISO-INCLUDE                 VALUE "I".
               88  JUDGE-ISO-SKIP                    VALUE "S".
               88  JUDGE-ISO-ONLY                    VALUE "O".
               88  JUDGE-ISO-VALID                   VALUE "I" "S" "O".

           03  LEGAL-ISO               PIC X(01).
               88  LEGAL-ISO-INCLUDE                 VALUE "I".
               88  LEGAL-ISO-SKIP                    VALUE "S".
               88  LEGAL-ISO-ONLY                    VALUE "O".
               88  LEGAL-ISO-VALID                   VALUE "I" "S" "O".

           03  NO1STPY-ISO             PIC X(01).
               88  NO1STPY-ISO-INCLUDE               VALUE "I".
               88  NO1STPY-ISO-SKIP                  VALUE "S".
               88  NO1STPY-ISO-ONLY                  VALUE "O".
               88  NO1STPY-ISO-VALID                 VALUE "I" "S" "O".

           03  REPORT-ISO              PIC X(01).
               88  REPORT-ISO-INCLUDE                VALUE "I".
               88  REPORT-ISO-SKIP                   VALUE "S".
               88  REPORT-ISO-ONLY                   VALUE "O".
               88  REPORT-ISO-VALID                  VALUE "I" "S" "O".

           03  SRW-GROUP-WORKERS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

           03  SRW-FD-FMNO             PIC 9(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-BEG-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-END-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.

       01  LN-COUNT            PIC 9(03)     VALUE 99.
       01  LN-FEED             PIC 9(02).
       01  PAGE-NUMBER         PIC 9(04)     VALUE ZEROES.

      ******************************************************************
      *
      *    H E A D E R - L I N E - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  H01-TIME                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  H01-NAME                PIC X(40) VALUE SPACES.
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(40) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-05.
           03  FILLER PIC X(30) VALUE "BRANCH    BORROWER NAME       ".
           03  FILLER PIC X(30) VALUE "              ACCOUNT        L".
           03  FILLER PIC X(10) VALUE "OAN AMOUNT".

       01  HEADER-LINE-06.
           03  FILLER PIC X(80) VALUE ALL "=".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(1).
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(5).
           03  D01-NAME                       PIC X(30).
           03  FILLER                         PIC X(5).
           03  D01-ACCTNO                     PIC Z(7)9.
           03  FILLER                         PIC X(3).
           03  D01-AMOUNT                     PIC ZZZ,ZZZ,ZZ9.99-.

      *
      *    R A N G E - L I N E - W O R K E R S
      *
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(48).
           03  R01-AMT                        REDEFINES R01-FIELD
                                              PIC ZZZ,ZZZ,ZZ9.99-.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9-.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(48).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/PILRPT_DEF.CPY".
       COPY "LIBLP/PILRPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/PILRPT_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 SORT-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE SORT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( PROCESS-STAT-BAD )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

           IF ( RECORDS-PROCESSED = ZEROES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM PROCESS-MODULE.

           IF EXT-EOBUF-GROUP-FG = "N"
              PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "PILRPT" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE PILRPT-QUEUE-POS TO Q-POS.
           MOVE PILRPT-COPIES-POS TO C-POS.
           MOVE PILRPT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-YYYYMMDD.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO              TO BEG-BRANCH END-BRANCH
              MOVE "N"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH      TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM LOAD-SORT-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SORT-FILE.
           PERFORM OPEN-SP1-FILE.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X"                        TO EXT-EOBUF-ERRCD.
           MOVE STAT---BAD                 TO PROCESS-STAT.

       INITIALIZATION-EXIT.
           EXIT.

      ***************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           IF ( UP-KEY )
              GO TO ENTER-REPORT-ISO.

       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

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
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

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

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

       ENTER-BEG-CLASS.

           MOVE LEGEND-BEG-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F6-KEY ) PERFORM SCAN-LC1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE LEGEND-END-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F6-KEY ) PERFORM SCAN-LC1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS."        TO VDU.
           MOVE ZEROES                     TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS."        TO VDU.
           MOVE 99                         TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-ACCTNO.

           MOVE LEGEND-BEG-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE LEGEND-END-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-BEG-LNAMT.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN060BEG-ACCTNO."  TO VDU.
           MOVE ZEROES                TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."  TO VDU.
           MOVE 999999                TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-LNAMT.

           MOVE LEGEND-BEG-LNAMT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E NN060BEG-LNAMT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-LNAMT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LNAMT.

           MOVE VDUNUM0 TO BEG-LNAMT.

      *-----------------------------------------------------------------
       ENTER-END-LNAMT.

           MOVE LEGEND-END-LNAMT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E NN060END-LNAMT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LNAMT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LNAMT.

           MOVE VDUNUM0 TO END-LNAMT.

           IF ( BEG-LNAMT > END-LNAMT )
              MOVE "INVALID LOAN AMOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNAMT.

           GO TO ENTER-BEG-PDUE.

      *-----------------------------------------------------------------
       ENTER-ALL-LNAMT.

           MOVE "S NN060BEG-LNAMT."  TO VDU.
           MOVE ZEROES               TO BEG-LNAMT VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NN060END-LNAMT."  TO VDU.
           MOVE 999999               TO END-LNAMT VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-PDUE.

           MOVE LEGEND-BEG-PDUE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-PDUE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LNAMT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-PDUE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-PDUE.

           MOVE VDUNUM0 TO BEG-PDUE.

      *-----------------------------------------------------------------
       ENTER-END-PDUE.

           MOVE LEGEND-END-PDUE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-PDUE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-PDUE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-PDUE.

           MOVE VDUNUM0 TO END-PDUE.

           IF ( BEG-PDUE > END-PDUE )
              MOVE "INVALID PAST DUE DAYS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-PDUE.

           GO TO ENTER-PL-ISO.

      *-----------------------------------------------------------------
       ENTER-ALL-PDUE.

           MOVE "SNNN020BEG-PDUE."   TO VDU.
           MOVE ZEROES               TO BEG-PDUE  VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-PDUE."   TO VDU.
           MOVE 99                   TO END-PDUE  VDUNUM0.
           PERFORM SCREENIO.

       ENTER-PL-ISO.

           MOVE LEGEND-PL-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PL-ISO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-PDUE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PL-ISO.

           MOVE VDUBUF TO PL-ISO.

           IF ( PL-ISO-INCLUDE ) OR
              ( PL-ISO-ONLY    )
              GO TO ENTER-PL-TYPE.

           IF ( PL-ISO-SKIP    )
              MOVE "S  A010PL-TYPE."       TO VDU
              MOVE "A"                     TO VDUBUF PL-TYPE
              PERFORM SCREENIO
              MOVE "S  A010PL-ACTIVITY."   TO VDU
              MOVE "B"                     TO VDUBUF PL-ACTIVITY
              PERFORM SCREENIO
              GO TO ENTER-BANKRUPT-ISO.

           GO TO ENTER-PL-ISO.

      *----------------------------------------------------------------
       ENTER-PL-TYPE.

           MOVE LEGEND-PL-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PL-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PL-ISO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PL-TYPE.

           MOVE VDUBUF TO PL-TYPE.

           IF ( NOT PL-TYPE-ALL ) AND
              ( NOT PL-TYPE-P1  ) AND
              ( NOT PL-TYPE-P2  ) AND
              ( NOT PL-TYPE-P3  ) AND
              ( NOT PL-TYPE-P4  ) AND
              ( NOT PL-TYPE-P5  )
              GO TO ENTER-PL-TYPE.

      *----------------------------------------------------------------
       ENTER-PL-ACTIVITY.

           MOVE LEGEND-PL-ACTIVITY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PL-ACTIVITY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PL-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PL-ACTIVITY.

           MOVE VDUBUF TO PL-ACTIVITY.

           IF ( NOT PL-ACTIVITY-INACTIVE ) AND
              ( NOT PL-ACTIVITY-ACTIVE   ) AND
              ( NOT PL-ACTIVITY-BOTH     )
              GO TO ENTER-PL-ACTIVITY.

       ENTER-BANKRUPT-ISO.

           MOVE LEGEND-BANKRUPT-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010BANKRUPT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY )
              IF ( PL-ISO-SKIP )
                 GO TO ENTER-PL-ISO
              ELSE
                 GO TO ENTER-PL-ACTIVITY.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BANKRUPT-ISO.

           MOVE VDUBUF TO BANKRUPT-ISO.

           IF ( NOT BANKRUPT-ISO-VALID )
              MOVE "INVALID: (I)NCLUDE, (S)KIP OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BANKRUPT-ISO.

      *-----------------------------------------------------------------
       ENTER-JUDGE-ISO.

           MOVE LEGEND-JUDGE-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010JUDGE-ISO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BANKRUPT-ISO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-JUDGE-ISO.

           MOVE VDUBUF TO JUDGE-ISO.

           IF ( NOT JUDGE-ISO-VALID )
              MOVE "INVALID: (I)NCLUDE, (S)KIP OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-JUDGE-ISO.

      *-----------------------------------------------------------------
       ENTER-LEGAL-ISO.

           MOVE LEGEND-LEGAL-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010LEGAL-ISO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-JUDGE-ISO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-LEGAL-ISO.

           MOVE VDUBUF TO LEGAL-ISO.

           IF ( NOT LEGAL-ISO-VALID )
              MOVE "INVALID: (I)NCLUDE, (S)KIP OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LEGAL-ISO.

      *-----------------------------------------------------------------
       ENTER-NO1STPY-ISO.

           MOVE LEGEND-NO1STPY-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010NO1STPY-ISO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-LEGAL-ISO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-NO1STPY-ISO.

           MOVE VDUBUF TO NO1STPY-ISO.

           IF ( NOT NO1STPY-ISO-VALID )
              MOVE "INVALID: (I)NCLUDE, (S)KIP OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NO1STPY-ISO.

      *-----------------------------------------------------------------
       ENTER-REPORT-ISO.

           MOVE LEGEND-REPORT-ISO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010REPORT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-NO1STPY-ISO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-REPORT-ISO.

           MOVE VDUBUF TO REPORT-ISO.

           IF ( NOT REPORT-ISO-VALID )
              MOVE "INVALID: (I)NCLUDE, (S)KIP OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REPORT-ISO.

       ENTER-SRW-FD-FMNO.

           IF ( REPORT-ISO-ONLY )
              MOVE SPACES TO WR-BUF
              MOVE "FORM-PROMPT FORM-NUMBER." TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE ZEROES TO SRW-FD-FMNO
              GO TO ENTER-PARAMETERS-END.

           MOVE LEGEND-SRW-FD-FMNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A220FORM-PROMPT."    TO VDU.
           MOVE "FORM NUMBER          :" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "ENNN030FORM-NUMBER." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-REPORT-ISO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F6-KEY ) PERFORM SCAN-FD1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-SRW-FD-FMNO.

           MOVE VDUNUM0 TO SRW-FD-FMNO FD-FMNO.

           IF ( SRW-FD-FMNO = ZEROES )
              GO TO ENTER-PARAMETERS-END.

           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.

           IF ( IO-BAD )
              MOVE "REQUESTED FORM NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SRW-FD-FMNO.

           IF FD-1STFILE NOT = 1
              MOVE "1STFILE MUST BE 1" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SRW-FD-FMNO.

       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( F1-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.
           IF ( F7-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."    TO VDU
              MOVE ENT-GROUP               TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."    TO VDU
              MOVE SPACES                  TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."    TO VDU
              MOVE BEG-BRANCH              TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."    TO VDU
              MOVE END-BRANCH              TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020BEG-CLASS."        TO VDU.
           MOVE BEG-CLASS                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS."        TO VDU.
           MOVE END-CLASS                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060BEG-ACCTNO."       TO VDU.
           MOVE BEG-ACCTNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."       TO VDU.
           MOVE END-ACCTNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NN060BEG-LNAMT."        TO VDU.
           MOVE BEG-LNAMT                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NN060END-LNAMT."        TO VDU.
           MOVE END-LNAMT                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020BEG-PDUE."         TO VDU.
           MOVE BEG-PDUE                   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-PDUE."         TO VDU.
           MOVE END-PDUE                   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010PL-ISO."           TO VDU.
           MOVE PL-ISO                     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PL-TYPE."          TO VDU.
           MOVE PL-TYPE                    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PL-ACTIVITY."      TO VDU.
           MOVE PL-ACTIVITY                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010BANKRUPT-ISO."     TO VDU.
           MOVE BANKRUPT-ISO               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010JUDGE-ISO."        TO VDU.
           MOVE JUDGE-ISO                  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LEGAL-ISO."        TO VDU.
           MOVE LEGAL-ISO                  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010NO1STPY-ISO."      TO VDU.
           MOVE NO1STPY-ISO                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010REPORT-ISO."       TO VDU.
           MOVE REPORT-ISO                 TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DISPLAY-PAR-SRW-FD-FMNO.

           IF ( REPORT-ISO-ONLY )
              GO TO DISPLAY-PARAMETERS-EXIT.

           MOVE "S  A220FORM-PROMPT."      TO VDU.
           MOVE "FORM NUMBER          :"   TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN030FORM-NUMBER."      TO VDU.
           MOVE SRW-FD-FMNO                TO VDUNUM0.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************
       SCAN-FD1-FILE SECTION.
      *******************************************

           MOVE ZEROES                     TO FDSCAN-KEY.
           MOVE "S"                        TO FDSCAN-FMTYPE.
           MOVE "S"                        TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF (FDSCAN-STATUS NOT = "00") OR (FDSCAN-KEY = SPACES)
               MOVE VDU---F6-KEY           TO VDUSTATUS
               GO TO SCAN-FD1-FILE-EXIT.

           MOVE "S  A030FORM-NUMBER."      TO VDU.
           MOVE FDSCAN-KEY-ALP             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE VDU---ENTER-KEY            TO VDUSTATUS.
           MOVE FDSCAN-KEY-ALP             TO VDUNUM0.

       SCAN-FD1-FILE-EXIT.
           EXIT.

      *********************************************
       SCAN-GR1-FILE SECTION.
      *********************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( GRSCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

       SCAN-GR1-FILE-EXIT.
           EXIT.

      *****************************************
       SCAN-LC1-FILE SECTION.
      *****************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE VDU---F6-KEY            TO VDUSTATUS
              GO TO SCAN-LC1-FILE-EXIT.

           MOVE "S"                        TO VDUACTION.
           MOVE LCSCAN-KEY                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE LCSCAN-KEY                 TO VDUNUM0.
           MOVE VDU---ENTER-KEY            TO VDUSTATUS.

       SCAN-LC1-FILE-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************

           MOVE ZEROES                     TO RECORDS-PROCESSED.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       EXECUTION-NEXT-GROUP.
           IF ( GROUP-FLAG NOT = "Y" )
              GO TO EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = 0 )
              GO TO EXECUTION-MODULE-EXIT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXECUTION-PROCESS-BRANCH.

       EXECUTION-NEXT-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO EXECUTION-MODULE-EXIT.

       EXECUTION-PROCESS-BRANCH.

           PERFORM PROCESS-BRANCH-DRIVER.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

     *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
     *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              FM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-FM1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LTP1-FILE.

           MOVE LN-PATH-OWNBR        TO LN-OWNBR
                                        QLN1-WBEG-OWNBR
                                        QLN1-WEND-OWNBR.
           MOVE BEG-ACCTNO           TO LN-ACCTNO
                                        QLN1-WBEG-ACCTNO.
           MOVE END-ACCTNO           TO QLN1-WEND-ACCTNO.

           PERFORM START-LN1-FILE.

       PROCESS-BRANCH-NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-BRANCH-DRIVER-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM ACCOUNT-VERIFY-ROUTINE.

           IF ( LN-CLASS < BEG-CLASS ) OR
              ( LN-CLASS > END-CLASS )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( LN-ACCTNO < BEG-ACCTNO ) OR
              ( LN-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( LN-SUM-LNAMT < BEG-LNAMT ) OR
              ( LN-SUM-LNAMT > END-LNAMT )
              GO TO PROCESS-BRANCH-NEXT-LN.

           MOVE LN-ORGST                   TO SP-ORGST.
           MOVE LN-CLASS                   TO SP-SPRCLASS.
           MOVE LN-SUBCLASS                TO SP-SUBCLASS.
           MOVE LN-LAWCODE                 TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-NEXT-LN.

           MOVE SYSDATE-YYYYMMDD           TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE               TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.

           IF ( PDUE-DAYS < BEG-PDUE ) OR
              ( PDUE-DAYS > END-PDUE )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-ISO-ONLY      ) AND
              ( AVW-STATUS-PL-NO )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-ISO-SKIP       ) AND
              ( AVW-STATUS-PL-YES )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-TYPE-P1           ) AND
              ( NOT AVW-STATUS-PL-P1 )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-TYPE-P2           ) AND
              ( NOT AVW-STATUS-PL-P2 )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-TYPE-P3           ) AND
              ( NOT AVW-STATUS-PL-P3 )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-TYPE-P4           ) AND
              ( NOT AVW-STATUS-PL-P4 )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-TYPE-P5           ) AND
              ( NOT AVW-STATUS-PL-P5 )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-ACTIVITY-ACTIVE     ) AND
              ( AVW-STATUS-PL-INACTIVE )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( PL-ACTIVITY-INACTIVE ) AND
              ( AVW-STATUS-PL-ACTIVE )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( BANKRUPT-ISO-ONLY      ) AND
              ( LN-BNKRPTDATE = ZEROES )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( BANKRUPT-ISO-SKIP          ) AND
              ( LN-BNKRPTDATE NOT = ZEROES )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( JUDGE-ISO-ONLY     ) AND
              ( LN-JDDATE = ZEROES )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( JUDGE-ISO-SKIP         ) AND
              ( LN-JDDATE NOT = ZEROES )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( LEGAL-ISO-ONLY        ) AND
              ( LN-LEGALDATE = ZEROES )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( LEGAL-ISO-SKIP            ) AND
              ( LN-LEGALDATE NOT = ZEROES )
              GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( NO1STPY-ISO-ONLY )
              IF ( LN-MIDSTREAM NOT = "Y" )
                 IF ( LN-DATE-PAID-LAST NOT = ZEROES )
                    GO TO PROCESS-BRANCH-NEXT-LN
                 END-IF
              ELSE
                 IF ( BR-MIDSTREAM-1STPAY-FG NOT = "Y" )
                    IF ( LN-DATE-PAID-LAST NOT = ZEROES )
                       GO TO PROCESS-BRANCH-NEXT-LN
                    END-IF
                 ELSE
                    IF ( LN-ORIGPURAMT NOT = LN-CURBAL )
                       GO TO PROCESS-BRANCH-NEXT-LN.

           IF ( NO1STPY-ISO-SKIP )
              IF ( LN-MIDSTREAM NOT = "Y" )
                 IF ( LN-DATE-PAID-LAST = ZEROES )
                    GO TO PROCESS-BRANCH-NEXT-LN
                 END-IF
              ELSE
              IF ( BR-MIDSTREAM-1STPAY-FG NOT = "Y" )
                 IF ( LN-DATE-PAID-LAST = ZEROES )
                    GO TO PROCESS-BRANCH-NEXT-LN
                 END-IF
              ELSE
                 IF ( LN-ORIGPURAMT = LN-CURBAL )
                    GO TO PROCESS-BRANCH-NEXT-LN.

      *-------------------------------------{ VALID RECORD TO PROCESS  }
       PROCESS-BRANCH-VALID-REC.

           ADD  01                         TO RECORDS-PROCESSED.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF ( IO-BAD )
              MOVE "SSNO NOT ON FILE"      TO BW-LNAME
              MOVE SPACES                  TO BW-FNAME
              MOVE ZEROES                  TO BW-ZIP.

           PERFORM UPDATE-SORT-DRIVER.

           GO TO PROCESS-BRANCH-NEXT-LN.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LTP1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-SORT-DRIVER SECTION.
      ******************************************************************

           MOVE BR-NO                      TO SAVE-SORT-BRNO.
           MOVE BW-SSNO                    TO SAVE-SORT-BW-SSNO.
           MOVE BW-LNAME                   TO SAVE-SORT-LNAME.
           MOVE BW-FNAME                   TO SAVE-SORT-FNAME.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           PERFORM READ-SORT-FILE.
           IF ( IO-GOOD )
              GO TO UPDATE-SORT-DRIVER-OPEN.

           MOVE SPACES                     TO SORT-REC.
           MOVE SAVE-SORT-KEY              TO SORT-KEY.
           MOVE BW-ZIP                     TO SORT-BW-ZIP.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > SORT---MAX
                   MOVE ZEROES         TO SORT-LN-ACCTNO  (SUB)
                                          SORT-LN-LNAMT   (SUB)
                                          SORT-LN-LOANDATE(SUB)
                                          SORT-LN-POFFDATE(SUB)
           END-PERFORM.

           PERFORM WRITE-SORT-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

      * SORT-ELE --- 01 = OPEN ACCOUNT      (SORT-ELE---OPEN)
      *              02 = PREVIOUS ACCOUNT  (SORT-ELE---PREV)

       UPDATE-SORT-DRIVER-OPEN.

           MOVE ZEROES                     TO SORT-ELE.

           IF ( AVW-STATUS-OPEN-NO )
              GO TO UPDATE-SORT-DRIVER-PREV.

           IF ( SORT-LN-ACCTNO(SORT-ELE---OPEN) = ZEROES )
              MOVE SORT-ELE---OPEN         TO SORT-ELE
              GO TO UPDATE-SORT-DRIVER-PREV.

           IF ( LN-LOANDATE > SORT-LN-LOANDATE(SORT-ELE---OPEN) )
              MOVE SORT-ELE---OPEN         TO SORT-ELE.

       UPDATE-SORT-DRIVER-PREV.

           IF ( AVW-STATUS-OPEN-YES  ) OR
              ( LN-POFFDATE = ZEROES )
              GO TO UPDATE-SORT-DRIVER-REWRITE.

           IF ( SORT-LN-POFFDATE(SORT-ELE---PREV) = ZEROES )
              MOVE SORT-ELE---PREV         TO SORT-ELE
              GO TO UPDATE-SORT-DRIVER-REWRITE.

           IF (LN-POFFDATE > SORT-LN-POFFDATE(SORT-ELE---PREV) )
              MOVE SORT-ELE---PREV         TO SORT-ELE.

       UPDATE-SORT-DRIVER-REWRITE.

           IF ( SORT-ELE = ZEROES )
              GO TO UPDATE-SORT-DRIVER-EXIT.

           MOVE LN-ACCTNO            TO SORT-LN-ACCTNO  (SORT-ELE).
           MOVE LN-LNAMT             TO SORT-LN-LNAMT   (SORT-ELE).

           MOVE LN-LOANDATE          TO SORT-LN-LOANDATE(SORT-ELE).

           MOVE LN-POCD              TO SORT-LN-POCD    (SORT-ELE).

           MOVE LN-POFFDATE          TO SORT-LN-POFFDATE(SORT-ELE).

           PERFORM REWRITE-SORT-FILE.

       UPDATE-SORT-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-MODULE SECTION.
      ******************************************************************

           IF ( NOT REPORT-ISO-SKIP      )
              PERFORM OPEN-PR-FILE.

           MOVE ZEROES                     TO SORT-BRNO.
           MOVE ZEROES                     TO SORT-BW-SSNO.
           MOVE SPACES                     TO SORT-BW-LNAME.
           MOVE SPACES                     TO SORT-BW-FNAME.

           PERFORM CLOSE-SORT-FILE.
           PERFORM OPEN-SORT-FILE.
           PERFORM START-SORT-FILE.

       PROCESS-NEXT-SORT-REC.

           PERFORM READ-SORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-MODULE-EXIT.

      *    AS PER PR#00605 (ORIGINAL REQUEST; JOSH ROYSTON), NEED
      *    TO USE BORROWERS (SSNO) THAT HAVE LOANS THAT HAD THE EXACT
      *    SAME LOAN AMOUNT (LN-LNAMT) AT LEAST TWICE.
      *
      *    01/20/2009 AS PER ROSENAUER, THE MOST NEWEST OPEN LOAN IS TO
      *    BE COMPARED TO THE MOST RECENT PAID OFF LOAN TO SEE IF THEY
      *    HAVE THE EXACT SAME LOAN AMOUNT (LN-LNAMT).
           IF ( SORT-LN-ACCTNO(SORT-ELE---OPEN) = ZEROES ) OR
              ( SORT-LN-ACCTNO(SORT-ELE---PREV) = ZEROES )
              GO TO PROCESS-NEXT-SORT-REC.

           IF ( SORT-LN-LNAMT(SORT-ELE---OPEN) NOT =
                SORT-LN-LNAMT(SORT-ELE---PREV) )
              GO TO PROCESS-NEXT-SORT-REC.

      *-----------------------------------------------{ VALID SORT-REC }
       PROCESS-VALID-RECORD.

           PERFORM FORM-DRIVER.
           PERFORM PRINT-DRIVER.

           GO TO PROCESS-NEXT-SORT-REC.

       PROCESS-MODULE-EXIT.
           PERFORM CLOSE-SORT-FILE.


      ******************************************************************
       ACCOUNT-VERIFY-ROUTINE SECTION.
      ******************************************************************

           MOVE SPACES TO AVW-WORKERS.

           IF ( LN-CURBAL NOT = ZEROES ) OR
              ( LN-INTBAL NOT = ZEROES ) OR
              ( LN-LCBAL  NOT = ZEROES ) OR
              ( LN-OTHBAL NOT = ZEROES )
              MOVE "Y"                     TO AVW-STATUS-OPEN
           ELSE
              MOVE "N"                     TO AVW-STATUS-OPEN.

           IF ( LN-CURBAL = ZEROES ) AND
              ( LN-INTBAL = ZEROES ) AND
              ( LN-LCBAL  = ZEROES ) AND
              ( LN-OTHBAL = ZEROES )
              MOVE "Y"                     TO AVW-STATUS-PAID
           ELSE
              MOVE "N"                     TO AVW-STATUS-PAID.

           IF ( LN-PLDATE NOT = ZEROES )
              MOVE "Y"                     TO AVW-STATUS-PL
           ELSE
              MOVE "N"                     TO AVW-STATUS-PL
              MOVE SPACES                  TO AVW-STATUS-PL-TYPE
              MOVE SPACES                  TO AVW-STATUS-PL-ACTIVITY
              GO TO ACCOUNT-VERIFY-ROUTINE-EXIT.

           IF ( LN-PLTYPE = "P" ) MOVE "P1" TO AVW-STATUS-PL-TYPE.
           IF ( LN-PLTYPE = "2" ) MOVE "P2" TO AVW-STATUS-PL-TYPE.
           IF ( LN-PLTYPE = "3" ) MOVE "P3" TO AVW-STATUS-PL-TYPE.
           IF ( LN-PLTYPE = "4" ) MOVE "P4" TO AVW-STATUS-PL-TYPE.
           IF ( LN-PLTYPE = "5" ) MOVE "P5" TO AVW-STATUS-PL-TYPE.

           IF ( LN-PLCD = "P" )
              MOVE "ACTIVE  " TO AVW-STATUS-PL-ACTIVITY
           ELSE
              MOVE "INACTIVE" TO AVW-STATUS-PL-ACTIVITY.

       ACCOUNT-VERIFY-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       FORM-DRIVER SECTION.
      ******************************************************************

           IF ( REPORT-ISO-ONLY      ) OR
              ( SRW-FD-FMNO = ZEROES )
              GO TO FORM-DRIVER-EXIT.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY.
           MOVE SORT-LN-ACCTNO(1)      TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE SRW-FD-FMNO            TO LPWSFM-EDIT-FM-FORMNO.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE SRW-FD-FMNO            TO FM-FMNO.

           MOVE FM1-KEY                TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

           MOVE SORT-BW-ZIP        TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE SYSDATE-YYYYMMDD   TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           IF IO-FG = 0
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

       FORM-DRIVER-END.

           PERFORM CLOSE-FM1-FILE.

       FORM-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PRINT-DRIVER SECTION.
      ******************************************************************

           IF ( REPORT-ISO-SKIP )
              GO TO PRINT-DRIVER-EXIT.

           MOVE SORT-BRNO                       TO D01-BRNO.

           STRING SORT-BW-LNAME, ", ",
                  SORT-BW-FNAME
                  DELIMITED BY "   "           INTO D01-NAME.

           MOVE SORT-LN-ACCTNO(SORT-ELE---OPEN) TO D01-ACCTNO.
           MOVE SORT-LN-LNAMT (SORT-ELE---OPEN) TO D01-AMOUNT.

           PERFORM WRITE-DETAIL-LINE.

       PRINT-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD  LN-FEED                   TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-HEADER-LINES SECTION.
      ******************************************************************

           MOVE 01                         TO LN-COUNT.
           ADD  01                         TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE TIME-EDIT                  TO H01-TIME.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           CALL "C$JUSTIFY"             USING H01-NAME "C".
           MOVE PAGE-NUMBER            TO H01-PAGE.

           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO H02-USERID.
           MOVE "POTENTIAL INCREASED LOANS EXTRACT/REPORT"
                                           TO H02-TITLE.

           MOVE HEADER-LINE-02             TO PR-REC.
           ADD  01                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-05             TO PR-REC.
           ADD  02                         TO LN-COUNT.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-06             TO PR-REC.
           ADD  01                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

       WRITE-HEADER-LINES-END.

           MOVE 2                         TO LN-FEED.

       WRITE-HEADER-LINES-EXIT.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           IF ( REPORT-ISO-SKIP )
              GO TO RANGE-LINES-EXIT.

           MOVE 88                                    TO LN-COUNT.

           IF ( GROUP-FLAG = "Y" )
              MOVE "BEGINNING BRANCH              : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : " TO R01-PROMPT
              MOVE SPACES                             TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH              : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS NUMBER        : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS NUMBER        : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LOAN AMOUNT         : "    TO R01-PROMPT.
           MOVE BEG-LNAMT                             TO R01-AMT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LOAN AMOUNT         : "    TO R01-PROMPT.
           MOVE END-LNAMT                             TO R01-AMT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING PAST DUE DAYS       : "    TO R01-PROMPT.
           MOVE BEG-PDUE                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    PAST DUE DAYS       : "    TO R01-PROMPT.
           MOVE END-PDUE                              TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L                    (I/S/O): "    TO R01-PROMPT.
           MOVE PL-ISO                                TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L - TYPE         (L/2/3/4/5): "    TO R01-PROMPT.
           MOVE PL-TYPE                               TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L - ACTIVITY         (I/A/B): "    TO R01-PROMPT.
           MOVE PL-ACTIVITY                           TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BANKRUPTCY             (I/S/O): "    TO R01-PROMPT.
           MOVE BANKRUPT-ISO                          TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "JUDGEMENT              (I/S/O): "    TO R01-PROMPT.
           MOVE JUDGE-ISO                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LEGAL                  (I/S/O): "    TO R01-PROMPT.
           MOVE LEGAL-ISO                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NO 1ST PAYMENT         (I/S/O): "    TO R01-PROMPT.
           MOVE NO1STPY-ISO                           TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT                 (I/S/O): "    TO R01-PROMPT.
           MOVE REPORT-ISO                            TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           IF ( REPORT-ISO-INCLUDE ) OR
              ( REPORT-ISO-SKIP    )
              MOVE "FORM NUMBER                   : " TO R01-PROMPT
              MOVE SRW-FD-FMNO                        TO R01-TEXT
              MOVE 02                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO PILRPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PILRPT-SYSDATE.
           DISPLAY PILRPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PILRPT-FORM-FIELDS TO WR-BUF.
           MOVE PILRPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE PILRPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE PILRPT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/PILRPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

           MOVE BR-NO                      TO PROGID-BRANCH.
           MOVE PROGID-TEXT-PROCESSING     TO PROGID-TEXT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

       COPY "LIBSP/SPIOSORT.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           PERFORM REMOVE-SORT-FILE.

       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PILRPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
