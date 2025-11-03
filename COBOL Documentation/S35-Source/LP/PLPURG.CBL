       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PLPURG.
       DATE-WRITTEN. 06/22/04.
      ******************************************************************
      *
      *                      (LP)
      *  DESCRIPTION:  P&L CHARGEOFF PURGE
      *
      *                CURRENTLY, PURGES CHARGE OFF ACCOUNTS, PAYMENTS
      *                MEMOS AND TRAILERS BASED UPON CHARGE OFF DATE RANGE
      *                LAST PAYMENT DATE RANGE.
      *
      *  REV:
      *   040622 BAH NEW, FOR WORLD AND CITIZENS (TN), PR# 1954
      * BAH 050601 CHANGED LN-TRW-1ST-30 TO LN-TRW-DOFD-DATE, DATE OF FIRST
      *            DELINQUENCY
      * BAH 051025 REMOVED LN-LEDGERCNT, LN-LPCNT, NOT USED....
      *   CS 081113 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM EARNED
      *             AND UNEARNED    REGACC PR#210
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *
      * KEC 2016.0506 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED MA-APPNO TO MA-APP-NUMBER TO MATCH MA1-KEY.
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      * CS 160509 REMOVED REDEF OF LE-FLAG/LE-SSNO AND MADE SEPARATE FIELDS
      *           CHANGED LE-FLAG FROM PIC X(4) TO PIC X. MOVED SPACES TO
      *           LE-FLAG
      *
      * KEC 2017.0425 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED 'LE' FILE TO HAVE SUB-PREFIX OF 'E' OR 'R'.
      *  BAH 170711 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 180824 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181030 GLOBAL LEFILE
      *  CS 181112 GLOBAL LSLFILE
      *  CS 181112 GLOBAL LSJFILE
      *  CS 181113 GLOBAL LSGFILE
      *  CS 181114 GLOBAL LSMFILE
      *  CS 181114 GLOBAL LSPFILE
      *  CS 181114 GLOBAL LSRFILE
      *  CS 181115 GLOBAL LSX-FILE
      *  CS 181119 GLOBAL LSD-FILE
      *  CS 181119 GLOBAL LSF-FILE
      *  CS 181121 GLOBAL LTA-FILE
      *  CS 181121 GLOBAL LTB-FILE
      * BAH 181128 GLOBAL LMFILE
      *  CS 181206 GLOBAL LTCFILE
      * BAH 181211 GLOBAL LNFILE
      * BAH 190102 GLOBAL LPFILE
      *  CS 190102 GLOBAL LTFFILE
      *  CS 190102 GLOBAL LTIFILE
      *  CS 190111 GLOBAL LTIFILE
      *  CS 190111 GLOBAL LTPFILE
      * BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 20190711 ADDED LPLHCL, FUTURE FIELDS WERE NOT CLEARED!
      * BAH 20190924 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 2024.0131 DELETE NOTICE IF EXIST
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/PLPURG.CBL S35 02/20/24-13:21:19 >".

      *
      *    F I L E - W O R K E R S
      *
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTF.CPY".
       COPY "LIBLP/LP01LTF_SQL.CPY".
       COPY "LIBLP/LPWSLTF.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LSD.CPY".
       COPY "LIBLP/LP01LSD_SQL.CPY".
       COPY "LIBLP/LPWSLSD.CPY".
       COPY "LIBLP/LP01LSF.CPY".
       COPY "LIBLP/LP01LSF_SQL.CPY".
       COPY "LIBLP/LPWSLSF.CPY".
       COPY "LIBLP/LP01LSG.CPY".
       COPY "LIBLP/LP01LSG_SQL.CPY".
       COPY "LIBLP/LPWSLSG.CPY".
       COPY "LIBLP/LP01LSJ.CPY".
       COPY "LIBLP/LP01LSJ_SQL.CPY".
       COPY "LIBLP/LPWSLSJ.CPY".
       COPY "LIBLP/LP01LSL.CPY".
       COPY "LIBLP/LP01LSL_SQL.CPY".
       COPY "LIBLP/LPWSLSL.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01LSP.CPY".
       COPY "LIBLP/LP01LSP_SQL.CPY".
       COPY "LIBLP/LPWSLSP.CPY".
       COPY "LIBLP/LP01LSR.CPY".
       COPY "LIBLP/LP01LSR_SQL.CPY".
       COPY "LIBLP/LPWSLSR.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".


      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
       01  CLASS-FG                     PIC X(03)         VALUE SPACES.
           88  CLASS-BEG                                  VALUE "BEG".
           88  CLASS-END                                  VALUE "END".

       01  FIRST-MEMO                   PIC X(01)         VALUE "N".
       01  FIRST-NAME                   PIC X(16).
       01  FIRST-TIME                   PIC X(01)         VALUE "Y".

       01  HEAD-UP-PAYMENTS             PIC X(01)         VALUE "N".
       01  HEAD-UP-ENDORSERS            PIC X(01)         VALUE "N".

       01  LEGEND                       PIC X(60)         VALUE SPACES.
       01  MESS-LIST                    PIC X(5)     VALUE "MESS.".

       01  NAME-WORKER                  PIC X(34).
       01  PRINT-PAYMENT-LINE           PIC X(01)         VALUE "N".

       01  PROGRESS-LINE.
           03  FILLER                   PIC X(08)         VALUE
                                                             "BRANCH: ".
           03  PROGRESS-LINE-BRANCH     PIC 9(04)         VALUE ZEROES.
           03  PROGRESS-LINE-BRANCHX    REDEFINES PROGRESS-LINE-BRANCH
                                        PIC X(04).
           03  PROGRESS-LINE-TEXT       PIC X(48)         VALUE SPACES.


       01  PROGRESS-MESSAGE-PURGE       PIC X(48)         VALUE
           " ----- PURGE IN PROGRESS........................".

       01  PROGRESS-MESSAGE-P-AND-R      PIC X(48)         VALUE
           " ----- PURGE AND REPORT IN PROGRESS.............".

       01  PROGRESS-MESSAGE-REPORT      PIC X(48)         VALUE
           " ----- REPORT IN PROGRESS.......................".

       01  PURGE-COUNT              PIC 9(11)  VALUE ZEROES.

       01  PURGE-FLAG                   PIC X(01).
           88  PURGE-OK                                   VALUE "Y".

       01  RECORDS-FOUND                PIC X(01)         VALUE SPACES.
           88  NO-RECORDS-FOUND                           VALUE "N".
           88  SKIP-RECORDS-FOUND                         VALUE SPACES.

       01  SUB                          PIC 9(06)  COMP-3 VALUE ZEROES.


       01  HOLD-SYSDATE                      PIC 9(8).

      *
      *    S C R E E N - F I E L D S
      *
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  ACTION                        PIC X(01).
               88  ACTION-BOTH               VALUE "B".
               88  ACTION-PURGE              VALUE "P".
               88  ACTION-REPORT             VALUE "R".
               88  ACTION-SINGLE-LINE        VALUE "L".
               88  ACTION-VALID              VALUE "B" "P" "R" "L".

           03  BEG-BRANCH                    PIC 9(4).
           03  END-BRANCH                    PIC 9(4).
           03  BEG-CLASS                     PIC 9(3).
           03  END-CLASS                     PIC 9(3).
           03  BEG-ACCT                      PIC 9(8).
           03  END-ACCT                      PIC 9(8).
           03  BEG-PL-DATE                   PIC 9(8).
           03  END-PL-DATE                   PIC 9(8).
           03  BEG-PAY-DATE                  PIC 9(8).
           03  END-PAY-DATE                  PIC 9(8).
           03  PRINT-OPTION-BUFFERS.
               05  PRINT-MEMOS               PIC X(01).
               05  PRINT-END                 PIC X(01).
               05  PRINT-PAYMENTS            PIC X(01).
           03  GROUP-OPTION-BUFFERS.
               05  GROUP-FLAG                PIC X(01).
               05  ENT-GROUP                 PIC X(04).

       01  VERSION-CONTROL                   PIC X(01)     VALUE "B".

       01  SAVE-BEG-BRANCH                   PIC 9(04)     VALUE ZEROES.
       01  SAVE-END-BRANCH                   PIC 9(04)     VALUE ZEROES.

       01  LN-FEED                       PIC 9(02)     VALUE ZEROES.
       01  LN-COUNT                      PIC 9(02)     VALUE ZEROES.

       01  PAGE-NUMBER                       PIC 9(04)     VALUE ZEROES.

      *
      *    H E A D E R - B U F F E R S
      *
       01  HEAD1.
           03  H-DATE                      PIC X(08)    VALUE SPACES.
           03  FILLER                      PIC X(02)   VALUE SPACES.
           03  H-HR                        PIC 9(02)   VALUE ZEROES.
           03  FILLER                      PIC X(01)   VALUE ":".
           03  H-MIN                       PIC 9(02)   VALUE ZEROES.
           03  FILLER                      PIC X(31)   VALUE SPACES.
           03  H-NAME                      PIC X(40)   VALUE SPACES.
           03  FILLER                      PIC X(37)   VALUE SPACES.
           03  FILLER                      PIC X(05)   VALUE "PAGE ".
           03  H-PAGE-NO                   PIC ZZZ9.

       01  HEAD2.
           03  FILLER                      PIC X(10)   VALUE "USER ID:".
           03  H-USERID                    PIC X(10).
           03  FILLER                      PIC X(26)   VALUE SPACES.
           03  FILLER                      PIC X(41)   VALUE
               "- - -   P&L PURGE REPORT   - - -".
           03  FILLER                      PIC X(45)   VALUE SPACES.

       01  HEAD3.
           03  FILLER                      PIC X(08)   VALUE "LOAN NO.".
           03  FILLER                      PIC X(01)   VALUE SPACES.
           03  D-LOAN-NUM                  PIC X(16).
           03  FILLER                      PIC X(01).
           03  D-TITLE                     PIC X(26).
           03  FILLER                      PIC X(01).
           03  FILLER                      PIC X(07)   VALUE "PAGE : ".
           03  D-PAGE                      PIC 9(01).
           03  FILLER                      PIC X(27)   VALUE SPACES.
           03  D-TITLE2                    PIC X(26).
           03  FILLER                      PIC X(01).
           03  FILLER                      PIC X(07)   VALUE "PAGE : ".
           03  D-PAGE2                     PIC 9(01).

       01  HEAD4.
           03  FILLER                      PIC X(08)   VALUE "LOAN NO.".
           03  FILLER                      PIC X(01)   VALUE SPACES.
           03  D2-LOAN-NUM                 PIC X(16).
           03  FILLER                      PIC X(01).
           03  D2-TITLE                    PIC X(21).
           03  FILLER                      PIC X(01).
           03  FILLER                      PIC X(07)   VALUE "PAGE : ".
           03  D2-PAGE                     PIC 9(01).
           03  FILLER                      PIC X(32)   VALUE SPACES.
           03  D2-TITLE2                   PIC X(26).
           03  FILLER                      PIC X(01).
           03  FILLER                      PIC X(07)   VALUE "PAGE : ".
           03  D2-PAGE2                    PIC 9(01).

       01  HEAD5-SINGLE.
           03  FILLER                      PIC X(8) VALUE "BRNO".
           03  FILLER                      PIC X(9) VALUE "ACCTNO".
           03  FILLER                      PIC X(10) VALUE "P&L  DATE".

       01  HEAD7.
           03  FILLER                      PIC X(46)   VALUE
               "----- LOAN ENDORSERS ------  BORROWER-SSNO".

           03  FILLER                      PIC X(51)   VALUE
               "BORROWER-NAME           ACCT-NO       LOAN-DATE".

           03  FILLER                      PIC X(35)   VALUE
               "OBLIGATED-RATE     OUTSTANDING-BAL".

       01  HEAD8.
           03  FILLER                      PIC X(52)   VALUE
               " TRAN   PAY  IB TR          TRANS    --- APPLIED ---".

           03  FILLER                      PIC X(17)   VALUE SPACES.

           03  FILLER                      PIC X(23)   VALUE
               "------- BALANCES ------".

           03  FILLER                      PIC X(38)   VALUE SPACES.

       01  HEAD9.
           03  FILLER                      PIC X(52)   VALUE
           " DATE   DATE PC CD REFNO   AMOUNT    INT/LC     PRIN".

           03  FILLER                      PIC X(42)   VALUE
           "  PDTH LCPD      OTH       INT      PRIN  ".

           03  FILLER                      PIC X(31)   VALUE
           "  EARNED  PROCEEDS      GL-DIST".

       01  HEAD-10.
           03  FILLER            PIC X(12)   VALUE "DESCRIPTION ".
           03  FILLER            PIC X(18)   VALUE "MAKE              ".
           03  FILLER            PIC X(18)   VALUE "MODEL             ".
           03  FILLER            PIC X(06)   VALUE "YEAR  ".
           03  FILLER            PIC X(10)   VALUE "STYLE     ".
           03  FILLER            PIC X(10)   VALUE "CLASS     ".
           03  FILLER            PIC X(18)   VALUE "IDENTIFICATION NO ".

       01  TEXT2A.
           03  FILLER                      PIC X(48)   VALUE
               "    CO OURS EFF DATE EXP DATE   TERM  COVERAGE".

           03  FILLER                      PIC X(16)   VALUE
               "PREMIUM    COMM.".

       01  TEXT2B.
           03  FILLER                        PIC X(54)     VALUE
               "CD DESCRIPTION        AMOUNT O  CD DESCRIPTION".

           03  FILLER                        PIC X(9)      VALUE
               "AMOUNT O".

       01  TEXT3A.
           03  FILLER                        PIC X(50)     VALUE
               " ORIG ANTIC - ADJ ANTIC = NET ANTIC  - EARNINGS".

           03  FILLER                        PIC X(23)     VALUE
               "= UNEARNED    ..UPFRONT".

       01  TEXT3B.
           03  FILLER                        PIC X(50)     VALUE
               "  --------- - --------- = ---------- - ----------".

           03  FILLER                        PIC X(23)     VALUE
               "= ----------  ---------".

       01  DASH-LINE                         PIC X(132)   VALUE ALL "-".

       01  SINGLE-DETAIL-LINE                PIC X(80)     VALUE SPACES.
       01  S-LINE1                REDEFINES SINGLE-DETAIL-LINE.
           03  S-BRNO                        PIC Z(4).
           03  FILLER                        PIC X(3).
           03  S-NUMBER                      PIC Z(6).
           03  FILLER                        PIC X(5).
           03  S-PLDATE                     PIC 99/99/99.

      *
      *    D E T A I L - L I N E - B U F F E R
      *
       01  DETAIL-LINE                      PIC X(132)     VALUE SPACES.

       01  D-LINE1                REDEFINES DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN1A.
               05  D-P1A                    PIC X(14).
               05  D-FIELD1AA               PIC X(18).
               05  D-F-1A         REDEFINES D-FIELD1AA.
                   07  D-1A-STATE           PIC X(02).
                   07  D-1A-DASH            PIC X(01).
                   07  D-1A-OWNBR           PIC X(04).
                   07  FILLER               PIC X(11).
               05  D-F-1AB        REDEFINES D-FIELD1AA.
                   07  D-1A-SSNO            PIC 999/99/9999.
                   07  FILLER               PIC X(02).
                   07  D-1A-MAKERCD         PIC X(01).
                   07  FILLER               PIC X(04).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 2
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN1B.
               05  D-P1B                    PIC X(11).
               05  D-FIELD1B                PIC X(25).
               05  D-F-1B         REDEFINES D-FIELD1B.
                   07  D-1B-STATE           PIC X(02).
                   07  D-1B-DASH            PIC X(01).
                   07  D-1B-OWNBR           PIC X(04).
                   07  FILLER               PIC X(18).
               05  D-F-1BB REDEFINES D-FIELD1B.
                   07  FILLER         PIC X(04).
                   07  D-P1B-LBX-PMPT PIC X(10).
                   07  D-P1B-LBOX     PIC X(01).
                   07  FILLER         PIC X(10).
      * COLUMN 3

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 3
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN1CD.
               05  D-COLUMN1C.
                   07  D-P1C                PIC X(18).
                   07  D-FIELD1C            PIC X(14).
                   07  D-F-1CA    REDEFINES D-FIELD1C.
                       09  D-1C-DATE        PIC 99/99/99   BLANK ZEROES.
                       09  FILLER           PIC X(06).
                   07  D-F-1CB    REDEFINES D-FIELD1C.
                       09  D-1C-RATE        PIC ZZZ.ZZZ.
                       09  FILLER           PIC X(07).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 4
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  D-COLUMN1D.
                   07  D-P1D                PIC X(18).
                   07  D-FIELD1D            PIC X(14).
                   07  D-F-1DA    REDEFINES D-FIELD1D.
                       09  D-1D-DATE        PIC 99/99/99   BLANK ZEROES.
                       09  FILLER           PIC X(06).
                   07  D-F-1DB    REDEFINES D-FIELD1D.
                       09  D-1D-RATE        PIC ZZZ.ZZZ.
                       09  FILLER           PIC X(07).

       01  D-LINE2                REDEFINES DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN2A.
               05  D-P2A                    PIC X(16).
               05  D-FIELD2AA               PIC X(16).
               05  D-F-2A         REDEFINES D-FIELD2AA.
                   07  D-2A-NOPY            PIC Z(03).
                   07  FILLER               PIC X(01).
                   07  D-2A-DASH            PIC X(01).
                   07  FILLER               PIC X(01).
                   07  D-2A-PYAMT           PIC Z(06).ZZ-.
               05  D-F-2AB        REDEFINES D-FIELD2AA.
                   07  D-2A-AMT             PIC Z(07).ZZ-.
                   07  FILLER               PIC X(05).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 2
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN2B.
               05  D-P2B                    PIC X(17).
               05  D-FIELD2B                PIC X(19).
               05  D-F-2B         REDEFINES D-FIELD2B.
                   07  D-2B-AMT             PIC Z(06).ZZ-.
                   07  FILLER               PIC X(09).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 3
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN2CD.
               05  D-COLUMN2C.
                   07  D-2C-P1              PIC X(09).
                   07  D-INSURED            PIC X(01).
                   07  FILLER               PIC X(02).
                   07  D-2C-P2              PIC X(07).
                   07  D-INSTYPES           PIC X(01).
                   07  FILLER               PIC X(01).
                   07  D-DESC1              PIC X(15).
                   07  FILLER               PIC X(01).
                   07  D-2C-P3              PIC X(10).
                   07  D-ELIG               PIC X(01).
                   07  FILLER               PIC X(01).
                   07  D-DESC2              PIC X(15).
               05  D-COLUMN2D     REDEFINES D-COLUMN2C.
                   07  D-2C-P4              PIC X(04).
                   07  D-INSCO              PIC Z(02).
                   07  FILLER               PIC X(02).
                   07  D-INSOURS            PIC X(01).
                   07  FILLER               PIC X(03).
                   07  D-INSEFF             PIC 99/99/99   BLANK ZEROES.
                   07  FILLER               PIC X(01).
                   07  D-INSEXP             PIC 99/99/99   BLANK ZEROES.
                   07  FILLER               PIC X(04).
                   07  D-INSTERM            PIC Z(03).
                   07  FILLER               PIC X(01).
                   07  D-INSCOVR            PIC Z(06).ZZ-.
                   07  D-INSPREM            PIC Z(05).ZZ-.
                   07  D-INSCOMM            PIC Z(05).ZZ.
               05  D-COLUMN2E     REDEFINES D-COLUMN2C.
                   07  D-FEECD1             PIC X(02).
                   07  FILLER               PIC X(01).
                   07  D-FEEDESC1           PIC X(16).
                   07  FILLER               PIC X(01).
                   07  D-FEEAMT1            PIC Z(05).ZZ-.
                   07  D-FEEOURS1           PIC X(01).
                   07  FILLER               PIC X(02).
                   07  D-FEECD2             PIC X(02).
                   07  FILLER               PIC X(01).
                   07  D-FEEDESC2           PIC X(16).
                   07  FILLER               PIC X(01).
                   07  D-FEEAMT2            PIC Z(05).ZZ-.
                   07  D-FEEOURS2           PIC X(01).

       01  D-LINE3                REDEFINES DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN3AB.
               05  D-COLUMN3A.
                   07  D-P3A                PIC X(17).
                   07  D-FIELD3AA           PIC X(13).
                   07  D-F-3A     REDEFINES D-FIELD3AA.
                       09  D-3A-AMT         PIC Z(07).ZZ-.
                       09  FILLER           PIC X(02).
                   07  D-F-3AB    REDEFINES D-FIELD3AA.
                       09  FILLER           PIC X(02).
                       09  D-3A-DATE        PIC 99/99/99 BLANK ZEROES.
                       09  FILLER           PIC X(03).
                   07  D-F-3AD    REDEFINES D-FIELD3AA.
                       09  FILLER           PIC X(02).
                       09  D-3A-1STPY       PIC X(08).
                       09  FILLER           PIC X(03).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 2
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  D-COLUMN3B.
                   07  D-P3B                PIC X(17).
                   07  D-FIELD3B            PIC X(12).
                   07  D-F-3BA    REDEFINES D-FIELD3B.
                       09  D-3B-AMT         PIC Z(06).ZZ-.
                       09  FILLER           PIC X(02).
                   07  D-F-3BB    REDEFINES D-FIELD3B.
                       09  D-3B-COUNT       PIC Z(09)-.
                       09  FILLER           PIC X(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 3
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN3CD.
               05  D-COLUMN3C.
                   07  D-P3C                PIC X(25).
                   07  D-3C-AMT             PIC Z(06).ZZ-.
                   07  D-3C-FLAG  REDEFINES D-3C-AMT
                                            PIC X(10).
                   07  FILLER               PIC X(38).
               05  D-COLUMN3CA    REDEFINES D-COLUMN3C.
                   07  D-3C-P1              PIC X(02).
                   07  D-ANTIC              PIC Z(06).ZZ-.
                   07  FILLER               PIC X(02).
                   07  D-ADJ                PIC Z(06).ZZ-.
                   07  FILLER               PIC X(02).
                   07  D-NET                PIC Z(06).ZZ-.
                   07  FILLER               PIC X(02).
                   07  D-EARN               PIC Z(06).ZZ-.
                   07  FILLER               PIC X(02).
                   07  D-UNEARN             PIC Z(06).ZZ-.
                   07  D-UN-ALP   REDEFINES D-UNEARN
                                            PIC X(10).
                   07  FILLER               PIC X(03).
                   07  D-UFRONT             PIC Z(06).ZZ.

       01  D-LINE4                REDEFINES DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN4A.
               05  D-P4A                    PIC X(17).
               05  D-FIELD4A                PIC X(13).
               05  D-F-4A         REDEFINES D-FIELD4A.
                   07  D-4A-AMT             PIC Z(06).ZZ-.
                   07  D-4A-COUNT REDEFINES D-4A-AMT
                                            PIC Z(09)-.
                   07  FILLER               PIC X(03).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 2
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN4B.
               05  D-P4B                    PIC X(17).
               05  D-FIELD4B                PIC X(12).
               05  D-F-4BB        REDEFINES D-FIELD4B.
                   07  D-4B-DATE            PIC 99/99/99  BLANK ZEROES.
                   07  FILLER               PIC X(04).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 3
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN4C.
               05  D-P4C                    PIC X(18).
               05  D-FIELD4C                PIC X(14).
               05  D-F-4CA        REDEFINES D-FIELD4C.
                   07  FILLER               PIC X(01).
                   07  D-4C-DATE            PIC 99/99/99   BLANK ZEROES.
                   07  FILLER               PIC X(05).
               05  D-F-4CB        REDEFINES D-FIELD4C.
                   07  D-4C-AMT             PIC Z(06).ZZ-.
                   07  D-4C-COUNT REDEFINES D-4C-AMT
                                            PIC Z(09)-.
                   07  FILLER               PIC X(04).
               05  D-F-4CC        REDEFINES D-FIELD4C.
                   07  D-4C-PROFILE         PIC 9(12).
                   07  FILLER               PIC X(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COLUMN 4
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-COLUMN4D.
               05  D-P4D                    PIC X(17).
               05  D-FIELD4D                PIC X(24).
               05  D-F-4DA        REDEFINES D-FIELD4D.
                   07  FILLER               PIC X(01).
                   07  D-4D-DATE            PIC 99/99/99 BLANK ZERO.
                   07  FILLER               PIC X(15).
               05  D-F-4DB        REDEFINES D-FIELD4D.
                   07  D-4D-AMT             PIC Z(06).ZZ-.
                   07  D-4D-COUNT REDEFINES D-4D-AMT PIC Z(9)-.
                   07  FILLER               PIC X(14).
               05  D-F-4DC        REDEFINES D-FIELD4D.
                   07  D-4D-PROFILE         PIC 9(12).
                   07  FILLER               PIC X(12).

       01  D-LINE5                REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(29).
           03  D-MEM-TAG1                   PIC X(08).
           03  D-MEM-DATE                   PIC 99/99/99.
           03  FILLER                       PIC X(05).
           03  D-MEM-TAG2                   PIC X(07).
           03  D-MEM-SEQ                    PIC Z9.
           03  FILLER                       PIC X(11).
           03  D-MEMO1                      PIC X(30).
           03  D-MEMO2                      PIC X(32).

       01  D-LINE6                REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(29).
           03  DLE-SSNO                     PIC 999/99/9999.
           03  FILLER                       PIC X(06).
           03  DLE-NAME                     PIC X(20).
           03  FILLER                       PIC X(04).
           03  DLE-OSACCTNO                 PIC 9(08).
           03  FILLER                       PIC X(06).
           03  DLE-OSDATE                   PIC 99/99/99.
           03  FILLER                       PIC X(14).
           03  DLE-OSRATE                   PIC ZZZ.ZZZ.
           03  FILLER                       PIC X(08).
           03  DLE-OSAMT                    PIC ZZZ,ZZZ.ZZ-.

       01  D-LINE7                REDEFINES DETAIL-LINE.
           03  DLP-TRDATE                   PIC 9(06).
           03  FILLER                       PIC X(01).
           03  DLP-PAYDATE                  PIC 9(06).
           03  FILLER                       PIC X(01).
           03  DLP-IBPC                     PIC X(01).
           03  FILLER                       PIC X(01).
           03  DLP-TRCD                     PIC X(02).
           03  FILLER                       PIC X(01).
           03  DLP-REFNO                    PIC X(05).
           03  DLP-TRAMT                    PIC ZZZZZZ.ZZ-.
           03  DLP-TRAMT-RATE     REDEFINES DLP-TRAMT
                                            PIC ZZZZZ.ZZZ-.
           03  DLP-APINT                    PIC ZZZZZZ.ZZ-.
           03  DLP-APCUR                   PIC ZZZZZZ.ZZ-.
           03  DLP-PDTH.
               05 DLP-PDTH-MO               PIC 9(02).
               05 DLP-PDTH-DAYR             PIC 9(02).
           03  FILLER                       PIC X(01).
           03  DLP-LCPDTH.
               05 DLP-LCPDTH-MO             PIC 9(02).
               05 DLP-LCPDTH-DAYR           PIC 9(02).
           03  DLP-OTHBAL                   PIC ZZZZZZ.ZZ-.
           03  DLP-INTBAL                   PIC ZZZZZZ.ZZ-.
           03  DLP-CURBAL                  PIC ZZZZZZ.ZZ.
           03  DLP-EARNED                   PIC ZZZZZZ.ZZ-.
           03  DLP-DLPROC                   PIC ZZZZZZ.ZZ-.
           03  FILLER                       PIC X(01).
           03  DLX-GLNO                     PIC ZZZ9/9999999.

       01  D-LINE8                REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(122).
           03  DLX-GLAMT                    PIC ZZZZZ.ZZ-.

       01  D-LINE9                REDEFINES DETAIL-LINE.
           03  D-DESCRIPTION                PIC X(10).
           03  FILLER                       PIC X(02).
           03  D-MAKE                       PIC X(16).
           03  FILLER                       PIC X(02).
           03  D-MODEL                      PIC X(16).
           03  FILLER                       PIC X(02).
           03  D-YEAR                       PIC Z(04).
           03  FILLER                       PIC X(02).
           03  D-STYLE                      PIC X(08).
           03  FILLER                       PIC X(02).
           03  D-LTCLASS                    PIC X(08).
           03  FILLER                       PIC X(02).
           03  D-ID                         PIC X(20).

       01  RANGE-LINE             REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(30).
           03  RANGE-SELECTION.
               04  R-BRANCH                 PIC Z(10).
               04  R-GROUP        REDEFINES R-BRANCH
                                            PIC X(10).
               04  FILLER                   PIC X(3).
               04  R-END-BRANCH             PIC Z(10).
               04  R-END-GROUP    REDEFINES R-END-BRANCH
                                            PIC X(10).
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CREAIXW.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/PLPURG_DEF.CPY".
       COPY "LIBLP/PLPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/PLPURG_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-RX1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTL1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LSR1-FILE.
           PERFORM CLOSE-LSP1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LSL1-FILE.
           PERFORM CLOSE-LSJ1-FILE.
           PERFORM CLOSE-LSG1-FILE.
           PERFORM CLOSE-LSF1-FILE.
           PERFORM CLOSE-LSD1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PLPURG"      TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           PERFORM GET-GPENV.

       BEGIN-GROUP-LOCALITY.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *
      *    N E X T - G R O U P - L O C A L I T Y
      *
       NEXT-GROUP-LOCALITY.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO END-MAIN-MODULE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9  OR BR-NO > END-BRANCH
              GO TO END-MAIN-MODULE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE
                                              LE-PATH-OVERRIDE
                                              LH-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LSD-PATH-OVERRIDE
                                              LSF-PATH-OVERRIDE
                                              LSG-PATH-OVERRIDE
                                              LSJ-PATH-OVERRIDE
                                              LSL-PATH-OVERRIDE
                                              LSM-PATH-OVERRIDE
                                              LSP-PATH-OVERRIDE
                                              LSR-PATH-OVERRIDE
                                              LSX-PATH-OVERRIDE
                                              LTA-PATH-OVERRIDE
                                              LTB-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              LTF-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTL-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              LX-PATH-OVERRIDE
                                              LXE-PATH-OVERRIDE
                                              LXG-PATH-OVERRIDE
                                              RX-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THERE IS NO IO ROUTINES FOR THE IX FILE.  SO HARDCODED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FILPATH-BASE               TO IX-ALL-BASE.
           MOVE FILPATH-MACHINE            TO IX-ALL-MACHINE.
           MOVE FILPATH-DATA               TO IX-ALL-DATA.
           MOVE IX-ALL-PATH                TO IX-PATH    CREAIX-IX-PATH.
           MOVE "Y"                        TO IX-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-NO1-FILE.
           PERFORM OPEN-LE1-FILE.
           PERFORM OPEN-LH1-FILE.
           PERFORM OPEN-LM1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LX1-FILE.
           PERFORM OPEN-LXE1-FILE.
           PERFORM OPEN-LXG1-FILE.
           PERFORM OPEN-RX1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PURGE LOAN RECORDS THAT FALL INTO RANGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LOAN-PURGE-MODULE.

       END-OF-GROUP.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-RX1-FILE.

           IF ( FIRST-TIME = "N" )
              MOVE "Y" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE ZEROES TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOCALITY.

       END-MAIN-MODULE.

           IF ( NO-RECORDS-FOUND )
              GO TO EXIT-MAIN-MODULE.

           PERFORM PRINT-RANGE-LINES.

       EXIT-MAIN-MODULE.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE PLPURG-QUEUE-POS TO Q-POS.
           MOVE PLPURG-COPIES-POS TO C-POS.
           MOVE PLPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF ( EXT-EOBUF-ERRCD = "E" ) GO TO END-INITIALIZATION.

           MOVE ZEROES TO BR-NO.
           PERFORM SEND-PROGRESS-LINE-WK.

           MOVE "N" TO RECORDS-FOUND.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SAVE-BEG-BRANCH
                 MOVE END-BRANCH TO SAVE-END-BRANCH
                 MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-CDB1-FILE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.

           GO TO EXIT-INITIALIZE.

       END-INITIALIZATION.

           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-JULIAN-BEG  TO BEG-PL-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-PL-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       BEGIN-ENTER-PARAMETERS.

           IF ( UP-KEY ) GO TO ENTER-PRINT-PAYMENTS.

       ENTER-ACTION-PRB.
           MOVE "E  A010ACT." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-ACTION-PRB.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION-VALID ) GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-ACTION-PRB.

       ENTER-BEG-BRANCH.

           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCHS.

           IF ( F6-KEY )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BEG-BRANCH
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-ACTION-PRB.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"           TO GROUP-FLAG
              MOVE 0             TO BEG-BRANCH
              MOVE 9999          TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-END-BRANCH.

           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BRANCH-SECURITY.

       ENTER-ALL-BRANCHS.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

       ENTER-BEG-CLASS.

           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF ( F6-KEY ) MOVE "BEG" TO CLASS-FG
                         PERFORM CLASS-SCAN
                         IF ( IO-FG NOT = ZEROES )
                            GO TO ENTER-BEG-CLASS.

           IF (                    F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    F3-KEY ) GO TO ENTER-ALL-CLASSES.
           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-END-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

       ENTER-END-CLASS.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF ( F6-KEY ) MOVE "END" TO CLASS-FG
                         PERFORM CLASS-SCAN
                         IF ( IO-FG NOT = ZEROES )
                            GO TO ENTER-END-CLASS.

           IF (                    F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-CLASS.

           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-ACCT.

       ENTER-ALL-CLASSES.
           MOVE "SNNN020CL1." TO VDU.
           MOVE ZEROES        TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-ACCT.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    F3-KEY ) GO TO ENTER-ALL-ACCTS.
           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-END-CLASS.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-ACCT.

           MOVE VDUNUM0 TO BEG-ACCT.

       ENTER-END-ACCT.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    F3-KEY ) GO TO ENTER-ALL-ACCTS.
           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-BEG-ACCT.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-ACCT.

           MOVE VDUNUM0 TO END-ACCT.

           IF ( BEG-ACCT > END-ACCT )
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCT.

           GO TO ENTER-BEG-DATE.

       ENTER-ALL-ACCTS.
           MOVE "SNNN060AC1." TO VDU.
           MOVE ZEROES        TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060AC2." TO VDU.
           MOVE 999999        TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-BEG-PAY-DATE.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F3-KEY ) GO TO ENTER-ALL-DATES.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( UP-KEY ) GO TO ENTER-END-ACCT.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-PL-DATE.

       ENTER-END-DATE.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-BEG-DATE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD     TO END-PL-DATE.

           IF END-PL-DATE < BEG-PL-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-PAY-DATE.

       ENTER-ALL-DATES.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG    TO BEG-PL-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END    TO END-PL-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-BEG-PAY-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO CHECK-ENTER-ACTION-CODE.

           MOVE "ERNM060PAYDT1." TO VDU.
           PERFORM SCREENIO.

           IF (  F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (  F3-KEY ) GO TO ENTER-ALL-PAY-DATES.
           IF (  F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (  UP-KEY ) GO TO ENTER-END-DATE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-PAY-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-PAY-DATE.

       ENTER-END-PAY-DATE.
           MOVE "ERNM060PAYDT2." TO VDU.
           PERFORM SCREENIO.

           IF (  F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (  F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (  UP-KEY ) GO TO ENTER-BEG-PAY-DATE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-PAY-DATE.

           MOVE VDU-DATE-YYYYMMDD     TO END-PAY-DATE.

           IF END-PAY-DATE < BEG-PAY-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-PAY-DATE.

           GO TO CHECK-ENTER-ACTION-CODE.

       ENTER-ALL-PAY-DATES.
           MOVE "S  8080PAYDT1." TO VDU.
           MOVE EXT-JULIAN-BEG    TO BEG-PAY-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080PAYDT2." TO VDU.
           MOVE EXT-JULIAN-END    TO END-PAY-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       CHECK-ENTER-ACTION-CODE.

           IF ( ACTION NOT = "P" ) GO TO ENTER-PRINT-MEMOS-YN.
           IF ( ACTION NOT = "L" ) GO TO ENTER-PRINT-MEMOS-YN.

           MOVE "S  A010PRMEMO." TO VDU.
           MOVE "N"              TO PRINT-MEMOS VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PREND." TO VDU.
           MOVE "N"             TO PRINT-END VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRPAYMENT." TO VDU.
           MOVE "N"                 TO PRINT-PAYMENTS VDUBUF.
           PERFORM SCREENIO.

           GO TO ENTER-ACCEPT-YN.

       ENTER-PRINT-MEMOS-YN.

           MOVE "E  A010PRMEMO." TO VDU.
           PERFORM SCREENIO.

           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-END-PAY-DATE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-MEMOS-YN.

           MOVE VDUBUF TO PRINT-MEMOS.

           IF ( PRINT-MEMOS     = "Y" ) GO TO ENTER-PRINT-END-YN.
           IF ( PRINT-MEMOS NOT = "N" ) GO TO ENTER-PRINT-MEMOS-YN.

       ENTER-PRINT-END-YN.

           MOVE "E  A010PREND." TO VDU.
           PERFORM SCREENIO.

           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-PRINT-MEMOS-YN.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-END-YN.

           MOVE VDUBUF TO PRINT-END.

           IF ( PRINT-END     = "Y" ) GO TO ENTER-PRINT-PAYMENTS.
           IF ( PRINT-END NOT = "N" ) GO TO ENTER-PRINT-END-YN.

       ENTER-PRINT-PAYMENTS.

           MOVE "E  A010PRPAYMENT." TO VDU.
           PERFORM SCREENIO.

           IF (                    F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-PRINT-END-YN.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-PAYMENTS.

           MOVE VDUBUF TO PRINT-PAYMENTS.

           IF ( PRINT-PAYMENTS     = "Y" ) GO TO ENTER-ACCEPT-YN.
           IF ( PRINT-PAYMENTS NOT = "N" ) GO TO ENTER-PRINT-PAYMENTS.

      D    STOP MESS.
       ENTER-ACCEPT-YN.

           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

       EXIT-ENTER-PARAMETERS.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF ( GROUP-FLAG = "Y" )
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACT." TO VDU.
           MOVE ACTION        TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060AC1." TO VDU.
           MOVE BEG-ACCT      TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE END-ACCT      TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-PL-DATE   TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE END-PL-DATE   TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080PAYDT1." TO VDU.
           MOVE BEG-PAY-DATE   TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080PAYDT2." TO VDU.
           MOVE END-PAY-DATE   TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRMEMO." TO VDU.
           MOVE PRINT-MEMOS      TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PREND." TO VDU.
           MOVE PRINT-END       TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRPAYMENT." TO VDU.
           MOVE PRINT-PAYMENTS      TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-DISPLAY-PARAMETERS.
           EXIT.

      ******************************************************************
       LOAN-PURGE-MODULE SECTION.
      ******************************************************************

           PERFORM SEND-PROGRESS-LINE-WK.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE BEG-ACCT  TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE END-ACCT  TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN-RECORD.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-LOAN-PURGE-MODULE.

           IF ( LN-CLASS < BEG-CLASS ) OR
                ( LN-CLASS > END-CLASS )
              GO TO NEXT-LN-RECORD.

           IF ( LN-ACCTNO < BEG-ACCT ) OR
                ( LN-ACCTNO > END-ACCT )
              GO TO NEXT-LN-RECORD.

      * CHECK IF LOAN RECORD IS PURGABLE
           PERFORM CHECK-PURGABILITY.
           IF ( NOT PURGE-OK )
      *       UNLOCK LN-FILE
              GO TO NEXT-LN-RECORD.

      * CHECK IF TO PRINT REPORT AND/OR PURGING LOAN RECORD(S)
           IF ( ACTION-BOTH   ) OR
              ( ACTION-REPORT )
              PERFORM COMPUTE-EARNINGS
              PERFORM DETAIL-LINE-PROCESS.

           IF ACTION-SINGLE-LINE
              MOVE "N" TO FIRST-TIME
              PERFORM SINGLE-LINE-PROCESS
              GO TO NEXT-LN-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM PURGE-LOAN-MEMOS.
           PERFORM PURGE-LOAN-ENDORSERS.
           PERFORM PURGE-LOAN-PAYMENTS.
           PERFORM PURGE-LX-RECORDS.
           PERFORM PURGE-LXG-RECORDS.
           PERFORM PURGE-LXE-RECORDS.
           PERFORM PURGE-LS-RECORDS.
           PERFORM PURGE-LT-RECORDS.

      * CHECK IF TO PRINT REPORT AND/OR PURGING LOAN RECORD(S)
           IF ( ACTION-BOTH  ) OR
              ( ACTION-PURGE )
              PERFORM PURGE-LOAN-PROCESS.

           MOVE "N" TO FIRST-TIME.

           GO TO NEXT-LN-RECORD.

       EXIT-LOAN-PURGE-MODULE.
           EXIT.

      ******************************************************************
       CHECK-PURGABILITY SECTION.
      ******************************************************************

           MOVE "Y" TO PURGE-FLAG.

      * THIS PURGE IS FOR P&L'S ONLY !!!!
           IF LN-PLDATE = 0
              MOVE "P" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

      * VERIFY P&L DATE IS BETWEEN BEGINNING AND ENDING DATE
           IF LN-PLDATE < BEG-PL-DATE
              MOVE "D" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

           IF LN-PLDATE > END-PL-DATE
              MOVE "D" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

           IF LN-DATE-PAID-LAST = 0
              MOVE LN-LOANDATE TO LN-DATE-PAID-LAST.
           IF LN-DATE-PAID-LAST < BEG-PAY-DATE
              MOVE "D" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

           IF LN-DATE-PAID-LAST > END-PAY-DATE
              MOVE "D" TO PURGE-FLAG
              GO TO EXIT-CHECK-PURGABILITY.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           IF ( LPWSSP-SP1-KEY NOT = SP1-KEY )
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF ( IO-FG NOT = ZEROES )
                 MOVE "S" TO PURGE-FLAG
                 GO TO EXIT-CHECK-PURGABILITY.


       EXIT-CHECK-PURGABILITY.
           EXIT.


      ******************************
       SINGLE-LINE-PROCESS SECTION.
      ******************************

           IF ( LN-COUNT > 60 )
               PERFORM HEAD-PAGE
               MOVE HEAD5-SINGLE TO PR-REC
               MOVE 1     TO LN-FEED
               PERFORM WRITE-PR-REC.

           MOVE BR-NO     TO S-BRNO.
           MOVE LN-ACCTNO TO S-NUMBER.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO S-PLDATE.

           MOVE SINGLE-DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED      TO LN-COUNT.
           MOVE 1           TO LN-FEED.
           MOVE SPACES      TO SINGLE-DETAIL-LINE.

           ADD 1 TO PURGE-COUNT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           PERFORM HEAD-PAGE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE "BORROWERS AND STATUS CODES" TO D-TITLE.
           MOVE 1 TO D-PAGE.
           MOVE "DATES AND RATES" TO D-TITLE2.
           MOVE 2 TO D-PAGE2.
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "ORIG ST/BR  :" TO D-P1A.
           MOVE LN-ORGST TO D-1A-STATE.
           MOVE "-" TO D-1A-DASH.
           MOVE LN-ORGBR TO D-1A-OWNBR.
           MOVE "LOAN STATUS CODES :" TO D-COLUMN1B.
           MOVE "ORIGINAL DATES:" TO D-P1C.
           MOVE "STATUS CHANGE DATES:" TO D-COLUMN1D.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CLASS       :" TO D-P1A.
           MOVE LN-CLASS TO D-F-1A.
           MOVE "OWN ST/BR:" TO D-P1B.
           MOVE LN-OWNST TO D-1B-STATE.
           MOVE "-" TO D-1B-DASH.
           MOVE LN-OWNBR TO D-1B-OWNBR.
           MOVE "ENTERED DATE    :" TO D-P1C.
           MOVE LN-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "PAID OFF DATE   :" TO D-P1D.
           MOVE LN-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LAW CODE    :" TO D-P1A.
           MOVE LN-LAWCODE TO D-1A-STATE.
           MOVE "PURCHASED DATE  :" TO D-P1C.
           MOVE LN-PURDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "JUDGEMENT DATE  :" TO D-P1D.
           MOVE LN-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "             " TO D-P1A.
           MOVE SPACES TO D-1A-STATE.
           MOVE "PURP CODE:" TO D-P1B.
           MOVE LN-PURCD TO D-1B-STATE.
           MOVE "LOAN DATE       :" TO D-P1C.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "P & L DATE      :" TO D-P1D.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COLAT CD :" TO D-P1B.
           MOVE LN-COLLCD TO D-1B-STATE.
           MOVE "ORG 1ST PAY DATE:" TO D-P1C.
           MOVE LN-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "ACCEL DATE      :" TO D-P1D.
           MOVE LN-ACCELDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BORROWER INFORMATION:" TO D-COLUMN1A.
           MOVE "COLAT ID :" TO D-P1B.
           MOVE LN-COLLID TO D-F-1B.
           MOVE "DATE INT START  :" TO D-P1C.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "RATE CHANGE DATE:" TO D-P1D.
           MOVE LN-REDUDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BORR #1  CD :" TO D-P1A.
           MOVE 1 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "SOURCE CD:" TO D-P1B.
           MOVE LN-SRCD TO D-1B-STATE.
           MOVE "FILING DATE     :" TO D-P1C.
           MOVE LN-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "LEGAL ACT DATE  :" TO D-P1D.
           MOVE LN-LEGALDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "SAME AS CASH    :" TO D-P1C.
           MOVE LN-CASHDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "BANKRUPT DATE   :" TO D-P1D.
           MOVE LN-BNKRPTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO BW-FNAME BW-LNAME.
           MOVE "BORR #2  CD :" TO D-P1A.
           MOVE 2 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "CONFIDENT:" TO D-P1B.
           MOVE LN-CONFIDENTCD TO D-1B-STATE.
           MOVE "DEBT AID DATE   :" TO D-P1D.
           MOVE LN-DEBTAIDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "ALLOTMENT:" TO D-P1B.
           MOVE LN-ALLOTCD TO D-1B-STATE.
           MOVE "LOCK BOX: " TO D-P1B-LBX-PMPT.
           MOVE LN-LBOX TO D-P1B-LBOX.
           MOVE "REPOSESSION DT  :" TO D-P1D.
           MOVE LN-REPODATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO BW-FNAME BW-LNAME.
           MOVE "BORR #3  CD :" TO D-P1A.
           MOVE 3 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "CONV ACCT:" TO D-P1B.
           MOVE LN-CONVERCD TO D-1B-STATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "MIDSTREAM:" TO D-P1B.
           MOVE LN-MIDSTREAM TO D-1B-STATE.
           STRING "-----------------------!","  RATE INFORMATION !",
                  "-------------------!" DELIMITED BY "!"
                  INTO D-COLUMN1CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO BW-FNAME BW-LNAME.
           MOVE "BORR #4  CD :" TO D-P1A.
           MOVE 4 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "          " TO D-P1B.
           MOVE SPACES TO D-1B-STATE.
           MOVE "A.P.R            :" TO D-P1C.
           MOVE LN-APRATE TO D-1C-RATE.
           MOVE "JUDGEMENT RATE   :" TO D-P1D.
           MOVE LN-JDRATE TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "RECAST CD:" TO D-P1B.
           MOVE LN-RECASTCD TO D-1B-STATE.
           MOVE "EFFECT INT RATE  :" TO D-P1C.
           MOVE LN-EFFRATE TO D-1C-RATE.
           MOVE "ACCEL RATE       :" TO D-P1D.
           MOVE LN-ACCRATE TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SIMPLE RATE      :" TO D-P1C.
           MOVE LN-SMPRATE TO D-1C-RATE.
           MOVE "RATE CHANGE/REDUC:" TO D-P1D.
           MOVE LN-REDURATE TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN STATUS CHANGES:" TO D-COLUMN1A.
           MOVE "P & L CD :" TO D-P1B.
           MOVE LN-PLCD TO D-1B-STATE.
           MOVE "OVERRIDING RATE  :" TO D-P1C.
           MOVE LN-OVRDRATE TO D-1C-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STATUS FLAGS:" TO D-P1A.
           MOVE LN-STATFLAGS TO D-FIELD1AA.
           MOVE "PAYOFF CD:" TO D-P1B.
           MOVE LN-POCD TO D-1B-STATE.
           MOVE "ENDORSER RATE    :" TO D-P1C.
           MOVE LN-ENDORATE TO D-1C-RATE.
           MOVE "SERVICE PERCENT :" TO D-P1D.
           MOVE LN-SERVPER TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LAST STATCHG:" TO D-P1A.
           MOVE LN-STATUSFG TO D-1A-STATE.
           MOVE "P&L TYPE :" TO D-P1B.
           MOVE LN-PLTYPE TO D-1B-STATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE "GENERAL LOAN INFORMATION  " TO D-TITLE.
           MOVE 3 TO D-PAGE.
           MOVE "INSURANCE AND FEES" TO D-TITLE2.
           MOVE 4 TO D-PAGE2.
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "GENERAL LOAN INFORMATION:" TO D-COLUMN2A.
           MOVE "LOAN FINANCE CHARGES:" TO D-COLUMN2B.
           STRING "-------------------!"," INSURANCE INFORMATION -----!",
                  "----------------!" DELIMITED BY "!" INTO D-COLUMN2CD.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SELLING PRICE :" TO D-P2A.
           MOVE LN-SELLPR TO D-2A-AMT.
           MOVE "FINANCE CHARGE :" TO D-P2B.
           MOVE LN-FINCHG TO D-2B-AMT.
           MOVE "INSURED:" TO D-2C-P1.
           MOVE LN-INSURED TO D-INSURED.
           MOVE "TYPE :" TO D-2C-P2.
           MOVE LN-INSTYPES TO D-INSTYPES.
           IF LN-INSTYPES = "B"
              MOVE " LL" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "C"
              MOVE " DL" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "D"
              MOVE " A&H" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "E"
              MOVE " OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "F"
              MOVE " LL AND A&H" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "G"
              MOVE " DL AND A&H" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "H"
              MOVE " LL AND OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "J"
              MOVE " DL AND OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "K"
              MOVE " A&H AND OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "L"
              MOVE " LL, A&H, OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "M"
              MOVE " DL, A&H, OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "N"
              MOVE " NONE" TO D-DESC1.
           MOVE "ELIGABLE:" TO D-2C-P3.
           MOVE LN-INSELIGIBLE TO D-ELIG.
           IF LN-INSELIGIBLE = "B"
              MOVE " LL" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "C"
              MOVE " DL" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "D"
              MOVE " A&H" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "E"
              MOVE " OTHER" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "F"
              MOVE " LL AND A&H" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "G"
              MOVE " DL AND A&H" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "H"
              MOVE " LL AND OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "J"
              MOVE " DL AND OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "K"
              MOVE " A&H AND OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "L"
              MOVE " LL, A&H, OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "M"
              MOVE " DL, A&H, OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "N"
              MOVE " NONE" TO D-DESC2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DOWN PAYMENT  :" TO D-P2A.
           MOVE LN-DOWNPY TO D-2A-AMT.
           MOVE "INTEREST CHG.  :" TO D-P2B.
           MOVE LN-INTCHG TO D-2B-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN AMOUNT   :" TO D-P2A.
           MOVE LN-LNAMT TO D-2A-AMT.
           MOVE "1ST PAY EXT CHG:" TO D-P2B.
           MOVE LN-EXTCHG TO D-2B-AMT.
           MOVE TEXT2A TO D-COLUMN2CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIGINAL TERM :" TO D-P2A.
           MOVE LN-ORGTERM TO D-2A-NOPY.
           MOVE "SERVICE CHARGE :" TO D-P2B.
           MOVE LN-SERCHG TO D-2B-AMT.
           MOVE "CL:" TO D-2C-P4.
           MOVE 1 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAINT. FEE     :" TO D-P2B.
           MULTIPLY LN-MAINTFEE BY LN-ORGTERM GIVING D-2B-AMT.
           MOVE "AH:" TO D-2C-P4.
           MOVE 2 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REG PAYMENTS  :" TO D-P2A.
           MOVE LN-NOREGPY TO D-2A-NOPY.
           MOVE "@" TO D-2A-DASH.
           MOVE LN-REGPYAMT TO D-2A-PYAMT.
           MOVE "PP:" TO D-2C-P4.
           MOVE 3 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "1ST PAYMENT   :" TO D-P2A.
           MOVE LN-1STPYAMT TO D-2A-PYAMT.
           MOVE "RELATED FIN CHARGE FIELDS:" TO D-COLUMN2B.
           MOVE "O1:" TO D-2C-P4.
           MOVE 4 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "POINTS         :" TO D-P2B.
           MOVE "O2:" TO D-2C-P4.
           MOVE 5 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LAST PAYMENT  :" TO D-P2A.
           MOVE LN-LASTPYAMT TO D-2A-AMT.
           MOVE "N/A            :" TO D-P2B.
           STRING "-------------------!",
            " FEE INFORMATION -----!","--------------------!"
                DELIMITED BY "!" INTO D-COLUMN2CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CASH PROCEEDS :" TO D-P2A.
           MOVE LN-PROCASH TO D-2A-AMT.
           MOVE "STATE FLAG     :" TO D-P2B.
           MOVE LN-STATE-FLAG TO D-FIELD2B.
           MOVE TEXT2B TO D-COLUMN2CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CASH TILL #   :" TO D-P2A.
           MOVE LN-TILLNO TO D-FIELD2AA.
           MOVE "STATE WORKER   :" TO D-P2B.
           MOVE LN-STATE-WORKER TO D-2B-AMT.
           MOVE 1 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 6 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRADE IN      :"  TO D-P2A.
           MOVE LN-TRADEIN         TO D-2A-AMT.
           MOVE "STATE WORKER 2 :" TO D-P2B.
           MOVE LN-STATE-WORKER2   TO D-2B-AMT.
           MOVE 2 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 7 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 3 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 8 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 4 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 9 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 5 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 10 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LPS-DISPLAY-LNNO TO D2-LOAN-NUM.
           MOVE "CUR. INFO / TOTALS     " TO D2-TITLE.
           MOVE 5 TO D2-PAGE.
           MOVE "ANTIC EARNINGS INFO" TO D2-TITLE2.
           MOVE 6 TO D2-PAGE2.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "CURRENT LOAN INFORMATION:" TO D-COLUMN3A.
           MOVE "CURRENT LOAN WORKERS:" TO D-COLUMN3B.
           MOVE TEXT3A TO D-COLUMN3CD.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CURRENT BAL    :" TO D-P3A.
           MOVE LN-CURBAL TO D-3A-AMT.
           MOVE "LCPAID WORKER  :" TO D-P3B.
           MOVE LN-LCPAID TO D-3B-AMT.
           MOVE 0 TO SUB.
           MOVE "I:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST BAL   :" TO D-P3A.
           MOVE LN-INTBAL TO D-3A-AMT.
           MOVE "LCPARTIAL      :" TO D-P3B.
           MOVE LN-LCPARTIALS TO D-3B-AMT.
           MOVE "E:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG BAL   :" TO D-P3A.
           MOVE LN-LCBAL TO D-3A-AMT.
           MOVE "STATEMENT CNTR :" TO D-P3B.
           MOVE LN-STMT-COUNTER TO D-3B-COUNT.
           MOVE "S:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER    BAL   :" TO D-P3A.
           MOVE LN-OTHBAL TO D-3A-AMT.
           MOVE "M:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG PDTH  :" TO D-P3A.
           MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-3A-DATE.
           MOVE "D:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST PDTH  :" TO D-P3A.
           MOVE LN-INTPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-3A-DATE.
           MOVE "RECAST INTEREST:" TO D-P3B.
           MOVE LN-RECASTINT TO D-3B-AMT.
           MOVE "O:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CUR 1ST DUE    :" TO D-P3A.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-3A-1STPY.
           MOVE "RECAST PRIN    :" TO D-P3B.
           MOVE LN-RECASTPRIN TO D-3B-AMT.
           MOVE TEXT3B TO D-COLUMN3CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONIES TO PDTH :" TO D-P3B.
           MOVE LN-TOTPAYMNTD TO D-3B-AMT.
           MOVE "TOTALS :" TO D-COLUMN3CD.
           PERFORM WRITE-DETAIL-LINE.

           COMPUTE D-ANTIC = EARN-TOT-ANTICERN + EARN-TOT-ANTICADJ.
           MOVE EARN-TOT-ANTICADJ TO D-ADJ.
           MOVE EARN-TOT-ANTICERN TO D-NET.
           MOVE EARN-TOT-ACCERN TO D-EARN.
           MOVE EARN-TOT-UNEARN TO D-UNEARN.
           MOVE EARN-TOT-UFERN TO D-UFRONT.
           PERFORM WRITE-DETAIL-LINE.

           STRING "---------------------- LOAN",
             " TOTALS ---------------------!" DELIMITED BY "!"
                  INTO D-COLUMN3AB.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EARNINGS       :" TO D-P3A.
           MOVE LN-TOTERN TO D-3A-AMT.
           MOVE "OTHER CHGS COLL:" TO D-P3B.
           MOVE LN-TOTOTH TO D-3B-AMT.
           MOVE "PROR MONTHEND UNEARNED:" TO D-P3C.
           MOVE LN-PRIOR-UNEARNED TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ANTIC INCOME   :" TO D-P3A.
           IF GP-INCL-OTH-IN-EARNED-N
              COMPUTE D-3A-AMT = LN-ACCERN(1) + LN-ACCERN(2) +
                                 LN-ACCERN(3) + LN-ACCERN(4) +
                                 LN-ACCERN(5)
           ELSE
           IF GP-INCL-OTH-IN-EARNED-R
              COMPUTE D-3A-AMT = LN-ACCERN(1) + LN-ACCERN(2) +
                                 LN-ACCERN(3) + LN-ACCERN(5)
           ELSE
              COMPUTE D-3A-AMT = LN-ACCERN(1) + LN-ACCERN(2) +
                                 LN-ACCERN(3) + LN-ACCERN(4) +
                                 LN-ACCERN(5) + LN-ACCERN(6).

           MOVE "NUMBER DEFER   :" TO D-P3B.
           MOVE LN-TOTNODEF TO D-3B-COUNT.
           MOVE "ACCR EARN DELIQ STOP CD:" TO D-P3C.
           MOVE LN-ERNDELQ-STOP TO D-3C-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST COLLCT:" TO D-P3A.
           MOVE LN-TOTINT TO D-3A-AMT.
           MOVE "YTD NUMBER DEF :" TO D-P3B.
           MOVE LN-YTDNODEF TO D-3B-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG COLLCT:" TO D-P3A.
           MOVE LN-TOTLCHG TO D-3A-AMT.
           MOVE "INTEREST PAID  :" TO D-P3B.
           MOVE LN-TOTINTPD TO D-3B-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEFER COLLECTED:" TO D-P3A.
           MOVE LN-TOTDEF TO D-3A-AMT.
           MOVE "YTD INT. PAID  :" TO D-P3B.
           MOVE LN-YTDINTPD TO D-3B-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERVACCT INCOME:" TO D-P3A.
           MOVE LN-TOTSERACCT TO D-3A-AMT.
           MOVE "LYR INT. PAID  :" TO D-P3B.
           MOVE LN-LYRINTPD TO D-3B-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST WAIVER:" TO D-P3A.
           MOVE LN-INTWAIVERS TO D-3A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG WAIVER:" TO D-P3A.
           MOVE LN-LCWAIVERS TO D-3A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM HEAD-PAGE.

           MOVE 2 TO LN-FEED.
           MOVE LPS-DISPLAY-LNNO TO D2-LOAN-NUM.
           MOVE "SALES FIN. AND REBATES " TO D2-TITLE.
           MOVE 7 TO D2-PAGE.
           MOVE "COLLECT/SOLICIT/PRIOR" TO D2-TITLE2.
           MOVE 8 TO D2-PAGE2.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 2 TO LN-FEED.

           MOVE "SALES FINANCE PURCHASE INFO:" TO D-COLUMN4A.
           MOVE "REFUND INFORMATION:" TO D-COLUMN4B.
           MOVE "COLLECTION INFORMATION:" TO D-COLUMN4C.
           MOVE "SOLICITATION INFORMATION" TO D-COLUMN4D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER NUMBER  :" TO D-P4A.
           MOVE LN-DLNO TO D-4A-AMT.
           MOVE "TRW DOFD       :" TO D-P4B.
           MOVE LN-TRW-DOFD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-4B-DATE.
           MOVE "COLLECTOR'S ID  :" TO D-P4C.
           MOVE LN-COLLECTOR TO D-FIELD4C.
           MOVE "HIGH CREDIT LIM:" TO D-P4D.
           MOVE LN-CREDITLIM TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RESERVE AMOUNT :" TO D-P4A.
           MOVE LN-DLRVPRIN(1) TO D-4A-AMT.
           MOVE "ACTION CODE     :" TO D-P4C.
           MOVE LN-ACTIONCD TO D-FIELD4C.
           MOVE "CREDIT SCORE NO:" TO D-P4D.
           MOVE LN-CRSCORE TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SPECIAL RESERVE:" TO D-P4A.
           MOVE LN-DLRVPRIN(2) TO D-4A-AMT.
           MOVE "CL REBATE FLAG :" TO D-P4B.
           MOVE LN-CL-REBATE TO D-FIELD4B.
           MOVE "CONTRACT PROFILE:" TO D-P4C.
           MOVE LN-CONPROFILE(1) TO D-4C-PROFILE.
           INSPECT D-4C-PROFILE REPLACING ALL '9' BY '-'.
           MOVE "LAST FORM SENT :" TO D-P4D.
           MOVE LN-LASTSFORMNO TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PARTICIPATE RSV:" TO D-P4A.
           MOVE LN-DLPARVHELD TO D-4A-AMT.
           MOVE "AH REBATE FLAG :" TO D-P4B.
           MOVE LN-AH-REBATE TO D-FIELD4B.
           MOVE "RECENCY  PROFILE:" TO D-P4C.
           MOVE LN-RECPROFILE(1) TO D-4C-PROFILE.
           INSPECT D-4C-PROFILE REPLACING ALL '9' BY '-'.
           MOVE "DATE FORM SENT :" TO D-P4D.
           MOVE LN-LASTSFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-4D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "DEALER COMM    :" TO D-P4A
           ELSE
              MOVE "DEALER DISCOUNT:" TO D-P4A.
           MOVE LN-DLDISC TO D-4A-AMT.
           MOVE "PP REBATE FLAG :" TO D-P4B.
           MOVE LN-PP-REBATE TO D-FIELD4B.
           MOVE "PROMISED DATE   :" TO D-P4C.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-4C-DATE.
           MOVE "SOLICT FORM CNT:" TO D-P4D.
           MOVE LN-SOLCNT TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECOURSE CODE  :" TO D-P4A.
           MOVE LN-DLRECOURSE TO D-FIELD4A.
           MOVE "O1 REBATE FLAG :" TO D-P4B.
           MOVE LN-O1-REBATE TO D-FIELD4B.
           MOVE "PROMISED AMOUNT :" TO D-P4C.
           MOVE LN-PROMAMT TO D-4C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PART RECOURSE  :" TO D-P4A.
           MOVE LN-DLPARTRCAMT TO D-4A-AMT.
           MOVE "O2 REBATE FLAG :" TO D-P4B.
           MOVE LN-O2-REBATE TO D-FIELD4B.
           MOVE "PROMISED DATE #2:" TO D-P4C.
           MOVE LN-PROMDATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-4C-DATE.
           MOVE "PRIOR LOAN INFORMATION:" TO D-COLUMN4D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER POOL    :" TO D-P4A.
           MOVE LN-DLPOOL TO D-4A-COUNT.
           MOVE "IN REBATE FLAG :" TO D-P4B.
           MOVE LN-INT-REBATE TO D-FIELD4B.
           MOVE "LST PMT DATE    :" TO D-P4C.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-4C-DATE.
           MOVE "CONVERTED ACCT#:" TO D-P4D.
           MOVE LN-OLDACCTNO TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SC REBATE FLAG :" TO D-P4B.
           MOVE LN-SC-REBATE TO D-FIELD4B.
           MOVE "LST PMT AMT     :" TO D-P4C.
           MOVE LN-AMT-PAID-LAST TO D-4C-AMT.
           MOVE "PRIOR LOAN NUM.:" TO D-P4D.
           MOVE LN-PRLNNO TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIG PURCH AMT :" TO D-P4A.
           MOVE LN-ORIGPURAMT TO D-4A-AMT.
           MOVE "MF REBATE FLAG :" TO D-P4B.
           MOVE LN-MF-REBATE TO D-FIELD4B.
           MOVE "LST FORM NO.    :" TO D-P4C.
           MOVE LN-LASTCFORMNO TO D-4C-COUNT.
           MOVE "PRIOR LOAN BAL :" TO D-P4D.
           MOVE LN-PRBAL TO D-4D-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIG PURCH TERM:" TO D-P4A.
           MOVE LN-ANTICTERM  TO D-4A-COUNT.
           MOVE "DF REBATE FLAG :" TO D-P4B.
           MOVE LN-DF-REBATE TO D-FIELD4B.
           MOVE "DATE FORM NO.   :" TO D-P4C.
           MOVE LN-LASTCFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-4C-DATE.
           MOVE "INTEREST DUE   :" TO D-P4D.
           MOVE LN-PRINTDUE TO D-4D-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PURCHASE BAL   :" TO D-P4A.
           MOVE LN-PURBAL  TO D-4A-AMT.
           MOVE "DEL EXTRACT CNT :" TO D-P4C.
           MOVE LN-DELCNT TO D-4C-COUNT.
           MOVE "FIN CHG REBS   :" TO D-P4D.
           MOVE LN-PRFINCHGRB TO D-4D-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEL FORM CNT    :" TO D-P4C.
           MOVE LN-DELCNT2 TO D-4C-COUNT.
           MOVE "INSURANCE REBS :" TO D-P4D.
           MOVE LN-PRINSRB TO D-4D-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOT ALLOW DELINQ:" TO D-P4C.
           MOVE LN-TOTNOALDEL TO D-4C-COUNT.
           MOVE "PAID OFF CODE  :" TO D-P4D.
           MOVE LN-PRPOCD TO D-FIELD4D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YTD ALLOW DELINQ:" TO D-P4C.
           MOVE LN-YTDNOALDEL TO D-4C-COUNT.
           MOVE "PAID OFF DATE  :" TO D-P4D.
           MOVE LN-PRPODATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-4D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SCREEN BEEP MESS:" TO D-P4C.
           MOVE "CONTRCT PROFILE:" TO D-P4D.
           MOVE LN-PRCONPROFILE TO D-4D-PROFILE.
           INSPECT D-4D-PROFILE REPLACING ALL '9' BY '-'.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-MESSAGE TO D-COLUMN4C.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD-10 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM OPEN-LTC1-FILE.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 0
              MOVE LTC-DESC(1)  TO D-DESCRIPTION
              MOVE LTC-MAKE(1)  TO D-MAKE
              MOVE LTC-MODEL(1) TO D-MODEL
              MOVE LTC-YEAR(1)  TO D-YEAR
              MOVE LTC-STYLE(1) TO D-STYLE
              MOVE LTC-CLASS(1) TO D-LTCLASS
              MOVE LTC-VIN(1)   TO D-ID
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO LTC-AUTO(1) LTC-AUTO(2)
                             LTC-HHGS(1) LTC-HHGS(2) LTC-HHGS(3).

           IF LTC-AUTO(2) NOT = SPACES
              MOVE LTC-DESC(2)  TO D-DESCRIPTION
              MOVE LTC-MAKE(2)  TO D-MAKE
              MOVE LTC-MODEL(2) TO D-MODEL
              MOVE LTC-YEAR(2)  TO D-YEAR
              MOVE LTC-STYLE(2) TO D-STYLE
              MOVE LTC-CLASS(2) TO D-LTCLASS
              MOVE LTC-VIN(2)   TO D-ID
              PERFORM WRITE-DETAIL-LINE.

           IF LTC-HHGS(1) NOT = SPACES
              MOVE LTC-HHGS(1) TO DETAIL-LINE
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF LTC-HHGS(2) NOT = SPACES
              MOVE LTC-HHGS(2) TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO PURGE-COUNT.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       PURGE-LOAN-MEMOS SECTION.
      ******************************************************************


           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
           MOVE 0             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ 
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.
           COMPUTE QLM1-WSBEG-DATE-REV = LM-DATE-REVERSED + 1.

           PERFORM START-LM1-FILE.
           MOVE "Y"       TO FIRST-MEMO.

       NEXT-MEMO.

           PERFORM READ-LM1-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES ) OR ( LM-BRNO NOT = LM-PATH-OWNBR )
              GO TO END-PURGE-LOAN-MEMOS.

           IF ( LM-ACCTNO NOT = LN-ACCTNO )
      *       UNLOCK LM-FILE
              GO TO END-PURGE-LOAN-MEMOS.

           IF ( PRINT-MEMOS = "Y" )
              PERFORM PRINT-LOAN-MEMOES.

           IF ( ACTION-BOTH  ) OR
              ( ACTION-PURGE )
              PERFORM DELETE-LM1-FILE.

           GO TO NEXT-MEMO.

       PRINT-LOAN-MEMOES.

           IF ( LN-COUNT > 57 )
              PERFORM HEAD-PAGE.

           IF ( FIRST-MEMO = "Y" )
              MOVE "N" TO FIRST-MEMO
              MOVE "-------  M E M O S  -------" TO D-LINE5
              MOVE 2 TO LN-FEED
           ELSE
              MOVE 1 TO LN-FEED.

           MOVE "DATE"   TO D-MEM-TAG1.
           SUBTRACT LM-DATE FROM 99999999 GIVING DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MEM-DATE.
           MOVE "SEQ"    TO D-MEM-TAG2.
           MOVE LM-SEQ   TO D-MEM-SEQ.
           MOVE LM-MEMO1 TO D-MEMO1.
           MOVE LM-MEMO2 TO D-MEMO2.
           PERFORM WRITE-DETAIL-LINE.

       END-PURGE-LOAN-MEMOS.
           EXIT.

      ******************************************************************
       PURGE-LOAN-ENDORSERS SECTION.
      ******************************************************************

           IF ( LN-SMALLOAN NOT = "Y" )
              GO TO END-PURGE-LOAN-ENDORSERS.

           MOVE LN-OWNBR  TO LE-BRNO
                             QLE1-WBEG-BRNO
                             QLE1-WEND-BRNO.
           MOVE LN-ACCTNO TO LE-ACCTNO
                             QLE1-WBEG-ACCTNO
                             QLE1-WEND-ACCTNO.

           MOVE "E"       TO LE-TYPE
                             QLE1-WBEG-TYPE.
           MOVE ALL "Z"   TO QLE1-WEND-TYPE.

           MOVE SPACES    TO LE-FLAG
                             QLE1-WBEG-FLAG.
           MOVE ALL "Z"   TO QLE1-WEND-FLAG.

           MOVE 0         TO LE-OSACCTNO LE-SSNO
                             QLE1-WBEG-OSACCTNO
                             QLE1-WBEG-SSNO.
           MOVE ALL "9"   TO QLE1-WEND-OSACCTNO
                             QLE1-WEND-SSNO.

           PERFORM START-LE1-FILE.
           MOVE "Y"       TO HEAD-UP-ENDORSERS.

       NEXT-ENDORSER.

           PERFORM READ-LE1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR ( LE-BRNO NOT = LE-PATH-OWNBR )
              GO TO END-PURGE-LOAN-ENDORSERS.

           IF ( LE-ACCTNO NOT = LN-ACCTNO )
      *       UNLOCK LE-FILE
              GO TO END-PURGE-LOAN-ENDORSERS.

           IF ( PRINT-END = "Y" )
              PERFORM PRINT-LOAN-ENDORSERS.

           IF ( ACTION-BOTH  ) OR
              ( ACTION-PURGE )
              PERFORM DELETE-LE1-FILE.

           GO TO NEXT-ENDORSER.

       PRINT-LOAN-ENDORSERS.

           IF ( LN-COUNT > 57 )
              MOVE "Y" TO HEAD-UP-ENDORSERS
              PERFORM HEAD-PAGE.

           IF ( HEAD-UP-ENDORSERS = "Y" )
              MOVE "N"   TO HEAD-UP-ENDORSERS
              MOVE 2     TO LN-FEED
              MOVE HEAD7 TO PR-REC
              PERFORM WRITE-PR-REC
              ADD 2      TO LN-COUNT.

           MOVE LE-SSNO     TO DLE-SSNO.
           INSPECT DLE-SSNO REPLACING ALL "/" BY "-".
           MOVE LE-OSACCTNO TO DLE-OSACCTNO.
           PERFORM FIND-BORROWER.
           MOVE LE-E-OSDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DLE-OSDATE.
           MOVE LE-E-OSRATE TO DLE-OSRATE.
           MOVE LE-E-OSAMT  TO DLE-OSAMT.
           PERFORM WRITE-DETAIL-LINE.

       END-PURGE-LOAN-ENDORSERS.
           EXIT.

      ******************************************************************
       PURGE-LOAN-PAYMENTS SECTION.
      ******************************************************************


           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.
           MOVE "Y"       TO HEAD-UP-PAYMENTS.

       NEXT-PAYMENT.

           PERFORM READ-LP1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-PURGE-LOAN-PAYMENTS.

           IF (LP-BRNO NOT = LP-PATH-OWNBR) OR
               (LP-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LP-FILE
              GO TO EXIT-PURGE-LOAN-PAYMENTS.

           IF ( PRINT-PAYMENTS = "Y" )
              PERFORM PRINT-LOAN-PAYMENTS.

           IF ( ACTION-BOTH  ) OR
              ( ACTION-PURGE )
              PERFORM DELETE-LP1-FILE.

           GO TO NEXT-PAYMENT.

       EXIT-PURGE-LOAN-PAYMENTS.
           EXIT.

      ******************************************************************
       PRINT-LOAN-PAYMENTS SECTION.
      ******************************************************************

           IF ( LN-COUNT > 57 )
              MOVE "Y" TO HEAD-UP-PAYMENTS
              PERFORM HEAD-PAGE.

           IF ( HEAD-UP-PAYMENTS = "Y" )
              MOVE "N"   TO HEAD-UP-PAYMENTS
              MOVE 2     TO LN-FEED
              MOVE HEAD8 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 1     TO LN-FEED
              MOVE HEAD9 TO PR-REC
              PERFORM WRITE-PR-REC
              ADD  3     TO LN-COUNT
              MOVE 1     TO LN-FEED
           ELSE
              MOVE 1     TO LN-FEED.

           MOVE LP-TRDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO DLP-TRDATE.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DLP-PAYDATE.
           MOVE LP-IBPC    TO DLP-IBPC.
           MOVE LP-TRCD    TO DLP-TRCD.

           IF ( LP-REV = "*" )
              MOVE "RV" TO DLP-TRCD.

           MOVE LP-REFNO TO DLP-REFNO.

           IF ( LP-TRCD = "JD" OR "AC" OR "IR" )
              MOVE LP-NEWSTAT-RATE TO DLP-TRAMT-RATE
           ELSE
              MOVE LP-TRAMT TO DLP-TRAMT.

           ADD LP-APINT LP-APLC GIVING DLP-APINT.
           MOVE LP-APCUR TO DLP-APCUR.

           MOVE LP-PDTH-DATE TO NUM-DATE.
           MOVE NUM-MO  TO DLP-PDTH-MO.

           IF ( LN-UNITPER-CD-DAYS )
              MOVE NUM-DA TO DLP-PDTH-DAYR
           ELSE
              MOVE NUM-YR TO DLP-PDTH-DAYR.

           MOVE LP-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO    TO DLP-LCPDTH-MO.

           IF ( LN-UNITPER-CD-DAYS )
              MOVE NUM-DA TO DLP-LCPDTH-DAYR
           ELSE
              MOVE NUM-YR TO DLP-LCPDTH-DAYR.

           MOVE LP-OTHBAL TO DLP-OTHBAL.
           ADD LP-INTBAL LP-LCBAL GIVING DLP-INTBAL.
           MOVE LP-CURBAL TO DLP-CURBAL.

           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE ZEROES TO LXE-EARN(1) LXE-EARN(2) LXE-EARN(3).

           COMPUTE DLP-EARNED = LP-EARNED(1) + LP-EARNED(2) +
                                LP-EARNED(3) + LXE-EARN (1) +
                                LXE-EARN (2) + LXE-EARN (3).

           MOVE LP-DLPROC TO DLP-DLPROC.
           MOVE "Y"       TO PRINT-PAYMENT-LINE.

           MOVE ZEROES    TO SUB.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
           PERFORM READ-LXG1-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO PRINT-CHECK-LP-GL.

       PRINT-LP-GL.

           ADD 1 TO SUB.

           IF ( SUB > 3 )
              GO TO PRINT-CHECK-LP-GL.

           IF ( LXG-GLNO(SUB) NOT = ZEROES )
              MOVE "N"            TO PRINT-PAYMENT-LINE
              MOVE LXG-GLNO(SUB)  TO DLX-GLNO
              INSPECT DLX-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE LXG-GLAMT(SUB) TO DLX-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-LP-GL.

       PRINT-CHECK-LP-GL.

           IF ( PRINT-PAYMENT-LINE = "Y" )
              PERFORM WRITE-DETAIL-LINE.

       END-PRINT-LOAN-PAYMENTS.
           EXIT.

      ******************************************************************
       FIND-BORROWER SECTION.
      ******************************************************************

           MOVE LE-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           IF ( IO-FG NOT = ZEROES )
               MOVE "** NO BORROWER MASTER **" TO DLE-NAME
               GO TO EXIT-FIND-BORROWER.

      *    UNLOCK BW-FILE.
           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER
                   REPLACING FIRST "                " BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO DLE-NAME.

       EXIT-FIND-BORROWER.
           EXIT.

      ******************************************************************
       DISPLAY-BORROWER SECTION.
      ******************************************************************

           MOVE LN-SSNO(SUB) TO BW-SSNO.

           IF ( SUB NOT = 1 )
              PERFORM READ-BW1-FILE.

           IF ( LN-SSNO(SUB) NOT = ZEROES )
              MOVE BW-SSNO         TO D-1A-SSNO
              INSPECT D-1A-SSNO REPLACING ALL "/" BY "-"
              MOVE LN-MAKERCD(SUB) TO D-1A-MAKERCD.

       EXIT-DISPLAY-BORROWER.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED      TO LN-COUNT.
           MOVE 1           TO LN-FEED.
           MOVE SPACES      TO DETAIL-LINE.

       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD  1           TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99    TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 2 TO LN-COUNT.

       EXIT-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PURGE-LOAN-PROCESS SECTION.
      ******************************************************************

           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE LN-PLDATE TO SYS-DATE.
           PERFORM TIM365.

           MOVE 0 TO SUB.
       SSNO-LOOP.

           ADD 1 TO SUB.

           IF ( SUB > 4 )
              GO TO DELETE-LOAN-RECORD.

           IF ( LN-SSNO(SUB) = ZEROES )
              GO TO SSNO-LOOP.

           PERFORM CLEAR-LH-FILE.
           MOVE LN-OWNBR         TO LH-BRNO.
           MOVE LN-SSNO(SUB)     TO LH-SSNO.
           MOVE LN-ACCTNO        TO LH-ACCTNO.
           MOVE LN-CLASS         TO LH-CLASS.
           MOVE LN-LAWCODE       TO LH-LAWCD.
           MOVE LN-COLLCD        TO LH-COLLCD.
           MOVE LN-MAKERCD(SUB)  TO LH-MAKERCD.
           MOVE LN-POCD          TO LH-POCD.
           MOVE LN-LOANDATE      TO LH-LOANDATE.
           MOVE LN-PLDATE        TO LH-PLDATE.
           MOVE LN-JDDATE        TO LH-JDDATE.
           MOVE LN-POFFDATE      TO LH-POFFDATE.
           MOVE LN-LNAMT         TO LH-LNAMT.
           MOVE LN-ORGTERM       TO LH-ORGTERM.
           MOVE ELAPSED-MONTHS   TO LH-POTERMS.
           MOVE LN-CONPROFILE(1) TO LH-PYPROFILE.
           MOVE LN-DLNO          TO LH-DLNO.
           MOVE LN-STATFLAGS     TO LH-STATUS.
           PERFORM WRITE-LH1-FILE.

           IF ( IO-FG NOT = ZEROES )
              IF ( E-CODE NOT = "22" )
                 GO TO IO-ERROR.

           GO TO SSNO-LOOP.

       DELETE-LOAN-RECORD.

           PERFORM DELETE-LN3-FILE.

           PERFORM DELETE-NOTICE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DELETE RX-RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-POCD-RESCIND )
              MOVE LN-ACCTNO TO RX-ACCTNO
              PERFORM READ-RX1-FILE
              IF IO-FG = 0
                 PERFORM DELETE-RX1-FILE.

       END-PURGE-LN-PROCESS.
           EXIT.


      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE "TOTAL REPORTED ACCOUNTS :" TO RANGE-LINE.
           MOVE PURGE-COUNT TO R-BRANCH.
           MOVE 4           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING/ENDING BRANCH :" TO RANGE-LINE.
           IF ( GROUP-FLAG = "Y" )
              MOVE ENT-GROUP  TO R-GROUP
              MOVE SPACES     TO R-GROUP
           ELSE
              MOVE BEG-BRANCH TO R-BRANCH
              MOVE END-BRANCH TO R-END-BRANCH.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING/ENDING CLASS  :" TO RANGE-LINE.
           MOVE BEG-CLASS   TO R-BRANCH.
           MOVE END-CLASS   TO R-END-BRANCH.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING/ENDING ACCOUNT:" TO RANGE-LINE.
           MOVE BEG-ACCT   TO R-BRANCH.
           MOVE END-ACCT   TO R-END-BRANCH.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING/ENDING PLDATE :" TO RANGE-LINE.
           MOVE BEG-PL-DATE TO R-GROUP.
           MOVE END-PL-DATE TO R-END-GROUP.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING/ENDING PAYDATE:" TO RANGE-LINE.
           MOVE BEG-PAY-DATE TO R-GROUP.
           MOVE END-PAY-DATE TO R-END-GROUP.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SYSTEM DATE" TO RANGE-LINE.
           MOVE HOLD-SYSDATE  TO R-GROUP.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       CLASS-SCAN SECTION.
      ******************************************************************
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
           MOVE ZEROES     TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
       PURGE-LX-RECORDS SECTION.
      ******************************************************************
           IF NOT (ACTION-BOTH OR ACTION-PURGE)
              GO TO PURGE-LX-RECORDS-EXIT.

           MOVE LN-OWNBR  TO LX-BRNO
                             QLX1-WBEG-BRNO
                             QLX1-WEND-BRNO.
           MOVE LN-ACCTNO TO LX-ACCTNO
                             QLX1-WBEG-ACCTNO
                             QLX1-WEND-ACCTNO.
           MOVE 0         TO LX-SEQNO
                             QLX1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLX1-WEND-SEQNO.

           PERFORM START-LX1-FILE.
       PURGE-LX-NEXT.
           PERFORM READ-LX1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LX-RECORDS-EXIT.

           IF (LX-BRNO NOT = LN-OWNBR) OR (LX-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LX-FILE
              GO TO PURGE-LX-RECORDS-EXIT.

           PERFORM DELETE-LX1-FILE.

           GO TO PURGE-LX-NEXT.

       PURGE-LX-RECORDS-EXIT.
           EXIT.

      ******************************************************************
       PURGE-LXG-RECORDS SECTION.
      ******************************************************************
           IF NOT (ACTION-BOTH OR ACTION-PURGE)
              GO TO PURGE-LXG-RECORDS-EXIT.

           MOVE LN-OWNBR  TO LXG-BRNO
                             QLXG1-WBEG-BRNO
                             QLXG1-WEND-BRNO.
           MOVE LN-ACCTNO TO LXG-ACCTNO
                             QLXG1-WBEG-ACCTNO
                             QLXG1-WEND-ACCTNO.
           MOVE 0         TO LXG-SEQNO
                             QLXG1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLXG1-WEND-SEQNO.

           PERFORM START-LXG1-FILE.
       PURGE-LXG-NEXT.
           PERFORM READ-LXG1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LXG-RECORDS-EXIT.

           IF (LXG-BRNO NOT = LN-OWNBR) OR
                (LXG-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LXG-FILE
              GO TO PURGE-LXG-RECORDS-EXIT.

           PERFORM DELETE-LXG1-FILE.

           GO TO PURGE-LXG-NEXT.

       PURGE-LXG-RECORDS-EXIT.
           EXIT.

      ******************************************************************
       PURGE-LXE-RECORDS SECTION.
      ******************************************************************
           IF NOT (ACTION-BOTH OR ACTION-PURGE)
              GO TO PURGE-LXE-RECORDS-EXIT.

           MOVE LN-OWNBR  TO LXE-BRNO
                             QLXE1-WBEG-BRNO
                             QLXE1-WEND-BRNO.
           MOVE LN-ACCTNO TO LXE-ACCTNO
                             QLXE1-WBEG-ACCTNO
                             QLXE1-WEND-ACCTNO.
           MOVE 0         TO LXE-SEQNO
                             QLXE1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLXE1-WEND-SEQNO.

           PERFORM START-LXE1-FILE.
       PURGE-LXE-NEXT.
           PERFORM READ-LXE1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LXE-RECORDS-EXIT.

           IF (LXE-BRNO NOT = LN-OWNBR) OR
                (LXE-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LXE-FILE
              GO TO PURGE-LXE-RECORDS-EXIT.

           PERFORM DELETE-LXE1-FILE.

           GO TO PURGE-LXE-NEXT.

       PURGE-LXE-RECORDS-EXIT.
           EXIT.

      ******************************************************************
       PURGE-LS-RECORDS SECTION.
      ******************************************************************
           IF NOT (ACTION-BOTH OR ACTION-PURGE)
              GO TO PURGE-LS-RECORDS-EXIT.

           PERFORM PURGE-LSD-RECORDS.
           PERFORM PURGE-LSF-RECORDS.
           PERFORM PURGE-LSG-RECORDS.
           PERFORM PURGE-LSJ-RECORDS.
           PERFORM PURGE-LSL-RECORDS.
           PERFORM PURGE-LSM-RECORDS.
           PERFORM PURGE-LSP-RECORDS.
           PERFORM PURGE-LSR-RECORDS.
           PERFORM PURGE-LSX-RECORDS.

       PURGE-LS-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSX-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSX1-FILE.

           MOVE LN-OWNBR  TO LSX-BRNO
                             QLSX1-WBEG-BRNO
                             QLSX1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSX-ACCTNO
                             QLSX1-WBEG-ACCTNO
                             QLSX1-WEND-ACCTNO.
           MOVE 0         TO LSX-SEQNO
                             QLSX1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSX1-WEND-SEQNO.

           PERFORM START-LSX1-FILE.
       PURGE-LSX-NEXT.
           PERFORM READ-LSX1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSX-RECORDS-EXIT.

           IF (LSX-BRNO NOT = LSX-PATH-OWNBR) OR
               (LSX-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSX-FILE
              GO TO PURGE-LSX-RECORDS-EXIT.

           PERFORM DELETE-LSX1-FILE.

           GO TO PURGE-LSX-NEXT.

       PURGE-LSX-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSR-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSR1-FILE.

           MOVE LN-OWNBR  TO LSR-BRNO
                             QLSR1-WBEG-BRNO
                             QLSR1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSR-ACCTNO
                             QLSR1-WBEG-ACCTNO
                             QLSR1-WEND-ACCTNO.
           MOVE 0         TO LSR-SEQNO
                             QLSR1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSR1-WEND-SEQNO.

           PERFORM START-LSR1-FILE.
       PURGE-LSR-NEXT.
           PERFORM READ-LSR1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSR-RECORDS-EXIT.

           IF (LSR-BRNO NOT = LSR-PATH-OWNBR) OR 
              ( LSR-ACCTNO NOT = LN-ACCTNO) 
      *       UNLOCK LSR-FILE
              GO TO PURGE-LSR-RECORDS-EXIT.

           PERFORM DELETE-LSR1-FILE.

           GO TO PURGE-LSR-NEXT.

       PURGE-LSR-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSP-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSP1-FILE.

           MOVE LN-OWNBR  TO LSP-BRNO
                             QLSP1-WBEG-BRNO
                             QLSP1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSP-ACCTNO
                             QLSP1-WBEG-ACCTNO
                             QLSP1-WEND-ACCTNO.
           MOVE 0         TO LSP-SEQNO
                             QLSP1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSP1-WEND-SEQNO.

           PERFORM START-LSP1-FILE.
       PURGE-LSP-NEXT.
           PERFORM READ-LSP1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSP-RECORDS-EXIT.

           IF (LSP-BRNO NOT = LSP-PATH-OWNBR) OR
                (LSP-ACCTNO NOT = LN-ACCTNO) 
      *       UNLOCK LSP-FILE
              GO TO PURGE-LSP-RECORDS-EXIT.

           PERFORM DELETE-LSP1-FILE.

           GO TO PURGE-LSP-NEXT.

       PURGE-LSP-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSM-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSM1-FILE.

           MOVE LN-OWNBR  TO LSM-BRNO
                             QLSM1-WBEG-BRNO
                             QLSM1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSM-ACCTNO
                             QLSM1-WBEG-ACCTNO
                             QLSM1-WEND-ACCTNO.
           MOVE 0         TO LSM-SEQNO
                             QLSM1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSM1-WEND-SEQNO.

           PERFORM START-LSM1-FILE.
       PURGE-LSM-NEXT.
           PERFORM READ-LSM1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSM-RECORDS-EXIT.

           IF (LSM-BRNO NOT = LSM-PATH-OWNBR) OR 
                (LSM-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSM-FILE
              GO TO PURGE-LSM-RECORDS-EXIT.

           PERFORM DELETE-LSM1-FILE.

           GO TO PURGE-LSM-NEXT.

       PURGE-LSM-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSL-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSL1-FILE.

           MOVE LN-OWNBR  TO LSL-BRNO
                             QLSL1-WBEG-BRNO
                             QLSL1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSL-ACCTNO
                             QLSL1-WBEG-ACCTNO
                             QLSL1-WEND-ACCTNO.
           MOVE 0         TO LSL-SEQNO
                             QLSL1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSL1-WEND-SEQNO.

           PERFORM START-LSL1-FILE.
       PURGE-LSL-NEXT.
           PERFORM READ-LSL1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSL-RECORDS-EXIT.

           IF (LSL-BRNO NOT = LSL-PATH-OWNBR) OR
               (LSL-ACCTNO NOT = LN-ACCTNO) 
      *       UNLOCK LSL-FILE
              GO TO PURGE-LSL-RECORDS-EXIT.

           PERFORM DELETE-LSL1-FILE.

           GO TO PURGE-LSL-NEXT.

       PURGE-LSL-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSJ-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSJ1-FILE.

           MOVE LN-OWNBR  TO LSJ-BRNO
                             QLSJ1-WBEG-BRNO
                             QLSJ1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSJ-ACCTNO
                             QLSJ1-WBEG-ACCTNO
                             QLSJ1-WEND-ACCTNO.
           MOVE 0         TO LSJ-SEQNO
                             QLSJ1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSJ1-WEND-SEQNO.

           PERFORM START-LSJ1-FILE.
       PURGE-LSJ-NEXT.
           PERFORM READ-LSJ1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSJ-RECORDS-EXIT.

           IF (LSJ-BRNO NOT = LSJ-PATH-OWNBR) OR
               (LSJ-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSJ-FILE
              GO TO PURGE-LSJ-RECORDS-EXIT.

           PERFORM DELETE-LSJ1-FILE.

           GO TO PURGE-LSJ-NEXT.

       PURGE-LSJ-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSG-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSG1-FILE.

           MOVE LN-OWNBR  TO LSG-BRNO
                             QLSG1-WBEG-BRNO
                             QLSG1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSG-ACCTNO
                             QLSG1-WBEG-ACCTNO
                             QLSG1-WEND-ACCTNO.
           MOVE 0         TO LSG-SEQNO
                             QLSG1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSG1-WEND-SEQNO.

           PERFORM START-LSG1-FILE.
       PURGE-LSG-NEXT.
           PERFORM READ-LSG1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSG-RECORDS-EXIT.

           IF (LSG-BRNO NOT = LSG-PATH-OWNBR) OR
               (LSG-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSG-FILE
              GO TO PURGE-LSG-RECORDS-EXIT.

           PERFORM DELETE-LSG1-FILE.

           GO TO PURGE-LSG-NEXT.

       PURGE-LSG-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSF-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSF1-FILE.

           MOVE LN-OWNBR  TO LSF-BRNO
                             QLSF1-WBEG-BRNO
                             QLSF1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSF-ACCTNO
                             QLSF1-WBEG-ACCTNO
                             QLSF1-WEND-ACCTNO.
           MOVE 0         TO LSF-SEQNO
                             QLSF1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSF1-WEND-SEQNO.

           PERFORM START-LSF1-FILE.
       PURGE-LSF-NEXT.
           PERFORM READ-LSF1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSF-RECORDS-EXIT.

           IF (LSF-BRNO NOT = LSF-PATH-OWNBR) OR
                (LSF-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSF-FILE
              GO TO PURGE-LSF-RECORDS-EXIT.

           PERFORM DELETE-LSF1-FILE.

           GO TO PURGE-LSF-NEXT.

       PURGE-LSF-RECORDS-EXIT.
           EXIT.

      **************************************************
       PURGE-LSD-RECORDS SECTION.
      **************************************************

           PERFORM OPEN-LSD1-FILE.

           MOVE LN-OWNBR  TO LSD-BRNO
                             QLSD1-WBEG-BRNO
                             QLSD1-WEND-BRNO.
           MOVE LN-ACCTNO TO LSD-ACCTNO
                             QLSD1-WBEG-ACCTNO
                             QLSD1-WEND-ACCTNO.
           MOVE 0         TO LSD-SEQNO
                             QLSD1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLSD1-WEND-SEQNO.

           PERFORM START-LSD1-FILE.
       PURGE-LSD-NEXT.
           PERFORM READ-LSD1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LSD-RECORDS-EXIT.

           IF (LSD-BRNO NOT = LSD-PATH-OWNBR) OR 
                (LSD-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSD-FILE
              GO TO PURGE-LSD-RECORDS-EXIT.

           PERFORM DELETE-LSD1-FILE.

           GO TO PURGE-LSD-NEXT.

       PURGE-LSD-RECORDS-EXIT.
           EXIT.

      ******************************************************************
       PURGE-LT-RECORDS SECTION.
      ******************************************************************

           IF NOT (ACTION-BOTH OR ACTION-PURGE)
              GO TO PURGE-LT-RECORDS-EXIT.

           PERFORM PURGE-LTA-RECORDS.
           PERFORM PURGE-LTB-RECORDS.
           PERFORM PURGE-LTC-RECORDS.
           PERFORM PURGE-LTF-RECORDS.
           PERFORM PURGE-LTI-RECORDS.
           PERFORM PURGE-LTL-RECORDS.
           PERFORM PURGE-LTP-RECORDS.

       PURGE-LT-RECORDS-EXIT.
           EXIT.


      *****************************************
       PURGE-LTA-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-LTA1-FILE.

           MOVE LN-OWNBR  TO LTA-BRNO
                             QLTA1-WBEG-BRNO
                             QLTA1-WEND-BRNO.
           MOVE LN-ACCTNO TO LTA-ACCTNO
                             QLTA1-WBEG-ACCTNO
                             QLTA1-WEND-ACCTNO.
           MOVE 0         TO LTA-SEQNO
                             QLTA1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLTA1-WEND-SEQNO.

           PERFORM START-LTA1-FILE.
       PURGE-LTA-NEXT.
           PERFORM READ-LTA1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LTA-RECORDS-EXIT.

           IF (LTA-BRNO NOT = LTA-PATH-OWNBR) OR 
               (LTA-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTA-FILE
              GO TO PURGE-LTA-RECORDS-EXIT.

           PERFORM DELETE-LTA1-FILE.

           GO TO PURGE-LTA-NEXT.

       PURGE-LTA-RECORDS-EXIT.
           PERFORM CLOSE-LTA1-FILE.

      *****************************************
       PURGE-LTB-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-LTB1-FILE.

           MOVE LN-OWNBR  TO LTB-BRNO
                             QLTB1-WBEG-BRNO
                             QLTB1-WEND-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO
                             QLTB1-WBEG-ACCTNO
                             QLTB1-WEND-ACCTNO.
           MOVE 0         TO LTB-SEQNO
                             QLTB1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLTB1-WEND-SEQNO.

           PERFORM START-LTB1-FILE.
       PURGE-LTB-NEXT.
           PERFORM READ-LTB1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LTB-RECORDS-EXIT.

           IF (LTB-BRNO NOT = LTB-PATH-OWNBR) OR
               (LTB-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTB-FILE
              GO TO PURGE-LTB-RECORDS-EXIT.

           PERFORM DELETE-LTB1-FILE.

           GO TO PURGE-LTB-NEXT.

       PURGE-LTB-RECORDS-EXIT.
           PERFORM CLOSE-LTB1-FILE.

      *****************************************
       PURGE-LTC-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-LTC1-FILE.

           MOVE LN-OWNBR  TO LTC-BRNO
                             QLTC1-WBEG-BRNO
                             QLTC1-WEND-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO
                             QLTC1-WBEG-ACCTNO
                             QLTC1-WEND-ACCTNO.
           MOVE 0         TO LTC-SEQNO
                             QLTC1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLTC1-WEND-SEQNO.

           PERFORM START-LTC1-FILE.
       PURGE-LTC-NEXT.
           PERFORM READ-LTC1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LTC-RECORDS-EXIT.

           IF (LTC-BRNO NOT = LTC-PATH-OWNBR) OR
               (LTC-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTC-FILE
              GO TO PURGE-LTC-RECORDS-EXIT.

           PERFORM DELETE-LTC1-FILE.

           GO TO PURGE-LTC-NEXT.

       PURGE-LTC-RECORDS-EXIT.
           PERFORM CLOSE-LTC1-FILE.

      *****************************************
       PURGE-LTF-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-LTF1-FILE.

           MOVE LN-OWNBR  TO LTF-BRNO
                             QLTF1-WBEG-BRNO
                             QLTF1-WEND-BRNO.
           MOVE LN-ACCTNO TO LTF-ACCTNO
                             QLTF1-WBEG-ACCTNO
                             QLTF1-WEND-ACCTNO.
           MOVE 0         TO LTF-SEQNO
                             QLTF1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLTF1-WEND-SEQNO.

           PERFORM START-LTF1-FILE.
       PURGE-LTF-NEXT.
           PERFORM READ-LTF1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LTF-RECORDS-EXIT.

           IF (LTF-BRNO NOT = LTF-PATH-OWNBR) OR
               (LTF-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTF-FILE
              GO TO PURGE-LTF-RECORDS-EXIT.

           PERFORM DELETE-LTF1-FILE.

           GO TO PURGE-LTF-NEXT.

       PURGE-LTF-RECORDS-EXIT.
           PERFORM CLOSE-LTF1-FILE.

      *****************************************
       PURGE-LTI-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-LTI1-FILE.

           MOVE LN-OWNBR  TO LTI-BRNO
                             QLTI1-WBEG-BRNO
                             QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO TO LTI-ACCTNO
                             QLTI1-WBEG-ACCTNO
                             QLTI1-WEND-ACCTNO.
           MOVE 0         TO LTI-SEQNO
                             QLTI1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.
       PURGE-LTI-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LTI-RECORDS-EXIT.

           IF (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
               (LTI-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTI-FILE
              GO TO PURGE-LTI-RECORDS-EXIT.

           PERFORM DELETE-LTI1-FILE.

           GO TO PURGE-LTI-NEXT.

       PURGE-LTI-RECORDS-EXIT.
           PERFORM CLOSE-LTI1-FILE.

      *****************************************
       PURGE-LTL-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-LTL1-FILE.

           MOVE LN-OWNBR  TO LTL-BRNO
                             QLTL1-WBEG-BRNO
                             QLTL1-WEND-BRNO.
           MOVE LN-ACCTNO TO LTL-ACCTNO
                             QLTL1-WBEG-ACCTNO
                             QLTL1-WEND-ACCTNO.
           MOVE 0         TO LTL-SEQNO
                             QLTL1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLTL1-WEND-SEQNO.

           PERFORM START-LTL1-FILE.
       PURGE-LTL-NEXT.
           PERFORM READ-LTL1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LTL-RECORDS-EXIT.

           IF (LTL-BRNO NOT = LTL-PATH-OWNBR) OR
               (LTL-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTL-FILE
              GO TO PURGE-LTL-RECORDS-EXIT.

           PERFORM DELETE-LTL1-FILE.

           GO TO PURGE-LTL-NEXT.

       PURGE-LTL-RECORDS-EXIT.
           PERFORM CLOSE-LTL1-FILE.

      *****************************************
       PURGE-LTP-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-LTP1-FILE.

           MOVE LN-OWNBR  TO LTP-BRNO
                             QLTP1-WBEG-BRANCH
                             QLTP1-WEND-BRANCH.
           MOVE LN-ACCTNO TO LTP-ACCTNO
                             QLTP1-WBEG-ACCTNO
                             QLTP1-WEND-ACCTNO.
           MOVE 0         TO LTP-SEQNO
                             QLTP1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLTP1-WEND-SEQNO.

           PERFORM START-LTP1-FILE.
       PURGE-LTP-NEXT.
           PERFORM READ-LTP1-FILE-NEXT.
           IF IO-BAD
              GO TO PURGE-LTP-RECORDS-EXIT.

           IF (LTP-BRNO NOT = LTP-PATH-OWNBR) OR
               (LTP-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTP-FILE
              GO TO PURGE-LTP-RECORDS-EXIT.

           PERFORM DELETE-LTP1-FILE.

           GO TO PURGE-LTP-NEXT.

       PURGE-LTP-RECORDS-EXIT.
           PERFORM CLOSE-LTP1-FILE.

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
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBLP/LPLHCL.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO PLPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PLPURG-SYSDATE.
           DISPLAY PLPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PLPURG-FORM-FIELDS TO WR-BUF.
           MOVE PLPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE PLPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE PLPURG-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/PLPURG_EVA.CPY".
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
       DELETE-NOTICE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-NO1-FILE.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM READ-CDB1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE SPACES TO CDB-DESC.

       SET-FEE-LEFT.
           MOVE LN-FEECODE(SUB) TO D-FEECD1 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC        TO D-FEEDESC1.
           MOVE LN-FEEAMT (SUB) TO D-FEEAMT1.
           MOVE LN-FEEOURS(SUB) TO D-FEEOURS1.

       SET-FEE-RIGHT.
           MOVE LN-FEECODE(SUB) TO D-FEECD2 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC        TO D-FEEDESC2.
           MOVE LN-FEEAMT (SUB) TO D-FEEAMT2.
           MOVE LN-FEEOURS(SUB) TO D-FEEOURS2.

       SET-INSURANCE.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM    TO D-INSTERM.
           MOVE LN-INSCOMP(SUB) TO D-INSCO.
           MOVE LN-INSOURS(SUB) TO D-INSOURS.
           MOVE LN-INSEFF-DATE (SUB) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-INSEFF.
           MOVE LN-INSEXP-DATE (SUB) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-INSEXP.
           MOVE LN-INSCOVR(SUB) TO D-INSCOVR.
           MOVE LN-INSPREM(SUB) TO D-INSPREM.
           MOVE LN-INSCOMM(SUB) TO D-INSCOMM.

       DISPLAY-COMPONENT-EARNINGS.
           ADD 1 TO SUB.
           ADD LN-ANTICADJ(SUB) LN-ANTICERN(SUB) GIVING D-ANTIC.

           MOVE LN-ANTICADJ(SUB) TO D-ADJ.
           MOVE LN-ANTICERN(SUB) TO D-NET.
           MOVE LN-ACCERN  (SUB) TO D-EARN.

           IF ( LN-PLCD = SPACES ) AND ( LN-SERVACCT NOT = "Y" )
              SUBTRACT LN-ACCERN(SUB) FROM LN-ANTICERN(SUB)
                                    GIVING D-UNEARN
              IF ( D-UN-ALP NOT = SPACES )
                 IF ( SUB = 1 ) AND
                    (( LN-ANTICADJ(1) NOT = ZEROES     ) OR
                     ( NOT ( LN-REBATE(4) = " " OR "C" )) )
                    MOVE "  REBATED?" TO D-UN-ALP
                 ELSE
                 IF ( SUB = 3 ) AND
                    (( LN-ANTICADJ(3) NOT = ZEROES     ) OR
                     ( NOT ( LN-REBATE(5) = " " OR "C" )) )
                    MOVE "  REBATED?" TO D-UN-ALP
                 ELSE
                 IF ( SUB = 4 ) AND
                    (( LN-ANTICADJ(4) NOT = ZEROES    ) OR
                     (NOT ( LN-REBATE(6) = " " OR "C" )) )
                    MOVE "  REBATED?" TO D-UN-ALP.

           MOVE LN-UFERN(SUB) TO D-UFRONT.

       CHECK-PAGE.
           ADD 1 TO LN-COUNT.

           IF ( LN-COUNT > 40 )
               PERFORM HEAD-PAGE.

           IF ( LN-COUNT = 2 )
               MOVE 6 TO LN-FEED
           ELSE
               MOVE 4 TO LN-FEED.

       SEND-PROGRESS-LINE-WK.

           IF ( BR-NO = ZEROES )
              MOVE "...."  TO PROGRESS-LINE-BRANCHX
           ELSE
              MOVE BR-NO   TO PROGRESS-LINE-BRANCH.

           IF ( ACTION-BOTH )
              MOVE PROGRESS-MESSAGE-P-AND-R TO PROGRESS-LINE-TEXT
           ELSE
           IF ( ACTION-PURGE )
              MOVE PROGRESS-MESSAGE-PURGE   TO PROGRESS-LINE-TEXT
           ELSE
           IF ( ACTION-REPORT )
              MOVE PROGRESS-MESSAGE-REPORT  TO PROGRESS-LINE-TEXT
           ELSE
              MOVE SPACES                   TO PROGRESS-LINE-TEXT.

           MOVE PROGRESS-LINE TO LEGEND.
           PERFORM SEND-LEGEND.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTFGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLSPGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLSLGS_SQL.CPY".
       COPY "LIBLP/LPLSJGS_SQL.CPY".
       COPY "LIBLP/LPLSGGS_SQL.CPY".
       COPY "LIBLP/LPLSFGS_SQL.CPY".
       COPY "LIBLP/LPLSDGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLE1IN.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN3IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLTA1IN.CPY".
       COPY "LIBLP/LPLTB1IN.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPLTF1IN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTL1IN.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
       COPY "LIBLP/LPLX1IN.CPY".
       COPY "LIBLP/LPLXE1IN.CPY".
       COPY "LIBLP/LPLXG1IN.CPY".
       COPY "LIBLP/LPLSD1IN.CPY".
       COPY "LIBLP/LPLSF1IN.CPY".
       COPY "LIBLP/LPLSG1IN.CPY".
       COPY "LIBLP/LPLSJ1IN.CPY".
       COPY "LIBLP/LPLSL1IN.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBLP/LPLSP1IN.CPY".
       COPY "LIBLP/LPLSR1IN.CPY".
       COPY "LIBLP/LPLSX1IN.CPY".
       COPY "LIBLP/LPRX1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SPACES TO ERRLOGW-KEY.
           STRING ACTION,BEG-BRANCH,"-",END-BRANCH,",",BEG-CLASS,"-",
                END-CLASS,",",BEG-ACCT,"-",END-ACCT, ",",BEG-PL-DATE,
                ",",END-PL-DATE,",",BEG-PAY-DATE,",",END-PAY-DATE
                  DELIMITED BY "  " INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SPACES TO ERRLOGW-KEY.
           STRING BEG-PL-DATE,"-",END-PL-DATE,
              ",","COUNT=",PURGE-COUNT DELIMITED BY "  "
              INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.


       EXIT-PROG.

           IF ( SAVE-BEG-BRANCH NOT = 9999 )
              MOVE SAVE-BEG-BRANCH TO BEG-BRANCH
              MOVE SAVE-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PLPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
