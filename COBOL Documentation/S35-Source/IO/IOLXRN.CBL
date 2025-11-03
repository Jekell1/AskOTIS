       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLXRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-08-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLXRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LXFILE 
      * TABLE DESC    : LOAN PAYMENT TRAILER FILE
      * OUTPUT FILE   : XPC/IOLXRN.PRE
      * USED FOR      : LIBLP/LPWSLX.CPY  --- QLX-REC
      *                 LIBLP/LPLX1R.CPY  --- READ
      *                 LIBLP/LPLX1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0408 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOLXRN.CBL S34 05/10/21-14:23:03 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".

      *=================================================================
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LPWSLX.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLX1-WHERE-BEG-KEY.
           03  QLX1-WBEG-BRNO             PIC 9(04).
           03  QLX1-WBEG-ACCTNO           PIC 9(08).
           03  QLX1-WBEG-SEQNO            PIC 9(05).

       01  QLX1-WHERE-END-KEY.
           03  QLX1-WEND-BRNO             PIC 9(04).
           03  QLX1-WEND-ACCTNO           PIC 9(08).
           03  QLX1-WEND-SEQNO            PIC 9(05).

       01  QLX-REC.
           03  QLX-BRNO             PIC 9(4).
           03  QLX-ACCTNO           PIC 9(8).
           03  QLX-SEQNO            PIC 9(5).
           03  QLX-LTOUCH-DATE      PIC X(10).
           03  QLXT-CODE            PIC X.
           03  QLXT-EARN-1          PIC S9(7)V99.
           03  QLXT-EARN-2          PIC S9(7)V99.
           03  QLXT-EARN-3          PIC S9(7)V99.
           03  QLXT-EARN-4          PIC S9(7)V99.
           03  QLXT-EARN-5          PIC S9(7)V99.
           03  QLXT-EARN-6          PIC S9(7)V99.
           03  QLXT-EARN-7          PIC S9(7)V99.
           03  QLXT-EARN-8          PIC S9(7)V99.
           03  QLXT-EARN-9          PIC S9(7)V99.
           03  QLXT-EARN-10         PIC S9(7)V99.
           03  QLXT-EARN-11          PIC S9(7)V99.
           03  QLXT-EARN-12          PIC S9(7)V99.
           03  QLXT-EARN-13          PIC S9(7)V99.
           03  QLXT-EARN-14          PIC S9(7)V99.
           03  QLXT-EARN-15          PIC S9(7)V99.
           03  QLXT-EARN-16          PIC S9(7)V99.
           03  QLXT-EARN-17          PIC S9(7)V99.
           03  QLXT-EARN-18          PIC S9(7)V99.
           03  QLXT-EARN-19          PIC S9(7)V99.
           03  QLXT-EARN-20          PIC S9(7)V99.
           03  QLXP-CODE            PIC X.
           03  QLXP-EARN-1          PIC S9(7)V99.
           03  QLXP-EARN-2          PIC S9(7)V99.
           03  QLXP-EARN-3          PIC S9(7)V99.
           03  QLXP-EARN-4          PIC S9(7)V99.
           03  QLXP-EARN-5          PIC S9(7)V99.
           03  QLXP-EARN-6          PIC S9(7)V99.
           03  QLXP-EARN-7          PIC S9(7)V99.
           03  QLXP-EARN-8          PIC S9(7)V99.
           03  QLXP-EARN-9          PIC S9(7)V99.
           03  QLXP-EARN-10         PIC S9(7)V99.
           03  QLXP-EARN-11          PIC S9(7)V99.
           03  QLXP-EARN-12          PIC S9(7)V99.
           03  QLXP-EARN-13          PIC S9(7)V99.
           03  QLXP-EARN-14          PIC S9(7)V99.
           03  QLXP-EARN-15          PIC S9(7)V99.
           03  QLXP-EARN-16          PIC S9(7)V99.
           03  QLXP-EARN-17          PIC S9(7)V99.
           03  QLXP-EARN-18          PIC S9(7)V99.
           03  QLXP-EARN-19          PIC S9(7)V99.
           03  QLXP-EARN-20          PIC S9(7)V99.
           03  QLXA-CODE            PIC X.
           03  QLXA-SRCD                PIC XX.
           03  QLXA-START-PMT-1       PIC 99999.
           03  QLXA-END-PMT-1         PIC 99999.
           03  QLXA-NET-CHANGE-1      PIC S9(7)V99.
           03  QLXA-START-PMT-2       PIC 99999.
           03  QLXA-END-PMT-2         PIC 99999.
           03  QLXA-NET-CHANGE-2      PIC S9(7)V99.
           03  QLXA-START-PMT-3       PIC 99999.
           03  QLXA-END-PMT-3         PIC 99999.
           03  QLXA-NET-CHANGE-3      PIC S9(7)V99.
           03  QLXA-START-PMT-4       PIC 99999.
           03  QLXA-END-PMT-4         PIC 99999.
           03  QLXA-NET-CHANGE-4      PIC S9(7)V99.
           03  QLXA-START-PMT-5       PIC 99999.
           03  QLXA-END-PMT-5         PIC 99999.
           03  QLXA-NET-CHANGE-5      PIC S9(7)V99.
           03  QLXA-START-PMT-6       PIC 99999.
           03  QLXA-END-PMT-6         PIC 99999.
           03  QLXA-NET-CHANGE-6      PIC S9(7)V99.
           03  QLXA-START-PMT-7       PIC 99999.
           03  QLXA-END-PMT-7         PIC 99999.
           03  QLXA-NET-CHANGE-7      PIC S9(7)V99.
           03  QLXA-START-PMT-8       PIC 99999.
           03  QLXA-END-PMT-8         PIC 99999.
           03  QLXA-NET-CHANGE-8      PIC S9(7)V99.
           03  QLXA-START-PMT-9       PIC 99999.
           03  QLXA-END-PMT-9         PIC 99999.
           03  QLXA-NET-CHANGE-9      PIC S9(7)V99.
           03  QLXA-LN-INSCOMP        PIC 99.
           03  QLXA-LN-INSOURS        PIC X.
           03  QLXA-LN-INSEFF-DATE    PIC X(10).
           03  QLXA-LN-INSEXP-DATE    PIC X(10).
           03  QLXA-LN-INSPREM        PIC S9(7)V99.
           03  QLXA-LN-INSCOVR        PIC S9(7)V99.
           03  QLXA-LN-INSCOMM        PIC S9(7)V99.
           03  QLXA-LN-INS-REBATE     PIC X.
           03  QLXA-LN-INSCODES       PIC X(3).
           03  QLXZ-CODE             PIC X.
           03  QLXZ-POST-REGPY       PIC S9(7)V99.
           03  QLXZ-POST-EXTRA-PRIN  PIC S9(7)V99.
           03  QLXZ-POST-OTH         PIC S9(7)V99.
           03  QLXZ-POST-LCHG        PIC S9(7)V99.
           03  QLXZ-PDUE-APINT       PIC S9(7)V99.
           03  QLXZ-PDUE-APPRIN      PIC S9(7)V99.
           03  QLXZ-CUR-APINT        PIC S9(7)V99.
           03  QLXZ-CUR-APPRIN       PIC S9(7)V99.
           03  QLXR-CODE             PIC X.
           03  QLXR-REB-AMT          PIC S9(7)V99.
           03  QLXX-CODE               PIC X.
           03  QLXX-INS-TAX-COUNTY-CD  PIC X(3).
           03  QLXX-INS-TAX-CITY-CD    PIC X(3).
           03  QLXM-CODE               PIC X.
           03  QLXM-MIP-CLPAID         PIC S9(7)V99.
           03  QLXM-MIP-CLPDTH-DATE    PIC X(10).
           03  QLXM-MIP-AHPAID         PIC S9(7)V99.
           03  QLXM-MIP-AHPDTH-DATE    PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).
           03  PROCESS-KEY             PIC 9(01).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-SEQNO               PIC 9(05).
           03  END-SEQNO               PIC 9(05).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(27) VALUE "BRNO    ACCTNO    SEQNO    ".
           03  FILLER PIC X(26) VALUE "R REBAMT     T EARN 1     ".
           03  FILLER PIC X(30) VALUE "P EARN 1".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LX-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LX-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LX-SEQNO                   PIC 9(05).
           03  FILLER                         PIC X(03).
           03  D01-LX-R-REB-AMT               PIC Z(6)9.99.
           03  FILLER                         PIC X(03).
           03  D01-LX-T-EARN1               PIC Z(6)9.99.
           03  FILLER                         PIC X(03).
           03  D01-LX-P-EARN1               PIC Z(6)9.99.

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

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOLXRN_DEF.CPY".
       COPY "LIBIO/IOLXRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLXRN_SCN.CPY".

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
                 PR-FILE.

       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
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

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE SPACES TO SENDMESS-HEAD.

           STRING "PROGRAM COMPLETED AT ", TIME-EDIT
                  DELIMITED BY SIZE INTO SENDMESS-HEAD.

           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOLXRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLXRN-QUEUE-POS  TO Q-POS.
           MOVE IOLXRN-COPIES-POS TO C-POS.
           MOVE IOLXRN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TIME
              MOVE SPACES TO LEGEND
              STRING "STARTING TIME: ", TIME-EDIT
                     DELIMITED BY SIZE INTO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
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

           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-END-SEQNO.

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

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES ) OR
              ( VDUNUM0 > 1      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================

       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "ENNN080END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *================================================================
       ENTER-BEG-SEQNO.

           MOVE "ENNN050BEG-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-END-SEQNO.

           MOVE "ENNN050END-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQNO.

           IF ( BEG-SEQNO > END-SEQNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQNO.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN050BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN050END-SEQNO." TO VDU.
           MOVE 99999               TO VDUNUM0  END-SEQNO.
           PERFORM SCREENIO.
      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
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

           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050END-SEQNO."       TO VDU.
           MOVE END-SEQNO                 TO VDUNUM0.
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

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

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

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE PACE-COUNTER-MAX TO PACE-COUNTER.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LX-PATH-OVERRIDE.


           MOVE BR-NO     TO QLX1-WBEG-BRNO
                              QLX1-WEND-BRNO.
           MOVE BEG-ACCTNO TO QLX1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLX1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLX1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLX1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LX1-FILE.
           PERFORM START-LX1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LX1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LX-BRNO NOT = LX-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.


           IF ( LX-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LX-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LX-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.


           MOVE LX-BRNO                    TO D01-LX-BRNO.
           MOVE LX-ACCTNO                  TO D01-LX-ACCTNO.
           MOVE LX-SEQNO                   TO D01-LX-SEQNO.
           IF LXT-CODE = "T"
              MOVE LXT-EARN(1) TO D01-LX-T-EARN1.
           IF LXT-CODE = "P"
              MOVE LXP-EARN(1) TO D01-LX-P-EARN1.
           IF LXR-CODE = "R"
              MOVE LXR-REB-AMT TO D01-LX-R-REB-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LX1-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE LX1-KEY TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

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
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "BEGINNING SEQNO               : "    TO R01-PROMPT.
           MOVE BEG-SEQNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQNO               : "    TO R01-PROMPT.
           MOVE END-SEQNO                             TO R01-NUMERIC.
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
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - LXFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOLXRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLXRN-SYSDATE.
           DISPLAY IOLXRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLXRN-FORM-FIELDS TO WR-BUF.
           MOVE IOLXRN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOLXRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLXRN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOLXRN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/RMFILE.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD LX-REC FROM QLX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LX-FIELDS.
           MOVE QLX-BRNO TO LX-BRNO.
           MOVE QLX-ACCTNO TO LX-ACCTNO.
           MOVE QLX-SEQNO TO LX-SEQNO.

           IF ( QLX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LX-LTOUCH-DATE
           ELSE
              MOVE QLX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LX-LTOUCH-DATE.

           MOVE QLXT-CODE TO LXT-CODE.
           MOVE QLXT-EARN-1 TO LXT-EARN(1).
           MOVE QLXT-EARN-2 TO LXT-EARN(2).
           MOVE QLXT-EARN-3 TO LXT-EARN(3).
           MOVE QLXT-EARN-4 TO LXT-EARN(4).
           MOVE QLXT-EARN-5 TO LXT-EARN(5).
           MOVE QLXT-EARN-6 TO LXT-EARN(6).
           MOVE QLXT-EARN-7 TO LXT-EARN(7).
           MOVE QLXT-EARN-8 TO LXT-EARN(8).
           MOVE QLXT-EARN-9 TO LXT-EARN(9).
           MOVE QLXT-EARN-10 TO LXT-EARN(10).
           MOVE QLXT-EARN-11 TO LXT-EARN(11).
           MOVE QLXT-EARN-12 TO LXT-EARN(12).
           MOVE QLXT-EARN-13 TO LXT-EARN(13).
           MOVE QLXT-EARN-14 TO LXT-EARN(14).
           MOVE QLXT-EARN-15 TO LXT-EARN(15).
           MOVE QLXT-EARN-16 TO LXT-EARN(16).
           MOVE QLXT-EARN-17 TO LXT-EARN(17).
           MOVE QLXT-EARN-18 TO LXT-EARN(18).
           MOVE QLXT-EARN-19 TO LXT-EARN(19).
           MOVE QLXT-EARN-20 TO LXT-EARN(20).
           MOVE QLXP-CODE TO LXP-CODE.
           MOVE QLXP-EARN-1 TO LXP-EARN(1).
           MOVE QLXP-EARN-2 TO LXP-EARN(2).
           MOVE QLXP-EARN-3 TO LXP-EARN(3).
           MOVE QLXP-EARN-4 TO LXP-EARN(4).
           MOVE QLXP-EARN-5 TO LXP-EARN(5).
           MOVE QLXP-EARN-6 TO LXP-EARN(6).
           MOVE QLXP-EARN-7 TO LXP-EARN(7).
           MOVE QLXP-EARN-8 TO LXP-EARN(8).
           MOVE QLXP-EARN-9 TO LXP-EARN(9).
           MOVE QLXP-EARN-10 TO LXP-EARN(10).
           MOVE QLXP-EARN-11 TO LXP-EARN(11).
           MOVE QLXP-EARN-12 TO LXP-EARN(12).
           MOVE QLXP-EARN-13 TO LXP-EARN(13).
           MOVE QLXP-EARN-14 TO LXP-EARN(14).
           MOVE QLXP-EARN-15 TO LXP-EARN(15).
           MOVE QLXP-EARN-16 TO LXP-EARN(16).
           MOVE QLXP-EARN-17 TO LXP-EARN(17).
           MOVE QLXP-EARN-18 TO LXP-EARN(18).
           MOVE QLXP-EARN-19 TO LXP-EARN(19).
           MOVE QLXP-EARN-20 TO LXP-EARN(20).
           MOVE QLXA-CODE TO LXA-CODE.
           MOVE QLXA-SRCD TO LXA-SRCD.
           MOVE QLXA-START-PMT-1 TO LXA-START-PMT(1).
           MOVE QLXA-END-PMT-1 TO LXA-END-PMT(1).
           MOVE QLXA-NET-CHANGE-1 TO LXA-NET-CHANGE(1).
           MOVE QLXA-START-PMT-2 TO LXA-START-PMT(2).
           MOVE QLXA-END-PMT-2 TO LXA-END-PMT(2).
           MOVE QLXA-NET-CHANGE-2 TO LXA-NET-CHANGE(2).
           MOVE QLXA-START-PMT-3 TO LXA-START-PMT(3).
           MOVE QLXA-END-PMT-3 TO LXA-END-PMT(3).
           MOVE QLXA-NET-CHANGE-3 TO LXA-NET-CHANGE(3).
           MOVE QLXA-START-PMT-4 TO LXA-START-PMT(4).
           MOVE QLXA-END-PMT-4 TO LXA-END-PMT(4).
           MOVE QLXA-NET-CHANGE-4 TO LXA-NET-CHANGE(4).
           MOVE QLXA-START-PMT-5 TO LXA-START-PMT(5).
           MOVE QLXA-END-PMT-5 TO LXA-END-PMT(5).
           MOVE QLXA-NET-CHANGE-5 TO LXA-NET-CHANGE(5).
           MOVE QLXA-START-PMT-6 TO LXA-START-PMT(6).
           MOVE QLXA-END-PMT-6 TO LXA-END-PMT(6).
           MOVE QLXA-NET-CHANGE-6 TO LXA-NET-CHANGE(6).
           MOVE QLXA-START-PMT-7 TO LXA-START-PMT(7).
           MOVE QLXA-END-PMT-7 TO LXA-END-PMT(7).
           MOVE QLXA-NET-CHANGE-7 TO LXA-NET-CHANGE(7).
           MOVE QLXA-START-PMT-8 TO LXA-START-PMT(8).
           MOVE QLXA-END-PMT-8 TO LXA-END-PMT(8).
           MOVE QLXA-NET-CHANGE-8 TO LXA-NET-CHANGE(8).
           MOVE QLXA-START-PMT-9 TO LXA-START-PMT(9).
           MOVE QLXA-END-PMT-9 TO LXA-END-PMT(9).
           MOVE QLXA-NET-CHANGE-9 TO LXA-NET-CHANGE(9).
           MOVE QLXA-LN-INSCOMP TO LXA-LN-INSCOMP.
           MOVE QLXA-LN-INSOURS TO LXA-LN-INSOURS.

           IF ( QLXA-LN-INSEFF-DATE = "1900-01-01" )
              MOVE ZEROES TO LXA-LN-INSEFF-DATE
           ELSE
              MOVE QLXA-LN-INSEFF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LXA-LN-INSEFF-DATE.


           IF ( QLXA-LN-INSEXP-DATE = "1900-01-01" )
              MOVE ZEROES TO LXA-LN-INSEXP-DATE
           ELSE
              MOVE QLXA-LN-INSEXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LXA-LN-INSEXP-DATE.

           MOVE QLXA-LN-INSPREM TO LXA-LN-INSPREM.
           MOVE QLXA-LN-INSCOVR TO LXA-LN-INSCOVR.
           MOVE QLXA-LN-INSCOMM TO LXA-LN-INSCOMM.
           MOVE QLXA-LN-INS-REBATE TO LXA-LN-INS-REBATE.
           MOVE QLXA-LN-INSCODES TO LXA-LN-INSCODES.
           MOVE QLXZ-CODE TO LXZ-CODE.
           MOVE QLXZ-POST-REGPY TO LXZ-POST-REGPY.
           MOVE QLXZ-POST-EXTRA-PRIN TO LXZ-POST-EXTRA-PRIN.
           MOVE QLXZ-POST-OTH TO LXZ-POST-OTH.
           MOVE QLXZ-POST-LCHG TO LXZ-POST-LCHG.
           MOVE QLXZ-PDUE-APINT TO LXZ-PDUE-APINT.
           MOVE QLXZ-PDUE-APPRIN TO LXZ-PDUE-APPRIN.
           MOVE QLXZ-CUR-APINT TO LXZ-CUR-APINT.
           MOVE QLXZ-CUR-APPRIN TO LXZ-CUR-APPRIN.
           MOVE QLXR-CODE TO LXR-CODE.
           MOVE QLXR-REB-AMT TO LXR-REB-AMT.
           MOVE QLXX-CODE TO LXX-CODE.
           MOVE QLXX-INS-TAX-COUNTY-CD TO LXX-INS-TAX-COUNTY-CD.
           MOVE QLXX-INS-TAX-CITY-CD TO LXX-INS-TAX-CITY-CD.
           MOVE QLXM-CODE TO LXM-CODE.
           MOVE QLXM-MIP-CLPAID TO LXM-MIP-CLPAID.

           IF ( QLXM-MIP-CLPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LXM-MIP-CLPDTH-DATE
           ELSE
              MOVE QLXM-MIP-CLPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LXM-MIP-CLPDTH-DATE.

           MOVE QLXM-MIP-AHPAID TO LXM-MIP-AHPAID.

           IF ( QLXM-MIP-AHPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LXM-MIP-AHPDTH-DATE
           ELSE
              MOVE QLXM-MIP-AHPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LXM-MIP-AHPDTH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QLX-REC FROM LX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LX-FIELDS.
           MOVE LX-BRNO TO QLX-BRNO.
           MOVE LX-ACCTNO TO QLX-ACCTNO.
           MOVE LX-SEQNO TO QLX-SEQNO.

           IF ( LX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLX-LTOUCH-DATE
           ELSE
              MOVE LX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLX-LTOUCH-DATE.

           MOVE LXT-CODE TO QLXT-CODE.
           MOVE LXT-EARN(1) TO QLXT-EARN-1.
           MOVE LXT-EARN(2) TO QLXT-EARN-2.
           MOVE LXT-EARN(3) TO QLXT-EARN-3.
           MOVE LXT-EARN(4) TO QLXT-EARN-4.
           MOVE LXT-EARN(5) TO QLXT-EARN-5.
           MOVE LXT-EARN(6) TO QLXT-EARN-6.
           MOVE LXT-EARN(7) TO QLXT-EARN-7.
           MOVE LXT-EARN(8) TO QLXT-EARN-8.
           MOVE LXT-EARN(9) TO QLXT-EARN-9.
           MOVE LXT-EARN(10) TO QLXT-EARN-10.
           MOVE LXT-EARN(11) TO QLXT-EARN-11.
           MOVE LXT-EARN(12) TO QLXT-EARN-12.
           MOVE LXT-EARN(13) TO QLXT-EARN-13.
           MOVE LXT-EARN(14) TO QLXT-EARN-14.
           MOVE LXT-EARN(15) TO QLXT-EARN-15.
           MOVE LXT-EARN(16) TO QLXT-EARN-16.
           MOVE LXT-EARN(17) TO QLXT-EARN-17.
           MOVE LXT-EARN(18) TO QLXT-EARN-18.
           MOVE LXT-EARN(19) TO QLXT-EARN-19.
           MOVE LXT-EARN(20) TO QLXT-EARN-20.
           MOVE LXP-CODE TO QLXP-CODE.
           MOVE LXP-EARN(1) TO QLXP-EARN-1.
           MOVE LXP-EARN(2) TO QLXP-EARN-2.
           MOVE LXP-EARN(3) TO QLXP-EARN-3.
           MOVE LXP-EARN(4) TO QLXP-EARN-4.
           MOVE LXP-EARN(5) TO QLXP-EARN-5.
           MOVE LXP-EARN(6) TO QLXP-EARN-6.
           MOVE LXP-EARN(7) TO QLXP-EARN-7.
           MOVE LXP-EARN(8) TO QLXP-EARN-8.
           MOVE LXP-EARN(9) TO QLXP-EARN-9.
           MOVE LXP-EARN(10) TO QLXP-EARN-10.
           MOVE LXP-EARN(11) TO QLXP-EARN-11.
           MOVE LXP-EARN(12) TO QLXP-EARN-12.
           MOVE LXP-EARN(13) TO QLXP-EARN-13.
           MOVE LXP-EARN(14) TO QLXP-EARN-14.
           MOVE LXP-EARN(15) TO QLXP-EARN-15.
           MOVE LXP-EARN(16) TO QLXP-EARN-16.
           MOVE LXP-EARN(17) TO QLXP-EARN-17.
           MOVE LXP-EARN(18) TO QLXP-EARN-18.
           MOVE LXP-EARN(19) TO QLXP-EARN-19.
           MOVE LXP-EARN(20) TO QLXP-EARN-20.
           MOVE LXA-CODE TO QLXA-CODE.
           MOVE LXA-SRCD TO QLXA-SRCD.
           MOVE LXA-START-PMT(1) TO QLXA-START-PMT-1.
           MOVE LXA-END-PMT(1) TO QLXA-END-PMT-1.
           MOVE LXA-NET-CHANGE(1) TO QLXA-NET-CHANGE-1.
           MOVE LXA-START-PMT(2) TO QLXA-START-PMT-2.
           MOVE LXA-END-PMT(2) TO QLXA-END-PMT-2.
           MOVE LXA-NET-CHANGE(2) TO QLXA-NET-CHANGE-2.
           MOVE LXA-START-PMT(3) TO QLXA-START-PMT-3.
           MOVE LXA-END-PMT(3) TO QLXA-END-PMT-3.
           MOVE LXA-NET-CHANGE(3) TO QLXA-NET-CHANGE-3.
           MOVE LXA-START-PMT(4) TO QLXA-START-PMT-4.
           MOVE LXA-END-PMT(4) TO QLXA-END-PMT-4.
           MOVE LXA-NET-CHANGE(4) TO QLXA-NET-CHANGE-4.
           MOVE LXA-START-PMT(5) TO QLXA-START-PMT-5.
           MOVE LXA-END-PMT(5) TO QLXA-END-PMT-5.
           MOVE LXA-NET-CHANGE(5) TO QLXA-NET-CHANGE-5.
           MOVE LXA-START-PMT(6) TO QLXA-START-PMT-6.
           MOVE LXA-END-PMT(6) TO QLXA-END-PMT-6.
           MOVE LXA-NET-CHANGE(6) TO QLXA-NET-CHANGE-6.
           MOVE LXA-START-PMT(7) TO QLXA-START-PMT-7.
           MOVE LXA-END-PMT(7) TO QLXA-END-PMT-7.
           MOVE LXA-NET-CHANGE(7) TO QLXA-NET-CHANGE-7.
           MOVE LXA-START-PMT(8) TO QLXA-START-PMT-8.
           MOVE LXA-END-PMT(8) TO QLXA-END-PMT-8.
           MOVE LXA-NET-CHANGE(8) TO QLXA-NET-CHANGE-8.
           MOVE LXA-START-PMT(9) TO QLXA-START-PMT-9.
           MOVE LXA-END-PMT(9) TO QLXA-END-PMT-9.
           MOVE LXA-NET-CHANGE(9) TO QLXA-NET-CHANGE-9.
           MOVE LXA-LN-INSCOMP TO QLXA-LN-INSCOMP.
           MOVE LXA-LN-INSOURS TO QLXA-LN-INSOURS.

           IF ( LXA-LN-INSEFF-DATE = ZEROES )
              MOVE "1900-01-01" TO QLXA-LN-INSEFF-DATE
           ELSE
              MOVE LXA-LN-INSEFF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLXA-LN-INSEFF-DATE.


           IF ( LXA-LN-INSEXP-DATE = ZEROES )
              MOVE "1900-01-01" TO QLXA-LN-INSEXP-DATE
           ELSE
              MOVE LXA-LN-INSEXP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLXA-LN-INSEXP-DATE.

           MOVE LXA-LN-INSPREM TO QLXA-LN-INSPREM.
           MOVE LXA-LN-INSCOVR TO QLXA-LN-INSCOVR.
           MOVE LXA-LN-INSCOMM TO QLXA-LN-INSCOMM.
           MOVE LXA-LN-INS-REBATE TO QLXA-LN-INS-REBATE.
           MOVE LXA-LN-INSCODES TO QLXA-LN-INSCODES.
           MOVE LXZ-CODE TO QLXZ-CODE.
           MOVE LXZ-POST-REGPY TO QLXZ-POST-REGPY.
           MOVE LXZ-POST-EXTRA-PRIN TO QLXZ-POST-EXTRA-PRIN.
           MOVE LXZ-POST-OTH TO QLXZ-POST-OTH.
           MOVE LXZ-POST-LCHG TO QLXZ-POST-LCHG.
           MOVE LXZ-PDUE-APINT TO QLXZ-PDUE-APINT.
           MOVE LXZ-PDUE-APPRIN TO QLXZ-PDUE-APPRIN.
           MOVE LXZ-CUR-APINT TO QLXZ-CUR-APINT.
           MOVE LXZ-CUR-APPRIN TO QLXZ-CUR-APPRIN.
           MOVE LXR-CODE TO QLXR-CODE.
           MOVE LXR-REB-AMT TO QLXR-REB-AMT.
           MOVE LXX-CODE TO QLXX-CODE.
           MOVE LXX-INS-TAX-COUNTY-CD TO QLXX-INS-TAX-COUNTY-CD.
           MOVE LXX-INS-TAX-CITY-CD TO QLXX-INS-TAX-CITY-CD.
           MOVE LXM-CODE TO QLXM-CODE.
           MOVE LXM-MIP-CLPAID TO QLXM-MIP-CLPAID.

           IF ( LXM-MIP-CLPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLXM-MIP-CLPDTH-DATE
           ELSE
              MOVE LXM-MIP-CLPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLXM-MIP-CLPDTH-DATE.

           MOVE LXM-MIP-AHPAID TO QLXM-MIP-AHPAID.

           IF ( LXM-MIP-AHPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLXM-MIP-AHPDTH-DATE
           ELSE
              MOVE LXM-MIP-AHPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLXM-MIP-AHPDTH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LX-ALL-DATA
              MOVE LX-ALL-PATH             TO LX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LX-ALL-BASE
              MOVE FILPATH-MACHINE         TO LX-ALL-MACHINE
              MOVE FILPATH-DATA            TO LX-ALL-DATA
              MOVE LX-ALL-PATH             TO LX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LXFILE NOT = "S" )
              MOVE LX-PATH-BEG             TO LX-SELECT-PATH-BEG
              MOVE LX-PATH-FILE            TO LX-SELECT-PATH-FILE
           ELSE
              MOVE LX-PATH-SQL             TO LX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LX1-FILE.
           PERFORM LOAD-LX1-FILE.
           PERFORM OPEN-IT.
           MOVE LX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LX1-FILE.
           PERFORM START-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           MOVE LX-SELECT-PATH TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.

           IF ( LX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LX1-FILE.

           IF ( QLX1-WEND-SEQNO = ZEROES    ) OR
              ( QLX1-WEND-SEQNO NOT NUMERIC )
              MOVE LX-PATH-OWNBR        TO QLX1-WBEG-BRNO
              MOVE LX-PATH-OWNBR        TO QLX1-WEND-BRNO
              MOVE LX-ACCTNO            TO QLX1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLX1-WEND-ACCTNO
              MOVE LX-SEQNO             TO QLX1-WBEG-SEQNO
              MOVE ALL "9"              TO QLX1-WEND-SEQNO.

           EXEC SQL
            DECLARE LX1_CURSOR CURSOR FOR
            SELECT LXFILE.LX_BRNO,
                   LXFILE.LX_ACCTNO,
                   LXFILE.LX_SEQNO,
                   ISNULL(CAST(LXFILE.LX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LXFILE.LXT_CODE,''),
                   LXFILE.LXT_EARN_1,
                   LXFILE.LXT_EARN_2,
                   LXFILE.LXT_EARN_3,
                   LXFILE.LXT_EARN_4,
                   LXFILE.LXT_EARN_5,
                   LXFILE.LXT_EARN_6,
                   LXFILE.LXT_EARN_7,
                   LXFILE.LXT_EARN_8,
                   LXFILE.LXT_EARN_9,
                   LXFILE.LXT_EARN_10,
                   LXFILE.LXT_EARN_11,
                   LXFILE.LXT_EARN_12,
                   LXFILE.LXT_EARN_13,
                   LXFILE.LXT_EARN_14,
                   LXFILE.LXT_EARN_15,
                   LXFILE.LXT_EARN_16,
                   LXFILE.LXT_EARN_17,
                   LXFILE.LXT_EARN_18,
                   LXFILE.LXT_EARN_19,
                   LXFILE.LXT_EARN_20,
                   ISNULL(LXFILE.LXP_CODE,''),
                   LXFILE.LXP_EARN_1,
                   LXFILE.LXP_EARN_2,
                   LXFILE.LXP_EARN_3,
                   LXFILE.LXP_EARN_4,
                   LXFILE.LXP_EARN_5,
                   LXFILE.LXP_EARN_6,
                   LXFILE.LXP_EARN_7,
                   LXFILE.LXP_EARN_8,
                   LXFILE.LXP_EARN_9,
                   LXFILE.LXP_EARN_10,
                   LXFILE.LXP_EARN_11,
                   LXFILE.LXP_EARN_12,
                   LXFILE.LXP_EARN_13,
                   LXFILE.LXP_EARN_14,
                   LXFILE.LXP_EARN_15,
                   LXFILE.LXP_EARN_16,
                   LXFILE.LXP_EARN_17,
                   LXFILE.LXP_EARN_18,
                   LXFILE.LXP_EARN_19,
                   LXFILE.LXP_EARN_20,
                   ISNULL(LXFILE.LXA_CODE,''),
                   ISNULL(LXFILE.LXA_SRCD,''),
                   LXFILE.LXA_START_PMT_1,
                   LXFILE.LXA_END_PMT_1,
                   LXFILE.LXA_NET_CHANGE_1,
                   LXFILE.LXA_START_PMT_2,
                   LXFILE.LXA_END_PMT_2,
                   LXFILE.LXA_NET_CHANGE_2,
                   LXFILE.LXA_START_PMT_3,
                   LXFILE.LXA_END_PMT_3,
                   LXFILE.LXA_NET_CHANGE_3,
                   LXFILE.LXA_START_PMT_4,
                   LXFILE.LXA_END_PMT_4,
                   LXFILE.LXA_NET_CHANGE_4,
                   LXFILE.LXA_START_PMT_5,
                   LXFILE.LXA_END_PMT_5,
                   LXFILE.LXA_NET_CHANGE_5,
                   LXFILE.LXA_START_PMT_6,
                   LXFILE.LXA_END_PMT_6,
                   LXFILE.LXA_NET_CHANGE_6,
                   LXFILE.LXA_START_PMT_7,
                   LXFILE.LXA_END_PMT_7,
                   LXFILE.LXA_NET_CHANGE_7,
                   LXFILE.LXA_START_PMT_8,
                   LXFILE.LXA_END_PMT_8,
                   LXFILE.LXA_NET_CHANGE_8,
                   LXFILE.LXA_START_PMT_9,
                   LXFILE.LXA_END_PMT_9,
                   LXFILE.LXA_NET_CHANGE_9,
                   LXFILE.LXA_LN_INSCOMP,
                   ISNULL(LXFILE.LXA_LN_INSOURS,''),
                   ISNULL(CAST(LXFILE.LXA_LN_INSEFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LXFILE.LXA_LN_INSEXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   LXFILE.LXA_LN_INSPREM,
                   LXFILE.LXA_LN_INSCOVR,
                   LXFILE.LXA_LN_INSCOMM,
                   ISNULL(LXFILE.LXA_LN_INS_REBATE,''),
                   ISNULL(LXFILE.LXA_LN_INSCODES,''),
                   ISNULL(LXFILE.LXZ_CODE,''),
                   LXFILE.LXZ_POST_REGPY,
                   LXFILE.LXZ_POST_EXTRA_PRIN,
                   LXFILE.LXZ_POST_OTH,
                   LXFILE.LXZ_POST_LCHG,
                   LXFILE.LXZ_PDUE_APINT,
                   LXFILE.LXZ_PDUE_APPRIN,
                   LXFILE.LXZ_CUR_APINT,
                   LXFILE.LXZ_CUR_APPRIN,
                   ISNULL(LXFILE.LXR_CODE,''),
                   LXFILE.LXR_REB_AMT,
                   ISNULL(LXFILE.LXX_CODE,''),
                   ISNULL(LXFILE.LXX_INS_TAX_COUNTY_CD,''),
                   ISNULL(LXFILE.LXX_INS_TAX_CITY_CD,''),
                   ISNULL(LXFILE.LXM_CODE,''),
                   LXFILE.LXM_MIP_CLPAID,
                   ISNULL(CAST(LXFILE.LXM_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LXFILE.LXM_MIP_AHPAID,
                   ISNULL(CAST(LXFILE.LXM_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.LXFILE
            WHERE LXFILE.LX_BRNO >= :QLX1-WBEG-BRNO
              AND LXFILE.LX_BRNO <= :QLX1-WEND-BRNO
              AND LXFILE.LX_ACCTNO >= :QLX1-WBEG-ACCTNO
              AND LXFILE.LX_ACCTNO <= :QLX1-WEND-ACCTNO
              AND LXFILE.LX_SEQNO >= :QLX1-WBEG-SEQNO
              AND LXFILE.LX_SEQNO <= :QLX1-WEND-SEQNO
            ORDER BY LXFILE.LX_BRNO,
                     LXFILE.LX_ACCTNO,
                     LXFILE.LX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLX1-WBEG-BRNO
                          QLX1-WEND-BRNO
                          QLX1-WBEG-ACCTNO
                          QLX1-WEND-ACCTNO
                          QLX1-WBEG-SEQNO
                          QLX1-WEND-SEQNO.

           MOVE STAT---GOOD TO LX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LX1-FILE.
           PERFORM READ-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO. 
           MOVE LX-SELECT-PATH TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.

           MOVE LX-BRNO   TO QLX-BRNO.
           MOVE LX-ACCTNO TO QLX-ACCTNO.
           MOVE LX-SEQNO TO QLX-SEQNO.

           EXEC SQL
            SELECT LXFILE.LX_BRNO,
                   LXFILE.LX_ACCTNO,
                   LXFILE.LX_SEQNO,
                   ISNULL(CAST(LXFILE.LX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LXFILE.LXT_CODE,''),
                   LXFILE.LXT_EARN_1,
                   LXFILE.LXT_EARN_2,
                   LXFILE.LXT_EARN_3,
                   LXFILE.LXT_EARN_4,
                   LXFILE.LXT_EARN_5,
                   LXFILE.LXT_EARN_6,
                   LXFILE.LXT_EARN_7,
                   LXFILE.LXT_EARN_8,
                   LXFILE.LXT_EARN_9,
                   LXFILE.LXT_EARN_10,
                   LXFILE.LXT_EARN_11,
                   LXFILE.LXT_EARN_12,
                   LXFILE.LXT_EARN_13,
                   LXFILE.LXT_EARN_14,
                   LXFILE.LXT_EARN_15,
                   LXFILE.LXT_EARN_16,
                   LXFILE.LXT_EARN_17,
                   LXFILE.LXT_EARN_18,
                   LXFILE.LXT_EARN_19,
                   LXFILE.LXT_EARN_20,
                   ISNULL(LXFILE.LXP_CODE,''),
                   LXFILE.LXP_EARN_1,
                   LXFILE.LXP_EARN_2,
                   LXFILE.LXP_EARN_3,
                   LXFILE.LXP_EARN_4,
                   LXFILE.LXP_EARN_5,
                   LXFILE.LXP_EARN_6,
                   LXFILE.LXP_EARN_7,
                   LXFILE.LXP_EARN_8,
                   LXFILE.LXP_EARN_9,
                   LXFILE.LXP_EARN_10,
                   LXFILE.LXP_EARN_11,
                   LXFILE.LXP_EARN_12,
                   LXFILE.LXP_EARN_13,
                   LXFILE.LXP_EARN_14,
                   LXFILE.LXP_EARN_15,
                   LXFILE.LXP_EARN_16,
                   LXFILE.LXP_EARN_17,
                   LXFILE.LXP_EARN_18,
                   LXFILE.LXP_EARN_19,
                   LXFILE.LXP_EARN_20,
                   ISNULL(LXFILE.LXA_CODE,''),
                   ISNULL(LXFILE.LXA_SRCD,''),
                   LXFILE.LXA_START_PMT_1,
                   LXFILE.LXA_END_PMT_1,
                   LXFILE.LXA_NET_CHANGE_1,
                   LXFILE.LXA_START_PMT_2,
                   LXFILE.LXA_END_PMT_2,
                   LXFILE.LXA_NET_CHANGE_2,
                   LXFILE.LXA_START_PMT_3,
                   LXFILE.LXA_END_PMT_3,
                   LXFILE.LXA_NET_CHANGE_3,
                   LXFILE.LXA_START_PMT_4,
                   LXFILE.LXA_END_PMT_4,
                   LXFILE.LXA_NET_CHANGE_4,
                   LXFILE.LXA_START_PMT_5,
                   LXFILE.LXA_END_PMT_5,
                   LXFILE.LXA_NET_CHANGE_5,
                   LXFILE.LXA_START_PMT_6,
                   LXFILE.LXA_END_PMT_6,
                   LXFILE.LXA_NET_CHANGE_6,
                   LXFILE.LXA_START_PMT_7,
                   LXFILE.LXA_END_PMT_7,
                   LXFILE.LXA_NET_CHANGE_7,
                   LXFILE.LXA_START_PMT_8,
                   LXFILE.LXA_END_PMT_8,
                   LXFILE.LXA_NET_CHANGE_8,
                   LXFILE.LXA_START_PMT_9,
                   LXFILE.LXA_END_PMT_9,
                   LXFILE.LXA_NET_CHANGE_9,
                   LXFILE.LXA_LN_INSCOMP,
                   ISNULL(LXFILE.LXA_LN_INSOURS,''),
                   ISNULL(CAST(LXFILE.LXA_LN_INSEFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LXFILE.LXA_LN_INSEXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   LXFILE.LXA_LN_INSPREM,
                   LXFILE.LXA_LN_INSCOVR,
                   LXFILE.LXA_LN_INSCOMM,
                   ISNULL(LXFILE.LXA_LN_INS_REBATE,''),
                   ISNULL(LXFILE.LXA_LN_INSCODES,''),
                   ISNULL(LXFILE.LXZ_CODE,''),
                   LXFILE.LXZ_POST_REGPY,
                   LXFILE.LXZ_POST_EXTRA_PRIN,
                   LXFILE.LXZ_POST_OTH,
                   LXFILE.LXZ_POST_LCHG,
                   LXFILE.LXZ_PDUE_APINT,
                   LXFILE.LXZ_PDUE_APPRIN,
                   LXFILE.LXZ_CUR_APINT,
                   LXFILE.LXZ_CUR_APPRIN,
                   ISNULL(LXFILE.LXR_CODE,''),
                   LXFILE.LXR_REB_AMT,
                   ISNULL(LXFILE.LXX_CODE,''),
                   ISNULL(LXFILE.LXX_INS_TAX_COUNTY_CD,''),
                   ISNULL(LXFILE.LXX_INS_TAX_CITY_CD,''),
                   ISNULL(LXFILE.LXM_CODE,''),
                   LXFILE.LXM_MIP_CLPAID,
                   ISNULL(CAST(LXFILE.LXM_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LXFILE.LXM_MIP_AHPAID,
                   ISNULL(CAST(LXFILE.LXM_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QLX-REC
            FROM DBO.LXFILE
            WHERE LXFILE.LX_BRNO = :QLX-BRNO
              AND LXFILE.LX_ACCTNO = :QLX-ACCTNO
              AND LXFILE.LX_SEQNO = :QLX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LX-FIELDS.

      *-----------------------------------------------------------------
       READ-LX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           MOVE LX-SELECT-PATH TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.
           INITIALIZE QLX-REC.

           IF ( LX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LX1_CURSOR INTO :QLX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LX1-FILE.

           IF ( LX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LX1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
      *=================================================================
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

      *    MOVE WK1-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOLXRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
