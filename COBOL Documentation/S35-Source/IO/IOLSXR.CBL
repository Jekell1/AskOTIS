       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLSXR.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-08-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLSXR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LSXFILE 
      * TABLE DESC    : LOAN TRAILER REPO INFO
      * OUTPUT FILE   : XPC/IOLSXR.PRE
      * USED FOR      : LIBLP/LPWSLSX.CPY  --- QLSX-REC
      *                 LIBLP/LPLSX1R.CPY  --- READ
      *                 LIBLP/LPLSX1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOLSXR.CBL S34 05/10/21-12:03:45 >".

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
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LPWSLSX.CPY".


      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLSX1-WHERE-BEG-KEY.
           03  QLSX1-WBEG-BRNO             PIC 9(04).
           03  QLSX1-WBEG-ACCTNO           PIC 9(08).
           03  QLSX1-WBEG-SEQNO            PIC 9(02).

       01  QLSX1-WHERE-END-KEY.
           03  QLSX1-WEND-BRNO             PIC 9(04).
           03  QLSX1-WEND-ACCTNO           PIC 9(08).
           03  QLSX1-WEND-SEQNO            PIC 9(02).

       01  QLSX-REC.
           03  QLSX-BRNO             PIC 9(4).
           03  QLSX-ACCTNO           PIC 9(8).
           03  QLSX-SEQNO            PIC 99.
           03  QLSX-LTOUCH-DATE          PIC X(10).
           03  QLSX-REPODATE             PIC X(10).
           03  QLSX-TITLE-MILEAGE        PIC 9(6).
           03  QLSX-VOLUNTARY-FG         PIC X.
           03  QLSX-PERFECT              PIC X.
           03  QLSX-AUTHORIZED-DATE      PIC X(10).
           03  QLSX-AUTHORIZED-BY        PIC X(16).
           03  QLSX-REASON               PIC X(60).
           03  QLSX-X-ASSIGNED-TO        PIC X(30).
           03  QLSX-ASSIGNED-DATE        PIC X(10).
           03  QLSX-REPO-CKNO            PIC 9(6).
           03  QLSX-REPO-CKDATE          PIC X(10).
           03  QLSX-REPO-CKAMT           PIC S9(6)V99.
           03  QLSX-STORED-ADR1          PIC X(30).
           03  QLSX-STORED-CKDATE        PIC X(10).
           03  QLSX-STORED-CKNO          PIC 9(6).
           03  QLSX-STORED-CKAMT         PIC S9(6)V99.
           03  QLSX-REDEEM-DATE          PIC X(10).
           03  QLSX-BUREAU-NOTIFIED      PIC X(10).
           03  QLSX-BUREAU-DATE          PIC X(10).
           03  QLSX-BUREAU-BY            PIC X(10).
           03  QLSX-10DAY-DATE           PIC X(10).
           03  QLSX-10DAY-BY             PIC X(10).
           03  QLSX-10DAY-COST           PIC S9(6)V99.
           03  QLSX-TITLE-APPL-DATE      PIC X(10).
           03  QLSX-TITLE-APPL-BY        PIC X(10).
           03  QLSX-TITLE-APPL-COST      PIC S9(6)V99.
           03  QLSX-COND-MILEAGE         PIC 9(6).
           03  QLSX-COND-REPORT-Y-N      PIC X.
           03  QLSX-REPAIRS-DESC-1       PIC X(60).
           03  QLSX-REPAIRS-DESC-2       PIC X(60).
           03  QLSX-REPAIRS-COST         PIC S9(6)V99.
           03  QLSX-ASSIGN-TO-1            PIC X(30).
           03  QLSX-ASSIGN-COMPLETE-DATE-1 PIC X(10).
           03  QLSX-ASSIGN-CKDATE-1        PIC X(10).
           03  QLSX-ASSIGN-CKNO-1          PIC 9(6).
           03  QLSX-ASSIGN-CKAMT-1         PIC S9(6)V99.
           03  QLSX-ASSIGN-TO-2            PIC X(30).
           03  QLSX-ASSIGN-COMPLETE-DATE-2 PIC X(10).
           03  QLSX-ASSIGN-CKDATE-2        PIC X(10).
           03  QLSX-ASSIGN-CKNO-2          PIC 9(6).
           03  QLSX-ASSIGN-CKAMT-2         PIC S9(6)V99.
           03  QLSX-OTH-EXP-1          PIC X(30).
           03  QLSX-OTH-EXP-CKDATE-1   PIC X(10).
           03  QLSX-OTH-EXP-CKNO-1     PIC 9(6).
           03  QLSX-OTH-EXP-CKAMT-1    PIC S9(6)V99.
           03  QLSX-OTH-EXP-2          PIC X(30).
           03  QLSX-OTH-EXP-CKDATE-2   PIC X(10).
           03  QLSX-OTH-EXP-CKNO-2     PIC 9(6).
           03  QLSX-OTH-EXP-CKAMT-2    PIC S9(6)V99.
           03  QLSX-OTH-EXP-3          PIC X(30).
           03  QLSX-OTH-EXP-CKDATE-3   PIC X(10).
           03  QLSX-OTH-EXP-CKNO-3     PIC 9(6).
           03  QLSX-OTH-EXP-CKAMT-3    PIC S9(6)V99.
           03  QLSX-INS-RB-REQ-DATE      PIC X(10).
           03  QLSX-INS-RB-REC-DATE      PIC X(10).
           03  QLSX-INS-RB-AMT           PIC S9(6)V99.
           03  QLSX-RB-REQ-DATE          PIC X(10).
           03  QLSX-RB-REC-DATE          PIC X(10).
           03  QLSX-RB-AMT               PIC S9(6)V99.
           03  QLSX-SIC-INS-FILE-DATE    PIC X(10).
           03  QLSX-SIC-INS-PAID-DATE    PIC X(10).
           03  QLSX-SIC-INS-PAID-AMT     PIC S9(6)V99.
           03  QLSX-BOOK-VALUE           PIC 9(6)V99.
           03  QLSX-SOLD-TO-1            PIC X(30).
           03  QLSX-SOLD-TO-2            PIC X(30).
           03  QLSX-SOLD-TO-3            PIC X(30).
           03  QLSX-SOLD-TO-DATE         PIC X(10).
           03  QLSX-SOLD-TO-AMT          PIC S9(6)V99.
           03  QLSX-DLNO                 PIC 9(4)V99.
           03  QLSX-DLR-RSV-CKNO         PIC 9(6).
           03  QLSX-DLR-RSV-CKAMT        PIC S9(6)V99.
           03  QLSX-PL-EXPENSE           PIC S9(6)V99.
           03  QLSX-DEFICIENCY-Y-N       PIC X.
           03  QLSX-NET-BAL-DUE          PIC S9(7)V99.
           03  QLSX-DEFICIENCY-BAL       PIC S9(7)V99.
           03  QLSX-AUCTION-EXPENSE      PIC S9(7)V99.
           03  QLSX-REASON-CODE          PIC XX.
           03  QLSX-TRIM                 PIC X(3).
           03  QLSX-DOORS                PIC X.
           03  QLSX-COLOR                PIC X(10).
           03  QLSX-CYLINDERS            PIC 99.
           03  QLSX-AUTO-TRANS           PIC X.
           03  QLSX-AC                   PIC X.
           03  QLSX-SROOF                PIC X.
           03  QLSX-LEATHER              PIC X.
           03  QLSX-4WD                  PIC X.
           03  QLSX-DIESEL               PIC X.
           03  QLSX-DUAL-AIR             PIC X.
           03  QLSX-EXT-CAB              PIC X.
           03  QLSX-AGENT-PHONE          PIC 9(10).
           03  QLSX-AGENT-FAX            PIC 9(10).
           03  QLSX-STORED-ADR2          PIC X(24).
           03  QLSX-X-STORED-CITY        PIC X(15).
           03  QLSX-X-STORED-STATE       PIC XX.
           03  QLSX-X-STORED-ZIP         PIC X(10).
           03  QLSX-INS-CLAIM-PENDING    PIC X.
           03  QLSX-WARRANTY-AVAILABLE   PIC X.
           03  QLSX-INS-REBATE-REQUESTED PIC X.
           03  QLSX-EXTRACTED            PIC X.
           03  QLSX-TITLE-SENT-DATE      PIC X(10).
           03  QLSX-DISPOSAL-HOLD        PIC XX.
           03  QLSX-NEXT-HLD-RVW-DATE    PIC X(10).
           03  QLSX-CASE-ID              PIC 9(10).
           03  QLSX-COLLATERAL-LOCATION  PIC XX.
           03  QLSX-VIN-MATCH            PIC X.
           03  QLSX-PERSONALS            PIC X.
           03  QLSX-KEYS                 PIC X.
           03  QLSX-DRIVES               PIC X.
           03  QLSX-DMG                  PIC X.
           03  QLSX-Y-ASSIGNED-TO        PIC X(40).
           03  QLSX-SUPPLIER-ID          PIC 9(7).
           03  QLSX-TRANSPORT-EMAIL      PIC X(40).
           03  QLSX-STORED-LOCATION      PIC X(40).
           03  QLSX-STORED-ADDRESS       PIC X(40).
           03  QLSX-Y-STORED-CITY        PIC X(20).
           03  QLSX-Y-STORED-STATE       PIC X(2).
           03  QLSX-Y-STORED-ZIP         PIC X(10).
           03  QLSX-AGENCY-NAME          PIC X(40).
           03  QLSX-AGENCY-ADDRESS       PIC X(40).
           03  QLSX-AGENCY-CITY          PIC X(20).
           03  QLSX-AGENCY-STATE         PIC X(2).
           03  QLSX-AGENCY-ZIP           PIC X(10).
           03  QLSX-AGENCY-PHONE         PIC 9(10).

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
           03  BEG-SEQNO               PIC 9(02).
           03  END-SEQNO               PIC 9(02).

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
           03  FILLER PIC X(24) VALUE "BRNO    ACCTNO    SEQ   ".
           03  FILLER PIC X(19) VALUE "REPODATE     REASON".
           03  FILLER PIC X(30) VALUE "            CKAMT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LSX-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LSX-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LSX-SEQNO                   PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-LSX-REPODATE               PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-LSX-REASON                  PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-LSX-REPO-CKAMT            PIC Z(5)9.99.
      

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
       COPY "LIBIO/IOLSXR_DEF.CPY".
       COPY "LIBIO/IOLSXR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLSXR_SCN.CPY".

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
           MOVE "IOLSXR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLSXR-QUEUE-POS  TO Q-POS.
           MOVE IOLSXR-COPIES-POS TO C-POS.
           MOVE IOLSXR-PRU-POS    TO P-POS.
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

      *=================================================================
       ENTER-BEG-SEQNO.

           MOVE "ENNN020BEG-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-END-SEQNO.

           MOVE "ENNN020END-SEQNO." TO VDU.
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

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN020BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQNO." TO VDU.
           MOVE 999                 TO VDUNUM0  END-SEQNO.
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

           MOVE "SNNN020BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQNO."       TO VDU.
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

           MOVE "B"                        TO LSX-PATH-OVERRIDE.

           MOVE BR-NO     TO QLSX1-WBEG-BRNO.
           MOVE BR-NO     TO QLSX1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLSX1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLSX1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLSX1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLSX1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LSX1-FILE.
           PERFORM START-LSX1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LSX1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LSX-BRNO NOT = LSX-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.


           IF ( LSX-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LSX-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LSX-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LSX-BRNO                    TO D01-LSX-BRNO.
           MOVE LSX-ACCTNO                  TO D01-LSX-ACCTNO.
           MOVE LSX-SEQNO       TO D01-LSX-SEQNO.
           MOVE LSX-REPODATE    TO D01-LSX-REPODATE.
           MOVE LSX-REASON      TO D01-LSX-REASON.
           MOVE LSX-REPO-CKAMT  TO D01-LSX-REPO-CKAMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LSX1-FILE.

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

           MOVE LSX1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LSXFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLSXR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLSXR-SYSDATE.
           DISPLAY IOLSXR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLSXR-FORM-FIELDS TO WR-BUF.
           MOVE IOLSXR-FORM-LIST TO WR-LIST.
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

           MOVE IOLSXR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLSXR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLSXR_EVA.CPY".
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
      *    LOAD LSX-REC FROM QLSX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LSX-FIELDS.
           MOVE QLSX-BRNO TO LSX-BRNO.
           MOVE QLSX-ACCTNO TO LSX-ACCTNO.
           MOVE QLSX-SEQNO TO LSX-SEQNO.

           IF ( QLSX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-LTOUCH-DATE
           ELSE
              MOVE QLSX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-LTOUCH-DATE.


           IF ( QLSX-REPODATE = "1900-01-01" )
              MOVE ZEROES TO LSX-REPODATE
           ELSE
              MOVE QLSX-REPODATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-REPODATE.

           MOVE QLSX-TITLE-MILEAGE TO LSX-TITLE-MILEAGE.
           MOVE QLSX-VOLUNTARY-FG TO LSX-VOLUNTARY-FG.
           MOVE QLSX-PERFECT TO LSX-PERFECT.

           IF ( QLSX-AUTHORIZED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-AUTHORIZED-DATE
           ELSE
              MOVE QLSX-AUTHORIZED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-AUTHORIZED-DATE.

           MOVE QLSX-AUTHORIZED-BY TO LSX-AUTHORIZED-BY.
           MOVE QLSX-REASON TO LSX-REASON.
           MOVE QLSX-X-ASSIGNED-TO TO LSX-X-ASSIGNED-TO.

           IF ( QLSX-ASSIGNED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-ASSIGNED-DATE
           ELSE
              MOVE QLSX-ASSIGNED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-ASSIGNED-DATE.

           MOVE QLSX-REPO-CKNO TO LSX-REPO-CKNO.

           IF ( QLSX-REPO-CKDATE = "1900-01-01" )
              MOVE ZEROES TO LSX-REPO-CKDATE
           ELSE
              MOVE QLSX-REPO-CKDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-REPO-CKDATE.

           MOVE QLSX-REPO-CKAMT TO LSX-REPO-CKAMT.
           MOVE QLSX-STORED-ADR1 TO LSX-STORED-ADR1.

           IF ( QLSX-STORED-CKDATE = "1900-01-01" )
              MOVE ZEROES TO LSX-STORED-CKDATE
           ELSE
              MOVE QLSX-STORED-CKDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-STORED-CKDATE.

           MOVE QLSX-STORED-CKNO TO LSX-STORED-CKNO.
           MOVE QLSX-STORED-CKAMT TO LSX-STORED-CKAMT.

           IF ( QLSX-REDEEM-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-REDEEM-DATE
           ELSE
              MOVE QLSX-REDEEM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-REDEEM-DATE.

           MOVE QLSX-BUREAU-NOTIFIED TO LSX-BUREAU-NOTIFIED.

           IF ( QLSX-BUREAU-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-BUREAU-DATE
           ELSE
              MOVE QLSX-BUREAU-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-BUREAU-DATE.

           MOVE QLSX-BUREAU-BY TO LSX-BUREAU-BY.

           IF ( QLSX-10DAY-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-10DAY-DATE
           ELSE
              MOVE QLSX-10DAY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-10DAY-DATE.

           MOVE QLSX-10DAY-BY TO LSX-10DAY-BY.
           MOVE QLSX-10DAY-COST TO LSX-10DAY-COST.

           IF ( QLSX-TITLE-APPL-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-TITLE-APPL-DATE
           ELSE
              MOVE QLSX-TITLE-APPL-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-TITLE-APPL-DATE.

           MOVE QLSX-TITLE-APPL-BY TO LSX-TITLE-APPL-BY.
           MOVE QLSX-TITLE-APPL-COST TO LSX-TITLE-APPL-COST.
           MOVE QLSX-COND-MILEAGE TO LSX-COND-MILEAGE.
           MOVE QLSX-COND-REPORT-Y-N TO LSX-COND-REPORT-Y-N.
           MOVE QLSX-REPAIRS-DESC-1 TO LSX-REPAIRS-DESC(1).
           MOVE QLSX-REPAIRS-DESC-2 TO LSX-REPAIRS-DESC(2).
           MOVE QLSX-REPAIRS-COST TO LSX-REPAIRS-COST.
           MOVE QLSX-ASSIGN-TO-1 TO LSX-ASSIGN-TO(1).

           IF ( QLSX-ASSIGN-COMPLETE-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO LSX-ASSIGN-COMPLETE-DATE(1)
           ELSE
              MOVE QLSX-ASSIGN-COMPLETE-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-ASSIGN-COMPLETE-DATE(1).


           IF ( QLSX-ASSIGN-CKDATE-1 = "1900-01-01" )
              MOVE ZEROES TO LSX-ASSIGN-CKDATE(1)
           ELSE
              MOVE QLSX-ASSIGN-CKDATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-ASSIGN-CKDATE(1).

           MOVE QLSX-ASSIGN-CKNO-1 TO LSX-ASSIGN-CKNO(1).
           MOVE QLSX-ASSIGN-CKAMT-1 TO LSX-ASSIGN-CKAMT(1).
           MOVE QLSX-ASSIGN-TO-2 TO LSX-ASSIGN-TO(2).

           IF ( QLSX-ASSIGN-COMPLETE-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO LSX-ASSIGN-COMPLETE-DATE(2)
           ELSE
              MOVE QLSX-ASSIGN-COMPLETE-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-ASSIGN-COMPLETE-DATE(2).


           IF ( QLSX-ASSIGN-CKDATE-2 = "1900-01-01" )
              MOVE ZEROES TO LSX-ASSIGN-CKDATE(2)
           ELSE
              MOVE QLSX-ASSIGN-CKDATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-ASSIGN-CKDATE(2).

           MOVE QLSX-ASSIGN-CKNO-2 TO LSX-ASSIGN-CKNO(2).
           MOVE QLSX-ASSIGN-CKAMT-2 TO LSX-ASSIGN-CKAMT(2).
           MOVE QLSX-OTH-EXP-1 TO LSX-OTH-EXP(1).

           IF ( QLSX-OTH-EXP-CKDATE-1 = "1900-01-01" )
              MOVE ZEROES TO LSX-OTH-EXP-CKDATE(1)
           ELSE
              MOVE QLSX-OTH-EXP-CKDATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-OTH-EXP-CKDATE(1).

           MOVE QLSX-OTH-EXP-CKNO-1 TO LSX-OTH-EXP-CKNO(1).
           MOVE QLSX-OTH-EXP-CKAMT-1 TO LSX-OTH-EXP-CKAMT(1).
           MOVE QLSX-OTH-EXP-2 TO LSX-OTH-EXP(2).

           IF ( QLSX-OTH-EXP-CKDATE-2 = "1900-01-01" )
              MOVE ZEROES TO LSX-OTH-EXP-CKDATE(2)
           ELSE
              MOVE QLSX-OTH-EXP-CKDATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-OTH-EXP-CKDATE(2).

           MOVE QLSX-OTH-EXP-CKNO-2 TO LSX-OTH-EXP-CKNO(2).
           MOVE QLSX-OTH-EXP-CKAMT-2 TO LSX-OTH-EXP-CKAMT(2).
           MOVE QLSX-OTH-EXP-3 TO LSX-OTH-EXP(3).

           IF ( QLSX-OTH-EXP-CKDATE-3 = "1900-01-01" )
              MOVE ZEROES TO LSX-OTH-EXP-CKDATE(3)
           ELSE
              MOVE QLSX-OTH-EXP-CKDATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-OTH-EXP-CKDATE(3).

           MOVE QLSX-OTH-EXP-CKNO-3 TO LSX-OTH-EXP-CKNO(3).
           MOVE QLSX-OTH-EXP-CKAMT-3 TO LSX-OTH-EXP-CKAMT(3).

           IF ( QLSX-INS-RB-REQ-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-INS-RB-REQ-DATE
           ELSE
              MOVE QLSX-INS-RB-REQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-INS-RB-REQ-DATE.


           IF ( QLSX-INS-RB-REC-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-INS-RB-REC-DATE
           ELSE
              MOVE QLSX-INS-RB-REC-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-INS-RB-REC-DATE.

           MOVE QLSX-INS-RB-AMT TO LSX-INS-RB-AMT.

           IF ( QLSX-RB-REQ-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-RB-REQ-DATE
           ELSE
              MOVE QLSX-RB-REQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-RB-REQ-DATE.


           IF ( QLSX-RB-REC-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-RB-REC-DATE
           ELSE
              MOVE QLSX-RB-REC-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-RB-REC-DATE.

           MOVE QLSX-RB-AMT TO LSX-RB-AMT.

           IF ( QLSX-SIC-INS-FILE-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-SIC-INS-FILE-DATE
           ELSE
              MOVE QLSX-SIC-INS-FILE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-SIC-INS-FILE-DATE.


           IF ( QLSX-SIC-INS-PAID-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-SIC-INS-PAID-DATE
           ELSE
              MOVE QLSX-SIC-INS-PAID-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-SIC-INS-PAID-DATE.

           MOVE QLSX-SIC-INS-PAID-AMT TO LSX-SIC-INS-PAID-AMT.
           MOVE QLSX-BOOK-VALUE TO LSX-BOOK-VALUE.
           MOVE QLSX-SOLD-TO-1 TO LSX-SOLD-TO(1).
           MOVE QLSX-SOLD-TO-2 TO LSX-SOLD-TO(2).
           MOVE QLSX-SOLD-TO-3 TO LSX-SOLD-TO(3).

           IF ( QLSX-SOLD-TO-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-SOLD-TO-DATE
           ELSE
              MOVE QLSX-SOLD-TO-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-SOLD-TO-DATE.

           MOVE QLSX-SOLD-TO-AMT TO LSX-SOLD-TO-AMT.
           MOVE QLSX-DLNO TO LSX-DLNO.
           MOVE QLSX-DLR-RSV-CKNO TO LSX-DLR-RSV-CKNO.
           MOVE QLSX-DLR-RSV-CKAMT TO LSX-DLR-RSV-CKAMT.
           MOVE QLSX-PL-EXPENSE TO LSX-PL-EXPENSE.
           MOVE QLSX-DEFICIENCY-Y-N TO LSX-DEFICIENCY-Y-N.
           MOVE QLSX-NET-BAL-DUE TO LSX-NET-BAL-DUE.
           MOVE QLSX-DEFICIENCY-BAL TO LSX-DEFICIENCY-BAL.
           MOVE QLSX-AUCTION-EXPENSE TO LSX-AUCTION-EXPENSE.
           MOVE QLSX-REASON-CODE TO LSX-REASON-CODE.
           MOVE QLSX-TRIM TO LSX-TRIM.
           MOVE QLSX-DOORS TO LSX-DOORS.
           MOVE QLSX-COLOR TO LSX-COLOR.
           MOVE QLSX-CYLINDERS TO LSX-CYLINDERS.
           MOVE QLSX-AUTO-TRANS TO LSX-AUTO-TRANS.
           MOVE QLSX-AC TO LSX-AC.
           MOVE QLSX-SROOF TO LSX-SROOF.
           MOVE QLSX-LEATHER TO LSX-LEATHER.
           MOVE QLSX-4WD TO LSX-4WD.
           MOVE QLSX-DIESEL TO LSX-DIESEL.
           MOVE QLSX-DUAL-AIR TO LSX-DUAL-AIR.
           MOVE QLSX-EXT-CAB TO LSX-EXT-CAB.
           MOVE QLSX-AGENT-PHONE TO LSX-AGENT-PHONE.
           MOVE QLSX-AGENT-FAX TO LSX-AGENT-FAX.
           MOVE QLSX-STORED-ADR2 TO LSX-STORED-ADR2.
           MOVE QLSX-X-STORED-CITY TO LSX-X-STORED-CITY.
           MOVE QLSX-X-STORED-STATE TO LSX-X-STORED-STATE.
           MOVE QLSX-X-STORED-ZIP TO LSX-X-STORED-ZIP.
           MOVE QLSX-INS-CLAIM-PENDING TO LSX-INS-CLAIM-PENDING.
           MOVE QLSX-WARRANTY-AVAILABLE TO LSX-WARRANTY-AVAILABLE.
           MOVE QLSX-INS-REBATE-REQUESTED TO LSX-INS-REBATE-REQUESTED.
           MOVE QLSX-EXTRACTED TO LSX-EXTRACTED.

           IF ( QLSX-TITLE-SENT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-TITLE-SENT-DATE
           ELSE
              MOVE QLSX-TITLE-SENT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-TITLE-SENT-DATE.

           MOVE QLSX-DISPOSAL-HOLD TO LSX-DISPOSAL-HOLD.

           IF ( QLSX-NEXT-HLD-RVW-DATE = "1900-01-01" )
              MOVE ZEROES TO LSX-NEXT-HLD-RVW-DATE
           ELSE
              MOVE QLSX-NEXT-HLD-RVW-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSX-NEXT-HLD-RVW-DATE.

           MOVE QLSX-CASE-ID TO LSX-CASE-ID.
           MOVE QLSX-COLLATERAL-LOCATION TO LSX-COLLATERAL-LOCATION.
           MOVE QLSX-VIN-MATCH TO LSX-VIN-MATCH.
           MOVE QLSX-PERSONALS TO LSX-PERSONALS.
           MOVE QLSX-KEYS TO LSX-KEYS.
           MOVE QLSX-DRIVES TO LSX-DRIVES.
           MOVE QLSX-DMG TO LSX-DMG.
           MOVE QLSX-Y-ASSIGNED-TO TO LSX-Y-ASSIGNED-TO.
           MOVE QLSX-SUPPLIER-ID TO LSX-SUPPLIER-ID.
           MOVE QLSX-TRANSPORT-EMAIL TO LSX-TRANSPORT-EMAIL.
           MOVE QLSX-STORED-LOCATION TO LSX-STORED-LOCATION.
           MOVE QLSX-STORED-ADDRESS TO LSX-STORED-ADDRESS.
           MOVE QLSX-Y-STORED-CITY TO LSX-Y-STORED-CITY.
           MOVE QLSX-Y-STORED-STATE TO LSX-Y-STORED-STATE.
           MOVE QLSX-Y-STORED-ZIP TO LSX-Y-STORED-ZIP.
           MOVE QLSX-AGENCY-NAME TO LSX-AGENCY-NAME.
           MOVE QLSX-AGENCY-ADDRESS TO LSX-AGENCY-ADDRESS.
           MOVE QLSX-AGENCY-CITY TO LSX-AGENCY-CITY.
           MOVE QLSX-AGENCY-STATE TO LSX-AGENCY-STATE.
           MOVE QLSX-AGENCY-ZIP TO LSX-AGENCY-ZIP.
           MOVE QLSX-AGENCY-PHONE TO LSX-AGENCY-PHONE.
 
      *-----------------------------------------------------------------
      *    LOAD QLSX-REC FROM LSX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LSX-FIELDS.
           MOVE LSX-BRNO TO QLSX-BRNO.
           MOVE LSX-ACCTNO TO QLSX-ACCTNO.
           MOVE LSX-SEQNO TO QLSX-SEQNO.

           IF ( LSX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-LTOUCH-DATE
           ELSE
              MOVE LSX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-LTOUCH-DATE.


           IF ( LSX-REPODATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-REPODATE
           ELSE
              MOVE LSX-REPODATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-REPODATE.

           MOVE LSX-TITLE-MILEAGE TO QLSX-TITLE-MILEAGE.
           MOVE LSX-VOLUNTARY-FG TO QLSX-VOLUNTARY-FG.
           MOVE LSX-PERFECT TO QLSX-PERFECT.

           IF ( LSX-AUTHORIZED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-AUTHORIZED-DATE
           ELSE
              MOVE LSX-AUTHORIZED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-AUTHORIZED-DATE.

           MOVE LSX-AUTHORIZED-BY TO QLSX-AUTHORIZED-BY.
           MOVE LSX-REASON TO QLSX-REASON.
           MOVE LSX-X-ASSIGNED-TO TO QLSX-X-ASSIGNED-TO.

           IF ( LSX-ASSIGNED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-ASSIGNED-DATE
           ELSE
              MOVE LSX-ASSIGNED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-ASSIGNED-DATE.

           MOVE LSX-REPO-CKNO TO QLSX-REPO-CKNO.

           IF ( LSX-REPO-CKDATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-REPO-CKDATE
           ELSE
              MOVE LSX-REPO-CKDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-REPO-CKDATE.

           MOVE LSX-REPO-CKAMT TO QLSX-REPO-CKAMT.
           MOVE LSX-STORED-ADR1 TO QLSX-STORED-ADR1.

           IF ( LSX-STORED-CKDATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-STORED-CKDATE
           ELSE
              MOVE LSX-STORED-CKDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-STORED-CKDATE.

           MOVE LSX-STORED-CKNO TO QLSX-STORED-CKNO.
           MOVE LSX-STORED-CKAMT TO QLSX-STORED-CKAMT.

           IF ( LSX-REDEEM-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-REDEEM-DATE
           ELSE
              MOVE LSX-REDEEM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-REDEEM-DATE.

           MOVE LSX-BUREAU-NOTIFIED TO QLSX-BUREAU-NOTIFIED.

           IF ( LSX-BUREAU-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-BUREAU-DATE
           ELSE
              MOVE LSX-BUREAU-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-BUREAU-DATE.

           MOVE LSX-BUREAU-BY TO QLSX-BUREAU-BY.

           IF ( LSX-10DAY-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-10DAY-DATE
           ELSE
              MOVE LSX-10DAY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-10DAY-DATE.

           MOVE LSX-10DAY-BY TO QLSX-10DAY-BY.
           MOVE LSX-10DAY-COST TO QLSX-10DAY-COST.

           IF ( LSX-TITLE-APPL-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-TITLE-APPL-DATE
           ELSE
              MOVE LSX-TITLE-APPL-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-TITLE-APPL-DATE.

           MOVE LSX-TITLE-APPL-BY TO QLSX-TITLE-APPL-BY.
           MOVE LSX-TITLE-APPL-COST TO QLSX-TITLE-APPL-COST.
           MOVE LSX-COND-MILEAGE TO QLSX-COND-MILEAGE.
           MOVE LSX-COND-REPORT-Y-N TO QLSX-COND-REPORT-Y-N.
           MOVE LSX-REPAIRS-DESC(1) TO QLSX-REPAIRS-DESC-1.
           MOVE LSX-REPAIRS-DESC(2) TO QLSX-REPAIRS-DESC-2.
           MOVE LSX-REPAIRS-COST TO QLSX-REPAIRS-COST.
           MOVE LSX-ASSIGN-TO(1) TO QLSX-ASSIGN-TO-1.

           IF ( LSX-ASSIGN-COMPLETE-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QLSX-ASSIGN-COMPLETE-DATE-1
           ELSE
              MOVE LSX-ASSIGN-COMPLETE-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-ASSIGN-COMPLETE-DATE-1.


           IF ( LSX-ASSIGN-CKDATE(1) = ZEROES )
              MOVE "1900-01-01" TO QLSX-ASSIGN-CKDATE-1
           ELSE
              MOVE LSX-ASSIGN-CKDATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-ASSIGN-CKDATE-1.

           MOVE LSX-ASSIGN-CKNO(1) TO QLSX-ASSIGN-CKNO-1.
           MOVE LSX-ASSIGN-CKAMT(1) TO QLSX-ASSIGN-CKAMT-1.
           MOVE LSX-ASSIGN-TO(2) TO QLSX-ASSIGN-TO-2.

           IF ( LSX-ASSIGN-COMPLETE-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QLSX-ASSIGN-COMPLETE-DATE-2
           ELSE
              MOVE LSX-ASSIGN-COMPLETE-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-ASSIGN-COMPLETE-DATE-2.


           IF ( LSX-ASSIGN-CKDATE(2) = ZEROES )
              MOVE "1900-01-01" TO QLSX-ASSIGN-CKDATE-2
           ELSE
              MOVE LSX-ASSIGN-CKDATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-ASSIGN-CKDATE-2.

           MOVE LSX-ASSIGN-CKNO(2) TO QLSX-ASSIGN-CKNO-2.
           MOVE LSX-ASSIGN-CKAMT(2) TO QLSX-ASSIGN-CKAMT-2.
           MOVE LSX-OTH-EXP(1) TO QLSX-OTH-EXP-1.

           IF ( LSX-OTH-EXP-CKDATE(1) = ZEROES )
              MOVE "1900-01-01" TO QLSX-OTH-EXP-CKDATE-1
           ELSE
              MOVE LSX-OTH-EXP-CKDATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-OTH-EXP-CKDATE-1.

           MOVE LSX-OTH-EXP-CKNO(1) TO QLSX-OTH-EXP-CKNO-1.
           MOVE LSX-OTH-EXP-CKAMT(1) TO QLSX-OTH-EXP-CKAMT-1.
           MOVE LSX-OTH-EXP(2) TO QLSX-OTH-EXP-2.

           IF ( LSX-OTH-EXP-CKDATE(2) = ZEROES )
              MOVE "1900-01-01" TO QLSX-OTH-EXP-CKDATE-2
           ELSE
              MOVE LSX-OTH-EXP-CKDATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-OTH-EXP-CKDATE-2.

           MOVE LSX-OTH-EXP-CKNO(2) TO QLSX-OTH-EXP-CKNO-2.
           MOVE LSX-OTH-EXP-CKAMT(2) TO QLSX-OTH-EXP-CKAMT-2.
           MOVE LSX-OTH-EXP(3) TO QLSX-OTH-EXP-3.

           IF ( LSX-OTH-EXP-CKDATE(3) = ZEROES )
              MOVE "1900-01-01" TO QLSX-OTH-EXP-CKDATE-3
           ELSE
              MOVE LSX-OTH-EXP-CKDATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-OTH-EXP-CKDATE-3.

           MOVE LSX-OTH-EXP-CKNO(3) TO QLSX-OTH-EXP-CKNO-3.
           MOVE LSX-OTH-EXP-CKAMT(3) TO QLSX-OTH-EXP-CKAMT-3.

           IF ( LSX-INS-RB-REQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-INS-RB-REQ-DATE
           ELSE
              MOVE LSX-INS-RB-REQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-INS-RB-REQ-DATE.


           IF ( LSX-INS-RB-REC-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-INS-RB-REC-DATE
           ELSE
              MOVE LSX-INS-RB-REC-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-INS-RB-REC-DATE.

           MOVE LSX-INS-RB-AMT TO QLSX-INS-RB-AMT.

           IF ( LSX-RB-REQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-RB-REQ-DATE
           ELSE
              MOVE LSX-RB-REQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-RB-REQ-DATE.


           IF ( LSX-RB-REC-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-RB-REC-DATE
           ELSE
              MOVE LSX-RB-REC-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-RB-REC-DATE.

           MOVE LSX-RB-AMT TO QLSX-RB-AMT.

           IF ( LSX-SIC-INS-FILE-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-SIC-INS-FILE-DATE
           ELSE
              MOVE LSX-SIC-INS-FILE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-SIC-INS-FILE-DATE.


           IF ( LSX-SIC-INS-PAID-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-SIC-INS-PAID-DATE
           ELSE
              MOVE LSX-SIC-INS-PAID-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-SIC-INS-PAID-DATE.

           MOVE LSX-SIC-INS-PAID-AMT TO QLSX-SIC-INS-PAID-AMT.
           MOVE LSX-BOOK-VALUE TO QLSX-BOOK-VALUE.
           MOVE LSX-SOLD-TO(1) TO QLSX-SOLD-TO-1.
           MOVE LSX-SOLD-TO(2) TO QLSX-SOLD-TO-2.
           MOVE LSX-SOLD-TO(3) TO QLSX-SOLD-TO-3.

           IF ( LSX-SOLD-TO-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-SOLD-TO-DATE
           ELSE
              MOVE LSX-SOLD-TO-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-SOLD-TO-DATE.

           MOVE LSX-SOLD-TO-AMT TO QLSX-SOLD-TO-AMT.
           MOVE LSX-DLNO TO QLSX-DLNO.
           MOVE LSX-DLR-RSV-CKNO TO QLSX-DLR-RSV-CKNO.
           MOVE LSX-DLR-RSV-CKAMT TO QLSX-DLR-RSV-CKAMT.
           MOVE LSX-PL-EXPENSE TO QLSX-PL-EXPENSE.
           MOVE LSX-DEFICIENCY-Y-N TO QLSX-DEFICIENCY-Y-N.
           MOVE LSX-NET-BAL-DUE TO QLSX-NET-BAL-DUE.
           MOVE LSX-DEFICIENCY-BAL TO QLSX-DEFICIENCY-BAL.
           MOVE LSX-AUCTION-EXPENSE TO QLSX-AUCTION-EXPENSE.
           MOVE LSX-REASON-CODE TO QLSX-REASON-CODE.
           MOVE LSX-TRIM TO QLSX-TRIM.
           MOVE LSX-DOORS TO QLSX-DOORS.
           MOVE LSX-COLOR TO QLSX-COLOR.
           MOVE LSX-CYLINDERS TO QLSX-CYLINDERS.
           MOVE LSX-AUTO-TRANS TO QLSX-AUTO-TRANS.
           MOVE LSX-AC TO QLSX-AC.
           MOVE LSX-SROOF TO QLSX-SROOF.
           MOVE LSX-LEATHER TO QLSX-LEATHER.
           MOVE LSX-4WD TO QLSX-4WD.
           MOVE LSX-DIESEL TO QLSX-DIESEL.
           MOVE LSX-DUAL-AIR TO QLSX-DUAL-AIR.
           MOVE LSX-EXT-CAB TO QLSX-EXT-CAB.
           MOVE LSX-AGENT-PHONE TO QLSX-AGENT-PHONE.
           MOVE LSX-AGENT-FAX TO QLSX-AGENT-FAX.
           MOVE LSX-STORED-ADR2 TO QLSX-STORED-ADR2.
           MOVE LSX-X-STORED-CITY TO QLSX-X-STORED-CITY.
           MOVE LSX-X-STORED-STATE TO QLSX-X-STORED-STATE.
           MOVE LSX-X-STORED-ZIP TO QLSX-X-STORED-ZIP.
           MOVE LSX-INS-CLAIM-PENDING TO QLSX-INS-CLAIM-PENDING.
           MOVE LSX-WARRANTY-AVAILABLE TO QLSX-WARRANTY-AVAILABLE.
           MOVE LSX-INS-REBATE-REQUESTED TO QLSX-INS-REBATE-REQUESTED.
           MOVE LSX-EXTRACTED TO QLSX-EXTRACTED.

           IF ( LSX-TITLE-SENT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-TITLE-SENT-DATE
           ELSE
              MOVE LSX-TITLE-SENT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-TITLE-SENT-DATE.

           MOVE LSX-DISPOSAL-HOLD TO QLSX-DISPOSAL-HOLD.

           IF ( LSX-NEXT-HLD-RVW-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSX-NEXT-HLD-RVW-DATE
           ELSE
              MOVE LSX-NEXT-HLD-RVW-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSX-NEXT-HLD-RVW-DATE.

           MOVE LSX-CASE-ID TO QLSX-CASE-ID.
           MOVE LSX-COLLATERAL-LOCATION TO QLSX-COLLATERAL-LOCATION.
           MOVE LSX-VIN-MATCH TO QLSX-VIN-MATCH.
           MOVE LSX-PERSONALS TO QLSX-PERSONALS.
           MOVE LSX-KEYS TO QLSX-KEYS.
           MOVE LSX-DRIVES TO QLSX-DRIVES.
           MOVE LSX-DMG TO QLSX-DMG.
           MOVE LSX-Y-ASSIGNED-TO TO QLSX-Y-ASSIGNED-TO.
           MOVE LSX-SUPPLIER-ID TO QLSX-SUPPLIER-ID.
           MOVE LSX-TRANSPORT-EMAIL TO QLSX-TRANSPORT-EMAIL.
           MOVE LSX-STORED-LOCATION TO QLSX-STORED-LOCATION.
           MOVE LSX-STORED-ADDRESS TO QLSX-STORED-ADDRESS.
           MOVE LSX-Y-STORED-CITY TO QLSX-Y-STORED-CITY.
           MOVE LSX-Y-STORED-STATE TO QLSX-Y-STORED-STATE.
           MOVE LSX-Y-STORED-ZIP TO QLSX-Y-STORED-ZIP.
           MOVE LSX-AGENCY-NAME TO QLSX-AGENCY-NAME.
           MOVE LSX-AGENCY-ADDRESS TO QLSX-AGENCY-ADDRESS.
           MOVE LSX-AGENCY-CITY TO QLSX-AGENCY-CITY.
           MOVE LSX-AGENCY-STATE TO QLSX-AGENCY-STATE.
           MOVE LSX-AGENCY-ZIP TO QLSX-AGENCY-ZIP.
           MOVE LSX-AGENCY-PHONE TO QLSX-AGENCY-PHONE.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LSX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSX-ALL-DATA
              MOVE LSX-ALL-PATH             TO LSX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSX-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSX-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSX-ALL-DATA
              MOVE LSX-ALL-PATH             TO LSX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LSXFILE NOT = "S" )
              MOVE LSX-PATH-BEG             TO LSX-SELECT-PATH-BEG
              MOVE LSX-PATH-FILE            TO LSX-SELECT-PATH-FILE
           ELSE
              MOVE LSX-PATH-SQL             TO LSX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LSX1-FILE.
           PERFORM LOAD-LSX1-FILE.
           PERFORM OPEN-IT.
           MOVE LSX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSX1-FILE.
           PERFORM START-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-SELECT-PATH TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.

           IF ( LSX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSX1-FILE.

           IF ( QLSX1-WEND-BRNO = ZEROES    ) OR
              ( QLSX1-WEND-BRNO NOT NUMERIC )
              MOVE LSX-PATH-OWNBR        TO QLSX1-WBEG-BRNO
              MOVE LSX-PATH-OWNBR        TO QLSX1-WEND-BRNO
              MOVE LSX-ACCTNO            TO QLSX1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSX1-WEND-ACCTNO
              MOVE LSX-SEQNO             TO QLSX1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSX1-WEND-SEQNO.


           EXEC SQL
            DECLARE LSX1_CURSOR CURSOR FOR
            SELECT LSXFILE.LSX_BRNO,
                   LSXFILE.LSX_ACCTNO,
                   LSXFILE.LSX_SEQNO,
                   ISNULL(CAST(LSXFILE.LSX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_REPODATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_TITLE_MILEAGE,
                   ISNULL(LSXFILE.LSX_VOLUNTARY_FG,''),
                   ISNULL(LSXFILE.LSX_PERFECT,''),
                   ISNULL(CAST(LSXFILE.LSX_AUTHORIZED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_AUTHORIZED_BY,''),
                   ISNULL(LSXFILE.LSX_REASON,''),
                   ISNULL(LSXFILE.LSX_X_ASSIGNED_TO,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGNED_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_REPO_CKNO,
                   ISNULL(CAST(LSXFILE.LSX_REPO_CKDATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_REPO_CKAMT,
                   ISNULL(LSXFILE.LSX_STORED_ADR1,''),
                   ISNULL(CAST(LSXFILE.LSX_STORED_CKDATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_STORED_CKNO,
                   LSXFILE.LSX_STORED_CKAMT,
                   ISNULL(CAST(LSXFILE.LSX_REDEEM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_BUREAU_NOTIFIED,''),
                   ISNULL(CAST(LSXFILE.LSX_BUREAU_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_BUREAU_BY,''),
                   ISNULL(CAST(LSXFILE.LSX_10DAY_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_10DAY_BY,''),
                   LSXFILE.LSX_10DAY_COST,
                   ISNULL(CAST(LSXFILE.LSX_TITLE_APPL_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_TITLE_APPL_BY,''),
                   LSXFILE.LSX_TITLE_APPL_COST,
                   LSXFILE.LSX_COND_MILEAGE,
                   ISNULL(LSXFILE.LSX_COND_REPORT_Y_N,''),
                   ISNULL(LSXFILE.LSX_REPAIRS_DESC_1,''),
                   ISNULL(LSXFILE.LSX_REPAIRS_DESC_2,''),
                   LSXFILE.LSX_REPAIRS_COST,
                   ISNULL(LSXFILE.LSX_ASSIGN_TO_1,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_CKDATE_1
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_ASSIGN_CKNO_1,
                   LSXFILE.LSX_ASSIGN_CKAMT_1,
                   ISNULL(LSXFILE.LSX_ASSIGN_TO_2,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_CKDATE_2
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_ASSIGN_CKNO_2,
                   LSXFILE.LSX_ASSIGN_CKAMT_2,
                   ISNULL(LSXFILE.LSX_OTH_EXP_1,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_1
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_1,
                   LSXFILE.LSX_OTH_EXP_CKAMT_1,
                   ISNULL(LSXFILE.LSX_OTH_EXP_2,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_2
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_2,
                   LSXFILE.LSX_OTH_EXP_CKAMT_2,
                   ISNULL(LSXFILE.LSX_OTH_EXP_3,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_3
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_3,
                   LSXFILE.LSX_OTH_EXP_CKAMT_3,
                   ISNULL(CAST(LSXFILE.LSX_INS_RB_REQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_INS_RB_REC_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_INS_RB_AMT,
                   ISNULL(CAST(LSXFILE.LSX_RB_REQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_RB_REC_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_RB_AMT,
                   ISNULL(CAST(LSXFILE.LSX_SIC_INS_FILE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_SIC_INS_PAID_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_SIC_INS_PAID_AMT,
                   LSXFILE.LSX_BOOK_VALUE,
                   ISNULL(LSXFILE.LSX_SOLD_TO_1,''),
                   ISNULL(LSXFILE.LSX_SOLD_TO_2,''),
                   ISNULL(LSXFILE.LSX_SOLD_TO_3,''),
                   ISNULL(CAST(LSXFILE.LSX_SOLD_TO_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_SOLD_TO_AMT,
                   LSXFILE.LSX_DLNO,
                   LSXFILE.LSX_DLR_RSV_CKNO,
                   LSXFILE.LSX_DLR_RSV_CKAMT,
                   LSXFILE.LSX_PL_EXPENSE,
                   ISNULL(LSXFILE.LSX_DEFICIENCY_Y_N,''),
                   LSXFILE.LSX_NET_BAL_DUE,
                   LSXFILE.LSX_DEFICIENCY_BAL,
                   LSXFILE.LSX_AUCTION_EXPENSE,
                   ISNULL(LSXFILE.LSX_REASON_CODE,''),
                   ISNULL(LSXFILE.LSX_TRIM,''),
                   ISNULL(LSXFILE.LSX_DOORS,''),
                   ISNULL(LSXFILE.LSX_COLOR,''),
                   LSXFILE.LSX_CYLINDERS,
                   ISNULL(LSXFILE.LSX_AUTO_TRANS,''),
                   ISNULL(LSXFILE.LSX_AC,''),
                   ISNULL(LSXFILE.LSX_SROOF,''),
                   ISNULL(LSXFILE.LSX_LEATHER,''),
                   ISNULL(LSXFILE.LSX_4WD,''),
                   ISNULL(LSXFILE.LSX_DIESEL,''),
                   ISNULL(LSXFILE.LSX_DUAL_AIR,''),
                   ISNULL(LSXFILE.LSX_EXT_CAB,''),
                   LSXFILE.LSX_AGENT_PHONE,
                   LSXFILE.LSX_AGENT_FAX,
                   ISNULL(LSXFILE.LSX_STORED_ADR2,''),
                   ISNULL(LSXFILE.LSX_X_STORED_CITY,''),
                   ISNULL(LSXFILE.LSX_X_STORED_STATE,''),
                   ISNULL(LSXFILE.LSX_X_STORED_ZIP,''),
                   ISNULL(LSXFILE.LSX_INS_CLAIM_PENDING,''),
                   ISNULL(LSXFILE.LSX_WARRANTY_AVAILABLE,''),
                   ISNULL(LSXFILE.LSX_INS_REBATE_REQUESTED,''),
                   ISNULL(LSXFILE.LSX_EXTRACTED,''),
                   ISNULL(CAST(LSXFILE.LSX_TITLE_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_DISPOSAL_HOLD,''),
                   ISNULL(CAST(LSXFILE.LSX_NEXT_HLD_RVW_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_CASE_ID,
                   ISNULL(LSXFILE.LSX_COLLATERAL_LOCATION,''),
                   ISNULL(LSXFILE.LSX_VIN_MATCH,''),
                   ISNULL(LSXFILE.LSX_PERSONALS,''),
                   ISNULL(LSXFILE.LSX_KEYS,''),
                   ISNULL(LSXFILE.LSX_DRIVES,''),
                   ISNULL(LSXFILE.LSX_DMG,''),
                   ISNULL(LSXFILE.LSX_Y_ASSIGNED_TO,''),
                   LSXFILE.LSX_SUPPLIER_ID,
                   ISNULL(LSXFILE.LSX_TRANSPORT_EMAIL,''),
                   ISNULL(LSXFILE.LSX_STORED_LOCATION,''),
                   ISNULL(LSXFILE.LSX_STORED_ADDRESS,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_CITY,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_STATE,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_ZIP,''),
                   ISNULL(LSXFILE.LSX_AGENCY_NAME,''),
                   ISNULL(LSXFILE.LSX_AGENCY_ADDRESS,''),
                   ISNULL(LSXFILE.LSX_AGENCY_CITY,''),
                   ISNULL(LSXFILE.LSX_AGENCY_STATE,''),
                   ISNULL(LSXFILE.LSX_AGENCY_ZIP,''),
                   LSXFILE.LSX_AGENCY_PHONE
            FROM DBO.LSXFILE
            WHERE LSXFILE.LSX_BRNO >= :QLSX1-WBEG-BRNO
              AND LSXFILE.LSX_BRNO <= :QLSX1-WEND-BRNO
              AND LSXFILE.LSX_ACCTNO >= :QLSX1-WBEG-ACCTNO
              AND LSXFILE.LSX_ACCTNO <= :QLSX1-WEND-ACCTNO
              AND LSXFILE.LSX_SEQNO >= :QLSX1-WBEG-SEQNO
              AND LSXFILE.LSX_SEQNO <= :QLSX1-WEND-SEQNO
            ORDER BY LSXFILE.LSX_BRNO,
                     LSXFILE.LSX_ACCTNO,
                     LSXFILE.LSX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSX1-WBEG-BRNO
                          QLSX1-WEND-BRNO
                          QLSX1-WBEG-ACCTNO
                          QLSX1-WEND-ACCTNO
                          QLSX1-WBEG-SEQNO
                          QLSX1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LSX1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-SELECT-PATH TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.

           IF ( LSX1-NG-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSX1-FILE.

           IF ( QLSX1-WEND-BRNO = ZEROES    ) OR
              ( QLSX1-WEND-BRNO NOT NUMERIC )
              MOVE LSX-PATH-OWNBR        TO QLSX1-WBEG-BRNO
              MOVE LSX-PATH-OWNBR        TO QLSX1-WEND-BRNO
              MOVE ALL "0"               TO QLSX1-WBEG-ACCTNO
              MOVE LSX-ACCTNO            TO QLSX1-WEND-ACCTNO
              MOVE ALL "0"               TO QLSX1-WBEG-SEQNO
              MOVE LSX-SEQNO             TO QLSX1-WEND-SEQNO.


           EXEC SQL
            DECLARE LSX1_NG_CURSOR CURSOR FOR
            SELECT LSXFILE.LSX_BRNO,
                   LSXFILE.LSX_ACCTNO,
                   LSXFILE.LSX_SEQNO,
                   ISNULL(CAST(LSXFILE.LSX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_REPODATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_TITLE_MILEAGE,
                   ISNULL(LSXFILE.LSX_VOLUNTARY_FG,''),
                   ISNULL(LSXFILE.LSX_PERFECT,''),
                   ISNULL(CAST(LSXFILE.LSX_AUTHORIZED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_AUTHORIZED_BY,''),
                   ISNULL(LSXFILE.LSX_REASON,''),
                   ISNULL(LSXFILE.LSX_X_ASSIGNED_TO,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGNED_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_REPO_CKNO,
                   ISNULL(CAST(LSXFILE.LSX_REPO_CKDATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_REPO_CKAMT,
                   ISNULL(LSXFILE.LSX_STORED_ADR1,''),
                   ISNULL(CAST(LSXFILE.LSX_STORED_CKDATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_STORED_CKNO,
                   LSXFILE.LSX_STORED_CKAMT,
                   ISNULL(CAST(LSXFILE.LSX_REDEEM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_BUREAU_NOTIFIED,''),
                   ISNULL(CAST(LSXFILE.LSX_BUREAU_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_BUREAU_BY,''),
                   ISNULL(CAST(LSXFILE.LSX_10DAY_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_10DAY_BY,''),
                   LSXFILE.LSX_10DAY_COST,
                   ISNULL(CAST(LSXFILE.LSX_TITLE_APPL_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_TITLE_APPL_BY,''),
                   LSXFILE.LSX_TITLE_APPL_COST,
                   LSXFILE.LSX_COND_MILEAGE,
                   ISNULL(LSXFILE.LSX_COND_REPORT_Y_N,''),
                   ISNULL(LSXFILE.LSX_REPAIRS_DESC_1,''),
                   ISNULL(LSXFILE.LSX_REPAIRS_DESC_2,''),
                   LSXFILE.LSX_REPAIRS_COST,
                   ISNULL(LSXFILE.LSX_ASSIGN_TO_1,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_CKDATE_1
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_ASSIGN_CKNO_1,
                   LSXFILE.LSX_ASSIGN_CKAMT_1,
                   ISNULL(LSXFILE.LSX_ASSIGN_TO_2,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_CKDATE_2
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_ASSIGN_CKNO_2,
                   LSXFILE.LSX_ASSIGN_CKAMT_2,
                   ISNULL(LSXFILE.LSX_OTH_EXP_1,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_1
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_1,
                   LSXFILE.LSX_OTH_EXP_CKAMT_1,
                   ISNULL(LSXFILE.LSX_OTH_EXP_2,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_2
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_2,
                   LSXFILE.LSX_OTH_EXP_CKAMT_2,
                   ISNULL(LSXFILE.LSX_OTH_EXP_3,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_3
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_3,
                   LSXFILE.LSX_OTH_EXP_CKAMT_3,
                   ISNULL(CAST(LSXFILE.LSX_INS_RB_REQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_INS_RB_REC_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_INS_RB_AMT,
                   ISNULL(CAST(LSXFILE.LSX_RB_REQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_RB_REC_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_RB_AMT,
                   ISNULL(CAST(LSXFILE.LSX_SIC_INS_FILE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_SIC_INS_PAID_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_SIC_INS_PAID_AMT,
                   LSXFILE.LSX_BOOK_VALUE,
                   ISNULL(LSXFILE.LSX_SOLD_TO_1,''),
                   ISNULL(LSXFILE.LSX_SOLD_TO_2,''),
                   ISNULL(LSXFILE.LSX_SOLD_TO_3,''),
                   ISNULL(CAST(LSXFILE.LSX_SOLD_TO_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_SOLD_TO_AMT,
                   LSXFILE.LSX_DLNO,
                   LSXFILE.LSX_DLR_RSV_CKNO,
                   LSXFILE.LSX_DLR_RSV_CKAMT,
                   LSXFILE.LSX_PL_EXPENSE,
                   ISNULL(LSXFILE.LSX_DEFICIENCY_Y_N,''),
                   LSXFILE.LSX_NET_BAL_DUE,
                   LSXFILE.LSX_DEFICIENCY_BAL,
                   LSXFILE.LSX_AUCTION_EXPENSE,
                   ISNULL(LSXFILE.LSX_REASON_CODE,''),
                   ISNULL(LSXFILE.LSX_TRIM,''),
                   ISNULL(LSXFILE.LSX_DOORS,''),
                   ISNULL(LSXFILE.LSX_COLOR,''),
                   LSXFILE.LSX_CYLINDERS,
                   ISNULL(LSXFILE.LSX_AUTO_TRANS,''),
                   ISNULL(LSXFILE.LSX_AC,''),
                   ISNULL(LSXFILE.LSX_SROOF,''),
                   ISNULL(LSXFILE.LSX_LEATHER,''),
                   ISNULL(LSXFILE.LSX_4WD,''),
                   ISNULL(LSXFILE.LSX_DIESEL,''),
                   ISNULL(LSXFILE.LSX_DUAL_AIR,''),
                   ISNULL(LSXFILE.LSX_EXT_CAB,''),
                   LSXFILE.LSX_AGENT_PHONE,
                   LSXFILE.LSX_AGENT_FAX,
                   ISNULL(LSXFILE.LSX_STORED_ADR2,''),
                   ISNULL(LSXFILE.LSX_X_STORED_CITY,''),
                   ISNULL(LSXFILE.LSX_X_STORED_STATE,''),
                   ISNULL(LSXFILE.LSX_X_STORED_ZIP,''),
                   ISNULL(LSXFILE.LSX_INS_CLAIM_PENDING,''),
                   ISNULL(LSXFILE.LSX_WARRANTY_AVAILABLE,''),
                   ISNULL(LSXFILE.LSX_INS_REBATE_REQUESTED,''),
                   ISNULL(LSXFILE.LSX_EXTRACTED,''),
                   ISNULL(CAST(LSXFILE.LSX_TITLE_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_DISPOSAL_HOLD,''),
                   ISNULL(CAST(LSXFILE.LSX_NEXT_HLD_RVW_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_CASE_ID,
                   ISNULL(LSXFILE.LSX_COLLATERAL_LOCATION,''),
                   ISNULL(LSXFILE.LSX_VIN_MATCH,''),
                   ISNULL(LSXFILE.LSX_PERSONALS,''),
                   ISNULL(LSXFILE.LSX_KEYS,''),
                   ISNULL(LSXFILE.LSX_DRIVES,''),
                   ISNULL(LSXFILE.LSX_DMG,''),
                   ISNULL(LSXFILE.LSX_Y_ASSIGNED_TO,''),
                   LSXFILE.LSX_SUPPLIER_ID,
                   ISNULL(LSXFILE.LSX_TRANSPORT_EMAIL,''),
                   ISNULL(LSXFILE.LSX_STORED_LOCATION,''),
                   ISNULL(LSXFILE.LSX_STORED_ADDRESS,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_CITY,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_STATE,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_ZIP,''),
                   ISNULL(LSXFILE.LSX_AGENCY_NAME,''),
                   ISNULL(LSXFILE.LSX_AGENCY_ADDRESS,''),
                   ISNULL(LSXFILE.LSX_AGENCY_CITY,''),
                   ISNULL(LSXFILE.LSX_AGENCY_STATE,''),
                   ISNULL(LSXFILE.LSX_AGENCY_ZIP,''),
                   LSXFILE.LSX_AGENCY_PHONE
            FROM DBO.LSXFILE
            WHERE LSXFILE.LSX_BRNO >= :QLSX1-WBEG-BRNO
              AND LSXFILE.LSX_BRNO <= :QLSX1-WEND-BRNO
              AND LSXFILE.LSX_ACCTNO >= :QLSX1-WBEG-ACCTNO
              AND LSXFILE.LSX_ACCTNO <= :QLSX1-WEND-ACCTNO
              AND LSXFILE.LSX_SEQNO >= :QLSX1-WBEG-SEQNO
              AND LSXFILE.LSX_SEQNO <= :QLSX1-WEND-SEQNO
            ORDER BY LSXFILE.LSX_BRNO DESC,
                     LSXFILE.LSX_ACCTNO DESC,
                     LSXFILE.LSX_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LSX1_NG_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSX1-WBEG-BRNO
                          QLSX1-WEND-BRNO
                          QLSX1-WBEG-ACCTNO
                          QLSX1-WEND-ACCTNO
                          QLSX1-WBEG-SEQNO
                          QLSX1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSX1-NG-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSX1-FILE.
           PERFORM READ-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO. 
           MOVE LSX-SELECT-PATH TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.

           MOVE LSX-BRNO   TO QLSX-BRNO.
           MOVE LSX-ACCTNO TO QLSX-ACCTNO.
           MOVE LSX-SEQNO TO QLSX-SEQNO.

           EXEC SQL
            SELECT LSXFILE.LSX_BRNO,
                   LSXFILE.LSX_ACCTNO,
                   LSXFILE.LSX_SEQNO,
                   ISNULL(CAST(LSXFILE.LSX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_REPODATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_TITLE_MILEAGE,
                   ISNULL(LSXFILE.LSX_VOLUNTARY_FG,''),
                   ISNULL(LSXFILE.LSX_PERFECT,''),
                   ISNULL(CAST(LSXFILE.LSX_AUTHORIZED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_AUTHORIZED_BY,''),
                   ISNULL(LSXFILE.LSX_REASON,''),
                   ISNULL(LSXFILE.LSX_X_ASSIGNED_TO,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGNED_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_REPO_CKNO,
                   ISNULL(CAST(LSXFILE.LSX_REPO_CKDATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_REPO_CKAMT,
                   ISNULL(LSXFILE.LSX_STORED_ADR1,''),
                   ISNULL(CAST(LSXFILE.LSX_STORED_CKDATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_STORED_CKNO,
                   LSXFILE.LSX_STORED_CKAMT,
                   ISNULL(CAST(LSXFILE.LSX_REDEEM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_BUREAU_NOTIFIED,''),
                   ISNULL(CAST(LSXFILE.LSX_BUREAU_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_BUREAU_BY,''),
                   ISNULL(CAST(LSXFILE.LSX_10DAY_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_10DAY_BY,''),
                   LSXFILE.LSX_10DAY_COST,
                   ISNULL(CAST(LSXFILE.LSX_TITLE_APPL_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_TITLE_APPL_BY,''),
                   LSXFILE.LSX_TITLE_APPL_COST,
                   LSXFILE.LSX_COND_MILEAGE,
                   ISNULL(LSXFILE.LSX_COND_REPORT_Y_N,''),
                   ISNULL(LSXFILE.LSX_REPAIRS_DESC_1,''),
                   ISNULL(LSXFILE.LSX_REPAIRS_DESC_2,''),
                   LSXFILE.LSX_REPAIRS_COST,
                   ISNULL(LSXFILE.LSX_ASSIGN_TO_1,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_CKDATE_1
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_ASSIGN_CKNO_1,
                   LSXFILE.LSX_ASSIGN_CKAMT_1,
                   ISNULL(LSXFILE.LSX_ASSIGN_TO_2,''),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_COMPLETE_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_ASSIGN_CKDATE_2
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_ASSIGN_CKNO_2,
                   LSXFILE.LSX_ASSIGN_CKAMT_2,
                   ISNULL(LSXFILE.LSX_OTH_EXP_1,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_1
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_1,
                   LSXFILE.LSX_OTH_EXP_CKAMT_1,
                   ISNULL(LSXFILE.LSX_OTH_EXP_2,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_2
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_2,
                   LSXFILE.LSX_OTH_EXP_CKAMT_2,
                   ISNULL(LSXFILE.LSX_OTH_EXP_3,''),
                   ISNULL(CAST(LSXFILE.LSX_OTH_EXP_CKDATE_3
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_OTH_EXP_CKNO_3,
                   LSXFILE.LSX_OTH_EXP_CKAMT_3,
                   ISNULL(CAST(LSXFILE.LSX_INS_RB_REQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_INS_RB_REC_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_INS_RB_AMT,
                   ISNULL(CAST(LSXFILE.LSX_RB_REQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_RB_REC_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_RB_AMT,
                   ISNULL(CAST(LSXFILE.LSX_SIC_INS_FILE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSXFILE.LSX_SIC_INS_PAID_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_SIC_INS_PAID_AMT,
                   LSXFILE.LSX_BOOK_VALUE,
                   ISNULL(LSXFILE.LSX_SOLD_TO_1,''),
                   ISNULL(LSXFILE.LSX_SOLD_TO_2,''),
                   ISNULL(LSXFILE.LSX_SOLD_TO_3,''),
                   ISNULL(CAST(LSXFILE.LSX_SOLD_TO_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_SOLD_TO_AMT,
                   LSXFILE.LSX_DLNO,
                   LSXFILE.LSX_DLR_RSV_CKNO,
                   LSXFILE.LSX_DLR_RSV_CKAMT,
                   LSXFILE.LSX_PL_EXPENSE,
                   ISNULL(LSXFILE.LSX_DEFICIENCY_Y_N,''),
                   LSXFILE.LSX_NET_BAL_DUE,
                   LSXFILE.LSX_DEFICIENCY_BAL,
                   LSXFILE.LSX_AUCTION_EXPENSE,
                   ISNULL(LSXFILE.LSX_REASON_CODE,''),
                   ISNULL(LSXFILE.LSX_TRIM,''),
                   ISNULL(LSXFILE.LSX_DOORS,''),
                   ISNULL(LSXFILE.LSX_COLOR,''),
                   LSXFILE.LSX_CYLINDERS,
                   ISNULL(LSXFILE.LSX_AUTO_TRANS,''),
                   ISNULL(LSXFILE.LSX_AC,''),
                   ISNULL(LSXFILE.LSX_SROOF,''),
                   ISNULL(LSXFILE.LSX_LEATHER,''),
                   ISNULL(LSXFILE.LSX_4WD,''),
                   ISNULL(LSXFILE.LSX_DIESEL,''),
                   ISNULL(LSXFILE.LSX_DUAL_AIR,''),
                   ISNULL(LSXFILE.LSX_EXT_CAB,''),
                   LSXFILE.LSX_AGENT_PHONE,
                   LSXFILE.LSX_AGENT_FAX,
                   ISNULL(LSXFILE.LSX_STORED_ADR2,''),
                   ISNULL(LSXFILE.LSX_X_STORED_CITY,''),
                   ISNULL(LSXFILE.LSX_X_STORED_STATE,''),
                   ISNULL(LSXFILE.LSX_X_STORED_ZIP,''),
                   ISNULL(LSXFILE.LSX_INS_CLAIM_PENDING,''),
                   ISNULL(LSXFILE.LSX_WARRANTY_AVAILABLE,''),
                   ISNULL(LSXFILE.LSX_INS_REBATE_REQUESTED,''),
                   ISNULL(LSXFILE.LSX_EXTRACTED,''),
                   ISNULL(CAST(LSXFILE.LSX_TITLE_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSXFILE.LSX_DISPOSAL_HOLD,''),
                   ISNULL(CAST(LSXFILE.LSX_NEXT_HLD_RVW_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSXFILE.LSX_CASE_ID,
                   ISNULL(LSXFILE.LSX_COLLATERAL_LOCATION,''),
                   ISNULL(LSXFILE.LSX_VIN_MATCH,''),
                   ISNULL(LSXFILE.LSX_PERSONALS,''),
                   ISNULL(LSXFILE.LSX_KEYS,''),
                   ISNULL(LSXFILE.LSX_DRIVES,''),
                   ISNULL(LSXFILE.LSX_DMG,''),
                   ISNULL(LSXFILE.LSX_Y_ASSIGNED_TO,''),
                   LSXFILE.LSX_SUPPLIER_ID,
                   ISNULL(LSXFILE.LSX_TRANSPORT_EMAIL,''),
                   ISNULL(LSXFILE.LSX_STORED_LOCATION,''),
                   ISNULL(LSXFILE.LSX_STORED_ADDRESS,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_CITY,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_STATE,''),
                   ISNULL(LSXFILE.LSX_Y_STORED_ZIP,''),
                   ISNULL(LSXFILE.LSX_AGENCY_NAME,''),
                   ISNULL(LSXFILE.LSX_AGENCY_ADDRESS,''),
                   ISNULL(LSXFILE.LSX_AGENCY_CITY,''),
                   ISNULL(LSXFILE.LSX_AGENCY_STATE,''),
                   ISNULL(LSXFILE.LSX_AGENCY_ZIP,''),
                   LSXFILE.LSX_AGENCY_PHONE
            INTO :QLSX-REC
            FROM DBO.LSXFILE
            WHERE LSXFILE.LSX_BRNO = :QLSX-BRNO
              AND LSXFILE.LSX_ACCTNO = :QLSX-ACCTNO
              AND LSXFILE.LSX_SEQNO = :QLSX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSX-FIELDS.

      *-----------------------------------------------------------------
       READ-LSX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-SELECT-PATH TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           INITIALIZE QLSX-REC.

           IF ( LSX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSX1_CURSOR INTO :QLSX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSX-FIELDS.

      *-----------------------------------------------------------------
       READ-LSX1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO.
           MOVE LSX-SELECT-PATH TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           INITIALIZE QLSX-REC.

           IF ( LSX1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSX1_NG_CURSOR INTO :QLSX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSX1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LSX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSX1-FILE.

           IF ( LSX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSX1-CURSOR-STAT.

           IF ( LSX1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSX1_NG_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSX1-NG-CURSOR-STAT.

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
              DESTROY IOLSXR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
