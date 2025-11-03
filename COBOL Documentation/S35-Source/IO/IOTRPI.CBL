       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRPI.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-13-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRPR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRPFILE 
      * TABLE DESC    : TRANSACTION PAYMENT FILE
      * OUTPUT FILE   : XPC/IOTRPR.PRE
      * USED FOR      : LIBLP/LPWSTRP.CPY  --- QTRP-REC
      *                 LIBLP/LPTRP1R.CPY  --- READ
      *                 LIBLP/LPTRP1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0413 ***NEW***
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
           "@(#)IO/IOTRPI.CBL S34 10/14/21-12:39:05 >".

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
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LPWSTRP.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRP1-WHERE-BEG-KEY.
           03  QTRP1-WBEG-BRNO             PIC 9(04).
           03  QTRP1-WBEG-DATE             PIC X(10).
           03  QTRP1-WBEG-CLASS            PIC 9(03).
           03  QTRP1-WBEG-ACCTNO           PIC 9(08).
           03  QTRP1-WBEG-LINE             PIC 9(03).

       01  QTRP1-WHERE-END-KEY.
           03  QTRP1-WEND-BRNO             PIC 9(04).
           03  QTRP1-WEND-DATE             PIC X(10).
           03  QTRP1-WEND-CLASS            PIC 9(03).
           03  QTRP1-WEND-ACCTNO           PIC 9(08).
           03  QTRP1-WEND-LINE             PIC 9(03).

 
       01  QTRP-REC.
           03  QTP-BRNO             PIC 9(4).
           03  QTP-DATE             PIC X(10).
           03  QTP-CLASS            PIC 999.
           03  QTP-ACCTNO           PIC 9(8).
           03  QTP-LINE             PIC 999.
           03  QTP-LTOUCH-DATE      PIC X(10).
           03  QTP-LAWCODE          PIC 99.
           03  QTP-JDCD             PIC X.
           03  QTP-PLCD             PIC X.
           03  QTP-SSNO             PIC 9(10).
           03  QTP-MAKER            PIC X.
           03  QTP-IBPC             PIC X.
           03  QTP-TRCD             PIC XX.
           03  QTP-REFNO            PIC X(5).
           03  QTP-PAYDATE          PIC X(10).
           03  QTP-DUEDATE          PIC X(10).
           03  QTP-TRAMT            PIC S9(7)V99.
           03  QTP-INTDUE           PIC S9(7)V99.
           03  QTP-APOTH            PIC S9(7)V99.
           03  QTP-APOT2            PIC S9(7)V99.
           03  QTP-APINT            PIC S9(7)V99.
           03  QTP-APLC             PIC S9(7)V99.
           03  QTP-APCUR            PIC S9(7)V99.
           03  QTP-GLNO-1         PIC 9(11).
           03  QTP-GLAMT-1        PIC S9(7)V99.
           03  QTP-GLNO-2         PIC 9(11).
           03  QTP-GLAMT-2        PIC S9(7)V99.
           03  QTP-GLNO-3         PIC 9(11).
           03  QTP-GLAMT-3        PIC S9(7)V99.
           03  QTP-EARNINGS-1     PIC S9(7)V99.
           03  QTP-EARNINGS-2     PIC S9(7)V99.
           03  QTP-EARNINGS-3     PIC S9(7)V99.
           03  QTP-EARNINGS-4     PIC S9(7)V99.
           03  QTP-EARNINGS-5     PIC S9(7)V99.
           03  QTP-EARNINGS-6     PIC S9(7)V99.
           03  QTP-EARNINGS-7     PIC S9(7)V99.
           03  QTP-EARNINGS-8     PIC S9(7)V99.
           03  QTP-EARNINGS-9     PIC S9(7)V99.
           03  QTP-EARNINGS-10    PIC S9(7)V99.
           03  QTP-WORKAMT-1     PIC S9(7)V99.
           03  QTP-WORKAMT-2     PIC S9(7)V99.
           03  QTP-WORKAMT-3     PIC S9(7)V99.
           03  QTP-WORKAMT-4     PIC S9(7)V99.
           03  QTP-WORKAMT-5     PIC S9(7)V99.
           03  QTP-WORKAMT-6     PIC S9(7)V99.
           03  QTP-WORKAMT-7     PIC S9(7)V99.
           03  QTP-WORKAMT-8     PIC S9(7)V99.
           03  QTP-WORKAMT-9     PIC S9(7)V99.
           03  QTP-WORKAMT-10    PIC S9(7)V99.
           03  QTP-OTHBAL           PIC S9(7)V99.
           03  QTP-OT2BAL           PIC S9(7)V99.
           03  QTP-INTBAL           PIC S9(7)V99.
           03  QTP-LCBAL            PIC S9(7)V99.
           03  QTP-CURBAL           PIC S9(7)V99.
           03  QTP-PDTH-DATE        PIC X(10).
           03  QTP-DLNO             PIC 9(4)V99.
           03  QTP-DLPROC           PIC S9(7)V99.
           03  QTP-SERVACCT         PIC X.
           03  QTP-SALEFIN          PIC X.
           03  QTP-INSURED          PIC X.
           03  QTP-INSOURS          PIC X.
           03  QTP-LOANTYPE         PIC X.
           03  QTP-TILLNO           PIC 9(2).
           03  QTP-LCPDTH-DATE      PIC X(10).
           03  QTP-POSTING-BR       PIC 9(4).
           03  QTP-OTHBR-FG         PIC X.
           03  QTP-REV              PIC X.
           03  QTP-REVTRCD          PIC XX.
           03  QTP-REVSEQNO         PIC 9(5).
           03  QTP-STATUSFG         PIC X.
           03  QTP-KEEPAPINT        PIC X.
           03  QTP-INTPAID          PIC S9(7)V99.
           03  QTP-PRIOR-POOLID     PIC 9(5).
           03  QTP-INSTYPES         PIC X.
           03  QTP-INSEARN-1        PIC S9(7)V99.
           03  QTP-INSEARN-2        PIC S9(7)V99.
           03  QTP-INSEARN-3        PIC S9(7)V99.
           03  QTP-INSEARN-4        PIC S9(7)V99.
           03  QTP-INSEARN-5        PIC S9(7)V99.
           03  QTP-INSEARN-6        PIC S9(7)V99.
           03  QTP-INSEARN-7        PIC S9(7)V99.
           03  QTP-INSEARN-8        PIC S9(7)V99.
           03  QTP-INSEARN-9        PIC S9(7)V99.
           03  QTP-INSEARN-10       PIC S9(7)V99.
           03  QTP-INSWORKAMT-1        PIC S9(7)V99.
           03  QTP-INSWORKAMT-2        PIC S9(7)V99.
           03  QTP-INSWORKAMT-3        PIC S9(7)V99.
           03  QTP-INSWORKAMT-4        PIC S9(7)V99.
           03  QTP-INSWORKAMT-5        PIC S9(7)V99.
           03  QTP-INSWORKAMT-6        PIC S9(7)V99.
           03  QTP-INSWORKAMT-7        PIC S9(7)V99.
           03  QTP-INSWORKAMT-8        PIC S9(7)V99.
           03  QTP-INSWORKAMT-9        PIC S9(7)V99.
           03  QTP-INSWORKAMT-10       PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-1      PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-2      PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-3      PIC S9(7)V99.
           03  QTP-DLPARVHELD-ADJ       PIC S9(7)V99.
           03  QTP-DLPARVPAID-ADJ       PIC S9(7)V99.
           03  QTP-NEWSTAT-RATE         PIC 999V999.
           03  QTP-POCD                 PIC XX.
           03  QTP-PODATE               PIC X(10).
           03  QTP-DL-RVCHGS-CODE-1     PIC X.
           03  QTP-DL-RVCHGS-CODE-2     PIC X.
           03  QTP-DL-RVCHGS-CODE-3     PIC X.
           03  QTP-DL-PARVHELD-CODE     PIC X.
           03  QTP-DL-PARVPAID-CODE     PIC X.
           03  QTP-TOTNODEF             PIC S999.
           03  QTP-LNAMT                PIC S9(7)V99.
           03  QTP-ORIG-1STPYDATE       PIC X(10).
           03  QTP-OLD1ST-PYDATE        PIC X(10).
           03  QTP-1STPYDATE            PIC X(10).
           03  QTP-ORGTERM              PIC 9(3).
           03  QTP-INSCOMM              PIC S9(5)V99.
           03  QTP-MANUAL-RB            PIC X.
           03  QTP-USERID               PIC X(10).
           03  QTP-POSTTIME             PIC 9(6).
           03  QTP-PLTYPE               PIC X.
           03  QTP-PL-OTH-AR            PIC S9(7)V99.
           03  QTP-LP-DLNO              PIC 9(4)V99.
           03  QTP-REPOCD               PIC X.
           03  QTP-SPRCLASS             PIC 999.
           03  QTP-MAX-DEFER-FG         PIC X.
           03  QTP-POSTDATE             PIC X(10).
           03  QTP-UNITPER-CD           PIC X.
           03  QTP-UNITPER-FREQ         PIC 9(3).
           03  QTP-SP-DEFFRMLA          PIC 99.
           03  QTP-COLLECTOR            PIC X(3).
           03  QTP-PENALTY-INT          PIC S9(7)V99.
           03  QTP-INSCOMP              PIC 99.
           03  QTP-PROMDATE             PIC X(10).
           03  QTP-PROMAMT              PIC S9(7)V99.
           03  QTP-PROMID               PIC X(3).
           03  QTP-INS-TAX-RB           PIC S9(7)V99.
           03  QTP-PROMDAYS-PASTDUE     PIC S9(3).
           03  QTP-MIP-FG               PIC X.
           03  QTP-MIP-CLPAID           PIC S9(7)V99.
           03  QTP-MIP-CLCOMM           PIC S9(7)V99.
           03  QTP-MIP-AHPAID           PIC S9(7)V99.
           03  QTP-MIP-AHCOMM           PIC S9(7)V99.
           03  QTP-MIP-CLPDTH-DATE      PIC X(10).
           03  QTP-MIP-AHPDTH-DATE      PIC X(10).
           03  QTP-INS-TAX-COUNTY-CD    PIC X(3).
           03  QTP-INS-TAX-CITY-CD      PIC X(3).
           03  QTP-APPLIED-TO-ESCROW    PIC S9(7)V99.
           03  QTP-ESCROW-AMT           PIC S9(7)V99.
           03  QTP-ESCROW-PREPAID       PIC S9(7)V99.
           03  QTP-ESCROW-DISBURSED     PIC S9(7)V99.
           03  QTP-LP-SEQNO             PIC 9(5).
           03  QTP-PRIOR-DF-CODE        PIC X(2).
           03  QTP-PRIOR-DF-POSTDATE    PIC X(10).
           03  QTP-CANCEL-REASON        PIC X(3).
           03  QTP-FROMCL               PIC 9(3).
           03  QTP-TOCL                 PIC 9(3).
           03  QTP-ADDON-TXT       PIC X(3).
           03  QTP-ADDON-TYP       PIC X(2).
           03  QTP-DLRVNUM         PIC 9.
           03  QTP-P4P5-TRANCD     PIC XX.
           03  QTP-P4P5-DLRVNUM    PIC 9.
           03  QTP-POOLID          PIC 9(5).

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
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-LINE                PIC 9(03).
           03  END-LINE                PIC 9(03).

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
           03  FILLER PIC X(30) VALUE "BRNO      DATE   CLASS   ACCTN".
           03  FILLER PIC X(30) VALUE "O  LINE   TR   TRAMT    POOLID".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TP-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TP-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TP-CLASS                  PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TP-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-TP-LINE                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TP-TRCD                    PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-TP-TRAMT                    PIC Z(6)9.99.
           03  FILLER                         PIC X(03).
           03  D01-TP-LOANDATE               PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TP-POOLID                  PIC 9(05).

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
       COPY "LIBIO/IOTRPR_DEF.CPY".
       COPY "LIBIO/IOTRPR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRPR_SCN.CPY".

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
           MOVE "IOTRPR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRPR-QUEUE-POS  TO Q-POS.
           MOVE IOTRPR-COPIES-POS TO C-POS.
           MOVE IOTRPR-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-LINE.

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
       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN030END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE 999                 TO VDUNUM0  END-CLASS.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
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

           GO TO ENTER-BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LINE.

           MOVE "ENNN030BEG-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LINE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-END-LINE.

           MOVE "ENNN030END-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LINE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LINE.

           IF ( BEG-LINE > END-LINE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LINE.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-LINE.

           MOVE "SNNN030BEG-LINE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-LINE.
           PERFORM SCREENIO.

           MOVE "SNNN030END-LINE." TO VDU.
           MOVE 999                 TO VDUNUM0  END-LINE.
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


           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SNNN050BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-LINE."       TO VDU.
           MOVE BEG-LINE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-LINE."       TO VDU.
           MOVE END-LINE                 TO VDUNUM0.
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

           MOVE "B"                        TO TRP-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QTRP1-WBEG-BRNO.
           MOVE BR-NO     TO QTRP1-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS  TO QTRP1-WBEG-CLASS.
           MOVE END-CLASS  TO QTRP1-WEND-CLASS.

           MOVE BEG-ACCTNO TO QTRP1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QTRP1-WEND-ACCTNO.
           MOVE BEG-LINE TO QTRP1-WBEG-LINE.
           MOVE END-LINE TO QTRP1-WEND-LINE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-TRP1-FILE
           PERFORM START-TRP1-FILE.
 
      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-TRP1-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TP-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TP-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TP-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( TP-DATE < BEG-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TP-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TP-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

           IF TP-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TP-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TP-LINE < BEG-LINE OR > END-LINE
              GO TO PROCESS-BRANCH-NEXT.


      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TP-BRNO                    TO D01-TP-BRNO.
           MOVE TP-DATE                    TO D01-TP-DATE.
           MOVE TP-CLASS                   TO D01-TP-CLASS.
           MOVE TP-ACCTNO                  TO D01-TP-ACCTNO.
           MOVE TP-LINE                    TO D01-TP-LINE.
           MOVE TP-TRCD                TO D01-TP-TRCD.
           MOVE TP-TRAMT                TO D01-TP-TRAMT.
           MOVE TP-POOLID                   TO D01-TP-POOLID.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-TRP1-FILE.

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

           IF PROCESS-KEY = 1
              MOVE TRP1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TP-DATE, TP-BRNO, TP-CLASS,
                     TP-ACCTNO, TP-LINE DELIMITED BY SIZE
                     INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
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

           MOVE "BEGINNING LINE                : "    TO R01-PROMPT.
           MOVE BEG-LINE                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LINE                : "    TO R01-PROMPT.
           MOVE END-LINE                              TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - TRPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRPR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRPR-SYSDATE.
           DISPLAY IOTRPR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRPR-FORM-FIELDS TO WR-BUF.
           MOVE IOTRPR-FORM-LIST TO WR-LIST.
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

           MOVE IOTRPR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRPR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRPR_EVA.CPY".
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
      *    LOAD TRP-REC FROM QTRP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRP-FIELDS.
           MOVE QTP-BRNO TO TP-BRNO.

           IF ( QTP-DATE = "1900-01-01" )
              MOVE ZEROES TO TP-DATE
           ELSE
              MOVE QTP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-DATE.

           MOVE QTP-CLASS TO TP-CLASS.
           MOVE QTP-ACCTNO TO TP-ACCTNO.
           MOVE QTP-LINE TO TP-LINE.

           IF ( QTP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TP-LTOUCH-DATE
           ELSE
              MOVE QTP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-LTOUCH-DATE.

           MOVE QTP-LAWCODE TO TP-LAWCODE.
           MOVE QTP-JDCD TO TP-JDCD.
           MOVE QTP-PLCD TO TP-PLCD.
           MOVE QTP-SSNO TO TP-SSNO.
           MOVE QTP-MAKER TO TP-MAKER.
           MOVE QTP-IBPC TO TP-IBPC.
           MOVE QTP-TRCD TO TP-TRCD.
           MOVE QTP-REFNO TO TP-REFNO.

           IF ( QTP-PAYDATE = "1900-01-01" )
              MOVE ZEROES TO TP-PAYDATE
           ELSE
              MOVE QTP-PAYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-PAYDATE.


           IF ( QTP-DUEDATE = "1900-01-01" )
              MOVE ZEROES TO TP-DUEDATE
           ELSE
              MOVE QTP-DUEDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-DUEDATE.

           MOVE QTP-TRAMT TO TP-TRAMT.
           MOVE QTP-INTDUE TO TP-INTDUE.
           MOVE QTP-APOTH TO TP-APOTH.
           MOVE QTP-APOT2 TO TP-APOT2.
           MOVE QTP-APINT TO TP-APINT.
           MOVE QTP-APLC TO TP-APLC.
           MOVE QTP-APCUR TO TP-APCUR.
           MOVE QTP-GLNO-1 TO TP-GLNO(1).
           MOVE QTP-GLAMT-1 TO TP-GLAMT(1).
           MOVE QTP-GLNO-2 TO TP-GLNO(2).
           MOVE QTP-GLAMT-2 TO TP-GLAMT(2).
           MOVE QTP-GLNO-3 TO TP-GLNO(3).
           MOVE QTP-GLAMT-3 TO TP-GLAMT(3).
           MOVE QTP-EARNINGS-1 TO TP-EARNINGS(1).
           MOVE QTP-EARNINGS-2 TO TP-EARNINGS(2).
           MOVE QTP-EARNINGS-3 TO TP-EARNINGS(3).
           MOVE QTP-EARNINGS-4 TO TP-EARNINGS(4).
           MOVE QTP-EARNINGS-5 TO TP-EARNINGS(5).
           MOVE QTP-EARNINGS-6 TO TP-EARNINGS(6).
           MOVE QTP-EARNINGS-7 TO TP-EARNINGS(7).
           MOVE QTP-EARNINGS-8 TO TP-EARNINGS(8).
           MOVE QTP-EARNINGS-9 TO TP-EARNINGS(9).
           MOVE QTP-EARNINGS-10 TO TP-EARNINGS(10).
           MOVE QTP-WORKAMT-1 TO TP-WORKAMT(1).
           MOVE QTP-WORKAMT-2 TO TP-WORKAMT(2).
           MOVE QTP-WORKAMT-3 TO TP-WORKAMT(3).
           MOVE QTP-WORKAMT-4 TO TP-WORKAMT(4).
           MOVE QTP-WORKAMT-5 TO TP-WORKAMT(5).
           MOVE QTP-WORKAMT-6 TO TP-WORKAMT(6).
           MOVE QTP-WORKAMT-7 TO TP-WORKAMT(7).
           MOVE QTP-WORKAMT-8 TO TP-WORKAMT(8).
           MOVE QTP-WORKAMT-9 TO TP-WORKAMT(9).
           MOVE QTP-WORKAMT-10 TO TP-WORKAMT(10).
           MOVE QTP-OTHBAL TO TP-OTHBAL.
           MOVE QTP-OT2BAL TO TP-OT2BAL.
           MOVE QTP-INTBAL TO TP-INTBAL.
           MOVE QTP-LCBAL TO TP-LCBAL.
           MOVE QTP-CURBAL TO TP-CURBAL.

           IF ( QTP-PDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO TP-PDTH-DATE
           ELSE
              MOVE QTP-PDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-PDTH-DATE.

           MOVE QTP-DLNO TO TP-DLNO.
           MOVE QTP-DLPROC TO TP-DLPROC.
           MOVE QTP-SERVACCT TO TP-SERVACCT.
           MOVE QTP-SALEFIN TO TP-SALEFIN.
           MOVE QTP-INSURED TO TP-INSURED.
           MOVE QTP-INSOURS TO TP-INSOURS.
           MOVE QTP-LOANTYPE TO TP-LOANTYPE.
           MOVE QTP-TILLNO TO TP-TILLNO.

           IF ( QTP-LCPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO TP-LCPDTH-DATE
           ELSE
              MOVE QTP-LCPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-LCPDTH-DATE.

           MOVE QTP-POSTING-BR TO TP-POSTING-BR.
           MOVE QTP-OTHBR-FG TO TP-OTHBR-FG.
           MOVE QTP-REV TO TP-REV.
           MOVE QTP-REVTRCD TO TP-REVTRCD.
           MOVE QTP-REVSEQNO TO TP-REVSEQNO.
           MOVE QTP-STATUSFG TO TP-STATUSFG.
           MOVE QTP-KEEPAPINT TO TP-KEEPAPINT.
           MOVE QTP-INTPAID TO TP-INTPAID.
           MOVE QTP-PRIOR-POOLID TO TP-PRIOR-POOLID.
           MOVE QTP-INSTYPES TO TP-INSTYPES.
           MOVE QTP-INSEARN-1 TO TP-INSEARN(1).
           MOVE QTP-INSEARN-2 TO TP-INSEARN(2).
           MOVE QTP-INSEARN-3 TO TP-INSEARN(3).
           MOVE QTP-INSEARN-4 TO TP-INSEARN(4).
           MOVE QTP-INSEARN-5 TO TP-INSEARN(5).
           MOVE QTP-INSEARN-6 TO TP-INSEARN(6).
           MOVE QTP-INSEARN-7 TO TP-INSEARN(7).
           MOVE QTP-INSEARN-8 TO TP-INSEARN(8).
           MOVE QTP-INSEARN-9 TO TP-INSEARN(9).
           MOVE QTP-INSEARN-10 TO TP-INSEARN(10).
           MOVE QTP-INSWORKAMT-1 TO TP-INSWORKAMT(1).
           MOVE QTP-INSWORKAMT-2 TO TP-INSWORKAMT(2).
           MOVE QTP-INSWORKAMT-3 TO TP-INSWORKAMT(3).
           MOVE QTP-INSWORKAMT-4 TO TP-INSWORKAMT(4).
           MOVE QTP-INSWORKAMT-5 TO TP-INSWORKAMT(5).
           MOVE QTP-INSWORKAMT-6 TO TP-INSWORKAMT(6).
           MOVE QTP-INSWORKAMT-7 TO TP-INSWORKAMT(7).
           MOVE QTP-INSWORKAMT-8 TO TP-INSWORKAMT(8).
           MOVE QTP-INSWORKAMT-9 TO TP-INSWORKAMT(9).
           MOVE QTP-INSWORKAMT-10 TO TP-INSWORKAMT(10).
           MOVE QTP-DLRVCHGS-ADJ-1 TO TP-DLRVCHGS-ADJ(1).
           MOVE QTP-DLRVCHGS-ADJ-2 TO TP-DLRVCHGS-ADJ(2).
           MOVE QTP-DLRVCHGS-ADJ-3 TO TP-DLRVCHGS-ADJ(3).
           MOVE QTP-DLPARVHELD-ADJ TO TP-DLPARVHELD-ADJ.
           MOVE QTP-DLPARVPAID-ADJ TO TP-DLPARVPAID-ADJ.
           MOVE QTP-NEWSTAT-RATE TO TP-NEWSTAT-RATE.
           MOVE QTP-POCD TO TP-POCD.

           IF ( QTP-PODATE = "1900-01-01" )
              MOVE ZEROES TO TP-PODATE
           ELSE
              MOVE QTP-PODATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-PODATE.

           MOVE QTP-DL-RVCHGS-CODE-1 TO TP-DL-RVCHGS-CODE(1).
           MOVE QTP-DL-RVCHGS-CODE-2 TO TP-DL-RVCHGS-CODE(2).
           MOVE QTP-DL-RVCHGS-CODE-3 TO TP-DL-RVCHGS-CODE(3).
           MOVE QTP-DL-PARVHELD-CODE TO TP-DL-PARVHELD-CODE.
           MOVE QTP-DL-PARVPAID-CODE TO TP-DL-PARVPAID-CODE.
           MOVE QTP-TOTNODEF TO TP-TOTNODEF.
           MOVE QTP-LNAMT TO TP-LNAMT.

           IF ( QTP-ORIG-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO TP-ORIG-1STPYDATE
           ELSE
              MOVE QTP-ORIG-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-ORIG-1STPYDATE.


           IF ( QTP-OLD1ST-PYDATE = "1900-01-01" )
              MOVE ZEROES TO TP-OLD1ST-PYDATE
           ELSE
              MOVE QTP-OLD1ST-PYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-OLD1ST-PYDATE.


           IF ( QTP-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO TP-1STPYDATE
           ELSE
              MOVE QTP-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-1STPYDATE.

           MOVE QTP-ORGTERM TO TP-ORGTERM.
           MOVE QTP-INSCOMM TO TP-INSCOMM.
           MOVE QTP-MANUAL-RB TO TP-MANUAL-RB.
           MOVE QTP-USERID TO TP-USERID.
           MOVE QTP-POSTTIME TO TP-POSTTIME.
           MOVE QTP-PLTYPE TO TP-PLTYPE.
           MOVE QTP-PL-OTH-AR TO TP-PL-OTH-AR.
           MOVE QTP-LP-DLNO TO TP-LP-DLNO.
           MOVE QTP-REPOCD TO TP-REPOCD.
           MOVE QTP-SPRCLASS TO TP-SPRCLASS.
           MOVE QTP-MAX-DEFER-FG TO TP-MAX-DEFER-FG.

           IF ( QTP-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TP-POSTDATE
           ELSE
              MOVE QTP-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-POSTDATE.

           MOVE QTP-UNITPER-CD TO TP-UNITPER-CD.
           MOVE QTP-UNITPER-FREQ TO TP-UNITPER-FREQ.
           MOVE QTP-SP-DEFFRMLA TO TP-SP-DEFFRMLA.
           MOVE QTP-COLLECTOR TO TP-COLLECTOR.
           MOVE QTP-PENALTY-INT TO TP-PENALTY-INT.
           MOVE QTP-INSCOMP TO TP-INSCOMP.

           IF ( QTP-PROMDATE = "1900-01-01" )
              MOVE ZEROES TO TP-PROMDATE
           ELSE
              MOVE QTP-PROMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-PROMDATE.

           MOVE QTP-PROMAMT TO TP-PROMAMT.
           MOVE QTP-PROMID TO TP-PROMID.
           MOVE QTP-INS-TAX-RB TO TP-INS-TAX-RB.
           MOVE QTP-PROMDAYS-PASTDUE TO TP-PROMDAYS-PASTDUE.
           MOVE QTP-MIP-FG TO TP-MIP-FG.
           MOVE QTP-MIP-CLPAID TO TP-MIP-CLPAID.
           MOVE QTP-MIP-CLCOMM TO TP-MIP-CLCOMM.
           MOVE QTP-MIP-AHPAID TO TP-MIP-AHPAID.
           MOVE QTP-MIP-AHCOMM TO TP-MIP-AHCOMM.

           IF ( QTP-MIP-CLPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO TP-MIP-CLPDTH-DATE
           ELSE
              MOVE QTP-MIP-CLPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-MIP-CLPDTH-DATE.


           IF ( QTP-MIP-AHPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO TP-MIP-AHPDTH-DATE
           ELSE
              MOVE QTP-MIP-AHPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-MIP-AHPDTH-DATE.

           MOVE QTP-INS-TAX-COUNTY-CD TO TP-INS-TAX-COUNTY-CD.
           MOVE QTP-INS-TAX-CITY-CD TO TP-INS-TAX-CITY-CD.
           MOVE QTP-APPLIED-TO-ESCROW TO TP-APPLIED-TO-ESCROW.
           MOVE QTP-ESCROW-AMT TO TP-ESCROW-AMT.
           MOVE QTP-ESCROW-PREPAID TO TP-ESCROW-PREPAID.
           MOVE QTP-ESCROW-DISBURSED TO TP-ESCROW-DISBURSED.
           MOVE QTP-LP-SEQNO TO TP-LP-SEQNO.
           MOVE QTP-PRIOR-DF-CODE TO TP-PRIOR-DF-CODE.

           IF ( QTP-PRIOR-DF-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TP-PRIOR-DF-POSTDATE
           ELSE
              MOVE QTP-PRIOR-DF-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TP-PRIOR-DF-POSTDATE.

           MOVE QTP-CANCEL-REASON TO TP-CANCEL-REASON.
           MOVE QTP-FROMCL TO TP-FROMCL.
           MOVE QTP-TOCL TO TP-TOCL.
           MOVE QTP-ADDON-TXT TO TP-ADDON-TXT.
           MOVE QTP-ADDON-TYP TO TP-ADDON-TYP.
           MOVE QTP-DLRVNUM TO TP-DLRVNUM.
           MOVE QTP-P4P5-TRANCD TO TP-P4P5-TRANCD.
           MOVE QTP-P4P5-DLRVNUM TO TP-P4P5-DLRVNUM.
           MOVE QTP-POOLID       TO TP-POOLID.
 
      *-----------------------------------------------------------------
      *    LOAD QTRP-REC FROM TRP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRP-FIELDS.
           MOVE TP-BRNO TO QTP-BRNO.

           IF ( TP-DATE = ZEROES )
              MOVE "1900-01-01" TO QTP-DATE
           ELSE
              MOVE TP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-DATE.

           MOVE TP-CLASS TO QTP-CLASS.
           MOVE TP-ACCTNO TO QTP-ACCTNO.
           MOVE TP-LINE TO QTP-LINE.

           IF ( TP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTP-LTOUCH-DATE
           ELSE
              MOVE TP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-LTOUCH-DATE.

           MOVE TP-LAWCODE TO QTP-LAWCODE.
           MOVE TP-JDCD TO QTP-JDCD.
           MOVE TP-PLCD TO QTP-PLCD.
           MOVE TP-SSNO TO QTP-SSNO.
           MOVE TP-MAKER TO QTP-MAKER.
           MOVE TP-IBPC TO QTP-IBPC.
           MOVE TP-TRCD TO QTP-TRCD.
           MOVE TP-REFNO TO QTP-REFNO.

           IF ( TP-PAYDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-PAYDATE
           ELSE
              MOVE TP-PAYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-PAYDATE.


           IF ( TP-DUEDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-DUEDATE
           ELSE
              MOVE TP-DUEDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-DUEDATE.

           MOVE TP-TRAMT TO QTP-TRAMT.
           MOVE TP-INTDUE TO QTP-INTDUE.
           MOVE TP-APOTH TO QTP-APOTH.
           MOVE TP-APOT2 TO QTP-APOT2.
           MOVE TP-APINT TO QTP-APINT.
           MOVE TP-APLC TO QTP-APLC.
           MOVE TP-APCUR TO QTP-APCUR.
           MOVE TP-GLNO(1) TO QTP-GLNO-1.
           MOVE TP-GLAMT(1) TO QTP-GLAMT-1.
           MOVE TP-GLNO(2) TO QTP-GLNO-2.
           MOVE TP-GLAMT(2) TO QTP-GLAMT-2.
           MOVE TP-GLNO(3) TO QTP-GLNO-3.
           MOVE TP-GLAMT(3) TO QTP-GLAMT-3.
           MOVE TP-EARNINGS(1) TO QTP-EARNINGS-1.
           MOVE TP-EARNINGS(2) TO QTP-EARNINGS-2.
           MOVE TP-EARNINGS(3) TO QTP-EARNINGS-3.
           MOVE TP-EARNINGS(4) TO QTP-EARNINGS-4.
           MOVE TP-EARNINGS(5) TO QTP-EARNINGS-5.
           MOVE TP-EARNINGS(6) TO QTP-EARNINGS-6.
           MOVE TP-EARNINGS(7) TO QTP-EARNINGS-7.
           MOVE TP-EARNINGS(8) TO QTP-EARNINGS-8.
           MOVE TP-EARNINGS(9) TO QTP-EARNINGS-9.
           MOVE TP-EARNINGS(10) TO QTP-EARNINGS-10.
           MOVE TP-WORKAMT(1) TO QTP-WORKAMT-1.
           MOVE TP-WORKAMT(2) TO QTP-WORKAMT-2.
           MOVE TP-WORKAMT(3) TO QTP-WORKAMT-3.
           MOVE TP-WORKAMT(4) TO QTP-WORKAMT-4.
           MOVE TP-WORKAMT(5) TO QTP-WORKAMT-5.
           MOVE TP-WORKAMT(6) TO QTP-WORKAMT-6.
           MOVE TP-WORKAMT(7) TO QTP-WORKAMT-7.
           MOVE TP-WORKAMT(8) TO QTP-WORKAMT-8.
           MOVE TP-WORKAMT(9) TO QTP-WORKAMT-9.
           MOVE TP-WORKAMT(10) TO QTP-WORKAMT-10.
           MOVE TP-OTHBAL TO QTP-OTHBAL.
           MOVE TP-OT2BAL TO QTP-OT2BAL.
           MOVE TP-INTBAL TO QTP-INTBAL.
           MOVE TP-LCBAL TO QTP-LCBAL.
           MOVE TP-CURBAL TO QTP-CURBAL.

           IF ( TP-PDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTP-PDTH-DATE
           ELSE
              MOVE TP-PDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-PDTH-DATE.

           MOVE TP-DLNO TO QTP-DLNO.
           MOVE TP-DLPROC TO QTP-DLPROC.
           MOVE TP-SERVACCT TO QTP-SERVACCT.
           MOVE TP-SALEFIN TO QTP-SALEFIN.
           MOVE TP-INSURED TO QTP-INSURED.
           MOVE TP-INSOURS TO QTP-INSOURS.
           MOVE TP-LOANTYPE TO QTP-LOANTYPE.
           MOVE TP-TILLNO TO QTP-TILLNO.

           IF ( TP-LCPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTP-LCPDTH-DATE
           ELSE
              MOVE TP-LCPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-LCPDTH-DATE.

           MOVE TP-POSTING-BR TO QTP-POSTING-BR.
           MOVE TP-OTHBR-FG TO QTP-OTHBR-FG.
           MOVE TP-REV TO QTP-REV.
           MOVE TP-REVTRCD TO QTP-REVTRCD.
           MOVE TP-REVSEQNO TO QTP-REVSEQNO.
           MOVE TP-STATUSFG TO QTP-STATUSFG.
           MOVE TP-KEEPAPINT TO QTP-KEEPAPINT.
           MOVE TP-INTPAID TO QTP-INTPAID.
           MOVE TP-PRIOR-POOLID TO QTP-PRIOR-POOLID.
           MOVE TP-INSTYPES TO QTP-INSTYPES.
           MOVE TP-INSEARN(1) TO QTP-INSEARN-1.
           MOVE TP-INSEARN(2) TO QTP-INSEARN-2.
           MOVE TP-INSEARN(3) TO QTP-INSEARN-3.
           MOVE TP-INSEARN(4) TO QTP-INSEARN-4.
           MOVE TP-INSEARN(5) TO QTP-INSEARN-5.
           MOVE TP-INSEARN(6) TO QTP-INSEARN-6.
           MOVE TP-INSEARN(7) TO QTP-INSEARN-7.
           MOVE TP-INSEARN(8) TO QTP-INSEARN-8.
           MOVE TP-INSEARN(9) TO QTP-INSEARN-9.
           MOVE TP-INSEARN(10) TO QTP-INSEARN-10.
           MOVE TP-INSWORKAMT(1) TO QTP-INSWORKAMT-1.
           MOVE TP-INSWORKAMT(2) TO QTP-INSWORKAMT-2.
           MOVE TP-INSWORKAMT(3) TO QTP-INSWORKAMT-3.
           MOVE TP-INSWORKAMT(4) TO QTP-INSWORKAMT-4.
           MOVE TP-INSWORKAMT(5) TO QTP-INSWORKAMT-5.
           MOVE TP-INSWORKAMT(6) TO QTP-INSWORKAMT-6.
           MOVE TP-INSWORKAMT(7) TO QTP-INSWORKAMT-7.
           MOVE TP-INSWORKAMT(8) TO QTP-INSWORKAMT-8.
           MOVE TP-INSWORKAMT(9) TO QTP-INSWORKAMT-9.
           MOVE TP-INSWORKAMT(10) TO QTP-INSWORKAMT-10.
           MOVE TP-DLRVCHGS-ADJ(1) TO QTP-DLRVCHGS-ADJ-1.
           MOVE TP-DLRVCHGS-ADJ(2) TO QTP-DLRVCHGS-ADJ-2.
           MOVE TP-DLRVCHGS-ADJ(3) TO QTP-DLRVCHGS-ADJ-3.
           MOVE TP-DLPARVHELD-ADJ TO QTP-DLPARVHELD-ADJ.
           MOVE TP-DLPARVPAID-ADJ TO QTP-DLPARVPAID-ADJ.
           MOVE TP-NEWSTAT-RATE TO QTP-NEWSTAT-RATE.
           MOVE TP-POCD TO QTP-POCD.

           IF ( TP-PODATE = ZEROES )
              MOVE "1900-01-01" TO QTP-PODATE
           ELSE
              MOVE TP-PODATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-PODATE.

           MOVE TP-DL-RVCHGS-CODE(1) TO QTP-DL-RVCHGS-CODE-1.
           MOVE TP-DL-RVCHGS-CODE(2) TO QTP-DL-RVCHGS-CODE-2.
           MOVE TP-DL-RVCHGS-CODE(3) TO QTP-DL-RVCHGS-CODE-3.
           MOVE TP-DL-PARVHELD-CODE TO QTP-DL-PARVHELD-CODE.
           MOVE TP-DL-PARVPAID-CODE TO QTP-DL-PARVPAID-CODE.
           MOVE TP-TOTNODEF TO QTP-TOTNODEF.
           MOVE TP-LNAMT TO QTP-LNAMT.

           IF ( TP-ORIG-1STPYDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-ORIG-1STPYDATE
           ELSE
              MOVE TP-ORIG-1STPYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-ORIG-1STPYDATE.


           IF ( TP-OLD1ST-PYDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-OLD1ST-PYDATE
           ELSE
              MOVE TP-OLD1ST-PYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-OLD1ST-PYDATE.


           IF ( TP-1STPYDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-1STPYDATE
           ELSE
              MOVE TP-1STPYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-1STPYDATE.

           MOVE TP-ORGTERM TO QTP-ORGTERM.
           MOVE TP-INSCOMM TO QTP-INSCOMM.
           MOVE TP-MANUAL-RB TO QTP-MANUAL-RB.
           MOVE TP-USERID TO QTP-USERID.
           MOVE TP-POSTTIME TO QTP-POSTTIME.
           MOVE TP-PLTYPE TO QTP-PLTYPE.
           MOVE TP-PL-OTH-AR TO QTP-PL-OTH-AR.
           MOVE TP-LP-DLNO TO QTP-LP-DLNO.
           MOVE TP-REPOCD TO QTP-REPOCD.
           MOVE TP-SPRCLASS TO QTP-SPRCLASS.
           MOVE TP-MAX-DEFER-FG TO QTP-MAX-DEFER-FG.

           IF ( TP-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-POSTDATE
           ELSE
              MOVE TP-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-POSTDATE.

           MOVE TP-UNITPER-CD TO QTP-UNITPER-CD.
           MOVE TP-UNITPER-FREQ TO QTP-UNITPER-FREQ.
           MOVE TP-SP-DEFFRMLA TO QTP-SP-DEFFRMLA.
           MOVE TP-COLLECTOR TO QTP-COLLECTOR.
           MOVE TP-PENALTY-INT TO QTP-PENALTY-INT.
           MOVE TP-INSCOMP TO QTP-INSCOMP.

           IF ( TP-PROMDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-PROMDATE
           ELSE
              MOVE TP-PROMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-PROMDATE.

           MOVE TP-PROMAMT TO QTP-PROMAMT.
           MOVE TP-PROMID TO QTP-PROMID.
           MOVE TP-INS-TAX-RB TO QTP-INS-TAX-RB.
           MOVE TP-PROMDAYS-PASTDUE TO QTP-PROMDAYS-PASTDUE.
           MOVE TP-MIP-FG TO QTP-MIP-FG.
           MOVE TP-MIP-CLPAID TO QTP-MIP-CLPAID.
           MOVE TP-MIP-CLCOMM TO QTP-MIP-CLCOMM.
           MOVE TP-MIP-AHPAID TO QTP-MIP-AHPAID.
           MOVE TP-MIP-AHCOMM TO QTP-MIP-AHCOMM.

           IF ( TP-MIP-CLPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTP-MIP-CLPDTH-DATE
           ELSE
              MOVE TP-MIP-CLPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-MIP-CLPDTH-DATE.


           IF ( TP-MIP-AHPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTP-MIP-AHPDTH-DATE
           ELSE
              MOVE TP-MIP-AHPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-MIP-AHPDTH-DATE.

           MOVE TP-INS-TAX-COUNTY-CD TO QTP-INS-TAX-COUNTY-CD.
           MOVE TP-INS-TAX-CITY-CD TO QTP-INS-TAX-CITY-CD.
           MOVE TP-APPLIED-TO-ESCROW TO QTP-APPLIED-TO-ESCROW.
           MOVE TP-ESCROW-AMT TO QTP-ESCROW-AMT.
           MOVE TP-ESCROW-PREPAID TO QTP-ESCROW-PREPAID.
           MOVE TP-ESCROW-DISBURSED TO QTP-ESCROW-DISBURSED.
           MOVE TP-LP-SEQNO TO QTP-LP-SEQNO.
           MOVE TP-PRIOR-DF-CODE TO QTP-PRIOR-DF-CODE.

           IF ( TP-PRIOR-DF-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTP-PRIOR-DF-POSTDATE
           ELSE
              MOVE TP-PRIOR-DF-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTP-PRIOR-DF-POSTDATE.

           MOVE TP-CANCEL-REASON TO QTP-CANCEL-REASON.
           MOVE TP-FROMCL TO QTP-FROMCL.
           MOVE TP-TOCL TO QTP-TOCL.
           MOVE TP-ADDON-TXT TO QTP-ADDON-TXT.
           MOVE TP-ADDON-TYP TO QTP-ADDON-TYP.
           MOVE TP-DLRVNUM TO QTP-DLRVNUM.
           MOVE TP-P4P5-TRANCD TO QTP-P4P5-TRANCD.
           MOVE TP-P4P5-DLRVNUM TO QTP-P4P5-DLRVNUM.
 
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRPFILE NOT = "S" )
              MOVE TRP-PATH-BEG             TO TRP-SELECT-PATH-BEG
              MOVE TRP-PATH-FILE            TO TRP-SELECT-PATH-FILE
           ELSE
              MOVE TRP-PATH-SQL             TO TRP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRP1-FILE.
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE

              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE.


           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
            SELECT TRPFILE.TP_BRNO,
                   ISNULL(CAST(TRPFILE.TP_DATE AS VARCHAR(10)),'0'),
                   TRPFILE.TP_CLASS,
                   TRPFILE.TP_ACCTNO,
                   TRPFILE.TP_LINE,
                   ISNULL(CAST(TRPFILE.TP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_LAWCODE,
                   ISNULL(TRPFILE.TP_JDCD,''),
                   ISNULL(TRPFILE.TP_PLCD,''),
                   TRPFILE.TP_SSNO,
                   ISNULL(TRPFILE.TP_MAKER,''),
                   ISNULL(TRPFILE.TP_IBPC,''),
                   ISNULL(TRPFILE.TP_TRCD,''),
                   ISNULL(TRPFILE.TP_REFNO,''),
                   ISNULL(CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_DUEDATE AS VARCHAR(10)),'0'),
                   TRPFILE.TP_TRAMT,
                   TRPFILE.TP_INTDUE,
                   TRPFILE.TP_APOTH,
                   TRPFILE.TP_APOT2,
                   TRPFILE.TP_APINT,
                   TRPFILE.TP_APLC,
                   TRPFILE.TP_APCUR,
                   TRPFILE.TP_GLNO_1,
                   TRPFILE.TP_GLAMT_1,
                   TRPFILE.TP_GLNO_2,
                   TRPFILE.TP_GLAMT_2,
                   TRPFILE.TP_GLNO_3,
                   TRPFILE.TP_GLAMT_3,
                   TRPFILE.TP_EARNINGS_1,
                   TRPFILE.TP_EARNINGS_2,
                   TRPFILE.TP_EARNINGS_3,
                   TRPFILE.TP_EARNINGS_4,
                   TRPFILE.TP_EARNINGS_5,
                   TRPFILE.TP_EARNINGS_6,
                   TRPFILE.TP_EARNINGS_7,
                   TRPFILE.TP_EARNINGS_8,
                   TRPFILE.TP_EARNINGS_9,
                   TRPFILE.TP_EARNINGS_10,
                   TRPFILE.TP_WORKAMT_1,
                   TRPFILE.TP_WORKAMT_2,
                   TRPFILE.TP_WORKAMT_3,
                   TRPFILE.TP_WORKAMT_4,
                   TRPFILE.TP_WORKAMT_5,
                   TRPFILE.TP_WORKAMT_6,
                   TRPFILE.TP_WORKAMT_7,
                   TRPFILE.TP_WORKAMT_8,
                   TRPFILE.TP_WORKAMT_9,
                   TRPFILE.TP_WORKAMT_10,
                   TRPFILE.TP_OTHBAL,
                   TRPFILE.TP_OT2BAL,
                   TRPFILE.TP_INTBAL,
                   TRPFILE.TP_LCBAL,
                   TRPFILE.TP_CURBAL,
                   ISNULL(CAST(TRPFILE.TP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_DLNO,
                   TRPFILE.TP_DLPROC,
                   ISNULL(TRPFILE.TP_SERVACCT,''),
                   ISNULL(TRPFILE.TP_SALEFIN,''),
                   ISNULL(TRPFILE.TP_INSURED,''),
                   ISNULL(TRPFILE.TP_INSOURS,''),
                   ISNULL(TRPFILE.TP_LOANTYPE,''),
                   TRPFILE.TP_TILLNO,
                   ISNULL(CAST(TRPFILE.TP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_POSTING_BR,
                   ISNULL(TRPFILE.TP_OTHBR_FG,''),
                   ISNULL(TRPFILE.TP_REV,''),
                   ISNULL(TRPFILE.TP_REVTRCD,''),
                   TRPFILE.TP_REVSEQNO,
                   ISNULL(TRPFILE.TP_STATUSFG,''),
                   ISNULL(TRPFILE.TP_KEEPAPINT,''),
                   TRPFILE.TP_INTPAID,
                   TRPFILE.TP_PRIOR_POOLID,
                   ISNULL(TRPFILE.TP_INSTYPES,''),
                   TRPFILE.TP_INSEARN_1,
                   TRPFILE.TP_INSEARN_2,
                   TRPFILE.TP_INSEARN_3,
                   TRPFILE.TP_INSEARN_4,
                   TRPFILE.TP_INSEARN_5,
                   TRPFILE.TP_INSEARN_6,
                   TRPFILE.TP_INSEARN_7,
                   TRPFILE.TP_INSEARN_8,
                   TRPFILE.TP_INSEARN_9,
                   TRPFILE.TP_INSEARN_10,
                   TRPFILE.TP_INSWORKAMT_1,
                   TRPFILE.TP_INSWORKAMT_2,
                   TRPFILE.TP_INSWORKAMT_3,
                   TRPFILE.TP_INSWORKAMT_4,
                   TRPFILE.TP_INSWORKAMT_5,
                   TRPFILE.TP_INSWORKAMT_6,
                   TRPFILE.TP_INSWORKAMT_7,
                   TRPFILE.TP_INSWORKAMT_8,
                   TRPFILE.TP_INSWORKAMT_9,
                   TRPFILE.TP_INSWORKAMT_10,
                   TRPFILE.TP_DLRVCHGS_ADJ_1,
                   TRPFILE.TP_DLRVCHGS_ADJ_2,
                   TRPFILE.TP_DLRVCHGS_ADJ_3,
                   TRPFILE.TP_DLPARVHELD_ADJ,
                   TRPFILE.TP_DLPARVPAID_ADJ,
                   TRPFILE.TP_NEWSTAT_RATE,
                   ISNULL(TRPFILE.TP_POCD,''),
                   ISNULL(CAST(TRPFILE.TP_PODATE AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_1,''),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_2,''),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_3,''),
                   ISNULL(TRPFILE.TP_DL_PARVHELD_CODE,''),
                   ISNULL(TRPFILE.TP_DL_PARVPAID_CODE,''),
                   TRPFILE.TP_TOTNODEF,
                   TRPFILE.TP_LNAMT,
                   ISNULL(CAST(TRPFILE.TP_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_ORGTERM,
                   TRPFILE.TP_INSCOMM,
                   ISNULL(TRPFILE.TP_MANUAL_RB,''),
                   ISNULL(TRPFILE.TP_USERID,''),
                   TRPFILE.TP_POSTTIME,
                   ISNULL(TRPFILE.TP_PLTYPE,''),
                   TRPFILE.TP_PL_OTH_AR,
                   TRPFILE.TP_LP_DLNO,
                   ISNULL(TRPFILE.TP_REPOCD,''),
                   TRPFILE.TP_SPRCLASS,
                   ISNULL(TRPFILE.TP_MAX_DEFER_FG,''),
                   ISNULL(CAST(TRPFILE.TP_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_UNITPER_CD,''),
                   TRPFILE.TP_UNITPER_FREQ,
                   TRPFILE.TP_SP_DEFFRMLA,
                   ISNULL(TRPFILE.TP_COLLECTOR,''),
                   TRPFILE.TP_PENALTY_INT,
                   TRPFILE.TP_INSCOMP,
                   ISNULL(CAST(TRPFILE.TP_PROMDATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_PROMAMT,
                   ISNULL(TRPFILE.TP_PROMID,''),
                   TRPFILE.TP_INS_TAX_RB,
                   TRPFILE.TP_PROMDAYS_PASTDUE,
                   ISNULL(TRPFILE.TP_MIP_FG,''),
                   TRPFILE.TP_MIP_CLPAID,
                   TRPFILE.TP_MIP_CLCOMM,
                   TRPFILE.TP_MIP_AHPAID,
                   TRPFILE.TP_MIP_AHCOMM,
                   ISNULL(CAST(TRPFILE.TP_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRPFILE.TP_INS_TAX_CITY_CD,''),
                   TRPFILE.TP_APPLIED_TO_ESCROW,
                   TRPFILE.TP_ESCROW_AMT,
                   TRPFILE.TP_ESCROW_PREPAID,
                   TRPFILE.TP_ESCROW_DISBURSED,
                   TRPFILE.TP_LP_SEQNO,
                   ISNULL(TRPFILE.TP_PRIOR_DF_CODE,''),
                   ISNULL(CAST(TRPFILE.TP_PRIOR_DF_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_CANCEL_REASON,''),
                   TRPFILE.TP_FROMCL,
                   TRPFILE.TP_TOCL,
                   ISNULL(TRPFILE.TP_ADDON_TXT,''),
                   ISNULL(TRPFILE.TP_ADDON_TYP,''),
                   TRPFILE.TP_DLRVNUM,
                   ISNULL(TRPFILE.TP_P4P5_TRANCD,''),
                   TRPFILE.TP_P4P5_DLRVNUM,
                   TRPFILE.TP_POOLID
            FROM DBO.TRPFILE
            WHERE TRPFILE.TP_BRNO >= :QTRP1-WBEG-BRNO
              AND TRPFILE.TP_BRNO <= :QTRP1-WEND-BRNO
              AND CAST(TRPFILE.TP_DATE AS VARCHAR(10))
                          >= :QTRP1-WBEG-DATE
              AND CAST(TRPFILE.TP_DATE AS VARCHAR(10))
                          <= :QTRP1-WEND-DATE
              AND TRPFILE.TP_CLASS >= :QTRP1-WBEG-CLASS
              AND TRPFILE.TP_CLASS <= :QTRP1-WEND-CLASS
              AND TRPFILE.TP_ACCTNO >= :QTRP1-WBEG-ACCTNO
              AND TRPFILE.TP_ACCTNO <= :QTRP1-WEND-ACCTNO
              AND TRPFILE.TP_LINE >= :QTRP1-WBEG-LINE
              AND TRPFILE.TP_LINE <= :QTRP1-WEND-LINE
            ORDER BY TRPFILE.TP_BRNO,
                     TRPFILE.TP_DATE,
                     TRPFILE.TP_CLASS,
                     TRPFILE.TP_ACCTNO,
                     TRPFILE.TP_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRP1-FILE.
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO. 
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           MOVE TP-BRNO             TO QTP-BRNO.
           MOVE TP-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTP-DATE.
           MOVE TP-CLASS            TO QTP-CLASS.
           MOVE TP-ACCTNO           TO QTP-ACCTNO.
           MOVE TP-LINE             TO QTP-LINE.

           EXEC SQL
            SELECT TRPFILE.TP_BRNO,
                   ISNULL(CAST(TRPFILE.TP_DATE AS VARCHAR(10)),'0'),
                   TRPFILE.TP_CLASS,
                   TRPFILE.TP_ACCTNO,
                   TRPFILE.TP_LINE,
                   ISNULL(CAST(TRPFILE.TP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_LAWCODE,
                   ISNULL(TRPFILE.TP_JDCD,''),
                   ISNULL(TRPFILE.TP_PLCD,''),
                   TRPFILE.TP_SSNO,
                   ISNULL(TRPFILE.TP_MAKER,''),
                   ISNULL(TRPFILE.TP_IBPC,''),
                   ISNULL(TRPFILE.TP_TRCD,''),
                   ISNULL(TRPFILE.TP_REFNO,''),
                   ISNULL(CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_DUEDATE AS VARCHAR(10)),'0'),
                   TRPFILE.TP_TRAMT,
                   TRPFILE.TP_INTDUE,
                   TRPFILE.TP_APOTH,
                   TRPFILE.TP_APOT2,
                   TRPFILE.TP_APINT,
                   TRPFILE.TP_APLC,
                   TRPFILE.TP_APCUR,
                   TRPFILE.TP_GLNO_1,
                   TRPFILE.TP_GLAMT_1,
                   TRPFILE.TP_GLNO_2,
                   TRPFILE.TP_GLAMT_2,
                   TRPFILE.TP_GLNO_3,
                   TRPFILE.TP_GLAMT_3,
                   TRPFILE.TP_EARNINGS_1,
                   TRPFILE.TP_EARNINGS_2,
                   TRPFILE.TP_EARNINGS_3,
                   TRPFILE.TP_EARNINGS_4,
                   TRPFILE.TP_EARNINGS_5,
                   TRPFILE.TP_EARNINGS_6,
                   TRPFILE.TP_EARNINGS_7,
                   TRPFILE.TP_EARNINGS_8,
                   TRPFILE.TP_EARNINGS_9,
                   TRPFILE.TP_EARNINGS_10,
                   TRPFILE.TP_WORKAMT_1,
                   TRPFILE.TP_WORKAMT_2,
                   TRPFILE.TP_WORKAMT_3,
                   TRPFILE.TP_WORKAMT_4,
                   TRPFILE.TP_WORKAMT_5,
                   TRPFILE.TP_WORKAMT_6,
                   TRPFILE.TP_WORKAMT_7,
                   TRPFILE.TP_WORKAMT_8,
                   TRPFILE.TP_WORKAMT_9,
                   TRPFILE.TP_WORKAMT_10,
                   TRPFILE.TP_OTHBAL,
                   TRPFILE.TP_OT2BAL,
                   TRPFILE.TP_INTBAL,
                   TRPFILE.TP_LCBAL,
                   TRPFILE.TP_CURBAL,
                   ISNULL(CAST(TRPFILE.TP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_DLNO,
                   TRPFILE.TP_DLPROC,
                   ISNULL(TRPFILE.TP_SERVACCT,''),
                   ISNULL(TRPFILE.TP_SALEFIN,''),
                   ISNULL(TRPFILE.TP_INSURED,''),
                   ISNULL(TRPFILE.TP_INSOURS,''),
                   ISNULL(TRPFILE.TP_LOANTYPE,''),
                   TRPFILE.TP_TILLNO,
                   ISNULL(CAST(TRPFILE.TP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_POSTING_BR,
                   ISNULL(TRPFILE.TP_OTHBR_FG,''),
                   ISNULL(TRPFILE.TP_REV,''),
                   ISNULL(TRPFILE.TP_REVTRCD,''),
                   TRPFILE.TP_REVSEQNO,
                   ISNULL(TRPFILE.TP_STATUSFG,''),
                   ISNULL(TRPFILE.TP_KEEPAPINT,''),
                   TRPFILE.TP_INTPAID,
                   TRPFILE.TP_PRIOR_POOLID,
                   ISNULL(TRPFILE.TP_INSTYPES,''),
                   TRPFILE.TP_INSEARN_1,
                   TRPFILE.TP_INSEARN_2,
                   TRPFILE.TP_INSEARN_3,
                   TRPFILE.TP_INSEARN_4,
                   TRPFILE.TP_INSEARN_5,
                   TRPFILE.TP_INSEARN_6,
                   TRPFILE.TP_INSEARN_7,
                   TRPFILE.TP_INSEARN_8,
                   TRPFILE.TP_INSEARN_9,
                   TRPFILE.TP_INSEARN_10,
                   TRPFILE.TP_INSWORKAMT_1,
                   TRPFILE.TP_INSWORKAMT_2,
                   TRPFILE.TP_INSWORKAMT_3,
                   TRPFILE.TP_INSWORKAMT_4,
                   TRPFILE.TP_INSWORKAMT_5,
                   TRPFILE.TP_INSWORKAMT_6,
                   TRPFILE.TP_INSWORKAMT_7,
                   TRPFILE.TP_INSWORKAMT_8,
                   TRPFILE.TP_INSWORKAMT_9,
                   TRPFILE.TP_INSWORKAMT_10,
                   TRPFILE.TP_DLRVCHGS_ADJ_1,
                   TRPFILE.TP_DLRVCHGS_ADJ_2,
                   TRPFILE.TP_DLRVCHGS_ADJ_3,
                   TRPFILE.TP_DLPARVHELD_ADJ,
                   TRPFILE.TP_DLPARVPAID_ADJ,
                   TRPFILE.TP_NEWSTAT_RATE,
                   ISNULL(TRPFILE.TP_POCD,''),
                   ISNULL(CAST(TRPFILE.TP_PODATE AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_1,''),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_2,''),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_3,''),
                   ISNULL(TRPFILE.TP_DL_PARVHELD_CODE,''),
                   ISNULL(TRPFILE.TP_DL_PARVPAID_CODE,''),
                   TRPFILE.TP_TOTNODEF,
                   TRPFILE.TP_LNAMT,
                   ISNULL(CAST(TRPFILE.TP_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_ORGTERM,
                   TRPFILE.TP_INSCOMM,
                   ISNULL(TRPFILE.TP_MANUAL_RB,''),
                   ISNULL(TRPFILE.TP_USERID,''),
                   TRPFILE.TP_POSTTIME,
                   ISNULL(TRPFILE.TP_PLTYPE,''),
                   TRPFILE.TP_PL_OTH_AR,
                   TRPFILE.TP_LP_DLNO,
                   ISNULL(TRPFILE.TP_REPOCD,''),
                   TRPFILE.TP_SPRCLASS,
                   ISNULL(TRPFILE.TP_MAX_DEFER_FG,''),
                   ISNULL(CAST(TRPFILE.TP_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_UNITPER_CD,''),
                   TRPFILE.TP_UNITPER_FREQ,
                   TRPFILE.TP_SP_DEFFRMLA,
                   ISNULL(TRPFILE.TP_COLLECTOR,''),
                   TRPFILE.TP_PENALTY_INT,
                   TRPFILE.TP_INSCOMP,
                   ISNULL(CAST(TRPFILE.TP_PROMDATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_PROMAMT,
                   ISNULL(TRPFILE.TP_PROMID,''),
                   TRPFILE.TP_INS_TAX_RB,
                   TRPFILE.TP_PROMDAYS_PASTDUE,
                   ISNULL(TRPFILE.TP_MIP_FG,''),
                   TRPFILE.TP_MIP_CLPAID,
                   TRPFILE.TP_MIP_CLCOMM,
                   TRPFILE.TP_MIP_AHPAID,
                   TRPFILE.TP_MIP_AHCOMM,
                   ISNULL(CAST(TRPFILE.TP_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRPFILE.TP_INS_TAX_CITY_CD,''),
                   TRPFILE.TP_APPLIED_TO_ESCROW,
                   TRPFILE.TP_ESCROW_AMT,
                   TRPFILE.TP_ESCROW_PREPAID,
                   TRPFILE.TP_ESCROW_DISBURSED,
                   TRPFILE.TP_LP_SEQNO,
                   ISNULL(TRPFILE.TP_PRIOR_DF_CODE,''),
                   ISNULL(CAST(TRPFILE.TP_PRIOR_DF_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_CANCEL_REASON,''),
                   TRPFILE.TP_FROMCL,
                   TRPFILE.TP_TOCL,
                   ISNULL(TRPFILE.TP_ADDON_TXT,''),
                   ISNULL(TRPFILE.TP_ADDON_TYP,''),
                   TRPFILE.TP_DLRVNUM,
                   ISNULL(TRPFILE.TP_P4P5_TRANCD,''),
                   TRPFILE.TP_P4P5_DLRVNUM,
                   TRPFILE.TP_POOLID
            INTO :QTRP-REC
            FROM DBO.TRPFILE
            WHERE TRPFILE.TP_BRNO = :QTP-BRNO
              AND TRPFILE.TP_DATE = :QTP-DATE
              AND TRPFILE.TP_CLASS = :QTP-CLASS
              AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
              AND TRPFILE.TP_LINE = :QTP-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRP1_CURSOR INTO :QTRP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.


      *-----------------------------------------------------------------
       WRITE-TRP1-FILE.

           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           ACCEPT TP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           PERFORM SET-TRP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRPFILE
             (TRPFILE.TP_BRNO,
              TRPFILE.TP_DATE,
              TRPFILE.TP_CLASS,
              TRPFILE.TP_ACCTNO,
              TRPFILE.TP_LINE,
              TRPFILE.TP_LTOUCH_DATE,
              TRPFILE.TP_LAWCODE,
              TRPFILE.TP_JDCD,
              TRPFILE.TP_PLCD,
              TRPFILE.TP_SSNO,
              TRPFILE.TP_MAKER,
              TRPFILE.TP_IBPC,
              TRPFILE.TP_TRCD,
              TRPFILE.TP_REFNO,
              TRPFILE.TP_PAYDATE,
              TRPFILE.TP_DUEDATE,
              TRPFILE.TP_TRAMT,
              TRPFILE.TP_INTDUE,
              TRPFILE.TP_APOTH,
              TRPFILE.TP_APOT2,
              TRPFILE.TP_APINT,
              TRPFILE.TP_APLC,
              TRPFILE.TP_APCUR,
              TRPFILE.TP_GLNO_1,
              TRPFILE.TP_GLAMT_1,
              TRPFILE.TP_GLNO_2,
              TRPFILE.TP_GLAMT_2,
              TRPFILE.TP_GLNO_3,
              TRPFILE.TP_GLAMT_3,
              TRPFILE.TP_EARNINGS_1,
              TRPFILE.TP_EARNINGS_2,
              TRPFILE.TP_EARNINGS_3,
              TRPFILE.TP_EARNINGS_4,
              TRPFILE.TP_EARNINGS_5,
              TRPFILE.TP_EARNINGS_6,
              TRPFILE.TP_EARNINGS_7,
              TRPFILE.TP_EARNINGS_8,
              TRPFILE.TP_EARNINGS_9,
              TRPFILE.TP_EARNINGS_10,
              TRPFILE.TP_WORKAMT_1,
              TRPFILE.TP_WORKAMT_2,
              TRPFILE.TP_WORKAMT_3,
              TRPFILE.TP_WORKAMT_4,
              TRPFILE.TP_WORKAMT_5,
              TRPFILE.TP_WORKAMT_6,
              TRPFILE.TP_WORKAMT_7,
              TRPFILE.TP_WORKAMT_8,
              TRPFILE.TP_WORKAMT_9,
              TRPFILE.TP_WORKAMT_10,
              TRPFILE.TP_OTHBAL,
              TRPFILE.TP_OT2BAL,
              TRPFILE.TP_INTBAL,
              TRPFILE.TP_LCBAL,
              TRPFILE.TP_CURBAL,
              TRPFILE.TP_PDTH_DATE,
              TRPFILE.TP_DLNO,
              TRPFILE.TP_DLPROC,
              TRPFILE.TP_SERVACCT,
              TRPFILE.TP_SALEFIN,
              TRPFILE.TP_INSURED,
              TRPFILE.TP_INSOURS,
              TRPFILE.TP_LOANTYPE,
              TRPFILE.TP_TILLNO,
              TRPFILE.TP_LCPDTH_DATE,
              TRPFILE.TP_POSTING_BR,
              TRPFILE.TP_OTHBR_FG,
              TRPFILE.TP_REV,
              TRPFILE.TP_REVTRCD,
              TRPFILE.TP_REVSEQNO,
              TRPFILE.TP_STATUSFG,
              TRPFILE.TP_KEEPAPINT,
              TRPFILE.TP_INTPAID,
              TRPFILE.TP_PRIOR_POOLID,
              TRPFILE.TP_INSTYPES,
              TRPFILE.TP_INSEARN_1,
              TRPFILE.TP_INSEARN_2,
              TRPFILE.TP_INSEARN_3,
              TRPFILE.TP_INSEARN_4,
              TRPFILE.TP_INSEARN_5,
              TRPFILE.TP_INSEARN_6,
              TRPFILE.TP_INSEARN_7,
              TRPFILE.TP_INSEARN_8,
              TRPFILE.TP_INSEARN_9,
              TRPFILE.TP_INSEARN_10,
              TRPFILE.TP_INSWORKAMT_1,
              TRPFILE.TP_INSWORKAMT_2,
              TRPFILE.TP_INSWORKAMT_3,
              TRPFILE.TP_INSWORKAMT_4,
              TRPFILE.TP_INSWORKAMT_5,
              TRPFILE.TP_INSWORKAMT_6,
              TRPFILE.TP_INSWORKAMT_7,
              TRPFILE.TP_INSWORKAMT_8,
              TRPFILE.TP_INSWORKAMT_9,
              TRPFILE.TP_INSWORKAMT_10,
              TRPFILE.TP_DLRVCHGS_ADJ_1,
              TRPFILE.TP_DLRVCHGS_ADJ_2,
              TRPFILE.TP_DLRVCHGS_ADJ_3,
              TRPFILE.TP_DLPARVHELD_ADJ,
              TRPFILE.TP_DLPARVPAID_ADJ,
              TRPFILE.TP_NEWSTAT_RATE,
              TRPFILE.TP_POCD,
              TRPFILE.TP_PODATE,
              TRPFILE.TP_DL_RVCHGS_CODE_1,
              TRPFILE.TP_DL_RVCHGS_CODE_2,
              TRPFILE.TP_DL_RVCHGS_CODE_3,
              TRPFILE.TP_DL_PARVHELD_CODE,
              TRPFILE.TP_DL_PARVPAID_CODE,
              TRPFILE.TP_TOTNODEF,
              TRPFILE.TP_LNAMT,
              TRPFILE.TP_ORIG_1STPYDATE,
              TRPFILE.TP_OLD1ST_PYDATE,
              TRPFILE.TP_1STPYDATE,
              TRPFILE.TP_ORGTERM,
              TRPFILE.TP_INSCOMM,
              TRPFILE.TP_MANUAL_RB,
              TRPFILE.TP_USERID,
              TRPFILE.TP_POSTTIME,
              TRPFILE.TP_PLTYPE,
              TRPFILE.TP_PL_OTH_AR,
              TRPFILE.TP_LP_DLNO,
              TRPFILE.TP_REPOCD,
              TRPFILE.TP_SPRCLASS,
              TRPFILE.TP_MAX_DEFER_FG,
              TRPFILE.TP_POSTDATE,
              TRPFILE.TP_UNITPER_CD,
              TRPFILE.TP_UNITPER_FREQ,
              TRPFILE.TP_SP_DEFFRMLA,
              TRPFILE.TP_COLLECTOR,
              TRPFILE.TP_PENALTY_INT,
              TRPFILE.TP_INSCOMP,
              TRPFILE.TP_PROMDATE,
              TRPFILE.TP_PROMAMT,
              TRPFILE.TP_PROMID,
              TRPFILE.TP_INS_TAX_RB,
              TRPFILE.TP_PROMDAYS_PASTDUE,
              TRPFILE.TP_MIP_FG,
              TRPFILE.TP_MIP_CLPAID,
              TRPFILE.TP_MIP_CLCOMM,
              TRPFILE.TP_MIP_AHPAID,
              TRPFILE.TP_MIP_AHCOMM,
              TRPFILE.TP_MIP_CLPDTH_DATE,
              TRPFILE.TP_MIP_AHPDTH_DATE,
              TRPFILE.TP_INS_TAX_COUNTY_CD,
              TRPFILE.TP_INS_TAX_CITY_CD,
              TRPFILE.TP_APPLIED_TO_ESCROW,
              TRPFILE.TP_ESCROW_AMT,
              TRPFILE.TP_ESCROW_PREPAID,
              TRPFILE.TP_ESCROW_DISBURSED,
              TRPFILE.TP_LP_SEQNO,
              TRPFILE.TP_PRIOR_DF_CODE,
              TRPFILE.TP_PRIOR_DF_POSTDATE,
              TRPFILE.TP_CANCEL_REASON,
              TRPFILE.TP_FROMCL,
              TRPFILE.TP_TOCL,
              TRPFILE.TP_ADDON_TXT,
              TRPFILE.TP_ADDON_TYP,
              TRPFILE.TP_DLRVNUM,
              TRPFILE.TP_P4P5_TRANCD,
              TRPFILE.TP_P4P5_DLRVNUM,
              TRPFILE.TP_POOLID)
            VALUES
             (:QTP-BRNO,
              NULLIF(:QTP-DATE,'1900-01-01'),
              :QTP-CLASS,
              :QTP-ACCTNO,
              :QTP-LINE,
              NULLIF(:QTP-LTOUCH-DATE,'1900-01-01'),
              :QTP-LAWCODE,
              :QTP-JDCD,
              :QTP-PLCD,
              :QTP-SSNO,
              :QTP-MAKER,
              :QTP-IBPC,
              :QTP-TRCD,
              :QTP-REFNO,
              NULLIF(:QTP-PAYDATE,'1900-01-01'),
              NULLIF(:QTP-DUEDATE,'1900-01-01'),
              :QTP-TRAMT,
              :QTP-INTDUE,
              :QTP-APOTH,
              :QTP-APOT2,
              :QTP-APINT,
              :QTP-APLC,
              :QTP-APCUR,
              :QTP-GLNO-1,
              :QTP-GLAMT-1,
              :QTP-GLNO-2,
              :QTP-GLAMT-2,
              :QTP-GLNO-3,
              :QTP-GLAMT-3,
              :QTP-EARNINGS-1,
              :QTP-EARNINGS-2,
              :QTP-EARNINGS-3,
              :QTP-EARNINGS-4,
              :QTP-EARNINGS-5,
              :QTP-EARNINGS-6,
              :QTP-EARNINGS-7,
              :QTP-EARNINGS-8,
              :QTP-EARNINGS-9,
              :QTP-EARNINGS-10,
              :QTP-WORKAMT-1,
              :QTP-WORKAMT-2,
              :QTP-WORKAMT-3,
              :QTP-WORKAMT-4,
              :QTP-WORKAMT-5,
              :QTP-WORKAMT-6,
              :QTP-WORKAMT-7,
              :QTP-WORKAMT-8,
              :QTP-WORKAMT-9,
              :QTP-WORKAMT-10,
              :QTP-OTHBAL,
              :QTP-OT2BAL,
              :QTP-INTBAL,
              :QTP-LCBAL,
              :QTP-CURBAL,
              NULLIF(:QTP-PDTH-DATE,'1900-01-01'),
              :QTP-DLNO,
              :QTP-DLPROC,
              :QTP-SERVACCT,
              :QTP-SALEFIN,
              :QTP-INSURED,
              :QTP-INSOURS,
              :QTP-LOANTYPE,
              :QTP-TILLNO,
              NULLIF(:QTP-LCPDTH-DATE,'1900-01-01'),
              :QTP-POSTING-BR,
              :QTP-OTHBR-FG,
              :QTP-REV,
              :QTP-REVTRCD,
              :QTP-REVSEQNO,
              :QTP-STATUSFG,
              :QTP-KEEPAPINT,
              :QTP-INTPAID,
              :QTP-PRIOR-POOLID,
              :QTP-INSTYPES,
              :QTP-INSEARN-1,
              :QTP-INSEARN-2,
              :QTP-INSEARN-3,
              :QTP-INSEARN-4,
              :QTP-INSEARN-5,
              :QTP-INSEARN-6,
              :QTP-INSEARN-7,
              :QTP-INSEARN-8,
              :QTP-INSEARN-9,
              :QTP-INSEARN-10,
              :QTP-INSWORKAMT-1,
              :QTP-INSWORKAMT-2,
              :QTP-INSWORKAMT-3,
              :QTP-INSWORKAMT-4,
              :QTP-INSWORKAMT-5,
              :QTP-INSWORKAMT-6,
              :QTP-INSWORKAMT-7,
              :QTP-INSWORKAMT-8,
              :QTP-INSWORKAMT-9,
              :QTP-INSWORKAMT-10,
              :QTP-DLRVCHGS-ADJ-1,
              :QTP-DLRVCHGS-ADJ-2,
              :QTP-DLRVCHGS-ADJ-3,
              :QTP-DLPARVHELD-ADJ,
              :QTP-DLPARVPAID-ADJ,
              :QTP-NEWSTAT-RATE,
              :QTP-POCD,
              NULLIF(:QTP-PODATE,'1900-01-01'),
              :QTP-DL-RVCHGS-CODE-1,
              :QTP-DL-RVCHGS-CODE-2,
              :QTP-DL-RVCHGS-CODE-3,
              :QTP-DL-PARVHELD-CODE,
              :QTP-DL-PARVPAID-CODE,
              :QTP-TOTNODEF,
              :QTP-LNAMT,
              NULLIF(:QTP-ORIG-1STPYDATE,'1900-01-01'),
              NULLIF(:QTP-OLD1ST-PYDATE,'1900-01-01'),
              NULLIF(:QTP-1STPYDATE,'1900-01-01'),
              :QTP-ORGTERM,
              :QTP-INSCOMM,
              :QTP-MANUAL-RB,
              :QTP-USERID,
              :QTP-POSTTIME,
              :QTP-PLTYPE,
              :QTP-PL-OTH-AR,
              :QTP-LP-DLNO,
              :QTP-REPOCD,
              :QTP-SPRCLASS,
              :QTP-MAX-DEFER-FG,
              NULLIF(:QTP-POSTDATE,'1900-01-01'),
              :QTP-UNITPER-CD,
              :QTP-UNITPER-FREQ,
              :QTP-SP-DEFFRMLA,
              :QTP-COLLECTOR,
              :QTP-PENALTY-INT,
              :QTP-INSCOMP,
              NULLIF(:QTP-PROMDATE,'1900-01-01'),
              :QTP-PROMAMT,
              :QTP-PROMID,
              :QTP-INS-TAX-RB,
              :QTP-PROMDAYS-PASTDUE,
              :QTP-MIP-FG,
              :QTP-MIP-CLPAID,
              :QTP-MIP-CLCOMM,
              :QTP-MIP-AHPAID,
              :QTP-MIP-AHCOMM,
              NULLIF(:QTP-MIP-CLPDTH-DATE,'1900-01-01'),
              NULLIF(:QTP-MIP-AHPDTH-DATE,'1900-01-01'),
              :QTP-INS-TAX-COUNTY-CD,
              :QTP-INS-TAX-CITY-CD,
              :QTP-APPLIED-TO-ESCROW,
              :QTP-ESCROW-AMT,
              :QTP-ESCROW-PREPAID,
              :QTP-ESCROW-DISBURSED,
              :QTP-LP-SEQNO,
              :QTP-PRIOR-DF-CODE,
              NULLIF(:QTP-PRIOR-DF-POSTDATE,'1900-01-01'),
              :QTP-CANCEL-REASON,
              :QTP-FROMCL,
              :QTP-TOCL,
              :QTP-ADDON-TXT,
              :QTP-ADDON-TYP,
              :QTP-DLRVNUM,
              :QTP-P4P5-TRANCD,
              :QTP-P4P5-DLRVNUM,
              :QTP-POOLID)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRP1-FILE.

           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           ACCEPT TP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           PERFORM SET-TRP-FIELDS.

           EXEC SQL
            UPDATE DBO.TRPFILE
             SET
              TRPFILE.TP_BRNO = :QTP-BRNO,
              TRPFILE.TP_DATE =
                 NULLIF(:QTP-DATE,'1900-01-01'),
              TRPFILE.TP_CLASS = :QTP-CLASS,
              TRPFILE.TP_ACCTNO = :QTP-ACCTNO,
              TRPFILE.TP_LINE = :QTP-LINE,
              TRPFILE.TP_LTOUCH_DATE =
                 NULLIF(:QTP-LTOUCH-DATE,'1900-01-01'),
              TRPFILE.TP_LAWCODE = :QTP-LAWCODE,
              TRPFILE.TP_JDCD = :QTP-JDCD,
              TRPFILE.TP_PLCD = :QTP-PLCD,
              TRPFILE.TP_SSNO = :QTP-SSNO,
              TRPFILE.TP_MAKER = :QTP-MAKER,
              TRPFILE.TP_IBPC = :QTP-IBPC,
              TRPFILE.TP_TRCD = :QTP-TRCD,
              TRPFILE.TP_REFNO = :QTP-REFNO,
              TRPFILE.TP_PAYDATE =
                 NULLIF(:QTP-PAYDATE,'1900-01-01'),
              TRPFILE.TP_DUEDATE =
                 NULLIF(:QTP-DUEDATE,'1900-01-01'),
              TRPFILE.TP_TRAMT = :QTP-TRAMT,
              TRPFILE.TP_INTDUE = :QTP-INTDUE,
              TRPFILE.TP_APOTH = :QTP-APOTH,
              TRPFILE.TP_APOT2 = :QTP-APOT2,
              TRPFILE.TP_APINT = :QTP-APINT,
              TRPFILE.TP_APLC = :QTP-APLC,
              TRPFILE.TP_APCUR = :QTP-APCUR,
              TRPFILE.TP_GLNO_1 = :QTP-GLNO-1,
              TRPFILE.TP_GLAMT_1 = :QTP-GLAMT-1,
              TRPFILE.TP_GLNO_2 = :QTP-GLNO-2,
              TRPFILE.TP_GLAMT_2 = :QTP-GLAMT-2,
              TRPFILE.TP_GLNO_3 = :QTP-GLNO-3,
              TRPFILE.TP_GLAMT_3 = :QTP-GLAMT-3,
              TRPFILE.TP_EARNINGS_1 = :QTP-EARNINGS-1,
              TRPFILE.TP_EARNINGS_2 = :QTP-EARNINGS-2,
              TRPFILE.TP_EARNINGS_3 = :QTP-EARNINGS-3,
              TRPFILE.TP_EARNINGS_4 = :QTP-EARNINGS-4,
              TRPFILE.TP_EARNINGS_5 = :QTP-EARNINGS-5,
              TRPFILE.TP_EARNINGS_6 = :QTP-EARNINGS-6,
              TRPFILE.TP_EARNINGS_7 = :QTP-EARNINGS-7,
              TRPFILE.TP_EARNINGS_8 = :QTP-EARNINGS-8,
              TRPFILE.TP_EARNINGS_9 = :QTP-EARNINGS-9,
              TRPFILE.TP_EARNINGS_10 = :QTP-EARNINGS-10,
              TRPFILE.TP_WORKAMT_1 = :QTP-WORKAMT-1,
              TRPFILE.TP_WORKAMT_2 = :QTP-WORKAMT-2,
              TRPFILE.TP_WORKAMT_3 = :QTP-WORKAMT-3,
              TRPFILE.TP_WORKAMT_4 = :QTP-WORKAMT-4,
              TRPFILE.TP_WORKAMT_5 = :QTP-WORKAMT-5,
              TRPFILE.TP_WORKAMT_6 = :QTP-WORKAMT-6,
              TRPFILE.TP_WORKAMT_7 = :QTP-WORKAMT-7,
              TRPFILE.TP_WORKAMT_8 = :QTP-WORKAMT-8,
              TRPFILE.TP_WORKAMT_9 = :QTP-WORKAMT-9,
              TRPFILE.TP_WORKAMT_10 = :QTP-WORKAMT-10,
              TRPFILE.TP_OTHBAL = :QTP-OTHBAL,
              TRPFILE.TP_OT2BAL = :QTP-OT2BAL,
              TRPFILE.TP_INTBAL = :QTP-INTBAL,
              TRPFILE.TP_LCBAL = :QTP-LCBAL,
              TRPFILE.TP_CURBAL = :QTP-CURBAL,
              TRPFILE.TP_PDTH_DATE =
                 NULLIF(:QTP-PDTH-DATE,'1900-01-01'),
              TRPFILE.TP_DLNO = :QTP-DLNO,
              TRPFILE.TP_DLPROC = :QTP-DLPROC,
              TRPFILE.TP_SERVACCT = :QTP-SERVACCT,
              TRPFILE.TP_SALEFIN = :QTP-SALEFIN,
              TRPFILE.TP_INSURED = :QTP-INSURED,
              TRPFILE.TP_INSOURS = :QTP-INSOURS,
              TRPFILE.TP_LOANTYPE = :QTP-LOANTYPE,
              TRPFILE.TP_TILLNO = :QTP-TILLNO,
              TRPFILE.TP_LCPDTH_DATE =
                 NULLIF(:QTP-LCPDTH-DATE,'1900-01-01'),
              TRPFILE.TP_POSTING_BR = :QTP-POSTING-BR,
              TRPFILE.TP_OTHBR_FG = :QTP-OTHBR-FG,
              TRPFILE.TP_REV = :QTP-REV,
              TRPFILE.TP_REVTRCD = :QTP-REVTRCD,
              TRPFILE.TP_REVSEQNO = :QTP-REVSEQNO,
              TRPFILE.TP_STATUSFG = :QTP-STATUSFG,
              TRPFILE.TP_KEEPAPINT = :QTP-KEEPAPINT,
              TRPFILE.TP_INTPAID = :QTP-INTPAID,
              TRPFILE.TP_PRIOR_POOLID = :QTP-PRIOR-POOLID,
              TRPFILE.TP_INSTYPES = :QTP-INSTYPES,
              TRPFILE.TP_INSEARN_1 = :QTP-INSEARN-1,
              TRPFILE.TP_INSEARN_2 = :QTP-INSEARN-2,
              TRPFILE.TP_INSEARN_3 = :QTP-INSEARN-3,
              TRPFILE.TP_INSEARN_4 = :QTP-INSEARN-4,
              TRPFILE.TP_INSEARN_5 = :QTP-INSEARN-5,
              TRPFILE.TP_INSEARN_6 = :QTP-INSEARN-6,
              TRPFILE.TP_INSEARN_7 = :QTP-INSEARN-7,
              TRPFILE.TP_INSEARN_8 = :QTP-INSEARN-8,
              TRPFILE.TP_INSEARN_9 = :QTP-INSEARN-9,
              TRPFILE.TP_INSEARN_10 = :QTP-INSEARN-10,
              TRPFILE.TP_INSWORKAMT_1 = :QTP-INSWORKAMT-1,
              TRPFILE.TP_INSWORKAMT_2 = :QTP-INSWORKAMT-2,
              TRPFILE.TP_INSWORKAMT_3 = :QTP-INSWORKAMT-3,
              TRPFILE.TP_INSWORKAMT_4 = :QTP-INSWORKAMT-4,
              TRPFILE.TP_INSWORKAMT_5 = :QTP-INSWORKAMT-5,
              TRPFILE.TP_INSWORKAMT_6 = :QTP-INSWORKAMT-6,
              TRPFILE.TP_INSWORKAMT_7 = :QTP-INSWORKAMT-7,
              TRPFILE.TP_INSWORKAMT_8 = :QTP-INSWORKAMT-8,
              TRPFILE.TP_INSWORKAMT_9 = :QTP-INSWORKAMT-9,
              TRPFILE.TP_INSWORKAMT_10 = :QTP-INSWORKAMT-10,
              TRPFILE.TP_DLRVCHGS_ADJ_1 = :QTP-DLRVCHGS-ADJ-1,
              TRPFILE.TP_DLRVCHGS_ADJ_2 = :QTP-DLRVCHGS-ADJ-2,
              TRPFILE.TP_DLRVCHGS_ADJ_3 = :QTP-DLRVCHGS-ADJ-3,
              TRPFILE.TP_DLPARVHELD_ADJ = :QTP-DLPARVHELD-ADJ,
              TRPFILE.TP_DLPARVPAID_ADJ = :QTP-DLPARVPAID-ADJ,
              TRPFILE.TP_NEWSTAT_RATE = :QTP-NEWSTAT-RATE,
              TRPFILE.TP_POCD = :QTP-POCD,
              TRPFILE.TP_PODATE =
                 NULLIF(:QTP-PODATE,'1900-01-01'),
              TRPFILE.TP_DL_RVCHGS_CODE_1 = :QTP-DL-RVCHGS-CODE-1,
              TRPFILE.TP_DL_RVCHGS_CODE_2 = :QTP-DL-RVCHGS-CODE-2,
              TRPFILE.TP_DL_RVCHGS_CODE_3 = :QTP-DL-RVCHGS-CODE-3,
              TRPFILE.TP_DL_PARVHELD_CODE = :QTP-DL-PARVHELD-CODE,
              TRPFILE.TP_DL_PARVPAID_CODE = :QTP-DL-PARVPAID-CODE,
              TRPFILE.TP_TOTNODEF = :QTP-TOTNODEF,
              TRPFILE.TP_LNAMT = :QTP-LNAMT,
              TRPFILE.TP_ORIG_1STPYDATE =
                 NULLIF(:QTP-ORIG-1STPYDATE,'1900-01-01'),
              TRPFILE.TP_OLD1ST_PYDATE =
                 NULLIF(:QTP-OLD1ST-PYDATE,'1900-01-01'),
              TRPFILE.TP_1STPYDATE =
                 NULLIF(:QTP-1STPYDATE,'1900-01-01'),
              TRPFILE.TP_ORGTERM = :QTP-ORGTERM,
              TRPFILE.TP_INSCOMM = :QTP-INSCOMM,
              TRPFILE.TP_MANUAL_RB = :QTP-MANUAL-RB,
              TRPFILE.TP_USERID = :QTP-USERID,
              TRPFILE.TP_POSTTIME = :QTP-POSTTIME,
              TRPFILE.TP_PLTYPE = :QTP-PLTYPE,
              TRPFILE.TP_PL_OTH_AR = :QTP-PL-OTH-AR,
              TRPFILE.TP_LP_DLNO = :QTP-LP-DLNO,
              TRPFILE.TP_REPOCD = :QTP-REPOCD,
              TRPFILE.TP_SPRCLASS = :QTP-SPRCLASS,
              TRPFILE.TP_MAX_DEFER_FG = :QTP-MAX-DEFER-FG,
              TRPFILE.TP_POSTDATE =
                 NULLIF(:QTP-POSTDATE,'1900-01-01'),
              TRPFILE.TP_UNITPER_CD = :QTP-UNITPER-CD,
              TRPFILE.TP_UNITPER_FREQ = :QTP-UNITPER-FREQ,
              TRPFILE.TP_SP_DEFFRMLA = :QTP-SP-DEFFRMLA,
              TRPFILE.TP_COLLECTOR = :QTP-COLLECTOR,
              TRPFILE.TP_PENALTY_INT = :QTP-PENALTY-INT,
              TRPFILE.TP_INSCOMP = :QTP-INSCOMP,
              TRPFILE.TP_PROMDATE =
                 NULLIF(:QTP-PROMDATE,'1900-01-01'),
              TRPFILE.TP_PROMAMT = :QTP-PROMAMT,
              TRPFILE.TP_PROMID = :QTP-PROMID,
              TRPFILE.TP_INS_TAX_RB = :QTP-INS-TAX-RB,
              TRPFILE.TP_PROMDAYS_PASTDUE = :QTP-PROMDAYS-PASTDUE,
              TRPFILE.TP_MIP_FG = :QTP-MIP-FG,
              TRPFILE.TP_MIP_CLPAID = :QTP-MIP-CLPAID,
              TRPFILE.TP_MIP_CLCOMM = :QTP-MIP-CLCOMM,
              TRPFILE.TP_MIP_AHPAID = :QTP-MIP-AHPAID,
              TRPFILE.TP_MIP_AHCOMM = :QTP-MIP-AHCOMM,
              TRPFILE.TP_MIP_CLPDTH_DATE =
                 NULLIF(:QTP-MIP-CLPDTH-DATE,'1900-01-01'),
              TRPFILE.TP_MIP_AHPDTH_DATE =
                 NULLIF(:QTP-MIP-AHPDTH-DATE,'1900-01-01'),
              TRPFILE.TP_INS_TAX_COUNTY_CD =
                   :QTP-INS-TAX-COUNTY-CD,
              TRPFILE.TP_INS_TAX_CITY_CD = :QTP-INS-TAX-CITY-CD,
              TRPFILE.TP_APPLIED_TO_ESCROW =
                   :QTP-APPLIED-TO-ESCROW,
              TRPFILE.TP_ESCROW_AMT = :QTP-ESCROW-AMT,
              TRPFILE.TP_ESCROW_PREPAID = :QTP-ESCROW-PREPAID,
              TRPFILE.TP_ESCROW_DISBURSED = :QTP-ESCROW-DISBURSED,
              TRPFILE.TP_LP_SEQNO = :QTP-LP-SEQNO,
              TRPFILE.TP_PRIOR_DF_CODE = :QTP-PRIOR-DF-CODE,
              TRPFILE.TP_PRIOR_DF_POSTDATE =
                 NULLIF(:QTP-PRIOR-DF-POSTDATE,'1900-01-01'),
              TRPFILE.TP_CANCEL_REASON = :QTP-CANCEL-REASON,
              TRPFILE.TP_FROMCL = :QTP-FROMCL,
              TRPFILE.TP_TOCL = :QTP-TOCL,
              TRPFILE.TP_ADDON_TXT = :QTP-ADDON-TXT,
              TRPFILE.TP_ADDON_TYP = :QTP-ADDON-TYP,
              TRPFILE.TP_DLRVNUM = :QTP-DLRVNUM,
              TRPFILE.TP_P4P5_TRANCD = :QTP-P4P5-TRANCD,
              TRPFILE.TP_P4P5_DLRVNUM = :QTP-P4P5-DLRVNUM,
              TRPFILE.TP_POOLID = :QTP-POOLID
            WHERE TRPFILE.TP_BRNO = :QTP-BRNO
              AND TRPFILE.TP_DATE = :QTP-DATE
              AND TRPFILE.TP_CLASS = :QTP-CLASS
              AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
              AND TRPFILE.TP_LINE = :QTP-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRP1-FILE.

           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           PERFORM DELETE-IT.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           PERFORM SET-TRP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRPFILE
            WHERE TRPFILE.TP_BRNO = :QTP-BRNO
              AND TRPFILE.TP_DATE = :QTP-DATE
              AND TRPFILE.TP_CLASS = :QTP-CLASS
              AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
              AND TRPFILE.TP_LINE = :QTP-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
      *=================================================================
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
              DESTROY IOTRPR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
