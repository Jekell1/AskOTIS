       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOBWRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-26-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOBWRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : BWFILE
      * TABLE DESC    : BORROWER MASTER FILE
      * OUTPUT FILE   : XPC/IOBWRN.PRE
      * USED FOR      : LIBLP/LPWSBW.CPY  --- QBW-REC
      *                 LIBLP/LPBW1R.CPY  --- READ
      *                 LIBLP/LPBW1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPBW2RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPBW3RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPBW4RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0326 ***NEW***
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
           "@(#)IO/IOBWRN.CBL S34 06/29/21-13:01:36 >".

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
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LPWSBW.CPY".


      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QBW1-WHERE-BEG-KEY.
           03  QBW1-WBEG-OWNBR             PIC 9(4).
           03  QBW1-WBEG-SSNO             PIC 9(9).
       01  QBW1-WHERE-END-KEY.
           03  QBW1-WEND-OWNBR             PIC 9(4).
           03  QBW1-WEND-SSNO             PIC 9(9).
       01  QBW2-WHERE-BEG-KEY.
           03  QBW2-WBEG-OWNBR             PIC 9(4).
           03  QBW2-WBEG-LNAME            PIC X(35).
           03  QBW2-WBEG-FNAME            PIC X(35).
       01  QBW2-WHERE-END-KEY.
           03  QBW2-WEND-OWNBR             PIC 9(4).
           03  QBW2-WEND-LNAME            PIC X(35).
           03  QBW2-WEND-FNAME            PIC X(35).
       01  QBW3-WHERE-BEG-KEY.
           03  QBW3-WBEG-OWNBR             PIC 9(4).
           03  QBW3-WBEG-SPSSNO           PIC 9(9).
       01  QBW3-WHERE-END-KEY.
           03  QBW3-WEND-OWNBR             PIC 9(4).
           03  QBW3-WEND-SPSSNO           PIC 9(9).
       01  QBW4-WHERE-BEG-KEY.
           03  QBW4-WBEG-OWNBR             PIC 9(4).
           03  QBW4-WBEG-SPLNAME          PIC X(35).
           03  QBW4-WBEG-SPFNAME          PIC X(35).
       01  QBW4-WHERE-END-KEY.
           03  QBW4-WEND-OWNBR             PIC 9(4).
           03  QBW4-WEND-SPLNAME          PIC X(35).
           03  QBW4-WEND-SPFNAME          PIC X(35).
 
       01  QBW-REC.
           03  QBW-OWNBR                              PIC 9(4).
           03  QBW-SSNO                               PIC 9(9).
           03  QBW-LNAME                              PIC X(35).
           03  QBW-FNAME                              PIC X(35).
           03  QBW-MNAME                              PIC X(35).
           03  QBW-NICKNAME                           PIC X(35).
           03  QBW-GENERATION                         PIC X(7).
           03  QBW-ADR1                               PIC X(30).
           03  QBW-ADR2                               PIC X(30).
           03  QBW-CITY                               PIC X(30).
           03  QBW-STATE                              PIC XX.
           03  QBW-ZIP                                PIC X(10).
           03  QBW-LTOUCH-DATE                        PIC X(10).
           03  QBW-REFDATE                            PIC X(10).
           03  QBW-BDATE                              PIC X(10).
           03  QBW-PHONE                              PIC 9(10).
           03  QBW-CELL-PHONE                         PIC 9(10).
           03  QBW-NATIONALITY                        PIC X(8).
           03  QBW-GENDER                             PIC X.
           03  QBW-DEPEND                             PIC 99.
           03  QBW-RENTCD                             PIC X(4).
           03  QBW-TIMEADDR                           PIC X(4).
           03  QBW-CREDITCD                           PIC X(4).
           03  QBW-STRTGCD                            PIC X(6).
           03  QBW-DWELLCD                            PIC XX.
           03  QBW-COUNTY                             PIC X(4).
           03  QBW-ORIGIN                             PIC X(4).
           03  QBW-CENSUSCD                           PIC X(10).
           03  QBW-ORGSLSMN                           PIC 9(4).
           03  QBW-REPCODE                            PIC X(4).
           03  QBW-SPSSNO                             PIC 9(9).
           03  QBW-SPLNAME                            PIC X(35).
           03  QBW-SPFNAME                            PIC X(35).
           03  QBW-SP-BDATE                           PIC X(10).
           03  QBW-EMPLOYER                           PIC X(35).
           03  QBW-EMPLPOS                            PIC X(24).
           03  QBW-EMPPHONE                           PIC 9(10).
           03  QBW-EMPEXT                             PIC 9(4).
           03  QBW-EMPYEARS                           PIC 99.
           03  QBW-EMPPAY                             PIC 9(7)V99.
           03  QBW-EMPPAYOT                           PIC 9(7)V99.
           03  QBW-EMPPAYSR                           PIC X(8).
           03  QBW-EMPADDR1                           PIC X(30).
           03  QBW-EMPADDR2                           PIC X(30).
           03  QBW-EMPCITY                            PIC X(30).
           03  QBW-EMPSTATE                           PIC XX.
           03  QBW-EMPZIP                             PIC X(10).
           03  QBW-WORKHRS                            PIC X(9).
           03  QBW-LOANS                              PIC 99.
           03  QBW-INTYTD                             PIC S9(7)V99.
           03  QBW-INTLYR                             PIC S9(7)V99.
           03  QBW-LIASMLN                            PIC S9(7)V99.
           03  QBW-LIAOTH                             PIC S9(7)V99.
           03  QBW-LASTPODATE                         PIC X(10).
           03  QBW-PRLOANS                            PIC 9(3).
           03  QBW-RE-INTYTD                          PIC S9(7)V99.
           03  QBW-RE-INTLYR                          PIC S9(7)V99.
           03  QBW-CREDIT-LIMIT                       PIC 9(7)V99.
           03  QBW-CREDIT-SCORE                       PIC 9(4).
           03  QBW-CRED-SCORE-1                       PIC 9(4).
           03  QBW-CRED-SCORE-2                       PIC 9(4).
           03  QBW-CRED-SCORE-3                       PIC 9(4).
           03  QBW-CRED-SCORE-4                       PIC 9(4).
           03  QBW-BNKRPT-SCORE-1                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-2                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-3                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-4                     PIC 9(4).
           03  QBW-LAST-FORM-NO                       PIC 9(4).
           03  QBW-LAST-FORM-DATE                     PIC X(10).
           03  QBW-PRIOR-RN-FOR10-DATE                PIC X(10).
           03  QBW-LAST-RN-FOR10-DATE                 PIC X(10).
           03  QBW-ADR-PHONE-CHG-DATE                 PIC X(10).
           03  QBW-BAD-ADR-FG                         PIC X(1).
           03  QBW-BAD-PHONE-FG                       PIC X(1).
           03  QBW-BAD-EMPPHONE-FG                    PIC X(1).
           03  QBW-BAD-MISC-PHONE1-FG                 PIC X(1).
           03  QBW-BAD-MISC-PHONE2-FG                 PIC X(1).
           03  QBW-BAD-CELL-FG                        PIC X(1).
           03  QBW-MISC-PHONE-1                       PIC 9(10).
           03  QBW-MISC-EXT-1                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-1                    PIC XX.
           03  QBW-MISC-PHONE-2                       PIC 9(10).
           03  QBW-MISC-EXT-2                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-2                    PIC XX.
           03  QBW-MISC-PHONE-3                       PIC 9(10).
           03  QBW-MISC-EXT-3                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-3                    PIC XX.
           03  QBW-MISC-PHONE-4                       PIC 9(10).
           03  QBW-MISC-EXT-4                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-4                    PIC XX.
           03  QBW-MISC-PHONE-5                       PIC 9(10).
           03  QBW-MISC-EXT-5                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-5                    PIC XX.
           03  QBW-MISC-PHONE-6                       PIC 9(10).
           03  QBW-MISC-EXT-6                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-6                    PIC XX.
           03  QBW-CONTACT-DESC-1                     PIC X(46).
           03  QBW-CONTACT-PHONE-1                    PIC 9(10).
           03  QBW-CONTACT-EXT-1                      PIC 9(4).
           03  QBW-CONTACT-RELATION-1                 PIC X(12).
           03  QBW-CONTACT-DESC-2                     PIC X(46).
           03  QBW-CONTACT-PHONE-2                    PIC 9(10).
           03  QBW-CONTACT-EXT-2                      PIC 9(4).
           03  QBW-CONTACT-RELATION-2                 PIC X(12).
           03  QBW-CONTACT-DESC-3                     PIC X(46).
           03  QBW-CONTACT-PHONE-3                    PIC 9(10).
           03  QBW-CONTACT-EXT-3                      PIC 9(4).
           03  QBW-CONTACT-RELATION-3                 PIC X(12).
           03  QBW-CONTACT-DESC-4                     PIC X(46).
           03  QBW-CONTACT-PHONE-4                    PIC 9(10).
           03  QBW-CONTACT-EXT-4                      PIC 9(4).
           03  QBW-CONTACT-RELATION-4                 PIC X(12).
           03  QBW-CONTACT-DESC-5                     PIC X(46).
           03  QBW-CONTACT-PHONE-5                    PIC 9(10).
           03  QBW-CONTACT-EXT-5                      PIC 9(4).
           03  QBW-CONTACT-RELATION-5                 PIC X(12).
           03  QBW-CONTACT-DESC-6                     PIC X(46).
           03  QBW-CONTACT-PHONE-6                    PIC 9(10).
           03  QBW-CONTACT-EXT-6                      PIC 9(4).
           03  QBW-CONTACT-RELATION-6                 PIC X(12).
           03  QBW-CONTACT-DESC-7                     PIC X(46).
           03  QBW-CONTACT-PHONE-7                    PIC 9(10).
           03  QBW-CONTACT-EXT-7                      PIC 9(4).
           03  QBW-CONTACT-RELATION-7                 PIC X(12).
           03  QBW-CONTACT-DESC-8                     PIC X(46).
           03  QBW-CONTACT-PHONE-8                    PIC 9(10).
           03  QBW-CONTACT-EXT-8                      PIC 9(4).
           03  QBW-CONTACT-RELATION-8                 PIC X(12).
           03  QBW-CONTACT-DESC-9                     PIC X(46).
           03  QBW-CONTACT-PHONE-9                    PIC 9(10).
           03  QBW-CONTACT-EXT-9                      PIC 9(4).
           03  QBW-CONTACT-RELATION-9                 PIC X(12).
           03  QBW-CONTACT-DESC-10                    PIC X(46).
           03  QBW-CONTACT-PHONE-10                   PIC 9(10).
           03  QBW-CONTACT-EXT-10                     PIC 9(4).
           03  QBW-CONTACT-RELATION-10                PIC X(12).
           03  QBW-SRCD                               PIC XX.
           03  QBW-CALL-ON-SUNDAY                     PIC X.
           03  QBW-SOLICIT                            PIC X.
           03  QBW-CONFIDENTIAL                       PIC X.
           03  QBW-PRIVACY-OPT-OUT                    PIC X.
           03  QBW-OPT-IN-EMAIL                       PIC X.
           03  QBW-OPT-IN-TEXT                        PIC X.
           03  QBW-CITY-TAX-CD                        PIC X(3).
           03  QBW-DEALTRACK-CHG-DATE                 PIC X(10).
           03  QBW-DECEASED                           PIC X.
           03  QBW-DECEASED-DATE                      PIC X(10).
           03  QBW-EMAIL                              PIC X(40).
           03  QBW-IDENT-NO                           PIC X(18).
           03  QBW-IDENT-TYPE                         PIC XX.
           03  QBW-IDENT-STATE                        PIC XX.
           03  QBW-MILITARY-FLAG                      PIC X.
           03  QBW-ORGBR                              PIC 9(4).

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
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).
           03  BEG-LNAME               PIC X(35).
           03  END-LNAME               PIC X(35).
           03  BEG-SPSSNO              PIC 9(09).
           03  END-SPSSNO              PIC 9(09).
           03  BEG-SPLNAME             PIC X(35).
           03  END-SPLNAME             PIC X(35).

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
           03  FILLER PIC X(41) VALUE "BRNO    SSNO   LNAME".
           03  FILLER PIC X(30) VALUE "FNAME        BDATE     LIASMLN".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(06) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BW-OWNBR                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-BW-SSNO                    PIC 9(09).
           03  FILLER                         PIC X(01).
           03  D01-BW-LNAME                   PIC X(25).
           03  FILLER                         PIC X(01).
           03  D01-BW-FNAME                   PIC X(10).
           03  FILLER                         PIC X(01).
           03  D01-BW-BDATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(01).
           03  D01-BW-LIASMLN                 PIC Z(6)9.99-.

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
       COPY "LIBIO/IOBWRN_DEF.CPY".
       COPY "LIBIO/IOBWRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOBWRN_SCN.CPY".

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
           MOVE "IOBWRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOBWRN-QUEUE-POS  TO Q-POS.
           MOVE IOBWRN-COPIES-POS TO C-POS.
           MOVE IOBWRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-SSNO.

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
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES                 TO VDUBUF.
           PERFORM SCREENIO.

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
              ( VDUNUM0 > 5      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-SSNO.

           MOVE "ERNX090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE "ERNX090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SRNX090BEG-SSNO." TO VDU.
           MOVE ZEROES             TO VDU-SSNO-EDIT BEG-SSNO.
           PERFORM SCREENIO.

           MOVE "SRNX090END-SSNO." TO VDU.
           MOVE 999999999          TO VDU-SSNO-EDIT END-SSNO.
           PERFORM SCREENIO.
      *=================================================================
       ENTER-BEG-LNAME.

           MOVE "E  A350BEG-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LNAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-END-LNAME.

           MOVE "E  A350END-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-LNAME.

           IF ( BEG-LNAME > END-LNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNAME.

           GO TO ENTER-BEG-SPSSNO.

      *-----------------------------------------------------------------
       ENTER-ALL-LNAME.

           MOVE "S  A000BEG-LNAME." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-LNAME.
           PERFORM SCREENIO.

           MOVE "S  A000END-LNAME." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-LNAME.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SPSSNO.

           MOVE "ERNX090BEG-SPSSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SPSSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SPSSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SPSSNO.

      *-----------------------------------------------------------------
       ENTER-END-SPSSNO.

           MOVE "ERNX090END-SPSSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SPSSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SPSSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SPSSNO.

           IF ( BEG-SPSSNO > END-SPSSNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SPSSNO.

           GO TO ENTER-BEG-SPLNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-SPSSNO.

           MOVE "SRNX090BEG-SPSSNO." TO VDU.
           MOVE ZEROES             TO VDU-SSNO-EDIT BEG-SPSSNO.
           PERFORM SCREENIO.

           MOVE "SRNX090END-SPSSNO." TO VDU.
           MOVE 999999999          TO VDU-SSNO-EDIT END-SPSSNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SPLNAME.

           MOVE "E  A350BEG-SPLNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SPSSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SPLNAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SPLNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-SPLNAME.

      *-----------------------------------------------------------------
       ENTER-END-SPLNAME.

           MOVE "E  A350END-SPLNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SPLNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SPLNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-SPLNAME.

           IF ( BEG-SPLNAME > END-SPLNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SPLNAME.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SPLNAME.

           MOVE "S  A000BEG-SPLNAME." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-SPLNAME.
           PERFORM SCREENIO.

           MOVE "S  A000END-SPLNAME." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-SPLNAME.
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

           MOVE "SRNX090BEG-SSNO."        TO VDU.
           MOVE BEG-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.
           MOVE "SRNX090END-SSNO."        TO VDU.
           MOVE END-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A350BEG-LNAME."        TO VDU.
           MOVE BEG-LNAME                  TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A350END-LNAME."        TO VDU.
           MOVE END-LNAME                  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SRNX090BEG-SPSSNO."        TO VDU.
           MOVE BEG-SPSSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.
           MOVE "SRNX090END-SPSSNO."        TO VDU.
           MOVE END-SPSSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A350BEG-SPLNAME."        TO VDU.
           MOVE BEG-SPLNAME                  TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A350END-SPLNAME."        TO VDU.
           MOVE END-SPLNAME                  TO VDUBUF.
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

           MOVE "B"                        TO BW-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QBW1-WBEG-OWNBR 
                              QBW2-WBEG-OWNBR
                              QBW3-WBEG-OWNBR
                              QBW4-WBEG-OWNBR.

           MOVE BR-NO       TO QBW1-WEND-OWNBR
                                QBW2-WEND-OWNBR
                                QBW3-WEND-OWNBR
                                QBW4-WEND-OWNBR.

           MOVE BEG-SSNO     TO QBW1-WBEG-SSNO.
           MOVE END-SSNO     TO QBW1-WEND-SSNO.
           MOVE BEG-LNAME    TO QBW2-WBEG-LNAME.
           MOVE END-LNAME    TO QBW2-WEND-LNAME.
           MOVE SPACES       TO QBW2-WBEG-FNAME.
           MOVE ALL "Z"      TO QBW2-WEND-FNAME.

           MOVE BEG-SPSSNO   TO QBW3-WBEG-SPSSNO.
           MOVE END-SPSSNO   TO QBW3-WEND-SPSSNO.
           MOVE BEG-SPLNAME  TO QBW4-WBEG-SPLNAME.
           MOVE END-SPLNAME  TO QBW4-WEND-SPLNAME.
           MOVE SPACES       TO QBW4-WBEG-SPFNAME.
           MOVE ALL "Z"      TO QBW4-WEND-SPFNAME.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-BW1-FILE
              PERFORM START-BW1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-BW2-FILE
              PERFORM START-BW2-FILE
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM OPEN-BW3-FILE
              PERFORM START-BW3-FILE
           ELSE
           IF ( PROCESS-KEY = 4 )
              PERFORM OPEN-BW4-FILE
              PERFORM START-BW4-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-BW1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-BW2-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM READ-BW3-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 4 )
              PERFORM READ-BW4-FILE-NEXT.
           IF ( IO-BAD ) OR ( BW-OWNBR NOT = BW-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( BW-SSNO < BEG-SSNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( BW-SSNO > END-SSNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 2 )
              IF ( BW-LNAME  < BEG-LNAME  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( BW-LNAME  > END-LNAME  )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 3 )
              IF ( BW-SPSSNO < BEG-SPSSNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( BW-SPSSNO > END-SPSSNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 4 )
              IF ( BW-SPLNAME  < BEG-SPLNAME  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( BW-SPLNAME  > END-SPLNAME  )
                 GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE BW-OWNBR                    TO D01-BW-OWNBR.
           MOVE BW-SSNO                     TO D01-BW-SSNO.
           MOVE BW-LNAME                    TO D01-BW-LNAME.
           MOVE BW-FNAME                    TO D01-BW-FNAME.
           MOVE BW-BDATE                    TO D01-BW-BDATE.
           MOVE BW-LIASMLN                  TO D01-BW-LIASMLN.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-BW1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-BW2-FILE
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM CLOSE-BW3-FILE
           ELSE
           IF ( PROCESS-KEY = 4 )
              PERFORM CLOSE-BW4-FILE.

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

           IF ( PROCESS-KEY = 1 )
              MOVE BW1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING BW-OWNBR, BW-LNAME, BW-FNAME
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 3 )
              STRING BW-OWNBR, BW-SPSSNO
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY
           ELSE
              STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING SSNO               : "    TO R01-PROMPT.
           MOVE BEG-SSNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO               : "    TO R01-PROMPT.
           MOVE END-SSNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LNAME               : "    TO R01-PROMPT.
           MOVE BEG-LNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LNAME               : "    TO R01-PROMPT.
           MOVE END-LNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING FNAME               : "    TO R01-PROMPT.
           MOVE SPACES                                TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FNAME               : "    TO R01-PROMPT.
           MOVE ALL "Z"                               TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SPSSNO               : "    TO R01-PROMPT.
           MOVE BEG-SPSSNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SPSSNO               : "    TO R01-PROMPT.
           MOVE END-SPSSNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SPLNAME               : "    TO R01-PROMPT.
           MOVE BEG-SPLNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SPLNAME               : "    TO R01-PROMPT.
           MOVE END-SPLNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SPFNAME               : "    TO R01-PROMPT.
           MOVE SPACES                                  TO R01-TEXT.
           MOVE 1                                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SPFNAME               : "    TO R01-PROMPT.
           MOVE ALL "Z"                                 TO R01-TEXT.
           MOVE 1                                       TO LN-FEED.
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
           MOVE "SQL TABLE IO UTILITY - BWFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOBWRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOBWRN-SYSDATE.
           DISPLAY IOBWRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOBWRN-FORM-FIELDS TO WR-BUF.
           MOVE IOBWRN-FORM-LIST TO WR-LIST.
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

           MOVE IOBWRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOBWRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOBWRN_EVA.CPY".
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
      *    LOAD BW-REC FROM QBW-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BW-FIELDS.
           MOVE QBW-OWNBR TO BW-OWNBR.
           MOVE QBW-SSNO TO BW-SSNO.
           MOVE QBW-LNAME TO BW-LNAME.
           MOVE QBW-FNAME TO BW-FNAME.
           MOVE QBW-MNAME TO BW-MNAME.
           MOVE QBW-NICKNAME TO BW-NICKNAME.
           MOVE QBW-GENERATION TO BW-GENERATION.
           MOVE QBW-ADR1 TO BW-ADR1.
           MOVE QBW-ADR2 TO BW-ADR2.
           MOVE QBW-CITY TO BW-CITY.
           MOVE QBW-STATE TO BW-STATE.
           MOVE QBW-ZIP TO BW-ZIP.

           MOVE QBW-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-LTOUCH-DATE.


           MOVE QBW-REFDATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-REFDATE.


           MOVE QBW-BDATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-BDATE.

           MOVE QBW-PHONE TO BW-PHONE.
           MOVE QBW-CELL-PHONE TO BW-CELL-PHONE.
           MOVE QBW-NATIONALITY TO BW-NATIONALITY.
           MOVE QBW-GENDER TO BW-GENDER.
           MOVE QBW-DEPEND TO BW-DEPEND.
           MOVE QBW-RENTCD TO BW-RENTCD.
           MOVE QBW-TIMEADDR TO BW-TIMEADDR.
           MOVE QBW-CREDITCD TO BW-CREDITCD.
           MOVE QBW-STRTGCD TO BW-STRTGCD.
           MOVE QBW-DWELLCD TO BW-DWELLCD.
           MOVE QBW-COUNTY TO BW-COUNTY.
           MOVE QBW-ORIGIN TO BW-ORIGIN.
           MOVE QBW-CENSUSCD TO BW-CENSUSCD.
           MOVE QBW-ORGSLSMN TO BW-ORGSLSMN.
           MOVE QBW-REPCODE TO BW-REPCODE.
           MOVE QBW-SPSSNO TO BW-SPSSNO.
           MOVE QBW-SPLNAME TO BW-SPLNAME.
           MOVE QBW-SPFNAME TO BW-SPFNAME.

           MOVE QBW-SP-BDATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-SP-BDATE.

           MOVE QBW-EMPLOYER TO BW-EMPLOYER.
           MOVE QBW-EMPLPOS TO BW-EMPLPOS.
           MOVE QBW-EMPPHONE TO BW-EMPPHONE.
           MOVE QBW-EMPEXT TO BW-EMPEXT.
           MOVE QBW-EMPYEARS TO BW-EMPYEARS.
           MOVE QBW-EMPPAY TO BW-EMPPAY.
           MOVE QBW-EMPPAYOT TO BW-EMPPAYOT.
           MOVE QBW-EMPPAYSR TO BW-EMPPAYSR.
           MOVE QBW-EMPADDR1 TO BW-EMPADDR1.
           MOVE QBW-EMPADDR2 TO BW-EMPADDR2.
           MOVE QBW-EMPCITY TO BW-EMPCITY.
           MOVE QBW-EMPSTATE TO BW-EMPSTATE.
           MOVE QBW-EMPZIP TO BW-EMPZIP.
           MOVE QBW-WORKHRS TO BW-WORKHRS.
           MOVE QBW-LOANS TO BW-LOANS.
           MOVE QBW-INTYTD TO BW-INTYTD.
           MOVE QBW-INTLYR TO BW-INTLYR.
           MOVE QBW-LIASMLN TO BW-LIASMLN.
           MOVE QBW-LIAOTH TO BW-LIAOTH.

           MOVE QBW-LASTPODATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-LASTPODATE.

           MOVE QBW-PRLOANS TO BW-PRLOANS.
           MOVE QBW-RE-INTYTD TO BW-RE-INTYTD.
           MOVE QBW-RE-INTLYR TO BW-RE-INTLYR.
           MOVE QBW-CREDIT-LIMIT TO BW-CREDIT-LIMIT.
           MOVE QBW-CREDIT-SCORE TO BW-CREDIT-SCORE.
           MOVE QBW-CRED-SCORE-1 TO BW-CRED-SCORE(1).
           MOVE QBW-CRED-SCORE-2 TO BW-CRED-SCORE(2).
           MOVE QBW-CRED-SCORE-3 TO BW-CRED-SCORE(3).
           MOVE QBW-CRED-SCORE-4 TO BW-CRED-SCORE(4).
           MOVE QBW-BNKRPT-SCORE-1 TO BW-BNKRPT-SCORE(1).
           MOVE QBW-BNKRPT-SCORE-2 TO BW-BNKRPT-SCORE(2).
           MOVE QBW-BNKRPT-SCORE-3 TO BW-BNKRPT-SCORE(3).
           MOVE QBW-BNKRPT-SCORE-4 TO BW-BNKRPT-SCORE(4).
           MOVE QBW-LAST-FORM-NO TO BW-LAST-FORM-NO.

           MOVE QBW-LAST-FORM-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-LAST-FORM-DATE.


           MOVE QBW-PRIOR-RN-FOR10-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-PRIOR-RN-FOR10-DATE.


           MOVE QBW-LAST-RN-FOR10-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-LAST-RN-FOR10-DATE.


           MOVE QBW-ADR-PHONE-CHG-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-ADR-PHONE-CHG-DATE.

           MOVE QBW-BAD-ADR-FG TO BW-BAD-ADR-FG.
           MOVE QBW-BAD-PHONE-FG TO BW-BAD-PHONE-FG.
           MOVE QBW-BAD-EMPPHONE-FG TO BW-BAD-EMPPHONE-FG.
           MOVE QBW-BAD-MISC-PHONE1-FG TO BW-BAD-MISC-PHONE1-FG.
           MOVE QBW-BAD-MISC-PHONE2-FG TO BW-BAD-MISC-PHONE2-FG.
           MOVE QBW-BAD-CELL-FG TO BW-BAD-CELL-FG.
           MOVE QBW-MISC-PHONE-1 TO BW-MISC-PHONE(1).
           MOVE QBW-MISC-EXT-1 TO BW-MISC-EXT(1).
           MOVE QBW-MISC-PHONE-CD-1 TO BW-MISC-PHONE-CD(1).
           MOVE QBW-MISC-PHONE-2 TO BW-MISC-PHONE(2).
           MOVE QBW-MISC-EXT-2 TO BW-MISC-EXT(2).
           MOVE QBW-MISC-PHONE-CD-2 TO BW-MISC-PHONE-CD(2).
           MOVE QBW-MISC-PHONE-3 TO BW-MISC-PHONE(3).
           MOVE QBW-MISC-EXT-3 TO BW-MISC-EXT(3).
           MOVE QBW-MISC-PHONE-CD-3 TO BW-MISC-PHONE-CD(3).
           MOVE QBW-MISC-PHONE-4 TO BW-MISC-PHONE(4).
           MOVE QBW-MISC-EXT-4 TO BW-MISC-EXT(4).
           MOVE QBW-MISC-PHONE-CD-4 TO BW-MISC-PHONE-CD(4).
           MOVE QBW-MISC-PHONE-5 TO BW-MISC-PHONE(5).
           MOVE QBW-MISC-EXT-5 TO BW-MISC-EXT(5).
           MOVE QBW-MISC-PHONE-CD-5 TO BW-MISC-PHONE-CD(5).
           MOVE QBW-MISC-PHONE-6 TO BW-MISC-PHONE(6).
           MOVE QBW-MISC-EXT-6 TO BW-MISC-EXT(6).
           MOVE QBW-MISC-PHONE-CD-6 TO BW-MISC-PHONE-CD(6).
           MOVE QBW-CONTACT-DESC-1 TO BW-CONTACT-DESC(1).
           MOVE QBW-CONTACT-PHONE-1 TO BW-CONTACT-PHONE(1).
           MOVE QBW-CONTACT-EXT-1 TO BW-CONTACT-EXT(1).
           MOVE QBW-CONTACT-RELATION-1 TO BW-CONTACT-RELATION(1).
           MOVE QBW-CONTACT-DESC-2 TO BW-CONTACT-DESC(2).
           MOVE QBW-CONTACT-PHONE-2 TO BW-CONTACT-PHONE(2).
           MOVE QBW-CONTACT-EXT-2 TO BW-CONTACT-EXT(2).
           MOVE QBW-CONTACT-RELATION-2 TO BW-CONTACT-RELATION(2).
           MOVE QBW-CONTACT-DESC-3 TO BW-CONTACT-DESC(3).
           MOVE QBW-CONTACT-PHONE-3 TO BW-CONTACT-PHONE(3).
           MOVE QBW-CONTACT-EXT-3 TO BW-CONTACT-EXT(3).
           MOVE QBW-CONTACT-RELATION-3 TO BW-CONTACT-RELATION(3).
           MOVE QBW-CONTACT-DESC-4 TO BW-CONTACT-DESC(4).
           MOVE QBW-CONTACT-PHONE-4 TO BW-CONTACT-PHONE(4).
           MOVE QBW-CONTACT-EXT-4 TO BW-CONTACT-EXT(4).
           MOVE QBW-CONTACT-RELATION-4 TO BW-CONTACT-RELATION(4).
           MOVE QBW-CONTACT-DESC-5 TO BW-CONTACT-DESC(5).
           MOVE QBW-CONTACT-PHONE-5 TO BW-CONTACT-PHONE(5).
           MOVE QBW-CONTACT-EXT-5 TO BW-CONTACT-EXT(5).
           MOVE QBW-CONTACT-RELATION-5 TO BW-CONTACT-RELATION(5).
           MOVE QBW-CONTACT-DESC-6 TO BW-CONTACT-DESC(6).
           MOVE QBW-CONTACT-PHONE-6 TO BW-CONTACT-PHONE(6).
           MOVE QBW-CONTACT-EXT-6 TO BW-CONTACT-EXT(6).
           MOVE QBW-CONTACT-RELATION-6 TO BW-CONTACT-RELATION(6).
           MOVE QBW-CONTACT-DESC-7 TO BW-CONTACT-DESC(7).
           MOVE QBW-CONTACT-PHONE-7 TO BW-CONTACT-PHONE(7).
           MOVE QBW-CONTACT-EXT-7 TO BW-CONTACT-EXT(7).
           MOVE QBW-CONTACT-RELATION-7 TO BW-CONTACT-RELATION(7).
           MOVE QBW-CONTACT-DESC-8 TO BW-CONTACT-DESC(8).
           MOVE QBW-CONTACT-PHONE-8 TO BW-CONTACT-PHONE(8).
           MOVE QBW-CONTACT-EXT-8 TO BW-CONTACT-EXT(8).
           MOVE QBW-CONTACT-RELATION-8 TO BW-CONTACT-RELATION(8).
           MOVE QBW-CONTACT-DESC-9 TO BW-CONTACT-DESC(9).
           MOVE QBW-CONTACT-PHONE-9 TO BW-CONTACT-PHONE(9).
           MOVE QBW-CONTACT-EXT-9 TO BW-CONTACT-EXT(9).
           MOVE QBW-CONTACT-RELATION-9 TO BW-CONTACT-RELATION(9).
           MOVE QBW-CONTACT-DESC-10 TO BW-CONTACT-DESC(10).
           MOVE QBW-CONTACT-PHONE-10 TO BW-CONTACT-PHONE(10).
           MOVE QBW-CONTACT-EXT-10 TO BW-CONTACT-EXT(10).
           MOVE QBW-CONTACT-RELATION-10 TO BW-CONTACT-RELATION(10).
           MOVE QBW-SRCD TO BW-SRCD.
           MOVE QBW-CALL-ON-SUNDAY TO BW-CALL-ON-SUNDAY.
           MOVE QBW-SOLICIT TO BW-SOLICIT.
           MOVE QBW-CONFIDENTIAL TO BW-CONFIDENTIAL.
           MOVE QBW-PRIVACY-OPT-OUT TO BW-PRIVACY-OPT-OUT.
           MOVE QBW-OPT-IN-EMAIL TO BW-OPT-IN-EMAIL.
           MOVE QBW-OPT-IN-TEXT TO BW-OPT-IN-TEXT.
           MOVE QBW-CITY-TAX-CD TO BW-CITY-TAX-CD.

           MOVE QBW-DEALTRACK-CHG-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-DEALTRACK-CHG-DATE.

           MOVE QBW-DECEASED TO BW-DECEASED.

           MOVE QBW-DECEASED-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-DECEASED-DATE.

           MOVE QBW-EMAIL TO BW-EMAIL.
           MOVE QBW-IDENT-NO TO BW-IDENT-NO.
           MOVE QBW-IDENT-TYPE TO BW-IDENT-TYPE.
           MOVE QBW-IDENT-STATE TO BW-IDENT-STATE.
           MOVE QBW-MILITARY-FLAG TO BW-MILITARY-FLAG.
           MOVE QBW-ORGBR TO BW-ORGBR.
 
      *-----------------------------------------------------------------
      *    LOAD QBW-REC FROM BW-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BW-FIELDS.
           MOVE BW-OWNBR TO QBW-OWNBR.
           MOVE BW-SSNO TO QBW-SSNO.
           MOVE BW-LNAME TO QBW-LNAME.
           MOVE BW-FNAME TO QBW-FNAME.
           MOVE BW-MNAME TO QBW-MNAME.
           MOVE BW-NICKNAME TO QBW-NICKNAME.
           MOVE BW-GENERATION TO QBW-GENERATION.
           MOVE BW-ADR1 TO QBW-ADR1.
           MOVE BW-ADR2 TO QBW-ADR2.
           MOVE BW-CITY TO QBW-CITY.
           MOVE BW-STATE TO QBW-STATE.
           MOVE BW-ZIP TO QBW-ZIP.

           MOVE BW-LTOUCH-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-LTOUCH-DATE.


           MOVE BW-REFDATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-REFDATE.


           MOVE BW-BDATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-BDATE.

           MOVE BW-PHONE TO QBW-PHONE.
           MOVE BW-CELL-PHONE TO QBW-CELL-PHONE.
           MOVE BW-NATIONALITY TO QBW-NATIONALITY.
           MOVE BW-GENDER TO QBW-GENDER.
           MOVE BW-DEPEND TO QBW-DEPEND.
           MOVE BW-RENTCD TO QBW-RENTCD.
           MOVE BW-TIMEADDR TO QBW-TIMEADDR.
           MOVE BW-CREDITCD TO QBW-CREDITCD.
           MOVE BW-STRTGCD TO QBW-STRTGCD.
           MOVE BW-DWELLCD TO QBW-DWELLCD.
           MOVE BW-COUNTY TO QBW-COUNTY.
           MOVE BW-ORIGIN TO QBW-ORIGIN.
           MOVE BW-CENSUSCD TO QBW-CENSUSCD.
           MOVE BW-ORGSLSMN TO QBW-ORGSLSMN.
           MOVE BW-REPCODE TO QBW-REPCODE.
           MOVE BW-SPSSNO TO QBW-SPSSNO.
           MOVE BW-SPLNAME TO QBW-SPLNAME.
           MOVE BW-SPFNAME TO QBW-SPFNAME.

           MOVE BW-SP-BDATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-SP-BDATE.

           MOVE BW-EMPLOYER TO QBW-EMPLOYER.
           MOVE BW-EMPLPOS TO QBW-EMPLPOS.
           MOVE BW-EMPPHONE TO QBW-EMPPHONE.
           MOVE BW-EMPEXT TO QBW-EMPEXT.
           MOVE BW-EMPYEARS TO QBW-EMPYEARS.
           MOVE BW-EMPPAY TO QBW-EMPPAY.
           MOVE BW-EMPPAYOT TO QBW-EMPPAYOT.
           MOVE BW-EMPPAYSR TO QBW-EMPPAYSR.
           MOVE BW-EMPADDR1 TO QBW-EMPADDR1.
           MOVE BW-EMPADDR2 TO QBW-EMPADDR2.
           MOVE BW-EMPCITY TO QBW-EMPCITY.
           MOVE BW-EMPSTATE TO QBW-EMPSTATE.
           MOVE BW-EMPZIP TO QBW-EMPZIP.
           MOVE BW-WORKHRS TO QBW-WORKHRS.
           MOVE BW-LOANS TO QBW-LOANS.
           MOVE BW-INTYTD TO QBW-INTYTD.
           MOVE BW-INTLYR TO QBW-INTLYR.
           MOVE BW-LIASMLN TO QBW-LIASMLN.
           MOVE BW-LIAOTH TO QBW-LIAOTH.

           MOVE BW-LASTPODATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-LASTPODATE.

           MOVE BW-PRLOANS TO QBW-PRLOANS.
           MOVE BW-RE-INTYTD TO QBW-RE-INTYTD.
           MOVE BW-RE-INTLYR TO QBW-RE-INTLYR.
           MOVE BW-CREDIT-LIMIT TO QBW-CREDIT-LIMIT.
           MOVE BW-CREDIT-SCORE TO QBW-CREDIT-SCORE.
           MOVE BW-CRED-SCORE(1) TO QBW-CRED-SCORE-1.
           MOVE BW-CRED-SCORE(2) TO QBW-CRED-SCORE-2.
           MOVE BW-CRED-SCORE(3) TO QBW-CRED-SCORE-3.
           MOVE BW-CRED-SCORE(4) TO QBW-CRED-SCORE-4.
           MOVE BW-BNKRPT-SCORE(1) TO QBW-BNKRPT-SCORE-1.
           MOVE BW-BNKRPT-SCORE(2) TO QBW-BNKRPT-SCORE-2.
           MOVE BW-BNKRPT-SCORE(3) TO QBW-BNKRPT-SCORE-3.
           MOVE BW-BNKRPT-SCORE(4) TO QBW-BNKRPT-SCORE-4.
           MOVE BW-LAST-FORM-NO TO QBW-LAST-FORM-NO.

           MOVE BW-LAST-FORM-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-LAST-FORM-DATE.


           MOVE BW-PRIOR-RN-FOR10-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-PRIOR-RN-FOR10-DATE.


           MOVE BW-LAST-RN-FOR10-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-LAST-RN-FOR10-DATE.


           MOVE BW-ADR-PHONE-CHG-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-ADR-PHONE-CHG-DATE.

           MOVE BW-BAD-ADR-FG TO QBW-BAD-ADR-FG.
           MOVE BW-BAD-PHONE-FG TO QBW-BAD-PHONE-FG.
           MOVE BW-BAD-EMPPHONE-FG TO QBW-BAD-EMPPHONE-FG.
           MOVE BW-BAD-MISC-PHONE1-FG TO QBW-BAD-MISC-PHONE1-FG.
           MOVE BW-BAD-MISC-PHONE2-FG TO QBW-BAD-MISC-PHONE2-FG.
           MOVE BW-BAD-CELL-FG TO QBW-BAD-CELL-FG.
           MOVE BW-MISC-PHONE(1) TO QBW-MISC-PHONE-1.
           MOVE BW-MISC-EXT(1) TO QBW-MISC-EXT-1.
           MOVE BW-MISC-PHONE-CD(1) TO QBW-MISC-PHONE-CD-1.
           MOVE BW-MISC-PHONE(2) TO QBW-MISC-PHONE-2.
           MOVE BW-MISC-EXT(2) TO QBW-MISC-EXT-2.
           MOVE BW-MISC-PHONE-CD(2) TO QBW-MISC-PHONE-CD-2.
           MOVE BW-MISC-PHONE(3) TO QBW-MISC-PHONE-3.
           MOVE BW-MISC-EXT(3) TO QBW-MISC-EXT-3.
           MOVE BW-MISC-PHONE-CD(3) TO QBW-MISC-PHONE-CD-3.
           MOVE BW-MISC-PHONE(4) TO QBW-MISC-PHONE-4.
           MOVE BW-MISC-EXT(4) TO QBW-MISC-EXT-4.
           MOVE BW-MISC-PHONE-CD(4) TO QBW-MISC-PHONE-CD-4.
           MOVE BW-MISC-PHONE(5) TO QBW-MISC-PHONE-5.
           MOVE BW-MISC-EXT(5) TO QBW-MISC-EXT-5.
           MOVE BW-MISC-PHONE-CD(5) TO QBW-MISC-PHONE-CD-5.
           MOVE BW-MISC-PHONE(6) TO QBW-MISC-PHONE-6.
           MOVE BW-MISC-EXT(6) TO QBW-MISC-EXT-6.
           MOVE BW-MISC-PHONE-CD(6) TO QBW-MISC-PHONE-CD-6.
           MOVE BW-CONTACT-DESC(1) TO QBW-CONTACT-DESC-1.
           MOVE BW-CONTACT-PHONE(1) TO QBW-CONTACT-PHONE-1.
           MOVE BW-CONTACT-EXT(1) TO QBW-CONTACT-EXT-1.
           MOVE BW-CONTACT-RELATION(1) TO QBW-CONTACT-RELATION-1.
           MOVE BW-CONTACT-DESC(2) TO QBW-CONTACT-DESC-2.
           MOVE BW-CONTACT-PHONE(2) TO QBW-CONTACT-PHONE-2.
           MOVE BW-CONTACT-EXT(2) TO QBW-CONTACT-EXT-2.
           MOVE BW-CONTACT-RELATION(2) TO QBW-CONTACT-RELATION-2.
           MOVE BW-CONTACT-DESC(3) TO QBW-CONTACT-DESC-3.
           MOVE BW-CONTACT-PHONE(3) TO QBW-CONTACT-PHONE-3.
           MOVE BW-CONTACT-EXT(3) TO QBW-CONTACT-EXT-3.
           MOVE BW-CONTACT-RELATION(3) TO QBW-CONTACT-RELATION-3.
           MOVE BW-CONTACT-DESC(4) TO QBW-CONTACT-DESC-4.
           MOVE BW-CONTACT-PHONE(4) TO QBW-CONTACT-PHONE-4.
           MOVE BW-CONTACT-EXT(4) TO QBW-CONTACT-EXT-4.
           MOVE BW-CONTACT-RELATION(4) TO QBW-CONTACT-RELATION-4.
           MOVE BW-CONTACT-DESC(5) TO QBW-CONTACT-DESC-5.
           MOVE BW-CONTACT-PHONE(5) TO QBW-CONTACT-PHONE-5.
           MOVE BW-CONTACT-EXT(5) TO QBW-CONTACT-EXT-5.
           MOVE BW-CONTACT-RELATION(5) TO QBW-CONTACT-RELATION-5.
           MOVE BW-CONTACT-DESC(6) TO QBW-CONTACT-DESC-6.
           MOVE BW-CONTACT-PHONE(6) TO QBW-CONTACT-PHONE-6.
           MOVE BW-CONTACT-EXT(6) TO QBW-CONTACT-EXT-6.
           MOVE BW-CONTACT-RELATION(6) TO QBW-CONTACT-RELATION-6.
           MOVE BW-CONTACT-DESC(7) TO QBW-CONTACT-DESC-7.
           MOVE BW-CONTACT-PHONE(7) TO QBW-CONTACT-PHONE-7.
           MOVE BW-CONTACT-EXT(7) TO QBW-CONTACT-EXT-7.
           MOVE BW-CONTACT-RELATION(7) TO QBW-CONTACT-RELATION-7.
           MOVE BW-CONTACT-DESC(8) TO QBW-CONTACT-DESC-8.
           MOVE BW-CONTACT-PHONE(8) TO QBW-CONTACT-PHONE-8.
           MOVE BW-CONTACT-EXT(8) TO QBW-CONTACT-EXT-8.
           MOVE BW-CONTACT-RELATION(8) TO QBW-CONTACT-RELATION-8.
           MOVE BW-CONTACT-DESC(9) TO QBW-CONTACT-DESC-9.
           MOVE BW-CONTACT-PHONE(9) TO QBW-CONTACT-PHONE-9.
           MOVE BW-CONTACT-EXT(9) TO QBW-CONTACT-EXT-9.
           MOVE BW-CONTACT-RELATION(9) TO QBW-CONTACT-RELATION-9.
           MOVE BW-CONTACT-DESC(10) TO QBW-CONTACT-DESC-10.
           MOVE BW-CONTACT-PHONE(10) TO QBW-CONTACT-PHONE-10.
           MOVE BW-CONTACT-EXT(10) TO QBW-CONTACT-EXT-10.
           MOVE BW-CONTACT-RELATION(10) TO QBW-CONTACT-RELATION-10.
           MOVE BW-SRCD TO QBW-SRCD.
           MOVE BW-CALL-ON-SUNDAY TO QBW-CALL-ON-SUNDAY.
           MOVE BW-SOLICIT TO QBW-SOLICIT.
           MOVE BW-CONFIDENTIAL TO QBW-CONFIDENTIAL.
           MOVE BW-PRIVACY-OPT-OUT TO QBW-PRIVACY-OPT-OUT.
           MOVE BW-OPT-IN-EMAIL TO QBW-OPT-IN-EMAIL.
           MOVE BW-OPT-IN-TEXT TO QBW-OPT-IN-TEXT.
           MOVE BW-CITY-TAX-CD TO QBW-CITY-TAX-CD.

           MOVE BW-DEALTRACK-CHG-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-DEALTRACK-CHG-DATE.

           MOVE BW-DECEASED TO QBW-DECEASED.

           MOVE BW-DECEASED-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QBW-DECEASED-DATE.

           MOVE BW-EMAIL TO QBW-EMAIL.
           MOVE BW-IDENT-NO TO QBW-IDENT-NO.
           MOVE BW-IDENT-TYPE TO QBW-IDENT-TYPE.
           MOVE BW-IDENT-STATE TO QBW-IDENT-STATE.
           MOVE BW-MILITARY-FLAG TO QBW-MILITARY-FLAG.
           MOVE BW-ORGBR TO QBW-ORGBR.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW1-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW1-WEND-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            INTO :QBW-REC
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *=================================================================
       LOAD-BW2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW2-FILE.
           PERFORM LOAD-BW2-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW2-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-LNAME, BW-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( BW2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW2-FILE.

           IF ( QBW2-WEND-OWNBR = ZEROES    ) OR
              ( QBW2-WEND-OWNBR NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW2-WBEG-OWNBR
                                           QBW2-WEND-OWNBR
              MOVE BW-LNAME             TO QBW2-WBEG-LNAME
              MOVE ALL "Z"              TO QBW2-WEND-LNAME
              CALL "C$TOLOWER" USING QBW2-WEND-LNAME, VALUE 35
              MOVE BW-FNAME             TO QBW2-WBEG-FNAME
              MOVE ALL "Z"              TO QBW2-WEND-FNAME
              CALL "C$TOLOWER" USING QBW2-WEND-FNAME, VALUE 35.

           EXEC SQL
            DECLARE BW2_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW2-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW2-WEND-OWNBR
              AND (BWFILE.BW_LNAME + BWFILE.BW_FNAME)
                >=
                  (:QBW2-WBEG-LNAME + :QBW2-WBEG-FNAME)
              AND (BWFILE.BW_LNAME + BWFILE.BW_FNAME)
                <=
                  (:QBW2-WEND-LNAME + :QBW2-WEND-FNAME)
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_LNAME,
                       BWFILE.BW_FNAME,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW2-WBEG-OWNBR 
                          QBW2-WEND-OWNBR.
           MOVE SPACES TO QBW2-WBEG-LNAME
                          QBW2-WBEG-FNAME
                          QBW2-WEND-LNAME
                          QBW2-WEND-FNAME.

           MOVE STAT---GOOD TO BW2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-LNAME, BW-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW2_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW2-FILE.

           IF ( BW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW2-CURSOR-STAT.

      *=================================================================
       LOAD-BW3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW3-FILE.
           PERFORM LOAD-BW3-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW3-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-SPSSNO
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( BW3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW3-FILE.

           IF ( QBW3-WEND-OWNBR = ZEROES    ) OR
              ( QBW3-WEND-OWNBR NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW3-WBEG-OWNBR
                                           QBW3-WEND-OWNBR
              MOVE BW-SPSSNO            TO QBW3-WBEG-SPSSNO
              MOVE ALL "9"              TO QBW3-WEND-SPSSNO.

           EXEC SQL
            DECLARE BW3_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW3-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW3-WEND-OWNBR
              AND BWFILE.BW_SPSSNO >= :QBW3-WBEG-SPSSNO
              AND BWFILE.BW_SPSSNO <= :QBW3-WEND-SPSSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SPSSNO, 
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW3-WBEG-OWNBR 
                          QBW3-WEND-OWNBR
                          QBW3-WBEG-SPSSNO
                          QBW3-WEND-SPSSNO.

           MOVE STAT---GOOD TO BW3-CURSOR-STAT.

      *-----------------------------------------------------------------
      *    STILL LOADING FILE WORKERS LIKE E-WORKERS
      *-----------------------------------------------------------------
       READ-BW3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           STRING BW-OWNBR, BW-SPSSNO
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW3_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW3-FILE.

           IF ( BW3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW3-CURSOR-STAT.

      *=================================================================
       LOAD-BW4-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW4-FILE.
           PERFORM LOAD-BW4-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW4-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( BW4-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW4-FILE.

           IF ( QBW4-WEND-OWNBR = ZEROES    ) OR
              ( QBW4-WEND-OWNBR NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW4-WBEG-OWNBR
                                           QBW4-WEND-OWNBR
              MOVE BW-SPLNAME           TO QBW4-WBEG-SPLNAME
              MOVE ALL "Z"              TO QBW4-WEND-SPLNAME
              CALL "C$TOLOWER" USING QBW4-WEND-SPLNAME, VALUE 35
              MOVE BW-SPFNAME           TO QBW4-WBEG-SPFNAME
              MOVE ALL "Z"              TO QBW4-WEND-SPFNAME
              CALL "C$TOLOWER" USING QBW4-WEND-SPFNAME, VALUE 35.

           EXEC SQL
            DECLARE BW4_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW4-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW4-WEND-OWNBR
              AND (BWFILE.BW_SPLNAME + BWFILE.BW_SPFNAME)
                >=
                  (:QBW4-WBEG-SPLNAME + :QBW4-WBEG-SPFNAME)
              AND (BWFILE.BW_SPLNAME + BWFILE.BW_SPFNAME)
                <=
                  (:QBW4-WEND-SPLNAME + :QBW4-WEND-SPFNAME)
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SPLNAME,
                       BWFILE.BW_SPFNAME,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW4_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW4-WBEG-OWNBR 
                          QBW4-WEND-OWNBR.
           MOVE SPACES TO QBW4-WBEG-SPLNAME
                          QBW4-WBEG-SPFNAME
                          QBW4-WEND-SPLNAME
                          QBW4-WEND-SPFNAME.

           MOVE STAT---GOOD TO BW4-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW4-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW4-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW4_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW4-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW4-FILE.

           IF ( BW4-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW4_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW4-CURSOR-STAT.


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
              DESTROY IOBWRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
