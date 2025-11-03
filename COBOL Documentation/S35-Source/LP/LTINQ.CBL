       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LTINQ.
       AUTHOR.          KEC.
       DATE-WRITTEN.    062398.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   GLOBAL VIN SCAN
      *                       INQUIRY MENU
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 062398 ***NEW***
      * KEC 120998 ADDED 'PROGRESS-INDICATOR'. PER CINDY!
      *  CS 110223 BUG WITH DISPLAY WHEN GOING TO 2ND PAGE OF DATA (MOVE
      *            KEY-TO-PAGE-KEY PARAGRAPH.  THERE WAS SOME CODE LEFT
      *            FROM THE APP-SCAN (WHICH IS WHERE THIS WAS TAKEN FROM)
      *            THAT WIPED OUT THE VIN# THAT WAS ENTERED & JUNK WAS
      *            DISPLAY IN ENDING BRANCH.... DATA WAS CORRECT PR#3720 MODERN
      * BAH 160114 A30 - TESTED WITH NEW LTCFILE AND NEW IXFILE!
      * KEC 2017.0221 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED PAGE-KEY-TABLE TO MATCH TO THE IX1-KEY;
      *         IX-BRNO WAS ADDED TO IX1-KEY.
      * BAH 170705 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 181205 LTCFILE
      *  CS 181211 IXFILE
      *  CS 181211 WAS MOVING EXT-FILPATH-BRANCH TO GET THE LTC IN MOVE
      *            FIELDS, CHANGED TO MOVE WK-BRANCH TO LTC-BRNO. 
      * BAH 20190919 REMOVED ACCESS-CALL ON IXFILE
      * JKC 2024-0307 REVISED TO REMOVE EDITOR COPY MEMBER AND HARDCODE.
      * BAH 2024.0815 ELIMINATE DISPLAY OF SSNO IN MORE INFO #1673 S35Q-118
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WORK FILE TO SORT OUT THE BRANCHS' (GROUP) FILES FOR 'IX'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT WK-FILE ASSIGN TO WK-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY WK-KEY
                  FILE STATUS FILE-STAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *
      *    W O R K - F I L E
      *
      *-----------------------------------------------------------------
      *    WORK FILE TO LOAD ALL BRANCHS WITHIN A GROUP SO TO BETTER
      *     ABLE TO GO UP AND DOWN THE SCAN WITHOUT USING GROUP-LOGIC.
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.

      *-----------------------------------------------------------------
       01  WK-REC.
           03  WK-BRANCH           PIC 9(4).
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/LTINQ.CBL S35 03/07/24-14:47:36 >".

      ******************************************************************
      *
      *    F I L E - P A T H - W O R K E R S
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
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
      *=================================================================
       01  WK-KEY                  PIC 9(4)         VALUE ZEROES.
       01  WK-IFN                  PIC X(2)         VALUE "WK".
       01  WK-PATH                 PIC X(35)        VALUE SPACES.
      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                        PIC X VALUE " ".


       01  CURRENT-BRANCH               PIC 9(04)     VALUE ZEROES.
           88  CURRENT-BRANCH-EOF                     VALUE ZEROES.

       01  IX-FILE-STATUS              PIC X(01)     VALUE SPACES.
           88  IX-FILE-CLOSE                         VALUE "C".
           88  IX-FILE-OPEN                          VALUE "O".

       01  ONE                          PIC S9(1)     VALUE ZEROES.
      *------------------------------------------------------------
       01  HEADER-VIN-1.
           03 FILLER                   PIC X(40)
              VALUE " BR. -----------VEHICLE INFO----------  ".
           03 FILLER                   PIC X(40)
              VALUE "   -------------LOAN INFO---------------".

       01  HEADER-VIN-2.
           03 FILLER                   PIC X(40)
              VALUE " NUM YEAR  MAKE    MODEL      TITLE     ".
           03 FILLER                   PIC X(40)
              VALUE "   NAME       ACCT #    BALANCE STATUS  ".

      *---------------------------------------------------------------
       01  LEGEND-VIN-ID.
           03 FILLER                    PIC X(30)
              VALUE "F1-RESET, F7-EXIT, ENTER VIN N".
           03 FILLER                    PIC X(5)
              VALUE "UMBER".

       01  LEGEND-VIN-ID-6.
           03 FILLER                     PIC X(30)
              VALUE "F1-RESET, F7-EXIT, ENTER LAST ".
           03 FILLER                     PIC X(30)
              VALUE "6 OF VIN #                    ".

       01  LEGEND-BEGIN-BRANCH.
           03 FILLER                    PIC X(30)
              VALUE "F1-RESET, F3-ALL, F6-SCAN GROU".
           03 FILLER                    PIC X(30)
              VALUE "P, F7-EXIT, ENTER BEGINNING BR".
           03 FILLER                    PIC X(11)
              VALUE "ANCH NUMBER".

       01  LEGEND-ENDING-BRANCH.
           03 FILLER                    PIC X(30)
              VALUE "F1-RESET, F3-ALL, F6-SCAN GROU".
           03 FILLER                    PIC X(30)
              VALUE "P, F7-EXIT, ENTER ENDING BRANC".
           03 FILLER                    PIC X(8)
              VALUE "H NUMBER".

       01  LEGEND-SELECTION-1.
           03 FILLER                    PIC X(30)
              VALUE "F1-RESET, F6-MORE INFO., F7-EX".
           03 FILLER                    PIC X(30)
              VALUE "IT, UP OR DOWN KEY            ".

       01  LEGEND-MORE-INFO.
           03 FILLER                   PIC X(30)
              VALUE "PRESS 'ENTER' TO CONTINUE ....".
           03 FILLER                   PIC X(30)
              VALUE "..............................".

      *-----------------------------------------------------------------
      *    PRIMARY-KEY-TABLE:  USED TO GET PRIMARY-KEY FROM FLDSEL.
      *-----------------------------------------------------------------
       01  PRIMARY-KEY-TABLE.
           03  PRIMARY-KEY-IX1-KEY         OCCURS 11.
               05  PRIMARY-KEY-IX-BRNO     PIC 9(4).
               05  PRIMARY-KEY-IX-CODE     PIC X(1).
               05  PRIMARY-KEY-IX-ACCTNO   PIC 9(8).
               05  PRIMARY-KEY-IX-SEQNO    PIC 9(3).

      *-----------------------------------------------------------------
       01  PROGRAM-CONTROL              PIC X(01)     VALUE SPACES.
           88  CONTROL-CLEAR                          VALUE "C".
           88  CONTROL-CONTINUE                       VALUE " ".
           88  CONTROL-END                            VALUE "N".
           88  CONTROL-ERROR                          VALUE "R".
           88  CONTROL-EXIT                           VALUE "X".
      *-----------------------------------------------------------------
      *    SELECTION-IX1-KEY:  THE PRIMARY-KEY SELECTED FROM FLDSEL
      *-----------------------------------------------------------------
       01  SELECTION-IX1-KEY           VALUE ZEROES.
           03  SELECTION-IX-BRNO       PIC 9(04).
           03  SELECTION-IX-CODE       PIC X(01).
           03  SELECTION-IX-ACCTNO     PIC 9(08).
           03  SELECTION-IX-SEQNO      PIC 9(03).

       01  SUB                          PIC 9(3)      VALUE ZEROES.

       01  FLDSEL-STAT                   PIC X(2).
           88  FLDSEL-F1-KEY                         VALUE "10".
           88  FLDSEL-F2-KEY                         VALUE "12".
           88  FLDSEL-F3-KEY                         VALUE "16".
           88  FLDSEL-F4-KEY                         VALUE "18".
           88  FLDSEL-F5-KEY                         VALUE "1:".
           88  FLDSEL-F6-KEY                         VALUE "1<".
           88  FLDSEL-F7-KEY                         VALUE "1>".
           88  FLDSEL-F8-KEY                         VALUE "1 ".
           88  FLDSEL-F9-KEY                         VALUE "1(".
           88  FLDSEL-F10-KEY                        VALUE "1)".
           88  FLDSEL-F11-KEY                        VALUE "1!".
           88  FLDSEL-F12-KEY                        VALUE "1#".
           88  FLDSEL-ENTER-KEY                      VALUE "00".
           88  FLDSEL-HOME-KEY                       VALUE "1H".
           88  FLDSEL-UP-KEY                         VALUE "1U".
           88  FLDSEL-DOWN-KEY                       VALUE "1D".
           88  FLDSEL-PAGE-UP-KEY                    VALUE "1P".
           88  FLDSEL-PAGE-DOWN-KEY                  VALUE "1N".

       01  FLDSEL-FIELD            PIC X(4)    VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER              PIC X(44)   VALUE
             "010 020 030 040 050 060 070 080 090 100 110.".

       01  FLDSEL-BUF.
           03  FLDSEL-BRANCH       PIC ZZZ9.
           03  FILLER              PIC X(1).
           03  FLDSEL-YEAR         PIC X(4).
           03  FILLER              PIC X(2).
           03  FLDSEL-MAKE         PIC X(6).
           03  FILLER              PIC X(2).
           03  FLDSEL-MODEL        PIC X(10).
           03  FILLER              PIC X(1).
           03  FLDSEL-TITLE        PIC X(8).
           03  FILLER              PIC X(5).
           03  FLDSEL-LASTNAME     PIC X(10).
           03  FILLER              PIC X(1).
           03  FLDSEL-ACCTNO       PIC 9(6).
           03  FILLER              PIC X(2).
           03  FLDSEL-BALANCE      PIC ZZ,ZZ9.99.
           03  FILLER              PIC X.
           03  FLDSEL-STATUS       PIC X(4).
           03  FILLER              PIC X(1).
           03  FLDSEL-TRANSFERRED  PIC X(3).


       01  FIRST-FIELD                   PIC X(4)    VALUE "010.".
       01  LAST-FIELD                    PIC X(4)    VALUE "110.".

      *=================================================================
      *
      *    P A G E - K E Y - T A B L E
      *
      *=================================================================
       01  PAGE-KEY-TABLE.
           03  PAGE-KEY-IX1-KEY        OCCURS 600.
               05  PAGE-KEY-BRNO       PIC 9(04).
               05  PAGE-KEY-CODE       PIC X(01).
               05  PAGE-KEY-ACCTNO     PIC 9(08).
               05  PAGE-KEY-SEQNO      PIC 9(03).

      *-----------------------------------------------------------------
       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 9(4)         VALUE 600.

      *=================================================================
      *
      *    E D I T O R - T E L E P H O N E
      *
      *------------[ INPUT ]----( AAA1112222 )-----[ OUTPUT ]-----------
      *  EDITOR-INPUT-PHONE            |  EDITOR-OUTPUT-PHONE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  EDITOR-PHONE
      *  EDITOR-PHONE-SHORT
      *
      *------------[ INPUT ]--( (AAA)-111-2222 )---[ OUTPUT ]-----------
      *  EDITOR-INPUT-PHONE-EDIT       |  EDITOR-OUTPUT-PHONE-EDIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  EDITOR-PHONE-EDIT
      *  EDITOR-PHONE-SHORT-EDIT
      *
      *=================================================================

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FORMAT: AAA1112222
      *    NAME  : EDITOR-PHONE
      *            EDITOR-PHONE-SHORT
      *            EDITOR-INPUT-PHONE
      *            EDITOR-OUTPUT-PHONE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  EDITOR-PHONE                                       PIC 9(10).
       01  EDITOR-INPUT-PHONE          REDEFINES EDITOR-PHONE PIC 9(10).
       01  EDITOR-OUTPUT-PHONE         REDEFINES EDITOR-PHONE PIC 9(10).
       01  EDITOR-PHONE-RDF            REDEFINES EDITOR-PHONE.
           03  EDITOR-PHONE-AREA                              PIC 9(3).
           03  EDITOR-PHONE-AREA-CODE  REDEFINES EDITOR-PHONE-AREA
                                                              PIC 9(3).
           03  EDITOR-PHONE-SHORT                             PIC 9(7).
           03  EDITOR-PHONE-STANDARD   REDEFINES EDITOR-PHONE-SHORT.
               05  EDITOR-PHONE-3                             PIC 9(3).
               05  EDITOR-PHONE-4                             PIC 9(4).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FORMAT: (AAA)-111-2222
      *                  111-2222
      *    NAME  : EDITOR-PHONE-EDIT
      *            EDITOR-PHONE-SHORT-EDIT
      *            EDITOR-INPUT-PHONE-EDIT
      *            EDITOR-OUTPUT-PHONE-EDIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  EDITOR-PHONE-EDIT                                  PIC X(14).
       01  EDITOR-INPUT-PHONE-EDIT     REDEFINES EDITOR-PHONE-EDIT
                                                              PIC X(14).
       01  EDITOR-OUTPUT-PHONE-EDIT    REDEFINES EDITOR-PHONE-EDIT
                                                              PIC X(14).
       01  EDITOR-PHONE-EDIT-RDF       REDEFINES EDITOR-PHONE-EDIT.
           03  EDITOR-PHONE-EDIT-P1                           PIC X(1).
           03  EDITOR-PHONE-EDIT-AREA                         PIC 9(3).
           03  EDITOR-PHONE-EDIT-P2                           PIC X(1).
           03  EDITOR-PHONE-EDIT-D1                           PIC X(1).
           03  EDITOR-PHONE-SHORT-EDIT                        PIC X(8).
           03  EDITOR-PHONE-SHT-EDITX REDEFINES EDITOR-PHONE-SHORT-EDIT.
               05  EDITOR-PHONE-EDIT-3                        PIC 9(3).
               05  EDITOR-PHONE-EDIT-D2                       PIC X(1).
               05  EDITOR-PHONE-EDIT-4                        PIC 9(4).

      *=================================================================
      *
      *    P R O G R E S S - I N D I C A T O R ----- (PGID)
      *
      *=================================================================
       01  PROGRESS-INDICATOR.
           03  PGID-PARENTHESIS-LEFT  PIC X(01)   VALUE "(".
           03  PGID-INDICATOR         PIC X(01)   VALUE "-".
           03  PGID-PARENTHESIS-RIGHT PIC X(01)   VALUE ")".
           03  PGID-BRANCH-PROMPT     PIC X(09)   VALUE " BRANCH: ".
           03  PGID-BRANCH            PIC 9(04)   VALUE ZEROES.
           03  PGID-HYPHEN            PIC X(05)   VALUE " --- ".
           03  PGID-TEXT              PIC X(59)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  PROGRESS-INDICATOR-LIST    PIC X(57)   VALUE
            "ENDMORE DASHLINE1 LEGEND DASHLINE2 PAGE DASHLINE3 PAGENO.".

      *-----------------------------------------------------------------
       01  PROGRESS-INDICATOR-SLEEP   PIC 9(04)   VALUE ZEROES.
           88  PROGRESS-INDICATOR-MAX             VALUE 250 THRU 9999.

      *-----------------------------------------------------------------
       01  PROGRESS-INDICATOR-LEVEL   PIC 9(01)    VALUE ZEROES.

      *-----------------------------------------------------------------
       01  PROGRESS-INDICATOR-TEXT-TABLE.
           03  FILLER PIC X(30) VALUE "VERIFYING BRANCHS IN RANGE....".
           03  FILLER PIC X(29) VALUE ".............................".

           03  FILLER PIC X(30) VALUE "SEARCHING FOR COLLATERAL 'VIN'".
           03  FILLER PIC X(29) VALUE " RECORDS.....................".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PROGRESS-INDICATOR-TEXT-TBL    REDEFINES
           PROGRESS-INDICATOR-TEXT-TABLE.
           03  PROGRESS-INDICATOR-TEXT-ELE   PIC X(59)   OCCURS 2 TIMES.

      *=================================================================
      *
      *    R A N G E S
      *
      *=================================================================
       01  RANGE-BUFFER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  RANGE-BUF.

               05  BEG-BRANCH            PIC 9(4).
               05  END-BRANCH            PIC 9(4).
               05  VIN-IDENTIFICATION    PIC X(20).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  RANGE-MISC-BUF.
               05  ENT-GROUP             PIC X(4).
               05  GROUP-FLAG            PIC X(1).

               05  RANGE-ENTRY-BEG-END   PIC X(3).
                   88  RANGE-ENTRY-BEGINNING         VALUE "BEG".
                   88  RANGE-ENTRY-ENDING            VALUE "END".

      *-----------------------------------------------------------------
       01  RANGE-LIST.
           03  FILLER PIC X(12) VALUE "PRIMARY-KEY ".
           03  FILLER PIC X(22) VALUE "BEG-BRANCH END-BRANCH.".

      *=================================================================
      *
      *    D I S P L A Y - B U F F E R
      *
      *=================================================================
       01  HEADER-BUF.
           05  HEADER-LINE-1           PIC X(80).
           05  HEADER-LINE-2           PIC X(80).
           05  HEADER-LINE-3           PIC X(80).
           05  LEGEND                  REDEFINES HEADER-LINE-3
                                       PIC X(80).

       01  DISPLAY-BUF.
           03  D-LINE                 OCCURS 11.
               05  S1-BRANCH           PIC ZZZ9.
               05  FILLER              PIC X(01).
               05  S1-YEAR             PIC X(4).
               05  FILLER              PIC X(2).
               05  S1-MAKE             PIC X(6).
               05  FILLER              PIC X(02).
               05  S1-MODEL            PIC X(10).
               05  FILLER              PIC X(01).
               05  S1-TITLE            PIC X(8).
               05  FILLER              PIC X(5).
               05  S1-LASTNAME         PIC X(10).
               05  FILLER              PIC X(01).
               05  S1-ACCTNO           PIC 9(6).
               05  FILLER              PIC X(02).
               05  S1-BALANCE          PIC ZZ,ZZ9.99.
               05  FILLER              PIC X.
               05  S1-STATUS           PIC X(4).
               05  FILLER              PIC X(1).
               05  S1-TRANSFERRED      PIC X(3).
           03  D-END-MORE              PIC X(7).
           03  D-DASH-1                PIC XX.
           03  D-DASH-2                PIC XX.
           03  D-PAGE                  PIC X(5).
           03  D-DASH-3                PIC X.
           03  D-PAGENO                PIC ZZ9.

       01  DISPLAY-LIST.
           03  FILLER                           PIC X(44)   VALUE
              "010 020 030 040 050 060 070 080 090 100 110 ".
           03  FILLER                           PIC X(53)   VALUE
              "ENDMORE:RF DASHLINE1 DASHLINE2 PAGE DASHLINE3 PAGENO.".

      *-----------------------------------------------------------------
      *
      *    M O R E - I N F O - B U F F E R
      *
      *-----------------------------------------------------------------
       01  MORE-INFO-LIST.
           05  FILLER                           PIC X(27)   VALUE
               "HEAD-010 HEAD-020 HEAD-030 ".
           03  FILLER                           PIC X(52)   VALUE
              "010 020 030 040 050 060 070 080 090 100 110 ENDMORE ".
           03  FILLER                           PIC X(49)   VALUE
              "DASHLINE1 LEGEND DASHLINE2 PAGE DASHLINE3 PAGENO.".

       01  MORE-INFO-BUF.
      *HEAD-010
           03  MI-LINE-01                 PIC X(80).
           03  MI-LINE-02.
               05 M02-VIN-PMT             PIC X(5).
               05 FILLER                  PIC X.
               05 M02-VIN                 PIC X(20).
               05 M02-DESCRIPTION-PMT     PIC X(6).
               05 FILLER                  PIC X(1).
               05 M02-DESCRIPTION         PIC X(30).
               05 FILLER                  PIC X.
               05 M02-TITLE-PMT           PIC X(7).
               05 FILLER                  PIC X.
               05 M02-TITLE-NO            PIC X(8).
           03  MI-LINE-03                 PIC X(80).
           03  MI-LINE-04.
               05  M04-BRANCH-PMT         PIC X(08).
               05  FILLER                 PIC X(01).
               05  M04-BRANCH             PIC 9(04).
               05  FILLER                 PIC X(02).
               05  M04-BR-PHONE-PMT       PIC X(6).
               05  FILLER                 PIC X.
               05  M04-BR-PHONE           PIC X(14).
               05  FILLER                 PIC X(44).
           03  MI-LINE-05                 PIC X(80).
           03  MI-LINE-06.
               05  FILLER                 PIC X(45).
               05  M06-ACCTNO-PMT         PIC X(10).
               05  FILLER                 PIC X.
               05  M06-ACCTNO             PIC 9(6).
               05  FILLER                 PIC X(18).
           03  MI-LINE--07                PIC X(80).
           03  MI-LINE-08.
               05  M08-NAME-PMT           PIC X(8).
               05  FILLER                 PIC X.
               05  M08-NAME               PIC X(30).
               05  FILLER                 PIC X(6).
               05  M08-LOANDATE-PMT       PIC X(10).
               05  FILLER                 PIC X.
               05  M08-LOANDATE           PIC 99/99/99.
               05  FILLER                 PIC X(16).
           03  MI-LINE-09                 PIC X(80).
           03  MI-LINE-10.
               05  M10-ADDRESS-PMT        PIC X(8).
               05  FILLER                 PIC X.
               05  M10-STREET-ADD         PIC X(35).
               05  FILLER                 PIC X.
               05  M10-PAID-BALANCE-PMT   PIC X(10).
               05  FILLER                 PIC X.
               05  M10-PAID-DATE          PIC 99/99/99.
               05  M10-BAL REDEFINES M10-PAID-DATE.
                   07  M10-BALANCE        PIC ZZZZ9.99.
               05  FILLER                 PIC X(16).
           03  MI-LINE-11.
               05  FILLER                 PIC X(45).
               05  M14-REPODATE-PMT       PIC X(10).
               05  FILLER                 PIC X.
               05  M14-REPODATE           PIC 99/99/99 BLANK ZERO.
               05  FILLER                 PIC XX.
               05  M14-SOLD-PMT           PIC X(5).
               05  FILLER                 PIC X.
               05  M14-SOLDDATE           PIC 99/99/99 BLANK ZERO.
           03  MI-LINE-12.
               05  FILLER                 PIC X(9).
               05  M12-CITY-ST-ZIP        PIC X(27).
               05  FILLER                 PIC X(09).
               05  M12-PLDATE-PMT         PIC X(10).
               05  FILLER                 PIC X.
               05  M12-PLDATE             PIC 99/99/99.
               05  FILLER                 PIC X(16).
           03  MI-LINE-13                 PIC X(80).
           03  MI-LINE-14                 PIC X(80).
           03  LEGEND-MORE-INFO-BUFFER.
               05  FILLER                   PIC X(09).
               05  LEGEND-MORE-INFO-BUF     PIC X(60).
               05  FILLER                   PIC X(11).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  HEADER-LIST.
           05  FILLER                           PIC X(18)   VALUE
               "HEAD-010 HEAD-020 ".

           05  LEGEND-LIST.
               07  FILLER                       PIC X(08)   VALUE
                   "HEAD-030".
               07  LEGEND-LIST-ATTRIBUTE        PIC X(03).

           05  HEADER-LIST-PERIOD               PIC X(01)   VALUE
               " ".

       01  BOTTOM-LIST        PIC X(7) VALUE "LEGEND.".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       01  LTINQ-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LTINQ_DEF.CPY".
       COPY "LIBLP/LTINQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LTINQ_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IX2-FILE.
           PERFORM CLOSE-IX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           PERFORM INITIALIZATION.

       MAIN-MODULE.

           PERFORM ENTER-PRIMARY-KEY.
           IF ( CONTROL-CLEAR )
               GO TO MAIN-MODULE.
           IF ( CONTROL-EXIT  )
               GO TO EXIT-MAIN-PROGRAM.

           PERFORM PROCESS-PRIMARY-KEY.
           IF ( CONTROL-EXIT   )
               GO TO EXIT-MAIN-PROGRAM.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LP/LTINQ"    TO FORM-NAM.
           MOVE FORM-PATH     TO FORM-PATHNAME.

           MOVE "LTINQ" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS LTINQ-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

           MOVE "S  A300TITLE:R."                TO VDU.
           MOVE " GLOBAL VIN INQUIRY SCAN      " TO VDUBUF.
           PERFORM SCREENIO.


       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
       ENTER-PRIMARY-KEY SECTION.
      ******************************************************************
       CLEAR-ENTER-PRIMARY-KEY.

           MOVE SPACES               TO PROGRAM-CONTROL.

           MOVE SPACES     TO WR-BUF.
           MOVE RANGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES       TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES      TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-VIN-ID.
           IF BR-COLL-ID-SCAN = "S"
              MOVE LEGEND-VIN-ID-6 TO LEGEND
           ELSE
              MOVE LEGEND-VIN-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A200PRIMARY-KEY." TO VDU.
           PERFORM SCREENIO.
           IF ( F1-KEY ) GO TO CLEAR-ENTER-PRIMARY-KEY.
           IF ( F3-KEY ) MOVE ZEROES TO VDUNUM0
                         MOVE "00"   TO VDUSTATUS.
           IF ( F7-KEY ) GO TO EXIT-PROGRAM-ENTER-PRIMARY.
           IF NOT (ENTER-KEY) GO TO ENTER-VIN-ID.

           MOVE VDUBUF TO VIN-IDENTIFICATION.

           IF VDUBUF = " "
              GO TO ENTER-VIN-ID.

       ENTER-BEG-BRANCH.
           MOVE "BEG" TO RANGE-ENTRY-BEG-END.

           MOVE LEGEND-BEGIN-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF ( F1-KEY ) GO TO CLEAR-ENTER-PRIMARY-KEY.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF ( F7-KEY ) GO TO EXIT-PROGRAM-ENTER-PRIMARY.
           IF ( F6-KEY )
              PERFORM SCAN-GR-FILE
              IF ( IO-FG NOT = ZEROES )
                 GO TO ENTER-BEG-BRANCH.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
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
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N"                  TO GROUP-FLAG.
           MOVE WGR-SRESULT          TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-END-BRANCH.

           MOVE "END" TO RANGE-ENTRY-BEG-END.

           MOVE LEGEND-ENDING-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF ( F1-KEY ) GO TO CLEAR-ENTER-PRIMARY-KEY.
           IF ( F7-KEY ) GO TO EXIT-PROGRAM-ENTER-PRIMARY.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF (  BEG-BRANCH > END-BRANCH ) OR
              (( BEG-BRANCH = ZEROES     ) AND
               ( END-BRANCH = ZEROES     ))
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.


           GO TO CHECK-BRANCH-SECURITY.

       ENTER-ALL-BRANCH.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       CHECK-BRANCH-SECURITY.

           GO TO EXIT-ENTER-PRIMARY-KEY.

       EXIT-PROGRAM-ENTER-PRIMARY.
           MOVE "X" TO PROGRAM-CONTROL.

       EXIT-ENTER-PRIMARY-KEY.
           EXIT.

      ******************************************************************
       PROCESS-PRIMARY-KEY SECTION.
      ******************************************************************

           PERFORM CREATE-WORK-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-WK-FILE.

      * CLEAR PAGE-KEY-TABLE 
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 600
                  MOVE SPACES TO PAGE-KEY-CODE  (SUB)
                  MOVE ZEROES TO PAGE-KEY-BRNO  (SUB)
                                 PAGE-KEY-ACCTNO(SUB)
                                 PAGE-KEY-SEQNO (SUB)
           END-PERFORM.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           MOVE "C"         TO IX-FILE-STATUS.

       START-BUILD-PAGENO.

           ADD ONE TO CURRENT-PAGE.

           IF ( CURRENT-PAGE < 1          ) OR
              ( CURRENT-PAGE > PAGE-COUNT )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE ZEROES TO PRIMARY-KEY-TABLE.
           MOVE 1      TO SUB.

      * SETTING UP FOR BRANCH PATH INFO FOR 'LOCAL' FILES.

           IF ( CURRENT-PAGE = 1 )
              MOVE 0                           TO CURRENT-BRANCH
           ELSE
           IF ( PAGE-KEY-BRNO(CURRENT-PAGE) NOT = 0 )
              MOVE PAGE-KEY-BRNO(CURRENT-PAGE) TO CURRENT-BRANCH.

           MOVE CURRENT-BRANCH TO WK-KEY.
           PERFORM START-WK-FILE.

           PERFORM SETUP-NEXT-BRANCH-NUMBER.

           IF ( CURRENT-BRANCH-EOF )
              IF ( CURRENT-PAGE = 1 )
                 GO TO END-PROCESS-PRIMARY-KEY
              ELSE
                 MOVE -1 TO ONE
                 GO TO START-BUILD-PAGENO.

       STARTING-FILE-FOR-PAGE.
           MOVE WK-BRANCH          TO IX-BRNO.
           MOVE VIN-IDENTIFICATION TO IX-VIN.

           PERFORM START-IX2-FILE.

       BUILD-PAGENO.

           PERFORM READ-IX2-FILE-NEXT.
           IF ( IO-FG = ZEROES       ) AND
              ( SUB = 1              ) AND
              ( IX-BRNO = IX-PATH-OWNBR) AND
              ( CURRENT-PAGE NOT = 1 )
              PERFORM READ-IX2-FILE-NEXT UNTIL
                 (
                   (IX-BRNO   = PAGE-KEY-BRNO  (CURRENT-PAGE)) AND
                   (IX-CODE   = PAGE-KEY-CODE  (CURRENT-PAGE)) AND
                   (IX-ACCTNO = PAGE-KEY-ACCTNO(CURRENT-PAGE)) AND
                   (IX-SEQNO  = PAGE-KEY-SEQNO (CURRENT-PAGE))
                  ) OR ( IO-FG = 9 ) OR (IX-BRNO NOT = IX-PATH-OWNBR).

      * SETTING UP FOR BRANCH PATH INFO FOR 'LOCAL' FILES.

           IF ( IO-FG NOT = 0 ) OR (IX-BRNO NOT = IX-PATH-OWNBR)
              PERFORM SETUP-NEXT-BRANCH-NUMBER
              IF ( CURRENT-BRANCH-EOF )
                 GO TO DISPLAY-PAGE
              ELSE
                 PERFORM START-NEW-BRANCH-IX-FILE
                 GO TO BUILD-PAGENO.

           MOVE 2 TO PROGRESS-INDICATOR-LEVEL.
           PERFORM SEND-PROGRESS-INDICATOR.

       CHECK-IF-WITHIN-RANGE.

           IF ( IX-VIN NOT = VIN-IDENTIFICATION )
              PERFORM SETUP-NEXT-BRANCH-NUMBER
              IF ( CURRENT-BRANCH-EOF )
                 GO TO DISPLAY-PAGE
              ELSE
                 PERFORM START-NEW-BRANCH-IX-FILE
                 GO TO BUILD-PAGENO.

           IF ( SUB > 11 )
              GO TO DISPLAY-PAGE.

       MOVE-FIELDS.
           IF NOT (IX-CODE = "C")
              GO TO BUILD-PAGENO.

            MOVE LTC-PATH-OWNBR     TO LTC-BRNO.
            MOVE IX-ACCTNO          TO LTC-ACCTNO LN-ACCTNO.
            MOVE 1                  TO LTC-SEQNO.
            PERFORM READ-LTC1-FILE.
            IF IO-FG NOT = ZERO
               GO TO BUILD-PAGENO.

            MOVE CURRENT-BRANCH TO S1-BRANCH(SUB).
      * IX-SEQNO = SUBSCRIPT OF THE LT VIN
            MOVE LTC-YEAR(IX-SEQNO)     TO S1-YEAR(SUB).
            MOVE LTC-MAKE(IX-SEQNO)     TO S1-MAKE(SUB).
            MOVE LTC-MODEL(IX-SEQNO)    TO S1-MODEL(SUB).
            MOVE LTC-TITLE-NO(IX-SEQNO) TO S1-TITLE(SUB).

            PERFORM READ-LN1-FILE.
            IF IO-FG NOT = ZERO
               GO TO BUILD-PAGENO.

            MOVE LN-ACCTNO TO S1-ACCTNO(SUB).
            MOVE LN-CURBAL TO S1-BALANCE(SUB).

            IF LN-PLCD = "P"
               MOVE "P&L" TO S1-STATUS(SUB)
            ELSE
               IF LN-IS-REPO-AR
                   MOVE "REPO" TO S1-STATUS(SUB)
               ELSE
                   MOVE SPACES TO S1-STATUS(SUB).

            IF LN-POCD = "BT"
               MOVE "TFR" TO S1-TRANSFERRED(SUB).

              MOVE LN-SSNO(1) TO BW-SSNO.
              PERFORM READ-BW1-FILE.
              IF IO-FG NOT = ZERO
                 GO TO BUILD-PAGENO.

              MOVE BW-LNAME TO S1-LASTNAME(SUB).

      * MOVE TO PRIMARY KEY TABLE

           MOVE CURRENT-BRANCH TO PRIMARY-KEY-IX-BRNO   (SUB).
           MOVE IX-CODE        TO PRIMARY-KEY-IX-CODE   (SUB).
           MOVE IX-ACCTNO      TO PRIMARY-KEY-IX-ACCTNO (SUB).
           MOVE IX-SEQNO       TO PRIMARY-KEY-IX-SEQNO  (SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

           IF ( IO-FG NOT = ZEROES ) AND ( SUB = 1 )
              MOVE "NO RECORDS FOUND!" TO MESS
              PERFORM SEND-MESS
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO END-PROCESS-PRIMARY-KEY.

           MOVE "MORE..." TO D-END-MORE.

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( IO-FG = ZEROES )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    PERFORM MOVE-KEY-TO-PAGE-KEY
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO LEGEND
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE
                 MOVE "--" TO D-DASH-1.

           PERFORM SETUP-HEADER.

           MOVE SPACES TO D-DASH-1 D-DASH-2 D-DASH-3.
           MOVE "PAGE:" TO D-PAGE.
           MOVE CURRENT-PAGE       TO D-PAGENO.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LEGEND-SELECTION-1 TO WR-BUF.
           MOVE BOTTOM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.

           PERFORM CALL-FLDSEL.

           PERFORM GET-SELECTION-IX1-KEY.

           IF ( FLDSEL-F1-KEY )
              MOVE "C" TO PROGRAM-CONTROL
              GO TO END-PROCESS-PRIMARY-KEY.

           IF ( FLDSEL-F6-KEY )
              PERFORM DISPLAY-MORE-INFO
              MOVE ZEROES TO ONE
              GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-F7-KEY )
              MOVE "X" TO PROGRAM-CONTROL
              GO TO END-PROCESS-PRIMARY-KEY.

           IF ( FLDSEL-UP-KEY )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1         TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-PAGE-UP-KEY )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1          TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-DOWN-KEY OR FLDSEL-PAGE-DOWN-KEY )
              IF ( CURRENT-PAGE NOT < PAGE-COUNT )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1           TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-HOME-KEY )
              MOVE ZEROES      TO CURRENT-PAGE
              MOVE 1           TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF NOT ( FLDSEL-ENTER-KEY )
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       END-PROCESS-PRIMARY-KEY.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-WK-FILE.

       EXIT-PROCESS-PRIMARY-KEY.
           EXIT.

      ******************************************************************
       CREATE-WORK-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-WK-FILE-OUTPUT.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO END-CREATE-WORK-FILE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES ) OR ( BR-NO > END-BRANCH )
              GO TO END-CREATE-WORK-FILE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE 1    TO PROGRESS-INDICATOR-LEVEL.
           MOVE 9999 TO PROGRESS-INDICATOR-SLEEP.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO IX-PATH-OVERRIDE.

      *-----------------------------------------------------------------
       UPDATE-WK-FILE.

           MOVE BR-NO TO WK-BRANCH WK-KEY.
           PERFORM WRITE-WK-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

       END-OF-GROUP.

           GO TO NEXT-GROUP-LOC.

       END-CREATE-WORK-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-WK-FILE.

       EXIT-CREATE-WORK-FILE.
           EXIT.

      ******************************************************************
       SETUP-NEXT-BRANCH-NUMBER SECTION.
      ******************************************************************

           IF ( IX-FILE-OPEN )
              PERFORM CLOSE-LOCAL-FILES.

           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              MOVE 0 TO CURRENT-BRANCH
              GO TO EXIT-SETUP-NEXT-BRANCH.

           MOVE WK-BRANCH TO BR-NO CURRENT-BRANCH.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           MOVE 2    TO PROGRESS-INDICATOR-LEVEL.
           MOVE 9999 TO PROGRESS-INDICATOR-SLEEP.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              IX-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE.

           PERFORM OPEN-IX2-FILE.
           PERFORM OPEN-LTC1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE "O" TO IX-FILE-STATUS.

           GO TO EXIT-SETUP-NEXT-BRANCH.

       CLOSE-LOCAL-FILES.

           MOVE "C" TO IX-FILE-STATUS.

           PERFORM CLOSE-IX2-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

       EXIT-SETUP-NEXT-BRANCH.
           EXIT.

      ******************************************************************
       DISPLAY-MORE-INFO SECTION.
      ******************************************************************

           IF ( IX-FILE-OPEN )
              PERFORM CLOSE-IX2-FILE
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-LTC1-FILE
              PERFORM CLOSE-BW1-FILE
              MOVE "C" TO IX-FILE-STATUS.

           MOVE PRIMARY-KEY-IX-BRNO(FSEL-CURR) TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              IX-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE.

           MOVE PRIMARY-KEY-IX-BRNO  (FSEL-CURR) TO IX-BRNO.
           MOVE PRIMARY-KEY-IX-CODE  (FSEL-CURR) TO IX-CODE.
           MOVE PRIMARY-KEY-IX-ACCTNO(FSEL-CURR) TO IX-ACCTNO.
           MOVE PRIMARY-KEY-IX-SEQNO (FSEL-CURR) TO IX-SEQNO.

      * OPEN LOCAL FILES
           PERFORM OPEN-IX1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LTC1-FILE.
           MOVE "O" TO IX-FILE-STATUS.

           PERFORM READ-IX1-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO END-DISPLAY-MORE-INFO-CLOSE.

      * MOVE DISPLAY MORE INFO

           MOVE SPACES TO WR-BUF DISPLAY-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ALL "-" TO MI-LINE-01.

      * DISPLAY VIN NUMBER,TITLE,YEAR,MAKE,MODEL

           MOVE LTC-PATH-OWNBR     TO LTC-BRNO.
           MOVE IX-ACCTNO      TO LTC-ACCTNO LN-ACCTNO.
           MOVE 1              TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG NOT = ZERO
               GO TO END-DISPLAY-MORE-INFO-CLOSE.

           MOVE "VIN -" TO M02-VIN-PMT.

      * IX-SEQNO = SUBSCRIPT OF THE LT VIN
           MOVE LTC-VIN(IX-SEQNO) TO  M02-VIN.

           MOVE "DESC -"TO M02-DESCRIPTION-PMT.
           STRING LTC-YEAR(IX-SEQNO)," ",
                  LTC-MAKE(IX-SEQNO)," ",
                  LTC-MODEL(IX-SEQNO)," "
                  LTC-STYLE(IX-SEQNO)
               DELIMITED BY "  " INTO M02-DESCRIPTION.

           MOVE "TITLE -" TO M02-TITLE-PMT.
           MOVE LTC-TITLE-NO(IX-SEQNO) TO M02-TITLE-NO.

      * DISPLAY BRANCH INFO

           MOVE "BRANCH :"            TO M04-BRANCH-PMT.
           MOVE BR-NO                 TO M04-BRANCH.
           MOVE "PHONE:"              TO M04-BR-PHONE-PMT.
           MOVE BR-PHONE              TO EDITOR-INPUT-PHONE.
           PERFORM EDITOR-PHONE-FORMAT-EDIT.
           MOVE EDITOR-OUTPUT-PHONE-EDIT TO M04-BR-PHONE.

      * DISPLAY ACCOUNT NUMBER

           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = ZEROS
               GO TO END-DISPLAY-MORE-INFO-CLOSE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = ZEROS
               GO TO END-DISPLAY-MORE-INFO-CLOSE.

           MOVE "LOAN #   :" TO M06-ACCTNO-PMT.
           MOVE LN-ACCTNO    TO M06-ACCTNO.

      * DISPLAY NAME

           MOVE "NAME   :" TO M08-NAME-PMT.
           STRING BW-LNAME,", " BW-FNAME
               DELIMITED BY "   " INTO M08-NAME.

           MOVE "LOAN DATE:" TO M08-LOANDATE-PMT.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO M08-LOANDATE.

      * DISPLAY ADDRESS, BALANCE OR PAYOFF DATE

           MOVE "ADDRESS:" TO M10-ADDRESS-PMT.
           STRING BW-ADR1," ",BW-ADR2 DELIMITED
              BY "  " INTO M10-STREET-ADD.

           IF LN-CURBAL NOT = 0
               MOVE "BALANCE  :" TO M10-PAID-BALANCE-PMT
               MOVE LN-CURBAL    TO M10-BALANCE
           ELSE
              MOVE "PAID OFF :" TO M10-PAID-BALANCE-PMT
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO M10-PAID-DATE.

      * REPO/SOLD DATES

           IF LN-REPODATE NOT = ZEROS
              MOVE "REPO DATE:" TO M14-REPODATE-PMT
              MOVE LN-REPODATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO M14-REPODATE.

           IF LN-SOLDDATE NOT = ZEROS
              MOVE "SOLD:" TO M14-SOLD-PMT
              MOVE LN-SOLDDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO M14-SOLDDATE.

      * DISPLAY CITY, P&L DATE

           STRING  BW-CITY,", ",BW-STATE," ",BW-ZIP
              DELIMITED BY "  " INTO M12-CITY-ST-ZIP.

           IF LN-PLCD NOT = " "
               MOVE "P&L DATE :" TO M12-PLDATE-PMT
               MOVE LN-PLDATE    TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY  TO M12-PLDATE.

           MOVE ALL "-" TO MI-LINE-14.

           MOVE LEGEND-MORE-INFO TO LEGEND-MORE-INFO-BUF.
 
           MOVE MORE-INFO-BUF TO WR-BUF.
           MOVE MORE-INFO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.

           MOVE "S  A010SEL." TO VDU.
           MOVE SPACES        TO VDUBUF.
           PERFORM SCREENIO.

       END-DISPLAY-MORE-INFO-CLOSE.
           PERFORM CLOSE-IX1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTC1-FILE.

       EXIT-DISPLAY-MORE-INFO.
           EXIT.

      ******************************************************************
      *
      *    E D I T O R - P H O N E - F O R M A T - E D I T
      *
      *-----------------------------------------------------------------
      *    DESC: MOVE TO NEW PHONE FORMAT: AAA1112222 TO (AAA) 111-2222
      *    IN  : EDITOR-INPUT-PHONE
      *    OUT : EDITOR-OUTPUT-PHONE-EDIT
      *          EDITOR-PHONE-SHORT-EDIT
      ******************************************************************
       EDITOR-PHONE-FORMAT-EDIT SECTION.

           MOVE "("               TO EDITOR-PHONE-EDIT-P1.
           MOVE EDITOR-PHONE-AREA TO EDITOR-PHONE-EDIT-AREA.
           MOVE ")"               TO EDITOR-PHONE-EDIT-P2.
           MOVE " "               TO EDITOR-PHONE-EDIT-D1.
           MOVE EDITOR-PHONE-3    TO EDITOR-PHONE-EDIT-3.
           MOVE "-"               TO EDITOR-PHONE-EDIT-D2.
           MOVE EDITOR-PHONE-4    TO EDITOR-PHONE-EDIT-4.

       EXIT-PHONE-FORMAT-EDIT.
           EXIT.

      ******************************************************************
       SEND-PROGRESS-INDICATOR SECTION.
      ******************************************************************

           IF NOT ( PROGRESS-INDICATOR-MAX )
              ADD 1 TO PROGRESS-INDICATOR-SLEEP
              GO TO EXIT-SEND-PROGRESS-INDICT.

           MOVE ZEROES TO PROGRESS-INDICATOR-SLEEP.

       SETUP-PROGRESS-INDICATOR.

           IF ( PGID-INDICATOR = "-" ) MOVE "\" TO PGID-INDICATOR
           ELSE
           IF ( PGID-INDICATOR = "\" ) MOVE "|" TO PGID-INDICATOR
           ELSE
           IF ( PGID-INDICATOR = "|" ) MOVE "/" TO PGID-INDICATOR
           ELSE
           IF ( PGID-INDICATOR = "/" ) MOVE "-" TO PGID-INDICATOR.

           MOVE "("                    TO PGID-PARENTHESIS-LEFT.
           MOVE ")"                    TO PGID-PARENTHESIS-RIGHT.
           MOVE " BRANCH: "            TO PGID-BRANCH-PROMPT.
           MOVE BR-NO                  TO PGID-BRANCH.
           MOVE " --- "                TO PGID-HYPHEN.

           MOVE PROGRESS-INDICATOR-TEXT-ELE(PROGRESS-INDICATOR-LEVEL)
                                       TO PGID-TEXT.

       SENDING-PROGRESS-INDICATOR.

           MOVE PROGRESS-INDICATOR TO WR-BUF.
           MOVE PROGRESS-INDICATOR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-SEND-PROGRESS-INDICT.
           EXIT.

      *****************************************************************
       SCAN-GR-FILE SECTION.
      *****************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-GR-FILE.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE ZEROES     TO IO-FG.
           MOVE GRSCAN-KEY TO VDUBUF.

       EXIT-SCAN-GR-FILE.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LTINQ-SCREEN UPON LTINQ-WINDOW-HANDLE.
           MOVE LTINQ-FORM-FIELDS TO WR-BUF.
           MOVE LTINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE LTINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTINQ-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LTINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/CHKSEC.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *-----------------------------------------------------------------
       MOVE-KEY-TO-PAGE-KEY.

           MOVE CURRENT-BRANCH      TO PAGE-KEY-BRNO(PAGE-COUNT).
           MOVE IX-CODE             TO PAGE-KEY-CODE(PAGE-COUNT).
           MOVE IX-ACCTNO           TO PAGE-KEY-ACCTNO(PAGE-COUNT).
           MOVE IX-SEQNO            TO PAGE-KEY-SEQNO(PAGE-COUNT).

       GET-SELECTION-IX1-KEY.

           MOVE PRIMARY-KEY-IX-BRNO  (FSEL-CURR) TO SELECTION-IX-BRNO.
           MOVE PRIMARY-KEY-IX-CODE  (FSEL-CURR) TO SELECTION-IX-CODE.
           MOVE PRIMARY-KEY-IX-ACCTNO(FSEL-CURR) TO SELECTION-IX-ACCTNO.
           MOVE PRIMARY-KEY-IX-SEQNO (FSEL-CURR) TO SELECTION-IX-SEQNO.

      *-----------------------------------------------------------------
       START-NEW-BRANCH-IX-FILE.
           MOVE IX-PATH-OWNBR      TO IX-BRNO.
           MOVE VIN-IDENTIFICATION TO IX-VIN.

           PERFORM START-IX2-FILE.

      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "   " TO LEGEND-LIST-ATTRIBUTE.

       SETUP-HEADER.
           MOVE HEADER-VIN-1 TO HEADER-LINE-1.
           MOVE HEADER-VIN-2 TO HEADER-LINE-2.
           MOVE ALL "-"      TO HEADER-LINE-3.

           MOVE "." TO HEADER-LIST-PERIOD.

           MOVE HEADER-BUF TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO HEADER-LIST-PERIOD.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPIX2RN.CPY".
       COPY "LIBLP/LPIX1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.

           MOVE EXIT-PATH  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LTINQ-SCREEN.
           DESTROY LTINQ-WINDOW-HANDLE.
           EXIT PROGRAM.
