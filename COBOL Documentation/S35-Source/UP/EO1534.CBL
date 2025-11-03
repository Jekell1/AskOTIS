       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EO1534.
       DATE-WRITTEN.  02-26-2020.
      ******************************************************************
      *
      *  IDENTIFICATION: UP/EO1534.CBL
      *
      *  DESCRIPTION   : UPGRADE A15 TO A30 FOR EO BATCH FILES
      *
      *=================================================================
      * COPY EO DIRECTORIES AND FILES FROM SOURCE TO TARGET FOR
      * BEGINNING AND ENDING YEAR RANGE.  SOURCE R2 EO DIRECTORIES ARE
      * COPIED TO TARGET R1 EO DIRECTORIES.
      *
      * 2015: /USR/BEOM/R1/AL1500/EO/*15  /USR/DATA/R1/AL1500/EO/.
      * 2016: /USR/BEOM/R2/TX0400/EO/*16  /USR/DATA/R1/TX0400/EO/.
      *
      *=================================================================
      * REV:
      * JKC 2020-0226 ***NEW*** PT0173
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * REPORT FILE
           SELECT REPORT-FILE ASSIGN TO EO1534-REPORT-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY EO1534-REPORT-KEY
                  FILE STATUS FILE-STAT.

      * USING FOR ORIGINAL MACHINE
           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.

      ******************************************************************
      ******************************************************************
      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      * USING FOR BR-MACHINE IF FIX WAS ALREADY RUN
       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".

      ******************************************************************
      *
      *    R E P O R T - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR PRINTING REPORT
      ******************************************************************
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.

      *-----------------------------------------------------------------
       01  REPORT-REC.
           03  REPORT-DETAIL-LINE         PIC X(132).
           03  REPORT-LN-FEED             PIC 9(002).

      ******************************************************************
      ******************************************************************
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/EO1534.CBL S34 06/21/21-15:54:01 >".

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

      *-----------------------------------------------------------------
       01  EO1534-REPORT-IFN           PIC X(06)     VALUE "REPORT".
       01  EO1534-REPORT-KEY           PIC 9(09)     VALUE ZEROES.
       01  EO1534-REPORT-PATH          PIC X(35)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(02)     VALUE "PR".
       01  OBR-KEY                     PIC 9(4).
       01  OBR-PATH                    PIC X(35)     VALUE SPACES.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CASE-BUF                    PIC X(04)     VALUE SPACES.

      *-----------------------------------------------------------------
      *    CP -R /USR/BEOM/R1/SS####/EO/*YY  /USR/DATA/R1/SS####/EO/.
      *-----------------------------------------------------------------
       01  COPYEO-SYSTEM-BUF.
           03  COPYEO-CP-CMD           PIC X(06)     VALUE "CP -R".
           03  COPYEO-SOURCE-FULL.
               05  COPYEO-SOURCE-BASE  PIC X(09)     VALUE SPACES.
               05  FILLER              PIC X(01)     VALUE "/".
               05  COPYEO-SOURCE-MACH  PIC X(02)     VALUE SPACES.
               05  FILLER              PIC X(01)     VALUE "/".
               05  COPYEO-SOURCE-DATA  PIC X(06)     VALUE SPACES.
               05  FILLER              PIC X(05)     VALUE "/EO/*".
               05  COPYEO-SOURCE-YY    PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE SPACES.
           03  COPYEO-TARGET-FULL.
               05  COPYEO-TARGET-BASE  PIC X(09)     VALUE SPACES.
               05  FILLER              PIC X(01)     VALUE "/".
               05  COPYEO-TARGET-MACH  PIC X(02)     VALUE SPACES.
               05  FILLER              PIC X(01)     VALUE "/".
               05  COPYEO-TARGET-DATA  PIC X(06)     VALUE SPACES.
               05  FILLER              PIC X(05)     VALUE "/EO/.".

      *-----------------------------------------------------------------
       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  ERRCD-LIST                  PIC X(06)     VALUE "ERRCD.".
       01  EXITPROG                    PIC X(09)     VALUE "UP/UPMENU".
       01  EXITPROG-LIST               PIC X(09)     VALUE "EXITPROG.".

      *-----------------------------------------------[ HEX CHARACTERS ]
       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-LINE-FEED               PIC X(01)     VALUE X"0A".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  ER-FEED                     PIC 9(01)     VALUE 1.
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

       01  BRANCH-TABLE.
           03  BRANCH-MACHINE          PIC XX OCCURS 9999 TIMES.

       01  SUBSCRIPT-WORKERS.
           03  ELE                     PIC 9(04)     VALUE ZEROES.
           03  SUB                     PIC 9(04)     VALUE ZEROES.

      *=================================================[ UPGRADE-STAT ]
       78  UPGRADE-STAT---GOOD                     VALUE "00".
       78  UPGRADE-STAT---BAD                      VALUE "XX".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  UPGRADE-STAT                PIC X(02)   VALUE SPACES.
           88  UPGRADE-STAT-GOOD                   VALUE "00".
           88  UPGRADE-STAT-BAD                    VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      *=================================================================
       01  WSS-CH-TIME                 PIC 9(06)   VALUE ZEROES.
       01  FILLER            REDEFINES WSS-CH-TIME.
           03  WSS-CH-HR               PIC 9(02).
           03  WSS-CH-MIN              PIC 9(02).
           03  WSS-CH-SEC              PIC 9(02).

       01  WSS-DATE-YYYYSMM            PIC 9999/99.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER PRINT".
           03  FILLER PIC X(30) VALUE "ER OPTIONS                    ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-SOURCE-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER SOURC".
           03  FILLER PIC X(30) VALUE "E PATH                        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-TARGET-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TARGET PATH               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, UP/DOWN, ENTER BEGINNING".
           03  FILLER PIC X(19) VALUE " BRANCH NUMBER     ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING BRANCH NUMBER      ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING DATE (CCYY)     ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING DATE (CCYY)        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-OPTION-FILES.
           03  FILLER PIC X(30) VALUE "F1 THRU F7, F8-HELP, UP/DOWN, ".
           03  FILLER PIC X(30) VALUE "ENTER (Y)ES/(N)O/(C)HECK FOR U".
           03  FILLER PIC X(19) VALUE "PDATING THIS FILE  ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-ACCEPT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TO ACCEPT (YES/NO)        ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEGIN-PROCESSING.
           03  FILLER PIC X(30) VALUE "PROCESSING....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(19) VALUE "...................".

      ******************************************************************
      *
      *    M E S S A G E - W O R K E R S
      *
      ******************************************************************
       01  MESSAGE-INVALID-YN.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O ONLY!!!".
           03  FILLER PIC X(30) VALUE "                              ".

       01  MESSAGE-INVALID-YNC.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O OR (C)H".
           03  FILLER PIC X(30) VALUE "ECK FILE ONLY!!!              ".

       01  MESSAGE-INVALID-FILE-PATH.
           03  FILLER PIC X(30) VALUE "INVALID FILE PATH; PATH NOT FO".
           03  FILLER PIC X(30) VALUE "UND!!!                        ".

       01  MESSAGE-INVALID-SAME-PATHS.
           03  FILLER PIC X(30) VALUE "INVALID: SOURCE AND TARGET CAN".
           03  FILLER PIC X(30) VALUE "NOT BE THE SAME               ".

      ******************************************************************
      *
      *    P R I N T - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-1.
           03  H1-DATE              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(02)   VALUE SPACES.
           03  H1-TIME              PIC X(08)   VALUE SPACES.
           03  FILLER               PIC X(28)   VALUE SPACES.
           03  H1-NAME              PIC X(40)   VALUE SPACES.
           03  FILLER               PIC X(37)   VALUE SPACES.
           03  FILLER               PIC X(05)   VALUE "PAGE".
           03  H1-PAGE-NO           PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-2.
           03  H2-USERID-PROMPT     PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID            PIC X(10)   VALUE SPACES.
           03  FILLER               PIC X(22)   VALUE SPACES.
           03  H2-TITLE             PIC X(46)   VALUE
               "----- COPY EO BATCH FILES --- A15 TO A30 -----".
           03  FILLER               PIC X(43)   VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-RANGE-LINE        REDEFINES DETAIL-LINE.
           03  R1-DESC                        PIC X(34).
           03  FILLER                         PIC X(01).
           03  R1-FIELD                       PIC X(35).

           03  R1-DATE-RDF          REDEFINES R1-FIELD.
               05  R1-DATE                    PIC 99/99/99.
               05  FILLER                     PIC X(27).

           03  R1-MMYY-RDF          REDEFINES R1-FIELD.
               05  R1-MMYY                    PIC 99/99.
               05  FILLER                     PIC X(30).

           03  R1-NUMERIC-RDF       REDEFINES R1-FIELD.
               05  R1-NUMERIC                 PIC Z(09)9.
               05  FILLER                     PIC X(25).

           03  FILLER                         PIC X(62).

      ******************************************************************
      *
      *    E O 1 5 3 0 - W O R K E R S
      *
      ******************************************************************
       01  EO1534-RECORDS-FOUND        PIC X(01).
           88  EO1534-NO-RECORDS-FOUND               VALUE "N".
           88  EO1534-EXIT-PRIOR-TO-EXECUTION        VALUE "X".

      ******************************************************************
      *
      *    E O 1 5 3 0 - F I L E - W O R K E R S
      *
      ******************************************************************
       01  EO1534-CURRENT-FILE                 PIC X(08)   VALUE SPACES.
       01  EO1534-CURRENT-MMYY                 PIC 9(04)   VALUE ZEROES.

       01  EO1534-FILE-BEG-TIME                PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES EO1534-FILE-BEG-TIME.
           03  EO1534-FILE-BEG-TIME-HH         PIC 9(02).
           03  EO1534-FILE-BEG-TIME-MM         PIC 9(02).
           03  EO1534-FILE-BEG-TIME-SS         PIC 9(02).

       01  EO1534-FILE-INPUT                   PIC X(22)   VALUE SPACES.
       01  FILLER                    REDEFINES EO1534-FILE-INPUT.
           03  EO1534-FILE-INPUT-DIR           PIC X(02).
           03  EO1534-FILE-INPUT-SLASH         PIC X(01).
           03  EO1534-FILE-INPUT-NAME          PIC X(19).

      *-----------------------------------------------------------------
       78  EO1534-FILE-ORDER---SOURCE                      VALUE "S".
       78  EO1534-FILE-ORDER---TARGET                      VALUE "T".
       77  EO1534-FILE-ORDER                   PIC X(01)   VALUE SPACES.
           88  EO1534-FILE-ORDER-SOURCE                    VALUE "S".
           88  EO1534-FILE-ORDER-TARGET                    VALUE "T".

      *-----------------------------------------------------------------
       78  EO1534-FILE-TYPE---BASESH                       VALUE "B".
       78  EO1534-FILE-TYPE---GLOBAL                       VALUE "G".
       78  EO1534-FILE-TYPE---LOCAL                        VALUE "L".
       77  EO1534-FILE-TYPE                    PIC X(01)   VALUE SPACES.
           88  EO1534-FILE-TYPE-BASESH                     VALUE "B".
           88  EO1534-FILE-TYPE-GLOBAL                     VALUE "G".
           88  EO1534-FILE-TYPE-LOCAL                      VALUE "L".

      *=================================================================
       01  EO1534-FILE-PATH                          PIC X(35).

       01  FILLER                          REDEFINES EO1534-FILE-PATH.
           03  FILLER                                PIC X(01).
           03  EO1534-FILE-PATH-FIL                  PIC X(34).

       01  FILLER                          REDEFINES EO1534-FILE-PATH.
           03  EO1534-FILE-PATH-SLASH-1              PIC X(01).
           03  EO1534-FILE-PATH-DIR-1                PIC X(03).
           03  EO1534-FILE-PATH-SLASH-2              PIC X(01).
           03  EO1534-FILE-PATH-DIR-2                PIC X(04).
           03  EO1534-FILE-PATH-SLASH-3              PIC X(01).
           03  EO1534-FILE-PATH-MACHINE              PIC X(02).
           03  EO1534-FILE-PATH-SLASH-4              PIC X(01).
           03  EO1534-FILE-PATH-GLOBAL-LOCAL         PIC X(22).

       01  FILLER                          REDEFINES EO1534-FILE-PATH.
           03  FILLER                                PIC X(13).
           03  EO1534-FILE-PATH-GLOBAL-PATH.
               05  EO1534-FILE-PATH-GLOBAL-DIR       PIC X(02).
               05  EO1534-FILE-PATH-GLOBAL-SLASH     PIC X(01).
               05  EO1534-FILE-PATH-GLOBAL-FILE      PIC X(19).

       01  FILLER                          REDEFINES EO1534-FILE-PATH.
           03  EO1534-FILLER                         PIC X(13).
           03  EO1534-FILE-PATH-LOCAL-DATA           PIC X(06).
           03  EO1534-FILE-PATH-LOCAL-DATA-SL        PIC X(01).
           03  EO1534-FILE-PATH-LOCAL-PATH.
               05  EO1534-FILE-PATH-LOCAL-DIR        PIC X(02).
               05  EO1534-FILE-PATH-LOCAL-SLASH      PIC X(01).
               05  EO1534-FILE-PATH-LOCAL-FILE       PIC X(12).

      *=================================================================
       01  EO1534-TIME-BEG                     PIC 9(06)   VALUE ZEROES.
       01  EO1534-TIME-BEGX          REDEFINES EO1534-TIME-BEG
                                               PIC X(06).
       01  FILLER                    REDEFINES EO1534-TIME-BEG.
           03  EO1534-TIME-BEG-HH              PIC 9(02).
           03  EO1534-TIME-BEG-MM              PIC 9(02).
           03  EO1534-TIME-BEG-SS              PIC 9(02).

       01  EO1534-TIME-END                     PIC 9(06)   VALUE ZEROES.
       01  EO1534-TIME-ENDX          REDEFINES EO1534-TIME-END
                                               PIC X(06).
       01  FILLER                    REDEFINES EO1534-TIME-END.
           03  EO1534-TIME-END-HH              PIC 9(02).
           03  EO1534-TIME-END-MM              PIC 9(02).
           03  EO1534-TIME-END-SS              PIC 9(02).

       01  EO1534-TIME-ELAPSED                 PIC 9(06)   VALUE ZEROES.
       01  FILLER REDEFINES EO1534-TIME-ELAPSED.
           03  EO1534-TIME-ELAPSED-HH          PIC 9(02).
           03  EO1534-TIME-ELAPSED-MM          PIC 9(02).
           03  EO1534-TIME-ELAPSED-SS          PIC 9(02).

       01  EO1534-TIME-SECONDS                 PIC 9(06)   VALUE ZEROES.

      ******************************************************************
      *
      *    E O 1 5 3 0 - D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  EO1534-LN-FEED                     PIC 9(03)    VALUE ZEROES.

      *=================================================================
       01  EO1534-DETAIL-LINE                 PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  EO1534-DETAIL-LINE-1     REDEFINES EO1534-DETAIL-LINE.
           03  EO1534-D1-BRANCH-PROMPT        PIC X(08).
           03  EO1534-D1-BRANCH               PIC ZZZ9.
           03  FILLER                         PIC X(02).
           03  EO1534-D1-BRANCH-NAME          PIC X(66).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-2            REDEFINES EO1534-DETAIL-LINE.
           03  EO1534-D2-MACHINE-PROMPT       PIC X(09).
           03  EO1534-D2-MACHINE              PIC X(02).
           03  FILLER                         PIC X(03).
           03  EO1534-D2-MACHINE-NAME         PIC X(66).

      *-----------------------------------------------------------------
       01  EO1534-FILE-LINE-1       REDEFINES EO1534-DETAIL-LINE.
           03  EO1534-F1-BRNO-PROMPT          PIC X(06).
           03  EO1534-F1-BRNO                 PIC Z(9)9.
           03  FILLER                         PIC X(02).
           03  EO1534-F1-FILE-MESSAGE         PIC X(62).
           03  FILLER               REDEFINES EO1534-F1-FILE-MESSAGE.
               05  EO1534-F1-RECORD-READ-PROMPT   PIC X(06).
               05  EO1534-F1-RECORD-READ          PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  EO1534-F1-RECORD-WRITE-PROMPT  PIC X(07).
               05  EO1534-F1-RECORD-WRITE         PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  EO1534-F1-RECORD-SKIP-PROMPT   PIC X(06).
               05  EO1534-F1-RECORD-SKIP          PIC Z,ZZZ,ZZZ,ZZ9.
           03  FILLER                         PIC X(02).
           03  EO1534-F1-TIMES                PIC X(50).
           03  FILLER               REDEFINES EO1534-F1-TIMES.
               05  FILLER                     PIC X(05).
               05  EO1534-F1-BEG-PROMPT       PIC X(05).
               05  EO1534-F1-BEG-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  EO1534-F1-END-PROMPT       PIC X(05).
               05  EO1534-F1-END-TIME         PIC 99/99/99.
               05  FILLER                     PIC X(02).
               05  EO1534-F1-TOT-PROMPT       PIC X(07).
               05  EO1534-F1-TOT-TIME         PIC 99/99/99.

      ******************************************************************
      *
      *    E O 1 5 3 4 - M V 1 5 - F I L E - W O R K E R S
      *
      *=================================================================
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
       01  EO1534-MV15-OLD-PATH                  PIC X(35).

       01  FILLER                      REDEFINES EO1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(13).
           03  EO1534-MV15-OLD-BASESH-FILE       PIC X(22).

       01  FILLER                      REDEFINES EO1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(16).
           03  EO1534-MV15-OLD-GLOBAL-FILE       PIC X(19).

       01  FILLER                      REDEFINES EO1534-MV15-OLD-PATH.
           03  FILLER                            PIC X(23).
           03  EO1534-MV15-OLD-LOCAL-FILE        PIC X(12).

      *-----------------------------------------------------------------
       01  EO1534-MV15-NEW-PATH                  PIC X(35).

       01  FILLER                      REDEFINES EO1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(13).
           03  EO1534-MV15-NEW-BASESH-O          PIC X(01).
           03  EO1534-MV15-NEW-BASESH-FILE       PIC X(21).

       01  FILLER                      REDEFINES EO1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(16).
           03  EO1534-MV15-NEW-GLOBAL-O          PIC X(01).
           03  EO1534-MV15-NEW-GLOBAL-FILE       PIC X(18).

       01  FILLER                      REDEFINES EO1534-MV15-NEW-PATH.
           03  FILLER                            PIC X(23).
           03  EO1534-MV15-NEW-LOCAL-O           PIC X(01).
           03  EO1534-MV15-NEW-LOCAL-FILE        PIC X(11).

      ******************************************************************
      *
      *    E O 1 5 3 0 - V P I - W O R K E R S 
      *
      *=================================================================
      *    EO1534 VIDEO PROGRESS INDICATOR (VPI)
      ******************************************************************
       01  EO1534-VPI-BRNO                 PIC 9(04)   VALUE ZEROES.

       01  EO1534-VPI-PRU-LP               PIC X(03)   VALUE "HLD".
           88  EO1534-VPI-QUEUE-PIGGYBACK              VALUE "PRU"
                                                             "DEV".
           88  EO1534-VPI-QUEUE-PRINTER                VALUE "   ".
           88  EO1534-VPI-QUEUE-TERMINAL               VALUE "VDU".
           88  EO1534-VPI-QUEUE-VIEW                   VALUE "HLD".
           88  EO1534-VPI-QUEUE-PRINTER-OR-VIEW        VALUE "   "
                                                             "HLD".

       01  EO1534-VPI-ROUTE-BUF            PIC X(02)   VALUE "00".
           88  EO1534-VPI-ROUTE-MENU-DRIVEN            VALUE "00" "  ".

       01  EO1534-VPI-SLEEP-COUNTER        PIC 9(04)   VALUE ZEROES.
       01  EO1534-VPI-SLEEP-COUNTER-MAX    PIC 9(04)   VALUE 500.
       01  EO1534-VPI-TICKER               PIC X(01)   VALUE "-".
       01  EO1534-VPI-TIME-EDIT            PIC X(08)   VALUE SPACES.

      *=================================================================
      *
      *    E O 1 5 3 0 - V P I - D I S P L A Y  - W O R K E R S
      *
      *=================================================================
       01  EO1534-VPI-DISPLAY-BUF               PIC X(80)  VALUE SPACES.
       01  FILLER                     REDEFINES EO1534-VPI-DISPLAY-BUF.
           03  EO1534-VPI-DISPLAY-LEFT          PIC X(01).
           03  EO1534-VPI-DISPLAY-TICKER        PIC X(01).
           03  EO1534-VPI-DISPLAY-RIGHT         PIC X(01).
           03  FILLER                           PIC X(01).
           03  EO1534-VPI-DISPLAY-TEXT          PIC X(76).
           03  FILLER                 REDEFINES EO1534-VPI-DISPLAY-TEXT.
               05  EO1534-VPI-DISPLAY-BRNO      PIC 9(04).
               05  FILLER                       PIC X(02).
               05  EO1534-VPI-DISPLAY-FILE      PIC X(60).
               05  FILLER                       PIC X(02).
               05  EO1534-VPI-DISPLAY-TIME      PIC X(08).

       01  EO1534-VPI-DISPLAY-LIST.
           03  FILLER PIC X(14) VALUE "LEGEND ACCEPT.".

      *=================================================================
      *
      *    E O 1 5 3 0 - V P I - F I L E - W O R K E R S
      *
      *=================================================================
       78  EO1534-VPI-FILE-TYPE---GLOBAL                 VALUE "G".
       78  EO1534-VPI-FILE-TYPE---LOCAL                  VALUE "L".
       77  EO1534-VPI-FILE-TYPE            PIC X(01)     VALUE SPACES.
           88  EO1534-VPI-FILE-TYPE-GLOBAL               VALUE "G".
           88  EO1534-VPI-FILE-TYPE-LOCAL                VALUE "L".

      *-----------------------------------------------------------------
       01  EO1534-VPI-FILE-PREFIX         PIC X(06)   VALUE SPACES.
           88  EO1534-VPI-FILE-CH                     VALUE "CH".
           88  EO1534-VPI-FILE-TR                     VALUE "TR".
      *- - - - - - - - - - -{ MISC. }- - - - - - - - - - -{ MISC. }- - -
           88  EO1534-VPI-FILE-WK1                    VALUE "WK1".
           88  EO1534-VPI-FILE-WK2                    VALUE "WK2".
           88  EO1534-VPI-FILE-REPORT                 VALUE "REPORT".

      *-----------------------------------------------------------------
       01  EO1534-VPI-FILE-MMYY           PIC 9(04)   VALUE ZEROES.

      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       01  DEFAULT-GLOBAL-OPTIONS      PIC X(01)     VALUE SPACES.
       01  DEFAULT-LOCAL-OPTIONS       PIC X(01)     VALUE SPACES.

       01  COPIES-LIST                 PIC X(07)     VALUE "COPIES.".
       01  EOBUF-COPIES                PIC 9(01).

       01  CUSTOMER-LINE-LIST          PIC X(11)    VALUE "START-TIME.".
       01  CUSTOMER-LINE-BUF.
           03  CUSTOMER-RIGHT-SIDE     PIC X(02)     VALUE SPACES.
           03  CUSTOMER-ID             PIC X(08)     VALUE SPACES.
           03  CUSTOMER-LEFT-SIDE      PIC X(02)     VALUE SPACES.

       01  FIL-LIST                    PIC X(11)    VALUE "DATA-SET:B.".
       01  FIL-BUF.
           03  FIL-PROMPT              PIC X(13)     VALUE SPACES.
           03  FIL-INFO                PIC X(35)     VALUE SPACES.

       01  MACHINE-LIST                PIC X(12)   VALUE "MMACHINE:BF.".
       01  MACHINE-BUF.
           03  MACHINE-PROMPT          PIC X(18)     VALUE SPACES.
           03  MACHINE-NAME            PIC X(02)     VALUE SPACES.

       01  PRU-BUF                     PIC 9(02).
       01  PRU-LIST                    PIC X(04)     VALUE "PRU.".

      *-----------------------------------------------------------------
       01  SAVE-REPORT-COMMAND.
           03  SAVE-REPORT-CMD         PIC X(03)   VALUE "MV ".
           03  SAVE-REPORT-SOURCE      PIC X(35)   VALUE SPACES.
           03  FILLER                  PIC X(01)   VALUE SPACES.
           03  SAVE-REPORT-TARGET      PIC X(35)   VALUE SPACES.
           03  FILLER                  REDEFINES SAVE-REPORT-TARGET.
               05  FILLER              PIC X(09).
               05  SAVE-REPORT-TARGET-NEW
                                       PIC X(07).
               05  SAVE-REPORT-TARGET-MKTEMP
                                       PIC X(19).

      *-----------------------------------------------------------------
       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".
       01  TARGET-PATH-LIST            PIC X(07)     VALUE "T-PATH.".

       01  STARTING-TIME-LIST          PIC X(11)    VALUE "START-TIME.".
       01  STARTING-TIME-BUF.
           03  STARTING-TIME-RIGHT     PIC X(02)     VALUE SPACES.
           03  STARTING-TIME           PIC X(08)     VALUE SPACES.
           03  STARTING-TIME-LEFT      PIC X(02)     VALUE SPACES.

      *=================================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------

           03  EO1534-MACHINE          PIC X(02).
           03  EO1534-SOURCE-PATH      PIC X(35).
           03  EO1534-TARGET-PATH      PIC X(35).

           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).

           03  GROUP-OPTION-WORKERS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

           03  BEG-DATE-CCYY           PIC 9(04).
           03  FILLER                  REDEFINES BEG-DATE-CCYY.
               05  BEG-DATE-CCYY-CC    PIC 9(02).
               05  BEG-DATE-CCYY-YY    PIC 9(02).

           03  END-DATE-CCYY           PIC 9(04).
           03  FILLER                  REDEFINES END-DATE-CCYY.
               05  END-DATE-CCYY-CC    PIC 9(02).
               05  END-DATE-CCYY-YY    PIC 9(02).

           03  EO1534-FILE-OPTION-WORKERS.
               05  EO1534-FILE-OPTION-EO             PIC X(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

       01  EO1534-PRU-LP               PIC X(03).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
      *------------------------------------------{ NEED TO BE TOGETHER }
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/EO1534_DEF.CPY".
       COPY "LIBUP/EO1534_WKS.CPY".
     
      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
     
      *=================================================================
       SCREEN SECTION.
       COPY "LIBUP/EO1534_SCN.CPY".

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
                 OBR-FILE REPORT-FILE PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OBR-FILE REPORT-FILE.

      *-----------------------------------------------------------------
       COPY "LIBGB/DECLAR2.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LOAD A TABLE INDEXED BY BR-NO JUST TO KNOW THAT BR-MACHINE.
      * USING THE ORIGINAL A15 BRANCH FILE IN CASE YOU ALREADY CHANGED
      * THE NEW FILE TO ALL BE R1 BRANCHES!
      * BRANCH 9990 WILL BE R1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LOAD-BRANCH-TABLE.

           IF ( ERRCD NOT = SPACES )
              MOVE "NEED ORIGINAL BRANCH FILE FOR MACHINE" TO MESS
              PERFORM SEND-MESS
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           IF ( IO-BAD ) OR ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           PERFORM EO1534-VPI-START.
           PERFORM EO1534-VPI-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    READ SOURCE BRANCH RECODE (OBR-REC)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BR-NO                     TO OBR-KEY.
           PERFORM READ-OBR-FILE.

           IF ( IO-BAD )
              PERFORM EO1534-CREATE-FILE-ERROR
              GO TO MAIN-PROGRAM-NEXT-GROUP.

           PERFORM EO1534-LOAD-FILE-BEG-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO COPY BOTH RISC (R1 & R2) TO THE PRIMARY (R1).
      *    CP -R /USR/BEOM/R1/SS####/EO/*YY  /USR/DATA/R1/SS####/EO/*YY 
      *    CP -R /USR/BEOM/R2/SS####/EO/*YY  /USR/DATA/R1/SS####/EO/*YY 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           CALL "C$TOLOWER" USING COPYEO-CP-CMD, VALUE 06.
           MOVE EO1534-SOURCE-PATH         TO COPYEO-SOURCE-BASE.
           MOVE OBR-MACHINE                TO COPYEO-SOURCE-MACH.
           MOVE OBR-DATA-PATH              TO COPYEO-SOURCE-DATA.
           MOVE ZEROES                     TO COPYEO-SOURCE-YY.
           MOVE EO1534-TARGET-PATH         TO COPYEO-TARGET-BASE.
           MOVE "R1"                       TO COPYEO-TARGET-MACH.
           CALL "C$TOLOWER" USING COPYEO-TARGET-MACH, VALUE 02.
           MOVE BR-DATA-PATH               TO COPYEO-TARGET-DATA.

           PERFORM VARYING SUB FROM BEG-DATE-CCYY-YY BY 1
                     UNTIL SUB > END-DATE-CCYY-YY
                   MOVE SUB                TO COPYEO-SOURCE-YY
                   MOVE COPYEO-SYSTEM-BUF  TO SYSTEM-BUF
                   PERFORM SYSTEM-CALL
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM EO1534-CREATE-FILE-TOTALS.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM PRINT-REPORT-MODULE.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      *****************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE "X"                        TO EO1534-RECORDS-FOUND.
           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "EO1534"                   TO VDU-FORM-NAME.
           MOVE EO1534-QUEUE-POS           TO Q-POS.
           MOVE EO1534-COPIES-POS          TO C-POS.
           MOVE EO1534-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF  C-BUF  P-BUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/EOINIT.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE ZEROES                  TO BEG-BRANCH
              MOVE 9999                    TO END-BRANCH
              MOVE "Y"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP         TO ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE.

           MOVE PRU-LP                     TO EO1534-PRU-LP.
           MOVE "N"                        TO EO1534-RECORDS-FOUND.
           PERFORM SEND-STARTING-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ SETUP-HEADER }
           MOVE EXT-CONAME-CONAME          TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE         TO H1-DATE.
           MOVE EXT-CONAME-USER-ID         TO H2-USERID.

      *- - - - - - - - - - - - - - - - - - - - - -{ MAKING TEMP FILES  }
           MOVE MKTEMP-DEFAULT             TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF                 TO EO1534-REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.

           MOVE ZEROES TO EO1534-REPORT-KEY.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAMETERS-BEGIN.

           PERFORM SEND-TITLE.
           PERFORM SEND-FIL.
           PERFORM SEND-MACHINE.
           PERFORM SEND-CUSTOMER-LINE.

           MOVE EO1534-SOURCE-PATH         TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EO1534-TARGET-PATH         TO WR-BUF.
           MOVE TARGET-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP-KEY )
              GO TO ENTER-END-DATE.

      *=================================================================
       ENTER-SOURCE-PATH.

           MOVE LEGEND-SOURCE-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", EO1534-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                     TO EO1534-SOURCE-PATH.

      *-----------------------------------------------------------------
       ENTER-TARGET-PATH.

           MOVE LEGEND-TARGET-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090T-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              GO TO ENTER-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TARGET FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", EO1534-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-TARGET-PATH.

           MOVE VDUBUF                     TO EO1534-TARGET-PATH.

           IF ( EO1534-SOURCE-PATH = EO1534-TARGET-PATH )
              PERFORM INVALID-SAME-PATHS-MESSAGE
              GO TO ENTER-SOURCE-PATH.

      *=================================================================
      *    ENTERING BRANCH NUMBER RANGE.
      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.
       
           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.
       
           IF ( UP-KEY ) GO TO ENTER-TARGET-PATH.
           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
       
           IF ( F6-KEY )
              PERFORM SCAN-GR-FILE
              IF ( IO-BAD )
                 GO TO ENTER-BEG-BRANCH.
       
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
       
      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
       
           MOVE "N"                        TO GROUP-FLAG.
           MOVE WGR-SRESULT                TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH."       TO VDU.
           MOVE BEG-BRANCH                 TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.
       
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
       
           IF ( UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
       
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.
       
           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO VDUNUM0 BEG-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO VDUNUM0 END-BRANCH.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
       
           PERFORM BR-RANGE-SECURITY.
       
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO ENTER-ACCEPT-YN.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < 2000 ) OR
              ( VDUNUM0 > 2040 )
              MOVE "ERROR: MUST BE YEAR 2000 THRU 2040" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           MOVE VDUNUM0 TO BEG-DATE-CCYY.

      *=================================================================
       ENTER-END-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO ENTER-END-BRANCH.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < 2000 ) OR
              ( VDUNUM0 > 2040 )
              MOVE "ERROR: MUST BE YEAR 2000 THRU 2040" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

           MOVE VDUNUM0 TO END-DATE-CCYY.

           IF ( BEG-DATE-CCYY > END-DATE-CCYY )
              MOVE "ERROR: DATE RANGE INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT TO LEGEND.
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

           PERFORM SEND-MACHINE.

           MOVE "S  A090S-PATH."          TO VDU.
           MOVE EO1534-SOURCE-PATH        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A090T-PATH."          TO VDU.
           MOVE EO1534-TARGET-PATH        TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

           MOVE EXT-EOBUF-DATE1            TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-CCYY         TO BEG-DATE-CCYY.

           MOVE EXT-EOBUF-DATE2            TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-CCYY         TO END-DATE-CCYY.

      *=================================================================
       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L O A D - B R A N C H - T A B L E
      *
      *=================================================================
      * LOAD A BRANCH TABLE ONE TIME INDEXED BY BR-NO FOR BR-MACHINE
      ******************************************************************
        LOAD-BRANCH-TABLE SECTION.

           MOVE SPACES TO ERRCD.

           MOVE "BR"                       TO EO1534-CURRENT-FILE.
           MOVE "GB/BRFILE"                TO EO1534-FILE-INPUT.
           MOVE EO1534-FILE-TYPE---GLOBAL  TO EO1534-FILE-TYPE.

           MOVE EO1534-FILE-ORDER---SOURCE TO EO1534-FILE-ORDER.
           PERFORM EO1534-LOAD-FILE-PATH.
           MOVE EO1534-FILE-PATH           TO EO1534-MV15-OLD-PATH.
           PERFORM EO1534-MV15-OLD-TO-NEW.
           MOVE EO1534-MV15-NEW-PATH       TO OBR-PATH.

      * MOVE OF OLD FILE TO NEW FILE WAS BAD
           IF ( STAT-BAD ) 
              MOVE "X" TO ERRCD
              GO TO LOAD-BRANCH-TABLE-EXIT.

           PERFORM OPEN-OBR-FILE.

           MOVE 1 TO OBR-KEY.
           PERFORM START-OBR-FILE.

        LOAD-NEXT-BRANCH.
           PERFORM READ-OBR-FILE-NEXT.
           IF IO-BAD
              MOVE SPACES   TO ERRCD
              MOVE OBR-PATH TO EO1534-MV15-NEW-PATH
              PERFORM EO1534-MV15-NEW-TO-OLD
              GO TO LOAD-BRANCH-TABLE-EXIT.

           MOVE OBR-MACHINE TO BRANCH-MACHINE(OBR-NO).

           GO TO LOAD-NEXT-BRANCH.

       LOAD-BRANCH-TABLE-EXIT.
           EXIT.

      *****************************************************************
      *
      *    S C A N - G R - F I L E
      *
      *****************************************************************
       SCAN-GR-FILE SECTION.
       
           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-GR-FILE.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "00"       TO VDUSTATUS.
           MOVE ZEROES     TO IO-FG.
           MOVE GRSCAN-KEY TO VDUBUF.
       
      *=================================================================
       EXIT-SCAN-GR-FILE.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - M O D U L E
      *
      ******************************************************************
       PRINT-REPORT-MODULE SECTION.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "REPORT"                   TO EO1534-CURRENT-FILE.
           MOVE EO1534-FILE-TYPE---GLOBAL  TO EO1534-FILE-TYPE.

           PERFORM EO1534-VPI-START.

           PERFORM OPEN-PR-FILE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP 'REPORT' FILE  }
           MOVE ZEROES                     TO EO1534-REPORT-KEY.

           PERFORM START-REPORT-FILE.

      *=================================================================
       PRINT-REPORT-NEXT-LINE.

           PERFORM READ-REPORT-FILE-NEXT.

           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-END.

           PERFORM EO1534-VPI-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE REPORT-DETAIL-LINE         TO DETAIL-LINE.
           MOVE REPORT-LN-FEED             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-NEXT-LINE.

      *=================================================================
       PRINT-REPORT-MODULE-END.

           PERFORM CLOSE-REPORT-FILE.
           PERFORM PRINT-RANGES.

      *-----------------------------------------------------------------
       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R A N G E S
      *
      ******************************************************************
       PRINT-RANGES SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE 99 TO LN-COUNT.

           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                    :" TO R1-DESC
              MOVE ENT-GROUP                            TO R1-FIELD
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER          :" TO R1-DESC
              MOVE BEG-BRANCH                           TO R1-NUMERIC
              MOVE 2                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH NUMBER             :" TO R1-DESC
              MOVE END-BRANCH                           TO R1-NUMERIC
              MOVE 1                                    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SOURCE PATH                      :" TO R1-DESC.
           MOVE EO1534-SOURCE-PATH                   TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TARGET PATH                      :" TO R1-DESC.
           MOVE EO1534-TARGET-PATH                   TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE (CCYY)            :" TO R1-DESC.
           MOVE BEG-DATE-CCYY                        TO R1-NUMERIC.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE (CCYY)               :" TO R1-DESC.
           MOVE END-DATE-CCYY                        TO R1-NUMERIC.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME OF THE UPGRADE     :" TO R1-DESC.
           MOVE STARTING-TIME                        TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME OF THE UPGRADE     :" TO R1-DESC.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                            TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       EXIT-PRINT-RANGES.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

      *KC  IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
           IF ( LN-WK GREATER THAN 60 )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED                     TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

      *=================================================================
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - H E A D E R - L I N E S
      *
      ******************************************************************
       WRITE-HEADER-LINES SECTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE: 01 }
           ADD 01                          TO PAGE-NUMBER.
           MOVE PAGE-NUMBER                TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO H1-TIME.
           MOVE HEADER-LINE-1              TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE: 02 }
           MOVE HEADER-LINE-2              TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ LINE: 03 }
           MOVE ALL "-"                    TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 04                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.

      *=================================================================
       EXIT-WRITE-HEADER-LINES.
           EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 0 - C R E A T E - D E T A I L - L I N E
      *
      *=================================================================
      *    IN  : EO1534-DETAIL-LINE
      *          EO1534-LN-FEED
      *          EO1534-REPORT-KEY
      *
      *    OUT : EO1534-DETAIL-LINE
      *          EO1534-LN-FEED
      *          EO1534-REPORT-KEY
      *
      *    DESC: WRITE RECORD TO REPORT-FILE USING DETAIL-LINE.
      ******************************************************************
       EO1534-CREATE-DETAIL-LINE SECTION.

            IF ( EO1534-REPORT-KEY = 999999999 )
               GO TO EO1534-CREATE-DETAIL-EXIT.

      *- - - - - - - - - - - - - - - - - - - -{ CREATE 'REPORT' RECORD }
            ADD  01                        TO EO1534-REPORT-KEY.
            MOVE EO1534-DETAIL-LINE        TO REPORT-DETAIL-LINE.
            MOVE EO1534-LN-FEED            TO REPORT-LN-FEED.

            PERFORM WRITE-REPORT-FILE.

            IF ( IO-BAD )
               GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ RESET FIELDS }
            MOVE SPACES                    TO EO1534-DETAIL-LINE.
            MOVE 01                        TO EO1534-LN-FEED.

      *=================================================================
       EO1534-CREATE-DETAIL-EXIT.
            EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 0 - C R E A T E - F I L E - E R R O R
      *
      *=================================================================
      *    IN  : EO1534-CURRECT-FILE
      *          EO1534-FILE-ORDER    [SOURCE/TARGET]
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE ERROR LINE FOR THE REPORT-FILE.
      ******************************************************************
       EO1534-CREATE-FILE-ERROR SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "BRNO: "                   TO EO1534-F1-BRNO-PROMPT.
           MOVE BR-NO                     TO EO1534-F1-BRNO.

           STRING "ERROR - SOURCE BRANCH FILE: ",STAT
                  DELIMITED BY "  "      INTO EO1534-F1-FILE-MESSAGE.

           MOVE 01                         TO EO1534-LN-FEED.
           PERFORM EO1534-CREATE-DETAIL-LINE.

      *=================================================================
       EO1534-CREATE-FILE-ER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 0 - C R E A T E - F I L E - T O T A L S
      *
      *=================================================================
      *    IN  : EO1534-CURRECT-FILE
      *          EO1534-RECORD-COUNTERS
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE TOTAL LINE FOR THE REPORT-FILE.
      ******************************************************************
       EO1534-CREATE-FILE-TOTALS SECTION.

           MOVE "BRNO: "                   TO EO1534-F1-BRNO-PROMPT.
           MOVE BR-NO                      TO EO1534-F1-BRNO.
           MOVE COPYEO-TARGET-FULL         TO EO1534-F1-FILE-MESSAGE.

           MOVE "BEG: "                  TO EO1534-F1-BEG-PROMPT.
           MOVE EO1534-FILE-BEG-TIME     TO EO1534-F1-BEG-TIME.
           INSPECT EO1534-F1-BEG-TIME REPLACING ALL "/" BY ":".

           MOVE "END: "                  TO EO1534-F1-END-PROMPT.
           MOVE "Y"                      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACES                   TO TIME-MILITARY-FG.
           MOVE HH-MM-SS                 TO EO1534-F1-END-TIME.
           INSPECT EO1534-F1-END-TIME REPLACING ALL "/" BY ":".

           MOVE "TOTAL: "                TO EO1534-F1-TOT-PROMPT.
           MOVE EO1534-FILE-BEG-TIME     TO EO1534-TIME-BEG.
           MOVE HH-MM-SS                 TO EO1534-TIME-END.
           PERFORM EO1534-LOAD-TIME-ELAPSED.
           MOVE EO1534-TIME-ELAPSED      TO EO1534-F1-TOT-TIME.
           INSPECT EO1534-F1-TOT-TIME REPLACING ALL "/" BY ":".

           MOVE 01                          TO EO1534-LN-FEED.
           PERFORM EO1534-CREATE-DETAIL-LINE.

      *=================================================================
       EO1534-CREATE-FILE-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 0 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          EO1534-FILE-INPUT  [EXP.  XX/XXXXXX OR XX/TR####]
      *          EO1534-FILE-ORDER  [SOURCE/TARGET]
      *          EO1534-FILE-TYPE   [GLOBAL/LOCAL]
      *          EO1534-SOURCE-PATH
      *          EO1534-TARGET-PATH
      *
      *    OUT : EO1534-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       EO1534-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO EO1534-FILE-PATH.

           IF ( EO1534-FILE-INPUT           = SPACES ) OR
              ( EO1534-FILE-INPUT-DIR       = SPACES ) OR
              ( EO1534-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( EO1534-FILE-INPUT-NAME      = SPACES )
              GO TO EO1534-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EO1534-FILE-ORDER-SOURCE ) AND
              ( EO1534-FILE-TYPE-GLOBAL  )
              MOVE EO1534-SOURCE-PATH  TO EO1534-FILE-PATH
              MOVE "/"                 TO EO1534-FILE-PATH-SLASH-3
              MOVE EO1534-MACHINE      TO EO1534-FILE-PATH-MACHINE
              MOVE "/"                 TO EO1534-FILE-PATH-SLASH-4
              MOVE EO1534-FILE-INPUT   TO EO1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EO1534-FILE-ORDER-SOURCE ) AND
              ( EO1534-FILE-TYPE-LOCAL   )
              MOVE EO1534-SOURCE-PATH    TO EO1534-FILE-PATH
              MOVE "/"                   TO EO1534-FILE-PATH-SLASH-3
      * NEED ORIGINAL OLD BRANCH, MIGHT HAVE RAN FIX
              MOVE BRANCH-MACHINE(BR-NO) TO EO1534-FILE-PATH-MACHINE
              MOVE "/"                 TO EO1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO EO1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO EO1534-FILE-PATH-LOCAL-DATA-SL
              MOVE EO1534-FILE-INPUT   TO EO1534-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EO1534-FILE-ORDER-TARGET ) AND
              ( EO1534-FILE-TYPE-GLOBAL  )
              MOVE EO1534-TARGET-PATH  TO EO1534-FILE-PATH
              MOVE "/"                 TO EO1534-FILE-PATH-SLASH-3
              MOVE EO1534-MACHINE      TO EO1534-FILE-PATH-MACHINE
              MOVE "/"                 TO EO1534-FILE-PATH-SLASH-4
              MOVE EO1534-FILE-INPUT   TO EO1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EO1534-FILE-ORDER-TARGET ) AND
              ( EO1534-FILE-TYPE-LOCAL   )
              MOVE EO1534-TARGET-PATH  TO EO1534-FILE-PATH
              MOVE "/"                 TO EO1534-FILE-PATH-SLASH-3
              MOVE BR-MACHINE          TO EO1534-FILE-PATH-MACHINE
              MOVE "/"                 TO EO1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO EO1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO EO1534-FILE-PATH-LOCAL-DATA-SL
              MOVE EO1534-FILE-INPUT   TO EO1534-FILE-PATH-LOCAL-PATH.

      *=================================================================
       EO1534-LOAD-FILE-PATH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 4 - L O A D - T I M E - E L A P S E D
      *
      *=================================================================
      *    IN  : EO1534-TIME-BEG
      *          EO1534-TIME-END
      *
      *    OUT : EO1534-TIME-ELAPSED
      *
      *    DESC: USED TO GET THE ELAPSED TIME BETWEEN BEGIN AND END
      *
      *    NOTE: IF THE TIME CROSSED OVER MIDNIGHT, NEED TO ADD 24 HOURS
      *          TO THE END TIME SO TO CALCULATE THE ELAPSED TIME.
      *
      *=================================================================
      *    CALCULATION IS BASED ON CONVERTING THE TIME INTO SECONDS:
      *      1 HOUR   = 3600 SECONDS
      *      1 MINUTE = 60 SECONDS
      *      1 SECOND = 1 SECOND
      *
      *      02:34:56  = 9296
      *      02 * 3600 = 7200
      *      34 * 60   = 2040
      *      56 * 1    = 56
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * (9296/3600))) / 60
      *      56 = (9296 - (3600 * (9296/3600)) -
      *           ( 60 * ((9296 - (3600 * (9296/3600)))
      *
      *      9296 = 02:34:56
      *      02 = 9296 / 3600
      *      34 = (9296 - (3600 * [2]) / 60
      *      56 = (9296 - (3600 * [2]) - (60 * [34])
      *            9296 - (  7200    ) - (  2040   )
      *
      ******************************************************************
       EO1534-LOAD-TIME-ELAPSED SECTION.

           IF ( EO1534-TIME-BEG-HH > EO1534-TIME-END-HH )
              ADD 24 TO EO1534-TIME-END-HH.

           COMPUTE EO1534-TIME-SECONDS = (( EO1534-TIME-END-HH * 3600 )
                                       +  ( EO1534-TIME-END-MM * 60   )
                                       +    EO1534-TIME-END-SS         )
                                       - (( EO1534-TIME-BEG-HH * 3600 )
                                       +  ( EO1534-TIME-BEG-MM * 60   )
                                       +    EO1534-TIME-BEG-SS        ).

           COMPUTE EO1534-TIME-ELAPSED-HH =
                   EO1534-TIME-SECONDS / 3600.

           COMPUTE EO1534-TIME-ELAPSED-MM =
                   ( EO1534-TIME-SECONDS -
                    (3600 * EO1534-TIME-ELAPSED-HH))
                   / 60.

           COMPUTE EO1534-TIME-ELAPSED-SS =
                   EO1534-TIME-SECONDS -
                   (3600 * EO1534-TIME-ELAPSED-HH) -
                   (60 * EO1534-TIME-ELAPSED-MM).

      *=================================================================
       EO1534-LOAD-TIME-ELAP-EXIT.
           EXIT.

      ******************************************************************
      * DUE TO ACU4GL, THE A15 FILE NEEDS TO BE RENAMED (VIA MOVE; MV)
      * SO IT IDENTIFIES THE CORRECT FILE.
      * FOR EXAMPLE, USING THE STFILE, THE A15 FILE OF
      * /US1/DA15/R1/LP/STFILE IS SEEN AS THE A34 STFILE DATABASE TABLE.
      * WHEN TRYING TO OPEN THE A15 FILE IT WILL RESULT IN AN ACU ERROR
      * OF "9D,,2".  FOR THE UPGRADE PROGRAMS TO WORK, NEED TO:
      * 
      *  1. MOVE /US1/DA15/R1/LP/STFILE TO /US1/DA15/R1/LP/OSTFILE
      *  2. OPEN, READ-NEXT, CLOSE ON THE OSTFILE.
      *  3. MOVE /US1/DA15/R1/LP/OSTFILE TO /US1/DA15/R1/LP/STFILE
      * 
      * NEED TO MOVE THE FILE BACK TO ITS ORIGINAL NAME IN CASE THE
      * UPGRADE IS BEING EXECUTED MULTIPLE TIMES (QA TESTING) OR FOR
      * SIDE-BY-SIDE TESTING BETWEEN A15 & A34.
      *
      *=================================================================
      *  BASESH - /US1/DA15/SH/YYFILE
      *  GLOBAL - /US1/DA15/R1/XX/YYFILE
      *  LOCAL  - /US1/DA15/R1/SS####/XX/YYFILE
      *           ....|....1....|....2....|....3
      *
      ******************************************************************
      *
      *    E O 1 5 3 4 - M V 1 5 - O L D - T O - N E W
      *
      *=================================================================
      * IN  : EO1534-MV15-OLD-PATH
      *       EO1534-FILE-TYPE  [B, G, L]
      * OUT : EO1534-MV15-NEW-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE OLD FILE TO NEW FILE.
      ******************************************************************
       EO1534-MV15-OLD-TO-NEW SECTION.

           MOVE EO1534-MV15-OLD-PATH TO EO1534-MV15-NEW-PATH.

           IF ( EO1534-FILE-TYPE-BASESH )
              MOVE "O" TO EO1534-MV15-NEW-BASESH-O 
              MOVE EO1534-MV15-OLD-BASESH-FILE TO
                   EO1534-MV15-NEW-BASESH-FILE
           ELSE
           IF ( EO1534-FILE-TYPE-GLOBAL )
              MOVE "O" TO EO1534-MV15-NEW-GLOBAL-O 
              MOVE EO1534-MV15-OLD-GLOBAL-FILE TO
                   EO1534-MV15-NEW-GLOBAL-FILE
           ELSE
              MOVE "O" TO EO1534-MV15-NEW-LOCAL-O 
              MOVE EO1534-MV15-OLD-LOCAL-FILE TO
                   EO1534-MV15-NEW-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE EO1534-MV15-OLD-PATH TO MV-FILE1.
           MOVE EO1534-MV15-NEW-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       EO1534-MV15-OLD-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 4 - M V 1 5 - N E W - T O - O L D
      *
      *=================================================================
      * IN  : EO1534-MV15-NEW-PATH
      *       EO1534-FILE-TYPE  [B, G, L]
      * OUT : EO1534-MV15-OLD-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE NEW FILE TO OLD FILE.
      ******************************************************************
       EO1534-MV15-NEW-TO-OLD SECTION.

           MOVE EO1534-MV15-NEW-PATH TO EO1534-MV15-OLD-PATH.

           IF ( EO1534-FILE-TYPE-BASESH )
              MOVE EO1534-MV15-NEW-BASESH-FILE TO
                   EO1534-MV15-OLD-BASESH-FILE
           ELSE
           IF ( EO1534-FILE-TYPE-GLOBAL )
              MOVE EO1534-MV15-NEW-GLOBAL-FILE TO
                   EO1534-MV15-OLD-GLOBAL-FILE
           ELSE
              MOVE EO1534-MV15-NEW-LOCAL-FILE TO
                   EO1534-MV15-OLD-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE EO1534-MV15-NEW-PATH TO MV-FILE1.
           MOVE EO1534-MV15-OLD-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       EO1534-MV15-NEW-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 0 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  EO1534-VPI-BRNO
      *        EO1534-VPI-FILE-PREFIX
      *        EO1534-VPI-FILE-MMYY
      *        EO1534-VPI-FILE-TYPE
      *        EO1534-VPI-PRU-LP
      *        EO1534-VPI-ROUTE-BUF
      *        EO1534-VPI-SLEEP-COUNTER
      *        EO1534-VPI-SLEEP-COUNTER-MAX
      *        EO1534-VPI-TICKER
      *        EO1534-VPI-TIME-EDIT
      *
      *  OUT:  EO1534-VPI-SLEEP-COUNTER
      *        EO1534-VPI-TICKER
      *
      * DESC:  EO1534 VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       EO1534-VPI-DISPLAY SECTION.

           IF ( EO1534-VPI-SLEEP-COUNTER 
              < EO1534-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO EO1534-VPI-SLEEP-COUNTER
              GO TO EO1534-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE EO1534-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO EO1534-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO EO1534-VPI-DISPLAY-BUF.

           MOVE "("                        TO EO1534-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO EO1534-VPI-DISPLAY-RIGHT.

           IF ( EO1534-VPI-TICKER = "-" )
              MOVE "\"                     TO EO1534-VPI-DISPLAY-TICKER
                                              EO1534-VPI-TICKER
           ELSE
           IF ( EO1534-VPI-TICKER = "\" )
              MOVE "|"                     TO EO1534-VPI-DISPLAY-TICKER
                                              EO1534-VPI-TICKER
           ELSE
           IF ( EO1534-VPI-TICKER = "|" )
              MOVE "/"                     TO EO1534-VPI-DISPLAY-TICKER
                                              EO1534-VPI-TICKER
           ELSE
              MOVE "-"                     TO EO1534-VPI-DISPLAY-TICKER
                                              EO1534-VPI-TICKER.

           MOVE BR-NO                      TO EO1534-VPI-DISPLAY-BRNO.
           MOVE BR-NAME                    TO EO1534-VPI-DISPLAY-FILE

           MOVE EO1534-VPI-TIME-EDIT       TO EO1534-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING EO1534-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EO1534-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( EO1534-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE EO1534-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE EO1534-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       EO1534-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E O 1 5 3 0 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  EO1534-CURRENT-FILE
      *        EO1534-CURRENT-MMYY
      *        EO1534-FILE-TYPE
      *        EO1534-PRU-LP
      *
      *  OUT:  EO1534-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       EO1534-VPI-START SECTION.

           IF ( EO1534-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO EO1534-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO EO1534-VPI-BRNO.

           MOVE EO1534-CURRENT-FILE        TO EO1534-VPI-FILE-PREFIX.
           MOVE EO1534-CURRENT-MMYY        TO EO1534-VPI-FILE-MMYY.
           MOVE EO1534-FILE-TYPE           TO EO1534-VPI-FILE-TYPE.
           MOVE EO1534-PRU-LP              TO EO1534-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO EO1534-VPI-ROUTE-BUF.
           MOVE 9999                       TO EO1534-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO EO1534-VPI-TIME-EDIT.

      *=================================================================
       EO1534-VPI-START-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - F I L E - B E G - T I M E
      *
      *=================================================================
      *    IN  : NOTHING
      *    OUT : EO1534-FILE-BEG-TIME
      *    DESC: LOAD THE BEGINNING TIME OF THE UPGRADE OF FILE
      ******************************************************************
       EO1534-LOAD-FILE-BEG-TIME SECTION.

           MOVE "Y"                        TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE SPACES                     TO TIME-MILITARY-FG.
           MOVE HH-MM-SS                   TO EO1534-FILE-BEG-TIME.

      *=================================================================
       EO1534-LOAD-FILE-B-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.
     
      *    MOVE EXT-CONAME-CONAME          TO EO1534-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO EO1534-SYSDATE.
      *    DISPLAY EO1534-SCREEN         UPON TCLP-MAIN-WINDOW-HANDLE.
           DISPLAY EO1534-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EO1534-FORM-FIELDS         TO WR-BUF.
           MOVE EO1534-FORM-LIST           TO WR-LIST.
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
     
           MOVE EO1534-HELP--FILE          TO HELP-LINK-FILE.
           MOVE EO1534-HELP--DIR           TO HELP-LINK-DIR.
     
      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
     
           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.
     
           EVALUATE TRUE
              COPY "LIBUP/EO1534_EVA.CPY".
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
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************************************
      *
      *     M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPH SECTION.
      *=================================================================
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       INVALID-FILE-PATH-MESSAGE.
           MOVE MESSAGE-INVALID-FILE-PATH TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       INVALID-SAME-PATHS-MESSAGE.
           MOVE MESSAGE-INVALID-SAME-PATHS TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       INVALID-YNC-MESSAGE.
           MOVE MESSAGE-INVALID-YNC TO MESS.
           PERFORM SEND-MESS.

      *=================================================================
       SEND-CUSTOMER-LINE.

           MOVE "[ "         TO CUSTOMER-RIGHT-SIDE.
           MOVE "**N/A**"    TO CUSTOMER-ID.
           MOVE " ]"         TO CUSTOMER-LEFT-SIDE.

           MOVE CUSTOMER-LINE-BUF          TO WR-BUF.
           MOVE CUSTOMER-LINE-LIST         TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-FIL.

           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.

           MOVE SPACES                     TO EO1534-FILE-PATH.
           STRING "/", GETENV-BUF
                  DELIMITED BY "  "      INTO EO1534-FILE-PATH.
           MOVE EO1534-FILE-PATH-MACHINE   TO EO1534-MACHINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK TO SEE IF THE SOURCE & TARGET PATHS ARE SET FROM BATCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EO1534-SOURCE-PATH = SPACES )
              MOVE EO1534-FILE-PATH        TO EO1534-SOURCE-PATH
           ELSE
              MOVE EO1534-SOURCE-PATH      TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE EO1534-FILE-PATH     TO EO1534-SOURCE-PATH.
     
           IF ( EO1534-TARGET-PATH = SPACES )
              MOVE EO1534-FILE-PATH        TO EO1534-TARGET-PATH
           ELSE
              MOVE EO1534-TARGET-PATH      TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE EO1534-FILE-PATH     TO EO1534-TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CURRENT FIL: "            TO FIL-PROMPT.
           MOVE EO1534-FILE-PATH           TO FIL-INFO.


           MOVE FIL-BUF                    TO WR-BUF.
           MOVE FIL-LIST                   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND                     TO WR-BUF.
           MOVE LEGEND-LIST                TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-MACHINE.

           MOVE "ONLY FOR MACHINE: " TO MACHINE-PROMPT.
           MOVE EO1534-MACHINE       TO MACHINE-NAME.

           MOVE MACHINE-BUF                TO WR-BUF.
           MOVE MACHINE-LIST               TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-STARTING-TIME.

           PERFORM GET-TIME.
           MOVE "[ "      TO STARTING-TIME-RIGHT.
           MOVE TIME-EDIT TO STARTING-TIME.
           MOVE " ]"      TO STARTING-TIME-LEFT.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE STARTING-TIME-BUF       TO WR-BUF
              MOVE STARTING-TIME-LIST      TO WR-LIST
              PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-TITLE.
           MOVE "S  A300TITLE:FR."                       TO VDU.
           MOVE "[ A15 TO A30 --- EO BATCH FILES      ]" TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      *=================================================================
      * COPYMEMBER: LIBGB/GBBRRN
      *=================================================================
       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN INPUT OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.

      *-----------------------------------------------------------------
       READ-OBR-FILE.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE.
           IF IO-FG = 8
              GO TO READ-OBR-FILE.

      *-----------------------------------------------------------------
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.

      *-----------------------------------------------------------------
       START-OBR-FILE.
           PERFORM START-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           START OBR-FILE KEY NOT < OBR-KEY.

      *-----------------------------------------------------------------
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.

      *=================================================================
       OPEN-REPORT-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE EO1534-REPORT-IFN TO E-FILE.
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
           MOVE EO1534-REPORT-IFN TO E-FILE.
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
           MOVE EO1534-REPORT-IFN TO E-FILE.
           MOVE EO1534-REPORT-KEY TO E-KEYX.
           READ REPORT-FILE.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE.

      *-----------------------------------------------------------------
       READ-REPORT-FILE-NEXT.

           PERFORM READ-IT.
           MOVE EO1534-REPORT-IFN TO E-FILE.
           MOVE EO1534-REPORT-KEY TO E-KEYX.
           READ REPORT-FILE NEXT.

           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE-NEXT.

      *-----------------------------------------------------------------
       START-REPORT-FILE.

           PERFORM START-IT.
           MOVE EO1534-REPORT-IFN TO E-FILE.
           MOVE EO1534-REPORT-KEY TO E-KEYX.
           START REPORT-FILE KEY NOT < EO1534-REPORT-KEY.
           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       WRITE-REPORT-FILE.

           PERFORM WRITE-IT.
           MOVE EO1534-REPORT-IFN TO E-FILE.
           MOVE EO1534-REPORT-KEY TO E-KEYX.

           WRITE REPORT-REC.

           IF ( IO-LOCKED )
             GO TO WRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       REWRITE-REPORT-FILE.

           PERFORM REWRITE-IT.
           MOVE EO1534-REPORT-IFN TO E-FILE.
           MOVE EO1534-REPORT-KEY TO E-KEYX.
           REWRITE REPORT-REC.

           IF ( IO-LOCKED )
              GO TO REWRITE-REPORT-FILE.

           UNLOCK REPORT-FILE.

      *-----------------------------------------------------------------
       CLOSE-REPORT-FILE.

           PERFORM CLOSE-IT.
           CLOSE REPORT-FILE.

      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
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

           IF ( EO1534-EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVING REPORT FILE IN /USR/TMP TO /USR/TMP/EO1534.MKTEMP
      *    SAVING REPORT FILE IN CASE OF ANY OF THE UPGRADE PROGRAMS
      *    BOMB OUT SO YOU CAN SEE WHICH FILE WAS PROCESSED LAST.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MV "                      TO SAVE-REPORT-CMD.
           CALL "C$TOLOWER" USING SAVE-REPORT-CMD, VALUE 03.

           MOVE EO1534-REPORT-PATH         TO MKTEMP-BUF
                                              SAVE-REPORT-SOURCE
                                              SAVE-REPORT-TARGET.

           MOVE "EO1534."                  TO SAVE-REPORT-TARGET-NEW.
           CALL "C$TOLOWER" USING SAVE-REPORT-TARGET-NEW, VALUE 07.

           MOVE MKTEMP-NAME                TO SAVE-REPORT-TARGET-MKTEMP.

           MOVE SAVE-REPORT-COMMAND        TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       STOP-RUN.
           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EO1534-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

