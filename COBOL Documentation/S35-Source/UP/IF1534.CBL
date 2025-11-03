       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IF1534.
       DATE-WRITTEN.  08-07-2019.
      ******************************************************************
      *
      *  IDENTIFICATION: UP/IF1534.CBL
      *
      *  DESCRIPTION   : FIRST FILES THAT MUST GET CONVERTED
      *
      *  ---GLOBAL---
      *  BR [GB01BR] RELATIVE TO INDEX
      *  B2 [GB01B2] MERGE INTO BRFILE
      *  GP [GB01GP] MERGED RECORDS/KEYS #1 & 2, RELATIVE TO INDEX
      *  GR [GB01GL] ALREADY INDEX, NEEDS TO GET CONVERTED PRIOR TO MAIN ONE
      *
      *=================================================================
      * REV:
      * BAH 2019-0807 NEW FOR A15 TO A34
      * AMM 2019-1104 FORCE F3-ALL FOR BRANCH RANGE
      * BAH 2019-0114 REMOVED GP-AP FIELDS
      * KEC 2020-0309 CHANGED UP1534-REPORT-KEY FROM 9(9) TO 9(11).
      * BAH 2020-0506 MOVE 0 TO BR-REGION-NUMBER, FOUND NON NUMERIC(434)
      * BAH 2020-1210 ADJUST BR-TIME-ZONE-FACTOR FOR R2 BRANCHES, PER KEVIN
      * BAH 2021-1101 REMOVED THE FOLLOWING:
      *          MOVE OGR-TITLE                  TO GR-TITLE.
      *          MOVE OGR-DESC(1)                TO GR-DESC(1).
      *          MOVE OGR-DESC(2)                TO GR-DESC(2).
      *          MOVE OGR-CONTACT-NO             TO GR-CONTACT-NO.
      * BAH 2022-0627 ADDED NOT NUMERIC CHECKS TO GPFILE
      * BAH 2022-0921 ADDED BR-VERITEC-LICENSE
      * BAH 230302 ADDED GP-TEST-PP-AUTO-INSCOVR #1594
      * BAH 230419 REMOVED GP-TEST-PP-AUTO-INSCOVR #1594A
      * BAH 2023.1023 ADDED GP-CREATE-PAY-XTRP-XNO #1629
      * BAH 2023.1110 ADDED BR-BRANCH-CLOSED AND BR-ALLOW-F3-ALL-DOCS #1630
      * BAH 2023.1128 FIXED NON NUMERIC IN GPFILE
      * JKC 2023-0713 ADDED BR-USE-NEW-WFARGO-ACCT #1616
      * BAH 2024-0718 ADDED DATABASE NAME AND SERVER NAME TO RANGE LINES
      * JKC 2024-1203 ADDED MORE '2020' CHECKS ON 
      *                 OB2-GL-ESCROW-BALANCE
      *                 OB2-GL-ESCROW-PAYABLE
      * JKC 2025-0403 FORCE MOVE OF ZEROES TO THE FOLLOWING SINCE THEY
      *               WERE COMP-6 SO COULD NOT DO A NUMERIC CHECK:
      *                 OB2-GL-ESCROW-BALANCE
      *                 OB2-GL-ESCROW-PAYABLE
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       COPY "LIBUP/UP1534_SPFSREPORT.CPY".

      * COPYMEMBER: LIBGB/GBFSBR
           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.
      * COPYMEMBER: LIBGB/GBFSB2
           SELECT OB2-FILE ASSIGN TO OB2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OB21-KEY
                  FILE STATUS FILE-STAT.

      * COPYMEMBER: LIBGB/GBFSGP
           SELECT OGP-FILE ASSIGN OGP-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OGP-KEY
                  FILE STATUS FILE-STAT.

      * COPYMEMBER: LIBGB/GBFSGR
           SELECT OGR-FILE ASSIGN TO OGR-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OGR1-KEY
                  FILE STATUS FILE-STAT.

      *====================================================[ TARGET FS ]

      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       COPY "LIBUP/UP1534_SPFDREPORT.CPY".

      *====================================================[ SOURCE FD ]
      * COPYMEMBER: LIBGB/GBFDBR
       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".
      ******************************************************************
      *    BRANCH TRAILER FILE
      ******************************************************************
       FD  OB2-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01B2.CPY" REPLACING LEADING "B2" BY "OB2".

      *=================================================================
      *    LIBGB/GBFDGP --- LAST UPDATED: 2015-1009 JAY
      *=================================================================
      * COPYMEMBER: LIBGB/GBFDGP
       FD  OGP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01GP.CPY" REPLACING LEADING "GP" BY "OGP".

      *=================================================================
      *    LIBGB/GBFDGR --- LAST UPDATED: 2015-1009 JAY
      *=================================================================
      * COPYMEMBER: LIBGB/GBFDGR
       FD  OGR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01GR.CPY" REPLACING LEADING "GR" BY "OGR".


      *====================================================[ TARGET FD ]

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/IF1534.CBL S35 04/03/25-08:52:47 >".

      ******************************************************************
      *
      *    F I L E - P A T H - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBGB/GBWSGR.CPY".

      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(02)     VALUE "PR".
       01  UP1534-REPORT-KEY           PIC 9(11).
       01  UP1534-REPORT-PATH          PIC X(35).

      *====================================================[ SOURCE WS ]
       01  OBR-KEY                     PIC 9(04)     VALUE ZEROES.
       01  OGP-KEY                     PIC 9(01)     VALUE ZEROES.

       01  OBR-PATH                    PIC X(35)     VALUE SPACES.
       01  OB2-PATH                    PIC X(35)     VALUE SPACES.
       01  OGP-PATH                    PIC X(35)     VALUE SPACES.
       01  OGR-PATH                    PIC X(35)     VALUE SPACES.

       01  OB2-RECORD-STAT             PIC X(02)   VALUE SPACES.
           88  OB2-RECORD-STAT-GOOD                VALUE "00".
           88  OB2-RECORD-STAT-BAD                 VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      *====================================================[ TARGET WS ]
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CASE-BUF                    PIC X(04)     VALUE SPACES.

       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  ERRCD-LIST                  PIC X(06)     VALUE "ERRCD.".

       01  EXITPROG                    PIC X(09)     VALUE "UP/UPMENU".
       01  EXITPROG-LIST               PIC X(09)     VALUE "EXITPROG.".

      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  ER-FEED                     PIC 9(01)     VALUE 1.
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  RECORDS-FOUND               PIC X(01).
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(03)     VALUE ZEROES.

      *=================================================[ UPGRADE-STAT ]
       78  UPGRADE-STAT---GOOD                     VALUE "00".
       78  UPGRADE-STAT---BAD                      VALUE "XX".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  UPGRADE-STAT                PIC X(02)   VALUE SPACES.
           88  UPGRADE-STAT-GOOD                   VALUE "00".
           88  UPGRADE-STAT-BAD                    VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER PRINT".
           03  FILLER PIC X(30) VALUE "ER OPTIONS                    ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-SOURCE-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER SOURC".
           03  FILLER PIC X(30) VALUE "E PATH                        ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-TARGET-PATH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TARGET PATH               ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, UP/DOWN, ENTER BEGINNING".
           03  FILLER PIC X(17) VALUE " BRANCH NUMBER   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING BRANCH NUMBER      ".
           03  FILLER PIC X(17) VALUE "                 ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-OPTION-FILES.
           03  FILLER PIC X(30) VALUE "F1 THRU F7, F8-HELP, UP/DOWN, ".
           03  FILLER PIC X(30) VALUE "ENTER (Y)ES/(N)O/(C)HECK FOR U".
           03  FILLER PIC X(17) VALUE "PDATING THIS FILE".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-ACCEPT.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER TO ACCEPT (YES/NO)        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEGIN-PROCESSING.
           03  FILLER PIC X(30) VALUE "PROCESSING....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(17) VALUE ".................".

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
      *    M V A 1 5 - F I L E - W O R K E R S
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
       01  MVA15-OLD-PATH                            PIC X(35).

       01  FILLER                          REDEFINES MVA15-OLD-PATH.
           03  FILLER                                PIC X(13).
           03  MVA15-OLD-BASESH-FILE                 PIC X(22).

       01  FILLER                          REDEFINES MVA15-OLD-PATH.
           03  FILLER                                PIC X(16).
           03  MVA15-OLD-GLOBAL-FILE                 PIC X(19).

       01  FILLER                          REDEFINES MVA15-OLD-PATH.
           03  FILLER                                PIC X(23).
           03  MVA15-OLD-LOCAL-FILE                  PIC X(12).

      *-----------------------------------------------------------------
       01  MVA15-NEW-PATH                            PIC X(35).

       01  FILLER                          REDEFINES MVA15-NEW-PATH.
           03  FILLER                                PIC X(13).
           03  MVA15-NEW-BASESH-O                    PIC X(01).
           03  MVA15-NEW-BASESH-FILE                 PIC X(21).

       01  FILLER                          REDEFINES MVA15-NEW-PATH.
           03  FILLER                                PIC X(16).
           03  MVA15-NEW-GLOBAL-O                    PIC X(01).
           03  MVA15-NEW-GLOBAL-FILE                 PIC X(18).

       01  FILLER                          REDEFINES MVA15-NEW-PATH.
           03  FILLER                                PIC X(23).
           03  MVA15-NEW-LOCAL-O                     PIC X(01).
           03  MVA15-NEW-LOCAL-FILE                  PIC X(11).

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
           03  H2-TITLE             PIC X(45)   VALUE
               "------- INITIALIZE A15 TO A32 REPORT --------".
           03  FILLER               PIC X(44)   VALUE SPACES.

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

       01  RISC2                  PIC X(02) VALUE "R2".
       01  SOURCE-MACHINE         PIC X(02).
       01  TARGET-MACHINE         PIC X(02).
      *=================================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------

           03  CURRENT-MACHINE         PIC X(02).
           03  SOURCE-PATH             PIC X(35).
           03  TARGET-PATH             PIC X(35).

           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).

           03  GROUP-OPTION-WORKERS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

           03  FILE-OPTION-WORKERS.
               05  FILE-OPTION-BR      PIC X(01).
               05  FILE-OPTION-GP      PIC X(01).
               05  FILE-OPTION-GR      PIC X(01).

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
       COPY "LIBUP/UP1534W.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/IF1534_DEF.CPY".
       COPY "LIBUP/IF1534_WKS.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBUP/IF1534_SCN.CPY".

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
                 REPORT-FILE PR-FILE
                 OBR-FILE    OB2-FILE OGP-FILE OGR-FILE.

       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE REPORT-FILE
                 OBR-FILE OB2-FILE OGP-FILE  OGR-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           CALL "C$TOLOWER" USING RISC2, VALUE 02.

      * CURRENT-MACHINE WILL BE RISC1, "R1"
           MOVE CURRENT-MACHINE TO SOURCE-MACHINE TARGET-MACHINE.
           PERFORM PROCESS-FILE-GP.
           PERFORM PROCESS-FILE-BR.
           PERFORM PROCESS-FILE-GR.

      **********************************************************
      * PROCESS RISC2 SOURCE FILES TO MERGE/CONVERT
      **********************************************************
           MOVE "R2" TO SOURCE-MACHINE.
           MOVE "R1" TO TARGET-MACHINE.
           CALL "C$TOLOWER" USING SOURCE-MACHINE, VALUE 02.
           CALL "C$TOLOWER" USING TARGET-MACHINE, VALUE 02.

           PERFORM PROCESS-FILE-BR.

       MAIN-PROGRAM-EXIT.
           PERFORM PRINT-REPORT-MODULE.

           GO TO END-PROGRAM.

      *****************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE "X"                        TO RECORDS-FOUND.
           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "IF1534"                   TO VDU-FORM-NAME.
           MOVE IF1534-QUEUE-POS           TO Q-POS.
           MOVE IF1534-COPIES-POS          TO C-POS.
           MOVE IF1534-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF  C-BUF  P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF ( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES )
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

           MOVE "N"                        TO RECORDS-FOUND.
           PERFORM SEND-STARTING-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ SETUP-HEADER }
           MOVE EXT-CONAME-CONAME          TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE         TO H1-DATE.
           MOVE EXT-CONAME-USER-ID         TO H2-USERID.

      *- - - - - - - - - - - - - - - - - - - - - -{ MAKING TEMP FILES  }
           MOVE MKTEMP-DEFAULT             TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF                 TO UP1534-REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.

           MOVE ZEROES TO UP1534-REPORT-KEY.

           PERFORM OPEN-REPORT-FILE.

       INITIALIZATION-EXIT.
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

           MOVE SOURCE-PATH                TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE TARGET-PATH                TO WR-BUF.
           MOVE TARGET-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF ( UP-KEY )
              GO TO ENTER-OPTION-GR.

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
           MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DIR-ACCESS-BUF.

           STRING VDUBUF, "/", CURRENT-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                TO SOURCE-PATH.

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

           STRING VDUBUF, "/", CURRENT-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.

           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              PERFORM INVALID-FILE-PATH-MESSAGE
              GO TO ENTER-TARGET-PATH.

           MOVE VDUBUF                TO TARGET-PATH.

           IF ( SOURCE-PATH = TARGET-PATH )
              PERFORM INVALID-SAME-PATHS-MESSAGE
              GO TO ENTER-SOURCE-PATH.

      *=================================================================
      *    ENTERING BRANCH NUMBER RANGE.
      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

      *    MOVE "E  A000BEG-BRANCH." TO VDU.
      *    PERFORM SCREENIO.

           GO TO ENTER-ALL-BRANCH.

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

           IF ( UP-KEY           ) AND
              ( GROUP-FLAG = "Y" )
              GO TO ENTER-BEG-BRANCH.

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

      *========================================{ SOURCE & TARGET FILES }
       ENTER-OPTION-BR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET THE OPTIONS FOR BR & GP TO "N"; THIS IS NEEDED
      *    IN ORDER IN VERIFYING IF NEEDING TO FORCE OTHER OPTIONS TO
      *    "N" IF THE BR/GP IS A "Y"; NEED TO CONVERT BR & GP BEFORE
      *    CONVERTING OTHER OPTIONS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO FILE-OPTION-BR.
           MOVE "N" TO FILE-OPTION-GP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-BR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-TARGET-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-BR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-BR.

           IF NOT ( FILE-OPTION-BR = "Y" OR "N" OR "C" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-BR.

      *-----------------------------------------------------------------
       ENTER-OPTION-GP.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-GP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-BR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-GP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-GP.

           IF NOT ( FILE-OPTION-GP = "Y" OR "N" OR "C" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-GP.

      *-----------------------------------------------------------------
       ENTER-OPTION-GR.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-GR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-GP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-GR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-GR.

           IF NOT ( FILE-OPTION-GR = "Y" OR "N" OR "C" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-GR.

       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

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
           MOVE SOURCE-PATH               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A090T-PATH."          TO VDU.
           MOVE TARGET-PATH               TO VDUBUF.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ GLOBAL }
           MOVE "S  A010OPTION-BR."    TO VDU.
           MOVE FILE-OPTION-BR         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-GP."    TO VDU.
           MOVE FILE-OPTION-GP         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-GR."    TO VDU.
           MOVE FILE-OPTION-GR         TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

       EO-EXEC-AUDIT-EXIT.
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
      *    P R O C E S S - F I L E - B R
      *
      *=================================================================
      *    IN  : BR-REC
      *          UP1534-VPI WORKERS
      *          FILE-OPTION-WORKERS
      *
      *    OUT : UP1534-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-BR SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-BR = "N" )
              GO TO PROCESS-FILE-BR-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO BR-PATH-OVERRIDE.
           MOVE "BR"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/BRFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ACCESS-BUF       OBR-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-BR-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO BR-PATH.

           MOVE "GB/B2FILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ACCESS-BUF       OB2-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              OPEN I-O OB2-FILE
              CLOSE OB2-FILE.
            
      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-BR = "C" )
              PERFORM OPEN-OBR-FILE
              PERFORM CLOSE-OBR-FILE
              GO TO PROCESS-FILE-BR-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES TO BR-REC OBR-REC.

           MOVE ZEROES                     TO OBR-KEY.

           PERFORM OPEN-BR-FILE.

           MOVE OB2-PATH TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH TO OB2-PATH.
           PERFORM OPEN-OB21-FILE.

           MOVE OBR-PATH TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH TO OBR-PATH.
           PERFORM OPEN-OBR-FILE.
           PERFORM START-OBR-FILE.

      *=================================================================
       PROCESS-FILE-BR-NEXT.

           PERFORM READ-OBR-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-BR-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      * DO NOT OVERWRITE THE BRANCH 9990 FROM RISC1!!!!!
           IF OBR-NO = 9990 AND OBR-MACHINE = RISC2
              GO TO PROCESS-FILE-BR-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ GET MERGE RECORD }
           MOVE OBR-NO                     TO OB2-BRNO.
           PERFORM READ-OB21-FILE.
           IF ( IO-BAD )
              MOVE STAT---BAD              TO OB2-RECORD-STAT
           ELSE
              MOVE STAT---GOOD             TO OB2-RECORD-STAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ VALID RECORD }
           PERFORM CLEAR-BR-FILE.

           MOVE OBR-KEY                    TO BR-NO.
           MOVE 0                          TO BR-LTOUCH-DATE.
           MOVE OBR-NAME                   TO BR-NAME.
           MOVE OBR-ADR1                   TO BR-ADR1.
           MOVE OBR-ADR2                   TO BR-ADR2.
           MOVE OBR-CITY                   TO BR-CITY.
           MOVE OBR-STATE                  TO BR-STATE.
           MOVE OBR-ZIP                    TO BR-ZIP.
           MOVE OBR-PHONE                  TO BR-PHONE.
           MOVE OBR-RETAIN                 TO BR-RETAIN.
           MOVE OBR-NEXT-CHECK             TO BR-NEXT-CHECK.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                   MOVE OBR-PRINTER(SUB)   TO BR-PRINTER(SUB)
           END-PERFORM.

           MOVE OBR-FEE-GL-BRNO            TO BR-FEE-GL-BRNO.
           MOVE OBR-REMIT1                 TO BR-REMIT1.
           MOVE OBR-REMIT2                 TO BR-REMIT2.
           MOVE OBR-REMIT3                 TO BR-REMIT3.
           MOVE OBR-STMNT1                 TO BR-STMNT1.
           MOVE OBR-STMNT2                 TO BR-STMNT2.

           IF OBR-INS-PRODUCER-NUM NOT NUMERIC
              MOVE 0                       TO BR-INS-PRODUCER-NUM
           ELSE
           MOVE OBR-INS-PRODUCER-NUM       TO BR-INS-PRODUCER-NUM.

           MOVE OBR-DLRCHK                 TO BR-DLRCHK.
           MOVE OBR-FRONT-END-FG           TO BR-FRONT-END-FG.
           MOVE OBR-EMLNRG-PHONE           TO BR-EMLNRG-PHONE.
           MOVE OBR-POST-GL                TO BR-POST-GL.
           MOVE OBR-ALLOTMNTS              TO BR-ALLOTMNTS.
           MOVE OBR-EXTRACT-AUTO-UPD       TO BR-EXTRACT-AUTO-UPD.
           MOVE OBR-PCNI-LEDGER            TO BR-PCNI-LEDGER.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                   MOVE OBR-PRINTERFG(SUB) TO BR-PRINTERFG(SUB)
           END-PERFORM.

           MOVE OBR-NEGDLRSV               TO BR-NEGDLRSV.
           MOVE OBR-CPCORP-ORDER           TO BR-CPCORP-ORDER.
           MOVE OBR-PRIO-MEMO-FLAG         TO BR-PRIO-MEMO-FLAG.
           MOVE OBR-PREBILL-PL-JD-OPT      TO BR-PREBILL-PL-JD-OPT.
           MOVE OBR-LOAN-NUM-FLAG          TO BR-LOAN-NUM-FLAG.
           MOVE OBR-DOWNPY-FLAG            TO BR-DOWNPY-FLAG.
           MOVE OBR-AUTO-INS-EARN          TO BR-AUTO-INS-EARN.
           MOVE OBR-OTH-INS-LEVELS         TO BR-OTH-INS-LEVELS.
           MOVE OBR-ALLOW-INTERBR          TO BR-ALLOW-INTERBR.
           MOVE OBR-UNER-DLANRP            TO BR-UNER-DLANRP.
           MOVE OBR-SECOND-PROMISE         TO BR-SECOND-PROMISE.
           MOVE OBR-AVAIL-CREDIT           TO BR-AVAIL-CREDIT.
           MOVE OBR-RECP1                  TO BR-RECP1.
           MOVE OBR-RECP2                  TO BR-RECP2.
           MOVE OBR-INC-WAIVERS            TO BR-INC-WAIVERS.
           MOVE OBR-INC-INS                TO BR-INC-INS.
           IF OBR-BP-NEXT-BATCH NOT NUMERIC
              MOVE 0                       TO BR-BP-NEXT-BATCH
           ELSE
           MOVE OBR-BP-NEXT-BATCH          TO BR-BP-NEXT-BATCH.
           MOVE OBR-BP-NOPAYOFF            TO BR-BP-NOPAYOFF.
           MOVE OBR-BP-ODDPAY              TO BR-BP-ODDPAY.
           MOVE OBR-BP-LC                  TO BR-BP-LC.
           MOVE OBR-BP-NOPRIN              TO BR-BP-NOPRIN.
           MOVE OBR-BP-FLASH               TO BR-BP-FLASH.
           MOVE OBR-BP-ACTIONCD            TO BR-BP-ACTIONCD.
           MOVE OBR-BP-MAXCON              TO BR-BP-MAXCON.
           MOVE OBR-BP-MAXREC              TO BR-BP-MAXREC.
           MOVE OBR-BP-PL                  TO BR-BP-PL.
           MOVE OBR-BP-JD                  TO BR-BP-JD.
           MOVE OBR-BP-BRANCH              TO BR-BP-BRANCH.
           MOVE OBR-AGE-ERN                TO BR-AGE-ERN.
           MOVE OBR-OTHER-BRANCH-GL        TO BR-OTHER-BRANCH-GL.
           MOVE OBR-SALES-SMALL            TO BR-SALES-SMALL.
           MOVE OBR-NEW-COLL-INQUIRY       TO BR-NEW-COLL-INQUIRY.
           MOVE OBR-CHANGE-MEMO            TO BR-CHANGE-MEMO.
           MOVE OBR-INS-ADDON              TO BR-INS-ADDON.
           MOVE OBR-TRAILER-REC            TO BR-TRAILER-REC.
           MOVE OBR-ALDEL-FLAG             TO BR-ALDEL-FLAG.
           MOVE OBR-IB-LCHG-MESS           TO BR-IB-LCHG-MESS.
           MOVE OBR-COL-QUEUE-PAST-DUE     TO BR-COL-QUEUE-PAST-DUE.
           MOVE OBR-LI-DLCOMM-CMSI-PP-MAX  TO BR-LI-DLCOMM-CMSI-PP-MAX.
           MOVE OBR-ADDON-FEECD-O2(1)      TO BR-ADDON-FEECD-O2(1).
           MOVE OBR-ADDON-FEECD-O2(2)      TO BR-ADDON-FEECD-O2(2).
           MOVE OBR-AUTO-CHECK             TO BR-AUTO-CHECK.
           MOVE OBR-RECEIPT-PROMDATE       TO BR-RECEIPT-PROMDATE.
           MOVE OBR-RECEIPT-INTPD          TO BR-RECEIPT-INTPD.
           MOVE OBR-COLL-ID-SCAN           TO BR-COLL-ID-SCAN.
           MOVE OBR-STOP-ON-COLL           TO BR-STOP-ON-COLL.
           MOVE OBR-DUEDATE-CHANGE-CD      TO BR-DUEDATE-CHANGE-CD.
           MOVE OBR-CHECK-EOM-FG           TO BR-CHECK-EOM-FG.
           MOVE OBR-VOLUME-DLANRP          TO BR-VOLUME-DLANRP.
           MOVE OBR-PDTH-NXDUE             TO BR-PDTH-NXDUE.
           MOVE OBR-LC-CODE                TO BR-LC-CODE.
           MOVE OBR-VALIDATE-PRUS          TO BR-VALIDATE-PRUS.
           MOVE OBR-AUTO-OPEN-BAL          TO BR-AUTO-OPEN-BAL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9
              MOVE OBR-OPEN-BAL(SUB)       TO BR-OPEN-BAL(SUB)
           END-PERFORM.
      * ADDED MORE OCCURS TO BR-OPEN-BAL (NOW OCCURS 20)
           PERFORM VARYING SUB FROM 10 BY 1 UNTIL SUB > 20
              MOVE 0                       TO BR-OPEN-BAL(SUB).

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
                   MOVE OBR-TENDER-CD(SUB) TO BR-TENDER-CD(SUB)
           END-PERFORM.

           MOVE OBR-DLPARV-ADJ-FG          TO BR-DLPARV-ADJ-FG.
           MOVE OBR-OVERRIDE-BR-STATE      TO BR-OVERRIDE-BR-STATE.
           MOVE OBR-DEFAULT-SC-CLASS       TO BR-DEFAULT-SC-CLASS.
           MOVE OBR-RECDEL-NOT-REC         TO BR-RECDEL-NOT-REC.
           MOVE OBR-POST-AHEAD-FG          TO BR-POST-AHEAD-FG.
           MOVE OBR-CAN-LOANS-FG           TO BR-CAN-LOANS-FG.
           MOVE OBR-NON-MONTHLY-CAN-LOANS  TO BR-NON-MONTHLY-CAN-LOANS.
           MOVE OBR-BP-ALLOW-PMT-ACCT-OTHBAL TO
                                           BR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE OBR-LBOX-ALLOW-PMT-ACCT-OTHBAL TO
                                         BR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
      * CLEANSE - KM SAID THEY DO NOT USE AND I WAS FINDING 2020 IN HEXDMP
           MOVE 0                          TO BR-REGION-NUMBER.
           MOVE OBR-OUR-FEE                TO BR-OUR-FEE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE OBR-REB-METHOD    (SUB) TO BR-REB-METHOD    (SUB)
              MOVE OBR-REBDAYS-MONTH (SUB) TO BR-REBDAYS-MONTH (SUB)
              IF OBR-ACCRUAL-MONTHS(SUB) NOT NUMERIC
                 MOVE 0                    TO BR-ACCRUAL-MONTHS(SUB)
              ELSE
              MOVE OBR-ACCRUAL-MONTHS(SUB) TO BR-ACCRUAL-MONTHS(SUB)
              END-IF
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IN A15, METHOD2(1) AND METHOD2(4) ARE USED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE OBR-REB-METHOD-2(1)      TO BR-REB-METHOD(6).
           MOVE OBR-REBDAYS-MONTH-2(1)   TO BR-REBDAYS-MONTH(6).
           MOVE OBR-ACCRUAL-MONTHS-2(1)  TO BR-ACCRUAL-MONTHS(6).

           MOVE OBR-REB-METHOD-2(4)      TO BR-REB-METHOD(7).
           MOVE OBR-REBDAYS-MONTH-2(4)   TO BR-REBDAYS-MONTH(7).
           MOVE OBR-ACCRUAL-MONTHS-2(4)  TO BR-ACCRUAL-MONTHS(7).

           PERFORM VARYING SUB FROM 8 BY 1 UNTIL SUB > 11
              MOVE ZEROES                TO BR-REB-METHOD(SUB)
              MOVE ZEROES                TO BR-REBDAYS-MONTH(SUB)
              MOVE ZEROES                TO BR-ACCRUAL-MONTHS(SUB)
           END-PERFORM.

           MOVE OBR-GLOVERSHORT            TO BR-GLOVERSHORT.
           MOVE OBR-GLBANK                 TO BR-GLBANK.
           MOVE OBR-GLONHAND               TO BR-GLONHAND.
           MOVE OBR-BANKBAL                TO BR-BANKBAL.
           MOVE OBR-RETAIN-AMT             TO BR-RETAIN-AMT.
           MOVE OBR-LCARDFG                TO BR-LCARDFG.
           MOVE OBR-DATA-PATH              TO BR-DATA-PATH.
           MOVE OBR-MACHINE                TO BR-MACHINE.
           MOVE OBR-MANAGER                TO BR-MANAGER.
           MOVE OBR-EOD-PROCEDURE          TO BR-EOD-PROCEDURE.
           MOVE OBR-BP-PAYOFF-NONCASH      TO BR-BP-PAYOFF-NONCASH.
           MOVE OBR-BP-PAYOFF-OV           TO BR-BP-PAYOFF-OV.
           MOVE OBR-LI-DLCOMM-CMSI-O3-MAX  TO BR-LI-DLCOMM-CMSI-O3-MAX.
           MOVE OBR-LR-GROUP-COLLID        TO BR-LR-GROUP-COLLID.
           MOVE OBR-UNITPER-FG             TO BR-UNITPER-FG.
           MOVE OBR-UNITPER-Y-D-FG         TO BR-UNITPER-Y-D-FG.
           MOVE OBR-NO-PROMPT-FOR-OTHBR    TO BR-NO-PROMPT-FOR-OTHBR.
           MOVE OBR-WORLD-BRANCH-TYPE      TO BR-WORLD-BRANCH-TYPE.
           MOVE OBR-COL-QUEUE-BEHAVIOR-FG  TO BR-COL-QUEUE-BEHAVIOR-FG.
           MOVE OBR-CONVERT-AUTO-ACCTNO    TO BR-CONVERT-AUTO-ACCTNO.
           MOVE OBR-TS-REPO-INS-FG         TO BR-TS-REPO-INS-FG.
           MOVE OBR-BOOK-SF-BR-ONLY        TO BR-BOOK-SF-BR-ONLY.
           MOVE OBR-FORCE-VALID-CITY-COUNTY
              TO BR-FORCE-VALID-CITY-COUNTY.
           MOVE OBR-SET-COLLCD-DL-MFGCD    TO BR-SET-COLLCD-DL-MFGCD.
           MOVE OBR-DEFAULT-SF-UNITPER-CD  TO BR-DEFAULT-SF-UNITPER-CD.
           MOVE OBR-DEFAULT-SF-PURCD       TO BR-DEFAULT-SF-PURCD.
           MOVE OBR-DEFAULT-LN-UNITPER-CD  TO BR-DEFAULT-LN-UNITPER-CD.
           MOVE OBR-DEFAULT-LN-PURCD       TO BR-DEFAULT-LN-PURCD.
           MOVE OBR-TAP-OTHER-DL           TO BR-TAP-OTHER-DL.
           MOVE OBR-AH-AND-CL              TO BR-AH-AND-CL.
           MOVE OBR-NO-CASH-PROCEEDS       TO BR-NO-CASH-PROCEEDS.
           MOVE OBR-RI-BEFORE-REPO         TO BR-RI-BEFORE-REPO.
           MOVE OBR-RI-BEFORE-P4P5         TO BR-RI-BEFORE-P4P5.
           MOVE OBR-NOT-OURS-REFUND        TO BR-NOT-OURS-REFUND.
           MOVE OBR-SFDLO3-WINDOW-FG       TO BR-SFDLO3-WINDOW-FG.
           MOVE OBR-CALC-INS-ADJ           TO BR-CALC-INS-ADJ.
           MOVE OBR-AC-BEFORE-REPO         TO BR-AC-BEFORE-REPO.
           MOVE OBR-AC-BEFORE-P4P5         TO BR-AC-BEFORE-P4P5.
           IF OBR-BANK-ROUTING-NO NOT NUMERIC OR
               (OBR-BANK-ROUTING-NO = 202020202 OR 020202020)
              MOVE 0                       TO BR-BANK-ROUTING-NO
           ELSE
           MOVE OBR-BANK-ROUTING-NO        TO BR-BANK-ROUTING-NO.
           MOVE OBR-BANK-CLEARING-GL       TO BR-BANK-CLEARING-GL.
           MOVE OBR-GLBANK-DEPOSIT         TO BR-GLBANK-DEPOSIT.
           MOVE OBR-BANK-ACCTNO            TO BR-BANK-ACCTNO.
           MOVE OBR-DEFAULT-SC-SUBCLASS    TO BR-DEFAULT-SC-SUBCLASS.
           MOVE OBR-NO-OUT-DLR-RSV         TO BR-NO-OUT-DLR-RSV.
           MOVE OBR-COLLECTOR-QUEUE        TO BR-COLLECTOR-QUEUE.
           MOVE OBR-COLL-QUEUE-CON-REC     TO BR-COLL-QUEUE-CON-REC.
           MOVE OBR-JE-ALLOCATION-SKIP     TO BR-JE-ALLOCATION-SKIP.
           MOVE OBR-EXPAND-APP-INPUT       TO BR-EXPAND-APP-INPUT.
           MOVE OBR-A50-CPCORP             TO BR-A50-CPCORP.
           MOVE OBR-INBR-BILNSC-FG         TO BR-INBR-BILNSC-FG.
           MOVE OBR-INBR-BRIEF-FG          TO BR-INBR-BRIEF-FG.
           MOVE OBR-LEDGER-JUST-INTCHG     TO BR-LEDGER-JUST-INTCHG.
           MOVE OBR-BYINQ-START-PAGE       TO BR-BYINQ-START-PAGE.
           MOVE OBR-DSINQ-START-PAGE       TO BR-DSINQ-START-PAGE.
           MOVE OBR-APP-REQUIRED-FOR-INPUT TO BR-APP-REQUIRED-FOR-INPUT.
           MOVE OBR-CREDIT-SCORING-FG      TO BR-CREDIT-SCORING-FG.
           MOVE OBR-COL-QUEUE-LOW-TO-HI    TO BR-COL-QUEUE-LOW-TO-HI.
           MOVE OBR-CAN-GROUP-ID           TO BR-CAN-GROUP-ID.
           MOVE OBR-AUTO-LNSTAT-WINDOW     TO BR-AUTO-LNSTAT-WINDOW.
      * THE CENTRAL TIME BRANCHES ARE SET TO 0.  THE MOUNTAIN TIME BRANCHES
      * ARE SET TO -1.  SO ALL R2 BRANCHES NEED TO INCREMENT THAT FIELD
      * BY -1 SO THE CENTRAL TIME BRANCHES ARE -1 AND THE MOUNTAIN TIME
      * BRANCHES ARE -2, SINCE THE O/S TIME WILL BE EST.
           MOVE OBR-TIME-ZONE-FACTOR       TO BR-TIME-ZONE-FACTOR.
           IF (OBR-MACHINE = RISC2)
              IF OBR-TIME-ZONE-FACTOR = 0
                 MOVE -1 TO BR-TIME-ZONE-FACTOR
              ELSE
              IF OBR-TIME-ZONE-FACTOR = -1
                 MOVE -2 TO BR-TIME-ZONE-FACTOR
              ELSE
              IF OBR-TIME-ZONE-FACTOR = -2
                 MOVE -3 TO BR-TIME-ZONE-FACTOR.
           MOVE OBR-LONPF-OPT-DISPLAY      TO BR-LONPF-OPT-DISPLAY.
           MOVE OBR-GIFTNET                TO BR-GIFTNET.
           MOVE OBR-GIFTNET-LEDGER-LINE    TO BR-GIFTNET-LEDGER-LINE.
           MOVE OBR-NO-FT-EMP              TO BR-NO-FT-EMP.
           MOVE OBR-NO-PT-EMP              TO BR-NO-PT-EMP.
           MOVE OBR-BALLOON-LOANS-FG       TO BR-BALLOON-LOANS-FG.
           MOVE OBR-GL-TYPE-ALLOWED        TO BR-GL-TYPE-ALLOWED.
           MOVE OBR-GL-ENTRY-DEFAULT       TO BR-GL-ENTRY-DEFAULT.
           MOVE OBR-MIDSTREAM-1STPAY-FG    TO BR-MIDSTREAM-1STPAY-FG.
           MOVE OBR-NODEF-1ST-X-MONTHS     TO BR-NODEF-1ST-X-MONTHS.
           MOVE OBR-ADD-INS-FEECODE        TO BR-ADD-INS-FEECODE.
           MOVE OBR-ROAD-G-SGL-FEECODE     TO BR-ROAD-G-SGL-FEECODE.
           MOVE OBR-ROAD-G-FAM-FEECODE     TO BR-ROAD-G-FAM-FEECODE.
           MOVE OBR-NODEF-IF-ZERO-AMT      TO BR-NODEF-IF-ZERO-AMT.
           MOVE OBR-ASSESS-LATE-CHGS       TO BR-ASSESS-LATE-CHGS.
           MOVE OBR-NEXT-SOLICIT-NO        TO BR-NEXT-SOLICIT-NO.
           MOVE OBR-ADDON-FEECD-O3(1)      TO BR-ADDON-FEECD-O3(1).
           MOVE OBR-ADDON-FEECD-O3(2)      TO BR-ADDON-FEECD-O3(2).
           MOVE OBR-IMMEDIATE-RECEIPT      TO BR-IMMEDIATE-RECEIPT.
           MOVE OBR-IMMEDIATE-DOC-PRINT    TO BR-IMMEDIATE-DOC-PRINT.
           MOVE OBR-AUTO-PL-REF-AC         TO BR-AUTO-PL-REF-AC.
           MOVE OBR-FAX-NUMBER             TO BR-FAX-NUMBER.
           MOVE OBR-ALPHA-DRAWERS          TO BR-ALPHA-DRAWERS.
           MOVE OBR-LATECHG-ON-ALLOTMENTS  TO BR-LATECHG-ON-ALLOTMENTS.
           MOVE OBR-ALLOW-SYS-AS-EFFDATE   TO BR-ALLOW-SYS-AS-EFFDATE.
           MOVE OBR-TAX-PREBOOK-CUST       TO BR-TAX-PREBOOK-CUST.
           MOVE OBR-TAX-PREBOOK-NONCUST    TO BR-TAX-PREBOOK-NONCUST.
           MOVE OBR-PASSWORD-DRAWER        TO BR-PASSWORD-DRAWER.
           MOVE OBR-ONESTEP-RN-FG          TO BR-ONESTEP-RN-FG.
           MOVE OBR-LI-LISCAN-FG           TO BR-LI-LISCAN-FG.
           MOVE OBR-HIDE-END-BAL           TO BR-HIDE-END-BAL.
           MOVE OBR-LBOX-NOPAYOFF          TO BR-LBOX-NOPAYOFF.
           MOVE OBR-LBOX-ODDPAY            TO BR-LBOX-ODDPAY.
           MOVE OBR-LBOX-LC                TO BR-LBOX-LC.
           MOVE OBR-LBOX-NOPRIN            TO BR-LBOX-NOPRIN.
           MOVE OBR-LBOX-FLASH             TO BR-LBOX-FLASH.
           MOVE OBR-LBOX-ACTIONCD          TO BR-LBOX-ACTIONCD.
           IF OBR-LBOX-MAXCON NOT NUMERIC OR
               (OBR-LBOX-MAXCON = 202 OR 020)
              MOVE 0                       TO BR-LBOX-MAXCON
           ELSE
           MOVE OBR-LBOX-MAXCON            TO BR-LBOX-MAXCON.
           IF OBR-LBOX-MAXREC NOT NUMERIC OR
               (OBR-LBOX-MAXREC = 202 OR 020)
              MOVE 0                       TO BR-LBOX-MAXREC
           ELSE
           MOVE OBR-LBOX-MAXREC            TO BR-LBOX-MAXREC.
           MOVE OBR-LBOX-PL                TO BR-LBOX-PL.
           MOVE OBR-LBOX-JD                TO BR-LBOX-JD.
           MOVE OBR-LBOX                   TO BR-LBOX.
           MOVE OBR-LATECHG-ON-LBOX        TO BR-LATECHG-ON-LBOX.
           MOVE OBR-ALLOW-OV-TRCD          TO BR-ALLOW-OV-TRCD.
           MOVE OBR-GLOBAL-QUEUE           TO BR-GLOBAL-QUEUE.
           MOVE OBR-LBOX-PAYOFF-NONCASH    TO BR-LBOX-PAYOFF-NONCASH.
           MOVE OBR-LBOX-PAYOFF-OV         TO BR-LBOX-PAYOFF-OV.

           IF ( OB2-RECORD-STAT-BAD )
               GO TO PROCESS-FILE-BR-WRITE.

           MOVE OB2-COLLECT-MANAGER        TO BR-COLLECT-MANAGER.
           MOVE OB2-COLLECT-PHONE          TO BR-COLLECT-PHONE.
           MOVE OB2-COLLECT-EXT            TO BR-COLLECT-EXT.
           MOVE OB2-ALT-INTO-THIS-BRANCH   TO BR-ALT-INTO-THIS-BRANCH.
           MOVE OB2-POFF-TITLE-WINDOW      TO BR-POFF-TITLE-WINDOW.
           MOVE OB2-COUPON-STATEMENT       TO BR-COUPON-STATEMENT.
           MOVE OB2-SUPERVISOR             TO BR-SUPERVISOR.
           MOVE OB2-AREA-MANAGER           TO BR-AREA-MANAGER.
           IF OB2-MIN-MONTHS-DEF NOT NUMERIC
              MOVE 0                       TO BR-MIN-MONTHS-DEF
           ELSE
           MOVE OB2-MIN-MONTHS-DEF         TO BR-MIN-MONTHS-DEF.
           MOVE OB2-CC-FLAG                TO BR-CC-FLAG.
           MOVE OB2-CC-ADR1                TO BR-CC-ADR1.
           MOVE OB2-CC-CITY                TO BR-CC-CITY.
           MOVE OB2-CC-STATE               TO BR-CC-STATE.
           MOVE OB2-CC-ZIP                 TO BR-CC-ZIP.
           IF OB2-MIN-MONTHS-D2 NOT NUMERIC
              MOVE 0                       TO BR-MIN-MONTHS-D2
           ELSE
           MOVE OB2-MIN-MONTHS-D2          TO BR-MIN-MONTHS-D2.
           MOVE OB2-SKIP-MIDSTREAM-TRW     TO BR-SKIP-MIDSTREAM-TRW.

           MOVE 0                          TO BR-GL-ESCROW-BALANCE.
           MOVE 0                          TO BR-GL-ESCROW-PAYABLE.
           MOVE OB2-ALLOW-FORCE-OPEN-FG    TO BR-ALLOW-FORCE-OPEN-FG.
           IF OB2-ACH-BANK-NO NOT NUMERIC
              MOVE 0                       TO BR-ACH-BANK-NO
           ELSE
           MOVE OB2-ACH-BANK-NO            TO BR-ACH-BANK-NO.
           MOVE OB2-WIMENU-CHECK-WRITING   TO BR-WIMENU-CHECK-WRITING.
           MOVE OB2-ADP-PAY-GROUP          TO BR-ADP-PAY-GROUP.
           MOVE OB2-ALLOW-SCANNED-CKS      TO BR-ALLOW-SCANNED-CKS.
           MOVE OB2-CHECK-DEPOSIT-GL       TO BR-CHECK-DEPOSIT-GL.
           MOVE OB2-OPTION-PST-LASER       TO BR-OPTION-PST-LASER.
           MOVE OB2-USE-DEBIT-CARDS        TO BR-USE-DEBIT-CARDS.
           MOVE OB2-GL-DEBIT-CARD-PROCEEDS TO BR-GL-DEBIT-CARD-PROCEEDS.
           IF OB2-WKDAY-BANKID NOT NUMERIC OR
             (OB2-WKDAY-BANKID = 2020 OR 0202)
             MOVE 0                        TO BR-WKDAY-BANKID
           ELSE
           MOVE OB2-WKDAY-BANKID           TO BR-WKDAY-BANKID.
           IF OB2-VERITEC-LICENSE NOT NUMERIC OR
             (OB2-VERITEC-LICENSE = 2020202 OR 0202020)
             MOVE 0                        TO BR-VERITEC-LICENSE
           ELSE
             MOVE OB2-VERITEC-LICENSE      TO BR-VERITEC-LICENSE.
           MOVE OB2-TEST-PP-AUTO-INSCOVR   TO BR-TEST-PP-AUTO-INSCOVR.
           MOVE OB2-USE-NEW-WFARGO-ACCT    TO BR-USE-NEW-WFARGO-ACCT.
           MOVE OB2-BRANCH-CLOSED          TO BR-BRANCH-CLOSED.
           MOVE OB2-ALLOW-F3-ALL-DOCS      TO BR-ALLOW-F3-ALL-DOCS.

       PROCESS-FILE-BR-WRITE.
      *- - - - - - - - - - - - - - - - - - - - - - - - -{ WRITE RECORD }
           PERFORM WRITE-BR-FILE.
           IF ( IO-BAD )
      * ONLY REWRITE THE BRANCH RECORDS THAT ARE RISC2 AND A "R2" BRANCH
              IF ( FILE-STAT-DISP = "22,00" ) 
                 IF (SOURCE-MACHINE = RISC2) AND (BR-MACHINE = RISC2)
                    PERFORM REWRITE-BR-FILE
                 ELSE
                    GO TO PROCESS-FILE-BR-NEXT
      * ONLY ERROR OUT IF NOT A REWRITE
              ELSE
                 ADD 1                     TO UP1534-RECORD-COUNTER-SKIP
                 MOVE BR-NO               TO UP1534-UPDATE-KEY
                 MOVE FILE-STAT-DISP       TO UP1534-UPDATE-STAT
                 MOVE "E"                  TO UP1534-UPDATE-TYPE
                 PERFORM UP1534-CREATE-FILE-UPDATE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-BR-NEXT.

      *=================================================================
       PROCESS-FILE-BR-END.

           PERFORM CLOSE-OB21-FILE.
           MOVE OB2-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

           PERFORM CLOSE-OBR-FILE.
           MOVE OBR-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

           PERFORM CLOSE-BR-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

      *-----------------------------------------------------------------
       PROCESS-FILE-BR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - G P
      *
      *=================================================================
      *    IN  : BR-REC
      *          UP1534-VPI WORKERS
      *          FILE-OPTION-WORKERS
      *
      *    OUT : UP1534-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-GP SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-GP = "N" )
              GO TO PROCESS-FILE-GP-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO GP-PATH-OVERRIDE.
           MOVE "GP"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/GPFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ACCESS-BUF       OGP-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-GP-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO GP-PATH.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-GP = "C" )
              PERFORM OPEN-OGP-FILE
              PERFORM CLOSE-OGP-FILE
              GO TO PROCESS-FILE-GP-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WORLD ONLY USES GP-KEY #1 (STANDARD) & #2 (ACCOUNTS PAYABLE).
      *    FOR A32 & A34, GP-KEY #1 & #2 ARE MERGED INTO ONE RECORD
      *    WHICH IS ALL IN GP-KEY #1 NOW.
      *    FOR CONVERTING A30 TO A32, NEED TO DO THE FOLLOWING:
      *      1. OPEN A15 & A32 GPFILE
      *      2. CLEAR A32 GP-REC (CLEAR ROUTINE FROM GB/GPMAN1)
      *      3. READ A15 GP-KEY #1
      *      4. MOVE A15 GP-REC TO A32 GP-REC
      *      5. READ A15 GP-KEY #2
      *      6. MOVE A15 GP-REC TO A32 GP-REC
      *      7. WRITE A32 GP-REC
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - -{ 1. OPEN A15 & A32 GPFILE }
           PERFORM OPEN-GP-FILE.

           MOVE OGP-PATH TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH TO OGP-PATH.
           PERFORM OPEN-OGP-FILE.

           PERFORM CLEAR-GP-FILE.

      *- - - - - - - - - - - - - - - - - - - -{ 3. READ A30 GP-KEY #1 }
           MOVE 1 TO OGP-KEY.
           PERFORM READ-OGP-FILE.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GP-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      *- - - - - - - - - - - - - - -{ 4. MOVE A15 GP-REC TO A32 GP-REC }
           MOVE 1                          TO GP-KEY.

           MOVE OGP-BEG-FISCAL        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD         TO GP-BEG-FISCAL-DATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             MOVE OGP-END-FISCAL(SUB) TO DATE-MMDDYY
             PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
             MOVE DATE-YYYYMMDD       TO GP-END-FISCAL-DATE(SUB)
           END-PERFORM.

           MOVE OGP-BANK-GL                TO GP-BANK-GL.
           MOVE OGP-BANK-CLEARING-GL       TO GP-BANK-CLEARING-GL.
           MOVE OGP-BANK-ROUTING-NO        TO GP-BANK-ROUTING-NO.
           MOVE OGP-BANK-ACCTNO            TO GP-BANK-ACCTNO.
           MOVE OGP-TR-RETAIN-MONTHS       TO GP-TR-RETAIN-MONTHS.
           MOVE OGP-FED-TAX-ID             TO GP-FED-TAX-ID.
           MOVE OGP-CORP-NAME              TO GP-CORP-NAME.
           MOVE OGP-SHORT-NAME             TO GP-SHORT-NAME.
           MOVE OGP-BANK-NAME              TO GP-BANK-NAME.
           MOVE OGP-BANK-DEPOSIT-GL        TO GP-BANK-DEPOSIT-GL.
           MOVE OGP-AGE-BUCKET-CD          TO GP-AGE-BUCKET-CD.
           MOVE OGP-THE-ONLY-SF-SRCD       TO GP-THE-ONLY-SF-SRCD.
           MOVE OGP-ALLOW-BATCH-CHECKS     TO GP-ALLOW-BATCH-CHECKS.
           MOVE OGP-DLRVPRIN-3-FG          TO GP-DLRVPRIN-3-FG.
           MOVE OGP-DLRVCHGS-3-FG          TO GP-DLRVCHGS-3-FG.
           MOVE OGP-CASH-METHOD-ONLY       TO GP-CASH-METHOD-ONLY.
           MOVE OGP-IB-ACCRUALS-DONE       TO GP-IB-ACCRUALS-DONE.
           MOVE OGP-EOD-RETAIN-MONTHS      TO GP-EOD-RETAIN-MONTHS.
           MOVE OGP-NO-LC-IN-ARREARAGE-FG  TO GP-NO-LC-IN-ARREARAGE-FG.
           MOVE OGP-DEFAULT-O3-INS-COMPANY TO GP-DEFAULT-O3-INS-COMPANY.
           MOVE OGP-STOP-ON-PRIOR-FG       TO GP-STOP-ON-PRIOR-FG.
           MOVE OGP-APPLICATION-FG         TO GP-APPLICATION-FG.
           MOVE OGP-DEFAULT-ACCRATE        TO GP-DEFAULT-ACCRATE.
           MOVE OGP-DLR-DISP-TYPE          TO GP-DLR-DISP-TYPE.
           MOVE OGP-ALLOW-RLOGINS          TO GP-ALLOW-RLOGINS.
           MOVE OGP-AUTO-REPO-WINDOW       TO GP-AUTO-REPO-WINDOW.
           MOVE OGP-ALLOW-PI-WITH-SE-BAL   TO GP-ALLOW-PI-WITH-SE-BAL.
           MOVE OGP-ALLOW-MORE-COLLECT     TO GP-ALLOW-MORE-COLLECT.
           MOVE OGP-BATCH-MORE-THAN-DAY    TO GP-BATCH-MORE-THAN-DAY.
           MOVE OGP-PRINT-GL-RECAP         TO GP-PRINT-GL-RECAP.
           MOVE OGP-LOAN-CONVERSION-UPD-FG TO GP-LOAN-CONVERSION-UPD-FG.
           MOVE OGP-FORM-CHG-CD            TO GP-FORM-CHG-CD.
           MOVE OGP-PROGRP-PRINT-REDEEM    TO GP-PROGRP-PRINT-REDEEM.
           MOVE OGP-BW-HOUSING-CODE        TO GP-BW-HOUSING-CODE.
           MOVE OGP-BW-CREDIT-SCORE        TO GP-BW-CREDIT-SCORE.
           MOVE OGP-DISPLAY-BANK-BAL       TO GP-DISPLAY-BANK-BAL.
           MOVE OGP-PROGRP-DL-CONT         TO GP-PROGRP-DL-CONT.
           MOVE OGP-DLRVPRIN-2-FG          TO GP-DLRVPRIN-2-FG.
           MOVE OGP-DLRVPRIN-1-FG          TO GP-DLRVPRIN-1-FG.
           MOVE OGP-ADV-TO-1ST-NO-CORR     TO GP-ADV-TO-1ST-NO-CORR.
           MOVE OGP-COMPANY-FLAG           TO GP-COMPANY-FLAG.
           MOVE OGP-INTERBR-AR-AP-FULLGL   TO GP-INTERBR-AR-AP-FULLGL.
           MOVE OGP-SKIP-BI-UPDATE         TO GP-SKIP-BI-UPDATE.
           MOVE OGP-ASK-HARD-COPY-MEMOS    TO GP-ASK-HARD-COPY-MEMOS.
           MOVE OGP-RLOGIN-LOCAL-PROFILE   TO GP-RLOGIN-LOCAL-PROFILE.
           MOVE OGP-RLOGIN-REMOTE-PROFILE  TO GP-RLOGIN-REMOTE-PROFILE.
           MOVE OGP-REPODATE-PAYDATE       TO GP-REPODATE-PAYDATE.
           MOVE OGP-POST-INTO-FUTURE       TO GP-POST-INTO-FUTURE.
           MOVE OGP-RENEWAL-COLLATERAL     TO GP-RENEWAL-COLLATERAL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF OGP-PROGRP-LN-CLASS(SUB) NOT NUMERIC
                MOVE 0 TO GP-PROGRP-LN-CLASS(SUB)
             ELSE
                MOVE OGP-PROGRP-LN-CLASS(SUB) TO
                                              GP-PROGRP-LN-CLASS(SUB)
             END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
             IF OGP-PROGRP-SF-CLASS(SUB) NOT NUMERIC
                MOVE 0 TO GP-PROGRP-SF-CLASS(SUB)
             ELSE
                MOVE OGP-PROGRP-SF-CLASS(SUB) TO
                                              GP-PROGRP-SF-CLASS(SUB)
             END-IF
           END-PERFORM.

           MOVE OGP-RENEW-DISC-UNERN       TO GP-RENEW-DISC-UNERN.
           MOVE OGP-NOTES-EOD-BATCH-ID     TO GP-NOTES-EOD-BATCH-ID.
           IF OGP-NEXT-NOTE NOT NUMERIC
              MOVE 1                       TO GP-NEXT-NOTE
           ELSE
              MOVE OGP-NEXT-NOTE           TO GP-NEXT-NOTE.
           IF OGP-NEXT-BATCH-ID NOT NUMERIC
              MOVE 0                       TO GP-NEXT-BATCH-ID
           ELSE
              MOVE OGP-NEXT-BATCH-ID       TO GP-NEXT-BATCH-ID.
           MOVE OGP-REFNO-VERIFY           TO GP-REFNO-VERIFY.
           MOVE OGP-ACTIONCD-VERIFY        TO GP-ACTIONCD-VERIFY.
           MOVE OGP-BP-NEXT-BATCH          TO GP-BP-NEXT-BATCH.
           MOVE OGP-IB-ACCRUE-EARN-ON-REPO TO GP-IB-ACCRUE-EARN-ON-REPO.
           MOVE OGP-DELINQ-WITH-O2         TO GP-DELINQ-WITH-O2.
           MOVE OGP-ALLOW-REPO-ON-PL       TO GP-ALLOW-REPO-ON-PL.
           MOVE OGP-BK-PRIOR-TO-POSTING    TO GP-BK-PRIOR-TO-POSTING.
           MOVE OGP-AMT-CASH-PROCEEDS      TO GP-AMT-CASH-PROCEEDS.
           MOVE OGP-INCL-OTH-IN-EARNED     TO GP-INCL-OTH-IN-EARNED.
           MOVE OGP-VALID-BDATE            TO GP-VALID-BDATE.
           MOVE OGP-ARCHIVE-DIRECTORY      TO GP-ARCHIVE-DIRECTORY.
           MOVE OGP-DL-MEMO-TOGGLE         TO GP-DL-MEMO-TOGGLE.
           MOVE OGP-RN-FOR10-LN            TO GP-RN-FOR10-LN.
           MOVE OGP-LI-APPLICATION-FG      TO GP-LI-APPLICATION-FG.
           MOVE OGP-OT-INS-TERM            TO GP-OT-INS-TERM.
           MOVE OGP-UNEARNED-STATS         TO GP-UNEARNED-STATS.
           MOVE OGP-PDTH-GT-SCHED-PAY      TO GP-PDTH-GT-SCHED-PAY.
           IF OGP-LB-NEXT-BATCH NOT NUMERIC
              MOVE 200                     TO GP-LB-NEXT-BATCH
           ELSE
              MOVE OGP-LB-NEXT-BATCH       TO GP-LB-NEXT-BATCH.
           MOVE OGP-RECDEL-REVERSAL-FG     TO GP-RECDEL-REVERSAL-FG.
           MOVE OGP-REF-PHONE              TO GP-REF-PHONE.
           MOVE OGP-ALLOW-PL-ON-SX         TO GP-ALLOW-PL-ON-SX.
           MOVE OGP-USE-CONRATE            TO GP-USE-CONRATE.
           MOVE OGP-CPCORP-CURBAL          TO GP-CPCORP-CURBAL.
           MOVE OGP-TIME-ZONE-Q            TO GP-TIME-ZONE-Q.
           IF OGP-TIME-ZONE-FACTOR NOT NUMERIC
              MOVE 0 TO OGP-TIME-ZONE-FACTOR.
           MOVE OGP-TIME-ZONE-FACTOR       TO GP-TIME-ZONE-FACTOR.
           MOVE OGP-TIME-ZONE-DLS          TO GP-TIME-ZONE-DLS.
           IF OGP-CALL-START-TIME NOT NUMERIC
              MOVE 0 TO OGP-CALL-START-TIME.
           MOVE OGP-CALL-START-TIME        TO GP-CALL-START-TIME.
           IF OGP-CALL-END-TIME NOT NUMERIC
              MOVE 0 TO OGP-CALL-END-TIME.
           MOVE OGP-CALL-END-TIME          TO GP-CALL-END-TIME.
           MOVE OGP-RV-MEMO                TO GP-RV-MEMO.
           MOVE OGP-PENALTY-INT            TO GP-PENALTY-INT.
           MOVE OGP-ALLOW-SET-FOR-NONCASH  TO GP-ALLOW-SET-FOR-NONCASH.
           MOVE OGP-IGNORE-LCHG-NXDUE      TO GP-IGNORE-LCHG-NXDUE.
           MOVE OGP-ALLOW-ENTRY-TRADEIN    TO GP-ALLOW-ENTRY-TRADEIN.
           IF OGP-TRUE-NEXT-BATCH NOT NUMERIC
              MOVE 500                     TO GP-TRUE-NEXT-BATCH
           ELSE
              MOVE OGP-TRUE-NEXT-BATCH     TO GP-TRUE-NEXT-BATCH.
           MOVE OGP-REPO-CODE              TO GP-REPO-CODE.
           MOVE OGP-ALLOW-MORTGAGE-INT-Z   TO GP-ALLOW-MORTGAGE-INT-Z.
           IF OGP-PROM-AMT-MIN-PCT NOT NUMERIC
              MOVE 0 TO OGP-PROM-AMT-MIN-PCT.
           MOVE OGP-PROM-AMT-MIN-PCT       TO GP-PROM-AMT-MIN-PCT.
           MOVE OGP-BW-PRIVACY-OPT-OUT     TO GP-BW-PRIVACY-OPT-OUT.
           MOVE OGP-NO-GL-ON-O2-WHEN-PL    TO GP-NO-GL-ON-O2-WHEN-PL.
           MOVE OGP-INTERIM-COUPONS        TO GP-INTERIM-COUPONS.
           IF OGP-MGRAM-NEXT-BATCH NOT NUMERIC
              MOVE 600                     TO GP-MGRAM-NEXT-BATCH
           ELSE
           MOVE OGP-MGRAM-NEXT-BATCH       TO GP-MGRAM-NEXT-BATCH.
           MOVE OGP-RESCHEDULE-MEMO        TO GP-RESCHEDULE-MEMO.
           MOVE OGP-DONT-COLL-INT-ON-SS    TO GP-DONT-COLL-INT-ON-SS.
           MOVE OGP-DL-ACH-FG              TO GP-DL-ACH-FG.
           MOVE OGP-GL-NEW-JE              TO GP-GL-NEW-JE.
           MOVE OGP-CLEAR-PROM-PDTH        TO GP-CLEAR-PROM-PDTH.
           MOVE OGP-CREATE-TP-FORMS        TO GP-CREATE-TP-FORMS.
           MOVE OGP-CHECK-RECONCILIATION   TO GP-CHECK-RECONCILIATION.
           MOVE OGP-CORP-BRNO              TO GP-CORP-BRNO.
           MOVE OGP-PL-REPO-REASON         TO GP-PL-REPO-REASON.
           MOVE OGP-ALLOW-MIP-INSURANCE    TO GP-ALLOW-MIP-INSURANCE.
           MOVE OGP-ENTER-LPO-FG           TO GP-ENTER-LPO-FG.
           MOVE OGP-DLCOMM-CMSI-FG         TO GP-DLCOMM-CMSI-FG.
           MOVE OGP-REFCD-NEXT-BATCH       TO GP-REFCD-NEXT-BATCH.
           MOVE OGP-EARLIEST-CLOSE-TIME    TO GP-EARLIEST-CLOSE-TIME.
           MOVE OGP-GROUP-APPROVAL-FG      TO GP-GROUP-APPROVAL-FG.
           MOVE OGP-APPROVAL-MEMO          TO GP-APPROVAL-MEMO.
           MOVE OGP-FORCE-INPUT-PP-COVERAGE
                                          TO GP-FORCE-INPUT-PP-COVERAGE.
           MOVE OGP-NO-SC-USE-DM           TO GP-NO-SC-USE-DM.
           MOVE OGP-DEFAULT-O3-INSOURS     TO GP-DEFAULT-O3-INSOURS.
           MOVE OGP-USE-BW-BAD-FLAGS       TO GP-USE-BW-BAD-FLAGS.
           MOVE OGP-NON-FILE-FEECODE       TO GP-NON-FILE-FEECODE.
           MOVE OGP-CHBORR-PAST-DUE-DAYS   TO GP-CHBORR-PAST-DUE-DAYS.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
            MOVE OGP-CLOSED-FISCAL-FG(SUB) TO GP-CLOSED-FISCAL-FG(SUB)
           END-PERFORM.

           MOVE OGP-BACKDATE-MEMO          TO GP-BACKDATE-MEMO.
           MOVE OGP-OTREF-NEXT-BATCH       TO GP-OTREF-NEXT-BATCH.
           MOVE OGP-NEW-REPO-UNEARNED-ACCTS
                                          TO GP-NEW-REPO-UNEARNED-ACCTS.
           MOVE OGP-BK-FLAG-CONFIDENTIAL   TO GP-BK-FLAG-CONFIDENTIAL.
           MOVE OGP-AMORTIZE-ACQUIST-FEE   TO GP-AMORTIZE-ACQUIST-FEE.
           MOVE OGP-DENY-FORM-NO-ADDRESS   TO GP-DENY-FORM-NO-ADDRESS.
           MOVE OGP-HIDE-FIRST-5-SSNO      TO GP-HIDE-FIRST-5-SSNO.
           MOVE OGP-DEFERMENT-APPROVAL     TO GP-DEFERMENT-APPROVAL.
           MOVE OGP-INCLUDE-O2-IN-BAL      TO GP-INCLUDE-O2-IN-BAL.
           MOVE OGP-LOAN-ACH-PROCESSING    TO GP-LOAN-ACH-PROCESSING.
           MOVE OGP-PAYEE-PROCESSING-FG    TO GP-PAYEE-PROCESSING-FG.
           MOVE OGP-AUTO-BANKRUPT-WINDOW   TO GP-AUTO-BANKRUPT-WINDOW.
           IF OGP-BW-SCORES-QUARTER-SUB NOT NUMERIC
              MOVE 0                       TO GP-BW-SCORES-QUARTER-SUB
           ELSE
           MOVE OGP-BW-SCORES-QUARTER-SUB  TO GP-BW-SCORES-QUARTER-SUB.
           MOVE OGP-POST-XX-BF-PAYMENTS    TO GP-POST-XX-BF-PAYMENTS.
           MOVE OGP-VALIDATE-ENDORSER-AGE  TO GP-VALIDATE-ENDORSER-AGE.
           MOVE OGP-ALLOW-MANUAL-CHECKS    TO GP-ALLOW-MANUAL-CHECKS.
           MOVE OGP-EXPANDED-FOLLOW-UP     TO GP-EXPANDED-FOLLOW-UP.
           MOVE OGP-NEXT-EMPLOYEE-NO       TO GP-NEXT-EMPLOYEE-NO.
           MOVE OGP-USE-RESULT-WORKED-FG   TO GP-USE-RESULT-WORKED-FG.
           MOVE OGP-MAX-PROMISE-DAYS       TO GP-MAX-PROMISE-DAYS.
           MOVE OGP-PURGE-TO-ARCHIVE-FG    TO GP-PURGE-TO-ARCHIVE-FG.
           MOVE OGP-EDIT-BORROWER-NAME-FG  TO GP-EDIT-BORROWER-NAME-FG.
           MOVE OGP-RECAP-CLASS-TOTALS     TO GP-RECAP-CLASS-TOTALS.
           MOVE OGP-BRING-AUTO-INS-ON-RENEWAL
                                        TO GP-BRING-AUTO-INS-ON-RENEWAL.
           MOVE OGP-ALLOW-MF-ON-IB         TO GP-ALLOW-MF-ON-IB.
           MOVE OGP-USE-MIN-FORM-SENT-DAYS TO GP-USE-MIN-FORM-SENT-DAYS.
           MOVE OGP-DISPLAY-BAD-ADR-FLAG   TO GP-DISPLAY-BAD-ADR-FLAG.
           IF OGP-DAYS-IDENT-NEW-PAPER NOT NUMERIC
              MOVE 0 TO OGP-DAYS-IDENT-NEW-PAPER.
           MOVE OGP-DAYS-IDENT-NEW-PAPER   TO GP-DAYS-IDENT-NEW-PAPER.
           MOVE OGP-ESCROW-FG              TO GP-ESCROW-FG.
           MOVE OGP-SECURITY-FG            TO GP-SECURITY-FG.
           MOVE OGP-GPS-INFORMATION        TO GP-GPS-INFORMATION.
           MOVE OGP-CUST-INFO-FG           TO GP-CUST-INFO-FG.
           MOVE OGP-USE-ADDRESS-BW-SCAN    TO GP-USE-ADDRESS-BW-SCAN.
           MOVE OGP-EXPANDED-LEGAL-WINDOW  TO GP-EXPANDED-LEGAL-WINDOW.
           MOVE OGP-USE-SCRA               TO GP-USE-SCRA.
           MOVE OGP-USE-ACCRUAL-EARN-FILE  TO GP-USE-ACCRUAL-EARN-FILE.
           MOVE OGP-MESSAGE-ATTR           TO GP-MESSAGE-ATTR.
           MOVE OGP-JUDGMENT-WINDOW        TO GP-JUDGMENT-WINDOW.
           MOVE OGP-DUE-29-30-31           TO GP-DUE-29-30-31.
           MOVE OGP-DEFAULT-SKIP-TRW       TO GP-DEFAULT-SKIP-TRW.
           IF OGP-EARLIEST-OPEN-TIME NOT NUMERIC
              MOVE 0                       TO GP-EARLIEST-OPEN-TIME
           ELSE
           MOVE OGP-EARLIEST-OPEN-TIME     TO GP-EARLIEST-OPEN-TIME.
           MOVE OGP-TRANSFER-CONPROFILE    TO GP-TRANSFER-CONPROFILE.
           IF OGP-BATCH-REFCD-DUP-DAYS NOT NUMERIC
              MOVE 0                       TO GP-BATCH-REFCD-DUP-DAYS
           ELSE
           MOVE OGP-BATCH-REFCD-DUP-DAYS   TO GP-BATCH-REFCD-DUP-DAYS.
           MOVE OGP-CREATE-PAY-XTRP-XNO    TO GP-CREATE-PAY-XTRP-XNO.

      *- - - - - - - - - - - - - - - - - - - -{ 5. READ A30 GP-KEY #2 }
           MOVE 2 TO OGP-KEY.
           PERFORM READ-OGP-FILE.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GP-WRITE.

           ADD 01 TO UP1534-RECORD-COUNTER-READ.

      *-----------------------------------------{ 7. WRITE A32 GP-REC }
       PROCESS-FILE-GP-WRITE.

           PERFORM WRITE-GP-FILE.
           IF ( IO-BAD )
              IF ( FILE-STAT-DISP = "22,00" )
                 PERFORM REWRITE-GP-FILE
              ELSE
                 ADD 1                     TO UP1534-RECORD-COUNTER-SKIP
                 MOVE GP-KEY               TO UP1534-UPDATE-KEY
                 MOVE FILE-STAT-DISP       TO UP1534-UPDATE-STAT
                 MOVE "E"                  TO UP1534-UPDATE-TYPE
                 PERFORM UP1534-CREATE-FILE-UPDATE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

      *=================================================================
       PROCESS-FILE-GP-END.

           PERFORM CLOSE-OGP-FILE.
           MOVE OGP-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

           PERFORM CLOSE-GP-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

      *-----------------------------------------------------------------
       PROCESS-FILE-GP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - G R
      *
      *=================================================================
      *    IN  : GR-REC
      *          UP1534-VPI WORKERS
      *          FILE-OPTION-WORKERS
      *
      *    OUT : UP1534-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-GR SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-GR = "N" )
              GO TO PROCESS-FILE-GR-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO GR-PATH-OVERRIDE.
           MOVE "GR"                       TO UP1534-CURRENT-FILE.
           MOVE "GB/GRFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE ZEROES                     TO UP1534-RECORD-COUNTERS.

           PERFORM UP1534-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ACCESS-BUF       OGR-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP1534-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-GR-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP1534-FILE-ORDER---TARGET TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO GR-PATH.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-GR = "C" )
              PERFORM OPEN-OGR1-FILE
              PERFORM CLOSE-OGR1-FILE
              GO TO PROCESS-FILE-GR-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES TO OGR1-KEY.

           PERFORM OPEN-GR1-FILE.

           MOVE OGR-PATH TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH TO OGR-PATH.
           PERFORM OPEN-OGR1-FILE.
           PERFORM START-OGR1-FILE.

      *=================================================================
       PROCESS-FILE-GR-NEXT.

           PERFORM READ-OGR1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-GR-END.

           ADD  01  TO UP1534-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP1534-VPI-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ VALID RECORD }
           PERFORM CLEAR-GR-FILE.

           MOVE OGR-ID                     TO GR-ID.
           MOVE OGR-NAME                   TO GR-NAME.
           IF OGR-LEVEL NOT NUMERIC
              MOVE 1                       TO GR-LEVEL
           ELSE
           MOVE OGR-LEVEL                  TO GR-LEVEL.

      * 5/2/2023 JAY & BETH REMOVED NUMERIC TEST ON THIS, THEY ARE ALPHA!
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 100
              MOVE OGR-MEMBER-ID(SUB) TO GR-MEMBER-ID(SUB)
           END-PERFORM.

           MOVE OGR-CONTACT                TO GR-CONTACT.
           MOVE OGR-ADDR-1                 TO GR-ADDR-1.
           MOVE OGR-ADDR-2                 TO GR-ADDR-2.
           MOVE OGR-CITY                   TO GR-CITY.
           MOVE OGR-STATE                  TO GR-STATE.
           MOVE OGR-ZIP                    TO GR-ZIP.
           IF OGR-PHONE NOT NUMERIC
              MOVE 0                       TO GR-PHONE
           ELSE
              MOVE OGR-PHONE               TO GR-PHONE.
           IF OGR-HOME-BRANCH NOT NUMERIC
              MOVE 0                       TO GR-HOME-BRANCH
           ELSE
           MOVE OGR-HOME-BRANCH            TO GR-HOME-BRANCH.
           IF OGR-LN-APPROVAL-LIMIT NOT NUMERIC
              MOVE 0                       TO GR-LN-APPROVAL-LIMIT
           ELSE
           MOVE OGR-LN-APPROVAL-LIMIT      TO GR-LN-APPROVAL-LIMIT.

           PERFORM WRITE-GR1-FILE.
           IF ( IO-BAD )
              IF ( FILE-STAT-DISP = "22,00" )
                 PERFORM REWRITE-GR1-FILE
              ELSE
                 ADD 1                     TO UP1534-RECORD-COUNTER-SKIP
                 MOVE GR1-KEY              TO UP1534-UPDATE-KEY
                 MOVE FILE-STAT-DISP       TO UP1534-UPDATE-STAT
                 MOVE "E"                  TO UP1534-UPDATE-TYPE
                 PERFORM UP1534-CREATE-FILE-UPDATE
                 GO TO IO-ERROR.

           ADD 1 TO UP1534-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-GR-NEXT.

       PROCESS-FILE-GR-END.

           PERFORM CLOSE-OGR1-FILE.
           MOVE OGR-PATH TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

           PERFORM CLOSE-GR1-FILE.

           PERFORM UP1534-CREATE-FILE-TOTALS.

       PROCESS-FILE-GR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - M O D U L E
      *
      ******************************************************************
       PRINT-REPORT-MODULE SECTION.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "REPORT"                   TO UP1534-CURRENT-FILE.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.

           PERFORM UP1534-VPI-START.

           PERFORM OPEN-PR-FILE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP 'REPORT' FILE  }
           MOVE ZEROES                     TO UP1534-REPORT-KEY.

           PERFORM OPEN-REPORT-FILE.
           PERFORM START-REPORT-FILE.

      *=================================================================
       PRINT-REPORT-NEXT-LINE.

           PERFORM READ-REPORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-END.

           PERFORM UP1534-VPI-DISPLAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE REPORT-DETAIL-LINE         TO DETAIL-LINE.
           MOVE REPORT-LN-FEED             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-NEXT-LINE.

       PRINT-REPORT-MODULE-END.

           PERFORM CLOSE-REPORT-FILE.
           PERFORM PRINT-RANGES.

       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R A N G E S
      *
      ******************************************************************
       PRINT-RANGES SECTION.

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

           MOVE "DATABASE NAME                    :" TO R1-DESC.
           MOVE EXT-ACUSQL-DB-NAME                   TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERVER NAME                      :" TO R1-DESC.
           MOVE EXT-ACUSQL-SERVER-NAME               TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SOURCE PATH                      :" TO R1-DESC.
           MOVE SOURCE-PATH                          TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TARGET PATH                      :" TO R1-DESC.
           MOVE TARGET-PATH                          TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "*********[ GLOBAL FILES ]*********" TO DETAIL-LINE.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BR-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-BR                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GP-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-GP                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GR-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-GR                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-RANGES.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED                     TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

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
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-SYSDATE         TO IF1534-SYSDATE.
           DISPLAY IF1534-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IF1534-FORM-FIELDS         TO WR-BUF.
           MOVE IF1534-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IF1534-HELP--FILE          TO HELP-LINK-FILE.
           MOVE IF1534-HELP--DIR           TO HELP-LINK-DIR.

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
              COPY "LIBUP/IF1534_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GBBRCL.CPY".
       COPY "LIBGB/GBGPCL.CPY".
       COPY "LIBGB/GBGRCL.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *COPY "LIBUP/UP1534.CPY".
      * COPYMEMBER: LIBUP/UP1534.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UP1534.CPY
      *
      * DESCRIPTION   : ROUTINES FOR UPGRADE A15 TO A32
      *
      *                 UP1534-CREATE-DETAIL-LINE
      *                 UP1534-CREATE-BRANCH-HEAD
      *                 UP1534-CREATE-MACHINE-HEAD
      *                 UP1534-CREATE-FILE-ERROR
      *                 UP1534-CREATE-FILE-TOTALS
      *                 UP1534-LOAD-FILE-PATH
      *                 UP1534-VPI-DISPLAY
      *                 UP1534-VPI-START
      *
      *=================================================================
      * REV:
      * KEC 2018-0918 ***NEW*** LOCAL-TO-GLOBAL
      *
      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - D E T A I L - L I N E
      *
      *=================================================================
      *    IN  : UP1534-DETAIL-LINE
      *          UP1534-LN-FEED
      *          UP1534-REPORT-KEY
      *
      *    OUT : UP1534-DETAIL-LINE
      *          UP1534-LN-FEED
      *          UP1534-REPORT-KEY
      *
      *    DESC: WRITE RECORD TO REPORT-FILE USING DETAIL-LINE.
      ******************************************************************
       UP1534-CREATE-DETAIL-LINE SECTION.

            IF ( UP1534-REPORT-KEY = 999999999 )
               GO TO UP1534-CREATE-DETAIL-EXIT.

      *- - - - - - - - - - - - - - - - - - - -{ CREATE 'REPORT' RECORD }
            ADD  01                        TO UP1534-REPORT-KEY.
            MOVE UP1534-DETAIL-LINE        TO REPORT-DETAIL-LINE.
            MOVE UP1534-LN-FEED            TO REPORT-LN-FEED.

            PERFORM WRITE-REPORT-FILE.

            IF ( IO-BAD )
               GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ RESET FIELDS }
            MOVE SPACES                    TO UP1534-DETAIL-LINE.
            MOVE 01                        TO UP1534-LN-FEED.

      *=================================================================
       UP1534-CREATE-DETAIL-EXIT.
            EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - B R A N C H - H E A D
      *
      *=================================================================
      *    IN  : BR-REC
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE HEADER LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP1534-CREATE-BRANCH-HEAD SECTION.

           MOVE "BRANCH: "                 TO UP1534-D1-BRANCH-PROMPT.
           MOVE BR-NO                      TO UP1534-D1-BRANCH.
           MOVE BR-NAME                    TO UP1534-D1-BRANCH-NAME.
           MOVE 02                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-B-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - M A C H I N E - H E A D
      *
      *=================================================================
      *    IN  : UP1534-MACHINE
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE MACHINE LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP1534-CREATE-MACHINE-HEAD SECTION.

           MOVE "MACHINE: "                TO UP1534-D2-MACHINE-PROMPT.
      *KC  MOVE UP1534-MACHINE             TO UP1534-D2-MACHINE.
           MOVE CURRENT-MACHINE            TO UP1534-D2-MACHINE.
           MOVE "RISC MACHINE"             TO UP1534-D2-MACHINE-NAME.
           MOVE 02                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-M-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - F I L E - E R R O R
      *
      *=================================================================
      *    IN  : UP1534-CURRECT-FILE
      *          UP1534-FILE-ORDER    [SOURCE/TARGET]
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE ERROR LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP1534-CREATE-FILE-ERROR SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP1534-F1-FILE-PROMPT.
           MOVE UP1534-CURRENT-FILE        TO UP1534-F1-FILE.

           IF ( UP1534-FILE-ORDER-SOURCE )
              STRING "ERROR - SOURCE FILE: ",STAT
                  DELIMITED BY "  "      INTO UP1534-F1-FILE-MESSAGE
           ELSE
           IF ( UP1534-FILE-ORDER-TARGET )
              STRING "ERROR - TARGET FILE: ",STAT
                  DELIMITED BY "  "      INTO UP1534-F1-FILE-MESSAGE
           ELSE
              STRING "ERROR - MISC.. FILE: ",STAT
                  DELIMITED BY "  "      INTO UP1534-F1-FILE-MESSAGE.

           MOVE 01                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-FILE-ER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - F I L E - U P D A T E
      *
      *=================================================================
      *    IN  : UP1534-CURRENT-FILE
      *          UP1534-UPDATE-KEY
      *          UP1534-UPDATE-STAT
      *          UP1534-UPDATE-TYPE    [ERROR/SKIP]
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE LINE OF SKIP/ERROR RECORD FOR THE REPORT-FILE.
      ******************************************************************
       UP1534-CREATE-FILE-UPDATE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP1534-F1-FILE-PROMPT.
           MOVE UP1534-CURRENT-FILE        TO UP1534-F1-FILE.

           IF ( UP1534-UPDATE-TYPE-ERROR )
              MOVE SPACES                  TO UP1534-F1-FILE-MESSAGE
              STRING "ERROR - KEY: ",
                     UP1534-UPDATE-KEY, " ["
                     UP1534-UPDATE-STAT, "]"
                     DELIMITED BY "  "   INTO UP1534-F1-FILE-MESSAGE
           ELSE
              MOVE SPACES                  TO UP1534-F1-FILE-MESSAGE
              STRING "SKIP - KEY: ",
                     UP1534-UPDATE-KEY
                     DELIMITED BY "  "   INTO UP1534-F1-FILE-MESSAGE.

           MOVE 01                         TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-FILE-UP-END.

           MOVE SPACES                     TO UP1534-UPDATE-KEY
                                              UP1534-UPDATE-TYPE
                                              UP1534-UPDATE-STAT.

      *-----------------------------------------------------------------
       UP1534-CREATE-FILE-UP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - C R E A T E - F I L E - T O T A L S
      *
      *=================================================================
      *    IN  : UP1534-CURRECT-FILE
      *          UP1534-RECORD-COUNTERS
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE TOTAL LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP1534-CREATE-FILE-TOTALS SECTION.

           MOVE "FILE: "                    TO UP1534-F1-FILE-PROMPT.
           MOVE UP1534-CURRENT-FILE         TO UP1534-F1-FILE.

           MOVE "READ: "                    TO
                UP1534-F1-RECORD-READ-PROMPT.
           MOVE UP1534-RECORD-COUNTER-READ  TO UP1534-F1-RECORD-READ.

           MOVE "WRITE: "                   TO
                UP1534-F1-RECORD-WRITE-PROMPT.
           MOVE UP1534-RECORD-COUNTER-WRITE TO UP1534-F1-RECORD-WRITE.

           MOVE "SKIP: "                    TO
                UP1534-F1-RECORD-SKIP-PROMPT.
           MOVE UP1534-RECORD-COUNTER-SKIP  TO UP1534-F1-RECORD-SKIP.

           MOVE 01                          TO UP1534-LN-FEED.
           PERFORM UP1534-CREATE-DETAIL-LINE.

      *=================================================================
       UP1534-CREATE-FILE-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          UP1534-FILE-INPUT  [EXP.  XX/XXXXXX]
      *          UP1534-FILE-ORDER  [SOURCE/TARGET]
      *          UP1534-FILE-TYPE   [GLOBAL/LOCAL]
      *          UP1534-SOURCE-PATH
      *          UP1534-TARGET-PATH
      *
      *    OUT : UP1534-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       UP1534-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO UP1534-FILE-PATH.

           IF ( UP1534-FILE-INPUT           = SPACES ) OR
              ( UP1534-FILE-INPUT-DIR       = SPACES ) OR
              ( UP1534-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( UP1534-FILE-INPUT-NAME      = SPACES )
              GO TO UP1534-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-SOURCE ) AND
              ( UP1534-FILE-TYPE-GLOBAL  )
              MOVE SOURCE-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE SOURCE-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-SOURCE ) AND
              ( UP1534-FILE-TYPE-LOCAL   )
              MOVE SOURCE-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE SOURCE-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP1534-FILE-PATH-LOCAL-DATA-SL
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-TARGET ) AND
              ( UP1534-FILE-TYPE-GLOBAL  )
            MOVE TARGET-PATH         TO UP1534-FILE-PATH
            MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
            MOVE TARGET-MACHINE      TO UP1534-FILE-PATH-MACHINE
            MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
            MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-FILE-ORDER-TARGET ) AND
              ( UP1534-FILE-TYPE-LOCAL   )
              MOVE TARGET-PATH         TO UP1534-FILE-PATH
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-3
              MOVE TARGET-MACHINE      TO UP1534-FILE-PATH-MACHINE
              MOVE "/"                 TO UP1534-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP1534-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP1534-FILE-PATH-LOCAL-DATA-SL
              MOVE UP1534-FILE-INPUT   TO UP1534-FILE-PATH-LOCAL-PATH.

       UP1534-LOAD-FILE-PATH-EXIT.
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
      *    U P 1 5 3 4 - M V 1 5 - O L D - T O - N E W
      *
      *=================================================================
      * IN  : UP1534-MV15-OLD-PATH
      *       UP1534-FILE-TYPE  [B, G, L]
      * OUT : UP1534-MV15-NEW-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE OLD FILE TO NEW FILE.
      ******************************************************************
       UP1534-MV15-OLD-TO-NEW SECTION.

           MOVE UP1534-MV15-OLD-PATH TO UP1534-MV15-NEW-PATH.

           IF ( UP1534-FILE-TYPE-BASESH )
              MOVE "O" TO UP1534-MV15-NEW-BASESH-O 
              MOVE UP1534-MV15-OLD-BASESH-FILE TO
                   UP1534-MV15-NEW-BASESH-FILE
           ELSE
           IF ( UP1534-FILE-TYPE-GLOBAL )
              MOVE "O" TO UP1534-MV15-NEW-GLOBAL-O 
              MOVE UP1534-MV15-OLD-GLOBAL-FILE TO
                   UP1534-MV15-NEW-GLOBAL-FILE
           ELSE
              MOVE "O" TO UP1534-MV15-NEW-LOCAL-O 
              MOVE UP1534-MV15-OLD-LOCAL-FILE TO
                   UP1534-MV15-NEW-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE UP1534-MV15-OLD-PATH TO MV-FILE1.
           MOVE UP1534-MV15-NEW-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       UP1534-MV15-OLD-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - M V 1 5 - N E W - T O - O L D
      *
      *=================================================================
      * IN  : UP1534-MV15-NEW-PATH
      *       UP1534-FILE-TYPE  [B, G, L]
      * OUT : UP1534-MV15-OLD-PATH
      * DESC: EXECUTE SYSTEM MOVE COMMAND TO MOVE NEW FILE TO OLD FILE.
      ******************************************************************
       UP1534-MV15-NEW-TO-OLD SECTION.

           MOVE UP1534-MV15-NEW-PATH TO UP1534-MV15-OLD-PATH.

           IF ( UP1534-FILE-TYPE-BASESH )
              MOVE UP1534-MV15-NEW-BASESH-FILE TO
                   UP1534-MV15-OLD-BASESH-FILE
           ELSE
           IF ( UP1534-FILE-TYPE-GLOBAL )
              MOVE UP1534-MV15-NEW-GLOBAL-FILE TO
                   UP1534-MV15-OLD-GLOBAL-FILE
           ELSE
              MOVE UP1534-MV15-NEW-LOCAL-FILE TO
                   UP1534-MV15-OLD-LOCAL-FILE.

           MOVE MV-CMD               TO SYSTEM-BUF.
           MOVE UP1534-MV15-NEW-PATH TO MV-FILE1.
           MOVE UP1534-MV15-OLD-PATH TO MV-FILE2.

           PERFORM SYSTEM-CALL.

      *=================================================================
       UP1534-MV15-NEW-TO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  UP1534-VPI-BRNO
      *        UP1534-VPI-FILE-PREFIX
      *        UP1534-VPI-FILE-TYPE
      *        UP1534-VPI-PRU-LP
      *        UP1534-VPI-ROUTE-BUF
      *        UP1534-VPI-SLEEP-COUNTER
      *        UP1534-VPI-SLEEP-COUNTER-MAX
      *        UP1534-VPI-TICKER
      *        UP1534-VPI-TIME-EDIT
      *
      *  OUT:  UP1534-VPI-SLEEP-COUNTER
      *        UP1534-VPI-TICKER
      *
      * DESC:  UP1534 VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       UP1534-VPI-DISPLAY SECTION.

           IF ( UP1534-VPI-SLEEP-COUNTER 
              < UP1534-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO UP1534-VPI-SLEEP-COUNTER
              GO TO UP1534-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE UP1534-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO UP1534-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO UP1534-VPI-DISPLAY-BUF.

           MOVE "("                        TO UP1534-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO UP1534-VPI-DISPLAY-RIGHT.

           IF ( UP1534-VPI-TICKER = "-" )
              MOVE "\"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
           IF ( UP1534-VPI-TICKER = "\" )
              MOVE "|"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
           IF ( UP1534-VPI-TICKER = "|" )
              MOVE "/"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER
           ELSE
              MOVE "-"                     TO UP1534-VPI-DISPLAY-TICKER
                                              UP1534-VPI-TICKER.

           IF ( UP1534-VPI-FILE-TYPE-LOCAL )
              MOVE UP1534-VPI-BRNO         TO UP1534-VPI-DISPLAY-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET THE FILE NAME IN THE DISPLAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "***FILE NOT FOUND***"     TO UP1534-VPI-DISPLAY-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ GLOBAL }
           IF ( UP1534-VPI-FILE-AL )
              MOVE "G/L ALLOCATION (AL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BR )
              MOVE "BRANCH MASTER (BR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BY )
              MOVE "BRANCH STATISTICS (BY)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CD )
              MOVE "CODE DESCRIPTION (CD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CL )
              MOVE "CAN LOAN (CL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EA )
              MOVE "EXTRACTION (EA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EP )
              MOVE "ELECTRONIC PAYMENT (EP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-EW )
              MOVE "ELECTRONIC PAY WORK (EW)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-FD )
              MOVE "FORMS DATABASE (FD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-FS )
              MOVE "FINANCIAL STATEMENT (FS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GI )
              MOVE "G/L INTERFACE (GI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GL )
              MOVE "G/L MASTER (GL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GT )
              MOVE "G/L TRANSACTIONS (GT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GQ )
              MOVE "GLOBAL QUEUE (GQ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-II )
              MOVE "IRS IDENTIFICATION (II)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IR )
              MOVE "INSURANCE RATE TABLE (IR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-JD )
              MOVE "G/L JOURNAL DETAIL (JD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-JH )
              MOVE "G/L JOURNAL HISTORY (JH)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LC )
              MOVE "LOAN CLASS (LC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PE )
              MOVE "PASSWORD EXCEPTIONS (PE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-RB )
              MOVE "REBATE/CANCEL (RB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-RC )
              MOVE "DAILY STATUS (RC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SE )
              MOVE "USER SECURITY (SE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SN )
              MOVE "SECURITY NAMES (SN)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SM )
              MOVE "STATE PARAM MEMOS (SM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SP )
              MOVE "STATE PARAMETERS (SP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-SS )
              MOVE "CREDIT SCORE STATS (SS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ST )
              MOVE "STATE FILE (ST)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TZ )
              MOVE "TIME ZONE (TZ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-WR )
              MOVE "WORLD RATING SHEET (WR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ LOCAL  }
           IF ( UP1534-VPI-FILE-BM )
              MOVE "BORROWER MEMO (BM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-BW )
              MOVE "BORROWER MASTER (BW)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CA )
              MOVE "CASH DRAWER (CA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CI )
              MOVE "CUSTOMER INFORMATION (CI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CJ )
              MOVE "COLLECTOR JOURNAL (CJ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CK )
              MOVE "LOAN CHECK MASTER (CK)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CP )
              MOVE "COLLECTOR POOL (CP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-CQ )
              MOVE "COLLECTOR QUEUE (CQ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DL )
              MOVE "DEALER MASTER (DL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DM )
              MOVE "DEALER MEMO (DM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DS )
              MOVE "DEALER STATISTICS (DS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-DT )
              MOVE "DEALER TRANSACTION (DT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ES )
              MOVE "LOAN ESCROW (ES)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-GB )
              MOVE "GLOBAL (GB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IC )
              MOVE "INSURANCE COMPANY (IC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IR )
              MOVE "INSURANCE RATE (IR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-IX )
              MOVE "ALTERNATE INDEX (IX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LE )
              MOVE "LOAN ENDORSER (LE)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LH )
              MOVE "LOAN HISTORY (LH)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LM )
              MOVE "LOAN MEMO (LM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LN )
              MOVE "LOAN MASTER (LN)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LP )
              MOVE "LOAN PAYMENT (LP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LS )
              MOVE "LOAN STATUS TRAILER (LS)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSD )
              MOVE "LOAN STATUS - DEFICIENCY (LSD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSF )
              MOVE "LOAN STATUS - FOLLOW UP (LSF)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSG )
              MOVE "LOAN STATUS - GPS STARTER (LSG)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSJ )
              MOVE "LOAN STATUS - JUDGEMENT (LSJ)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSL )
              MOVE "LOAN STATUS - LEGEL (LSL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSM )
              MOVE "LOAN STATUS - MISC (LSM)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSP )
              MOVE "LOAN STATUS - PAID THRU (LSP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSR )
              MOVE "LOAN STATUS - RESCHEDULE (LSR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LSX )
              MOVE "LOAN STATUS - REPO (LSX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LT )
              MOVE "LOAN TRAILER (LT)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTA )
              MOVE "LOAN TRAILER - AUTO INS (LTA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTB )
              MOVE "LOAN TRAILER - BANKRUPTCY (LTB)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTC )
              MOVE "LOAN TRAILER - COLLATERAL (LTC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTD )
              MOVE "LOAN TRAILER - DEBIT CARD (LTD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTF )
              MOVE "LOAN TRAILER - FEE CODE (LTF)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTI )
              MOVE "LOAN TRAILER - INSURANCE (LTI)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTL )
              MOVE "LOAN TRAILER - LIENHOLDER (LTL)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTP )
              MOVE "LOAN TRAILER - PYMT SCHEDULE (LTP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LTR )
              MOVE "LOAN TRAILER - REVOLVING (LTR)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-LX )
              MOVE "LOAN PAYMENT ADDON (LX)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-ML )
              MOVE "MAIL LIST (ML)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-MP )
              MOVE "LOAN MEMO PAYMENT (MP)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PA )
              MOVE "PAYEE (PA)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PC )
              MOVE "LOAN SCRATCH COLLATERAL (PC)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-PD )
              MOVE "LOAN SCRATCH PAD (PD)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-TY )
              MOVE "TODAY'S TOTAL STATISTICS (TY)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ MISC }
           IF ( UP1534-VPI-FILE-WK1 )
              MOVE "WORK FILE 1 (WK1)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-WK2 )
              MOVE "WORK FILE 2 (WK2)"
                TO UP1534-VPI-DISPLAY-FILE
           ELSE
           IF ( UP1534-VPI-FILE-REPORT )
              MOVE "UP1534 REPORT (REPORT)"
                TO UP1534-VPI-DISPLAY-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP1534-VPI-TIME-EDIT       TO UP1534-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING UP1534-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP1534-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( UP1534-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE UP1534-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE UP1534-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       UP1534-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 1 5 3 4 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  UP1534-CURRENT-FILE
      *        UP1534-FILE-TYPE
      *        UP1534-PRU-LP
      *
      *  OUT:  UP1534-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       UP1534-VPI-START SECTION.

           IF ( UP1534-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO UP1534-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO UP1534-VPI-BRNO.

           MOVE UP1534-CURRENT-FILE        TO UP1534-VPI-FILE-PREFIX.
           MOVE UP1534-FILE-TYPE           TO UP1534-VPI-FILE-TYPE.
      *KC  MOVE UP1534-PRU-LP              TO UP1534-VPI-PRU-LP.
           MOVE PRU-LP                     TO UP1534-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO UP1534-VPI-ROUTE-BUF.
           MOVE 9999                       TO UP1534-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO UP1534-VPI-TIME-EDIT.

      *=================================================================
       UP1534-VPI-START-EXIT.
           EXIT.


      ******************************************************************
      *
      *     M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPH SECTION.
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

           MOVE SPACES                     TO UP1534-FILE-PATH.
           STRING "/", GETENV-BUF
                  DELIMITED BY "  "      INTO UP1534-FILE-PATH.
           MOVE UP1534-FILE-PATH-MACHINE   TO CURRENT-MACHINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK TO SEE IF THE SOURCE & TARGET PATHS ARE SET FROM BATCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SOURCE-PATH = SPACES )
              MOVE UP1534-FILE-PATH        TO SOURCE-PATH
           ELSE
              MOVE SOURCE-PATH             TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE UP1534-FILE-PATH     TO SOURCE-PATH.
     
           IF ( TARGET-PATH = SPACES )
              MOVE UP1534-FILE-PATH        TO TARGET-PATH
           ELSE
              MOVE TARGET-PATH             TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE UP1534-FILE-PATH     TO TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CURRENT FIL: "            TO FIL-PROMPT.
           MOVE UP1534-FILE-PATH           TO FIL-INFO.

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
           MOVE CURRENT-MACHINE      TO MACHINE-NAME.

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
           MOVE "S  A300TITLE:FR."               TO VDU.
           MOVE "[ 'A15' TO 'A34' INITIALIZE  ]" TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBUP/UP1534_SPIOREPORT.CPY".

      * COPYMEMBER: LIBGB/GBBRRN
       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN INPUT OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.
       START-OBR-FILE.
           PERFORM START-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           START OBR-FILE KEY NOT < OBR-KEY.
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.
      * COPYMEMBER: LIBGB/GBOB21R
       OPEN-OB21-FILE.
           PERFORM OPEN-IT.
           MOVE OB2-PATH TO E-FILE.
           OPEN I-O OB2-FILE.
           IF IO-FG = 8
             GO TO OPEN-OB21-FILE.
           IF IO-FG = 7
             CLOSE OB2-FILE
             GO TO OPEN-OB21-FILE.
       READ-OB21-FILE.
           PERFORM READ-IT.
           MOVE OB2-PATH TO E-FILE.
           MOVE OB21-KEY TO E-KEYX.
           READ OB2-FILE.
           IF IO-FG = 8
              GO TO READ-OB21-FILE.
       CLOSE-OB21-FILE.
           PERFORM CLOSE-IT.
           CLOSE OB2-FILE.

      * COPYMEMBER: LIBGB/GBGPR
       OPEN-OGP-FILE.
           PERFORM OPEN-IT.
           MOVE OGP-PATH TO E-FILE.
           OPEN INPUT OGP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGP-FILE.
           IF IO-FG = 7
              CLOSE OGP-FILE
              GO TO OPEN-OGP-FILE.
       READ-OGP-FILE.
           PERFORM READ-IT.
           MOVE OGP-PATH TO E-FILE.
           MOVE OGP-KEY TO E-KEYX.
           READ OGP-FILE.
           IF IO-FG = 8
              GO TO READ-OGP-FILE.

       READ-OGP-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGP-PATH TO E-FILE.
           MOVE OGP-KEY TO E-KEYX.
           READ OGP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGP-FILE-NEXT.
       START-OGP-FILE.
           PERFORM START-IT.
           MOVE OGP-PATH TO E-FILE.
           MOVE OGP-KEY TO E-KEYX.
           START OGP-FILE KEY NOT < OGP-KEY.
       CLOSE-OGP-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGP-FILE.

      * COPYMEMBER: LIBGB/GBGR1RN
       OPEN-OGR1-FILE.
           PERFORM OPEN-IT.
           MOVE OGR-PATH TO E-FILE.
           OPEN INPUT OGR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGR1-FILE.
           IF IO-FG = 7
              CLOSE OGR-FILE
              GO TO OPEN-OGR1-FILE.
       READ-OGR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGR-PATH TO E-FILE.
           MOVE OGR1-KEY TO E-KEYX.
           READ OGR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGR1-FILE-NEXT.
       START-OGR1-FILE.
           PERFORM START-IT.
           MOVE OGR-PATH TO E-FILE.
           MOVE OGR1-KEY TO E-KEYX.
           START OGR-FILE KEY NOT < OGR1-KEY.
       CLOSE-OGR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGR-FILE.

      *====================================================[ TARGET IO ]
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/GBGPIN.CPY".
       COPY "LIBGB/GBGR1IN.CPY".

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

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVING REPORT FILE IN /USR/TMP TO /USR/TMP/IF1534.MKTEMP
      *    SAVING REPORT FILE IN CASE OF ANY OF THE UPGRADE PROGRAMS
      *    BOMB OUT SO YOU CAN SEE WHICH FILE WAS PROCESSED LAST.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MV "                      TO SAVE-REPORT-CMD.
           CALL "C$TOLOWER" USING SAVE-REPORT-CMD, VALUE 03.

           MOVE UP1534-REPORT-PATH         TO MKTEMP-BUF
                                              SAVE-REPORT-SOURCE
                                              SAVE-REPORT-TARGET.

           MOVE "IF1534."                  TO SAVE-REPORT-TARGET-NEW.
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
      *KC>    DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  *> KEC 2018.0130
              PERFORM FORM-RESET
              DESTROY IF1534-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

