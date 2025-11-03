       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       INSTAL.
       AUTHOR.              BLM.
       DATE-WRITTEN. 07/27/2012.
      **************************************************************************
      *
      *                      (SP)
      *  DESCRIPTION:  PARADATA PROGRAM & FORM INSTALLER
      *                WORLD PR# 815
      *
      * THIS PROGRAM IS GENERIC AND CHECKS $REL FOR WHAT IS LIVE AND TEST
      *
      * IF UPDATING TO A15/A99, THEN YOU . A15 BEFORE YOU RUN
      * IF UPDATING TO S35/S99, THEN YOU . S35 BEFORE YOU RUN
      *
      * NOTE: THE SH/INSTAL.SH ON THE SERVER MUST HAVE CORRECT RELEASES
      *       IN THEM. 
      *   
      * HELP  - HELP/LNAGED.TXT
      * COBOL - CO/LNAGED
      * SHELL - SH/XXX.SH
      * PST   - PST/PST465.01
      * SQL   - SQL/CRNOFL_REWRITE
      * 
      *
      * REV:
      * BLM 121016 CHANGED NAME OF IMPORT FILE LIST FROM PR????? TO "LIST"
      * BLM 121017 EXPANDED PR FROM 7 TO 8, SO NOW WE CAN HAVE PR1234.1,
      *            PR1234.2, ETC.
      * BLM 121023 INITIALIZE FILE-BUF WHEN CONVERTING FORMS, HELP, ETC.,
      *            PROBLEM IF FORM OR HELP IS < 6 CHARACTERS
      * BLM 121121 TRY TO RESOLVE CONFUSION WHEN INSTALLING ON WORLD1 OR WORLD2:
      *            IF WORLD1 OR 2, CREATE TAG IN DIRECTORY SO WE CAN TELL IF
      *            INSTALLED ON OTHER MACHINE, FOR EXAMPLE:
      *             /USR/PFSI/NEW/PR1234/WORLD1.A92.MMDDYY
      *            THEN IF OTHER MACHINE IS INSTALLED, DON'T WARN THAT LIST
      *            ALREADY EXISTS.  IF OTHER MACHINE NOT INSTALLED, DON'T
      *            ASK AND DON'T REMOVE LIST.
      *            ALSO, IF WORLD1 OR 2, CHECK IF PR ALREADY DONE ON THIS
      *            MACHINE TODAY AND WARN.
      * BLM 121204 NEW-IMPORT-PATH WAS NOT BEING SETUP WHEN INSTALLING TO A32,
      *            SO PR WAS NOT GETTING REMOVED FROM /USR/PFSI/P3/NEW
      * BLM 121204 IF LIST CONTAINS FM, SM, OR HELP ITEM, MAKE SURE THE LINE
      *            HAS SUFFIX OF .F, .S, OR .H.
      *  CS 140522 REMOVED LIBGB/RM_CMD.CPY
      * BLM 160607 FIX BUG, PROBLEM WHEN REVERSING COPY TO A32 ON WORLD2, 
      *            TEST-IMPORT-PATH2 WAS NOT GETTING SET UP, PR# 1161
      * BLM 170301 CHANGES FOR A32, GET RID OF COPY COMMANDS FOR FM & SM,
      *            GET DIR OUT OF PATH FOR CO & HELP, ADD .TXT SUFFIX FOR HELP
      * BLM 171201 CHANGES FOR A32: CHANGE PATH TO P3 INSTEAD OF PR, CHANGE A15
      *            TO A32, CHANGE A99 TO A92
      * BLM 180125 SOME ROUTINES STILL HAD LOGIC TO STRIP THE .H SUFFIX FROM
      *            HELP FILE NAME, REMOVE LOGIC, HELP WILL BE IDENTIFIED BY
      *            .TXT SUFFIX IN A32
      * BLM 180208 MOVE FORM-PROGX TO PRU-PROG, REPORT WAS GOING TO MKTEMP-BUF
      *            INSTEAD OF INSTALXXXXX
      * BAH 190415 CHANGE INSTAL_CMD.CPY TO WORK WITH QA DIRECTORIES, QA0002
      * BAH 190528 WAS NOT REMOVING THE DIRECTORIES FROM NEW, NEEDED TO USE
      *            DIR-ACCESS, QA0031
      * BAH 190913 WORK FOR A32 AND A92 (TEST).
      * BAH 2020.0410 WORK FOR A34 AND A94 (TEST).
      * BAH 2023.0929 WORK FOR S35 AND S99 (TEST).
      * BAH 2024.0530 FIXED /USR/TMP REPORT NAME TO START WITH "INSTAL", WAS
      *               CREATING AS XXXXINSTALXXXXXX S35Q-58
      * BAH 2025.0203 ADD /USR/S35/SQL DIRECTORY S35Q-162
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN TO IMPORT-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
           SELECT LIST-FILE ASSIGN TO LS-TEMPFILE
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
           SELECT LOG-FILE ASSIGN TO TEMP-LOG-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
           SELECT SYS-NAME-FILE ASSIGN TO SYS-NAME-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 80 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03 OUT-BYTE                  PIC X  OCCURS 80 TIMES.
       01  DESC-REC.
           03  PR-DESC-PMT     PIC X(4).
               88  PR-DESC-VALID VALUE "DESC".
           03  FILLER          PIC X(2).
           03  PR-DESC         PIC X(72).
           03  FILLER          PIC X(2).
       01  CO-REC.
           03  PR-CO           PIC X(2).
               88  PR-CO-VALID VALUE "CO".
           03  FILLER          PIC X.
           03  PR-CO-PROG      PIC X(6).
           03  FILLER          PIC X(71).
       01  HELP-REC.
           03  PR-HELP         PIC X(4).
               88 PR-HELP-VALID VALUE "HELP".
           03  FILLER          PIC X.
           03  PR-HELP-PROG    PIC X(10).
           03  FILLER          PIC X(65).
       01  SHELL-REC.
           03  PR-SHELL        PIC X(2).
               88 PR-SHELL-VALID VALUE "SH".
           03  FILLER          PIC X.
           03  PR-SHELL-NAME   PIC X(30).
           03  FILLER          PIC X(46).
       01  PCL-REC.
           03  PR-PCL          PIC X(3).
               88 PR-PCL-VALID VALUE "PCL".
           03  FILLER          PIC X.
           03  PR-PCL-NAME     PIC X(12).
           03  FILLER          PIC X(64).
       01  PST-REC.
           03  PR-PST          PIC X(3).
               88 PR-PST-VALID VALUE "PST".
           03  FILLER          PIC X.
           03  PR-PST-NAME     PIC X(12).
           03  FILLER          PIC X(64).
       01  SQL-REC.
           03  PR-SQL          PIC X(3).
               88 PR-SQL-VALID VALUE "SQL".
           03  FILLER          PIC X.
           03  PR-SQL-NAME     PIC X(30).
           03  FILLER          PIC X(46).

       FD  LIST-FILE
           RECORD IS VARYING IN SIZE FROM 76 TO 200 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LIST-REC.
           03  LIST-CHAR             PIC X OCCURS 200 TIMES.

       FD  LOG-FILE
           RECORD IS VARYING IN SIZE FROM 20 TO 80 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LOG-REC.
           03  LOG-CHAR             PIC X OCCURS 80 TIMES.

       FD  SYS-NAME-FILE   LABEL RECORDS ARE STANDARD.
       01  SYS-NAME-REC.
           03  SYS-NAME             PIC X(10).
           03  FILLER               PIC X(7).
       01  WORLD-TAG-REC.
           03  FILLER               PIC X(5).
           03  WORLD-TAG-MACHINE    PIC 9.
           03  FILLER               PIC X.
           03  WORLD-TAG-REL        PIC X(3).
           03  FILLER               PIC X.
           03  WORLD-TAG-MMDDYY     PIC 9(6).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/INSTAL S35 02/06/25-10:53:29 >".


       01  PR-IFN               PIC XX      VALUE "PR".
       01  LS-TEMPFILE          PIC X(35).
       01  TEMP-LOG-PATH        PIC X(35).

      * IMPORT-PATH EXAMPLE     : /USR/PFSI/P3/NEW/PR123/PR123
      * NEW IMPORT-PATH EXAMPLE : /USR/PFSI/P3/NEW/PR123
      * TEST IMPORT-PATH EXAMPLE: /USR/PFSI/P3/TEST/PR12345
       01  IMPORT-PATH          PIC X(33).
       01  NEW-IMPORT-FILE-PATH.
           03  NEW-IMPORT-PATH1 PIC X(17).
           03  NEW-IMPORT-PATH2.
               05  FILLER       PIC XX.
               05  NEW-PRNO     PIC X(6).
       01  TEST-IMPORT-FILE-PATH.
           03  TEST-IMPORT-PATH1 PIC X(18).
           03  TEST-IMPORT-PATH2.
               05  FILLER        PIC XX.
               05  TEST-PRNO     PIC X(6).
       01  DONE-FILE-PATH.
           03  DONE-PATH1        PIC X(18).
           03  DONE-PATH2.
               05  FILLER        PIC XX.
               05  DONE-PRNO     PIC X(6).

       01  SYS-NAME-PATH         PIC X(35).
       01  SYSTEM-NAME           PIC X(10).
       01  FILLER REDEFINES SYSTEM-NAME.
           03  SYSTEM-NAME-1ST-5 PIC X(5).
           03  FILLER            PIC X(5).

       01  HOLD-TEST-REL     PIC X(3).
       01  HOLD-LIVE-REL     PIC X(3).
      * USR/A32
       01  HOLD-REL              PIC X(7).
       01  HOLD-RELX REDEFINES HOLD-REL.
           03  FILLER            PIC X(6).
           03  HOLD-REL-VERSION  PIC 9.

       01  HOLD-FIL.
           03  HOLD-XXX             PIC X(3).
           03  HOLD-SLASH1          PIC X.
           03  HOLD-XXXX            PIC X(4).
           03  HOLD-SLASH2          PIC X.
           03  HOLD-MACHINE         PIC X(2).
           03  HOLD-SLASH3          PIC X.
           03  HOLD-DATA-PATH       PIC X(6).

       01  TEST-TAG-CMD.
           03  TEST-TAG-CREATE           PIC X(2)   VALUE "> ".
           03  TEST-TAG-PATH             PIC X(42).
       01  LIVE-TAG-CMD.
           03  LIVE-TAG-CREATE           PIC X(2)   VALUE "> ".
           03  LIVE-TAG-PATH             PIC X(43).
       01  REV-TAG-CMD.
           03  REV-TAG-CREATE           PIC X(2)   VALUE "> ".
           03  REV-TAG-PATH             PIC X(43).
       01  OTHER-MACHINE-DONE           PIC X.
       01  THIS-MACHINE-DONE            PIC X.
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  WS-LIST-REC.
           03  LIST-READ-WRITE   PIC X(10).
           03  LIST-LINKS        PIC X(1).
           03  LIST-USERID       PIC X(9).
           03  LIST-GROUP        PIC X(9).
           03  LIST-SIZE         PIC X(9).
           03  LIST-MONTH        PIC X(3).
           03  LIST-DAY          PIC X(2).
           03  LIST-TIME         PIC X(5).
           03  LIST-DIR-PR.
               05  LIST-NEW-DIR      PIC X(17).
               05  LIST-NEW-PR       PIC X(8).
               05  FILLER            PIC X.
           03  FILLER REDEFINES LIST-DIR-PR.
               05  LIST-TEST-DIR     PIC X(18).
               05  LIST-TEST-PR      PIC X(8).

       01  SEL             PIC 999.
           88 COPY-TO-TEST  VALUE 000001.
           88 COPY-TO-LIVE  VALUE 000002.
           88 REV-FROM-LIVE VALUE 000003.
       01  PR-NO-X.
           03  PR-NO       PIC 9(6).

       01  REV-YYMMDD      PIC 9(6).

       01  NUM-STAT        PIC 9.
       01  HOLD-PATH-ERR   PIC X(39) VALUE SPACES.

       01  LOG-WORK.
           03  LOG-PRNO         PIC X(8).
           03  FILLER           PIC X         VALUE " ".
           03  LOG-DATE         PIC 99/99/99.
           03  FILLER           PIC X         VALUE " ".
           03  LOG-TIME.
               05  LOG-HH       PIC 99.
               05  LOG-COL1     PIC X.
               05  LOG-MM       PIC 99.
               05  LOG-COL2     PIC X.
               05  LOG-SS       PIC 99.
           03  FILLER           PIC X         VALUE " ".
           03  LOG-USERID       PIC X(8).
           03  FILLER           PIC X(2)      VALUE "  ".
           03  LOG-FILENAM      PIC X(25).

       01  LOG-DESC-REC.
           03  LOG-DESC-PRNO    PIC X(8).
           03  FILLER           PIC X         VALUE " ".
           03  LOG-DESC-DATE    PIC 99/99/99.
           03  FILLER           PIC X         VALUE " ".
           03  LOG-DESC-REL     PIC X(3).
           03  FILLER           PIC X         VALUE " ".
           03  LOG-DESC         PIC X(58).

       01  STAR-LINE        PIC X(80)          VALUE ALL "*".

       01  REVERSE-LOG-LINE.
           03  LOG-REV-PRNO  PIC X(8).
           03  FILLER        PIC X         VALUE " ".
           03  LOG-REV-DATE  PIC 99/99/99.
           03  FILLER        PIC X         VALUE " ".
           03  LOG-REV-REL   PIC X(3).
           03  FILLER        PIC X(11)          VALUE
           " REVERSAL: ".
           03  FILLER        PIC X(32)          VALUE
           "RESTORED ITEMS WITH SUFFIXES OF ".
           03  FILLER        PIC X              VALUE ".".
           03  LOG-REV-DATE2 PIC 9(6).
           03  FILLER        PIC X              VALUE ".".
           03  LOG-REV-PR2   PIC X(8).

      *    **********************************
      *    *      W-SCREEN DISPLAY BUFFER
      *    **********************************
       01  FLD-LIST        PIC X(5)   VALUE "FLD.".

       01  DISPLAY-BUF           VALUE SPACES.
           03  D-FIELD           OCCURS 15 TIMES.
               05  D-NO          PIC Z9.
               05  D-DOT         PIC X.
               05  D-PR          PIC X(8).
               05  FILLER        PIC X.
               05  D-MONTH       PIC X(3).
               05  FILLER        PIC X.
               05  D-DAY         PIC X(2).
               05  FILLER        PIC X.
               05  D-TIME        PIC X(5).
               05  FILLER        PIC X.
               05  D-USERID      PIC X(8).
               05  FILLER        PIC X.
               05  D-DESC        PIC X(45).
           03  D-END-MORE        PIC X(10).
           03  D-IMPORT-PATH     PIC X(20).

       01  DISPLAY-LIST.
           03  FILLER   PIC X(10) VALUE "01P:P 01F ".
           03  FILLER   PIC X(10) VALUE "02P:P 02F ".
           03  FILLER   PIC X(10) VALUE "03P:P 03F ".
           03  FILLER   PIC X(10) VALUE "04P:P 04F ".
           03  FILLER   PIC X(10) VALUE "05P:P 05F ".
           03  FILLER   PIC X(10) VALUE "06P:P 06F ".
           03  FILLER   PIC X(10) VALUE "07P:P 07F ".
           03  FILLER   PIC X(10) VALUE "08P:P 08F ".
           03  FILLER   PIC X(10) VALUE "09P:P 09F ".
           03  FILLER   PIC X(10) VALUE "10P:P 10F ".
           03  FILLER   PIC X(10) VALUE "11P:P 11F ".
           03  FILLER   PIC X(10) VALUE "12P:P 12F ".
           03  FILLER   PIC X(10) VALUE "13P:P 13F ".
           03  FILLER   PIC X(10) VALUE "14P:P 14F ".
           03  FILLER   PIC X(10) VALUE "15P:P 15F ".

           03  FILLER   PIC X(8)  VALUE "ENDMORE ".
           03  FILLER   PIC X(11) VALUE "IMPORTPATH.".

       01  SKIP-READ-FLAG        PIC X VALUE "N".
       01  COUNT1                PIC 9 VALUE 0.
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP-3 VALUE 0.
       01  SUB1                 PIC 99.
       01  SUB2                 PIC 99.
       01  LEGEND.
           03  LEGEND1          PIC X(17).
           03  LEGEND2          PIC X(56).
       01  LEGEND-LIST          PIC X(7)     VALUE "LEGEND.".
       01  TODAY-YYMMDD         PIC 9(06).
       01  TODAYS-DATEX REDEFINES TODAY-YYMMDD.
           03  TD-YY            PIC 9(02).
           03  TD-MM            PIC 9(02).
           03  TD-DD            PIC 9(02).
       01  TODAY-MMDDYY         PIC 9(6).

       01  ERRORS-EXIST        PIC X VALUE SPACES.
       01  DATED-COPIES-EXIST  PIC X VALUE "N".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  ERRCD           PIC X.
       01  VALID-REC-COUNT     PIC 9(5)  VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE            PIC X(48)    VALUE
           "- - - -   PROGRAM INSTALLATION REPORT    - - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  D-LINE1  REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(06).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".


       COPY "LIBGB/GETPRW.CPY".
      *    SIZE OF THE FOLLOWING ITEM LIMITED TO 29
      *    BY RM COBOL EXTERNAL FILE NAME CONVENTION;
      *    ALSO SEE PRU-PATHX BELOW.
           03  PRU-CHAR REDEFINES PRU-FILE.
               05  PRU-C   PIC X OCCURS 29.
       01  PRU-PATH-MAX    PIC 99 COMP VALUE 29.
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PRINT.CPY".
       COPY "LIBSP/INSTAL_CMD.CPY".
      *COPY "LIBGB/UNAME-N_CMD.CPY".
       01  UNAME-N-CMD        PIC X(8)  VALUE"UNAME -N".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       01  INSTAL-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/INSTAL_DEF.CPY".
       COPY "LIBSP/INSTAL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/INSTAL_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE LIST-FILE LOG-FILE
               SYS-NAME-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE LIST-FILE LOG-FILE
               SYS-NAME-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
       MAIN-ROUTINE.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "INSTAL" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS INSTAL-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-WK-FILE.
           MOVE 0 TO VALID-REC-COUNT.

       AUDIT-NEXT-WK.
           MOVE SPACES TO WK-REC.
           PERFORM READ-WK-FILE.
           IF IO-BAD OR WK-REC(1:1) = X"1A"
               GO TO COPY-IN-ROUTINE.

           IF NOT PR-DESC-VALID
            IF NOT PR-CO-VALID
              IF NOT PR-HELP-VALID
               IF NOT PR-SHELL-VALID
                 IF NOT PR-PCL-VALID
                  IF NOT PR-PST-VALID
                     IF NOT PR-SQL-VALID
                    MOVE "  ERROR" TO ERRMSG-HEAD
                    MOVE "INVALID LINE FORMAT IN INPUT FILE, REJECTED!"
                          TO ERRMSG-MSG1
                    MOVE WK-REC TO ERRMSG-MSG2
                    MOVE SPACES TO ERRMSG-MSG3
                    PERFORM CALL-ERRMSG
                    GO TO END-PROGRAM.

           IF PR-DESC-VALID
              GO TO AUDIT-NEXT-WK.

           IF PR-HELP-VALID
              MOVE 0 TO COUNT1
              INSPECT PR-HELP-PROG TALLYING COUNT1 FOR ALL ".TXT"
              IF COUNT1 = 0
                 MOVE "HELP MISSING .TXT SUFFIX" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-PROGRAM.

      * MAKE SURE CO, FM, HELP OR SH ON LINE EXISTS, FOR EXAMPLE:
      * IF S99, MAKE SURE /USR/PFSI/P3/NEW/PR1234/LCMAIN.F EXISTS
      * IF S35, MAKE SURE /USR/S99/FM/MN/LCMAIN EXISTS
      * IF REVERSE, CHECK WHETHER /USR/S99/FM/MN/LCMAIN EXISTS

           IF PR-CO-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 MOVE NEW-IMPORT-FILE-PATH TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY "/?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?     "
                                                 BY PR-CO-PROG
              ELSE IF COPY-TO-LIVE
                 MOVE HOLD-TEST-REL TO LOG-CO-REL
                 MOVE PR-CO-PROG    TO LOG-CO-PROG
                 MOVE LOG-CO-FILE   TO ACCESS-BUF
              ELSE
                 MOVE HOLD-LIVE-REL TO LOG-CO-REL
                 MOVE PR-CO-PROG    TO LOG-CO-PROG
                 MOVE LOG-CO-FILE   TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                 REV-YYMMDD
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2
              END-IF

           ELSE IF PR-HELP-VALID
                ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 MOVE NEW-IMPORT-FILE-PATH TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY "/?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?         "
                                                 BY PR-HELP-PROG
              ELSE IF COPY-TO-LIVE
                 MOVE HOLD-TEST-REL TO LOG-HELP-REL
                 MOVE PR-HELP-PROG  TO LOG-HELP-PROG
                 MOVE LOG-HELP-FILE TO ACCESS-BUF
              ELSE
                 MOVE HOLD-LIVE-REL TO LOG-HELP-REL
                 MOVE PR-HELP-PROG  TO LOG-HELP-PROG
                 MOVE LOG-HELP-FILE TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                   REV-YYMMDD
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2
              END-IF
           ELSE IF PR-SHELL-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 MOVE NEW-IMPORT-FILE-PATH TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY "/?"
                 INSPECT ACCESS-BUF REPLACING
                         FIRST "?                             "
                                                 BY PR-SHELL-NAME
              ELSE IF COPY-TO-LIVE
                 MOVE HOLD-TEST-REL TO LOG-SH-REL
                 MOVE PR-SHELL-NAME TO LOG-SH-NAME
                 MOVE LOG-SH-FILE   TO ACCESS-BUF
              ELSE
                 MOVE HOLD-LIVE-REL TO LOG-SH-REL
                 MOVE PR-SHELL-NAME TO LOG-SH-NAME
                 MOVE LOG-SH-FILE   TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                   REV-YYMMDD
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2
              END-IF

           ELSE IF PR-PCL-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 MOVE NEW-IMPORT-FILE-PATH TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY "/?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?           "
                                                 BY PR-PCL-NAME
              ELSE IF COPY-TO-LIVE
                 MOVE HOLD-TEST-REL TO LOG-PCL-REL
                 MOVE PR-PCL-NAME   TO LOG-PCL-NAME
                 MOVE LOG-PCL-FILE  TO ACCESS-BUF
              ELSE
                 MOVE HOLD-LIVE-REL TO LOG-PCL-REL
                 MOVE PR-PCL-NAME   TO LOG-PCL-NAME
                 MOVE LOG-PCL-FILE  TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                   REV-YYMMDD
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                               TEST-IMPORT-PATH2
              END-IF
           ELSE IF PR-PST-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 MOVE NEW-IMPORT-FILE-PATH TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY "/?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?           "
                                                 BY PR-PST-NAME
              ELSE IF COPY-TO-LIVE
                 MOVE HOLD-TEST-REL TO LOG-PST-REL
                 MOVE PR-PST-NAME   TO LOG-PST-NAME
                 MOVE LOG-PST-FILE  TO ACCESS-BUF
              ELSE
                 MOVE HOLD-LIVE-REL TO LOG-PST-REL
                 MOVE PR-PST-NAME   TO LOG-PST-NAME
                 MOVE LOG-PST-FILE  TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                   REV-YYMMDD
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                               TEST-IMPORT-PATH2
              END-IF
           ELSE IF PR-SQL-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 MOVE NEW-IMPORT-FILE-PATH TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY "/?"
                 INSPECT ACCESS-BUF
                      REPLACING FIRST "?                             "
                                                 BY PR-SQL-NAME
              ELSE IF COPY-TO-LIVE
                 MOVE HOLD-TEST-REL TO LOG-SQL-REL
                 MOVE PR-SQL-NAME   TO LOG-SQL-NAME
                 MOVE LOG-SQL-FILE  TO ACCESS-BUF
              ELSE
                 MOVE HOLD-LIVE-REL TO LOG-SQL-REL
                 MOVE PR-SQL-NAME   TO LOG-SQL-NAME
                 MOVE LOG-SQL-FILE  TO ACCESS-BUF
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                   REV-YYMMDD
                 INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
                 INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                               TEST-IMPORT-PATH2.

           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "MISSING FILE IN LIST:" TO MESS
              MOVE ACCESS-BUF TO HOLD-PATH-ERR
              INSPECT MESS REPLACING FIRST "  " BY " ?"
              INSPECT MESS REPLACING FIRST
                     "?                                      "
                      BY HOLD-PATH-ERR
              PERFORM SEND-MESS
              IF REV-FROM-LIVE
                 GO TO AUDIT-NEXT-WK
              ELSE
                GO TO END-PROGRAM.

      *-MAKE SURE BACKUP DOES NOT ALREADY EXIST FOR TODAY
           IF COPY-TO-TEST
              GO TO AUDIT-NEXT-WK.
           IF REV-FROM-LIVE
              GO TO AUDIT-NEXT-WK.
           IF DATED-COPIES-EXIST = "Y"
              GO TO AUDIT-NEXT-WK.

           IF PR-CO-VALID
              MOVE HOLD-LIVE-REL  TO LOG-CO-REL
              MOVE PR-CO-PROG     TO LOG-CO-PROG
              MOVE LOG-CO-FILE    TO ACCESS-BUF
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                 TODAY-YYMMDD
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                            TEST-IMPORT-PATH2
           ELSE IF PR-HELP-VALID
              MOVE HOLD-LIVE-REL  TO LOG-HELP-REL
              MOVE PR-HELP-PROG   TO LOG-HELP-PROG
              MOVE LOG-HELP-FILE  TO ACCESS-BUF
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                 TODAY-YYMMDD
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                            TEST-IMPORT-PATH2
           ELSE IF PR-SHELL-VALID
              MOVE HOLD-LIVE-REL  TO LOG-SH-REL
              MOVE PR-SHELL-NAME  TO LOG-SH-NAME
              MOVE LOG-SH-FILE    TO ACCESS-BUF
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                 TODAY-YYMMDD
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                            TEST-IMPORT-PATH2
           ELSE IF PR-PCL-VALID
              MOVE HOLD-LIVE-REL  TO LOG-PCL-REL
              MOVE PR-PCL-NAME    TO LOG-PCL-NAME
              MOVE LOG-PCL-FILE   TO ACCESS-BUF
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                 TODAY-YYMMDD
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                            TEST-IMPORT-PATH2
           ELSE IF PR-PST-VALID
              MOVE HOLD-LIVE-REL  TO LOG-PST-REL
              MOVE PR-PST-NAME    TO LOG-PST-NAME
              MOVE LOG-PST-FILE   TO ACCESS-BUF
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                 TODAY-YYMMDD
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                            TEST-IMPORT-PATH2
           ELSE IF PR-SQL-VALID
              MOVE HOLD-LIVE-REL  TO LOG-SQL-REL
              MOVE PR-SQL-NAME    TO LOG-SQL-NAME
              MOVE LOG-SQL-FILE   TO ACCESS-BUF
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                                 TODAY-YYMMDD
              INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
              INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                            TEST-IMPORT-PATH2.

           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO AUDIT-NEXT-WK.

           MOVE "Y" TO DATED-COPIES-EXIST.
           MOVE ACCESS-BUF TO MESS.
           INSPECT MESS REPLACING FIRST "                " BY
                                        " ALREADY EXISTS!".
           PERFORM SEND-MESS.
           MOVE SPACES TO LEGEND1
           MOVE
           "DO YOU WANT TO PROCEED ANYWAY?  (Y/N)               :   "
                                                            TO LEGEND2.
           PERFORM SEND-LEGEND.
       PROCEED-YN.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO PROCEED-YN.
           IF VDUBUF = "     N"
              GO TO END-PROGRAM.
           IF VDUBUF = "     Y"
              GO TO AUDIT-NEXT-WK.
           GO TO PROCEED-YN.

       COPY-IN-ROUTINE.
           IF VALID-REC-COUNT = 0
              MOVE "INSTALL LIST CONTAINS NO FILES TO INSTALL!"
                 TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.
           MOVE 0 TO VALID-REC-COUNT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
       NEXT-WK.
           MOVE SPACES TO WK-REC.
           PERFORM READ-WK-FILE.
           IF (IO-BAD OR WK-REC(1:1) = X"1A")
              PERFORM WRITE-DETAIL-LINE
              IF HOLD-MACHINE = R1
                 MOVE TEMP-LOG-PATH      TO CAT-LOG-R1-MKTEMP
                 MOVE CAT-LOG-COMMAND-R1 TO SYSTEM-BUF
                 PERFORM SYSTEM-CALL
                 IF STAT NOT = "00"
                    MOVE STAT TO E-CODE
                    MOVE "SYSTEM-CALL: CAT-LOG-COMMAND-R1" TO E-FILE
                    GO TO IO-ERROR
                 END-IF
              END-IF
      *       IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
      *       PERFORM CREATE-TAG
      *       END-IF
              GO TO END-ROUTINE.

           IF PR-DESC-VALID
              ADD 1 TO VALID-REC-COUNT
              IF VALID-REC-COUNT < 2
                 MOVE STAR-LINE TO LOG-REC DETAIL-LINE
                 PERFORM WRITE-LOG-FILE
                 IF IO-BAD
                    GO TO IO-ERROR
                 END-IF
                 PERFORM WRITE-DETAIL-LINE
                 IF REV-FROM-LIVE
                    MOVE D-PR(PR-NO)      TO LOG-REV-PRNO LOG-REV-PR2
                    MOVE TODAY-MMDDYY     TO LOG-REV-DATE
                    MOVE REV-YYMMDD       TO LOG-REV-DATE2
                    MOVE HOLD-LIVE-REL    TO LOG-REV-REL
                    MOVE REVERSE-LOG-LINE TO LOG-REC DETAIL-LINE
                    PERFORM WRITE-LOG-FILE
                    IF IO-BAD
                       GO TO IO-ERROR
                    END-IF
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
              END-IF
              MOVE D-PR(PR-NO)  TO LOG-DESC-PRNO
              MOVE TODAY-MMDDYY TO LOG-DESC-DATE
              IF COPY-TO-TEST
                 MOVE HOLD-TEST-REL TO LOG-DESC-REL
              ELSE
                 MOVE HOLD-LIVE-REL TO LOG-DESC-REL
              END-IF
              MOVE PR-DESC TO LOG-DESC
              MOVE LOG-DESC-REC TO LOG-REC DETAIL-LINE
              PERFORM WRITE-LOG-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              END-IF
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-WK.

           IF PR-CO-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 PERFORM COPY-TEST-CO
                 GO TO NEXT-WK
              ELSE IF COPY-TO-LIVE
                 PERFORM COPY-LIVE-CO
                 GO TO NEXT-WK
              ELSE
                 PERFORM REVERSE-LIVE-CO
                 GO TO NEXT-WK.

           IF PR-HELP-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 PERFORM COPY-TEST-HELP
                 GO TO NEXT-WK
              ELSE IF COPY-TO-LIVE
                 PERFORM COPY-LIVE-HELP
                 GO TO NEXT-WK
              ELSE
                 PERFORM REVERSE-LIVE-HELP
                 GO TO NEXT-WK.

           IF PR-SHELL-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 PERFORM COPY-TEST-SH
                 GO TO NEXT-WK
              ELSE IF COPY-TO-LIVE
                 PERFORM COPY-LIVE-SH
                 GO TO NEXT-WK
              ELSE
                 PERFORM REVERSE-LIVE-SH
                 GO TO NEXT-WK.

           IF PR-PCL-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 PERFORM COPY-TEST-PCL
                 GO TO NEXT-WK
              ELSE IF COPY-TO-LIVE
                 PERFORM COPY-LIVE-PCL
                 GO TO NEXT-WK
              ELSE
                 PERFORM REVERSE-LIVE-PCL
                 GO TO NEXT-WK.

           IF PR-PST-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 PERFORM COPY-TEST-PST
                 GO TO NEXT-WK
              ELSE IF COPY-TO-LIVE
                 PERFORM COPY-LIVE-PST
                 GO TO NEXT-WK
              ELSE
                 PERFORM REVERSE-LIVE-PST
                 GO TO NEXT-WK.

           IF PR-SQL-VALID
              ADD 1 TO VALID-REC-COUNT
              IF COPY-TO-TEST
                 PERFORM COPY-TEST-SQL
                 GO TO NEXT-WK
              ELSE IF COPY-TO-LIVE
                 PERFORM COPY-LIVE-SQL
                 GO TO NEXT-WK
              ELSE
                 PERFORM REVERSE-LIVE-SQL
                 GO TO NEXT-WK.

           GO TO NEXT-WK.

       END-ROUTINE.
           PERFORM CLOSE-WK-FILE.
           IF VALID-REC-COUNT = 0
              GO TO END-PROGRAM.

           IF COPY-TO-TEST
              PERFORM MOVE-NEW-TO-TEST
           ELSE IF COPY-TO-LIVE
              PERFORM MOVE-TEST-TO-DONE
           ELSE
              PERFORM MOVE-DONE-TO-TEST.

           GO TO END-PROGRAM.

      ******************************************************************
      * COPY CO FROM /USR/PFSI/P3/NEW/PR123/PROG TO /USR/S99/CO/DIR/PROG
      * CALLING SH/INSTAL.SH
      * S35/SH INSTAL.SH COPIES TO S35 AND S99
      ******************************************************************
       COPY-TEST-CO SECTION.
           MOVE PR-CO-PROG       TO TEST-CO-FILENAM.
           MOVE NEW-IMPORT-PATH2 TO TEST-CO-SOURCE.
           INSPECT TEST-CO-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT TEST-CO-SOURCE REPLACING FIRST "?     "
                                           BY PR-CO-PROG.
           MOVE 0                TO TEST-CO-YYMMDD.
           MOVE TEST-CO-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: TEST-CO-COMMAND" TO E-FILE
              MOVE TEST-CO-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES           TO LOG-WORK.
           MOVE HOLD-TEST-REL    TO LOG-CO-REL.
           MOVE PR-CO-PROG       TO LOG-CO-PROG.
           MOVE LOG-CO-FILE      TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY HELP FROM /USR/PFSI/P3/NEW/PR123/PROG.H
      *                                     TO /USR/S99/HELP/DIR/PROG
      ******************************************************************
       COPY-TEST-HELP SECTION.
           MOVE NEW-IMPORT-PATH2 TO TEST-HELP-SOURCE.
           INSPECT TEST-HELP-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT TEST-HELP-SOURCE REPLACING FIRST "?         "
                                           BY PR-HELP-PROG.
           MOVE PR-HELP-PROG       TO TEST-HELP-FILENAM.
           MOVE 0                TO TEST-HELP-YYMMDD.
           MOVE TEST-HELP-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: TEST-HELP-COMMAND" TO E-FILE
              MOVE TEST-HELP-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES           TO LOG-WORK.
           MOVE HOLD-TEST-REL    TO LOG-HELP-REL.
           MOVE PR-HELP-PROG     TO LOG-HELP-PROG.
           MOVE LOG-HELP-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY SHELL FROM /USR/PFSI/PRS TO /USR/S99/SH
      ******************************************************************
       COPY-TEST-SH SECTION.
           MOVE PR-SHELL-NAME  TO TEST-SH-FILENAM.
           MOVE NEW-IMPORT-PATH2 TO TEST-SH-SOURCE.
           INSPECT TEST-SH-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT TEST-SH-SOURCE REPLACING
                         FIRST "?                             "
                                           BY PR-SHELL-NAME.
           MOVE 0              TO TEST-SH-YYMMDD.
           MOVE TEST-SH-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: TEST-SH-COMMAND" TO E-FILE
              MOVE TEST-SH-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES         TO LOG-WORK.
           MOVE HOLD-TEST-REL  TO LOG-SH-REL.
           MOVE PR-SHELL-NAME  TO LOG-SH-NAME.
           MOVE LOG-SH-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY PCL FROM /USR/PFSI/PR TO /USR/S99/PCL
      ******************************************************************
       COPY-TEST-PCL SECTION.
           MOVE PR-PCL-NAME      TO TEST-PCL-FILENAM.
           MOVE NEW-IMPORT-PATH2 TO TEST-PCL-SOURCE.
           INSPECT TEST-PCL-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT TEST-PCL-SOURCE REPLACING FIRST "?           "
                                           BY PR-PCL-NAME.
           MOVE 0                TO TEST-PCL-YYMMDD.
           MOVE TEST-PCL-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: TEST-PCL-COMMAND" TO E-FILE
              MOVE TEST-PCL-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES         TO LOG-WORK.
           MOVE HOLD-TEST-REL  TO LOG-PCL-REL.
           MOVE PR-PCL-NAME    TO LOG-PCL-NAME.
           MOVE LOG-PCL-FILE   TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY PST FROM /USR/PFSI/PR TO /USR/S99/PST
      ******************************************************************
       COPY-TEST-PST SECTION.
           MOVE PR-PST-NAME      TO TEST-PST-FILENAM.
           MOVE NEW-IMPORT-PATH2 TO TEST-PST-SOURCE.
           INSPECT TEST-PST-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT TEST-PST-SOURCE REPLACING FIRST "?           "
                                           BY PR-PST-NAME.
           MOVE 0                TO TEST-PST-YYMMDD.
           MOVE TEST-PST-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: TEST-PST-COMMAND" TO E-FILE
              MOVE TEST-PST-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES         TO LOG-WORK.
           MOVE HOLD-TEST-REL  TO LOG-PST-REL.
           MOVE PR-PST-NAME    TO LOG-PST-NAME.
           MOVE LOG-PST-FILE   TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY SQL FROM /USR/PFSI/PR TO /USR/S99/SQL
      ******************************************************************
       COPY-TEST-SQL SECTION.
           MOVE PR-SQL-NAME      TO TEST-SQL-FILENAM.
           MOVE NEW-IMPORT-PATH2 TO TEST-SQL-SOURCE.
           INSPECT TEST-SQL-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT TEST-SQL-SOURCE
                   REPLACING FIRST "?                             "
                   BY PR-SQL-NAME.
           MOVE 0                TO TEST-SQL-YYMMDD.
           MOVE TEST-SQL-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: TEST-SQL-COMMAND" TO E-FILE
              MOVE TEST-SQL-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES         TO LOG-WORK.
           MOVE HOLD-TEST-REL  TO LOG-SQL-REL.
           MOVE PR-SQL-NAME    TO LOG-SQL-NAME.
           MOVE LOG-SQL-FILE   TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY CO FROM /USR/PFSI/PRS TO /USR/A3?/CO/DIR
      * IF NEW PROGRAM MOVE 0 TO YYMMDD & SHELL WON'T TRY TO COPY TO DATED
      * IF DATED COPY EXISTS MOVE 0 TO YYMMDD & SHELL WILL NOT COPY
      *                                             OVER EXISTING COPY
      ******************************************************************
       COPY-LIVE-CO SECTION.
           MOVE TODAY-YYMMDD TO LIVE-CO-YYMMDD.

           MOVE HOLD-LIVE-REL  TO LOG-CO-REL.
           MOVE PR-CO-PROG     TO LOG-CO-PROG.
           MOVE LOG-CO-FILE    TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE 0           TO LIVE-CO-YYMMDD.

           MOVE HOLD-LIVE-REL  TO LOG-CO-REL.
           MOVE PR-CO-PROG     TO LOG-CO-PROG.
           MOVE LOG-CO-FILE    TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                              TODAY-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE 0            TO LIVE-CO-YYMMDD.

           MOVE PR-CO-PROG      TO LIVE-CO-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO LIVE-CO-SOURCE.
           MOVE LIVE-CO-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "ERROR" TO E-MSG
              MOVE "SYSTEM-CALL: LIVE-CO-COMMAND" TO E-FILE
              MOVE LIVE-CO-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-CO-REL.
           MOVE PR-CO-PROG      TO LOG-CO-PROG.
           MOVE LOG-CO-FILE     TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      *********************************************************************
      * COPY HELP FROM /USR/S99/HELP TO /USR/S35/HELP
      * IF NEW PROGRAM MOVE 0 TO YYMMDD & SHELL WON'T TRY TO COPY TO DATED
      * IF DATED COPY EXISTS MOVE 0 TO YYMMDD & SHELL WILL NOT COPY
      *                                             OVER EXISTING COPY
      *********************************************************************
       COPY-LIVE-HELP SECTION.
           MOVE TODAY-YYMMDD   TO LIVE-HELP-YYMMDD.
           MOVE HOLD-LIVE-REL  TO LOG-HELP-REL.
           MOVE PR-HELP-PROG   TO LOG-HELP-PROG.
           MOVE LOG-HELP-FILE  TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE 0           TO LIVE-HELP-YYMMDD.

           MOVE HOLD-LIVE-REL  TO LOG-HELP-REL.
           MOVE PR-HELP-PROG   TO LOG-HELP-PROG.
           MOVE LOG-HELP-FILE  TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                              TODAY-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE 0           TO LIVE-HELP-YYMMDD.

           MOVE PR-HELP-PROG      TO LIVE-HELP-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO LIVE-HELP-SOURCE.
           MOVE LIVE-HELP-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: LIVE-HELP-COMMAND" TO E-FILE
              MOVE LIVE-HELP-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES           TO LOG-WORK.
           MOVE HOLD-LIVE-REL    TO LOG-HELP-REL.
           MOVE PR-HELP-PROG     TO LOG-HELP-PROG.
           MOVE LOG-HELP-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY SHELL FROM /USR/PFSI/PRS TO /USR/S35/SH
      * IF NEW PROGRAM MOVE 0 TO YYMMDD & SHELL WON'T TRY TO COPY TO DATED
      * IF DATED COPY EXISTS MOVE 0 TO YYMMDD & SHELL WILL NOT COPY
      *                                             OVER EXISTING COPY
      ******************************************************************
       COPY-LIVE-SH SECTION.
           MOVE TODAY-YYMMDD   TO LIVE-SH-YYMMDD.
           MOVE HOLD-LIVE-REL  TO LOG-SH-REL.
           MOVE PR-SHELL-NAME  TO LOG-SH-NAME.
           MOVE LOG-SH-FILE    TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE 0           TO LIVE-SH-YYMMDD.

           MOVE HOLD-LIVE-REL  TO LOG-SH-REL.
           MOVE PR-SHELL-NAME  TO LOG-SH-NAME.
           MOVE LOG-SH-FILE    TO ACCESS-BUF
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                              TODAY-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE 0            TO LIVE-SH-YYMMDD.

           MOVE PR-SHELL-NAME   TO LIVE-SH-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO LIVE-SH-SOURCE.
           MOVE LIVE-SH-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: LIVE-SH-COMMAND" TO E-FILE
              MOVE LIVE-SH-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-SH-REL.
           MOVE PR-SHELL-NAME   TO LOG-SH-NAME.
           MOVE LOG-SH-FILE     TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY PCL FROM /USR/PFSI/PRS TO /USR/S35/PCL
      * IF NEW PROGRAM MOVE 0 TO YYMMDD & SHELL WON'T TRY TO COPY TO DATED
      * IF DATED COPY EXISTS MOVE 0 TO YYMMDD & SHELL WILL NOT COPY
      *                                             OVER EXISTING COPY
      ******************************************************************
       COPY-LIVE-PCL SECTION.
           MOVE TODAY-YYMMDD   TO LIVE-PCL-YYMMDD.
           MOVE HOLD-LIVE-REL  TO LOG-PCL-REL
           MOVE PR-PCL-NAME    TO LOG-PCL-NAME
           MOVE LOG-PCL-FILE   TO ACCESS-BUF
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE 0           TO LIVE-PCL-YYMMDD.

           MOVE HOLD-LIVE-REL  TO LOG-PCL-REL
           MOVE PR-PCL-NAME    TO LOG-PCL-NAME
           MOVE LOG-PCL-FILE   TO ACCESS-BUF
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                              TODAY-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE 0            TO LIVE-PCL-YYMMDD.

           MOVE PR-PCL-NAME     TO LIVE-PCL-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO LIVE-PCL-SOURCE.
           MOVE LIVE-PCL-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: LIVE-PCL-COMMAND" TO E-FILE
              MOVE LIVE-PCL-CMD2 TO E-KEYX
              GO TO IO-ERROR.

           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-PCL-REL.
           MOVE PR-PCL-NAME     TO LOG-PCL-NAME.
           MOVE LOG-PCL-FILE     TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY PST FROM /USR/PFSI/PRS TO /USR/A3?/PST
      * IF NEW PROGRAM MOVE 0 TO YYMMDD & SHELL WON'T TRY TO COPY TO DATED
      * IF DATED COPY EXISTS MOVE 0 TO YYMMDD & SHELL WILL NOT COPY
      *                                             OVER EXISTING COPY
      ******************************************************************
       COPY-LIVE-PST SECTION.
           MOVE TODAY-YYMMDD   TO LIVE-PST-YYMMDD.
           MOVE HOLD-LIVE-REL  TO LOG-PST-REL
           MOVE PR-PST-NAME    TO LOG-PST-NAME
           MOVE LOG-PST-FILE   TO ACCESS-BUF
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE 0           TO LIVE-PST-YYMMDD.

           MOVE HOLD-LIVE-REL  TO LOG-PST-REL
           MOVE PR-PST-NAME    TO LOG-PST-NAME
           MOVE LOG-PST-FILE   TO ACCESS-BUF
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                              TODAY-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE 0            TO LIVE-PST-YYMMDD.

           MOVE PR-PST-NAME     TO LIVE-PST-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO LIVE-PST-SOURCE.
           MOVE LIVE-PST-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: LIVE-PST-COMMAND" TO E-FILE
              MOVE LIVE-PST-CMD2 TO E-KEYX
              GO TO IO-ERROR.

           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-PST-REL.
           MOVE PR-PST-NAME     TO LOG-PST-NAME.
           MOVE LOG-PST-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY SQL FROM /USR/PFSI/PRS TO /USR/A3?/SQL
      * IF NEW PROGRAM MOVE 0 TO YYMMDD & SHELL WON'T TRY TO COPY TO DATED
      * IF DATED COPY EXISTS MOVE 0 TO YYMMDD & SHELL WILL NOT COPY
      *                                             OVER EXISTING COPY
      ******************************************************************
       COPY-LIVE-SQL SECTION.
           MOVE TODAY-YYMMDD   TO LIVE-SQL-YYMMDD.
           MOVE HOLD-LIVE-REL  TO LOG-SQL-REL.
           MOVE PR-SQL-NAME    TO LOG-SQL-NAME.
           MOVE LOG-SQL-FILE   TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE 0           TO LIVE-SQL-YYMMDD.

           MOVE HOLD-LIVE-REL  TO LOG-SQL-REL.
           MOVE PR-SQL-NAME    TO LOG-SQL-NAME.
           MOVE LOG-SQL-FILE   TO ACCESS-BUF
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?"
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                              TODAY-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                              TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE 0            TO LIVE-SQL-YYMMDD.

           MOVE PR-SQL-NAME     TO LIVE-SQL-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO LIVE-SQL-SOURCE.
           MOVE LIVE-SQL-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: LIVE-SQL-COMMAND" TO E-FILE
              MOVE LIVE-SQL-CMD2 TO E-KEYX
              GO TO IO-ERROR.

           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-SQL-REL.
           MOVE PR-SQL-NAME     TO LOG-SQL-NAME.
           MOVE LOG-SQL-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

      ******************************************************************
      * COPY /USR/A3?/CO/DIR/PROG.YYMMDD.PR123 TO /USR/A3?/CO/DIR/PROG
      * NOTE, IF NEW PROGRAM, DATED COPY MAY NOT EXIST, THEN SKIP
      ******************************************************************
       REVERSE-LIVE-CO SECTION.
           MOVE HOLD-LIVE-REL  TO LOG-CO-REL.
           MOVE PR-CO-PROG     TO LOG-CO-PROG.
           MOVE LOG-CO-FILE    TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                           REV-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                        TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO EXIT-REV-LIVE-CO.

           MOVE REV-YYMMDD        TO REV-CO-YYMMDD.
           MOVE PR-CO-PROG        TO REV-CO-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO REV-CO-SOURCE.
           MOVE REV-CO-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "ERROR" TO E-MSG
              MOVE "SYSTEM-CALL: REV-CO-COMMAND" TO E-FILE
              MOVE REV-CO-CMD2 TO E-KEYX
              GO TO IO-ERROR.

           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-CO-REL.
           MOVE PR-CO-PROG      TO LOG-CO-PROG.
           MOVE LOG-CO-FILE     TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.
       EXIT-REV-LIVE-CO.
           EXIT.

      ************************************************************************
      * COPY /USR/A3?/HELP/HELP.YYMMDD.PR123 TO /USR/A3?/HELP/HELP
      ************************************************************************
       REVERSE-LIVE-HELP SECTION.
           MOVE HOLD-LIVE-REL  TO LOG-HELP-REL.
           MOVE PR-HELP-PROG   TO LOG-HELP-PROG.
           MOVE LOG-HELP-FILE  TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                             REV-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                        TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO REV-LIVE-HELP-EXIT.

           MOVE REV-YYMMDD     TO REV-HELP-YYMMDD.
           MOVE PR-HELP-PROG   TO REV-HELP-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO REV-HELP-SOURCE.
           MOVE REV-HELP-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REV-HELP-COMMAND" TO E-FILE
              MOVE REV-HELP-CMD2 TO E-KEYX
              GO TO IO-ERROR.

           MOVE SPACES           TO LOG-WORK.
           MOVE HOLD-LIVE-REL    TO LOG-HELP-REL.
           MOVE PR-HELP-PROG     TO LOG-HELP-PROG.
           MOVE LOG-HELP-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.

       REV-LIVE-HELP-EXIT.
           EXIT.

      *********************************************************************
      * COPY /USR/A3?/SH/SHELL.YYMMDD.PR123 TO /USR/A3?/SH/SHELL
      *********************************************************************
       REVERSE-LIVE-SH SECTION.
           MOVE HOLD-LIVE-REL  TO LOG-SH-REL.
           MOVE PR-SHELL-NAME  TO LOG-SH-NAME.
           MOVE LOG-SH-FILE    TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                             REV-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                        TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO REV-LIVE-SH-EXIT.

           MOVE REV-YYMMDD        TO REV-SH-YYMMDD.
           MOVE PR-SHELL-NAME     TO REV-SH-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO REV-SH-SOURCE.
           MOVE REV-SH-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REV-SH-COMMAND" TO E-FILE
              MOVE REV-SH-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-SH-REL.
           MOVE PR-SHELL-NAME   TO LOG-SH-NAME.
           MOVE LOG-SH-FILE     TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.
       REV-LIVE-SH-EXIT.
           EXIT.

      ******************************************************************
      * COPY /USR/A3?/PCL/PCL.YYMMDD.PR123 TO /USR/A3?/PCL/PCL
      ******************************************************************
       REVERSE-LIVE-PCL SECTION.
           MOVE HOLD-LIVE-REL TO LOG-PCL-REL.
           MOVE PR-PCL-NAME   TO LOG-PCL-NAME.
           MOVE LOG-PCL-FILE  TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                             REV-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                         TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO REV-LIVE-PCL-EXIT.

           MOVE REV-YYMMDD        TO REV-PCL-YYMMDD.
           MOVE PR-PCL-NAME       TO REV-PCL-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO REV-PCL-SOURCE.
           MOVE REV-PCL-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REV-PCL-COMMAND" TO E-FILE
              MOVE REV-PCL-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-PCL-REL.
           MOVE PR-PCL-NAME     TO LOG-PCL-NAME.
           MOVE LOG-PCL-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.
       REV-LIVE-PCL-EXIT.
           EXIT.

      ******************************************************************
      * COPY /USR/A3?/PCL/PCL.YYMMDD.PR123 TO /USR/A3?/PCL/PCL
      ******************************************************************
       REVERSE-LIVE-PST SECTION.
           MOVE HOLD-LIVE-REL TO LOG-PST-REL.
           MOVE PR-PST-NAME   TO LOG-PST-NAME.
           MOVE LOG-PST-FILE  TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                             REV-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                         TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO REV-LIVE-PST-EXIT.

           MOVE REV-YYMMDD        TO REV-PST-YYMMDD.
           MOVE PR-PST-NAME       TO REV-PST-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO REV-PST-SOURCE.
           MOVE REV-PST-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REV-PST-COMMAND" TO E-FILE
              MOVE REV-PST-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-PST-REL.
           MOVE PR-PST-NAME     TO LOG-PST-NAME.
           MOVE LOG-PST-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.
       REV-LIVE-PST-EXIT.
           EXIT.

      ******************************************************************
      * COPY /USR/S3?/SQL/SQL.YYMMDD.PR123 TO /USR/S9?/SQL/SQL
      ******************************************************************
       REVERSE-LIVE-SQL SECTION.
           MOVE HOLD-LIVE-REL TO LOG-SQL-REL.
           MOVE PR-SQL-NAME   TO LOG-SQL-NAME.
           MOVE LOG-SQL-FILE  TO ACCESS-BUF.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?     " BY
                                             REV-YYMMDD.
           INSPECT ACCESS-BUF REPLACING FIRST "  " BY ".?".
           INSPECT ACCESS-BUF REPLACING FIRST "?       " BY
                                         TEST-IMPORT-PATH2.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              GO TO REV-LIVE-SQL-EXIT.

           MOVE REV-YYMMDD        TO REV-SQL-YYMMDD.
           MOVE PR-SQL-NAME       TO REV-SQL-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO REV-SQL-SOURCE.
           MOVE REV-SQL-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "ERROR" TO E-MSG
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REV-SQL-COMMAND" TO E-FILE
              MOVE REV-SQL-CMD2 TO E-KEYX
              GO TO IO-ERROR.
           MOVE SPACES          TO LOG-WORK.
           MOVE HOLD-LIVE-REL   TO LOG-SQL-REL.
           MOVE PR-SQL-NAME     TO LOG-SQL-NAME.
           MOVE LOG-SQL-FILE    TO LOG-FILENAM.
           PERFORM ADD-TO-TMP-LOG-FILE.
       REV-LIVE-SQL-EXIT.
           EXIT.

      ******************************************************************
      * CREATE DIRECTORY IN TEST, EG MKDIR /USR/PFSI/P3/TEST/PR1234
      *
      * MOVE INSTALL SHELL FROM NEW TO TEST, FOR EXAMPLE:
      * MV /USR/PFSI/NEW/PR1234/PR1234 /USR/PFSI/TEST/PR1234/PR1234
      *
      * THEN, REMOVE CO'S, FM'S, HELP'S & SHELLS FROM NEW, EG:
      * RM /USR/PFSI/P3/TEST/PR1234/*
      *
      * THEN REMOVE EMPTY DIRECTORY, EG  RMDIR /USR/PFSI/P3/NEW/PR1234
      ******************************************************************
       MOVE-NEW-TO-TEST SECTION.
           MOVE LS-TEST-PATH     TO TEST-IMPORT-PATH1.
           MOVE D-PR(PR-NO)      TO TEST-IMPORT-PATH2.
           CALL "C$MAKEDIR" USING TEST-IMPORT-FILE-PATH GIVING NUM-STAT.
           MOVE "NA"             TO MOVE-TO-TEST-FILENAM.
           MOVE NEW-IMPORT-PATH2 TO MOVE-TO-TEST-SOURCE.
           INSPECT MOVE-TO-TEST-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT MOVE-TO-TEST-SOURCE REPLACING FIRST "?   "
                                           BY LOWER-CASE-LIST.
           MOVE 0                TO MOVE-TO-TEST-YYMMDD.
           MOVE MOVE-TO-TEST-COMMAND   TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: MOVE-TO-TEST-COMMAND" TO E-FILE
              GO TO IO-ERROR.
           IF (SYSTEM-NAME = PFSIMO OR SYSTEM-NAME = PFSIMI
               OR SYSTEM-NAME-1ST-5 = WORLD)
                 GO TO MOVE-NEW-TO-TEST-EXIT.

           MOVE SPACES TO LEGEND1
           MOVE
           "REMOVE LIST & PROGRAMS FROM NEW DIRECTORY? (Y/N)    :   "
                                                            TO LEGEND2.
           PERFORM SEND-LEGEND.
       REMOVE-FROM-NEW-YN.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010FLD." TO VDU.
           PERFORM SCREENIO.
           IF NOT ENTER-KEY
              GO TO REMOVE-FROM-NEW-YN.
           IF VDUBUF = "     N"
              GO TO MOVE-NEW-TO-TEST-EXIT.
           IF VDUBUF NOT = "     Y"
              GO TO REMOVE-FROM-NEW-YN.
           MOVE "RM" TO RM-BUF.
           CALL "C$TOLOWER" USING RM-BUF, VALUE 2.
           MOVE NEW-IMPORT-FILE-PATH TO RM-FILE.
           INSPECT RM-FILE REPLACING FIRST "  " BY "/*".
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE NEW FILES" TO E-FILE
              MOVE SYSTEM-BUF TO E-KEYX
              GO TO IO-ERROR.

           MOVE NEW-IMPORT-FILE-PATH TO REM-DIR-FILENAM.
           MOVE "NA"                 TO REM-DIR-SOURCE.
           MOVE 0                    TO REM-DIR-YYMMDD.
           MOVE REMOVE-DIRECTORY-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE NEW DIRECTORY" TO E-FILE
              MOVE REM-DIRECTORY-CMD2 TO E-KEYX
              GO TO IO-ERROR.
       MOVE-NEW-TO-TEST-EXIT.
           EXIT.

      ******************************************************************
      * CREATE DIRECTORY IN DONE, EG MKDIR /USR/PFSI/P3/DONE/PR1234
      *
      * THEN, MOVE INSTALL SHELL FROM TEST TO DONE, FOR EXAMPLE:
      * MV /USR/PFSI/TEST/PR1234/PR1234 /USR/PFSI/DONE/PR1234/PR1234
      *
      * THEN REMOVE SHELL IN TEST, EG    RM /USR/PFSI/P3/TEST/PR1234/*
      * THEN REMOVE EMPTY DIRECTORY, EG  RMDIR /USR/PFSI/P3/TEST/PR1234
      ******************************************************************
       MOVE-TEST-TO-DONE SECTION.
           MOVE LS-DONE-PATH      TO DONE-PATH1.
           MOVE D-PR(PR-NO)       TO DONE-PATH2.
           CALL "C$MAKEDIR" USING DONE-FILE-PATH GIVING NUM-STAT.
           MOVE "NA"              TO MOVE-TO-DONE-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO MOVE-TO-DONE-SOURCE.
           INSPECT MOVE-TO-DONE-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT MOVE-TO-DONE-SOURCE REPLACING FIRST "?   "
                                           BY LOWER-CASE-LIST.
           MOVE 0                 TO MOVE-TO-DONE-YYMMDD.
           MOVE MOVE-TO-DONE-COMMAND   TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: MOVE-TO-DONE-COMMAND" TO E-FILE
              MOVE SYSTEM-BUF TO E-KEYX
              GO TO IO-ERROR.

           IF SYSTEM-NAME = PFSIMO
              GO TO ASK-REMOVE-FROM-TEST.

           IF SYSTEM-NAME-1ST-5 NOT = WORLD
              GO TO REMOVE-FROM-TEST.

           IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
              IF OTHER-MACHINE-DONE = "N"
                 GO TO MOVE-TEST-TO-DONE-EXIT
              ELSE
                 GO TO REMOVE-FROM-TEST.

       ASK-REMOVE-FROM-TEST.
           MOVE SPACES TO LEGEND1
           MOVE
           "REMOVE INSTALL LIST FROM TEST DIRECTORY? (Y/N)         :"
                                                            TO LEGEND2.
           PERFORM SEND-LEGEND.
       REMOVE-FROM-TEST-YN.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010FLD." TO VDU.
           PERFORM SCREENIO.
           IF NOT ENTER-KEY
              GO TO REMOVE-FROM-TEST-YN.
           IF VDUBUF = "     N"
              GO TO MOVE-TEST-TO-DONE-EXIT.
           IF VDUBUF NOT = "     Y"
              GO TO REMOVE-FROM-TEST-YN.

       REMOVE-FROM-TEST.
           MOVE "RM" TO RM-BUF.
           CALL "C$TOLOWER" USING RM-BUF, VALUE 2.
           MOVE TEST-IMPORT-FILE-PATH TO RM-FILE.
           INSPECT RM-FILE REPLACING FIRST "  " BY "/*".
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE TEST FILE" TO E-FILE
              GO TO IO-ERROR.
           MOVE TEST-IMPORT-FILE-PATH TO REM-DIR-FILENAM.
           MOVE "NA"                  TO REM-DIR-SOURCE.
           MOVE 0                     TO REM-DIR-YYMMDD.
           MOVE REMOVE-DIRECTORY-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE TEST DIRECTORY" TO E-FILE
              MOVE REM-DIRECTORY-CMD2 TO E-KEYX
              GO TO IO-ERROR.
       CHECK-REMOVE-FROM-NEW.
      * /USR/PFSI/P3/NEW/PR0001
           MOVE NEW-IMPORT-FILE-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT-BAD
              GO TO MOVE-TEST-TO-DONE-EXIT.

           MOVE "RM" TO RM-BUF.
           CALL "C$TOLOWER" USING RM-BUF, VALUE 2.
           MOVE NEW-IMPORT-FILE-PATH TO RM-FILE.
           INSPECT RM-FILE REPLACING FIRST "  " BY "/*".
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE NEW FILES" TO E-FILE
              GO TO IO-ERROR.
           MOVE NEW-IMPORT-FILE-PATH TO REM-DIR-FILENAM.
           MOVE "NA"                 TO REM-DIR-SOURCE.
           MOVE 0                    TO REM-DIR-YYMMDD.
           MOVE REMOVE-DIRECTORY-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE NEW DIRECTORY" TO E-FILE
              MOVE REM-DIRECTORY-CMD2 TO E-KEYX
              GO TO IO-ERROR.
       MOVE-TEST-TO-DONE-EXIT.
           EXIT.

      ******************************************************************
      * CREATE DIRECTORY IN TEST, EG MKDIR /USR/PFSI/P3/TEST/PR1234
      *
      * THEN, MOVE INSTALL SHELL FROM DONE TO TEST, FOR EXAMPLE:
      * MV /USR/PFSI/DONE/PR1234/PR1234 /USR/PFSI/TEST/PR1234/PR1234
      *
      * THEN REMOVE SHELL IN DONE, EG    RM /USR/PFSI/P3/DONE/PR1234/*
      * THEN REMOVE EMPTY DIRECTORY, EG  RMDIR /USR/PFSI/P3/DONE/PR1234
      ******************************************************************
       MOVE-DONE-TO-TEST SECTION.
           MOVE LS-TEST-PATH      TO TEST-IMPORT-PATH1.
           MOVE D-PR(PR-NO)       TO TEST-IMPORT-PATH2.
           CALL "C$MAKEDIR" USING TEST-IMPORT-FILE-PATH GIVING NUM-STAT.
           MOVE "NA"              TO BACK-TO-TEST-FILENAM.
           MOVE TEST-IMPORT-PATH2 TO BACK-TO-TEST-SOURCE.
           INSPECT BACK-TO-TEST-SOURCE REPLACING FIRST "  " BY "/?".
           INSPECT BACK-TO-TEST-SOURCE REPLACING FIRST "?   "
                                           BY LOWER-CASE-LIST.
           MOVE 0                 TO BACK-TO-TEST-YYMMDD.
           MOVE BACK-TO-TEST-COMMAND   TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: BACK-TO-TEST-COMMAND" TO E-FILE
              GO TO IO-ERROR.
           IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
              IF OTHER-MACHINE-DONE = "N"
                 GO TO MOVE-DONE-TO-TEST-EXIT.

           MOVE "RM" TO RM-BUF.
           CALL "C$TOLOWER" USING RM-BUF, VALUE 2.
           MOVE DONE-FILE-PATH TO RM-FILE.
           INSPECT RM-FILE REPLACING FIRST "  " BY "/*".
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE DONE FILE" TO E-FILE
              GO TO IO-ERROR.

           MOVE DONE-FILE-PATH TO REM-DIR-FILENAM.
           MOVE "NA"           TO REM-DIR-SOURCE.
           MOVE 0              TO REM-DIR-YYMMDD.
           MOVE REMOVE-DIRECTORY-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE STAT TO E-CODE
              MOVE "SYSTEM-CALL: REMOVE DONE DIRECTORY" TO E-FILE
              MOVE REM-DIRECTORY-CMD2 TO E-KEYX
              GO TO IO-ERROR.
       MOVE-DONE-TO-TEST-EXIT.
           EXIT.

      ***********************************************************
      * ADD TO TEMP LOG FILE IN MKTEMP, WILL CAT TEMP LOG
      * FILE TO /USR/DATA/R?/LG/P3LOG WHEN PR INSTALL COMPLETE
      ***********************************************************
       ADD-TO-TMP-LOG-FILE SECTION.
           MOVE D-PR(PR-NO)  TO LOG-PRNO.
           MOVE EXT-CONAME-USER-ID  TO LOG-USERID.
           MOVE TODAY-MMDDYY TO LOG-DATE.
           ACCEPT TIME-NOW FROM TIME.
           MOVE CUR-HH       TO LOG-HH.
           MOVE ":"          TO LOG-COL1 LOG-COL2.
           MOVE CUR-MM       TO LOG-MM.
           MOVE CUR-SS       TO LOG-SS.
           MOVE LOG-WORK     TO LOG-REC DETAIL-LINE.
           PERFORM WRITE-LOG-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           PERFORM WRITE-DETAIL-LINE.

      ********************************************************************
      * JUST FOR WORLD1 OR WORLD2, CREATE LOG IN DIRECTORY TO
      * HELP KEEP TRACK OF WHETHER THE OTHER MACHINE IS COMPLETED,
      * FOR EX., IF TAG (/USR/PFSI/P3/NEW/PR1234/WORLD1.A9?.010513,
      * THEN INSTALL ON RISC2 KNOWS IT'S OK TO REMOVE DIRECTORY FROM NEW.
      ********************************************************************
      *CREATE-TAG SECTION.
      *    IF COPY-TO-TEST
      *       MOVE SPACES TO TEST-TAG-PATH
      *       STRING NEW-IMPORT-FILE-PATH, "/",
      *              SYSTEM-NAME, ".",
      *              HOLD-TEST-REL, ".",
      *              TODAY-MMDDYY
      *              DELIMITED BY " "
      *              INTO TEST-TAG-PATH
      *       MOVE TEST-TAG-CMD TO SYSTEM-BUF
      *    ELSE IF COPY-TO-LIVE
      *       MOVE SPACES TO LIVE-TAG-PATH
      *       STRING TEST-IMPORT-FILE-PATH, "/",
      *              SYSTEM-NAME, ".",
      *              HOLD-LIVE-REL, ".",
      *              TODAY-MMDDYY
      *              DELIMITED BY " "
      *              INTO LIVE-TAG-PATH
      *       MOVE LIVE-TAG-CMD TO SYSTEM-BUF
      *    ELSE
      *       MOVE SPACES TO REV-TAG-PATH
      *       STRING DONE-FILE-PATH, "/",
      *              SYSTEM-NAME, ".",
      *              "REV", ".",
      *              TODAY-MMDDYY
      *              DELIMITED BY " "
      *              INTO REV-TAG-PATH
      *       MOVE REV-TAG-CMD TO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.
      *    IF STAT NOT = "00"
      *       MOVE "ERROR CREATING TAG" TO MESS
      *       PERFORM SEND-MESS.

      ***********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
       CHECK-RANGES.
           ACCEPT TODAY-YYMMDD FROM DATE.
           MOVE TD-YY TO NUM-YR.
           MOVE TD-MM TO NUM-MO.
           MOVE TD-DD TO NUM-DA.
           MOVE NUM-DATE TO TODAY-MMDDYY.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO POST-USERID.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO HOLD-REL.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO SYS-NAME-PATH.
      *    ------------------------------------------------------------
      *    PERFORM "UNAME -N > {TEMP FILENAME}"
      *    ------------------------------------------------------------
           CALL "C$TOLOWER" USING UNAME-N-CMD, VALUE 8.
           MOVE SPACES TO SYSTEM-BUF.
           STRING UNAME-N-CMD," > " SYS-NAME-PATH
                  DELIMITED BY "  " INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           PERFORM OPEN-SYS-NAME-FILE.
      *    ------------------------------------------------------------
      *    READ "UNAME -N" RESULT
      *    ------------------------------------------------------------
           PERFORM READ-SYS-NAME-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           PERFORM CLOSE-SYS-NAME-FILE.

           MOVE SYS-NAME TO SYSTEM-NAME.

       DISPLAY-SELECT-FILE.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LS-TEMPFILE
                              LS-NEW-TEMPFILE
                              LS-TEST-TEMPFILE
                              LS-DONE-TEMPFILE.
           PERFORM OPEN-LIST-FILE-OUTPUT.
           PERFORM CLOSE-LIST-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO TEMP-LOG-PATH.

           PERFORM OPEN-LOG-FILE-OUTPUT.
           MOVE SPACES TO ERRCD.
       SELECT-TEST-LIVE.
           MOVE SPACES TO LEGEND1.
           MOVE "S99" TO HOLD-TEST-REL.
           MOVE "S35" TO HOLD-LIVE-REL.
           MOVE
            "1=S99 INSTALL, 2=S35 INSTALL, 3=REVERSE S35, F7-EXIT:   "
                                   TO LEGEND2.
           PERFORM SEND-LEGEND.

       ENTER-SELECTION.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "ENNN060FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO ENDIT.
           IF NOT ENTER-KEY
              GO TO ENTER-SELECTION.
           MOVE VDUNUM0 TO SEL.

           IF NOT COPY-TO-TEST
              IF NOT COPY-TO-LIVE
                 IF NOT REV-FROM-LIVE
              MOVE "PLEASE ENTER '1' OR '2' OR '3'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SELECTION.

           MOVE "N" TO SKIP-READ-FLAG.

           PERFORM DISPLAY-BUILD.

           PERFORM OPEN-LIST-FILE-INPUT.
       RETURN-AND-DISPLAY.
           PERFORM DISPLAY-SCROLL.
       REC-PR-NO.
           MOVE SPACES TO LEGEND1.
           IF REV-FROM-LIVE
              MOVE
             "^, F7-EXIT, RTN-SCROLL, ENTER LINE TO REVERSE:  "
               TO LEGEND2
            ELSE
              MOVE
             "^, F7-EXIT, RTN-SCROLL, ENTER LINE TO INSTALL:  "
               TO LEGEND2.
           PERFORM SEND-LEGEND.
       GET-PR-NO.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A060FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO ENDIT.
           IF UP-KEY
              GO TO SELECT-TEST-LIVE.
           IF NOT ENTER-KEY
              GO TO GET-PR-NO.
           IF VDUBUF = SPACES
              PERFORM DISPLAY-SCROLL
              GO TO GET-PR-NO.

           MOVE VDUBUF TO PR-NO-X.
           INSPECT PR-NO-X REPLACING LEADING SPACES BY ZEROES.
           IF PR-NO-X IS NOT NUMERIC
              GO TO GET-PR-NO.
           IF PR-NO < 1  OR PR-NO > 15
              GO TO GET-PR-NO.

           IF D-PR(PR-NO) = SPACES
              GO TO GET-PR-NO.

           PERFORM SETUP-IMPORT-PATH.
           MOVE IMPORT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD 
             MOVE "INVALID FILE: &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
                 TO MESS
             INSPECT MESS REPLACING FIRST
                                "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
                 BY IMPORT-PATH
             PERFORM SEND-MESS
             GO TO GET-PR-NO.

       GET-DESC.
           PERFORM GET-FIRST-WK.
           IF IO-BAD OR WK-REC(1:1) = X"1A"
              MOVE "SELECTED INSTALL FILE IS EMPTY!" TO MESS
              PERFORM SEND-MESS
              GO TO GET-PR-NO.
           IF PR-DESC = "NO DESCRIPTION FOUND!"
              MOVE "LINE 1 DOES NOT BEGIN WITH 'DESC'" TO MESS
              PERFORM SEND-MESS
              GO TO GET-PR-NO.
           MOVE PR-DESC TO LEGEND.
           PERFORM SEND-LEGEND.
       ENTER-REVERSE-DATE.
           IF NOT REV-FROM-LIVE
              GO TO OK-YN.

           MOVE SPACES TO LEGEND1.
           MOVE
             "ENTER DATE TO REVERSE IN FORMAT YYMMDD, NO SLASHES     :"
               TO LEGEND2.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060FLD." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO REV-YYMMDD.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO GET-PR-NO.
           IF NOT ENTER-KEY GO TO ENTER-REVERSE-DATE.
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY  GO TO ENDIT.
           IF UP-KEY GO TO GET-PR-NO.
           IF NOT ENTER-KEY GO TO OK-YN.
           IF VDUBUF = "Y"  GO TO CHECK-DONE.
           IF VDUBUF = "N"  GO TO GET-PR-NO.
           GO TO OK-YN.
       CHECK-DONE.
      *    IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
      *       PERFORM CHECK-OTHER-MACHINE-INSTALL
      *    END-IF.

           IF REV-FROM-LIVE
              GO TO CHECK-TEST.

           IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
             IF COPY-TO-LIVE
               IF THIS-MACHINE-DONE = "Y"
                 MOVE SPACES TO LEGEND1
                 MOVE
            "PR ALREADY INSTALLED ON THIS MACHINE TODAY; CONTINUE?   "
                             TO LEGEND2
                 PERFORM SEND-LEGEND
                 GO TO REPLACE-DONE-SHELL.

           IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
              IF OTHER-MACHINE-DONE = "Y"
                 GO TO CHECK-TEST.

           MOVE LS-DONE-PATH   TO DONE-PATH1.
           MOVE D-PR(PR-NO)    TO DONE-PATH2.
           MOVE DONE-FILE-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT-BAD
              GO TO CHECK-TEST.

           MOVE SPACES TO LEGEND1.
           MOVE
            "PR ALREADY EXISTS IN DONE; DO YOU WISH TO REPLACE? Y/N: "
                TO LEGEND2.
           PERFORM SEND-LEGEND.
       REPLACE-DONE-SHELL.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO REPLACE-DONE-SHELL.
           IF VDUBUF = "     N"
              GO TO END-PROGRAM.
           IF VDUBUF NOT = "     Y"
              GO TO REPLACE-DONE-SHELL.
       CHECK-TEST.
           IF COPY-TO-LIVE
              GO TO SET-HEAD.

           IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
            IF COPY-TO-TEST
              IF THIS-MACHINE-DONE = "Y"
                 MOVE SPACES TO LEGEND1
                 MOVE
            "PR ALREADY INSTALLED ON THIS MACHINE TODAY; CONTINUE?   "
                             TO LEGEND2
                 PERFORM SEND-LEGEND
                 GO TO REPLACE-TEST-SHELL.

           MOVE LS-TEST-PATH          TO TEST-IMPORT-PATH1.
           MOVE D-PR(PR-NO)           TO TEST-IMPORT-PATH2.
           IF (SYSTEM-NAME = WORLD1 OR SYSTEM-NAME = WORLD2)
              IF OTHER-MACHINE-DONE = "Y"
                 GO TO SET-HEAD.
           MOVE TEST-IMPORT-FILE-PATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT-BAD
              GO TO SET-HEAD.

           MOVE SPACES TO LEGEND1.
           MOVE
            "PR ALREADY EXISTS IN TEST; DO YOU WISH TO REPLACE? Y/N: "
                TO LEGEND2.
           PERFORM SEND-LEGEND.
       REPLACE-TEST-SHELL.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO REPLACE-TEST-SHELL.
           IF VDUBUF = "     N"
              GO TO END-PROGRAM.
           IF VDUBUF NOT = "     Y"
              GO TO REPLACE-TEST-SHELL.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "UPDATE IN PROGRESS...................." TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "HLD"              TO PRU-LP.
           MOVE 1                  TO PRU-COPIES.
           MOVE SPOOL-PATH-DEFAULT TO PRU-DIR.
           MOVE FORM-PROGX         TO PRU-PROG.
           PERFORM SHIFT-PATH.
           MOVE "XXXXXX"           TO PRU-X.
           PERFORM SHIFT-PATH.
           MOVE PRU-FILE TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO PRU-FILE.

           PERFORM OPEN-PR-FILE.

           MOVE 99 TO LN-COUNT.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************************************
       DISPLAY-BUILD SECTION.
      ****************************************************
           MOVE EXT-FILPATH-FIL TO HOLD-FIL.
           MOVE "SCANNING DIRECTORY..." TO LEGEND.
           PERFORM SEND-LEGEND.
           IF COPY-TO-TEST
              MOVE LS-NEW-COMMAND TO SYSTEM-BUF
              PERFORM SYSTEM-CALL
           ELSE IF COPY-TO-LIVE
              MOVE LS-TEST-COMMAND TO SYSTEM-BUF
              PERFORM SYSTEM-CALL
           ELSE
              MOVE LS-DONE-COMMAND TO SYSTEM-BUF
              PERFORM SYSTEM-CALL
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-DISPLAY-BUILD.
           EXIT.

      ******************************************************************
      *
      *    D I S P L A Y - S C R O L L
      *
      ******************************************************************
       DISPLAY-SCROLL SECTION.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1      TO SUB.

      * IF SKIP-READ-FLAG = Y,  ALREADY READ NEXT RECORD TO DISPLAY

           IF SKIP-READ-FLAG = "Y"
              GO TO SKIP-READ-LS.

       DISPLAY-NEXT-LSEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MUST MOVE SPACES TO WK-PR; REQUEST NAMES THAT ARE LESS
      *     THAN 6 CHARACTERS (FOR EXAMPLE, LIBAT) WILL SHOW PART OF THE
      *     PREVIOUS NAME.  FOR EXAMPLE:
      *
      *           REPORT NAME             DISPLAYED AS
      *           ------------            ------------
      *           GNEXTRA002CA            GNEXTRA002CA
      *           GNEXTRA002FX            GNEXTRA002FX
      *           LIBATA005JC             LIBATA005JCX
      *           LIBATA005E7             LIBATA005E7X
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO LIST-REC.

           PERFORM READ-LIST-FILE-NEXT.
           IF ( IO-BAD )
              PERFORM CLOSE-LIST-FILE
              PERFORM OPEN-LIST-FILE-INPUT
              MOVE "** END **" TO D-END-MORE
              GO TO DISPLAY-SCROLL-END.

           MOVE SPACES TO WS-LIST-REC.
           UNSTRING LIST-REC DELIMITED BY ALL " " INTO
                    LIST-READ-WRITE
                    LIST-LINKS
                    LIST-USERID
                    LIST-GROUP
                    LIST-SIZE
                    LIST-MONTH
                    LIST-DAY
                    LIST-TIME
                    LIST-DIR-PR.

           IF ( SUB > 15 )
              MOVE "MORE..." TO D-END-MORE
              MOVE "Y" TO SKIP-READ-FLAG
              GO TO DISPLAY-SCROLL-END.

       SKIP-READ-LS.
           MOVE SUB         TO D-NO(SUB).
           MOVE "."         TO D-DOT(SUB).
           MOVE LIST-USERID TO D-USERID(SUB).
           MOVE LIST-MONTH  TO D-MONTH(SUB).
           MOVE LIST-DAY    TO D-DAY(SUB).
           MOVE LIST-TIME   TO D-TIME(SUB).
           IF COPY-TO-TEST
              MOVE LIST-NEW-PR  TO D-PR(SUB)
           ELSE
              MOVE LIST-TEST-PR TO D-PR(SUB).
           MOVE SUB TO PR-NO.
           PERFORM SETUP-IMPORT-PATH.

           MOVE IMPORT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "MISSING INSTALL LIST" TO D-DESC(SUB)
           ELSE
              PERFORM GET-FIRST-WK
              MOVE PR-DESC TO D-DESC(SUB).

           ADD 1 TO SUB.

           GO TO DISPLAY-NEXT-LSEC.

       DISPLAY-SCROLL-END.

           IF COPY-TO-TEST
              MOVE LS-NEW-PATH  TO D-IMPORT-PATH
           ELSE IF COPY-TO-LIVE
              MOVE LS-TEST-PATH TO D-IMPORT-PATH
           ELSE
              MOVE LS-DONE-PATH TO D-IMPORT-PATH.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-SCROLL-EXIT.
           EXIT.

      ********************************************************
      * SET UP IMPORT PATH FOR NEW OR TEST WITH PR#
      * NEW IMPORT-PATH EXAMPLE : /USR/PFSI/P3/NEW/PR123/LIST
      * TEST IMPORT-PATH EXAMPLE: /USR/PFSI/P3/TEST/PR1234/LIST
      ********************************************************
       SETUP-IMPORT-PATH SECTION.
           IF COPY-TO-TEST
              MOVE LS-NEW-PATH          TO NEW-IMPORT-PATH1
              MOVE D-PR(PR-NO)          TO NEW-IMPORT-PATH2
              MOVE NEW-IMPORT-FILE-PATH TO IMPORT-PATH
              INSPECT IMPORT-PATH REPLACING FIRST "  " BY "/ "
              INSPECT IMPORT-PATH REPLACING FIRST
                 "    " BY LOWER-CASE-LIST
           ELSE IF COPY-TO-LIVE
              MOVE LS-TEST-PATH        TO TEST-IMPORT-PATH1
              MOVE D-PR(PR-NO)         TO TEST-IMPORT-PATH2
              MOVE TEST-IMPORT-FILE-PATH TO IMPORT-PATH
              INSPECT IMPORT-PATH REPLACING FIRST "  " BY "/ "
              INSPECT IMPORT-PATH REPLACING FIRST
                 "    " BY LOWER-CASE-LIST
      * ALSO MUST SETUP NEW PATH, NEED SO WE HAVE PATH TO REMOVE
              MOVE LS-NEW-PATH         TO NEW-IMPORT-PATH1
              MOVE D-PR(PR-NO)         TO NEW-IMPORT-PATH2
              MOVE NEW-IMPORT-FILE-PATH TO IMPORT-PATH
              INSPECT IMPORT-PATH REPLACING FIRST "  " BY "/ "
              INSPECT IMPORT-PATH REPLACING FIRST
                 "    " BY LOWER-CASE-LIST
           ELSE
              MOVE LS-DONE-PATH        TO DONE-PATH1
              MOVE D-PR(PR-NO)         TO DONE-PATH2
              MOVE DONE-FILE-PATH      TO IMPORT-PATH
              INSPECT IMPORT-PATH REPLACING FIRST "  " BY "/ "
              INSPECT IMPORT-PATH REPLACING FIRST
                 "    " BY LOWER-CASE-LIST.

      ********************************************************
      *  GET FIRST WK RECORD FOR PR TO DISPLAY DESCRIPTION
      ********************************************************
       GET-FIRST-WK SECTION.
           PERFORM OPEN-WK-FILE.
           MOVE SPACES TO WK-REC.
           PERFORM READ-WK-FILE.
           IF IO-BAD  OR WK-REC(1:1) = X"1A"
              MOVE 9 TO IO-FG
           ELSE
              IF NOT PR-DESC-VALID
                 MOVE "NO DESCRIPTION FOUND!" TO PR-DESC.
           PERFORM CLOSE-WK-FILE.

      ******************************************************************
      * ONLY FOR WORLD1 
      * LOOK FOR TAG TO SEE IF PR HAS BEEN INSTALLED ON OTHER MACHINE.
      * SET FLAG TO HELP DETERMINE WHETHER TO GIVE WARNINGS (ALEADY
      * INSTALLED) & DON'T ASK TO REMOVE IF OTHER MACHINE NOT INSTALLED.
      * SO FOR EX, IF INSTALLING TO A9? ON WORLD1, LOOK FOR
      * /USR/PFSI/P3/NEW/PR1234/WORLD2.A9?.MMDDYY
      ******************************************************************
      *CHECK-OTHER-MACHINE-INSTALL SECTION.
      *    MOVE "N" TO OTHER-MACHINE-DONE.
      *    IF SYSTEM-NAME = WORLD1
      *       IF COPY-TO-TEST
      *          MOVE SPACES TO TEST-TAG-PATH
      *          STRING NEW-IMPORT-FILE-PATH, "/",
      *                 WORLD2, ".",
      *                 HOLD-TEST-REL, ".",
      *                 TODAY-MMDDYY
      *                 DELIMITED BY " "
      *                 INTO TEST-TAG-PATH
      *          MOVE TEST-TAG-PATH TO ACCESS-BUF.
      *    IF SYSTEM-NAME = WORLD2
      *       IF COPY-TO-TEST
      *          MOVE SPACES TO TEST-TAG-PATH
      *          STRING NEW-IMPORT-FILE-PATH, "/",
      *                 WORLD1, ".",
      *                 HOLD-TEST-REL, ".",
      *                 TODAY-MMDDYY
      *                 DELIMITED BY " "
      *                 INTO TEST-TAG-PATH
      *          MOVE TEST-TAG-PATH TO ACCESS-BUF.
      *    IF SYSTEM-NAME = WORLD1
      *       IF COPY-TO-LIVE
      *          MOVE SPACES TO LIVE-TAG-PATH
      *          STRING TEST-IMPORT-FILE-PATH, "/",
      *                 WORLD2, ".",
      *                 HOLD-LIVE-REL, ".",
      *                 TODAY-MMDDYY
      *                 DELIMITED BY " "
      *                 INTO LIVE-TAG-PATH
      *          MOVE LIVE-TAG-PATH TO ACCESS-BUF.
      *    IF SYSTEM-NAME = WORLD2
      *       IF COPY-TO-LIVE
      *          MOVE SPACES TO LIVE-TAG-PATH
      *          STRING TEST-IMPORT-FILE-PATH, "/",
      *                 WORLD1, ".",
      *                 HOLD-LIVE-REL, ".",
      *                 TODAY-MMDDYY
      *                 DELIMITED BY " "
      *                 INTO LIVE-TAG-PATH
      *          MOVE LIVE-TAG-PATH TO ACCESS-BUF.
      *    IF SYSTEM-NAME = WORLD1
      *       IF REV-FROM-LIVE
      *          MOVE SPACES TO REV-TAG-PATH
      *          STRING DONE-FILE-PATH, "/",
      *                 WORLD2, ".",
      *                 "REV", ".",
      *                 TODAY-MMDDYY
      *                 DELIMITED BY " "
      *                 INTO REV-TAG-PATH
      *          MOVE REV-TAG-PATH TO ACCESS-BUF.
      *    IF SYSTEM-NAME = WORLD2
      *       IF REV-FROM-LIVE
      *          MOVE SPACES TO REV-TAG-PATH
      *          STRING DONE-FILE-PATH, "/",
      *                 WORLD1, ".",
      *                 "REV", ".",
      *                 TODAY-MMDDYY
      *                 DELIMITED BY " "
      *                 INTO REV-TAG-PATH
      *          MOVE REV-TAG-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT-GOOD
      *       MOVE "Y" TO OTHER-MACHINE-DONE.
      *    IF SYSTEM-NAME = WORLD1
      *       INSPECT ACCESS-BUF REPLACING FIRST "2." BY "1."
      *    ELSE
      *       INSPECT ACCESS-BUF REPLACING FIRST "1." BY "2.".
      *    PERFORM ACCESS-CALL.
      *    IF STAT-GOOD
      *       MOVE "Y" TO THIS-MACHINE-DONE.

      ********************************
       SHIFT-PATH SECTION.
      ********************************
           MOVE 0 TO SUB1.
       SHIFT-FIND-SP.
           ADD 1 TO SUB1.
           IF SUB1 > PRU-PATH-MAX
              GO TO SHIFT-EXIT.
           IF PRU-C(SUB1) NOT = " "
              GO TO SHIFT-FIND-SP.
           MOVE SUB1 TO SUB2.
       SHIFT-FIND-FILE.
           ADD 1 TO SUB2.
           IF SUB2 > PRU-PATH-MAX
              GO TO SHIFT-EXIT.
           IF PRU-C(SUB2) = " "
              GO TO SHIFT-FIND-FILE.
       SHIFT-NEXT-CHAR.
           MOVE PRU-C(SUB2) TO PRU-C(SUB1).
           MOVE " " TO PRU-C(SUB2).
           ADD 1 TO SUB1.
           ADD 1 TO SUB2.
           IF SUB2 NOT > PRU-PATH-MAX
              GO TO SHIFT-NEXT-CHAR.
       SHIFT-EXIT.
           EXIT.

      ****************************
      *   DETAIL LINE PROCESS    *
      ****************************
      *DETAIL-LINE-PROCESS SECTION.
      *    MOVE WK-REC TO DETAIL-LINE.
      *    PERFORM WRITE-DETAIL-LINE.
      *END-DETAIL-LN-PROCESS.
      *    EXIT.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
           ADD LN-FEED  TO LN-COUNT.
           IF LN-COUNT > 60
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
      *        HEAD PAGE ROUTINE    *
      *******************************
       HEAD-PAGE SECTION.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 3 TO LN-COUNT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO INSTAL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO INSTAL-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY INSTAL-SCREEN UPON INSTAL-WINDOW-HANDLE.
           MOVE INSTAL-FORM-FIELDS TO WR-BUF.
           MOVE INSTAL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE INSTAL-HELP--FILE TO HELP-LINK-FILE.
           MOVE INSTAL-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/INSTAL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
      *  MISCELLANEOUS ROUTINES       *
      *********************************
       MISC SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
      *
      *    I - O   R O U T I N E    S E C T I O N
      *
      *****************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE IMPORT-PATH TO E-FILE.
           OPEN INPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
       READ-WK-FILE.
           PERFORM READ-IT.
           MOVE IMPORT-PATH TO E-FILE.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.

       OPEN-LIST-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE LS-TEMPFILE TO E-FILE.
           OPEN OUTPUT LIST-FILE.
           IF IO-FG = 8
             GO TO OPEN-LIST-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE LIST-FILE
             GO TO OPEN-LIST-FILE-OUTPUT.
           MOVE SPACES TO LIST-REC.

       OPEN-LIST-FILE-INPUT.
           PERFORM OPEN-IT.
           MOVE LS-TEMPFILE TO E-FILE.
           OPEN INPUT LIST-FILE.
           IF IO-FG = 8
             GO TO OPEN-LIST-FILE-INPUT.
           IF IO-FG = 7
             CLOSE LIST-FILE
             GO TO OPEN-LIST-FILE-INPUT.
       READ-LIST-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LS-TEMPFILE TO E-FILE.
           MOVE SPACES TO E-KEYX.
           READ LIST-FILE AT END MOVE 9 TO IO-FG.
       WRITE-LIST-FILE.
           PERFORM WRITE-IT.
           MOVE LS-TEMPFILE TO E-FILE.
           MOVE SPACES TO E-KEYX.
           WRITE LIST-REC.
           MOVE SPACES TO LIST-REC.
       CLOSE-LIST-FILE.
           PERFORM CLOSE-IT.
           CLOSE LIST-FILE.
       OPEN-LOG-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TEMP-LOG-PATH TO E-FILE.
           OPEN OUTPUT LOG-FILE.
           IF IO-FG = 8
             GO TO OPEN-LOG-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE LOG-FILE
             GO TO OPEN-LOG-FILE-OUTPUT.
           MOVE SPACES TO LOG-REC.
       WRITE-LOG-FILE.
           PERFORM WRITE-IT.
           MOVE TEMP-LOG-PATH TO E-FILE.
           MOVE SPACES TO E-KEYX.
           WRITE LOG-REC.
           MOVE SPACES TO LOG-REC.
       CLOSE-LOG-FILE.
           PERFORM CLOSE-IT.
           CLOSE LOG-FILE.
       OPEN-SYS-NAME-FILE.
           PERFORM OPEN-IT.
           MOVE SYS-NAME-PATH TO E-FILE.
           OPEN INPUT SYS-NAME-FILE.
           IF IO-FG = 8
              GO TO OPEN-SYS-NAME-FILE.
           IF IO-FG = 7
              CLOSE SYS-NAME-FILE
              GO TO OPEN-SYS-NAME-FILE.
       READ-SYS-NAME-FILE.
           PERFORM READ-IT.
           MOVE SYS-NAME-PATH TO E-FILE.
           READ SYS-NAME-FILE.
           IF IO-FG = 8
              GO TO READ-SYS-NAME-FILE.
       CLOSE-SYS-NAME-FILE.
           PERFORM CLOSE-IT.
           CLOSE SYS-NAME-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           MOVE TEMP-LOG-PATH TO ACCESS-BUF
           PERFORM REMOVE-WORKFILE.

           MOVE LS-TEMPFILE TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE SYS-NAME-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           MOVE "SP/SPINMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY INSTAL-SCREEN.
           DESTROY INSTAL-WINDOW-HANDLE.
           EXIT PROGRAM.
