       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GR15DA.
       AUTHOR.       JKC.
       DATE-WRITTEN. 08-20-2020.
      ******************************************************************
      *
      * IDENTIFICATION: UP/GR15DA
      *
      * DESCRIPTION   : A15 DATA ANALYSIS FOR GR15DA.
      *                 COPIED FROM GROUP FILE LISTING (LS/GRLST2).
      *                 GET A15 DATA FOR UPGRADE TO A34 SO TO GET
      *                 DATA MIGRATION STRATEGY.
      *
      * OUTPUT FILE(S): /USR/TMP/GR15DA-1-??????.TXT   (FILENAME DETAIL)
      *                 /USR/TMP/GR15DA-2-??????.TXT   (FILENAME TOTAL )
      *                 /USR/TMP/GR15DA-3-??????.TXT   (GR LEVEL DETAIL)
      *                 /USR/TMP/GR15DA-4-??????.TXT   (GR LEVEL TOTAL )
      *
      * **** NOTE ****: WHEN IMPORTING THE FILE INTO EXCEL, YOU NEED TO
      *                 USE THESE SETTINGS IN ORDER TO GET THE
      *                 GROUP/BRNO COLUMN TO SHOW:
      *
      *                   DELIMITER: TAB
      *                   DATA TYPE DETECTION: BASED ON ENTIRE DATASET
      *                                        ^^^^^^^^^^^^^^^^^^^^^^^
      *
      *=================================================================
      * WK2 WK3 GROUP LEVELS: 0 = BRANCH
      *                       1 = WORLD
      *                       3 = DIVISION
      *                       4 = STATE
      *                       6 = REGION
      *                       7 = DISTRICT
      *                       9 = GROUP TOTALS
      *
      *=================================================================
      * REV:
      * JKC 2020-0820 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *=================================================================
      *    A15 BRFILE
      *=================================================================
      * COPYMEMBER: LIBGB/GBFSBR
           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.

      *=================================================================
      *    EXTRACTION FILES
      *=================================================================
           SELECT GR15DA-1-FILE ASSIGN OUTPUT GR15DA-1-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT GR15DA-2-FILE ASSIGN OUTPUT GR15DA-2-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT GR15DA-3-FILE ASSIGN OUTPUT GR15DA-3-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT GR15DA-4-FILE ASSIGN OUTPUT GR15DA-4-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      *=================================================================
      *    VUTIL INFO FILE 
      *=================================================================
           SELECT VUTIL-INFO-FILE ASSIGN TO VUTIL-INFO-PATH
                  ORGANIZATION SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      *=================================================================
      *    WORK FILES
      *=================================================================
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.

           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.

           SELECT WK3-FILE ASSIGN WK3-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK3-KEY
                  FILE STATUS FILE-STAT.

      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *=================================================================
      *    A15 BRFILE
      *=================================================================
      * COPYMEMBER: LIBGB/GBFDBR
       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".

      *=================================================================
      *    VUTIL INFO FILE 
      *=================================================================
       FD  VUTIL-INFO-FILE
           LABEL RECORDS ARE STANDARD.

      *-----------------------------------------------------------------
       01  VUTIL-INFO-REC.
           03  VUTIL-INFO-BUF        PIC X(33).
           03  VUTIL-INFO-CR         PIC X(01).
      *--->03  VUTIL-INFO-LF         PIC X(01).

      *=================================================================
      *    WORK FILES
      *=================================================================
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK1-REC.
           03  WK1-KEY.
               05  WK1-BRNO            PIC 9(04).
               05  WK1-SEQ             PIC 9(01).
           03  WK1-SCURR-TABLE.
               05  WK1-SCURR           PIC 9(04) COMP OCCURS 15.

      *=================================================================
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-FILENAME        PIC X(10).
               05  WK2-GR-LEVEL        PIC 9(01).
               05  WK2-GR-LEVELX       REDEFINES WK2-GR-LEVEL
                                       PIC X(01).
               05  WK2-GR-ID           PIC X(04).
               05  WK2-BRNO REDEFINES WK2-GR-ID
                                       PIC 9(04).
           03  WK2-RISC-MACHINE        PIC X(02).
           03  WK2-STATE               PIC X(02).
           03  WK2-DATA-PATH           PIC X(06).
           03  WK2-BRANCH-INFO         PIC X(40).
           03  WK2-FILEPATH            PIC X(40).
           03  WK2-FILESIZE            PIC 9(18).
           03  WK2-RECORDS-COUNT       PIC 9(18).

      *=================================================================
       FD  WK3-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK3-REC.
           03  WK3-KEY.
               05  WK3-GR-LEVEL        PIC 9(01).
               05  WK3-GR-LEVELX       REDEFINES WK3-GR-LEVEL
                                       PIC X(01).
               05  WK3-GR-ID           PIC X(04).
               05  WK3-BRNO REDEFINES WK3-GR-ID
                                       PIC 9(04).
               05  WK3-FILENAME        PIC X(10).
           03  WK3-RISC-MACHINE        PIC X(02).
           03  WK3-STATE               PIC X(02).
           03  WK3-DATA-PATH           PIC X(06).
           03  WK3-BRANCH-INFO         PIC X(40).
           03  WK3-FILEPATH            PIC X(40).
           03  WK3-FILESIZE            PIC 9(18).
           03  WK3-RECORDS-COUNT       PIC 9(18).

      *=================================================================
      *    EXTRACTION FILES
      *=================================================================
       FD  GR15DA-1-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  GR15DA-1-REC.
           03  GR15DA-1-BYTE         PIC X(01)     OCCURS 250 TIMES.

       FD  GR15DA-2-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  GR15DA-2-REC.
           03  GR15DA-2-BYTE         PIC X(01)     OCCURS 250 TIMES.

       FD  GR15DA-3-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  GR15DA-3-REC.
           03  GR15DA-3-BYTE         PIC X(01)     OCCURS 250 TIMES.

       FD  GR15DA-4-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  GR15DA-4-REC.
           03  GR15DA-4-BYTE         PIC X(01)     OCCURS 250 TIMES.

      *#################################################################
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      *#################################################################
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/GR15DA.CBL S34 05/26/21-13:33:56 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      *=================================================================
      * GR15DA-#-PATH: /USR/TMP/GR15DA-#-??????.TXT
      *                ?????? = PROCESS-ID-BUF
      *=================================================================
       01  GR15DA-1-PATH.
           03  GR15DA-1-MKTEMP-BUF      PIC X(09) VALUE "/USR/TMP/".
           03  GR15DA-1-FILE-NAME       PIC X(09) VALUE "GR15DA-1-".
           03  GR15DA-1-MKTEMP-NAME     PIC X(26) VALUE SPACES.

       01  GR15DA-2-PATH.
           03  GR15DA-2-MKTEMP-BUF      PIC X(09) VALUE "/USR/TMP/".
           03  GR15DA-2-FILE-NAME       PIC X(09) VALUE "GR15DA-2-".
           03  GR15DA-2-MKTEMP-NAME     PIC X(26) VALUE SPACES.

       01  GR15DA-3-PATH.
           03  GR15DA-3-MKTEMP-BUF      PIC X(09) VALUE "/USR/TMP/".
           03  GR15DA-3-FILE-NAME       PIC X(09) VALUE "GR15DA-3-".
           03  GR15DA-3-MKTEMP-NAME     PIC X(26) VALUE SPACES.

       01  GR15DA-4-PATH.
           03  GR15DA-4-MKTEMP-BUF      PIC X(09) VALUE "/USR/TMP/".
           03  GR15DA-4-FILE-NAME       PIC X(09) VALUE "GR15DA-4-".
           03  GR15DA-4-MKTEMP-NAME     PIC X(26) VALUE SPACES.

      *=================================================================
       01  GR15DA-FILE-PATH                          PIC X(35).

       01  FILLER                          REDEFINES GR15DA-FILE-PATH.
           03  FILLER                                PIC X(01).
           03  GR15DA-FILE-PATH-FIL                  PIC X(34).

       01  FILLER                          REDEFINES GR15DA-FILE-PATH.
           03  GR15DA-FILE-PATH-SLASH-1              PIC X(01).
           03  GR15DA-FILE-PATH-DIR-1                PIC X(03).
           03  GR15DA-FILE-PATH-SLASH-2              PIC X(01).
           03  GR15DA-FILE-PATH-DIR-2                PIC X(04).
           03  GR15DA-FILE-PATH-SLASH-3              PIC X(01).
           03  GR15DA-FILE-PATH-MACHINE              PIC X(02).
           03  GR15DA-FILE-PATH-SLASH-4              PIC X(01).
           03  GR15DA-FILE-PATH-GLOBAL-LOCAL         PIC X(22).

       01  FILLER                          REDEFINES GR15DA-FILE-PATH.
           03  FILLER                                PIC X(13).
           03  GR15DA-FILE-PATH-GLOBAL-PATH.
               05  GR15DA-FILE-PATH-GLOBAL-DIR       PIC X(02).
               05  GR15DA-FILE-PATH-GLOBAL-SLASH     PIC X(01).
               05  GR15DA-FILE-PATH-GLOBAL-FILE      PIC X(19).

       01  FILLER                          REDEFINES GR15DA-FILE-PATH.
           03  GR15DA-FILLER                         PIC X(13).
           03  GR15DA-FILE-PATH-LOCAL-DATA           PIC X(06).
           03  GR15DA-FILE-PATH-LOCAL-DATA-SL        PIC X(01).
           03  GR15DA-FILE-PATH-LOCAL-PATH.
               05  GR15DA-FILE-PATH-LOCAL-DIR        PIC X(02).
               05  GR15DA-FILE-PATH-LOCAL-SLASH      PIC X(01).
               05  GR15DA-FILE-PATH-LOCAL-FILE       PIC X(12).

      *===============================================[ A15 FILE PATHS ]
       01  OBR-KEY                     PIC 9(04)    VALUE ZEROES.
       01  OBR-PATH                    PIC X(35)    VALUE SPACES.

       01  OBM-PATH                    PIC X(35)     VALUE SPACES.
       01  OBW-PATH                    PIC X(35)     VALUE SPACES.
       01  OCA-PATH                    PIC X(35)     VALUE SPACES.
       01  OBX-PATH                    PIC X(35)     VALUE SPACES.
       01  OCI-PATH                    PIC X(35)     VALUE SPACES.
       01  OCJ-PATH                    PIC X(35)     VALUE SPACES.
       01  OCK-PATH                    PIC X(35)     VALUE SPACES.
       01  OCP-PATH                    PIC X(35)     VALUE SPACES.
       01  OCX-PATH                    PIC X(35)     VALUE SPACES.
       01  ODL-PATH                    PIC X(35)     VALUE SPACES.
       01  ODT-PATH                    PIC X(35)     VALUE SPACES.
       01  OGB-PATH                    PIC X(35)     VALUE SPACES.
       01  OIC-PATH                    PIC X(35)     VALUE SPACES.
       01  OIX-PATH                    PIC X(35)     VALUE SPACES.
       01  OLE-PATH                    PIC X(35)     VALUE SPACES.
       01  OLH-PATH                    PIC X(35)     VALUE SPACES.
       01  OLM-PATH                    PIC X(35)     VALUE SPACES.
       01  OLN-PATH                    PIC X(35)     VALUE SPACES.
       01  OLP-PATH                    PIC X(35)     VALUE SPACES.
       01  OLS-PATH                    PIC X(35)     VALUE SPACES.
       01  OLT-PATH                    PIC X(35)     VALUE SPACES.
       01  OLX-PATH                    PIC X(35)     VALUE SPACES.
       01  OML-PATH                    PIC X(35)     VALUE SPACES.
       01  OPA-PATH                    PIC X(35)     VALUE SPACES.
       01  ORX-PATH                    PIC X(35)     VALUE SPACES.
       01  OTY-PATH                    PIC X(35)     VALUE SPACES.

      *=================================================================
      *01  PR-IFN                      PIC X(11)    VALUE "0DPRINT".
       01  VUTIL-INFO-IFN              PIC X(10)    VALUE "VUTIL-INFO".
       01  VUTIL-INFO-KEY              PIC X(40)    VALUE SPACES.
       01  VUTIL-INFO-PATH             PIC X(40)    VALUE SPACES.
       01  WK1-IFN                     PIC X(03)    VALUE "WK1".
       01  WK1-PATH                    PIC X(40)    VALUE SPACES.
       01  WK2-IFN                     PIC X(03)    VALUE "WK2".
       01  WK2-PATH                    PIC X(40)    VALUE SPACES.
       01  WK3-IFN                     PIC X(03)    VALUE "WK3".
       01  WK3-PATH                    PIC X(40)    VALUE SPACES.

      *=================================================================
       01  HOLD-WK-KEYS.
           03  HOLD-WK2-KEY.
               05  HOLD-WK2-FILENAME       PIC X(10).
               05  HOLD-WK2-GR-LEVEL       PIC 9(01).
               05  HOLD-WK2-GR-LEVELX      REDEFINES HOLD-WK2-GR-LEVEL
                                           PIC X(01).
               05  HOLD-WK2-GR-ID          PIC X(04).
               05  HOLD-WK2-BRNO REDEFINES HOLD-WK2-GR-ID
                                           PIC 9(04).
           03  HOLD-WK3-KEY.
               05  HOLD-WK3-GR-LEVEL       PIC 9(01).
               05  HOLD-WK3-GR-LEVELX      REDEFINES HOLD-WK3-GR-LEVEL
                                           PIC X(01).
               05  HOLD-WK3-GR-ID          PIC X(04).
               05  HOLD-WK3-BRNO REDEFINES HOLD-WK3-GR-ID
                                           PIC 9(04).
               05  HOLD-WK3-FILENAME       PIC X(10).

      *===============================================[ HEX CHARACTERS ]
       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-LINE-FEED               PIC X(01)     VALUE X"0A".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".
       01  HEX-DOT-TXT                 PIC X(04)     VALUE X"2E747874".

      ******************************************************************
      *
      *    M E S S A G E - W O R K E R S
      *
      ******************************************************************
       01  MESSAGE-INVALID-YN.
           03  FILLER PIC X(30) VALUE "INVALID: (Y)ES OR (N)O ONLY!!!".
           03  FILLER PIC X(30) VALUE "                              ".

       01  MESSAGE-INVALID-FILE-PATH.
           03  FILLER PIC X(30) VALUE "INVALID FILE PATH; PATH NOT FO".
           03  FILLER PIC X(30) VALUE "UND!!!                        ".

       01  MESSAGE-INVALID-SAME-PATHS.
           03  FILLER PIC X(30) VALUE "INVALID: SOURCE AND TARGET CAN".
           03  FILLER PIC X(30) VALUE "NOT BE THE SAME               ".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CURRENT-BRANCH-INFO         PIC X(40)     VALUE SPACES.
       01  CURRENT-FILEPATH            PIC X(40)     VALUE SPACES.
       01  CURRENT-FILESIZE            PIC 9(18)     VALUE ZEROES.
       01  CURRENT-RECORDS-COUNT       PIC 9(18)     VALUE ZEROES.

       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  HOLD-SEQ                    PIC 9(04)     VALUE 0.

      *01  LN-WORKERS.
      *    03  LN-FEED                 PIC 9(02)     VALUE 0.
      *    03  LN-COUNT                PIC 9(02)     VALUE 61.
      *    03  LN-WK                   PIC 9(02)     VALUE 0.
      *01  PAGE-NUMBER                 PIC 9(03)     VALUE 0.

       01  SUB                         PIC 9999      VALUE 0.
       01  SUB-PARENT                  PIC 9999      VALUE 0.
       01  SUB-WGR                     PIC 9999      VALUE 0.

       01  MACHINE-WORKERS.
           03  CURRENT-MACHINE         PIC X(02).
           03  RISC1                   PIC X(02)     VALUE X'7231'.
           03  RISC2                   PIC X(02)     VALUE X'7232'.
           03  SOURCE-MACHINE          PIC X(02).
           03  TARGET-MACHINE          PIC X(02).

       01  WSS-LCASE-NUM-OF-RECORDS    PIC X(13)  VALUE "# OF RECORDS:".

       01  WSS-UP1534-FILE-INPUT       PIC X(22)  VALUE SPACES.
       01  FILLER REDEFINES WSS-UP1534-FILE-INPUT.
           03  WSS-UP1534-FILE-INPUT-DIR   PIC X(02).
           03  FILLER                      PIC X(01).
           03  WSS-UP1534-FILE-INPUT-NAME  PIC X(19).

       01  WSS-VUTIL-INFO-BUF          PIC X(33).
       01  FILLER REDEFINES WSS-VUTIL-INFO-BUF.
           03  WSS-VUTIL-INFO-BUF-BASE PIC X(15).
           03  WSS-VUTIL-INFO-BUF-NUMX PIC X(18).
           03  WSS-VUTIL-INFO-BUF-NUM  REDEFINES
               WSS-VUTIL-INFO-BUF-NUMX PIC 9(18).

      *=================================================================
      *    SYSTEM COMMANDS (SYSCMD)
      *-----------------------------------------------------------------
      *     SYSCMD-VUTIL-RECS:
      *      VUTIL -I 
      *       /USR/DAR1/R#/SSBBBB/XX/YYFILE
      *        | GREP "# OF RECORDS:" > 
      *         MKTEMPFILE
      *
      *     SYSCMD-VUTIL-SIZE:
      *      VUTIL -I 
      *       /USR/DAR1/R#/SSBBBB/XX/YYFILE
      *        | GREP "FILE SIZE:" > 
      *         MKTEMPFILE
      *
      *     NOTE: NEED TO GET '# OF RECORDS' FIRST THEN 'FILE SIZE'
      *
      *=================================================================
       01  SYSCMD-VUTIL-RECS.
           03  SYSCMD-VUTIL-RECS-VUTIL  PIC X(09) VALUE "VUTIL -I ".
           03  SYSCMD-VUTIL-RECS-FILE   PIC X(40) VALUE SPACES.
           03  SYSCMD-VUTIL-RECS-GREP.
               05  FILLER               PIC X(08) VALUE " | GREP ".
               05  FILLER               PIC X(01) VALUE X'22'.
               05  FILLER               PIC X(13) VALUE "# OF RECORDS:".
               05  FILLER               PIC X(01) VALUE X'22'.
               05  FILLER               PIC X(03) VALUE " > ".
           03  SYSCMD-VUTIL-RECS-MKTEMP PIC X(40) VALUE SPACES.

       01  SYSCMD-VUTIL-SIZE.
           03  SYSCMD-VUTIL-SIZE-VUTIL  PIC X(09) VALUE "VUTIL -I ".
           03  SYSCMD-VUTIL-SIZE-FILE   PIC X(40) VALUE SPACES.
           03  SYSCMD-VUTIL-SIZE-GREP.
               05  FILLER               PIC X(08) VALUE " | GREP ".
               05  FILLER               PIC X(01) VALUE X'22'.
               05  FILLER               PIC X(10) VALUE "FILE SIZE:".
               05  FILLER               PIC X(01) VALUE X'22'.
               05  FILLER               PIC X(04) VALUE " >> ".
           03  SYSCMD-VUTIL-SIZE-MKTEMP PIC X(40) VALUE SPACES.

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
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
      *01  LEGEND-PRINTER-OPTIONS.
      *    03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER PRINT".
      *    03  FILLER PIC X(30) VALUE "ER OPTIONS                    ".
      *    03  FILLER PIC X(17) VALUE "                 ".

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
      *01  LEGEND-OPTION-FILES.
      *    03  FILLER PIC X(30) VALUE "F1 THRU F7, F8-HELP, UP/DOWN, ".
      *    03  FILLER PIC X(30) VALUE "ENTER (Y)ES/(N)O/(C)HECK FOR U".
      *    03  FILLER PIC X(17) VALUE "PDATING THIS FILE".

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
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       01  SCREEN-RANGE-WORKERS.
           03  SOURCE-PATH             PIC X(35).
           03  TARGET-PATH             PIC X(35).
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

      *-----------------------------------------------------------------
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

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".
       01  TARGET-PATH-LIST            PIC X(07)     VALUE "T-PATH.".

       01  STARTING-TIME-LIST          PIC X(11)    VALUE "START-TIME.".
       01  STARTING-TIME-BUF.
           03  STARTING-TIME-RIGHT     PIC X(02)     VALUE SPACES.
           03  STARTING-TIME           PIC X(08)     VALUE SPACES.
           03  STARTING-TIME-LEFT      PIC X(02)     VALUE SPACES.

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
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBUP/UP1534W.CPY".

      *-----------------------------------------------------------------
       01  GR15DA-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/GR15DA_DEF.CPY".
       COPY "LIBUP/GR15DA_WKS.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBUP/GR15DA_SCN.CPY".

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
                 VUTIL-INFO-FILE WK1-FILE WK2-FILE WK3-FILE
                 GR15DA-1-FILE GR15DA-2-FILE
                 GR15DA-3-FILE GR15DA-4-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE VUTIL-INFO-FILE WK1-FILE WK2-FILE WK3-FILE
                 GR15DA-1-FILE GR15DA-2-FILE
                 GR15DA-3-FILE GR15DA-4-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIND, MOVE, OPEN A15 BRFILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE RISC1                      TO CURRENT-MACHINE
                                              SOURCE-MACHINE
                                              TARGET-MACHINE.

           MOVE "GB/BRFILE"                TO UP1534-FILE-INPUT.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ACCESS-BUF       OBR-PATH.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE "ERROR: CANNOT FIND A15 BRFILE" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           MOVE OBR-PATH TO UP1534-MV15-OLD-PATH.
           PERFORM UP1534-MV15-OLD-TO-NEW.
           MOVE UP1534-MV15-NEW-PATH TO OBR-PATH.
           PERFORM OPEN-OBR-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
      *  1. BUILD WK1-FILE; LIST OF ALL BRANCHES WITHIN THE GROUP
      *     [READ-GR-FILE-NEXT]
      *  2. BUILD WK2-FILE & WK3-FILE; SORT DATA BASED ON A BRANCH'S
      *     FILE
      *     [PROCESS-ROUTINE]
      *  4. BUILD GR15DA-#-FILE; EXTRACTION FILES
      *     BASED ON SORT DATA
      *     [EXTRACT-WK2-ROUTINE & EXTRACT-WK3-ROUTINE]
      *=================================================================
       READ-GRFILE-NEXT.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO PROCESS-ROUTINE.

           MOVE 0 TO HOLD-SEQ.

      *-----------------------------------------------------------------
       CREATE-WK1-REC.

           MOVE WGR-SRESULT TO WK1-BRNO.
           MOVE HOLD-SEQ TO WK1-SEQ.
           MOVE WGR-SCURR-TABLE TO WK1-SCURR-TABLE.
           PERFORM WRITE-WK1-FILE.

           IF ( IO-BAD )
              IF ( E-CODE NOT = "22" )
                 GO TO IO-ERROR
              ELSE
                 IF ( HOLD-SEQ < 10 )
                    ADD 1 TO HOLD-SEQ
                    GO TO CREATE-WK1-REC
                 ELSE
                    GO TO IO-ERROR.

           GO TO READ-GRFILE-NEXT.

      *=================================================================
       PROCESS-ROUTINE.

           MOVE 0 TO WK1-KEY WK1-SEQ.
           PERFORM START-WK1-FILE.

      *-----------------------------------------------------------------
       NEXT-WORK-REC.

           PERFORM READ-WK1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO EXTRACT-WK2-ROUTINE.

           PERFORM SEND-LEGEND-PROCESS-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE WK1-BRNO TO OBR-KEY.
           PERFORM READ-OBR-FILE.

           IF ( IO-BAD )
              MOVE "*** INVALID BRANCH!!! ***" TO CURRENT-BRANCH-INFO
           ELSE
              MOVE OBR-CITY TO CURRENT-BRANCH-INFO
              INSPECT CURRENT-BRANCH-INFO REPLACING FIRST "  " BY ", "
              INSPECT CURRENT-BRANCH-INFO REPLACING FIRST "  "
                      BY OBR-STATE AFTER INITIAL " ".

           IF ( WK1-SEQ > 0 )
              MOVE "*** DUPLICATE BRANCH! ***" TO CURRENT-BRANCH-INFO.

           IF NOT ( OBR-MACHINE = RISC1 OR RISC2 )
              MOVE "*** INVALID MACHINE!! ***" TO CURRENT-BRANCH-INFO.

           MOVE OBR-DATA-PATH TO BR-DATA-PATH. *> NEED FOR LOAD PATHS

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE OBR-MACHINE                TO SOURCE-MACHINE.
           MOVE UP1534-FILE-ORDER---SOURCE TO UP1534-FILE-ORDER.
           MOVE UP1534-FILE-TYPE---LOCAL   TO UP1534-FILE-TYPE.

           MOVE "LP/BMFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "GB/BWFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "GB/BXFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/CAFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/CIFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "SP/CJFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/CKFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "SP/CPFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/CXFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/DLFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/DTFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

      *KC  MOVE "GB/GBFILE"                TO UP1534-FILE-INPUT.
      *KC  PERFORM VUTIL-INFO-MODULE.
      *KC  PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/ICFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/IXFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/LEFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/LHFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/LMFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/LNFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "GB/LPFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/LSFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/LTFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "GB/LXFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/MLFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/PAFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/RXFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           MOVE "LP/TYFILE"                TO UP1534-FILE-INPUT.
           PERFORM VUTIL-INFO-MODULE.
           PERFORM UPDATE-SORT-MODULE.

           GO TO NEXT-WORK-REC.

      *=================================================================
       EXTRACT-WK2-ROUTINE.

           PERFORM SEND-LEGEND-EXTRACT.

           MOVE SPACES TO WK2-FILENAME.
           MOVE ZEROES TO WK2-GR-LEVEL.
           MOVE SPACES TO WK2-GR-ID.
           PERFORM START-WK2-FILE.

      *-----------------------------------------------------------------
       EXTRACT-WK2-NEXT-REC.

           PERFORM READ-WK2-FILE-NEXT.

           IF ( IO-BAD )
              GO TO EXTRACT-WK3-ROUTINE.

           MOVE SPACES TO GR15DA-1-REC.

           STRING WK2-FILENAME,      HEX-TAB,
                  WK2-GR-LEVEL,      HEX-TAB,
                  WK2-GR-ID,         HEX-TAB,
                  WK2-RISC-MACHINE,  HEX-TAB,
                  WK2-STATE,         HEX-TAB,
                  WK2-DATA-PATH,     HEX-TAB,
                  WK2-BRANCH-INFO,   HEX-TAB,
                  WK2-FILEPATH,      HEX-TAB,
                  WK2-FILESIZE,      HEX-TAB,
                  WK2-RECORDS-COUNT, HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE INTO GR15DA-1-REC.

           IF ( WK2-GR-LEVEL = 0 )
              PERFORM WRITE-GR15DA-1-FILE
           ELSE
              MOVE GR15DA-1-REC TO GR15DA-2-REC
              PERFORM WRITE-GR15DA-2-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXTRACT-WK2-NEXT-REC.

      *=================================================================
       EXTRACT-WK3-ROUTINE.

           PERFORM SEND-LEGEND-EXTRACT.

           MOVE ZEROES TO WK3-GR-LEVEL.
           MOVE SPACES TO WK3-GR-ID.
           MOVE SPACES TO WK3-FILENAME.
           PERFORM START-WK3-FILE.

      *-----------------------------------------------------------------
       EXTRACT-WK3-NEXT-REC.

           PERFORM READ-WK3-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           MOVE SPACES TO GR15DA-3-REC.

           STRING WK3-GR-LEVEL,      HEX-TAB,
                  WK3-GR-ID,         HEX-TAB,
                  WK3-FILENAME,      HEX-TAB,
                  WK3-RISC-MACHINE,  HEX-TAB,
                  WK3-STATE,         HEX-TAB,
                  WK3-DATA-PATH,     HEX-TAB,
                  WK3-BRANCH-INFO,   HEX-TAB,
                  WK3-FILEPATH,      HEX-TAB,
                  WK3-FILESIZE,      HEX-TAB,
                  WK3-RECORDS-COUNT, HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE INTO GR15DA-3-REC.
        
           IF ( WK3-GR-LEVEL = 0 )
              PERFORM WRITE-GR15DA-3-FILE
           ELSE
              MOVE GR15DA-3-REC TO GR15DA-4-REC
              PERFORM WRITE-GR15DA-4-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXTRACT-WK3-NEXT-REC.

      *=================================================================
       MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CLOSE, MOVE A15 BRFILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM CLOSE-OBR-FILE.
           MOVE UP1534-FILE-TYPE---GLOBAL  TO UP1534-FILE-TYPE.
           MOVE OBR-PATH                   TO UP1534-MV15-NEW-PATH.
           PERFORM UP1534-MV15-NEW-TO-OLD.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROG.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "GR15DA" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS GR15DA-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              MOVE "PROGRAM NOT BATCHABLE" TO MESS
              PERFORM SEND-MESS
              MOVE "99" TO EXT-ROUTE-BUF
              GO TO END-PROGRAM.

           PERFORM GET-GPENV.
           PERFORM ENTER-PARAMETERS.

           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE LEGEND-BEGIN-PROCESSING TO LEGEND.
           PERFORM SEND-LEGEND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           CALL "C$TOLOWER" USING SYSCMD-VUTIL-RECS-VUTIL, VALUE 09.
           CALL "C$TOLOWER" USING SYSCMD-VUTIL-RECS-GREP, VALUE 26.
           CALL "C$TOLOWER" USING SYSCMD-VUTIL-SIZE-VUTIL, VALUE 09.
           CALL "C$TOLOWER" USING SYSCMD-VUTIL-SIZE-GREP, VALUE 24.
           CALL "C$TOLOWER" USING WSS-LCASE-NUM-OF-RECORDS, VALUE 12.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

           MOVE MKTEMP-DEFAULT TO GR15DA-1-MKTEMP-BUF
                                  GR15DA-2-MKTEMP-BUF
                                  GR15DA-3-MKTEMP-BUF
                                  GR15DA-4-MKTEMP-BUF.

           MOVE SPACES         TO MESS.

           STRING MKTEMP-NAME, HEX-DOT-TXT
                  DELIMITED BY "  " INTO MESS.

           MOVE MESS           TO GR15DA-1-MKTEMP-NAME
                                  GR15DA-2-MKTEMP-NAME
                                  GR15DA-3-MKTEMP-NAME
                                  GR15DA-4-MKTEMP-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-GR15DA-1-FILE-OUTPUT.
           MOVE SPACES TO GR15DA-1-REC.

           STRING "FILENAME",        HEX-TAB,
                  "LEVEL",           HEX-TAB,
                  "GROUP/BRNO",      HEX-TAB,
                  "RISC",            HEX-TAB,
                  "STATE",           HEX-TAB,
                  "DATA",            HEX-TAB,
                  "BRANCH INFO",     HEX-TAB,
                  "PATH",            HEX-TAB,
                  "FILE SIZE",       HEX-TAB,
                  "# OF RECORDS",    HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE INTO GR15DA-1-REC.
        
           PERFORM WRITE-GR15DA-1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-GR15DA-2-FILE-OUTPUT.
           MOVE SPACES TO GR15DA-2-REC.

           STRING "FILENAME",        HEX-TAB,
                  "LEVEL",           HEX-TAB,
                  "GROUP/BRNO",      HEX-TAB,
                  "RISC",            HEX-TAB,
                  "STATE",           HEX-TAB,
                  "DATA",            HEX-TAB,
                  "BRANCH INFO",     HEX-TAB,
                  "PATH",            HEX-TAB,
                  "FILE SIZE",       HEX-TAB,
                  "# OF RECORDS",    HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE INTO GR15DA-2-REC.
        
           PERFORM WRITE-GR15DA-2-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-GR15DA-3-FILE-OUTPUT.
           MOVE SPACES TO GR15DA-3-REC.

           STRING "LEVEL",           HEX-TAB,
                  "GROUP/BRNO",      HEX-TAB,
                  "FILENAME",        HEX-TAB,
                  "RISC",            HEX-TAB,
                  "STATE",           HEX-TAB,
                  "DATA",            HEX-TAB,
                  "BRANCH INFO",     HEX-TAB,
                  "PATH",            HEX-TAB,
                  "FILE SIZE",       HEX-TAB,
                  "# OF RECORDS",    HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE INTO GR15DA-3-REC.
        
           PERFORM WRITE-GR15DA-3-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-GR15DA-4-FILE-OUTPUT.
           MOVE SPACES TO GR15DA-4-REC.

           STRING "LEVEL",           HEX-TAB,
                  "GROUP/BRNO",      HEX-TAB,
                  "FILENAME",        HEX-TAB,
                  "RISC",            HEX-TAB,
                  "STATE",           HEX-TAB,
                  "DATA",            HEX-TAB,
                  "BRANCH INFO",     HEX-TAB,
                  "PATH",            HEX-TAB,
                  "FILE SIZE",       HEX-TAB,
                  "# OF RECORDS",    HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE INTO GR15DA-4-REC.
        
           PERFORM WRITE-GR15DA-4-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO VUTIL-INFO-PATH.
           PERFORM OPEN-VUTIL-INFO-FILE-OUTPUT.
           PERFORM CLOSE-VUTIL-INFO-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK3-PATH.
           PERFORM OPEN-WK3-FILE-OUTPUT.
           PERFORM CLOSE-WK3-FILE.
           PERFORM OPEN-WK3-FILE.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAMETERS-BEGIN.

           MOVE RISC1 TO CURRENT-MACHINE
           PERFORM SEND-TITLE.
           PERFORM SEND-FIL.
           PERFORM SEND-MACHINE.
           PERFORM SEND-CUSTOMER-LINE.

           MOVE SOURCE-PATH                TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *JKC MOVE TARGET-PATH                TO WR-BUF.
      *JKC MOVE TARGET-PATH-LIST           TO WR-LIST.
      *JKC PERFORM CALL-WRFORM.

      *JKC IF ( UP-KEY )
      *JKC    GO TO ENTER-OPTION-GR.

      *-----------------------------------------------------------------
      *ENTER-PRINTER-OPTIONS.

      *    MOVE GR15DA-QUEUE-POS TO Q-POS.
      *    MOVE GR15DA-COPIES-POS TO C-POS.
      *    MOVE GR15DA-PRU-POS TO P-POS.
      *    MOVE SPACES TO Q-BUF C-BUF P-BUF.
      *    PERFORM PRU-IN.
      *    IF STAT NOT = "00"
      *       GO TO END-PROGRAM.

      *-----------------------------------------------------------------
       ENTER-SOURCE-PATH.

           MOVE LEGEND-SOURCE-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
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

           GO TO ENTER-BEG-BRANCH.   *> SKIP ENTRY

           MOVE LEGEND-TARGET-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090T-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
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

           MOVE "S  A000BEG-BRANCH." TO VDU.     *> FORCE WAC GROUP
           MOVE "WAC "               TO VDUBUF.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-TARGET-PATH.
           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-END.

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
      *JKC    GO TO ENTER-BEG-BRANCH.
              MOVE VDU---F7-KEY TO VDUSTATUS
              GO TO ENTER-PARAMETERS-END.

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
           IF ( F7-KEY ) GO TO ENTER-PARAMETERS-END.

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

      *-----------------------------------------------------------------
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = "N" )
              GO TO ENTER-PARAMETERS-BEGIN.

           IF ( VDUBUF NOT = "Y" )
              GO TO ENTER-ACCEPT-YN.

      *=================================================================
       ENTER-PARAMETERS-END.

           IF ( F7-KEY )
              MOVE "X" TO ERRCD
           ELSE
              MOVE SPACES TO ERRCD.

      *-----------------------------------------------------------------
       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R - F I L E
      *
      ******************************************************************
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

      *=================================================================
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
      *    U P D A T E - S O R T - M O D U L E
      *
      ******************************************************************
       UPDATE-SORT-MODULE SECTION.

           MOVE UP1534-FILE-INPUT          TO WSS-UP1534-FILE-INPUT.

           MOVE ZEROES TO SUB-PARENT.

      *-----------------------------------------------------------------
       PARENT-WK2-LOOP.

           ADD 1 TO SUB-PARENT.

           IF ( SUB-PARENT > 15 )
              GO TO PARENT-WK3-LOOP-START.

           IF ( WK1-SCURR(SUB-PARENT) = ZEROES )
              GO TO PARENT-WK2-LOOP.

           MOVE WK1-SCURR(SUB-PARENT)      TO SUB-WGR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ DETAIL }
           MOVE SPACES                     TO WK2-REC.
           MOVE WSS-UP1534-FILE-INPUT-NAME TO WK2-FILENAME.
           MOVE WGR-SID-LEVEL(SUB-WGR)     TO WK2-GR-LEVEL.
           MOVE WGR-SID      (SUB-WGR)     TO WK2-GR-ID.
           MOVE WK2-KEY                    TO HOLD-WK2-KEY.

           PERFORM READ-WK2-FILE.

           IF ( IO-BAD )
              PERFORM MISC-CREATE-WK2-FILE.

           ADD CURRENT-FILESIZE               TO WK2-FILESIZE.
           ADD CURRENT-RECORDS-COUNT          TO WK2-RECORDS-COUNT.
           PERFORM REWRITE-WK2-FILE

      *- - - - - - - - - - - - - - - - - - - - -{ TOTAL: LEVEL, GROUP }
           IF ( WGR-SID-LEVEL(SUB-WGR) = ZEROES )
              GO TO PARENT-WK2-LOOP.

           MOVE SPACES                     TO WK2-REC.
           MOVE "~~~"                      TO WK2-FILENAME.
           MOVE WGR-SID-LEVEL(SUB-WGR)     TO WK2-GR-LEVEL.
           MOVE WGR-SID      (SUB-WGR)     TO WK2-GR-ID.
           MOVE WK2-KEY                    TO HOLD-WK2-KEY.

           PERFORM READ-WK2-FILE.

           IF ( IO-BAD )
              PERFORM MISC-CREATE-WK2-FILE.

           ADD CURRENT-FILESIZE               TO WK2-FILESIZE.
           ADD CURRENT-RECORDS-COUNT          TO WK2-RECORDS-COUNT.
           PERFORM REWRITE-WK2-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TOTAL: FILE }
      *    MOVE SPACES                     TO WK2-REC.
      *    MOVE WSS-UP1534-FILE-INPUT-NAME TO WK2-FILENAME.
      *    MOVE "~"                        TO WK2-GR-LEVELX.
      *    MOVE "~~~"                      TO WK2-GR-ID.
      *    MOVE WK2-KEY                    TO HOLD-WK2-KEY.

      *    PERFORM READ-WK2-FILE.

      *    IF ( IO-BAD )
      *       PERFORM MISC-CREATE-WK2-FILE.

      *    ADD CURRENT-FILESIZE               TO WK2-FILESIZE.
      *    ADD CURRENT-RECORDS-COUNT          TO WK2-RECORDS-COUNT.
      *    PERFORM REWRITE-WK2-FILE

      *- - - - - - - - - - - - - - - - - - - - - -{ TOTAL: FILE, LEVEL }
           MOVE SPACES                     TO WK2-REC.
           MOVE WSS-UP1534-FILE-INPUT-NAME TO WK2-FILENAME.
           MOVE WGR-SID-LEVEL(SUB-WGR)     TO WK2-GR-LEVEL.
           MOVE "~~~"                      TO WK2-GR-ID.
           MOVE WK2-KEY                    TO HOLD-WK2-KEY.

           PERFORM READ-WK2-FILE.

           IF ( IO-BAD )
              PERFORM MISC-CREATE-WK2-FILE.

           ADD CURRENT-FILESIZE               TO WK2-FILESIZE.
           ADD CURRENT-RECORDS-COUNT          TO WK2-RECORDS-COUNT.
           PERFORM REWRITE-WK2-FILE

           GO TO PARENT-WK2-LOOP.

      *=================================================================
       PARENT-WK3-LOOP-START.

           MOVE ZEROES TO SUB-PARENT.

      *-----------------------------------------------------------------
       PARENT-WK3-LOOP.

           ADD 1 TO SUB-PARENT.

           IF ( SUB-PARENT > 15 )
              GO TO UPDATE-SORT-MODULE-EXIT.

           IF ( WK1-SCURR(SUB-PARENT) = ZEROES )
              GO TO PARENT-WK3-LOOP.

           MOVE WK1-SCURR(SUB-PARENT)      TO SUB-WGR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ DETAIL }
           MOVE SPACES                     TO WK3-REC.
           MOVE WGR-SID-LEVEL(SUB-WGR)     TO WK3-GR-LEVEL.
           MOVE WGR-SID      (SUB-WGR)     TO WK3-GR-ID.
           MOVE WSS-UP1534-FILE-INPUT-NAME TO WK3-FILENAME.
           MOVE WK3-KEY                    TO HOLD-WK3-KEY.

           PERFORM READ-WK3-FILE.

           IF ( IO-BAD )
              PERFORM MISC-CREATE-WK3-FILE.

           ADD CURRENT-FILESIZE               TO WK3-FILESIZE.
           ADD CURRENT-RECORDS-COUNT          TO WK3-RECORDS-COUNT.
           PERFORM REWRITE-WK3-FILE

      *- - - - - - - - - - - - - - - - - - - - -{ TOTAL: LEVEL, GROUP }
           MOVE SPACES                     TO WK3-REC.
           MOVE WGR-SID-LEVEL(SUB-WGR)     TO WK3-GR-LEVEL.
           MOVE WGR-SID      (SUB-WGR)     TO WK3-GR-ID.
           MOVE "~~~"                      TO WK3-FILENAME.
           MOVE WK3-KEY                    TO HOLD-WK3-KEY.

           PERFORM READ-WK3-FILE.

           IF ( IO-BAD )
              PERFORM MISC-CREATE-WK3-FILE.

           ADD CURRENT-FILESIZE               TO WK3-FILESIZE.
           ADD CURRENT-RECORDS-COUNT          TO WK3-RECORDS-COUNT.
           PERFORM REWRITE-WK3-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ TOTAL: LEVEL }
           MOVE SPACES                     TO WK3-REC.
           MOVE WGR-SID-LEVEL(SUB-WGR)     TO WK3-GR-LEVEL.
           MOVE "~~~"                      TO WK3-GR-ID.
           MOVE "~~~"                      TO WK3-FILENAME.
           MOVE WK3-KEY                    TO HOLD-WK3-KEY.

           PERFORM READ-WK3-FILE.

           IF ( IO-BAD )
              PERFORM MISC-CREATE-WK3-FILE.

           ADD CURRENT-FILESIZE               TO WK3-FILESIZE.
           ADD CURRENT-RECORDS-COUNT          TO WK3-RECORDS-COUNT.
           PERFORM REWRITE-WK3-FILE

      *- - - - - - - - - - - - - - - - - - - - - -{ TOTAL: LEVEL, FILE }
           IF ( WGR-SID-LEVEL(SUB-WGR) = 0 OR 1 )
              GO TO PARENT-WK3-LOOP.

           MOVE SPACES                     TO WK3-REC.
           MOVE WGR-SID-LEVEL(SUB-WGR)     TO WK3-GR-LEVEL.
           MOVE "~~~"                      TO WK3-GR-ID.
           MOVE WSS-UP1534-FILE-INPUT-NAME TO WK3-FILENAME.
           MOVE WK3-KEY                    TO HOLD-WK3-KEY.

           PERFORM READ-WK3-FILE.

           IF ( IO-BAD )
              PERFORM MISC-CREATE-WK3-FILE.

           ADD CURRENT-FILESIZE               TO WK3-FILESIZE.
           ADD CURRENT-RECORDS-COUNT          TO WK3-RECORDS-COUNT.
           PERFORM REWRITE-WK3-FILE

           GO TO PARENT-WK3-LOOP.

      *=================================================================
       UPDATE-SORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    V U T I L - I N F O - M O D U L E
      *
      ******************************************************************
       VUTIL-INFO-MODULE SECTION.

           PERFORM UP1534-LOAD-FILE-PATH.
           MOVE UP1534-FILE-PATH           TO ACCESS-BUF
                                              CURRENT-FILEPATH.
           PERFORM ACCESS-CALL.

           IF ( STAT-BAD )
              GO TO VUTIL-INFO-MODULE-ERROR.
           
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE: NEED TO DO '# OF RECORDS' FIRST THEN 'FILE SIZE'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CURRENT-FILEPATH           TO SYSCMD-VUTIL-RECS-FILE
                                              SYSCMD-VUTIL-SIZE-FILE.
           MOVE VUTIL-INFO-PATH            TO SYSCMD-VUTIL-RECS-MKTEMP
                                              SYSCMD-VUTIL-SIZE-MKTEMP.

           MOVE SYSCMD-VUTIL-RECS          TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           IF ( STAT-BAD )
              GO TO VUTIL-INFO-MODULE-ERROR.

           MOVE SYSCMD-VUTIL-SIZE          TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           IF ( STAT-BAD )
              GO TO VUTIL-INFO-MODULE-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-VUTIL-INFO-FILE.

      *=================================================================
       VUTIL-INFO-MOD-READ-NEXT.

           PERFORM READ-VUTIL-INFO-FILE-NEXT.

           IF ( IO-BAD )
              GO TO VUTIL-INFO-MODULE-END.

           MOVE VUTIL-INFO-BUF TO WSS-VUTIL-INFO-BUF.
           INSPECT WSS-VUTIL-INFO-BUF-NUMX REPLACING ALL " " BY "0".

           IF ( WSS-VUTIL-INFO-BUF-BASE = WSS-LCASE-NUM-OF-RECORDS )
              MOVE WSS-VUTIL-INFO-BUF-NUM TO CURRENT-RECORDS-COUNT
           ELSE
              MOVE WSS-VUTIL-INFO-BUF-NUM TO CURRENT-FILESIZE.

           GO TO VUTIL-INFO-MOD-READ-NEXT.

      *=================================================================
       VUTIL-INFO-MODULE-ERROR.

           MOVE ZEROES TO CURRENT-FILESIZE 
                          CURRENT-RECORDS-COUNT.

      *=================================================================
       VUTIL-INFO-MODULE-END.

           PERFORM CLOSE-VUTIL-INFO-FILE.

      *-----------------------------------------------------------------
       VUTIL-INFO-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-SYSDATE TO GR15DA-SYSDATE.
           DISPLAY GR15DA-SCREEN UPON GR15DA-WINDOW-HANDLE.
           MOVE GR15DA-FORM-FIELDS TO WR-BUF.
           MOVE GR15DA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE GR15DA-HELP--FILE TO HELP-LINK-FILE.
           MOVE GR15DA-HELP--DIR  TO HELP-LINK-DIR.

      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUP/GR15DA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.

       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *COPY "LIBGB/GETPR.CPY".
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

      *=================================================================
       MISC-CREATE-WK2-FILE.
           MOVE SPACES                     TO WK2-REC.
           MOVE HOLD-WK2-KEY               TO WK2-KEY.

           IF ( WK2-GR-LEVEL = 0 )
              MOVE OBR-MACHINE             TO WK2-RISC-MACHINE
              MOVE OBR-STATE               TO WK2-STATE
              MOVE OBR-DATA-PATH           TO WK2-DATA-PATH
              MOVE CURRENT-FILEPATH        TO WK2-FILEPATH.

           MOVE ZEROES                     TO WK2-FILESIZE.
           MOVE ZEROES                     TO WK2-RECORDS-COUNT.

           PERFORM WRITE-WK2-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

      *-----------------------------------------------------------------
       MISC-CREATE-WK3-FILE.
           MOVE SPACES                     TO WK3-REC
           MOVE HOLD-WK3-KEY               TO WK3-KEY.

           IF ( WK3-GR-LEVEL  = 0     ) AND
              ( WK3-GR-ID NOT = "~~~" )
              MOVE OBR-MACHINE             TO WK3-RISC-MACHINE
              MOVE OBR-STATE               TO WK3-STATE
              MOVE OBR-DATA-PATH           TO WK3-DATA-PATH
              MOVE CURRENT-BRANCH-INFO     TO WK3-BRANCH-INFO
              IF ( WK3-FILENAME NOT = "~~~" )
                 MOVE CURRENT-FILEPATH     TO WK3-FILEPATH.

           MOVE ZEROES                     TO WK3-FILESIZE.
           MOVE ZEROES                     TO WK3-RECORDS-COUNT.

           PERFORM WRITE-WK3-FILE

           IF ( IO-BAD )
              GO TO IO-ERROR.

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

           MOVE SPACES                     TO GR15DA-FILE-PATH.
           STRING "/", GETENV-BUF
                  DELIMITED BY "  "      INTO GR15DA-FILE-PATH.
           MOVE GR15DA-FILE-PATH-MACHINE   TO CURRENT-MACHINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK TO SEE IF THE SOURCE & TARGET PATHS ARE SET FROM BATCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SOURCE-PATH = SPACES )
              MOVE GR15DA-FILE-PATH        TO SOURCE-PATH
           ELSE
              MOVE SOURCE-PATH             TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE GR15DA-FILE-PATH     TO SOURCE-PATH.
     
           IF ( TARGET-PATH = SPACES )
              MOVE GR15DA-FILE-PATH        TO TARGET-PATH
           ELSE
              MOVE TARGET-PATH             TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE GR15DA-FILE-PATH     TO TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CURRENT FIL: "            TO FIL-PROMPT.
           MOVE GR15DA-FILE-PATH           TO FIL-INFO.

           MOVE FIL-BUF                    TO WR-BUF.
           MOVE FIL-LIST                   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND-EXTRACT.
           MOVE "EXTRACTING..."            TO WR-BUF.
           MOVE LEGEND-LIST                TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND-PROCESS-BRNO.
           MOVE SPACES                     TO LEGEND.
           STRING "PROCESSING: ", WK1-BRNO
                  DELIMITED BY "  "      INTO LEGEND.
           MOVE LEGEND                     TO WR-BUF.
           MOVE LEGEND-LIST                TO WR-LIST.
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
           MOVE "[ A15 GROUP DATA - UP1534    ]" TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/GBWK3IN.CPY".

      *=================================================================
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

       READ-OBR-FILE.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE.
           IF IO-FG = 8
              GO TO READ-OBR-FILE.

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

      *=================================================================
       OPEN-GR15DA-1-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE GR15DA-1-PATH TO E-FILE.
           OPEN OUTPUT GR15DA-1-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-GR15DA-1-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE GR15DA-1-FILE
              GO TO OPEN-GR15DA-1-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-GR15DA-1-FILE.

           PERFORM READ-IT.
           MOVE GR15DA-1-PATH TO E-FILE.
           READ GR15DA-1-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-GR15DA-1-FILE.

      *-----------------------------------------------------------------
       WRITE-GR15DA-1-FILE.

           PERFORM WRITE-IT.
           MOVE GR15DA-1-PATH TO E-FILE.
           WRITE GR15DA-1-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-GR15DA-1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GR15DA-1-FILE.

           PERFORM CLOSE-IT.
           CLOSE GR15DA-1-FILE.

      *=================================================================
       OPEN-GR15DA-2-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE GR15DA-2-PATH TO E-FILE.
           OPEN OUTPUT GR15DA-2-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-GR15DA-2-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE GR15DA-2-FILE
              GO TO OPEN-GR15DA-2-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-GR15DA-2-FILE.

           PERFORM READ-IT.
           MOVE GR15DA-2-PATH TO E-FILE.
           READ GR15DA-2-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-GR15DA-2-FILE.

      *-----------------------------------------------------------------
       WRITE-GR15DA-2-FILE.

           PERFORM WRITE-IT.
           MOVE GR15DA-2-PATH TO E-FILE.
           WRITE GR15DA-2-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-GR15DA-2-FILE.

      *-----------------------------------------------------------------
       CLOSE-GR15DA-2-FILE.

           PERFORM CLOSE-IT.
           CLOSE GR15DA-2-FILE.

      *=================================================================
       OPEN-GR15DA-3-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE GR15DA-3-PATH TO E-FILE.
           OPEN OUTPUT GR15DA-3-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-GR15DA-3-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE GR15DA-3-FILE
              GO TO OPEN-GR15DA-3-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-GR15DA-3-FILE.

           PERFORM READ-IT.
           MOVE GR15DA-3-PATH TO E-FILE.
           READ GR15DA-3-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-GR15DA-3-FILE.

      *-----------------------------------------------------------------
       WRITE-GR15DA-3-FILE.

           PERFORM WRITE-IT.
           MOVE GR15DA-3-PATH TO E-FILE.
           WRITE GR15DA-3-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-GR15DA-3-FILE.

      *-----------------------------------------------------------------
       CLOSE-GR15DA-3-FILE.

           PERFORM CLOSE-IT.
           CLOSE GR15DA-3-FILE.

      *=================================================================
       OPEN-GR15DA-4-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE GR15DA-4-PATH TO E-FILE.
           OPEN OUTPUT GR15DA-4-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-GR15DA-4-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE GR15DA-4-FILE
              GO TO OPEN-GR15DA-4-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-GR15DA-4-FILE.

           PERFORM READ-IT.
           MOVE GR15DA-4-PATH TO E-FILE.
           READ GR15DA-4-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-GR15DA-4-FILE.

      *-----------------------------------------------------------------
       WRITE-GR15DA-4-FILE.

           PERFORM WRITE-IT.
           MOVE GR15DA-4-PATH TO E-FILE.
           WRITE GR15DA-4-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-GR15DA-4-FILE.

      *-----------------------------------------------------------------
       CLOSE-GR15DA-4-FILE.

           PERFORM CLOSE-IT.
           CLOSE GR15DA-4-FILE.

      *=================================================================
       OPEN-VUTIL-INFO-FILE.

           PERFORM OPEN-IT.
           MOVE VUTIL-INFO-IFN TO E-FILE.
           OPEN I-O VUTIL-INFO-FILE.

           IF ( IO-LOCKED       ) GO TO OPEN-VUTIL-INFO-FILE.
           IF ( IO-ALREADY-OPEN ) CLOSE VUTIL-INFO-FILE
                                  GO TO OPEN-VUTIL-INFO-FILE.

      *-----------------------------------------------------------------
       OPEN-VUTIL-INFO-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE VUTIL-INFO-IFN TO E-FILE.
           OPEN OUTPUT VUTIL-INFO-FILE.

           IF ( IO-LOCKED       ) GO TO OPEN-VUTIL-INFO-FILE-OUTPUT.
           IF ( IO-ALREADY-OPEN ) CLOSE VUTIL-INFO-FILE
                                  GO TO OPEN-VUTIL-INFO-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-VUTIL-INFO-FILE.

           PERFORM READ-IT.
           MOVE VUTIL-INFO-IFN TO E-FILE.
           MOVE VUTIL-INFO-KEY TO E-KEYX.
           READ VUTIL-INFO-FILE.

           IF ( IO-LOCKED ) GO TO READ-VUTIL-INFO-FILE.

      *-----------------------------------------------------------------
       READ-VUTIL-INFO-FILE-NEXT.

           PERFORM READ-IT.
           MOVE VUTIL-INFO-IFN TO E-FILE.
           MOVE VUTIL-INFO-KEY TO E-KEYX.
           READ VUTIL-INFO-FILE NEXT.

           IF ( IO-LOCKED ) GO TO READ-VUTIL-INFO-FILE-NEXT.

      *-----------------------------------------------------------------
       CLOSE-VUTIL-INFO-FILE.

           PERFORM CLOSE-IT.
           CLOSE VUTIL-INFO-FILE.

      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-PROG.
           PERFORM CLOSE-FILES.
      *    PERFORM PRU-OUT.
           MOVE VUTIL-INFO-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK3-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           MOVE "UP/UPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GR15DA-SCREEN.
           DESTROY GR15DA-WINDOW-HANDLE.
           EXIT PROGRAM.
