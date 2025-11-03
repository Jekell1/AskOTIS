       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SCRN1.
       AUTHOR.                          MICHAEL J DONIGAN.
       DATE-WRITTEN.                    1998/02/XX - 99:99:99.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.


       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    -------------------------------------------------------------
      *    DESCRIPTION: CONVERT PARADATA FORM TO ACU SCREEN SECTION
      *                 AND COPY MEMBERS.
      *
      *    INPUT FILE IS LOCATED IN $REL/FM/{2 CHAR DIR}/{FORM NAME}
      *
      *    OUTPUT FILE IS LOCATED IN $RELSRC/LIB{2 CHAR DIR}/{FORM NAME}
      *       INPUT FILE MUST HAVE BEEN SUBJECT TO FORMSBA
      *
      *
      *    INPUT FILE FORMAT:
      *
      *    P                  -DENOTES PROMPT
      *    QUEUE REPORT:      -PROMPT TEXT
      *    3                  -LINE COORDINATE
      *    11                 -COLUMN COORDINATE
      *    Y                  -BOLD Y/N
      *      ...
      *    F                  -DENOTES FIELD
      *    AC1                -FIELD NAME
      *    ______             -FIELD VALUE
      *    N(6)               -FIELD PICTURE (N-ALPHA NUM D-NUMERIC)
      *    21                 -FIELD LINE COORDINATE
      *    29                 -FIELD COLUMN COORDINATE
      *    YNYNNNNNNNY        -FIELD DISPLAY ATTRIBUTES
      *      ...
      *    -------------------------------------------------------------
      *   MJD 120521 REMOVING .HE FILE. HELP WILL BE HANDLED IN A
      *              DIFFERENT METHOD
      *   MJD 120522 CHANGED IN-FILE PATH FOR FORM INFO TO BE:
      *               /USR/A??/FM/XX/[{2 CHAR DIR}/{FORM NAME}
      *               HOPE TO EVENTUALLY MAKE IT RECURSIVE FOR
      *               MULTIPLE FORMS AT A TIME
      *               - REMOVED EXTRA SPACES IN "VALUE" DECLARATIONS
      *               - ADDED "LABEL" AND "ENTRY-FIELD" TAGS TO .SCN
      *               - FIXED EMBEDDED SPACES WHEN FORM NAME < 6 CHARS
      *               - REMOVED NON DISPLAY FROM .WKS .EVA AND .DEF REC
      *               - WRITE NONDISPLAY FIELDS TO LOG FILE
      *                  /$REL/NDLOGS/{DIR}_{FORM NAME}.NDS
      *               - ADDED OPTION TO CONVER EN ENTIRE DIRECTORY AT ONCE
      *    MJD 120528 - ADDED OPTION TO CONVERT ALL FORMS AT ONCE
      *               - ADDED REPORT ONLY OPTION
      *               - WRITE ERRORS TO LOG FILE
      *                  /$REL/ERLOGS/{DIR}_{FORM NAME}.ERR
      *                  CURRENTLY JUST LISTS WARNINGS FOR ODD FIELD PICTURS
      *               - ADDED A LINE TO *.EVA TO MOVE FORM ATTRIBUTES.
      *                 NEEDED THIS FOR RIGHT JUSTIFICATION ON ACCEPT.
      *               - CORRECTED ALL ODD FIELD TYPES (WARNINGS IN ERRLOGS)
      *               - FIXED A PROBLEM WITH ENTRY FIELD ATTRIBUTES.
      *                 "IF" WAS LOOKING FOR LOWER CASE BUT FIELD WAS
      *                 CONVERTED TO UPPER FURTHUR UP IN THE CODE.
      *    MJD 120602 - ADDED LOGIC TO SKIP DIRECTORIES FX, CV, AM, QA
      *               - ADDED LOGIC TO SKIP LOGGING OF THE FOLLOWING ND FIELDS
      *                 VOLID, TRUE, REPORT, REPNO, MAXPRU, ERRPR,
      *                 FORM1, MISC1, F9F10
      *                 AS PER NONDISPLAYS DOC IN THE A15 VERYANT DIRECTORY.
      *    MJD 120608 - COMBINED .DEF AND .WKS INTO ONE FILE .WKS
      *    MJD 120823 - BROKE APART .DEF AND .WKS INTO SEP FILE AGAIN.
      *                 IF A PROGRAM CONTAINS TWO OR MORE FORMS (I.E. SUB
      *                 WINDOWS THAT ARE NOT CALLED PROGRAMS BUT ARE PART
      *                 OF THE CALLING PROGRAM) THERE ARE PROBLEMS BECAUSE THE
      *                 88'S THAT REDEFINE ATTRIBUTES FOR EACH FORM NEED TO BE
      *                 CONTIGUOUS.   THERE ARE ALSO DUPS IF THEY HAVE THE SAME
      *                 FIELD NAMES.
      *    MJD 121016 - CREATED AN 01 LEVEL FOR WKS FILE XXXX-FORM-FIELDS
      *                 MADE EXISTING FORM FIELDS 03'S UNDER THIS 01 LEVEL
      *               - CHANGED HELP--FILE TO BE XXXX-HELP--FILE
      *                 XXXX DENOTES FORM NAME.
      *    MJD 121019 - IF THE VALUE IN THE FORM IS "_*" THEN I AM SETTING
      *                 THE VALUE IN WORKING STORAGE TO SPACES.
      *    MJD 121022 - CHANGED PIC 9'S TO PIC X'S.    I DIDN'T REALIZE
      *                 WE ACTUALLY HAD ANY PIC 9'S IN THE FORMS.
      *    MJD 121022 - CHANGES FOR PRU TIL WE DETERMINE PRINTING METHOD.
      *                 IF FIELD = QUEUE AND IS NOT NON-DISP AND = "Y"
      *                 THEN FORCE "V" TO VIEW
      *    MJD 121128 - FIXED AN ISSUE WITH EL-CHG.   IT WAS GETTING RESET
      *                 IN THE ERROR ROUTINE AND THEREFORE THE ADJUSTED
      *                 PIC WAS NOT GETTING UPDATED PROPERLY.
      *    MJD 121128 - ADDED LOGIC TO SUBTRACT 1 FROM THE COL VALUE IF
      *                 THE FORM  NAME IS MODBOR, CHBORR OR BWCREA.
      *                 THIS WILL CORRECT ISSUES WITH THE SCREEN FITTING
      *                 INTO THE 78X22 FRAME.
      *   MJD 130830 ADDED GETFMW
      *   MJD  131209 CORRECTED LOGIC TO CORRECTLY CONVERT FIELDS THAT ARE
      *               DEFINED IN THE FORM PIC 9 AS TEXT.   ADDED LOGIC TO
      *               CORRECTLY HANDLE LMMAIN FIELD 020 WHICH IS LENGTH 60
      *               AND ALL "-".
      * MJD 140604 ADDED LOGIC TO CREATE LIST FOR WRFORM AND APPEND TO _WKS.
      *            REMOVED TRAILING SPACES FROM FILES
      * MJD 141002 FIXED EVA TO MOVE ATTRIBUTE WORKER FROM DEF INSTEAD OF
      *            USING HARD CODED ATTRIBUTES.
      *            FOR SCANS MAKE THE BOLD ATTRIBUTE AN "R" WHICH WILL BE
      *            INTERPRETED AS "BOLD" AND "REVERSE" IN THE SCREEN HANDLER
      *            THE FIELD IS A TITLE OR WSEL/LEGEND.  APPLY THIS TO
      *            WI/CL/PGMENU AS WELL.
      * JKC 2024-0105 THE ORIGINAL LOCATION OF THE FORM TEMPLATE FILES
      *          (ASCII) WAS IN /USR/X##/FM BUT WAS MOVED TO BE WITHIN
      *          /US2/SRC/X##/FM (SOFT LINK TO /US2/SVN/X##/WKS/FM) SO
      *          THAT IT CAN BE SAVED WITHIN THE SVN REPOSITORY.
      *          TO ACHIEVE THAT I CHANGE WS-REL-PATH FROM PIC X(07)
      *          [USR/X##] TO PIC X(11) [US2/SRC/X##].
      *
      ******************************************************************
           SELECT IN-FILE ASSIGN SRC-PATH
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS INFILE-STAT.

           SELECT SCN-FILE ASSIGN SCN-PATH
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT WKS-FILE ASSIGN WKS-PATH
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT WKS-FILE2 ASSIGN WKS-PATH2
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DEF-FILE  ASSIGN DEF-PATH
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT EVA-FILE  ASSIGN EVA-PATH
                   ORGANIZATION IS LINE SEQUENTIAL.

      *--->SELECT HLP-FILE  ASSIGN HLP-PATH
      *--->        ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NDS-FILE  ASSIGN NDS-PATH
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERR-FILE  ASSIGN ERR-PATH
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LIST-FILE1 ASSIGN LIST-FILE1-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.

           SELECT LIST-FILE2 ASSIGN LIST-FILE2-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT-DIR.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  IN-FILE     DATA RECORD IS IN-REC.
       01  IN-REC          PIC X(80).
       01  IN-RECA.
           03  IN-LINE-COL PIC 9(03).
           03  FILLER      PIC X(77).

       FD  SCN-FILE    DATA RECORD IS SCN-REC.
       01  SCN-REC         PIC X(80).

       FD  WKS-FILE   DATA RECORD IS WKS-REC.
       01  WKS-REC        PIC X(80).

       FD  WKS-FILE2   DATA RECORD IS WKS-REC2.
       01  WKS-REC2        PIC X(80).

       FD  DEF-FILE    DATA RECORD IS DEF-REC.
       01  DEF-REC         PIC X(80).

       FD  EVA-FILE    DATA RECORD IS EVA-REC.
       01  EVA-REC         PIC X(80).

      *FD  HLP-FILE    DATA RECORD IS HLP-REC.
      *01  HLP-REC         PIC X(80).

       FD  NDS-FILE    DATA RECORD IS NDS-REC.
       01  NDS-REC         PIC X(80).

       FD  ERR-FILE    DATA RECORD IS ERR-REC.
       01  ERR-REC         PIC X(132).

       FD  LIST-FILE1
           RECORD IS VARYING IN SIZE FROM 1 TO 80 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LIST-REC.
           03  LIST-CHAR             PIC X OCCURS 80 TIMES.

       FD  LIST-FILE2
           RECORD IS VARYING IN SIZE FROM 1 TO 80 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LIST-REC2.
           03  LIST-CHAR2             PIC X OCCURS 80 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)GB/SCRN1.CBL S35 03/15/24-08:34:55 >".
      * MISC VALUES
       78  LOWER-A                     VALUE X'61'.
       78  LOWER-B                     VALUE X'62'.
       78  LOWER-C                     VALUE X'63'.
       78  LOWER-D                     VALUE X'64'.
       78  LOWER-E                     VALUE X'65'.
       78  LOWER-F                     VALUE X'66'.
       78  LOWER-G                     VALUE X'67'.
       78  LOWER-H                     VALUE X'68'.
       78  LOWER-I                     VALUE X'69'.
       78  LOWER-J                     VALUE X'6A'.
       78  LOWER-K                     VALUE X'6B'.
       78  LOWER-L                     VALUE X'6C'.
       78  LOWER-M                     VALUE X'6D'.
       78  LOWER-N                     VALUE X'6E'.
       78  LOWER-O                     VALUE X'6F'.
       78  LOWER-P                     VALUE X'70'.
       78  LOWER-Q                     VALUE X'71'.
       78  LOWER-R                     VALUE X'72'.
       78  LOWER-S                     VALUE X'73'.
       78  LOWER-T                     VALUE X'74'.
       78  LOWER-U                     VALUE X'75'.
       78  LOWER-V                     VALUE X'76'.
       78  LOWER-W                     VALUE X'77'.
       78  LOWER-X                     VALUE X'78'.
       78  LOWER-Y                     VALUE X'79'.
       78  LOWER-Z                     VALUE X'7A'.
       78  WS-QUOTE                    VALUE X'22'.
       78  WS-SPACE                    VALUE " ".
       78  WS-PERIOD                   VALUE ".".
       78  WS-STAR                     VALUE "*".

      * DISPLAY ATTRIBUTES:
       78  DISP-UNDERLINE              VALUE "UNDERLINE.".
       78  DISP-BOLD                   VALUE "BOLD.     ".
       78  DISP-LOW                    VALUE "LOW.      ".
       78  DISP-STANDARD               VALUE "STANDARD. ".
       78  DISP-BLINK                  VALUE "BLINK.    ".
       78  DISP-REVERSE                VALUE "REVERSE.  ".

       78  DISP--88                    VALUE "-88".
       78  DISP--POS                   VALUE "-POS".
       78  DISP--SIZE                  VALUE "-SIZE".
       78  DISP--ATR                   VALUE "-ATR".
       78  DISP-POS                    VALUE "POS".
       78  DISP-SIZE                   VALUE "SIZE".
       78  DISP-VDU-POS                VALUE "VDU-POSX".
       78  DISP-VDU-SIZE               VALUE "VDU-SIZEX".
       78  DISP-VDU-ATR                VALUE "VDU-ATRX".

       01  DISPLAY-PATH                  PIC X(40).
       01  SRC-PATH                      PIC X(40).
       01  DEF-PATH                      PIC X(40).
       01  EVA-PATH                      PIC X(40).
      *01  HLP-PATH                      PIC X(40).
       01  NDS-PATH                      PIC X(40).
       01  ERR-PATH                      PIC X(40).
       01  SCN-PATH                      PIC X(40).
       01  WKS-PATH                     PIC X(40).
       01  WKS-PATH2                    PIC X(40).
       01  LIST-FILE1-PATH               PIC X(40).
       01  LIST-FILE2-PATH               PIC X(40).
       01  LIST-FILE2-OPEN-FG            PIC X(01) VALUE "N".
           88 LIST-FILE2-OPEN            VALUE "Y".
       01  INPUT-DIR                     PIC X(02).
       01  INPUT-FILE                    PIC X(20).
       01  HELP-FILE                     PIC X(06).

       01  FORM-NAME                     PIC X(06).
           88 FORM-NAME-SCAN2      VALUE "PGMENU" "CLMENU" "WIMENU".
       01  FILLER REDEFINES FORM-NAME.
           03 FORM-NAME-12               PIC X(2).
           03 FORM-NAME-36               PIC X(4).
              88 FORM-NAME-SCAN    VALUE "SCAN" "SCN2" "SCN3"
                                         "LNSC" "LNS2" "LNS3"
                                         "2SCN" "MSCN" "ISCN".

       01  HELP-DIRECTORY                PIC X(02).
       01  HELP-FILE-DEFAULT             PIC X(09).
       01  JUNK                          PIC X(01).
       01  RUN-TYPE                      PIC X(01).
           88 REPORT-ONLY                VALUE "R".
      *01  MESS                          PIC X(61).
       01  SCRN1-POP-UP-WINDOW2          PIC X(10).
       01  SUB                           PIC 9(3).
       01  MSG1-LENGTH                   PIC 9(3).
       01  MSG1-REMAIN                   PIC 9(3).
       01  MSG1-COLX                     PIC X(2).
       01  FILLER REDEFINES MSG1-COLX.
           03  MSG1-COL                  PIC 9(2).
       01  MSG1-ROWX                     PIC X(2).
       01  FILLER REDEFINES MSG1-ROWX.
           03  MSG1-ROW                  PIC 9(2).
       01  WS-RELSRC.
           03  FILLER                    PIC X(01) VALUE "/".
           03  WS-RELSRC-PATH            PIC X(11) VALUE SPACES.
           03  FILLER                    PIC X(04) VALUE "/LIB".
           03  WS-RELSRC-DIR             PIC X(02) VALUE SPACES.
           03  FILLER                    PIC X(01) VALUE "/".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * THE ORIGINAL LOCATION OF THE FORM TEMPLATE FILES (ASCII) WAS
      * IN /USR/X##/FM BUT WAS MOVED TO BE WITHIN /US2/SRC/X##/FM
      * (SOFT LINK TO /US2/SVN/X##/WKS/FM) SO THAT IT CAN BE SAVED
      * WITHIN THE SVN REPOSITORY.                        JKC 2024-0105
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WS-REL.
           03  FILLER                    PIC X(01) VALUE "/".
      *    03  WS-REL-PATH               PIC X(07) VALUE SPACES.
           03  WS-REL-PATH               PIC X(11) VALUE SPACES.
           03  FILLER                    PIC X(07) VALUE "/FM/XX/".
           03  WS-REL-DIR                PIC X(02) VALUE SPACES.
           03  FILLER                    PIC X(01) VALUE "/".
       01  WS-LS                         PIC X(05) VALUE "LS -1".
       01  WS-LS-D                       PIC X(05) VALUE "LS -D".
       01  PROCESS-FULL-DIR              PIC X(01) VALUE "N".
       01  PROCESS-ALL-FG                PIC X(01) VALUE "N".
           88 PROCESS-ALL                VALUE  "Y".

       01  LIST-DIR-WORKER.
           03 LDW-PATH                     PIC X(15).
           03 LDW-DIR-NAME                 PIC X(02).
              88 LDW-SKIP-DIR              VALUE "CV" "FX"
                                                 "AM" "QA"
                                                 "CB" "UP" "UT".
           03 LDW-JUNK                     PIC X(63).
       01  LIST-REC-WORKER.
           03 LRW-PATH                     PIC X(18).
           03 LRW-FORM-NAME                PIC X(06).
           03 LRW-JUNK                     PIC X(56).

       01  INFILE-STAT                     PIC X(02).
       01  FILE-STAT-DIR                   PIC X(02).

      * - WORKERS FOR ERROR LOG
       01  FILES-OPENED                  PIC X(01) VALUE "N".
       01  EL-ERROR                      PIC X(01).
       01  EL-EXISTS                     PIC X(01) VALUE "N".
       01  EL-MESSAGE-TEXT               PIC X(129) VALUE SPACES.
      *01  FILLER REDEFINES EL-MESSAGE-TEXT.
      *    03  EL-MESS                   PIC X(80).
       01  EL-CHG                    PIC X(15).
      *    03  FILLER                    PIC X(34).

       01  OPJECTS-OPEN-FGS.
           03  BOX1-OPEN-FG              PIC X(01)  VALUE "N".
               88 BOX1-OPEN              VALUE "Y".
           03  WINDOW1-OPEN-FG           PIC X(01)  VALUE "N".
               88 WINDOW1-OPEN           VALUE "Y".


      * WORKERS FOR LOWER TO UPPER CONVERSION
       01  TEXT-DATA                     PIC X(80)  VALUE SPACES.
       78  TEXT-LEN                                 VALUE 80.

       01  WS-CONT                       PIC X(01)  VALUE SPACE.

      * LINE 1 OF SCREEN SECTION - PROMPTS
       01  LEVEL-ONE.
           03  FILLER       PIC X(07)  VALUE SPACES.
           03  FILLER       PIC X(04)  VALUE "01  ".
           03  L1-TITLE     PIC X(15)  VALUE SPACES.

      * LINE 1 OF WORKING STORAGE INPUT ITEM.
       01  D-FIELD-LINE-0.
           03  FILLER              PIC X(07)  VALUE SPACES.
           03  FILLER              PIC X(04)  VALUE "01  ".
           03  WS-FORM-FIELD0      PIC X(30)  VALUE SPACES.

       01  D-FIELD-LINE-1.
           03  FILLER              PIC X(11)  VALUE SPACES.
           03  FILLER              PIC X(04)  VALUE "03  ".
           03  WS-FORM-FIELD       PIC X(19)  VALUE SPACES.
           03  FILLER              PIC X(05)  VALUE " PIC ".
           03  WS-FLD-PICTURE      PIC X(24)  VALUE SPACES.

       01  D-FIELD-LINE-1B.
           03  FILLER        PIC X(11)  VALUE SPACES.
           03  FILLER        PIC X(17)  VALUE "03  FILLER PIC X(".
           03  WS-FL-1B-PIC  PIC 99.
           03  FILLER        PIC X(08) VALUE ") VALUE ".
           03  FILLER        PIC X(01) VALUE WS-QUOTE.
           03  WS-FL-1B-VALUE PIC X(20) VALUE SPACES.

       01  D-FIELD-LINE-1B-LAST-PERIOD.
           03  FILLER              PIC X(11)  VALUE SPACES.
           03  FILLER              PIC X(27)
                   VALUE "03  FILLER PIC X(01) VALUE ".
           03  FILLER              PIC X(01) VALUE WS-QUOTE.
           03  FILLER              PIC X(01) VALUE ".".
           03  FILLER              PIC X(01) VALUE WS-QUOTE.
           03  FILLER              PIC X(01) VALUE ".".

      * LINE 1 OF ???????-NONDISPLAY-BUF (PICTURE CLAUSE).
       01  D-ND-LINE-1             PIC X(80) VALUE SPACES.

      * LINE 1 OF SCREEN SECTION INPUT ITEM.
       01  SCR-FLD-LINE-1.
           03  FILLER              PIC X(07)  VALUE SPACES.
           03  FILLER              PIC X(04)  VALUE "01  ".
           03  FILLER              PIC X(04)  VALUE "SCR-".
           03  SCR-FLD-NAME        PIC X(30)  VALUE SPACES.

      * LINE 2 OF WORKING STORAGE INPUT ITEM.
       01  D-FIELD-LINE-2.
           03  FILLER              PIC X(14)  VALUE SPACES.
           03  FILLER              PIC X(06)  VALUE "VALUE ".
           03  WS-FLD-VALUE        PIC X(54)  VALUE SPACES.

       01  D-COMMENT-LINE.
           03 FILLER               PIC X(07)  VALUE "      *".
           03  D-COMMENT           PIC X(65)  VALUE SPACES.

       01  LEVEL-THREE.
           03  FILLER       PIC X(06)  VALUE SPACES.
           03  L3-COMMENT   PIC X(01)  VALUE SPACES.
           03  FILLER       PIC X(04)  VALUE SPACES.
           03  FILLER       PIC X(03)  VALUE "03 ".
           03  FILLER       PIC X(12)  VALUE " LABEL LINE ".
      *    03  FILLER       PIC X(12)  VALUE "       LINE ".
           03  L3-LINE-NO   PIC X(02)  VALUE ZEROES.
           03  FILLER       PIC X(05)  VALUE " COL ".
           03  L3-COL-NO    PIC X(02)  VALUE ZEROES.
      *    03  FILLER       PIC X(01)  VALUE " ".
      *    03  L3-VALUE     PIC X(60)  VALUE SPACES.

       01  LEVEL-THREE2.
           03  FILLER       PIC X(12)  VALUE SPACES.
           03  L3-VALUE     PIC X(60)  VALUE ZEROES.

       01  LEVEL-THREE2B.
           03  FILLER       PIC X(12)  VALUE SPACES.
           03  L3B-VALUE    PIC X(60)  VALUE ZEROES.

       01  LEVEL-THREE-ITEM.
           03  FILLER       PIC X(06)  VALUE SPACES.
           03  L3-COMMENT2  PIC X(01)  VALUE SPACES.
           03  FILLER       PIC X(04)  VALUE SPACES.
           03  FILLER       PIC X(03)  VALUE "03 ".
           03  FILLER       PIC X(20)  VALUE " ENTRY-FIELD, USING ".
           03  L3-FIELD     PIC X(40)  VALUE SPACES.

       01  LEVEL-THREE-B.
           03  FILLER       PIC X(50)  VALUE SPACES.
           03  L3-ATTRIBUTE PIC X(10)  VALUE SPACES.

       01  SCRN-LINES.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(05)  VALUE "LINE ".
           03  SCRN-LINEX.
               05  SCRN-LINE PIC 9(02).

       01  SCRN-COLUMNS.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(05)  VALUE "COL  ".
           03  SCRN-COLX.
               05  SCRN-COL PIC 9(02).

       01  SCRN-CHARACTER.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(09)  VALUE "CHARACTER".

       01  SCRN-RJUS.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(22)  VALUE "RIGHT".
       01  SCRN-LJUS.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(22)  VALUE "LEFT".

       01  SCRN-BLANK-ZERO.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(15)  VALUE "BLANK WHEN ZERO".

       01  SCRN-REQD.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(08)  VALUE "REQUIRED".

       01  SCRN-ZFIL.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(09)  VALUE "ZERO-FILL".

       01  SCRN-BOLD.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(05)  VALUE "BOLD.".
       01  SCRN-BOLD-REV.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(14)  VALUE "BOLD, REVERSE.".

       01  SCRN-LOW.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(05)  VALUE "LOW. ".

       01  SCRN-AUTO.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(04)  VALUE "AUTO".

       01  SCRN-ECHO.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(04)  VALUE "ECHO".

       01  SCRN-NO-ECHO.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(07)  VALUE "NO ECHO".

       01  SCRN-UPPER.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(07)  VALUE "UPPER".

       01  SCRN-NO-BOX.
           03  FILLER       PIC X(15)  VALUE SPACES.
           03  FILLER       PIC X(07)  VALUE "NO-BOX".

       01  FIELD-WORKERS.
           03  FIELD-FIELD     PIC X(15).
           88  FIELD-FIELD-ND-SKIP  VALUE "VOLID"
                                          "TRUE"
                                          "REPORT"
                                          "REPNO"
                                          "MAXPRU"
                                          "ERRPR"
                                          "FORM1"
                                          "MISC1"
                                          "F9F10".
           88  FIELD-FIELD-BRHASH   VALUE "BR#".
           88  FIELD-FIELD-REV      VALUE "TITLE" "WSEL" "MSEL"
                                          "TITLEX" "LEGEND".

           03  FIELD-VALUE     PIC X(60).
           03  FIELD-PIC       PIC X(15).
           88  FIELD-PIC-FOR-9      VALUE
                           "D(09)" "D(9)"  "D(19)" "D(29)" "D(39)"
                           "D(49)" "D(59)" "D(69)" "D(79)"
                           "N(09)" "N(9)"  "N(19)" "N(29)" "N(39)"
                           "N(49)" "N(59)" "N(69)" "N(79)"
                           "X(09)" "X(9)"  "X(19)" "X(29)" "X(39)"
                           "X(49)" "X(59)" "X(69)" "X(79)"
                           "9(09)" "9(9)"  "9(19)" "9(29)" "9(39)"
                           "9(49)" "9(59)" "9(69)" "9(79)".
           88  FIELD-PIC-FOR-9B     VALUE
                           "9(09)" "9(9)"  "9(19)" "9(29)" "9(39)"
                           "9(49)" "9(59)" "9(69)" "9(79)".
           03  FIELD-SIZE      PIC 9(02).
           03  FIELD-ALP-NUM   PIC X(01).
           03  FIELD-POSITION  PIC 9(04).
               88  FIELD-IS-NONDISPLAY     VALUE 000000.
           03  FIELD-POSITIONX REDEFINES FIELD-POSITION.
               05  FIELD-LINEX.
                   07 FIELD-LINE   PIC 9(02).
               05  FIELD-COLX.
                   07 FIELD-COL    PIC 9(02).
           03  FIELD-ATTRIB.
               05  FIELD-HOLD  PIC X(01).
               05  FIELD-RJUS  PIC X(01).
               05  FIELD-BOLD  PIC X(01).
               05  FIELD-AUTO  PIC X(01).
               05  FIELD-ZFIL  PIC X(01).
               05  FIELD-VALU  PIC X(01).
               05  FIELD-MUST  PIC X(01).
               05  FIELD-CONS  PIC X(01).
               05  FIELD-TOTL  PIC X(01).
               05  FIELD-PERM  PIC X(01).
               05  FIELD-ECHO  PIC X(01).


      ******************************************************************
      *
      * THE FOLLOWING LINES MAKE UP THE XXXXX.DEF COPY MEMBER.
      *
      *  01  LPMENU-HELP-BUF.
      *      03  FILLER PIC X(22) VALUE "SEL            010201".
      ******************************************************************
       01  HELP-01-LINE.
           03  FILLER               PIC X(07) VALUE SPACES.
           03  FILLER               PIC X(04) VALUE "01  ".
           03  HELP-01-BUF          PIC X(30) VALUE SPACES.

       01  HELP-03-LINE.
           03  FILLER              PIC X(11) VALUE SPACES.
           03  FILLER              PIC X(27) VALUE
               "03  FILLER PIC X(22) VALUE ".
           03  FILLER               PIC X(01) VALUE WS-QUOTE.
           03  HELP-03-VALUE        PIC X(21) VALUE SPACES.
           03  HELP-03-VALUEX REDEFINES HELP-03-VALUE.
               05  HELP-03-FIELD    PIC X(15).
               05  HELP-03-POSITION PIC 9(04).
               05  HELP-03-SIZE     PIC 9(02).
           03  FILLER               PIC X(01) VALUE "\".
           03  FILLER               PIC X(01) VALUE WS-QUOTE.
           03  FILLER               PIC X(01) VALUE WS-PERIOD.

      ******************************************************************
      *
      * THE FOLLOWING LINES MAKE UP THE XXXXX.DEF COPY MEMBER.
      *
      ******************************************************************
      *-----------------------------------------------------------------
      * 88 LINE FOR POS
      * 88 XXXX_88             VALUE "XXXXXXXXXXXXX".
      *-----------------------------------------------------------------
       01  FLD-88-LINE.
           03  FILLER            PIC X(06)  VALUE SPACES.
           03  FLD-88-COMMENT    PIC X(01)  VALUE SPACE.
           03  FILLER            PIC X(05)  VALUE "88   ".
           03  FLD-88-NAME       PIC X(27)  VALUE SPACES.
           03  FILLER            PIC X(07)  VALUE " VALUE ".
           03  FLD-88-VALUE      PIC X(21)  VALUE SPACES.
       01  FLD-78-LINE.
           03  FILLER            PIC X(06)  VALUE SPACES.
           03  FLD-78-COMMENT    PIC X(01)  VALUE SPACE.
           03  FILLER            PIC X(05)  VALUE "78   ".
           03  FLD-78-NAME       PIC X(27)  VALUE SPACES.
           03  FILLER            PIC X(07)  VALUE " VALUE ".
           03  FLD-78-VALUE      PIC X(14)  VALUE SPACES.

      *-----------------------------------------------------------------
      * 78 LINE FOR POS
      * 78 XXXX_POS        VALUE 999999.
      *-----------------------------------------------------------------
       01  LC-78-LINE.
           03  FILLER            PIC X(06)  VALUE SPACES.
           03  LC-78-COMMENT     PIC X(01)  VALUE SPACE.
           03  FILLER            PIC X(05)  VALUE "78   ".
           03  LC-78-NAME        PIC X(27)  VALUE SPACES.
           03  FILLER            PIC X(01)  VALUE SPACES.
           03  FILLER            PIC X(07)  VALUE "VALUE ".
           03  LC-78-VALUE.
               05  LC-78-LIN     PIC 9(03)  VALUE ZEROS.
               05  LC-78-COL     PIC 9(03)  VALUE ZEROS.
           03  FILLER            PIC X(01)  VALUE ".".

      *-----------------------------------------------------------------
      * 78 LINE FOR SIZE
      * 78 XXXX_SIZE           VALUE 99.
      *-----------------------------------------------------------------
       01  SIZE-78-LINE.
           03  FILLER            PIC X(06)  VALUE SPACES.
           03  SIZE-78-COMMENT   PIC X(01)  VALUE SPACE.
           03  FILLER            PIC X(05)  VALUE "78   ".
           03  SIZE-78-NAME      PIC X(27)  VALUE SPACES.
           03  FILLER            PIC X(01)  VALUE SPACES.
           03  FILLER            PIC X(07)  VALUE "VALUE ".
           03  SIZE-78-VALUE     PIC 9(02)  VALUE ZEROS.
           03  FILLER            PIC X(01)  VALUE ".".

      *-----------------------------------------------------------------
      * 78 LINE FOR SIZE
      * 78 XXXX_SIZE           VALUE 99.
      *-----------------------------------------------------------------
       01  HELP-78-LINE.
           03  FILLER            PIC X(06)  VALUE SPACES.
           03  HELP-78-COMMENT   PIC X(01)  VALUE SPACE.
           03  FILLER            PIC X(05)  VALUE "78   ".
           03  HELP-78-NAME      PIC X(20)  VALUE "HELP--FILE".
           03  FILLER            PIC X(07)  VALUE SPACES.
           03  FILLER            PIC X(07)  VALUE "VALUE ".
           03  HELP-78-VALUE     PIC X(21)  VALUE SPACES.

      ******************************************************************
      *
      * THE FOLLOWING LINES MAKE UP THE XXXXX.EVA COPY MEMBER.
      *
      *     WHEN XXXXXXXX_88       MOVE XXXX_POS TO VDU-POS
      *                            MOVE XXXX_SIZE    TO VDU-ADJ-LEN
      ******************************************************************
       01  EVAL-LINE.
           03  FILLER            PIC X(06)  VALUE SPACES.
           03  EVAL-COMMENT      PIC X(01)  VALUE SPACE.
           03  FILLER            PIC X(04)  VALUE SPACES.
           03  EVAL-WHEN         PIC X(05)  VALUE "WHEN ".
           03  EVAL-NAME         PIC X(55)  VALUE SPACES.

       01  EVAL-LINE1.
           03  FILLER            PIC X(06)  VALUE SPACES.
           03  EVAL-COMMENT      PIC X(01)  VALUE SPACE.
           03  FILLER            PIC X(17)  VALUE SPACES.
           03  EVAL-MOVE         PIC X(05)  VALUE "MOVE ".
           03  EVAL-SOURCE       PIC X(28)  VALUE SPACES.
           03  FILLER            PIC X(04)  VALUE " TO ".
           03  EVAL-DEST         PIC X(18)  VALUE SPACES.

       01  EVAL-LINE2.
           03  FILLER            PIC X(23)  VALUE SPACES.
           03  FILLER            PIC X(06)  VALUE " MOVE ".
           03  FILLER            PIC X(01)  VALUE WS-QUOTE.
           03  FILLER            PIC X(02)  VALUE "00".
           03  FILLER            PIC X(25)  VALUE WS-QUOTE.
           03  FILLER            PIC X(11)  VALUE " TO STAT".

      ******************************************************************
      *
      * DISCLAIMER LINES
      *
      ******************************************************************
       01  COMMENT-DASH.
           03  FILLER            PIC X(07)  VALUE "      *".
           03  FILLER            PIC X(01)  OCCURS 66 TIMES
                                            VALUE "-".
       01  DISC-LINE1.
           03  FILLER            PIC X(08)  VALUE "      * ".
           03  FILLER            PIC X(66)  VALUE
               "DO NOT MODIFY THIS COPY MEMBER DIRECTLY.".
       01  DISC-LINE2.
           03  FILLER            PIC X(08)  VALUE "      * ".
           03  FILLER            PIC X(66)  VALUE
               "CODE CONTAINED HERE WAS GENERATED BY THE FORM".
       01  DISC-LINE3.
           03  FILLER            PIC X(08)  VALUE "      * ".
           03  FILLER            PIC X(66)  VALUE
               "CONVERSION PROGRAM.".
       01  DISC-LINE4.
           03  FILLER            PIC X(08)  VALUE "      * ".
           03  FILLER            PIC X(03)  VALUE
               "DO ".
           03  FILLER            PIC X(04)  VALUE
               "NOT ".
           03  FILLER            PIC X(07)  VALUE
               "CHANGE ".
           03  FILLER            PIC X(04)  VALUE
               "CASE".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETFMW.CPY".

      ******************************************************************
       PROCEDURE DIVISION USING FORM-PATHNAME EXIT-PATHNAME.
      ******************************************************************
       MAIN SECTION.
      D    STOP MESS.
       A100-OPEN.

           ACCEPT WS-RELSRC-PATH FROM ENVIRONMENT "RELSRC".
      *    ACCEPT WS-REL-PATH    FROM ENVIRONMENT "REL".
           ACCEPT WS-REL-PATH    FROM ENVIRONMENT "RELSRC".

           DISPLAY OMITTED
              BLANK SCREEN.

           DISPLAY "ENTER DIRECTORY OF FORM TO CONVERT: " AT 002002.
           DISPLAY "(ENTER DIRECTORY, Q=QUIT, *=ALL)" AT 002045.
           ACCEPT  INPUT-DIR AT 002039
                   ECHO
                   UPPER.
           EVALUATE INPUT-DIR
              WHEN "Q"     STOP RUN
              WHEN "*"     MOVE "Y" TO PROCESS-ALL-FG
                                       PROCESS-FULL-DIR
                           PERFORM CREATE-DIR-LIST
                           MOVE "*" TO INPUT-FILE
                           GO TO PROCESS-DIR
           END-EVALUATE.
           DISPLAY "                                " AT 002045.

           DISPLAY "ENTER FORM TO CONVERT: " AT 003002
           STRING "(Q = QUIT; * = ALL OF  " INPUT-DIR " )"
                DELIMITED BY SIZE
                INTO MESS.
           DISPLAY MESS AT 003040.
           ACCEPT  INPUT-FILE AT 003025
                   UPPER.
           EVALUATE INPUT-FILE
              WHEN "Q"     STOP RUN
           END-EVALUATE.
           MOVE SPACES TO MESS.
           DISPLAY MESS AT 003040.

       PROCESS-NEXT-DIR.
           IF PROCESS-ALL
              MOVE SPACES TO LIST-REC2
              READ LIST-FILE2 NEXT AT END MOVE "D" TO PROCESS-ALL-FG
                                          GO TO EXIT-PROGRAM
              END-READ
              IF FILE-STAT-DIR NOT = "00"
                 GO TO EXIT-PROGRAM
              END-IF
              MOVE LIST-REC2 TO LIST-DIR-WORKER
              IF FILE-STAT-DIR = "00" AND LDW-JUNK NOT = SPACES
                 GO TO PROCESS-NEXT-DIR
              END-IF
              IF LDW-SKIP-DIR
                 GO TO PROCESS-NEXT-DIR
              END-IF
              MOVE LDW-DIR-NAME TO INPUT-DIR
              MOVE "*" TO INPUT-FILE
           ELSE
              IF FILE-STAT = "++"
                 GO TO EXIT-PROGRAM.

       PROCESS-DIR.
           MOVE SPACES TO FORM-NAME.
           UNSTRING INPUT-FILE
                    DELIMITED BY " "
                    INTO FORM-NAME
                         MESS.

           IF NOT BOX1-OPEN
              DISPLAY BOX
                      AT 004002
                      LINES 8
                      SIZE 78
              END-DISPLAY.

           MOVE INPUT-DIR  TO WS-RELSRC-DIR
                              WS-REL-DIR

           PERFORM SET-PATHS.

           IF NOT BOX1-OPEN
              DISPLAY "INPUT FILE............ " AT 005003

              MOVE SPACES TO DISPLAY-PATH
              IF PROCESS-ALL
                 STRING WS-REL-PATH "/FM/XX/*"
                     DELIMITED BY " " INTO DISPLAY-PATH
                 DISPLAY DISPLAY-PATH AT 005025
              ELSE
                 DISPLAY SRC-PATH AT 005025
              END-IF
              DISPLAY "OUTPUT FILES.......... " AT 007003
              DISPLAY DEF-PATH  AT 007025
              DISPLAY EVA-PATH  AT 008025
              DISPLAY SCN-PATH  AT 009025
              DISPLAY WKS-PATH AT 010025
      *--->   DISPLAY HLP-PATH  AT 011025

              DISPLAY "ARE THESE CORRECT?     " AT 012002
              DISPLAY "(Y, N, R=REPORT ONLY, Q=QUIT)" AT 012030
              ACCEPT RUN-TYPE AT 012025
                     UPPER
              MOVE "Y" TO BOX1-OPEN-FG
              EVALUATE RUN-TYPE
                 WHEN "Y"     GO TO CONVERT-CONTINUE
                 WHEN "R"     GO TO CONVERT-CONTINUE
                 WHEN "Q"     STOP RUN
                 WHEN OTHER   MOVE "N" TO BOX1-OPEN-FG
                              GO TO A100-OPEN
              END-EVALUATE
              MOVE "N" TO BOX1-OPEN-FG
              GO TO A100-OPEN.

       CONVERT-CONTINUE.
           IF INPUT-FILE = "*"
             PERFORM CREATE-LIST-BY-DIR
             IF FILE-STAT = "++"
                IF PROCESS-ALL
                   GO TO PROCESS-NEXT-DIR
                ELSE
                   GO TO  EXIT-PROGRAM.

           IF NOT WINDOW1-OPEN
              DISPLAY WINDOW
                      AT 014002
                      LINES 9
                      SIZE 78
                      BOXED
                      ERASE
                      POP-UP AREA IS SCRN1-POP-UP-WINDOW2
              END-DISPLAY
              DISPLAY "GENERATING SCREEN DEFINITION..."
              DISPLAY " "
              MOVE "Y" TO WINDOW1-OPEN-FG.

       MULTIPLE-FORM-LOOP.
           PERFORM SET-PATHS.

              OPEN INPUT  IN-FILE.
           IF NOT REPORT-ONLY
              OPEN OUTPUT SCN-FILE  
              WRITE SCN-REC FROM COMMENT-DASH
              WRITE SCN-REC FROM DISC-LINE1
              WRITE SCN-REC FROM DISC-LINE2
              WRITE SCN-REC FROM DISC-LINE3
              WRITE SCN-REC FROM DISC-LINE4
              WRITE SCN-REC FROM COMMENT-DASH
              OPEN OUTPUT WKS-FILE  
              OPEN OUTPUT WKS-FILE2  
              WRITE WKS-REC FROM COMMENT-DASH
              WRITE WKS-REC FROM DISC-LINE1
              WRITE WKS-REC FROM DISC-LINE2
              WRITE WKS-REC FROM DISC-LINE3
              WRITE WKS-REC FROM DISC-LINE4
              WRITE WKS-REC FROM COMMENT-DASH
              MOVE SPACES TO HELP-78-NAME HELP-78-VALUE
              STRING FORM-NAME "-HELP--FILE"
                     DELIMITED BY " "
                     INTO HELP-78-NAME
              STRING WS-QUOTE FORM-NAME
                     WS-QUOTE WS-PERIOD
                     DELIMITED BY " "
                     INTO HELP-78-VALUE
              WRITE WKS-REC FROM HELP-78-LINE
              MOVE SPACES TO HELP-78-NAME HELP-78-VALUE
              STRING FORM-NAME "-HELP--DIR"
                     DELIMITED BY " "
                     INTO HELP-78-NAME
              STRING WS-QUOTE INPUT-DIR
                     WS-QUOTE WS-PERIOD
                     DELIMITED BY " "
                     INTO HELP-78-VALUE
              WRITE WKS-REC FROM HELP-78-LINE
      * 01 LEVEL FOR .WKS FILE
              MOVE SPACES TO WS-FORM-FIELD0.
              STRING FORM-NAME "-FORM-FIELDS."
                  DELIMITED BY " "
                  INTO WS-FORM-FIELD0
              WRITE WKS-REC FROM D-FIELD-LINE-0

      * 01 LEVEL FOR .WKS3 FILE
              MOVE SPACES TO WS-FORM-FIELD0.
              STRING FORM-NAME "-FORM-LIST."
                  DELIMITED BY " "
                  INTO WS-FORM-FIELD0
              WRITE WKS-REC2 FROM D-FIELD-LINE-0

              OPEN OUTPUT EVA-FILE  
              WRITE EVA-REC FROM COMMENT-DASH
              WRITE EVA-REC FROM DISC-LINE1
              WRITE EVA-REC FROM DISC-LINE2
              WRITE EVA-REC FROM DISC-LINE3
              WRITE EVA-REC FROM DISC-LINE4
              WRITE EVA-REC FROM COMMENT-DASH
              OPEN OUTPUT DEF-FILE
              WRITE DEF-REC FROM COMMENT-DASH
              WRITE DEF-REC FROM DISC-LINE1
              WRITE DEF-REC FROM DISC-LINE2
              WRITE DEF-REC FROM DISC-LINE3
              WRITE DEF-REC FROM DISC-LINE4
              WRITE DEF-REC FROM COMMENT-DASH.

           OPEN OUTPUT NDS-FILE.
           OPEN OUTPUT ERR-FILE.
           MOVE "Y" TO FILES-OPENED.

           MOVE "N" TO EL-EXISTS.

           MOVE SPACES TO L1-TITLE.
           STRING FORM-NAME "-SCREEN." DELIMITED BY " " INTO L1-TITLE.
           IF NOT REPORT-ONLY
              WRITE SCN-REC FROM LEVEL-ONE.

      *    STATUS UPDATE
           MOVE SPACES TO MESS.
           IF REPORT-ONLY
              STRING "CREATING REPORTS FOR " INPUT-DIR "/" INPUT-FILE
                   DELIMITED BY SIZE INTO MESS
           ELSE
              STRING "CREATING FILES FOR " INPUT-DIR "/" INPUT-FILE
                   DELIMITED BY SIZE INTO MESS.
           DISPLAY MESS.

       A100-READ.

           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO A100-CONTINUE.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.

           EVALUATE IN-REC(1:1)
              WHEN LOWER-P PERFORM BUILD-PROMPT
              WHEN LOWER-F PERFORM BUILD-FIELD
              WHEN OTHER CONTINUE
           END-EVALUATE.

       A100-CONTINUE.
           IF INFILE-STAT = "00"
              GO TO A100-READ.

      *   PROCESSING SINGLE FORM OK TO EXIT HERE WE ARE DONE
           IF PROCESS-FULL-DIR NOT = "Y"
              MOVE "D" TO PROCESS-ALL-FG
              GO TO EXIT-PROGRAM.

           PERFORM CLOSE-FILES.

        A100-READ-NEXT.
           MOVE SPACES TO LIST-REC.
           READ LIST-FILE1 NEXT AT END PERFORM EXIT-PROGRAM
                                       GO TO PROCESS-NEXT-DIR.
           IF FILE-STAT NOT = "00"
              GO TO EXIT-PROGRAM.

           MOVE LIST-REC TO LIST-REC-WORKER
           IF FILE-STAT = "00" AND LRW-JUNK NOT = SPACES
              GO TO A100-READ-NEXT.

           MOVE LRW-FORM-NAME TO INPUT-FILE FORM-NAME.
           GO TO MULTIPLE-FORM-LOOP.


      ******************************************************************
      * READ IN INFO FOR PROMPT
      ******************************************************************
       BUILD-PROMPT SECTION.

      *    -------------------------------------------------------------
      *    READ PROMPT TEXT
      *    -------------------------------------------------------------
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-PROMPT-EXIT.

      * REPLACE | "| WITH |''|
           INSPECT IN-REC REPLACING ALL X'2220' BY X'2727'.
      * REPLACE |" | WITH |''|
           INSPECT IN-REC REPLACING ALL X'2022' BY X'2727'.
      * REPLACE |"| WITH |'|
           INSPECT IN-REC REPLACING ALL X'22' BY X'27'.
           MOVE SPACES TO L3-VALUE
                          L3B-VALUE.
           MOVE 0 TO SUB.
           INSPECT IN-REC TALLYING SUB FOR TRAILING " ".
           SUBTRACT SUB FROM 80 GIVING MSG1-LENGTH.
           IF MSG1-LENGTH = 0
              MOVE 1 TO MSG1-LENGTH.
           IF MSG1-LENGTH > 40
              COMPUTE MSG1-REMAIN = MSG1-LENGTH - 40
              IF MSG1-LENGTH < 1
                 CONTINUE
              END-IF
              IF MSG1-REMAIN < 1
                 CONTINUE
              END-IF
              STRING '"' IN-REC(1:40) '"'
                      DELIMITED BY SIZE INTO L3-VALUE
              STRING '"' IN-REC(41:MSG1-REMAIN) '"'
                      DELIMITED BY SIZE INTO L3B-VALUE
           ELSE
              STRING '"' IN-REC(1:MSG1-LENGTH) '"'
                      DELIMITED BY SIZE INTO L3-VALUE.

      *    -------------------------------------------------------------
      *    READ PROMPT LINE COORDINATE
      *    -------------------------------------------------------------
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-PROMPT-EXIT.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.

           IF IN-REC(1:1) NOT NUMERIC
                   GO TO BUILD-PROMPT-EXIT
           END-IF.

           IF IN-REC(2:1) = " "
                   MOVE "0"         TO L3-LINE-NO(1:1)
                   MOVE IN-REC(1:1) TO L3-LINE-NO(2:1)
                                       MSG1-ROWX
           ELSE
                   MOVE IN-REC(1:2) TO L3-LINE-NO
                                       MSG1-ROWX
           END-IF.

      *    -------------------------------------------------------------
      *    READ PROMPT COLUMN COORDINATE
      *    -------------------------------------------------------------
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-PROMPT-EXIT.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.

           IF IN-REC(1:1) NOT NUMERIC
                   GO TO BUILD-PROMPT-EXIT
           END-IF.

           IF IN-REC(2:1) = " "
                   MOVE "0"         TO L3-COL-NO(1:1)
                   MOVE IN-REC(1:1) TO L3-COL-NO(2:1)
                                       MSG1-COLX
           ELSE
                   MOVE IN-REC(1:2) TO L3-COL-NO
                                       MSG1-COLX
           END-IF.
      * CORRECT FOR LEFT JUSTIFIED SINGLE NUMERICS
           EVALUATE MSG1-COLX
             WHEN "1 "  MOVE "01" TO MSG1-COLX
             WHEN "2 "  MOVE "02" TO MSG1-COLX
             WHEN "3 "  MOVE "03" TO MSG1-COLX
             WHEN "4 "  MOVE "04" TO MSG1-COLX
             WHEN "5 "  MOVE "05" TO MSG1-COLX
             WHEN "6 "  MOVE "06" TO MSG1-COLX
             WHEN "7 "  MOVE "07" TO MSG1-COLX
             WHEN "8 "  MOVE "08" TO MSG1-COLX
             WHEN "9 "  MOVE "09" TO MSG1-COLX
             WHEN "0 "  MOVE "00" TO MSG1-COLX
           END-EVALUATE.

      *  NEED TO SHIFT EVERYTHING OVER ONE FOR SPECIFIC FORMS TO MAKE THEM
      * FIT IN TO THE WINDOW FRAME WITHOUT TRUNCATING FIELDS.
           EVALUATE FORM-NAME
              WHEN "BWCREA" SUBTRACT 1 FROM MSG1-COL GIVING MSG1-COL
              WHEN "CHBORR" SUBTRACT 1 FROM MSG1-COL GIVING MSG1-COL
              WHEN "MODBOR" SUBTRACT 1 FROM MSG1-COL GIVING MSG1-COL
           END-EVALUATE.
           MOVE MSG1-COL TO L3-COL-NO.
      *    -------------------------------------------------------------
      *    READ PROMPT DISPLAY ATTRIBUTE  (BOLD?)
      *    -------------------------------------------------------------
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-PROMPT-EXIT.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.

           IF IN-REC(1:1) = LOWER-N
                   MOVE DISP-BOLD        TO L3-ATTRIBUTE
           ELSE
                   MOVE DISP-LOW         TO L3-ATTRIBUTE
           END-IF.

           IF NOT REPORT-ONLY
              WRITE SCN-REC FROM LEVEL-THREE
              WRITE SCN-REC FROM LEVEL-THREE2
              WRITE SCN-REC FROM LEVEL-THREE-B
              IF LEVEL-THREE2B NOT = SPACES
                 ADD 40 TO MSG1-COL
                 MOVE MSG1-COL TO L3-COL-NO
                 WRITE SCN-REC FROM LEVEL-THREE
                 WRITE SCN-REC FROM LEVEL-THREE2B
                 WRITE SCN-REC FROM LEVEL-THREE-B.

       BUILD-PROMPT-EXIT.
           EXIT.

      ******************************************************************
      * READ FIELD INPUT INFO
      ******************************************************************
       BUILD-FIELD SECTION.
           INITIALIZE FIELD-WORKERS.

      *    -------------------------------------------------------------
      *    READ FIELD TEXT
      *    -------------------------------------------------------------
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:15) TO FIELD-FIELD.
           CALL "C$TOUPPER" USING FIELD-FIELD, VALUE 15.

      *    -------------------------------------------------------------
      *    IF FIELD NAME IS "BR#" CONVERT IT TO "BRNUM"
      *    THIS OCCURS IN LPMENU (LIBGB/MENU) AND ?????
      *    -------------------------------------------------------------
           IF FIELD-FIELD-BRHASH
              MOVE "BRNUM" TO FIELD-FIELD.

           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           MOVE IN-REC(1:60) TO FIELD-VALUE.

           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:15) TO FIELD-PIC.
           CALL "C$TOUPPER" USING FIELD-PIC, VALUE 15.
           MOVE SPACES TO EL-CHG.

           EVALUATE TRUE
      * ALPHA FIELDS
             WHEN FIELD-PIC = "N(01)             " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(02)             " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(03)             " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(04)             " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(05)             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(06)             " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(07)             " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(08)             " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(09)             " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(1)              " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(2)              " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(3)              " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(4)              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(5)              " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(6)              " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(7)              " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(8)              " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(9)              " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(10)             " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(11)             " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(12)             " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(13)             " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(14)             " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(15)             " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(16)             " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(17)             " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(18)             " MOVE 18 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(19)             " MOVE 19 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(20)             " MOVE 20 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(21)             " MOVE 21 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(22)             " MOVE 22 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(23)             " MOVE 23 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(24)             " MOVE 24 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(25)             " MOVE 25 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(26)             " MOVE 26 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(27)             " MOVE 27 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(28)             " MOVE 28 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(29)             " MOVE 29 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(30)             " MOVE 30 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(31)             " MOVE 31 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(32)             " MOVE 32 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(33)             " MOVE 33 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(34)             " MOVE 34 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(35)             " MOVE 35 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(36)             " MOVE 36 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(37)             " MOVE 37 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(38)             " MOVE 38 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(39)             " MOVE 39 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(40)             " MOVE 40 TO FIELD-SIZE
             WHEN FIELD-PIC = "N((40)            " MOVE 40 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(41)             " MOVE 41 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(42)             " MOVE 42 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(43)             " MOVE 43 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(44)             " MOVE 44 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(45)             " MOVE 45 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(46)             " MOVE 46 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(47)             " MOVE 47 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(48)             " MOVE 48 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(49)             " MOVE 49 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(50)             " MOVE 50 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(51)             " MOVE 51 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(52)             " MOVE 52 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(53)             " MOVE 53 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(54)             " MOVE 54 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(55)             " MOVE 55 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(56)             " MOVE 56 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(57)             " MOVE 57 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(58)             " MOVE 58 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(59)             " MOVE 59 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(60)             " MOVE 60 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(61)             " MOVE 61 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(62)             " MOVE 62 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(63)             " MOVE 63 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(64)             " MOVE 64 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(65)             " MOVE 65 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(66)             " MOVE 66 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(67)             " MOVE 67 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(68)             " MOVE 68 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(69)             " MOVE 69 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(70)             " MOVE 70 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(71)             " MOVE 71 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(72)             " MOVE 72 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(73)             " MOVE 73 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(74)             " MOVE 74 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(75)             " MOVE 75 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(76)             " MOVE 76 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(77)             " MOVE 77 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(78)             " MOVE 78 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(79)             " MOVE 79 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(80)             " MOVE 80 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(93)             " MOVE 93 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(100)           " MOVE 100 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(110)           " MOVE 110 TO FIELD-SIZE
             WHEN FIELD-PIC = "N(127)           " MOVE 127 TO FIELD-SIZE

             WHEN FIELD-PIC = "N                 " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "NN                " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNN               " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNN              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNN             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNN            " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNN           " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNN          " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNN         " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNN        " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNN       " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNNN      " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNNNN     " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNNNNN    " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNNNNNN   " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNNNNNNN  " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNNNNNNNN " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "NNNNNNNNNNNNNNNNNN" MOVE 18 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(01)             " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(02)             " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(03)             " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(04)             " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(05)             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(06)             " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(07)             " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(08)             " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(09)             " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(1)              " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(2)              " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(3)              " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(4)              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(5)              " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(6)              " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(7)              " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(8)              " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(9)              " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(10)             " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(11)             " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(12)             " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(13)             " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(14)             " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(15)             " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(16)             " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(17)             " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(18)             " MOVE 18 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(19)             " MOVE 19 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(20)             " MOVE 20 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(21)             " MOVE 21 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(22)             " MOVE 22 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(23)             " MOVE 23 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(24)             " MOVE 24 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(25)             " MOVE 25 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(26)             " MOVE 26 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(27)             " MOVE 27 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(28)             " MOVE 28 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(29)             " MOVE 29 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(30)             " MOVE 30 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(31)             " MOVE 31 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(32)             " MOVE 32 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(33)             " MOVE 33 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(34)             " MOVE 34 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(35)             " MOVE 35 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(36)             " MOVE 36 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(37)             " MOVE 37 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(38)             " MOVE 38 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(39)             " MOVE 39 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(40)             " MOVE 40 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(41)             " MOVE 41 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(42)             " MOVE 42 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(43)             " MOVE 43 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(44)             " MOVE 44 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(45)             " MOVE 45 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(46)             " MOVE 46 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(47)             " MOVE 47 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(48)             " MOVE 48 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(49)             " MOVE 49 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(50)             " MOVE 50 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(51)             " MOVE 51 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(52)             " MOVE 52 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(53)             " MOVE 53 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(54)             " MOVE 54 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(55)             " MOVE 55 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(56)             " MOVE 56 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(57)             " MOVE 57 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(58)             " MOVE 58 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(59)             " MOVE 59 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(60)             " MOVE 60 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(61)             " MOVE 61 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(62)             " MOVE 62 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(63)             " MOVE 63 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(64)             " MOVE 64 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(65)             " MOVE 65 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(66)             " MOVE 66 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(67)             " MOVE 67 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(68)             " MOVE 68 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(69)             " MOVE 69 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(70)             " MOVE 70 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(71)             " MOVE 71 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(72)             " MOVE 72 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(73)             " MOVE 73 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(74)             " MOVE 74 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(75)             " MOVE 75 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(76)             " MOVE 76 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(77)             " MOVE 77 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(78)             " MOVE 78 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(79)             " MOVE 79 TO FIELD-SIZE
             WHEN FIELD-PIC = "X(80)             " MOVE 80 TO FIELD-SIZE

             WHEN FIELD-PIC = "X                 " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "XX                " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXX               " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXX              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXX             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXX            " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXX           " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXX          " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXX         " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXX        " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXX       " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXXX      " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXXXX     " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXXXXX    " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXXXXXX   " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXXXXXXX  " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXXXXXXXX " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "XXXXXXXXXXXXXXXXXX" MOVE 18 TO FIELD-SIZE
      * NUMERIC FIELDS
             WHEN FIELD-PIC = "9(01)             " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(02)             " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(03)             " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(04)             " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(05)             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(06)             " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(07)             " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(08)             " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(09)             " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(1)              " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(2)              " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(3)              " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(4)              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(5)              " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(6)              " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(7)              " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(8)              " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(9)              " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(10)             " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(11)             " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(12)             " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(13)             " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(14)             " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(15)             " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(16)             " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(17)             " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(18)             " MOVE 18 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(19)             " MOVE 19 TO FIELD-SIZE
             WHEN FIELD-PIC = "9(20)             " MOVE 20 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(01)             " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(02)             " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(03)             " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(04)             " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(05)             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(06)             " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(07)             " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(08)             " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(09)             " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(1)              " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(2)              " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(3)              " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(4)              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(5)              " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(6)              " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(7)              " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(8)              " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(9)              " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(10)             " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(11)             " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(12)             " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(13)             " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(14)             " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(15)             " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(16)             " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(17)             " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(18)             " MOVE 18 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(19)             " MOVE 19 TO FIELD-SIZE
             WHEN FIELD-PIC = "D(20)             " MOVE 20 TO FIELD-SIZE

             WHEN FIELD-PIC = "9                 " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "99                " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "999               " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "9999              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "99999             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "999999            " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "9999999           " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "99999999          " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "999999999         " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "9999999999        " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "99999999999       " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "999999999999      " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "9999999999999     " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "99999999999999    " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "999999999999999   " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "9999999999999999  " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "99999999999999999 " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "999999999999999999" MOVE 18 TO FIELD-SIZE
             WHEN FIELD-PIC = "D                 " MOVE  1 TO FIELD-SIZE
             WHEN FIELD-PIC = "DD                " MOVE  2 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDD               " MOVE  3 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDD              " MOVE  4 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDD             " MOVE  5 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDD            " MOVE  6 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDD           " MOVE  7 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDD          " MOVE  8 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDD         " MOVE  9 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDD        " MOVE 10 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDD       " MOVE 11 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDDD      " MOVE 12 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDDDD     " MOVE 13 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDDDDD    " MOVE 14 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDDDDDD   " MOVE 15 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDDDDDDD  " MOVE 16 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDDDDDDDD " MOVE 17 TO FIELD-SIZE
             WHEN FIELD-PIC = "DDDDDDDDDDDDDDDDDD" MOVE 18 TO FIELD-SIZE

             WHEN FIELD-PIC = "DDDDDDDD-DD       " MOVE 11 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(11)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "DDD-DD-DDDD       " MOVE 11 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(11)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "DDD-DDD-DDDD      " MOVE 12 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(12)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "D(3)-D(2)-D(4)    " MOVE 11 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(11)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "D(03)-D(02)-D(04) " MOVE 11 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(11)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "D(04)-D(07)       " MOVE 12 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(12)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "D(4)-D(7)         " MOVE 12 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(12)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "DD/DD/DD          " MOVE  8 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(08)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "(DDD)-DDD-DDDD    " MOVE 14 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(14)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "DDDD-             " MOVE  5 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(05)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "DD:DD             " MOVE  5 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(05)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "DD/DD             " MOVE  5 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(05)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "D(8)-D(2)         " MOVE 11 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(11)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "D(6)-D(2)         " MOVE  9 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(09)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "DD:DD:DD          " MOVE  8 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(08)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "9(6).99           " MOVE  9 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                               MOVE "9(06)V99" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "99/99/99          " MOVE  8 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(08)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "NN/NN/NN          " MOVE  8 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(08)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "NNN-NN-NNNN       " MOVE 11 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(11)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "(NNN)-NNN-NNNN    " MOVE 14 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(14)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "NNNN-NNNNNNN      " MOVE 12 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(12)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "NN-N(4)           " MOVE  7 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(07)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "NN(8)             " MOVE  9 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(09)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "N(2)/N(2)         " MOVE  5 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(05)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "N(4)-N(7)         " MOVE 12 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(12)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "N(6)-N(2)         " MOVE  9 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(09)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "N/N/N             " MOVE  5 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(05)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "NN/NN             " MOVE  5 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(05)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "XX-X(4)           " MOVE  7 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "D(07)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN FIELD-PIC = "A                 " MOVE  1 TO FIELD-SIZE
                                                  MOVE "1" TO EL-ERROR
                                                  MOVE "X(01)" TO EL-CHG
                                                  PERFORM WRITE-ERR-LOG
             WHEN OTHER                            MOVE "2" TO EL-ERROR
                                                   PERFORM WRITE-ERR-LOG
           END-EVALUATE.

             IF FIELD-PIC(1:1) = "D"
                MOVE "9" TO FIELD-ALP-NUM
             ELSE
                MOVE "X" TO FIELD-ALP-NUM.

           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.
           IF IN-REC(2:1) = " "
                   MOVE "0"         TO FIELD-LINEX(1:1)
                   MOVE IN-REC(1:1) TO FIELD-LINEX(2:1)
           ELSE
                   MOVE IN-REC(1:2) TO FIELD-LINEX(1:2)
           END-IF.

           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOLOWER" USING IN-REC, VALUE 80.
           IF IN-REC(2:1) = " "
                   MOVE "0"         TO FIELD-COLX(1:1)
                   MOVE IN-REC(1:1) TO FIELD-COLX(2:1)
           ELSE
                   MOVE IN-REC(1:2) TO FIELD-COLX(1:2)
           END-IF.

      *  NEED TO SHIFT EVERYTHING OVER ONE FOR SPECIFIC FORMS TO MAKE THEM
      * FIT IN TO THE WINDOW FRAME WITHOUT TRUNCATING FIELDS.
           EVALUATE FORM-NAME
              WHEN "BWCREA" SUBTRACT 1 FROM FIELD-COL GIVING FIELD-COL
              WHEN "CHBORR" SUBTRACT 1 FROM FIELD-COL GIVING FIELD-COL
              WHEN "MODBOR" SUBTRACT 1 FROM FIELD-COL GIVING FIELD-COL
           END-EVALUATE.

           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(1:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(2:1).
      * BOLD
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
      * IF A SCAN WINDOW MAKE REVERSE IN THE ARIBUTES.  'R' IN BOLD WILL DENOTE
      *  REVERSE TO SCREEN.CPY
           IF (FORM-NAME-SCAN AND FIELD-FIELD-REV) OR
              (FORM-NAME-SCAN2 AND FIELD-FIELD-REV)
              MOVE "R" TO FIELD-ATTRIB(3:1)
           ELSE
              MOVE IN-REC(1:1) TO FIELD-ATTRIB(3:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(4:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(5:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(6:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(7:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(8:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(9:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(10:1).
           PERFORM READ-INPUT-FILE.
           IF INFILE-STAT NOT = "00"
              GO TO BUILD-FIELD-EXIT.
           CALL "C$TOUPPER" USING IN-REC, VALUE 80.
           MOVE IN-REC(1:1) TO FIELD-ATTRIB(11:1).

      *    -------------------------------------------------------------
      *    CONVERT FIELDS TO UPPERCASE (EXCEPT VALUE FIELD).
      *    -------------------------------------------------------------
      *    MOVE FIELD-FIELD TO TEXT-DATA.
      *    CALL "C$TOUPPER" USING TEXT-DATA, VALUE TEXT-LEN.
      *    MOVE TEXT-DATA TO FIELD-FIELD.

           MOVE FIELD-PIC TO TEXT-DATA.
           CALL "C$TOUPPER" USING TEXT-DATA, VALUE TEXT-LEN.
           MOVE TEXT-DATA TO FIELD-PIC.

           IF EL-CHG NOT = SPACES MOVE EL-CHG TO FIELD-PIC.
           MOVE SPACES TO EL-CHG.

      *   MAKINF ALL FIELDS ALPHA TIL WE CAN FIGURE IT OUT MORE.
      *   AT THIS POINT WE ARE NOT USING THEM FOR INPUT ANYWAY.
           IF NOT FIELD-PIC-FOR-9
              INSPECT FIELD-PIC REPLACING ALL "9" BY "X".
           IF FIELD-PIC-FOR-9B
              EVALUATE FIELD-PIC
                  WHEN "9(09)" MOVE "X(09)" TO FIELD-PIC
                  WHEN "9(9)"  MOVE  "X(9)" TO FIELD-PIC
                  WHEN "9(19)" MOVE "X(19)" TO FIELD-PIC
                  WHEN "9(29)" MOVE "X(29)" TO FIELD-PIC
                  WHEN "9(39)" MOVE "X(39)" TO FIELD-PIC
                  WHEN "9(49)" MOVE "X(49)" TO FIELD-PIC
                  WHEN "9(59)" MOVE "X(59)" TO FIELD-PIC
                  WHEN "9(69)" MOVE "X(69)" TO FIELD-PIC
                  WHEN "9(79)" MOVE "X(79)" TO FIELD-PIC
               END-EVALUATE.

           INSPECT FIELD-PIC REPLACING ALL "N" BY "X".
           INSPECT FIELD-PIC REPLACING ALL "D" BY "X".
           INSPECT FIELD-PIC REPLACING ALL ":" BY "X".
           INSPECT FIELD-PIC REPLACING ALL "/" BY "X".
           INSPECT FIELD-PIC REPLACING ALL "-" BY "X".

           MOVE FIELD-ATTRIB TO TEXT-DATA.
      *    CALL "C$TOUPPER" USING TEXT-DATA, VALUE TEXT-LEN.
           MOVE TEXT-DATA TO FIELD-ATTRIB.

      *    -------------------------------------------------------------
      *    SETUP OUTPUT LINE FOR WORKING STORAGE AND SCREEN SECTION
      *    -------------------------------------------------------------
           MOVE SPACES TO SCR-FLD-NAME
                          L3-FIELD
                          WS-FORM-FIELD
                          WS-FLD-PICTURE
                          WS-FL-1B-PIC
                          WS-FL-1B-VALUE.


           STRING FORM-NAME "-" FIELD-FIELD
               DELIMITED BY " "
               INTO L3-FIELD.

      * 03 LEVEL FOR .WKS FILE
           STRING FORM-NAME "-" FIELD-FIELD
               DELIMITED BY " "
               INTO WS-FORM-FIELD.

           MOVE 0 TO SUB.
           INSPECT FIELD-FIELD TALLYING SUB FOR TRAILING " ".
           SUBTRACT SUB FROM 15 GIVING MSG1-LENGTH.
           IF MSG1-LENGTH = 0
              MOVE 1 TO MSG1-LENGTH.
           STRING FIELD-FIELD(1:MSG1-LENGTH) '#".'
                   DELIMITED BY SIZE INTO WS-FL-1B-VALUE.
           INSPECT WS-FL-1B-VALUE REPLACING ALL "#" BY " ".
           ADD 1 MSG1-LENGTH GIVING WS-FL-1B-PIC.

           STRING FIELD-FIELD WS-PERIOD
                  DELIMITED BY " " INTO SCR-FLD-NAME.

           STRING FIELD-PIC
                  DELIMITED BY " " INTO WS-FLD-PICTURE.

           MOVE FIELD-LINE    TO SCRN-LINE.
           MOVE FIELD-COL     TO SCRN-COL.

           MOVE SPACES TO WS-FLD-VALUE.
           IF  FORM-NAME = "LMMAIN" AND FIELD-FIELD = "020"
               STRING " ALL " WS-QUOTE "-" WS-QUOTE "."
               DELIMITED BY SIZE INTO WS-FLD-VALUE
           ELSE
           IF FIELD-VALUE = SPACES OR "`"
              MOVE "SPACES." TO WS-FLD-VALUE
           ELSE
              MOVE 0 TO SUB
              MOVE SPACES TO WS-FLD-VALUE
              INSPECT FIELD-VALUE TALLYING SUB FOR TRAILING " "
              SUBTRACT SUB FROM 60 GIVING MSG1-LENGTH
              IF MSG1-LENGTH = 0
                 MOVE 1 TO MSG1-LENGTH
              END-IF
                 IF FIELD-VALUE(1:1) = "_"
                     MOVE "SPACES." TO WS-FLD-VALUE
                 ELSE
                    STRING WS-QUOTE FIELD-VALUE(1:MSG1-LENGTH)
                           WS-QUOTE WS-PERIOD
                           DELIMITED BY SIZE INTO WS-FLD-VALUE
                 END-IF
      *       FORCE QUEUE TO BE "V" FOR GET PRU
              IF FIELD-FIELD = "QUEUE"
                 IF NOT FIELD-IS-NONDISPLAY
                    IF FIELD-VALUE = "Y"
                       STRING WS-QUOTE "V" WS-QUOTE WS-PERIOD
                              DELIMITED BY SIZE INTO WS-FLD-VALUE.

           MOVE SPACES TO D-ND-LINE-1.
           STRING INPUT-DIR " " FORM-NAME " " FIELD-FIELD " "
                            WS-FLD-PICTURE " " WS-FLD-VALUE
                  DELIMITED BY SIZE
                  INTO D-ND-LINE-1.

           MOVE SPACE TO L3-COMMENT2.


           IF FIELD-IS-NONDISPLAY
              GO TO SKIP-NONDISPLAY-FIELDS.

      *    -------------------------------------------------------------
      *    PRINT DASHED LINE TO SEPARATE FIELDS.
      *    -------------------------------------------------------------
           MOVE SPACES TO D-COMMENT.
           INSPECT D-COMMENT REPLACING ALL " " BY "-".
           IF NOT REPORT-ONLY
              WRITE SCN-REC FROM D-COMMENT-LINE.

      *    -------------------------------------------------------------
      *    WRITE FIRST FOUR LINES OF FIELD IN SCREEN SECTION
      *    -------------------------------------------------------------
           IF NOT REPORT-ONLY
      *       WRITE SCN-REC FROM SCR-FLD-LINE-1
              WRITE SCN-REC FROM LEVEL-THREE-ITEM
              WRITE SCN-REC FROM SCRN-LINES
              WRITE SCN-REC FROM SCRN-COLUMNS
              WRITE SCN-REC FROM SCRN-CHARACTER
      *    -------------------------------------------------------------
      *    SETUP FIELD ATTRIBUTES
      *    -------------------------------------------------------------
              IF FIELD-RJUS = "Y"
                 WRITE SCN-REC FROM SCRN-RJUS
              ELSE
                 WRITE SCN-REC FROM SCRN-LJUS
              END-IF
              IF FIELD-ZFIL = "Y"
                 WRITE SCN-REC FROM SCRN-ZFIL
              END-IF
              IF FIELD-AUTO = "Y"
                 WRITE SCN-REC FROM SCRN-AUTO
              END-IF
              IF FIELD-MUST = "Y"
                 WRITE SCN-REC FROM SCRN-REQD
              END-IF
      *    -------------------------------------------------------------
      *    CONVERT ALL CHARACTER ENTRY TO UPPER CASE.
      *    -------------------------------------------------------------
              WRITE SCN-REC FROM SCRN-UPPER
      *    -------------------------------------------------------------
      *    DO NOT DISPLAY A BOX AROUND THE ENTRY-FIELD
      *    -------------------------------------------------------------
              WRITE SCN-REC FROM SCRN-NO-BOX
      *    -------------------------------------------------------------
      *    DISPLAY ALL INPUT FIELDS IN HIGH INTENSITY,
      *    ADDED LOW INTENSITY OPTION, SOME FIELDS ARE PROMPTS THAT
      *        NEED TO BE LOW INTENSITY.
      *    IF ITS A SCAN WINDOW MAKE TITLE AND WSEL/LEGEND BOLD AND REVERSE
      *    -------------------------------------------------------------
           IF (FORM-NAME-SCAN AND FIELD-FIELD-REV) OR
              (FORM-NAME-SCAN2 AND FIELD-FIELD-REV)
                 WRITE SCN-REC FROM SCRN-BOLD-REV
              ELSE
                 IF FIELD-BOLD = "Y"
                    WRITE SCN-REC FROM SCRN-BOLD
                 ELSE
                    WRITE SCN-REC FROM SCRN-LOW
                 END-IF
              END-IF
               
      *    -------------------------------------------------------------
      *    WRITE OUTPUT FOR WORKING STOREAGE FIELDS.
      *    -------------------------------------------------------------
              WRITE WKS-REC FROM D-FIELD-LINE-1
              WRITE WKS-REC FROM D-FIELD-LINE-2
              WRITE WKS-REC2 FROM D-FIELD-LINE-1B.

      *    -------------------------------------------------------------
      *    CREATE *.DEF LINE 1
      *    -------------------------------------------------------------
           MOVE SPACES TO FLD-88-NAME FLD-88-VALUE.
           STRING FORM-NAME "-" FIELD-FIELD DISP--88
              DELIMITED BY " " INTO FLD-88-NAME.

           MOVE 0 TO SUB.
           INSPECT FIELD-FIELD TALLYING SUB FOR TRAILING " ".
           SUBTRACT SUB FROM 15 GIVING MSG1-LENGTH.
           IF MSG1-LENGTH = 0
              MOVE 1 TO MSG1-LENGTH.

                 STRING '"' FIELD-FIELD(1:MSG1-LENGTH) '.".'
                         DELIMITED BY SIZE INTO FLD-88-VALUE.

      *    STRING '"' FORM-NAME "-" FIELD-FIELD '.".'
      *            DELIMITED BY " " INTO FLD-88-VALUE.

           IF NOT REPORT-ONLY
              WRITE DEF-REC FROM FLD-88-LINE.

      *    -------------------------------------------------------------
      *    CREATE *.DEF LINE 2
      *    -------------------------------------------------------------
           MOVE SPACES TO LC-78-NAME.
           STRING FORM-NAME "-" FIELD-FIELD DISP--POS
              DELIMITED BY " " INTO LC-78-NAME.
           MOVE FIELD-LINE TO LC-78-LIN.
           MOVE FIELD-COL  TO LC-78-COL.
           IF NOT REPORT-ONLY
              WRITE DEF-REC FROM LC-78-LINE.

      *    -------------------------------------------------------------
      *    CREATE *.DEF LINE 3
      *    -------------------------------------------------------------
           MOVE SPACES TO SIZE-78-NAME.
           STRING FORM-NAME "-" FIELD-FIELD DISP--SIZE
              DELIMITED BY " " INTO SIZE-78-NAME.
           MOVE FIELD-SIZE TO SIZE-78-VALUE.
           IF NOT REPORT-ONLY
              WRITE DEF-REC FROM SIZE-78-LINE.

      *    -------------------------------------------------------------
      *    CREATE *.DEF LINE 4 FORM ATTRIBUTES
      *    -------------------------------------------------------------
           MOVE SPACES TO FLD-78-NAME FLD-78-VALUE.
           STRING FORM-NAME "-" FIELD-FIELD DISP--ATR
              DELIMITED BY " " INTO FLD-78-NAME.

           MOVE 0 TO SUB.
           INSPECT FIELD-ATTRIB TALLYING SUB FOR TRAILING " ".
           SUBTRACT SUB FROM 11 GIVING MSG1-LENGTH.
           IF MSG1-LENGTH = 0
              MOVE 1 TO MSG1-LENGTH.
           STRING '"' FIELD-ATTRIB(1:MSG1-LENGTH) '".'
                   DELIMITED BY SIZE INTO FLD-78-VALUE.

           IF NOT REPORT-ONLY
              WRITE DEF-REC FROM FLD-78-LINE.

      *    -------------------------------------------------------------
      *    CREATE *.EVA LINE 1  (WHEN ###### )
      *    -------------------------------------------------------------
           MOVE "WHEN " TO EVAL-WHEN.
           MOVE SPACES TO EVAL-NAME EVAL-SOURCE.
           STRING 'VDU-FORM-NAME#=#"' FORM-NAME '"#AND#'
                  FORM-NAME "-" FIELD-FIELD DISP--88
              DELIMITED BY " " INTO EVAL-NAME.
           INSPECT EVAL-NAME REPLACING ALL "#" BY " ".
           IF NOT REPORT-ONLY
              WRITE EVA-REC FROM EVAL-LINE.

      *    -------------------------------------------------------------
      *    CREATE *.EVA LINE 2  (MOVE ###### TO VDU-POS)
      *    -------------------------------------------------------------
           MOVE SPACES TO EVAL-SOURCE.
           MOVE "MOVE " TO EVAL-MOVE.
           STRING FORM-NAME "-" FIELD-FIELD DISP--POS
              DELIMITED BY " " INTO EVAL-SOURCE.
           MOVE DISP-VDU-POS TO EVAL-DEST.
           IF NOT REPORT-ONLY
              WRITE EVA-REC FROM EVAL-LINE1.

      *    -------------------------------------------------------------
      *    CREATE *.EVA LINE 3  (MOVE ### TO VDU-SIZEX)
      *    -------------------------------------------------------------
           MOVE SPACES TO EVAL-SOURCE.
           MOVE "MOVE " TO EVAL-MOVE.
           STRING FORM-NAME "-" FIELD-FIELD DISP--SIZE
              DELIMITED BY " " INTO EVAL-SOURCE.
           MOVE DISP-VDU-SIZE TO EVAL-DEST.
           IF NOT REPORT-ONLY
              WRITE EVA-REC FROM EVAL-LINE1.

      *    -------------------------------------------------------------
      *    CREATE *.EVA LINE 4  (MOVE FIELD-ATTRIB TO VDU-ATRX)
      *    -------------------------------------------------------------
           MOVE SPACES TO EVAL-SOURCE.
           MOVE "MOVE " TO EVAL-MOVE.
      *--->STRING '"' FIELD-ATTRIB '"'
      *--->   DELIMITED BY SIZE  INTO EVAL-SOURCE.
           STRING FORM-NAME "-" FIELD-FIELD DISP--ATR
              DELIMITED BY " " INTO EVAL-SOURCE.
           MOVE DISP-VDU-ATR TO EVAL-DEST.
           IF NOT REPORT-ONLY
              WRITE EVA-REC FROM EVAL-LINE1.

      *    -------------------------------------------------------------
      *    WRITE *.EVA LINE 5  (MOVE "00" TO STAT)
      *    -------------------------------------------------------------
           IF NOT REPORT-ONLY
              WRITE EVA-REC FROM EVAL-LINE2.

       SKIP-NONDISPLAY-FIELDS.
           IF FIELD-IS-NONDISPLAY
      *       ----------------------------------------------------------
      *       WRITE NDS INFO FOR REFERENCE
      *       ----------------------------------------------------------
              IF NOT FIELD-FIELD-ND-SKIP
                 WRITE NDS-REC FROM D-ND-LINE-1
                 GO TO BUILD-FIELD-EXIT.


       BUILD-FIELD-EXIT.
           EXIT.

       READ-INPUT-FILE SECTION.
           MOVE SPACES TO IN-REC.
           READ IN-FILE AT END MOVE "XX" TO INFILE-STAT.
       READ-INPUT-FILE-EXIT.
           EXIT.

      *--------------------------------------------------------------------
      * CREATE LIST OF ALL FORMS IN A GIVEN DIRECTORY TO CONVERT THEM ALL
      *--------------------------------------------------------------------
       CREATE-DIR-LIST SECTION.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LIST-FILE2-PATH

           CALL "C$TOLOWER" USING WS-LS-D, VALUE 5.
           STRING WS-LS-D " /" WS-REL-PATH "/FM/XX/?? > " MKTEMP-BUF
                   DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF.
           MOVE "Y" TO PROCESS-ALL-FG.
           MOVE SPACES TO LIST-DIR-WORKER.
           OPEN INPUT LIST-FILE2.
           MOVE "Y" TO LIST-FILE2-OPEN-FG.
       CREATE-DIR-LIST-RN.
           MOVE SPACES TO LIST-REC.
           READ LIST-FILE2 NEXT AT END MOVE "D" TO PROCESS-ALL-FG
                                       GO TO EXIT-PROGRAM.
           MOVE LIST-REC2 TO LIST-DIR-WORKER
           IF FILE-STAT = "00" AND LDW-JUNK NOT = SPACES
              GO TO CREATE-DIR-LIST-RN.
           IF LDW-SKIP-DIR
              GO TO CREATE-DIR-LIST-RN.
           MOVE LDW-DIR-NAME TO INPUT-DIR.

      *--------------------------------------------------------------------
      * CREATE LIST OF ALL FORMS IN A GIVEN DIRECTORY TO CONVERT THEM ALL
      *--------------------------------------------------------------------
       CREATE-LIST-BY-DIR SECTION.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LIST-FILE1-PATH

           CALL "C$TOLOWER" USING WS-LS, VALUE 5.
           STRING WS-LS " " WS-REL "* > " MKTEMP-BUF
                   DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF.
           MOVE "Y" TO PROCESS-FULL-DIR.
           MOVE SPACES TO LIST-REC-WORKER.
           OPEN INPUT LIST-FILE1.
       CREATE-LIST-BY-DIR-RN.
           MOVE SPACES TO LIST-REC.
           READ LIST-FILE1 NEXT AT END MOVE "++" TO FILE-STAT.
           IF FILE-STAT NOT = "++"
              MOVE LIST-REC TO LIST-REC-WORKER
              IF FILE-STAT = "00" AND LRW-JUNK NOT = SPACES
                 GO TO CREATE-LIST-BY-DIR-RN
              END-IF
              MOVE LRW-FORM-NAME TO INPUT-FILE FORM-NAME.


       CLOSE-FILES SECTION.
           IF FILES-OPENED = "Y"
              IF NOT REPORT-ONLY
                 CLOSE SCN-FILE
                 WRITE WKS-REC2 FROM D-FIELD-LINE-1B-LAST-PERIOD
                 CLOSE WKS-FILE
                 CLOSE WKS-FILE2
                 CLOSE DEF-FILE
                 CLOSE EVA-FILE
      *--->      CLOSE HLP-FILE
      *   DO NOT COMBINE WKS AND DEF. WE NEED TO KEEP AEPARATE.
      *--------->MOVE SPACES TO SYSTEM-BUF
      *          STRING CAT-CMD " " WKS-PATH ">>" DEF-PATH
      *                 DELIMITED BY "  " INTO SYSTEM-BUF
      *          CALL "SYSTEM" USING SYSTEM-BUF
      *          MOVE SPACES TO SYSTEM-BUF
      *          STRING CAT-CMD " " DEF-PATH ">" WKS-PATH
      *                 DELIMITED BY "  " INTO SYSTEM-BUF
      *          CALL "SYSTEM" USING SYSTEM-BUF
      *          MOVE SPACES TO SYSTEM-BUF
      *          STRING RM-CMD " " DEF-PATH
      *              DELIMITED BY "  " INTO SYSTEM-BUF
      *--------->CALL "SYSTEM" USING SYSTEM-BUF

      *   APPEND _WKS3 TO _WKS
                     STRING CAT-CMD " " WKS-PATH2 ">>" WKS-PATH
                            DELIMITED BY "  " INTO SYSTEM-BUF
                     CALL "SYSTEM" USING SYSTEM-BUF
      *   REMOVE _WKS3
                     MOVE SPACES TO SYSTEM-BUF
                     STRING RM-CMD " " WKS-PATH2
                            DELIMITED BY "  " INTO SYSTEM-BUF
                     CALL "SYSTEM" USING SYSTEM-BUF
                     MOVE SPACES TO SYSTEM-BUF
                     STRING "/" WS-RELSRC-PATH
                            "/UT/RM-TR-SP.SH#"  DEF-PATH
                            DELIMITED BY "  " INTO SYSTEM-BUF
                     INSPECT SYSTEM-BUF REPLACING ALL '#' BY ' '
                     CALL "SYSTEM" USING SYSTEM-BUF
                     MOVE SPACES TO SYSTEM-BUF
                     STRING "/" WS-RELSRC-PATH
                            "/UT/RM-TR-SP.SH#"  SCN-PATH
                            DELIMITED BY "  " INTO SYSTEM-BUF
                     INSPECT SYSTEM-BUF REPLACING ALL '#' BY ' '
                     CALL "SYSTEM" USING SYSTEM-BUF
                     MOVE SPACES TO SYSTEM-BUF
                     STRING "/" WS-RELSRC-PATH
                            "/UT/RM-TR-SP.SH#"  WKS-PATH
                            DELIMITED BY "  " INTO SYSTEM-BUF
                     INSPECT SYSTEM-BUF REPLACING ALL '#' BY ' '
                     CALL "SYSTEM" USING SYSTEM-BUF
                     MOVE SPACES TO SYSTEM-BUF
                     STRING "/" WS-RELSRC-PATH
                            "/UT/RM-TR-SP.SH#"  EVA-PATH
                            DELIMITED BY "  " INTO SYSTEM-BUF
                     INSPECT SYSTEM-BUF REPLACING ALL '#' BY ' '
                     CALL "SYSTEM" USING SYSTEM-BUF
              END-IF
              CLOSE IN-FILE
              CLOSE NDS-FILE
              CLOSE ERR-FILE
              MOVE "N" TO FILES-OPENED
              MOVE SPACES TO SYSTEM-BUF
              IF EL-EXISTS NOT = "Y"
                 STRING RM-CMD " " ERR-PATH
                     DELIMITED BY "  " INTO SYSTEM-BUF
                 CALL "SYSTEM" USING SYSTEM-BUF.

       SET-PATHS SECTION.

           MOVE SPACES TO SRC-PATH
                          DEF-PATH
                          EVA-PATH
                          SCN-PATH
                          WKS-PATH
                          WKS-PATH2
                          NDS-PATH
                          ERR-PATH.
           
           STRING WS-REL INPUT-FILE
                  DELIMITED BY " " INTO SRC-PATH.
           STRING WS-RELSRC INPUT-FILE "_DEF.CPY"
                  DELIMITED BY " " INTO DEF-PATH.
           STRING WS-RELSRC INPUT-FILE "_EVA.CPY"
                  DELIMITED BY " " INTO EVA-PATH.
           STRING WS-RELSRC INPUT-FILE "_SCN.CPY"
                  DELIMITED BY " " INTO SCN-PATH.
           STRING WS-RELSRC INPUT-FILE "_WKS.CPY"
                  DELIMITED BY " " INTO WKS-PATH.
           STRING WS-RELSRC INPUT-FILE "_WKS3.CPY"
                  DELIMITED BY " " INTO WKS-PATH2.
      *--->STRING WS-RELSRC INPUT-FILE "_HLP.CPY"
      *--->       DELIMITED BY " " INTO HLP-PATH.
           STRING "/" WS-RELSRC-PATH "/NDLOGS/" INPUT-DIR "_"
                  INPUT-FILE ".NDS"
                  DELIMITED BY " " INTO NDS-PATH.
           STRING "/" WS-RELSRC-PATH "/ERLOGS/" INPUT-DIR "_"
                  INPUT-FILE ".ERR"
                  DELIMITED BY " " INTO ERR-PATH.


       WRITE-ERR-LOG SECTION.
           MOVE SPACES TO EL-MESSAGE-TEXT.
           EVALUATE TRUE
                WHEN EL-ERROR = "1"
                          STRING "W "INPUT-DIR " " FORM-NAME
                                 " INVALID FORM PIC :  "
                                 FIELD-FIELD " " FIELD-PIC " " EL-CHG
                                 DELIMITED BY SIZE INTO EL-MESSAGE-TEXT
                WHEN EL-ERROR = "2"
                          STRING "E "INPUT-DIR " " FORM-NAME
                                 " INVALID FORM PIC :  "
                                 FIELD-FIELD " " FIELD-PIC " " EL-CHG
                                 DELIMITED BY SIZE INTO EL-MESSAGE-TEXT
                WHEN EL-ERROR = "3"
                          STRING "E " INPUT-DIR " " FORM-NAME
                                 " PROMPT TOO LONG :  "
                                 "'" IN-REC(1:MSG1-LENGTH) "'"
                                 DELIMITED BY SIZE INTO EL-MESSAGE-TEXT

           END-EVALUATE.
           WRITE ERR-REC FROM EL-MESSAGE-TEXT.
           MOVE "Y" TO EL-EXISTS.

       MISC-SECTION SECTION.
           COPY "LIBGB/MKTEMP.CPY".

       EXIT-PROGRAM SECTION.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
           IF PROCESS-FULL-DIR = "Y"
               CLOSE LIST-FILE1
               MOVE SPACES TO SYSTEM-BUF
               STRING RM-CMD " " LIST-FILE1-PATH
                      DELIMITED BY "  " INTO SYSTEM-BUF
               CALL "SYSTEM" USING SYSTEM-BUF.

           IF PROCESS-ALL
              GO TO EXIT-PROGRAM-NO-STOP.

           IF LIST-FILE2-OPEN
              CLOSE LIST-FILE2
              MOVE SPACES TO SYSTEM-BUF
              STRING RM-CMD " " LIST-FILE2-PATH
                     DELIMITED BY "  " INTO SYSTEM-BUF
              CALL "SYSTEM" USING SYSTEM-BUF.

           MOVE SPACES TO MESS.
           STRING "NON-DISPLAY LOGS SAVED IN: /"
                  WS-RELSRC-PATH "/NDLOGS/"
                  DELIMITED BY SIZE INTO MESS.
           DISPLAY MESS.
           MOVE SPACES TO MESS.
           STRING "ERROR LOGS SAVED IN: /"
                  WS-RELSRC-PATH "/ERLOGS/"
                  DELIMITED BY SIZE INTO MESS.
           DISPLAY MESS.

           MOVE SPACES TO MESS.
           ACCEPT MESS
                  REVERSE
                  DEFAULT "         HIT RETURN TO CONTINUE".
           CLOSE WINDOW SCRN1-POP-UP-WINDOW2.
           MOVE SPACES TO SYSTEM-BUF.

           ACCEPT MESS
                  AT 022002
                  REVERSE
                  DEFAULT "         HIT RETURN TO EXIT".

           STOP RUN.

       EXIT-PROGRAM-NO-STOP.
              MOVE "Y" TO PROCESS-ALL-FG.
