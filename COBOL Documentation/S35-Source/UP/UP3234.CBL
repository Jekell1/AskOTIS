       IDENTIFICATION DIVISION.
       PROGRAM-ID.    UP3234.
       AUTHOR.        KEC.
       DATE-WRITTEN.  08-31-2018.
      ******************************************************************
      *
      *  IDENTIFICATION: UP/UP3234.CBL
      *
      *  DESCRIPTION   : UPGRADE FOR FILES: 'A32' TO 'A34'
      *
      *  ---GLOBAL---
      *  EO  [GB01EO]  - FROM KEVIN  - MUST BE FILE.DOC/UP3535_EOFILE
      *  FD  [LP01FD]  - FROM KEVIN
      *  FDB [LP01FDB] - BETH KEEPING UP ON WAC4GL A32
      *  SN  [GB01SN]  - BETH KEEPING UP ON WAC4GL A32
      *
      *  *** = ACU4GL HAS A LIMITATION OF 1024 COLUMNS/FIELDS AND 
      *  8064 BYTES PER ROW/RECORD.
      *
      *=================================================================
      * REV:
      * KEC 2020-0831 ***NEW*** DUE TO A CHANGE IT WAS NOT POSSIBLE TO
      *          KEEP EOFILE AND FDFILE UP TO DATE WITHIN A30.  SO A32
      *          WAS LOADED ONTO A LOCAL AIX SERVER AT WAC FOR KEVIN
      *          MULLEN'S TEAM TO KEEP THE FILES UP TO DATE.  THE FILES
      *          THEN WILL BE TRANSFERRED TO /USR/S35/FILE.DOC AT THE
      *          TIME OF UPGRADING TO LIVE PRODUCTION:
      *             UP3535_EOFILE AND .VIX - THESE ARE IN A35
      *             UP3234_FDFILE AND .VIX
      *             UP3234_FDBFILE AND .VIX
      *             UP3234_SNFILE AND .VIX
      *
      * 2023.0601 CHANGED OEOFILE TO BE UP3535_EOFILE AS THEY ARE NOW
      *           USING A35 TO KEEP THAT FILE CURRENT
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       COPY "LIBUP/UP3234_SPFSREPORT.CPY".

      *=================================================================
      *    NEED TO HARDCODED FS FOR A32 FILES;
      *    FILES ARE A32 VISION FILE TO BE UPLOADED TO A34 SQL;
      *    NEED TO HAVE OXXX-PATH NOT OXXX-SELECT-PATH.
      *====================================================[ SOURCE FS ]
      *COPY "LIBUP/A30SRC/GBFSEO.CPY" REPLACING LEADING "EO" BY "OEO".
      * COPYMEMBER: LIBGB/GBFSEO
      *    SELECT EO-FILE ASSIGN TO EO-SELECT-PATH             
           SELECT OEO-FILE ASSIGN TO OEO-PATH             
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OEO1-KEY
                  FILE STATUS FILE-STAT.

      *COPY "LIBUP/A30SRC/LPFSFD.CPY" REPLACING LEADING "FD" BY "OFD".
      * COPYMEMBER: LIBLP/LPFSFD
      *    SELECT FD-FILE ASSIGN FD-SELECT-PATH               
           SELECT OFD-FILE ASSIGN OFD-PATH               
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OFD1-KEY
                  FILE STATUS FILE-STAT.

      *COPY "LIBUP/A30SRC/LPFSFDB.CPY"
      *                               REPLACING LEADING "FDB" BY "OFDB".
      * COPYMEMBER: LIBLP/LPFSFDB
      *    SELECT FDB-FILE ASSIGN FDB-SELECT-PATH            
           SELECT OFDB-FILE ASSIGN OFDB-PATH 
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OFDB1-KEY
                  FILE STATUS FILE-STAT.

           SELECT OSN-FILE ASSIGN TO OSN-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OSN1-KEY
                  ALTERNATE RECORD KEY OSN2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.

      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       COPY "LIBUP/UP3234_SPFDREPORT.CPY".

      *====================================================[ SOURCE FD ]
       FD  OEO-FILE
           LABEL RECORDS ARE STANDARD.
       01  OEO-REC.
           03  OEO1-KEY.
               05  OEO-PCODE                    PIC X(3).
               05  OEO-STEP                     PIC 9999.
      $XFD DATE=YYYYMMDD
           03  OEO-LTOUCH-DATE                  PIC 9(08).
           03  OEO-DIR                          PIC XX.
           03  OEO-PROGRAM                      PIC X(6).
           03  OEO-COPIES                       PIC 9.
           03  OEO-COMMENT                      PIC X(30).
           03  OEO-MESS                         PIC X(60).
      $XFD DATE=YYYYMMDD
           03  OEO-START-DATE                   PIC 9(8).
           03  OEO-START-TIME                   PIC 9(6).
      $XFD DATE=YYYYMMDD
           03  OEO-COMPLETE-DATE                PIC 9(8).
           03  OEO-COMPLETE-TIME                PIC 9(6).
           03  OEO-COMPLETE-STAT                PIC X(6).
           03  OEO-PARAMETERS                   PIC X(600).

           03  OEO-ERROR-STEP-X                 PIC XXXX.
           03  OEO-ERROR-STEP REDEFINES OEO-ERROR-STEP-X
                                               PIC 9999.
           03  OEO-TRANSFER                     PIC X.
           03  OEO-LEVEL                        PIC 9.

           03  OEO-FUTURE-NUMERIC               PIC 9(8)   OCCURS 4.
           03  OEO-FUTURE-ALPHA                 PIC X(10)  OCCURS 5.

       FD  OFD-FILE
           LABEL RECORDS ARE STANDARD.
       01  OFD-REC.
           03  OFD1-KEY.
               05  OFD-FMNO                     PIC 9(4).
      $XFD DATE=YYYYMMDD
           03  OFD-LTOUCH-DATE                  PIC 9(08)      COMP-3.
           03  OFD-DESC                         PIC X(25).
           03  OFD-OPTION               PIC X(1).
           03  OFD-FORM-CHG-CD          PIC X.
           03  OFD-PROG-PREFIX          PIC X.
           03  OFD-TYPE                 PIC X.
           03  OFD-LNMEMO               PIC X.
           03  OFD-BODY                 PIC 9(4)  COMP-3.
           03  OFD-TOP                  PIC 9(4)  COMP-3.
           03  OFD-BOTTOM               PIC 9(4)  COMP-3.
           03  OFD-PROG                 PIC XX.
           03  OFD-PROMPTS              PIC X(80) OCCURS 30.
           03  OFD-INFO.
               05  OFD-FIELDINFO                  OCCURS 50.
                   07  OFD-FIELDNO      PIC 9(4)  COMP-3.
                   07  OFD-ROWCOL.
                       09  OFD-FIELDROW PIC 9(2)  COMP-3.
                       09  OFD-FIELDCOL PIC 9(2)  COMP-3.
                   07  OFD-FIELDED      PIC 9(2)  COMP-3.
                   07  OFD-FIELDKEY     PIC 9(4)  COMP-3.
           03  OFD-1STFILE              PIC 9(2)  COMP-3.
           03  OFD-INCR-DELCNT          PIC X.
           03  OFD-FMTYPE               PIC X.
           03  OFD-PRCONT               PIC X.
           03  OFD-FMLINK.
               05  OFD-NEXTFM           PIC 9(4).
           03  OFD-VALID-BRANCH         PIC X(4).
           03  OFD-MIN-FORM-SENT-DAYS   PIC 99.
           03  OFD-SKIP-BAD-ADR         PIC X.
           03  OFD-SECURITY-PROFILE     PIC X.
           03  OFD-FORM-RIGHT-TO-CURE   PIC X.
           03  OFD-TEST-WHICH-STATE     PIC X.
           03  OFD-STATE-TABLE.
               05  OFD-STATE            PIC XX OCCURS 50.
           03  OFD-TYPE-OF-DOC          PIC XX.
           03  OFD-FUTURE-FLAGS         PIC X           OCCURS 3.
           03  OFD-FUTURE-ALPHA         PIC X(10)       OCCURS 5.
           03  OFD-FUTURE-NUMERIC       PIC 9(4) COMP-3 OCCURS 5.


       FD  OFDB-FILE
           LABEL RECORDS ARE STANDARD.
       01  OFDB-REC.
           03  OFDB1-KEY.
               05  OFDB-DBNO                    PIC 9(02).
      $XFD DATE=YYYYMMDD
           03  OFDB-LTOUCH-DATE                 PIC 9(08)      COMP-3.
           03  OFDB-FIELD-DESCS.
               05  OFDB-DATABASE                OCCURS 200.
                   07  OFDB-DNAME               PIC X(08).
                   07  OFDB-DOFFSET             PIC 9(04)      COMP-3.
                   07  OFDB-DDIGITS             PIC 9(02)      COMP-3.
                   07  OFDB-DTYPE               PIC 9(02)      COMP-3.
                   07  OFDB-DDECIMAL            PIC 9(02)      COMP-3.
           03  OFDB-CALLKEY                     OCCURS 2.
               05  OFDB-CALLID                  PIC X(02).
               05  OFDB-KEYLOC                  PIC 9(04)       COMP-3.


       FD  OSN-FILE
           LABEL RECORDS ARE STANDARD.
       01  OSN-REC.
           03  OSN2-KEY.
               05  OSN-SEQ              PIC 9(8).
               05  OSN1-KEY.
                   07  OSN-FORM         PIC X(6).
      $XFD DATE=YYYYMMDD
           03  OSN-LTOUCH-DATE          PIC 9(08)   COMP-3.
           03  OSN-DESC                 PIC X(30).
           03  OSN-PURCHASABLE          PIC X.
           03  OSN-FUTURE-FLAG          PIC X     OCCURS 2.
           03  OSN-FUTURE-ALPHA         PIC X(10) OCCURS 2.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/UP3234.CBL S35 02/23/24-11:34:36 >".

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
      *====================================================[ TARGET WS ]
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LP01FDB_SQL.CPY".
       COPY "LIBLP/LPWSFDB.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".

       COPY "LIBLP/LP01WK.CPY".

      *-----------------------------------------------------------------
      *    UP3234-FILEDOC-PATH IS UNIQUE FOR A30-TO-A34 UPGRADE FOR
      *    MAKING THE SOURCE PATH OF /USR/A3#/FILE.DOC
      *-----------------------------------------------------------------
       01  UP3234-FILEDOC-PATH.
           03  FILLER                  PIC X(1)      VALUE "/".
           03  UP3234-FILEDOC-REL      PIC X(7)      VALUE "USR/A34".
           03  FILLER                  PIC X(9)      VALUE "/FILE.DOC".

      *-----------------------------------------------------------------
       01  PR-IFN                      PIC X(02)     VALUE "PR".
       01  UP3234-REPORT-KEY           PIC 9(09).
       01  UP3234-REPORT-PATH          PIC X(35).

      *====================================================[ SOURCE WS ]
       COPY "LIBGB/GBWSEO.CPY"  REPLACING LEADING "EO" BY "OEO".
       COPY "LIBLP/LPWSFD.CPY"  REPLACING LEADING "FD" BY "OFD"
                                          LEADING "LPWSFD" BY "OLPWSFD".
       COPY "LIBLP/LPWSFDB.CPY" REPLACING LEADING "FDB" BY "OFDB".
       COPY "LIBGB/GBWSSN.CPY"  REPLACING LEADING "SN" BY "OSN".


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

       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  ER-FEED                     PIC 9(01)     VALUE 1.
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

       01  RECORDS-FOUND               PIC X(01).
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  SUB                         PIC 99.

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
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING DATE (MMYY)     ".
           03  FILLER PIC X(19) VALUE "                   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP/DOWN, EN".
           03  FILLER PIC X(30) VALUE "TER ENDING DATE (MMYY)        ".
           03  FILLER PIC X(19) VALUE "                   ".

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
               "--------- UPGRADE A32 TO A34 REPORT ---------".
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

           03  BEG-DATE-YYYYMM         PIC 9(06).
           03  END-DATE-YYYYMM         PIC 9(06).

           03  FILE-OPTION-WORKERS.
               05  FILE-OPTION-EO      PIC X(01).
               05  FILE-OPTION-FD      PIC X(01).
               05  FILE-OPTION-FDB     PIC X(01).
               05  FILE-OPTION-SN      PIC X(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      *01  UP3234-PRU-LP               PIC X(03).

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

       COPY "LIBUP/UP3234W.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
      *------------------------------------------{ NEED TO BE TOGETHER }
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/UP3234_DEF.CPY".
       COPY "LIBUP/UP3234_WKS.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

      *=================================================================
       SCREEN SECTION.
       COPY "LIBUP/UP3234_SCN.CPY".

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
                 OEO-FILE  OSN-FILE OFD-FILE OFDB-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-FDB1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-EO1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE REPORT-FILE
                 OEO-FILE    OSN-FILE   OFD-FILE  OFDB-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP FORM-PATHNAME.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GLOBAL FILES UPGRADE 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           PERFORM PROCESS-FILE-EO.
           PERFORM PROCESS-FILE-FD.
           PERFORM PROCESS-FILE-FDB.
           PERFORM PROCESS-FILE-SN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    START BRANCH-GROUP LOGIC
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( GROUP-FLAG = "N" )
      *       MOVE BEG-BRANCH TO BR-NO
      *       PERFORM START-BR-FILE
      *       GO TO MAIN-PROGRAM-NEXT-BRANCH.
      *
      *    MOVE ENT-GROUP TO GR-ID.
      *    PERFORM SEARCH-GR-INIT.
      *
      *=================================================================
      *    LOOPING TO GET NEXT GROUP OR BRANCH.
      *=================================================================
      *MAIN-PROGRAM-NEXT-GROUP.
      *
      *    IF ( GROUP-FLAG = "N" )
      *       GO TO MAIN-PROGRAM-NEXT-BRANCH.
      *
      *    PERFORM SEARCH-GR-NEXT.
      *
      *    IF ( WGR-SRESULT = ZEROES )
      *       GO TO MAIN-PROGRAM-EXIT.
      *
      *    MOVE WGR-SRESULT TO BR-NO.
      *    PERFORM READ-BR-FILE.
      *    IF ( IO-BAD )
      *       GO TO IO-ERROR.
      *
      *    GO TO MAIN-PROGRAM-PROCESS-BRNO.
      *
      *-----------------------------------------------------------------
      *MAIN-PROGRAM-NEXT-BRANCH.
      *
      *    PERFORM READ-BR-FILE-NEXT.
      *
      *    IF ( IO-BAD             ) OR
      *       ( BR-NO > END-BRANCH )
      *       GO TO MAIN-PROGRAM-EXIT.
      *
      *-----------------------------------------------------------------
      *MAIN-PROGRAM-PROCESS-BRNO.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETTING BUT NOT NECESSARY AT THE MOMENT 2015-1002 JAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
      *    MOVE BR-MACHINE                 TO FILPATH-MACHINE.
      *    MOVE BR-DATA-PATH               TO FILPATH-DATA.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PERFORM UP3234-CREATE-BRANCH-HEAD.
      *
      *    PERFORM PROCESS-FILE-XX.
      *
      *    GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-EXIT.

           PERFORM PRINT-REPORT-MODULE.
           GO TO END-PROGRAM.

      *****************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "X"                        TO RECORDS-FOUND.
           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "UP3234"                   TO VDU-FORM-NAME.
           MOVE UP3234-QUEUE-POS           TO Q-POS.
           MOVE UP3234-COPIES-POS          TO C-POS.
           MOVE UP3234-PRU-POS             TO P-POS.
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

           MOVE "N"                        TO RECORDS-FOUND.
           PERFORM SEND-STARTING-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ SETUP-HEADER }
           MOVE EXT-CONAME-CONAME          TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE         TO H1-DATE.
           MOVE EXT-CONAME-USER-ID         TO H2-USERID.

      *- - - - - - - - - - - - - - - - - - - - - -{ MAKING TEMP FILES  }
           MOVE MKTEMP-DEFAULT             TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF                 TO UP3234-REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.

           MOVE ZEROES TO UP3234-REPORT-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ OPEN FILES }
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-REPORT-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      *-----------------------------------------------------------------
       ENTER-PARAMETERS-BEGIN.

           PERFORM SEND-TITLE.
           PERFORM SEND-FIL.
           PERFORM SEND-MACHINE.

           MOVE SOURCE-PATH                TO WR-BUF.
           MOVE SOURCE-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE TARGET-PATH                TO WR-BUF.
           MOVE TARGET-PATH-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP-KEY )
              GO TO ENTER-OPTION-FDB.

      *=================================================================
      *ENTER-SOURCE-PATH.

      *    MOVE LEGEND-SOURCE-PATH TO LEGEND.
      *    PERFORM SEND-LEGEND.

      *    MOVE "E  A170S-PATH." TO VDU.
      *    PERFORM SCREENIO.

      *    IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
      *    IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( VDUBUF = SPACES )
      *       GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH.
      *    PERFORM DIR-ACCESS-CALL.

      *    IF ( STAT-BAD )
      *       PERFORM INVALID-FILE-PATH-MESSAGE
      *       GO TO ENTER-SOURCE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE: A32-TO-A34 UPGRADE USES /USR/A3#/FILE.DOC AS THE
      *          SOURCE PATH WHICH DOES NOT REQUIRE THE FOLLOWING OF
      *          PUTTING THE RISC MACHINE WITHIN THE SOURCE PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE SPACES                     TO DIR-ACCESS-BUF.
      *
      *    STRING VDUBUF, "/", CURRENT-MACHINE
      *           DELIMITED BY " "       INTO DIR-ACCESS-BUF.
      *
      *    PERFORM DIR-ACCESS-CALL.
      *
      *    IF ( STAT-BAD )
      *       PERFORM INVALID-FILE-PATH-MESSAGE
      *       GO TO ENTER-SOURCE-PATH.
      *
      *    MOVE VDUBUF                TO SOURCE-PATH.

      *-----------------------------------------------------------------
       ENTER-TARGET-PATH.

           MOVE LEGEND-TARGET-PATH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A090T-PATH." TO VDU.
           PERFORM SCREENIO.

      *KC  IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
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
      *KC     GO TO ENTER-SOURCE-PATH.
              GO TO ENTER-TARGET-PATH.

      *=================================================================
      *    ENTERING BRANCH NUMBER RANGE.
      *=================================================================
       ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO *NOT* ALLOW BRANCH ENTRY; FORCE F3-ALL BRANCHES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO ENTER-ALL-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *    DISPLAYING THE USER PROMPT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "S  A220DISPLAY-USER." TO VDU.
      *    MOVE SPACES                 TO VDUBUF
      *
      *    IF ( BEG-BRANCH = ZEROES ) AND
      *       ( END-BRANCH = ZEROES )
      *       MOVE "***USER BRANCH ONLY***" TO VDUBUF.
      *
      *--->PERFORM SCREENIO.

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
       ENTER-OPTION-EO.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-EO." TO VDU.
           PERFORM SCREENIO.

      *KC  IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        UP-KEY ) GO TO ENTER-TARGET-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-EO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-EO.

           IF NOT ( FILE-OPTION-EO = "Y" OR "N" OR "C" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-EO.

      *-----------------------------------------------------------------
       ENTER-OPTION-FD.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-FD." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-EO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-FD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-FD.

           IF NOT ( FILE-OPTION-FD = "Y" OR "N" OR "C" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-FD.

      *-----------------------------------------------------------------
       ENTER-OPTION-FDB.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-FDB." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-FD.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-FDB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-FDB.

           IF NOT ( FILE-OPTION-FDB = "Y" OR "N" OR "C" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-FDB.

      *-----------------------------------------------------------------
       ENTER-OPTION-SN.

           MOVE LEGEND-OPTION-FILES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010OPTION-SN." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-OPTION-FDB.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F2-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F4-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F5-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F6-KEY ) GO TO ENTER-ALL-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-OPTION-SN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FILE-OPTION-SN.

           IF NOT ( FILE-OPTION-SN = "Y" OR "N" OR "C" )
              PERFORM INVALID-YNC-MESSAGE
              GO TO ENTER-OPTION-SN.

      *- - - - - - - - - - - - - - - - - - - - -{ SKIP TO 'DATE RANGE' }
           GO TO ENTER-ACCEPT-YN.

      *========================================{ SOURCE & TARGET FILES }
       ENTER-ALL-OPTIONS.

           IF ( F2-KEY )
              MOVE "C" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "C" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F3-KEY )
              MOVE "Y" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "Y" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F4-KEY )
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "N" TO DEFAULT-LOCAL-OPTIONS
           ELSE
           IF ( F5-KEY )
              MOVE "Y" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "N" TO DEFAULT-LOCAL-OPTIONS
           ELSE
              MOVE "N" TO DEFAULT-GLOBAL-OPTIONS
              MOVE "Y" TO DEFAULT-LOCAL-OPTIONS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ GLOBAL }
           MOVE "S  A010OPTION-EO."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-EO.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-FD."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-FD.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-FDB."    TO VDU.
           MOVE DEFAULT-GLOBAL-OPTIONS TO VDUBUF FILE-OPTION-FDB.
           PERFORM SCREENIO.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170S-PATH."          TO VDU.
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
           MOVE "S  A010OPTION-EO."    TO VDU.
           MOVE FILE-OPTION-EO         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-FD."    TO VDU.
           MOVE FILE-OPTION-FD         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010OPTION-FDB."   TO VDU.
           MOVE FILE-OPTION-FDB        TO VDUBUF.
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

      *=================================================================
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
      *    P R O C E S S - F I L E - E O
      *
      *=================================================================
      *    IN  : EO-REC
      *          UP3234-VPI WORKERS
      *          FILE-OPTION-WORKERS
      *
      *    OUT : UP3234-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-EO SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-EO = "N" )
              GO TO PROCESS-FILE-EO-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO EO-PATH-OVERRIDE
                                             OEO-PATH-OVERRIDE.
           MOVE "EO"                       TO UP3234-CURRENT-FILE.
           MOVE "GB/EOFILE"                TO UP3234-FILE-INPUT.
           MOVE UP3234-FILE-TYPE---GLOBAL  TO UP3234-FILE-TYPE.
           MOVE ZEROES                     TO UP3234-RECORD-COUNTERS.

           PERFORM UP3234-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP3234-FILE-ORDER---SOURCE TO UP3234-FILE-ORDER.
      *KC  PERFORM UP3234-LOAD-FILE-PATH.
           PERFORM MISC-LOAD-SOURCE-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO ACCESS-BUF OEO-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP3234-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-EO-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP3234-FILE-ORDER---TARGET TO UP3234-FILE-ORDER.
           PERFORM UP3234-LOAD-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO EO-PATH.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-EO = "C" )
              PERFORM OPEN-OEO1-FILE
              PERFORM CLOSE-OEO1-FILE
              GO TO PROCESS-FILE-EO-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES TO OEO1-KEY.

           PERFORM OPEN-EO1-FILE.

           PERFORM OPEN-OEO1-FILE.
           PERFORM START-OEO1-FILE.

      *=================================================================
       PROCESS-FILE-EO-NEXT.

           PERFORM READ-OEO1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-EO-END.

           ADD  01  TO UP3234-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP3234-VPI-DISPLAY.

           MOVE OEO-REC                    TO EO-REC.
           IF OEO-LEVEL NOT NUMERIC
              MOVE 0 TO EO-LEVEL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-EO1-FILE.
           IF ( IO-BAD )
              ADD 1                        TO UP3234-RECORD-COUNTER-SKIP
              MOVE EO1-KEY                 TO UP3234-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP3234-UPDATE-STAT
              MOVE "E"                     TO UP3234-UPDATE-TYPE
              PERFORM UP3234-CREATE-FILE-UPDATE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-EO-NEXT
              ELSE
                 GO TO IO-ERROR.

           ADD 1 TO UP3234-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-EO-NEXT.

      *=================================================================
       PROCESS-FILE-EO-END.

           PERFORM CLOSE-OEO1-FILE.
           PERFORM CLOSE-EO1-FILE.

           PERFORM UP3234-CREATE-FILE-TOTALS.

       PROCESS-FILE-EO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - F D
      *
      *=================================================================
      *    IN  : FD-REC
      *          UP3234-VPI WORKERS
      *          FILE-OPTION-WORKERS
      *
      *    OUT : UP3234-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-FD SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-FD = "N" )
              GO TO PROCESS-FILE-FD-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO FD-PATH-OVERRIDE
                                             OFD-PATH-OVERRIDE.
           MOVE "FD"                       TO UP3234-CURRENT-FILE.
           MOVE "LP/FDFILE"                TO UP3234-FILE-INPUT.
           MOVE UP3234-FILE-TYPE---GLOBAL  TO UP3234-FILE-TYPE.
           MOVE ZEROES                     TO UP3234-RECORD-COUNTERS.

           PERFORM UP3234-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP3234-FILE-ORDER---SOURCE TO UP3234-FILE-ORDER.
      *KC  PERFORM UP3234-LOAD-FILE-PATH.
           PERFORM MISC-LOAD-SOURCE-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO ACCESS-BUF OFD-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP3234-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-FD-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP3234-FILE-ORDER---TARGET TO UP3234-FILE-ORDER.
           PERFORM UP3234-LOAD-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO FD-PATH.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-FD = "C" )
              PERFORM OPEN-OFD1-FILE
              PERFORM CLOSE-OFD1-FILE
              GO TO PROCESS-FILE-FD-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES TO OFD1-KEY.

           PERFORM OPEN-FD1-FILE.

           PERFORM OPEN-OFD1-FILE.
           PERFORM START-OFD1-FILE.

      *=================================================================
       PROCESS-FILE-FD-NEXT.

           PERFORM READ-OFD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-FD-END.

           ADD  01  TO UP3234-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP3234-VPI-DISPLAY.

           MOVE OFD-REC                    TO FD-REC.

           
           IF FD-BODY NOT NUMERIC
              MOVE 0 TO FD-BODY.
           IF FD-TOP NOT NUMERIC
              MOVE 0 TO FD-TOP.
           IF FD-BOTTOM NOT NUMERIC
              MOVE 0 TO FD-BOTTOM.
           IF FD-1STFILE NOT NUMERIC
              MOVE 0 TO FD-1STFILE.
           IF FD-NEXTFM NOT NUMERIC
              MOVE 0 TO FD-NEXTFM.
           IF FD-MIN-FORM-SENT-DAYS NOT NUMERIC
              MOVE 0 TO FD-MIN-FORM-SENT-DAYS.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 50
              IF FD-FIELDNO(SUB) NOT NUMERIC
                 MOVE 0 TO FD-FIELDNO(SUB)
              END-IF
              IF FD-FIELDROW(SUB) NOT NUMERIC
                 MOVE 0 TO FD-FIELDROW(SUB)
              END-IF
              IF FD-FIELDCOL(SUB) NOT NUMERIC
                 MOVE 0 TO FD-FIELDCOL(SUB)
              END-IF
              IF FD-FIELDED(SUB) NOT NUMERIC
                 MOVE 0 TO FD-FIELDED(SUB)
              END-IF
              IF FD-FIELDKEY(SUB) NOT NUMERIC
                 MOVE 0 TO FD-FIELDKEY(SUB)
              END-IF
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-FD1-FILE.
           IF ( IO-BAD )
              ADD 1                        TO UP3234-RECORD-COUNTER-SKIP
              MOVE FD1-KEY                 TO UP3234-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP3234-UPDATE-STAT
              MOVE "E"                     TO UP3234-UPDATE-TYPE
              PERFORM UP3234-CREATE-FILE-UPDATE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-FD-NEXT
              ELSE
                 GO TO IO-ERROR.

           ADD 1 TO UP3234-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-FD-NEXT.

      *=================================================================
       PROCESS-FILE-FD-END.

           PERFORM CLOSE-OFD1-FILE.
           PERFORM CLOSE-FD1-FILE.

           PERFORM UP3234-CREATE-FILE-TOTALS.

       PROCESS-FILE-FD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - F D B
      *
      *=================================================================
      *    IN  : FDB-REC
      *          UP3234-VPI WORKERS
      *          FILE-OPTION-WORKERS
      *
      *    OUT : UP3234-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-FDB SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-FDB = "N" )
              GO TO PROCESS-FILE-FDB-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO FDB-PATH-OVERRIDE
                                             OFDB-PATH-OVERRIDE.
           MOVE "FDB"                      TO UP3234-CURRENT-FILE.
           MOVE "LP/FDBFILE"               TO UP3234-FILE-INPUT.
           MOVE UP3234-FILE-TYPE---GLOBAL  TO UP3234-FILE-TYPE.
           MOVE ZEROES                     TO UP3234-RECORD-COUNTERS.

           PERFORM UP3234-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP3234-FILE-ORDER---SOURCE TO UP3234-FILE-ORDER.
      *KC  PERFORM UP3234-LOAD-FILE-PATH.
           PERFORM MISC-LOAD-SOURCE-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO ACCESS-BUF OFDB-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP3234-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-FDB-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP3234-FILE-ORDER---TARGET TO UP3234-FILE-ORDER.
           PERFORM UP3234-LOAD-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO FDB-PATH.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-FDB = "C" )
              PERFORM OPEN-OFDB1-FILE
              PERFORM CLOSE-OFDB1-FILE
              GO TO PROCESS-FILE-FDB-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES TO OFDB1-KEY.

           PERFORM OPEN-FDB1-FILE.

           PERFORM OPEN-OFDB1-FILE.
           PERFORM START-OFDB1-FILE.

      *=================================================================
       PROCESS-FILE-FDB-NEXT.

           PERFORM READ-OFDB1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-FDB-END.

           ADD  01  TO UP3234-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP3234-VPI-DISPLAY.

           MOVE OFDB-REC                   TO FDB-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-FDB1-FILE.
           IF ( IO-BAD )
              ADD 1                        TO UP3234-RECORD-COUNTER-SKIP
              MOVE FDB1-KEY                TO UP3234-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP3234-UPDATE-STAT
              MOVE "E"                     TO UP3234-UPDATE-TYPE
              PERFORM UP3234-CREATE-FILE-UPDATE
              IF ( FILE-STAT-DISP = "22,00" )
                 PERFORM REWRITE-FDB1-FILE
              ELSE
                 GO TO IO-ERROR.

           ADD 1 TO UP3234-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-FDB-NEXT.

      *=================================================================
       PROCESS-FILE-FDB-END.

           PERFORM CLOSE-OFDB1-FILE.
           PERFORM CLOSE-FDB1-FILE.

           PERFORM UP3234-CREATE-FILE-TOTALS.

       PROCESS-FILE-FDB-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - F I L E - E O
      *
      *=================================================================
      *    IN  : SN-REC
      *          UP3234-VPI WORKERS
      *          FILE-OPTION-WORKERS
      *
      *    OUT : UP3234-VPI WORKERS
      *          UPGRADE-STAT   [BAD/GOOD]
      *
      *    DESC: PROCESS SOURCE TO TARGET OF GIVEN FILE.
      *
      ******************************************************************
       PROCESS-FILE-SN SECTION.

           MOVE UPGRADE-STAT---GOOD        TO UPGRADE-STAT.

           IF ( FILE-OPTION-SN = "N" )
              GO TO PROCESS-FILE-SN-EXIT.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "Y"                        TO SN-PATH-OVERRIDE
                                             OSN-PATH-OVERRIDE.
           MOVE "SN"                       TO UP3234-CURRENT-FILE.
           MOVE "GB/SNFILE"                TO UP3234-FILE-INPUT.
           MOVE UP3234-FILE-TYPE---GLOBAL  TO UP3234-FILE-TYPE.
           MOVE ZEROES                     TO UP3234-RECORD-COUNTERS.

           PERFORM UP3234-VPI-START.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP SOURCE PATHS }
           MOVE UP3234-FILE-ORDER---SOURCE TO UP3234-FILE-ORDER.
           PERFORM MISC-LOAD-SOURCE-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO ACCESS-BUF OSN-PATH.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE STAT                    TO UPGRADE-STAT
              PERFORM UP3234-CREATE-FILE-ERROR
              GO TO PROCESS-FILE-SN-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ SETUP TARGET PATHS }
           MOVE UP3234-FILE-ORDER---TARGET TO UP3234-FILE-ORDER.
           PERFORM UP3234-LOAD-FILE-PATH.
           MOVE UP3234-FILE-PATH           TO SN-PATH.

      *- - - - - - - - - - - - - - - - - - -{ CHECK SOURCE FILE STATUS }
           IF ( FILE-OPTION-SN = "C" )
              PERFORM OPEN-OSN1-FILE
              PERFORM CLOSE-OSN1-FILE
              GO TO PROCESS-FILE-SN-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN & START FILES }
           MOVE SPACES TO OSN1-KEY.

           PERFORM OPEN-SN1-FILE.

           PERFORM OPEN-OSN1-FILE.
           PERFORM START-OSN1-FILE.

      *=================================================================
       PROCESS-FILE-SN-NEXT.

           PERFORM READ-OSN1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-FILE-SN-END.

           ADD  01  TO UP3234-RECORD-COUNTER-READ.
           MOVE "Y" TO RECORDS-FOUND.
           PERFORM UP3234-VPI-DISPLAY.

           MOVE OSN-REC                    TO SN-REC.
           IF OSN-SEQ NOT NUMERIC
              MOVE 0 TO SN-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-SN1-FILE.
           IF ( IO-BAD )
              ADD 1                        TO UP3234-RECORD-COUNTER-SKIP
              MOVE SN1-KEY                 TO UP3234-UPDATE-KEY
              MOVE FILE-STAT-DISP          TO UP3234-UPDATE-STAT
              MOVE "E"                     TO UP3234-UPDATE-TYPE
              PERFORM UP3234-CREATE-FILE-UPDATE
              IF ( FILE-STAT-DISP = "22,00" )
                 GO TO PROCESS-FILE-SN-NEXT
              ELSE
                 GO TO IO-ERROR.

           ADD 1 TO UP3234-RECORD-COUNTER-WRITE.

           GO TO PROCESS-FILE-SN-NEXT.

      *=================================================================
       PROCESS-FILE-SN-END.

           PERFORM CLOSE-OSN1-FILE.
           PERFORM CLOSE-SN1-FILE.

           PERFORM UP3234-CREATE-FILE-TOTALS.

       PROCESS-FILE-SN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R I N T - R E P O R T - M O D U L E
      *
      ******************************************************************
       PRINT-REPORT-MODULE SECTION.

      *- - - - - - - - - - - - - - - - - - -{ SET CURRENT FILE WORKERS }
           MOVE "REPORT"                   TO UP3234-CURRENT-FILE.
           MOVE UP3234-FILE-TYPE---GLOBAL  TO UP3234-FILE-TYPE.

           PERFORM UP3234-VPI-START.

           PERFORM OPEN-PR-FILE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP 'REPORT' FILE  }
           MOVE ZEROES                     TO UP3234-REPORT-KEY.

           PERFORM OPEN-REPORT-FILE.
           PERFORM START-REPORT-FILE.

      *=================================================================
       PRINT-REPORT-NEXT-LINE.

           PERFORM READ-REPORT-FILE-NEXT.

           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-END.

           PERFORM UP3234-VPI-DISPLAY.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE SOURCE-PATH                          TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TARGET PATH                      :" TO R1-DESC.
           MOVE TARGET-PATH                          TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "*********[ GLOBAL FILES ]*********" TO DETAIL-LINE.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "EO-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-EO                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "FD-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-FD                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "FDB-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-FDB                      TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN-FILE (Y/N)                    :" TO R1-DESC.
           MOVE FILE-OPTION-SN                       TO R1-FIELD.
           MOVE 1                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME OF THE UPGRADE     :" TO R1-DESC.
           MOVE STARTING-TIME                        TO R1-FIELD.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           IF ( EXT-CONAME-RPT-LINES > ZEROES )
              IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
                 PERFORM WRITE-HEADER-LINES
              ELSE
                 NEXT SENTENCE
           ELSE
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
      *    U P 3 0 3 2 - C R E A T E - D E T A I L - L I N E
      *
      *=================================================================
      *    IN  : UP3234-DETAIL-LINE
      *          UP3234-LN-FEED
      *          UP3234-REPORT-KEY
      *
      *    OUT : UP3234-DETAIL-LINE
      *          UP3234-LN-FEED
      *          UP3234-REPORT-KEY
      *
      *    DESC: WRITE RECORD TO REPORT-FILE USING DETAIL-LINE.
      ******************************************************************
       UP3234-CREATE-DETAIL-LINE SECTION.

            IF ( UP3234-REPORT-KEY = 999999999 )
               GO TO UP3234-CREATE-DETAIL-EXIT.

      *- - - - - - - - - - - - - - - - - - - -{ CREATE 'REPORT' RECORD }
            ADD  01                        TO UP3234-REPORT-KEY.
            MOVE UP3234-DETAIL-LINE        TO REPORT-DETAIL-LINE.
            MOVE UP3234-LN-FEED            TO REPORT-LN-FEED.

            PERFORM WRITE-REPORT-FILE.

            IF ( IO-BAD )
               GO TO IO-ERROR.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ RESET FIELDS }
            MOVE SPACES                    TO UP3234-DETAIL-LINE.
            MOVE 01                        TO UP3234-LN-FEED.

      *=================================================================
       UP3234-CREATE-DETAIL-EXIT.
            EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - C R E A T E - B R A N C H - H E A D
      *
      *=================================================================
      *    IN  : BR-REC
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE HEADER LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-BRANCH-HEAD SECTION.

           MOVE "BRANCH: "                 TO UP3234-D1-BRANCH-PROMPT.
           MOVE BR-NO                      TO UP3234-D1-BRANCH.
           MOVE BR-NAME                    TO UP3234-D1-BRANCH-NAME.
           MOVE 02                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-B-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - C R E A T E - M A C H I N E - H E A D
      *
      *=================================================================
      *    IN  : UP3234-MACHINE
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE MACHINE LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-MACHINE-HEAD SECTION.

           MOVE "MACHINE: "                TO UP3234-D2-MACHINE-PROMPT.
      *KC  MOVE UP3234-MACHINE             TO UP3234-D2-MACHINE.
           MOVE CURRENT-MACHINE            TO UP3234-D2-MACHINE.
           MOVE "RISC MACHINE"             TO UP3234-D2-MACHINE-NAME.
           MOVE 02                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-M-HEAD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - C R E A T E - F I L E - E R R O R
      *
      *=================================================================
      *    IN  : UP3234-CURRECT-FILE
      *          UP3234-FILE-ORDER    [SOURCE/TARGET]
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE ERROR LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-FILE-ERROR SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP3234-F1-FILE-PROMPT.
           MOVE UP3234-CURRENT-FILE        TO UP3234-F1-FILE.

           IF ( UP3234-FILE-ORDER-SOURCE )
              STRING "ERROR - SOURCE FILE: ",STAT
                  DELIMITED BY "  "      INTO UP3234-F1-FILE-MESSAGE
           ELSE
           IF ( UP3234-FILE-ORDER-TARGET )
              STRING "ERROR - TARGET FILE: ",STAT
                  DELIMITED BY "  "      INTO UP3234-F1-FILE-MESSAGE
           ELSE
              STRING "ERROR - MISC.. FILE: ",STAT
                  DELIMITED BY "  "      INTO UP3234-F1-FILE-MESSAGE.

           MOVE 01                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-FILE-ER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - C R E A T E - F I L E - U P D A T E
      *
      *=================================================================
      *    IN  : UP3234-CURRENT-FILE
      *          UP3234-UPDATE-KEY
      *          UP3234-UPDATE-STAT
      *          UP3234-UPDATE-TYPE    [ERROR/SKIP]
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE LINE OF SKIP/ERROR RECORD FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-FILE-UPDATE SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEGIN  }

           MOVE "FILE: "                   TO UP3234-F1-FILE-PROMPT.
           MOVE UP3234-CURRENT-FILE        TO UP3234-F1-FILE.

           IF ( UP3234-UPDATE-TYPE-ERROR )
              MOVE SPACES                  TO UP3234-F1-FILE-MESSAGE
              STRING "ERROR - KEY: ",
                     UP3234-UPDATE-KEY, " ["
                     UP3234-UPDATE-STAT, "]"
                     DELIMITED BY "  "   INTO UP3234-F1-FILE-MESSAGE
           ELSE
              MOVE SPACES                  TO UP3234-F1-FILE-MESSAGE
              STRING "SKIP - KEY: ",
                     UP3234-UPDATE-KEY
                     DELIMITED BY "  "   INTO UP3234-F1-FILE-MESSAGE.

           MOVE 01                         TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-FILE-UP-END.

           MOVE SPACES                     TO UP3234-UPDATE-KEY
                                              UP3234-UPDATE-TYPE
                                              UP3234-UPDATE-STAT.

      *-----------------------------------------------------------------
       UP3234-CREATE-FILE-UP-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - C R E A T E - F I L E - T O T A L S
      *
      *=================================================================
      *    IN  : UP3234-CURRECT-FILE
      *          UP3234-RECORD-COUNTERS
      *          STAT
      *
      *    OUT : NOTHING
      *
      *    DESC: CREATE TOTAL LINE FOR THE REPORT-FILE.
      ******************************************************************
       UP3234-CREATE-FILE-TOTALS SECTION.

           MOVE "FILE: "                    TO UP3234-F1-FILE-PROMPT.
           MOVE UP3234-CURRENT-FILE         TO UP3234-F1-FILE.

           MOVE "READ: "                    TO
                UP3234-F1-RECORD-READ-PROMPT.
           MOVE UP3234-RECORD-COUNTER-READ  TO UP3234-F1-RECORD-READ.

           MOVE "WRITE: "                   TO
                UP3234-F1-RECORD-WRITE-PROMPT.
           MOVE UP3234-RECORD-COUNTER-WRITE TO UP3234-F1-RECORD-WRITE.

           MOVE "SKIP: "                    TO
                UP3234-F1-RECORD-SKIP-PROMPT.
           MOVE UP3234-RECORD-COUNTER-SKIP  TO UP3234-F1-RECORD-SKIP.

           MOVE 01                          TO UP3234-LN-FEED.
           PERFORM UP3234-CREATE-DETAIL-LINE.

      *=================================================================
       UP3234-CREATE-FILE-T-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - L O A D - F I L E - P A T H
      *
      *=================================================================
      *    IN  : BR-REC
      *          UP3234-FILE-INPUT  [EXP.  XX/XXXXXX]
      *          UP3234-FILE-ORDER  [SOURCE/TARGET]
      *          UP3234-FILE-TYPE   [GLOBAL/LOCAL]
      *          UP3234-SOURCE-PATH
      *          UP3234-TARGET-PATH
      *
      *    OUT : UP3234-FILE-PATH
      *          - GLOBAL: /XXX/XXXX/R#/XX/XXXXXX
      *          - LOCAL : /XXX/XXXX/R#/XX####/XX/XXXXXX
      *
      *    DESC: LOAD FILE PATH BASED ON SOURCE/TARGET & GLOBAL/LOCAL.
      ******************************************************************
       UP3234-LOAD-FILE-PATH SECTION.

           MOVE SPACES                     TO UP3234-FILE-PATH.

           IF ( UP3234-FILE-INPUT           = SPACES ) OR
              ( UP3234-FILE-INPUT-DIR       = SPACES ) OR
              ( UP3234-FILE-INPUT-SLASH NOT = "/"    ) OR
              ( UP3234-FILE-INPUT-NAME      = SPACES )
              GO TO UP3234-LOAD-FILE-PATH-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-SOURCE ) AND
              ( UP3234-FILE-TYPE-GLOBAL  )
      *KC     MOVE UP3234-SOURCE-PATH  TO UP3234-FILE-PATH
              MOVE SOURCE-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD SOURCE LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-SOURCE ) AND
              ( UP3234-FILE-TYPE-LOCAL   )
      *KC     MOVE UP3234-SOURCE-PATH  TO UP3234-FILE-PATH
              MOVE SOURCE-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP3234-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP3234-FILE-PATH-LOCAL-DATA-SL
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-LOCAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET GLOBAL --- /XXX/XXXX/R#/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-TARGET ) AND
              ( UP3234-FILE-TYPE-GLOBAL  )
      *KC     MOVE UP3234-TARGET-PATH  TO UP3234-FILE-PATH
              MOVE TARGET-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-GLOBAL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD TARGET LOCAL --- /XXX/XXXX/R#/XX####/XX/XXXXXX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-FILE-ORDER-TARGET ) AND
              ( UP3234-FILE-TYPE-LOCAL   )
      *KC     MOVE UP3234-TARGET-PATH  TO UP3234-FILE-PATH
              MOVE TARGET-PATH         TO UP3234-FILE-PATH
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-3
      *KC     MOVE UP3234-MACHINE      TO UP3234-FILE-PATH-MACHINE
              MOVE CURRENT-MACHINE     TO UP3234-FILE-PATH-MACHINE
              MOVE "/"                 TO UP3234-FILE-PATH-SLASH-4
              MOVE BR-DATA-PATH        TO UP3234-FILE-PATH-LOCAL-DATA
              MOVE "/"                 TO UP3234-FILE-PATH-LOCAL-DATA-SL
              MOVE UP3234-FILE-INPUT   TO UP3234-FILE-PATH-LOCAL-PATH.

      *=================================================================
       UP3234-LOAD-FILE-PATH-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - V P I - D I S P L A Y 
      *
      *=================================================================
      *   IN:  UP3234-VPI-BRNO
      *        UP3234-VPI-FILE-PREFIX
      *        UP3234-VPI-FILE-TYPE
      *        UP3234-VPI-PRU-LP
      *        UP3234-VPI-ROUTE-BUF
      *        UP3234-VPI-SLEEP-COUNTER
      *        UP3234-VPI-SLEEP-COUNTER-MAX
      *        UP3234-VPI-TICKER
      *        UP3234-VPI-TIME-EDIT
      *
      *  OUT:  UP3234-VPI-SLEEP-COUNTER
      *        UP3234-VPI-TICKER
      *
      * DESC:  UP3234 VIDEO PROGRESS INDICATOR (VPI)
      *        DISPLAYS TO THE SCREEN THE BRANCH NUMBER & FILE THE
      *        PROGRAM IS CURRENTLY CONVERTING.
      *
      ******************************************************************
       UP3234-VPI-DISPLAY SECTION.

           IF ( UP3234-VPI-SLEEP-COUNTER 
              < UP3234-VPI-SLEEP-COUNTER-MAX )
              ADD 1 TO UP3234-VPI-SLEEP-COUNTER
              GO TO UP3234-VPI-DISPLAY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BEGIN TO CREATE UP3234-VPI-DISPLAY-BUF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO UP3234-VPI-SLEEP-COUNTER.
           MOVE SPACES                     TO UP3234-VPI-DISPLAY-BUF.

           MOVE "("                        TO UP3234-VPI-DISPLAY-LEFT.
           MOVE ")"                        TO UP3234-VPI-DISPLAY-RIGHT.

           IF ( UP3234-VPI-TICKER = "-" )
              MOVE "\"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER
           ELSE
           IF ( UP3234-VPI-TICKER = "\" )
              MOVE "|"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER
           ELSE
           IF ( UP3234-VPI-TICKER = "|" )
              MOVE "/"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER
           ELSE
              MOVE "-"                     TO UP3234-VPI-DISPLAY-TICKER
                                              UP3234-VPI-TICKER.

           IF ( UP3234-VPI-FILE-TYPE-LOCAL )
              MOVE UP3234-VPI-BRNO         TO UP3234-VPI-DISPLAY-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET THE FILE NAME IN THE DISPLAY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "***FILE NOT FOUND***"     TO UP3234-VPI-DISPLAY-FILE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ GLOBAL }
           IF ( UP3234-VPI-FILE-EO )
              MOVE "BATCH REPORT PROCESSING (EO)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-SN )
              MOVE "USER SECURITY (SN)" TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-FD )
              MOVE "FORM DESCRIPTION (FD)" TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-FDB )
              MOVE "FORM DESCRIPTION DATABASE (FDB)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -{ MISC }
           IF ( UP3234-VPI-FILE-WK1 )
              MOVE "WORK FILE 1 (WK1)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-WK2 )
              MOVE "WORK FILE 2 (WK2)"
                TO UP3234-VPI-DISPLAY-FILE
           ELSE
           IF ( UP3234-VPI-FILE-REPORT )
              MOVE "UP3234 REPORT (REPORT)"
                TO UP3234-VPI-DISPLAY-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UP3234-VPI-TIME-EDIT       TO UP3234-VPI-DISPLAY-TIME.
           CALL "C$JUSTIFY" USING UP3234-VPI-DISPLAY-TEXT "L".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SENDING THE 'PROGRESS INDICTATOR' LINE DISPLAY TO THE SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UP3234-VPI-ROUTE-MENU-DRIVEN     ) AND
              ( UP3234-VPI-QUEUE-PRINTER-OR-VIEW )
              MOVE UP3234-VPI-DISPLAY-BUF  TO WR-BUF
              MOVE UP3234-VPI-DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *=================================================================
       UP3234-VPI-DISPLAY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    U P 3 0 3 2 - V P I - S T A R T
      *
      *=================================================================
      *   IN:  UP3234-CURRENT-FILE
      *        UP3234-FILE-TYPE
      *        UP3234-PRU-LP
      *
      *  OUT:  UP3234-VPI WORKERS
      *
      * DESC:  SETTING THE VIDEO PROGRESS INDICATOR (VPI) WORKERS FOR
      *        THE START OF FILE UPGRADE.
      *
      ******************************************************************
       UP3234-VPI-START SECTION.

           IF ( UP3234-FILE-TYPE-LOCAL )
              MOVE BR-NO                   TO UP3234-VPI-BRNO
           ELSE
              MOVE ZEROES                  TO UP3234-VPI-BRNO.

           MOVE UP3234-CURRENT-FILE        TO UP3234-VPI-FILE-PREFIX.
           MOVE UP3234-FILE-TYPE           TO UP3234-VPI-FILE-TYPE.
      *KC  MOVE UP3234-PRU-LP              TO UP3234-VPI-PRU-LP.
           MOVE PRU-LP                     TO UP3234-VPI-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO UP3234-VPI-ROUTE-BUF.
           MOVE 9999                       TO UP3234-VPI-SLEEP-COUNTER.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO UP3234-VPI-TIME-EDIT.

      *=================================================================
       UP3234-VPI-START-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

      *    MOVE EXT-CONAME-CONAME          TO UP3234-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO UP3234-SYSDATE.
      *    DISPLAY UP3234-SCREEN         UPON TCLP-MAIN-WINDOW-HANDLE.
           DISPLAY UP3234-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UP3234-FORM-FIELDS         TO WR-BUF.
           MOVE UP3234-FORM-LIST           TO WR-LIST.
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

           MOVE UP3234-HELP--FILE          TO HELP-LINK-FILE.
           MOVE UP3234-HELP--DIR           TO HELP-LINK-DIR.

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
              COPY "LIBUP/UP3234_EVA.CPY".
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
      * THIS IS NEEDED FOR WRITE OF SP1-FILE
      *COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/GPENV.CPY".     *> NOT NEEDED SINCE CONVERTING
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *COPY "LIBUP/UP3234.CPY".

      *COPY "LIBLP/LPLNCL.CPY".
      *COPY "LIBLP/LPLXCL.CPY".
      *COPY "LIBLP/LPCDCL.CPY".
      *COPY "LIBLP/LPCDBCL.CPY".

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
      *    LOAD UP3234-FILE-PATH FOR SOURCE DIRECTORY.
      *    /USR/A3#/FILE.DOC/UP3234_??FILE  [GOOD]
      *    /USR/A3#/FILE.DOC/XXXXXX_??FILE  [BAD]
      *=================================================================
       MISC-LOAD-SOURCE-FILE-PATH.

           MOVE SPACES TO MESS.

           IF ( UP3234-CURRENT-FILE = "EO" )
              MOVE "UP3535_EOFILE"         TO MESS
           ELSE
           IF ( UP3234-CURRENT-FILE = "SN" )
              MOVE "UP3234_SNFILE"         TO MESS
           ELSE
           IF ( UP3234-CURRENT-FILE = "FD" )
              MOVE "UP3234_FDFILE"         TO MESS
           ELSE
           IF ( UP3234-CURRENT-FILE = "FDB" )
              MOVE "UP3234_FDBFILE"        TO MESS
           ELSE
              MOVE "XXXXXX_"               TO MESS.

           CALL "C$TOLOWER" USING MESS, VALUE 06.

           MOVE SPACES TO UP3234-FILE-PATH.

           STRING UP3234-FILEDOC-PATH, "/", MESS
                  DELIMITED BY "  "        INTO UP3234-FILE-PATH.

      *=================================================================
       SEND-FIL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    UP3234-FILEDOC-PATH IS UNIQUE FOR A32-TO-A34 UPGRADE FOR
      *    MAKING THE SOURCE PATH OF /USR/A3#/FILE.DOC
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF                 TO UP3234-FILEDOC-REL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.

           MOVE SPACES                     TO UP3234-FILE-PATH.
           STRING "/", GETENV-BUF
                  DELIMITED BY "  "      INTO UP3234-FILE-PATH.
           MOVE UP3234-FILE-PATH-MACHINE   TO CURRENT-MACHINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK TO SEE IF THE SOURCE & TARGET PATHS ARE SET FROM BATCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( SOURCE-PATH = SPACES )
      *       MOVE UP3234-FILEDOC-PATH     TO SOURCE-PATH
      *    ELSE
      *       MOVE SOURCE-PATH             TO DIR-ACCESS-BUF
      *       PERFORM DIR-ACCESS-CALL
      *       IF ( STAT-BAD )
      *KC        MOVE UP3234-FILEDOC-PATH  TO SOURCE-PATH.
           MOVE UP3234-FILEDOC-PATH        TO SOURCE-PATH.
     
           IF ( TARGET-PATH = SPACES )
              MOVE UP3234-FILE-PATH        TO TARGET-PATH
           ELSE
              MOVE TARGET-PATH             TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE UP3234-FILE-PATH     TO TARGET-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CURRENT FIL: "            TO FIL-PROMPT.
           MOVE UP3234-FILE-PATH           TO FIL-INFO.

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
           MOVE "[ 'A32' TO 'A34' CONVERSION  ]" TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPFDBGS_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      *----------------------------------------------------[ TARGET IO ]
       COPY "LIBGB/GBEO1IN.CPY".
       COPY "LIBLP/LPFD1IN.CPY".
       COPY "LIBLP/LPFDB1IN.CPY".
       COPY "LIBGB/GBSN1IN.CPY".

       COPY "LIBUP/UP3234_SPIOREPORT.CPY".

      *=================================================================
      *   DO NOT NEED TO DO 'REPLACING' ON THE COPY MEMBERS;
      *   ALREADY ADDED AN 'O' IN FRONT OF THE FILE PREFIX.
      *====================================================[ SOURCE IO ]
       COPY "LIBUP/A32SRC/GBEO1RN.CPY".
       COPY "LIBUP/A32SRC/LPFD1RN.CPY".
       COPY "LIBUP/A32SRC/LPFDB1RN.CPY".
       COPY "LIBUP/A32SRC/GBSN1RN.CPY".

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
      *    MOVING REPORT FILE IN /USR/TMP TO /USR/TMP/UP3234.MKTEMP
      *    SAVING REPORT FILE IN CASE OF ANY OF THE UPGRADE PROGRAMS
      *    BOMB OUT SO YOU CAN SEE WHICH FILE WAS PROCESSED LAST.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MV "                      TO SAVE-REPORT-CMD.
           CALL "C$TOLOWER" USING SAVE-REPORT-CMD, VALUE 03.

           MOVE UP3234-REPORT-PATH         TO MKTEMP-BUF
                                              SAVE-REPORT-SOURCE
                                              SAVE-REPORT-TARGET.

           MOVE "UP3234."                  TO SAVE-REPORT-TARGET-NEW.
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
              DESTROY UP3234-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

