      ***************************************************************
      * 7/26/12  CONVERSION TO A30 COMPLETED
      *          NOTE- VERIFIED UP TO DATE.  DID NOT INCLUDE
      *             GEC-NON-MONTHLY-CAN-LOANS, BUT I COULDN'T FIND THIS
      *             FLAG BEING USED, SO I DIDN'T BRING IT OVER (FLAG
      *             ADDED BY JOHN, PR# J043
      *
      *                    (CONAME.EXT)
      *
      *   GLOBAL ENVIRONMENT CONAME BUFFER
      *
      * REV:
      *  MJD 000526 CONVERTED FOR A311
      *  MJD 120601 CHANGED CONAME BUF TO BE EXTERNAL.
      *             REMOVED "VALUE"'S FROM EXTERNAL WORKERS.  NOT ALLOWED.
      *  BLM 120723 ADDED CONAME-USER-GROUP AS PART OF CONAME-USER-NM
      *  MJD 121016 CHANGED CONAME.EXT-CONAME-BUF TO BE EXT-CONAME-BUF
      *             ADDED EXT- PREFIX TO ALL FIELDS COINTAINED IN THE BUFFER.
      *  KEC 121019 ADDED EXT-FILPATH.
      *  MJD 121029 ADDED EXT-MENU-OPTION-FG.   THIS WILL BE USED IN PLACES
      *             WHERE THE MENU WOULD SET A NON-DISPLAY OPTION FOR
      *             THE PROGRAM.  I.E. BYINQ TO DETERMINE SNAPSHOT/REAL-TIME.
      *  MJD 121214 ADDED HANDLE FOR MAIN TCLP INITIAL WINDOW.
      *  BAH 130109 ADDED BICREA-CALLED-FROM 
      *  MJD 130123 RENAMED 1 CHAR FILLER IN USER-ID WITH
      *             EXT-CONAME-USER-PROFILE-CD.
      *  BLM 131212 REDEFINE EXT-CONAME-SYSDATE TO HAVE
      *             EXT-CONAME-SYSDATE-MM, DD, YY
      *  BAH 140409 REMOVED FILLER OF 10 IN FRONT OF EXT-CONAME-BRANCH-NAME
      *              WAS CENTERING IT AND LOSING FIRST COUPLE CHARACTERS
      *              EXAMPLE: DISPLAYING "ARADATA", LOSING THE P
      *  MJD 140502 ADDED EXT-WIN WORKERS FOR WINDOW OFFSET AND SIZE IN VERYANT
      *  BAH 140512 ADDED EXT-SPANISH-FORMS FOR WORLD
      *  MJD ADDED MORE EXTRNAL WORKERS FOR WINDOWS.
      *      LABEL-OFFET THROUGH WCOLOR-005
      *  MJD 150121 ADDED NAMES TO FILLER LINES UNDER EXT-TERM-ATTRIBUTES
      *  BLM 150408 ADDED REDEFINITION OF EXT-FIL-PATH TO INCLUDE:
      *    EXT-FILPATH-LOCAL, EXAMPLE:     /USR/DATA/R1/SC0001/
      *    EXT-FILPATH-GLOBAL, EXAMPLE:    /USR/DATA/R1/
      *    ALSO ADDED EXT-FILPATH-SLASH2 (AFTER BRANCH)
      *
      * KEC 2015.1124 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED A REDEFINES ON EXT-FILPATH-BRANCH FOR
      *         THE FIELD NAME EXT-FILPATH-BRNO.
      *
      * KEC 2016.0502 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASE EXT-MISC-LINK-BUF TO 12 BYTES DUE TO
      *          EXT-LAMENU-APP-NUMBER NEEDING MORE.
      *         EXT-LAMENU-APPNO CHANGED TO EXT-LAMENU-APP-NUMBER.
      *
      * KEC 2018-0822 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      *         LEAVING EXT-LAMENU-APP-NUMBER HERE IN CASE
      *         NEEDED IN THE FUTURE.
      *
      * KEC 2018-0926 ADDED EXT-RELEASE FOR A32 & A34 (4GL).
      *
      * KEC 2019-1105 THE A15-TO-A34 UPGRADE (UP1534) FOR WORLD MAY TAKE
      *               LONGER THAN ONE WEEKEND.  SO NEED A WAY TO
      *               DETERMINE WHAT IS A VISION FILE AND SQL TABLE.
      *               ADDED EXT-IO-FILE-TYPE AND ITS SUBORDINATE
      *               FIELDS WHICH WILL BE USED IN EACH FILE'S
      *               IO ROUTINE LOAD PARAGRAPHS TO BUILD THE
      *               SELECT PATH (??-SELECT-PATH).
      * BAH 2020-0610 ADDED A FIELD FOR TESTING SQL DATABASE VALUE FOR
      *               A ZERO 0 (NULL) DATE, SETENV WILL SET TO 19000101
      *
      * JKC 2020-0612 COMMENTED OUT THE FOLLOWING SINCE A GREP SHOWED
      *         THEY WERE NOT USED, WHILE MOVING EXT-MENU-OPTION-FG OUT
      *         OF THE GROUP LEVEL OF EXT-TERM-ATTRIBUTES (NOT SURE
      *         WHY IT IS THERE SINCE IT IS NOT A TERM ITEM):
      *            EXT-CONAME-AUTO-INS-EARN
      *            EXT-CONAME-EXTRACT-AUTO-UPD
      *            EXT-CONAME-BRGIFTNET
      *            EXT-TERM-ATTRIBUTES
      *            EXT-TERM-NAME
      *            EXT-TERM-ATTR-REVERSE
      *            EXT-TERM-HAS-REVERSE
      *            EXT-TERM-ATTR-BLINK
      *            EXT-TERM-HAS-BLINK
      *            EXT-TERM-ATTR-UNDERLINE
      *            EXT-TERM-HAS-UNDERLINE
      *            EXT-TERM-ATTR-DUAL-INTENSITY
      *            EXT-TERM-HAS-DUAL-INTENSITY
      *            EXT-TERM-ATTR-132-MODE
      *            EXT-TERM-HAS-132-MODE
      *            EXT-TERM-ATTR-COLOR
      *            EXT-TERM-HAS-COLOR
      *            EXT-TERM-ATTR-LINE-DRAW
      *            EXT-TERM-HAS-LINE-DRAW
      *            EXT-TERM-LINES
      *            EXT-TERM-COLUMNS
      *            EXT-TERM-ATTR-PIGGY
      *            EXT-TERM-HAS-PIGGY
      *            EXT-TERM-ATTR-VIS-ATTRIB
      *            EXT-TERM-HAS-VIS-ATTRIB
      *            EXT-TERM-ATTR-GUI
      *            EXT-TERM-HAS-GUI
      *            EXT-TERM-USABLE-SCRN-HEIGHT
      *            EXT-TERM-USABLE-SCRN-WIDTH
      *            EXT-TERM-PHYS-SCRN-HEIGHT
      *            EXT-TERM-PHYS-SCRN-WIDTH
      *            EXT-MISC-LINK-BUF
      *            EXT-LAMENU-APP-NUMBER
      *            EXT-LAMENU-CALLING-STAT
      *            EXT-RELEASE
      *            EXT-RELEASE-VISION
      * BAH 2020-0623 ADDED A4GL-WHERE-CONSTRAINT
      * JKC 2020-1023 CHANGED '01  PARADATA.';
      *               ADDED '03  PARADATA-REL-VERSION' WHICH WILL BE
      *               POPULATED BY REL ENVIRONMENT VARIABLE.
      * JKC 2020-1026 ADDED PARADATA-REL-VER-EXT.
      * JKC 2021-0114 ADDED ACUSQL CONNECTION VARIABLES:
      *               + EXT-ACUSQL-ODBC-NAME
      *               + EXT-ACUSQL-LOGIN
      *               + EXT-ACUSQL-PSWD
      *
      * JKC 2021-0726 ADDED EXT-SQL-CONNECT-STAT FOR CHECKING IF 
      *               CONNECTION TO SQL SERVER HAS ALREADY BEEN MADE;
      *               USED TO ONLY CONNECT ONE-TIME INSTEAD OF MULTIPLE
      *               TIMES IN EVERY PROGRAM.
      *
      * JKC 2021-0726 ADDED EXT-ACUSQL-CONNECT-STAT FOR CHECKING IF 
      * JKC 2021-0727 ADDED MORE ACUSQL CONNECTION VARIABLES:
      *               + EXT-ACUSQL-CONNECT-STAT
      *               - EXT-SQL-CONNECT-STAT
      *
      * BAH 2022-0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2022-0627 RENAMED EXT-ACUSQL-ODBC-NAME TO DSN-NAME
      * JKC 2022-0120 ADDED EXT-IO-AUFILE
      * BAH 2023-0504 REMOVED EXT-CONMAE-A50-CPCORP
      * JKC 2023-0901 ADDED EXT-TCLP-ACUMEM-WORKERS
      * JKC 2023-1114 ADDED EXT-ACUSQL-AUTOCOMMIT
      * JKC 2023-1127 REMOVED EXT-IO-FILE-TYPE AND ITS SUBORDINATES.
      *               FIELDS WHICH WILL BE USED IN EACH FILE'S
      *               IO ROUTINE LOAD PARAGRAPHS TO BUILD THE
      *               SELECT PATH (??-SELECT-PATH); NO LONGER NEEDED
      *               SINCE EVERYTHING WILL BE SQL WITH NO VISION.
      * JKC 2023-1212 ADDED EXT-ACUSQL-SERVER-WORKERS;
      *               + EXT-ACUSQL-DB-NAME
      *               + EXT-ACUSQL-SERVER-NAME
      *               + EXT-ACUSQL-VERSION
      * JKC 2023-1220 ADDED EXT-JULIAN-DATE-WORKERS;
      *               + EXT-JULIAN-BEG
      *               + EXT-JULIAN-END
      *               + EXT-JULIAN-CC
      * JKC 2024-0124 ADDED EXT-TCLP-TRACE-WORKERS;
      *               + EXT-TCLP-TRACE
      * JKC 2024-0131 ADDED MORE EXT-TCLP-TRACE-WORKERS
      *          + EXT-TCLP-TRACE-FORM-PATH
      *          + EXT-TCLP-TRACE-MESS
      *          + EXT-TCLP-TRACE-SKIP-YN
      * JKC 2024-0131 ADDED MORE EXT-TCLP-TRACE-WORKERS
      *          + EXT-TCLP-TRACE-LEVEL
      * JKC 2024-0404 CHANGED THE FOLLOWING:
      *          + EXT-FILLER-FUTURE-USE   PIC X(300).
      *          - EXT-WINDOW-CELL-HEIGHT           PIC 99.
      *          - EXT-WINDOW-CELL-WIDTH            PIC 99.
      *          - VDU-FIELD-PROMPT.
      *          - VDU-WIN-L-OFFSET
      *          - VDU-WIN-C-OFFSET
      *          - VDU-WIN-W-MOD
      *          - VDU-WIN-H-MOD
      *          - VDU-WCTRL-TCLP
      *          - VDU-WCTRL-FWIN
      *          - VDU-WCTRL-SCAN
      *          - VDU-WCTRL-EOINIT
      *          - EXT-WCTRL-005
      *          - VDU-WCOLOR-TCLP
      *          - VDU-WCOLOR-FWIN
      *          - VDU-WCOLOR-SCAN
      *          - VDU-WCOLOR-EOINIT
      *          - EXT-WCOLOR-005
      *          - VDU-LABEL-OFFSET
      *          - EXT-ACUSQL-AUTOCOMMIT  PIC X(01) EXTERNAL.
      *
      * JKC 2024-0501 ADDED EXT-API-WORKERS, EXT-API-SCREEN-DISABLE,
      *         EXT-API-MESS FOR FUTURE USE OF API PROGRAMS.
      *
      ******************************************************************

      ************************************************I***************
      * USED IN CONAME.XFER TO DISPLAY SYSDATE, COMPANY NAME AND
      * THE LOAN MANAGER ON MENUS
      ************************************************I***************
       01  NAM-LST              PIC X(15)     VALUE "SYSDATE CONAME.".
       01  PARA-LST             PIC X(9)      VALUE "PARADATA.".
       01  PARADATA-REL-VER-EXT PIC X(03)     EXTERNAL.
       01  PARADATA.
           03  FILLER PIC X(31) VALUE "THE LOAN MANAGER FINANCE SYSTEM".
           03  FILLER PIC X(02) VALUE " (".
           03  PARADATA-REL-VERSION
                      PIC X(03) VALUE "A##".
           03  FILLER PIC X(06) VALUE ")     ".

      ************************************************I***************

      * REMOVED FROM EOBUF
       01  EXT-ROUTE-BUF      PIC XX        EXTERNAL.


       01  EXT-CONAME-BUF                   EXTERNAL.
           03  EXT-CONAME-DAILY-PASSWORD       PIC 9(4).
           03  EXT-CONAME-RPT-LINES            PIC 99.
      * THIS COMES FROM ENV VARIABLE POSTNAME, IF SPACES, THEN LOGNAME
           03  EXT-CONAME-USER-ID.
               05  EXT-CONAME-USER-PROFILE-CD  PIC X.
               05  EXT-CONAME-USER-LOC         PIC 9(4).
               05  EXT-CONAME-USER-NM.
                   07  EXT-CONAME-USER-GROUP   PIC X(3).
                   07  FILLER                  PIC X(2).
               05  EXT-CONAME-USER-FILLER      PIC X(19).

           03  EXT-CONAME-NAM-DATA.
               05  EXT-CONAME-SYSDATE          PIC X(8).
               05  FILLER REDEFINES EXT-CONAME-SYSDATE.
                   07  EXT-CONAME-SYSDATE-MM   PIC 99.
                   07  FILLER                  PIC X.
                   07  EXT-CONAME-SYSDATE-DD   PIC 99.
                   07  FILLER                  PIC X.
                   07  EXT-CONAME-SYSDATE-YY   PIC 99.
      * USED IN CONAME-XFER AT THE GROUP 03 LEVEL
               05  EXT-CONAME-CONAME1.
                   07  EXT-CONAME-BRANCH-NAME  PIC X(40).

      * USED BY CONAM2.C
           03  EXT-CONAME-CONAME               PIC X(40).

           03  EXT-CONAME-BRNO-BUF             PIC ZZZZ.
           03  EXT-CONAME-NEW-COLL-INQUIRY     PIC X.
      *USED FOR CL/CLMENU NOT TO READ THE BRFILE
           03  EXT-CONAME-CAN-LOANS-FG         PIC X.
      *USED FOR CL/WIMENU NOT TO READ THE BRFILE
           03  EXT-CONAME-BALLOON-LOANS-FG     PIC X.
           03  EXT-MENU-OPTION-FG             PIC XX.

      *=================================================================
      *    EXT-FILPATH -- USED TO SET FULL FILE PATHS (SH/GLOBAL/LOCAL).
      *    GB/SETENV SETS THIS TO "FIL", PUTS FRONT "/" IN IT.
      *    EXAMPLE: /US2/DATA/R1/SC0001/GB
      *=================================================================
       01  EXT-FILPATH-EXTERNAL                EXTERNAL.
           03  EXT-FILPATH                     PIC X(35).
           03  FILLER                          REDEFINES EXT-FILPATH.
               05  EXT-FILPATH-SLASH           PIC X(01).
               05  EXT-FILPATH-FIL             PIC X(34).
           03  FILLER                          REDEFINES EXT-FILPATH.
               05  EXT-FILPATH-BASE            PIC X(09).
               05  EXT-FILPATH-BASEX REDEFINES EXT-FILPATH-BASE.
                   07  FILLER                  PIC X.
                   07  EXT-FILPATH-DIR1    PIC X(3).
                   07  FILLER              PIC X.
                   07  EXT-FILPATH-DIR2    PIC X(4).
               05  FILLER                      PIC X(01).
               05  EXT-FILPATH-MACHINE         PIC X(02).
               05  FILLER                      PIC X(01).
               05  EXT-FILPATH-DATA            PIC X(06).
               05  EXT-FILPATH-DATAX REDEFINES EXT-FILPATH-DATA.
                   07  EXT-FILPATH-STATE       PIC XX.
                   07  EXT-FILPATH-BRANCH      PIC 9999.
                   07  FILLER REDEFINES EXT-FILPATH-BRANCH.
                       09  EXT-FILPATH-BRNO    PIC 9999.
                   07  FILLER REDEFINES EXT-FILPATH-BRANCH.
                       09  EXT-FILPATH-BRANCH1 PIC 9.
                       09  EXT-FILPATH-BRANCH2 PIC 9(3).
               05  EXT-FILPATH-SLASH2          PIC X(01).
               05  FILLER                      PIC X(15).
           03  FILLER                          REDEFINES EXT-FILPATH.
      * EXT-FILPATH-LOCAL EXAMPLE:  /USR/DATA/R1/SC0001/
      * EXT-FILPATH-GLOBAL EXAMPLE: /USR/DATA/R1/
               05  EXT-FILPATH-LOCAL.
                   07  EXT-FILPATH-GLOBAL      PIC X(13).
                   07  FILLER                  PIC X(7).
               05  EXT-FILPATH-FILENAME        PIC X(15).

      *=================================================================
      * EXT-RELEASE -- USING ENVIRONMENT "REL" TO GET RELEASE
      * VERSION. USED FOR DETERMINING IF USING:
      * - VISION FILES [ A32 & A33; LOCAL-TO-GLOBAL; ]
      * - SQL DATABASE [ A34; GLOBAL TABLES; 4GL ]
      * PRIMARY USE IS FOR THE FILE SELECT (FS) COPY MEMBERS; FOR THE
      * 'SELECT XX-FILE ASSIGN TO XX-SELECT-PATH' WHERE THE PATH
      * VARIABLE IS SET WITHIN THE IO COPY MEMBERS.
      *=================================================================
      *01  EXT-RELEASE                         PIC X(03) EXTERNAL.
      *    88  EXT-RELEASE-VISION                  VALUE "A32" "A92".

      *=================================================================
      * ACUSQL CONNECTION VARIABLES -- USED TO FOR CONNECTING TO THE
      * SQL SERVER VIA ACUSQL; POPULATED VIA GB/SETENV
      *=================================================================
       01  EXT-ACUSQL-CONNECT-WORKERS      EXTERNAL.
           03  EXT-ACUSQL-DSN-NAME         PIC X(30).
           03  EXT-ACUSQL-LOGIN            PIC X(30).
           03  EXT-ACUSQL-PSWD             PIC X(30).
           03  EXT-ACUSQL-CONNECT-STAT     PIC X(02).
               88  EXT-ACUSQL-CONNECT-STAT-GOOD    VALUE "00".
               88  EXT-ACUSQL-CONNECT-STAT-BAD     VALUE "  " THRU "//"
                                                         "01" THRU "~~".
       01  EXT-ACUSQL-SERVER-WORKERS       EXTERNAL.
           03  EXT-ACUSQL-DB-NAME          PIC X(50).
           03  EXT-ACUSQL-SERVER-NAME      PIC X(50).
           03  EXT-ACUSQL-VERSION          PIC X(100).

      *=================================================================
       01  EXT-JULIAN-DATE-WORKERS         EXTERNAL.
           03  EXT-JULIAN-BEG              PIC 9(8)   COMP.
           03  EXT-JULIAN-END              PIC 9(8)   COMP.
           03  EXT-JULIAN-CC               PIC 9(2)   COMP.

      *=================================================================
       01 TCLP-WINDOW-HANDLES                  EXTERNAL.
          03 TCLP-MAIN-WINDOW-HANDLE           PIC X(10).

       01  EXT-TCLP-ACUMEM-WORKERS             EXTERNAL.
           03  EXT-TCLP-ACUMEM                 PIC X(01).
               88  EXT-TCLP-ACUMEM-DISABLED    VALUE "0".
               88  EXT-TCLP-ACUMEM-CREATE      VALUE "1".
               88  EXT-TCLP-ACUMEM-UPDATE      VALUE "2".
               88  EXT-TCLP-ACUMEM-VALID       VALUE "0" "1" "2".
           03  EXT-TCLP-ACUMEM-PID             PIC X(10).

       01  EXT-TCLP-TRACE-WORKERS              EXTERNAL.
           03  EXT-TCLP-TRACE                  PIC X(01).
               88  EXT-TCLP-TRACE-DISABLE      VALUE "N".
               88  EXT-TCLP-TRACE-BRANCH       VALUE "B".
               88  EXT-TCLP-TRACE-UPDATE       VALUE "B" "Y".
               88  EXT-TCLP-TRACE-VALID        VALUE "N" "B" "Y".

           03  EXT-TCLP-TRACE-FORM-PATH            PIC X(22).
           03  FILLER REDEFINES EXT-TCLP-TRACE-FORM-PATH.
               05  FILLER                          PIC X(1).
               05  EXT-TCLP-TRACE-FORM-REL         PIC X(7).
               05  FILLER            REDEFINES EXT-TCLP-TRACE-FORM-REL.
                   07  FILLER                      PIC X(4).
                   07  EXT-TCLP-TRACE-LOGUID-REL   PIC X(3).
               05  FILLER                          PIC X(1).
               05  EXT-TCLP-TRACE-FORM-OBJ         PIC X(2).
               05  FILLER                          PIC X(1).
               05  EXT-TCLP-TRACE-FORM-NAM.
                   07  EXT-TCLP-TRACE-FORM-DIRX    PIC X(2).
                   07  FILLER                      PIC X(1).
                   07  EXT-TCLP-TRACE-FORM-PROGX   PIC X(6).
               05  FILLER                          PIC X(1).

           03  EXT-TCLP-TRACE-LEVEL            PIC 9(02).
           03  EXT-TCLP-TRACE-MESS             PIC X(61).
           03  EXT-TCLP-TRACE-SKIP-YN          PIC X(1).


      *=================================================================
      * FOR API PROGRAMS:
      * + NEED SCREEN INTERACTIONS DISABLED (Y)
      * + NEED TO PASS SEND-MESS MESSAGES FROM CALLING PROGRAMS
      *   BACK TO KNOW WHAT TYPE OF ERRORS OCCURED.
      *=================================================================
       01  EXT-API-WORKERS                     EXTERNAL.
           03  EXT-API-SCREEN-DISABLE          PIC X(1).
           03  EXT-API-MESS                    PIC X(61).
           03  FILLER                          PIC X(100).

      * HAD TO ADD AS EXTERNAL, SORRY!
      * ONLY NEEDS TO BE FILLED IN FROM LNCRE2, BULKG2, BULKT2
      * NEEDED AS EXTERNAL CAUSE BICREA IS CALLED FROM LOAN I/O ROUTINES
      * SO I WOULD HAVE HAD TO ADD A NEW FIELD IN A TON OF PROGRAMS
       01  BICREA-CALLED-FROM                  PIC X(6) EXTERNAL.

       01  EXT-F12-OPEN                     PIC X(01) EXTERNAL.

      *=================================================================
      * THIS AREA IS FOR FUTURE USE WHEN NEEDING A NEW EXTERNAL.
      * CANNOT ADD A NEW EXTERNAL FIELD WITHOUT DOING A GLOBAL
      * RECOMPILE.  SO TAKE BYTES NEEDED FROM FILLER IN
      * EXT-FILLER-FUTURE-USE.
      *=================================================================
       01  EXT-FILLER-FUTURE-USE           PIC X(300) EXTERNAL.
      *01  FILLER REDEFINES EXT-FILLER-FUTURE-USE.
      *    03  EXT-??????                  PIC X(1).
      *    03  FILLER                      PIC X(299).
