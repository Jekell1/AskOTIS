      * COPYMEMBER: LIBGB/FILEWK
      **************************************************************************
      *
      *  IDENTIFICATION:  LIBGB/FILEWK
      *
      *  DESCRIPTION   :  FILE ERROR WORKERS
      *
      *-----------------------------------------------------------------
      *  REV:
      *  JTG 072996 ADDED SAVE-KEY2 AND CHANGED SAVE-KEY1 TO PIC 9(10)
      *  JTG 092496 ADDED E-KEYX-SSNO  E-KEYX-ACCTNO E-KEYX-OWNBR
      *             TO ALLOW DISPLAY OF BI1-KEY
      *  KEC 112796 ADDED E-KEYX-RX-ACCTNO.
      *  KEC 072298 ADDED '88' LEVELS FOR STAT  (STAT-VALID).
      *  MJD 102998 ADDED E-KEYX-NT-ACCTNO.
      *  JTG 121198 ADDED E-KEYX RT-FIELDS FOR RT2-KEY
      *  KEC 082699 ADDED '88' LEVELS FOR IO-FG (IO-GOOD/IO-BAD)
      *             ADDED '88' LEVELS FOR STAT  (STAT-GOOD/STAT-BAD)
      *             THIS WAS DONE PER THE 'DEVELOPMENT' MEETING (082599)
      *  KEC 083099 ADDED '88' LEVELS FOR IO-FG
      *                                  (IO-ALREADY-OPEN/IO-LOCKED)
      *  KEC 090799 ADDED '88' LEVELS FOR STAT FOR FUNCTION KEYS.
      *  JTG 000508 ADDED LOGIC FOR SECOND ERROR MESSAGE TO INFORM
      *             OPERATOR TO STOP AND CALL SUPPORT          PARADATA #1171
      *  BLV 000619 CHANGED FILLER OF 6 IN ERROR-MSG-STAT TO BE PIGGY-BACK-FLAG,
      *             WHICH GETS USED BY LONPMC & DECLARE
      *  BLV 021022 CHANGED FILLER OF 1 IN ERROR-MSG-STAT TO BE ERROR-LOCK-FG,
      *             WILL TRY TO USE IN DECLARE TO RETURN TO A PROGRAM WITH
      *             A FILE STATUS OF 8 (TO GIVE THE PROGRAM CONTROL) INSTEAD
      *             OF FORCING THE "RECORD LOCKED" WINDOW
      *  KEC 030523 ADDED THE FOLLOWING FIELDS.  ADDED 'LOCAL-FILE-*'
      *             TO MAKE A EASY ROUTINE TO LOAD LOCAL FILE PATHS.
      *             ALSO THE '78' LEVELS TO SET STAT.
      *               01  LOCAL-FILE-DATA-PATH   PIC X(06) VALUE SPACES.
      *               01  LOCAL-FILE-MACHINE     PIC X(02) VALUE SPACES.
      *               01  LOCAL-FILE-STAT        PIC X(02) VALUE SPACES.
      *               78  SET-STAT-GOOD                    VALUE "00".
      *               78  SET-STAT-BAD                     VALUE "XX".
      *  KEC 041118  ADDED THE FOLLOWING FIELDS.  ADDED 'LOCAL-FILE-*'
      *               78  IO---BAD                         VALUE 9.
      *               78  IO---GOOD                        VALUE 0.
      *               78  STAT---GOOD                      VALUE "00".
      *               78  STAT---BAD                       VALUE "XX".
      *  KEC 050817 ADDED THE FOLLOWING FIELDS:
      *               01  EXECUTION-STAT PIC X(02) VALUE SPACES.
      *                   88  EXECUTION-STAT-GOOD  VALUE "00".
      *                   88  EXECUTION-STAT-BAD   VALUE "  " THRU "//"
      *                                                  "01" THRU "~~".
      * BLM 110706 BROUGHT FROM A15, ADDED BACK A FEW ITEMS USED BY NEW
      *            ROUTINES
      * BLM 111213 ADD SYSTEM-STAT (ACU'S SYSTEM CALL NEEDED NUMERIC STAT,
      *            FILE-INFO (REPLACING ACCESS-CALL WITH C$FILEINFO
      * KEC 121017 ADDED FILPATH-WORKERS & EXT-FILPATH-EXTERNAL.
      * BAH 121219 REMOVED SYSTEM-STAT FROM HERE, BELONGS IN SYSTEMW
      * BLM 131009 REMOVED STAT-F1-KEY, ETC
      * BLM 140416 ADDED VIX-SUFFIX & IDX-SUFFIX FOR USE IN LIBGB/RMFILE
      * KEC 151124 ADDED A REDEFINES FILPATH-DATA TO BREAKDOWN THE
      *            STATE & BRANCH...IT MATCHES THE EXTERNAL FIELD
      *            IN CONAME_EXT.
      * BLM 161216 INCREASE SIZE OF ERRMSG-HEAD
      * BAH 190207 ADDED IO-MULTI-FG FOR USE WITH FILES THAT HAVE BEEN
      *            SPLIT SO WE CAN IDENTIFY WHICH FILE HAD BAD I/O
      * JKC 2020-0217 ADDED WORKERS TO GET ACU4GL ERRORS IN ORDER TO
      *               DISPLAY THEM; THERE IS MORE DETAILS FROM ACU4GL.
      * JKC 2020-1023 MOVED '01  SENDMESS-BUF' TO BE A REDEFINES OF
      *              '01  ERRMSG-BUF.' TO SAVE ON SPACE.
      * BAH 2022-0412 REMOVED IO-INVALID AND IO-VALID, LET'S PLEASE STAY
      *                WITH ONE OPTION, IO-BAD AND IO-GOOD. DONT NEED SO
      *                MANY OPTIONS TO LOOK FOR!
      * JKC 2024-0304 REMOVED ACU4GL ERROR DISPLAY LOGIC
      * BAH 2024-0308 REMOVED E-FILEY, SENDMESS-BUF, ERROR-MSG, ERROR-MISC-HEAD,
      *               ERROR-MISC-MSG1, MSG2, MSG3, EXECUTION-STAT,
      *               RELATIVE-RECORD-TERMINATOR, SAVE-KEY, SAVE-KEY2,
      *               FILE-INFO
      ******************************************************************
       01  E-FILE                                    VALUE SPACES.
           03  E-LFN                       PIC X(02).
           03  E-NAME                      PIC X(09).
           03  FILLER                      PIC X(40).

       01  TMMSG-MESS                  PIC X(61).
       01  ERRMSG-BUF.
           03  ERRMSG-HEAD             PIC X(61).
           03  ERRMSG-MSG1             PIC X(61).
           03  ERRMSG-MSG2             PIC X(61).
           03  ERRMSG-MSG3             PIC X(61).
           03  ERRMSG-STAT             PIC X(02).

      *-----------------------------------------------------------------
       01  ERROR-MSG-HEAD.
           03  E-MSG                       PIC X(15).
           03  THIS                        REDEFINES E-MSG.
               05  E-MSG4                  PIC X(04).
               05  FILLER                  PIC X(11).
           03  FILLER                      PIC X(01) VALUE SPACES.

       01  ERROR-MSG-PATH.
           03  FILLER                      PIC X(05) VALUE "FILE=".
           03  E-FILEX                     PIC X(55).
           03  FILLER                      PIC X(01) VALUE SPACES.

       01  ERROR-MSG-KEY.
           03  FILLER                      PIC X(05) VALUE " KEY=".
           03  E-KEYX                      PIC X(55).
           03  FILLER                      PIC X(01) VALUE SPACES.

      
      * PIGGY-BACK-FLAG REALLY NOT USED BUT THIS 01 LEVEL IS MOVED TO
      *       OTHER BUFFERS AT PIC X(61) SO I DIDNT WANT TO REMOVE IT
      * ERROR-LOCK-FG IS ONLY CHECKED IN DECLARE BUT NEVER SET ANYWHERE
      *       AGAIN, DONT WANT TO REMOVE FOR BUFFER SIZE
       01  ERROR-MSG-STAT.
           03  FILLER                      PIC X(05) VALUE "STAT=".
           03  E-STATUS-ERROR.
               05  E-CODE                  PIC X(02).
               05  FILLER                  PIC X(01).
               05  E-SUBCODE               PIC X(02).
           03  PIGGY-BACK-FLAG             PIC X(06) VALUE SPACES.
           03  FILLER                      PIC X(09) VALUE "PROGRAM= ".
           03  E-PROG                      PIC X(35).
           03  ERROR-LOCK-FG               PIC X(01) VALUE SPACES.


       01  EXTENDED-FILE-STAT.
           03  FILE-STAT                   PIC X(02).
           03  FILE-STAT9                  REDEFINES FILE-STAT
                                           PIC 9(04) COMP.
           03  SECONDARY-FILE-STAT         PIC X(02).

       01  FILE-STAT-DISP                            VALUE SPACES.
           03  FILE-STATD1                 PIC X(02).
           03  FILE-STATC                  PIC X(01).
           03  FILE-STATD2                 PIC X(02).

      *------------------------------------------------------{ FILPATH }
      *    /USR/DATA/R1/XX####
      *    BASE    = /USR/DATA OR /USR/BEOM OR /USR/TEST ETC.
      *    MACHINE = R1 OR R2 OR SH
      *    DATA    = XX#### STATE & BRNO
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    EXT-FILPATH-WORKERS USE IS EXTERNAL FOR ALL PROGRAMS.
      *    FILPATH-WORKERS     USE IS INTERNAL TO A GIVEN PROGRAM.
      *
      *    FILPATH-WORKERS ARE USED IN THE 'LOAD' IO OPERATIONS FOR
      *    MAJORITY OF IO COPY MEMBERS.  IN A FILE'S LOAD ROUTINE,
      *    THE USE OF THE 'OVERRIDE OPTION' (??-PATH-OVERRIDE)
      *    WILL DETERMINE WHICH FIELDS TO USE WHEN LOADING THE PATH.
      *
      *    THE 'YES OVERRIDE OPTION' (??-PATH-OVERRIDE = "Y") WILL
      *    CAUSE THE LOAD ROUTINE TO *NOT* CHANGE THE PATH; THE PATH
      *    WAS HARDCODED WITHIN A GIVEN PROGRAM PRIOR TO LOAD/OPEN.
      *
      *    THE 'DEFAULT OVERRIDE OPTION' (??-PATH-OVERRIDE = SPACES)
      *    WILL USE THE EXTERNAL FILPATH WORKERS (EXT-FILPATH).
      *
      *    THE 'BRANCH OVERRIDE OPTION' (??-PATH-OVERRIDE = "B") WILL
      *    USE THE FILPATH-WORKERS.  WHEN USING THIS OPTION THE GIVEN
      *    PROGRAM MUST SET THE FILPATH-WORKERS AFTER THE READ OF THE
      *    BR FILE (READ-BR-FILE OR READ-BR-FILE-NEXT) AND BEFORE THE
      *    USE OF LOAD/OPEN ROUTINES:
      *
      *    MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
      *    MOVE BR-MACHINE                 TO FILPATH-MACHINE.
      *    MOVE BR-DATA-PATH               TO FILPATH-DATA.
      *
      *    ***NOTE*** FILPATH-BASE COULD BE SET TO THE ARCHIVE
      *    DIRECTORY (/USR/ARCH) INSTEAD OF EXT-FILPATH-BASE
      *    IN SOME CASES.
      *
      *LOAD-??-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *    IF ( ??-PATH-OVERRIDE = "Y" ) 
      *       NEXT SENTENCE
      *    ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *    IF ( ??-PATH-OVERRIDE = SPACES ) 
      *       MOVE EXT-FILPATH-BASE        TO ??-ALL-BASE
      *       MOVE EXT-FILPATH-MACHINE     TO ??-ALL-MACHINE
      *       MOVE EXT-FILPATH-DATA        TO ??-ALL-DATA
      *       MOVE ??-ALL-PATH             TO ??-PATH
      *    ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *    IF ( ??-PATH-OVERRIDE = "B" ) 
      *       MOVE FILPATH-BASE            TO ??-ALL-BASE
      *       MOVE FILPATH-MACHINE         TO ??-ALL-MACHINE
      *       MOVE FILPATH-DATA            TO ??-ALL-DATA
      *       MOVE ??-ALL-PATH             TO ??-PATH.
      *
      *-----------------------------------------------------------------
       01  FILPATH-WORKERS.
           03  FILPATH-BASE            PIC X(09)     VALUE SPACES.
           03  FILLER                  REDEFINES FILPATH-BASE.
               05  FILPATH-BASE-USR    PIC X(05).
               05  FILPATH-BASE-DIR    PIC X(04).
           03  FILPATH-MACHINE         PIC X(02)     VALUE SPACES.
           03  FILPATH-DATA            PIC X(06)     VALUE SPACES.
           03  FILLER                  REDEFINES FILPATH-DATA.
               05  FILPATH-STATE       PIC X(02).
               05  FILPATH-BRANCH      PIC 9(04).
               05  FILLER              REDEFINES FILPATH-BRANCH.
                   07  FILPATH-BRNO    PIC 9(04).
               05  FILLER              REDEFINES FILPATH-BRANCH.
                   07  FILPATH-BRANCH1 PIC 9(01).
                   07  FILPATH-BRANCH2 PIC 9(03).

      * USED FOR FILES THAT WERE BROKE INTO MULTIPE FILES LIKE BY AND SP
       01  IO-MULTI-FG                     PIC X  VALUE SPACES.

       01  IO-FG                           PIC 9(01) VALUE ZEROES.
           88  IO-ALREADY-OPEN                 VALUE 7.
           88  IO-BAD                          VALUE 1 THRU 9.
           88  IO-GOOD                         VALUE 0.
           88  IO-LOCKED                       VALUE 8.
       01  IO-STATUS                       PIC 9(01) VALUE ZEROES.
           88 IO-AT-END                        VALUE 9.
           88 IO-NOT-AT-END                    VALUE 0.
           88 IO-INVALID-KEY                   VALUE 9.
           88 IO-NOT-INVALID-KEY               VALUE 0.

      *01  LOCAL-FILE-DATA-PATH            PIC X(06) VALUE SPACES.
      *01  LOCAL-FILE-MACHINE              PIC X(02) VALUE SPACES.
      *01  LOCAL-FILE-STAT                 PIC X(02) VALUE SPACES.
      *    88  LOCAL-FILE-STAT-GOOD            VALUE "00".
      *    88  LOCAL-FILE-STAT-BAD             VALUE "  " THRU "//"
      *                                              "01" THRU "~~".

       01  LOCK-STATUS-93-RETRIES          PIC 9(01) VALUE ZEROES.
       01  LOCK-STATUS-99-RETRIES          PIC 9(02) VALUE ZEROES.
       01  VIX-SUFFIX                      PIC X(4)  VALUE X"2E766978".
       01  IDX-SUFFIX                      PIC X(4)  VALUE X"2E696478".

       78  STAT---GOOD                         VALUE "00".
       78  STAT---BAD                          VALUE "XX".
       01  STAT                            PIC X(02) VALUE SPACES.
           88  STAT-VALID                      VALUE "00".
           88  STAT-GOOD                       VALUE "00".
           88  STAT-BAD                        VALUE "  " THRU "//"
                                                     "01" THRU "~~".

      *- - - - - - - - - - - - - - - - - - - - - - - -{ FUNCTION KEYS  }
      *    88  STAT-F1-KEY                     VALUE "10".
      *    88  STAT-F2-KEY                     VALUE "12".
      *    88  STAT-F3-KEY                     VALUE "16".
      *    88  STAT-F4-KEY                     VALUE "18".
      *    88  STAT-F5-KEY                     VALUE "1:".
      *    88  STAT-F6-KEY                     VALUE "1<".
      *    88  STAT-F7-KEY                     VALUE "1>".
      *    88  STAT-F8-KEY                     VALUE "1 ".
      *    88  STAT-F9-KEY                     VALUE "1(".
      *    88  STAT-F10-KEY                    VALUE "1)".
      *    88  STAT-F11-KEY                    VALUE "1!".
      *    88  STAT-F12-KEY                    VALUE "1#".
      *    88  STAT-DOWN-KEY                   VALUE "1D".
      *    88  STAT-ENTER-KEY                  VALUE "00".
      *    88  STAT-HOME-KEY                   VALUE "1H".
      *    88  STAT-LEFT-KEY                   VALUE "1L".
      *    88  STAT-PAGE-UP-KEY                VALUE "1P".
      *    88  STAT-PAGE-DOWN-KEY              VALUE "1N".
      *    88  STAT-RIGHT-KEY                  VALUE "1R".
      *    88  STAT-UP-KEY                     VALUE "1U".

       01  STAT-99 REDEFINES STAT          PIC 9(02).

       01  WINDOWS-OPEN                    PIC 9(01) VALUE ZEROES.

       COPY "LIBCL/ERRLOGW.CPY".

      ******************************************************************
      *
      *                      ***** N O T E *****
      *
      *-----------------------------------------------------------------
      *   WHEN MODIFYING FILEWK, DO NOT CHANGE THE POSITION OF 'MESS'.
      *   (IT MUST BE THE LAST DATA-NAME SO THAT IT MAY BE REDEFINED.)
      ******************************************************************
       01  MESS                            PIC X(61) VALUE SPACES.
