      ******************************************************************
      *                      (LIBGB/MENU)
      *=================================================================
      * REV:
      * BAH 082696 ADDED BRCPCORP-LIST AND BRCPCORP , INSTANT (368)
      * JTG 110796 ADDED GETENVW & GETENV
      * MJD 031097 ADDED PERFORM BEFORE-SEL : BEFORE ENTERING SELECTION.
      *                  PERFORM AFTER-SEL  : AFTER ENTERING SELECTION.
      *                  FROM NOW ON INSTEAD OF CHANGING LIBGB/MENU FOR
      *                  SPECIFIC MENU'S, CHANGES CAN BE MADE WITHIN THE
      *                  MENU.
      * JTG 042997 ADDED OPTION FOR F12 VVVF12
      * BLV 053097 IF F1 FROM ANY AP MENU, RETURN TO APMENU, NOT LPMENU
      * BLV 062797 MOVE HIDDEN LPCPMU OPTIONS TO LPCPMU.C (BEFORE-SEL)
      * MJD 101697 IN LPIQMU "PURGED LOAN INQUIRY" HAS CHANGED FROM
      *            OPTION 6 TO 8.
      * BLV 111797 ADDED PASSWDW (NEEDED FOR LPMNMU)
      * BAH 022698 REMOVED BRMPURGE
      * MJD 032598 ADDED LIBGB/RLOGINW. ADDED SEPARATE FLAGE FOR
      *            RLOGIN VS. ALTERNATE LOGIN. ADDED BLINKING BRANCH #
      *            IF USER IS REMOTELY LOGGED IN.
      * MJD 061698 REMOVED RLOGIN-YES. ADDED RLOGIN-IS-REMOTE AND
      *            RLOGIN-IS-LOCAL.
      *            ADDED DISPLAY OF "REMOTE" IF USER IS ON A HOST MACHINE.
      * BLV 073098 ADDED LOGUIDW (NEEDED IN SP/SPAQMU)
      * JTG 990111 ADDED OPTIONAL DISPLAY OF 18. REVOLVING STATEMENT RUN
      * JTG 990210 ADDED OPTIONAL DISPLAY OF 19. REVOLVING STATEMENT PRINT
      * BLV 990610 ADDED LOGIC FOR VARIABLE F11 INTERRUPT (TFCMNU)
      * KEC 990908 ADDED LOGIC FOR VARIABLE F11 INTERRUPT (TFCMNU)
      * KEC 990908 ADDED '88' LEVELS FOR STAT  (STAT-GOOD/STAT-BAD)
      *            ADDED '88' LEVELS FOR STAT FOR FUNCTION KEYS
      *            THIS WAS DONE PER THE 'DEVELOPMENT' MEETING (082599)
      * MJD 000605 REPLACED GPENV WITH A HARD CODED VERSION. THE COPY MEMBER
      *            IS A SECTION AND WE CANNOT HAVE A SECTION HERE, IT WILL
      *             DISTURB THE NORMAL FLOW OF THE PROGRAM.
      * BLV 000627 REMOVED XFER & WRFORM OF REVOLVING STATEMENT PROMPTS
      * BLM 110707 UNSTARRED F47INQ-LIST
      * KEC 121019 ADDED FILPATH
      * BLM 130617 FIXING MISSING ":" IN BRNO-LIST-FLASH
      *  CS 130821 PER MEETING REMOVE CRTVARS.DEF/ACUGUI.DEF
      * BLM 150610 ADDED REDEF OF FORM-NAME WITH FORM-DIRX & FORM-PROGX &
      *            ADDED A COPY OF LIBGB/LOGUIDW.CPY
      * BAH 180814 RENAMED CHKSEC-FIELDS TO CHKSEC-FIELDS
      *                    CHKSEC-STAT   TO CHKSEC-STAT
      *                    SAVE-FORM-PATH TO CHKSEC-FORM-PATH
      * JKC 210106 CHANGED SENDMESS-BUF WORKERS TO HAVE MULTIPLE LINES.
      * BAH 2024.0308 REMOVED SENDMESS-BUF WORKERS
      * BAH 2024.0311 REMOVED FORM-STAT
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  3B.
       OBJECT-COMPUTER.  3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(28) VALUE
           "@(#)./LIBGB/MENU1A.CPY S35 >".

       01  ROUTE-BUF              PIC XX   VALUE "00".

       01  MESS                   PIC X(61).
       01  EXT-EOBUF-MESS      REDEFINES MESS
                                  PIC X(61).

      *------------------------------------------------------{ FILPATH }
      *    USED WITH EXT-FILPATH IN SETTING FILE PATHS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    /USR/DATA/R1/XX####
      *    BASE    = /USR/DATA OR /USR/BEOM OR /USR/TEST ETC.
      *    MACHINE = R1 OR R2 OR SH
      *    DATA    = XX#### STATE & BRNO
      *-----------------------------------------------------------------
       01  FILPATH-WORKERS.
           03  FILPATH-BASE            PIC X(09)     VALUE SPACES.
           03  FILLER                  REDEFINES FILPATH-BASE.
               05  FILPATH-BASE-USR    PIC X(05).
               05  FILPATH-BASE-DIR    PIC X(04).
           03  FILPATH-MACHINE         PIC X(02)     VALUE SPACES.
           03  FILPATH-DATA            PIC X(06)     VALUE SPACES.

      *-----------------------------------------------------------------
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       01  HELPSH                 PIC X    VALUE " ".
       01  HELPSH-LIST            PIC X(7) VALUE "PNOTES.".
       01  REMOTE-INFO-BUF.
           03  BRNO-BUF           PIC Z(4).
           03  REMOTE-BUF         PIC X(6).
       01  BRNO-LIST-FLASH        PIC X(17) VALUE "BRNUM:F REMOTE:F.".
       01  BRNO-LIST              PIC X(6)  VALUE "BRNUM.".

      *---------------------------------------------------------{ STAT }
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

      *-----------------------------------------------------------------
       01  FORM-BUF               PIC 99.
       01  FORM-LIST              PIC X(4) VALUE "SEL.".
       01  FORM-PATH.
           03  FILLER             PIC X.
           03  FORM-REL           PIC X(7).
           03  FILLER REDEFINES FORM-REL.
               05  FILLER         PIC X(4).
               05  LOGUID-REL     PIC X(3).
           03  FILLER             PIC X.
           03  FORM-OBJ           PIC XX.
           03  FILLER             PIC X.
           03  FORM-NAM           PIC X(9).
           03  FILLER REDEFINES FORM-NAM.
               05  FORM-DIRX  PIC XX.
               05  FILLER     PIC X.
               05  FORM-PROGX PIC X(6).
           03  FILLER             PIC X.
       01  CHKSEC-FIELDS.
           03  CHKSEC-FORM-PATH     PIC X(22).
           03  CHKSEC-STAT          PIC XX.

       01  PROMPT-BUF             PIC X(30).
       01  PROMPT-LIST            PIC X(9) VALUE "RMT-BR:P.".
       01  PROMPT-MSG             PIC X(26) VALUE
           "11. ALTERNATE BRANCH LOGIN".

       01  SCREEN-LIST            PIC X(7) VALUE "MDIQ:D.".

       COPY "LIBGB/ACUCOBOL_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/RLOGINW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".
