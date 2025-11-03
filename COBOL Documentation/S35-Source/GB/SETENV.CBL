       IDENTIFICATION DIVISION.
       PROGRAM-ID.        SETENV.
      *********************************************************************
      *
      *   SET ENVIRONMENT
      *             - ENVIRONMENT "GLOBENV"      (GEC-ENV-CONAME-BUF)
      *             - ENVIRONMENT "GPENV" (GP-REC)
      *
      * REV:
      * KEC 082697 ADDED BR-GIFTNET TO GEC-ENV-CONAME-BUF
      * JTG 092597 ADDED BR-BALLOON-LOANS-FG, CASTLE FINANCE
      * JTG 022698 ADDED LOGIC TO ACCEPT 'SYSDATE_YYMMDD' FROM ENVIRONMENT
      *  MG  991008 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * MJD 000524 CENTER JUSTIFIED GEC-CONAME AND GEC-CONAME1 SO THAT
      *            IT WILL DISPLAY NICE ON ALL THE FORMS.
      * BAH 001013 DISPLAY MESSAGE IF LOGGED IN AS "ROOT"
      * JTG 040220 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      * BAH 051219 CHECK ENV VARIABLE PRTSCR AND PRTSCR_OPT1 AND SET
      *            GEC-PRU (DEFAULT PRINTER) IF FOUND...SUPPORT PR# 2653
      * MJD 121217 ADDED VDU-FIELD-PROMPT-DEFAULT.  TESTING  /MJD
      * BAH 130624 COMMENTED OUT ROOT LOGIN CHECK
      * MJD 130930 CHANGED DEFAULT PROMPT FROM " " TO "_" BASED ON DISCUSSION
      *            FROM OUT WEEKELY MEETING.
      * BAH 131025 MOVED EXT-FILPATH TO HERE FROM TCLP, NEED TO CALL THIS
      *            PROGRAM FROM EOCRON ONLY PROGRAMS AND NEEDED FILE
      *            PATHS SET, NOT SURE WHY IT WAS IN TCLP ANYHOW
      * MJD 140502 ADDED EXT-WIN WORKERS FOR WINDOW SIZE AND OFFSET IN VERYANT
      * MJD 140603 ADDED EXT- WORKERS FOR COLOT AND CONTROL VALUES IN VINDOWS.
      * KEC 141118 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *            ADDED STAT CHECK AND ERROR MESSAGE ON GETENV CALL
      *            FOR "FIL".
      *
      * KEC 2018-0926 ADDED EXT-RELEASE FOR A32 & A34 (4GL).
      * KEC 2019-1105 THE A15-TO-A34 UPGRADE (UP1534) FOR WORLD MAY TAKE
      *               LONGER THAN ONE WEEKEND.  SO NEED A WAY TO
      *               DETERMINE WHAT IS A VISION FILE AND SQL TABLE.
      *               ADDED LOAD-EXT-IO-FILE-TYPE SECTION TO HELP
      *               IDENTIFY WHAT IS VISION FILE AND SQL TABLE.
      *
      * KEC 2019-1106 MADE A CHANGE TO SEARCH THE XCBLCONFIG FOR
      *               HOST OPTIONS FOR 'ODBC' AND 'MSSQL'.
      * JKC 2020-0612 REMOVED EXT-RELEASE SINCE NOT USED.
      *         COMMENTED OUT THE FOLLOWING SINCE THEY WERE NOT
      *         BEING USED; COMMENTED OUT IN CONAME_EXT.CPY:
      *            EXT-CONAME-AUTO-INS-EARN
      *            EXT-CONAME-BRGIFTNET
      *            EXT-CONAME-EXTRACT-AUTO-UPD
      *
      * JKC 2020-1019 CHANGED SETTING GB-RPT-LINES TO DEFAULT TO 60
      *          IF CANNOT FIND GB-REC FOR GIVEN BRANCH NUMBER.
      *
      * JKC 2020-1023 ADDED '01  WS-REL-ENVIRONMENT.' WHICH IS USED TO
      *          POPULATE PARADATA-REL-VERSION.
      * JKC 2020-1023 CHANGED FOR WS-REL-ENVIRONMENT TO POPULATE
      *          PARADATA-REL-VER-EXT.
      *
      * JKC 2021-0114 ADDED LOGIC FOR SETTING ACUSQL CONNECTION WORKERS.
      *               + LOAD-EXT-ACUSQL-WORKERS
      *               + EXT-ACUSQL-ODBC-NAME
      *               + EXT-ACUSQL-LOGIN
      *               + EXT-ACUSQL-PSWD
      *
      * JKC 2021-0114 ADDED 'PERFORM SQL-CONNECT' TO CONNECT-ACUSQL
      *          SECTION TO CONFIRM THAT CONNECTION VARIABLES ARE GOOD.
      *
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS 
      *
      * JKC 2022-0519 FIXED AN ISSUE ON CONNECTING TO SQL SERVER; 
      *          CANNOT RECONNECT TO SQL SERVER WITH CONNECTION ALREADY
      *          MADE WITHIN WINDOWS BUT WORKS FOR LINUX.
      * BAH 2022.0627 RENAMED EXT-ACUSQL-ODBC-NAME TO EXT-ACUSQL-DSN-NAME
      *
      * JKC 2023-0901 ADDED TCLP-ACUMEM-MODULE SECTION FOR LOGGING
      *          OTIS MEMORY USAGE; CALLS TO CL/ACUMEM
      *
      * JKC 2023-1114 REMOVED LOAD-EXT-IO-FILE-TYPE SECTION; NO LONGER
      *               NEEDED; FORCING ALL "S" TO EXT-IO-FILE-TYPE
      *               (ACU4GL ITEM) TILL WE CAN REMOVE IT FROM USE IN
      *               ALL IO COPY MEMBERS.
      *               ADDED LOAD-ACUSQL-CONFIG SECTION; TO BE USED TO
      *               READ THRU THE XCBLCONFIG FILE TO GET ACUSQL
      *               SETTINGS SO THAT WE CAN SET OTIS/COBOL SETTINGS.
      *
      * JKC 2024-0113 SETTING EXT-JULIAN-DATE-WORKERS; SET THESE AS
      *          EXTERNAL VARIABLES INSTEAD OF WORKERS WITHIN
      *          LIBGB/DATERW WHICH MAKES IT EASIER TO CHANGE THE
      *          'SLIDING WINDOW' IN THE FUTURE; REDUCES NEEDING TO
      *          RECOMPILE 110 PROGRAMS PLUS THOSE WITHIN 56 COPY
      *          MEMBERS THAT USE THESE DATERW WORKERS; USING THESE
      *          THREE EXTERNAL FIELDS IT ONLY REQUIRES RECOMPILE
      *          ONE PROGRAM (GB/SETENV).
      *
      * JKC 2024-0124 ADDED OTIS TRACE LOGGING LOGIC
      * JKC 2024-0131 REMOVED SETTING THE EXTERNAL OTIS TRACE WORKERS;
      *          THEY ARE NOW BEING SET IN GB/TCLP.
      * BAH 2024.0404 REMOVED THE FOLLOWING:
      *               - EXT-CONAME-CONAME1
      *               - EXT-CONAME-CONAME.
      *               - VDU-FIELD-PROMPT.
      *               - VDU-WIN-L-OFFSET
      *               - VDU-WIN-C-OFFSET
      *               - VDU-WIN-W-MOD
      *               - VDU-WIN-H-MOD
      *               - VDU-WCTRL-TCLP
      *               - VDU-WCTRL-FWIN
      *               - VDU-WCTRL-SCAN
      *               - VDU-WCTRL-EOINIT
      *               - EXT-WCTRL-005
      *               - VDU-WCOLOR-TCLP
      *               - VDU-WCOLOR-FWIN
      *               - VDU-WCOLOR-SCAN
      *               - VDU-WCOLOR-EOINIT
      *               - EXT-WCOLOR-005
      *               - VDU-LABEL-OFFSET
      *    
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.  INITIALIZE THEM.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  3B.
       OBJECT-COMPUTER.  3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------------------------------------------------
      * XCBLCONFIG FILE - /USR/A##/SH/XCBLCONFIG
      *-----------------------------------------------------------------
      *KC  SELECT XCBLCONFIG-FILE ASSIGN TO XCBLCONFIG-PATH
      *KC                 ORGANIZATION LINE SEQUENTIAL
      *KC                 ACCESS SEQUENTIAL
      *KC                 LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *KC                 FILE STATUS FILE-STAT.

       DATA DIVISION.
       FILE SECTION.

      *-----------------------------------------------------------------
      * XCBLCONFIG FILE - /USR/A##/SH/XCBLCONFIG
      *-----------------------------------------------------------------
      *FD  XCBLCONFIG-FILE
      *    RECORD IS VARYING IN SIZE FROM 0 TO 100 CHARACTERS
      *    LABEL RECORDS ARE STANDARD.
      *01  XCBLCONFIG-REC              PIC X(100).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GB/SETENV S35 03/17/25-14:34:34 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".

      *01  XCBLCONFIG-PATH.
      *    03  FILLER                   PIC X(1)   VALUE "/".
      *    03  XCBLCONFIG-PATH-REL      PIC X(7)   VALUE "USR/A34".
      *    03  FILLER                   PIC X(4)   VALUE "/SH/".
      *    03  XCBLCONFIG-PATH-FILENAME PIC X(10)  VALUE "XCBLCONFIG".

       COPY "LIBGB/CONAME_EXT.CPY".

       01  ERRCD                       PIC X VALUE " ".
      *01  D-DEF-PRU         PIC X(7)     VALUE " ".
      *01  D-DEF-PRUX        REDEFINES D-DEF-PRU.
      *    03  D-DEF-TYPE    PIC X.
      *    03  FILLER        PIC X(4).
      *    03  D-DEF-PRINTER PIC 99.

       01  D-DATE1           PIC X(10)     VALUE " ".
       01  D-DATE            REDEFINES D-DATE1.
           03  D-YY          PIC 99.
           03  D-MM          PIC 99.
           03  D-DD          PIC 99.
       01  WS-DATE           REDEFINES D-DATE1.
           03  WS-D1    PIC 9.
           03  WS-D2    PIC 9.
           03  WS-D3    PIC 9.
           03  WS-D4    PIC 9.
           03  WS-D5    PIC 9.
           03  WS-D6    PIC 9.

       01  WS-REL-ENVIRONMENT.
           03  FILLER          PIC X(04) VALUE "USR/".
           03  WS-REL-VERSION  PIC X(03) VALUE "A##".

       01  WS-SUM       PIC 99.
       01  WS-PROD      PIC 9(6).
       01  WS-ANSWER   REDEFINES WS-PROD.
           03  FILLER   PIC 99.
           03  WS-ANS2  PIC 9999.
       01  TM-DATE.
           03  TM-MM         PIC 99.
           03  FILLER        PIC X         VALUE "/".
           03  TM-DD         PIC 99.
           03  FILLER        PIC X         VALUE "/".
           03  TM-YY         PIC 99.

      *01  HOLD-SQLCA        PIC X(208)    VALUE SPACES.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SQL                    PIC X(200).
       01  RET-DB-NAME                 PIC X(50).
       01  RET-SERVER-NAME             PIC X(50).
       01  RET-VERSION                 PIC X(100).
       01  MISC-RETURN-CODE            PIC S9(4)   COMP-5.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       COPY "LIBGB/UNAMEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
      *DECLARATIVES.
      *DECLARE-ERROR SECTION.
      *    USE AFTER ERROR PROCEDURE ON
      *       XCBLCONFIG-FILE.
      *COPY "LIBGB/DECLARE.CPY".
      *    PERFORM CLOSE-GP-FILE.
      *    PERFORM CLOSE-GB-FILE.
      *    PERFORM CLOSE-BR-FILE.
      *    CLOSE
      *       XCBLCONFIG-FILE.
      *COPY "LIBGB/DECLAR2.CPY".

      ************************************
       MAIN-ROUTINES SECTION.
      ************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * INITIALIZE API WORKERS
           MOVE SPACES TO EXT-API-WORKERS.
           MOVE "N"    TO EXT-API-SCREEN-DISABLE.     *> ENABLE SCREENS

      * MEMORY LOGGING
           PERFORM TCLP-ACUMEM-MODULE.       

      * GET OTIS VERSION.
      
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE "ERROR: ENVIRONMENT VARIABLE 'REL' NOT FOUND" TO MESS
              PERFORM SEND-MESS
              MOVE "USR/A??" TO GETENV-BUF.

           MOVE GETENV-BUF     TO WS-REL-ENVIRONMENT.
           MOVE WS-REL-VERSION TO PARADATA-REL-VER-EXT.
    
      * SET EXT-PATH FOR GLOBAL/LOCAL FILE PATHS
      
           MOVE "FIL"           TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-BAD )
              MOVE "ERROR: ENVIRONMENT VARIABLE 'FIL' NOT FOUND" TO MESS
              PERFORM SEND-MESS.

      D    STOP GETENV-BUF.
           MOVE "/"             TO EXT-FILPATH.
           MOVE GETENV-BUF      TO EXT-FILPATH-FIL.
           MOVE "/"             TO EXT-FILPATH-SLASH2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CONNECT TO SQL VIA ACUSQL BY SETTING EXTERNAL VARIABLES;
      *    LOAD ADDITIONAL SETTINGS FOR ACUSQL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM CONNECT-ACUSQL.
      *JKC PERFORM LOAD-ACUSQL-CONFIG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETTING EXT-JULIAN-DATE-WORKERS; SET THESE AS EXTERNAL
      *    VARIABLES INSTEAD OF WORKERS WITHIN LIBGB/DATERW WHICH
      *    MAKES IT EASIER TO CHANGE THE 'SLIDING WINDOW' IN THE
      *    FUTURE; REDUCES NEEDING TO RECOMPILE 110 PROGRAMS PLUS THOSE
      *    WITHIN 56 COPY MEMBERS THAT USE THESE DATERW WORKERS; USING
      *    THESE THREE EXTERNAL FIELDS IT ONLY REQUIRES RECOMPILE
      *    ONE PROGRAM (GB/SETENV).                       JKC 2023-1220
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 19500101  TO EXT-JULIAN-BEG.           
           MOVE 20491231  TO EXT-JULIAN-END.           
           MOVE 50        TO EXT-JULIAN-CC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   DO *NOT* ADD ANY TYPE OF COMP FIELDS TO GP-REC (KEY 1);
      *   SET ENVIRONMENT (GB/SETENV) WILL NOT WORK WITH IT.
      *   LOOK AT THE SHARED DRIVE FOR 'COBOL_SET_ENV_COMP.DOCX 
      *   FOR MORE INFORMATION.                     BH & KC 2017-1004
      *
      *   SET GLOBAL ENVIRONMENT VARIABLE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO GP-KEY.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.
           IF IO-BAD
              MOVE "GLOBAL PARAMETER RECORD MISSING" TO MESS
              PERFORM SEND-MESS
           ELSE
              SET ENVIRONMENT "GPENV" TO GP-REC.

      **********************************************************
      *    SET UP CONAME.EXT ENVIRONMENT WORKERS
      **********************************************************
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "     NO GBFILE RECORD FOUND" TO GB-COMPANY-NAME
              MOVE 60 TO GB-RPT-LINES.

      * ROUTE BUF FOR BATCH VS MENU DRIVEN
           MOVE "00" TO EXT-ROUTE-BUF.

      * GET NO OF REPORT LINES:
           MOVE GB-RPT-LINES TO EXT-CONAME-RPT-LINES.

      * GET CORPORATE NAME:
           MOVE GB-COMPANY-NAME TO EXT-CONAME-CONAME1
                                   EXT-CONAME-CONAME.

           CALL "C$JUSTIFY" USING EXT-CONAME-CONAME1 "C".
           CALL "C$JUSTIFY" USING EXT-CONAME-CONAME  "C".

      * SETUP SYSTEM DATE:
           MOVE "SYSDATE_YYMMDD" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-GOOD
              MOVE GETENV-BUF TO D-DATE1
           ELSE
              ACCEPT D-DATE1 FROM DATE.
           MOVE D-MM TO TM-MM.
           MOVE D-DD TO TM-DD.
           MOVE D-YY TO TM-YY.
           MOVE TM-DATE TO EXT-CONAME-SYSDATE.

      * GET DAILY PASSWORD:
           COMPUTE WS-SUM =
               WS-D1 + WS-D2 + WS-D3 + WS-D4 + WS-D5 + WS-D6.
           COMPUTE WS-PROD = WS-SUM * WS-SUM * WS-SUM.
           MOVE WS-ANS2 TO EXT-CONAME-DAILY-PASSWORD.

      * GET USER ID:
           PERFORM UNAME-CALL.
           MOVE UNAME-BUF TO EXT-CONAME-USER-ID.
           MOVE " " TO EXT-CONAME-USER-FILLER.

           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SETENV"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           STRING "EXTERNAL USERID = ", EXT-CONAME-USER-ID
                  DELIMITED BY "  "      INTO EXT-TCLP-TRACE-MESS.
           PERFORM TRACE-SEND-MESS.

      * GET BRANCH INFORMATION:
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE "N" TO EXT-CONAME-NEW-COLL-INQUIRY
              MOVE "N" TO EXT-CONAME-CAN-LOANS-FG
           ELSE
              MOVE SPACES              TO EXT-CONAME-CONAME1
              MOVE BR-NAME             TO EXT-CONAME-BRANCH-NAME
              CALL "C$JUSTIFY" USING EXT-CONAME-CONAME1 "C"
              MOVE BR-NO               TO EXT-CONAME-BRNO-BUF
              MOVE BR-NEW-COLL-INQUIRY TO EXT-CONAME-NEW-COLL-INQUIRY
              MOVE BR-CAN-LOANS-FG     TO EXT-CONAME-CAN-LOANS-FG
              MOVE BR-BALLOON-LOANS-FG TO EXT-CONAME-BALLOON-LOANS-FG.
           
      * SETENV IS CALLED FROM F10 (WIMENU/SIMENU.  NEED TO MAKE SURE
      *IT DOES NOT GET RESET WHILE F12 IS ALREADY OPEN
           IF NOT (EXT-F12-OPEN = "Y")
              MOVE "N" TO EXT-F12-OPEN.

           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    C O N N E C T - A C U S Q L
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : EXT-ACUSQL-CONNECT-WORKERS
      *       EXT-ACUSQL-SERVER-WORKERS
      * DESC: SET ALL EXT-ACUSQL-CONNECT WORKERS FOR ACUSQL THEN 
      *       CONNECT TO THE SQL SERVER VIA ACUSQL.
      *=================================================================
      * EXT-ACUSQL-DSN-NAME = ASSIGN TO THE NAME OF THE INTENDED
      *                        ODBC DATA
      *
      * EXT-ACUSQL-LOGIN = SPECIFY THE USERNAME OF AN AUTHORIZED
      *                    DATABASE USER.
      *
      * EXT-ACUSQL-PSWD = SPECIFY THE VALUE OF THE PASSWORD ASSOCIATED
      *                   WITH THE VALUE OF ACUSQL_USER.
      *
      * EXT-ACUSQL-DB-NAME = SQL DATABASE NAME
      *
      * EXT-ACUSQL-SERVER-NAME = SQL SERVER NAME
      *
      * EXT-ACUSQL-VERSION = SQL VERSION
      *
      *=================================================================
      * NOTE: THIS SECTION WORKED DIFFERENTLY BETWEEN LINUX & WINDOWS.
      *       JEFF POTTS WAS ERRORING IN THE WINDOWS VERSION
      *           CODE: 1-
      *           STATE: 08002
      *           ERRML: 0025
      *           ERRPC: 0029
      *           ERRMC: DUPLICATE CONNECTION NAME
      *       NOW CHECKING IF CONNECTION IS ALREADY ESTABLISH (GOOD).
      *       IF GOOD, CLEAR WORKERS THEN SET CONNECTION WORKER BACK
      *       TO BE GOOD; THEN ALLOWED NORMAL LOGIC TO CONTINUE AS IS.
      ******************************************************************
       CONNECT-ACUSQL SECTION.

           IF ( EXT-ACUSQL-CONNECT-STAT-GOOD )
              MOVE SPACES      TO EXT-ACUSQL-CONNECT-WORKERS
              MOVE STAT---GOOD TO EXT-ACUSQL-CONNECT-STAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OTIS_DSN_NAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE SPACES TO MESS
              STRING "ERROR: ENVIRONMENT VARIABLE NOT FOUND - ",
                     "OTIS_DSN_NAME"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE GETENV-BUF TO EXT-ACUSQL-DSN-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OTIS_ACUSQL_LOGIN" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE SPACES TO MESS
              STRING "ERROR: ENVIRONMENT VARIABLE NOT FOUND - ",
                     "OTIS_ACUSQL_LOGIN"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE GETENV-BUF TO EXT-ACUSQL-LOGIN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OTIS_ACUSQL_PSWD" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE SPACES TO MESS
              STRING "ERROR: ENVIRONMENT VARIABLE NOT FOUND - ",
                     "OTIS_ACUSQL_PSWD"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE GETENV-BUF TO EXT-ACUSQL-PSWD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SQL-CONNECT.   *> DEFINED IN LIBGB/CONNECT_SQL.CPY

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GET SQL SERVER INFORMATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "** INVALID SSP CALL **"   TO EXT-ACUSQL-DB-NAME
                                              EXT-ACUSQL-SERVER-NAME
                                              EXT-ACUSQL-VERSION.

           PERFORM START-MISC-CSR.

           IF ( SQLCODE NOT = 0 )
              GO TO CONNECT-ACUSQL-EXIT.

           PERFORM READ-MISC-CSR-NEXT.

           IF ( SQLCODE NOT = 0 )
              GO TO CONNECT-ACUSQL-EXIT.

           PERFORM CLOSE-MISC-CSR.

           MOVE RET-DB-NAME                TO EXT-ACUSQL-DB-NAME.
           MOVE RET-SERVER-NAME            TO EXT-ACUSQL-SERVER-NAME.
           MOVE RET-VERSION                TO EXT-ACUSQL-VERSION.
 
      *=================================================================
       CONNECT-ACUSQL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    T C L P - A C U M E M - M O D U L E 
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : EXT-TCLP-ACUMEM
      *       EXT-TCLP-ACUMEM-PID
      * DESC: ACUCOBOL MEMORY LOGGING.  CHECK IF `TCLP_ACUMEM`
      *       ENVIRONMENT VARIABLE IS "Y" (ENABLE ACUMEM).  IF SO, THEN
      *       CALL TO CL/ACUMEM WITH EXT-TCLP-ACUMEM TO "1" (CREATE) TO
      *       CREATE ACUMEM FILE USING THE `TCLP_ACUMEM.SH` SCRIPT TO
      *       FIND THE PROCESS ID (PID) FOR THIS OTIS SESSION (RUNCBL).
      *       IF PID FOUND THEN SET EXT-TCLP-ACUMEM-PID WITH IT, AND
      *       EXT-TCLP-ACUMEM TO "2" (UPDATE).
      ******************************************************************
       TCLP-ACUMEM-MODULE SECTION.

           IF ( EXT-TCLP-ACUMEM-DISABLED ) OR
              ( EXT-TCLP-ACUMEM-UPDATE   )
              GO TO TCLP-ACUMEM-MODULE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GET TCLP_ACUMEM ENVIRONMENT VARIABLE; SHOULD BE "Y" OR "N"
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TCLP_ACUMEM" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD             ) OR
              ( GETENV-BUF NOT = "Y" )
      * DISABLE ACUMEM
              MOVE "0"    TO EXT-TCLP-ACUMEM 
              MOVE SPACES TO EXT-TCLP-ACUMEM-PID
              GO TO TCLP-ACUMEM-MODULE-END.

      * CREATE ACUMEM
           MOVE "1" TO EXT-TCLP-ACUMEM. 

           CALL "ACUMEM" USING FORM-PATHNAME.
           CANCEL "ACUMEM".

           IF ( NOT EXT-TCLP-ACUMEM-VALID )
      * DISABLE ACUMEM
              MOVE "0" TO EXT-TCLP-ACUMEM
              MOVE SPACES TO EXT-TCLP-ACUMEM-PID.

      *=================================================================
       TCLP-ACUMEM-MODULE-END.

           MOVE FORM-PATHNAME              TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SETENV"                   TO EXT-TCLP-TRACE-FORM-PROGX.
           STRING "EXTERNAL ACUMEM = ", EXT-TCLP-ACUMEM
                  DELIMITED BY "  "      INTO EXT-TCLP-TRACE-MESS.
           PERFORM TRACE-SEND-MESS.

      *-----------------------------------------------------------------
       TCLP-ACUMEM-MODULE-EXIT.
           EXIT.

      **************************************
       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************
       MISC-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/UNAME.CPY".
       COPY "LIBGB/GETENV.CPY".

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.

       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGPRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

      *=================================================================
       START-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "START ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              PERFORM CLOSE-MISC-CSR.

           MOVE "SELECT DB_NAME(),@@SERVERNAME,@@VERSION" 
              TO PASS-SQL.

           EXEC SQL
            DECLARE MISC_CURSOR CURSOR FOR
                :MISC-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN MISC_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

           MOVE STAT---GOOD TO MISC-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-MISC-CSR-NEXT.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "READ ERROR"     TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH MISC_CURSOR INTO  
                   :RET-DB-NAME,
                   :RET-SERVER-NAME, 
                   :RET-VERSION
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       CLOSE-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "CLOSE ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE MISC_CURSOR
              END-EXEC
              MOVE STAT---BAD TO MISC-CURSOR-STAT.

      *=================================================================
      *OPEN-XCBLCONFIG-FILE.
      *
      *    PERFORM OPEN-IT.
      *    MOVE XCBLCONFIG-PATH TO E-FILE.
      *    OPEN INPUT XCBLCONFIG-FILE.
      *
      *    IF ( IO-LOCKED       ) GO TO OPEN-XCBLCONFIG-FILE.
      *    IF ( IO-ALREADY-OPEN ) CLOSE XCBLCONFIG-FILE
      *                           GO TO OPEN-XCBLCONFIG-FILE.
      *
      *-----------------------------------------------------------------
      *READ-XCBLCONFIG-FILE.
      *
      *    PERFORM READ-IT.
      *    MOVE XCBLCONFIG-PATH TO E-FILE.
      **   MOVE XCBLCONFIG-KEY TO E-KEYX.
      *    READ XCBLCONFIG-FILE.
      *
      *    IF ( IO-LOCKED ) GO TO READ-XCBLCONFIG-FILE.
      *
      *-----------------------------------------------------------------
      *READ-XCBLCONFIG-FILE-NEXT.
      *
      *    PERFORM READ-IT.
      *    MOVE XCBLCONFIG-PATH TO E-FILE.
      **   MOVE XCBLCONFIG-KEY TO E-KEYX.
      *    READ XCBLCONFIG-FILE NEXT.
      *
      *    IF ( IO-LOCKED ) GO TO READ-XCBLCONFIG-FILE-NEXT.
      *
      *-----------------------------------------------------------------
      *CLOSE-XCBLCONFIG-FILE.
      *
      *    PERFORM CLOSE-IT.
      *    CLOSE XCBLCONFIG-FILE.

      *==================================[ NEEDS TO BE LAST IN SECTION ]
       COPY "LIBGB/FERRORS.CPY".

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROGRAM.
       EXIT-PROG.
           EXIT PROGRAM.
