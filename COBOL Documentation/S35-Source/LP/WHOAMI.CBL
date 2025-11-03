       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WHOAMI.
       AUTHOR.          MJD.
       DATE-WRITTEN.    041696
      *****************************************************************
      *                    (LP/WHOAMI)
      *   CALLED FROM LPMENU
      *
      *   DESCRIPTION: DETERMINES WHETHER A PERSON IS LOGGED IN
      *                FROM ANOTHER MACHINE OR HAS LOGGED IN VIA AN
      *                "SU" COMMAND. USED TO DETERMINE IF THE USER
      *                IS ALLOWED TO DO A LOGIN TO ANOTHER BRANCH.
      *
      *   IN:   [NO INPUT]
      *   OUT:  RLOGIN-FG    : "N" NOT ELIGIBLE FOR OPTION
      *                        "Y" ELIGIBLE FOR OPTION.
      *                        (BASED UPON GP-ALLOW-RLOGINS)
      *         RLOGIN-ALLOW-REMOTE : "Y" ALLOW USERS TO DO REMOTE LOGIN
      *                                ACROSS MACHINES
      *         RLOGIN-ALLOW-LOCAL : "Y" ALLOW USERS TO DO REMOTE LOGIN
      *                                ACROSS BRANCHES
      *         RLOGIN-HOST  : HOST FROM "UNAME -N"
      *                            (FIRST CHAR ONLY)
      *         RLOGIN-USERID: USER FROM ENVIRONMENT "LOGNAME"
      *         RLOGIN-ALTERNATE : "Y" USER IS ALTERNATELY LOGGED IN.
      *                            "N" USER IS NOT ALTERNATELY LOGGED IN.
      *                             (USED TO BLINK BRANCH IN LIBGB/MENU)
      *         RLOGIN-REMOTE    : "Y" USER IS REMOTELY LOGGED IN.
      *                            "N" USER IS NOT REMOTELY LOGGED IN.
      *                             (USED TO BLINK BRANCH IN LIBGB/MENU)
      *
      *   REQUIRES THE SCREEN FIELDS:
      *         MENU OPTION:    RMT-BR        N(30) DISPLAY.
      *         RLOGIN-FG:      RLOG-FG       N(5)  NON-DISPLAY.
      *         RLOGIN-HOST:    RLOG-HOST     N     NON-DISPLAY.
      *         RLOGIN-USER:    RLOG-USER     N(10) NON-DISPLAY.
      *
      * REV:
      *  MJD 042396 ADDED GP-ALLOW-RLOGINS. IF NOT = "Y" THEN DO NOT ALLOW
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 032398 CHANGED FLAGE FOR RLOGINS.
      *  MJD 032498 ADDED REMOTE_LOGIN, LOCAL_LOGIN, AND SECURITY (SEE
      *             ABOVE FOR DESCRIPTION OF HOW IT WORKS). CHANGED TO
      *             GET USER FROM 'LOGNAME' RATHER THAN 'WHO AM I'
      *             CHANGED RLOGIN-BUF TO ADD NEW FLAGS, RLOGIN-YES,
      *             RLOGIN-ALLOW-REMOTE, AND RLOGIN-ALLOW-LOCAL.
      *  MJD 032698 ADDED GP-RLOGIN-LOCAL-PROFILE AND
      *             GP-RLOGIN-REMOTE-PROFILE. THESE CONTAIN THE LOWEST
      *             LOGIN THAT CAN ACCESS REMOTE LOGINS.
      *  MJD 000117 FIXED BUF REMOTE LOGIN SECURITY WAS TESTING
      *             GP-RLOGIN-LOCAL-PROFILE INSTEAD OF GP-RLOGIN-REMOTE-PROFILE
      *  BLF 050608 ADDED AN ACCESS CALL ON WKFILE (MKTEMP-BUF) TO TRY
      *             TO PIN DOWN PROBLEMS WHEN THERE ARE MANY OPEN ERRORS OF 35
      *             ON WKFILE. WILL DO AN ACCESS CALL & LOG AN ERROR & NOT
      *             ATTEMPT OPEN, SO USER WON'T GET ERROR ON SCREEN,
      *             ALSO ADDED LOGS IF BAD STATS ON SYSTEM CALLS,
      *             LENDMARK, PL# 496
      *  MJD 110309 EXPANDED WHOAMI-HOST-MACHINE AND WHOAMI-HOST-LINE.
      *             RAN INTO AN ISSUE ON WDT WHERE THE LENGHT OF THE LINES
      *             WERE TO LONG AND WERE GETTING TRUNCATED RESULTING IN
      *             THE EXTRACT ROUTINE TO NEVER FIND THE ")".
      *             ALSO ADDED TEST TO ENSURE INDEX DOES NOT GO OUT OF BOUNDS
      *             AND OVERWRITE OTHER FIELDS IN WORKING STORAGE.
      *  BLM 140417 INITIALIZE SYSTEM-BUF TO SPACES BEFORE STRINGING
      *             IN UNAME & WHO AM I COMMANDS FOR SYSTEM CALL
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKS.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       FD  WK-FILE 
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-REDEFINE-AREA         PIC X(160).
           03  WK-WHOAMI   REDEFINES WK-REDEFINE-AREA.
               05  WK-USERID            PIC X(10).
               05  WK-LINE              PIC X(150).
           03  WK-UNAME-N  REDEFINES WK-REDEFINE-AREA.
               05  WK-HOST-PROFILE  PIC X.
               05  WK-LINE2         PIC X(159).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/WHOAMI.CBL S35 02/01/24-08:42:02 >".

       01  WK-IFN                 PIC X(3)   VALUE "WK".

       01  SUB1                PIC 9(6)  COMP VALUE 0.
       01  SUB2                PIC 9(6)  COMP VALUE 0.

       01  HOLD-POSTNAME       PIC X(10) VALUE SPACES.
       01  HOLD-LOGNAME.
           03  HOLD-SECURITY   PIC X(1).
           03  HOLD-BR         PIC 9(4).
           03  HOLD-NAME       PIC X(5).

       01  WHOAMI-HOST-MACHINE    PIC X(80).
       01  FILLER  REDEFINES WHOAMI-HOST-MACHINE.
           03  WHOAMI-HOST        PIC X  OCCURS 80 TIMES.

       01  WHOAMI-HOST-LINE       PIC X(150).
       01  FILLER  REDEFINES WHOAMI-HOST-LINE.
           03  WHOAMI-LINE        PIC X  OCCURS 150 TIMES.

       COPY "LIBGB/MKTEMPW_CMD.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       01  WHOAMI-CMD     PIC X(8) VALUE "WHO AM I".
       01  UNAME-N-CMD        PIC X(8)  VALUE"UNAME -N".

       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBGB/RLOGINW.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME RLOGIN-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *     PROGRAM CONTROL
      *****************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      *    'SQL-CONNECT' SO THAT IT CAN GET THE EXACT PROGRAM NAME
      *    IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "WHOAMI"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM SQL-CONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WHOAMI" TO FORM-PROG.

           PERFORM GET-GPENV.

      * INITIALIZE THE PASSED BUFFER
           MOVE SPACES TO RLOGIN-BUF.

           MOVE GP-ALLOW-RLOGINS TO RLOGIN-FG.

           IF RLOGIN-FG NOT = "Y"
              MOVE "N" TO RLOGIN-ALLOW-LOCAL
                          RLOGIN-ALLOW-REMOTE
              GO TO TEST-RLOGIN-CONT.

           PERFORM INITIALIZATION.

      * TEST USER SECURITY FOR LOCAL/REMOTE LOGINS

           PERFORM TEST-LOCAL-SECURITY.
           PERFORM TEST-REMOTE-SECURITY.

           IF RLOGIN-ALLOW-REMOTE = "N"
              GO TO TEST-RLOGIN-CONT.

      * CREATE UNIQUE FILENAME FOR TEMP FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE TEMP FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO RLOGIN-FG
              GO TO TEST-RLOGIN-CONT.

      * PERFORM "UNAME -N > {TEMP FILENAME}"
      * PERFORM "WHO AM I >> {TEMP FILENAME}"

           MOVE SPACES TO SYSTEM-BUF.
           CALL "C$TOLOWER" USING UNAME-N-CMD, VALUE 8.
           STRING UNAME-N-CMD," > " MKTEMP-BUF
                  DELIMITED BY "  " INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "UNAM" TO ERRLOGW-TYPE
              PERFORM LOG-MKTEMP-ERROR.

           MOVE SPACES TO SYSTEM-BUF.
           CALL "C$TOLOWER" USING WHOAMI-CMD, VALUE 8.
           STRING WHOAMI-CMD," >> " MKTEMP-BUF
                  DELIMITED BY "  " INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "WHOM" TO ERRLOGW-TYPE
              PERFORM LOG-MKTEMP-ERROR.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "MKTM" TO ERRLOGW-TYPE
              PERFORM LOG-MKTEMP-ERROR
              MOVE "N" TO RLOGIN-FG
              GO TO TEST-RLOGIN-CONT.

           PERFORM OPEN-WK-FILE-INPUT.

      * READ "UNAME -N" RESULT

           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              MOVE "N" TO RLOGIN-FG
              GO TO TEST-RLOGIN-CONT.

           MOVE WK-HOST-PROFILE TO RLOGIN-HOST.

      * READ "WHO AM I" RESULT.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              MOVE "N" TO RLOGIN-FG
              GO TO TEST-RLOGIN-CONT.

           PERFORM CLOSE-WK-FILE.

           MOVE WK-LINE TO WHOAMI-HOST-LINE.

           MOVE 0 TO SUB1 SUB2.

      * SEARCH WHOAMI-LINE FOR FIRST OCCURANCE OF "("

       SCAN-WHOAMI-LINE.
           ADD 1 TO SUB1.
           IF WHOAMI-LINE(SUB1) = "("
              GO TO EXTRACT-HOST.

           IF SUB1 < 150
              GO TO SCAN-WHOAMI-LINE.

           IF RLOGIN-HOST = SPACES
              MOVE "N" TO RLOGIN-ALLOW-REMOTE
           ELSE
              MOVE "Y" TO RLOGIN-ALLOW-REMOTE.

           GO TO TEST-RLOGIN-CONT.

      * PUT HOST MACHINE PORTION OF "WHO AM I" IN WHOAMI-HOST-MACHINE.
      *  EX.
      *  WHOAMI-LINE         = "..........(M1)......"
      *  WHOAMI-HOST-MACHINE = "M1"

       EXTRACT-HOST.
           ADD 1 TO SUB1.
           IF WHOAMI-LINE(SUB1) NOT = ")" AND SUB2 < 80
              ADD 1 TO SUB2
              MOVE WHOAMI-LINE(SUB1) TO WHOAMI-HOST(SUB2)
              GO TO EXTRACT-HOST.

      *  TEST WHOAMI-HOST MACHINE AGAINST HOST MACHINE (LOCALPROFILE)
      *  IF WHOAMI-HOST-MACHINE(1) = RLOGIN-HOST
      *     DO NOT DISPLAY MENU OPTION FOR REMOTE LOGINS
      *     (USER HAS ALREADY DONE RLOGIN TO OTHER MACHINE)
      *  ELSE
      *     OK TO CONTINUE.
      *     (USER HAS NOT DONE RLOGIN TO OTHER MACHINE)
      *
      *  NOTE: THIS TESTS THE FIRST CHARECTER ONLY:
      *        WHOAMI-HOST-MACHINE(1) = M    (FROM M1)
      *        RLOGIN-HOST            = M    (FROM MERC1)

           IF WHOAMI-HOST-MACHINE NOT = SPACES
              IF WHOAMI-HOST(1)  = RLOGIN-HOST
                 MOVE "N" TO RLOGIN-ALLOW-REMOTE
                 MOVE "Y" TO RLOGIN-IS-REMOTE
              ELSE
                 MOVE "Y" TO RLOGIN-ALLOW-REMOTE.

      * IF RLOGIN-FG = "Y"
      *    IF UNAME NOT = "WHO AM I" THEN USER HAS DONE A "SU" TO
      *         ANOTHER ID.

       TEST-RLOGIN-CONT.

      * WE ARE NO LONGER DOING A 'SU -' SO WE DON'T CARE.
      * WE ARE ALLOWING REMOTE LOGINS WITHIN EACH MACHINE BY CHANGING
      * POSTNAME, FIL, AND THE CURRENT WORKING DIRECTORY. THEREFORE
      * AS LONG AS THE USER IS ON THE SAME MACHINE, THERE IS NO LIMIT
      * TO THE NUMBER OF BRANCHES HE CAN TRAVERSE.

           IF HOLD-POSTNAME NOT = HOLD-LOGNAME
              MOVE "Y" TO RLOGIN-IS-LOCAL.

           GO TO END-ROUTINE.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE SPACES TO WHOAMI-HOST-MACHINE
                          WHOAMI-HOST-LINE
                          RLOGIN-HOST.

           MOVE "N" TO RLOGIN-IS-LOCAL
                       RLOGIN-IS-REMOTE.

           MOVE "Y" TO RLOGIN-ALLOW-LOCAL
                       RLOGIN-ALLOW-REMOTE.

           MOVE "POSTNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO HOLD-POSTNAME.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO HOLD-LOGNAME
                              RLOGIN-USERID.

       END-INITIALIZE.
           EXIT.

      *****************************************************************
       TEST-REMOTE-SECURITY SECTION.
      *****************************************************************
           IF GP-RLOGIN-REMOTE-PROFILE = " "
              MOVE "N" TO RLOGIN-ALLOW-REMOTE
              GO TO EXIT-TEST-REMOTE-SECURITY.

           CALL "C$TOLOWER" USING GP-RLOGIN-REMOTE-PROFILE, VALUE 1.
           IF HOLD-SECURITY > GP-RLOGIN-REMOTE-PROFILE
              MOVE "N" TO RLOGIN-ALLOW-REMOTE.

       EXIT-TEST-REMOTE-SECURITY.
           EXIT.

      *****************************************************************
       TEST-LOCAL-SECURITY SECTION.
      *****************************************************************

      * TEST USER SECURITY FOR LOCAL ACCESS (ALTERNATE LOGIN)

           IF GP-RLOGIN-LOCAL-PROFILE = " "
              MOVE "N" TO RLOGIN-ALLOW-LOCAL
              GO TO EXIT-TEST-LOCAL-SECURITY.

           CALL "C$TOLOWER" USING GP-RLOGIN-LOCAL-PROFILE, VALUE 1.
           IF HOLD-SECURITY > GP-RLOGIN-LOCAL-PROFILE
              MOVE "N" TO RLOGIN-ALLOW-LOCAL.

       EXIT-TEST-LOCAL-SECURITY.
           EXIT.

      *****************************************************************
       LOG-MKTEMP-ERROR SECTION.
      *****************************************************************
           MOVE "E" TO ERRLOGW-ACTION.
           MOVE MKTEMP-BUF TO ERRLOGW-FILE.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************************
       MISC-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBWKLSIO.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      *    'CLOSE-FILES' (LIBGB/*DECLARE*) SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG; NEED TO MOVE SPACES INTO
      *    EXT-TCLP-TRACE-FORM-PATH AFTER 'CLOSE-FILES' TO RESUME
      *    USING FORM-PATHNAME.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "WHOAMI"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM CLOSE-FILES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.
