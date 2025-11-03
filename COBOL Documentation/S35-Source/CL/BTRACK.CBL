       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BTRACK.
       AUTHOR.          JKC.
       DATE-WRITTEN.    01-03-2025.
      ******************************************************************
      *
      * IDENTIFICATION: CL/BTRACK
      *
      * DESCRIPTION   : BATCH TRACK PROGRAM
      *                 USED TO TRACK THE EXECUTION OF BATCH PROGRAMS
      *
      *                 ORIGINALLY DESIGNED FOR S35 DATA LOAD
      *                 QA COMPARISON PROGRAMS USING BATCH CODE "Q35";
      *                 USED FOR THE BASH SCRIPT "Q35.SH" TO EXECUTE AND
      *                 TRACK THE EXECUTION.
      *
      * INPUT  FIELDS : EXT-EOBUF-PRPATH
      *
      * OUTPUT FILE   : /TMP/####??????-XXXXXX
      *
      *                 ####   = BATCH STEP
      *                 ?????? = BATCH PROGRAM BEING EXECUTED
      *                 XXXXXX = USERID ID
      *
      *=================================================================
      * REV:
      * JKC 2025-0103 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/BTRACK.CBL S35 01/07/25-15:56:13 >".

      *=================================================================
       01  ERRCD                       PIC X(01)     VALUE SPACES.

       01  STAT                        PIC X(02)     VALUE SPACES.
           88  STAT-VALID                      VALUE "00".
           88  STAT-GOOD                       VALUE "00".
           88  STAT-BAD                        VALUE "  " THRU "//"
                                                     "01" THRU "~~".
       01  STAT-99 REDEFINES STAT      PIC 9(02).

       01  USERID                      PIC X(10)     VALUE SPACES.

      *=================================================================
       01  WSS-DATE-NOW                PIC 9(8).
       01  FILLER                      REDEFINES WSS-DATE-NOW.
           03  WSS-DATE-CC             PIC 9(2).
           03  WSS-DATE-YY             PIC 9(2).
           03  WSS-DATE-MM             PIC 9(2).
           03  WSS-DATE-DD             PIC 9(2).
       01  FILLER                      REDEFINES WSS-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  WSS-DATE-YYMMDD         PIC 9(6).

       01  WSS-DETAIL-LINE.
           03  WSS-DETAIL-DATE         PIC 9999/99/99.
           03  FILLER                  PIC X(1).
           03  WSS-DETAIL-TIME         PIC 99/99/99.
           03  FILLER                  PIC X(1).
           03  WSS-DETAIL-TEXT         PIC X(45).

       01  WSS-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  WSS-ECHO-TEXT           PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  WSS-EOBUF-PRPATH            PIC X(50)     VALUE SPACES.
       01  FILLER                      REDEFINES WSS-EOBUF-PRPATH.
           03  FILLER                  PIC X(32).
           03  WSS-EO-ALL-SPOOL-FILE   PIC X(18).

       01  WSS-FORM-PATH               PIC X(22)     VALUE SPACES.

       01  WSS-OUTPUT-FILE.
           03  FILLER                  PIC X(04)     VALUE "/TMP".
           03  WSS-OUTPUT-FILE-NAME    PIC X(40).

       01  WSS-RM-CMD.
           03  FILLER                  PIC X(07)     VALUE "RM -RF ".
           03  WSS-RM-FILE             PIC X(40)     VALUE SPACES.

       01  WSS-SYSTEM.
           03  WSS-SYSTEM-CMD          PIC X(120)    VALUE SPACES.
           03  WSS-SYSTEM-DIRECT       PIC X(04)     VALUE " >  ".
           03  WSS-SYSTEM-OUTPUT       PIC X(55)     VALUE SPACES.

       01  WSS-TIME-NOW.
           03  WSS-TIME-HH             PIC 9(2).
           03  WSS-TIME-MM             PIC 9(2).
           03  WSS-TIME-SS             PIC 9(2).
           03  WSS-TIME-HD             PIC 9(2).
       01  FILLER                      REDEFINES WSS-TIME-NOW.
           03  WSS-TIME-HHMMSSHD       PIC 9(8).
       01  FILLER                      REDEFINES WSS-TIME-NOW.
           03  WSS-TIME-HHMMSS         PIC 9(6).
           03  FILLER                  PIC 9(2).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/EOBUF_EXT.CPY".         *> NEED FOR EXT-EOBUF-PRPATH
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/UNAMEW.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S E C T I O N
      *
      ******************************************************************
       LINKAGE SECTION.
       01  FORM-PATHNAME               PIC X(22).
       COPY "LIBCL/BTRACKW.CPY".

      ******************************************************************
      *
      *    P R O C E D U E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME
                   BTRACK-WORKERS.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP FORM-PATHNAME.

           IF NOT ( BTRACK-SWITCH-VALID )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INITIALIZATION OF TRACE FIELDS (TIME, DATE, LOG FILE NAME)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ACCEPT WSS-TIME-NOW        FROM TIME.
           ACCEPT WSS-DATE-NOW        FROM CENTURY-DATE.

           MOVE EXT-EOBUF-PRPATH           TO WSS-EOBUF-PRPATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GET USERID
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "USER" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE "Z9999ZZZ"              TO GETENV-BUF
              CALL "C$TOLOWER" USING GETENV-BUF, VALUE 10.

           MOVE GETENV-BUF                 TO USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LOAD OUTPUT FILE FIELDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           CALL "C$TOLOWER" USING WSS-ECHO-CMD,    VALUE 4.
           CALL "C$TOLOWER" USING WSS-OUTPUT-FILE, VALUE 4.

           STRING WSS-EO-ALL-SPOOL-FILE, "-", USERID
                  DELIMITED BY "  "      INTO WSS-OUTPUT-FILE-NAME.

           MOVE WSS-OUTPUT-FILE            TO WSS-SYSTEM-OUTPUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    REMOVE BTRACK FILE IF REQUESTED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BTRACK-SWITCH-REMOVE )
              CALL "C$TOLOWER" USING WSS-RM-CMD, VALUE 7
              MOVE WSS-OUTPUT-FILE         TO WSS-RM-FILE
              CALL 'SYSTEM' USING WSS-RM-CMD
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    POPULATE BTRACK DETAIL LINE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LOAD-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WRITE BTRACK DETAIL LINE TO BTRACK FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BTRACK-SWITCH-CREATE )
              MOVE " >  "                  TO WSS-SYSTEM-DIRECT
           ELSE
              MOVE " >> "                  TO WSS-SYSTEM-DIRECT.

           MOVE WSS-DETAIL-LINE            TO WSS-ECHO-TEXT.
           MOVE WSS-ECHO-CMD               TO WSS-SYSTEM-CMD.
           CALL 'SYSTEM' USING WSS-SYSTEM.

           MOVE SPACES                     TO WSS-DETAIL-LINE.

      *=================================================================
       MAIN-PROGRAM-EXIT.

           MOVE SPACES                     TO BTRACK-MESS.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    L O A D - D E T A I L - L I N E
      *
      ******************************************************************
       LOAD-DETAIL-LINE SECTION.

           MOVE SPACES                     TO WSS-DETAIL-LINE.

           MOVE WSS-DATE-NOW               TO WSS-DETAIL-DATE.
           INSPECT WSS-DETAIL-DATE REPLACING ALL "/" BY "-".

           MOVE WSS-TIME-HHMMSS            TO WSS-DETAIL-TIME.
           INSPECT WSS-DETAIL-TIME REPLACING FIRST "/" BY ":".
           INSPECT WSS-DETAIL-TIME REPLACING FIRST "/" BY ":".

           IF ( BTRACK-SWITCH-CREATE )
              MOVE "PROCESSING START"      TO WSS-DETAIL-TEXT.

           IF ( BTRACK-SWITCH-DONE )
              MOVE "DONE"                  TO WSS-DETAIL-TEXT.

           IF ( BTRACK-SWITCH-GLOBAL )
              IF ( BTRACK-MESS NOT = SPACES )
                 MOVE BTRACK-MESS          TO WSS-DETAIL-TEXT
              ELSE
                 MOVE "PROCESSING GLOBAL"  TO WSS-DETAIL-TEXT.

           IF ( BTRACK-SWITCH-LOCAL )
              IF ( BTRACK-MESS NOT = SPACES )
                 MOVE BTRACK-MESS          TO WSS-DETAIL-TEXT
              ELSE
              IF ( BTRACK-BRNO = ZEROES )
                 MOVE "PROCESSING LOCAL"   TO WSS-DETAIL-TEXT
              ELSE
                 STRING "PROCESSING BRANCH: ", BTRACK-BRNO
                       DELIMITED BY "  " INTO WSS-DETAIL-TEXT.

      *=================================================================
       LOAD-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *=================================================================
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/UNAME.CPY".
      *=================================================================

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
