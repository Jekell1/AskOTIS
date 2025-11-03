       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRACE.
       AUTHOR.          JKC.
       DATE-WRITTEN.    01-24-2024.
      ******************************************************************
      *
      * IDENTIFICATION: CL/TRACE
      *
      * DESCRIPTION   : TCLP TRACE LOGGING PROGRAM
      *
      *                 CALLED FROM THE 'SQL-CONNECT' PARAGRAPHS
      *                   + LIBGB/CONNECT_SQL
      *
      *                 CALLED FROM THE 'CLOSE-FILES' PARAGRAPHS
      *                 WITHIN THE FOLLOWING:
      *                   + LIBGB/DECLARE
      *                   + LIBGB/DECLAREOD
      *                   + LIBGB/DECLARE_SQL
      *                   + LIBGB/DECLRP
      *                   + LIBGB/DECLRP_SQL
      *
      *                 CALLED FROM THE BRANCH FILE (BRFILE) PARAGRAPHS
      *                   + LIBGB/GBBRIN
      *                   + LIBGB/GBBRRN
      *
      * ENVIRONMENT   : EXPORT TCLP_TRACE=N       *> DISABLE
      *                 EXPORT TCLP_TRACE=B       *> BRANCH LOGGING
      *                 EXPORT TCLP_TRACE=Y       *> BASIC  LOGGING
      *
      * INPUT  FIELDS : EXT-TCLP-TRACE
      *                 EXT-TCLP-TRACE-FORM-PATH       (SPACES/POPULATE)
      *                 EXT-TCLP-TRACE-TRACE-MESS      (POPULATE)
      *                 EXT-TCLP-TRACE-TRACE-SKIP-YN   (Y/N)
      *
      * OUTPUT FIELDS : EXT-TCLP-TRACE-FORM-PATH       (SPACES)
      *                 EXT-TCLP-TRACE-TRACE-MESS      (SPACES)
      *                 EXT-TCLP-TRACE-TRACE-SKIP-YN   ("N")
      *
      *=================================================================
      * REV:
      * JKC 2024-0124 ***NEW***
      * JKC 2024-0126 CHANGED THE TRACE FILE NAME TO INCLUDE USER PID
      * JKC 2024-0131 ADDED MORE LOGIC TO USE NEW EXTERNAL TRACE
      *          WORKERS FOR BETTER IDENTIFYING WHICH PROGRAM IS USED
      *          AS WELL AS ABLE TO SKIP LOGGING A GIVEN EVENT.
      * JKC 2024-0309 INCREASED THE SIZE OF TRACE-SYSTEM-OUTPUT AND
      *          TRACE-OUTPUT-USERID; ON ATLPERF, THE PPID AT THE END
      *          OF THE OUTPUT FILE WAS BEING CUTOFF; ONLY 5 BYTES OF A
      *           7 BYTE PPID WAS BEING PLACED INTO THE TRACE OUTPUT
      *           FILE THUS CAUSING TWO OTIS EXECUTIONS TO BE PUT INTO
      *           THE SAME TRACE FILE.
      * JKC 2024-0324 COMMENT OUT TRACE-OUTPUT-DATE WITHIN
      *          TRACE-OUTPUT-FILE; WAS HAVING ISSUES WITH THE
      *          A15-TO-S35 PROGRAMS WHERE IT CROSSED INTO A NEW
      *          DAY SO THE TRACE LOG WAS SPLIT INTO TWO FILES.
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)CL/TRACE.CBL S35 03/24/24-13:11:57 >".

      *=================================================================
       01  ERRCD                       PIC X(01)     VALUE SPACES.

       01  STAT                        PIC X(02)     VALUE SPACES.
           88  STAT-VALID                      VALUE "00".
           88  STAT-GOOD                       VALUE "00".
           88  STAT-BAD                        VALUE "  " THRU "//"
                                                     "01" THRU "~~".
       01  STAT-99 REDEFINES STAT      PIC 9(02).

       01  USERID                      PIC X(10)     VALUE SPACES.
       01  WSS-FORM-PATH               PIC X(22)     VALUE SPACES.

      *=================================================================
       01  TRACE-DATE-NOW.
           03  TRACE-DATE-CC           PIC 9(2).
           03  TRACE-DATE-YY           PIC 9(2).
           03  TRACE-DATE-MM           PIC 9(2).
           03  TRACE-DATE-DD           PIC 9(2).
       01  FILLER            REDEFINES TRACE-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  TRACE-DATE-YYMMDD       PIC 9(6).

       01  TRACE-DETAIL-LINE.
           03  TRACE-DETAIL-TIME       PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  TRACE-DETAIL-LEVEL      PIC 9(2).
           03  FILLER                  PIC X(1).
           03  TRACE-DETAIL-PROGRAM    PIC X(7).
           03  FILLER                  PIC X(1).
           03  TRACE-DETAIL-TEXT       PIC X(57).

       01  TRACE-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  TRACE-ECHO-TEXT         PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  TRACE-MESS                  PIC X(61)     VALUE SPACES.
       01  FILLER                      REDEFINES TRACE-MESS.
           03  TRACE-MESS-PROGRAM-STAT PIC X(14).
           03  FILLER                  PIC X(47).
       01  FILLER                      REDEFINES TRACE-MESS.
           03  TRACE-MESS-OTIS-STAT    PIC X(22).
           03  FILLER                  PIC X(39).

       01  TRACE-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(06)     VALUE "TRACE-".
      *KC  03  TRACE-OUTPUT-DATE       PIC 9(06)     VALUE ZEROES.
      *KC  03  FILLER                  PIC X(01)     VALUE "-".
           03  TRACE-OUTPUT-USERID     PIC X(30)     VALUE ZEROES.

       01  TRACE-SYSTEM.
           03  TRACE-SYSTEM-CMD        PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  TRACE-SYSTEM-OUTPUT     PIC X(55)     VALUE SPACES.

       01  TRACE-TIME-NOW.
           03  TRACE-TIME-HH           PIC 9(2).
           03  TRACE-TIME-MM           PIC 9(2).
           03  TRACE-TIME-SS           PIC 9(2).
           03  TRACE-TIME-HD           PIC 9(2).
       01  FILLER                      REDEFINES TRACE-TIME-NOW.
           03  TRACE-TIME-HHMMSSHD     PIC 9(8).
       01  FILLER                      REDEFINES TRACE-TIME-NOW.
           03  TRACE-TIME-HHMMSS       PIC 9(6).
           03  FILLER                  PIC 9(2).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONAME_EXT.CPY".
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

      ******************************************************************
      *
      *    P R O C E D U E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           IF ( EXT-TCLP-TRACE-DISABLE       ) OR
              ( EXT-TCLP-TRACE-SKIP-YN = "Y" )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INITIALIZATION OF TRACE FIELDS (TIME, DATE, LOG FILE NAME)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ACCEPT TRACE-TIME-NOW         FROM TIME.
           ACCEPT TRACE-DATE-NOW         FROM CENTURY-DATE.

      *KC  MOVE TRACE-DATE-YYMMDD          TO TRACE-OUTPUT-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GET USERID
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "USER" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE "Z9999ZZZ"              TO GETENV-BUF
              CALL "C$TOLOWER" USING GETENV-BUF, VALUE 10.

           MOVE GETENV-BUF                 TO USERID.

           MOVE "TCLP_PID" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              MOVE SPACES                  TO GETENV-BUF.

           MOVE SPACES                     TO TRACE-OUTPUT-USERID.

           STRING USERID, "-", GETENV-BUF
                  DELIMITED BY " "       INTO TRACE-OUTPUT-USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           CALL "C$TOLOWER" USING TRACE-OUTPUT-FILE, VALUE 8.
           CALL "C$TOLOWER" USING TRACE-ECHO-CMD,    VALUE 4.

           MOVE TRACE-OUTPUT-FILE          TO TRACE-SYSTEM-OUTPUT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PROCESS EXT-TCLP-TRACE-LEVEL FOR THE DEPTH LEVEL FOR
      *    OTIS/PROGRAM STARTING (+)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-TCLP-TRACE-MESS        TO TRACE-MESS.

           IF ( TRACE-MESS-OTIS-STAT    = "OTIS SESSION STARTED /" ) OR
              ( TRACE-MESS-PROGRAM-STAT = "PROGRAM START"          )
              ADD 01                       TO EXT-TCLP-TRACE-LEVEL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    POPULATE TRACE DETAIL LINE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TRACE-TIME-HHMMSSHD        TO TRACE-DETAIL-TIME.
           INSPECT TRACE-DETAIL-TIME REPLACING FIRST "/" BY ":".
           INSPECT TRACE-DETAIL-TIME REPLACING FIRST "/" BY ":".
           INSPECT TRACE-DETAIL-TIME REPLACING FIRST "/" BY ".".

           MOVE EXT-TCLP-TRACE-LEVEL       TO TRACE-DETAIL-LEVEL.

           IF ( EXT-TCLP-TRACE-FORM-PATH NOT = SPACES )
              MOVE EXT-TCLP-TRACE-FORM-PATH TO WSS-FORM-PATH
           ELSE
              MOVE FORM-PATHNAME            TO WSS-FORM-PATH.

           MOVE WSS-FORM-PATH(16:7)        TO TRACE-DETAIL-PROGRAM.
           MOVE EXT-TCLP-TRACE-MESS        TO TRACE-DETAIL-TEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WRITE TRACE DETAIL LINE TO TRACE LOG FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TRACE-DETAIL-LINE          TO TRACE-ECHO-TEXT.
           MOVE TRACE-ECHO-CMD             TO TRACE-SYSTEM-CMD.
           CALL 'SYSTEM' USING TRACE-SYSTEM.

           MOVE SPACES                     TO TRACE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PROCESS EXT-TCLP-TRACE-LEVEL FOR THE DEPTH LEVEL FOR
      *    OTIS/PROGRAM COMPLETING/FINISHING (-)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-TCLP-TRACE-MESS        TO TRACE-MESS.

           IF ( TRACE-MESS-OTIS-STAT    = "OTIS SESSION COMPLETED" ) OR
              ( TRACE-MESS-PROGRAM-STAT = "PROGRAM FINISH"         )
              SUBTRACT 01                FROM EXT-TCLP-TRACE-LEVEL.

      *=================================================================
      *    RESET SPECIFIC EXT-TCLP-TRACE-WORKERS BEFORE RETURNING
      *    BACK TO CALLING PROGRAM.
      *=================================================================
       MAIN-PROGRAM-EXIT.

           MOVE SPACES TO EXT-TCLP-TRACE-FORM-PATH
                          EXT-TCLP-TRACE-MESS.

           MOVE "N"    TO EXT-TCLP-TRACE-SKIP-YN.

           GO TO END-PROGRAM.

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
