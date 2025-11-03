       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCONN.
       AUTHOR.        JKC.
       DATE-WRITTEN.  01-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCONN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL CONNECTION LOGIC
      * TABLE NAME    : *NONE* 
      * TABLE DESC    : *NONE*
      * OUTPUT FILE   : XPC/IOCONN.PRE
      * USED FOR      : LIBIO/CONNECTW_SQL.CPY
      *                 LIBIO/CONNECT_SQL.CPY
      *                 LIBIO/DISCONNECT_SQL.CPY
      *
      *=================================================================
      * REV:
      * JKC 2021-0115 ***NEW*** WORKING ON CONCEPT
      * JKC 2021-0525 ADDED 'WHENEVER SQLERROR' IN SQL-CONNECT SECTION.
      *               UPDATED THE WORKERS NEEDED.
      * JKC 2021-0702 CHANGED SQL-DISCONNECT TO NOT DO ANY
      *               SQL-IO-VALIDATION SINCE IT REALLY IS NOT NEEDED
      *               AS WELL AS PROGRAMS WITH LIBIO/DISCONNECT_SQL
      *               WILL NOT COMPILE SINCE NOT HAVING ALL THE NEEDED
      *               FIELDS DEFINED.
      * JKC 2021-0819 FIXED TO DISPLAY PROGRAM NAME IN ERROR WINDOW.
      *               FORCED USING '1900-01-01' WITHIN SQL-GET-DATE
      *               AND SQL-SET-DATE.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/IOCONN S35 03/31/23-09:23:17 >".

      ******************************************************************
      ******************************************************************
      ******************************************************************
      * COPY MEMBER: LIBGB/SQLCONNECT.CPY
      *-----------------------------------------------------------------
      * PRE-COMPILE OUTPUT BELOW GOES INTO LIBGB/SQLCONNECTW.CPY
      *-----------------------------------------------------------------
      * WORKERS THAT NEED TO BE IN EACH INDIVIDUAL PROGRAM:
      * `EXEC SQL INCLUDE SQLCA END-EXEC.`
      ******************************************************************
      ******************************************************************
      ******************************************************************
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-GP-KEY                      PIC 99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  SQL-CONNECT-WORKERS.
           03  SQL-CONNECT-SERVER          PIC X(30).
           03  SQL-CONNECT-USERID          PIC X(30).
           03  SQL-CONNECT-PSWD            PIC X(30).
      *??? 03  SQL-CONNECT-DATABASE        PIC X(20).
      *??? 03  SQL-CONNECT-OWNER           PIC X(20).
      *??? 03  SQL-CONNECT-PRE-FILE        PIC X(20).
      *    03  SQL-CONNECT-SQLCODE         PIC 9(09)-.
      *    03  SQL-CONNECT-SQLSTATE        PIC X(05).
      *    03  SQL-CONNECT-SQLERRMC        PIC X(70).
      *    03  SQL-CONNECT-STAT            PIC X(02) VALUE "XX".
      *        88  SQL-CONNECT-STAT-GOOD       VALUE "00".
      *        88  SQL-CONNECT-STAT-BAD        VALUE "  " THRU "//"
      *                                              "01" THRU "~~".

       01  SQL-DATE-WORKERS.
           03  SQL-DATE-YYYYMMDD                     PIC 9(8).
           03  FILLER REDEFINES SQL-DATE-YYYYMMDD.
               05  SQL-DATE-YYYYMMDD-YYYY            PIC 9(4).
               05  SQL-DATE-YYYYMMDD-MM              PIC 9(2).
               05  SQL-DATE-YYYYMMDD-DD              PIC 9(2).

           03  SQL-DATE-YYYY-MM-DD                   PIC X(10).
           03  SQL-DATE-YYYY-MM-DD-NUM REDEFINES SQL-DATE-YYYY-MM-DD
                                                     PIC 9999/99/99.
           03  FILLER REDEFINES SQL-DATE-YYYY-MM-DD.
               05  SQL-DATE-YYYY-MM-DD-YYYY          PIC 9(4).
               05  FILLER                            PIC X(1).
               05  SQL-DATE-YYYY-MM-DD-MM            PIC 9(2).
               05  FILLER                            PIC X(1).
               05  SQL-DATE-YYYY-MM-DD-DD            PIC 9(2).

       01  SQL-ERROR-BUF.
           03  SQL-ERROR-01                          PIC X(70).
           03  SQL-ERROR-02                          PIC X(70).
           03  SQL-ERROR-03                          PIC X(70).
           03  SQL-ERROR-04                          PIC X(70).
           03  SQL-ERROR-05                          PIC X(70).
           03  FILLER REDEFINES SQL-ERROR-05.
               05  SQL-ERROR-05-SQLCODEX             PIC X(6).
               05  SQL-ERROR-05-SQLCODE              PIC Z(8)9-.
               05  FILLER                            PIC X(3).
               05  SQL-ERROR-05-SQLSTATEX            PIC X(7).
               05  SQL-ERROR-05-SQLSTATE             PIC X(5).
               05  FILLER                            PIC X(3).
               05  SQL-ERROR-05-SQLERRMLX            PIC X(7).
               05  SQL-ERROR-05-SQLERRML             PIC 9(4)-.
               05  FILLER                            PIC X(3).
               05  SQL-ERROR-05-SQLERRPLX            PIC X(7).
               05  SQL-ERROR-05-SQLERRPL             PIC 9(4)-.
           03  SQL-ERROR-06                          PIC X(70).
           03  FILLER REDEFINES SQL-ERROR-06.
               05  SQL-ERROR-06-KEYX                 PIC X(6).
               05  SQL-ERROR-06-KEY                  PIC X(64).
           03  SQL-ERROR-07                          PIC X(70).
           03  SQL-ERROR-08                          PIC X(70).
       01  SQL-DEBUG-BUF REDEFINES SQL-ERROR-BUF.
           03  SQL-DEBUG-01                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-01.
               05  SQL-DEBUG-01-SQLCODEX             PIC X(7).
               05  SQL-DEBUG-01-SQLCODE              PIC 9(9)-.
               05  FILLER                            PIC X(3).
               05  SQL-DEBUG-01-SQLCABCX             PIC X(7).
               05  SQL-DEBUG-01-SQLCABC              PIC 9(9)-.
               05  FILLER                            PIC X(12).
               05  SQL-DEBUG-01-SQLCAIDX             PIC X(6).
               05  SQL-DEBUG-01-SQLCAID              PIC X(8).
           03  SQL-DEBUG-02                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-02.
               05  SQL-DEBUG-02-SQLSTATEX            PIC X(7).
               05  SQL-DEBUG-02-SQLSTATE             PIC X(5).
               05  FILLER                            PIC X(8).
               05  SQL-DEBUG-02-SQLERRMLX            PIC X(7).
               05  SQL-DEBUG-02-SQLERRML             PIC 9(4)-.
               05  FILLER                            PIC X(3).
               05  SQL-DEBUG-02-SQLERRPLX            PIC X(7).
               05  SQL-DEBUG-02-SQLERRPL             PIC 9(4)-.
               05  FILLER                            PIC X(2).
               05  SQL-DEBUG-02-SQLWARNX             PIC X(6).
               05  SQL-DEBUG-02-SQLWARN0             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN1             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN2             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN3             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN4             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN5             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN6             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN7             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN8             PIC X(1).
               05  SQL-DEBUG-02-SQLWARN9             PIC X(1).
               05  SQL-DEBUG-02-SQLWARNA             PIC X(1).
           03  SQL-DEBUG-03                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-03.
               05  SQL-DEBUG-03-SQLERRDX             PIC X(7).
               05  SQL-DEBUG-03-SQLERRD-1            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-03-SQLERRD-2            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-03-SQLERRD-3            PIC 9(9)-.
           03  SQL-DEBUG-04                          PIC X(70).
           03  FILLER REDEFINES SQL-DEBUG-04.
               05  SQL-DEBUG-04-SQLERRDX             PIC X(7).
               05  SQL-DEBUG-04-SQLERRD-4            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-04-SQLERRD-5            PIC 9(9)-.
               05  FILLER                            PIC X(1).
               05  SQL-DEBUG-04-SQLERRD-6            PIC 9(9)-.
           03  SQL-DEBUG-05                          PIC X(70).
           03  SQL-DEBUG-06                          PIC X(70).
           03  SQL-DEBUG-07                          PIC X(70).
           03  SQL-DEBUG-08                          PIC X(70).

      ******************************************************************
      ******************************************************************
      ******************************************************************
      * THE FOLLOWING NEEDED FOR PRE-COMPILE; NOT NEEDED IN COPY MEMBERS
      ******************************************************************
      ******************************************************************
      ******************************************************************
       01  E-FILE                      VALUE SPACES.
           03  E-LFN                   PIC X(02).
           03  E-NAME                  PIC X(09).
           03  FILLER                  PIC X(40).

       01  E-PROG                      PIC X(35).

       01  ERROR-MSG-HEAD.
           03  E-MSG                   PIC X(15).
           03  THIS                    REDEFINES E-MSG.
               05  E-MSG4              PIC X(04).
               05  FILLER              PIC X(11).
           03  FILLER                  PIC X(01) VALUE SPACES.

       01  ERROR-MSG                   REDEFINES ERROR-MSG-HEAD
                                       PIC X(16).

       01  ERROR-MSG-KEY.
           03  FILLER                  PIC X(05) VALUE " KEY=".
           03  E-KEYX                  PIC X(55).

      *-----------------------------------------------------------------
       78  IO---BAD                              VALUE 9.
       78  IO---GOOD                             VALUE 0.
       01  IO-FG                       PIC 9(01) VALUE ZEROES.
           88  IO-ALREADY-OPEN                   VALUE 7.
           88  IO-BAD                            VALUE 1 THRU 9.
           88  IO-GOOD                           VALUE 0.
           88  IO-LOCKED                         VALUE 8.

      *-----------------------------------------------------------------
       78  STAT---GOOD                           VALUE "00".
       78  STAT---BAD                            VALUE "XX".
       01  STAT                        PIC X(02) VALUE SPACES.
           88  STAT-VALID                        VALUE "00".
           88  STAT-GOOD                         VALUE "00".
           88  STAT-BAD                          VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *=================================================================
       COPY "LIBGB/CONAME_EXT.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      * SQL-IO-VALIDATION IS EMPTY SO THAT THE PROGRAM COMPILES.
      * SQL-IO-VALIDATION IS NORMALLY IN THE DECLARE COPY MEMBERS.
      * SQL-IO-VALIDATION IS NEEDED FOR `PERFORM SQL-IO-VALIDATION`
      ******************************************************************
      ******************************************************************
      ******************************************************************
       SQL-IO-VALIDATION.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      * COPY MEMBER: LIBGB/SQLCONNECT.CPY
      *-----------------------------------------------------------------
      * ***NEW*** COPY MEMBER; PRE-COMPILED CODE INSERTED INTO IT.
      ******************************************************************
      ******************************************************************
      ******************************************************************

      ******************************************************************
      *
      *    S Q L - C O N N E C T
      *
      *=================================================================
      * IN  : EXT-ACUSQL-CONNECT-WORKERS
      * OUT : SQL-CONNECT-WORKERS
      *       IO-FG    [0 = GOOD; 9 = BAD]
      * DESC: MAKE CONNECTION TO THE SQL SERVER:
      *       1. CHECK CONNECTION STATUS
      *          A. BAD THEN MAKE CONNECTION
      *          B. GOOD THEN CONTINUE
      *       2. VERIFY CONNECTION
      *          A. BAD THEN CONTINUE
      *          B. GOOD THEN EXIT PARAGRAPH
      *       3. MAKE CONNECTION
      *       4. VERIFY CONNECTIONSTATUS
      *          A. BAD THEN ERROR PROCEDURE
      *          B. GOOD THEN EXIT PARAGRAPH
      ******************************************************************
       SQL-CONNECT.

           IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
              PERFORM SQL-CONNECT-MAKE
              IF ( SQLCODE < 0 )
                 PERFORM SQL-CONNECT-ERROR
                 EXIT PARAGRAPH.

           PERFORM SQL-CONNECT-VERIFY.

           IF ( SQLCODE = 0 )
              PERFORM SQL-CONNECT-WHENEVER
              MOVE STAT---GOOD TO EXT-ACUSQL-CONNECT-STAT
              EXIT PARAGRAPH.

           PERFORM SQL-CONNECT-MAKE.

           IF ( SQLCODE < 0 )
              PERFORM SQL-CONNECT-ERROR
              MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT
              EXIT PARAGRAPH.

           PERFORM SQL-CONNECT-WHENEVER.
           MOVE STAT---GOOD TO EXT-ACUSQL-CONNECT-STAT.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - E R R O R
      *
      *=================================================================
      * IN  : SQLCA WORKERS
      * OUT : NONE
      * DESC: PROCESS ERROR FOR SQL SERVER CONNECTION
      ******************************************************************
       SQL-CONNECT-ERROR.

           MOVE "CONNECTION"            TO E-MSG.
           MOVE "SQL SERVER CONNECTION" TO E-FILE.
           PERFORM SQL-ERROR.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - M A K E
      *
      *=================================================================
      * IN  : EXT-ACUSQL-CONNECT-WORKERS
      * OUT : SQLCA WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      ******************************************************************
       SQL-CONNECT-MAKE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET CONNECTION WORKERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-ACUSQL-DSN-NAME  TO SQL-CONNECT-SERVER.
           MOVE EXT-ACUSQL-LOGIN     TO SQL-CONNECT-USERID.
           MOVE EXT-ACUSQL-PSWD      TO SQL-CONNECT-PSWD.

      *??? MOVE "WAAZA34SQL-DEV'     TO SQL-CONNECT-SERVER
      *??? MOVE "DBO"    	     TO SQL-CONNECT-OWNER
      *??? MOVE "OTIS"               TO SQL-CONNECT-DATABASE
           
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MAKE CONNECTION TO SQL SERVER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS C1
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - V E R I F Y
      *
      *=================================================================
      * IN  : NONE
      * OUT : SQLCA WORKERS
      * DESC: VERIFY THAT CONNECTION IS GOOD.  DOING A ROW COUNT ON THE
      *       GPFILE SINCE IT SHOULD BE QUICK WITH ONLY HAVING ONE ROW.
      ******************************************************************
       SQL-CONNECT-VERIFY.

           EXEC SQL
                SELECT GPFILE.GP_KEY
                 INTO :SQL-GP-KEY
                 FROM DBO.GPFILE
                WHERE GPFILE.GP_KEY = 1
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - W H E N E V E R
      *
      *=================================================================
      * IN  : NONE
      * OUT : NONE
      * DESC: DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      *       GPFILE SINCE IT SHOULD BE QUICK WITH ONLY HAVING ONE ROW.
      * NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *          CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *          AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
      ******************************************************************
       SQL-CONNECT-WHENEVER.

           EXEC SQL WHENEVER SQLERROR
              PERFORM SQL-IO-VALIDATION
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - D I S C O N N E C T
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : SQL-CONNECT-WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      *=================================================================
      * ***NOTE*** NEED TO HAVE SQL-DISCONNECT IN LIBIO/CONNECT_SQL AND
      *            LIBIO/DISCONNECT_SQL
      ******************************************************************
       SQL-DISCONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO HAVE WHENEVER SQLERROR NOT CALL TO SQL-IO-VALIDATION
      *    FOR CLOSING THE CONNECTION. THE USE IN LIBIO/DISCONNECT_SQL
      *    WILL NOT HAVE SQL-IO-VALIDATION SO NEED TO SHUT IT OFF
      *    (CONTINUE) AND DISCONNECT THEN PUT BACK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.

           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.

      *KC  IF ( SQLCODE < 0 )     *> NO NEED TO CHECK SINCE EXITING
      *KC     PERFORM SQL-ERROR.  *> APPLICATION VIA `STOP RUN`

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED THE FOLLOWING FOR LIBIO/CONNECT_SQL
      *    NEED TO COMMENT OUT THE FOLLOWING FOR LIBIO/DISCONNECT_SQL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES     TO SQL-CONNECT-WORKERS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      *    NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *          CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *          AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL WHENEVER SQLERROR
              PERFORM SQL-IO-VALIDATION
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - D E B U G
      *
      *=================================================================
      * IN  : SQLCA
      * OUT : NOTHING
      * DESC: DISPLAY MESSAGE BOX OF SQLCA SETTINGS FOR DEBUG
      ******************************************************************
       SQL-DEBUG.

           MOVE SPACES                TO SQL-DEBUG-BUF
           MOVE "CODE : "             TO SQL-DEBUG-01-SQLCODEX
           MOVE SQLCODE               TO SQL-DEBUG-01-SQLCODE
           MOVE "CABC : "             TO SQL-DEBUG-01-SQLCABCX
           MOVE SQLCABC               TO SQL-DEBUG-01-SQLCABC
           MOVE "CAID: "              TO SQL-DEBUG-01-SQLCAIDX
           MOVE SQLCAID               TO SQL-DEBUG-01-SQLCAID
           MOVE "STATE: "             TO SQL-DEBUG-02-SQLSTATEX
           MOVE SQLSTATE              TO SQL-DEBUG-02-SQLSTATE
           MOVE "ERRML: "             TO SQL-DEBUG-02-SQLERRMLX
           MOVE SQLERRML              TO SQL-DEBUG-02-SQLERRML
           MOVE "ERRPL: "             TO SQL-DEBUG-02-SQLERRPLX
           MOVE SQLERRPL              TO SQL-DEBUG-02-SQLERRPL
           MOVE "WARN: "              TO SQL-DEBUG-02-SQLWARNX
           MOVE SQLWARN0              TO SQL-DEBUG-02-SQLWARN0
           MOVE SQLWARN1              TO SQL-DEBUG-02-SQLWARN1
           MOVE SQLWARN2              TO SQL-DEBUG-02-SQLWARN2
           MOVE SQLWARN3              TO SQL-DEBUG-02-SQLWARN3
           MOVE SQLWARN4              TO SQL-DEBUG-02-SQLWARN4
           MOVE SQLWARN5              TO SQL-DEBUG-02-SQLWARN5
           MOVE SQLWARN6              TO SQL-DEBUG-02-SQLWARN6
           MOVE SQLWARN7              TO SQL-DEBUG-02-SQLWARN7
           MOVE SQLWARN8              TO SQL-DEBUG-02-SQLWARN8
           MOVE SQLWARN9              TO SQL-DEBUG-02-SQLWARN9
           MOVE SQLWARNA              TO SQL-DEBUG-02-SQLWARNA
           MOVE "ERRD : "             TO SQL-DEBUG-03-SQLERRDX
           MOVE SQLERRD(1)            TO SQL-DEBUG-03-SQLERRD-1
           MOVE SQLERRD(2)            TO SQL-DEBUG-03-SQLERRD-2
           MOVE SQLERRD(3)            TO SQL-DEBUG-03-SQLERRD-3
           MOVE SQLERRD(4)            TO SQL-DEBUG-04-SQLERRD-4
           MOVE SQLERRD(5)            TO SQL-DEBUG-04-SQLERRD-5
           MOVE SQLERRD(6)            TO SQL-DEBUG-04-SQLERRD-6
           STRING "------------------------------",
                  "[ ERRMC ]",
                  "-------------------------------"
                  DELIMITED BY SIZE INTO SQL-DEBUG-05
           MOVE SQLERRMC              TO SQL-DEBUG-06
           STRING "------------------------------",
                  "[ ERRPC ]",
                  "-------------------------------"
                  DELIMITED BY SIZE INTO SQL-DEBUG-07
           MOVE SQLERRPC              TO SQL-DEBUG-08.

           DISPLAY MESSAGE BOX SQL-DEBUG-01, X"0A"
                               SQL-DEBUG-02, X"0A"
                               SQL-DEBUG-03, X"0A"
                               SQL-DEBUG-04, X"0A"
                               SQL-DEBUG-05, X"0A"
                               SQL-DEBUG-06, X"0A"
                               SQL-DEBUG-07, X"0A"
                               SQL-DEBUG-08.

      ******************************************************************
      *
      *    S Q L - E R R O R
      *
      *=================================================================
      * IN  : E-MSG
      *       E-KEY
      *       SQLCA
      *       SQL-CONNECT-WORKERS
      * OUT : IO-FG   [0=GOOD; BAD!=0]
      * DESC: DISPLAY MESSAGE BOX OF SQL SERVER CONNECTION ERROR;
      *       DISCONNECT FROM SERVER
      ******************************************************************
       SQL-ERROR.

           MOVE SPACES TO SQL-ERROR-BUF.

           STRING "STOP!!!  ",
                  "CALL HOME OFFICE FOR IT SUPPORT IMMEDIATELY!!!"
                  DELIMITED BY SIZE INTO SQL-ERROR-01.

           STRING "STOP!!!  ",
                  "AN ERROR HAS OCCURED --- DO NOT PROCEED!!!"
                  DELIMITED BY SIZE INTO SQL-ERROR-02.

           STRING E-MSG, " --- FILE: ", E-FILE,
                  " --- PROGRAM: ", E-PROG
                  DELIMITED BY "  " INTO SQL-ERROR-04.

           MOVE "CODE: "  TO SQL-ERROR-05-SQLCODEX.
           MOVE SQLCODE   TO SQL-ERROR-05-SQLCODE.
           MOVE "STATE: " TO SQL-ERROR-05-SQLSTATEX.
           MOVE SQLSTATE  TO SQL-ERROR-05-SQLSTATE.
           MOVE "ERRML: " TO SQL-ERROR-05-SQLERRMLX.
           MOVE SQLERRML  TO SQL-ERROR-05-SQLERRML.
           MOVE "ERRPC: " TO SQL-ERROR-05-SQLERRPLX.
           MOVE SQLERRPL  TO SQL-ERROR-05-SQLERRPL.

           IF ( E-MSG4 = "DELE" ) OR
              ( E-MSG4 = "REWR" ) OR
              ( E-MSG4 = "WRIT" )
              MOVE "KEY : " TO SQL-ERROR-06-KEYX
              MOVE E-KEYX   TO SQL-ERROR-06-KEY.

           MOVE SQLERRMC  TO SQL-ERROR-07.
           MOVE SQLERRPC  TO SQL-ERROR-08.

           DISPLAY MESSAGE BOX SQL-ERROR-01, X"0A"
                               SQL-ERROR-02, X"0A"
                               SQL-ERROR-03, X"0A"
                               SQL-ERROR-04, X"0A"
                               SQL-ERROR-05, X"0A"
                               SQL-ERROR-06, X"0A"
                               SQL-ERROR-07, X"0A"
                               SQL-ERROR-08.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NOTE: WE ARE DOING A SQL-DISCONNECT AT THE END OF SQL-ERROR
      *        TO FORCE PROGRAMS TO STOP EXECUTING.  FOR EXAMPLE,
      *        LP/ACRERN CALLS TO LP/AC1ERN WHICH IF AC1ERN ERRORS
      *        WE WANT WHEN RETURNING BACK TO ACRERN TO HAVE THE
      *        CONNECTION CLOSED ("THE CONNECTION DOES NOT EXIST") THUS
      *        CAUSING AN ERROR ON ANY IO OPERATIONS (BLOCK UPDATES;
      *        WRITE/REWRITE).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SQL-DISCONNECT.
           MOVE 9 TO IO-FG.          

      ******************************************************************
      *
      *    S Q L - G E T - D A T E
      *
      *=================================================================
      * IN  : SQL-DATE-YYYY-MM-DD
      * OUT : SQL-DATE-YYYYMMDD
      * DESC: CONVERT DATE FROM YYYY-MM-DD TO YYYYMMDD FORMAT;
      *       NEEDED FOR 'GET' FIELDS ROUTINES.
      ******************************************************************
       SQL-GET-DATE.

           IF ( SQL-DATE-YYYY-MM-DD = "1900-01-01" )
              MOVE ZEROES TO SQL-DATE-YYYYMMDD
           ELSE
              INSPECT SQL-DATE-YYYY-MM-DD REPLACING ALL SPACES BY ZEROES
              MOVE SQL-DATE-YYYY-MM-DD-YYYY TO SQL-DATE-YYYYMMDD-YYYY
              MOVE SQL-DATE-YYYY-MM-DD-MM   TO SQL-DATE-YYYYMMDD-MM
              MOVE SQL-DATE-YYYY-MM-DD-DD   TO SQL-DATE-YYYYMMDD-DD.

      ******************************************************************
      *
      *    S Q L - S E T - D A T E
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: CONVERT DATE FROM YYYYMMDD TO YYYY-MM-DD FORMAT;
      *       NEEDED FOR 'SET' FIELDS ROUTINES.
      ******************************************************************
       SQL-SET-DATE.

           IF ( SQL-DATE-YYYYMMDD = ZEROES )
              MOVE "1900-01-01" TO SQL-DATE-YYYY-MM-DD
           ELSE
              MOVE SQL-DATE-YYYYMMDD TO SQL-DATE-YYYY-MM-DD-NUM
              INSPECT SQL-DATE-YYYY-MM-DD-NUM REPLACING ALL "/" BY "-".

      ******************************************************************
      ******************************************************************
      ******************************************************************
      * THE FOLLOWING NEEDED FOR PRE-COMPILE; NOT NEEDED IN COPY MEMBERS
      ******************************************************************
      ******************************************************************
      ******************************************************************
           EXIT PROGRAM.
