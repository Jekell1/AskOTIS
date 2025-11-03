      * COPYMEMBER: LIBIO/CONNECT_SQL.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBIO/CONNECT_SQL
      * DESCRIPTION   : SQL SERVER CONNECTION ROUTINES
      * PRE-COMPILE   : IO/IOCONN.CBL
      *
      * NEEDED COPYS  : LIBGB/FILEWK
      *                 LIBGB/DECLARE   -OR-
      *                 LIBGB/DECLAREOD -OR-
      *                 LIBGB/DECLRP
      *
      *=================================================================
      * REV:
      * JKC 2021-0115 ***NEW***
      * JKC 2021-0223 CLEANING UP SOME OF THE CODE.
      * JKC 2021-0324 ADDED SQL-GET-DATE & SQL-SET-DATE.
      * JKC 2021-0430 CHANGES MADE WHILE WORKING ON DECLARE ROUTINES.
      * JKC 2021-0511 CHANGED NAME FROM SQLCONNECT TO CONNECT_SQL
      * JKC 2021-0714 ADDED DISPLAY OF PROGRAM NAME IN SQL-ERROR;
      *               FORM-PATHNAME(16:6)
      * JKC 2020-0726 ADDED EXT-SQL-CONNECT-STAT FOR CHECKING IF 
      *               CONNECTION TO SQL SERVER HAS ALREADY BEEN MADE;
      *               USED TO ONLY CONNECT ONE-TIME INSTEAD OF MULTIPLE
      *               TIMES IN EVERY PROGRAM.
      *
      * JKC 2021-0816 CHANGED SQL-GET-DATE AND SQL-SET-DATE TO CHECK
      *               FOR ZERO DATES & '1900-01-01'.
      *
      * JKC 2021-0817 UPDATED FROM XPC/IOCONN.PRE FOR LATEST CHANGES.
      * JKC 2021-0927 CHANGED SQL-ERROR SECTION TO ALWAYS DISPLAY
      *               E-KEYX NO MATTER WHAT IO TYPE (E-MSG4).
      *
      * JKC 2022-0209 MADE CHANGE TO SET SQL CONNECTION FIELDS AT THE
      *               START OF THE 'SQL-CONNECT' PARAGRAPH FOR POTENTIAL
      *               FUTURE USE OF HAVING MULTIPLE CONNECTIONS.
      *               DID SOME CLEANUP OF UNNEED CODE THAT WAS USED
      *               FOR A TWO-STEP COMPILE PROCESS; WE USE A ONE-STEP
      *               COMPILE GOING FORWARD AT THIS POINT.
      *
      * JKC 2022-0214 MADE CHANGE TO SQL-DISCONNECT TO 
      *               `MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT`
      *               DUE TO ISSUE FOUND WITH CLOSING CURSORS VIA
      *               SQL-ERROR;
      *               IN SQL-IO-VALIDATION ROUTINE ADDED A CHECK TO SEE
      *               IF THE SQL CONNECTION (EXT-ACUSQL-CONNECT-STAT)
      *               IS OPEN (GOOD) THEN `PERFORM CLOSE-FILES`;
      *               NEED TO ONLY CLOSE FILES/TABLES WHEN THERE IS
      *               AN SQL CONNECTION OTHERWISE YOU CAN GET INTO
      *               AN INIFITE LOOP DUE TO THE SQL-DISCONNECT IN
      *               SQL-ERROR WHICH THEN YOU CANNOT CLOSE CURSORS
      *               IF THE CONNECTION HAS BEEN CLOSED.
      *
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET THE SQL CONNECTION FIELDS RIGHT AWAY;
      *    IF WE MAKE MULTIPLE CONNNECTIONS FOR EACH/SEPERATE TABLES
      *    THEN THESE SQL CONNECTION FIELDS NEED TO BE SET SO THAT
      *    THEY CAN BE USED FOR EACH IO ROUTINE NEEDED;
      *    SQL CONNECTION FIELDS WERE ONLY BEING POPULATED WHEN
      *    GOING INTO 'SQL-CONNECT-MAKE' WHICH MAY NOT BE DONE IF
      *    THE CONNECT STAT WAS GOOD.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-ACUSQL-ODBC-NAME TO SQL-CONNECT-SERVER.
           MOVE EXT-ACUSQL-LOGIN     TO SQL-CONNECT-USERID.
           MOVE EXT-ACUSQL-PSWD      TO SQL-CONNECT-PSWD.
      *??? MOVE "WAAZA34SQL-DEV'     TO SQL-CONNECT-SERVER
      *??? MOVE "DB_OWNER"    	     TO SQL-CONNECT-OWNER
      *??? MOVE "OTIS"               TO SQL-CONNECT-DATABASE

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      * IN  : SQL-CONNECT-WORKERS
      * OUT : SQLCA WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      ******************************************************************
       SQL-CONNECT-MAKE.

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
           MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT.

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

           MOVE "KEY : "  TO SQL-ERROR-06-KEYX.
           MOVE E-KEYX    TO SQL-ERROR-06-KEY.

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

