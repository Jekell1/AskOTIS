      * COPYMEMBER: LIBGB/CONNECT_SQL.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/CONNECT_SQL
      * DESCRIPTION   : SQL SERVER CONNECTION ROUTINES
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
      * JKC 2022-0622 MOVED THIS FROM LIBIO TO LIBGB; LIBIO WAS REMOVED
      * BAH 2022.0627 RENAMED EXT-ACUSQL-ODBC-NAME TO EXT-ACUSQL-DSN-NAME

      * JKC 2022-0804 REMOVED `SQL-CONNECT-VERIFY' PARAGRAPH; REPLACED
      *          IT WITH `SELECT 1 INTO :QGP-KEY`.
      *
      * JKC 2022-0805 CHANGED :QGP-KEY TO BE :SQL-CONNECT-VERIFY.
      * JKC 2022-0912 ADDED SQL-TEXT-DECREMENT AND SQL-TEXT-INCREMENT
      *          ROUTINES; USED IN CERTAIN IO START PARAGRAPH;
      *          NEEDED FOR SQL WHERE CONSTRAINTS SO TO GET THE
      *          START PARAGRAPHS TO ACT LIKE THE VISION FILES.
      * JKC 2022-0913 ADDED SQL-DATE-DECREMENT AND SQL-DATE-INCREMENT
      *          ROUTINES; USED IN CERTAIN IO START PARAGRAPH;
      *          NEEDED FOR SQL WHERE CONSTRAINTS SO TO GET THE
      *          START PARAGRAPHS TO ACT LIKE THE VISION FILES.
      * JKC 2022-1013 CHANGE SQL-TEXT-INCREMENT ROUTINE; ADDED
      *          SQL-TEXT-INCREMENT-ALPHA.
      *
      * JKC 2023-0815 ADDED SQL-TEXT-ASCII; HAS ALL HEX VALUES OF
      *          ALL CHARACTERS (ALPHA, NUMBERS, SPECIAL); THERE WAS AN
      *          ISSUE IN THE TEXT INCREMENT ROUTINE WHERE A "60+" WAS
      *          BEING INCREMENTED TO "60 " WHICH RESETS FROM THE START
      *          GIVING BAD RESULTS IN SCAN WINDOW; NEEDED TO ALLOW THE
      *          USE OF SPECIAL CHARACTERS TO INCREMENT OR DECREMENT.
      *
      * JKC 2023-0815 FIXED ONE ISSUE IN SQL-TEXT-INCREMENT SO THAT
      *          IT INCREMENTS A TEXT WHEN THE LAST CHARACTER IS NOT
      *          IN THE POSITION OF THE MAX SIZE; "M  " NOW WILL BE
      *          "N  " INSTEAD OF "M! ".
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
           MOVE EXT-ACUSQL-DSN-NAME  TO SQL-CONNECT-SERVER.
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

           EXEC SQL              *> VERIFY SQL SERVER CONNECTION EXISTS
                SELECT 1 INTO :SQL-CONNECT-VERIFY
           END-EXEC.

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
      * ***NOTE*** NEED TO HAVE SQL-DISCONNECT IN LIBGB/CONNECT_SQL AND
      *            LIBGB/DISCONNECT_SQL
      ******************************************************************
       SQL-DISCONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO HAVE WHENEVER SQLERROR NOT CALL TO SQL-IO-VALIDATION
      *    FOR CLOSING THE CONNECTION. THE USE IN LIBGB/DISCONNECT_SQL
      *    WILL NOT HAVE SQL-IO-VALIDATION SO NEED TO SHUT IT OFF
      *    (CONTINUE) AND DISCONNECT THEN PUT BACK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.

           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.

      *KC  IF ( SQLCODE < 0 )     *> NO NEED TO CHECK SINCE EXITING
      *KC     PERFORM SQL-ERROR.  *> APPLICATION VIA `STOP RUN`

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED THE FOLLOWING FOR LIBGB/CONNECT_SQL
      *    NEED TO COMMENT OUT THE FOLLOWING FOR LIBGB/DISCONNECT_SQL
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
      *    S Q L - D A T E - D E C R E M E N T
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: TAKE INPUT DATE THEN DECREMENT IT BY ONE DAY THEN
      *       FORMAT IT THEN RETURN IT BACK;
      *       TO GET A START NOT GREATER THAN TO WORK IN SQL LIKE IT
      *       DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO
      *       SUBTRACT ONE TO THE WHERE SCAN BEG (WSBEG).
      ******************************************************************
       SQL-DATE-DECREMENT.
           MOVE SQL-DATE-YYYYMMDD          TO NDTE-DATE.
           MOVE -1                         TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE                  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.

      ******************************************************************
      *
      *    S Q L - D A T E - I N C R E M E N T
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: TAKE INPUT DATE THEN INCREMENT IT BY ONE DAY THEN
      *       FORMAT IT THEN RETURN IT BACK;
      *       TO GET A START TO WORK IN SQL LIKE IT DOES IN VISION FILES
      *       FOR MULTIPLE SCAN PAGES, NEED TO ADD ONE TO THE
      *       WHERE SCAN BEG (WSBEG).
      ******************************************************************
       SQL-DATE-INCREMENT.
           MOVE SQL-DATE-YYYYMMDD          TO NDTE-DATE.
           MOVE 1                          TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE                  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.

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
      *
      *    S Q L - T E X T - D E C R E M E N T
      *
      *=================================================================
      * IN  : SQL-TEXT
      * OUT : SQL-TEXT
      * DESC: USED TO DECREMENT THE LAST CHARACTER OF A STRING SO THAT
      *       A START-NOT-GREATER PARAGRAPH (DECLARE CURSOR) WORKS LIKE
      *       A VISION FILE IN A MULTI-PAGE SCAN WINDOW;
      *       FIND THE LAST CHARACTER IN A TEXT STRING THEN DECREMENT
      *       IT BY ONE.  USING SQL-TEXT-ASCII WORKERS, WORK BACKWARDS:
      *
      *         " "     BECOMES " " (REMAINS SPACE; FIRST CHARACTER)
      *         "!"     BECOMES " "
      *         "1"     BECOMES "0"
      *         "B"     BECOMES "A" (UPPER B AND A)
      *         "~"     BECOMES "}"
      *
      *         "   "   BECOMES "   " (REMAINS SPACES; NO WHERE TO GO)
      *         "!  "   BECOMES "   "
      *         " ! "   BECOMES "   "
      *         "012"   BECOMES "011"
      *         "ABC"   BECOMES "ABB" (UPPER)
      *         "~~ "   BECOMES "~} "
      *         "~~!"   BECOMES "~~ "
      *         "~~~"   BECOMES "~~}"
      *
      ******************************************************************
       SQL-TEXT-DECREMENT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT FIELD IS SPACES, THEN KEEP IT SPACES (NO WHERE TO GO)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT = SPACES )
              MOVE ZEROES TO SQL-TEXT-SIZE
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIND THE LAST CHARACTER IN THE SIZE OF THE TEXT STRING BY
      *    SEARCHING FOR THE LAST CHARACTER THAT IS NOT A SPACE THEN
      *    USE THAT SUBSCRIPT TO SET SQL-TEXT-SIZE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO SQL-TEXT-SIZE.

           PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-MAX BY -1
                        UNTIL SQL-TEXT-S1 <= ZEROES

                   IF ( SQL-TEXTX(SQL-TEXT-S1) NOT = SPACES )
                      MOVE SQL-TEXT-S1 TO SQL-TEXT-SIZE
                      MOVE ZEROES      TO SQL-TEXT-S1
                   END-IF

           END-PERFORM.

           IF ( SQL-TEXT-SIZE = ZEROES )
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGE LAST CHARACTER FOUND BACKWARDS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SQL-TEXTX(SQL-TEXT-SIZE) TO SQL-TEXT-CURRENT.
           PERFORM SQL-TEXT-FIND-ASCII.
           MOVE SQL-TEXT-BEFORE          TO SQL-TEXTX(SQL-TEXT-SIZE).

      ******************************************************************
      *
      *    S Q L - T E X T - I N C R E M E N T
      *
      *=================================================================
      * IN  : SQL-TEXT
      *       SQL-TEXT-SIZE [SIZE OF THE COBOL FIELD SIZE = PIC X(##)]
      * OUT : SQL-TEXT
      * DESC: USED TO INCREMENT TEXT STRING.  NEED THE SIZE OF THE COBOL
      *       FIELD TO KNOW WHICH CHARACTER NEEDS TO BE INCREMENTED TO.
      *
      *       IF THE LAST CHARACTER (SQL-TEXT-SIZE) IS NOT SPACE, THEN
      *       INCREMENT THAT CHARACTER:
      *
      *         " "     BECOMES "!"
      *         "/"     BECOMES "0"
      *         "9"     BECOMES ":"
      *         "@"     BECOMES "A" (UPPER A)
      *         "Z"     BECOMES "[" (UPPER Z)
      *         "`"     BECOMES "A" (LOWER A)
      *         "Z"     BECOMES "{" (LOWER Z)
      *         "~"     BECOMES "~" (~ REMAINS ~; LAST CHARACTER)
      *
      *         "AB!"   BECOMES "AB"" (UPPER AB)
      *         "AB/"   BECOMES "AB0" (UPPER AB)
      *         "AB9"   BECOMES "AB:" (UPPER AB)
      *         "AB@"   BECOMES "ABA" (UPPER ABA)
      *         "ABZ"   BECOMES "AB[" (UPPER ABZ)
      *         "AB`"   BECOMES "ABA" (UPPER AB LOWER A)
      *         "ABZ"   BECOMES "AB{" (UPPER AB LOWER Z)
      *         "AB~"   BECOMES "AC " (UPPER AB~ || UPPER AC)
      *         "AZZ"   BECOMES "AZ[" (UPPER AZZ)
      *         "AZ~"   BECOMES "A[ " (UPPER AZ~)
      *         "A~~"   BECOMES "B  " (UPPER A~~)
      *         "~~~"   REMAINS "~~~" (NO WHERE TO GO TO)
      *
      *         "AB!  " BECOMES "AB!! " (UPPER AB)
      *         "AB/  " BECOMES "AB/! " (UPPER AB)
      *         "AB9  " BECOMES "AB9! " (UPPER AB)
      *         "AB@  " BECOMES "AB@! " (UPPER AB)
      *         "ABZ  " BECOMES "ABZ! " (UPPER ABZ)
      *         "AB`  " BECOMES "AB`! " (UPPER AB)
      *         "AB~  " BECOMES "AB~! " (UPPER AB)
      *         "~~~  " BECOMES "~~~! "
      *
      *         "A !"   BECOMES "A "" (UPPER A)
      *         "A /"   BECOMES "A 0" (UPPER A)
      *         "A 9"   BECOMES "A :" (UPPER A)
      *         "A @"   BECOMES "A A" (UPPER A A)
      *         "A Z"   BECOMES "A [" (UPPER ABZ)
      *         "A `"   BECOMES "A A" (UPPER A SPACE LOWER A)
      *         "A ~"   BECOMES "A! " (UPPER A)
      *         "~  "   BECOMES "~! " 
      *         "~~ "   BECOMES "~~!" 
      *
      ******************************************************************
       SQL-TEXT-INCREMENT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT FIELD IS SPACES, THEN MAKE IT A LEADING ! ("!     ")
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT = SPACES )
              MOVE SQL-TEXT-ASCIIX(2) TO SQL-TEXT        *> ! CHARACTER
              MOVE ZEROES             TO SQL-TEXT-SIZE
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF SQL-TEXT-SIZE IS NOT SET THEN FIND THE SIZE OF THE TEXT
      *    STRING BY SEARCHING FOR THE LAST CHARACTER THAT IS NOT A
      *    SPACE THEN USE THAT SUBSCRIPT TO SET SQL-TEXT-SIZE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-SIZE NOT NUMERIC ) OR
              ( SQL-TEXT-SIZE = ZEROES    )
              MOVE 01 TO SQL-TEXT-SIZE
              PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-MAX BY -1
                        UNTIL SQL-TEXT-S1 <= ZEROES

                      IF ( SQL-TEXTX(SQL-TEXT-S1) NOT = SPACES )
                         MOVE SQL-TEXT-S1 TO SQL-TEXT-SIZE
                         MOVE ZEROES      TO SQL-TEXT-S1
                      END-IF

              END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF TEXT SIZE = 1, INCREMENT EXECPT IF A `~`
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-SIZE = 1 )
              IF ( SQL-TEXTX(1) = "~" )
                 MOVE ZEROES         TO SQL-TEXT-SIZE
                 EXIT PARAGRAPH
              ELSE
                 MOVE SQL-TEXTX(1)   TO SQL-TEXT-CURRENT
                 PERFORM SQL-TEXT-FIND-ASCII
                 MOVE SQL-TEXT-AFTER TO SQL-TEXTX(1)
                 MOVE ZEROES         TO SQL-TEXT-SIZE
                 EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIND LAST CHARACTER THAT IS NOT A SPACE FROM THE SIZE OF
      *    STRING; NEED TO KNOW IF THE LAST CHARACTER IS ALSO THE
      *    LAST IN THE SIZE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SQL-TEXT-SIZE TO SQL-TEXT-LAST.

           PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-SIZE BY -1
                     UNTIL SQL-TEXT-S1 <= 0

                   IF ( SQL-TEXTX(SQL-TEXT-S1) NOT = SPACES )
                      MOVE SQL-TEXT-S1 TO SQL-TEXT-LAST
                      MOVE ZEROES      TO SQL-TEXT-S1
                   END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF LAST CHARACTER (NON-SPACE; SQL-TEXT-LAST) IS ALSO THE
      *    LAST POSITON OF THE STRING (SQL-TEXT-SIZE), THEN INCREMENT
      *    THAT POSITION EXCEPT IF A `~`
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-LAST                = SQL-TEXT-SIZE ) AND
              ( SQL-TEXTX(SQL-TEXT-LAST) NOT = "~"           )
              MOVE SQL-TEXTX(SQL-TEXT-LAST) TO SQL-TEXT-CURRENT
              PERFORM SQL-TEXT-FIND-ASCII
              MOVE SQL-TEXT-AFTER TO SQL-TEXTX(SQL-TEXT-LAST)
              MOVE ZEROES         TO SQL-TEXT-SIZE
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF LAST CHARACTER (NON-SPACE; SQL-TEXT-LAST) IS ***NOT*** THE
      *    LAST POSITON OF THE STRING (SQL-TEXT-SIZE), THEN:
      *     - IF CHARACTER IS NOT `~`, THEN INCREMENT THAT POSITION
      *     - IF CHARACTER IS `~`, THEN INCREMENT THE NEXT POSITION
      *       (SHOULD BE A SPACE; BECOMES A "!")
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( SQL-TEXT-LAST NOT = SQL-TEXT-SIZE )
      *       IF ( SQL-TEXTX(SQL-TEXT-LAST) NOT = "~" )
      *          MOVE SQL-TEXTX(SQL-TEXT-LAST) TO SQL-TEXT-CURRENT
      *          PERFORM SQL-TEXT-FIND-ASCII
      *          MOVE SQL-TEXT-AFTER TO SQL-TEXTX(SQL-TEXT-LAST)
      *          EXIT PARAGRAPH
      *       ELSE 
      *          MOVE SQL-TEXT-ASCIIX(2) TO SQL-TEXTX(SQL-TEXT-LAST + 1)
      *          MOVE ZEROES             TO SQL-TEXT-SIZE
      *          EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF LAST CHARACTER (NON-SPACE; SQL-TEXT-LAST) IS ***NOT*** THE
      *    LAST POSITON OF THE STRING (SQL-TEXT-SIZE), THEN INCREMENT
      *    THE NEXT POSITION (SHOULD BE A SPACE; BECOMES A "!")
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-LAST NOT = SQL-TEXT-SIZE )
              MOVE SQL-TEXT-ASCIIX(2) TO SQL-TEXTX(SQL-TEXT-LAST + 1)
              MOVE ZEROES             TO SQL-TEXT-SIZE
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LAST CHARACTER (SQL-TEXT-LAST) IS EQUAL TO LAST POSITION OF
      *    THE STRING (SQL-TEXT-SIZE) AND IS EQUAL TO A `~`.  NEED TO
      *    MAKE THAT POSITION A SPACE, THEN WORK BACKWARDS AND INCREMENT
      *    THAT POSITION UNLESS IT ALSO IS A `~` WHICH IF SO MAKE IT
      *    ALSO A SPACE AND REPEAT BACKWARDS TILL NON ~ CHARACTER FOUND:
      *
      *         "AZ~"   BECOMES "A[ " (UPPER AZ~)
      *         "A~~"   BECOMES "B  " (UPPER A~~)
      *         "A ~"   BECOMES "A! " (UPPER A)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SQL-TEXT-S1 FROM SQL-TEXT-LAST BY -1
                     UNTIL SQL-TEXT-S1 <= 0

                   IF ( SQL-TEXTX(SQL-TEXT-S1) = "~" )
                      MOVE SPACES TO SQL-TEXTX(SQL-TEXT-S1)
                   ELSE
                      MOVE SQL-TEXTX(SQL-TEXT-S1) TO SQL-TEXT-CURRENT
                      PERFORM SQL-TEXT-FIND-ASCII
                      MOVE SQL-TEXT-AFTER TO SQL-TEXTX(SQL-TEXT-S1)
                      MOVE ZEROES         TO SQL-TEXT-S1
                   END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF SQL-TEXT EQUAL SPACES, THEN FILL IT WITH ALL `~`;
      *    THE PREVIOUS `PERFORM VARYING` CLEARED (SPACES) TO SQL-TEXT
      *    SO NEED TO POPULATE IT WITH `~` SINCE THERE NEEDS TO GET TO
      *    MAX VALUE TO SIGNAL THERE WAS NO WHERE TO GO:
      *
      *         "~~~"   REMAINS "~~~" (NO WHERE TO GO TO)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT = SPACES )
              MOVE ALL "~" TO SQL-TEXT.

      ******************************************************************
      *
      *    S Q L - T E X T - F I N D - A S C I I
      *
      *=================================================================
      * IN  : SQL-TEXT-CURRENT
      * OUT : SQL-TEXT-AFTER
      *       SQL-TEXT-BEFORE
      * DESC: FIND THE CURRENT CHARACTER IN SQL-TEXT-ASCII THEN RETURN
      *       BACK THE CHARACTER BEFORE & AFTER THE CURRENT CHARACTER.
      ******************************************************************
       SQL-TEXT-FIND-ASCII.

           MOVE ZEROES TO SQL-TEXT-S4.

           PERFORM VARYING SQL-TEXT-S3 FROM 1 BY 1
                     UNTIL SQL-TEXT-S3 > SQL-TEXT-ASCII-MAX

                  IF ( SQL-TEXT-ASCIIX(SQL-TEXT-S3) = SQL-TEXT-CURRENT )
                     MOVE SQL-TEXT-S3        TO SQL-TEXT-S4
                     MOVE SQL-TEXT-ASCII-MAX TO SQL-TEXT-S3
                  END-IF

           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SQL-TEXT-S4 = ZEROES )
              MOVE SPACES TO SQL-TEXT-AFTER
                             SQL-TEXT-BEFORE
              EXIT PARAGRAPH.

           IF ( SQL-TEXT-S4 = 1 )
              MOVE SQL-TEXT-ASCIIX(2)               TO SQL-TEXT-AFTER
              MOVE SPACES                           TO SQL-TEXT-BEFORE
           ELSE
           IF ( SQL-TEXT-S4 = SQL-TEXT-ASCII-MAX )
              MOVE SPACES                           TO SQL-TEXT-AFTER
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 - 1) TO SQL-TEXT-BEFORE
           ELSE
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 + 1) TO SQL-TEXT-AFTER
              MOVE SQL-TEXT-ASCIIX(SQL-TEXT-S4 - 1) TO SQL-TEXT-BEFORE.

