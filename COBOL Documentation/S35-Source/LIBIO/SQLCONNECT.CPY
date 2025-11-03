      * COPYMEMBER: LIBIO/SQLCONNECT.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBIO/SQLCONNECT
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
      *
      ******************************************************************
      *
      *    S Q L - C O N N E C T
      *
      *=================================================================
      * IN  : EXT-ACUSQL-CONNECT-WORKERS
      * OUT : SQL-CONNECT-WORKERS
      *       IO-FG    [0 = GOOD; NOT 0 = BAD]
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      ******************************************************************
       SQL-CONNECT.

           MOVE EXT-ACUSQL-ODBC-NAME TO SQL-CONNECT-SERVER.
           MOVE EXT-ACUSQL-LOGIN     TO SQL-CONNECT-USERID.
           MOVE EXT-ACUSQL-PSWD      TO SQL-CONNECT-PSWD.

      *??? MOVE "WAAZA34SQL-DEV'     TO SQL-CONNECT-SERVER
      *??? MOVE "DB_OWNER"    	     TO SQL-CONNECT-OWNER
      *??? MOVE "OTIS"               TO SQL-CONNECT-DATABASE
           
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MAKE CONNECTION TO SQL SERVER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           PERFORM CALL "SQL$START" END-CALL 

           CALL "SQL$DISCONNECTALL"  END-CALL 

            END-PERFORM
           .

           
           PERFORM CALL "SQL$START" END-CALL 

           CALL "SQL$CONNECT" USING SQL-CONNECT-SERVER 'SQLCON' SQL-CONN
      -    ECT-USERID SQL-CONNECT-PSWD  END-CALL 

            END-PERFORM
           .

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 0 )
              MOVE STAT---GOOD TO SQL-CONNECT-STAT
           ELSE
              MOVE SPACES      TO SQL-CONNECT-WORKERS
              MOVE STAT---BAD  TO SQL-CONNECT-STAT.
           
      ******************************************************************
      *
      *    S Q L - D I S C O N N E C T
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : SQL-CONNECT-WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      ******************************************************************
       SQL-DISCONNECT.

           
           PERFORM CALL "SQL$START" END-CALL 

           CALL "SQL$COMMIT"  END-CALL 

            END-PERFORM
           .
           
           PERFORM CALL "SQL$START" END-CALL 

           CALL "SQL$DISCONNECTALL"  END-CALL 

            END-PERFORM
           .

           IF ( SQLCODE < 0 )
              PERFORM SQL-ERROR.

           MOVE SPACES     TO SQL-CONNECT-WORKERS.
      *    MOVE SQLCODE    TO SQL-CONNECT-SQLCODE.
           MOVE STAT---BAD TO SQL-CONNECT-STAT.

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

      *%%% IF ( SQLCODE NOT = ZEROES )
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
               MOVE SQLERRPC              TO SQL-DEBUG-08

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

           STRING E-MSG, " --- FILE: ", E-FILE
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

           INSPECT SQL-DATE-YYYY-MM-DD REPLACING ALL SPACES BY ZEROES.
           MOVE SQL-DATE-YYYY-MM-DD-YYYY TO SQL-DATE-YYYYMMDD-YYYY.
           MOVE SQL-DATE-YYYY-MM-DD-MM   TO SQL-DATE-YYYYMMDD-MM.
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

           MOVE SQL-DATE-YYYYMMDD TO SQL-DATE-YYYY-MM-DD-NUM.
           INSPECT SQL-DATE-YYYY-MM-DD-NUM REPLACING ALL "/" BY "-".

