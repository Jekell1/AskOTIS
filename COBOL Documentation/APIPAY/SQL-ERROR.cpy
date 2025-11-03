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

           IF ( EXT-TCLP-TRACE-UPDATE )
              STRING "ERROR: E-MSG, " | FILE = ", E-FILE
                     DELIMITED BY "  " INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME MESS
              CANCEL "TRACE"

              STRING "ERROR: KEY = ", E-KEYX
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE"

              STRING "ERROR: STATE = ", SQLSTATE
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE"

              STRING "ERROR: ", SQLERRMC
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SQL-ERROR-BUF.

           STRING "STOP!!!  ",
                  "CALL HOME OFFICE FOR IT SUPPORT IMMEDIATELY!!!"
                  DELIMITED BY SIZE INTO SQL-ERROR-01.

           STRING "STOP!!!  ",
                  "AN ERROR HAS OCCURED --- DO NOT PROCEED!!!"
                  DELIMITED BY SIZE INTO SQL-ERROR-02.

           STRING E-MSG, " --- FILE: ", E-FILE,
                  " --- PROGRAM: ", E-PROG(16:7)
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

           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE SQL-ERROR-04 TO EXT-API-MESS
           ELSE
           IF ( SQL-ERROR-DISPLAY = "Y" )
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
