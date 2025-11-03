       IDENTIFICATION DIVISION.
       PROGRAM-ID.                         LNTEST.
       AUTHOR.                             RMZ.
       REMARKS.
	   SHOW THE EXECUTION OF A STORED PROCEDURE USING A CURSOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./UT/LNTEST.CBL S35 04/05/23-13:16:52 >".

       01  QLN-REC.
           03  QLN-OWNBR                             PIC 9(4).         
           03  QLN-ACCTNO                            PIC 9(8).        

      *=================================================================
      * P-TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMER_LNK.CPY
      * P-TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMEWK.CPY
      * ANY CHANGE HERE MUST BE MADE IN THE OTHER COPY MEMBERS!!!!!
      *=================================================================
       01  P-TIMER.
           03  P-TIMER-TYPE-SET             PIC X(1).
               88  P-TIMER-TYPE-SET-VALID   VALUE "E" "R" "U".

           03  P-TIMER-LINK-BUF.
               05  P-TIMER-STAT             PIC X(02).
               05  P-TIMER-DAYSEC-BEG       PIC 99V9(7).
               05  P-TIMER-DAYSEC-END       PIC 99V9(7).
               05  P-TIMER-DAYSEC-ELAPSED   PIC 99V9(7).
               05  P-TIMER-BEG-EDIT.
                   07  P-TIMER-BEG-HHMMSS   PIC 99/99/99.
                   07  FILLER               PIC X(01).
                   07  P-TIMER-BEG-AMPM     PIC X(02).
               05  P-TIMER-END-EDIT.
                   07  P-TIMER-END-HHMMSS   PIC 99/99/99.
                   07  FILLER               PIC X(01).
                   07  P-TIMER-END-AMPM     PIC X(02).
               05  P-TIMER-ELAPSED-EDIT     PIC 99/99/99/99.
               05  P-TIMER-TYPE             PIC X(1).
                   88  P-TIMER-TYPE-EXCEL   VALUE "E".
                   88  P-TIMER-TYPE-REPORT  VALUE "R".
                   88  P-TIMER-TYPE-UTILITY VALUE "U".
                   88  P-TIMER-TYPE-VALID   VALUE "E" "R" "U".

       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  sqlserver     pic x(64) value spaces.
       01  sqluser       pic x(32) value spaces.
       01  sqlpassword   pic x(32) value spaces.

       01  WBEG-OWNBR    PIC 9(4)  VALUE ZEROS.
       01  WEND-OWNBR    PIC 9(4)  VALUE ZEROS.
       01  WBEG-ACCTNO   PIC 9(8)  VALUE ZEROS.
       01  WEND-ACCTNO   PIC 9(8)  VALUE ZEROS. 

       01  C-RECORD.
           05  C-COUNT                         PIC 9(5) COMP-3.
           05  C-REC-GROUP                     PIC X(204000).
           05  C-REC-INDIV  REDEFINES C-REC-GROUP.            
               07 C-REC     PIC X(12)          OCCURS 17000 TIMES.  
                  
       01  SUB           PIC 99999             VALUE ZEROS.

       01  ret-code                            pic s9(4) comp-5.
       01  dsn-name                            pic x(12).
       01  userid                              pic x(8).
       01  passwd.
           49 passwd-length                    PIC s9(4) comp-5 value 0.
           49 passwd-name                      PIC x(18).

       EXEC SQL END DECLARE SECTION END-EXEC.

       COPY "LIBGB/GETFMW.CPY".

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN SECTION.
           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.
           DISPLAY "THIS PROGRAM DEMONSTRATES CONNECTING TO A SQL SERVER
      -    " INSTANCE AND EXECUTING".
           DISPLAY "A STORED PROCEDURE (SP_LNFILE_BLOCK).  HOPEFULLY YOU 
      -    " HAVE EXECUTED 'CREATE' TO".
           DISPLAY "CREATE AND POPULATE THE TABLE, AND INSTALLED THE STO
      -    "RED PROCEDURE IN".
           DISPLAY "'SP_LNFILE_BLOB.SQL' INTO A LOCATION THAT IT CAN BE 
      -    "FOUND.".
           PERFORM INITIALIZATION-ROUTINE.
           PERFORM MAKE-CONNECTION.
           PERFORM EXECUTE-IT.
           PERFORM DISCONNECT-CONNECTION.
           DISPLAY "PROGRAM COMPLETE. PRESS ENTER TO EXIT."
           ACCEPT OMITTED.
           STOP RUN.

       EXECUTE-IT.
           DISPLAY "ENTER THE BRANCH OWNBR: ", no.
           accept wbeg-ownbr.
      *    DISPLAY "ENTER THE ENDING OWNBR: ", no.
      *    accept wend-ownbr.
           DISPLAY "ENTER THE BEGINNING ACCTNO: ", no.
           accept wbeg-acctno.
           DISPLAY "ENTER THE ENDING ACCTNO: ", no.
           accept wend-acctno.
           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.
           EXEC SQL declare splncursor cursor for
                    :ret-code = exec sp_lnfile_blob ( :wbeg-ownbr,
                    :wbeg-acctno, :wend-acctno )
           END-EXEC.

      *             :wend-ownbr, :wbeg-acctno, :wend-acctno )

           DISPLAY "ENTERING OPEN CURSOR.".

	   EXEC SQL
	       open splncursor
	   END-EXEC.

           DISPLAY "EXITING OPEN CURSOR.".

      *    MOVE 0 TO NUM-ROWS-READ.

           DISPLAY "ENTERING FETCH.".

	   PERFORM UNTIL SQLCODE NOT = 0
	       EXEC SQL
	           FETCH splncursor into
                    :c-count, :c-rec-group           
	       END-EXEC
               DISPLAY "ACTUAL FETCH COMPLETE.", SQLCODE OF SQLCA

               IF SQLCODE < 0
                   PERFORM ERROR-EXIT
               END-IF
	       IF SQLCODE = 0
      *		   ADD 1 TO NUM-ROWS-READ
                   MOVE 1 TO SUB 
                   PERFORM UNTIL (SUB > C-COUNT)
                   MOVE C-REC(SUB) TO  QLN-REC
                   IF    (SUB = 1     OR SUB = 1000  OR SUB = 2000 
                       OR SUB = 3000  OR SUB = 4000  OR SUB = 5000
                       OR SUB = 6000  OR SUB = 7000  OR SUB = 8000
                       OR SUB = 9000  OR SUB = 10000 
                       OR SUB = 11000 OR SUB = 12000
                       OR SUB = 13000 OR SUB = 14000 OR SUB = 15000 
                       OR SUB = 16000 OR SUB = 17000) 
      *                OR SUB = 18000
      *                OR SUB = 19000 OR SUB = 20000)
	               DISPLAY "REC: ", C-COUNT, SUB, QLN-OWNBR,
                                        QLN-ACCTNO 
                   END-IF 
                   ADD 1 TO SUB      
                   END-PERFORM 
	       END-IF
	   END-PERFORM.

           DISPLAY "EXITING FETCH.".

           EXEC SQL
               close splncursor
           END-EXEC.
      * IF NUM-ROWS NOT = NUM-ROWS-READ
      *	       DISPLAY "STORED PROCEDURE ERROR, " NUM-ROWS,
      *	               " NOT = ", NUM-ROWS-READ
      * END-IF.
      *COPY "COMMON.CPY".


       INITIALIZATION-ROUTINE.


      *     display "Enter the server name: ", no advancing.
      *     accept dsn-name.
      *     display "Enter your user id (or press RETURN to use Windows A
      *-           "uthentication): ", no.
      *     accept userid.
      *     if userid not = spaces
      *         display "Enter your password : ", no
      *         accept passwd-name

      * Passwords in a CONNECT statement must be entered in a VARCHAR format
      * with the length of the input string.
               inspect passwd-name tallying passwd-length for characters
                  before initial " "
      *     end-if.

           MOVE "R"    TO P-TIMER-TYPE-SET.
           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.

           IF ( P-TIMER-TYPE-SET-VALID )
               MOVE P-TIMER-TYPE-SET TO P-TIMER-TYPE
           ELSE
               MOVE "R" TO P-TIMER-TYPE.     *> REPORT OPTION
 
           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

           DISPLAY "RUN START: " P-TIMER-BEG-EDIT.


       MAKE-CONNECTION.

           accept sqlserver from environment
                        "otis_dsn_name".
           accept userid from environment "otis_acusql_login".
           accept passwd-name from environment "otis_acusql_pswd".

           exec sql connect to :sqlserver as C1
                          user :userid using :passwd-name
           end-exec.

           exec sql whenever sqlerror go to error-exit end-exec.

           if sqlcode < 0 perform error-exit.

       DISCONNECT-CONNECTION.
           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           IF sqlcode < 0 PERFORM ERROR-EXIT.

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      *    PERFORM TIMER-STOP.
      *    MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.
           DISPLAY "RUN STOP: " P-TIMER-END-EDIT.
           INSPECT P-TIMER-ELAPSED-EDIT REPLACING ALL "/" BY "-".
           DISPLAY "RUN TIME: " P-TIMER-ELAPSED-EDIT.

       ERROR-EXIT.

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

      *    PERFORM TIMER-STOP.
      *    MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.
           DISPLAY "RUN STOP: " P-TIMER-END-EDIT.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           DISPLAY "SQL ERROR: SQLCODE  " SQLCODE OF SQLCA.
           DISPLAY "           SQLSTATE " SQLSTATE OF SQLCA.
           DISPLAY SQLERRMC OF SQLCA.
           accept omitted.
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
