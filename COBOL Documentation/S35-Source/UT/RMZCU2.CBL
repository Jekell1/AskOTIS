       IDENTIFICATION DIVISION.
       PROGRAM-ID.                         RMZCU2.
       AUTHOR.                             rmz.
       REMARKS.
	   Show the execution of a stored procedure using a cursor.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RMZCU2 S35 03/31/23-09:23:42 >".
       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  C-RECORD.
           05  C-GR-KEY                            PIC X(35).
           05  C-MEMBER-ID                         PIC 9(04).

       01  num-rows                            pic s9(5) comp-5.
       01  num-rows-read                       pic s9(5) comp-5.
       01  ret-code                            pic s9(4) comp-5.
       01  dsn-name                            pic x(12).
       01  userid                              pic x(8).
       01  passwd.
           49 passwd-length                    PIC s9(4) comp-5 value 0.
           49 passwd-name                      PIC x(18).
       EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
       PROCEDURE DIVISION.
       Main Section.
           EXEC SQL WHENEVER SQLERROR GO TO Error-Exit END-EXEC.
           Display "This program demonstrates connecting to a SQL Server
      -    " instance and executing".
           display "a stored procedure (SP_BRANCH_GROUP).  Hopefully you
      -    " have executed 'create' to".
           display "create and populate the table, and installed the sto
      -    "red procedure in".
           display "'br_gr.sql' into a location that it can be found.
      -    "".
           perform initialization-routine.
           perform make-connection.
           perform execute-it.
           perform disconnect-connection.
           Display "Program Complete. Press Enter to Exit."
           accept omitted.
           stop run.

       execute-it.
           display "Enter the start group to search for: ", no.
           accept BEGIN-GR-ID.
           display "Enter the end group to search for: ", no.
           accept END-GR-ID.
           EXEC SQL WHENEVER SQLERROR GO TO Error-Exit END-EXEC.
           EXEC SQL declare spcursor cursor for
                    :ret-code = exec SP_BRANCH_GROUP 
                  ( :BEGIN-GR-ID,
                    :END-GR-ID
                  )
           END-EXEC.

	   EXEC SQL
	       open spcursor
	   END-EXEC.

	   move 0 to num-rows-read.
	   perform until SQLCODE not = 0
	       EXEC SQL
	           FETCH spcursor into
                    :C-GR-KEY,
                    :C-MEMBER-ID
	       END-EXEC
               if sqlcode < 0
                   perform error-exit
               end-if
	       if SQLCODE = 0
		   add 1 to num-rows-read
	           display "Group Key: ", C-GR-KEY
                   display "Branch Id: "  C-MEMBER-ID
                   display " "
	       end-if
	   end-perform.
           EXEC SQL
               close spcursor
           END-EXEC.

      -	   if num-rows not = num-rows-read
      -	       display "stored procedure error, " num-rows,
      -	               " not = ", num-rows-read
      -	   end-if.

      *copy "common.cpy".
       initialization-routine.
           display "Enter the server name: ", no advancing.
           accept dsn-name.
           display "Enter your user id (or press RETURN to use Windows A
      -           "uthentication): ", no.
           accept userid.
	   if userid not = spaces
               display "Enter your password : ", no
               accept passwd-name

      * Passwords in a CONNECT statement must be entered in a VARCHAR format
      * with the length of the input string.
               inspect passwd-name tallying passwd-length for characters
                  before initial " "
	   end-if.

       make-connection.
           if userid not = spaces
	       EXEC SQL CONNECT TO :dsn-name as C1
		       USER :userid USING :passwd
	       END-EXEC
	   else
	       EXEC SQL CONNECT TO :dsn-name as C1
	       END-EXEC
	   end-if.
           if sqlcode < 0 perform error-exit.
           if sqlcode = 0 display "Connection successful".

       disconnect-connection.
           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           if sqlcode < 0 perform error-exit.

       Error-Exit.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           display "SQL Error: SQLCODE  " SQLCODE of SQLCA.
           display "           SQLSTATE " SQLSTATE of SQLCA.
           display SQLERRMC OF SQLCA.
           accept omitted.
           EXEC SQL DISCONNECT ALL END-EXEC.
           stop run.
