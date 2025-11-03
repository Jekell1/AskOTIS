       IDENTIFICATION DIVISION.
       PROGRAM-ID.                         listcust2.
       AUTHOR.                             rlz.
       REMARKS.
	   Show the execution of a stored procedure using a cursor.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/SQLCU2 S35 03/31/23-09:24:30 >".
       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  C-RECORD.
           05  C-NUMBER                            PIC S9(3) COMP-5.
           05  C-FIRST-NAME                        PIC X(81).
           05  C-LAST-NAME                         PIC X(81).
           05  C-BIRTHDAY                          PIC X(10).
           05  C-INFO                              PIC X(11).

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
           display "a stored procedure (sp_listcust).  Hopefully you hav
      -    "e executed 'create' to".
           display "create and populate the table, and installed the sto
      -    "red procedure in".
           display "'listcust.sql' into a location that it can be found.
      -    "".
      *    perform initialization-routine.
           perform make-connection.
           perform execute-it.
           perform disconnect-connection.
           Display "Program Complete. Press Enter to Exit."
           accept omitted.
           stop run.

       execute-it.
           display "Enter the name to search for: ", no.
           accept c-last-name.
           EXEC SQL WHENEVER SQLERROR GO TO Error-Exit END-EXEC.
           EXEC SQL declare spcursor cursor for
                    :ret-code = exec sp_listcustomer ( :c-last-name,
                    :num-rows out )
           END-EXEC.

	   EXEC SQL
	       open spcursor
	   END-EXEC.

	   move 0 to num-rows-read.
	   perform until SQLCODE not = 0
	       EXEC SQL
	           FETCH spcursor into
                    :c-last-name, :c-first-name,
                    :c-birthday
	       END-EXEC
               if sqlcode < 0
                   perform error-exit
               end-if
	       if SQLCODE = 0
		   add 1 to num-rows-read
	           display "Last Name: ", c-last-name
                   display "First Name: " c-first-name
                   display "Birthday: " c-birthday
                   display " "
	       end-if
	   end-perform.
           EXEC SQL
               close spcursor
           END-EXEC.
	   if num-rows not = num-rows-read
	       display "stored procedure error, " num-rows,
	               " not = ", num-rows-read
	   end-if.

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
