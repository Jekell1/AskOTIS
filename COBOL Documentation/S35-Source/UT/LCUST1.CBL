       IDENTIFICATION DIVISION.
       PROGRAM-ID.                         listcust1.
       AUTHOR.                             rlz.
      * Copyright (C) 1995-2000,2020 Micro Focus or one of its affiliates.
      *
      * The only warranties for products and services of Micro Focus
      * and its affiliates and licensors ("Micro Focus") are set
      * forth in the express warranty statements accompanying such
      * products and services. Nothing herein should be construed as
      * constituting an additional warranty. Micro Focus shall not
      * be liable for technical or editorial errors or omissions
      * contained herein. The information contained herein is
      * subject to change without notice.
      *
      * Contains Confidential Information. Except as specifically
      * indicated otherwise, a valid license is required for possession,
      * use or copying. Consistent with FAR 12.211 and 12.212,
      * Commercial Computer Software, Computer Software Documentation,
      * and Technical Data for Commercial Items are licensed to the U.S.
      * Government under vendor's standard commercial license.

       REMARKS.
	   Show the execution of a stored procedure.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LCUST1 S35 03/31/23-09:23:36 >".
       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  sqlserver     pic x(64) value spaces.
       01  sqluser       pic x(32) value spaces.
       01  sqlpassword   pic x(32) value spaces.

       01  C-RECORD.
           05  C-NUMBER                            PIC S9(3) COMP-5.
           05  C-FIRST-NAME                        PIC X(80).
           05  C-LAST-NAME                         PIC X(80).
           05  C-BIRTHDAY                          PIC X(10).
           05  C-INFO                              PIC X(10).

       01  num-rows                            pic s9(5) comp-5.
       01  ret-code                            pic s9(4) comp-5.
       01  dsn-name                            pic x(20).
      *01  userid                              pic x(8).
       01  userid                              pic x(16).
       01  passwd.
      *kc  49 passwd-length                    PIC s9(4) comp-5 value 0.
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
      *kc  perform initialization-routine.
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
           EXEC SQL :ret-code = exec sp_listcustomer ( :c-last-name,
                    :num-rows out )
           END-EXEC.
           if sqlcode < 0 perform error-exit.
           display " Number of rows with that last name: " no advancing.
           display num-rows convert.
           display " Return value of the stored proc: " no advancing.
           display ret-code convert.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      *kc      inspect passwd-name tallying passwd-length for characters
      *kc         before initial " "
	   end-if.

       make-connection.
      *    if userid not = spaces
      *       EXEC SQL CONNECT TO :dsn-name as C1
      *                      USER :userid USING :passwd
      *       END-EXEC
      *    else
      *       EXEC SQL CONNECT TO :dsn-name as C1
      *       END-EXEC
      *    end-if.
      *    if sqlcode < 0 perform error-exit.
      *    if sqlcode = 0 display "Connection successful".

           accept sqlserver from environment
                        "otis_acusql_odbc_nm".
           accept userid from environment "otis_acusql_login".
           accept passwd-name from environment "otis_acusql_pswd".

           exec sql connect to :sqlserver as C1
                          user :userid using :passwd-name
           end-exec.
      
           exec sql whenever sqlerror go to error-exit end-exec.
      
           if sqlcode < 0 perform error-exit.
      
      
      *kc  exec sql connect to :sqlserver as connect2
      *kc                 user :userid using :passwd
      *kc  end-exec.
      
      *    exec sql connect to :sqlserver as connect2
      *                   user :userid using :passwd-name
      *    end-exec.
      
           if sqlcode < 0 perform error-exit.

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
