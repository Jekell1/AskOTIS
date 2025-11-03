       IDENTIFICATION DIVISION.
       PROGRAM-ID.                         LISTCUST1.
       AUTHOR.                             RLZ.
       REMARKS.
	   SHOW THE EXECUTION OF A STORED PROCEDURE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/SQLST1 S35 03/31/23-09:24:30 >".
       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  C-RECORD.
           05  C-NUMBER                            PIC S9(3) COMP-5.
           05  C-FIRST-NAME                        PIC X(80).
           05  C-LAST-NAME                         PIC X(80).
           05  C-BIRTHDAY                          PIC X(10).
           05  C-INFO                              PIC X(10).
           05  C-ST-CODE                           PIC X(2).
           05  C-ST-DESC                           PIC X(16).

       01  NUM-ROWS                            PIC S9(5) COMP-5.
       01  RET-CODE                            PIC S9(4) COMP-5.
      *01  DSN-NAME                            PIC X(12).
       01  DSN-NAME                            PIC X(30).
      *01  USERID                              PIC X(8).
       01  USERID                              PIC X(30).
       01  PASSWD.
           49 PASSWD-LENGTH                    PIC S9(4) COMP-5 VALUE 0.
           49 PASSWD-NAME                      PIC X(18).
       01  DEMO-QUERY                          PIC X(2000).
       EXEC SQL END DECLARE SECTION END-EXEC.

       COPY "LIBGB/CONAME_EXT.CPY".
       01  MESS PIC X(61).

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN SECTION.
      D    STOP MESS.
           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.
      *     DISPLAY "THIS PROGRAM DEMONSTRATES CONNECTING TO A SQL SERVER
      *-    " INSTANCE AND EXECUTING".
      *     DISPLAY "A STORED PROCEDURE (SP_LISTCUST).  HOPEFULLY YOU HAV
      *-    "E EXECUTED 'CREATE' TO".
      *     DISPLAY "CREATE AND POPULATE THE TABLE, AND INSTALLED THE STO
      *-    "RED PROCEDURE IN".
      *     DISPLAY "'LISTCUST.SQL' INTO A LOCATION THAT IT CAN BE FOUND.
      *-    "".
      D    STOP MESS.
           PERFORM INITIALIZATION-ROUTINE.
           PERFORM MAKE-CONNECTION.
           PERFORM EXECUTE-IT.
           PERFORM DISCONNECT-CONNECTION.
      *    DISPLAY "PROGRAM COMPLETE. PRESS ENTER TO EXIT."
      *    ACCEPT OMITTED.
           STOP RUN.

       EXECUTE-IT.
      *    DISPLAY "ENTER THE NAME TO SEARCH FOR: ", NO.
      *    ACCEPT C-LAST-NAME.
           MOVE "MI" TO C-ST-CODE.

           EXEC SQL WHENEVER SQLERROR GO TO ERROR-EXIT END-EXEC.
      *    EXEC SQL :RET-CODE = EXEC SP_LISTCUSTOMER ( :C-LAST-NAME,
      *             :NUM-ROWS OUT )
      *    END-EXEC.

      *JKC EXEC SQL :RET-CODE = EXEC SQLST_READ_NEXT ( :C-ST-CODE,
      *JKC          :NUM-ROWS OUT )
      *JKC END-EXEC.

      D    STOP MESS.
           STRING "SELECT C_FIRST_NAME, C_LAST_NAME FROM CUSTOMER"
                  DELIMITED BY SIZE,
                  " WHERE C_NUMBER >= ?" DELIMITED BY SIZE
               INTO
                  DEMO-QUERY.

           EXEC SQL
              PREPARE QID FROM :DEMO-QUERY
           END-EXEC.

           IF SQLCODE < 0 PERFORM ERROR-EXIT.
           DISPLAY " NUMBER OF ROWS WITH THAT LAST NAME: " NO ADVANCING.
           DISPLAY NUM-ROWS CONVERT.
           DISPLAY " RETURN VALUE OF THE STORED PROC: " NO ADVANCING.
           DISPLAY RET-CODE CONVERT.
           DISPLAY " ST-DESC: " NO ADVANCING.
           DISPLAY C-ST-CODE.

      *COPY "COMMON.CPY".
       INITIALIZATION-ROUTINE.
      *     DISPLAY "ENTER THE SERVER NAME: ", NO ADVANCING.
      *     ACCEPT DSN-NAME.
      *     DISPLAY "ENTER YOUR USER ID (OR PRESS RETURN TO USE WINDOWS A
      *-           "UTHENTICATION): ", NO.
      *     ACCEPT USERID.
      *   IF USERID NOT = SPACES
      *        DISPLAY "ENTER YOUR PASSWORD : ", NO
      *        ACCEPT PASSWD-NAME

           MOVE EXT-ACUSQL-DSN-NAME  TO DSN-NAME.
           MOVE EXT-ACUSQL-LOGIN     TO USERID.
           MOVE EXT-ACUSQL-PSWD      TO PASSWD-NAME.

      * PASSWORDS IN A CONNECT STATEMENT MUST BE ENTERED IN A VARCHAR FORMAT
      * WITH THE LENGTH OF THE INPUT STRING.
               INSPECT PASSWD-NAME TALLYING PASSWD-LENGTH FOR CHARACTERS
                  BEFORE INITIAL " ".
      *   END-IF.

       MAKE-CONNECTION.
           IF USERID NOT = SPACES
	       EXEC SQL CONNECT TO :DSN-NAME AS C1
		       USER :USERID USING :PASSWD
	       END-EXEC
	   ELSE
	       EXEC SQL CONNECT TO :DSN-NAME AS C1
	       END-EXEC
	   END-IF.
           IF SQLCODE < 0 PERFORM ERROR-EXIT.
           IF SQLCODE = 0 DISPLAY "CONNECTION SUCCESSFUL".

       DISCONNECT-CONNECTION.
           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           IF SQLCODE < 0 PERFORM ERROR-EXIT.

       ERROR-EXIT.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           DISPLAY "SQL ERROR: SQLCODE  " SQLCODE OF SQLCA.
           DISPLAY "           SQLSTATE " SQLSTATE OF SQLCA.
           DISPLAY SQLERRMC OF SQLCA.
           ACCEPT OMITTED.
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
