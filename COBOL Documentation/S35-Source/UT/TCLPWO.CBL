      *A30 COMPLETED CONVERSION
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        TCLP IS INITIAL.
      ******************************************************************
      *
      *   TRANSACTION DRIVER
      *
      *         ...TO SET EVALUATION PERIOD SEE BELOW
      * 
      *   JUST FYI, AS OF 8/2014, WHEN YOU TYPE TCLP IN A30, 
      *   THIS IS WHAT GETS EXECUTED.   BLM
      *        GB/TCLP
      *        GB/SETENV
      *        LP/LPMENU
      *        CL/CHKSEC
      *        LP/WHOAMI
      *  
      *=================================================================
      * REV:
      * BLV 070298 ADDED LOGIC AT STOP-RUN TO DO A LOGUID
      *            CALL WITH A PROGRAM NAME OF "LOGGEDOFF"
      * MG  991008 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * MJD 991203 ADDED SECURITY TO LIMIT NUMBER OF USERS
      * MJD 121214 ADDED DISPLAY OF INITIAL WINDOW W/ HANDLE.
      * BLM 130213 CHANGED LOGUID TO NOT CALL C PROGRAM
      * BAH 130703 CHANGED CELL HEIGHT TO 18 FROM 10
      * KEC 131025 MOVED LOGIC OF SETTING EXT-FILPATH TO GB/SETENV
      * MJD 140604 ADDED EXT- WORKERS FOR WINDOW CONTROL. MOVED
      *            DISPLAY OF WINDOW TO OCCUR AFTER CALLING SETENV
      *            TO DEFINE EXT WORKERS.
      * MJD 150317 REMOVED IS????? COPYBOOKS.
      * BLM 150610 CHANGES FOR NOT RETURNING TO TCLP
      * JKC 210702 ADDED SQL-DISCONNECT BEFORE 'STOP RUN'; WE FOUND THAT
      *            THE USE OF SQL-DISCONNECT WHEN EXITING THE
      *            APPLICATION IS ONLY NEEDED; DOING A SQL-DISCONNECT
      *            WHEN EXITING A PROGRAM WILL CAUSE ALL CURSORS TO BE
      *            CLOSED FROM THE CALLING PROGRAM WHICH EFFECTS IT 
      *            WHEN CONTINUING WITH A READ-NEXT.
      *
      * JKC 2020-0726 ADDED EXT-SQL-CONNECT-STAT FOR CHECKING IF 
      *               CONNECTION TO SQL SERVER HAS ALREADY BEEN MADE;
      *               USED TO ONLY CONNECT ONE-TIME INSTEAD OF MULTIPLE
      *               TIMES IN EVERY PROGRAM; PLACED IN GB/TCLP SO TO
      *               INITIALLY SET IT TO "XX" (BAD) SO THAT IT WILL
      *               CONNECT TO SQL SERVER ON FIRST CALL TO SQL-CONNECT
      *
      * JKC 2021-0910 CHANGED TO USE EXT-ACUSQL-CONNECT-STAT 
      *               INSTEAD OF EXT-SQL-CONNECT-STAT
      *
      * JKC 220623 HAD TO ADD `EXEC SQL INCLUDE` (SQLCA & SQLDA) FOR
      *            THE USE OF LIBGB/DISCONNECT_SQL.CPY TO COMPILE
      *            WITHOUT WARNINGS WHICH DOES NOT SHOW ANY WARNING
      *            MESSAGE BUT SHOWS 1,000+ DUPLICATION OF THIS LINE:
      *               EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
      *
      * BAH 20220802 COMMENTED OUT LOGUID
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  3B.
       OBJECT-COMPUTER.  3B.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/TCLPWO S35 03/31/23-09:24:37 >".

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.

       01  EXT-EOBUF-MESS        PIC X(60).

       01  FORM-PATHNAME     PIC X(22).
       01  EXIT-PATHNAME     PIC X(22).

       77  SCRN-LINES PIC 9(4).
       77  SCRN-SIZE PIC 9(4).
	   
	   
	   01  START-MESSAGE.
	       03  SM-STATE                       PIC XX.
	       03  SM-BRANCH                      PIC 9(4).
		   03  SM-CONAME                      PIC X(40).


	   
       COPY "LIBUT/WINOTISCORE.CPY".
	   
       01  HOLD-FIL-PATH.
           03  FILLER           PIC X(12).
           03  HOLD-FIL-STATE   PIC X(2).
           03  HOLD-FIL-BR      PIC 9(4).
	   

       01 SEND-ID                                   PIC 9(5).
       01 SEND-DATA-LEN                             PIC 9(4).
       01 SEND-DATA-IDX                             PIC 9(4).
       01 SEND-DATA.
            03 SEND-DATA-VALUE                      PIC X(1400).
            03 SEND-DATA-ARR REDEFINES SEND-DATA-VALUE 
                    OCCURS 1400 TIMES               PIC X.
       01 SEND-ERROR                                PIC 9.
       
       01 RECEIVE-ID                                PIC 9(4).
       01 RECEIVE-DATA-LEN                          PIC 9(4).
       01 RECEIVE-DATA-IDX                          PIC 9(4).
       01 RECEIVE-DATA.
            03 RECEIVE-DATA-VALUE                   PIC X(1400).
            03 RECEIVE-DATA-ARR REDEFINES RECEIVE-DATA-VALUE
                    OCCURS 1400 TIMES               PIC X.
       01 RECEIVE-ERROR                             PIC 9.

       77 FIELD-DELIMITER                     PIC X VALUE "^".



       01 MB-COMMAND                           PIC X.
       01 MB-DATA-FIELD-LEN                    PIC 9(3).
       01 MB-DATA-FIELD.
           03 MB-DATA-FIELD-VALUE              PIC X(128).
           03 MB-DATA-FIELD-ARRAY REDEFINES MB-DATA-FIELD-VALUE
               OCCURS 128 TIMES                PIC X.
       01 MB-MSG-BUFFER-LEN                    PIC 9(5).
       01 MB-MSG-BUFFER.           
           03 MB-MSG-BUFFER-VALUE              PIC X(16384).
           03 MB-MSG-BUFFER-ARRAY REDEFINES MB-MSG-BUFFER-VALUE
               OCCURS 16384 TIMES              PIC X.        
       01 MB-ERR-CODE                          PIC 9.


       01 SET-TO-START                         PIC 9.
       01 MB-MSG-BUFFER-POS                       PIC 9(5).
       01 RETURN-DATA-COUNT                    PIC 9(3).
       01 RETURN-DATA.
           03 RETURN-DATA-ARRAY 
               OCCURS 256 TIMES                PIC X(128).
       01 END-OF-DATA                          PIC 9.
       01 RESULT-CODE                          PIC 9.
	   

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/FILEWK.CPY".


       PROCEDURE DIVISION.

      * FORCE TO 80X24.  IF NOT VEERYANT WILL FILL ENTORE SCREEN WITH WINDOW.
      *  IF CHANGING HERE CHANGE CL/PGMENU, CL/SIMENU, CL/WIMENU AS WELL.
            MOVE 80  TO SCRN-SIZE.
            MOVE 24 TO SCRN-LINES.
       

      * NEED TO SET EXT-ACUSQL-CONNECT-STAT TO BAD SO THAT IT MAKES
      * INITIAL CONNECTION TO SQL SERVER ON FIRST CALL TO SQL-CONNECT.
           MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT.

       MAIN-LINE.
      D     STOP MESS.
        MOVE  "TCLPWO" TO WO-ERR-PROG-ID.
        SCREENSETUP.

      * MOVED DISPLAY OF WINDOW FURTHUR DOWN, IT NEEDS TO OCCUR
      *  AFTER CALLING SWETENV |MJD

      * OLD DISPLAY , PRIOR TO JOHNS VISIT 5/14 |MJD
      *     DISPLAY STANDARD GRAPHICAL WINDOW
      *            AT SCREEN LINE 1
      *            AT SCREEN COL 1
      *             CELL HEIGHT 18
      *             CELL WIDTH 10
      *             SIZE 84
      *             LINES 24
      *             TITLE "PARADATA FINANCIAL SYSTEMS"
      *             BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
      *             HANDLE IS TCLP-MAIN-WINDOW-HANDLE.

           MOVE "/   /   /  /" TO FORM-PATH.

      * I DO NOT KNOW IF WE NEED THIS ??? BAH 1/29/2013
      *
      * TO FIX PIGGY BACK FLOW CONTROL PROBLEM
      *    MOVE STTY-PIGGY-ON TO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.

      * PRE-LOAD OPTIONS (DEBUG MODE, ETC)
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-GOOD
              MOVE GETENV-BUF TO LOADCO-OPT.

      *----------------------------------------------------------
      *    SET CONAME & GPFILE & BRFILE ENVIRONMENT VARIABLES:
      *----------------------------------------------------------
           MOVE "FIL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO HOLD-FIL-PATH.
           MOVE HOLD-FIL-STATE TO SM-STATE.
           MOVE HOLD-FIL-BR TO SM-BRANCH.
           MOVE "LOAN MANAGER API" TO SM-CONAME.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE "ENV ERROR: REL" TO WO-ERR-DESC
              PERFORM WINOTIS-SEND-ERROR
              GO TO STOP-RUN.

      *    SETUP FORM-PATHNAME FOR USE IN SENDMESS
      *    IT DID NOT EXIST PREVIOUSLY IN TCLP.C
*NEW*      MOVE GETENV-BUF TO FORM-REL.
*NEW*      MOVE "CO" TO FORM-OBJ.
*NEW*      MOVE "GB/TCLP  " TO FORM-NAM.
*NEW*      MOVE FORM-PATH TO FORM-PATHNAME EXIT-PATHNAME.

           MOVE GETENV-BUF TO FORM-REL.
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.


      * MOVED DISPLAY OF WINDOW HERE, NEED TO EXECUTE SETENV TO
      * CONFIGURE EXT WORKERS. |MJD
            DISPLAY STANDARD GRAPHICAL WINDOW
                LINES SCRN-LINES * VDU-WIN-L-OFFSET
                SIZE SCRN-SIZE * VDU-WIN-C-OFFSET
      *         LABEL-OFFSET 20
                LABEL-OFFSET VDU-LABEL-OFFSET
                RESIZABLE
                AUTO-MINIMIZE
                MODELESS 
      *         LAYOUT-MANAGER LM-RESIZE
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
      *          ICON W-ICON20         REPLACE WITH OUR LOGO ICON
      *          FOREGROUND-COLOR RGB X#000000
                TITLE "  PARADATA FINANCIAL SYSTEMS  "
      *          EVENT  WIN-EVT
                HANDLE IS TCLP-MAIN-WINDOW-HANDLE.


		   PERFORM WINOTIS-SEND-START.
      		   IF WINOTIS-ERROR <> 0
      *       STRING LOGUID-LOGNAME, " ",
      *              LOGUID-REL, " ",
      *              "SYNCERR   ",
      *              LOGUID-MO, "/", LOGUID-DA, "/", LOGUID-YR, " ",
      *              LOGUID-HH, ":", LOGUID-MM,  " ",
      *              LOGUID-POSTBRANCH, " ",
      *              LOGUID-INTERBR, " ",
      *              DELIMITED BY SIZE
      *              INTO LOGUID-BUF
      *       MOVE "CL/LOGUID" TO FORM-NAM
      *       CALL FORM-PROGX USING FORM-PATHNAME LOGUID-BUF EXIT-PATHNAME
      *       CANCEL FORM-PROGX
			 GO TO STOP-RUN.
		   
           GET-CLIENT-COMMAND.
             PERFORM WINOTIS-RECV-MESSAGE.
			 IF WINOTIS-ERROR <> 0
			   GO TO STOP-RUN.
		     IF (WOR-MSG-ID = OTCR-TERMINATE)
               GO TO STOP-RUN.
		     IF (WOR-MSG-ID = OTCR-PING)
               MOVE "WOPING" TO FORM-PROGX.
		     IF (WOR-MSG-ID = OTCR-BWSCAN-START)
               MOVE "BWSCANWO" TO FORM-PROGX.
		     IF (WOR-MSG-ID = OTCR-STMAIN)
               MOVE "STMNWO" TO FORM-PROGX.
             MOVE 0 TO RESULT-CODE.  

			 
             CALL FORM-PROGX USING 
			     WOR-MSG-ID
			     WOR-BUFFER-LEN
				 WOR-BUFFER-VALUE.
             CANCEL FORM-PROGX.
			 IF RESULT-CODE <> 0 
			     GO TO STOP-RUN.
			 GO TO GET-CLIENT-COMMAND.	 


       WINOTIS-SEND-START.
	        MOVE OTCS-START TO WOS-MSG-ID.
	  		MOVE START-MESSAGE TO  WOS-BUFFER-VALUE.
	  		MOVE 46 TO WOS-BUFFER-LEN.
			PERFORM WINOTIS-SEND-MESSAGE.
	   
       WINOTIS-SEND-SHUTDOWN.
	        MOVE OTCS-TERMINATE TO WOS-MSG-ID.
	  		MOVE SPACES TO  WOS-BUFFER-VALUE.
	  		MOVE 0 TO WOS-BUFFER-LEN.
			PERFORM WINOTIS-SEND-MESSAGE.



      ***************************
       MISC-ROUTINES SECTION.
      ***************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".
       COPY "LIBGB/DISCONNECT_SQL.CPY".
	   COPY "LIBUT/WOSRMSG.CPY".	   
	   
       STOP-RUN.
           PERFORM SQL-DISCONNECT.  *> NEED TO BE LAST BEFORE `STOP RUN`
           STOP RUN.
