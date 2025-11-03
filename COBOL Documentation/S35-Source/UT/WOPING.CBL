      *A30 COMPLETED CONVERSION
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        WOPING.
      ******************************************************************
      *
      *   PING PROCESS FOR HEADLESS PROCESS.  THIS WAS WRITTEN SO THAT
      *   I'D HAVE SOMETHING SIMPLE TO TEST WITH.
      *  
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  3B.
       OBJECT-COMPUTER.  3B.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/WOPING S35 03/31/23-09:24:39 >".


       01  FORM-PATHNAME     PIC X(22).
       01  EXIT-PATHNAME     PIC X(22).

       77  SCRN-LINES PIC 9(4).
       77  SCRN-SIZE PIC 9(4).
	   
       01  STOP-MSG             PIC X(5) VALUE "DEBUG".
	   
       01  PING-MESSAGE.
           03  PM-DATE                        PIC 9(8).
           03  PM-TIME                        PIC 9(12).
	   
       COPY "LIBUT/WINOTISCORE.CPY".

       LINKAGE SECTION.
       01 COMMAND-ID              PIC 9(5).
       01 MSG-BUFFER-LEN          PIC 9(5).
       01 MSG-BUFFER.          
           03 MSG-BUFFER-VALUE    PIC X(WO-BFR-MAX).
           03 MSG-BUFFER-ARRAY REDEFINES 
                 MSG-BUFFER-VALUE
                 OCCURS WO-BFR-MAX TIMES  PIC X. 
			   

       PROCEDURE DIVISION USING
            COMMAND-ID
            MSG-BUFFER-LEN
            MSG-BUFFER.

      * FORCE TO 80X24.  IF NOT VEERYANT WILL FILL ENTORE SCREEN WITH WINDOW.
      *  IF CHANGING HERE CHANGE CL/PGMENU, CL/SIMENU, CL/WIMENU AS WELL.
            MOVE 80  TO SCRN-SIZE.
            MOVE 24 TO SCRN-LINES.
       

       MAIN-LINE.
      D     STOP STOP-MSG.
       
        SCREENSETUP.
		
		ACCEPT PM-DATE FROM DATE YYYYMMDD.
		ACCEPT PM-TIME FROM MICROSECOND-TIME.
		
        MOVE OTCS-PING-DATA TO WOS-MSG-ID.
  		MOVE PING-MESSAGE TO  WOS-BUFFER-VALUE.
  		MOVE 20 TO WOS-BUFFER-LEN.
		PERFORM WINOTIS-SEND-MESSAGE.
		
		EXIT PROGRAM.



      ***************************
       MISC-ROUTINES SECTION.
      ***************************
	   COPY "LIBUT/WOSRMSG.CPY".	   
	   
