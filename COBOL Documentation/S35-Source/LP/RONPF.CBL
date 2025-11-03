       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RONPF.
       DATE-WRITTEN.    080192.
      ******************************************************************
      *
      *  DESC:  INTERRUPT TRANSACTION PROCESSING DRIVER
      * REV:
      *  071995 BLL IF CALLED FROM CL/WIMENU(LOAN-PROC-FG = " "),
      *             CALL MKTEMP FOR USER-PID & REMOVE WK FILE AT END
      *  140829 MJD MODIFIED THE DISPLAY WINDOW LOGIC FLOW. SEE
      *             DETAILED COMMENTS BELOW WHERE THE WINDOW IS DISPLAYED.
      *             FIXED BUG.  TEMP FILE WAS NOT GETTING REMOVED BECAUSE
      *             TEMP-PID WAS NOT GETTING SET FROM USER-PID.
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  BAH 170713 ACTIVATE 8 DIGIT DATES, PD0006
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/RONPF.CBL S34 06/14/21-15:47:59 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       01  PROG-WK.
           03  FILLER          PIC X(8)  VALUE "LP/LONPF".
           03  PROG-NO         PIC X     VALUE "0".

       01  WS-TRANS-DATE         PIC 9(8).
       01  SAVE-TCLP-MAIN-WINDOW-HANDLE   PIC X(10).
       01  RONPF-WINDOW-HANDLE   PIC X(10).

       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  EXT-EOBUF-MESS         REDEFINES MESS
                              PIC X(60).
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/UNAMEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/LOADCOW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  ACCTNO        PIC 9(8).
       01  LOAN-PROC-FG  PIC X.
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME ACCTNO LOAN-PROC-FG PROG-BUF
                  EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * IF CALLED FROM WITHIN LOAN INPUT TO PAYOFF AN ACCOUNT
           IF LOAN-PROC-FG = "Y"
              MOVE TRANS-DATE TO WS-TRANS-DATE.

      *    MOVE "LP/LONPF" TO WINDOW-FORM-NAM.
           MOVE "LONPF"    TO VDU-FORM-NAME.

      **************************************************************************
      * ADDED NEW WINDOW LOGIC HERE.  RONPF IS USED TO LAUNCH LONPF0 FROM
      * F-10 AND FROM WITHING LOAN PROCESSING.  LONPFO IS CODED TO
      * DISPLAY ON THE TCLP-MAIN-WINDOW-HANDLE.  IN ORDER TO ALLOW THAT
      * TO STILL HAPPEN SMOOTHLY, I AM OPENING A TEMP WINDOW HERE AND
      * THEN PASSING THE HANDLE OF THIS TEMP WINDOW FORWARD AS THE
      * TCLP-MAIN-WINDOW-HANDLE.  THIS WILL ALLOW ALL LONPF0 ETC TO
      * FUNCTION PROPERLY WHILE THEY ARE LAUNCHED. UPON EXIT, I WILL
      * CLOSE THE TEMP WINDOW, AND RESTORE THE TCLP MAIN WINDOW HANDLE
      * SO THAT CURRENT AND FUTURE PROGRAMS MAY ALSO FLOW SMOOTHLY
      **************************************************************************
           DISPLAY FLOATING GRAPHICAL WINDOW
              AT SCREEN LINE 1 * VDU-WIN-L-OFFSET
              AT SCREEN COL  1 * VDU-WIN-C-OFFSET
              SIZE    80 + VDU-WIN-W-MOD
              LINES   24 + VDU-WIN-H-MOD
              BLANK SCREEN
              CELL  SIZE IS LABEL FONT
              COLOR IS VDU-WCOLOR-TCLP
              CONTROL VALUE IS VDU-WCTRL-TCLP
              HANDLE IS RONPF-WINDOW-HANDLE.

           MOVE TCLP-MAIN-WINDOW-HANDLE TO SAVE-TCLP-MAIN-WINDOW-HANDLE.
           MOVE RONPF-WINDOW-HANDLE TO TCLP-MAIN-WINDOW-HANDLE.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           DISPLAY LONPF-SCREEN.

           MOVE LONPF-FORM-FIELDS TO WR-BUF.
           MOVE LONPF-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LOAN-PROC-FG NOT = "Y"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE LPLOAN-SYSDATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO WS-TRANS-DATE.
        
           MOVE "A1"          TO RC-STATUS.
           PERFORM UNAME-CALL.
           MOVE UNAME-BR      TO RC-BRNO.
           MOVE WS-TRANS-DATE TO RC-TRANS-DATE.

           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              MOVE "TODAY HAS NOT BEEN OPENED FOR PAYMENT PROCESSING!"
                                                       TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-PROG.

      * IF COMING FROM F10 INTERRUPT, GET MKTEMP TO USE FOR TEMP FILE
      * IF COMING FROM LOAN PROCESSING (LOAN INPUTS), USE THAT TEMP FILE
      *   THAT IS PASSED IN PROG-BUF

           IF LOAN-PROC-FG NOT = "Y"
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE PROCESS-ID-BUF TO USER-PID.

           MOVE "Y" TO INTERRUPT-FG.
           MOVE ACCTNO TO LOAN-KEY.

           MOVE 0 TO PROG-NO.
       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADCOLP.CPY".
       LOAD-POST.
           IF ERRCD = "E" OR ERRCD = "X"
              GO TO EXIT-PROG.

           MOVE NEW-PROG-SUB TO PROG-NO.
           GO TO LOAD-PRE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE LONPF-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/UNAME.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       FORM-RESET.

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RC2-FILE.
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.
       LOAD-RESTART.
       EXIT-PROG.

      * IF COMING FROM INTERRUPT, REMOVE TEMPORARY WORK FILE
           IF LOAN-PROC-FG NOT = "Y"
              MOVE USER-PID TO TEMP-PID
              MOVE TEMP-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPF-SCREEN.
           DESTROY RONPF-WINDOW-HANDLE.
           MOVE SAVE-TCLP-MAIN-WINDOW-HANDLE TO TCLP-MAIN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
