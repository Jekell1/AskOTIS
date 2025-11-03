       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RONPI.
       DATE-WRITTEN.    05/13/2008.
      ******************************************************************
      *
      *  DESC:  INTERRUPT CHECK WRITING DRIVER FROM F10
      *
      * REV:
      * 080513 BAH NEW FOR RIVERSIDE WISC PR# 3192
      * 150501  CS CHANGED CHECK ON OPEN DAY TO READ THE RC FILE AND NOT
      *             CHECK FOR THE OP FLAG
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
       77  SCCS-IDENTIFIER PIC X(42) VALUE
           "@(#)./LP/RONPI.CBL S35 02/27/24-11:42:06 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       01  PROG-WK.
           03  FILLER          PIC X(8)  VALUE "LP/CHCKI".
           03  PROG-NO         PIC X     VALUE "0".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  EXT-EOBUF-MESS         REDEFINES MESS
                              PIC X(60).
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/LOADCOW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CHCKI_DEF.CPY".
       COPY "LIBLP/CHCKI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).

       SCREEN SECTION.
       COPY "LIBLP/CHCKI_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 24  TO WINDOW-LINES.
           MOVE 80  TO WINDOW-COLUMNS.
           MOVE 1   TO WINDOW-BEGIN-Y.
           MOVE 1   TO WINDOW-BEGIN-X.
           MOVE " " TO WINDOW-FIT-TO.

           MOVE "LP/CHCKI" TO WINDOW-FORM-NAM.

           MOVE "CHCKI" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY CHCKI-SCREEN.
           MOVE CHCKI-FORM-FIELDS TO WR-BUF.
           MOVE CHCKI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "A1" TO RC-STATUS.
           MOVE EXT-CONAME-USER-LOC TO RC-BRNO.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD     TO RC-TRANS-DATE.
           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              MOVE "TODAY HAS NOT BEEN OPENED FOR PAYMENT PROCESSING!"
                                                       TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-PROG.

      * USE THE 'RC' INSTEAD OF CHECKING FOR THE OP FLAG

      * IF COMING FROM INTERRUPT, GET MKTEMP TO USE FOR TEMP FILE
      * THERE MAY BE ANOTHER TEMP FILE IN LOAN PROCESSING, BUT
      * PROGBUF WILL NOT BE PASSED BACK SO WE DON'T CARE...

      *    IF LOAN-PROC-FG NOT = "Y"
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE PROCESS-ID-BUF TO USER-PID.


      * INTERRUPT-FG IS IN LPPFBUF WHICH IS PASSED WHEN CALLING CHCKI0
           MOVE "Y" TO INTERRUPT-FG.
           MOVE 0 TO LOAN-KEY.
           MOVE EXT-CONAME-USER-LOC TO BRANCH.
           MOVE "I0" TO NEW-PROG.

           MOVE 0 TO PROG-NO.
       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADCOLP.CPY".
       LOAD-POST.
           IF ERRCD = "E" OR ERRCD = "X"
              GO TO EXIT-PROG.

           MOVE NEW-PROG-SUB TO PROG-NO.
           GO TO LOAD-PRE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************
       FORM-RESET SECTION.
      *********************************
       FORM-RESET-EXIT.
           EXIT.

      *********************************
       SETUP-LINK-INFO SECTION.
      *********************************
           MOVE CHCKI-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHCKI-HELP--DIR TO HELP-LINK-DIR.

      *********************************
       VDU-CONVERT-FIELD SECTION.
      *********************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CHCKI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".

       COPY "LIBLP/LPEDPATHIO.CPY".

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RC2-FILE.
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".

       END-ROUTINE.
       LOAD-RESTART.
       EXIT-PROG.

      * IF COMING FROM INTERRUPT, REMOVE TEMPORARY WORK FILE
      *    IF LOAN-PROC-FG NOT = "Y"
      *       CALL FLDVAL USING STAT PID-LIST TEMP-PID
      *       IF STAT = "00" AND TEMP-PID NOT = " "
                 MOVE TEMP-PATH TO ACCESS-BUF
                 PERFORM REMOVE-WORKFILE.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHCKI-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.

           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.
