       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EORUN.
       DATE-WRITTEN.     03/15/91.
      **********************************************************
      *   DIR :  SP
      *   DESC:  BATCH MENU EXECUTION DRIVER
      *            RUNS IN BACKGROUND WITH NO SCREEN IO
      *          EX. EOD, EOM
      *
      * CURRENTLY CALLED FROM LP/LONPR0, NP/NPEOD, SP/AUTOCL,
      *
      * REV:
      *  JTG 022294 ADDED LOGIC FOR EOM CALLED FROM LONPR0.C
      *  BLK 072194 REWRITE - REMOVE UPDATE OF EO, REMOVE CLEAR, AUDIT
      *  BLL 071095 ADD ERFILE TO LOG PROCESS
      *  BLV 061198 REMOVED STATUS CHECK ON LOGUID CALL
      *  BLV 091798 CHANGE TO USE DATE2 FOR EO-SPOOL-DATE, WAS USING
      *             SYSDATE, NEED CHANGE FOR NPEOD
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *             DECLARATIVES
      *  BAH 120314 MOVE EO-COMMENT TO EXT-EOBUF-MESS SO CERTAIN REPORTS CAN
      *             HAVE IT PRINT IN H-TITLE, MULLEN PR# 3991
      *   CS 140603 REPLACED MKDIR-CMD WITH C$MAKEDIR
      *   CS 161102 COMMENTED OUT THESE LINES IN LOAD-IT, EOD BATCH WAS 
      *             LOCKING UP
      *  BAH 170313 CHANGED EORUN-DATE1 AND EORUN-DATE2 TO 9(8), PD0006
      *  20170907 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20220816 COMMENTED OUT LOGUID
      * JKC 2024-0202 ADDED OTIS TRACE LOGGING LOGIC (LIBCL/TRACE)
      *
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSER.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDER.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)SP/EORUN.CBL S35 02/13/24-13:37:21 >".
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       COPY "LIBLP/LPWSER.CPY".

       COPY "LIBGB/EOBUF_EXT.CPY".

       01  CURRENT-STEP   PIC 9999.
       01  LOAD-STAT      PIC X(6).
       01  FIRST-TIME     PIC X.
       01  ER-FEED        PIC 9        VALUE 1.

       01  MESS-LIST      PIC X(5)  VALUE "MESS.".
       01  MSEL           PIC X(40) VALUE " ".
       01  ALLMESS-LIST   PIC X(20) VALUE "MSEL SEL MSEL2 MESS.".

       01  AUDIT-ERROR       PIC 9  VALUE 0.
       01  SECURITY-ERROR    PIC 9  VALUE 0.
       01  STEP-COUNT        PIC 9999.
       01  SKIP-SWITCH       PIC 9  VALUE 0.
       01  SKIP-STEP         PIC 9999 VALUE 0.

       01  WS-PROG-PATHNAME.
           03  WS-PGM-DIR    PIC XX.
           03  WS-PGM-SL     PIC X    VALUE "/".
           03  WS-PGM-NAME   PIC X(6).
       01  WS-COMPLETE-STAT  PIC X(20).

       01  DATE-TIME-WORKERS.
           03  ACC-TIME     PIC 9(8).
           03  THIS REDEFINES ACC-TIME.
               05  ACC-HR    PIC 99.
               05  ACC-MN    PIC 99.
               05  ACC-SC    PIC 99.
               05  ACC-MS    PIC 99.
           03  CUR-DATE     PIC 9(8).
           03  CUR-TIME     PIC 9(6).
           03  THIS REDEFINES CUR-TIME.
               05  CUR-HR    PIC 99.
               05  CUR-MN    PIC 99.
               05  CUR-SC    PIC 99.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       01  HELP-LINK-BUF.
           03 HELP-LINK-PATH-INFO.
              05  HELP-LINK-DIR     PIC XX.
              05  HELP-LINK-FILE    PIC X(20).
           03  HELP-LINK-FIELD   PIC X(15).
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EORUN-BUF.
           03  EORUN-PTYPE          PIC XXX.
           03  EORUN-PCODE          PIC XXX.
           03  EORUN-HOUR-BUF       PIC 99.
           03  EORUN-MIN-BUF        PIC 99.
           03  EORUN-DATE1          PIC 9(8).
           03  EORUN-DATE2          PIC 9(8).
           03  EORUN-FIRST-STEP     PIC 9(4).
           03  EORUN-LAST-STEP      PIC 9(4).
           03  EORUN-ERROR.
               05  EORUN-ERR-PROG       PIC X(10).
               05  EORUN-ERR-MSG        PIC X(10).
       01  EXIT-PATHNAME PIC X(22).

       PROCEDURE DIVISION USING FORM-PATHNAME EORUN-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               ER-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-EO1-FILE.
           CLOSE
               ER-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************
      *       MAIN PROGRAM MODULE
      ******************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE SPACES TO EORUN-ERROR.

           PERFORM INITIAL-SETUP.

      **********************************************
      *    CREATE SPOOL DIRECTORY
      **********************************************
       CREATE-SPOOL-DIR.
           MOVE "CREATING WORK FILES..........." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " " TO EO-SPOOL-FILE.
           PERFORM LOAD-EO-SPOOL.

           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "CAN'T CREATE SPOOL DIRECTORY" TO MESS
                                                        EORUN-ERROR
                 PERFORM SEND-MESS
                 MOVE EO-SPOOL TO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

      **************************
       BEGUPD.
      **************************
      *E-FIRST.
      *    MOVE "FIRST." TO MESS.
      *    CALL FLDVAL USING STAT MESS FIRST-STEP.
      *    IF STAT NOT = "00"
      *       MOVE "STARTING PROCEDURE FORM ERROR" TO MESS
      *                                               EORUN-ERROR
      *                                               ER-MESSAGE
      *       PERFORM UPDATE-LOG
      *       PERFORM SEND-MESS
      *       GO TO IO-ERROR.
      *E-LAST.
      *    MOVE "LAST." TO MESS.
      *    CALL FLDVAL USING STAT MESS LAST-STEP.
      *    IF STAT NOT = "00"
      *       MOVE "ENDING PROCEDURE FORM ERROR" TO MESS
      *                                             EORUN-ERROR
      *                                             ER-MESSAGE
      *       PERFORM UPDATE-LOG
      *       PERFORM SEND-MESS
      *       GO TO IO-ERROR.

       START-AUDIT.
           MOVE 0 TO AUDIT-ERROR
                     STEP-COUNT
                     SECURITY-ERROR.

           MOVE "60" TO EXT-ROUTE-BUF.
           MOVE SPACES TO LASTNEXT-BUF.
           MOVE EORUN-FIRST-STEP TO CURRENT-STEP.
           MOVE "Y" TO FIRST-TIME.

           MOVE SPACES TO EXT-EOBUF-ERRCD.
       NEXT-AUDIT.

           PERFORM GET-NEXT-REPORT-ENTRY.
           IF IO-FG = 9
              GO TO END-AUDIT.

           ADD 1 TO STEP-COUNT.

           PERFORM PROGRAM-LOAD.

           PERFORM CLEAN-MESS.

           PERFORM UPDATE-REPORT-ENTRY.
           IF WS-COMPLETE-STAT = "RUN OK"
              GO TO NEXT-AUDIT.

           MOVE "S  A500MSEL2." TO VDU.
           MOVE " " TO MESS.

           MOVE WS-PROG-PATHNAME TO EORUN-ERR-PROG.
           MOVE WS-COMPLETE-STAT TO EORUN-ERR-MSG.
           IF WS-COMPLETE-STAT = "NOTFND"
              MOVE " " TO VDUBUF
              STRING "PROGRAM: " WS-PROG-PATHNAME " IS MISSING"
                     DELIMITED BY SIZE INTO VDUBUF
              MOVE VDUBUF TO ER-MESSAGE
           ELSE
           IF WS-COMPLETE-STAT = "FORMNF"
              MOVE "PROGRAM FORM MISSING" TO VDUBUF ER-MESSAGE
           ELSE
           IF WS-COMPLETE-STAT = "SECVIZ"
              MOVE "CANNOT BE RUN--MEMORY OVERFLOW" TO VDUBUF
                                                       ER-MESSAGE
           ELSE
           IF WS-COMPLETE-STAT = "SECVIO"
              MOVE "CANNOT BE RUN--OPTION NOT PURCHASED" TO VDUBUF
                                                            ER-MESSAGE
              MOVE 1 TO SECURITY-ERROR
           ELSE
           IF WS-COMPLETE-STAT = "SECVIS"
              MOVE "CANNOT BE RUN--SECURITY VIOLATION" TO VDUBUF
                                                          ER-MESSAGE
              MOVE 1 TO SECURITY-ERROR
           ELSE
           IF WS-COMPLETE-STAT = "MEMOVF"
              MOVE "MEMORY OVERFLOW OCCURRED" TO VDUBUF
                                                 ER-MESSAGE
           ELSE
           IF WS-COMPLETE-STAT = "D.VERS"
              MOVE "NEW VERSION--RANGES MUST BE RE-ENTERED"
                                         TO VDUBUF ER-MESSAGE
           ELSE
              MOVE " " TO VDUBUF
              STRING "EXECUTION ERROR OCCURRED (" WS-COMPLETE-STAT
                        ")" DELIMITED BY SIZE INTO VDUBUF.
              MOVE VDUBUF TO ER-MESSAGE
           PERFORM SCREENIO.

           PERFORM UPDATE-LOG.

           MOVE " " TO MESS.
           STRING "STEP:" EO-STEP " " WS-PROG-PATHNAME
                 "..        PRESS RTN:" DELIMITED BY SIZE INTO MSEL.
           PERFORM SEND-LEGEND.

           MOVE 1 TO AUDIT-ERROR.
           IF SECURITY-ERROR = 0
              GO TO NEXT-AUDIT.

       END-AUDIT.
           IF SECURITY-ERROR = 1
              MOVE "YOU ARE NOT AUTHORIZED TO RUN THIS PROCEDURE."
                    TO MESS ER-MESSAGE
              PERFORM UPDATE-LOG
              PERFORM SEND-MESS
              GO TO END-PROGRAM.
           IF AUDIT-ERROR = 1
              MOVE "CAN'T RUN UNTIL ERRORS ARE CORRECTED." TO MESS
                                                        ER-MESSAGE
              PERFORM UPDATE-LOG
              PERFORM SEND-MESS
              GO TO END-PROGRAM.
           IF STEP-COUNT = ZERO
              MOVE "INVALID RANGE--REENTER" TO MESS ER-MESSAGE
              PERFORM UPDATE-LOG
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           PERFORM CLEAN-MESS.

      *****************************************
       START-PROCESS.
      *****************************************
           MOVE "BATCH PROCESSING IN PROGRESS..." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "70" TO EXT-ROUTE-BUF.
           MOVE SPACES TO LASTNEXT-BUF.
           MOVE EORUN-FIRST-STEP TO CURRENT-STEP.
           MOVE "Y" TO FIRST-TIME.
           MOVE SPACES TO EXT-EOBUF-ERRCD.
           MOVE 0 TO SKIP-SWITCH SKIP-STEP.

       NEXT-PROCESS.
           PERFORM GET-NEXT-REPORT-ENTRY.
           IF IO-FG = 9
              GO TO END-PROCESS.

           IF SKIP-SWITCH = 2
              IF EO-STEP NOT < SKIP-STEP
                 MOVE ZERO TO SKIP-SWITCH SKIP-STEP.

           IF SKIP-SWITCH = 0
      *       CALL XFER USING STAT ROUTE-LIST ROUTE-BUF
              PERFORM PROGRAM-LOAD
              PERFORM CLEAN-MESS.

           PERFORM UPDATE-REPORT-ENTRY.

           MOVE WS-COMPLETE-STAT TO ER-MESSAGE.
           PERFORM UPDATE-LOG.

           GO TO NEXT-PROCESS.

       END-PROCESS.

           GO TO ENDIT.

      *************************************
       GET-NEXT-REPORT-ENTRY SECTION.
      *************************************

           PERFORM OPEN-EO1-FILE.

           MOVE EORUN-PCODE     TO EO-PCODE.
           MOVE CURRENT-STEP    TO EO-STEP.

           IF FIRST-TIME = "Y"
              PERFORM START-EO1-FILE
              MOVE "N" TO FIRST-TIME
           ELSE
              PERFORM START-EO1-FILE-GREATER.
           IF IO-FG = 9
              GO TO GET-NEXT-EXIT.

           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO GET-NEXT-EXIT.

           IF EO-PCODE NOT = EORUN-PCODE OR EO-STEP > EORUN-LAST-STEP
              MOVE 9 TO IO-FG
              GO TO GET-NEXT-EXIT.

           MOVE EO-STEP TO CURRENT-STEP.

       GET-NEXT-EXIT.
           MOVE EO-PCODE      TO EXT-EOBUF-PCODE.
           MOVE SPACES        TO EXT-EOBUF-MESS.
           MOVE EO-PARAMETERS TO EXT-EOBUF-DATA.
           MOVE EO-COPIES     TO EXT-EOBUF-COPIES.
           MOVE EORUN-DATE1   TO EXT-EOBUF-DATE1.
           MOVE EORUN-DATE2   TO EXT-EOBUF-DATE2.

           MOVE "/"        TO EO-SPOOL-FILE.
           MOVE EO-PROGRAM TO EO-SPOOL-NAME.
           MOVE EO-STEP    TO EO-SPOOL-STEP.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO EXT-EOBUF-PRPATH.
           MOVE "~" TO EXT-EOBUF-ERRCD.
      * PRINT THE COMMENT IN THE H-TITLE FOR CERTAIN REPORTS (EMTRRG, CPCORP)
           IF EXT-ROUTE-BUF = "70"
              MOVE EO-COMMENT TO EXT-EOBUF-MESS.

           MOVE EO-DIR     TO WS-PGM-DIR.
           MOVE "/"        TO WS-PGM-SL.
           MOVE EO-PROGRAM TO WS-PGM-NAME.
           MOVE WS-PROG-PATHNAME TO FORM-NAM.
           PERFORM CLOSE-EO1-FILE.

      *************************************
       UPDATE-REPORT-ENTRY SECTION.
      *************************************

           IF SKIP-SWITCH = 2
              MOVE "SKIP'D" TO WS-COMPLETE-STAT
           ELSE
              MOVE 1 TO SKIP-SWITCH
              IF LOAD-STAT NOT = " "
                 MOVE LOAD-STAT TO WS-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "!"
                 MOVE "FATAL!" TO WS-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "V"
                 MOVE "D.VERS" TO WS-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "|"
                 MOVE "SECVIS" TO WS-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "S"
                 MOVE " " TO WS-COMPLETE-STAT
                 STRING "ERR :" EXT-EOBUF-ERRCD
                    DELIMITED BY SIZE INTO WS-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD NOT = " "
                 MOVE " " TO WS-COMPLETE-STAT
                 STRING "FAIL:" EXT-EOBUF-ERRCD
                    DELIMITED BY SIZE INTO WS-COMPLETE-STAT
              ELSE
                 MOVE 0 TO SKIP-SWITCH
                 MOVE "RUN OK" TO WS-COMPLETE-STAT.
           IF SKIP-SWITCH = 1
              IF EO-ERROR-STEP-X IS NUMERIC
                 IF EO-ERROR-STEP > EO-STEP
                    MOVE EO-ERROR-STEP TO SKIP-STEP
                    MOVE 2 TO SKIP-SWITCH.
           IF SKIP-SWITCH = 1
              MOVE 0 TO SKIP-SWITCH.
       UPDATE-ENTRY-EXIT.
           EXIT.

      *************************************
       UPDATE-LOG SECTION.
      *************************************

           MOVE EO-SPOOL-PCODE TO ER-ALL-SPOOL-PCODE.
           MOVE EO-SPOOL-DATE  TO ER-ALL-SPOOL-DATE.
           PERFORM LOAD-ER-FILE.
           MOVE ER-PATH        TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "ER" TO E-FILE
              OPEN OUTPUT ER-FILE
              CLOSE ER-FILE.
           MOVE E-FILE TO E-FILEX.
           PERFORM OPEN-ER-FILE.
           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ER-DATE.
           MOVE CUR-TIME TO ER-TIME.
           INSPECT ER-TIME REPLACING ALL "/" BY ":".
           MOVE EXT-CONAME-USER-ID TO ER-USER.
           MOVE EO-DIR TO ER-DIR.
           MOVE "/" TO ER-SLASH.
           MOVE EO-PROGRAM TO ER-PROGRAM.
           PERFORM WRITE-ER-REC.
           PERFORM CLOSE-ER-FILE.

      ********************************
       PROGRAM-LOAD SECTION.
      ********************************
       PGM-LOAD.

           MOVE SPACES TO NEXT-PROGRAM.
           MOVE SPACES TO LOAD-STAT.
           IF LOADCO-OP1 = " " OR LOADCO-OP1 = "1"
              GO TO LOAD-IT.

           MOVE FORM-NAM TO LOADCO-ARG.
           MOVE LOADCO-OPT TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

       LOAD-IT.

           MOVE "CO" TO FORM-OBJ.

      *    PERFORM LOGUID-CALL.

           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME ON OVERFLOW
              MOVE "MEMOVF" TO LOAD-STAT
              GO TO LOAD-POST.
           CANCEL FORM-PROGX.
           IF FORM-OBJ = " "
              MOVE "FORMNF" TO LOAD-STAT
              GO TO LOAD-POST.

      * SECURITY
           IF FORM-OBJ = "3Z"
              MOVE "SECVIZ" TO LOAD-STAT
              GO TO LOAD-POST.
           IF FORM-OBJ = "3O"
              MOVE "SECVIO" TO LOAD-STAT
              GO TO LOAD-POST.
           IF FORM-OBJ = "3S"
              MOVE "SECVIS" TO LOAD-STAT
              GO TO LOAD-POST.

       LOAD-POST.
      * MADE EXTERNAL
      *    CALL FLDVAL USING STAT EXT-EOBUF-LIST EXT-EOBUF-BUF.
      *    IF STAT NOT = "00"
      *       MOVE "FLDVAL EOBUF ERROR" TO MESS ER-MESSAGE
      *       PERFORM UPDATE-LOG
      *       PERFORM SEND-MESS
      *       GO TO IO-ERROR.
      *    CALL FLDVAL USING STAT LASTNEXT-LIST LASTNEXT-BUF.
      *    IF STAT NOT = "00"
      *       MOVE "FLDVAL LASTNEXT ERROR" TO MESS ER-MESSAGE
      *       PERFORM UPDATE-LOG
      *       PERFORM SEND-MESS
      *       GO TO IO-ERROR.

           IF EXT-EOBUF-ERRCD NOT = " " OR LOAD-STAT NOT = SPACES
              MOVE SPACES TO LAST-PROGRAM NEXT-PROGRAM
           ELSE
              MOVE WS-PROG-PATHNAME TO LAST-PROGRAM
              IF NEXT-PROGRAM NOT = " "
                 MOVE NEXT-PROGRAM TO FORM-NAM WS-PROG-PATHNAME
                 GO TO PGM-LOAD.

      *******************************************
       INITIAL-SETUP SECTION.
      *******************************************

           MOVE EORUN-DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE EORUN-PCODE TO EO-SPOOL-PCODE
                               EO-PCODE.
           PERFORM LOAD-EO-SPOOL.

           MOVE 0 TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           PERFORM CLOSE-EO1-FILE.
           IF IO-FG = 9
              MOVE "PROCEDURE CODE NOT FOUND" TO MESS ER-MESSAGE
              PERFORM UPDATE-LOG
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           PERFORM CLEAN-MESS.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ********************************
       MISC SECTION.
      ********************************
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
      *COPY "LIBGB/LOGUID.CPY".
       COPY "LIBLP/LPPRINT.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAN-MESS.
           MOVE SPACES TO WR-BUF.
           MOVE ALLMESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-DATE-TIME.
           ACCEPT CUR-DATE FROM CENTURY-DATE.
           ACCEPT ACC-TIME FROM TIME.
           MOVE ACC-HR TO CUR-HR.
           MOVE ACC-MN TO CUR-MN.
           MOVE ACC-SC TO CUR-SC.


      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBEO1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ************************************************
       END-PROGRAM SECTION.
      ************************************************
       ENDIT.

      *    CALL XFER USING STAT EORUN-ERR-LIST EORUN-ERROR.
           MOVE "SP/EOMENU" TO FORM-NAM.

           MOVE FORM-PATH TO FORM-PATHNAME.
       EXIT-PROG.
       STOP-RUN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "EORUN"       TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXIT PROGRAM.
