       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EOEXEC.
       DATE-WRITTEN.     7/19/88.
      ************************************************************************
      *   DIR :  SP
      *   DESC:  BATCH REPORT EXECUTION DRIVER
      *   NOTE:  TIMER WILL WORK UP TO 24 HOURS; FOR EXAMPLE, IT'S 9AM & YOU
      *          SELECT A START TIME OF 2AM, PROGRAM WILL ADD 24 HOURS ON TO
      *          THE TARGET OF 2AM & COUNT DOWN TO IT.
      * REV:
      *  061995 BAH MOVE COMMENT TO EXT-EOBUF-MESS, SO THE CPCORP
      *             H-TITLE PRINTS THE STEP NAME
      *  072695 BLL ADD VERIFY PASSWORD ROUTINE TO SEE IF
      *             PASSWORD ON FILE FOR SELECTED PROCEDURE CODE
      *  091595 BLL DON'T USE SYSTEM DATE FOR SPOOL DIRECTORY, USE
      *             DATE 2
      *  BLV 061198 REMOVED STATUS CHECK ON LOGUID CALL
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *             DECLARATIVES
      *  BAH 070329 PASSWORD TO ENTER A TIME INSTEAD OF F5-NOW, WORLD PR# 534
      *  BLM 130328 CHANGED ACCESS-CALL TO DIR-ACCESS-CALL, CHANGED MKDIR
      *             TO CALL "C$SYSTEM" INSTEAD OF SYSTEM-CALL
      *  BAH 130412 ADDED EXT-EOBUF-ERRCD EXTERNAL FOR ALL BATCHABLE PROGRAMS
      *  BLM 131118 WHEN IN AUDIT & GET-NEXT-REPORT-ENTRY RETURNS IO-FG OF 9
      *             PROGRAM SHOULD SEND MSG "RUNNOT RUN UNTIL ERRORS ARE
      *             CORRECTED".  BUT EXT-ROUTE-BUF WAS "60" SO SEND-MESS
      *             DIDN'T STOP, JUST WENT BACK TO MENU WITH NO MESSAGE.
      *             I FORCED 0 TO ROUTE-BUF IN THIS SITUATION, WE'RE GOING
      *             TO END-PROGRAM SO WHO CARES...
      *  BLM 140501 WHEN IN AUDIT & GET-NEXT-REPORT-ENTRY RETURNS IO-FG OF 9
      *   CS 140603 REPLACED MKDIR-CMD WITH C$MAKEDIR
      *  BLM 141031 PUT AUDIT ERRORS IN MESSAGE BOX SO YOU CAN SEE THEM
      *  MJD 160830 REMOVED DIRECTORY AND SLASH WORKERS FROM DISPLAY AND MOVE
      *             EO-PROGRAM DIRECTLY TO FORM-PROGX.
      *  BLM 170310 CHANGE EXT-EOBUF-DATE1 & EXT-EOBUF-DATE2 TO 9(8), PD0006
      *  20170905 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20220816 COMMENTED OUT LOGUID
      *  BAH 2024.0717 ADDED CHECK OF EXT-ACUSQL-CONNECT-STAT-BAD AFTER
      *                RETURN FROM EACH STEP, IN CASE A PROGRAM CAUSED
      *                AN ERROR SUCH AS MISSING AN SSP, IT SHOULD "FAIL" AND
      *                CONTINUE TO NEXT STEP BUT THE ERROR IN THE CALLED
      *                PROGRAM CAUSED A DISCONNECT TO THE DATABASE. NEEDS
      *                TO RECONNECT TO THE SQL SERVER TO WRITE TO THE EOFILE
      *                STEP THAT IT FAILED. CHANGE WAS MADE IN EOINIT  S35Q-88
      *                IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
      *                   PERFORM SQL-CONNECT
      * 
      ************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/EOEXEC S35 07/19/24-11:22:00 >".
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".

       COPY "LIBGB/EOBUF_EXT.CPY".

       01  ENTRY-ROUTE  PIC 99.
           88  FROM-EOD-MENU          VALUE 01.
           88  FROM-EOM-MENU          VALUE 02.
           88  FROM-EOY-MENU          VALUE 03.
           88  FROM-GB-MENU           VALUE 04.

       01  EOEXEC-WINDOW-HANDLE       PIC X(10).
       01  FIRST-STEP     PIC 9999.
       01  LAST-STEP      PIC 9999.
       01  CURRENT-STEP   PIC 9999.
       01  DISPLAY-STEP   PIC 9999.
       01  LOAD-STAT      PIC X(6).
       01  FIRST-TIME     PIC X.
       01  PCODE          PIC XXX.
       01  DATE1          PIC 9(8).
       01  DATE2          PIC 9(8).
       01  SINGLE-PAGE    PIC X.
       01  HOUR-BUF       PIC 99.
       01  MIN-BUF        PIC 99.
       01  WRAP-FLAG      PIC 9.
       01  CUR-MINUTES    PIC 9999.
       01  TRIG-MINUTES   PIC 9999.
       01  DIFF-MINUTES   PIC S9999.
       01  TIMER-LIST     PIC X(36)    VALUE
           "CHR CMIN IHR:F HRS AND:S IMIN:F MIN.".
       01  TIMER-MESS.
           03  D-CUR-HR   PIC 99.
           03  D-CUR-MN   PIC 99.
           03  D-IN-HR    PIC ZZ-      BLANK ZERO.
           03  D-HRS      PIC X(3).
           03  D-AND      PIC X.
           03  D-IN-MN    PIC Z9-      BLANK ZERO.
           03  D-MIN      PIC X(7).
       01  IN-HR          PIC S999.
       01  IN-MN          PIC S999.
       01  MESS-LIST      PIC X(5)  VALUE "MESS.".
       01  MSEL           PIC X(40) VALUE " ".
       01  ALLMESS-LIST   PIC X(20) VALUE "MSEL SEL MSEL2 MESS.".

       01  AUDIT-ERROR       PIC 9  VALUE 0.
       01  SECURITY-ERROR    PIC 9  VALUE 0.
       01  STEP-COUNT        PIC 9999.
       01  SKIP-SWITCH       PIC 9  VALUE 0.
       01  SKIP-STEP         PIC 9999 VALUE 0.

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

       01  SYSTIME-BUF.
           03  SYS-TIME     PIC 99/99/99.
       01  SYSTIME-LIST     PIC X(8)  VALUE "SYSTIME.".

       01  SCROLL-SUB       PIC 99.
       01  WK1              PIC 99.
       01  WK2              PIC 99.

       01  WS-PGM-NAME   PIC X(6).

       01  SCROLL-LIST.
           03  FILLER       PIC X(48)  VALUE
              "010 020 030 040 050 060 070 080 090 100 ENDMORE.".
       01  SCROLL-BUF.
           03  SCROLL-LINE   OCCURS 10 TIMES.
               05  SCROLL-LINE2            PIC X(79).
               05  THIS REDEFINES SCROLL-LINE2.
                   07  FILLER              PIC X(11).
                   07  SCROLL-CENTER       PIC X(57).
                   07  FILLER              PIC X(11).
               05  THIS REDEFINES SCROLL-LINE2.
                   07  SCROLL-STEP         PIC ZZZ9-.
                   07  SCROLL-PROG-NAME    PIC X(7).
                   07  SCROLL-PROG-FILLER  PIC X(3).
                   07  SCROLL-PROG-DESC    PIC X(31).
                   07  SCROLL-START-DATE   PIC 99/99/99-.
                   07  SCROLL-START-TIME   PIC 99/99/99-.
                   07  SCROLL-START-X REDEFINES SCROLL-START-TIME
                                           PIC X(9).
                   07  SCROLL-COMPL-TIME   PIC 99/99/99-.
                   07  SCROLL-COMPL-X REDEFINES SCROLL-COMPL-TIME
                                           PIC X(9).
                   07  SCROLL-COMPLETE-STAT PIC X(6).
               05  THIS REDEFINES SCROLL-LINE2.
                   07  FILLER              PIC X(46).
                   07  SCROLL-AUDIT-MESS.
                       09  FILLER          PIC X(18).
                       09  SCROLL-RUN-MESS PIC X(15).
               05  THIS REDEFINES SCROLL-LINE2.
                   07  FILLER              PIC X(19).
                   07  SCROLL-MESS         PIC X(60).
           03  SCROLL-END-MORE             PIC X(10).


       COPY "LIBWI/PFSWINDOWSW.CPY".
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
       COPY "LIBWI/EOSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  NAME-SEQ                  VALUE 0.
       78  TIME-SEQ                  VALUE 1.
     
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/EOEXEC_DEF.CPY".
       COPY "LIBWI/COUNTD_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBSP/EOEXEC_WKS.CPY".
       COPY "LIBWI/COUNTD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).

       SCREEN SECTION.
       COPY "LIBSP/EOEXEC_SCN.CPY".
       COPY "LIBWI/COUNTD_SCN.CPY".

       PROCEDURE DIVISION USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE "01" TO EXT-ROUTE-BUF.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE " " TO PCODE.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EOEXEC" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS EOEXEC-WINDOW-HANDLE.

      ************************************
      *   O P T I O N S   M E N U
      ************************************
       RRPCODE.
      *    IF FROM-EOD-MENU
      *       MOVE "D" TO VDUBUF
      *       GO TO DISP-PDESC.
      *    IF FROM-EOM-MENU
      *       MOVE "M" TO VDUBUF
      *       GO TO DISP-PDESC.
      *    IF FROM-EOY-MENU
      *       MOVE "Y" TO VDUBUF
      *       GO TO DISP-PDESC.
      *    CALL FRESET USING STAT.


           PERFORM FORM-RESET.

           MOVE "F7-EXIT, F6-SCAN, ENTER PROCEDURE CODE:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A030PCODE." TO VDU.
           MOVE PCODE TO VDUBUF.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO ENDIT.

           IF VDUSTATUS = "1<"
              PERFORM EO-SCAN.

           MOVE VDUBUF TO PCODE EO-PCODE.
           IF VDUSTATUS NOT = "00"
              GO TO RRPCODE.

           IF VDUBUF = SPACES AND PCODE NOT = SPACES
              PERFORM SCROLL-PAGE
              GO TO RRPCODE.
       DISP-PDESC.
           PERFORM LOAD-EO-SPOOL.
           MOVE VDUBUF TO PCODE EO-SPOOL-PCODE EO-PCODE.
           MOVE "S  A030PCODE." TO VDU.
           MOVE PCODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE PCODE TO EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           PERFORM CLOSE-EO1-FILE.
           IF IO-FG = 9
              MOVE 00 TO ENTRY-ROUTE
              MOVE " " TO PCODE
              GO TO RRPCODE.

           MOVE "S  A300PDESC." TO VDU.
           MOVE EO-COMMENT TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM VERIFY-BATCH-NAME-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE "RUNNING THIS BATCH IS NOT ALLOWED WITHOUT PASSWORD"
                                                TO MESS
              PERFORM SEND-MESS
              MOVE 00 TO ENTRY-ROUTE
              MOVE " " TO PCODE
              GO TO RRPCODE.

           PERFORM CLEAN-MESS.

           MOVE "N" TO SINGLE-PAGE.
           MOVE 0 TO DISPLAY-STEP.

           PERFORM SCROLL-PAGE.

           MOVE 0 TO MIN-BUF.

       RRHOUR.
           MOVE "F7, F5-PROCESS NOW, F3-CHECK TIME, MILITARY TIME:"
                    TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-DATE-TIME.
           MOVE "ENNN020HOUR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" OR "10" GO TO RRPCODE.
           IF VDUSTATUS = "16" GO TO SYTIME.
           IF VDUSTATUS = "1:" GO TO NOW.
           IF VDUSTATUS NOT = "00" GO TO RRHOUR.
           MOVE VDUNUM0 TO HOUR-BUF.
           IF HOUR-BUF > 23
              GO TO RRHOUR.
           MOVE "S  A020HOUR." TO VDU.
           MOVE HOUR-BUF TO VDUBUF.
           PERFORM SCREENIO.

       RRMIN.
           MOVE "ENNN020MIN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO SYTIME.
           IF VDUSTATUS = "1U" GO TO RRHOUR.
           IF VDUSTATUS = "1:" GO TO NOW.
           IF VDUSTATUS NOT = "00" GO TO RRHOUR.
           MOVE VDUNUM0 TO MIN-BUF.
           IF MIN-BUF > 59
              GO TO RRHOUR.
           MOVE "S  A020MIN." TO VDU.
           MOVE MIN-BUF TO VDUBUF.
           PERFORM SCREENIO.
           IF HOUR-BUF = 0
              IF MIN-BUF = 0
                 PERFORM SCROLL-PAGE
                 GO TO RRMIN.

      * CHECK IF PASSWORD NEEDED TO SET THE TIMER
           MOVE TIME-SEQ TO PASSWDW-SEQ.
           MOVE "SET THE TIMER" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO RRHOUR.

           GO TO BEGINIT.
       SYTIME.
           PERFORM GET-DATE-TIME.
           MOVE CUR-TIME TO SYS-TIME.
           INSPECT SYSTIME-BUF REPLACING ALL "/" BY ":".

           MOVE SYSTIME-LIST TO WR-LIST.
           MOVE SYSTIME-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM SUSPEND 3 TIMES.
           GO TO RRHOUR.
       NOW.
           MOVE "SNNN020HOUR." TO VDU.
           MOVE 99 TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020MIN." TO VDU.
           MOVE 99 TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A030NOW." TO VDU.
           MOVE "NOW" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 99 TO MIN-BUF HOUR-BUF.
       BEGINIT.
           MOVE "F1, ^, (RTN)-TODAYS DATE:" TO MESS.
           PERFORM SEND-LEGEND.
       RRDATE1.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO RRPCODE.
           IF VDUSTATUS = "1U" GO TO RRHOUR.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RRDATE1.
           IF VDUNUM0 = 0
              MOVE "S  8080DATE1." TO VDU
              MOVE CUR-DATE TO VDU-DATE-YYYYMMDD DATE1
              PERFORM SCREENIO.

           MOVE VDU-DATE-YYYYMMDD TO DATE1 NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO RRDATE1.
       RRDATE2.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO RRPCODE.
           IF VDUSTATUS = "1U" GO TO RRDATE1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RRDATE2.
           IF VDUNUM0 = 0
              MOVE "S  8060DATE2." TO VDU
              MOVE CUR-DATE TO VDU-DATE-YYYYMMDD DATE2
              PERFORM SCREENIO.

           MOVE VDU-DATE-YYYYMMDD TO DATE2 NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO RRDATE2.

           IF DATE2 < DATE1
              MOVE "INVALID DATE RANGE - RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO RRDATE1.

           MOVE "CREATING WORK FILES..........8" TO MESS.
           PERFORM SEND-LEGEND.


      ********************************************************************
      * A30: CHANGE ACCESS-CALL TO DIR-ACCESS-CALL, USE C$SYSTEM FOR MKDIR
      ********************************************************************
       CREATE-SPOOL-DIR.
           MOVE DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE.
           MOVE " " TO EO-SPOOL-FILE.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                 PERFORM SEND-MESS
                 MOVE EO-SPOOL TO MESS
                 PERFORM SEND-MESS
                 GO TO RRPCODE.

      **************************
      * BEGUPD.
      ***************************
            MOVE "Y" TO STAT.
      *     PERFORM PRU-IN.

            MOVE "F1, F3-ALL, ^, (RTN)-SCROLL BOTTOM:" TO MESS.
            PERFORM SEND-LEGEND.
        E-FIRST.
            MOVE "ENNN040FIRST." TO VDU.
            PERFORM SCREENIO.
            IF VDUSTATUS = "10" GO TO RRPCODE.
            IF VDUSTATUS = "16" GO TO ALL-STEPS.
            IF VDUSTATUS = "1U" GO TO RRDATE2.
            IF NOT (VDUSTATUS = "00" OR "1D") GO TO E-FIRST.
            MOVE VDUNUM0 TO FIRST-STEP.
            IF FIRST-STEP = 0
               PERFORM SCROLL-PAGE
               GO TO E-FIRST.

        E-LAST.
            MOVE "ENNN040LAST." TO VDU.
            PERFORM SCREENIO.
            IF VDUSTATUS = "10" GO TO RRPCODE.
            IF VDUSTATUS = "16" GO TO ALL-STEPS.
            IF VDUSTATUS = "1U" GO TO E-FIRST.
            IF NOT (VDUSTATUS = "00" OR "1D") GO TO E-LAST.
            MOVE VDUNUM0 TO LAST-STEP.
            IF LAST-STEP = 0
               PERFORM SCROLL-PAGE
               GO TO E-LAST.
            IF FIRST-STEP > LAST-STEP GO TO E-LAST.
            GO TO START-AUDIT.

        ALL-STEPS.
            MOVE "SNNN040FIRST." TO VDU.
            MOVE 1 TO VDUNUM0 FIRST-STEP.
            PERFORM SCREENIO.
            MOVE "SNNN040LAST." TO VDU.
            MOVE 9999 TO VDUNUM0 LAST-STEP.
            PERFORM SCREENIO.

        START-AUDIT.
            MOVE 0 TO AUDIT-ERROR STEP-COUNT SECURITY-ERROR
                      DISPLAY-STEP.

      D    STOP MESS.
            PERFORM CLEAR-EO-FILE.

            MOVE "60" TO EXT-ROUTE-BUF.
            MOVE SPACES TO LASTNEXT-BUF.
            PERFORM INIT-PROGRESS-SCREEN.
            IF IO-FG = 9
               MOVE "BAD STARTING REPORT NUMBER" TO MESS
               PERFORM SEND-MESS
               GO TO E-FIRST.

            MOVE SPACES TO EXT-EOBUF-ERRCD.
        NEXT-AUDIT.
            PERFORM GET-NEXT-REPORT-ENTRY.
            IF IO-FG = 9
               GO TO END-AUDIT.

            ADD 1 TO STEP-COUNT.

            PERFORM PROGRAM-LOAD.

            PERFORM CLEAN-MESS.

            PERFORM UPDATE-REPORT-ENTRY.
            IF EO-COMPLETE-STAT = "READY"
               GO TO NEXT-AUDIT.

            MOVE "S  A500MSEL2." TO VDU.

            MOVE SPACES TO MESS.

            IF EO-COMPLETE-STAT = "NOTFND"
               MOVE SPACES TO VDUBUF
               STRING "PROGRAM: " WS-PGM-NAME " IS MISSING"
                      DELIMITED BY SIZE INTO VDUBUF
            ELSE
            IF EO-COMPLETE-STAT = "FORMNF"
               MOVE "PROGRAM FORM MISSING" TO VDUBUF
            ELSE
            IF EO-COMPLETE-STAT = "SECVIZ"
               MOVE "CANNOT BE RUN--MEMORY OVERFLOW" TO VDUBUF
            ELSE
            IF EO-COMPLETE-STAT = "SECVIO"
               MOVE "CANNOT BE RUN--OPTION NOT PURCHASED" TO VDUBUF
               MOVE 1 TO SECURITY-ERROR
            ELSE
            IF EO-COMPLETE-STAT = "SECVIS"
               MOVE "CANNOT BE RUN--SECURITY VIOLATION" TO VDUBUF
               MOVE 1 TO SECURITY-ERROR
            ELSE
            IF EO-COMPLETE-STAT = "MEMOVF"
               MOVE "MEMORY OVERFLOW OCCURRED" TO VDUBUF
            ELSE
            IF EO-COMPLETE-STAT = "D.VERS"
               MOVE "NEW VERSION--RANGES MUST BE RE-ENTERED"
                                          TO VDUBUF
            ELSE
               MOVE SPACES TO VDUBUF
               STRING "EXECUTION ERROR OCCURRED (" EO-COMPLETE-STAT
                         ")" DELIMITED BY SIZE INTO VDUBUF.
            PERFORM SCREENIO.

           MOVE SPACES TO MESS.
           STRING "STEP: " EO-STEP " " WS-PGM-NAME 
                 "..        PRESS RTN:" DELIMITED BY SIZE INTO MSEL.
           PERFORM SEND-LEGEND.
           MOVE 1 TO AUDIT-ERROR.
        AUDIT-PAUSE.
      *     MOVE "R  A010SEL." TO VDU.
      *     PERFORM SCREENIO.
      *     IF VDUSTATUS = "1>" GO TO END-AUDIT.
      *     IF VDUSTATUS NOT = "00" GO TO AUDIT-PAUSE.
            IF SECURITY-ERROR = 0
               GO TO NEXT-AUDIT.
       END-AUDIT.
            IF SECURITY-ERROR = 1
               MOVE "S  A400MSEL ." TO VDU
               MOVE "INVALID AUTHORIZATION TO RUN PROCEDURE" TO VDUBUF
               PERFORM SCREENIO
               MOVE "S  A500MSEL2." TO VDU
               MOVE "PRESS <ENTER> TO EXIT:" TO VDUBUF
               PERFORM SCREENIO
               MOVE "E  A010SEL." TO VDU
               PERFORM SCREENIO
               MOVE "00" TO VDUSTATUS
               GO TO END-PROGRAM.
            IF AUDIT-ERROR = 1
               MOVE "S  A400MSEL." TO VDU
               MOVE "CANNOT RUN UNTIL ERRORS ARE CORRECTED" TO VDUBUF
               PERFORM SCREENIO
               MOVE "S  A500MSEL2." TO VDU
               MOVE "PRESS <ENTER> TO EXIT:" TO VDUBUF
               PERFORM SCREENIO
               MOVE "E  A010SEL." TO VDU
               PERFORM SCREENIO
               MOVE "00" TO VDUSTATUS
               GO TO END-PROGRAM.

            IF STEP-COUNT = ZERO
               PERFORM SCROLL-PAGE
               MOVE "INVALID RANGE" TO MESS
               PERFORM SEND-MESS
               GO TO E-FIRST.

            PERFORM CLEAN-MESS.

            MOVE "S  A400MSEL." TO VDU.
            MOVE "READY TO BEGIN BATCH PROCESSING!" TO VDUBUF.
            PERFORM SCREENIO.

            MOVE "S  A500MSEL2." TO VDU.
            MOVE "ENTER 'Y' TO START OR 'C' TO CANCEL:" TO VDUBUF.
            PERFORM SCREENIO.

         GET-Y-OR-F1.
            MOVE "E  A010SEL." TO VDU.
            PERFORM SCREENIO.
            IF VDUSTATUS NOT = "00" GO TO GET-Y-OR-F1.
            IF VDUBUF = "C" GO TO END-PROGRAM.
            IF VDUBUF NOT = "Y" GO TO GET-Y-OR-F1.

            PERFORM CLEAN-MESS.

      ******************************************
        COUNTDOWN-TIMER.
      ******************************************
            IF HOUR-BUF = 99 GO TO START-PROCESS.

            MOVE 11 TO WINDOW-LINES.
            MOVE 78 TO WINDOW-COLUMNS.
            MOVE 12 TO WINDOW-BEGIN-Y.
            MOVE 2 TO WINDOW-BEGIN-X.
            MOVE "COUNTD" TO VDU-FORM-NAME.
            MOVE "WI/COUNTD" TO WINDOW-FORM-NAM.

            PERFORM OPEN-WINDOW.
            DISPLAY COUNTD-SCREEN.

            MOVE COUNTD-FORM-FIELDS TO WR-BUF. 
            MOVE COUNTD-FORM-LIST TO WR-LIST. 
            PERFORM CALL-WRFORM.

            MOVE "S  A700DISTURB:SR." TO VDU.
            MOVE " * * *      D O   N O T   D I S T U R B      * * *"
                                  TO VDUBUF.
            PERFORM SCREENIO.

            ACCEPT ACC-TIME FROM TIME.
            COMPUTE CUR-MINUTES = (ACC-HR * 60) + ACC-MN.
            COMPUTE TRIG-MINUTES = (HOUR-BUF * 60) + MIN-BUF.
            IF CUR-MINUTES > TRIG-MINUTES
               MOVE 1 TO WRAP-FLAG
               ADD 1440 TO TRIG-MINUTES
            ELSE
               MOVE 0 TO WRAP-FLAG.
        TIMER-LOOP.
            IF CUR-MINUTES NOT < TRIG-MINUTES
               DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
               DESTROY COUNTD-SCREEN
               PERFORM CLOSE-WINDOW
               MOVE "EOEXEC" TO VDU-FORM-NAME
               GO TO START-PROCESS.

            MOVE ACC-HR TO D-CUR-HR.
            MOVE ACC-MN TO D-CUR-MN.
            SUBTRACT CUR-MINUTES FROM TRIG-MINUTES
                  GIVING DIFF-MINUTES.
            DIVIDE DIFF-MINUTES BY 60
                  GIVING IN-HR REMAINDER IN-MN.
            MOVE IN-HR TO D-IN-HR.
            IF IN-HR = 0
               MOVE " " TO D-HRS
            ELSE
               IF IN-HR = 1
                  MOVE "HR " TO D-HRS
               ELSE
                  MOVE "HRS " TO D-HRS.
            MOVE IN-MN TO D-IN-MN.
            IF IN-MN = 0
               MOVE " " TO D-MIN
            ELSE
               IF IN-MN = 1
                  MOVE "MINUTE" TO D-MIN
               ELSE
                  MOVE "MINUTES" TO D-MIN.
            MOVE " " TO D-AND.
            IF D-HRS NOT = " "
               IF D-MIN NOT = " "
                    MOVE "&" TO D-AND.

            MOVE TIMER-MESS TO WR-BUF.
            MOVE TIMER-LIST TO WR-LIST.
            PERFORM CALL-WRFORM.

            SUBTRACT ACC-SC FROM 61 GIVING ACC-SC.
            PERFORM SUSPEND ACC-SC TIMES.
            ACCEPT ACC-TIME FROM TIME.
            IF ACC-HR = 00 AND WRAP-FLAG = 1
               MOVE 2 TO WRAP-FLAG.
            COMPUTE CUR-MINUTES = (ACC-HR * 60) + ACC-MN.
            IF WRAP-FLAG = 2
               ADD 1440 TO CUR-MINUTES.

            GO TO TIMER-LOOP.

      ******************************************
        START-PROCESS.
      ******************************************
            MOVE "BATCH PROCESSING IN PROGRESS..." TO MESS.
            PERFORM SEND-LEGEND.

            PERFORM CLEAR-EO-FILE.

            MOVE "70" TO EXT-ROUTE-BUF.
            MOVE SPACES TO LASTNEXT-BUF.
            PERFORM INIT-PROGRESS-SCREEN.
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
               PERFORM PROGRAM-LOAD
               PERFORM CLEAN-MESS.

            PERFORM UPDATE-REPORT-ENTRY.
            IF EXT-EOBUF-ERRCD = "I"
               GO TO EXIT-PROG.

            GO TO NEXT-PROCESS.

        END-PROCESS.
            MOVE ZERO TO DISPLAY-STEP.
            PERFORM SCROLL-PAGE.
            MOVE "S  A020HOUR." TO VDU.
            MOVE "00" TO VDUBUF.
            PERFORM SCREENIO.
            MOVE "S  A020MIN." TO VDU.
            MOVE "00" TO VDUBUF.
            PERFORM SCREENIO.
            MOVE 0 TO MIN-BUF.
            GO TO RRHOUR.

      *************************************
       SCROLL-PAGE SECTION.
      *************************************

           IF SINGLE-PAGE = "Y"
              GO TO SCROLL-EXIT.

           PERFORM OPEN-EO1-FILE.
           MOVE PCODE TO EO-PCODE.
           MOVE DISPLAY-STEP TO EO-STEP.
           PERFORM START-EO1-FILE.

           MOVE SPACES TO SCROLL-BUF.

           MOVE 0 TO SCROLL-SUB.
       SCROLL-NEXT.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO SCROLL-END.

           IF EO-PCODE NOT = PCODE
              GO TO SCROLL-END.

           IF EO-STEP = 0
              GO TO SCROLL-NEXT.

           IF SCROLL-SUB = 10
              GO TO SCROLL-MORE.

           IF EO-MESS NOT = SPACES AND SCROLL-SUB = 9
              GO TO SCROLL-MORE.

           ADD 1 TO SCROLL-SUB.
           MOVE EO-STEP TO SCROLL-STEP(SCROLL-SUB).
           MOVE SPACES TO SCROLL-PROG-FILLER(SCROLL-SUB).
           MOVE EO-PROGRAM TO SCROLL-PROG-NAME(SCROLL-SUB).
           MOVE EO-COMMENT TO SCROLL-PROG-DESC(SCROLL-SUB).
           IF EO-START-DATE > 0
              MOVE EO-START-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO SCROLL-START-DATE(SCROLL-SUB)
              MOVE EO-START-TIME TO SCROLL-START-TIME(SCROLL-SUB)
              INSPECT SCROLL-START-X(SCROLL-SUB)
                      REPLACING ALL "/" BY ":"
              MOVE EO-COMPLETE-TIME TO SCROLL-COMPL-TIME(SCROLL-SUB)
              INSPECT SCROLL-COMPL-X(SCROLL-SUB)
                      REPLACING ALL "/" BY ":".
           MOVE EO-COMPLETE-STAT TO SCROLL-COMPLETE-STAT(SCROLL-SUB).
           IF EO-MESS NOT = SPACES
              ADD 1 TO SCROLL-SUB
              MOVE EO-MESS TO SCROLL-MESS(SCROLL-SUB).
           GO TO SCROLL-NEXT.

       SCROLL-MORE.
           MOVE "N" TO SINGLE-PAGE.
           MOVE "MORE..." TO SCROLL-END-MORE.
           MOVE EO-STEP TO DISPLAY-STEP.

           GO TO SCROLL-XFER.

       SCROLL-END.
           IF DISPLAY-STEP = 0
              MOVE "Y" TO SINGLE-PAGE.

           MOVE 0 TO DISPLAY-STEP.

           MOVE "** END **" TO SCROLL-END-MORE.
       SCROLL-XFER.
           PERFORM SEND-SCROLL.
       SCROLL-EXIT.
           PERFORM CLOSE-EO1-FILE.


      *************************************
       CLEAR-EO-FILE SECTION.
      *************************************
           MOVE PCODE TO EO-PCODE.
           MOVE FIRST-STEP TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM START-EO1-FILE.
       CLEAR-NEXT-EO.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CLEAR-EO-EXIT.

           IF EO-PCODE NOT = PCODE
              GO TO CLEAR-EO-EXIT.

           IF EO-STEP > LAST-STEP
              GO TO CLEAR-EO-EXIT.

           MOVE ZERO TO EO-START-TIME EO-COMPLETE-TIME
                        EO-START-DATE EO-COMPLETE-DATE.
           MOVE SPACES TO EO-COMPLETE-STAT EO-MESS.

           PERFORM REWRITE-EO1-FILE.
           GO TO CLEAR-NEXT-EO.

       CLEAR-EO-EXIT.
           PERFORM CLOSE-EO1-FILE.


      *************************************
       INIT-PROGRESS-SCREEN SECTION.
      *************************************
           MOVE SPACES TO SCROLL-BUF.
           PERFORM OPEN-EO1-FILE.

           MOVE PCODE TO EO-PCODE.
           MOVE FIRST-STEP TO EO-STEP.
           PERFORM START-EO1-FILE.
           IF IO-FG = 9
              GO TO INIT-DISP-EXIT.

           MOVE 1 TO SCROLL-SUB.
       INIT-DISP-NEXT.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO INIT-DISP-XFER.

           IF EO-PCODE NOT = PCODE OR EO-STEP > LAST-STEP
              GO TO INIT-DISP-XFER.

           MOVE EO-STEP TO SCROLL-STEP(SCROLL-SUB).
           MOVE SPACES TO SCROLL-PROG-FILLER(SCROLL-SUB).
           MOVE EO-PROGRAM TO SCROLL-PROG-NAME(SCROLL-SUB).
           MOVE EO-COMMENT TO SCROLL-PROG-DESC(SCROLL-SUB).
           ADD 1 TO SCROLL-SUB.
           IF SCROLL-SUB NOT > 10
              GO TO INIT-DISP-NEXT.
       INIT-DISP-XFER.
           MOVE 0 TO IO-FG.
           PERFORM SEND-SCROLL.
       INIT-DISP-EXIT.
           MOVE ZERO TO SCROLL-SUB.
           PERFORM CLOSE-EO1-FILE.
           MOVE FIRST-STEP TO CURRENT-STEP.
           MOVE "Y" TO FIRST-TIME.


      *************************************
       GET-NEXT-REPORT-ENTRY SECTION.
      *************************************

           PERFORM OPEN-EO1-FILE.

       GET-NEXT-REPORT.

           MOVE PCODE TO EO-PCODE.
           MOVE CURRENT-STEP TO EO-STEP.

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

           IF EO-PCODE NOT = PCODE OR EO-STEP > LAST-STEP
              MOVE 9 TO IO-FG
              GO TO GET-NEXT-EXIT.

           IF SCROLL-SUB < 10
              ADD 1 TO SCROLL-SUB
              GO TO GET-NEXT-SKIP-SCROLL.

           MOVE "N" TO SINGLE-PAGE.
           MOVE 1 TO WK1.
           MOVE 2 TO WK2.
       GET-NEXT-SCROLL.
           MOVE SCROLL-LINE(WK2) TO SCROLL-LINE(WK1).
           IF WK2 < 10
              ADD 1 TO WK1, WK2
              GO TO GET-NEXT-SCROLL.

       GET-NEXT-SKIP-SCROLL.
           MOVE EO-STEP TO SCROLL-STEP(SCROLL-SUB) CURRENT-STEP.
           MOVE SPACES TO SCROLL-PROG-FILLER(SCROLL-SUB).
           MOVE EO-PROGRAM TO SCROLL-PROG-NAME(SCROLL-SUB).
           MOVE EO-COMMENT TO SCROLL-PROG-DESC(SCROLL-SUB).
           IF EXT-ROUTE-BUF = "60"
              MOVE "VERIFYING..." TO SCROLL-AUDIT-MESS(SCROLL-SUB)
              GO TO GET-NEXT-EXIT.

           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO EO-START-DATE.
           MOVE CUR-TIME TO EO-START-TIME.
           MOVE EO-START-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO SCROLL-START-DATE(SCROLL-SUB).
           MOVE EO-START-TIME TO SCROLL-START-TIME(SCROLL-SUB).
           INSPECT SCROLL-START-X(SCROLL-SUB)
                   REPLACING ALL "/" BY ":".

           MOVE "IN PROGRESS..." TO SCROLL-RUN-MESS(SCROLL-SUB).

           PERFORM REWRITE-EO1-FILE.

       GET-NEXT-EXIT.
           PERFORM SEND-SCROLL.

           MOVE EO-PCODE TO EXT-EOBUF-PCODE.
           MOVE SPACES TO EXT-EOBUF-MESS.
           MOVE EO-PARAMETERS TO EXT-EOBUF-DATA.
           MOVE EO-COPIES TO EXT-EOBUF-COPIES.
           MOVE DATE1 TO EXT-EOBUF-DATE1.
           MOVE DATE2 TO EXT-EOBUF-DATE2.
           MOVE "/" TO EO-SPOOL-FILE.
           MOVE EO-PROGRAM TO EO-SPOOL-NAME.
           MOVE EO-STEP TO EO-SPOOL-STEP.
           IF EO-DIR = "PY"
              MOVE "_P" TO EO-SPOOL-SUF
           ELSE
              MOVE "  " TO EO-SPOOL-SUF.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO EXT-EOBUF-PRPATH.
           MOVE "~" TO EXT-EOBUF-ERRCD.
      * PRINT THE COMMENT IN THE H-TITLE FOR CPCORP
           IF EXT-ROUTE-BUF = "70"
              MOVE EO-COMMENT TO EXT-EOBUF-MESS.

      * MADE EOBUF EXTERNAL
           MOVE EO-PROGRAM TO WS-PGM-NAME FORM-PROGX.
           PERFORM CLOSE-EO1-FILE.

      *************************************
       UPDATE-REPORT-ENTRY SECTION.
      *************************************
       UPDATE-RPT-ENTRY.
           PERFORM OPEN-EO1-FILE.
           MOVE PCODE TO EO-PCODE.
           MOVE CURRENT-STEP TO EO-STEP.
           PERFORM READ-EO1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-EOBUF-MESS TO EO-MESS.
           IF LOAD-STAT = "NOTFND"
              MOVE SPACES TO EO-MESS
              STRING "PROGRAM: " WS-PGM-NAME " IS MISSING"
                     DELIMITED BY SIZE INTO EO-MESS.
           IF SKIP-SWITCH = 2
              MOVE "SKIP'D" TO EO-COMPLETE-STAT
           ELSE
              MOVE 1 TO SKIP-SWITCH
              IF LOAD-STAT NOT = SPACES
                 MOVE LOAD-STAT TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "!"
                 MOVE "FATAL!" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "V"
                 MOVE "D.VERS" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "|"
                 MOVE "SECVIS" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "S"
                 MOVE SPACES TO EO-COMPLETE-STAT
                 STRING "ERR :" EXT-EOBUF-ERRCD
                    DELIMITED BY SIZE INTO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "I"
                 MOVE "CANCEL" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD NOT = " "
                 MOVE SPACES TO EO-COMPLETE-STAT
                 STRING "FAIL:" EXT-EOBUF-ERRCD
                    DELIMITED BY SIZE INTO EO-COMPLETE-STAT
              ELSE
                 MOVE 0 TO SKIP-SWITCH
                 MOVE "RUN OK" TO EO-COMPLETE-STAT.

      *
      *    IF SKIP-SWITCH = 1
      *       IF EO-ERROR-STEP-X IS NUMERIC
      *          IF EO-ERROR-STEP > EO-STEP
      *             MOVE EO-ERROR-STEP TO SKIP-STEP
      *             MOVE 2 TO SKIP-SWITCH.
           IF SKIP-SWITCH = 1
              MOVE 0 TO SKIP-SWITCH.
           IF EXT-ROUTE-BUF = "60"
              IF EO-COMPLETE-STAT = "RUN OK"
                 MOVE EXT-EOBUF-DATA TO EO-PARAMETERS
                 MOVE "READY TO RUN."
                      TO SCROLL-AUDIT-MESS(SCROLL-SUB)
                 MOVE "READY" TO EO-COMPLETE-STAT
                 GO TO UPDATE-ENTRY-EXIT
              ELSE
                 MOVE "ERROR--UNABLE TO RUN"
                      TO SCROLL-AUDIT-MESS(SCROLL-SUB)
                 MOVE EO-COMPLETE-STAT
                      TO SCROLL-COMPLETE-STAT(SCROLL-SUB)
                 GO TO UPDATE-ENTRY-EXIT.
           PERFORM GET-DATE-TIME.
           MOVE CUR-TIME TO EO-COMPLETE-TIME.
           MOVE CUR-DATE TO EO-COMPLETE-DATE.
           MOVE EO-COMPLETE-TIME TO SCROLL-COMPL-TIME(SCROLL-SUB).
           INSPECT SCROLL-COMPL-X(SCROLL-SUB)
                   REPLACING ALL "/" BY ":".
           MOVE EO-COMPLETE-STAT TO
                       SCROLL-COMPLETE-STAT(SCROLL-SUB).
       UPDATE-ENTRY-EXIT.
           IF EO-MESS = SPACES
              GO TO UPDATE-MESS-SKIP.
           IF SCROLL-SUB = 9
              MOVE 10 TO SCROLL-SUB
              GO TO UPDATE-SKIP-SCROLL.
           IF SCROLL-SUB < 9
              ADD 1 TO SCROLL-SUB
              GO TO UPDATE-INSERT-LINE.
           MOVE "N" TO SINGLE-PAGE.
           MOVE 1 TO WK1.
           MOVE 2 TO WK2.
       UPDATE-SCROLL-UP.
           MOVE SCROLL-LINE(WK2) TO SCROLL-LINE(WK1).
           IF WK2 < 10
              ADD 1 TO WK1, WK2
              GO TO UPDATE-SCROLL-UP.
           GO TO UPDATE-SKIP-SCROLL.

       UPDATE-INSERT-LINE.
           MOVE "N" TO SINGLE-PAGE.
           MOVE 10 TO WK1.
           MOVE 9 TO WK2.
       UPDATE-SCROLL-DOWN.
           MOVE SCROLL-LINE(WK2) TO SCROLL-LINE(WK1).
           IF WK2 > SCROLL-SUB
              SUBTRACT 1 FROM WK1 WK2
              GO TO UPDATE-SCROLL-DOWN.
       UPDATE-SKIP-SCROLL.
           MOVE SPACES TO SCROLL-LINE(SCROLL-SUB).
           MOVE EO-MESS TO SCROLL-MESS(SCROLL-SUB).
       UPDATE-MESS-SKIP.
           PERFORM REWRITE-EO1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM SEND-SCROLL.
           PERFORM CLOSE-EO1-FILE.


      ********************************
       PROGRAM-LOAD SECTION.
      ********************************
       PGM-LOAD.

           MOVE SPACES TO NEXT-PROGRAM.
      * MADE LASTNEXT-BUF EXTERNAL
      *    CALL XFER USING STAT LASTNEXT-LIST LASTNEXT-BUF.
           MOVE SPACES TO LOAD-STAT.
           IF LOADCO-OP1 = " " OR LOADCO-OP1 = "1"
              GO TO LOAD-IT.

           MOVE FORM-NAM TO LOADCO-ARG.
           MOVE LOADCO-OPT TO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

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

      D    STOP MESS.
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

      *  BAH 2024.0717 ADDED CHECK OF EXT-ACUSQL-CONNECT-STAT-BAD AFTER
      *                RETURN FROM EACH STEP, IN CASE A PROGRAM CAUSED
      *                AN ERROR SUCH AS MISSING AN SSP, IT SHOULD "FAIL" AND
      *                CONTINUE TO NEXT STEP BUT THE ERROR IN THE CALLED
      *                PROGRAM CAUSED A DISCONNECT TO THE DATABASE. NEEDS
      *                TO RECONNECT TO THE SQL SERVER TO WRITE TO THE EOFILE
      *                STEP THAT IT FAILED.
           IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
              MOVE "SQL DISCONNECT" TO EXT-EOBUF-MESS
              PERFORM SQL-CONNECT
              IF ( EXT-ACUSQL-CONNECT-STAT-BAD )
                 MOVE "E" TO EXT-EOBUF-ERRCD.

           IF EXT-EOBUF-ERRCD NOT = " " OR LOAD-STAT NOT = SPACES
              MOVE SPACES TO LAST-PROGRAM NEXT-PROGRAM
           ELSE
              MOVE WS-PGM-NAME TO LAST-PROGRAM
              IF NEXT-PROGRAM NOT = SPACES
                 MOVE NEXT-PROGRAM TO FORM-NAM WS-PGM-NAME
                 STRING NEXT-PROGRAM "...." DELIMITED BY SIZE
                    INTO SCROLL-RUN-MESS(SCROLL-SUB)
                 PERFORM SEND-SCROLL
                 GO TO PGM-LOAD.


      *************************************
       EO-SCAN SECTION.
      *************************************
           MOVE SPACES TO EOSCAN-KEY.
           MOVE "WI/EOSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EOSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF EOSCAN-STATUS NOT = "00" OR EOSCAN-KEY = SPACES
              MOVE "X" TO VDUSTATUS
              GO TO EXIT-EO-SCAN.

           MOVE "S  A030PCODE." TO VDU.
           MOVE EOSCAN-KEY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE EOSCAN-KEY TO VDUBUF.
           MOVE "00" TO VDUSTATUS.

       EXIT-EO-SCAN.
           EXIT.

      ******************************************************************
       VERIFY-BATCH-NAME-PASSWORD SECTION.
      ******************************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE PCODE TO PASSWDW-PGM.
           MOVE NAME-SEQ TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD TO" TO PASSWDW-PROMPT1.
           MOVE "RUN THIS BATCH PROCEDURE:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************
       VERIFY-PASSWORD SECTION.
      ***************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "EOEXEC" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD TO" TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           MOVE EXT-CONAME-CONAME TO EOEXEC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOEXEC-SYSDATE.
           DISPLAY EOEXEC-SCREEN UPON EOEXEC-WINDOW-HANDLE.
           MOVE EOEXEC-FORM-FIELDS TO WR-BUF.
           MOVE EOEXEC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE EOEXEC-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOEXEC-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/EOEXEC_EVA.CPY".
              COPY "LIBWI/COUNTD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************
       MISC SECTION.
      ********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
      *COPY "LIBGB/LOGUID.CPY".
       SEND-SCROLL.
           MOVE SCROLL-LIST TO WR-LIST.
           MOVE SCROLL-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
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

      ********************************
       IO-ROUTINES SECTION.
      ********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-EO1-FILE.
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBEO1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************
       END-PROGRAM SECTION.
      ******************************************************
       ENDIT.
       EXIT-PROG.
           MOVE "SP/EOMENU" TO FORM-NAM.

           MOVE EXIT-PATHNAME TO FORM-PATHNAME.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EOEXEC-SCREEN.
           DESTROY EOEXEC-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
      *A30 PARTIAL CONVERSION  * NEED TO IDENTIFY WHERE CALLED FROM
