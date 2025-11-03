       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PERUSE.
       DATE-WRITTEN.    4/23/92.
      **************************************************************************
      *  DIR : GB
      *  DESC: PERUSE PRINTED REPORT
      *
      * REV:
      *  MJD 102396 ADDED OPTION FOR PIGGYBACK PRINTING.
      *  JTG 032098 CHANGED TO SET DEFAULT USERID TO ENV: 'LOGNAME'
      *             SINCE ALTERNATE LOGIN SETS       ENV: 'POSTNAME'
      *             TO THE NEW BRANCH YOUR POSTING TO. BUT ALL REPORTS
      *             CREATED UNDER THE ALTERNATE ENV: 'POSTNAME' ARE
      *             CREATED BY THE OPERATING SYSTEM WITH YOU SIGNIN ID 'LOGNAME'
      *  MG  991007 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  KEC 111099 MOVE SPACES TO WK-REPORT BEFORE READING THE NEXT WK;
      *              PROGRAM NAMES THAT ARE LESS THAN 6 CHARACTERS
      *              (FOR EXAMPLE, LIBAT) WILL SHOW PART OF THE PREVIOUS
      *              NAME.  FOR EXAMPLE:
      *
      *                   REPORT NAME             DISPLAYED AS
      *                   ------------            ------------
      *                   GNEXTRA002CA            GNEXTRA002CA
      *                   GNEXTRA002FX            GNEXTRA002FX
      *                   LIBATA005JC             LIBATA005JCX
      *                   LIBATA005E7             LIBATA005E7X
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  MJD 000522 AS A TEST REPLACES PARADATA SYSTEM-CALL WITH ACUCOBOL
      *             SYSTEM CALL. ACU'S CALL MAKES PERUSE WORK CORRECTLY
      *             IN DEBUG (RUNCBL W/O -B).
      *  BLV 030625 CHANGED WORK FILE TO BE VARIABLE LENGTH & UNSTRING THE
      *             RECORD TO WORKING STORAGE FIELDS TO ACCOMODATE CHANGE IN
      *             LS COMMAND IN AIX 5.0
      *  BLV 030627 CLEAR WK-REC BEFORE READING NEXT; VARIABLE LENGTH FILE NOW
      *             & SHORTER RECORD (LIKE 5 CHARACTER PROGRAM NAME) HAD
      *             EXTRA GARBAGE FROM PRIOR RECORD AT END SO YOU COULDN'T
      *             ACCESS IT
      *  BLV 030701 FIX PROBLEM WITH DAY UNSTRINGING AS LEFT JUSTIFIED &
      *             NOT MATCHING SYS-DAY IF 1 DIGIT DAY IN SCO
      *  BAH 030922 WAS FORCING MATCH OF DATE, PERIOD IN MIDDLE OF 'IF', COULD
      *             NOT SEE ANY OTHER DATES IF ANSWER 'N' TO TODAY ONLY.
      *             KEVIN
      *   CS 120614 REPLACED LACESS WITH ACCESS (C$FILEINFO) UPGRADE TO NEW
      *             PLATFORM VERYANT
      *  BLM 141110 IF I-AM-REGACC, USE ENVIRONMENT VARIABLE 'USER' INSTEAD OF
      *             'LOGNAME'.   REGIONAL LOGS IN WITH THEIR BBT LOGIN &
      *             SETS LOGNAME & POSTNAME TO PARADATA TYPE LOGINS, LIKE
      *             X1234XXX.  SO NEED TO USE ACTUALLY LOGIN SINCE THAT IS
      *             WHAT REPORTS IN /USR/TMP ARE STAMPED WITH. REGACC PR# 360
      *  JKC 211109 THERE WAS AN ERROR ON THE DISPLAY OF PAGES 2 &
      *             BEYOND; IT SKIPPED THE FIRST RECORD FOR EACH PAGE.
      *             ADDED SKIP-READ-NEXT-WK TO KNOW IF TO DO READ-NEXT.
      *
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKS.CPY".
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       FD  WK-FILE
           RECORD IS VARYING IN SIZE FROM 76 TO 90 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-CHAR             PIC X OCCURS 90 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/PERUSE.CBL S35 03/08/24-14:47:31 >".

      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************
       01  WK-IFN          PIC X(2) VALUE "WK".


      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  WS-WK-REC.
           03  WK-READ-WRITE   PIC X(10).
           03  WK-LINKS        PIC X(1).
           03  WK-USERID       PIC X(9).
           03  WK-GROUP        PIC X(9).
           03  WK-SIZE         PIC X(9).
           03  WK-MONTH        PIC X(3).
           03  WK-DAY          PIC X(2).
           03  WK-TIME         PIC X(5).
           03  WK-DIR-REPORT.
               05  WK-DIR      PIC X(9).
               05  WK-REPORT   PIC X(12).


       01  SAVE-PRU-PATH   PIC X(75) VALUE SPACES.
       01  QUEUE-FLAG      PIC X VALUE "N".
       01  LCK-FG          PIC X VALUE "N".
       01  PRINTER-SELECTED  PIC X VALUE "N".
       01  USERID          PIC X(10).
       01  SYS-MONTH       PIC 99.
       01  SYS-DAY         PIC 99.
       01  WORK-DAY        PIC 99.
       01  WORK-DAYX REDEFINES WORK-DAY.
           03  WORK-DAY1   PIC X.
           03  WORK-DAY2   PIC X.

       01  ERRCD           PIC X VALUE " ".
       01  SUB             PIC 999   VALUE 0.
       01  CHECK-DATE-FLAG PIC X     VALUE "N".
       01  CHECK-USER-FLAG PIC X     VALUE "N".
       01  SKIP-READ-NEXT-WK PIC X(1) VALUE "N".

      *01  SEL             PIC 999.
       01  REPORT-NO-X.
           03  REPORT-NO   PIC 999.

      *    **********************************
      *    *      W-SCREEN DISPLAY BUFFER
      *    **********************************
       01  MESS-LIST       PIC X(5)   VALUE "MESS.".
       01  FLD-LIST        PIC X(5)   VALUE "FLD.".

       01  DISPLAY-BUF           VALUE SPACES.
           03  D-FIELD           OCCURS 30 TIMES.
               05  D-NO          PIC Z9.
               05  D-DOT         PIC X.
               05  D-DESC.
                   07  D-REPORT  PIC X(12).
                   07  FILLER    PIC X.
                   07  D-MONTH   PIC X(3).
                   07  FILLER    PIC X.
                   07  D-DAY     PIC X(2).
                   07  FILLER    PIC X.
                   07  D-TIME    PIC X(5).
                   07  FILLER    PIC X.
                   07  D-USERID  PIC X(9).
           03  D-END-MORE        PIC X(10).

       01  DISPLAY-LIST.
           03  FILLER   PIC X(10) VALUE "01P:P 01F ".
           03  FILLER   PIC X(10) VALUE "02P:P 02F ".
           03  FILLER   PIC X(10) VALUE "03P:P 03F ".
           03  FILLER   PIC X(10) VALUE "04P:P 04F ".
           03  FILLER   PIC X(10) VALUE "05P:P 05F ".
           03  FILLER   PIC X(10) VALUE "06P:P 06F ".
           03  FILLER   PIC X(10) VALUE "07P:P 07F ".
           03  FILLER   PIC X(10) VALUE "08P:P 08F ".
           03  FILLER   PIC X(10) VALUE "09P:P 09F ".
           03  FILLER   PIC X(10) VALUE "10P:P 10F ".
           03  FILLER   PIC X(10) VALUE "11P:P 11F ".
           03  FILLER   PIC X(10) VALUE "12P:P 12F ".
           03  FILLER   PIC X(10) VALUE "13P:P 13F ".
           03  FILLER   PIC X(10) VALUE "14P:P 14F ".
           03  FILLER   PIC X(10) VALUE "15P:P 15F ".
           03  FILLER   PIC X(10) VALUE "16P:P 16F ".
           03  FILLER   PIC X(10) VALUE "17P:P 17F ".
           03  FILLER   PIC X(10) VALUE "18P:P 18F ".
           03  FILLER   PIC X(10) VALUE "19P:P 19F ".
           03  FILLER   PIC X(10) VALUE "20P:P 20F ".
           03  FILLER   PIC X(10) VALUE "21P:P 21F ".
           03  FILLER   PIC X(10) VALUE "22P:P 22F ".
           03  FILLER   PIC X(10) VALUE "23P:P 23F ".
           03  FILLER   PIC X(10) VALUE "24P:P 24F ".
           03  FILLER   PIC X(10) VALUE "25P:P 25F ".
           03  FILLER   PIC X(10) VALUE "26P:P 26F ".
           03  FILLER   PIC X(10) VALUE "27P:P 27F ".
           03  FILLER   PIC X(10) VALUE "28P:P 28F ".
           03  FILLER   PIC X(10) VALUE "29P:P 29F ".
           03  FILLER   PIC X(10) VALUE "30P:P 30F ".
           03  FILLER   PIC X(8)  VALUE "ENDMORE.".

       01  SEL-MSG1.
           03  FILLER     PIC X(39) VALUE
           "F7-EXIT, LIST ONLY, REPORTS CREATED ON ".
           03  SEL-MONTH  PIC XXX   VALUE SPACES.
           03  FILLER     PIC X     VALUE SPACES.
           03  SEL-DAY    PIC Z9.
           03  FILLER     PIC X(9)  VALUE "?  (Y/N)".

       01  SEL-MSG2.
           03  FILLER     PIC X(39) VALUE
           "F7-EXIT, LIST ONLY, REPORTS CREATED BY ".
           03  SEL-USER   PIC X(10) VALUE SPACES.
           03  FILLER     PIC X(9)  VALUE "?  (Y/N)".

       COPY "LIBGB/PASSWDW.CPY".
      *COPY "LIBGB/PERUSE_CMD.CPY".
       01  LS-COMMAND.
           03  LS-TMP            PIC X(15) VALUE "LS -L /USR/TMP/".
           03  FILLER            PIC X(14) VALUE "[A-W,Y-Z][0-Z]".
           03  FILLER            PIC X(16) VALUE "[0-Z][0-Z][0-Z]*".
           03  FILLER            PIC X(3)  VALUE " > ".
           03  LS-TEMPFILE       PIC X(35).
       01  M-COMMAND.
           03  M-TMP             PIC X(16) VALUE "PERUSE /USR/TMP/".
           03  M-REPORT          PIC X(35).
       01  CAT-COMMAND.
           03  CAT-TMP           PIC X(22) VALUE
                                           "TPUT MC5;CAT /USR/TMP/".
           03  CAT-REPORT        PIC X(38).
           03  FILLER            PIC X(9) VALUE ";TPUT MC4".
       01  PERUSE-QUEUE-PATH.
           03  Q-TMP             PIC X(9)  VALUE "/USR/TMP/".
           03  Q-PATH-REPORT     PIC X(12).

      * TRANSPARENT PRINT ON:
       01  CMD-PRU       PIC X(8)  VALUE "TPUT MC5".
      * TRANSPARENT PRINT OFF:
       01  CMD-PRUOFF    PIC X(8)  VALUE "TPUT MC4".

       01  MONTH-BUF.
           03  FILLER            PIC X(3) VALUE "JAN".
           03  FILLER            PIC X(3) VALUE "FEB".
           03  FILLER            PIC X(3) VALUE "MAR".
           03  FILLER            PIC X(3) VALUE "APR".
           03  FILLER            PIC X(3) VALUE "MAY".
           03  FILLER            PIC X(3) VALUE "JUN".
           03  FILLER            PIC X(3) VALUE "JUL".
           03  FILLER            PIC X(3) VALUE "AUG".
           03  FILLER            PIC X(3) VALUE "SEP".
           03  FILLER            PIC X(3) VALUE "OCT".
           03  FILLER            PIC X(3) VALUE "NOV".
           03  FILLER            PIC X(3) VALUE "DEC".
       01  MONTH-BUFX REDEFINES MONTH-BUF.
           03  MONTH-RAY         PIC X(3) OCCURS 12.
       01  MONTH-BUFY REDEFINES MONTH-BUF.
           03  FILLER            OCCURS 12.
               05  FILLER        PIC X.
               05  MONTH-LOWER   PIC XX.

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/PERUSE_DEF.CPY".
       COPY "LIBGB/PERUSE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       01  PERUSE-WINDOW-HANDLE   PIC X(10).
       01  PERUSE2-WINDOW-HANDLE   PIC X(10).

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGB/PERUSE_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ********************************************
      *          MAIN PROGRAM MODULE
      ********************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       MAIN-MODULE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PERUSE" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO SYS-MONTH.
           MOVE ALP-DA TO SYS-DAY.

           MOVE "N" TO SKIP-READ-NEXT-WK.

      * FIX ALL WORKING STORAGE COMMANDS TO BE IN CORRECT FORMAT
           CALL "C$TOLOWER" USING LS-TMP, VALUE 15.
           CALL "C$TOLOWER" USING M-TMP, VALUE 16.
           CALL "C$TOLOWER" USING CAT-COMMAND, VALUE 69.
           CALL "C$TOLOWER" USING Q-TMP, VALUE 9.
           CALL "C$TOLOWER" USING CMD-PRU, VALUE 8.
           CALL "C$TOLOWER" USING CMD-PRUOFF, VALUE 8.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              CALL "C$TOLOWER" USING MONTH-LOWER(SUB), VALUE 2
           END-PERFORM.

      *-----------------------------------------------------------------
      *    CHANGED TO SET DEFAULT USERID TO ENV: 'LOGNAME'
      *    SINCE ALTERNATE LOGIN SETS       ENV: 'POSTNAME'
      *    TO THE NEW BRANCH YOUR POSTING TO. BUT ALL REPORTS
      *    CREATED UNDER THE ALTERNATE ENV: 'POSTNAME' ARE
      *    CREATED BY THE OPERATING SYSTEM WITH YOU SIGNIN ID 'LOGNAME'
      *    (EXCEP REGACC, THEY USE BBT LOGINS, SO GET ACTUAL USER NAME)
      *-----------------------------------------------------------------
      *    IF GP-I-AM-REGACC
      *       MOVE "USER" TO GETENV-BUF
      *    ELSE
              MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-GOOD
              MOVE GETENV-BUF TO USERID.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LS-TEMPFILE.

           DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                      CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS PERUSE-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       START-CHECK-DATE.
           MOVE MONTH-RAY(SYS-MONTH) TO SEL-MONTH.
           MOVE SYS-DAY TO SEL-DAY.
           MOVE SEL-MSG1 TO MESS.
           PERFORM SEND-LEGEND.
       GET-CHECK-DATE.
           MOVE "S NA030FLD." TO VDU.
           MOVE "Y" TO VDUBUF.
           PERFORM SCREENIO.

      * REPORTS CREATED ON ????
           MOVE "E NA030FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO GET-CHECK-DATE.

      *    NOTE: FIELD IS RIGHT JUSTIFING ENTRIES
           MOVE SPACES TO CHECK-DATE-FLAG.
           IF VDUBUF = SPACES
              MOVE "Y" TO CHECK-DATE-FLAG
              MOVE "S  A030FLD." TO VDU
              MOVE "  Y" TO VDUBUF
              PERFORM SCREENIO.
           IF VDUBUF = "  Y" OR VDUBUF = "Y"
              MOVE "Y" TO CHECK-DATE-FLAG.
           IF VDUBUF = "  N" OR VDUBUF = "N"
              MOVE "N" TO CHECK-DATE-FLAG.
           IF CHECK-DATE-FLAG = SPACES
              GO TO GET-CHECK-DATE.

       START-CHECK-USER.
           MOVE USERID TO SEL-USER.
           MOVE SEL-MSG2 TO MESS.
           PERFORM SEND-LEGEND.
       GET-CHECK-USER.
           MOVE "S NA030FLD." TO VDU.
           MOVE "Y" TO VDUBUF.
           PERFORM SCREENIO.

      * REPORTS CREATED BY ????
           MOVE "E NA030FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF UP-KEY
              GO TO START-CHECK-DATE.
           IF NOT ENTER-KEY
              GO TO GET-CHECK-USER.

      *    NOTE: FIELD IS RIGHT JUSTIFING ENTRIES
           MOVE SPACES TO CHECK-USER-FLAG.
           IF VDUBUF = SPACES
              MOVE "Y" TO CHECK-USER-FLAG
              MOVE "S  A030FLD." TO VDU
              MOVE "  Y" TO VDUBUF
              PERFORM SCREENIO.
           IF VDUBUF = "  Y" OR VDUBUF = "Y"
              MOVE "Y" TO CHECK-USER-FLAG.
           IF VDUBUF = "  N" OR VDUBUF = "N"
              MOVE "N" TO CHECK-USER-FLAG.
           IF CHECK-USER-FLAG = SPACES
              GO TO GET-CHECK-USER.
           IF CHECK-USER-FLAG = "Y"
              GO TO DISPLAY-CURRENT-LIST.

           MOVE " " TO PASSWDW-STAT.
           MOVE "VIEW" TO PASSWDW-PGM.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO VIEW ALL REPORTS:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
              "YOU ARE NOT AUTHORIZED FOR GLOBAL REPORT VIEWING"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO GET-CHECK-USER.

       DISPLAY-CURRENT-LIST.
           PERFORM DISPLAY-BUILD.

           PERFORM OPEN-WK-FILE-INPUT.
           MOVE SPACES TO WK-REC.
       RETURN-AND-DISPLAY.
           PERFORM DISPLAY-SCROLL.

       REC-RPTNO.
           IF QUEUE-FLAG = "N" MOVE
           "^, F7-EXIT, F6-PRINT, RTN-SCROLL, ENTER REPORT # TO VIEW:"
              TO MESS
           ELSE MOVE
           "^, F7-EXIT, F6-VIEW, RTN-SCROLL, ENTER REPORT # TO PRINT:"
              TO MESS.
           PERFORM SEND-LEGEND.
           IF PRINTER-SELECTED = "Y"
                        OR QUEUE-FLAG = "N"
              GO TO GET-RPTNO.
       SETPR.
           MOVE "F7-RETURN TO VIEW REPORTS, ENTER PRINTER #:"
              TO MESS.
           PERFORM SEND-LEGEND.
      *    MOVE "P" TO STAT.
      *    MOVE S TO STAT TO LIMIT QUEUE OPTIONS TO 'Y' AND 'N'.
           MOVE "S" TO STAT.
           MOVE PERUSE-QUEUE-POS TO Q-POS.
           MOVE 000000           TO C-POS.
           MOVE "1"              TO C-BUF.
           MOVE PERUSE-PRU-POS   TO P-POS.
           MOVE SPACES           TO Q-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT = "1U" OR STAT = "10"
              MOVE "N" TO QUEUE-FLAG
              GO TO REC-RPTNO.
           IF STAT = "1>"
              GO TO END-PROGRAM.

           MOVE PRU-PATH TO SAVE-PRU-PATH.
           MOVE "Y" TO PRINTER-SELECTED.
           MOVE
           "^, F7-EXIT, F6-VIEW, RTN-SCROLL, ENTER REPORT # TO PRINT:"
              TO MESS.
           PERFORM SEND-LEGEND.
       GET-RPTNO.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *    CALL FRESET USING STAT FLD-LIST.
           MOVE "E  A030FLD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF F6-KEY
              IF QUEUE-FLAG = "N"
                 MOVE "Y" TO QUEUE-FLAG
                 GO TO REC-RPTNO
              ELSE
                 MOVE "N" TO QUEUE-FLAG
                 GO TO REC-RPTNO.
           IF UP-KEY
              GO TO START-CHECK-USER.
           IF NOT ENTER-KEY
              GO TO GET-RPTNO.
           IF VDUBUF = SPACES
              PERFORM DISPLAY-SCROLL
              GO TO GET-RPTNO.
           MOVE VDUBUF TO REPORT-NO-X.
           INSPECT REPORT-NO-X REPLACING LEADING SPACES BY ZEROES.
           IF REPORT-NO-X IS NOT NUMERIC
              GO TO GET-RPTNO.
           IF REPORT-NO < 1  OR REPORT-NO > 30
              GO TO GET-RPTNO.

           IF D-REPORT(REPORT-NO) = SPACES
              GO TO GET-RPTNO.

           IF QUEUE-FLAG = "N"
              GO TO BEGIN-PERUSE.

      *    TEST FOR PIGGYBACK PRINT OPTION
           IF PRU-LP NOT = "   "
              GO TO BEGIN-PIGGY.

           MOVE D-REPORT(REPORT-NO) TO Q-PATH-REPORT.
           MOVE SAVE-PRU-PATH TO PRU-PATH.
           MOVE PERUSE-QUEUE-PATH TO PRU-FILE.
           PERFORM PRU-OUT.
           GO TO REC-RPTNO.

       BEGIN-PIGGY.
           MOVE "      REPORT PRINT IN PROGRESS:" TO MESS.
           PERFORM SEND-LEGEND.

      *    PERFORM PRU-PREP.

      * SET TRANSPARENT MODE BY SWITCHING TO AUXILIARY MODE,
      * ENSURE THAT DC1-DC3 CONTROL IS ENABLED:
      *    MOVE CMD-PRU TO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.
      *    PERFORM SUSPEND 2 TIMES.

      * SEPARATING THESE COMMANDS WILL NOT WORK WITH ANZIO
      * PERUSE.CMD   TPUT MC5;CAT /USR/TMP/(CAT-REPORT)

           MOVE D-REPORT(REPORT-NO) TO CAT-REPORT.
           MOVE CAT-COMMAND TO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.

           PERFORM PRU-RESET.
      * RESET TERMINAL MODE BY UNSWITCHING AUXILIARY MODE:
           PERFORM SUSPEND 2 TIMES.
      *    MOVE CMD-PRUOFF TO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.
      *    PERFORM SUSPEND 2 TIMES.

           MOVE
           "^, F7-EXIT, F6-VIEW, RTN-SCROLL, ENTER REPORT # TO PRINT:"
              TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "Y" TO LCK-FG.

           GO TO REC-RPTNO.

       BEGIN-PERUSE.

      * OPEN A DUMMY BLANK WINDOW TO DISPLAY THE REPORT IN
           DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS PERUSE2-WINDOW-HANDLE.

           MOVE D-REPORT(REPORT-NO) TO M-REPORT.
           MOVE M-COMMAND TO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

           DESTROY PERUSE2-WINDOW-HANDLE.

           GO TO RETURN-AND-DISPLAY.

      **************************************
       DISPLAY-BUILD SECTION.
      **************************************

           MOVE "SCANNING DIRECTORY..." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE MKTEMP-BUF TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.

           MOVE LS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           PERFORM CLEAR-LEGEND.
       EXIT-DISPLAY-BUILD.
           EXIT.

      ******************************************************************
       DISPLAY-SCROLL SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1      TO SUB.

       DISPLAY-NEXT-LSEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MUST MOVE SPACES TO WK-REPORT; PROGRAM NAMES THAT ARE LESS
      *     THAN 6 CHARACTERS (FOR EXAMPLE, LIBAT) WILL SHOW PART OF THE
      *     PREVIOUS NAME.  FOR EXAMPLE:
      *
      *           REPORT NAME             DISPLAYED AS
      *           ------------            ------------
      *           GNEXTRA002CA            GNEXTRA002CA
      *           GNEXTRA002FX            GNEXTRA002FX
      *           LIBATA005JC             LIBATA005JCX
      *           LIBATA005E7             LIBATA005E7X
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( SKIP-READ-NEXT-WK = "Y" )
              MOVE "N" TO SKIP-READ-NEXT-WK
              MOVE ZEROES TO IO-FG
           ELSE
              MOVE SPACES TO WK-REC
              PERFORM READ-WK-FILE-NEXT.

           IF ( IO-BAD )
              PERFORM CLOSE-WK-FILE
              PERFORM OPEN-WK-FILE-INPUT
              MOVE "** END **" TO D-END-MORE
              GO TO DISPLAY-SCROLL-END.

           MOVE SPACES TO WS-WK-REC.
           UNSTRING WK-REC DELIMITED BY ALL " " INTO
                    WK-READ-WRITE
                    WK-LINKS
                    WK-USERID
                    WK-GROUP
                    WK-SIZE
                    WK-MONTH
                    WK-DAY
                    WK-TIME
                    WK-DIR-REPORT.

           IF ( CHECK-DATE-FLAG = "Y" )
              MOVE WK-DAY TO WORK-DAYX
              INSPECT WORK-DAYX REPLACING LEADING SPACES BY ZEROES
              IF WORK-DAY2 = " "
                 MOVE WORK-DAY1 TO WORK-DAY2
                 MOVE 0 TO WORK-DAY1
              END-IF
              IF ( WK-MONTH NOT = MONTH-RAY(SYS-MONTH) ) OR
                 ( WORK-DAY NOT = SYS-DAY              )
                 GO TO DISPLAY-NEXT-LSEC.

           IF ( CHECK-USER-FLAG = "Y"  ) AND
              ( WK-USERID NOT = USERID )
              GO TO DISPLAY-NEXT-LSEC.

           IF ( SUB > 30 )
              MOVE "Y" TO SKIP-READ-NEXT-WK
              MOVE "MORE..." TO D-END-MORE
              GO TO DISPLAY-SCROLL-END.

           MOVE SUB       TO D-NO(SUB).
           MOVE "."       TO D-DOT(SUB).
           MOVE WK-USERID TO D-USERID(SUB).
           MOVE WK-MONTH  TO D-MONTH(SUB).
           MOVE WK-DAY    TO D-DAY(SUB).
           MOVE WK-TIME   TO D-TIME(SUB).
           MOVE WK-REPORT TO D-REPORT(SUB).

           ADD 1 TO SUB.

           GO TO DISPLAY-NEXT-LSEC.

       DISPLAY-SCROLL-END.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-SCROLL-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************************
       FORM-RESET SECTION.
      ************************************************************
           MOVE EXT-CONAME-CONAME TO PERUSE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PERUSE-SYSDATE.
           DISPLAY PERUSE-SCREEN UPON PERUSE-WINDOW-HANDLE.
           MOVE PERUSE-FORM-FIELDS TO WR-BUF.
           MOVE PERUSE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************************
           MOVE PERUSE-HELP--FILE TO HELP-LINK-FILE.
           MOVE PERUSE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/PERUSE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBWKLSIO.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-PROGRAM SECTION.
      *********************************************
      *  NO PRINTER LOCKS ARE TAKING PLACE SO WE REALLY
      *  DO NOT NEED TO PERFORM PRU-OUT.
      *  (NO LOCKS BECAUSE WE ONLY ALLOW DIRECT TO PIGGY)
      *    IF LCK-FG = "Y"
      *       PERFORM PRU-OUT.

           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PERUSE-SCREEN.
           DESTROY PERUSE-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
