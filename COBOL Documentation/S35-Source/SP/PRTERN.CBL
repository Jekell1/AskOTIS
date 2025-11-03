       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PRTERN.
       AUTHOR.             MJD.
       DATE-WRITTEN.  01/20/98.
      ******************************************************************
      *  DIR : GB
      *  DESC: PERUSE PRINTED EARNING REPORTS
      *
      *   NOTE:   THIS PROGRAM CALLS /$REL/SH/PRTERN.SH
      *                              /$REL/SH/PRTER2.SH
      *
      * REV:
      *  MJD 990215 FIXED VIEW REPORTS. CREATED TMP FILE IN /USR/TMP TO
      *             HOLD LIST OF REPORTS TO PERUSE IF VIEWING REPORTS.
      *             REMOVED QUEUE OPTION FROM SCREEN.  ALLOW OPTION
      *             TO PRINT (LINE), VIEW, OR PIGGYBACK REPORTS.
      *  MJD 990225 FIXED BUG, SKIPPING AC2* REPORTS.
      *  MJD 990625 ADDED OPTION TO PRINT INDIVIDUAL REPORTS AS WELL AS
      *             PRINTING ALL REPORTS.
      *  BAH 040902 F3-ALL OPTIONS WERE NOT PRINTING...NEEDED TO CLEAR
      *             -ALL-FILENAME FIRST...
      *             F1 AND UP ARROW WERE NOT RESETTING FIELDS CORRECTLY,
      *             STUFF WOULD PRINT TO SCREEN FOR PIGGYBACK, ETC..
      *   CS 120614 REPLACE LACCESS/LACESSW WITH ACCESS/ACCESSW,USES C$FILEINFO
      *             INCREASED ACCESS-BUF TO 127 VERYANT
      *  20171013 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  2019.0508 BAH PIGGYBACK PRINTING NOT WORKING, CAT-CMD NEEDED TO
      *                BE MOVED TO SYSTEM-BUF PRIOR TO MOVING CAT-FILE OR
      *                IT WIPED OUT THE FILE TO CAT! QA0021
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/PRTERN.CBL S34 06/20/21-20:40:25 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".

      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  SAVE-PRU-PATH            PIC X(75) VALUE SPACES.
       01  SYS-MONTH                PIC 99.
       01  SYS-DAY                  PIC 99.

       01  ERRCD                    PIC X VALUE " ".
       01  SUB                      PIC 999   VALUE 0.
       78  WS-NA                    VALUE X'6E61'.

      *    **********************************
      *    *      W-SCREEN DISPLAY BUFFER
      *    **********************************
       01  MESS-LIST                PIC X(5)   VALUE "MESS.".
       01  DISPLAY-LIST             PIC X(56)  VALUE
           "RPT-TYPE RPT-BRNO RPT-BRDESC RPT-DATE RPT-ACTION ACCEPT.".

       01  REPORT-FIELDS.
           03  REPORT-TYPE          PIC X.
           03  REPORT-BRNO          PIC 9(4).
           03  REPORT-DATE          PIC 9(4).
           03  REPORT-DATEX      REDEFINES REPORT-DATE.
               05 REPORT-MO         PIC X(2).
               05 REPORT-YR         PIC X(2).
           03  REPORT-TO-PRINT      PIC 9(01).
               88 REPORT-TO-PRINT-VALID    VALUE 0 1 2 3 4.
           03  REPORT-ACTION        PIC X.
               88  REPORT-PIGGY     VALUE "I".
               88  REPORT-QUEUE     VALUE "P".
               88  REPORT-VIEW      VALUE "V".
               88  REPORT-TRANS     VALUE "T".

       01  ALL-BRANCHES-SELECTED    PIC X  VALUE "N".

      *-----------------------------------------------------------------
      *    /USR/DATA/R#/*/ED/XXX####XXXXXXXXXX
      *-----------------------------------------------------------------
       01  DETAIL-ALL.
           03  DETAIL-ALL-BASE          PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  DETAIL-ALL-MACHINE       PIC X(02).
           03  FILLER                   PIC X(06)   VALUE "/*/ED/".
           03  DETAIL-ALL-FILENAME.
               05  DETAIL-ALL-NAME      PIC X(3).
               05  DETAIL-ALL-DATE      PIC 9(4).
               05  DETAIL-ALL-FILE      PIC X(10).
               05  FILLER REDEFINES DETAIL-ALL-FILE.
                   07  DETAIL-ALL-STAR  PIC X(01).
                   07  FILLER           PIC X(09).

      *-----------------------------------------------------------------
      *    /USR/DATA/R#/*####/ED/XXX####XXXXXXXXXX
      *-----------------------------------------------------------------
       01  DETAIL-LOCAL.
           03  DETAIL-LOC-BASE          PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  DETAIL-LOC-MACHINE       PIC X(02).
           03  FILLER                   PIC X(02)   VALUE "/*".
           03  DETAIL-LOC-BRNO          PIC 9(04).
           03  FILLER                   PIC X(04)   VALUE "/ED/".
           03  DETAIL-LOC-FILENAME.
               05  DETAIL-LOC-NAME      PIC X(03).
               05  DETAIL-LOC-DATE      PIC 9(04).
               05  DETAIL-LOC-FILE      PIC X(10).
               05  FILLER REDEFINES DETAIL-LOC-FILE.
                   07  DETAIL-LOC-STAR  PIC X(01).
                   07  FILLER           PIC X(09).

      *-----------------------------------------------------------------
      *    /USR/DATA/R#/ED/XXX####XXXXXXXXXX
      *-----------------------------------------------------------------
       01  SUMMARY-ALL.
           03  SUMMARY-ALL-BASE          PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  SUMMARY-ALL-MACHINE       PIC X(02).
           03  FILLER                    PIC X(04)   VALUE "/ED/".
           03  SUMMARY-ALL-FILENAME.
               05  SUMMARY-ALL-NAME     PIC X(3).
               05  SUMMARY-ALL-DATE     PIC 9(4).
               05  SUMMARY-ALL-FILE     PIC X(10).
               05  FILLER REDEFINES SUMMARY-ALL-FILE.
                   07  SUMMARY-ALL-STAR PIC X(01).
                   07  FILLER           PIC X(09).

       01  SEL-TYPE-MSG1            PIC X(40)  VALUE
           "F7-EXIT, 'D' - DETAIL, 'S' - SUMMARY".
       01  SEL-BRNO-MSG1            PIC X(40)  VALUE
           "F7-EXIT, ENTER BRANCH # OR F3-ALL".
       01  SEL-NAME-MSG1            PIC X(40)  VALUE
           "F7-EXIT, ENTER REPORT # OR F3-ALL".
       01  SEL-DATE-MSG1            PIC X(40)  VALUE
           "F7-EXIT, ENTER REPORT DATE (MMYY)".
       01  SEL-ACTION-MSG1          PIC X(40)  VALUE
           "F7-EXIT, 'P'RINT, 'V'IEW, OR 'T'RANSFER".

      *-----------------------------------------------------------------
      * PATH OF SPECIFIC FILE TO PRINT
      *-----------------------------------------------------------------
       01  REPORT-TO-PRINT-PATH.
           03  FILLER                     PIC X(11).
           03  REPORT-TO-PRINT-PATH-STAR  PIC X(6).

      *-----------------------------------------------------------------
      * SCREEN LIST OF PROMPTS TO DISPLAY
      *-----------------------------------------------------------------
       01  RPT-LIST.
           03  FILLER            PIC X(22) VALUE
               "RPTLIST1:D RPTLIST2:D.".

      *-----------------------------------------------------------------
      * FILENAMES FOR 'D' DETAIL FILES.
      *-----------------------------------------------------------------
       01  LOC-RPT-TABLEX.
           03  FILLER            PIC X(17)  VALUE "ACR0000D.UNEARNED".
           03  FILLER            PIC X(17)  VALUE "ACR0000R.IBACCRUE".
           03  FILLER            PIC X(17)  VALUE "AC20000D.NONACCRU".
           03  FILLER            PIC X(17)  VALUE "AC20000R.PLUNEARN".
       01  LOC-RPT-TBL  REDEFINES LOC-RPT-TABLEX.
           03  LOC-RPT  OCCURS 4.
               05  FILLER        PIC X(3).
               05  LOC-RPT-DT    PIC 9(4).
               05  FILLER        PIC X(10).

      *-----------------------------------------------------------------
      * FILENAMES FOR 'S" SUMMARY FILES.
      *-----------------------------------------------------------------
       01  GLB-RPT-TABLEX.
           03  FILLER            PIC X(17)  VALUE "ACR0000S.UNEARNED".
           03  FILLER            PIC X(17)  VALUE "ACR0000T.IBACCRUE".
           03  FILLER            PIC X(17)  VALUE "AC20000S.NONACCRU".
           03  FILLER            PIC X(17)  VALUE "AC20000T.PLUNEARN".
       01  GLB-RPT-TBL  REDEFINES GLB-RPT-TABLEX.
           03  GLB-RPT  OCCURS 4.
               05  FILLER        PIC X(3).
               05  GLB-RPT-DT    PIC 9(4).
               05  FILLER        PIC X(10).

      *-----------------------------------------------------------------
      * DISPLAY PROMPTS FOR REPORTS.
      *-----------------------------------------------------------------
       01  DISP-RPT.
           05  FILLER        PIC X(20)  VALUE "1.                  ".
           05  FILLER        PIC X(02).
           05  FILLER        PIC X(20)  VALUE "2.                  ".
           05  FILLER        PIC X(02).
           05  FILLER        PIC X(20)  VALUE "3.                  ".
           05  FILLER        PIC X(02).
           05  FILLER        PIC X(20)  VALUE "4.                  ".
           05  FILLER        PIC X(02).
       01  DISP-NAMES REDEFINES DISP-RPT.
           03  DISP-PATHX        OCCURS 4.
               05  FILLER            PIC X(03).
               05  DISP-PATH         PIC X(17).
               05  FILLER            PIC X(02).

      *-----------------------------------------------------------------
      * FILE FOUND FLAG 'Y'-REPORT FOUND / 'N'-REPORT NOT FOUND
      *-----------------------------------------------------------------
       01  FILE-FOUNDX.
           03 FILE-FOUND                PIC X(01)  OCCURS 4.
       01  FILLER REDEFINES FILE-FOUNDX PIC X(4).
           88 NO-FILES-FOUND                       VALUE "NNNN".


       COPY "LIBGB/PASSWDW.CPY".
       01  PERUSE-QUEUE-PATH.
           03  Q-PATH-REPORT     PIC X(53).

       01  M-REPORT              PIC X(53).
      *     "/$REL/SH/EC /$REL/SH/PRTERN.SH ".
       01  P-COMMAND.
           03  FILLER            PIC X    VALUE "/".
           03  P-REL             PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  P-EC              PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  P-REL2            PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  P-SHELL           PIC X(10) VALUE "PRTERN.SH ".
           03  P-REPORT          PIC X(35).

      *   "/$REL/SH/EC /$REL/SH/PRTER2.SH ".
       01  T-COMMAND.
           03  FILLER            PIC X    VALUE "/".
           03  T-REL             PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  T-EC              PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  T-REL2            PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  T-SHELL           PIC X(10) VALUE "PRTER2.SH ".
           03  T-REPORT          PIC X(35).

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       01  PRTERN-WINDOW-HANDLE PIC X(10).
       01  PRTERN2-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PRTERN_DEF.CPY".
       COPY "LIBSP/PRTERN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/PRTERN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PRTERN" TO VDU-FORM-NAME.
       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS PRTERN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO SYS-MONTH.
           MOVE ALP-DA TO SYS-DAY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-FILPATH-BASE           TO DETAIL-ALL-BASE
                                              DETAIL-LOC-BASE
                                              SUMMARY-ALL-BASE.

           MOVE EXT-FILPATH-MACHINE        TO DETAIL-ALL-MACHINE
                                              DETAIL-LOC-MACHINE
                                              SUMMARY-ALL-MACHINE.


           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO P-REL P-REL2
                              T-REL T-REL2.
           CALL "C$TOLOWER" USING P-EC, VALUE 3.
           CALL "C$TOLOWER" USING T-EC, VALUE 3.
           CALL "C$TOLOWER" USING P-SHELL, VALUE 10.
           CALL "C$TOLOWER" USING T-SHELL, VALUE 10.

       START-REPORT-CLEAR.
           MOVE SPACES TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "N" TO ALL-BRANCHES-SELECTED.
           MOVE SPACES TO WR-BUF.
           MOVE RPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       START-REPORT-TYPE.
           MOVE SEL-TYPE-MSG1 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010RPT-TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO START-REPORT-CLEAR.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO START-REPORT-TYPE.

           MOVE VDUBUF TO REPORT-TYPE.
           IF NOT (REPORT-TYPE = "D" OR "S")
              MOVE "ENTER 'D' FOR DETAIL OR 'S' FOR SUMMARY" TO MESS
              PERFORM SEND-MESS
              GO TO START-REPORT-TYPE.
           IF REPORT-TYPE = "S"
              MOVE 0 TO PASSWDW-SEQ
              MOVE "TO VIEW SUMMARY REPORTS:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE
                 "YOU ARE NOT AUTHORIZED FOR GLOBAL REPORT VIEWING"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO START-REPORT-TYPE
              END-IF
              MOVE "S  A040RPT-BRNO." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A016RPT-BRDESC." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO START-REPORT-DATE.

       START-REPORT-BRNO.
           MOVE SEL-BRNO-MSG1 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040RPT-BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO START-REPORT-TYPE.
           IF VDUSTATUS = "16" GO TO START-ALL-BRANCHES.
           IF VDUSTATUS = "10" GO TO START-REPORT-CLEAR.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO START-REPORT-BRNO.
           MOVE VDUNUM0 TO REPORT-BRNO BR-NO.

           IF EXT-FILPATH-BRANCH NOT = REPORT-BRNO
              MOVE 1 TO PASSWDW-SEQ
              MOVE "TO VIEW ALL REPORTS:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE
                 "YOU ARE NOT AUTHORIZED FOR GLOBAL REPORT VIEWING"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO START-REPORT-BRNO.

           PERFORM GET-BR.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO START-REPORT-BRNO.

           MOVE "S  A016RPT-BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO ALL-BRANCHES-SELECTED.
           GO TO START-REPORT-DATE.
       START-ALL-BRANCHES.
           MOVE 1 TO PASSWDW-SEQ.
           MOVE "TO VIEW ALL REPORTS:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
              "YOU ARE NOT AUTHORIZED FOR GLOBAL REPORT VIEWING"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO START-REPORT-BRNO.

           MOVE "Y" TO ALL-BRANCHES-SELECTED.
           MOVE "S  A040RPT-BRNO." TO VDU.
           MOVE "****" TO VDUBUF
           PERFORM SCREENIO.
           MOVE "S  A016RPT-BRDESC." TO VDU.
           MOVE "ALL BRANCHES" TO VDUBUF.
           PERFORM SCREENIO.

       START-REPORT-DATE.
           MOVE SEL-DATE-MSG1 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040RPT-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              IF REPORT-TYPE = "S"
                 GO TO START-REPORT-TYPE
              ELSE
                 GO TO START-REPORT-BRNO.
           IF VDUSTATUS = "10" GO TO START-REPORT-CLEAR.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO START-REPORT-DATE.
           MOVE VDUNUM0 TO REPORT-DATE.
           MOVE REPORT-MO TO DATE-MMDDYY-MM.
           MOVE 01        TO DATE-MMDDYY-DD.
           MOVE REPORT-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              MOVE "S  A040RPT-DATE." TO VDU
              MOVE "____" TO VDUBUF
              PERFORM SCREENIO
              GO TO START-REPORT-DATE.

       START-REPORT-NAME.
           PERFORM SETUP-PROMPT.
           MOVE SEL-NAME-MSG1 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN010RPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO START-REPORT-DATE.
           IF VDUSTATUS = "10" GO TO START-REPORT-CLEAR.
           IF VDUSTATUS = "16"
              MOVE "SNNN010RPT." TO VDU
              MOVE 0 TO VDUNUM0
              PERFORM SCREENIO
              MOVE 0 TO VDUNUM0
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO START-REPORT-NAME.
           MOVE VDUNUM0 TO REPORT-TO-PRINT.
           IF NOT REPORT-TO-PRINT-VALID
              GO TO START-REPORT-NAME.

      *    -------------------------------------------------------------
      *    IF ALL FILES SELECTED BUT NON EXIST THEN DON'T CONTINUE
      *    -------------------------------------------------------------
           IF REPORT-TO-PRINT = 0
              IF NO-FILES-FOUND
                 MOVE " THERE ARE NO FILES TO SELECT" TO MESS
                 PERFORM SEND-MESS
                 GO TO START-REPORT-NAME.

      *    -------------------------------------------------------------
      *    IF SINGLE FILE SELECTED BUT NON EXIST THEN DON'T CONTINUE
      *    ELSE SETUP REPORT PATH.
      *    -------------------------------------------------------------
           IF REPORT-TO-PRINT NOT = 0
              IF FILE-FOUND(REPORT-TO-PRINT) NOT = "Y"
                 MOVE "FILE IS NOT AVAILABLE" TO MESS
                 PERFORM SEND-MESS
                 GO TO START-REPORT-NAME
              END-IF
              IF REPORT-TYPE = "S"
                 MOVE GLB-RPT(REPORT-TO-PRINT) TO REPORT-TO-PRINT-PATH
              ELSE
                 MOVE LOC-RPT(REPORT-TO-PRINT) TO REPORT-TO-PRINT-PATH
              END-IF
              MOVE "*" TO REPORT-TO-PRINT-PATH-STAR.

       START-REPORT-ACTION.
           MOVE SEL-ACTION-MSG1 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RPT-ACTION." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO START-REPORT-NAME.
           IF VDUSTATUS = "10" GO TO START-REPORT-CLEAR.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO START-REPORT-ACTION.
           MOVE VDUBUF TO REPORT-ACTION.
      D    STOP MESS.
           IF NOT (REPORT-ACTION = "P" OR "V" OR "I" OR "T")
              MOVE "ENTER 'P'RINT 'V'IEW, 'I'-PIGGYBACK, OR 'T'RANSFER"
                   TO MESS
              PERFORM SEND-MESS
              GO TO START-REPORT-ACTION.
           IF REPORT-TRANS
              PERFORM GET-GB
              IF IO-FG NOT = 0 OR GB-UUCP-HOME = SPACES
                 MOVE "UNKNOWN UUCP MACHINE NAME" TO MESS
                 PERFORM SEND-MESS
                 GO TO START-REPORT-ACTION.

       REC-RPTNO.
           IF REPORT-VIEW
              MOVE "PREPARING REPORT FOR VIEW"  TO MESS
           ELSE
              IF REPORT-TRANS
                 MOVE "PREPARING REPORT FOR TRANSFER"  TO MESS
              ELSE
                 MOVE "PREPARING REPORT FOR PRINTING" TO MESS.
           PERFORM SEND-LEGEND.
           IF REPORT-VIEW OR REPORT-TRANS
              GO TO START-REPORT-ACCEPT.

       SETPR.
           MOVE "F7-RETURN TO VIEW REPORTS, ENTER PRINTER #:"
              TO MESS.
           PERFORM SEND-LEGEND.

      *    FORCE QUEUE FIELD. DO NOT ALLOW USER ENTRY.
           IF REPORT-PIGGY MOVE "P" TO STAT.
           IF REPORT-QUEUE MOVE "Y" TO STAT.
           IF REPORT-VIEW  MOVE " " TO STAT.
           IF REPORT-TRANS MOVE " " TO STAT.
           MOVE 000000 TO Q-POS C-POS.
           MOVE PRTERN-PRU-POS TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE "01" TO C-BUF.
           PERFORM PRU-IN.
           IF STAT = "1U" OR STAT = "10"
              MOVE "V" TO REPORT-ACTION
              GO TO START-REPORT-ACCEPT.
           IF STAT = "1>"
              GO TO END-PROGRAM.
           MOVE PRU-PATH TO SAVE-PRU-PATH.

       START-REPORT-ACCEPT.
           PERFORM CLEAR-LEGEND.
           MOVE "E RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO START-REPORT-ACTION.
           IF VDUSTATUS = "10" GO TO START-REPORT-CLEAR.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO START-REPORT-ACCEPT.
           IF VDUBUF NOT = "Y"
              GO TO START-REPORT-ACCEPT.

       GET-RPTNO.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE FILE LIST" TO MESS
              PERFORM SEND-MESS
              GO TO START-REPORT-ACCEPT.

           IF REPORT-ACTION = "V"
              GO TO BEGIN-PERUSE.

           IF REPORT-ACTION = "T"
              GO TO BEGIN-TRANSFER.

           IF REPORT-ACTION = "I"
              GO TO BEGIN-PIGGY.

      * PRINT NOW
           MOVE "      REPORT PRINT IN PROGRESS:" TO MESS.
           PERFORM SEND-LEGEND.
           IF REPORT-TYPE = "D"
              IF ALL-BRANCHES-SELECTED = "Y"
                 EVALUATE REPORT-TO-PRINT
                    WHEN 0
                       MOVE SPACES TO DETAIL-ALL-FILENAME
                       MOVE "AC2" TO DETAIL-ALL-NAME
                       MOVE REPORT-DATE TO DETAIL-ALL-DATE
                       MOVE "*" TO DETAIL-ALL-STAR
                       MOVE DETAIL-ALL TO Q-PATH-REPORT
                       MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                       PERFORM PRU-OUT
                       MOVE "ACR" TO DETAIL-ALL-NAME
                       MOVE REPORT-DATE TO DETAIL-ALL-DATE
                       MOVE "*" TO DETAIL-ALL-STAR
                       MOVE DETAIL-ALL TO Q-PATH-REPORT
                       MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                       PERFORM PRU-OUT
                    WHEN OTHER
                       MOVE REPORT-TO-PRINT-PATH TO DETAIL-ALL-FILENAME
                       MOVE DETAIL-ALL TO Q-PATH-REPORT
                       MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                       PERFORM PRU-OUT
                 END-EVALUATE
              ELSE
                 EVALUATE REPORT-TO-PRINT
                    WHEN 0
                       MOVE SPACES TO DETAIL-LOC-FILENAME
                       MOVE "AC2" TO DETAIL-LOC-NAME
                       MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                       MOVE REPORT-DATE TO DETAIL-LOC-DATE
                       MOVE "*" TO DETAIL-LOC-STAR
                       MOVE DETAIL-LOCAL TO Q-PATH-REPORT
                       MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                       PERFORM PRU-OUT
                       MOVE "ACR" TO DETAIL-LOC-NAME
                       MOVE DETAIL-LOCAL TO Q-PATH-REPORT
                       MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                       PERFORM PRU-OUT
                   WHEN OTHER
                       MOVE REPORT-TO-PRINT-PATH TO DETAIL-LOC-FILENAME
                       MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                       MOVE DETAIL-LOCAL TO Q-PATH-REPORT
                       MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                       PERFORM PRU-OUT
                 END-EVALUATE.


           IF REPORT-TYPE = "S"
              EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO SUMMARY-ALL-FILENAME
                    MOVE "AC2" TO SUMMARY-ALL-NAME
                    MOVE REPORT-DATE TO SUMMARY-ALL-DATE
                    MOVE "*" TO SUMMARY-ALL-STAR
                    MOVE SUMMARY-ALL TO Q-PATH-REPORT
                    MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                    PERFORM PRU-OUT
                    MOVE "ACR" TO SUMMARY-ALL-NAME
                    MOVE SUMMARY-ALL TO Q-PATH-REPORT
                    MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                    PERFORM PRU-OUT
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO SUMMARY-ALL-FILENAME
                    MOVE SUMMARY-ALL TO Q-PATH-REPORT
                    MOVE PERUSE-QUEUE-PATH TO PRU-FILE
                    PERFORM PRU-OUT
              END-EVALUATE.

           GO TO START-REPORT-ACCEPT.

       BEGIN-PIGGY.
           MOVE "      REPORT PRINT IN PROGRESS:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM PRU-PREP.

           IF REPORT-TYPE = "D"
              IF ALL-BRANCHES-SELECTED = "Y"
                 EVALUATE REPORT-TO-PRINT
                    WHEN 0
                       MOVE SPACES TO DETAIL-ALL-FILENAME
                       MOVE "AC2" TO DETAIL-ALL-NAME
                       MOVE REPORT-DATE TO DETAIL-ALL-DATE
                       MOVE "*" TO DETAIL-ALL-STAR
                       MOVE CAT-CMD    TO SYSTEM-BUF
                       MOVE DETAIL-ALL TO CAT-FILE
                       PERFORM SYSTEM-CALL
                       MOVE "ACR" TO DETAIL-ALL-NAME
                       MOVE CAT-CMD    TO SYSTEM-BUF
                       MOVE DETAIL-ALL TO CAT-FILE
                       PERFORM SYSTEM-CALL
                    WHEN OTHER
                       MOVE REPORT-TO-PRINT-PATH TO DETAIL-ALL-FILENAME
                       MOVE CAT-CMD    TO SYSTEM-BUF
                       MOVE DETAIL-ALL TO CAT-FILE
                       PERFORM SYSTEM-CALL
                 END-EVALUATE
              ELSE
                 EVALUATE REPORT-TO-PRINT
                    WHEN 0
                       MOVE SPACES TO DETAIL-LOC-FILENAME
                       MOVE "AC2" TO DETAIL-LOC-NAME
                       MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                       MOVE REPORT-DATE TO DETAIL-LOC-DATE
                       MOVE "*" TO DETAIL-LOC-STAR
                       MOVE CAT-CMD      TO SYSTEM-BUF
                       MOVE DETAIL-LOCAL TO CAT-FILE
                       PERFORM SYSTEM-CALL
                       MOVE "ACR" TO DETAIL-LOC-NAME
                       MOVE CAT-CMD      TO SYSTEM-BUF
                       MOVE DETAIL-LOCAL TO CAT-FILE
                       PERFORM SYSTEM-CALL
                    WHEN OTHER
                       MOVE REPORT-TO-PRINT-PATH TO DETAIL-LOC-FILENAME
                       MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                       MOVE CAT-CMD      TO SYSTEM-BUF
                       MOVE DETAIL-LOCAL TO CAT-FILE
                       PERFORM SYSTEM-CALL
                 END-EVALUATE.

           IF REPORT-TYPE = "S"
              EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO SUMMARY-ALL-FILENAME
                    MOVE "AC2" TO SUMMARY-ALL-NAME
                    MOVE REPORT-DATE TO SUMMARY-ALL-DATE
                    MOVE "*" TO SUMMARY-ALL-STAR
                    MOVE CAT-CMD     TO SYSTEM-BUF
                    MOVE SUMMARY-ALL TO CAT-FILE
                    PERFORM SYSTEM-CALL
                    MOVE "ACR" TO SUMMARY-ALL-NAME
                    MOVE CAT-CMD     TO SYSTEM-BUF
                    MOVE SUMMARY-ALL TO CAT-FILE
                    PERFORM SYSTEM-CALL
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO SUMMARY-ALL-FILENAME
                    MOVE CAT-CMD     TO SYSTEM-BUF
                    MOVE SUMMARY-ALL TO CAT-FILE
                    PERFORM SYSTEM-CALL
              END-EVALUATE.

           PERFORM PRU-RESET.
           GO TO START-REPORT-ACCEPT.

       BEGIN-PERUSE.
      *    PERFORM ENDWIN-CALL.

      * OPEN A DUMMY BLANK WINDOW TO DISPLAY THE REPORT IN
           DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                      HANDLE IS PRTERN2-WINDOW-HANDLE.


           IF REPORT-TYPE = "D"
              IF ALL-BRANCHES-SELECTED = "Y"
                 EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO DETAIL-ALL-FILENAME
                    MOVE "AC2" TO DETAIL-ALL-NAME
                    MOVE REPORT-DATE TO DETAIL-ALL-DATE
                    MOVE "*" TO DETAIL-ALL-STAR
                    MOVE DETAIL-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE "ACR" TO DETAIL-ALL-NAME
                    MOVE DETAIL-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO P-REPORT
                    MOVE P-COMMAND TO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO DETAIL-ALL-FILENAME
                    MOVE DETAIL-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO P-REPORT
                    MOVE P-COMMAND TO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 END-EVALUATE
              ELSE
                 EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO DETAIL-LOC-FILENAME
                    MOVE "AC2" TO DETAIL-LOC-NAME
                    MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                    MOVE REPORT-DATE TO DETAIL-LOC-DATE
                    MOVE "*" TO DETAIL-LOC-STAR
                    MOVE DETAIL-LOCAL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE "ACR" TO DETAIL-LOC-NAME
                    MOVE DETAIL-LOCAL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO P-REPORT
                    MOVE P-COMMAND TO SYSTEM-BUF
      *             PERFORM SYSTEM-CALL
                    CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO DETAIL-LOC-FILENAME
                    MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                    MOVE DETAIL-LOCAL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO P-REPORT
                    MOVE P-COMMAND TO SYSTEM-BUF
      *             PERFORM SYSTEM-CALL
                    CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99
                 END-EVALUATE.

           IF REPORT-TYPE = "S"
              EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO SUMMARY-ALL-FILENAME
                    MOVE "AC2" TO SUMMARY-ALL-NAME
                    MOVE REPORT-DATE TO SUMMARY-ALL-DATE
                    MOVE "*" TO SUMMARY-ALL-STAR
                    MOVE SUMMARY-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE "ACR" TO SUMMARY-ALL-NAME
                    MOVE SUMMARY-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO P-REPORT
                    MOVE P-COMMAND TO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO SUMMARY-ALL-FILENAME
                    MOVE SUMMARY-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO P-REPORT
                    MOVE P-COMMAND TO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
              END-EVALUATE.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           DESTROY PRTERN2-WINDOW-HANDLE.

           GO TO START-REPORT-ACCEPT.

       BEGIN-TRANSFER.
      *    PERFORM ENDWIN-CALL.

      * OPEN A DUMMY BLANK WINDOW TO DISPLAY THE REPORT IN
           DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                      HANDLE IS PRTERN2-WINDOW-HANDLE.

           IF REPORT-TYPE = "D"
              IF ALL-BRANCHES-SELECTED = "Y"
                 EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO DETAIL-ALL-FILENAME
                    MOVE "AC2" TO DETAIL-ALL-NAME
                    MOVE REPORT-DATE TO DETAIL-ALL-DATE
                    MOVE "*" TO DETAIL-ALL-STAR
                    MOVE DETAIL-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE "ACR" TO DETAIL-ALL-NAME
                    MOVE DETAIL-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO T-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING T-COMMAND " " GB-UUCP-HOME " " REPORT-TYPE
                           DELIMITED BY "  " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO DETAIL-ALL-FILENAME
                    MOVE DETAIL-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO T-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING T-COMMAND " " GB-UUCP-HOME " " REPORT-TYPE
                           DELIMITED BY "  " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 END-EVALUATE
              ELSE
                 EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO DETAIL-LOC-FILENAME
                    MOVE "AC2" TO DETAIL-LOC-NAME
                    MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                    MOVE REPORT-DATE TO DETAIL-LOC-DATE
                    MOVE "*" TO DETAIL-LOC-STAR
                    MOVE DETAIL-LOCAL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE "ACR" TO DETAIL-LOC-NAME
                    MOVE DETAIL-LOCAL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO T-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING T-COMMAND " " GB-UUCP-HOME " " REPORT-TYPE
                           DELIMITED BY "  " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO DETAIL-LOC-FILENAME
                    MOVE REPORT-BRNO TO DETAIL-LOC-BRNO
                    MOVE DETAIL-LOCAL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO T-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING T-COMMAND " " GB-UUCP-HOME " " REPORT-TYPE
                           DELIMITED BY "  " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 END-EVALUATE.

           IF REPORT-TYPE = "S"
              EVALUATE REPORT-TO-PRINT
                 WHEN 0
                    MOVE SPACES TO SUMMARY-ALL-FILENAME
                    MOVE "AC2" TO SUMMARY-ALL-NAME
                    MOVE REPORT-DATE TO SUMMARY-ALL-DATE
                    MOVE "*" TO SUMMARY-ALL-STAR
                    MOVE SUMMARY-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE "ACR" TO SUMMARY-ALL-NAME
                    MOVE SUMMARY-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO T-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING T-COMMAND " " GB-UUCP-HOME " " REPORT-TYPE
                           DELIMITED BY "  " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                 WHEN OTHER
                    MOVE REPORT-TO-PRINT-PATH TO SUMMARY-ALL-FILENAME
                    MOVE SUMMARY-ALL TO M-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING LS-CMD " -1 " M-REPORT " >>" MKTEMP-BUF
                        DELIMITED BY "   " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE MKTEMP-BUF TO T-REPORT
                    MOVE SPACES TO SYSTEM-BUF
                    STRING T-COMMAND " " GB-UUCP-HOME " " REPORT-TYPE
                           DELIMITED BY "  " INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
              END-EVALUATE.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           DESTROY PRTERN2-WINDOW-HANDLE.

           GO TO START-REPORT-ACCEPT.

      ************************************************
       VERIFY-PASSWORD SECTION.
      ************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "PRTERN" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
       SETUP-PROMPT SECTION.
      ******************************************************************

      * SUMMARY FILES IN GLOBAL ED.

           IF REPORT-TYPE = "S"
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                 MOVE REPORT-DATE TO GLB-RPT-DT(SUB)
                 MOVE GLB-RPT(SUB) TO SUMMARY-ALL-FILENAME
                 MOVE SPACES TO ACCESS-BUF
                 STRING "/" EXT-FILPATH-FIL "/../ED/" GLB-RPT(SUB)
                    DELIMITED BY " " INTO ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF STAT = "00"
                    MOVE "Y" TO FILE-FOUND(SUB)
                     MOVE  GLB-RPT(SUB) TO DISP-PATH(SUB)
                 ELSE
                    MOVE "N" TO FILE-FOUND(SUB)
                    MOVE WS-NA TO DISP-PATH(SUB)
                 END-IF
              END-PERFORM
              MOVE DISP-RPT TO WR-BUF
              MOVE RPT-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO SETUP-PROMPT-EXIT.

      * DETAIL FILES IN GLOBAL ED.

           IF REPORT-TYPE = "D"
              IF ALL-BRANCHES-SELECTED = "N"
      * PERFORM ACCESS CALLS IF A SINGLE BRANCH IS SELECTED
                 PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                    MOVE REPORT-DATE TO LOC-RPT-DT(SUB)
                    MOVE LOC-RPT(SUB) TO SUMMARY-ALL-FILENAME
                    MOVE SPACES TO ACCESS-BUF
                    STRING "/" EXT-FILPATH-FIL "/../../" BR-MACHINE "/"
                         BR-DATA-PATH "/ED/" LOC-RPT(SUB)
                         DELIMITED BY " " INTO ACCESS-BUF
                    PERFORM ACCESS-CALL
                    IF STAT = "00"
                       MOVE "Y" TO FILE-FOUND(SUB)
                       MOVE  LOC-RPT(SUB) TO DISP-PATH(SUB)
                    ELSE
                       MOVE "N" TO FILE-FOUND(SUB)
                       MOVE WS-NA TO DISP-PATH(SUB)
                    END-IF
                 END-PERFORM
              ELSE
      * DONT PERFORM ACCESS CALLS IF ALL BRANCHES SELECTED
                 PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                    MOVE "Y" TO FILE-FOUND(SUB)
                    MOVE REPORT-DATE TO LOC-RPT-DT(SUB)
                    MOVE LOC-RPT(SUB) TO DISP-PATH(SUB)
                 END-PERFORM
              END-IF
              MOVE DISP-RPT TO WR-BUF
              MOVE RPT-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       SETUP-PROMPT-EXIT.
           EXIT.


      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO PRTERN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PRTERN-SYSDATE.
           DISPLAY PRTERN-SCREEN UPON PRTERN-WINDOW-HANDLE.
           MOVE PRTERN-FORM-FIELDS TO WR-BUF.
           MOVE PRTERN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE PRTERN-HELP--FILE TO HELP-LINK-FILE.
           MOVE PRTERN-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PRTERN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       GET-GB.
           PERFORM OPEN-GB-FILE.
           MOVE REPORT-BRNO TO GB-BRNO.
           MOVE 1           TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPEMMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY PRTERN-WINDOW-HANDLE.
           EXIT PROGRAM.
