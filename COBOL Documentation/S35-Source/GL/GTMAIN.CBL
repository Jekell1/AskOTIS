       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GTMAIN.
      ******************************************************************
      *
      *                 (GL)                                     
      *  DESCRIPTION:  G/L TRANSACTION FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * BLV 080897 ADD GT-SMITH-SENT (WORLD)
      * MJD 090597 CHANGED GT-AMT TO BY S9(9)V99.
      * GLF 102997 REMOVED RESET OF FIELDS
      * GLB 990614 TEST GT-FLAG = "P" INSTEAD OF "Y"
      * MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLF 070110 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD, ALSO LOG DELETE,
      *            SUPPORT PR# 2780
      * BLF 070119 ADDED FIELDS FOR POSTING USERID, DATE & TIME,
      *            SUPPORT PR# 2780
      * KEC 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
      * KEC 2016.1129 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170320 FIX GT DATES FOR 8 DIGITS, PD0006
      * BAH 20190710 ADDED GLGTCL, FUTURE FIELDS WERE NOT CLEARED!
      * BAH 2023.1005 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
      * RMZ 2024.0606 IMPROVED PROCESSING TIME - STORED PROCEDURE. 
      *               SCAN PROCESSING IS NOW MORE EFFICIENT.
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GL/GTMAIN S35 06/05/24-10:56:41 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".

      *=================================================================
      *   GTCH-REC FROM 'REPLACING LEADING' ON GT-REC.        [CHANGED ]
      *   GTOR-REC FROM 'REPLACING LEADING' ON GT-REC.        [ORIGINAL]
      *=================================================================
       COPY "LIBGL/GL01GT.CPY" REPLACING LEADING "GT" BY "GTCH".
       COPY "LIBGL/GL01GT.CPY" REPLACING LEADING "GT" BY "GTOR".

      *=================================================================

       01  GT1-CONN-OPEN-SW            PIC X          VALUE "N".

       01  CHANGED-GT-REC              PIC X(GT-SIZE) VALUE SPACES.
       01  ORIGINAL-GT-REC             PIC X(GT-SIZE) VALUE SPACES.

      *=================================================================
       01  HOLD-GT1-KEY.
           03  HOLD-GT-SOURCE          PIC 9(01).
           03  HOLD-GT-DATE2           PIC 9(08).
           03  HOLD-GT3-KEY.
               05  HOLD-GT-BRANCH      PIC 9(04).
               05  HOLD-GT-ACCTNO      PIC 9(08).
               05  HOLD-GT-DATE3       PIC 9(08).
               05  HOLD-GT-DRCRFG      PIC X(01).
               05  HOLD-GT-LINE        PIC 9(06).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  WORK-ERRLOG-ACCOUNT      PIC Z(7).
           03  WORK-ERRLOG-DATE         PIC 9(6).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(6).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  ERRCD              PIC X    VALUE SPACES.
       01  CHAN-DATA          VALUE SPACES.
           03  CHAN           PIC X(4).
       01  ONE                PIC S9.
       01  ELE                PIC 9(2).
       01  ACTION             VALUE SPACES.
           03  CHKEY          PIC X(4).
           03  CHNO           PIC 9(2).
       01  IO-TYPE            PIC X(3).
       01  NEW-FLAG           PIC 9.
       01  FIRST-SCAN         PIC 9       VALUE 0.
       01  WORK-TIME          PIC 9(6).
       01  FILLER REDEFINES WORK-TIME.
           04  TIME-HHMM      PIC 9(4).
           04  FILLER         PIC 9(2).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-010       PIC X.
           03  D-020       PIC X(14).
           03  D-030       PIC ZZZZZZZZZ.ZZ-.
           03  D-040       PIC X(32).
           03  D-050       PIC 99.
           03  D-060       PIC X.
           03  D-070       PIC X(8).
           03  D-080       PIC 99/99/99 BLANK ZERO.
           03  D-090       PIC 9(4) BLANK ZERO.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 9.
       01  SPEC-TABLE.
      * 1. GT-FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. GT-REF
           03  FILLER      PIC X(8) VALUE "E  A140 ".
      * 3. GT-AMT
           03  FILLER      PIC X(8) VALUE "ENNS092 ".
      * 4. GT-EXPL
           03  FILLER      PIC X(8) VALUE "E  A320 ".
      * 5. GT-PERIOD
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 6. GT-SMITH-SENT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 7. GT-POST-USERID
           03  FILLER      PIC X(8) VALUE "E  A080N".
      * 8. GT-POST-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 9. GT-POST-TIME
           03  FILLER      PIC X(8) VALUE "ENNN040N".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 9 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       01  GTMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GTMAIN_DEF.CPY".
       COPY "LIBGL/GTMAIN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/GTMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************
      *      MAIN PROGRAM MODULE
      ******************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
           MOVE "GTMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GTMAIN-WINDOW-HANDLE.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-GT-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      *************************************
       REC-GT-MODULE SECTION.
      *************************************
       REC-GT-MODULE-NO.
      D    STOP MESS.
           MOVE SPACES TO GT1-KEY.
           MOVE 0 TO GT-SOURCE GT-DATE2 GT-BRANCH GT-ACCTNO
                     GT-DATE3 GT-LINE.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
      *    CALL FRESET USING STAT.
      *    MOVE " ENTER SOURCE, F6-SCAN" TO MESS.
           MOVE " ENTER SOURCE         " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N010SOURCE." TO VDU.
           PERFORM SCREENIO.
      *    IF F6-KEY
      *       PERFORM ENTER-SCANKEY
      *       GO TO END-REC-GT-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GT-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-GT-MODULE-NO.
           IF VDUNUM0 < 1 OR > 5
              GO TO REC-GT-MODULE-NO.
           MOVE VDUNUM0 TO GT-SOURCE.
       REC-TRANS-DATE.
      *    MOVE "ENTER TRANSACTION DATE, F6-SCAN" TO MESS.
           MOVE "ENTER TRANSACTION DATE         " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ER M080TRDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY OR UP-KEY
      *       CALL FRESET USING STAT
              GO TO REC-GT-MODULE-NO.
      *    IF F6-KEY
      *       PERFORM ENTER-SCANKEY
      *       GO TO END-REC-GT-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GT-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-TRANS-DATE.
           IF VDUNUM0 = 0
              GO TO REC-TRANS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO GT-DATE1
                                     GT-DATE2
                                     GT-DATE3.

       REC-LOCATION.
      *    MOVE "ENTER BRANCH, F6-SCAN" TO MESS.
           MOVE "ENTER BRANCH         " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
      *       CALL FRESET USING STAT
              GO TO REC-GT-MODULE-NO.
           IF UP-KEY
              GO TO REC-TRANS-DATE.
      *    IF F6-KEY
      *       PERFORM ENTER-SCANKEY
      *       GO TO END-REC-GT-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GT-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-LOCATION.
           IF VDUNUM0 = 0
              GO TO REC-LOCATION.
           MOVE VDUNUM0 TO BR-NO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
               MOVE " INVALID BRANCH NUMBER" TO MESS
               PERFORM SEND-MESS
               GO TO REC-LOCATION.
           MOVE VDUNUM0 TO GT-BRANCH GL-BRANCH.
       REC-ACCT-NO.
           MOVE "F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N070ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
      *       CALL FRESET USING STAT
              GO TO REC-GT-MODULE-NO.
           IF UP-KEY
              GO TO REC-LOCATION.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GT-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GT-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-ACCT-NO.
           IF VDUNUM0 = 0
              GO TO REC-ACCT-NO.
           MOVE VDUNUM0 TO GT-ACCTNO GL-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO REC-ACCT-NO.
       REC-DRCRFG.
           MOVE "F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA010DRCRFG." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
      *       CALL FRESET USING STAT
              GO TO REC-GT-MODULE-NO.
           IF UP-KEY
              GO TO REC-ACCT-NO.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GT-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GT-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-DRCRFG.
           IF VDUBUF NOT = "1" AND NOT = "2"
              GO TO REC-DRCRFG.
           MOVE VDUBUF TO GT-DRCRFG.
       REC-LINE-NO.
           MOVE "F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060LINENO." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
      *       CALL FRESET USING STAT
              GO TO REC-GT-MODULE-NO.
           IF UP-KEY
              GO TO REC-DRCRFG.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GT-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GT-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-LINE-NO.
           MOVE VDUNUM0 TO GT-LINE.
           MOVE GT1-KEY TO HOLD-GT1-KEY.
       READ-GTFILE.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-GT1-FILE.
           PERFORM READ-GT1-FILE. *> EMBEDDED - GTFILE
           PERFORM CLOSE-GT1-FILE.
           IF IO-GOOD
              MOVE GT-REC TO ORIGINAL-GT-REC 
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-GT-MODULE.
           EXIT.

      *************************************
       ENTER-SCANKEY SECTION.
      *************************************
       ENTER-SCAN.
      *    MOVE "F1 RESET SCAN, F6 CONTINUE SCAN" TO MESS.
      *    PERFORM SEND-LEGEND.
      *    MOVE "S  A010SOURCE." TO VDU.
      *    MOVE SPACES TO VDUBUF.
      *    PERFORM SCREENIO.
           PERFORM OPEN-GT1-FILE.
       ENTER-KEY.
      *    MOVE "E RA320SCANKEY." TO VDU.
           IF FIRST-SCAN = 1
              MOVE "ENNN010SOURCE." TO VDU
              PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF F6-KEY
              GO TO READ-GT1FILE-NEXT.
           IF NOT ENTER-KEY
              GO TO ENTER-KEY.

      *    IF VDUBUF = SPACES
      *       IF FIRST-SCAN NOT = 0
      *          MOVE GT1-KEY TO HOLD-GT1-KEY
      *          MOVE GT-REC TO ORIGINAL-GT-REC                 
      *          GO TO CLOSE-SCANKEY
      *       ELSE
      *          GO TO ENTER-KEY.
 
           IF FIRST-SCAN NOT = 0
              MOVE GT1-KEY TO HOLD-GT1-KEY
              MOVE GT-REC TO ORIGINAL-GT-REC                 
              GO TO CLOSE-SCANKEY
           ELSE
              GO TO ENTER-KEY. 

       READ-GT1FILE-NEXT.
           IF FIRST-SCAN = 0
              IF GT-DATE2 = 0
                 MOVE 19000101 TO GT-DATE2
              END-IF
              IF GT-DATE3 = 0
                 MOVE 19000101 TO GT-DATE3
              END-IF
              PERFORM START-GT1-FILE *> SSP - GTFILE, GLFILE
              MOVE 1 TO FIRST-SCAN.
           PERFORM READ-GT1-FILE-NEXT. *> EMBEDDED - GTFILE, GLFILE
           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "S  A010SOURCE." TO VDU.
           MOVE GT-SOURCE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A080TRDATE." TO VDU.
           MOVE GT-DATE2                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE GT-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070ACCTNO." TO VDU.
           MOVE GT-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           IF GL-BR-ACCOUNT = 99999999999 
              MOVE "S  A300ACCTMSG."                TO VDU 
              MOVE "*** INVALID ACCOUNT NUMBER ***" TO VDUBUF 
              PERFORM SCREENIO
           ELSE
              MOVE "S  A300ACCTMSG."                TO VDU 
              MOVE SPACES                           TO VDUBUF 
              PERFORM SCREENIO.
             
           MOVE "S  A010DRCRFG." TO VDU.
           MOVE GT-DRCRFG TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SNNN060LINENO." TO VDU.
           MOVE GT-LINE TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           PERFORM CLOSE-GT1-FILE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF UP-KEY
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 F-07 F-08 F-09
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO GT-FLAG.
           IF GT-FLAG NOT = " "
      * GLB  06/14/99
      *       IF GT-FLAG NOT = "Y"
              IF GT-FLAG NOT = "P"
                 MOVE "SPACE OR 'P'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO GT-REF.
           GO TO CREATE-ROUTINE.
       F-03.
           IF GT-DRCRFG = "1"
              IF VDUNUM2 NOT > 0
                 GO TO ENTER-ELE.
           IF GT-DRCRFG = "2"
              IF VDUNUM2 > 0
                 GO TO ENTER-ELE.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.

           MOVE VDUNUM2 TO GT-AMT.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO GT-EXPL.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM0 TO GT-PERIOD.
           IF GT-PERIOD < 0 OR > 12
              MOVE "0 THRU 12" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO GT-SMITH-SENT.
           IF GT-SMITH-SENT NOT = " "
              IF GT-SMITH-SENT NOT = "Y"
                 MOVE "SPACE OR 'Y'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO GT-POST-USERID.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDU-DATE-YYYYMMDD TO GT-POST-DATE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM0 TO GT-POST-TIME.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *************************************
       WRITE-MODULE SECTION.
      *************************************
           PERFORM OPEN-GT1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE GT-REC TO CHANGED-GT-REC.
           IF ORIGINAL-GT-REC = CHANGED-GT-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-GT1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF GT-REC NOT = ORIGINAL-GT-REC              
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-GT-REC TO GT-REC.
           PERFORM REWRITE-GT1-FILE. *> EMBEDDED - GTFILE
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE TO GT-POST-DATE.
           MOVE POST-TIME TO WORK-TIME.
           MOVE TIME-HHMM TO GT-POST-TIME.
           MOVE POST-USERID    TO GT-POST-USERID.
           PERFORM WRITE-GT1-FILE. *> EMBEDDED - GTFILE
           IF IO-BAD
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-GT1-FILE.

      *************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *************************************

           MOVE ORIGINAL-GT-REC TO GTOR-REC.
           MOVE CHANGED-GT-REC  TO GTCH-REC.

           MOVE SPACES                     TO WORK-ERRLOG-DETAIL.
           MOVE GT-BRANCH                  TO WORK-ERRLOG-BR.
           MOVE GT-ACCTNO                  TO WORK-ERRLOG-ACCOUNT.
           MOVE GT-DATE1                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO WORK-ERRLOG-DATE.
           MOVE "TO "                      TO WORK-ERRLOG-TO-X.

           IF GTOR-FLAG NOT = GTCH-FLAG
             MOVE "UPD FG"                TO WORK-ERRLOG-DESC
             MOVE GTOR-FLAG               TO WORK-ERRLOG-ORG-ALP
             MOVE GTCH-FLAG               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF GTOR-REF NOT = GTCH-REF
             MOVE "REFER "                TO WORK-ERRLOG-DESC
             MOVE GTOR-REF                TO WORK-ERRLOG-ORG-ALP
             MOVE GTCH-REF                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF GTOR-AMT NOT = GTCH-AMT
             MOVE "AMOUNT"                TO WORK-ERRLOG-DESC
             MOVE GTOR-AMT                TO WORK-ERRLOG-ORG-N2
             MOVE GTCH-AMT                TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF GTOR-EXPL NOT = GTCH-EXPL
             MOVE "EXPLAN"                TO WORK-ERRLOG-DESC
             MOVE GTOR-EXPL               TO WORK-ERRLOG-ORG-ALP
             MOVE GTCH-EXPL               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF GTOR-PERIOD NOT = GTCH-PERIOD
             MOVE "PERIOD"                TO WORK-ERRLOG-DESC
             MOVE GTOR-PERIOD             TO WORK-ERRLOG-ORG-N0
             MOVE GTCH-PERIOD             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF GTOR-SMITH-SENT NOT = GTCH-SMITH-SENT
             MOVE "SENT  "                TO WORK-ERRLOG-DESC
             MOVE GTOR-SMITH-SENT         TO WORK-ERRLOG-ORG-ALP
             MOVE GTCH-SMITH-SENT         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      *************************************
       LOG-FIELD-CHANGE SECTION.
      *************************************

      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *************************************
       DELETE-MODULE SECTION.
      *************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-GT1-FILE.
           PERFORM READ-GT1-FILE. *> EMBEDDED - GTFILE
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF GT-REC NOT = ORIGINAL-GT-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-GT1-FILE. *> EMBEDDED - GTFILE

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING GT1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-GT1-FILE.

      *************************************
       NEW-RECORD-SETUP SECTION.
      *************************************
           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-GT-FILE.

           MOVE HOLD-GT1-KEY TO GT1-KEY.
           MOVE 0 TO ELE.
      * GT-DATE2 IS PART OF PRIMARY KEY
           MOVE GT-DATE2 TO GT-DATE1 GT-DATE3.
           MOVE 0 TO GT-AMT GT-PERIOD
                     GT-POST-DATE GT-POST-TIME.

      *************************************
       DISPLAY-FIELDS SECTION.
      *************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE GT-FLAG TO D-010.
           MOVE GT-REF  TO D-020.
           MOVE GT-AMT  TO D-030.
           MOVE GT-EXPL TO D-040.
           MOVE GT-PERIOD TO D-050.
           MOVE GT-SMITH-SENT TO D-060.
           MOVE GT-POST-USERID TO D-070.
           MOVE GT-POST-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-080.
           MOVE GT-POST-TIME TO D-090.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       COPY "LIBGL/GLGTCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************
       FORM-RESET SECTION.
      *************************************
           MOVE EXT-CONAME-CONAME TO GTMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GTMAIN-SYSDATE.
      *    DISPLAY OMITTED ERASE SCREEN.
      * USING THE LINE 1 COL 1 WORKS MUCH MORE CONSISTENTLY!
      * THE OMITTED LINE WOULD NOT CLEAR ON A F1 FROM THE BOTTOM BUT WOULD
      * CLEAR WHEN PART WAY THRU A NEW RECORD. DIDN'T MAKE SENSE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GTMAIN-SCREEN UPON GTMAIN-WINDOW-HANDLE.
           MOVE GTMAIN-FORM-FIELDS TO WR-BUF.
           MOVE GTMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************
       SETUP-LINK-INFO SECTION.
      *************************************
           MOVE GTMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GTMAIN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GTMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL."  TO VDU.
           MOVE MESS            TO VDUBUF.
           PERFORM SCREENIO.

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GT1-CONNECTION.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1RN.CPY".

      * COPY "LIBGL/GLGT1IN.CPY".
      * COPYMEMBER: LIBGL/GLGT1IN
      ******************************************************************
      * REV:
      * JKC 2023-1019 COMMENTED OUT START-GT1-FILE-GREATER; ONE PROGRAM
      *          (GL/DELDUP) ONLY USES IT; DUE TO THE COMPLEX GT1-KEY I
      *          DO NOT BELIEVE IT IS PROPERLY SETUP FOR IT TO WORK
      *          CORRECTLY; AFTER DISCUSSING WITH KEVIN MULLEN, DELDUP
      *          WAS ONLY USED ONCE BUT WOULD BE NICE TO HAVE IN CASE
      *          NEEDED FOR AN EMERGENCY; RON ZAMISKA WAS ABLE TO
      *          ACCOMPLISH WHAT DELDUP DOES WITHIN A SQL CODE; WE
      *          ARCHIVED DELDUP FROM USE SO THE USE OF
      *          START-GT1-FILE-GREATER IS NO LONGER NEEDED.
      ******************************************************************
       LOAD-GT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

           IF GT1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO GT1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS GT1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-GT1-FILE. *> SSP - GTFILE, GLFILE
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( QGT1-WEND-LINE = ZEROES    ) OR
      *       ( QGT1-WEND-LINE NOT NUMERIC )
      *       MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
      *       MOVE 9                    TO QGT1-WEND-SOURCE
      *
      *       MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
      *       PERFORM SQL-SET-DATE
      *       MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
      *       MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
      *       PERFORM SQL-SET-DATE
      *       MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3
      *
      *       MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
      *       PERFORM SQL-SET-DATE
      *       MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
      *                                    QGT1-WEND-DATE3
      *
      *       MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
      *       MOVE ALL "9"              TO QGT1-WEND-BRANCH
      *       MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
      *       MOVE ALL "9"              TO QGT1-WEND-ACCTNO
      *       MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
      *       MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
      *       MOVE GT-LINE              TO QGT1-WBEG-LINE
      *       MOVE ALL "9"              TO QGT1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *       IF ( GT-SOURCE = 9 )
      *          MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
      *       ELSE
      *          COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1
      *       END-IF
      *
      *       MOVE GT-DATE2             TO NDTE-DATE
      *       MOVE 1                    TO NDTE-HOLD
      *       PERFORM INCREMENT-DAYS
      *       MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *       PERFORM SQL-SET-DATE
      *       MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2
      *
      *       IF ( GT-BRANCH = 9999 )
      *          MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
      *       ELSE
      *          COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1
      *       END-IF
      *
      *       IF ( GT-ACCTNO = 99999999 )
      *          MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
      *       ELSE
      *          COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1
      *       END-IF
      *
      *       MOVE GT-DATE3             TO NDTE-DATE
      *       MOVE 1                    TO NDTE-HOLD
      *       PERFORM INCREMENT-DAYS
      *       MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *       PERFORM SQL-SET-DATE
      *       MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3
      *
      *       MOVE 01                   TO SQL-TEXT-SIZE
      *       MOVE GT-DRCRFG            TO SQL-TEXT
      *       PERFORM SQL-TEXT-INCREMENT
      *       MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE GT-SOURCE             TO QGT1-WBEG-SOURCE.
           MOVE GT-DATE2              TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD   TO QGT1-WBEG-DATE2.
           MOVE '2099-12-31'          TO QGT1-WEND-DATE2.
           MOVE GT-BRANCH             TO QGT1-WBEG-BRANCH.

           IF ( GT-ACCTNO   = ZEROES  ) OR
              ( GT-ACCTNO NOT NUMERIC )
              MOVE ALL "0"            TO QGT1-WBEG-ACCTNO
           ELSE 
              MOVE GT-ACCTNO          TO QGT1-WBEG-ACCTNO.

           MOVE ALL "9"               TO QGT1-WEND-ACCTNO.

      *    IF ( GT-DRCRFG NOT = 1 )   AND 
      *       ( GT-DRCRFG NOT = 2 )
      *        MOVE "1"               TO QGT1-WBEG-DRCRFG
      *        MOVE "2"               TO QGT1-WEND-DRCRFG
      *    ELSE
      *        MOVE GT-DRCRFG         TO QGT1-WBEG-DRCRFG
      *                                  QGT1-WEND-DRCRFG.

           MOVE "1"                   TO QGT1-WBEG-DRCRFG.
           MOVE "2"                   TO QGT1-WEND-DRCRFG. 


      *    IF ( GT-LINE     = ZEROES  ) OR
      *       ( GT-LINE   NOT NUMERIC )
      *       MOVE ALL "0"            TO QGT1-WBEG-LINE
      *    ELSE 
      *       MOVE GT-LINE            TO QGT1-WBEG-LINE.

           MOVE ALL "0"               TO QGT1-WBEG-LINE
           MOVE ALL "9"               TO QGT1-WEND-LINE.

      * STORED PROCEDURE:
      *
      * SELECT GTFILE.GT_FLAG,
      *        GTFILE.GT_REF,
      *        CAST(GTFILE.GT_DATE1       AS VARCHAR(10))  AS GT_DATE1,
      *        GTFILE.GT_SOURCE,	      
      *        CAST(GTFILE.GT_DATE2       AS VARCHAR(10))  AS GT_DATE2,
      *        GTFILE.GT_BRANCH,	      
      *        GTFILE.GT_ACCTNO,	      
      *        CAST(GTFILE.GT_DATE3       AS VARCHAR(10))  AS GT_DATE3,
      *        GTFILE.GT_DRCRFG,
      *        GTFILE.GT_LINE,
      *        CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10))  AS GT_LTOUCH_DATE,
      *        GTFILE.GT_AMT,
      *        GTFILE.GT_EXPL,
      *        GTFILE.GT_PERIOD,
      *        GTFILE.GT_SMITH_SENT,
      *        GTFILE.GT_POST_USERID,
      *        CAST(GTFILE.GT_POST_DATE   AS VARCHAR(10))  AS GT_POST_DATE,
      *        GTFILE.GT_POST_TIME,
      *        ISNULL(GLFILE.GL_BR_ACCOUNT, '99999999999') AS GL_BR_ACCOUNT   
      *   FROM DBO.GTFILE GTFILE
      *   LEFT OUTER JOIN
      *        DBO.GLFILE GLFILE
      *    ON (SUBSTRING(CAST(FORMAT(GLFILE.GL_BR_ACCOUNT, 'D11') AS CHAR(11)),1,4) 
      *        = GTFILE.GT_BRANCH
      *   AND  SUBSTRING(CAST(FORMAT(GLFILE.GL_BR_ACCOUNT, 'D11') AS CHAR(11)),5,7) 
      *        = GTFILE.GT_ACCTNO) 
      *
      *  WHERE (GTFILE.GT_SOURCE       = @QGT1_WBEG_SOURCE
      *    AND  GTFILE.GT_DATE2  BETWEEN @QGT1_WBEG_DATE2 
      *                              AND @QGT1_WEND_DATE2 
      *    AND  GTFILE.GT_BRANCH       = @QGT1_WBEG_BRANCH
      *    AND  GTFILE.GT_ACCTNO BETWEEN @QGT1_WBEG_ACCTNO 
      *                              AND @QGT1_WEND_ACCTNO 
      *    AND  GTFILE.GT_DRCRFG BETWEEN @QGT1_WBEG_DRCRFG 
      *                              AND @QGT1_WEND_DRCRFG 
      *    AND  GTFILE.GT_LINE   BETWEEN @QGT1_WBEG_LINE   
      *                              AND @QGT1_WEND_LINE)


           EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
                :GT1-RETURN-CODE = EXEC SSP_GTMAIN_GTFILE_SELECT 
              ( :QGT1-WBEG-SOURCE,
                :QGT1-WBEG-DATE2,
                :QGT1-WEND-DATE2, 
                :QGT1-WBEG-BRANCH, 
                :QGT1-WBEG-ACCTNO,
                :QGT1-WEND-ACCTNO,  
                :QGT1-WBEG-DRCRFG, 
                :QGT1-WEND-DRCRFG, 
                :QGT1-WBEG-LINE,   
                :QGT1-WEND-LINE )
           END-EXEC.
   
           EXEC SQL
               OPEN GT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3
                          QGT1-WSBEG-DATE2
                          QGT1-WSBEG-DATE3
                          QGT1-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE
                          QGT1-WSBEG-SOURCE
                          QGT1-WSBEG-BRANCH
                          QGT1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-GT1-FILE. *> EMBEDDED - GTFILE 
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-SOURCE           TO QGT-SOURCE.
           MOVE GT-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.
           MOVE GT-BRANCH           TO QGT-BRANCH.
           MOVE GT-ACCTNO           TO QGT-ACCTNO.
           MOVE GT-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.
           MOVE GT-DRCRFG           TO QGT-DRCRFG.
           MOVE GT-LINE             TO QGT-LINE.

           EXEC SQL
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT. *> EMBEDDED - GTFILE, GLFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.
           INITIALIZE QGL-REC.

           EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH GT1_CURSOR 
                  INTO 
                    :QGT-FLAG,
                    :QGT-REF,
                    :QGT-DATE1,        
                    :QGT-SOURCE,        
                    :QGT-DATE2,        
                    :QGT-BRANCH,        
                    :QGT-ACCTNO,        
                    :QGT-DATE3,        
                    :QGT-DRCRFG,           
                    :QGT-LINE,             
                    :QGT-LTOUCH-DATE,  
                    :QGT-AMT,              
                    :QGT-EXPL,             
                    :QGT-PERIOD,           
                    :QGT-SMITH-SENT,       
                    :QGT-POST-USERID,      
                    :QGT-POST-DATE,    
                    :QGT-POST-TIME,        
                    :QGL-BR-ACCOUNT         
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS
              PERFORM GET-GL-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       WRITE-GT1-FILE. *> EMBEDDED - GTFILE 

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.GTFILE
             (GTFILE.GT_FLAG,
              GTFILE.GT_REF,
              GTFILE.GT_DATE1,
              GTFILE.GT_SOURCE,
              GTFILE.GT_DATE2,
              GTFILE.GT_BRANCH,
              GTFILE.GT_ACCTNO,
              GTFILE.GT_DATE3,
              GTFILE.GT_DRCRFG,
              GTFILE.GT_LINE,
              GTFILE.GT_LTOUCH_DATE,
              GTFILE.GT_AMT,
              GTFILE.GT_EXPL,
              GTFILE.GT_PERIOD,
              GTFILE.GT_SMITH_SENT,
              GTFILE.GT_POST_USERID,
              GTFILE.GT_POST_DATE,
              GTFILE.GT_POST_TIME)
            VALUES
             (:QGT-FLAG,
              :QGT-REF,
              :QGT-DATE1,
              :QGT-SOURCE,
              :QGT-DATE2,
              :QGT-BRANCH,
              :QGT-ACCTNO,
              :QGT-DATE3,
              :QGT-DRCRFG,
              :QGT-LINE,
              :QGT-LTOUCH-DATE,
              :QGT-AMT,
              :QGT-EXPL,
              :QGT-PERIOD,
              :QGT-SMITH-SENT,
              :QGT-POST-USERID,
              :QGT-POST-DATE,
              :QGT-POST-TIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GT1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT    
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-GT1-FILE. *> EMBEDDED - GTFILE 

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.GTFILE
             SET
              GTFILE.GT_FLAG = :QGT-FLAG,
              GTFILE.GT_REF = :QGT-REF,
              GTFILE.GT_DATE1 = :QGT-DATE1,
              GTFILE.GT_SOURCE = :QGT-SOURCE,
              GTFILE.GT_DATE2 = :QGT-DATE2,
              GTFILE.GT_BRANCH = :QGT-BRANCH,
              GTFILE.GT_ACCTNO = :QGT-ACCTNO,
              GTFILE.GT_DATE3 = :QGT-DATE3,
              GTFILE.GT_DRCRFG = :QGT-DRCRFG,
              GTFILE.GT_LINE = :QGT-LINE,
              GTFILE.GT_LTOUCH_DATE = :QGT-LTOUCH-DATE,
              GTFILE.GT_AMT = :QGT-AMT,
              GTFILE.GT_EXPL = :QGT-EXPL,
              GTFILE.GT_PERIOD = :QGT-PERIOD,
              GTFILE.GT_SMITH_SENT = :QGT-SMITH-SENT,
              GTFILE.GT_POST_USERID = :QGT-POST-USERID,
              GTFILE.GT_POST_DATE = :QGT-POST-DATE,
              GTFILE.GT_POST_TIME = :QGT-POST-TIME
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GT1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT    
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-GT1-FILE. *> EMBEDDED - GTFILE 

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           PERFORM DELETE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GT1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT    
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.

           PERFORM CLOSE-IT.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-GT1-CONNECTION.

           IF GT1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT GT1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO GT1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *************************************
       END-ROUTINE SECTION.
      *************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GTMAIN-SCREEN.
           DESTROY GTMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
