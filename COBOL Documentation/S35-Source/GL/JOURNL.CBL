       IDENTIFICATION DIVISION.
       PROGRAM-ID.      JOURNL.
      ******************************************************************
      *
      *                 (GL)
      *  DESCRIPTION:   G/L JOURNAL ENTRY
      *
      *=================================================================
      * REV:
      * MJD 090897 CHANGED GT-AMT TO BE PIC S9(9)V99
      * BLV 990316 FIX DISPLAY-BUF FOR LONGER GT-AMT
      * BLV 990513 FIELD TOTADJ NOT LONG ENOUGH SO SIGN WAS TRUNCATED;
      *            DIDN'T CHANGE PROGRAM, JUST CHANGED FIELD FROM 16 TO 17
      * MG  000207 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLF 070119 ADD UPDATE OF GT-POST-USERID, GT-POST-DATE,
      *            GT-POST-TIME, SUPPORT PR# 2780
      * BLF 080401 FIX SIZE OF ORIGINAL-REC, CHANGED REC.  THESE HAVE BEEN
      *            INCORRECT FOR YEARS (WAS 84 BYTES FROM A48) BUT DIDN'T
      *            CAUSE A PROBLEM UNTIL I ADDED USERID & POSTING TIME,
      *            THEN THEY WERE SAYING "UNDERGONE CHANGE" AND WOULDN'T
      *            ALLOW DELETE OR MODIFICATION OF EXISTING RECORD., PL# 624
      * KEC 2016.0105 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
      * KEC 2016.1130 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170407 FIX FOR 8 DIGIT DATES
      * BAH 20190710 ADDED GLGTCL, FUTURE FIELDS WERE NOT CLEARED!
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/JOURNL.CBL S35 05/09/23-13:49:08 >".

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
       01  ERRCD              PIC X       VALUE SPACES.
       01  CHAN               PIC X(4)    VALUE "CHAN".
       01  ONE                PIC S9.
       01  ELE                PIC 9(2).
       01  ACTION             VALUE SPACES.
           03  CHKEY          PIC X(4).
           03  CHNO           PIC 9(2).
       01  IO-TYPE            PIC X(3).
       01  INVALID-GL-READ    PIC X.
       01  NEW-FLAG           PIC 9.
       01  HOLD-GT-REF      PIC X(14) VALUE SPACES.
       01  HOLD-GT-AMT      PIC S9(9)V99  VALUE +0.
       01  HOLD-GT-EXPL     PIC X(36) VALUE SPACES.

       01  TOTAL-ADJUSTMENT    PIC S9(10)V99     VALUE +0 COMP-3.
       01  HOLD-TOT-ADJUSTMENT PIC S9(10)V99    VALUE +0 COMP-3.

       01  WORK-TIME          PIC 9(6).
       01  FILLER REDEFINES WORK-TIME.
           04  TIME-HHMM      PIC 9(4).
           04  TIME-SS        PIC 9(2).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-010       PIC X(14).
           03  D-020       PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-030       PIC X(36).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(12)  VALUE
           "010 020 030.".
       01  DISPLAY-NAMES   PIC X(4)  VALUE "NEW.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 3.
       01  SPEC-TABLE.
      * 1. GT-REF
           03  FILLER      PIC X(8) VALUE "E  A140 ".
      * 2. GT-AMT
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 3. GT-EXPL
           03  FILLER      PIC X(8) VALUE "E  A360 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 3 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  RESET-LIST        PIC X(38)   VALUE
           "ACCTNO DESC DRCRFG LINENO SEL NEW 020.".

       COPY "LIBGL/GLSCANW.CPY".
       COPY "LIBGL/GLSCAN_CMD.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  JOURNL-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/JOURNL_DEF.CPY".
       COPY "LIBGL/JOURNL_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/JOURNL_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *    ***************************
      *    *      MAIN PROGRAM MODULE
      *    ***************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       MAIN-MODULE.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "JOURNL" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS JOURNL-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           PERFORM FORM-RESET.

       MAIN-ROUTINE.
           MOVE RESET-LIST TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           PERFORM REC-ENTER-KEYS-MODULE.
           IF IO-TYPE = "END"
              IF TOTAL-ADJUSTMENT NOT = 0
                 PERFORM OUT-OF-BALANCE
                 IF IO-TYPE = "RTN"
                    GO TO MAIN-ROUTINE
                 ELSE
                    GO TO END-ROUTINE
              ELSE
                 GO TO END-ROUTINE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
      * BAW  032488  RESET EVERYTHING ON F1 AND DELETE
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
      *    IF IO-TYPE = "CLR"
      *       CALL FRESET USING STAT.
           IF INVALID-GL-READ NOT = "Y"
               MOVE SPACES TO HOLD-GT-REF
                              HOLD-GT-EXPL
               MOVE 0 TO HOLD-GT-AMT.
           GO TO MAIN-ROUTINE.
      *    **************************
      *    *    RECEIVE STATE & CITY CODES
      *    *         MODULE
      *    **************************
       REC-ENTER-KEYS-MODULE SECTION.

           MOVE 0      TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "SN N020SOURCE." TO VDU.
           MOVE 1 TO VDUNUM0 GT-SOURCE
                             HOLD-GT-SOURCE.
           PERFORM SCREENIO.

           GO TO REC-TRANS-DATE.
       RESET-FIELDS.
           PERFORM FORM-RESET.
       REC-SOURCE.
           MOVE "EN N020SOURCE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-SOURCE.
           IF VDUNUM0 = 0
              GO TO REC-SOURCE.
           MOVE VDUNUM0 TO GT-SOURCE HOLD-GT-SOURCE.
           GO TO REC-TRANS-DATE.
       RESET-TRANS-DATE.
           MOVE "TRDATE." TO MESS.
      *    CALL FRESET USING STAT MESS.
       REC-TRANS-DATE.
           MOVE "ERNM060TRDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF UP-KEY
              GO TO REC-SOURCE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO RESET-TRANS-DATE.

           MOVE VDU-DATE-YYYYMMDD TO 
                GT-DATE1 GT-DATE2 GT-DATE3 HOLD-GT-DATE2 HOLD-GT-DATE3.

           IF VDU-DATE-YYYYMMDD = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000TRDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD
                GT-DATE1 GT-DATE2 GT-DATE3 HOLD-GT-DATE2 HOLD-GT-DATE3
              PERFORM SCREENIO.

       REC-LOCATION.
           MOVE "ENNN040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF UP-KEY
              GO TO REC-TRANS-DATE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF F6-KEY
              GO TO SCAN-FOR-GL-ACCOUNTS1.
           IF NOT ENTER-KEY
              GO TO REC-LOCATION.
           IF VDUNUM0 = 0
              GO TO REC-LOCATION.
           MOVE VDUNUM0 TO HOLD-GT-BRANCH.
           GO TO VERIFY-BRANCH.

       SCAN-FOR-GL-ACCOUNTS1.

           MOVE SPACES TO GLSCAN-KEY.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-KEY = SPACES
              GO TO REC-ACCT-NO.

           MOVE "SN N040BRANCH." TO VDU.
           MOVE GLSCAN-BRANCH TO VDUNUM0 HOLD-GT-BRANCH.
           PERFORM SCREENIO.
           MOVE "SN N070ACCTNO." TO VDU.
           MOVE GLSCAN-ACCOUNT TO VDUNUM0 HOLD-GT-ACCTNO.
           PERFORM SCREENIO.

       VERIFY-BRANCH.
           MOVE HOLD-GT-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " INVALID BRANCH NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LOCATION.
           GO TO REC-ACCT-NO.

       RESET-ACCT-NO.
      *    MOVE "ACCTNO DESC." TO MESS.
      *    CALL FRESET USING STAT MESS.
       REC-ACCT-NO.
           MOVE "ENNN070ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF UP-KEY
              GO TO REC-LOCATION.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.

           IF F6-KEY
              GO TO SCAN-FOR-GL-ACCOUNTS2.

           IF NOT ENTER-KEY
              GO TO RESET-ACCT-NO.
           IF VDUNUM0 = 0
              GO TO RESET-ACCT-NO.
           MOVE VDUNUM0 TO HOLD-GT-ACCTNO.
           GO TO VERIFY-GL-MASTER.

       SCAN-FOR-GL-ACCOUNTS2.
           MOVE SPACES TO GLSCAN-KEY.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-KEY = SPACES
              GO TO REC-ACCT-NO.

           MOVE "SN N040BRANCH." TO VDU.
           MOVE GLSCAN-BRANCH TO VDUNUM0 HOLD-GT-BRANCH.
           PERFORM SCREENIO.
           MOVE "SN N070ACCTNO." TO VDU.
           MOVE GLSCAN-ACCOUNT TO VDUNUM0 HOLD-GT-ACCTNO.
           PERFORM SCREENIO.

       VERIFY-GL-MASTER.
           MOVE "N" TO INVALID-GL-READ.
           MOVE HOLD-GT-BRANCH TO GL-BRANCH.
           MOVE HOLD-GT-ACCTNO TO GL-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO INVALID-GL-READ
              MOVE "** NOT ON FILE **" TO GL-DESCRIPTION.
           MOVE "S  A360DESC." TO VDU.
           MOVE GL-DESCRIPTION TO VDUBUF.
           PERFORM SCREENIO.

           MOVE HOLD-GT-BRANCH TO GT-BRANCH.
           MOVE HOLD-GT-ACCTNO TO GT-ACCTNO.
           GO TO REC-DRCRFG.
       RESET-DRCRFG.
           MOVE "DRCRFG." TO MESS.
      *    CALL FRESET USING STAT MESS.
       REC-DRCRFG.
           MOVE "E  A010DRCRFG." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF UP-KEY
              GO TO REC-ACCT-NO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO RESET-DRCRFG.
           IF VDUBUF NOT = "1" AND NOT = "2"
              GO TO REC-DRCRFG.
           MOVE VDUBUF TO GT-DRCRFG HOLD-GT-DRCRFG.
           GO TO REC-LINE-NO.

       RESET-LINE-NO.
           MOVE "LINENO." TO MESS.
      *    CALL FRESET USING STAT MESS.
       REC-LINE-NO.
           MOVE "ENNN060LINENO." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF UP-KEY
              GO TO REC-DRCRFG.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO RESET-LINE-NO.
           MOVE VDUNUM0 TO HOLD-GT-LINE.
           PERFORM GET-VALID-LINE-NUMBER.
           IF HOLD-GT-LINE = 0
             OR HOLD-GT-LINE = GT-LINE
               GO TO VALID-LINE-ENTRY.
           GO TO READ-GLTRAN-FILE.
       VALID-LINE-ENTRY.
           IF INVALID-GL-READ = "Y"
              MOVE " INVALID G/L NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO REC-ACCT-NO.
           MOVE GT-LINE TO HOLD-GT-LINE.
           MOVE "SN N060LINENO." TO VDU.
           MOVE GT-LINE TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM NEW-RECORD-SETUP.
           GO TO END-REC-ENTER-KEYS-MODULE.
       READ-GLTRAN-FILE.
           MOVE HOLD-GT-BRANCH TO GT-BRANCH.
           MOVE HOLD-GT-ACCTNO TO GT-ACCTNO.
           MOVE HOLD-GT-LINE TO GT-LINE.
           PERFORM OPEN-GT1-FILE.
           PERFORM READ-GT1-FILE.
           PERFORM CLOSE-GT1-FILE.
           IF IO-GOOD
              MOVE GT-REC TO ORIGINAL-GT-REC
              MOVE GT1-KEY TO HOLD-GT1-KEY
              PERFORM DISPLAY-FIELDS
              IF INVALID-GL-READ = "Y"
                 MOVE GT-REF TO HOLD-GT-REF
                 MOVE GT-AMT TO HOLD-GT-AMT
                 MOVE GT-EXPL TO HOLD-GT-EXPL
                 GO TO END-REC-ENTER-KEYS-MODULE
              ELSE
                 GO TO END-REC-ENTER-KEYS-MODULE.
       INVALID-LINE-NUMBER.
           MOVE " INVALID LINE NUMBER" TO MESS.
           PERFORM SEND-MESS.
           GO TO RESET-LINE-NO.
       END-REC-ENTER-KEYS-MODULE.
           EXIT.
       GET-VALID-LINE-NUMBER SECTION.
       GET-VALID-LINE.
           MOVE 0 TO GT-LINE.
           PERFORM OPEN-GT1-FILE.
       GET-VALID-LINE-CONTINUE.
           ADD 1 TO GT-LINE.
           PERFORM READ-GT1-FILE.
           IF IO-BAD
              GO TO VALID-LINE-EXIT.
           GO TO GET-VALID-LINE-CONTINUE.
       VALID-LINE-EXIT.
           PERFORM CLOSE-GT1-FILE.
      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           IF GT-FLAG NOT = SPACES
              MOVE " TRANSACTION HAS BEEN UPDATED" TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           MOVE TOTAL-ADJUSTMENT TO HOLD-TOT-ADJUSTMENT.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO ELE.
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
              GO TO VERIFY-TOTAL-ADJUSTMENT.
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
                 GO TO GOTO-FIELD.
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO VERIFY-TOTAL-ADJUSTMENT.
           IF NOT ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03
                 DEPENDING ON ELE.
           GO TO VERIFY-TOTAL-ADJUSTMENT.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           IF VDUBUF = SPACES
              MOVE "S  A140010." TO VDU
              MOVE HOLD-GT-REF TO VDUBUF
                                    GT-REF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           MOVE VDUBUF TO GT-REF.
           GO TO CREATE-ROUTINE.
       F-02.
           IF HOLD-GT-DRCRFG = "1"
              IF VDUNUM2 NOT > 0
                 GO TO ENTER-ELE.
           IF HOLD-GT-DRCRFG = "2"
              IF VDUNUM2 > 0
                 GO TO ENTER-ELE.
           IF VDUNUM2 = 0
              MOVE "S  S092020." TO VDU
              MOVE HOLD-GT-AMT TO VDUNUM2
              PERFORM SCREENIO
              MOVE HOLD-GT-AMT TO VDUNUM2
              MOVE 0 TO HOLD-GT-AMT.

           SUBTRACT GT-AMT FROM TOTAL-ADJUSTMENT.
           MOVE VDUNUM2 TO GT-AMT.

           ADD VDUNUM2 TO TOTAL-ADJUSTMENT.
           PERFORM DISPLAY-TOTAL-ADJUSTMENT.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUBUF = SPACES
              MOVE "S  A360030." TO VDU
              MOVE HOLD-GT-EXPL TO VDUBUF GT-EXPL
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           MOVE VDUBUF TO GT-EXPL.
           GO TO CREATE-ROUTINE.
       VERIFY-TOTAL-ADJUSTMENT.
           IF NEW-FLAG = 0
               MOVE HOLD-TOT-ADJUSTMENT TO TOTAL-ADJUSTMENT
               MOVE 0 TO GT-AMT.
           SUBTRACT GT-AMT FROM TOTAL-ADJUSTMENT.
           PERFORM DISPLAY-TOTAL-ADJUSTMENT.
       END-ENTRY-MODULE.
           EXIT.

       DISPLAY-TOTAL-ADJUSTMENT SECTION.
       DISPLAY-NEW-TOTALS.
           MOVE "S  S102TOTADJ." TO VDU.
           MOVE TOTAL-ADJUSTMENT TO VDUNUM2.
           PERFORM SCREENIO.
      /
      ***********************************
      *       WRITE RECORD
      *         MODULE
      ***********************************
       WRITE-MODULE SECTION.
       REWRITE-RECORD.
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
           MOVE CHANGED-GT-REC TO GT-REC.
           PERFORM REWRITE-GT1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE   TO GT-POST-DATE.
           MOVE POST-TIME   TO WORK-TIME.
           MOVE TIME-HHMM   TO GT-POST-TIME.
           MOVE POST-USERID TO GT-POST-USERID.
           PERFORM WRITE-GT1-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-GT1-FILE.
      *
      *    ****************************
      *    *     DELETE RECORD
      *    *       MODULE
      *    ****************************
       DELETE-MODULE SECTION.
       DELETE-RECORD.
           SUBTRACT GT-AMT FROM TOTAL-ADJUSTMENT.
           PERFORM DISPLAY-TOTAL-ADJUSTMENT.
           PERFORM OPEN-GT1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM READ-GT1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF GT-REC NOT = ORIGINAL-GT-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-GT1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-GT1-FILE.
      *    ****************************
      *    *     NEW RECORD ROUTINE
      *    ****************************
       NEW-RECORD-SETUP SECTION.
       NEW-RECORD.
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.

           PERFORM CLEAR-GT-FILE.

           MOVE HOLD-GT1-KEY TO GT1-KEY.
      * GT-DATE2 IS PART OF PRIMARY KEY
           MOVE GT-DATE2 TO GT-DATE1 GT-DATE3.

      *    ***********************
      *    *    DISPLAY FIELDS
      *    ***********************
       DISPLAY-FIELDS SECTION.
       SETUP-BUFFER.
           MOVE GT-REF  TO D-010.
           MOVE GT-AMT  TO D-020.
           MOVE GT-EXPL TO D-030.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       OUT-OF-BALANCE SECTION.
       OUT-OF-BAL.
           MOVE SPACES TO IO-TYPE.
           MOVE "ACCEPT OUT OF BALANCE ENTRY (Y/N) ?:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010SEL2." TO VDU.
           PERFORM SCREENIO.
           IF NOT ENTER-KEY
              GO TO OUT-OF-BAL.
           IF VDUBUF  = "Y"
              MOVE SPACES TO IO-TYPE
           ELSE
              IF VDUBUF = "N"
                 MOVE "RTN" TO IO-TYPE
              ELSE
                 GO TO OUT-OF-BAL.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
           MOVE SPACES  TO WR-BUF.
           MOVE "SEL2." TO WR-LIST.
           PERFORM CALL-WRFORM.

      **********************************************************
      *  REQUIRED SECTION: CALLED BY SCREENIO:
      *  RESET FORM (SCREEN) TO ITS INITIAL STATE (LIKE FRESET).
      **********************************************************
       FORM-RESET SECTION.
           MOVE EXT-CONAME-CONAME TO JOURNL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO JOURNL-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY JOURNL-SCREEN UPON JOURNL-WINDOW-HANDLE.
           MOVE JOURNL-FORM-FIELDS TO WR-BUF. 
           MOVE JOURNL-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
      * REQUIRED SECTION: CALLED BY SCREENIO:
      *
      * SETUP CORRECT HELP FIELDS
      **********************************************************
       SETUP-LINK-INFO SECTION.
           MOVE JOURNL-HELP--FILE TO HELP-LINK-FILE.
           MOVE JOURNL-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/JOURNL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGL/GLGTCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/DATER.CPY".

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
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGL/GLGT1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY JOURNL-SCREEN.
           DESTROY JOURNL-WINDOW-HANDLE.
           EXIT PROGRAM.
