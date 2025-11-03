       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GTFIX.
      ******************************************************************
      *
      *                 (FX)
      *  CHECK SAVE-REC SIZE BEFORE RE-USING
      *
      *  DESCRIPTION:   FIX GTFILE (WRONG ACCT NUMBER)
      *
      *  NOTE: THIS PROGRAM GL/GTFIX MUST EXIST IN THE SPECIAL PASSWORD
      *          FILE (PWFILE).
      *
      *=================================================================
      * REV:
      * MJD 090897 CHANGED GT-AMT TO PIC S9(9)V99
      * MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLF 070116 MAKE LOG ENTRY IN /USR/DATA/R1/LG/GL.YYMMDD TO TRACK
      *            WHEN THIS UPDATE WAS RUN, BY WHOM, & RANGES SELECTED,
      *            SUPPORT PR# 2780
      * BAH 070907 CHANGE BRANCH AND NEWLOC TO 4 DIGITS, WAGNER PL# 601
      *
      * KEC 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
      *
      * KEC 2016.1130 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)GL/GTFIX.CBL J35 12/21/23-08:35:09 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *=================================================================
      *   OGT-REC FROM 'REPLACING LEADING' ON GT-REC.         [ORIGINAL]
      *=================================================================
       COPY "LIBGL/GL01GT.CPY" REPLACING LEADING "GT" BY "OGT".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD             PIC X       VALUE SPACES.

       01  ACTION             PIC X(6)   VALUE SPACES.
       01  IO-TYPE            PIC X(3).

       01  HOLD-BRNCH         PIC 9(4).
       01  HOLD-ACCT          PIC 9(7).
       01  HOLD-LINE          PIC 9(6) COMP-3.

       01  TOTAL-ADJUSTMENT   PIC S9(8)V99     VALUE +0 COMP.

      *    **********************************
      *    *      SCREENIO DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-010       PIC X(14).
           03  D-020       PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-030       PIC X(36).
           03  D-040       PIC 99/99/99.
           03  D-050       PIC Z9.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(20)  VALUE
           "010 020 030 040 050.".

       01  LOG-UPDATE-MSG.
           03  FILLER         PIC X(5) VALUE "OLD: ".
           03  LOG-OLD-BR     PIC 9999.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-OLD-ACCT   PIC 9(7).
           03  FILLER         PIC X    VALUE " ".
           03  LOG-DATE       PIC 99/99/99.
           03  FILLER         PIC X    VALUE " ".
           03  LOG-OLD-LINE   PIC ZZ99.
           03  FILLER         PIC X(6) VALUE " NEW: ".
           03  LOG-NEW-BR     PIC 9999.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-NEW-ACCT   PIC 9(7).
           03  FILLER         PIC X    VALUE " ".
           03  LOG-NEW-LINE   PIC ZZ99.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GTFIX-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GTFIX_DEF.CPY".
       COPY "LIBGL/GTFIX_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/GTFIX_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************

           PERFORM SQL-CONNECT.

           MOVE "GTFIX" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GTFIX-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.
           PERFORM OPEN-GT1-FILE.

           PERFORM REC-ENTER-KEYS-MODULE.
           IF IO-TYPE = "END"
              IF TOTAL-ADJUSTMENT = 0
                 GO TO END-ROUTINE
              ELSE
                 PERFORM CHECK-IN-BALANCE
                 IF IO-TYPE = "END"
                    GO TO MAIN-MODULE
                 ELSE
                    GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "END"
              GO TO MAIN-MODULE.
           GO TO MAIN-MODULE.

      ***************************************
       CHECK-IN-BALANCE SECTION.
      ***************************************
           MOVE SPACES TO IO-TYPE.

           MOVE "S  A000MSEL:BF." TO VDU.
           MOVE "              ACCEPT OUT-OF-BALANCE ENTRY? (Y/N):"
                                  TO VDUBUF.
           PERFORM SCREENIO.

       GET-BALANCE-YN.
           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = SPACES
              GO TO GET-BALANCE-YN.
           IF VDUBUF NOT = "     Y"
              MOVE "END" TO IO-TYPE.
 
       EXIT-CHECK-IN-BALANCE.
           EXIT.

      *******************************
       REC-ENTER-KEYS-MODULE SECTION.
      *******************************
       REC-ENTER-KEYS-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           GO TO REC-LOCATION.
       RESET-FIELDS.
           PERFORM FORM-RESET.
       REC-LOCATION.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-LOCATION.

           MOVE VDUNUM0 TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " INVALID BRANCH NUMBER" TO MESS
              PERFORM SEND-MESS.

           MOVE VDUNUM0 TO GT-BRANCH
                           HOLD-BRNCH
                           GL-BRANCH.

           GO TO REC-ACCT-NO.
       REC-ACCT-NO.
           MOVE "EN N070ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF UP-KEY
              GO TO REC-LOCATION.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-ACCT-NO.

           MOVE VDUNUM0 TO GT-ACCTNO
                           HOLD-ACCT
                           GL-ACCOUNT.

           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID ACCOUNT NUMBER" TO GL-DESCRIPTION.

           MOVE "S  A360DESC." TO VDU.
           MOVE GL-DESCRIPTION TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GT-BRANCH TO QGT3-WBEG-BRANCH
                             QGT3-WEND-BRANCH.
           MOVE GT-ACCTNO TO QGT3-WBEG-ACCTNO
                             QGT3-WEND-ACCTNO.
           MOVE 19000101  TO GT-DATE3 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3.

           MOVE SPACES     TO GT-DRCRFG
                              QGT3-WBEG-DRCRFG.
           MOVE ALL "Z"    TO QGT3-WEND-DRCRFG.
           MOVE 0          TO GT-LINE
                              QGT3-WBEG-LINE.
           MOVE ALL "9"    TO QGT3-WEND-LINE

      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
           IF ( GT-BRANCH = 9999 )
              MOVE GT-BRANCH         TO QGT3-WSBEG-BRANCH
           ELSE
              COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1.

           IF ( GT-ACCTNO = 99999999 )
              MOVE GT-ACCTNO         TO QGT3-WSBEG-ACCTNO
           ELSE
              COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1.

           MOVE GT-DATE3             TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3.

           MOVE 01                   TO SQL-TEXT-SIZE.
           MOVE GT-DRCRFG            TO SQL-TEXT
           PERFORM SQL-TEXT-INCREMENT
           MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.

           PERFORM START-GT3-FILE.
       RETURN-FOR-SCAN.
      D    STOP MESS.
           PERFORM READ-GT3-FILE-NEXT.
           IF IO-BAD
              MOVE "NO RECORDS FOUND FOR THIS ACCOUNT" TO MESS
              PERFORM SEND-MESS
              GO TO RESET-FIELDS.

           IF GT-ACCTNO NOT = HOLD-ACCT OR GT-BRANCH NOT = HOLD-BRNCH
              MOVE "NO RECORDS FOUND FOR THIS ACCOUNT" TO MESS
              PERFORM SEND-MESS
              GO TO RESET-FIELDS.

           PERFORM READ-GT1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM DISPLAY-FIELDS.

           MOVE GT-REC TO OGT-REC.
       REC-NEW-LOC.
           MOVE "EN N040NEWLOC." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF F6-KEY
              PERFORM SET-START-FIELDS
              PERFORM START-GT3-FILE
              PERFORM READ-GT3-FILE-NEXT
              GO TO RETURN-FOR-SCAN.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-NEW-LOC.
           IF VDUNUM0 = 0
              GO TO REC-NEW-LOC.
           MOVE VDUNUM0 TO BR-NO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
               MOVE " INVALID BRANCH NUMBER" TO MESS
               PERFORM SEND-MESS.

           MOVE VDUNUM0 TO GT-BRANCH
                           HOLD-BRNCH
                           GL-BRANCH.

           GO TO REC-NEW-ACCT.

       REC-NEW-ACCT.
           MOVE "EN N070NEWACCT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO RESET-FIELDS.
           IF UP-KEY
              GO TO REC-NEW-LOC.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-ENTER-KEYS-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-NEW-ACCT.
           IF VDUNUM0 = 0
               GO TO REC-NEW-ACCT.
           MOVE VDUNUM0 TO GT-ACCTNO
                           HOLD-ACCT
                           GL-ACCOUNT.

           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
              GO TO REC-NEW-LOC.

           MOVE "S  A360NEWDESC." TO VDU.
           MOVE GL-DESCRIPTION TO VDUBUF.
           PERFORM SCREENIO.

       END-REC-ENTER-KEYS-MODULE.
           EXIT.


      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF ACTION = "     Y"
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "     N"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           GO TO MODIFY-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ***********************************
       WRITE-MODULE SECTION.
      ***********************************

           MOVE OGT-REC TO GT-REC.
           MOVE GT-BRANCH TO LOG-OLD-BR.
           MOVE GT-ACCTNO TO LOG-OLD-ACCT.
           MOVE GT-LINE   TO LOG-OLD-LINE.
           PERFORM READ-GT1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM DELETE-GT1-FILE.

           MOVE 1 TO HOLD-LINE.
       WRITE-NEW-ACCT.
           MOVE OGT-REC TO GT-REC.

           MOVE HOLD-BRNCH TO GT-BRANCH.
           MOVE HOLD-ACCT TO GT-ACCTNO.
           MOVE HOLD-LINE TO GT-LINE.
           PERFORM WRITE-GT1-FILE.
           IF IO-BAD
              IF E-CODE = "22"
                 ADD 1 TO HOLD-LINE
                 GO TO WRITE-NEW-ACCT.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE GT-DATE1      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO LOG-DATE.

           MOVE HOLD-BRNCH TO LOG-NEW-BR.
           MOVE HOLD-ACCT  TO LOG-NEW-ACCT.
           MOVE HOLD-LINE  TO LOG-NEW-LINE.
           PERFORM LOG-CHANGES.

       END-WRITE-MODULE.
           PERFORM CLOSE-GT1-FILE.

      ************************
       DISPLAY-FIELDS SECTION.
      ************************
           MOVE GT-REF  TO D-010.
           MOVE GT-AMT  TO D-020.
           MOVE GT-EXPL TO D-030.

           MOVE GT-DATE1     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-040.

           MOVE GT-SOURCE    TO D-050.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      ************************
       LOG-CHANGES SECTION.
      ************************
      * CODE TO WRITE CHANGE FILE.
           MOVE "G"              TO ERRLOGW-ACTION.

           MOVE LOG-UPDATE-MSG   TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO GTFIX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GTFIX-SYSDATE.
      *    DISPLAY OMITTED ERASE SCREEN.
      * USING THE LINE 1 COL 1 WORKS MUCH MORE CONSISTENTLY!
      * THE OMITTED LINE WOULD NOT CLEAR ON A F1 FROM THE BOTTOM BUT WOULD
      * CLEAR WHEN PART WAY THRU A NEW RECORD. DIDN'T MAKE SENSE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GTFIX-SCREEN UPON GTFIX-WINDOW-HANDLE.
           MOVE GTFIX-FORM-FIELDS TO WR-BUF.
           MOVE GTFIX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE GTFIX-HELP--FILE TO HELP-LINK-FILE.
           MOVE GTFIX-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GTFIX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
       SET-START-FIELDS.
           MOVE GT-BRANCH TO QGT3-WBEG-BRANCH
                             QGT3-WEND-BRANCH.
           MOVE GT-ACCTNO TO QGT3-WBEG-ACCTNO
                             QGT3-WEND-ACCTNO.

           MOVE GT-DATE3  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3.

           MOVE GT-DRCRFG            TO QGT3-WBEG-DRCRFG.
           MOVE ALL "Z"              TO QGT3-WEND-DRCRFG.
           MOVE GT-LINE              TO QGT3-WBEG-LINE.
           MOVE ALL "9"              TO QGT3-WEND-LINE.

      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
           IF ( GT-BRANCH = 9999 )
              MOVE GT-BRANCH         TO QGT3-WSBEG-BRANCH
           ELSE
              COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1.

           IF ( GT-ACCTNO = 99999999 )
              MOVE GT-ACCTNO         TO QGT3-WSBEG-ACCTNO
           ELSE
              COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1.

           MOVE GT-DATE3             TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3.

           MOVE 01                   TO SQL-TEXT-SIZE.
           MOVE GT-DRCRFG            TO SQL-TEXT
           PERFORM SQL-TEXT-INCREMENT
           MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.

      *************************
       IO-ROUTINES SECTION.
      *************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT3-FILE.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGL/GLGT1IN.CPY".
       COPY "LIBGL/GLGT3RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *************************
       END-ROUTINE SECTION.
      *************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "GL/GLMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GTFIX-SCREEN.
           DESTROY GTFIX-WINDOW-HANDLE.
           EXIT PROGRAM.
