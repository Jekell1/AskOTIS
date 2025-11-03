       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ALMAIN.
       DATE-WRITTEN.    072186.
      *****************************************************
      *  DIR : GL
      *  DESC: AUTO G/L ALLOCATION MAINTENANCE
      *  REV:
      *   052495 BLL CHANGE KEY FROM 16 CHARS TO 12
      *   102997 GLF REMOVED RESET OF FIELDS
      *   032898 BLV ADD EXITPROG (CALLED FROM LPM2MU & GLMNMU)
      *   000124  MG FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *   161227 BLM CHANGE ORIG & CHANGED REC TO AL-SIZE
      *   190514 MOVE 0 TO AL-FUTURE-NUMERICS, NO CLEAR ROUTINE
      * BAH 20190627 FIXED AL-FUTURE-NUMERIC CLEAR
      *
      * JKC 2023-0127 FOR G35 & S35, REMOVED IO FOR BRFILE.
      *
      *****************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GL/ALMAIN S35 02/01/24-13:10:02 >".
       COPY "LIBGL/GL01AL.CPY".
       COPY "LIBGL/GL01AL_SQL.CPY".
       COPY "LIBGL/GLWSAL.CPY".
      *COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
      *COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  SUB             PIC 99.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  CHAN            PIC X(4).
       01  NEW-FLAG        PIC 9.
       01  HOLD-KEY        PIC X(12).
       01  ORIGINAL-REC    PIC X(AL-SIZE).
       01  CHANGED-REC     PIC X(AL-SIZE).
       01  DELETE-FLAG     PIC X VALUE "Y".
       01  WORK-GLNO1.
           03  WORK-BRANCH  PIC 9(4).
           03  WORK-ACCOUNT PIC 9(7).
       01  WORK-GLNO REDEFINES WORK-GLNO1  PIC 9(11).
       01  VERIFY-BRNO     PIC 9(4) VALUE 0.

       01  DISPLAY-FLAG       PIC X VALUE "N".

       01  ERRCD              PIC X.
       01  TOTAL-PERCENT   PIC S9(6)V99.

      ***************************************
      *     SCREEN DISPLAY LIST             *
      ***************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03 D-TYPE            PIC X.
           03 D-DESC            PIC X(30).
           03 D-DISTRIBUTIONS OCCURS 10 TIMES.
              05  D-BRANCH       PIC 9999.
              05  D-ACCOUNT      PIC 9(7).
              05  D-PERCENT      PIC ZZ9.99-.
              05  D-GLDESC       PIC X(28).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)  VALUE
           "010 020 030 03A 03B 03C 040 04A 04B 04C ".
           03  FILLER      PIC X(48)  VALUE
           "050 05A 05B 05C 060 06A 06B 06C 070 07A 07B 07C ".
           03  FILLER      PIC X(48)  VALUE
           "080 08A 08B 08C 090 09A 09B 09C 100 10A 10B 10C ".
           03  FILLER      PIC X(32)  VALUE
           "110 11A 11B 11C 120 12A 12B 12C.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 12.
       01  SPEC-TABLE.
      * 1. TYPE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. DESCRIPTION
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 3. BRANCH 1
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 4. BRANCH 2
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 5. BRANCH 3
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 6. BRANCH 4
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 7. BRANCH 5
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 8. BRANCH 6
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 9. BRANCH 7
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *10. BRANCH 8
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *11. BRANCH 9
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *12. BRANCH 10
           03  FILLER      PIC X(8) VALUE "ENNN040 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 12 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGL/ALSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGL/GLSCANW.CPY".
       01  ALMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/ALMAIN_DEF.CPY".
       COPY "LIBGL/ALMAIN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      *********************************
      *    MAIN PROGRAM MODULE        *
      *********************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/ALMAIN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************
       MAIN-PROGRAM SECTION.
      **************************************

           PERFORM SQL-CONNECT.

           MOVE "ALMAIN" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ALMAIN-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE "SN N040BRNO." TO VDU.
           MOVE EXT-CONAME-USER-LOC TO VERIFY-BRNO VDUNUM0.
           PERFORM SCREENIO.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE 0 TO NEW-FLAG.
           PERFORM REC-AL-KEY.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
       ENTER-THE-RECORD.
           PERFORM ENTRY-MODULE.
           IF AL-TYPE = "P"
              PERFORM VERIFY-100PERCENT.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF AL-TYPE = "P"
              IF TOTAL-PERCENT NOT = 0
                 GO TO ENTER-THE-RECORD.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           GO TO MAIN-MODULE.

      *******************************
       REC-AL-KEY SECTION.
      *******************************
           GO TO REC-AL-KEY-NO.
       ENTER-VERIFY-BRANCH.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO VERIFY-BRNO.
       REC-AL-KEY-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE "E  A120ID." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-VERIFY-BRANCH.
           IF F6-KEY
              MOVE 10 TO ALSCAN-MAX-ACCTS
      *       MOVE "ID." TO WINDOW-FIT-TO
              MOVE VDUBUF TO ALSCAN-KEY
              MOVE "WI/ALSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH ALSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF ALSCAN-STATUS NOT = "00"
                 GO TO REC-AL-KEY-NO
              ELSE
                 MOVE "S  A000ID." TO VDU
                 MOVE ALSCAN-KEY TO VDUBUF AL1-KEY
                 PERFORM SCREENIO
                 GO TO READ-ALLOC-FILE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-AL-KEY.
           IF NOT ENTER-KEY
              GO TO REC-AL-KEY-NO.
           IF VDUBUF = " "
              GO TO REC-AL-KEY-NO.
           MOVE VDUBUF TO AL1-KEY.
       READ-ALLOC-FILE.
           MOVE AL1-KEY TO HOLD-KEY.
           PERFORM OPEN-AL1-FILE.
           PERFORM READ-AL1-FILE.
           PERFORM CLOSE-AL1-FILE.
           IF IO-BAD
              PERFORM NEW-RECORD-SETUP
           ELSE
              MOVE AL-REC TO ORIGINAL-REC
              PERFORM DISPLAY-FIELDS.
       END-REC-AL-KEY.
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
              MOVE 1 TO ELE.
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
           INSPECT ACTION REPLACING ALL SPACES BY ZERO.
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
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F4.
           IF UP-KEY
              MOVE -1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F6.
           IF NOT F6-KEY
              GO TO TEST-F5.
           GO TO GOTO-FIELD.
       TEST-F5.
           IF F5-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
           IF NOT ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-GL F-GL F-GL F-GL F-GL F-GL F-GL F-GL
                 F-GL F-GL DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ***************************************
      *      SET RECORD ELEMENTS            *
      ***************************************

       F-01.
           MOVE VDUBUF TO AL-TYPE.
           IF AL-TYPE NOT = "A" AND AL-TYPE NOT = "P"
              MOVE "ENTER 'A-AMOUNT' OR 'P-PERCENT'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO AL-DESC.
           GO TO CREATE-ROUTINE.
       F-GL.
           COMPUTE SUB = ELE - 2.
           IF NOT F6-KEY
              GO TO CONTINUE-BRANCH.

           MOVE SPACES TO GLSCAN-KEY.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-KEY = SPACES
              GO TO ENTER-ELE.

           MOVE "SN N040000." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE GLSCAN-BRANCH TO VDUNUM0 WORK-BRANCH.
           PERFORM SCREENIO.
           MOVE "SN N07000A." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-ACCOUNT TO VDUNUM0 WORK-ACCOUNT.
           PERFORM SCREENIO.
           GO TO SKIP-TO-VERIFY.
       CONTINUE-BRANCH.
           MOVE VDUNUM0 TO WORK-BRANCH.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF UP-KEY
               SUBTRACT 1 FROM ELE
               GO TO CREATE-ROUTINE.
       F-ACCT.
           MOVE "ENNN07000A." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-GL.
           MOVE VDUNUM0 TO WORK-ACCOUNT.
           IF WORK-ACCOUNT = 0
               MOVE 0 TO AL-ACCT(SUB)
               MOVE 0 TO AL-PCT(SUB)
               MOVE MAX TO ELE
               GO TO CREATE-ROUTINE.
       SKIP-TO-VERIFY.
           PERFORM VERIFY-GL-ACCOUNT.
           IF IO-BAD
               SUBTRACT 1 FROM ELE
               GO TO CREATE-ROUTINE.
           MOVE WORK-GLNO TO AL-ACCT(SUB).
       F-PERCENT.
           IF AL-TYPE = "A"
              MOVE "S  S03200B." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO AL-PCT(SUB) VDUNUM2
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           MOVE "E  S03200B." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY
               SUBTRACT 1 FROM ELE
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO AL-PCT(SUB).
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *********************************
       WRITE-MODULE SECTION.
      *********************************
           PERFORM CHECK-NO-ACCTS.
           PERFORM OPEN-AL1-FILE.
           MOVE HOLD-KEY TO AL1-KEY.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE AL-REC TO CHANGED-REC.
           IF ORIGINAL-REC = CHANGED-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-AL1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF AL-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-REC TO AL-REC.
           PERFORM REWRITE-AL1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-AL1-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-AL1-FILE.

      *********************************
       DELETE-MODULE SECTION.
      *********************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-AL1-FILE.
           PERFORM READ-AL1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF AL-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           IF DELETE-FLAG NOT = "N"
      *       UNLOCK AL-FILE
              PERFORM DELETE-AL-FILE-REC
           ELSE
              MOVE "Y" TO DELETE-FLAG.
       END-DELETE-MODULE.
           PERFORM CLOSE-AL1-FILE.

      *********************************
       CHECK-NO-ACCTS SECTION.
      *********************************
           MOVE 0 TO SUB AL-NO-ACCTS.
       ACCT-COUNT-LOOP.
           ADD 1 TO SUB.
           IF SUB > 10 GO TO EXIT-CHECK-ACCTS.
           IF AL-TYPE = "A"
              IF AL-ACCT(SUB) NOT = 0
                 ADD 1 TO AL-NO-ACCTS.
           IF AL-TYPE = "P"
              IF AL-PCT(SUB) NOT = 0
                 ADD 1 TO AL-NO-ACCTS.
           GO TO ACCT-COUNT-LOOP.
       EXIT-CHECK-ACCTS.
           EXIT.

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-AL-FILE.

           MOVE HOLD-KEY TO AL1-KEY.
           MOVE 0 TO ELE.

      ****************************
       DISPLAY-FIELDS SECTION.
      ****************************
           MOVE 1 TO SUB.
           MOVE "Y" TO DISPLAY-FLAG.
           MOVE SPACES TO DISPLAY-BUF.
       MOVE-DISTRIBUTIONS.
           MOVE AL-ACCT(SUB) TO WORK-GLNO.
           MOVE WORK-BRANCH TO D-BRANCH(SUB).
           MOVE WORK-ACCOUNT TO D-ACCOUNT(SUB).
           MOVE SUB TO ELE.
           IF WORK-GLNO NOT = 0
              PERFORM VERIFY-GL-ACCOUNT.

           MOVE AL-PCT(SUB) TO D-PERCENT(SUB).
           IF WORK-GLNO NOT = 0
              MOVE GL-DESCRIPTION TO D-GLDESC(SUB).
           ADD 1 TO SUB.
           IF SUB < 11
               GO TO MOVE-DISTRIBUTIONS.

           MOVE AL-TYPE TO D-TYPE.
           MOVE AL-DESC TO D-DESC.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "N" TO DISPLAY-FLAG.

      *****************************
       VERIFY-GL-ACCOUNT SECTION.
      *****************************

           MOVE 0 TO IO-FG.
           PERFORM OPEN-GL1-FILE.
           MOVE WORK-GLNO TO GL-BR-ACCOUNT.
           IF WORK-BRANCH = 0
              MOVE VERIFY-BRNO TO GL-BRANCH.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
               MOVE "** INVALID ACCOUNT **" TO GL-DESCRIPTION.
       SKIP-READ-GL.
           MOVE "S  A30000C." TO VDU.
           MOVE GL-DESCRIPTION TO VDUBUF.
           MOVE ELE TO VDUROW.
           IF DISPLAY-FLAG NOT = "Y"
              PERFORM SCREENIO.

      ******************************
       VERIFY-100PERCENT SECTION.
      ******************************
           MOVE 100 TO TOTAL-PERCENT.
           MOVE 1 TO SUB.
       PERCENT-LOOP.
           SUBTRACT AL-PCT(SUB) FROM TOTAL-PERCENT.
           ADD 1 TO SUB.
           IF SUB < 11
               GO TO PERCENT-LOOP.
           IF TOTAL-PERCENT NOT = 0
              MOVE "PERCENT DISTRIBUTION MUST TOTAL 100.00%" TO MESS
              PERFORM SEND-MESS.

      *******************************
       DELETE-AL-FILE-REC SECTION.
      *******************************
           PERFORM READ-AL1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-AL-FILE.

           PERFORM DELETE-AL1-FILE.

       END-DELETE-AL-FILE.
           PERFORM CLOSE-AL1-FILE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGL/GLALCL.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO ALMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ALMAIN-SYSDATE.
      *    DISPLAY OMITTED ERASE SCREEN.
      * USING THE LINE 1 COL 1 WORKS MUCH MORE CONSISTENTLY!
      * THE OMITTED LINE WOULD NOT CLEAR ON A F1 FROM THE BOTTOM BUT WOULD
      * CLEAR WHEN PART WAY THRU A NEW RECORD. DIDN'T MAKE SENSE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ALMAIN-SCREEN UPON ALMAIN-WINDOW-HANDLE.
           MOVE ALMAIN-FORM-FIELDS TO WR-BUF.
           MOVE ALMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE ALMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ALMAIN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/ALMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-YN.
           MOVE " Y OR N !" TO MESS.
           PERFORM SEND-MESS.
       BRANCH-OVER-LIMIT.
           MOVE " EXCEEDS GLOBAL LOCATION LIMIT" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " RECORD HAD CHANGES MADE TO IT!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                    TO MESS.
           PERFORM SEND-MESS.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-AL1-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLALGS_SQL.CPY".
       COPY "LIBGL/GLAL1IN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *******************************
       END-ROUTINE SECTION.
      *******************************
       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALMAIN-SCREEN.
           DESTROY ALMAIN-WINDOW-HANDLE.
           MOVE EXIT-PATHNAME TO FORM-PATHNAME.
           EXIT PROGRAM.
