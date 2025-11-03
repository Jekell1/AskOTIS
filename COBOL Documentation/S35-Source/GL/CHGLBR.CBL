       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHGLBR.
      ******************************************************************
      *
      *  DIR :  GL
      *  DESC:  CHANGE BRANCH # ON GLFILE, GTFILE
      *
      *=================================================================
      * REV:
      *     120615 REPLACE LACCESS/LACCESSW WITH ACCESS/ACCESSW, USES
      *            C$FILEINFO; ACCESS-BUF INCREASED TO 127 VERYANT
      *
      * KEC 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/CHGLBR.CBL J35 12/21/23-08:32:03 >".

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
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".

      *=================================================================
      *   HOLD-GL-REC FROM 'REPLACING LEADING' ON GL-REC.
      *   HOLD-GT-REC FROM 'REPLACING LEADING' ON GT-REC.
      *   HOLD-WR-REC FROM 'REPLACING LEADING' ON WR-REC.
      *=================================================================
       COPY "LIBGL/GL01GL.CPY" REPLACING LEADING "GL" BY "HOLD-GL".
       COPY "LIBGL/GL01GT.CPY" REPLACING LEADING "GT" BY "HOLD-GT".
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "HOLD-WR".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  EXISTING-LOC        PIC 9(4).
       01  NEW-LOC             PIC 9(4).

       01  ALL-BRANCHES            PIC X VALUE "N".

       01  ERRCD                   PIC X VALUE " ".

       01  MESS-LIST               PIC X(5) VALUE "MESS.".

       01  GL-COUNT                PIC 9(6)  VALUE 0.
       01  GT-COUNT                PIC 9(6)  VALUE 0.
       01  DISPLAY-BUF.
           03  DISPLAY-GL              PIC ZZZZZ9.
           03  FILLER                  PIC X(14) VALUE
               " GL MASTERS & ".
           03  DISPLAY-GT              PIC ZZZZZ9.
           03  FILLER                  PIC X(26) VALUE
               " GL TRANS TRANSFERED FROM ".
           03  DISPLAY-EXISTING        PIC 9(4).
           03  FILLER                  PIC X(4) VALUE
               " TO ".
           03  DISPLAY-NEW             PIC 9(4).
           03  FILLER                  PIC X    VALUE ".".

      ******************************************************************
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  CHGLBR-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/CHGLBR_DEF.CPY".
       COPY "LIBGL/CHGLBR_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*****************************************
      *                                        *
      *     P R O G R A M   C O N T R O L      *
      *                                        *
      ******************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/CHGLBR_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
 
      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHGLBR" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CHGLBR-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.
       MAIN-MODULE.
      D    STOP MESS.
           PERFORM INITIALIZATION.

           PERFORM CHECK-EXISTING-RECORDS.
           IF ERRCD = "Y"
              MOVE "GL BRANCH NUMBER CHANGE WAS NOT DONE." TO MESS
              PERFORM SEND-MESS
              GO TO END-PROG.

           PERFORM TRANSFER-RECORDS.

           PERFORM SEND-MSG.
           MOVE "GL BRANCH # CHANGE COMPLETED !" TO MESS.
           PERFORM SEND-MESS.
           GO TO MAIN-MODULE.

      *********************************************
       INITIALIZATION SECTION.
      *********************************************
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-GT1-FILE.

       E-LOC1.
           MOVE "EN N040LOC1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY GO TO E-LOC1.
           MOVE VDUNUM0 TO EXISTING-LOC.
       E-NEWLOC1.
           MOVE "N" TO ALL-BRANCHES.
           MOVE "EN N040NEWLOC1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO E-LOC1.
           IF UP-KEY GO TO E-LOC1.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY GO TO E-NEWLOC1.
           MOVE VDUNUM0 TO NEW-LOC.
       ACCEPT-IT.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO END-PROGRAM.
           IF F1-KEY GO TO E-LOC1.
           IF UP-KEY GO TO E-NEWLOC1.
           IF VDUBUF = "Y" GO TO EXIT-INITIALIZE.
           IF VDUBUF = "N" GO TO E-LOC1.
           GO TO ACCEPT-IT.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************
       CHECK-EXISTING-RECORDS SECTION.
      *********************************************

      * CHECK FOR EXISTING RECORDS IN GL, GT, WR WITH
      * NEW (TARGET) BRANCH NUMBER           

           MOVE " "     TO ERRCD.

           MOVE NEW-LOC TO GL-BRANCH
                           QGL1-WBEG-BRANCH
                           QGL1-WEND-BRANCH.
           MOVE 0       TO GL-ACCOUNT
                           QGL1-WBEG-ACCOUNT.
           MOVE ALL "9" TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.
           PERFORM READ-GL1-FILE-NEXT.
      *    UNLOCK GL-FILE.
           IF IO-BAD
              GO TO CHECK-GT.

           IF GL-BRANCH = NEW-LOC
              MOVE "GL MASTERS EXIST FOR TARGET BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "Y" TO ERRCD
              GO TO EXIT-CHECK-EXISTING.

       CHECK-GT.
           MOVE NEW-LOC  TO GT-BRANCH
                            QGT3-WBEG-BRANCH
                            QGT3-WEND-BRANCH.
           MOVE 0        TO GT-ACCTNO
                            QGT3-WBEG-ACCTNO
           MOVE ALL "9"  TO QGT3-WEND-ACCTNO.

           MOVE 19000101 TO GT-DATE3 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3.

           MOVE " "      TO GT-DRCRFG
                            QGT3-WBEG-DRCRFG.
           MOVE ALL "Z"  TO QGT3-WEND-DRCRFG.
           MOVE 0        TO GT-LINE
                            QGT3-WBEG-LINE.
           MOVE ALL "9"  TO QGT3-WEND-LINE.

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
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3

           MOVE 01                   TO SQL-TEXT-SIZE
           MOVE GT-DRCRFG            TO SQL-TEXT
           PERFORM SQL-TEXT-INCREMENT
           MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.

           PERFORM START-GT3-FILE.

           PERFORM READ-GT3-FILE-NEXT.
      *    UNLOCK GT-FILE.
           IF IO-BAD
              GO TO EXIT-CHECK-EXISTING.

           IF GT-BRANCH = NEW-LOC
              MOVE "GL TRANSACTIONS EXIST FOR TARGET BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "Y" TO ERRCD
              GO TO EXIT-CHECK-EXISTING.

       EXIT-CHECK-EXISTING.
           EXIT.

      *********************************************
       TRANSFER-RECORDS SECTION.
      *********************************************

      * CHANGE BRANCH # IN GLFILE, GTFILE         

           MOVE EXISTING-LOC  TO GT-BRANCH
                                 QGT3-WBEG-BRANCH
                                 QGT3-WEND-BRANCH.
           MOVE 0        TO GT-ACCTNO
                            QGT3-WBEG-ACCTNO
           MOVE ALL "9"  TO QGT3-WEND-ACCTNO.

           MOVE 19000101 TO GT-DATE3 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3.

           MOVE " "      TO GT-DRCRFG
                            QGT3-WBEG-DRCRFG.
           MOVE ALL "Z"  TO QGT3-WEND-DRCRFG.
           MOVE 0        TO GT-LINE
                            QGT3-WBEG-LINE.
           MOVE ALL "9"  TO QGT3-WEND-LINE.

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
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3

           MOVE 01                   TO SQL-TEXT-SIZE
           MOVE GT-DRCRFG            TO SQL-TEXT
           PERFORM SQL-TEXT-INCREMENT
           MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.

           PERFORM START-GT3-FILE.
       NEXT-GT.
           PERFORM READ-GT3-FILE-NEXT.
           IF IO-BAD
              GO TO START-GL.

           IF GT-BRANCH NOT = EXISTING-LOC
              GO TO START-GL.

           MOVE GT-REC  TO HOLD-GT-REC.
           MOVE NEW-LOC TO GT-BRANCH.
           PERFORM WRITE-GT1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           ADD 1 TO GT-COUNT.

           MOVE HOLD-GT-REC TO GT-REC.
           PERFORM READ-GT1-FILE.
           PERFORM DELETE-GT1-FILE.

      * I DONT THINK THE DATABASE NEEDS THIS AS THERE ARE DIFFERENT CURSORS
      *    MOVE HOLD-GT-REC TO GT-REC.
      *    PERFORM START-GT3-FILE.

           GO TO NEXT-GT.

       START-GL.
           MOVE EXISTING-LOC TO GL-BRANCH
                                QGL1-WBEG-BRANCH
                                QGL1-WEND-BRANCH.
           MOVE 0       TO GL-ACCOUNT
                           QGL1-WBEG-ACCOUNT.
           MOVE ALL "9" TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.
       NEXT-GL.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
      *       UNLOCK GL-FILE
              GO TO EXIT-TRANSFER-RECORDS.

           IF GL-BRANCH NOT = EXISTING-LOC
      *       UNLOCK GL-FILE
              GO TO EXIT-TRANSFER-RECORDS.

           MOVE GL-REC TO HOLD-GL-REC.
           MOVE NEW-LOC TO GL-BRANCH.
           PERFORM WRITE-GL1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           ADD 1 TO GL-COUNT.
           MOVE HOLD-GL-REC TO GL-REC.
           PERFORM READ-GL1-FILE.
           PERFORM DELETE-GL1-FILE.

      * I DONT THINK WE NEED THIS WITH THE DATABASE
      *    MOVE HOLD-GL-REC TO GL-REC.
      *    PERFORM START-GL1-FILE.

           GO TO NEXT-GL.

       EXIT-TRANSFER-RECORDS.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO CHGLBR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CHGLBR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHGLBR-SCREEN UPON CHGLBR-WINDOW-HANDLE.
           MOVE CHGLBR-FORM-FIELDS TO WR-BUF.
           MOVE CHGLBR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE CHGLBR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHGLBR-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/CHGLBR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************************
       MISC-PARAGRAPHS SECTION.
      **********************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       SEND-MSG.
           MOVE GL-COUNT TO DISPLAY-GL.
           MOVE GT-COUNT TO DISPLAY-GT.
           MOVE EXISTING-LOC TO DISPLAY-EXISTING.
           MOVE NEW-LOC TO DISPLAY-NEW.
           MOVE MESS-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      **********************************************************
       IO-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT3-FILE.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GL1-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGL/GLGT3RN.CPY".
       COPY "LIBGL/GLGT1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **********************************************************
       END-PROGRAM SECTION.
      **********************************************************
       END-PROG.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHGLBR-SCREEN.
           DESTROY CHGLBR-WINDOW-HANDLE.
           EXIT PROGRAM.
