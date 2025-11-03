       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHBRNO.
      ******************************************************************
      *  DIR :  GL
      *  DESC:  CHANGE BRANCH # ON GLFILE ONLY
      *  REV :
      *    121011 COPIED FROM CHGLBR AND CHANGED TO ONLY FIX GLFILE
      *                                                   CITIZENS, PR# 4128
      * KEC 2023-0127 FOR S35, COMMENTED OUT THE USE OF BRFILE AND
      *               SQL COPY MEMBERS; NOT NEEDED
      *
      *****************************************************************
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
           "@(#)GL/CHBRNO.CBL S35 01/27/23-14:31:36 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
      *COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

      *=================================================================
      *   HOLD-GL-REC FROM 'REPLACING LEADING' ON GL-REC.
      *=================================================================
       COPY "LIBGL/GL01GL.CPY" REPLACING LEADING "GL" BY "HOLD-GL".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  EXISTING-LOC        PIC 9(4).
       01  NEW-LOC             PIC 9(4).

       01  ALL-BRANCHES            PIC X VALUE "N".

       01  ERRCD                   PIC X VALUE " ".

       01  MESS-LIST               PIC X(5) VALUE "MESS.".

       01  GL-COUNT                PIC 9(6)  VALUE 0.
       01  DISPLAY-BUF.
           03  DISPLAY-GL              PIC ZZZZZ9.
           03  FILLER                  PIC X(12) VALUE
               " GL MASTERS ".
           03  FILLER                  PIC X(16) VALUE
               "TRANSFERED FROM ".
           03  DISPLAY-EXISTING        PIC 9(4).
           03  FILLER                  PIC X(4) VALUE
               " TO ".
           03  DISPLAY-NEW             PIC 9(4).
           03  FILLER                  PIC X(24) VALUE ".".


      ******************************************************************
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
      *COPY "LIBGB/LACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  CHBRNO-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/CHBRNO_DEF.CPY".
       COPY "LIBGL/CHBRNO_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/CHBRNO_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
       MAIN-PROGRAM SECTION.
      ************************************
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHBRNO" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CHBRNO-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM INITIALIZATION.

           PERFORM OPEN-GL1-FILE.

           PERFORM CHECK-EXISTING-RECORDS.
           IF ERRCD = "Y"
              MOVE "GL BRANCH NUMBER RECORDS EXIST" TO MESS
              PERFORM SEND-MESS
              MOVE "GL BRANCH NUMBER CHANGE WAS NOT DONE." TO MESS
              PERFORM SEND-MESS
              GO TO END-PROG.

           PERFORM TRANSFER-RECORDS.

           PERFORM SEND-MSG.
           MOVE "GL BRANCH # CHANGE COMPLETED !" TO MESS.
           PERFORM SEND-MESS.
           GO TO MAIN-MODULE.

      ********************************
       INITIALIZATION SECTION.
      ********************************
       E-LOC1.
           MOVE "EN N040LOC1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO E-LOC1.
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

      * CHECK FOR EXISTING RECORDS IN GLFILE  WITH NEW
      * (TARGET) BRANCH NUMBER          

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
              GO TO EXIT-CHECK-EXISTING.

           IF GL-BRANCH = NEW-LOC
              MOVE "GL MASTERS EXIST FOR TARGET BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "Y" TO ERRCD
              GO TO EXIT-CHECK-EXISTING.

       EXIT-CHECK-EXISTING.
           EXIT.

      *********************************************
       TRANSFER-RECORDS SECTION.
      *********************************************

      * CHANGE BRANCH # IN GLFILE         

           MOVE EXISTING-LOC TO GL-BRANCH
                                QGL1-WBEG-BRANCH
                                QGL1-WEND-BRANCH.
           MOVE 0            TO GL-ACCOUNT
                                QGL1-WBEG-ACCOUNT.
           MOVE ALL "9"      TO QGL1-WEND-ACCOUNT.

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

      * DONT THINK DATABASE NEEDS THIS
      *    MOVE HOLD-GL-REC TO GL-REC.
      *    PERFORM START-GL1-FILE.
           GO TO NEXT-GL.

       EXIT-TRANSFER-RECORDS.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO CHBRNO-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CHBRNO-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHBRNO-SCREEN UPON CHBRNO-WINDOW-HANDLE.
           MOVE CHBRNO-FORM-FIELDS TO WR-BUF.
           MOVE CHBRNO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE CHBRNO-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHBRNO-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/CHBRNO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************
       MISC-PARAGRAPHS SECTION.
      **************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       SEND-MSG.
           MOVE GL-COUNT TO DISPLAY-GL.
           MOVE EXISTING-LOC TO DISPLAY-EXISTING.
           MOVE NEW-LOC TO DISPLAY-NEW.
           MOVE MESS-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************
       END-PROGRAM SECTION.
      *******************************
       END-PROG.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHBRNO-SCREEN.
           DESTROY CHBRNO-WINDOW-HANDLE.
           EXIT PROGRAM.
