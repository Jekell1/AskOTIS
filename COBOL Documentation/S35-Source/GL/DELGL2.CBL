       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DELGL2.
       AUTHOR. MJD.
       DATE-WRITTEN. 122795.
      *****************************************************************
      *               DELETE A GL # FOR ALL BRANCHES OR A
      *             GROUP OF GL'S FOR ALL BRANCHES.
      * THIS IS A DESTRUCTIVE VERSION OF DELGL1 - NOT ON MENU,
      *
      * DELETES GL MASTERS REGARDLESS OF BALANCE, USE WITH CAUTION! BLV
      *
      * REV:
      * BLV 080897 BROUGHT OVER FROM A90
      * BAH 040298 ADDED EXITPROG, CALLED FROM INSTMU AND GLMNMU
      * MG  000125 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./GL/DELGL2.CBL S35 09/20/22-14:25:47 >".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  ERRCD              PIC X         VALUE SPACES.
       01  ACCEPT-BUF         PIC X         VALUE SPACES.

       01  MESS-LIST          PIC X(5)      VALUE
           "MESS.".

       01  BEG-BRANCH         PIC 9(4)      VALUE 0.
       01  END-BRANCH         PIC 9(4)      VALUE 0.
       01  BEG-ACCOUNT        PIC 9(7).
       01  END-ACCOUNT        PIC 9(7).
       01  CUR-BAL            PIC S9(11)V99 COMP-3 VALUE 1.
       01  LYCUR-BAL          PIC S9(11)V99 COMP-3 VALUE 1.
       01  SKIPPED-ACCOUNTS   PIC X         VALUE "N".

       01  IO-TYPE            PIC X(3)      VALUE SPACES.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  DELGL2-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/DELGL2_DEF.CPY".
       COPY "LIBGL/DELGL2_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/DELGL2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DELGL2" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS DELGL2-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.
           PERFORM INITIALIZATION.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           MOVE "N" TO SKIPPED-ACCOUNTS.

           PERFORM UPDATE-GL-FILE.

           IF SKIPPED-ACCOUNTS = "Y"
              MOVE "SOME ACTIVE ACCOUNTS WERE SKIPPED" TO MESS
              PERFORM SEND-MESS.

           GO TO END-ROUTINE.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE SPACES TO IO-TYPE.
      *----------------------------------------------------------------
       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BRANCH.
      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF UP-KEY GO TO BR-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO ACCT-01.
      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       ACCT-01.
           MOVE "ENNN070GL1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF UP-KEY GO TO BR-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO ACCT-01.
           MOVE VDUNUM0 TO BEG-ACCOUNT.
      *----------------------------------------------------------------
       ACCT-02.
           MOVE "ENNN070GL2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ACCT-01.
           IF UP-KEY GO TO ACCT-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO BR-02.
           MOVE VDUNUM0 TO END-ACCOUNT.
           IF BEG-ACCOUNT > END-ACCOUNT
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ACCT-01.
      *----------------------------------------------------------------
       ACCEPT-SCREEN-DATA.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF UP-KEY GO TO ACCT-02.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY
              GO TO ACCEPT-SCREEN-DATA.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO BR-01.
           IF ACCEPT-BUF NOT = "Y"
              GO TO ACCEPT-SCREEN-DATA.

           MOVE "GL/MASTER ACCOUNT CLEAR IN PROGRESS"
               TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-GL1-FILE.

           GO TO EXIT-INITIALIZATION.

       ENDIT.
           MOVE "END" TO IO-TYPE.
       EXIT-INITIALIZATION.
           EXIT.

      *****************************************************************
       UPDATE-GL-FILE SECTION.
      *****************************************************************
           MOVE BEG-BRANCH  TO GL-BRANCH
                               QGL1-WBEG-BRANCH.
           MOVE BEG-ACCOUNT TO GL-ACCOUNT
                               QGL1-WBEG-ACCOUNT.
           MOVE END-BRANCH  TO QGL1-WEND-BRANCH.
           MOVE END-ACCOUNT TO QGL1-WEND-ACCOUNT.
           PERFORM START-GL1-FILE.

       UPDATE-GLMASTER.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO END-UPDATE-GL.

           IF GL-BRANCH > END-BRANCH
              GO TO END-UPDATE-GL.

           IF GL-BRANCH < BEG-BRANCH
      *       UNLOCK GL-FILE
              GO TO UPDATE-GLMASTER.

           IF GL-ACCOUNT > END-ACCOUNT OR  GL-ACCOUNT < BEG-ACCOUNT
      *       UNLOCK GL-FILE
              GO TO UPDATE-GLMASTER.

           PERFORM SET-CURRBAL.
           PERFORM SET-LYCURRBAL.


           PERFORM DELETE-GL1-FILE.

           GO TO UPDATE-GLMASTER.

       END-UPDATE-GL.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO DELGL2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DELGL2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DELGL2-SCREEN UPON DELGL2-WINDOW-HANDLE.
           MOVE DELGL2-FORM-FIELDS TO WR-BUF.
           MOVE DELGL2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE DELGL2-HELP--FILE TO HELP-LINK-FILE.
           MOVE DELGL2-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/DELGL2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************************
       MISC-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SET-CURRBAL.
           COMPUTE VDUNUM2 CUR-BAL = GL-OPENBAL
                     + GL-CYBAL(1)  + GL-CYBAL(2)  + GL-CYBAL(3)
                     + GL-CYBAL(4)  + GL-CYBAL(5)  + GL-CYBAL(6)
                     + GL-CYBAL(7)  + GL-CYBAL(8)  + GL-CYBAL(9)
                     + GL-CYBAL(10) + GL-CYBAL(11) + GL-CYBAL(12).
       SET-LYCURRBAL.
           COMPUTE LYCUR-BAL = GL-LYOPENBAL
                     + GL-LYBAL(1)  + GL-LYBAL(2)  + GL-LYBAL(3)
                     + GL-LYBAL(4)  + GL-LYBAL(5)  + GL-LYBAL(6)
                     + GL-LYBAL(7)  + GL-LYBAL(8)  + GL-LYBAL(9)
                     + GL-LYBAL(10) + GL-LYBAL(11) + GL-LYBAL(12).

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DELGL2-SCREEN.
           DESTROY DELGL2-WINDOW-HANDLE.
           EXIT PROGRAM.
