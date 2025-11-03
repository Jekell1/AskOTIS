       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRANCH.
      ****************************************************
      *                   (GL)
      *
      *       CREATE NEW BRANCH GL RECORDS
      *
      * REV:
      *  MG  000124 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      ****************************************************
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
           "@(#)./GL/BRANCH.CBL S35 09/20/22-13:06:16 >".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD          PIC X      VALUE SPACES.

       01  WHOLE-MESS     PIC X(36).
       01  WHOLE-MESS-RED REDEFINES WHOLE-MESS.
           03  FIRST-PART  PIC X(25).
           03  SECOND-PART PIC 9(11).

       01  NEWBR          PIC 9(4)    VALUE 0.
       01  OLDBR          PIC 9(4)    VALUE 0.

       01  HOLD-GL1-KEY   PIC 9(11)   VALUE 0.
       01  SUB            PIC 99      VALUE 0.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  BRANCH-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/BRANCH_DEF.CPY".
       COPY "LIBGL/BRANCH_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/BRANCH_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************
       MAIN-MODULE SECTION.
      **************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRANCH" TO VDU-FORM-NAME.

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
                   HANDLE IS BRANCH-WINDOW-HANDLE.
       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-BR-FILE.

       ENTER-OLDBR.
           MOVE "EN N040OLDBR." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO END-ROUTINE.
           IF NOT ENTER-KEY GO TO ENTER-OLDBR.
           MOVE VDUNUM0 TO OLDBR BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-OLDBR.

           MOVE "S  A000OBNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       ENTER-NEWBR.
           MOVE "EN N040NEWBR." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-OLDBR.
           IF UP-KEY GO TO ENTER-OLDBR.
           IF F7-KEY GO TO END-ROUTINE.
           IF NOT ENTER-KEY GO TO ENTER-NEWBR.
           MOVE VDUNUM0 TO NEWBR BR-NO.
           IF NEWBR = OLDBR
              GO TO ENTER-NEWBR.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE " BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEWBR.

           MOVE "S  A000NBNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       ENTER-ACCEPT.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-OLDBR.
           IF F7-KEY GO TO END-ROUTINE.
           IF NOT ENTER-KEY GO TO ENTER-ACCEPT.
           IF VDUBUF = "N"
              PERFORM FORM-RESET
              GO TO ENTER-OLDBR.
           IF VDUBUF NOT = "Y"
              GO TO ENTER-ACCEPT.
       SET-INPROGRESS.
           MOVE "S  A000MESS." TO VDU.
           MOVE "* NEW BRANCH G/L CREATION IN PROGRESS *"
              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE OLDBR   TO GL-BRANCH
                           QGL1-WBEG-BRANCH
                           QGL1-WEND-BRANCH.

           MOVE 0       TO GL-ACCOUNT
                           QGL1-WBEG-ACCOUNT.
           MOVE ALL "9" TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.
       READ-GLMASTER.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF GL-BRANCH > OLDBR
              GO TO END-ROUTINE.

           MOVE GL-BR-ACCOUNT TO HOLD-GL1-KEY.

           MOVE NEWBR         TO GL-BRANCH GL-CONSOL.
           MOVE 0             TO GL-OPENBAL GL-LYOPENBAL.

           PERFORM CLEAR-GL-REC
              VARYING SUB FROM 1 BY 1 UNTIL SUB > 12.

           PERFORM WRITE-GL1-FILE.

      ****  THE FOLLOWING DISPLAY SHOULD BE LEFT IN THE PROGRAM!
      ****  UNTIL THE INSPECT TRAP 14 IS FIXED
           IF IO-BAD
              IF E-CODE = "22"
                 MOVE "DUPLICATE GL ACCOUNT # = " TO FIRST-PART
                 MOVE GL-BR-ACCOUNT TO SECOND-PART
                 MOVE WHOLE-MESS TO MESS
                 PERFORM SEND-MESS
              ELSE
                 GO TO IO-ERROR.

           MOVE HOLD-GL1-KEY TO GL-BR-ACCOUNT.
           GO TO READ-GLMASTER.
       CLEAR-GL-REC.
           MOVE 0 TO GL-CYBAL(SUB) GL-LYBAL(SUB) GL-CYBUD(SUB)
                     GL-LYBUD(SUB) GL-NYBUD(SUB).

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO BRANCH-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRANCH-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BRANCH-SCREEN UPON BRANCH-WINDOW-HANDLE.
           MOVE BRANCH-FORM-FIELDS TO WR-BUF.
           MOVE BRANCH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE BRANCH-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRANCH-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/BRANCH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************************************
       MISC-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/ACCESS.CPY".

      **********************************************************
       IO-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **********************************************************
       END-ROUTINE SECTION.
      **********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRANCH-SCREEN.
           DESTROY BRANCH-WINDOW-HANDLE.
           EXIT PROGRAM.
