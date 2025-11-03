       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLFX12.
       DATE-WRITTEN.    030289.
      *
      *                 WRITTEN FOR CUSTOMERS WHO UPDATED END OF YEAR
      *                    THEN ADDED AUDIT ENTRIES FOR THAT YEAR.
      *                    FIRST THEY RUN TRANS UPDATE FOR PERIOD 12
      *                    DATES OF LAST YEAR WHICH WILL PUT AMOUNTS
      *                    IN CYBAL(12).  NOW THEY RUN THIS..
      *   DESCRIPTION:  NEED TO ADD CURRENT PERIOD 12 INTO
      *                                 LYR PERIOD 12 AND
      *                 UPDATE OPENING BAL OR RETAINED EARNINGS
      *
      *  MG  000203 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLF 070119 MAKE LOG ENTRY IN /USR/DATA/R1/LG/GL.YYMMDD TO TRACK
      *             WHEN THIS FIX WAS RUN, BY WHOM, & RANGES SELECTED.
      *             REMOVED SELECTION FROM MENU, REPLACED WITH PREVIOUS
      *             YEAR GL UPDATE.  SUPPORT PR# 2780
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/GLFX12.CBL S34 06/10/21-14:43:08 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  BRANCH            PIC 9(4)  VALUE 0.
       01  ERRCD             PIC X VALUE "N".

       01  ACCEPT-BUF           PIC X        VALUE " ".

       01  MESS-LIST         PIC X(5) VALUE "MESS.".
       01  MESS-BUF.
           03  FILLER        PIC X(22).
           03  MESS-NUM      PIC 9(10).

       01  LOG-UPDATE-MSG.
           03  FILLER         PIC X(23) VALUE
               " FIX PERIOD 12 WAS RUN.".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  GLFX12-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLFX12_DEF.CPY".
       COPY "LIBGL/GLFX12_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/GLFX12_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************
       MAIN-PROGRAM SECTION.
      **********************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLFX12" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GLFX12-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.
       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO END-ROUTINE.
           IF ACCEPT-BUF NOT = "Y" GO TO ENTER-ACCEPT.

           PERFORM LOG-CHANGES.

      * ZERO OUT BRANCH WORK RETAIN AMOUNT TO USE FOR HOLDING FILES
           PERFORM OPEN-BR-FILE.
           MOVE 0 TO BR-NO.
           PERFORM START-BR-FILE.

       NEXT-BR.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              GO TO START-GL.

           MOVE 0 TO BR-RETAIN-AMT.
           PERFORM REWRITE-BR-FILE.

           GO TO NEXT-BR.

       START-GL.
           PERFORM CLOSE-BR-FILE.
           PERFORM OPEN-BR-FILE.

           PERFORM OPEN-GL1-FILE.

           MOVE 0 TO GL-BRANCH.
           MOVE 0 TO GL-ACCOUNT.
           PERFORM START-GL1-FILE.

           MOVE "UPDATING GL FILE" TO MESS-BUF.
       NEXT-GL.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO UPDATE-RETAIN.

           MOVE GL-BR-ACCOUNT TO MESS-NUM.
           PERFORM SEND-LEGEND.

           IF GL-CYBAL(12) = 0
      *       UNLOCK GL-FILE
              GO TO NEXT-GL.

           ADD GL-CYBAL(12) TO GL-LYBAL(12).
           IF GL-CLEAR NOT = "Y"
              ADD GL-CYBAL(12) TO GL-OPENBAL
           ELSE
              PERFORM UPDATE-HOLD-BR.
           MOVE 0 TO GL-CYBAL(12).

           PERFORM REWRITE-GL1-FILE.
           GO TO NEXT-GL.

       UPDATE-RETAIN.
           PERFORM OPEN-BR-FILE.
           MOVE 0 TO BR-NO.
           PERFORM START-BR-FILE.

       NEXT-RETAIN.
           PERFORM CLOSE-GL1-FILE.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF BR-RETAIN-AMT = 0
      *       UNLOCK BR-FILE
              GO TO NEXT-RETAIN.

           PERFORM OPEN-GL1-FILE.

           MOVE BR-RETAIN TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              GO TO NEXT-RETAIN.

           IF GL-TYPE = "C"
              MULTIPLY -1 BY BR-RETAIN-AMT.
           ADD BR-RETAIN-AMT TO GL-OPENBAL.

           PERFORM REWRITE-GL1-FILE.

           GO TO NEXT-RETAIN.

      *********************************
       UPDATE-HOLD-BR SECTION.
      *********************************
           PERFORM OPEN-BR-FILE.
           MOVE GL-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH " TO MESS
              PERFORM SEND-MESS
              GO TO END-UPDATE-HOLD-BR.
      * DONT CARE ABOUT CYBAL BECAUSE IT GETS 0 IN MAIN ROUTINE
           IF GL-TYPE = "C"
              MULTIPLY -1 BY GL-CYBAL(12).
           ADD GL-CYBAL(12) TO BR-RETAIN-AMT.
           PERFORM REWRITE-BR-FILE.
       END-UPDATE-HOLD-BR.
           PERFORM CLOSE-BR-FILE.

      ************************************************
       LOG-CHANGES SECTION.
      ************************************************
      * CODE TO WRITE CHANGE FILE.
           MOVE "G"              TO ERRLOGW-ACTION.

           MOVE LOG-UPDATE-MSG   TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO GLFX12-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLFX12-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GLFX12-SCREEN UPON GLFX12-WINDOW-HANDLE.
           MOVE GLFX12-FORM-FIELDS TO WR-BUF.
           MOVE GLFX12-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE GLFX12-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLFX12-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLFX12_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/GBBRIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLFX12-SCREEN.
           DESTROY GLFX12-WINDOW-HANDLE.
           EXIT PROGRAM.
