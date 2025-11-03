       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLDUP.
      ******************************************************************
      *  DIR :  GL
      *  DESC:  GENERAL LEDGER ACCTNO DUP TO OTHER BRANCHES
      *  REV :
      *         012495 BLL ADD ACCOUNT # RANGE, TARGET BRNO RANGE
      *         000203 MG  FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT
      *                MEETING.
      *         070404 BLF REMOVE UPDATING GL-TYPE FROM ROUTINE TO
      *                FIX MISC FIELDS - IF CHANGING FROM A DEBIT TO
      *                CREDIT OR VICE VERSA, MUST MULTIPLY OPENING BALS
      *                & ALL PERIOD BALS BY -1!!  MAY ALLOW THIS IN A
      *                SEPARATE FIX RUN FROM $, GL/FXTYPE.
      *         080416 BLM FIX BUG FROM ABOVE!!  I DELETED HOLD-GL-TYPE
      *                SO IF YOU ANSWERED "Y" TO FIX MISC. FIELDS, IT
      *                WOULD BE COPYING GL-TYPE TO GL-CLEAR, SO INSTEAD
      *                OF GL-CLEAR BEING Y OR N, IT WOULD BE D OR C, &
      *                THIS WILL MESS UP END-OF-YEAR!!!
      *         090324 BLM FIX TO MOVE SPANISH DESCRIPTION TO TARGET
      *                ON EXISTING GL, WORLD PR# 616
      *         120620 REPLACED LACCESS/LACCESSW WITH ACCESS/ACCESSW, USES
      *                C$FILEINFO   VERYANT
      * BAH 151204 INCREASED RECORD SIZE, PD0005
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
       77  SCCS-IDENTIFIER PIC X(42) VALUE
           "@(#)./GL/GLDUP.CBL S34 06/10/21-14:40:16 >".
      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *=================================================================
      *   HOLD-GL-REC FROM 'REPLACING LEADING' ON GL-REC.
      *=================================================================
       COPY "LIBGL/GL01GL.CPY" REPLACING LEADING "GL" BY "HOLD-GL".

       01  SAVE-GL1-KEY     PIC X(11).

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  GL-RANGES.
           03  EXISTING-LOC        PIC 9(4).
           03  ACCT1               PIC 9(7).
           03  ACCT2               PIC 9(7).
           03  NEW-LOC1            PIC 9(4).
           03  NEW-LOC2            PIC 9(4).
           03  FIX-MISC-FG         PIC X.

       01  ALL-BRANCHES            PIC X VALUE "N".
       01  ALL-ACCTS               PIC X VALUE "N".

       01  ERRCD                   PIC X.
       01  SUB                     PIC 99 VALUE 0.

       01  DISPLAY-BUF.
           03  FILLER                  PIC X(19) VALUE
               "CREATING GL ACCT # ".
           03  DISPLAY-ACCT            PIC 9(7).
           03  FILLER                  PIC X(14) VALUE
               " FOR BRANCH # ".
           03  DISPLAY-BRANCH          PIC 9(4).
           03  FILLER                  PIC X     VALUE ".".
           03  FILLER                  PIC X(3)  VALUE SPACES.
           03  DISPLAY-COUNT           PIC 9(6)  VALUE 0.
           03  FILLER                  PIC X(17) VALUE
               " RECORDS CREATED.".

       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  GLDUP-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLDUP_DEF.CPY".
       COPY "LIBGL/GLDUP_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/GLDUP_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************
       MAIN-PROGRAM SECTION.
      ******************************************
           PERFORM SQL-CONNECT.
           MOVE "GLDUP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GLDUP-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
       MAIN-MODULE.
           PERFORM FORM-RESET.
           PERFORM INITIALIZATION.

           PERFORM CREATE-GL-MASTER.

           MOVE "DUPLICATION COMPLETED !" TO MESS.
           PERFORM SEND-MESS.
           GO TO MAIN-MODULE.

      ******************************************
       INITIALIZATION SECTION.
      ******************************************
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-BR-FILE.

       E-LOC1.
           MOVE "EN N040LOC1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY GO TO E-LOC1.
           MOVE VDUNUM0 TO EXISTING-LOC.
       E-ACCT1.
           MOVE "EN N070ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO E-LOC1.
           IF F3-KEY GO TO ALL-ACCTS.
           IF UP-KEY GO TO E-LOC1.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY GO TO E-ACCT1.
           MOVE VDUNUM0 TO ACCT1.
       E-ACCT2.
           MOVE "EN N070ACCT2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO E-LOC1.
           IF UP-KEY GO TO E-ACCT1.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY GO TO E-ACCT2.
           MOVE VDUNUM0 TO ACCT2.
           MOVE "N" TO ALL-ACCTS.
           GO TO E-NEWLOC1.
       ALL-ACCTS.
           MOVE "SN N070ACCT1." TO VDU.
           MOVE 0 TO ACCT1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070ACCT2." TO VDU.
           MOVE 9999999 TO ACCT2 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "Y" TO ALL-ACCTS.
       E-NEWLOC1.
           MOVE "N" TO ALL-BRANCHES.
           MOVE "EN N040NEWLOC1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO E-LOC1.
           IF F3-KEY
              GO TO ALL-LOCS.
           IF UP-KEY GO TO E-ACCT2.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY GO TO E-NEWLOC1.
           MOVE VDUNUM0 TO NEW-LOC1.
       E-NEWLOC2.
           MOVE "EN N040NEWLOC2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO E-LOC1.
           IF UP-KEY GO TO E-NEWLOC1.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY GO TO E-NEWLOC2.
           MOVE VDUNUM0 TO NEW-LOC2.
           MOVE "N" TO ALL-BRANCHES.
           GO TO FIX-MISC.
       ALL-LOCS.
           MOVE "SN N040NEWLOC1." TO VDU.
           MOVE 0 TO NEW-LOC1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040NEWLOC2." TO VDU.
           MOVE 9999 TO NEW-LOC2 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "Y" TO ALL-BRANCHES.
       FIX-MISC.
           MOVE "E  A000FXMISC." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO END-PROGRAM.
           IF F1-KEY GO TO E-LOC1.
           IF UP-KEY GO TO E-NEWLOC2.
           MOVE VDUBUF TO FIX-MISC-FG.
           IF VDUBUF = "Y" GO TO ACCEPT-IT.
           IF VDUBUF NOT = "N" GO TO FIX-MISC.
       ACCEPT-IT.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO END-PROGRAM.
           IF F1-KEY GO TO E-LOC1.
           IF UP-KEY GO TO E-NEWLOC2.
           IF VDUBUF = "Y" GO TO EXIT-INITIALIZE.
           IF VDUBUF = "N" GO TO E-LOC1.
           GO TO ACCEPT-IT.
       EXIT-INITIALIZE.
           MOVE 0 TO DISPLAY-COUNT.

      ******************************************
       CREATE-GL-MASTER SECTION.
      ******************************************
           MOVE EXISTING-LOC TO GL-BRANCH
                                QGL1-WBEG-BRANCH
                                QGL1-WEND-BRANCH.
           MOVE ACCT1        TO GL-ACCOUNT
                                QGL1-WBEG-ACCOUNT.
           MOVE ACCT2        TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.
       NEXT-GL.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CREATE-GL.

      *    UNLOCK GL-FILE.
           IF GL-BRANCH NOT = EXISTING-LOC
              GO TO EXIT-CREATE-GL.

           IF GL-ACCOUNT > ACCT2
              GO TO EXIT-CREATE-GL.

           MOVE GL-REC     TO HOLD-GL-REC.

           MOVE GL1-KEY    TO SAVE-GL1-KEY.

           MOVE GL-ACCOUNT TO DISPLAY-ACCT.

           MOVE NEW-LOC1   TO BR-NO.

           PERFORM START-BR-FILE.
       NEXT-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              GO TO NEXT-GL.

           IF BR-NO = EXISTING-LOC
              GO TO NEXT-BRANCH.

           IF BR-NO > NEW-LOC2
              GO TO NEXT-GL.

           MOVE BR-NO TO DISPLAY-BRANCH.

       CREATE-GL-REC.

           MOVE HOLD-GL-REC TO GL-REC.
           MOVE BR-NO       TO GL-BRANCH GL-CONSOL.
           MOVE 0           TO GL-OPENBAL GL-LYOPENBAL.

           PERFORM CLEAR-GL-REC
              VARYING SUB FROM 1 BY 1 UNTIL SUB > 12.
           PERFORM WRITE-GL1-FILE.

           IF IO-BAD
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 IF FIX-MISC-FG = "Y"
                    GO TO CHECK-REWRITE-GL.

           IF IO-GOOD
              ADD 1 TO DISPLAY-COUNT.
           PERFORM SEND-MSG.

           GO TO NEXT-BRANCH.

           GO TO NEXT-GL.

       CHECK-REWRITE-GL.
           MOVE HOLD-GL-REC TO GL-REC.
           MOVE BR-NO TO GL-BRANCH.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              GO TO NEXT-BRANCH.
           MOVE HOLD-GL-DESCRIPTION TO GL-DESCRIPTION.
           MOVE HOLD-GL-SPANISH-DESC TO GL-SPANISH-DESC.
           MOVE HOLD-GL-CLEAR TO GL-CLEAR.
           PERFORM REWRITE-GL1-FILE.
           GO TO NEXT-BRANCH.

       EXIT-CREATE-GL.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           MOVE EXT-CONAME-CONAME TO GLDUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLDUP-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GLDUP-SCREEN UPON GLDUP-WINDOW-HANDLE.
           MOVE GLDUP-FORM-FIELDS TO WR-BUF.
           MOVE GLDUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE GLDUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLDUP-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLDUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       CLEAR-GL-REC.
           MOVE 0 TO GL-CYBAL(SUB) GL-LYBAL(SUB) GL-CYBUD(SUB)
                     GL-LYBUD(SUB) GL-NYBUD(SUB).
       SEND-MSG.
           MOVE "S  A000MESS." TO VDU.
           MOVE DISPLAY-BUF TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************
       END-PROGRAM SECTION.
      ******************************************
       END-PROG.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLDUP-SCREEN.
           DESTROY GLDUP-WINDOW-HANDLE.
           EXIT PROGRAM.
