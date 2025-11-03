       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLDSMN.
      *****************************************************
      *   G/L DESCRIPTION FILE MAINTENANCE
      *
      * REV:
      *  JTG 012489 CHANGED CLEAR RECORD ROUTINE
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/GLDSMN.CBL S34 06/03/21-20:59:56 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  ERRCD           PIC X       VALUE SPACES.
       01  SUB             PIC 999     VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  HOLD-GL1-KEY    PIC 9(11).

       01  ORIGINAL-GL-REC    PIC X(GL-SIZE).
       01  CHANGED-GL-REC     PIC X(GL-SIZE).

       01  DISPLAY-BUF         VALUE SPACES.
           03  F01             PIC X(36).
       01  DISPLAY-LISTS.
           03  FILLER          PIC X(4) VALUE
           "010.".
       01  DISPLAY-LIST        REDEFINES DISPLAY-LISTS
                               PIC X(5).
       01  MAX             PIC 99   VALUE 1.
       01  SPEC-TABLE.
      * 1. ACCOUNT DESCRIPTION
           03  FILLER      PIC X(8) VALUE "E  A360 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 1 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GLDSMN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/GLDSMN_DEF.CPY".
       COPY "LIBMN/GLDSMN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/GLDSMN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************
       MAIN-PROGRAM SECTION.
      ******************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "GLDSMN" TO VDU-FORM-NAME.

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
                   HANDLE IS GLDSMN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-GL-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           GO TO MAIN-MODULE.

      ******************************************
       REC-GL-MODULE SECTION.
      ******************************************
       REC-GL-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GL-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-GL-MODULE-NO.
           IF VDUNUM0 = 0
              GO TO REC-GL-MODULE-NO.
           MOVE VDUNUM0 TO GL-BRANCH BR-NO.
       READ-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO REC-GL-MODULE-NO.
       REC-GL-ACCTNO.
           MOVE "EN N070ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR
               VDUSTATUS = "1U"
                  GO TO REC-GL-MODULE-NO.
           IF VDUSTATUS NOT = "00"
              GO TO REC-GL-ACCTNO.
           IF VDUNUM0 = 0
              GO TO REC-GL-ACCTNO.
           MOVE VDUNUM0 TO GL-ACCOUNT.
       READ-GENERAL-FILE.
           MOVE GL1-KEY TO HOLD-GL1-KEY.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-FG = 0
              MOVE GL-REC TO ORIGINAL-GL-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-GL-MODULE.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
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
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
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
                 GO TO CREATE-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           IF ELE = 1
              GO TO F-01.
           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUBUF TO GL-DESCRIPTION.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-GL1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE GL-REC TO CHANGED-GL-REC.
           IF ORIGINAL-GL-REC = CHANGED-GL-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-GL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF GL-REC NOT = ORIGINAL-GL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-GL-REC TO GL-REC.
           PERFORM REWRITE-GL1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-GL1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-GL1-FILE.

      ************************************************
       DELETE-MODULE SECTION.
      ************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF GL-REC NOT = ORIGINAL-GL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-GL1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-GL1-FILE.

      ************************************************
       NEW-RECORD-SETUP SECTION.
      ************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           MOVE SPACES TO GL-REC.
           MOVE HOLD-GL1-KEY TO GL1-KEY.
           MOVE "D" TO GL-TYPE.
           MOVE "N" TO GL-CLEAR.
           MOVE 0 TO GL-CONSOL GL-OPENBAL GL-NUMBER1 GL-NUMBER2
                     GL-LYOPENBAL.
           MOVE 0 TO ELE SUB.
           PERFORM CLEAR-GL-TABLE 11 TIMES.
       CLEAR-GL-TABLE.
           ADD 1 TO SUB.
           MOVE 0 TO GL-CYBAL(SUB)
                     GL-LYBAL(SUB)
                     GL-CYBUD(SUB)
                     GL-LYBUD(SUB)
                     GL-NYBUD(SUB).

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE GL-DESCRIPTION TO F01.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO GLDSMN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO GLDSMN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GLDSMN-SCREEN UPON GLDSMN-WINDOW-HANDLE.
           MOVE GLDSMN-FORM-FIELDS TO WR-BUF.
           MOVE GLDSMN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE GLDSMN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLDSMN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/GLDSMN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       NO-DELETE.
           MOVE " LOANS EXIST FOR THIS CLASS - CANNOT DELETE!" TO MESS
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
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
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGL1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLDSMN-SCREEN.
           DESTROY GLDSMN-WINDOW-HANDLE.
           EXIT PROGRAM.
