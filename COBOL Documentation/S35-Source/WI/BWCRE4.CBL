       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWCRE4.
       DATE-WRITTEN.
      **************************************************************************
      *   BORROWER CREATE   F5-CREATE BORROWER FROM LOAN INPUT
      *                          PAGE 4 (ALTERNATE ADDRESS)
      *                        (WINDOW)
      * REV:
      *  BAH 030520 NEW FOR ALTERNATE ADDRESS OUT OF THE BXFILE, TRAILER
      *             REGACC PR# 27
      *  BAH 030626 CREATE A CHANGES REC AND BORROWER MEMO WHEN FIELDS ARE
      *             CHANGED, REGACC
      *  BAH 120418 ADDED P01 AND P02 FOR SPANISH VS ENGLISH, WORLD PR# 800
      *  BAH 130123 A30 CONVERSION
      *  BAH 150928 ADDED P03 FOR SPANISH VS ENGLISH, WORLD PR# 1103
      *  BAH 170119 ADDED LEGEND-01-SPANISH FOR WORLD PR#1218
      *  BLM 20210920 MOVE BX-PATH-OWNBR TO BX-BRNO BEFORE WRITING BX
      **************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./WI/BWCRE4.CBL S34 09/20/21-15:44:58 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       01  ERRCD               PIC X     VALUE SPACES.

       01  SAVE-BM1-KEY.
           03  SAVE-BM-BRNO    PIC 9(4).
           03  SAVE-BM-SSNO    PIC 9(9).
           03  SAVE-BM-DATE    PIC 9(8).
           03  SAVE-BM-SEQ     PIC 9(3).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-CHAR   PIC X(24).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).

       01  LEGEND          PIC X(35).
       01  LEGEND-01.
           03  FILLER      PIC X(21)  VALUE
           "F1, F7-EXIT, ENTER #:".
       01  LEGEND-01-SPANISH.
           03  FILLER      PIC X(24)  VALUE
           "F1, F7-SALIR, # DE FILA:".
       01  LEGEND-02.
           03  FILLER      PIC X(8)  VALUE
           " F7-EXIT".

       01  BWCRE4-LINES          PIC 99  VALUE 22.
       01  BWCRE4-COLUMNS        PIC 99  VALUE 78.
       01  BWCRE4-BEGIN-Y        PIC 99  VALUE 2.
       01  BWCRE4-BEGIN-X        PIC 99  VALUE 2.

       01  ONE             PIC S9     COMP.
       01  ELE             PIC 9(2)   COMP.
       01  NEW-FLAG        PIC 9.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  IO-TYPE         PIC X(3).

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01                 PIC X(24).
           03  F02                 PIC X(24).
           03  F03                 PIC X(15).
           03  F04                 PIC X(2).
           03  F05                 PIC X(10).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(20) VALUE
           "010 020 030 040 050.".


       01  MAX             PIC 99   VALUE 05.

       01  SPEC-TABLE.
      * 1. ALTERNATE ADDRESS # 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 2. ALTERNATE ADDRESS # 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 3. ALTERNATE CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 4. ALTERNATE STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 5. ALTERNATE ZIP CODE
           03  FILLER      PIC X(8) VALUE "E  A100 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 5 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  LEGEND-WALK.
           03  FILLER                   PIC X(10)  VALUE "F1, ^-UP, ".
           03  FILLER                   PIC X      VALUE X'76'.
           03  FILLER                   PIC X(24)  VALUE
           "-DOWN, ENTER FIELD INFO:".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BWCRE4_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/BWCRE4_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  BWCRE4-WINDOW-HANDLE       PIC X(10).
       01  CHAN                       PIC X(4).


      *=================================================================
      *   BWCH-REC FROM 'REPLACING LEADING' ON BW-REC. [CHANGED-BW-REC]
      *   BXCH-REC FROM 'REPLACING LEADING' ON BX-REC. [CHANGED-BX-REC]
      *   BWOR-REC FROM 'REPLACING LEADING' ON BW-REC. [ORIGINAL-BW-REC]
      *   BXOR-REC FROM 'REPLACING LEADING' ON BX-REC. [ORIGINAL-BX-REC]
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWOR".
       COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "BXOR".
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWCH".
       COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "BXCH".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01BW.CPY".
       01  ALLOW-CHANGES       PIC X.

       SCREEN SECTION.
       COPY "LIBWI/BWCRE4_SCN.CPY".

       PROCEDURE DIVISION USING FORM-PATHNAME
                  BW-REC
                  ALLOW-CHANGES 
                  EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "BWCRE4" TO VDU-FORM-NAME.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE BWCRE4-BEGIN-Y * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  BWCRE4-BEGIN-X * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    BWCRE4-COLUMNS + VDU-WIN-W-MOD
                      LINES   BWCRE4-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS BWCRE4-WINDOW-HANDLE.

           MOVE " " TO VDU-WIMENU-SELECTION.

      * STORE ORIGINAL BW-REC FROM PAGE 1:
           MOVE BW-REC TO BWOR-REC.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

      * RESTORE ORIGINAL BW-REC FROM PAGE 1:
           IF IO-TYPE = "CLR"
              MOVE BWOR-REC TO BW-REC.

           GO TO MAIN-MODULE.


      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE " " TO IO-TYPE.
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
      * ALLOW MODIFY OF BRANCH
           IF ALLOW-CHANGES = "Y"
              PERFORM TEST-LEGEND-01
           ELSE
              MOVE LEGEND-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              PERFORM UPDATE-BX-FILE
              PERFORM CREATE-CHANGE-REC
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           IF ALLOW-CHANGES = "N"
              PERFORM ALARM
              MOVE "PLEASE NOTE..." TO ERRMSG-HEAD
              MOVE "CHANGES ARE NOT ALLOWED FROM THIS SCREEN!!"
                                            TO ERRMSG-MSG1
              MOVE SPACES TO ERRMSG-MSG2 ERRMSG-MSG3
              PERFORM CALL-ERRMSG
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
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
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 1
              MOVE LEGEND-WALK TO LEGEND
           ELSE
              MOVE "F1-CANCEL, ENTER FIELD INFO:" TO LEGEND.
           PERFORM SEND-LEGEND.
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
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 DEPENDING ON ELE.

           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO BX-ALT-ADR1.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BX-ALT-ADR2.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BX-ALT-CITY.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BX-ALT-STATE.
           IF VDUBUF = " " AND BX-ALT-ADR1 = " " AND
                               BX-ALT-ADR2 = " "
              GO TO CREATE-ROUTINE.

           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO BX-ALT-ZIP.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************
       UPDATE-BX-FILE SECTION.
      ************************************
           MOVE BX-REC TO BXCH-REC.
           IF BXOR-REC = BXCH-REC
              GO TO EXIT-UPDATE-BX-FILE.

           PERFORM OPEN-BX1-FILE.
           MOVE BW-SSNO TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           MOVE BXCH-REC TO BX-REC.
           IF IO-FG = 0
              PERFORM REWRITE-BX1-FILE
           ELSE
              PERFORM WRITE-BX1-FILE.

           PERFORM CLOSE-BX1-FILE.

       EXIT-UPDATE-BX-FILE.
           EXIT.

      *****************************
       CREATE-CHANGE-REC SECTION.
      *****************************
           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-BM1-FILE.

           IF BXOR-ALT-ADR1 NOT = BX-ALT-ADR1
              PERFORM CREATE-SETUP-REC
              MOVE "BX ALT ADR1" TO CH-DESCRIPT
              MOVE BXOR-ALT-ADR1 TO CH-ORG-CHAR
              MOVE BX-ALT-ADR1 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF BXOR-ALT-ADR2 NOT = BX-ALT-ADR2
              PERFORM CREATE-SETUP-REC
              MOVE "BX ALT ADR2" TO CH-DESCRIPT
              MOVE BXOR-ALT-ADR2 TO CH-ORG-CHAR
              MOVE BX-ALT-ADR2 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF BXOR-ALT-CITY NOT = BX-ALT-CITY
              PERFORM CREATE-SETUP-REC
              MOVE "BX ALT CITY" TO CH-DESCRIPT
              MOVE BXOR-ALT-CITY TO CH-ORG-CHAR
              MOVE BX-ALT-CITY TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF BXOR-ALT-STATE NOT = BX-ALT-STATE
              PERFORM CREATE-SETUP-REC
              MOVE "BX ALT STATE" TO CH-DESCRIPT
              MOVE BXOR-ALT-STATE TO CH-ORG-CHAR
              MOVE BX-ALT-STATE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF BXOR-ALT-ZIP NOT = BX-ALT-ZIP
              PERFORM CREATE-SETUP-REC
              MOVE "BX ALT ZIP" TO CH-DESCRIPT
              MOVE BXOR-ALT-ZIP TO CH-ORG-CHAR
              MOVE BX-ALT-ZIP TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.

           PERFORM CLOSE-CH-FILE.
           GO TO EXIT-CREATE-CHANGE-REC.
       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.

           MOVE BW-OWNBR           TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE "M"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE 1                  TO CH-SEQ.

           MOVE BW-SSNO            TO CH-SSNO.

       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ************************************
       UPDATE-CH-BM-FILE SECTION.
      ************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO NOT = "Y"
              GO TO UPDATE-CH-BM-FILE-EXIT.

           MOVE BM-PATH-OWNBR TO SAVE-BM-BRNO.
           MOVE CH-SSNO       TO SAVE-BM-SSNO.
           COMPUTE SAVE-BM-DATE = 99999999 - SYS-DATE.
           MOVE 1             TO SAVE-BM-SEQ.
       UPDATE-CH-BM-FILE-AGAIN.
           MOVE SPACES TO BM-REC.

           MOVE SAVE-BM1-KEY TO BM1-KEY.
           MOVE CH-USERID TO MEMO-USERID.
           MOVE CH-DESCRIPT TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA TO BM-MEMO.

           MOVE "9" TO BM-MEMO-TYPE.
           PERFORM WRITE-BM1-FILE.
           IF IO-BAD
              IF SAVE-BM-SEQ < 999
                 ADD 1 TO SAVE-BM-SEQ
                 GO TO UPDATE-CH-BM-FILE-AGAIN.

       UPDATE-CH-BM-FILE-EXIT.
           EXIT.

      ****************************************
       DISPLAY-FIELDS SECTION.
      ****************************************
           MOVE SPACES TO DISPLAY-BUF.

           PERFORM OPEN-BX1-FILE.
           MOVE BW-SSNO TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           PERFORM CLOSE-BX1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-BX-FILE
              MOVE BX-PATH-OWNBR TO BX-BRNO
              MOVE BW-SSNO TO BX-SSNO.
           MOVE BX-REC TO BXOR-REC.

           IF BR-STATE = "MX"
              MOVE "S  A150P01." TO VDU
              MOVE "COLONIA CL    :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A150P02." TO VDU
              MOVE "ENTR CALLE    :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A150P03." TO VDU
              MOVE "CLAVE PAIS    :" TO VDUBUF
              PERFORM SCREENIO.
           MOVE BX-ALT-ADR1 TO F01.
           MOVE BX-ALT-ADR2 TO F02.
           MOVE BX-ALT-CITY TO F03.
           MOVE BX-ALT-STATE TO F04.
           MOVE BX-ALT-ZIP TO F05.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPBXCL.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE4-SCREEN.
           DISPLAY BWCRE4-SCREEN UPON BWCRE4-WINDOW-HANDLE.
           MOVE BWCRE4-FORM-FIELDS TO WR-BUF.
           MOVE BWCRE4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE BWCRE4-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWCRE4-HELP--DIR TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BWCRE4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARAGRAPHS SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       TEST-LEGEND-01.
           IF BR-STATE = "MX"
              MOVE LEGEND-01-SPANISH TO LEGEND
           ELSE
              MOVE LEGEND-01 TO LEGEND.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".

      ******************************
       END-ROUTINE SECTION.
      ******************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE4-SCREEN.
           DESTROY BWCRE4-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
