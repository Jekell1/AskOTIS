       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRDMAN.
       DATE-WRITTEN. 11-01-94.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   TRANSACTION FILE MAINTENANCE
      *                      DEPOSITS
      *
      * REV:
      *   MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *   MJD 062995 ADDED TD-USERID, TD-POSTTIME, AND TD-POSTDATE.
      *              SET NO RESET ON ALL DISPLAY FIELDS.
      * MJD 131121 A30 CONVERSION COMPLETED
      * BLM 160921 ADDED TD-CHECK-DEPOSIT, FIELD 10, WORLD PR# 1188
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *  CS 170508 ACTIVATE 8 DIGIT DATES
      * BAH 2019.0507 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BAH 20190930 REMOVED ACCESS-CALL TRDFILE
      * BLM 20211022 FIX GL ACCT ENTRY SO YOU CAN ENTER OR NOT ENTER
      *              DASH AND LEADING ZEROES
      * BAH 2023.1006 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
      * BAH 2025.0124 VALIDATE MONTH ENTERED S35Q-159
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/TRDMAN.CBL S34 10/22/21-15:04:22 >".

       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".


       01  ERRCD           PIC X       VALUE " ".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  SAVE-BRNO       PIC 9(4) VALUE 0.
       01  SAVE-MMYY       PIC 9(4) VALUE 0.
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

       01  ORIGINAL-TRD-REC    PIC X(TRD-SIZE).
       01  CHANGED-TRD-REC     PIC X(TRD-SIZE).

      ******************************************************************
      *    *     SCREEN DISPLAY BUFFER      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F02         PIC 9(12).
           03  F03         PIC 9(12).
           03  F04         PIC X.
           03  F05         PIC 9(12).
           03  F06         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F07         PIC X(10).
           03  F08         PIC 9(6)           BLANK ZERO.
           03  F09         PIC 99/99/99       BLANK ZERO.
           03  F10         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100.".

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 10.
       01  SPEC-TABLE.
      * 1. DEPOSIT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 2. BANK ACCOUNT NUMBER
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      * 3. CASH ACCOUNT NUMBER
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      * 4. GL-FLAG
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 5. OVERSHORT ACCOUNT NUMBER
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      * 6. OVERSHORT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 7. POSTED USERID
           03  FILLER      PIC X(8) VALUE "E NA100 ".
      * 8. POSTTIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 9. POSTDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *10. CHECK DEPOSIT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 10 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       01  KEY-BUF.
           03  D-BRANCH    PIC ZZZ9.
           03  D-MMYY.
               05  D-MM    PIC 99.
               05  D-YY    PIC 99.
           03  D-DAY       PIC 99.
           03  D-BRNO      PIC ZZZ9.
           03  D-CODE      PIC X.
       01  KEY-LIST.
           03  FILLER      PIC X(26) VALUE
               "BRANCH MMYY DAY BRNO CODE.".

       01  SCREEN-GL.
           03  SC-BRANCH   PIC 9(4).
           03  SC-DASH     PIC X.
           03  SC-NUMBER   PIC 9(7).
       01  SCREEN-GL-NUM REDEFINES SCREEN-GL PIC 9(12).
       01  WORK-GL         PIC 9(11).
       01  WORK-NUMBER REDEFINES WORK-GL.
           03  WORK-BR     PIC 9(4).
           03  WORK-NO     PIC 9(7).
       01  DISP-GL.
           03  DISP-BR     PIC 9(4).
           03  FILLER      PIC X VALUE "-".
           03  DISP-NO     PIC 9(7).

       COPY "LIBWI/TRDSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 TRDMAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRDMAN_DEF.CPY".
       COPY "LIBMN/TRDMAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/TRDMAN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M    M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TRDMAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TRDMAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.

       ENTER-BRANCH.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO SAVE-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN


           PERFORM LOAD-BRANCH-MODULE.
           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

       ENTER-MONTH.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH.

           MOVE VDUNUM0 TO SAVE-MMYY.
           IF SAVE-MM < 1 OR > 12
              MOVE "INVALID MMYY, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH.

           PERFORM OPEN-TRD1-FILE.

       RESTART-MAINT.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO RESTART-MAINT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRD-TD-DATE LPWSTRD-TD-BRNO.
           MOVE 0             TO NEW-FLAG.

           MOVE "ENNN020DAY." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES        TO MESS.
           PERFORM CALL-TMMSG.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1<"
              MOVE SAVE-BRNO   TO TRDSCN-BRNO
              MOVE SAVE-MMYY   TO TRDSCNW-MMYY
              MOVE "Y"         TO TRDSCN-RETURN
              MOVE "WI/TRDSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRDSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRDSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRDSCN-TRD1-KEY TO LPWSTRD-TRD1-KEY
                 PERFORM DISPLAY-KEY
                 GO TO READ-TRANS-FILE.


           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DAY.

           MOVE VDUNUM0 TO DATE-MMDDYY-DD.
           MOVE SAVE-MM TO DATE-MMDDYY-MM.
           MOVE SAVE-YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD TO LPWSTRD-TD-DATE.

      *----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRD-TD-BRNO
                                      TD-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN

      *    MOVE "ENNN040BRNO." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "10"
      *       MOVE "CLR" TO IO-TYPE
      *       GO TO END-ENTER-KEYS-MODULE.
      *    IF VDUSTATUS = "1U"
      *       GO TO ENTER-DAY.
      *    IF VDUSTATUS = "1>"
      *       MOVE "END" TO IO-TYPE
      *       GO TO END-ENTER-KEYS-MODULE.
      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      *    THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      *    PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE VDUNUM0 TO BR-NO LPWSTRD-TD-BRNO TD-BRNO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE "S NA200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "D"            TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRD1-FILE.
           MOVE LPWSTRD-TRD1-KEY TO TRD1-KEY.
           PERFORM READ-TRD1-FILE.
           IF IO-FG = 0
              MOVE TRD-REC TO ORIGINAL-TRD-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      ******************************************************************
      *    E N T R Y    M O D U L E
      *
      *        NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                   1 - INITIAL NEW RECORD ENTRY
      *                   2 - MODIFY NEW RECORD
      ******************************************************************
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

      *----------------------------------------------------------------
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

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE)  TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE 0          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *           SET RECORD ELEMENTS
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUNUM2 TO TD-DEPOSIT.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-02.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TD-BANK.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-03.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TD-CASH.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TD-GLFG.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-05.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TD-GLOVERSHORT.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-06.
           MOVE VDUNUM2 TO TD-OVERSHORT.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO TD-USERID.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-08.
           MOVE VDUNUM0 TO TD-POSTTIME.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-09.
           MOVE VDU-DATE-YYYYMMDD  TO TD-POSTDATE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM2 TO TD-CHECK-DEPOSIT.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRD1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE TRD-REC TO CHANGED-TRD-REC.

           IF ORIGINAL-TRD-REC = CHANGED-TRD-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRD1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRD-REC NOT = ORIGINAL-TRD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRD-REC TO TRD-REC.
           PERFORM REWRITE-TRD1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRD1-KEY                    TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

      *----------------------------------------------------------------
       WRITE-RECORD.
           PERFORM WRITE-TRD1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRD1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRD1-FILE.
           PERFORM READ-TRD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRD-REC NOT = ORIGINAL-TRD-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRD1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRD1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1                    TO NEW-FLAG.
           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRD-FILE.
           MOVE LPWSTRD-TRD1-KEY TO TRD1-KEY.
           MOVE 0         TO ELE.

      ********************************************
       CHECK-GLNO SECTION.
      ********************************************
           MOVE VDUBUF TO SCREEN-GL.
           CALL "C$JUSTIFY" USING SCREEN-GL, "R".
           INSPECT SCREEN-GL REPLACING LEADING " " BY "0".
           MOVE 0 TO WORK-GL.
           IF SCREEN-GL-NUM IS NUMERIC
              MOVE SCREEN-GL-NUM TO WORK-GL
              GO TO DISPLAY-GL. 
           IF SC-DASH = "-"
              IF (SC-BRANCH NUMERIC AND SC-NUMBER NUMERIC)
                 MOVE SC-BRANCH TO WORK-BR
                 MOVE SC-NUMBER TO WORK-NO.
           IF WORK-GL NOT NUMERIC
              MOVE 9 TO IO-FG
              GO TO EXIT-CHECK-GLNO.
       DISPLAY-GL.
           MOVE "S  A120" TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0   TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO VDUBUF.
           PERFORM SCREENIO.
       READ-GL.
           MOVE WORK-GL TO GL-BR-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID G/L#" TO MESS
              PERFORM SEND-MESS.

           IF GL-BR-ACCOUNT = 0
              MOVE 0 TO IO-FG.
       EXIT-CHECK-GLNO.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           IF TD-DEPOSIT NOT = 0
              MOVE TD-BANK TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL TO F02
              MOVE TD-CASH TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL TO F03
              MOVE TD-DEPOSIT  TO F01.

           IF TD-GLFG <= " "
              MOVE " " TO F04
           ELSE
              MOVE TD-GLFG TO F04.

           MOVE TD-GLOVERSHORT TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F05.
           MOVE TD-OVERSHORT    TO F06.
           MOVE TD-USERID       TO F07.
           MOVE TD-POSTTIME     TO F08.
           MOVE TD-POSTDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO F09.

           MOVE TD-CHECK-DEPOSIT TO F10.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.

           MOVE LPWSTRD-TD-DATE     TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM    TO D-MM.
           MOVE DATE-YYYYMMDD-YY    TO D-YY.

           MOVE DATE-YYYYMMDD-DD    TO D-DAY.
           MOVE LPWSTRD-TD-BRNO     TO D-BRNO.
           MOVE "D"                 TO D-CODE.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************************************
       FORM-RESET SECTION.
      *************************************************************
           MOVE EXT-CONAME-CONAME TO TRDMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRDMAN-SYSDATE.
           DISPLAY SPACES UPON TRDMAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRDMAN-SCREEN UPON TRDMAN-WINDOW-HANDLE.
           IF SAVE-BRNO NOT = 0
              MOVE SAVE-BRNO TO TRDMAN-BRANCH
              MOVE SAVE-MMYY TO TRDMAN-MMYY.
           MOVE TRDMAN-FORM-FIELDS TO WR-BUF.
           MOVE TRDMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************************
           MOVE TRDMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRDMAN-HELP--DIR TO HELP-LINK-DIR.

      *************************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRDMAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

     
     
      ******************************************************************
       LOAD-BRANCH-MODULE SECTION.
      ******************************************************************
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO BR-PATH
              GO TO LOAD-BRANCH-MODULE-EXIT.
     
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
     
           MOVE "B"                        TO TRD-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRDCL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRD1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY TRDMAN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
