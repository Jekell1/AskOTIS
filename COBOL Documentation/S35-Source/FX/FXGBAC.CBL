       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXGBAC.
       AUTHOR.  MJD.
       DATE-WRITTEN.    111595.
      *****************************************************************
      *                           (FX)
      *
      *  DESCRIPTION:  CHANGE GLOBAL LAST ACCRUAL DATE 
      *  
      * REV:
      *  GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BAH 190918 REMOVED ACCESS-CALL ON GBFILE
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
      *****************************************************************
      *      WORKING STORAGE SECTION
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXGBAC.CBL S34 05/21/21-14:59:51 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD             PIC X     VALUE " ".

       01  ACCEPT-FG         PIC X     VALUE " ".

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
 
       01  TODAYS-DATE       PIC 9(8)      VALUE 0.
       01  TODAYS-DATEX     REDEFINES TODAYS-DATE.
           03  TODAYS-CC     PIC 99.
           03  TODAYS-YR     PIC 99.
           03  TODAYS-MO     PIC 99.
           03  TODAYS-DA     PIC 99.

       01  TEST-MMYY         PIC 9(4).
       01  FILLER           REDEFINES TEST-MMYY.
           03  TEST-MM       PIC 9(2).
           03  TEST-YY       PIC 9(2).

       01  BR1               PIC 9999      VALUE 0.
       01  BR2               PIC 9999      VALUE 0.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
 
       01  FXGBAC-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXGBAC_DEF.CPY".
       COPY "LIBFX/FXGBAC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
 
      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXGBAC_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
 
      *****************************************************************
      *       MAIN PROGRAM CONTROL
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIAL-ROUTINE.
           IF ERRCD NOT = " "
              GO TO END-ROUTINE.

           PERFORM OPEN-BR-FILE.
           MOVE BR1 TO BR-NO.
           PERFORM START-BR-FILE.

       MAIN-NEXT-BR.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF BR-NO > BR2
              GO TO END-ROUTINE.

       MAIN-NEXT-BR-ACCEPT.
      *       Y -  RUN TO COMPLETION
      *       S -  STOP AFTER NEXT BRANCH
      *       F7 - EXIT
           IF ACCEPT-FG = "S"
              MOVE "/" TO ACCEPT-FG
           ELSE
           IF ACCEPT-FG = "/"
              MOVE "E  A010ACCEPT." TO VDU
              PERFORM SCREENIO
              IF VDUSTATUS = "1>"
                 GO TO END-ROUTINE
              ELSE
                 IF NOT (VDUBUF = "Y" OR "S")
                    GO TO MAIN-NEXT-BR-ACCEPT.

           IF BR-DATA-PATH = " "
                  OR BR-MACHINE = " "
              MOVE " " TO MESS
              STRING "          BRANCH "
                        BR-NO
                          " MISSING DATA PATH, WILL SKIP"
                                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO MAIN-NEXT-BR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO GB-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

           GO TO MAIN-NEXT-BR.

       MAIN-PROGRAM-EXIT.
           EXIT.

      *****************************************************************
       GLOBAL-UPDATE SECTION.
      *****************************************************************
 
      * CHECK GB ACCRUAL DATE
           MOVE " " TO MESS.

           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.

           MOVE TODAYS-MO TO TEST-MM.   
           MOVE TODAYS-YR TO TEST-YY.  
           MOVE TEST-MMYY TO GB-LAST-ACCRUE.
           PERFORM REWRITE-GB-FILE.
           PERFORM CLOSE-GB-FILE.

       GLOBAL-UPDATE-EXIT.
           EXIT.

      *****************************************************************
       INITIAL-ROUTINE SECTION.
      *****************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FXGBAC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXGBAC-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.
        
           MOVE " " TO ERRCD.
           
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE "S  A000TODAYS." TO VDU.
           MOVE DATE-MMDDYY TO TODAYS-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.

       INIT-ACCEPT-TODAY.
           MOVE "E NM060TODAYS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-TODAY.
           MOVE VDU-DATE-YYYYMMDD TO TODAYS-DATE.

       INIT-ACCEPT-BR1.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF VDUSTATUS = "16"
              GO TO INIT-ACCEPT-BR-ALL.
           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-TODAY.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR1.
           MOVE VDUNUM0 TO BR1.

       INIT-ACCEPT-BR2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-BR1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR2.
           MOVE VDUNUM0 TO BR2.
           IF BR1 > BR2
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-BR1.
           GO TO INIT-ACCEPT-ACCEPT.

       INIT-ACCEPT-BR-ALL.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BR1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO BR2 VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
      *    Y - RUN TO COMPLETION
      *    S - STOP AFTER EACH BRANCH 
      *----------------------------------------------------------------
       INIT-ACCEPT-ACCEPT.
           MOVE "S  A040MESS2." TO VDU.
           PERFORM SCREENIO.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-BR2.
           IF VDUSTATUS NOT = "00"
              GO TO INIT-ACCEPT-ACCEPT.
           MOVE VDUBUF TO ACCEPT-FG.
           IF ACCEPT-FG = "N"
              GO TO INIT-ACCEPT-TODAY.
           IF NOT (ACCEPT-FG = "Y" OR "S")
              GO TO INIT-ACCEPT-ACCEPT.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS.
           PERFORM SEND-LEGEND.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO ERRCD.
       INITIALIZE-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXGBAC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXGBAC-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXGBAC-SCREEN UPON FXGBAC-WINDOW-HANDLE.
           MOVE FXGBAC-FORM-FIELDS TO WR-BUF.
           MOVE FXGBAC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXGBAC-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXGBAC-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXGBAC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARA SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
 
      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXGBAC-SCREEN.
           DESTROY FXGBAC-WINDOW-HANDLE.
           EXIT PROGRAM.
