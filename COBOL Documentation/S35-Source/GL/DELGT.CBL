       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELGT.
      ******************************************************************
      *                (GL)
      *      PURGE GTFILE REGARDLESS OF UPDATE FLAG
      *      IDENTICAL TO ENDMTH EXCEPT NO TAG LOGIC & DOES NOT
      *      CHECK GT-FLAG
      * REV:
      * BLF 041227 ADD BEG & ENDING BRANCH RANGE
      * BLF 060710 ADD BEG & ENDING SOURCE RANGE
      * BLM 100929 ADD COUNT OF RECS DELETED & DISPLAY WHEN COMPLETE
      * KEC 2016.1129 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170322 ACTIVATE 8 DIGIT DATE, PD0006
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(42) VALUE
           "@(#)./GL/DELGT.CBL S34 06/09/21-20:36:11 >".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-DATE         PIC 9(8).
           03  END-DATE         PIC 9(8).
           03  BEG-SOURCE       PIC 9.
           03  END-SOURCE       PIC 9.
       01  VERSION-CONTROL    PIC X VALUE "A".
       01  MESS-LIST          PIC X(5)      VALUE "MESS.".
       01  FIX-COUNT          PIC 9(6)      VALUE 0.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/DELGT_DEF.CPY".
       COPY "LIBGL/DELGT_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/DELGT_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ****************************************************
       MAIN-MODULE SECTION.
      ****************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DELGT" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "** GL TRANSACTION DELETE IN PROGRESS **" TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM CLEAR-GT1-FILE.

           MOVE "&&&&&& GL TRANSACTIONS DELETED." TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&&&" BY FIX-COUNT.
           PERFORM SEND-MESS.

           GO TO END-PROGRAM.

      *********************************************
       INITIALIZATION SECTION.
      *********************************************
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.
       COPY "LIBGB/EOINIT.CPY".
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************
       EO-EXEC-AUDIT SECTION.
      *********************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *********************************************
       ENTER-PARAMETERS SECTION.
      *********************************************
           IF UP-KEY
              GO TO SOURCE-02.
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
           GO TO REC-BEG-DATE.
      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       REC-BEG-DATE.
           MOVE "00" TO VDUSTATUS.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SOURCE-01.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO BR-02.
           IF F3-KEY
               MOVE "S  A080BEGDATE." TO VDU
               MOVE 0 TO BEG-DATE VDU-DATE-EDIT
               PERFORM SCREENIO
               MOVE "S  A080ENDDATE." TO VDU
               MOVE 99999999 TO END-DATE VDU-DATE-EDIT
               PERFORM SCREENIO
               GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO REC-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       REC-END-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SOURCE-01.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO REC-BEG-DATE.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO REC-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF BEG-DATE > END-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO REC-BEG-DATE.
      *----------------------------------------------------------------
       SOURCE-01.
           MOVE "ENNN010SOURCE1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-SOURCES.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO BR-02
              ELSE
                 GO TO REC-END-DATE.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO SOURCE-01.
           MOVE VDUNUM0 TO BEG-SOURCE.
      *----------------------------------------------------------------
       SOURCE-02.
           MOVE "ENNN010SOURCE2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO SOURCE-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO SOURCE-02.
           MOVE VDUNUM0 TO END-SOURCE.
           IF BEG-SOURCE > END-SOURCE
              MOVE "INVALID SOURCE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO SOURCE-01.
           GO TO EXIT-PARM.
      *----------------------------------------------------------------
       ALL-SOURCES.
           MOVE "SNNN010SOURCE1." TO VDU.
           MOVE 0 TO BEG-SOURCE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN010SOURCE2." TO VDU.
           MOVE 9 TO END-SOURCE VDUNUM0.
           PERFORM SCREENIO.
       EXIT-PARM.
           EXIT.

      *********************************************
       DISPLAY-PARAMETERS SECTION.
      *********************************************

      *********************************************
       CLEAR-GT1-FILE SECTION.
      *********************************************

           PERFORM OPEN-GT1-FILE.
       CLEAR-NEXT-GT1.
           PERFORM READ-GT1-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CLEAR-GT1.

           IF GT-BRANCH < BEG-BRANCH OR GT-BRANCH > END-BRANCH
      *       UNLOCK GT-FILE
              GO TO CLEAR-NEXT-GT1.

           IF GT-SOURCE < BEG-SOURCE OR GT-SOURCE > END-SOURCE
      *       UNLOCK GT-FILE
              GO TO CLEAR-NEXT-GT1.

           IF GT-DATE1 < BEG-DATE OR GT-DATE1 > END-DATE
      *       UNLOCK GT-FILE
              GO TO CLEAR-NEXT-GT1.

           PERFORM DELETE-GT1-FILE.

           ADD 1 TO FIX-COUNT.

           GO TO CLEAR-NEXT-GT1.
       EXIT-CLEAR-GT1.
           PERFORM CLOSE-GT1-FILE.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO DELGT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DELGT-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DELGT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DELGT-FORM-FIELDS TO WR-BUF.
           MOVE DELGT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE DELGT-HELP--FILE TO HELP-LINK-FILE.
           MOVE DELGT-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/DELGT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************
       MISC-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-GT1-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGT1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "GL/GLMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DELGT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
