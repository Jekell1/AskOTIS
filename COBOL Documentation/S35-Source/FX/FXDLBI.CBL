       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXDLBI.
       DATE-WRITTEN.    021094.
      **********************************************************************
      *  DESCRIPTION:  DELETE ALL RECORDS FOR A BRANCH IN BIFILE
      *
      * REV:
      *  JTG 081694 ADDED LOGIC FOR BI-ENTDATE
      *  BLV 010917 ADDED LOGIC FOR BEG & ENDING BRANCH #
      *  BLV 010917 REMOVED DISPLAY ON EVERY RECORD; JUST DISPLAY READ
      *             COUNT EVERY 1000 RECORDS; IMPROVED SPEED, FOR EX.
      *             10 SECONDS INSTEAD OF 5 MINUTES ON OUR SYSTEM;
      *             TOOK ABOUT 5 MINUTES FOR 1.3 MILLION RECS AT WORLD
      *  BAH 2022.1006 HARD CODED STARTS
      *  MB  2024.0717 PERFORMANCE TUNING S35Q-92
      *                CHANGED TO DELETE REQUESTED BRANCHES IN 1 STATEMENT
      **********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXDLBI.CBL S35 10/03/24-13:43:07 >".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".

       01  ERRCD             PIC X     VALUE SPACES.

       01  ACCEPT-BUF        PIC X     VALUE " ".

       01  BEG-BRANCH        PIC 9999.
       01  END-BRANCH        PIC 9999.

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".

       01  COUNTER           PIC 9(8) VALUE 0.
       01  COUNTERX REDEFINES COUNTER.
           03  FILLER        PIC X(5).
           03  COUNTER-LAST3 PIC 999.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FXDLBI-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXDLBI_DEF.CPY".
       COPY "LIBFX/FXDLBI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXDLBI_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE "FXDLBI" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXDLBI-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE " " TO ERRCD.
       ENTER-BEG-BRANCH.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-BRANCH.
           IF VDUNUM0 = 0
              GO TO ENTER-BEG-BRANCH.
           MOVE VDUNUM0 TO BEG-BRANCH.
       ENTER-END-BRANCH.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BEG-BRANCH.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-BRANCH.
           IF VDUNUM0 = 0
              GO TO ENTER-END-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.
       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-END-BRANCH.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO END-ROUTINE.
           IF ACCEPT-BUF NOT = "Y"
              GO TO ENTER-ACCEPT.

           MOVE "DELETE IN PROGRESS..." TO MESS.
           PERFORM SEND-LEGEND.

      D    STOP MESS.
           PERFORM OPEN-BI1-FILE.

      *     MOVE 0       TO BI-SSNO
      *                     BI-ACCTNO
      *                     QBI1-WBEG-SSNO
      *                     QBI1-WBEG-ACCTNO.
      *     MOVE ALL "9" TO QBI1-WEND-SSNO
      *                     QBI1-WEND-ACCTNO.
           MOVE BEG-BRANCH TO BI-OWNBR
                              QBI1-WBEG-OWNBR.
           MOVE END-BRANCH TO QBI1-WEND-OWNBR.
      *     PERFORM START-BI1-FILE.

           PERFORM DELETE-BI1-FILE-ALL.
      *     IF IO-FG = 9
      *        PERFORM IO-ERROR.

           GO TO END-ROUTINE.

      *NEXT-BI.
      *    PERFORM READ-BI1-FILE-NEXT.
      *    IF IO-FG = 9
      *       GO TO END-ROUTINE.
      *    ADD 1 TO COUNTER.
      *    IF COUNTER-LAST3 = 0
      *       STRING "DELETE IN PROGRESS: "
      *                      COUNTER " RECORDS READ..."
      *            DELIMITED BY SIZE INTO MESS
      *       PERFORM SEND-LEGEND.
      *    IF (BI-OWNBR < BEG-BRANCH OR BI-OWNBR > END-BRANCH)
      *       UNLOCK BI-FILE
      *       GO TO NEXT-BI.
      *    PERFORM DELETE-BI1-FILE.
      *    IF IO-FG = 9
      *       PERFORM IO-ERROR.
      *    GO TO NEXT-BI.

       EXIT-MAIN-ROUTINE.
           EXIT.

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXDLBI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXDLBI-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXDLBI-SCREEN UPON FXDLBI-WINDOW-HANDLE.
           MOVE FXDLBI-FORM-FIELDS TO WR-BUF.
           MOVE FXDLBI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXDLBI-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXDLBI-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXDLBI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BI1-FILE.
       COPY "LIBLP/LPBIGS_SQL.CPY".

       COPY "LIBLP/LPBI1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-BI1-FILE-ALL.  *>  EMBEDDED BI

           PERFORM DELETE-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW.   *> SWITCH TO UPDATE_CONN CONNECTION
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.BIFILE
            WHERE BIFILE.BI_OWNBR BETWEEN :QBI1-WBEG-OWNBR
                                      AND :QBI1-WEND-OWNBR
           END-EXEC.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BI1-FILE.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW.   *> SWITCH TO C1 CONNECTION
           PERFORM SQL-SWAP-UPDATE-CONNECTION.
      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXDLBI-SCREEN.          
           DESTROY FXDLBI-WINDOW-HANDLE.
           EXIT PROGRAM.
