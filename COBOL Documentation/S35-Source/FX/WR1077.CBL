       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WR1077.
       DATE-WRITTEN.    04/29/2015.
      ************************************************************************
      *                    (FX/WR1077.C)
      *
      *  DESCRIPTION: SET LN-SKIP-TRW TO " " IF IT WAS A "B"
      *               
      *    THE FX/BUYBAC PROGRAM FOR DEBT SALE BUY BACK SETS LN-SKIP-TRW TO
      *     A "B" TO METRO2 CAN IDENTIFY THEM TO WRITE A K2 SEGMENT.
      *    THIS FIX WILL GET RAN AFTER THE METRO2 IS RAN TO PUT THE SKIP
      *     FLAG BACK TO SPACES
      *
      * REV:
      *  150429 BAH NEW PR#1077
      * BAH 181203 GLOBAL LNFILE
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      ************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *      W O R K I N G - S T O R A G E     S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)FX/UNFREZ A15 02/02/06-10:19:34 06/25/12-13:57:31 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------

       01  LN-FEED           PIC 9(2) VALUE ZERO.

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S   
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  SV-BEG-BR           PIC 9(4)    VALUE 9999.
       01  SV-END-BR           PIC 9(4)    VALUE 9999.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S 
      *
      ******************************************************************
       01  WR1077-WINDOW-HANDLE   PIC X(10).
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/WR1077_DEF.CPY".
       COPY "LIBFX/WR1077_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/WR1077_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *     M A I N    M O D U L E
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WR1077" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS WR1077-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.

           STRING "PROGRAM IN PROGRESS......BRNO: " 
                     BR-NO
                       DELIMITED BY SIZE INTO MESS-BUF.
           PERFORM SEND-LEGEND.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B" TO LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.

           PERFORM PROCESS-LN-FILE.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *     I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           PERFORM ENTER-PARAMETERS.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO INITIALIZE-EXIT.
 
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
 
           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           GO TO INITIALIZE-EXIT.

      *-----------------------------------------------------------------
       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

      *-----------------------------------------------------------------
       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    ENTER REPORT PARAMETERS
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO INIT-ACCEPT-ACCEPT.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR NOT > END-BR GO TO INIT-ACCEPT-ACCEPT.
           PERFORM INVALID-RANGE.
           GO TO BR-01.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       INIT-ACCEPT-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              GO TO BR-02.

           IF VDUSTATUS NOT = "00"
              GO TO INIT-ACCEPT-ACCEPT.

           IF VDUBUF = "N" GO TO BR-01.
           IF VDUBUF NOT = "Y" GO TO INIT-ACCEPT-ACCEPT.

      *-----------------------------------------------------------------
       EXIT-PARM.
           EXIT.

      ******************************************************************
       PROCESS-LN-FILE SECTION.
      ******************************************************************
           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.


           PERFORM START-LN1-FILE.

       PROCESS-LN-FILE-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-LN-FILE-EXIT.

           IF LN-SKIP-TRW = "B"
              MOVE " " TO LN-SKIP-TRW
              PERFORM REWRITE-LN1-FILE.
      *    ELSE
      *       UNLOCK LN-FILE.

           GO TO PROCESS-LN-FILE-NEXT.

       PROCESS-LN-FILE-EXIT.
           PERFORM CLOSE-LN1-FILE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".


      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO WR1077-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WR1077-SYSDATE.
           DISPLAY WR1077-SCREEN UPON WR1077-WINDOW-HANDLE.
           MOVE WR1077-FORM-FIELDS TO WR-BUF.
           MOVE WR1077-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE WR1077-HELP--FILE TO HELP-LINK-FILE.
           MOVE WR1077-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/WR1077_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     M I S C E L L A N E O U S      P A R A G R A P H S  
      *
      ******************************************************************
       MISC-PARA SECTION.
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    I O    R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D    R O U T I N E 
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WR1077-SCREEN.
           DESTROY WR1077-WINDOW-HANDLE.
           EXIT PROGRAM.
