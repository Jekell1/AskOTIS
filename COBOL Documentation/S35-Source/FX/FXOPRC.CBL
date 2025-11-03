       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FXOPRC.
       AUTHOR.        CS&KEC.
       DATE-WRITTEN.  08-23-1999.
      ******************************************************************
      *
      *  IDENTIFICATION: FX/FXOPRC.C
      *
      *  DESCRIPTION   : CREATING 'OPEN' RC STATUS (RCFILE)
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 1999.0823 ***NEW***
      * BLM 080709 CREATE OP FLAGS IN ED WHEN CREATING RC TO OPEN DAY
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSOP.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDOP.CPY".
 
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXOPRC.CBL S34 05/24/21-11:52:29 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
      *-----------------------------------------------------------------

      *=================================================================
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      *=================================================================
       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(40)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".
       01  RC-EXISTS                   PIC X         VALUE SPACES.

      *-----------------------------------------------------------------
       01  RANGE-WORKERS.
           03  BEG-BRANCH              PIC 9(4)     VALUE ZEROES.
           03  END-BRANCH              PIC 9(4)     VALUE ZEROES.
           03  RCDATE                  PIC 9(8)     VALUE ZEROES.
           03  GROUP-OPTIONS.
               05  GROUP-FLAG          PIC X(1)     VALUE SPACES.
               05  ENT-GROUP           PIC X(4)     VALUE SPACES.

       01  FXOPRC-WINDOW-HANDLE PIC X(10).

      *=================================================================
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      *=================================================================
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXOPRC_DEF.CPY".
       COPY "LIBFX/FXOPRC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
 
      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXOPRC_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 OP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE OP-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM PRIMARY-DRIVER-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FXOPRC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXOPRC-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE "PROGRAM IN PROGRESS...................." TO LEGEND.
           PERFORM SEND-LEGEND.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           MOVE SPACES TO ERRCD.

       ENTER-BEG-BRANCH.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.
       
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"                  TO GROUP-FLAG.
           MOVE WGR-SRESULT          TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-RCDATE.

           MOVE "ERNM060RCDATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-RCDATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDU-DATE-YYYYMMDD TO RCDATE.

      *=================================================================
       ENTER-ACCEPT.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-RCDATE.
           IF (        F1-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( VDUBUF = "N" ) GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-ACCEPT.

      *=================================================================
       ENTER-PARAMETERS-END.

           MOVE "X" TO ERRCD.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       SCAN-GR-FILE SECTION.
      *****************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE "1<" TO VDUSTATUS
              GO TO SCAN-GR-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE GRSCAN-KEY TO VDUBUF.

       SCAN-GR-FILE-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-DRIVER-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-RC1-FILE.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-RECORD.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-RECORD.

           IF ( GROUP-FLAG = "N" ) 
              GO TO NEXT-BRANCH-RECORD.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO PRIMARY-DRIVER-MODULE-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-RECORD.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD         ) OR
              ( BR-NO > END-BRANCH )
              GO TO PRIMARY-DRIVER-MODULE-END.

       MOVE-BRANCH-PATH-INFO.

           PERFORM PROCESS-RC-FILE.

           MOVE EXT-FILPATH-BASE    TO OPEN-PATH-BASE.
           MOVE BR-MACHINE          TO OPEN-PATH-MACHINE.
           MOVE BR-NO               TO OPEN-PATH-BRNO.
           MOVE RCDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO OPEN-PATH-TRDATE.
           MOVE OPEN-PATH-FORMAT TO OPEN-PATH
                                    ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-OP-FILE-OUTPUT.

           GO TO NEXT-GROUP-RECORD.

       PRIMARY-DRIVER-MODULE-END.

           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-RC1-FILE.

       PRIMARY-DRIVER-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-RC-FILE SECTION.
      ******************************************************************
           MOVE BR-NO  TO RC-BRNO.
           MOVE RCDATE TO RC-TRANS-DATE.
           
           PERFORM READ-RC1-FILE.
        
           IF IO-FG = 9
              MOVE "N" TO RC-EXISTS
           ELSE 
              MOVE "Y" TO RC-EXISTS.

           IF RC-EXISTS = "N"
              PERFORM CLEAR-RC-FILE
              MOVE BR-NO TO RC-BRNO
              MOVE RCDATE TO RC-TRANS-DATE
              MOVE "A1" TO RC-STATUS
              MOVE "PFSIMI SET A1" TO RC-ERROR-MSG
              PERFORM WRITE-RC1-FILE
           ELSE
              MOVE "A1" TO RC-STATUS
              MOVE "PFSIMI SET A1" TO RC-ERROR-MSG
              PERFORM REWRITE-RC1-FILE.         

       PROCESS-RC-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GBRCCL.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXOPRC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXOPRC-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXOPRC-SCREEN UPON FXOPRC-WINDOW-HANDLE.
           MOVE FXOPRC-FORM-FIELDS TO WR-BUF.
           MOVE FXOPRC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXOPRC-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXOPRC-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXOPRC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      **********************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBLP/LPOPIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXOPRC-SCREEN.
           DESTROY FXOPRC-WINDOW-HANDLE.
           EXIT PROGRAM.
