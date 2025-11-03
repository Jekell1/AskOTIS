       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TR1697.
       DATE-WRITTEN.    01/17/2025.
      ****************************************************************
      *
      *
      *  DESCRIPTION: CLEAR TP-CLOSFEE-REB-AMT
      *               4% CLOSING FEE FOR "GA"
      *
      * REV;
      *  2025.0109 BAH #1697 CLEAR TP-CLOSFEE-REB-AMT S35Q-157
      ****************************************************************
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

      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/TR1697.CBL S35 01/17/25-14:17:16 >".

       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  PR-IFN            PIC X(2) VALUE "PR".

       01  BEG-BRANCH             PIC 9999 VALUE 0.
       01  END-BRANCH             PIC 9999 VALUE 0.
       01  MONTH-YEAR        PIC 9(4).
       01  MONTH-X REDEFINES MONTH-YEAR.
           03  MONTH-MM      PIC 99.
           03  YEAR-YY       PIC 99.

       01  ENT-GROUP         PIC X(4).
       01  GROUP-FLAG        PIC X.
       01  ACCEPT-BUF        PIC X.

       01  ERRCD           PIC X.


       01  MESS-LIST               PIC X(5) VALUE "MESS.".
       01  MESS-BUF                PIC X(40) VALUE SPACES.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       01  TR1697-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/TR1697_DEF.CPY".
       COPY "LIBFX/TR1697_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/TR1697_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE "TR1697" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TR1697-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE " " TO ERRCD.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

       GETPR.
           MOVE TR1697-QUEUE-POS TO Q-POS.
           MOVE TR1697-COPIES-POS TO C-POS.
           MOVE TR1697-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-ROUTINE.

       ENTER-BR1.
           MOVE "E  A040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS = "16" GO TO ENTER-BR-ALL.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BR1
              ELSE
                 MOVE "S  A040BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BR1.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BR1.

           IF WGR-TYPE = "A"
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"  TO GROUP-FLAG
              MOVE 0    TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BR2.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-BR1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BR2.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO ENTER-BR1.

           GO TO CHECK-BRANCH-SECURITY.

       ENTER-BR-ALL.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO ENTER-BR1.

       ENTER-MONTH-YEAR.
           MOVE "ENNN040MONTH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-BR1.
           IF VDUSTATUS = "1U" GO TO ENTER-BR2.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-MONTH-YEAR.
           MOVE VDUNUM0 TO MONTH-YEAR.

       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-MONTH-YEAR.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO END-ROUTINE.
           IF NOT (ACCEPT-BUF = "Y" )
              GO TO ENTER-ACCEPT.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRP-PATH-OVERRIDE.

           PERFORM PROCESS-BRANCH.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************
       PROCESS-BRANCH SECTION.
      ******************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
      * FIRST DAY OF MONTH
           MOVE MONTH-MM            TO DATE-MMDDYY-MM.
           MOVE 01                  TO DATE-MMDDYY-DD.
           MOVE YEAR-YY             TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
      * TO LAST DAY OF MONTH
           MOVE MONTH-MM            TO DATE-MMDDYY-MM.
           MOVE 33                  TO DATE-MMDDYY-DD.
           MOVE YEAR-YY             TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE
           PERFORM NEW-DATE-CALCULATION
           MOVE NDTE-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE 0                   TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE ALL "9"             TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS-BUF.
           PERFORM SEND-LEGEND.

       PROCESS-NEXT-TR.
           PERFORM READ-TRP1-FILE-NEXT.
           IF ( IO-FG = 9 ) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO PROCESS-BRANCH-EXIT.

           MOVE TP-DATE TO DATE-YYYYMMDD.
           IF DATE-YYYYMMDD-MM NOT = MONTH-MM OR
               DATE-YYYYMMDD-YY NOT = YEAR-YY
              GO TO PROCESS-NEXT-TR.

           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE 0 TO TP-CLOSFEE-REB-AMT.

           PERFORM REWRITE-TRP1-FILE.

           GO TO PROCESS-NEXT-TR.

       PROCESS-BRANCH-EXIT.
           PERFORM CLOSE-TRP1-FILE.


      ******************************************************************
       SEND-PROGRESS-INDICATOR SECTION.
      ******************************************************************

           MOVE BR-NO                  TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE SPACES                 TO PROGID-ROUTE-BUF.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           PERFORM PROGID-DISPLAY.

       SEND-PROGID-INDICATOR-EXIT.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/PROGID.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO TR1697-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TR1697-SYSDATE.
           DISPLAY TR1697-SCREEN UPON TR1697-WINDOW-HANDLE.
           MOVE TR1697-FORM-FIELDS TO WR-BUF.
           MOVE TR1697-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE TR1697-HELP--FILE TO HELP-LINK-FILE.
           MOVE TR1697-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/TR1697_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS-BUF  TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************
       END-PROGRAM SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TR1697-SCREEN.
           DESTROY TR1697-WINDOW-HANDLE.
           EXIT PROGRAM.
