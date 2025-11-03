       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXGBDT.
       AUTHOR.          CLS.
       DATE-WRITTEN.    08-21-2019
      ******************************************************************
      *
      *  DESCRIPTION:  SET MONTHEND MMYY, CALENDAR MMYY AND
      *                FISCAL MMYY DATES IN GLOBAL FILE
      *
      * REV:
      * CLS 2019-0821 PT#0174
      * BAH 2019-0918 REMOVED ACCESS-CALL ON GBFILE
      * BAH 2019-1002 FIXED SETTING THE UPDATE FIELDS (DATE/TIME/USERID)
      *               IN THE EOYC & EOYF AREAS.
      ******************************************************************
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
           "@(#)FX/FXGBDT.CBL S34 05/21/21-14:59:40 >".
      ******************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      ******************************************
       01  PR-IFN             PIC X(2) VALUE "PR".
       01  SAVE-LOGNAME       PIC X(10) VALUE SPACES.
 
       01  MESS-LIST         PIC X(5) VALUE "MESS.".

       01  DATE-SW           PIC XXX  VALUE "   ".
           88 BAD-DATE                VALUE "BAD".
 
       01  WS-DATE-YYMMDD            PIC 9(6).
       01  WS-DATE-X                 REDEFINES WS-DATE-YYMMDD.
           03  FILLER                          PIC XX.
           03  WS-MM                           PIC 99.
           03  WS-DD                           PIC 99.
      **********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  LAST-MONTH-MMYY     PIC 9(4).
           03  LAST-FISCAL-MMYY    PIC 9(4).
           03  LAST-CALENDAR-MMYY  PIC 9(4).
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXGBDT_DEF.CPY".
       COPY "LIBFX/FXGBDT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       01  MSEL-LIST            PIC X(5)  VALUE "MSEL.".
       01  LEGEND               PIC X(70).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.
 
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXGBDT_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FXGBDT" TO VDU-FORM-NAME.
          
           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO SAVE-LOGNAME.
                     
           PERFORM INITIAL-ROUTINE.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-ROUTINE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
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
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              MOVE 0 TO BR-NO
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
           MOVE "B"                        TO GB-PATH-OVERRIDE.

           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.             
           IF IO-FG NOT = 0
              GO TO END-OF-GROUP.

           MOVE LAST-MONTH-MMYY           TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM               TO GB-EOM-TAG-CCYYMM.

           MOVE LAST-FISCAL-MMYY          TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM               TO GB-FISCAL-EOY-TAG-CCYYMM.

           MOVE LAST-CALENDAR-MMYY        TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM               TO GB-CALENDAR-EOY-TAG-CCYYMM.

           MOVE EXT-CONAME-SYSDATE        TO ALP-DATE.             
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO GB-EOM-UPDATE-DATE
                                             GB-EOYF-UPDATE-DATE
                                             GB-EOYC-UPDATE-DATE.

           PERFORM GET-TIME.
           MOVE HH-MM-SS                  TO GB-EOM-UPDATE-TIME
                                             GB-EOYF-UPDATE-TIME
                                             GB-EOYC-UPDATE-TIME.

           MOVE SAVE-LOGNAME              TO GB-EOM-UPDATE-USERID
                                             GB-EOYF-UPDATE-USERID
                                             GB-EOYC-UPDATE-USERID.

           PERFORM REWRITE-GB-FILE.
 
       END-OF-GROUP.
           PERFORM CLOSE-GB-FILE.

           GO TO NEXT-GROUP-LOC.

      ***********************************
      *   INITIAL ROUTINE
      ***********************************
       INITIAL-ROUTINE SECTION.
           MOVE FXGBDT-QUEUE-POS TO Q-POS.
           MOVE FXGBDT-COPIES-POS TO C-POS.
           MOVE FXGBDT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GB-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-LAST-CALENDAR.

       BR-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING BRANCH"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
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
              MOVE 0 TO WGR-SRESULT
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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING BRANCH" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       ENTER-LAST-MONTH.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER LAST MONTH END MMYY"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN040MONTHEND." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-LAST-MONTH.
           PERFORM DATE-CHECK-MMYY.
           IF BAD-DATE
                GO TO ENTER-LAST-MONTH.

           MOVE VDUNUM0 TO LAST-MONTH-MMYY.

       ENTER-LAST-FISCAL.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER LAST FISCAL YEAR" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN040FISCAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-LAST-MONTH.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-LAST-FISCAL.
           PERFORM DATE-CHECK-MMYY.
           IF BAD-DATE
                GO TO ENTER-LAST-FISCAL.

           MOVE VDUNUM0 TO LAST-FISCAL-MMYY.

       ENTER-LAST-CALENDAR.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER LAST CALENDAR YEAR"
                                  TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN040CALENDAR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-LAST-FISCAL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO
                                      ENTER-LAST-CALENDAR.
           PERFORM DATE-CHECK-MMYY.
           IF BAD-DATE
                GO TO ENTER-LAST-CALENDAR.
           MOVE VDUNUM0 TO LAST-CALENDAR-MMYY.

       EXIT-PARM.
           EXIT.

       DATE-CHECK-MMYY SECTION.
           MOVE SPACES TO DATE-SW.
           MOVE VDU-NUM TO WS-DATE-YYMMDD.

           IF ( WS-MM = ZEROES ) AND ( WS-DD = ZEROES )
              GO TO DATE-CHECK-MMYY-EXIT.

           IF ( WS-MM < 1 ) OR ( WS-MM > 12 )
              MOVE "BAD MONTH" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW.

       DATE-CHECK-MMYY-EXIT.
           EXIT.


      ***********************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
        



      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO FXGBDT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXGBDT-SYSDATE.
           DISPLAY FXGBDT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FXGBDT-FORM-FIELDS TO WR-BUF.

           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE FXGBDT-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXGBDT-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXGBDT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY". 
       COPY "LIBGB/TIMEIO.CPY".

      ******************************
      *   MISCELLANEOUS PARAGRAPHS *
      ******************************
       MISC-PARA SECTION.
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
      *COPY "LIBGB/UPPER_CMD.CPY".
       SEND-LEGEND.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.
       SEND-MSEL.
           MOVE MSEL-LIST TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
 
 
      /********************************
      *      F I L E   I / O          *
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
       END-ROUTINE SECTION.
       END-PROGRAM.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "UP/UPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY FXGBDT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
