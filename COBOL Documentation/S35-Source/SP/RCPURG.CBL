      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCPURG.
       AUTHOR.        CS.
       DATE-WRITTEN.  05-05-2003.
      ******************************************************************
      *
      *  IDENTIFICATION: SP/RCPURG.C
      *
      *  DESCRIPTION   : PURGE RCFILE BY BRANCH, MONTHS OLD TO PURGE
      *
      *-----------------------------------------------------------------
      * REV:
      * CS  030505    ***NEW***    [PR# 2069]  KEVIN MULLEN
      * KEC 2015.1123 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  20171015 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  20200807 BAH FIXED MESSAGE-PURGE-DT DATE
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/RCPURG.CBL S34 06/21/21-10:03:20 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".

      *=================================================================
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      *=================================================================
       01  LEGEND                      PIC X(40)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  PURGE-FROM-DATE             PIC 9(8)      VALUE ZEROS.
       01  MESSAGE-MONTHS-MINIMUM      PIC 99        VALUE ZEROS.
       01  MESSAGE-PURGE-DATE.
           03  FILLER                  PIC X(11) VALUE
              "RC'S FROM  ".
           03  MESSAGE-PURGE-DT        PIC 99/99/99.
           03  FILLER                  PIC X(26) VALUE
              " AND PRIOR WILL BE PURGED.".

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-FEED          PIC 99       VALUE 0.


      *========================================================
      *  S C R E E N   -   F I E L D S
      *========================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  PURGE-CURRENT-DATE      PIC 9(8).
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  PURGE-MONTHS-OLD        PIC 99.
               88  PURGE-MONTHS-VALID  VALUE  12 THRU 99.
           03  GROUP-OPTIONS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

       01  VERSION-CONTROL         PIC X   VALUE "A".

      *=================================================================
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      *=================================================================
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/RCPURG_DEF.CPY".
       COPY "LIBSP/RCPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/RCPURG_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM PRIMARY-DRIVER-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RCPURG"      TO FORM-PROG VDU-FORM-NAME.


           MOVE RCPURG-QUEUE-POS TO Q-POS.
           MOVE RCPURG-COPIES-POS TO C-POS.
           MOVE RCPURG-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
      *-----------------------------------------------------------------
       COPY "LIBGB/EOINIT.CPY".
      *-----------------------------------------------------------------
       INITIALIZATION-CONTINUE.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROGRAM IN PROGRESS.................." TO LEGEND
              PERFORM SEND-LEGEND.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO PURGE-CURRENT-DATE.
           PERFORM FIND-PURGE-FROM-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES TO EXT-EOBUF-ERRCD.

           IF UP-KEY GO TO ENTER-PURGE-MONTHS-OLD.

       DISPLAY-PURGE-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
               GO TO ENTER-BEG-BRANCH.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PURGE-CURRENT-DATE.
           MOVE "S  8000PURGE-DTE." TO VDU.
           MOVE DATE-YYYYMMDD TO PURGE-CURRENT-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------
       ENTER-BEG-BRANCH.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

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
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

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

      *----------------------------------------------------
       ENTER-PURGE-MONTHS-OLD.

           MOVE "ENNN020PURGE-MONTHS." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PURGE-MONTHS-OLD.

           MOVE  VDUNUM0     TO PURGE-MONTHS-OLD.

           IF PURGE-MONTHS-OLD  < 12
              MOVE 12 TO MESSAGE-MONTHS-MINIMUM
              STRING "MONTHS OLD CAN NOT BE LESS THAN ",
                                          MESSAGE-MONTHS-MINIMUM
              DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PURGE-MONTHS-OLD.

           PERFORM FIND-PURGE-FROM-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE PURGE-FROM-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY        TO MESSAGE-PURGE-DT
              MOVE MESSAGE-PURGE-DATE TO MESS
              PERFORM SEND-MESS.

           GO TO ENTER-PARAMETERS-EXIT.

       ENTER-PARAMETERS-END.

           MOVE "X" TO EXT-EOBUF-ERRCD.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  N020PURGE-MONTHS." TO VDU.
           MOVE PURGE-MONTHS-OLD    TO VDUNUM0.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
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

      *=================================================================
      *    LOOPING TO GET NEXT GROUP OR BRANCH.
      *=================================================================
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-RECORD.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD         ) OR
              ( BR-NO > END-BRANCH )
              GO TO PRIMARY-DRIVER-MODULE-END.

       MOVE-BRANCH-PATH-INFO.

           PERFORM PROCESS-RC-FILE.

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

           MOVE BR-NO    TO RC-BRNO.
           MOVE 19000101 TO RC-TRANS-DATE.

           PERFORM START-RC1-FILE.

       PROCESS-RC-FILE-NEXT.
           PERFORM READ-RC1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-PROGRAM.

           IF RC-BRNO NOT = BR-NO
              GO TO PROCESS-RC-FILE-EXIT.

           IF RC-STATUS = "A1" OR "B1"
              GO TO PROCESS-RC-FILE-NEXT.

           IF RC-TRANS-DATE > PURGE-FROM-DATE
              GO TO PROCESS-RC-FILE-EXIT.

      *---------------------------------------------------------------
           PERFORM DELETE-RC1-FILE.
      *---------------------------------------------------------------

           GO TO PROCESS-RC-FILE-NEXT.

       PROCESS-RC-FILE-EXIT.
           EXIT.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".



      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO RCPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RCPURG-SYSDATE.
           DISPLAY RCPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RCPURG-FORM-FIELDS TO WR-BUF.
           MOVE RCPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE RCPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE RCPURG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/RCPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       FIND-PURGE-FROM-DATE.
           MOVE PURGE-CURRENT-DATE TO NDTE-DATE.
           MULTIPLY PURGE-MONTHS-OLD BY -1 GIVING NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO PURGE-FROM-DATE NUM-DATE
                             DATE-YYYYMMDD.

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
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RCPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
