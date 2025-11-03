       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AULIST.
       DATE-WRITTEN.    20220208.
      **********************************************************************
      *                      (LS)
      *    DESCRIPTION:  AUFILE AUDIT CHANGES LISTING
      *
      *  REV:
      *   20220208 NEW #1542
      **********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
          SELECT EXTRACT-FILE ASSIGN OUTPUT EXTRACT-PATH
              ORGANIZATION LINE SEQUENTIAL
              LOCK MODE AUTOMATIC WITH LOCK ON RECORD
              FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      *****************************************************************
      *    EXTRACT FILE
      *****************************************************************
       FD  EXTRACT-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 500 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  EXTRACT-REC.
           03  EXTRACT-BYTE             PIC X OCCURS 500 TIMES.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/AULIST.CBL J35 12/21/23-09:19:10 >".

       COPY "LIBGB/GB01AU.CPY".
       COPY "LIBGB/GB01AU_SQL.CPY".
       COPY "LIBGB/GBWSAU.CPY".

      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".

       01  EXTRACT-PATH.
           03  EXTRACT-PATH1    PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER           PIC X(6) VALUE "AUDITW".

       01  EXTRACT-FIELDS.
           03  EXT-DATE         PIC 9999/99/99.
           03  EXT-TIME         PIC 99/99/99.
           03  EXT-LTOUCH-DATE  PIC 9999/99/99.

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  TOT-EXTRACTED        PIC 9(10)    VALUE 0.

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  FILE-NAME       PIC X(7) OCCURS 5.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  CREATE-EXTRACT  PIC X.

       01  VERSION-CONTROL     PIC X VALUE "A".

      ******************************************************************
      *     P R I N T   B U F F E R S
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(42)    VALUE
           "- - -  AUDIT CHANGES FILE LISTING - - - -".
           03  FILLER           PIC X(44)    VALUE " ".


       01  HEAD3.
           03  FILLER      PIC X(36)    VALUE
           "DATE        FILE         SEQ  BRNO  ".
           03  FILLER      PIC X(51)    VALUE "FILE KEY ".
           03  FILLER      PIC X(43)    VALUE
           "TICKET                 TIME   USERID   PROG".
      *     1234567890123456789012345678901234567890123

       01  HEAD3B.
           03  FILLER      PIC X(53)    VALUE
           "FIELD".

       01  HEAD4.
           03  H4-DASH      PIC X(132).

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-DATE             PIC 9999/99/99.
           03  FILLER             PIC X(2).
           03  D-FILE-NAME        PIC X(7).
           03  FILLER             PIC X(2).
           03  D-SEQ              PIC Z(5)99.
           03  FILLER             PIC X(2).
           03  D-BRANCH           PIC Z(4).
           03  FILLER             PIC X(2).
           03  D-FILE-KEY         PIC X(50).
           03  FILLER             PIC X.
           03  D-TICKET           PIC X(20).
           03  FILLER             PIC X.
           03  D-TIME             PIC 99/99/99.
           03  FILLER             PIC X.
           03  D-USERID           PIC X(8).
           03  FILLER             PIC X.
           03  D-PROGRAM          PIC X(6).
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-FIELD-DESC       PIC X(30).
           03  FILLER             PIC X(2).
           03  D-FIELD-TAG-ORIG   PIC X(6).
           03  D-VALUE            PIC X(80).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(50).
           03  RANGE-SELECTION  PIC X(15).
           03  RANGE-NUMBER REDEFINES RANGE-SELECTION.
               05  R-NUM        PIC Z(10).
               05  FILLER       PIC X(5).
           03  RANGE-DATE REDEFINES RANGE-SELECTION.
               05 RANGE-DT      PIC 99/99/99.
               05 FILLER        PIC X(7).


       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/AULIST_DEF.CPY".
       COPY "LIBLS/AULIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLS/AULIST_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE EXTRACT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-AU1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE EXTRACT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AULIST" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      * ******* = F3 ALL CODES
           IF FILE-NAME(1) = "*******"
              MOVE SPACES TO AU-FILE-NAME
           ELSE
              IF FILE-NAME(1) NOT = SPACES AND
                  (FILE-NAME(2) = SPACES AND FILE-NAME(3) = SPACES AND
                    FILE-NAME(4) = SPACES AND FILE-NAME(5) = SPACES)
                 MOVE FILE-NAME(1)  TO AU-FILE-NAME
              ELSE
                 MOVE SPACES TO AU-FILE-NAME.

           MOVE AU-FILE-NAME        TO QAU1-WBEG-FILE-NAME.
           IF FILE-NAME(1) = SPACES
      * F3-ALL
              MOVE ALL "Z"          TO QAU1-WEND-FILE-NAME
           ELSE
              MOVE FILE-NAME(1)     TO QAU1-WEND-FILE-NAME.

           MOVE BEG-DATE  TO AU-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QAU1-WBEG-DATE
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QAU1-WEND-DATE.
           MOVE 0                   TO AU-SEQ
                                       QAU1-WBEG-SEQ.
           MOVE ALL "9"             TO QAU1-WEND-SEQ.

           PERFORM START-AU1-FILE.

       NEXT-AU.
           PERFORM READ-AU1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF (FILE-NAME(1) NOT = "*******") AND
               (FILE-NAME(1) NOT = SPACES) AND
                (FILE-NAME(2) = SPACES AND FILE-NAME(3) = SPACES AND
                  FILE-NAME(4) = SPACES AND FILE-NAME(5) = SPACES)
                  IF AU-FILE-NAME NOT = FILE-NAME(1)
                     GO TO END-ROUTINE.

           IF (FILE-NAME(1) NOT = "*******") AND
                FILE-NAME(1) NOT = AU-FILE-NAME AND
                  FILE-NAME(2) NOT = AU-FILE-NAME AND
                   FILE-NAME(3) NOT = AU-FILE-NAME AND
                    FILE-NAME(4) NOT = AU-FILE-NAME AND
                     FILE-NAME(5) NOT = AU-FILE-NAME
              GO TO NEXT-AU.

           IF AU-DATE < BEG-DATE OR AU-DATE > END-DATE
              GO TO NEXT-AU.

      **************************************************
           PERFORM DETAIL-LINE-PROCESS.
      **************************************************

           IF CREATE-EXTRACT = "Y"
              PERFORM CREATE-EXTRACT-FILE.



           MOVE "N" TO FIRST-TIME.

           GO TO NEXT-AU.

       END-ROUTINE.
           IF FIRST-TIME = "Y" AND EXT-ROUTE-BUF = "00"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           IF FIRST-TIME = "Y"
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************************
       INITIALIZATION SECTION.
      ***********************************************
           MOVE AULIST-QUEUE-POS TO Q-POS.
           MOVE AULIST-COPIES-POS TO C-POS.
           MOVE AULIST-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE EXT-CONAME-CONAME TO H-NAME.
           CALL "C$JUSTIFY" USING H-NAME "C".
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF CREATE-EXTRACT = "Y"
              CALL "C$TOLOWER" USING EXTRACT-PATH1, VALUE 13
              MOVE EXTRACT-PATH1 TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT-BAD
                 CALL "C$MAKEDIR" USING ACCESS-BUF GIVING STAT-99
              END-IF
              PERFORM OPEN-EXTRACT-FILE-OUTPUT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-AU1-FILE.
           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ****************************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ****************************************************
       ENTER-PARAMETERS SECTION.
      ****************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-EXTRACT.

       CODE-01.
           MOVE "E  A070CODE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CODES.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CODE-01.
           MOVE VDUBUF TO FILE-NAME(1).
           IF FILE-NAME(1) = SPACES GO TO CODE-01.
       CODE-02.
           MOVE "E  A070CODE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CODE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CODE-02.
           MOVE VDUBUF TO FILE-NAME(2).
       CODE-03.
           MOVE "E  A070CODE3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CODE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CODE-03.
           MOVE VDUBUF TO FILE-NAME(3).
       CODE-04.
           MOVE "E  A070CODE4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CODE-03.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CODE-04.
           MOVE VDUBUF TO FILE-NAME(4).
       CODE-05.
           MOVE "E  A070CODE5." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CODE-04.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CODE-05.
           MOVE VDUBUF TO FILE-NAME(5).

           GO TO DT-01.

       ALL-CODES.
           MOVE "S  A070CODE1." TO VDU.
           MOVE "*******"       TO VDUBUF FILE-NAME(1).
           PERFORM SCREENIO.

           MOVE "S  A070CODE2." TO VDU.
           MOVE SPACES          TO VDUBUF FILE-NAME(2).
           PERFORM SCREENIO.
           MOVE "S  A070CODE3." TO VDU.
           MOVE SPACES          TO VDUBUF FILE-NAME(3).
           PERFORM SCREENIO.
           MOVE "S  A070CODE4." TO VDU.
           MOVE SPACES          TO VDUBUF FILE-NAME(4).
           PERFORM SCREENIO.
           MOVE "S  A070CODE5." TO VDU.
           MOVE SPACES          TO VDUBUF FILE-NAME(5).
           PERFORM SCREENIO.


       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO CODE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO ENTER-EXTRACT.

       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-EXTRACT.
           MOVE "E  A010EXTRACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO CODE-05
              ELSE
                 GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-EXTRACT.
           MOVE VDUBUF TO CREATE-EXTRACT.
           IF CREATE-EXTRACT NOT = "Y" AND NOT = "N"
              GO TO ENTER-EXTRACT.

       END-IT.

       EXIT-PARM.
           EXIT.

      *******************************************
       DISPLAY-PARAMETERS SECTION.
      *******************************************
           MOVE "S  A070CODE1." TO VDU.
           MOVE FILE-NAME(1)    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A070CODE2." TO VDU.
           MOVE FILE-NAME(2)    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A070CODE3." TO VDU.
           MOVE FILE-NAME(3)    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A070CODE4." TO VDU.
           MOVE FILE-NAME(4)    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A070CODE5." TO VDU.
           MOVE FILE-NAME(5)    TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************

           MOVE AU-DATE         TO D-DATE.
           MOVE AU-FILE-NAME    TO D-FILE-NAME.
           MOVE AU-SEQ          TO D-SEQ.
           MOVE AU-BRANCH       TO D-BRANCH.
           MOVE AU-FILE-KEY     TO D-FILE-KEY.
           MOVE AU-TICKET       TO D-TICKET.
           MOVE AU-TIME         TO D-TIME.
           INSPECT D-TIME REPLACING ALL "/" BY ":".
           MOVE AU-USERID       TO D-USERID.
           MOVE AU-PROGRAM      TO D-PROGRAM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE AU-FIELD-DESC    TO D-FIELD-DESC.
           IF AU-ACTION = "C"
              MOVE "ORIG: "      TO D-FIELD-TAG-ORIG
              MOVE AU-ORIG-VALUE TO D-VALUE.
           PERFORM WRITE-DETAIL-LINE.

           IF AU-ACTION = "C"
              MOVE "NEW : "      TO D-FIELD-TAG-ORIG
              MOVE AU-NEW-VALUE  TO D-VALUE
              PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.

      **************************************************
       CREATE-EXTRACT-FILE SECTION.
      **************************************************
           MOVE AU-DATE TO EXT-DATE.
           MOVE AU-TIME TO EXT-TIME.
           INSPECT EXT-TIME REPLACING ALL "/" BY ":".
           MOVE AU-LTOUCH-DATE TO EXT-LTOUCH-DATE.

           MOVE SPACES TO EXTRACT-REC.
           STRING 
                  AU-FILE-NAME, X'09',
                  EXT-DATE, X'09',
                  AU-SEQ, X'09',
                  AU-ACTION, X'09',
                  AU-FILE-TYPE, X'09',
                  AU-BRANCH, X'09',
                  AU-FILE-KEY, X'09',
                  AU-FIELD-DESC, X'09',
                  AU-ORIG-VALUE, X'09',
                  AU-NEW-VALUE, X'09',
                  AU-TICKET, X'09',
                  AU-PROGRAM, X'09',
                  EXT-TIME, X'09',
                  AU-USERID, X'09',
                  EXT-LTOUCH-DATE
               DELIMITED BY "   " INTO EXTRACT-REC.
           PERFORM WRITE-EXTRACT-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           ADD 1 TO TOT-EXTRACTED.

      *********************************************
       WRITE-DETAIL-LINE SECTION.
      *********************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *********************************************
       HEAD-PAGE SECTION.
      *********************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3B TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "=" TO H4-DASH.
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 8 TO LN-COUNT.

       END-HEAD-PAGE.
           EXIT.

      *************************************
       PRINT-RANGE-LINES SECTION.
      *************************************
           MOVE EXT-CONAME-RPT-LINES     TO LN-COUNT.

           MOVE 4 TO LN-FEED.
           MOVE "CODE 1"          TO RANGE-LINE.
           MOVE FILE-NAME(1)      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "CODE 2"          TO RANGE-LINE.
           MOVE FILE-NAME(2)      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "CODE 3"          TO RANGE-LINE.
           MOVE FILE-NAME(3)      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "CODE 4"          TO RANGE-LINE.
           MOVE FILE-NAME(4)      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "CODE 5"          TO RANGE-LINE.
           MOVE FILE-NAME(5)      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE"        TO RANGE-LINE.
           MOVE BEG-DATE          TO RANGE-DT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END DATE"        TO RANGE-LINE.
           MOVE END-DATE          TO RANGE-DT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXTRACT ?"        TO RANGE-LINE.
           MOVE CREATE-EXTRACT     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           IF CREATE-EXTRACT = "Y"
              MOVE "TOTAL EXTRACTED" TO RANGE-LINE
              MOVE TOT-EXTRACTED TO R-NUM
              PERFORM WRITE-DETAIL-LINE.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO AULIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AULIST-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY AULIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AULIST-FORM-FIELDS TO WR-BUF. 
           MOVE AULIST-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE AULIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE AULIST-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/AULIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARA SECTION.
      **********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".

      **********************************
       IO-ROUTINES SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBAUGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBAU1RN.CPY".
      *-----------------------------------------------------------------
       OPEN-EXTRACT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE EXTRACT-PATH TO E-FILE.
           OPEN OUTPUT EXTRACT-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-EXTRACT-FILE-OUTPUT.
           IF ( IO-FG = 7 )
              CLOSE EXTRACT-FILE
              GO TO OPEN-EXTRACT-FILE-OUTPUT.

      *-----------------------------------------------------------------
       WRITE-EXTRACT-FILE.
           PERFORM WRITE-IT.
           MOVE EXTRACT-PATH TO E-FILE.
           WRITE EXTRACT-REC.
           IF ( IO-FG = 8 )
              GO TO WRITE-EXTRACT-FILE.
      *-----------------------------------------------------------------
       CLOSE-EXTRACT-FILE.
           PERFORM CLOSE-IT.
           CLOSE EXTRACT-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AULIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
