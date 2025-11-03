       IDENTIFICATION DIVISION.
       PROGRAM-ID.       RE1098.
       DATE-WRITTEN.     120390.
      *****************************************************
      *                      (LP)
      *   DESCRIPTION:  REAL ESTATE INTEREST PAID 1098 EXTRACTION
      *                 BORROWER FILE
      *
      *          * * NOTE NOT GLOBAL! * *
      * REV:
      *  040120 BAH FM-BRNO WAS ZERO, SO LONPMC WAS GETTING A READ 23
      *             ON THE BRFILE...MOVE POST-USERID-BR TO FM-BRNO
      *
      * KEC 2016.0217 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *  BAH 170712 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181001 GLOBAL BWFILE
      *  BAH 181115 GLOBAL FMFILE
      *  BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      *  BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/RE1098.CBL S34 05/21/21-15:11:29 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.
       01  TRANS-DATE             PIC 9(8).

      **********************
      *   LINE WORKERS     *
      **********************
      *01  LN-FEED          PIC 99       VALUE 0.
      *01  LN-COUNT         PIC 99       VALUE 0.
      *01  PAGE-NUMBER      PIC 9(4)     VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-INT          PIC 9(6).
           03  END-INT          PIC 9(6).
           03  WHICHYEAR        PIC X.
           03  FORMNO           PIC 999.

       01  VERSION-CONTROL      PIC X VALUE "A".

       01  ERRCD                PIC X VALUE " ".
       01  MESS-LIST            PIC X(5) VALUE "MESS.".

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/RE1098_DEF.CPY".
       COPY "LIBLP/RE1098_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/RE1098_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************************
       MAIN-MODULE SECTION.
      *****************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "RE1098" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-FM1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.


           PERFORM START-BW1-FILE.

       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO END-PROGRAM.

           IF WHICHYEAR = "C"
              IF BW-RE-INTYTD < BEG-INT OR BW-RE-INTYTD > END-INT
                 GO TO NEXT-BW.
           IF WHICHYEAR = "L"
              IF BW-RE-INTLYR < BEG-INT OR BW-RE-INTLYR > END-INT
                  GO TO NEXT-BW.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY.
           MOVE BW-SSNO                TO LPWSFM-EDIT-FM-SSNO.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FORMNO                 TO FM-FMNO.

           MOVE FM1-KEY                TO HOLD-FM1-KEY.

           PERFORM READ-FM1-FILE.

           MOVE "N"                TO FM-PRINT.
           MOVE BW-ZIP             TO FM-ZIP.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           IF IO-FG = 0
      * PREVIOUS REQUEST OUTSTANDING
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE.

           PERFORM CLOSE-FM1-FILE.

           GO TO NEXT-BW.

      ******************************************************
       INITIALIZATION SECTION.
      ******************************************************
           MOVE RE1098-QUEUE-POS TO Q-POS.
           MOVE RE1098-COPIES-POS TO C-POS.
           MOVE RE1098-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-FD1-FILE.
           MOVE FORMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REAL ESTATE PAID EXTRACTION IN PROGRESS" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.


      **************************************
       EO-EXEC-AUDIT SECTION.
      **************************************

      **************************************
       ENTER-PARAMETERS SECTION.
      **************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO FMNO.

       BEGIN.
           MOVE "ENNN060BEGINT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLINT.
           IF VDUSTATUS = "1U" GO TO BEGIN.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN.
           MOVE VDUNUM0 TO BEG-INT.
           GO TO ENDIN.
       ALLINT.
           MOVE "SNNN060BEGINT." TO VDU.
           MOVE 0 TO BEG-INT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDINT." TO VDU.
           MOVE 999999 TO END-INT VDUNUM0.
           PERFORM SCREENIO.
           GO TO WHYEAR.
       ENDIN.
           MOVE "ENNN060ENDINT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BEGIN.
           IF VDUSTATUS = "1U" GO TO BEGIN.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDIN.
           MOVE VDUNUM0 TO END-INT.
           IF BEG-INT > END-INT
              MOVE "INVALID INTEREST PAID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN.
       WHYEAR.
           MOVE "E  A010YEAR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BEGIN.
           IF VDUSTATUS = "1U" GO TO ENDIN.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO WHYEAR.
           MOVE VDUBUF TO WHICHYEAR.
           IF WHICHYEAR = "C" GO TO FMNO.
           IF WHICHYEAR NOT = "L" GO TO WHYEAR.
       FMNO.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BEGIN.
           IF VDUSTATUS = "1U" GO TO WHYEAR.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FMNO.
           MOVE VDUNUM0 TO FORMNO.
           PERFORM OPEN-FD1-FILE.
           MOVE FORMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO FMNO.

       ACCPT.

       EXIT-PARM.
           EXIT.

      **************************************
       DISPLAY-PARAMETERS SECTION.
      **************************************

           MOVE "SNNN060BEGINT." TO VDU.
           MOVE BEG-INT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDINT." TO VDU.
           MOVE END-INT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010YEAR." TO VDU.
           MOVE WHICHYEAR TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SNNN030FORMNO." TO VDU.
           MOVE FORMNO TO VDUNUM0.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO RE1098-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RE1098-SYSDATE.
           DISPLAY RE1098-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RE1098-FORM-FIELDS TO WR-BUF.
           MOVE RE1098-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE RE1098-HELP--FILE TO HELP-LINK-FILE.
           MOVE RE1098-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/RE1098_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

      *********************************************************
       IO-ROUTINE SECTION.
      *********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************************************
       END-PROGRAM SECTION.
      *********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPEYMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RE1098-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
