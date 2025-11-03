       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BATCHP.
       DATE-WRITTEN.  02-01-2018.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/BATCHP.C
      *
      * DESCRIPTION   :  BATCH FORM PRINT FOR LONPEX DOC FILE CREATE
      *                  CALLS LP/LONPEX
      *
      *  FOLLOW INSTRUCTIONS BELOW;
      *
      *     NOT ON A MENU, ONLY RUNS IN BATCH
      *
      *
      * REV:
      *  180201 BAH NEW WORLD PR#1301
      *  BAH 20190919 REMOVED ACCESS-CALL ON FMFILE
      *  BAH 20200702 MOVED OPEN-BR-FILE #TST001
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BATCHP.CBL S34 05/21/21-14:50:30 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".


      *-----------------------------------------------------------------
       01  WORLD-PATH.
           03  WORLD-PATH-BASE           PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  WORLD-PATH-MACHINE        PIC X(02).
           03  FILLER                    PIC X(11) VALUE "/ED/DOCEXT.".
           03  WORLD-YR                  PIC 99.
           03  WORLD-MO                  PIC 99.
           03  WORLD-DA                  PIC 99.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-BEG-FMNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING FORM NUMBER      ".
           03  FILLER PIC X(19) VALUE "                   ".
 
       01  LEGEND-END-FMNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G FORM NUMBER                 ".
           03  FILLER PIC X(19) VALUE "                   ".
 
       01  LEGEND-PRINT-STATUS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT,  FORM HAS B".
           03  FILLER PIC X(30) VALUE "EEN (P)RINTED, (U)NPRINTED OR ".
           03  FILLER PIC X(19) VALUE "(B)OTH             ".
 
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".
 
      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-FMNO                       PIC 9(04).
           03  END-FMNO                       PIC 9(04).
           03  FORM-PRINT-STATUS              PIC X(01).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BATCHP_DEF.CPY".
       COPY "LIBLP/BATCHP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BATCHP_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BATCHP"      TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH-BASE    TO WORLD-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WORLD-PATH-MACHINE.

           PERFORM OPEN-BR-FILE.

       COPY "LIBGB/EOINIT.CPY".

           MOVE "EXTRACTION IN PROGRESS" TO LEGEND.
           PERFORM SEND-LEGEND.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           MOVE "SN N040CORPBRNO." TO VDU.
           MOVE GP-CORP-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           IF GP-CORP-BRNO = 0
              MOVE "MUST HAVE A CORPORATE BRANCH #" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO ENTER-PARAMETERS-EXIT.

           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CORPORATE BRANCH #" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO ENTER-PARAMETERS-EXIT.

           IF ( UP-KEY )
              GO TO ENTER-FORM-PRINT-STATUS.

       ENTER-BEG-FMNO.

           MOVE LEGEND-BEG-FMNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-FMNO." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FMNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FMNO.

           MOVE VDUNUM0 TO BEG-FMNO.
           IF BEG-FMNO = 0
              GO TO ENTER-BEG-FMNO.

       ENTER-END-FMNO.

           MOVE LEGEND-END-FMNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-FMNO." TO VDU.
           PERFORM SCREENIO.
           IF (        UP-KEY ) GO TO ENTER-BEG-FMNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FMNO.

           MOVE VDUNUM0 TO END-FMNO.

           IF ( BEG-FMNO > END-FMNO )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FMNO.

           GO TO ENTER-FORM-PRINT-STATUS.

       ENTER-ALL-FMNO.

           MOVE "SNNN040BEG-FMNO." TO VDU.
           MOVE 1             TO VDUNUM0 BEG-FMNO.
           PERFORM SCREENIO.

           MOVE "SNNN040END-FMNO." TO VDU.
           MOVE 9999               TO VDUNUM0 END-FMNO.
           PERFORM SCREENIO.


       ENTER-FORM-PRINT-STATUS.

           MOVE LEGEND-PRINT-STATUS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRINT-STATUS." TO VDU.
           PERFORM SCREENIO.
           IF (        UP-KEY ) GO TO ENTER-END-FMNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-FORM-PRINT-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FORM-PRINT-STATUS.

           IF FORM-PRINT-STATUS NOT = "P" AND NOT = "R"
              GO TO ENTER-FORM-PRINT-STATUS.

       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

           MOVE "SNNN040BEG-FMNO."        TO VDU.
           MOVE BEG-FMNO                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-FMNO."        TO VDU.
           MOVE END-FMNO                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010PRINT-STATUS."    TO VDU.
           MOVE FORM-PRINT-STATUS         TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************

      * WORLD WILL ONLY CREATE A NEW FILE, IF IT EXISTS ALREADY IT
      * WILL EXIT
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY-MM TO WORLD-MO.
           MOVE DATE-MMDDYY-DD TO WORLD-DA.
           MOVE DATE-MMDDYY-YY TO WORLD-YR.
           MOVE WORLD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "FILE ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       EXECUTION-MODULE SECTION.
      ******************************************************************

           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO EXECUTION-MODULE-EXIT.

           MOVE 0 TO BRANCH.

           MOVE BEG-FMNO          TO IP-BEG-FMNO IP-FMNO.
           MOVE END-FMNO          TO IP-END-FMNO.
           MOVE FORM-PRINT-STATUS TO IP-PRFG.

           MOVE "LP/LONPEX" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH
                                  EXIT-PATHNAME.
           CANCEL FORM-PATH.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           DISPLAY BATCHP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BATCHP-FORM-FIELDS TO WR-BUF.
           MOVE BATCHP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE BATCHP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BATCHP-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BATCHP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE " "  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BATCHP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
