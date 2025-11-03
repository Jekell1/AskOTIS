       IDENTIFICATION DIVISION.
       PROGRAM-ID.    REPLST.
       AUTHOR.        KEC.
       DATE-WRITTEN.  05-31-2000.
      ******************************************************************
      *
      * IDENTIFICATION:  LS/REPLST.C
      *
      * DESCRIPTION   :  REP. CODE FILE LIST (CD)
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0531 {PR#00000} PARADATA FINANCIAL [JIM KRUSE]; NEW
      * KEC 2016.0311 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-RC-CODE TO NO LONGER USE HASH.
      *         REMOVED CD-RC-CODE-ALPHA; USING CD-RC-CODE NOW.
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      ******************************************************************
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
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/REPLST.CBL S34 06/10/21-13:14:06 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ENTRY-TYPE                  PIC X(03)     VALUE SPACES.

       01  EXITING-PROGRAM             PIC X(1)      VALUE "P".
           88  EXITING-AFTER-RUN                     VALUE "A".
           88  EXITING-PRIOR-RUN                     VALUE "P".

       01  FIRST-TIME                  PIC X(01)     VALUE "Y".
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(02)     VALUE ZEROES.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  RECORDS-FOUND               PIC X(01)     VALUE SPACES.
           88  NO-RECORDS-FOUND                      VALUE "N".

      ******************************************************************
      *
      *    S C R E E N - F I E L D S
      *
      ******************************************************************
       01  SCREEN-BUF                  VALUE SPACES.
           03  BEG-REP                 PIC X(08).
           03  END-REP                 PIC X(08).
           03  ERRCD                   PIC X(01).


      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H01-HR                  PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H01-MIN                 PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(06)     VALUE SPACES.
           03  H01-NAME                PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(10)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H02-USERID              PIC X(10).
           03  FILLER                  PIC X(05)     VALUE SPACES.
           03  FILLER                  PIC X(29)     VALUE
                                        "--- REP CODE FILE LISTING ---".
           03  FILLER                  PIC X(26)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER                  PIC X(80)     VALUE ALL "=".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(80)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(06).
           03  D01-CODEX               PIC X(06).
           03  D01-CODE                PIC X(08).
           03  FILLER                  PIC X(06).
           03  D01-DESCX               PIC X(13).
           03  D01-DESC                PIC X(35).
           03  FILLER                  PIC X(06).

      *-----------------------------------------------------------------
       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-PROMPT              PIC X(32).
           03  R01-TEXT                PIC X(20).
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       01  REPLST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/REPLST_DEF.CPY".
       COPY "LIBLS/REPLST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/REPLST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-PR-FILE.
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

           PERFORM PRIMARY-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "REPLST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS REPLST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM ENTER-PARAMETERS.

           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE EXT-CONAME-CONAME  TO H01-NAME.
           MOVE EXT-CONAME-SYSDATE TO H01-DATE.
           MOVE EXT-CONAME-USER-ID  TO H02-USERID.

           MOVE "LISTING IN PROGRESS....................." TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "A" TO EXITING-PROGRAM.

           PERFORM OPEN-PR-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           MOVE SPACES TO ERRCD.

       ENTER-PRINTER-OPTIONS.
           MOVE REPLST-QUEUE-POS TO Q-POS.
           MOVE REPLST-COPIES-POS TO C-POS.
           MOVE REPLST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.

           IF ( STAT-BAD )
              MOVE "X" TO ERRCD
              GO TO ENTER-PARAMETERS-EXIT.

           MOVE SPACES TO ERRCD.

       
       ENTER-BEG-REP.

           MOVE "BEG" TO ENTRY-TYPE.

           MOVE "E  A080BEG-REP." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PRINTER-OPTIONS.
           IF (        F3-KEY ) GO TO ENTER-ALL-REP.
           IF (        F6-KEY ) PERFORM SCAN-CD-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-REP.

       
           MOVE VDUBUF TO BEG-REP.

       
       ENTER-END-REP.

           MOVE "END" TO ENTRY-TYPE.

           MOVE "E  A080END-REP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-REP.
           IF (        F1-KEY ) GO TO ENTER-PRINTER-OPTIONS.
           IF (        F6-KEY ) PERFORM SCAN-CD-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-REP.

       
           MOVE VDUBUF TO END-REP.

           IF ( BEG-REP > END-REP )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-REP.

           GO TO ENTER-ACCEPT-YN.

       
       ENTER-ALL-REP.

           MOVE "S  A080BEG-REP." TO VDU.
           MOVE SPACES            TO VDUBUF BEG-REP.
           PERFORM SCREENIO.

           MOVE "S  A080END-REP." TO VDU.
           MOVE "ZZZZZZZZ"        TO VDUBUF END-REP.
           PERFORM SCREENIO.

       
       ENTER-ACCEPT-YN.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-REP.
           IF (        F1-KEY ) GO TO ENTER-PRINTER-OPTIONS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

           IF ( VDUBUF = "N" ) GO TO ENTER-PRINTER-OPTIONS.
           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-END.

           GO TO ENTER-ACCEPT-YN.

       
       ENTER-PARAMETERS-END.

           IF ( F1-KEY ) GO TO ENTER-PRINTER-OPTIONS.
           IF ( F7-KEY ) MOVE "X" TO ERRCD.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       SCAN-CD-FILE SECTION.
      ******************************************************************

           MOVE "RC"       TO CDSCAN-TYPE.
           MOVE SPACES     TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-CD-FILE-EXIT.

           IF ( ENTRY-TYPE = "BEG" )
              MOVE "S  A080BEG-REP." TO VDU
           ELSE
              MOVE "S  A080END-REP." TO VDU.

           MOVE CDSCAN-CODE       TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE       TO VDUBUF
           MOVE "00"              TO VDUSTATUS.

       SCAN-CD-FILE-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-MODULE SECTION.
      ******************************************************************

           MOVE 99 TO LN-COUNT.

           MOVE "RC"       TO CD-TYPE.
           MOVE BEG-REP    TO CD-CODE.

           PERFORM OPEN-CD1-FILE.
           PERFORM START-CD1-FILE.

       READ-NEXT-CD-RECORD.

           PERFORM READ-CD1-FILE-NEXT.
           IF ( IO-BAD ) OR ( CD-TYPE NOT = "RC" ) OR
                            ( CD-CODE > END-REP )
              GO TO PRIMARY-MODULE-END.

           IF ( CD-CODE < BEG-REP )
              GO TO READ-NEXT-CD-RECORD.

           PERFORM PRINT-CD-RECORD.

           GO TO READ-NEXT-CD-RECORD.

       PRIMARY-MODULE-END.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO RECORDS-FOUND
              GO TO PRIMARY-MODULE-EXIT.

           PERFORM PRINT-RANGE-LINES.

       PRIMARY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-CD-RECORD SECTION.
      ******************************************************************

           MOVE "CODE: "         TO D01-CODEX.
           MOVE CD-CODE          TO D01-CODE.
           MOVE "DESCRIPTION: "  TO D01-DESCX.
           MOVE CD-DESC          TO D01-DESC.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-CD-RECORD-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       PAGE-HEADER SECTION.
      ******************************************************************

           MOVE "N"         TO FIRST-TIME.
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H01-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H01-HR.
           MOVE T-MM        TO H01-MIN.

       
           MOVE HEADER-LINE-01 TO PR-REC.
           MOVE 99             TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEADER-LINE-02 TO PR-REC.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEADER-LINE-03 TO PR-REC.
           MOVE 2              TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE 1 TO LN-FEED.
           MOVE 4 TO LN-COUNT.

       PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE 3 TO LN-FEED.

           MOVE "BEGINNING REP CODE            : " TO R01-PROMPT.
           MOVE BEG-REP                            TO R01-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    REP CODE            : " TO R01-PROMPT.
           MOVE END-REP                            TO R01-TEXT.
           PERFORM WRITE-DETAIL-LINE.

       
       PRINT-RANGE-LINES-EXIT.
           EXIT.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO REPLST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO REPLST-SYSDATE.
           DISPLAY REPLST-SCREEN UPON REPLST-WINDOW-HANDLE.
           MOVE REPLST-FORM-FIELDS TO WR-BUF.
           MOVE REPLST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE REPLST-HELP--FILE TO HELP-LINK-FILE.
           MOVE REPLST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/REPLST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       
       END-RUN.

           IF ( EXITING-PRIOR-RUN )
              GO TO EXIT-PROG.

           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

       
       EXIT-PROG.

           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY REPLST-WINDOW-HANDLE.
           EXIT PROGRAM.
