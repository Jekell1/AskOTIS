       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GILST2.
       DATE-WRITTEN.  08-25-198?.
      ******************************************************************
      *
      * IDENTIFICATION:  LS/GILST2.C
      *
      * DESCRIPTION   :  MISSING GI ACCOUNT NUMBERS
      *                  READS GIFILE, CHECKS FOR ALL ACCOUNT #'S
      *                  IN THE GLFILE AND PRINT ANY THAT ARE NOT FOUND
      *
      *-----------------------------------------------------------------
      * REV:
      * BAH 020189 REARRANGED
      * SLC 092589 ADDED GI-ACR-INT-AR-AR
      * BAH 111991 WAS NOT PRINTING 27 ON PAGE 2
      * BAH 062193 CHANGED EXTENSION TO ADDON
      * BAH 120193 CHANGED TO MATCH SCREENS
      * JFF 102095 ADDED CLASS SCAN
      * JTG 111095 ADDED PRINT OF GI-ACR-INT-AR
      * MJD 042696 ADDED GP-FILE GP-DLR-DISP-TYPE.
      * MJD 111396 ADDED GI-CONV-OFFSET TO PAGE 1.
      * GLF 052097 CHANGED PG3 PAYABLES TO UNEARNED INCOME
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * JTG 043098 ADDED GI-PARTIAL-PLEXP (PARTIAL CHARGEOFF)
      * BAH 062998 RENAMED GI-EREXTCHG TO GI-ERADDON AND CHANGED
      *                    GI-UNEXTCHG TO GI-UNADDON
      * KEC 000530 {PR#00000} PARADATA FINANCIAL [JIM ROSENAUER];
      *             ADDED PAGE 6.
      * BLV 112500 FIXED CHECK ON CLASS RANGE, SAID INVALID CLASS
                   "IF BEG-CLASS > END-BRANCH", FIXED TO END-CLASS
      *  GLB 020507 ADDED GI-ACH-BANK; REGACC PR#1797
      *  MG  020930 CHANGED THE LISTING TO MATCH THE MAINTENANCE
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
           "@(#)LS/GILST2.CBL S34 06/10/21-12:23:23 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME                  PIC X(01)     VALUE "Y".
       01  SUB                         PIC 9(03)     VALUE 0.
       01  MESS-LIST                   PIC X(05)     VALUE "MESS.".
       01  WORK-ACCOUNT                PIC 9(11).
       01  FILLER REDEFINES WORK-ACCOUNT.
           03  WORK-BR                 PIC 9(4).
           03  WORK-ACCT               PIC 9(7).
       01  BAD-COUNT                   PIC 9(11).

       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(02)     VALUE 3.
           03  LN-FEED                 PIC 9(02)     VALUE 0.
           03  LN-WK                   PIC 9(02)     VALUE 0.
           03  PAGE-NUMBER             PIC 9(04)     VALUE 0.

       01  RECORDS-FOUND               PIC X(01)     VALUE SPACES.
           88  NO-RECORDS-FOUND                      VALUE "N".

      ******************************************************************
      *
      *    S C R E E N - F I E L D S
      *
      ******************************************************************
       01  SCREEN-BUF                  VALUE SPACES.
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  BEG-CLASS               PIC 9(02).
           03  END-CLASS               PIC 9(02).
           03  ERRCD                   PIC X(01).

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE                  PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H-HR                    PIC 9(02)     VALUE 0.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H-MIN                   PIC 9(02)     VALUE 0.
           03  FILLER                  PIC X(06)     VALUE SPACES.
           03  H-NAME                  PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(10)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  FILLER                  PIC X(39)     VALUE
               "MISSING GL # IN THE GI RECORD LISTING".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(80)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE                      REDEFINES DETAIL-LINE.
           03  D-GI-BRANCH             PIC 9(4).
           03  FILLER                  PIC X(02).
           03  D-GI-CLASS              PIC 99.
           03  FILLER                  PIC X(02).
           03  D-SUB                   PIC Z99.
           03  FILLER                  PIC X(02).
           03  D-GI-GL-BR              PIC 9(4).
           03  D-DASH                  PIC X.
           03  D-GI-GL-ACCT            PIC 9(7).
           03  FILLER                  PIC X(02).

      *-----------------------------------------------------------------
       01  RANGE-LINE                  REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(30).
           03  RANGE-SELECTION.
               05  R-BRANCH            PIC X(04).
               05  FILLER              PIC X(02).
               05  R-CLASS             PIC X(04).
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
       01  GILST2-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GILST2_DEF.CPY".
       COPY "LIBLS/GILST2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GILST2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GILST2" TO VDU-FORM-NAME.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.

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

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GILST2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM ENTER-PARAMETERS.

           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES TO ERRCD.

           MOVE GILST2-QUEUE-POS TO Q-POS.
           MOVE GILST2-COPIES-POS TO C-POS.
           MOVE GILST2-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.

           IF ( STAT-BAD )
              MOVE "X" TO ERRCD
              GO TO ENTER-PARAMETERS-EXIT.

           MOVE SPACES TO ERRCD.

       
       ENTER-BEG-BRANCH.

           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

       
           MOVE VDUNUM0 TO BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE "ENNN040BR2." TO VDU.
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

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BR1." TO VDU.
           MOVE ZEROES        TO VDUNUM0 BEG-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO VDUNUM0 END-BRANCH.
           PERFORM SCREENIO.

       
       ENTER-BEG-CLASS.

           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

       
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN020LC1." TO VDU.
           MOVE ZEROES        TO VDUNUM0 BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN020LC2." TO VDU.
           MOVE 99            TO VDUNUM0 END-CLASS.
           PERFORM SCREENIO.

       
       ENTER-ACCEPT-YN.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

       
           IF ( VDUBUF = "N" ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-END.

           GO TO ENTER-ACCEPT-YN.

       
       ENTER-PARAMETERS-END.

           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F7-KEY ) MOVE "X" TO ERRCD.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-GI1-FILE.
           PERFORM OPEN-GL1-FILE.

           MOVE BEG-BRANCH TO GI-BRANCH
                              QGI1-WBEG-BRANCH
                              QGI1-WEND-BRANCH.
           MOVE BEG-CLASS  TO GI-CLASS
                              QGI1-WBEG-CLASS.
           MOVE END-CLASS  TO QGI1-WEND-CLASS.

           PERFORM START-GI1-FILE.

       READ-NEXT-GI-RECORD.
           PERFORM READ-GI1-FILE-NEXT.
           IF ( IO-BAD                 ) OR
              ( GI-BRANCH > END-BRANCH )
              GO TO PRIMARY-MODULE-END.

           IF ( GI-CLASS < BEG-CLASS ) OR
              ( GI-CLASS > END-CLASS )
              GO TO READ-NEXT-GI-RECORD.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > GI-ACCT-IX-MAX
              MOVE GI-BR-ACCOUNT(SUB) TO GL-BR-ACCOUNT
              PERFORM READ-GL1-FILE
              IF IO-BAD AND GI-BR-ACCOUNT(SUB) NOT = 0
                 ADD 1 TO BAD-COUNT
                 PERFORM PRINT-GI-RECORD
              END-IF
           END-PERFORM.

           GO TO READ-NEXT-GI-RECORD.

       
       PRIMARY-MODULE-END.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO RECORDS-FOUND
              GO TO PRIMARY-MODULE-EXIT.

           PERFORM PRINT-RANGE-LINES.

       PRIMARY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-GI-RECORD SECTION.
      ******************************************************************
       
           MOVE GI-BRANCH TO D-GI-BRANCH.
           MOVE GI-CLASS TO D-GI-CLASS.
           MOVE SUB TO D-SUB.
           MOVE GI-BR-ACCOUNT(SUB) TO WORK-ACCOUNT.
           MOVE WORK-BR TO D-GI-GL-BR.
           MOVE "-" TO D-DASH.
           MOVE WORK-ACCT TO D-GI-GL-ACCT.
           MOVE 2        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GI-RECORD-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN 60
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           MOVE "N"         TO FIRST-TIME.
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99    TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 1 TO LN-COUNT.

       HEAD-PAGE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE 1 TO LN-FEED.

           MOVE "BAD COUNT " TO RANGE-LINE.
           MOVE BAD-COUNT                      TO R-BRANCH.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGINNING BRANCH CODE, CLASS" TO RANGE-LINE.
           MOVE BEG-BRANCH                     TO R-BRANCH.
           MOVE BEG-CLASS                      TO R-CLASS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH CODE, CLASS" TO RANGE-LINE.
           MOVE END-BRANCH                  TO R-BRANCH.
           MOVE END-CLASS                   TO R-CLASS.
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
           MOVE EXT-CONAME-CONAME TO GILST2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GILST2-SYSDATE.
           DISPLAY GILST2-SCREEN UPON GILST2-WINDOW-HANDLE.
           MOVE GILST2-FORM-FIELDS TO WR-BUF.
           MOVE GILST2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GILST2-HELP--FILE TO HELP-LINK-FILE.
           MOVE GILST2-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GILST2_EVA.CPY".
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
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GILST2-SCREEN.
           DESTROY GILST2-WINDOW-HANDLE.
           EXIT PROGRAM.
