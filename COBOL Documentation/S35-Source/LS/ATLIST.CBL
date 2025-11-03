       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ATLIST.
       AUTHOR.        CS.
       DATE-WRITTEN.  10-05-2001.
      ******************************************************************
      *
      * IDENTIFICATION:  LS/ATLIST.C
      *
      * DESCRIPTION   :  ACTIVITY CODE FILE LISTING (CD)
      *
      * REV:
      * 011005   ADDED "AT" (ACTIVITY CODE LISTING)
      *
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-AC-RE-REC TO BE SPLIT INTO CD-AT-REC &
      *         CD-RE-REC.  FIELDS CD-??-ACTIONCD, CD-??-MEMO-REQUIRED,
      *         CD-??-SUPERVISOR-ONLY ARE IDENTICAL (LOCATION, NAME,
      *         SIZE) IN SITUATIONS WERE NOT SURE WHICH TYPE CD-REC
      *         WAS READ.
      *
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LS/ATLIST.CBL S35 09/20/22-13:05:25 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

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
       01  ERRCD           PIC X VALUE SPACES.


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
           03  FILLER                  PIC X(31)     VALUE SPACES.
           03  H01-NAME                PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(37)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZ9.
           03  FILLER                  PIC X(27)     VALUE SPACES.
      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H02-USERID              PIC X(10).
           03  FILLER                  PIC X(23)     VALUE SPACES.
           03  FILLER                  PIC X(45)     VALUE
                       "       --- ACTIVITY CODE LISTING ---         ".
           03  FILLER                  PIC X(44)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER                  PIC X(132)     VALUE ALL "=".

      *------------------------------------------------------------------
       01  HEADER-LINE-04.
           03  FILLER                  PIC X(18)    VALUE SPACES.
           03  FILLER                  PIC X(04)    VALUE "CODE".
           03  FILLER                  PIC X(08)    VALUE SPACES.
           03  FILLER                  PIC X(11)    VALUE "DESCRIPTION".
           03  FILLER                  PIC X(24)    VALUE SPACES.
           03  FILLER                  PIC X(04)    VALUE "MEMO".
           03  FILLER                  PIC X(03)    VALUE SPACES.
           03  FILLER                  PIC X(04)    VALUE "SUPV".
           03  FILLER                  PIC X(03)    VALUE SPACES.
           03  FILLER                  PIC X(06)    VALUE "RESULT".
           03  FILLER                  PIC X(03)    VALUE SPACES.
           03  FILLER                  PIC X(04)    VALUE "ACTN".
           03  FILLER                  PIC X(03)    VALUE SPACES.
           03  FILLER                  PIC X(04)    VALUE SPACES.
           03  FILLER                  PIC X(03)    VALUE SPACES.
           03  FILLER                  PIC X(04)    VALUE SPACES.
           03  FILLER                  PIC X(26)    VALUE SPACES.

       01  HEADER-LINE-05.
           03  FILLER                  PIC X(65)    VALUE SPACES.
           03  H05-REST-OF-LINE.
               05  FILLER                  PIC X(04)    VALUE "REQD".
               05  FILLER                  PIC X(03)    VALUE SPACES.
               05  FILLER                  PIC X(04)    VALUE "ONLY".
               05  FILLER                  PIC X(03)    VALUE SPACES.
               05  FILLER                  PIC X(06)    VALUE "REQD  ".
               05  FILLER                  PIC X(03)    VALUE SPACES.
               05  FILLER                  PIC X(04)    VALUE "CODE".
               05  FILLER                  PIC X(03)    VALUE SPACES.
               05  FILLER                  PIC X(04)    VALUE SPACES.
               05  FILLER                  PIC X(03)    VALUE SPACES.
               05  FILLER                  PIC X(04)    VALUE SPACES.
               05  FILLER                  PIC X(26)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD-PR-LINE.
           03  FILLER                  PIC X(18)    VALUE SPACES.
           03  FILLER                  PIC X(05)    VALUE "CODE ".
           03  FILLER                  PIC X(03)    VALUE SPACES.
           03  FILLER                  PIC X(35)    VALUE "DESCRIPTION".
           03  FILLER                  PIC X(19)    VALUE SPACES.

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(132)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE                      REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(18).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-KEY-AREA.
               10  D-CODE              PIC X(03).
               10  FILLER              PIC X(09).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  DISPLAY-AT.
               05 D-AT-DESC            PIC X(27).
               05 FILLER               PIC X(11).
               05 D-ITEM-A             PIC X.
               05 FILLER               PIC X(6).
               05 D-ITEM-B             PIC X.
               05 FILLER               PIC X(8).
               05 D-ITEM-C             PIC X.
               05 FILLER               PIC X(3).
               05 D-ITEM-D             PIC X(4).
               05 FILLER               PIC X(7).
               05 FILLER               PIC X(9).

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
       COPY "LIBGB/DATERW.CPY".
       01  ATLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/ATLIST_DEF.CPY".
       COPY "LIBLS/ATLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/ATLIST_SCN.CPY".

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
           MOVE "ATLIST" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ATLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE EXT-CONAME-CONAME  TO H01-NAME.
           MOVE EXT-CONAME-SYSDATE TO H01-DATE.
           MOVE EXT-CONAME-USER-ID  TO H02-USERID.

           MOVE "LISTING IN PROGRESS......................" TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "A" TO EXITING-PROGRAM.

           PERFORM OPEN-PR-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES TO ERRCD.

           MOVE ATLIST-QUEUE-POS TO Q-POS.
           MOVE ATLIST-COPIES-POS TO C-POS.
           MOVE ATLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.

           IF ( STAT-BAD )
              MOVE "X" TO ERRCD
              GO TO ENTER-PARAMETERS-EXIT.

           MOVE SPACES TO ERRCD.

       ENTER-ACCEPT-YN.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           MOVE 99 TO LN-COUNT.

           MOVE "AT"    TO CD-TYPE.

           PERFORM OPEN-CD1-FILE.
           PERFORM START-CD1-FILE.

       READ-NEXT-CD-RECORD.

           PERFORM READ-CD1-FILE-NEXT.
           IF ( IO-BAD ) OR ( CD-TYPE NOT = "AT" )
              GO TO PRIMARY-MODULE-END.

           PERFORM PRINT-CD-RECORD.

           GO TO READ-NEXT-CD-RECORD.

       PRIMARY-MODULE-END.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO RECORDS-FOUND
              GO TO PRIMARY-MODULE-EXIT.

       PRIMARY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-CD-RECORD SECTION.
      ******************************************************************
           MOVE CD-CODE TO D-CODE.
           MOVE CD-DESC TO D-AT-DESC.

           MOVE CD-AT-MEMO-REQUIRED       TO D-ITEM-A.
           MOVE CD-AT-SUPERVISOR-ONLY     TO D-ITEM-B.
           MOVE CD-AT-RESULT-REQUIRED     TO D-ITEM-C.
           MOVE CD-AT-ACTIONCD            TO D-ITEM-D.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-CD-RECORD-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK > EXT-CONAME-RPT-LINES )
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

           MOVE HEADER-LINE-04 TO PR-REC.
           MOVE 2   TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE  HEADER-LINE-05 TO PR-REC.
           MOVE 1   TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 8 TO LN-COUNT.

       PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO ATLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ATLIST-SYSDATE.
           DISPLAY ATLIST-SCREEN UPON ATLIST-WINDOW-HANDLE.
           MOVE ATLIST-FORM-FIELDS TO WR-BUF.
           MOVE ATLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE ATLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE ATLIST-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/ATLIST_EVA.CPY".
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
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ATLIST-SCREEN.
           DESTROY ATLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
