       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FMREPT.
       AUTHOR.        AMM.
       DATE-WRITTEN.  10-07-2005.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/FMREPT.C
      *
      * DESCRIPTION   :  FORM FILE LISTING REPORT
      *
      *=================================================================
      * REV:
      * AMM 2005.1007 PR#02617 ***NEW***
      * KEC 2013.0313 FIXED A PROBLEM WHERE PROGRAM WAS NOT RUNNING
      *               CORRECTLY FOR GROUP LOGIC DUE TO MISSING LINE:
      *               MOVE WGR-SRESULT TO BR-NO.
      * KEC 2016.0211 FIXED KEYS & RECS FOR FD-REC.
      *               CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * KEC 2018.0424 REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * KEC 2018-0823 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181030 GLOBAL GBFILE
      * BAH 181114 GLOBAL FMFILE
      * BAH 20190925 REMOVED ACCESS-CALL GBFILE, FMFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".


       COPY "LIBSP/SPFSREPORT.CPY".

      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".


      ******************************************************************
      *
      *    R E P O R T - F I L E
      *
      *-----------------------------------------------------------------
      *    FILE USED TO PRINT A REPORT
      ******************************************************************
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.

      *=================================================================
       78  REPORT---FORM-TYPE-CK-5                       VALUE 555.
       78  REPORT---FORM-TYPE-CKLN                       VALUE 999.
      *=================================================================
       01  REPORT-REC.
           03  REPORT-KEY.

               05  REPORT-BRNO                       PIC 9(04).
               05  REPORT-FMNO                       PIC 9(04).
               05  REPORT-PRINT-STATUS               PIC X(10).

               05  REPORT-FORM-TYPE                  PIC 9(03).
                   88  REPORT-FORM-TYPE-BW               VALUE 02.
                   88  REPORT-FORM-TYPE-NO               VALUE 04.
                   88  REPORT-FORM-TYPE-CK-5             VALUE 555.
                   88  REPORT-FORM-TYPE-CKLN             VALUE 999.
                   88  REPORT-FORM-TYPE-IV               VALUE 22.
                   88  REPORT-FORM-TYPE-LN               VALUE 01 04.
                   88  REPORT-FORM-TYPE-NT               VALUE 21.
                   88  REPORT-FORM-TYPE-VALID            VALUE 01 02 04
                                                               05 21 22
                                                               999.

               05  REPORT-FM-INITKEY                 PIC X(20).
               05  REPORT-FM-BW-INITKEY    REDEFINES REPORT-FM-INITKEY.
                   07  REPORT-FM-BW-SSNO             PIC 9(09).
                   07  FILLER                        PIC X(11).

               05  REPORT-FM-CK-INITKEY    REDEFINES REPORT-FM-INITKEY.
                   07  REPORT-FM-CK-BRNO             PIC 9(04).
                   07  REPORT-FM-CK-TYPE             PIC X(02).
                   07  REPORT-FM-CK-SEQNO            PIC 9(04).
                   07  REPORT-FM-CK-CKFMNO           PIC 9(04).

               05  REPORT-FM-LN-INITKEY    REDEFINES REPORT-FM-INITKEY.
                   07  REPORT-FM-LN-ACCTNO           PIC 9(08).
                   07  REPORT-FM-LN-FORMNO           PIC 9(04).
                   07  FILLER                        PIC X(08).

               05  REPORT-FM-NT-INITKEY    REDEFINES REPORT-FM-INITKEY.
                   07  REPORT-FM-NT-NUMBER           PIC 9(10).
                   07  REPORT-FM-LN-FORMNO           PIC 9(04).
                   07  FILLER                        PIC X(06).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/FMREPT.CBL S34 05/21/21-14:51:33 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".

       COPY "LIBSP/SPWSREPORT.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT,  ENTER BEGINNING BRANCH ".
           03  FILLER PIC X(19) VALUE "NUMBER OR GROUP ID ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G BRANCH NUMBER               ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING ACCOUNT NUMBER   ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G ACCOUNT NUMBER              ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-FMNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING FORM NUMBER      ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-FMNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G FORM NUMBER                 ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING SOCIAL SECURITY N".
           03  FILLER PIC X(19) VALUE "UMBER              ".

       01  LEGEND-END-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SOCIAL SECURITY NUMBER      ".
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
      *-----------------------------------------------------------------
           03  BEG-BRANCH                         PIC 9(04).
           03  END-BRANCH                         PIC 9(04).

           03  BEG-ACCTNO                         PIC 9(08).
           03  END-ACCTNO                         PIC 9(08).

           03  BEG-FMNO                           PIC 9(04).
           03  END-FMNO                           PIC 9(04).

           03  BEG-SSNO                           PIC 9(09).
           03  END-SSNO                           PIC 9(09).

           03  FORM-PRINT-STATUS                  PIC X(01).
               88  FORM-PRINT-STATUS-PRINTED          VALUE "P".
               88  FORM-PRINT-STATUS-UNPRINTED        VALUE "U".
               88  FORM-PRINT-STATUS-BOTH             VALUE "B".
               88  FORM-PRINT-STATUS-VALID            VALUE "P" "U" "B".

           03  GROUP-OPTIONS.
               05  GROUP-FLAG                     PIC X(01).
               05  ENT-GROUP                      PIC X(04).

      *=================================================================
       01  VERSION-CONTROL             PIC X(01)     VALUE "B".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-TIME           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-NAME           PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(10) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  FILLER             PIC X(05) VALUE SPACES.
           03  H02-TITLE          PIC X(82) VALUE SPACES.
           03  FILLER             PIC X(25) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-04.
           03  FILLER PIC X(30) VALUE "BRANCH   FORM   TYPE        ST".
           03  FILLER PIC X(30) VALUE "ATUS         NUMBER     INITKE".
           03  FILLER PIC X(30) VALUE "Y                             ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.

           03  FILLER                         PIC X(01).
           03  D01-BRNO                       PIC ZZZ9.
           03  FILLER                         PIC X(04).
           03  D01-FMNO                       PIC ZZZ9.
           03  FILLER                         PIC X(03).
           03  D01-TYPE                       PIC X(09).
           03  FILLER                         PIC X(03).
           03  D01-PRINT-STATUS               PIC X(09).
           03  FILLER                         PIC X(03).

           03  D01-SSNO                       PIC 999/99/9999.
           03  FILLER                         REDEFINES D01-SSNO.
               05  D01-NUMBER                 PIC ZZZZZZZ9.
               05  FILLER                     PIC X(03).

           03  FILLER                         PIC X(03).
           03  D01-INITKEY                    PIC X(20).
           03  FILLER                         PIC X(58).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 9999/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).
           03  R01-SSNO                       REDEFINES R01-FIELD
                                              PIC 999/99/9999.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/FMREPT_DEF.CPY".
       COPY "LIBLP/FMREPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/FMREPT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 REPORT-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE REPORT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE "X"           TO RECORDS-FOUND.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FMREPT"      TO FORM-PROG VDU-FORM-NAME.

           MOVE FMREPT-QUEUE-POS TO Q-POS.
           MOVE FMREPT-COPIES-POS TO C-POS.
           MOVE FMREPT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PROGID-TEXT-PROCESSING TO LEGEND
              PERFORM SEND-LEGEND

              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ MAKE PATHS }
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM INITIAL-PROGRESS-INDICATOR.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           IF ( UP-KEY )
              GO TO ENTER-FORM-PRINT-STATUS.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
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
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

      *KC->IF (( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES ))
      *       MOVE "S  A160DISPLAY-USER." TO VDU
      *       MOVE "USER BRANCH ONLY"     TO VDUBUF
      *       PERFORM SCREENIO
      *    ELSE
      *       MOVE "S  A160DISPLAY-USER." TO VDU
      *       MOVE SPACES                 TO VDUBUF
      *KC->   PERFORM SCREENIO.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.
      *KC->MOVE "S  A160DISPLAY-USER." TO VDU.
      *    MOVE SPACES                 TO VDUBUF.
      *KC->PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-BEG-FMNO.

           MOVE LEGEND-BEG-FMNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-FMNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FMNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FMNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-FMNO.

      *-----------------------------------------------------------------
       ENTER-END-FMNO.

           MOVE LEGEND-END-FMNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-FMNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-FMNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FMNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-FMNO.

           IF ( BEG-FMNO > END-FMNO )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FMNO.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-FMNO.

           MOVE "SNNN040BEG-FMNO." TO VDU.
           MOVE ZEROES             TO VDUNUM0 BEG-FMNO.
           PERFORM SCREENIO.

           MOVE "SNNN040END-FMNO." TO VDU.
           MOVE 9999               TO VDUNUM0 END-FMNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE LEGEND-BEG-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-FMNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE LEGEND-END-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-BEG-SSNO.   

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN060BEG-ACCTNO." TO VDU.
           MOVE ZEROES               TO VDUNUM0 BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO." TO VDU.
           MOVE 999999               TO VDUNUM0 END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
      *ENTER-BEG-APPNO.
      *
      *    MOVE LEGEND-BEG-APPNO TO LEGEND.
      *    PERFORM SEND-LEGEND.
      *
      *    MOVE "ENNN060BEG-APPNO." TO VDU.
      *    PERFORM SCREENIO.
      *
      *    IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
      *    IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF (        F3-KEY ) GO TO ENTER-ALL-APPNO.
      *    IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-APPNO.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE VDUNUM0 TO BEG-APPNO.
      *
      *-----------------------------------------------------------------
      *ENTER-END-APPNO.
      *
      *    MOVE LEGEND-END-APPNO TO LEGEND.
      *    PERFORM SEND-LEGEND.
      *
      *    MOVE "ENNN060END-APPNO." TO VDU.
      *    PERFORM SCREENIO.
      *
      *    IF (        UP-KEY ) GO TO ENTER-BEG-APPNO.
      *    IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF ( NOT ENTER-KEY ) GO TO ENTER-END-APPNO.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE VDUNUM0 TO END-APPNO.
      *
      *    IF ( BEG-APPNO > END-APPNO )
      *       MOVE "INVALID RANGE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-BEG-APPNO.
      *
      *    GO TO ENTER-BEG-SSNO.
      *
      *-----------------------------------------------------------------
      *ENTER-ALL-APPNO.
      *
      *    MOVE "SNNN060BEG-APPNO." TO VDU.
      *    MOVE ZEROES              TO VDUNUM0 BEG-APPNO.
      *    PERFORM SCREENIO.
      *
      *    MOVE "SNNN060END-APPNO." TO VDU.
      *    MOVE 99999999            TO VDUNUM0 END-APPNO.
      *    PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SSNO.

           MOVE LEGEND-BEG-SSNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNX090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.      
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE LEGEND-END-SSNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNX090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-FORM-PRINT-STATUS.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SNNX090BEG-SSNO." TO VDU.
           MOVE ZEROES             TO VDU-SSNO-EDIT BEG-SSNO.
           PERFORM SCREENIO.

           MOVE "SNNX090END-SSNO." TO VDU.
           MOVE 999999999          TO VDU-SSNO-EDIT END-SSNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-FORM-PRINT-STATUS.

           MOVE LEGEND-PRINT-STATUS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRINT-STATUS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-FORM-PRINT-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO FORM-PRINT-STATUS.

           IF NOT ( FORM-PRINT-STATUS-VALID )
              GO TO ENTER-FORM-PRINT-STATUS.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES ))
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BEG-FMNO."        TO VDU.
           MOVE BEG-FMNO                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-FMNO."        TO VDU.
           MOVE END-FMNO                  TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN090BEG-SSNO."        TO VDU.
           MOVE BEG-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "SNNN090END-SSNO."        TO VDU.
           MOVE END-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRINT-STATUS."    TO VDU.
           MOVE FORM-PRINT-STATUS         TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************

      ******************************************************************
       SCAN-GR1-FILE SECTION.
      ******************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( GRSCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
       EXECUTION-MODULE SECTION.
      ******************************************************************

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       EXECUTION-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO EXECUTION-MODULE-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXECUTION-PROCESS-BRANCH.

      *-----------------------------------------------------------------
       EXECUTION-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO EXECUTION-MODULE-END.

       EXECUTION-PROCESS-BRANCH.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-END.

           PERFORM PRINT-REPORT-MODULE.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.
      ******************************************************************

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO FM-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE.

           PERFORM OPEN-FM1-FILE.

           PERFORM OPEN-GB-FILE.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-MODULE-EXIT.

           MOVE FM-PATH-OWNBR     TO FM-BRNO.
           MOVE SPACES            TO FM-INITKEY.
           MOVE ZEROES            TO FM-FMNO.

           PERFORM START-FM1-FILE.

       PROCESS-BRANCH-NEXT-FM.

           PERFORM READ-FM1-FILE-NEXT.
           IF ( IO-BAD ) OR ( FM-BRNO NOT = FM-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF ( FM-FMNO < BEG-FMNO ) OR
              ( FM-FMNO > END-FMNO )
              GO TO PROCESS-BRANCH-NEXT-FM.

           MOVE FM-FMNO                    TO FD-FMNO.
           PERFORM GET-FD1-RECORD.
           IF ( IO-BAD )
              MOVE "XX"                    TO FD-PROG
              MOVE ZEROES                  TO FD-1STFILE.

           MOVE SPACES                     TO REPORT-REC.
           MOVE BR-NO                      TO REPORT-BRNO.
           MOVE FM-FMNO                    TO REPORT-FMNO.
           MOVE SPACES                     TO REPORT-PRINT-STATUS.

           IF ( FD-PROG = GB-CHECK-PROG )
              MOVE REPORT---FORM-TYPE-CKLN TO REPORT-FORM-TYPE
           ELSE
           IF ( FM-FMNO = 05 )
              MOVE REPORT---FORM-TYPE-CK-5 TO REPORT-FORM-TYPE
           ELSE
              MOVE FD-1STFILE              TO REPORT-FORM-TYPE.

           MOVE FM-INITKEY                 TO REPORT-FM-INITKEY.

           IF (( REPORT-FORM-TYPE-BW          )  OR
               ( REPORT-FORM-TYPE-IV          )) AND
              (( REPORT-FM-BW-SSNO < BEG-FMNO )  OR
               ( REPORT-FM-BW-SSNO > END-FMNO ))
              GO TO PROCESS-BRANCH-NEXT-FM.

           IF (( REPORT-FORM-TYPE-CKLN            )  OR
               ( REPORT-FORM-TYPE-LN              )) AND
              (( REPORT-FM-LN-ACCTNO < BEG-ACCTNO )  OR
               ( REPORT-FM-LN-ACCTNO > END-ACCTNO ))
              GO TO PROCESS-BRANCH-NEXT-FM.

           IF ( REPORT-FORM-TYPE-NT                ) AND
              (( REPORT-FM-NT-NUMBER < BEG-ACCTNO )  OR
               ( REPORT-FM-NT-NUMBER > END-ACCTNO ))
              GO TO PROCESS-BRANCH-NEXT-FM.

           IF (( FORM-PRINT-STATUS-PRINTED   )  AND
               ( FM-PRINT NOT = "Y"          )) OR
              (( FORM-PRINT-STATUS-UNPRINTED )  AND
               ( FM-PRINT     = "Y"          ))
              GO TO PROCESS-BRANCH-NEXT-FM.

      * VALID; PROCESS RECORD  

           MOVE "Y" TO RECORDS-FOUND.

           IF ( FM-PRINT = "Y" )
              MOVE "PRINTED   "            TO REPORT-PRINT-STATUS
           ELSE
              MOVE "UNPRINTED "            TO REPORT-PRINT-STATUS.

           PERFORM WRITE-REPORT-FILE.

           GO TO PROCESS-BRANCH-NEXT-FM.

       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-FM1-FILE.

       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-MODULE SECTION.
      ******************************************************************

           PERFORM CLOSE-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.

           MOVE ZEROES                     TO REPORT-BRNO.
           MOVE ZEROES                     TO REPORT-FMNO.
           MOVE SPACES                     TO REPORT-PRINT-STATUS.
           MOVE ZEROES                     TO REPORT-FORM-TYPE.
           MOVE SPACES                     TO REPORT-FM-INITKEY.
           PERFORM START-REPORT-FILE.

       PRINT-REPORT-FILE-NEXT.

           PERFORM READ-REPORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-END.

           MOVE REPORT-BRNO                TO D01-BRNO.
           MOVE REPORT-FMNO                TO D01-FMNO.

           MOVE "??"                              TO MESS.
           IF ( REPORT-FORM-TYPE-BW   ) MOVE "BW" TO MESS.
           IF ( REPORT-FORM-TYPE-CK-5 ) MOVE "CK" TO MESS.
           IF ( REPORT-FORM-TYPE-CKLN ) MOVE "CK" TO MESS.
           IF ( REPORT-FORM-TYPE-IV   ) MOVE "IV" TO MESS.
           IF ( REPORT-FORM-TYPE-LN   ) MOVE "LN" TO MESS.
           IF ( REPORT-FORM-TYPE-NT   ) MOVE "NT" TO MESS.

           STRING MESS, " (", REPORT-FORM-TYPE, ")"
                  DELIMITED BY "  "   INTO D01-TYPE.

           MOVE REPORT-PRINT-STATUS        TO D01-PRINT-STATUS.

           IF ( REPORT-FORM-TYPE-BW )
              MOVE REPORT-FM-BW-SSNO       TO D01-SSNO
              INSPECT D01-SSNO REPLACING ALL "/" BY "-"
           ELSE
           IF ( REPORT-FORM-TYPE-CK-5 )
              MOVE REPORT-FM-CK-SEQNO      TO D01-NUMBER
           ELSE
           IF ( REPORT-FORM-TYPE-CKLN )
              MOVE REPORT-FM-LN-ACCTNO     TO D01-NUMBER
           ELSE
           IF ( REPORT-FORM-TYPE-IV )
              MOVE REPORT-FM-BW-SSNO       TO D01-SSNO
              INSPECT D01-SSNO REPLACING ALL "/" BY "-"
           ELSE
           IF ( REPORT-FORM-TYPE-LN )
              MOVE REPORT-FM-LN-ACCTNO     TO D01-NUMBER
           ELSE
           IF ( REPORT-FORM-TYPE-NT )
              MOVE REPORT-FM-NT-NUMBER     TO D01-NUMBER
           ELSE
              MOVE ZEROES                  TO D01-NUMBER
              INSPECT D01-NUMBER REPLACING ALL ZEROES BY "?".

           MOVE REPORT-FM-INITKEY          TO D01-INITKEY.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-FILE-NEXT.

       PRINT-REPORT-MODULE-END.

           PERFORM PRINT-REPORT-RANGES.

       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-RANGES SECTION.
      ******************************************************************

           IF ( NO-RECORDS-FOUND )
              GO TO PRINT-REPORT-RANGES-EXIT.

           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING FORM NUMBER         : "    TO R01-PROMPT.
           MOVE BEG-FMNO                              TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FORM NUMBER         : "    TO R01-PROMPT.
           MOVE END-FMNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SOCIAL SECURITY #   : "    TO R01-PROMPT.
           MOVE BEG-SSNO                              TO R01-SSNO.
           INSPECT R01-SSNO REPLACING ALL "/" BY "-".
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SOCIAL SECURITY #   : "    TO R01-PROMPT.
           MOVE END-SSNO                              TO R01-SSNO.
           INSPECT R01-SSNO REPLACING ALL "/" BY "-".
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT STATUS (P/U/B)          : "    TO R01-PROMPT.
           MOVE FORM-PRINT-STATUS                     TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-REPORT-RANGES-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER-LINES.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-PAGE-HEADER-LINES SECTION.
      ******************************************************************

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE TIME-EDIT                        TO H01-TIME.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "FORM REQUEST REPORT"            TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-04                   TO PR-REC.
           ADD  02                               TO LN-COUNT.
           MOVE 02                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "="                          TO PR-REC.
           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       WRITE-PAGE-HEADER-LN-END.

           MOVE 01 TO LN-FEED.

       WRITE-PAGE-HEADER-LN-EXIT.
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
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO FMREPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FMREPT-SYSDATE.
           DISPLAY FMREPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FMREPT-FORM-FIELDS TO WR-BUF.
           MOVE FMREPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE FMREPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE FMREPT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/FMREPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       GET-FD1-RECORD.

           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.

      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=======================================================[ PROGID ]
       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

           MOVE BR-NO     TO PROGID-BRANCH.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1RN.CPY".
       COPY "LIBSP/SPIOREPORT.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           PERFORM REMOVE-REPORT-FILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              MOVE SV-BEG-BRANCH TO BEG-BRANCH
              MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y"  )
              MOVE SV-BEG-BRANCH TO BEG-BRANCH
              MOVE SV-END-BRANCH TO END-BRANCH
              MOVE SV-GROUP-FLAG TO GROUP-FLAG
              MOVE SV-ENT-GROUP  TO ENT-GROUP.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY FMREPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
