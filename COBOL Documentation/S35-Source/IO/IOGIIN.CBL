       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGIIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGIIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GIFILE
      * TABLE DESC    : GENERAL LEDGER CONTROL FILE
      * OUTPUT FILE   : XPC/IOGIIN.PRE
      * USED FOR      : LIBGL/GLWSGI.CPY  --- QGI-REC
      *                 LIBGL/GLGI1R.CPY  --- READ
      *                 LIBGL/GLGI1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0511 ***NEW***
      *
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOGIIN.CBL S34 05/18/21-08:07:14 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GLWSGI.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QGI1-WHERE-BEG-KEY.
           03  QGI1-WBEG-BRANCH           PIC 9(04).
           03  QGI1-WBEG-CLASS            PIC 9(03).
 
       01  QGI1-WHERE-END-KEY.
           03  QGI1-WEND-BRANCH           PIC 9(04).
           03  QGI1-WEND-CLASS            PIC 9(03).
 
       01  QGI-REC.
           03  QGI-BRANCH          PIC 9(4).
           03  QGI-CLASS           PIC 999.
           03  QGI-LTOUCH-DATE     PIC X(10).
           03  QGI-CASH            PIC 9(11).
           03  QGI-BANK            PIC 9(11).
           03  QGI-NSF             PIC 9(11).
           03  QGI-AR              PIC 9(11).
           03  QGI-PL-1            PIC 9(11).
           03  QGI-PL-2            PIC 9(11).
           03  QGI-PL-3            PIC 9(11).
           03  QGI-PL-4            PIC 9(11).
           03  QGI-PL-5            PIC 9(11).
           03  QGI-DLINSREB        PIC 9(11).
           03  QGI-ACR-INT-AR-AR   PIC 9(11).
           03  QGI-OTHBR-REC       PIC 9(11).
           03  QGI-OTHBR-PAY       PIC 9(11).
           03  QGI-UNINTCHG        PIC 9(11).
           03  QGI-UNADDON         PIC 9(11).
           03  QGI-UNSERCHG        PIC 9(11).
           03  QGI-UNMAINTFEE      PIC 9(11).
           03  QGI-UNDLDISC        PIC 9(11).
           03  QGI-UNOTHER         PIC 9(11).
           03  QGI-UN7             PIC 9(11).
           03  QGI-UN8             PIC 9(11).
           03  QGI-UN9             PIC 9(11).
           03  QGI-UN10            PIC 9(11).
           03  QGI-UNCL1           PIC 9(11).
           03  QGI-UNCL2           PIC 9(11).
           03  QGI-UNAH            PIC 9(11).
           03  QGI-UNPP            PIC 9(11).
           03  QGI-UNO1            PIC 9(11).
           03  QGI-UNO2            PIC 9(11).
           03  QGI-UNO3            PIC 9(11).
           03  QGI-UNO4            PIC 9(11).
           03  QGI-UNO5            PIC 9(11).
           03  QGI-UNO6            PIC 9(11).
           03  QGI-UNO7            PIC 9(11).
           03  QGI-UNINTREPO       PIC 9(11).
           03  QGI-UNADDONINTREPO  PIC 9(11).
           03  QGI-PLEXP-1         PIC 9(11).
           03  QGI-PLEXP-2         PIC 9(11).
           03  QGI-PLEXP-3         PIC 9(11).
           03  QGI-PLEXP-4         PIC 9(11).
           03  QGI-PLEXP-5         PIC 9(11).
           03  QGI-DLPARVEX        PIC 9(11).
           03  QGI-PLOFFSET-1      PIC 9(11).
           03  QGI-PLOFFSET-2      PIC 9(11).
           03  QGI-PLOFFSET-3      PIC 9(11).
           03  QGI-PLOFFSET-4      PIC 9(11).
           03  QGI-PLOFFSET-5      PIC 9(11).
           03  QGI-SERVOFF         PIC 9(11).
           03  QGI-PAYCL1          PIC 9(11).
           03  QGI-PAYCL2          PIC 9(11).
           03  QGI-PAYAH           PIC 9(11).
           03  QGI-PAYPP           PIC 9(11).
           03  QGI-PAYO1           PIC 9(11).
           03  QGI-PAYO2           PIC 9(11).
           03  QGI-PAYO3           PIC 9(11).
           03  QGI-PAYO4           PIC 9(11).
           03  QGI-PAYO5           PIC 9(11).
           03  QGI-PAYO6           PIC 9(11).
           03  QGI-PAYO7           PIC 9(11).
           03  QGI-DLPAYABLE       PIC 9(11).
           03  QGI-DLRVPRIN-1      PIC 9(11).
           03  QGI-DLRVPRIN-2      PIC 9(11).
           03  QGI-DLRVPRIN-3      PIC 9(11).
           03  QGI-DLRVCHGS-1      PIC 9(11).
           03  QGI-DLRVCHGS-2      PIC 9(11).
           03  QGI-DLRVCHGS-3      PIC 9(11).
           03  QGI-DLPARV          PIC 9(11).
           03  QGI-PAYALLOT        PIC 9(11).
           03  QGI-DIRECT-PAY      PIC 9(11).
           03  QGI-ERINTCHG        PIC 9(11).
           03  QGI-ERADDON         PIC 9(11).
           03  QGI-ERSERCHG        PIC 9(11).
           03  QGI-ERMAINTFEE      PIC 9(11).
           03  QGI-ERDLDISC        PIC 9(11).
           03  QGI-EROTHER         PIC 9(11).
           03  QGI-EARN7           PIC 9(11).
           03  QGI-EARN8           PIC 9(11).
           03  QGI-EARN9           PIC 9(11).
           03  QGI-EARN10          PIC 9(11).
           03  QGI-LATECHG         PIC 9(11).
           03  QGI-INTEREST        PIC 9(11).
           03  QGI-DEFERMENTS      PIC 9(11).
           03  QGI-SAPRINCIPAL     PIC 9(11).
           03  QGI-PLINT-1         PIC 9(11).
           03  QGI-PLINT-2         PIC 9(11).
           03  QGI-PLINT-3         PIC 9(11).
           03  QGI-PLINT-4         PIC 9(11).
           03  QGI-PLINT-5         PIC 9(11).
           03  QGI-PLREC-1         PIC 9(11).
           03  QGI-PLREC-2         PIC 9(11).
           03  QGI-PLREC-3         PIC 9(11).
           03  QGI-PLREC-4         PIC 9(11).
           03  QGI-PLREC-5         PIC 9(11).
           03  QGI-CL1             PIC 9(11).
           03  QGI-CL2             PIC 9(11).
           03  QGI-AH              PIC 9(11).
           03  QGI-PP              PIC 9(11).
           03  QGI-O1              PIC 9(11).
           03  QGI-O2              PIC 9(11).
           03  QGI-O3              PIC 9(11).
           03  QGI-O4              PIC 9(11).
           03  QGI-O5              PIC 9(11).
           03  QGI-O6              PIC 9(11).
           03  QGI-O7              PIC 9(11).
           03  QGI-PLNEW-AR-1      PIC 9(11).
           03  QGI-PLNEW-AR-2      PIC 9(11).
           03  QGI-PLNEW-AR-3      PIC 9(11).
           03  QGI-PLNEW-AR-4      PIC 9(11).
           03  QGI-PLNEW-AR-5      PIC 9(11).
           03  QGI-REPO-AR         PIC 9(11).
           03  QGI-REPO-INT        PIC 9(11).
           03  QGI-ACR-INT-AR      PIC 9(11).
           03  QGI-CONV-OFFSET     PIC 9(11).
           03  QGI-PARTIAL-PLEXP   PIC 9(11).
           03  QGI-PAYLBOX             PIC 9(11).
           03  QGI-PENALTY-INT         PIC 9(11).
           03  QGI-PAYTPAY             PIC 9(11).
           03  QGI-PAYTCOLL            PIC 9(11).
           03  QGI-PAYMGRAM            PIC 9(11).
           03  QGI-REPO-ADDON-EXP      PIC 9(11).
           03  QGI-ACH-BANK            PIC 9(11).
           03  QGI-LN-APPROVAL-LIMIT   PIC 9(7).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-BRANCH. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH     (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH     ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH               PIC 9(04).
           03  END-BRANCH               PIC 9(04).
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "BRNO   CLASS        CASH      ".
           03  FILLER PIC X(30) VALUE "ACH BANK   LIMIT".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GI-BRANCH                  PIC 9(4).
           03  FILLER                         PIC X(05).
           03  D01-GI-CLASS                   PIC 9(3).
           03  FILLER                         PIC X(02).
           03  D01-GI-CASH                    PIC 9(11).
           03  FILLER                         PIC X(02).
           03  D01-GI-ACH-BANK                PIC 9(11).
           03  FILLER                         PIC X(02).
           03  D01-GI-LN-APPROVAL-LIMIT       PIC 9(7).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOGIIN_DEF.CPY".
       COPY "LIBIO/IOGIIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGIIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PR-FILE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                  TO GI-PATH-OVERRIDE.

           MOVE BEG-BRANCH            TO QGI1-WBEG-BRANCH.
           MOVE BEG-CLASS             TO QGI1-WBEG-CLASS.

           MOVE END-BRANCH            TO QGI1-WEND-BRANCH.
           MOVE END-CLASS             TO QGI1-WEND-CLASS.

           PERFORM OPEN-GI1-FILE.
           PERFORM START-GI1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-GI1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GI-BRANCH < BEG-BRANCH )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( GI-BRANCH > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

           IF GI-CLASS < BEG-CLASS OR > END-CLASS
               GO TO MAIN-PROGRAM-NEXT.

           MOVE GI-BRANCH                     TO D01-GI-BRANCH.
           MOVE GI-CLASS                     TO D01-GI-CLASS.
           MOVE GI-CASH                      TO D01-GI-CASH.
           MOVE GI-ACH-BANK                  TO D01-GI-ACH-BANK.
           MOVE GI-LN-APPROVAL-LIMIT         TO D01-GI-LN-APPROVAL-LIMIT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOGIIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGIIN-QUEUE-POS  TO Q-POS.
           MOVE IOGIIN-COPIES-POS TO C-POS.
           MOVE IOGIIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-BEG-CLASS..

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE 0                   TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-BRANCH." TO VDU.
           MOVE ALL "9"              TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "E  A030END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-CLASS..

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SN N030BEG-CLASS." TO VDU.
           MOVE 0                   TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030END-CLASS." TO VDU.
           MOVE ALL "9"              TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           MOVE "SN N040BEG-BRANCH."    TO VDU.
           MOVE BEG-BRANCH              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-BRANCH."    TO VDU.
           MOVE END-BRANCH              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030BEG-CLASS."    TO VDU.
           MOVE BEG-CLASS              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030END-CLASS."    TO VDU.
           MOVE END-CLASS              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE GI1-KEY     TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BRANCH               : "    TO R01-PROMPT.
           MOVE BEG-BRANCH                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRANCH               : "    TO R01-PROMPT.
           MOVE END-BRANCH                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - GIFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOGIIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGIIN-SYSDATE.
           DISPLAY IOGIIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGIIN-FORM-FIELDS TO WR-BUF.
           MOVE IOGIIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOGIIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGIIN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOGIIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD GI-REC FROM QGI-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GI-FIELDS.
           MOVE QGI-BRANCH TO GI-BRANCH.
           MOVE QGI-CLASS TO GI-CLASS.

           IF ( QGI-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GI-LTOUCH-DATE
           ELSE
              MOVE QGI-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GI-LTOUCH-DATE.

           MOVE QGI-CASH TO GI-CASH.
           MOVE QGI-BANK TO GI-BANK.
           MOVE QGI-NSF TO GI-NSF.
           MOVE QGI-AR TO GI-AR.
           MOVE QGI-PL-1 TO GI-PL(1).
           MOVE QGI-PL-2 TO GI-PL(2).
           MOVE QGI-PL-3 TO GI-PL(3).
           MOVE QGI-PL-4 TO GI-PL(4).
           MOVE QGI-PL-5 TO GI-PL(5).
           MOVE QGI-DLINSREB TO GI-DLINSREB.
           MOVE QGI-ACR-INT-AR-AR TO GI-ACR-INT-AR-AR.
           MOVE QGI-OTHBR-REC TO GI-OTHBR-REC.
           MOVE QGI-OTHBR-PAY TO GI-OTHBR-PAY.
           MOVE QGI-UNINTCHG TO GI-UNINTCHG.
           MOVE QGI-UNADDON TO GI-UNADDON.
           MOVE QGI-UNSERCHG TO GI-UNSERCHG.
           MOVE QGI-UNMAINTFEE TO GI-UNMAINTFEE.
           MOVE QGI-UNDLDISC TO GI-UNDLDISC.
           MOVE QGI-UNOTHER TO GI-UNOTHER.
           MOVE QGI-UN7 TO GI-UN7.
           MOVE QGI-UN8 TO GI-UN8.
           MOVE QGI-UN9 TO GI-UN9.
           MOVE QGI-UN10 TO GI-UN10.
           MOVE QGI-UNCL1 TO GI-UNCL1.
           MOVE QGI-UNCL2 TO GI-UNCL2.
           MOVE QGI-UNAH TO GI-UNAH.
           MOVE QGI-UNPP TO GI-UNPP.
           MOVE QGI-UNO1 TO GI-UNO1.
           MOVE QGI-UNO2 TO GI-UNO2.
           MOVE QGI-UNO3 TO GI-UNO3.
           MOVE QGI-UNO4 TO GI-UNO4.
           MOVE QGI-UNO5 TO GI-UNO5.
           MOVE QGI-UNO6 TO GI-UNO6.
           MOVE QGI-UNO7 TO GI-UNO7.
           MOVE QGI-UNINTREPO TO GI-UNINTREPO.
           MOVE QGI-UNADDONINTREPO TO GI-UNADDONINTREPO.
           MOVE QGI-PLEXP-1 TO GI-PLEXP(1).
           MOVE QGI-PLEXP-2 TO GI-PLEXP(2).
           MOVE QGI-PLEXP-3 TO GI-PLEXP(3).
           MOVE QGI-PLEXP-4 TO GI-PLEXP(4).
           MOVE QGI-PLEXP-5 TO GI-PLEXP(5).
           MOVE QGI-DLPARVEX TO GI-DLPARVEX.
           MOVE QGI-PLOFFSET-1 TO GI-PLOFFSET(1).
           MOVE QGI-PLOFFSET-2 TO GI-PLOFFSET(2).
           MOVE QGI-PLOFFSET-3 TO GI-PLOFFSET(3).
           MOVE QGI-PLOFFSET-4 TO GI-PLOFFSET(4).
           MOVE QGI-PLOFFSET-5 TO GI-PLOFFSET(5).
           MOVE QGI-SERVOFF TO GI-SERVOFF.
           MOVE QGI-PAYCL1 TO GI-PAYCL1.
           MOVE QGI-PAYCL2 TO GI-PAYCL2.
           MOVE QGI-PAYAH TO GI-PAYAH.
           MOVE QGI-PAYPP TO GI-PAYPP.
           MOVE QGI-PAYO1 TO GI-PAYO1.
           MOVE QGI-PAYO2 TO GI-PAYO2.
           MOVE QGI-PAYO3 TO GI-PAYO3.
           MOVE QGI-PAYO4 TO GI-PAYO4.
           MOVE QGI-PAYO5 TO GI-PAYO5.
           MOVE QGI-PAYO6 TO GI-PAYO6.
           MOVE QGI-PAYO7 TO GI-PAYO7.
           MOVE QGI-DLPAYABLE TO GI-DLPAYABLE.
           MOVE QGI-DLRVPRIN-1 TO GI-DLRVPRIN(1).
           MOVE QGI-DLRVPRIN-2 TO GI-DLRVPRIN(2).
           MOVE QGI-DLRVPRIN-3 TO GI-DLRVPRIN(3).
           MOVE QGI-DLRVCHGS-1 TO GI-DLRVCHGS(1).
           MOVE QGI-DLRVCHGS-2 TO GI-DLRVCHGS(2).
           MOVE QGI-DLRVCHGS-3 TO GI-DLRVCHGS(3).
           MOVE QGI-DLPARV TO GI-DLPARV.
           MOVE QGI-PAYALLOT TO GI-PAYALLOT.
           MOVE QGI-DIRECT-PAY TO GI-DIRECT-PAY.
           MOVE QGI-ERINTCHG TO GI-ERINTCHG.
           MOVE QGI-ERADDON TO GI-ERADDON.
           MOVE QGI-ERSERCHG TO GI-ERSERCHG.
           MOVE QGI-ERMAINTFEE TO GI-ERMAINTFEE.
           MOVE QGI-ERDLDISC TO GI-ERDLDISC.
           MOVE QGI-EROTHER TO GI-EROTHER.
           MOVE QGI-EARN7 TO GI-EARN7.
           MOVE QGI-EARN8 TO GI-EARN8.
           MOVE QGI-EARN9 TO GI-EARN9.
           MOVE QGI-EARN10 TO GI-EARN10.
           MOVE QGI-LATECHG TO GI-LATECHG.
           MOVE QGI-INTEREST TO GI-INTEREST.
           MOVE QGI-DEFERMENTS TO GI-DEFERMENTS.
           MOVE QGI-SAPRINCIPAL TO GI-SAPRINCIPAL.
           MOVE QGI-PLINT-1 TO GI-PLINT(1).
           MOVE QGI-PLINT-2 TO GI-PLINT(2).
           MOVE QGI-PLINT-3 TO GI-PLINT(3).
           MOVE QGI-PLINT-4 TO GI-PLINT(4).
           MOVE QGI-PLINT-5 TO GI-PLINT(5).
           MOVE QGI-PLREC-1 TO GI-PLREC(1).
           MOVE QGI-PLREC-2 TO GI-PLREC(2).
           MOVE QGI-PLREC-3 TO GI-PLREC(3).
           MOVE QGI-PLREC-4 TO GI-PLREC(4).
           MOVE QGI-PLREC-5 TO GI-PLREC(5).
           MOVE QGI-CL1 TO GI-CL1.
           MOVE QGI-CL2 TO GI-CL2.
           MOVE QGI-AH TO GI-AH.
           MOVE QGI-PP TO GI-PP.
           MOVE QGI-O1 TO GI-O1.
           MOVE QGI-O2 TO GI-O2.
           MOVE QGI-O3 TO GI-O3.
           MOVE QGI-O4 TO GI-O4.
           MOVE QGI-O5 TO GI-O5.
           MOVE QGI-O6 TO GI-O6.
           MOVE QGI-O7 TO GI-O7.
           MOVE QGI-PLNEW-AR-1 TO GI-PLNEW-AR(1).
           MOVE QGI-PLNEW-AR-2 TO GI-PLNEW-AR(2).
           MOVE QGI-PLNEW-AR-3 TO GI-PLNEW-AR(3).
           MOVE QGI-PLNEW-AR-4 TO GI-PLNEW-AR(4).
           MOVE QGI-PLNEW-AR-5 TO GI-PLNEW-AR(5).
           MOVE QGI-REPO-AR TO GI-REPO-AR.
           MOVE QGI-REPO-INT TO GI-REPO-INT.
           MOVE QGI-ACR-INT-AR TO GI-ACR-INT-AR.
           MOVE QGI-CONV-OFFSET TO GI-CONV-OFFSET.
           MOVE QGI-PARTIAL-PLEXP TO GI-PARTIAL-PLEXP.
           MOVE QGI-PAYLBOX TO GI-PAYLBOX.
           MOVE QGI-PENALTY-INT TO GI-PENALTY-INT.
           MOVE QGI-PAYTPAY TO GI-PAYTPAY.
           MOVE QGI-PAYTCOLL TO GI-PAYTCOLL.
           MOVE QGI-PAYMGRAM TO GI-PAYMGRAM.
           MOVE QGI-REPO-ADDON-EXP TO GI-REPO-ADDON-EXP.
           MOVE QGI-ACH-BANK TO GI-ACH-BANK.
           MOVE QGI-LN-APPROVAL-LIMIT TO GI-LN-APPROVAL-LIMIT.
 
      *-----------------------------------------------------------------
      *    LOAD QGI-REC FROM GI-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GI-FIELDS.
           MOVE GI-BRANCH TO QGI-BRANCH.
           MOVE GI-CLASS TO QGI-CLASS.

           IF ( GI-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGI-LTOUCH-DATE
           ELSE
              MOVE GI-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGI-LTOUCH-DATE.

           MOVE GI-CASH TO QGI-CASH.
           MOVE GI-BANK TO QGI-BANK.
           MOVE GI-NSF TO QGI-NSF.
           MOVE GI-AR TO QGI-AR.
           MOVE GI-PL(1) TO QGI-PL-1.
           MOVE GI-PL(2) TO QGI-PL-2.
           MOVE GI-PL(3) TO QGI-PL-3.
           MOVE GI-PL(4) TO QGI-PL-4.
           MOVE GI-PL(5) TO QGI-PL-5.
           MOVE GI-DLINSREB TO QGI-DLINSREB.
           MOVE GI-ACR-INT-AR-AR TO QGI-ACR-INT-AR-AR.
           MOVE GI-OTHBR-REC TO QGI-OTHBR-REC.
           MOVE GI-OTHBR-PAY TO QGI-OTHBR-PAY.
           MOVE GI-UNINTCHG TO QGI-UNINTCHG.
           MOVE GI-UNADDON TO QGI-UNADDON.
           MOVE GI-UNSERCHG TO QGI-UNSERCHG.
           MOVE GI-UNMAINTFEE TO QGI-UNMAINTFEE.
           MOVE GI-UNDLDISC TO QGI-UNDLDISC.
           MOVE GI-UNOTHER TO QGI-UNOTHER.
           MOVE GI-UN7 TO QGI-UN7.
           MOVE GI-UN8 TO QGI-UN8.
           MOVE GI-UN9 TO QGI-UN9.
           MOVE GI-UN10 TO QGI-UN10.
           MOVE GI-UNCL1 TO QGI-UNCL1.
           MOVE GI-UNCL2 TO QGI-UNCL2.
           MOVE GI-UNAH TO QGI-UNAH.
           MOVE GI-UNPP TO QGI-UNPP.
           MOVE GI-UNO1 TO QGI-UNO1.
           MOVE GI-UNO2 TO QGI-UNO2.
           MOVE GI-UNO3 TO QGI-UNO3.
           MOVE GI-UNO4 TO QGI-UNO4.
           MOVE GI-UNO5 TO QGI-UNO5.
           MOVE GI-UNO6 TO QGI-UNO6.
           MOVE GI-UNO7 TO QGI-UNO7.
           MOVE GI-UNINTREPO TO QGI-UNINTREPO.
           MOVE GI-UNADDONINTREPO TO QGI-UNADDONINTREPO.
           MOVE GI-PLEXP(1) TO QGI-PLEXP-1.
           MOVE GI-PLEXP(2) TO QGI-PLEXP-2.
           MOVE GI-PLEXP(3) TO QGI-PLEXP-3.
           MOVE GI-PLEXP(4) TO QGI-PLEXP-4.
           MOVE GI-PLEXP(5) TO QGI-PLEXP-5.
           MOVE GI-DLPARVEX TO QGI-DLPARVEX.
           MOVE GI-PLOFFSET(1) TO QGI-PLOFFSET-1.
           MOVE GI-PLOFFSET(2) TO QGI-PLOFFSET-2.
           MOVE GI-PLOFFSET(3) TO QGI-PLOFFSET-3.
           MOVE GI-PLOFFSET(4) TO QGI-PLOFFSET-4.
           MOVE GI-PLOFFSET(5) TO QGI-PLOFFSET-5.
           MOVE GI-SERVOFF TO QGI-SERVOFF.
           MOVE GI-PAYCL1 TO QGI-PAYCL1.
           MOVE GI-PAYCL2 TO QGI-PAYCL2.
           MOVE GI-PAYAH TO QGI-PAYAH.
           MOVE GI-PAYPP TO QGI-PAYPP.
           MOVE GI-PAYO1 TO QGI-PAYO1.
           MOVE GI-PAYO2 TO QGI-PAYO2.
           MOVE GI-PAYO3 TO QGI-PAYO3.
           MOVE GI-PAYO4 TO QGI-PAYO4.
           MOVE GI-PAYO5 TO QGI-PAYO5.
           MOVE GI-PAYO6 TO QGI-PAYO6.
           MOVE GI-PAYO7 TO QGI-PAYO7.
           MOVE GI-DLPAYABLE TO QGI-DLPAYABLE.
           MOVE GI-DLRVPRIN(1) TO QGI-DLRVPRIN-1.
           MOVE GI-DLRVPRIN(2) TO QGI-DLRVPRIN-2.
           MOVE GI-DLRVPRIN(3) TO QGI-DLRVPRIN-3.
           MOVE GI-DLRVCHGS(1) TO QGI-DLRVCHGS-1.
           MOVE GI-DLRVCHGS(2) TO QGI-DLRVCHGS-2.
           MOVE GI-DLRVCHGS(3) TO QGI-DLRVCHGS-3.
           MOVE GI-DLPARV TO QGI-DLPARV.
           MOVE GI-PAYALLOT TO QGI-PAYALLOT.
           MOVE GI-DIRECT-PAY TO QGI-DIRECT-PAY.
           MOVE GI-ERINTCHG TO QGI-ERINTCHG.
           MOVE GI-ERADDON TO QGI-ERADDON.
           MOVE GI-ERSERCHG TO QGI-ERSERCHG.
           MOVE GI-ERMAINTFEE TO QGI-ERMAINTFEE.
           MOVE GI-ERDLDISC TO QGI-ERDLDISC.
           MOVE GI-EROTHER TO QGI-EROTHER.
           MOVE GI-EARN7 TO QGI-EARN7.
           MOVE GI-EARN8 TO QGI-EARN8.
           MOVE GI-EARN9 TO QGI-EARN9.
           MOVE GI-EARN10 TO QGI-EARN10.
           MOVE GI-LATECHG TO QGI-LATECHG.
           MOVE GI-INTEREST TO QGI-INTEREST.
           MOVE GI-DEFERMENTS TO QGI-DEFERMENTS.
           MOVE GI-SAPRINCIPAL TO QGI-SAPRINCIPAL.
           MOVE GI-PLINT(1) TO QGI-PLINT-1.
           MOVE GI-PLINT(2) TO QGI-PLINT-2.
           MOVE GI-PLINT(3) TO QGI-PLINT-3.
           MOVE GI-PLINT(4) TO QGI-PLINT-4.
           MOVE GI-PLINT(5) TO QGI-PLINT-5.
           MOVE GI-PLREC(1) TO QGI-PLREC-1.
           MOVE GI-PLREC(2) TO QGI-PLREC-2.
           MOVE GI-PLREC(3) TO QGI-PLREC-3.
           MOVE GI-PLREC(4) TO QGI-PLREC-4.
           MOVE GI-PLREC(5) TO QGI-PLREC-5.
           MOVE GI-CL1 TO QGI-CL1.
           MOVE GI-CL2 TO QGI-CL2.
           MOVE GI-AH TO QGI-AH.
           MOVE GI-PP TO QGI-PP.
           MOVE GI-O1 TO QGI-O1.
           MOVE GI-O2 TO QGI-O2.
           MOVE GI-O3 TO QGI-O3.
           MOVE GI-O4 TO QGI-O4.
           MOVE GI-O5 TO QGI-O5.
           MOVE GI-O6 TO QGI-O6.
           MOVE GI-O7 TO QGI-O7.
           MOVE GI-PLNEW-AR(1) TO QGI-PLNEW-AR-1.
           MOVE GI-PLNEW-AR(2) TO QGI-PLNEW-AR-2.
           MOVE GI-PLNEW-AR(3) TO QGI-PLNEW-AR-3.
           MOVE GI-PLNEW-AR(4) TO QGI-PLNEW-AR-4.
           MOVE GI-PLNEW-AR(5) TO QGI-PLNEW-AR-5.
           MOVE GI-REPO-AR TO QGI-REPO-AR.
           MOVE GI-REPO-INT TO QGI-REPO-INT.
           MOVE GI-ACR-INT-AR TO QGI-ACR-INT-AR.
           MOVE GI-CONV-OFFSET TO QGI-CONV-OFFSET.
           MOVE GI-PARTIAL-PLEXP TO QGI-PARTIAL-PLEXP.
           MOVE GI-PAYLBOX TO QGI-PAYLBOX.
           MOVE GI-PENALTY-INT TO QGI-PENALTY-INT.
           MOVE GI-PAYTPAY TO QGI-PAYTPAY.
           MOVE GI-PAYTCOLL TO QGI-PAYTCOLL.
           MOVE GI-PAYMGRAM TO QGI-PAYMGRAM.
           MOVE GI-REPO-ADDON-EXP TO QGI-REPO-ADDON-EXP.
           MOVE GI-ACH-BANK TO QGI-ACH-BANK.
           MOVE GI-LN-APPROVAL-LIMIT TO QGI-LN-APPROVAL-LIMIT.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-GI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GI-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GI-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GI-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GI-ALL-BASE
              MOVE FILPATH-MACHINE         TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GIFILE NOT = "S" )
              MOVE GI-PATH-BEG             TO GI-SELECT-PATH-BEG
              MOVE GI-PATH-FILE            TO GI-SELECT-PATH-FILE
           ELSE
              MOVE GI-PATH-SQL             TO GI-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GI1-FILE.
           PERFORM LOAD-GI1-FILE.
           PERFORM OPEN-IT.
           MOVE GI-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GI1-FILE.
           PERFORM START-IT.
           MOVE GI-SELECT-PATH TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.

           IF ( GI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GI1-FILE.

           IF ( QGI1-WEND-CLASS = ZEROES    ) OR
              ( QGI1-WEND-CLASS NOT NUMERIC )
              MOVE GI-BRANCH            TO QGI1-WBEG-BRANCH
              MOVE ALL "9"              TO QGI1-WEND-BRANCH
              MOVE GI-CLASS             TO QGI1-WBEG-CLASS
              MOVE ALL "9"              TO QGI1-WEND-CLASS.

           EXEC SQL
            DECLARE GI1_CURSOR CURSOR FOR
            SELECT GIFILE.GI_BRANCH,
                   GIFILE.GI_CLASS,
                   ISNULL(CAST(GIFILE.GI_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GIFILE.GI_CASH,
                   GIFILE.GI_BANK,
                   GIFILE.GI_NSF,
                   GIFILE.GI_AR,
                   GIFILE.GI_PL_1,
                   GIFILE.GI_PL_2,
                   GIFILE.GI_PL_3,
                   GIFILE.GI_PL_4,
                   GIFILE.GI_PL_5,
                   GIFILE.GI_DLINSREB,
                   GIFILE.GI_ACR_INT_AR_AR,
                   GIFILE.GI_OTHBR_REC,
                   GIFILE.GI_OTHBR_PAY,
                   GIFILE.GI_UNINTCHG,
                   GIFILE.GI_UNADDON,
                   GIFILE.GI_UNSERCHG,
                   GIFILE.GI_UNMAINTFEE,
                   GIFILE.GI_UNDLDISC,
                   GIFILE.GI_UNOTHER,
                   GIFILE.GI_UN7,
                   GIFILE.GI_UN8,
                   GIFILE.GI_UN9,
                   GIFILE.GI_UN10,
                   GIFILE.GI_UNCL1,
                   GIFILE.GI_UNCL2,
                   GIFILE.GI_UNAH,
                   GIFILE.GI_UNPP,
                   GIFILE.GI_UNO1,
                   GIFILE.GI_UNO2,
                   GIFILE.GI_UNO3,
                   GIFILE.GI_UNO4,
                   GIFILE.GI_UNO5,
                   GIFILE.GI_UNO6,
                   GIFILE.GI_UNO7,
                   GIFILE.GI_UNINTREPO,
                   GIFILE.GI_UNADDONINTREPO,
                   GIFILE.GI_PLEXP_1,
                   GIFILE.GI_PLEXP_2,
                   GIFILE.GI_PLEXP_3,
                   GIFILE.GI_PLEXP_4,
                   GIFILE.GI_PLEXP_5,
                   GIFILE.GI_DLPARVEX,
                   GIFILE.GI_PLOFFSET_1,
                   GIFILE.GI_PLOFFSET_2,
                   GIFILE.GI_PLOFFSET_3,
                   GIFILE.GI_PLOFFSET_4,
                   GIFILE.GI_PLOFFSET_5,
                   GIFILE.GI_SERVOFF,
                   GIFILE.GI_PAYCL1,
                   GIFILE.GI_PAYCL2,
                   GIFILE.GI_PAYAH,
                   GIFILE.GI_PAYPP,
                   GIFILE.GI_PAYO1,
                   GIFILE.GI_PAYO2,
                   GIFILE.GI_PAYO3,
                   GIFILE.GI_PAYO4,
                   GIFILE.GI_PAYO5,
                   GIFILE.GI_PAYO6,
                   GIFILE.GI_PAYO7,
                   GIFILE.GI_DLPAYABLE,
                   GIFILE.GI_DLRVPRIN_1,
                   GIFILE.GI_DLRVPRIN_2,
                   GIFILE.GI_DLRVPRIN_3,
                   GIFILE.GI_DLRVCHGS_1,
                   GIFILE.GI_DLRVCHGS_2,
                   GIFILE.GI_DLRVCHGS_3,
                   GIFILE.GI_DLPARV,
                   GIFILE.GI_PAYALLOT,
                   GIFILE.GI_DIRECT_PAY,
                   GIFILE.GI_ERINTCHG,
                   GIFILE.GI_ERADDON,
                   GIFILE.GI_ERSERCHG,
                   GIFILE.GI_ERMAINTFEE,
                   GIFILE.GI_ERDLDISC,
                   GIFILE.GI_EROTHER,
                   GIFILE.GI_EARN7,
                   GIFILE.GI_EARN8,
                   GIFILE.GI_EARN9,
                   GIFILE.GI_EARN10,
                   GIFILE.GI_LATECHG,
                   GIFILE.GI_INTEREST,
                   GIFILE.GI_DEFERMENTS,
                   GIFILE.GI_SAPRINCIPAL,
                   GIFILE.GI_PLINT_1,
                   GIFILE.GI_PLINT_2,
                   GIFILE.GI_PLINT_3,
                   GIFILE.GI_PLINT_4,
                   GIFILE.GI_PLINT_5,
                   GIFILE.GI_PLREC_1,
                   GIFILE.GI_PLREC_2,
                   GIFILE.GI_PLREC_3,
                   GIFILE.GI_PLREC_4,
                   GIFILE.GI_PLREC_5,
                   GIFILE.GI_CL1,
                   GIFILE.GI_CL2,
                   GIFILE.GI_AH,
                   GIFILE.GI_PP,
                   GIFILE.GI_O1,
                   GIFILE.GI_O2,
                   GIFILE.GI_O3,
                   GIFILE.GI_O4,
                   GIFILE.GI_O5,
                   GIFILE.GI_O6,
                   GIFILE.GI_O7,
                   GIFILE.GI_PLNEW_AR_1,
                   GIFILE.GI_PLNEW_AR_2,
                   GIFILE.GI_PLNEW_AR_3,
                   GIFILE.GI_PLNEW_AR_4,
                   GIFILE.GI_PLNEW_AR_5,
                   GIFILE.GI_REPO_AR,
                   GIFILE.GI_REPO_INT,
                   GIFILE.GI_ACR_INT_AR,
                   GIFILE.GI_CONV_OFFSET,
                   GIFILE.GI_PARTIAL_PLEXP,
                   GIFILE.GI_PAYLBOX,
                   GIFILE.GI_PENALTY_INT,
                   GIFILE.GI_PAYTPAY,
                   GIFILE.GI_PAYTCOLL,
                   GIFILE.GI_PAYMGRAM,
                   GIFILE.GI_REPO_ADDON_EXP,
                   GIFILE.GI_ACH_BANK,
                   GIFILE.GI_LN_APPROVAL_LIMIT
            FROM DBO.GIFILE
            WHERE GIFILE.GI_BRANCH >= :QGI1-WBEG-BRANCH
              AND GIFILE.GI_BRANCH <= :QGI1-WEND-BRANCH
              AND GIFILE.GI_CLASS >= :QGI1-WBEG-CLASS
              AND GIFILE.GI_CLASS <= :QGI1-WEND-CLASS
            ORDER BY GIFILE.GI_BRANCH,
                     GIFILE.GI_CLASS
           END-EXEC.

           EXEC SQL
               OPEN GI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGI1-WBEG-BRANCH
                          QGI1-WBEG-CLASS
                          QGI1-WEND-BRANCH
                          QGI1-WEND-CLASS.

           MOVE STAT---GOOD TO GI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GI1-FILE.
           PERFORM READ-IT.
           MOVE GI-SELECT-PATH TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.

           MOVE GI-BRANCH    TO QGI-BRANCH. 
           MOVE GI-CLASS     TO QGI-CLASS.

           EXEC SQL
            SELECT GIFILE.GI_BRANCH,
                   GIFILE.GI_CLASS,
                   ISNULL(CAST(GIFILE.GI_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GIFILE.GI_CASH,
                   GIFILE.GI_BANK,
                   GIFILE.GI_NSF,
                   GIFILE.GI_AR,
                   GIFILE.GI_PL_1,
                   GIFILE.GI_PL_2,
                   GIFILE.GI_PL_3,
                   GIFILE.GI_PL_4,
                   GIFILE.GI_PL_5,
                   GIFILE.GI_DLINSREB,
                   GIFILE.GI_ACR_INT_AR_AR,
                   GIFILE.GI_OTHBR_REC,
                   GIFILE.GI_OTHBR_PAY,
                   GIFILE.GI_UNINTCHG,
                   GIFILE.GI_UNADDON,
                   GIFILE.GI_UNSERCHG,
                   GIFILE.GI_UNMAINTFEE,
                   GIFILE.GI_UNDLDISC,
                   GIFILE.GI_UNOTHER,
                   GIFILE.GI_UN7,
                   GIFILE.GI_UN8,
                   GIFILE.GI_UN9,
                   GIFILE.GI_UN10,
                   GIFILE.GI_UNCL1,
                   GIFILE.GI_UNCL2,
                   GIFILE.GI_UNAH,
                   GIFILE.GI_UNPP,
                   GIFILE.GI_UNO1,
                   GIFILE.GI_UNO2,
                   GIFILE.GI_UNO3,
                   GIFILE.GI_UNO4,
                   GIFILE.GI_UNO5,
                   GIFILE.GI_UNO6,
                   GIFILE.GI_UNO7,
                   GIFILE.GI_UNINTREPO,
                   GIFILE.GI_UNADDONINTREPO,
                   GIFILE.GI_PLEXP_1,
                   GIFILE.GI_PLEXP_2,
                   GIFILE.GI_PLEXP_3,
                   GIFILE.GI_PLEXP_4,
                   GIFILE.GI_PLEXP_5,
                   GIFILE.GI_DLPARVEX,
                   GIFILE.GI_PLOFFSET_1,
                   GIFILE.GI_PLOFFSET_2,
                   GIFILE.GI_PLOFFSET_3,
                   GIFILE.GI_PLOFFSET_4,
                   GIFILE.GI_PLOFFSET_5,
                   GIFILE.GI_SERVOFF,
                   GIFILE.GI_PAYCL1,
                   GIFILE.GI_PAYCL2,
                   GIFILE.GI_PAYAH,
                   GIFILE.GI_PAYPP,
                   GIFILE.GI_PAYO1,
                   GIFILE.GI_PAYO2,
                   GIFILE.GI_PAYO3,
                   GIFILE.GI_PAYO4,
                   GIFILE.GI_PAYO5,
                   GIFILE.GI_PAYO6,
                   GIFILE.GI_PAYO7,
                   GIFILE.GI_DLPAYABLE,
                   GIFILE.GI_DLRVPRIN_1,
                   GIFILE.GI_DLRVPRIN_2,
                   GIFILE.GI_DLRVPRIN_3,
                   GIFILE.GI_DLRVCHGS_1,
                   GIFILE.GI_DLRVCHGS_2,
                   GIFILE.GI_DLRVCHGS_3,
                   GIFILE.GI_DLPARV,
                   GIFILE.GI_PAYALLOT,
                   GIFILE.GI_DIRECT_PAY,
                   GIFILE.GI_ERINTCHG,
                   GIFILE.GI_ERADDON,
                   GIFILE.GI_ERSERCHG,
                   GIFILE.GI_ERMAINTFEE,
                   GIFILE.GI_ERDLDISC,
                   GIFILE.GI_EROTHER,
                   GIFILE.GI_EARN7,
                   GIFILE.GI_EARN8,
                   GIFILE.GI_EARN9,
                   GIFILE.GI_EARN10,
                   GIFILE.GI_LATECHG,
                   GIFILE.GI_INTEREST,
                   GIFILE.GI_DEFERMENTS,
                   GIFILE.GI_SAPRINCIPAL,
                   GIFILE.GI_PLINT_1,
                   GIFILE.GI_PLINT_2,
                   GIFILE.GI_PLINT_3,
                   GIFILE.GI_PLINT_4,
                   GIFILE.GI_PLINT_5,
                   GIFILE.GI_PLREC_1,
                   GIFILE.GI_PLREC_2,
                   GIFILE.GI_PLREC_3,
                   GIFILE.GI_PLREC_4,
                   GIFILE.GI_PLREC_5,
                   GIFILE.GI_CL1,
                   GIFILE.GI_CL2,
                   GIFILE.GI_AH,
                   GIFILE.GI_PP,
                   GIFILE.GI_O1,
                   GIFILE.GI_O2,
                   GIFILE.GI_O3,
                   GIFILE.GI_O4,
                   GIFILE.GI_O5,
                   GIFILE.GI_O6,
                   GIFILE.GI_O7,
                   GIFILE.GI_PLNEW_AR_1,
                   GIFILE.GI_PLNEW_AR_2,
                   GIFILE.GI_PLNEW_AR_3,
                   GIFILE.GI_PLNEW_AR_4,
                   GIFILE.GI_PLNEW_AR_5,
                   GIFILE.GI_REPO_AR,
                   GIFILE.GI_REPO_INT,
                   GIFILE.GI_ACR_INT_AR,
                   GIFILE.GI_CONV_OFFSET,
                   GIFILE.GI_PARTIAL_PLEXP,
                   GIFILE.GI_PAYLBOX,
                   GIFILE.GI_PENALTY_INT,
                   GIFILE.GI_PAYTPAY,
                   GIFILE.GI_PAYTCOLL,
                   GIFILE.GI_PAYMGRAM,
                   GIFILE.GI_REPO_ADDON_EXP,
                   GIFILE.GI_ACH_BANK,
                   GIFILE.GI_LN_APPROVAL_LIMIT
            INTO :QGI-REC
            FROM DBO.GIFILE
            WHERE GIFILE.GI_BRANCH = :QGI-BRANCH
              AND GIFILE.GI_CLASS = :QGI-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GI-FIELDS.

      *-----------------------------------------------------------------
       READ-GI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GI-SELECT-PATH TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           INITIALIZE QGI-REC.

           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GI1_CURSOR INTO :QGI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GI-FIELDS.


      *-----------------------------------------------------------------
       WRITE-GI1-FILE.

           ACCEPT GI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GI-SELECT-PATH TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           PERFORM SET-GI-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GIFILE
             (GIFILE.GI_BRANCH,
              GIFILE.GI_CLASS,
              GIFILE.GI_LTOUCH_DATE,
              GIFILE.GI_CASH,
              GIFILE.GI_BANK,
              GIFILE.GI_NSF,
              GIFILE.GI_AR,
              GIFILE.GI_PL_1,
              GIFILE.GI_PL_2,
              GIFILE.GI_PL_3,
              GIFILE.GI_PL_4,
              GIFILE.GI_PL_5,
              GIFILE.GI_DLINSREB,
              GIFILE.GI_ACR_INT_AR_AR,
              GIFILE.GI_OTHBR_REC,
              GIFILE.GI_OTHBR_PAY,
              GIFILE.GI_UNINTCHG,
              GIFILE.GI_UNADDON,
              GIFILE.GI_UNSERCHG,
              GIFILE.GI_UNMAINTFEE,
              GIFILE.GI_UNDLDISC,
              GIFILE.GI_UNOTHER,
              GIFILE.GI_UN7,
              GIFILE.GI_UN8,
              GIFILE.GI_UN9,
              GIFILE.GI_UN10,
              GIFILE.GI_UNCL1,
              GIFILE.GI_UNCL2,
              GIFILE.GI_UNAH,
              GIFILE.GI_UNPP,
              GIFILE.GI_UNO1,
              GIFILE.GI_UNO2,
              GIFILE.GI_UNO3,
              GIFILE.GI_UNO4,
              GIFILE.GI_UNO5,
              GIFILE.GI_UNO6,
              GIFILE.GI_UNO7,
              GIFILE.GI_UNINTREPO,
              GIFILE.GI_UNADDONINTREPO,
              GIFILE.GI_PLEXP_1,
              GIFILE.GI_PLEXP_2,
              GIFILE.GI_PLEXP_3,
              GIFILE.GI_PLEXP_4,
              GIFILE.GI_PLEXP_5,
              GIFILE.GI_DLPARVEX,
              GIFILE.GI_PLOFFSET_1,
              GIFILE.GI_PLOFFSET_2,
              GIFILE.GI_PLOFFSET_3,
              GIFILE.GI_PLOFFSET_4,
              GIFILE.GI_PLOFFSET_5,
              GIFILE.GI_SERVOFF,
              GIFILE.GI_PAYCL1,
              GIFILE.GI_PAYCL2,
              GIFILE.GI_PAYAH,
              GIFILE.GI_PAYPP,
              GIFILE.GI_PAYO1,
              GIFILE.GI_PAYO2,
              GIFILE.GI_PAYO3,
              GIFILE.GI_PAYO4,
              GIFILE.GI_PAYO5,
              GIFILE.GI_PAYO6,
              GIFILE.GI_PAYO7,
              GIFILE.GI_DLPAYABLE,
              GIFILE.GI_DLRVPRIN_1,
              GIFILE.GI_DLRVPRIN_2,
              GIFILE.GI_DLRVPRIN_3,
              GIFILE.GI_DLRVCHGS_1,
              GIFILE.GI_DLRVCHGS_2,
              GIFILE.GI_DLRVCHGS_3,
              GIFILE.GI_DLPARV,
              GIFILE.GI_PAYALLOT,
              GIFILE.GI_DIRECT_PAY,
              GIFILE.GI_ERINTCHG,
              GIFILE.GI_ERADDON,
              GIFILE.GI_ERSERCHG,
              GIFILE.GI_ERMAINTFEE,
              GIFILE.GI_ERDLDISC,
              GIFILE.GI_EROTHER,
              GIFILE.GI_EARN7,
              GIFILE.GI_EARN8,
              GIFILE.GI_EARN9,
              GIFILE.GI_EARN10,
              GIFILE.GI_LATECHG,
              GIFILE.GI_INTEREST,
              GIFILE.GI_DEFERMENTS,
              GIFILE.GI_SAPRINCIPAL,
              GIFILE.GI_PLINT_1,
              GIFILE.GI_PLINT_2,
              GIFILE.GI_PLINT_3,
              GIFILE.GI_PLINT_4,
              GIFILE.GI_PLINT_5,
              GIFILE.GI_PLREC_1,
              GIFILE.GI_PLREC_2,
              GIFILE.GI_PLREC_3,
              GIFILE.GI_PLREC_4,
              GIFILE.GI_PLREC_5,
              GIFILE.GI_CL1,
              GIFILE.GI_CL2,
              GIFILE.GI_AH,
              GIFILE.GI_PP,
              GIFILE.GI_O1,
              GIFILE.GI_O2,
              GIFILE.GI_O3,
              GIFILE.GI_O4,
              GIFILE.GI_O5,
              GIFILE.GI_O6,
              GIFILE.GI_O7,
              GIFILE.GI_PLNEW_AR_1,
              GIFILE.GI_PLNEW_AR_2,
              GIFILE.GI_PLNEW_AR_3,
              GIFILE.GI_PLNEW_AR_4,
              GIFILE.GI_PLNEW_AR_5,
              GIFILE.GI_REPO_AR,
              GIFILE.GI_REPO_INT,
              GIFILE.GI_ACR_INT_AR,
              GIFILE.GI_CONV_OFFSET,
              GIFILE.GI_PARTIAL_PLEXP,
              GIFILE.GI_PAYLBOX,
              GIFILE.GI_PENALTY_INT,
              GIFILE.GI_PAYTPAY,
              GIFILE.GI_PAYTCOLL,
              GIFILE.GI_PAYMGRAM,
              GIFILE.GI_REPO_ADDON_EXP,
              GIFILE.GI_ACH_BANK,
              GIFILE.GI_LN_APPROVAL_LIMIT)
            VALUES
             (:QGI-BRANCH,
              :QGI-CLASS,
              NULLIF(:QGI-LTOUCH-DATE,'1900-01-01'),
              :QGI-CASH,
              :QGI-BANK,
              :QGI-NSF,
              :QGI-AR,
              :QGI-PL-1,
              :QGI-PL-2,
              :QGI-PL-3,
              :QGI-PL-4,
              :QGI-PL-5,
              :QGI-DLINSREB,
              :QGI-ACR-INT-AR-AR,
              :QGI-OTHBR-REC,
              :QGI-OTHBR-PAY,
              :QGI-UNINTCHG,
              :QGI-UNADDON,
              :QGI-UNSERCHG,
              :QGI-UNMAINTFEE,
              :QGI-UNDLDISC,
              :QGI-UNOTHER,
              :QGI-UN7,
              :QGI-UN8,
              :QGI-UN9,
              :QGI-UN10,
              :QGI-UNCL1,
              :QGI-UNCL2,
              :QGI-UNAH,
              :QGI-UNPP,
              :QGI-UNO1,
              :QGI-UNO2,
              :QGI-UNO3,
              :QGI-UNO4,
              :QGI-UNO5,
              :QGI-UNO6,
              :QGI-UNO7,
              :QGI-UNINTREPO,
              :QGI-UNADDONINTREPO,
              :QGI-PLEXP-1,
              :QGI-PLEXP-2,
              :QGI-PLEXP-3,
              :QGI-PLEXP-4,
              :QGI-PLEXP-5,
              :QGI-DLPARVEX,
              :QGI-PLOFFSET-1,
              :QGI-PLOFFSET-2,
              :QGI-PLOFFSET-3,
              :QGI-PLOFFSET-4,
              :QGI-PLOFFSET-5,
              :QGI-SERVOFF,
              :QGI-PAYCL1,
              :QGI-PAYCL2,
              :QGI-PAYAH,
              :QGI-PAYPP,
              :QGI-PAYO1,
              :QGI-PAYO2,
              :QGI-PAYO3,
              :QGI-PAYO4,
              :QGI-PAYO5,
              :QGI-PAYO6,
              :QGI-PAYO7,
              :QGI-DLPAYABLE,
              :QGI-DLRVPRIN-1,
              :QGI-DLRVPRIN-2,
              :QGI-DLRVPRIN-3,
              :QGI-DLRVCHGS-1,
              :QGI-DLRVCHGS-2,
              :QGI-DLRVCHGS-3,
              :QGI-DLPARV,
              :QGI-PAYALLOT,
              :QGI-DIRECT-PAY,
              :QGI-ERINTCHG,
              :QGI-ERADDON,
              :QGI-ERSERCHG,
              :QGI-ERMAINTFEE,
              :QGI-ERDLDISC,
              :QGI-EROTHER,
              :QGI-EARN7,
              :QGI-EARN8,
              :QGI-EARN9,
              :QGI-EARN10,
              :QGI-LATECHG,
              :QGI-INTEREST,
              :QGI-DEFERMENTS,
              :QGI-SAPRINCIPAL,
              :QGI-PLINT-1,
              :QGI-PLINT-2,
              :QGI-PLINT-3,
              :QGI-PLINT-4,
              :QGI-PLINT-5,
              :QGI-PLREC-1,
              :QGI-PLREC-2,
              :QGI-PLREC-3,
              :QGI-PLREC-4,
              :QGI-PLREC-5,
              :QGI-CL1,
              :QGI-CL2,
              :QGI-AH,
              :QGI-PP,
              :QGI-O1,
              :QGI-O2,
              :QGI-O3,
              :QGI-O4,
              :QGI-O5,
              :QGI-O6,
              :QGI-O7,
              :QGI-PLNEW-AR-1,
              :QGI-PLNEW-AR-2,
              :QGI-PLNEW-AR-3,
              :QGI-PLNEW-AR-4,
              :QGI-PLNEW-AR-5,
              :QGI-REPO-AR,
              :QGI-REPO-INT,
              :QGI-ACR-INT-AR,
              :QGI-CONV-OFFSET,
              :QGI-PARTIAL-PLEXP,
              :QGI-PAYLBOX,
              :QGI-PENALTY-INT,
              :QGI-PAYTPAY,
              :QGI-PAYTCOLL,
              :QGI-PAYMGRAM,
              :QGI-REPO-ADDON-EXP,
              :QGI-ACH-BANK,
              :QGI-LN-APPROVAL-LIMIT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GI1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GI1-FILE.

           ACCEPT GI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GI-SELECT-PATH TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           PERFORM SET-GI-FIELDS.

           EXEC SQL
            UPDATE DBO.GIFILE
             SET
              GIFILE.GI_BRANCH = :QGI-BRANCH,
              GIFILE.GI_CLASS = :QGI-CLASS,
              GIFILE.GI_LTOUCH_DATE =
                 NULLIF(:QGI-LTOUCH-DATE,'1900-01-01'),
              GIFILE.GI_CASH = :QGI-CASH,
              GIFILE.GI_BANK = :QGI-BANK,
              GIFILE.GI_NSF = :QGI-NSF,
              GIFILE.GI_AR = :QGI-AR,
              GIFILE.GI_PL_1 = :QGI-PL-1,
              GIFILE.GI_PL_2 = :QGI-PL-2,
              GIFILE.GI_PL_3 = :QGI-PL-3,
              GIFILE.GI_PL_4 = :QGI-PL-4,
              GIFILE.GI_PL_5 = :QGI-PL-5,
              GIFILE.GI_DLINSREB = :QGI-DLINSREB,
              GIFILE.GI_ACR_INT_AR_AR = :QGI-ACR-INT-AR-AR,
              GIFILE.GI_OTHBR_REC = :QGI-OTHBR-REC,
              GIFILE.GI_OTHBR_PAY = :QGI-OTHBR-PAY,
              GIFILE.GI_UNINTCHG = :QGI-UNINTCHG,
              GIFILE.GI_UNADDON = :QGI-UNADDON,
              GIFILE.GI_UNSERCHG = :QGI-UNSERCHG,
              GIFILE.GI_UNMAINTFEE = :QGI-UNMAINTFEE,
              GIFILE.GI_UNDLDISC = :QGI-UNDLDISC,
              GIFILE.GI_UNOTHER = :QGI-UNOTHER,
              GIFILE.GI_UN7 = :QGI-UN7,
              GIFILE.GI_UN8 = :QGI-UN8,
              GIFILE.GI_UN9 = :QGI-UN9,
              GIFILE.GI_UN10 = :QGI-UN10,
              GIFILE.GI_UNCL1 = :QGI-UNCL1,
              GIFILE.GI_UNCL2 = :QGI-UNCL2,
              GIFILE.GI_UNAH = :QGI-UNAH,
              GIFILE.GI_UNPP = :QGI-UNPP,
              GIFILE.GI_UNO1 = :QGI-UNO1,
              GIFILE.GI_UNO2 = :QGI-UNO2,
              GIFILE.GI_UNO3 = :QGI-UNO3,
              GIFILE.GI_UNO4 = :QGI-UNO4,
              GIFILE.GI_UNO5 = :QGI-UNO5,
              GIFILE.GI_UNO6 = :QGI-UNO6,
              GIFILE.GI_UNO7 = :QGI-UNO7,
              GIFILE.GI_UNINTREPO = :QGI-UNINTREPO,
              GIFILE.GI_UNADDONINTREPO = :QGI-UNADDONINTREPO,
              GIFILE.GI_PLEXP_1 = :QGI-PLEXP-1,
              GIFILE.GI_PLEXP_2 = :QGI-PLEXP-2,
              GIFILE.GI_PLEXP_3 = :QGI-PLEXP-3,
              GIFILE.GI_PLEXP_4 = :QGI-PLEXP-4,
              GIFILE.GI_PLEXP_5 = :QGI-PLEXP-5,
              GIFILE.GI_DLPARVEX = :QGI-DLPARVEX,
              GIFILE.GI_PLOFFSET_1 = :QGI-PLOFFSET-1,
              GIFILE.GI_PLOFFSET_2 = :QGI-PLOFFSET-2,
              GIFILE.GI_PLOFFSET_3 = :QGI-PLOFFSET-3,
              GIFILE.GI_PLOFFSET_4 = :QGI-PLOFFSET-4,
              GIFILE.GI_PLOFFSET_5 = :QGI-PLOFFSET-5,
              GIFILE.GI_SERVOFF = :QGI-SERVOFF,
              GIFILE.GI_PAYCL1 = :QGI-PAYCL1,
              GIFILE.GI_PAYCL2 = :QGI-PAYCL2,
              GIFILE.GI_PAYAH = :QGI-PAYAH,
              GIFILE.GI_PAYPP = :QGI-PAYPP,
              GIFILE.GI_PAYO1 = :QGI-PAYO1,
              GIFILE.GI_PAYO2 = :QGI-PAYO2,
              GIFILE.GI_PAYO3 = :QGI-PAYO3,
              GIFILE.GI_PAYO4 = :QGI-PAYO4,
              GIFILE.GI_PAYO5 = :QGI-PAYO5,
              GIFILE.GI_PAYO6 = :QGI-PAYO6,
              GIFILE.GI_PAYO7 = :QGI-PAYO7,
              GIFILE.GI_DLPAYABLE = :QGI-DLPAYABLE,
              GIFILE.GI_DLRVPRIN_1 = :QGI-DLRVPRIN-1,
              GIFILE.GI_DLRVPRIN_2 = :QGI-DLRVPRIN-2,
              GIFILE.GI_DLRVPRIN_3 = :QGI-DLRVPRIN-3,
              GIFILE.GI_DLRVCHGS_1 = :QGI-DLRVCHGS-1,
              GIFILE.GI_DLRVCHGS_2 = :QGI-DLRVCHGS-2,
              GIFILE.GI_DLRVCHGS_3 = :QGI-DLRVCHGS-3,
              GIFILE.GI_DLPARV = :QGI-DLPARV,
              GIFILE.GI_PAYALLOT = :QGI-PAYALLOT,
              GIFILE.GI_DIRECT_PAY = :QGI-DIRECT-PAY,
              GIFILE.GI_ERINTCHG = :QGI-ERINTCHG,
              GIFILE.GI_ERADDON = :QGI-ERADDON,
              GIFILE.GI_ERSERCHG = :QGI-ERSERCHG,
              GIFILE.GI_ERMAINTFEE = :QGI-ERMAINTFEE,
              GIFILE.GI_ERDLDISC = :QGI-ERDLDISC,
              GIFILE.GI_EROTHER = :QGI-EROTHER,
              GIFILE.GI_EARN7 = :QGI-EARN7,
              GIFILE.GI_EARN8 = :QGI-EARN8,
              GIFILE.GI_EARN9 = :QGI-EARN9,
              GIFILE.GI_EARN10 = :QGI-EARN10,
              GIFILE.GI_LATECHG = :QGI-LATECHG,
              GIFILE.GI_INTEREST = :QGI-INTEREST,
              GIFILE.GI_DEFERMENTS = :QGI-DEFERMENTS,
              GIFILE.GI_SAPRINCIPAL = :QGI-SAPRINCIPAL,
              GIFILE.GI_PLINT_1 = :QGI-PLINT-1,
              GIFILE.GI_PLINT_2 = :QGI-PLINT-2,
              GIFILE.GI_PLINT_3 = :QGI-PLINT-3,
              GIFILE.GI_PLINT_4 = :QGI-PLINT-4,
              GIFILE.GI_PLINT_5 = :QGI-PLINT-5,
              GIFILE.GI_PLREC_1 = :QGI-PLREC-1,
              GIFILE.GI_PLREC_2 = :QGI-PLREC-2,
              GIFILE.GI_PLREC_3 = :QGI-PLREC-3,
              GIFILE.GI_PLREC_4 = :QGI-PLREC-4,
              GIFILE.GI_PLREC_5 = :QGI-PLREC-5,
              GIFILE.GI_CL1 = :QGI-CL1,
              GIFILE.GI_CL2 = :QGI-CL2,
              GIFILE.GI_AH = :QGI-AH,
              GIFILE.GI_PP = :QGI-PP,
              GIFILE.GI_O1 = :QGI-O1,
              GIFILE.GI_O2 = :QGI-O2,
              GIFILE.GI_O3 = :QGI-O3,
              GIFILE.GI_O4 = :QGI-O4,
              GIFILE.GI_O5 = :QGI-O5,
              GIFILE.GI_O6 = :QGI-O6,
              GIFILE.GI_O7 = :QGI-O7,
              GIFILE.GI_PLNEW_AR_1 = :QGI-PLNEW-AR-1,
              GIFILE.GI_PLNEW_AR_2 = :QGI-PLNEW-AR-2,
              GIFILE.GI_PLNEW_AR_3 = :QGI-PLNEW-AR-3,
              GIFILE.GI_PLNEW_AR_4 = :QGI-PLNEW-AR-4,
              GIFILE.GI_PLNEW_AR_5 = :QGI-PLNEW-AR-5,
              GIFILE.GI_REPO_AR = :QGI-REPO-AR,
              GIFILE.GI_REPO_INT = :QGI-REPO-INT,
              GIFILE.GI_ACR_INT_AR = :QGI-ACR-INT-AR,
              GIFILE.GI_CONV_OFFSET = :QGI-CONV-OFFSET,
              GIFILE.GI_PARTIAL_PLEXP = :QGI-PARTIAL-PLEXP,
              GIFILE.GI_PAYLBOX = :QGI-PAYLBOX,
              GIFILE.GI_PENALTY_INT = :QGI-PENALTY-INT,
              GIFILE.GI_PAYTPAY = :QGI-PAYTPAY,
              GIFILE.GI_PAYTCOLL = :QGI-PAYTCOLL,
              GIFILE.GI_PAYMGRAM = :QGI-PAYMGRAM,
              GIFILE.GI_REPO_ADDON_EXP = :QGI-REPO-ADDON-EXP,
              GIFILE.GI_ACH_BANK = :QGI-ACH-BANK,
              GIFILE.GI_LN_APPROVAL_LIMIT =
                   :QGI-LN-APPROVAL-LIMIT
             WHERE GIFILE.GI_BRANCH = :QGI-BRANCH
               AND GIFILE.GI_CLASS = :QGI-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GI1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GI1-FILE.

           PERFORM DELETE-IT.
           MOVE GI-SELECT-PATH TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           PERFORM SET-GI-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GIFILE
             WHERE GIFILE.GI_BRANCH = :QGI-BRANCH
               AND GIFILE.GI_CLASS = :QGI-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GI1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GI1-FILE.
           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GI1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################

      *=================================================================
      *=================================================================
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOGIIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
