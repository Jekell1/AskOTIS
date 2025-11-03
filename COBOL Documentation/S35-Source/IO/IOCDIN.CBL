       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCDIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCDIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CDFILE
      * TABLE DESC    : CODE FILE
      * OUTPUT FILE   : XPC/IOCDIN.PRE
      * USED FOR      : LIBLP/LPWSCD.CPY  --- QCD-REC
      *                 LIBLP/LPCD1R.CPY  --- READ
      *                 LIBLP/LPCD1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOCDIN.CBL S35 11/01/21-12:25:53 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCD1-WHERE-BEG-KEY.
           03  QCD1-WBEG-TYPE            PIC X(2).
           03  QCD1-WBEG-CODE            PIC X(8).
 
       01  QCD1-WHERE-END-KEY.
           03  QCD1-WEND-TYPE            PIC X(2).
           03  QCD1-WEND-CODE            PIC X(8).
 
       01  QCD-REC.
           03  QCD-TYPE                   PIC X(2).
           03  QCD-CODE                   PIC X(8).
           03  QCD-LTOUCH-DATE            PIC X(10).
           03  QCD-DESC                   PIC X(35).
           03  QCD-AT-ACTIONCD            PIC X(04).
           03  QCD-AT-MEMO-REQUIRED       PIC X(01).
           03  QCD-AT-SUPERVISOR-ONLY     PIC X(01).
           03  QCD-AT-RESULT-REQUIRED     PIC X(01).
           03  QCD-BR-FILE-TYPE           PIC XX.
           03  QCD-BR-FILE-NAME           PIC X(6).
           03  QCD-BR-PAYOFF-FG           PIC X.
           03  QCD-BR-POST-SS-TRCD        PIC X.
           03  QCD-BR-GL-BRANCH           PIC 9(4).
           03  QCD-BR-GL-ACCT             PIC 9(7).
           03  QCD-BR-ALLOT-OPTION        PIC X.
           03  QCD-BR-LBOX-OPTION         PIC X.
           03  QCD-BR-LC-FG               PIC X.
           03  QCD-BR-OT-GL-BRANCH        PIC 9(4).
           03  QCD-BR-OT-GL-ACCT          PIC 9(7).
           03  QCD-BR-MULTIPLE-REFCDS     PIC X.
           03  QCD-BR-COLLECT-LC          PIC X.
           03  QCD-CO-GROUP               PIC X(01).
           03  QCD-CO-SECURED             PIC X(01).
           03  QCD-CO-TRW                 PIC X(02).
           03  QCD-CT-INSTAXFRMLA-1       PIC X.
           03  QCD-CT-INSTAXAMT-1         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-2       PIC X.
           03  QCD-CT-INSTAXAMT-2         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-3       PIC X.
           03  QCD-CT-INSTAXAMT-3         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-4       PIC X.
           03  QCD-CT-INSTAXAMT-4         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-5       PIC X.
           03  QCD-CT-INSTAXAMT-5         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-6       PIC X.
           03  QCD-CT-INSTAXAMT-6         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-7       PIC X.
           03  QCD-CT-INSTAXAMT-7         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-8       PIC X.
           03  QCD-CT-INSTAXAMT-8         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-9       PIC X.
           03  QCD-CT-INSTAXAMT-9         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-10      PIC X.
           03  QCD-CT-INSTAXAMT-10        PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-11      PIC X.
           03  QCD-CT-INSTAXAMT-11        PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-12      PIC X.
           03  QCD-CT-INSTAXAMT-12        PIC 99V99.
           03  QCD-CT-BOTH-TAX-FG         PIC X.
           03  QCD-CY-INSTAXFRMLA-1       PIC X.
           03  QCD-CY-INSTAXAMT-1         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-2       PIC X.
           03  QCD-CY-INSTAXAMT-2         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-3       PIC X.
           03  QCD-CY-INSTAXAMT-3         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-4       PIC X.
           03  QCD-CY-INSTAXAMT-4         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-5       PIC X.
           03  QCD-CY-INSTAXAMT-5         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-6       PIC X.
           03  QCD-CY-INSTAXAMT-6         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-7       PIC X.
           03  QCD-CY-INSTAXAMT-7         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-8       PIC X.
           03  QCD-CY-INSTAXAMT-8         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-9       PIC X.
           03  QCD-CY-INSTAXAMT-9         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-10      PIC X.
           03  QCD-CY-INSTAXAMT-10        PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-11      PIC X.
           03  QCD-CY-INSTAXAMT-11        PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-12      PIC X.
           03  QCD-CY-INSTAXAMT-12        PIC 99V99.
           03  QCD-CY-BOTH-TAX-FG         PIC X.
           03  QCD-EA-CQ-FG               PIC X(1).
           03  QCD-EA-GQ-FG               PIC X(1).
           03  QCD-ELGRACE                PIC 9(2).
           03  QCD-ELGRACECD              PIC X(1).
           03  QCD-ELTYPE                 PIC X(2).
           03  QCD-ELRATE                 PIC 9(4)V9(4).
           03  QCD-ELFRMLA                PIC X(2).
           03  QCD-ELMINCHG               PIC 9(2)V99.
           03  QCD-ELMAXAMT               PIC 9(4)V99.
           03  QCD-ELDELFAC               PIC 9(3).
           03  QCD-ELYRTYPE               PIC 9(4).
           03  QCD-ELUNITPER-CD           PIC X(1).
           03  QCD-PO-TITLE-HOLD-DAYS     PIC 9(02).
           03  QCD-RE-ACTIONCD            PIC X(04).
           03  QCD-RE-MEMO-REQUIRED       PIC X(01).
           03  QCD-RE-SUPERVISOR-ONLY     PIC X(01).
           03  QCD-RE-ACTIVITY-REQUIRED   PIC X(01).
           03  QCD-RE-RESCHEDULE-REQ      PIC X(01).
           03  QCD-RE-RESCHEDULE-TIME     PIC 9(03).
           03  QCD-RE-WORKED-ACCOUNT      PIC X(01).
           03  QCD-FC-FEEAMT              PIC S9(7)V99.
           03  QCD-TC-REFNO-1             PIC X(05).
           03  QCD-TC-REFNO-2             PIC X(05).
           03  QCD-TC-REFNO-3             PIC X(05).
           03  QCD-TC-REFNO-4             PIC X(05).
           03  QCD-TC-REFNO-5             PIC X(05).
           03  QCD-TC-REFNO-6             PIC X(05).
           03  QCD-TC-REFNO-7             PIC X(05).
           03  QCD-TC-REFNO-8             PIC X(05).
           03  QCD-TC-REFNO-9             PIC X(05).
           03  QCD-TC-REFNO-10            PIC X(05).
           03  QCD-PS-TDR-FLAG            PIC X(01).

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

       01  LEGEND-BEG-TYPE. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING TYPE       (F3-ALL) ".

       01  LEGEND-END-TYPE. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING TYPE       ".

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
           03  BEG-TYPE               PIC X(02).
           03  END-TYPE               PIC X(02).
           03  BEG-CODE               PIC X(08).
           03  END-CODE               PIC X(08).

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
           03  FILLER PIC X(39) VALUE "TYPE   CODE         DESC".
           03  FILLER PIC X(30) VALUE "LTOUCH".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CD-TYPE                    PIC XX.
           03  FILLER                         PIC X(05).
           03  D01-CD-CODE                    PIC X(8).
           03  FILLER                         PIC X(05).
           03  D01-CD-DESC                    PIC X(15).
           03  FILLER                         PIC X(05).
           03  D01-CD-LTOUCH                  PIC 9999/99/99.

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
       COPY "LIBIO/IOCDIN_DEF.CPY".
       COPY "LIBIO/IOCDIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCDIN_SCN.CPY".

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
           MOVE "B"                  TO CD-PATH-OVERRIDE.

           MOVE BEG-TYPE            TO QCD1-WBEG-TYPE.
           MOVE BEG-CODE            TO QCD1-WBEG-CODE.

           MOVE END-TYPE            TO QCD1-WEND-TYPE.
           MOVE END-CODE            TO QCD1-WEND-CODE.

           PERFORM OPEN-CD1-FILE.
           PERFORM START-CD1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-CD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( CD-TYPE < BEG-TYPE )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( CD-TYPE > END-TYPE )
              GO TO MAIN-PROGRAM-END.

           IF CD-CODE < BEG-CODE OR > END-CODE
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CD-TYPE                    TO D01-CD-TYPE.
           MOVE CD-CODE                    TO D01-CD-CODE.
           MOVE CD-DESC                    TO D01-CD-DESC.
           MOVE CD-LTOUCH-DATE             TO D01-CD-LTOUCH.
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
           MOVE "IOCDIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCDIN-QUEUE-POS  TO Q-POS.
           MOVE IOCDIN-COPIES-POS TO C-POS.
           MOVE IOCDIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-TYPE.

           MOVE LEGEND-BEG-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TYPE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-TYPE.

      *-----------------------------------------------------------------
       ENTER-END-TYPE.

           MOVE LEGEND-END-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020END-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-TYPE.

           IF ( BEG-TYPE > END-TYPE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TYPE.

           GO TO ENTER-BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-ALL-TYPE.

           MOVE "S  A020BEG-TYPE." TO VDU.
           MOVE SPACES               TO BEG-TYPE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-TYPE." TO VDU.
           MOVE ALL "Z"              TO END-TYPE VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CODE.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A080BEG-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-TYPE
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-END-CODE.

           MOVE "E  A080END-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-CODE.

           IF ( BEG-CODE > END-CODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CODE.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-CODE.

           MOVE "S  A080BEG-CODE." TO VDU.
           MOVE SPACES               TO BEG-CODE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A080END-CODE." TO VDU.
           MOVE ALL "Z"              TO END-CODE VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CODE.
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

           MOVE "S  A000BEG-TYPE."    TO VDU.
           MOVE BEG-TYPE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-TYPE."    TO VDU.
           MOVE END-TYPE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-CODE."    TO VDU.
           MOVE BEG-CODE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-CODE."    TO VDU.
           MOVE END-CODE              TO VDUBUF.
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

           MOVE CD1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING GROUP               : "    TO R01-PROMPT.
           MOVE BEG-TYPE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    GROUP               : "    TO R01-PROMPT.
           MOVE END-TYPE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CODE               : "    TO R01-PROMPT.
           MOVE BEG-CODE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CODE               : "    TO R01-PROMPT.
           MOVE END-CODE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

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
           MOVE "SQL TABLE IO UTILITY - CDFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCDIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCDIN-SYSDATE.
           DISPLAY IOCDIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCDIN-FORM-FIELDS TO WR-BUF.
           MOVE IOCDIN-FORM-LIST TO WR-LIST.
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

           MOVE IOCDIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCDIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCDIN_EVA.CPY".
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
      *    LOAD CD-REC FROM QCD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CD-FIELDS.
           MOVE QCD-TYPE TO CD-TYPE.
           MOVE QCD-CODE TO CD-CODE.

           IF ( QCD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CD-LTOUCH-DATE
           ELSE
              MOVE QCD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CD-LTOUCH-DATE.

           MOVE QCD-DESC TO CD-DESC.
           MOVE QCD-AT-ACTIONCD TO CD-AT-ACTIONCD.
           MOVE QCD-AT-MEMO-REQUIRED TO CD-AT-MEMO-REQUIRED.
           MOVE QCD-AT-SUPERVISOR-ONLY TO CD-AT-SUPERVISOR-ONLY.
           MOVE QCD-AT-RESULT-REQUIRED TO CD-AT-RESULT-REQUIRED.
           MOVE QCD-BR-FILE-TYPE TO CD-BR-FILE-TYPE.
           MOVE QCD-BR-FILE-NAME TO CD-BR-FILE-NAME.
           MOVE QCD-BR-PAYOFF-FG TO CD-BR-PAYOFF-FG.
           MOVE QCD-BR-POST-SS-TRCD TO CD-BR-POST-SS-TRCD.
           MOVE QCD-BR-GL-BRANCH TO CD-BR-GL-BRANCH.
           MOVE QCD-BR-GL-ACCT TO CD-BR-GL-ACCT.
           MOVE QCD-BR-ALLOT-OPTION TO CD-BR-ALLOT-OPTION.
           MOVE QCD-BR-LBOX-OPTION TO CD-BR-LBOX-OPTION.
           MOVE QCD-BR-LC-FG TO CD-BR-LC-FG.
           MOVE QCD-BR-OT-GL-BRANCH TO CD-BR-OT-GL-BRANCH.
           MOVE QCD-BR-OT-GL-ACCT TO CD-BR-OT-GL-ACCT.
           MOVE QCD-BR-MULTIPLE-REFCDS TO CD-BR-MULTIPLE-REFCDS.
           MOVE QCD-BR-COLLECT-LC TO CD-BR-COLLECT-LC.
           MOVE QCD-CO-GROUP TO CD-CO-GROUP.
           MOVE QCD-CO-SECURED TO CD-CO-SECURED.
           MOVE QCD-CO-TRW TO CD-CO-TRW.
           MOVE QCD-CT-INSTAXFRMLA-1 TO CD-CT-INSTAXFRMLA(1).
           MOVE QCD-CT-INSTAXAMT-1 TO CD-CT-INSTAXAMT(1).
           MOVE QCD-CT-INSTAXFRMLA-2 TO CD-CT-INSTAXFRMLA(2).
           MOVE QCD-CT-INSTAXAMT-2 TO CD-CT-INSTAXAMT(2).
           MOVE QCD-CT-INSTAXFRMLA-3 TO CD-CT-INSTAXFRMLA(3).
           MOVE QCD-CT-INSTAXAMT-3 TO CD-CT-INSTAXAMT(3).
           MOVE QCD-CT-INSTAXFRMLA-4 TO CD-CT-INSTAXFRMLA(4).
           MOVE QCD-CT-INSTAXAMT-4 TO CD-CT-INSTAXAMT(4).
           MOVE QCD-CT-INSTAXFRMLA-5 TO CD-CT-INSTAXFRMLA(5).
           MOVE QCD-CT-INSTAXAMT-5 TO CD-CT-INSTAXAMT(5).
           MOVE QCD-CT-INSTAXFRMLA-6 TO CD-CT-INSTAXFRMLA(6).
           MOVE QCD-CT-INSTAXAMT-6 TO CD-CT-INSTAXAMT(6).
           MOVE QCD-CT-INSTAXFRMLA-7 TO CD-CT-INSTAXFRMLA(7).
           MOVE QCD-CT-INSTAXAMT-7 TO CD-CT-INSTAXAMT(7).
           MOVE QCD-CT-INSTAXFRMLA-8 TO CD-CT-INSTAXFRMLA(8).
           MOVE QCD-CT-INSTAXAMT-8 TO CD-CT-INSTAXAMT(8).
           MOVE QCD-CT-INSTAXFRMLA-9 TO CD-CT-INSTAXFRMLA(9).
           MOVE QCD-CT-INSTAXAMT-9 TO CD-CT-INSTAXAMT(9).
           MOVE QCD-CT-INSTAXFRMLA-10 TO CD-CT-INSTAXFRMLA(10).
           MOVE QCD-CT-INSTAXAMT-10 TO CD-CT-INSTAXAMT(10).
           MOVE QCD-CT-INSTAXFRMLA-11 TO CD-CT-INSTAXFRMLA(11).
           MOVE QCD-CT-INSTAXAMT-11 TO CD-CT-INSTAXAMT(11).
           MOVE QCD-CT-INSTAXFRMLA-12 TO CD-CT-INSTAXFRMLA(12).
           MOVE QCD-CT-INSTAXAMT-12 TO CD-CT-INSTAXAMT(12).
           MOVE QCD-CT-BOTH-TAX-FG TO CD-CT-BOTH-TAX-FG.
           MOVE QCD-CY-INSTAXFRMLA-1 TO CD-CY-INSTAXFRMLA(1).
           MOVE QCD-CY-INSTAXAMT-1 TO CD-CY-INSTAXAMT(1).
           MOVE QCD-CY-INSTAXFRMLA-2 TO CD-CY-INSTAXFRMLA(2).
           MOVE QCD-CY-INSTAXAMT-2 TO CD-CY-INSTAXAMT(2).
           MOVE QCD-CY-INSTAXFRMLA-3 TO CD-CY-INSTAXFRMLA(3).
           MOVE QCD-CY-INSTAXAMT-3 TO CD-CY-INSTAXAMT(3).
           MOVE QCD-CY-INSTAXFRMLA-4 TO CD-CY-INSTAXFRMLA(4).
           MOVE QCD-CY-INSTAXAMT-4 TO CD-CY-INSTAXAMT(4).
           MOVE QCD-CY-INSTAXFRMLA-5 TO CD-CY-INSTAXFRMLA(5).
           MOVE QCD-CY-INSTAXAMT-5 TO CD-CY-INSTAXAMT(5).
           MOVE QCD-CY-INSTAXFRMLA-6 TO CD-CY-INSTAXFRMLA(6).
           MOVE QCD-CY-INSTAXAMT-6 TO CD-CY-INSTAXAMT(6).
           MOVE QCD-CY-INSTAXFRMLA-7 TO CD-CY-INSTAXFRMLA(7).
           MOVE QCD-CY-INSTAXAMT-7 TO CD-CY-INSTAXAMT(7).
           MOVE QCD-CY-INSTAXFRMLA-8 TO CD-CY-INSTAXFRMLA(8).
           MOVE QCD-CY-INSTAXAMT-8 TO CD-CY-INSTAXAMT(8).
           MOVE QCD-CY-INSTAXFRMLA-9 TO CD-CY-INSTAXFRMLA(9).
           MOVE QCD-CY-INSTAXAMT-9 TO CD-CY-INSTAXAMT(9).
           MOVE QCD-CY-INSTAXFRMLA-10 TO CD-CY-INSTAXFRMLA(10).
           MOVE QCD-CY-INSTAXAMT-10 TO CD-CY-INSTAXAMT(10).
           MOVE QCD-CY-INSTAXFRMLA-11 TO CD-CY-INSTAXFRMLA(11).
           MOVE QCD-CY-INSTAXAMT-11 TO CD-CY-INSTAXAMT(11).
           MOVE QCD-CY-INSTAXFRMLA-12 TO CD-CY-INSTAXFRMLA(12).
           MOVE QCD-CY-INSTAXAMT-12 TO CD-CY-INSTAXAMT(12).
           MOVE QCD-CY-BOTH-TAX-FG TO CD-CY-BOTH-TAX-FG.
           MOVE QCD-EA-CQ-FG TO CD-EA-CQ-FG.
           MOVE QCD-EA-GQ-FG TO CD-EA-GQ-FG.
           MOVE QCD-ELGRACE TO CD-ELGRACE.
           MOVE QCD-ELGRACECD TO CD-ELGRACECD.
           MOVE QCD-ELTYPE TO CD-ELTYPE.
           MOVE QCD-ELRATE TO CD-ELRATE.
           MOVE QCD-ELFRMLA TO CD-ELFRMLA.
           MOVE QCD-ELMINCHG TO CD-ELMINCHG.
           MOVE QCD-ELMAXAMT TO CD-ELMAXAMT.
           MOVE QCD-ELDELFAC TO CD-ELDELFAC.
           MOVE QCD-ELYRTYPE TO CD-ELYRTYPE.
           MOVE QCD-ELUNITPER-CD TO CD-ELUNITPER-CD.
           MOVE QCD-PO-TITLE-HOLD-DAYS TO CD-PO-TITLE-HOLD-DAYS.
           MOVE QCD-RE-ACTIONCD TO CD-RE-ACTIONCD.
           MOVE QCD-RE-MEMO-REQUIRED TO CD-RE-MEMO-REQUIRED.
           MOVE QCD-RE-SUPERVISOR-ONLY TO CD-RE-SUPERVISOR-ONLY.
           MOVE QCD-RE-ACTIVITY-REQUIRED TO CD-RE-ACTIVITY-REQUIRED.
           MOVE QCD-RE-RESCHEDULE-REQ TO CD-RE-RESCHEDULE-REQ.
           MOVE QCD-RE-RESCHEDULE-TIME TO CD-RE-RESCHEDULE-TIME.
           MOVE QCD-RE-WORKED-ACCOUNT TO CD-RE-WORKED-ACCOUNT.
           MOVE QCD-FC-FEEAMT TO CD-FC-FEEAMT.
           MOVE QCD-TC-REFNO-1 TO CD-TC-REFNO(1).
           MOVE QCD-TC-REFNO-2 TO CD-TC-REFNO(2).
           MOVE QCD-TC-REFNO-3 TO CD-TC-REFNO(3).
           MOVE QCD-TC-REFNO-4 TO CD-TC-REFNO(4).
           MOVE QCD-TC-REFNO-5 TO CD-TC-REFNO(5).
           MOVE QCD-TC-REFNO-6 TO CD-TC-REFNO(6).
           MOVE QCD-TC-REFNO-7 TO CD-TC-REFNO(7).
           MOVE QCD-TC-REFNO-8 TO CD-TC-REFNO(8).
           MOVE QCD-TC-REFNO-9 TO CD-TC-REFNO(9).
           MOVE QCD-TC-REFNO-10 TO CD-TC-REFNO(10).
           MOVE QCD-PS-TDR-FLAG TO CD-PS-TDR-FLAG.
 
      *-----------------------------------------------------------------
      *    LOAD QCD-REC FROM CD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CD-FIELDS.
           MOVE CD-TYPE TO QCD-TYPE.
           MOVE CD-CODE TO QCD-CODE.

           IF ( CD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCD-LTOUCH-DATE
           ELSE
              MOVE CD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCD-LTOUCH-DATE.

           MOVE CD-DESC TO QCD-DESC.
           MOVE CD-AT-ACTIONCD TO QCD-AT-ACTIONCD.
           MOVE CD-AT-MEMO-REQUIRED TO QCD-AT-MEMO-REQUIRED.
           MOVE CD-AT-SUPERVISOR-ONLY TO QCD-AT-SUPERVISOR-ONLY.
           MOVE CD-AT-RESULT-REQUIRED TO QCD-AT-RESULT-REQUIRED.
           MOVE CD-BR-FILE-TYPE TO QCD-BR-FILE-TYPE.
           MOVE CD-BR-FILE-NAME TO QCD-BR-FILE-NAME.
           MOVE CD-BR-PAYOFF-FG TO QCD-BR-PAYOFF-FG.
           MOVE CD-BR-POST-SS-TRCD TO QCD-BR-POST-SS-TRCD.
           MOVE CD-BR-GL-BRANCH TO QCD-BR-GL-BRANCH.
           MOVE CD-BR-GL-ACCT TO QCD-BR-GL-ACCT.
           MOVE CD-BR-ALLOT-OPTION TO QCD-BR-ALLOT-OPTION.
           MOVE CD-BR-LBOX-OPTION TO QCD-BR-LBOX-OPTION.
           MOVE CD-BR-LC-FG TO QCD-BR-LC-FG.
           MOVE CD-BR-OT-GL-BRANCH TO QCD-BR-OT-GL-BRANCH.
           MOVE CD-BR-OT-GL-ACCT TO QCD-BR-OT-GL-ACCT.
           MOVE CD-BR-MULTIPLE-REFCDS TO QCD-BR-MULTIPLE-REFCDS.
           MOVE CD-BR-COLLECT-LC TO QCD-BR-COLLECT-LC.
           MOVE CD-CO-GROUP TO QCD-CO-GROUP.
           MOVE CD-CO-SECURED TO QCD-CO-SECURED.
           MOVE CD-CO-TRW TO QCD-CO-TRW.
           MOVE CD-CT-INSTAXFRMLA(1) TO QCD-CT-INSTAXFRMLA-1.
           MOVE CD-CT-INSTAXAMT(1) TO QCD-CT-INSTAXAMT-1.
           MOVE CD-CT-INSTAXFRMLA(2) TO QCD-CT-INSTAXFRMLA-2.
           MOVE CD-CT-INSTAXAMT(2) TO QCD-CT-INSTAXAMT-2.
           MOVE CD-CT-INSTAXFRMLA(3) TO QCD-CT-INSTAXFRMLA-3.
           MOVE CD-CT-INSTAXAMT(3) TO QCD-CT-INSTAXAMT-3.
           MOVE CD-CT-INSTAXFRMLA(4) TO QCD-CT-INSTAXFRMLA-4.
           MOVE CD-CT-INSTAXAMT(4) TO QCD-CT-INSTAXAMT-4.
           MOVE CD-CT-INSTAXFRMLA(5) TO QCD-CT-INSTAXFRMLA-5.
           MOVE CD-CT-INSTAXAMT(5) TO QCD-CT-INSTAXAMT-5.
           MOVE CD-CT-INSTAXFRMLA(6) TO QCD-CT-INSTAXFRMLA-6.
           MOVE CD-CT-INSTAXAMT(6) TO QCD-CT-INSTAXAMT-6.
           MOVE CD-CT-INSTAXFRMLA(7) TO QCD-CT-INSTAXFRMLA-7.
           MOVE CD-CT-INSTAXAMT(7) TO QCD-CT-INSTAXAMT-7.
           MOVE CD-CT-INSTAXFRMLA(8) TO QCD-CT-INSTAXFRMLA-8.
           MOVE CD-CT-INSTAXAMT(8) TO QCD-CT-INSTAXAMT-8.
           MOVE CD-CT-INSTAXFRMLA(9) TO QCD-CT-INSTAXFRMLA-9.
           MOVE CD-CT-INSTAXAMT(9) TO QCD-CT-INSTAXAMT-9.
           MOVE CD-CT-INSTAXFRMLA(10) TO QCD-CT-INSTAXFRMLA-10.
           MOVE CD-CT-INSTAXAMT(10) TO QCD-CT-INSTAXAMT-10.
           MOVE CD-CT-INSTAXFRMLA(11) TO QCD-CT-INSTAXFRMLA-11.
           MOVE CD-CT-INSTAXAMT(11) TO QCD-CT-INSTAXAMT-11.
           MOVE CD-CT-INSTAXFRMLA(12) TO QCD-CT-INSTAXFRMLA-12.
           MOVE CD-CT-INSTAXAMT(12) TO QCD-CT-INSTAXAMT-12.
           MOVE CD-CT-BOTH-TAX-FG TO QCD-CT-BOTH-TAX-FG.
           MOVE CD-CY-INSTAXFRMLA(1) TO QCD-CY-INSTAXFRMLA-1.
           MOVE CD-CY-INSTAXAMT(1) TO QCD-CY-INSTAXAMT-1.
           MOVE CD-CY-INSTAXFRMLA(2) TO QCD-CY-INSTAXFRMLA-2.
           MOVE CD-CY-INSTAXAMT(2) TO QCD-CY-INSTAXAMT-2.
           MOVE CD-CY-INSTAXFRMLA(3) TO QCD-CY-INSTAXFRMLA-3.
           MOVE CD-CY-INSTAXAMT(3) TO QCD-CY-INSTAXAMT-3.
           MOVE CD-CY-INSTAXFRMLA(4) TO QCD-CY-INSTAXFRMLA-4.
           MOVE CD-CY-INSTAXAMT(4) TO QCD-CY-INSTAXAMT-4.
           MOVE CD-CY-INSTAXFRMLA(5) TO QCD-CY-INSTAXFRMLA-5.
           MOVE CD-CY-INSTAXAMT(5) TO QCD-CY-INSTAXAMT-5.
           MOVE CD-CY-INSTAXFRMLA(6) TO QCD-CY-INSTAXFRMLA-6.
           MOVE CD-CY-INSTAXAMT(6) TO QCD-CY-INSTAXAMT-6.
           MOVE CD-CY-INSTAXFRMLA(7) TO QCD-CY-INSTAXFRMLA-7.
           MOVE CD-CY-INSTAXAMT(7) TO QCD-CY-INSTAXAMT-7.
           MOVE CD-CY-INSTAXFRMLA(8) TO QCD-CY-INSTAXFRMLA-8.
           MOVE CD-CY-INSTAXAMT(8) TO QCD-CY-INSTAXAMT-8.
           MOVE CD-CY-INSTAXFRMLA(9) TO QCD-CY-INSTAXFRMLA-9.
           MOVE CD-CY-INSTAXAMT(9) TO QCD-CY-INSTAXAMT-9.
           MOVE CD-CY-INSTAXFRMLA(10) TO QCD-CY-INSTAXFRMLA-10.
           MOVE CD-CY-INSTAXAMT(10) TO QCD-CY-INSTAXAMT-10.
           MOVE CD-CY-INSTAXFRMLA(11) TO QCD-CY-INSTAXFRMLA-11.
           MOVE CD-CY-INSTAXAMT(11) TO QCD-CY-INSTAXAMT-11.
           MOVE CD-CY-INSTAXFRMLA(12) TO QCD-CY-INSTAXFRMLA-12.
           MOVE CD-CY-INSTAXAMT(12) TO QCD-CY-INSTAXAMT-12.
           MOVE CD-CY-BOTH-TAX-FG TO QCD-CY-BOTH-TAX-FG.
           MOVE CD-EA-CQ-FG TO QCD-EA-CQ-FG.
           MOVE CD-EA-GQ-FG TO QCD-EA-GQ-FG.
           MOVE CD-ELGRACE TO QCD-ELGRACE.
           MOVE CD-ELGRACECD TO QCD-ELGRACECD.
           MOVE CD-ELTYPE TO QCD-ELTYPE.
           MOVE CD-ELRATE TO QCD-ELRATE.
           MOVE CD-ELFRMLA TO QCD-ELFRMLA.
           MOVE CD-ELMINCHG TO QCD-ELMINCHG.
           MOVE CD-ELMAXAMT TO QCD-ELMAXAMT.
           MOVE CD-ELDELFAC TO QCD-ELDELFAC.
           MOVE CD-ELYRTYPE TO QCD-ELYRTYPE.
           MOVE CD-ELUNITPER-CD TO QCD-ELUNITPER-CD.
           MOVE CD-PO-TITLE-HOLD-DAYS TO QCD-PO-TITLE-HOLD-DAYS.
           MOVE CD-RE-ACTIONCD TO QCD-RE-ACTIONCD.
           MOVE CD-RE-MEMO-REQUIRED TO QCD-RE-MEMO-REQUIRED.
           MOVE CD-RE-SUPERVISOR-ONLY TO QCD-RE-SUPERVISOR-ONLY.
           MOVE CD-RE-ACTIVITY-REQUIRED TO QCD-RE-ACTIVITY-REQUIRED.
           MOVE CD-RE-RESCHEDULE-REQ TO QCD-RE-RESCHEDULE-REQ.
           MOVE CD-RE-RESCHEDULE-TIME TO QCD-RE-RESCHEDULE-TIME.
           MOVE CD-RE-WORKED-ACCOUNT TO QCD-RE-WORKED-ACCOUNT.
           MOVE CD-FC-FEEAMT TO QCD-FC-FEEAMT.
           MOVE CD-TC-REFNO(1) TO QCD-TC-REFNO-1.
           MOVE CD-TC-REFNO(2) TO QCD-TC-REFNO-2.
           MOVE CD-TC-REFNO(3) TO QCD-TC-REFNO-3.
           MOVE CD-TC-REFNO(4) TO QCD-TC-REFNO-4.
           MOVE CD-TC-REFNO(5) TO QCD-TC-REFNO-5.
           MOVE CD-TC-REFNO(6) TO QCD-TC-REFNO-6.
           MOVE CD-TC-REFNO(7) TO QCD-TC-REFNO-7.
           MOVE CD-TC-REFNO(8) TO QCD-TC-REFNO-8.
           MOVE CD-TC-REFNO(9) TO QCD-TC-REFNO-9.
           MOVE CD-TC-REFNO(10) TO QCD-TC-REFNO-10.
           MOVE CD-PS-TDR-FLAG TO QCD-PS-TDR-FLAG.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBCD1RN
       LOAD-CD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CD-ALL-BASE
              MOVE FILPATH-MACHINE         TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CDFILE NOT = "S" )
              MOVE CD-PATH-BEG             TO CD-SELECT-PATH-BEG
              MOVE CD-PATH-FILE            TO CD-SELECT-PATH-FILE
           ELSE
              MOVE CD-PATH-SQL             TO CD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CD1-FILE.
           PERFORM LOAD-CD1-FILE.
           PERFORM OPEN-IT.
           MOVE CD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CD1-FILE.
           PERFORM START-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.

           IF ( CD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CD1-FILE.

           IF ( QCD1-WEND-TYPE = SPACES    ) OR
              ( QCD1-WEND-CODE = SPACES    )
              MOVE CD-TYPE              TO QCD1-WBEG-TYPE
              MOVE ALL "Z"              TO QCD1-WEND-TYPE
              MOVE CD-CODE              TO QCD1-WBEG-CODE
              MOVE ALL "Z"              TO QCD1-WEND-CODE.

           EXEC SQL
            DECLARE CD1_CURSOR CURSOR FOR
            SELECT ISNULL(CDFILE.CD_TYPE,''),
                   ISNULL(CDFILE.CD_CODE,''),
                   ISNULL(CAST(CDFILE.CD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDFILE.CD_DESC,''),
                   ISNULL(CDFILE.CD_AT_ACTIONCD,''),
                   ISNULL(CDFILE.CD_AT_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_AT_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_AT_RESULT_REQUIRED,''),
                   ISNULL(CDFILE.CD_BR_FILE_TYPE,''),
                   ISNULL(CDFILE.CD_BR_FILE_NAME,''),
                   ISNULL(CDFILE.CD_BR_PAYOFF_FG,''),
                   ISNULL(CDFILE.CD_BR_POST_SS_TRCD,''),
                   CDFILE.CD_BR_GL_BRANCH,
                   CDFILE.CD_BR_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_ALLOT_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LBOX_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LC_FG,''),
                   CDFILE.CD_BR_OT_GL_BRANCH,
                   CDFILE.CD_BR_OT_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_MULTIPLE_REFCDS,''),
                   ISNULL(CDFILE.CD_BR_COLLECT_LC,''),
                   ISNULL(CDFILE.CD_CO_GROUP,''),
                   ISNULL(CDFILE.CD_CO_SECURED,''),
                   ISNULL(CDFILE.CD_CO_TRW,''),
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_1,''),
                   CDFILE.CD_CT_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_2,''),
                   CDFILE.CD_CT_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_3,''),
                   CDFILE.CD_CT_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_4,''),
                   CDFILE.CD_CT_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_5,''),
                   CDFILE.CD_CT_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_6,''),
                   CDFILE.CD_CT_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_7,''),
                   CDFILE.CD_CT_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_8,''),
                   CDFILE.CD_CT_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_9,''),
                   CDFILE.CD_CT_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_10,''),
                   CDFILE.CD_CT_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_11,''),
                   CDFILE.CD_CT_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_12,''),
                   CDFILE.CD_CT_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CT_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_1,''),
                   CDFILE.CD_CY_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_2,''),
                   CDFILE.CD_CY_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_3,''),
                   CDFILE.CD_CY_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_4,''),
                   CDFILE.CD_CY_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_5,''),
                   CDFILE.CD_CY_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_6,''),
                   CDFILE.CD_CY_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_7,''),
                   CDFILE.CD_CY_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_8,''),
                   CDFILE.CD_CY_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_9,''),
                   CDFILE.CD_CY_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_10,''),
                   CDFILE.CD_CY_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_11,''),
                   CDFILE.CD_CY_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_12,''),
                   CDFILE.CD_CY_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CY_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_EA_CQ_FG,''),
                   ISNULL(CDFILE.CD_EA_GQ_FG,''),
                   CDFILE.CD_ELGRACE,
                   ISNULL(CDFILE.CD_ELGRACECD,''),
                   ISNULL(CDFILE.CD_ELTYPE,''),
                   CDFILE.CD_ELRATE,
                   ISNULL(CDFILE.CD_ELFRMLA,''),
                   CDFILE.CD_ELMINCHG,
                   CDFILE.CD_ELMAXAMT,
                   CDFILE.CD_ELDELFAC,
                   CDFILE.CD_ELYRTYPE,
                   ISNULL(CDFILE.CD_ELUNITPER_CD,''),
                   CDFILE.CD_PO_TITLE_HOLD_DAYS,
                   ISNULL(CDFILE.CD_RE_ACTIONCD,''),
                   ISNULL(CDFILE.CD_RE_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_RE_ACTIVITY_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_RESCHEDULE_REQ,''),
                   CDFILE.CD_RE_RESCHEDULE_TIME,
                   ISNULL(CDFILE.CD_RE_WORKED_ACCOUNT,''),
                   CDFILE.CD_FC_FEEAMT,
                   ISNULL(CDFILE.CD_TC_REFNO_1,''),
                   ISNULL(CDFILE.CD_TC_REFNO_2,''),
                   ISNULL(CDFILE.CD_TC_REFNO_3,''),
                   ISNULL(CDFILE.CD_TC_REFNO_4,''),
                   ISNULL(CDFILE.CD_TC_REFNO_5,''),
                   ISNULL(CDFILE.CD_TC_REFNO_6,''),
                   ISNULL(CDFILE.CD_TC_REFNO_7,''),
                   ISNULL(CDFILE.CD_TC_REFNO_8,''),
                   ISNULL(CDFILE.CD_TC_REFNO_9,''),
                   ISNULL(CDFILE.CD_TC_REFNO_10,''),
                   ISNULL(CDFILE.CD_PS_TDR_FLAG,'')
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE >= :QCD1-WBEG-TYPE
              AND CDFILE.CD_TYPE <= :QCD1-WEND-TYPE
              AND CDFILE.CD_CODE >= :QCD1-WBEG-CODE
              AND CDFILE.CD_CODE <= :QCD1-WEND-CODE
              ORDER BY CDFILE.CD_TYPE,
                       CDFILE.CD_CODE
           END-EXEC.

           EXEC SQL
               OPEN CD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCD1-WBEG-TYPE
                          QCD1-WEND-TYPE
                          QCD1-WBEG-CODE
                          QCD1-WEND-CODE.

           MOVE STAT---GOOD TO CD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CD1-FILE.
           PERFORM READ-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.

           MOVE CD-TYPE    TO QCD-TYPE.
           MOVE CD-CODE    TO QCD-CODE.

           EXEC SQL
            SELECT ISNULL(CDFILE.CD_TYPE,''),
                   ISNULL(CDFILE.CD_CODE,''),
                   ISNULL(CAST(CDFILE.CD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDFILE.CD_DESC,''),
                   ISNULL(CDFILE.CD_AT_ACTIONCD,''),
                   ISNULL(CDFILE.CD_AT_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_AT_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_AT_RESULT_REQUIRED,''),
                   ISNULL(CDFILE.CD_BR_FILE_TYPE,''),
                   ISNULL(CDFILE.CD_BR_FILE_NAME,''),
                   ISNULL(CDFILE.CD_BR_PAYOFF_FG,''),
                   ISNULL(CDFILE.CD_BR_POST_SS_TRCD,''),
                   CDFILE.CD_BR_GL_BRANCH,
                   CDFILE.CD_BR_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_ALLOT_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LBOX_OPTION,''),
                   ISNULL(CDFILE.CD_BR_LC_FG,''),
                   CDFILE.CD_BR_OT_GL_BRANCH,
                   CDFILE.CD_BR_OT_GL_ACCT,
                   ISNULL(CDFILE.CD_BR_MULTIPLE_REFCDS,''),
                   ISNULL(CDFILE.CD_BR_COLLECT_LC,''),
                   ISNULL(CDFILE.CD_CO_GROUP,''),
                   ISNULL(CDFILE.CD_CO_SECURED,''),
                   ISNULL(CDFILE.CD_CO_TRW,''),
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_1,''),
                   CDFILE.CD_CT_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_2,''),
                   CDFILE.CD_CT_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_3,''),
                   CDFILE.CD_CT_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_4,''),
                   CDFILE.CD_CT_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_5,''),
                   CDFILE.CD_CT_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_6,''),
                   CDFILE.CD_CT_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_7,''),
                   CDFILE.CD_CT_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_8,''),
                   CDFILE.CD_CT_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_9,''),
                   CDFILE.CD_CT_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_10,''),
                   CDFILE.CD_CT_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_11,''),
                   CDFILE.CD_CT_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CT_INSTAXFRMLA_12,''),
                   CDFILE.CD_CT_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CT_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_1,''),
                   CDFILE.CD_CY_INSTAXAMT_1,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_2,''),
                   CDFILE.CD_CY_INSTAXAMT_2,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_3,''),
                   CDFILE.CD_CY_INSTAXAMT_3,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_4,''),
                   CDFILE.CD_CY_INSTAXAMT_4,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_5,''),
                   CDFILE.CD_CY_INSTAXAMT_5,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_6,''),
                   CDFILE.CD_CY_INSTAXAMT_6,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_7,''),
                   CDFILE.CD_CY_INSTAXAMT_7,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_8,''),
                   CDFILE.CD_CY_INSTAXAMT_8,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_9,''),
                   CDFILE.CD_CY_INSTAXAMT_9,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_10,''),
                   CDFILE.CD_CY_INSTAXAMT_10,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_11,''),
                   CDFILE.CD_CY_INSTAXAMT_11,
                   ISNULL(CDFILE.CD_CY_INSTAXFRMLA_12,''),
                   CDFILE.CD_CY_INSTAXAMT_12,
                   ISNULL(CDFILE.CD_CY_BOTH_TAX_FG,''),
                   ISNULL(CDFILE.CD_EA_CQ_FG,''),
                   ISNULL(CDFILE.CD_EA_GQ_FG,''),
                   CDFILE.CD_ELGRACE,
                   ISNULL(CDFILE.CD_ELGRACECD,''),
                   ISNULL(CDFILE.CD_ELTYPE,''),
                   CDFILE.CD_ELRATE,
                   ISNULL(CDFILE.CD_ELFRMLA,''),
                   CDFILE.CD_ELMINCHG,
                   CDFILE.CD_ELMAXAMT,
                   CDFILE.CD_ELDELFAC,
                   CDFILE.CD_ELYRTYPE,
                   ISNULL(CDFILE.CD_ELUNITPER_CD,''),
                   CDFILE.CD_PO_TITLE_HOLD_DAYS,
                   ISNULL(CDFILE.CD_RE_ACTIONCD,''),
                   ISNULL(CDFILE.CD_RE_MEMO_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_SUPERVISOR_ONLY,''),
                   ISNULL(CDFILE.CD_RE_ACTIVITY_REQUIRED,''),
                   ISNULL(CDFILE.CD_RE_RESCHEDULE_REQ,''),
                   CDFILE.CD_RE_RESCHEDULE_TIME,
                   ISNULL(CDFILE.CD_RE_WORKED_ACCOUNT,''),
                   CDFILE.CD_FC_FEEAMT,
                   ISNULL(CDFILE.CD_TC_REFNO_1,''),
                   ISNULL(CDFILE.CD_TC_REFNO_2,''),
                   ISNULL(CDFILE.CD_TC_REFNO_3,''),
                   ISNULL(CDFILE.CD_TC_REFNO_4,''),
                   ISNULL(CDFILE.CD_TC_REFNO_5,''),
                   ISNULL(CDFILE.CD_TC_REFNO_6,''),
                   ISNULL(CDFILE.CD_TC_REFNO_7,''),
                   ISNULL(CDFILE.CD_TC_REFNO_8,''),
                   ISNULL(CDFILE.CD_TC_REFNO_9,''),
                   ISNULL(CDFILE.CD_TC_REFNO_10,''),
                   ISNULL(CDFILE.CD_PS_TDR_FLAG,'')
            INTO :QCD-REC
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE = :QCD-TYPE
              AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       READ-CD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CD1_CURSOR INTO :QCD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       WRITE-CD1-FILE.
           ACCEPT CD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           PERFORM SET-CD-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CDFILE
             (CDFILE.CD_TYPE,
              CDFILE.CD_CODE,
              CDFILE.CD_LTOUCH_DATE,
              CDFILE.CD_DESC,
              CDFILE.CD_AT_ACTIONCD,
              CDFILE.CD_AT_MEMO_REQUIRED,
              CDFILE.CD_AT_SUPERVISOR_ONLY,
              CDFILE.CD_AT_RESULT_REQUIRED,
              CDFILE.CD_BR_FILE_TYPE,
              CDFILE.CD_BR_FILE_NAME,
              CDFILE.CD_BR_PAYOFF_FG,
              CDFILE.CD_BR_POST_SS_TRCD,
              CDFILE.CD_BR_GL_BRANCH,
              CDFILE.CD_BR_GL_ACCT,
              CDFILE.CD_BR_ALLOT_OPTION,
              CDFILE.CD_BR_LBOX_OPTION,
              CDFILE.CD_BR_LC_FG,
              CDFILE.CD_BR_OT_GL_BRANCH,
              CDFILE.CD_BR_OT_GL_ACCT,
              CDFILE.CD_BR_MULTIPLE_REFCDS,
              CDFILE.CD_BR_COLLECT_LC,
              CDFILE.CD_CO_GROUP,
              CDFILE.CD_CO_SECURED,
              CDFILE.CD_CO_TRW,
              CDFILE.CD_CT_INSTAXFRMLA_1,
              CDFILE.CD_CT_INSTAXAMT_1,
              CDFILE.CD_CT_INSTAXFRMLA_2,
              CDFILE.CD_CT_INSTAXAMT_2,
              CDFILE.CD_CT_INSTAXFRMLA_3,
              CDFILE.CD_CT_INSTAXAMT_3,
              CDFILE.CD_CT_INSTAXFRMLA_4,
              CDFILE.CD_CT_INSTAXAMT_4,
              CDFILE.CD_CT_INSTAXFRMLA_5,
              CDFILE.CD_CT_INSTAXAMT_5,
              CDFILE.CD_CT_INSTAXFRMLA_6,
              CDFILE.CD_CT_INSTAXAMT_6,
              CDFILE.CD_CT_INSTAXFRMLA_7,
              CDFILE.CD_CT_INSTAXAMT_7,
              CDFILE.CD_CT_INSTAXFRMLA_8,
              CDFILE.CD_CT_INSTAXAMT_8,
              CDFILE.CD_CT_INSTAXFRMLA_9,
              CDFILE.CD_CT_INSTAXAMT_9,
              CDFILE.CD_CT_INSTAXFRMLA_10,
              CDFILE.CD_CT_INSTAXAMT_10,
              CDFILE.CD_CT_INSTAXFRMLA_11,
              CDFILE.CD_CT_INSTAXAMT_11,
              CDFILE.CD_CT_INSTAXFRMLA_12,
              CDFILE.CD_CT_INSTAXAMT_12,
              CDFILE.CD_CT_BOTH_TAX_FG,
              CDFILE.CD_CY_INSTAXFRMLA_1,
              CDFILE.CD_CY_INSTAXAMT_1,
              CDFILE.CD_CY_INSTAXFRMLA_2,
              CDFILE.CD_CY_INSTAXAMT_2,
              CDFILE.CD_CY_INSTAXFRMLA_3,
              CDFILE.CD_CY_INSTAXAMT_3,
              CDFILE.CD_CY_INSTAXFRMLA_4,
              CDFILE.CD_CY_INSTAXAMT_4,
              CDFILE.CD_CY_INSTAXFRMLA_5,
              CDFILE.CD_CY_INSTAXAMT_5,
              CDFILE.CD_CY_INSTAXFRMLA_6,
              CDFILE.CD_CY_INSTAXAMT_6,
              CDFILE.CD_CY_INSTAXFRMLA_7,
              CDFILE.CD_CY_INSTAXAMT_7,
              CDFILE.CD_CY_INSTAXFRMLA_8,
              CDFILE.CD_CY_INSTAXAMT_8,
              CDFILE.CD_CY_INSTAXFRMLA_9,
              CDFILE.CD_CY_INSTAXAMT_9,
              CDFILE.CD_CY_INSTAXFRMLA_10,
              CDFILE.CD_CY_INSTAXAMT_10,
              CDFILE.CD_CY_INSTAXFRMLA_11,
              CDFILE.CD_CY_INSTAXAMT_11,
              CDFILE.CD_CY_INSTAXFRMLA_12,
              CDFILE.CD_CY_INSTAXAMT_12,
              CDFILE.CD_CY_BOTH_TAX_FG,
              CDFILE.CD_EA_CQ_FG,
              CDFILE.CD_EA_GQ_FG,
              CDFILE.CD_ELGRACE,
              CDFILE.CD_ELGRACECD,
              CDFILE.CD_ELTYPE,
              CDFILE.CD_ELRATE,
              CDFILE.CD_ELFRMLA,
              CDFILE.CD_ELMINCHG,
              CDFILE.CD_ELMAXAMT,
              CDFILE.CD_ELDELFAC,
              CDFILE.CD_ELYRTYPE,
              CDFILE.CD_ELUNITPER_CD,
              CDFILE.CD_PO_TITLE_HOLD_DAYS,
              CDFILE.CD_RE_ACTIONCD,
              CDFILE.CD_RE_MEMO_REQUIRED,
              CDFILE.CD_RE_SUPERVISOR_ONLY,
              CDFILE.CD_RE_ACTIVITY_REQUIRED,
              CDFILE.CD_RE_RESCHEDULE_REQ,
              CDFILE.CD_RE_RESCHEDULE_TIME,
              CDFILE.CD_RE_WORKED_ACCOUNT,
              CDFILE.CD_FC_FEEAMT,
              CDFILE.CD_TC_REFNO_1,
              CDFILE.CD_TC_REFNO_2,
              CDFILE.CD_TC_REFNO_3,
              CDFILE.CD_TC_REFNO_4,
              CDFILE.CD_TC_REFNO_5,
              CDFILE.CD_TC_REFNO_6,
              CDFILE.CD_TC_REFNO_7,
              CDFILE.CD_TC_REFNO_8,
              CDFILE.CD_TC_REFNO_9,
              CDFILE.CD_TC_REFNO_10,
              CDFILE.CD_PS_TDR_FLAG)
            VALUES
             (:QCD-TYPE,
              :QCD-CODE,
              NULLIF(:QCD-LTOUCH-DATE,'1900-01-01'),
              :QCD-DESC,
              :QCD-AT-ACTIONCD,
              :QCD-AT-MEMO-REQUIRED,
              :QCD-AT-SUPERVISOR-ONLY,
              :QCD-AT-RESULT-REQUIRED,
              :QCD-BR-FILE-TYPE,
              :QCD-BR-FILE-NAME,
              :QCD-BR-PAYOFF-FG,
              :QCD-BR-POST-SS-TRCD,
              :QCD-BR-GL-BRANCH,
              :QCD-BR-GL-ACCT,
              :QCD-BR-ALLOT-OPTION,
              :QCD-BR-LBOX-OPTION,
              :QCD-BR-LC-FG,
              :QCD-BR-OT-GL-BRANCH,
              :QCD-BR-OT-GL-ACCT,
              :QCD-BR-MULTIPLE-REFCDS,
              :QCD-BR-COLLECT-LC,
              :QCD-CO-GROUP,
              :QCD-CO-SECURED,
              :QCD-CO-TRW,
              :QCD-CT-INSTAXFRMLA-1,
              :QCD-CT-INSTAXAMT-1,
              :QCD-CT-INSTAXFRMLA-2,
              :QCD-CT-INSTAXAMT-2,
              :QCD-CT-INSTAXFRMLA-3,
              :QCD-CT-INSTAXAMT-3,
              :QCD-CT-INSTAXFRMLA-4,
              :QCD-CT-INSTAXAMT-4,
              :QCD-CT-INSTAXFRMLA-5,
              :QCD-CT-INSTAXAMT-5,
              :QCD-CT-INSTAXFRMLA-6,
              :QCD-CT-INSTAXAMT-6,
              :QCD-CT-INSTAXFRMLA-7,
              :QCD-CT-INSTAXAMT-7,
              :QCD-CT-INSTAXFRMLA-8,
              :QCD-CT-INSTAXAMT-8,
              :QCD-CT-INSTAXFRMLA-9,
              :QCD-CT-INSTAXAMT-9,
              :QCD-CT-INSTAXFRMLA-10,
              :QCD-CT-INSTAXAMT-10,
              :QCD-CT-INSTAXFRMLA-11,
              :QCD-CT-INSTAXAMT-11,
              :QCD-CT-INSTAXFRMLA-12,
              :QCD-CT-INSTAXAMT-12,
              :QCD-CT-BOTH-TAX-FG,
              :QCD-CY-INSTAXFRMLA-1,
              :QCD-CY-INSTAXAMT-1,
              :QCD-CY-INSTAXFRMLA-2,
              :QCD-CY-INSTAXAMT-2,
              :QCD-CY-INSTAXFRMLA-3,
              :QCD-CY-INSTAXAMT-3,
              :QCD-CY-INSTAXFRMLA-4,
              :QCD-CY-INSTAXAMT-4,
              :QCD-CY-INSTAXFRMLA-5,
              :QCD-CY-INSTAXAMT-5,
              :QCD-CY-INSTAXFRMLA-6,
              :QCD-CY-INSTAXAMT-6,
              :QCD-CY-INSTAXFRMLA-7,
              :QCD-CY-INSTAXAMT-7,
              :QCD-CY-INSTAXFRMLA-8,
              :QCD-CY-INSTAXAMT-8,
              :QCD-CY-INSTAXFRMLA-9,
              :QCD-CY-INSTAXAMT-9,
              :QCD-CY-INSTAXFRMLA-10,
              :QCD-CY-INSTAXAMT-10,
              :QCD-CY-INSTAXFRMLA-11,
              :QCD-CY-INSTAXAMT-11,
              :QCD-CY-INSTAXFRMLA-12,
              :QCD-CY-INSTAXAMT-12,
              :QCD-CY-BOTH-TAX-FG,
              :QCD-EA-CQ-FG,
              :QCD-EA-GQ-FG,
              :QCD-ELGRACE,
              :QCD-ELGRACECD,
              :QCD-ELTYPE,
              :QCD-ELRATE,
              :QCD-ELFRMLA,
              :QCD-ELMINCHG,
              :QCD-ELMAXAMT,
              :QCD-ELDELFAC,
              :QCD-ELYRTYPE,
              :QCD-ELUNITPER-CD,
              :QCD-PO-TITLE-HOLD-DAYS,
              :QCD-RE-ACTIONCD,
              :QCD-RE-MEMO-REQUIRED,
              :QCD-RE-SUPERVISOR-ONLY,
              :QCD-RE-ACTIVITY-REQUIRED,
              :QCD-RE-RESCHEDULE-REQ,
              :QCD-RE-RESCHEDULE-TIME,
              :QCD-RE-WORKED-ACCOUNT,
              :QCD-FC-FEEAMT,
              :QCD-TC-REFNO-1,
              :QCD-TC-REFNO-2,
              :QCD-TC-REFNO-3,
              :QCD-TC-REFNO-4,
              :QCD-TC-REFNO-5,
              :QCD-TC-REFNO-6,
              :QCD-TC-REFNO-7,
              :QCD-TC-REFNO-8,
              :QCD-TC-REFNO-9,
              :QCD-TC-REFNO-10,
              :QCD-PS-TDR-FLAG)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CD1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CD1-FILE.

           ACCEPT CD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           PERFORM SET-CD-FIELDS.

           EXEC SQL
            UPDATE DBO.CDFILE
             SET
              CDFILE.CD_TYPE = :QCD-TYPE,
              CDFILE.CD_CODE = :QCD-CODE,
              CDFILE.CD_LTOUCH_DATE =
                 NULLIF(:QCD-LTOUCH-DATE,'1900-01-01'),
              CDFILE.CD_DESC = :QCD-DESC,
              CDFILE.CD_AT_ACTIONCD = :QCD-AT-ACTIONCD,
              CDFILE.CD_AT_MEMO_REQUIRED = :QCD-AT-MEMO-REQUIRED,
              CDFILE.CD_AT_SUPERVISOR_ONLY =
                   :QCD-AT-SUPERVISOR-ONLY,
              CDFILE.CD_AT_RESULT_REQUIRED =
                   :QCD-AT-RESULT-REQUIRED,
              CDFILE.CD_BR_FILE_TYPE = :QCD-BR-FILE-TYPE,
              CDFILE.CD_BR_FILE_NAME = :QCD-BR-FILE-NAME,
              CDFILE.CD_BR_PAYOFF_FG = :QCD-BR-PAYOFF-FG,
              CDFILE.CD_BR_POST_SS_TRCD = :QCD-BR-POST-SS-TRCD,
              CDFILE.CD_BR_GL_BRANCH = :QCD-BR-GL-BRANCH,
              CDFILE.CD_BR_GL_ACCT = :QCD-BR-GL-ACCT,
              CDFILE.CD_BR_ALLOT_OPTION = :QCD-BR-ALLOT-OPTION,
              CDFILE.CD_BR_LBOX_OPTION = :QCD-BR-LBOX-OPTION,
              CDFILE.CD_BR_LC_FG = :QCD-BR-LC-FG,
              CDFILE.CD_BR_OT_GL_BRANCH = :QCD-BR-OT-GL-BRANCH,
              CDFILE.CD_BR_OT_GL_ACCT = :QCD-BR-OT-GL-ACCT,
              CDFILE.CD_BR_MULTIPLE_REFCDS =
                   :QCD-BR-MULTIPLE-REFCDS,
              CDFILE.CD_BR_COLLECT_LC = :QCD-BR-COLLECT-LC,
              CDFILE.CD_CO_GROUP = :QCD-CO-GROUP,
              CDFILE.CD_CO_SECURED = :QCD-CO-SECURED,
              CDFILE.CD_CO_TRW = :QCD-CO-TRW,
              CDFILE.CD_CT_INSTAXFRMLA_1 = :QCD-CT-INSTAXFRMLA-1,
              CDFILE.CD_CT_INSTAXAMT_1 = :QCD-CT-INSTAXAMT-1,
              CDFILE.CD_CT_INSTAXFRMLA_2 = :QCD-CT-INSTAXFRMLA-2,
              CDFILE.CD_CT_INSTAXAMT_2 = :QCD-CT-INSTAXAMT-2,
              CDFILE.CD_CT_INSTAXFRMLA_3 = :QCD-CT-INSTAXFRMLA-3,
              CDFILE.CD_CT_INSTAXAMT_3 = :QCD-CT-INSTAXAMT-3,
              CDFILE.CD_CT_INSTAXFRMLA_4 = :QCD-CT-INSTAXFRMLA-4,
              CDFILE.CD_CT_INSTAXAMT_4 = :QCD-CT-INSTAXAMT-4,
              CDFILE.CD_CT_INSTAXFRMLA_5 = :QCD-CT-INSTAXFRMLA-5,
              CDFILE.CD_CT_INSTAXAMT_5 = :QCD-CT-INSTAXAMT-5,
              CDFILE.CD_CT_INSTAXFRMLA_6 = :QCD-CT-INSTAXFRMLA-6,
              CDFILE.CD_CT_INSTAXAMT_6 = :QCD-CT-INSTAXAMT-6,
              CDFILE.CD_CT_INSTAXFRMLA_7 = :QCD-CT-INSTAXFRMLA-7,
              CDFILE.CD_CT_INSTAXAMT_7 = :QCD-CT-INSTAXAMT-7,
              CDFILE.CD_CT_INSTAXFRMLA_8 = :QCD-CT-INSTAXFRMLA-8,
              CDFILE.CD_CT_INSTAXAMT_8 = :QCD-CT-INSTAXAMT-8,
              CDFILE.CD_CT_INSTAXFRMLA_9 = :QCD-CT-INSTAXFRMLA-9,
              CDFILE.CD_CT_INSTAXAMT_9 = :QCD-CT-INSTAXAMT-9,
              CDFILE.CD_CT_INSTAXFRMLA_10 =
                   :QCD-CT-INSTAXFRMLA-10,
              CDFILE.CD_CT_INSTAXAMT_10 = :QCD-CT-INSTAXAMT-10,
              CDFILE.CD_CT_INSTAXFRMLA_11 =
                   :QCD-CT-INSTAXFRMLA-11,
              CDFILE.CD_CT_INSTAXAMT_11 = :QCD-CT-INSTAXAMT-11,
              CDFILE.CD_CT_INSTAXFRMLA_12 =
                   :QCD-CT-INSTAXFRMLA-12,
              CDFILE.CD_CT_INSTAXAMT_12 = :QCD-CT-INSTAXAMT-12,
              CDFILE.CD_CT_BOTH_TAX_FG = :QCD-CT-BOTH-TAX-FG,
              CDFILE.CD_CY_INSTAXFRMLA_1 = :QCD-CY-INSTAXFRMLA-1,
              CDFILE.CD_CY_INSTAXAMT_1 = :QCD-CY-INSTAXAMT-1,
              CDFILE.CD_CY_INSTAXFRMLA_2 = :QCD-CY-INSTAXFRMLA-2,
              CDFILE.CD_CY_INSTAXAMT_2 = :QCD-CY-INSTAXAMT-2,
              CDFILE.CD_CY_INSTAXFRMLA_3 = :QCD-CY-INSTAXFRMLA-3,
              CDFILE.CD_CY_INSTAXAMT_3 = :QCD-CY-INSTAXAMT-3,
              CDFILE.CD_CY_INSTAXFRMLA_4 = :QCD-CY-INSTAXFRMLA-4,
              CDFILE.CD_CY_INSTAXAMT_4 = :QCD-CY-INSTAXAMT-4,
              CDFILE.CD_CY_INSTAXFRMLA_5 = :QCD-CY-INSTAXFRMLA-5,
              CDFILE.CD_CY_INSTAXAMT_5 = :QCD-CY-INSTAXAMT-5,
              CDFILE.CD_CY_INSTAXFRMLA_6 = :QCD-CY-INSTAXFRMLA-6,
              CDFILE.CD_CY_INSTAXAMT_6 = :QCD-CY-INSTAXAMT-6,
              CDFILE.CD_CY_INSTAXFRMLA_7 = :QCD-CY-INSTAXFRMLA-7,
              CDFILE.CD_CY_INSTAXAMT_7 = :QCD-CY-INSTAXAMT-7,
              CDFILE.CD_CY_INSTAXFRMLA_8 = :QCD-CY-INSTAXFRMLA-8,
              CDFILE.CD_CY_INSTAXAMT_8 = :QCD-CY-INSTAXAMT-8,
              CDFILE.CD_CY_INSTAXFRMLA_9 = :QCD-CY-INSTAXFRMLA-9,
              CDFILE.CD_CY_INSTAXAMT_9 = :QCD-CY-INSTAXAMT-9,
              CDFILE.CD_CY_INSTAXFRMLA_10 =
                   :QCD-CY-INSTAXFRMLA-10,
              CDFILE.CD_CY_INSTAXAMT_10 = :QCD-CY-INSTAXAMT-10,
              CDFILE.CD_CY_INSTAXFRMLA_11 =
                   :QCD-CY-INSTAXFRMLA-11,
              CDFILE.CD_CY_INSTAXAMT_11 = :QCD-CY-INSTAXAMT-11,
              CDFILE.CD_CY_INSTAXFRMLA_12 =
                   :QCD-CY-INSTAXFRMLA-12,
              CDFILE.CD_CY_INSTAXAMT_12 = :QCD-CY-INSTAXAMT-12,
              CDFILE.CD_CY_BOTH_TAX_FG = :QCD-CY-BOTH-TAX-FG,
              CDFILE.CD_EA_CQ_FG = :QCD-EA-CQ-FG,
              CDFILE.CD_EA_GQ_FG = :QCD-EA-GQ-FG,
              CDFILE.CD_ELGRACE = :QCD-ELGRACE,
              CDFILE.CD_ELGRACECD = :QCD-ELGRACECD,
              CDFILE.CD_ELTYPE = :QCD-ELTYPE,
              CDFILE.CD_ELRATE = :QCD-ELRATE,
              CDFILE.CD_ELFRMLA = :QCD-ELFRMLA,
              CDFILE.CD_ELMINCHG = :QCD-ELMINCHG,
              CDFILE.CD_ELMAXAMT = :QCD-ELMAXAMT,
              CDFILE.CD_ELDELFAC = :QCD-ELDELFAC,
              CDFILE.CD_ELYRTYPE = :QCD-ELYRTYPE,
              CDFILE.CD_ELUNITPER_CD = :QCD-ELUNITPER-CD,
              CDFILE.CD_PO_TITLE_HOLD_DAYS =
                   :QCD-PO-TITLE-HOLD-DAYS,
              CDFILE.CD_RE_ACTIONCD = :QCD-RE-ACTIONCD,
              CDFILE.CD_RE_MEMO_REQUIRED = :QCD-RE-MEMO-REQUIRED,
              CDFILE.CD_RE_SUPERVISOR_ONLY =
                   :QCD-RE-SUPERVISOR-ONLY,
              CDFILE.CD_RE_ACTIVITY_REQUIRED =
                   :QCD-RE-ACTIVITY-REQUIRED,
              CDFILE.CD_RE_RESCHEDULE_REQ =
                   :QCD-RE-RESCHEDULE-REQ,
              CDFILE.CD_RE_RESCHEDULE_TIME =
                   :QCD-RE-RESCHEDULE-TIME,
              CDFILE.CD_RE_WORKED_ACCOUNT =
                   :QCD-RE-WORKED-ACCOUNT,
              CDFILE.CD_FC_FEEAMT = :QCD-FC-FEEAMT,
              CDFILE.CD_TC_REFNO_1 = :QCD-TC-REFNO-1,
              CDFILE.CD_TC_REFNO_2 = :QCD-TC-REFNO-2,
              CDFILE.CD_TC_REFNO_3 = :QCD-TC-REFNO-3,
              CDFILE.CD_TC_REFNO_4 = :QCD-TC-REFNO-4,
              CDFILE.CD_TC_REFNO_5 = :QCD-TC-REFNO-5,
              CDFILE.CD_TC_REFNO_6 = :QCD-TC-REFNO-6,
              CDFILE.CD_TC_REFNO_7 = :QCD-TC-REFNO-7,
              CDFILE.CD_TC_REFNO_8 = :QCD-TC-REFNO-8,
              CDFILE.CD_TC_REFNO_9 = :QCD-TC-REFNO-9,
              CDFILE.CD_TC_REFNO_10 = :QCD-TC-REFNO-10,
              CDFILE.CD_PS_TDR_FLAG = :QCD-PS-TDR-FLAG
             WHERE CDFILE.CD_TYPE = :QCD-TYPE
               AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CD1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CD1-FILE.

           PERFORM DELETE-IT.
           MOVE CD-SELECT-PATH TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           PERFORM SET-CD-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CDFILE
             WHERE CDFILE.CD_TYPE = :QCD-TYPE
               AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CD1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.
           IF ( CD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CD1-CURSOR-STAT.

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
              DESTROY IOCDIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
