       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGLRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-23-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGLRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GLFILEFILE 
      * TABLE DESC    : GL MASTER FILE
      * OUTPUT FILE   : XPC/IOGLRN.PRE
      * USED FOR      : LIBGL/GLWSGL.CPY  --- QGL-REC
      *                 LIBGL/GLGL1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0323 ***NEW***
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
           "@(#)IO/IOGLRN.CBL S34 06/30/21-11:53:28 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QGL1-WHERE-BEG-KEY.
           03  QGL1-WBEG-BR-ACCOUNT        PIC 9(11).
           03  QGL1-WBEG-BR-ACCOUNTX REDEFINES QGL1-WBEG-BR-ACCOUNT.
               05  QGL1-WBEG-BRANCH        PIC 9(4).
               05  QGL1-WBEG-ACCOUNT       PIC 9(7).

       01  QGL1-WHERE-END-KEY.
           03  QGL1-WEND-BR-ACCOUNT        PIC 9(11).
           03  QGL1-WEND-BR-ACCOUNTX REDEFINES QGL1-WEND-BR-ACCOUNT.
               05  QGL1-WEND-BRANCH        PIC 9(4).
               05  QGL1-WEND-ACCOUNT       PIC 9(7).
 
       01  QGL2-WHERE-BEG-KEY.
           03  QGL2-WBEG-DESC              PIC X(36).
 
       01  QGL2-WHERE-END-KEY.
           03  QGL2-WEND-DESC              PIC X(36).

       01  QGL-REC.
           03  QGL-FLAG             PIC X.
           03  QGL-BR-ACCOUNT       PIC 9(11).
           03  QGL-LTOUCH-DATE      PIC X(10).
           03  QGL-DESCRIPTION      PIC X(36).
           03  QGL-CONSOL           PIC 9(4).
           03  QGL-OPENBAL          PIC S9(9)V99.
           03  QGL-CYBAL-1          PIC S9(9)V99.
           03  QGL-CYBAL-2          PIC S9(9)V99.
           03  QGL-CYBAL-3          PIC S9(9)V99.
           03  QGL-CYBAL-4          PIC S9(9)V99.
           03  QGL-CYBAL-5          PIC S9(9)V99.
           03  QGL-CYBAL-6          PIC S9(9)V99.
           03  QGL-CYBAL-7          PIC S9(9)V99.
           03  QGL-CYBAL-8          PIC S9(9)V99.
           03  QGL-CYBAL-9          PIC S9(9)V99.
           03  QGL-CYBAL-10         PIC S9(9)V99.
           03  QGL-CYBAL-11         PIC S9(9)V99.
           03  QGL-CYBAL-12         PIC S9(9)V99.
           03  QGL-LYBAL-1          PIC S9(9)V99.
           03  QGL-LYBAL-2          PIC S9(9)V99.
           03  QGL-LYBAL-3          PIC S9(9)V99.
           03  QGL-LYBAL-4          PIC S9(9)V99.
           03  QGL-LYBAL-5          PIC S9(9)V99.
           03  QGL-LYBAL-6          PIC S9(9)V99.
           03  QGL-LYBAL-7          PIC S9(9)V99.
           03  QGL-LYBAL-8          PIC S9(9)V99.
           03  QGL-LYBAL-9          PIC S9(9)V99.
           03  QGL-LYBAL-10         PIC S9(9)V99.
           03  QGL-LYBAL-11         PIC S9(9)V99.
           03  QGL-LYBAL-12         PIC S9(9)V99.
           03  QGL-CYBUD-1          PIC S9(9).
           03  QGL-CYBUD-2          PIC S9(9).
           03  QGL-CYBUD-3          PIC S9(9).
           03  QGL-CYBUD-4          PIC S9(9).
           03  QGL-CYBUD-5          PIC S9(9).
           03  QGL-CYBUD-6          PIC S9(9).
           03  QGL-CYBUD-7          PIC S9(9).
           03  QGL-CYBUD-8          PIC S9(9).
           03  QGL-CYBUD-9          PIC S9(9).
           03  QGL-CYBUD-10         PIC S9(9).
           03  QGL-CYBUD-11         PIC S9(9).
           03  QGL-CYBUD-12         PIC S9(9).
           03  QGL-LYBUD-1          PIC S9(9).
           03  QGL-LYBUD-2          PIC S9(9).
           03  QGL-LYBUD-3          PIC S9(9).
           03  QGL-LYBUD-4          PIC S9(9).
           03  QGL-LYBUD-5          PIC S9(9).
           03  QGL-LYBUD-6          PIC S9(9).
           03  QGL-LYBUD-7          PIC S9(9).
           03  QGL-LYBUD-8          PIC S9(9).
           03  QGL-LYBUD-9          PIC S9(9).
           03  QGL-LYBUD-10         PIC S9(9).
           03  QGL-LYBUD-11         PIC S9(9).
           03  QGL-LYBUD-12         PIC S9(9).
           03  QGL-NYBUD-1          PIC S9(9).
           03  QGL-NYBUD-2          PIC S9(9).
           03  QGL-NYBUD-3          PIC S9(9).
           03  QGL-NYBUD-4          PIC S9(9).
           03  QGL-NYBUD-5          PIC S9(9).
           03  QGL-NYBUD-6          PIC S9(9).
           03  QGL-NYBUD-7          PIC S9(9).
           03  QGL-NYBUD-8          PIC S9(9).
           03  QGL-NYBUD-9          PIC S9(9).
           03  QGL-NYBUD-10         PIC S9(9).
           03  QGL-NYBUD-11         PIC S9(9).
           03  QGL-NYBUD-12         PIC S9(9).
           03  QGL-TYPE             PIC X.
           03  QGL-CLEAR            PIC X.
           03  QGL-NUMBER1          PIC 9(7).
           03  QGL-NUMBER2          PIC 9(7).
           03  QGL-FS-FLAG          PIC X.
           03  QGL-REPORTS-1        PIC X.
           03  QGL-REPORTS-2        PIC X.
           03  QGL-REPORTS-3        PIC X.
           03  QGL-REPORTS-4        PIC X.
           03  QGL-REPORTS-5        PIC X.
           03  QGL-LYOPENBAL        PIC S9(9)V99.
           03  QGL-DLALLOC-GL       PIC X.
           03  QGL-SPANISH-DESC     PIC X(36).

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
           03  FILLER PIC X(30) VALUE "BEGIN  BRANCH/ACCOUNT(F3-ALL) ".

       01  LEGEND-END-BRANCH. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER END BRANCH/ACCOUNT".

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
           03  BEG-BR-ACCOUNT           PIC 9(11).
           03  BEG-X REDEFINES BEG-BR-ACCOUNT.
               05  BEG-BRANCH        PIC 9(4).
               05  BEG-ACCOUNT       PIC 9(7).
           03  END-BR-ACCOUNT           PIC 9(11).
           03  END-X REDEFINES END-BR-ACCOUNT.
               05  END-BRANCH        PIC 9(4).
               05  END-ACCOUNT       PIC 9(7).
           03  BEG-DESC                 PIC X(36).
           03  END-DESC                 PIC X(36).

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
           03  FILLER PIC X(38) VALUE "BRNO   ACCOUNT   DESC".
           03  FILLER PIC X(30) VALUE "OPEN BAL  TYP       CY BAL".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GL-BRANCH                  PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-GL-ACCOUNT                 PIC 9(07).
           03  FILLER                         PIC X(03).
           03  D01-GL-DESC                    PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-GL-OPENBAL                 PIC Z(7)99.99.
           03  FILLER                         PIC X(03).
           03  D01-GL-TYPE                    PIC X.
           03  FILLER                         PIC X(03).
           03  D01-GL-CYBAL-1                 PIC Z(7)99.99.

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
       COPY "LIBIO/IOGLRN_DEF.CPY".
       COPY "LIBIO/IOGLRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGLRN_SCN.CPY".

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
           MOVE "B"                  TO GL-PATH-OVERRIDE.

           MOVE BEG-BRANCH       TO QGL1-WBEG-BRANCH.
           MOVE BEG-ACCOUNT      TO QGL1-WBEG-ACCOUNT.
           MOVE END-BRANCH       TO QGL1-WEND-BRANCH.
           MOVE END-ACCOUNT      TO QGL1-WEND-ACCOUNT.

           MOVE BEG-DESC             TO QGL2-WBEG-DESC.
           MOVE END-DESC             TO QGL2-WEND-DESC.


           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-GL1-FILE
              PERFORM START-GL1-FILE
           ELSE
              PERFORM OPEN-GL2-FILE
              PERFORM START-GL2-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-GL1-FILE-NEXT
           ELSE
              PERFORM READ-GL2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        
           IF ( PROCESS-KEY = 1 )
              IF ( GL-BRANCH < BEG-BRANCH )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( GL-BRANCH > END-BRANCH )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( GL-ACCOUNT < BEG-ACCOUNT  OR
                      GL-ACCOUNT > END-ACCOUNT)
                 GO TO MAIN-PROGRAM-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( GL-DESCRIPTION < BEG-DESC )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( GL-DESCRIPTION > END-DESC )
                 GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE GL-BRANCH                  TO D01-GL-BRANCH.
           MOVE GL-ACCOUNT                 TO D01-GL-ACCOUNT.
           MOVE GL-DESCRIPTION             TO D01-GL-DESC.
           MOVE GL-OPENBAL                 TO D01-GL-OPENBAL.
           MOVE GL-TYPE                    TO D01-GL-TYPE.
           MOVE GL-CYBAL(1)                TO D01-GL-CYBAL-1.
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
           MOVE "IOGLRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGLRN-QUEUE-POS  TO Q-POS.
           MOVE IOGLRN-COPIES-POS TO C-POS.
           MOVE IOGLRN-PRU-POS    TO P-POS.
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

           MOVE "EN N110BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BR-ACCOUNT.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N110END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BR-ACCOUNT.

           IF ( BEG-BR-ACCOUNT > END-BR-ACCOUNT )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-BEG-DESC.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SN N110BEG-BRANCH." TO VDU.
           MOVE 0                    TO BEG-BR-ACCOUNT VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N110END-BRANCH." TO VDU.
           MOVE 99999999999         TO END-BR-ACCOUNT VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-DESC.

           MOVE "E  A360BEG-DESC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DESC.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DESC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-DESC.

       ENTER-END-DESC.

           MOVE "E  A360END-DESC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DESC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DESC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-DESC.

           IF ( BEG-DESC > END-DESC )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DESC.

           GO TO ENTER-PROCESS-KEY.

      *==============================================================
       ENTER-ALL-DESC.
           MOVE "S  A360BEG-DESC." TO VDU.
           MOVE SPACES               TO BEG-DESC VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A360END-DESC." TO VDU.
           MOVE ALL "Z"              TO END-DESC VDUBUF.
           PERFORM SCREENIO.

      *============================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DESC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 2      )     
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

           MOVE "SN N110BEG-BRANCH."    TO VDU.
           MOVE BEG-BR-ACCOUNT          TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N110END-BRANCH."    TO VDU.
           MOVE END-BR-ACCOUNT          TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-DESC."    TO VDU.
           MOVE BEG-DESC              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-DESC."    TO VDU.
           MOVE END-DESC              TO VDUBUF.
           PERFORM SCREENIO.

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

           IF ( PROCESS-KEY = 1 )
              MOVE GL1-KEY TO PACE-DETAIL-KEY
           ELSE
              MOVE GL2-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING BRANCH/ACCOUNT       : "   TO R01-PROMPT.
           MOVE BEG-BR-ACCOUNT                        TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRANCH/ACCOUNT       : "   TO R01-PROMPT.
           MOVE END-BR-ACCOUNT                        TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DESC               : "    TO R01-PROMPT.
           MOVE BEG-DESC                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DESC               : "    TO R01-PROMPT.
           MOVE END-DESC                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - GLFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOGLRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGLRN-SYSDATE.
           DISPLAY IOGLRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGLRN-FORM-FIELDS TO WR-BUF.
           MOVE IOGLRN-FORM-LIST TO WR-LIST.
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

           MOVE IOGLRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGLRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOGLRN_EVA.CPY".
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
      *    LOAD GL-REC FROM QGL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GL-FIELDS.
           MOVE QGL-FLAG TO GL-FLAG.
           MOVE QGL-BR-ACCOUNT TO GL-BR-ACCOUNT.

           IF ( QGL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GL-LTOUCH-DATE
           ELSE
              MOVE QGL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GL-LTOUCH-DATE.

           MOVE QGL-DESCRIPTION TO GL-DESCRIPTION.
           MOVE QGL-CONSOL TO GL-CONSOL.
           MOVE QGL-OPENBAL TO GL-OPENBAL.
           MOVE QGL-CYBAL-1 TO GL-CYBAL(1).
           MOVE QGL-CYBAL-2 TO GL-CYBAL(2).
           MOVE QGL-CYBAL-3 TO GL-CYBAL(3).
           MOVE QGL-CYBAL-4 TO GL-CYBAL(4).
           MOVE QGL-CYBAL-5 TO GL-CYBAL(5).
           MOVE QGL-CYBAL-6 TO GL-CYBAL(6).
           MOVE QGL-CYBAL-7 TO GL-CYBAL(7).
           MOVE QGL-CYBAL-8 TO GL-CYBAL(8).
           MOVE QGL-CYBAL-9 TO GL-CYBAL(9).
           MOVE QGL-CYBAL-10 TO GL-CYBAL(10).
           MOVE QGL-CYBAL-11 TO GL-CYBAL(11).
           MOVE QGL-CYBAL-12 TO GL-CYBAL(12).
           MOVE QGL-LYBAL-1 TO GL-LYBAL(1).
           MOVE QGL-LYBAL-2 TO GL-LYBAL(2).
           MOVE QGL-LYBAL-3 TO GL-LYBAL(3).
           MOVE QGL-LYBAL-4 TO GL-LYBAL(4).
           MOVE QGL-LYBAL-5 TO GL-LYBAL(5).
           MOVE QGL-LYBAL-6 TO GL-LYBAL(6).
           MOVE QGL-LYBAL-7 TO GL-LYBAL(7).
           MOVE QGL-LYBAL-8 TO GL-LYBAL(8).
           MOVE QGL-LYBAL-9 TO GL-LYBAL(9).
           MOVE QGL-LYBAL-10 TO GL-LYBAL(10).
           MOVE QGL-LYBAL-11 TO GL-LYBAL(11).
           MOVE QGL-LYBAL-12 TO GL-LYBAL(12).
           MOVE QGL-CYBUD-1 TO GL-CYBUD(1).
           MOVE QGL-CYBUD-2 TO GL-CYBUD(2).
           MOVE QGL-CYBUD-3 TO GL-CYBUD(3).
           MOVE QGL-CYBUD-4 TO GL-CYBUD(4).
           MOVE QGL-CYBUD-5 TO GL-CYBUD(5).
           MOVE QGL-CYBUD-6 TO GL-CYBUD(6).
           MOVE QGL-CYBUD-7 TO GL-CYBUD(7).
           MOVE QGL-CYBUD-8 TO GL-CYBUD(8).
           MOVE QGL-CYBUD-9 TO GL-CYBUD(9).
           MOVE QGL-CYBUD-10 TO GL-CYBUD(10).
           MOVE QGL-CYBUD-11 TO GL-CYBUD(11).
           MOVE QGL-CYBUD-12 TO GL-CYBUD(12).
           MOVE QGL-LYBUD-1 TO GL-LYBUD(1).
           MOVE QGL-LYBUD-2 TO GL-LYBUD(2).
           MOVE QGL-LYBUD-3 TO GL-LYBUD(3).
           MOVE QGL-LYBUD-4 TO GL-LYBUD(4).
           MOVE QGL-LYBUD-5 TO GL-LYBUD(5).
           MOVE QGL-LYBUD-6 TO GL-LYBUD(6).
           MOVE QGL-LYBUD-7 TO GL-LYBUD(7).
           MOVE QGL-LYBUD-8 TO GL-LYBUD(8).
           MOVE QGL-LYBUD-9 TO GL-LYBUD(9).
           MOVE QGL-LYBUD-10 TO GL-LYBUD(10).
           MOVE QGL-LYBUD-11 TO GL-LYBUD(11).
           MOVE QGL-LYBUD-12 TO GL-LYBUD(12).
           MOVE QGL-NYBUD-1 TO GL-NYBUD(1).
           MOVE QGL-NYBUD-2 TO GL-NYBUD(2).
           MOVE QGL-NYBUD-3 TO GL-NYBUD(3).
           MOVE QGL-NYBUD-4 TO GL-NYBUD(4).
           MOVE QGL-NYBUD-5 TO GL-NYBUD(5).
           MOVE QGL-NYBUD-6 TO GL-NYBUD(6).
           MOVE QGL-NYBUD-7 TO GL-NYBUD(7).
           MOVE QGL-NYBUD-8 TO GL-NYBUD(8).
           MOVE QGL-NYBUD-9 TO GL-NYBUD(9).
           MOVE QGL-NYBUD-10 TO GL-NYBUD(10).
           MOVE QGL-NYBUD-11 TO GL-NYBUD(11).
           MOVE QGL-NYBUD-12 TO GL-NYBUD(12).
           MOVE QGL-TYPE TO GL-TYPE.
           MOVE QGL-CLEAR TO GL-CLEAR.
           MOVE QGL-NUMBER1 TO GL-NUMBER1.
           MOVE QGL-NUMBER2 TO GL-NUMBER2.
           MOVE QGL-FS-FLAG TO GL-FS-FLAG.
           MOVE QGL-REPORTS-1 TO GL-REPORTS(1).
           MOVE QGL-REPORTS-2 TO GL-REPORTS(2).
           MOVE QGL-REPORTS-3 TO GL-REPORTS(3).
           MOVE QGL-REPORTS-4 TO GL-REPORTS(4).
           MOVE QGL-REPORTS-5 TO GL-REPORTS(5).
           MOVE QGL-LYOPENBAL TO GL-LYOPENBAL.
           MOVE QGL-DLALLOC-GL TO GL-DLALLOC-GL.
           MOVE QGL-SPANISH-DESC TO GL-SPANISH-DESC.
 
      *-----------------------------------------------------------------
      *    LOAD QGL-REC FROM GL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GL-FIELDS.
           MOVE GL-FLAG TO QGL-FLAG.
           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           IF ( GL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGL-LTOUCH-DATE
           ELSE
              MOVE GL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGL-LTOUCH-DATE.

           MOVE GL-DESCRIPTION TO QGL-DESCRIPTION.
           MOVE GL-CONSOL TO QGL-CONSOL.
           MOVE GL-OPENBAL TO QGL-OPENBAL.
           MOVE GL-CYBAL(1) TO QGL-CYBAL-1.
           MOVE GL-CYBAL(2) TO QGL-CYBAL-2.
           MOVE GL-CYBAL(3) TO QGL-CYBAL-3.
           MOVE GL-CYBAL(4) TO QGL-CYBAL-4.
           MOVE GL-CYBAL(5) TO QGL-CYBAL-5.
           MOVE GL-CYBAL(6) TO QGL-CYBAL-6.
           MOVE GL-CYBAL(7) TO QGL-CYBAL-7.
           MOVE GL-CYBAL(8) TO QGL-CYBAL-8.
           MOVE GL-CYBAL(9) TO QGL-CYBAL-9.
           MOVE GL-CYBAL(10) TO QGL-CYBAL-10.
           MOVE GL-CYBAL(11) TO QGL-CYBAL-11.
           MOVE GL-CYBAL(12) TO QGL-CYBAL-12.
           MOVE GL-LYBAL(1) TO QGL-LYBAL-1.
           MOVE GL-LYBAL(2) TO QGL-LYBAL-2.
           MOVE GL-LYBAL(3) TO QGL-LYBAL-3.
           MOVE GL-LYBAL(4) TO QGL-LYBAL-4.
           MOVE GL-LYBAL(5) TO QGL-LYBAL-5.
           MOVE GL-LYBAL(6) TO QGL-LYBAL-6.
           MOVE GL-LYBAL(7) TO QGL-LYBAL-7.
           MOVE GL-LYBAL(8) TO QGL-LYBAL-8.
           MOVE GL-LYBAL(9) TO QGL-LYBAL-9.
           MOVE GL-LYBAL(10) TO QGL-LYBAL-10.
           MOVE GL-LYBAL(11) TO QGL-LYBAL-11.
           MOVE GL-LYBAL(12) TO QGL-LYBAL-12.
           MOVE GL-CYBUD(1) TO QGL-CYBUD-1.
           MOVE GL-CYBUD(2) TO QGL-CYBUD-2.
           MOVE GL-CYBUD(3) TO QGL-CYBUD-3.
           MOVE GL-CYBUD(4) TO QGL-CYBUD-4.
           MOVE GL-CYBUD(5) TO QGL-CYBUD-5.
           MOVE GL-CYBUD(6) TO QGL-CYBUD-6.
           MOVE GL-CYBUD(7) TO QGL-CYBUD-7.
           MOVE GL-CYBUD(8) TO QGL-CYBUD-8.
           MOVE GL-CYBUD(9) TO QGL-CYBUD-9.
           MOVE GL-CYBUD(10) TO QGL-CYBUD-10.
           MOVE GL-CYBUD(11) TO QGL-CYBUD-11.
           MOVE GL-CYBUD(12) TO QGL-CYBUD-12.
           MOVE GL-LYBUD(1) TO QGL-LYBUD-1.
           MOVE GL-LYBUD(2) TO QGL-LYBUD-2.
           MOVE GL-LYBUD(3) TO QGL-LYBUD-3.
           MOVE GL-LYBUD(4) TO QGL-LYBUD-4.
           MOVE GL-LYBUD(5) TO QGL-LYBUD-5.
           MOVE GL-LYBUD(6) TO QGL-LYBUD-6.
           MOVE GL-LYBUD(7) TO QGL-LYBUD-7.
           MOVE GL-LYBUD(8) TO QGL-LYBUD-8.
           MOVE GL-LYBUD(9) TO QGL-LYBUD-9.
           MOVE GL-LYBUD(10) TO QGL-LYBUD-10.
           MOVE GL-LYBUD(11) TO QGL-LYBUD-11.
           MOVE GL-LYBUD(12) TO QGL-LYBUD-12.
           MOVE GL-NYBUD(1) TO QGL-NYBUD-1.
           MOVE GL-NYBUD(2) TO QGL-NYBUD-2.
           MOVE GL-NYBUD(3) TO QGL-NYBUD-3.
           MOVE GL-NYBUD(4) TO QGL-NYBUD-4.
           MOVE GL-NYBUD(5) TO QGL-NYBUD-5.
           MOVE GL-NYBUD(6) TO QGL-NYBUD-6.
           MOVE GL-NYBUD(7) TO QGL-NYBUD-7.
           MOVE GL-NYBUD(8) TO QGL-NYBUD-8.
           MOVE GL-NYBUD(9) TO QGL-NYBUD-9.
           MOVE GL-NYBUD(10) TO QGL-NYBUD-10.
           MOVE GL-NYBUD(11) TO QGL-NYBUD-11.
           MOVE GL-NYBUD(12) TO QGL-NYBUD-12.
           MOVE GL-TYPE TO QGL-TYPE.
           MOVE GL-CLEAR TO QGL-CLEAR.
           MOVE GL-NUMBER1 TO QGL-NUMBER1.
           MOVE GL-NUMBER2 TO QGL-NUMBER2.
           MOVE GL-FS-FLAG TO QGL-FS-FLAG.
           MOVE GL-REPORTS(1) TO QGL-REPORTS-1.
           MOVE GL-REPORTS(2) TO QGL-REPORTS-2.
           MOVE GL-REPORTS(3) TO QGL-REPORTS-3.
           MOVE GL-REPORTS(4) TO QGL-REPORTS-4.
           MOVE GL-REPORTS(5) TO QGL-REPORTS-5.
           MOVE GL-LYOPENBAL TO QGL-LYOPENBAL.
           MOVE GL-DLALLOC-GL TO QGL-DLALLOC-GL.
           MOVE GL-SPANISH-DESC TO QGL-SPANISH-DESC.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-GL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GL-ALL-BASE
              MOVE FILPATH-MACHINE         TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GLFILE NOT = "S" )
              MOVE GL-PATH-BEG             TO GL-SELECT-PATH-BEG
              MOVE GL-PATH-FILE            TO GL-SELECT-PATH-FILE
           ELSE
              MOVE GL-PATH-SQL             TO GL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GL1-FILE.
           PERFORM LOAD-GL1-FILE.
           PERFORM OPEN-IT.
           MOVE GL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GL1-FILE.
           PERFORM START-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           IF ( GL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GL1-FILE.

           IF ( QGL1-WEND-ACCOUNT = ZEROES    ) OR
              ( QGL1-WEND-ACCOUNT NOT NUMERIC )
              MOVE GL-BRANCH        TO QGL1-WBEG-BRANCH
              MOVE GL-ACCOUNT       TO QGL1-WBEG-ACCOUNT
              MOVE ALL "9"          TO QGL1-WEND-BRANCH
                                       QGL1-WEND-ACCOUNT.

           EXEC SQL
            DECLARE GL1_CURSOR CURSOR FOR
            SELECT ISNULL(GLFILE.GL_FLAG,''),
                   GLFILE.GL_BR_ACCOUNT,
                   ISNULL(CAST(GLFILE.GL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GLFILE.GL_DESCRIPTION,''),
                   GLFILE.GL_CONSOL,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   ISNULL(GLFILE.GL_TYPE,''),
                   ISNULL(GLFILE.GL_CLEAR,''),
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   ISNULL(GLFILE.GL_FS_FLAG,''),
                   ISNULL(GLFILE.GL_REPORTS_1,''),
                   ISNULL(GLFILE.GL_REPORTS_2,''),
                   ISNULL(GLFILE.GL_REPORTS_3,''),
                   ISNULL(GLFILE.GL_REPORTS_4,''),
                   ISNULL(GLFILE.GL_REPORTS_5,''),
                   GLFILE.GL_LYOPENBAL,
                   ISNULL(GLFILE.GL_DLALLOC_GL,''),
                   ISNULL(GLFILE.GL_SPANISH_DESC,'')
            FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT >= :QGL1-WBEG-BR-ACCOUNT
              AND GLFILE.GL_BR_ACCOUNT <= :QGL1-WEND-BR-ACCOUNT
            ORDER BY GLFILE.GL_BR_ACCOUNT
           END-EXEC.

           EXEC SQL
               OPEN GL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGL1-WBEG-BRANCH
                          QGL1-WBEG-ACCOUNT
                          QGL1-WEND-BRANCH
                          QGL1-WEND-ACCOUNT.

           MOVE STAT---GOOD TO GL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GL1-FILE.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           EXEC SQL
            SELECT ISNULL(GLFILE.GL_FLAG,''),
                   GLFILE.GL_BR_ACCOUNT,
                   ISNULL(CAST(GLFILE.GL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GLFILE.GL_DESCRIPTION,''),
                   GLFILE.GL_CONSOL,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   ISNULL(GLFILE.GL_TYPE,''),
                   ISNULL(GLFILE.GL_CLEAR,''),
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   ISNULL(GLFILE.GL_FS_FLAG,''),
                   ISNULL(GLFILE.GL_REPORTS_1,''),
                   ISNULL(GLFILE.GL_REPORTS_2,''),
                   ISNULL(GLFILE.GL_REPORTS_3,''),
                   ISNULL(GLFILE.GL_REPORTS_4,''),
                   ISNULL(GLFILE.GL_REPORTS_5,''),
                   GLFILE.GL_LYOPENBAL,
                   ISNULL(GLFILE.GL_DLALLOC_GL,''),
                   ISNULL(GLFILE.GL_SPANISH_DESC,'')
            INTO :QGL-REC
            FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       READ-GL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GL1_CURSOR INTO :QGL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.
           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-GL2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GL-ALL-BASE
              MOVE FILPATH-MACHINE         TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GLFILE NOT = "S" )
              MOVE GL-PATH-BEG             TO GL-SELECT-PATH-BEG
              MOVE GL-PATH-FILE            TO GL-SELECT-PATH-FILE
           ELSE
              MOVE GL-PATH-SQL             TO GL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GL2-FILE.
           PERFORM LOAD-GL2-FILE.
           PERFORM OPEN-IT.
           MOVE GL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GL2-FILE.
           PERFORM START-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL2-KEY TO E-KEYX.

           IF ( GL2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GL2-FILE.

           IF ( QGL2-WEND-DESC = SPACES )
              MOVE GL-DESCRIPTION       TO QGL2-WBEG-DESC
              MOVE ALL "Z"              TO QGL2-WEND-DESC.

           EXEC SQL
            DECLARE GL2_CURSOR CURSOR FOR
            SELECT ISNULL(GLFILE.GL_FLAG,''),
                   GLFILE.GL_BR_ACCOUNT,
                   ISNULL(CAST(GLFILE.GL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GLFILE.GL_DESCRIPTION,''),
                   GLFILE.GL_CONSOL,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   ISNULL(GLFILE.GL_TYPE,''),
                   ISNULL(GLFILE.GL_CLEAR,''),
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   ISNULL(GLFILE.GL_FS_FLAG,''),
                   ISNULL(GLFILE.GL_REPORTS_1,''),
                   ISNULL(GLFILE.GL_REPORTS_2,''),
                   ISNULL(GLFILE.GL_REPORTS_3,''),
                   ISNULL(GLFILE.GL_REPORTS_4,''),
                   ISNULL(GLFILE.GL_REPORTS_5,''),
                   GLFILE.GL_LYOPENBAL,
                   ISNULL(GLFILE.GL_DLALLOC_GL,''),
                   ISNULL(GLFILE.GL_SPANISH_DESC,'')
            FROM DBO.GLFILE
            WHERE GLFILE.GL_DESCRIPTION >= :QGL2-WBEG-DESC
              AND GLFILE.GL_DESCRIPTION <= :QGL2-WEND-DESC
            ORDER BY GLFILE.GL_DESCRIPTION,
                     GLFILE.GL_BR_ACCOUNT
           END-EXEC.

           EXEC SQL
               OPEN GL2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGL2-WBEG-DESC
                          QGL2-WEND-DESC.

           MOVE STAT---GOOD TO GL2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GL2-FILE.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL2-KEY TO E-KEYX.

           MOVE GL-DESCRIPTION TO QGL-DESCRIPTION.

           EXEC SQL
            SELECT ISNULL(GLFILE.GL_FLAG,''),
                   GLFILE.GL_BR_ACCOUNT,
                   ISNULL(CAST(GLFILE.GL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GLFILE.GL_DESCRIPTION,''),
                   GLFILE.GL_CONSOL,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   ISNULL(GLFILE.GL_TYPE,''),
                   ISNULL(GLFILE.GL_CLEAR,''),
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   ISNULL(GLFILE.GL_FS_FLAG,''),
                   ISNULL(GLFILE.GL_REPORTS_1,''),
                   ISNULL(GLFILE.GL_REPORTS_2,''),
                   ISNULL(GLFILE.GL_REPORTS_3,''),
                   ISNULL(GLFILE.GL_REPORTS_4,''),
                   ISNULL(GLFILE.GL_REPORTS_5,''),
                   GLFILE.GL_LYOPENBAL,
                   ISNULL(GLFILE.GL_DLALLOC_GL,''),
                   ISNULL(GLFILE.GL_SPANISH_DESC,'')
            INTO :QGL-REC
            FROM DBO.GLFILE
            WHERE GLFILE.GL_DESCRIPTION = :QGL-DESCRIPTION
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       READ-GL2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL2-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           IF ( GL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GL2_CURSOR INTO :QGL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL2-FILE.
           IF ( GL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GL2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GL2-CURSOR-STAT.

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
              DESTROY IOGLRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
