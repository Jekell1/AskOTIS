       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOIRRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-16-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOIRRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : IIFILE 
      * TABLE DESC    : INSURANCE RATE TABLE
      * OUTPUT FILE   : XPC/IOIRRN.PRE
      * USED FOR      : LIBLP/LPWSIR.CPY  --- QIR-REC
      *                 LIBLP/LPIRR.CPY  --- READ
      *                 LIBLP/LPIRRN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0316 ***NEW***
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
           "@(#)IO/IOIRRN.CBL S34 05/18/21-08:59:50 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LPWSIR.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QIR-WHERE-BEG-KEY.
           03  QIR-WBEG-KEY            PIC 9(3).
 
       01  QIR-WHERE-END-KEY.
           03  QIR-WEND-KEY            PIC 9(3).
 
       01  QIR-REC.
           03  QIR-KEY                PIC 999.
           03  QIR-LTOUCH-DATE        PIC X(10).
           03  QIR-PER-FG             PIC X.
           03  QIR-INSRATE-1          PIC 99V999.
           03  QIR-INSRATE-2          PIC 99V999.
           03  QIR-INSRATE-3          PIC 99V999.
           03  QIR-INSRATE-4          PIC 99V999.
           03  QIR-INSRATE-5          PIC 99V999.
           03  QIR-INSRATE-6          PIC 99V999.
           03  QIR-INSRATE-7          PIC 99V999.
           03  QIR-INSRATE-8          PIC 99V999.
           03  QIR-INSRATE-9          PIC 99V999.
           03  QIR-INSRATE-10         PIC 99V999.
           03  QIR-INSRATE-11          PIC 99V999.
           03  QIR-INSRATE-12          PIC 99V999.
           03  QIR-INSRATE-13          PIC 99V999.
           03  QIR-INSRATE-14          PIC 99V999.
           03  QIR-INSRATE-15          PIC 99V999.
           03  QIR-INSRATE-16          PIC 99V999.
           03  QIR-INSRATE-17          PIC 99V999.
           03  QIR-INSRATE-18          PIC 99V999.
           03  QIR-INSRATE-19          PIC 99V999.
           03  QIR-INSRATE-20         PIC 99V999.
           03  QIR-INSRATE-21          PIC 99V999.
           03  QIR-INSRATE-22          PIC 99V999.
           03  QIR-INSRATE-23          PIC 99V999.
           03  QIR-INSRATE-24          PIC 99V999.
           03  QIR-INSRATE-25          PIC 99V999.
           03  QIR-INSRATE-26          PIC 99V999.
           03  QIR-INSRATE-27          PIC 99V999.
           03  QIR-INSRATE-28          PIC 99V999.
           03  QIR-INSRATE-29          PIC 99V999.
           03  QIR-INSRATE-30         PIC 99V999.
           03  QIR-INSRATE-31          PIC 99V999.
           03  QIR-INSRATE-32          PIC 99V999.
           03  QIR-INSRATE-33          PIC 99V999.
           03  QIR-INSRATE-34          PIC 99V999.
           03  QIR-INSRATE-35          PIC 99V999.
           03  QIR-INSRATE-36          PIC 99V999.
           03  QIR-INSRATE-37          PIC 99V999.
           03  QIR-INSRATE-38          PIC 99V999.
           03  QIR-INSRATE-39          PIC 99V999.
           03  QIR-INSRATE-40         PIC 99V999.
           03  QIR-INSRATE-41          PIC 99V999.
           03  QIR-INSRATE-42          PIC 99V999.
           03  QIR-INSRATE-43          PIC 99V999.
           03  QIR-INSRATE-44          PIC 99V999.
           03  QIR-INSRATE-45          PIC 99V999.
           03  QIR-INSRATE-46          PIC 99V999.
           03  QIR-INSRATE-47          PIC 99V999.
           03  QIR-INSRATE-48          PIC 99V999.
           03  QIR-INSRATE-49          PIC 99V999.
           03  QIR-INSRATE-50         PIC 99V999.
           03  QIR-INSRATE-51          PIC 99V999.
           03  QIR-INSRATE-52          PIC 99V999.
           03  QIR-INSRATE-53          PIC 99V999.
           03  QIR-INSRATE-54          PIC 99V999.
           03  QIR-INSRATE-55          PIC 99V999.
           03  QIR-INSRATE-56          PIC 99V999.
           03  QIR-INSRATE-57          PIC 99V999.
           03  QIR-INSRATE-58          PIC 99V999.
           03  QIR-INSRATE-59          PIC 99V999.
           03  QIR-INSRATE-60         PIC 99V999.
           03  QIR-INSRATE-61          PIC 99V999.
           03  QIR-INSRATE-62          PIC 99V999.
           03  QIR-INSRATE-63          PIC 99V999.
           03  QIR-INSRATE-64          PIC 99V999.
           03  QIR-INSRATE-65          PIC 99V999.
           03  QIR-INSRATE-66          PIC 99V999.
           03  QIR-INSRATE-67          PIC 99V999.
           03  QIR-INSRATE-68          PIC 99V999.
           03  QIR-INSRATE-69          PIC 99V999.
           03  QIR-INSRATE-70         PIC 99V999.
           03  QIR-INSRATE-71          PIC 99V999.
           03  QIR-INSRATE-72          PIC 99V999.
           03  QIR-INSRATE-73          PIC 99V999.
           03  QIR-INSRATE-74          PIC 99V999.
           03  QIR-INSRATE-75          PIC 99V999.
           03  QIR-INSRATE-76          PIC 99V999.
           03  QIR-INSRATE-77          PIC 99V999.
           03  QIR-INSRATE-78          PIC 99V999.
           03  QIR-INSRATE-79          PIC 99V999.
           03  QIR-INSRATE-80         PIC 99V999.
           03  QIR-INSRATE-81          PIC 99V999.
           03  QIR-INSRATE-82          PIC 99V999.
           03  QIR-INSRATE-83          PIC 99V999.
           03  QIR-INSRATE-84          PIC 99V999.
           03  QIR-INSRATE-85          PIC 99V999.
           03  QIR-INSRATE-86          PIC 99V999.
           03  QIR-INSRATE-87          PIC 99V999.
           03  QIR-INSRATE-88          PIC 99V999.
           03  QIR-INSRATE-89          PIC 99V999.
           03  QIR-INSRATE-90         PIC 99V999.
           03  QIR-INSRATE-91          PIC 99V999.
           03  QIR-INSRATE-92          PIC 99V999.
           03  QIR-INSRATE-93          PIC 99V999.
           03  QIR-INSRATE-94          PIC 99V999.
           03  QIR-INSRATE-95          PIC 99V999.
           03  QIR-INSRATE-96          PIC 99V999.
           03  QIR-INSRATE-97          PIC 99V999.
           03  QIR-INSRATE-98          PIC 99V999.
           03  QIR-INSRATE-99          PIC 99V999.
           03  QIR-INSRATE-100         PIC 99V999.
           03  QIR-INSRATE-101         PIC 99V999.
           03  QIR-INSRATE-102         PIC 99V999.
           03  QIR-INSRATE-103         PIC 99V999.
           03  QIR-INSRATE-104         PIC 99V999.
           03  QIR-INSRATE-105         PIC 99V999.
           03  QIR-INSRATE-106         PIC 99V999.
           03  QIR-INSRATE-107         PIC 99V999.
           03  QIR-INSRATE-108         PIC 99V999.
           03  QIR-INSRATE-109         PIC 99V999.
           03  QIR-INSRATE-110         PIC 99V999.
           03  QIR-INSRATE-111         PIC 99V999.
           03  QIR-INSRATE-112         PIC 99V999.
           03  QIR-INSRATE-113         PIC 99V999.
           03  QIR-INSRATE-114         PIC 99V999.
           03  QIR-INSRATE-115         PIC 99V999.
           03  QIR-INSRATE-116         PIC 99V999.
           03  QIR-INSRATE-117         PIC 99V999.
           03  QIR-INSRATE-118         PIC 99V999.
           03  QIR-INSRATE-119         PIC 99V999.
           03  QIR-INSRATE-120         PIC 99V999.
           03  QIR-DESC               PIC X(40).
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

       01  LEGEND-BEG-KEY. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING KEY        (F3-ALL) ".

       01  LEGEND-END-KEY. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING KEY        ".

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
           03  BEG-KEY               PIC 9(03).
           03  END-KEY               PIC 9(03).

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
           03  FILLER PIC X(30) VALUE "KEY    PER    DESC".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-IR-KEY                     PIC 9(3).
           03  FILLER                         PIC X(05).
           03  D01-IR-PER-FLAG                PIC X.
           03  FILLER                         PIC X(05).
           03  D01-IR-DESC                    PIC X(40).
           03  FILLER                         PIC X(05).
           03  D01-IR-INSRATE-1               PIC 99.999.
           03  FILLER                         PIC X(05).
           03  D01-IR-INSRATE-10              PIC 99.999.
           03  FILLER                         PIC X(05).
           03  D01-IR-INSRATE-20              PIC 99.999.

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
       COPY "LIBIO/IOIRRN_DEF.CPY".
       COPY "LIBIO/IOIRRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOIRRN_SCN.CPY".

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
           MOVE "B"                  TO IR-PATH-OVERRIDE.

           MOVE BEG-KEY            TO QIR-WBEG-KEY.
           MOVE END-KEY            TO QIR-WEND-KEY.

           PERFORM OPEN-IR-FILE.
           PERFORM START-IR-FILE.

       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-IR-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( IR-KEY < BEG-KEY )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( IR-KEY > END-KEY )
              GO TO MAIN-PROGRAM-END.

      *-------------------------------------------------------{ RANGES }
           MOVE IR-KEY                     TO D01-IR-KEY.
           MOVE IR-PER-FG                  TO D01-IR-PER-FLAG.
           MOVE IR-DESC                    TO D01-IR-DESC.
           MOVE IR-INSRATE(1)              TO D01-IR-INSRATE-1.
           MOVE IR-INSRATE(10)             TO D01-IR-INSRATE-10.
           MOVE IR-INSRATE(20)             TO D01-IR-INSRATE-20.
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
           MOVE "IOIRRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOIRRN-QUEUE-POS  TO Q-POS.
           MOVE IOIRRN-COPIES-POS TO C-POS.
           MOVE IOIRRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-KEY.

           MOVE LEGEND-BEG-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N030BEG-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-KEY.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-KEY.

      *-----------------------------------------------------------------
       ENTER-END-KEY.

           MOVE LEGEND-END-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N030END-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-KEY.

           IF ( BEG-KEY > END-KEY )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-KEY.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-KEY.

           MOVE "S  N030BEG-KEY." TO VDU.
           MOVE 0                    TO BEG-KEY VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N030END-KEY." TO VDU.
           MOVE 999                  TO END-KEY VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-KEY.
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

           MOVE "S  N030BEG-KEY."    TO VDU.
           MOVE BEG-KEY              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N030END-KEY."    TO VDU.
           MOVE END-KEY              TO VDUNUM0.
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

           MOVE IR-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING KEY                 : "    TO R01-PROMPT.
           MOVE BEG-KEY                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    KEY                 : "    TO R01-PROMPT.
           MOVE END-KEY                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - IRFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOIRRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOIRRN-SYSDATE.
           DISPLAY IOIRRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOIRRN-FORM-FIELDS TO WR-BUF.
           MOVE IOIRRN-FORM-LIST TO WR-LIST.
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

           MOVE IOIRRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOIRRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOIRRN_EVA.CPY".
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
      *    LOAD IR-REC FROM QIR-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-IR-FIELDS.
           MOVE QIR-KEY TO IR-KEY.

           IF ( QIR-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO IR-LTOUCH-DATE
           ELSE
              MOVE QIR-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO IR-LTOUCH-DATE.

           MOVE QIR-PER-FG TO IR-PER-FG.
           MOVE QIR-INSRATE-1 TO IR-INSRATE(1).
           MOVE QIR-INSRATE-2 TO IR-INSRATE(2).
           MOVE QIR-INSRATE-3 TO IR-INSRATE(3).
           MOVE QIR-INSRATE-4 TO IR-INSRATE(4).
           MOVE QIR-INSRATE-5 TO IR-INSRATE(5).
           MOVE QIR-INSRATE-6 TO IR-INSRATE(6).
           MOVE QIR-INSRATE-7 TO IR-INSRATE(7).
           MOVE QIR-INSRATE-8 TO IR-INSRATE(8).
           MOVE QIR-INSRATE-9 TO IR-INSRATE(9).
           MOVE QIR-INSRATE-10 TO IR-INSRATE(10).
           MOVE QIR-INSRATE-11 TO IR-INSRATE(11).
           MOVE QIR-INSRATE-12 TO IR-INSRATE(12).
           MOVE QIR-INSRATE-13 TO IR-INSRATE(13).
           MOVE QIR-INSRATE-14 TO IR-INSRATE(14).
           MOVE QIR-INSRATE-15 TO IR-INSRATE(15).
           MOVE QIR-INSRATE-16 TO IR-INSRATE(16).
           MOVE QIR-INSRATE-17 TO IR-INSRATE(17).
           MOVE QIR-INSRATE-18 TO IR-INSRATE(18).
           MOVE QIR-INSRATE-19 TO IR-INSRATE(19).
           MOVE QIR-INSRATE-20 TO IR-INSRATE(20).
           MOVE QIR-INSRATE-21 TO IR-INSRATE(21).
           MOVE QIR-INSRATE-22 TO IR-INSRATE(22).
           MOVE QIR-INSRATE-23 TO IR-INSRATE(23).
           MOVE QIR-INSRATE-24 TO IR-INSRATE(24).
           MOVE QIR-INSRATE-25 TO IR-INSRATE(25).
           MOVE QIR-INSRATE-26 TO IR-INSRATE(26).
           MOVE QIR-INSRATE-27 TO IR-INSRATE(27).
           MOVE QIR-INSRATE-28 TO IR-INSRATE(28).
           MOVE QIR-INSRATE-29 TO IR-INSRATE(29).
           MOVE QIR-INSRATE-30 TO IR-INSRATE(30).
           MOVE QIR-INSRATE-31 TO IR-INSRATE(31).
           MOVE QIR-INSRATE-32 TO IR-INSRATE(32).
           MOVE QIR-INSRATE-33 TO IR-INSRATE(33).
           MOVE QIR-INSRATE-34 TO IR-INSRATE(34).
           MOVE QIR-INSRATE-35 TO IR-INSRATE(35).
           MOVE QIR-INSRATE-36 TO IR-INSRATE(36).
           MOVE QIR-INSRATE-37 TO IR-INSRATE(37).
           MOVE QIR-INSRATE-38 TO IR-INSRATE(38).
           MOVE QIR-INSRATE-39 TO IR-INSRATE(39).
           MOVE QIR-INSRATE-40 TO IR-INSRATE(40).
           MOVE QIR-INSRATE-41 TO IR-INSRATE(41).
           MOVE QIR-INSRATE-42 TO IR-INSRATE(42).
           MOVE QIR-INSRATE-43 TO IR-INSRATE(43).
           MOVE QIR-INSRATE-44 TO IR-INSRATE(44).
           MOVE QIR-INSRATE-45 TO IR-INSRATE(45).
           MOVE QIR-INSRATE-46 TO IR-INSRATE(46).
           MOVE QIR-INSRATE-47 TO IR-INSRATE(47).
           MOVE QIR-INSRATE-48 TO IR-INSRATE(48).
           MOVE QIR-INSRATE-49 TO IR-INSRATE(49).
           MOVE QIR-INSRATE-50 TO IR-INSRATE(50).
           MOVE QIR-INSRATE-51 TO IR-INSRATE(51).
           MOVE QIR-INSRATE-52 TO IR-INSRATE(52).
           MOVE QIR-INSRATE-53 TO IR-INSRATE(53).
           MOVE QIR-INSRATE-54 TO IR-INSRATE(54).
           MOVE QIR-INSRATE-55 TO IR-INSRATE(55).
           MOVE QIR-INSRATE-56 TO IR-INSRATE(56).
           MOVE QIR-INSRATE-57 TO IR-INSRATE(57).
           MOVE QIR-INSRATE-58 TO IR-INSRATE(58).
           MOVE QIR-INSRATE-59 TO IR-INSRATE(59).
           MOVE QIR-INSRATE-60 TO IR-INSRATE(60).
           MOVE QIR-INSRATE-61 TO IR-INSRATE(61).
           MOVE QIR-INSRATE-62 TO IR-INSRATE(62).
           MOVE QIR-INSRATE-63 TO IR-INSRATE(63).
           MOVE QIR-INSRATE-64 TO IR-INSRATE(64).
           MOVE QIR-INSRATE-65 TO IR-INSRATE(65).
           MOVE QIR-INSRATE-66 TO IR-INSRATE(66).
           MOVE QIR-INSRATE-67 TO IR-INSRATE(67).
           MOVE QIR-INSRATE-68 TO IR-INSRATE(68).
           MOVE QIR-INSRATE-69 TO IR-INSRATE(69).
           MOVE QIR-INSRATE-70 TO IR-INSRATE(70).
           MOVE QIR-INSRATE-71 TO IR-INSRATE(71).
           MOVE QIR-INSRATE-72 TO IR-INSRATE(72).
           MOVE QIR-INSRATE-73 TO IR-INSRATE(73).
           MOVE QIR-INSRATE-74 TO IR-INSRATE(74).
           MOVE QIR-INSRATE-75 TO IR-INSRATE(75).
           MOVE QIR-INSRATE-76 TO IR-INSRATE(76).
           MOVE QIR-INSRATE-77 TO IR-INSRATE(77).
           MOVE QIR-INSRATE-78 TO IR-INSRATE(78).
           MOVE QIR-INSRATE-79 TO IR-INSRATE(79).
           MOVE QIR-INSRATE-80 TO IR-INSRATE(80).
           MOVE QIR-INSRATE-81 TO IR-INSRATE(81).
           MOVE QIR-INSRATE-82 TO IR-INSRATE(82).
           MOVE QIR-INSRATE-83 TO IR-INSRATE(83).
           MOVE QIR-INSRATE-84 TO IR-INSRATE(84).
           MOVE QIR-INSRATE-85 TO IR-INSRATE(85).
           MOVE QIR-INSRATE-86 TO IR-INSRATE(86).
           MOVE QIR-INSRATE-87 TO IR-INSRATE(87).
           MOVE QIR-INSRATE-88 TO IR-INSRATE(88).
           MOVE QIR-INSRATE-89 TO IR-INSRATE(89).
           MOVE QIR-INSRATE-90 TO IR-INSRATE(90).
           MOVE QIR-INSRATE-91 TO IR-INSRATE(91).
           MOVE QIR-INSRATE-92 TO IR-INSRATE(92).
           MOVE QIR-INSRATE-93 TO IR-INSRATE(93).
           MOVE QIR-INSRATE-94 TO IR-INSRATE(94).
           MOVE QIR-INSRATE-95 TO IR-INSRATE(95).
           MOVE QIR-INSRATE-96 TO IR-INSRATE(96).
           MOVE QIR-INSRATE-97 TO IR-INSRATE(97).
           MOVE QIR-INSRATE-98 TO IR-INSRATE(98).
           MOVE QIR-INSRATE-99 TO IR-INSRATE(99).
           MOVE QIR-INSRATE-100 TO IR-INSRATE(100).
           MOVE QIR-INSRATE-101 TO IR-INSRATE(101).
           MOVE QIR-INSRATE-102 TO IR-INSRATE(102).
           MOVE QIR-INSRATE-103 TO IR-INSRATE(103).
           MOVE QIR-INSRATE-104 TO IR-INSRATE(104).
           MOVE QIR-INSRATE-105 TO IR-INSRATE(105).
           MOVE QIR-INSRATE-106 TO IR-INSRATE(106).
           MOVE QIR-INSRATE-107 TO IR-INSRATE(107).
           MOVE QIR-INSRATE-108 TO IR-INSRATE(108).
           MOVE QIR-INSRATE-109 TO IR-INSRATE(109).
           MOVE QIR-INSRATE-110 TO IR-INSRATE(110).
           MOVE QIR-INSRATE-111 TO IR-INSRATE(111).
           MOVE QIR-INSRATE-112 TO IR-INSRATE(112).
           MOVE QIR-INSRATE-113 TO IR-INSRATE(113).
           MOVE QIR-INSRATE-114 TO IR-INSRATE(114).
           MOVE QIR-INSRATE-115 TO IR-INSRATE(115).
           MOVE QIR-INSRATE-116 TO IR-INSRATE(116).
           MOVE QIR-INSRATE-117 TO IR-INSRATE(117).
           MOVE QIR-INSRATE-118 TO IR-INSRATE(118).
           MOVE QIR-INSRATE-119 TO IR-INSRATE(119).
           MOVE QIR-INSRATE-120 TO IR-INSRATE(120).
           MOVE QIR-DESC TO IR-DESC.
 
      *-----------------------------------------------------------------
      *    LOAD QIR-REC FROM IR-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-IR-FIELDS.
           MOVE IR-KEY TO QIR-KEY.

           IF ( IR-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QIR-LTOUCH-DATE
           ELSE
              MOVE IR-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QIR-LTOUCH-DATE.

           MOVE IR-PER-FG TO QIR-PER-FG.
           MOVE IR-INSRATE(1) TO QIR-INSRATE-1.
           MOVE IR-INSRATE(2) TO QIR-INSRATE-2.
           MOVE IR-INSRATE(3) TO QIR-INSRATE-3.
           MOVE IR-INSRATE(4) TO QIR-INSRATE-4.
           MOVE IR-INSRATE(5) TO QIR-INSRATE-5.
           MOVE IR-INSRATE(6) TO QIR-INSRATE-6.
           MOVE IR-INSRATE(7) TO QIR-INSRATE-7.
           MOVE IR-INSRATE(8) TO QIR-INSRATE-8.
           MOVE IR-INSRATE(9) TO QIR-INSRATE-9.
           MOVE IR-INSRATE(10) TO QIR-INSRATE-10.
           MOVE IR-INSRATE(11) TO QIR-INSRATE-11.
           MOVE IR-INSRATE(12) TO QIR-INSRATE-12.
           MOVE IR-INSRATE(13) TO QIR-INSRATE-13.
           MOVE IR-INSRATE(14) TO QIR-INSRATE-14.
           MOVE IR-INSRATE(15) TO QIR-INSRATE-15.
           MOVE IR-INSRATE(16) TO QIR-INSRATE-16.
           MOVE IR-INSRATE(17) TO QIR-INSRATE-17.
           MOVE IR-INSRATE(18) TO QIR-INSRATE-18.
           MOVE IR-INSRATE(19) TO QIR-INSRATE-19.
           MOVE IR-INSRATE(20) TO QIR-INSRATE-20.
           MOVE IR-INSRATE(21) TO QIR-INSRATE-21.
           MOVE IR-INSRATE(22) TO QIR-INSRATE-22.
           MOVE IR-INSRATE(23) TO QIR-INSRATE-23.
           MOVE IR-INSRATE(24) TO QIR-INSRATE-24.
           MOVE IR-INSRATE(25) TO QIR-INSRATE-25.
           MOVE IR-INSRATE(26) TO QIR-INSRATE-26.
           MOVE IR-INSRATE(27) TO QIR-INSRATE-27.
           MOVE IR-INSRATE(28) TO QIR-INSRATE-28.
           MOVE IR-INSRATE(29) TO QIR-INSRATE-29.
           MOVE IR-INSRATE(30) TO QIR-INSRATE-30.
           MOVE IR-INSRATE(31) TO QIR-INSRATE-31.
           MOVE IR-INSRATE(32) TO QIR-INSRATE-32.
           MOVE IR-INSRATE(33) TO QIR-INSRATE-33.
           MOVE IR-INSRATE(34) TO QIR-INSRATE-34.
           MOVE IR-INSRATE(35) TO QIR-INSRATE-35.
           MOVE IR-INSRATE(36) TO QIR-INSRATE-36.
           MOVE IR-INSRATE(37) TO QIR-INSRATE-37.
           MOVE IR-INSRATE(38) TO QIR-INSRATE-38.
           MOVE IR-INSRATE(39) TO QIR-INSRATE-39.
           MOVE IR-INSRATE(40) TO QIR-INSRATE-40.
           MOVE IR-INSRATE(41) TO QIR-INSRATE-41.
           MOVE IR-INSRATE(42) TO QIR-INSRATE-42.
           MOVE IR-INSRATE(43) TO QIR-INSRATE-43.
           MOVE IR-INSRATE(44) TO QIR-INSRATE-44.
           MOVE IR-INSRATE(45) TO QIR-INSRATE-45.
           MOVE IR-INSRATE(46) TO QIR-INSRATE-46.
           MOVE IR-INSRATE(47) TO QIR-INSRATE-47.
           MOVE IR-INSRATE(48) TO QIR-INSRATE-48.
           MOVE IR-INSRATE(49) TO QIR-INSRATE-49.
           MOVE IR-INSRATE(50) TO QIR-INSRATE-50.
           MOVE IR-INSRATE(51) TO QIR-INSRATE-51.
           MOVE IR-INSRATE(52) TO QIR-INSRATE-52.
           MOVE IR-INSRATE(53) TO QIR-INSRATE-53.
           MOVE IR-INSRATE(54) TO QIR-INSRATE-54.
           MOVE IR-INSRATE(55) TO QIR-INSRATE-55.
           MOVE IR-INSRATE(56) TO QIR-INSRATE-56.
           MOVE IR-INSRATE(57) TO QIR-INSRATE-57.
           MOVE IR-INSRATE(58) TO QIR-INSRATE-58.
           MOVE IR-INSRATE(59) TO QIR-INSRATE-59.
           MOVE IR-INSRATE(60) TO QIR-INSRATE-60.
           MOVE IR-INSRATE(61) TO QIR-INSRATE-61.
           MOVE IR-INSRATE(62) TO QIR-INSRATE-62.
           MOVE IR-INSRATE(63) TO QIR-INSRATE-63.
           MOVE IR-INSRATE(64) TO QIR-INSRATE-64.
           MOVE IR-INSRATE(65) TO QIR-INSRATE-65.
           MOVE IR-INSRATE(66) TO QIR-INSRATE-66.
           MOVE IR-INSRATE(67) TO QIR-INSRATE-67.
           MOVE IR-INSRATE(68) TO QIR-INSRATE-68.
           MOVE IR-INSRATE(69) TO QIR-INSRATE-69.
           MOVE IR-INSRATE(70) TO QIR-INSRATE-70.
           MOVE IR-INSRATE(71) TO QIR-INSRATE-71.
           MOVE IR-INSRATE(72) TO QIR-INSRATE-72.
           MOVE IR-INSRATE(73) TO QIR-INSRATE-73.
           MOVE IR-INSRATE(74) TO QIR-INSRATE-74.
           MOVE IR-INSRATE(75) TO QIR-INSRATE-75.
           MOVE IR-INSRATE(76) TO QIR-INSRATE-76.
           MOVE IR-INSRATE(77) TO QIR-INSRATE-77.
           MOVE IR-INSRATE(78) TO QIR-INSRATE-78.
           MOVE IR-INSRATE(79) TO QIR-INSRATE-79.
           MOVE IR-INSRATE(80) TO QIR-INSRATE-80.
           MOVE IR-INSRATE(81) TO QIR-INSRATE-81.
           MOVE IR-INSRATE(82) TO QIR-INSRATE-82.
           MOVE IR-INSRATE(83) TO QIR-INSRATE-83.
           MOVE IR-INSRATE(84) TO QIR-INSRATE-84.
           MOVE IR-INSRATE(85) TO QIR-INSRATE-85.
           MOVE IR-INSRATE(86) TO QIR-INSRATE-86.
           MOVE IR-INSRATE(87) TO QIR-INSRATE-87.
           MOVE IR-INSRATE(88) TO QIR-INSRATE-88.
           MOVE IR-INSRATE(89) TO QIR-INSRATE-89.
           MOVE IR-INSRATE(90) TO QIR-INSRATE-90.
           MOVE IR-INSRATE(91) TO QIR-INSRATE-91.
           MOVE IR-INSRATE(92) TO QIR-INSRATE-92.
           MOVE IR-INSRATE(93) TO QIR-INSRATE-93.
           MOVE IR-INSRATE(94) TO QIR-INSRATE-94.
           MOVE IR-INSRATE(95) TO QIR-INSRATE-95.
           MOVE IR-INSRATE(96) TO QIR-INSRATE-96.
           MOVE IR-INSRATE(97) TO QIR-INSRATE-97.
           MOVE IR-INSRATE(98) TO QIR-INSRATE-98.
           MOVE IR-INSRATE(99) TO QIR-INSRATE-99.
           MOVE IR-INSRATE(100) TO QIR-INSRATE-100.
           MOVE IR-INSRATE(101) TO QIR-INSRATE-101.
           MOVE IR-INSRATE(102) TO QIR-INSRATE-102.
           MOVE IR-INSRATE(103) TO QIR-INSRATE-103.
           MOVE IR-INSRATE(104) TO QIR-INSRATE-104.
           MOVE IR-INSRATE(105) TO QIR-INSRATE-105.
           MOVE IR-INSRATE(106) TO QIR-INSRATE-106.
           MOVE IR-INSRATE(107) TO QIR-INSRATE-107.
           MOVE IR-INSRATE(108) TO QIR-INSRATE-108.
           MOVE IR-INSRATE(109) TO QIR-INSRATE-109.
           MOVE IR-INSRATE(110) TO QIR-INSRATE-110.
           MOVE IR-INSRATE(111) TO QIR-INSRATE-111.
           MOVE IR-INSRATE(112) TO QIR-INSRATE-112.
           MOVE IR-INSRATE(113) TO QIR-INSRATE-113.
           MOVE IR-INSRATE(114) TO QIR-INSRATE-114.
           MOVE IR-INSRATE(115) TO QIR-INSRATE-115.
           MOVE IR-INSRATE(116) TO QIR-INSRATE-116.
           MOVE IR-INSRATE(117) TO QIR-INSRATE-117.
           MOVE IR-INSRATE(118) TO QIR-INSRATE-118.
           MOVE IR-INSRATE(119) TO QIR-INSRATE-119.
           MOVE IR-INSRATE(120) TO QIR-INSRATE-120.
           MOVE IR-DESC TO QIR-DESC.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-IR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IR-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IR-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO IR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO IR-ALL-MACHINE
              MOVE IR-ALL-PATH             TO IR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IR-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO IR-ALL-BASE
              MOVE FILPATH-MACHINE         TO IR-ALL-MACHINE
              MOVE IR-ALL-PATH             TO IR-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-IRFILE NOT = "S" )
              MOVE IR-PATH-BEG             TO IR-SELECT-PATH-BEG
              MOVE IR-PATH-FILE            TO IR-SELECT-PATH-FILE
           ELSE
              MOVE IR-PATH-SQL             TO IR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-IR-FILE.
           PERFORM LOAD-IR-FILE.
           PERFORM OPEN-IT.
           MOVE IR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-IR-FILE.
           PERFORM START-IT.
           MOVE IR-SELECT-PATH TO E-FILE.
           MOVE IR-KEY TO E-KEYX.

           IF ( IR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-IR-FILE.

           IF ( QIR-WEND-KEY = ZEROES    ) OR
              ( QIR-WEND-KEY NOT NUMERIC )
              MOVE IR-KEY               TO QIR-WBEG-KEY
              MOVE 999                  TO QIR-WEND-KEY.

           EXEC SQL
            DECLARE IR_CURSOR CURSOR FOR
            SELECT IRFILE.IR_KEY,
                   ISNULL(CAST(IRFILE.IR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(IRFILE.IR_PER_FG,''),
                   IRFILE.IR_INSRATE_1,
                   IRFILE.IR_INSRATE_2,
                   IRFILE.IR_INSRATE_3,
                   IRFILE.IR_INSRATE_4,
                   IRFILE.IR_INSRATE_5,
                   IRFILE.IR_INSRATE_6,
                   IRFILE.IR_INSRATE_7,
                   IRFILE.IR_INSRATE_8,
                   IRFILE.IR_INSRATE_9,
                   IRFILE.IR_INSRATE_10,
                   IRFILE.IR_INSRATE_11,
                   IRFILE.IR_INSRATE_12,
                   IRFILE.IR_INSRATE_13,
                   IRFILE.IR_INSRATE_14,
                   IRFILE.IR_INSRATE_15,
                   IRFILE.IR_INSRATE_16,
                   IRFILE.IR_INSRATE_17,
                   IRFILE.IR_INSRATE_18,
                   IRFILE.IR_INSRATE_19,
                   IRFILE.IR_INSRATE_20,
                   IRFILE.IR_INSRATE_21,
                   IRFILE.IR_INSRATE_22,
                   IRFILE.IR_INSRATE_23,
                   IRFILE.IR_INSRATE_24,
                   IRFILE.IR_INSRATE_25,
                   IRFILE.IR_INSRATE_26,
                   IRFILE.IR_INSRATE_27,
                   IRFILE.IR_INSRATE_28,
                   IRFILE.IR_INSRATE_29,
                   IRFILE.IR_INSRATE_30,
                   IRFILE.IR_INSRATE_31,
                   IRFILE.IR_INSRATE_32,
                   IRFILE.IR_INSRATE_33,
                   IRFILE.IR_INSRATE_34,
                   IRFILE.IR_INSRATE_35,
                   IRFILE.IR_INSRATE_36,
                   IRFILE.IR_INSRATE_37,
                   IRFILE.IR_INSRATE_38,
                   IRFILE.IR_INSRATE_39,
                   IRFILE.IR_INSRATE_40,
                   IRFILE.IR_INSRATE_41,
                   IRFILE.IR_INSRATE_42,
                   IRFILE.IR_INSRATE_43,
                   IRFILE.IR_INSRATE_44,
                   IRFILE.IR_INSRATE_45,
                   IRFILE.IR_INSRATE_46,
                   IRFILE.IR_INSRATE_47,
                   IRFILE.IR_INSRATE_48,
                   IRFILE.IR_INSRATE_49,
                   IRFILE.IR_INSRATE_50,
                   IRFILE.IR_INSRATE_51,
                   IRFILE.IR_INSRATE_52,
                   IRFILE.IR_INSRATE_53,
                   IRFILE.IR_INSRATE_54,
                   IRFILE.IR_INSRATE_55,
                   IRFILE.IR_INSRATE_56,
                   IRFILE.IR_INSRATE_57,
                   IRFILE.IR_INSRATE_58,
                   IRFILE.IR_INSRATE_59,
                   IRFILE.IR_INSRATE_60,
                   IRFILE.IR_INSRATE_61,
                   IRFILE.IR_INSRATE_62,
                   IRFILE.IR_INSRATE_63,
                   IRFILE.IR_INSRATE_64,
                   IRFILE.IR_INSRATE_65,
                   IRFILE.IR_INSRATE_66,
                   IRFILE.IR_INSRATE_67,
                   IRFILE.IR_INSRATE_68,
                   IRFILE.IR_INSRATE_69,
                   IRFILE.IR_INSRATE_70,
                   IRFILE.IR_INSRATE_71,
                   IRFILE.IR_INSRATE_72,
                   IRFILE.IR_INSRATE_73,
                   IRFILE.IR_INSRATE_74,
                   IRFILE.IR_INSRATE_75,
                   IRFILE.IR_INSRATE_76,
                   IRFILE.IR_INSRATE_77,
                   IRFILE.IR_INSRATE_78,
                   IRFILE.IR_INSRATE_79,
                   IRFILE.IR_INSRATE_80,
                   IRFILE.IR_INSRATE_81,
                   IRFILE.IR_INSRATE_82,
                   IRFILE.IR_INSRATE_83,
                   IRFILE.IR_INSRATE_84,
                   IRFILE.IR_INSRATE_85,
                   IRFILE.IR_INSRATE_86,
                   IRFILE.IR_INSRATE_87,
                   IRFILE.IR_INSRATE_88,
                   IRFILE.IR_INSRATE_89,
                   IRFILE.IR_INSRATE_90,
                   IRFILE.IR_INSRATE_91,
                   IRFILE.IR_INSRATE_92,
                   IRFILE.IR_INSRATE_93,
                   IRFILE.IR_INSRATE_94,
                   IRFILE.IR_INSRATE_95,
                   IRFILE.IR_INSRATE_96,
                   IRFILE.IR_INSRATE_97,
                   IRFILE.IR_INSRATE_98,
                   IRFILE.IR_INSRATE_99,
                   IRFILE.IR_INSRATE_100,
                   IRFILE.IR_INSRATE_101,
                   IRFILE.IR_INSRATE_102,
                   IRFILE.IR_INSRATE_103,
                   IRFILE.IR_INSRATE_104,
                   IRFILE.IR_INSRATE_105,
                   IRFILE.IR_INSRATE_106,
                   IRFILE.IR_INSRATE_107,
                   IRFILE.IR_INSRATE_108,
                   IRFILE.IR_INSRATE_109,
                   IRFILE.IR_INSRATE_110,
                   IRFILE.IR_INSRATE_111,
                   IRFILE.IR_INSRATE_112,
                   IRFILE.IR_INSRATE_113,
                   IRFILE.IR_INSRATE_114,
                   IRFILE.IR_INSRATE_115,
                   IRFILE.IR_INSRATE_116,
                   IRFILE.IR_INSRATE_117,
                   IRFILE.IR_INSRATE_118,
                   IRFILE.IR_INSRATE_119,
                   IRFILE.IR_INSRATE_120,
                   ISNULL(IRFILE.IR_DESC,'')
            FROM DBO.IRFILE
            WHERE IRFILE.IR_KEY >= :QIR-WBEG-KEY
              AND IRFILE.IR_KEY <= :QIR-WEND-KEY
            ORDER BY IRFILE.IR_KEY
           END-EXEC.

           EXEC SQL
               OPEN IR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QIR-WBEG-KEY
                          QIR-WEND-KEY.

           MOVE STAT---GOOD TO IR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-IR-FILE.
           PERFORM READ-IT.
           MOVE IR-SELECT-PATH TO E-FILE.
           MOVE IR-KEY TO E-KEYX.

           MOVE IR-KEY    TO QIR-KEY.  

           EXEC SQL
            SELECT IRFILE.IR_KEY,
                   ISNULL(CAST(IRFILE.IR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(IRFILE.IR_PER_FG,''),
                   IRFILE.IR_INSRATE_1,
                   IRFILE.IR_INSRATE_2,
                   IRFILE.IR_INSRATE_3,
                   IRFILE.IR_INSRATE_4,
                   IRFILE.IR_INSRATE_5,
                   IRFILE.IR_INSRATE_6,
                   IRFILE.IR_INSRATE_7,
                   IRFILE.IR_INSRATE_8,
                   IRFILE.IR_INSRATE_9,
                   IRFILE.IR_INSRATE_10,
                   IRFILE.IR_INSRATE_11,
                   IRFILE.IR_INSRATE_12,
                   IRFILE.IR_INSRATE_13,
                   IRFILE.IR_INSRATE_14,
                   IRFILE.IR_INSRATE_15,
                   IRFILE.IR_INSRATE_16,
                   IRFILE.IR_INSRATE_17,
                   IRFILE.IR_INSRATE_18,
                   IRFILE.IR_INSRATE_19,
                   IRFILE.IR_INSRATE_20,
                   IRFILE.IR_INSRATE_21,
                   IRFILE.IR_INSRATE_22,
                   IRFILE.IR_INSRATE_23,
                   IRFILE.IR_INSRATE_24,
                   IRFILE.IR_INSRATE_25,
                   IRFILE.IR_INSRATE_26,
                   IRFILE.IR_INSRATE_27,
                   IRFILE.IR_INSRATE_28,
                   IRFILE.IR_INSRATE_29,
                   IRFILE.IR_INSRATE_30,
                   IRFILE.IR_INSRATE_31,
                   IRFILE.IR_INSRATE_32,
                   IRFILE.IR_INSRATE_33,
                   IRFILE.IR_INSRATE_34,
                   IRFILE.IR_INSRATE_35,
                   IRFILE.IR_INSRATE_36,
                   IRFILE.IR_INSRATE_37,
                   IRFILE.IR_INSRATE_38,
                   IRFILE.IR_INSRATE_39,
                   IRFILE.IR_INSRATE_40,
                   IRFILE.IR_INSRATE_41,
                   IRFILE.IR_INSRATE_42,
                   IRFILE.IR_INSRATE_43,
                   IRFILE.IR_INSRATE_44,
                   IRFILE.IR_INSRATE_45,
                   IRFILE.IR_INSRATE_46,
                   IRFILE.IR_INSRATE_47,
                   IRFILE.IR_INSRATE_48,
                   IRFILE.IR_INSRATE_49,
                   IRFILE.IR_INSRATE_50,
                   IRFILE.IR_INSRATE_51,
                   IRFILE.IR_INSRATE_52,
                   IRFILE.IR_INSRATE_53,
                   IRFILE.IR_INSRATE_54,
                   IRFILE.IR_INSRATE_55,
                   IRFILE.IR_INSRATE_56,
                   IRFILE.IR_INSRATE_57,
                   IRFILE.IR_INSRATE_58,
                   IRFILE.IR_INSRATE_59,
                   IRFILE.IR_INSRATE_60,
                   IRFILE.IR_INSRATE_61,
                   IRFILE.IR_INSRATE_62,
                   IRFILE.IR_INSRATE_63,
                   IRFILE.IR_INSRATE_64,
                   IRFILE.IR_INSRATE_65,
                   IRFILE.IR_INSRATE_66,
                   IRFILE.IR_INSRATE_67,
                   IRFILE.IR_INSRATE_68,
                   IRFILE.IR_INSRATE_69,
                   IRFILE.IR_INSRATE_70,
                   IRFILE.IR_INSRATE_71,
                   IRFILE.IR_INSRATE_72,
                   IRFILE.IR_INSRATE_73,
                   IRFILE.IR_INSRATE_74,
                   IRFILE.IR_INSRATE_75,
                   IRFILE.IR_INSRATE_76,
                   IRFILE.IR_INSRATE_77,
                   IRFILE.IR_INSRATE_78,
                   IRFILE.IR_INSRATE_79,
                   IRFILE.IR_INSRATE_80,
                   IRFILE.IR_INSRATE_81,
                   IRFILE.IR_INSRATE_82,
                   IRFILE.IR_INSRATE_83,
                   IRFILE.IR_INSRATE_84,
                   IRFILE.IR_INSRATE_85,
                   IRFILE.IR_INSRATE_86,
                   IRFILE.IR_INSRATE_87,
                   IRFILE.IR_INSRATE_88,
                   IRFILE.IR_INSRATE_89,
                   IRFILE.IR_INSRATE_90,
                   IRFILE.IR_INSRATE_91,
                   IRFILE.IR_INSRATE_92,
                   IRFILE.IR_INSRATE_93,
                   IRFILE.IR_INSRATE_94,
                   IRFILE.IR_INSRATE_95,
                   IRFILE.IR_INSRATE_96,
                   IRFILE.IR_INSRATE_97,
                   IRFILE.IR_INSRATE_98,
                   IRFILE.IR_INSRATE_99,
                   IRFILE.IR_INSRATE_100,
                   IRFILE.IR_INSRATE_101,
                   IRFILE.IR_INSRATE_102,
                   IRFILE.IR_INSRATE_103,
                   IRFILE.IR_INSRATE_104,
                   IRFILE.IR_INSRATE_105,
                   IRFILE.IR_INSRATE_106,
                   IRFILE.IR_INSRATE_107,
                   IRFILE.IR_INSRATE_108,
                   IRFILE.IR_INSRATE_109,
                   IRFILE.IR_INSRATE_110,
                   IRFILE.IR_INSRATE_111,
                   IRFILE.IR_INSRATE_112,
                   IRFILE.IR_INSRATE_113,
                   IRFILE.IR_INSRATE_114,
                   IRFILE.IR_INSRATE_115,
                   IRFILE.IR_INSRATE_116,
                   IRFILE.IR_INSRATE_117,
                   IRFILE.IR_INSRATE_118,
                   IRFILE.IR_INSRATE_119,
                   IRFILE.IR_INSRATE_120,
                   ISNULL(IRFILE.IR_DESC,'')
            INTO :QIR-REC
            FROM DBO.IRFILE
            WHERE IRFILE.IR_KEY = :QIR-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IR-FIELDS.

      *-----------------------------------------------------------------
       READ-IR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE IR-SELECT-PATH TO E-FILE.
           MOVE IR-KEY TO E-KEYX.
           INITIALIZE QIR-REC.

           IF ( IR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH IR_CURSOR INTO :QIR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-IR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-IR-FILE.
           IF ( IR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE IR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO IR-CURSOR-STAT.

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
              DESTROY IOIRRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
