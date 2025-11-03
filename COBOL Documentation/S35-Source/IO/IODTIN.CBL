       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IODTIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  04-19-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IODTIN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : DTFILE 
      * TABLE DESC    : DEALER TRANSACTION FILE
      * OUTPUT FILE   : XPC/IODTIN.PRE
      * USED FOR      : LIBLP/LPWSDT.CPY  --- QDT-REC
      *                 LIBLP/LPDT1I.CPY  --- READ
      *                 LIBLP/LPDT1IN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPDT2IN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPDT3IN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPDT4IN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0419 ***NEW***
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
           "@(#)IO/IODTIN.CBL S34 10/25/21-14:01:39 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".

      *=================================================================
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LPWSDT.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QDT1-WHERE-BEG-KEY.
           03  QDT1-WBEG-BRNO              PIC 9(04).
           03  QDT1-WBEG-NUM               PIC 9(06).
           03  QDT1-WBEG-CLASS             PIC 9(03).
           03  QDT1-WBEG-ACCTNO            PIC 9(08).
           03  QDT1-WBEG-DATE              PIC X(10).
           03  QDT1-WBEG-SEQ               PIC 9(03).
 
       01  QDT1-WHERE-END-KEY.
           03  QDT1-WEND-BRNO              PIC 9(04).
           03  QDT1-WEND-NUM               PIC 9(06).
           03  QDT1-WEND-CLASS             PIC 9(03).
           03  QDT1-WEND-ACCTNO            PIC 9(08).
           03  QDT1-WEND-DATE              PIC X(10).
           03  QDT1-WEND-SEQ               PIC 9(03).
 
       01  QDT2-WHERE-BEG-KEY.
           03  QDT2-WBEG-BRNO              PIC 9(04).
           03  QDT2-WBEG-BATCHID           PIC 9(10).
           03  QDT2-WBEG-BATCHID-SEQ       PIC 9(05).

       01  QDT2-WHERE-END-KEY.
           03  QDT2-WEND-BRNO              PIC 9(04).
           03  QDT2-WEND-BATCHID           PIC 9(10).
           03  QDT2-WEND-BATCHID-SEQ       PIC 9(05).

       01  QDT3-WHERE-BEG-KEY.
           03  QDT3-WBEG-STATUS            PIC X.
           03  QDT3-WBEG-BRNO              PIC 9(04).
           03  QDT3-WBEG-NUM               PIC 9(06).
           03  QDT3-WBEG-CLASS             PIC 9(03).
           03  QDT3-WBEG-ACCTNO            PIC 9(08).
           03  QDT3-WBEG-DATE              PIC X(10).
           03  QDT3-WBEG-SEQ               PIC 9(03).
 
       01  QDT3-WHERE-END-KEY.
           03  QDT3-WEND-STATUS            PIC X.
           03  QDT3-WEND-BRNO              PIC 9(04).
           03  QDT3-WEND-NUM               PIC 9(06).
           03  QDT3-WEND-CLASS             PIC 9(03).
           03  QDT3-WEND-ACCTNO            PIC 9(08).
           03  QDT3-WEND-DATE              PIC X(10).
           03  QDT3-WEND-SEQ               PIC 9(03).
 
       01  QDT4-WHERE-BEG-KEY.
           03  QDT4-WBEG-BRNO              PIC 9(04).
           03  QDT4-WBEG-CKNO              PIC 9(06).

       01  QDT4-WHERE-END-KEY.
           03  QDT4-WEND-BRNO              PIC 9(04).
           03  QDT4-WEND-CKNO              PIC 9(06).

       01  QDT-REC.
           03  QDT-STATUS                PIC X.
           03  QDT-BRNO                  PIC 9(4).
           03  QDT-NUM                   PIC 9(6).
           03  QDT-CLASS                 PIC 9(3).
           03  QDT-ACCTNO                PIC 9(8).
           03  QDT-DATE                  PIC X(10).
           03  QDT-SEQ                   PIC 9(3).
           03  QDT-LTOUCH-DATE           PIC X(10).
           03  QDT-TYPE                  PIC X.
           03  QDT-LPSEQNO               PIC 9(5).
           03  QDT-TRCD                  PIC XX.
           03  QDT-DLRVPRIN-1            PIC S9(7)V99.
           03  QDT-DLRVPRIN-2            PIC S9(7)V99.
           03  QDT-DLRVPRIN-3            PIC S9(7)V99.
           03  QDT-DLRVCHGS-1            PIC S9(7)V99.
           03  QDT-DLRVCHGS-2            PIC S9(7)V99.
           03  QDT-DLRVCHGS-3            PIC S9(7)V99.
           03  QDT-DLPARVHELD            PIC S9(7)V99.
           03  QDT-DLPARVPAID            PIC S9(7)V99.
           03  QDT-PROCEEDS              PIC S9(7)V99.
           03  QDT-RELEASED              PIC S9(7)V99.
           03  QDT-PAYMENTS              PIC S9(7)V99.
           03  QDT-CKNO                  PIC 9(6).
           03  QDT-CKDATE                PIC X(10).
           03  QDT-BATCHID               PIC 9(10).
           03  QDT-BATCHID-SEQ           PIC 9(5).
           03  QDT-PAID                  PIC X.
           03  QDT-REVERSE               PIC X.
           03  QDT-REVERSE-TRCD          PIC XX.
           03  QDT-PURBAL                PIC S9(7)V99.
           03  QDT-APCUR                 PIC S9(7)V99.
           03  QDT-DLRVNO                PIC 9.
           03  QDT-SELECTED              PIC X.
           03  QDT-P4P5-TRCD             PIC XX.
           03  QDT-DLRVPRIN-PAY          PIC S9(7)V99.
           03  QDT-DLRVCHGS-PAY          PIC S9(7)V99.
           03  QDT-DLPARVHELD-PAY        PIC S9(7)V99.
           03  QDT-CHECK-VOID            PIC X.
           03  QDT-UNPAID                PIC X.
           03  QDT-MEMO                  PIC X.
           03  QDT-EVENT-CD              PIC X(2).
           03  QDT-PART-HELD-STATUS      PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.

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
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "NGE VALUE                     ".

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
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

           03  PROCESS-KEY             PIC 9(01).
           03  BEG-NUM                 PIC 9(06).
           03  END-NUM                 PIC 9(06).
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-SEQ                 PIC 9(03).
           03  END-SEQ                 PIC 9(03).
           03  BEG-BATCHID             PIC 9(10).
           03  END-BATCHID             PIC 9(10).
           03  BEG-BATCHSEQ            PIC 9(05).
           03  END-BATCHSEQ            PIC 9(05).
           03  BEG-STATUS              PIC X.
           03  END-STATUS              PIC X.
           03  BEG-CKNO                PIC 9(06).
           03  END-CKNO                PIC 9(06).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

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
           03  FILLER PIC X(30) VALUE "BRNO DLNO  CLASS ACCTNO DATE  ".
           03  FILLER PIC X(30) VALUE "SEQNO BATCHID BATCHSEQ STATUS ".
           03  FILLER PIC X(30) VALUE "CHECK NO                      ".
           03  FILLER PIC X(06) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-DT-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-DT-NUM                     PIC 9(06).
           03  FILLER                         PIC X(01).
           03  D01-DT-CLASS                   PIC 9(03).
           03  FILLER                         PIC X(01).
           03  D01-DT-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-DT-DATE                    PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-DT-SEQ                     PIC 9(03).
           03  FILLER                         PIC X(01).
           03  D01-DT-BATCHID                 PIC 9(10).
           03  FILLER                         PIC X(01).
           03  D01-DT-BATCHID-SEQ             PIC 9(05).
           03  FILLER                         PIC X(01).
           03  D01-DT-STATUS                  PIC X.
           03  FILLER                         PIC X(01).
           03  D01-DT-CKNO                    PIC 9(06).

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
      *COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
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
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IODTIN_DEF.CPY".
       COPY "LIBIO/IODTIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IODTIN_SCN.CPY".

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
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
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

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE SPACES TO SENDMESS-HEAD.

           STRING "PROGRAM COMPLETED AT ", TIME-EDIT
                  DELIMITED BY SIZE INTO SENDMESS-HEAD.

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
           MOVE "IODTIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IODTIN-QUEUE-POS  TO Q-POS.
           MOVE IODTIN-COPIES-POS TO C-POS.
           MOVE IODTIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TIME
              MOVE SPACES TO LEGEND
              STRING "STARTING TIME: ", TIME-EDIT
                     DELIMITED BY SIZE INTO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
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

           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
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
              GO TO ENTER-END-CKNO.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
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

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES ) OR
              ( VDUNUM0 > 4      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-NUM.

           MOVE "ENNN060BEG-NUM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-NUM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-NUM.

      *-----------------------------------------------------------------
       ENTER-END-NUM.

           MOVE "ENNN060END-NUM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NUM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-NUM.

           IF ( BEG-NUM > END-NUM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NUM.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-NUM.

           MOVE "SNNN060BEG-NUM." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-NUM.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUM." TO VDU.
           MOVE 999999              TO VDUNUM0  END-NUM.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-NUM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN030END-CLASS." TO VDU.
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

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE 999                 TO VDUNUM0  END-CLASS.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "ENNN080END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-DATE.

           MOVE "ENNN080BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ENNN080END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "SNNN080BEG-DATE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-DATE.
           PERFORM SCREENIO.

           MOVE "SNNN080END-DATE." TO VDU.
           MOVE ALL "9"             TO VDUNUM0  END-DATE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQ.

           MOVE "ENNN030BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE "ENNN030END-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQ.

           IF ( BEG-SEQ > END-SEQ )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ.

           GO TO ENTER-BEG-BATCHID.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SNNN030BEG-SEQ." TO VDU.
           MOVE ZEROES            TO VDUNUM0  BEG-SEQ.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQ." TO VDU.
           MOVE 999               TO VDUNUM0  END-SEQ.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-BATCHID.

           MOVE "ENNN100BEG-BATCHID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BATCHID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BATCHID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BATCHID.

      *-----------------------------------------------------------------
       ENTER-END-BATCHID.

           MOVE "ENNN030END-BATCHID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BATCHID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BATCHID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BATCHID.

           IF ( BEG-BATCHID > END-BATCHID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BATCHID.

           GO TO ENTER-BEG-BATCHID-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-BATCHID.

           MOVE "SNNN100BEG-BATCHID." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-BATCHID.
           PERFORM SCREENIO.

           MOVE "SNNN100END-BATCHID." TO VDU.
           MOVE ALL "9"             TO VDUNUM0  END-BATCHID.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-BATCHID-SEQ.

           MOVE "ENNN050BEG-BATCHSEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BATCHID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BATCHID-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BATCHID-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BATCHSEQ.

      *-----------------------------------------------------------------
       ENTER-END-BATCHID-SEQ.

           MOVE "ENNN050END-BATCHSEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BATCHID-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BATCHID-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BATCHSEQ.

           IF ( BEG-BATCHSEQ > END-BATCHSEQ )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BATCHID-SEQ.

           GO TO ENTER-BEG-STATUS.

      *-----------------------------------------------------------------
       ENTER-ALL-BATCHID-SEQ.

           MOVE "SNNN050BEG-BATCHSEQ." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-BATCHSEQ.
           PERFORM SCREENIO.

           MOVE "SNNN050END-BATCHSEQ." TO VDU.
           MOVE 99999               TO VDUNUM0  END-BATCHSEQ.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-STATUS.

           MOVE "E  A010BEG-STATUS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BATCHID-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-STATUS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-STATUS.

      *-----------------------------------------------------------------
       ENTER-END-STATUS.

           MOVE "E  A010END-STATUS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-STATUS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-STATUS.

           IF ( BEG-STATUS > END-STATUS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-STATUS.

           GO TO ENTER-BEG-CKNO.

      *-----------------------------------------------------------------
       ENTER-ALL-STATUS.

           MOVE "S  A010BEG-STATUS." TO VDU.
           MOVE SPACES               TO VDUBUF  BEG-STATUS.
           PERFORM SCREENIO.

           MOVE "S  A010END-STATUS." TO VDU.
           MOVE ALL "Z"              TO VDUBUF  END-STATUS.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CKNO.

           MOVE "ENNN060BEG-CKNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-STATUS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CKNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CKNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CKNO.

      *-----------------------------------------------------------------
       ENTER-END-CKNO.

           MOVE "ENNN060END-CKNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CKNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CKNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CKNO.

           IF ( BEG-CKNO > END-CKNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CKNO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-CKNO.

           MOVE "SNNN060BEG-CKNO." TO VDU.
           MOVE ZEROES             TO VDUNUM0  BEG-CKNO.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO." TO VDU.
           MOVE 999999             TO VDUNUM0  END-CKNO.
           PERFORM SCREENIO.

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

           MOVE "SNNN060BEG-NUM."         TO VDU.
           MOVE BEG-NUM                   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUM."         TO VDU.
           MOVE END-NUM                   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-DATE."        TO VDU.
           MOVE BEG-DATE                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-DATE."        TO VDU.
           MOVE END-DATE                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-SEQ."         TO VDU.
           MOVE BEG-SEQ                   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQ."         TO VDU.
           MOVE END-SEQ                   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN100BEG-BATCHID."     TO VDU.
           MOVE BEG-BATCHID               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN100END-BATCHID."     TO VDU.
           MOVE END-BATCHID               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050BEG-BATCHSEQ."    TO VDU.
           MOVE BEG-BATCHSEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050END-BATCHSEQ."    TO VDU.
           MOVE END-BATCHSEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010BEG-STATUS."      TO VDU.
           MOVE BEG-STATUS                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-STATUS."      TO VDU.
           MOVE END-STATUS                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN060BEG-CKNO."        TO VDU.
           MOVE BEG-CKNO                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO."        TO VDU.
           MOVE END-CKNO                  TO VDUNUM0.
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
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.

           MOVE GRSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO GRSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VALID SELECTION  }
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE PACE-COUNTER-MAX TO PACE-COUNTER.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DT-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO          TO QDT1-WBEG-BRNO 
                                   QDT2-WBEG-BRNO 
                                   QDT3-WBEG-BRNO
                                   QDT4-WBEG-BRNO.

           MOVE BR-NO          TO QDT1-WEND-BRNO
                                   QDT2-WEND-BRNO 
                                   QDT3-WEND-BRNO
                                   QDT4-WEND-BRNO.

           MOVE BEG-NUM         TO QDT1-WBEG-NUM 
                                   QDT3-WBEG-NUM.

           MOVE END-NUM         TO QDT1-WEND-NUM
                                   QDT3-WEND-NUM.

           MOVE BEG-CLASS       TO QDT1-WBEG-CLASS
                                   QDT3-WBEG-CLASS.

           MOVE END-CLASS       TO QDT1-WEND-CLASS
                                   QDT3-WEND-CLASS.

           MOVE BEG-ACCTNO      TO QDT1-WBEG-ACCTNO
                                   QDT3-WBEG-ACCTNO.

           MOVE END-ACCTNO      TO QDT1-WEND-ACCTNO
                                   QDT3-WEND-ACCTNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE
                                        QDT3-WBEG-DATE.

           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE
                                        QDT3-WEND-DATE.

           MOVE BEG-SEQ         TO QDT1-WBEG-SEQ
                                   QDT3-WBEG-SEQ.

           MOVE END-SEQ         TO QDT1-WEND-SEQ
                                   QDT3-WEND-SEQ.

           MOVE BEG-BATCHID     TO QDT2-WBEG-BATCHID.
           MOVE END-BATCHID     TO QDT2-WEND-BATCHID.

           MOVE BEG-BATCHSEQ    TO QDT2-WBEG-BATCHID-SEQ.
           MOVE END-BATCHSEQ    TO QDT2-WEND-BATCHID-SEQ.

           MOVE BEG-STATUS      TO QDT3-WBEG-STATUS.
           MOVE END-STATUS      TO QDT3-WEND-STATUS.

           MOVE BEG-CKNO        TO QDT4-WBEG-CKNO.
           MOVE END-CKNO        TO QDT4-WEND-CKNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-DT1-FILE
              PERFORM START-DT1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-DT2-FILE
              PERFORM START-DT2-FILE
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM OPEN-DT3-FILE
              PERFORM START-DT3-FILE
           ELSE
           IF ( PROCESS-KEY = 4 )
              PERFORM OPEN-DT4-FILE
              PERFORM START-DT4-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-DT1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-DT2-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM READ-DT3-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 4 )
              PERFORM READ-DT4-FILE-NEXT.

           IF ( IO-BAD ) OR ( DT-BRNO NOT = DT-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( DT-NUM  < BEG-NUM )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-NUM > END-NUM )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
              IF ( DT-CLASS  < BEG-CLASS  ) OR
                 ( DT-CLASS  > END-CLASS  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-ACCTNO < BEG-ACCTNO ) OR
                 ( DT-ACCTNO > END-ACCTNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-DATE < BEG-DATE ) OR
                 ( DT-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-SEQ < BEG-SEQ ) OR
                 ( DT-SEQ > END-SEQ )
                 GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( DT-CKNO < BEG-CKNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-CKNO > END-CKNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 3 )
              IF ( DT-STATUS < BEG-STATUS )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-STATUS > END-STATUS )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
              IF ( DT-NUM  < BEG-NUM ) OR
                 ( DT-NUM  > END-NUM )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-CLASS  < BEG-CLASS  ) OR
                 ( DT-CLASS  > END-CLASS  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-ACCTNO < BEG-ACCTNO ) OR
                 ( DT-ACCTNO > END-ACCTNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-DATE < BEG-DATE ) OR
                 ( DT-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-SEQ < BEG-SEQ ) OR
                 ( DT-SEQ > END-SEQ )
                 GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 4 )
              IF ( DT-CKNO < BEG-CKNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DT-CKNO > END-CKNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE DT-BRNO                    TO D01-DT-BRNO.
           MOVE DT-NUM                     TO D01-DT-NUM.
           MOVE DT-CLASS                   TO D01-DT-CLASS.
           MOVE DT-ACCTNO                  TO D01-DT-ACCTNO.
           MOVE DT-DATE                    TO D01-DT-DATE.
           MOVE DT-SEQ                     TO D01-DT-SEQ.
           MOVE DT-BATCHID                 TO D01-DT-BATCHID.
           MOVE DT-BATCHID-SEQ             TO D01-DT-BATCHID-SEQ.
           MOVE DT-STATUS                  TO D01-DT-STATUS.
           MOVE DT-CKNO                    TO D01-DT-CKNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-DT1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-DT2-FILE
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM CLOSE-DT3-FILE
           ELSE
           IF ( PROCESS-KEY = 4 )
              PERFORM CLOSE-DT4-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
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
              MOVE DT1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 3 )
              MOVE DT3-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 4 )
              STRING DT-BRNO, DT-CKNO 
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING NUM                 : "    TO R01-PROMPT.
           MOVE BEG-NUM                               TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    NUM                 : "    TO R01-PROMPT.
           MOVE END-NUM                               TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE                : "    TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE                : "    TO R01-PROMPT.
           MOVE END-DATE                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SEQ NO              : "    TO R01-PROMPT.
           MOVE BEG-SEQ                               TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQ NO              : "    TO R01-PROMPT.
           MOVE END-SEQ                               TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BATCHID             : "    TO R01-PROMPT.
           MOVE BEG-BATCHID                           TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BATCHID             : "    TO R01-PROMPT.
           MOVE END-BATCHID                           TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BATCH ID SEQ        : "    TO R01-PROMPT.
           MOVE BEG-BATCHSEQ                          TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BATCH ID SEQ        : "    TO R01-PROMPT.
           MOVE END-BATCHSEQ                          TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING STATUS              : "    TO R01-PROMPT.
           MOVE BEG-STATUS                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    STATUS              : "    TO R01-PROMPT.
           MOVE END-STATUS                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CKNO                : "    TO R01-PROMPT.
           MOVE BEG-CKNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CKNO                : "    TO R01-PROMPT.
           MOVE END-CKNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
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
           MOVE "SQL TABLE IO UTILITY - DTFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IODTIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IODTIN-SYSDATE.
           DISPLAY IODTIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IODTIN-FORM-FIELDS TO WR-BUF.
           MOVE IODTIN-FORM-LIST TO WR-LIST.
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

           MOVE IODTIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IODTIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IODTIN_EVA.CPY".
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
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/RMFILE.CPY".
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
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD DT-REC FROM QDT-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-DT-FIELDS.
           MOVE QDT-STATUS TO DT-STATUS.
           MOVE QDT-BRNO TO DT-BRNO.
           MOVE QDT-NUM TO DT-NUM.
           MOVE QDT-CLASS TO DT-CLASS.
           MOVE QDT-ACCTNO TO DT-ACCTNO.

           IF ( QDT-DATE = "1900-01-01" )
              MOVE ZEROES TO DT-DATE
           ELSE
              MOVE QDT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DT-DATE.

           MOVE QDT-SEQ TO DT-SEQ.

           IF ( QDT-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO DT-LTOUCH-DATE
           ELSE
              MOVE QDT-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DT-LTOUCH-DATE.

           MOVE QDT-TYPE TO DT-TYPE.
           MOVE QDT-LPSEQNO TO DT-LPSEQNO.
           MOVE QDT-TRCD TO DT-TRCD.
           MOVE QDT-DLRVPRIN-1 TO DT-DLRVPRIN(1).
           MOVE QDT-DLRVPRIN-2 TO DT-DLRVPRIN(2).
           MOVE QDT-DLRVPRIN-3 TO DT-DLRVPRIN(3).
           MOVE QDT-DLRVCHGS-1 TO DT-DLRVCHGS(1).
           MOVE QDT-DLRVCHGS-2 TO DT-DLRVCHGS(2).
           MOVE QDT-DLRVCHGS-3 TO DT-DLRVCHGS(3).
           MOVE QDT-DLPARVHELD TO DT-DLPARVHELD.
           MOVE QDT-DLPARVPAID TO DT-DLPARVPAID.
           MOVE QDT-PROCEEDS TO DT-PROCEEDS.
           MOVE QDT-RELEASED TO DT-RELEASED.
           MOVE QDT-PAYMENTS TO DT-PAYMENTS.
           MOVE QDT-CKNO TO DT-CKNO.

           IF ( QDT-CKDATE = "1900-01-01" )
              MOVE ZEROES TO DT-CKDATE
           ELSE
              MOVE QDT-CKDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DT-CKDATE.

           MOVE QDT-BATCHID TO DT-BATCHID.
           MOVE QDT-BATCHID-SEQ TO DT-BATCHID-SEQ.
           MOVE QDT-PAID TO DT-PAID.
           MOVE QDT-REVERSE TO DT-REVERSE.
           MOVE QDT-REVERSE-TRCD TO DT-REVERSE-TRCD.
           MOVE QDT-PURBAL TO DT-PURBAL.
           MOVE QDT-APCUR TO DT-APCUR.
           MOVE QDT-DLRVNO TO DT-DLRVNO.
           MOVE QDT-SELECTED TO DT-SELECTED.
           MOVE QDT-P4P5-TRCD TO DT-P4P5-TRCD.
           MOVE QDT-DLRVPRIN-PAY TO DT-DLRVPRIN-PAY.
           MOVE QDT-DLRVCHGS-PAY TO DT-DLRVCHGS-PAY.
           MOVE QDT-DLPARVHELD-PAY TO DT-DLPARVHELD-PAY.
           MOVE QDT-CHECK-VOID TO DT-CHECK-VOID.
           MOVE QDT-UNPAID TO DT-UNPAID.
           MOVE QDT-MEMO TO DT-MEMO.
           MOVE QDT-EVENT-CD TO DT-EVENT-CD.
           MOVE QDT-PART-HELD-STATUS TO DT-PART-HELD-STATUS.
 
      *-----------------------------------------------------------------
      *    LOAD QDT-REC FROM DT-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-DT-FIELDS.
           MOVE DT-STATUS TO QDT-STATUS.
           MOVE DT-BRNO TO QDT-BRNO.
           MOVE DT-NUM TO QDT-NUM.
           MOVE DT-CLASS TO QDT-CLASS.
           MOVE DT-ACCTNO TO QDT-ACCTNO.

           IF ( DT-DATE = ZEROES )
              MOVE "1900-01-01" TO QDT-DATE
           ELSE
              MOVE DT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDT-DATE.

           MOVE DT-SEQ TO QDT-SEQ.

           IF ( DT-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QDT-LTOUCH-DATE
           ELSE
              MOVE DT-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDT-LTOUCH-DATE.

           MOVE DT-TYPE TO QDT-TYPE.
           MOVE DT-LPSEQNO TO QDT-LPSEQNO.
           MOVE DT-TRCD TO QDT-TRCD.
           MOVE DT-DLRVPRIN(1) TO QDT-DLRVPRIN-1.
           MOVE DT-DLRVPRIN(2) TO QDT-DLRVPRIN-2.
           MOVE DT-DLRVPRIN(3) TO QDT-DLRVPRIN-3.
           MOVE DT-DLRVCHGS(1) TO QDT-DLRVCHGS-1.
           MOVE DT-DLRVCHGS(2) TO QDT-DLRVCHGS-2.
           MOVE DT-DLRVCHGS(3) TO QDT-DLRVCHGS-3.
           MOVE DT-DLPARVHELD TO QDT-DLPARVHELD.
           MOVE DT-DLPARVPAID TO QDT-DLPARVPAID.
           MOVE DT-PROCEEDS TO QDT-PROCEEDS.
           MOVE DT-RELEASED TO QDT-RELEASED.
           MOVE DT-PAYMENTS TO QDT-PAYMENTS.
           MOVE DT-CKNO TO QDT-CKNO.

           IF ( DT-CKDATE = ZEROES )
              MOVE "1900-01-01" TO QDT-CKDATE
           ELSE
              MOVE DT-CKDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDT-CKDATE.

           MOVE DT-BATCHID TO QDT-BATCHID.
           MOVE DT-BATCHID-SEQ TO QDT-BATCHID-SEQ.
           MOVE DT-PAID TO QDT-PAID.
           MOVE DT-REVERSE TO QDT-REVERSE.
           MOVE DT-REVERSE-TRCD TO QDT-REVERSE-TRCD.
           MOVE DT-PURBAL TO QDT-PURBAL.
           MOVE DT-APCUR TO QDT-APCUR.
           MOVE DT-DLRVNO TO QDT-DLRVNO.
           MOVE DT-SELECTED TO QDT-SELECTED.
           MOVE DT-P4P5-TRCD TO QDT-P4P5-TRCD.
           MOVE DT-DLRVPRIN-PAY TO QDT-DLRVPRIN-PAY.
           MOVE DT-DLRVCHGS-PAY TO QDT-DLRVCHGS-PAY.
           MOVE DT-DLPARVHELD-PAY TO QDT-DLPARVHELD-PAY.
           MOVE DT-CHECK-VOID TO QDT-CHECK-VOID.
           MOVE DT-UNPAID TO QDT-UNPAID.
           MOVE DT-MEMO TO QDT-MEMO.
           MOVE DT-EVENT-CD TO QDT-EVENT-CD.
           MOVE DT-PART-HELD-STATUS TO QDT-PART-HELD-STATUS.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBLP/LPDT1R
      * COPYMEMBER: LIBLP/LPDT1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-DT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DT-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DT-ALL-BASE
              MOVE FILPATH-MACHINE         TO DT-ALL-MACHINE
              MOVE FILPATH-DATA            TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-DTFILE NOT = "S" )
              MOVE DT-PATH-BEG             TO DT-SELECT-PATH-BEG
              MOVE DT-PATH-FILE            TO DT-SELECT-PATH-FILE
           ELSE
              MOVE DT-PATH-SQL             TO DT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-DT1-FILE.
           PERFORM LOAD-DT1-FILE.
           PERFORM OPEN-IT.
           MOVE DT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DT1-FILE.
           PERFORM START-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.

           IF ( DT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DT1-FILE.

           IF ( QDT1-WEND-SEQ = ZEROES    ) OR
              ( QDT1-WEND-SEQ NOT NUMERIC )
              MOVE DT-BRNO              TO QDT1-WBEG-BRNO
              MOVE DT-BRNO              TO QDT1-WEND-BRNO
              MOVE DT-NUM               TO QDT1-WBEG-NUM 
              MOVE ALL "9"              TO QDT1-WEND-NUM 
              MOVE DT-CLASS             TO QDT1-WBEG-CLASS
              MOVE ALL "9"              TO QDT1-WEND-CLASS
              MOVE DT-ACCTNO            TO QDT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QDT1-WEND-ACCTNO
              MOVE DT-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE
              MOVE DT-SEQ               TO QDT1-WBEG-SEQ
              MOVE ALL "9"              TO QDT1-WEND-SEQ.

           EXEC SQL
            DECLARE DT1_CURSOR CURSOR FOR
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            FROM DBO.DTFILE
            WHERE DTFILE.DT_BRNO >= :QDT1-WBEG-BRNO
              AND DTFILE.DT_BRNO <= :QDT1-WEND-BRNO
              AND DTFILE.DT_NUM >= :QDT1-WBEG-NUM 
              AND DTFILE.DT_NUM <= :QDT1-WEND-NUM 
              AND DTFILE.DT_CLASS >= :QDT1-WBEG-CLASS
              AND DTFILE.DT_CLASS <= :QDT1-WEND-CLASS
              AND DTFILE.DT_ACCTNO >= :QDT1-WBEG-ACCTNO
              AND DTFILE.DT_ACCTNO <= :QDT1-WEND-ACCTNO
              AND CAST(DTFILE.DT_DATE AS VARCHAR(10))
                          >= :QDT1-WBEG-DATE
              AND CAST(DTFILE.DT_DATE AS VARCHAR(10))
                          <= :QDT1-WEND-DATE
              AND DTFILE.DT_SEQ >= :QDT1-WBEG-SEQ
              AND DTFILE.DT_SEQ <= :QDT1-WEND-SEQ
            ORDER BY DTFILE.DT_BRNO,
                     DTFILE.DT_NUM,
                     DTFILE.DT_CLASS,
                     DTFILE.DT_ACCTNO,
                     DTFILE.DT_DATE,
                     DTFILE.DT_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDT1-WBEG-BRNO 
                          QDT1-WEND-BRNO
                          QDT1-WBEG-NUM
                          QDT1-WEND-NUM
                          QDT1-WBEG-CLASS
                          QDT1-WEND-CLASS
                          QDT1-WBEG-ACCTNO
                          QDT1-WEND-ACCTNO
                          QDT1-WBEG-SEQ
                          QDT1-WEND-SEQ.
 
           MOVE SPACES TO QDT1-WBEG-DATE
                          QDT1-WEND-DATE.

           MOVE STAT---GOOD TO DT1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DT1-FILE.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO. 
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.

           MOVE DT-BRNO   TO QDT-BRNO.
           MOVE DT-NUM    TO QDT-NUM.
           MOVE DT-CLASS  TO QDT-CLASS.
           MOVE DT-ACCTNO TO QDT-ACCTNO.
           MOVE DT-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QDT-DATE.
           MOVE DT-SEQ    TO QDT-SEQ.

           EXEC SQL
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            INTO :QDT-REC
            FROM DBO.DTFILE
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
              AND DTFILE.DT_NUM = :QDT-NUM
              AND DTFILE.DT_CLASS = :QDT-CLASS
              AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
              AND DTFILE.DT_DATE = :QDT-DATE
              AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       READ-DT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.
           INITIALIZE QDT-REC.

           IF ( DT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DT1_CURSOR INTO :QDT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       WRITE-DT1-FILE.

           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           ACCEPT DT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.
           PERFORM SET-DT-FIELDS.

           EXEC SQL
            INSERT INTO DBO.DTFILE
             (DTFILE.DT_STATUS,
              DTFILE.DT_BRNO,
              DTFILE.DT_NUM,
              DTFILE.DT_CLASS,
              DTFILE.DT_ACCTNO,
              DTFILE.DT_DATE,
              DTFILE.DT_SEQ,
              DTFILE.DT_LTOUCH_DATE,
              DTFILE.DT_TYPE,
              DTFILE.DT_LPSEQNO,
              DTFILE.DT_TRCD,
              DTFILE.DT_DLRVPRIN_1,
              DTFILE.DT_DLRVPRIN_2,
              DTFILE.DT_DLRVPRIN_3,
              DTFILE.DT_DLRVCHGS_1,
              DTFILE.DT_DLRVCHGS_2,
              DTFILE.DT_DLRVCHGS_3,
              DTFILE.DT_DLPARVHELD,
              DTFILE.DT_DLPARVPAID,
              DTFILE.DT_PROCEEDS,
              DTFILE.DT_RELEASED,
              DTFILE.DT_PAYMENTS,
              DTFILE.DT_CKNO,
              DTFILE.DT_CKDATE,
              DTFILE.DT_BATCHID,
              DTFILE.DT_BATCHID_SEQ,
              DTFILE.DT_PAID,
              DTFILE.DT_REVERSE,
              DTFILE.DT_REVERSE_TRCD,
              DTFILE.DT_PURBAL,
              DTFILE.DT_APCUR,
              DTFILE.DT_DLRVNO,
              DTFILE.DT_SELECTED,
              DTFILE.DT_P4P5_TRCD,
              DTFILE.DT_DLRVPRIN_PAY,
              DTFILE.DT_DLRVCHGS_PAY,
              DTFILE.DT_DLPARVHELD_PAY,
              DTFILE.DT_CHECK_VOID,
              DTFILE.DT_UNPAID,
              DTFILE.DT_MEMO,
              DTFILE.DT_EVENT_CD,
              DTFILE.DT_PART_HELD_STATUS)
            VALUES
             (:QDT-STATUS,
              :QDT-BRNO,
              :QDT-NUM,
              :QDT-CLASS,
              :QDT-ACCTNO,
              NULLIF(:QDT-DATE,'1900-01-01'),
              :QDT-SEQ,
              NULLIF(:QDT-LTOUCH-DATE,'1900-01-01'),
              :QDT-TYPE,
              :QDT-LPSEQNO,
              :QDT-TRCD,
              :QDT-DLRVPRIN-1,
              :QDT-DLRVPRIN-2,
              :QDT-DLRVPRIN-3,
              :QDT-DLRVCHGS-1,
              :QDT-DLRVCHGS-2,
              :QDT-DLRVCHGS-3,
              :QDT-DLPARVHELD,
              :QDT-DLPARVPAID,
              :QDT-PROCEEDS,
              :QDT-RELEASED,
              :QDT-PAYMENTS,
              :QDT-CKNO,
              NULLIF(:QDT-CKDATE,'1900-01-01'),
              :QDT-BATCHID,
              :QDT-BATCHID-SEQ,
              :QDT-PAID,
              :QDT-REVERSE,
              :QDT-REVERSE-TRCD,
              :QDT-PURBAL,
              :QDT-APCUR,
              :QDT-DLRVNO,
              :QDT-SELECTED,
              :QDT-P4P5-TRCD,
              :QDT-DLRVPRIN-PAY,
              :QDT-DLRVCHGS-PAY,
              :QDT-DLPARVHELD-PAY,
              :QDT-CHECK-VOID,
              :QDT-UNPAID,
              :QDT-MEMO,
              :QDT-EVENT-CD,
              :QDT-PART-HELD-STATUS)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-DT1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-DT1-FILE.

           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           ACCEPT DT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.
           PERFORM SET-DT-FIELDS.

           EXEC SQL
            UPDATE DBO.DTFILE
             SET
              DTFILE.DT_STATUS = :QDT-STATUS,
              DTFILE.DT_BRNO = :QDT-BRNO,
              DTFILE.DT_NUM = :QDT-NUM,
              DTFILE.DT_CLASS = :QDT-CLASS,
              DTFILE.DT_ACCTNO = :QDT-ACCTNO,
              DTFILE.DT_DATE =
                 NULLIF(:QDT-DATE,'1900-01-01'),
              DTFILE.DT_SEQ = :QDT-SEQ,
              DTFILE.DT_LTOUCH_DATE =
                 NULLIF(:QDT-LTOUCH-DATE,'1900-01-01'),
              DTFILE.DT_TYPE = :QDT-TYPE,
              DTFILE.DT_LPSEQNO = :QDT-LPSEQNO,
              DTFILE.DT_TRCD = :QDT-TRCD,
              DTFILE.DT_DLRVPRIN_1 = :QDT-DLRVPRIN-1,
              DTFILE.DT_DLRVPRIN_2 = :QDT-DLRVPRIN-2,
              DTFILE.DT_DLRVPRIN_3 = :QDT-DLRVPRIN-3,
              DTFILE.DT_DLRVCHGS_1 = :QDT-DLRVCHGS-1,
              DTFILE.DT_DLRVCHGS_2 = :QDT-DLRVCHGS-2,
              DTFILE.DT_DLRVCHGS_3 = :QDT-DLRVCHGS-3,
              DTFILE.DT_DLPARVHELD = :QDT-DLPARVHELD,
              DTFILE.DT_DLPARVPAID = :QDT-DLPARVPAID,
              DTFILE.DT_PROCEEDS = :QDT-PROCEEDS,
              DTFILE.DT_RELEASED = :QDT-RELEASED,
              DTFILE.DT_PAYMENTS = :QDT-PAYMENTS,
              DTFILE.DT_CKNO = :QDT-CKNO,
              DTFILE.DT_CKDATE =
                 NULLIF(:QDT-CKDATE,'1900-01-01'),
              DTFILE.DT_BATCHID = :QDT-BATCHID,
              DTFILE.DT_BATCHID_SEQ = :QDT-BATCHID-SEQ,
              DTFILE.DT_PAID = :QDT-PAID,
              DTFILE.DT_REVERSE = :QDT-REVERSE,
              DTFILE.DT_REVERSE_TRCD = :QDT-REVERSE-TRCD,
              DTFILE.DT_PURBAL = :QDT-PURBAL,
              DTFILE.DT_APCUR = :QDT-APCUR,
              DTFILE.DT_DLRVNO = :QDT-DLRVNO,
              DTFILE.DT_SELECTED = :QDT-SELECTED,
              DTFILE.DT_P4P5_TRCD = :QDT-P4P5-TRCD,
              DTFILE.DT_DLRVPRIN_PAY = :QDT-DLRVPRIN-PAY,
              DTFILE.DT_DLRVCHGS_PAY = :QDT-DLRVCHGS-PAY,
              DTFILE.DT_DLPARVHELD_PAY = :QDT-DLPARVHELD-PAY,
              DTFILE.DT_CHECK_VOID = :QDT-CHECK-VOID,
              DTFILE.DT_UNPAID = :QDT-UNPAID,
              DTFILE.DT_MEMO = :QDT-MEMO,
              DTFILE.DT_EVENT_CD = :QDT-EVENT-CD,
              DTFILE.DT_PART_HELD_STATUS = :QDT-PART-HELD-STATUS
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
               AND DTFILE.DT_NUM = :QDT-NUM
               AND DTFILE.DT_CLASS = :QDT-CLASS
               AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
               AND DTFILE.DT_DATE = :QDT-DATE
               AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DT1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-DT1-FILE.

           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           PERFORM DELETE-IT.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT1-KEY        TO E-KEYX.
           PERFORM SET-DT-FIELDS.

           EXEC SQL
            DELETE FROM DBO.DTFILE
             WHERE DTFILE.DT_BRNO = :QDT-BRNO
               AND DTFILE.DT_NUM = :QDT-NUM
               AND DTFILE.DT_CLASS = :QDT-CLASS
               AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
               AND DTFILE.DT_DATE = :QDT-DATE
               AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-DT1-FILE.

      *-----------------------------------------------------------------
       CLOSE-DT1-FILE.

           IF ( DT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DT1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-DT2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DT-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DT-ALL-BASE
              MOVE FILPATH-MACHINE         TO DT-ALL-MACHINE
              MOVE FILPATH-DATA            TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-DTFILE NOT = "S" )
              MOVE DT-PATH-BEG             TO DT-SELECT-PATH-BEG
              MOVE DT-PATH-FILE            TO DT-SELECT-PATH-FILE
           ELSE
              MOVE DT-PATH-SQL             TO DT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-DT2-FILE.
           PERFORM LOAD-DT2-FILE.
           PERFORM OPEN-IT.
           MOVE DT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DT2-FILE.
           PERFORM START-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.

           IF ( DT2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DT2-FILE.

           IF ( QDT2-WEND-BATCHID-SEQ = ZEROES    ) OR
              ( QDT2-WEND-BATCHID-SEQ NOT NUMERIC )
              MOVE DT-BRNO              TO QDT2-WBEG-BRNO
              MOVE DT-BRNO              TO QDT2-WEND-BRNO
              MOVE DT-BATCHID           TO QDT2-WBEG-BATCHID
              MOVE ALL "9"              TO QDT2-WEND-BATCHID
              MOVE DT-BATCHID-SEQ       TO QDT2-WBEG-BATCHID-SEQ
              MOVE ALL "9"              TO QDT2-WEND-BATCHID-SEQ.

           EXEC SQL
            DECLARE DT2_CURSOR CURSOR FOR
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            FROM DBO.DTFILE
            WHERE DTFILE.DT_BRNO >= :QDT2-WBEG-BRNO
              AND DTFILE.DT_BRNO <= :QDT2-WEND-BRNO
              AND DTFILE.DT_BATCHID >= :QDT2-WBEG-BATCHID
              AND DTFILE.DT_BATCHID <= :QDT2-WEND-BATCHID
              AND DTFILE.DT_BATCHID_SEQ >= :QDT2-WBEG-BATCHID-SEQ
              AND DTFILE.DT_BATCHID_SEQ <= :QDT2-WEND-BATCHID-SEQ
            ORDER BY DTFILE.DT_BRNO,
                     DTFILE.DT_BATCHID,
                     DTFILE.DT_BATCHID_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DT2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDT2-WBEG-BRNO 
                          QDT2-WEND-BRNO
                          QDT2-WBEG-BATCHID
                          QDT2-WEND-BATCHID
                          QDT2-WBEG-BATCHID-SEQ
                          QDT2-WEND-BATCHID-SEQ.

           MOVE STAT---GOOD TO DT2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DT2-FILE.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO. 
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.

           MOVE DT-BRNO         TO QDT-BRNO.
           MOVE DT-BATCHID      TO QDT-BATCHID.
           MOVE DT-BATCHID-SEQ  TO QDT-BATCHID-SEQ.

           EXEC SQL
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            INTO :QDT-REC
            FROM DBO.DTFILE
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
              AND DTFILE.DT_BATCHID = :QDT-BATCHID
              AND DTFILE.DT_BATCHID_SEQ = :QDT-BATCHID-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       READ-DT2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QDT-REC.

           IF ( DT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DT2_CURSOR INTO :QDT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-DT2-FILE.

           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           ACCEPT DT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-BATCHID, DT-BATCHID-SEQ
              DELIMITED BY SIZE INTO E-KEYX.
           PERFORM SET-DT-FIELDS.

           EXEC SQL
            UPDATE DBO.DTFILE
             SET
              DTFILE.DT_STATUS = :QDT-STATUS,
              DTFILE.DT_BRNO = :QDT-BRNO,
              DTFILE.DT_NUM = :QDT-NUM,
              DTFILE.DT_CLASS = :QDT-CLASS,
              DTFILE.DT_ACCTNO = :QDT-ACCTNO,
              DTFILE.DT_DATE =
                 NULLIF(:QDT-DATE,'1900-01-01'),
              DTFILE.DT_SEQ = :QDT-SEQ,
              DTFILE.DT_LTOUCH_DATE =
                 NULLIF(:QDT-LTOUCH-DATE,'1900-01-01'),
              DTFILE.DT_TYPE = :QDT-TYPE,
              DTFILE.DT_LPSEQNO = :QDT-LPSEQNO,
              DTFILE.DT_TRCD = :QDT-TRCD,
              DTFILE.DT_DLRVPRIN_1 = :QDT-DLRVPRIN-1,
              DTFILE.DT_DLRVPRIN_2 = :QDT-DLRVPRIN-2,
              DTFILE.DT_DLRVPRIN_3 = :QDT-DLRVPRIN-3,
              DTFILE.DT_DLRVCHGS_1 = :QDT-DLRVCHGS-1,
              DTFILE.DT_DLRVCHGS_2 = :QDT-DLRVCHGS-2,
              DTFILE.DT_DLRVCHGS_3 = :QDT-DLRVCHGS-3,
              DTFILE.DT_DLPARVHELD = :QDT-DLPARVHELD,
              DTFILE.DT_DLPARVPAID = :QDT-DLPARVPAID,
              DTFILE.DT_PROCEEDS = :QDT-PROCEEDS,
              DTFILE.DT_RELEASED = :QDT-RELEASED,
              DTFILE.DT_PAYMENTS = :QDT-PAYMENTS,
              DTFILE.DT_CKNO = :QDT-CKNO,
              DTFILE.DT_CKDATE =
                 NULLIF(:QDT-CKDATE,'1900-01-01'),
              DTFILE.DT_BATCHID = :QDT-BATCHID,
              DTFILE.DT_BATCHID_SEQ = :QDT-BATCHID-SEQ,
              DTFILE.DT_PAID = :QDT-PAID,
              DTFILE.DT_REVERSE = :QDT-REVERSE,
              DTFILE.DT_REVERSE_TRCD = :QDT-REVERSE-TRCD,
              DTFILE.DT_PURBAL = :QDT-PURBAL,
              DTFILE.DT_APCUR = :QDT-APCUR,
              DTFILE.DT_DLRVNO = :QDT-DLRVNO,
              DTFILE.DT_SELECTED = :QDT-SELECTED,
              DTFILE.DT_P4P5_TRCD = :QDT-P4P5-TRCD,
              DTFILE.DT_DLRVPRIN_PAY = :QDT-DLRVPRIN-PAY,
              DTFILE.DT_DLRVCHGS_PAY = :QDT-DLRVCHGS-PAY,
              DTFILE.DT_DLPARVHELD_PAY = :QDT-DLPARVHELD-PAY,
              DTFILE.DT_CHECK_VOID = :QDT-CHECK-VOID,
              DTFILE.DT_UNPAID = :QDT-UNPAID,
              DTFILE.DT_MEMO = :QDT-MEMO,
              DTFILE.DT_EVENT_CD = :QDT-EVENT-CD,
              DTFILE.DT_PART_HELD_STATUS = :QDT-PART-HELD-STATUS
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
               AND DTFILE.DT_NUM = :QDT-NUM
               AND DTFILE.DT_CLASS = :QDT-CLASS
               AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
               AND DTFILE.DT_DATE = :QDT-DATE
               AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DT2-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       CLOSE-DT2-FILE.

           IF ( DT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DT2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DT1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-DT3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DT-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DT-ALL-BASE
              MOVE FILPATH-MACHINE         TO DT-ALL-MACHINE
              MOVE FILPATH-DATA            TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-DTFILE NOT = "S" )
              MOVE DT-PATH-BEG             TO DT-SELECT-PATH-BEG
              MOVE DT-PATH-FILE            TO DT-SELECT-PATH-FILE
           ELSE
              MOVE DT-PATH-SQL             TO DT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-DT3-FILE.
           PERFORM LOAD-DT3-FILE.
           PERFORM OPEN-IT.
           MOVE DT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DT3-FILE.
           PERFORM START-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT3-KEY        TO E-KEYX.

           IF ( DT3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DT3-FILE.

           IF ( QDT3-WEND-SEQ = ZEROES    ) OR
              ( QDT3-WEND-SEQ NOT NUMERIC )
              MOVE DT-STATUS            TO QDT3-WBEG-STATUS
              MOVE ALL "Z"              TO QDT3-WEND-STATUS
              MOVE DT-BRNO              TO QDT3-WBEG-BRNO
              MOVE DT-BRNO              TO QDT3-WEND-BRNO
              MOVE DT-NUM               TO QDT3-WBEG-NUM 
              MOVE ALL "9"              TO QDT3-WEND-NUM 
              MOVE DT-CLASS             TO QDT3-WBEG-CLASS
              MOVE ALL "9"              TO QDT3-WEND-CLASS
              MOVE DT-ACCTNO            TO QDT3-WBEG-ACCTNO
              MOVE ALL "9"              TO QDT3-WEND-ACCTNO
              MOVE DT-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT3-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QDT3-WEND-DATE
              MOVE DT-SEQ               TO QDT3-WBEG-SEQ
              MOVE ALL "9"              TO QDT3-WEND-SEQ.

           EXEC SQL
            DECLARE DT3_CURSOR CURSOR FOR
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            FROM DBO.DTFILE
            WHERE DTFILE.DT_STATUS >= :QDT3-WBEG-STATUS
              AND DTFILE.DT_STATUS <= :QDT3-WEND-STATUS
              AND DTFILE.DT_BRNO >= :QDT3-WBEG-BRNO
              AND DTFILE.DT_BRNO <= :QDT3-WEND-BRNO
              AND DTFILE.DT_NUM >= :QDT3-WBEG-NUM 
              AND DTFILE.DT_NUM <= :QDT3-WEND-NUM 
              AND DTFILE.DT_CLASS >= :QDT3-WBEG-CLASS
              AND DTFILE.DT_CLASS <= :QDT3-WEND-CLASS
              AND DTFILE.DT_ACCTNO >= :QDT3-WBEG-ACCTNO
              AND DTFILE.DT_ACCTNO <= :QDT3-WEND-ACCTNO
              AND CAST(DTFILE.DT_DATE AS VARCHAR(10))
                          >= :QDT3-WBEG-DATE
              AND CAST(DTFILE.DT_DATE AS VARCHAR(10))
                          <= :QDT3-WEND-DATE
              AND DTFILE.DT_SEQ >= :QDT3-WBEG-SEQ
              AND DTFILE.DT_SEQ <= :QDT3-WEND-SEQ
            ORDER BY DTFILE.DT_STATUS,
                     DTFILE.DT_BRNO,
                     DTFILE.DT_NUM,
                     DTFILE.DT_CLASS,
                     DTFILE.DT_ACCTNO,
                     DTFILE.DT_DATE,
                     DTFILE.DT_SEQ
           END-EXEC.

           EXEC SQL
               OPEN DT3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDT3-WBEG-BRNO 
                          QDT3-WEND-BRNO
                          QDT3-WBEG-NUM
                          QDT3-WEND-NUM
                          QDT3-WBEG-CLASS
                          QDT3-WEND-CLASS
                          QDT3-WBEG-ACCTNO
                          QDT3-WEND-ACCTNO
                          QDT3-WBEG-SEQ
                          QDT3-WEND-SEQ.
 
           MOVE SPACES TO QDT3-WBEG-DATE
                          QDT3-WEND-DATE
                          QDT3-WBEG-STATUS
                          QDT3-WEND-STATUS.


           MOVE STAT---GOOD TO DT3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DT3-FILE.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO. 
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT3-KEY        TO E-KEYX.

           MOVE DT-STATUS TO QDT-STATUS.
           MOVE DT-BRNO   TO QDT-BRNO.
           MOVE DT-NUM    TO QDT-NUM.
           MOVE DT-CLASS  TO QDT-CLASS.
           MOVE DT-ACCTNO TO QDT-ACCTNO.
           MOVE DT-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QDT-DATE.
           MOVE DT-SEQ    TO QDT-SEQ.

           EXEC SQL
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            INTO :QDT-REC
            FROM DBO.DTFILE
            WHERE DTFILE.DT_STATUS = :QDT-STATUS
              AND DTFILE.DT_BRNO = :QDT-BRNO
              AND DTFILE.DT_NUM = :QDT-NUM
              AND DTFILE.DT_CLASS = :QDT-CLASS
              AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
              AND DTFILE.DT_DATE = :QDT-DATE
              AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       READ-DT3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT3-KEY        TO E-KEYX.
           INITIALIZE QDT-REC.

           IF ( DT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DT3_CURSOR INTO :QDT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-DT3-FILE.

           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           ACCEPT DT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE DT3-KEY        TO E-KEYX.
           PERFORM SET-DT-FIELDS.

           EXEC SQL
            UPDATE DBO.DTFILE
             SET
              DTFILE.DT_STATUS = :QDT-STATUS,
              DTFILE.DT_BRNO = :QDT-BRNO,
              DTFILE.DT_NUM = :QDT-NUM,
              DTFILE.DT_CLASS = :QDT-CLASS,
              DTFILE.DT_ACCTNO = :QDT-ACCTNO,
              DTFILE.DT_DATE =
                 NULLIF(:QDT-DATE,'1900-01-01'),
              DTFILE.DT_SEQ = :QDT-SEQ,
              DTFILE.DT_LTOUCH_DATE =
                 NULLIF(:QDT-LTOUCH-DATE,'1900-01-01'),
              DTFILE.DT_TYPE = :QDT-TYPE,
              DTFILE.DT_LPSEQNO = :QDT-LPSEQNO,
              DTFILE.DT_TRCD = :QDT-TRCD,
              DTFILE.DT_DLRVPRIN_1 = :QDT-DLRVPRIN-1,
              DTFILE.DT_DLRVPRIN_2 = :QDT-DLRVPRIN-2,
              DTFILE.DT_DLRVPRIN_3 = :QDT-DLRVPRIN-3,
              DTFILE.DT_DLRVCHGS_1 = :QDT-DLRVCHGS-1,
              DTFILE.DT_DLRVCHGS_2 = :QDT-DLRVCHGS-2,
              DTFILE.DT_DLRVCHGS_3 = :QDT-DLRVCHGS-3,
              DTFILE.DT_DLPARVHELD = :QDT-DLPARVHELD,
              DTFILE.DT_DLPARVPAID = :QDT-DLPARVPAID,
              DTFILE.DT_PROCEEDS = :QDT-PROCEEDS,
              DTFILE.DT_RELEASED = :QDT-RELEASED,
              DTFILE.DT_PAYMENTS = :QDT-PAYMENTS,
              DTFILE.DT_CKNO = :QDT-CKNO,
              DTFILE.DT_CKDATE =
                 NULLIF(:QDT-CKDATE,'1900-01-01'),
              DTFILE.DT_BATCHID = :QDT-BATCHID,
              DTFILE.DT_BATCHID_SEQ = :QDT-BATCHID-SEQ,
              DTFILE.DT_PAID = :QDT-PAID,
              DTFILE.DT_REVERSE = :QDT-REVERSE,
              DTFILE.DT_REVERSE_TRCD = :QDT-REVERSE-TRCD,
              DTFILE.DT_PURBAL = :QDT-PURBAL,
              DTFILE.DT_APCUR = :QDT-APCUR,
              DTFILE.DT_DLRVNO = :QDT-DLRVNO,
              DTFILE.DT_SELECTED = :QDT-SELECTED,
              DTFILE.DT_P4P5_TRCD = :QDT-P4P5-TRCD,
              DTFILE.DT_DLRVPRIN_PAY = :QDT-DLRVPRIN-PAY,
              DTFILE.DT_DLRVCHGS_PAY = :QDT-DLRVCHGS-PAY,
              DTFILE.DT_DLPARVHELD_PAY = :QDT-DLPARVHELD-PAY,
              DTFILE.DT_CHECK_VOID = :QDT-CHECK-VOID,
              DTFILE.DT_UNPAID = :QDT-UNPAID,
              DTFILE.DT_MEMO = :QDT-MEMO,
              DTFILE.DT_EVENT_CD = :QDT-EVENT-CD,
              DTFILE.DT_PART_HELD_STATUS = :QDT-PART-HELD-STATUS
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
               AND DTFILE.DT_NUM = :QDT-NUM
               AND DTFILE.DT_CLASS = :QDT-CLASS
               AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
               AND DTFILE.DT_DATE = :QDT-DATE
               AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DT3-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       CLOSE-DT3-FILE.

           IF ( DT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DT3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DT3-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-DT4-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DT-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DT-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DT-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DT-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DT-ALL-BASE
              MOVE FILPATH-MACHINE         TO DT-ALL-MACHINE
              MOVE FILPATH-DATA            TO DT-ALL-DATA
              MOVE DT-ALL-PATH             TO DT-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-DTFILE NOT = "S" )
              MOVE DT-PATH-BEG             TO DT-SELECT-PATH-BEG
              MOVE DT-PATH-FILE            TO DT-SELECT-PATH-FILE
           ELSE
              MOVE DT-PATH-SQL             TO DT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-DT4-FILE.
           PERFORM LOAD-DT4-FILE.
           PERFORM OPEN-IT.
           MOVE DT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DT4-FILE.
           PERFORM START-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-CKNO DELIMITED BY "  " INTO E-KEYX.

           IF ( DT4-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DT4-FILE.

           IF ( QDT4-WEND-CKNO = ZEROES    ) OR
              ( QDT4-WEND-CKNO NOT NUMERIC )
              MOVE DT-BRNO              TO QDT4-WBEG-BRNO
              MOVE DT-BRNO              TO QDT4-WEND-BRNO
              MOVE DT-CKNO              TO QDT4-WBEG-CKNO
              MOVE ALL "9"              TO QDT4-WEND-CKNO.

           EXEC SQL
            DECLARE DT4_CURSOR CURSOR FOR
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            FROM DBO.DTFILE
            WHERE DTFILE.DT_BRNO >= :QDT4-WBEG-BRNO
              AND DTFILE.DT_BRNO <= :QDT4-WEND-BRNO
              AND DTFILE.DT_CKNO >= :QDT4-WBEG-CKNO
              AND DTFILE.DT_CKNO <= :QDT4-WEND-CKNO
            ORDER BY DTFILE.DT_BRNO,
                     DTFILE.DT_CKNO
           END-EXEC.

           EXEC SQL
               OPEN DT4_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDT4-WBEG-BRNO 
                          QDT4-WEND-BRNO
                          QDT4-WBEG-CKNO
                          QDT4-WEND-CKNO.

           MOVE STAT---GOOD TO DT4-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DT4-FILE.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO. 
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-CKNO DELIMITED BY "  " INTO E-KEYX.

           MOVE DT-BRNO   TO QDT-BRNO.
           MOVE DT-CKNO   TO QDT-CKNO.

           EXEC SQL
            SELECT ISNULL(DTFILE.DT_STATUS,''),
                   DTFILE.DT_BRNO,
                   DTFILE.DT_NUM,
                   DTFILE.DT_CLASS,
                   DTFILE.DT_ACCTNO,
                   ISNULL(CAST(DTFILE.DT_DATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_SEQ,
                   ISNULL(CAST(DTFILE.DT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DTFILE.DT_TYPE,''),
                   DTFILE.DT_LPSEQNO,
                   ISNULL(DTFILE.DT_TRCD,''),
                   DTFILE.DT_DLRVPRIN_1,
                   DTFILE.DT_DLRVPRIN_2,
                   DTFILE.DT_DLRVPRIN_3,
                   DTFILE.DT_DLRVCHGS_1,
                   DTFILE.DT_DLRVCHGS_2,
                   DTFILE.DT_DLRVCHGS_3,
                   DTFILE.DT_DLPARVHELD,
                   DTFILE.DT_DLPARVPAID,
                   DTFILE.DT_PROCEEDS,
                   DTFILE.DT_RELEASED,
                   DTFILE.DT_PAYMENTS,
                   DTFILE.DT_CKNO,
                   ISNULL(CAST(DTFILE.DT_CKDATE AS VARCHAR(10)),'0'),
                   DTFILE.DT_BATCHID,
                   DTFILE.DT_BATCHID_SEQ,
                   ISNULL(DTFILE.DT_PAID,''),
                   ISNULL(DTFILE.DT_REVERSE,''),
                   ISNULL(DTFILE.DT_REVERSE_TRCD,''),
                   DTFILE.DT_PURBAL,
                   DTFILE.DT_APCUR,
                   DTFILE.DT_DLRVNO,
                   ISNULL(DTFILE.DT_SELECTED,''),
                   ISNULL(DTFILE.DT_P4P5_TRCD,''),
                   DTFILE.DT_DLRVPRIN_PAY,
                   DTFILE.DT_DLRVCHGS_PAY,
                   DTFILE.DT_DLPARVHELD_PAY,
                   ISNULL(DTFILE.DT_CHECK_VOID,''),
                   ISNULL(DTFILE.DT_UNPAID,''),
                   ISNULL(DTFILE.DT_MEMO,''),
                   ISNULL(DTFILE.DT_EVENT_CD,''),
                   ISNULL(DTFILE.DT_PART_HELD_STATUS,'')
            INTO :QDT-REC
            FROM DBO.DTFILE
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
              AND DTFILE.DT_CKNO = :QDT-CKNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT4-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       READ-DT4-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-CKNO DELIMITED BY "  " INTO E-KEYX.
           INITIALIZE QDT-REC.

           IF ( DT4-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DT4_CURSOR INTO :QDT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DT4-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DT-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-DT4-FILE.

           MOVE DT-PATH-OWNBR  TO DT-BRNO.
           ACCEPT DT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DT-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DT-BRNO, DT-CKNO DELIMITED BY "  " INTO E-KEYX.
           PERFORM SET-DT-FIELDS.

           EXEC SQL
            UPDATE DBO.DTFILE
             SET
              DTFILE.DT_STATUS = :QDT-STATUS,
              DTFILE.DT_BRNO = :QDT-BRNO,
              DTFILE.DT_NUM = :QDT-NUM,
              DTFILE.DT_CLASS = :QDT-CLASS,
              DTFILE.DT_ACCTNO = :QDT-ACCTNO,
              DTFILE.DT_DATE =
                 NULLIF(:QDT-DATE,'1900-01-01'),
              DTFILE.DT_SEQ = :QDT-SEQ,
              DTFILE.DT_LTOUCH_DATE =
                 NULLIF(:QDT-LTOUCH-DATE,'1900-01-01'),
              DTFILE.DT_TYPE = :QDT-TYPE,
              DTFILE.DT_LPSEQNO = :QDT-LPSEQNO,
              DTFILE.DT_TRCD = :QDT-TRCD,
              DTFILE.DT_DLRVPRIN_1 = :QDT-DLRVPRIN-1,
              DTFILE.DT_DLRVPRIN_2 = :QDT-DLRVPRIN-2,
              DTFILE.DT_DLRVPRIN_3 = :QDT-DLRVPRIN-3,
              DTFILE.DT_DLRVCHGS_1 = :QDT-DLRVCHGS-1,
              DTFILE.DT_DLRVCHGS_2 = :QDT-DLRVCHGS-2,
              DTFILE.DT_DLRVCHGS_3 = :QDT-DLRVCHGS-3,
              DTFILE.DT_DLPARVHELD = :QDT-DLPARVHELD,
              DTFILE.DT_DLPARVPAID = :QDT-DLPARVPAID,
              DTFILE.DT_PROCEEDS = :QDT-PROCEEDS,
              DTFILE.DT_RELEASED = :QDT-RELEASED,
              DTFILE.DT_PAYMENTS = :QDT-PAYMENTS,
              DTFILE.DT_CKNO = :QDT-CKNO,
              DTFILE.DT_CKDATE =
                 NULLIF(:QDT-CKDATE,'1900-01-01'),
              DTFILE.DT_BATCHID = :QDT-BATCHID,
              DTFILE.DT_BATCHID_SEQ = :QDT-BATCHID-SEQ,
              DTFILE.DT_PAID = :QDT-PAID,
              DTFILE.DT_REVERSE = :QDT-REVERSE,
              DTFILE.DT_REVERSE_TRCD = :QDT-REVERSE-TRCD,
              DTFILE.DT_PURBAL = :QDT-PURBAL,
              DTFILE.DT_APCUR = :QDT-APCUR,
              DTFILE.DT_DLRVNO = :QDT-DLRVNO,
              DTFILE.DT_SELECTED = :QDT-SELECTED,
              DTFILE.DT_P4P5_TRCD = :QDT-P4P5-TRCD,
              DTFILE.DT_DLRVPRIN_PAY = :QDT-DLRVPRIN-PAY,
              DTFILE.DT_DLRVCHGS_PAY = :QDT-DLRVCHGS-PAY,
              DTFILE.DT_DLPARVHELD_PAY = :QDT-DLPARVHELD-PAY,
              DTFILE.DT_CHECK_VOID = :QDT-CHECK-VOID,
              DTFILE.DT_UNPAID = :QDT-UNPAID,
              DTFILE.DT_MEMO = :QDT-MEMO,
              DTFILE.DT_EVENT_CD = :QDT-EVENT-CD,
              DTFILE.DT_PART_HELD_STATUS = :QDT-PART-HELD-STATUS
            WHERE DTFILE.DT_BRNO = :QDT-BRNO
               AND DTFILE.DT_NUM = :QDT-NUM
               AND DTFILE.DT_CLASS = :QDT-CLASS
               AND DTFILE.DT_ACCTNO = :QDT-ACCTNO
               AND DTFILE.DT_DATE = :QDT-DATE
               AND DTFILE.DT_SEQ = :QDT-SEQ
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DT4-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       CLOSE-DT4-FILE.

           IF ( DT4-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DT4_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DT4-CURSOR-STAT.

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

      *    MOVE WK1-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

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

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IODTIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
