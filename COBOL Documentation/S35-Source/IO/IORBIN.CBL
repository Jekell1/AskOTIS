       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IORBIN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  03-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IORBIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : RBFILE 
      * TABLE DESC    : REBATE / CANCELLATION TABLE FILE
      * OUTPUT FILE   : XPC/IORBIN.PRE
      * USED FOR      : LIBLP/LPWSRB.CPY  --- QRB-REC
      *                 LIBLP/LPRB1R.CPY  --- READ
      *                 LIBLP/LPRB1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      *  BAH 2021-0311 ***NEW***
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
           "@(#)IO/IORBIN.CBL S34 05/18/21-11:09:45 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01RB.CPY".
       COPY "LIBLP/LPWSRB.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QRB1-WHERE-BEG-KEY.
           03  QRB1-WBEG-TBL-NO         PIC 99.
           03  QRB1-WBEG-TERM           PIC 9(3).

       01  QRB1-WHERE-END-KEY.
           03  QRB1-WEND-TBL-NO         PIC 99.
           03  QRB1-WEND-TERM           PIC 9(3).

       01  QRB-REC.
           03  QRB-TBL-NO    PIC 99.
           03  QRB-TERM      PIC 9(3).
           03  QRB-MON-DAY-CD    PIC X.
           03  QRB-AMT-1       PIC 9(4).
           03  QRB-RATE-1      PIC 99V99.
           03  QRB-AMT-2       PIC 9(4).
           03  QRB-RATE-2      PIC 99V99.
           03  QRB-AMT-3       PIC 9(4).
           03  QRB-RATE-3      PIC 99V99.
           03  QRB-AMT-4       PIC 9(4).
           03  QRB-RATE-4      PIC 99V99.
           03  QRB-AMT-5       PIC 9(4).
           03  QRB-RATE-5      PIC 99V99.
           03  QRB-AMT-6       PIC 9(4).
           03  QRB-RATE-6      PIC 99V99.
           03  QRB-AMT-7       PIC 9(4).
           03  QRB-RATE-7      PIC 99V99.
           03  QRB-AMT-8       PIC 9(4).
           03  QRB-RATE-8      PIC 99V99.
           03  QRB-AMT-9       PIC 9(4).
           03  QRB-RATE-9      PIC 99V99.
           03  QRB-AMT-10      PIC 9(4).
           03  QRB-RATE-10     PIC 99V99.
           03  QRB-AMT-11       PIC 9(4).
           03  QRB-RATE-11      PIC 99V99.
           03  QRB-AMT-12       PIC 9(4).
           03  QRB-RATE-12      PIC 99V99.
           03  QRB-AMT-13       PIC 9(4).
           03  QRB-RATE-13      PIC 99V99.
           03  QRB-AMT-14       PIC 9(4).
           03  QRB-RATE-14      PIC 99V99.
           03  QRB-AMT-15       PIC 9(4).
           03  QRB-RATE-15      PIC 99V99.
           03  QRB-AMT-16       PIC 9(4).
           03  QRB-RATE-16      PIC 99V99.
           03  QRB-AMT-17       PIC 9(4).
           03  QRB-RATE-17      PIC 99V99.
           03  QRB-AMT-18       PIC 9(4).
           03  QRB-RATE-18      PIC 99V99.
           03  QRB-AMT-19       PIC 9(4).
           03  QRB-RATE-19      PIC 99V99.
           03  QRB-AMT-20       PIC 9(4).
           03  QRB-RATE-20      PIC 99V99.
           03  QRB-AMT-21       PIC 9(4).
           03  QRB-RATE-21      PIC 99V99.
           03  QRB-AMT-22       PIC 9(4).
           03  QRB-RATE-22      PIC 99V99.
           03  QRB-AMT-23       PIC 9(4).
           03  QRB-RATE-23      PIC 99V99.
           03  QRB-AMT-24       PIC 9(4).
           03  QRB-RATE-24      PIC 99V99.
           03  QRB-AMT-25       PIC 9(4).
           03  QRB-RATE-25      PIC 99V99.
           03  QRB-AMT-26       PIC 9(4).
           03  QRB-RATE-26      PIC 99V99.
           03  QRB-AMT-27       PIC 9(4).
           03  QRB-RATE-27      PIC 99V99.
           03  QRB-AMT-28       PIC 9(4).
           03  QRB-RATE-28      PIC 99V99.
           03  QRB-AMT-29       PIC 9(4).
           03  QRB-RATE-29      PIC 99V99.
           03  QRB-AMT-30       PIC 9(4).
           03  QRB-RATE-30      PIC 99V99.
           03  QRB-AMT-31       PIC 9(4).
           03  QRB-RATE-31      PIC 99V99.
           03  QRB-AMT-32       PIC 9(4).
           03  QRB-RATE-32      PIC 99V99.
           03  QRB-AMT-33       PIC 9(4).
           03  QRB-RATE-33      PIC 99V99.
           03  QRB-AMT-34       PIC 9(4).
           03  QRB-RATE-34      PIC 99V99.
           03  QRB-AMT-35       PIC 9(4).
           03  QRB-RATE-35      PIC 99V99.
           03  QRB-AMT-36       PIC 9(4).
           03  QRB-RATE-36      PIC 99V99.
           03  QRB-AMT-37       PIC 9(4).
           03  QRB-RATE-37      PIC 99V99.
           03  QRB-AMT-38       PIC 9(4).
           03  QRB-RATE-38      PIC 99V99.
           03  QRB-AMT-39       PIC 9(4).
           03  QRB-RATE-39      PIC 99V99.
           03  QRB-AMT-40       PIC 9(4).
           03  QRB-RATE-40      PIC 99V99.
           03  QRB-AMT-41       PIC 9(4).
           03  QRB-RATE-41      PIC 99V99.
           03  QRB-AMT-42       PIC 9(4).
           03  QRB-RATE-42      PIC 99V99.
           03  QRB-AMT-43       PIC 9(4).
           03  QRB-RATE-43      PIC 99V99.
           03  QRB-AMT-44       PIC 9(4).
           03  QRB-RATE-44      PIC 99V99.
           03  QRB-AMT-45       PIC 9(4).
           03  QRB-RATE-45      PIC 99V99.
           03  QRB-AMT-46       PIC 9(4).
           03  QRB-RATE-46      PIC 99V99.
           03  QRB-AMT-47       PIC 9(4).
           03  QRB-RATE-47      PIC 99V99.
           03  QRB-AMT-48       PIC 9(4).
           03  QRB-RATE-48      PIC 99V99.
           03  QRB-AMT-49       PIC 9(4).
           03  QRB-RATE-49      PIC 99V99.
           03  QRB-AMT-50       PIC 9(4).
           03  QRB-RATE-50      PIC 99V99.
           03  QRB-AMT-51       PIC 9(4).
           03  QRB-RATE-51      PIC 99V99.
           03  QRB-AMT-52       PIC 9(4).
           03  QRB-RATE-52      PIC 99V99.
           03  QRB-AMT-53       PIC 9(4).
           03  QRB-RATE-53      PIC 99V99.
           03  QRB-AMT-54       PIC 9(4).
           03  QRB-RATE-54      PIC 99V99.
           03  QRB-AMT-55       PIC 9(4).
           03  QRB-RATE-55      PIC 99V99.
           03  QRB-AMT-56       PIC 9(4).
           03  QRB-RATE-56      PIC 99V99.
           03  QRB-AMT-57       PIC 9(4).
           03  QRB-RATE-57      PIC 99V99.
           03  QRB-AMT-58       PIC 9(4).
           03  QRB-RATE-58      PIC 99V99.
           03  QRB-AMT-59       PIC 9(4).
           03  QRB-RATE-59      PIC 99V99.
           03  QRB-AMT-60       PIC 9(4).
           03  QRB-RATE-60      PIC 99V99.
           03  QRB-AMT-61       PIC 9(4).
           03  QRB-RATE-61      PIC 99V99.
           03  QRB-AMT-62       PIC 9(4).
           03  QRB-RATE-62      PIC 99V99.
           03  QRB-AMT-63       PIC 9(4).
           03  QRB-RATE-63      PIC 99V99.
           03  QRB-AMT-64       PIC 9(4).
           03  QRB-RATE-64      PIC 99V99.
           03  QRB-AMT-65       PIC 9(4).
           03  QRB-RATE-65      PIC 99V99.
           03  QRB-AMT-66       PIC 9(4).
           03  QRB-RATE-66      PIC 99V99.
           03  QRB-AMT-67       PIC 9(4).
           03  QRB-RATE-67      PIC 99V99.
           03  QRB-AMT-68       PIC 9(4).
           03  QRB-RATE-68      PIC 99V99.
           03  QRB-AMT-69       PIC 9(4).
           03  QRB-RATE-69      PIC 99V99.
           03  QRB-AMT-70       PIC 9(4).
           03  QRB-RATE-70      PIC 99V99.
           03  QRB-AMT-71       PIC 9(4).
           03  QRB-RATE-71      PIC 99V99.
           03  QRB-AMT-72       PIC 9(4).
           03  QRB-RATE-72      PIC 99V99.
           03  QRB-AMT-73       PIC 9(4).
           03  QRB-RATE-73      PIC 99V99.
           03  QRB-AMT-74       PIC 9(4).
           03  QRB-RATE-74      PIC 99V99.
           03  QRB-AMT-75       PIC 9(4).
           03  QRB-RATE-75      PIC 99V99.
           03  QRB-LTOUCH-DATE   PIC X(10).
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

       01  LEGEND-BEG-TABLE. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING TABLE      (F3-ALL) ".

       01  LEGEND-END-TABLE.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING TABLE      ".

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
           03  BEG-TBL                 PIC 99.
           03  END-TBL                 PIC 99.
           03  BEG-TERM                PIC 9(3).
           03  END-TERM                PIC 9(3).

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
           03  FILLER PIC X(33) VALUE "TABLE  TERM  MON  AMT1  RATE1 ".
           03  FILLER PIC X(29) VALUE "AMT2  RATE2   AMT10  RATE10  ".
           03  FILLER PIC X(30) VALUE "AMT75  RATE75".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-RB-TABLE                   PIC 99.
           03  FILLER                         PIC X(05).
           03  D01-RB-TERM                    PIC 999.
           03  FILLER                         PIC X(03).
           03  D01-RB-MON-DAY-CD              PIC X.
           03  FILLER                         PIC X(03).
           03  D01-RB-AMT-1                   PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-RB-RATE-1                  PIC 99.99.
           03  FILLER                         PIC X(03).
           03  D01-RB-AMT-2                   PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-RB-RATE-2                  PIC 99.99.
           03  FILLER                         PIC X(03).
           03  D01-RB-AMT-10                  PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-RB-RATE-10                 PIC 99.99.
           03  FILLER                         PIC X(03).
           03  D01-RB-AMT-75                  PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-RB-RATE-75                 PIC 99.99.

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
       COPY "LIBIO/IORBIN_DEF.CPY".
       COPY "LIBIO/IORBIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IORBIN_SCN.CPY".

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
           MOVE "B"                        TO RB-PATH-OVERRIDE.

           MOVE BEG-TBL                 TO QRB1-WBEG-TBL-NO.
           MOVE END-TBL                 TO QRB1-WEND-TBL-NO.
           MOVE BEG-TERM                TO QRB1-WBEG-TERM.
           MOVE END-TERM                TO QRB1-WEND-TERM.

           PERFORM OPEN-RB1-FILE.
           PERFORM START-RB1-FILE.

       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-RB1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( RB-TBL-NO < BEG-TBL )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( RB-TBL-NO > END-TBL )
              GO TO MAIN-PROGRAM-END.

           IF RB-TERM < BEG-TERM OR RB-TERM > END-TERM
              GO TO MAIN-PROGRAM-NEXT.

           MOVE RB-TBL-NO                  TO D01-RB-TABLE.
           MOVE RB-TERM                    TO D01-RB-TERM.
           MOVE RB-MON-DAY-CD              TO D01-RB-MON-DAY-CD.
           MOVE RB-AMT(1)                  TO D01-RB-AMT-1.
           MOVE RB-RATE(1)                 TO D01-RB-RATE-1.
           MOVE RB-AMT(2)                  TO D01-RB-AMT-2.
           MOVE RB-RATE(2)                 TO D01-RB-RATE-2.
           MOVE RB-AMT(10)                 TO D01-RB-AMT-10.
           MOVE RB-RATE(10)                TO D01-RB-RATE-10.
           MOVE RB-AMT(75)                 TO D01-RB-AMT-75.
           MOVE RB-RATE(75)                TO D01-RB-RATE-75.

      *-------------------------------------------------------{ RANGES }
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
           MOVE "IORBIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IORBIN-QUEUE-POS  TO Q-POS.
           MOVE IORBIN-COPIES-POS TO C-POS.
           MOVE IORBIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-TBL.

           MOVE LEGEND-BEG-TABLE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N020BEG-TBL." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TBL.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TBL.

           MOVE VDUNUM0 TO BEG-TBL.

      *-----------------------------------------------------------------
       ENTER-END-TBL.

           MOVE LEGEND-END-TABLE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N020END-TBL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TBL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TBL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-TBL.

           IF ( BEG-TBL > END-TBL )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TBL.

           GO TO ENTER-BEG-TERM.

      *-----------------------------------------------------------------
       ENTER-ALL-TBL.

           MOVE "S  N020BEG-TBL." TO VDU.
           MOVE 0               TO BEG-TBL VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N020END-TBL." TO VDU.
           MOVE 99              TO END-TBL VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-TERM.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N030BEG-TERM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-TBL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TERM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TBL.

           MOVE VDUNUM0 TO BEG-TERM.

      *-----------------------------------------------------------------
       ENTER-END-TERM.


           MOVE "EN N030END-TERM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TERM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TBL.

           MOVE VDUNUM0 TO END-TERM.

           IF ( BEG-TERM > END-TERM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TERM.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-TERM.

           MOVE "S  N030BEG-TERM." TO VDU.
           MOVE 0               TO BEG-TERM VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A030END-TERM." TO VDU.
           MOVE 99              TO END-TERM VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-TERM.
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

           MOVE "S  N000BEG-TBL."    TO VDU.
           MOVE BEG-TBL              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N000END-TBL."    TO VDU.
           MOVE END-TBL          TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N000BEG-TERM."    TO VDU.
           MOVE BEG-TERM              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N000END-TERM."    TO VDU.
           MOVE END-TERM          TO VDUNUM0.
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

           MOVE RB1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING TABLE               : "    TO R01-PROMPT.
           MOVE BEG-TBL                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TABLE               : "    TO R01-PROMPT.
           MOVE END-TBL                               TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING TERM               : "    TO R01-PROMPT.
           MOVE BEG-TERM                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TERM               : "    TO R01-PROMPT.
           MOVE END-TERM                               TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - RBFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IORBIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IORBIN-SYSDATE.
           DISPLAY IORBIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IORBIN-FORM-FIELDS TO WR-BUF.
           MOVE IORBIN-FORM-LIST TO WR-LIST.
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

           MOVE IORBIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IORBIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IORBIN_EVA.CPY".
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
      *    LOAD RB-REC FROM QRB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-RB-FIELDS.
           MOVE QRB-TBL-NO TO RB-TBL-NO.
           MOVE QRB-TERM TO RB-TERM.
           MOVE QRB-MON-DAY-CD TO RB-MON-DAY-CD.
           MOVE QRB-AMT-1 TO RB-AMT(1).
           MOVE QRB-RATE-1 TO RB-RATE(1).
           MOVE QRB-AMT-2 TO RB-AMT(2).
           MOVE QRB-RATE-2 TO RB-RATE(2).
           MOVE QRB-AMT-3 TO RB-AMT(3).
           MOVE QRB-RATE-3 TO RB-RATE(3).
           MOVE QRB-AMT-4 TO RB-AMT(4).
           MOVE QRB-RATE-4 TO RB-RATE(4).
           MOVE QRB-AMT-5 TO RB-AMT(5).
           MOVE QRB-RATE-5 TO RB-RATE(5).
           MOVE QRB-AMT-6 TO RB-AMT(6).
           MOVE QRB-RATE-6 TO RB-RATE(6).
           MOVE QRB-AMT-7 TO RB-AMT(7).
           MOVE QRB-RATE-7 TO RB-RATE(7).
           MOVE QRB-AMT-8 TO RB-AMT(8).
           MOVE QRB-RATE-8 TO RB-RATE(8).
           MOVE QRB-AMT-9 TO RB-AMT(9).
           MOVE QRB-RATE-9 TO RB-RATE(9).
           MOVE QRB-AMT-10 TO RB-AMT(10).
           MOVE QRB-RATE-10 TO RB-RATE(10).
           MOVE QRB-AMT-11 TO RB-AMT(11).
           MOVE QRB-RATE-11 TO RB-RATE(11).
           MOVE QRB-AMT-12 TO RB-AMT(12).
           MOVE QRB-RATE-12 TO RB-RATE(12).
           MOVE QRB-AMT-13 TO RB-AMT(13).
           MOVE QRB-RATE-13 TO RB-RATE(13).
           MOVE QRB-AMT-14 TO RB-AMT(14).
           MOVE QRB-RATE-14 TO RB-RATE(14).
           MOVE QRB-AMT-15 TO RB-AMT(15).
           MOVE QRB-RATE-15 TO RB-RATE(15).
           MOVE QRB-AMT-16 TO RB-AMT(16).
           MOVE QRB-RATE-16 TO RB-RATE(16).
           MOVE QRB-AMT-17 TO RB-AMT(17).
           MOVE QRB-RATE-17 TO RB-RATE(17).
           MOVE QRB-AMT-18 TO RB-AMT(18).
           MOVE QRB-RATE-18 TO RB-RATE(18).
           MOVE QRB-AMT-19 TO RB-AMT(19).
           MOVE QRB-RATE-19 TO RB-RATE(19).
           MOVE QRB-AMT-20 TO RB-AMT(20).
           MOVE QRB-RATE-20 TO RB-RATE(20).
           MOVE QRB-AMT-21 TO RB-AMT(21).
           MOVE QRB-RATE-21 TO RB-RATE(21).
           MOVE QRB-AMT-22 TO RB-AMT(22).
           MOVE QRB-RATE-22 TO RB-RATE(22).
           MOVE QRB-AMT-23 TO RB-AMT(23).
           MOVE QRB-RATE-23 TO RB-RATE(23).
           MOVE QRB-AMT-24 TO RB-AMT(24).
           MOVE QRB-RATE-24 TO RB-RATE(24).
           MOVE QRB-AMT-25 TO RB-AMT(25).
           MOVE QRB-RATE-25 TO RB-RATE(25).
           MOVE QRB-AMT-26 TO RB-AMT(26).
           MOVE QRB-RATE-26 TO RB-RATE(26).
           MOVE QRB-AMT-27 TO RB-AMT(27).
           MOVE QRB-RATE-27 TO RB-RATE(27).
           MOVE QRB-AMT-28 TO RB-AMT(28).
           MOVE QRB-RATE-28 TO RB-RATE(28).
           MOVE QRB-AMT-29 TO RB-AMT(29).
           MOVE QRB-RATE-29 TO RB-RATE(29).
           MOVE QRB-AMT-30 TO RB-AMT(30).
           MOVE QRB-RATE-30 TO RB-RATE(30).
           MOVE QRB-AMT-31 TO RB-AMT(31).
           MOVE QRB-RATE-31 TO RB-RATE(31).
           MOVE QRB-AMT-32 TO RB-AMT(32).
           MOVE QRB-RATE-32 TO RB-RATE(32).
           MOVE QRB-AMT-33 TO RB-AMT(33).
           MOVE QRB-RATE-33 TO RB-RATE(33).
           MOVE QRB-AMT-34 TO RB-AMT(34).
           MOVE QRB-RATE-34 TO RB-RATE(34).
           MOVE QRB-AMT-35 TO RB-AMT(35).
           MOVE QRB-RATE-35 TO RB-RATE(35).
           MOVE QRB-AMT-36 TO RB-AMT(36).
           MOVE QRB-RATE-36 TO RB-RATE(36).
           MOVE QRB-AMT-37 TO RB-AMT(37).
           MOVE QRB-RATE-37 TO RB-RATE(37).
           MOVE QRB-AMT-38 TO RB-AMT(38).
           MOVE QRB-RATE-38 TO RB-RATE(38).
           MOVE QRB-AMT-39 TO RB-AMT(39).
           MOVE QRB-RATE-39 TO RB-RATE(39).
           MOVE QRB-AMT-40 TO RB-AMT(40).
           MOVE QRB-RATE-40 TO RB-RATE(40).
           MOVE QRB-AMT-41 TO RB-AMT(41).
           MOVE QRB-RATE-41 TO RB-RATE(41).
           MOVE QRB-AMT-42 TO RB-AMT(42).
           MOVE QRB-RATE-42 TO RB-RATE(42).
           MOVE QRB-AMT-43 TO RB-AMT(43).
           MOVE QRB-RATE-43 TO RB-RATE(43).
           MOVE QRB-AMT-44 TO RB-AMT(44).
           MOVE QRB-RATE-44 TO RB-RATE(44).
           MOVE QRB-AMT-45 TO RB-AMT(45).
           MOVE QRB-RATE-45 TO RB-RATE(45).
           MOVE QRB-AMT-46 TO RB-AMT(46).
           MOVE QRB-RATE-46 TO RB-RATE(46).
           MOVE QRB-AMT-47 TO RB-AMT(47).
           MOVE QRB-RATE-47 TO RB-RATE(47).
           MOVE QRB-AMT-48 TO RB-AMT(48).
           MOVE QRB-RATE-48 TO RB-RATE(48).
           MOVE QRB-AMT-49 TO RB-AMT(49).
           MOVE QRB-RATE-49 TO RB-RATE(49).
           MOVE QRB-AMT-50 TO RB-AMT(50).
           MOVE QRB-RATE-50 TO RB-RATE(50).
           MOVE QRB-AMT-51 TO RB-AMT(51).
           MOVE QRB-RATE-51 TO RB-RATE(51).
           MOVE QRB-AMT-52 TO RB-AMT(52).
           MOVE QRB-RATE-52 TO RB-RATE(52).
           MOVE QRB-AMT-53 TO RB-AMT(53).
           MOVE QRB-RATE-53 TO RB-RATE(53).
           MOVE QRB-AMT-54 TO RB-AMT(54).
           MOVE QRB-RATE-54 TO RB-RATE(54).
           MOVE QRB-AMT-55 TO RB-AMT(55).
           MOVE QRB-RATE-55 TO RB-RATE(55).
           MOVE QRB-AMT-56 TO RB-AMT(56).
           MOVE QRB-RATE-56 TO RB-RATE(56).
           MOVE QRB-AMT-57 TO RB-AMT(57).
           MOVE QRB-RATE-57 TO RB-RATE(57).
           MOVE QRB-AMT-58 TO RB-AMT(58).
           MOVE QRB-RATE-58 TO RB-RATE(58).
           MOVE QRB-AMT-59 TO RB-AMT(59).
           MOVE QRB-RATE-59 TO RB-RATE(59).
           MOVE QRB-AMT-60 TO RB-AMT(60).
           MOVE QRB-RATE-60 TO RB-RATE(60).
           MOVE QRB-AMT-61 TO RB-AMT(61).
           MOVE QRB-RATE-61 TO RB-RATE(61).
           MOVE QRB-AMT-62 TO RB-AMT(62).
           MOVE QRB-RATE-62 TO RB-RATE(62).
           MOVE QRB-AMT-63 TO RB-AMT(63).
           MOVE QRB-RATE-63 TO RB-RATE(63).
           MOVE QRB-AMT-64 TO RB-AMT(64).
           MOVE QRB-RATE-64 TO RB-RATE(64).
           MOVE QRB-AMT-65 TO RB-AMT(65).
           MOVE QRB-RATE-65 TO RB-RATE(65).
           MOVE QRB-AMT-66 TO RB-AMT(66).
           MOVE QRB-RATE-66 TO RB-RATE(66).
           MOVE QRB-AMT-67 TO RB-AMT(67).
           MOVE QRB-RATE-67 TO RB-RATE(67).
           MOVE QRB-AMT-68 TO RB-AMT(68).
           MOVE QRB-RATE-68 TO RB-RATE(68).
           MOVE QRB-AMT-69 TO RB-AMT(69).
           MOVE QRB-RATE-69 TO RB-RATE(69).
           MOVE QRB-AMT-70 TO RB-AMT(70).
           MOVE QRB-RATE-70 TO RB-RATE(70).
           MOVE QRB-AMT-71 TO RB-AMT(71).
           MOVE QRB-RATE-71 TO RB-RATE(71).
           MOVE QRB-AMT-72 TO RB-AMT(72).
           MOVE QRB-RATE-72 TO RB-RATE(72).
           MOVE QRB-AMT-73 TO RB-AMT(73).
           MOVE QRB-RATE-73 TO RB-RATE(73).
           MOVE QRB-AMT-74 TO RB-AMT(74).
           MOVE QRB-RATE-74 TO RB-RATE(74).
           MOVE QRB-AMT-75 TO RB-AMT(75).
           MOVE QRB-RATE-75 TO RB-RATE(75).

           IF ( QRB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RB-LTOUCH-DATE
           ELSE
              MOVE QRB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RB-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QRB-REC FROM RB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-RB-FIELDS.
           MOVE RB-TBL-NO TO QRB-TBL-NO.
           MOVE RB-TERM TO QRB-TERM.
           MOVE RB-MON-DAY-CD TO QRB-MON-DAY-CD.
           MOVE RB-AMT(1) TO QRB-AMT-1.
           MOVE RB-RATE(1) TO QRB-RATE-1.
           MOVE RB-AMT(2) TO QRB-AMT-2.
           MOVE RB-RATE(2) TO QRB-RATE-2.
           MOVE RB-AMT(3) TO QRB-AMT-3.
           MOVE RB-RATE(3) TO QRB-RATE-3.
           MOVE RB-AMT(4) TO QRB-AMT-4.
           MOVE RB-RATE(4) TO QRB-RATE-4.
           MOVE RB-AMT(5) TO QRB-AMT-5.
           MOVE RB-RATE(5) TO QRB-RATE-5.
           MOVE RB-AMT(6) TO QRB-AMT-6.
           MOVE RB-RATE(6) TO QRB-RATE-6.
           MOVE RB-AMT(7) TO QRB-AMT-7.
           MOVE RB-RATE(7) TO QRB-RATE-7.
           MOVE RB-AMT(8) TO QRB-AMT-8.
           MOVE RB-RATE(8) TO QRB-RATE-8.
           MOVE RB-AMT(9) TO QRB-AMT-9.
           MOVE RB-RATE(9) TO QRB-RATE-9.
           MOVE RB-AMT(10) TO QRB-AMT-10.
           MOVE RB-RATE(10) TO QRB-RATE-10.
           MOVE RB-AMT(11) TO QRB-AMT-11.
           MOVE RB-RATE(11) TO QRB-RATE-11.
           MOVE RB-AMT(12) TO QRB-AMT-12.
           MOVE RB-RATE(12) TO QRB-RATE-12.
           MOVE RB-AMT(13) TO QRB-AMT-13.
           MOVE RB-RATE(13) TO QRB-RATE-13.
           MOVE RB-AMT(14) TO QRB-AMT-14.
           MOVE RB-RATE(14) TO QRB-RATE-14.
           MOVE RB-AMT(15) TO QRB-AMT-15.
           MOVE RB-RATE(15) TO QRB-RATE-15.
           MOVE RB-AMT(16) TO QRB-AMT-16.
           MOVE RB-RATE(16) TO QRB-RATE-16.
           MOVE RB-AMT(17) TO QRB-AMT-17.
           MOVE RB-RATE(17) TO QRB-RATE-17.
           MOVE RB-AMT(18) TO QRB-AMT-18.
           MOVE RB-RATE(18) TO QRB-RATE-18.
           MOVE RB-AMT(19) TO QRB-AMT-19.
           MOVE RB-RATE(19) TO QRB-RATE-19.
           MOVE RB-AMT(20) TO QRB-AMT-20.
           MOVE RB-RATE(20) TO QRB-RATE-20.
           MOVE RB-AMT(21) TO QRB-AMT-21.
           MOVE RB-RATE(21) TO QRB-RATE-21.
           MOVE RB-AMT(22) TO QRB-AMT-22.
           MOVE RB-RATE(22) TO QRB-RATE-22.
           MOVE RB-AMT(23) TO QRB-AMT-23.
           MOVE RB-RATE(23) TO QRB-RATE-23.
           MOVE RB-AMT(24) TO QRB-AMT-24.
           MOVE RB-RATE(24) TO QRB-RATE-24.
           MOVE RB-AMT(25) TO QRB-AMT-25.
           MOVE RB-RATE(25) TO QRB-RATE-25.
           MOVE RB-AMT(26) TO QRB-AMT-26.
           MOVE RB-RATE(26) TO QRB-RATE-26.
           MOVE RB-AMT(27) TO QRB-AMT-27.
           MOVE RB-RATE(27) TO QRB-RATE-27.
           MOVE RB-AMT(28) TO QRB-AMT-28.
           MOVE RB-RATE(28) TO QRB-RATE-28.
           MOVE RB-AMT(29) TO QRB-AMT-29.
           MOVE RB-RATE(29) TO QRB-RATE-29.
           MOVE RB-AMT(30) TO QRB-AMT-30.
           MOVE RB-RATE(30) TO QRB-RATE-30.
           MOVE RB-AMT(31) TO QRB-AMT-31.
           MOVE RB-RATE(31) TO QRB-RATE-31.
           MOVE RB-AMT(32) TO QRB-AMT-32.
           MOVE RB-RATE(32) TO QRB-RATE-32.
           MOVE RB-AMT(33) TO QRB-AMT-33.
           MOVE RB-RATE(33) TO QRB-RATE-33.
           MOVE RB-AMT(34) TO QRB-AMT-34.
           MOVE RB-RATE(34) TO QRB-RATE-34.
           MOVE RB-AMT(35) TO QRB-AMT-35.
           MOVE RB-RATE(35) TO QRB-RATE-35.
           MOVE RB-AMT(36) TO QRB-AMT-36.
           MOVE RB-RATE(36) TO QRB-RATE-36.
           MOVE RB-AMT(37) TO QRB-AMT-37.
           MOVE RB-RATE(37) TO QRB-RATE-37.
           MOVE RB-AMT(38) TO QRB-AMT-38.
           MOVE RB-RATE(38) TO QRB-RATE-38.
           MOVE RB-AMT(39) TO QRB-AMT-39.
           MOVE RB-RATE(39) TO QRB-RATE-39.
           MOVE RB-AMT(40) TO QRB-AMT-40.
           MOVE RB-RATE(40) TO QRB-RATE-40.
           MOVE RB-AMT(41) TO QRB-AMT-41.
           MOVE RB-RATE(41) TO QRB-RATE-41.
           MOVE RB-AMT(42) TO QRB-AMT-42.
           MOVE RB-RATE(42) TO QRB-RATE-42.
           MOVE RB-AMT(43) TO QRB-AMT-43.
           MOVE RB-RATE(43) TO QRB-RATE-43.
           MOVE RB-AMT(44) TO QRB-AMT-44.
           MOVE RB-RATE(44) TO QRB-RATE-44.
           MOVE RB-AMT(45) TO QRB-AMT-45.
           MOVE RB-RATE(45) TO QRB-RATE-45.
           MOVE RB-AMT(46) TO QRB-AMT-46.
           MOVE RB-RATE(46) TO QRB-RATE-46.
           MOVE RB-AMT(47) TO QRB-AMT-47.
           MOVE RB-RATE(47) TO QRB-RATE-47.
           MOVE RB-AMT(48) TO QRB-AMT-48.
           MOVE RB-RATE(48) TO QRB-RATE-48.
           MOVE RB-AMT(49) TO QRB-AMT-49.
           MOVE RB-RATE(49) TO QRB-RATE-49.
           MOVE RB-AMT(50) TO QRB-AMT-50.
           MOVE RB-RATE(50) TO QRB-RATE-50.
           MOVE RB-AMT(51) TO QRB-AMT-51.
           MOVE RB-RATE(51) TO QRB-RATE-51.
           MOVE RB-AMT(52) TO QRB-AMT-52.
           MOVE RB-RATE(52) TO QRB-RATE-52.
           MOVE RB-AMT(53) TO QRB-AMT-53.
           MOVE RB-RATE(53) TO QRB-RATE-53.
           MOVE RB-AMT(54) TO QRB-AMT-54.
           MOVE RB-RATE(54) TO QRB-RATE-54.
           MOVE RB-AMT(55) TO QRB-AMT-55.
           MOVE RB-RATE(55) TO QRB-RATE-55.
           MOVE RB-AMT(56) TO QRB-AMT-56.
           MOVE RB-RATE(56) TO QRB-RATE-56.
           MOVE RB-AMT(57) TO QRB-AMT-57.
           MOVE RB-RATE(57) TO QRB-RATE-57.
           MOVE RB-AMT(58) TO QRB-AMT-58.
           MOVE RB-RATE(58) TO QRB-RATE-58.
           MOVE RB-AMT(59) TO QRB-AMT-59.
           MOVE RB-RATE(59) TO QRB-RATE-59.
           MOVE RB-AMT(60) TO QRB-AMT-60.
           MOVE RB-RATE(60) TO QRB-RATE-60.
           MOVE RB-AMT(61) TO QRB-AMT-61.
           MOVE RB-RATE(61) TO QRB-RATE-61.
           MOVE RB-AMT(62) TO QRB-AMT-62.
           MOVE RB-RATE(62) TO QRB-RATE-62.
           MOVE RB-AMT(63) TO QRB-AMT-63.
           MOVE RB-RATE(63) TO QRB-RATE-63.
           MOVE RB-AMT(64) TO QRB-AMT-64.
           MOVE RB-RATE(64) TO QRB-RATE-64.
           MOVE RB-AMT(65) TO QRB-AMT-65.
           MOVE RB-RATE(65) TO QRB-RATE-65.
           MOVE RB-AMT(66) TO QRB-AMT-66.
           MOVE RB-RATE(66) TO QRB-RATE-66.
           MOVE RB-AMT(67) TO QRB-AMT-67.
           MOVE RB-RATE(67) TO QRB-RATE-67.
           MOVE RB-AMT(68) TO QRB-AMT-68.
           MOVE RB-RATE(68) TO QRB-RATE-68.
           MOVE RB-AMT(69) TO QRB-AMT-69.
           MOVE RB-RATE(69) TO QRB-RATE-69.
           MOVE RB-AMT(70) TO QRB-AMT-70.
           MOVE RB-RATE(70) TO QRB-RATE-70.
           MOVE RB-AMT(71) TO QRB-AMT-71.
           MOVE RB-RATE(71) TO QRB-RATE-71.
           MOVE RB-AMT(72) TO QRB-AMT-72.
           MOVE RB-RATE(72) TO QRB-RATE-72.
           MOVE RB-AMT(73) TO QRB-AMT-73.
           MOVE RB-RATE(73) TO QRB-RATE-73.
           MOVE RB-AMT(74) TO QRB-AMT-74.
           MOVE RB-RATE(74) TO QRB-RATE-74.
           MOVE RB-AMT(75) TO QRB-AMT-75.
           MOVE RB-RATE(75) TO QRB-RATE-75.

           IF ( RB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QRB-LTOUCH-DATE
           ELSE
              MOVE RB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRB-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-RB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RB-ALL-MACHINE
              MOVE RB-ALL-PATH             TO RB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RB-ALL-BASE
              MOVE FILPATH-MACHINE         TO RB-ALL-MACHINE
              MOVE RB-ALL-PATH             TO RB-PATH.

*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-RBFILE NOT = "S" )
*G2G          MOVE RB-PATH-BEG             TO RB-SELECT-PATH-BEG
*G2G          MOVE RB-PATH-FILE            TO RB-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE RB-PATH-SQL             TO RB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-RB1-FILE.
           PERFORM LOAD-RB1-FILE.
           PERFORM OPEN-IT.
           MOVE RB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RB1-FILE.
           PERFORM START-IT.
           MOVE RB-SELECT-PATH TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.

           IF ( RB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RB1-FILE.

           IF ( QRB1-WEND-TBL-NO = ZEROES    ) OR
              ( QRB1-WEND-TBL-NO NOT NUMERIC )
              MOVE RB-TBL-NO    TO QRB1-WBEG-TBL-NO
              MOVE 99           TO QRB1-WEND-TBL-NO
              MOVE RB-TERM      TO QRB1-WBEG-TERM
              MOVE 999          TO QRB1-WEND-TERM.

           EXEC SQL
            DECLARE RB1_CURSOR CURSOR FOR
            SELECT RBFILE.RB_TBL_NO,
                   RBFILE.RB_TERM,
                   ISNULL(RBFILE.RB_MON_DAY_CD,''),
                   RBFILE.RB_AMT_1,
                   RBFILE.RB_RATE_1,
                   RBFILE.RB_AMT_2,
                   RBFILE.RB_RATE_2,
                   RBFILE.RB_AMT_3,
                   RBFILE.RB_RATE_3,
                   RBFILE.RB_AMT_4,
                   RBFILE.RB_RATE_4,
                   RBFILE.RB_AMT_5,
                   RBFILE.RB_RATE_5,
                   RBFILE.RB_AMT_6,
                   RBFILE.RB_RATE_6,
                   RBFILE.RB_AMT_7,
                   RBFILE.RB_RATE_7,
                   RBFILE.RB_AMT_8,
                   RBFILE.RB_RATE_8,
                   RBFILE.RB_AMT_9,
                   RBFILE.RB_RATE_9,
                   RBFILE.RB_AMT_10,
                   RBFILE.RB_RATE_10,
                   RBFILE.RB_AMT_11,
                   RBFILE.RB_RATE_11,
                   RBFILE.RB_AMT_12,
                   RBFILE.RB_RATE_12,
                   RBFILE.RB_AMT_13,
                   RBFILE.RB_RATE_13,
                   RBFILE.RB_AMT_14,
                   RBFILE.RB_RATE_14,
                   RBFILE.RB_AMT_15,
                   RBFILE.RB_RATE_15,
                   RBFILE.RB_AMT_16,
                   RBFILE.RB_RATE_16,
                   RBFILE.RB_AMT_17,
                   RBFILE.RB_RATE_17,
                   RBFILE.RB_AMT_18,
                   RBFILE.RB_RATE_18,
                   RBFILE.RB_AMT_19,
                   RBFILE.RB_RATE_19,
                   RBFILE.RB_AMT_20,
                   RBFILE.RB_RATE_20,
                   RBFILE.RB_AMT_21,
                   RBFILE.RB_RATE_21,
                   RBFILE.RB_AMT_22,
                   RBFILE.RB_RATE_22,
                   RBFILE.RB_AMT_23,
                   RBFILE.RB_RATE_23,
                   RBFILE.RB_AMT_24,
                   RBFILE.RB_RATE_24,
                   RBFILE.RB_AMT_25,
                   RBFILE.RB_RATE_25,
                   RBFILE.RB_AMT_26,
                   RBFILE.RB_RATE_26,
                   RBFILE.RB_AMT_27,
                   RBFILE.RB_RATE_27,
                   RBFILE.RB_AMT_28,
                   RBFILE.RB_RATE_28,
                   RBFILE.RB_AMT_29,
                   RBFILE.RB_RATE_29,
                   RBFILE.RB_AMT_30,
                   RBFILE.RB_RATE_30,
                   RBFILE.RB_AMT_31,
                   RBFILE.RB_RATE_31,
                   RBFILE.RB_AMT_32,
                   RBFILE.RB_RATE_32,
                   RBFILE.RB_AMT_33,
                   RBFILE.RB_RATE_33,
                   RBFILE.RB_AMT_34,
                   RBFILE.RB_RATE_34,
                   RBFILE.RB_AMT_35,
                   RBFILE.RB_RATE_35,
                   RBFILE.RB_AMT_36,
                   RBFILE.RB_RATE_36,
                   RBFILE.RB_AMT_37,
                   RBFILE.RB_RATE_37,
                   RBFILE.RB_AMT_38,
                   RBFILE.RB_RATE_38,
                   RBFILE.RB_AMT_39,
                   RBFILE.RB_RATE_39,
                   RBFILE.RB_AMT_40,
                   RBFILE.RB_RATE_40,
                   RBFILE.RB_AMT_41,
                   RBFILE.RB_RATE_41,
                   RBFILE.RB_AMT_42,
                   RBFILE.RB_RATE_42,
                   RBFILE.RB_AMT_43,
                   RBFILE.RB_RATE_43,
                   RBFILE.RB_AMT_44,
                   RBFILE.RB_RATE_44,
                   RBFILE.RB_AMT_45,
                   RBFILE.RB_RATE_45,
                   RBFILE.RB_AMT_46,
                   RBFILE.RB_RATE_46,
                   RBFILE.RB_AMT_47,
                   RBFILE.RB_RATE_47,
                   RBFILE.RB_AMT_48,
                   RBFILE.RB_RATE_48,
                   RBFILE.RB_AMT_49,
                   RBFILE.RB_RATE_49,
                   RBFILE.RB_AMT_50,
                   RBFILE.RB_RATE_50,
                   RBFILE.RB_AMT_51,
                   RBFILE.RB_RATE_51,
                   RBFILE.RB_AMT_52,
                   RBFILE.RB_RATE_52,
                   RBFILE.RB_AMT_53,
                   RBFILE.RB_RATE_53,
                   RBFILE.RB_AMT_54,
                   RBFILE.RB_RATE_54,
                   RBFILE.RB_AMT_55,
                   RBFILE.RB_RATE_55,
                   RBFILE.RB_AMT_56,
                   RBFILE.RB_RATE_56,
                   RBFILE.RB_AMT_57,
                   RBFILE.RB_RATE_57,
                   RBFILE.RB_AMT_58,
                   RBFILE.RB_RATE_58,
                   RBFILE.RB_AMT_59,
                   RBFILE.RB_RATE_59,
                   RBFILE.RB_AMT_60,
                   RBFILE.RB_RATE_60,
                   RBFILE.RB_AMT_61,
                   RBFILE.RB_RATE_61,
                   RBFILE.RB_AMT_62,
                   RBFILE.RB_RATE_62,
                   RBFILE.RB_AMT_63,
                   RBFILE.RB_RATE_63,
                   RBFILE.RB_AMT_64,
                   RBFILE.RB_RATE_64,
                   RBFILE.RB_AMT_65,
                   RBFILE.RB_RATE_65,
                   RBFILE.RB_AMT_66,
                   RBFILE.RB_RATE_66,
                   RBFILE.RB_AMT_67,
                   RBFILE.RB_RATE_67,
                   RBFILE.RB_AMT_68,
                   RBFILE.RB_RATE_68,
                   RBFILE.RB_AMT_69,
                   RBFILE.RB_RATE_69,
                   RBFILE.RB_AMT_70,
                   RBFILE.RB_RATE_70,
                   RBFILE.RB_AMT_71,
                   RBFILE.RB_RATE_71,
                   RBFILE.RB_AMT_72,
                   RBFILE.RB_RATE_72,
                   RBFILE.RB_AMT_73,
                   RBFILE.RB_RATE_73,
                   RBFILE.RB_AMT_74,
                   RBFILE.RB_RATE_74,
                   RBFILE.RB_AMT_75,
                   RBFILE.RB_RATE_75,
                   ISNULL(CAST(RBFILE.RB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.RBFILE
            WHERE RBFILE.RB_TBL_NO >= :QRB1-WBEG-TBL-NO
              AND RBFILE.RB_TBL_NO <= :QRB1-WEND-TBL-NO
              AND RBFILE.RB_TERM >= :QRB1-WBEG-TERM
              AND RBFILE.RB_TERM <= :QRB1-WEND-TERM
            ORDER BY RBFILE.RB_TBL_NO,
                     RBFILE.RB_TERM
           END-EXEC.

           EXEC SQL
               OPEN RB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRB1-WBEG-TBL-NO
                          QRB1-WEND-TBL-NO
                          QRB1-WBEG-TERM
                          QRB1-WEND-TERM.

           MOVE STAT---GOOD TO RB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RB1-FILE.
           PERFORM READ-IT.
           MOVE RB-SELECT-PATH TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.

           MOVE RB-TBL-NO    TO QRB-TBL-NO. 
           MOVE RB-TERM      TO QRB-TERM.

           EXEC SQL
            SELECT RBFILE.RB_TBL_NO,
                   RBFILE.RB_TERM,
                   ISNULL(RBFILE.RB_MON_DAY_CD,''),
                   RBFILE.RB_AMT_1,
                   RBFILE.RB_RATE_1,
                   RBFILE.RB_AMT_2,
                   RBFILE.RB_RATE_2,
                   RBFILE.RB_AMT_3,
                   RBFILE.RB_RATE_3,
                   RBFILE.RB_AMT_4,
                   RBFILE.RB_RATE_4,
                   RBFILE.RB_AMT_5,
                   RBFILE.RB_RATE_5,
                   RBFILE.RB_AMT_6,
                   RBFILE.RB_RATE_6,
                   RBFILE.RB_AMT_7,
                   RBFILE.RB_RATE_7,
                   RBFILE.RB_AMT_8,
                   RBFILE.RB_RATE_8,
                   RBFILE.RB_AMT_9,
                   RBFILE.RB_RATE_9,
                   RBFILE.RB_AMT_10,
                   RBFILE.RB_RATE_10,
                   RBFILE.RB_AMT_11,
                   RBFILE.RB_RATE_11,
                   RBFILE.RB_AMT_12,
                   RBFILE.RB_RATE_12,
                   RBFILE.RB_AMT_13,
                   RBFILE.RB_RATE_13,
                   RBFILE.RB_AMT_14,
                   RBFILE.RB_RATE_14,
                   RBFILE.RB_AMT_15,
                   RBFILE.RB_RATE_15,
                   RBFILE.RB_AMT_16,
                   RBFILE.RB_RATE_16,
                   RBFILE.RB_AMT_17,
                   RBFILE.RB_RATE_17,
                   RBFILE.RB_AMT_18,
                   RBFILE.RB_RATE_18,
                   RBFILE.RB_AMT_19,
                   RBFILE.RB_RATE_19,
                   RBFILE.RB_AMT_20,
                   RBFILE.RB_RATE_20,
                   RBFILE.RB_AMT_21,
                   RBFILE.RB_RATE_21,
                   RBFILE.RB_AMT_22,
                   RBFILE.RB_RATE_22,
                   RBFILE.RB_AMT_23,
                   RBFILE.RB_RATE_23,
                   RBFILE.RB_AMT_24,
                   RBFILE.RB_RATE_24,
                   RBFILE.RB_AMT_25,
                   RBFILE.RB_RATE_25,
                   RBFILE.RB_AMT_26,
                   RBFILE.RB_RATE_26,
                   RBFILE.RB_AMT_27,
                   RBFILE.RB_RATE_27,
                   RBFILE.RB_AMT_28,
                   RBFILE.RB_RATE_28,
                   RBFILE.RB_AMT_29,
                   RBFILE.RB_RATE_29,
                   RBFILE.RB_AMT_30,
                   RBFILE.RB_RATE_30,
                   RBFILE.RB_AMT_31,
                   RBFILE.RB_RATE_31,
                   RBFILE.RB_AMT_32,
                   RBFILE.RB_RATE_32,
                   RBFILE.RB_AMT_33,
                   RBFILE.RB_RATE_33,
                   RBFILE.RB_AMT_34,
                   RBFILE.RB_RATE_34,
                   RBFILE.RB_AMT_35,
                   RBFILE.RB_RATE_35,
                   RBFILE.RB_AMT_36,
                   RBFILE.RB_RATE_36,
                   RBFILE.RB_AMT_37,
                   RBFILE.RB_RATE_37,
                   RBFILE.RB_AMT_38,
                   RBFILE.RB_RATE_38,
                   RBFILE.RB_AMT_39,
                   RBFILE.RB_RATE_39,
                   RBFILE.RB_AMT_40,
                   RBFILE.RB_RATE_40,
                   RBFILE.RB_AMT_41,
                   RBFILE.RB_RATE_41,
                   RBFILE.RB_AMT_42,
                   RBFILE.RB_RATE_42,
                   RBFILE.RB_AMT_43,
                   RBFILE.RB_RATE_43,
                   RBFILE.RB_AMT_44,
                   RBFILE.RB_RATE_44,
                   RBFILE.RB_AMT_45,
                   RBFILE.RB_RATE_45,
                   RBFILE.RB_AMT_46,
                   RBFILE.RB_RATE_46,
                   RBFILE.RB_AMT_47,
                   RBFILE.RB_RATE_47,
                   RBFILE.RB_AMT_48,
                   RBFILE.RB_RATE_48,
                   RBFILE.RB_AMT_49,
                   RBFILE.RB_RATE_49,
                   RBFILE.RB_AMT_50,
                   RBFILE.RB_RATE_50,
                   RBFILE.RB_AMT_51,
                   RBFILE.RB_RATE_51,
                   RBFILE.RB_AMT_52,
                   RBFILE.RB_RATE_52,
                   RBFILE.RB_AMT_53,
                   RBFILE.RB_RATE_53,
                   RBFILE.RB_AMT_54,
                   RBFILE.RB_RATE_54,
                   RBFILE.RB_AMT_55,
                   RBFILE.RB_RATE_55,
                   RBFILE.RB_AMT_56,
                   RBFILE.RB_RATE_56,
                   RBFILE.RB_AMT_57,
                   RBFILE.RB_RATE_57,
                   RBFILE.RB_AMT_58,
                   RBFILE.RB_RATE_58,
                   RBFILE.RB_AMT_59,
                   RBFILE.RB_RATE_59,
                   RBFILE.RB_AMT_60,
                   RBFILE.RB_RATE_60,
                   RBFILE.RB_AMT_61,
                   RBFILE.RB_RATE_61,
                   RBFILE.RB_AMT_62,
                   RBFILE.RB_RATE_62,
                   RBFILE.RB_AMT_63,
                   RBFILE.RB_RATE_63,
                   RBFILE.RB_AMT_64,
                   RBFILE.RB_RATE_64,
                   RBFILE.RB_AMT_65,
                   RBFILE.RB_RATE_65,
                   RBFILE.RB_AMT_66,
                   RBFILE.RB_RATE_66,
                   RBFILE.RB_AMT_67,
                   RBFILE.RB_RATE_67,
                   RBFILE.RB_AMT_68,
                   RBFILE.RB_RATE_68,
                   RBFILE.RB_AMT_69,
                   RBFILE.RB_RATE_69,
                   RBFILE.RB_AMT_70,
                   RBFILE.RB_RATE_70,
                   RBFILE.RB_AMT_71,
                   RBFILE.RB_RATE_71,
                   RBFILE.RB_AMT_72,
                   RBFILE.RB_RATE_72,
                   RBFILE.RB_AMT_73,
                   RBFILE.RB_RATE_73,
                   RBFILE.RB_AMT_74,
                   RBFILE.RB_RATE_74,
                   RBFILE.RB_AMT_75,
                   RBFILE.RB_RATE_75,
                   ISNULL(CAST(RBFILE.RB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QRB-REC
            FROM DBO.RBFILE
            WHERE RBFILE.RB_TBL_NO = :QRB-TBL-NO
              AND RBFILE.RB_TERM = :QRB-TERM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RB-FIELDS.

      *-----------------------------------------------------------------
       READ-RB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RB-SELECT-PATH TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           INITIALIZE QRB-REC.

           IF ( RB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RB1_CURSOR INTO :QRB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-RB1-FILE.

           ACCEPT RB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE RB-SELECT-PATH TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           PERFORM SET-RB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.RBFILE
             (RBFILE.RB_TBL_NO,
              RBFILE.RB_TERM,
              RBFILE.RB_MON_DAY_CD,
              RBFILE.RB_AMT_1,
              RBFILE.RB_RATE_1,
              RBFILE.RB_AMT_2,
              RBFILE.RB_RATE_2,
              RBFILE.RB_AMT_3,
              RBFILE.RB_RATE_3,
              RBFILE.RB_AMT_4,
              RBFILE.RB_RATE_4,
              RBFILE.RB_AMT_5,
              RBFILE.RB_RATE_5,
              RBFILE.RB_AMT_6,
              RBFILE.RB_RATE_6,
              RBFILE.RB_AMT_7,
              RBFILE.RB_RATE_7,
              RBFILE.RB_AMT_8,
              RBFILE.RB_RATE_8,
              RBFILE.RB_AMT_9,
              RBFILE.RB_RATE_9,
              RBFILE.RB_AMT_10,
              RBFILE.RB_RATE_10,
              RBFILE.RB_AMT_11,
              RBFILE.RB_RATE_11,
              RBFILE.RB_AMT_12,
              RBFILE.RB_RATE_12,
              RBFILE.RB_AMT_13,
              RBFILE.RB_RATE_13,
              RBFILE.RB_AMT_14,
              RBFILE.RB_RATE_14,
              RBFILE.RB_AMT_15,
              RBFILE.RB_RATE_15,
              RBFILE.RB_AMT_16,
              RBFILE.RB_RATE_16,
              RBFILE.RB_AMT_17,
              RBFILE.RB_RATE_17,
              RBFILE.RB_AMT_18,
              RBFILE.RB_RATE_18,
              RBFILE.RB_AMT_19,
              RBFILE.RB_RATE_19,
              RBFILE.RB_AMT_20,
              RBFILE.RB_RATE_20,
              RBFILE.RB_AMT_21,
              RBFILE.RB_RATE_21,
              RBFILE.RB_AMT_22,
              RBFILE.RB_RATE_22,
              RBFILE.RB_AMT_23,
              RBFILE.RB_RATE_23,
              RBFILE.RB_AMT_24,
              RBFILE.RB_RATE_24,
              RBFILE.RB_AMT_25,
              RBFILE.RB_RATE_25,
              RBFILE.RB_AMT_26,
              RBFILE.RB_RATE_26,
              RBFILE.RB_AMT_27,
              RBFILE.RB_RATE_27,
              RBFILE.RB_AMT_28,
              RBFILE.RB_RATE_28,
              RBFILE.RB_AMT_29,
              RBFILE.RB_RATE_29,
              RBFILE.RB_AMT_30,
              RBFILE.RB_RATE_30,
              RBFILE.RB_AMT_31,
              RBFILE.RB_RATE_31,
              RBFILE.RB_AMT_32,
              RBFILE.RB_RATE_32,
              RBFILE.RB_AMT_33,
              RBFILE.RB_RATE_33,
              RBFILE.RB_AMT_34,
              RBFILE.RB_RATE_34,
              RBFILE.RB_AMT_35,
              RBFILE.RB_RATE_35,
              RBFILE.RB_AMT_36,
              RBFILE.RB_RATE_36,
              RBFILE.RB_AMT_37,
              RBFILE.RB_RATE_37,
              RBFILE.RB_AMT_38,
              RBFILE.RB_RATE_38,
              RBFILE.RB_AMT_39,
              RBFILE.RB_RATE_39,
              RBFILE.RB_AMT_40,
              RBFILE.RB_RATE_40,
              RBFILE.RB_AMT_41,
              RBFILE.RB_RATE_41,
              RBFILE.RB_AMT_42,
              RBFILE.RB_RATE_42,
              RBFILE.RB_AMT_43,
              RBFILE.RB_RATE_43,
              RBFILE.RB_AMT_44,
              RBFILE.RB_RATE_44,
              RBFILE.RB_AMT_45,
              RBFILE.RB_RATE_45,
              RBFILE.RB_AMT_46,
              RBFILE.RB_RATE_46,
              RBFILE.RB_AMT_47,
              RBFILE.RB_RATE_47,
              RBFILE.RB_AMT_48,
              RBFILE.RB_RATE_48,
              RBFILE.RB_AMT_49,
              RBFILE.RB_RATE_49,
              RBFILE.RB_AMT_50,
              RBFILE.RB_RATE_50,
              RBFILE.RB_AMT_51,
              RBFILE.RB_RATE_51,
              RBFILE.RB_AMT_52,
              RBFILE.RB_RATE_52,
              RBFILE.RB_AMT_53,
              RBFILE.RB_RATE_53,
              RBFILE.RB_AMT_54,
              RBFILE.RB_RATE_54,
              RBFILE.RB_AMT_55,
              RBFILE.RB_RATE_55,
              RBFILE.RB_AMT_56,
              RBFILE.RB_RATE_56,
              RBFILE.RB_AMT_57,
              RBFILE.RB_RATE_57,
              RBFILE.RB_AMT_58,
              RBFILE.RB_RATE_58,
              RBFILE.RB_AMT_59,
              RBFILE.RB_RATE_59,
              RBFILE.RB_AMT_60,
              RBFILE.RB_RATE_60,
              RBFILE.RB_AMT_61,
              RBFILE.RB_RATE_61,
              RBFILE.RB_AMT_62,
              RBFILE.RB_RATE_62,
              RBFILE.RB_AMT_63,
              RBFILE.RB_RATE_63,
              RBFILE.RB_AMT_64,
              RBFILE.RB_RATE_64,
              RBFILE.RB_AMT_65,
              RBFILE.RB_RATE_65,
              RBFILE.RB_AMT_66,
              RBFILE.RB_RATE_66,
              RBFILE.RB_AMT_67,
              RBFILE.RB_RATE_67,
              RBFILE.RB_AMT_68,
              RBFILE.RB_RATE_68,
              RBFILE.RB_AMT_69,
              RBFILE.RB_RATE_69,
              RBFILE.RB_AMT_70,
              RBFILE.RB_RATE_70,
              RBFILE.RB_AMT_71,
              RBFILE.RB_RATE_71,
              RBFILE.RB_AMT_72,
              RBFILE.RB_RATE_72,
              RBFILE.RB_AMT_73,
              RBFILE.RB_RATE_73,
              RBFILE.RB_AMT_74,
              RBFILE.RB_RATE_74,
              RBFILE.RB_AMT_75,
              RBFILE.RB_RATE_75,
              RBFILE.RB_LTOUCH_DATE)
            VALUES
             (:QRB-TBL-NO,
              :QRB-TERM,
              :QRB-MON-DAY-CD,
              :QRB-AMT-1,
              :QRB-RATE-1,
              :QRB-AMT-2,
              :QRB-RATE-2,
              :QRB-AMT-3,
              :QRB-RATE-3,
              :QRB-AMT-4,
              :QRB-RATE-4,
              :QRB-AMT-5,
              :QRB-RATE-5,
              :QRB-AMT-6,
              :QRB-RATE-6,
              :QRB-AMT-7,
              :QRB-RATE-7,
              :QRB-AMT-8,
              :QRB-RATE-8,
              :QRB-AMT-9,
              :QRB-RATE-9,
              :QRB-AMT-10,
              :QRB-RATE-10,
              :QRB-AMT-11,
              :QRB-RATE-11,
              :QRB-AMT-12,
              :QRB-RATE-12,
              :QRB-AMT-13,
              :QRB-RATE-13,
              :QRB-AMT-14,
              :QRB-RATE-14,
              :QRB-AMT-15,
              :QRB-RATE-15,
              :QRB-AMT-16,
              :QRB-RATE-16,
              :QRB-AMT-17,
              :QRB-RATE-17,
              :QRB-AMT-18,
              :QRB-RATE-18,
              :QRB-AMT-19,
              :QRB-RATE-19,
              :QRB-AMT-20,
              :QRB-RATE-20,
              :QRB-AMT-21,
              :QRB-RATE-21,
              :QRB-AMT-22,
              :QRB-RATE-22,
              :QRB-AMT-23,
              :QRB-RATE-23,
              :QRB-AMT-24,
              :QRB-RATE-24,
              :QRB-AMT-25,
              :QRB-RATE-25,
              :QRB-AMT-26,
              :QRB-RATE-26,
              :QRB-AMT-27,
              :QRB-RATE-27,
              :QRB-AMT-28,
              :QRB-RATE-28,
              :QRB-AMT-29,
              :QRB-RATE-29,
              :QRB-AMT-30,
              :QRB-RATE-30,
              :QRB-AMT-31,
              :QRB-RATE-31,
              :QRB-AMT-32,
              :QRB-RATE-32,
              :QRB-AMT-33,
              :QRB-RATE-33,
              :QRB-AMT-34,
              :QRB-RATE-34,
              :QRB-AMT-35,
              :QRB-RATE-35,
              :QRB-AMT-36,
              :QRB-RATE-36,
              :QRB-AMT-37,
              :QRB-RATE-37,
              :QRB-AMT-38,
              :QRB-RATE-38,
              :QRB-AMT-39,
              :QRB-RATE-39,
              :QRB-AMT-40,
              :QRB-RATE-40,
              :QRB-AMT-41,
              :QRB-RATE-41,
              :QRB-AMT-42,
              :QRB-RATE-42,
              :QRB-AMT-43,
              :QRB-RATE-43,
              :QRB-AMT-44,
              :QRB-RATE-44,
              :QRB-AMT-45,
              :QRB-RATE-45,
              :QRB-AMT-46,
              :QRB-RATE-46,
              :QRB-AMT-47,
              :QRB-RATE-47,
              :QRB-AMT-48,
              :QRB-RATE-48,
              :QRB-AMT-49,
              :QRB-RATE-49,
              :QRB-AMT-50,
              :QRB-RATE-50,
              :QRB-AMT-51,
              :QRB-RATE-51,
              :QRB-AMT-52,
              :QRB-RATE-52,
              :QRB-AMT-53,
              :QRB-RATE-53,
              :QRB-AMT-54,
              :QRB-RATE-54,
              :QRB-AMT-55,
              :QRB-RATE-55,
              :QRB-AMT-56,
              :QRB-RATE-56,
              :QRB-AMT-57,
              :QRB-RATE-57,
              :QRB-AMT-58,
              :QRB-RATE-58,
              :QRB-AMT-59,
              :QRB-RATE-59,
              :QRB-AMT-60,
              :QRB-RATE-60,
              :QRB-AMT-61,
              :QRB-RATE-61,
              :QRB-AMT-62,
              :QRB-RATE-62,
              :QRB-AMT-63,
              :QRB-RATE-63,
              :QRB-AMT-64,
              :QRB-RATE-64,
              :QRB-AMT-65,
              :QRB-RATE-65,
              :QRB-AMT-66,
              :QRB-RATE-66,
              :QRB-AMT-67,
              :QRB-RATE-67,
              :QRB-AMT-68,
              :QRB-RATE-68,
              :QRB-AMT-69,
              :QRB-RATE-69,
              :QRB-AMT-70,
              :QRB-RATE-70,
              :QRB-AMT-71,
              :QRB-RATE-71,
              :QRB-AMT-72,
              :QRB-RATE-72,
              :QRB-AMT-73,
              :QRB-RATE-73,
              :QRB-AMT-74,
              :QRB-RATE-74,
              :QRB-AMT-75,
              :QRB-RATE-75,
              NULLIF(:QRB-LTOUCH-DATE,'1900-01-01'))
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-RB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-RB1-FILE.

           ACCEPT RB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE RB-SELECT-PATH TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           PERFORM SET-RB-FIELDS.

           EXEC SQL
            UPDATE DBO.RBFILE
             SET
              RBFILE.RB_TBL_NO = :QRB-TBL-NO,
              RBFILE.RB_TERM = :QRB-TERM,
              RBFILE.RB_MON_DAY_CD = :QRB-MON-DAY-CD,
              RBFILE.RB_AMT_1 = :QRB-AMT-1,
              RBFILE.RB_RATE_1 = :QRB-RATE-1,
              RBFILE.RB_AMT_2 = :QRB-AMT-2,
              RBFILE.RB_RATE_2 = :QRB-RATE-2,
              RBFILE.RB_AMT_3 = :QRB-AMT-3,
              RBFILE.RB_RATE_3 = :QRB-RATE-3,
              RBFILE.RB_AMT_4 = :QRB-AMT-4,
              RBFILE.RB_RATE_4 = :QRB-RATE-4,
              RBFILE.RB_AMT_5 = :QRB-AMT-5,
              RBFILE.RB_RATE_5 = :QRB-RATE-5,
              RBFILE.RB_AMT_6 = :QRB-AMT-6,
              RBFILE.RB_RATE_6 = :QRB-RATE-6,
              RBFILE.RB_AMT_7 = :QRB-AMT-7,
              RBFILE.RB_RATE_7 = :QRB-RATE-7,
              RBFILE.RB_AMT_8 = :QRB-AMT-8,
              RBFILE.RB_RATE_8 = :QRB-RATE-8,
              RBFILE.RB_AMT_9 = :QRB-AMT-9,
              RBFILE.RB_RATE_9 = :QRB-RATE-9,
              RBFILE.RB_AMT_10 = :QRB-AMT-10,
              RBFILE.RB_RATE_10 = :QRB-RATE-10,
              RBFILE.RB_AMT_11 = :QRB-AMT-11,
              RBFILE.RB_RATE_11 = :QRB-RATE-11,
              RBFILE.RB_AMT_12 = :QRB-AMT-12,
              RBFILE.RB_RATE_12 = :QRB-RATE-12,
              RBFILE.RB_AMT_13 = :QRB-AMT-13,
              RBFILE.RB_RATE_13 = :QRB-RATE-13,
              RBFILE.RB_AMT_14 = :QRB-AMT-14,
              RBFILE.RB_RATE_14 = :QRB-RATE-14,
              RBFILE.RB_AMT_15 = :QRB-AMT-15,
              RBFILE.RB_RATE_15 = :QRB-RATE-15,
              RBFILE.RB_AMT_16 = :QRB-AMT-16,
              RBFILE.RB_RATE_16 = :QRB-RATE-16,
              RBFILE.RB_AMT_17 = :QRB-AMT-17,
              RBFILE.RB_RATE_17 = :QRB-RATE-17,
              RBFILE.RB_AMT_18 = :QRB-AMT-18,
              RBFILE.RB_RATE_18 = :QRB-RATE-18,
              RBFILE.RB_AMT_19 = :QRB-AMT-19,
              RBFILE.RB_RATE_19 = :QRB-RATE-19,
              RBFILE.RB_AMT_20 = :QRB-AMT-20,
              RBFILE.RB_RATE_20 = :QRB-RATE-20,
              RBFILE.RB_AMT_21 = :QRB-AMT-21,
              RBFILE.RB_RATE_21 = :QRB-RATE-21,
              RBFILE.RB_AMT_22 = :QRB-AMT-22,
              RBFILE.RB_RATE_22 = :QRB-RATE-22,
              RBFILE.RB_AMT_23 = :QRB-AMT-23,
              RBFILE.RB_RATE_23 = :QRB-RATE-23,
              RBFILE.RB_AMT_24 = :QRB-AMT-24,
              RBFILE.RB_RATE_24 = :QRB-RATE-24,
              RBFILE.RB_AMT_25 = :QRB-AMT-25,
              RBFILE.RB_RATE_25 = :QRB-RATE-25,
              RBFILE.RB_AMT_26 = :QRB-AMT-26,
              RBFILE.RB_RATE_26 = :QRB-RATE-26,
              RBFILE.RB_AMT_27 = :QRB-AMT-27,
              RBFILE.RB_RATE_27 = :QRB-RATE-27,
              RBFILE.RB_AMT_28 = :QRB-AMT-28,
              RBFILE.RB_RATE_28 = :QRB-RATE-28,
              RBFILE.RB_AMT_29 = :QRB-AMT-29,
              RBFILE.RB_RATE_29 = :QRB-RATE-29,
              RBFILE.RB_AMT_30 = :QRB-AMT-30,
              RBFILE.RB_RATE_30 = :QRB-RATE-30,
              RBFILE.RB_AMT_31 = :QRB-AMT-31,
              RBFILE.RB_RATE_31 = :QRB-RATE-31,
              RBFILE.RB_AMT_32 = :QRB-AMT-32,
              RBFILE.RB_RATE_32 = :QRB-RATE-32,
              RBFILE.RB_AMT_33 = :QRB-AMT-33,
              RBFILE.RB_RATE_33 = :QRB-RATE-33,
              RBFILE.RB_AMT_34 = :QRB-AMT-34,
              RBFILE.RB_RATE_34 = :QRB-RATE-34,
              RBFILE.RB_AMT_35 = :QRB-AMT-35,
              RBFILE.RB_RATE_35 = :QRB-RATE-35,
              RBFILE.RB_AMT_36 = :QRB-AMT-36,
              RBFILE.RB_RATE_36 = :QRB-RATE-36,
              RBFILE.RB_AMT_37 = :QRB-AMT-37,
              RBFILE.RB_RATE_37 = :QRB-RATE-37,
              RBFILE.RB_AMT_38 = :QRB-AMT-38,
              RBFILE.RB_RATE_38 = :QRB-RATE-38,
              RBFILE.RB_AMT_39 = :QRB-AMT-39,
              RBFILE.RB_RATE_39 = :QRB-RATE-39,
              RBFILE.RB_AMT_40 = :QRB-AMT-40,
              RBFILE.RB_RATE_40 = :QRB-RATE-40,
              RBFILE.RB_AMT_41 = :QRB-AMT-41,
              RBFILE.RB_RATE_41 = :QRB-RATE-41,
              RBFILE.RB_AMT_42 = :QRB-AMT-42,
              RBFILE.RB_RATE_42 = :QRB-RATE-42,
              RBFILE.RB_AMT_43 = :QRB-AMT-43,
              RBFILE.RB_RATE_43 = :QRB-RATE-43,
              RBFILE.RB_AMT_44 = :QRB-AMT-44,
              RBFILE.RB_RATE_44 = :QRB-RATE-44,
              RBFILE.RB_AMT_45 = :QRB-AMT-45,
              RBFILE.RB_RATE_45 = :QRB-RATE-45,
              RBFILE.RB_AMT_46 = :QRB-AMT-46,
              RBFILE.RB_RATE_46 = :QRB-RATE-46,
              RBFILE.RB_AMT_47 = :QRB-AMT-47,
              RBFILE.RB_RATE_47 = :QRB-RATE-47,
              RBFILE.RB_AMT_48 = :QRB-AMT-48,
              RBFILE.RB_RATE_48 = :QRB-RATE-48,
              RBFILE.RB_AMT_49 = :QRB-AMT-49,
              RBFILE.RB_RATE_49 = :QRB-RATE-49,
              RBFILE.RB_AMT_50 = :QRB-AMT-50,
              RBFILE.RB_RATE_50 = :QRB-RATE-50,
              RBFILE.RB_AMT_51 = :QRB-AMT-51,
              RBFILE.RB_RATE_51 = :QRB-RATE-51,
              RBFILE.RB_AMT_52 = :QRB-AMT-52,
              RBFILE.RB_RATE_52 = :QRB-RATE-52,
              RBFILE.RB_AMT_53 = :QRB-AMT-53,
              RBFILE.RB_RATE_53 = :QRB-RATE-53,
              RBFILE.RB_AMT_54 = :QRB-AMT-54,
              RBFILE.RB_RATE_54 = :QRB-RATE-54,
              RBFILE.RB_AMT_55 = :QRB-AMT-55,
              RBFILE.RB_RATE_55 = :QRB-RATE-55,
              RBFILE.RB_AMT_56 = :QRB-AMT-56,
              RBFILE.RB_RATE_56 = :QRB-RATE-56,
              RBFILE.RB_AMT_57 = :QRB-AMT-57,
              RBFILE.RB_RATE_57 = :QRB-RATE-57,
              RBFILE.RB_AMT_58 = :QRB-AMT-58,
              RBFILE.RB_RATE_58 = :QRB-RATE-58,
              RBFILE.RB_AMT_59 = :QRB-AMT-59,
              RBFILE.RB_RATE_59 = :QRB-RATE-59,
              RBFILE.RB_AMT_60 = :QRB-AMT-60,
              RBFILE.RB_RATE_60 = :QRB-RATE-60,
              RBFILE.RB_AMT_61 = :QRB-AMT-61,
              RBFILE.RB_RATE_61 = :QRB-RATE-61,
              RBFILE.RB_AMT_62 = :QRB-AMT-62,
              RBFILE.RB_RATE_62 = :QRB-RATE-62,
              RBFILE.RB_AMT_63 = :QRB-AMT-63,
              RBFILE.RB_RATE_63 = :QRB-RATE-63,
              RBFILE.RB_AMT_64 = :QRB-AMT-64,
              RBFILE.RB_RATE_64 = :QRB-RATE-64,
              RBFILE.RB_AMT_65 = :QRB-AMT-65,
              RBFILE.RB_RATE_65 = :QRB-RATE-65,
              RBFILE.RB_AMT_66 = :QRB-AMT-66,
              RBFILE.RB_RATE_66 = :QRB-RATE-66,
              RBFILE.RB_AMT_67 = :QRB-AMT-67,
              RBFILE.RB_RATE_67 = :QRB-RATE-67,
              RBFILE.RB_AMT_68 = :QRB-AMT-68,
              RBFILE.RB_RATE_68 = :QRB-RATE-68,
              RBFILE.RB_AMT_69 = :QRB-AMT-69,
              RBFILE.RB_RATE_69 = :QRB-RATE-69,
              RBFILE.RB_AMT_70 = :QRB-AMT-70,
              RBFILE.RB_RATE_70 = :QRB-RATE-70,
              RBFILE.RB_AMT_71 = :QRB-AMT-71,
              RBFILE.RB_RATE_71 = :QRB-RATE-71,
              RBFILE.RB_AMT_72 = :QRB-AMT-72,
              RBFILE.RB_RATE_72 = :QRB-RATE-72,
              RBFILE.RB_AMT_73 = :QRB-AMT-73,
              RBFILE.RB_RATE_73 = :QRB-RATE-73,
              RBFILE.RB_AMT_74 = :QRB-AMT-74,
              RBFILE.RB_RATE_74 = :QRB-RATE-74,
              RBFILE.RB_AMT_75 = :QRB-AMT-75,
              RBFILE.RB_RATE_75 = :QRB-RATE-75,
              RBFILE.RB_LTOUCH_DATE =
                 NULLIF(:QRB-LTOUCH-DATE,'1900-01-01')
            WHERE RBFILE.RB_TBL_NO = :QRB-TBL-NO
              AND RBFILE.RB_TERM = :QRB-TERM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-RB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-RB1-FILE.

           PERFORM DELETE-IT.
           MOVE RB-SELECT-PATH TO E-FILE.
           MOVE RB1-KEY TO E-KEYX.
           PERFORM SET-RB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.RBFILE
            WHERE RBFILLE.RB_TBL_NO = :QRB-TBL-NO
              AND RBFILLE.RB_TERM = :QRB-TERM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-RB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-RB1-FILE.
           IF ( RB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RB1-CURSOR-STAT.

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
              DESTROY IORBIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
