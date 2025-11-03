       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMCLRG.
       DATE-WRITTEN. 052897.
      ******************************************************************
      *
      *    DESCRIPTION:  CLASS (ACCOUNT) TRANSFER REGISTER
      *                      TR1-CODE = "J"
      *
      * REV:
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  GLF 043099 WAS MOVING SPACES TO DETAIL-LINE ON SUMMARY ONLY IN
      *             WRONG SPOT CAUSING LINES TO BE MISSING, IS NOW FIXED
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  GLF 081999 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  KEC 000728 {PR#01273} PARADATA FINANCIAL [JUDY]
      *              REPLACED THE PART IN THE DETAIL-LINE IN DISPLAYING
      *              THE ACCTNO WITH SOME STATUS FLAGS WITH STANDARD
      *              LAYOUT FROM LIBLP/LPSTAT (LPS-DISPLAY-LNNO).
      *   CS 081111  ADDED GP-INCL-OTH-IN-EARNED = 'R', THIS IS REGACC
      *              OPTION TO EXCLUDE OTHER(6) AND MAINTFEE(4) FROM
      *              EARNED/UNEARNED REGACC PR#210
      *  BAH 170526 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181207 GLOBAL LNFILE
      *  BAH 20190919 REMOVED ACCESS-CALL ON TRJFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *                 SUMMARY TOTALS WORK RECORD
      *-----------------------------------------------------------------
      *  WK-AMT(1)  - # OF LOANS         WK-AMT(14) - TJ-INSPREM(3)
      *  WK-AMT(2)  - TJ-LNAMT           WK-AMT(15) - TJ-INSPREM(3)
      *  WK-AMT(3)  - TJ-FINCHG          WK-AMT(16) - TJ-INSPREM(3)
      *  WK-AMT(4)  - TJ-UNEARN(1+6)     WK-AMT(17) - TJ-FEEAMT(1)
      *  WK-AMT(5)  - TJ-INSPREM(1)      WK-AMT(18) - TJ-FEEAMT(1)
      *  WK-AMT(6)  - TJ-INSPREM(1)      WK-AMT(19) - TJ-FEEAMT(1)
      *  WK-AMT(7)  - TJ-INSPREM(1)      WK-AMT(20) - TJ-DLRVPRIN(1)
      *  WK-AMT(8)  - TJ-INSPREM(1)      WK-AMT(21) - TJ-DLRVPRIN(2)
      *  WK-AMT(9)  - TJ-INSPREM(1)      WK-AMT(22) - TJ-DLDISC
      *  WK-AMT(10) - TJ-INSPREM(1)      WK-AMT(23) - TJ-DLPARVHELD
      *  WK-AMT(11) - TJ-INSPREM(2)      WK-AMT(24) - TJ-PROCEEDS
      *  WK-AMT(12) - TJ-INSPREM(2)      WK-AMT(25) - TJ-PURBAL
      *  WK-AMT(13) - TJ-INSPREM(2)      WK-AMT(26) - TJ-ANTICERN(1+6)
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR        PIC 9(4).
               05  WK-CLASS     PIC 9(3).
               05  WK-CODE      PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  WK-ID                  PIC X.
           03  WK-NO                  PIC 9(9).
           03  WK-AMT  OCCURS 26      PIC S9(7)V99 COMP-3.

      ******************************************************************
      * SORTED LNFILE BY BRANCH, CLASS, ACCOUNT
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH           PIC 9(4).
               05  WK2-CLASS            PIC 9(3).
               05  WK2-ACCTNO           PIC 9(8).
               05  WK2-DATE             PIC 9(8).
               05  WK2-LAWCODE          PIC 99.
               05  WK2-LINE             PIC 9(3).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMCLRG.CBL S35 02/21/24-10:33:01 >".
      ******************************************************************
      *
      *   F I L E   P A T H S
      *
      ******************************************************************
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  BR-IFN               PIC X(2)     VALUE "BR".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK2-PATH             PIC X(35).

      *-----------------------------------------------------------------
      *    HOLD-AMT IS AN EXACT DUPLICATE OF WK-AMT
      *-----------------------------------------------------------------
       01  HOLD-AMT             OCCURS 26    PIC S9(7)V99 COMP-3.

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  BRANCH-TOTALS-TAB.
           03  TOT-BR-AMT          PIC S9(7)V99 COMP-3 OCCURS 26.
       01  LOAN-CLASS-TOTALS-TAB.
           03  TOT-LC-AMT          PIC S9(7)V99 COMP-3 OCCURS 26.
       01  GRAND-TOTALS-TAB.
           03  TOT-GD-AMT          PIC S9(7)V99 COMP-3 OCCURS 26.

       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP-3.

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).
           03  SAVE-WK-CODE     PIC XXX.

       01  SUMMARY-FG           PIC X          VALUE "N".
       01  TOTAL-HEAD-FG        PIC X          VALUE "N".

       01  FIRST-TIME           PIC X          VALUE "Y".
       01  MESS-LIST            PIC X(5)       VALUE "MESS.".
       01  WS-UNEARN            PIC S9(7)V99   VALUE ZEROES.

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NOLOANS         PIC S9(7)     COMP.
               05  WS-GTT-LNAMT           PIC S9(7)V99  COMP.
               05  WS-GTT-FINCHG          PIC S9(7)V99  COMP.
               05  WS-GTT-UNEARN          PIC S9(7)V99  COMP.
               05  WS-GTT-TRBAL           PIC S9(7)V99  COMP.
      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

       01  CLASS-FG             PIC X(3)    VALUE "   ".
           88  CLASS-BEG                    VALUE "BEG".
           88  CLASS-END                    VALUE "END".
       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GTT-ISO         PIC X.
           03  SUM-ONLY        PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X(1) OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "C".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(31)    VALUE " ".
           03  FILLER           PIC X(33) VALUE
               "CLASS (ACCOUNT) TRANSFER REGISTER".
           03  FILLER           PIC X(38)    VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(29) VALUE " ".
           03  FILLER           PIC X(68) VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(47) VALUE
           "ACCOUNT           MAKER".
           03  FILLER           PIC X(49) VALUE
           "LOANDATE   TRANSFRD   DEALER        LOANAMT".
           03  FILLER           PIC X(36) VALUE
           "FINCHG      UNEARN     BALANCE  FROM".

      *-----------------------------------------------------------------
       01  HEAD8.
           03  FILLER           PIC X(20) VALUE " ".
           03  FILLER           PIC X(48) VALUE
           "LOANS        LOANAMT           FINCHG".
           03  FILLER           PIC X(64) VALUE
           "UNEARN          BALANCE".

       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.


      *-----------------------------------------------------------------
      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.

      *KC->03  D-NUMBER         PIC Z(6).
      *    03  FILLER           PIC X.
      *    03  D-PLTYPE         PIC X.
      *    03  D-REPOCD         PIC X.
      *    03  FILLER           PIC XX.
      *    03  D-MAKER          PIC X(30).
      *KC->03  FILLER           PIC X(6).

           03  D-DISPLAY-LNNO   PIC X(16).
           03  FILLER           PIC X(02).
           03  D-MAKER          PIC X(27).
           03  FILLER           PIC X(02).
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC X(3).
           03  D-TRANSDATE      PIC 99/99/99.
           03  FILLER           PIC X(3).
           03  D-DLNO           PIC Z(4).99.
           03  FILLER           PIC X(5).
           03  D-LNAMT          PIC Z(6).99-.
           03  FILLER           PIC X(2).
           03  D-FINCHG         PIC Z(6).99-.
           03  FILLER           PIC X(2).
           03  D-UNEARN         PIC Z(6).99-.
           03  FILLER           PIC X(2).
           03  D-BALANCE        PIC Z(6).99-   BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-FROM           PIC 99.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D2-GROUP             PIC X(19).
           03  D2-GROUP-1           REDEFINES D2-GROUP.
               05  S-TOTNAME        PIC X(19).
           03  D2-GROUP-2           REDEFINES D2-GROUP.
               05  D2-PROMPT1       PIC X(7).
               05  FILLER           PIC X(2).
               05  D2-GRNO          PIC X(4).
               05  D2-BRNO          REDEFINES D2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D2-GR-LEVEL      PIC 9(1).
               05  FILLER           PIC X(2).
           03  D2-GROUP-3           REDEFINES D2-GROUP.
               05  D2-PROMPT2       PIC X(19).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  FILLER           PIC X.
           03  D-S-NO-LOANS     PIC ZZZZ-.
           03  FILLER           PIC X(2).
           03  D-S-LNAMT        PIC ZZ,ZZZ,999.99- BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-S-FINCHG       PIC ZZ,ZZZ,999.99- BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-S-UNEARN       PIC ZZ,ZZZ,999.99- BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-S-BALANCE      PIC ZZ,ZZZ,999.99- BLANK ZERO.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05  R-CHAR   PIC X(6).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMCLRG_DEF.CPY".
       COPY "LIBLP/EMCLRG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMCLRG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMCLRG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           PERFORM CLEAR-BR-TOTALS.
           PERFORM CLEAR-LC-TOTALS.
           PERFORM CLEAR-GD-TOTALS.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRJ-PATH-OVERRIDE.

           PERFORM OPEN-TRJ1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

      *-----------------------------------------------------------------
       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.

           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK2-BRANCH  TO TJ-BRNO.
           MOVE WK2-CLASS   TO TJ-CLASS.
           MOVE WK2-ACCTNO  TO TJ-ACCTNO.
           MOVE WK2-DATE    TO TJ-DATE.
           MOVE WK2-LAWCODE TO TJ-LAWCODE.
           MOVE WK2-LINE    TO TJ-LINE.

           PERFORM READ-TRJ1-FILE.
           IF IO-FG NOT = 0
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE TJ-BRNO TO HOLD-BRNO
              MOVE TJ-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF TJ-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TJ-BRNO TO HOLD-BRNO
              MOVE TJ-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF TJ-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
              MOVE TJ-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           PERFORM CREATE-WK.

           PERFORM TR-DETAIL1.

           GO TO NEXT-TR-REPORT.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM GRAND-TOTALS
              IF ( GTT-ISO NOT = "S" )
                 MOVE 99 TO LN-COUNT
                 PERFORM GROUP-TOTAL-TABLE
                 PERFORM PRINT-RANGE-LINES
              ELSE
                 PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE EMCLRG-QUEUE-POS TO Q-POS.
           MOVE EMCLRG-COPIES-POS TO C-POS.
           MOVE EMCLRG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 1 TO SUB.

           MOVE 0 TO PAGE-NUMBER.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE GL WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "TRANSFER REGISTER IN PROGRESS........." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 1      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           PERFORM GET-GPENV.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO SUMYN.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO DTE.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DTE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO CL-02
           ELSE
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO DA-01.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DTE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

      *-----------------------------------------------------------------
       DA-01.
           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO GTT-ISO-PARAGRAPH.

           IF VDUSTATUS = "1U" GO TO DTE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.

           IF VDUNUM0 = 0
              GO TO DA-01.

           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.
           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO SUMYN.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO DA-01.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

      *----------------------------------------------------------------
       SUMYN.

           IF ( GTT-ISO = "O" )
              MOVE "S  A010SUMONLY." TO VDU
              MOVE "Y" TO SUM-ONLY VDUBUF
              PERFORM  SCREENIO
              GO TO LOCAL-PRINT.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 IF ( DAY-OF-MONTH = 99 )
                    MOVE "SNNN020DA1." TO VDU
                    PERFORM SCREENIO
                    GO TO DTE.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO DTE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( GROUP-FLAG = "Y" )
                 IF ( GTT-ISO = "O" )
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO SUMYN
              ELSE
                 GO TO SUMYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

           IF ( GTT-ISO NOT = "S" )
              IF ( LOCAL-PRINT-FG = "Y" )
                 MOVE MESSAGE-01-LOC-PRT TO MESS
                 PERFORM SEND-MESS
                 GO TO ASK-LOCAL.

      *-----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       SORT-ROUTINE SECTION.
      ******************************************************************

           MOVE BR-NO               TO TJ-BRNO
                                       QTRJ1-WBEG-BRNO
                                       QTRJ1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WEND-DATE.
           MOVE BEG-CLASS           TO TJ-CLASS
                                       QTRJ1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRJ1-WEND-CLASS.
           MOVE 0                   TO TJ-ACCTNO TJ-LAWCODE TJ-LINE
                                       QTRJ1-WBEG-ACCTNO
                                       QTRJ1-WBEG-LAWCODE
                                       QTRJ1-WBEG-LINE.
           MOVE ALL "9"             TO QTRJ1-WEND-ACCTNO
                                       QTRJ1-WEND-LAWCODE
                                       QTRJ1-WEND-LINE.
           PERFORM START-TRJ1-FILE.

      *-----------------------------------------------------------------
       SORT-TR-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO EXIT-SORT-ROUTINE.

      * ONLY REPORT THE NEW CLASS, NOT THE ONE IT CAME FROM
           IF TJ-LNAMT < 0
              GO TO SORT-TR-NEXT.

           IF TJ-DATE > ENDING-DATE
              GO TO EXIT-SORT-ROUTINE.

           IF TJ-CLASS < BEG-CLASS OR TJ-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

           MOVE TJ-DATE    TO WK2-DATE.
           MOVE TJ-BRNO    TO WK2-BRANCH.
           MOVE TJ-CLASS   TO WK2-CLASS.
           MOVE TJ-ACCTNO  TO WK2-ACCTNO.
           MOVE TJ-LAWCODE TO WK2-LAWCODE.
           MOVE TJ-LINE TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       EXIT-SORT-ROUTINE.
           PERFORM CLOSE-WK2-FILE.

      ******************************************************************
       TR-DETAIL1 SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

           MOVE TJ-SSNO(1) TO BW-SSNO.
           PERFORM MOVE-BORROWER.
           MOVE TJ-LOANDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-LOANDATE.
   
           MOVE TJ-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-TRANSDATE.

           MOVE TJ-DLNO TO D-DLNO.
           MOVE TJ-LNAMT TO D-LNAMT.
           MOVE TJ-FINCHG TO D-FINCHG.

           IF GP-INCL-OTH-IN-EARNED-N
              COMPUTE D-UNEARN = TJ-UNEARN(1) + TJ-UNEARN(2) +
                                 TJ-UNEARN(3) + TJ-UNEARN(4) +
                                 TJ-UNEARN(5)
           ELSE
           IF GP-INCL-OTH-IN-EARNED-R
              COMPUTE D-UNEARN = TJ-UNEARN(1) + TJ-UNEARN(2) +
                                 TJ-UNEARN(3) + TJ-UNEARN(5)
           ELSE
              COMPUTE D-UNEARN = TJ-UNEARN(1) + TJ-UNEARN(2) +
                                 TJ-UNEARN(3) + TJ-UNEARN(4) +
                                 TJ-UNEARN(5) + TJ-UNEARN(6).

           MOVE TJ-PURBAL TO D-BALANCE.
           IF TJ-LNAMT < 0
              MOVE TJ-REF-TOLC TO D-FROM
           ELSE
              MOVE TJ-REF-FRLC TO D-FROM.
           IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE TJ-SRCD   TO SAVE-WK-CODE.
           MOVE TJ-BRNO  TO SAVE-WK-BR.
           MOVE TJ-CLASS TO SAVE-WK-CL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              MOVE 0 TO HOLD-AMT(SUB)
           END-PERFORM.

      *-----------------------------------------------------------------
       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF ( IO-FG = 0 )
              GO TO CREATE-WK-UPDATE.

           MOVE SPACES      TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.
           MOVE 0           TO WK-NO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              MOVE 0 TO WK-AMT(SUB)
           END-PERFORM.

       WRITE-NEW-WK-REC.
           PERFORM WRITE-WK-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

      *-----------------------------------------------------------------
       CREATE-WK-UPDATE.
           MOVE "L"      TO WK-ID.
           MOVE TJ-CLASS TO WK-NO.
           PERFORM READ-WK-FILE.
           IF ( IO-FG = 9 )
              MOVE " " TO WK-REC
              GO TO CREATE-LC-WK.

           IF ( TJ-LNAMT > 0 )
              ADD 1 TO WK-AMT(1) HOLD-AMT(1)
           ELSE
              SUBTRACT 1 FROM WK-AMT(1) HOLD-AMT(1).

           ADD TJ-LNAMT  TO WK-AMT(2)  HOLD-AMT(2).
           ADD TJ-PURBAL TO WK-AMT(25) HOLD-AMT(25).
           ADD TJ-FINCHG TO WK-AMT(3)  HOLD-AMT(3).

           COMPUTE WS-UNEARN = TJ-UNEARN(1) + TJ-UNEARN(2) +
                               TJ-UNEARN(3) + TJ-UNEARN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD TJ-UNEARN(6) TO WS-UNEARN.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD TJ-UNEARN(4) TO WS-UNEARN.

           ADD WS-UNEARN TO WK-AMT(4) HOLD-AMT(4).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD TJ-DLRVPRIN(1) TO WK-AMT(20) HOLD-AMT(20).
           ADD TJ-DLRVPRIN(2) TO WK-AMT(21) HOLD-AMT(21).
           ADD TJ-DLDISC      TO WK-AMT(22) HOLD-AMT(22).
           ADD TJ-DLPARVHELD  TO WK-AMT(23) HOLD-AMT(23).
           ADD TJ-PROCEEDS    TO WK-AMT(24) HOLD-AMT(24).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM REWRITE-WK-FILE.

           GO TO NEXT-LC-DETAIL-ACCUM.

      *-----------------------------------------------------------------
       CREATE-LC-WK.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              MOVE 0 TO WK-AMT(SUB)
           END-PERFORM.

           MOVE "L"      TO WK-ID.
           MOVE TJ-CLASS TO WK-NO.

           IF ( TJ-LNAMT > 0 )
              MOVE 1 TO WK-AMT(1) HOLD-AMT(1)
           ELSE
              MOVE -1 TO WK-AMT(1) HOLD-AMT(1).

           MOVE TJ-LNAMT    TO WK-AMT(2)  HOLD-AMT(2).
           MOVE TJ-PURBAL   TO WK-AMT(25) HOLD-AMT(25).
           MOVE TJ-FINCHG   TO WK-AMT(3)  HOLD-AMT(3).

           COMPUTE WS-UNEARN = TJ-UNEARN(1) + TJ-UNEARN(2) +
                               TJ-UNEARN(3) + TJ-UNEARN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD TJ-UNEARN(6) TO WS-UNEARN.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD TJ-UNEARN(4) TO WS-UNEARN.

           ADD WS-UNEARN TO WK-AMT(4) HOLD-AMT(4).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TJ-DLRVPRIN(1) TO WK-AMT(20) HOLD-AMT(20).
           MOVE TJ-DLRVPRIN(2) TO WK-AMT(21) HOLD-AMT(21).
           MOVE TJ-DLDISC      TO WK-AMT(22) HOLD-AMT(22).
           MOVE TJ-DLPARVHELD  TO WK-AMT(23) HOLD-AMT(23).
           MOVE TJ-PROCEEDS    TO WK-AMT(24) HOLD-AMT(24).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-WK-FILE.
           IF ( IO-FG = 9 )
              PERFORM IO-ERROR.


      *-----------------------------------------------------------------
       NEXT-LC-DETAIL-ACCUM.
          
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              ADD HOLD-AMT(SUB) TO TOT-LC-AMT(SUB)
                                   TOT-BR-AMT(SUB)
                                   TOT-GD-AMT(SUB)
           END-PERFORM.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       MOVE-BORROWER SECTION.
      ******************************************************************
           IF BW-SSNO = 0
              GO TO EXIT-MOVE-BORROWER.

           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "* * NOT FOUND * *" TO BW-LNAME
              MOVE " " TO BW-FNAME.

           MOVE SPACES TO D-MAKER.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO D-MAKER.

       EXIT-MOVE-BORROWER.
           EXIT.

      ******************************************************************
       CLEAR-BR-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              MOVE 0 TO TOT-BR-AMT(SUB)
           END-PERFORM.

       END-CLEAR-BR-TOTALS.
           EXIT.

      ******************************************************************
       CLEAR-GD-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              MOVE 0 TO TOT-GD-AMT(SUB)
           END-PERFORM.

       END-CLEAR-GD-TOTALS.
           EXIT.

      ******************************************************************
       CLEAR-LC-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 26
              MOVE 0 TO TOT-LC-AMT(SUB)
           END-PERFORM.

       END-CLEAR-LC-TOTALS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              IF TOTAL-HEAD-FG = "N"
                  PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-CLASS TO H-LCNO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE "Y" TO TOTAL-HEAD-FG.
           MOVE "Y" TO SUMMARY-FG.
           IF GTT-ISO NOT = "O"
              PERFORM TOTALS-HEADER-PAGE.

           MOVE "CLASS TOTALS:" TO S-TOTNAME.
           MOVE TOT-LC-AMT(1) TO D-S-NO-LOANS.
           MOVE TOT-LC-AMT(2) TO D-S-LNAMT.
           MOVE TOT-LC-AMT(3) TO D-S-FINCHG.
           MOVE TOT-LC-AMT(4) TO D-S-UNEARN.
           MOVE TOT-LC-AMT(25) TO D-S-BALANCE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-LC-TOTALS.
           MOVE "N" TO TOTAL-HEAD-FG.
           MOVE "N" TO SUMMARY-FG.

       END-CLASS-TOTALS.
           EXIT.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE "Y" TO TOTAL-HEAD-FG.
           MOVE "BRANCH TOTALS:" TO S-TOTNAME.

           MOVE TOT-BR-AMT(1) TO D-S-NO-LOANS.
           MOVE TOT-BR-AMT(2) TO D-S-LNAMT.
           MOVE TOT-BR-AMT(3) TO D-S-FINCHG.
           MOVE TOT-BR-AMT(4) TO D-S-UNEARN.
           MOVE TOT-BR-AMT(25) TO D-S-BALANCE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-BR-TOTALS.
           MOVE "N" TO SUMMARY-FG.
           MOVE "N" TO TOTAL-HEAD-FG.

       END-BRANCH-TOTALS.
           EXIT.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE "Y" TO TOTAL-HEAD-FG.
           MOVE "GRAND  TOTALS:" TO S-TOTNAME.

           MOVE TOT-GD-AMT(1) TO D-S-NO-LOANS.
           MOVE TOT-GD-AMT(2) TO D-S-LNAMT.
           MOVE TOT-GD-AMT(3) TO D-S-FINCHG.
           MOVE TOT-GD-AMT(4) TO D-S-UNEARN.
           MOVE TOT-GD-AMT(25) TO D-S-BALANCE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           PERFORM CLEAR-GD-TOTALS.
           MOVE "N" TO SUMMARY-FG.
           MOVE "N" TO TOTAL-HEAD-FG.

       END-GRAND-TOTALS.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD8 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 5 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
              MOVE HEAD4 TO PR-REC
              PERFORM WRITE-PR-REC
              IF SUMMARY-FG = "N"
                 MOVE HEAD5 TO PR-REC
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 7 TO LN-COUNT
                 MOVE 2 TO LN-FEED
              ELSE
                 MOVE 4 TO LN-COUNT
                 MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       TOTALS-HEADER-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD8 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      ***************************************************************
       CLASS-SCAN SECTION.
      ***************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.
       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************

           ADD WK-AMT(1)      TO  WS-GTT-NOLOANS   ( GTT-TOTAL-SUB ).
           ADD WK-AMT(2)      TO  WS-GTT-LNAMT     ( GTT-TOTAL-SUB ).
           ADD WK-AMT(3)      TO  WS-GTT-FINCHG    ( GTT-TOTAL-SUB ).
           ADD WK-AMT(4)      TO  WS-GTT-UNEARN    ( GTT-TOTAL-SUB ).
           ADD WK-AMT(25)     TO  WS-GTT-TRBAL     ( GTT-TOTAL-SUB ).

       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES      TO WK-KEY.
           MOVE SPACES      TO WK-CODE.
           MOVE WGR-SRESULT TO WK-BR.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       GTT-NEXT-WK1.

           PERFORM READ-WK-FILE-NEXT.

           IF ( IO-FG   NOT = 0           ) OR
              ( WK-BR NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

           PERFORM GTT-CHECK-ADD-TOTALS.

           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ******************************************************************

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO D2-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO D2-GRNO.
           MOVE GR-LEVEL  TO D2-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************

           MOVE WS-GTT-NOLOANS   (GTT-TOTAL-SUB) TO D-S-NO-LOANS.
           MOVE WS-GTT-LNAMT     (GTT-TOTAL-SUB) TO D-S-LNAMT.
           MOVE WS-GTT-FINCHG    (GTT-TOTAL-SUB) TO D-S-FINCHG.
           MOVE WS-GTT-UNEARN    (GTT-TOTAL-SUB) TO D-S-UNEARN.
           MOVE WS-GTT-TRBAL     (GTT-TOTAL-SUB) TO D-S-BALANCE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - PRINT DETAIL AFTER LOAD - - -
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - CLEAR LEVEL IN TABLE  - - -
           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       GET-DISPLAY-LNNO SECTION.
      ******************************************************************

      * LPS-DISPLAY-LNNO IS SET FIRST SO THAT IF THERE IS A ERROR
      * IT WILL BE ALREADY DISPLAYING FOR A ERROR, OTHERWISE IT WILL
      * RESET LPS-DISPLAY-LNNO IN 'PERFORM LPSTAT-DISPLAY.'

           MOVE TJ-CLASS  TO LPS-CLASS.
           MOVE "-"       TO LPS-DASH.
           MOVE TJ-ACCTNO TO LPS-NUMBER.
           MOVE "{N/A}"   TO LPS-7STATUS.

           MOVE TJ-BRNO  TO LN-OWNBR.
           MOVE TJ-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           PERFORM LPSTAT-DISPLAY.

       GET-DISPLAY-LNNO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMCLRG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMCLRG-SYSDATE.
           DISPLAY EMCLRG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMCLRG-FORM-FIELDS TO WR-BUF.
           MOVE EMCLRG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMCLRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMCLRG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMCLRG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRJ1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              IF EXT-ROUTE-BUF = "00" OR "70"
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY EMCLRG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
