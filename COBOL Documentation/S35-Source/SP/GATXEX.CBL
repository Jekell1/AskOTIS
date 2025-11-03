       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GATXEX.
       DATE-WRITTEN. 6/26/09.
      *******************************************************************
      *                      (SP)
      *    DESCRIPTION:  GEORGIA TAX GILA EXTRACTION
      *                  LOANS MADE DURING QUARTER
      *                    (TAKEN FROM GATXEX)
      *
      *    TAB DELIMITED WITH HEADERS
      *
      * WORLD STILL USES 4/27/2023
      *
      * REV:
      *  062609 BLM NEW FOR WORLD GA TAX REPORT EXTRACTION, WORLD PR# 642
      *  101220 BAH ADDED GATX-NO-LOANS, TOTAL LOANS FROM EMLNRG, WORLD PR# 729
      *  BAH 160203 REMOVED LTFILE, PD0003
      *  BLM 160315 FIX FOR NEW BY-DATE FORMAT, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * 20170922 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * 20180306 BLM GET NON-FILE FROM BY-FEE TABLE NOT BY-INS-TABLE
      * BAH 181214 GLOBAL LNFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 20190625 REMOVED LCSCAN-REVOLVING PT0153
      * BAH 20191003 REMOVED ACCESS-CALL LNFILE, TRP, TRL
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 2025.0115 MAKE TAB DELIMITED, ADD HEADERS AND SEVERAL NEW COLUMNS
      *               #1697 S35Q-157
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT GATX-FILE ASSIGN OUTPUT GATX-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *  SUMMARY TOTALS WORK RECORD
      *
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO          PIC 9(4).
           03  WK-LNAMT             PIC S9(9)V99  COMP-3.
           03  WK-INTCHG            PIC S9(9)V99  COMP-3.
           03  WK-PL-RECV-AMT       PIC S9(9)V99  COMP-3.
           03  WK-PL-RECV-INT       PIC S9(9)V99  COMP-3.
           03  WK-PL-TRANS-AMT      PIC S9(9)V99  COMP-3.
           03  WK-PL-UNEARNED-INT   PIC S9(9)V99  COMP-3.
           03  WK-REB-INT-APP-PRIN  PIC S9(9)V99  COMP-3.
           03  WK-REB-TOT-APP-INT   PIC S9(9)V99  COMP-3.
           03  WK-REB-TOT-APP-PRIN  PIC S9(9)V99  COMP-3.
           03  WK-SMALL-LOAN-AR     PIC S9(9)V99  COMP-3.
           03  WK-NO-LIFE-PREMS     PIC S9(9)     COMP-3.
           03  WK-LIFE-PREMS        PIC S9(9)V99  COMP-3.
           03  WK-LIFE-REFS         PIC S9(9)V99  COMP-3.
           03  WK-NO-AH-PREMS       PIC S9(9)     COMP-3.
           03  WK-AH-PREMS          PIC S9(9)V99  COMP-3.
           03  WK-AH-REFS           PIC S9(9)V99  COMP-3.
           03  WK-NO-PROP-PREMS     PIC S9(9)     COMP-3.
           03  WK-PROP-PREMS        PIC S9(9)V99  COMP-3.
           03  WK-PROP-REFS         PIC S9(9)V99  COMP-3.
           03  WK-NO-AUTO-PREMS     PIC S9(9)     COMP-3.
           03  WK-AUTO-PREMS        PIC S9(9)V99  COMP-3.
           03  WK-AUTO-REFS         PIC S9(9)V99  COMP-3.
           03  WK-NO-NONFILE-PREMS  PIC S9(9)     COMP-3.
           03  WK-NONFILE-PREMS     PIC S9(9)V99  COMP-3.
           03  WK-NONFILE-REFS      PIC S9(9)V99  COMP-3.
           03  WK-LIFE-CLAIMS       PIC S9(9)V99  COMP-3.
           03  WK-AH-CLAIMS         PIC S9(9)V99  COMP-3.
           03  WK-NO-LIFE-CLAIMS    PIC S9(9)     COMP-3.
           03  WK-NO-AH-CLAIMS      PIC S9(9)     COMP-3.
           03  WK-PROP-CLAIMS       PIC S9(9)V99  COMP-3.
           03  WK-AUTO-CLAIMS       PIC S9(9)V99  COMP-3.
           03  WK-NO-PROP-CLAIMS    PIC S9(9)     COMP-3.
           03  WK-NO-AUTO-CLAIMS    PIC S9(9)     COMP-3.
           03  WK-NONFILE-CLAIMS    PIC S9(9)V99  COMP-3.
           03  WK-NO-NONFILE-CLAIMS PIC S9(9)     COMP-3.
           03  WK-NO-LOANS          PIC S9(9)     COMP-3.
           03  WK-NO-GA-50-LOANS    PIC S9(9)     COMP-3.
           03  WK-AMT-GA-50-LOANS   PIC S9(9)V99  COMP-3.
           03  WK-NO-REB-4-8        PIC S9(9)     COMP-3.
           03  WK-AMT-REB-4-8       PIC S9(9)V99  COMP-3.
           03  WK-NO-CLOSFEE-REB    PIC S9(9)     COMP-3.
           03  WK-AMT-CLOSFEE-REB   PIC S9(9)V99  COMP-3.
           03  WK-NO-REB-INT        PIC S9(9)     COMP-3.
           03  WK-NO-REB-MF         PIC S9(9)     COMP-3.
           03  WK-AMT-REB-MF        PIC S9(9)V99  COMP-3.
           03  WK-NO-APLC           PIC S9(9)     COMP-3.
           03  WK-AMT-APLC          PIC S9(9)V99  COMP-3.

      ******************************************************************
      * OUTPUT FILE FOR GEORGIA LOAN TAX FORM
      ******************************************************************
       FD  GATX-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 800 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  GATX-REC.
           03  GATX-BYTE    PIC X OCCURS 800 TIMES.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/GATXEX S35 01/24/25-07:46:56 >".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".

       01  RECORD-COUNT         PIC 9(8)  VALUE 0.
       01  CR-1                 PIC X VALUE X"0D".
       01  TAB-1                PIC X VALUE X"09".

       01  GATX-PATH.
           03  GATX-PATH-BASE          PIC X(09).
           03  FILLER                  PIC X(01)  VALUE "/".
           03  GATX-PATH-MACHINE       PIC X(02).
           03  FILLER                  PIC X(9)  VALUE "/ED/GATX.".
           03  GATX-PATH-MM            PIC 99.
           03  GATX-PATH-YY            PIC 99.

       01  GATX-RECORD.
           03  GATX-BRNO              PIC 9(4).
           03  GATX-LNAMT             PIC 9(9).99-.
           03  GATX-INTCHG            PIC 9(9).99-.
           03  GATX-PL-RECV-AMT       PIC 9(9).99-.
           03  GATX-PL-RECV-INT       PIC 9(9).99-.
           03  GATX-PL-TRANS-AMT      PIC 9(9).99-.
           03  GATX-PL-UNEARNED-INT   PIC 9(9).99-.
           03  GATX-REB-INT-APP-PRIN  PIC 9(9).99-.
           03  GATX-REB-TOT-APP-INT   PIC 9(9).99-.
           03  GATX-REB-TOT-APP-PRIN  PIC 9(9).99-.
           03  GATX-SMALL-LOAN-AR     PIC 9(9).99-.
           03  GATX-NO-LIFE-PREMS     PIC 9(9)-.
           03  GATX-LIFE-PREMS        PIC 9(9).99-.
           03  GATX-LIFE-REFS         PIC 9(9).99-.
           03  GATX-NO-AH-PREMS       PIC 9(9)-.
           03  GATX-AH-PREMS          PIC 9(9).99-.
           03  GATX-AH-REFS           PIC 9(9).99-.
           03  GATX-NO-PROP-PREMS     PIC 9(9)-.
           03  GATX-PROP-PREMS        PIC 9(9).99-.
           03  GATX-PROP-REFS         PIC 9(9).99-.
           03  GATX-NO-AUTO-PREMS     PIC 9(9)-.
           03  GATX-AUTO-PREMS        PIC 9(9).99-.
           03  GATX-AUTO-REFS         PIC 9(9).99-.
           03  GATX-NO-NONFILE-PREMS  PIC 9(9)-.
           03  GATX-NONFILE-PREMS     PIC 9(9).99-.
           03  GATX-NONFILE-REFS      PIC 9(9).99-.
           03  GATX-LIFE-CLAIMS       PIC 9(9).99-.
           03  GATX-AH-CLAIMS         PIC 9(9).99-.
           03  GATX-NO-LIFE-CLAIMS    PIC 9(9)-.
           03  GATX-NO-AH-CLAIMS      PIC 9(9)-.
           03  GATX-PROP-CLAIMS       PIC 9(9).99-.
           03  GATX-AUTO-CLAIMS       PIC 9(9).99-.
           03  GATX-NO-PROP-CLAIMS    PIC 9(9)-.
           03  GATX-NO-AUTO-CLAIMS    PIC 9(9)-.
           03  GATX-NONFILE-CLAIMS    PIC 9(9).99-.
           03  GATX-NO-NONFILE-CLAIMS PIC 9(9)-.
           03  GATX-NO-LOANS          PIC 9(9)-.
           03  GATX-NO-GA-50-LOANS    PIC 9(9)-.
           03  GATX-AMT-GA-50-LOANS   PIC 9(9).99-.
           03  GATX-NO-REB-4-8        PIC 9(9)-.
           03  GATX-AMT-REB-4-8       PIC 9(9).99-.
           03  GATX-NO-CLOSFEE-REB    PIC 9(9)-.
           03  GATX-AMT-CLOSFEE-REB   PIC 9(9).99-.
           03  GATX-NO-REB-INT        PIC 9(9)-.
           03  GATX-NO-REB-MF         PIC 9(9)-.
           03  GATX-AMT-REB-MF        PIC 9(9).99-.
           03  GATX-NO-APLC           PIC 9(9)-.
           03  GATX-AMT-APLC          PIC 9(9).99-.

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
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP-3.
       01  END-PERIOD           PIC 9(2)  VALUE 0.

       01  TEST-WORKAMT         PIC S9(7)V99.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".
       01  TYPE-FLAG            PIC X        VALUE " ".
       01  REMAINDER-WK         PIC S9(9)V99 VALUE 0.
       01  RESULT-WK            PIC S9(9)    VALUE 0.

      ******************************************************************
      *  GRAND TOTALS
      ******************************************************************
       01  GRAND-TOTALS.
           03  TOT-LNAMT             PIC S9(9)V99.
           03  TOT-INTCHG            PIC S9(9)V99.
           03  TOT-PL-RECV-AMT       PIC S9(9)V99.
           03  TOT-PL-RECV-INT       PIC S9(9)V99.
           03  TOT-PL-TRANS-AMT      PIC S9(9)V99.
           03  TOT-PL-UNEARNED-INT   PIC S9(9)V99.
           03  TOT-REB-INT-APP-PRIN  PIC S9(9)V99.
           03  TOT-REB-TOT-APP-INT   PIC S9(9)V99.
           03  TOT-REB-TOT-APP-PRIN  PIC S9(9)V99.
           03  TOT-SMALL-LOAN-AR     PIC S9(9)V99.
           03  TOT-NO-LIFE-PREMS     PIC S9(9).
           03  TOT-LIFE-PREMS        PIC S9(9)V99.
           03  TOT-LIFE-REFS         PIC S9(9)V99.
           03  TOT-NO-AH-PREMS       PIC S9(9).
           03  TOT-AH-PREMS          PIC S9(9)V99.
           03  TOT-AH-REFS           PIC S9(9)V99.
           03  TOT-NO-PROP-PREMS     PIC S9(9).
           03  TOT-PROP-PREMS        PIC S9(9)V99.
           03  TOT-PROP-REFS         PIC S9(9)V99.
           03  TOT-NO-AUTO-PREMS     PIC S9(9).
           03  TOT-AUTO-PREMS        PIC S9(9)V99.
           03  TOT-AUTO-REFS         PIC S9(9)V99.
           03  TOT-NO-NONFILE-PREMS  PIC S9(9).
           03  TOT-NONFILE-PREMS     PIC S9(9)V99.
           03  TOT-NONFILE-REFS      PIC S9(9)V99.
           03  TOT-LIFE-CLAIMS       PIC S9(9)V99.
           03  TOT-AH-CLAIMS         PIC S9(9)V99.
           03  TOT-NO-LIFE-CLAIMS    PIC S9(9).
           03  TOT-NO-AH-CLAIMS      PIC S9(9).
           03  TOT-PROP-CLAIMS       PIC S9(9)V99.
           03  TOT-AUTO-CLAIMS       PIC S9(9)V99.
           03  TOT-NO-PROP-CLAIMS    PIC S9(9).
           03  TOT-NO-AUTO-CLAIMS    PIC S9(9).
           03  TOT-NONFILE-CLAIMS    PIC S9(9)V99.
           03  TOT-NO-NONFILE-CLAIMS PIC S9(9).
           03  TOT-NO-LOANS          PIC S9(9).
           03  TOT-NO-GA-50-LOANS    PIC S9(9).
           03  TOT-AMT-GA-50-LOANS   PIC S9(9)V99.
           03  TOT-NO-REB-4-8        PIC S9(9).
           03  TOT-AMT-REB-4-8       PIC S9(9)V99.
           03  TOT-NO-CLOSFEE-REB    PIC S9(9).
           03  TOT-AMT-CLOSFEE-REB   PIC S9(9)V99.
           03  TOT-NO-REB-INT        PIC S9(9).
           03  TOT-NO-REB-MF         PIC S9(9).
           03  TOT-AMT-REB-MF        PIC S9(9)V99.
           03  TOT-NO-APLC           PIC S9(9).
           03  TOT-AMT-APLC          PIC S9(9)V99.


      *-----------------------------------------------------------------
       COPY "LIBLP/EMSFRG_CMD.CPY".
      *-----------------------------------------------------------------

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
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-DATE.
               05  BEG-MONTH   PIC 99.
               05  BEG-YEAR    PIC 99.
           03  BDATE REDEFINES BEG-DATE PIC 9999.
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  REPORT-ONLY-FG  PIC X.
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE PIC X(1)  OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "E".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       01  BEGINNING-DATE      PIC 9(8).
       01  ENDING-DATE         PIC 9(8).
       01  TEST-DATE.
           03  TEST-MONTH   PIC 99.
           03  TEST-YEAR    PIC 99.

       01  HOLD-BY-DATE-CCYYMM     PIC 9(6).

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
           03  FILLER           PIC X(32)    VALUE " ".
           03  FILLER           PIC X(27) VALUE
               "GEORGIA LOAN TAX EXTRACTION".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(50) VALUE " ".
           03  H-DATE1          PIC 99/99/99.
           03  FILLER           PIC X(6) VALUE " THRU ".
           03  H-DATE2          PIC 99/99/99.

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BR-NAME.
               05  D-BR-PROMPT      PIC X(4).
               05  D-BRNO           PIC 9(4).
               05  FILLER           PIC X(9).
           03  FILLER           PIC X(4).
           03  D-PROMPT1        PIC X(19).
           03  D-AMT1           PIC ZZZZ,ZZZ,ZZZ.99-.
           03  D-NO1            REDEFINES D-AMT1
                                PIC ZZZZZZZ,ZZZ,ZZ9-.
           03  FILLER           PIC X(3).
           03  D-PROMPT2        PIC X(19).
           03  D-AMT2           PIC ZZZZ,ZZZ,ZZZ.99-.
           03  D-NO2            REDEFINES D-AMT2
                                PIC ZZZZZZZ,ZZZ,ZZ9-.
           03  FILLER           PIC X(3).
           03  D-PROMPT3        PIC X(19).
           03  D-AMT3           PIC ZZZZ,ZZZ,ZZZ.99-.
           03  D-NO3            REDEFINES D-AMT3
                                PIC ZZZZZZZ,ZZZ,ZZ9-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05  R-CHAR   PIC X(6).

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GRWORKW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/GATXEX_DEF.CPY".
       COPY "LIBSP/GATXEX_WKS.CPY".
      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY "LIBSP/GATXEX_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE GATX-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE GATX-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GATXEX" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

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
              GO TO PROCESS-WORK-FILE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-WORK-FILE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO TRL-PATH-OVERRIDE
                              TRP-PATH-OVERRIDE.


           MOVE BEG-DATE TO TEST-DATE.

      * PAYMENTS
           PERFORM PROCESS-TRP-TRANSACTIONS.
      * LOANS
           PERFORM PROCESS-TRL-TRANSACTIONS.

      *----------------------------------------------------------------
      * GET INSURANCE STATISTICS FROM BRANCH STAT FILE
      *----------------------------------------------------------------
       GET-INSURANCE-FROM-BY.
           MOVE TEST-MONTH TO DATE-MMDDYY-MM.
           MOVE TEST-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.

           MOVE DATE-YYYYMM    TO BY-DATE-CCYYMM HOLD-BY-DATE-CCYYMM
                                  QBY1-WBEG-DATE-CCYYMM
                                  QBY1-WEND-DATE-CCYYMM.

           MOVE BR-NO          TO BY-BRNO
                                  QBY1-WBEG-BRNO
                                  QBY1-WEND-BRNO.
           MOVE BEG-CLASS      TO BY-CLASS
                                  QBY1-WBEG-CLASS.
           MOVE END-CLASS      TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE.
       NEXT-BY.
           PERFORM READ-BY1-FILE-NEXT.
           IF IO-BAD
              GO TO GET-CLAIMS-FROM-WR.

           IF BY-CLASS > END-CLASS
              GO TO GET-CLAIMS-FROM-WR.

           IF BY-BRNO NOT = BR-NO
              GO TO GET-CLAIMS-FROM-WR.

           IF BY-DATE-CCYYMM NOT = HOLD-BY-DATE-CCYYMM
              GO TO GET-CLAIMS-FROM-WR.

           MOVE "I" TO TYPE-FLAG.
           PERFORM CREATE-WORK-RECORD.

           GO TO NEXT-BY.

      *----------------------------------------------------------------
      * GET CLAIMS FROM RATING SHEET FILE
      *----------------------------------------------------------------
       GET-CLAIMS-FROM-WR.
           MOVE TEST-MONTH TO WR-MM.
           MOVE TEST-YEAR  TO WR-YYYY.
           IF TEST-YEAR < EXT-JULIAN-CC
              ADD 2000 TO WR-YYYY
           ELSE
              ADD 1900 TO WR-YYYY.
           MOVE BR-NO      TO WR-BRNO.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              GO TO CHECK-NEXT-MONTH.

           MOVE "C" TO TYPE-FLAG.
           PERFORM CREATE-WORK-RECORD.

       CHECK-NEXT-MONTH.
           IF TEST-DATE NOT = END-DATE
              MOVE TEST-YEAR  TO DATE-YYMMDD-YY
              MOVE TEST-MONTH TO DATE-YYMMDD-MM
              MOVE 01         TO DATE-YYMMDD-DD
              PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO NDTE-DATE
              MOVE 1             TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-MM TO TEST-MONTH
              MOVE NDTE-YY TO TEST-YEAR
              GO TO GET-INSURANCE-FROM-BY.

      *----------------------------------------------------------------
      * AT THIS POINT, ALL TRFILES HAVE BEEN READ
      *----------------------------------------------------------------
       GET-RECEIVABLES-FROM-GL.
           MOVE BR-NO TO GL-BRANCH.
           MOVE 1110000 TO GL-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           MOVE "G" TO TYPE-FLAG.
           PERFORM CREATE-WORK-RECORD.

      *-----------------------------------------------------------------
       END-OF-GROUP.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       PROCESS-WORK-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      * WRITE HEADER RECORD
           IF REPORT-ONLY-FG = "N"
              PERFORM WRITE-HEADER-RECORD.

       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE "Y" TO RECORDS-FOUND.

           MOVE WK-BRNO               TO GATX-BRNO.
           MOVE WK-LNAMT              TO GATX-LNAMT.
           MOVE WK-INTCHG             TO GATX-INTCHG.
           MOVE WK-PL-RECV-AMT        TO GATX-PL-RECV-AMT.
           MOVE WK-PL-RECV-INT        TO GATX-PL-RECV-INT.
           MOVE WK-PL-TRANS-AMT       TO GATX-PL-TRANS-AMT.
           MOVE WK-PL-UNEARNED-INT    TO GATX-PL-UNEARNED-INT.
           MOVE WK-REB-INT-APP-PRIN   TO GATX-REB-INT-APP-PRIN.
           MOVE WK-REB-TOT-APP-INT    TO GATX-REB-TOT-APP-INT.
           MOVE WK-REB-TOT-APP-PRIN   TO GATX-REB-TOT-APP-PRIN.
           MOVE WK-SMALL-LOAN-AR      TO GATX-SMALL-LOAN-AR.
           MOVE WK-NO-LIFE-PREMS      TO GATX-NO-LIFE-PREMS.
           MOVE WK-LIFE-PREMS         TO GATX-LIFE-PREMS.
           MOVE WK-LIFE-REFS          TO GATX-LIFE-REFS.
           MOVE WK-NO-AH-PREMS        TO GATX-NO-AH-PREMS.
           MOVE WK-AH-PREMS           TO GATX-AH-PREMS.
           MOVE WK-AH-REFS            TO GATX-AH-REFS.
           MOVE WK-NO-PROP-PREMS      TO GATX-NO-PROP-PREMS.
           MOVE WK-PROP-PREMS         TO GATX-PROP-PREMS.
           MOVE WK-PROP-REFS          TO GATX-PROP-REFS.
           MOVE WK-NO-AUTO-PREMS      TO GATX-NO-AUTO-PREMS.
           MOVE WK-AUTO-PREMS         TO GATX-AUTO-PREMS.
           MOVE WK-AUTO-REFS          TO GATX-AUTO-REFS.
           MOVE WK-NO-NONFILE-PREMS   TO GATX-NO-NONFILE-PREMS.
           MOVE WK-NONFILE-PREMS      TO GATX-NONFILE-PREMS.
           MOVE WK-NONFILE-REFS       TO GATX-NONFILE-REFS.
           MOVE WK-LIFE-CLAIMS        TO GATX-LIFE-CLAIMS.
           MOVE WK-AH-CLAIMS          TO GATX-AH-CLAIMS.
           MOVE WK-NO-LIFE-CLAIMS     TO GATX-NO-LIFE-CLAIMS.
           MOVE WK-NO-AH-CLAIMS       TO GATX-NO-AH-CLAIMS.
           MOVE WK-PROP-CLAIMS        TO GATX-PROP-CLAIMS.
           MOVE WK-AUTO-CLAIMS        TO GATX-AUTO-CLAIMS.
           MOVE WK-NO-PROP-CLAIMS     TO GATX-NO-PROP-CLAIMS.
           MOVE WK-NO-AUTO-CLAIMS     TO GATX-NO-AUTO-CLAIMS.
           MOVE WK-NONFILE-CLAIMS     TO GATX-NONFILE-CLAIMS.
           MOVE WK-NO-NONFILE-CLAIMS  TO GATX-NO-NONFILE-CLAIMS.
           MOVE WK-NO-LOANS           TO GATX-NO-LOANS.
           MOVE WK-NO-GA-50-LOANS     TO GATX-NO-GA-50-LOANS.
           MOVE WK-AMT-GA-50-LOANS    TO GATX-AMT-GA-50-LOANS.
           MOVE WK-NO-REB-4-8         TO GATX-NO-REB-4-8.
           MOVE WK-AMT-REB-4-8        TO GATX-AMT-REB-4-8.
           MOVE WK-NO-CLOSFEE-REB     TO GATX-NO-CLOSFEE-REB.
           MOVE WK-AMT-CLOSFEE-REB    TO GATX-AMT-CLOSFEE-REB.
           MOVE WK-NO-REB-INT         TO GATX-NO-REB-INT.
           MOVE WK-NO-REB-MF          TO GATX-NO-REB-MF.
           MOVE WK-AMT-REB-MF         TO GATX-AMT-REB-MF.
           MOVE WK-NO-APLC            TO GATX-NO-APLC.
           MOVE WK-AMT-APLC           TO GATX-AMT-APLC.

           IF REPORT-ONLY-FG = "N"
              PERFORM WRITE-DETAIL-RECORD.

           ADD 1 TO RECORD-COUNT.

       ADD-TO-GRAND-TOTALS.
           ADD WK-LNAMT              TO TOT-LNAMT.
           ADD WK-INTCHG             TO TOT-INTCHG.
           ADD WK-PL-RECV-AMT        TO TOT-PL-RECV-AMT.
           ADD WK-PL-RECV-INT        TO TOT-PL-RECV-INT.
           ADD WK-PL-TRANS-AMT       TO TOT-PL-TRANS-AMT.
           ADD WK-PL-UNEARNED-INT    TO TOT-PL-UNEARNED-INT.
           ADD WK-REB-INT-APP-PRIN   TO TOT-REB-INT-APP-PRIN.
           ADD WK-REB-TOT-APP-INT    TO TOT-REB-TOT-APP-INT.
           ADD WK-REB-TOT-APP-PRIN   TO TOT-REB-TOT-APP-PRIN.
           ADD WK-SMALL-LOAN-AR      TO TOT-SMALL-LOAN-AR.
           ADD WK-NO-LIFE-PREMS      TO TOT-NO-LIFE-PREMS.
           ADD WK-LIFE-PREMS         TO TOT-LIFE-PREMS.
           ADD WK-LIFE-REFS          TO TOT-LIFE-REFS.
           ADD WK-NO-AH-PREMS        TO TOT-NO-AH-PREMS.
           ADD WK-AH-PREMS           TO TOT-AH-PREMS.
           ADD WK-AH-REFS            TO TOT-AH-REFS.
           ADD WK-NO-PROP-PREMS      TO TOT-NO-PROP-PREMS.
           ADD WK-PROP-PREMS         TO TOT-PROP-PREMS.
           ADD WK-PROP-REFS          TO TOT-PROP-REFS.
           ADD WK-NO-AUTO-PREMS      TO TOT-NO-AUTO-PREMS.
           ADD WK-AUTO-PREMS         TO TOT-AUTO-PREMS.
           ADD WK-AUTO-REFS          TO TOT-AUTO-REFS.
           ADD WK-NO-NONFILE-PREMS   TO TOT-NO-NONFILE-PREMS.
           ADD WK-NONFILE-PREMS      TO TOT-NONFILE-PREMS.
           ADD WK-NONFILE-REFS       TO TOT-NONFILE-REFS.
           ADD WK-LIFE-CLAIMS        TO TOT-LIFE-CLAIMS.
           ADD WK-AH-CLAIMS          TO TOT-AH-CLAIMS.
           ADD WK-NO-LIFE-CLAIMS     TO TOT-NO-LIFE-CLAIMS.
           ADD WK-NO-AH-CLAIMS       TO TOT-NO-AH-CLAIMS.
           ADD WK-PROP-CLAIMS        TO TOT-PROP-CLAIMS.
           ADD WK-AUTO-CLAIMS        TO TOT-AUTO-CLAIMS.
           ADD WK-NO-PROP-CLAIMS     TO TOT-NO-PROP-CLAIMS.
           ADD WK-NO-AUTO-CLAIMS     TO TOT-NO-AUTO-CLAIMS.
           ADD WK-NONFILE-CLAIMS     TO TOT-NONFILE-CLAIMS.
           ADD WK-NO-NONFILE-CLAIMS  TO TOT-NO-NONFILE-CLAIMS.
           ADD WK-NO-LOANS           TO TOT-NO-LOANS.
           ADD WK-NO-GA-50-LOANS     TO TOT-NO-GA-50-LOANS
           ADD WK-AMT-GA-50-LOANS    TO TOT-AMT-GA-50-LOANS
           ADD WK-NO-REB-4-8         TO TOT-NO-REB-4-8
           ADD WK-AMT-REB-4-8        TO TOT-AMT-REB-4-8
           ADD WK-NO-CLOSFEE-REB     TO TOT-NO-CLOSFEE-REB
           ADD WK-AMT-CLOSFEE-REB    TO TOT-AMT-CLOSFEE-REB
           ADD WK-NO-REB-INT         TO TOT-NO-REB-INT
           ADD WK-NO-REB-MF          TO TOT-NO-REB-MF
           ADD WK-AMT-REB-MF         TO TOT-AMT-REB-MF
           ADD WK-NO-APLC            TO TOT-NO-APLC
           ADD WK-AMT-APLC           TO TOT-AMT-APLC.

       PREPARE-SUMMARY-REPORT.
           MOVE "BR# "                   TO D-BR-PROMPT.
           MOVE WK-BRNO                  TO D-BRNO.
           MOVE "TOTAL LOAN AMOUNT :"    TO D-PROMPT1.
           MOVE GATX-LNAMT               TO D-AMT1.
           MOVE "AMT LIFE REFUNDS  :"    TO D-PROMPT2.
           MOVE GATX-LIFE-REFS           TO D-AMT2.
           MOVE "AMT NONFILE REFS  :"    TO D-PROMPT3.
           MOVE GATX-NONFILE-REFS        TO D-AMT3.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE BR-NAME                  TO D-BR-NAME.
           MOVE "INTEREST CHARGE   :"    TO D-PROMPT1.
           MOVE GATX-INTCHG              TO D-AMT1.
           MOVE "# A&H POLICIES    :"    TO D-PROMPT2.
           MOVE GATX-NO-AH-PREMS         TO D-NO2
           MOVE "AMT LIFE CLAIMS   :"    TO D-PROMPT3.
           MOVE GATX-LIFE-CLAIMS         TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L RECOVERIES AMT:"    TO D-PROMPT1.
           MOVE GATX-PL-RECV-AMT         TO D-AMT1.
           MOVE "AMT A&H PREMIUMS  :"    TO D-PROMPT2.
           MOVE GATX-AH-PREMS            TO D-AMT2.
           MOVE "AMT A&H CLAIMS    :"    TO D-PROMPT3.
           MOVE GATX-AH-CLAIMS           TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L RECOVERIES INT:"    TO D-PROMPT1.
           MOVE GATX-PL-RECV-INT         TO D-AMT1.
           MOVE "AMT A&H REFUNDS   :"    TO D-PROMPT2.
           MOVE GATX-AH-REFS             TO D-AMT2.
           MOVE "# LIFE CLAIMS     :"    TO D-PROMPT3.
           MOVE GATX-NO-LIFE-CLAIMS      TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L TRANS AMT     :"    TO D-PROMPT1.
           MOVE GATX-PL-TRANS-AMT        TO D-AMT1.
           MOVE "# PROP POLICIES   :"    TO D-PROMPT2.
           MOVE GATX-NO-PROP-PREMS       TO D-NO2.
           MOVE "# A&H CLAIMS      :"    TO D-PROMPT3.
           MOVE GATX-NO-AH-CLAIMS        TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L UNEARNED INT  :"    TO D-PROMPT1.
           MOVE GATX-PL-UNEARNED-INT     TO D-AMT1.
           MOVE "AMT PROP PREMIUMS :"    TO D-PROMPT2.
           MOVE GATX-PROP-PREMS          TO D-AMT2.
           MOVE "AMT PROP CLAIMS   :"    TO D-PROMPT3.
           MOVE GATX-PROP-CLAIMS         TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APP-PRIN (INT)    :"    TO D-PROMPT1.
           MOVE GATX-REB-INT-APP-PRIN    TO D-AMT1.
           MOVE "AMT PROP REFUNDS  :"    TO D-PROMPT2.
           MOVE GATX-PROP-REFS           TO D-AMT2.
           MOVE "AMT AUTO CLAIMS   :"    TO D-PROMPT3.
           MOVE GATX-AUTO-CLAIMS         TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APP-INT (TOTAL)   :"    TO D-PROMPT1.
           MOVE GATX-REB-TOT-APP-INT     TO D-AMT1.
           MOVE "# AUTO POLICIES   :"    TO D-PROMPT2.
           MOVE GATX-NO-AUTO-PREMS       TO D-NO2.
           MOVE "# PROP CLAIMS     :"    TO D-PROMPT3.
           MOVE GATX-NO-PROP-CLAIMS      TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APP-PRIN (TOTAL)  :"    TO D-PROMPT1.
           MOVE GATX-REB-TOT-APP-PRIN    TO D-AMT1.
           MOVE "AMT AUTO PREMIUMS :"    TO D-PROMPT2.
           MOVE GATX-AUTO-PREMS          TO D-AMT2.
           MOVE "# AUTO CLAIMS     :"    TO D-PROMPT3.
           MOVE GATX-NO-AUTO-CLAIMS      TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SMALL LOANS A/R   :"    TO D-PROMPT1.
           MOVE GATX-SMALL-LOAN-AR       TO D-AMT1.
           MOVE "AMT AUTO REFUNDS  :"    TO D-PROMPT2.
           MOVE GATX-AUTO-REFS           TO D-AMT2.
           MOVE "AMT NONFILE CLAIMS:"    TO D-PROMPT3.
           MOVE GATX-NONFILE-CLAIMS      TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# LIFE POLICIES   :"    TO D-PROMPT1.
           MOVE GATX-NO-LIFE-PREMS       TO D-NO1.
           MOVE "# NONFILE POLICIES:"    TO D-PROMPT2.
           MOVE GATX-NO-NONFILE-PREMS    TO D-NO2.
           MOVE "# NONFILE CLAIMS  :"    TO D-PROMPT3.
           MOVE GATX-NO-NONFILE-CLAIMS   TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AMT LIFE PREMIUMS :"    TO D-PROMPT1.
           MOVE GATX-LIFE-PREMS          TO D-AMT1.
           MOVE "AMT NONFILE PREMS :"    TO D-PROMPT2.
           MOVE GATX-NONFILE-PREMS       TO D-AMT2.
           MOVE "# LOANS           :"    TO D-PROMPT3.
           MOVE GATX-NO-LOANS            TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

      * 1/13/2025 #1697
           MOVE "# CLASS 50 LOANS  :"    TO D-PROMPT1.
           MOVE GATX-NO-GA-50-LOANS      TO D-NO1.
           MOVE "# 4% 8% REFUNDS   :"    TO D-PROMPT2.
           MOVE GATX-NO-REB-4-8          TO D-NO2.
           MOVE "# CLOSING FEE REFS:"    TO D-PROMPT3.
           MOVE GATX-NO-CLOSFEE-REB      TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AMT CLASS 50 LOANS:"    TO D-PROMPT1.
           MOVE GATX-AMT-GA-50-LOANS     TO D-AMT1.
           MOVE "AMT 4% 8% REFUNDS :"    TO D-PROMPT2.
           MOVE GATX-AMT-REB-4-8         TO D-AMT2.
           MOVE "AMT CLOSE FEE REF :"    TO D-PROMPT3.
           MOVE GATX-AMT-CLOSFEE-REB     TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# MAINTFEE REFUNDS:"    TO D-PROMPT1.
           MOVE GATX-NO-REB-MF           TO D-NO1.
           MOVE "# TRANS W/LATE FEE:"    TO D-PROMPT2.
           MOVE GATX-NO-APLC             TO D-NO2.
           MOVE "# INTEREST REFS   :"    TO D-PROMPT3.
           MOVE GATX-NO-REB-INT          TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AMT MAINTFEE REFS :"    TO D-PROMPT1.
           MOVE GATX-AMT-REB-MF          TO D-AMT1.
           MOVE "AMT TRANS LATE FEE:"    TO D-PROMPT2.
           MOVE GATX-AMT-APLC            TO D-AMT2.
           PERFORM WRITE-DETAIL-LINE.

      *  PRINT 3 BRANCHES PER PAGE:

           DIVIDE RECORD-COUNT BY 3 GIVING RESULT-WK
              REMAINDER REMAINDER-WK.
           IF REMAINDER-WK = 0
              MOVE 99 TO LN-COUNT.

           GO TO NEXT-WK.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE GATXEX-QUEUE-POS TO Q-POS.
           MOVE GATXEX-COPIES-POS TO C-POS.
           MOVE GATXEX-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           MOVE 0 TO PAGE-NUMBER.
           MOVE BEG-MONTH TO DATE-MMDDYY-MM.
           MOVE BEG-YEAR  TO DATE-MMDDYY-YY.
           MOVE 1         TO DATE-MMDDYY-DD.
           MOVE DATE-MMDDYY TO H-DATE1.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 50        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO DATE-YYYYMMDD ENDING-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-DATE2.

           PERFORM FIND-END-FISCAL-PERIOD.

       
      * CREATE INDEX KEY FILE FOR TRFILE OUTPUT
       
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF REPORT-ONLY-FG = "N"
              PERFORM LOAD-GATX-FILE
              PERFORM OPEN-GATX-FILE-OUTPUT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE ZEROS TO GRAND-TOTALS.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-WR1-FILE.
           PERFORM OPEN-BY1-FILE.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           CALL "C$JUSTIFY" USING H-NAME "C".
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

       
           IF ( GROUP-FLAG = "Y" )
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO NUM-DATE.
           MOVE NUM-MO TO BEG-MONTH.
           MOVE NUM-YR TO BEG-YEAR.
           MOVE EXT-EOBUF-DATE2 TO NUM-DATE.
           MOVE NUM-MO TO END-MONTH.
           MOVE NUM-YR TO END-YEAR.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO REPORT-ONLY-YN.

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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
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
       
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO REPORT-ONLY-YN.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-BEG-DATE.
           MOVE VDUNUM0 TO BDATE.

           IF BEG-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

       ENTER-END-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO REPORT-ONLY-YN.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BEG-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-END-DATE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

           MOVE BEG-MONTH TO DATE-MMDDYY-MM.
           MOVE 1         TO DATE-MMDDYY-DD.
           MOVE BEG-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE 1         TO DATE-MMDDYY-DD.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYS-DATE.

           IF SYS-DATE < NUM-DATE
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

      *-----------------------------------------------------------------

       REPORT-ONLY-YN.
           MOVE LEGEND-REPORT-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010REPORTONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO ENTER-END-DATE.

           IF VDUSTATUS NOT = "00" AND "1D" GO TO REPORT-ONLY-YN.
           MOVE VDUBUF TO REPORT-ONLY-FG.

           IF REPORT-ONLY-FG NOT = "N" AND NOT = "Y"
              GO TO REPORT-ONLY-YN.

           IF REPORT-ONLY-FG = "Y"
              GO TO END-PARM.

           PERFORM LOAD-GATX-FILE.
           MOVE GATX-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "FILE EXISTS FOR THIS DATE, WILL BE OVERWRITTEN"
                TO MESS
              PERFORM SEND-MESS.

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
              MOVE ENT-GROUP TO VDUBUF
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
      
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A010REPORTONLY." TO VDU.
           MOVE REPORT-ONLY-FG       TO VDUBUF.
           PERFORM SCREENIO.

      ***********************************************************
       PROCESS-TRP-TRANSACTIONS SECTION.
      ***********************************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE.
           MOVE 1                    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.

       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO PROCESS-TRP-EXIT.

           IF TP-DATE > ENDING-DATE
              GO TO PROCESS-TRP-EXIT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TRP-NEXT.

      *  TYPE 2 P&L RECOVERIES

           IF TP-OTHBR-FG = "C"
              GO TO CHECK-FOR-PLS.

           IF TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5" OR "WI"
              GO TO CHECK-FOR-PLS.

           IF TP-PLCD  = SPACES
              GO TO CHECK-FOR-PLS.

           IF TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST" OR
              (TP-TRCD = "RV" AND (TP-REFNO = "ADD" OR "ADJ" OR "CNL") )
              IF TP-PLCD = LOW-VALUES
                 GO TO CHECK-FOR-PLS.

           MOVE "X" TO TYPE-FLAG.
           PERFORM CREATE-WORK-RECORD.

       CHECK-FOR-PLS.

           IF NOT (TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
                              OR "PX")
              GO TO CHECK-FOR-REFUNDS.

           MOVE "P" TO TYPE-FLAG.
           PERFORM CREATE-WORK-RECORD.

       CHECK-FOR-REFUNDS.

           IF TP-PLCD = "P"
              GO TO SORT-TRP-NEXT.

           IF TP-TRCD = "WI"
              GO TO SORT-TRP-NEXT.

           IF TP-APLC NOT = 0
              MOVE "F" TO TYPE-FLAG
              PERFORM CREATE-WORK-RECORD.

           IF NOT (TP-TRCD =  "RC" OR "RA" OR "RP" OR "R1"
                            OR "R2" OR "R3" OR "RI" OR "RS" OR "RM"
                             OR "RD" )
      * INCLUDE ACCOUNTS WITH ACCELLERATED OR EXPIRED INTEREST
              IF TP-APINT NOT = 0 AND TP-LOANTYPE = "P"
                 NEXT SENTENCE
              ELSE
                 GO TO SORT-TRP-NEXT.

           MOVE "R" TO TYPE-FLAG.
           PERFORM CREATE-WORK-RECORD.

           GO TO SORT-TRP-NEXT.

       PROCESS-TRP-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      **************************************************
       PROCESS-TRL-TRANSACTIONS SECTION.
      **************************************************

           PERFORM OPEN-TRL1-FILE.

           MOVE BEG-CLASS      TO TL-CLASS.
           MOVE 0              TO TL-ACCTNO TL-LAWCODE TL-LINE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE BEG-CLASS           TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRL1-WEND-CLASS.
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.
           PERFORM START-TRL1-FILE.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO PROCESS-TRL-EXIT.

           IF TL-DATE > ENDING-DATE
              GO TO PROCESS-TRL-EXIT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.

           MOVE "L" TO TYPE-FLAG.
           PERFORM CREATE-WORK-RECORD.

           GO TO SORT-TRL-NEXT.

       PROCESS-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      ******************************************************************
       CREATE-WORK-RECORD SECTION.
      ******************************************************************
           MOVE BR-NO TO WK-BRNO.
           PERFORM READ-WK-FILE.
           IF IO-GOOD
              GO TO CONTINUE-CREATE-WORK.

           MOVE 0 TO WK-LNAMT
                     WK-INTCHG
                     WK-PL-RECV-AMT
                     WK-PL-RECV-INT
                     WK-PL-TRANS-AMT
                     WK-PL-UNEARNED-INT
                     WK-REB-INT-APP-PRIN
                     WK-REB-TOT-APP-INT
                     WK-REB-TOT-APP-PRIN
                     WK-SMALL-LOAN-AR
                     WK-NO-LIFE-PREMS
                     WK-LIFE-PREMS
                     WK-LIFE-REFS
                     WK-NO-AH-PREMS
                     WK-AH-PREMS
                     WK-AH-REFS
                     WK-NO-PROP-PREMS
                     WK-PROP-PREMS
                     WK-PROP-REFS
                     WK-NO-AUTO-PREMS
                     WK-AUTO-PREMS
                     WK-AUTO-REFS
                     WK-NO-NONFILE-PREMS
                     WK-NONFILE-PREMS
                     WK-NONFILE-REFS
                     WK-LIFE-CLAIMS
                     WK-AH-CLAIMS
                     WK-NO-LIFE-CLAIMS
                     WK-NO-AH-CLAIMS
                     WK-PROP-CLAIMS
                     WK-AUTO-CLAIMS
                     WK-NO-PROP-CLAIMS
                     WK-NO-AUTO-CLAIMS
                     WK-NONFILE-CLAIMS
                     WK-NO-NONFILE-CLAIMS
                     WK-NO-LOANS
                     WK-NO-GA-50-LOANS
                     WK-AMT-GA-50-LOANS
                     WK-NO-REB-4-8
                     WK-AMT-REB-4-8
                     WK-NO-CLOSFEE-REB
                     WK-AMT-CLOSFEE-REB
                     WK-NO-REB-INT
                     WK-NO-REB-MF
                     WK-AMT-REB-MF
                     WK-NO-APLC
                     WK-AMT-APLC.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       CONTINUE-CREATE-WORK.

      * LATE FEES COLLECTED
           IF TYPE-FLAG = "F"
               ADD TP-APLC TO WK-AMT-APLC
               IF TP-APLC < 0
                  SUBTRACT 1 FROM WK-NO-APLC
               ELSE
                  ADD 1 TO WK-NO-APLC.

           IF TYPE-FLAG = "L"
              ADD TL-LNAMT TO WK-LNAMT
              ADD TL-INTCHG TO WK-INTCHG
              IF TL-LNAMT < 0
                 SUBTRACT 1 FROM WK-NO-LOANS
              ELSE
                 ADD 1 TO WK-NO-LOANS.

      * GA CLASS 50 LOANS $3000 AND LESS
           IF TYPE-FLAG = "L"
              IF TL-CLASS = 50
                 IF TL-LNAMT > 0 AND TL-LNAMT NOT > 3000.00
                    ADD 1 TO WK-NO-GA-50-LOANS
                    ADD TL-LNAMT TO WK-AMT-GA-50-LOANS
                 ELSE
                 IF TL-LNAMT < 0
                    IF (TL-LNAMT * -1) NOT > 3000.00
                       SUBTRACT 1 FROM WK-NO-GA-50-LOANS
                       ADD TL-LNAMT TO WK-AMT-GA-50-LOANS.

           IF TYPE-FLAG = "I"
              PERFORM UPDATE-INSURANCE.

           IF TYPE-FLAG = "C"
              ADD WR-LIFE-CLAIMS(1)       TO WK-LIFE-CLAIMS
              ADD WR-AH-CLAIMS(1)         TO WK-AH-CLAIMS
              ADD WR-NO-LIFE-CLAIMS(1)    TO WK-NO-LIFE-CLAIMS
              ADD WR-NO-AH-CLAIMS(1)      TO WK-NO-AH-CLAIMS
              ADD WR-FIRE-CLAIMS(1)       TO WK-PROP-CLAIMS
              ADD WR-AUTO-CLAIMS(1)       TO WK-AUTO-CLAIMS
              ADD WR-NO-FIRE-CLAIMS(1)    TO WK-NO-PROP-CLAIMS
              ADD WR-NO-AUTO-CLAIMS(1)    TO WK-NO-AUTO-CLAIMS
              ADD WR-LOTS-NF-CLAIMS(1)    TO WK-NONFILE-CLAIMS
              ADD WR-LOTS-NO-NF-CLAIMS(1) TO WK-NO-NONFILE-CLAIMS.

           IF TYPE-FLAG = "G"
              MOVE GL-OPENBAL TO WK-SMALL-LOAN-AR
              PERFORM VARYING SUB FROM 1 BY 1
                           UNTIL SUB > END-PERIOD
                 ADD GL-CYBAL(SUB) TO WK-SMALL-LOAN-AR
              END-PERFORM.

           IF TYPE-FLAG = "X"
              IF ( TP-OTHBR-FG = " " OR "G" )
                 IF NOT (TP-TRCD = "PI" OR "OT")
                    ADD TP-APINT    TO WK-PL-RECV-INT
                    IF TP-TRCD NOT = "JD"
                       ADD TP-TRAMT TO WK-PL-RECV-AMT.

           IF TYPE-FLAG = "P"
              IF (TP-OTHBR-FG = " " OR "C")
                 IF (TP-TRCD = "PL" OR "P2" OR "P3" OR "P4"
                           OR "P5" OR "PX")
                    ADD TP-TRAMT TO WK-PL-TRANS-AMT
                 END-IF
              END-IF
              IF TP-OTHBR-FG = " " OR "G"
                 ADD TP-WORKAMT(1) TO WK-PL-UNEARNED-INT.

           IF TYPE-FLAG = "R"
      * APINT IS THE INTEREST COLLECTED ON PRECOMP ACCOUNTS
              IF TP-OTHBR-FG = " " OR "G"
                 ADD TP-APINT TO WK-REB-TOT-APP-INT
      * NEED THIS STATEMENT SO THE INTEREST COLLECTED ONLY ACCOUNTS
      * DONT ADD IN THEIR PRINCIPAL !!
                 IF (TP-TRCD =  "RC" OR "RA" OR "RP" OR "R1" OR "RD"
                          OR "R2" OR "R3" OR "RI" OR "RS" OR "RM")
                    ADD TP-APCUR TO WK-REB-TOT-APP-PRIN.

      * MAINTENANCE FEE REFUNDS
           IF TYPE-FLAG = "R"
              IF TP-OTHBR-FG = " " OR "G"
                 IF TP-TRCD =  "RM"
                    ADD TP-APCUR TO WK-AMT-REB-MF
                    IF TP-APCUR < 0
                       SUBTRACT 1 FROM WK-NO-REB-MF
                    ELSE
                       IF TP-APCUR > 0
                         ADD 1 TO WK-NO-REB-MF.

           IF TYPE-FLAG = "R"
              IF TP-OTHBR-FG = " " OR "G"
                 IF TP-TRCD =  "RI"
                    ADD TP-APCUR TO WK-REB-INT-APP-PRIN.

      * SERVICE CHARGE REFUNDS THAT HAD 4% 8% PORTION REFUNDED
           IF TYPE-FLAG = "R"
              IF (TP-OTHBR-FG = " " OR "G")
                 IF TP-TRCD = "RS"
      * REVERSAL
                    IF TP-TRAMT < 0
                       COMPUTE TEST-WORKAMT = (TP-TRAMT * -1) -
                                    (TP-CLOSFEE-REB-AMT * -1)
                       IF TEST-WORKAMT > 0
                          SUBTRACT 1 FROM WK-NO-REB-4-8
                          SUBTRACT TEST-WORKAMT FROM WK-AMT-REB-4-8.
           IF TYPE-FLAG = "R"
              IF (TP-OTHBR-FG = " " OR "G")
                 IF TP-TRCD = "RS"
                    IF TP-TRAMT > 0
                       COMPUTE TEST-WORKAMT = TP-TRAMT
                                            - TP-CLOSFEE-REB-AMT
                       IF TEST-WORKAMT > 0
                          ADD 1 TO WK-NO-REB-4-8
                          ADD TEST-WORKAMT TO WK-AMT-REB-4-8.

      * SERVICE CHARGE REFUNDS THAT HAD CLOSING FEE PORTION REFUNDED
           IF TYPE-FLAG = "R"
              IF (TP-OTHBR-FG = " " OR "G")
                 IF TP-TRCD = "RS"
                    ADD TP-CLOSFEE-REB-AMT TO WK-AMT-CLOSFEE-REB
                    IF TP-CLOSFEE-REB-AMT > 0
                       ADD 1 TO WK-NO-CLOSFEE-REB
                    ELSE
                    IF TP-CLOSFEE-REB-AMT < 0
                       SUBTRACT 1 FROM WK-NO-CLOSFEE-REB.

           PERFORM REWRITE-WK-FILE.

      *************************************
       WRITE-HEADER-RECORD SECTION.
      *************************************
           MOVE SPACES TO GATX-REC.
           STRING "BRNO", TAB-1,
                  "LOAN AMT", TAB-1,
                  "INT CHARGE", TAB-1,
                  "PL RECV AMT", TAB-1,
                  "PL RECV INT", TAB-1,
                  "PL TRANS AMT", TAB-1,
                  "PL UNEARN INT", TAB-1,
                  "REB INT APP PRIN", TAB-1,
                  "REB TOT APP INT", TAB-1,
                  "REB TOT APP PRIN", TAB-1,
                  "SMALL LOAN AR", TAB-1,
                  "NO. LIFE PREMS", TAB-1,
                  "LIFE PREMIUMS", TAB-1,
                  "LIFE REFUNDS", TAB-1,
                  "NO. AH PREMIUMS", TAB-1,
                  "AH PREMIUMS", TAB-1,
                  "AH REFUNDS", TAB-1,
                  "NO. PROP PREMS", TAB-1,
                  "PROP PREMIUMS", TAB-1,
                  "PROP REFUNDS", TAB-1,
                  "NO. AUTO PREMS", TAB-1,
                  "AUTO PREMIUMS", TAB-1,
                  "AUTO REFUNDS", TAB-1,
                  "NO. NONFLE PREMS", TAB-1,
                  "NONFLE PREMIUMS", TAB-1,
                  "NONFLE REFUNDS", TAB-1,
                  "LIFE CLAIMS", TAB-1,
                  "AH CLAIMS", TAB-1,
                  "NO. LIFE CLAIMS", TAB-1,
                  "NO. AH CLAIMS", TAB-1,
                  "PROP CLAIMS", TAB-1,
                  "AUTO CLAIMS", TAB-1,
                  "NO. PROP CLAIMS", TAB-1,
                  "NO. AUTO CLAIMS", TAB-1,
                  "NONFLE CLAIMS", TAB-1,
                  "NO. NONFLE CLAIMS", TAB-1,
                  "NO. LOANS", TAB-1,
                  "NO. CL50 LOANS", TAB-1,
                  "AMT CL50 LOANS", TAB-1,
                  "NO. 4%8% REFS", TAB-1,
                  "AMT 4%8% REFS", TAB-1,
                  "NO. CLOSEFEE REFS", TAB-1,
                  "AMT CLOSEFEE REFS", TAB-1,
                  "NO. INTEREST REFS", TAB-1,
                  "NO. MAINTFEE REFS", TAB-1,
                  "AMT MAINTFEE REFS", TAB-1,
                  "NO. APPLIED LCHG", TAB-1,
                  "AMT. APPLIED LCHG", CR-1
            DELIMITED BY SIZE INTO GATX-REC.
           PERFORM WRITE-GATX-FILE.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE 1 TO RECORD-COUNT.

       EXIT-CREATE-HEADER-RECORD.
           EXIT.

      ********************************************
       WRITE-DETAIL-RECORD SECTION.
      ********************************************
           MOVE SPACES TO GATX-REC.

           STRING
                 GATX-BRNO             , TAB-1,
                 GATX-LNAMT            , TAB-1,
                 GATX-INTCHG           , TAB-1,
                 GATX-PL-RECV-AMT      , TAB-1,
                 GATX-PL-RECV-INT      , TAB-1,
                 GATX-PL-TRANS-AMT     , TAB-1,
                 GATX-PL-UNEARNED-INT  , TAB-1,
                 GATX-REB-INT-APP-PRIN , TAB-1,
                 GATX-REB-TOT-APP-INT  , TAB-1,
                 GATX-REB-TOT-APP-PRIN , TAB-1,
                 GATX-SMALL-LOAN-AR    , TAB-1,
                 GATX-NO-LIFE-PREMS    , TAB-1,
                 GATX-LIFE-PREMS       , TAB-1,
                 GATX-LIFE-REFS        , TAB-1,
                 GATX-NO-AH-PREMS      , TAB-1,
                 GATX-AH-PREMS         , TAB-1,
                 GATX-AH-REFS          , TAB-1,
                 GATX-NO-PROP-PREMS    , TAB-1,
                 GATX-PROP-PREMS       , TAB-1,
                 GATX-PROP-REFS        , TAB-1,
                 GATX-NO-AUTO-PREMS    , TAB-1,
                 GATX-AUTO-PREMS       , TAB-1,
                 GATX-AUTO-REFS        , TAB-1,
                 GATX-NO-NONFILE-PREMS , TAB-1,
                 GATX-NONFILE-PREMS    , TAB-1,
                 GATX-NONFILE-REFS     , TAB-1,
                 GATX-LIFE-CLAIMS      , TAB-1,
                 GATX-AH-CLAIMS        , TAB-1,
                 GATX-NO-LIFE-CLAIMS   , TAB-1,
                 GATX-NO-AH-CLAIMS     , TAB-1,
                 GATX-PROP-CLAIMS      , TAB-1,
                 GATX-AUTO-CLAIMS      , TAB-1,
                 GATX-NO-PROP-CLAIMS   , TAB-1,
                 GATX-NO-AUTO-CLAIMS   , TAB-1,
                 GATX-NONFILE-CLAIMS   , TAB-1,
                 GATX-NO-NONFILE-CLAIMS, TAB-1,
                 GATX-NO-LOANS         , TAB-1,
                 GATX-NO-GA-50-LOANS   , TAB-1,
                 GATX-AMT-GA-50-LOANS  , TAB-1,
                 GATX-NO-REB-4-8       , TAB-1,
                 GATX-AMT-REB-4-8      , TAB-1,
                 GATX-NO-CLOSFEE-REB   , TAB-1,
                 GATX-AMT-CLOSFEE-REB  , TAB-1,
                 GATX-NO-REB-INT       , TAB-1,
                 GATX-NO-REB-MF        , TAB-1,
                 GATX-AMT-REB-MF       , TAB-1,
                 GATX-NO-APLC          , TAB-1,
                 GATX-AMT-APLC         , CR-1
              DELIMITED BY SIZE INTO GATX-REC.
           PERFORM WRITE-GATX-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
       EXIT-WRITE-DETAIL-RECORD.
           EXIT.

      ************************************
       UPDATE-INSURANCE SECTION.
      ************************************
           ADD BY-INS-NO-WROTE(1,1) TO WK-NO-LIFE-PREMS.
           ADD BY-INS-AMT-WROTE(1,1) TO WK-LIFE-PREMS.
           ADD BY-CLREB1(1) TO WK-LIFE-REFS.
           ADD BY-CLREB2(1) TO WK-LIFE-REFS.
           ADD BY-CLREB3(1) TO WK-LIFE-REFS.
           ADD BY-CLREB4(1) TO WK-LIFE-REFS.

           ADD BY-INS-NO-WROTE(1,2) TO WK-NO-AH-PREMS.
           ADD BY-INS-AMT-WROTE(1,2) TO WK-AH-PREMS.
           ADD BY-AHREB(1) TO WK-AH-REFS.

           ADD BY-INS-NO-WROTE(1,3) TO WK-NO-PROP-PREMS.
           ADD BY-INS-AMT-WROTE(1,3) TO WK-PROP-PREMS.
           ADD BY-PPREB(1) TO WK-PROP-REFS.

           ADD BY-INS-NO-WROTE(1,6) TO WK-NO-AUTO-PREMS.
           ADD BY-INS-AMT-WROTE(1,6) TO WK-AUTO-PREMS.
           ADD BY-O3REB(1) TO WK-AUTO-REFS.

           ADD BY-FEE-NO-WROTE(1,1) TO WK-NO-NONFILE-PREMS.
           ADD BY-FEE-AMT-WROTE(1,1) TO WK-NONFILE-PREMS.
           MOVE 0 TO WK-NONFILE-REFS.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       FIND-END-FISCAL-PERIOD SECTION.
      ******************************************************************

      *  FIND ENDING FISCAL PERIOD - MATCH UP REPORT ENDING DATE
      *  WITH DATE IN FISCAL TABLE TO GET GL PERIOD

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE 99        TO DATE-MMDDYY-DD.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.

           MOVE 1 TO END-PERIOD.
       NEXT-END-FISCAL-PERIOD.
           IF NDTE-DATE NOT > GP-END-FISCAL-DATE(END-PERIOD)
              GO TO EXIT-FIND-END-FISCAL.

           IF END-PERIOD < 12
              ADD 1 TO END-PERIOD
              GO TO NEXT-END-FISCAL-PERIOD.

            MOVE "X" TO EXT-EOBUF-ERRCD.

        EXIT-FIND-END-FISCAL.
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
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
      
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 3 TO LN-FEED.
       
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
       
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGIN DATE" TO RANGE-LINE.
           MOVE BDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END   DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "RECORD COUNT" TO RANGE-LINE.
           MOVE RECORD-COUNT   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "N"    TO LCSCAN-SALEFIN.
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

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           MOVE EXT-CONAME-CONAME TO GATXEX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GATXEX-SYSDATE.
           DISPLAY GATXEX-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GATXEX-FORM-FIELDS TO WR-BUF.
           MOVE GATXEX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/GATXEX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       PRINT-GRAND-TOTALS.
           MOVE "GRAND TOTALS:"          TO D-BR-NAME.
           MOVE "TOTAL LOAN AMOUNT :"    TO D-PROMPT1.
           MOVE TOT-LNAMT                TO D-AMT1.
           MOVE "AMT LIFE REFUNDS  :"    TO D-PROMPT2.
           MOVE TOT-LIFE-REFS            TO D-AMT2.
           MOVE "AMT NONFILE REFS  :"    TO D-PROMPT3.
           MOVE TOT-NONFILE-REFS         TO D-AMT3.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES                   TO D-BR-NAME.
           MOVE "INTEREST CHARGE   :"    TO D-PROMPT1.
           MOVE TOT-INTCHG               TO D-AMT1.
           MOVE "# A&H POLICIES    :"    TO D-PROMPT2.
           MOVE TOT-NO-AH-PREMS          TO D-NO2
           MOVE "AMT LIFE CLAIMS   :"    TO D-PROMPT3.
           MOVE TOT-LIFE-CLAIMS          TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L RECOVERIES AMT:"    TO D-PROMPT1.
           MOVE TOT-PL-RECV-AMT          TO D-AMT1.
           MOVE "AMT A&H PREMIUMS  :"    TO D-PROMPT2.
           MOVE TOT-AH-PREMS             TO D-AMT2.
           MOVE "AMT A&H CLAIMS    :"    TO D-PROMPT3.
           MOVE TOT-AH-CLAIMS            TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L RECOVERIES INT:"    TO D-PROMPT1.
           MOVE TOT-PL-RECV-INT          TO D-AMT1.
           MOVE "AMT A&H REFUNDS   :"    TO D-PROMPT2.
           MOVE TOT-AH-REFS              TO D-AMT2.
           MOVE "# LIFE CLAIMS     :"    TO D-PROMPT3.
           MOVE TOT-NO-LIFE-CLAIMS       TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L TRANS AMT     :"    TO D-PROMPT1.
           MOVE TOT-PL-TRANS-AMT         TO D-AMT1.
           MOVE "# PROP POLICIES   :"    TO D-PROMPT2.
           MOVE TOT-NO-PROP-PREMS        TO D-NO2.
           MOVE "# A&H CLAIMS      :"    TO D-PROMPT3.
           MOVE TOT-NO-AH-CLAIMS         TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L UNEARNED INT  :"    TO D-PROMPT1.
           MOVE TOT-PL-UNEARNED-INT      TO D-AMT1.
           MOVE "AMT PROP PREMIUMS :"    TO D-PROMPT2.
           MOVE TOT-PROP-PREMS           TO D-AMT2.
           MOVE "AMT PROP CLAIMS   :"    TO D-PROMPT3.
           MOVE TOT-PROP-CLAIMS          TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APP-PRIN (INT)    :"    TO D-PROMPT1.
           MOVE TOT-REB-INT-APP-PRIN     TO D-AMT1.
           MOVE "AMT PROP REFUNDS  :"    TO D-PROMPT2.
           MOVE TOT-PROP-REFS            TO D-AMT2.
           MOVE "AMT AUTO CLAIMS   :"    TO D-PROMPT3.
           MOVE TOT-AUTO-CLAIMS          TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APP-INT (TOTAL)   :"    TO D-PROMPT1.
           MOVE TOT-REB-TOT-APP-INT      TO D-AMT1.
           MOVE "# AUTO POLICIES   :"    TO D-PROMPT2.
           MOVE TOT-NO-AUTO-PREMS        TO D-NO2.
           MOVE "# PROP CLAIMS     :"    TO D-PROMPT3.
           MOVE TOT-NO-PROP-CLAIMS       TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APP-PRIN (TOTAL)  :"    TO D-PROMPT1.
           MOVE TOT-REB-TOT-APP-PRIN     TO D-AMT1.
           MOVE "AMT AUTO PREMIUMS :"    TO D-PROMPT2.
           MOVE TOT-AUTO-PREMS           TO D-AMT2.
           MOVE "# AUTO CLAIMS     :"    TO D-PROMPT3.
           MOVE TOT-NO-AUTO-CLAIMS       TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SMALL LOANS A/R   :"    TO D-PROMPT1.
           MOVE TOT-SMALL-LOAN-AR        TO D-AMT1.
           MOVE "AMT AUTO REFUNDS  :"    TO D-PROMPT2.
           MOVE TOT-AUTO-REFS            TO D-AMT2.
           MOVE "AMT NONFILE CLAIMS:"    TO D-PROMPT3.
           MOVE TOT-NONFILE-CLAIMS       TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# LIFE POLICIES   :"    TO D-PROMPT1.
           MOVE TOT-NO-LIFE-PREMS        TO D-NO1.
           MOVE "# NONFILE POLICIES:"    TO D-PROMPT2.
           MOVE TOT-NO-NONFILE-PREMS     TO D-NO2.
           MOVE "# NONFILE CLAIMS  :"    TO D-PROMPT3
           MOVE TOT-NO-NONFILE-CLAIMS    TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AMT LIFE PREMIUMS :"    TO D-PROMPT1.
           MOVE TOT-LIFE-PREMS           TO D-AMT1.
           MOVE "AMT NONFILE PREMS :"    TO D-PROMPT2.
           MOVE TOT-NONFILE-PREMS        TO D-AMT2.
           MOVE "# LOANS           :"    TO D-PROMPT3.
           MOVE TOT-NO-LOANS             TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

      * 1/13/2025 #1697
           MOVE "# CLASS 50 LOANS  :"    TO D-PROMPT1.
           MOVE TOT-NO-GA-50-LOANS      TO D-NO1.
           MOVE "# 4% 8% REFUNDS   :"    TO D-PROMPT2.
           MOVE TOT-NO-REB-4-8          TO D-NO2.
           MOVE "# CLOSING FEE REFS:"    TO D-PROMPT3.
           MOVE TOT-NO-CLOSFEE-REB      TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AMT CLASS 50 LOANS:"    TO D-PROMPT1.
           MOVE TOT-AMT-GA-50-LOANS     TO D-AMT1.
           MOVE "AMT 4% 8% REFUNDS :"    TO D-PROMPT2.
           MOVE TOT-AMT-REB-4-8         TO D-AMT2.
           MOVE "AMT CLOSE FEE REF :"    TO D-PROMPT3.
           MOVE TOT-AMT-CLOSFEE-REB     TO D-AMT3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# MAINTFEE REFUNDS:"    TO D-PROMPT1.
           MOVE TOT-NO-REB-MF            TO D-NO1.
           MOVE "# TRANS W/LATE FEE:"    TO D-PROMPT2.
           MOVE TOT-NO-APLC              TO D-NO2.
           MOVE "# INTEREST REFS   :"    TO D-PROMPT3.
           MOVE TOT-NO-REB-INT           TO D-NO3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AMT MAINTFEE REFS :"    TO D-PROMPT1.
           MOVE TOT-AMT-REB-MF           TO D-AMT1.
           MOVE "AMT TRANS LATE FEE:"    TO D-PROMPT2.
           MOVE TOT-AMT-APLC             TO D-AMT2.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPBY1RN.CPY".
       COPY "LIBSP/SPWR1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
      *-----------------------------------------------------------------
       LOAD-GATX-FILE.
           MOVE EXT-FILPATH-BASE     TO GATX-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO GATX-PATH-MACHINE.
           MOVE END-MONTH TO GATX-PATH-MM.
           MOVE END-YEAR  TO GATX-PATH-YY.
       OPEN-GATX-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE GATX-PATH TO E-FILE.
           OPEN OUTPUT GATX-FILE.
           IF IO-FG = 8
              GO TO OPEN-GATX-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE GATX-FILE
              GO TO OPEN-GATX-FILE-OUTPUT.
       WRITE-GATX-FILE.
           PERFORM WRITE-IT.
           MOVE GATX-PATH TO E-FILE.
           WRITE GATX-REC.
           IF IO-FG = 8
              GO TO WRITE-GATX-FILE.
       CLOSE-GATX-FILE.
           PERFORM CLOSE-IT.
           CLOSE GATX-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH   TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GATXEX-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
