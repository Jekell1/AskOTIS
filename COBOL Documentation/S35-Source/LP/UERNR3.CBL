       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UERNR3.
       DATE-WRITTEN.    08/09/2022.
      ******************************************************************
      *
      *                        (LP)
      * DESCRIPTION:    UNEARNED REPORT WITH ACTION CODE 2 ENTRY
      *                 COPIED FROM UERNR2
      * REV:
      *  20220809 BAH #1565 SECURITIZATION
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  ALTERNATE RECORD KEY WK2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      /*****************************************************
      *   SUMMARY TOTALS WORK RECORD                       *
      ******************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK1-KEY.
               05  WK-BRANCH           PIC 9(4).
               05  WK2-KEY.
                   07  WK-CLASS        PIC 9(3).
           03  WK-NOACCTS              PIC 9(6)      COMP-3.
           03  WK-ACCERN               PIC S9(9)V99  COMP-3.
           03  WK-INT                  PIC S9(9)V99  COMP-3.
           03  WK-ADDON                PIC S9(9)V99  COMP-3.
           03  WK-SRV                  PIC S9(9)V99  COMP-3.
           03  WK-MFEE                 PIC S9(9)V99  COMP-3.
           03  WK-DISC                 PIC S9(9)V99  COMP-3.
           03  WK-OTHER                PIC S9(9)V99  COMP-3.
           03  WK-ENDING               PIC S9(9)V99  COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/UERNR3.CBL S35 08/09/22-09:49:04 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  PR-IFN        PIC X(2)    VALUE "PR".
       01  WK1-IFN       PIC X(3)    VALUE "WK1".
       01  WK2-IFN       PIC X(3)    VALUE "WK2".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.
       01  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(25)    VALUE
           "REPORT IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9(4).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  SUMMARY-FG           PIC X        VALUE "N".
       01  BR-TOTALS            PIC X        VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  HOLD-LN-OWNBR        PIC 9(4)     VALUE 0.
       01  HOLD-WK-BRANCH       REDEFINES HOLD-LN-OWNBR
                                PIC 9(4).

       01  HOLD-LN-CLASS        PIC 9(3)     VALUE 0.
       01  HOLD-WK-CLASS        REDEFINES HOLD-LN-CLASS
                                PIC 9(3).

       01  ACCERN               PIC S9(9)V99  COMP VALUE 0.
       01  ORIG-ANTIC           PIC S9(9)V99  COMP VALUE 0.
       01  WS-INT               PIC S9(9)V99  COMP VALUE 0.
       01  ADDON                PIC S9(9)V99  COMP VALUE 0.
       01  SRV                  PIC S9(9)V99  COMP VALUE 0.
       01  MFEE                 PIC S9(9)V99  COMP VALUE 0.
       01  WS-DISC              PIC S9(9)V99  COMP VALUE 0.
       01  W-OTHER              PIC S9(9)V99  COMP VALUE 0.
       01  ENDING               PIC S9(9)V99  COMP VALUE 0.
       01  SUB                  PIC 9(6) COMP-3.
       01  CLSUB                PIC 9(3) COMP-3.
       01  W-LOAN-NUM.
           03  FILLER           PIC XXX.
           03  LOAN-NUM         PIC X(13).

      *   TOTALS SUB 1 = CLASS
      *              2 = BRANCH
       01  TOTALS-TABLE.
           03  TOTAL-TBL          OCCURS 2.
               05  T-NOACCTS   PIC 9(6)         COMP.
               05  T-ACCERN    PIC S9(9)V99     COMP.
               05  T-INT       PIC S9(9)V99     COMP.
               05  T-ADDON     PIC S9(9)V99     COMP.
               05  T-SRV       PIC S9(9)V99     COMP.
               05  T-MFEE      PIC S9(9)V99     COMP.
               05  T-DISC      PIC S9(9)V99     COMP.
               05  T-OTHER     PIC S9(9)V99     COMP.
               05  T-ENDING    PIC S9(9)V99     COMP.

      ******************************************************************
      *    W S - G T T - T A B L E
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL       OCCURS 10 TIMES.
               05  WS-GTT-CLASS-TBL           OCCURS 101.
                   07  WS-GTT-NOACCTS         PIC 9(7)     COMP.
                   07  WS-GTT-ACCERN          PIC S9(9)V99 COMP.
                   07  WS-GTT-INT             PIC S9(9)V99 COMP.
                   07  WS-GTT-ADDON           PIC S9(9)V99 COMP.
                   07  WS-GTT-SRV             PIC S9(9)V99 COMP.
                   07  WS-GTT-MFEE            PIC S9(9)V99 COMP.
                   07  WS-GTT-DISC            PIC S9(9)V99 COMP.
                   07  WS-GTT-OTHER           PIC S9(9)V99 COMP.
                   07  WS-GTT-ENDING          PIC S9(9)V99 COMP.

       01  CLASS-FG            PIC X(3)      VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 999.
           03  END-CLASS       PIC 999.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  SUM-ONLY        PIC X.
           03  ACTION-CODE2    PIC X(4).
           03  GTT-ISO         PIC X.
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE     PIC X(1)     OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(30)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(38)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(27)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "UNEARNED INCOME REPORT (SECURITIZATION)  ".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(97)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-LCNO      PIC 99.
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(20).
           03  FILLER      PIC X(99) VALUE " ".

       01  HEAD5.
           03  FILLER      PIC X(29)    VALUE SPACE.
           03  FILLER      PIC X(22)    VALUE
               "ANTICIPATED  ADJUST TO".
           03  FILLER      PIC X(44)    VALUE
               "   ------------------- UNEARNED INCOME -----".
           03  FILLER      PIC X(20)    VALUE
               "---------------     ".
           03  FILLER      PIC X(28)    VALUE
               " TOTAL     TOTAL            ".

       01  HEAD6.
           03  FILLER      PIC X(23)    VALUE
               "ACCOUNT NO.   BORROWER ".
           03  FILLER      PIC X(5)    VALUE SPACE.
           03  FILLER      PIC X(45)    VALUE
               "TERM EARNINGS    ANTIC.  INTEREST     ADDON  ".
           03  FILLER      PIC X(48)    VALUE
           "  SERCHG  MAINTFEE   DL DISC     OTHER  UNEARNED".
           03  FILLER      PIC X(10)    VALUE
               "    EARNED".

       01  NEW-BRANCH.
           03  FILLER           PIC X(8) VALUE "BRANCH  ".
           03  D-T-BRNO         PIC 9(4).
           03  FILLER           PIC XX.
           03  D-T-BRDESC       PIC X(20).

       01  DETAIL-LINE     PIC X(132) VALUE SPACES.
       01  D-LINE1         REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-BWNAME    PIC X(14).
           03  FILLER      PIC X.
           03  D-ANTERM    PIC ZZ9            BLANK ZERO.
           03  D-EARN      PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ADJUST    PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-INT       PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ADDON     PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-SRV       PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-MFEE      PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-DISC      PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-OTHER     PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ENDING    PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-ACCERN    PIC ZZZZZ9.99-     BLANK ZERO.

       01  CLASS-TOTAL REDEFINES DETAIL-LINE.
           03  D-TAG            PIC X(24).
           03  FILLER           PIC X.
           03  D-T-NOACCTS-T    PIC ZZZZZ9.
           03  D-T-TAG-T        PIC X.
           03  FILLER           PIC X(17).
           03  D-T-INT-T        PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-SRV-T        PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-DISC-T       PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-ENDING-T     PIC ZZZZZZZZ9.99-   BLANK ZERO.

       01  CLASS-TOTAL-2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(59).
           03  D-T-ADDON-T      PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-MFEE-T       PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-OTHER-T      PIC ZZZZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-ACCERN-T     PIC ZZZZZZZZ9.99-   BLANK ZERO.

       01  TOTAL-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC XXX.
           03  D-T-CLASS        PIC 99.
           03  FILLER           PIC X(12).
           03  D-T-NOACCTS      PIC ZZZZZZ9.
           03  D-T-TAG          PIC X.
           03  D-T-INT          PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-ADDON        PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-SRV          PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-MFEE         PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-DISC         PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-OTHER        PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-ENDING       PIC Z(8)9.99-   BLANK ZERO.
           03  D-T-ACCERN       PIC Z(8)9.99-   BLANK ZERO.
       01  BRANCH-GROUPID       REDEFINES DETAIL-LINE.
           03  D-TYPE           PIC X(10).
           03  D-GRID.
               05  D-BRNO       PIC 9(4).
           03  FILLER           PIC XX.
           03  D-BRDESC         PIC X(20).
           03  FILLER           PIC X(96).

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).
           03  FILLER           REDEFINES RANGE-SELECTION.
               05  RANGE-SELECTION-NUM  PIC 9(5).
       01  SUMMARY1.
           03  FILLER           PIC X(11) VALUE
               "**SUMMARY**".
           03  FILLER           PIC X(121) VALUE " ".

       01  SUMMARY2.
           03  FILLER           PIC X(22) VALUE SPACES.
           03  FILLER           PIC X(48) VALUE
               "NO     ------------------------ UNEARNED INCOME ".
           03  FILLER           PIC X(45) VALUE
               "-------------------------               TOTAL".
           03  FILLER           PIC X(13) VALUE "        TOTAL".

       01  SUMMARY3.
           03  FILLER           PIC X(29) VALUE
           "   CLASS           ACCTS     ".
           03  FILLER           PIC X(50) VALUE
           "INTEREST        ADDON       SERCHG     MAINTFEE   ".
           03  FILLER           PIC X(49) VALUE
           "   DL DISC        OTHER     UNEARNED       EARNED".

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/UERNR3_DEF.CPY".
       COPY "LIBLP/UERNR3_WKS.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/UERNR3_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "UERNR3" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE  ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
             IF EXT-ROUTE-BUF = "00"
                MOVE BR-NO TO DISPLAY-MESS-BR
                MOVE DISPLAY-MESS TO MESS
                PERFORM SEND-LEGEND.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.


           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF LN-ACTIONCD2 NOT = ACTION-CODE2
              GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM COMPUTE-EARNINGS.

      * NEED THE "AND" FOR REGENCY
      * ACCOUNTS WITH JUST UNEARN(6) WERE BEING SKIPPED BECAUSE THE
      * GP-INCL-OTH-IN-EARNED = "N" WHICH CAUSES EARN-TOT-UNEARN TO BE 0
      * BUT THEY NEED TO SEE THESE ACCOUNTS, PL#  253
           IF EARN-TOT-UNEARN = 0 AND EARN-UNEARN(6) = 0
             AND EARN-UNEARN(4) = 0
                GO TO NEXT-LN.

           IF SUM-ONLY = "Y"
              GO TO BYPASS-DETAIL.

           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-OWNBR NOT = HOLD-LN-OWNBR
              MOVE "Y" TO BR-TOTALS
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

       BYPASS-DETAIL.
           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LN.

      *-----------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           GO TO NEXT-GROUP-LOC.
      *-----------------------------------------------------------

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "Y" TO BR-TOTALS.
           IF SUM-ONLY NOT = "Y"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS.
           IF (GTT-ISO = "I" OR GTT-ISO = "O")
              MOVE 99 TO LN-COUNT
              MOVE "Y" TO SUMMARY-FG
              PERFORM GROUP-TOTAL-TABLE.
           IF GTT-ISO NOT = "O"
              PERFORM SUMMARY-TOTALS
              PERFORM CORPORATE-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE UERNR3-QUEUE-POS TO Q-POS.
           MOVE UERNR3-COPIES-POS TO C-POS.
           MOVE UERNR3-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

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

           MOVE 0 TO T-NOACCTS(1) T-NOACCTS(2) T-ACCERN(1) T-ACCERN(2)
                     T-INT(1)     T-INT(2)     T-ADDON(1)  T-ADDON (2)
                     T-SRV(1)     T-SRV(2)     T-MFEE(1)   T-MFEE(2)
                     T-DISC(1)    T-DISC(2)    T-OTHER(1)  T-OTHER (2)
                     T-ENDING(1)  T-ENDING(2).

           MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************

      *****************************************
       ENTER-PARAMETERS SECTION.
      *****************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO GTT-ISO-PARAGRAPH.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

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

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
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
              GO TO CL-01.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE " INVALID BRANCH RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE "S  A160DISPLAY-USER." TO VDU
                 MOVE "USER BRANCH ONLY" TO VDUBUF
                 PERFORM SCREENIO
              ELSE
                 MOVE "S  A160DISPLAY-USER." TO VDU
                 MOVE SPACES TO VDUBUF
                 PERFORM SCREENIO.

           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE " INVALID LOAN CLASS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO ACTIONCD2.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       ACTIONCD2.
           MOVE "E  A040ACTION." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1<"
              MOVE "A2" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO ACTIONCD2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTIONCD2.
           MOVE VDUBUF TO ACTION-CODE2.
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTIONCD2.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.
      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO EXIT-PARM.

           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO SUMYN.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

       EXIT-PARM.
           EXIT.

      *****************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************

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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A040ACTION." TO VDU
           MOVE ACTION-CODE2 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************
           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME.

           PERFORM COMPUTE-EARNINGS.

           MOVE EARN-TOT-ACCERN TO ACCERN.
           MOVE EARN-TOT-UNEARN TO ENDING.

           MOVE EARN-UNEARN(1)  TO WS-INT.
           MOVE EARN-UNEARN(2)  TO ADDON.
           MOVE EARN-UNEARN(3)  TO SRV.
           MOVE EARN-UNEARN(4)  TO MFEE.
           MOVE EARN-UNEARN(5)  TO WS-DISC.
           MOVE EARN-UNEARN(6)  TO W-OTHER.

           ADD EARN-TOT-ANTICERN EARN-TOT-ANTICADJ
                                        GIVING ORIG-ANTIC.
           MOVE ORIG-ANTIC TO D-EARN.
           MOVE EARN-TOT-ANTICADJ TO D-ADJUST.

           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO W-LOAN-NUM.
           MOVE LOAN-NUM         TO D-LOAN-NUM.
           MOVE LN-ANTICTERM TO D-ANTERM.
           MOVE ACCERN       TO D-ACCERN.
           MOVE WS-INT       TO D-INT.
           MOVE ADDON        TO D-ADDON.
           MOVE SRV          TO D-SRV.
           MOVE MFEE         TO D-MFEE.
           MOVE WS-DISC      TO D-DISC.
           MOVE W-OTHER      TO D-OTHER.
           MOVE ENDING       TO D-ENDING.

           IF SUM-ONLY NOT = "Y"
              PERFORM WRITE-DETAIL-LINE.

           PERFORM UPDATE-WK-FILE.
           ADD 1       TO T-NOACCTS(1) T-NOACCTS(2).
           ADD ACCERN  TO T-ACCERN(1)  T-ACCERN(2).
           ADD WS-INT  TO T-INT(1)     T-INT(2).
           ADD ADDON   TO T-ADDON(1)   T-ADDON(2).
           ADD SRV     TO T-SRV(1)     T-SRV(2).
           ADD MFEE    TO T-MFEE(1)    T-MFEE(2).
           ADD WS-DISC TO T-DISC(1)    T-DISC(2).
           ADD W-OTHER TO T-OTHER(1)   T-OTHER(2).
           ADD ENDING  TO T-ENDING(1)  T-ENDING(2).

       END-DETAIL-LN-PROCESS.
           EXIT.

      *****************************************
       UPDATE-WK-FILE SECTION.
      *****************************************
       UPDATE-WK.
           MOVE LN-OWNBR TO WK-BRANCH.
           MOVE LN-CLASS TO WK-CLASS.
           PERFORM READ-WK1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-WK-FIELDS.

           MOVE SPACES   TO WK-REC.
           MOVE LN-OWNBR TO WK-BRANCH.
           MOVE LN-CLASS TO WK-CLASS.
           MOVE 0 TO WK-NOACCTS WK-ACCERN WK-INT   WK-ADDON WK-SRV
                     WK-MFEE    WK-DISC   WK-OTHER WK-ENDING.
           PERFORM WRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO UPDATE-WK.

       UPDATE-WK-FIELDS.
           ADD 1       TO WK-NOACCTS.
           ADD ACCERN  TO WK-ACCERN.
           ADD WS-INT  TO WK-INT.
           ADD ADDON   TO WK-ADDON.
           ADD SRV     TO WK-SRV.
           ADD MFEE    TO WK-MFEE.
           ADD WS-DISC TO WK-DISC.
           ADD W-OTHER TO WK-OTHER.
           ADD ENDING  TO WK-ENDING.

           PERFORM REWRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       END-UPDATE-WK-FILE.
           EXIT.

      ******************************************
       PROCESS-BORROWER SECTION.
      ******************************************
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "**NOT ON FILE**" TO D-BWNAME
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-BWNAME.
       END-PROCESS-BORROWER.
           EXIT.

      ******************************************
       HEAD-PAGE SECTION.
      ******************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           IF SUMMARY-FG = "Y"
              GO TO HEAD-SUMMARY.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF SUMMARY-FG = "Y"
              GO TO HEAD-SUMMARY.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
           GO TO END-HEAD-PAGE.
       HEAD-SUMMARY.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY1 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY2 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************

           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACTION CODE 2      :" TO RANGE-LINE.
           MOVE ACTION-CODE2           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY       :" TO RANGE-LINE.
           MOVE SUM-ONLY               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GROUP TOTALS       :" TO RANGE-LINE.
           MOVE GTT-ISO                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************
           MOVE HOLD-WK-BRANCH TO BR-NO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

      *********************************************
       SET-CLASS-HEADINGS SECTION.
      *********************************************
           MOVE HOLD-WK-CLASS TO H-LCNO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC TO H-LCDESC.

      *********************************************
       CLASS-TOTALS SECTION.
      *********************************************
           MOVE 3 TO LN-FEED.
           MOVE "CLASS TOTALS:" TO D-TAG.
           MOVE 1 TO SUB.
           PERFORM PRINT-TOTALS.
           IF BR-TOTALS = "N"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED.
       END-CLASS-TOTALS.
           MOVE 0 TO T-NOACCTS(1) T-ACCERN(1) T-INT(1) T-ADDON(1)
                     T-SRV(1)     T-MFEE(1)   T-DISC(1) T-OTHER(1)
                     T-ENDING(1).

      ******************************************
       BRANCH-TOTALS SECTION.
      ******************************************
           MOVE 2 TO LN-FEED.
           MOVE "BRANCH TOTALS:" TO D-TAG.
           MOVE 2 TO SUB.
           IF SUMMARY-FG = "N"
              PERFORM PRINT-TOTALS
           ELSE
              PERFORM PRINT-SUM-BRANCH.
           IF SUMMARY-FG = "N"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED.
           MOVE "N" TO BR-TOTALS.
       END-BRANCH-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2)     T-MFEE(2)   T-DISC(2) T-OTHER(2)
                     T-ENDING(2).

      ******************************************
       PRINT-CORPORATE-TOTS SECTION.
      ******************************************
           MOVE 2 TO LN-FEED.
           MOVE "CORPORATE TOTALS:" TO D-TAG.
           MOVE 1 TO SUB.
           PERFORM PRINT-SUM-BRANCH.
       END-CORP-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2)     T-MFEE(2)   T-DISC(2) T-OTHER(2)
                     T-ENDING(2).

      ******************************************
       PRINT-TOTALS SECTION.
      ******************************************
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS-T.
           MOVE "#" TO D-T-TAG-T.
           MOVE T-ACCERN(SUB) TO D-T-ACCERN-T.
           MOVE T-ADDON(SUB) TO D-T-ADDON-T.
           MOVE T-MFEE(SUB) TO D-T-MFEE-T.
           MOVE T-OTHER(SUB) TO D-T-OTHER-T.
           PERFORM WRITE-DETAIL-LINE.
           MOVE T-INT(SUB) TO D-T-INT-T.
           MOVE T-SRV(SUB) TO D-T-SRV-T.
           MOVE T-DISC(SUB) TO D-T-DISC-T.
           MOVE T-ENDING(SUB) TO D-T-ENDING-T.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       PRINT-SUM-BRANCH SECTION.
      ******************************************
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.
           MOVE T-ACCERN(SUB) TO D-T-ACCERN.
           MOVE T-ADDON(SUB) TO D-T-ADDON.
           MOVE T-MFEE(SUB) TO D-T-MFEE.
           MOVE T-OTHER(SUB) TO D-T-OTHER.
           MOVE T-INT(SUB) TO D-T-INT.
           MOVE T-SRV(SUB) TO D-T-SRV.
           MOVE T-DISC(SUB) TO D-T-DISC.
           MOVE T-ENDING(SUB) TO D-T-ENDING.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       SUMMARY-TOTALS SECTION.
      ******************************************

           MOVE "Y" TO SUMMARY-FG FIRST-TIME.
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2)     T-MFEE(2)   T-DISC(2) T-OTHER(2)
                     T-ENDING(2).
           PERFORM CLOSE-WK1-FILE.

           PERFORM OPEN-WK1-FILE.
           MOVE 0 TO WK-BRANCH WK-CLASS.

           PERFORM START-WK1-FILE.
       SUMMARY-NEXT-WK.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              PERFORM BRANCH-TOTALS
              GO TO END-SUMMARY-TOTALS.
           IF FIRST-TIME = "Y"
              MOVE WK-BRANCH TO HOLD-WK-BRANCH BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE.
           IF WK-BRANCH NOT = HOLD-WK-BRANCH
              PERFORM BRANCH-TOTALS
              MOVE 2 TO LN-FEED
              MOVE WK-BRANCH TO HOLD-WK-BRANCH BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE
              MOVE 3 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              PERFORM WRITE-DETAIL-LINE
              MOVE "N" TO FIRST-TIME.

           MOVE WK-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE WK-NOACCTS TO D-T-NOACCTS.
           MOVE WK-ACCERN TO D-T-ACCERN.
           MOVE WK-INT TO D-T-INT.
           MOVE WK-ADDON TO D-T-ADDON.
           MOVE WK-SRV TO D-T-SRV.
           MOVE WK-MFEE TO D-T-MFEE.
           MOVE WK-DISC TO D-T-DISC.
           MOVE WK-OTHER TO D-T-OTHER.
           MOVE WK-ENDING TO D-T-ENDING.

           PERFORM WRITE-DETAIL-LINE.

           ADD WK-NOACCTS TO T-NOACCTS(2).
           ADD WK-ACCERN TO T-ACCERN(2).
           ADD WK-INT TO T-INT(2).
           ADD WK-ADDON TO T-ADDON(2).
           ADD WK-SRV TO T-SRV(2).
           ADD WK-MFEE TO T-MFEE(2).
           ADD WK-DISC TO T-DISC(2).
           ADD WK-OTHER TO T-OTHER(2).
           ADD WK-ENDING TO T-ENDING(2).
           GO TO SUMMARY-NEXT-WK.

       END-SUMMARY-TOTALS.
           EXIT.

      ******************************************
       CORPORATE-TOTALS SECTION.
      ******************************************
           MOVE "Y" TO SUMMARY-FG FIRST-TIME.
           MOVE 0 TO T-NOACCTS(1) T-ACCERN(1) T-INT(1)
                     T-ADDON(1) T-SRV(1) T-MFEE(1) T-DISC(1)
                     T-OTHER(1) T-ENDING(1).
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2)
                     T-ADDON(2) T-SRV(2) T-MFEE(2) T-DISC(2)
                     T-OTHER(2) T-ENDING(2).
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK2-FILE.
       CORPORATE-NEXT-WK.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              PERFORM PRINT-CORPORATE
              PERFORM PRINT-CORPORATE-TOTS
              GO TO END-CORPORATE-TOTALS.
           IF FIRST-TIME = "Y"
              MOVE 3 TO LN-FEED
              MOVE "CORPORATE TOTALS:" TO D-T-BRDESC
              MOVE WK-CLASS TO HOLD-WK-CLASS
              MOVE "N" TO FIRST-TIME.
           IF WK-CLASS NOT = HOLD-WK-CLASS
              PERFORM PRINT-CORPORATE
              MOVE WK-CLASS TO HOLD-WK-CLASS.

           ADD WK-NOACCTS TO T-NOACCTS(2).
           ADD WK-ACCERN TO T-ACCERN(2).
           ADD WK-INT TO T-INT(2).
           ADD WK-ADDON TO T-ADDON(2).
           ADD WK-SRV TO T-SRV(2).
           ADD WK-MFEE TO T-MFEE(2).
           ADD WK-DISC TO T-DISC(2).
           ADD WK-OTHER TO T-OTHER(2).
           ADD WK-ENDING TO T-ENDING(2).
           GO TO CORPORATE-NEXT-WK.

       END-CORPORATE-TOTALS.
           PERFORM CLOSE-WK1-FILE.

      *********************************************
       PRINT-CORPORATE SECTION.
      *********************************************
           MOVE HOLD-WK-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE T-NOACCTS(2) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.
           MOVE T-ACCERN(2) TO D-T-ACCERN.
           MOVE T-INT(2) TO D-T-INT.
           MOVE T-ADDON(2) TO D-T-ADDON.
           MOVE T-SRV(2) TO D-T-SRV.
           MOVE T-MFEE(2) TO D-T-MFEE.
           MOVE T-DISC(2) TO D-T-DISC.
           MOVE T-OTHER(2) TO D-T-OTHER.
           MOVE T-ENDING(2) TO D-T-ENDING.
           PERFORM WRITE-DETAIL-LINE.
           ADD T-NOACCTS(2) TO T-NOACCTS(1).
           ADD T-ACCERN(2) TO T-ACCERN(1).
           ADD T-INT(2) TO T-INT(1).
           ADD T-ADDON(2) TO T-ADDON(1).
           ADD T-SRV(2) TO T-SRV(1).
           ADD T-MFEE(2) TO T-MFEE(1).
           ADD T-DISC(2) TO T-DISC(1).
           ADD T-OTHER(2) TO T-OTHER(1).
           ADD T-ENDING(2) TO T-ENDING(1).
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2)
                     T-ADDON(2) T-SRV(2) T-MFEE(2)
                     T-DISC(2) T-OTHER(2) T-ENDING(2).

      ******************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************
           MOVE 0 TO WK-CLASS.
           MOVE WGR-SRESULT TO WK-BRANCH.
           PERFORM START-WK1-FILE.

       GTT-NEXT-WK1.

           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           IF WK-BRANCH NOT = WGR-SRESULT
              GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           IF WK-CLASS = 0
              GO TO EXIT-GTT-ADD-TOTALS.

           MOVE WK-CLASS TO CLSUB.
           PERFORM GTT-ADD-UNEARNED.
           MOVE 100 TO CLSUB.
           PERFORM GTT-ADD-UNEARNED.
           GO TO EXIT-GTT-ADD-TOTALS.
       GTT-ADD-UNEARNED.
           ADD WK-NOACCTS   TO WS-GTT-NOACCTS(GTT-TOTAL-SUB CLSUB).
           ADD WK-ACCERN    TO WS-GTT-ACCERN(GTT-TOTAL-SUB CLSUB).
           ADD WK-INT       TO WS-GTT-INT(GTT-TOTAL-SUB CLSUB).
           ADD WK-ADDON     TO WS-GTT-ADDON(GTT-TOTAL-SUB CLSUB).
           ADD WK-SRV       TO WS-GTT-SRV(GTT-TOTAL-SUB CLSUB).
           ADD WK-MFEE      TO WS-GTT-MFEE(GTT-TOTAL-SUB CLSUB).
           ADD WK-DISC      TO WS-GTT-DISC(GTT-TOTAL-SUB CLSUB).
           ADD WK-OTHER     TO WS-GTT-OTHER(GTT-TOTAL-SUB CLSUB).
           ADD WK-ENDING    TO WS-GTT-ENDING(GTT-TOTAL-SUB CLSUB).
       EXIT-GTT-ADD-TOTALS.
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

           MOVE SPACES       TO DETAIL-LINE.
           MOVE 9            TO GTT-TOTAL-SUB.
           MOVE "BRANCH #: " TO D-TYPE.
           MOVE BR-NO        TO D-BRNO.
           MOVE BR-NAME      TO D-BRDESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES       TO DETAIL-LINE.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP ID: " TO D-TYPE.
           MOVE GR-ID        TO D-GRID.
           MOVE GR-NAME      TO D-BRDESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES       TO DETAIL-LINE.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING CLSUB FROM 1 BY 1 UNTIL CLSUB > 100
              IF WS-GTT-NOACCTS(GTT-TOTAL-SUB, CLSUB) > 0
                 IF CLSUB < 100
                    MOVE CLSUB TO D-T-CLASS
                 ELSE
                    IF GTT-TOTAL-SUB = 9
                       MOVE "BRANCH TOTALS:" TO D-TAG
                    ELSE
                       MOVE "&&&& TOTALS:" TO D-TAG
                       INSPECT D-TAG REPLACING FIRST "&&&&" BY GR-ID
                    END-IF
                 END-IF
                 MOVE WS-GTT-NOACCTS(GTT-TOTAL-SUB CLSUB) TO D-T-NOACCTS
                 MOVE WS-GTT-INT(GTT-TOTAL-SUB CLSUB)     TO D-T-INT
                 MOVE WS-GTT-ADDON(GTT-TOTAL-SUB CLSUB)   TO D-T-ADDON
                 MOVE WS-GTT-SRV(GTT-TOTAL-SUB CLSUB)     TO D-T-SRV
                 MOVE WS-GTT-MFEE(GTT-TOTAL-SUB CLSUB)    TO D-T-MFEE
                 MOVE WS-GTT-DISC(GTT-TOTAL-SUB CLSUB)    TO D-T-DISC
                 MOVE WS-GTT-OTHER(GTT-TOTAL-SUB CLSUB)   TO D-T-OTHER
                 MOVE WS-GTT-ENDING(GTT-TOTAL-SUB, CLSUB) TO D-T-ENDING
                 MOVE WS-GTT-ACCERN(GTT-TOTAL-SUB CLSUB)  TO D-T-ACCERN
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      *****************************************************************
       CODE-SCAN SECTION.
      *****************************************************************

           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A040ACTION." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

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


      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO UERNR3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO UERNR3-SYSDATE.
           DISPLAY UERNR3-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UERNR3-FORM-FIELDS TO WR-BUF. 
           MOVE UERNR3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE UERNR3-HELP--FILE TO HELP-LINK-FILE.
           MOVE UERNR3-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/UERNR3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.



       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-LC.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO LC-DESC.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       OPEN-WK1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE-OUTPUT.
           UNLOCK WK-FILE.
       OPEN-WK1-FILE.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE.
           UNLOCK WK-FILE.
       READ-WK1-FILE.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       START-WK1-FILE.
           PERFORM START-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK1-KEY.
           UNLOCK WK-FILE.
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK2-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-NEXT.
       START-WK2-FILE.
           PERFORM START-IT.
           MOVE WK2-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK2-KEY.
           UNLOCK WK-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
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
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY UERNR3-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
