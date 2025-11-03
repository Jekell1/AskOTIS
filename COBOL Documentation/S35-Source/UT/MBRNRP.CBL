       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UERNRP.
       DATE-WRITTEN. 03-15-89
      ******************************************************************
      *
      *                        (LP)
      * DESCRIPTION:    UNEARNED REPORT
      *
      *=================================================================
      * REV:
      * JFF 100695 ADDED CLASS SCAN
      * JFF 070996 ADDED GROUP LOGIC
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *            READING BWFILE.
      * BLV 060497 CHANGED TO WORK FROM NEW BATCH DRIVER (BREXEC)
      * BAH 072197 EXPANDED WK TOTALS AND TOTAL-TBL TOTALS
      *            FOR AUTOPRIME, PL# 136
      * JTG 110397 EXPANDED WK TOTALS AND TOTAL-TBL TOTALS
      *            FOR RELIANCE, PL# XXX
      * BAH 120197 EXPANDED TOTAL-TBL FROM S9(7) TO S9(9) FOR RAC
      * BAH 030298 CLASS RANGE TEST NOT CORRECT
      * MJD 031798 REMOVED SCREEN DISPLAY OF ACCOUNT # DURING RUN.
      *            DISPLAY BRANCH NUMBER ONLY.
      * BAH 060198 ADDED DEALER NO. OPTINO FOR AUTOPRIME A50 UPGRADE...
      * BAH 070798 END-BRANCH WAS GETTING SET TO 999 FOR WGR-TYPE "A"
      *            AND WORLD WAS LOSING BRANCH 9992 FOR "SC"
      * MJD 990802 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      * BAH 991102 ACCOUNTS WITH JUST UNEARN(6) WERE BEING SKIPPED
      *            BECAUSE THE GP-INCL-OTH-IN-EARNED = "N" WHICH
      *            CAUSES EARN-TOT-UNEARN TO BE 0 BUT THEY NEED TO SEE
      *            THESE ACCOUNTS, PL#  253, REGENCY
      *  CS 081218 ACCOUNTS WITH UNEARNED(4) WERE SKIPPED IF GP-INCL-
      *            OTH-IN-EARNED WAS "R"   PR#210
      * BLM 130131 ADD OPTION FOR SKIP, INCLUDE, ONLY FOR GROUP TOTALS,
      *            WORLD PR# 849
      * BAH 140429 REPLACED RESERVED WORD INT/DISC TO WS PREFIX, VERYANT
      *  CS 140805 'REPORT IN PROGRESS' DISPLAYING OVER DATE WHEN RUNNING
      *            IN BATCH, IF YOU TRY AND RE-RUN BATCH WITHOUT CLEARING
      *            SCREEN, IT GIVES 'INVALID DATE' AND MUST SPACE THRU DATE
      * KEC 2017.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 181004 GLOBAL BWFILE, L2G
      * BAH 181211 GLOBAL LNFILE, L2G
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE
      * MB  20230727 PERFORMANCE TUNING - ADDED STORED PROCEDURE, ADDED
      *               STATE PARAMETER ARRAY PROCESSING
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
           "@(#)LP/UERNRP.CBL S35 06/05/23-15:25:45 >".
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

       01  LN3-CONN-OPEN-SW            PIC X           VALUE "N".

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
           03  DEALER          PIC 9999V99.
           03  GTT-ISO         PIC X.
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE     PIC X(1)     OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "C".

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
           03  FILLER           PIC X(17)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "                 UNEARNED INCOME REPORT  ".
           03  FILLER           PIC X(53) VALUE " ".

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
               05  RANGE-SELECTION-NUM  PIC 9(4).99.

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

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".

       COPY "LIBGB/GRWORKW.CPY".
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
       COPY "LIBLP/UERNRP_DEF.CPY".
       COPY "LIBLP/UERNRP_WKS.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/UERNRP_SCN.CPY".

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
      *     PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "UERNRP" TO FORM-PROG VDU-FORM-NAME.

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

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

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

      *     PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CLASS     TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE END-CLASS     TO QLN3-WEND-CLASS.
           MOVE 0             TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF DEALER NOT = 0
              IF LN-DLNO NOT = DEALER
                 GO TO NEXT-LN.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      *     IF LPWSSP-SP1-KEY NOT = SP1-KEY
      *        MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
      *        IF IO-FG NOT = 0
      *           GO TO IO-ERROR.

           PERFORM ARRAYSP-READ.
           IF NOT IO-GOOD
               STRING "MISSING SPR: ",LN-OWNBR,LN-CLASS,LN-ACCTNO
                               DELIMITED BY "  " INTO MESS
               PERFORM SEND-MESS
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

       END-OF-GROUP.
      *     PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
      D    STOP MESS.
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

      *************************************************
       INITIALIZATION SECTION.
      *************************************************

           MOVE UERNRP-QUEUE-POS TO Q-POS.
           MOVE UERNRP-COPIES-POS TO C-POS.
           MOVE UERNRP-PRU-POS   TO P-POS.
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

           PERFORM ARRAYSP-LOAD.

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

           PERFORM ENABLE-INTERRUPT.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *************************************************
       EO-EXEC-AUDIT SECTION.
      *************************************************

      *************************************************
       ENTER-PARAMETERS SECTION.
      *************************************************
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
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
      *----MOVE VDUNUM0 TO BEG-BRANCH.
      *----------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------
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
           GO TO DL-NO.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       DL-NO.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-NO.
           MOVE VDUNUM2 TO DEALER.
           GO TO SUMYN.
       ALL-DL.
           MOVE "S  A070DLNO." TO VDU.
           MOVE "****.**" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO DEALER.
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-NO.
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

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************

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
           IF DEALER = 0
              MOVE "S  A070DLNO." TO VDU
              MOVE "****.**" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN042DLNO." TO VDU
              MOVE DEALER TO VDUNUM2
              PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.

      *************************************************
       DETAIL-LINE-PROCESS SECTION.
      *************************************************
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

      *************************************************
       UPDATE-WK-FILE SECTION.
      *************************************************

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
      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "**NOT ON FILE**" TO D-BWNAME
      *        GO TO END-PROCESS-BORROWER.
 
           IF BW-FNAME = SPACES AND BW-LNAME = SPACES
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

           MOVE "DEALER NO          :" TO RANGE-LINE.
           IF DEALER = 0
              MOVE "****.**"           TO RANGE-SELECTION
           ELSE
              MOVE DEALER              TO RANGE-SELECTION-NUM.
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

      *********************************************
       BRANCH-TOTALS SECTION.
      *********************************************
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

      *********************************************
       PRINT-CORPORATE-TOTS SECTION.
      *********************************************
           MOVE 2 TO LN-FEED.
           MOVE "CORPORATE TOTALS:" TO D-TAG.
           MOVE 1 TO SUB.
           PERFORM PRINT-SUM-BRANCH.
       END-CORP-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-ACCERN(2) T-INT(2) T-ADDON(2)
                     T-SRV(2)     T-MFEE(2)   T-DISC(2) T-OTHER(2)
                     T-ENDING(2).

      *********************************************
       PRINT-TOTALS SECTION.
      *********************************************
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

      *********************************************
       PRINT-SUM-BRANCH SECTION.
      *********************************************
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

      *******************************************
       SUMMARY-TOTALS SECTION.
      *******************************************

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

      *********************************************
       CORPORATE-TOTALS SECTION.
      *********************************************
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

      *********************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      *********************************************

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
           PERFORM TRAP-INTERRUPT.

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

      ***************************************************************
       CLASS-SCAN SECTION.
      ***************************************************************
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
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
           MOVE EXT-CONAME-CONAME TO UERNRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO UERNRP-SYSDATE.
           DISPLAY UERNRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UERNRP-FORM-FIELDS TO WR-BUF. 
           MOVE UERNRP-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE UERNRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE UERNRP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/UERNRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      **************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************
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

      **************************************************
       IO-ROUTINE SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SP_UERNRP_OLD_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR, :QLN3-WBEG-CLASS, 
                    :QLN3-WEND-CLASS) 
                END-EXEC.

            EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           INITIALIZE QLN-REC QBW-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LN3_CURSOR INTO 
                :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-SERVACCT,
                   :QLN-STATFLAGS,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-ORGTERM,
                   :QLN-DLNO,
                   :QLN-CURBAL,
                   :QLN-ANTICTERM,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICERN-10,
                   :QLN-ANTICADJ-1,
                   :QLN-ANTICADJ-2,
                   :QLN-ANTICADJ-3,
                   :QLN-ANTICADJ-4,
                   :QLN-ANTICADJ-5,
                   :QLN-ANTICADJ-6,
                   :QLN-ANTICADJ-7,
                   :QLN-ANTICADJ-8,
                   :QLN-ANTICADJ-9,
                   :QLN-ANTICADJ-10,
                   :QLN-UFERN-1,
                   :QLN-UFERN-2,
                   :QLN-UFERN-3,
                   :QLN-UFERN-4,
                   :QLN-UFERN-5,
                   :QLN-UFERN-6,
                   :QLN-UFERN-7,
                   :QLN-UFERN-8,
                   :QLN-UFERN-9,
                   :QLN-UFERN-10,
                   :QLN-ACCERN-1,
                   :QLN-ACCERN-2,
                   :QLN-ACCERN-3,
                   :QLN-ACCERN-4,
                   :QLN-ACCERN-5,
                   :QLN-ACCERN-6,
                   :QLN-ACCERN-7,
                   :QLN-ACCERN-8,
                   :QLN-ACCERN-9,
                   :QLN-ACCERN-10,
                   :QLN-TOTNODEF,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QBW-FNAME,
                   :QBW-LNAME,
                   :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.


       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.


       COPY "LIBLP/LPSP1RN.CPY".
      * COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

      *COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
      * 20230713 BAH ADDED BR-USE-NEW-WFARGO-ACCT #1616
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
           SELECT BRFILE.BR_NO,
                  BRFILE.BR_NAME,
                  BRFILE.BR_LOAN_NUM_FLAG,
                  BRFILE.BR_DATA_PATH,
                  BRFILE.BR_MACHINE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
           SELECT BRFILE.BR_NO,
                  BRFILE.BR_NAME,
                  BRFILE.BR_LOAN_NUM_FLAG,
                  BRFILE.BR_DATA_PATH,
                  BRFILE.BR_MACHINE
            INTO :QBR-NO,
                 :QBR-NAME,
                 :QBR-LOAN-NUM-FLAG,
                 :QBR-DATA-PATH,
                 :QBR-MACHINE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO 
                   :QBR-NO,
                   :QBR-NAME,
                   :QBR-LOAN-NUM-FLAG,
                   :QBR-DATA-PATH,
                   :QBR-MACHINE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           PERFORM CLOSE-IT.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.




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

      **************************************************
       END-PROGRAM SECTION.
      **************************************************
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
              DESTROY UERNRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
