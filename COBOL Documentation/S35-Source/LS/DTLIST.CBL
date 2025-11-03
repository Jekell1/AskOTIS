       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DTLIST.
       DATE-WRITTEN. 022086
      ******************************************************************
      *
      *                      (LS)
      *    DESCRIPTION:  DEALER TRANSACTION FILE LISTING
      *
      * REV:
      *  JTG 083195 CHANGED TO INCLUDE PARVPAID WITH PARVHELD
      *  KEC 091695 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091695 CHANGED MILITARY TIME TO AM/PM ON HEADER.
      *  KEC 092195 SAVED DT-TRCD AND MOVED IT BACK TO DT-TRCD AFTER
      *             DISPLAYING TRCD-DESC.  INCORRECT TRCD WAS BEING
      *             ENTERED BECAUSE OF JOURNAL-SUMMARY MOVE OF DT-TRCD
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  JFF 111095 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  KEC 112995 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  BAH 121396 SKIP RESCISSION LOANS
      *  JTG 060497 ADDED TEST FOR "R3" & "C3"
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  JTG 091897 ADDED TEST FOR TRCD = 'AH', WORLD A90
      *  KEC 041498 ADDED OPTION FOR A 'STATEMENT TYPE' (DETAIL-LINE):
      *                 PAYABLE STATEMENT AND TRANSACTION STATEMENT.
      *
      *             FIXED 'RECORDS-FOUND' SO TO AVOID 'NO RECORDS FOUND'
      *              MESSAGES IF EXITING WITHOUT RUNNING.
      *
      *             ADDED 'PROGRAM-EXIT-STATUS' SO TO AVOID 'NO REPORT
      *              GENERATED' MESSAGE IF EXITING WITHOUT RUNNING.
      *   CS 042898 ADDED "RB" AND "RO" RENEWAL CODES
      *  BAH 052198 INCLUDE DT-DATE = 000000, "BF" LINE...
      *  GLF 033099 ADD TEST FOR TRCD'S 'PN' AND 'PZ' TFC
      *  JTG 990623 CORRECTED LOGIC TO GIVE SUMMARY PAGE FOR DEALER IN ALL CASES
      *             PROGRAM SKIPPED SUMMARY WHEN PREVIOUS DEALER HAD NO CLASS
      *             TOTALS TO PRINT.
      *             ALSO CHANGED PRINT LINE FOR MULTIPLE 'NW' TRANSACTIONS DUE
      *             TO MULTIPLE CHECK DISBURSMENT.
      *             ALSO REMOVED PRINT OF LN-SEQNO FROM REPORT.
      *  GLF 990924 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      *  BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181029 GLOBAL DTFILE
      *   CS 190425 FIXED REDEFINE OF WK2-KEY
      *  BAH 20190930 REMOVED ACCESS-CALL DTFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       COPY "LIBGB/GBFSWK2I.CPY".
       I-O-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *   RSZ - 119   CISZ - 512
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRANCH     PIC 9(4)     COMP.
               05  WK-DLNO       PIC 9(4)V99  COMP.
               05  WK-CLASS      PIC 9(3)     COMP.
               05  WK-SEQ        PIC 9.
               05  WK-TRCD       PIC XX.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  WK-DLRVPRIN-1     PIC S9(7)V99    COMP-3.
           03  WK-DLRVPRIN-2     PIC S9(7)V99    COMP-3.
           03  WK-DLRVPRIN-3     PIC S9(7)V99    COMP-3.
           03  WK-DLRVPRIN-TOTAL PIC S9(7)V99    COMP-3.
           03  WK-DLRVCHGS-1     PIC S9(7)V99    COMP-3.
           03  WK-DLRVCHGS-2     PIC S9(7)V99    COMP-3.
           03  WK-DLRVCHGS-3     PIC S9(7)V99    COMP-3.
           03  WK-DLRVCHGS-TOTAL PIC S9(7)V99    COMP-3.
           03  WK-DLPARVHELD     PIC S9(7)V99    COMP-3.
           03  WK-DLPARVPAID     PIC S9(7)V99    COMP-3.
           03  WK-PROCEEDS       PIC S9(7)V99    COMP-3.
           03  WK-PAYMENTS       PIC S9(7)V99    COMP-3.
      ******************************************************************
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  FILLER        PIC X(54).
           03  WK2-KEY-AREA2 REDEFINES WK2-KEY.
               05  WK2-KEY1-BRNO       PIC 9(4).
               05  WK2-KEY1-DLNO       PIC 9(4)V99.
               05  WK2-KEY1-CLASS      PIC 9(3).
               05  WK2-KEY1-ACCTNO     PIC 9(8).
               05  WK2-KEY1-DATE       PIC 9(8).
               05  WK2-KEY1-SEQ        PIC 9(6).
               05  WK2-KEY1-UNIQUE     PIC 9(4).
               05  FILLER              PIC X(15).
           03  WK2-DLNO              PIC 9(4)V99.
           03  WK2-CLASS             PIC 9(3).
           03  WK2-ACCTNO            PIC 9(8).
           03  WK2-DATE              PIC 9(8).
           03  WK2-SEQ               PIC 9(6).
           03  WK2-BRNO              PIC 9(4).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/DTLIST.CBL S35 02/21/24-12:05:06 >".

      ******************************************************************
      *
      *     F I L E   I F N
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       
       01  WK2PATH              PIC X(30).

       01  WK-IFN               PIC X(2) VALUE "WK".
       01  WK2-IFN              PIC X(3) VALUE "WK2".
      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *  MISC WORKERS
      ******************************************************************
       01  PROGRAM-EXIT-STATUS       PIC X(1)        VALUE "P".
           88  PROGRAM-EXIT-AFTER-RUN                VALUE "A".
           88  PROGRAM-EXIT-PRIOR-RUN                VALUE "P".

       01  DETAIL-PROCESS-STATUS     PIC X(1)        VALUE " ".
           88  CONTINUE-DETAIL-PROCESS               VALUE "C".
           88  END-DETAIL-PROCESS                    VALUE "X".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND-FG     PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  RECORDS-FOUND                 VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NEW-FLAG             PIC 9        VALUE 0.

       01  W-DLNO               PIC 9999V99.
       01  W-DLNUM              REDEFINES W-DLNO
                                PIC 9(6).

       01  LC                   PIC 9 VALUE 1.
       01  DL                   PIC 9 VALUE 2.
       01  LC-NET               PIC 9 VALUE 3.
       01  DL-NET               PIC 9 VALUE 4.

       
      *  HOLD-DL-PATH1 = TO HOLD THE DL-PATH OF THE PREVIOUS BRANCH
      *                  IN ORDER TO PRINT CORRECT DL-INFO IN THE
      *                  HEADER. (NEEDED BECAUSE OF STRUCTURE OF PROG.)
      *
      *  HOLD-DL-PATH2 = TO HOLD-CURRENT BRANCH DL-PATH.
       
       01  HOLD-DL-PATH1        PIC X(35)      VALUE " ".
       01  HOLD-DL-PATH2        PIC X(35)      VALUE " ".

       
      *  HOLD-BRANCH-INFO = TO HOLD BRANCH INFORMATION NEEDED ON HEADER.
       
       01  HOLD-UNIQUE          PIC 9(4).

       01  HOLD-BRANCH-INFO.
           03  HOLD-BR-NO       PIC 9(4)       VALUE 0.
           03  HOLD-BR-NAME     PIC X(40)      VALUE " ".
           03  HOLD-BR-ADR1     PIC X(24)      VALUE " ".
           03  HOLD-BR-ADR2     PIC X(24)      VALUE " ".
           03  HOLD-BR-CITY     PIC X(15)      VALUE " ".
           03  HOLD-BR-STATE    PIC X(2)       VALUE " ".
           03  HOLD-BR-ZIP      PIC X(10)      VALUE " ".

       01  SAVE-WORKERS.
           03  SAVE-CLASS       PIC 9(3).
           03  SAVE-DL          PIC 9999V99.
           03  FILLER           REDEFINES SAVE-DL.
               05  SAVE-DLNO    PIC 9999.
               05  SAVE-DLBR    PIC 9(2).
           03  SAVE-BR          PIC 9(4).

       01  SAVE-FIRST-WORKERS.
           03  SAVE-FIRST-DL    PIC 9999V99.
           03  SAVE-FIRST-BR    PIC 9(4).

       01  SAVE-ACCTNO          PIC 9(8)  COMP-3  VALUE 0.
       01  SAVE-TRCD            PIC X(2)          VALUE " ".

      ******************************************************************
      *            T A B L E S
      ******************************************************************
       01  TOTALS-TABLE.
           04  TOTALS-TYPE      OCCURS 4.
               05  DLRVPRIN-1-TOT     PIC S9(7)V99 COMP-3.
               05  DLRVPRIN-2-TOT     PIC S9(7)V99 COMP-3.
               05  DLRVPRIN-3-TOT     PIC S9(7)V99 COMP-3.
               05  DLRVPRIN-TOTAL     PIC S9(7)V99 COMP-3.
               05  DLRVCHGS-1-TOT     PIC S9(7)V99 COMP-3.
               05  DLRVCHGS-2-TOT     PIC S9(7)V99 COMP-3.
               05  DLRVCHGS-3-TOT     PIC S9(7)V99 COMP-3.
               05  DLRVCHGS-TOTAL     PIC S9(7)V99 COMP-3.
               05  DLPARVHELD-TOT     PIC S9(7)V99 COMP-3.
               05  DLPARVPAID-TOT     PIC S9(7)V99 COMP-3.
               05  PROCEEDS-TOT       PIC S9(7)V99 COMP-3.
               05  PAYMENTS-TOT       PIC S9(7)V99 COMP-3.
               05  DT-COUNTER         PIC 9(6) COMP-3.

       01  WS-LINE-WORKERS.
           03  WS-DLRVPRIN-1       PIC S9(7)V99 COMP-3.
           03  WS-DLRVPRIN-2       PIC S9(7)V99 COMP-3.
           03  WS-DLRVPRIN-3       PIC S9(7)V99 COMP-3.
           03  WS-DLRVPRIN-TOTAL   PIC S9(7)V99 COMP-3.
           03  WS-DLRVCHGS-1       PIC S9(7)V99 COMP-3.
           03  WS-DLRVCHGS-2       PIC S9(7)V99 COMP-3.
           03  WS-DLRVCHGS-3       PIC S9(7)V99 COMP-3.
           03  WS-DLRVCHGS-TOTAL   PIC S9(7)V99 COMP-3.
           03  WS-DLPARVHELD       PIC S9(7)V99 COMP-3.
           03  WS-DLPARVPAID       PIC S9(7)V99 COMP-3.
           03  WS-PROCEEDS         PIC S9(7)V99 COMP-3.
           03  WS-PAYMENTS         PIC S9(7)V99 COMP-3.
      ******************************************************************
      *     S C R E E N   F I E L D S   *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  BEG-DLNO        PIC 9(4)V99.
           03  END-DLNO        PIC 9(4)V99.
           03  END-DLNUM REDEFINES END-DLNO PIC 9(6).
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  DETAIL-OR-SUMM  PIC X.
           03  DETAIL-TYPE     PIC X(1).
               88  DETAIL-TYPE-PAYABLE     VALUE "P".
               88  DETAIL-TYPE-TRANSACTION VALUE "T".
           03  GROUP-FLAG      PIC X(1).
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  DATE1           PIC 9(8).
           03  DATE2           PIC 9(8).
           03  TRCD            PIC XX.
           03  PRINT-SEQ       PIC X.

       01  VERSION-CONTROL     PIC X(1)   VALUE "C".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

      *=================================================================
      *
      *    H E A D E R - B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE              PIC X(8)    VALUE " ".
           03  FILLER              PIC X(2)    VALUE " ".
           03  H-TIME              PIC X(8)    VALUE " ".
           03  FILLER              PIC X(30)   VALUE " ".
           03  FILLER              PIC X(31)   VALUE
                                      "-- D E A L E R   D E T A I L --".
           03  FILLER              PIC X(44)   VALUE " ".
           03  FILLER              PIC X(5)    VALUE "PAGE ".
           03  H-PAGE-NO           PIC ZZZ9.

       
       01  HEAD2.
           03  FILLER              PIC X(10)   VALUE "USER ID:".
           03  H-USERID            PIC X(10).
           03  FILLER              PIC X(55)   VALUE " ".
           03  FILLER              PIC X(57)   VALUE " ".

       
       01  HEAD3.
           03  FILLER              PIC X(52)   VALUE " ".
           03  FILLER              PIC X(7)    VALUE
               "PERIOD ".
           03  H-BEG-DATE          PIC 99/99/99.
           03  FILLER              PIC X       VALUE "-".
           03  H-END-DATE          PIC 99/99/99.
           03  FILLER              PIC X(58)   VALUE " ".

       
       01  HEAD4.
           03  FILLER              PIC X(20)   VALUE " ".
           03  FILLER              PIC X(14)   VALUE
               "PREPARED FOR: ".
           03  HD-DLNO             PIC 9(4).99.
           03  FILLER              PIC X(43)   VALUE " ".
           03  FILLER              PIC X(13)   VALUE
               "PREPARED BY: ".
           03  HD-BRNO             PIC 9(4).
           03  FILLER              PIC X(33)   VALUE " ".

       
       01  HEAD5.
           03  FILLER              PIC X(20)   VALUE " ".
           03  HD-DL-INFO          PIC X(30).
           03  FILLER              PIC X(34)   VALUE " ".
           03  HD-BR-INFO          PIC X(30).
           03  FILLER              PIC X(18)   VALUE " ".

       
       01  HEAD-PAYABLE-DT1.
           03  FILLER              PIC X(67)   VALUE " ".
           03  FILLER              PIC X(09)   VALUE "PRINCIPAL".
           03  FILLER              PIC X(04)   VALUE " ".
           03  FILLER              PIC X(07)   VALUE "CHARGES".
           03  FILLER              PIC X(04)   VALUE " ".
           03  FILLER              PIC X(07)   VALUE "PARTIC.".
           03  FILLER              PIC X(34)   VALUE " ".

       
       01  HEAD-PAYABLE-DT2.
           03  FILLER              PIC X(01)   VALUE " ".
           03  FILLER              PIC X(07)   VALUE " ".
           03  FILLER              PIC X(03)   VALUE " ".
           03  FILLER              PIC X(13)   VALUE "BORROWER NAME".
           03  FILLER              PIC X(10)   VALUE " ".
           03  FILLER              PIC X(04)   VALUE "DATE".
           03  FILLER              PIC X(02)   VALUE " ".
           03  FILLER              PIC X(16)   VALUE "CODE/DESCRIPTION".
           03  FILLER              PIC X(12)   VALUE " ".
           03  FILLER              PIC X(08)   VALUE "RESERVES".
           03  FILLER              PIC X(03)   VALUE " ".
           03  FILLER              PIC X(08)   VALUE "RESERVES".
           03  FILLER              PIC X(03)   VALUE " ".
           03  FILLER              PIC X(08)   VALUE "WITHHELD".
           03  FILLER              PIC X(03)   VALUE " ".
           03  FILLER              PIC X(08)   VALUE "PROCEEDS".
           03  FILLER              PIC X(07)   VALUE " ".
           03  FILLER              PIC X(04)   VALUE "PAID".
           03  FILLER              PIC X(04)   VALUE " ".
           03  FILLER              PIC X(07)   VALUE "CHECK #".
           03  FILLER              PIC X(01)   VALUE " ".

       
       01  HEAD-TRANSACTION-DT1.
           03  FILLER              PIC X(27)   VALUE " ".
           03  FILLER              PIC X(7)    VALUE "ACCOUNT".
           03  FILLER              PIC X(23)   VALUE " ".
           03  FILLER              PIC X(8)    VALUE "ACTIVITY".
           03  FILLER              PIC X(2)    VALUE " ".
           03  FILLER              PIC X(9)    VALUE "PRINCIPAL".
           03  FILLER              PIC X(2)    VALUE " ".
           03  FILLER              PIC X(9)    VALUE "PRINCIPAL".
           03  FILLER              PIC X(3)    VALUE " ".
           03  FILLER              PIC X(7)    VALUE "CHARGES".
           03  FILLER              PIC X(4)    VALUE " ".
           03  FILLER              PIC X(7)    VALUE "CHARGES".
           03  FILLER              PIC X(6)    VALUE " ".
           03  FILLER              PIC X(6)    VALUE "PARTIC".
           03  FILLER              PIC X(5)    VALUE " ".
           03  FILLER              PIC X(7)    VALUE "  NOTE ".

       
       01  HEAD-TRANSACTION-DT2.
           03  FILLER              PIC X(16)   VALUE "CODE/DESCRIPTION".
           03  FILLER              PIC X(12)   VALUE " ".
           03  FILLER              PIC X(6)    VALUE "NUMBER".
           03  FILLER              PIC X(2)    VALUE " ".
           03  FILLER              PIC X(4)    VALUE "NAME".
           03  FILLER              PIC X(19)   VALUE " ".
           03  FILLER              PIC X(4)    VALUE "DATE".
           03  FILLER              PIC X(4)    VALUE " ".
           03  FILLER              PIC X(9)    VALUE "RESERVE 1".
           03  FILLER              PIC X(2)    VALUE " ".
           03  FILLER              PIC X(9)    VALUE "RESERVE 2".
           03  FILLER              PIC X(2)    VALUE " ".
           03  FILLER              PIC X(9)    VALUE "RESERVE 1".
           03  FILLER              PIC X(2)    VALUE " ".
           03  FILLER              PIC X(9)    VALUE "RESERVE 2".
           03  FILLER              PIC X(4)    VALUE " ".
           03  FILLER              PIC X(9)    VALUE "HELD/PAID".
           03  FILLER              PIC X(3)    VALUE " ".
           03  FILLER              PIC X(6)    VALUE "AMOUNT".
           03  FILLER              PIC X(3)    VALUE " ".

       
       01  HEAD-DT3.
           03  FILLER              PIC X(132)  VALUE ALL "-".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE " ".

       01  DETAIL-1                 REDEFINES DETAIL-LINE.
           03  BEG-DETAIL.
               05  FILLER                     PIC XX.
               05  S-CLASS                    PIC 99.
               05  FILLER                     PIC X.
               05  D-DESC                     PIC X(16).
               05  FILLER                     PIC X(11).
           03  SUM-DETAIL-2         REDEFINES BEG-DETAIL.
               05  FILLER                     PIC XX.
               05  S-DESC                     PIC X(18).
               05  FILLER                     PIC X(12).

           03  FILLER                         PIC X(100).

       01  DETAIL-PAYABLE           REDEFINES DETAIL-LINE.
           03  D-P-PREFIX                     PIC X(31).
           03  D-P-ACCTNO-NAME      REDEFINES D-P-PREFIX.
               05  FILLER                     PIC X(3).
               05  D-P-NUMBER                 PIC Z(6).
               05  FILLER                     PIC X(2).
               05  D-P-BW-NAME                PIC X(20).
           03  FILLER                         PIC X(1).
           03  D-P-DATE                       PIC 99/99/99   BLANK ZERO.
           03  FILLER                         PIC X(1).
           03  D-P-TRCD                       PIC X(2).
           03  FILLER                         PIC X(1).
           03  D-P-TR-DESC                    PIC X(21).
           03  FILLER                         PIC X(1).
           03  D-P-DLRVPRIN                   PIC Z(7).ZZ-.
           03  D-P-DLRVCHGS                   PIC Z(7).ZZ-.
           03  D-P-DLPARVHELD                 PIC Z(7).ZZ-.
           03  D-P-PROCEEDS                   PIC Z(7).ZZ-.
           03  D-P-PAYMENTS                   PIC Z(7).ZZ-.
           03  FILLER                         PIC X(4).
           03  D-P-CHECK-NO                   PIC Z(6).
           03  FILLER                         PIC X(1).

       
       01  DETAIL-TRANSACTION       REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(1).
           03  D-T-TRCD                       PIC X(2).
           03  FILLER                         PIC X(1).
           03  D-T-TR-DESC                    PIC X(21).
           03  FILLER                         PIC X(3).
           03  D-T-NUMBER                     PIC Z(6).
           03  FILLER                         PIC X(2).
           03  D-T-BW-NAME                    PIC X(20).
           03  FILLER                         PIC X(1).
           03  D-T-DATE                       PIC 99/99/99   BLANK ZERO.
           03  FILLER                         PIC X(1).
           03  D-T-DLRVPRIN-1                 PIC Z(7).ZZ-.
           03  D-T-DLRVPRIN-2                 PIC Z(7).ZZ-.
           03  D-T-DLRVCHGS-1                 PIC Z(7).ZZ-.
           03  D-T-DLRVCHGS-2                 PIC Z(7).ZZ-.
           03  D-T-DLPARVHELD                 PIC Z(7).ZZ-.
           03  D-T-PROCEEDS                   PIC Z(7).ZZ-.

       
       01  RANGE-LN                 REDEFINES DETAIL-LINE.
           03  RANGE-LINE                     PIC X(20).
           03  RANGE-SELECTION                PIC X(112).
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y   W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/DTLIST_DEF.CPY".
       COPY "LIBLS/DTLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/DTLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                     PR-FILE WK-FILE WK2-FILE.
       
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DL1-FILE.
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
           MOVE "DTLIST" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "A" TO PROGRAM-EXIT-STATUS.
           MOVE "N" TO RECORDS-FOUND-FG.

           PERFORM OPEN-WK2-FILE.
           MOVE SPACES TO WK2-REC.
           PERFORM START-WK2-FILE.
       
       NEXT-WK2.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF FIRST-TIME = "Y"
              PERFORM SETUP-BRANCH-INFO
              MOVE "Y" TO RECORDS-FOUND-FG
              MOVE "N" TO FIRST-TIME
              MOVE WK2-BRNO  TO SAVE-BR SAVE-FIRST-BR
              MOVE WK2-DLNO  TO SAVE-DL SAVE-FIRST-DL
              MOVE WK2-CLASS TO SAVE-CLASS.

           PERFORM SET-DT1-KEY.
           PERFORM READ-DT1-FILE.
           IF IO-BAD
              MOVE "MISSING DEALER DETAIL SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

      * BRANCH TOTALS

           IF (DT-BRNO NOT = SAVE-BR)
              MOVE 2        TO LN-FEED
              PERFORM CLASS-TOTALS
              PERFORM DEALER-TOTALS
              PERFORM JOURNAL-SUMMARY
              PERFORM SETUP-BRANCH-INFO
              MOVE DT-BRNO  TO SAVE-BR SAVE-FIRST-BR
              MOVE DT-DLNO  TO SAVE-DL SAVE-FIRST-DL
              MOVE DT-CLASS TO SAVE-CLASS
              MOVE 99       TO LN-COUNT.

      * DEALER TOTALS

           IF (DT-DL NOT = SAVE-DLNO) OR
              DT-DLBR NOT = SAVE-DLBR
                 MOVE 2        TO LN-FEED
                 PERFORM CLASS-TOTALS
                 PERFORM DEALER-TOTALS
                 MOVE DT-DLNO  TO SAVE-DL
                 MOVE DT-CLASS TO SAVE-CLASS
                 MOVE 99    TO LN-COUNT.

      * CLASS TOTALS

           IF DT-CLASS NOT = SAVE-CLASS
              MOVE 2        TO LN-FEED
              PERFORM CLASS-TOTALS
              MOVE 2        TO LN-FEED
              MOVE DT-CLASS TO SAVE-CLASS.

       
           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-WK2.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE 2   TO LN-FEED.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
              PERFORM DEALER-TOTALS
              PERFORM JOURNAL-SUMMARY
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE DTLIST-QUEUE-POS TO Q-POS.
           MOVE DTLIST-COPIES-POS TO C-POS.
           MOVE DTLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       
       COPY "LIBGB/EOINIT.CPY".
       

           IF EXT-ROUTE-BUF = "00"
              MOVE "DEALER LISTING IN PROGRESS..........." TO WR-BUF
               MOVE MESS-LIST TO WR-LIST
               PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-SYSDATE    TO H-DATE.
           MOVE EXT-CONAME-USER-ID    TO H-USERID.
           MOVE DATE1       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEG-DATE.
           MOVE DATE2       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-END-DATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF ( BEG-BR = 0 ) AND ( END-BR = 0 )
              MOVE BEG-BR    TO SV-BEG-BR
              MOVE END-BR    TO SV-END-BR
              MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           PERFORM SORT-DEALER-DETAIL-IN.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM ZERO-TABLES.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE EXT-EOBUF-DATE1 TO DATE1.
           MOVE EXT-EOBUF-DATE2 TO DATE2.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO DT-01.

       
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
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

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
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
              MOVE " " TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO DL-01.

       
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

       
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR NOT > END-BR GO TO DL-01.
           PERFORM INVALID-RANGE.
           GO TO BR-01.

       
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       
       DL-01.
           IF ( (BEG-BR = 0) AND (END-BR = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE " " TO VDUBUF
              PERFORM SCREENIO.

           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DL-01.
           MOVE VDUNUM2 TO BEG-DLNO.

       
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DL-02.
           MOVE VDUNUM2 TO END-DLNO.

           IF BEG-DLNO > END-DLNO
              MOVE "INVALID DEALER RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO DL-01.

           GO TO DC-01.

       
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DLNO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DLNO VDUNUM2.
           PERFORM SCREENIO.

       
       DC-01.
           MOVE "ENNN020DC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DC.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DC-01.
           MOVE VDUNUM0 TO BEG-CLASS.

       
       DC-02.
           MOVE "ENNN020DC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DC-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DC-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO DC-01.

           GO TO DET-01.

       
       ALL-DC.
           MOVE "SNNN020DC1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020DC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       
       DET-01.
           MOVE "E  A010DETSUM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DC-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DET-01.
           MOVE VDUBUF TO DETAIL-OR-SUMM.
           IF DETAIL-OR-SUMM NOT = "D" AND NOT = "S" GO TO DET-01.

       
       ENTER-DETAIL-TYPE.

           MOVE "E  A010DETAIL-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF (                    UP-KEY ) GO TO DET-01.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-DETAIL-TYPE.

           MOVE VDUBUF TO DETAIL-TYPE.

           IF NOT ( DETAIL-TYPE-PAYABLE     ) AND
              NOT ( DETAIL-TYPE-TRANSACTION )
              MOVE "ENTER 'P' OR 'T' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DETAIL-TYPE.

       
       DT-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO LOCAL-PRINT.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO ENTER-DETAIL-TYPE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO DATE1.
       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO DATE2.

           IF DATE2 = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000DT2." TO VDU
              MOVE DATE-YYYYMMDD TO DATE2 VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.
           GO TO ENTER-TRCD.

       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO DATE1 VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO DATE2 VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       
       ENTER-TRCD.

           MOVE "E  A000TRCD." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "16" GO TO ALL-TRCD.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO ENTER-TRCD.

           MOVE VDUBUF TO TRCD.
           GO TO ENTER-PRSEQ.
       ALL-TRCD.
           MOVE "S  A000TRCD." TO VDU.
           MOVE "~~" TO VDUBUF TRCD.
           PERFORM SCREENIO.
       
       ENTER-PRSEQ.

           MOVE "E  A000PRSEQ." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-TRCD.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO ENTER-PRSEQ.

           MOVE VDUBUF TO PRINT-SEQ.
           IF PRINT-SEQ = "D" GO TO LOCAL-PRINT.
           IF PRINT-SEQ = "A" GO TO LOCAL-PRINT.
           GO TO ENTER-PRSEQ.
       
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BR = END-BR
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

       
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO ENTER-PRSEQ
              ELSE
                 GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

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
              MOVE " " TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.
       
           MOVE "SNNN042DL1." TO VDU.
           MOVE BEG-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
       
           MOVE "SNNN042DL2." TO VDU.
           MOVE END-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
       
           MOVE "SNNN020DC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "SNNN020DC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "S  A010DETSUM." TO VDU.
           MOVE DETAIL-OR-SUMM TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010DETAIL-TYPE." TO VDU.
           MOVE DETAIL-TYPE           TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG       TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       SORT-DEALER-DETAIL-IN SECTION.
      ******************************************************************

      * READ DEALER DETAIL, WRITE WORK FILE TO SEQUENCE REPORT

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE "A" TO PROGRAM-EXIT-STATUS.
           MOVE "N" TO RECORDS-FOUND-FG.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.

           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BR
              GO TO FINISH-SORT-IN.
       
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              DL-PATH-OVERRIDE
                                              DT-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-DT1-FILE.

           MOVE BR-NO      TO DT-BRNO
                              QDT1-WBEG-BRNO
                              QDT1-WEND-BRNO.
           MOVE BEG-DLNO   TO DT-DLNO.
           MOVE DT-NUM     TO QDT1-WBEG-NUM.
           MOVE END-DLNUM  TO QDT1-WEND-NUM.

           MOVE BEG-CLASS  TO DT-CLASS 
                              QDT1-WBEG-CLASS.
           MOVE END-CLASS  TO QDT1-WEND-CLASS.

           MOVE 0          TO DT-ACCTNO DT-SEQ
                              QDT1-WBEG-ACCTNO
                              QDT1-WBEG-SEQ.

           MOVE ALL "9"    TO QDT1-WEND-CLASS
                              QDT1-WEND-ACCTNO
                              QDT1-WEND-SEQ.
 
           MOVE 19000101   TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END           TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE
           PERFORM START-DT1-FILE.

       NEXT-DT.
           PERFORM READ-DT1-FILE-NEXT.
           IF (IO-BAD) OR (DT-BRNO NOT = BR-NO)     
              GO TO FINISH-SORT-IN.

           IF DT-DLNO > END-DLNO
              GO TO FINISH-SORT-IN.

           IF DT-CLASS < BEG-CLASS OR DT-CLASS > END-CLASS
              GO TO NEXT-DT.

      * INCLUDE IF DT-DATE = 0, ITS THE BAL FWD RECORD FROM PURGE !!
           IF DT-DATE NOT = 0
              IF DT-DATE < DATE1
                 OR DT-DATE > DATE2
                    GO TO NEXT-DT.

           IF TRCD NOT = "~~"
              IF DT-TRCD NOT = TRCD
                 GO TO NEXT-DT.

           PERFORM CLEAR-WK2-REC.
           MOVE 0    TO HOLD-UNIQUE.

       FIND-WK2-RECORD.
           PERFORM SET-WK2-KEY.
           PERFORM READ-WK2-FILE.
           IF IO-FG = 0
              ADD 1 TO HOLD-UNIQUE
              GO TO FIND-WK2-RECORD.

           PERFORM CLEAR-WK2-REC.
           PERFORM SET-WK2-KEY.

           MOVE DT-DLNO       TO WK2-DLNO.
           MOVE DT-CLASS      TO WK2-CLASS.
           MOVE DT-ACCTNO     TO WK2-ACCTNO.
           MOVE DT-DATE       TO WK2-DATE.
           MOVE DT-SEQ        TO WK2-SEQ.
           MOVE DT-BRNO       TO WK2-BRNO.

           PERFORM WRITE-WK2-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO NEXT-DT.
       FINISH-SORT-IN.
           PERFORM CLOSE-WK2-FILE.
           PERFORM CLOSE-DT1-FILE.


      ********************************
       SET-WK2-KEY SECTION.
      ********************************
           MOVE DT-BRNO              TO WK2-KEY1-BRNO.
           MOVE DT-DLNO              TO WK2-KEY1-DLNO.
           MOVE DT-CLASS             TO WK2-KEY1-CLASS.
           MOVE 0                    TO WK2-KEY1-ACCTNO.
           IF PRINT-SEQ = "A"
              MOVE DT-ACCTNO         TO WK2-KEY1-ACCTNO.
           MOVE DT-DATE              TO WK2-KEY1-DATE.
           MOVE DT-SEQ               TO WK2-KEY1-SEQ.
           MOVE HOLD-UNIQUE          TO WK2-KEY1-UNIQUE.

      ********************************
       SET-DT1-KEY SECTION.
      ********************************
           MOVE WK2-BRNO             TO DT-BRNO.
           MOVE WK2-DLNO             TO DT-DLNO.
           MOVE WK2-CLASS            TO DT-CLASS.
           MOVE WK2-ACCTNO           TO DT-ACCTNO.
           MOVE WK2-DATE             TO DT-DATE.
           MOVE WK2-SEQ              TO DT-SEQ.

      ***********************************
       DETAIL-LINE-PROCESS SECTION.
      ***********************************
           IF DETAIL-TYPE-PAYABLE
              PERFORM DETAIL-LINE-PAYABLE
           ELSE
              PERFORM DETAIL-LINE-TRANS.

           IF END-DETAIL-PROCESS
              GO TO EXIT-DETAIL-LINE-PROCESS.

      *=================================================================
      *    ADD TO TOTALS AND UPDATE WK-FILE
      *=================================================================
       ADD-TO-TOTALS.

           MOVE 0   TO WK-BRANCH.
           MOVE 0   TO WK-DLNO.
           MOVE DT-CLASS TO WK-CLASS.
           MOVE 1        TO WK-SEQ.

           IF ( DT-TRCD = "BF" )
              MOVE 0           TO WK-SEQ
              ADD WS-DLRVPRIN-1     TO DLRVPRIN-1-TOT(LC-NET)
              ADD WS-DLRVPRIN-2     TO DLRVPRIN-2-TOT(LC-NET)
              ADD WS-DLRVPRIN-3     TO DLRVPRIN-3-TOT(LC-NET)
              ADD WS-DLRVPRIN-TOTAL TO DLRVPRIN-TOTAL(LC-NET)
              ADD WS-DLRVCHGS-1     TO DLRVCHGS-1-TOT(LC-NET)
              ADD WS-DLRVCHGS-2     TO DLRVCHGS-2-TOT(LC-NET)
              ADD WS-DLRVCHGS-3     TO DLRVCHGS-3-TOT(LC-NET)
              ADD WS-DLRVCHGS-TOTAL TO DLRVCHGS-TOTAL(LC-NET)
              ADD WS-DLPARVHELD     TO DLPARVHELD-TOT(LC-NET)
              ADD WS-DLPARVPAID     TO DLPARVPAID-TOT(LC-NET)
              ADD WS-PROCEEDS       TO PROCEEDS-TOT  (LC-NET)
              ADD WS-PAYMENTS       TO PAYMENTS-TOT  (LC-NET).

           MOVE DT-TRCD TO WK-TRCD.
           MOVE 0  TO NEW-FLAG.

           PERFORM READ-WK-FILE.
           IF IO-BAD
              MOVE 1        TO NEW-FLAG
              MOVE 0        TO WK-BRANCH
              MOVE 0        TO WK-DLNO
              MOVE DT-CLASS TO WK-CLASS
              MOVE 1        TO WK-SEQ
              MOVE DT-TRCD  TO WK-TRCD
              MOVE 0        TO WK-DLRVPRIN-1
                               WK-DLRVPRIN-2
                               WK-DLRVPRIN-3
                               WK-DLRVPRIN-TOTAL
                               WK-DLRVCHGS-1
                               WK-DLRVCHGS-2
                               WK-DLRVCHGS-3
                               WK-DLRVCHGS-TOTAL
                               WK-DLPARVHELD
                               WK-DLPARVPAID
                               WK-PROCEEDS
                               WK-PAYMENTS.

       
           ADD WS-DLRVPRIN-1     TO DLRVPRIN-1-TOT (LC)
                                    DLRVPRIN-1-TOT (DL)
                                    WK-DLRVPRIN-1.

           ADD WS-DLRVPRIN-2     TO DLRVPRIN-2-TOT (LC)
                                    DLRVPRIN-2-TOT (DL)
                                    WK-DLRVPRIN-2.

           ADD WS-DLRVPRIN-3     TO DLRVPRIN-3-TOT (LC)
                                    DLRVPRIN-3-TOT (DL)
                                    WK-DLRVPRIN-3.

           ADD WS-DLRVPRIN-TOTAL TO DLRVPRIN-TOTAL (LC)
                                    DLRVPRIN-TOTAL (DL)
                                    WK-DLRVPRIN-TOTAL.

           ADD WS-DLRVCHGS-1     TO DLRVCHGS-1-TOT (LC)
                                    DLRVCHGS-1-TOT (DL)
                                    WK-DLRVCHGS-1.

           ADD WS-DLRVCHGS-2     TO DLRVCHGS-2-TOT (LC)
                                    DLRVCHGS-2-TOT (DL)
                                    WK-DLRVCHGS-2.

           ADD WS-DLRVCHGS-3     TO DLRVCHGS-3-TOT (LC)
                                    DLRVCHGS-3-TOT (DL)
                                    WK-DLRVCHGS-3.

           ADD WS-DLRVCHGS-TOTAL TO DLRVCHGS-TOTAL (LC)
                                    DLRVCHGS-TOTAL (DL)
                                    WK-DLRVCHGS-TOTAL.

           ADD WS-DLPARVHELD     TO DLPARVHELD-TOT (LC)
                                    DLPARVHELD-TOT (DL)
                                    WK-DLPARVHELD.

           ADD WS-DLPARVPAID     TO DLPARVPAID-TOT (LC)
                                    DLPARVPAID-TOT (DL)
                                    WK-DLPARVPAID.

           ADD WS-PROCEEDS       TO PROCEEDS-TOT (LC)
                                    PROCEEDS-TOT (DL)
                                    WK-PROCEEDS.

           ADD WS-PAYMENTS       TO PAYMENTS-TOT (LC)
                                    PAYMENTS-TOT (DL)
                                    WK-PAYMENTS.

           IF ( WS-PROCEEDS NOT < 0 )
             ADD 1        TO DT-COUNTER(LC) DT-COUNTER(DL)
           ELSE
             SUBTRACT 1 FROM DT-COUNTER(LC) DT-COUNTER(DL).

           IF ( NEW-FLAG = 1 )
              PERFORM WRITE-WK-FILE
           ELSE
              PERFORM REWRITE-WK-FILE.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      **********************************************
       DETAIL-LINE-PAYABLE SECTION.
      **********************************************
           MOVE "C" TO DETAIL-PROCESS-STATUS.

           IF DT-ACCTNO = 0
              MOVE ALL "." TO D-P-PREFIX.

           IF DT-ACCTNO = 0
              GO TO SKIP-BORROWER-DLP.

           IF DT-ACCTNO = SAVE-ACCTNO
              GO TO SKIP-BORROWER-DLP.

           MOVE DT-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              MOVE "INVALID LOAN# " TO D-P-BW-NAME
              GO TO SKIP-BORROWER-DLP.
      *       MOVE "X" TO DETAIL-PROCESS-STATUS
      *       GO TO EXIT-DETAIL-LINE-PAYABLE.

           IF ( LN-POCD-INRESCISSION ) OR ( LN-POCD-RESCIND )
              MOVE "X" TO DETAIL-PROCESS-STATUS
              GO TO EXIT-DETAIL-LINE-PAYABLE.

       
      * SKIP DELETED ACCOUNTS ON SAME DAY AS ENTERED
       
           IF ( LN-POCD-VOID )
              IF ( LN-ENTDATE = LN-POFFDATE )
                  MOVE "X" TO DETAIL-PROCESS-STATUS
                  GO TO EXIT-DETAIL-LINE-PAYABLE.

           MOVE DT-ACCTNO     TO D-P-NUMBER.

       
       FIND-BORROWER-DLP.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "NOT ON FILE" TO BW-LNAME
              MOVE " " TO BW-FNAME.

           MOVE SPACES TO D-P-BW-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                 INTO D-P-BW-NAME.
       
       SKIP-BORROWER-DLP.
           MOVE DT-ACCTNO     TO D-P-NUMBER.

           MOVE DT-DLRVPRIN(1) TO WS-DLRVPRIN-1.
           MOVE DT-DLRVPRIN(2) TO WS-DLRVPRIN-2.
           MOVE DT-DLRVPRIN(3) TO WS-DLRVPRIN-3.
           MOVE DT-DLRVCHGS(1) TO WS-DLRVCHGS-1.
           MOVE DT-DLRVCHGS(2) TO WS-DLRVCHGS-2.
           MOVE DT-DLRVCHGS(3) TO WS-DLRVCHGS-3.

           COMPUTE WS-DLRVPRIN-TOTAL = DT-DLRVPRIN(1) +
                                       DT-DLRVPRIN(2) +
                                       DT-DLRVPRIN(3).

           COMPUTE WS-DLRVCHGS-TOTAL = DT-DLRVCHGS(1) +
                                       DT-DLRVCHGS(2) +
                                       DT-DLRVCHGS(3).

      *--->COMPUTE WS-DLPARVHELD     = DT-DLPARVHELD + DT-DLPARVPAID.
           MOVE DT-DLPARVHELD TO WS-DLPARVHELD.
           MOVE DT-DLPARVPAID TO WS-DLPARVPAID.

           MOVE DT-PROCEEDS TO WS-PROCEEDS.
           MOVE DT-PAYMENTS TO WS-PAYMENTS.

           IF NOT ( (DT-ACCTNO = SAVE-ACCTNO) AND
                    (DT-TRCD = "NW"         )
                  )
                    MOVE DT-DATE TO DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO D-P-DATE
                    MOVE DT-TRCD     TO D-P-TRCD
                    PERFORM DISPLAY-TRCD.

           MOVE DT-ACCTNO TO SAVE-ACCTNO.

           IF DT-TRCD = "NW"
              IF WS-PROCEEDS < 0
                 MOVE "ACCOUNT DELETED" TO D-P-TR-DESC
                 MOVE "DL"              TO D-P-TRCD.

           MOVE WS-DLRVPRIN-TOTAL TO D-P-DLRVPRIN.
           MOVE WS-DLRVCHGS-TOTAL TO D-P-DLRVCHGS.
           MOVE WS-DLPARVHELD     TO D-P-DLPARVHELD.
           MOVE WS-PROCEEDS       TO D-P-PROCEEDS.
           MOVE WS-PAYMENTS       TO D-P-PAYMENTS.
           MOVE DT-CKNO           TO D-P-CHECK-NO.

           IF ( DETAIL-OR-SUMM = "D" )
              PERFORM WRITE-DETAIL-LINE.

       EXIT-DETAIL-LINE-PAYABLE.
           EXIT.

      *******************************************
       DETAIL-LINE-TRANS SECTION.
      *******************************************

           MOVE "C" TO DETAIL-PROCESS-STATUS.

           IF (DT-ACCTNO = 0) OR (DT-ACCTNO = SAVE-ACCTNO)
              GO TO SKIP-BORROWER-DLT.

           MOVE DT-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              MOVE "INVALID LOAN# " TO D-T-BW-NAME
              GO TO SKIP-BORROWER-DLT.
      *       MOVE "X" TO DETAIL-PROCESS-STATUS
      *       GO TO EXIT-DETAIL-LINE-TRANS.

           IF ( LN-POCD-INRESCISSION ) OR ( LN-POCD-RESCIND )
              MOVE "X" TO DETAIL-PROCESS-STATUS
              GO TO EXIT-DETAIL-LINE-TRANS.

       
      * SKIP DELETED ACCOUNTS ON SAME DAY AS ENTERED
       

           IF ( LN-POCD = "**" )
              IF ( LN-ENTDATE = LN-POFFDATE )
                  MOVE "X" TO DETAIL-PROCESS-STATUS
                  GO TO EXIT-DETAIL-LINE-TRANS.

           MOVE DT-ACCTNO     TO D-T-NUMBER.

       
       FIND-BORROWER-DLT.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "NOT ON FILE" TO BW-LNAME
              MOVE " " TO BW-FNAME.

           MOVE SPACES TO D-T-BW-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                  INTO D-T-BW-NAME.
       
       SKIP-BORROWER-DLT.

           MOVE DT-ACCTNO     TO D-T-NUMBER.

           MOVE DT-DLRVPRIN(1) TO WS-DLRVPRIN-1.
           MOVE DT-DLRVPRIN(2) TO WS-DLRVPRIN-2.
           MOVE DT-DLRVCHGS(1) TO WS-DLRVCHGS-1.
           MOVE DT-DLRVCHGS(2) TO WS-DLRVCHGS-2.
           ADD DT-DLPARVHELD DT-DLPARVPAID GIVING WS-DLPARVHELD.
           MOVE DT-PROCEEDS    TO WS-PROCEEDS.
           MOVE DT-PAYMENTS    TO WS-PAYMENTS.

           IF NOT ( (DT-ACCTNO = SAVE-ACCTNO) AND
                    (DT-TRCD = "NW"         )
                  )
                    MOVE DT-DATE TO DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO D-T-DATE
                    MOVE DT-TRCD     TO D-T-TRCD
                    PERFORM DISPLAY-TRCD.

           MOVE DT-ACCTNO TO SAVE-ACCTNO.

           IF DT-TRCD = "NW"
              IF WS-PROCEEDS < 0
                 MOVE "ACCOUNT DELETED" TO D-T-TR-DESC
                 MOVE "DL"              TO D-T-TRCD.

           MOVE WS-DLRVPRIN-1 TO D-T-DLRVPRIN-1.
           MOVE WS-DLRVPRIN-2 TO D-T-DLRVPRIN-2.
           MOVE WS-DLRVCHGS-1 TO D-T-DLRVCHGS-1.
           MOVE WS-DLRVCHGS-2 TO D-T-DLRVCHGS-2.
           MOVE WS-DLPARVHELD TO D-T-DLPARVHELD.
           MOVE WS-PROCEEDS   TO D-T-PROCEEDS.
      *--->MOVE WS-PAYMENTS   TO D-T-PAYMENTS.
      *--->MOVE DT-CKNO       TO D-T-CKNO.

           IF ( DETAIL-OR-SUMM = "D" )
              PERFORM WRITE-DETAIL-LINE.

       EXIT-DETAIL-LINE-TRANS.
           EXIT.

      *******************************************
       JOURNAL-SUMMARY SECTION.
      *******************************************

           MOVE 99 TO LN-COUNT.

           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM ZERO-TABLES.
      *JTG MOVE "Y"           TO FIRST-TIME.     NEWPORT 990623
           MOVE 0             TO SAVE-CLASS.
           MOVE SAVE-FIRST-BR TO SAVE-BR.
           MOVE SAVE-FIRST-DL TO SAVE-DL.

           IF ( DETAIL-OR-SUMM = "S" )
              MOVE "X" TO DETAIL-OR-SUMM.

      *=================================================================
       NEXT-WK.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              PERFORM CLASS-TOTALS
              PERFORM DEALER-TOTALS
              PERFORM OPEN-WK-FILE-OUTPUT
              PERFORM OPEN-WK-FILE
              MOVE 0 TO PAGE-NUMBER
              GO TO END-JOURNAL-SUMMARY.

           IF ( WK-CLASS NOT = SAVE-CLASS )
              IF ( FIRST-TIME = "Y" )
                 MOVE "Y" TO RECORDS-FOUND-FG
      *JTG       MOVE "N" TO FIRST-TIME       NEWPORT 990623
                 GO TO SKIP-CLASS-TOTALS
              ELSE
                 PERFORM CLASS-TOTALS
                 MOVE 2 TO LN-FEED.

       
       SKIP-CLASS-TOTALS.

           IF ( DETAIL-TYPE-PAYABLE )
              PERFORM PRINT-JOURNAL-SUMMARY-DLP
           ELSE
              PERFORM PRINT-JOURNAL-SUMMARY-DLT.

       
       ADD-SUM-TOTALS.

           ADD WK-DLRVPRIN-1     TO DLRVPRIN-1-TOT (LC)
                                    DLRVPRIN-1-TOT (DL).

           ADD WK-DLRVPRIN-2     TO DLRVPRIN-2-TOT (LC)
                                    DLRVPRIN-2-TOT (DL).

           ADD WK-DLRVPRIN-3     TO DLRVPRIN-3-TOT (LC)
                                    DLRVPRIN-3-TOT (DL).

           ADD WK-DLRVPRIN-TOTAL TO DLRVPRIN-TOTAL (LC)
                                    DLRVPRIN-TOTAL (DL).

           ADD WK-DLRVCHGS-1     TO DLRVCHGS-1-TOT (LC)
                                    DLRVCHGS-1-TOT (DL).

           ADD WK-DLRVCHGS-2     TO DLRVCHGS-2-TOT (LC)
                                    DLRVCHGS-2-TOT (DL).

           ADD WK-DLRVCHGS-3     TO DLRVCHGS-3-TOT (LC)
                                    DLRVCHGS-3-TOT (DL).

           ADD WK-DLRVCHGS-TOTAL TO DLRVCHGS-TOTAL (LC)
                                    DLRVCHGS-TOTAL (DL).

           ADD WK-DLPARVHELD     TO DLPARVHELD-TOT (LC)
                                    DLPARVHELD-TOT (DL).

           ADD WK-DLPARVPAID     TO DLPARVPAID-TOT (LC)
                                    DLPARVPAID-TOT (DL).

           ADD WK-PROCEEDS    TO PROCEEDS-TOT (LC) PROCEEDS-TOT (DL).
           ADD WK-PAYMENTS    TO PAYMENTS-TOT (LC) PAYMENTS-TOT (DL).

           IF ( WK-TRCD = "BF" )
              MOVE WK-DLRVPRIN-1     TO DLRVPRIN-1-TOT (LC-NET)
              MOVE WK-DLRVPRIN-2     TO DLRVPRIN-2-TOT (LC-NET)
              MOVE WK-DLRVPRIN-3     TO DLRVPRIN-3-TOT (LC-NET)
              MOVE WK-DLRVPRIN-TOTAL TO DLRVPRIN-TOTAL (LC-NET)
              MOVE WK-DLRVCHGS-1     TO DLRVCHGS-1-TOT (LC-NET)
              MOVE WK-DLRVCHGS-2     TO DLRVCHGS-2-TOT (LC-NET)
              MOVE WK-DLRVCHGS-3     TO DLRVCHGS-3-TOT (LC-NET)
              MOVE WK-DLRVCHGS-TOTAL TO DLRVCHGS-TOTAL (LC-NET)
              MOVE WK-DLPARVHELD     TO DLPARVHELD-TOT (LC-NET)
              MOVE WK-DLPARVPAID     TO DLPARVPAID-TOT (LC-NET)
              MOVE WK-PROCEEDS       TO PROCEEDS-TOT   (LC-NET)
              ADD WK-PAYMENTS        TO PAYMENTS-TOT   (LC-NET).

           GO TO NEXT-WK.

      *=================================================================
      *    PRINT-JOURNAL-SUMMARY ----- DETAIL-LINE-PAYABLE (DLP)
      *=================================================================
       PRINT-JOURNAL-SUMMARY-DLP.

           MOVE WK-CLASS          TO SAVE-CLASS.
           MOVE DT-TRCD           TO SAVE-TRCD.
           MOVE WK-TRCD           TO D-P-TRCD DT-TRCD.
           PERFORM DISPLAY-TRCD.
           MOVE SAVE-TRCD         TO DT-TRCD.

           MOVE WK-DLRVPRIN-TOTAL TO D-P-DLRVPRIN.
           MOVE WK-DLRVCHGS-TOTAL TO D-P-DLRVCHGS.
           MOVE WK-DLPARVHELD     TO D-P-DLPARVHELD.
           MOVE WK-PROCEEDS       TO D-P-PROCEEDS.
           MOVE WK-PAYMENTS       TO D-P-PAYMENTS.

           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
      *    PRINT-JOURNAL-SUMMARY ----- DETAIL-LINE-TRANSACTION (DLT)
      *=================================================================
       PRINT-JOURNAL-SUMMARY-DLT.

           MOVE WK-CLASS          TO SAVE-CLASS.
           MOVE DT-TRCD           TO SAVE-TRCD.
           MOVE WK-TRCD           TO D-T-TRCD DT-TRCD.
           PERFORM DISPLAY-TRCD.
           MOVE SAVE-TRCD         TO DT-TRCD.

           MOVE WK-DLRVPRIN-1 TO D-T-DLRVPRIN-1.
           MOVE WK-DLRVPRIN-2 TO D-T-DLRVPRIN-2.
           MOVE WK-DLRVCHGS-1 TO D-T-DLRVCHGS-1.
           MOVE WK-DLRVCHGS-2 TO D-T-DLRVCHGS-2.
           MOVE WK-DLPARVHELD TO D-T-DLPARVHELD.
           MOVE WK-PROCEEDS   TO D-T-PROCEEDS.
      *--->MOVE WK-PAYMENTS   TO D-T-PAYMENTS.

           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       END-JOURNAL-SUMMARY.

           IF ( DETAIL-OR-SUMM = "X" )
              MOVE "S" TO DETAIL-OR-SUMM.

       EXIT-JOURNAL-SUMMARY.
           EXIT.

      ******************************************************************
      *
      *    WRITE DETAIL LINE
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
       
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.

           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE " "    TO DETAIL-LINE.

      ******************************************************************
      *
      *    H E A D - P A G E
      *
      ******************************************************************
       HEAD-PAGE SECTION.
       
       HEAD-ROUTINE.

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H-TIME.

           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEAD3       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
       BRANCH-HEADER.
           MOVE HOLD-BR-NO  TO HD-BRNO.
      *--->MOVE SAVE-BR     TO HD-BRNO.

      *--->PERFORM OPEN-BR-FILE.
      *--->PERFORM READ-BR-FILE.

      *--->IF IO-BAD
      *--->  MOVE "INVALID BRANCH" TO HD-BR-INFO
      *--->ELSE

           MOVE HOLD-BR-NAME TO HD-BR-INFO.

      *--->PERFORM CLOSE-BR-FILE.

       
       DEALER-HEADER.
       
      *    THIS CHECKS TO SEE IF THERE WAS A CHANGE IN BRANCH #'S,
      *    IF SO, THE OLD DL-PATH (PATH1) IS INSERTED AND THE NEW PATH
      *    IS SAVED IN HOLD-DL-PATH2.
       
           IF ( HOLD-BR-NO NOT = BR-NO )
              MOVE DL-PATH                 TO HOLD-DL-PATH2
              MOVE HOLD-DL-PATH1           TO DL-PATH
              MOVE "Y"                     TO DL-PATH-OVERRIDE.

       

           MOVE SAVE-BR TO DL-BRNO.
           MOVE SAVE-DL TO HD-DLNO DL-DLNO.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           IF IO-BAD
              MOVE "INVALID DEALER" TO HD-DL-INFO
           ELSE
              MOVE DL-NAME          TO HD-DL-INFO.

           PERFORM CLOSE-DL1-FILE.

       
      *    RESETTING THE NEW DL-PATH BACK.
       
           IF ( HOLD-BR-NO NOT = BR-NO )
              MOVE HOLD-DL-PATH2           TO DL-PATH
              MOVE "B"                     TO DL-PATH-OVERRIDE.
       

           MOVE HEAD4     TO PR-REC.
           MOVE 1         TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEAD5     TO PR-REC.
           MOVE 1         TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HOLD-BR-ADR1   TO HD-BR-INFO.
           MOVE DL-ADR1        TO HD-DL-INFO.
           MOVE HEAD5          TO PR-REC.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HOLD-BR-ADR2   TO HD-BR-INFO.
           MOVE DL-ADR2        TO HD-DL-INFO.
           MOVE HEAD5          TO PR-REC.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HOLD-BR-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "      " BY ", ++ +".
           INSPECT MESS REPLACING FIRST "++" BY HOLD-BR-STATE.
           INSPECT MESS REPLACING FIRST "+         " BY HOLD-BR-ZIP.
           MOVE MESS TO HD-BR-INFO.

           MOVE DL-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "      " BY ", ++ +".
           INSPECT MESS REPLACING FIRST "++" BY DL-STATE.
           INSPECT MESS REPLACING FIRST "+         " BY DL-ZIP.
           MOVE MESS TO HD-DL-INFO.
           MOVE HEAD5     TO PR-REC.
           MOVE 1         TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
       DETAIL-HEADER.

           IF ( DETAIL-TYPE-PAYABLE )
              MOVE HEAD-PAYABLE-DT1     TO PR-REC
           ELSE
              MOVE HEAD-TRANSACTION-DT1 TO PR-REC.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           IF ( DETAIL-TYPE-PAYABLE )
              MOVE HEAD-PAYABLE-DT2     TO PR-REC
           ELSE
              MOVE HEAD-TRANSACTION-DT2 TO PR-REC.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD-DT3 TO PR-REC.
           MOVE 1        TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 15 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *    C L A S S - T O T A L S
      *
      ******************************************************************
       CLASS-TOTALS SECTION.

           MOVE SAVE-CLASS TO LC-CODE.

           IF LC-CODE NOT = 0
              MOVE LC-CODE TO S-CLASS
              PERFORM OPEN-LC1-FILE
              PERFORM READ-LC1-FILE
              IF IO-BAD
                 MOVE "INVALID CLASS" TO D-DESC
              ELSE
                 MOVE LC-DESC         TO D-DESC.

           IF ( DETAIL-TYPE-PAYABLE )
              PERFORM CLASS-TOTALS-PAYABLE
           ELSE
              PERFORM CLASS-TOTALS-TRANSACTION.

      *=================================================================
       END-CLASS-TOTALS.

           MOVE 0 TO
                DLRVPRIN-1-TOT(LC)       DLRVPRIN-1-TOT(LC-NET)
                DLRVPRIN-2-TOT(LC)       DLRVPRIN-2-TOT(LC-NET)
                DLRVPRIN-3-TOT(LC)       DLRVPRIN-3-TOT(LC-NET)
                DLRVPRIN-TOTAL(LC)       DLRVPRIN-TOTAL(LC-NET)
                DLRVCHGS-1-TOT(LC)       DLRVCHGS-1-TOT(LC-NET)
                DLRVCHGS-2-TOT(LC)       DLRVCHGS-2-TOT(LC-NET)
                DLRVCHGS-3-TOT(LC)       DLRVCHGS-3-TOT(LC-NET)
                DLRVCHGS-TOTAL(LC)       DLRVCHGS-TOTAL(LC-NET)
                DLPARVHELD-TOT(LC)       DLPARVHELD-TOT(LC-NET)
                DLPARVPAID-TOT(LC)       DLPARVPAID-TOT(LC-NET)
                PROCEEDS-TOT  (LC)       PROCEEDS-TOT  (LC-NET)
                PAYMENTS-TOT  (LC)       PAYMENTS-TOT  (LC-NET)
                DT-COUNTER    (LC)       SAVE-ACCTNO.

      *=================================================================
       END-CLASS-TOTALS.

           MOVE " " TO DETAIL-LINE.

       EXIT-CLASS-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    C L A S S - T O T A L S - P A Y A B L E
      *
      ******************************************************************
       CLASS-TOTALS-PAYABLE SECTION.

           MOVE DLRVPRIN-TOTAL (LC) TO D-P-DLRVPRIN.
           MOVE DLRVCHGS-TOTAL (LC) TO D-P-DLRVCHGS.
           MOVE DLPARVHELD-TOT (LC) TO D-P-DLPARVHELD.
           MOVE PROCEEDS-TOT   (LC) TO D-P-PROCEEDS.
           MOVE PAYMENTS-TOT   (LC) TO D-P-PAYMENTS.

           MOVE 2 TO LN-FEED.

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

           IF ( DLRVPRIN-1-TOT(LC-NET) = 0 ) AND
              ( DLRVPRIN-2-TOT(LC-NET) = 0 ) AND
              ( DLRVPRIN-3-TOT(LC-NET) = 0 ) AND
              ( DLRVCHGS-1-TOT(LC-NET) = 0 ) AND
              ( DLRVCHGS-2-TOT(LC-NET) = 0 ) AND
              ( DLRVCHGS-3-TOT(LC-NET) = 0 ) AND
              ( DLPARVHELD-TOT(LC-NET) = 0 ) AND
              ( PROCEEDS-TOT  (LC-NET) = 0 ) AND
              ( PAYMENTS-TOT  (LC-NET) = 0 )
              GO TO EXIT-CLASS-TOTALS-PAYABLE.

           MOVE "NET CHANGE...." TO D-P-TR-DESC.

           SUBTRACT DLRVPRIN-TOTAL(LC-NET) FROM DLRVPRIN-TOTAL(LC)
                                         GIVING D-P-DLRVPRIN.

           SUBTRACT DLRVCHGS-TOTAL(LC-NET) FROM DLRVCHGS-TOTAL(LC)
                                         GIVING D-P-DLRVCHGS.

           SUBTRACT DLPARVHELD-TOT(LC-NET) FROM DLPARVHELD-TOT(LC)
                                         GIVING D-P-DLPARVHELD.

           SUBTRACT PROCEEDS-TOT  (LC-NET) FROM PROCEEDS-TOT(LC)
                                         GIVING D-P-PROCEEDS.

           SUBTRACT PAYMENTS-TOT  (LC-NET) FROM PAYMENTS-TOT(LC)
                                         GIVING D-P-PAYMENTS.

           ADD DLRVPRIN-1-TOT(LC-NET) TO DLRVPRIN-1-TOT(DL-NET).
           ADD DLRVPRIN-2-TOT(LC-NET) TO DLRVPRIN-2-TOT(DL-NET).
           ADD DLRVPRIN-3-TOT(LC-NET) TO DLRVPRIN-3-TOT(DL-NET).
           ADD DLRVPRIN-TOTAL(LC-NET) TO DLRVPRIN-TOTAL(DL-NET).
           ADD DLRVCHGS-1-TOT(LC-NET) TO DLRVCHGS-1-TOT(DL-NET).
           ADD DLRVCHGS-2-TOT(LC-NET) TO DLRVCHGS-2-TOT(DL-NET).
           ADD DLRVCHGS-3-TOT(LC-NET) TO DLRVCHGS-3-TOT(DL-NET).
           ADD DLRVCHGS-TOTAL(LC-NET) TO DLRVCHGS-TOTAL(DL-NET).
           ADD DLPARVHELD-TOT(LC-NET) TO DLPARVHELD-TOT(DL-NET).
           ADD DLPARVPAID-TOT(LC-NET) TO DLPARVPAID-TOT(DL-NET).
           ADD PROCEEDS-TOT  (LC-NET) TO PROCEEDS-TOT(DL-NET).
           ADD PAYMENTS-TOT  (LC-NET) TO PAYMENTS-TOT(DL-NET).

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

       EXIT-CLASS-TOTALS-PAYABLE.
           EXIT.

      ******************************************************************
      *
      *    C L A S S - T O T A L S - T R A N S A C T I O N
      *
      ******************************************************************
       CLASS-TOTALS-TRANSACTION SECTION.

           MOVE DLRVPRIN-1-TOT (LC) TO D-T-DLRVPRIN-1.
           MOVE DLRVPRIN-2-TOT (LC) TO D-T-DLRVPRIN-2.
           MOVE DLRVCHGS-1-TOT (LC) TO D-T-DLRVCHGS-1.
           MOVE DLRVCHGS-2-TOT (LC) TO D-T-DLRVCHGS-2.
           MOVE DLPARVHELD-TOT (LC) TO D-T-DLPARVHELD.
           MOVE PROCEEDS-TOT   (LC) TO D-T-PROCEEDS.
      *--->MOVE PAYMENTS-TOT   (LC) TO D-T-PAYMENTS.

           MOVE 2 TO LN-FEED.

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

           IF ( DLRVPRIN-1-TOT(LC-NET) = 0 ) AND
              ( DLRVPRIN-2-TOT(LC-NET) = 0 ) AND
              ( DLRVCHGS-1-TOT(LC-NET) = 0 ) AND
              ( DLRVCHGS-2-TOT(LC-NET) = 0 ) AND
              ( DLPARVHELD-TOT(LC-NET) = 0 ) AND
              ( PROCEEDS-TOT  (LC-NET) = 0 ) AND
              ( PAYMENTS-TOT  (LC-NET) = 0 )
              GO TO EXIT-CLASS-TOTALS-TRANS.

           MOVE "NET CHANGE...." TO D-T-TR-DESC.

           SUBTRACT DLRVPRIN-1-TOT(LC-NET) FROM DLRVPRIN-1-TOT(LC)
                                         GIVING D-T-DLRVPRIN-1.

           SUBTRACT DLRVPRIN-2-TOT(LC-NET) FROM DLRVPRIN-2-TOT(LC)
                                         GIVING D-T-DLRVPRIN-2.

           SUBTRACT DLRVCHGS-1-TOT(LC-NET) FROM DLRVCHGS-1-TOT(LC)
                                         GIVING D-T-DLRVCHGS-1.

           SUBTRACT DLRVCHGS-2-TOT(LC-NET) FROM DLRVCHGS-2-TOT(LC)
                                         GIVING D-T-DLRVCHGS-2.

           SUBTRACT DLPARVHELD-TOT(LC-NET) FROM DLPARVHELD-TOT(LC)
                                         GIVING D-T-DLPARVHELD.

           SUBTRACT PROCEEDS-TOT  (LC-NET) FROM PROCEEDS-TOT(LC)
                                         GIVING D-T-PROCEEDS.

      *--->SUBTRACT PAYMENTS-TOT  (LC-NET) FROM PAYMENTS-TOT(LC)
      *--->                              GIVING D-T-PAYMENTS.

           ADD DLRVPRIN-1-TOT(LC-NET) TO DLRVPRIN-1-TOT(DL-NET).
           ADD DLRVPRIN-2-TOT(LC-NET) TO DLRVPRIN-2-TOT(DL-NET).
           ADD DLRVCHGS-1-TOT(LC-NET) TO DLRVCHGS-1-TOT(DL-NET).
           ADD DLRVCHGS-2-TOT(LC-NET) TO DLRVCHGS-2-TOT(DL-NET).
           ADD DLPARVHELD-TOT(LC-NET) TO DLPARVHELD-TOT(DL-NET).
           ADD PROCEEDS-TOT  (LC-NET) TO PROCEEDS-TOT  (DL-NET).
           ADD PAYMENTS-TOT  (LC-NET) TO PAYMENTS-TOT  (DL-NET).

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

       EXIT-CLASS-TOTALS-TRANS.
           EXIT.

      ******************************************************************
      *
      *    D E A L E R - T O T A L S
      *
      ******************************************************************
       DEALER-TOTALS SECTION.

           MOVE "DEALER TOTALS:" TO S-DESC.

           IF ( DETAIL-TYPE-PAYABLE )
              PERFORM DEALER-TOTALS-PAYABLE
           ELSE
              PERFORM DEALER-TOTALS-TRANSACTION.

       
           MOVE 0 TO
                DLRVPRIN-1-TOT(DL)       DLRVPRIN-1-TOT(DL-NET)
                DLRVPRIN-2-TOT(DL)       DLRVPRIN-2-TOT(DL-NET)
                DLRVPRIN-3-TOT(DL)       DLRVPRIN-3-TOT(DL-NET)
                DLRVPRIN-TOTAL(DL)       DLRVPRIN-TOTAL(DL-NET)
                DLRVCHGS-1-TOT(DL)       DLRVCHGS-1-TOT(DL-NET)
                DLRVCHGS-2-TOT(DL)       DLRVCHGS-2-TOT(DL-NET)
                DLRVCHGS-3-TOT(DL)       DLRVCHGS-3-TOT(DL-NET)
                DLRVCHGS-TOTAL(DL)       DLRVCHGS-TOTAL(DL-NET)
                DLPARVHELD-TOT(DL)       DLPARVHELD-TOT(DL-NET)
                DLPARVPAID-TOT(DL)       DLPARVPAID-TOT(DL-NET)
                PROCEEDS-TOT  (DL)       PROCEEDS-TOT  (DL-NET)
                PAYMENTS-TOT  (DL)       PAYMENTS-TOT  (DL-NET)
                DT-COUNTER    (DL).

           MOVE 2 TO DL.

      *=================================================================
       END-DEALER-TOTALS.

           MOVE " " TO DETAIL-LINE.

       EXIT-DEALER-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    D E A L E R - T O T A L S - P A Y A B L E
      *
      ******************************************************************
       DEALER-TOTALS-PAYABLE SECTION.

           MOVE DLRVPRIN-TOTAL (DL) TO D-P-DLRVPRIN.
           MOVE DLRVCHGS-TOTAL (DL) TO D-P-DLRVCHGS.
           MOVE DLPARVHELD-TOT (DL) TO D-P-DLPARVHELD.
           MOVE PROCEEDS-TOT   (DL) TO D-P-PROCEEDS.
           MOVE PAYMENTS-TOT   (DL) TO D-P-PAYMENTS.
      *--->MOVE DT-COUNTER     (DL) TO D-P-COUNTER.

           MOVE 2 TO LN-FEED.

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

           IF ( DLRVPRIN-1-TOT(DL-NET) = 0 ) AND
              ( DLRVPRIN-2-TOT(DL-NET) = 0 ) AND
              ( DLRVPRIN-3-TOT(DL-NET) = 0 ) AND
              ( DLRVCHGS-1-TOT(DL-NET) = 0 ) AND
              ( DLRVCHGS-2-TOT(DL-NET) = 0 ) AND
              ( DLRVCHGS-3-TOT(DL-NET) = 0 ) AND
              ( DLPARVHELD-TOT(DL-NET) = 0 ) AND
              ( PROCEEDS-TOT  (DL-NET) = 0 ) AND
              ( PAYMENTS-TOT  (DL-NET) = 0 )
              GO TO EXIT-DEALER-TOTALS-PAYABLE.

           MOVE "NET CHANGE...." TO D-P-TR-DESC.

           SUBTRACT DLRVPRIN-TOTAL(DL-NET) FROM DLRVPRIN-TOTAL(DL)
                                         GIVING D-P-DLRVPRIN.

           SUBTRACT DLRVCHGS-TOTAL(DL-NET) FROM DLRVCHGS-TOTAL(DL)
                                         GIVING D-P-DLRVCHGS.

           SUBTRACT DLPARVHELD-TOT(DL-NET) FROM DLPARVHELD-TOT(DL)
                                         GIVING D-P-DLPARVHELD.

           SUBTRACT PROCEEDS-TOT  (DL-NET) FROM PROCEEDS-TOT(DL)
                                         GIVING D-P-PROCEEDS.

           SUBTRACT PAYMENTS-TOT  (DL-NET) FROM PAYMENTS-TOT(DL)
                                         GIVING D-P-PAYMENTS.

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

       EXIT-DEALER-TOTALS-PAYABLE.
           EXIT.

      ******************************************************************
      *
      *    D E A L E R - T O T A L S - T R A N S A C T I O N
      *
      ******************************************************************
       DEALER-TOTALS-TRANSACTION SECTION.

           MOVE DLRVPRIN-1-TOT (DL) TO D-T-DLRVPRIN-1.
           MOVE DLRVPRIN-2-TOT (DL) TO D-T-DLRVPRIN-2.
           MOVE DLRVCHGS-1-TOT (DL) TO D-T-DLRVCHGS-1.
           MOVE DLRVCHGS-2-TOT (DL) TO D-T-DLRVCHGS-2.
           MOVE DLPARVHELD-TOT (DL) TO D-T-DLPARVHELD.
           MOVE PROCEEDS-TOT   (DL) TO D-T-PROCEEDS.
      *--->MOVE PAYMENTS-TOT   (DL) TO D-T-PAYMENTS.
      *--->MOVE DT-COUNTER     (DL) TO D-T-COUNTER.

           MOVE 2 TO LN-FEED.

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

           IF ( DLRVPRIN-1-TOT(DL-NET) = 0 ) AND
              ( DLRVPRIN-2-TOT(DL-NET) = 0 ) AND
              ( DLRVCHGS-1-TOT(DL-NET) = 0 ) AND
              ( DLRVCHGS-2-TOT(DL-NET) = 0 ) AND
              ( DLPARVHELD-TOT(DL-NET) = 0 ) AND
              ( PROCEEDS-TOT  (DL-NET) = 0 ) AND
              ( PAYMENTS-TOT  (DL-NET) = 0 )
              GO TO EXIT-DEALER-TOTALS-TRANS.

           MOVE "NET CHANGE...." TO D-T-TR-DESC.

           SUBTRACT DLRVPRIN-1-TOT(DL-NET) FROM DLRVPRIN-1-TOT(DL)
                                         GIVING D-T-DLRVPRIN-1.

           SUBTRACT DLRVPRIN-2-TOT(DL-NET) FROM DLRVPRIN-2-TOT(DL)
                                         GIVING D-T-DLRVPRIN-2.

           SUBTRACT DLRVCHGS-1-TOT(DL-NET) FROM DLRVCHGS-1-TOT(DL)
                                         GIVING D-T-DLRVCHGS-1.

           SUBTRACT DLRVCHGS-2-TOT(DL-NET) FROM DLRVCHGS-2-TOT(DL)
                                         GIVING D-T-DLRVCHGS-2.

           SUBTRACT DLPARVHELD-TOT(DL-NET) FROM DLPARVHELD-TOT(DL)
                                         GIVING D-T-DLPARVHELD.

           SUBTRACT PROCEEDS-TOT  (DL-NET) FROM PROCEEDS-TOT(DL)
                                         GIVING D-T-PROCEEDS.

      *--->SUBTRACT PAYMENTS-TOT  (DL-NET) FROM PAYMENTS-TOT(DL)
      *--->                              GIVING D-T-PAYMENTS.

           IF ( DETAIL-OR-SUMM = "D" ) OR ( DETAIL-OR-SUMM = "X" )
              PERFORM WRITE-DETAIL-LINE.

       EXIT-DEALER-TOTALS-TRANS.
           EXIT.

      ******************************************************************
      *
      *    D I S P L A Y - T R C D
      *
       
      *    DISPLAY THE DESCRIPTION OF THE TRANSACTION CODE.
      ******************************************************************
       DISPLAY-TRCD SECTION.

           IF DT-TRCD = "NW"
              IF DETAIL-TYPE-PAYABLE
                 MOVE "NEW ACCOUNT" TO D-P-TR-DESC
              ELSE
                 MOVE "NEW ACCOUNT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "BF" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "BALANCE FORWARD" TO D-P-TR-DESC
              ELSE
                 MOVE "BALANCE FORWARD" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * DEALER RESERVES:
       
           IF ( DT-TRCD = "PR" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RESERVE PRIN PAYMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "RESERVE PRIN PAYMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PC" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RESERVE CHGS PAYMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "RESERVE CHGS PAYMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PP" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RESERVE PARTIC PAYMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "RESERVE PARTIC PAYMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "DR" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "PAYMENT FROM RESERVE" TO D-P-TR-DESC
              ELSE
                 MOVE "PAYMENT FROM RESERVE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "DC" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "PAYMENT CHARGES RSV" TO D-P-TR-DESC
              ELSE
                 MOVE "PAYMENT CHARGES RSV" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "DP" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "PMNT PARTIC.RESERVE " TO D-P-TR-DESC
              ELSE
                 MOVE "PMNT PARTIC.RESERVE " TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RR" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RECEIPT PRINCIPAL RSV" TO D-P-TR-DESC
              ELSE
                 MOVE "RECEIPT PRINCIPAL RSV" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RC" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RECEIPT CHARGES RSV" TO D-P-TR-DESC
              ELSE
                 MOVE "RECEIPT CHARGES RSV" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RP" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RECEIPT PARTIC. RSV" TO D-P-TR-DESC
              ELSE
                 MOVE "RECEIPT PARTIC. RSV" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * REBATES:
       
           IF ( DT-TRCD = "RI" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE INTEREST" TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE INTEREST" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RS" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE SERVICE CHARGE" TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE SERVICE CHARGE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RC" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE C/LIFE INSURANCE" TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE C/LIFE INSURANCE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RA" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE A&H INSURANCE" TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE A&H INSURANCE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RP" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE PP INSURANCE" TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE PP INSURANCE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "R1" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE OTHER 1 INSURANCE" TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE OTHER 1 INSURANCE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "R2" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE OT2 INS." TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE OT2 INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "R3" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REBATE OT3 INS." TO D-P-TR-DESC
              ELSE
                 MOVE "REBATE OT3 INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * P&L TRANSACTIONS:
       
           IF ( DT-TRCD = "PL" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "P&L ACTIVE" TO D-P-TR-DESC
              ELSE
                 MOVE "P&L ACTIVE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PI" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "P&L INACTIVE" TO D-P-TR-DESC
              ELSE
                 MOVE "P&L INACTIVE" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PW" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "P&L WRITE OFF" TO D-P-TR-DESC
              ELSE
                 MOVE "P&L WRITE OFF" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * PAYOFFS/RENEWALS:
       
           IF ( DT-TRCD = "PO" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "PAYOFF" TO D-P-TR-DESC
              ELSE
                 MOVE "PAYOFF" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PB" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RENEWAL WITH PROCEEDS" TO D-P-TR-DESC
              ELSE
                 MOVE "RENEWAL WITH PROCEEDS" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RN" OR "RB" OR "RO")
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "RENEWAL BALANCE ONLY" TO D-P-TR-DESC
              ELSE
                 MOVE "RENEWAL BALANCE ONLY" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "SC" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "SALES.FIN CONVERSION" TO D-P-TR-DESC
              ELSE
                 MOVE "SALES.FIN CONVERSION" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * INSURANCE CANCELS:
       
           IF ( DT-TRCD = "CC" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "CANCEL CL INS." TO D-P-TR-DESC
              ELSE
                 MOVE "CANCEL CL INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "CA" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "CANCEL AH INS." TO D-P-TR-DESC
              ELSE
                 MOVE "CANCEL AH INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "CP" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "CANCEL PP INS." TO D-P-TR-DESC
              ELSE
                 MOVE "CANCEL PP INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "C1" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "CANCEL OT1 INS." TO D-P-TR-DESC
              ELSE
                 MOVE "CANCEL OT1 INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "C2" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "CANCEL OT2 INS." TO D-P-TR-DESC
              ELSE
                 MOVE "CANCEL OT2 INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "C3" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "CANCEL OT3 INS." TO D-P-TR-DESC
              ELSE
                 MOVE "CANCEL OT3 INS." TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "DF" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "DEFERMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "DEFERMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * WAIVER TRANSACTIONS:
       
           IF ( DT-TRCD = "WL" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "WAIVE LATE CHG" TO D-P-TR-DESC
              ELSE
                 MOVE "WAIVE LATE CHG" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "WI" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "WAIVE INTEREST" TO D-P-TR-DESC
              ELSE
                 MOVE "WAIVE INTEREST" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "WO" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "WAIVE OTHER" TO D-P-TR-DESC
              ELSE
                 MOVE "WAIVE OTHER" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * PAYMENTS NON RESERVE:
       
           IF ( DT-TRCD = "PY" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "NORMAL PAYMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "NORMAL PAYMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PE" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "PAYMENT EXCEPTION" TO D-P-TR-DESC
              ELSE
                 MOVE "PAYMENT EXCEPTION" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "IN" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "INSURANCE LOSS" TO D-P-TR-DESC
              ELSE
                 MOVE "INSURANCE LOSS" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "SS" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REPO SOLD" TO D-P-TR-DESC
              ELSE
                 MOVE "REPO SOLD" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PA" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "ALLOTMENT PAYMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "ALLOTMENT PAYMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "AH" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "A&H PAYMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "A&H PAYMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PN" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "NON CASH PAYMENT" TO D-P-TR-DESC
              ELSE
                 MOVE "NON CASH PAYMENT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "PZ" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "NO CASH/NO RECDEL PMT" TO D-P-TR-DESC
              ELSE
                 MOVE "NO CASH/NO RECDEL PMT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * ASSESMENTS:
       
           IF ( DT-TRCD = "OT" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "ASSESS OTH. CHG" TO D-P-TR-DESC
              ELSE
                 MOVE "ASSESS OTH. CHG" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "AI" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "ASSESS INT. CHG" TO D-P-TR-DESC
              ELSE
                 MOVE "ASSESS INT. CHG" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "AL" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "ASSESS LATE CHG" TO D-P-TR-DESC
              ELSE
                 MOVE "ASSESS LATE CHG" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       
      * MISC:
       
           IF ( DT-TRCD = "JD" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "JUDGEMENT FULL" TO D-P-TR-DESC
              ELSE
                 MOVE "JUDGEMENT FULL" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "AC" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "ACCOUNT ACCELERATED" TO D-P-TR-DESC
              ELSE
                 MOVE "ACCOUNT ACCELERATED" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "IR" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "INTEREST CHANGE/REDUC" TO D-P-TR-DESC
              ELSE
                 MOVE "INTEREST CHANGE/REDUC" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "AP" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "APPLY TO PRINCIPAL" TO D-P-TR-DESC
              ELSE
                 MOVE "APPLY TO PRINCIPAL" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "RV" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "REVERSAL (F6)" TO D-P-TR-DESC
              ELSE
                 MOVE "REVERSAL (F6)" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "TX" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "TFR PURCHASED TO SERV" TO D-P-TR-DESC
              ELSE
                 MOVE "TFR PURCHASED TO SERV" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "TP" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "TFR SERV TO PURCHASED" TO D-P-TR-DESC
              ELSE
                 MOVE "TFR SERV TO PURCHASED" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "SJ" )
              IF ( DETAIL-TYPE-PAYABLE )
                 MOVE "JUDGEMENT SPLIT" TO D-P-TR-DESC
              ELSE
                 MOVE "JUDGEMENT SPLIT" TO D-T-TR-DESC
              END-IF
              GO TO EXIT-DISPLAY-TRCD.

       EXIT-DISPLAY-TRCD.
           EXIT.

      ******************************************
       SETUP-BRANCH-INFO SECTION.
      ******************************************
           IF RECORDS-FOUND
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-BW1-FILE
              PERFORM CLOSE-DT1-FILE
              PERFORM CLOSE-DL1-FILE.

           PERFORM OPEN-BR-FILE.
           MOVE WK2-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-BR-FILE.

           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              DL-PATH-OVERRIDE
                                              DT-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-DT1-FILE.
           PERFORM OPEN-DL1-FILE.

           MOVE BR-NO    TO HOLD-BR-NO.
           MOVE BR-NAME  TO HOLD-BR-NAME.
           MOVE BR-ADR1  TO HOLD-BR-ADR1.
           MOVE BR-ADR2  TO HOLD-BR-ADR2.
           MOVE BR-CITY  TO HOLD-BR-CITY.
           MOVE BR-STATE TO HOLD-BR-STATE.
           MOVE BR-ZIP   TO HOLD-BR-ZIP.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
       
           MOVE 4 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH" TO RANGE-LINE
              MOVE BEG-BR TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH" TO RANGE-LINE
              MOVE END-BR TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGINNING DEALER :" TO RANGE-LINE.
           MOVE BEG-DLNO             TO W-DLNO.
           MOVE W-DLNUM              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ENDING           :" TO RANGE-LINE.
           MOVE END-DLNO             TO W-DLNO.
           MOVE W-DLNUM              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGINNING CLASS   :" TO RANGE-LINE.
           MOVE BEG-CLASS             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ENDING           :" TO RANGE-LINE.
           MOVE END-CLASS             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
      ******************************************************************
      *
      *    C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO DTLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DTLIST-SYSDATE.
           DISPLAY DTLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DTLIST-FORM-FIELDS TO WR-BUF.
           MOVE DTLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE DTLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE DTLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/DTLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-WK2-REC.
           MOVE SPACES TO WK2-REC.
           MOVE 0 TO WK2-KEY1-BRNO.
           MOVE 0 TO WK2-KEY1-DLNO.
           MOVE 0 TO WK2-KEY1-CLASS.
           MOVE 0 TO WK2-KEY1-DATE.
           MOVE 0 TO WK2-KEY1-UNIQUE.
           MOVE 0 TO WK2-DLNO.
           MOVE 0 TO WK2-CLASS.
           MOVE 0 TO WK2-ACCTNO.
           MOVE 0 TO WK2-DATE.
           MOVE 0 TO WK2-SEQ.
           MOVE 0 TO WK2-BRNO.
       ZERO-TABLES.
           MOVE 0 TO DLRVPRIN-1-TOT(LC)
                     DLRVPRIN-2-TOT(LC)
                     DLRVPRIN-3-TOT(LC)
                     DLRVPRIN-TOTAL(LC)
                     DLRVCHGS-1-TOT(LC)
                     DLRVCHGS-2-TOT(LC)
                     DLRVCHGS-3-TOT(LC)
                     DLRVCHGS-TOTAL(LC)
                     DLPARVHELD-TOT(LC)
                     DLPARVPAID-TOT(LC)
                     PROCEEDS-TOT  (LC)
                     PAYMENTS-TOT  (LC)
                     DT-COUNTER    (LC).
           MOVE TOTALS-TYPE (LC) TO TOTALS-TYPE (DL)
                                    TOTALS-TYPE (LC-NET)
                                    TOTALS-TYPE (DL-NET).
       CLEAR-WK-REC.
           MOVE " " TO WK-TRCD DT-TRCD.
           MOVE 0   TO WK-BRANCH       WK-DLNO
                       WK-CLASS        WK-SEQ
                       WK-DLRVPRIN-1   WK-DLRVPRIN-2
                       WK-DLRVPRIN-3   WK-DLRVPRIN-TOTAL
                       WK-DLRVCHGS-1   WK-DLRVCHGS-2
                       WK-DLRVCHGS-3   WK-DLRVCHGS-TOTAL
                       WK-DLPARVHELD   WK-DLPARVPAID
                       WK-PROCEEDS     WK-PAYMENTS.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPDT1RN.CPY".
       
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( PROGRAM-EXIT-AFTER-RUN )
              IF ( EXT-ROUTE-BUF = "00" )
                 PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              IF EXT-ROUTE-BUF = "00" OR "70"
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       
       EXIT-PROG.
           IF ( SV-BEG-BR NOT = 9999 )
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPLSMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DTLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
