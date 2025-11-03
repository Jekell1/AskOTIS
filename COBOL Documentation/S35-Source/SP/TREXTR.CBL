       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TREXTR.
       DATE-WRITTEN.    121801.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  DATA WAREHOUSE TRANSACTION EXTRACTION
      *
      * CREATES THE FOLLOWING OUTPUT EXTRACT FILES IN /USR/EXTRACT;
      *
      *TRANS_BULK,       RECORD LENGTH =  802, BRANCH (BULK) TRANSFERS (TRB)
      *TRANS_CHECKS,     RECORD LENGTH =  371, CASH/CHECK (TRC)
      *TRANS_DEPOSIT,    RECORD LENGTH =  193, DEPOSIT (TRD)
      *TRANS_CLASSXFER,  RECORD LENGTH =  801, CLASS TRANSFER (TRJ)
      *TRANS_CONVERSION, RECORD LENGTH =  801, CONVERSION (TRK)
      *TRANS_LOAN,       RECORD LENGTH = 1064, DIRECT LOANS (TRL)
      *TRANS_PMT,        RECORD LENGTH =  936, PAYMENTS (TRP)
      *TRANS_SALESFIN,   RECORD LENGTH = 1143, SALES FINANCE (TRS)
      *TRANS_SERVICE,    RECORD LENGTH = 1079, TRANSFER TO SERVICE (TRX)
      *TRANS_DRAWER,     RECORD LENGTH =  697, CASH DRAWER BALANCING (TRW)
      *
      * REV:
      *            REGIONAL ACCEPTANCE, PR 1675
      * BLV 020306 FIXED MISSING WRITES ON DEPOSIT & CHECK TRANSACTIONS
      * BLV 021015 ADDED NEW FIELDS IN TRP: INS-TAB-RB, PROMDAYS-PASTDUE,
      *            MIP FIELDS
      *  CS 040907 ADD BR-REGION-NUMBER TO OUTPUT XTRB,XTRC,XTRD,XTRJ,XTRK,XTRL,      *            XTRP, XTRS,XTRX [PR#76 REGACC]
      * BAH 060620 ADDED XTRP-LINE-3 TO HOLD TRP-LINE, ITS COMP IN THE LIVE
      *            COPY MEMBER WHICH ALLOWS IT TO HOLD UP TO 255 (FF)
      *            WAS TRUNCATING IN HERE WHEN MOVING TO XTRP-LINE 9(2)
      *            CAUSING DUP KEYS IN THE WAREHOUSE, REGACC PL# 541
      * BAH 110809 ADDED XTRP-LP-SEQNO , REGACC PR# 295
      *  CS 120807 ADDED XTRP-PRIOR-DF-CODE & XTRP-PRIOR-DF-POSTDATE REGACC
      *                                                    PR #318
      * BAH 150615 EXPANDED XTRB-KEY, XTRC-KEY, XTRD-KEY, XTRJ-KEY, XTRK-KEY
      *            XTRL-KEY, XTRP-KEY, XTRS-KEY AND XTRX-KEY TO 9(9) FROM 9(7)
      *                                                    WORLD PR# 1088
      * MJD 171010 ACTIVATE 8 DIGIT DATES, PD0006
      *            REMOVED REFERENCE TO LPFDTRG AND CLEANED UP SOME
      *            MISSING/CHANGED FIELD NAMES
      * BAH 180306 ADDED TL-MIP-FG THRU TL-DEBIT-CARD-AMT, WORLD #1306
      *  CS 180522 CHANGED MOVE OF TR?-ACCTNO(8) TO A WORKING STORAGE
      *            WS-A15-NUMBER 9(6), WS-A15-SEQNO 9(2) TO MATCH A15 OUTPUT
      * BAH 20190724 TC-TILLNO WAS WRONG, NEED TO MOVE TC-TILLNO WHICH IS
      *              99 TO WS-TILLNO AND ONLY MOVE THE 1 BYTE TO XTC-TILLNO
      *              WHICH IS STILL PIC X LIKE A15. ALSO MOVE TC-ACCTNO,
      *              CS HAD COMMENTED IT OUT.
      *              SAME WITH TL-TILLNO, TP-TILLNO
      *              XTP-PDTH WAS 0, MOVE TP-PDTH-DATE, QA0064
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * BAH 2021.0113 ADDED XTRC-EXTRACT-FG, WORLD #1486
      * BAH 2021.0308 ADDED XTRC-DEBIT-REFCD, WORLD #1490
      * BAH 2021.0804 ADDED TRFILE "W" CASH DRAWER, #1509
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 20220210 FIXED XTRW-TILLNO
      * BAH 2022.0421 ADDED TRB-ACTIONCD2, TRL-ACTIONCD2, TRP-ACTIONCD2,
      *                     TRS-ACTIONCD2, SECURITIZATION #1551
      * BAH 2022.0912 ADDED TRP-LCDUE, VERITEC "IL" #1570
      * RMZ 2023.1010 PERFORMANCE TUNING - ADDED STORED PROCEDURES
      *               EMBEDDED SQL QUERIES
      * BAH 2023.1018 ADDED TRP-REPAY-TRANS-ID #1641
      * RMZ 2024.0520 CURRENTLY ONLY TRB-ACCTNO HAS THE ACCTNO IN THE
      *               A15 FORMAT (WITH A '01' IN THE LAST TWO BYTES).
      *               CHANGED CODE TO MOVE TR?-ACCTNO(8) TO WORKING
      *               STORAGE WS-A15-NUMBER 9(6), WS-A15-SEQNO 9(2)
      *               WITH '01', TO MATCH A15 OUTPUT FOR ALL THE OTHER
      *               'TR?' FILES GENERATED. IN 'TRL' FILE, 'TL-PRLNNO'
      *               ALSO NEEDED TO BE REFORMATTED FOR A15. S35Q-38
      * BAH 2025-0115 RMZ ADDED TP-CLOSFEE-REB-AMT AND TRP-REPAY-TRANS-ID TO
      *               THE SSP FOR TRPFILE. ADDED XTRP-CLOSFEE-REB-AMT
      *                                                     #1697 S35Q-157
      *
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       COPY "LIBEX/EXFSXTRB.CPY".
       COPY "LIBEX/EXFSXTRC.CPY".
       COPY "LIBEX/EXFSXTRD.CPY".
       COPY "LIBEX/EXFSXTRJ.CPY".
       COPY "LIBEX/EXFSXTRK.CPY".
       COPY "LIBEX/EXFSXTRL.CPY".
       COPY "LIBEX/EXFSXTRP.CPY".
       COPY "LIBEX/EXFSXTRS.CPY".
       COPY "LIBEX/EXFSXTRW.CPY".
       COPY "LIBEX/EXFSXTRX.CPY".


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXTRB.CPY".
       COPY "LIBEX/EXFDXTRC.CPY".
       COPY "LIBEX/EXFDXTRD.CPY".
       COPY "LIBEX/EXFDXTRJ.CPY".
       COPY "LIBEX/EXFDXTRK.CPY".
       COPY "LIBEX/EXFDXTRL.CPY".
       COPY "LIBEX/EXFDXTRP.CPY".
       COPY "LIBEX/EXFDXTRS.CPY".
       COPY "LIBEX/EXFDXTRW.CPY".
       COPY "LIBEX/EXFDXTRX.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/TREXTR.CBL S35 05/23/24-13:14:52 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
      *********************************************************************
      * LIBEX/EXWSXTRB: DEPOSIT TRANSACTION EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRB-PATH.
           03  XTRB-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRB-FILENAME       PIC X(10) VALUE "TRANS_BULK".
      *********************************************************************
      * LIBEX/EXWSXTRC: CASH/CHECK TRANSACTION EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRC-PATH.
           03  XTRC-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRC-FILENAME       PIC X(12) VALUE "TRANS_CHECKS".
      *********************************************************************
      * LIBEX/EXWSXTRD: DEPOSIT TRANSACTION EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRD-PATH.
           03  XTRD-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRD-FILENAME       PIC X(13) VALUE "TRANS_DEPOSIT".
      *********************************************************************
      * LIBEX/EXWSXTRJ: CLASS XFER TRANSACTION EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRJ-PATH.
           03  XTRJ-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRJ-FILENAME       PIC X(15) VALUE "TRANS_CLASSXFER".
      *********************************************************************
      * LIBEX/EXWSXTRK: CONVERSION TRANSACTION EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRK-PATH.
           03  XTRK-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRK-FILENAME       PIC X(16) VALUE "TRANS_CONVERSION".
      *********************************************************************
      * LIBEX/EXWSXTRL: LOAN TRANSACTION EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRL-PATH.
           03  XTRL-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRL-FILENAME       PIC X(10) VALUE "TRANS_LOAN".
      *********************************************************************
      * LIBEX/EXWSXTRP: PAYMENT TRANSACTION EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRP-PATH.
           03  XTRP-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRP-FILENAME       PIC X(9) VALUE "TRANS_PMT".
      *********************************************************************
      * LIBEX/EXWSXTRS: SALES FINANCE TRANS EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRS-PATH.
           03  XTRS-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRS-FILENAME       PIC X(14) VALUE "TRANS_SALESFIN".
      *********************************************************************
      * LIBEX/EXWSXTRW: CASH DRAWER BALANCING EXTRACT PATH
      *********************************************************************
       01  XTRW-PATH.
           03  XTRW-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRW-FILENAME       PIC X(13) VALUE "TRANS_DRAWER".
      *********************************************************************
      * LIBEX/EXWSXTRX: XFER TO SERVICE TRANS EXTRACTION FILE PATH WORKERS
      *********************************************************************
       01  XTRX-PATH.
           03  XTRX-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XTRX-FILENAME       PIC X(13) VALUE "TRANS_SERVICE".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

      *----------------------------------------------------------------
       01  XTRB-KEY      PIC 9(9)     VALUE 0.
       01  XTRC-KEY      PIC 9(9)     VALUE 0.
       01  XTRD-KEY      PIC 9(9)     VALUE 0.
       01  XTRJ-KEY      PIC 9(9)     VALUE 0.
       01  XTRK-KEY      PIC 9(9)     VALUE 0.
       01  XTRL-KEY      PIC 9(9)     VALUE 0.
       01  XTRP-KEY      PIC 9(9)     VALUE 0.
       01  XTRS-KEY      PIC 9(9)     VALUE 0.
       01  XTRW-KEY      PIC 9(9)     VALUE 0.
       01  XTRX-KEY      PIC 9(9)     VALUE 0.

       01  WS-BRACCTNO.
           03  WS-BRNO     PIC 9(4).
           03  WS-NUMBER   PIC 9(6).

       01 WS-A15-ACCTNO.
          03  WS-A15-NUMBER    PIC 9(6).
          03  WS-A15-SEQNO     PIC 9(2).

      * TC-TILLNO IS 9(2) IN A30. XTC-TILL IS STILL PIC X LIKE A15.
      * TL-TILLNO, TP-TILLNO TOO
       01  WS-TILLNO           PIC 99.
       01  WS-TILLNOZ REDEFINES WS-TILLNO.
           03  FILLER          PIC X.
           03  WS-TILLNO-X     PIC X.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SQL            PIC X(7500).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  MISC-CONN-OPEN-SW   PIC X  VALUE "N".

       01  WS-SELECT           PIC X(7100).
       01  WS-WHERE            PIC X(200).
       01  WS-ORDERBY          PIC X(200).

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-DATE               PIC 9(8).
           03  BEG-DATEX REDEFINES BEG-DATE.
               05  BEG-CC             PIC 99.
               05  BEG-YY             PIC 99.
               05  BEG-MM             PIC 99.
               05  BEG-DD             PIC 99.
           03  END-DATE               PIC 9(8).
           03  END-DATEX REDEFINES END-DATE.
               05  END-CC             PIC 99.
               05  END-YY             PIC 99.
               05  END-MM             PIC 99.
               05  END-DD             PIC 99.
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  BEG-CLASS              PIC 9(3).
           03  END-CLASS              PIC 9(3).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "B".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(24)    VALUE " ".
           03  H-TITLE          PIC X(27)    VALUE
           "TRANSACTION FILE EXTRACTION".

      *-----------------------------------------------------------------

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/TREXTR_DEF.CPY".
       COPY "LIBSP/TREXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/TREXTR_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE 
               XTRB-FILE XTRC-FILE XTRD-FILE XTRJ-FILE XTRK-FILE
               XTRL-FILE XTRP-FILE XTRS-FILE XTRX-FILE XTRW-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-MISC-CONNECTION.
           CLOSE
               XTRB-FILE XTRC-FILE XTRD-FILE XTRJ-FILE XTRK-FILE
               XTRL-FILE XTRP-FILE XTRS-FILE XTRX-FILE XTRW-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

      D    STOP MESS.

           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TREXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

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

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"   TO TRB-PATH-OVERRIDE
                         TRC-PATH-OVERRIDE
                         TRD-PATH-OVERRIDE
                         TRJ-PATH-OVERRIDE
                         TRK-PATH-OVERRIDE
                         TRL-PATH-OVERRIDE
                         TRP-PATH-OVERRIDE
                         TRS-PATH-OVERRIDE
                         TRW-PATH-OVERRIDE
                         TRX-PATH-OVERRIDE.


           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           PERFORM TRB-CREATE.
           PERFORM TRC-CREATE.
           PERFORM TRD-CREATE.
           PERFORM TRJ-CREATE.
           PERFORM TRK-CREATE.

XXX   D    STOP MESS.

           PERFORM TRL-CREATE.
           PERFORM TRP-CREATE.
           PERFORM TRS-CREATE.
           PERFORM TRW-CREATE.
           PERFORM TRX-CREATE.

       END-OF-GROUP.

           MOVE "Y" TO FIRST-TIME.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ************************************************
       TRB-CREATE SECTION.
      ************************************************
           PERFORM OPEN-TRB1-FILE.

           MOVE BR-NO               TO TB-BRNO
                                       QTRB1-WBEG-BRNO
                                       QTRB1-WEND-BRNO.
           MOVE BEG-DATE            TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WEND-DATE.
           MOVE BEG-CLASS           TO TB-CLASS
                                       QTRB1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRB1-WEND-CLASS.
           MOVE 0                   TO TB-ACCTNO TB-LAWCODE TB-LINE
                                       QTRB1-WBEG-ACCTNO
                                       QTRB1-WBEG-LAWCODE
                                       QTRB1-WBEG-LINE.
           MOVE ALL "9"             TO QTRB1-WEND-ACCTNO
                                       QTRB1-WEND-LAWCODE
                                       QTRB1-WEND-LINE.
           PERFORM START-TRB1-FILE. *> SSP - TRBFILE

       TRB-CREATE-NEXT.
           PERFORM READ-TRB1-FILE-NEXT. *> EMBEDDED - TRBFILE
           IF IO-FG = 9 OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO END-TRB-CREATE.

           IF TB-DATE > END-DATE
              GO TO END-TRB-CREATE.

           IF TB-CLASS < BEG-CLASS OR TB-CLASS > END-CLASS
              GO TO TRB-CREATE-NEXT.

           MOVE SPACES                TO XTRB-REC.

           MOVE TB-BRNO               TO XTRB-BRNO WS-BRNO.
           MOVE TB-ACCTNO             TO WS-NUMBER.

      *CS TO MATCH A15 ACCOUNT NUMBER
           IF TB-ACCTNO                = ZEROES
              MOVE TB-ACCTNO          TO XTRB-ACCTNO
           ELSE
              MOVE TB-ACCTNO          TO WS-A15-NUMBER
              MOVE 01                 TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO      TO XTRB-ACCTNO.

           MOVE WS-BRACCTNO           TO XTRB-BRACCTNO.
           MOVE BR-REGION-NUMBER      TO XTRB-REGION-NUMBER.
           MOVE TB-DATE               TO XTRB-TRANS-DATE.
           MOVE "B"                   TO XTRB-CODE.
           MOVE TB-CLASS              TO XTRB-CLASS.
           MOVE TB-LINE               TO XTRB-LINE.
           MOVE TB-LAWCODE            TO XTRB-LAWCODE.
           MOVE TB-SSNO(1)            TO XTRB-SSNO1.
           MOVE TB-MAKERCD(1)         TO XTRB-MAKERCD1.
           MOVE TB-SSNO(2)            TO XTRB-SSNO2.
           MOVE TB-MAKERCD(2)         TO XTRB-MAKERCD2.
           MOVE TB-SSNO(3)            TO XTRB-SSNO3.
           MOVE TB-MAKERCD(3)         TO XTRB-MAKERCD3.
           MOVE TB-SSNO(4)            TO XTRB-SSNO4.
           MOVE TB-MAKERCD(4)         TO XTRB-MAKERCD4.
           MOVE TB-REF-FRTO           TO XTRB-REF-FRTO.
           MOVE TB-REFBR              TO XTRB-REFBR.
           MOVE TB-PURCD              TO XTRB-PURCD.
           MOVE TB-SRCD               TO XTRB-SRCD.
           MOVE TB-COLLCD             TO XTRB-COLLCD.
           MOVE TB-SERVACCT           TO XTRB-SERVACCT.
           MOVE TB-LOANTYPE           TO XTRB-LOANTYPE.
           MOVE TB-LOANDATE           TO XTRB-LOANDATE.
           MOVE TB-PROCEEDS           TO XTRB-PROCEEDS.
           INSPECT XTRB-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE TB-LNAMT              TO XTRB-LNAMT.
           INSPECT XTRB-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TB-FINCHG             TO XTRB-FINCHG.
           INSPECT XTRB-FINCHG REPLACING LEADING "+" BY " ".
           MOVE TB-INTCHG             TO XTRB-INTCHG.
           INSPECT XTRB-INTCHG REPLACING LEADING "+" BY " ".
           MOVE TB-SERCHG             TO XTRB-SERCHG.
           MOVE TB-EXTCHG             TO XTRB-EXTCHG.
           MOVE TB-MAINTFEE           TO XTRB-MAINTFEE.
           MOVE TB-ORGTERM            TO XTRB-ORGTERM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE SPACES            TO XTRB-FEECODE(SUB)
               MOVE 0                 TO XTRB-FEEAMT(SUB)
               INSPECT XTRB-FEEAMT(SUB) REPLACING LEADING "+" BY " "
               MOVE SPACE             TO XTRB-FEEOURS(SUB)
           END-PERFORM.

           MOVE TB-INSURED            TO XTRB-INSURED.
           MOVE SPACE                 TO XTRB-INSTYPES.
           MOVE SPACE                 TO XTRB-INSELIGIBLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE 0                 TO XTRB-INSCOMP(SUB)
               MOVE SPACE             TO XTRB-INSOURS(SUB)
               MOVE 0                 TO XTRB-INSEFF(SUB)
               MOVE 0                 TO XTRB-INSEXP(SUB)
               MOVE 0                 TO XTRB-INSPREM(SUB)
               INSPECT XTRB-INSPREM(SUB) REPLACING LEADING "+" BY " "
               MOVE 0                 TO XTRB-INSCOMM(SUB)
               INSPECT XTRB-INSCOMM(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TB-DLNO               TO XTRB-DLNO.
           MOVE TB-DLRV               TO XTRB-DLRV.
           INSPECT XTRB-DLRV REPLACING LEADING "+" BY " ".
           MOVE TB-DLSPRV             TO XTRB-DLSPRV.
           INSPECT XTRB-DLSPRV REPLACING LEADING "+" BY " ".
           MOVE TB-DLDISC             TO XTRB-DLDISC.
           INSPECT XTRB-DLDISC REPLACING LEADING "+" BY " ".
           MOVE TB-DLPARV             TO XTRB-DLPARV.
           INSPECT XTRB-DLPARV REPLACING LEADING "+" BY " ".
           MOVE TB-DLRECOURSE         TO XTRB-DLRECOURSE.
           MOVE TB-DLPARTRCAMT        TO XTRB-DLPARTRCAMT.
           MOVE TB-PURBAL             TO XTRB-PURBAL.
           INSPECT XTRB-PURBAL REPLACING LEADING "+" BY " ".

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TB-UNEARN(SUB)  TO XTRB-UNEARN(SUB)
               INSPECT XTRB-UNEARN(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

      *CS TO MATCH A15 ACCOUNT NUMBER
           IF TB-REF-ACCTNO             = ZEROES
              MOVE TB-REF-ACCTNO       TO XTRB-REF-ACCTNO
           ELSE
              MOVE TB-REF-ACCTNO       TO WS-A15-NUMBER
              MOVE 01                  TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO       TO XTRB-REF-ACCTNO.

           MOVE TB-DLRVPRIN(1)        TO XTRB-DLRVPRIN1.
           INSPECT XTRB-DLRVPRIN1 REPLACING LEADING "+" BY " ".
           MOVE TB-DLRVPRIN(2)        TO XTRB-DLRVPRIN2.
           INSPECT XTRB-DLRVPRIN2 REPLACING LEADING "+" BY " ".
           MOVE TB-DLRVPRIN(3)        TO XTRB-DLRVPRIN3.
           INSPECT XTRB-DLRVPRIN3 REPLACING LEADING "+" BY " ".
           MOVE TB-DLRVCHGS(1)        TO XTRB-DLRVCHGS1.
           INSPECT XTRB-DLRVCHGS1 REPLACING LEADING "+" BY " ".
           MOVE TB-DLRVCHGS(2)        TO XTRB-DLRVCHGS2.
           INSPECT XTRB-DLRVCHGS2 REPLACING LEADING "+" BY " ".
           MOVE TB-DLRVCHGS(3)        TO XTRB-DLRVCHGS3.
           INSPECT XTRB-DLRVCHGS3 REPLACING LEADING "+" BY " ".
           MOVE TB-DLPARVHELD         TO XTRB-DLPARVHELD.
           INSPECT XTRB-DLPARVHELD REPLACING LEADING "+" BY " ".
           MOVE TB-DLPARVPAID         TO XTRB-DLPARVPAID.
           INSPECT XTRB-DLPARVPAID REPLACING LEADING "+" BY " ".

           MOVE TB-USERID             TO XTRB-USERID.
           MOVE TB-POSTDATE           TO XTRB-POSTDATE.
           MOVE TB-POSTTIME           TO XTRB-POSTTIME.
           MOVE TB-PLCD               TO XTRB-PLCD.
           MOVE TB-PLTYPE             TO XTRB-PLTYPE.
           MOVE TB-REPOCD             TO XTRB-REPOCD.
           MOVE TB-ACTIONCD2          TO XTRB-ACTIONCD2.
           MOVE X"0D"                 TO XTRB-CR.
           MOVE X"0A"                 TO XTRB-LF.

           ADD 1 TO XTRB-KEY.
           PERFORM WRITE-XTRB-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.
           GO TO TRB-CREATE-NEXT.

       END-TRB-CREATE.
           PERFORM CLOSE-TRB1-FILE.

       EXIT-TRB-CREATE.
           EXIT.

      ************************************************
       TRC-CREATE SECTION.
      ************************************************

           PERFORM OPEN-TRC1-FILE.

           MOVE BR-NO          TO TC-BRNO
                                  QTRC1-WBEG-BRNO
                                  QTRC1-WEND-BRNO.
           MOVE BEG-DATE       TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE
           MOVE END-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.
           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE. *> SSP - TRCFILE

       TRC-CREATE-NEXT.
           PERFORM READ-TRC1-FILE-NEXT. *> EMBEDDED - TRCFILE
           IF IO-FG = 9 OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO END-TRC-CREATE.

           IF TC-KEY-DATE > END-DATE
              GO TO END-TRC-CREATE.

           MOVE SPACES                TO XTRC-REC.

           MOVE TC-BRNO              TO XTRC-BRNO.
           MOVE TC-CKNO              TO XTRC-CKNO.
           MOVE TC-KEY-DATE          TO XTRC-TRANS-DATE.
           MOVE "C"                  TO XTRC-CODE.
           MOVE TC-CLASS             TO XTRC-CLASS.
           MOVE TC-SEQ               TO XTRC-SEQ.
           MOVE TC-TYPE              TO XTRC-TYPE.
           MOVE TC-DATE              TO XTRC-DATE.
           MOVE TC-AMT               TO XTRC-AMT.
           INSPECT XTRC-AMT REPLACING LEADING "+" BY " ".
           MOVE TC-PAYEE             TO XTRC-PAYEE.
           MOVE TC-REF               TO XTRC-REF.
           MOVE TC-REASON            TO XTRC-REASON.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE TC-GLNO(SUB)     TO XTRC-GLNO(SUB)
               MOVE TC-GLAMT(SUB)    TO XTRC-GLAMT(SUB)
               INSPECT XTRC-GLAMT(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TC-VOID              TO XTRC-VOID.
      
      * MATCH A15 ACCOUNT NUMBER
           IF TC-ACCTNO                 = ZEROES
              MOVE TC-ACCTNO           TO XTRC-ACCTNO
           ELSE
              MOVE TC-ACCTNO           TO WS-A15-NUMBER
              MOVE 01                  TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO       TO XTRC-ACCTNO.

           MOVE TC-PAID              TO  XTRC-PAID.
           MOVE TC-PGM               TO  XTRC-PGM.
           MOVE TC-DLRVNO            TO  XTRC-DLRVNO.
           MOVE TC-SPRCLASS          TO  XTRC-SPRCLASS.
           MOVE TC-DLNO              TO  XTRC-DLNO.
      * TC-TILLNO IS 9(2) IN A30. XTC-TILL IS STILL PIC X LIKE A15.
      *    MOVE TC-TILLNO            TO XTRC-TILLNO.
           MOVE TC-TILLNO            TO WS-TILLNO.
           MOVE WS-TILLNO-X          TO XTRC-TILLNO.
           MOVE TC-USERID            TO XTRC-USERID.
           MOVE TC-POSTDATE          TO XTRC-POSTDATE.
           MOVE TC-POSTTIME          TO XTRC-POSTTIME.
           MOVE BR-REGION-NUMBER     TO XTRC-REGION-NUMBER.
           MOVE TC-EXTRACT-FG        TO XTRC-EXTRACT-FG.
           MOVE TC-DEBIT-REFCD       TO XTRC-DEBIT-REFCD.

           MOVE X"0D"                TO XTRC-CR.
           MOVE X"0A"                TO XTRC-LF.

           ADD 1 TO XTRC-KEY.
           PERFORM WRITE-XTRC-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRC-CREATE-NEXT.

       END-TRC-CREATE.
           PERFORM CLOSE-TRC1-FILE.

       EXIT-TRC-CREATE.
           EXIT.

      ************************************************
       TRD-CREATE SECTION.
      ************************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE BR-NO               TO TD-BRNO
                                       QTRD1-WBEG-BRNO
                                       QTRD1-WEND-BRNO.
           MOVE BEG-DATE            TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRD1-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRD1-WEND-DATE.

           PERFORM START-TRD1-FILE. *> SSP - TRDFILE

       TRD-CREATE-NEXT.
           PERFORM READ-TRD1-FILE-NEXT. *> EMBEDDED - TRDFILE
           IF IO-FG = 9 OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO END-TRD-CREATE.

           IF TD-DATE > END-DATE
              GO TO END-TRD-CREATE.

           MOVE SPACES TO XTRD-REC.

           MOVE TD-BRNO              TO XTRD-BRNO.
           MOVE TD-DATE              TO XTRD-TRANS-DATE.

           MOVE "D"                  TO XTRD-CODE.
           MOVE TD-DEPOSIT           TO XTRD-DEPOSIT.
           INSPECT XTRD-DEPOSIT REPLACING LEADING "+" BY " ".
           MOVE TD-BANK              TO XTRD-BANK.
           MOVE TD-CASH              TO XTRD-CASH.
           MOVE TD-GLFG              TO XTRD-GLFG.
           MOVE TD-GLOVERSHORT       TO XTRD-GLOVERSHORT.
           MOVE TD-OVERSHORT         TO XTRD-OVERSHORT.
           INSPECT XTRD-OVERSHORT REPLACING LEADING "+" BY " ".
           MOVE TD-USERID            TO  XTRD-USERID.
           MOVE TD-POSTDATE          TO XTRD-POSTDATE.
           MOVE TD-POSTTIME          TO  XTRD-POSTTIME.
           MOVE BR-REGION-NUMBER     TO XTRD-REGION-NUMBER.

           MOVE X"0D"                 TO XTRD-CR.
           MOVE X"0A"                 TO XTRD-LF.

           ADD 1 TO XTRD-KEY.
           PERFORM WRITE-XTRD-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRD-CREATE-NEXT.

       END-TRD-CREATE.
           PERFORM CLOSE-TRD1-FILE.

       EXIT-TRD-CREATE.
           EXIT.

      ************************************************
       TRJ-CREATE SECTION.
      ************************************************
           PERFORM OPEN-TRJ1-FILE.

           MOVE BR-NO               TO TJ-BRNO
                                       QTRJ1-WBEG-BRNO
                                       QTRJ1-WEND-BRNO.
           MOVE BEG-DATE            TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
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

           PERFORM START-TRJ1-FILE. *> SSP - TRJFILE

       TRJ-CREATE-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT. *> EMBEDDED - TRJFILE
           IF IO-FG = 9 OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO END-TRJ-CREATE.

           IF TJ-DATE > END-DATE
              GO TO END-TRJ-CREATE.

           IF TJ-CLASS < BEG-CLASS OR TJ-CLASS > END-CLASS
              GO TO TRJ-CREATE-NEXT.

           MOVE SPACES                TO XTRJ-REC.

           MOVE TJ-BRNO               TO XTRJ-BRNO WS-BRNO.
           MOVE TJ-ACCTNO             TO WS-NUMBER.

      * MATCH A15 ACCOUNT NUMBER
           IF TJ-ACCTNO                 = ZEROES
              MOVE TJ-ACCTNO           TO XTRJ-ACCTNO
           ELSE
              MOVE TJ-ACCTNO           TO WS-A15-NUMBER
              MOVE 01                  TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO       TO XTRJ-ACCTNO.

           MOVE WS-BRACCTNO           TO XTRJ-BRACCTNO.
           MOVE BR-REGION-NUMBER      TO XTRJ-REGION-NUMBER.
           MOVE TJ-DATE               TO XTRJ-TRANS-DATE.
           MOVE "J"                   TO XTRJ-CODE.
           MOVE TJ-CLASS              TO XTRJ-CLASS.
           MOVE TJ-LINE               TO XTRJ-LINE.
           MOVE TJ-LAWCODE            TO XTRJ-LAWCODE.
           MOVE TJ-SSNO(1)            TO XTRJ-SSNO1.
           MOVE TJ-MAKERCD(1)         TO XTRJ-MAKERCD1.
           MOVE TJ-SSNO(2)            TO XTRJ-SSNO2.
           MOVE TJ-MAKERCD(2)         TO XTRJ-MAKERCD2.
           MOVE TJ-SSNO(3)            TO XTRJ-SSNO3.
           MOVE TJ-MAKERCD(3)         TO XTRJ-MAKERCD3.
           MOVE TJ-SSNO(4)            TO XTRJ-SSNO4.
           MOVE TJ-MAKERCD(4)         TO XTRJ-MAKERCD4.
           MOVE TJ-REF-FRLC           TO XTRJ-REF-FRLC.
           MOVE TJ-REF-X              TO XTRJ-REF-X.
           MOVE TJ-REF-TOLC           TO XTRJ-REF-TOLC.
           MOVE TJ-PURCD              TO XTRJ-PURCD.
           MOVE TJ-SRCD               TO XTRJ-SRCD.
           MOVE TJ-COLLCD             TO XTRJ-COLLCD.
           MOVE TJ-SERVACCT           TO XTRJ-SERVACCT.
           MOVE TJ-LOANTYPE           TO XTRJ-LOANTYPE.
           MOVE TJ-LOANDATE           TO XTRJ-LOANDATE.
           MOVE TJ-PROCEEDS           TO XTRJ-PROCEEDS.
           INSPECT XTRJ-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE TJ-LNAMT              TO XTRJ-LNAMT.
           INSPECT XTRJ-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TJ-FINCHG             TO XTRJ-FINCHG.
           INSPECT XTRJ-FINCHG REPLACING LEADING "+" BY " ".
           MOVE TJ-INTCHG             TO XTRJ-INTCHG.
           MOVE TJ-SERCHG             TO XTRJ-SERCHG.
           MOVE TJ-EXTCHG             TO XTRJ-EXTCHG.
           MOVE TJ-MAINTFEE           TO XTRJ-MAINTFEE.
           MOVE TJ-ORGTERM            TO XTRJ-ORGTERM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE SPACES            TO XTRJ-FEECODE(SUB)
               MOVE 0                 TO XTRJ-FEEAMT(SUB)
               INSPECT XTRJ-FEEAMT(SUB) REPLACING LEADING "+" BY " "
               MOVE SPACE             TO XTRJ-FEEOURS(SUB)
           END-PERFORM.

           MOVE TJ-INSURED            TO XTRJ-INSURED.
           MOVE SPACE                 TO XTRJ-INSTYPES.
           MOVE SPACE                 TO XTRJ-INSELIGIBLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE 0                 TO XTRJ-INSCOMP(SUB)
               MOVE 0                 TO XTRJ-INSOURS(SUB)
               MOVE 0                 TO XTRJ-INSEFF(SUB)
               MOVE 0                 TO XTRJ-INSEXP(SUB)
               MOVE 0                 TO XTRJ-INSPREM(SUB)
               INSPECT XTRJ-INSPREM(SUB) REPLACING LEADING "+" BY " "
               MOVE 0                 TO XTRJ-INSCOMM(SUB)
               INSPECT XTRJ-INSCOMM(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TJ-DLNO               TO XTRJ-DLNO.
           MOVE TJ-DLRV               TO XTRJ-DLRV.
           INSPECT XTRJ-DLRV REPLACING LEADING "+" BY " ".
           MOVE TJ-DLSPRV             TO XTRJ-DLSPRV.
           INSPECT XTRJ-DLSPRV REPLACING LEADING "+" BY " ".
           MOVE TJ-DLDISC             TO XTRJ-DLDISC.
           INSPECT XTRJ-DLDISC REPLACING LEADING "+" BY " ".
           MOVE TJ-DLPARV             TO XTRJ-DLPARV.
           INSPECT XTRJ-DLPARV REPLACING LEADING "+" BY " ".
           MOVE TJ-DLRECOURSE         TO XTRJ-DLRECOURSE.
           MOVE TJ-DLPARTRCAMT        TO XTRJ-DLPARTRCAMT.
           MOVE TJ-PURBAL             TO XTRJ-PURBAL.
           INSPECT XTRJ-PURBAL REPLACING LEADING "+" BY " ".

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TJ-UNEARN(SUB)  TO XTRJ-UNEARN(SUB)
               INSPECT XTRJ-UNEARN(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TJ-PRLNNO             TO XTRJ-PRLNNO.
           MOVE TJ-DLRVPRIN(1)        TO XTRJ-DLRVPRIN1.
           INSPECT XTRJ-DLRVPRIN1 REPLACING LEADING "+" BY " ".
           MOVE TJ-DLRVPRIN(2)        TO XTRJ-DLRVPRIN2.
           INSPECT XTRJ-DLRVPRIN2 REPLACING LEADING "+" BY " ".
           MOVE TJ-DLRVPRIN(3)        TO XTRJ-DLRVPRIN3.
           INSPECT XTRJ-DLRVPRIN3 REPLACING LEADING "+" BY " ".
           MOVE TJ-DLRVCHGS(1)        TO XTRJ-DLRVCHGS1.
           INSPECT XTRJ-DLRVCHGS1 REPLACING LEADING "+" BY " ".
           MOVE TJ-DLRVCHGS(2)        TO XTRJ-DLRVCHGS2.
           INSPECT XTRJ-DLRVCHGS2 REPLACING LEADING "+" BY " ".
           MOVE TJ-DLRVCHGS(3)        TO XTRJ-DLRVCHGS3.
           INSPECT XTRJ-DLRVCHGS3 REPLACING LEADING "+" BY " ".
           MOVE TJ-DLPARVHELD         TO XTRJ-DLPARVHELD.
           INSPECT XTRJ-DLPARVHELD REPLACING LEADING "+" BY " ".
           MOVE TJ-DLPARVPAID         TO XTRJ-DLPARVPAID.
           INSPECT XTRJ-DLPARVPAID REPLACING LEADING "+" BY " ".

           MOVE TJ-USERID             TO XTRJ-USERID.
           MOVE TJ-POSTDATE           TO XTRJ-POSTDATE.
           MOVE TJ-POSTTIME           TO XTRJ-POSTTIME.
           MOVE TJ-PLCD               TO XTRJ-PLCD.
           MOVE TJ-PLTYPE             TO XTRJ-PLTYPE.
           MOVE TJ-REPOCD             TO XTRJ-REPOCD.
           MOVE X"0D"                 TO XTRJ-CR.
           MOVE X"0A"                 TO XTRJ-LF.

           ADD 1 TO XTRJ-KEY.
           PERFORM WRITE-XTRJ-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRJ-CREATE-NEXT.

       END-TRJ-CREATE.
           PERFORM CLOSE-TRJ1-FILE.

       EXIT-TRJ-CREATE.
           EXIT.

      ************************************************
       TRK-CREATE SECTION.
      ************************************************
           PERFORM OPEN-TRK1-FILE.

           MOVE BR-NO               TO TK-BRNO
                                       QTRK1-WBEG-BRNO
                                       QTRK1-WEND-BRNO.
           MOVE BEG-DATE            TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WEND-DATE.
           MOVE BEG-CLASS           TO TK-CLASS
                                       QTRK1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRK1-WEND-CLASS.
           MOVE 0                   TO TK-ACCTNO TK-LAWCODE TK-LINE
                                       QTRK1-WBEG-ACCTNO
                                       QTRK1-WBEG-LAWCODE
                                       QTRK1-WBEG-LINE.
           MOVE ALL "9"             TO QTRK1-WEND-ACCTNO
                                       QTRK1-WEND-LAWCODE
                                       QTRK1-WEND-LINE.

           PERFORM START-TRK1-FILE. *> SSP - TRKFILE

       TRK-CREATE-NEXT.
           PERFORM READ-TRK1-FILE-NEXT. *> EMBEDDED - TRKFILE
           IF IO-FG = 9 OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              GO TO END-TRK-CREATE.

           IF TK-DATE > END-DATE
              GO TO END-TRK-CREATE.

           IF TK-CLASS < BEG-CLASS OR TK-CLASS > END-CLASS
              GO TO TRK-CREATE-NEXT.

           MOVE SPACES                TO XTRK-REC.

           MOVE TK-BRNO               TO XTRK-BRNO WS-BRNO.
           MOVE TK-ACCTNO             TO WS-NUMBER.

      * MATCH A15 ACCOUNT NUMBER
           IF TK-ACCTNO                 = ZEROES
              MOVE TK-ACCTNO           TO XTRK-ACCTNO
           ELSE
              MOVE TK-ACCTNO           TO WS-A15-NUMBER
              MOVE 01                  TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO       TO XTRK-ACCTNO.

           MOVE WS-BRACCTNO           TO XTRK-BRACCTNO.
           MOVE BR-REGION-NUMBER      TO XTRK-REGION-NUMBER.
           MOVE TK-DATE               TO XTRK-TRANS-DATE.

           MOVE "K"                   TO XTRK-CODE.
           MOVE TK-CLASS              TO XTRK-CLASS.
           MOVE TK-LINE               TO XTRK-LINE.
           MOVE TK-LAWCODE            TO XTRK-LAWCODE.
           MOVE TK-SSNO(1)            TO XTRK-SSNO1.
           MOVE TK-MAKERCD(1)         TO XTRK-MAKERCD1.
           MOVE TK-SSNO(2)            TO XTRK-SSNO2.
           MOVE TK-MAKERCD(2)         TO XTRK-MAKERCD2.
           MOVE TK-SSNO(3)            TO XTRK-SSNO3.
           MOVE TK-MAKERCD(3)         TO XTRK-MAKERCD3.
           MOVE TK-SSNO(4)            TO XTRK-SSNO4.
           MOVE TK-MAKERCD(4)         TO XTRK-MAKERCD4.
           MOVE TK-REF-FRTO           TO XTRK-REF-FRTO.
           MOVE TK-REFBR              TO XTRK-REFBR.
           MOVE TK-PURCD              TO XTRK-PURCD.
           MOVE TK-SRCD               TO XTRK-SRCD.
           MOVE TK-COLLCD             TO XTRK-COLLCD.
           MOVE TK-SERVACCT           TO XTRK-SERVACCT.
           MOVE TK-LOANTYPE           TO XTRK-LOANTYPE.
           MOVE TK-REPOCD             TO XTRK-REPOCD.
           MOVE TK-PLCD               TO XTRK-PLCD.
           MOVE TK-PLTYPE             TO XTRK-PLTYPE.
           MOVE TK-LOANDATE           TO XTRK-LOANDATE.
           MOVE TK-PROCEEDS           TO XTRK-PROCEEDS.
           INSPECT XTRK-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE TK-LNAMT              TO XTRK-LNAMT.
           INSPECT XTRK-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TK-FINCHG             TO XTRK-FINCHG.
           INSPECT XTRK-FINCHG REPLACING LEADING "+" BY " ".
           MOVE TK-INTCHG             TO XTRK-INTCHG.
           MOVE TK-SERCHG             TO XTRK-SERCHG.
           MOVE TK-EXTCHG             TO XTRK-EXTCHG.
           MOVE TK-MAINTFEE           TO XTRK-MAINTFEE.
           MOVE TK-ORGTERM            TO XTRK-ORGTERM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE SPACES            TO XTRK-FEECODE(SUB)
               MOVE 0                 TO XTRK-FEEAMT(SUB)
               INSPECT XTRK-FEEAMT(SUB) REPLACING LEADING "+" BY " "
               MOVE SPACE             TO XTRK-FEEOURS(SUB)
           END-PERFORM.

           MOVE TK-INSURED            TO XTRK-INSURED.
           MOVE SPACES                TO XTRK-INSTYPES.
           MOVE SPACES                TO XTRK-INSELIGIBLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE 0                 TO XTRK-INSCOMP(SUB)
               MOVE 0                 TO XTRK-INSOURS(SUB)
               MOVE 0                 TO XTRK-INSEFF(SUB)
               MOVE 0                 TO XTRK-INSEXP(SUB)
               MOVE 0                 TO XTRK-INSPREM(SUB)
               INSPECT XTRK-INSPREM(SUB) REPLACING LEADING "+" BY " "
               MOVE 0                 TO XTRK-INSCOMM(SUB)
               INSPECT XTRK-INSCOMM(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TK-DLNO               TO XTRK-DLNO.
           MOVE TK-DLRV               TO XTRK-DLRV.
           INSPECT XTRK-DLRV REPLACING LEADING "+" BY " ".
           MOVE TK-DLSPRV             TO XTRK-DLSPRV.
           INSPECT XTRK-DLSPRV REPLACING LEADING "+" BY " ".
           MOVE TK-DLDISC             TO XTRK-DLDISC.
           INSPECT XTRK-DLDISC REPLACING LEADING "+" BY " ".
           MOVE TK-DLPARV             TO XTRK-DLPARV.
           INSPECT XTRK-DLPARV REPLACING LEADING "+" BY " ".
           MOVE TK-DLRECOURSE         TO XTRK-DLRECOURSE.
           MOVE TK-DLPARTRCAMT        TO XTRK-DLPARTRCAMT.
           MOVE TK-PURBAL             TO XTRK-PURBAL.
           INSPECT XTRK-PURBAL REPLACING LEADING "+" BY " ".

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TK-UNEARN(SUB)  TO XTRK-UNEARN(SUB)
               INSPECT XTRK-UNEARN(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TK-PRLNNO             TO XTRK-PRLNNO.
           MOVE TK-DLRVPRIN(1)        TO XTRK-DLRVPRIN1.
           INSPECT XTRK-DLRVPRIN1 REPLACING LEADING "+" BY " ".
           MOVE TK-DLRVPRIN(2)        TO XTRK-DLRVPRIN2.
           INSPECT XTRK-DLRVPRIN2 REPLACING LEADING "+" BY " ".
           MOVE TK-DLRVPRIN(3)        TO XTRK-DLRVPRIN3.
           INSPECT XTRK-DLRVPRIN3 REPLACING LEADING "+" BY " ".
           MOVE TK-DLRVCHGS(1)        TO XTRK-DLRVCHGS1.
           INSPECT XTRK-DLRVCHGS1 REPLACING LEADING "+" BY " ".
           MOVE TK-DLRVCHGS(2)        TO XTRK-DLRVCHGS2.
           INSPECT XTRK-DLRVCHGS2 REPLACING LEADING "+" BY " ".
           MOVE TK-DLRVCHGS(3)        TO XTRK-DLRVCHGS3.
           INSPECT XTRK-DLRVCHGS3 REPLACING LEADING "+" BY " ".
           MOVE TK-DLPARVHELD         TO XTRK-DLPARVHELD.
           INSPECT XTRK-DLPARVHELD REPLACING LEADING "+" BY " ".
           MOVE TK-DLPARVPAID         TO XTRK-DLPARVPAID.
           INSPECT XTRK-DLPARVPAID REPLACING LEADING "+" BY " ".

           MOVE TK-USERID             TO XTRK-USERID.
           MOVE TK-POSTDATE           TO XTRK-POSTDATE.
           MOVE TK-POSTTIME           TO XTRK-POSTTIME.
           MOVE X"0D"                 TO XTRK-CR.
           MOVE X"0A"                 TO XTRK-LF.

           ADD 1 TO XTRK-KEY.
           PERFORM WRITE-XTRK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.
           GO TO TRK-CREATE-NEXT.

       END-TRK-CREATE.
           PERFORM CLOSE-TRK1-FILE.

       EXIT-TRK-CREATE.
           EXIT.

      ************************************************
       TRL-CREATE SECTION.
      ************************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEG-DATE            TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
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

           PERFORM START-TRL1-FILE. *> SSP - TRLFILE

       TRL-CREATE-NEXT.
           PERFORM READ-TRL1-FILE-NEXT. *> EMBEDDED - TRLFILE
           IF IO-FG = 9 OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO END-TRL-CREATE.

           IF TL-DATE > END-DATE
              GO TO END-TRL-CREATE.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO TRL-CREATE-NEXT.

           MOVE SPACES                TO XTRL-REC.

           MOVE TL-BRNO               TO XTRL-BRNO WS-BRNO.

           MOVE TL-ACCTNO             TO WS-NUMBER.

      * MATCH A15 ACCOUNT NUMBER
           IF TL-ACCTNO                 = ZEROES
              MOVE TL-ACCTNO           TO XTRL-ACCTNO
           ELSE
              MOVE TL-ACCTNO           TO WS-A15-NUMBER
              MOVE 01                  TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO       TO XTRL-ACCTNO.

           MOVE WS-BRACCTNO           TO XTRL-BRACCTNO.
           MOVE BR-REGION-NUMBER      TO XTRL-REGION-NUMBER.
           MOVE TL-DATE               TO XTRL-TRANS-DATE.
           MOVE "L"                   TO XTRL-CODE.
           MOVE TL-CLASS              TO XTRL-CLASS.
           MOVE TL-LINE               TO XTRL-LINE.
           MOVE TL-LAWCODE            TO XTRL-LAWCODE.
           MOVE TL-SSNO(1)            TO XTRL-SSNO1.
           MOVE TL-MAKERCD(1)         TO XTRL-MAKERCD1.
           MOVE TL-SSNO(2)            TO XTRL-SSNO2.
           MOVE TL-MAKERCD(2)         TO XTRL-MAKERCD2.
           MOVE TL-SSNO(3)            TO XTRL-SSNO3.
           MOVE TL-MAKERCD(3)         TO XTRL-MAKERCD3.
           MOVE TL-SSNO(4)            TO XTRL-SSNO4.
           MOVE TL-MAKERCD(4)         TO XTRL-MAKERCD4.
           MOVE TL-PURCD              TO XTRL-PURCD.
           MOVE TL-SRCD               TO XTRL-SRCD.
           MOVE TL-COLLCD             TO XTRL-COLLCD.
           MOVE TL-SERVACCT           TO XTRL-SERVACCT.
           MOVE TL-LOANTYPE           TO XTRL-LOANTYPE.
           MOVE TL-LOANDATE           TO XTRL-LOANDATE.
           MOVE TL-INTDATE            TO XTRL-INTDATE.
           MOVE TL-PROCEEDS           TO XTRL-PROCEEDS.
           INSPECT XTRL-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE TL-LNAMT              TO XTRL-LNAMT.
           INSPECT XTRL-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TL-FINCHG             TO XTRL-FINCHG.
           INSPECT XTRL-FINCHG REPLACING LEADING "+" BY " ".
           MOVE TL-INTCHG             TO XTRL-INTCHG.
           INSPECT XTRL-INTCHG REPLACING LEADING "+" BY " ".
           MOVE TL-SERCHG             TO XTRL-SERCHG.
           INSPECT XTRL-SERCHG REPLACING LEADING "+" BY " ".
           MOVE TL-EXTCHG             TO XTRL-EXTCHG.
           INSPECT XTRL-EXTCHG REPLACING LEADING "+" BY " ".
           MOVE TL-MAINTFEE           TO XTRL-MAINTFEE.
           INSPECT XTRL-MAINTFEE REPLACING LEADING "+" BY " ".
           MOVE TL-ORGTERM            TO XTRL-ORGTERM.
           MOVE TL-1STPYAMT           TO XTRL-1STPYAMT.
           INSPECT XTRL-1STPYAMT REPLACING LEADING "+" BY " ".
           MOVE TL-NOREGPY            TO XTRL-NOREGPY.
           MOVE TL-REGPYAMT           TO XTRL-REGPYAMT.
           INSPECT XTRL-REGPYAMT REPLACING LEADING "+" BY " ".
           MOVE TL-LASTPYAMT          TO XTRL-LASTPYAMT.
           INSPECT XTRL-LASTPYAMT REPLACING LEADING "+" BY " ".
           MOVE TL-PROCASH            TO XTRL-PROCASH.
           INSPECT XTRL-PROCASH REPLACING LEADING "+" BY " ".

      * TL-TILLNO IS 9(2) IN A30. XTL-TILLNO IS STILL PIC X LIKE A15.
      *    MOVE TL-TILLNO            TO XTRL-TILLNO.
           MOVE TL-TILLNO            TO WS-TILLNO.
           MOVE WS-TILLNO-X          TO XTRL-TILLNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE TL-FEECODE(SUB)   TO XTRL-FEECODE(SUB)
               MOVE TL-FEEAMT(SUB)    TO XTRL-FEEAMT(SUB)
               INSPECT XTRL-FEEAMT(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TL-INSURED            TO XTRL-INSURED.
           MOVE TL-INSTYPES           TO XTRL-INSTYPES.
           MOVE TL-INSELIGIBLE        TO XTRL-INSELIGIBLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE TL-INSCOMP(SUB)   TO XTRL-INSCOMP(SUB)
               MOVE TL-INSOURS(SUB)   TO XTRL-INSOURS(SUB)
               MOVE TL-INSEFF-DATE(SUB) TO XTRL-INSEFF(SUB)
               MOVE TL-INSEXP-DATE(SUB) TO XTRL-INSEXP(SUB)
               MOVE TL-INSPREM(SUB)   TO XTRL-INSPREM(SUB)
               INSPECT XTRL-INSPREM(SUB) REPLACING LEADING "+" BY " "
               MOVE TL-INSCOMM(SUB)   TO XTRL-INSCOMM(SUB)
               INSPECT XTRL-INSCOMM(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TL-DLNO               TO XTRL-DLNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TL-EARNINGS(SUB)  TO XTRL-EARNINGS(SUB)
               INSPECT XTRL-EARNINGS(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TL-ANTICERN(SUB)  TO XTRL-ANTICERN(SUB)
               INSPECT XTRL-ANTICERN(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

      * MATCH A15 ACCOUNT NUMBER
           IF TL-PRLNNO                = ZEROES
              MOVE TL-PRLNNO          TO XTRL-PRLNNO             
           ELSE
              MOVE TL-PRLNNO          TO WS-A15-NUMBER
              MOVE 01                 TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO      TO XTRL-PRLNNO.         

           MOVE TL-PRBAL              TO XTRL-PRBAL.
           INSPECT XTRL-PRBAL REPLACING LEADING "+" BY " ".
           MOVE TL-PRINTDUE           TO XTRL-PRINTDUE.
           INSPECT XTRL-PRINTDUE REPLACING LEADING "+" BY " ".
           MOVE TL-PRFINCHGRB         TO XTRL-PRFINCHGRB.
           INSPECT XTRL-PRFINCHGRB REPLACING LEADING "+" BY " ".
           MOVE TL-PRINSRB            TO XTRL-PRINSRB.
           INSPECT XTRL-PRINSRB REPLACING LEADING "+" BY " ".

           MOVE TL-LPEOD5-FG          TO XTRL-LPEOD5-FG.
           MOVE TL-STATE-WORKER       TO XTRL-STATE-WORKER.
           INSPECT XTRL-STATE-WORKER REPLACING LEADING "+" BY " ".

      * MATCH A15 ACCOUNT NUMBER
           IF TL-RESTRICTD-ACCTNO       = ZEROES
              MOVE TL-RESTRICTD-ACCTNO TO XTRL-RESTRICTD-ACCTNO
           ELSE
              MOVE TL-RESTRICTD-ACCTNO TO WS-A15-NUMBER 
              MOVE 01                  TO WS-A15-SEQNO 
              MOVE WS-A15-ACCTNO       TO XTRL-RESTRICTD-ACCTNO.

           MOVE TL-SPRCLASS           TO XTRL-SPRCLASS.
           MOVE TL-USERID             TO XTRL-USERID.
           MOVE TL-POSTDATE           TO XTRL-POSTDATE.
           MOVE TL-POSTTIME           TO XTRL-POSTTIME.
           MOVE TL-GIFTNET            TO XTRL-GIFTNET.
           MOVE TL-APR                TO XTRL-APR.
           MOVE TL-SP-LNCOST-FRMLA    TO XTRL-SP-LNCOST-FRMLA.
           MOVE TL-MIP-FG             TO XTRL-MIP-FG.
           MOVE TL-INS-TAX-COUNTY-CD  TO XTRL-INS-TAX-COUNTY-CD.
           MOVE TL-INS-TAX-CITY-CD    TO XTRL-INS-TAX-CITY-CD.
           MOVE TL-PRDLDISC-UNEARN    TO XTRL-PRDLDISC-UNEARN.
           MOVE TL-POINTS             TO XTRL-POINTS.
           MOVE TL-DEBIT-CARD-AMT     TO XTRL-DEBIT-CARD-AMT.
           MOVE TL-ACTIONCD2          TO XTRL-ACTIONCD2.

           MOVE X"0D"                 TO XTRL-CR.
           MOVE X"0A"                 TO XTRL-LF.

           ADD 1 TO XTRL-KEY.
           PERFORM WRITE-XTRL-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRL-CREATE-NEXT.

       END-TRL-CREATE.
           PERFORM CLOSE-TRL1-FILE.

       EXIT-TRL-CREATE.
           EXIT.

      ************************************************
       TRP-CREATE SECTION.
      ************************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEG-DATE            TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
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

           PERFORM START-TRP1-FILE. *> SSP - TRP

       TRP-CREATE-NEXT.
           PERFORM READ-TRP1-FILE-NEXT. *> EMBEDDED - TRPFILE
           IF IO-FG = 9 OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO END-TRP-CREATE.

           IF TP-DATE > END-DATE
              GO TO END-TRP-CREATE.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO TRP-CREATE-NEXT.

      * POOLS
      *    IF TP-CODE  = "F"
      *       GO TO TRP-CREATE-NEXT.

           MOVE SPACES                   TO XTRP-REC.

           MOVE TP-BRNO                  TO XTRP-BRNO WS-BRNO.
           MOVE TP-ACCTNO                TO WS-NUMBER.

      * MATCH A15 ACCOUNT NUMBER
           IF TP-ACCTNO                   = ZEROES
              MOVE TP-ACCTNO             TO XTRP-ACCTNO
           ELSE
              MOVE TP-ACCTNO             TO WS-A15-NUMBER
              MOVE 01                    TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO         TO XTRP-ACCTNO.

           MOVE WS-BRACCTNO              TO XTRP-BRACCTNO.
           MOVE BR-REGION-NUMBER         TO XTRP-REGION-NUMBER.
           MOVE TP-DATE                  TO XTRP-TRANS-DATE.
           MOVE "P"                      TO XTRP-CODE.
           MOVE TP-CLASS                 TO XTRP-CLASS.
           MOVE TP-LINE                  TO XTRP-LINE
                                            XTRP-LINE-3.
           MOVE TP-LAWCODE               TO XTRP-LAWCODE.
           MOVE TP-JDCD                  TO XTRP-JDCD.
           MOVE TP-PLCD                  TO XTRP-PLCD.
           MOVE SPACE                    TO XTRP-MORELN.
           MOVE TP-SSNO                  TO XTRP-SSNO.
           MOVE TP-MAKER                 TO XTRP-MAKER.
           MOVE TP-IBPC                  TO XTRP-IBPC.
           MOVE TP-TRCD                  TO XTRP-TRCD.
           MOVE TP-REFNO                 TO XTRP-REFNO.
           MOVE TP-PAYDATE               TO XTRP-PAYDATE.
           MOVE TP-DUEDATE               TO XTRP-DUEDATE.
           MOVE TP-TRAMT                 TO XTRP-TRAMT.
           INSPECT XTRP-TRAMT REPLACING LEADING "+" BY " ".
           MOVE TP-INTDUE                TO XTRP-INTDUE.
           INSPECT XTRP-INTDUE REPLACING LEADING "+" BY " ".
           MOVE TP-APOTH                 TO XTRP-APOTH.
           INSPECT XTRP-APOTH REPLACING LEADING "+" BY " ".
           MOVE TP-APOT2                 TO XTRP-APOT2.
           INSPECT XTRP-APOT2 REPLACING LEADING "+" BY " ".
           MOVE TP-APINT                 TO XTRP-APINT.
           INSPECT XTRP-APINT REPLACING LEADING "+" BY " ".
           MOVE TP-APLC                  TO XTRP-APLC.
           INSPECT XTRP-APLC REPLACING LEADING "+" BY " ".
           MOVE TP-APCUR                 TO XTRP-APCUR.
           INSPECT XTRP-APCUR REPLACING LEADING "+" BY " ".

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE TP-GLNO(SUB)          TO XTRP-GLNO(SUB)
              MOVE TP-GLAMT(SUB)         TO XTRP-GLAMT(SUB)
              INSPECT XTRP-GLAMT(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE TP-EARNINGS(SUB)      TO XTRP-EARNINGS(SUB)
              INSPECT XTRP-EARNINGS(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TP-OTHBAL                TO XTRP-OTHBAL.
           INSPECT XTRP-OTHBAL REPLACING LEADING "+" BY " ".
           MOVE TP-OT2BAL                TO XTRP-OT2BAL.
           INSPECT XTRP-OT2BAL REPLACING LEADING "+" BY " ".
           MOVE TP-INTBAL                TO XTRP-INTBAL.
           INSPECT XTRP-INTBAL REPLACING LEADING "+" BY " ".
           MOVE TP-LCBAL                 TO XTRP-LCBAL.
           INSPECT XTRP-LCBAL REPLACING LEADING "+" BY " ".
           MOVE TP-CURBAL                TO XTRP-CURBAL.
           INSPECT XTRP-CURBAL REPLACING LEADING "+" BY " ".

           MOVE TP-PDTH-DATE             TO XTRP-PDTHRU.

           MOVE TP-DLNO                  TO XTRP-DLNO.
           MOVE TP-DLPROC                TO XTRP-DLPROC.
           INSPECT XTRP-DLPROC REPLACING LEADING "+" BY " ".
           MOVE TP-SERVACCT              TO XTRP-SERVACCT.
           MOVE TP-SALEFIN               TO XTRP-SALEFIN.
           MOVE TP-INSURED               TO XTRP-INSURED.
           MOVE TP-INSOURS               TO XTRP-INSOURS.
           MOVE TP-LOANTYPE              TO XTRP-LOANTYPE.
      * TP-TILLNO IS 9(2) IN A30. XTP-TILL IS STILL PIC X LIKE A15.
      *    MOVE TP-TILLNO                TO  XTRP-TILLNO.
           MOVE TP-TILLNO                TO WS-TILLNO.
           MOVE WS-TILLNO-X              TO XTRP-TILLNO.
           MOVE TP-LCPDTH-DATE           TO XTRP-LCPD.
           MOVE TP-DLNO                  TO XTRP-DLNO.
           MOVE TP-WORKAMT(1)            TO XTRP-WORKAMT1.
           INSPECT XTRP-WORKAMT1 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(2)            TO XTRP-WORKAMT2.
           INSPECT XTRP-WORKAMT2 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(3)            TO XTRP-WORKAMT3.
           INSPECT XTRP-WORKAMT3 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(4)            TO XTRP-WORKAMT4.
           INSPECT XTRP-WORKAMT4 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(5)            TO XTRP-WORKAMT5.
           INSPECT XTRP-WORKAMT5 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(6)            TO XTRP-WORKAMT6.
           INSPECT XTRP-WORKAMT6 REPLACING LEADING "+" BY " ".
           MOVE TP-POSTING-BR            TO XTRP-POSTING-BR.
           MOVE TP-OTHBR-FG              TO XTRP-OTHBR-FG.
           MOVE TP-REV                   TO XTRP-REV.
           MOVE TP-REVTRCD               TO XTRP-REVTRCD.
           MOVE TP-REVSEQNO              TO XTRP-REVSEQNO.
           MOVE TP-STATUSFG              TO XTRP-STATUSFG.
           MOVE TP-KEEPAPINT             TO XTRP-KEEPAPINT.
           MOVE TP-INTPAID               TO XTRP-INTPAID.
           INSPECT XTRP-INTPAID REPLACING LEADING "+" BY " ".
           MOVE TP-PRIOR-POOLID          TO XTRP-PRIOR-POOLID.
           MOVE TP-INSTYPES              TO XTRP-INSTYPES.
      *    MOVE TP-DLRVPRIN1             TO XTRP-DLRVPRIN1.
           MOVE 0                        TO XTRP-DLRVPRIN1.
           INSPECT XTRP-DLRVPRIN1 REPLACING LEADING "+" BY " ".
      *    MOVE TP-DLPARVPAID            TO XTRP-DLPARVPAID.
           MOVE 0                        TO XTRP-DLPARVPAID.
           INSPECT XTRP-DLPARVPAID REPLACING LEADING "+" BY " ".
      *    MOVE TP-DLPARVHELD            TO XTRP-DLPARVHELD.
           MOVE 0                        TO XTRP-DLPARVHELD.
           INSPECT XTRP-DLPARVHELD REPLACING LEADING "+" BY " ".
           MOVE TP-DLPARVHELD-ADJ        TO XTRP-DLPARVHELD-ADJ.
           INSPECT XTRP-DLPARVHELD-ADJ REPLACING LEADING "+" BY " ".
           MOVE TP-DLPARVPAID-ADJ        TO XTRP-DLPARVPAID-ADJ.
           INSPECT XTRP-DLPARVPAID-ADJ REPLACING LEADING "+" BY " ".
           MOVE TP-NEWSTAT-RATE          TO XTRP-NEWSTAT-RATE.
           MOVE TP-POCD                  TO XTRP-POCD.
           MOVE TP-PODATE                TO XTRP-PODATE.
           MOVE TP-DL-RVCHGS-CODE(1)     TO XTRP-DL-RVCHGS-CODE1.
           MOVE TP-DL-RVCHGS-CODE(2)     TO XTRP-DL-RVCHGS-CODE2.
           MOVE TP-DL-RVCHGS-CODE(3)     TO XTRP-DL-RVCHGS-CODE3.
           MOVE TP-DL-PARVHELD-CODE      TO XTRP-DL-PARVHELD-CODE.
           MOVE TP-DL-PARVPAID-CODE      TO XTRP-DL-PARVPAID-CODE.
           MOVE TP-TOTNODEF              TO XTRP-TOTNODEF.
           INSPECT XTRP-TOTNODEF REPLACING LEADING "+" BY " ".
           MOVE TP-LNAMT                 TO XTRP-LNAMT.
           INSPECT XTRP-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TP-ORIG-1STPYDATE        TO XTRP-ORIG-1STPAY.
           MOVE TP-OLD1ST-PYDATE         TO XTRP-OLD1ST-PYDATE.
           MOVE TP-1STPYDATE             TO XTRP-1STPYDATE.
           MOVE TP-ORGTERM               TO XTRP-ORGTERM.
           MOVE TP-INSCOMM               TO XTRP-INSCOMM.
           INSPECT XTRP-INSCOMM REPLACING LEADING "+" BY " ".
           MOVE TP-MANUAL-RB             TO XTRP-MANUAL-RB.
           MOVE TP-USERID                TO XTRP-USERID.
           MOVE TP-POSTTIME              TO XTRP-POSTTIME.
           MOVE TP-PLTYPE                TO XTRP-PLTYPE.
           MOVE TP-PL-OTH-AR             TO XTRP-PL-OTH-AR.
           INSPECT XTRP-PL-OTH-AR REPLACING LEADING "+" BY " ".
           MOVE TP-LP-DLNO               TO XTRP-LP-DLNO.
           MOVE TP-REPOCD                TO XTRP-REPOCD.
           MOVE TP-SPRCLASS              TO XTRP-SPRCLASS.
           MOVE TP-MAX-DEFER-FG          TO XTRP-MAX-DEFER-FG.
           MOVE TP-POSTDATE              TO XTRP-POSTDATE.
           MOVE TP-UNITPER-CD            TO XTRP-UNITPER-CD.
           MOVE TP-UNITPER-FREQ          TO XTRP-UNITPER-FREQ.
           MOVE "N"                      TO XTRP-REVOLVING.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE 0                     TO XTRP-RINSPREM(SUB)
              INSPECT XTRP-RINSPREM(SUB) REPLACING LEADING "+" BY " "
              MOVE 0                     TO XTRP-RINSCOMM(SUB)
              INSPECT XTRP-RINSCOMM(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE 0                        TO XTRP-APOTH-MEMBERSHIP.
           INSPECT XTRP-APOTH-MEMBERSHIP REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-NSF.
           INSPECT XTRP-APOTH-NSF REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC1.
           INSPECT XTRP-APOTH-MISC1 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC2.
           INSPECT XTRP-APOTH-MISC2 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC3.
           INSPECT XTRP-APOTH-MISC3 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC4.
           INSPECT XTRP-APOTH-MISC4 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC5.
           INSPECT XTRP-APOTH-MISC5 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC6.
           INSPECT XTRP-APOTH-MISC6 REPLACING LEADING "+" BY " ".
           MOVE TP-SP-DEFFRMLA           TO XTRP-SP-DEFFRMLA.
           MOVE TP-COLLECTOR             TO XTRP-COLLECTOR.
           MOVE TP-PENALTY-INT           TO XTRP-PENALTY-INT.
           INSPECT XTRP-PENALTY-INT REPLACING LEADING "+" BY " ".
           MOVE TP-INSCOMP               TO XTRP-INSCOMP.
           MOVE TP-PROMDATE              TO XTRP-PROMDATE.
           MOVE TP-PROMAMT               TO XTRP-PROMAMT.
           INSPECT XTRP-PROMAMT REPLACING LEADING "+" BY " ".
           MOVE TP-PROMID                TO XTRP-PROMID.

      * UPDATED FIELDS, 10/15/02, BLV:
           MOVE TP-INS-TAX-RB            TO XTRP-INS-TAX-RB.
           INSPECT XTRP-INS-TAX-RB REPLACING LEADING "+" BY " ".
           MOVE TP-PROMDAYS-PASTDUE      TO XTRP-PROMDAYS-PASTDUE.
           INSPECT XTRP-PROMDAYS-PASTDUE REPLACING LEADING "+" BY " ".

           MOVE TP-MIP-FG                TO XTRP-MIP-FG.
           MOVE TP-MIP-CLPAID            TO XTRP-MIP-CLPAID.
           INSPECT XTRP-MIP-CLPAID REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-CLCOMM            TO XTRP-MIP-CLCOMM.
           INSPECT XTRP-MIP-CLCOMM REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-AHPAID            TO XTRP-MIP-AHPAID.
           INSPECT XTRP-MIP-AHPAID REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-AHCOMM            TO XTRP-MIP-AHCOMM.
           INSPECT XTRP-MIP-AHCOMM REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-CLPDTH-DATE       TO XTRP-MIP-CLPDTH.
           MOVE TP-MIP-AHPDTH-DATE       TO XTRP-MIP-AHPDTH.
           MOVE TP-LP-SEQNO                 TO XTRP-LP-SEQNO.

           MOVE TP-PRIOR-DF-CODE            TO XTRP-PRIOR-DF-CODE.

           IF TP-PRIOR-DF-POSTDATE = 20202020 OR 02020202
              MOVE 0 TO XTRP-PRIOR-DF-POSTDATE
           ELSE
              MOVE TP-PRIOR-DF-POSTDATE TO XTRP-PRIOR-DF-POSTDATE.

           MOVE TP-ACTIONCD2             TO XTRP-ACTIONCD2.
           MOVE TP-LCDUE                 TO XTRP-LCDUE.
           INSPECT XTRP-LCDUE REPLACING LEADING "+" BY " ".
           MOVE TP-REPAY-TRANS-ID        TO XTRP-REPAY-TRANS-ID.
           MOVE TP-CLOSFEE-REB-AMT       TO XTRP-CLOSFEE-REB-AMT.
           INSPECT XTRP-CLOSFEE-REB-AMT REPLACING LEADING "+" BY " ".

           MOVE X"0D"                    TO XTRP-CR.
           MOVE X"0A"                    TO XTRP-LF.

           ADD 1 TO XTRP-KEY.
           PERFORM WRITE-XTRP-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRP-CREATE-NEXT.

       END-TRP-CREATE.
           PERFORM CLOSE-TRP1-FILE.

       EXIT-TRP-CREATE.
           EXIT.

      ************************************************
       TRS-CREATE SECTION.
      ************************************************

           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE BEG-DATE            TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE BEG-CLASS           TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRS1-WEND-CLASS.
           MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.
           PERFORM START-TRS1-FILE. *> SSP - TRSFILE

       TRS-CREATE-NEXT.
           PERFORM READ-TRS1-FILE-NEXT. *> EMBEDDED - TRSFILE
           IF IO-FG = 9 OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO END-TRS-CREATE.

           IF TS-DATE > END-DATE
              GO TO END-TRS-CREATE.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO TRS-CREATE-NEXT.

           MOVE SPACES                TO XTRS-REC.

           MOVE TS-BRNO               TO XTRS-BRNO WS-BRNO.
           MOVE TS-ACCTNO             TO WS-NUMBER.

      * MATCH A15 ACCOUNT NUMBER
           IF TS-ACCTNO                   = ZEROES
              MOVE TS-ACCTNO             TO XTRS-ACCTNO
           ELSE
              MOVE TS-ACCTNO             TO WS-A15-NUMBER
              MOVE 01                    TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO         TO XTRS-ACCTNO.

           MOVE WS-BRACCTNO           TO XTRS-BRACCTNO.
           MOVE BR-REGION-NUMBER      TO XTRS-REGION-NUMBER.

           MOVE TS-DATE               TO XTRS-TRANS-DATE.
           MOVE "S"                   TO XTRS-CODE.
           MOVE TS-CLASS              TO XTRS-CLASS.
           MOVE TS-LINE               TO XTRS-LINE.
           MOVE TS-LAWCODE            TO XTRS-LAWCODE.
           MOVE TS-SSNO(1)            TO XTRS-SSNO1.
           MOVE TS-MAKERCD(1)         TO XTRS-MAKERCD1.
           MOVE TS-SSNO(2)            TO XTRS-SSNO2.
           MOVE TS-MAKERCD(2)         TO XTRS-MAKERCD2.
           MOVE TS-SSNO(3)            TO XTRS-SSNO3.
           MOVE TS-MAKERCD(3)         TO XTRS-MAKERCD3.
           MOVE TS-SSNO(4)            TO XTRS-SSNO4.
           MOVE TS-MAKERCD(4)         TO XTRS-MAKERCD4.
           MOVE TS-PURCD              TO XTRS-PURCD.
           MOVE TS-SRCD               TO XTRS-SRCD.
           MOVE TS-COLLCD             TO XTRS-COLLCD.
           MOVE TS-SERVACCT           TO XTRS-SERVACCT.
           MOVE TS-LOANTYPE           TO XTRS-LOANTYPE.
           MOVE TS-LOANDATE           TO XTRS-LOANDATE.
           MOVE TS-INTDATE            TO XTRS-INTDATE.
           MOVE TS-PROCEEDS           TO XTRS-PROCEEDS.
           INSPECT XTRS-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE TS-LNAMT              TO XTRS-LNAMT.
           INSPECT XTRS-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TS-FINCHG             TO XTRS-FINCHG.
           INSPECT XTRS-FINCHG REPLACING LEADING "+" BY " ".
           MOVE TS-INTCHG             TO XTRS-INTCHG.
           INSPECT XTRS-INTCHG REPLACING LEADING "+" BY " ".
           MOVE TS-SERCHG             TO XTRS-SERCHG.
           INSPECT XTRS-SERCHG REPLACING LEADING "+" BY " ".
           MOVE TS-EXTCHG             TO XTRS-EXTCHG.
           INSPECT XTRS-EXTCHG REPLACING LEADING "+" BY " ".
           MOVE TS-MAINTFEE           TO XTRS-MAINTFEE.
           INSPECT XTRS-MAINTFEE REPLACING LEADING "+" BY " ".
           MOVE TS-ORGTERM            TO XTRS-ORGTERM.
           MOVE TS-1STPYAMT           TO XTRS-1STPYAMT.
           INSPECT XTRS-1STPYAMT REPLACING LEADING "+" BY " ".
           MOVE TS-NOREGPY            TO XTRS-NOREGPY.
           MOVE TS-REGPYAMT           TO XTRS-REGPYAMT.
           INSPECT XTRS-REGPYAMT REPLACING LEADING "+" BY " ".
           MOVE TS-LASTPYAMT          TO XTRS-LASTPYAMT.
           INSPECT XTRS-LASTPYAMT REPLACING LEADING "+" BY " ".

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE TS-FEECODE(SUB)   TO XTRS-FEECODE(SUB)
               MOVE TS-FEEAMT(SUB)    TO XTRS-FEEAMT(SUB)
               INSPECT XTRS-FEEAMT(SUB) REPLACING LEADING "+" BY " "
               MOVE TS-FEEOURS(SUB)   TO XTRS-FEEOURS(SUB)
           END-PERFORM.

           MOVE TS-INSURED            TO XTRS-INSURED.
           MOVE TS-INSTYPES           TO XTRS-INSTYPES.
           MOVE TS-INSELIGIBLE        TO XTRS-INSELIGIBLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE TS-INSCOMP(SUB)   TO XTRS-INSCOMP(SUB)
               MOVE TS-INSOURS(SUB)   TO XTRS-INSOURS(SUB)
               MOVE TS-INSEFF-DATE(SUB) TO XTRS-INSEFF(SUB)
               MOVE TS-INSEXP-DATE(SUB) TO XTRS-INSEXP(SUB)
               MOVE TS-INSPREM(SUB)   TO XTRS-INSPREM(SUB)
               INSPECT XTRS-INSPREM(SUB) REPLACING LEADING "+" BY " "
               MOVE TS-INSCOMM(SUB)   TO XTRS-INSCOMM(SUB)
               INSPECT XTRS-INSCOMM(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

            MOVE 0 TO XTRS-INSCOMP(9) XTRS-INSCOMP(10)
                      XTRS-INSEFF(9)  XTRS-INSEFF(10)
                      XTRS-INSEXP(9)  XTRS-INSEXP(10)
                      XTRS-INSPREM(9) XTRS-INSPREM(10)
                      XTRS-INSCOMM(9) XTRS-INSCOMM(10).
            MOVE " " TO XTRS-INSOURS(9) XTRS-INSOURS(10).
            INSPECT XTRS-INSPREM(9) REPLACING LEADING "+" BY " ".
            INSPECT XTRS-INSPREM(10) REPLACING LEADING "+" BY " ".
            INSPECT XTRS-INSCOMM(9) REPLACING LEADING "+" BY " ".
            INSPECT XTRS-INSCOMM(10) REPLACING LEADING "+" BY " ".

           MOVE TS-DLNO               TO XTRS-DLNO.
           MOVE TS-DLRECOURSE         TO XTRS-DLRECOURSE.
           MOVE TS-DLPARTRCAMT        TO XTRS-DLPARTRCAMT.
           INSPECT XTRS-DLPARTRCAMT REPLACING LEADING "+" BY " ".
           MOVE TS-PURBAL             TO XTRS-PURBAL.
           INSPECT XTRS-PURBAL REPLACING LEADING "+" BY " ".
           MOVE TS-DLRVPRIN(1)        TO XTRS-DLRVPRIN1.
           INSPECT XTRS-DLRVPRIN1 REPLACING LEADING "+" BY " ".
           MOVE TS-DLRVPRIN(2)        TO XTRS-DLRVPRIN2.
           INSPECT XTRS-DLRVPRIN2 REPLACING LEADING "+" BY " ".
           MOVE TS-DLRVPRIN(3)        TO XTRS-DLRVPRIN3.
           INSPECT XTRS-DLRVPRIN3 REPLACING LEADING "+" BY " ".
           MOVE TS-DLRVCHGS(1)        TO XTRS-DLRVCHGS1.
           INSPECT XTRS-DLRVCHGS1 REPLACING LEADING "+" BY " ".
           MOVE TS-DLRVCHGS(2)        TO XTRS-DLRVCHGS2.
           INSPECT XTRS-DLRVCHGS2 REPLACING LEADING "+" BY " ".
           MOVE TS-DLRVCHGS(3)        TO XTRS-DLRVCHGS3.
           INSPECT XTRS-DLRVCHGS3 REPLACING LEADING "+" BY " ".
           MOVE TS-DLPARVHELD         TO XTRS-DLPARVHELD.
           INSPECT XTRS-DLPARVHELD REPLACING LEADING "+" BY " ".
           MOVE TS-DLPARVPAID         TO XTRS-DLPARVPAID.
           INSPECT XTRS-DLPARVPAID REPLACING LEADING "+" BY " ".
           MOVE TS-DLDISC             TO XTRS-DLDISC.
           INSPECT XTRS-DLDISC REPLACING LEADING "+" BY " ".
           MOVE TS-ANTICTERM          TO XTRS-ANTICTERM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TS-ANTICERN(SUB)  TO XTRS-ANTICERN(SUB)
               INSPECT XTRS-ANTICERN(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TS-EARNINGS(SUB)  TO XTRS-EARNINGS(SUB)
               INSPECT XTRS-EARNINGS(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TS-PRLNNO             TO XTRS-PRLNNO.
           MOVE TS-INSPREM-DLADJ      TO XTRS-INSPREM-DLADJ.
           INSPECT XTRS-INSPREM-DLADJ REPLACING LEADING "+" BY " ".
           MOVE TS-PRDLDISC-UNEARN    TO XTRS-PRDLDISC-UNEARN.
           INSPECT XTRS-PRDLDISC-UNEARN REPLACING LEADING "+" BY " ".
           MOVE TS-SP-LNCOST-FRMLA    TO XTRS-SP-LNCOST-FRMLA.
           MOVE TS-DL-RVCHGS-CODE(1)  TO XTRS-DL-RVCHGS-CODE1.
           MOVE TS-DL-RVCHGS-CODE(2)  TO XTRS-DL-RVCHGS-CODE2.
           MOVE TS-DL-RVCHGS-CODE(3)  TO XTRS-DL-RVCHGS-CODE3.
           MOVE TS-DL-PARVHELD-CODE   TO XTRS-DL-PARVHELD-CODE.
           MOVE TS-DL-PARVPAID-CODE   TO XTRS-DL-PARVPAID-CODE.
           MOVE TS-SPRCLASS           TO XTRS-SPRCLASS.
           MOVE TS-O3INS-DLPROC       TO XTRS-O3INS-DLPROC.
           INSPECT XTRS-O3INS-DLPROC REPLACING LEADING "+" BY " ".
           MOVE TS-USERID             TO XTRS-USERID.
           MOVE TS-POSTDATE           TO XTRS-POSTDATE.
           MOVE TS-POSTTIME           TO XTRS-POSTTIME.

           MOVE TS-DLCOMM-PP-CMSI     TO XTRS-DLCOMM-PP-CMSI.
           INSPECT XTRS-DLCOMM-PP-CMSI REPLACING LEADING "+" BY " ".
           MOVE TS-DLCOMM-O3-CMSI     TO XTRS-DLCOMM-O3-CMSI.
           INSPECT XTRS-DLCOMM-O3-CMSI REPLACING LEADING "+" BY " ".

           MOVE TS-MIP-FG             TO XTRS-MIP-FG.
           MOVE TS-DLPAID             TO XTRS-DLPAID.
           MOVE TS-ACTIONCD2          TO XTRS-ACTIONCD2.

           MOVE X"0D"                 TO XTRS-CR.
           MOVE X"0A"                 TO XTRS-LF.

           ADD 1 TO XTRS-KEY.
           PERFORM WRITE-XTRS-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRS-CREATE-NEXT.

       END-TRS-CREATE.
           PERFORM CLOSE-TRS1-FILE.

       EXIT-TRS-CREATE.
           EXIT.

      ************************************************
       TRW-CREATE SECTION.
      ************************************************

           PERFORM OPEN-TRW1-FILE.

           MOVE BR-NO               TO TW-BRNO
                                       QTRW1-WBEG-BRNO
                                       QTRW1-WEND-BRNO.
           MOVE BEG-DATE            TO TW-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRW1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRW1-WEND-DATE.
           MOVE 0                   TO TW-TILLNO
                                       QTRW1-WBEG-TILLNO.
           MOVE ALL "9"             TO QTRW1-WEND-TILLNO.

           PERFORM START-TRW1-FILE. *> SSP - TRWFILE

       TRW-CREATE-NEXT.
           PERFORM READ-TRW1-FILE-NEXT. *> EMBEDDED - TRWFILE
           IF IO-FG = 9 OR (TRW-PATH-OWNBR NOT = TW-BRNO)
              GO TO END-TRW-CREATE.

           IF TW-DATE > END-DATE
              GO TO END-TRW-CREATE.

           MOVE SPACES                TO XTRW-REC.
           MOVE TW-BRNO               TO XTRW-BRNO.
           MOVE TW-DATE               TO XTRW-TRANS-DATE.
      * TW-TILLNO IS 9(2) IN A30. XTRW-TILL IS STILL PIC X LIKE A15.
           MOVE TW-TILLNO             TO WS-TILLNO.
           MOVE WS-TILLNO-X           TO XTRW-TILLNO.

           MOVE "W"                   TO XTRW-CODE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
             MOVE TW-POSTNAME(SUB)  TO XTRW-POSTNAME(SUB)
             MOVE TW-TIME(SUB)      TO XTRW-TIME(SUB)
             MOVE TW-AMOUNT(SUB)    TO XTRW-AMOUNT(SUB)
             INSPECT XTRW-AMOUNT(SUB) REPLACING LEADING "+" BY " "
             MOVE TW-OVERSHORT(SUB) TO XTRW-OVERSHORT(SUB)
             INSPECT XTRW-OVERSHORT(SUB)
                                      REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE X"0D"                 TO XTRW-CR.
           MOVE X"0A"                 TO XTRW-LF.

           ADD 1 TO XTRW-KEY.
           PERFORM WRITE-XTRW-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRW-CREATE-NEXT.

       END-TRW-CREATE.
           PERFORM CLOSE-TRW1-FILE.

       EXIT-TRW-CREATE.
           EXIT.

      ************************************************
       TRX-CREATE SECTION.
      ************************************************

           PERFORM OPEN-TRX1-FILE.

           MOVE BR-NO               TO TX-BRNO
                                       QTRX1-WBEG-BRNO
                                       QTRX1-WEND-BRNO.
           MOVE BEG-DATE            TO TX-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WEND-DATE.
           MOVE BEG-CLASS           TO TX-CLASS
                                       QTRX1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRX1-WEND-CLASS.
           MOVE 0                   TO TX-ACCTNO TX-LAWCODE TX-LINE
                                       QTRX1-WBEG-ACCTNO
                                       QTRX1-WBEG-LAWCODE
                                       QTRX1-WBEG-LINE.
           MOVE ALL "9"             TO QTRX1-WEND-ACCTNO
                                       QTRX1-WEND-LAWCODE
                                       QTRX1-WEND-LINE.
           PERFORM START-TRX1-FILE. *> SSP - TRXFILE

       TRX-CREATE-NEXT.
           PERFORM READ-TRX1-FILE-NEXT. *> EMBEDDED - TRXFILE
           IF IO-FG = 9 OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              GO TO END-TRX-CREATE.

           IF TX-DATE > END-DATE
              GO TO END-TRX-CREATE.

           IF TX-CLASS < BEG-CLASS OR TX-CLASS > END-CLASS
              GO TO TRX-CREATE-NEXT.

           MOVE SPACES                TO XTRX-REC.

           MOVE TX-BRNO               TO XTRX-BRNO WS-BRNO.
           MOVE TX-ACCTNO             TO WS-NUMBER.

      * MATCH A15 ACCOUNT NUMBER
           IF TX-ACCTNO                 = ZEROES
              MOVE TX-ACCTNO           TO XTRX-ACCTNO
           ELSE
              MOVE TX-ACCTNO           TO WS-A15-NUMBER
              MOVE 01                  TO WS-A15-SEQNO
              MOVE WS-A15-ACCTNO       TO XTRX-ACCTNO.

           MOVE WS-BRACCTNO           TO XTRX-BRACCTNO.
           MOVE BR-REGION-NUMBER      TO XTRX-REGION-NUMBER.

           MOVE TX-DATE               TO XTRX-TRANS-DATE.
           MOVE "X"                   TO XTRX-CODE.
           MOVE TX-CLASS              TO XTRX-CLASS.
           MOVE TX-LINE               TO XTRX-LINE.
           MOVE TX-LAWCODE            TO XTRX-LAWCODE.
           MOVE TX-SSNO(1)            TO XTRX-SSNO1.
           MOVE TX-MAKERCD(1)         TO XTRX-MAKERCD1.
           MOVE TX-SSNO(2)            TO XTRX-SSNO2.
           MOVE TX-MAKERCD(2)         TO XTRX-MAKERCD2.
           MOVE TX-SSNO(3)            TO XTRX-SSNO3.
           MOVE TX-MAKERCD(3)         TO XTRX-MAKERCD3.
           MOVE TX-SSNO(4)            TO XTRX-SSNO4.
           MOVE TX-MAKERCD(4)         TO XTRX-MAKERCD4.
           MOVE TX-PURCD              TO XTRX-PURCD.
           MOVE TX-SRCD               TO XTRX-SRCD.
           MOVE TX-COLLCD             TO XTRX-COLLCD.
           MOVE TX-SERVACCT           TO XTRX-SERVACCT.
           MOVE TX-LOANTYPE           TO XTRX-LOANTYPE.
           MOVE TX-LOANDATE           TO XTRX-LOANDATE.
           MOVE TX-PROCEEDS           TO XTRX-PROCEEDS.
           INSPECT XTRX-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE TX-LNAMT              TO XTRX-LNAMT.
           INSPECT XTRX-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TX-FINCHG             TO XTRX-FINCHG.
           INSPECT XTRX-FINCHG REPLACING LEADING "+" BY " ".
           MOVE TX-INTCHG             TO XTRX-INTCHG.
           MOVE TX-SERCHG             TO XTRX-SERCHG.
           MOVE TX-EXTCHG             TO XTRX-EXTCHG.
           MOVE TX-MAINTFEE           TO XTRX-MAINTFEE.
           MOVE TX-ORGTERM            TO XTRX-ORGTERM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE SPACES            TO XTRX-FEECODE(SUB)
               MOVE 0                 TO XTRX-FEEAMT(SUB)
               INSPECT XTRX-FEEAMT(SUB) REPLACING LEADING "+" BY " "
               MOVE SPACE             TO XTRX-FEEOURS(SUB)
           END-PERFORM.

           MOVE TX-INSURED            TO XTRX-INSURED.
           MOVE SPACE                 TO XTRX-INSTYPES.
           MOVE SPACE                 TO XTRX-INSELIGIBLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 0                 TO XTRX-INSCOMP(SUB)
               MOVE 0                 TO XTRX-INSOURS(SUB)
               MOVE 0                 TO XTRX-INSEFF(SUB)
               MOVE 0                 TO XTRX-INSEXP(SUB)
               MOVE 0                 TO XTRX-INSPREM(SUB)
               INSPECT XTRX-INSPREM(SUB) REPLACING LEADING "+" BY " "
               MOVE 0                 TO XTRX-INSCOMM(SUB)
               INSPECT XTRX-INSCOMM(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TX-DLNO               TO XTRX-DLNO.
           MOVE TX-DLRECOURSE         TO XTRX-DLRECOURSE.
           MOVE TX-DLPARTRCAMT        TO XTRX-DLPARTRCAMT.
           INSPECT XTRX-DLPARTRCAMT REPLACING LEADING "+" BY " ".
           INSPECT XTRX-PURBAL REPLACING LEADING "+" BY " ".
           MOVE TX-DLRVPRIN(1)        TO XTRX-DLRVPRIN1.
           INSPECT XTRX-DLRVPRIN1 REPLACING LEADING "+" BY " ".
           MOVE TX-DLRVPRIN(2)        TO XTRX-DLRVPRIN2.
           INSPECT XTRX-DLRVPRIN2 REPLACING LEADING "+" BY " ".
           MOVE TX-DLRVPRIN(3)        TO XTRX-DLRVPRIN3.
           INSPECT XTRX-DLRVPRIN3 REPLACING LEADING "+" BY " ".
           MOVE TX-DLRVCHGS(1)        TO XTRX-DLRVCHGS1.
           INSPECT XTRX-DLRVCHGS1 REPLACING LEADING "+" BY " ".
           MOVE TX-DLRVCHGS(2)        TO XTRX-DLRVCHGS2.
           INSPECT XTRX-DLRVCHGS2 REPLACING LEADING "+" BY " ".
           MOVE TX-DLRVCHGS(3)        TO XTRX-DLRVCHGS3.
           INSPECT XTRX-DLRVCHGS3 REPLACING LEADING "+" BY " ".
           MOVE TX-DLPARVHELD         TO XTRX-DLPARVHELD.
           INSPECT XTRX-DLPARVHELD REPLACING LEADING "+" BY " ".
           MOVE TX-DLPARVPAID         TO XTRX-DLPARVPAID.
           INSPECT XTRX-DLPARVPAID REPLACING LEADING "+" BY " ".
           MOVE TX-DLDISC             TO XTRX-DLDISC.
           INSPECT XTRX-DLDISC REPLACING LEADING "+" BY " ".
           MOVE TX-ANTICTERM          TO XTRX-ANTICTERM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TX-ANTICERN(SUB)  TO XTRX-ANTICERN(SUB)
               INSPECT XTRX-ANTICERN(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               MOVE TX-EARNINGS(SUB)  TO XTRX-EARNINGS(SUB)
               INSPECT XTRX-EARNINGS(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TX-PRLNNO             TO XTRX-PRLNNO.
           MOVE TX-PRBAL              TO XTRX-PRBAL.
           INSPECT XTRX-PRBAL REPLACING LEADING "+" BY " ".
           MOVE TX-TFRBAL             TO XTRX-TFRBAL.
           INSPECT XTRX-TFRBAL REPLACING LEADING "+" BY " ".
           MOVE TX-SPRCLASS           TO XTRX-SPRCLASS.
           MOVE TX-USERID             TO XTRX-USERID.
           MOVE TX-POSTDATE           TO XTRX-POSTDATE.
           MOVE TX-POSTTIME           TO XTRX-POSTTIME.
           MOVE X"0D"                 TO XTRX-CR.
           MOVE X"0A"                 TO XTRX-LF.

           ADD 1 TO XTRX-KEY.
           PERFORM WRITE-XTRX-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO TRX-CREATE-NEXT.

       END-TRX-CREATE.
           PERFORM CLOSE-TRX1-FILE.

       EXIT-TRX-CREATE.
           EXIT.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE TREXTR-QUEUE-POS TO Q-POS.
           MOVE TREXTR-COPIES-POS TO C-POS.
           MOVE TREXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XTRP-DIRECTORY, VALUE 13.
           MOVE XTRP-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS.............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           CALL "C$TOLOWER" USING XTRB-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRB-FILE-OUTPUT.
           PERFORM CLOSE-XTRB-FILE.
           PERFORM OPEN-XTRB-FILE.

           CALL "C$TOLOWER" USING XTRC-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRC-FILE-OUTPUT.
           PERFORM CLOSE-XTRC-FILE.
           PERFORM OPEN-XTRC-FILE.

           CALL "C$TOLOWER" USING XTRD-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRD-FILE-OUTPUT.
           PERFORM CLOSE-XTRD-FILE.
           PERFORM OPEN-XTRD-FILE.

           CALL "C$TOLOWER" USING XTRJ-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRJ-FILE-OUTPUT.
           PERFORM CLOSE-XTRJ-FILE.
           PERFORM OPEN-XTRJ-FILE.

           CALL "C$TOLOWER" USING XTRK-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRK-FILE-OUTPUT.
           PERFORM CLOSE-XTRK-FILE.
           PERFORM OPEN-XTRK-FILE.

           CALL "C$TOLOWER" USING XTRL-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRL-FILE-OUTPUT.
           PERFORM CLOSE-XTRL-FILE.
           PERFORM OPEN-XTRL-FILE.

           CALL "C$TOLOWER" USING XTRP-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRP-FILE-OUTPUT.
           PERFORM CLOSE-XTRP-FILE.
           PERFORM OPEN-XTRP-FILE.

           CALL "C$TOLOWER" USING XTRS-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRS-FILE-OUTPUT.
           PERFORM CLOSE-XTRS-FILE.
           PERFORM OPEN-XTRS-FILE.

           CALL "C$TOLOWER" USING XTRX-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRX-FILE-OUTPUT.
           PERFORM CLOSE-XTRX-FILE.
           PERFORM OPEN-XTRX-FILE.

           CALL "C$TOLOWER" USING XTRW-DIRECTORY, VALUE 13.
           PERFORM OPEN-XTRW-FILE-OUTPUT.
           PERFORM CLOSE-XTRW-FILE.
           PERFORM OPEN-XTRW-FILE.

           PERFORM OPEN-MISC-CONNECTION.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.
           IF END-MM NOT = BEG-MM OR END-YY NOT = BEG-YY
              MOVE "DATE RANGE MAY NOT CROSS MONTHS!" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO CL-02.

      *----------------------------------------------------------------
       BEGDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

           IF BEG-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8080BEGDATE." TO VDU
              MOVE DATE-YYYYMMDD TO BEG-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENDDT.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000ENDDATE." TO VDU
              MOVE DATE-YYYYMMDD TO END-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

           IF END-MM NOT = BEG-MM OR END-YY NOT = BEG-YY
              MOVE "DATE RANGE MAY NOT CROSS MONTHS!" TO MESS
              PERFORM SEND-MESS
              GO TO BEGDT.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
      *    IF VDUSTATUS = "1U" GO TO ENDDT.
           IF VDUSTATUS = "1U" GO TO BEGDT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S NA000BR1." TO VDU
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
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
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

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
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
      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
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
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO EXIT-PARM.
      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNA040BR2." TO VDU
              MOVE SPACES TO VDUNUM0
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
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CLASS       :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *************************************************************
       CLASS-SCAN SECTION.
      *************************************************************
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
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO TREXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TREXTR-SYSDATE.
           DISPLAY TREXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TREXTR-FORM-FIELDS TO WR-BUF.
           MOVE TREXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE TREXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE TREXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/TREXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       OPEN-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS MISC_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO MISC-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC
               PERFORM SQL-IO-VALIDATION.

      ******************************************************************
       CLOSE-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT MISC_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO MISC-CONN-OPEN-SW.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRWGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPTRB1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRB1RN
       LOAD-TRB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRB-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRB-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH.

      *-----------------------------------------------------------------
       OPEN-TRB1-FILE.
           PERFORM LOAD-TRB1-FILE.
           PERFORM OPEN-IT.
           MOVE TRB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRB1-FILE. *> SSP - TRBFILE
           PERFORM START-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRB1-FILE.

           STRING "SELECT TRBFILE.TB_BRNO,"
                  "CAST(TRBFILE.TB_DATE AS VARCHAR(10))," 
                  "TRBFILE.TB_CLASS,"
                  "TRBFILE.TB_ACCTNO,"
                  "TRBFILE.TB_LAWCODE,"
                  "TRBFILE.TB_LINE,"
                  "TRBFILE.TB_SSNO_1,"
                  "TRBFILE.TB_SSNO_2,"
                  "TRBFILE.TB_SSNO_3,"
                  "TRBFILE.TB_SSNO_4,"
                  "TRBFILE.TB_MAKERCD_1," 
                  "TRBFILE.TB_MAKERCD_2,"
                  "TRBFILE.TB_MAKERCD_3," 
                  "TRBFILE.TB_MAKERCD_4," 
                  "TRBFILE.TB_REF_FRTO," 
                  "TRBFILE.TB_REFBR,"
                  "TRBFILE.TB_PURCD," 
                  "TRBFILE.TB_SRCD," 
                  "TRBFILE.TB_COLLCD," 
                  "TRBFILE.TB_SERVACCT," 
                  "TRBFILE.TB_LOANTYPE," 
                  "CAST(TRBFILE.TB_LOANDATE  AS VARCHAR(10)),"
                  "TRBFILE.TB_PROCEEDS,"
                  "TRBFILE.TB_LNAMT,"
                  "TRBFILE.TB_FINCHG,"
                  "TRBFILE.TB_INTCHG,"
                  "TRBFILE.TB_SERCHG,"
                  "TRBFILE.TB_EXTCHG,"
                  "TRBFILE.TB_MAINTFEE,"
                  "TRBFILE.TB_ORGTERM,"
                  "TRBFILE.TB_INSURED,"
                  "TRBFILE.TB_DLNO,"
                  "TRBFILE.TB_DLRV,"
                  "TRBFILE.TB_DLSPRV,"
                  "TRBFILE.TB_DLDISC,"
                  "TRBFILE.TB_DLPARV,"
                  "TRBFILE.TB_DLRECOURSE," 
                  "TRBFILE.TB_DLPARTRCAMT,"
                  "TRBFILE.TB_PURBAL,"
                  "TRBFILE.TB_UNEARN_1,"
                  "TRBFILE.TB_UNEARN_2,"
                  "TRBFILE.TB_UNEARN_3,"
                  "TRBFILE.TB_UNEARN_4,"
                  "TRBFILE.TB_UNEARN_5,"
                  "TRBFILE.TB_UNEARN_6,"
                  "TRBFILE.TB_UNEARN_7,"
                  "TRBFILE.TB_UNEARN_8,"
                  "TRBFILE.TB_UNEARN_9,"
                  "TRBFILE.TB_UNEARN_10,"
                  "TRBFILE.TB_REF_ACCTNO,"
                  "TRBFILE.TB_DLRVPRIN_1,"
                  "TRBFILE.TB_DLRVPRIN_2,"
                  "TRBFILE.TB_DLRVPRIN_3,"
                  "TRBFILE.TB_DLRVCHGS_1,"
                  "TRBFILE.TB_DLRVCHGS_2,"
                  "TRBFILE.TB_DLRVCHGS_3,"
                  "TRBFILE.TB_DLPARVPAID,"
                  "TRBFILE.TB_DLPARVHELD,"
                  "TRBFILE.TB_USERID," 
                  "CAST(TRBFILE.TB_POSTDATE  AS VARCHAR(10))," 
                  "TRBFILE.TB_POSTTIME,"
                  "TRBFILE.TB_PLCD,"
                  "TRBFILE.TB_PLTYPE,"
                  "TRBFILE.TB_REPOCD,"
                  "TRBFILE.TB_ACTIONCD2"
                  " FROM DBO.TRBFILE | " INTO WS-SELECT.

           STRING " WHERE TRBFILE.TB_BRNO = " ,  
                   QTRB1-WBEG-BRNO ,  
                  " AND TRBFILE.TB_DATE BETWEEN '"  ,
                   QTRB1-WBEG-DATE  ,
                  "' AND '" ,
                   QTRB1-WEND-DATE ,
                  "' AND TRBFILE.TB_CLASS BETWEEN " ,
                   QTRB1-WBEG-CLASS ,
                  " AND " ,
                   QTRB1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING " ORDER BY TRBFILE.TB_BRNO,"
                  " TRBFILE.TB_DATE,"
                  " TRBFILE.TB_CLASS,"
                  " TRBFILE.TB_ACCTNO,"
                  " TRBFILE.TB_LAWCODE,"
                  " TRBFILE.TB_LINE |" INTO WS-ORDERBY.
 
           STRING WS-SELECT  DELIMITED BY "|",
                  WS-WHERE   DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           INSPECT PASS-SQL REPLACING ALL "|" BY SPACE.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRB1_CURSOR CURSOR FOR
                :TRB1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO TRB1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRB1-FILE-NEXT. *> EMBEDDED - TRBFILE
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           INITIALIZE QTRB-REC.

           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                  FETCH TRB1_CURSOR INTO  
                   :QTB-BRNO,
                   :QTB-DATE, 
                   :QTB-CLASS,
                   :QTB-ACCTNO,
                   :QTB-LAWCODE,
                   :QTB-LINE,
                   :QTB-SSNO-1,
                   :QTB-SSNO-2,
                   :QTB-SSNO-3,
                   :QTB-SSNO-4,
                   :QTB-MAKERCD-1, 
                   :QTB-MAKERCD-2, 
                   :QTB-MAKERCD-3, 
                   :QTB-MAKERCD-4, 
                   :QTB-REF-FRTO, 
                   :QTB-REFBR,
                   :QTB-PURCD, 
                   :QTB-SRCD, 
                   :QTB-COLLCD, 
                   :QTB-SERVACCT, 
                   :QTB-LOANTYPE, 
                   :QTB-LOANDATE,
                   :QTB-PROCEEDS,
                   :QTB-LNAMT,
                   :QTB-FINCHG,
                   :QTB-INTCHG,
                   :QTB-SERCHG,
                   :QTB-EXTCHG,
                   :QTB-MAINTFEE,
                   :QTB-ORGTERM,
                   :QTB-INSURED, 
                   :QTB-DLNO,
                   :QTB-DLRV,
                   :QTB-DLSPRV,
                   :QTB-DLDISC,
                   :QTB-DLPARV,
                   :QTB-DLRECOURSE, 
                   :QTB-DLPARTRCAMT,
                   :QTB-PURBAL,
                   :QTB-UNEARN-1,
                   :QTB-UNEARN-2,
                   :QTB-UNEARN-3,
                   :QTB-UNEARN-4,
                   :QTB-UNEARN-5,
                   :QTB-UNEARN-6,
                   :QTB-UNEARN-7,
                   :QTB-UNEARN-8,
                   :QTB-UNEARN-9,
                   :QTB-UNEARN-10,
                   :QTB-REF-ACCTNO,
                   :QTB-DLRVPRIN-1,
                   :QTB-DLRVPRIN-2,
                   :QTB-DLRVPRIN-3,
                   :QTB-DLRVCHGS-1,
                   :QTB-DLRVCHGS-2,
                   :QTB-DLRVCHGS-3,
                   :QTB-DLPARVPAID,
                   :QTB-DLPARVHELD,
                   :QTB-USERID, 
                   :QTB-POSTDATE, 
                   :QTB-POSTTIME,
                   :QTB-PLCD,
                   :QTB-PLTYPE,
                   :QTB-REPOCD,
                   :QTB-ACTIONCD2
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRB1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRB1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRB1-CURSOR-STAT.

      *-----------------------------------------------------------------

      * COPY "LIBLP/LPTRC1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRC1RN
       LOAD-TRC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRC-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRC-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRC1-FILE. *> SSP - TRCFILE
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           STRING  "SELECT TRCFILE.TC_BRNO,"
                   "CAST(TRCFILE.TC_KEY_DATE  AS VARCHAR(10))," 
                   "TRCFILE.TC_CKNO,"
                   "TRCFILE.TC_CLASS,"
                   "TRCFILE.TC_SEQ,"
                   "TRCFILE.TC_TYPE," 
                   "CAST(TRCFILE.TC_DATE      AS VARCHAR(10))," 
                   "TRCFILE.TC_AMT,"
                   "TRCFILE.TC_PAYEE," 
                   "TRCFILE.TC_REF,"
                   "TRCFILE.TC_REASON," 
                   "TRCFILE.TC_GLNO_1,"
                   "TRCFILE.TC_GLNO_2,"
                   "TRCFILE.TC_GLNO_3,"
                   "TRCFILE.TC_GLNO_4,"
                   "TRCFILE.TC_GLNO_5,"
                   "TRCFILE.TC_GLAMT_1,"
                   "TRCFILE.TC_GLAMT_2,"
                   "TRCFILE.TC_GLAMT_3,"
                   "TRCFILE.TC_GLAMT_4,"
                   "TRCFILE.TC_GLAMT_5,"
                   "TRCFILE.TC_VOID,"
                   "TRCFILE.TC_ACCTNO,"
                   "TRCFILE.TC_PAID,"
                   "TRCFILE.TC_PGM,"
                   "TRCFILE.TC_DLRVNO,"
                   "TRCFILE.TC_SPRCLASS,"
                   "TRCFILE.TC_DLNO,"
                   "TRCFILE.TC_USERID," 
                   "CAST(TRCFILE.TC_POSTDATE  AS VARCHAR(10))," 
                   "TRCFILE.TC_POSTTIME,"
                   "TRCFILE.TC_TILLNO,"
                   "TRCFILE.TC_EXTRACT_FG," 
                   "TRCFILE.TC_DEBIT_REFCD"
                  " FROM DBO.TRCFILE | " INTO WS-SELECT.

           STRING "WHERE TRCFILE.TC_BRNO = ",
                    QTRC1-WBEG-BRNO
                   " AND TRCFILE.TC_KEY_DATE BETWEEN '",
                    QTRC1-WBEG-KEY-DATE
                   " ' AND '",
                    QTRC1-WEND-KEY-DATE,
                  "' AND TRCFILE.TC_CLASS BETWEEN " ,
                   QTRC1-WBEG-CLASS ,
                  " AND " ,
                   QTRC1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING " ORDER BY TRCFILE.TC_BRNO,"
                  " TRCFILE.TC_KEY_DATE,"
                  " TRCFILE.TC_CKNO,"
                  " TRCFILE.TC_CLASS,"
                  " TRCFILE.TC_SEQ |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
                :TRC1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRC1-WBEG-BRNO.
           MOVE SPACES TO QTRC1-WBEG-KEY-DATE
                          QTRC1-WEND-KEY-DATE.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.
 
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT. *> EMBEDDED - TRCFILE
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                 FETCH TRC1_CURSOR INTO  
                   :QTC-BRNO,
                   :QTC-KEY-DATE, 
                   :QTC-CKNO,
                   :QTC-CLASS,
                   :QTC-SEQ,
                   :QTC-TYPE, 
                   :QTC-DATE, 
                   :QTC-AMT,
                   :QTC-PAYEE, 
                   :QTC-REF,
                   :QTC-REASON, 
                   :QTC-GLNO-1,
                   :QTC-GLNO-2,
                   :QTC-GLNO-3,
                   :QTC-GLNO-4,
                   :QTC-GLNO-5,
                   :QTC-GLAMT-1,
                   :QTC-GLAMT-2,
                   :QTC-GLAMT-3,
                   :QTC-GLAMT-4,
                   :QTC-GLAMT-5,
                   :QTC-VOID,
                   :QTC-ACCTNO,
                   :QTC-PAID,
                   :QTC-PGM,
                   :QTC-DLRVNO,
                   :QTC-SPRCLASS,
                   :QTC-DLNO,
                   :QTC-USERID, 
                   :QTC-POSTDATE, 
                   :QTC-POSTTIME,
                   :QTC-TILLNO,
                   :QTC-EXTRACT-FG, 
                   :QTC-DEBIT-REFCD 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRC1-CURSOR-STAT.
           
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRD1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRD1RN
       LOAD-TRD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRD-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRD-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH.

      *-----------------------------------------------------------------
       OPEN-TRD1-FILE.
           PERFORM LOAD-TRD1-FILE.
           PERFORM OPEN-IT.
           MOVE TRD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRD1-FILE. *> SSP - TRDFILE
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD1-FILE.

           STRING "SELECT TRDFILE.TD_BRNO,"
                  "CAST(TRDFILE.TD_DATE      AS VARCHAR(10))," 
                  "TRDFILE.TD_DEPOSIT,"
                  "TRDFILE.TD_BANK,"
                  "TRDFILE.TD_CASH,"
                  "TRDFILE.TD_GLFG," 
                  "TRDFILE.TD_GLOVERSHORT,"
                  "TRDFILE.TD_OVERSHORT,"
                  "TRDFILE.TD_USERID," 
                  "CAST(TRDFILE.TD_POSTDATE  AS VARCHAR(10))," 
                  "TRDFILE.TD_POSTTIME"
                  " FROM DBO.TRDFILE | " INTO WS-SELECT.

           STRING " WHERE TRDFILE.TD_BRNO = ",
                   QTRD1-WBEG-BRNO ,
                  " AND TRDFILE.TD_DATE BETWEEN '",
                   QTRD1-WBEG-DATE ,  
                  " ' AND '" ,
                   QTRD1-WEND-DATE ,
                  "' | " INTO WS-WHERE.
            
           STRING " ORDER BY TRDFILE.TD_BRNO,"
                  " TRDFILE.TD_DATE |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED  BY "|",
                  WS-WHERE  DELIMITED  BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRD1_CURSOR CURSOR FOR
                :TRD1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD1-WBEG-BRNO.
           MOVE SPACES TO QTRD1-WBEG-DATE
                          QTRD1-WEND-DATE.

           MOVE STAT---GOOD TO TRD1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRD1-FILE-NEXT. *> EMBEDDED - TRDFILE
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                 FETCH TRD1_CURSOR INTO  
                   :QTD-BRNO,         
                   :QTD-DATE,     
                   :QTD-DEPOSIT,
                   :QTD-BANK,
                   :QTD-CASH,
                   :QTD-GLFG, 
                   :QTD-GLOVERSHORT,
                   :QTD-OVERSHORT,
                   :QTD-USERID, 
                   :QTD-POSTDATE, 
                   :QTD-POSTTIME
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRD1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRD1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRD1-CURSOR-STAT.

      *-----------------------------------------------------------------
  
  
      * COPY "LIBLP/LPTRJ1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRJ1RN
       LOAD-TRJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH.

      *-----------------------------------------------------------------
       OPEN-TRJ1-FILE.
           PERFORM LOAD-TRJ1-FILE.
           PERFORM OPEN-IT.
           MOVE TRJ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRJ1-FILE. *> SSP - TRJFILE
           PERFORM START-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRJ1-FILE.

           STRING "SELECT TRJFILE.TJ_BRNO,"
                  "CAST(TRJFILE.TJ_DATE AS VARCHAR(10))," 
                  "TRJFILE.TJ_CLASS,"
                  "TRJFILE.TJ_ACCTNO,"
                  "TRJFILE.TJ_LAWCODE,"
                  "TRJFILE.TJ_LINE,"
                  "TRJFILE.TJ_SSNO_1,"
                  "TRJFILE.TJ_SSNO_2,"
                  "TRJFILE.TJ_SSNO_3,"
                  "TRJFILE.TJ_SSNO_4,"
                  "TRJFILE.TJ_MAKERCD_1," 
                  "TRJFILE.TJ_MAKERCD_2," 
                  "TRJFILE.TJ_MAKERCD_3," 
                  "TRJFILE.TJ_MAKERCD_4," 
                  "TRJFILE.TJ_REF_FRLC,"
                  "TRJFILE.TJ_REF_X," 
                  "TRJFILE.TJ_REF_TOLC,"
                  "TRJFILE.TJ_PURCD," 
                  "TRJFILE.TJ_SRCD," 
                  "TRJFILE.TJ_COLLCD," 
                  "TRJFILE.TJ_SERVACCT," 
                  "TRJFILE.TJ_LOANTYPE," 
                  "CAST(TRJFILE.TJ_LOANDATE AS VARCHAR(10))," 
                  "TRJFILE.TJ_PROCEEDS,"
                  "TRJFILE.TJ_LNAMT,"
                  "TRJFILE.TJ_FINCHG,"
                  "TRJFILE.TJ_INTCHG,"
                  "TRJFILE.TJ_SERCHG,"
                  "TRJFILE.TJ_EXTCHG,"
                  "TRJFILE.TJ_MAINTFEE,"
                  "TRJFILE.TJ_ORGTERM,"
                  "TRJFILE.TJ_INSURED,"
                  "TRJFILE.TJ_DLNO,"
                  "TRJFILE.TJ_DLRV,"
                  "TRJFILE.TJ_DLSPRV,"
                  "TRJFILE.TJ_DLDISC,"
                  "TRJFILE.TJ_DLPARV,"
                  "TRJFILE.TJ_DLRECOURSE," 
                  "TRJFILE.TJ_DLPARTRCAMT,"
                  "TRJFILE.TJ_PURBAL,"
                  "TRJFILE.TJ_UNEARN_1,"
                  "TRJFILE.TJ_UNEARN_2,"
                  "TRJFILE.TJ_UNEARN_3,"
                  "TRJFILE.TJ_UNEARN_4,"
                  "TRJFILE.TJ_UNEARN_5,"
                  "TRJFILE.TJ_UNEARN_6,"
                  "TRJFILE.TJ_UNEARN_7,"
                  "TRJFILE.TJ_UNEARN_8,"
                  "TRJFILE.TJ_UNEARN_9,"
                  "TRJFILE.TJ_UNEARN_10,"
                  "TRJFILE.TJ_PRLNNO,"
                  "TRJFILE.TJ_DLRVPRIN_1,"
                  "TRJFILE.TJ_DLRVPRIN_2,"
                  "TRJFILE.TJ_DLRVPRIN_3,"
                  "TRJFILE.TJ_DLRVCHGS_1,"
                  "TRJFILE.TJ_DLRVCHGS_2,"
                  "TRJFILE.TJ_DLRVCHGS_3,"
                  "TRJFILE.TJ_DLPARVPAID,"
                  "TRJFILE.TJ_DLPARVHELD,"
                  "TRJFILE.TJ_USERID," 
                  "CAST(TRJFILE.TJ_POSTDATE AS VARCHAR(10))," 
                  "TRJFILE.TJ_POSTTIME,"
                  "TRJFILE.TJ_PLCD," 
                  "TRJFILE.TJ_PLTYPE,"
                  "TRJFILE.TJ_REPOCD" 
                   " FROM DBO.TRJFILE | " INTO WS-SELECT.

           STRING " WHERE TRJFILE.TJ_BRNO = ",
                    QTRJ1-WBEG-BRNO ,
                  " AND TRJFILE.TJ_DATE BETWEEN '" ,
                    QTRJ1-WBEG-DATE ,
                   "' AND '",
                    QTRJ1-WEND-DATE ,
                  "' AND TRJFILE.TJ_CLASS BETWEEN " ,
                   QTRJ1-WBEG-CLASS ,
                  " AND " ,
                   QTRJ1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING " ORDER BY TRJFILE.TJ_BRNO,"
                  " TRJFILE.TJ_DATE,"
                  " TRJFILE.TJ_CLASS,"
                  " TRJFILE.TJ_ACCTNO,"
                  " TRJFILE.TJ_LAWCODE,"
                  " TRJFILE.TJ_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT  DELIMITED BY "|",
                  WS-WHERE   DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRJ1_CURSOR CURSOR FOR
                :TRJ1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRJ1-WBEG-BRNO.
           MOVE SPACES TO QTRJ1-WBEG-DATE
                          QTRJ1-WEND-DATE.

           MOVE STAT---GOOD TO TRJ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRJ1-FILE-NEXT. *> EMBEDDED - TRJFILE
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           INITIALIZE QTRJ-REC.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                  FETCH TRJ1_CURSOR INTO
                   :QTJ-BRNO,
                   :QTJ-DATE, 
                   :QTJ-CLASS,
                   :QTJ-ACCTNO,
                   :QTJ-LAWCODE,
                   :QTJ-LINE,
                   :QTJ-SSNO-1,
                   :QTJ-SSNO-2,
                   :QTJ-SSNO-3,
                   :QTJ-SSNO-4,
                   :QTJ-MAKERCD-1, 
                   :QTJ-MAKERCD-2, 
                   :QTJ-MAKERCD-3, 
                   :QTJ-MAKERCD-4, 
                   :QTJ-REF-FRLC,
                   :QTJ-REF-X, 
                   :QTJ-REF-TOLC,
                   :QTJ-PURCD, 
                   :QTJ-SRCD, 
                   :QTJ-COLLCD, 
                   :QTJ-SERVACCT, 
                   :QTJ-LOANTYPE, 
                   :QTJ-LOANDATE, 
                   :QTJ-PROCEEDS,
                   :QTJ-LNAMT,
                   :QTJ-FINCHG,
                   :QTJ-INTCHG,
                   :QTJ-SERCHG,
                   :QTJ-EXTCHG,
                   :QTJ-MAINTFEE,
                   :QTJ-ORGTERM,
                   :QTJ-INSURED, 
                   :QTJ-DLNO,
                   :QTJ-DLRV,
                   :QTJ-DLSPRV,
                   :QTJ-DLDISC,
                   :QTJ-DLPARV,
                   :QTJ-DLRECOURSE, 
                   :QTJ-DLPARTRCAMT,
                   :QTJ-PURBAL,
                   :QTJ-UNEARN-1,
                   :QTJ-UNEARN-2,
                   :QTJ-UNEARN-3,
                   :QTJ-UNEARN-4,
                   :QTJ-UNEARN-5,
                   :QTJ-UNEARN-6,
                   :QTJ-UNEARN-7,
                   :QTJ-UNEARN-8,
                   :QTJ-UNEARN-9,
                   :QTJ-UNEARN-10,
                   :QTJ-PRLNNO,
                   :QTJ-DLRVPRIN-1,
                   :QTJ-DLRVPRIN-2,
                   :QTJ-DLRVPRIN-3,
                   :QTJ-DLRVCHGS-1,
                   :QTJ-DLRVCHGS-2,
                   :QTJ-DLRVCHGS-3,
                   :QTJ-DLPARVPAID,
                   :QTJ-DLPARVHELD,
                   :QTJ-USERID, 
                   :QTJ-POSTDATE, 
                   :QTJ-POSTTIME,
                   :QTJ-PLCD, 
                   :QTJ-PLTYPE,
                   :QTJ-REPOCD 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRJ1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRJ1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
  
  
      * COPY "LIBLP/LPTRK1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRK1RN
       LOAD-TRK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRK-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRK-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH.

      *-----------------------------------------------------------------
       OPEN-TRK1-FILE.
           PERFORM LOAD-TRK1-FILE.
           PERFORM OPEN-IT.
           MOVE TRK-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRK1-FILE. *> SSP - TRKFILE
           PERFORM START-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRK1-FILE.

           STRING "SELECT TRKFILE.TK_BRNO,"
                  "CAST(TRKFILE.TK_DATE AS VARCHAR(10)),"
                  "TRKFILE.TK_CLASS,"
                  "TRKFILE.TK_ACCTNO,"
                  "TRKFILE.TK_LAWCODE,"
                  "TRKFILE.TK_LINE,"
                  "TRKFILE.TK_SSNO_1,"
                  "TRKFILE.TK_SSNO_2,"
                  "TRKFILE.TK_SSNO_3,"
                  "TRKFILE.TK_SSNO_4,"
                  "TRKFILE.TK_MAKERCD_1," 
                  "TRKFILE.TK_MAKERCD_2," 
                  "TRKFILE.TK_MAKERCD_3," 
                  "TRKFILE.TK_MAKERCD_4," 
                  "TRKFILE.TK_REF_FRTO,"
                  "TRKFILE.TK_REFBR,"
                  "TRKFILE.TK_PURCD," 
                  "TRKFILE.TK_SRCD," 
                  "TRKFILE.TK_COLLCD," 
                  "TRKFILE.TK_SERVACCT," 
                  "TRKFILE.TK_LOANTYPE," 
                  "TRKFILE.TK_REPOCD," 
                  "TRKFILE.TK_PLCD," 
                  "TRKFILE.TK_PLTYPE," 
                  "CAST(TRKFILE.TK_LOANDATE AS VARCHAR(10))," 
                  "TRKFILE.TK_PROCEEDS,"
                  "TRKFILE.TK_LNAMT,"
                  "TRKFILE.TK_FINCHG,"
                  "TRKFILE.TK_INTCHG,"
                  "TRKFILE.TK_SERCHG,"
                  "TRKFILE.TK_EXTCHG,"
                  "TRKFILE.TK_MAINTFEE,"
                  "TRKFILE.TK_ORGTERM,"
                  "TRKFILE.TK_INSURED," 
                  "TRKFILE.TK_DLNO,"
                  "TRKFILE.TK_DLRV,"
                  "TRKFILE.TK_DLSPRV,"
                  "TRKFILE.TK_DLDISC,"
                  "TRKFILE.TK_DLPARV,"
                  "TRKFILE.TK_DLRECOURSE," 
                  "TRKFILE.TK_DLPARTRCAMT,"
                  "TRKFILE.TK_PURBAL,"
                  "TRKFILE.TK_UNEARN_1,"
                  "TRKFILE.TK_UNEARN_2,"
                  "TRKFILE.TK_UNEARN_3,"
                  "TRKFILE.TK_UNEARN_4,"
                  "TRKFILE.TK_UNEARN_5,"
                  "TRKFILE.TK_UNEARN_6,"
                  "TRKFILE.TK_UNEARN_7,"
                  "TRKFILE.TK_UNEARN_8,"
                  "TRKFILE.TK_UNEARN_9,"
                  "TRKFILE.TK_UNEARN_10,"
                  "TRKFILE.TK_PRLNNO,"
                  "TRKFILE.TK_DLRVPRIN_1,"
                  "TRKFILE.TK_DLRVPRIN_2,"
                  "TRKFILE.TK_DLRVPRIN_3,"
                  "TRKFILE.TK_DLRVCHGS_1,"
                  "TRKFILE.TK_DLRVCHGS_2,"
                  "TRKFILE.TK_DLRVCHGS_3,"
                  "TRKFILE.TK_DLPARVPAID,"
                  "TRKFILE.TK_DLPARVHELD,"
                  "TRKFILE.TK_USERID," 
                  "CAST(TRKFILE.TK_POSTDATE AS VARCHAR(10))," 
                  "TRKFILE.TK_POSTTIME"
                  " FROM DBO.TRKFILE | " INTO WS-SELECT.

           STRING " WHERE TRKFILE.TK_BRNO = " ,
                   QTRK1-WBEG-BRNO ,
                  " AND TRKFILE.TK_DATE BETWEEN '" ,
                   QTRK1-WBEG-DATE , 
                  "' AND '" ,
                   QTRK1-WEND-DATE ,
                  "' AND TRKFILE.TK_CLASS BETWEEN " ,
                   QTRK1-WBEG-CLASS ,
                  " AND " ,
                   QTRK1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING " ORDER BY TRKFILE.TK_BRNO,"
                  "   TRKFILE.TK_DATE,"
                  "   TRKFILE.TK_CLASS,"
                  "   TRKFILE.TK_ACCTNO,"
                  "   TRKFILE.TK_LAWCODE,"
                  "   TRKFILE.TK_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT  DELIMITED BY "|",
                  WS-WHERE   DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRK1_CURSOR CURSOR FOR
                :TRK1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRK1-WBEG-BRNO.
           MOVE SPACES TO QTRK1-WBEG-DATE
                          QTRK1-WEND-DATE.

           MOVE STAT---GOOD TO TRK1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRK1-FILE-NEXT. *> EMBEDDED - TRKFILE
           PERFORM READ-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           INITIALIZE QTRK-REC.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   FETCH TRK1_CURSOR INTO  
                   :QTK-BRNO, 
                   :QTK-DATE,
                   :QTK-CLASS,
                   :QTK-ACCTNO,
                   :QTK-LAWCODE,
                   :QTK-LINE,
                   :QTK-SSNO-1,
                   :QTK-SSNO-2,
                   :QTK-SSNO-3,
                   :QTK-SSNO-4,
                   :QTK-MAKERCD-1, 
                   :QTK-MAKERCD-2, 
                   :QTK-MAKERCD-3, 
                   :QTK-MAKERCD-4, 
                   :QTK-REF-FRTO,
                   :QTK-REFBR,
                   :QTK-PURCD, 
                   :QTK-SRCD, 
                   :QTK-COLLCD, 
                   :QTK-SERVACCT, 
                   :QTK-LOANTYPE, 
                   :QTK-REPOCD, 
                   :QTK-PLCD, 
                   :QTK-PLTYPE, 
                   :QTK-LOANDATE, 
                   :QTK-PROCEEDS,
                   :QTK-LNAMT,
                   :QTK-FINCHG,
                   :QTK-INTCHG,
                   :QTK-SERCHG,
                   :QTK-EXTCHG,
                   :QTK-MAINTFEE,
                   :QTK-ORGTERM,
                   :QTK-INSURED, 
                   :QTK-DLNO,
                   :QTK-DLRV,
                   :QTK-DLSPRV,
                   :QTK-DLDISC,
                   :QTK-DLPARV,
                   :QTK-DLRECOURSE, 
                   :QTK-DLPARTRCAMT,
                   :QTK-PURBAL,
                   :QTK-UNEARN-1,
                   :QTK-UNEARN-2,
                   :QTK-UNEARN-3,
                   :QTK-UNEARN-4,
                   :QTK-UNEARN-5,
                   :QTK-UNEARN-6,
                   :QTK-UNEARN-7,
                   :QTK-UNEARN-8,
                   :QTK-UNEARN-9,
                   :QTK-UNEARN-10,
                   :QTK-PRLNNO,
                   :QTK-DLRVPRIN-1,
                   :QTK-DLRVPRIN-2,
                   :QTK-DLRVPRIN-3,
                   :QTK-DLRVCHGS-1,
                   :QTK-DLRVCHGS-2,
                   :QTK-DLRVCHGS-3,
                   :QTK-DLPARVPAID,
                   :QTK-DLPARVHELD,
                   :QTK-USERID, 
                   :QTK-POSTDATE, 
                   :QTK-POSTTIME
                 END-EXEC
              ELSE
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRK-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRK1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRK1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRK1-CURSOR-STAT.

      *-----------------------------------------------------------------
  
  
      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRL1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE. *> SSP - TRLFILE
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.
 
           STRING "SELECT TRLFILE.TL_BRNO,"
                  "CAST(TRLFILE.TL_DATE AS VARCHAR(10))," 
                  "TRLFILE.TL_CLASS,"
                  "TRLFILE.TL_ACCTNO,"
                  "TRLFILE.TL_LAWCODE,"
                  "TRLFILE.TL_LINE,"
                  "TRLFILE.TL_SSNO_1,"
                  "TRLFILE.TL_SSNO_2,"
                  "TRLFILE.TL_SSNO_3,"
                  "TRLFILE.TL_SSNO_4,"
                  "TRLFILE.TL_MAKERCD_1," 
                  "TRLFILE.TL_MAKERCD_2," 
                  "TRLFILE.TL_MAKERCD_3," 
                  "TRLFILE.TL_MAKERCD_4," 
                  "TRLFILE.TL_PURCD," 
                  "TRLFILE.TL_SRCD," 
                  "TRLFILE.TL_COLLCD," 
                  "TRLFILE.TL_SERVACCT," 
                  "TRLFILE.TL_LOANTYPE," 
                  "CAST(TRLFILE.TL_LOANDATE AS VARCHAR(10)),"
                  "CAST(TRLFILE.TL_INTDATE  AS VARCHAR(10))," 
                  "TRLFILE.TL_PROCEEDS,"
                  "TRLFILE.TL_LNAMT,"
                  "TRLFILE.TL_FINCHG,"
                  "TRLFILE.TL_INTCHG,"
                  "TRLFILE.TL_SERCHG,"
                  "TRLFILE.TL_EXTCHG,"
                  "TRLFILE.TL_MAINTFEE,"
                  "TRLFILE.TL_ORGTERM,"
                  "TRLFILE.TL_1STPYAMT,"
                  "TRLFILE.TL_NOREGPY,"
                  "TRLFILE.TL_REGPYAMT,"
                  "TRLFILE.TL_LASTPYAMT,"
                  "TRLFILE.TL_PROCASH,"
                  "TRLFILE.TL_TILLNO,"
                  "TRLFILE.TL_FEECODE_1," 
                  "TRLFILE.TL_FEECODE_2," 
                  "TRLFILE.TL_FEECODE_3," 
                  "TRLFILE.TL_FEECODE_4," 
                  "TRLFILE.TL_FEECODE_5," 
                  "TRLFILE.TL_FEECODE_6," 
                  "TRLFILE.TL_FEECODE_7," 
                  "TRLFILE.TL_FEECODE_8," 
                  "TRLFILE.TL_FEECODE_9," 
                  "TRLFILE.TL_FEECODE_10," 
                  "TRLFILE.TL_FEECODE_11," 
                  "TRLFILE.TL_FEECODE_12," 
                  "TRLFILE.TL_FEECODE_13," 
                  "TRLFILE.TL_FEECODE_14," 
                  "TRLFILE.TL_FEECODE_15," 
                  "TRLFILE.TL_FEECODE_16," 
                  "TRLFILE.TL_FEECODE_17," 
                  "TRLFILE.TL_FEECODE_18," 
                  "TRLFILE.TL_FEECODE_19," 
                  "TRLFILE.TL_FEECODE_20," 
                  "TRLFILE.TL_FEEAMT_1,"
                  "TRLFILE.TL_FEEAMT_2,"
                  "TRLFILE.TL_FEEAMT_3,"
                  "TRLFILE.TL_FEEAMT_4,"
                  "TRLFILE.TL_FEEAMT_5,"
                  "TRLFILE.TL_FEEAMT_6,"
                  "TRLFILE.TL_FEEAMT_7,"
                  "TRLFILE.TL_FEEAMT_8,"
                  "TRLFILE.TL_FEEAMT_9,"
                  "TRLFILE.TL_FEEAMT_10,"
                  "TRLFILE.TL_FEEAMT_11,"
                  "TRLFILE.TL_FEEAMT_12,"
                  "TRLFILE.TL_FEEAMT_13,"
                  "TRLFILE.TL_FEEAMT_14,"
                  "TRLFILE.TL_FEEAMT_15,"
                  "TRLFILE.TL_FEEAMT_16,"
                  "TRLFILE.TL_FEEAMT_17,"
                  "TRLFILE.TL_FEEAMT_18,"
                  "TRLFILE.TL_FEEAMT_19,"
                  "TRLFILE.TL_FEEAMT_20,"
                  "TRLFILE.TL_INSURED," 
                  "TRLFILE.TL_INSTYPES," 
                  "TRLFILE.TL_INSELIGIBLE," 
                  "TRLFILE.TL_INSCOMP_1,"
                  "TRLFILE.TL_INSCOMP_2,"
                  "TRLFILE.TL_INSCOMP_3,"
                  "TRLFILE.TL_INSCOMP_4,"
                  "TRLFILE.TL_INSCOMP_5,"
                  "TRLFILE.TL_INSCOMP_6,"
                  "TRLFILE.TL_INSCOMP_7,"
                  "TRLFILE.TL_INSCOMP_8,"
                  "TRLFILE.TL_INSCOMP_9,"
                  "TRLFILE.TL_INSCOMP_10,"
                  "TRLFILE.TL_INSOURS_1," 
                  "TRLFILE.TL_INSOURS_2," 
                  "TRLFILE.TL_INSOURS_3," 
                  "TRLFILE.TL_INSOURS_4," 
                  "TRLFILE.TL_INSOURS_5," 
                  "TRLFILE.TL_INSOURS_6," 
                  "TRLFILE.TL_INSOURS_7," 
                  "TRLFILE.TL_INSOURS_8," 
                  "TRLFILE.TL_INSOURS_9," 
                  "TRLFILE.TL_INSOURS_10," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_1  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_2  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_3  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_4  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_5  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_6  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_7  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_8  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_9  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEFF_DATE_10 AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_1  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_2  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_3  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_4  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_5  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_6  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_7  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_8  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_9  AS VARCHAR(10))," 
                  "CAST(TRLFILE.TL_INSEXP_DATE_10 AS VARCHAR(10)),"  
                  "TRLFILE.TL_INSPREM_1,"
                  "TRLFILE.TL_INSPREM_2,"
                  "TRLFILE.TL_INSPREM_3,"
                  "TRLFILE.TL_INSPREM_4,"
                  "TRLFILE.TL_INSPREM_5,"
                  "TRLFILE.TL_INSPREM_6,"
                  "TRLFILE.TL_INSPREM_7,"
                  "TRLFILE.TL_INSPREM_8,"
                  "TRLFILE.TL_INSPREM_9,"
                  "TRLFILE.TL_INSPREM_10,"
                  "TRLFILE.TL_INSCOMM_1,"
                  "TRLFILE.TL_INSCOMM_2,"
                  "TRLFILE.TL_INSCOMM_3,"
                  "TRLFILE.TL_INSCOMM_4,"
                  "TRLFILE.TL_INSCOMM_5,"
                  "TRLFILE.TL_INSCOMM_6,"
                  "TRLFILE.TL_INSCOMM_7,"
                  "TRLFILE.TL_INSCOMM_8,"
                  "TRLFILE.TL_INSCOMM_9,"
                  "TRLFILE.TL_INSCOMM_10,"
                  "TRLFILE.TL_DLNO,"
                  "TRLFILE.TL_EARNINGS_1,"
                  "TRLFILE.TL_EARNINGS_2,"
                  "TRLFILE.TL_EARNINGS_3,"
                  "TRLFILE.TL_EARNINGS_4,"
                  "TRLFILE.TL_EARNINGS_5,"
                  "TRLFILE.TL_EARNINGS_6,"
                  "TRLFILE.TL_EARNINGS_7,"
                  "TRLFILE.TL_EARNINGS_8,"
                  "TRLFILE.TL_EARNINGS_9,"
                  "TRLFILE.TL_EARNINGS_10,"
                  "TRLFILE.TL_ANTICERN_1,"
                  "TRLFILE.TL_ANTICERN_2,"
                  "TRLFILE.TL_ANTICERN_3,"
                  "TRLFILE.TL_ANTICERN_4,"
                  "TRLFILE.TL_ANTICERN_5,"
                  "TRLFILE.TL_ANTICERN_6,"
                  "TRLFILE.TL_ANTICERN_7,"
                  "TRLFILE.TL_ANTICERN_8,"
                  "TRLFILE.TL_ANTICERN_9,"
                  "TRLFILE.TL_ANTICERN_10,"
                  "TRLFILE.TL_PRLNNO,"
                  "TRLFILE.TL_PRBAL,"
                  "TRLFILE.TL_PRINTDUE,"
                  "TRLFILE.TL_PRFINCHGRB,"
                  "TRLFILE.TL_PRINSRB,"
                  "TRLFILE.TL_LPEOD5_FG," 
                  "TRLFILE.TL_STATE_WORKER,"
                  "TRLFILE.TL_RESTRICTD_ACCTNO,"
                  "TRLFILE.TL_SPRCLASS,"
                  "TRLFILE.TL_USERID," 
                  "CAST(TRLFILE.TL_POSTDATE AS VARCHAR(10))," 
                  "TRLFILE.TL_POSTTIME,"
                  "TRLFILE.TL_GIFTNET," 
                  "TRLFILE.TL_APR,"
                  "TRLFILE.TL_SP_LNCOST_FRMLA," 
                  "TRLFILE.TL_MIP_FG," 
                  "TRLFILE.TL_INS_TAX_COUNTY_CD," 
                  "TRLFILE.TL_INS_TAX_CITY_CD," 
                  "TRLFILE.TL_PRDLDISC_UNEARN,"
                  "TRLFILE.TL_POINTS,"
                  "TRLFILE.TL_DEBIT_CARD_AMT,"
                  "TRLFILE.TL_ACTIONCD2" 
                  " FROM DBO.TRLFILE | " INTO WS-SELECT.

           STRING " WHERE TRLFILE.TL_BRNO = " ,
                    QTRL1-WBEG-BRNO ,
                  " AND TRLFILE.TL_DATE BETWEEN '" ,
                    QTRL1-WBEG-DATE ,
                  " ' AND '" ,
                    QTRL1-WEND-DATE ,
                  "' AND TRLFILE.TL_CLASS BETWEEN " ,
                    QTRL1-WBEG-CLASS ,
                  " AND " ,
                    QTRL1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING  " ORDER BY TRLFILE.TL_BRNO,"
                   "  TRLFILE.TL_DATE,"
                   "  TRLFILE.TL_CLASS,"
                   "  TRLFILE.TL_ACCTNO,"
                   "  TRLFILE.TL_LAWCODE,"
                   "  TRLFILE.TL_LINE  |" INTO WS-ORDERBY.

           STRING WS-SELECT  DELIMITED BY "|",
                  WS-WHERE   DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :TRL1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.

           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT. *> EMBEDDED - TRLFILE
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                FETCH TRL1_CURSOR INTO  
                  :QTL-BRNO, 
                  :QTL-DATE, 
                  :QTL-CLASS,
                  :QTL-ACCTNO,
                  :QTL-LAWCODE,
                  :QTL-LINE,
                  :QTL-SSNO-1,
                  :QTL-SSNO-2,
                  :QTL-SSNO-3,
                  :QTL-SSNO-4,
                  :QTL-MAKERCD-1, 
                  :QTL-MAKERCD-2, 
                  :QTL-MAKERCD-3, 
                  :QTL-MAKERCD-4, 
                  :QTL-PURCD, 
                  :QTL-SRCD, 
                  :QTL-COLLCD, 
                  :QTL-SERVACCT, 
                  :QTL-LOANTYPE, 
                  :QTL-LOANDATE, 
                  :QTL-INTDATE, 
                  :QTL-PROCEEDS,
                  :QTL-LNAMT,
                  :QTL-FINCHG,
                  :QTL-INTCHG,
                  :QTL-SERCHG,
                  :QTL-EXTCHG,
                  :QTL-MAINTFEE,
                  :QTL-ORGTERM,
                  :QTL-1STPYAMT,
                  :QTL-NOREGPY,
                  :QTL-REGPYAMT,
                  :QTL-LASTPYAMT,
                  :QTL-PROCASH,
                  :QTL-TILLNO,
                  :QTL-FEECODE-1, 
                  :QTL-FEECODE-2, 
                  :QTL-FEECODE-3, 
                  :QTL-FEECODE-4, 
                  :QTL-FEECODE-5, 
                  :QTL-FEECODE-6, 
                  :QTL-FEECODE-7, 
                  :QTL-FEECODE-8, 
                  :QTL-FEECODE-9, 
                  :QTL-FEECODE-10, 
                  :QTL-FEECODE-11, 
                  :QTL-FEECODE-12, 
                  :QTL-FEECODE-13, 
                  :QTL-FEECODE-14, 
                  :QTL-FEECODE-15, 
                  :QTL-FEECODE-16, 
                  :QTL-FEECODE-17, 
                  :QTL-FEECODE-18, 
                  :QTL-FEECODE-19, 
                  :QTL-FEECODE-20, 
                  :QTL-FEEAMT-1,
                  :QTL-FEEAMT-2,
                  :QTL-FEEAMT-3,
                  :QTL-FEEAMT-4,
                  :QTL-FEEAMT-5,
                  :QTL-FEEAMT-6,
                  :QTL-FEEAMT-7,
                  :QTL-FEEAMT-8,
                  :QTL-FEEAMT-9,
                  :QTL-FEEAMT-10,
                  :QTL-FEEAMT-11,
                  :QTL-FEEAMT-12,
                  :QTL-FEEAMT-13,
                  :QTL-FEEAMT-14,
                  :QTL-FEEAMT-15,
                  :QTL-FEEAMT-16,
                  :QTL-FEEAMT-17,
                  :QTL-FEEAMT-18,
                  :QTL-FEEAMT-19,
                  :QTL-FEEAMT-20,
                  :QTL-INSURED, 
                  :QTL-INSTYPES, 
                  :QTL-INSELIGIBLE, 
                  :QTL-INSCOMP-1,
                  :QTL-INSCOMP-2,
                  :QTL-INSCOMP-3,
                  :QTL-INSCOMP-4,
                  :QTL-INSCOMP-5,
                  :QTL-INSCOMP-6,
                  :QTL-INSCOMP-7,
                  :QTL-INSCOMP-8,
                  :QTL-INSCOMP-9,
                  :QTL-INSCOMP-10,
                  :QTL-INSOURS-1, 
                  :QTL-INSOURS-2, 
                  :QTL-INSOURS-3, 
                  :QTL-INSOURS-4, 
                  :QTL-INSOURS-5, 
                  :QTL-INSOURS-6, 
                  :QTL-INSOURS-7, 
                  :QTL-INSOURS-8, 
                  :QTL-INSOURS-9, 
                  :QTL-INSOURS-10, 
                  :QTL-INSEFF-DATE-1, 
                  :QTL-INSEFF-DATE-2, 
                  :QTL-INSEFF-DATE-3, 
                  :QTL-INSEFF-DATE-4, 
                  :QTL-INSEFF-DATE-5, 
                  :QTL-INSEFF-DATE-6, 
                  :QTL-INSEFF-DATE-7, 
                  :QTL-INSEFF-DATE-8, 
                  :QTL-INSEFF-DATE-9, 
                  :QTL-INSEFF-DATE-10, 
                  :QTL-INSEXP-DATE-1, 
                  :QTL-INSEXP-DATE-2, 
                  :QTL-INSEXP-DATE-3, 
                  :QTL-INSEXP-DATE-4, 
                  :QTL-INSEXP-DATE-5, 
                  :QTL-INSEXP-DATE-6, 
                  :QTL-INSEXP-DATE-7, 
                  :QTL-INSEXP-DATE-8, 
                  :QTL-INSEXP-DATE-9, 
                  :QTL-INSEXP-DATE-10, 
                  :QTL-INSPREM-1,
                  :QTL-INSPREM-2,
                  :QTL-INSPREM-3,
                  :QTL-INSPREM-4,
                  :QTL-INSPREM-5,
                  :QTL-INSPREM-6,
                  :QTL-INSPREM-7,
                  :QTL-INSPREM-8,
                  :QTL-INSPREM-9,
                  :QTL-INSPREM-10,
                  :QTL-INSCOMM-1,
                  :QTL-INSCOMM-2,
                  :QTL-INSCOMM-3,
                  :QTL-INSCOMM-4,
                  :QTL-INSCOMM-5,
                  :QTL-INSCOMM-6,
                  :QTL-INSCOMM-7,
                  :QTL-INSCOMM-8,
                  :QTL-INSCOMM-9,
                  :QTL-INSCOMM-10,
                  :QTL-DLNO,
                  :QTL-EARNINGS-1,
                  :QTL-EARNINGS-2,
                  :QTL-EARNINGS-3,
                  :QTL-EARNINGS-4,
                  :QTL-EARNINGS-5,
                  :QTL-EARNINGS-6,
                  :QTL-EARNINGS-7,
                  :QTL-EARNINGS-8,
                  :QTL-EARNINGS-9,
                  :QTL-EARNINGS-10,
                  :QTL-ANTICERN-1,
                  :QTL-ANTICERN-2,
                  :QTL-ANTICERN-3,
                  :QTL-ANTICERN-4,
                  :QTL-ANTICERN-5,
                  :QTL-ANTICERN-6,
                  :QTL-ANTICERN-7,
                  :QTL-ANTICERN-8,
                  :QTL-ANTICERN-9,
                  :QTL-ANTICERN-10,
                  :QTL-PRLNNO,
                  :QTL-PRBAL,
                  :QTL-PRINTDUE,
                  :QTL-PRFINCHGRB,
                  :QTL-PRINSRB,
                  :QTL-LPEOD5-FG, 
                  :QTL-STATE-WORKER,
                  :QTL-RESTRICTD-ACCTNO,
                  :QTL-SPRCLASS,
                  :QTL-USERID, 
                  :QTL-POSTDATE,
                  :QTL-POSTTIME,
                  :QTL-GIFTNET, 
                  :QTL-APR,
                  :QTL-SP-LNCOST-FRMLA, 
                  :QTL-MIP-FG, 
                  :QTL-INS-TAX-COUNTY-CD, 
                  :QTL-INS-TAX-CITY-CD, 
                  :QTL-PRDLDISC-UNEARN,
                  :QTL-POINTS,
                  :QTL-DEBIT-CARD-AMT,
                  :QTL-ACTIONCD2 
                 END-EXEC
              ELSE 
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.
 
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRP1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRP1RN
      * 20220923 BAH ADDED TP-LCDUE *1570
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRP1-FILE. *> SSP - TRPFILE
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           STRING "SELECT TRPFILE.TP_BRNO,"
                  "CAST(TRPFILE.TP_DATE AS VARCHAR(10)),"
                  "TRPFILE.TP_CLASS,"
                  "TRPFILE.TP_ACCTNO,"
                  "TRPFILE.TP_LINE,"
                  "TRPFILE.TP_LAWCODE,"
                  "TRPFILE.TP_JDCD," 
                  "TRPFILE.TP_PLCD," 
                  "TRPFILE.TP_SSNO,"
                  "TRPFILE.TP_MAKER," 
                  "TRPFILE.TP_IBPC," 
                  "TRPFILE.TP_TRCD," 
                  "TRPFILE.TP_REFNO," 
                  "CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10))," 
                  "CAST(TRPFILE.TP_DUEDATE AS VARCHAR(10))," 
                  "TRPFILE.TP_TRAMT,"
                  "TRPFILE.TP_INTDUE,"
                  "TRPFILE.TP_APOTH,"
                  "TRPFILE.TP_APOT2,"
                  "TRPFILE.TP_APINT,"
                  "TRPFILE.TP_APLC,"
                  "TRPFILE.TP_APCUR,"
                  "TRPFILE.TP_GLNO_1,"
                  "TRPFILE.TP_GLNO_2,"
                  "TRPFILE.TP_GLNO_3,"
                  "TRPFILE.TP_GLAMT_1,"
                  "TRPFILE.TP_GLAMT_2,"
                  "TRPFILE.TP_GLAMT_3,"
                  "TRPFILE.TP_EARNINGS_1,"
                  "TRPFILE.TP_EARNINGS_2,"
                  "TRPFILE.TP_EARNINGS_3,"
                  "TRPFILE.TP_EARNINGS_4,"
                  "TRPFILE.TP_EARNINGS_5,"
                  "TRPFILE.TP_EARNINGS_6,"
                  "TRPFILE.TP_EARNINGS_7,"
                  "TRPFILE.TP_EARNINGS_8,"
                  "TRPFILE.TP_EARNINGS_9,"
                  "TRPFILE.TP_EARNINGS_10,"
                  "TRPFILE.TP_WORKAMT_1,"
                  "TRPFILE.TP_WORKAMT_2,"
                  "TRPFILE.TP_WORKAMT_3,"
                  "TRPFILE.TP_WORKAMT_4,"
                  "TRPFILE.TP_WORKAMT_5,"
                  "TRPFILE.TP_WORKAMT_6,"
                  "TRPFILE.TP_WORKAMT_7,"
                  "TRPFILE.TP_WORKAMT_8,"
                  "TRPFILE.TP_WORKAMT_9,"
                  "TRPFILE.TP_WORKAMT_10,"
                  "TRPFILE.TP_OTHBAL,"
                  "TRPFILE.TP_OT2BAL,"
                  "TRPFILE.TP_INTBAL,"
                  "TRPFILE.TP_LCBAL,"
                  "TRPFILE.TP_CURBAL,"
                  "CAST(TRPFILE.TP_PDTH_DATE AS VARCHAR(10))," 
                  "TRPFILE.TP_DLNO,"
                  "TRPFILE.TP_DLPROC,"
                  "TRPFILE.TP_SERVACCT," 
                  "TRPFILE.TP_SALEFIN," 
                  "TRPFILE.TP_INSURED," 
                  "TRPFILE.TP_INSOURS," 
                  "TRPFILE.TP_LOANTYPE," 
                  "TRPFILE.TP_TILLNO,"
                  "CAST(TRPFILE.TP_LCPDTH_DATE AS VARCHAR(10)),"
                  "TRPFILE.TP_POSTING_BR,"
                  "TRPFILE.TP_OTHBR_FG," 
                  "TRPFILE.TP_REV," 
                  "TRPFILE.TP_REVTRCD," 
                  "TRPFILE.TP_REVSEQNO,"
                  "TRPFILE.TP_STATUSFG," 
                  "TRPFILE.TP_KEEPAPINT," 
                  "TRPFILE.TP_INTPAID,"
                  "TRPFILE.TP_PRIOR_POOLID,"
                  "TRPFILE.TP_INSTYPES,"
                  "TRPFILE.TP_DLPARVHELD_ADJ,"
                  "TRPFILE.TP_DLPARVPAID_ADJ,"
                  "TRPFILE.TP_NEWSTAT_RATE,"
                  "TRPFILE.TP_POCD,"
                  "CAST(TRPFILE.TP_PODATE AS VARCHAR(10))," 
                  "TRPFILE.TP_DL_RVCHGS_CODE_1," 
                  "TRPFILE.TP_DL_RVCHGS_CODE_2," 
                  "TRPFILE.TP_DL_RVCHGS_CODE_3," 
                  "TRPFILE.TP_DL_PARVHELD_CODE," 
                  "TRPFILE.TP_DL_PARVPAID_CODE," 
                  "TRPFILE.TP_TOTNODEF,"
                  "TRPFILE.TP_LNAMT,"
                  "CAST(TRPFILE.TP_ORIG_1STPYDATE AS VARCHAR(10))," 
                  "CAST(TRPFILE.TP_OLD1ST_PYDATE  AS VARCHAR(10))," 
                  "CAST(TRPFILE.TP_1STPYDATE      AS VARCHAR(10))," 
                  "TRPFILE.TP_ORGTERM,"
                  "TRPFILE.TP_INSCOMM,"
                  "TRPFILE.TP_MANUAL_RB," 
                  "TRPFILE.TP_USERID," 
                  "TRPFILE.TP_POSTTIME,"
                  "TRPFILE.TP_PLTYPE," 
                  "TRPFILE.TP_PL_OTH_AR,"
                  "TRPFILE.TP_LP_DLNO,"
                  "TRPFILE.TP_REPOCD,"
                  "TRPFILE.TP_SPRCLASS,"
                  "TRPFILE.TP_MAX_DEFER_FG," 
                  "CAST(TRPFILE.TP_POSTDATE       AS VARCHAR(10)),"
                  "TRPFILE.TP_UNITPER_CD," 
                  "TRPFILE.TP_UNITPER_FREQ,"
                  "TRPFILE.TP_SP_DEFFRMLA,"
                  "TRPFILE.TP_COLLECTOR," 
                  "TRPFILE.TP_PENALTY_INT,"
                  "TRPFILE.TP_INSCOMP,"
                  "CAST(TRPFILE.TP_PROMDATE       AS VARCHAR(10)),"
                  "TRPFILE.TP_PROMAMT,"
                  "TRPFILE.TP_PROMID,"
                  "TRPFILE.TP_INS_TAX_RB,"
                  "TRPFILE.TP_PROMDAYS_PASTDUE,"
                  "TRPFILE.TP_MIP_FG,"
                  "TRPFILE.TP_MIP_CLPAID,"
                  "TRPFILE.TP_MIP_CLCOMM,"
                  "TRPFILE.TP_MIP_AHPAID,"
                  "TRPFILE.TP_MIP_AHCOMM,"
                  "CAST(TRPFILE.TP_MIP_CLPDTH_DATE AS VARCHAR(10)),"
                  "CAST(TRPFILE.TP_MIP_AHPDTH_DATE AS VARCHAR(10)),"
                  "TRPFILE.TP_LP_SEQNO,"
                  "TRPFILE.TP_PRIOR_DF_CODE,"
                  "CAST(TRPFILE.TP_PRIOR_DF_POSTDATE AS VARCHAR(10)),"
                  "TRPFILE.TP_ACTIONCD2,"
                  "TRPFILE.TP_LCDUE,"
                  "TRPFILE.TP_REPAY_TRANS_ID,"
                  "TRPFILE.TP_CLOSFEE_REB_AMT"
                  " FROM DBO.TRPFILE | " INTO WS-SELECT.

           STRING " WHERE  TRPFILE.TP_BRNO = ",
                    QTRP1-WBEG-BRNO ,  
                  " AND TRPFILE.TP_DATE BETWEEN '" ,
                    QTRP1-WBEG-DATE ,
                  " 'AND '" ,
                    QTRP1-WEND-DATE ,
                  "' AND TRPFILE.TP_CLASS BETWEEN " ,
                    QTRP1-WBEG-CLASS ,
                  " AND " ,
                    QTRP1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING " ORDER BY TRPFILE.TP_BRNO,"
                    " TRPFILE.TP_DATE,"
                    " TRPFILE.TP_CLASS,"
                    " TRPFILE.TP_ACCTNO,"
                    " TRPFILE.TP_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT  DELIMITED BY "|",
                  WS-WHERE   DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT. *> EMBEDDED - TRPFILE
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                 FETCH TRP1_CURSOR INTO  
                   :QTP-BRNO,
                   :QTP-DATE,
                   :QTP-CLASS,
                   :QTP-ACCTNO,
                   :QTP-LINE,
                   :QTP-LAWCODE,
                   :QTP-JDCD, 
                   :QTP-PLCD, 
                   :QTP-SSNO,
                   :QTP-MAKER, 
                   :QTP-IBPC, 
                   :QTP-TRCD, 
                   :QTP-REFNO, 
                   :QTP-PAYDATE, 
                   :QTP-DUEDATE, 
                   :QTP-TRAMT,
                   :QTP-INTDUE,
                   :QTP-APOTH,
                   :QTP-APOT2,
                   :QTP-APINT,
                   :QTP-APLC,
                   :QTP-APCUR,
                   :QTP-GLNO-1,
                   :QTP-GLNO-2,
                   :QTP-GLNO-3,
                   :QTP-GLAMT-1,
                   :QTP-GLAMT-2,
                   :QTP-GLAMT-3,
                   :QTP-EARNINGS-1,
                   :QTP-EARNINGS-2,
                   :QTP-EARNINGS-3,
                   :QTP-EARNINGS-4,
                   :QTP-EARNINGS-5,
                   :QTP-EARNINGS-6,
                   :QTP-EARNINGS-7,
                   :QTP-EARNINGS-8,
                   :QTP-EARNINGS-9,
                   :QTP-EARNINGS-10,
                   :QTP-WORKAMT-1,
                   :QTP-WORKAMT-2,
                   :QTP-WORKAMT-3,
                   :QTP-WORKAMT-4,
                   :QTP-WORKAMT-5,
                   :QTP-WORKAMT-6,
                   :QTP-WORKAMT-7,
                   :QTP-WORKAMT-8,
                   :QTP-WORKAMT-9,
                   :QTP-WORKAMT-10,
                   :QTP-OTHBAL,
                   :QTP-OT2BAL,
                   :QTP-INTBAL,
                   :QTP-LCBAL,
                   :QTP-CURBAL,
                   :QTP-PDTH-DATE, 
                   :QTP-DLNO,
                   :QTP-DLPROC,
                   :QTP-SERVACCT, 
                   :QTP-SALEFIN, 
                   :QTP-INSURED, 
                   :QTP-INSOURS, 
                   :QTP-LOANTYPE, 
                   :QTP-TILLNO,
                   :QTP-LCPDTH-DATE,
                   :QTP-POSTING-BR,
                   :QTP-OTHBR-FG, 
                   :QTP-REV, 
                   :QTP-REVTRCD, 
                   :QTP-REVSEQNO,
                   :QTP-STATUSFG, 
                   :QTP-KEEPAPINT, 
                   :QTP-INTPAID,
                   :QTP-PRIOR-POOLID,
                   :QTP-INSTYPES,
                   :QTP-DLPARVHELD-ADJ,
                   :QTP-DLPARVPAID-ADJ,
                   :QTP-NEWSTAT-RATE,
                   :QTP-POCD,
                   :QTP-PODATE, 
                   :QTP-DL-RVCHGS-CODE-1, 
                   :QTP-DL-RVCHGS-CODE-2, 
                   :QTP-DL-RVCHGS-CODE-3, 
                   :QTP-DL-PARVHELD-CODE, 
                   :QTP-DL-PARVPAID-CODE, 
                   :QTP-TOTNODEF,
                   :QTP-LNAMT,
                   :QTP-ORIG-1STPYDATE, 
                   :QTP-OLD1ST-PYDATE, 
                   :QTP-1STPYDATE, 
                   :QTP-ORGTERM,
                   :QTP-INSCOMM,
                   :QTP-MANUAL-RB, 
                   :QTP-USERID, 
                   :QTP-POSTTIME,
                   :QTP-PLTYPE, 
                   :QTP-PL-OTH-AR,
                   :QTP-LP-DLNO,
                   :QTP-REPOCD,
                   :QTP-SPRCLASS,
                   :QTP-MAX-DEFER-FG, 
                   :QTP-POSTDATE,
                   :QTP-UNITPER-CD, 
                   :QTP-UNITPER-FREQ,
                   :QTP-SP-DEFFRMLA,
                   :QTP-COLLECTOR, 
                   :QTP-PENALTY-INT,
                   :QTP-INSCOMP,
                   :QTP-PROMDATE,
                   :QTP-PROMAMT,
                   :QTP-PROMID,
                   :QTP-INS-TAX-RB,
                   :QTP-PROMDAYS-PASTDUE,
                   :QTP-MIP-FG,
                   :QTP-MIP-CLPAID,
                   :QTP-MIP-CLCOMM,
                   :QTP-MIP-AHPAID,
                   :QTP-MIP-AHCOMM,
                   :QTP-MIP-CLPDTH-DATE,
                   :QTP-MIP-AHPDTH-DATE,
                   :QTP-LP-SEQNO,
                   :QTP-PRIOR-DF-CODE,
                   :QTP-PRIOR-DF-POSTDATE,
                   :QTP-ACTIONCD2,
                   :QTP-LCDUE,
                   :QTP-REPAY-TRANS-ID,
                   :QTP-CLOSFEE-REB-AMT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.

      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRS1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRS1RN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRS1-FILE. *> SSP - TRSFILE
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           STRING "SELECT TRSFILE.TS_BRNO,"
                  "CAST(TRSFILE.TS_DATE AS VARCHAR(10))," 
                  "TRSFILE.TS_CLASS,"
                  "TRSFILE.TS_ACCTNO,"
                  "TRSFILE.TS_LAWCODE,"
                  "TRSFILE.TS_LINE,"
                  "TRSFILE.TS_SSNO_1,"
                  "TRSFILE.TS_SSNO_2,"
                  "TRSFILE.TS_SSNO_3,"
                  "TRSFILE.TS_SSNO_4,"
                  "TRSFILE.TS_MAKERCD_1," 
                  "TRSFILE.TS_MAKERCD_2," 
                  "TRSFILE.TS_MAKERCD_3," 
                  "TRSFILE.TS_MAKERCD_4," 
                  "TRSFILE.TS_PURCD," 
                  "TRSFILE.TS_SRCD," 
                  "TRSFILE.TS_COLLCD," 
                  "TRSFILE.TS_SERVACCT," 
                  "TRSFILE.TS_LOANTYPE," 
                  "CAST(TRSFILE.TS_LOANDATE AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INTDATE  AS VARCHAR(10))," 
                  "TRSFILE.TS_PROCEEDS,"
                  "TRSFILE.TS_LNAMT,"
                  "TRSFILE.TS_FINCHG,"
                  "TRSFILE.TS_INTCHG,"
                  "TRSFILE.TS_SERCHG,"
                  "TRSFILE.TS_EXTCHG,"
                  "TRSFILE.TS_MAINTFEE,"
                  "TRSFILE.TS_ORGTERM,"
                  "TRSFILE.TS_1STPYAMT,"
                  "TRSFILE.TS_NOREGPY,"
                  "TRSFILE.TS_REGPYAMT,"
                  "TRSFILE.TS_LASTPYAMT,"
                  "TRSFILE.TS_FEECODE_1,"
                  "TRSFILE.TS_FEECODE_2," 
                  "TRSFILE.TS_FEECODE_3," 
                  "TRSFILE.TS_FEECODE_4," 
                  "TRSFILE.TS_FEECODE_5," 
                  "TRSFILE.TS_FEECODE_6," 
                  "TRSFILE.TS_FEECODE_7," 
                  "TRSFILE.TS_FEECODE_8," 
                  "TRSFILE.TS_FEECODE_9," 
                  "TRSFILE.TS_FEECODE_10," 
                  "TRSFILE.TS_FEECODE_11," 
                  "TRSFILE.TS_FEECODE_12," 
                  "TRSFILE.TS_FEECODE_13," 
                  "TRSFILE.TS_FEECODE_14," 
                  "TRSFILE.TS_FEECODE_15," 
                  "TRSFILE.TS_FEECODE_16," 
                  "TRSFILE.TS_FEECODE_17," 
                  "TRSFILE.TS_FEECODE_18," 
                  "TRSFILE.TS_FEECODE_19," 
                  "TRSFILE.TS_FEECODE_20," 
                  "TRSFILE.TS_FEEAMT_1,"          
                  "TRSFILE.TS_FEEAMT_2,"
                  "TRSFILE.TS_FEEAMT_3,"
                  "TRSFILE.TS_FEEAMT_4,"
                  "TRSFILE.TS_FEEAMT_5,"
                  "TRSFILE.TS_FEEAMT_6,"
                  "TRSFILE.TS_FEEAMT_7,"
                  "TRSFILE.TS_FEEAMT_8,"
                  "TRSFILE.TS_FEEAMT_9,"
                  "TRSFILE.TS_FEEAMT_10,"
                  "TRSFILE.TS_FEEAMT_11,"
                  "TRSFILE.TS_FEEAMT_12,"
                  "TRSFILE.TS_FEEAMT_13,"
                  "TRSFILE.TS_FEEAMT_14,"
                  "TRSFILE.TS_FEEAMT_15,"
                  "TRSFILE.TS_FEEAMT_16,"
                  "TRSFILE.TS_FEEAMT_17,"
                  "TRSFILE.TS_FEEAMT_18,"
                  "TRSFILE.TS_FEEAMT_19,"
                  "TRSFILE.TS_FEEAMT_20,"
                  "TRSFILE.TS_FEEOURS_1," 
                  "TRSFILE.TS_FEEOURS_2," 
                  "TRSFILE.TS_FEEOURS_3," 
                  "TRSFILE.TS_FEEOURS_4," 
                  "TRSFILE.TS_FEEOURS_5," 
                  "TRSFILE.TS_FEEOURS_6," 
                  "TRSFILE.TS_FEEOURS_7," 
                  "TRSFILE.TS_FEEOURS_8," 
                  "TRSFILE.TS_FEEOURS_9," 
                  "TRSFILE.TS_FEEOURS_10," 
                  "TRSFILE.TS_FEEOURS_11," 
                  "TRSFILE.TS_FEEOURS_12," 
                  "TRSFILE.TS_FEEOURS_13," 
                  "TRSFILE.TS_FEEOURS_14," 
                  "TRSFILE.TS_FEEOURS_15," 
                  "TRSFILE.TS_FEEOURS_16," 
                  "TRSFILE.TS_FEEOURS_17," 
                  "TRSFILE.TS_FEEOURS_18," 
                  "TRSFILE.TS_FEEOURS_19," 
                  "TRSFILE.TS_FEEOURS_20," 
                  "TRSFILE.TS_INSURED," 
                  "TRSFILE.TS_INSTYPES," 
                  "TRSFILE.TS_INSELIGIBLE," 
                  "TRSFILE.TS_INSCOMP_1,"
                  "TRSFILE.TS_INSCOMP_2,"
                  "TRSFILE.TS_INSCOMP_3,"
                  "TRSFILE.TS_INSCOMP_4,"
                  "TRSFILE.TS_INSCOMP_5,"
                  "TRSFILE.TS_INSCOMP_6,"
                  "TRSFILE.TS_INSCOMP_7,"
                  "TRSFILE.TS_INSCOMP_8,"
                  "TRSFILE.TS_INSCOMP_9,"
                  "TRSFILE.TS_INSCOMP_10,"
                  "TRSFILE.TS_INSOURS_1," 
                  "TRSFILE.TS_INSOURS_2," 
                  "TRSFILE.TS_INSOURS_3," 
                  "TRSFILE.TS_INSOURS_4," 
                  "TRSFILE.TS_INSOURS_5," 
                  "TRSFILE.TS_INSOURS_6," 
                  "TRSFILE.TS_INSOURS_7," 
                  "TRSFILE.TS_INSOURS_8," 
                  "TRSFILE.TS_INSOURS_9," 
                  "TRSFILE.TS_INSOURS_10,"
                  "CAST(TRSFILE.TS_INSEFF_DATE_1 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_2 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_3 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_4 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_5 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_6 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_7 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_8 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_9 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEFF_DATE_10 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_1 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_2 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_3 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_4 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_5 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_6 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_7 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_8 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_9 AS VARCHAR(10))," 
                  "CAST(TRSFILE.TS_INSEXP_DATE_10 AS VARCHAR(10))," 
                  "TRSFILE.TS_INSPREM_1,"
                  "TRSFILE.TS_INSPREM_2,"
                  "TRSFILE.TS_INSPREM_3,"
                  "TRSFILE.TS_INSPREM_4,"
                  "TRSFILE.TS_INSPREM_5,"
                  "TRSFILE.TS_INSPREM_6,"
                  "TRSFILE.TS_INSPREM_7,"
                  "TRSFILE.TS_INSPREM_8,"
                  "TRSFILE.TS_INSPREM_9,"
                  "TRSFILE.TS_INSPREM_10,"
                  "TRSFILE.TS_INSCOMM_1,"
                  "TRSFILE.TS_INSCOMM_2,"
                  "TRSFILE.TS_INSCOMM_3,"
                  "TRSFILE.TS_INSCOMM_4,"
                  "TRSFILE.TS_INSCOMM_5,"
                  "TRSFILE.TS_INSCOMM_6,"
                  "TRSFILE.TS_INSCOMM_7,"
                  "TRSFILE.TS_INSCOMM_8,"
                  "TRSFILE.TS_INSCOMM_9,"
                  "TRSFILE.TS_INSCOMM_10,"
                  "TRSFILE.TS_DLCOMM_PP_CMSI,"
                  "TRSFILE.TS_DLCOMM_O3_CMSI,"
                  "TRSFILE.TS_DLNO,"
                  "TRSFILE.TS_DLRECOURSE," 
                  "TRSFILE.TS_DLPARTRCAMT,"
                  "TRSFILE.TS_PURBAL,"
                  "TRSFILE.TS_DLRVPRIN_1,"
                  "TRSFILE.TS_DLRVPRIN_2,"
                  "TRSFILE.TS_DLRVPRIN_3,"
                  "TRSFILE.TS_DLRVCHGS_1,"
                  "TRSFILE.TS_DLRVCHGS_2,"
                  "TRSFILE.TS_DLRVCHGS_3,"             
                  "TRSFILE.TS_DL_RVCHGS_CODE_1," 
                  "TRSFILE.TS_DL_RVCHGS_CODE_2," 
                  "TRSFILE.TS_DL_RVCHGS_CODE_3," 
                  "TRSFILE.TS_DL_PARVHELD_CODE," 
                  "TRSFILE.TS_DL_PARVPAID_CODE," 
                  "TRSFILE.TS_DLPARVHELD,"
                  "TRSFILE.TS_DLPARVPAID,"
                  "TRSFILE.TS_DLDISC,"
                  "TRSFILE.TS_ANTICTERM,"
                  "TRSFILE.TS_ANTICERN_1,"
                  "TRSFILE.TS_ANTICERN_2,"
                  "TRSFILE.TS_ANTICERN_3,"
                  "TRSFILE.TS_ANTICERN_4,"
                  "TRSFILE.TS_ANTICERN_5,"
                  "TRSFILE.TS_ANTICERN_6,"
                  "TRSFILE.TS_ANTICERN_7,"
                  "TRSFILE.TS_ANTICERN_8,"
                  "TRSFILE.TS_ANTICERN_9,"
                  "TRSFILE.TS_ANTICERN_10,"
                  "TRSFILE.TS_EARNINGS_1,"
                  "TRSFILE.TS_EARNINGS_2,"
                  "TRSFILE.TS_EARNINGS_3,"
                  "TRSFILE.TS_EARNINGS_4,"
                  "TRSFILE.TS_EARNINGS_5,"
                  "TRSFILE.TS_EARNINGS_6,"
                  "TRSFILE.TS_EARNINGS_7,"
                  "TRSFILE.TS_EARNINGS_8,"
                  "TRSFILE.TS_EARNINGS_9,"
                  "TRSFILE.TS_EARNINGS_10,"
                  "TRSFILE.TS_PRLNNO,"
                  "TRSFILE.TS_INSPREM_DLADJ,"
                  "TRSFILE.TS_PRDLDISC_UNEARN,"
                  "TRSFILE.TS_SP_LNCOST_FRMLA," 
                  "TRSFILE.TS_DLPAID," 
                  "TRSFILE.TS_MIP_FG," 
                  "TRSFILE.TS_SPRCLASS,"
                  "TRSFILE.TS_O3INS_DLPROC,"
                  "TRSFILE.TS_USERID," 
                  "CAST(TRSFILE.TS_POSTDATE AS VARCHAR(10))," 
                  "TRSFILE.TS_POSTTIME,"
                  "TRSFILE.TS_ACTIONCD2" 
                  " FROM DBO.TRSFILE |" INTO WS-SELECT.

           STRING " WHERE TRSFILE.TS_BRNO = ",
                    QTRS1-WBEG-BRNO ,
                  " AND TRSFILE.TS_DATE BETWEEN '" ,
                    QTRS1-WBEG-DATE , 
                  "' AND '" ,
                    QTRS1-WEND-DATE ,
                  "' AND TRSFILE.TS_CLASS BETWEEN " ,
                    QTRS1-WBEG-CLASS ,
                  " AND " ,
                    QTRS1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING " ORDER BY TRSFILE.TS_BRNO,"
                   "  TRSFILE.TS_DATE,"
                   "  TRSFILE.TS_CLASS,"
                   "  TRSFILE.TS_ACCTNO,"
                   "  TRSFILE.TS_LAWCODE,"
                   "  TRSFILE.TS_LINE | " INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRS1_CURSOR CURSOR FOR
                :TRS1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-BRNO.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.

           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT. *> EMBEDDED - TRSFILE
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.


           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                  FETCH TRS1_CURSOR INTO  
                   :QTS-BRNO, 
                   :QTS-DATE, 
                   :QTS-CLASS,
                   :QTS-ACCTNO,
                   :QTS-LAWCODE,
                   :QTS-LINE,
                   :QTS-SSNO-1,
                   :QTS-SSNO-2,
                   :QTS-SSNO-3,
                   :QTS-SSNO-4,
                   :QTS-MAKERCD-1, 
                   :QTS-MAKERCD-2, 
                   :QTS-MAKERCD-3, 
                   :QTS-MAKERCD-4, 
                   :QTS-PURCD, 
                   :QTS-SRCD, 
                   :QTS-COLLCD, 
                   :QTS-SERVACCT, 
                   :QTS-LOANTYPE, 
                   :QTS-LOANDATE, 
                   :QTS-INTDATE, 
                   :QTS-PROCEEDS,
                   :QTS-LNAMT,
                   :QTS-FINCHG,
                   :QTS-INTCHG,
                   :QTS-SERCHG,
                   :QTS-EXTCHG,
                   :QTS-MAINTFEE,
                   :QTS-ORGTERM,
                   :QTS-1STPYAMT,
                   :QTS-NOREGPY,
                   :QTS-REGPYAMT,
                   :QTS-LASTPYAMT,
                   :QTS-FEECODE-1, 
                   :QTS-FEECODE-2, 
                   :QTS-FEECODE-3, 
                   :QTS-FEECODE-4, 
                   :QTS-FEECODE-5, 
                   :QTS-FEECODE-6, 
                   :QTS-FEECODE-7, 
                   :QTS-FEECODE-8, 
                   :QTS-FEECODE-9, 
                   :QTS-FEECODE-10, 
                   :QTS-FEECODE-11, 
                   :QTS-FEECODE-12, 
                   :QTS-FEECODE-13, 
                   :QTS-FEECODE-14, 
                   :QTS-FEECODE-15, 
                   :QTS-FEECODE-16, 
                   :QTS-FEECODE-17, 
                   :QTS-FEECODE-18, 
                   :QTS-FEECODE-19, 
                   :QTS-FEECODE-20, 
                   :QTS-FEEAMT-1,          
                   :QTS-FEEAMT-2,
                   :QTS-FEEAMT-3,
                   :QTS-FEEAMT-4,
                   :QTS-FEEAMT-5,
                   :QTS-FEEAMT-6,
                   :QTS-FEEAMT-7,
                   :QTS-FEEAMT-8,
                   :QTS-FEEAMT-9,
                   :QTS-FEEAMT-10,
                   :QTS-FEEAMT-11,
                   :QTS-FEEAMT-12,
                   :QTS-FEEAMT-13,
                   :QTS-FEEAMT-14,
                   :QTS-FEEAMT-15,
                   :QTS-FEEAMT-16,
                   :QTS-FEEAMT-17,
                   :QTS-FEEAMT-18,
                   :QTS-FEEAMT-19,
                   :QTS-FEEAMT-20,
                   :QTS-FEEOURS-1, 
                   :QTS-FEEOURS-2, 
                   :QTS-FEEOURS-3, 
                   :QTS-FEEOURS-4, 
                   :QTS-FEEOURS-5, 
                   :QTS-FEEOURS-6, 
                   :QTS-FEEOURS-7, 
                   :QTS-FEEOURS-8, 
                   :QTS-FEEOURS-9, 
                   :QTS-FEEOURS-10, 
                   :QTS-FEEOURS-11, 
                   :QTS-FEEOURS-12, 
                   :QTS-FEEOURS-13, 
                   :QTS-FEEOURS-14, 
                   :QTS-FEEOURS-15, 
                   :QTS-FEEOURS-16, 
                   :QTS-FEEOURS-17, 
                   :QTS-FEEOURS-18, 
                   :QTS-FEEOURS-19, 
                   :QTS-FEEOURS-20, 
                   :QTS-INSURED, 
                   :QTS-INSTYPES, 
                   :QTS-INSELIGIBLE, 
                   :QTS-INSCOMP-1,
                   :QTS-INSCOMP-2,
                   :QTS-INSCOMP-3,
                   :QTS-INSCOMP-4,
                   :QTS-INSCOMP-5,
                   :QTS-INSCOMP-6,
                   :QTS-INSCOMP-7,
                   :QTS-INSCOMP-8,
                   :QTS-INSCOMP-9,
                   :QTS-INSCOMP-10,
                   :QTS-INSOURS-1, 
                   :QTS-INSOURS-2, 
                   :QTS-INSOURS-3, 
                   :QTS-INSOURS-4, 
                   :QTS-INSOURS-5, 
                   :QTS-INSOURS-6, 
                   :QTS-INSOURS-7, 
                   :QTS-INSOURS-8, 
                   :QTS-INSOURS-9, 
                   :QTS-INSOURS-10,  
                   :QTS-INSEFF-DATE-1, 
                   :QTS-INSEFF-DATE-2, 
                   :QTS-INSEFF-DATE-3, 
                   :QTS-INSEFF-DATE-4, 
                   :QTS-INSEFF-DATE-5, 
                   :QTS-INSEFF-DATE-6, 
                   :QTS-INSEFF-DATE-7, 
                   :QTS-INSEFF-DATE-8, 
                   :QTS-INSEFF-DATE-9, 
                   :QTS-INSEFF-DATE-10, 
                   :QTS-INSEXP-DATE-1, 
                   :QTS-INSEXP-DATE-2, 
                   :QTS-INSEXP-DATE-3, 
                   :QTS-INSEXP-DATE-4, 
                   :QTS-INSEXP-DATE-5, 
                   :QTS-INSEXP-DATE-6, 
                   :QTS-INSEXP-DATE-7, 
                   :QTS-INSEXP-DATE-8, 
                   :QTS-INSEXP-DATE-9, 
                   :QTS-INSEXP-DATE-10, 
                   :QTS-INSPREM-1,
                   :QTS-INSPREM-2,
                   :QTS-INSPREM-3,
                   :QTS-INSPREM-4,
                   :QTS-INSPREM-5,
                   :QTS-INSPREM-6,
                   :QTS-INSPREM-7,
                   :QTS-INSPREM-8,
                   :QTS-INSPREM-9,
                   :QTS-INSPREM-10,
                   :QTS-INSCOMM-1,
                   :QTS-INSCOMM-2,
                   :QTS-INSCOMM-3,
                   :QTS-INSCOMM-4,
                   :QTS-INSCOMM-5,
                   :QTS-INSCOMM-6,
                   :QTS-INSCOMM-7,
                   :QTS-INSCOMM-8,
                   :QTS-INSCOMM-9,
                   :QTS-INSCOMM-10,
                   :QTS-DLCOMM-PP-CMSI,
                   :QTS-DLCOMM-O3-CMSI,
                   :QTS-DLNO,
                   :QTS-DLRECOURSE, 
                   :QTS-DLPARTRCAMT,
                   :QTS-PURBAL,
                   :QTS-DLRVPRIN-1,
                   :QTS-DLRVPRIN-2,
                   :QTS-DLRVPRIN-3,
                   :QTS-DLRVCHGS-1,
                   :QTS-DLRVCHGS-2,
                   :QTS-DLRVCHGS-3,              
                   :QTS-DL-RVCHGS-CODE-1, 
                   :QTS-DL-RVCHGS-CODE-2, 
                   :QTS-DL-RVCHGS-CODE-3, 
                   :QTS-DL-PARVHELD-CODE, 
                   :QTS-DL-PARVPAID-CODE, 
                   :QTS-DLPARVHELD,
                   :QTS-DLPARVPAID,
                   :QTS-DLDISC,
                   :QTS-ANTICTERM,
                   :QTS-ANTICERN-1,
                   :QTS-ANTICERN-2,
                   :QTS-ANTICERN-3,
                   :QTS-ANTICERN-4,
                   :QTS-ANTICERN-5,
                   :QTS-ANTICERN-6,
                   :QTS-ANTICERN-7,
                   :QTS-ANTICERN-8,
                   :QTS-ANTICERN-9,
                   :QTS-ANTICERN-10,
                   :QTS-EARNINGS-1,
                   :QTS-EARNINGS-2,
                   :QTS-EARNINGS-3,
                   :QTS-EARNINGS-4,
                   :QTS-EARNINGS-5,
                   :QTS-EARNINGS-6,
                   :QTS-EARNINGS-7,
                   :QTS-EARNINGS-8,
                   :QTS-EARNINGS-9,
                   :QTS-EARNINGS-10,
                   :QTS-PRLNNO,
                   :QTS-INSPREM-DLADJ,
                   :QTS-PRDLDISC-UNEARN,
                   :QTS-SP-LNCOST-FRMLA, 
                   :QTS-DLPAID, 
                   :QTS-MIP-FG, 
                   :QTS-SPRCLASS,
                   :QTS-O3INS-DLPROC,
                   :QTS-USERID, 
                   :QTS-POSTDATE,
                   :QTS-POSTTIME,
                   :QTS-ACTIONCD2 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRW1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRW1RN
       LOAD-TRW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH            TO TRW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRW-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRW-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH            TO TRW-PATH.

      *-----------------------------------------------------------------
       OPEN-TRW1-FILE.
           PERFORM LOAD-TRW1-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRW1-FILE. *> SSP - TRWFILE
           PERFORM START-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRW1-FILE.

           STRING "SELECT TRWFILE.TW_BRNO,"
                  "CAST(TRWFILE.TW_DATE AS VARCHAR(10))," 
                  "TRWFILE.TW_TILLNO,"
                  "TRWFILE.TW_POSTNAME_1," 
                  "TRWFILE.TW_POSTNAME_2," 
                  "TRWFILE.TW_POSTNAME_3," 
                  "TRWFILE.TW_POSTNAME_4," 
                  "TRWFILE.TW_POSTNAME_5," 
                  "TRWFILE.TW_POSTNAME_6," 
                  "TRWFILE.TW_POSTNAME_7," 
                  "TRWFILE.TW_POSTNAME_8," 
                  "TRWFILE.TW_POSTNAME_9," 
                  "TRWFILE.TW_POSTNAME_10," 
                  "TRWFILE.TW_POSTNAME_11," 
                  "TRWFILE.TW_POSTNAME_12," 
                  "TRWFILE.TW_POSTNAME_13," 
                  "TRWFILE.TW_POSTNAME_14," 
                  "TRWFILE.TW_POSTNAME_15," 
                  "TRWFILE.TW_POSTNAME_16," 
                  "TRWFILE.TW_POSTNAME_17," 
                  "TRWFILE.TW_POSTNAME_18," 
                  "TRWFILE.TW_POSTNAME_19," 
                  "TRWFILE.TW_POSTNAME_20," 
                  "TRWFILE.TW_TIME_1,"
                  "TRWFILE.TW_TIME_2,"
                  "TRWFILE.TW_TIME_3,"
                  "TRWFILE.TW_TIME_4,"
                  "TRWFILE.TW_TIME_5,"
                  "TRWFILE.TW_TIME_6,"
                  "TRWFILE.TW_TIME_7,"
                  "TRWFILE.TW_TIME_8,"
                  "TRWFILE.TW_TIME_9,"
                  "TRWFILE.TW_TIME_10,"
                  "TRWFILE.TW_TIME_11,"
                  "TRWFILE.TW_TIME_12,"
                  "TRWFILE.TW_TIME_13,"
                  "TRWFILE.TW_TIME_14,"
                  "TRWFILE.TW_TIME_15,"
                  "TRWFILE.TW_TIME_16,"
                  "TRWFILE.TW_TIME_17,"
                  "TRWFILE.TW_TIME_18,"
                  "TRWFILE.TW_TIME_19,"
                  "TRWFILE.TW_TIME_20,"
                  "TRWFILE.TW_AMOUNT_1,"
                  "TRWFILE.TW_AMOUNT_2,"
                  "TRWFILE.TW_AMOUNT_3,"
                  "TRWFILE.TW_AMOUNT_4,"
                  "TRWFILE.TW_AMOUNT_5,"
                  "TRWFILE.TW_AMOUNT_6,"
                  "TRWFILE.TW_AMOUNT_7,"
                  "TRWFILE.TW_AMOUNT_8,"
                  "TRWFILE.TW_AMOUNT_9,"
                  "TRWFILE.TW_AMOUNT_10,"
                  "TRWFILE.TW_AMOUNT_11,"
                  "TRWFILE.TW_AMOUNT_12,"
                  "TRWFILE.TW_AMOUNT_13,"
                  "TRWFILE.TW_AMOUNT_14,"
                  "TRWFILE.TW_AMOUNT_15,"
                  "TRWFILE.TW_AMOUNT_16,"
                  "TRWFILE.TW_AMOUNT_17,"
                  "TRWFILE.TW_AMOUNT_18,"
                  "TRWFILE.TW_AMOUNT_19,"
                  "TRWFILE.TW_AMOUNT_20,"
                  "TRWFILE.TW_OVERSHORT_1,"
                  "TRWFILE.TW_OVERSHORT_2,"
                  "TRWFILE.TW_OVERSHORT_3,"
                  "TRWFILE.TW_OVERSHORT_4,"
                  "TRWFILE.TW_OVERSHORT_5,"
                  "TRWFILE.TW_OVERSHORT_6,"
                  "TRWFILE.TW_OVERSHORT_7,"
                  "TRWFILE.TW_OVERSHORT_8,"
                  "TRWFILE.TW_OVERSHORT_9,"
                  "TRWFILE.TW_OVERSHORT_10,"
                  "TRWFILE.TW_OVERSHORT_11,"
                  "TRWFILE.TW_OVERSHORT_12,"
                  "TRWFILE.TW_OVERSHORT_13,"
                  "TRWFILE.TW_OVERSHORT_14,"
                  "TRWFILE.TW_OVERSHORT_15,"
                  "TRWFILE.TW_OVERSHORT_16,"
                  "TRWFILE.TW_OVERSHORT_17,"
                  "TRWFILE.TW_OVERSHORT_18,"
                  "TRWFILE.TW_OVERSHORT_19,"
                  "TRWFILE.TW_OVERSHORT_20"
                  " FROM DBO.TRWFILE |" INTO WS-SELECT.

           STRING " WHERE TRWFILE.TW_BRNO = ",
                    QTRW1-WBEG-BRNO ,
                  " AND TRWFILE.TW_DATE BETWEEN '" ,
                    QTRW1-WBEG-DATE , 
                  "' AND '" ,
                    QTRW1-WEND-DATE ,
                  "' | " INTO WS-WHERE.

           STRING " ORDER BY TRWFILE.TW_BRNO,"
                   "  TRWFILE.TW_DATE,"
                   "  TRWFILE.TW_TILLNO | " INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRW1_CURSOR CURSOR FOR
                :TRW1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRW1-WBEG-BRNO.
           MOVE SPACES TO QTRW1-WBEG-DATE
                          QTRW1-WEND-DATE.

           MOVE STAT---GOOD TO TRW1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRW1-FILE-NEXT. *> EMBEDDED - TRWFILE
           PERFORM READ-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-PATH-SQL    TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.
           INITIALIZE QTRW-REC.


           IF ( TRW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                  FETCH TRW1_CURSOR INTO  
                   :QTW-BRNO,  
                   :QTW-DATE, 
                   :QTW-TILLNO,
                   :QTW-POSTNAME-1, 
                   :QTW-POSTNAME-2, 
                   :QTW-POSTNAME-3, 
                   :QTW-POSTNAME-4, 
                   :QTW-POSTNAME-5, 
                   :QTW-POSTNAME-6, 
                   :QTW-POSTNAME-7, 
                   :QTW-POSTNAME-8, 
                   :QTW-POSTNAME-9, 
                   :QTW-POSTNAME-10, 
                   :QTW-POSTNAME-11, 
                   :QTW-POSTNAME-12, 
                   :QTW-POSTNAME-13, 
                   :QTW-POSTNAME-14, 
                   :QTW-POSTNAME-15, 
                   :QTW-POSTNAME-16, 
                   :QTW-POSTNAME-17, 
                   :QTW-POSTNAME-18, 
                   :QTW-POSTNAME-19, 
                   :QTW-POSTNAME-20, 
                   :QTW-TIME-1,
                   :QTW-TIME-2,
                   :QTW-TIME-3,
                   :QTW-TIME-4,
                   :QTW-TIME-5,
                   :QTW-TIME-6,
                   :QTW-TIME-7,
                   :QTW-TIME-8,
                   :QTW-TIME-9,
                   :QTW-TIME-10,
                   :QTW-TIME-11,
                   :QTW-TIME-12,
                   :QTW-TIME-13,
                   :QTW-TIME-14,
                   :QTW-TIME-15,
                   :QTW-TIME-16,
                   :QTW-TIME-17,
                   :QTW-TIME-18,
                   :QTW-TIME-19,
                   :QTW-TIME-20,
                   :QTW-AMOUNT-1,
                   :QTW-AMOUNT-2,
                   :QTW-AMOUNT-3,
                   :QTW-AMOUNT-4,
                   :QTW-AMOUNT-5,
                   :QTW-AMOUNT-6,
                   :QTW-AMOUNT-7,
                   :QTW-AMOUNT-8,
                   :QTW-AMOUNT-9,
                   :QTW-AMOUNT-10,
                   :QTW-AMOUNT-11,
                   :QTW-AMOUNT-12,
                   :QTW-AMOUNT-13,
                   :QTW-AMOUNT-14,
                   :QTW-AMOUNT-15,
                   :QTW-AMOUNT-16,
                   :QTW-AMOUNT-17,
                   :QTW-AMOUNT-18,
                   :QTW-AMOUNT-19,
                   :QTW-AMOUNT-20,
                   :QTW-OVERSHORT-1,
                   :QTW-OVERSHORT-2,
                   :QTW-OVERSHORT-3,
                   :QTW-OVERSHORT-4,
                   :QTW-OVERSHORT-5,
                   :QTW-OVERSHORT-6,
                   :QTW-OVERSHORT-7,
                   :QTW-OVERSHORT-8,
                   :QTW-OVERSHORT-9,
                   :QTW-OVERSHORT-10,
                   :QTW-OVERSHORT-11,
                   :QTW-OVERSHORT-12,
                   :QTW-OVERSHORT-13,
                   :QTW-OVERSHORT-14,
                   :QTW-OVERSHORT-15,
                   :QTW-OVERSHORT-16,
                   :QTW-OVERSHORT-17,
                   :QTW-OVERSHORT-18,
                   :QTW-OVERSHORT-19,
                   :QTW-OVERSHORT-20
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRW1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRW1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRW1-CURSOR-STAT.

      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRX1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRX1RN
       LOAD-TRX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRX-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRX-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH.

      *-----------------------------------------------------------------
       OPEN-TRX1-FILE.
           PERFORM LOAD-TRX1-FILE.
           PERFORM OPEN-IT.
           MOVE TRX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRX1-FILE. *> SSP - TRXFILE
           PERFORM START-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRX1-FILE.

           STRING "SELECT TRXFILE.TX_BRNO,"
                  "CAST(TRXFILE.TX_DATE AS VARCHAR(10)),"
                  "TRXFILE.TX_CLASS,"
                  "TRXFILE.TX_ACCTNO,"
                  "TRXFILE.TX_LAWCODE,"
                  "TRXFILE.TX_LINE,"
                  "TRXFILE.TX_SSNO_1,"
                  "TRXFILE.TX_SSNO_2,"
                  "TRXFILE.TX_SSNO_3,"
                  "TRXFILE.TX_SSNO_4,"
                  "TRXFILE.TX_MAKERCD_1," 
                  "TRXFILE.TX_MAKERCD_2," 
                  "TRXFILE.TX_MAKERCD_3," 
                  "TRXFILE.TX_MAKERCD_4," 
                  "TRXFILE.TX_PURCD," 
                  "TRXFILE.TX_SRCD," 
                  "TRXFILE.TX_COLLCD," 
                  "TRXFILE.TX_SERVACCT," 
                  "TRXFILE.TX_LOANTYPE," 
                  "CAST(TRXFILE.TX_LOANDATE AS VARCHAR(10))," 
                  "TRXFILE.TX_PROCEEDS,"
                  "TRXFILE.TX_LNAMT,"
                  "TRXFILE.TX_FINCHG,"
                  "TRXFILE.TX_INTCHG,"
                  "TRXFILE.TX_SERCHG,"
                  "TRXFILE.TX_EXTCHG,"
                  "TRXFILE.TX_MAINTFEE,"
                  "TRXFILE.TX_ORGTERM,"
                  "TRXFILE.TX_INSURED,"
                  "TRXFILE.TX_DLNO,"
                  "TRXFILE.TX_DLRECOURSE,"
                  "TRXFILE.TX_DLPARTRCAMT,"
                  "TRXFILE.TX_DLRVPRIN_1,"
                  "TRXFILE.TX_DLRVPRIN_2,"
                  "TRXFILE.TX_DLRVPRIN_3,"
                  "TRXFILE.TX_DLRVCHGS_1,"
                  "TRXFILE.TX_DLRVCHGS_2,"
                  "TRXFILE.TX_DLRVCHGS_3,"
                  "TRXFILE.TX_DLPARVHELD,"
                  "TRXFILE.TX_DLPARVPAID,"
                  "TRXFILE.TX_DLDISC,"
                  "TRXFILE.TX_ANTICTERM,"
                  "TRXFILE.TX_ANTICERN_1,"
                  "TRXFILE.TX_ANTICERN_2,"
                  "TRXFILE.TX_ANTICERN_3,"
                  "TRXFILE.TX_ANTICERN_4,"
                  "TRXFILE.TX_ANTICERN_5,"
                  "TRXFILE.TX_ANTICERN_6,"
                  "TRXFILE.TX_ANTICERN_7,"
                  "TRXFILE.TX_ANTICERN_8,"
                  "TRXFILE.TX_ANTICERN_9,"
                  "TRXFILE.TX_ANTICERN_10,"
                  "TRXFILE.TX_EARNINGS_1,"
                  "TRXFILE.TX_EARNINGS_2,"
                  "TRXFILE.TX_EARNINGS_3,"
                  "TRXFILE.TX_EARNINGS_4,"
                  "TRXFILE.TX_EARNINGS_5,"
                  "TRXFILE.TX_EARNINGS_6,"
                  "TRXFILE.TX_EARNINGS_7,"
                  "TRXFILE.TX_EARNINGS_8,"
                  "TRXFILE.TX_EARNINGS_9,"
                  "TRXFILE.TX_EARNINGS_10,"
                  "TRXFILE.TX_PRLNNO,"
                  "TRXFILE.TX_PRBAL,"
                  "TRXFILE.TX_TFRBAL,"
                  "TRXFILE.TX_SPRCLASS,"
                  "TRXFILE.TX_USERID," 
                  "CAST(TRXFILE.TX_POSTDATE AS VARCHAR(10))," 
                  "TRXFILE.TX_POSTTIME"
                  " FROM DBO.TRXFILE  | " INTO WS-SELECT.

           STRING " WHERE TRXFILE.TX_BRNO = ",
                    QTRX1-WBEG-BRNO ,
                  " AND TRXFILE.TX_DATE BETWEEN '" ,
                    QTRX1-WBEG-DATE ,
                  "' AND '" ,
                    QTRX1-WEND-DATE ,
                  "' AND TRXFILE.TX_CLASS BETWEEN " ,
                   QTRX1-WBEG-CLASS ,
                  " AND " ,
                   QTRX1-WEND-CLASS ,
                  " | " INTO WS-WHERE.

           STRING " ORDER BY TRXFILE.TX_BRNO,"
                   "  TRXFILE.TX_DATE,"
                   "  TRXFILE.TX_CLASS,"
                   "  TRXFILE.TX_ACCTNO,"
                   "  TRXFILE.TX_LAWCODE,"
                   "  TRXFILE.TX_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRX1_CURSOR CURSOR FOR
                :TRX1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRX1-WBEG-BRNO.
           MOVE SPACES TO QTRX1-WBEG-DATE
                          QTRX1-WEND-DATE.

           MOVE STAT---GOOD TO TRX1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRX1-FILE-NEXT. *> EMBEDDED - TRXFILE
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           INITIALIZE QTRX-REC.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                  FETCH TRX1_CURSOR INTO  
                   :QTX-BRNO,
                   :QTX-DATE,
                   :QTX-CLASS,
                   :QTX-ACCTNO,
                   :QTX-LAWCODE,
                   :QTX-LINE,
                   :QTX-SSNO-1,
                   :QTX-SSNO-2,
                   :QTX-SSNO-3,
                   :QTX-SSNO-4,
                   :QTX-MAKERCD-1, 
                   :QTX-MAKERCD-2, 
                   :QTX-MAKERCD-3, 
                   :QTX-MAKERCD-4, 
                   :QTX-PURCD, 
                   :QTX-SRCD, 
                   :QTX-COLLCD, 
                   :QTX-SERVACCT, 
                   :QTX-LOANTYPE, 
                   :QTX-LOANDATE, 
                   :QTX-PROCEEDS,
                   :QTX-LNAMT,
                   :QTX-FINCHG,
                   :QTX-INTCHG,
                   :QTX-SERCHG,
                   :QTX-EXTCHG,
                   :QTX-MAINTFEE,
                   :QTX-ORGTERM,
                   :QTX-INSURED,
                   :QTX-DLNO,
                   :QTX-DLRECOURSE,
                   :QTX-DLPARTRCAMT,
                   :QTX-DLRVPRIN-1,
                   :QTX-DLRVPRIN-2,
                   :QTX-DLRVPRIN-3,
                   :QTX-DLRVCHGS-1,
                   :QTX-DLRVCHGS-2,
                   :QTX-DLRVCHGS-3,
                   :QTX-DLPARVHELD,
                   :QTX-DLPARVPAID,
                   :QTX-DLDISC,
                   :QTX-ANTICTERM,
                   :QTX-ANTICERN-1,
                   :QTX-ANTICERN-2,
                   :QTX-ANTICERN-3,
                   :QTX-ANTICERN-4,
                   :QTX-ANTICERN-5,
                   :QTX-ANTICERN-6,
                   :QTX-ANTICERN-7,
                   :QTX-ANTICERN-8,
                   :QTX-ANTICERN-9,
                   :QTX-ANTICERN-10,
                   :QTX-EARNINGS-1,
                   :QTX-EARNINGS-2,
                   :QTX-EARNINGS-3,
                   :QTX-EARNINGS-4,
                   :QTX-EARNINGS-5,
                   :QTX-EARNINGS-6,
                   :QTX-EARNINGS-7,
                   :QTX-EARNINGS-8,
                   :QTX-EARNINGS-9,
                   :QTX-EARNINGS-10,
                   :QTX-PRLNNO,
                   :QTX-PRBAL,
                   :QTX-TFRBAL,
                   :QTX-SPRCLASS,
                   :QTX-USERID, 
                   :QTX-POSTDATE, 
                   :QTX-POSTTIME
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRX1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRX1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------


       COPY "LIBEX/EXXTRBI.CPY".
       COPY "LIBEX/EXXTRCI.CPY".
       COPY "LIBEX/EXXTRDI.CPY".
       COPY "LIBEX/EXXTRJI.CPY".
       COPY "LIBEX/EXXTRKI.CPY".
       COPY "LIBEX/EXXTRLI.CPY".
       COPY "LIBEX/EXXTRPI.CPY".
       COPY "LIBEX/EXXTRSI.CPY".
       COPY "LIBEX/EXXTRWI.CPY".
       COPY "LIBEX/EXXTRXI.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TREXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
