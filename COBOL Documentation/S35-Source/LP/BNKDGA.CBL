       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BNKDGA.
       DATE-WRITTEN.    011985.
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  GEORGIA LOAN TAX REPORT. DETAIL.
      *
      * REV:
      *  JTG 101895 ADDED GROUP LOGIC AND CROSS DATA PATH LOGIC
      *  JFF 110195 ADDED LOAN CLASS SCAN
      *  MJD 112095 ADDED TEST FOR PAID OFF DATE AND P&L DATE.
      *  MJD 121295 ADDED USER BRANCH ONLY OPTION.
      *                   DATE TEST FOR PAYMENT RECORDS
      *                   BRANCH SECURITY LOGIC
      *             IN PAID OUT SECTION REPLACED PR BAL WITH AMOUNT OF
      *              OF PAYOFF.
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *   CS 042898 ADDED "RB" AND "RN" RENEWAL CODES TO TEST FOR PAID OFF
      *             ACCOUNT
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 040129 THIS PROGRAM NEVER WORKED IN BATCH, MISSING FIELD
      *              "DT2." IN DISPLAY-PARAMETERS...FIXED
      *             ADDED ENDING CLASS FOR WORLD, PR# 341
      *  BAH 160125 REMOVE LTFILE, PD0003
      *  BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181203 GLOBAL LNFILE
      *  BAH 20190924 REMOVED ACCESS-CALL LNFILE
      * BAH 20220803 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WP-FILE ASSIGN WP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WP-KEY
                  FILE STATUS FILE-STAT.
           SELECT WC-FILE ASSIGN WC-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WC-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".


      *************************************
      *   PAID OUT ACCOUNT KEY FILE.
      *************************************
       FD  WP-FILE
           LABEL RECORDS ARE STANDARD.
       01  WP-REC.
           03  WP-KEY.
               05  WP-OWNBR            PIC 9(4).
               05  WP-CLASS            PIC 9(3).
               05  WP-ACCTNO           PIC 9(8).

      *************************************
      *  CHARGE OFF ACCOUNT KEY FILE
      *************************************
       FD  WC-FILE
           LABEL RECORDS ARE STANDARD.
       01  WC-REC.
           03  WC-KEY.
               05  WC-OWNBR            PIC 9(4).
               05  WC-CLASS            PIC 9(3).
               05  WC-ACCTNO           PIC 9(8).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BNKDGA.CBL J35 12/21/23-08:45:30 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       01  WP-BUF               PIC X(35).
       01  WC-BUF               PIC X(35).

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WP-IFN               PIC X(2)     VALUE "WP".
       01  WC-IFN               PIC X(2)     VALUE "WC".


      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED                 PIC 999      VALUE 0.
           03  LN-COUNT                PIC 999      VALUE 99.
           03  LN-WK                   PIC 999      VALUE 0.
       01  PAGE-NUMBER                 PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  NEW-BRANCH                  PIC X        VALUE "Y".
       01  SUB                         PIC 9(2)     COMP.
       01  SUB1                        PIC 9(2)     COMP.
       01  SUB2                        PIC 9(2)     COMP.
       01  HOLD-FEES                   PIC S9(5)V99 VALUE ZERO.

      *----------------------------------------------------------------
      * FIRST-PAYMENT-REC : "Y" WHEN READING FIRST PAYMENT RECORD
      * PL-DATE-OKAY      : "Y" IF ACCOUNT WAS PUT IN P&L THIS MONTH.
      *----------------------------------------------------------------
       01  FIRST-PAYMENT-REC           PIC X        VALUE "Y".
       01  PL-DATE-OKAY                PIC X        VALUE "N".

      * 01  INS-STATUS                  PIC 9 VALUE 0.
      *     88  INS-FOUND        VALUE 1.
      *     88  INS-PRINTED             VALUE 2.
      * 01  LEV                  PIC 9(2)     COMP.

       01  RECORDS-FOUND               PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  FIRST-TIME                  PIC X        VALUE "Y".
       01  SUMMARY-FG                  PIC X        VALUE "N".
       01  VALID-DATE                  PIC X        VALUE "Y".
       01  NAME-WK                     PIC X(70).
       01  DATE-WK                     PIC 9(4).
       01  FILLER REDEFINES DATE-WK.
           03  MONTH-WK                PIC 9(2).
           03  YEAR-WK                 PIC 9(2).
       01  HOLD-BRANCH                 PIC 9(4)     COMP   VALUE 0.
       01  HOLD-TYPE                   PIC 9(2)     COMP   VALUE 0.

       01  INT-REF                     PIC S9(5)V99  COMP-3  VALUE 0.
       01  SERV-REF                    PIC S9(5)V99  COMP-3  VALUE 0.
       01  MAINT-REF                   PIC S9(5)V99  COMP-3  VALUE 0.
       01  CL-REF                      PIC S9(5)V99  COMP-3  VALUE 0.
       01  AH-REF                      PIC S9(5)V99  COMP-3  VALUE 0.
       01  PP-REF                      PIC S9(5)V99  COMP-3  VALUE 0.
       01  PO-AMT                      PIC S9(5)V99  COMP-3  VALUE 0.

      *************************************************************
      *         LEVELS  : BRANCH / GRAND
      *         BRACKETS CORRESPOND TO NEW BUSINESS
      *                                PAID OFF
      *                                CHARGE OFF
      *************************************************************
       01  TT-TAB.
           03  TT-LEVEL           OCCURS 2.
               05  TT-BRACKET     OCCURS 3.
                   07  TT-AMOUNTS.
                       09  TT-LNAMT    PIC S9(10)V99 COMP.
                       09  TT-INT      PIC S9(10)V99 COMP.
                       09  TT-SERV     PIC S9(10)V99 COMP.
                       09  TT-FEES     PIC S9(10)V99 COMP.
                       09  TT-PROCEEDS PIC S9(10)V99 COMP.
                       09  TT-CL       PIC S9(10)V99 COMP.
                       09  TT-AH       PIC S9(10)V99 COMP.
                       09  TT-PP       PIC S9(10)V99 COMP.
                       09  TT-MAINT    PIC S9(10)V99 COMP.
                   07  TT-AMOUNTX  REDEFINES TT-AMOUNTS.
                       09  TT-AMT OCCURS 9 PIC S9(10)V99 COMP.

       01  TOTAL-TYPE-TABLE.
           03  FILLER                  PIC X(12)  VALUE "NEW BUSINESS".
           03  FILLER                  PIC X(12)  VALUE "PAID OFFS   ".
           03  FILLER                  PIC X(12)  VALUE "CHARGE OFFS ".
           03  FILLER                  PIC X(12)  VALUE "RECOVERY    ".
           03  FILLER                  PIC X(12)  VALUE "REFUNDS     ".
           03  FILLER                  PIC X(12)  VALUE "SOLD/TRANS  ".
       01  TOTAL-TYPEX   REDEFINES TOTAL-TYPE-TABLE.
           03  TOTAL-TYPE   OCCURS 6   PIC X(12).

      * 01  TYPE-ALP-TAB.
      *     03  FILLER                 PIC X(18) VALUE
      *     "COCOLLATERAL     :".
      *     03  FILLER                 PIC X(18) VALUE
      *     "SRSOURCE         :".
      *     03  FILLER                 PIC X(18) VALUE
      *     "PUPURPOSE        :".
      * 01  TYPE-TABX REDEFINES TYPE-ALP-TAB.
      *     03  TYPE-ALPX         OCCURS 3.
      *         05  TYPE-KEY            PIC XX.
      *         05  TYPE-ALP            PIC X(16).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).
           03  BEG-CLASS               PIC 9(3).
           03  END-CLASS               PIC 9(3).
           03  BEG-LN-DATE             PIC 9(8).
           03  END-LN-DATE             PIC 9(8).
           03  SERVA                   PIC X.
           03  DETAIL-TYPE             PIC X.
           03  GROUP-FLAG              PIC X.
           03  ENT-GROUP               PIC X(4).

       01  VERSION-CONTROL             PIC X VALUE "C".

       01 SV-BEG-BR                    PIC 9999 VALUE 9999.
       01 SV-END-BR                    PIC 9999 VALUE 9999.
       COPY "LIBGB/PASSWDW.CPY".


      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
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

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "- - - -  GEORGIA LOAN TAX REPORT  - - - -".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNOX.
               05  H-BRNO       PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(21) VALUE " ".
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(6)  VALUE " THRU ".
           03  H-END-DATE       PIC 99/99/99.

       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).
       01  HEAD4TYPE.
           03  FILLER           PIC X(52)  VALUE SPACES.
           03  H-TITLE           PIC X(28).
           03  FILLER           PIC X(52)  VALUE SPACES.
       01  PAYOFF-TITLE         PIC X(28) VALUE
           "****     PAID  OFFS     ****".
       01  NEWBUS-TITLE         PIC X(28) VALUE
           "****   NEW   BUSINESS   ****".
       01  CHARGE-TITLE         PIC X(28) VALUE
           "****    CHARGE  OFFS    ****".
       01  BRANCH-TITLE         PIC X(28) VALUE
           "****   BRANCH TOTALS    ****".
       01  GRAND-TITLE         PIC X(28) VALUE
           "****   GRAND  TOTALS    ****".

       01  HEAD5.
           03  FILLER           PIC X(44) VALUE
           "ACCOUNT #         BORROWER                  ".
           03  FILLER           PIC X(50) VALUE
           "LOAN   TERM    LOAN   INTEREST SERVICE MAINT  FEES".
           03  FILLER           PIC X(35) VALUE
           "   PROCEEDS --INSURANCE  PREMIUMS--".

       01 HEAD5A.
           03  FILLER           PIC X(44).
           03  FILLER           PIC X(42) VALUE
           "DATE           AMOUNT CHARGE   CHARGE  FEE".
           03  FILLER           PIC X(22).
           03  FILLER           PIC X(20) VALUE
           "  CL      AH      PP".

       01  HEAD6.
           03  FILLER           PIC X(47) VALUE
           "ACCOUNT #         BORROWER                 PAID".
           03  FILLER           PIC X(44) VALUE
           " OFF DATE  TERM   PO AMT  REB INT   REB SERV".
           03  FILLER           PIC X(39) VALUE
           "  REB MAINT  REB CL    REB AH    REB PP".

       01  HEAD7.
           03  FILLER           PIC X(47) VALUE
           "ACCOUNT #         BORROWER                     ".
           03  FILLER           PIC X(44) VALUE
           "LOAN DATE        PL AMNT                    ".
           03  FILLER           PIC X(38) VALUE
           "                              PL DATE ".
       01 HEAD7A.
           03  FILLER           PIC X(95).
           03  FILLER           PIC X(32) VALUE
           "  CL INS     AH INS     PP INS  ".

       01  HEAD8.
           03  FILLER           PIC X(12) VALUE
           "DESCRIPTION ".
           03  FILLER           PIC X(15) VALUE
           "    LOAN AMOUNT".
           03  FILLER           PIC X(15) VALUE
           "        INT CHG".
           03  FILLER           PIC X(15) VALUE
           "       SERV CHG".
           03  FILLER           PIC X(15) VALUE
           "      FEES     ".
           03  FILLER           PIC X(15) VALUE
           "       PROCEEDS".
           03  FILLER           PIC X(15) VALUE
           "    CL  CHG/REB".
           03  FILLER           PIC X(15) VALUE
           "    AH  CHG/REB".
           03  FILLER           PIC X(15) VALUE
           "    PP  CHG/REB".
       01 HEAD8A.
           03  FILLER           PIC X(56).
           03  FILLER           PIC X(15) VALUE
           "      MAINT CHG".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  DETAIL1             REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC X(16).
           03  FILLER           PIC XX.
           03  D-NAME           PIC X(24).
           03  FILLER           PIC X.
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC X(1).
           03  D-TERM           PIC ZZ9.
           03  FILLER           PIC X(1).
           03  D-LNAMT          PIC Z(5)9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-INTEREST       PIC Z(4)9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-FEE-4-8        PIC Z(3)9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-MAINT          PIC Z9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-FEES           PIC Z(3)9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-PROCEEDS       PIC Z(4)9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-CL             PIC Z(3)9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-AH             PIC Z(3)9.99  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-PP             PIC Z(3)9.99  BLANK ZERO.

       01  DETAIL2             REDEFINES DETAIL-LINE.
           03  D2-LOAN-NUM      PIC X(16).
           03  FILLER           PIC X(2).
           03  D2-NAME          PIC X(29).
           03  FILLER           PIC X.
           03  D2-LOANDATE      PIC 99/99/99.
           03  FILLER           PIC X(2).
           03  D2-TERM          PIC 9(3).
           03  FILLER           PIC X(1).
           03  D-PRBAL          PIC Z(5)9.99-  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-RI             PIC Z(4)9.99-  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-RS             PIC Z(4)9.99-  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-RM             PIC Z(4)9.99-  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-RC             PIC Z(4)9.99-  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-RA             PIC Z(4)9.99-  BLANK ZERO.
           03  FILLER           PIC X(1).
           03  D-RP             PIC Z(4)9.99-  BLANK ZERO.

       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  D3-LOAN-NUM      PIC X(16).
           03  FILLER           PIC XX.
           03  D3-NAME          PIC X(29).
           03  FILLER           PIC X.
           03  D3-LOANDATE      PIC 99/99/99.
           03  FILLER           PIC X(6).
           03  D-PLAMT          PIC Z(5)9.99-  BLANK ZERO.
           03  FILLER           PIC X(49).
           03  D-PLDATE         PIC 99/99/99.
           03  FILLER           PIC X.

       01  DETAIL4             REDEFINES DETAIL-LINE.
           03  D-TOTAL-TYPE     PIC X(12).
           03  D-TOTALS         OCCURS 8.
               05  FILLER       PIC X(1).
               05  D-TT-AMT     PIC Z(9)9.99-.
       01  DETAIL4A            REDEFINES DETAIL-LINE.
           03  D2-TOTAL-TYPE    PIC X(12).
           03  FILLER           PIC X(1).
           03  D2-TT-AMT        PIC Z(9)9.99-.

       01 DETAIL4B              REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(58).
           03  D-TT-MAINT       PIC Z(9)9.99-.

       01  RANGE-LINE          REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  RANGE-SEL        PIC X(10).
           03  FILLER          REDEFINES RANGE-SEL.
               05  RANGE-DATE   PIC ZZZZ/ZZ/ZZ.
      *----------------------------------------------------------------
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BNKDGA_DEF.CPY".
       COPY "LIBLP/BNKDGA_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BNKDGA_SCN.CPY".

      /****************************************************************
      *     P R O G R A M   C O N T R O L
      ****************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WP-FILE WC-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WP-FILE WC-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************
       MAIN-MODULE SECTION.
      *************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BNKDGA" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
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
           PERFORM CLOSE-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           MOVE BR-NO TO HOLD-BRANCH H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

           MOVE BR-NO TO HOLD-BRANCH H-BRNO.
           MOVE BR-NAME TO H-BRNAME.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE 0 TO HOLD-TYPE.
           MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           PERFORM OPEN-WP-FILE-OUTPUT.
           PERFORM CLOSE-WP-FILE.
           PERFORM OPEN-WP-FILE.

           PERFORM OPEN-WC-FILE-OUTPUT.
           PERFORM CLOSE-WC-FILE.
           PERFORM OPEN-WC-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

      *----------------------------------------------------------------
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-BRANCH.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-BRANCH.

           IF LN-CLASS < BEG-CLASS
              GO TO NEXT-LN.

      * PL ACCOUNTS CHECKED BELOW.
           IF SERVA NOT = " "
              IF (SERVA = "O") AND (LN-SERVACCT NOT = "Y")
                 GO TO NEXT-LN
              ELSE
              IF (SERVA = "S") AND (LN-SERVACCT = "Y")
                 GO TO NEXT-LN.

      * ZBAL ACCOUNTS CHECKED BELOW.

           MOVE "Y" TO VALID-DATE.

           IF LN-SALEFIN = "Y"
              MOVE LN-PURDATE TO SYS-DATE
           ELSE
              MOVE LN-LOANDATE TO SYS-DATE.
           IF SYS-DATE < BEG-LN-DATE OR
                SYS-DATE > END-LN-DATE
              MOVE "N" TO VALID-DATE.

           PERFORM GET-SP.

      * CREATE KEY FILE FOR PAID OUT ACCOUNTS
           IF LN-CURBAL = 0
              IF LN-INTBAL = 0
                 IF LN-LCBAL = 0
                    IF LN-OTHBAL = 0
                       IF LN-POFFDATE NOT = 0
                          PERFORM CREATE-PO-KEY-FILE
                          GO TO NEXT-LN.

      * CREATE KEY-FILE FOR CHARGE OFF ACCOUNTS
           IF LN-PLDATE NOT = 0
              PERFORM CREATE-PL-KEY-FILE
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-LN-DATE OR LN-LOANDATE > END-LN-DATE
              GO TO NEXT-LN.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LN.

       END-OF-BRANCH.

           PERFORM PROCESS-WP-REC.
           PERFORM PROCESS-WC-REC.

           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-WP-FILE.
           PERFORM CLOSE-WC-FILE.
           PERFORM BRANCH-TOTALS.
           PERFORM CLEAR-TT-BRANCH.
           MOVE "N" TO DETAIL-TYPE
                       SUMMARY-FG.
           COMPUTE LN-COUNT = EXT-CONAME-RPT-LINES + 1.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              PERFORM RANGE-LINES
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM GRAND-TOTALS
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE BNKDGA-QUEUE-POS TO Q-POS.
           MOVE BNKDGA-COPIES-POS TO C-POS.
           MOVE BNKDGA-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BR-FILE.

           MOVE "N" TO DETAIL-TYPE.

      *----------------------------------------------------------------
      * CREATE PAID OUT KEY FILE.
      *----------------------------------------------------------------
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WP-BUF.
           PERFORM OPEN-WP-FILE-OUTPUT.
           PERFORM CLOSE-WP-FILE.

      *----------------------------------------------------------------
      * CREATE CHARGED OFF KEY FILE.
      *----------------------------------------------------------------
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WC-BUF.
           PERFORM OPEN-WC-FILE-OUTPUT.
           PERFORM CLOSE-WC-FILE.

           PERFORM CLEAR-TT-TOTALS.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE BEG-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEG-DATE.
           MOVE END-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-END-DATE.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF BEG-CLASS = END-CLASS
              MOVE BEG-CLASS TO H-LCNO
              MOVE LC-DESC TO H-LCDESC
           ELSE
              MOVE "ALL" TO H-LCDESC.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LN-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-LN-DATE.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO DT-01.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
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
              GO TO CHECK-BR-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH        TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
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

           GO TO CHECK-BR-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU
           MOVE SPACES TO VDUBUF
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BR-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF NOT IO-GOOD
                 GO TO CL-01.
           IF VDUSTATUS = "16"
              MOVE 0 TO BEG-CLASS VDUNUM0
              MOVE "SN N020CL1." TO VDU
              PERFORM SCREENIO
              MOVE 99 TO END-CLASS VDUNUM0
              MOVE "SN N020CL2." TO VDU
              PERFORM SCREENIO
              GO TO DT-01.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
           IF BEG-CLASS = 0
              GO TO CL-01.

           PERFORM OPEN-LC1-FILE.
           MOVE BEG-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              MOVE "INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF END-CLASS = 0
              GO TO CL-02.
           IF END-CLASS < BEG-CLASS
              GO TO CL-02.
       DT-01.
           MOVE "S  A090ALLDT." TO VDU.
           MOVE "         " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDUNUM0 TO DATE-WK.

           MOVE MONTH-WK TO DATE-MMDDYY-MM.
           MOVE YEAR-WK  TO DATE-MMDDYY-YY.
           MOVE 01       TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO DT-01.

           MOVE NUM-DATE TO BEG-LN-DATE NDTE-DATE.
           MOVE 51 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO END-LN-DATE.

           GO TO SERVA-01.
       ALL-DT.
           MOVE "S  A040DT1." TO VDU.
           MOVE 0 TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A090ALLDT." TO VDU.
           MOVE "ALL DATES" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE.
           MOVE EXT-JULIAN-END TO END-LN-DATE.
       SERVA-01.
           IF BEG-CLASS NOT = 0
              MOVE " " TO SERVA
              MOVE "S  A010SERVA." TO VDU
              PERFORM SCREENIO
              GO TO OK-YN.
           MOVE "E  A010SERVA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SERVA-01.
           MOVE VDUBUF TO SERVA.
           IF NOT (SERVA = "I" OR "S" OR "O") GO TO SERVA-01.
       OK-YN.
           MOVE " " TO EXT-EOBUF-ERRCD.

           IF BEG-LN-DATE = 0
              MOVE EXT-JULIAN-BEG TO BEG-LN-DATE.
           IF END-LN-DATE = 99999999
              MOVE EXT-JULIAN-END TO END-LN-DATE.
           IF END-LN-DATE < BEG-LN-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A040BR2." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040DT1." TO VDU.
           MOVE BEG-LN-DATE TO DATE-YYYYMMDD NUM-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE NUM-MO TO DATE-MMDDYY-DD.
           MOVE 0      TO DATE-MMDDYY-MM.
           MOVE DATE-MMDDYY TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010SERVA." TO VDU.
           MOVE SERVA TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************************************

           PERFORM MOVE-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE NAME-WK          TO D-NAME.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-LOANDATE.
           MOVE LN-ORGTERM       TO D-TERM.
           MOVE LN-LNAMT         TO D-LNAMT.
           MOVE LN-INTCHG        TO D-INTEREST.
           MOVE LN-SERCHG        TO D-FEE-4-8.
           MOVE LN-MAINTFEE      TO D-MAINT.
           MOVE 0 TO HOLD-FEES.
           MOVE LN-PROCEEDS      TO D-PROCEEDS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              ADD LN-FEEAMT(SUB) TO HOLD-FEES
           END-PERFORM.
           MOVE HOLD-FEES        TO D-FEES.
           MOVE LN-INSPREM(1)    TO D-CL.
           MOVE LN-INSPREM(2)    TO D-AH.
           MOVE LN-INSPREM(3)    TO D-PP.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM ACCUM-TT-TOTALS.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      *****************************************************************
       PROCESS-WP-REC SECTION.
      *****************************************************************
           MOVE "P" TO DETAIL-TYPE.
           COMPUTE LN-COUNT = EXT-CONAME-RPT-LINES + 1.
           PERFORM CLOSE-WP-FILE.
           PERFORM OPEN-WP-FILE.

           MOVE 0 TO WP-OWNBR WP-CLASS WP-ACCTNO.

           PERFORM START-WP-FILE.
       NEXT-WP.
           PERFORM READ-WP-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PROCESS-WP.

           MOVE WP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           PERFORM FIND-PRIOR-REFUNDS.
           MOVE PO-AMT TO D-PRBAL.
           MOVE INT-REF TO D-RI.
           MOVE SERV-REF TO D-RS.
           MOVE MAINT-REF TO D-RM.
           MOVE CL-REF TO D-RC.
           MOVE AH-REF TO D-RA.
           MOVE PP-REF TO D-RP.

           IF DETAIL-LINE NOT = SPACES
              PERFORM MOVE-BORROWER
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D2-LOAN-NUM
              MOVE NAME-WK          TO D2-NAME
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY      TO D2-LOANDATE
              MOVE LN-ORGTERM       TO D2-TERM
              PERFORM WRITE-DETAIL-LINE
              PERFORM ACCUM-TT-TOTALS.

           GO TO NEXT-WP.
       EXIT-PROCESS-WP.
           EXIT.

      *****************************************************************
       PROCESS-WC-REC SECTION.
      *****************************************************************
           MOVE "C" TO DETAIL-TYPE.
           COMPUTE LN-COUNT = EXT-CONAME-RPT-LINES + 1.
           PERFORM CLOSE-WC-FILE.
           PERFORM OPEN-WC-FILE.
           MOVE 0 TO WC-OWNBR WC-CLASS WC-ACCTNO.
           PERFORM START-WC-FILE.
       NEXT-WC.
           PERFORM READ-WC-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PROCESS-WC.

           MOVE WC-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           PERFORM CHECK-PL-DATE.
           IF PL-DATE-OKAY NOT = "Y"
              GO TO NEXT-WC.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D3-LOAN-NUM.
           PERFORM MOVE-BORROWER.
           MOVE NAME-WK          TO D3-NAME.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D3-LOANDATE.
           MOVE LN-PLAMT         TO D-PLAMT.
           MOVE LN-PLDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-PLDATE.

           PERFORM WRITE-DETAIL-LINE.
           PERFORM ACCUM-TT-TOTALS.
           GO TO NEXT-WC.
       EXIT-PROCESS-WC.
           EXIT.

      *****************************************************************
       BRANCH-TOTALS SECTION.
      *****************************************************************
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO SUMMARY-FG.
           MOVE "B" TO DETAIL-TYPE.
           COMPUTE LN-COUNT = EXT-CONAME-RPT-LINES + 1.
           MOVE 1 TO SUB.
           PERFORM PRINT-TT-TOTALS VARYING SUB1 FROM 1 BY 1
                                   UNTIL SUB1 > 3.
           MOVE TOTAL-TYPE(4) TO D2-TOTAL-TYPE.
           MOVE 0 TO D2-TT-AMT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE TOTAL-TYPE(5) TO D2-TOTAL-TYPE.
           COMPUTE D2-TT-AMT = TT-INT(1, 2) + TT-SERV(1, 2) +
                               TT-MAINT(1, 2) + TT-AH(1, 2) +
                               TT-CL(1, 2) + TT-PP(1, 2).
           PERFORM WRITE-DETAIL-LINE.
           MOVE TOTAL-TYPE(6) TO D2-TOTAL-TYPE.
           MOVE 0 TO D2-TT-AMT.
           PERFORM WRITE-DETAIL-LINE.


      *****************************************************************
       GRAND-TOTALS SECTION.
      *****************************************************************
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO H-BRNOX
           ELSE
              MOVE 0 TO H-BRNO.
           MOVE "ALL" TO H-BRNAME.
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO SUMMARY-FG.
           MOVE "G" TO DETAIL-TYPE.
           COMPUTE LN-COUNT = EXT-CONAME-RPT-LINES + 1.
           MOVE 2 TO LN-FEED.
           MOVE 2 TO SUB.
           PERFORM PRINT-TT-TOTALS VARYING SUB1 FROM 1 BY 1
                                   UNTIL SUB1 > 3.
           MOVE TOTAL-TYPE(4) TO D2-TOTAL-TYPE.
           MOVE 0 TO D2-TT-AMT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE TOTAL-TYPE(5) TO D2-TOTAL-TYPE.
           COMPUTE D2-TT-AMT = TT-INT(2, 2) + TT-SERV(2, 2) +
                               TT-MAINT(2, 2) + TT-AH(2, 2) +
                               TT-CL(2, 2) + TT-PP(2, 2).
           PERFORM WRITE-DETAIL-LINE.
           MOVE TOTAL-TYPE(6) TO D2-TOTAL-TYPE.
           MOVE 0 TO D2-TT-AMT.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       MOVE-BORROWER SECTION.
      *****************************************************************
           IF LN-SSNO(1) = 0
              GO TO EXIT-MOVE-BORROWER.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "* *  NOT FOUND  * *" TO D-NAME
              GO TO EXIT-MOVE-BORROWER.

           MOVE BW-LNAME TO NAME-WK.
           INSPECT NAME-WK REPLACING FIRST "   " BY ", +".
           INSPECT NAME-WK REPLACING
              FIRST "+                                  " BY BW-FNAME.
       EXIT-MOVE-BORROWER.
           EXIT.

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************
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

           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF SUMMARY-FG = "N"
              IF DETAIL-TYPE = "N"
                 MOVE NEWBUS-TITLE TO H-TITLE
                 MOVE HEAD4TYPE TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD5 TO PR-REC
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD5A TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 8 TO LN-COUNT
              END-IF
              IF DETAIL-TYPE = "P"
                 MOVE PAYOFF-TITLE TO H-TITLE
                 MOVE HEAD4TYPE TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD6 TO PR-REC
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 8 TO LN-COUNT
              END-IF
              IF DETAIL-TYPE = "C"
                 MOVE CHARGE-TITLE TO H-TITLE
                 MOVE HEAD4TYPE TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD7 TO PR-REC
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 8 TO LN-COUNT
              END-IF
           ELSE
              IF DETAIL-TYPE = "B"
                 MOVE BRANCH-TITLE TO H-TITLE
                 MOVE HEAD4TYPE TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD8 TO PR-REC
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD8A TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 9 TO LN-COUNT
              END-IF
              IF DETAIL-TYPE = "G"
                 MOVE GRAND-TITLE TO H-TITLE
                 MOVE HEAD4TYPE TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD8 TO PR-REC
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD8A TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 9 TO LN-COUNT
              END-IF.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.

      *****************************************************************
       RANGE-LINES SECTION.
      *****************************************************************

           MOVE " " TO DETAIL-TYPE.
           PERFORM HEAD-PAGE.
           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "GROUP     " TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING  DATE" TO RANGE-LINE.
           MOVE BEG-LN-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING  DATE" TO RANGE-LINE.
           MOVE END-LN-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "SERVICE ACCOUNTS?" TO RANGE-LINE.
           MOVE SERVA TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       CREATE-PO-KEY-FILE SECTION.
      *****************************************************************
           MOVE LN-OWNBR TO WP-OWNBR.
           MOVE LN-CLASS TO WP-CLASS.
           MOVE LN-ACCTNO TO WP-ACCTNO.
           PERFORM WRITE-WP-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

      *****************************************************************
       CREATE-PL-KEY-FILE SECTION.
      *****************************************************************
           MOVE LN-OWNBR TO WC-OWNBR.
           MOVE LN-CLASS TO WC-CLASS.
           MOVE LN-ACCTNO TO WC-ACCTNO.
           PERFORM WRITE-WC-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

      *****************************************************************
       ACCUM-TT-TOTALS SECTION.
      *****************************************************************
           IF DETAIL-TYPE = "N"
              PERFORM ACCUM-NEW-BUSINESS
           ELSE
           IF DETAIL-TYPE = "P"
              PERFORM ACCUM-PAID-OFF
           ELSE
              PERFORM ACCUM-CHG-OFF.
           GO TO EXIT-ACCUM-TT-TOTALS.
       ACCUM-NEW-BUSINESS.
           ADD LN-LNAMT TO TT-LNAMT(1, 1) TT-LNAMT(2, 1).
           ADD LN-INTCHG TO TT-INT(1, 1)  TT-INT(2, 1).
           ADD LN-SERCHG TO TT-SERV(1, 1) TT-SERV(2, 1).
           ADD LN-MAINTFEE TO TT-MAINT(1, 1) TT-MAINT(2, 1).
           ADD HOLD-FEES TO TT-FEES(1, 1) TT-FEES(2, 1).
           ADD LN-PROCEEDS TO TT-PROCEEDS(1, 1) TT-PROCEEDS(2, 1).
           ADD LN-INSPREM(1) TO TT-CL(1, 1) TT-CL(2, 1).
           ADD LN-INSPREM(2) TO TT-AH(1, 1) TT-AH(2, 1).
           ADD LN-INSPREM(3) TO TT-PP(1, 1) TT-PP(2, 1).
       ACCUM-PAID-OFF.
           ADD PO-AMT TO TT-LNAMT(1, 2) TT-LNAMT(2, 2).
           ADD INT-REF  TO TT-INT(1, 2) TT-INT(2, 2).
           ADD SERV-REF  TO TT-SERV(1, 2) TT-SERV(2, 2).
           ADD MAINT-REF  TO TT-MAINT(1, 2) TT-MAINT(2, 2).
           ADD AH-REF  TO TT-AH(1, 2) TT-AH(2, 2).
           ADD CL-REF  TO TT-CL(1, 2) TT-CL(2, 2).
           ADD PP-REF  TO TT-PP(1, 2) TT-PP(2, 2).
       ACCUM-CHG-OFF.
           ADD LN-PLAMT TO TT-LNAMT(1, 3) TT-LNAMT(2, 3).
       EXIT-ACCUM-TT-TOTALS.
           EXIT.

      *----------------------------------------------------------------
      * FIND PRIOR REFUNDS SECTION
      *   IN :  LN-ACCTNO
      *  OUT :  INT-REF      REBATED INTEREST.
      *         SERV-REF     REBATED SERVICE CHARGES.
      *         MAINT-REF    REBATED MAINTENANCE FEES.
      *         CL-REF       REBATED CL INSURANCE
      *         AH-REF       REBATED AH INSURANCE
      *         PP-REF       REBATED PP INSURANCE
      *         PO-AMT       PAID OUT AMOUNT
      *----------------------------------------------------------------
      *****************************************************************
       FIND-PRIOR-REFUNDS SECTION.
      *****************************************************************
           MOVE 0 TO CL-REF AH-REF PP-REF INT-REF
                     SERV-REF MAINT-REF PO-AMT.
           MOVE "Y" TO FIRST-PAYMENT-REC.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-PRIOR-REFUND.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO EXIT-FIND-PRIOR-REFUNDS.

           IF (LP-BRNO NOT = LN-OWNBR) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-FIND-PRIOR-REFUNDS.
           IF LP-REV = "Y"
              GO TO NEXT-PRIOR-REFUND.

           IF (LP-TRDATE < BEG-LN-DATE) OR (LP-TRDATE > END-LN-DATE)
              GO TO NEXT-PRIOR-REFUND.

      *----------------------------------------------------------------
      * TEST TO MAKE SURE THAT FIRST RECORD READ IS PO, RN, PB, OR SC.
      *  IF NOT THEN THE ACCOUNT IS NOT PAID OUT.
      *----------------------------------------------------------------
           IF FIRST-PAYMENT-REC = "Y"
              IF LP-TRCD = "PO" OR "RN" OR "PB" OR "SC"
                              OR "RB" OR "RO"
                 MOVE LP-TRAMT TO PO-AMT
              ELSE
                 GO TO EXIT-FIND-PRIOR-REFUNDS.

           MOVE "N" TO FIRST-PAYMENT-REC.

           IF LP-TRCD = "RI"
              ADD LP-TRAMT TO INT-REF
           ELSE
              IF LP-TRCD = "RM"
                 ADD LP-TRAMT TO MAINT-REF
           ELSE
              IF LP-TRCD = "RS"
                 ADD LP-TRAMT TO SERV-REF
           ELSE
              IF LP-TRCD = "RC"
                 ADD LP-TRAMT TO CL-REF
           ELSE
              IF LP-TRCD = "RA"
              ADD LP-TRAMT TO AH-REF
           ELSE
              IF LP-TRCD = "RP"
                 ADD LP-TRAMT TO PP-REF.
           GO TO NEXT-PRIOR-REFUND.
       EXIT-FIND-PRIOR-REFUNDS.
           PERFORM CLOSE-LP1-FILE.

      *----------------------------------------------------------------
      * CHECK P&L DATE SECTION.
      *   IN : LN-ACCTNO
      *  OUT : PL-DATE-OKAY -  "Y" = ACCOUNT WAS PUT IN P&L THIS MONTH.
      *                     -  "N" = ACCOUNT WAS NOT PUT IN P&L THIS MONTH.
      *----------------------------------------------------------------
      *****************************************************************
       CHECK-PL-DATE SECTION.
      *****************************************************************

           MOVE "N" TO PL-DATE-OKAY.
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-PRIOR-PL-DATE.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO EXIT-CHECK-PL-DATE.
           IF (LP-BRNO NOT = LN-OWNBR) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-CHECK-PL-DATE.
           IF LP-REV = "Y"
              GO TO NEXT-PRIOR-PL-DATE.

      *----------------------------------------------------------------
      * TEST TRANS DATE AGAINST BEG AND END OF MONTH.
      *----------------------------------------------------------------
           IF (LP-TRDATE < BEG-LN-DATE) OR (LP-TRDATE > END-LN-DATE)
              GO TO NEXT-PRIOR-PL-DATE.

      *----------------------------------------------------------------
      * TEST TRANSACTION CODES FOR P&L.
      *----------------------------------------------------------------
           IF LP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
              MOVE "Y" TO PL-DATE-OKAY
              GO TO EXIT-CHECK-PL-DATE
           ELSE
              MOVE "N" TO PL-DATE-OKAY.
           GO TO NEXT-PRIOR-PL-DATE.
       EXIT-CHECK-PL-DATE.
           PERFORM CLOSE-LP1-FILE.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           MOVE "SN N020CL1." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO BNKDGA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BNKDGA-SYSDATE.
           DISPLAY BNKDGA-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BNKDGA-FORM-FIELDS TO WR-BUF.
           MOVE BNKDGA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE BNKDGA-HELP--FILE TO HELP-LINK-FILE.
           MOVE BNKDGA-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BNKDGA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARA SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
      *----------------------------------------------------------------
       CLEAR-TT-TOTALS.
           PERFORM CL-TT-LEVEL VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
                                 AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
                                 AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 9.
      *----------------------------------------------------------------
       CLEAR-TT-BRANCH.
           MOVE 1 TO SUB.
           PERFORM CL-TT-LEVEL VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
                               AFTER   SUB2 FROM 1 BY 1 UNTIL SUB2 > 9.
      *----------------------------------------------------------------
       CL-TT-LEVEL.
           MOVE 0 TO TT-AMT(SUB ,SUB1, SUB2).
      *----------------------------------------------------------------
       PRINT-TT-TOTALS.
           MOVE TOTAL-TYPE(SUB1) TO D-TOTAL-TYPE.
           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 8
              MOVE TT-AMT(SUB SUB1 SUB2) TO D-TT-AMT(SUB2)
           END-PERFORM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE TT-AMT(SUB, SUB1, 9) TO D-TT-MAINT.
           PERFORM WRITE-DETAIL-LINE.
      *----------------------------------------------------------------
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 MOVE "INVALID STATE PARAMETER RECORD" TO MESS
                 PERFORM SEND-MESS
                 MOVE "X" TO EXT-EOBUF-ERRCD
                 GO TO IO-ERROR.
      *----------------------------------------------------------------

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".

      *----------------------------------------------------------------
      * IO ROUTINES FOR PAID OUT KEY FILE.
      *----------------------------------------------------------------
       OPEN-WP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WP-IFN TO E-FILE.
           OPEN OUTPUT WP-FILE.
           IF IO-LOCKED
              GO TO OPEN-WP-FILE-OUTPUT.
           IF IO-ALREADY-OPEN
              CLOSE WP-FILE
              GO TO OPEN-WP-FILE-OUTPUT.
           UNLOCK WP-FILE.
       OPEN-WP-FILE.
           PERFORM OPEN-IT.
           MOVE WP-IFN TO E-FILE.
           OPEN I-O WP-FILE.
           IF IO-LOCKED
              GO TO OPEN-WP-FILE.
           IF IO-ALREADY-OPEN
              CLOSE WP-FILE
              GO TO OPEN-WP-FILE.
           UNLOCK WP-FILE.
       READ-WP-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WP-IFN TO E-FILE.
           MOVE WP-KEY TO E-KEYX.
           READ WP-FILE NEXT.
           IF IO-LOCKED
              GO TO READ-WP-FILE-NEXT.
       START-WP-FILE.
           PERFORM START-IT.
           MOVE WP-IFN TO E-FILE.
           MOVE WP-KEY TO E-KEYX.
           START WP-FILE KEY NOT < WP-KEY.
           UNLOCK WP-FILE.
       WRITE-WP-FILE.
           PERFORM WRITE-IT.
           MOVE WP-IFN TO E-FILE.
           MOVE WP-KEY TO E-KEYX.
           WRITE WP-REC.
           IF IO-LOCKED
              GO TO WRITE-WP-FILE.
           UNLOCK WP-FILE.
       CLOSE-WP-FILE.
           PERFORM CLOSE-IT.
           CLOSE WP-FILE.

      *----------------------------------------------------------------
      * IO-ROUTINES FOR CHARGED OFF KEY FILE.
      *----------------------------------------------------------------
       OPEN-WC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WC-IFN TO E-FILE.
           OPEN OUTPUT WC-FILE.
           IF IO-LOCKED
              GO TO OPEN-WC-FILE-OUTPUT.
           IF IO-ALREADY-OPEN
              CLOSE WC-FILE
              GO TO OPEN-WC-FILE-OUTPUT.
           UNLOCK WC-FILE.
       OPEN-WC-FILE.
           PERFORM OPEN-IT.
           MOVE WC-IFN TO E-FILE.
           OPEN I-O WC-FILE.
           IF IO-LOCKED
              GO TO OPEN-WC-FILE.
           IF IO-ALREADY-OPEN
              CLOSE WC-FILE
              GO TO OPEN-WC-FILE.
           UNLOCK WC-FILE.
       READ-WC-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WC-IFN TO E-FILE.
           MOVE WC-KEY TO E-KEYX.
           READ WC-FILE NEXT.
           IF IO-LOCKED
              GO TO READ-WC-FILE-NEXT.
       START-WC-FILE.
           PERFORM START-IT.
           MOVE WC-IFN TO E-FILE.
           MOVE WC-KEY TO E-KEYX.
           START WC-FILE KEY NOT < WC-KEY.
           UNLOCK WC-FILE.
       WRITE-WC-FILE.
           PERFORM WRITE-IT.
           MOVE WC-IFN TO E-FILE.
           MOVE WC-KEY TO E-KEYX.
           WRITE WC-REC.
           IF IO-LOCKED
              GO TO WRITE-WC-FILE.
           UNLOCK WC-FILE.
       CLOSE-WC-FILE.
           PERFORM CLOSE-IT.
           CLOSE WC-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *----------------------------------------------------------------
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           MOVE WP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WC-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/GATXMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BNKDGA-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
