       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EMCARG.
       DATE-WRITTEN. 12-01-88
      ******************************************************************
      *                      (LP)
      *  DESCRIPTION:  END OF MONTH CASH AND BANK REGISTER
      *
      *  PRINTS ONLY TRC TYPES "CA", "RR", "RC", "RP", "RD", "BK", 
      *                          "ID", "IW" OR "DB"
      *              TRP "RV" THAT ARE "NSF"
      *              TRD DEPOSITS
      *
      *  REV:
      *  KEC 091495 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091595 CHANGE MILITARY TIME TO AM/PM ON HEADER.
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  BAH 093095 ADDED D-TILL WHICH IS TC-ACCTNO
      *  KEC 101295 COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  KEC 101695 MADE CHANGE TO HEADER TO PRINT VALID BRANCH NAME.
      *  BLL 111095 INCREASE AMT & GL AMT ON DETAIL LINE
      *             TO PRINT MILLION
      *  JFF 111095 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  JTG 112195 CHANGED TO PRINT CLASS ON DEALER RECEIPTS
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  KEC 113095 FIXED LOCAL-SPOOL TO NOT PRINT THE RANGE-LINES.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             IF LOCAL-PRINT-FG NOT = "Y" IN INITIALIZATION.
      *  KEC 121395 ADDED LOGIC TO PRINT ERROR MESSAGE IF ERROR ON
      *             READING THE GI-FILE.
      *  BAH 070996 ADDED LP-FILE SO WE COULD GET THE DATE OF THE PAYMENT
      *             THAT WAS REVERSED VIA "NSF", USING TP-REVSEQNO, RAC #0013
      *  BAH 031197 ON THE CHANGE ABOVE, IF INTERBRANCH PAYMENT, DONT
      *             TRY TO GET PAYMENT REVERSED, JUST PRINT "POSTED FOR :"
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  JTG 010898 ADDED PRINT OF BORROWER NAMES FOR NFS TRANSACTIONS
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BLV 022498 ADD GPENV; CHANGED TEST-BK-REASON FOR WORLD TO PRINT
      *             DIFFERENT TYPE & REASON FOR INT DEPOSITS, WITHDRAWALS
      *  GLF 022798 REMOVED "END OF MONTH" FORM TITLE
      *  BAH 030698 CHANGED WK2-LINE TO PIC 9(3) FROM 99 BECAUSE TP-LINE
      *             PIC 99 COMP ACTUALLY STORES A 3 DIGIT NUMBER !.
      *  BLV 043098 FIXED - ALL BRANCHES DISPLAYED "USER BRANCH ONLY"
      *  BAH 062598 ADDED TC-TILLNO, USE TO STORE IT IN TC-ACCTNO BUT NOW
      *             TILLNO IS ALPHA
      *  GLF 081999 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  KEC 001002 {PR#00000} MERCURY FINANCE [CINDY]
      *              CHANGED WK2-LINE FROM 9(3) TO 9(2) COMP JUST LIKE
      *              TP-LINE; BETH ORIGINALLY MADE THE CHANGE BUT HAD
      *              GOT CAUGHT IN LP/EMTRRG SO CHANGED TO 9(2) COMP.
      *  GLB 010702  MAKE NECESSARY CHANGES TO INCLUDE NEW TC-TYPE "RD"
      *              RECEIPT OF DEALER CHECK
      *  CS  020410 WHEN POSTING INTERBRANCH NSF REVERSE, DETAIL LINE SHOWS
      *             POSTED FOR FOR BOTH THE "TO" & "FROM" BRANCH. DONT PRINT
      *             FOR THE FOR BRANCH [KEVIN-MERCURY FINANCE]
      *  CS  020410 ON INTERBRANCH NSF REVERSALS, GL DISTRIBUTION WAS NOT
      *             CORRECT. ALSO CHANGED 020410 ENTRY ABOVE TO PRINT "TO" &
      *             "FROM" BRANCH
      *  CS  020705 GL SUMMARY SHOWED REVERSAL OF "OT" CHARGES AS A CREDIT AND
      *             SHOULD BE A DEBIT CAUSING THE SUMMARY TO BE OUT OF BALANCE.
      *             CHANGED SETUP-TP-GL TO MULTIPLY GLAMT BY -1.
      *             [PR#372 REGENCY]
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *   CS 120214 ADDED IF TR1-CODE = 'C' ADDED TC-TYPE 'ID' & 'IW'
      *             IF NOT (TC-TYPE = "CA" OR "RR" OR "RC"
      *                               OR "RP" OR "RD" OR "BK" OR
      *                                  "ID" OR "IW"  WORLD PR#790
      *             'ID' OR 'IW' ARE ENTERED IN THE BANK DEPOSIT/WITHDRAWL
      *             SCREENS AND UPDATE CA-RUNNING WITH THE AMOUNT & FORCE
      *             THE FIRST LEVEL OF GL FROM  BR-GLONHAND
      *  BAH 121119 ORDER THE WORK FILE BETTER FOR WORLD ONLY. THE INT.DEPO
      *             WERE MIXED IN WITH ALL THE OTHER "BK" TRANSACTIONS. I
      *             ADDED A SECTION FOR WORLD TO SET-WK2-TYPE DIFFERENT
      *                                                    WORLD PR# 829
      *  BAH 141021 PRINT TILL (DRAWER) FOR RESERVE RECEIPTS, CFSC PL#863
      *  BLM 161018 PRINT LINE FOR CHECK DEPOSIT & ADD TO TOTALS & GL
      *             SUMMARY IF BR-ALLOW-SCANNED-CKS = Y, WORLD PR# 1188
      *  BAH 161103 CHANGED D-TILL TO Z9
      *  BAH 170526 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 190516 CHANGED HOLD-GI1-KEY TO X(7) FROM X(6), QA0023
      *  BAH 20190926 REMOVED ACCESS-CALL BWFILE, TRP, TRC, TRD
      *  BAH 2021.0308 ADDED TC-TYPE "DB" DEBIT UMC #1490
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      *  MB  20230906 PERFORMANCE TUNING  EMBEDDED SQL WITH LIMITED COLUMNS
      ********************************************************************
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

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR         PIC 9(4).
               05  WK-GLNO       PIC 9(11).
           03  WK-DRGLAMT        PIC S9(9)V99    COMP-3.
           03  WK-CRGLAMT        PIC S9(9)V99    COMP-3.

       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH          PIC 9(4).
               05  WK2-TYPE            PIC 99.
               05  WK2-DATE            PIC 9(8).
               05  WK2-CODE            PIC X.
               05  WK2-CLASS           PIC 9(3).
               05  WK2-CKNO            PIC 9(6).
               05  WK2-SEQ             PIC 9(5).
               05  WK2-ACCTNO          PIC 9(8).
               05  WK2-LINE            PIC 9(3).
           03  WK2-DATA                PIC X(250).
           03  WK2-TRC-DATA            REDEFINES WK2-DATA.
               05  WK2-TC-TYPE             PIC X(2).
               05  WK2-TC-DATE             PIC 9(8).
               05  WK2-TC-AMT              PIC S9(7)V99.
               05  WK2-TC-PAYEE            PIC X(60).
               05  WK2-TC-REF              PIC 9(6).
               05  WK2-TC-REASON           PIC X(30).
               05  WK2-TC-GLNO-1           PIC 9(11).
               05  WK2-TC-GLAMT-1          PIC S9(7)V99.
               05  WK2-TC-GLNO-2           PIC 9(11).
               05  WK2-TC-GLAMT-2          PIC S9(7)V99.
               05  WK2-TC-GLNO-3           PIC 9(11).
               05  WK2-TC-GLAMT-3          PIC S9(7)V99.
               05  WK2-TC-GLNO-4           PIC 9(11).
               05  WK2-TC-GLAMT-4          PIC S9(7)V99.
               05  WK2-TC-GLNO-5           PIC 9(11).
               05  WK2-TC-GLAMT-5          PIC S9(7)V99.
               05  WK2-TC-DLRVNO           PIC 9.
               05  WK2-TC-DLNO             PIC 9(4)V99.
               05  WK2-TC-TILLNO           PIC 9(2).
               05  WK2-TC-DEBIT-REFCD      PIC X(5).
           03  WK2-TRD-DATA            REDEFINES WK2-DATA.
               05  WK2-TD-DEPOSIT          PIC S9(7)V99.
               05  WK2-TD-CHECK-DEPOSIT    PIC S9(7)V99.
               05  WK2-TD-BANK             PIC 9(11).
               05  WK2-TD-CASH             PIC 9(11).
               05  WK2-TD-GLOVERSHORT      PIC 9(11).
               05  WK2-TD-OVERSHORT        PIC S9(5)V99.
           03  WK2-TRP-DATA            REDEFINES WK2-DATA.
               05  WK2-TP-SSNO             PIC 9(10).
               05  WK2-TP-REFNO            PIC X(5).
               05  WK2-TP-PAYDATE          PIC 9(8).
               05  WK2-TP-TRAMT            PIC S9(7)V99.
               05  WK2-TP-APINT            PIC S9(7)V99.
               05  WK2-TP-APLC             PIC S9(7)V99.
               05  WK2-TP-APCUR            PIC S9(7)V99.
               05  WK2-TP-GLNO-1           PIC 9(11).
               05  WK2-TP-GLAMT-1          PIC S9(7)V99.
               05  WK2-TP-GLNO-2           PIC 9(11).
               05  WK2-TP-GLAMT-2          PIC S9(7)V99.
               05  WK2-TP-GLNO-3           PIC 9(11).
               05  WK2-TP-GLAMT-3          PIC S9(7)V99.
               05  WK2-TP-POSTING-BR       PIC 9(4).
               05  WK2-TP-OTHBR-FG         PIC X.
               05  WK2-TP-REV              PIC X.
               05  WK2-TP-REVSEQNO         PIC 9(5). 

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMCARG.CBL S35 11/22/23-07:53:11 >".
      ****************************
      *   F I L E   P A T H S    *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  WK2-IFN              PIC X(3)    VALUE "WK2".
       01  WK-IFN               PIC X(2)    VALUE "WK".

       01  WK2-PATH             PIC X(35).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUBX                 PIC 9(6) COMP-3.
       01  FIRST-TIME           PIC X               VALUE "Y".
       01  RECORDS-FOUND        PIC X               VALUE " ".
           88  NO-RECORDS-FOUND                     VALUE "N".
       01  GL-SUM-FG            PIC X               VALUE SPACES.
       01  MSEL-LIST            PIC X(5)            VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)            VALUE "MESS.".
       01  WORK-GLNO            PIC 9(11).
       01  WORK-GL              REDEFINES WORK-GLNO.
           03  WORK-GLBR        PIC 9(4).
           03  WORK-GLNUM       PIC 9(7).
       01  HOLD-BRNO            PIC 9(4)            VALUE 0.
       01  HOLD-GI1-KEY         PIC X(7)            VALUE SPACES.
       01  HOLD-TYPE            PIC 99              VALUE 0.
       01  GLMAX                PIC 9       VALUE 0.
       01  GLAMT                PIC S9(9)V99 COMP-3 VALUE 0.
       01  GLNO                 PIC 9(11)    VALUE 0.

       01  WK-NSF-TEXT.
           03  FILLER           PIC X(13) VALUE "ORIG POSTED: ".
           03  WK-NSF-DATE      PIC 99/99/99.
       01  WK-NSF-TEXT2.
           03  FILLER           PIC X(09) VALUE "POSTED : ".
           03  WK-INTERBRANCH   PIC X(4).
           03  WK-NSF-BR        PIC 9(4).

       01  BANK-TOTAL           PIC S9(9)V99 COMP-3 VALUE 0.
       01  NSF-TOTAL            PIC S9(9)V99 COMP-3 VALUE 0.
       01  DEPOSIT-TOTAL        PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-DEPOSIT-TOTAL     PIC S9(9)V99 COMP-3 VALUE 0.
       01  RECEIPT-TOTAL        PIC S9(9)V99 COMP-3 VALUE 0.
       01  DISBURSE-TOTAL       PIC S9(9)V99 COMP-3 VALUE 0.
       01  DEALER-TOTAL         PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-TOTDR             PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-TOTCR             PIC S9(9)V99 COMP-3 VALUE 0.
       01  DEBIT-RECEIPT-TOTAL  PIC S9(9)V99 COMP-3 VALUE 0.
       01  DEBIT-DISBURSE-TOTAL PIC S9(9)V99 COMP-3 VALUE 0.

      *-----------------------------------------------------------------
       01  WS-FIELDS.
           03  WS-ERROR-GI-FILE              PIC X(1)     VALUE "N".
           03  WS-ERROR-GI-MESSAGE.
               05  FILLER                    PIC X(10)    VALUE SPACES.
               05  FILLER                    PIC X(20)    VALUE ALL "*".
               05  FILLER                    PIC X(45)    VALUE
               "ERROR OCCURED TRYING TO ACCESS GI-RECORD --- ".
               05  FILLER                    PIC X(28)    VALUE
               "THIS BRANCH HAS BEEN SKIPPED".
               05  FILLER                    PIC X(20)    VALUE ALL "*".
               05  FILLER                    PIC X(9)     VALUE SPACES.

      *-----------------------------------------------------------------
       COPY "LIBLP/EMSFRG_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.
       COPY "LIBGB/PASSWDW.CPY".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(22)    VALUE " ".
           03  FILLER           PIC X(48)    VALUE
           "       - CASH/BANK DISBURSEMENT REGISTER -      ".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(20).
           03  FILLER      PIC X(70)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(17) VALUE "TYPE     --DATE--".
           03  FILLER      PIC X(7)  VALUE " ".
           03  FILLER      PIC X(14) VALUE "AMOUNT  PAYEE".
           03  FILLER      PIC X(27) VALUE " ".
           03  FILLER      PIC X(14) VALUE "REF #  REASON".
           03  FILLER      PIC X(29) VALUE " ".
           03  FILLER      PIC X(9)  VALUE "GL-NUMBER".
           03  FILLER      PIC X(7)  VALUE " ".
           03  FILLER      PIC X(7)  VALUE "AMOUNT".

       01  HEAD5.
           03  FILLER      PIC X(35) VALUE
           "------------GENERAL LEDGER SUMMARY:".

       01  HEAD6.
           03  FILLER      PIC X(53) VALUE
           " GL ACCOUNT   NAME".
           03  FILLER      PIC X(35) VALUE
           "DEBIT         CREDIT            NET".
      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-TYPE      PIC X(8).
           03  FILLER      PIC X.
           03  D-CKDATE    PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-CKAMT     PIC -----,---.99.
           03  FILLER      PIC XX.
           03  D-PAYEE     PIC X(30).
           03  D-RESERVE-STUFF REDEFINES D-PAYEE.
               05  D-DLPAYEE PIC X(19).
               05  FILLER    PIC X.
               05  D-DLCLASS PIC 9(2).
               05  FILLER    PIC X.
               05  D-DLNO    PIC Z(4).99 BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-REFNO     PIC ZZZZZ9     BLANK ZERO.
           03  D-DEBIT-REFCD REDEFINES D-REFNO
                           PIC X(6).
           03  FILLER      PIC XX.
           03  D-REASON    PIC X(30).
           03  FILLER      PIC XX.
           03  D-TILL      PIC Z9.
           03  FILLER      PIC X.
           03  D-GLBR      PIC 9(4).
           03  D-DASH      PIC X.
           03  D-GLNUM     PIC 9(7).
           03  FILLER      PIC X.
           03  D-GLAMT     PIC -----,---.99.

      *-----------------------------------------------------------------
       01  T-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(23).
           03  T-CKTOTAL   PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  S-LINE REDEFINES DETAIL-LINE.
           03  S-GLBR      PIC 9(4).
           03  S-DASH      PIC X.
           03  S-GLNUM     PIC 9(7).
           03  FILLER      PIC X(3).
           03  S-GLNAME    PIC X(30).
           03  S-DRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-CRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-NET       PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  ERROR-GI-LINE REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(30).
           03  D-ERR-PROMPT1           PIC X(5).
           03  D-ERR-DAY               PIC 9(2).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT2           PIC X(8).
           03  D-ERR-BRNO              PIC 9(4).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT3           PIC X(6).
           03  D-ERR-CODE              PIC X(1).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT4           PIC X(7).
           03  D-ERR-CLASS             PIC 9(2).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT5           PIC X(8).
           03  D-ERR-SUBKEY            PIC X(9).
           03  FILLER                  PIC X(30).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/EOSPOOLW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMCARG_DEF.CPY".
       COPY "LIBLP/EMCARG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMCARG_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK2-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK2-FILE WK-FILE.
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
           MOVE "EMCARG" TO FORM-PROG VDU-FORM-NAME.

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
                                              LP-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              TRD-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              STRING "REPORT IN PROGRESS...", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BW1-FILE.

           PERFORM SORT-TRP-FILE.
           PERFORM SORT-TRC-FILE.
           PERFORM SORT-TRD-FILE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.
           MOVE "N" TO WS-ERROR-GI-FILE.

      *-----------------------------------------------------------------
       NEXT-CK.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           IF WK2-CODE = "D"
              MOVE WK2-BRANCH TO TD-BRNO
              MOVE WK2-DATE   TO TD-DATE
      *        PERFORM READ-TRD1-FILE
              MOVE WK2-TD-DEPOSIT       TO TD-DEPOSIT
              MOVE WK2-TD-CHECK-DEPOSIT TO TD-CHECK-DEPOSIT
              MOVE WK2-TD-BANK          TO TD-BANK
              MOVE WK2-TD-CASH          TO TD-CASH
              MOVE WK2-TD-GLOVERSHORT   TO TD-GLOVERSHORT
              MOVE WK2-TD-OVERSHORT     TO TD-OVERSHORT
           ELSE
           IF WK2-CODE = "C"
              MOVE WK2-BRANCH TO TC-BRNO
              MOVE WK2-DATE   TO TC-KEY-DATE
              MOVE WK2-CKNO   TO TC-CKNO
              MOVE WK2-CLASS  TO TC-CLASS
              MOVE WK2-SEQ    TO TC-SEQ
      *        PERFORM READ-TRC1-FILE
              MOVE WK2-TC-TYPE             TO TC-TYPE
              MOVE WK2-TC-DATE             TO TC-DATE
              MOVE WK2-TC-AMT              TO TC-AMT
              MOVE WK2-TC-PAYEE            TO TC-PAYEE
              MOVE WK2-TC-REF              TO TC-REF
              MOVE WK2-TC-REASON           TO TC-REASON
              MOVE WK2-TC-GLNO-1           TO TC-GLNO(1)
              MOVE WK2-TC-GLAMT-1          TO TC-GLAMT(1)
              MOVE WK2-TC-GLNO-2           TO TC-GLNO(2)
              MOVE WK2-TC-GLAMT-2          TO TC-GLAMT(2)
              MOVE WK2-TC-GLNO-3           TO TC-GLNO(3)
              MOVE WK2-TC-GLAMT-3          TO TC-GLAMT(3)
              MOVE WK2-TC-GLNO-4           TO TC-GLNO(4)
              MOVE WK2-TC-GLAMT-4          TO TC-GLAMT(4)
              MOVE WK2-TC-GLNO-5           TO TC-GLNO(5)
              MOVE WK2-TC-GLAMT-5          TO TC-GLAMT(5)
              MOVE WK2-TC-DLRVNO           TO TC-DLRVNO
              MOVE WK2-TC-DLNO             TO TC-DLNO
              MOVE WK2-TC-TILLNO           TO TC-TILLNO
              MOVE WK2-TC-DEBIT-REFCD      TO TC-DEBIT-REFCD
           ELSE
              MOVE WK2-BRANCH TO TP-BRNO
              MOVE WK2-DATE   TO TP-DATE
      *       MOVE WK2-CODE   TO TP-CODE
      *       MOVE 0          TO TP-POOLID
              MOVE WK2-CLASS  TO TP-CLASS
              MOVE WK2-ACCTNO TO TP-ACCTNO
              MOVE WK2-LINE   TO TP-LINE
      *        PERFORM READ-TRP1-FILE.
              MOVE WK2-TP-SSNO     TO TP-SSNO
              MOVE WK2-TP-REFNO    TO TP-REFNO
              MOVE WK2-TP-PAYDATE  TO TP-PAYDATE
              MOVE WK2-TP-TRAMT    TO TP-TRAMT
              MOVE WK2-TP-APINT    TO TP-APINT
              MOVE WK2-TP-APLC     TO TP-APLC
              MOVE WK2-TP-APCUR    TO TP-APCUR
              MOVE WK2-TP-GLNO-1   TO TP-GLNO(1)
              MOVE WK2-TP-GLAMT-1  TO TP-GLAMT(1)
              MOVE WK2-TP-GLNO-2   TO TP-GLNO(2)
              MOVE WK2-TP-GLAMT-2  TO TP-GLAMT(2)
              MOVE WK2-TP-GLNO-3   TO TP-GLNO(3)
              MOVE WK2-TP-GLAMT-3  TO TP-GLAMT(3)
              MOVE WK2-TP-POSTING-BR  TO TP-POSTING-BR
              MOVE WK2-TP-OTHBR-FG    TO TP-OTHBR-FG
              MOVE WK2-TP-REV         TO TP-REV
              MOVE WK2-TP-REVSEQNO    TO TP-REVSEQNO.

      *     IF IO-FG NOT = 0
      *        MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE WK2-BRANCH TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           IF WK2-BRANCH NOT = HOLD-BRNO
              PERFORM BRANCH-TOTALS
              MOVE WK2-BRANCH TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           IF ( WS-ERROR-GI-FILE = "Y" )
              PERFORM WRITE-ERROR-LINE
              PERFORM CLOSE-TRC1-FILE
              PERFORM CLOSE-TRD1-FILE
              PERFORM CLOSE-TRP1-FILE
              PERFORM CLOSE-BW1-FILE
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE-OUTPUT
              GO TO NEXT-GROUP-LOC.

           GO TO NEXT-CK.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
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
              PERFORM BRANCH-TOTALS
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE EMCARG-QUEUE-POS TO Q-POS.
           MOVE EMCARG-COPIES-POS TO C-POS.
           MOVE EMCARG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

      * CREATE WORK FILE FOR GL SUMMARY:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE GL WORKFILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
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

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GI1-FILE.
           PERFORM OPEN-GL1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **********************************
       EO-EXEC-AUDIT SECTION.
      **********************************

           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.
           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      **********************************
       ENTER-PARAMETERS SECTION.
      **********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF ( DAY-OF-MONTH = 99 )
                 MOVE "SNNN020DA1." TO VDU
                 MOVE 99 TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO  DA-01
              ELSE
                 GO TO  DA-01.

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
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
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

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       DTE.
           IF EXT-ROUTE-BUF NOT  = "00"
              GO TO DA-01.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

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
              GO TO LOCAL-PRINT.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO DTE
              ELSE
                 GO TO BR-02.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           IF VDUNUM0 = 0 GO TO DA-01.
           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

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
           MOVE LEGEND-LOCAL-SPOOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              IF ( DAY-OF-MONTH = 99 )
                 MOVE "SNNN020DA1." TO VDU
                 MOVE 99 TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO  DA-01
              ELSE
                 GO TO  DA-01.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

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

           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.

      *---------------------------------------------------------
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL   :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
       SORT-TRP-FILE SECTION.
      ***************************************
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

      *     MOVE 0                   TO TP-CLASS TP-ACCTNO TP-LINE
      *                                 QTRP1-WBEG-CLASS
      *                                 QTRP1-WBEG-ACCTNO
      *                                 QTRP1-WBEG-LINE.
      *     MOVE ALL "9"             TO QTRP1-WEND-CLASS
      *                                 QTRP1-WEND-ACCTNO
      *                                 QTRP1-WEND-LINE.
           PERFORM START-TRP1-FILE.  *> EMBEDDED  TRP

       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.  *> EMBEDDED  TRP
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-TRP-EXIT.

      * THIS CHECK ALSO DONE IN CURSOR
           IF TP-DATE > ENDING-DATE
              GO TO SORT-TRP-EXIT.

      * THIS CHECK ALSO DONE IN CURSOR
           IF TP-REFNO NOT = "NSF  "
              GO TO SORT-TRP-NEXT.

           MOVE 0 TO WK2-CLASS WK2-CKNO WK2-SEQ WK2-ACCTNO WK2-LINE.

           MOVE TP-DATE   TO WK2-DATE.
           MOVE TP-BRNO   TO WK2-BRANCH.
      *    MOVE TP-CODE   TO WK2-CODE.
           MOVE "P"       TO WK2-CODE.

           MOVE 5         TO WK2-TYPE.
           MOVE TP-CLASS  TO WK2-CLASS.
           MOVE TP-ACCTNO TO WK2-ACCTNO.
           MOVE TP-LINE   TO WK2-LINE.
           MOVE  TP-SSNO     TO  WK2-TP-SSNO.
           MOVE  TP-REFNO    TO  WK2-TP-REFNO.
           MOVE  TP-PAYDATE  TO WK2-TP-PAYDATE.
           MOVE  TP-TRAMT    TO WK2-TP-TRAMT.
           MOVE  TP-APINT    TO WK2-TP-APINT.
           MOVE  TP-APLC     TO WK2-TP-APLC.
           MOVE  TP-APCUR    TO WK2-TP-APCUR..
           MOVE  TP-GLNO(1)   TO WK2-TP-GLNO-1.
           MOVE  TP-GLAMT(1)  TO WK2-TP-GLAMT-1.
           MOVE  TP-GLNO(2)   TO WK2-TP-GLNO-2.
           MOVE  TP-GLAMT(2)  TO WK2-TP-GLAMT-2.
           MOVE  TP-GLNO(3)   TO WK2-TP-GLNO-3.   
           MOVE  TP-GLAMT(3)  TO WK2-TP-GLAMT-3. 
           MOVE  TP-POSTING-BR  TO WK2-TP-POSTING-BR. 
           MOVE  TP-OTHBR-FG    TO WK2-TP-OTHBR-FG.     
           MOVE  TP-REV         TO WK2-TP-REV. 
           MOVE  TP-REVSEQNO    TO WK2-TP-REVSEQNO. 

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRP-NEXT.

       SORT-TRP-EXIT.
           EXIT.

      ***************************************
       SORT-TRC-FILE SECTION.
      ***************************************
           PERFORM OPEN-TRC1-FILE.

           MOVE BR-NO               TO TC-BRNO
                                       QTRC1-WBEG-BRNO.

           MOVE BEGINNING-DATE      TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE ENDING-DATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.

      *     MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
      *                                 QTRC1-WBEG-CKNO
      *                                 QTRC1-WBEG-CLASS
      *                                 QTRC1-WBEG-SEQ.
      *     MOVE ALL "9"             TO QTRC1-WEND-CKNO
      *                                 QTRC1-WEND-CLASS
      *                                 QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.   *> EMBEDDED  TRC

       SORT-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.   *> EMBEDDED  TRC
           IF (IO-FG NOT = 0)            *> OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO SORT-TRC-EXIT.

      *     IF TC-KEY-DATE > ENDING-DATE
      *        GO TO SORT-TRC-EXIT.

      * THIS CHECK ALSO DONE IN CURSOR
           IF NOT (TC-TYPE = "CA" OR "RR" OR "RC"
                   OR "RP" OR "RD" OR "BK" OR "ID" OR "IW" OR "DB")
              GO TO SORT-TRC-NEXT.

           MOVE 0 TO WK2-CLASS WK2-CKNO WK2-SEQ WK2-ACCTNO WK2-LINE.

           MOVE TC-KEY-DATE TO WK2-DATE.
           MOVE TC-BRNO     TO WK2-BRANCH.
           MOVE "C"         TO WK2-CODE.
           MOVE TC-CKNO     TO WK2-CKNO
           MOVE TC-CLASS    TO WK2-CLASS.
           MOVE TC-SEQ      TO WK2-SEQ.

           IF TC-TYPE = "BK"
              MOVE 1 TO WK2-TYPE
           ELSE
              IF TC-TYPE = "RR"
                 MOVE 4 TO WK2-TYPE
              ELSE
                 IF TC-TYPE = "RC"
                    MOVE 5 TO WK2-TYPE
                 ELSE
                    IF TC-TYPE = "RP"
                       MOVE 6 TO WK2-TYPE
                    ELSE
                       IF TC-TYPE = "RD"
                          MOVE 7 TO WK2-TYPE
                       ELSE
                          IF TC-TYPE = "CA"
                             IF TC-AMT < 0
                                MOVE 8 TO WK2-TYPE
                             ELSE
                                MOVE 9 TO WK2-TYPE.

      *  "INT.DEPO"
           IF TC-TYPE = "BK" OR "ID" OR "IW"
              IF TC-AMT < 0
                 MOVE 1 TO WK2-TYPE
              ELSE
      * "INT.WITH"
                 IF TC-TYPE = "IW"
                    MOVE 3  TO WK2-TYPE
                 ELSE
      * WITHDRAWAL
                    MOVE 4 TO WK2-TYPE.

           IF TC-TYPE = "CA"
      * RECEIPTS
              IF TC-AMT < 0
                 MOVE 6 TO WK2-TYPE
             ELSE
      * DISBURSEMENTS
                  MOVE 7 TO WK2-TYPE.

      * RECEIPT DEALER RESERVE FROM PRIN
           IF TC-TYPE = "RR"
              MOVE 8 TO WK2-TYPE
           ELSE
      * RECEIPT DEALER RESERVE FROM CHARGES
              IF TC-TYPE = "RC"
                 MOVE 9 TO WK2-TYPE
              ELSE
      * RECEIPT DEALER PARTICIPATION HELD
                 IF TC-TYPE = "RP"
                    MOVE 10 TO WK2-TYPE
                 ELSE
      * RECEIPT DEALER PROCEEDS
                    IF TC-TYPE = "RD"
                       MOVE 11 TO WK2-TYPE.

           MOVE TC-TYPE             TO WK2-TC-TYPE.
           MOVE TC-DATE             TO WK2-TC-DATE.
           MOVE TC-AMT              TO WK2-TC-AMT.
           MOVE TC-PAYEE            TO WK2-TC-PAYEE.
           MOVE TC-REF              TO WK2-TC-REF.
           MOVE TC-REASON           TO WK2-TC-REASON.
           MOVE TC-GLNO(1)          TO WK2-TC-GLNO-1.
           MOVE TC-GLAMT(1)         TO WK2-TC-GLAMT-1.
           MOVE TC-GLNO(2)          TO WK2-TC-GLNO-2.
           MOVE TC-GLAMT(2)         TO WK2-TC-GLAMT-2.
           MOVE TC-GLNO(3)          TO WK2-TC-GLNO-3.
           MOVE TC-GLAMT(3)         TO WK2-TC-GLAMT-3.
           MOVE TC-GLNO(4)          TO WK2-TC-GLNO-4.
           MOVE TC-GLAMT(4)         TO WK2-TC-GLAMT-4.
           MOVE TC-GLNO(5)          TO WK2-TC-GLNO-5.
           MOVE TC-GLAMT(5)         TO WK2-TC-GLAMT-5.
           MOVE TC-DLRVNO           TO WK2-TC-DLRVNO.
           MOVE TC-DLNO             TO WK2-TC-DLNO.
           MOVE TC-TILLNO           TO WK2-TC-TILLNO.
           MOVE TC-DEBIT-REFCD      TO WK2-TC-DEBIT-REFCD.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRC-NEXT.

       SORT-TRC-EXIT.
           EXIT.

      ***************************************
       SORT-TRD-FILE SECTION.
      ***************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE BR-NO          TO TD-BRNO
                                  QTRD1-WBEG-BRNO.

           MOVE BEGINNING-DATE       TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE.
           MOVE ENDING-DATE          TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE.

           PERFORM START-TRD1-FILE.  *> EMBEDDED  TRD

       SORT-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT.  *> EMBEDDED  TRD
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO SORT-TRD-EXIT.

           IF TD-DATE > ENDING-DATE
              GO TO SORT-TRD-EXIT.

           MOVE 0 TO WK2-CLASS WK2-CKNO WK2-SEQ WK2-ACCTNO WK2-LINE.

           MOVE TD-DATE TO WK2-DATE.
           MOVE TD-BRNO TO WK2-BRANCH.
           MOVE "D"     TO WK2-CODE.
           MOVE 2       TO WK2-TYPE.

           MOVE TD-DEPOSIT       TO WK2-TD-DEPOSIT.           
           MOVE TD-CHECK-DEPOSIT TO WK2-TD-CHECK-DEPOSIT.    
           MOVE TD-BANK          TO WK2-TD-BANK.            
           MOVE TD-CASH          TO WK2-TD-CASH.              
           MOVE TD-GLOVERSHORT   TO WK2-TD-GLOVERSHORT.      
           MOVE TD-OVERSHORT     TO WK2-TD-OVERSHORT.        

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRD-NEXT.

       SORT-TRD-EXIT.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE WK2-BRANCH TO GI-BRANCH.

           IF ( WK2-CODE = "C" )
              IF ( TC-CLASS = 0 )
                 MOVE BR-GLBANK   TO GI-BANK WK-GLNO
                 MOVE BR-GLONHAND TO GI-CASH
              ELSE
                 MOVE TC-CLASS TO GI-CLASS
                 PERFORM GET-GI
                 IF ( WS-ERROR-GI-FILE = "Y" )
                    GO TO END-DETAIL-LN-PROCESS.

           IF ( WK2-CODE = "P" )
              MOVE TP-CLASS TO GI-CLASS
              PERFORM GET-GI
              IF ( WS-ERROR-GI-FILE = "Y" )
                 GO TO END-DETAIL-LN-PROCESS
              ELSE
                 PERFORM UPDATE-NSF-REVERSAL
                 MOVE 3 TO GLMAX
                 MOVE "BANK NSF" TO D-TYPE
                 MOVE TP-ACCTNO TO D-REFNO
                 COMPUTE D-CKAMT = TP-TRAMT * -1
                 MOVE TP-PAYDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-CKDATE
                 MOVE TP-SSNO TO BW-SSNO
                 PERFORM READ-BW1-FILE
                 IF IO-FG = 0
                    MOVE SPACES TO D-PAYEE
                    STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                      INTO D-PAYEE
                 END-IF
                 IF TP-OTHBR-FG NOT = "G"
                    ADD TP-TRAMT TO NSF-TOTAL
                 END-IF
                 PERFORM GET-NSF-REVERSED.

           IF WK2-CODE = "C"
              PERFORM UPDATE-CHECK-TYPE
              MOVE 5 TO GLMAX
              MOVE TC-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-CKDATE
              MOVE TC-AMT TO D-CKAMT
              MOVE TC-PAYEE TO D-PAYEE
              MOVE TC-REASON TO D-REASON
              MOVE TC-REF TO D-REFNO
              IF TC-TYPE = "DB"
                 IF TC-AMT < 0
                    ADD TC-AMT TO DEBIT-RECEIPT-TOTAL
                    MOVE "DEBIT R" TO D-TYPE
                    MOVE TC-DEBIT-REFCD TO D-DEBIT-REFCD
                 ELSE
                    ADD TC-AMT TO DEBIT-DISBURSE-TOTAL
                    MOVE "DEBIT D" TO D-TYPE
                    MOVE TC-DEBIT-REFCD TO D-DEBIT-REFCD
              ELSE
              IF TC-TYPE = "CA"
                 IF TC-AMT < 0
                    ADD TC-AMT TO RECEIPT-TOTAL
                    MOVE "RECEIPT" TO D-TYPE
                    MOVE TC-TILLNO TO D-TILL
                 ELSE
                    ADD TC-AMT TO DISBURSE-TOTAL
                    MOVE "DISBURSE" TO D-TYPE
                    MOVE TC-TILLNO TO D-TILL
              ELSE
                 IF TC-TYPE = "RR" OR "RC" OR "RP"
                    ADD TC-AMT TO DEALER-TOTAL
                    MOVE "RESERVE" TO D-TYPE
                    PERFORM TEST-RE-OR-SR
                 ELSE
                    IF TC-TYPE = "RD"
                       ADD TC-AMT TO DEALER-TOTAL
                       MOVE "REC PROC" TO D-TYPE
                       PERFORM TEST-RE-OR-SR
                    ELSE
                       ADD TC-AMT TO BANK-TOTAL
                       MOVE "BANK" TO D-TYPE
                       PERFORM TEST-BK-REASON.

           IF WK2-CODE = "D"
              PERFORM UPDATE-DEPOSIT
              MOVE "DEPOSIT" TO D-TYPE
              MOVE TD-DEPOSIT TO D-CKAMT
              MOVE TD-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-CKDATE
              ADD TD-DEPOSIT TO DEPOSIT-TOTAL
              GO TO PRINT-NO-GL.

           MOVE 1 TO SUBX.

           IF WK2-CODE = "C"
              PERFORM SETUP-TC-GL
           ELSE
              PERFORM SETUP-TP-GL.

           IF GLNO NOT = 0
              PERFORM UPDATE-GL-SUMMARY
              MOVE GLNO TO WORK-GLNO
              MOVE WORK-GLBR TO D-GLBR
              MOVE "-" TO D-DASH
              MOVE WORK-GLNUM TO D-GLNUM
              MOVE GLAMT TO D-GLAMT
              MOVE 2 TO SUBX.

      *-----------------------------------------------------------------
       PRINT-NO-GL.
           IF HOLD-TYPE NOT = WK2-TYPE
              IF FIRST-TIME = "Y"
                 MOVE WK2-TYPE TO HOLD-TYPE
              ELSE
                 MOVE WK2-TYPE TO HOLD-TYPE
                 MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE "CHK DEP" TO D-TYPE
              MOVE TD-CHECK-DEPOSIT TO D-CKAMT
              MOVE TD-DATE   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-CKDATE
              ADD TD-CHECK-DEPOSIT TO CK-DEPOSIT-TOTAL
              PERFORM WRITE-DETAIL-LINE.

           IF WK2-CODE = "D"
              GO TO END-DETAIL-LN-PROCESS.

      *-----------------------------------------------------------------
       PRINT-GLDIST.
           IF SUBX > GLMAX
              GO TO END-DETAIL-LN-PROCESS.

           IF WK2-CODE = "C"
              PERFORM SETUP-TC-GL
           ELSE
              PERFORM SETUP-TP-GL.

           IF GLAMT = 0
              ADD 1 TO SUBX
              GO TO PRINT-GLDIST.

           PERFORM UPDATE-GL-SUMMARY.
           MOVE GLNO TO WORK-GLNO.
           MOVE WORK-GLBR TO D-GLBR.
           MOVE "-" TO D-DASH.
           MOVE WORK-GLNUM TO D-GLNUM.
           MOVE GLAMT TO D-GLAMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           ADD 1 TO SUBX.
           GO TO PRINT-GLDIST.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************************************************
      * PRINT ORIGINAL POSTING DATE OF PAYMENT THAT WAS "NSF" REVERSED
      * RAC #0013, 7/9/96 BAH
      ****************************************************************
       GET-NSF-REVERSED SECTION.
      ****************************************************************

      * DONT TRY TO GET PAYMENT REVERSED IF INTER BRANCH PAYMENT !!
           IF TP-BRNO NOT = TP-POSTING-BR
              IF TP-OTHBR-FG = "C"
                MOVE TP-POSTING-BR TO WK-NSF-BR
                MOVE "FOR"         TO WK-INTERBRANCH
                MOVE WK-NSF-TEXT2  TO D-REASON
              ELSE
                MOVE TP-POSTING-BR TO WK-NSF-BR
                MOVE "BY "         TO WK-INTERBRANCH
                MOVE WK-NSF-TEXT2  TO D-REASON
           ELSE
              MOVE TP-BRNO     TO LP-BRNO
              MOVE TP-ACCTNO   TO LP-ACCTNO
              MOVE TP-REVSEQNO TO LP-SEQNO
              PERFORM OPEN-LP1-FILE
              PERFORM READ-LP1-FILE
              PERFORM CLOSE-LP1-FILE
              IF IO-FG = 0
                 MOVE LP-PAYDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO WK-NSF-DATE
                 MOVE WK-NSF-TEXT TO D-REASON.

      ********************************************
      *    NOTE: THESE UPDATE BY CHECK TYPE
      *          MUST MATCH LPEOD5.C
      ********************************************
       UPDATE-NSF-REVERSAL SECTION.
      ********************************************
           IF TP-OTHBR-FG = " " OR "C"
              MOVE GI-NSF TO GLNO
              MOVE TP-TRAMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              IF TP-OTHBR-FG = "C"
                 MOVE GI-OTHBR-PAY TO GLNO
                 COMPUTE GLAMT = 0 - TP-TRAMT
                 PERFORM UPDATE-GL-SUMMARY
              END-IF
            END-IF.


           IF TP-OTHBR-FG = " " OR "G"
              MOVE GI-LATECHG TO GLNO
              COMPUTE GLAMT = 0 - TP-APLC
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-INTEREST TO GLNO
              COMPUTE GLAMT = 0 - TP-APINT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-AR TO GLNO
              COMPUTE GLAMT = 0 - TP-APCUR
              PERFORM UPDATE-GL-SUMMARY
              IF TP-OTHBR-FG = "G"
                 MOVE GI-OTHBR-REC TO GLNO
                 MOVE TP-TRAMT TO GLAMT
                 PERFORM UPDATE-GL-SUMMARY
              END-IF
           END-IF.

      *********************************************
       UPDATE-DEPOSIT SECTION.
      *********************************************
      * TD-DEPOSIT = ACTUAL ENDING - OPENING
           MOVE TD-BANK TO GLNO.
           MOVE TD-DEPOSIT TO GLAMT.
           PERFORM UPDATE-GL-SUMMARY.
      * IF SCANNED CKS, HANDLE CHECK DEPOSIT
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE BR-CHECK-DEPOSIT-GL TO GLNO
              MOVE TD-CHECK-DEPOSIT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY.
      * TD-OVERSHORT = EXPECTED ENDING - ACTUAL ENDING (SHORT=DEBIT)
           MOVE TD-GLOVERSHORT TO GLNO.
           MOVE TD-OVERSHORT TO GLAMT.
           PERFORM UPDATE-GL-SUMMARY.
      * CASH MUST GET NET TRANSACTIONS
           MOVE TD-CASH TO GLNO.
           COMPUTE GLAMT = 0 - TD-DEPOSIT - TD-OVERSHORT.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              SUBTRACT TD-CHECK-DEPOSIT FROM GLAMT.
           PERFORM UPDATE-GL-SUMMARY.

      ********************************************
      *    UPDATE CHECK TYPES
      *       ONLY TRC TYPES "CA" AND "RR" AND "RC"
      *                                "RP" AND "RD" AND "BK"
      *
      *    NOTE: THESE UPDATE BY CHECK TYPE
      *          MUST MATCH LPEOD5.C
      ********************************************
       UPDATE-CHECK-TYPE SECTION.

           IF TC-TYPE = "CA"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

      * DEBIT CARD (UMC) #1450
           IF TC-TYPE = "DB"
              PERFORM GET-REFCD-GL
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "BK"
              MOVE GI-BANK TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RR"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLRVPRIN(TC-DLRVNO) TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RC"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLRVCHGS(TC-DLRVNO) TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RP"
              MOVE GI-CASH TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLPARV TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "RD"
              MOVE GI-BANK TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLPAYABLE TO GLNO
              COMPUTE GLAMT = 0 - TC-AMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.


           MOVE GI-BANK TO GLNO.
           COMPUTE GLAMT = 0 - TC-AMT.
           PERFORM UPDATE-GL-SUMMARY.
       UPDATE-CHECK-TYPE-END.
           EXIT.

      ***********************************
       UPDATE-GL-SUMMARY SECTION.
      ***********************************
       UPDATE-GL-SUMMARY-REC.

           MOVE WK2-BRANCH TO WK-BR.
           MOVE GLNO       TO WK-GLNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO UPDATE-GL-SUMMARY-CREATE.

           IF GLAMT > 0
              ADD GLAMT TO WK-DRGLAMT
           ELSE
              ADD GLAMT TO WK-CRGLAMT.

           PERFORM REWRITE-WK-FILE.
           GO TO UPDATE-GL-SUMMARY-EXIT.

       UPDATE-GL-SUMMARY-CREATE.
           MOVE WK2-BRANCH TO WK-BR.
           MOVE GLNO       TO WK-GLNO.
           MOVE 0          TO WK-DRGLAMT WK-CRGLAMT.
           PERFORM WRITE-WK-FILE.

           GO TO UPDATE-GL-SUMMARY-REC.

       UPDATE-GL-SUMMARY-EXIT.

      ****************************
       BRANCH-TOTALS SECTION.
      ****************************

           MOVE "TOTAL BANK     :" TO DETAIL-LINE.
           MULTIPLY -1 BY BANK-TOTAL GIVING T-CKTOTAL.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NSF CHECKS     :" TO DETAIL-LINE.
           MULTIPLY -1 BY NSF-TOTAL GIVING T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL DEPOSITS :" TO DETAIL-LINE.
           MOVE DEPOSIT-TOTAL TO T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF CK-DEPOSIT-TOTAL > 0
              MOVE "TOTAL CHECK DEP:" TO DETAIL-LINE
              MOVE CK-DEPOSIT-TOTAL TO T-CKTOTAL
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL DEALER   :" TO DETAIL-LINE.
           MULTIPLY -1 BY DEALER-TOTAL GIVING T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECEIPTS :" TO DETAIL-LINE.
           MULTIPLY -1 BY RECEIPT-TOTAL GIVING T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL DISBURSE :" TO DETAIL-LINE.
           MOVE DISBURSE-TOTAL TO T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL DEBIT RECEIPTS :" TO DETAIL-LINE.
           MULTIPLY -1 BY DEBIT-RECEIPT-TOTAL GIVING T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL DEBIT DISBURSE :" TO DETAIL-LINE.
           MOVE DEBIT-DISBURSE-TOTAL TO T-CKTOTAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO GL-SUM-FG.
           MOVE 99 TO LN-COUNT.
           PERFORM PRINT-BR-GL-SUMMARY.
           MOVE 0 TO RECEIPT-TOTAL DISBURSE-TOTAL DEALER-TOTAL
                     BANK-TOTAL DEPOSIT-TOTAL NSF-TOTAL
                     CK-DEPOSIT-TOTAL.
           MOVE 99 TO LN-COUNT.
           MOVE SPACES TO GL-SUM-FG.
       END-BRANCH-TOTALS.
           EXIT.

      ***********************************
       PRINT-BR-GL-SUMMARY SECTION.
      ***********************************
           MOVE "Y" TO GL-SUM-FG.
           MOVE 0 TO CK-TOTDR CK-TOTCR.
           MOVE HOLD-BRNO TO WK-BR.
           MOVE 0 TO WK-GLNO.
           PERFORM  START-WK-FILE.
           IF IO-FG NOT = 0
              GO TO PRINT-BR-GL-SUM-END.
       PRINT-BR-GL-SUM-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PRINT-BR-GL-SUM-LINE.

           IF WK-BR NOT = HOLD-BRNO
              GO TO PRINT-BR-GL-SUM-LINE.
           MOVE WK-GLNO TO WORK-GLNO.
           MOVE WORK-GLBR TO S-GLBR.
           MOVE "-" TO S-DASH.
           MOVE WORK-GLNUM TO S-GLNUM.

           MOVE WK-GLNO TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-FG NOT = 0
              MOVE "****NOT ON FILE***" TO S-GLNAME
           ELSE
              MOVE GL-DESCRIPTION TO S-GLNAME.

           IF WK-DRGLAMT NOT = 0
              MOVE WK-DRGLAMT TO S-DRGLAMT
              ADD WK-DRGLAMT TO CK-TOTDR.

           IF WK-CRGLAMT NOT = 0
              MOVE WK-CRGLAMT TO S-CRGLAMT
              ADD WK-CRGLAMT TO CK-TOTCR.

           COMPUTE S-NET = WK-DRGLAMT + WK-CRGLAMT.

           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-BR-GL-SUM-NEXT.
       PRINT-BR-GL-SUM-LINE.
           MOVE SPACES TO DETAIL-LINE.
           MOVE "TOTAL:" TO S-GLNAME.
           MOVE CK-TOTDR TO S-DRGLAMT.
           MOVE CK-TOTCR TO S-CRGLAMT.
           COMPUTE S-NET = CK-TOTDR + CK-TOTCR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-BR-GL-SUM-END.
           MOVE 99 TO LN-COUNT.
           MOVE SPACES TO GL-SUM-FG.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO HD-BRNO.
           MOVE BR-NAME   TO HD-BRNAME.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.
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
           MOVE BR-GLBANK TO WK-GLNO.
      *--->MOVE HOLD-BRNO TO HD-BRNO.
      *--->MOVE BR-NAME TO HD-BRNAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GL-SUM-FG NOT = "Y"
              MOVE HEAD4 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GL-SUM-FG = "Y"
              MOVE 4 TO LN-FEED
              MOVE HEAD5 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 9 TO LN-COUNT
           ELSE
              MOVE 5 TO LN-COUNT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.

      ******************************************************************
       WRITE-ERROR-LINE SECTION.
      ******************************************************************
           MOVE WS-ERROR-GI-MESSAGE TO DETAIL-LINE.
           MOVE 3                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY: "             TO D-ERR-PROMPT1.
           MOVE WK2-DATE            TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-DD    TO D-ERR-DAY.
           MOVE "BRANCH: "          TO D-ERR-PROMPT2.
           MOVE WK2-BRANCH          TO D-ERR-BRNO.
           MOVE "CODE: "            TO D-ERR-PROMPT3.
           MOVE WK2-CODE            TO D-ERR-CODE.
           MOVE "CLASS: "           TO D-ERR-PROMPT4.
           MOVE WK2-CLASS           TO D-ERR-CLASS.
           MOVE "SUBKEY: "          TO D-ERR-PROMPT5.
           IF WK2-CODE = "C"
              MOVE WK2-CKNO         TO D-ERR-SUBKEY
           ELSE
           IF WK2-CODE = "P"
              MOVE WK2-ACCTNO       TO D-ERR-SUBKEY.
           MOVE 2                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-WRITE-ERROR-LINE.
           EXIT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 1 TO LN-COUNT.
      * DONT SKIP TO A WHOLE NEW PAGE
           MOVE 4 TO LN-FEED.
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
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

      *****************************
       GET-REFCD-GL SECTION.
      *****************************
           MOVE "BR"           TO CD-TYPE.
           MOVE TC-DEBIT-REFCD TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-GOOD AND (CD-BR-FILE-TYPE = "C")
              MOVE CD-BR-GL-ACCOUNTS TO GLNO
           ELSE
              MOVE 9999999           TO GLNO.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO EMCARG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMCARG-SYSDATE.
           DISPLAY EMCARG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMCARG-FORM-FIELDS TO WR-BUF.
           MOVE EMCARG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE EMCARG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMCARG-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMCARG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************

       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       TEST-BK-REASON.
           IF TC-REASON = SPACES
              IF TC-AMT < 0
                 MOVE "DEPOSIT" TO D-REASON
              ELSE
                 MOVE "WITHDRAWAL" TO D-REASON.
           IF TC-AMT < 0
              MOVE "INT.DEPO" TO D-TYPE
              MOVE "INTERIM" TO D-REASON
           ELSE
              IF TC-TYPE = "IW"
                 MOVE "INT.WITH" TO D-TYPE
                 MOVE "INTERIM" TO D-REASON
              ELSE
                 MOVE "BNK WTHD" TO D-TYPE
                 MOVE "WITHDRAWAL" TO D-REASON
              END-IF
              IF TC-REASON NOT = SPACES
                 MOVE TC-REASON TO D-REASON.
      *-----------------------------------------------------------------
       TEST-RE-OR-SR.
           MOVE SPACES TO D-PAYEE.
           MOVE TC-PAYEE TO D-DLPAYEE.
           MOVE TC-CLASS TO D-DLCLASS.
           MOVE TC-DLNO TO D-DLNO.
           MOVE TC-TILLNO TO D-TILL.
           IF TC-TYPE = "RR"
              STRING "RESERVE FROM PRINCIPAL ",TC-DLRVNO DELIMITED
                BY "  " INTO D-REASON
              STRING "RSVPRN ",TC-DLRVNO DELIMITED BY "  " INTO D-TYPE
           ELSE
           IF TC-TYPE = "RC"
              STRING "RESERVE FROM CHARGES ",TC-DLRVNO DELIMITED
                BY "  " INTO D-REASON
              STRING "RSVCHG ",TC-DLRVNO DELIMITED BY "  " INTO D-TYPE
           ELSE
           IF TC-TYPE = "RP"
              MOVE "RSVPAR "TO D-TYPE
              MOVE "PARTICIPATING RESERVE" TO D-REASON
           ELSE
           IF TC-TYPE = "RD"
              MOVE "RECPRO "TO D-TYPE
              MOVE "RECEIPT PROCEEDS" TO D-REASON.
           IF TC-REASON NOT = SPACES
              MOVE TC-REASON TO D-REASON.
      *-----------------------------------------------------------------
       SETUP-TC-GL.
           MOVE TC-GLNO(SUBX) TO GLNO.
           MOVE TC-GLAMT(SUBX) TO GLAMT.
      *-----------------------------------------------------------------
       SETUP-TP-GL.
      * THE ONLY "P" RECORDS ARE "NSF" IN THE REFNO
           MOVE TP-GLNO(SUBX) TO GLNO.
           MOVE TP-GLAMT(SUBX) TO GLAMT.
           MULTIPLY -1 BY GLAMT.
      *-----------------------------------------------------------------
       GET-GI.
           IF ( HOLD-GI1-KEY NOT = GI1-KEY )
              PERFORM READ-GI1-FILE
              IF ( IO-FG NOT = 0 )
                 MOVE "Y" TO WS-ERROR-GI-FILE
              ELSE
                 MOVE GI1-KEY TO HOLD-GI1-KEY.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".

      * COPY "LIBLP/LPTRC1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
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
       START-TRC1-FILE.  *> EMBEDDED  TRC
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
            SELECT TRCFILE.TC_BRNO,
                   CAST(TRCFILE.TC_KEY_DATE AS VARCHAR(10)),
                   TRCFILE.TC_CKNO,
                   TRCFILE.TC_CLASS,
                   TRCFILE.TC_SEQ,
                   TRCFILE.TC_TYPE, 
                   CAST(TRCFILE.TC_DATE AS VARCHAR(10)),
                   TRCFILE.TC_AMT,
                   TRCFILE.TC_PAYEE, 
                   TRCFILE.TC_REF,
                   TRCFILE.TC_REASON, 
                   TRCFILE.TC_GLNO_1,
                   TRCFILE.TC_GLNO_2,
                   TRCFILE.TC_GLNO_3,
                   TRCFILE.TC_GLNO_4,
                   TRCFILE.TC_GLNO_5,
                   TRCFILE.TC_GLAMT_1,
                   TRCFILE.TC_GLAMT_2,
                   TRCFILE.TC_GLAMT_3,
                   TRCFILE.TC_GLAMT_4,
                   TRCFILE.TC_GLAMT_5,
                   TRCFILE.TC_DLRVNO,
                   TRCFILE.TC_DLNO,
                   TRCFILE.TC_TILLNO,
                   TRCFILE.TC_DEBIT_REFCD
            FROM DBO.TRCFILE

            WHERE TRCFILE.TC_BRNO  = :QTRC1-WBEG-BRNO
              AND TRCFILE.TC_KEY_DATE BETWEEN :QTRC1-WBEG-KEY-DATE 
                     AND :QTRC1-WEND-KEY-DATE
              AND TRCFILE.TC_TYPE IN 
                  ('CA','RR','RC','RP','RD','BK','ID','IW','DB')

            ORDER BY TRCFILE.TC_BRNO,
                     TRCFILE.TC_KEY_DATE,
                     TRCFILE.TC_CKNO,
                     TRCFILE.TC_CLASS,
                     TRCFILE.TC_SEQ
           END-EXEC.

           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRC1-WBEG-BRNO.

           MOVE SPACES TO QTRC1-WBEG-KEY-DATE
                          QTRC1-WEND-KEY-DATE.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT.   *> EMBEDDED  TRC
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           IF ( TRC1-CURSOR-STAT-GOOD )
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
                   :QTC-DLRVNO,
                   :QTC-DLNO,
                   :QTC-TILLNO,
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

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
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
       START-TRD1-FILE.   *> EMBEDDED  TRD
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           IF ( TRD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD1-FILE.
 
           EXEC SQL
            DECLARE TRD1_CURSOR CURSOR FOR
            SELECT TRDFILE.TD_BRNO,
                   CAST(TRDFILE.TD_DATE AS VARCHAR(10)), 
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT
            FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO = :QTRD1-WBEG-BRNO
              AND TRDFILE.TD_DATE BETWEEN :QTRD1-WBEG-DATE 
                       AND :QTRD1-WEND-DATE
            ORDER BY TRDFILE.TD_BRNO,
                     TRDFILE.TD_DATE
           END-EXEC.

           EXEC SQL
               OPEN TRD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD1-WBEG-BRNO.
 
           MOVE SPACES TO QTRD1-WBEG-DATE
                          QTRD1-WEND-DATE.

           MOVE STAT---GOOD TO TRD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRD1-FILE-NEXT.  *> EMBEDDED  TRD
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRD1_CURSOR INTO
                   :QTD-BRNO,
                   :QTD-DATE,
                   :QTD-DEPOSIT,
                   :QTD-CHECK-DEPOSIT,
                   :QTD-BANK,
                   :QTD-CASH,
                   :QTD-GLOVERSHORT,
                   :QTD-OVERSHORT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRD1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRD1-CURSOR-STAT.
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
       START-TRP1-FILE.  *> EMBEDDED  TRP
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
            SELECT TRPFILE.TP_BRNO,
                   CAST(TRPFILE.TP_DATE AS VARCHAR(10)),
                   TRPFILE.TP_CLASS,
                   TRPFILE.TP_ACCTNO,
                   TRPFILE.TP_LINE,
                   TRPFILE.TP_SSNO,
                   TRPFILE.TP_REFNO,
                   CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),
                   TRPFILE.TP_TRAMT,
                   TRPFILE.TP_APINT,
                   TRPFILE.TP_APLC,
                   TRPFILE.TP_APCUR,
                   TRPFILE.TP_GLNO_1,
                   TRPFILE.TP_GLNO_2,
                   TRPFILE.TP_GLNO_3,
                   TRPFILE.TP_GLAMT_1,
                   TRPFILE.TP_GLAMT_2,
                   TRPFILE.TP_GLAMT_3,
                   TRPFILE.TP_POSTING_BR,
                   TRPFILE.TP_OTHBR_FG,
                   TRPFILE.TP_REV,
                   TRPFILE.TP_REVSEQNO
            FROM DBO.TRPFILE

            WHERE TRPFILE.TP_BRNO = :QTRP1-WBEG-BRNO  
              AND TRPFILE.TP_DATE BETWEEN :QTRP1-WBEG-DATE 
                       AND :QTRP1-WEND-DATE
              AND TRPFILE.TP_REFNO = 'NSF  '

            ORDER BY TRPFILE.TP_BRNO,
                     TRPFILE.TP_DATE,
                     TRPFILE.TP_CLASS,
                     TRPFILE.TP_ACCTNO,
                     TRPFILE.TP_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.  *> EMBEDDED  TRP
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRP1_CURSOR INTO 
                   :QTP-BRNO,
                   :QTP-DATE,
                   :QTP-CLASS,
                   :QTP-ACCTNO,
                   :QTP-LINE,
                   :QTP-SSNO,
                   :QTP-REFNO,
                   :QTP-PAYDATE,
                   :QTP-TRAMT,
                   :QTP-APINT,
                   :QTP-APLC,
                   :QTP-APCUR,
                   :QTP-GLNO-1,
                   :QTP-GLNO-2,
                   :QTP-GLNO-3,
                   :QTP-GLAMT-1,
                   :QTP-GLAMT-2,
                   :QTP-GLAMT-3,
                   :QTP-POSTING-BR,
                   :QTP-OTHBR-FG,
                   :QTP-REV,
                   :QTP-REVSEQNO
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.
      *-----------------------------------------------------------------

       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************
       END-PROGRAM SECTION.
      *******************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMCARG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
