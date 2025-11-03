       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TXMOR2.
       DATE-WRITTEN.    120602.
      *****************************************************************
      *   MONTHLY INSURANCE TAX REPORT
      *       PREMIUM & REFUND REPORT
      *            REPORTS LIFE, AH&H, PROP, O1 AND O2
      *               WORLD
      *     #9. FROM EMICMU
      *
      *    SITUATIONS REQUIRING REPORTING:
      *
      *    NEW PREMIUMS:
      *        1. NEW LOANS IN THE TRFILE FOR (MMYY)
      *                             TR1-CODE = "L" OR "S"
      *                             TRL-FEECODES = "TC" OR "TA" OR "TP"
      *                                         OR "T1" OR "T2" OR "T3"
      *
      *    REBATES OR CANCELLATIONS:
      *        1. REBATES IN THE TRFILE FOR (MMYY) IF TR1-CODE = "P"
      *                                           AND TP-INS-TAX-RB = 0
      *
      *    NOTES:
      *    RIGHT NOW, THE TAX ON INSURANCE ONLY WORKS FOR DIRECT LOANS,
      *    NOT SALES FINANCE, SO I STARRED OFF THE STUFF ABOUT TS.
      *    ALSO, JOHN FIXED ADDONS TO NOT ALLOW ADDONS IF THE TAX ON
      *    INSURANCE FEATURE IS ACTIVE.
      *    WE CURRENTLY DON'T ALLOW THE CANCELLATION TRANSACTIONS
      *    (CC, CA, ETC) BUT I LEFT IN THE LOGIC TO CHECK FOR THEM.
      *    JIM SAID FOR NOW I DIDN'T HAVE TO MESS WITH TOTALS BY
      *    INSURANCE COMPANY SINCE WORLD IS ALL INSURANCE COMPANY 1;
      *    STARRED OFF ALL REFERENCES TO INS CO. FOR NOW.....
      *    IF SOMEBODY NEEDS IT, WE'LL NEED LOGIC TO CHECK ALL 6
      *    BUCKETS FOR INS CO. & PUT BACK THE TOTALS FOR INS.CO.
      *    I AM ACCUMULATING STATE TOTALS BUT NOT PRINTING THEM.
      *    NOTE, THIS REPORT WAS STARTED FROM PPMORP.
      *                                12/17/02  BARB
      *
      *
      * REV:
      *  090695 BAH WAS NOT GETTING IC FILE FROM EACH FILE SET
      *  100695 JFF ADDED CLASS SCAN
      *  100995 BLL GR-FILE NOT IN DECLARATIVES
      *  110295 MJD CHANGED READ OF LN TO GO TO SORT-TR-NEXT INSTEAD
      *             OF IO-ERROR IF FILE NOT FOUND.
      *  121096 MJD CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.
      *  021297 BLV FIX PRINTING INS CO'S OUT OF RANGE
      *  060497 BLV CHANGE TO WORK WITH NEW BATCH DRIVER (BREXEC)
      *  GLF 090397 REMOVED ALL USE OF GLOBAL
      *  BLV 042998 WAS DISPLAYING BEG-IC & END-IC FOR BEG & ENDING CLASS
      *  GLF 082499 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BAH 991103 WK-QTY WAS NOT SIGNED, IF THE FIRST ACCOUNT WAS A VOID,
      *             THE WK-QTY WAS 1 INSTEAD OF -1, CAUSING BAD TOTALS
      *             KEVIN, REGACC
      *  BAH 000621 ADDED THE "AND" TO THIS "IF" IN KEY-DETAIL SECTION
      *             IF LN-ACCTNO = WORK-ACCTNO AND (LN-OWNBR = WORK-OWNBR)
      *             REGENCY, KY0413 LAST ACCT # 500037, AND KY0414 FIRST
      *             ACCT # 500037 AND THE KEY-DETAIL WAS ADDING THESE 2
      *             ACCTS INFO TOGETHER, PL# 287
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  010810 BAH ADDED TEST IN THE READ-TR LOOP TO ONLY INCLUDE OWNING
      *             BRANCH RECORDS, ANOTHER BRANCH PAYOFF WITH THE SAME
      *             ACCTNO WAS BEING INCLUDED ON BOTH BRANCHES REPORT ! AJ
      *             OWNING BRANCHES DID MATCH, IT WAS THE WRONG ACCOUNT !!
      *             ONLY FIELD TO TEST IS TP-OTHBR-FG "G" = OWNER "C" = POSTER
      *  BAH 011113 ADDED TP-INSCOMP FOR ADDON/REVERSE/ADDON WITH DIFFERENT
      *             INSURANCE COMPANIES, REGACC PR# 1617
      *  BAH 020501 BUG FROM 010810 CHANGE, TESTED TP-OTHBR ON THE "L" AND "S"
      *             INSTEAD OF JUST "P" RECORDS, ACTUALLY CAUGHT A "C"
      *             IN THE TL-EFFDATE(3) FIELD FOR 04/30/02 STORED AS
      *             00 43 00 2F !! REGWAN PL# 365
      *  BAH 020722 TEST TP-INSOURS INSTEAD OF THE LN-INSOURS(3), ON A
      *             REVERSAL OF AN ADDON, THE LOAN RECORD INSURANCE IS
      *             BLANK (KEVIN)!!
      *  BLV 030328 ADD NET TOTALS, WORLD PR# 322
      *  BLF 041011 ADD INSURANCE COMPANY RANGE, WORLD
      *  BAH 041013 CHANGED O3 TO BE SURCHARGE FOR PROPERTY, WORLD, JIMR
      *  BAH 041210 PROP SURCHARGE WAS NOT TOTALING IF SUMMARY ONLY
      *  BAH 100401 CHANGED SURCHARGE FROM 15 TO 18% FOR WORLD PR# 673
      *   CS 170619 ACTIVATE 8 DIGIT DATES
      * BAH 180119 REMOVED LPBAGE, NOT USED, PL#986
      * BAH 181211 GLOBAL LNFILE, L2G
      * BAH 20190718 ENTER BRANCH WAS RESETTING FIELD SO YOU COULDNT SEE
      *              WHAT WAS ALREADY ENTERED FOR A BATCH, QA0055
      * BAH 20190922 REMOVED ACCESS-CALL LNFILE, TRP, TRL
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * RMZ 20230725 PERFORMANCE TUNING - ADDED STORED PROCEDURE #S35Q-70
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * VALID LOANS WORK FILE
       COPY "LIBGB/GBFSWKI.CPY".
      * TOTALS FILE
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      *************************************
      *   SUMMARY TOTALS WORK RECORD      *
      *************************************
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK1-REC.
           03  WK1-KEY.
      *    WK-CODE: 1-BR, 2-ST, 3-GRAND
               05  WK-CODE   PIC 9.
               05  WK-ST.
                   07  WK-BR PIC 9(4).
           03  WK-TAX-TABLE       OCCURS 6.
               05  WK-TAX-AMT     PIC S9(5)V99 COMP-3.
               05  WK-TAX-REBATE  PIC S9(5)V99 COMP-3.

      *************************************
      *   WORK FILE FOR VALID LOANS
      *************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-OWNBR       PIC 9(4).
               05  WORK-CLASS       PIC 9(3).
               05  WORK-ACCTNO      PIC 9(8).
               05  WORK-DATE        PIC 9(8).
               05  WORK-COUNTY-CD   PIC X(3).
               05  WORK-CITY-CD     PIC X(3).
           03  WORK-TAX-TABLE       OCCURS 6.
               05  WORK-TAX-AMT     PIC S9(5)V99 COMP-3.
               05  WORK-TAX-REBATE  PIC S9(5)V99 COMP-3.
           03  WORK-TR-CODE         PIC X.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/TXMOR2.CBL S34 08/10/21-21:03:43 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-BEG-IC           PIC 9(2)     VALUE 0.
       01  QWS-END-IC           PIC 9(2)     VALUE 0.
       01  QWS-BEG-ST           PIC X(2)     VALUE SPACES.
       01  QWS-END-ST           PIC X(2)     VALUE SPACES.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  TRP1-CONN-OPEN-SW    PIC X        VALUE "N".
       01  TRL1-CONN-OPEN-SW    PIC X        VALUE "N".

       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK1-PATH             PIC X(35).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BR-PAGE-NUMBER       PIC 9(4) COMP-3 VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(2)     VALUE 1.
       01  FEE-SUB              PIC 9(2)     VALUE 1.
       01  SURCHARGE-REBATE     PIC S9(5)V99   COMP-3 VALUE 0.
       01  TAX-RATE             PIC S9(5)V9(5) COMP-3 VALUE 0.
       01  SURCHARGE-PERCENT    PIC S9(5)V9(5) COMP-3 VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  NAME-WK              PIC X(34).
       01  ADD-WK               PIC X(16).
       01  HOLD-IC              PIC 99       COMP-3 VALUE 0.
       01  HOLD-BR              PIC 9(4)     COMP-3 VALUE 0.
       01  HOLD-LN-BR           PIC 9(4)     COMP-3 VALUE 9999.
       01  SAVE-WK1-KEY         PIC X(5).
       01  SAVE-CODE            PIC 9.
       01  SAVE-ST.
           03  SAVE-BR          PIC 9(4).
       01  FIRST-TOT            PIC X.
       01  DASH-LINE            PIC X(132)   VALUE ALL "-".

       01  REC-EXISTS           PIC X        VALUE "N".
       01  TOTAL-FG             PIC X        VALUE "N".
       01  COMPANY-FG           PIC X        VALUE "N".
       01  INSURANCE-TAX-FG     PIC X        VALUE "N".

       01  RPT-END-DATE         PIC 9(8)     COMP-3 VALUE 0.
       01  BEGINNING-DATE       PIC 9(8)     COMP-3 VALUE 0.
       01  CLASS-FG             PIC X(3)      VALUE "   ".
           88  CLASS-BEG                      VALUE "BEG".
           88  CLASS-END                      VALUE "END".
       01  NET-AMT              PIC S9(5)V99 COMP-3 VALUE 0.
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-IC          PIC 99.
           03  END-IC          PIC 99.
           03  BEG-LC          PIC 9(3).
           03  END-LC          PIC 9(3).
           03  BEG-ST          PIC XX.
           03  END-ST          PIC XX.
           03  MMYY            PIC 9(4).
           03  MM-YY REDEFINES MMYY.
               05  MM          PIC 9(2).
               05  YY          PIC 9(2).
           03  SUMMARY-ONLY    PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999   VALUE 9999.
       01  SV-END-BR           PIC 9999   VALUE 9999.
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
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(29)    VALUE " ".
           03  FILLER           PIC X(8)     VALUE "COMPANY ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10)    VALUE " ".
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(21)    VALUE
           "INSURANCE TAX REPORT ".
           03  FILLER           PIC X(13)    VALUE "FOR MONTH OF ".
           03  H-MMYY           PIC 99/99.

       01  HEAD3                VALUE " ".
           03  H-PAGE-DESC      PIC X(46).
           03  H-STNO.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X.
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(45).
           03  H-PAGE-PROMPT    PIC X(12).
           03  H-BR-PAGE        PIC ZZZZ.

       01  HEAD4.
           03  FILLER           PIC X(48) VALUE
           "ACCOUNT #      BORROWER        LN/PO DT  CNT CIT".
           03  FILLER           PIC X(35) VALUE
           "  ------ TAXES ON PREMIUMS ------- ".
           03  FILLER           PIC X(9) VALUE "   SUR   ".
           03  FILLER           PIC X(32) VALUE
           "--------- TAX REBATES ----------".
           03  FILLER           PIC X(7) VALUE "    SUR".

       01  HEAD5.
           03  FILLER           PIC X(48) VALUE SPACES.
           03  FILLER           PIC X(42) VALUE
           "  LIFE    A&H   PROP     O1     O2   PROP ".
           03  FILLER           PIC X(42) VALUE
           "  LIFE    A&H   PROP     O1     O2   PROP ".

       01  HEAD6.
           03  FILLER           PIC X(12) VALUE SPACES.
           03  FILLER           PIC X(25) VALUE
           "  ----------- TOTAL TAXES".
           03  FILLER           PIC X(25) VALUE
           " ON PREMIUMS ----------- ".
           03  FILLER           PIC X(10) VALUE "      SUR ".
           03  FILLER           PIC X(25) VALUE
           "  ------------- TOTAL TAX".
           03  FILLER           PIC X(25) VALUE
           " REBATES --------------  ".
           03  FILLER           PIC X(9) VALUE "      SUR".

       01  HEAD7.
           03  FILLER           PIC X(12) VALUE SPACES.
           03  FILLER           PIC X(30) VALUE
           "     LIFE       A&H      PROP ".
           03  FILLER           PIC X(30) VALUE
           "       O1        O2      PROP ".
           03  FILLER           PIC X(30) VALUE
           "     LIFE       A&H      PROP ".
           03  FILLER           PIC X(30) VALUE
           "       O1        O2      PROP ".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC X(14).
           03  FILLER           PIC X.
           03  D-NAME           PIC X(16).
           03  FILLER           PIC X.
           03  D-LOANDATE       PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-COUNTY-CD      PIC XXX.
           03  FILLER           PIC X.
           03  D-CITY-CD        PIC XXX.
           03  D-TAX-AMT        PIC Z(3).99- BLANK ZERO OCCURS 6.
           03  D-REFUND-AMT     PIC Z(3).99- BLANK ZERO OCCURS 6.

       01  TOTAL1 REDEFINES DETAIL-LINE.
           03  D-TOT-PROMPT     PIC X(12).
           03  D-TOT-AMT        PIC Z(6).99- BLANK ZERO OCCURS 6.
           03  D-TOT-REBATE     PIC Z(6).99- BLANK ZERO OCCURS 6.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).

       COPY "LIBWI/PFSWINDOWSW.CPY".
      * COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/TXMOR2_DEF.CPY".
       COPY "LIBLP/TXMOR2_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/TXMOR2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRL1-CONNECTION.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ************************************************
       MAIN-MODULE SECTION.
      ************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TXMOR2" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      D    STOP MESS.
       
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO PROCESS-REPORT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.

           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-REPORT.

       
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "INSURANCE REPORT IN PROGRESS........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-TRP-FILE.
           PERFORM SORT-TRL-FILE.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           GO TO NEXT-GROUP-LOC.

       PROCESS-REPORT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM RUN-REPORT.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              MOVE "Y" TO COMPANY-FG
              PERFORM BR-TOTALS
              PERFORM GRAND-TOTALS
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE TXMOR2-QUEUE-POS TO Q-POS.
           MOVE TXMOR2-COPIES-POS TO C-POS.
           MOVE TXMOR2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "CREATING WORK FILE.................." TO MESS
              PERFORM  SEND-LEGEND.

       
      * CREATE TOTALS WORK FILE
       
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

       
      * CREATE WORK FILE FOR VALID LOANS
       
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "INSURANCE REPORT IN PROGRESS......." TO MESS
              PERFORM SEND-LEGEND.

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

      * RPT-END-DATE IS ALREADY SET, JUST NEED A BEGINNING FOR TR

           MOVE MM TO DATE-MMDDYY-MM.
           MOVE YY TO DATE-MMDDYY-YY.
           MOVE 01 TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           PERFORM OPEN-WK1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM ARRAYSP-LOAD.
      *     PERFORM ENABLE-INTERRUPT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MMYY TO H-MMYY.
           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE RPT-END-DATE.
           MOVE NDTE-MM TO MM.
           MOVE NDTE-YY TO YY.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U" GO TO SO-01.
       
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
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
              GO TO CHECK-BRANCH-SECURITY.

       
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
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
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

       
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       
       IC-01.

           MOVE "ENNN020IC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-01.
           MOVE VDUNUM0 TO BEG-IC.
       
       IC-02.
           MOVE "ENNN020IC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO IC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-02.
           MOVE VDUNUM0 TO END-IC.
           IF BEG-IC NOT > END-IC GO TO LC-01.
           PERFORM INVALID-RANGE.
           GO TO IC-01.
       
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 1 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.
       
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO IC-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-02.
           MOVE VDUNUM0 TO BEG-LC.
       
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-02.
           MOVE VDUNUM0 TO END-LC.
           IF BEG-LC NOT > END-LC GO TO ST-01.
           PERFORM INVALID-RANGE.
           GO TO LC-01.
       
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-LC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-LC VDUNUM0.
           PERFORM SCREENIO.
       
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.
           MOVE VDUBUF TO BEG-ST.
       
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-02.
           MOVE VDUBUF TO END-ST.
           IF BEG-ST NOT > END-ST GO TO DT-01.
           PERFORM INVALID-RANGE.
           GO TO ST-01.
       
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  " TO BEG-ST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ALL "Z" TO END-ST.
           CALL "C$TOLOWER" USING END-ST, VALUE 2.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
       
       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SO-01.
           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ST-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDUNUM0 TO MMYY.
           IF MM < 1 OR MM > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.
           MOVE MM TO DATE-MMDDYY-MM.
           MOVE YY TO DATE-MMDDYY-YY.
           MOVE 33 TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO RPT-END-DATE.
       
       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N") GO TO SO-01.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
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
       
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-LC TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-LC TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      *************************************************
       RUN-REPORT SECTION.
      *************************************************
       GET-NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE LN-OWNBR TO HOLD-BR
              PERFORM BR-HEADER
              PERFORM GET-LN-BW-PATH.

           IF WORK-OWNBR NOT = HOLD-BR
              PERFORM BR-TOTALS
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH
              MOVE 99 TO LN-COUNT.

           MOVE WORK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE. *> EMBEDDED - LNFILE
           IF IO-FG NOT = 0
              GO TO END-RUN-REPORT.

      *******************************************
      * NOTE:
      *   THIS CODE MUST STAY BEFORE THE SUMMARY ONLY CHECK
      *    OR THE PROPERTY SURCHARGE WILL NOT TOTAL
      *******************************************

      * PROPERTY TAX AND SURCHARGE
           IF WORK-TAX-AMT(3) NOT = 0
      * SURCHARGE
              COMPUTE WORK-TAX-AMT(6) ROUNDED = LN-INSPREM(3) * .018
              IF WORK-TAX-AMT(3) < 0
                 COMPUTE WORK-TAX-AMT(6) = WORK-TAX-AMT(6) * -1
              END-IF
              MOVE WORK-TAX-AMT(6) TO D-TAX-AMT(6)
      * TAX LESS THE SURCHARGE
              COMPUTE WORK-TAX-AMT(3) ROUNDED = WORK-TAX-AMT(3) -
                                        WORK-TAX-AMT(6)
              MOVE WORK-TAX-AMT(3) TO D-TAX-AMT(3).

      * PROPERTY REFUND TAX AND SURCHARGE
           IF WORK-TAX-REBATE(3) NOT = 0
              PERFORM COMPUTE-SURCHARGE
      * SURCHARGE
              MOVE SURCHARGE-REBATE TO D-REFUND-AMT(6)
                                       WORK-TAX-REBATE(6)
      * TAX REFUND LESS THE SURCHARGE
              COMPUTE WORK-TAX-REBATE(3) ROUNDED = WORK-TAX-REBATE(3) -
                                       SURCHARGE-REBATE
              MOVE WORK-TAX-REBATE(3) TO D-REFUND-AMT(3).

           IF SUMMARY-ONLY = "Y"
              MOVE SPACES TO DETAIL-LINE
              GO TO SKIP-PRINT-DETAIL.

      * SEUP PRIMARY INSURED INFO:
           IF LN-INSURED = "S"
              MOVE LN-SSNO(2) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM SETUP-INSURED.

           IF LN-INSURED = "S"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE. *> EMBEDDED - BW

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 STRING "MISSING SPR: ",LN-OWNBR,LN-CLASS,LN-ACCTNO
                               DELIMITED BY "  " INTO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           IF (LN-POFFDATE NOT = 0 AND WORK-TR-CODE NOT = "L")
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY TO D-LOANDATE
           ELSE
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY TO D-LOANDATE.

           MOVE WORK-COUNTY-CD TO D-COUNTY-CD.
           MOVE WORK-CITY-CD TO D-CITY-CD.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE WORK-TAX-AMT(SUB)    TO D-TAX-AMT(SUB)
              MOVE WORK-TAX-REBATE(SUB) TO D-REFUND-AMT(SUB)
           END-PERFORM.


           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       SKIP-PRINT-DETAIL.
           PERFORM CREATE-TOTAL-RECS.

           GO TO GET-NEXT-WK.
       END-RUN-REPORT.
           EXIT.

      **********************************************
       SORT-TRP-FILE SECTION.
      **********************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE RPT-END-DATE        TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-LC              TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-LC              TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.
           MOVE TP-DATE              TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.
           MOVE BEG-IC               TO QWS-BEG-IC.
           MOVE END-IC               TO QWS-END-IC.
           MOVE BEG-ST               TO QWS-BEG-ST.
           MOVE END-ST               TO QWS-END-ST.

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

           PERFORM START-TRP1-FILE. *> SSP - TRP,LN

       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT. *> EMBEDDED - TRP,LN
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-TRP-EXIT.

      *     PERFORM NPR-TRAP-INTERRUPT.

           IF TP-DATE > RPT-END-DATE
              GO TO SORT-TRP-EXIT.

           IF TP-CLASS < BEG-LC OR TP-CLASS > END-LC
              GO TO SORT-TRP-NEXT.

           IF TP-INS-TAX-RB = 0
              GO TO SORT-TRP-NEXT
           ELSE
              IF TP-INSCOMP < BEG-IC OR TP-INSCOMP > END-IC
                 GO TO SORT-TRP-NEXT.

      *     MOVE TP-BRNO   TO LN-OWNBR.
      *     MOVE TP-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG  NOT = 0
      *        GO TO SORT-TRP-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRP-NEXT.

      *    IF LN-OWNBR NOT = TR1-BRNO    <<< THIS DOES NOT WORK ANYMORE !!
      * IF ANOTHER BRANCH DOES A PAYOFF, AND THE SAME LOAN # EXIST
      * IN BOTH BRANCHES, (NOT REALLY THE SAME LOAN!), IT WILL PRINT FOR
      * BOTH BRANCHES, SHOULD ONLY PRINT FOR THE OWNING BRANCH !!

      * ONLY REPORT FOR THE OWNING BRANCH ("G")
           IF TP-OTHBR-FG = "C"
              GO TO SORT-TRP-NEXT.

           MOVE TP-DATE              TO WORK-DATE.
           MOVE TP-BRNO              TO WORK-OWNBR.
           MOVE TP-CLASS             TO WORK-CLASS.
           MOVE TP-ACCTNO            TO WORK-ACCTNO.
           MOVE TP-INS-TAX-COUNTY-CD TO WORK-COUNTY-CD.
           MOVE TP-INS-TAX-CITY-CD   TO WORK-CITY-CD.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES    TO WK-REC
              MOVE TP-DATE   TO WORK-DATE
              MOVE TP-BRNO   TO WORK-OWNBR
              MOVE TP-CLASS  TO WORK-CLASS
              MOVE TP-ACCTNO TO WORK-ACCTNO
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                 MOVE 0 TO WORK-TAX-AMT(SUB) WORK-TAX-REBATE(SUB)
              END-PERFORM.

           MOVE TP-INS-TAX-COUNTY-CD TO WORK-COUNTY-CD.
           MOVE TP-INS-TAX-CITY-CD   TO WORK-CITY-CD.

           IF (TP-TRCD = "RC" OR "CC")
              COMPUTE WORK-TAX-REBATE(1) =
                      WORK-TAX-REBATE(1) + TP-INS-TAX-RB
           ELSE
           IF (TP-TRCD = "RA" OR "CA")
              COMPUTE WORK-TAX-REBATE(2) =
                      WORK-TAX-REBATE(2) + TP-INS-TAX-RB
           ELSE
           IF (TP-TRCD = "RP" OR "CP")
              COMPUTE WORK-TAX-REBATE(3) =
                      WORK-TAX-REBATE(3) + TP-INS-TAX-RB
           ELSE
           IF (TP-TRCD = "R1" OR "C1")
              COMPUTE WORK-TAX-REBATE(4) =
                      WORK-TAX-REBATE(4) + TP-INS-TAX-RB
           ELSE
           IF (TP-TRCD = "R2" OR "C2")
              COMPUTE WORK-TAX-REBATE(5) =
                      WORK-TAX-REBATE(5) + TP-INS-TAX-RB
           ELSE
           IF (TP-TRCD = "R3" OR "C3")
              COMPUTE WORK-TAX-REBATE(5) =
                      WORK-TAX-REBATE(5) + TP-INS-TAX-RB.

           IF WORK-TR-CODE = " "
      *       MOVE TP-CODE TO WORK-TR-CODE.
              MOVE "P" TO WORK-TR-CODE.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRP-NEXT.

       SORT-TRP-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      **********************************************
       SORT-TRL-FILE SECTION.
      **********************************************

           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE RPT-END-DATE        TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE BEG-LC              TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-LC              TO QTRL1-WEND-CLASS.
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE. *> SSP - TRL,LN

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT. *> EMBEDDED - TRL,LN
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

      *     PERFORM NPR-TRAP-INTERRUPT.

           IF TL-DATE > RPT-END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-LC OR TL-CLASS > END-LC
              GO TO SORT-TRL-NEXT.

           PERFORM CHECK-TL-FOR-INSURANCE-TAX.
           IF INSURANCE-TAX-FG = "N"
              GO TO SORT-TRL-NEXT.

      *     MOVE TL-BRNO   TO LN-OWNBR.
      *     MOVE TL-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG  NOT = 0
      *        GO TO SORT-TRL-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRL-NEXT.

           MOVE TL-DATE              TO WORK-DATE.
           MOVE TL-BRNO              TO WORK-OWNBR.
           MOVE TL-CLASS             TO WORK-CLASS.
           MOVE TL-ACCTNO            TO WORK-ACCTNO.
           MOVE TL-INS-TAX-COUNTY-CD TO WORK-COUNTY-CD.
           MOVE TL-INS-TAX-CITY-CD   TO WORK-CITY-CD.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES    TO WK-REC
              MOVE TL-DATE   TO WORK-DATE
              MOVE TL-BRNO   TO WORK-OWNBR
              MOVE TL-CLASS  TO WORK-CLASS
              MOVE TL-ACCTNO TO WORK-ACCTNO
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                 MOVE 0 TO WORK-TAX-AMT(SUB) WORK-TAX-REBATE(SUB)
              END-PERFORM.

           MOVE TL-INS-TAX-COUNTY-CD TO WORK-COUNTY-CD
           MOVE TL-INS-TAX-CITY-CD   TO WORK-CITY-CD
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              COMPUTE FEE-SUB = SUB + 4
              IF TL-FEECODE-INS-TAX(FEE-SUB)
                 IF TL-INSCOMP(SUB) >= BEG-IC
                    IF TL-INSCOMP(SUB) <= END-IC
                       COMPUTE WORK-TAX-AMT(SUB) =
                               WORK-TAX-AMT(SUB) + TL-FEEAMT(FEE-SUB)
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

           IF WORK-TR-CODE = " "
              MOVE "L" TO WORK-TR-CODE.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      ****************************************
       CREATE-TOTAL-RECS SECTION.
      ****************************************

      * WK-CODE: 1-BR, 2-ST, 3-GB
           MOVE SPACES TO WK1-KEY.
           MOVE 1 TO WK-CODE.
           MOVE LN-OWNBR TO WK-BR.
           PERFORM CREATE-WK.

           MOVE SPACES TO WK1-KEY.
           MOVE 2 TO WK-CODE.
           MOVE LN-ORGST TO WK-ST.
           PERFORM CREATE-WK.

           MOVE SPACES TO WK1-KEY.
           MOVE 2 TO WK-CODE.
           MOVE 3 TO WK-CODE.
           PERFORM CREATE-WK.

       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      ****************************************
       CREATE-WK SECTION.
      ****************************************
           MOVE WK1-KEY TO SAVE-WK1-KEY.
           MOVE "N" TO REC-EXISTS.
       CHECK-REC-EXISTS.
           PERFORM READ-WK1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-WK1-REC.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.
           MOVE " " TO WK1-REC.
           MOVE SAVE-WK1-KEY TO WK1-KEY.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE 0 TO WK-TAX-AMT(SUB) WK-TAX-REBATE(SUB)
           END-PERFORM.
           PERFORM WRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.
       UPDATE-WK1-REC.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              COMPUTE WK-TAX-AMT(SUB) =
                      WK-TAX-AMT(SUB) + WORK-TAX-AMT(SUB)
              COMPUTE WK-TAX-REBATE(SUB) =
                      WK-TAX-REBATE(SUB) + WORK-TAX-REBATE(SUB)
           END-PERFORM.
           PERFORM REWRITE-WK1-FILE.

      ****************************************
       COMPUTE-SURCHARGE SECTION.
      ****************************************

      * 1. "TP" FEE AMOUNT DIVIDED BY PP INSURANCE AMOUNT EQUALS %
      * 2. DIVIDE 1.5% BY THE % COMPUTED IN STEP 1 WHICH WILL GIVE YOU WHAT
      *    PERCENT OF THE ORIGINAL TOTAL TAX WAS TAX AND WHAT WAS SURCHARGE
      * 3. MULTIPLY THE % FROM STEP 2 TIMES THE INSURANCE TAX REBATE REFUND
      *    AMOUNT IN THE "RP" TRAN RECORD - THIS WILL BE THE SURCHARGE REBATE
      * 4. SUBTRACT THE SURCHARGE REBATE FROM THE INSURANCE TAX REBATE REFUND
      *    AMOUNT AND THIS WILL BE THE TAX REBATE AMOUNT

      * MUST FIRST FIND THE ORIGINAL TAX AMOUNT, "TP"
           MOVE 1 TO SUB.
           MOVE 0 TO SURCHARGE-REBATE TAX-RATE SURCHARGE-PERCENT.

       FIND-TAX-FEECODE.
           IF SUB > 10
              GO TO EXIT-COMPUTE-SURCHARGE.

           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO FIND-TAX-FEECODE.

           IF LN-FEECODE(SUB) = "TP"
              COMPUTE TAX-RATE ROUNDED =
                 ( LN-FEEAMT(SUB) / LN-INSPREM(3) )
              COMPUTE SURCHARGE-PERCENT ROUNDED =
                  ( ( 1.5 / TAX-RATE ) / 100  )
              COMPUTE SURCHARGE-REBATE ROUNDED =
                 ( SURCHARGE-PERCENT * WORK-TAX-REBATE(3) )
              GO TO EXIT-COMPUTE-SURCHARGE.

           ADD 1 TO SUB.
           GO TO FIND-TAX-FEECODE.

       EXIT-COMPUTE-SURCHARGE.
           EXIT.

      ****************************************
       BR-TOTALS SECTION.
      ****************************************
           MOVE "B R A N C H   T O T A L S:" TO H-PAGE-DESC.
           MOVE 1 TO SAVE-CODE.
           MOVE HOLD-BR TO SAVE-BR.
           PERFORM PRINT-TOTALS.

           MOVE WORK-OWNBR TO HOLD-BR.
           MOVE " " TO H-PAGE-DESC.
           PERFORM BR-HEADER.
           MOVE 0 TO BR-PAGE-NUMBER.

      ****************************************
       GRAND-TOTALS SECTION.
      ****************************************
           MOVE "C O M P A N Y   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 3 TO SAVE-CODE.
           MOVE 0 TO HOLD-IC.
           MOVE " " TO SAVE-ST.
           PERFORM PRINT-TOTALS.

      ****************************************
       PRINT-TOTALS SECTION.
      ****************************************

           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.
           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE SAVE-ST TO WK-ST.
           PERFORM START-WK1-FILE.
       NEXT-TOTAL-LINE.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-PRINT-TOTALS.
           IF WK-CODE NOT = SAVE-CODE
              GO TO EXIT-PRINT-TOTALS.
           PERFORM MOVE-TOTALS.
           GO TO NEXT-TOTAL-LINE.
       EXIT-PRINT-TOTALS.
           MOVE "N" TO TOTAL-FG.

      ******************************************
       SETUP-INSURED SECTION.
      ******************************************
           PERFORM READ-BW1-FILE. *> EMBEDDED - BW
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES TO BW-LNAME.

      * SET NAME OF PRIMARY INSURED:
           MOVE BW-FNAME TO NAME-WK.
           MOVE BW-LNAME TO ADD-WK.
           INSPECT NAME-WK REPLACING FIRST "  " BY " +".
           INSPECT NAME-WK REPLACING FIRST "+               " BY ADD-WK.
           MOVE NAME-WK TO D-NAME.


       SETUP-INSURED-EXIT.
           EXIT.

      ******************************************
       CHECK-TL-FOR-INSURANCE-TAX SECTION.
      ******************************************

           MOVE "N" TO INSURANCE-TAX-FG.
           MOVE 1 TO SUB.
       CHECK-TL-LOOP.
           COMPUTE FEE-SUB = SUB + 4.
           IF TL-FEECODE-INS-TAX(FEE-SUB)
              IF TL-INSCOMP(SUB) >= BEG-IC
                 IF TL-INSCOMP(SUB) <= END-IC
                    MOVE "Y" TO INSURANCE-TAX-FG
                    GO TO EXIT-CHECK-TL.

           IF SUB < 6
              ADD 1 TO SUB
              GO TO CHECK-TL-LOOP.

       EXIT-CHECK-TL.
           EXIT.

      ******************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       HEAD-PAGE SECTION.
      ****************************
           MOVE "N" TO FIRST-TIME.
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

           IF H-PAGE-DESC = " "
              ADD 1 TO BR-PAGE-NUMBER
              MOVE BR-PAGE-NUMBER TO H-BR-PAGE
              MOVE "BRANCH PAGE:" TO H-PAGE-PROMPT
           ELSE
              MOVE 0 TO H-BR-PAGE
              MOVE " " TO H-PAGE-PROMPT.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF TOTAL-FG = "N"
              MOVE HEAD4 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD5 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD6 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF TOTAL-FG = "N"
              NEXT SENTENCE
           ELSE
              MOVE HEAD7 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.

           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       RANGE-LINES SECTION.
      *******************************
           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG INS CO" TO RANGE-LINE.
           MOVE BEG-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END INS CO" TO RANGE-LINE.
           MOVE END-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG ORIG STATE" TO RANGE-LINE.
           MOVE BEG-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END ORIG STATE" TO RANGE-LINE.
           MOVE END-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-LC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTH AND YEAR" TO RANGE-LINE.
           MOVE MMYY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *******************************
       MOVE-TOTALS SECTION.
      *******************************
           IF SAVE-CODE = 1
              MOVE "BRANCH TOTAL" TO D-TOT-PROMPT
           ELSE IF SAVE-CODE = 3
              MOVE "GRAND TOTALS" TO D-TOT-PROMPT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE WK-TAX-AMT(SUB)  TO D-TOT-AMT(SUB)
              MOVE WK-TAX-REBATE(SUB)  TO D-TOT-REBATE(SUB)
           END-PERFORM.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "  NET TOTALS" TO D-TOT-PROMPT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              COMPUTE NET-AMT = WK-TAX-AMT(SUB) - WK-TAX-REBATE(SUB)
              MOVE NET-AMT  TO D-TOT-AMT(SUB)
           END-PERFORM.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************************************************
       CLASS-SCAN SECTION.
      ****************************************************************
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
              MOVE "SNNN020LC1." TO VDU
           ELSE
              MOVE "SNNN020LC2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO TXMOR2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TXMOR2-SYSDATE.
           DISPLAY TXMOR2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TXMOR2-FORM-FIELDS TO WR-BUF.
           MOVE TXMOR2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE TXMOR2-HELP--FILE TO HELP-LINK-FILE.
           MOVE TXMOR2-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/TXMOR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       
       BR-HEADER.
           MOVE HOLD-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.
           MOVE HOLD-BR TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
       
       GET-LN-BW-PATH.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

       
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       

      ******************************************************
       IO-ROUTINES SECTION.
      ******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
      * REV:
      * JKC 2024-0404 ADDED LN-LOS-APP-ID
      ******************************************************************
       LOAD-LN1-FILE.
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

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LN1-FILE.  *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1, 
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2,
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4, 
                   LNFILE.LN_ORGST, 
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_STATFLAGS, 
                   CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)),
                   CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
                   CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
                   CAST(LNFILE.LN_POFFDATE       AS VARCHAR(10)),
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_FEECODE_1, 
                   LNFILE.LN_FEECODE_2, 
                   LNFILE.LN_FEECODE_3, 
                   LNFILE.LN_FEECODE_4, 
                   LNFILE.LN_FEECODE_5, 
                   LNFILE.LN_FEECODE_6, 
                   LNFILE.LN_FEECODE_7, 
                   LNFILE.LN_FEECODE_8, 
                   LNFILE.LN_FEECODE_9, 
                   LNFILE.LN_FEECODE_10, 
                   LNFILE.LN_FEECODE_11, 
                   LNFILE.LN_FEECODE_12, 
                   LNFILE.LN_FEECODE_13, 
                   LNFILE.LN_FEECODE_14, 
                   LNFILE.LN_FEECODE_15, 
                   LNFILE.LN_FEECODE_16, 
                   LNFILE.LN_FEECODE_17, 
                   LNFILE.LN_FEECODE_18, 
                   LNFILE.LN_FEECODE_19, 
                   LNFILE.LN_FEECODE_20, 
                   LNFILE.LN_FEEAMT_1,
                   LNFILE.LN_FEEAMT_2,
                   LNFILE.LN_FEEAMT_3,
                   LNFILE.LN_FEEAMT_4,
                   LNFILE.LN_FEEAMT_5,
                   LNFILE.LN_FEEAMT_6,
                   LNFILE.LN_FEEAMT_7,
                   LNFILE.LN_FEEAMT_8,
                   LNFILE.LN_FEEAMT_9,
                   LNFILE.LN_FEEAMT_10,
                   LNFILE.LN_FEEAMT_11,
                   LNFILE.LN_FEEAMT_12,
                   LNFILE.LN_FEEAMT_13,
                   LNFILE.LN_FEEAMT_14,
                   LNFILE.LN_FEEAMT_15,
                   LNFILE.LN_FEEAMT_16,
                   LNFILE.LN_FEEAMT_17,
                   LNFILE.LN_FEEAMT_18,
                   LNFILE.LN_FEEAMT_19,
                   LNFILE.LN_FEEAMT_20,
                   LNFILE.LN_INSURED, 
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_UNITPER_CD, 
                   LNFILE.LN_UNITPER_FREQ
              INTO 
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
                   :QLN-STATFLAGS, 
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-POFFDATE,
                   :QLN-ORGTERM,
                   :QLN-FEECODE-1, 
                   :QLN-FEECODE-2, 
                   :QLN-FEECODE-3, 
                   :QLN-FEECODE-4, 
                   :QLN-FEECODE-5, 
                   :QLN-FEECODE-6, 
                   :QLN-FEECODE-7, 
                   :QLN-FEECODE-8, 
                   :QLN-FEECODE-9, 
                   :QLN-FEECODE-10, 
                   :QLN-FEECODE-11, 
                   :QLN-FEECODE-12, 
                   :QLN-FEECODE-13, 
                   :QLN-FEECODE-14, 
                   :QLN-FEECODE-15, 
                   :QLN-FEECODE-16, 
                   :QLN-FEECODE-17, 
                   :QLN-FEECODE-18, 
                   :QLN-FEECODE-19, 
                   :QLN-FEECODE-20, 
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QLN-INSURED, 
                   :QLN-INSPREM-1,
                   :QLN-INSPREM-2,
                   :QLN-INSPREM-3,
                   :QLN-INSPREM-4,
                   :QLN-INSPREM-5,
                   :QLN-INSPREM-6,
                   :QLN-INSPREM-7,
                   :QLN-INSPREM-8,
                   :QLN-INSPREM-9,
                   :QLN-INSPREM-10,
                   :QLN-TOTNODEF,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD, 
                   :QLN-UNITPER-FREQ
              FROM DBO.LNFILE
             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

      *     PERFORM CLOSE-IT.
      *     IF ( LN1-CURSOR-STAT-GOOD )
      *        EXEC SQL
      *             CLOSE LN1_CURSOR
      *        END-EXEC
      *        MOVE STAT---BAD TO LN1-CURSOR-STAT.

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

           IF TRP1-CONN-OPEN-SW = "N"
              MOVE "Y" TO TRP1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE. *> SSP - TRP,LN
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE 99991231         TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE
              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE TP-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE

              IF ( TP-CLASS = 999 )
                 MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1
              END-IF

              IF ( TP-ACCTNO = 99999999 )
                 MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1
              END-IF

              IF ( TP-LINE = 999 )
                 MOVE TP-LINE           TO QTRP1-WSBEG-LINE
              ELSE
                 COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

      * STORED PROCEDURE SELECT STATEMENT: 
      *
      *  SELECT TRPFILE.TP_BRNO,
      *         CAST(TRPFILE.TP_DATE AS VARCHAR(10)), 
      *         TRPFILE.TP_CLASS,
      *         TRPFILE.TP_ACCTNO,
      *         TRPFILE.TP_LINE,
      *         TRPFILE.TP_TRCD, 
      *         TRPFILE.TP_OTHBR_FG, 
      *         TRPFILE.TP_INSCOMP,
      *         TRPFILE.TP_INS_TAX_RB,
      *         TRPFILE.TP_INS_TAX_COUNTY_CD, 
      *         TRPFILE.TP_INS_TAX_CITY_CD, 
      *         LNFILE.LN_OWNBR,
      *         LNFILE.LN_ACCTNO,
      *         LNFILE.LN_CLASS,
      *         LNFILE.LN_SUBCLASS,
      *         LNFILE.LN_LAWCODE,
      *         LNFILE.LN_SSNO1,
      *         LNFILE.LN_MAKERCD1, 
      *         LNFILE.LN_SSNO2,
      *         LNFILE.LN_MAKERCD2,
      *         LNFILE.LN_SSNO3,
      *         LNFILE.LN_MAKERCD3, 
      *         LNFILE.LN_SSNO4,
      *         LNFILE.LN_MAKERCD4, 
      *         LNFILE.LN_ORGST, 
      *         LNFILE.LN_SPRCLASS,
      *         LNFILE.LN_STATFLAGS, 
      *         CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)),
      *         CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *         CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
      *         CAST(LNFILE.LN_POFFDATE       AS VARCHAR(10)),
      *         LNFILE.LN_ORGTERM,
      *         LNFILE.LN_FEECODE_1, 
      *         LNFILE.LN_FEECODE_2, 
      *         LNFILE.LN_FEECODE_3, 
      *         LNFILE.LN_FEECODE_4, 
      *         LNFILE.LN_FEECODE_5, 
      *         LNFILE.LN_FEECODE_6, 
      *         LNFILE.LN_FEECODE_7, 
      *         LNFILE.LN_FEECODE_8, 
      *         LNFILE.LN_FEECODE_9, 
      *         LNFILE.LN_FEECODE_10, 
      *         LNFILE.LN_FEECODE_11, 
      *         LNFILE.LN_FEECODE_12, 
      *         LNFILE.LN_FEECODE_13, 
      *         LNFILE.LN_FEECODE_14, 
      *         LNFILE.LN_FEECODE_15, 
      *         LNFILE.LN_FEECODE_16, 
      *         LNFILE.LN_FEECODE_17, 
      *         LNFILE.LN_FEECODE_18, 
      *         LNFILE.LN_FEECODE_19, 
      *         LNFILE.LN_FEECODE_20, 
      *         LNFILE.LN_FEEAMT_1,
      *         LNFILE.LN_FEEAMT_2,
      *         LNFILE.LN_FEEAMT_3,
      *         LNFILE.LN_FEEAMT_4,
      *         LNFILE.LN_FEEAMT_5,
      *         LNFILE.LN_FEEAMT_6,
      *         LNFILE.LN_FEEAMT_7,
      *         LNFILE.LN_FEEAMT_8,
      *         LNFILE.LN_FEEAMT_9,
      *         LNFILE.LN_FEEAMT_10,
      *         LNFILE.LN_FEEAMT_11,
      *         LNFILE.LN_FEEAMT_12,
      *         LNFILE.LN_FEEAMT_13,
      *         LNFILE.LN_FEEAMT_14,
      *         LNFILE.LN_FEEAMT_15,
      *         LNFILE.LN_FEEAMT_16,
      *         LNFILE.LN_FEEAMT_17,
      *         LNFILE.LN_FEEAMT_18,
      *         LNFILE.LN_FEEAMT_19,
      *         LNFILE.LN_FEEAMT_20,
      *         LNFILE.LN_INSURED, 
      *         LNFILE.LN_INSPREM_1,
      *         LNFILE.LN_INSPREM_2,
      *         LNFILE.LN_INSPREM_3,
      *         LNFILE.LN_INSPREM_4,
      *         LNFILE.LN_INSPREM_5,
      *         LNFILE.LN_INSPREM_6,
      *         LNFILE.LN_INSPREM_7,
      *         LNFILE.LN_INSPREM_8,
      *         LNFILE.LN_INSPREM_9,
      *         LNFILE.LN_INSPREM_10,
      *         LNFILE.LN_TOTNODEF,
      *         LNFILE.LN_SUM_ORGTERM,
      *         LNFILE.LN_UNITPER_CD, 
      *         LNFILE.LN_UNITPER_FREQ
      *
      *    FROM DBO.TRPFILE TRPFILE
      *      INNER JOIN
      *         DBO.LNFILE  LNFILE
      *         ON ( LNFILE.LN_OWNBR           = TRPFILE.TP_BRNO
      *        AND   LNFILE.LN_ACCTNO          = TRPFILE.TP_ACCTNO 
      *        AND   LNFILE.LN_ORGST     BETWEEN @QWS_BEG_ST 
      *                                      AND @QWS_END_ST)
      *      WHERE ( TRPFILE.TP_BRNO           = @QTRP1_WBEG_BRNO   
      *        AND   TRPFILE.TP_DATE     BETWEEN @QTRP1_WBEG_DATE
      *                                      AND @QTRP1_WEND_DATE  
      *        AND   TRPFILE.TP_CLASS    BETWEEN @QTRP1_WBEG_CLASS
      *                                      AND @QTRP1_WEND_CLASS 
      *        AND   TRPFILE.TP_INSCOMP  BETWEEN @QWS_BEG_IC
      *                                      AND @QWS_END_IC
      *        AND   TRPFILE.TP_INS_TAX_RB    != 0 
      *        AND   TRPFILE.TP_OTHBR_FG      != 'C')
      *
      *   ORDER BY TRPFILE.TP_BRNO,
      *            TRPFILE.TP_DATE,
      *            TRPFILE.TP_CLASS,
      *            TRPFILE.TP_ACCTNO,
      *            TRPFILE.TP_LINE;

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = EXEC SSP_TXMOR2_TRPFILE_SELECT
              ( :QTRP1-WBEG-BRNO,   
                :QTRP1-WBEG-DATE,   
                :QTRP1-WEND-DATE,   
                :QTRP1-WBEG-CLASS,  
                :QTRP1-WEND-CLASS,
                :QWS-BEG-IC,
                :QWS-END-IC,
                :QWS-BEG-ST,
                :QWS-END-ST ) 
           END-EXEC.
  
           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE
                          QTRP1-WSBEG-CLASS
                          QTRP1-WSBEG-ACCTNO
                          QTRP1-WSBEG-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT. *> EMBEDDED - TRP,LN
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRP1_CURSOR INTO 
                     :QTP-BRNO,
                     :QTP-DATE,  
                     :QTP-CLASS,
                     :QTP-ACCTNO,
                     :QTP-LINE,
                     :QTP-TRCD, 
                     :QTP-OTHBR-FG, 
                     :QTP-INSCOMP,
                     :QTP-INS-TAX-RB,
                     :QTP-INS-TAX-COUNTY-CD, 
                     :QTP-INS-TAX-CITY-CD, 
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
                     :QLN-STATFLAGS, 
                     :QLN-LOANDATE,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-POFFDATE,
                     :QLN-ORGTERM,
                     :QLN-FEECODE-1, 
                     :QLN-FEECODE-2, 
                     :QLN-FEECODE-3, 
                     :QLN-FEECODE-4, 
                     :QLN-FEECODE-5, 
                     :QLN-FEECODE-6, 
                     :QLN-FEECODE-7, 
                     :QLN-FEECODE-8, 
                     :QLN-FEECODE-9, 
                     :QLN-FEECODE-10, 
                     :QLN-FEECODE-11, 
                     :QLN-FEECODE-12, 
                     :QLN-FEECODE-13, 
                     :QLN-FEECODE-14, 
                     :QLN-FEECODE-15, 
                     :QLN-FEECODE-16, 
                     :QLN-FEECODE-17, 
                     :QLN-FEECODE-18, 
                     :QLN-FEECODE-19, 
                     :QLN-FEECODE-20, 
                     :QLN-FEEAMT-1,
                     :QLN-FEEAMT-2,
                     :QLN-FEEAMT-3,
                     :QLN-FEEAMT-4,
                     :QLN-FEEAMT-5,
                     :QLN-FEEAMT-6,
                     :QLN-FEEAMT-7,
                     :QLN-FEEAMT-8,
                     :QLN-FEEAMT-9,
                     :QLN-FEEAMT-10,
                     :QLN-FEEAMT-11,
                     :QLN-FEEAMT-12,
                     :QLN-FEEAMT-13,
                     :QLN-FEEAMT-14,
                     :QLN-FEEAMT-15,
                     :QLN-FEEAMT-16,
                     :QLN-FEEAMT-17,
                     :QLN-FEEAMT-18,
                     :QLN-FEEAMT-19,
                     :QLN-FEEAMT-20,
                     :QLN-INSURED, 
                     :QLN-INSPREM-1,
                     :QLN-INSPREM-2,
                     :QLN-INSPREM-3,
                     :QLN-INSPREM-4,
                     :QLN-INSPREM-5,
                     :QLN-INSPREM-6,
                     :QLN-INSPREM-7,
                     :QLN-INSPREM-8,
                     :QLN-INSPREM-9,
                     :QLN-INSPREM-10,
                     :QLN-TOTNODEF,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD, 
                     :QLN-UNITPER-FREQ
              END-EXEC   
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF (( TRP1-CURSOR-STAT-GOOD ) AND
               ( TRP1-CONN-OPEN-SW = "Y" ))
              EXEC SQL SET CONNECTION TRP1_CONNECT  END-EXEC
              EXEC SQL
                  CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT  
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-CONNECTION.

           IF TRP1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TRP1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TRP1-CONN-OPEN-SW.

      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
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

           IF TRL1-CONN-OPEN-SW = "N"
              MOVE "Y" TO TRL1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRL1-FILE. *> SSP - TRL,LN
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

           IF ( QTRL1-WEND-LINE = ZEROES    ) OR
              ( QTRL1-WEND-LINE NOT NUMERIC )
              MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
              MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
              MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
              MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
              MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
              MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
              MOVE TL-LINE              TO QTRL1-WBEG-LINE
              MOVE ALL "9"              TO QTRL1-WEND-CLASS
                                           QTRL1-WEND-ACCTNO
                                           QTRL1-WEND-LAWCODE
                                           QTRL1-WEND-LINE.

      *  STORED PROCEDURE:
      *
      *   SELECT TRLFILE.TL_BRNO,
      *          CAST(TRLFILE.TL_DATE AS VARCHAR(10)), 
      *          TRLFILE.TL_CLASS,
      *          TRLFILE.TL_ACCTNO,
      *          TRLFILE.TL_LAWCODE,
      *          TRLFILE.TL_LINE,
      *          TRLFILE.TL_FEECODE_1, 
      *          TRLFILE.TL_FEECODE_2, 
      *          TRLFILE.TL_FEECODE_3, 
      *          TRLFILE.TL_FEECODE_4, 
      *          TRLFILE.TL_FEECODE_5, 
      *          TRLFILE.TL_FEECODE_6, 
      *          TRLFILE.TL_FEECODE_7, 
      *          TRLFILE.TL_FEECODE_8, 
      *          TRLFILE.TL_FEECODE_9, 
      *          TRLFILE.TL_FEECODE_10, 
      *          TRLFILE.TL_FEECODE_11, 
      *          TRLFILE.TL_FEECODE_12, 
      *          TRLFILE.TL_FEECODE_13, 
      *          TRLFILE.TL_FEECODE_14, 
      *          TRLFILE.TL_FEECODE_15, 
      *          TRLFILE.TL_FEECODE_16, 
      *          TRLFILE.TL_FEECODE_17, 
      *          TRLFILE.TL_FEECODE_18, 
      *          TRLFILE.TL_FEECODE_19, 
      *          TRLFILE.TL_FEECODE_20, 
      *          TRLFILE.TL_FEEAMT_1,
      *          TRLFILE.TL_FEEAMT_2,
      *          TRLFILE.TL_FEEAMT_3,
      *          TRLFILE.TL_FEEAMT_4,
      *          TRLFILE.TL_FEEAMT_5,
      *          TRLFILE.TL_FEEAMT_6,
      *          TRLFILE.TL_FEEAMT_7,
      *          TRLFILE.TL_FEEAMT_8,
      *          TRLFILE.TL_FEEAMT_9,
      *          TRLFILE.TL_FEEAMT_10,
      *          TRLFILE.TL_FEEAMT_11,
      *          TRLFILE.TL_FEEAMT_12,
      *          TRLFILE.TL_FEEAMT_13,
      *          TRLFILE.TL_FEEAMT_14,
      *          TRLFILE.TL_FEEAMT_15,
      *          TRLFILE.TL_FEEAMT_16,
      *          TRLFILE.TL_FEEAMT_17,
      *          TRLFILE.TL_FEEAMT_18,
      *          TRLFILE.TL_FEEAMT_19,
      *          TRLFILE.TL_FEEAMT_20,
      *          TRLFILE.TL_INSCOMP_1,
      *          TRLFILE.TL_INSCOMP_2,
      *          TRLFILE.TL_INSCOMP_3,
      *          TRLFILE.TL_INSCOMP_4,
      *          TRLFILE.TL_INSCOMP_5,
      *          TRLFILE.TL_INSCOMP_6,
      *          TRLFILE.TL_INSCOMP_7,
      *          TRLFILE.TL_INSCOMP_8,
      *          TRLFILE.TL_INSCOMP_9,
      *          TRLFILE.TL_INSCOMP_10,
      *          TRLFILE.TL_INS_TAX_COUNTY_CD, 
      *          TRLFILE.TL_INS_TAX_CITY_CD,  
      *          LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SUBCLASS,
      *          LNFILE.LN_LAWCODE,
      *          LNFILE.LN_SSNO1,
      *          LNFILE.LN_MAKERCD1, 
      *          LNFILE.LN_SSNO2,
      *          LNFILE.LN_MAKERCD2,
      *          LNFILE.LN_SSNO3,
      *          LNFILE.LN_MAKERCD3, 
      *          LNFILE.LN_SSNO4,
      *          LNFILE.LN_MAKERCD4, 
      *          LNFILE.LN_ORGST, 
      *          LNFILE.LN_SPRCLASS,
      *          LNFILE.LN_STATFLAGS, 
      *          CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)),
      *          CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *          CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
      *          CAST(LNFILE.LN_POFFDATE       AS VARCHAR(10)),
      *          LNFILE.LN_ORGTERM,
      *          LNFILE.LN_FEECODE_1, 
      *          LNFILE.LN_FEECODE_2, 
      *          LNFILE.LN_FEECODE_3, 
      *          LNFILE.LN_FEECODE_4, 
      *          LNFILE.LN_FEECODE_5, 
      *          LNFILE.LN_FEECODE_6, 
      *          LNFILE.LN_FEECODE_7, 
      *          LNFILE.LN_FEECODE_8, 
      *          LNFILE.LN_FEECODE_9, 
      *          LNFILE.LN_FEECODE_10, 
      *          LNFILE.LN_FEECODE_11, 
      *          LNFILE.LN_FEECODE_12, 
      *          LNFILE.LN_FEECODE_13, 
      *          LNFILE.LN_FEECODE_14, 
      *          LNFILE.LN_FEECODE_15, 
      *          LNFILE.LN_FEECODE_16, 
      *          LNFILE.LN_FEECODE_17, 
      *          LNFILE.LN_FEECODE_18, 
      *          LNFILE.LN_FEECODE_19, 
      *          LNFILE.LN_FEECODE_20, 
      *          LNFILE.LN_FEEAMT_1,
      *          LNFILE.LN_FEEAMT_2,
      *          LNFILE.LN_FEEAMT_3,
      *          LNFILE.LN_FEEAMT_4,
      *          LNFILE.LN_FEEAMT_5,
      *          LNFILE.LN_FEEAMT_6,
      *          LNFILE.LN_FEEAMT_7,
      *          LNFILE.LN_FEEAMT_8,
      *          LNFILE.LN_FEEAMT_9,
      *          LNFILE.LN_FEEAMT_10,
      *          LNFILE.LN_FEEAMT_11,
      *          LNFILE.LN_FEEAMT_12,
      *          LNFILE.LN_FEEAMT_13,
      *          LNFILE.LN_FEEAMT_14,
      *          LNFILE.LN_FEEAMT_15,
      *          LNFILE.LN_FEEAMT_16,
      *          LNFILE.LN_FEEAMT_17,
      *          LNFILE.LN_FEEAMT_18,
      *          LNFILE.LN_FEEAMT_19,
      *          LNFILE.LN_FEEAMT_20,
      *          LNFILE.LN_INSURED, 
      *          LNFILE.LN_INSPREM_1,
      *          LNFILE.LN_INSPREM_2,
      *          LNFILE.LN_INSPREM_3,
      *          LNFILE.LN_INSPREM_4,
      *          LNFILE.LN_INSPREM_5,
      *          LNFILE.LN_INSPREM_6,
      *          LNFILE.LN_INSPREM_7,
      *          LNFILE.LN_INSPREM_8,
      *          LNFILE.LN_INSPREM_9,
      *          LNFILE.LN_INSPREM_10,
      *          LNFILE.LN_TOTNODEF,
      *          LNFILE.LN_SUM_ORGTERM,
      *          LNFILE.LN_UNITPER_CD, 
      *          LNFILE.LN_UNITPER_FREQ
      *
      *     FROM DBO.TRLFILE TRLFILE
      *       INNER JOIN
      *          DBO.LNFILE  LNFILE
      *          ON ( LNFILE.LN_OWNBR           = TRLFILE.TL_BRNO
      *         AND   LNFILE.LN_ACCTNO          = TRLFILE.TL_ACCTNO 
      *         AND   LNFILE.LN_ORGST     BETWEEN @QWS_BEG_ST 
      *                                       AND @QWS_END_ST )
      *       WHERE ( TRLFILE.TL_BRNO           = @QTRL1_WBEG_BRNO   
      *         AND   TRLFILE.TL_DATE     BETWEEN @QTRL1_WBEG_DATE
      *                                       AND @QTRL1_WEND_DATE  
      *         AND   TRLFILE.TL_CLASS    BETWEEN @QTRL1_WBEG_CLASS
      *                                       AND @QTRL1_WEND_CLASS )
      *
      *       ORDER BY TRLFILE.TL_BRNO,
      *                TRLFILE.TL_DATE,
      *                TRLFILE.TL_CLASS,
      *                TRLFILE.TL_ACCTNO,
      *                TRLFILE.TL_LAWCODE,
      *                TRLFILE.TL_LINE;

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = EXEC SSP_TXMOR2_TRLFILE_SELECT
              ( :QTRL1-WBEG-BRNO,   
                :QTRL1-WBEG-DATE,   
                :QTRL1-WEND-DATE,   
                :QTRL1-WBEG-CLASS,  
                :QTRL1-WEND-CLASS, 
                :QWS-BEG-ST,
                :QWS-END-ST ) 
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WEND-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS
                          QTRL1-WBEG-ACCTNO
                          QTRL1-WEND-ACCTNO
                          QTRL1-WBEG-LAWCODE
                          QTRL1-WEND-LAWCODE
                          QTRL1-WBEG-LINE
                          QTRL1-WEND-LINE.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.


           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRL1_CURSOR 
                 INTO 
                  :QTL-BRNO,
                  :QTL-DATE, 
                  :QTL-CLASS,
                  :QTL-ACCTNO,
                  :QTL-LAWCODE,
                  :QTL-LINE,
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
                  :QTL-INS-TAX-COUNTY-CD, 
                  :QTL-INS-TAX-CITY-CD,  
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
                  :QLN-STATFLAGS, 
                  :QLN-LOANDATE,
                  :QLN-ORIG-1STPYDATE,
                  :QLN-1STPYDATE,
                  :QLN-POFFDATE,
                  :QLN-ORGTERM,
                  :QLN-FEECODE-1, 
                  :QLN-FEECODE-2, 
                  :QLN-FEECODE-3, 
                  :QLN-FEECODE-4, 
                  :QLN-FEECODE-5, 
                  :QLN-FEECODE-6, 
                  :QLN-FEECODE-7, 
                  :QLN-FEECODE-8, 
                  :QLN-FEECODE-9, 
                  :QLN-FEECODE-10, 
                  :QLN-FEECODE-11, 
                  :QLN-FEECODE-12, 
                  :QLN-FEECODE-13, 
                  :QLN-FEECODE-14, 
                  :QLN-FEECODE-15, 
                  :QLN-FEECODE-16, 
                  :QLN-FEECODE-17, 
                  :QLN-FEECODE-18, 
                  :QLN-FEECODE-19, 
                  :QLN-FEECODE-20, 
                  :QLN-FEEAMT-1,
                  :QLN-FEEAMT-2,
                  :QLN-FEEAMT-3,
                  :QLN-FEEAMT-4,
                  :QLN-FEEAMT-5,
                  :QLN-FEEAMT-6,
                  :QLN-FEEAMT-7,
                  :QLN-FEEAMT-8,
                  :QLN-FEEAMT-9,
                  :QLN-FEEAMT-10,
                  :QLN-FEEAMT-11,
                  :QLN-FEEAMT-12,
                  :QLN-FEEAMT-13,
                  :QLN-FEEAMT-14,
                  :QLN-FEEAMT-15,
                  :QLN-FEEAMT-16,
                  :QLN-FEEAMT-17,
                  :QLN-FEEAMT-18,
                  :QLN-FEEAMT-19,
                  :QLN-FEEAMT-20,
                  :QLN-INSURED, 
                  :QLN-INSPREM-1,
                  :QLN-INSPREM-2,
                  :QLN-INSPREM-3,
                  :QLN-INSPREM-4,
                  :QLN-INSPREM-5,
                  :QLN-INSPREM-6,
                  :QLN-INSPREM-7,
                  :QLN-INSPREM-8,
                  :QLN-INSPREM-9,
                  :QLN-INSPREM-10,
                  :QLN-TOTNODEF,
                  :QLN-SUM-ORGTERM,
                  :QLN-UNITPER-CD, 
                  :QLN-UNITPER-FREQ
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRL1_CONNECT  END-EXEC
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
             EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRL1-CONNECTION.

           IF TRL1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TRL1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TRL1-CONN-OPEN-SW.
  
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
        READ-BW1-FILE. *> EMBEDDED - BW
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME, 
                   BWFILE.BW_FNAME, 
                   BWFILE.BW_ADR1, 
                   BWFILE.BW_ADR2, 
                   BWFILE.BW_CITY, 
                   BWFILE.BW_STATE, 
                   BWFILE.BW_ZIP, 
                   CAST(BWFILE.BW_BDATE AS VARCHAR(10)) 
              INTO  
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME, 
                   :QBW-FNAME, 
                   :QBW-ADR1, 
                   :QBW-ADR2, 
                   :QBW-CITY, 
                   :QBW-STATE, 
                   :QBW-ZIP, 
                   :QBW-BDATE 
              FROM DBO.BWFILE
             WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.
 
      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

      *     PERFORM CLOSE-IT.
      *     IF ( BW1-CURSOR-STAT-GOOD )
      *        EXEC SQL
      *             CLOSE BW1_CURSOR
      *        END-EXEC
      *        MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------


       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************************************
       END-PROGRAM SECTION.
      ********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK1-PATH TO ACCESS-BUF.
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
              MOVE "LP/EMICMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TXMOR2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
