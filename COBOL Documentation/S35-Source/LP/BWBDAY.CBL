       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWBDAY.
       DATE-WRITTEN.    090694.
      ******************************************************************
      *
      *    DIR :  LP
      *    DESC:  BIRTHDAY REPORT
      *
      *              *NEW*
      *      WORLD PR#316
      *      THIS IS BASICALLY THE SAME AS ASOPEN EXCEPT THERE IS A
      *      REPORT OPTION THAT PRINTS IN BIRTHDAY ORDER.  THERE IS
      *      ALSO AN OPTION TO REPORT/GENERATE FORM REQUESTS FOR ALL
      *      QUALIFYING ACCOUNTS OR ON THE MOST CURRENT ACCOUNT (LN-LOANDATE
      *      IS THE MEASURE FOR DETERMINING NEWEST ACCOUNT.
      * REV:
      *  BAH 040419 ADDED ORIGIN FOR CASHCO, PR# 2273
      *  BAH 040819 CHECK BW-CONFIDENTIAL, PR# 2343
      *  BLV 040614 MAY ENTER A GROUP, NOW CROSSES DATA PATHS, ADDED OPTION
      *             FOR CORP PRINTING, ADDED BRSECURE.  IF RUNNING FOR
      *             GROUP OR BRANCH RANGE, WILL CHECK FOR DUPLICATE SSNOS
      *             REGARDLESS OF BRANCH IF "N" TO "MULTIPLE ACCOUNTS".
      *             PR#1030, PL# 324
      *  BLV 050914 ENDING DATE RANGE NOT TESTED... WHEN RAN FOR 0901 TO 0915
      *             ENTIRE MONTH PRINTED [WORLD]
      *  BAH 071008 WORLD NO LONGER SOLICITING BORROWERS IN KANSAS PR#557
      *  BAH 140429 CHANGED RESERVED WORD REPORT TO REPORTX, VERYANT
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0217 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *
      * BAH 170510 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171229 CORPORATE FORM PRINTING WAS NOT WORKING, NEEDED TO READ
      *            GP-CORP-BRNO AND SET CORRECT FM-PATH, ALSO DEFAULT CORP
      *            FORM FLAG TO "Y" IF EXTRACTION FORM, WORLD #1293
      * BAH 180205 TEST BRANCH RESTRICTIONS ON FORM REQUEST, FD-SECURITY
      *            AND FD-VALID-BRANCH, WORLD PR#1301
      * BAH 181114 GLOBAL FMFILE
      * BAH 181204 GLOBAL LNFILE
      * BAH 190108 GLOBAL NOFILE
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190710 BDAY-DATE NEEDS TO STAY MMDDYY SO REPORT COMES OUT IN
      *              CORRECT ORDER, BY MONTH! QA0041
      * BAH 20190924 REMOVED ACCESS-CALL LNFILE, FMFILE
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 2024.0201 REMOVED NOTICE FOR A FORM REQUEST
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT BDAY-FILE ASSIGN TO BDAY-PATH
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
           RECORD KEY BDAY-KEY
           FILE STATUS FILE-STAT.

           SELECT MANYACCTS-FILE ASSIGN TO MANYACCTS-PATH
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
           RECORD KEY MANYACCTS-KEY
           FILE STATUS FILE-STAT.


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  BDAY-FILE
           LABEL RECORDS ARE STANDARD.
       01  BDAY-REC.
           03  BDAY-KEY.
      * THIS NEEDS TO STAY MMDDYY, THATS THE ORDER THEY WANT THIS REPORT
               05  BDAY-DATE-MMDDYY PIC 9(6).
               05  BDAY-OWNBR       PIC 9(4).
               05  BDAY-SSNO        PIC 9(9).
               05  BDAY-ACCTNO      PIC 9(8).

           03  BDAY-LOANDATE      PIC 9(8).
           03  BDAY-NAME          PIC X(32).

       FD  MANYACCTS-FILE
           LABEL RECORDS ARE STANDARD.
       01  MANYACCTS-REC.
           03  MANYACCTS-KEY.
               05 MANYACCTS-SSNO        PIC 9(9).
           03  MANYACCTS-INITKEY.
               05  MANYACCTS-ACCTNO     PIC 9(8).
               05  MANYACCTS-FORMNO     PIC 9(4).
               05  FILLER               PIC X(4).
               05  MANYACCTS-BRNO2      PIC 9(4).
           03  MANYACCTS-LOANDATE       PIC 9(8) COMP-3.
           03  MANYACCTS-BDAY           PIC 9(8).
           03  MANYACCTS-OWNBR          PIC 9(4).
           03  MANYACCTS-DATA-PATH      PIC X(6).
           03  MANYACCTS-MACHINE        PIC XX.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BWBDAY.CBL S35 02/20/24-13:17:55 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".

       01  PR-IFN        PIC X(2)    VALUE "PR".
       01  BDAY-IFN      PIC X(4)    VALUE "BDAY".

       01  HOLD-BR-KEY   PIC 9(4)    VALUE 0.

       01  BDAY-PATH     PIC X(35)   VALUE SPACES.
       01  MANYACCTS-PATH PIC X(35)  VALUE SPACES.

       01  HOLD-FM1-KEY                PIC X(28)     VALUE SPACES.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED              PIC 99       VALUE 0.
       01  LN-COUNT             PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
       01  CLASS-FG             PIC X(3)     VALUE SPACES.

      **********************
      *   MISC WORKERS     *
      **********************

       01  SUB                  PIC 999.

      *01  LP-TRCD              PIC XX    VALUE SPACES.
       01  PAYS-MADE            PIC 999.
       01  PAYS-REMAIN          PIC 999.

       01  ORIGIN-WK.
           03  ORIGIN-CHAR      PIC X        OCCURS 4.
       01  ORIGIN-SUB           PIC 9.
       01  ORIGIN-MATCH         PIC X.

       01  PROFILE-OK           PIC X.
       01  PROFILE-HIGH         PIC 999.
       01  PROFILE-WORKER-01.
           03  PROFILE-WORKER   PIC 9(12).
           03  PROFILE-TAB REDEFINES PROFILE-WORKER
                                OCCURS 12 PIC 9.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  SKIP-RECORDS-FOUND            VALUE " ".

       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.

       01  EDIT-FM-ZIP.
           03  EDIT-FM-LAST     PIC X(9).
           03  EDIT-FM-FIRST    PIC XXX.

       01  HOLD-BDATE           PIC 9(8).
       01  CREATE-FM-FG         PIC X VALUE "Y".
       01  SAVE-INITKEY         PIC X(20) VALUE SPACES.


      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  LOAN-FIELDS.
               05  BEG-BDAY        PIC 9(4).
               05  FILLER REDEFINES BEG-BDAY.
                   07  BEG-MM      PIC 99.
                   07  BEG-DD      PIC 99.
               05  END-BDAY        PIC 9(4).
               05  FILLER REDEFINES END-BDAY.
                   07  END-MM      PIC 99.
                   07  END-DD      PIC 99.
               05  MINPAYS-MADE    PIC 999.
               05  MINPAYS-REMAIN  PIC 999.
               05  MINTERM         PIC 999.
               05  MAXTERM         PIC 999.
               05  MINNXTPAY       PIC 99.
               05  MAXNXTPAY       PIC 99.
               05  MINLSTPAY       PIC 99.
               05  MAXLSTPAY       PIC 99.
               05  MINAV           PIC 9(6).
               05  FORMNO          PIC 999.
               05  ZIP-CODE        PIC X.
               05  BEG-CLASS       PIC 99.
               05  END-CLASS       PIC 99.
               05  REPORTX         PIC X.
               05  MANY-ACCOUNTS   PIC X.
               05  ORIGINX.
                   07  ORIGIN-CD   PIC X OCCURS 4.
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).
           03  CORP-FM-FG          PIC X.

       01  VERSION-CONTROL         PIC X      VALUE "D".

       01  SAVE-FM-ALL-PATH.
           03  SAVE-FM-ALL-BASE    PIC X(09).
           03  FILLER              PIC X(01)   VALUE "/".
           03  SAVE-FM-ALL-MACHINE PIC XX.
           03  FILLER              PIC X       VALUE "/".
           03  SAVE-FM-ALL-DATA    PIC X(6).
           03  FILLER              PIC X(10)   VALUE "/LP/FMFILE".

       01  SAVE-NO-ALL-PATH.
           03  SAVE-NO-ALL-BASE    PIC X(09).
           03  FILLER              PIC X(01)   VALUE "/".
           03  SAVE-NO-ALL-MACHINE PIC XX.
           03  FILLER              PIC X       VALUE "/".
           03  SAVE-NO-ALL-DATA    PIC X(6).
           03  FILLER              PIC X(10)   VALUE "/LP/NOFILE".

       01  SCREEN-BUF              PIC X(53) VALUE SPACES.

       01  SCREEN-LIST.
           03  FILLER          PIC X(12) VALUE
           "BWDT1 BWDT2 ".
           03  FILLER          PIC X(44) VALUE
           "MINPAYSM MINPAYSR MINTERM MAXTERM MINNXTPAY ".
           03  FILLER          PIC X(36) VALUE
           "MAXNXTPAY MINLSTPAY MAXLSTPAY MINAV ".
           03  FILLER          PIC X(11) VALUE
           "FORMNO ZIP.".

      *-------------------------------------------------------
      *   REPORT PRINT
      *-------------------------------------------------------
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
           03  FILLER           PIC X(21)    VALUE " ".
           03  FILLER           PIC X(50)    VALUE
           "       BIRTHDAY LISTING SELECTION REPORT       ".

       01  HEAD4.
           03  FILLER          PIC X(8)  VALUE "BR #    ".
           03  FILLER          PIC X(32) VALUE "CUSTOMER NAME".
           03  FILLER          PIC X(4)  VALUE SPACES.
           03  FILLER          PIC X(6)  VALUE "ACCT #".
           03  FILLER          PIC X(4)  VALUE SPACES.
           03  FILLER          PIC X(8)  VALUE "BIRTHDAY".

       01  DETAIL-LINE         PIC X(132)   VALUE SPACES.
       01  D-LINE1           REDEFINES DETAIL-LINE.
           03  D-OWNBR         PIC 9(4).
           03  FILLER          PIC X(4).
           03  D-NAME          PIC X(32).
           03  FILLER          PIC X(4).
           03  D-NUMBER        PIC 9(6).
           03  FILLER          PIC X(4).
           03  D-BDAY          PIC 99/99/99.

       01  RANGE-LN             REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(30).
           03  RANGE-SELECTION  PIC X(102).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWBDAY_DEF.CPY".
       COPY "LIBLP/BWBDAY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BWBDAY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE MANYACCTS-FILE BDAY-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE MANYACCTS-FILE BDAY-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWBDAY" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO PROCESS-BDAY-FILE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-BDAY-FILE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    HAVE TO OVERRIDE THE PATHS FOR FM DUE TO THE PATHS
      *    BEING ALTERED ELSEWHERE IN THIS PROGRAM.  WAS EASIER TO
      *    USE THE "Y" OVERRIDE OPTION HERE.               JAY 2014-1029
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FILPATH-BASE            TO FM-ALL-BASE.
           MOVE FILPATH-MACHINE         TO FM-ALL-MACHINE.
           MOVE FILPATH-DATA            TO FM-ALL-DATA.
           MOVE FM-ALL-PATH             TO FM-PATH.
           MOVE "Y"                     TO FM-PATH-OVERRIDE.

      * DONT BOTHER IF NO FORM REQUESTD
           IF ( CORP-FM-FG) = "Y"
              IF FORMNO NOT = 0
                 PERFORM GET-FM-PATH.

           IF EXT-ROUTE-BUF = "00"
              IF NOT (PRU-LP = "VDU" OR "PRU")
                 MOVE "LOAN FILE EXTRACTION IN PROGRESS BRANCH &&&&"
                                                     TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
                 MOVE MESS TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.

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

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

      *  POCD - **  IS A DELETED CONTRACT
           IF LN-POCD = "**"
              GO TO NEXT-LN.

      * ALWAYS SKIP ZERO BALANCE:
           IF LN-CURBAL = 0
              GO TO NEXT-LN.

      * SKIP ANY RENEWED ACCTS, SOLICITATION WILL PICK UP RENEWED LOAN
           IF LN-POCD = "PB" OR "10"
              GO TO NEXT-LN.

      * SKIP PL ACCOUNTS:
           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN.

      * SKIP JUDGEMENTS:
           IF LN-JDDATE NOT = 0
              GO TO NEXT-LN.

      * SKIP CONFIDENTIAL ACCOUNTS:
           IF LN-CONFIDENTCD = "Y"
              GO TO NEXT-LN.


      * SKIP REPO, LEGAL, BANKRUPT, AND DEBTAID ACCOUNTS:
           IF LN-REPODATE NOT = 0 OR
               LN-DEBTAIDATE NOT = 0 OR
                LN-LEGALDATE NOT = 0 OR
                 LN-BNKRPTDATE NOT = 0
                    GO TO NEXT-LN.

      *---------------------------------------------------------------
      *  MAKE SURE THE BORROWER BIRTHDAY FALLS WITHIN THE ENTERED RANGE
      *  BEFORE DOING CALCULATIONS
      *-----------------------------------------------------------------
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              GO TO NEXT-LN.

      * 10/8/2007 PR# 557
           IF BW-STATE = "KS"
              GO TO NEXT-LN.

           MOVE BW-BDATE TO HOLD-BDATE.

           IF BW-SOLICIT = "N" OR BW-CONFIDENTIAL = "Y"
              GO TO NEXT-LN.

      * TEST BORROWER ORIGIN (CASHCO PR# 2273)
           IF ORIGINX NOT = "????"
              MOVE BW-ORIGIN TO ORIGIN-WK
              PERFORM ORIGIN-CD-CHECK
              IF ORIGIN-MATCH NOT = "Y"
                 GO TO NEXT-LN.

      * TEST BIRTHDAY RANGE
           MOVE BW-BDATE   TO SYS-DATE.
           MOVE BW-BDATE   TO NUM-DATE.
      * ONLY ENTER MONTH AND DAY ON SCREEN INPUT
           MOVE BEG-MM     TO NUM-MO.
           MOVE BEG-DD     TO NUM-DA.
           IF BW-BDATE < NUM-DATE
              GO TO NEXT-LN.

           MOVE END-MM     TO NUM-MO.
           MOVE END-DD     TO NUM-DA.
           IF BW-BDATE > NUM-DATE
              GO TO NEXT-LN.

      *********************************************
      * TEST LOAN OPTIONS:
      *********************************************
       CONTINUE-PROCESSING-LOAN.
      * TEST MINIMUM PAYS MADE:
           IF LN-REGPYAMT NOT = 0
              COMPUTE PAYS-MADE = LN-TOTPAYMNTD / LN-REGPYAMT.
           IF PAYS-MADE < MINPAYS-MADE
              GO TO NEXT-LN.

      * TEST MINIMUM PAYS REMAINING:
           COMPUTE PAYS-REMAIN =
               (LN-ORGTERM * LN-REGPYAMT - LN-TOTPAYMNTD)
                                               / LN-REGPYAMT.
           IF PAYS-REMAIN < MINPAYS-REMAIN
              GO TO NEXT-LN.

      * TEST MINIMUM/MAXIMUM ORIGINAL TERM:
           IF LN-ORGTERM < MINTERM
                  OR LN-ORGTERM > MAXTERM
              GO TO NEXT-LN.

      * TEST MINIMUM/MAXIMUM TO THE NEXT DUE PAYMENT:
           IF MINNXTPAY = 0
              IF MAXNXTPAY = 99
                 GO TO TEST-DAYS-SINCE-LAST-PAYMENT.

           MOVE LN-1STPYDATE   TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NDTE-DATE.
           MOVE 1              TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

           MOVE TRANS-DATE TO NUM-DATE.
           MOVE NDTE-DATE  TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < MINNXTPAY OR
               ELAPSED-DAYS > MAXNXTPAY
              GO TO NEXT-LN.

      * TEST MINIMUM/MAXIMUM DAYS SINCE LAST PAYMENT:
       TEST-DAYS-SINCE-LAST-PAYMENT.
           IF MINLSTPAY = 0
              IF MAXLSTPAY = 99
                 GO TO TEST-CONTRACUAL-PROFILE.

           IF LN-DATE-PAID-LAST = 0
              GO TO NEXT-LN.

           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           MOVE TRANS-DATE        TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < MINLSTPAY OR
               ELAPSED-DAYS > MAXLSTPAY
              GO TO NEXT-LN.

      * TEST CONTRACTUAL PROFILE:
       TEST-CONTRACUAL-PROFILE.
           MOVE LN-CONPROFILE(1) TO PROFILE-WORKER.
           MOVE 30 TO PROFILE-HIGH.
           PERFORM TEST-PROFILE.
           IF PROFILE-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST RECENCY PROFILE:
           MOVE LN-RECPROFILE(1) TO PROFILE-WORKER.
           MOVE 30 TO PROFILE-HIGH.
           PERFORM TEST-PROFILE.
           IF PROFILE-OK NOT = "Y"
              GO TO NEXT-LN.

      * TEST MIN AVAIL CREDIT:
            IF MINAV NOT = 0
               PERFORM GET-SPR
               MOVE TRANS-DATE TO AVCR-PAYDATE
               MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
               PERFORM COMPUTE-AVAILABLE-CREDIT
               IF AVCR-AVAILCREDIT < MINAV
                  GO TO NEXT-LN.

           MOVE "N" TO FIRST-TIME.

           PERFORM CREATE-FM-REC.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           GO TO NEXT-GROUP-LOC.

       PROCESS-BDAY-FILE.
           IF REPORTX NOT = "Y"
              GO TO END-ROUTINE.

           MOVE 0 TO BDAY-DATE-MMDDYY BDAY-SSNO BDAY-ACCTNO BDAY-OWNBR.

           PERFORM START-BDAY-FILE.
       PROCESS-BDAY-NEXT.
           PERFORM READ-BDAY-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE BDAY-OWNBR       TO  D-OWNBR.
           MOVE BDAY-NAME        TO  D-NAME.
           MOVE BDAY-ACCTNO      TO  D-NUMBER.
           MOVE BDAY-DATE-MMDDYY TO  D-BDAY.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-BDAY-NEXT.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              MOVE "Y" TO RECORDS-FOUND.

           IF REPORTX = "Y"
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      **************************
       ORIGIN-CD-CHECK SECTION.
      **************************
           MOVE "Y" TO ORIGIN-MATCH.
           MOVE 0 TO ORIGIN-SUB.

       ORIGIN-NEXT-CHAR.
           ADD 1 TO ORIGIN-SUB.

           IF ORIGIN-CHAR(ORIGIN-SUB) NOT = ORIGIN-CD(ORIGIN-SUB) AND
              ORIGIN-CD(ORIGIN-SUB) NOT = "?"
              MOVE "N" TO ORIGIN-MATCH
              GO TO ORIGIN-CD-CHECK-EXIT.

           IF ORIGIN-SUB < 4
              GO TO ORIGIN-NEXT-CHAR.

       ORIGIN-CD-CHECK-EXIT.
           EXIT.

      ********************************************************
      * IF CORPORATE FORM PRINTING, THEN PUT THE
      * CORPORATE BRNO PATH IN THE FM-PATH, SO ALL FORM REQUESTS
      * ARE CREATED IN THE HOME OFFICE, THEN WHEN THEY GO "PRINT"
      * THEM ALL IT WILL CREATE AN ASCII FILE
      ********************************************************
      *************************
       GET-FM-PATH SECTION.
      *************************

           IF GP-CORP-BRNO = 0
              GO TO EXIT-GET-FM-PATH.

           MOVE BR-NO TO HOLD-BR-KEY.
           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO END-GET-FM-PATH.

           MOVE EXT-FILPATH-BASE           TO FM-ALL-BASE.
           MOVE BR-MACHINE                 TO FM-ALL-MACHINE.
           MOVE BR-DATA-PATH               TO FM-ALL-DATA.
           MOVE FM-ALL-PATH                TO FM-PATH.
           MOVE "Y"                        TO FM-PATH-OVERRIDE.

       END-GET-FM-PATH.
      * GET ORIGINAL BRANCH BACK
           MOVE HOLD-BR-KEY TO BR-NO.
           PERFORM START-BR-FILE.
           PERFORM READ-BR-FILE-NEXT.

       EXIT-GET-FM-PATH.
           EXIT.

      ******************************************
       CREATE-BDAY-RECORD SECTION.
      ******************************************
           IF FD-1STFILE = 2
              PERFORM CHECK-FOR-MANY-ACCTS-2.

           IF CREATE-FM-FG = "N"
              GO TO END-CREATE-BDAY-REC.

           MOVE SPACES TO BDAY-REC.

           MOVE BW-BDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO BDAY-DATE-MMDDYY.
           MOVE BW-SSNO     TO BDAY-SSNO.
           MOVE LN-ACCTNO   TO BDAY-ACCTNO.
           MOVE LN-OWNBR    TO BDAY-OWNBR.
           MOVE LN-LOANDATE TO BDAY-LOANDATE.
           STRING BW-FNAME, " ", BW-LNAME DELIMITED BY "   "
                            INTO BDAY-NAME.
           PERFORM WRITE-BDAY-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       END-CREATE-BDAY-REC.
           EXIT.

      *****************************************************
       INITIALIZATION SECTION.
      *****************************************************
           MOVE BWBDAY-QUEUE-POS TO Q-POS.
           MOVE BWBDAY-COPIES-POS TO C-POS.
           MOVE BWBDAY-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-USER-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE H-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE 99 TO LN-COUNT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "BIRTHDAY LISTING  RUN IN PROGRESS....." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO BDAY-PATH.
           PERFORM OPEN-BDAY-FILE-OUTPUT.
           PERFORM CLOSE-BDAY-FILE.
           PERFORM OPEN-BDAY-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO MANYACCTS-PATH.
           PERFORM OPEN-MANYACCTS-FILE-OUTPUT.
           PERFORM CLOSE-MANYACCTS-FILE.
           PERFORM OPEN-MANYACCTS-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************
       EO-EXEC-AUDIT SECTION.
      *********************************************
           MOVE EXT-EOBUF-DATE1 TO NUM-DATE.
           MOVE NUM-MO TO BEG-MM.
           MOVE NUM-DA TO BEG-DD.
           MOVE EXT-EOBUF-DATE2 TO NUM-DATE.
           MOVE NUM-MO TO END-MM.
           MOVE NUM-DA TO END-DD.

      *********************************************
       ENTER-PARAMETERS SECTION.
      *********************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO REC-ZCODE.

           PERFORM GET-USER-BRANCH.

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
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
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
           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
      *----------------------------------------------------------------

       REC-BWDT1.
           MOVE "ENTER BEGINNING BIRTH DATE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BWDT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BWDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BWDT1.
           MOVE VDUNUM0 TO BEG-BDAY.
       REC-BWDT2.
           MOVE "ENTER ENDING BIRTH DATE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BWDT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BWDT1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BWDT2.
           MOVE VDUNUM0 TO END-BDAY.


           MOVE BEG-MM   TO NUM-MO.
           MOVE BEG-DD   TO NUM-DA.
           MOVE 2017     TO NUM-CCYY.

           MOVE END-MM   TO S-MM.
           MOVE END-DD   TO S-DD.
           MOVE 2017     TO S-CCYY.
           IF SYS-DATE < NUM-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO REC-BWDT1.

           GO TO END-BWDT.

       ALL-BWDT.
           MOVE "SN N040BWDT1." TO VDU.
           MOVE 0101 TO BEG-BDAY VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040BWDT2." TO VDU.
           MOVE 1231 TO END-BDAY VDUNUM0.
           PERFORM SCREENIO.
       END-BWDT.

      *-----------------------------------------------------------------
       CL-01.
           MOVE "ENTER BEGINNING LOAN CLASS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO REC-BWDT2.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF NOT IO-GOOD
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENTER ENDING LOAN CLASS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF NOT IO-GOOD
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO REC-MINPAYSM.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       REC-MINPAYSM.
           MOVE "ENTER MINIMUM WHOLE PMTS MADE ON THE LOAN:"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MINPAYSM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINPAYSM.
           MOVE VDUNUM0 TO MINPAYS-MADE.

       REC-MINPAYSR.
           MOVE "ENTER MINIMUM WHOLE PMTS REMAINING ON THE LOAN:"
                                                                TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MINPAYSR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINPAYSM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINPAYSR.
           MOVE VDUNUM0 TO MINPAYS-REMAIN.

       REC-MINTERM.
           MOVE "ENNN030MINTERM." TO VDU.
           MOVE "ENTER MINIMUM ORIGINAL TERM, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINPAYSR.
           IF VDUSTATUS = "16" GO TO ALL-TERM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINTERM.
           MOVE VDUNUM0 TO MINTERM.
       REC-MAXTERM.
           MOVE "ENTER MAXIMUM ORIGINAL TERM:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MAXTERM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINTERM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXTERM.
           MOVE VDUNUM0 TO MAXTERM.
           IF MINTERM > MAXTERM
              GO TO REC-MINTERM.
           GO TO END-TERM.

       ALL-TERM.
           MOVE "SNNN030MINTERM." TO VDU.
           MOVE 0 TO MINTERM VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030MAXTERM." TO VDU.
           MOVE 999 TO MAXTERM VDUNUM0.
           PERFORM SCREENIO.
       END-TERM.

       REC-MINNXTPAY.
           MOVE "ENNN020MINNXTPAY." TO VDU.
           MOVE
           "ENTER MINIMUM DAYS UNTIL NEXT PAYMENT DUE, F3-ALL:"
                                                     TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MAXTERM.
           IF VDUSTATUS = "16" GO TO ALL-NXTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINNXTPAY.
           MOVE VDUNUM0 TO MINNXTPAY.
       REC-MAXNXTPAY.
           MOVE
           "ENTER MAXIMUM DAYS UNTIL NEXT PAYMENT DUE, F3-ALL:"
                                                     TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MAXNXTPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINNXTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXNXTPAY.
           MOVE VDUNUM0 TO MAXNXTPAY.
           IF MINNXTPAY > MAXNXTPAY
              GO TO REC-MINNXTPAY.
           GO TO END-NXTPAY.

       ALL-NXTPAY.
           MOVE "SNNN020MINNXTPAY." TO VDU.
           MOVE 0 TO MINNXTPAY VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020MAXNXTPAY." TO VDU.
           MOVE 999 TO MAXNXTPAY VDUNUM0.
           PERFORM SCREENIO.
       END-NXTPAY.

       REC-MINLSTPAY.
           MOVE "ENNN020MINLSTPAY." TO VDU.
           MOVE
           "ENTER MINIMUM DAYS SINCE LAST PAYMENT MADE, F3-ALL:"
                                                     TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MAXNXTPAY.
           IF VDUSTATUS = "16" GO TO ALL-LSTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINLSTPAY.
           MOVE VDUNUM0 TO MINLSTPAY.
       REC-MAXLSTPAY.
           MOVE
           "ENTER MAXIMUM DAYS SINCE LAST PAYMENT MADE, F3-ALL:"
                                                     TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MAXLSTPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINLSTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXLSTPAY.
           MOVE VDUNUM0 TO MAXLSTPAY.
           IF MINLSTPAY > MAXLSTPAY
              GO TO REC-MINLSTPAY.
           GO TO END-LSTPAY.

       ALL-LSTPAY.
           MOVE "SNNN020MINLSTPAY." TO VDU.
           MOVE ZERO TO MINLSTPAY VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020MAXLSTPAY." TO VDU.
           MOVE 999 TO MAXLSTPAY VDUNUM0.
           PERFORM SCREENIO.
       END-LSTPAY.

       REC-MINAV.
           MOVE "ENTER MINIMUM AVAILABLE CREDIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060MINAV." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MAXLSTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINAV.
           MOVE VDUNUM0 TO MINAV.

       MULTIPLE-ACCTS-YN.
           MOVE "INCLUDE ALL ACCOUNTS (Y/N)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010MANYACCTS." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO REC-MINAV.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO MULTIPLE-ACCTS-YN.
           MOVE VDUBUF TO MANY-ACCOUNTS.
           IF NOT (MANY-ACCOUNTS = "Y" OR "N")
              GO TO MULTIPLE-ACCTS-YN.

       ENTER-ORIGIN.
           MOVE "ENTER BORROWER ORIGIN (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ORIGIN." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ORIGIN.
           IF UP-KEY GO TO MULTIPLE-ACCTS-YN.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY AND NOT DOWN-KEY
             GO TO ENTER-ORIGIN.

           MOVE VDUBUF TO ORIGINX.
           GO TO REC-FORM.

       ALL-ORIGIN.
           MOVE "S  A040ORIGIN." TO VDU.
           MOVE "????" TO ORIGINX VDUBUF.
           PERFORM SCREENIO.

       REC-FORM.
           MOVE "ENTER FORM TO BE SENT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ORIGIN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-FORM.
           MOVE VDUNUM0 TO FORMNO FD-FMNO.
           IF FORMNO NOT = 0
              PERFORM OPEN-FD1-FILE
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE
              IF NOT IO-GOOD
                 GO TO REC-FORM
              ELSE
                 PERFORM VALIDATE-FORM
                 IF IO-FG NOT = 0
                    GO TO REC-FORM..

       REC-ZCODE.
           MOVE "PRINT FORMS IN NAME OR ZIP CODE ORDER (A/Z):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ZIP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-FORM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REC-ZCODE.
           MOVE VDUBUF TO ZIP-CODE.
           IF ZIP-CODE NOT = "A" AND NOT = "Z"
              GO TO REC-ZCODE.

       PRINT-REPORT.
           MOVE "INCLUDE REPORT:"   TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RPT."   TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO REC-ZCODE.
           IF NOT(ENTER-KEY OR DOWN-KEY) GO TO PRINT-REPORT.
           MOVE VDUBUF TO REPORTX.
           IF NOT (REPORTX =  "Y" OR "N")
              GO TO PRINT-REPORT.
           IF FORMNO = 0
              IF REPORTX = "N"
                 GO TO PRINT-REPORT.

       CORP-FM.
      * IF BUILDING EXTRACTION FILE (LONPEX), MUST USE CORP PRINT FLAG
           IF ( FD-OPTION-EXTRACTION )
              MOVE "S  A010CORPFM." TO VDU
              MOVE "Y" TO CORP-FM-FG VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-PARM.
      * WORLD PR#1293, BRANCHES ENTER THEIR OWN BRANCH RANGE BUT NEED
      *                EXTRACTION FILE (LONPEX)..CORP BRANCH PRINTING
      *    IF BEG-BRANCH = END-BRANCH
      *       MOVE "S  A010CORPFM." TO VDU
      *       MOVE "N" TO CORP-FM-FG VDUBUF
      *       PERFORM SCREENIO
      *       GO TO EXIT-PARM.
           MOVE "E  A010CORPFM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-REPORT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CORP-FM.
           MOVE VDUBUF TO CORP-FM-FG.
           IF CORP-FM-FG NOT = "Y"
              IF CORP-FM-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO CORP-FM.

       EXIT-PARM.
           PERFORM CLEAR-LEGEND.

      *******************************
       VALIDATE-FORM SECTION.
      *******************************
           MOVE 0 TO IO-FG.

           IF NOT (FD-1STFILE = 1 OR 2 OR 4)
              MOVE "FIRST FILE MUST BE LN, BW, OR NO FILE" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
           ELSE
              IF (FD-1STFILE = 1 OR 4) AND (FD-FMTYPE NOT = "S")
                 MOVE "FORM IS NOT SOLICITATION TYPE" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.

           IF FD-SECURITY-PROFILE NOT = " "
              IF EXT-CONAME-USER-PROFILE-CD > FD-SECURITY-PROFILE
                 MOVE "YOU ARE NOT AUTHORIZED TO REQUEST THIS FORM."
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.

           IF FD-VALID-BRANCH NOT = SPACES
              IF FD-VALID-BRANCH NUMERIC
                 IF EXT-CONAME-USER-LOC NOT = FD-VALID-BRANCH
                    MOVE "NOT A VALID BRANCH" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                 ELSE
                    NEXT SENTENCE
              ELSE
                 MOVE FD-VALID-BRANCH TO GR-ID
                 PERFORM OPEN-GR1-FILE
                 PERFORM SEARCH-GR-INIT
                 MOVE EXT-CONAME-USER-LOC TO WGR-SRESULT
                 PERFORM SEARCH-GR-VERIFY
                 IF WGR-ERR > 0
                    MOVE "NOT A VALID BRANCH" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG.

       EXIT-VALIDATE-FORM.
           EXIT.

      **************************************
       DISPLAY-PARAMETERS SECTION.
      **************************************
           MOVE LOAN-FIELDS TO SCREEN-BUF.
           MOVE SCREEN-BUF TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************
       CREATE-FM-REC SECTION.
      *************************
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

      * CUTSHEET PRINT WONT WORK IF BRNO2 IS FILLED IN !!! IF THEY USE
      * CUSTSHEET, DO NOT ANSWER "Y" TO CORPORATE PRINT !!
           IF CORP-FM-FG = "Y"
              MOVE LN-OWNBR   TO LPWSFM-EDIT-FM-BRNO2
                                 LPWSFM-EDIT-FM-BW-BRNO2.

           IF FD-1STFILE = 2
              MOVE LN-SSNO(1)             TO LPWSFM-EDIT-FM-SSNO
              MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
           ELSE
              MOVE LN-ACCTNO              TO LPWSFM-EDIT-FM-ACCTNO
              MOVE FORMNO                 TO LPWSFM-EDIT-FM-FORMNO
              MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
              PERFORM CHECK-FOR-MULTIPLE-ACCTS
              IF CREATE-FM-FG = "N"
                 GO TO END-CREATE-FM-REC.

           IF FORMNO = ZERO
              GO TO SKIP-CREATE-FM.

      * THESE MUST STAY HERE BELOW CHECK-FOR-MULTIPLE

           PERFORM OPEN-FM1-FILE.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE FORMNO        TO FM-FMNO.

           MOVE FM1-KEY TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

      * PRINT FORMS IN LAST NAME ORDER
           IF ZIP-CODE = "A"
              MOVE BW-LNAME    TO EDIT-FM-LAST
              MOVE BW-FNAME    TO EDIT-FM-FIRST
              MOVE EDIT-FM-ZIP TO FM-ZIP
           ELSE
      * PRINT FORMS IN ZIP CODE ORDER
              MOVE BW-ZIP      TO FM-ZIP.

           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           IF IO-GOOD
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

           PERFORM CLOSE-FM1-FILE.

       SKIP-CREATE-FM.

           IF REPORTX = "Y"
              PERFORM CREATE-BDAY-RECORD.

       END-CREATE-FM-REC.
           EXIT.

      *******************************************
       CHECK-FOR-MULTIPLE-ACCTS SECTION.
      *******************************************
           MOVE "Y" TO CREATE-FM-FG.

           IF MANY-ACCOUNTS NOT  = "N"
              GO TO EXIT-CHECK-FOR-MANY-ACCTS.

           MOVE LN-SSNO(1)    TO MANYACCTS-SSNO.
           PERFORM READ-MANYACCTS-FILE.

           IF IO-BAD
              MOVE LN-SSNO(1)   TO MANYACCTS-SSNO
              MOVE LN-OWNBR     TO MANYACCTS-OWNBR
              MOVE LN-LOANDATE  TO MANYACCTS-LOANDATE
              MOVE HOLD-BDATE   TO MANYACCTS-BDAY
              MOVE FM-INITKEY   TO MANYACCTS-INITKEY
              MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH
              MOVE BR-MACHINE   TO MANYACCTS-MACHINE
              PERFORM WRITE-MANYACCTS-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO EXIT-CHECK-FOR-MANY-ACCTS.

      *MULTIPLE ACCOUNTS - GET ACCOUNT WITH LATEST LOAN DATE
      *TO CREATE REQUEST
           IF LN-LOANDATE < MANYACCTS-LOANDATE
              MOVE "N" TO CREATE-FM-FG
              GO TO EXIT-CHECK-FOR-MANY-ACCTS.

           MOVE FM-INITKEY TO SAVE-INITKEY.
           MOVE "Y" TO CREATE-FM-FG.
           PERFORM REMOVE-PRIOR-REQUEST.
           MOVE SAVE-INITKEY TO FM-INITKEY.

       END-UPDATE-MANYACCTS.
           MOVE LN-SSNO(1)   TO MANYACCTS-SSNO.
           MOVE LN-LOANDATE  TO MANYACCTS-LOANDATE.
           MOVE LN-OWNBR     TO MANYACCTS-OWNBR.
           MOVE FM-INITKEY   TO MANYACCTS-INITKEY.
           MOVE HOLD-BDATE   TO MANYACCTS-BDAY.
           MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH.
           MOVE BR-MACHINE   TO MANYACCTS-MACHINE.
           PERFORM REWRITE-MANYACCTS-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      *---------------------------------------------
       EXIT-CHECK-FOR-MANY-ACCTS.
           EXIT.


      *********************************************
       REMOVE-PRIOR-REQUEST SECTION.
      *********************************************

           IF FORMNO = 0
              GO TO REMOVE-BDAY-REC.

           MOVE MANYACCTS-OWNBR     TO FM-BRNO.
           MOVE MANYACCTS-INITKEY   TO FM-INITKEY.
           MOVE FORMNO              TO FM-FMNO.

           IF CORP-FM-FG = "N"
              MOVE FM-ALL-PATH TO SAVE-FM-ALL-PATH
              IF FM-ALL-DATA NOT = MANYACCTS-DATA-PATH
                 MOVE EXT-FILPATH-BASE     TO FM-ALL-BASE
                 MOVE MANYACCTS-MACHINE    TO FM-ALL-MACHINE
                 MOVE MANYACCTS-DATA-PATH  TO FM-ALL-DATA
                 MOVE FM-ALL-PATH          TO FM-PATH
                 MOVE "Y"                  TO FM-PATH-OVERRIDE.

           PERFORM OPEN-FM1-FILE.
           PERFORM READ-FM1-FILE.
           IF IO-GOOD
              PERFORM DELETE-FM1-FILE.
           PERFORM CLOSE-FM1-FILE.

           IF CORP-FM-FG = "N"
              MOVE SAVE-FM-ALL-PATH  TO FM-ALL-PATH FM-PATH.

       REMOVE-BDAY-REC.
            MOVE MANYACCTS-OWNBR  TO BDAY-OWNBR.
            MOVE MANYACCTS-BDAY   TO DATE-YYYYMMDD.
            PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
            MOVE DATE-MMDDYY      TO BDAY-DATE-MMDDYY.
            MOVE MANYACCTS-SSNO   TO BDAY-SSNO.
            MOVE MANYACCTS-ACCTNO TO BDAY-ACCTNO.

            PERFORM READ-BDAY-FILE.
            IF IO-GOOD
               PERFORM DELETE-BDAY-FILE.

       EXIT-REMOVE-PRIOR-REQUEST.
           EXIT.

      *--------------------------------------------------------
      * THIS IS ANOTHER ROUTINE TO CHECK FOR MULTIPLE ACCTS,
      * JUST FOR KEEPING THEM OFF REPORT (OUT OF WKFILE) IF
      * 1ST FILE IS 2, CREATE-FM DOESN'T CHECK FOR MULTIPLES
      * WILL CHECK REGARDLESS OF MANY-ACCTS FLAG, SINCE 1ST
      * FILE OF 2 WILLL NEVER HAVE MULTIPLES!
      *--------------------------------------------------------
      *******************************************
       CHECK-FOR-MANY-ACCTS-2 SECTION.
      *******************************************
           MOVE "Y" TO CREATE-FM-FG.

           MOVE LN-SSNO(1)    TO MANYACCTS-SSNO.
           PERFORM READ-MANYACCTS-FILE.
           IF IO-BAD
              MOVE LN-SSNO(1)    TO MANYACCTS-SSNO
              MOVE LN-OWNBR      TO MANYACCTS-OWNBR
              MOVE LN-ACCTNO     TO MANYACCTS-ACCTNO
              MOVE FORMNO        TO MANYACCTS-FORMNO
              MOVE LN-LOANDATE   TO MANYACCTS-LOANDATE
              MOVE HOLD-BDATE    TO MANYACCTS-BDAY
              MOVE BR-DATA-PATH  TO MANYACCTS-DATA-PATH
              MOVE BR-MACHINE    TO MANYACCTS-MACHINE
              PERFORM WRITE-MANYACCTS-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO EXIT-CHECK-FOR-MANY-ACCTS-2.

      *MULTIPLE ACCOUNTS - GET ACCOUNT WITH LATEST LOAN DATE
      *TO CREATE REQUEST
           IF LN-LOANDATE < MANYACCTS-LOANDATE
              MOVE "N" TO CREATE-FM-FG
              GO TO EXIT-CHECK-FOR-MANY-ACCTS-2.

           MOVE "Y" TO CREATE-FM-FG.
           PERFORM REMOVE-PRIOR-REQUEST-2.

       END-UPDATE-MANYACCTS-2.
           MOVE LN-SSNO(1)   TO MANYACCTS-SSNO.
           MOVE LN-LOANDATE  TO MANYACCTS-LOANDATE.
           MOVE LN-ACCTNO    TO MANYACCTS-ACCTNO.
           MOVE FORMNO       TO MANYACCTS-FORMNO.
           MOVE LN-OWNBR     TO MANYACCTS-OWNBR.
           MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH.
           MOVE BR-MACHINE   TO MANYACCTS-MACHINE.
           PERFORM REWRITE-MANYACCTS-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-CHECK-FOR-MANY-ACCTS-2.
           EXIT.


      ******************************************
       REMOVE-PRIOR-REQUEST-2 SECTION.
      ******************************************
            MOVE MANYACCTS-OWNBR  TO BDAY-OWNBR.
            MOVE MANYACCTS-BDAY   TO DATE-YYYYMMDD.
            PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
            MOVE DATE-MMDDYY      TO BDAY-DATE-MMDDYY.
            MOVE MANYACCTS-SSNO   TO BDAY-SSNO.
            MOVE MANYACCTS-ACCTNO TO BDAY-ACCTNO.
            PERFORM READ-BDAY-FILE.
            IF IO-GOOD
               PERFORM DELETE-BDAY-FILE.

       EXIT-REMOVE-PRIOR-REQUEST-2.
           EXIT.


      *******************************************
      *    TEST PROFILES:
      *
      *    0 - CUR       6 - 180 TO 269
      *    1 - 30        7 - 270+
      *    2 - 60        8 - NOT USED
      *    3 - 90        9 - MONTH NOT GRADED
      *    4 - 120
      *    5 - 150
      *******************************************
      *******************************************
       TEST-PROFILE SECTION.
      *******************************************
           MOVE "Y" TO PROFILE-OK.
           DIVIDE 30 INTO PROFILE-HIGH.
           IF PROFILE-HIGH > 6
              IF PROFILE-HIGH < 9
                 MOVE 6 TO PROFILE-HIGH
              ELSE
                 MOVE 7 TO PROFILE-HIGH.
           MOVE 1 TO SUB.
       TEST-PROFILE-AGAIN.
           IF SUB > 12
              GO TO TEST-PROFILE-EXIT.
           IF PROFILE-TAB(SUB) = 9
              GO TO TEST-PROFILE-EXIT.
           IF PROFILE-TAB(SUB) NOT > PROFILE-HIGH
              ADD 1 TO SUB
              GO TO TEST-PROFILE-AGAIN.
           MOVE " " TO PROFILE-OK.
       TEST-PROFILE-EXIT.
           EXIT.


      *******************************
       HEAD-PAGE SECTION.
      ******************************
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

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 4  TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

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
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           MOVE "BEGINNING BIRTHDATE         :" TO RANGE-LINE.
           MOVE BEG-BDAY                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BIRTHDATE            :" TO RANGE-LINE.
           MOVE END-BDAY                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS             :" TO RANGE-LINE.
           MOVE BEG-CLASS                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING                      :" TO RANGE-LINE.
           MOVE END-CLASS                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM WHOLE PAYS MADE     :" TO RANGE-LINE.
           MOVE MINPAYS-MADE                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM WHOLE PAYS REMAIN   :" TO RANGE-LINE.
           MOVE MINPAYS-REMAIN                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM TERM                :" TO RANGE-LINE.
           MOVE MINTERM                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM TERM                :" TO RANGE-LINE.
           MOVE MAXTERM                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM DAYS TO NEXT PAYMENT:" TO RANGE-LINE.
           MOVE MINNXTPAY                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM DAYS TO NEXT PAYMENT:" TO RANGE-LINE.
           MOVE MAXNXTPAY                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM DAYS SINCE LAST PAY :" TO RANGE-LINE.
           MOVE MINLSTPAY                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM DAYS SINCE LAST PAY :" TO RANGE-LINE.
           MOVE MAXLSTPAY                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN AVAIL CREDIT            :" TO RANGE-LINE.
           MOVE MINAV                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE MULTIPLE ACCOUNTS   :" TO RANGE-LINE.
           MOVE MANY-ACCOUNTS                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BORROWER ORIGIN             :" TO RANGE-LINE.
           MOVE ORIGINX                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FORM NUMBER                 :" TO RANGE-LINE.
           MOVE FORMNO                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FORM PRINT ORDER (A/Z)      :" TO RANGE-LINE.
           MOVE ZIP-CODE                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE REPORT              :" TO RANGE-LINE.
           MOVE REPORTX                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CORPORATE FORM PRINTING     :" TO RANGE-LINE.
           MOVE CORP-FM-FG                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.


      ********************************************
       CLASS-SCAN SECTION.
      ********************************************
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

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO BWBDAY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWBDAY-SYSDATE.
           DISPLAY BWBDAY-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BWBDAY-FORM-FIELDS TO WR-BUF.
           MOVE BWBDAY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE BWBDAY-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWBDAY-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWBDAY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
      *    MISC ROUTINES SECTION
      ************************************************
       MISC-ROUTINES SECTION.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       GET-USER-BRANCH.
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "N" TO BR-AVAIL-CREDIT
           ELSE
              MOVE EXT-CONAME-USER-LOC TO BR-NO
              PERFORM GET-BR
              IF NOT IO-GOOD
                 MOVE "N" TO BR-AVAIL-CREDIT.

       GET-SPR.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
       GET-BR.
           IF BR-NO NOT = HOLD-BR-KEY
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              MOVE BR-NO TO HOLD-BR-KEY.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       OPEN-BDAY-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE BDAY-IFN TO E-FILE.
           OPEN OUTPUT BDAY-FILE.
           IF IO-FG = 8
              GO TO OPEN-BDAY-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE BDAY-FILE
              GO TO OPEN-BDAY-FILE-OUTPUT.
           UNLOCK BDAY-FILE.
       OPEN-BDAY-FILE.
           PERFORM OPEN-IT.
           MOVE BDAY-IFN TO E-FILE.
           OPEN I-O BDAY-FILE.
           IF IO-FG = 8
              GO TO OPEN-BDAY-FILE.
           IF IO-FG = 7
              CLOSE BDAY-FILE
              GO TO OPEN-BDAY-FILE.
           UNLOCK BDAY-FILE.
       READ-BDAY-FILE.
           PERFORM READ-IT.
           MOVE BDAY-IFN TO E-FILE.
           MOVE BDAY-KEY TO E-KEYX.
           READ BDAY-FILE.
           IF IO-FG = 8
              GO TO READ-BDAY-FILE.
       READ-BDAY-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BDAY-IFN TO E-FILE.
           MOVE BDAY-KEY TO E-KEYX.
           READ BDAY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-BDAY-FILE-NEXT.
       START-BDAY-FILE.
           PERFORM START-IT.
           MOVE BDAY-IFN TO E-FILE.
           MOVE BDAY-KEY TO E-KEYX.
           START BDAY-FILE KEY NOT < BDAY-KEY.
           UNLOCK BDAY-FILE.
       WRITE-BDAY-FILE.
           PERFORM WRITE-IT.
           MOVE BDAY-IFN TO E-FILE.
           MOVE BDAY-KEY TO E-KEYX.
           WRITE BDAY-REC.
           IF IO-FG = 8
              GO TO WRITE-BDAY-FILE.
           UNLOCK BDAY-FILE.
       DELETE-BDAY-FILE.
           PERFORM DELETE-IT.
           MOVE BDAY-IFN TO E-FILE.
           MOVE BDAY-KEY TO E-KEYX.
           DELETE BDAY-FILE.
           IF IO-FG = 8
              GO TO DELETE-BDAY-FILE.
           UNLOCK BDAY-FILE.
       CLOSE-BDAY-FILE.
           PERFORM CLOSE-IT.
           CLOSE BDAY-FILE.

       OPEN-MANYACCTS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           OPEN OUTPUT MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO OPEN-MANYACCTS-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE MANYACCTS-FILE
              GO TO OPEN-MANYACCTS-FILE-OUTPUT.
           UNLOCK MANYACCTS-FILE.

       OPEN-MANYACCTS-FILE.
           PERFORM OPEN-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           OPEN I-O MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO OPEN-MANYACCTS-FILE.
           IF IO-FG = 7
              CLOSE MANYACCTS-FILE
              GO TO OPEN-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       READ-MANYACCTS-FILE.
           PERFORM READ-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           READ MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO READ-MANYACCTS-FILE.

       WRITE-MANYACCTS-FILE.
           PERFORM WRITE-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           WRITE MANYACCTS-REC.
           IF IO-FG = 8
              GO TO WRITE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.


       REWRITE-MANYACCTS-FILE.
           PERFORM REWRITE-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           REWRITE MANYACCTS-REC.
           IF IO-FG = 8
              GO TO REWRITE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       DELETE-MANYACCTS-FILE.
           PERFORM DELETE-IT.
           MOVE MANYACCTS-PATH TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           DELETE MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO DELETE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       CLOSE-MANYACCTS-FILE.
           PERFORM CLOSE-IT.
           CLOSE MANYACCTS-FILE.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE BDAY-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE MANYACCTS-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF EXT-ROUTE-BUF  = "00"
              MOVE "LP/ASMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BWBDAY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

