       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ASPAID.
       DATE-WRITTEN.    090694.
      ******************************************************************
      *    DIR :  LP
      *    DESC:  ADVERTISING/SOLICITATION LOAN SELECTION
      *           PAID OUT ACCOUNTS ONLY, NO BORROWER OPTIONS
      *           SKIP PB, RN , AND SC
      *
      * REV:
      *  083094 BAH ADDED ALPHA OR ZIP CODE ORDER
      *  093094 BAH ADDED A PAID OUT DATE RANGE
      *  101294 BAH REMOVE BR-STATE TEST, THEY DO SOLICIT OUT OF STATE
      *  KEC 091096 FIXED PRINTING "NO REPORT GENERATED" WHEN EXITING.
      *  MJD 090397 FIXED BUG: TESTING MIN/MAXNXTPAY WHEN IT SHOULD HAVE
      *             BEEN TESTING MIN/MAXLSTPAY.
      *  MJD 021998 FIXED NO RECORDS FOUND BUG. REQ#158
      *  MJD 990204 ADDED TEST OF BW-SOLICIT AND CONFIDENTCD.
      *             (REQ# 206 - BC ACCEPTANCE)
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BLV 991122 SKIP ACCOUNTS WHICH WERE TRANSFERRED, PER JIM R. (WORLD)
      *  CS  020110 ADD OPTION TO SKIP MULTIPLE ACCOUNTS [PR#341 REGIONAL MGMT]
      *  BAH 040223 ADD BEG AND END CRSCORE FOR PACESETTER, PR# 2250
      *  BAH 040419 ADDED ORIGIN FOR CASHCO, PR# 2273
      *  BLF 041014 MAY ENTER A GROUP, NOW CROSSES DATA PATHS, ADDED OPTION
      *             FOR CORP PRINTING, ADDED BRSECURE.  IF RUNNING FOR
      *             GROUP OR BRANCH RANGE, WILL CHECK FOR DUPLICATE SSNOS
      *             REGARDLESS OF BRANCH IF "N" TO "MULTIPLE ACCOUNTS".
      *             IF CORP PRINTING SELECTED, WILL SORT BY ZIP REGARDLESS
      *  BAH 051202 SUNBELT ONLY, WANTS REC AND CON PROFILE TEST TO INCLUDE
      *             ACCOUNTS UP TO 90 DAYS, NOT JUST 30....SUNSHINE/HERITAGE/
      *             SUNBELT PR# 2656
      *  BAH 051207 SUNBELT WANTS TO SKIP NONFILE PAIDOUTS (92)
      *  BAH 071008 WORLD NO LONGER SOLICITING BORROWERS IN KANSAS PR#557
      *  BLM 100414 INSTEAD OF SKIPPING LN-POCD OF 10 OR PB, JUST SKIP IF
      *             LN-PO-TRCD NOT = PO, WORLD PR# 684
      * BAH 150213 WORLD WANTS TO SKIP BW-PRIVACY-OPT-OUT = "M" OR "B"
      *            SO THEY DONT DUPLICATED THEIR TEXTING EFFORTS, PR# 1054
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      * BAH 170125 TAKE OUT LAST CHANGE(1054), THEY FEEL ALL CUSTOMERS 
      *            SHOULD BE ELIGIBLE FOR MAILED SOLICITATION IN ADDITION
      *            TO TEXT MESSAGE REGARDLESS OF OPT-IN CODE, WORLD PR#1221
      * BAH 170320 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171229 CORPORATE FORM PRINTING WAS NOT WORKING, NEEDED TO READ
      *            GP-CORP-BRNO AND SET CORRECT FM-PATH, ALSO DEFAULT CORP
      *            FORM FLAG TO "Y" IF EXTRACTION FORM, WORLD #1293
      * BAH 180205 TEST BRANCH RESTRICTIONS ON FORM REQUEST, FD-SECURITY
      *            AND FD-VALID-BRANCH, WORLD PR#1301
      * BAH 180212 FORCE MANY-ACCOUNTS TO "N" FOR WORLD AND DO NOT ALLOW
      *            SCREEN ENTRY ON THAT FIELD, WORLD #1301A
      * BAH 180215 MESSED UP THE REMOVE-PRIOR MANY-ACCTS SAVE FORM PATH
      *            PUT IT BACK! PR#1301B
      * BAH 181112 GLOBAL FMFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 190108 GLOBAL NOFILE
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 190613 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE, FMFILE
      * BAH 20200420 REPLACED LOCAL OPENS WITH LOADS AND MOVED OPENS
      *              TO INITIALIZE, REMOVED CLOSES, LET DECLARATIVES CLOSE
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * MB  20231027 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *              ADDED STATE PARAMETER ARRAY PROCESSING
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      * BAH 2024.0201 REMOVED NOTICE FOR A FORM REQUEST
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

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

           03  MANYACCTS-POFFDATE       PIC 9(8) COMP-3.
           03  MANYACCTS-OWNBR          PIC 9(4).
           03  MANYACCTS-DATA-PATH      PIC X(6).
           03  MANYACCTS-MACHINE        PIC XX.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/ASPAID.CBL S35 05/21/24-09:20:25 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
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
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
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
       01  MANYACCTS-IFN PIC X(9)    VALUE "MANYACCTS".
       01  HOLD-BR-KEY   PIC 9(4)    VALUE 0.

       01  MANYACCTS-PATH  PIC X(35) VALUE SPACES.

       01  HOLD-FM1-KEY            PIC X(28) VALUE SPACES.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-MINPAYS-MADE              PIC 9(5).
       01  PASS-MINPAYS-REMAIN            PIC 9(5).
       01  PASS-BEG-CRSCORE               PIC 9(5).
       01  PASS-END-CRSCORE               PIC 9(5).
       01  PASS-POFFDATE1                 PIC X(10).
       01  PASS-POFFDATE2                 PIC X(10).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW            PIC X        VALUE "N".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  EDIT-FM-ZIP.
           03  EDIT-FM-LAST     PIC X(9).
           03  EDIT-FM-FIRST    PIC XXX.

       01  SUB                  PIC 999.
       01  PODATE1              PIC 9(8).
       01  PODATE2              PIC 9(8).

       01  ORIGIN-WK.
           03  ORIGIN-CHAR      PIC X        OCCURS 4.
       01  ORIGIN-SUB           PIC 9.
       01  ORIGIN-MATCH         PIC X.
       01  PAYS-MADE            PIC 999.
       01  PAYS-REMAIN          PIC 999.

       01  PROFILE-OK           PIC X.
       01  PROFILE-HIGH         PIC 999.
       01  PROFILE-WORKER-01.
           03  PROFILE-WORKER   PIC 9(12).
           03  PROFILE-TAB REDEFINES PROFILE-WORKER
                                OCCURS 12 PIC 9.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.


       01  CREATE-FM-REQUEST    PIC X VALUE "Y".

       01  SAVE-INITKEY         PIC X(20) VALUE SPACES.
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  LOAN-FIELDS.
               05  MINPAYS-MADE    PIC 999.
               05  MINPAYS-REMAIN  PIC 999.
               05  MINTERM         PIC 999.
               05  MAXTERM         PIC 999.
               05  BEG-PODATE      PIC 99/99/99.
               05  END-PODATE      PIC 99/99/99.
               05  MINNXTPAY       PIC 99.
               05  MAXNXTPAY       PIC 99.
               05  MINLSTPAY       PIC 99.
               05  MAXLSTPAY       PIC 99.
               05  MINAV           PIC 9(6).
               05  FORMNO          PIC 999.
               05  ZIP-CODE        PIC X.
               05  MANY-ACCOUNTS   PIC X.
               05  BEG-CRSCORE     PIC 9(3).
               05  END-CRSCORE     PIC 9(3).
               05  ORIGINX.
                   07  ORIGIN-CD   PIC X OCCURS 4.
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  CORP-FM-FG       PIC X.

       01  VERSION-CONTROL         PIC X      VALUE "C".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X          VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)       VALUE SPACES.

       01  SAVE-FM-ALL-PATH    PIC X(35).
       01  SAVE-NO-ALL-PATH    PIC X(35).

       01  SCREEN-BUF              PIC X(57) VALUE SPACES.

       01  SCREEN-LIST.
           03  FILLER          PIC X(56) VALUE
           "MINPAYSM MINPAYSR MINTERM MAXTERM LNDT1 LNDT2 MINNXTPAY ".
           03  FILLER          PIC X(36) VALUE
           "MAXNXTPAY MINLSTPAY MAXLSTPAY MINAV ".
           03  FILLER          PIC X(42) VALUE
           "FORMNO ZIP MANYACCTS SCORE1 SCORE2 ORIGIN.".


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
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
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GRWORKW.CPY".

       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ASPAID_DEF.CPY".
       COPY "LIBLP/ASPAID_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBLP/ARRAYSPW.CPY". 

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ASPAID_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE MANYACCTS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-LN3-CONNECTION.
           CLOSE MANYACCTS-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ASPAID" TO VDU-FORM-NAME.

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
                                              LN-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              FM-PATH-OVERRIDE
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

           IF ( CORP-FM-FG = "Y")
              IF FORMNO NOT = 0
                 PERFORM GET-FM-PATH.

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

      * MOVE OPENS TO INITIALIZE, REPLACE WITH LOADS TO SET XX-PATH-OWNBR 
           PERFORM LOAD-LN3-FILE.
           PERFORM LOAD-BW1-FILE.

      *     MOVE 0             TO LN-CLASS LN-ACCTNO.

           MOVE BR-NO          TO LN-OWNBR QLN3-WBEG-OWNBR.
           MOVE MINPAYS-MADE   TO PASS-MINPAYS-MADE.
           MOVE MINPAYS-REMAIN TO PASS-MINPAYS-REMAIN.
           MOVE BEG-CRSCORE    TO PASS-BEG-CRSCORE.
           MOVE END-CRSCORE    TO PASS-END-CRSCORE.

           MOVE PODATE1      TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-POFFDATE1.
           MOVE PODATE2      TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-POFFDATE2.

           PERFORM START-LN3-FILE.  *> SSP LN BW
      D    STOP MESS.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.  *> EMBEDDED LN BW
           IF IO-BAD                       *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      *  POCD - ** IS A DELETED CONTRACT, "BT" IS TRANSFERRED ACCOUNT
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-POCD = "**" OR LN-POCD = "BT"
              GO TO NEXT-LN.

      * CHANGED TO JUST SKIP LN-POTRCD NOT = PO, WORLD PR# 684
      *    IF LN-POCD = "PB" OR "10"
      *       GO TO NEXT-LN.

      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-PO-TRCD NOT = "PO"
              GO TO NEXT-LN.

      * SKIP PL ACCOUNTS:
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN.

      * SKIP JUDGEMENTS:
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-JDDATE NOT = 0
              GO TO NEXT-LN.

      * SKIP CONFIDENTIAL ACCOUNTS:
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-CONFIDENTCD = "Y"
              GO TO NEXT-LN.

      * ALWAYS SKIP OPEN ACCOUNTS:
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-CURBAL NOT = 0
              GO TO NEXT-LN.

      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-POFFDATE = 0
              GO TO NEXT-LN.

      * SKIP REPO, LEGAL, BANKRUPT, AND DEBTAID ACCOUNTS:
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-REPODATE NOT = 0 OR
               LN-DEBTAIDATE NOT = 0 OR
                LN-LEGALDATE NOT = 0 OR
                 LN-BNKRPTDATE NOT = 0
                    GO TO NEXT-LN.


      * TEST DATE RANGE:
           IF LN-POFFDATE <  PODATE1 OR LN-POFFDATE > PODATE2
              GO TO NEXT-LN.

      * TEST CRSCORE RANGE
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF LN-CRSCORE < BEG-CRSCORE OR LN-CRSCORE > END-CRSCORE
              GO TO NEXT-LN.

      * TEST MINIMUM PAYS MADE:
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF MINPAYS-MADE NOT = 0
              IF LN-REGPYAMT NOT = 0
                 COMPUTE PAYS-MADE = LN-TOTPAYMNTD / LN-REGPYAMT
                 IF PAYS-MADE < MINPAYS-MADE
                    GO TO NEXT-LN.

      * TEST MINIMUM PAYS REMAINING:
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF MINPAYS-REMAIN NOT = 0
              COMPUTE PAYS-REMAIN =
                  (LN-ORGTERM * LN-REGPYAMT - LN-TOTPAYMNTD)
                                                  / LN-REGPYAMT
              IF PAYS-REMAIN < MINPAYS-REMAIN
                 GO TO NEXT-LN.

      * TEST MINIMUM/MAXIMUM ORIGINAL TERM:
           IF LN-ORGTERM < MINTERM OR LN-ORGTERM > MAXTERM
              GO TO NEXT-LN.

           PERFORM GET-SPR.

      * TEST MINIMUM/MAXIMUM TO THE NEXT DUE PAYMENT:
           IF MINNXTPAY = 0
              IF MAXNXTPAY = 99
                 GO TO TEST-DAYS-SINCE-LAST-PAYMENT.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

           MOVE TRANS-DATE TO NUM-DATE.
           MOVE NDTE-DATE  TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < MINNXTPAY OR ELAPSED-DAYS > MAXNXTPAY
              GO TO NEXT-LN.

      * TEST MINIMUM/MAXIMUM DAYS SINCE LAST PAYMENT:
       TEST-DAYS-SINCE-LAST-PAYMENT.
           IF MINLSTPAY = 0
              IF MAXLSTPAY = 99
                 GO TO TEST-CONTRACTUAL-PROFILE.

           IF LN-DATE-PAID-LAST = 0
              GO TO NEXT-LN.

           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           MOVE TRANS-DATE        TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < MINLSTPAY OR ELAPSED-DAYS > MAXLSTPAY
              GO TO NEXT-LN.

       TEST-CONTRACTUAL-PROFILE.
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
               MOVE TRANS-DATE    TO AVCR-PAYDATE
               MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
               PERFORM COMPUTE-AVAILABLE-CREDIT
               IF AVCR-AVAILCREDIT < MINAV
                  GO TO NEXT-LN.

      * BW FIELDS RETRIEVED IN STORED PROCEDURE
      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF NOT IO-GOOD
      *        GO TO NEXT-LN.

      * MAKE SURE BORROWER DOESNT HAVE ANOTHER LOAN
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF BW-LIASMLN NOT = 0 OR
               BW-LIAOTH NOT = 0
              GO TO NEXT-LN.

      * 10/8/2007 PR# 557
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF BW-STATE = "KS"
              GO TO NEXT-LN.

      *    -------------------------------------------------------------
      *    TEST SOLICIT FLAG AND CONFIDENTIAL CODE.
      *    -------------------------------------------------------------
      * THIS CHECK ALSO MADE IN STORED PROCEDURE
           IF BW-SOLICIT = "N" OR BW-CONFIDENTIAL = "Y"
              GO TO NEXT-LN.

      * TEST BORROWER ORIGIN (CASHCO PR# 2273)
           IF ORIGINX NOT = "????"
              MOVE BW-ORIGIN TO ORIGIN-WK
              PERFORM ORIGIN-CD-CHECK
              IF ORIGIN-MATCH NOT = "Y"
                 GO TO NEXT-LN.

      * PR # 17, TAKE OUT STATE TEST, THEY DO SOLICIT OUT OF STATE
      * DONT SOLICIT OUT OF STATE PEOPLE
      *    IF BR-STATE NOT = BW-STATE
      *       GO TO NEXT-LN.

           MOVE "N" TO FIRST-TIME.

           IF FORMNO NOT = 0
              PERFORM CREATE-FM-REC.

           GO TO NEXT-LN.

       END-OF-GROUP.
      * REMOVE LOCAL CLOSES, LET DECLARATIVES CLOSE FILES
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              MOVE "Y" TO RECORDS-FOUND.

           GO TO END-PROGRAM.

      *****************************************************
      *    I N I T I A L I Z A T I O N
      *****************************************************
       INITIALIZATION SECTION.
           MOVE 0 TO Q-POS C-POS P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.


       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.

           MOVE BEG-PODATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PODATE1.
           MOVE END-PODATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PODATE2.

           PERFORM GET-USER-BRANCH.

           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-FM1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO MANYACCTS-PATH.
           PERFORM OPEN-MANYACCTS-FILE-OUTPUT.
           PERFORM CLOSE-MANYACCTS-FILE.
           PERFORM OPEN-MANYACCTS-FILE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LOAN SOLICITATION RUN IN PROGRESS....." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.
           PERFORM ARRAYSP-LOAD.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************
      *    AUDIT TO SETUP DATES FOR BATCH PROCESS
      *********************************************
       EO-EXEC-AUDIT SECTION.
           MOVE BEG-PODATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PODATE1.
           MOVE BEG-PODATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PODATE2.

      *********************************************
      *    ENTER REPORT PARAMETERS
      *********************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ORIGIN.

           PERFORM GET-USER-BRANCH.

           MOVE "S  A010MANYACCTS." TO VDU.
           MOVE "N" TO MANY-ACCOUNTS VDUBUF.
           PERFORM SCREENIO.

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
       REC-MINPAYSM.
           MOVE "ENTER MINIMUM WHOLE PMTS MADE ON THE LOAN:"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MINPAYSM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
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

       REC-PODATE1.
           MOVE "ENTER BEGINNING PAID OUT DATE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MAXTERM.
           IF VDUSTATUS = "16" GO TO ALL-PODT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PODATE1.
           MOVE VDUNUM0 TO BEG-PODATE.
           MOVE VDU-DATE-YYYYMMDD TO PODATE1.
       REC-PODATE2.
           MOVE "ENTER ENDING PAID OUT DATE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PODATE1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-PODATE2.
           MOVE VDUNUM0 TO END-PODATE.
           MOVE VDU-DATE-YYYYMMDD TO PODATE2.

           IF PODATE2 < PODATE1
              GO TO REC-PODATE1.

           GO TO END-PODATE.

       ALL-PODT.
           MOVE "S  A080LNDT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO PODATE1 DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO BEG-PODATE VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A080LNDT2." TO VDU.
           MOVE EXT-JULIAN-END TO PODATE2 DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO END-PODATE VDU-DATE-EDIT.
           PERFORM SCREENIO.
       END-PODATE.

       REC-MINNXTPAY.
           MOVE "ENNN020MINNXTPAY." TO VDU.
           MOVE
           "ENTER MINIMUM DAYS UNTIL NEXT PAYMENT DUE, F3-ALL:"
                                                     TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-PODATE2.
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

       REC-FORM.
           MOVE "ENTER FORM TO BE SENT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINAV.
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
                    GO TO REC-FORM.

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


       ENTER-BEG-CRSCORE.
           MOVE "ENTER BEGINNING CREDIT SCORE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030SCORE1." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-SCORE.
           IF UP-KEY
              GO TO REC-ZCODE.

           IF NOT ENTER-KEY AND NOT DOWN-KEY
              GO TO ENTER-BEG-CRSCORE.

           MOVE VDUNUM0 TO BEG-CRSCORE.
           GO TO ENTER-END-CRSCORE.

       ALL-SCORE.
           MOVE "SNNN030SCORE1." TO VDU.
           MOVE 0 TO BEG-CRSCORE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030SCORE2." TO VDU.
           MOVE 999 TO END-CRSCORE VDUNUM0.
           PERFORM SCREENIO.
           GO TO ENTER-ORIGIN.

       ENTER-END-CRSCORE.
           MOVE "ENTER ENDING CREDIT SCORE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030SCORE2." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO ENTER-BEG-CRSCORE.
           IF NOT ENTER-KEY AND NOT DOWN-KEY
              GO TO ENTER-END-CRSCORE.

           MOVE VDUNUM0 TO END-CRSCORE.
           IF BEG-CRSCORE > END-CRSCORE
              GO TO ENTER-END-CRSCORE.

       ENTER-ORIGIN.
           MOVE "ENTER BORROWER ORIGIN ( ?-WILD ):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ORIGIN." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ORIGIN.
           IF UP-KEY GO TO ENTER-END-CRSCORE.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY AND NOT DOWN-KEY
             GO TO ENTER-ORIGIN.

           MOVE VDUBUF TO ORIGINX.
           GO TO CORP-FM.

       ALL-ORIGIN.
           MOVE "S  A040ORIGIN." TO VDU.
           MOVE "????" TO ORIGINX VDUBUF.
           PERFORM SCREENIO.

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
           IF VDUSTATUS = "1U" GO TO REC-FORM.
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

      ******************************
       VALIDATE-FORM SECTION.
      ******************************
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

      **************************************
      *   DISPLAY REPORT PARAMETERS
      **************************************
       DISPLAY-PARAMETERS SECTION.
       DISPLAY-PARM.
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE LOAN-FIELDS TO SCREEN-BUF.
           MOVE SCREEN-BUF TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A010CORPFM." TO VDU.
           MOVE CORP-FM-FG       TO VDUBUF.
           PERFORM SCREENIO.

      ********************************************
       CREATE-FM-REC SECTION.
      ********************************************
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
              PERFORM CHECK-FOR-MANY-ACCTS
              IF CREATE-FM-REQUEST = "N"
                 GO TO END-CREATE-FM-REC.

      * THESE MUST STAY HERE BELOW CHECK-FOR-MANY-ACCTS

           PERFORM LOAD-FM1-FILE.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE FORMNO        TO FM-FMNO.

           MOVE FM1-KEY       TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

      * PRINT FORMS IN LAST NAME ORDER
           IF ZIP-CODE = "A"
              MOVE BW-LNAME    TO EDIT-FM-LAST
              MOVE BW-FNAME    TO EDIT-FM-FIRST
              MOVE EDIT-FM-ZIP TO FM-ZIP
           ELSE
      * PRINT FORMS IN ZIP CODE ORDER
              MOVE BW-ZIP TO FM-ZIP.

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

       END-CREATE-FM-REC.
           EXIT.

      *********************************************
       CHECK-FOR-MANY-ACCTS SECTION.
      *********************************************
           MOVE "Y" TO CREATE-FM-REQUEST.

           IF MANY-ACCOUNTS = "Y"
              GO TO EXIT-CHECK-FOR-MANY-ACCTS.

           MOVE LN-SSNO(1)    TO MANYACCTS-SSNO.
           PERFORM READ-MANYACCTS-FILE.
           IF IO-BAD
              MOVE LN-POFFDATE  TO MANYACCTS-POFFDATE
              MOVE LN-OWNBR     TO MANYACCTS-OWNBR
              MOVE FM-INITKEY   TO MANYACCTS-INITKEY
              MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH
              MOVE BR-MACHINE   TO MANYACCTS-MACHINE
              PERFORM WRITE-MANYACCTS-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO EXIT-CHECK-FOR-MANY-ACCTS.

      *MULTIPLE ACCOUNTS - GET ACCOUNT WITH LATEST PAYOFF DATE
      *TO CREATE REQUEST
           IF LN-POFFDATE < MANYACCTS-POFFDATE
              MOVE "N" TO CREATE-FM-REQUEST
              GO TO EXIT-CHECK-FOR-MANY-ACCTS.

           MOVE FM-INITKEY TO SAVE-INITKEY.
           MOVE "Y" TO CREATE-FM-REQUEST.
           PERFORM REMOVE-PRIOR-REQUEST.
           MOVE SAVE-INITKEY TO FM-INITKEY.

       END-UPDATE-MANYACCTS.
           MOVE LN-SSNO(1)   TO MANYACCTS-SSNO.
           MOVE LN-POFFDATE  TO MANYACCTS-POFFDATE.
           MOVE FM-INITKEY   TO MANYACCTS-INITKEY.
           MOVE LN-OWNBR     TO MANYACCTS-OWNBR.
           MOVE BR-DATA-PATH TO MANYACCTS-DATA-PATH.
           MOVE BR-MACHINE   TO MANYACCTS-MACHINE.
           PERFORM REWRITE-MANYACCTS-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-CHECK-FOR-MANY-ACCTS.
           EXIT.


      *****************************************
       REMOVE-PRIOR-REQUEST SECTION.
      *****************************************

           MOVE MANYACCTS-OWNBR    TO FM-BRNO.
           MOVE MANYACCTS-INITKEY  TO FM-INITKEY.
           MOVE FORMNO             TO FM-FMNO.

           IF CORP-FM-FG = "N"
              MOVE FM-ALL-PATH TO SAVE-FM-ALL-PATH
              IF FM-ALL-DATA NOT = MANYACCTS-DATA-PATH
                 MOVE EXT-FILPATH-BASE    TO FM-ALL-BASE
                 MOVE MANYACCTS-MACHINE   TO FM-ALL-MACHINE
                 MOVE MANYACCTS-DATA-PATH TO FM-ALL-DATA
                 MOVE FM-ALL-PATH         TO FM-PATH
                 MOVE "Y"                 TO FM-PATH-OVERRIDE.

           PERFORM LOAD-FM1-FILE.
           PERFORM READ-FM1-FILE.
           IF IO-GOOD
              PERFORM DELETE-FM1-FILE.

           IF CORP-FM-FG = "N"
              MOVE SAVE-FM-ALL-PATH TO FM-ALL-PATH FM-PATH.

       END-REMOVE-PRIOR-REQUEST.

       EXIT-REMOVE-PRIOR-REQUEST.
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
       TEST-PROFILE SECTION.
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

       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/GPENV.CPY".
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

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO ASPAID-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ASPAID-SYSDATE.
           DISPLAY ASPAID-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ASPAID-FORM-FIELDS TO WR-BUF.
           MOVE ASPAID-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE ASPAID-HELP--FILE TO HELP-LINK-FILE.
           MOVE ASPAID-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ASPAID_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       MISC-ROUTINES SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
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
              MOVE SPACES TO BR-STATE
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
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF NOT IO-GOOD
                 GO TO IO-ERROR.
       GET-BR.
           IF BR-NO NOT = HOLD-BR-KEY
              PERFORM READ-BR-FILE
              MOVE BR-NO TO HOLD-BR-KEY.

      /***************************************
      *         I / O      R O U T I N E S
      ****************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       OPEN-MANYACCTS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE MANYACCTS-IFN TO E-FILE.
           OPEN OUTPUT MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO OPEN-MANYACCTS-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE MANYACCTS-FILE
              GO TO OPEN-MANYACCTS-FILE-OUTPUT.
           UNLOCK MANYACCTS-FILE.

       OPEN-MANYACCTS-FILE.
           PERFORM OPEN-IT.
           MOVE MANYACCTS-IFN TO E-FILE.
           OPEN I-O MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO OPEN-MANYACCTS-FILE.
           IF IO-FG = 7
              CLOSE MANYACCTS-FILE
              GO TO OPEN-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       READ-MANYACCTS-FILE.
           PERFORM READ-IT.
           MOVE MANYACCTS-IFN TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           READ MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO READ-MANYACCTS-FILE.

       WRITE-MANYACCTS-FILE.
           PERFORM WRITE-IT.
           MOVE MANYACCTS-IFN TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           WRITE MANYACCTS-REC.
           IF IO-FG = 8
              GO TO WRITE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.


       REWRITE-MANYACCTS-FILE.
           PERFORM REWRITE-IT.
           MOVE MANYACCTS-IFN TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           REWRITE MANYACCTS-REC.
           IF IO-FG = 8
              GO TO REWRITE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       DELETE-MANYACCTS-FILE.
           PERFORM DELETE-IT.
           MOVE MANYACCTS-IFN TO E-FILE.
           MOVE MANYACCTS-KEY TO E-KEYX.
           DELETE MANYACCTS-FILE.
           IF IO-FG = 8
              GO TO DELETE-MANYACCTS-FILE.
           UNLOCK MANYACCTS-FILE.

       CLOSE-MANYACCTS-FILE.
           PERFORM CLOSE-IT.
           CLOSE MANYACCTS-FILE.

       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".

      * COPY "LIBLP/LPFM1IN.CPY".
      * COPYMEMBER: LIBLP/LPFM1IN
       LOAD-FM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( FM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( FM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO FM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( FM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO FM-ALL-BASE
              MOVE FILPATH-MACHINE         TO FM-ALL-MACHINE
              MOVE FILPATH-DATA            TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH.

      *-----------------------------------------------------------------
       OPEN-FM1-FILE.
           PERFORM LOAD-FM1-FILE.
           PERFORM OPEN-IT.
           MOVE FM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-FM1-FILE.
           PERFORM READ-IT.
           MOVE FM-PATH-OWNBR  TO FM-BRNO. 
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           INITIALIZE QFM-REC.

           MOVE FM-BRNO    TO QFM-BRNO.
           MOVE FM-INITKEY TO QFM-INITKEY.
           MOVE FM-FMNO    TO QFM-FMNO.

           EXEC SQL
            SELECT FMFILE.FM_BRNO,
                   FMFILE.FM_INITKEY,
                   FMFILE.FM_FMNO,
                   FMFILE.FM_ZIP,
                   FMFILE.FM_PRINT,
                   FMFILE.FM_STMT_COUNTER,
                   CAST(FMFILE.FM_TRDATE AS VARCHAR(10)),
                   FMFILE.FM_USERID,
                   CAST(FMFILE.FM_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QFM-REC
            FROM DBO.FMFILE
            WHERE FMFILE.FM_BRNO = :QFM-BRNO
              AND FMFILE.FM_INITKEY = :QFM-INITKEY
              AND FMFILE.FM_FMNO = :QFM-FMNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-FM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-FM1-FILE.

           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           ACCEPT FM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           PERFORM SET-FM-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.FMFILE
             (FMFILE.FM_BRNO,
              FMFILE.FM_INITKEY,
              FMFILE.FM_FMNO,
              FMFILE.FM_ZIP,
              FMFILE.FM_PRINT,
              FMFILE.FM_STMT_COUNTER,
              FMFILE.FM_TRDATE,
              FMFILE.FM_USERID,
              FMFILE.FM_LTOUCH_DATE)
            VALUES
             (:QFM-BRNO,
              :QFM-INITKEY,
              :QFM-FMNO,
              :QFM-ZIP,
              :QFM-PRINT,
              :QFM-STMT-COUNTER,
              :QFM-TRDATE,
              :QFM-USERID,
              :QFM-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-FM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-FM1-FILE.

           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           ACCEPT FM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           PERFORM SET-FM-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.FMFILE
             SET
              FMFILE.FM_BRNO         = :QFM-BRNO,
              FMFILE.FM_INITKEY      = :QFM-INITKEY,
              FMFILE.FM_FMNO         = :QFM-FMNO,
              FMFILE.FM_ZIP          = :QFM-ZIP,
              FMFILE.FM_PRINT        = :QFM-PRINT,
              FMFILE.FM_STMT_COUNTER = :QFM-STMT-COUNTER,
              FMFILE.FM_TRDATE       = :QFM-TRDATE,
              FMFILE.FM_USERID       = :QFM-USERID,
              FMFILE.FM_LTOUCH_DATE  = :QFM-LTOUCH-DATE
            WHERE FMFILE.FM_BRNO    = :QFM-BRNO
              AND FMFILE.FM_INITKEY = :QFM-INITKEY
              AND FMFILE.FM_FMNO    = :QFM-FMNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-FM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-FM1-FILE.

           MOVE FM-PATH-OWNBR  TO FM-BRNO.
           PERFORM DELETE-IT.
           MOVE FM-PATH-SQL    TO E-FILE.
           MOVE FM1-KEY        TO E-KEYX.
           PERFORM SET-FM-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.FMFILE
             WHERE FMFILE.FM_BRNO    = :QFM-BRNO
               AND FMFILE.FM_INITKEY = :QFM-INITKEY
               AND FMFILE.FM_FMNO    = :QFM-FMNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-FM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-FM1-FILE.
           PERFORM CLOSE-IT.
      *-----------------------------------------------------------------
 
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

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

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.  *> SSP LN BW
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           MOVE LN-OWNBR       TO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_ASPAID_LNFILE_SELECT
                  (:QLN3-WBEG-OWNBR,
                   :PASS-MINPAYS-MADE, :PASS-MINPAYS-REMAIN,
                   :PASS-BEG-CRSCORE, :PASS-END-CRSCORE,
                   :PASS-POFFDATE1, :PASS-POFFDATE2)
                END-EXEC.

      *     SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_ORGBR,
      *            LNFILE.LN_OWNST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_CONVERCD,
      *            LNFILE.LN_MIDSTREAM,
      *            LNFILE.LN_PURCD,
      *            LNFILE.LN_SRCD,
      *            LNFILE.LN_COLLCD,
      *            LNFILE.LN_COLLID,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_PLCD,
      *            LNFILE.LN_PLTYPE,
      *            LNFILE.LN_CONFIDENTCD,
      *            LNFILE.LN_ALLOTCD,
      *            LNFILE.LN_RECASTCD,
      *            LNFILE.LN_SKIP_TRW,
      *            LNFILE.LN_STATUSFG,
      *            LNFILE.LN_SMALLOAN,
      *            LNFILE.LN_SALEFIN,
      *            LNFILE.LN_SERVACCT,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_ADDON_FG,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            LNFILE.LN_LC_CODE,
      *            LNFILE.LN_CAN_LOAN_NO,
      *            LNFILE.LN_STATFLAGS,
      *            CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_RESCISSION_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PART_RECOURSE_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PL_TRDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ACCELDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDUDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PO_TRDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LEGALDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_DISMISS_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DEBTAIDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REPODATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDEEMDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_SOLDDATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INS_CONFIRM_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_RN_FOR10_DATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_TRW_DOFD_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_PYPA_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BT_TRDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_METRO2_LAST_PMT_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_FOLLOWUP_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_ACCRUAL_DATE AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_EFFRATE,
      *            LNFILE.LN_SMPRATE,
      *            LNFILE.LN_YLDRATE,
      *            LNFILE.LN_OVRDRATE,
      *            LNFILE.LN_ENDORATE,
      *            LNFILE.LN_JDRATE,
      *            LNFILE.LN_ACCRATE,
      *            LNFILE.LN_REDURATE,
      *            LNFILE.LN_CONRATE,
      *            LNFILE.LN_SERVPER,
      *            LNFILE.LN_SELLPR,
      *            LNFILE.LN_DOWNPY,
      *            LNFILE.LN_TRADEIN,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_FINCHG,
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_POINTS,
      *            LNFILE.LN_PROCEEDS,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_PROCASH,
      *            LNFILE.LN_TILLNO,
      *            LNFILE.LN_STATE_FLAG,
      *            LNFILE.LN_STATE_WORKER,
      *            LNFILE.LN_STATE_WORKER2,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEOURS_1,
      *            LNFILE.LN_FEEFINCHG_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEOURS_2,
      *            LNFILE.LN_FEEFINCHG_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEOURS_3,
      *            LNFILE.LN_FEEFINCHG_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEOURS_4,
      *            LNFILE.LN_FEEFINCHG_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEOURS_5,
      *            LNFILE.LN_FEEFINCHG_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEOURS_6,
      *            LNFILE.LN_FEEFINCHG_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEOURS_7,
      *            LNFILE.LN_FEEFINCHG_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEOURS_8,
      *            LNFILE.LN_FEEFINCHG_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEOURS_9,
      *            LNFILE.LN_FEEFINCHG_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEOURS_10,
      *            LNFILE.LN_FEEFINCHG_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEOURS_11,
      *            LNFILE.LN_FEEFINCHG_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEOURS_12,
      *            LNFILE.LN_FEEFINCHG_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEOURS_13,
      *            LNFILE.LN_FEEFINCHG_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEOURS_14,
      *            LNFILE.LN_FEEFINCHG_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEOURS_15,
      *            LNFILE.LN_FEEFINCHG_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEOURS_16,
      *            LNFILE.LN_FEEFINCHG_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEOURS_17,
      *            LNFILE.LN_FEEFINCHG_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEOURS_18,
      *            LNFILE.LN_FEEFINCHG_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEOURS_19,
      *            LNFILE.LN_FEEFINCHG_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_FEEOURS_20,
      *            LNFILE.LN_FEEFINCHG_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSELIGIBLE,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSOURS_1,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOMM_1,
      *            LNFILE.LN_INSANTICERN_1,
      *            LNFILE.LN_INSANTICADJ_1,
      *            LNFILE.LN_INSUFERN_1,
      *            LNFILE.LN_INSACCERN_1,
      *            LNFILE.LN_INSEARN_ORDER_1,
      *            LNFILE.LN_INSFINCHG_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSOURS_2,
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOMM_2,
      *            LNFILE.LN_INSANTICERN_2,
      *            LNFILE.LN_INSANTICADJ_2,
      *            LNFILE.LN_INSUFERN_2,
      *            LNFILE.LN_INSACCERN_2,
      *            LNFILE.LN_INSEARN_ORDER_2,
      *            LNFILE.LN_INSFINCHG_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSOURS_3,
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOMM_3,
      *            LNFILE.LN_INSANTICERN_3,
      *            LNFILE.LN_INSANTICADJ_3,
      *            LNFILE.LN_INSUFERN_3,
      *            LNFILE.LN_INSACCERN_3,
      *            LNFILE.LN_INSEARN_ORDER_3,
      *            LNFILE.LN_INSFINCHG_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSOURS_4,
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOMM_4,
      *            LNFILE.LN_INSANTICERN_4,
      *            LNFILE.LN_INSANTICADJ_4,
      *            LNFILE.LN_INSUFERN_4,
      *            LNFILE.LN_INSACCERN_4,
      *            LNFILE.LN_INSEARN_ORDER_4,
      *            LNFILE.LN_INSFINCHG_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSOURS_5,
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOMM_5,
      *            LNFILE.LN_INSANTICERN_5,
      *            LNFILE.LN_INSANTICADJ_5,
      *            LNFILE.LN_INSUFERN_5,
      *            LNFILE.LN_INSACCERN_5,
      *            LNFILE.LN_INSEARN_ORDER_5,
      *            LNFILE.LN_INSFINCHG_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSOURS_6,
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOMM_6,
      *            LNFILE.LN_INSANTICERN_6,
      *            LNFILE.LN_INSANTICADJ_6,
      *            LNFILE.LN_INSUFERN_6,
      *            LNFILE.LN_INSACCERN_6,
      *            LNFILE.LN_INSEARN_ORDER_6,
      *            LNFILE.LN_INSFINCHG_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSOURS_7,
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOMM_7,
      *            LNFILE.LN_INSANTICERN_7,
      *            LNFILE.LN_INSANTICADJ_7,
      *            LNFILE.LN_INSUFERN_7,
      *            LNFILE.LN_INSACCERN_7,
      *            LNFILE.LN_INSEARN_ORDER_7,
      *            LNFILE.LN_INSFINCHG_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSOURS_8,
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOMM_8,
      *            LNFILE.LN_INSANTICERN_8,
      *            LNFILE.LN_INSANTICADJ_8,
      *            LNFILE.LN_INSUFERN_8,
      *            LNFILE.LN_INSACCERN_8,
      *            LNFILE.LN_INSEARN_ORDER_8,
      *            LNFILE.LN_INSFINCHG_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSOURS_9,
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOMM_9,
      *            LNFILE.LN_INSANTICERN_9,
      *            LNFILE.LN_INSANTICADJ_9,
      *            LNFILE.LN_INSUFERN_9,
      *            LNFILE.LN_INSACCERN_9,
      *            LNFILE.LN_INSEARN_ORDER_9,
      *            LNFILE.LN_INSFINCHG_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_INSCOMM_10,
      *            LNFILE.LN_INSANTICERN_10,
      *            LNFILE.LN_INSANTICADJ_10,
      *            LNFILE.LN_INSUFERN_10,
      *            LNFILE.LN_INSACCERN_10,
      *            LNFILE.LN_INSEARN_ORDER_10,
      *            LNFILE.LN_INSFINCHG_10,
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            LNFILE.LN_GARNISH_EXTR,
      *            LNFILE.LN_SHIP_SAME_FG,
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_TOT_PARTIAL_CHGOFF,
      *            LNFILE.LN_LPO,
      *            LNFILE.LN_REP_CD,
      *            LNFILE.LN_EMP_ID,
      *            LNFILE.LN_PROCESS_CD,
      *            LNFILE.LN_WCBC_CREDITLIM,
      *            LNFILE.LN_FOLLOWUP_CD,
      *            LNFILE.LN_MIP_FG,
      *            LNFILE.LN_MIP_MTD_CLAHPAID,
      *            CAST(LNFILE.LN_MIP_CLPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_MIP_TOT_CLPAID,
      *            CAST(LNFILE.LN_MIP_AHPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_MIP_TOT_AHPAID,
      *            LNFILE.LN_IL_GROSS_INCOME,
      *            LNFILE.LN_IL_GROSS_INCOME2,
      *            LNFILE.LN_RESCISSION,
      *            LNFILE.LN_INTERIM_TRW,
      *            LNFILE.LN_DLNO,
      *            LNFILE.LN_DLPOOL,
      *            LNFILE.LN_DLRECOURSE,
      *            LNFILE.LN_DLPARTRCAMT,
      *            LNFILE.LN_ORIGPURAMT,
      *            LNFILE.LN_PURBAL,
      *            LNFILE.LN_DLRVPRIN_1,
      *            LNFILE.LN_DLRVPRIN_2,
      *            LNFILE.LN_DLRVPRIN_3,
      *            LNFILE.LN_DLRVCHGS_1,
      *            LNFILE.LN_DLRVCHGS_2,
      *            LNFILE.LN_DLRVCHGS_3,
      *            LNFILE.LN_DLPARVHELD,
      *            LNFILE.LN_DLPARVPAID,
      *            LNFILE.LN_DLDISC,
      *            CAST(LNFILE.LN_LCPDTH_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_LCPAID,
      *            LNFILE.LN_LCPARTIALS,
      *            LNFILE.LN_STMT_COUNTER,
      *            LNFILE.LN_PRIOR_STMT_COUNTER,
      *            LNFILE.LN_FROZEN_REASON,
      *            LNFILE.LN_COUPON_STATEMENT,
      *            LNFILE.LN_MONTHEND_BAL,
      *            LNFILE.LN_ANTICTERM,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICADJ_1,
      *            LNFILE.LN_UFERN_1,
      *            LNFILE.LN_ACCERN_1,
      *            LNFILE.LN_EARN_ORDER_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICADJ_2,
      *            LNFILE.LN_UFERN_2,
      *            LNFILE.LN_ACCERN_2,
      *            LNFILE.LN_EARN_ORDER_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICADJ_3,
      *            LNFILE.LN_UFERN_3,
      *            LNFILE.LN_ACCERN_3,
      *            LNFILE.LN_EARN_ORDER_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICADJ_4,
      *            LNFILE.LN_UFERN_4,
      *            LNFILE.LN_ACCERN_4,
      *            LNFILE.LN_EARN_ORDER_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICADJ_5,
      *            LNFILE.LN_UFERN_5,
      *            LNFILE.LN_ACCERN_5,
      *            LNFILE.LN_EARN_ORDER_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICADJ_6,
      *            LNFILE.LN_UFERN_6,
      *            LNFILE.LN_ACCERN_6,
      *            LNFILE.LN_EARN_ORDER_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICADJ_7,
      *            LNFILE.LN_UFERN_7,
      *            LNFILE.LN_ACCERN_7,
      *            LNFILE.LN_EARN_ORDER_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICADJ_8,
      *            LNFILE.LN_UFERN_8,
      *            LNFILE.LN_ACCERN_8,
      *            LNFILE.LN_EARN_ORDER_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICADJ_9,
      *            LNFILE.LN_UFERN_9,
      *            LNFILE.LN_ACCERN_9,
      *            LNFILE.LN_EARN_ORDER_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_ANTICADJ_10,
      *            LNFILE.LN_UFERN_10,
      *            LNFILE.LN_ACCERN_10,
      *            LNFILE.LN_EARN_ORDER_10,
      *            LNFILE.LN_LMO_PRIOR_INTEREST,
      *            LNFILE.LN_PRIOR_UNEARNED,
      *            LNFILE.LN_ERNDELQ_STOP,
      *            LNFILE.LN_NONACRERN,
      *            LNFILE.LN_PRIOR_NONACRERN,
      *            CAST(LNFILE.LN_ERN_IB_PDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_ERN_IB_PRINBAL,
      *            LNFILE.LN_ERN_IB_INTOWE,
      *            LNFILE.LN_TOTERN,
      *            LNFILE.LN_TOTOTH,
      *            LNFILE.LN_TOTINT,
      *            LNFILE.LN_TOTLCHG,
      *            LNFILE.LN_TOTDEF,
      *            LNFILE.LN_TOTSERACCT,
      *            LNFILE.LN_LCWAIVERS,
      *            LNFILE.LN_INTWAIVERS,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTEXCPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_YTDNODEF,
      *            LNFILE.LN_TOTNODEFACT,
      *            LNFILE.LN_YTDNODEFACT,
      *            LNFILE.LN_TOTNOALDEL,
      *            LNFILE.LN_YTDNOALDEL,
      *            LNFILE.LN_YTDINTPD,
      *            LNFILE.LN_LYRINTPD,
      *            LNFILE.LN_TOTINTPD,
      *            LNFILE.LN_RECASTINT,
      *            LNFILE.LN_RECASTPRIN,
      *            LNFILE.LN_CL_REBATE,
      *            LNFILE.LN_AH_REBATE,
      *            LNFILE.LN_PP_REBATE,
      *            LNFILE.LN_INT_REBATE,
      *            LNFILE.LN_SC_REBATE,
      *            LNFILE.LN_MF_REBATE,
      *            LNFILE.LN_DF_REBATE,
      *            LNFILE.LN_EX_REBATE,
      *            LNFILE.LN_O1_REBATE,
      *            LNFILE.LN_O2_REBATE,
      *            LNFILE.LN_O3_REBATE,
      *            LNFILE.LN_O4_REBATE,
      *            LNFILE.LN_O5_REBATE,
      *            LNFILE.LN_O6_REBATE,
      *            LNFILE.LN_O7_REBATE,
      *            LNFILE.LN_UN_O7_REB,
      *            LNFILE.LN_UN_O8_REB,
      *            LNFILE.LN_UN_O9_REB,
      *            LNFILE.LN_UN_O10_REB,
      *            LNFILE.LN_PLREASON,
      *            LNFILE.LN_COLLECTOR,
      *            LNFILE.LN_ACTIONCD,
      *            LNFILE.LN_ACTIONCD2,
      *            LNFILE.LN_ATCODE,
      *            LNFILE.LN_RECODE,
      *            LNFILE.LN_ESCROW_FG,
      *            LNFILE.LN_PROMDAYS_PASTDUE,
      *            LNFILE.LN_PROMID,
      *            CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),
      *            LNFILE.LN_PROMAMT,
      *            CAST(LNFILE.LN_PROMDATE2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *            LNFILE.LN_AMT_PAID_LAST,
      *            LNFILE.LN_LASTCFORMNO,
      *            CAST(LNFILE.LN_LASTCFORMDATE AS VARCHAR(10)),
      *            LNFILE.LN_DELCNT,
      *            LNFILE.LN_DELCNT2,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_CRSCORE,
      *            LNFILE.LN_LASTSFORMNO,
      *            CAST(LNFILE.LN_LASTSFORMDATE AS VARCHAR(10)),
      *            LNFILE.LN_SOLCNT,
      *            LNFILE.LN_MESSAGE,
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_PRBAL,
      *            LNFILE.LN_PRINTDUE,
      *            LNFILE.LN_PRFINCHGRB,
      *            LNFILE.LN_PRINSRB,
      *            LNFILE.LN_PRPOCD,
      *            CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),
      *            LNFILE.LN_PRDELINQAMT,
      *            LNFILE.LN_PRLNAMT,
      *            LNFILE.LN_PRPDUE_DAYS,
      *            LNFILE.LN_PRDLDISC_UNEARN,
      *            LNFILE.LN_PRCONPROFILE,
      *            LNFILE.LN_SUM_ORIGPURAMT,
      *            LNFILE.LN_SUM_LNAMT,
      *            LNFILE.LN_SUM_FINCHG,
      *            LNFILE.LN_SUM_INTCHG,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_SUM_REGPYAMT,
      *            LNFILE.LN_SUM_EXTCHG,
      *            LNFILE.LN_SUM_SERCHG,
      *            LNFILE.LN_SUM_MAINTFEE,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_DAYS_IN_UNITPER,
      *            LNFILE.LN_UNITPER_PER_YEAR,
      *            LNFILE.LN_REPOCD,
      *            LNFILE.LN_BR_TFR_FG,
      *            LNFILE.LN_O3INS_DLPROC,
      *            LNFILE.LN_INSPREM_DLADJ,
      *            LNFILE.LN_PLAMT,
      *            LNFILE.LN_MTD_PRIN,
      *            LNFILE.LN_MTD_LC,
      *            LNFILE.LN_MTD_INT,
      *            LNFILE.LN_MTD_OT,
      *            LNFILE.LN_MTD_EX,
      *            LNFILE.LN_BNKRPT_CHAPTER,
      *            LNFILE.LN_BMO_CON_STATUS,
      *            LNFILE.LN_BMO_REC_STATUS,
      *            LNFILE.LN_APP_NUMBER,
      *            LNFILE.LN_GIFTNET,
      *            LNFILE.LN_GIFTNET_OUR_PO,
      *            LNFILE.LN_PROC_DISBURSED,
      *            LNFILE.LN_NOODDPY,
      *            LNFILE.LN_CONPROFILE_1,
      *            LNFILE.LN_CONPROFILE_2,
      *            LNFILE.LN_CONPROFILE_3,
      *            LNFILE.LN_CONPROFILE_4,
      *            LNFILE.LN_CONPROFILE_5,
      *            LNFILE.LN_RECPROFILE_1,
      *            LNFILE.LN_RECPROFILE_2,
      *            LNFILE.LN_RECPROFILE_3,
      *            LNFILE.LN_RECPROFILE_4,
      *            LNFILE.LN_RECPROFILE_5,
      *            LNFILE.LN_TRW_STATUS,
      *            LNFILE.LN_ORIGIN,
      *            LNFILE.LN_PO_TRCD,
      *            LNFILE.LN_NEXIS_CRSCORE,
      *            LNFILE.LN_TRW_B_CONSUMER_IND,
      *            LNFILE.LN_PARVHELD_PAID_FG,
      *            LNFILE.LN_ACCT_FROZEN,
      *            LNFILE.LN_TRW_SPECIAL_CODE,
      *            LNFILE.LN_CREATION_STATUS,
      *            LNFILE.LN_TOTNOLCHG,
      *            LNFILE.LN_LBOX,
      *            LNFILE.LN_EXTRA_PRIN_LFP,
      *            LNFILE.LN_REMAIN_INT_SINCE_LFP,
      *            LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
      *            LNFILE.LN_INTERIM_COUPON,
      *            LNFILE.LN_DLCOMM_O3_CMSI,
      *            LNFILE.LN_DLCOMM_PP_CMSI,
      *            LNFILE.LN_TRW_J_CONSUMER_IND,
      *            LNFILE.LN_ACH,
      *            LNFILE.LN_1099_C_FG,
      *            LNFILE.LN_TRW_COMPLIANCE_CODE,
      *            LNFILE.LN_UNWIND_REASON_CODE,
      *            LNFILE.LN_PBI_PBD_FG,
      *            LNFILE.LN_OLD_ACCT_FORMAT_FG,
      *            LNFILE.LN_APPROVED_BY_USER,
      *            LNFILE.LN_APPROVED_BY_CODE,
      *            CAST(LNFILE.LN_APPROVED_BY_DATE AS VARCHAR(10)),
      *            LNFILE.LN_APPROVED_BY_TIME,
      *            CAST(LNFILE.LN_LTOUCH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_LTOUCH_TIME, 
      *            BWFILE.BW_OWNBR,
      *            BWFILE.BW_SSNO,
      *            BWFILE.BW_LNAME,
      *            BWFILE.BW_FNAME,
      *            BWFILE.BW_STATE,
      *            BWFILE.BW_ZIP,
      *            BWFILE.BW_ORIGIN,
      *            BWFILE.BW_LIASMLN,
      *            BWFILE.BW_LIAOTH,
      *            CAST(BWFILE.BW_LAST_RN_FOR10_DATE AS VARCHAR(10)),
      *            BWFILE.BW_SOLICIT,
      *            BWFILE.BW_CONFIDENTIAL
      *      FROM DBO.LNFILE LNFILE
      *      INNER JOIN DBO.BWFILE BWFILE
      *       ON BWFILE.BW_OWNBR  = LNFILE.LN_OWNBR 
      *       AND BWFILE.BW_SSNO   = LNFILE.LN_SSNO1
      *       WHERE LNFILE.LN_OWNBR = @QLN3_WBEG_BRNO
      *        AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *        AND LNFILE.LN_POCD  NOT IN ('**', 'BT')
      *        AND LNFILE.LN_PO_TRCD = 'PO'
      *        AND LNFILE.LN_PLDATE = '1900-01-01' 
      *        AND LNFILE.LN_JDDATE = '1900-01-01'
      *        AND LNFILE.LN_CONFIDENTCD != 'Y'
      *        AND LNFILE.LN_CURBAL = 0
      *        AND LNFILE.LN_POFFDATE  != '1900-01-01'
      *        AND (LNFILE.LN_REPODATE  = '1900-01-01' AND LNFILE.LN_DEBTAIDATE = '1900-01-01' AND 
      *             LNFILE.LN_LEGALDATE = '1900-01-01' AND LNFILE.LN_BNKRPTDATE = '1900-01-01')
      *        AND (LNFILE.LN_REGPYAMT = 0 OR
      *            ( (LNFILE.LN_TOTPAYMNTD / LNFILE.LN_REGPYAMT) >= @MINPAYS_MADE) ) 
      *        AND (LNFILE.LN_REGPYAMT = 0 OR
      *            ( (LNFILE.LN_ORGTERM * LNFILE.LN_REGPYAMT - LNFILE.LN_TOTPAYMNTD) / LNFILE.LN_REGPYAMT) >= @MINPAYS_REMAIN)
      *        AND  BWFILE.BW_STATE != 'KS' 
      *        AND (BWFILE.BW_LIASMLN = 0 OR BWFILE.BW_LIAOTH = 0) 
      *        AND (BWFILE.BW_SOLICIT != 'N' OR BWFILE.BW_CONFIDENTIAL != 'Y')
      *    ORDER BY LNFILE.LN_CLASS, LNFILE.LN_ACCTNO 

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *> EMBEDDED LN BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
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
                   :QLN-ORGBR,
                   :QLN-OWNST,
                   :QLN-SPRCLASS,
                   :QLN-CONVERCD,
                   :QLN-MIDSTREAM,
                   :QLN-PURCD,
                   :QLN-SRCD,
                   :QLN-COLLCD,
                   :QLN-COLLID,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PLTYPE,
                   :QLN-CONFIDENTCD,
                   :QLN-ALLOTCD,
                   :QLN-RECASTCD,
                   :QLN-SKIP-TRW,
                   :QLN-STATUSFG,
                   :QLN-SMALLOAN,
                   :QLN-SALEFIN,
                   :QLN-SERVACCT,
                   :QLN-LOANTYPE,
                   :QLN-ADDON-FG,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LC-CODE,
                   :QLN-CAN-LOAN-NO,
                   :QLN-STATFLAGS,
                   :QLN-ENTDATE ,
                   :QLN-LOANDATE ,
                   :QLN-PURDATE ,
                   :QLN-INTDATE ,
                   :QLN-CASHDATE ,
                   :QLN-FILEDATE ,
                   :QLN-RESCISSION-DATE ,
                   :QLN-PART-RECOURSE-DATE ,
                   :QLN-ORIG-1STPYDATE ,
                   :QLN-1STPYDATE ,
                   :QLN-JDDATE ,
                   :QLN-PLDATE ,
                   :QLN-PL-TRDATE ,
                   :QLN-ACCELDATE ,
                   :QLN-REDUDATE  ,
                   :QLN-POFFDATE  ,
                   :QLN-PO-TRDATE ,
                   :QLN-LEGALDATE ,
                   :QLN-BNKRPTDATE ,
                   :QLN-BNKRPT-DISMISS-DATE ,
                   :QLN-BNKRPT-DISCHARGE-DATE ,
                   :QLN-BNKRPT-REAFFIRM-DATE ,
                   :QLN-DEBTAIDATE ,
                   :QLN-REPODATE   ,
                   :QLN-REDEEMDATE ,
                   :QLN-SOLDDATE   ,
                   :QLN-INS-CONFIRM-DATE ,
                   :QLN-PRIOR-RN-FOR10-DATE ,
                   :QLN-LAST-RN-FOR10-DATE  ,
                   :QLN-TRW-DOFD-DATE ,
                   :QLN-LAST-PYPA-DATE ,
                   :QLN-BT-TRDATE ,
                   :QLN-METRO2-LAST-PMT-DATE ,
                   :QLN-FOLLOWUP-DATE ,
                   :QLN-LAST-ACCRUAL-DATE ,
                   :QLN-APRATE,
                   :QLN-EFFRATE,
                   :QLN-SMPRATE,
                   :QLN-YLDRATE,
                   :QLN-OVRDRATE,
                   :QLN-ENDORATE,
                   :QLN-JDRATE,
                   :QLN-ACCRATE,
                   :QLN-REDURATE,
                   :QLN-CONRATE,
                   :QLN-SERVPER,
                   :QLN-SELLPR,
                   :QLN-DOWNPY,
                   :QLN-TRADEIN,
                   :QLN-LNAMT,
                   :QLN-FINCHG,
                   :QLN-INTCHG,
                   :QLN-SERCHG,
                   :QLN-EXTCHG,
                   :QLN-MAINTFEE,
                   :QLN-POINTS,
                   :QLN-PROCEEDS,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PROCASH,
                   :QLN-TILLNO,
                   :QLN-STATE-FLAG,
                   :QLN-STATE-WORKER,
                   :QLN-STATE-WORKER2,
                   :QLN-FEECODE-1,
                   :QLN-FEEAMT-1,
                   :QLN-FEEOURS-1,
                   :QLN-FEEFINCHG-1,
                   :QLN-FEECODE-2,
                   :QLN-FEEAMT-2,
                   :QLN-FEEOURS-2,
                   :QLN-FEEFINCHG-2,
                   :QLN-FEECODE-3,
                   :QLN-FEEAMT-3,
                   :QLN-FEEOURS-3,
                   :QLN-FEEFINCHG-3,
                   :QLN-FEECODE-4,
                   :QLN-FEEAMT-4,
                   :QLN-FEEOURS-4,
                   :QLN-FEEFINCHG-4,
                   :QLN-FEECODE-5,
                   :QLN-FEEAMT-5,
                   :QLN-FEEOURS-5,
                   :QLN-FEEFINCHG-5,
                   :QLN-FEECODE-6,
                   :QLN-FEEAMT-6,
                   :QLN-FEEOURS-6,
                   :QLN-FEEFINCHG-6,
                   :QLN-FEECODE-7,
                   :QLN-FEEAMT-7,
                   :QLN-FEEOURS-7,
                   :QLN-FEEFINCHG-7,
                   :QLN-FEECODE-8,
                   :QLN-FEEAMT-8,
                   :QLN-FEEOURS-8,
                   :QLN-FEEFINCHG-8,
                   :QLN-FEECODE-9,
                   :QLN-FEEAMT-9,
                   :QLN-FEEOURS-9,
                   :QLN-FEEFINCHG-9,
                   :QLN-FEECODE-10,
                   :QLN-FEEAMT-10,
                   :QLN-FEEOURS-10,
                   :QLN-FEEFINCHG-10,
                   :QLN-FEECODE-11,
                   :QLN-FEEAMT-11,
                   :QLN-FEEOURS-11,
                   :QLN-FEEFINCHG-11,
                   :QLN-FEECODE-12,
                   :QLN-FEEAMT-12,
                   :QLN-FEEOURS-12,
                   :QLN-FEEFINCHG-12,
                   :QLN-FEECODE-13,
                   :QLN-FEEAMT-13,
                   :QLN-FEEOURS-13,
                   :QLN-FEEFINCHG-13,
                   :QLN-FEECODE-14,
                   :QLN-FEEAMT-14,
                   :QLN-FEEOURS-14,
                   :QLN-FEEFINCHG-14,
                   :QLN-FEECODE-15,
                   :QLN-FEEAMT-15,
                   :QLN-FEEOURS-15,
                   :QLN-FEEFINCHG-15,
                   :QLN-FEECODE-16,
                   :QLN-FEEAMT-16,
                   :QLN-FEEOURS-16,
                   :QLN-FEEFINCHG-16,
                   :QLN-FEECODE-17,
                   :QLN-FEEAMT-17,
                   :QLN-FEEOURS-17,
                   :QLN-FEEFINCHG-17,
                   :QLN-FEECODE-18,
                   :QLN-FEEAMT-18,
                   :QLN-FEEOURS-18,
                   :QLN-FEEFINCHG-18,
                   :QLN-FEECODE-19,
                   :QLN-FEEAMT-19,
                   :QLN-FEEOURS-19,
                   :QLN-FEEFINCHG-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-20,
                   :QLN-FEEOURS-20,
                   :QLN-FEEFINCHG-20,
                   :QLN-INSURED,
                   :QLN-INSTYPES,
                   :QLN-INSELIGIBLE,
                   :QLN-INSCOMP-1,
                   :QLN-INSOURS-1,
                   :QLN-INSEFF-DATE-1 ,
                   :QLN-INSEXP-DATE-1 ,
                   :QLN-INSPREM-1,
                   :QLN-INSCOVR-1,
                   :QLN-INSCOMM-1,
                   :QLN-INSANTICERN-1,
                   :QLN-INSANTICADJ-1,
                   :QLN-INSUFERN-1,
                   :QLN-INSACCERN-1,
                   :QLN-INSEARN-ORDER-1,
                   :QLN-INSFINCHG-1,
                   :QLN-INSCOMP-2,
                   :QLN-INSOURS-2,
                   :QLN-INSEFF-DATE-2 ,
                   :QLN-INSEXP-DATE-2 ,
                   :QLN-INSPREM-2,
                   :QLN-INSCOVR-2,
                   :QLN-INSCOMM-2,
                   :QLN-INSANTICERN-2,
                   :QLN-INSANTICADJ-2,
                   :QLN-INSUFERN-2,
                   :QLN-INSACCERN-2,
                   :QLN-INSEARN-ORDER-2,
                   :QLN-INSFINCHG-2,
                   :QLN-INSCOMP-3,
                   :QLN-INSOURS-3,
                   :QLN-INSEFF-DATE-3 ,
                   :QLN-INSEXP-DATE-3 ,
                   :QLN-INSPREM-3,
                   :QLN-INSCOVR-3,
                   :QLN-INSCOMM-3,
                   :QLN-INSANTICERN-3,
                   :QLN-INSANTICADJ-3,
                   :QLN-INSUFERN-3,
                   :QLN-INSACCERN-3,
                   :QLN-INSEARN-ORDER-3,
                   :QLN-INSFINCHG-3,
                   :QLN-INSCOMP-4,
                   :QLN-INSOURS-4,
                   :QLN-INSEFF-DATE-4 ,
                   :QLN-INSEXP-DATE-4 ,
                   :QLN-INSPREM-4,
                   :QLN-INSCOVR-4,
                   :QLN-INSCOMM-4,
                   :QLN-INSANTICERN-4,
                   :QLN-INSANTICADJ-4,
                   :QLN-INSUFERN-4,
                   :QLN-INSACCERN-4,
                   :QLN-INSEARN-ORDER-4,
                   :QLN-INSFINCHG-4,
                   :QLN-INSCOMP-5,
                   :QLN-INSOURS-5,
                   :QLN-INSEFF-DATE-5 ,
                   :QLN-INSEXP-DATE-5 ,
                   :QLN-INSPREM-5,
                   :QLN-INSCOVR-5,
                   :QLN-INSCOMM-5,
                   :QLN-INSANTICERN-5,
                   :QLN-INSANTICADJ-5,
                   :QLN-INSUFERN-5,
                   :QLN-INSACCERN-5,
                   :QLN-INSEARN-ORDER-5,
                   :QLN-INSFINCHG-5,
                   :QLN-INSCOMP-6,
                   :QLN-INSOURS-6,
                   :QLN-INSEFF-DATE-6 ,
                   :QLN-INSEXP-DATE-6 ,
                   :QLN-INSPREM-6,
                   :QLN-INSCOVR-6,
                   :QLN-INSCOMM-6,
                   :QLN-INSANTICERN-6,
                   :QLN-INSANTICADJ-6,
                   :QLN-INSUFERN-6,
                   :QLN-INSACCERN-6,
                   :QLN-INSEARN-ORDER-6,
                   :QLN-INSFINCHG-6,
                   :QLN-INSCOMP-7,
                   :QLN-INSOURS-7,
                   :QLN-INSEFF-DATE-7 ,
                   :QLN-INSEXP-DATE-7 ,
                   :QLN-INSPREM-7,
                   :QLN-INSCOVR-7,
                   :QLN-INSCOMM-7,
                   :QLN-INSANTICERN-7,
                   :QLN-INSANTICADJ-7,
                   :QLN-INSUFERN-7,
                   :QLN-INSACCERN-7,
                   :QLN-INSEARN-ORDER-7,
                   :QLN-INSFINCHG-7,
                   :QLN-INSCOMP-8,
                   :QLN-INSOURS-8,
                   :QLN-INSEFF-DATE-8 ,
                   :QLN-INSEXP-DATE-8 ,
                   :QLN-INSPREM-8,
                   :QLN-INSCOVR-8,
                   :QLN-INSCOMM-8,
                   :QLN-INSANTICERN-8,
                   :QLN-INSANTICADJ-8,
                   :QLN-INSUFERN-8,
                   :QLN-INSACCERN-8,
                   :QLN-INSEARN-ORDER-8,
                   :QLN-INSFINCHG-8,
                   :QLN-INSCOMP-9,
                   :QLN-INSOURS-9,
                   :QLN-INSEFF-DATE-9 ,
                   :QLN-INSEXP-DATE-9 ,
                   :QLN-INSPREM-9,
                   :QLN-INSCOVR-9,
                   :QLN-INSCOMM-9,
                   :QLN-INSANTICERN-9,
                   :QLN-INSANTICADJ-9,
                   :QLN-INSUFERN-9,
                   :QLN-INSACCERN-9,
                   :QLN-INSEARN-ORDER-9,
                   :QLN-INSFINCHG-9,
                   :QLN-INSCOMP-10,
                   :QLN-INSOURS-10,
                   :QLN-INSEFF-DATE-10 ,
                   :QLN-INSEXP-DATE-10 ,
                   :QLN-INSPREM-10,
                   :QLN-INSCOVR-10,
                   :QLN-INSCOMM-10,
                   :QLN-INSANTICERN-10,
                   :QLN-INSANTICADJ-10,
                   :QLN-INSUFERN-10,
                   :QLN-INSACCERN-10,
                   :QLN-INSEARN-ORDER-10,
                   :QLN-INSFINCHG-10,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-GARNISH-EXTR,
                   :QLN-SHIP-SAME-FG,
                   :QLN-AD-D-INS-COVR,
                   :QLN-TOT-PARTIAL-CHGOFF,
                   :QLN-LPO,
                   :QLN-REP-CD,
                   :QLN-EMP-ID,
                   :QLN-PROCESS-CD,
                   :QLN-WCBC-CREDITLIM,
                   :QLN-FOLLOWUP-CD,
                   :QLN-MIP-FG,
                   :QLN-MIP-MTD-CLAHPAID,
                   :QLN-MIP-CLPDTH-DATE ,
                   :QLN-MIP-TOT-CLPAID,
                   :QLN-MIP-AHPDTH-DATE ,
                   :QLN-MIP-TOT-AHPAID,
                   :QLN-IL-GROSS-INCOME,
                   :QLN-IL-GROSS-INCOME2,
                   :QLN-RESCISSION,
                   :QLN-INTERIM-TRW,
                   :QLN-DLNO,
                   :QLN-DLPOOL,
                   :QLN-DLRECOURSE,
                   :QLN-DLPARTRCAMT,
                   :QLN-ORIGPURAMT,
                   :QLN-PURBAL,
                   :QLN-DLRVPRIN-1,
                   :QLN-DLRVPRIN-2,
                   :QLN-DLRVPRIN-3,
                   :QLN-DLRVCHGS-1,
                   :QLN-DLRVCHGS-2,
                   :QLN-DLRVCHGS-3,
                   :QLN-DLPARVHELD,
                   :QLN-DLPARVPAID,
                   :QLN-DLDISC,
                   :QLN-LCPDTH-DATE ,
                   :QLN-INTPDTH-DATE ,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-LCPAID,
                   :QLN-LCPARTIALS,
                   :QLN-STMT-COUNTER,
                   :QLN-PRIOR-STMT-COUNTER,
                   :QLN-FROZEN-REASON,
                   :QLN-COUPON-STATEMENT,
                   :QLN-MONTHEND-BAL,
                   :QLN-ANTICTERM,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICADJ-1,
                   :QLN-UFERN-1,
                   :QLN-ACCERN-1,
                   :QLN-EARN-ORDER-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICADJ-2,
                   :QLN-UFERN-2,
                   :QLN-ACCERN-2,
                   :QLN-EARN-ORDER-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICADJ-3,
                   :QLN-UFERN-3,
                   :QLN-ACCERN-3,
                   :QLN-EARN-ORDER-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICADJ-4,
                   :QLN-UFERN-4,
                   :QLN-ACCERN-4,
                   :QLN-EARN-ORDER-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICADJ-5,
                   :QLN-UFERN-5,
                   :QLN-ACCERN-5,
                   :QLN-EARN-ORDER-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICADJ-6,
                   :QLN-UFERN-6,
                   :QLN-ACCERN-6,
                   :QLN-EARN-ORDER-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICADJ-7,
                   :QLN-UFERN-7,
                   :QLN-ACCERN-7,
                   :QLN-EARN-ORDER-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICADJ-8,
                   :QLN-UFERN-8,
                   :QLN-ACCERN-8,
                   :QLN-EARN-ORDER-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICADJ-9,
                   :QLN-UFERN-9,
                   :QLN-ACCERN-9,
                   :QLN-EARN-ORDER-9,
                   :QLN-ANTICERN-10,
                   :QLN-ANTICADJ-10,
                   :QLN-UFERN-10,
                   :QLN-ACCERN-10,
                   :QLN-EARN-ORDER-10,
                   :QLN-LMO-PRIOR-INTEREST,
                   :QLN-PRIOR-UNEARNED,
                   :QLN-ERNDELQ-STOP,
                   :QLN-NONACRERN,
                   :QLN-PRIOR-NONACRERN,
                   :QLN-ERN-IB-PDTH-DATE ,
                   :QLN-ERN-IB-PRINBAL,
                   :QLN-ERN-IB-INTOWE,
                   :QLN-TOTERN,
                   :QLN-TOTOTH,
                   :QLN-TOTINT,
                   :QLN-TOTLCHG,
                   :QLN-TOTDEF,
                   :QLN-TOTSERACCT,
                   :QLN-LCWAIVERS,
                   :QLN-INTWAIVERS,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTEXCPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-YTDNODEF,
                   :QLN-TOTNODEFACT,
                   :QLN-YTDNODEFACT,
                   :QLN-TOTNOALDEL,
                   :QLN-YTDNOALDEL,
                   :QLN-YTDINTPD,
                   :QLN-LYRINTPD,
                   :QLN-TOTINTPD,
                   :QLN-RECASTINT,
                   :QLN-RECASTPRIN,
                   :QLN-CL-REBATE,
                   :QLN-AH-REBATE,
                   :QLN-PP-REBATE,
                   :QLN-INT-REBATE,
                   :QLN-SC-REBATE,
                   :QLN-MF-REBATE,
                   :QLN-DF-REBATE,
                   :QLN-EX-REBATE,
                   :QLN-O1-REBATE,
                   :QLN-O2-REBATE,
                   :QLN-O3-REBATE,
                   :QLN-O4-REBATE,
                   :QLN-O5-REBATE,
                   :QLN-O6-REBATE,
                   :QLN-O7-REBATE,
                   :QLN-UN-O7-REB,
                   :QLN-UN-O8-REB,
                   :QLN-UN-O9-REB,
                   :QLN-UN-O10-REB,
                   :QLN-PLREASON,
                   :QLN-COLLECTOR,
                   :QLN-ACTIONCD,
                   :QLN-ACTIONCD2,
                   :QLN-ATCODE,
                   :QLN-RECODE,
                   :QLN-ESCROW-FG,
                   :QLN-PROMDAYS-PASTDUE,
                   :QLN-PROMID,
                   :QLN-PROMDATE ,
                   :QLN-PROMAMT,
                   :QLN-PROMDATE2 ,
                   :QLN-DATE-PAID-LAST ,
                   :QLN-AMT-PAID-LAST,
                   :QLN-LASTCFORMNO,
                   :QLN-LASTCFORMDATE ,
                   :QLN-DELCNT,
                   :QLN-DELCNT2,
                   :QLN-CREDITLIM,
                   :QLN-CRSCORE,
                   :QLN-LASTSFORMNO,
                   :QLN-LASTSFORMDATE ,
                   :QLN-SOLCNT,
                   :QLN-MESSAGE,
                   :QLN-OLDACCTNO,
                   :QLN-PRLNNO,
                   :QLN-PRBAL,
                   :QLN-PRINTDUE,
                   :QLN-PRFINCHGRB,
                   :QLN-PRINSRB,
                   :QLN-PRPOCD,
                   :QLN-PRPODATE ,
                   :QLN-PRDELINQAMT,
                   :QLN-PRLNAMT,
                   :QLN-PRPDUE-DAYS,
                   :QLN-PRDLDISC-UNEARN,
                   :QLN-PRCONPROFILE,
                   :QLN-SUM-ORIGPURAMT,
                   :QLN-SUM-LNAMT,
                   :QLN-SUM-FINCHG,
                   :QLN-SUM-INTCHG,
                   :QLN-SUM-ORGTERM,
                   :QLN-SUM-REGPYAMT,
                   :QLN-SUM-EXTCHG,
                   :QLN-SUM-SERCHG,
                   :QLN-SUM-MAINTFEE,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-DAYS-IN-UNITPER,
                   :QLN-UNITPER-PER-YEAR,
                   :QLN-REPOCD,
                   :QLN-BR-TFR-FG,
                   :QLN-O3INS-DLPROC,
                   :QLN-INSPREM-DLADJ,
                   :QLN-PLAMT,
                   :QLN-MTD-PRIN,
                   :QLN-MTD-LC,
                   :QLN-MTD-INT,
                   :QLN-MTD-OT,
                   :QLN-MTD-EX,
                   :QLN-BNKRPT-CHAPTER,
                   :QLN-BMO-CON-STATUS,
                   :QLN-BMO-REC-STATUS,
                   :QLN-APP-NUMBER,
                   :QLN-GIFTNET,
                   :QLN-GIFTNET-OUR-PO,
                   :QLN-PROC-DISBURSED,
                   :QLN-NOODDPY,
                   :QLN-CONPROFILE-1,
                   :QLN-CONPROFILE-2,
                   :QLN-CONPROFILE-3,
                   :QLN-CONPROFILE-4,
                   :QLN-CONPROFILE-5,
                   :QLN-RECPROFILE-1,
                   :QLN-RECPROFILE-2,
                   :QLN-RECPROFILE-3,
                   :QLN-RECPROFILE-4,
                   :QLN-RECPROFILE-5,
                   :QLN-TRW-STATUS,
                   :QLN-ORIGIN,
                   :QLN-PO-TRCD,
                   :QLN-NEXIS-CRSCORE,
                   :QLN-TRW-B-CONSUMER-IND,
                   :QLN-PARVHELD-PAID-FG,
                   :QLN-ACCT-FROZEN,
                   :QLN-TRW-SPECIAL-CODE,
                   :QLN-CREATION-STATUS,
                   :QLN-TOTNOLCHG,
                   :QLN-LBOX,
                   :QLN-EXTRA-PRIN-LFP,
                   :QLN-REMAIN-INT-SINCE-LFP,
                   :QLN-REMAIN-PRIN-SINCE-LFP,
                   :QLN-INTERIM-COUPON,
                   :QLN-DLCOMM-O3-CMSI,
                   :QLN-DLCOMM-PP-CMSI,
                   :QLN-TRW-J-CONSUMER-IND,
                   :QLN-ACH,
                   :QLN-1099-C-FG,
                   :QLN-TRW-COMPLIANCE-CODE,
                   :QLN-UNWIND-REASON-CODE,
                   :QLN-PBI-PBD-FG,
                   :QLN-OLD-ACCT-FORMAT-FG,
                   :QLN-APPROVED-BY-USER,
                   :QLN-APPROVED-BY-CODE,
                   :QLN-APPROVED-BY-DATE ,
                   :QLN-APPROVED-BY-TIME,
                   :QLN-LTOUCH-DATE ,
                   :QLN-LTOUCH-TIME,
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-STATE,
                   :QBW-ZIP,
                   :QBW-ORIGIN,
                   :QBW-LIASMLN,
                   :QBW-LIAOTH,
                   :QBW-LAST-RN-FOR10-DATE,
                   :QBW-SOLICIT,
                   :QBW-CONFIDENTIAL
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.
              
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

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF
           PERFORM REMOVE-WORKFILE.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

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
              MOVE "LP/ASMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ASPAID-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
