       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CPINQ1.
       DATE-WRITTEN.    022885.
      *****************************************************************
      *   DIRECTORY  : SP
      *   DESCRIPTION: LOAN INQUIRY  F10 ACCOUNT INQUIRY
      *                PROG 1 - DETAIL
      * REV:
      * SLC 012088 GOT RID OF DUPLICATE TIME COPYFILES.
      * JTG 012589 CHANGED FD-COLLFMTYPE TO FMTYPE
      *               "    MEANING FROM Y TO C
      * SLC 012789 REMOVED COPYFILES LPINDU AND LPINDUW.
      *            THEY ARE NOW PART OF LPRATE.
      * JTG 010290 REMOVED PAYOFF CALC / USED FIGURES GENERATED
      *            IN CREATE-NO-REC ROUTINE, ADDED NEW DEFAULT
      *            SCREEN DISPLAY
      * BAH 032790 CHANGED DATE 'IF' FOR PERFORMING CRNO-REC BECAUSE
      *            IF YOU CHANGED THE DATE BACK TO TODAY, IT WOULD
      *            NEVER RECALC THE POFF FIGURES !!
      * JTG 052290 ADDED DISPLAY OF AVCR-RESTRICT-FG (GA)
      * BAH 080790 ADD LT-FILE FOR ADDONS (SPPDTH)
      * SLC 091490 ADDED LPSCHD(W) TO UNPACK PMT SCHD IN LATE CHARGES.
      * BAH 102391 CHANGED THE MEMO DISPLAY
      * BAH 072092 COMMENTED OUT NOT DISPLAYING "RV"S, COLLECTORS NEED
      *            TO SEE NSF REVERSALS, SHOW WHATEVER LAST 2 EXIST
      * BAH 072492 IF DELETED MEMO SEQ 1, AND HAD 2 AND 3, WASNT
      *            DISPLAYING CORRECTLY
      * 101794 BAH REMOVED ALL OCCURANCES OF LP2
      * 053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      * 060695 BAH COMBINED APOTH AND APOT2 IN L-050
      * 083195 KEC ADDED LIBLP/LPNOCL
      * 090495 JTG ADDED TEST FOR GP-NO-LC-IN-ARREARAGE
      * 011696 BAH MOVE "PO" TO CRNO-LPTRCD WHEN F6-PAYOFF PERFORMS
      *            CREATE-NOREC
      * 013196 JTG ADDED F5, FOR PAYOFF
      * MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      * MJD 071896 ADDED LOGIC FOR WEEKLY UNITPER.
      * JTG 110796 ALLOWED CTRL'N'
      * GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      * BAH 050997 ALWAYS DISPLAY BALANCE, EVEN ON REVERSALS, #487
      * MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      * JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      * JTG 060497 ADDED LOGIC FOR 'O3' REBATE
      * JTG 072897 ADDED LPWDEL/W  MERCURY #E135
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * JTG 110796 ALLOWED CTRL'R'
      * GLF 101797 ADDED MOVE LN-APPNO TO EXT-WI-CL-APPNO
      * JTG 111197 ADDED LOGIC FOR LIBLP/DEFPOL
      *             RE: BR-NODEF-1ST-X-MONTHS #167
      * KEC 031098 ADDED 'F4-TOGGLE ON DATE' OPTION.  TOGGLE ON DATE
      *            FROM LP-PAYDATE TO LP-TRDATE AND BACK.
      * JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *            NO CHANGES WERE REQUIRED
      * MJD 083398 ADDED ENVIRONMENT VARIABLE TO DETERMINE WHETHER OR NOT
      *            TO DISPLAY BALANCES ON REVERSALS. FIXED DISPLAY TO INDICATE
      *            INCLUDED O2 BAL WITH "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      * JTG 101298 ALLOWED DISPLAY OF NEGATIVE CURBAL REVOL REGENCY
      * GLF 110498 ADDED MOVE TRANS-DATE TO EXT-WI-CL-TRANS-DATE FOR MEMOS
      * JTG 121898 ADDED CTRL 'Y', REVOLVING TRANSACTION SCAN REGENCY
      * JTG 990218 ADDED LOGIC TO STOP LAST DUE & NEXT DUE FROM DISPLAYING
      * BAH 990618 ADDED "ST"
      * MJD 001019 CHANGED TO ACCEPT PAGE-UP AND PAGE-DOWN (AS PER JTG)
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      * JTG 020806 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *            CHANGED SEND-DUE-DATE ROUTINE TO INCLUDE DUE MONTHLY
      *            INSURANCE PREMIUMS (MIP)
      * BAH 021108 ALLOW F11 TO BRING UP COLLECTIONS ASSIST SCREEN,
      *            REGACC PR# 8, NEED TO UNSET F11PROG
      * JTG 030205 CHANGED LDUE & TDUE DISPLAY PER DIRK FOR EXCLUSION
      *            OF MIP INSURANCE PREMIUMS                      REGENCY #1998
      * BAH 040812 WAS NOT CLEARING BORROWER-KEY ON F1, SO IF THE NEXT ACCOUNT
      *            YOU PULLED UP HAD THE SAME BORROWER ANYWHERE IN IT, IT WOULD
      *            DISPLAY THAT PREVIOUS BORROWER INFO INSTEAD OF THE MAKER..
      *            JUDY/CITIZENS TN
      * BAH 050303 SEND-OUTSTANDING FORMS WAS DISPLAYING A PRINTED FORM TWICE
      *            IF IT WAS THE LN-LASTCFORMNO AND ALSO THE FIRST FM RECORD
      *            READ IN, SO I PUT A TEST IN FOR THE SAME FORM NO...SUN, LA
      *            PL# 468
      * BAH 050406 ADDED LONPP1 FOR THRIFT INVESTMENT HARD COPY, PR# 2507
      * BLF 070802 DO A FLDVAL FOR VALUE OF ARCHIVE-FG; IF "Y", PASSED ACCT.
      *            IS IN ARCHIVE DIRECTORY, SO SET UP ARCHIVE PATHS FOR LN,
      *            LP, LT, LM.  IF ARCHIVE, DON'T ALLOW F3 CHANGES, F5 PAYOFF,
      *            F6 FLEX FORM INQ, WORLD PR# 547
      * BLM 080916 IF EXT-WI-CL-ARCHIVE-FG = "Y", MOVE "AR" TO IP-BUF & XFER SO
      *            LONPP0 CAN SET UP ARCHIVE PATHS, WORLD PR# 547
      * BLM 100305 ADD OPTION TO CALL ESCROW INFO WINDOW (WI/ESINFO) IF
      *            CONTROL/R IS PRESSED, REGWAN PR# 3506
      * BLM 120302 DON'T ALLOW F11 COLLECTION ASSIST WINDOW IF I-AM-WORLD &
      *            MEXICO, WORLD PR# 792
      * BAH 130107 ADDED COLAS2 FOR REGACC ONLY, PR# 330
      * BAH 130521 ADDED CODE FOR GP-DUE-29-30-31, SOAUTO PR# 4239
      * BAH 131114 DO NOT MOVE SOC-WORK TO BORROWER-KEY FOR F6 FLEX, WAS
      *            MOVING XXXXX1234 IF HIDE FLAG WAS ON, CAUSES
      *            MISSING PROGRAM AT MODERN, MODERN PL# 827
      *  CS 140925 TRIED TO FIX THE SCREEN FLICKERING WHEN YOU ENTERED AN
      *            ACCT, F1, ANOTHER ACCT, F1, BY ADDING A FORM RESET THAT
      *            DESTROYED ALL CONTROLS (A15 HAD CALL FRESET)
      * BLM 101015 SET OVERRIDE PATHS ON LN, LP, LT, & LM IF ARCHIVE
      * BLM 101020 REVISE LOGIC FOR NEW LOAD ROUTINES
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      * BAH 161102 ADDED READ-BW1-FILE FOR CHANGE TO LPAVCR WORLD PR1199
      *            SC AVAILABLE CREDIT RESTRICTION
      *
      * KEC 2016.1116 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED RVLINE-SEQNO WHICH IS FOR LTR-SEQNO.
      * BAH 170103 REMOVE BW-ARCHIVE-PATH, BORROWER NOT IN ARCHIVE, WORLD #1215
      * BAH 170824 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181115 GLOBAL FMFILE
      * BAH 181129 GLOBAL LMFILE
      * BAH 190103 GLOBAL LPFILE
      * BAH 190131 REMOVED COLAS2
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 190314 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 20190712 FORMS WAITING TO PRINT WERE NOT DISPLAY IMMEDIATELY AFTER
      *              THE F3 FMNO ASSIGN. THE WAY WE NOW CALL LONPS4 IS
      *              DIFFERENT THAN A15 SO THIS NEEDS TO PERFORM
      *              DISPLAY-OPTIONAL-INFO AFTER CALL TO LONPS4. QA0047
      * BAH 20190712 NEW MEMOS JUST ENTERED TODAY WOULD NOT DISPLAY AFTER
      *              YOU EXIT THE MEMO WINDOW LIKE A15 DOES. THE WAY WE
      *              CALL MEMOSC AND DONT EXIT THIS PROGRAM IS DIFFERENT
      *              THAN A15 SO THIS NEEDS TO NOW SEND-MEMO-UP
      *              AND DISPLAY IT AFTER CALL TO MEMOSC. QA0048
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220802 HARD CODED STARTS
      * JKC 2022-0923 CHANGED THE HARDCODING OF THE STARTS OF LPFILE.
      * JKC 2022-0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      * BAH 2023.0511 REMOVED LONPP0
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CPINQ1.CBL S35 02/26/24-10:16:25 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE
      *        "/LP/LNFILE".

      *********************************************************
      *   LPFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LP-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE
      *        "/GB/LPFILE".

      *********************************************************
      *   LTPFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTP-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE
      *        "/LP/LTPFILE".

      *********************************************************
      *   LTIFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTI-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE
      *        "/LP/LTIFILE".

      *********************************************************
      *   LMFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LM-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LM-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE
      *        "/LP/LMFILE".

       COPY "LIBLP/LP01NO.CPY".

       01  HOLD1-KEY-DOWN      PIC X(17)    VALUE SPACES.
       01  HOLD2-KEY-UP        PIC X(17)    VALUE SPACES.

       01  DISP-REV-BAL        PIC X              VALUE "Y".
       01  FULL-PAGE           PIC X              VALUE " ".
       01  ENTRY-FLAG          PIC 9              VALUE 0.
       01  LN-WK               PIC 9(6) COMP-3    VALUE 0.
       01  SUB                 PIC 9(6) COMP-3    VALUE 0.
       01  SUB1                PIC 9(6) COMP-3    VALUE 0.
       01  SUB2                PIC 9(6) COMP-3    VALUE 0.
       01  CURRENT-SUB         PIC 9(6) COMP-3    VALUE 0.

       01  MAKER-CD            PIC X        VALUE SPACES.
       01  FORM-NO             PIC 9(4)     COMP-3.

       01  SOC-WORK            PIC 9(9)     VALUE 0.
       01  SOC-WORK-ED         REDEFINES SOC-WORK.
           03  SOC-WK1         PIC 999.
           03  SOC-WK2         PIC 99.
           03  SOC-WK3         PIC 9999.

      *******************************************
      *    OUTSTANDING FORM LINE
      *******************************************
       01  OSF-LINE            VALUE " ".
           03  OSF-DATE        PIC 999999.
           03  FILLER          PIC X.
           03  OSF-PRINTED     PIC X(7).
           03  FILLER          PIC X.
           03  OSF-FMNO        PIC ZZZ9.
           03  FILLER          PIC X.
           03  OSF-FD-DESC     PIC X(20).
       01  DUE-LINE            VALUE " ".
           03  FILLER          PIC X.
           03  DUE-PROMPT      PIC X(11).
           03  DUE-DATE-DUE    PIC 99/99/99  BLANK ZERO.
           03  DUE-LCAMT       PIC Z(6).ZZ.
           03  FILLER          PIC X.
           03  DUE-AMOUNT      PIC Z(6).ZZ.
           03  FILLER          PIC XX.
       01  DEF-LINE REDEFINES DUE-LINE.
           03  FILLER          PIC X.
           03  DEF-PROMPT      PIC X(24).
           03  DEF-DATE        PIC 9(4).
       01  ACCTNO-ALP.
           03  ACCTNO-NUM      PIC 9(8).

      **************************************************
      *         ORDER LINE BUFFER                      *
      **************************************************
       01  LN-BUF              VALUE SPACES.
           03  L-010           PIC 9(6).
           03  L-DATE-SIGNAL   PIC X.
           03  L-020           PIC XX.
           03  FILLER          PIC X.
           03  L-030           PIC X(5).
           03  L-040           PIC Z(5).99-  BLANK ZERO.
           03  L-040RATE       REDEFINES L-040
                               PIC ZZZZ.999-.
           03  FILLER         REDEFINES L-040.
               05  L-040D      PIC X.
               05  L-040DLNO   PIC 9999.99-.
           03  L-050           PIC Z(4).99-  BLANK ZERO.
           03  L-060           PIC Z(4).99-  BLANK ZERO.
           03  L-070           PIC Z(5).99-  BLANK ZERO.
           03 L-080-MO     PIC 99.
           03 L-080-DAYR   PIC 99.
           03  L-090           PIC Z(5).99-  BLANK ZERO.
           03  L-100           PIC Z(4).99-  BLANK ZERO.
           03  L-110           PIC Z(5)9.99-.
           03  FILLER         REDEFINES L-110.
               05  FILLER      PIC X(9).
               05  L-120       PIC X.
      * DISPLAY BUFFER USED IS PRINCIPAL IS OVER 99,999.99
      * LEFT OUT PAID THRU, DOESNT MATTER ON A PAYOFF
       01  LN2-BUF             REDEFINES LN-BUF.
           03  L2-010          PIC 9(6).
           03  L2-DATE-SIGNAL  PIC X.
           03  L2-020          PIC XX.
           03  FILLER          PIC X.
           03  L2-030          PIC X(5).
           03  L2-040          PIC Z(7).99- BLANK ZERO.
           03  L2-040RATE     REDEFINES L2-040
                               PIC ZZZZZZ.999-.
           03  FILLER         REDEFINES L2-040.
               05  FILLER       PIC X.
               05  L2-040D      PIC X.
               05  FILLER       PIC X.
               05  L2-040DLNO   PIC 9999.99-.
           03  L2-050          PIC Z(4).99- BLANK ZERO.
           03  L2-060          PIC Z(5).99- BLANK ZERO.
           03  L2-070          PIC Z(6).99- BLANK ZERO.
           03  FILLER          PIC X.
           03  L2-090          PIC Z(5).99- BLANK ZERO.
           03  L2-100          PIC Z(4).99- BLANK ZERO.
           03  L2-110          PIC Z(5)9.99-.
           03  FILLER         REDEFINES L2-110.
               05  FILLER      PIC X(9).
               05  L2-120      PIC X.
       01  D-MEMO              REDEFINES LN-BUF.
           03  D-MEMO-DATE     PIC 99/99/99.
           03  FILLER          PIC XX.
           03  D-MEMO-SEQ      PIC 99.
           03  FILLER          PIC X(5).
           03  D-MEMO-MEMO     PIC X(60).

       01  HEAD-2.
           03  FILLER        PIC X(40) VALUE
               " DATE  CD REF    AMOUNT   OTHER  INT/LC ".
           03  FILLER        PIC X(40) VALUE
               "    PRIN PDTH   OTHER  INT/LC      PRIN".

       01  LINE-LIST.
           03  FILLER          PIC X(32)    VALUE
           "L01 L02 L03 L04 L05 L06 L07 L08.".
       01  LINE-LIST12.
           03  FILLER          PIC X(8)     VALUE
           "L01 L02.".
       01  LINE-LIST3456.
           03  FILLER          PIC X(16)    VALUE
           "L03 L04 L05 L06.".
       01  LINE-LIST78.
           03  FILLER          PIC X(8)     VALUE
           "L07 L08.".

       01  CREATE-NOREC-DONE-FG PIC X       VALUE " ".

       01  SAVE-LINE-BUFFER                   PIC X(640)   VALUE SPACES.

       01  LINE-BUFFER                                     VALUE SPACES.
           03  D-LINE                                      OCCURS 16.
               05  D-FORM-LN                  PIC X(040).
               05  D-TEST-MEMO      REDEFINES D-FORM-LN.
                   07  D-TEST-DATE            PIC 99/99/99.
                   07  FILLER                 PIC X(032).
               05  D-DUE-LN                   PIC X(040).

       01  LINE-BUFFERX             REDEFINES LINE-BUFFER.
           03  D-LINE12                       PIC X(160).
           03  D-LINE3456                     PIC X(320).
           03  D-LINE78                       PIC X(160).

       01  TOGGLE-BUFFER                                   VALUE SPACES.
           03  T-LINE                                      OCCURS 8.
               05  T-DATE                     PIC 9(06).
               05  T-DATE-SIGNAL              PIC X(01).
               05  FILLER                     PIC X(73).

       01  TOGGLE-DATES-BUFFER                             VALUE ZEROES.
           03  TOGGLE-DATES-TABLE                          OCCURS 8.
               05  TOGGLE-PAYDATE             PIC 9(06).
               05  TOGGLE-TRANS-DATE          PIC 9(06).

       01  TOGGLE-DATE                        PIC X(01)    VALUE "P".
           88  TOGGLE-DATE-PAYDATE                         VALUE "P".
           88  TOGGLE-DATE-TRANS-DATE                      VALUE "T".

       01  TOGGLE-FIRST-TIME                  PIC X(01)    VALUE "Y".
           88  TOGGLE-FIRST-TIME-NO                        VALUE "N".
           88  TOGGLE-FIRST-TIME-YES                       VALUE "Y".

       01  LEGEND-01.
           03  FILLER                   PIC X(35)  VALUE
           "F1 F2-MEMO F3 F4 F5-POFF F6-FLEX ^/".
           03  FILLER                    PIC X VALUE X'76'.
           03  FILLER                   PIC X(11)  VALUE
           " P-PRINT  :".

       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/RVLINEW.CPY".
       COPY "LIBWI/COLASTW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CPINQ_DEF.CPY".
       COPY "LIBSP/CPINQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA.
           03  CPINQ-PROG-BUF      PIC X(356).
           03  SOC-BUF             PIC X(11)    VALUE SPACES.
           03  SOC-BUF-ED          REDEFINES SOC-BUF.
               05  SOC-ED1         PIC 999.
               05  FILLER          PIC X.
               05  SOC-ED2         PIC 99.
               05  FILLER          PIC X.
               05  SOC-ED3         PIC 9999.
           03  INTERRUPT-FG        PIC X.
           03  AVAILABLE-BUF       PIC X(12).
           03  ARCHIVE-FG          PIC X.
           03  FLEX-INQ-FORM       PIC 9(3).
           03  FLEX-INQ-LINES      PIC 9(2).
           03  CPINQ-WINDOW-HANDLE PIC X(10).
           03  HOLD-EXT-FILPATH    PIC X(19).

       SCREEN SECTION.
       COPY "LIBSP/CPINQ_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CPINQ" TO VDU-FORM-NAME.

           MOVE CPINQ-PROG-BUF TO PROG-BUF.

      * ALLOW CTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.
      * NOT'T ALLOW F10 INTERRUPT:
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "F" TO VDU-CLMENU-SELECTION.

           PERFORM GET-GPENV.

      * TEST ENVIRONMENT VARIABLE "RVLINES" TO DETERMINE WHETHER
      * OR NOT TO DISPLAY BALANCES ON REVERSALS.
      
           MOVE "RVLINES" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE "N" TO DISP-REV-BAL
           ELSE
              MOVE "Y" TO DISP-REV-BAL.

       MAIN-INIT-LN-REC.
      *    IF ARCHIVE-FG = "Y"
      *       PERFORM SETUP-ARCHIVE-PATHS
      *       IF IO-BAD
      *          GO TO END-ROUTINE.

           MOVE SPACES TO ERRCD.

           MOVE LOAN-KEY TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE "LOAN RECORD DELETED" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

      * ALLOW CNTRL 'B' INTERRUPT:
           MOVE "WI/RVLINE"   TO VDU-CTRL-B-PROG.
           MOVE LN-OWNBR      TO RVLINE-BRNO.
           MOVE LN-ACCTNO     TO RVLINE-ACCTNO.
           MOVE LN-UNITPER-CD TO RVLINE-UNITPER-CD.
      * ARCHIVE-FG IS SET IN CPINQ0 BASED ON WHERE IT FINDS THE LOAN
           MOVE ARCHIVE-FG    TO RVLINE-ARCHIVE-FG.
           MOVE RVLINE-BUF    TO VDU-CTRL-B-BUF.

           MOVE SOC-ED1 TO SOC-WK1.
           MOVE SOC-ED2 TO SOC-WK2.
           MOVE SOC-ED3 TO SOC-WK3.
           IF SOC-WORK = LN-SSNO(1)
              MOVE LN-MAKERCD(1) TO MAKER-CD
           ELSE
              IF SOC-WORK = LN-SSNO(2)
                 MOVE LN-MAKERCD(2) TO MAKER-CD
              ELSE
                 IF SOC-WORK = LN-SSNO(3)
                    MOVE LN-MAKERCD(3) TO MAKER-CD
                 ELSE
                    IF SOC-WORK = LN-SSNO(4)
                       MOVE LN-MAKERCD(4) TO MAKER-CD.

           MOVE BORROWER-KEY TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "INVALID GLOBAL RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           MOVE SPACES TO HOLD1-KEY-DOWN.
           MOVE "Y" TO FULL-PAGE.

      * LPFILE IS NOT OPEN YET SO IN ORDER TO HAVE A GOOD HOLD KEY, USE
      * LN-OWNBR
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 99999     TO LP-SEQNO.
           MOVE LP1-KEY TO HOLD2-KEY-UP.

           PERFORM DISPLAY-OPTIONAL-INFO.

           PERFORM SELECTION-MODULE.
           GO TO END-ROUTINE.

      **************************************************
       SELECTION-MODULE SECTION.
      **************************************************
       SELECTION-ENTRY.
           MOVE SPACES TO ROUTE-CD.
           MOVE 0 TO SUB1.

      * F1 F2-MEMO F3 F4 F5-POFF F6-FLEX ^/V P-PRINT  :
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 0 TO NEW-PROG-SUB BORROWER-KEY
              GO TO SELECTION-MODULE-EXIT.

           IF VDUSTATUS = "12"
              MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR
              MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO
      *       MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER
              MOVE 1             TO EXT-WI-CL-SSNO-NO
              MOVE LN-SSNO(1)    TO EXT-WI-CL-SSNO(1)
              MOVE "T"           TO MEMOSC-ONLY
              MOVE " "           TO MEMOSC-WIMENU-SELECTION
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
      * CLEAR EXISTING MEMO LINES AND REDISPLAY
              MOVE SPACES TO D-LINE(7) D-LINE(8)
              PERFORM SEND-MEMO-UP
              PERFORM DISPLAY-OPTIONAL-LINES
              GO TO SELECTION-ENTRY.

           IF VDUSTATUS = "16"
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE BW-SSNO TO BORROWER-KEY
              MOVE PROG-BUF TO CPINQ-PROG-BUF
              MOVE "LP/LONPS4" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                    FORM-LINK-AREA
                                    EXIT-PATHNAME
              CANCEL FORM-PROGX
              MOVE CPINQ-PROG-BUF TO PROG-BUF
              IF ERRCD = "E" OR "X"
                 GO TO END-ROUTINE
              ELSE
                 MOVE 1 TO SUB1
                 PERFORM DISPLAY-OPTIONAL-INFO
                 GO TO SELECTION-ENTRY.

           IF ( F4-KEY ) PERFORM TOGGLE-ON-DATE
                         GO TO SELECTION-ENTRY.

           IF VDUSTATUS = "1>"
              MOVE 0 TO NEW-PROG-SUB BORROWER-KEY
              MOVE "X" TO ERRCD
              GO TO SELECTION-MODULE-EXIT.
           IF UP-KEY OR PAGE-UP-KEY
              PERFORM SEND-HEAD-2
              PERFORM DISPLAY-ENTRY-UP
              GO TO SELECTION-ENTRY.
           IF DOWN-KEY OR PAGE-DOWN-KEY
              PERFORM SEND-HEAD-2
              PERFORM DISPLAY-ENTRY-DOWN
              GO TO SELECTION-ENTRY.
           IF VDUSTATUS = "1:"
              MOVE TRANS-DATE    TO EXT-WI-CL-TRANS-DATE
              MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR
              MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO
      *       MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER
              MOVE LN-SSNO(1) TO EXT-WI-CL-SSNO(1)
              MOVE LN-SSNO(2) TO EXT-WI-CL-SSNO(2)
              MOVE LN-SSNO(3) TO EXT-WI-CL-SSNO(3)
              MOVE LN-SSNO(4) TO EXT-WI-CL-SSNO(4)
              MOVE LN-MAKERCD(1) TO EXT-WI-CL-MAKERCD(1)
              MOVE LN-MAKERCD(2) TO EXT-WI-CL-MAKERCD(2)
              MOVE LN-MAKERCD(3) TO EXT-WI-CL-MAKERCD(3)
              MOVE LN-MAKERCD(4) TO EXT-WI-CL-MAKERCD(4)
              MOVE LN-DLNO                   TO EXT-WI-CL-DLNO
              MOVE EXT-CONAME-SYSDATE        TO EXT-WI-CL-SYSDATE
              MOVE EXT-CONAME-DAILY-PASSWORD TO EXT-WI-CL-PASSWD
              MOVE "WI/WIPOFF" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO SELECTION-ENTRY.
           IF VDUSTATUS = "1<"
              GO TO FLEX-INQ-ENTRY.
      * F11-KEY
           IF VDUSTATUS = "1!"
              MOVE LN-OWNBR  TO COLAST-LN-OWNBR
              MOVE LN-ACCTNO TO COLAST-LN-ACCTNO
              MOVE "WI/COLAST" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH COLAST-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX.

           GO TO SELECTION-ENTRY.

       FLEX-INQ-ENTRY.
           MOVE 0 TO CURRENT-SUB.
           MOVE "F1, F6-FLEX SCAN, ENTER FLEX FORM NO:"
                                                    TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "RNNN060MEMODATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO RESET-FLEX-SCREEN.

           MOVE 000 TO FLEX-INQ-FORM.
       FLEX-INQ.
           IF VDUSTATUS = "1<"
              MOVE LN-ACCTNO TO LOAN-KEY
      * CAN NOT MOVE THIS FIELD, IT COULD BE XXXXX1234 FOR GP-HIDE
      * BORROWER-KEY SHOULD ALREADY BE CORRECT, EVEN IF BY COMAKER
      *       MOVE SOC-WORK TO BORROWER-KEY
              MOVE 08 TO FLEX-INQ-LINES
      *       MOVE "3" TO NEW-PROG-SUB
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE BW-SSNO TO BORROWER-KEY
              MOVE PROG-BUF TO CPINQ-PROG-BUF
              MOVE "SP/CPINQ3" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                    FORM-LINK-AREA
                                    EXIT-PATHNAME
              CANCEL FORM-PROGX
              MOVE CPINQ-PROG-BUF TO PROG-BUF
              IF ERRCD = "E" OR "X"
                 GO TO END-ROUTINE
              ELSE
                 GO TO RESET-FLEX-SCREEN.
           IF VDUSTATUS NOT = "00"
              GO TO FLEX-INQ-ENTRY.
           IF VDUNUM0 < 1000 AND VDUNUM0 NOT = 0
              MOVE VDUNUM0 TO FLEX-INQ-FORM
              MOVE "1<" TO VDUSTATUS
              GO TO FLEX-INQ.
           GO TO SELECTION-ENTRY.

       RESET-FLEX-SCREEN.
           MOVE "MEMODATE MEMODESC ACCEPT." TO MESS.
           PERFORM RESET-MESS.
           MOVE "Y" TO FULL-PAGE.

           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 99999     TO LP-SEQNO.
           MOVE LP1-KEY TO HOLD2-KEY-UP.

           PERFORM DISPLAY-OPTIONAL-INFO.

           GO TO SELECTION-ENTRY.
       SELECTION-MODULE-EXIT.
           EXIT.

      **********************************
       SEND-OUTSTANDING-FORMS SECTION.
      **********************************
           COMPUTE SUB2 = SUB1 + 2.
           PERFORM OPEN-FD1-FILE.

      * TEST FOR LAST COLLECTION LETTER SENT:
           IF LN-LASTCFORMNO NOT = 0
              MOVE LN-LASTCFORMNO TO FORM-NO
              PERFORM GET-FD-DESC
              IF FD-FMTYPE = "C"
                 MOVE LN-LASTCFORMDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO OSF-DATE
                 MOVE "PRINTED" TO OSF-PRINTED
                 MOVE FORM-NO TO OSF-FMNO
                 MOVE FD-DESC TO OSF-FD-DESC
                 PERFORM MOVE-OSF-LINE.

      * TEST FOR FORM WAITING TO BE SENT:
           PERFORM OPEN-FM1-FILE.
           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE LN-ACCTNO     TO ACCTNO-NUM.
           MOVE ACCTNO-ALP    TO FM-INITKEY.
           MOVE 0             TO FM-FMNO.

           PERFORM START-FM1-FILE.
       SEND-FM1-FORMS.
           IF SUB2 = 9
              GO TO EXIT-SEND-OUTSTANDING-FORMS.
           PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG = 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO EXIT-SEND-OUTSTANDING-FORMS.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF LPWSFM-EDIT-FM-ACCTNO NOT = ACCTNO-ALP
              GO TO EXIT-SEND-OUTSTANDING-FORMS.

           MOVE FM-FMNO TO FORM-NO.
           PERFORM GET-FD-DESC.
           IF FD-FMTYPE NOT = "C"
              GO TO SEND-FM1-FORMS.

      * DONT DISPLAY THIS SAME ONE AGAIN...
           IF LN-LASTCFORMNO = FORM-NO
              IF FM-PRINT = "Y"
                 GO TO SEND-FM1-FORMS.

           MOVE FORM-NO TO OSF-FMNO.
           MOVE FD-DESC TO OSF-FD-DESC.
           IF FM-PRINT = "Y"
              MOVE "PRINTED" TO OSF-PRINTED
            ELSE
              MOVE "WAITING" TO OSF-PRINTED.
           PERFORM MOVE-OSF-LINE.

           GO TO EXIT-SEND-OUTSTANDING-FORMS.

       GET-FD-DESC.
           MOVE FORM-NO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "Y" TO FD-FMTYPE
              MOVE "* * UNKNOWN FORM * *" TO FD-DESC.

       MOVE-OSF-LINE.
           MOVE OSF-LINE TO D-FORM-LN(SUB2).
           ADD 1 TO SUB2.
           MOVE SPACES TO OSF-LINE.

       EXIT-SEND-OUTSTANDING-FORMS.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-FM1-FILE.

      ******************************************************************
       DISPLAY-ENTRY-DOWN SECTION.
      ******************************************************************

           MOVE SPACES         TO LINE-BUFFER LN-BUF.
           MOVE ZEROES         TO LN-WK.
           MOVE "Y"            TO TOGGLE-FIRST-TIME.
           MOVE HOLD1-KEY-DOWN TO LP1-KEY.

           IF ( LP-SEQNO = 99999 )
              MOVE 9 TO IO-FG
              GO TO DISPLAY-DOWN-EXIT.

           MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM OPEN-LP1-FILE.

      * ***NOTE*** 'START-LP1-FILE-SCAN' MUST BE USED WHEN FIRST USING
      * 'START-LP1-FILE-NOT-GREATER'; USED WHEN DISPLAYING MULTIPLE
      * PAGES IN AN UP & DOWN FUNCTION; YOU NEED TO ADD ONE TO 
      * LP-SEQNO (IN WBEG) IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      
           MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           COMPUTE QLP1-WBEG-SEQNO = LP-SEQNO + 1.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
           IF ( LP-BRNO = 9999 )
              MOVE LP-BRNO    TO QLP1-WSBEG-BRNO
           ELSE
              COMPUTE QLP1-WSBEG-BRNO = LP-BRNO + 1.

           IF ( LP-ACCTNO = 99999999 )
              MOVE LP-ACCTNO  TO QLP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QLP1-WSBEG-ACCTNO = LP-ACCTNO + 1.

           PERFORM START-LP1-FILE-SCAN.

           MOVE 0 TO ENTRY-FLAG.

      *=================================================================
       READ-LPFILE1-NEXT.

           IF ( ENTRY-FLAG = 2 )
              MOVE 9 TO IO-FG
              GO TO BYPASS-LPFILE1-READ.

           PERFORM READ-LP1-FILE-NEXT.
           IF ( LP-ACCTNO NOT = LN-ACCTNO ) OR
               ( LP-BRNO NOT = LP-PATH-OWNBR )
              IF ( IO-FG = ZEROES )
                 ADD 1 TO ENTRY-FLAG
                 GO TO READ-LPFILE1-NEXT.

      *-----------------------------------------------------------------
       BYPASS-LPFILE1-READ.

           ADD 1 TO LN-WK.

           IF ( IO-FG = ZEROES )
              PERFORM SETUP-LINE
              MOVE LN-WK TO SUB
              PERFORM SET-TOGGLE-BUFFER.

           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACES TO LN-BUF.

           IF ( LN-WK = 8 )
              GO TO DISPLAY-XFER-DOWN.

           IF ( IO-FG = ZEROES )
              GO TO READ-LPFILE1-NEXT.

           IF ( FULL-PAGE = "Y" )
              MOVE "N"       TO FULL-PAGE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY   TO HOLD2-KEY-UP
              PERFORM DISPLAY-ENTRY-UP
              GO TO DISPLAY-DOWN-EXIT.

       DISPLAY-XFER-DOWN.

           PERFORM DISPLAY-ALL-LINES.

       DISPLAY-DOWN-EXIT.

           IF ( IO-FG = ZEROES )
              MOVE "Y"       TO FULL-PAGE
              MOVE LP1-KEY   TO HOLD1-KEY-DOWN
           ELSE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY   TO HOLD1-KEY-DOWN.

           PERFORM CLOSE-LP1-FILE.

       EXIT-DISPLAY-ENTRY-DOWN.
           EXIT.

      ******************************************************************
       DISPLAY-ENTRY-UP SECTION.
      ******************************************************************

           MOVE SPACES       TO LINE-BUFFER LN-BUF.
           MOVE 9            TO LN-WK.
           MOVE "Y"          TO TOGGLE-FIRST-TIME.
           MOVE HOLD2-KEY-UP TO LP1-KEY.

           IF ( LP-SEQNO = ZEROES )
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-EXIT.

           MOVE LP1-KEY TO HOLD1-KEY-DOWN.
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           MOVE LP-SEQNO      TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           MOVE ZEROES TO ENTRY-FLAG.

      *=================================================================
       READ-LPFILE2-NEXT.

           IF ( ENTRY-FLAG = 2 )
              MOVE 9 TO IO-FG
              GO TO BYPASS-LPFILE2-READ.

           PERFORM READ-LP1-FILE-PREVIOUS.

           IF ( LP-ACCTNO NOT = LN-ACCTNO ) OR
                ( LP-BRNO NOT = LP-PATH-OWNBR )
              IF ( IO-FG = ZEROES )
                 ADD 1 TO ENTRY-FLAG
                 GO TO READ-LPFILE2-NEXT.

      *-----------------------------------------------------------------
       BYPASS-LPFILE2-READ.

           SUBTRACT 1 FROM LN-WK.

           IF ( LN-WK < 1 )
              MOVE "Y" TO FULL-PAGE
              GO TO DISPLAY-XFER-UP.

           IF ( IO-FG = ZEROES )
              PERFORM SETUP-LINE
              MOVE LN-WK TO SUB
              PERFORM SET-TOGGLE-BUFFER.

           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACES TO LN-BUF.

           IF ( IO-FG = ZEROES )
              GO TO READ-LPFILE2-NEXT.

           IF ( FULL-PAGE = "Y" )
              MOVE "N"       TO FULL-PAGE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE ZEROES    TO LP-SEQNO
              MOVE LP1-KEY   TO HOLD1-KEY-DOWN
              PERFORM DISPLAY-ENTRY-DOWN
              GO TO DISPLAY-UP-EXIT.

       DISPLAY-XFER-UP.

           PERFORM DISPLAY-ALL-LINES.

       DISPLAY-UP-EXIT.

           IF ( IO-FG = ZEROES )
              MOVE "Y"       TO FULL-PAGE
              MOVE LP1-KEY   TO HOLD2-KEY-UP
           ELSE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE ZEROES    TO LP-SEQNO
              MOVE LP1-KEY   TO HOLD2-KEY-UP.

           PERFORM CLOSE-LP1-FILE.

       EXIT-DISPLAY-ENTRY-UP.
           EXIT.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE SPACES TO LN-BUF.
           IF LP-APCUR > 99999.99 OR LP-APCUR < -99999.99
              PERFORM SETUP-LINE2
              GO TO END-SETUP-LINE.

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
           ELSE
              MOVE LP-TRDATE  TO DATE-YYYYMMDD.

           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO L-010.

           IF LP-PAYDATE NOT = LP-TRDATE
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE "."          TO L-DATE-SIGNAL
              ELSE
      * LOWER CASE T
                 MOVE X'74' TO L-DATE-SIGNAL.

           MOVE LP-TRCD  TO L-020.
           MOVE LP-REFNO TO L-030 LPWSLP-LP-REFNO.
           IF LP-TRCD = "JD" OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO L-040RATE
           ELSE
              IF (LP-TRCD = "P4" OR "P5")
                    AND (LP-DLNO NOT= 0)
                 MOVE "D" TO L-040D
                 MOVE LP-DLNO TO L-040DLNO
              ELSE
                 MOVE LP-TRAMT TO L-040
                 IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                     (LP-TRCD = "RV" AND
                      (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL")))
                   IF LP-IBPC = "I"
                      MOVE LP-TRAMT TO L-040
                   ELSE
                      MOVE LP-APCUR TO L-040.

           IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 OR ( (LP-TRCD = "RV") AND
                       (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                            OR "RB" OR "RO")
                    )
              )  AND (LP-APOT2 NOT = 0)
              ADD LP-APOTH LP-APOT2 GIVING L-050
              INSPECT L-050 REPLACING TRAILING " " BY "*"
           ELSE
      *       MOVE LP-APOTH TO L-050.
              ADD LP-APOTH LP-APOT2 GIVING L-050.

           ADD LP-APINT LP-APLC GIVING L-060.
           MOVE LP-APCUR TO L-070.
           IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                 (LP-TRCD = "RV" AND
                  (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL")))
                AND LP-IBPC = "P"
                   MOVE LP-TRAMT TO L-070.

      * 5/9/97 BAH ALWAYS DISPLAY BALANCE, #487
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET (MJD AS PER JTG)
      
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                   DISP-REV-BAL = "N"
              MOVE "*" TO L-120
           ELSE
              MOVE LP-PDTH-DATE TO NUM-DATE
              MOVE NUM-MO TO L-080-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE NUM-DA TO L-080-DAYR
              ELSE
                 MOVE NUM-YR TO L-080-DAYR
              END-IF
              COMPUTE L-090 = LP-OTHBAL + LP-OT2BAL
              ADD LP-INTBAL LP-LCBAL GIVING L-100
              MOVE LP-CURBAL TO L-110
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                         NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
                 MOVE "*" TO L-120.

       END-SETUP-LINE.
           EXIT.

      ******************************************
       SETUP-LINE2 SECTION.
      ******************************************

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
           ELSE
              MOVE LP-TRDATE  TO DATE-YYYYMMDD.

           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO L2-010.

           IF LP-TRDATE NOT = LP-PAYDATE
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE "."          TO L2-DATE-SIGNAL
              ELSE
      * LOWER CASE T
                 MOVE X'74' TO L2-DATE-SIGNAL.

           MOVE LP-TRCD TO L2-020.
           MOVE LP-REFNO TO L2-030 LPWSLP-LP-REFNO.
           IF LP-TRCD = "JD" OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO L2-040RATE
           ELSE
              IF (LP-TRCD = "P4" OR "P5")
                    AND (LP-DLNO NOT= 0)
                 MOVE "D" TO L2-040D
                 MOVE LP-DLNO TO L2-040DLNO
              ELSE
                 MOVE LP-TRAMT TO L2-040
                 IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                      (LP-TRCD = "RV" AND
                      (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL")))
                   IF LP-IBPC = "I"
                      MOVE LP-TRAMT TO L2-040
                   ELSE
                      MOVE LP-APCUR TO L2-040.

           IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 OR ( (LP-TRCD = "RV") AND
                       (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                            OR "RB" OR "RO")
                    )
              )  AND (LP-APOT2 NOT = 0)
              ADD LP-APOTH LP-APOT2 GIVING L2-050
              INSPECT L2-050 REPLACING TRAILING " " BY "*"
           ELSE
              ADD LP-APOTH LP-APOT2 GIVING L2-050.

           ADD LP-APINT LP-APLC GIVING L2-060.
           MOVE LP-APCUR TO L2-070.
           IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                 (LP-TRCD = "RV" AND
                  (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL")))
               AND LP-IBPC = "P"
                   MOVE LP-TRAMT TO L2-070.

      * 5/9/97 BAH ALWAYS DISPLAY BALANCE, #487
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET (MJD AS PER JTG)
      
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                   DISP-REV-BAL = "N"
              MOVE "*" TO L2-120
           ELSE
              COMPUTE L2-090 = LP-OTHBAL + LP-OT2BAL
              ADD LP-INTBAL LP-LCBAL GIVING L2-100
              MOVE LP-CURBAL TO L2-110
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                             NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
                 MOVE "*" TO L2-120.

      **************************************************
       DISPLAY-OPTIONAL-INFO SECTION.
      **************************************************
           PERFORM SEND-HEAD-2.

           IF CREATE-NOREC-DONE-FG = "Y"
              MOVE SAVE-LINE-BUFFER TO LINE-BUFFER
              GO TO DISPLAY-OPTIONAL-INFO-EXIT.

           PERFORM DISPLAY-INITIAL-UP.

           MOVE LN-ACCTNO     TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE    TO CRNOR2-SYSDATE.
           MOVE "Y"           TO CRNOR2-DEF-STOP.
           MOVE "N"           TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           PERFORM SEND-OUTSTANDING-FORMS.

           PERFORM SEND-DUE-DAYS.

           PERFORM SEND-MEMO-UP.

           MOVE "Y" TO CREATE-NOREC-DONE-FG.
           MOVE LINE-BUFFER TO SAVE-LINE-BUFFER.
       DISPLAY-OPTIONAL-INFO-EXIT.
           PERFORM DISPLAY-OPTIONAL-LINES.

      **************************************************
       DISPLAY-INITIAL-UP SECTION.
      **************************************************
           MOVE SPACES TO LINE-BUFFER LN-BUF.

           MOVE 2 TO LN-WK.
           MOVE "Y" TO TOGGLE-FIRST-TIME.
           MOVE HOLD2-KEY-UP TO LP1-KEY.
           MOVE LP1-KEY TO HOLD1-KEY-DOWN.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           MOVE LP-SEQNO      TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.

       DISPLAY-INITIAL-READ-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
               GO TO DISPLAY-INITIAL-XFER-UP.

      *    IF LP-REV = "Y" OR LP-TRCD = "RV"
      *       GO TO DISPLAY-INITIAL-READ-NEXT.

           PERFORM SETUP-LINE.
           MOVE LN-BUF TO D-LINE(LN-WK).

           MOVE LN-WK TO SUB
           PERFORM SET-TOGGLE-BUFFER.

           SUBTRACT 1 FROM LN-WK.
           IF LN-WK = 1
              GO TO DISPLAY-INITIAL-READ-NEXT.

       DISPLAY-INITIAL-XFER-UP.
           IF LN-WK = 1
              MOVE D-LINE(2) TO D-LINE(1)
              MOVE SPACES TO D-LINE(2).
       DISPLAY-INITIAL-UP-EXIT.
           MOVE 1 TO SUB1.
           PERFORM CLOSE-LP1-FILE.

      ******************************************
       SEND-MEMO-UP SECTION.
      ******************************************
           MOVE 8 TO SUB1.

           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
      * WANT TO SKIP PRIORITIES
           MOVE 1             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.
       SEND-MEMO-READ-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LM-BRNO NOT = LM-PATH-OWNBR)
                             OR (LM-ACCTNO NOT = LN-ACCTNO)
                              OR (LM-DATE = 0)
              GO TO SEND-MEMO-UP-EXIT.

           COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE SPACES      TO D-MEMO.
           MOVE DATE-MMDDYY TO D-MEMO-DATE.
           MOVE LM-SEQ      TO D-MEMO-SEQ.
           MOVE LM-MEMO     TO D-MEMO-MEMO.
      * ALWAYS PRINT MOST CURRENT, CHECKING FOR SEQUENCES...
      * HAD PROBLEMS IF SEQ HAD BEEN DELETED
           IF D-LINE(SUB1) NOT = SPACES
              IF LM-SEQ > 1
                 IF D-MEMO-DATE = D-TEST-DATE(SUB1)
                    NEXT SENTENCE
                 ELSE
                    IF SUB1 = 8
                       SUBTRACT 1 FROM SUB1
                       IF D-MEMO-DATE = D-TEST-DATE(SUB1)
                          NEXT SENTENCE
                       ELSE
                          GO TO SEND-MEMO-UP-EXIT
                    ELSE
                       IF SUB1 = 7
                          ADD 1 TO SUB1
                          IF D-MEMO-DATE = D-TEST-DATE(SUB1)
                             NEXT SENTENCE
                          ELSE
                             GO TO SEND-MEMO-UP-EXIT
                       ELSE
                          NEXT SENTENCE
              ELSE
                 GO TO SEND-MEMO-UP-EXIT.
           MOVE D-MEMO TO D-LINE(SUB1).
           IF SUB1 = 8
              SUBTRACT 1 FROM SUB1
              GO TO SEND-MEMO-READ-NEXT.
           IF SUB1 = 7
              ADD 1 TO SUB1
              GO TO SEND-MEMO-READ-NEXT.

       SEND-MEMO-UP-EXIT.
           PERFORM CLOSE-LM1-FILE.

      *******************************************
       SEND-DUE-DAYS SECTION.
      *******************************************

           COMPUTE SUB1 = SUB1 + 1.
           MOVE "LAST  DUE:"     TO DUE-PROMPT
           MOVE NO-LDUE-DATE     TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY      TO DUE-DATE-DUE
           MOVE NO-LDUELC        TO DUE-LCAMT.
           IF LN-MIP-FG = "Y"
              MOVE NO-LDUE-DATE TO POFF-PAYDATE
              PERFORM DETERMINE-MIP-REQUIRED
              IF LN-MIP-CLPDTH-DATE NOT = NO-LDUE-DATE
                 ADD POFF-LXM-MIP-CLPAID TO NO-LDUECONTR NO-LDUEAMT
              END-IF
              IF LN-MIP-AHPDTH-DATE NOT = NO-LDUE-DATE
                 ADD POFF-LXM-MIP-AHPAID
                               TO NO-LDUECONTR NO-LDUEAMT
              END-IF
           END-IF
           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-LDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE NO-LDUEAMT TO DUE-AMOUNT
           END-IF
           ADD 1 TO SUB1
           MOVE DUE-LINE TO D-DUE-LN(SUB1).

           MOVE "TODAY DUE:"     TO DUE-PROMPT.
           MOVE NO-TDUE-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO DUE-DATE-DUE.
           MOVE NO-TDUELC        TO DUE-LCAMT.
           IF LN-MIP-FG = "Y"
              MOVE NO-TDUE-DATE TO POFF-PAYDATE
              PERFORM DETERMINE-MIP-REQUIRED
    
              IF POFF-LXM-MIP-CLPAID NOT = 0
                 MOVE LN-MIP-CLPDTH-DATE TO NDTE-DATE
                 MOVE 1             TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 IF NO-TDUE-DATE >= NDTE-DATE
                    ADD POFF-LXM-MIP-CLPAID TO NO-TDUECONTR NO-TDUEAMT
                 END-IF
              END-IF
    
              IF POFF-LXM-MIP-AHPAID NOT = 0
                 MOVE LN-MIP-AHPDTH-DATE TO NDTE-DATE
                 MOVE 1             TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 IF NO-TDUE-DATE >= NDTE-DATE
                    ADD POFF-LXM-MIP-AHPAID TO NO-TDUECONTR NO-TDUEAMT
                 END-IF
              END-IF.

      * IF ACCOUNT HAS A DUE DAY OF 29,30 OR 31 AND THAT DATE DOES NOT EXIST
      * IN THE CURRENT MONTH, ON THE LAST DAY OF THE MONTH, ADD IN A REGULAR
      * PAYMENT TO THE DUE AMOUNT
           IF GP-DUE-29-30-31 = "Y"
              IF LN-1STPYDA = 29 OR 30 OR 31
      * FIRST GET LAST DAY OF CURRENT MONTH
                 MOVE TRANS-DATE TO NDTE-DATE
                 MOVE 31 TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION
      * CHECK TO SEE IF TODAY IS LAST DAY OF CURRENT MONTH
                 IF NDTE-DATE = TRANS-DATE
      * IF SO, DOES THE DUE DAY EXIST IN THIS MONTH ?
                    IF LN-1STPYDA > NDTE-DD
                       ADD LN-REGPYAMT TO NO-TDUECONTR NO-TDUEAMT.

           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-TDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE NO-TDUEAMT TO DUE-AMOUNT.
           ADD 1 TO SUB1.
           MOVE DUE-LINE TO D-DUE-LN(SUB1).

           MOVE "NEXT  DUE:"  TO DUE-PROMPT
           MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY   TO DUE-DATE-DUE
           MOVE NO-NXDUELC    TO DUE-LCAMT.
           IF LN-MIP-FG = "Y"
              MOVE NO-NXDUE-DATE TO POFF-PAYDATE
              PERFORM DETERMINE-MIP-REQUIRED
              ADD POFF-LXM-MIP-CLPAID
                  POFF-LXM-MIP-AHPAID TO NO-NXDUECONTR NO-NXDUEAMT
           END-IF
           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-NXDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE NO-NXDUEAMT TO DUE-AMOUNT
           END-IF
           ADD 1 TO SUB1
           MOVE DUE-LINE TO D-DUE-LN(SUB1).

           MOVE "DF" TO DEFPOL-TRCD
           MOVE TRANS-DATE TO DEFPOL-DATE
           PERFORM VALIDATE-DF-POLICY.
           IF DEFPOL-MAXIMUM = " "
              IF LN-CURBAL NOT = 0
                 MOVE NO-PDTH-DATE TO NDTE-DATE
                 MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
                 MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
                 MOVE 1 TO NDTE-HOLD
                 PERFORM INCREMENT-UNITPER
      * WANT BBBBMMYY IN A FIELD THATS YYYYMMDD
                 IF NOT LN-UNITPER-CD-DAYS
                    MOVE NDTE-YY TO NDTE-DD
                 END-IF
              ELSE
                 MOVE 0 TO NO-DEFERMENT NDTE-DATE
              END-IF
              MOVE "DEFERMENT CHARGE FOR " TO DEF-PROMPT
              MOVE NDTE-DATE TO DEF-DATE
              MOVE NO-DEFERMENT TO DUE-AMOUNT
              ADD 1 TO SUB1
              MOVE DUE-LINE TO D-DUE-LN(SUB1).

      ******************************************************************
       SET-TOGGLE-BUFFER SECTION.
      ******************************************************************

           IF ( TOGGLE-FIRST-TIME-YES )
              MOVE "N"    TO TOGGLE-FIRST-TIME
              MOVE SPACES TO TOGGLE-BUFFER
              MOVE SPACES TO TOGGLE-DATES-BUFFER.

           MOVE LP-PAYDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TOGGLE-PAYDATE   (SUB).
           MOVE LP-TRDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TOGGLE-TRANS-DATE(SUB).
           MOVE LN-BUF      TO T-LINE           (SUB).

       EXIT-SET-TOGGLE-BUFFER.
           EXIT.

      ******************************************************************
       TOGGLE-ON-DATE SECTION.
      ******************************************************************

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE "T" TO TOGGLE-DATE
           ELSE
              MOVE "P" TO TOGGLE-DATE.

           MOVE SPACES TO LINE-BUFFER.
           MOVE ZEROES TO SUB.

       NEXT-TOGGLE-ON-DATE.

           ADD 1 TO SUB.

           IF ( SUB > 8 )
              GO TO END-TOGGLE-ON-DATE.

           IF ( TOGGLE-PAYDATE   (SUB) NOT NUMERIC ) AND
              ( TOGGLE-TRANS-DATE(SUB) NOT NUMERIC )
              GO TO NEXT-TOGGLE-ON-DATE.

           IF TOGGLE-TRANS-DATE(SUB) NOT = TOGGLE-PAYDATE(SUB)
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE TOGGLE-PAYDATE   (SUB) TO T-DATE       (SUB)
                 MOVE "."                    TO T-DATE-SIGNAL(SUB)
              ELSE
                 MOVE TOGGLE-TRANS-DATE(SUB) TO T-DATE       (SUB)
                 MOVE X'74'           TO T-DATE-SIGNAL(SUB)
           ELSE
              MOVE TOGGLE-PAYDATE      (SUB) TO T-DATE       (SUB)
              MOVE SPACES                    TO T-DATE-SIGNAL(SUB).

           MOVE T-LINE(SUB) TO D-LINE(SUB).

           GO TO NEXT-TOGGLE-ON-DATE.

       END-TOGGLE-ON-DATE.

           PERFORM DISPLAY-ALL-LINES.

       EXIT-TOGGLE-ON-DATE.
           EXIT.

      *****************************************
       FORM-RESET SECTION.
      *****************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALL CONTROLS.
           DISPLAY CPINQ-SCREEN UPON CPINQ-WINDOW-HANDLE.
           MOVE CPINQ-FORM-FIELDS TO WR-BUF.
           MOVE CPINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE CPINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPINQ-HELP--DIR TO HELP-LINK-DIR.

      *************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CPINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-ALL-LINES.
           MOVE LINE-BUFFER TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-OPTIONAL-LINES.
           MOVE D-LINE12 TO WR-BUF.
           MOVE LINE-LIST12 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE D-LINE3456 TO WR-BUF.
           MOVE LINE-LIST3456 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE D-LINE78 TO WR-BUF.
           MOVE LINE-LIST78 TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SEND-HEAD-2.
           MOVE "S  A800HEAD:S." TO VDU.
           MOVE HEAD-2           TO VDUBUF.
           PERFORM SCREENIO.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ********************************************
       END-ROUTINE.
      ********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF ERRCD = "X" OR "E"
              IF HOLD-EXT-FILPATH NOT = EXT-FILPATH
                 MOVE HOLD-EXT-FILPATH TO EXT-FILPATH
                 SET ENVIRONMENT "FIL" TO EXT-FILPATH-FIL.
           MOVE PROG-BUF TO CPINQ-PROG-BUF.
       STOP-RUN.
           EXIT PROGRAM.
