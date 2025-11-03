       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AUDIQ1.
       DATE-WRITTEN.    022885.
      *****************************************************************
      *   DIRECTORY  : LP
      *   DESCRIPTION: AUDITER INQUIRY FOR COLLECTIONS
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
      * BAH 101794 REMOVED ALL OCCURANCES OF LP2
      * SLC 053095 COMBINED OTHBAL AND OT2BAL IN L-090
      * BAH 060695 COMBINED APOTH AND APOT2 IN L-050
      * MJD 080895 COPIED SP/SPINQ1.C TO LP/AUDIQ1.C AND LIBSP/CPINQ1.CMD
      *            TO LIBLP/AUDIQ1.CMD.  REMOVED F2-MEMO, F3-CHGS,
      *            AND F6-PAYOFF OPTIONS.
      * JTG 090495 ADDED TEST FOR GP-NO-LC-IN-RREARAGE
      * MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      * JTG 022897 REMOVED F9 LOGIC TO CALL AUMENU, SINCE GAINQ WAS
      *            INCORPRATED INTO CLMENU.C
      * GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      * MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      * JTG 060397 ADDED TEST OF 'O3' RE: BR-OTH-INS-LEVELS
      * JTG 072897 ADDED LPWDEL/W  MERCURY #E135
      * GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      * JTG 111797 ADDED LOGIC FOR LIBLP/DEFPOL
      *             RE: BR-NODEF-1ST-X-MONTHS #167
      * KEC 030998 ADDED 'F5-TOGGLE ON DATE' OPTION.  TOGGLE ON DATE
      *             FROM LP-PAYDATE TO LP-TRDATE AND BACK.
      * KEC 031698 CHANGE 'F5-TOGGLE ON DATE' OPTION TO
      *                   'F4-TOGGLE ON DATE' OPTION.
      *            CHANGE 'F4-GEORIGA ACCOUNT INQUIRY' OPTION TO
      *                   'F5-GEORIGA ACCOUNT INQUIRY' OPTION.
      * JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *            NO CHANGES WERE MADE RE: THIS PROBLEM
      * MJD 083398 FIXED DISPLAY TO INDICATE INCLUDED O2 BAL WITH
      *            "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      * JTG 101298 ALLOWED DISPLAY OF NEGATIVE CURBAL REVOL REGENCY
      * GLF 110698 MADE MEMO DATE 2000 COMPATIBLE
      * JTG 990218 ADDED LOGIC TO STOP LAST DUE & NEXT DUE FROM DISPLAYING
      * BAH 990618 ADDED "ST"
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * BLV 020628 CHECK STATUS COMMING BACK FROM GAINQY WINDOW, IF NOT
      *            SPACES THEN SET FLAG TO AUTOMATICALLY BRING UP NEW ACCOUNT
      *            IN AUDIQ0.  PERFORM SAME ACTION IF F2 KEY IS HIT.
      * BLV 020628 IF WORLD, THEN AUTOMATICALLY DISPLAY THE WI/GAINQY
      *            ACCOUNT INFO WINDOW UPON ENTRY.
      * BLV 020628 CALL CL/CLMENU INDEPENDANTLY OF SCREENIO SO THAT
      *            F2-NEXT-ACCOUNT OPTION WILL WORK FROM GAINQY.
      * BAH 050303 SEND-OUTSTANDING FORMS WAS DISPLAYING A PRINTED FORM TWICE
      *            IF IT WAS THE LN-LASTCFORMNO AND ALSO THE FIRST FM RECORD
      *            READ IN, SO I PUT A TEST IN FOR THE SAME FORM NO...SUN, LA
      *            PL# 468
      *  BAH 050406 ADDED LONPP1 FOR THRIFT INVESTMENT HARD COPY, PR# 2507
      *  BLF 070920 DO A FLDVAL FOR VALUE OF ARCHIVE-FG; IF "Y", PASSED ACCT.
      *             IS IN ARCHIVE DIRECTORY, SO SET UP ARCHIVE PATHS FOR LN,
      *             LP, LT, LM.  WORLD PR# 547
      *  BLM 080916 IF ARCHIVE-FG = "Y", MOVE "AR" TO IP-BUF & XFER SO
      *             LONPP0 CAN SET UP ARCHIVE PATHS, WORLD PR# 547
      *  BLM 110203 IF GP-I-AM-BAY-COUNTRY & USERID STARTS WITH "P", DON'T
      *             DISPLAY 2 MEMO LINES & LOCK OUT F9, BAYC PR# 3724.
      *   CS 141031 ADDED A MOVE OF SPACES TO GET-NEXT-FG IN THE SELECTION
      *             MODULE IF YOU PRESSED F1, A15 DID A FRESET AND THEN AT XFER       *             AT END-ROUTINE. IN A30, IF YOU ARE WORLD IT GETS SET TO 'GA'
      *             NEVER GETS CLEARED AND ITS IN THE LINKAGE, LEAVING YOU IN
      *             A LOOP.
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0205 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 161102 ADDED READ-BW1-FILE FOR CHANGE TO LPAVCR WORLD PR1199
      *            SC AVAILABLE CREDIT RESTRICTION
      * BAH 170103 REMOVE BW-ARCHIVE-PATH, BORROWER NOT IN ARCHIVE, WORLD #1215
      * BAH 170320 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181112 GLOBAL FMFILE
      * BAH 181127 GLOBAL LMFILE, ALSO COMMENTED OUT ARCHIVE
      * BAH 181221 GLOBAL LPFILE, CHANGED HOLD1-KEY-DOWN AND
      *            HOLD2-KEY-UP FROM 13 TO 17
      *  CS 190206 COMMENT AWFILE, REPLACE WITH LPCPAW
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20220803 HARD CODED START
      *
      * JKC 2022-0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      *
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
           "@(#)LP/AUDIQ1.CBL S35 02/20/24-15:03:32 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
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
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       COPY "LIBLP/LP01NO.CPY".

      ******************************************************************
      ******************************************************************
       01  START-WITH-GAINQY-FG        PIC X(01)  VALUE "Y".
           88  START-WITH-GAINQY                  VALUE "Y".

       01  START-WITHOUT-BALANCES-FG   PIC X(01)  VALUE "Y".
           88  START-WITHOUT-BALANCES             VALUE "Y".
      ******************************************************************
      ******************************************************************

       01  HOLD1-KEY-DOWN      PIC X(17)    VALUE SPACES.
       01  HOLD2-KEY-UP        PIC X(17)    VALUE SPACES.

       01  FULL-PAGE           PIC X              VALUE " ".
       01  ENTRY-FLAG          PIC 9              VALUE 0.
       01  LN-WK               PIC 9(6) COMP-3    VALUE 0.
       01  SUB                 PIC 9(6) COMP-3    VALUE 0.
       01  SUB1                PIC 9(6) COMP-3    VALUE 0.
       01  SUB2                PIC 9(6) COMP-3    VALUE 0.
       01  HOLD-TRANS-DATE     PIC 9(8) COMP-3    VALUE 0.

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
           03  L-080-MO        PIC 99.
           03  L-080-DAYR      PIC 99.
           03  L-090           PIC Z(5).99-  BLANK ZERO.
           03  L-100           PIC Z(4).99-  BLANK ZERO.
           03  L-110           PIC Z(5)9.99-.
           03  L-110-REDEF    REDEFINES L-110.
               05  FILLER      PIC X(9).
               05  L-120       PIC X.
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

      *-----------------------------------------------------------------
       01  LINE-BUFFER                                     VALUE SPACES.
           03  D-LINE                                      OCCURS 16.
               05  D-FORM-LN                  PIC X(040).
               05  D-TEST-MEMO      REDEFINES D-FORM-LN.
                   07  D-TEST-DATE            PIC 99/99/99.
                   07  FILLER                 PIC X(032).
               05  D-DUE-LN                   PIC X(040).

      *01  LINE-BUFFERX             REDEFINES LINE-BUFFER.
      *    03  D-LINE12                       PIC X(160).
      *    03  D-LINE3456                     PIC X(320).
      *    03  D-LINE78                       PIC X(160).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TOGGLE-BUFFER                                   VALUE SPACES.
           03  T-LINE                                      OCCURS 8.
      * NOTE: THESE DATES MUST STAY 9(6)
               05  T-DATE                     PIC 9(06).
               05  T-DATE-SIGNAL              PIC X(01).
               05  FILLER                     PIC X(73).

       01  TOGGLE-DATES-BUFFER                             VALUE ZEROES.
           03  TOGGLE-DATES-TABLE                          OCCURS 8.
      * NOTE: THESE DATES MUST STAY 9(6)
               05  TOGGLE-PAYDATE             PIC 9(06).
               05  TOGGLE-TRANS-DATE          PIC 9(06).

       01  TOGGLE-DATE                        PIC X(01)    VALUE "P".
           88  TOGGLE-DATE-PAYDATE                         VALUE "P".
           88  TOGGLE-DATE-TRANS-DATE                      VALUE "T".

       01  TOGGLE-FIRST-TIME                  PIC X(01)    VALUE "Y".
           88  TOGGLE-FIRST-TIME-NO                        VALUE "N".
           88  TOGGLE-FIRST-TIME-YES                       VALUE "Y".

      *-----------------------------------------------------------------

       01  LEGEND-01.
           03  FILLER                  PIC X(29)  VALUE
           "F1-BACK, F4-DATE, F9-INFO, ^/".
      * SMALL "V"
           03  FILLER                  PIC X VALUE X'76'.
           03  FILLER                  PIC X(15)  VALUE 
           ", P-PRINT     :".
       01  LEGEND-01-GA.
           03  FILLER                  PIC X(32)  VALUE
           "F1-BACK, F4-DATE, F5-INQ, F9, ^/".
      * SMALL "V"
           03  FILLER                  PIC X VALUE X'76'.
           03  FILLER                  PIC X(12)  VALUE
           ", P-PRINT  :".

       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/GAINQW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AUDIQ_DEF.CPY".
       COPY "LIBLP/AUDIQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA.
           03  AUDIQ-PROG-BUF            PIC X(400).
           03  SOC-BUF             PIC X(11)    VALUE SPACES.
           03  SOC-BUF-ED          REDEFINES SOC-BUF.
               05  SOC-ED1         PIC 999.
               05  FILLER          PIC X.
               05  SOC-ED2         PIC 99.
               05  FILLER          PIC X.
               05  SOC-ED3         PIC 9999.
           03  INTERRUPT-FG              PIC X.
           03  AVAILABLE-BUF             PIC X(12).
           03  ARCHIVE-FG                PIC X.
           03  GET-NEXT-FG               PIC X(02).
           03  FLEX-INQ-BUF        PIC 9(3).
           03  AUDIQ-WINDOW-HANDLE PIC X(10).

       SCREEN SECTION.
       COPY "LIBLP/AUDIQ_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AUDIQ" TO VDU-FORM-NAME.
      D    STOP MESS.

           MOVE AUDIQ-PROG-BUF TO PROG-BUF.

           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM GET-GPENV.

           MOVE TRANS-DATE TO HOLD-TRANS-DATE.

           MOVE SPACES TO ERRCD.
           MOVE BRANCH   TO LN-OWNBR.
           MOVE LOAN-KEY TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE "LOAN RECORD DELETED" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

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

           MOVE SOC-WORK TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "BORROWER RECORD MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF NOT IO-GOOD
              MOVE "INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           MOVE SPACES TO HOLD1-KEY-DOWN.
           MOVE "Y" TO FULL-PAGE.

      * LP FILE ISNT OPEN YET SO IN ORDER TO HAVE A GOOD HOLD KEY, USE
      * LN-OWNBR
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 99999     TO LP-SEQNO.
           MOVE LP1-KEY TO HOLD2-KEY-UP.

           PERFORM SEND-HEAD-2.
           PERFORM DISPLAY-ENTRY-UP.

           PERFORM SELECTION-MODULE.
           GO TO END-ROUTINE.

      **************************************************
       SELECTION-MODULE SECTION.
      **************************************************
       SELECTION-ENTRY.
           MOVE SPACES TO ROUTE-CD.
           MOVE 0 TO SUB1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  LEGEND-01    = "F1-BACK, F4-DATE, F9-INFO, ^/V, P-PRINT     :"
      *  LEGEND-01-GA = "F1-BACK, F4-DATE, F5-INQ, F9, ^/V, P-PRINT  :"
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-ORGST = "GA"
              MOVE LEGEND-01-GA TO MESS
              IF START-WITH-GAINQY AND GET-NEXT-FG = "GA"
                 MOVE SPACES     TO GET-NEXT-FG
                 MOVE LN-ACCTNO  TO GAINQ-ACCTNO
                 MOVE "WI/GAINQY" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH GAINQ-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 IF GAINQ-STAT NOT = SPACES
                    MOVE GAINQ-STAT TO GET-NEXT-FG
                    PERFORM FORM-RESET
                    MOVE 0 TO NEW-PROG-SUB
                 END-IF
                 GO TO SELECTION-MODULE-EXIT
              END-IF
           ELSE
              MOVE LEGEND-01 TO MESS.

           PERFORM SEND-LEGEND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
      *A15 DID CALL FRESET WHICH SET THIS BACK TO THE INITIAL VALUE OF " "
      *AND XFER AT END-ROUTINE
      *PRIOR TO ADDING THE MOVE OF SPACES, THE VALUE WAS ALWAYS "GA" IF YOU
      *WERE WORLD AND IN A LOOP 
      *    INQUIRY 
              MOVE SPACES TO GET-NEXT-FG
              PERFORM FORM-RESET
      *       CALL FRESET USING STAT
              MOVE 0 TO NEW-PROG-SUB
              GO TO SELECTION-MODULE-EXIT.
           IF VDUSTATUS = "12"
              PERFORM FORM-RESET
      *       CALL FRESET USING STAT
              MOVE 0 TO NEW-PROG-SUB
              MOVE VDUSTATUS TO GET-NEXT-FG
              GO TO SELECTION-MODULE-EXIT.

           IF ( F4-KEY ) PERFORM TOGGLE-ON-DATE
                         GO TO SELECTION-ENTRY.

           IF VDUSTATUS = "1("
              MOVE "CL/CLMENU" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATHNAME
                                   VDU-CLMENU-SELECTION
              CANCEL FORM-PROGX
              IF GET-NEXT-FG NOT = SPACES
                 MOVE GET-NEXT-FG TO GAINQ-STAT
                 PERFORM FORM-RESET
                 MOVE 0 TO NEW-PROG-SUB
              END-IF
              GO TO SELECTION-MODULE-EXIT.

       SKIP-F9-CHECK.
           IF ( LN-ORGST = "GA" )
              IF ( F5-KEY )
                 MOVE LN-ACCTNO TO GAINQ-ACCTNO
                 MOVE "WI/GAINQY" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH GAINQ-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 IF GAINQ-STAT NOT = SPACES
                    MOVE GAINQ-STAT TO GET-NEXT-FG
                    PERFORM FORM-RESET
                    MOVE 0 TO NEW-PROG-SUB
                 END-IF
                 GO TO SELECTION-MODULE-EXIT.

           IF VDUSTATUS = "1U"
              PERFORM SEND-HEAD-2
              PERFORM DISPLAY-ENTRY-UP
              GO TO SELECTION-ENTRY.
           IF VDUSTATUS = "1D"
              PERFORM SEND-HEAD-2
              PERFORM DISPLAY-ENTRY-DOWN
              GO TO SELECTION-ENTRY.

           GO TO SELECTION-ENTRY.

       SELECTION-MODULE-EXIT.
           EXIT.

      **************************************************
       DISPLAY-ENTRY-DOWN SECTION.
      **************************************************
           MOVE SPACES TO LINE-BUFFER LN-BUF.
           MOVE 0 TO LN-WK.
           MOVE "Y" TO TOGGLE-FIRST-TIME.
           MOVE HOLD1-KEY-DOWN TO LP1-KEY.
           IF LP-SEQNO = 99999
              MOVE 9 TO IO-FG
              GO TO DISPLAY-DOWN-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ***NOTE*** 'START-LP1-FILE-SCAN' MUST BE USED WHEN FIRST USING
      * 'START-LP1-FILE-NOT-GREATER'; USED WHEN DISPLAYING MULTIPLE
      * PAGES IN AN UP & DOWN FUNCTION; YOU NEED TO ADD ONE TO 
      * LP-SEQNO (IN WBEG) IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           IF NOT IO-GOOD
              MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                    QLP1-WEND-BRNO
              MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                    QLP1-WEND-ACCTNO
              COMPUTE QLP1-WBEG-SEQNO = LP-SEQNO + 1
              MOVE ALL "9"       TO QLP1-WEND-SEQNO
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( LP-BRNO = 9999 )
                 MOVE LP-BRNO    TO QLP1-WSBEG-BRNO
              ELSE
                 COMPUTE QLP1-WSBEG-BRNO = LP-BRNO + 1
              END-IF
   
              IF ( LP-ACCTNO = 99999999 )
                 MOVE LP-ACCTNO  TO QLP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QLP1-WSBEG-ACCTNO = LP-ACCTNO + 1
              END-IF

              PERFORM START-LP1-FILE-SCAN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO ENTRY-FLAG.

       READ-LPFILE1-NEXT.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO BYPASS-LPFILE1-READ.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-GOOD
              IF (LP-ACCTNO NOT = LN-ACCTNO) OR
                  (LP-BRNO NOT = LP-PATH-OWNBR)
                 ADD 1 TO ENTRY-FLAG
                 GO TO READ-LPFILE1-NEXT.
       BYPASS-LPFILE1-READ.
           ADD 1 TO LN-WK.
           IF IO-GOOD
              PERFORM SETUP-LINE
              MOVE LN-WK TO SUB
              PERFORM SET-TOGGLE-BUFFER.

           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACES TO LN-BUF.
           IF LN-WK = 8
              GO TO DISPLAY-XFER-DOWN.
           IF IO-GOOD
              GO TO READ-LPFILE1-NEXT.
           IF FULL-PAGE = "Y"
              MOVE "N" TO FULL-PAGE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY TO HOLD2-KEY-UP
              PERFORM DISPLAY-ENTRY-UP
              GO TO DISPLAY-DOWN-EXIT.
       DISPLAY-XFER-DOWN.
           PERFORM DISPLAY-ALL-LINES.
       DISPLAY-DOWN-EXIT.
           IF IO-GOOD
              MOVE "Y" TO FULL-PAGE
              MOVE LP1-KEY TO HOLD1-KEY-DOWN
           ELSE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY TO HOLD1-KEY-DOWN.
           PERFORM CLOSE-LP1-FILE.

      ********************************************
       DISPLAY-ENTRY-UP SECTION.
      ********************************************
           MOVE SPACES TO LINE-BUFFER LN-BUF.
           MOVE 9 TO LN-WK.
           MOVE "Y" TO TOGGLE-FIRST-TIME.

           MOVE HOLD2-KEY-UP TO LP1-KEY.
           IF LP-SEQNO = 0
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-EXIT.
           MOVE LP1-KEY TO HOLD1-KEY-DOWN.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE LP-SEQNO      TO QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           MOVE 0 TO ENTRY-FLAG.
       READ-LPFILE2-NEXT.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO BYPASS-LPFILE2-READ.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF (LP-ACCTNO NOT = LN-ACCTNO) OR
                            (LP-BRNO NOT = LP-PATH-OWNBR)
              IF IO-GOOD
                 ADD 1 TO ENTRY-FLAG
                 GO TO READ-LPFILE2-NEXT.
       BYPASS-LPFILE2-READ.
           SUBTRACT 1 FROM LN-WK.
           IF LN-WK < 1
              MOVE "Y" TO FULL-PAGE
              GO TO DISPLAY-XFER-UP.
           IF IO-GOOD
              PERFORM SETUP-LINE
              MOVE LN-WK TO SUB
              PERFORM SET-TOGGLE-BUFFER.

           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACES TO LN-BUF.
           IF IO-GOOD
              GO TO READ-LPFILE2-NEXT.
           IF FULL-PAGE = "Y"
              MOVE "N" TO FULL-PAGE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 0         TO LP-SEQNO
              MOVE LP1-KEY TO HOLD1-KEY-DOWN
              PERFORM DISPLAY-ENTRY-DOWN
              GO TO DISPLAY-UP-EXIT.
       DISPLAY-XFER-UP.
           PERFORM DISPLAY-ALL-LINES.
       DISPLAY-UP-EXIT.
           IF IO-GOOD
              MOVE "Y" TO FULL-PAGE
              MOVE LP1-KEY TO HOLD2-KEY-UP
           ELSE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 0         TO LP-SEQNO
              MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM CLOSE-LP1-FILE.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE SPACES TO LN-BUF.

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L-010
           ELSE
              MOVE LP-TRDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L-010.

           IF LP-PAYDATE NOT = LP-TRDATE
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE "."          TO L-DATE-SIGNAL
              ELSE
      * LOWER CASE T
                 MOVE X'74' TO L-DATE-SIGNAL.

           MOVE LP-TRCD TO L-020.

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
              ADD LP-APOTH LP-APOT2 GIVING L-050.

           ADD LP-APINT LP-APLC GIVING L-060.
           MOVE LP-APCUR TO L-070.
           IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                 (LP-TRCD = "RV" AND
                  (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL")))
               AND LP-IBPC = "P"
                   MOVE LP-TRAMT TO L-070.
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
              MOVE "*" TO L-120
           ELSE
              MOVE LP-PDTH-DATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-MM TO L-080-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE DATE-YYYYMMDD-DD TO L-080-DAYR
              ELSE
                 MOVE DATE-YYYYMMDD-YY TO L-080-DAYR
              END-IF
              COMPUTE L-090 = LP-OTHBAL + LP-OT2BAL
              ADD LP-INTBAL LP-LCBAL GIVING L-100
              MOVE LP-CURBAL TO L-110.


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

      *=================================================================
       NEXT-TOGGLE-ON-DATE.

           ADD 1 TO SUB.

           IF ( SUB > 8 )
              GO TO END-TOGGLE-ON-DATE.

           IF ( TOGGLE-PAYDATE   (SUB) NOT NUMERIC ) AND
              ( TOGGLE-TRANS-DATE(SUB) NOT NUMERIC )
              GO TO NEXT-TOGGLE-ON-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF TOGGLE-PAYDATE(SUB) NOT = TOGGLE-TRANS-DATE(SUB)
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE TOGGLE-PAYDATE   (SUB) TO T-DATE       (SUB)
                 MOVE "."                    TO T-DATE-SIGNAL(SUB)
              ELSE
                 MOVE TOGGLE-TRANS-DATE(SUB) TO T-DATE       (SUB)
      * LOWER CASE T
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

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY AUDIQ-SCREEN UPON AUDIQ-WINDOW-HANDLE.
           MOVE AUDIQ-FORM-FIELDS TO WR-BUF.
           MOVE AUDIQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE AUDIQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE AUDIQ-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AUDIQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-ALL-LINES.
           MOVE LINE-BUFFER TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SEND-HEAD-2.
           MOVE "S  A800HEAD:S." TO VDU.
           MOVE HEAD-2 TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           IF GAINQ-STAT NOT = SPACES
              MOVE GAINQ-STAT TO GET-NEXT-FG.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           IF HOLD-TRANS-DATE NOT = 0
              MOVE HOLD-TRANS-DATE TO TRANS-DATE.
           MOVE PROG-BUF TO AUDIQ-PROG-BUF.
           EXIT PROGRAM.
