       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WIPOFF.
       DATE-WRITTEN.    122391.
      **************************************************************************
      *          WINDOW LOAN PAYOFF
      *
      * REV:
      * 101794 BAH REMOVED ALL OCCURANCES OF LP2
      * 053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      * JTG 090495 ADDED TEST FOR GP-NO-LC-IN-ARREARAGE
      * BAH 011696 ADDED CRNOR2-LPTRCD, SET IT TO "PO" TO GET A TRUE
      *            PAYOFF
      * JTG 012596 CHANGED TO ALLOW COMPANY PAYOFF
      * JTG 013196 STOPPED GETTING PAYOFF NUMBERS FROM WORKFILE
      * MJD 071896 ADDED LOGIC FOR WEEKLY UNITPER.
      * BAH 121996 ADDED L2-BUF WITH A LINE DISPLAY THAT CAN HANDLE
      *            APCUR OF > 99999.99 FOR ADVANCED CREDIT, #421
      * JTG 060497 ADDED LOGIC FOR 'O3' REBATE
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * JTG 122297 ADDED SET OF CRNOR2-RECAST-FG #617 RE: IDEAL A48 TO A60
      * MJD 083398 FIXED DISPLAY TO INDICATE INCLUDED O2 BAL WITH
      *            "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      * BAH 990507 APOTH WAS TRUNCATING THOUSAND PLACE, REMOVED "REF" SINCE
      *            NOTHING GOES THERE ANYWAY !, TFC , PL #219
      * BAH 010726 L-040 (TRAMT) WAS ONLY Z(5).99, TRUNCATING LARGE NUMBERS,
      *            THERE WAS PLENTY OF ROOM, MADE IT Z(6).99- REGWAN PR# 1545
      * JTG 010801 EXPANDED SAVE-NO-REC-FIELDS TO MATCH NO-REC       LENDMARK
      * BAH 010808 ADDED F5-MEMO OPTION, TO CREATE A PAYOFF QUOTE LOAN MEMO,
      *            JIM, PR# 1488
      * JTG 011128 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      *            ADDED SETUP-LINE2
      * JTG 020801 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      * JTG 021009 MADE CALL TO REGGAP WINDOW TO DETERMINE IF ACCOUNT IS TOTAL
      *            LOSS AND IF 'Y' THEN ZERO OUT 'PP' REFUND        REGACC #0003
      * BAH 021230 REGACC PR# 13, PASSWORD PROTECT AGAINST ENTERING DATE 90
      *            DAYS BEFORE OR AFTER SYSTEM DATE.
      * JTG 031103 NEW ENTRY FOR 'BR-INC-INS = L' WHICH WILL MEAN
      *            TO PROMPT TO KEEP INSURANCE IN FORCE FOR
      *            CREDIT LIFE ONLY. ALSO ASK QUESTION NOT ONLY
      *            IN PAYOFF POSTING BUT ALSO IN INQUIRY       FIN.MANMENT #    
      * BAH 051018 ADDED ANOTHER F1 TOGGLE OPTION OF LEGAL: (1) SUPPRESS ALL
      *            INS REBATES, (2) FORCE INT REB OF # 5, (3) USE ALL OPTIONS
      *            EXCEPT DEDUCT AMOUNT, LENDMAKR PR# 2599
      * BAH 060913 IGNORE ALL SPR SPECIAL OPTION FORMULAS FOR F1 LEGAL TOGGLE,
      *            LENDMARK PR# 2839
      * BAH 070212 ONLY ALLOW LENDMARK TO HAVE THE NEW F1 LEGAL TOGGLE,
      *            WORLD PR# 525
      * BAH 071102 RESET TRANS-DATE WHEN INVALID PASSWORD IS ENTERED, IT WAS
      *            GIVING THE "WRONG" PASSWORD MESSAGE BUT THEN USING THAT
      *            DATE ON THE NEXT PAYOFF INQUIRY...REGACC PL# 608
      * BAH 080208 MAKE THE MEMO FLAGGED AS SYSTEM GENERATED, SUPPORT PR# 3153
      *  CS 080325 ADD GP-I-AM-CASHCO TO TEST FOR THE LEGAL PAYOFF. THEY USED
      *            IT BEFORE 2/12/07 CHANGE... OK PER MEMO FROM JR
      * BLM 100318 IF ACCOUNT HAS ESCROW & ESCROW BALANCE IS NOT 0, SEND A
      *            MESSAGE, REGWAN PR# 3506
      * BAH 150203 DO NOT ALLOW A PAYOFF DATE PRIOR TO LOAN DATE, PL#847 FINMGM
      * BAH 160128 SPLIT OUT LTFILE, PD0003
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      * BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181130 GLOBAL LMFILE
      **************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/WIPOFF S35 06/24/24-09:26:22 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01ES.CPY".
       COPY "LIBLP/LP01ES_SQL.CPY".
       COPY "LIBLP/LPWSES.CPY".

       01  HOLD-SP1-KEY        PIC X(9).              
       01  HOLD-LN-ADDON-FG    PIC X.

       COPY "LIBLP/LP01NO.CPY".

      **************************************************************
      *    THESE FIELDS HOLD NO-REC FIELDS FOR THE PAYOFF DATE,
      *    SINCE THE PAYOFF GOOD THRU ROUTINES, RECALCULATE NO-REC
      **************************************************************
       01  SAVE-NO-REC-FIELDS.
      *----TO DATE DUE FIGURES-----------------------
           03  SAVE-NO-TDUE-DATE        PIC 9(8)     COMP-3.
           03  SAVE-NO-TDUELC           PIC S9(7)V99 COMP-3.
           03  SAVE-NO-TDUEAMT          PIC S9(7)V99 COMP-3.
           03  SAVE-NO-TDUECONTR        PIC S9(7)V99 COMP-3.
      *----LAST DUE FIGURES--------------------------
           03  SAVE-NO-LDUE-DATE        PIC 9(8)     COMP-3.
           03  SAVE-NO-LDUELC           PIC S9(7)V99 COMP-3.
           03  SAVE-NO-LDUEAMT          PIC S9(7)V99 COMP-3.
           03  SAVE-NO-LDUECONTR        PIC S9(7)V99 COMP-3.
      *----NEXT DUE FIGURES--------------------------
           03  SAVE-NO-NXDUE-DATE       PIC 9(8)     COMP-3.
           03  SAVE-NO-NXDUEAMT         PIC S9(7)V99 COMP-3.
           03  SAVE-NO-NXDUELC          PIC S9(7)V99 COMP-3.
           03  SAVE-NO-NXDUECONTR       PIC S9(7)V99 COMP-3.

       01  WIPOFF-WINDOW-HANDLE  PIC X(10).
       01  ERRCD                 PIC X.
       01  REFRESH-SCREEN-LINES  PIC X.
       01  ONE                   PIC S9.
       01  CNT                   PIC 99.
       01  SUB                   PIC 9(3) COMP-3  VALUE 0.
       01  SUB1                  PIC 9(3) COMP-3  VALUE 0.
       01  SAVED-SUB1            PIC 9(3) COMP-3  VALUE 0.
       01  CUR-SUB               PIC 9(3) COMP-3  VALUE 0.
       01  NEWCUR-SUB            PIC 9(3) COMP-3  VALUE 0.
       01  CUST-COMPANY-TRCD     PIC XX           VALUE "PO".
       01  GET-PAYOFF-ONLY       PIC X            VALUE " ".
       01  DISP-REV-BAL          PIC X            VALUE "Y".
       01  TRANS-DATE            PIC 9(8).
       01  HOLD-WI-CL-TRANS-DATE PIC 9(8).

       01  SAVE-LM1-KEY         VALUE SPACES.
           03  SAVE-LM-BRNO     PIC 9(4).
           03  SAVE-LM-ACCTNO   PIC 9(8).
           03  SAVE-LM-DATE     PIC 9(8).
           03  SAVE-LM-SEQ      PIC 9(3).
       01  SAVE-LM-MEMO.
           03  MEMO-USERID      PIC X(10)  VALUE SPACES.
           03  FILLER           PIC X(17)  VALUE "QUOTED A PAYOFF: ".
           03  MEMO-DATE        PIC 99/99/99.
           03  FILLER           PIC X(2).
           03  MEMO-TRAMT       PIC Z(6).99.
       01  ESCROW-MSG.
           03  FILLER           PIC X(33) VALUE
               "ACCOUNT HAS AN ESCROW BALANCE OF ".
           03  D-ES-BALANCE     PIC $Z(6).99-.


       01  REBATE-DISP-TAB       PIC X(22)     VALUE
           "RCRARPRIRSRMRDRER1R2R3".
       01  REBATE-DISP-RED      REDEFINES REBATE-DISP-TAB.
           03  REB-TYPE          PIC XX  OCCURS 11.

       01  MIP-CL-MESS.
           03  FILLER           PIC X(4)  VALUE " (CL".
           03  MIP-CL           PIC ZZZZZZ9.99.
           03  FILLER           PIC X     VALUE ")".
       01  MIP-AH-MESS.
           03  FILLER           PIC X(4)  VALUE " (AH".
           03  MIP-AH           PIC ZZZZZZ9.99.
           03  FILLER           PIC X     VALUE ")".
       01  MIP-COLLECTED        PIC X(29) VALUE
           " MIP INSURANCE WAS COLLECTED!".

      **************************************************
      *         ORDER LINE BUFFER                      *
      **************************************************
       01  LINE-LIST.
           03  FILLER            PIC X(33)    VALUE
           "L01 L011   L013 L014   L015 L016 ".
           03  FILLER            PIC X(33)    VALUE
           "L02 L021   L023 L024   L025 L026 ".
           03  FILLER            PIC X(33)    VALUE
           "L03 L031   L033 L034   L035 L036 ".
           03  FILLER            PIC X(33)    VALUE
           "L04 L041   L043 L044   L045 L046 ".
           03  FILLER            PIC X(33)    VALUE
           "L05 L051   L053 L054   L055 L056 ".
           03  FILLER            PIC X(33)    VALUE
           "L06 L061   L063 L064   L065 L066 ".
           03  FILLER            PIC X(33)    VALUE
           "L07 L071   L073 L074   L075 L076 ".
           03  FILLER            PIC X(33)    VALUE
           "L08 L081   L083 L084   L085 L086.".
        01  LINE-LISTX           REDEFINES LINE-LIST.
            03  LINE-LISTXX      OCCURS 16.
                05  FILLER       PIC X(4).
                05  FILLER       PIC X(4).
                05  LINE-LISTX1  PIC XXX.
                05  FILLER       PIC X(5).
                05  FILLER       PIC X(7).
                05  FILLER       PIC X(5).
                05  FILLER       PIC X(5).

       01  LINE-BUFFER           VALUE " ".
           03  D-LINE            OCCURS 16.
               05  FILLER        PIC X(23).
               05  D-GOODTHRU    PIC 99/99/99.
               05  FILLER        PIC X(7).
               05  D-DUE-LN      PIC X(40).
       01  SAVED-LINE-BUFFER     PIC X(1248).

       01  LN-BUF              VALUE " ".
           03  L-010           PIC 9(6).
           03  FILLER          PIC X.
           03  L-020           PIC XX.
           03  FILLER          PIC X.
           03  L-040           PIC Z(6).99-   BLANK ZERO.
           03  L-040RATE      REDEFINES L-040
                               PIC ZZZZZ.999-.
           03  L-050           PIC Z(4).99-   BLANK ZERO.
           03  L-060           PIC Z(4).99-   BLANK ZERO.
           03  L-070           PIC Z(5)9.99-  BLANK ZERO.
           03 L-080-MO     PIC 99         BLANK ZERO.
           03 L-080-DAYR   PIC 99         BLANK ZERO.
           03  L-090           PIC Z(5).99-   BLANK ZERO.
           03  L-100           PIC Z(5).99-   BLANK ZERO.
           03  L-110           PIC Z(5)9.99-.
      * DISPLAY BUFFER USED IS PRINCIPAL IS OVER 99,999.99
      * LEFT OUT PAID THRU, DOESNT MATTER ON A PAYOFF
       01  LN2-BUF            REDEFINES LN-BUF.
           03  L2-010          PIC 9(6).
           03  FILLER          PIC X.
           03  L2-020          PIC XX.
           03  FILLER          PIC X.
           03  L2-030          PIC X(3).
           03  L2-040          PIC Z(7).99- BLANK ZERO.
           03  L2-050          PIC Z(4).99- BLANK ZERO.
           03  L2-060          PIC Z(5).99- BLANK ZERO.
           03  L2-070          PIC Z(6).99- BLANK ZERO.
      *    03  FILLER          PIC X.
           03  L2-090          PIC Z(5).99- BLANK ZERO.
           03  L2-100          PIC Z(4).99- BLANK ZERO.
           03  L2-110          PIC Z(5)9.99-.
           03  FILLER         REDEFINES L2-110.
               05  FILLER      PIC X(9).
               05  L2-120      PIC X.
       01  BALANCE-BUF        REDEFINES LN-BUF.
           03  FILLER          PIC X(30).
           03  B-080           PIC 9(4)       BLANK ZERO.
           03  B-090           PIC Z(5).99-   BLANK ZERO.
           03  B-100           PIC Z(5).99-   BLANK ZERO.
           03  B-110           PIC Z(5)9.99-.

       01  DUE-LINE            VALUE " ".
           03  FILLER          PIC X.
           03  DUE-PROMPT      PIC X(11).
           03  DUE-DATE-DUE    PIC 99/99/99   BLANK ZERO.
           03  DUE-LCAMT       PIC Z(6).ZZ.
           03  FILLER          PIC X.
           03  DUE-AMOUNT      PIC Z(6).ZZ.
           03  FILLER          PIC XX.

       01  GOOD-THRU-LINE      VALUE " ".
           03  FILLER          PIC X(23).
           03  GOOD-THRU       PIC 99/99/99.
       01  GOODTHRU-SUB        PIC 99.
       01  GOODTHRU-FG         PIC X           VALUE " ".
       01  SAVE-TRANS-DATE     PIC 9(8).
       01  SAVE-POFF-NETDUE    PIC S9(7)V99.

       01  LEGEND1.
           03  FILLER          PIC X(48)   VALUE
           "F7, F1-CUST/CO, F2-GOOD, F3-RN, F4-PB, F5-MEMO, ".
           03  FILLER          PIC X(18)   VALUE
           "ENTER PAYOFF DATE:".
       01  LEGEND1-LENDMARK.
           03  FILLER          PIC X(53)   VALUE
           "F7, F1-CUST/CO/LEGL, F2-GOOD, F3-RN, F4-PB, F5-MEMO, ".
           03  FILLER          PIC X(12)   VALUE
           "PAYOFF DATE:".
       01  LEGEND                PIC X(70).
       01  LEGEND2.
           03  FILLER          PIC X(46)   VALUE
           " F7, CREATE A PAYOFF QUOTE LOAN MEMO (Y/N) :  ".
           03  FILLER          PIC X(19)   VALUE SPACES.


       01  POFF-QUESTION1      PIC X(31)
           VALUE "ADD IN WAIVED LATE CHG (Y/N) : ".
       01  POFF-QUESTION2      PIC X(31)
           VALUE "KEEP CL  INS. IN FORCE (Y/N) : ".
       01  POFF-QUESTION3      PIC X(31)
           VALUE "KEEP A&H INS. IN FORCE (Y/N) : ".
       01  POFF-QUESTION4      PIC X(31)
           VALUE "KEEP PP  INS. IN FORCE (Y/N) : ".
       01  POFF-QUESTION5      PIC X(31)
           VALUE "KEEP OT1 INS. IN FORCE (Y/N) : ".
       01  POFF-QUESTION6      PIC X(31)
           VALUE "KEEP OT2 INS. IN FORCE (Y/N) : ".
       01  POFF-QUESTION7      PIC X(31)
           VALUE "KEEP OT3 INS. IN FORCE (Y/N) : ".
       01  POFF-QUESTION8      PIC X(31)
           VALUE "ADD IN WAIVED INTEREST (Y/N) : ".

       01  ANSWERS.
           03  POFF-ANSWER-03.
               05  POFF-ANSWER         PIC X OCCURS 8.
           03  FILLER                 REDEFINES POFF-ANSWER-03.
               05  POFF-ANSWER1        PIC X.
                   88  VALID-POFF-ANSWER1   VALUES 'Y' 'N'.
               05  POFF-ANSWER2        PIC X.
                   88  VALID-POFF-ANSWER2   VALUES 'Y' 'N'.
               05  POFF-ANSWER3        PIC X.
                   88  VALID-POFF-ANSWER3   VALUES 'Y' 'N'.
               05  POFF-ANSWER4        PIC X.
                   88  VALID-POFF-ANSWER4   VALUES 'Y' 'N'.
               05  POFF-ANSWER5        PIC X.
                   88  VALID-POFF-ANSWER5   VALUES 'Y' 'N'.
               05  POFF-ANSWER6        PIC X.
                   88  VALID-POFF-ANSWER6   VALUES 'Y' 'N'.
               05  POFF-ANSWER7        PIC X.
                   88  VALID-POFF-ANSWER7   VALUES 'Y' 'N'.
               05  POFF-ANSWER8        PIC X.
                   88  VALID-POFF-ANSWER8   VALUES 'Y' 'N'.

       01  WORK-EFFECT.
           03  WK-EFF              PIC X OCCURS 8.
       01  POFF-ANSWER-BUF         PIC X(12) VALUE
           "N  N  N  N  ".
       01  POFF-ANSWER-LIST        PIC X(16) VALUE
           "F42 F52 F62 F72.".
       01  POFF-WAIVE-MSG.
           03  FILLER              PIC X(20) VALUE
           "<-- TOTAL AMOUNT IS ".
           03  POFF-WAIVE-LIMIT    PIC ZZ,ZZ9.99.
       01  POFF-NO-WAIVE-MSG       PIC X(27) VALUE
           "THERE ARE NO WAIVED CHARGES".
       01  POFF-NO-INSURANCE-MSG   PIC X(27) VALUE
           "THIS INSURANCE NOT WRITTEN ".
       01  POFF-INSURANCE-REB-MSG  PIC X(27) VALUE
           "INSURANCE HAS BEEN REBATED ".
       01  WORK-LCWAIVERS          PIC S9(5)V99  COMP VALUE 0.
       01  WORK-INTWAIVERS         PIC S9(5)V99  COMP VALUE 0.
       01  WORK-XXWAIVERS          PIC S9(5)V99  COMP VALUE 0.
       01  WORK-REBATE-TAB.
           03  WORK-RB-TAB         PIC X OCCURS 13.
       01  SCR-ALL.
           03  SCR-1               PIC X(8).
           03  SCR-2               PIC 99.
           03  SCR-3               PIC XX.
       01  EXPIRED-RB-TAB          PIC X(8).
       01  FILLER                 REDEFINES EXPIRED-RB-TAB.
           03  EXPIRED-RB-FG       PIC X   OCCURS 8.
       01  ROUTE-CD                PIC XX.

       01  L025-BUF       PIC ZZZ,ZZZ.99-.
       01  L025-LIST      PIC X(5) VALUE "L025.".
       01  L035-BUF       PIC ZZZ,ZZZ.99-.
       01  L035-LIST      PIC X(5) VALUE "L035.".
       01  L045-BUF       PIC ZZZ,ZZZ.99-.
       01  L045-LIST      PIC X(5) VALUE "L045.".
       01  L055-BUF       PIC ZZZ,ZZZ.99-.
       01  L055-LIST      PIC X(5) VALUE "L055.".
       01  L065-BUF       PIC ZZZ,ZZZ.99-.
       01  L065-LIST      PIC X(5) VALUE "L065.".
       01  L075-BUF       PIC ZZZ,ZZZ.99-.
       01  L075-LIST      PIC X(5) VALUE "L075.".
       01  L085-BUF       PIC ZZZ,ZZZ.99-.
       01  L085-LIST      PIC X(5) VALUE "L085.".


      *COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  90-DAY-SEQ                   VALUE 01.

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBWI/LNRNEWW.CPY".
       COPY "LIBWI/LNPBRNW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/WIPOFF_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/WIPOFF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/WIPOFF_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WIPOFF" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
              AT SCREEN LINE 10 * VDU-WIN-L-OFFSET
                                * VDU-WIN-L-OFFSET
              AT SCREEN COL  2 * VDU-WIN-C-OFFSET * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
            COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    78 + VDU-WIN-W-MOD
                      LINES   13 + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS WIPOFF-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-WI-CL-TRANS-DATE TO HOLD-WI-CL-TRANS-DATE
                                        TRANS-DATE.

      * TEST ENVIRONMENT VARIABLE "RVLINES" TO DETERMINE WHETHER
      * OR NOT TO DISPLAY BALANCES ON REVERSALS.

           MOVE "RVLINES" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE "N" TO DISP-REV-BAL
           ELSE
              MOVE "Y" TO DISP-REV-BAL.

           PERFORM GET-GPENV.

           MOVE " " TO ERRCD.
           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE "LOAN RECORD DELETED" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.
           MOVE LN-ADDON-FG TO HOLD-LN-ADDON-FG.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           PERFORM OPEN-SP1-FILE.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           MOVE SP1-KEY TO HOLD-SP1-KEY.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

           IF (LN-ESCROW-FG = "Y" OR LN-ESCROW-FG = "I")
              PERFORM OPEN-ES1-FILE
              MOVE LN-ACCTNO TO ES-ACCTNO
              PERFORM READ-ES1-FILE
              PERFORM CLOSE-ES1-FILE
              IF IO-BAD
                 MOVE "ACCOUNT MISSING ESCROW RECORD!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "E" TO ERRCD
                 GO TO END-ROUTINE.

      *    IF (LN-ESCROW-FG = "Y" OR LN-ESCROW-FG = "I")
      *       MOVE "WI/ESINFO"   TO VDU-CTRL-R-PROG.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.

           MOVE " " TO ANSWERS.
           PERFORM ASK-PAYOFF-QUESTIONS.
           IF ROUTE-CD = "7"
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.
    
           IF POFF-ANSWER1 = "Y"
              ADD WORK-LCWAIVERS  TO LN-LCBAL.
           IF POFF-ANSWER8 = "Y"
              ADD WORK-INTWAIVERS TO LN-INTBAL.
    
           PERFORM SET-BAL-IBPC-LPREC.

       ENTER-NEW-PAYOFF-DATE-BEGIN.
           MOVE 0 TO CUR-SUB.

           PERFORM SEND-NEW-PAYOFF-DATE.
           IF (LN-ESCROW-FG = "Y" OR LN-ESCROW-FG = "I")
              IF ES-BALANCE NOT = 0
                 MOVE ES-BALANCE TO D-ES-BALANCE
                 MOVE ESCROW-MSG TO MESS
                 PERFORM SEND-MESS.
           MOVE LINE-BUFFER TO SAVED-LINE-BUFFER.
           MOVE SUB1 TO SAVED-SUB1.
           MOVE " " TO REFRESH-SCREEN-LINES.

       ENTER-NEW-PAYOFF-DATE.

           MOVE LEGEND1 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ER M060ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "10"
              IF CUST-COMPANY-TRCD = "PO"
                 MOVE "S  A024TITLE:F." TO VDU
                 MOVE "PB" TO CUST-COMPANY-TRCD
                 MOVE " LOAN PAYOFF (COMPANY)" TO VDUBUF
              ELSE
                 MOVE "S  A024TITLE." TO VDU
                 MOVE "PO" TO CUST-COMPANY-TRCD
                 MOVE " LOAN PAYOFF (CUSTOMER)" TO VDUBUF
              END-IF
              PERFORM SCREENIO
              GO TO ENTER-NEW-PAYOFF-DATE-BEGIN.

           IF VDUSTATUS = "12"
              IF GOODTHRU-FG = " "
                 MOVE "Y" TO GOODTHRU-FG
                 GO TO ENTER-NEW-PAYOFF-DATE-BEGIN
              ELSE
                 GO TO ENTER-NEW-PAYOFF-DATE.

           IF VDUSTATUS = "16"
              MOVE TRANS-DATE            TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/LNRNEW"          TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH LNRNEW-BUF
                                    EXIT-PATHNAME
              CANCEL FORM-PROGX
              MOVE HOLD-WI-CL-TRANS-DATE TO EXT-WI-CL-TRANS-DATE.

           IF VDUSTATUS = "18"
              MOVE TRANS-DATE            TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/LNPBRN"          TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH LNPBRN-BUF
                                    EXIT-PATHNAME
              CANCEL FORM-PROGX
              MOVE HOLD-WI-CL-TRANS-DATE TO EXT-WI-CL-TRANS-DATE.

      * F5-KEY
           IF VDUSTATUS = "1:"
              PERFORM CREATE-PAYOFF-LOAN-MEMO
              GO TO ENTER-NEW-PAYOFF-DATE.

           IF NOT (VDUSTATUS = "00" OR "1U" OR "1D")
              GO TO ENTER-NEW-PAYOFF-DATE.

           IF VDUNUM0 = 0 OR VDUSTATUS = "1U" OR "1D"
              IF REFRESH-SCREEN-LINES = "Y"
                 PERFORM DISPLAY-ALL-LINES
                 MOVE " " TO REFRESH-SCREEN-LINES
                 GO TO ENTER-NEW-PAYOFF-DATE
              END-IF
              IF SUB1 < 9
                 PERFORM DISPLAY-ALL-LINES
                 GO TO ENTER-NEW-PAYOFF-DATE
              END-IF
              IF SUB1 > 8
                 SUBTRACT 8 FROM SUB1
                 PERFORM SHIFT-UP-LINES
                 PERFORM DISPLAY-ALL-LINES
                 MOVE SAVED-LINE-BUFFER TO LINE-BUFFER
                 MOVE SAVED-SUB1 TO SUB1
                 MOVE "Y" TO REFRESH-SCREEN-LINES
                 GO TO ENTER-NEW-PAYOFF-DATE.

           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEW-PAYOFF-DATE.

      * MAKE SURE DATE IS NOT PRIOR TO LOANDATE PL#847
           IF LN-LOANDATE > NUM-DATE
              MOVE "PAYOFF DATE IS PRIOR TO LOAN DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEW-PAYOFF-DATE.

           MOVE NUM-DATE TO TRANS-DATE.

      * REGACC PR# 13, PASSWORD PROTECT AGAINST ENTERING DATE 90 DAYS
      *                BEFORE OR AFTER SYSTEM DATE.
      * NUM-DATE HAS NEW PAYOFF DATE IN IT
           MOVE HOLD-WI-CL-TRANS-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS > 90 OR ELAPSED-DAYS < -90
              MOVE "OUTSIDE 90 DAY RANGE" TO PASSWDW-PROMPT2
              MOVE 90-DAY-SEQ TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE HOLD-WI-CL-TRANS-DATE TO TRANS-DATE
                 GO TO ENTER-NEW-PAYOFF-DATE.

           GO TO ENTER-NEW-PAYOFF-DATE-BEGIN.


      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "WIPOFF" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **************************************
       SET-PAYOFF-LINES SECTION.
      **************************************
           PERFORM CLEAR-LP-FILE.

           MOVE TRANS-DATE TO LP-PAYDATE.
           MOVE LN-OTHBAL  TO LP-OTHBAL.
           MOVE LN-INTBAL  TO LP-INTBAL.
           MOVE LN-LCBAL   TO LP-LCBAL.
           MOVE LN-CURBAL  TO LP-CURBAL.

           MOVE 0 TO SUB SUB1.
           MOVE " " TO LINE-BUFFER.
       SET-PAYOFF-LINES-CONT.
           ADD 1 TO SUB.
           IF SUB < 12
              IF POFF-REBATE(SUB) NOT = 0
                 MOVE REB-TYPE(SUB) TO LP-TRCD
                 MOVE POFF-REBATE(SUB) TO LP-TRAMT LP-APCUR
                 SUBTRACT LP-APCUR FROM LP-CURBAL
               ELSE
                 GO TO SET-PAYOFF-LINES-CONT.
           IF SUB > 11
              MOVE CUST-COMPANY-TRCD TO LP-TRCD
              MOVE POFF-NETDUE       TO LP-TRAMT
              MOVE POFF-OTDUE        TO LP-APOTH
              MOVE POFF-LCDUE        TO LP-APLC
              MOVE POFF-INTDUE       TO LP-APINT
              SUBTRACT LP-APOTH LP-APINT LP-APLC FROM LP-TRAMT
                                           GIVING LP-APCUR
              IF LN-MIP-FG = "Y"
                 SUBTRACT POFF-LXM-MIP-CLPAID
                          POFF-LXM-MIP-AHPAID FROM LP-APCUR
              END-IF
              MOVE 0 TO LP-OTHBAL LP-INTBAL LP-LCBAL LP-CURBAL.
           PERFORM SETUP-LINE.
           ADD 1 TO SUB1.
           MOVE LN-BUF TO D-LINE(SUB1).
           IF SUB < 12
              GO TO SET-PAYOFF-LINES-CONT.

       SET-PAYOFF-LINES-EXIT.
           EXIT.

      *******************************************
       SHIFT-UP-LINES SECTION.
      *******************************************
           MOVE 0 TO CUR-SUB.
           MOVE SUB1 TO NEWCUR-SUB.
       SHIFT-UP-LINES-CONT.
           ADD 1 TO CUR-SUB NEWCUR-SUB.
           MOVE D-LINE(NEWCUR-SUB) TO D-LINE(CUR-SUB).
           IF CUR-SUB NOT = 8
              GO TO SHIFT-UP-LINES-CONT.

      *******************************************
       SET-AVAIL-CREDIT SECTION.
      *******************************************
           MOVE " " TO DUE-LINE.
           MOVE "AVAILABLE:"   TO DUE-PROMPT.
           MOVE NO-TDUE-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO DUE-DATE-DUE.
           MOVE NO-AVAILCREDIT TO DUE-AMOUNT.
           COMPUTE SUB1 = SUB1 + 2.
           MOVE DUE-LINE TO D-DUE-LN(SUB1).

      *******************************************
       SET-DUE-DAYS SECTION.
      *******************************************
           MOVE "LAST  DUE:"   TO DUE-PROMPT.
           MOVE SAVE-NO-LDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO DUE-DATE-DUE.
           MOVE SAVE-NO-LDUELC TO DUE-LCAMT.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE SAVE-NO-LDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE SAVE-NO-LDUEAMT TO DUE-AMOUNT.
           COMPUTE SUB1 = SUB1 + 2.
           MOVE DUE-LINE TO D-DUE-LN(SUB1).

           MOVE "TODAY DUE:"   TO DUE-PROMPT.
           MOVE SAVE-NO-TDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO DUE-DATE-DUE.
           MOVE SAVE-NO-TDUELC TO DUE-LCAMT.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE SAVE-NO-TDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE SAVE-NO-TDUEAMT TO DUE-AMOUNT.
           ADD 1 TO SUB1.
           MOVE DUE-LINE TO D-DUE-LN(SUB1).

           MOVE "NEXT  DUE:"    TO DUE-PROMPT.
           MOVE SAVE-NO-NXDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO DUE-DATE-DUE.
           MOVE SAVE-NO-NXDUELC TO DUE-LCAMT.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE SAVE-NO-NXDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE SAVE-NO-NXDUEAMT TO DUE-AMOUNT.
           ADD 1 TO SUB1.
           MOVE DUE-LINE TO D-DUE-LN(SUB1).

      *******************************************
       SET-GOOD-THRU SECTION.
      *******************************************
           MOVE "    PAYOFF GOOD THRU:" TO GOOD-THRU-LINE.
           MOVE TRANS-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO GOOD-THRU.
           MOVE GOOD-THRU-LINE TO D-LINE(SUB1).
           MOVE ":S"           TO LINE-LISTX1(SUB1).
           MOVE SUB1 TO GOODTHRU-SUB.

           MOVE DUE-LINE TO D-DUE-LN(SUB1).

      *******************************************
       WHEN-PAYOFF-GOOD SECTION.
      *******************************************
           MOVE TRANS-DATE  TO SAVE-TRANS-DATE.
           MOVE POFF-NETDUE TO SAVE-POFF-NETDUE.

           IF LN-LOANTYPE = "I"
                   OR POFF-INTDUE NOT = 0
              GO TO WHEN-PAYOFF-GOOD-EXIT.

           MOVE TRANS-DATE TO NUM-DATE.
      * TODAY IS PRIOR TO SCHEDULED PAY THIS MONTH:
           IF NUM-DA < LN-1STPYDA
              MOVE LN-1STPYDA TO NUM-DA
              MOVE NUM-DATE   TO TRANS-DATE
              MOVE "Y" TO GET-PAYOFF-ONLY
              PERFORM SET-NEW-PAYOFF-DATE-WORKERS
              IF POFF-NETDUE NOT = SAVE-POFF-NETDUE
                 MOVE -1 TO ONE
                 MOVE 0 TO CNT
                 PERFORM WHEN-PAYOFF-GOOD-AGAIN
                    UNTIL ( POFF-NETDUE = SAVE-POFF-NETDUE
                                  OR TRANS-DATE = SAVE-TRANS-DATE
                                    OR CNT > 31
                          )
                 IF CNT NOT > 31
                    GO TO WHEN-PAYOFF-GOOD-EXIT
                 ELSE
                    GO TO WHEN-PAYOFF-GOOD-CNT
              ELSE
                 MOVE +1 TO ONE
                 MOVE 0 TO CNT
                 PERFORM WHEN-PAYOFF-GOOD-AGAIN
                    UNTIL ( POFF-NETDUE NOT = SAVE-POFF-NETDUE
                                     OR CNT > 31
                          )
                 IF CNT NOT > 31
                    MOVE TRANS-DATE TO NDTE-DATE
                    MOVE -1 TO NDTE-HOLD
                    PERFORM INCREMENT-DAYS
                    MOVE NDTE-DATE TO TRANS-DATE
                    GO TO WHEN-PAYOFF-GOOD-EXIT
                 ELSE
                    GO TO WHEN-PAYOFF-GOOD-CNT.

      * TODAY IS ON OR AFTER SCHEDULED PAY THIS MONTH:
           IF NUM-DA NOT < LN-1STPYDA
              MOVE LN-1STPYDA TO NUM-DA
              MOVE NUM-DATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO TRANS-DATE
              MOVE "Y" TO GET-PAYOFF-ONLY
              PERFORM SET-NEW-PAYOFF-DATE-WORKERS
              IF POFF-NETDUE NOT = SAVE-POFF-NETDUE
                 MOVE -1 TO ONE
                 MOVE 0 TO CNT
                 PERFORM WHEN-PAYOFF-GOOD-AGAIN
                    UNTIL ( POFF-NETDUE = SAVE-POFF-NETDUE
                                  OR TRANS-DATE = SAVE-TRANS-DATE
                                    OR CNT > 31
                          )
                 IF CNT NOT > 31
                    GO TO WHEN-PAYOFF-GOOD-EXIT
                 ELSE
                    GO TO WHEN-PAYOFF-GOOD-CNT
              ELSE
                 MOVE +1 TO ONE
                 MOVE 0 TO CNT
                 PERFORM WHEN-PAYOFF-GOOD-AGAIN
                    UNTIL ( POFF-NETDUE NOT = SAVE-POFF-NETDUE
                                     OR CNT > 31
                          )
                 IF CNT NOT > 31
                    MOVE TRANS-DATE TO NDTE-DATE
                    MOVE -1 TO NDTE-HOLD
                    PERFORM INCREMENT-DAYS
                    MOVE NDTE-DATE TO TRANS-DATE
                    GO TO WHEN-PAYOFF-GOOD-EXIT
                 ELSE
                    IF TRANS-DATE > MDTE-DATE
                       GO TO WHEN-PAYOFF-GOOD-EXIT
                    ELSE
                       GO TO WHEN-PAYOFF-GOOD-CNT.

       WHEN-PAYOFF-GOOD-AGAIN.
           MOVE TRANS-DATE TO NDTE-DATE.
           MOVE ONE        TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO TRANS-DATE.
           MOVE "Y" TO GET-PAYOFF-ONLY.
           PERFORM SET-NEW-PAYOFF-DATE-WORKERS.
           ADD 1 TO CNT.

       WHEN-PAYOFF-GOOD-CNT.
           MOVE "PAYOFF DOES NOT CHANGE IN 30 DAYS!!!" TO MESS.
           PERFORM SEND-MESS.
      *    STRING "GOOD THRU DATE = " TRANS-DATE
      *           " IS INACCURATE!!, LOOP COUNT = " CNT
      *           DELIMITED BY SIZE INTO MESS.

       WHEN-PAYOFF-GOOD-EXIT.
           MOVE TRANS-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-GOODTHRU(GOODTHRU-SUB).
           MOVE SAVE-TRANS-DATE TO TRANS-DATE.

      ************************************************
       CREATE-PAYOFF-LOAN-MEMO SECTION.
      ************************************************

      * F7, CREATE A PAYOFF QUOTE LOAN MEMO (Y/N) :
           MOVE LEGEND2 TO LEGEND.
           PERFORM SEND-LEGEND.

       CREATE-PAYOFF-F5.
           MOVE "E RA060ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO EXIT-CREATE-PAYOFF-MEMO.
           IF VDUSTATUS NOT = "00"
              GO TO CREATE-PAYOFF-F5.
           IF VDUBUF = "N     "
              GO TO EXIT-CREATE-PAYOFF-MEMO.
           IF VDUBUF NOT = "Y     "
              GO TO CREATE-PAYOFF-F5.


           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE EXT-WI-CL-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - DATE-YYYYMMDD.
           MOVE 1             TO SAVE-LM-SEQ.
       CREATE-MEMO.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 0
              IF LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-MEMO
              ELSE
                 MOVE "MEMO NOT CREATED" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXIT-CREATE-PAYOFF-MEMO.

           MOVE SPACES TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.

           MOVE EXT-CONAME-USER-ID TO MEMO-USERID.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO MEMO-DATE.
           MOVE LP-TRAMT           TO MEMO-TRAMT.
           MOVE SAVE-LM-MEMO       TO LM-MEMO.
           MOVE "S"                TO LM-MEMO-TYPE.
           PERFORM WRITE-LM1-FILE.

           PERFORM CLOSE-LM1-FILE.
       EXIT-CREATE-PAYOFF-MEMO.
           EXIT.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE " " TO LN-BUF.
           IF LP-APCUR > 99999.99 OR LP-APCUR < -99999.99
              PERFORM SETUP-LINE2
              GO TO EXIT-SETUP-LINE.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010.
           MOVE LP-TRCD    TO L-020.
           MOVE LP-TRAMT   TO L-040.
           IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 OR ( (LP-TRCD = "RV") AND
                       (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                            OR "RB" OR "RO")
                    )
              )  AND (LP-APOT2 NOT = 0)
              ADD LP-APOTH LP-APOT2 GIVING L-050
              INSPECT L-050 REPLACING TRAILING " " BY "*"
           ELSE
               MOVE LP-APOTH TO L-050.
           ADD LP-APINT LP-APLC GIVING L-060.
           MOVE LP-APCUR TO L-070.
           IF LP-TRCD = "RV" OR LP-REV NOT = " "
              NEXT SENTENCE
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
              MOVE LP-CURBAL TO L-110.

       EXIT-SETUP-LINE.
           EXIT.

      *********************************
       SETUP-LINE2 SECTION.
      *********************************
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L2-010.
           MOVE LP-TRCD    TO L2-020.
           MOVE LP-REFNO   TO L2-030.
           MOVE LP-TRAMT   TO L2-040.
           ADD LP-APOTH LP-APOT2 GIVING L2-050.
           IF (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                  AND (LP-APOT2 NOT = 0)
              INSPECT L2-050 REPLACING TRAILING " " BY "*".
           ADD LP-APINT LP-APLC GIVING L2-060.
           MOVE LP-APCUR   TO L2-070.

           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                 NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                    DISP-REV-BAL = "N"
              MOVE "*" TO L2-120
           ELSE
              ADD LP-OTHBAL LP-OT2BAL GIVING L2-090
              ADD LP-INTBAL LP-LCBAL  GIVING L2-100
              MOVE LP-CURBAL TO L2-110
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                    NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
                 MOVE "*" TO L2-120.


      ****************************************
       SEND-NEW-PAYOFF-DATE SECTION.
      ****************************************
           MOVE "S  A040WAIT:F." TO VDU.
           MOVE "WAIT" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM SEND-LOAN-BAL-INFO.
           MOVE " " TO GET-PAYOFF-ONLY.
           PERFORM SET-NEW-PAYOFF-DATE-WORKERS.
           PERFORM SAVE-NO-REC-FIELDS.
           PERFORM SET-PAYOFF-LINES.
           PERFORM SET-AVAIL-CREDIT.
           IF GOODTHRU-FG = "Y"
              PERFORM SET-GOOD-THRU
              PERFORM WHEN-PAYOFF-GOOD.
           PERFORM SET-DUE-DAYS.
           PERFORM DISPLAY-ALL-LINES.
           MOVE "S  A040WAIT." TO VDU.
           PERFORM SCREENIO.

      * CL & AH MIP INSURANCE WAS COLLECTED
           IF LN-MIP-FG = "Y"
              IF (POFF-LXM-MIP-CLPAID NOT = 0) AND
                 (POFF-LXM-MIP-AHPAID NOT = 0)
                 MOVE POFF-LXM-MIP-CLPAID TO MIP-CL
                 MOVE POFF-LXM-MIP-AHPAID TO MIP-AH
                 STRING MIP-CL-MESS MIP-AH-MESS MIP-COLLECTED
                   DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
              ELSE
      * CL MIP INSURANCE WAS COLLECTED
              IF (POFF-LXM-MIP-CLPAID NOT = 0)
                 MOVE POFF-LXM-MIP-CLPAID TO MIP-CL
                 STRING MIP-CL-MESS MIP-COLLECTED
                   DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
              ELSE
      * AH MIP INSURANCE WAS COLLECTED
              IF (POFF-LXM-MIP-AHPAID NOT = 0)
                 MOVE POFF-LXM-MIP-AHPAID TO MIP-AH
                 STRING MIP-AH-MESS MIP-COLLECTED
                   DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS.

      ******************************************************************
       SET-NEW-PAYOFF-DATE-WORKERS SECTION.
      ******************************************************************
           MOVE EXT-WI-CL-ACCTNO   TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE         TO CRNOR2-SYSDATE.
           MOVE CUST-COMPANY-TRCD  TO CRNOR2-LPTRCD.
           MOVE GET-PAYOFF-ONLY    TO CRNOR2-GET-PAYOFF-ONLY.
           MOVE "Y"                TO CRNOR2-RECAST-FG.
           MOVE "N"                TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK
                                          NO-REC POFF-WORKERS
                                          LN-REC.
           CANCEL FORM-PROGX.

      **************************************************************
       SAVE-NO-REC-FIELDS SECTION.
      **************************************************************

      * SAVE NO-REC FIELDS FOR THE PAYOFF DATE,
      * SINCE THE PAYOFF GOOD THRU ROUTINES, RECALCULATE NO-REC

      * SAVE TO DATE DUE FIGURES:
           MOVE NO-TDUE-DATE  TO SAVE-NO-TDUE-DATE.
           MOVE NO-TDUELC     TO SAVE-NO-TDUELC.
           MOVE NO-TDUEAMT    TO SAVE-NO-TDUEAMT.
           MOVE NO-TDUECONTR  TO SAVE-NO-TDUECONTR.
      * SAVE LAST DUE FIGURES:
           MOVE NO-LDUE-DATE  TO SAVE-NO-LDUE-DATE.
           MOVE NO-LDUELC     TO SAVE-NO-LDUELC.
           MOVE NO-LDUEAMT    TO SAVE-NO-LDUEAMT.
           MOVE NO-LDUECONTR  TO SAVE-NO-LDUECONTR.
      * SAVE NEXT DUE FIGURES:
           MOVE NO-NXDUE-DATE TO SAVE-NO-NXDUE-DATE.
           MOVE NO-NXDUELC    TO SAVE-NO-NXDUELC.
           MOVE NO-NXDUEAMT   TO SAVE-NO-NXDUEAMT.
           MOVE NO-NXDUECONTR TO SAVE-NO-NXDUECONTR.


      *************************************************
       ASK-PAYOFF-QUESTIONS SECTION.
      *************************************************
           MOVE " "        TO WORK-EFFECT
                              LINE-BUFFER
                              EXPIRED-RB-TAB
                              ROUTE-CD.

           MOVE TRANS-DATE TO LP-PAYDATE.

           MOVE 8 TO SUB.
           IF LN-INSPREM(6) NOT = 0 AND LN-O3-REBATE = " "
              MOVE 6 TO ITRM-SUB
              MOVE LP-PAYDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO WK-EFF(6)
                 IF BR-INC-INS = "Y"
                    MOVE POFF-QUESTION7 TO D-LINE(SUB)
                    SUBTRACT 1 FROM SUB
                 END-IF
              ELSE
                 MOVE "Y" TO EXPIRED-RB-FG(6).
           IF LN-INSPREM(5) NOT = 0 AND LN-O2-REBATE = " "
              MOVE 5 TO ITRM-SUB
              MOVE LP-PAYDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO WK-EFF(5)
                 IF BR-INC-INS = "Y"
                    MOVE POFF-QUESTION6 TO D-LINE(SUB)
                    SUBTRACT 1 FROM SUB
                 END-IF
              ELSE
                 MOVE "Y" TO EXPIRED-RB-FG(5).
           IF LN-INSPREM(4) NOT = 0 AND LN-O1-REBATE = " "
              MOVE 4 TO ITRM-SUB
              MOVE LP-PAYDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO WK-EFF(4)
                 IF BR-INC-INS = "Y"
                    MOVE POFF-QUESTION5 TO D-LINE(SUB)
                    SUBTRACT 1 FROM SUB
                 END-IF
              ELSE
                 MOVE "Y" TO EXPIRED-RB-FG(4).
           IF LN-INSURED = "N" AND LN-INSTYPES = "N"
              GO TO ASK-BYPASS-INS.

           IF LN-INSPREM(3) NOT = 0 AND LN-PP-REBATE = " "
              MOVE 3 TO ITRM-SUB
              MOVE LP-PAYDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO WK-EFF(3)
                 IF BR-INC-INS = "Y"
                    MOVE POFF-QUESTION4 TO D-LINE(SUB)
                    SUBTRACT 1 FROM SUB
                 END-IF
              ELSE
                 MOVE "Y" TO EXPIRED-RB-FG(3).
           IF LN-INSPREM(2) NOT = 0 AND LN-AH-REBATE = " "
              MOVE 2 TO ITRM-SUB
              MOVE LP-PAYDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO WK-EFF(2)
                 IF BR-INC-INS = "Y"
                    MOVE POFF-QUESTION3 TO D-LINE(SUB)
                    SUBTRACT 1 FROM SUB
                 END-IF
              ELSE
                 MOVE "Y" TO EXPIRED-RB-FG(2).
           IF LN-INSPREM(1) NOT = 0 AND LN-CL-REBATE = " "
              MOVE 1 TO ITRM-SUB
              MOVE LP-PAYDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO WK-EFF(1)
                 IF BR-INC-INS = "Y" OR "L"
                    MOVE POFF-QUESTION2 TO D-LINE(SUB)
                    SUBTRACT 1 FROM SUB
                 END-IF
              ELSE
                 MOVE "Y" TO EXPIRED-RB-FG(1).

       ASK-BYPASS-INS.
           IF BR-INC-WAIVERS NOT = "Y"
              GO TO ASK-BYPASS-WAIVERS.

           IF LN-INTWAIVERS NOT = 0
              MOVE POFF-QUESTION8 TO D-LINE(SUB)
              SUBTRACT 1 FROM SUB.
           IF LN-LCWAIVERS NOT = 0
              MOVE POFF-QUESTION1 TO D-LINE(SUB)
              SUBTRACT 1 FROM SUB.

       ASK-BYPASS-WAIVERS.
           MOVE LINE-LIST TO WR-LIST.
           MOVE LINE-BUFFER TO WR-BUF.
           PERFORM CALL-WRFORM.

           IF SUB > 8
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.

           IF NOT (BR-INC-INS = "Y" OR "L")
              IF BR-INC-WAIVERS NOT = "Y"
                 GO TO ASK-PAYOFF-QUESTIONS-EXIT.

           MOVE LN-REBATE-TAB TO WORK-REBATE-TAB.
           MOVE "00" TO VDUSTATUS.

       ASK-PAYOFF-Q1.
           IF LN-LCWAIVERS = 0 OR BR-INC-WAIVERS NOT = "Y"
              IF VDUSTATUS = "1U"
                 ADD 1 TO SUB
                 GO TO ASK-PAYOFF-Q8
              ELSE
                 GO TO ASK-PAYOFF-Q8.
           IF BR-INC-WAIVERS NOT = "Y"
              GO TO ASK-PAYOFF-Q2.
           MOVE LN-LCWAIVERS TO POFF-WAIVE-LIMIT.
           PERFORM ASK-PAYOFF-READ-WAIVERS.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              GO TO ASK-PAYOFF-Q1.
           MOVE VDUBUF TO POFF-ANSWER1.
           IF POFF-ANSWER1 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER1
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER1
              PERFORM ASK-PAYOFF-INVALID
              GO TO ASK-PAYOFF-Q1.
           IF POFF-ANSWER1 = "N"
              PERFORM ASK-PAYOFF-CLEAR-WAIVERS
              GO TO ASK-PAYOFF-Q8.
       ASK-PAYOFF-Q1-PART2.
           MOVE "RN N072L" TO SCR-1.
           MOVE SUB        TO SCR-2.
      *    COMPUTE SCR-2 = SUB - 4.
      *    IF SCR-2 < 0
      *       ADD 10 TO SCR-2.
           MOVE "5." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 1 FROM SUB
              GO TO ASK-PAYOFF-Q1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ASK-PAYOFF-Q1-PART2.
           PERFORM FIND-WAIVER.
           MOVE WORK-XXWAIVERS TO WORK-LCWAIVERS.
           IF WORK-LCWAIVERS = 0
              SUBTRACT 1 FROM SUB
              MOVE "AMOUNT MUST BE GREATER THAN 0 IF ANSWERING 'Y'"
                  TO MESS
              PERFORM SEND-MESS
              GO TO ASK-PAYOFF-Q1.
           IF WORK-LCWAIVERS > LN-LCWAIVERS
              MOVE "EXCEEDS WAIVED CHARGES" TO MESS
              PERFORM SEND-MESS
              GO TO ASK-PAYOFF-Q1-PART2.

       ASK-PAYOFF-Q8.
           IF LN-INTWAIVERS = 0 OR BR-INC-WAIVERS NOT = "Y"
              IF VDUSTATUS = "1U"
                 IF LN-LCWAIVERS NOT = 0 AND POFF-ANSWER1 = "Y"
                    GO TO ASK-PAYOFF-Q1-PART2
                 ELSE
                 IF LN-LCWAIVERS NOT = 0
                    SUBTRACT 1 FROM SUB
                    GO TO ASK-PAYOFF-Q1
                 ELSE
                    ADD 1 TO SUB
                    GO TO ASK-PAYOFF-Q2
              ELSE
                 GO TO ASK-PAYOFF-Q2.
           IF BR-INC-WAIVERS NOT = "Y"
              GO TO ASK-PAYOFF-Q2.
           MOVE LN-INTWAIVERS TO POFF-WAIVE-LIMIT.
           PERFORM ASK-PAYOFF-READ-WAIVERS.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              IF LN-LCWAIVERS NOT = 0
                         AND POFF-ANSWER1 = "Y"
                 SUBTRACT 1 FROM SUB
                 GO TO ASK-PAYOFF-Q1-PART2
              ELSE
                 SUBTRACT 2 FROM SUB
                 GO TO ASK-PAYOFF-Q1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              GO TO ASK-PAYOFF-Q8.
           MOVE VDUBUF TO POFF-ANSWER8.
           IF POFF-ANSWER8 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER8
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER8
              PERFORM ASK-PAYOFF-INVALID
              GO TO ASK-PAYOFF-Q8.
           IF POFF-ANSWER8 = "N"
              PERFORM ASK-PAYOFF-CLEAR-WAIVERS
              GO TO ASK-PAYOFF-Q2.
       ASK-PAYOFF-Q8-PART2.
           MOVE "RN N072L" TO SCR-1.
           MOVE SUB        TO SCR-2.
      *    COMPUTE SCR-2 = SUB - 4.
      *    IF SCR-2 < 0
      *       ADD 10 TO SCR-2.
           MOVE "5." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 1 FROM SUB
              GO TO ASK-PAYOFF-Q8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ASK-PAYOFF-Q8-PART2.
           PERFORM FIND-WAIVER.
           MOVE WORK-XXWAIVERS TO WORK-INTWAIVERS.
           IF WORK-INTWAIVERS = 0
               SUBTRACT 1 FROM SUB
               MOVE "AMOUNT MUST BE GREATER THAN 0 IF ANSWERING 'Y'"
                  TO MESS
               PERFORM SEND-MESS
               GO TO ASK-PAYOFF-Q8.
           IF WORK-INTWAIVERS > LN-INTWAIVERS
              MOVE "EXCEEDS WAIVED INTEREST" TO MESS
              PERFORM SEND-MESS
              GO TO ASK-PAYOFF-Q8-PART2.

        ASK-PAYOFF-Q2.
           IF NOT (BR-INC-INS = "Y" OR "L")
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF LN-INSPREM(1) = 0
                    OR LN-CL-REBATE NOT = " "
                     OR WK-EFF(1) = " "
              IF VDUSTATUS = "1U"
                 IF LN-INTWAIVERS NOT = 0
                        AND POFF-ANSWER8 = "Y"
                          AND BR-INC-WAIVERS = "Y"
                    GO TO ASK-PAYOFF-Q8-PART2
                 ELSE
                    IF BR-INC-WAIVERS = "Y"
                       GO TO ASK-PAYOFF-Q8
                    ELSE
                       NEXT SENTENCE
              ELSE
                 GO TO ASK-PAYOFF-Q3.
           ADD 1 TO SUB.
           PERFORM ASK-PAYOFF-READ-YN.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              IF LN-INTWAIVERS NOT = 0
                      AND POFF-ANSWER8 = "Y"
                       AND BR-INC-WAIVERS = "Y"
                 SUBTRACT 1 FROM SUB
                 GO TO ASK-PAYOFF-Q8-PART2
              ELSE
                 IF BR-INC-WAIVERS = "N"
                    SUBTRACT 1 FROM SUB
                    GO TO ASK-PAYOFF-Q2
                 ELSE
                    SUBTRACT 2 FROM SUB
                    GO TO ASK-PAYOFF-Q8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              MOVE WORK-RB-TAB(1) TO LN-REBATE(1)
              GO TO ASK-PAYOFF-Q2.
           MOVE VDUBUF TO POFF-ANSWER2.
           IF POFF-ANSWER2 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER2
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER2
              PERFORM ASK-PAYOFF-INVALID
              MOVE WORK-RB-TAB(1) TO LN-REBATE(1)
              GO TO ASK-PAYOFF-Q2.
           IF POFF-ANSWER2 = "Y"
              MOVE "Y" TO LN-REBATE(1).

        ASK-PAYOFF-Q3.
           IF LN-INSPREM(2) = 0
                   OR LN-AH-REBATE NOT = " "
                    OR WK-EFF(2) = " "
                     OR BR-INC-INS = "L"
              IF VDUSTATUS = "1U"
                 MOVE WORK-RB-TAB(1) TO LN-REBATE(1)
                 GO TO ASK-PAYOFF-Q2
              ELSE
                 GO TO ASK-PAYOFF-Q4.
           ADD 1 TO SUB.
           PERFORM ASK-PAYOFF-READ-YN.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              MOVE WORK-RB-TAB(1) TO LN-REBATE(1)
              GO TO ASK-PAYOFF-Q2.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              MOVE WORK-RB-TAB(2) TO LN-REBATE(2)
              GO TO ASK-PAYOFF-Q3.
           MOVE VDUBUF TO POFF-ANSWER3.
           IF POFF-ANSWER3 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER3
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER3
              PERFORM ASK-PAYOFF-INVALID
              MOVE WORK-RB-TAB(2) TO LN-REBATE(2)
              GO TO ASK-PAYOFF-Q3.
           IF POFF-ANSWER3 = "Y"
              MOVE "Y" TO LN-REBATE(2).

        ASK-PAYOFF-Q4.
           IF LN-INSPREM(3) = 0
                    OR LN-PP-REBATE NOT = " "
                     OR WK-EFF(3) = " "
                      OR BR-INC-INS = "L"
              IF VDUSTATUS = "1U"
                 MOVE WORK-RB-TAB(2) TO LN-REBATE(2)
                 GO TO ASK-PAYOFF-Q3
              ELSE
                 GO TO ASK-PAYOFF-Q5.
           ADD 1 TO SUB.
           PERFORM ASK-PAYOFF-READ-YN.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              MOVE WORK-RB-TAB(2) TO LN-REBATE(2)
              GO TO ASK-PAYOFF-Q3.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              MOVE WORK-RB-TAB(3) TO LN-REBATE(3)
              GO TO ASK-PAYOFF-Q4.
           MOVE VDUBUF TO POFF-ANSWER4.
           IF POFF-ANSWER4 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER4
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER4
              PERFORM ASK-PAYOFF-INVALID
              MOVE WORK-RB-TAB(3) TO LN-REBATE(3)
              GO TO ASK-PAYOFF-Q4.
           IF POFF-ANSWER4 = "Y"
              MOVE "Y" TO LN-REBATE(3).

        ASK-PAYOFF-Q5.
           IF LN-INSPREM(4) = 0
                   OR LN-O1-REBATE NOT = " "
                    OR WK-EFF(4) = " "
                     OR BR-INC-INS = "L"
              IF VDUSTATUS = "1U"
                 MOVE WORK-RB-TAB(3) TO LN-REBATE(3)
                 GO TO ASK-PAYOFF-Q4
              ELSE
                 GO TO ASK-PAYOFF-Q6.
           ADD 1 TO SUB.
           PERFORM ASK-PAYOFF-READ-YN.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              MOVE WORK-RB-TAB(3) TO LN-REBATE(3)
              GO TO ASK-PAYOFF-Q4.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              MOVE WORK-RB-TAB(9) TO LN-REBATE(9)
              GO TO ASK-PAYOFF-Q5.
           MOVE VDUBUF TO POFF-ANSWER5.
           IF POFF-ANSWER5 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER5
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER5
              PERFORM ASK-PAYOFF-INVALID
              MOVE WORK-RB-TAB(9) TO LN-REBATE(9)
              GO TO ASK-PAYOFF-Q5.
           IF POFF-ANSWER5 = "Y"
              MOVE "Y" TO LN-REBATE(9).

        ASK-PAYOFF-Q6.
           IF LN-INSPREM(5) = 0
                    OR LN-O2-REBATE NOT = " "
                     OR WK-EFF(5) = " "
                      OR BR-INC-INS = "L"
              IF VDUSTATUS = "1U"
              MOVE WORK-RB-TAB(9) TO LN-REBATE(9)
                 GO TO ASK-PAYOFF-Q5
              ELSE
                 GO TO ASK-PAYOFF-Q7.
           ADD 1 TO SUB.
           PERFORM ASK-PAYOFF-READ-YN.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              MOVE WORK-RB-TAB(9) TO LN-REBATE(9)
              GO TO ASK-PAYOFF-Q5.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              MOVE WORK-RB-TAB(10) TO LN-REBATE(10)
              GO TO ASK-PAYOFF-Q6.
           MOVE VDUBUF TO POFF-ANSWER6.
           IF POFF-ANSWER6 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER6
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER6
              PERFORM ASK-PAYOFF-INVALID
              MOVE WORK-RB-TAB(10) TO LN-REBATE(10)
              GO TO ASK-PAYOFF-Q6.
           IF POFF-ANSWER6 = "Y"
              MOVE "Y" TO LN-REBATE(10).

        ASK-PAYOFF-Q7.
           IF LN-INSPREM(6) = 0
                    OR LN-O3-REBATE NOT = " "
                     OR WK-EFF(6) = " "
                      OR BR-INC-INS = "L"
              IF VDUSTATUS = "1U"
              MOVE WORK-RB-TAB(10) TO LN-REBATE(10)
                 GO TO ASK-PAYOFF-Q5
              ELSE
                 GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           ADD 1 TO SUB.
           PERFORM ASK-PAYOFF-READ-YN.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ASK-PAYOFF-QUESTIONS-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              MOVE WORK-RB-TAB(10) TO LN-REBATE(10)
              GO TO ASK-PAYOFF-Q6.
           IF NOT (VDUSTATUS = "00" OR "1D")
              SUBTRACT 1 FROM SUB
              MOVE WORK-RB-TAB(11) TO LN-REBATE(11)
              GO TO ASK-PAYOFF-Q7.
           MOVE VDUBUF TO POFF-ANSWER7.
           IF POFF-ANSWER7 = " "
              MOVE "S  A010L" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE "N" TO VDUBUF POFF-ANSWER7
              PERFORM SCREENIO.
           IF NOT VALID-POFF-ANSWER7
              PERFORM ASK-PAYOFF-INVALID
              MOVE WORK-RB-TAB(11) TO LN-REBATE(11)
              GO TO ASK-PAYOFF-Q7.
           IF POFF-ANSWER7 = "Y"
              MOVE "Y" TO LN-REBATE(11).
           GO TO ASK-PAYOFF-QUESTIONS-EXIT.

       ASK-PAYOFF-READ-WAIVERS.
           ADD 1 TO SUB.
           MOVE "S  A300L" TO SCR-1.
           MOVE SUB TO SCR-2.
           MOVE "6." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           MOVE POFF-WAIVE-MSG TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM ASK-PAYOFF-READ-YN.
       ASK-PAYOFF-CLEAR-WAIVERS.
           MOVE "S  A300L" TO SCR-1.
           MOVE SUB TO SCR-2.
           MOVE "6." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           MOVE " " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A110L" TO SCR-1.
           MOVE SUB        TO SCR-2.
           MOVE "5." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           MOVE " " TO VDUBUF.
           PERFORM SCREENIO.
       ASK-PAYOFF-READ-YN.
           MOVE "E RA010L" TO SCR-1.
           MOVE SUB     TO SCR-2.
           MOVE "4."    TO SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
       ASK-PAYOFF-INVALID.
           MOVE "ENTER 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
           SUBTRACT 1 FROM SUB.

       ASK-PAYOFF-QUESTIONS-EXIT.
           EXIT.

      *************************************************
       FIND-WAIVER SECTION.
      *************************************************
           IF SUB = 8
              MOVE VDUNUM2 TO WORK-XXWAIVERS L085-BUF
              MOVE L085-LIST TO WR-LIST
              MOVE L085-BUF TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
           IF SUB = 7
              MOVE VDUNUM2 TO WORK-XXWAIVERS L075-BUF
              MOVE L075-LIST TO WR-LIST
              MOVE L075-BUF TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
           IF SUB = 6
              MOVE VDUNUM2 TO WORK-XXWAIVERS L065-BUF
              MOVE L065-LIST TO WR-LIST
              MOVE L065-BUF TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
           IF SUB = 5
              MOVE VDUNUM2 TO WORK-XXWAIVERS L055-BUF
              MOVE L055-LIST TO WR-LIST
              MOVE L055-BUF TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
           IF SUB = 4
              MOVE VDUNUM2 TO WORK-XXWAIVERS L045-BUF
              MOVE L045-LIST TO WR-LIST
              MOVE L045-BUF TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
           IF SUB = 3
              MOVE VDUNUM2 TO WORK-XXWAIVERS L035-BUF
              MOVE L035-LIST TO WR-LIST
              MOVE L035-BUF TO WR-BUF
              PERFORM CALL-WRFORM
           ELSE
           IF SUB = 2
              MOVE VDUNUM2 TO WORK-XXWAIVERS L025-BUF
              MOVE L025-LIST TO WR-LIST
              MOVE L025-BUF TO WR-BUF
              PERFORM CALL-WRFORM.

      *************************************************
       SET-BAL-IBPC-LPREC SECTION.
      *************************************************
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL  TO LP-LCBAL.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-CURBAL TO LP-CURBAL.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPLPCL.CPY".


      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY WIPOFF-SCREEN UPON WIPOFF-WINDOW-HANDLE..
           MOVE WIPOFF-FORM-FIELDS TO WR-BUF.
           MOVE WIPOFF-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A024TITLE." TO VDU
           MOVE "PO" TO CUST-COMPANY-TRCD
           MOVE " LOAN PAYOFF (CUSTOMER)" TO VDUBUF
           PERFORM SCREENIO.
           MOVE WIPOFF-HEAD TO WR-BUF.
           MOVE "HEAD." TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE WIPOFF-HELP--FILE TO HELP-LINK-FILE.
           MOVE WIPOFF-HELP--DIR TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/WIPOFF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
       DISPLAY-ALL-LINES.
           MOVE LINE-LIST TO WR-LIST.
           MOVE LINE-BUFFER TO WR-BUF.
           PERFORM CALL-WRFORM.
       SEND-LOAN-BAL-INFO.
           MOVE "S  A080PODATE." TO VDU.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE SPACE TO BALANCE-BUF.
           MOVE PDTH-DATE TO B-080.
           COMPUTE B-090 = LN-OTHBAL + LN-OT2BAL.
           ADD LN-INTBAL LN-LCBAL GIVING B-100.
           MOVE LN-CURBAL TO B-110.
           MOVE "S  A000PODESC." TO VDU.
           MOVE BALANCE-BUF      TO VDUBUF.
           PERFORM SCREENIO.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-ES1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPESGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPES1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WIPOFF-SCREEN.
           DESTROY WIPOFF-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
