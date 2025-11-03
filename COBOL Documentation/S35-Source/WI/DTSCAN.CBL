       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DTSCAN.
      ******************************************************************
      *
      *  DEALER DETAIL WINDOW SCAN
      *
      *=================================================================
      * REV:
      * GLB  030116  ALLOW DEALER NO. OF ZERO
      * GLB  030117  MODIFY AUTO START FROM MAINTENANCE TO INCLUDE ACCT#
      * MJD 131122 A30 CONVERSION COMPLETED.  
      * BLM 160513 FIX FOR NEW DS-DATE FORMAT, PD0003
      * BAH 160615 CHANGE DT1-KEY, INCLUDE BRNO, 8 DIGIT DATE, 3 DIGIT CLASS
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * KEC 2017.0209 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          INCREASED PAGE-KEY2 FROM 30 TO 33 BYTES; DUE TO 
      *          ADDING DT-BRNO TO DT1-KEY WHICH IS IN DT3-KEY.
      * BAH 170417 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0608 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'DT' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 181029 GLOBAL DTFILE
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
           "@(#)WI/DTSCAN.CBL S35 04/16/24-15:36:21 >".

       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  DTSCAN-LINES          PIC 99  VALUE 17.
       01  DTSCAN-COLUMNS        PIC 99  VALUE 78.
       01  DTSCAN-BEGIN-Y        PIC 99  VALUE 2.
       01  DTSCAN-BEGIN-X        PIC 99  VALUE 4.

       01  ERRCD                    PIC X      VALUE SPACES.
       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  FIRST-TIME               PIC X      VALUE "Y".

       01  BATCHID.
           03  FILLER               PIC XX     VALUE "B-".
           03  BATCHID-MASK         PIC 9999999999.
       01  W-BATCHNO                PIC 9(10).
       01  W-BATCH             REDEFINES W-BATCHNO.
           03  W-BDLBR          PIC 9(4).
           03  W-BDLCL          PIC 99.
           03  W-BNUM           PIC 9(4).

       01  HOLD-DT1-KEY.
           03  HOLD-DT-BRNO           PIC 9(4).
           03  HOLD-DT-DLNO           PIC 9(4)V99.
           03  HOLD-DT-CLASS          PIC 9(3).
           03  HOLD-DT-ACCTNO         PIC 9(8).
           03  HOLD-DT-DATE           PIC 9(8).
           03  HOLD-DT-SEQ            PIC 9(3).

       01  HOLD-RSV                 PIC S9(8)V99.
       01  HOLD-RSVPRIN             PIC S9(8)V99.
       01  HOLD-RSVCHGS             PIC S9(8)V99.
       01  HOLD-RSVHELD             PIC S9(8)V99.
       01  HOLD-DUE                 PIC S9(8)V99.
       01  HOLD-PAID                PIC S9(8)V99.
       01  HOLD-PROCEEDS            PIC S9(8)V99.
       01  HOLD-UNPAID              PIC S9(8)V99.
       01  HOLD-UNPAIDM             PIC S9(8)V99.


       01  HOLD-TRDESC          PIC X(40).
       01  HOLD-DT-TRCD         PIC X(2)          VALUE " ".
           88  DEALER-ORIGINAL   VALUE "NW" "TX".
           88  MEMO-TRCD         VALUE "RV" "CK" "V " "DR" "DP" "DC".

       01  TRANS-DESC-TABLE.
      *    -----------------------------------
      *      ORIGINAL PROCEEDS
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "BF BALANCE FORWARD               ".
           03  FILLER           PIC X(33) VALUE
           "NW NEW ACCOUNT                   ".
           03  FILLER           PIC X(33) VALUE
      *    "RV REVERSAL (F6)                 ".
           "RV V O I D  ACCOUNT              ".
      *    -----------------------------------
      *      PROCEED CHECKS
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "CK CHECKS                       ".
           03  FILLER           PIC X(33) VALUE
           "V  VOIDED CHECKS                ".
      *    -----------------------------------
      *     DEALER RESERVES :
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "PR RESERVE PRIN PAYMENT          ".
           03  FILLER           PIC X(33) VALUE
           "PC RESERVE CHGS PAYMENT          ".
           03  FILLER           PIC X(33) VALUE
           "PP RESERVE PARTIC PAYMENT        ".
           03  FILLER           PIC X(33) VALUE
           "DR PAYMENT FROM RESERVE          ".
           03  FILLER           PIC X(33) VALUE
           "DC PAYMENT CHARGES RESERVE       ".
           03  FILLER           PIC X(33) VALUE
           "DP PMNT PARTIC.  RESERVE         ".
           03  FILLER           PIC X(33) VALUE
           "RR RECEIPT PRINCIPAL RSV         ".
           03  FILLER           PIC X(33) VALUE
           "RC RECEIPT CHARGES RSV           ".
           03  FILLER           PIC X(33) VALUE
           "RP RECEIPT PARTIC. RSV           ".
           03  FILLER           PIC X(33) VALUE
           "RD RECEIPT DLR PROCEEDS          ".
      *    -----------------------------------
      *     REBATES :
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "RI REBATE INTEREST               ".
           03  FILLER           PIC X(33) VALUE
           "RS REBATE SERVICE CHARGE         ".
           03  FILLER           PIC X(33) VALUE
           "RC REBATE C/LIFE INSURANCE       ".
           03  FILLER           PIC X(33) VALUE
           "RA REBATE A&H INSURANCE          ".
           03  FILLER           PIC X(33) VALUE
           "RP REBATE PP INSURANCE           ".
           03  FILLER           PIC X(33) VALUE
           "R1 REBATE OTHER 1 INSURANCE      ".
           03  FILLER           PIC X(33) VALUE
           "R2 REBATE OT2 INSURANCE          ".
           03  FILLER           PIC X(33) VALUE
           "R3 REBATE OT3 INSURANCE          ".
      *    -----------------------------------
      *     P&L TRANSACTIONS :
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "PL P&L ACTIVE                    ".
           03  FILLER           PIC X(33) VALUE
           "PI P&L INACTIVE                  ".
           03  FILLER           PIC X(33) VALUE
           "PW P&L WRITE OFF                 ".
      *    -----------------------------------
      *     PAYOFFS/RENEWALS:
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "PO PAYOFF                        ".
           03  FILLER           PIC X(33) VALUE
           "PB RENEWAL WITH PROCEEDS         ".
           03  FILLER           PIC X(33) VALUE
           "RN RENEWAL BALANCE ONLY          ".
           03  FILLER           PIC X(33) VALUE
           "RB RENEWAL BALANCE ONLY          ".
           03  FILLER           PIC X(33) VALUE
           "RO RENEWAL BALANCE ONLY          ".
           03  FILLER           PIC X(33) VALUE
           "SC SALES FIN CONVERSION          ".
      *    -----------------------------------
      *     INSURANCE CANCELS:
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "CC CANCEL CL INSURANCE           ".
           03  FILLER           PIC X(33) VALUE
           "CA CANCEL A&H INSURANCE          ".
           03  FILLER           PIC X(33) VALUE
           "CP CANCEL PP  INSURANCE          ".
           03  FILLER           PIC X(33) VALUE
           "C1 CANCEL OT1  INSURANCE         ".
           03  FILLER           PIC X(33) VALUE
           "C2 CANCEL OT2  INSURANCE         ".
           03  FILLER           PIC X(33) VALUE
           "C3 CANCEL OT3  INSURANCE         ".
           03  FILLER           PIC X(33) VALUE
           "DF DEFERMENT                     ".
      *    -----------------------------------
      *     WAIVER TRANSACTIONS:
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "WL WAIVE LATE CHARGE             ".
           03  FILLER           PIC X(33) VALUE
           "WI WAIVE INTEREST                ".
           03  FILLER           PIC X(33) VALUE
           "WO WAIVE OTHER                   ".
      *    -----------------------------------
      *     PAYMENTS NON RESERVE:
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "PY NORMAL PAYMENT                ".
           03  FILLER           PIC X(33) VALUE
           "PE PAYMENT EXCEPTION             ".
           03  FILLER           PIC X(33) VALUE
           "SS REPO SOLD                     ".
           03  FILLER           PIC X(33) VALUE
           "PA ALLOTMENT PAYMENT             ".
           03  FILLER           PIC X(33) VALUE
           "AH A&H PAYMENT                   ".
           03  FILLER           PIC X(33) VALUE
           "PN NON CASH PAYMENT              ".
           03  FILLER           PIC X(33) VALUE
           "PZ NO CASH/NO RECDEL PAYMENT     ".
      *    -----------------------------------
      *     ASSESMENTS:
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "OT ASSESS OTHER CHARGE           ".
           03  FILLER           PIC X(33) VALUE
           "AI ASSESS INTEREST CHARGE        ".
           03  FILLER           PIC X(33) VALUE
           "AL ASSESS LATE CHARGE            ".
      *    -----------------------------------
      *     MISC:
      *    -----------------------------------
           03  FILLER           PIC X(33) VALUE
           "JD JUDGEMENT FULL                ".
           03  FILLER           PIC X(33) VALUE
           "AC ACCOUNT ACCELERATED           ".
           03  FILLER           PIC X(33) VALUE
           "IR INTEREST CHANGE/REDUCE        ".
           03  FILLER           PIC X(33) VALUE
           "AP APPLY TO PRINCIPAL            ".
           03  FILLER           PIC X(33) VALUE
           "TX TFR PURCH TO SERVICE          ".
           03  FILLER           PIC X(33) VALUE
           "TP TFR SERV TO PURCHASE          ".
           03  FILLER           PIC X(33) VALUE
           "SJ JUDGEMENT SPLIT               ".
       01  TRANS-DESC-TABLE2 REDEFINES TRANS-DESC-TABLE.
           03  TRANS-CD-DESC-CODE OCCURS 58.
               05  TRANS-TRCD       PIC X(2).
               05  FILLER           PIC X.
               05  TRANS-TRDESC     PIC X(30).
       01  TRANS-TRMAX              PIC 9(3) VALUE 58.
       01  TRANS-TRSUB              PIC 9(3) VALUE 0.
       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-FIELD-NUM        REDEFINES FLDSEL-FIELD.
           03  FLDSEL-FIELD-N       PIC 99.
           03  FILLER               PIC XX.
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-T.
               05  FLDSEL-T-TRCD     PIC XX.
               05  FILLER            PIC X.
               05  FLDSEL-T-SEQ      PIC 999.
               05  FILLER            PIC X.
               05  FLDSEL-T-DATE     PIC 99/99/99.
               05  FILLER            PIC X.
               05  FLDSEL-T-DESC     PIC X(20).
               05  FLDSEL-T-DLNO     PIC ZZZ9.99.
               05  FILLER            PIC XX.
               05  FLDSEL-T-BRNO     PIC ZZZ9.
               05  FILLER            PIC XX.
               05  FLDSEL-T-AMT      PIC ZZZZZZ9.99-.
               05  FILLER            PIC X.
               05  FLDSEL-T-REFX.
                   07  FLDSEL-T-REF  PIC 9(6)      BLANK ZERO.
               05  FILLER            PIC XX.
               05  FLDSEL-T-CLASS    PIC 99.
               05  FILLER            PIC X.
               05  FLDSEL-T-ACCTNO   PIC Z(6)99.
               05  FLDSEL-T-REASON   PIC X(9).
           03  FLDSEL-P            REDEFINES FLDSEL-T.
               05  FLDSEL-P-PAYEE   PIC X(26).
               05  FLDSEL-P-AMT     PIC Z(7).99-.
               05  FILLER           PIC X.
               05  FLDSEL-P-TYPE    PIC XX.
               05  FILLER           PIC XX.
               05  FLDSEL-P-REF     PIC 9(6).
               05  FILLER           PIC XX.
               05  FLDSEL-P-REASON  PIC X(27).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  PT-LIST                  PIC X(17) VALUE "TYPE ATYPE 000:S.".
       01  PT-BUF.
           03  TYPE-SCAN            PIC X.
           03  TYPE-DESC            PIC X(7)  VALUE SPACES.
           03  HEAD-BUF             PIC X(77).


       01  DISPLAY-LIST.
           03  FILLER               PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
      *        TYPE 1  -  ALL DETAIL
               05  D-DETAIL.
                   07  D-T-TRCD     PIC XX.
                   07  FILLER       PIC X.
                   07  D-T-DATE     PIC 99/99/99.
                   07  FILLER       PIC X.
                   07  D-T-DESC     PIC X(19).
                   07  D-T-AMT      PIC Z(8).99-.
                   07  D-T-RSV      PIC Z(7).99-.
                   07  FILLER       PIC X.
                   07  D-T-CLASS    PIC 99.
                   07  D-T-CLASSX   PIC X.
                   07  D-T-NUMBER   PIC 9(6).
                   07  FILLER       PIC X.
                   07  D-T-CHECK-AREA.
                       09  D-T-PAID     PIC X.
                       09  FILLER       PIC XX.
                       09  D-T-CHECK    PIC Z(6)      BLANK ZERO.
                       09  D-T-CHECKX   PIC X.
                       09  FILLER       PIC X.
                       09  D-T-CKVOID   PIC X.
                   07  D-T-STATUS-AREA REDEFINES D-T-CHECK-AREA.
                       09  D-T-STATUS   PIC X.
                       09  D-T-STATUSX1 PIC X.
                       09  D-T-STATUSX2 PIC X(10).

      *        TYPE 2  -  BALANCES DUE
               05  D-DUE           REDEFINES D-DETAIL.
                   07  D-D-TRCD     PIC XX.
                   07  FILLER       PIC X.
                   07  D-D-DATE     PIC 99/99/99.
                   07  FILLER       PIC X.
                   07  D-D-NUMBER   PIC 9(6).
                   07  FILLER       PIC X.
                   07  D-D-STATUS   PIC XX.
                   07  D-D-PROC     PIC Z(7).99-.
                   07  D-D-REV      PIC XX.
                   07  D-D-PAID     PIC Z(7).99-.
                   07  D-D-DUE      PIC Z(7).99-.
                   07  FILLER       PIC X.
                   07  D-D-PAIDX    PIC X.
                   07  FILLER       PIC X.
                   07  D-D-REFX.
                       09  D-D-CKNO     PIC Z(6)      BLANK ZERO.
                       09  D-D-CKNOX    PIC X.
                       09  FILLER       PIC X.
                       09  D-D-CKDATE   PIC 99/99/99.
                       09  FILLER       PIC X.
                       09  D-D-CKVOID   PIC X.
                   07  D-D-BATCH-AREA REDEFINES D-D-REFX.
                       09  D-D-BATCHIDX PIC X(18).
                   07  D-D-PAID-AREA  REDEFINES D-D-REFX.
                       09  D-D-PAIDX2   PIC X(18).

               05  D-BAL           REDEFINES D-DETAIL.
                   07  D-B-ACCTNO       PIC Z(6).
                   07  FILLER           PIC X.
                   07  D-B-MO           PIC 99.
                   07  D-B-SLASH1       PIC X.
                   07  D-B-DA           PIC 99.
                   07  D-B-PROCEEDS     PIC Z(7).99-.
                   07  D-B-REV          PIC X.
                   07  D-B-PAID         PIC Z(7).99.
                   07  D-B-PAIDX        PIC X.
                   07  D-B-DUE          PIC Z(7).99-.
                   07  D-B-RSVPRIN      PIC Z(7).99-.
                   07  D-B-RSVCHGS      PIC Z(7).99-.
                   07  D-B-HELD         PIC Z(5).99-.

           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).
       01  NON-DISP-BUF.
           03  D1-LINE               OCCURS 9 TIMES.
               05  D-KEY-CLASS      PIC 99.
               05  D-KEY-ACCTNO     PIC 9(8).
               05  D-KEY-DATE       PIC 9(8).
               05  D-KEY-SEQ        PIC 9(3).

       01  DISPLAY-LIST2.
           03  FILLER               PIC X(15) VALUE
           "UNPAID UNPAIDM.".
       01  DISPLAY-BUF2.
           03  D-DS-UNPAID          PIC ZZZZ,ZZZ.99-.
           03  D-DS-UNPAIDM         PIC ZZZZ,ZZZ.99-.

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 20.

       01  PAGE-KEY.
           03  FILLER                   OCCURS 50 TIMES.
               05  PAGE-KEY2            PIC X(33).
               05  PAGE-KEY1.
                   07  PAGE-KEY-BRNO    PIC 9(4).
                   07  PAGE-KEY-DLNO    PIC 9(4)V99.
                   07  PAGE-KEY-CLASS   PIC 9(3).
                   07  PAGE-KEY-ACCTNO  PIC 9(8).
                   07  PAGE-KEY-DATE    PIC 9(8).
                   07  PAGE-KEY-SEQ     PIC 9(3).


       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBWI/DTSCAN_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       01 DTSCAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/DTSCAN_DEF.CPY".
       COPY "LIBWI/DTSCAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/DTSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/DTSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME DTSCAN-BUF EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "DTSCAN"      TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                    SCREEN LINE DTSCAN-BEGIN-X 
                    SCREEN COL DTSCAN-BEGIN-Y 
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE DTSCAN-COLUMNS
                   LINES DTSCAN-LINES
                   BLANK SCREEN
                   HANDLE IS DTSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE " "  TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO DTSCAN-STATUS.

           MOVE DTSCAN-DT1-KEY TO HOLD-DT1-KEY.
           IF DTSCANW-TYPE = 0
              MOVE 1 TO DTSCANW-TYPE.

           PERFORM SEND-HEADER-INFO.

           IF HOLD-DT-DLNO = 0
              GO TO ENTER-DEALER
           ELSE
              GO TO CONTINUE-DEALER.

      *************************************
      *    ENTER DTFILE SCAN TYPE
      *    1- DISPLAY ALL DETAIL RECORDS
      *    2- DISPLAY DUE TO DEALER (NW, PU)
      *    3- DISPLAY ALL BALANCES
      *               (PROCEEDS & RSV)
      *************************************
       ENTER-TYPE.
           MOVE "SN N010TYPE." TO VDU.
           MOVE DTSCANW-TYPE    TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           IF VDUNUM0 = 0
              MOVE "SN N010TYPE." TO VDU
              MOVE DTSCANW-TYPE TO VDUNUM0
              PERFORM SCREENIO.
           IF NOT (VDUNUM0 = 1 OR 2 OR 3)
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO DTSCANW-TYPE.

           PERFORM SEND-HEADER-INFO.


      *************************************
      *    ENTER DEALER NO.
      *************************************
       ENTER-DEALER.
           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "10"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              MOVE "WI/DLSCAN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF DLSCAN-STATUS NOT = "00"
                 GO TO ENTER-DEALER
              ELSE
                 MOVE DLSCAN-DLNO TO DL-DLNO VDUNUM2
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DEALER.

           IF VDUNUM2 = 0
              MOVE "SN N042DLNO." TO VDU
              MOVE HOLD-DT-DLNO TO VDUNUM2
              PERFORM SCREENIO.
      *    IF VDUNUM2 = 0
      *       GO TO ENTER-DEALER.
           MOVE VDUNUM2 TO HOLD-DT-DLNO.

       CONTINUE-DEALER.
           PERFORM OPEN-DL1-FILE.
           MOVE HOLD-DT-DLNO TO DL-DLNO.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE "MISSING DEALER...." TO DL-NAME
              MOVE SPACES               TO DL-ADR1
              MOVE 0                    TO DL-PHONE.

           IF HOLD-DT-CLASS = 0
              MOVE 0 TO HOLD-DT-ACCTNO HOLD-DT-DATE.
           IF HOLD-DT-ACCTNO = 0
              MOVE 0 TO HOLD-DT-DATE.
           MOVE 0 TO HOLD-DT-SEQ.

           PERFORM DISPLAY-DEALER-FIELDS.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              GO TO BEGIN-THE-SCAN.


      *************************************
       ENTER-DATE.
      *************************************
           MOVE LEGEND-03B TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-DEALER.
           IF VDUSTATUS = "10" GO TO ENTER-TYPE.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF STAT NOT = "00" AND NOT = "1D" GO TO ENTER-DATE.
           MOVE VDU-DATE-YYYYMMDD TO HOLD-DT-DATE.


      *************************************
       ENTER-ACCTNO.
      *************************************
           MOVE LEGEND-03 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N060ACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE.
           IF VDUSTATUS = "10" GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO HOLD-DT-ACCTNO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LN-CLASS.
           PERFORM CLOSE-LN1-FILE.
           MOVE LN-CLASS TO HOLD-DT-CLASS.

      *------------------------------------------------------------
      *  BEGIN DEALER DETAIL PAGE
      *------------------------------------------------------------
       BEGIN-THE-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           MOVE HOLD-DT-BRNO   TO PAGE-KEY-BRNO  (1).
           MOVE HOLD-DT-DLNO   TO PAGE-KEY-DLNO  (1).
           MOVE HOLD-DT-CLASS  TO PAGE-KEY-CLASS (1).
           MOVE HOLD-DT-ACCTNO TO PAGE-KEY-ACCTNO(1).
           MOVE HOLD-DT-DATE   TO PAGE-KEY-DATE  (1).
           MOVE HOLD-DT-SEQ    TO PAGE-KEY-SEQ   (1).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           PERFORM OPEN-DT1-FILE.
           MOVE SPACES TO DISPLAY-BUF
                          NON-DISP-BUF.

           MOVE 1 TO SUB.
           MOVE PAGE-KEY1(CURRENT-PAGE) TO DT1-KEY.

           PERFORM START-DT1-FILE.
            

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-DT1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9 OR (DT-BRNO NOT = DT-PATH-OWNBR)
              GO TO DISPLAY-PAGE.

           IF DT-DLNO NOT = HOLD-DT-DLNO
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF HOLD-DT-CLASS NOT = 0
              IF HOLD-DT-CLASS NOT = DT-CLASS
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           IF HOLD-DT-ACCTNO NOT = 0
              IF HOLD-DT-CLASS NOT = 0
                 IF HOLD-DT-ACCTNO NOT = DT-ACCTNO
                    MOVE 9 TO IO-FG
                    GO TO DISPLAY-PAGE.

           IF HOLD-DT-ACCTNO NOT = 0
              IF HOLD-DT-ACCTNO NOT = DT-ACCTNO
                 GO TO BUILD-PAGENO.

           IF HOLD-DT-DATE NOT = 0
              IF DT-DATE < HOLD-DT-DATE
                 GO TO BUILD-PAGENO.

           MOVE DT-TRCD TO HOLD-DT-TRCD.
           PERFORM FIND-TRANS-DESCRIPTION.

           IF DTSCANW-TYPE = 2
              IF NOT (DEALER-ORIGINAL)
                 GO TO BUILD-PAGENO.
           IF DTSCANW-TYPE = 3
              IF NOT (DEALER-ORIGINAL)
                 GO TO BUILD-PAGENO.

           IF DTSCANW-TYPE = 1 GO TO DISPLAY-TYPE1.
           IF DTSCANW-TYPE = 2 GO TO DISPLAY-TYPE2.
           IF DTSCANW-TYPE = 3 GO TO DISPLAY-TYPE3.

      *------- DISPLAY ALL DETAIL ---------
       DISPLAY-TYPE1.
           MOVE DT-TRCD       TO D-T-TRCD(SUB).
           MOVE DT-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-T-DATE(SUB).
           PERFORM DISPLAY-TRCD.
           MOVE HOLD-TRDESC  TO D-T-DESC(SUB).
           MOVE DT-CLASS      TO D-T-CLASS(SUB).
           MOVE "-"           TO D-T-CLASSX(SUB).
           MOVE DT-ACCTNO     TO D-T-NUMBER(SUB).
           IF DT-TRCD = "CK"
              OR DT-TRCD = "V "
                 MOVE DT-PAYMENTS   TO D-T-AMT(SUB)
           ELSE
                 MOVE DT-PROCEEDS      TO D-T-AMT(SUB).
           COMPUTE HOLD-RSV =
                    DT-DLRVPRIN(1) + DT-DLRVPRIN(2) + DT-DLRVPRIN(3)
                     + DT-DLRVCHGS(1) + DT-DLRVCHGS(2) + DT-DLRVCHGS(3)
                     + DT-DLPARVHELD.
           MOVE HOLD-RSV         TO D-T-RSV(SUB).

           MOVE DT-PROCEEDS   TO HOLD-DUE.
           IF DT-REVERSE = "Y"
              MOVE 0 TO HOLD-DUE.
           COMPUTE HOLD-DUE = HOLD-DUE - DT-PAYMENTS.

           IF DT-TRCD = "RV"
              GO TO DISPLAY-ALL-TYPES.

           IF (DEALER-ORIGINAL)
           IF DT-REVERSE = "Y"
                    IF HOLD-DUE = 0
                       MOVE "VOID COMPLETE" TO D-T-STATUS-AREA(SUB)
                       GO TO DISPLAY-ALL-TYPES.

           MOVE DT-STATUS        TO D-T-STATUS(SUB).
           MOVE "-"              TO D-T-STATUSX1(SUB).
           IF DT-STATUS = "R"
                    IF DT-BATCHID NOT = 0
                       MOVE DT-BATCHID TO BATCHID-MASK
                       MOVE BATCHID-MASK TO D-T-STATUSX2(SUB)
                    ELSE
                       MOVE "UNPRINTED "    TO D-T-STATUSX2(SUB).
           IF DT-STATUS = "P"
                    MOVE DT-PAYMENTS   TO D-T-AMT(SUB)
                    MOVE DT-PAID       TO D-T-PAID(SUB)
                    MOVE DT-CHECK-VOID TO D-T-CKVOID(SUB)
                    MOVE DT-CKNO       TO D-T-CHECK(SUB)
                    MOVE "#"           TO D-T-CHECKX(SUB).
           IF DT-STATUS = "V"
                    MOVE DT-PAYMENTS   TO D-T-AMT(SUB)
                    MOVE DT-PAID       TO D-T-PAID(SUB)
                    MOVE DT-CHECK-VOID TO D-T-CKVOID(SUB)
                    MOVE DT-CKNO       TO D-T-CHECK(SUB)
                    MOVE "#"           TO D-T-CHECKX(SUB).
           IF DT-STATUS = "C"
                    IF DT-CKNO NOT = 0
                       MOVE DT-PAYMENTS   TO D-T-AMT(SUB)
                       MOVE DT-PAID       TO D-T-PAID(SUB)
                       MOVE DT-CHECK-VOID TO D-T-CKVOID(SUB)
                       MOVE DT-CKNO       TO D-T-CHECK(SUB)
                       MOVE "#"           TO D-T-CHECKX(SUB)
                    ELSE
                        MOVE "RECEIPT DL" TO D-T-STATUSX2(SUB).


           IF DT-STATUS = " "
                    IF DT-PAID = "B"
                       MOVE "B" TO D-T-STATUS(SUB)
                       MOVE "UNRELEASED" TO D-T-STATUSX2(SUB)
                    END-IF
                    IF DT-PAID = "S"
                       MOVE "S" TO D-T-STATUS(SUB)
                       MOVE "UNPRINTED " TO D-T-STATUSX2(SUB).

           GO TO DISPLAY-ALL-TYPES.

      *------- DISPLAY DUE AMOUNTS ---------
       DISPLAY-TYPE2.
           MOVE DT-TRCD       TO D-D-TRCD(SUB).
           MOVE DT-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-D-DATE(SUB).
           IF DT-REVERSE = "Y"
              MOVE "V"        TO D-D-REV(SUB).
           MOVE DT-ACCTNO     TO D-D-NUMBER(SUB).
           MOVE DT-STATUS     TO D-D-STATUS (SUB).
           MOVE DT-PROCEEDS   TO D-D-PROC(SUB).
           MOVE DT-PAYMENTS   TO D-D-PAID (SUB).
           MOVE DT-PROCEEDS   TO HOLD-DUE.
           IF DT-REVERSE = "Y"
              MOVE 0 TO HOLD-DUE.
           COMPUTE HOLD-DUE = HOLD-DUE - DT-PAYMENTS.
           MOVE HOLD-DUE      TO D-D-DUE(SUB).
           MOVE DT-PAID       TO D-D-PAIDX  (SUB).
           MOVE DT-CHECK-VOID TO D-D-CKVOID(SUB).

           IF DT-STATUS = " "
              IF DT-REVERSE= "Y"
                 IF HOLD-DUE = 0
                    MOVE " -VOID COMPLETED- " TO D-D-PAIDX2(SUB)
                 END-IF
                 IF HOLD-DUE < 0
                    MOVE "  NEED VOID CHECK?" TO D-D-PAIDX2(SUB)
                 END-IF
                 GO TO DISPLAY-ALL-TYPES.

           IF DT-STATUS = "R"
              IF DT-BATCHID NOT = 0
                 MOVE DT-BATCHID TO BATCHID-MASK
                 MOVE BATCHID-MASK TO D-D-BATCHIDX(SUB)
              ELSE
                 MOVE "-SYS CHECK REQUEST"    TO D-D-BATCHIDX(SUB).
           IF DT-STATUS = "P"
              MOVE DT-CKNO       TO D-D-CKNO(SUB)
              MOVE "#"           TO D-D-CKNOX(SUB)
              MOVE DT-CKDATE     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY   TO D-D-CKDATE(SUB).
           IF DT-STATUS = " "
              IF DT-PAID = "S"
                 MOVE " -UNPRINTED CHECK-"    TO D-D-BATCHIDX(SUB).
           IF DT-STATUS = " "
              IF DT-PAID = "B"
                 IF DT-BATCHID NOT = 0
                    MOVE DT-BATCHID TO BATCHID-MASK
                    MOVE BATCHID-MASK TO D-D-BATCHIDX(SUB)
                 ELSE
                    MOVE "-UNRELEASED BATCH-" TO D-D-BATCHIDX(SUB).

           GO TO DISPLAY-ALL-TYPES.


      *------- DISPLAY BALANCES ---------
       DISPLAY-TYPE3.
           MOVE DT-ACCTNO       TO D-B-ACCTNO(SUB).
           MOVE DT-DATE         TO NUM-DATE.
           MOVE NUM-MO          TO D-B-MO(SUB).
           MOVE "/"             TO D-B-SLASH1(SUB).
           MOVE NUM-DA          TO D-B-DA(SUB).
           MOVE DT-PROCEEDS     TO D-B-PROCEEDS(SUB).
           COMPUTE HOLD-PROCEEDS =  DT-PROCEEDS.
           COMPUTE HOLD-RSVPRIN  =
                  (DT-DLRVPRIN(1) + DT-DLRVPRIN(2) + DT-DLRVPRIN(3) ).
           COMPUTE HOLD-RSVCHGS =
                  (DT-DLRVCHGS(1) + DT-DLRVCHGS(2) + DT-DLRVCHGS(3) ).
           COMPUTE HOLD-RSVHELD = DT-DLPARVHELD.
           IF DT-REVERSE = "Y"
              MOVE 0          TO HOLD-PROCEEDS HOLD-RSVPRIN
                                 HOLD-RSVCHGS  HOLD-RSVHELD
              MOVE "V"        TO D-B-REV(SUB).
           COMPUTE HOLD-PAID = DT-RELEASED + DT-PAYMENTS.
           MOVE HOLD-PAID      TO D-B-PAID(SUB).
           MOVE DT-PAID        TO D-B-PAIDX(SUB).
           COMPUTE HOLD-DUE  = HOLD-PROCEEDS - HOLD-PAID.
           MOVE HOLD-DUE       TO D-B-DUE(SUB).

           COMPUTE HOLD-RSVPRIN  =  HOLD-RSVPRIN - DT-DLRVPRIN-PAY.
           MOVE HOLD-RSVPRIN        TO D-B-RSVPRIN(SUB).

           COMPUTE HOLD-RSVCHGS =   HOLD-RSVCHGS - DT-DLRVCHGS-PAY.
           MOVE HOLD-RSVCHGS        TO D-B-RSVCHGS(SUB).

           COMPUTE HOLD-RSVHELD =   HOLD-RSVHELD - DT-DLPARVHELD-PAY.
           MOVE HOLD-RSVHELD        TO D-B-HELD(SUB).

           GO TO DISPLAY-ALL-TYPES.


       DISPLAY-ALL-TYPES.
           MOVE DT-CLASS          TO D-KEY-CLASS(SUB).
           MOVE DT-ACCTNO         TO D-KEY-ACCTNO(SUB).
           MOVE DT-DATE           TO D-KEY-DATE(SUB).
           MOVE DT-SEQ            TO D-KEY-SEQ(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO D-T-DESC(1)
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-DATE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE DT1-KEY TO PAGE-KEY1(PAGE-COUNT)
                    MOVE DT3-KEY TO PAGE-KEY2(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF CURRENT-PAGE = 1
              PERFORM DISPLAY-DEALER-STATS.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-04 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.
      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              GO TO ENTER-ACCTNO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-TYPE.
           IF NOT (FLDSEL-STAT = "00")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE  HOLD-DT-DLNO                     TO  DTSCANW-DLNO.
           MOVE  D-KEY-CLASS(FLDSEL-FIELD-N)   TO  DTSCAN-CLASS.
           MOVE  D-KEY-ACCTNO(FLDSEL-FIELD-N)  TO  DTSCAN-ACCTNO.
           MOVE  D-KEY-DATE(FLDSEL-FIELD-N)    TO  DTSCAN-DATE.
           MOVE  D-KEY-SEQ(FLDSEL-FIELD-N)     TO  DTSCAN-SEQ.

           MOVE "00" TO DTSCAN-STATUS.

           GO TO END-ROUTINE.

      ****************************************
       DISPLAY-DEALER-FIELDS SECTION.
      ****************************************
           MOVE "SN N042DLNO." TO VDU.
           MOVE DL-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "S  A000DLNAME." TO VDU.
           MOVE DL-NAME TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000DLADR1." TO VDU.
           MOVE DL-ADR1 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE DL-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE "S  A000DLPHONE." TO VDU.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *    DISPLAY THE DESCRIPTION OF THE TRANSACTION CODE.
      *    MEMO:
      *            DT-TRCD = "RR"  + DT-DLRVPRIN(1) (2)
      *            DT-TRCD = "DR"  - DT-DLRVPRIN(1) (2)
      *
      *            DT-TRCD = "RC"  + DT-DLRVCHGS(1) (2)
      *            DT-TRCD = "DC"  - DT-DLRVCHGS(1) (2)
      *
      *            DT-TRCD = "RP"  + DT-DLPARVHELD(1) (2)
      *            DT-TRCD = "DP"  - DT-DLPARVHELD(1) (2)
      ******************************************************************
       DISPLAY-TRCD SECTION.

           IF (DEALER-ORIGINAL)
              GO TO EXIT-DISPLAY-TRCD.

           IF ( DT-TRCD = "BF" )
              GO TO EXIT-DISPLAY-TRCD.

      * NEW MEMO TYPE DT'S

           IF ( DT-TRCD = "CK" )
                 MOVE "CHECK" TO HOLD-TRDESC
              IF DT-BATCHID NOT = 0
                 MOVE "BATCH ID: &&&&&&&&&&" TO HOLD-TRDESC
                 MOVE DT-BATCHID TO W-BATCHNO
                 INSPECT HOLD-TRDESC REPLACING FIRST "&&&&&&&&&&"
                    BY W-BATCHNO
              END-IF
              GO TO EXIT-DISPLAY-TRCD.
           IF ( DT-TRCD = "V " )
                 MOVE "VOID CHECK" TO HOLD-TRDESC
              GO TO EXIT-DISPLAY-TRCD.


           IF ( DT-TRCD = "RV" )
                 MOVE "DELETE ACCOUNT" TO HOLD-TRDESC
                 IF DT-REVERSE-TRCD NOT = "NW"
                    MOVE SPACES TO HOLD-TRDESC
                    STRING "REVERSE " DT-REVERSE-TRCD
                       DELIMITED BY SIZE INTO HOLD-TRDESC
                 END-IF
              GO TO EXIT-DISPLAY-TRCD.


       EXIT-DISPLAY-TRCD.
           EXIT.

      *************************************************
       FIND-TRANS-DESCRIPTION SECTION.
      *************************************************

           MOVE 0 TO TRANS-TRSUB.
           MOVE SPACES TO HOLD-TRDESC.
       NEXT-TRANS-CODE.
           ADD 1 TO TRANS-TRSUB.
           IF TRANS-TRCD(TRANS-TRSUB) = HOLD-DT-TRCD
              MOVE TRANS-TRDESC(TRANS-TRSUB) TO HOLD-TRDESC
              GO TO EXIT-GET-TRANS-DESC.
           IF TRANS-TRSUB < TRANS-TRMAX
              GO TO NEXT-TRANS-CODE.
           COMPUTE TRANS-TRSUB = TRANS-TRMAX + 1.
       EXIT-GET-TRANS-DESC.
           EXIT.

      *************************************************
       DISPLAY-DEALER-STATS SECTION.
      *************************************************
           MOVE 0 TO HOLD-UNPAID HOLD-UNPAIDM.

           PERFORM OPEN-DS1-FILE.
           MOVE 0              TO DS-DATE-CCYYMM.
           MOVE DTSCAN-BRNO    TO DS-BRANCH.
           MOVE HOLD-DT-DLNO   TO DS-DLNO.
           MOVE 0              TO DS-CLASS.

           PERFORM START-DS1-FILE.
       ACCUM-DEALER-STATS.
           PERFORM READ-DS1-FILE-NEXT.
           IF IO-FG = 9
              GO TO FINISH-DEALER-STATS.

           IF DS-DLNO NOT = HOLD-DT-DLNO
              GO TO FINISH-DEALER-STATS.

           IF DS-BRANCH NOT = DTSCAN-BRNO
              GO TO FINISH-DEALER-STATS.

           ADD DS-UNPAIDCK        TO HOLD-UNPAID.
           ADD DS-UNPAID-MANUAL   TO HOLD-UNPAIDM.
           GO TO ACCUM-DEALER-STATS.
       FINISH-DEALER-STATS.
           MOVE HOLD-UNPAID  TO D-DS-UNPAID.
           MOVE HOLD-UNPAIDM TO D-DS-UNPAIDM.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           DISPLAY SPACES UPON DTSCAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DTSCAN-SCREEN UPON DTSCAN-WINDOW-HANDLE.
           MOVE DTSCAN-FORM-FIELDS TO WR-BUF.
           MOVE DTSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE DTSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE DTSCAN-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/DTSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       MISC-PARAGRAPHS SECTION.
      *************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

       SEND-HEADER-INFO.
           MOVE DTSCANW-TYPE      TO TYPE-SCAN
           MOVE "BALS"            TO TYPE-DESC.
           MOVE BAL-PROMPT        TO HEAD-BUF.
           IF DTSCANW-TYPE = 1
              MOVE "DETAIL"       TO TYPE-DESC
              MOVE DETAIL-PROMPT  TO HEAD-BUF.
           IF DTSCANW-TYPE = 2
              MOVE "DUE   "       TO TYPE-DESC
              MOVE DUE-PROMPT     TO HEAD-BUF.
           MOVE PT-BUF TO WR-BUF.
           MOVE PT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF HOLD-DT-DLNO NOT = 0
              MOVE "SN N042DLNO." TO VDU
              MOVE HOLD-DT-DLNO TO VDUNUM2
              PERFORM SCREENIO.
           IF HOLD-DT-ACCTNO NOT = 0
              MOVE "SN N060ACCT." TO VDU
              MOVE HOLD-DT-ACCTNO TO VDUNUM0
              PERFORM SCREENIO.
           IF HOLD-DT-DATE NOT = 0
              MOVE "S  A000DT1." TO VDU
              MOVE HOLD-DT-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.


      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDT1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************
       END-ROUTINE SECTION.
      *******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DTSCAN-SCREEN.
           DESTROY DTSCAN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
