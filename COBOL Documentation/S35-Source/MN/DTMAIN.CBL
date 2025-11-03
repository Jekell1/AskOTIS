       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DTMAIN.
       DATE-WRITTEN.   012786.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   DEALER TRANSACTION FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * JFF 073196 ADDED DT-BATCHID-SEQ AND TOOK DT-BRNO OUT OF DT1-KEY.
      * GLF 102497 REMOVED RESET OF NUMERIC AND SIGNED FIELDS
      * MJD 112597 EXPANDED DT-REC TO 162 BYTES
      * BLF 050523 ADDED FIELD 30, DT-MEMO, REGACC PR# 103
      *            (DISPLAY ONLY)
      *  CS 060320 ADD DT-VENT-CD USED FOR CUSTOM DEALER STATEMENT
      *            REPORT [SAL PR#2503]
      *  CS 060726 HAD DT-EVENT-CD DEFINED AS NUMERIC, CHANGED TO ALPHA
      *  CS 070330 ALLOW DT-EVENT-CD TO BE SPACES.... BARB
      * BLF 080110 ADDED FIELD 32, DT-PART-HELD-STATUS, REGACC PR# 185
      * KEC 2017.0208 ADDED 'DT' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * KEC 2017.0213 MOVED DT-BRNO TO BE WITH THE REST OF THE ENTRY
      *               OF THE KEY ON THE SCREEN; REMOVED DT-BRNO FROM
      *               #28 ON THE SCREEN.
      * BAH 2017.0417 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.0608 ERRLOG
      * BAH 2018.1029 GLOBAL DTFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/DTMAIN.CBL S34 06/03/21-16:10:15 >".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

       01 DTMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       01  ERRCD             PIC X     VALUE SPACES.
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9     VALUE 0.
       01  FIRST-SCAN        PIC 9     VALUE 0.

       01  HOLD-DT1-KEY.
           03  HOLD-DT-BRNO           PIC 9(4).
           03  HOLD-DT-DLNO           PIC 9(4)V99.
           03  HOLD-DT-CLASS          PIC 9(3).
           03  HOLD-DT-ACCTNO         PIC 9(8).
           03  HOLD-DT-DATE           PIC 9(8).
           03  HOLD-DT-SEQ            PIC 9(3).

       01  WS-KEY            PIC X(25).
       01  WS-KEY-RDF        REDEFINES WS-KEY.
           03  WS-NUM        PIC 9(4)V99.
           03  WS-CLASS      PIC 99.
           03  WS-ACCTNO     PIC 9(8).
           03  WS-DATE       PIC 9(6).
           03  WS-SEQ        PIC 9(3).

       01  HOLD-TRDESC          PIC X(40).
       01  HOLD-DT-TRCD            PIC X(2)          VALUE " ".
           88  DEALER-ORIGINAL   VALUE "NW" "TX" "TP" "TS".
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
           "TS TFR ??? TO SERVICE            ".
           03  FILLER           PIC X(33) VALUE
           "SJ JUDGEMENT SPLIT               ".
       01  TRANS-DESC-TABLE2 REDEFINES TRANS-DESC-TABLE.
           03  TRANS-CD-DESC-CODE OCCURS 59.
               05  TRANS-TRCD       PIC X(2).
               05  FILLER           PIC X.
               05  TRANS-TRDESC     PIC X(30).
       01  TRANS-TRMAX              PIC 9(3) VALUE 59.
       01  TRANS-TRSUB              PIC 9(3) VALUE 0.

      *=================================================================
      *   DTCH-REC - CHANGED-REC
      *   DTOR-REC - ORIGINAL-REC
      *=================================================================
       COPY "LIBLP/LP01DT.CPY" REPLACING LEADING "DT" BY "DTCH".
       COPY "LIBLP/LP01DT.CPY" REPLACING LEADING "DT" BY "DTOR".

      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  WORK-ERRLOG-DLNO         PIC 9(4).
           03  WORK-ERRLOG-NUMBER       PIC 9(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(2).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(10).
               05  WORK-ERRLOG-ORG-N0 REDEFINES WORK-ERRLOG-ORG-ALP 
                                        PIC Z(9)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES WORK-ERRLOG-ORG-ALP 
                                        PIC Z(6).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(10).
               05  WORK-ERRLOG-CHG-N0 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC Z(9)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC Z(6).99-.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER1
      *    **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC X.
           03  F02           PIC XX.
           03  F021          PIC X(30).
           03  F03           PIC Z(4).
           03  F04           PIC Z,ZZZ,ZZZ.99-.
           03  F05           PIC Z,ZZZ,ZZZ.99-.
           03  F06           PIC Z,ZZZ,ZZZ.99-.
           03  F07           PIC Z,ZZZ,ZZZ.99-.
           03  F08           PIC Z,ZZZ,ZZZ.99-.
           03  F09           PIC Z,ZZZ,ZZZ.99-.
           03  F10           PIC Z,ZZZ,ZZZ.99-.
           03  F11           PIC Z,ZZZ,ZZZ.99-.
           03  F12           PIC Z,ZZZ,ZZZ.99-.
           03  F13           PIC Z,ZZZ,ZZZ.99-.
           03  F14           PIC Z,ZZZ,ZZZ.99-.
           03  F141          PIC Z,ZZZ,ZZZ.99-.
           03  F142          PIC Z,ZZZ,ZZZ.99-.
           03  F143          PIC Z,ZZZ,ZZZ.99-.
           03  F15           PIC Z(6).
           03  F151          PIC X.
           03  F16           PIC ZZ/ZZ/ZZ.
           03  F17           PIC 9(10).
           03  F18           PIC 9(5).
           03  F19           PIC X.
           03  F20           PIC X.
           03  F21           PIC X.
           03  F22           PIC XX.
           03  F23           PIC 9.
           03  F24           PIC ZZZ,ZZZ.99.
           03  F25           PIC ZZZ,ZZZ.99-.
           03  F26           PIC XX.
           03  F27           PIC X.
           03  F28           PIC X.
           03  F29           PIC X.
           03  F30           PIC 99.
           03  F31           PIC X.

       01  DISPLAY-LIST.
           03  FILLER        PIC X(44) VALUE
           "010 020 021 030 040 050 060 070 080 090 100 ".
           03  FILLER        PIC X(52) VALUE
           "110 120 130 140 141 142 143 150 151 160 170 180 190 ".
           03  FILLER        PIC X(48) VALUE
           "200 210 220 230 240 250 260 270 280 290 300 310.".

       01  DISPLAY-KEY-BUF.
           03  D-KEY-DLNO     PIC Z(4).99.
           03  D-KEY-CLASS    PIC 99.
           03  D-KEY-ACCTNO   PIC Z(6)99.
           03  D-KEY-DATE     PIC 99/99/99.
           03  D-KEY-SEQ      PIC 999.

       01  DISPLAY-KEY-LIST.
           03  FILLER         PIC X(38) VALUE
           "DLNO CLASS ACCTNO TRDATE SEQ.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 9(2)    VALUE 31.
       01  SPEC-TABLE.
      * 01. DT-TYPE
           03  FILLER        PIC X(8) VALUE "E  A010N".
      * 02. DT-TRCD
           03  FILLER        PIC X(8) VALUE "E  A020N".
      * 03. DT-LPSEQNO
           03  FILLER        PIC X(8) VALUE "ENNN040N".
      * 4 - 6 DT-RVPRIN (1-3)
           03  FILLER        PIC X(8) VALUE "E NS072N".
           03  FILLER        PIC X(8) VALUE "E NS072N".
           03  FILLER        PIC X(8) VALUE "E NS072N".
      * 7 - 9 DT-RVCHGS (1-3)
           03  FILLER        PIC X(8) VALUE "E NS072N".
           03  FILLER        PIC X(8) VALUE "E NS072N".
           03  FILLER        PIC X(8) VALUE "E NS072N".
      * 10. DT-DLPARVHELD
           03  FILLER        PIC X(8) VALUE "E NS072N".
      * 11. DT-DLPARVPAID
           03  FILLER        PIC X(8) VALUE "E NS072N".
      * 12. DT-PROCEEDS
           03  FILLER        PIC X(8) VALUE "E NS072N".
      * 13. DT-RELEASED
           03  FILLER        PIC X(8) VALUE "E NS072N".
      * 14. DT-PAYMENTS
           03  FILLER        PIC X(8) VALUE "E NS072N".
      * 15. DT-CKNO
           03  FILLER        PIC X(8) VALUE "ENNN060N".
      * 16. DT-CKDATE
           03  FILLER        PIC X(8) VALUE "ERNM060N".
      * 17. DT-BATCHID
           03  FILLER        PIC X(8) VALUE "ENNN100N".
      * 18. DT-BATCHID-SEQ
           03  FILLER        PIC X(8) VALUE "ENNN050N".
      * 19. DT-PAID
           03  FILLER        PIC X(8) VALUE "E  A010N".
      * 20. DT-STATUS
           03  FILLER        PIC X(8) VALUE "E  A010N".
      * 21. DT-REVERSE
           03  FILLER        PIC X(8) VALUE "E  A010N".
      * 22. DT-REVERSE-TRCD
           03  FILLER        PIC X(8) VALUE "E  A020N".
      * 23. DT-DLRVNO
           03  FILLER        PIC X(8) VALUE "ENNN010N".
      * 24. DT-PURBAL
           03  FILLER        PIC X(8) VALUE "ENNN062N".
      * 25. DT-APCUR
           03  FILLER        PIC X(8) VALUE "E NS062N".
      * 26. DT-P4P5-TRCD
           03  FILLER        PIC X(8) VALUE "E  A020N".
      * 27. DT-SELECTED
           03  FILLER        PIC X(8) VALUE "E  A010N".
      * 28. DT-UNPAID
           03  FILLER        PIC X(8) VALUE "E  A010N".
      * 29. DT-MEMO
           03  FILLER        PIC X(8) VALUE "E  A010N".
      * 30. DT-EVENT-CD
           03  FILLER        PIC X(8) VALUE "E  A020N".
      * 31. DT-PART-HELD-STATUS
           03  FILLER        PIC X(8) VALUE "E  A010N".


       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 31 TIMES.
               05  SPEC     PIC X(7).
               05  CHFG     PIC X.


       COPY "LIBWI/DTSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/DTMAIN_DEF.CPY".
       COPY "LIBMN/DTMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/DTMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE "DTMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
               COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS DTMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
              MOVE 0 TO DT-DLNO    DT-CLASS    DT-ACCTNO
                        DT-DATE    DT-SEQ      HOLD-DT-DLNO
                        HOLD-DT-CLASS HOLD-DT-ACCTNO HOLD-DT-DATE
                        HOLD-DT-SEQ   HOLD-DT-BRNO
              MOVE 1 TO DTSCANW-TYPE.

           PERFORM FORM-RESET.

           PERFORM REC-DT-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************
       REC-DT-MODULE SECTION.
      ******************************************
       REC-DT-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           PERFORM FORM-RESET.


       REC-DT-DLNO.
           MOVE "DLNAME." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "ENTER DEALER NO., F2-DEALER, F5-NEXT, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.

      *    IF ( UP-KEY ) GO TO REC-DT-BRNO.

           IF VDUSTATUS = "12"
              PERFORM DEALER-SCAN
                 IF IO-FG = 9
                    GO TO REC-DT-DLNO
                 ELSE
                    GO TO CONTINUE-DEALER.
           IF VDUSTATUS = "1:"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DT-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-DT-MODULE.

           IF VDUSTATUS = "1<"
      *       MOVE 0 TO HOLD-DT-CLASS HOLD-DT-ACCTNO HOLD-DT-SEQ
              MOVE 0 TO HOLD-DT-SEQ
              PERFORM DTFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-DT-DLNO
              ELSE
                 GO TO READ-DTFILE.

           IF VDUSTATUS = "10"
              GO TO REC-DT-MODULE-NO.
           IF VDUSTATUS NOT = "00"
             GO TO REC-DT-DLNO.
           IF VDUNUM2 = 0
              MOVE "SN N042DLNO." TO VDU
              MOVE HOLD-DT-DLNO TO VDUNUM2
              PERFORM SCREENIO.
       CONTINUE-DEALER.
      *    MOVE VDUNUM2 TO DT-DLNO DTSCANW-DLNO HOLD-DT-DLNO.
           MOVE VDUNUM2 TO  HOLD-DT-DLNO.
           PERFORM VERIFY-DLNO.

       REC-DT-CLASS.
           MOVE "CLSDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "^BACK TO DEALER, F5-NEXT, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-DT-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-DT-DLNO.
           IF VDUSTATUS = "1:"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DT-MODULE.
           IF VDUSTATUS = "1<"
      *       MOVE 0 TO HOLD-DT-ACCTNO HOLD-DT-SEQ
              MOVE 0 TO HOLD-DT-SEQ
              PERFORM DTFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-DT-CLASS
              ELSE
                 GO TO READ-DTFILE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-DT-CLASS.

      *    MOVE VDUNUM0 TO DT-CLASS HOLD-DT-CLASS.
           MOVE VDUNUM0 TO HOLD-DT-CLASS.
           PERFORM VERIFY-CLASS.
           IF IO-FG NOT = 0
              GO TO REC-DT-CLASS.

       REC-DT-ACCTNO.
           MOVE "^BACK TO CLASS, F5-NEXT, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN080ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-DT-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-DT-CLASS.
           IF VDUSTATUS = "1:"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DT-MODULE.

           IF VDUSTATUS = "1<"
      *       MOVE 0 TO HOLD-DT-ACCTNO
              MOVE 0 TO HOLD-DT-SEQ
              PERFORM DTFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-DT-DLNO
              ELSE
                 GO TO READ-DTFILE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-DT-ACCTNO.
      *    MOVE VDUNUM0 TO DT-ACCTNO HOLD-DT-ACCTNO.
           MOVE VDUNUM0 TO HOLD-DT-ACCTNO.

       REC-DT-DATE.
           MOVE "^BACK TO ACCT, F5-NEXT, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERRM060TRDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-DT-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-DT-ACCTNO.
           IF VDUSTATUS = "1:"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DT-MODULE.
           IF VDUSTATUS = "1<"
              MOVE 0 TO HOLD-DT-DATE HOLD-DT-SEQ
              PERFORM DTFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-DT-DLNO
              ELSE
                 GO TO READ-DTFILE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-DT-DATE.
           MOVE VDU-DATE-YYYYMMDD TO HOLD-DT-DATE.

       REC-DT-SEQ.
           MOVE "^BACK TO DATE, F5-NEXT, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N030SEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-DT-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-DT-DATE.
           IF VDUSTATUS = "1:"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DT-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM DTFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-DT-DLNO
              ELSE
                 GO TO READ-DTFILE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-DT-SEQ.
           MOVE VDUNUM0 TO  HOLD-DT-SEQ.

       READ-DTFILE.
           PERFORM OPEN-DT1-FILE.
           MOVE DT-PATH-OWNBR TO HOLD-DT-BRNO.
           MOVE HOLD-DT1-KEY  TO DT1-KEY.
           PERFORM READ-DT1-FILE.
           PERFORM CLOSE-DT1-FILE.
           IF IO-FG = 0
              MOVE DT-REC TO DTOR-REC
              PERFORM DISPLAY-KEY-FIELDS
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-DT-MODULE.
           EXIT.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE "F1 RESET SCAN, F6 CONTINUE SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE 0 TO FIRST-SCAN.

           PERFORM OPEN-DT1-FILE.
       ENTER-KEY.
           MOVE "E RA250SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-DT1FILE-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF        TO WS-KEY.

           MOVE DT-PATH-OWNBR TO DT-BRNO.
           MOVE WS-NUM        TO DT-DLNO.
           MOVE WS-CLASS      TO DT-CLASS.
           MOVE WS-ACCTNO     TO DT-ACCTNO.
           MOVE WS-DATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO DT-DATE.
           MOVE WS-SEQ        TO DT-SEQ.

           PERFORM START-DT1-FILE.
           MOVE 1 TO FIRST-SCAN.

       READ-DT1FILE-NEXT.
           IF FIRST-SCAN = 0
      * HOLD-DT1-KEY IS ALL 0'S AT THIS POINT, MUST HAVE BRANCH OR IT
      * WONT PASS THE DT-PATH-OWNBR TEST ON THE READ-NEXT BELOW
              MOVE DT-PATH-OWNBR TO HOLD-DT-BRNO
              MOVE HOLD-DT1-KEY  TO DT1-KEY
              PERFORM START-DT1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-DT1-FILE-NEXT.
           IF IO-FG = 9 OR (DT-BRNO NOT = DT-PATH-OWNBR) 
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK DT-FILE.
           MOVE DT-REC TO DTOR-REC.
           MOVE DT1-KEY TO HOLD-DT1-KEY.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.
           MOVE "SNNN042DLNO." TO VDU.
           MOVE DT-DLNO TO VDUNUM2 HOLD-DT-DLNO.
           PERFORM SCREENIO.
           PERFORM VERIFY-DLNO.
           MOVE "SN N020CLASS." TO VDU.
           MOVE DT-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-CLASS.
           MOVE "SN N080ACCTNO." TO VDU.
           MOVE DT-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A000TRDATE." TO VDU.
           MOVE DT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "SN N030SEQ." TO VDU.
           MOVE DT-SEQ TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM CLOSE-DT1-FILE.
       END-SCANKEY.
           EXIT.

      ****************************************
       DEALER-SCAN SECTION.
      ****************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           MOVE "SN N042DLNO." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           IF VDUBUF NOT = "P"
              IF VDUBUF NOT = "S"
                 MOVE "P - PURCHASED, S- SERVICED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO DT-TYPE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF  TO DT-TRCD.
           MOVE DT-TRCD TO HOLD-DT-TRCD.
           PERFORM FIND-TRANS-DESCRIPTION.
           MOVE "S  A000210." TO VDU.
           MOVE HOLD-TRDESC TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM0 TO DT-LPSEQNO.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO DT-DLRVPRIN(1).
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO DT-DLRVPRIN(2).
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO DT-DLRVPRIN(3).
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO DT-DLRVCHGS(1).
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO DT-DLRVCHGS(2).
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO DT-DLRVCHGS(3).
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO DT-DLPARVHELD.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM2 TO DT-DLPARVPAID.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO DT-PROCEEDS.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM2 TO DT-RELEASED.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM2 TO DT-PAYMENTS.
           MOVE "E NS072141." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM2 TO DT-DLRVPRIN-PAY.
           MOVE "E NS072142." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM2 TO DT-DLRVCHGS-PAY.
           MOVE "E NS072143." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM2 TO DT-DLPARVHELD-PAY.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO DT-CKNO.
           MOVE "E  A000151." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO DT-CHECK-VOID.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDU-DATE-YYYYMMDD TO DT-CKDATE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM0 TO DT-BATCHID.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM0 TO DT-BATCHID-SEQ.
           GO TO CREATE-ROUTINE.
       F-19.
           IF VDUBUF NOT = "M"
              IF VDUBUF NOT = "S"
                 IF VDUBUF NOT = "B"
           MOVE "M - MANUAL, S - SYSTEM, B - BATCH" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           MOVE VDUBUF TO DT-PAID.
           GO TO CREATE-ROUTINE.
       F-20.
           IF VDUBUF NOT = " "
              IF VDUBUF NOT = "R"
                 IF VDUBUF NOT = "P"
                    IF VDUBUF NOT = "C"
                 MOVE "SPACE-NEW,R-RELEASE,P-PRINTED,C-MEMO" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO DT-STATUS.
           GO TO CREATE-ROUTINE.
       F-21.
           IF VDUBUF NOT = " "
              IF VDUBUF NOT = "Y"
                 MOVE "ENTER EITHER ' ' OR 'Y'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO DT-REVERSE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUBUF TO DT-REVERSE-TRCD.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM0 TO DT-DLRVNO.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM2 TO DT-PURBAL.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUNUM2 TO DT-APCUR.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUBUF TO DT-P4P5-TRCD.
           IF DT-TRCD = "P4" OR "P5"
              IF NOT (DT-P4P5-TRCD = "PR" OR "PC" OR "PP")
                 MOVE "ENTER 'PR OR 'PC' OR 'PP'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF DT-P4P5-TRCD NOT = " "
                 MOVE "ENTER SPACE ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-27.
           IF NOT (VDUBUF  = " " OR "Y" OR "R")
              MOVE "ENTER EITHER ' ' OR 'Y' OR 'R'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO DT-SELECTED.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE VDUBUF TO DT-UNPAID.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDUBUF TO DT-MEMO.
           GO TO CREATE-ROUTINE.
       F-30.
           MOVE VDUBUF  TO DT-EVENT-CD.
           IF NOT (DT-EVENT-CD NUMERIC OR DT-EVENT-CD = SPACES)
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-31.
           IF NOT (VDUBUF  = " " OR "R")
              MOVE "ENTER EITHER ' ' OR 'R'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO DT-PART-HELD-STATUS.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       FIND-TRANS-DESCRIPTION SECTION.
      ******************************************************************

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

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
       REWRITE-RECORD.
           PERFORM OPEN-DT1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE DT-REC TO DTCH-REC.

           IF DTOR-REC = DTCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-DT1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF DT-REC NOT = DTOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE DTCH-REC TO DT-REC.

           PERFORM REWRITE-DT1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-DT1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-DT1-FILE.

      **********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE DT-BRNO   TO WORK-ERRLOG-BR.
           MOVE DT-DLNO   TO WORK-ERRLOG-DLNO.
           MOVE DT-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF DTOR-TYPE NOT = DTCH-TYPE
             MOVE "01"          TO WORK-ERRLOG-FIELD
             MOVE "PUR / SERV"  TO WORK-ERRLOG-DESC
             MOVE DTOR-TYPE     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-TYPE     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-TRCD NOT = DTCH-TRCD
             MOVE "02"          TO WORK-ERRLOG-FIELD
             MOVE "TRANS CODE"  TO WORK-ERRLOG-DESC
             MOVE DTOR-TRCD     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-TRCD     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-LPSEQNO NOT = DTCH-LPSEQNO
             MOVE "03"          TO WORK-ERRLOG-FIELD
             MOVE "LP SEQNO  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-LPSEQNO     TO WORK-ERRLOG-ORG-N0
             MOVE DTCH-LPSEQNO     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVPRIN(1) NOT = DTCH-DLRVPRIN(1)
             MOVE "04"          TO WORK-ERRLOG-FIELD
             MOVE "RVPRIN 1  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVPRIN(1)     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVPRIN(1)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVPRIN(2) NOT = DTCH-DLRVPRIN(2)
             MOVE "05"          TO WORK-ERRLOG-FIELD
             MOVE "RVPRIN 2  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVPRIN(2)     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVPRIN(2)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVPRIN(3) NOT = DTCH-DLRVPRIN(3)
             MOVE "06"          TO WORK-ERRLOG-FIELD
             MOVE "RVPRIN 3  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVPRIN(3)     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVPRIN(3)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.


           IF DTOR-DLRVCHGS(1) NOT = DTCH-DLRVCHGS(1)
             MOVE "07"          TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 1  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVCHGS(1)     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVCHGS(1)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVCHGS(2) NOT = DTCH-DLRVCHGS(2)
             MOVE "08"          TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 2  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVCHGS(2)     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVCHGS(2)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVCHGS(3) NOT = DTCH-DLRVCHGS(3)
             MOVE "09"          TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 3  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVCHGS(3)     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVCHGS(3)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLPARVHELD NOT = DTCH-DLPARVHELD
             MOVE "10"          TO WORK-ERRLOG-FIELD
             MOVE "PARTC HELD"  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLPARVHELD     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLPARVHELD     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLPARVPAID NOT = DTCH-DLPARVPAID
             MOVE "11"          TO WORK-ERRLOG-FIELD
             MOVE "PARTC PAID"  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLPARVPAID     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLPARVPAID     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-PROCEEDS NOT = DTCH-PROCEEDS
             MOVE "12"          TO WORK-ERRLOG-FIELD
             MOVE "PROCEEDS  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-PROCEEDS     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-PROCEEDS     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-RELEASED NOT = DTCH-RELEASED
             MOVE "13"          TO WORK-ERRLOG-FIELD
             MOVE "RELEASED  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-RELEASED     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-RELEASED     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-PAYMENTS NOT = DTCH-PAYMENTS
             MOVE "14"          TO WORK-ERRLOG-FIELD
             MOVE "PAID PROC "  TO WORK-ERRLOG-DESC
             MOVE DTOR-PAYMENTS     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-PAYMENTS     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVPRIN-PAY NOT = DTCH-DLRVPRIN-PAY
             MOVE "14"          TO WORK-ERRLOG-FIELD
             MOVE "PAID PRIN "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVPRIN-PAY     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVPRIN-PAY     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVCHGS-PAY NOT = DTCH-DLRVCHGS-PAY
             MOVE "14"          TO WORK-ERRLOG-FIELD
             MOVE "PAID CHGS "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVCHGS-PAY     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLRVCHGS-PAY     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLPARVHELD-PAY NOT = DTCH-DLPARVHELD-PAY
             MOVE "14"          TO WORK-ERRLOG-FIELD
             MOVE "PAID HELD "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLPARVHELD-PAY     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-DLPARVHELD-PAY     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-CKNO NOT = DTCH-CKNO
             MOVE "15"          TO WORK-ERRLOG-FIELD
             MOVE "CHECK #   "  TO WORK-ERRLOG-DESC
             MOVE DTOR-CKNO     TO WORK-ERRLOG-ORG-N0
             MOVE DTCH-CKNO     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-CHECK-VOID NOT = DTCH-CHECK-VOID
             MOVE "15"          TO WORK-ERRLOG-FIELD
             MOVE "CHECK VOID"  TO WORK-ERRLOG-DESC
             MOVE DTOR-CHECK-VOID     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-CHECK-VOID     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-CKDATE NOT = DTCH-CKDATE
             MOVE "16"          TO WORK-ERRLOG-FIELD
             MOVE "CHECK DATE"  TO WORK-ERRLOG-DESC
             MOVE DTOR-CKDATE     TO WORK-ERRLOG-ORG-N0
             MOVE DTCH-CKDATE     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-BATCHID NOT = DTCH-BATCHID
             MOVE "17"          TO WORK-ERRLOG-FIELD
             MOVE "BATCH ID  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-BATCHID     TO WORK-ERRLOG-ORG-N0
             MOVE DTCH-BATCHID     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-BATCHID-SEQ NOT = DTCH-BATCHID-SEQ
             MOVE "18"          TO WORK-ERRLOG-FIELD
             MOVE "BATCH ID  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-BATCHID-SEQ     TO WORK-ERRLOG-ORG-N0
             MOVE DTCH-BATCHID-SEQ     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-PAID NOT = DTCH-PAID
             MOVE "19"          TO WORK-ERRLOG-FIELD
             MOVE "PAID TYPE "  TO WORK-ERRLOG-DESC
             MOVE DTOR-PAID     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-PAID     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-STATUS NOT = DTCH-STATUS
             MOVE "20"          TO WORK-ERRLOG-FIELD
             MOVE "STATUS    "  TO WORK-ERRLOG-DESC
             MOVE DTOR-STATUS     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-STATUS     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-REVERSE NOT = DTCH-REVERSE
             MOVE "21"          TO WORK-ERRLOG-FIELD
             MOVE "REVERSE    "  TO WORK-ERRLOG-DESC
             MOVE DTOR-REVERSE     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-REVERSE     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-REVERSE-TRCD NOT = DTCH-REVERSE-TRCD
             MOVE "22"          TO WORK-ERRLOG-FIELD
             MOVE "REV TRCD   "  TO WORK-ERRLOG-DESC
             MOVE DTOR-REVERSE-TRCD     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-REVERSE-TRCD     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-DLRVNO NOT = DTCH-DLRVNO
             MOVE "23"          TO WORK-ERRLOG-FIELD
             MOVE "DLNO RVNO  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-DLRVNO     TO WORK-ERRLOG-ORG-N0
             MOVE DTCH-DLRVNO     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-PURBAL NOT = DTCH-PURBAL
             MOVE "24"          TO WORK-ERRLOG-FIELD
             MOVE "PURBAL     "  TO WORK-ERRLOG-DESC
             MOVE DTOR-PURBAL     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-PURBAL     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-APCUR NOT = DTCH-APCUR
             MOVE "25"          TO WORK-ERRLOG-FIELD
             MOVE "APPRIN    "  TO WORK-ERRLOG-DESC
             MOVE DTOR-APCUR     TO WORK-ERRLOG-ORG-N2
             MOVE DTCH-APCUR     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-P4P5-TRCD NOT = DTCH-P4P5-TRCD
             MOVE "26"          TO WORK-ERRLOG-FIELD
             MOVE "P4P5      "  TO WORK-ERRLOG-DESC
             MOVE DTOR-P4P5-TRCD     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-P4P5-TRCD     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-SELECTED NOT = DTCH-SELECTED
             MOVE "27"          TO WORK-ERRLOG-FIELD
             MOVE "SELECTED  "  TO WORK-ERRLOG-DESC
             MOVE DTOR-SELECTED     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-SELECTED     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-UNPAID NOT = DTCH-UNPAID
             MOVE "28"          TO WORK-ERRLOG-FIELD
             MOVE "UNPAID    "  TO WORK-ERRLOG-DESC
             MOVE DTOR-UNPAID     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-UNPAID     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-MEMO NOT = DTCH-MEMO
             MOVE "29"          TO WORK-ERRLOG-FIELD
             MOVE "MEMO      "  TO WORK-ERRLOG-DESC
             MOVE DTOR-MEMO     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-MEMO     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-EVENT-CD NOT = DTCH-EVENT-CD
             MOVE "30"          TO WORK-ERRLOG-FIELD
             MOVE "EVENT CODE"  TO WORK-ERRLOG-DESC
             MOVE DTOR-EVENT-CD     TO WORK-ERRLOG-ORG-N0
             MOVE DTCH-EVENT-CD     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF DTOR-PART-HELD-STATUS NOT = DTCH-PART-HELD-STATUS
             MOVE "31"          TO WORK-ERRLOG-FIELD
             MOVE "PARTHELDPD"  TO WORK-ERRLOG-DESC
             MOVE DTOR-PART-HELD-STATUS     TO WORK-ERRLOG-ORG-ALP
             MOVE DTCH-PART-HELD-STATUS     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      ************************************
       LOG-FIELD-CHANGE SECTION.
      ************************************

      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *****************************************
       DELETE-MODULE SECTION.
      *****************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-DT1-FILE.
           PERFORM READ-DT1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF DT-REC NOT = DTOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-DT1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING DT1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
       END-DELETE-MODULE.
           PERFORM CLOSE-DT1-FILE.

      ****************************************
       NEW-RECORD-SETUP SECTION.
      ****************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-DT-FILE.

           MOVE HOLD-DT1-KEY TO DT1-KEY.
           MOVE 0 TO ELE.
       NEW-RECORD-EXIT.
           EXIT.

      ****************************************
       DISPLAY-KEY-FIELDS SECTION.
      ****************************************
      *    MOVE DT-BRNO     TO D-KEY-BRNO.
           MOVE DT-DLNO     TO D-KEY-DLNO.
           MOVE DT-CLASS    TO D-KEY-CLASS.
           MOVE DT-ACCTNO   TO D-KEY-ACCTNO.
           MOVE DT-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-KEY-DATE.
           MOVE DT-SEQ      TO D-KEY-SEQ .
           MOVE DISPLAY-KEY-BUF TO WR-BUF.
           MOVE DISPLAY-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       DISPLAY-FIELDS SECTION.
      ****************************************
           MOVE DT-TYPE           TO F01.
           MOVE DT-TRCD           TO F02.
           MOVE DT-TRCD           TO HOLD-DT-TRCD.
           PERFORM FIND-TRANS-DESCRIPTION.
           MOVE HOLD-TRDESC       TO F021.
           MOVE DT-LPSEQNO        TO F03.
           MOVE DT-DLRVPRIN(1)    TO F04.
           MOVE DT-DLRVPRIN(2)    TO F05.
           MOVE DT-DLRVPRIN(3)    TO F06.
           MOVE DT-DLRVCHGS(1)    TO F07.
           MOVE DT-DLRVCHGS(2)    TO F08.
           MOVE DT-DLRVCHGS(3)    TO F09.
           MOVE DT-DLPARVHELD     TO F10.
           MOVE DT-DLPARVPAID     TO F11.
           MOVE DT-PROCEEDS       TO F12.
           MOVE DT-RELEASED       TO F13.
           MOVE DT-PAYMENTS       TO F14.
           MOVE DT-DLRVPRIN-PAY   TO F141.
           MOVE DT-DLRVCHGS-PAY   TO F142.
           MOVE DT-DLPARVHELD-PAY TO F143.
           MOVE DT-CKNO           TO F15.
           MOVE DT-CHECK-VOID     TO F151.
           MOVE DT-CKDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F16.
           MOVE DT-BATCHID        TO F17.
           MOVE DT-BATCHID-SEQ    TO F18.
           MOVE DT-PAID           TO F19.
           MOVE DT-STATUS         TO F20.
           MOVE DT-REVERSE        TO F21.
           MOVE DT-REVERSE-TRCD   TO F22.
           MOVE DT-DLRVNO         TO F23.
           MOVE DT-PURBAL         TO F24.
           MOVE DT-APCUR          TO F25.
           MOVE DT-P4P5-TRCD      TO F26.
           MOVE DT-SELECTED       TO F27.
           MOVE DT-UNPAID         TO F28.
           MOVE DT-MEMO           TO F29.
           MOVE DT-EVENT-CD       TO F30.
           MOVE DT-PART-HELD-STATUS TO F31.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *****************************************
       DTFILE-SCAN SECTION.
      *****************************************
           MOVE HOLD-DT1-KEY   TO DTSCAN-DT1-KEY.
           MOVE "WI/DTSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DTSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DTSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG
              MOVE DTSCAN-DT1-KEY TO DT1-KEY
              MOVE DT1-KEY        TO HOLD-DT1-KEY.

      *****************************************
       FORM-RESET SECTION.
      *****************************************
           MOVE EXT-CONAME-CONAME TO DTMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DTMAIN-SYSDATE.
           DISPLAY SPACES UPON DTMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DTMAIN-SCREEN UPON DTMAIN-WINDOW-HANDLE.
           MOVE DTMAIN-FORM-FIELDS TO WR-BUF.
           MOVE DTMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE DTMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE DTMAIN-HELP--DIR TO HELP-LINK-DIR.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/DTMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPDTCL.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           PERFORM CALL-TMMSG.
       VERIFY-DLNO.
           PERFORM OPEN-DL1-FILE.
           MOVE HOLD-DT-DLNO TO DL-DLNO.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO DL-NAME.
           MOVE "S  A250DLNAME." TO VDU.
           MOVE DL-NAME TO VDUBUF.
           PERFORM SCREENIO.

       VERIFY-CLASS.
           PERFORM OPEN-LC1-FILE.
           MOVE HOLD-DT-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE "S  A160CLSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY DTMAIN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
