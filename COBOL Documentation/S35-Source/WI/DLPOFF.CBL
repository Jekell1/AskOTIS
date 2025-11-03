       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLPOFF.
       DATE-WRITTEN.    042994.
      **************************************************************************
      *          WINDOW DEALER PAYOFF
      *              CALLED FROM F9 CLMENU AND
      *                             WI/SFINFO(F3), WI/SFMAST
      *
      * REV:
      *  JTG 032593 REVISED TO PASS BR-REC AND GB-REC
      *  BAH 101794 REMOVED ALL OCCURANCES OF LP-FILE
      *  SLC 053095 COMBINED OTHBAL AND OT2BAL IN L-090
      *  MJD 071795 CHANGED F3-SFINFO TO F5-SFINFO.
      *  MJD 071896 ADDED LOGIC FOR 6 DIGIT PDTH DATES
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  MJD 052197 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  JTG 060497 ADDED LOGIC FOR 'O3' REBATE
      *  JTG 010598 ADDED SET OF POFF-RECAST-FG IDEAL A48 TO A60 #617
      *  JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *             NOTE: NO CHANGES REQUIRED
      *  MJD 083398 FIXED DISPLAY TO INDICATE INCLUDED O2 BAL WITH
      *             "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      *   CS 150209 CHANGED BEGIN-X (DLPOFFW) FROM 2 TO 11, WAS HIDING ACCTNO
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      * BAH 170414 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
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

      ***************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/DLPOFF.CBL S35 04/22/24-10:50:25 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".


       COPY "LIBLP/LP01NO.CPY".

       01  DLPOFF-LINES              PIC 99  VALUE 13.
       01  DLPOFF-COLUMNS            PIC 99  VALUE 80.
       01  DLPOFF-BEGIN-Y            PIC 99  VALUE 1.
       01  DLPOFF-BEGIN-X            PIC 99  VALUE 11.

       01  ERRCD                 PIC X.
       01  REFRESH-SCREEN-LINES  PIC X.
       01  ONE                   PIC S9.
       01  CNT                   PIC 99.
       01  SUB                   PIC 9(3) COMP-3  VALUE 0.
       01  SUB1                  PIC 9(3) COMP-3  VALUE 0.
       01  SAVED-SUB1            PIC 9(3) COMP-3  VALUE 0.
       01  CUR-SUB               PIC 9(3) COMP-3  VALUE 0.
       01  NEWCUR-SUB            PIC 9(3) COMP-3  VALUE 0.
       01  TRANS-DATE            PIC 9(8).
       01  HOLD-WI-CL-TRANS-DATE PIC 9(8).

       01  REBATE-DISP-TAB       PIC X(22)     VALUE
           "RCRARPRIRSRMRDRER1R2R3".
       01  REBATE-DISP-RED      REDEFINES REBATE-DISP-TAB.
           03  REB-TYPE          PIC XX  OCCURS 11.

      **************************************************
      *         ORDER LINE BUFFER                      *
      **************************************************
       01  LINE-LIST.
           03  FILLER            PIC X(28)    VALUE
           "L01 L011   L013 L014   L015 ".
           03  FILLER            PIC X(28)    VALUE
           "L02 L021   L023 L024   L025 ".
           03  FILLER            PIC X(28)    VALUE
           "L03 L031   L033 L034   L035 ".
           03  FILLER            PIC X(28)    VALUE
           "L04 L041   L043 L044   L045 ".
           03  FILLER            PIC X(28)    VALUE
           "L05 L051   L053 L054   L055 ".
           03  FILLER            PIC X(28)    VALUE
           "L06 L061   L063 L064   L065 ".
           03  FILLER            PIC X(28)    VALUE
           "L07 L071   L073 L074   L075 ".
           03  FILLER            PIC X(28)    VALUE
           "L08 L081   L083 L084   L085.".
        01  LINE-LISTX           REDEFINES LINE-LIST.
            03  LINE-LISTXX      OCCURS 16.
                05  FILLER       PIC X(4).
                05  FILLER       PIC X(4).
                05  LINE-LISTX1  PIC XXX.
                05  FILLER       PIC X(5).
                05  FILLER       PIC X(4).
                05  LINE-LISTX3  PIC XXX.
                05  FILLER       PIC X(5).
       01  SAVED-LINE-LIST       PIC X(448).

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
           03  L-030           PIC X(5).
           03  L-040           PIC Z(5).99-  BLANK ZERO.
           03  L-040RATE       REDEFINES L-040
                               PIC ZZZZ.999-.
           03  L-050           PIC Z(3).99-  BLANK ZERO.
           03  L-060           PIC Z(4).99-  BLANK ZERO.
           03  L-070           PIC Z(5).99-  BLANK ZERO.
           03 L-080-MO         PIC 99.
           03 L-080-DAYR       PIC 99.
           03  L-090           PIC Z(5).99-  BLANK ZERO.
           03  L-100           PIC Z(4).99-  BLANK ZERO.
           03  L-110           PIC Z(5)9.99.
       01  BALANCE-BUF         REDEFINES LN-BUF.
           03  FILLER          PIC X(32).
           03  B-080           PIC 9(4)  BLANK ZERO.
           03  B-090           PIC Z(5).99-  BLANK ZERO.
           03  B-100           PIC Z(4).99-  BLANK ZERO.
           03  B-110           PIC Z(5)9.99.

       01  DUE-LINE            VALUE " ".
           03  FILLER          PIC X.
           03  DUE-PROMPT      PIC X(23).
           03  FILLER          PIC XXX.
           03  DUE-AMOUNT      PIC ZZZ,ZZ9.99-.
           03  FILLER          PIC X.

       01  GOOD-THRU-LINE      VALUE " ".
           03  FILLER          PIC X(23).
           03  GOOD-THRU       PIC 99/99/99.
       01  GOODTHRU-SUB        PIC 99.
       01  SAVE-TRANS-DATE     PIC 9(8).
       01  SAVE-POFF-NETDUE    PIC S9(7)V99.

       01  D-LIST              PIC X(9) VALUE "DLINFO.".
       01  D-BUF.
           03  FILLER          PIC X.
           03  D-DLNO          PIC ZZZ9.99.
           03  FILLER          PIC XX.
           03  D-DLNAME        PIC X(25).

       01  LEGEND1.
           03  FILLER          PIC X(47)   VALUE
           " F7-EXIT, F2-MEMO, F5-SALES INFO, (RTN)-SCROLL,".
           03  FILLER          PIC X(19)   VALUE
           " ENTER PAYOFF DATE:".
       01  LEGEND2.
           03  FILLER          PIC X(32)   VALUE
           " F7-EXIT, F2-MEMO, (RTN)-SCROLL,".
           03  FILLER          PIC X(19)   VALUE
           " ENTER PAYOFF DATE:".
       01  LEGEND                PIC X(70).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/SFINFOW.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       01  DLPOFF-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/DLPOFF_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/DLPOFF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/DLPOFFW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/DLPOFF_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME DLPOFF-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE "DLPOFF" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM GET-GPENV.

           MOVE EXT-WI-CL-TRANS-DATE TO HOLD-WI-CL-TRANS-DATE
                                        TRANS-DATE.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE DLPOFF-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  DLPOFF-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    DLPOFF-COLUMNS + VDU-WIN-W-MOD
                   LINES   DLPOFF-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS DLPOFF-WINDOW-HANDLE.

           PERFORM FORM-RESET.

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
              MOVE "NO GLOBAL RECORD " TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
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
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

           MOVE LN-DLNO TO DL-DLNO.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID DEALER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

           MOVE " " TO D-BUF.
           MOVE LN-DLNO TO D-DLNO.
           MOVE DL-NAME TO D-DLNAME.

           MOVE D-BUF TO WR-BUF.
           MOVE D-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.

       ENTER-NEW-PAYOFF-DATE-BEGIN.
           MOVE 0 TO CUR-SUB.
           IF DLPOFF-SFINFO-FG NOT = "N"
              MOVE LEGEND1 TO LEGEND
           ELSE
              MOVE LEGEND2 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A080ACCEPT:FR." TO VDU.
           MOVE " WORKING" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM SEND-NEW-PAYOFF-DATE.
           MOVE LINE-BUFFER TO SAVED-LINE-BUFFER.
           MOVE LINE-LIST TO SAVED-LINE-LIST.
           MOVE SUB1 TO SAVED-SUB1.
           MOVE " " TO REFRESH-SCREEN-LINES.

       ENTER-NEW-PAYOFF-DATE.
           MOVE "ER M060ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "12"
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO ENTER-NEW-PAYOFF-DATE.

           IF DLPOFF-SFINFO-FG NOT = "N"
              IF VDUSTATUS = "1:"
                 MOVE "N" TO SFINFO-DLPOFF-FG
                 MOVE "WI/SFINFO" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH SFINFO-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 GO TO ENTER-NEW-PAYOFF-DATE.

           IF NOT (VDUSTATUS = "00" OR "1U" OR "1D")
              GO TO ENTER-NEW-PAYOFF-DATE.
           IF VDUNUM0 = 0 OR VDUSTATUS = "1U" OR "1D"
              IF REFRESH-SCREEN-LINES = "Y"
                 PERFORM DISPLAY-ALL-LINES
                 MOVE " " TO REFRESH-SCREEN-LINES
                 GO TO ENTER-NEW-PAYOFF-DATE.
           IF VDUNUM0 = 0 OR VDUSTATUS = "1U" OR "1D"
              IF SUB1 < 9
                 PERFORM DISPLAY-ALL-LINES
                 GO TO ENTER-NEW-PAYOFF-DATE.
           IF VDUNUM0 = 0 OR VDUSTATUS = "1U" OR "1D"
              IF SUB1 > 8
                 SUBTRACT 8 FROM SUB1
                 PERFORM SHIFT-UP-LINES
                 PERFORM DISPLAY-ALL-LINES
                 MOVE SAVED-LINE-BUFFER TO LINE-BUFFER
                 MOVE SAVED-LINE-LIST TO LINE-LIST
                 MOVE SAVED-SUB1 TO SUB1
                 MOVE "Y" TO REFRESH-SCREEN-LINES
                 GO TO ENTER-NEW-PAYOFF-DATE.

           MOVE VDU-DATE-YYYYMMDD TO TRANS-DATE NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEW-PAYOFF-DATE.

           IF LN-LOANDATE > TRANS-DATE
              GO TO ENTER-NEW-PAYOFF-DATE.

           GO TO ENTER-NEW-PAYOFF-DATE-BEGIN.


      **************************************
       SET-PAYOFF-LINES SECTION.
      **************************************
           MOVE " " TO LP-REC.
           MOVE TRANS-DATE TO LP-PAYDATE.
           MOVE 0 TO LP-APOTH LP-APINT LP-APLC
                     LP-PDTH-DATE  LP-LCBAL.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

           MOVE 0 TO SUB SUB1.
           MOVE " " TO LINE-BUFFER.
           MOVE " " TO LINE-LISTX1(1) LINE-LISTX3(1)
                       LINE-LISTX1(2) LINE-LISTX3(2)
                       LINE-LISTX1(3) LINE-LISTX3(3)
                       LINE-LISTX1(4) LINE-LISTX3(4)
                       LINE-LISTX1(5) LINE-LISTX3(5)
                       LINE-LISTX1(6) LINE-LISTX3(6)
                       LINE-LISTX1(7) LINE-LISTX3(7)
                       LINE-LISTX1(8) LINE-LISTX3(8).
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
              MOVE "PO" TO LP-TRCD
              MOVE POFF-NETDUE TO LP-TRAMT
              MOVE POFF-OTDUE TO LP-APOTH
              MOVE POFF-LCDUE TO LP-APLC
              MOVE POFF-INTDUE TO LP-APINT
              SUBTRACT LP-APOTH LP-APINT LP-APLC FROM LP-TRAMT
                                           GIVING LP-APCUR
              MOVE 0 TO LP-OTHBAL LP-OT2BAL LP-INTBAL
                        LP-LCBAL LP-CURBAL.
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
           MOVE LINE-LISTX1(NEWCUR-SUB) TO LINE-LISTX1(CUR-SUB).
           MOVE LINE-LISTX3(NEWCUR-SUB) TO LINE-LISTX3(CUR-SUB).
           IF CUR-SUB NOT = 8
              GO TO SHIFT-UP-LINES-CONT.

      *******************************************
       SET-DEALER-PAYOFF SECTION.
      *******************************************
           ADD 3 TO SUB1.
           MOVE "    PAYOFF GOOD THRU:" TO GOOD-THRU-LINE.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO GOOD-THRU.
           MOVE GOOD-THRU-LINE          TO D-LINE(SUB1).
           MOVE ":S"                    TO LINE-LISTX1(SUB1).
           MOVE SUB1                    TO GOODTHRU-SUB.

           MOVE DUE-LINE TO D-DUE-LN(SUB1).
           MOVE ":S"     TO LINE-LISTX3(SUB1).

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE " " TO LN-BUF.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010.
           MOVE LP-TRCD     TO L-020.
           MOVE LP-REFNO    TO L-030.
           IF LP-TRCD = "JD" OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO L-040RATE
           ELSE
              MOVE LP-TRAMT TO L-040
              IF LP-REV = "F"
                 INSPECT L-040 REPLACING ALL "." BY X"76".

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

      ****************************************
       SEND-NEW-PAYOFF-DATE SECTION.
      ****************************************
           PERFORM SEND-LOAN-BAL-INFO.
           PERFORM SET-NEW-PAYOFF-DATE-WORKERS.
           PERFORM SET-PAYOFF-LINES.
           PERFORM SET-DEALER-PAYOFF.
           PERFORM WHEN-PAYOFF-GOOD.
           PERFORM DISPLAY-ALL-LINES.

      ************************************************
       SET-NEW-PAYOFF-DATE-WORKERS SECTION.
      ************************************************
           MOVE TRANS-DATE    TO POFF-PAYDATE.
           MOVE LN-MAKERCD(1) TO POFF-MAKERCD.
           MOVE "PO"          TO POFF-LPTRCD.
           MOVE " "           TO POFF-DEF-STOP.
           MOVE "Y"           TO POFF-RECAST-FG.
           PERFORM PAYOFF-LOAN-ROUTINE.

      *******************************************
       WHEN-PAYOFF-GOOD SECTION.
      *******************************************
           IF LN-LOANTYPE = "I"
                   OR POFF-INTDUE NOT = 0
              GO TO WHEN-PAYOFF-GOOD-EXIT.

           MOVE TRANS-DATE  TO SAVE-TRANS-DATE.
           MOVE POFF-NETDUE TO SAVE-POFF-NETDUE.

           MOVE TRANS-DATE TO NUM-DATE.
      * TODAY IS PRIOR TO SCHEDULED PAY THIS MONTH:
           IF NUM-DA < LN-1STPYDA
              MOVE LN-1STPYDA TO NUM-DA
              MOVE NUM-DATE   TO TRANS-DATE
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
                    MOVE -1         TO NDTE-HOLD
                    PERFORM INCREMENT-DAYS
                    MOVE NDTE-DATE  TO TRANS-DATE
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
           MOVE NDTE-DATE  TO TRANS-DATE.

           PERFORM SET-NEW-PAYOFF-DATE-WORKERS.

           ADD 1 TO CNT.

       WHEN-PAYOFF-GOOD-CNT.
           STRING "LOOP COUNT = " CNT "TRANS-DATE = " TRANS-DATE
                         DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.

       WHEN-PAYOFF-GOOD-EXIT.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-GOODTHRU(GOODTHRU-SUB).

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/RBACT.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON DLPOFF-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY DLPOFF-SCREEN UPON DLPOFF-WINDOW-HANDLE.
           MOVE DLPOFF-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE DLPOFF-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE DLPOFF-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLPOFF-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/DLPOFF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
       DISPLAY-ALL-LINES.
           MOVE LINE-BUFFER TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LOAN-BAL-INFO.
           MOVE "S  A080PODATE." TO VDU.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE SPACE TO BALANCE-BUF.
           MOVE PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO B-080.

           COMPUTE B-090 = LN-OTHBAL + LN-OT2BAL.
           ADD LN-INTBAL LN-LCBAL GIVING B-100.
           MOVE LN-CURBAL TO B-110.
           MOVE "S  A000PODESC." TO VDU.
           MOVE BALANCE-BUF TO VDUBUF.
           PERFORM SCREENIO.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY DLPOFF-SCREEN.
           DESTROY DLPOFF-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
