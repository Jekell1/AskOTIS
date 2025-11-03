       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNSTAT.
       DATE-WRITTEN.  10/16/96.
      ******************************************************************
      *
      *   DIR :  WI
      *   DESC:  LOAN STATUS ENTRY  (MANUAL CONVERSION)
      *
      *                  (WINDOW VERSION)
      *
      * NOTE: LN-REC IS PASSED IN VIA LINKAGE
      *       NO I/O IS PERFORMED ON THE LNFILE WITHIN THIS WINDOW.
      *
      *=================================================================
      * REV:
      * KEC 081997 ADDED CHECK IF ACCOUNT IS IN A VALID 'REPO' STATUS
      *            FOR WHEN ENTERING A 'REPO STATUS' CODE.  A REQUEST
      *            FROM CINDY!
      * KEC 090897 REVISED THE SCREEN FORMAT; MOVED #8 TO #12, MOVE #9
      *            THRU #12 BACK ONE TO #8 THRU #11.
      * CS  092597 WAS ALLOWING REPO STATUS W/O CHECKING INTEREST REBATED FLAG
      * CS  092697 CHANGED TEST FOR INPUT OF REPO TO USE LN-LOANTYPE INSTEAD
      *            OF IBPC-FG
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * BAH 022399 KEC'S CHANGE FROM 090897 DID NOT ADJUST PROMPTS AT
      *            BOTTOM OF SCREEN, LNSTAT.CMD !!
      * BAH 000914 WAS TESTING BRANCH FLAGS BUT NEVER READ THE FILE !!
      *            ANDREA AT SMOKY MOUNTAIN FINANCE
      * JTG 010329 CORRECTED SET OF LN-ANTICERN() AND LN-ACCERN() WHEN
      *            ENTRY OF REBATE FLAG AND REFUND AMOUNTS WERE ENTERED
      *            RE: LN-ANTICERN IS ALWAYS REDUCED BY REFUNDED AMOUNT
      *                FROM IT'S ORIGINAL VALUE             WOELD #258
      * BAH 061006 ALLOW BANKRUPT CHAPTER 12, WORLD PR# 497
      *  CS 161128 CHANGED ORIGINAL-REC PIC X 2800
      * BAH 170419 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0530 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 190128 SPLIT CDFILE
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
           "@(#)WI/LNSTAT.CBL S35 02/23/24-11:39:23 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *=================================================================
      *    ORIGINAL-LN-REC *NEEDS* TO BE HARDCODED DUE TO LINKAGE.
      *    LN-SIZE NOT USABLE FOR ORIGINAL-LN-REC BECAUSE
      *    LIBLP/LP01LN IS IN LINKAGE.
      *=================================================================
       01  ORIGINAL-LN-REC             PIC X(2800)    VALUE SPACES.

      ******************************************************************
       01  ERRCD             PIC X VALUE " ".
       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.

       01  LEGEND            PIC X(70).
       01  LEGEND-LIST       PIC X(8)   VALUE "WSEL:D.".

      *    -------------------------------------------------------------
      *    IF ACCOUNT HAS BALANCES THEN WINDOW IS FOR VIEWING ONLY,
      *       NO UPDATES ALLOWED.
      *    -------------------------------------------------------------
       01  VIEW-STATUS       PIC X.
           88  VIEW-ONLY     VALUE "Y".

      *    -------------------------------------------------------------
      *    TABLE USED TO SORT DATES IN POSTED ORDER.
      *    -------------------------------------------------------------
       01  SORT-TABLE   OCCURS 8.
           03  SORT-DATE     PIC 9(8).
           03  SORT-TYPE     PIC X.
       01  TEMP-TYPE         PIC X.
       01  HOLD-WORKER       PIC S9(8)V99.

      *    -------------------------------------------------------------
      *    SORT TABLE SUBSCRIPTS.
      *    -------------------------------------------------------------
       01  X1                PIC 99.
       01  Y1                PIC 99.
       01  Z1                PIC 99.

      ******************************************************************
      * SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC X.
           03  F02        PIC 99/99/99     BLANK ZERO.
           03  F03        PIC 99/99/99     BLANK ZERO.
           03  F04        PIC 99/99/99     BLANK ZERO.
           03  F041       PIC Z9           BLANK ZERO.
           03  F05        PIC 99/99/99     BLANK ZERO.
           03  F051       PIC ZZ9.999      BLANK ZERO.
           03  F06        PIC 99/99/99     BLANK ZERO.
           03  F061       PIC ZZ9.999      BLANK ZERO.
           03  F07        PIC 99/99/99     BLANK ZERO.
           03  F071       PIC ZZ9.999      BLANK ZERO.
           03  F08        PIC X.
           03  F081       PIC X.
           03  F082       PIC 99/99/99     BLANK ZERO.
           03  F083       PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  F09        PIC X.
           03  F091       PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  F10        PIC X.
           03  F101       PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  F11        PIC X.
           03  F111       PIC ZZZ,ZZ9.99   BLANK ZERO.
           03  F12        PIC X.
           03  F122       PIC 99/99/99     BLANK ZERO.
           03  F123       PIC 99/99/99     BLANK ZERO.
           03  F124       PIC 99/99/99     BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER     PIC X(52) VALUE
               "010 020 030 040 041 050 051 060 061 070 071 080 081 ".
           03  FILLER     PIC X(52) VALUE
               "082 083 090 091 100 101 110 111 120 122 123 124.".

      ******************************************************************
      * ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX            PIC 99   VALUE 12.
       01  SPEC-TABLE.
      *  1. LN-CONFIDENTIAL
           03  FILLER     PIC X(8) VALUE "E NA010 ".
      *  2. LN-DEBTAIDATE
           03  FILLER     PIC X(8) VALUE "ERNM060 ".
      *  3. LN-LEGALDATE
           03  FILLER     PIC X(8) VALUE "ERNM060 ".
      *  4. LN-BNKRPTDATE
           03  FILLER     PIC X(8) VALUE "ERNM060 ".
      *  5. LN-JDDATE
           03  FILLER     PIC X(8) VALUE "ERNM060 ".
      *  6. LN-ACCELDATE
           03  FILLER     PIC X(8) VALUE "ERNM060 ".
      *  7. LN-REDUDATE
           03  FILLER     PIC X(8) VALUE "ERNM060 ".
      *  8. LN-PLCD
           03  FILLER     PIC X(8) VALUE "E NA010 ".
      *  9. LN-INT-REFUND-FG
           03  FILLER     PIC X(8) VALUE "E NA010 ".
      * 10. LN-SC-REFUND-FG
           03  FILLER     PIC X(8) VALUE "E NA010 ".
      * 11. LN-MF-REFUND-FG
           03  FILLER     PIC X(8) VALUE "E NA010 ".
      * 12. LN-REPOCD
           03  FILLER     PIC X(8) VALUE "E NA010 ".
       01  SPEC-TAB      REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 12 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       01  LEGEND-INFO.
           03  LEGEND-A-01.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER CONFIDENTIAL CODE ".
               05  FILLER           PIC X(35)  VALUE
           " (Y/N):".
           03  LEGEND-A-02.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER DEBTAID DATE:".
               05  FILLER           PIC X(35)  VALUE
           " ".
           03  LEGEND-A-03.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER LEGAL DATE:".
               05  FILLER           PIC X(35)  VALUE
           " ".
           03  LEGEND-A-04.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER BANKRUPT DATE:".
               05  FILLER           PIC X(35)  VALUE
           " ".
           03  LEGEND-A-05.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER JUDGEMENT DATE:".
               05  FILLER           PIC X(35)  VALUE
           " ".
           03  LEGEND-A-06.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER ACCELLERATION DATE".
               05  FILLER           PIC X(35)  VALUE
           ":".
           03  LEGEND-A-07.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER INTEREST REDUCTION".
               05  FILLER           PIC X(35)  VALUE
           " DATE:".
           03  LEGEND-A-08.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER P&L CODE:".
               05  FILLER           PIC X(35)  VALUE
           " ".
           03  LEGEND-A-09.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER INTEREST REFUND FL".
               05  FILLER           PIC X(35)  VALUE
           "AG:".
           03  LEGEND-A-10.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER SERVICE CHARGE REF".
               05  FILLER           PIC X(35)  VALUE
           "UND FLAG:".
           03  LEGEND-A-11.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER MAINTENANCE FEE RE".
               05  FILLER           PIC X(35)  VALUE
           "FUND FLAG:".
           03  LEGEND-A-12.
               05  FILLER           PIC X(35)  VALUE
           " F1-RESET, ENTER REPO STATUS  (X/V/".
               05  FILLER           PIC X(35)  VALUE
           "S):".
       01  LEGEND-TABLE             REDEFINES LEGEND-INFO.
           03  LEGEND-TAB           OCCURS 12 TIMES
                                    PIC X(70).

      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LNSTAT_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/LNSTAT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01LN.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LNSTAT_SCN.CPY".
      
       PROCEDURE DIVISION
            USING FORM-PATHNAME LN-REC EXIT-PATHNAME.

      ******************************************************************
      * MAIN PROGRAM MODULE
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE 22 TO WINDOW-LINES.
           MOVE 78 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "WI/LNSTAT" TO WINDOW-FORM-NAM.
           MOVE "LNSTAT" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.

           MOVE LN-REC TO ORIGINAL-LN-REC.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      * IF ACCOUNT HAS ACTIVE BALANCES THEN DO NOT ALLOW USER
      * TO UPDATE/CHANGE RECORD.

           PERFORM TEST-BAL.
           IF VIEW-ONLY
              MOVE "N" TO CHFG(1) CHFG(2)  CHFG(3)  CHFG(4)
                          CHFG(5) CHFG(6)  CHFG(7)  CHFG(8)
                          CHFG(9) CHFG(10) CHFG(11) CHFG(12)
           ELSE
              MOVE " " TO CHFG(1) CHFG(2)  CHFG(3)  CHFG(4)
                          CHFG(5) CHFG(6)  CHFG(7)  CHFG(8)
                          CHFG(9) CHFG(10) CHFG(11) CHFG(12).

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE ORIGINAL-LN-REC TO LN-REC.
           PERFORM DISPLAY-FIELDS.

           PERFORM REC-LN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      ******************************************************************
       REC-LN-MODULE SECTION.
      ******************************************************************
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.
       END-REC-LN-MODULE.
           EXIT.

      ******************************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
           IF VIEW-ONLY
              MOVE " F7-EXIT:" TO LEGEND
           ELSE
              MOVE " F1-RESET, F7-EXIT, ENTER FIELD #:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF NOT VIEW-ONLY
              IF VDUSTATUS = "10"
                 MOVE "CLR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              IF VIEW-ONLY
                 MOVE "END" TO IO-TYPE
              ELSE
                 MOVE "RWR" TO IO-TYPE
              END-IF
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                   GO TO MODIFY-ROUTINE.

           MOVE LEGEND-TAB(ELE) TO LEGEND.
           PERFORM SEND-LEGEND.
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
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06
                 A-07 A-08 A-09 A-10 A-11 A-12
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.


      ******************************************************************
      * SET RECORD ELEMENTS
      ******************************************************************
       A-01.
           MOVE VDUBUF TO LN-CONFIDENTCD.
           IF NOT (LN-CONFIDENTCD = "Y" OR "N")
              MOVE "MUST ENTER 'Y', 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDU-DATE-YYYYMMDD TO LN-DEBTAIDATE.
           IF LN-DEBTAIDATE NOT = 0
              IF LN-DEBTAIDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDU-DATE-YYYYMMDD TO LN-LEGALDATE.
           IF LN-LEGALDATE NOT = 0
              IF LN-LEGALDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDU-DATE-YYYYMMDD TO LN-BNKRPTDATE.
           IF LN-BNKRPTDATE NOT = 0
              IF LN-BNKRPTDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF LN-BNKRPTDATE = 0
              MOVE "S  N020041." TO VDU
              MOVE 0 TO LN-BNKRPT-CHAPTER VDUNUM0
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-041.
           MOVE " F1-RESET, ENTER THE BANKRUPT CHAPTER (7/11/12/13):"
                                    TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NN020041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-041.
           MOVE VDUNUM0 TO LN-BNKRPT-CHAPTER.
           IF NOT (LN-BNKRPT-CHAPTER = 7 OR 11 OR 12 OR 13)
              MOVE "CHAPTER MUST BE 7, 11, 12 OR 13" TO MESS
              PERFORM SEND-MESS
              GO TO A-041.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDU-DATE-YYYYMMDD TO LN-JDDATE.
           IF LN-JDDATE NOT = 0
              IF LN-JDDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF LN-JDDATE = 0
              MOVE "S  N033051." TO VDU
              MOVE 0 TO LN-JDRATE VDUNUM3
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-051.
           MOVE " F1-RESET, ENTER JUDGEMENT RATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN033051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-051.
           MOVE VDUNUM3 TO LN-JDRATE.
           GO TO CREATE-ROUTINE.

       A-06.
           MOVE VDU-DATE-YYYYMMDD TO LN-ACCELDATE.
           IF LN-ACCELDATE NOT = 0
              IF LN-ACCELDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF LN-ACCELDATE = 0
              MOVE "S  N033061." TO VDU
              MOVE 0 TO LN-ACCRATE VDUNUM3
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-061.
           MOVE " F1-RESET, ENTER ACCELLERATION RATE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN033061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-061.
           MOVE VDUNUM3 TO LN-ACCRATE.
           IF LN-ACCRATE >= 999.999
              MOVE "RATE CANNOT EXCEED 999.998" TO MESS
              PERFORM SEND-MESS
              GO TO A-061.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDU-DATE-YYYYMMDD TO LN-REDUDATE.
           IF LN-REDUDATE NOT = 0
              IF LN-REDUDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF LN-REDUDATE = 0
              MOVE "S  N033071." TO VDU
              MOVE 0 TO LN-REDURATE VDUNUM3
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-071.
           MOVE " F1-RESET, ENTER INTEREST REDUCTION RATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN033071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-071.
           MOVE VDUNUM3 TO LN-REDURATE.
           IF LN-REDURATE >= 999.997
              MOVE "RATE CANNOT EXCEED 999.996" TO MESS
              PERFORM SEND-MESS
              GO TO A-071.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO LN-PLCD.
           IF NOT (LN-PLCD = " " OR "P")
              GO TO ENTER-ELE.
           IF LN-PLCD = " "
               MOVE "S NA010081." TO VDU
               MOVE " " TO LN-PLTYPE VDUBUF
               PERFORM SCREENIO
               MOVE "S  A060082." TO VDU
               MOVE 0 TO LN-PLDATE VDU-DATE-EDIT
               PERFORM SCREENIO
               MOVE "S  N062083." TO VDU
               MOVE 0 TO LN-PLAMT VDUNUM0
               PERFORM SCREENIO
               GO TO CREATE-ROUTINE.
       A-081.
           MOVE " F1-RESET, ENTER P&L TYPE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NA010081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-081.

           MOVE "PX"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE LN-PLTYPE.
           IF LN-PLTYPE NOT = "P"
              PERFORM GET-CD
              IF IO-FG NOT = 0
                 MOVE "INVALID P&L TYPE" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-081.
       A-082.
           MOVE " F1-RESET, ENTER P&L DATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NM060082." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-081.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-082.
           MOVE VDU-DATE-YYYYMMDD TO LN-PLDATE.
           IF LN-PLDATE NOT = 0
              IF LN-PLDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO A-082.
       A-083.
           MOVE " F1-RESET, ENTER P&L AMOUNT:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NN062083." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-082.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-083.
           MOVE VDUNUM2 TO LN-PLAMT.
           IF LN-PLAMT = 0
              MOVE "MUST ENTER DOLLAR AMOUNT" TO MESS
              PERFORM SEND-MESS
              GO TO A-083.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUBUF TO LN-INT-REBATE.
           IF NOT (LN-INT-REBATE = " " OR "Y" OR "N")
              MOVE "MUST ENTER 'Y' OR 'N' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LN-INT-REBATE = " " OR "N"
              MOVE "S  N062091." TO VDU
              MOVE 0 TO LN-ANTICADJ(1) VDUNUM2
                        LN-ACCERN(1)
              COMPUTE LN-ANTICERN(1) = LN-INTCHG + LN-EXTCHG
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-091.
           MOVE " ENTER INTEREST REFUND AMOUNT:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NN062091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-091.
           MOVE VDUNUM2 TO LN-ANTICADJ(1).
           COMPUTE HOLD-WORKER = LN-INTCHG + LN-EXTCHG.
           IF LN-ANTICADJ(1) > HOLD-WORKER
              PERFORM AMOUNT-EXCEEDED
              GO TO A-091.
           SUBTRACT LN-ANTICADJ(1) FROM HOLD-WORKER
                                        GIVING LN-ANTICERN(1)
                                               LN-ACCERN(1).
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUBUF TO LN-SC-REBATE.
           IF NOT (LN-SC-REBATE = " " OR "Y" OR "N")
              MOVE "MUST ENTER 'Y' OR 'N' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LN-SC-REBATE = " " OR "N"
              MOVE "S  N062101." TO VDU
              MOVE 0 TO LN-ANTICADJ(3) VDUNUM2
                        LN-ACCERN(3)
              MOVE LN-SERCHG TO LN-ANTICERN(3)
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-101.
           MOVE " ENTER SERVICE CHARGE REFUND AMOUNT:"
                                            TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NN062101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-101.
           MOVE VDUNUM2 TO LN-ANTICADJ(3).
           IF LN-ANTICADJ(3) > LN-SERCHG
              PERFORM AMOUNT-EXCEEDED
              GO TO A-101.
           SUBTRACT LN-ANTICADJ(3) FROM LN-SERCHG
                                        GIVING LN-ANTICERN(3)
                                               LN-ACCERN(3).
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO LN-MF-REBATE.
           IF NOT (LN-MF-REBATE = " " OR "Y" OR "N")
              MOVE "MUST ENTER 'Y' OR 'N' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LN-MF-REBATE = " " OR "N"
              MOVE "S  N062111." TO VDU
              MOVE 0 TO LN-ANTICADJ(4) VDUNUM2
                        LN-ACCERN(4)
              COMPUTE LN-ANTICERN(4) = LN-MAINTFEE * LN-ORGTERM
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-111.
           MOVE " ENTER MAINTENANCE FEE REFUND AMOUNT:"
                                               TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NN062111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-111.
           MOVE VDUNUM2 TO LN-ANTICADJ(4).
           COMPUTE HOLD-WORKER = LN-MAINTFEE * LN-ORGTERM
           IF LN-ANTICADJ(4) > HOLD-WORKER
              PERFORM AMOUNT-EXCEEDED
              GO TO A-111.
           SUBTRACT LN-ANTICADJ(4) FROM HOLD-WORKER
                                        GIVING LN-ANTICERN(4)
                                               LN-ACCERN(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-12.
           MOVE VDUBUF TO LN-REPOCD.

           IF NOT (LN-REPOCD = " " OR "X" OR "V" OR "S")
              GO TO ENTER-ELE.

      * ACCOUNT IS NOT REPO THEREFORE CLEAR REMAINING FIELDS
           IF LN-REPOCD = " "
              MOVE "S  A080122." TO VDU
              MOVE 0 TO LN-REPODATE VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE "S  A080123." TO VDU
              MOVE 0 TO LN-REDEEMDATE VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE "S  A080124." TO VDU
              MOVE 0 TO LN-SOLDDATE VDU-DATE-EDIT
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

      * CHECK IF ACCOUNT IS IN A VALID 'REPO' STATUS.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM GET-SP-RECORD.

           MOVE LN-DATE-PAID-LAST TO IBPC-DATE.
           PERFORM IBPC-TEST.

           IF ( BR-AC-BEFORE-REPO NOT = "N" ) AND
              ( LN-LOANTYPE = "P"           ) AND
              ( LN-ACCELDATE = ZEROES       )
              MOVE "ACCOUNT MUST BE ACCELERATED BEFORE SETTING REPO"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE
           ELSE
           IF ( BR-RI-BEFORE-REPO NOT = "N"      ) AND
              (LN-LOANTYPE = "P") AND
              ( NOT (LN-INT-REBATE = "Y" OR "R") )
              MOVE "INTEREST MUST BE REBATED BEFORE SETTING REPO"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

       A-122.
           MOVE " F1-RESET, ENTER ONHAND DATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060122." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-122.
           MOVE VDU-DATE-YYYYMMDD TO LN-REPODATE.
           IF LN-REPODATE NOT = 0
              IF LN-REPODATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO A-122.
           IF LN-REPODATE = 0
              MOVE "MUST ENTER ONHAND DATE" TO MESS
              PERFORM SEND-MESS
              GO TO A-122.
       A-123.
           MOVE " F1-RESET, ENTER REDEEMED DATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060123." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-123.
           MOVE VDU-DATE-YYYYMMDD TO LN-REDEEMDATE.
           IF LN-REDEEMDATE NOT = 0
              IF LN-REDEEMDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO A-123.
           IF LN-REPOCD = "V"
              IF LN-REDEEMDATE = 0
                 MOVE "MUST ENTER REDEEMED DATE FOR 'V' STATUS" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-123.
       A-124.
           MOVE " F1-RESET, ENTER SOLD DATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060124." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-124.
           MOVE VDU-DATE-YYYYMMDD TO LN-SOLDDATE.
           IF LN-SOLDDATE NOT = 0
              IF LN-SOLDDATE < LN-LOANDATE
                 MOVE "DATE CANNOT BE PRIOR TO LOAN DATE." TO MESS
                 PERFORM SEND-MESS
                 GO TO A-124.
           IF LN-REPOCD = "S"
              IF LN-SOLDDATE = 0
                 MOVE "MUST ENTER SOLD DATE FOR 'S' STATUS" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-124.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

      * WE CANNOT ACTUALLY WRITE THE LOAN RECORD HERE SO WE UPDATE
      * ORIGINAL REC TO SIMULATE THE WRITE.

            PERFORM UPDATE-STATUSFG.
            MOVE LN-REC TO ORIGINAL-LN-REC.

        END-WRITE-MODULE.
            EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE LN-CONFIDENTCD    TO F01.
           MOVE LN-DEBTAIDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F02.
           MOVE LN-LEGALDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F03.
           MOVE LN-BNKRPTDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F04.
           MOVE LN-BNKRPT-CHAPTER TO F041
           MOVE LN-JDDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F05.
           MOVE LN-JDRATE         TO F051
           MOVE LN-ACCELDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F06.
           MOVE LN-ACCRATE        TO F061.
           MOVE LN-REDUDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F07.
           MOVE LN-REDURATE       TO F071.
           MOVE LN-PLCD           TO F08.
           MOVE LN-PLTYPE         TO F081.
           MOVE LN-PLDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F082.
           MOVE LN-PLAMT          TO F083.
           MOVE LN-INT-REBATE     TO F09.
           MOVE LN-ANTICADJ(1)    TO F091.
           MOVE LN-SC-REBATE      TO F10.
           MOVE LN-ANTICADJ(3)    TO F101.
           MOVE LN-MF-REBATE      TO F11.
           MOVE LN-ANTICADJ(4)    TO F111.
           MOVE LN-REPOCD         TO F12.
           MOVE LN-REPODATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F122.
           MOVE LN-REDEEMDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F123.
           MOVE LN-SOLDDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F124.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       SETUP-UPDATE-ORDER-TBL SECTION.
      ******************************************************************

      * A SORTED TABLE OF DATES IS USED TO INSURE THAT LN-STATUSFLAGS
      * ARE SET IN THE PROPER POSTED ORDER.

           MOVE ZEROS TO  SORT-DATE(1)   SORT-DATE(4)
                          SORT-DATE(2)   SORT-DATE(5)
                          SORT-DATE(3)   SORT-DATE(6).
           MOVE SPACES TO SORT-TYPE(1)   SORT-DATE(4)
                          SORT-TYPE(2)   SORT-TYPE(5)
                          SORT-TYPE(3)   SORT-TYPE(6).

      * MOVE NEW LOAN STATUSES TO TABLE TO BE SORTED BY DATE

           MOVE 1 TO X1.
           IF LN-DEBTAIDATE NOT = 0
              MOVE LN-DEBTAIDATE TO SORT-DATE(X1)
              MOVE "D" TO SORT-TYPE(X1)
              ADD 1 TO X1.

           IF LN-LEGALDATE NOT = 0
              MOVE LN-LEGALDATE TO SORT-DATE(X1)
              MOVE "L" TO SORT-TYPE(X1)
              ADD 1 TO X1.

           IF LN-BNKRPTDATE NOT = 0
              MOVE LN-BNKRPTDATE TO SORT-DATE(X1)
              MOVE "B" TO SORT-TYPE(X1)
              ADD 1 TO X1.

           IF LN-JDDATE NOT = 0
              MOVE LN-JDDATE TO SORT-DATE(X1)
              MOVE "J" TO SORT-TYPE(X1)
              ADD 1 TO X1.

           IF LN-ACCELDATE NOT = 0
              MOVE LN-ACCELDATE TO SORT-DATE(X1)
              MOVE "A" TO SORT-TYPE(X1)
              ADD 1 TO X1.

           IF LN-REDUDATE NOT = 0
              MOVE LN-REDUDATE TO SORT-DATE(X1)
              MOVE "R" TO SORT-TYPE(X1)
              ADD 1 TO X1.

           IF LN-REPOCD  = "X"
              MOVE LN-REPODATE TO SORT-DATE(X1)
              MOVE "S" TO SORT-TYPE(X1)
              ADD 1 TO X1
           ELSE
              IF LN-REPOCD = "V"
                 MOVE LN-REDEEMDATE TO SORT-DATE(X1)
                 MOVE "S" TO SORT-TYPE(X1)
                 ADD 1 TO X1
              ELSE
                 IF LN-REPOCD = "S"
                    MOVE LN-SOLDDATE TO SORT-DATE(X1)
                    MOVE "S" TO SORT-TYPE(X1)
                    ADD 1 TO X1.

           IF LN-PLDATE NOT = 0
              MOVE LN-PLDATE TO SORT-DATE(X1)
              MOVE "P" TO SORT-TYPE(X1)
              ADD 1 TO X1.

      * SORT SORT-TABLE FROM OLDEST TO NEWEST POSTING DATE.

       SORT-TBL-BY-DATE.
              PERFORM VARYING X1 FROM 1 BY 1 UNTIL X1 > 7
                 COMPUTE Z1 = X1 + 1
                 PERFORM VARYING Y1 FROM Z1 BY 1 UNTIL Y1 > 8
                    IF SORT-TYPE(X1) NOT = SPACES
                       IF SORT-DATE(X1) > SORT-DATE(Y1)
                          MOVE SORT-DATE(X1) TO SORT-DATE(Y1)
                          MOVE SORT-DATE(Y1) TO SORT-DATE(X1)
                          MOVE SORT-TYPE(Y1) TO TEMP-TYPE
                          MOVE SORT-TYPE(X1) TO SORT-TYPE(Y1)
                          MOVE TEMP-TYPE TO SORT-TYPE(X1)
                       END-IF
                    END-IF
                 END-PERFORM
              END-PERFORM.

      *****************************************
       UPDATE-STATUSFG SECTION.
      *****************************************

           PERFORM SETUP-UPDATE-ORDER-TBL.

           MOVE SPACES TO LN-STATFLAGS.

           PERFORM VARYING X1 FROM 1 BY 1 UNTIL X1 > 8
              IF      SORT-TYPE(X1) = "D" PERFORM UPDATE-D-STATUSFG
              ELSE IF SORT-TYPE(X1) = "L" PERFORM UPDATE-L-STATUSFG
              ELSE IF SORT-TYPE(X1) = "B" PERFORM UPDATE-B-STATUSFG
              ELSE IF SORT-TYPE(X1) = "J" PERFORM UPDATE-J-STATUSFG
              ELSE IF SORT-TYPE(X1) = "A" PERFORM UPDATE-A-STATUSFG
              ELSE IF SORT-TYPE(X1) = "R" PERFORM UPDATE-R-STATUSFG
              ELSE IF SORT-TYPE(X1) = "S" PERFORM UPDATE-S-STATUSFG
              ELSE IF SORT-TYPE(X1) = "P" PERFORM UPDATE-P-STATUSFG
              END-IF END-IF END-IF END-IF END-IF END-IF END-IF END-IF
           END-PERFORM.

           MOVE 8 TO X1.
           MOVE " " TO LN-STATUSFG.
       SET-STATUSFG.
           IF SORT-TYPE(X1) = "J"
              MOVE "J" TO LN-STATUSFG
              GO TO END-UPDATE-STATUSFG
           ELSE
              IF SORT-TYPE (X1) = "A"
                 MOVE "A" TO LN-STATUSFG
                 GO TO END-UPDATE-STATUSFG
               ELSE
                  IF SORT-TYPE(X1) = "R"
                     MOVE "R" TO LN-STATUSFG
                     GO TO END-UPDATE-STATUSFG
                  END-IF
               END-IF
           END-IF
           SUBTRACT 1 FROM X1
           IF X1 > 0
              GO TO SET-STATUSFG.

           GO TO END-UPDATE-STATUSFG.

      * DEBTAID
       UPDATE-D-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
             AT END PERFORM PREPARE-TO-SHIFT
             WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-DEBTAIDATE NOT = 0)
                MOVE "D" TO LN-STATUSFLAGS(STFG).

      * LEGAL STATUS
       UPDATE-L-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
              AT END PERFORM PREPARE-TO-SHIFT
              WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-LEGALDATE NOT = 0)
                    MOVE "L" TO LN-STATUSFLAGS(STFG).

      * BANKRUPTCY
       UPDATE-B-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
             AT END PERFORM PREPARE-TO-SHIFT
             WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-BNKRPTDATE NOT = 0)
                   MOVE "B" TO LN-STATUSFLAGS(STFG).

      * JUDGEMENT
       UPDATE-J-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
              AT END PERFORM PREPARE-TO-SHIFT
              WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-JDDATE NOT = 0)
                    MOVE "J" TO LN-STATUSFLAGS(STFG).

      * ACCELLERATION
       UPDATE-A-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
              AT END PERFORM PREPARE-TO-SHIFT
              WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-ACCELDATE NOT = 0)
                    MOVE "A" TO LN-STATUSFLAGS(STFG).

      * INTEREST REDUCTION
       UPDATE-R-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
              AT END PERFORM PREPARE-TO-SHIFT
              WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-REDUDATE NOT = 0)
                    MOVE "R" TO LN-STATUSFLAGS(STFG).

      * REPOSSESSIONS
       UPDATE-S-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
             AT END PERFORM PREPARE-TO-SHIFT
             WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-REPOCD = "X")
                   MOVE "X" TO LN-STATUSFLAGS(STFG)
             WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-REPOCD = "V")
                   MOVE "V" TO LN-STATUSFLAGS(STFG)
             WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-REPOCD = "S")
                   MOVE "S" TO LN-STATUSFLAGS(STFG).

      * P&L
       UPDATE-P-STATUSFG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
             AT END PERFORM PREPARE-TO-SHIFT
             WHEN (LN-STATUSFLAGS(STFG) = " " AND LN-PLDATE NOT = 0)
                   MOVE LN-PLTYPE TO LN-STATUSFLAGS(STFG).

       END-UPDATE-STATUSFG.
           EXIT.

      ************************************
       PREPARE-TO-SHIFT SECTION.
      ************************************

      * IF NO ROOM IS LEFT IN LN-STATUSFLAGS THEN SHIFT EVERYTHING LEFT
      * ONE BYTE. (LEFTMOST STATUS IS LOST)

           SET STFG TO 1.
       SHIFT-LOOP.
           MOVE LN-STATUSFLAGS(STFG + 1) TO LN-STATUSFLAGS(STFG).
           SET STFG UP BY 1.
           IF STFG < 10
              GO TO SHIFT-LOOP.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           DISPLAY LNSTAT-SCREEN.
           MOVE LNSTAT-FORM-FIELDS TO WR-BUF.
           MOVE LNSTAT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LNSTAT-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNSTAT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LNSTAT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

      *-----------------------------------------------------------------
       GET-SP-RECORD.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.

           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           PERFORM CLOSE-SP1-FILE.

      *-----------------------------------------------------------------
       TEST-BAL.
           MOVE "Y" TO VIEW-STATUS.
           IF LN-CURBAL = 0
              IF LN-INTBAL = 0
                 IF LN-LCBAL = 0
                    IF LN-OTHBAL = 0
                        MOVE "N" TO VIEW-STATUS.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       AMOUNT-EXCEEDED.
           MOVE "AMOUNT REBATED CANNOT EXCEED AMOUNT CHARGED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNSTAT-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
