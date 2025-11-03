       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHDEFC.
       DATE-WRITTEN. 08/22/05.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   DEFICIENCY INFORMATION WINDOW
      *                 CALLED FROM F9 INTERRUPT
      *
      *         WRITTEN AND PAID FOR BY REGACC
      *
      *  REV:
      *   BAH 050822 REGACC PR# 112
      *   BAH 050912 REGACC, REDESIGNED....
      *   BAH 051026 MOVE LS-X-10DAY-COST TO D-OWED, INSTEAD OF CURBAL
      *   BAH 051108 LS-D-INSURANCE-PROCEEDS WAS TRUNCATING 2 DIGITS
      *              BECAUSE OF MOVING VDUNUM0 INSTEAD OF VDUNUM2
      *   BAH 060724 DISPLAY LS-D-DEF-LETTER-SENT, REGACC PR# 138
      *   BAH 160125 SPLIT OUT LTFILE, PD0003
      *   BAH 160209 SPLIT OUT LSFILE, PD0003
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      * BAH 170411 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181115 GLOBAL FMFILE
      * BAH 181130 GLOBAL LMFILE
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 190513 NEW CHFILE
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20191007 REMOVED ACCESS-CALL FMFILE
      * BAH 2024.0201 REMOVED NOTICE FOR A FORM REQUEST
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CHDEFC.CBL S35 02/22/24-08:57:33 >".
      ******************************************************************
      *
      *     P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LSD.CPY".
       COPY "LIBLP/LP01LSD_SQL.CPY".
       COPY "LIBLP/LPWSLSD.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  CHDEFC-LINES              PIC 99  VALUE 22.
       01  CHDEFC-COLUMNS            PIC 99  VALUE 78.
       01  CHDEFC-BEGIN-Y            PIC 99  VALUE 2.
       01  CHDEFC-BEGIN-X            PIC 99  VALUE 2.

       01  ERRCD           PIC X       VALUE SPACES.
       01  LEGEND          PIC X(70).
       01  LEGEND-01.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F7-UPDATE, ENTER FIELD #:".

       01  SAVE-LM1-KEY                 VALUE ZEROES.
           03  SAVE-LM-BRNO            PIC 9(04).
           03  SAVE-LM-ACCTNO          PIC 9(08).
           03  SAVE-LM-DATE            PIC 9(08).
           03  SAVE-LM-SEQ             PIC 9(03).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-CHAR   PIC X(24).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).
      *-----------------------------------------------------------------
       01  BRANCH            PIC 9(4)  VALUE 0.
       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

       01  NEW-FLAG          PIC 9.
       01  FORM-NUMBER       PIC 9(3)  VALUE 0.

       01  HOLD-LSD1-KEY.
           03  HOLD-BRNO     PIC 9(4).
           03  HOLD-ACCTNO   PIC 9(8).
           03  HOLD-SEQNO    PIC 99.

       01  HOLD-SYS-DATE   PIC 9(8).

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LSD.CPY" REPLACING LEADING "LSD" BY "LSD-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LSD.CPY" REPLACING LEADING "LSD" BY "LSD-CH".


       01  DISPLAY-BUF1      VALUE SPACES.
           03  D-NUMBER          PIC Z(6).
           03  D-NAME            PIC X(30).
           03  D-YEAR            PIC X(4).
           03  D-MAKE            PIC X(15).
           03  D-MODEL           PIC X(15).
           03  D-VIN             PIC X(20).
           03  D-REPODATE        PIC ZZ/ZZ/ZZ.
           03  D-OWED            PIC ZZZZZ9.99.
           03  D-SALES           PIC ZZZZZ9.99.
           03  D-AUCTION-EXP     PIC ZZZZZ9.99.
           03  D-REPO-EXP        PIC ZZZZZ9.99.

       01  DISPLAY-LIST1.
           03  FILLER      PIC X(46) VALUE
           "ACCTNO NAME YEAR MAKE MODEL VIN REPODATE OWED ".
           03  FILLER      PIC X(21) VALUE
           "SALES AUCEXP REPOEXP.".

       01  DISPLAY-BUF      VALUE SPACES.
      * AMOUNT FROM OWING
           03  D-01              PIC ZZZZZ9.99.
      * INSURANCE COMPANY
           03  D-02              PIC X(20).
      * STOLEN
           03  D-03              PIC ZZ/ZZ/ZZ.
      * INSURANCE PROCEEDS
           03  D-04              PIC ZZZZZ9.99.
      * GAP PAID
           03  D-05              PIC ZZZZZ9.99.
      * DATE CHECK RECEIVED
           03  D-06              PIC ZZ/ZZ/ZZ.
      * OTHER EXP. (EARNED CPP)
           03  D-07              PIC ZZZZZ9.99.
      * OTHER (EXPENSE) DESCRIPTION
           03  D-08              PIC X(15).
      * OTHER (EXPENSE)
           03  D-081             PIC ZZZZZ9.99.
      * TOTAL EXPENSES
           03  D-09              PIC ZZZZZ9.99.
      * INTEREST REBATES
           03  D-10              PIC ZZZZZ9.99.
      * CPP REBATES
           03  D-11              PIC ZZZZZ9.99.
      * GAP REBATES
           03  D-12              PIC ZZZZZ9.99.
      * WARRANTY REBATES
           03  D-13              PIC ZZZZZ9.99.
      * INSURANCE CHECK
           03  D-14              PIC ZZZZZ9.99.
      * OTHER (REBATES) DESCRIPTION
           03  D-15              PIC X(15).
      * OTHER (REBATES)
           03  D-151             PIC ZZZZZ9.99.
      * TOTAL CREDITS
           03  D-16              PIC ZZZZZ9.99.
      * AMT PAID ON YOUR BEHALF
           03  D-17              PIC ZZZZZ9.99.
      * AMT OF SURPLUS
           03  D-18              PIC ZZZZZ9.99.
      * AMT OF DEFICIENCY
           03  D-19              PIC ZZZZZ9.99.
      * SEND LETTER
           03  D-20              PIC ZZZ.
      * DATE LETTER SENT
           03  D-201             PIC 99/99/99 BLANK ZERO.
      * WHO ADJUSTED
           03  D-21              PIC X(20).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(44) VALUE
           "010 020 030 040 050 060 070 080 081 090 100 ".
           03  FILLER      PIC X(52) VALUE
           "110 120 130 140 150 151 160 170 180 190 200 201 210.".

      ******************************************************************
      *      ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      * 1 AMT FROM OWING
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 2 INSURANCE COMPANY
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 3 STOLEN DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 4 INSURANCE PROCEEDS
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 5 GAP PAID
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 6 DATE CHECK RECEIVED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 7  OTHER EXP. (EARNED CPP):
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 8   _____
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 9  TOTAL EXPENSES
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 10  INT REBATES
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 11  CPP REBATES
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 12  GAP REBATES
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 13  WARRANTY REBATES
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 14  INSURANCE CHECK
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 15   ____
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 16  TOTAL CREDITS
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 17  AMOUNT PAID
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 18  AMT FROM SURPLUS
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 19  AMT OF DEFICIENCY
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 20  SEND LETTER
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      * 21  WHO ADJUSTED
           03  FILLER      PIC X(8) VALUE "E  A200 ".

       01  SPEC-TAB      REDEFINES SPEC-TABLE.
           03  SPEC-REC  OCCURS 21 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       01  MAX           PIC 99   VALUE 21.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/PASSWDW.CPY".
       01  CREATE-SEQ              PIC 99 VALUE 01.
       01  MODIFY-SEQ              PIC 99 VALUE 02.
       01  DELETE-SEQ              PIC 99 VALUE 03.

       01  CHDEFC-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHDEFC_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHDEFC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHDEFC_SCN.CPY".

      ************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      /***********************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHDEFC" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BRANCH.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHDEFC-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHDEFC-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHDEFC-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHDEFC-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHDEFC-WINDOW-HANDLE.

           MOVE "S  A240TITLE:FR." TO VDU.
           MOVE "DEFICIENCY INFORMATION" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           MOVE 0 TO FORM-NUMBER.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE
             GO TO END-ROUTINE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      **************************
       REC-LS-MODULE SECTION.
      **************************
           MOVE SPACES TO IO-TYPE LSD-OR-REC.
           MOVE 0 TO NEW-FLAG.

           MOVE SPACES TO DISPLAY-BUF1.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           MOVE LN-ACCTNO TO D-NUMBER.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO D-NAME.

           PERFORM OPEN-LTC1-FILE.
           MOVE EXT-WI-CL-OWNBR  TO LTC-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LTC-ACCTNO.
           MOVE 1                TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-GOOD
              MOVE LTC-YEAR(1) TO D-YEAR
              MOVE LTC-MAKE(1) TO D-MAKE
              MOVE LTC-MODEL(1) TO D-MODEL
              MOVE LTC-VIN(1) TO D-VIN.
           PERFORM CLOSE-LTC1-FILE.

           PERFORM OPEN-LSX1-FILE.
           MOVE EXT-WI-CL-OWNBR  TO LSX-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSX-ACCTNO.
           MOVE 1                TO LSX-SEQNO.
           PERFORM READ-LSX1-FILE.
           IF IO-FG = 0
              MOVE LSX-REPODATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REPODATE
              MOVE LSX-SOLD-TO-AMT TO D-SALES
              MOVE LSX-AUCTION-EXPENSE TO D-AUCTION-EXP
              MOVE LSX-OTH-EXP-CKAMT(1) TO D-REPO-EXP
      * AMOUNT OWED AT DATE OF REPO
              MOVE LSX-10DAY-COST TO D-OWED.
           PERFORM CLOSE-LSX1-FILE.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-LSD1-FILE.

           MOVE EXT-WI-CL-OWNBR  TO LSD-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSD-ACCTNO.
           MOVE 1                TO LSD-SEQNO.

           MOVE LSD1-KEY         TO HOLD-LSD1-KEY.

           PERFORM READ-LSD1-FILE.
           PERFORM CLOSE-LSD1-FILE.
           IF IO-FG = 0
              MOVE LSD-REC TO LSD-OR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-LS-MODULE.

           PERFORM NEW-RECORD-SETUP.

           MOVE CREATE-SEQ TO PASSWDW-SEQ.
           MOVE "TO CREATE NEW RECORD:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LS-MODULE.

       END-REC-LS-MODULE.
           EXIT.

      ******************************************************************
      *    ENTRY-MODULE
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
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
           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

      * RECORD CAN ONLY BE DELETED WHILE AT PAGE 1.

           IF ACTION = "DELETE"
              MOVE DELETE-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW DELETE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE
              ELSE
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

      * TEST FOR PASSWORD TO BE ABLE TO MODIFY ANY FIELD...

           MOVE MODIFY-SEQ TO PASSWDW-SEQ.
           MOVE "TO ALLOW MODIFY:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE " F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.
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

       TEST-F6.
           IF ELE = 20
              IF VDUSTATUS = "1<"
                 PERFORM FORM-SCAN
                 IF IO-FG NOT = 0
                    GO TO ENTER-ELE.
       TEST-F7.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *          SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUNUM2 TO LSD-AMT-FROM-OWING.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO LSD-INS-COMPANY.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDU-DATE-YYYYMMDD TO LSD-STOLEN-DATE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO LSD-INSURANCE-PROCEEDS.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO LSD-GAP-INS-PAID.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDU-DATE-YYYYMMDD TO LSD-DATE-CHECK-RECV.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO LSD-OTHER-EXPENSE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO LSD-MISC-DESC-EXPENSE.
       F-081.
           MOVE "ENNN062081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-081.
           MOVE VDUNUM2 TO LSD-MISC-AMT-EXPENSE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO LSD-TOT-EXPENSE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO LSD-INT-REB.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM2 TO LSD-CPP-REB.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO LSD-GAP-REB.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM2 TO LSD-WARRANTY-REB.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM2 TO LSD-INSURANCE-CHECK.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO LSD-MISC-DESC-CREDIT.
       F-151.
           MOVE "ENNN062151." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-151.
           MOVE VDUNUM2 TO LSD-MISC-AMT-CREDIT.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM2 TO LSD-TOT-CREDITS.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM2 TO LSD-PAID-TO-OTHER.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM2 TO LSD-AMT-OF-SURPLUS.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM2 TO LSD-AMT-OF-DEFICIENCY.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM0 TO LSD-SEND-FMNO FORM-NUMBER.
           IF LSD-SEND-FMNO NOT = 0
              MOVE LSD-SEND-FMNO TO FD-FMNO
              PERFORM OPEN-FD1-FILE
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID FORM #" TO MESS PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO LSD-WHO-ADJUSTED.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
           PERFORM OPEN-LSD1-FILE.

           MOVE LSD-REC TO LSD-CH-REC.

           IF LSD-OR-REC = LSD-CH-REC
              GO TO END-WRITE-MODULE.

           IF LSD-OR-REC = SPACES
              GO TO WRITE-RECORD.

       REWRITE-RECORD.
           PERFORM READ-LSD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LSD-REC NOT = LSD-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE LSD-CH-REC TO LSD-REC.

           PERFORM REWRITE-LSD1-FILE.

           PERFORM CREATE-CHANGE-RECORD.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LSD1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           IF FORM-NUMBER NOT = 0
              PERFORM CREATE-FORM-REQUEST.

      *******************************
       CREATE-FORM-REQUEST SECTION.
      *******************************
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-CREATE-FORM-REQUEST.

           IF ( FD-OPTION-EXTRACTION )
              PERFORM GET-FM-PATH.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

           MOVE EXT-WI-CL-ACCTNO   TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE LSD-SEND-FMNO      TO LPWSFM-EDIT-FM-FORMNO.
           IF ( FD-OPTION-EXTRACTION )
              MOVE LN-OWNBR        TO LPWSFM-EDIT-FM-BRNO2.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE LSD-SEND-FMNO          TO FM-FMNO.

           MOVE BW-ZIP           TO FM-ZIP.
           MOVE 0                TO FM-STMT-COUNTER.
           MOVE HOLD-SYS-DATE    TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           PERFORM WRITE-FM1-FILE.
           IF ( IO-BAD               ) AND
              ( FD-OPTION-EXTRACTION )
              PERFORM REWRITE-FM1-FILE.

           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.

           PERFORM CLOSE-FM1-FILE.

       EXIT-CREATE-FORM-REQUEST.
           EXIT.

      * IF FORM IS AN "E" EXTRACTION TYPE FORM (FD-OPTION), THEN
      * PUT THE CORPORATE BRNO PATH IN THE FM-PATH, SO ALL FORM REQUESTS
      * ARE CREATED IN THE HOME OFFICE, THEN WHEN THEY GO "PRINT" THEM ALL
      * IT WILL CREATE AN ASCII FILE

      **************************
       GET-FM-PATH SECTION.
      **************************

           IF GP-CORP-BRNO = 0
              GO TO EXIT-GET-FM-PATH.

           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO EXIT-GET-FM-PATH.

      * USE CORPORATE BRANCH DATA PATH
           MOVE "Y"              TO FM-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE TO FM-ALL-BASE.
           MOVE BR-MACHINE       TO FM-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO FM-ALL-DATA.
           MOVE FM-ALL-PATH      TO FM-PATH.

       EXIT-GET-FM-PATH.
           EXIT.

      ************************
       FORM-SCAN SECTION.
      ************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-FORM-SCAN.

           MOVE "S  N030200." TO VDU.
           MOVE FDSCAN-KEY TO LSD-SEND-FMNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "00" TO VDUSTATUS.

       EXIT-FORM-SCAN.
           EXIT.

      ****************************
       DELETE-MODULE SECTION.
      ****************************
       DELETE-RECORD.
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LSD1-FILE.
           PERFORM READ-LSD1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LSD-REC NOT = LSD-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LSD1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LSD1-FILE.

      ****************************
       NEW-RECORD-SETUP SECTION.
      ****************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LSD-FILE.
           MOVE HOLD-LSD1-KEY TO LSD1-KEY.

       NEW-RECORD-EXIT.
           EXIT.

      ***************************
       DISPLAY-FIELDS SECTION.
      ***************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE LSD-AMT-FROM-OWING     TO D-01.
           MOVE LSD-INS-COMPANY        TO D-02.
           MOVE LSD-STOLEN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-03.
           MOVE LSD-INSURANCE-PROCEEDS TO D-04.
           MOVE LSD-GAP-INS-PAID       TO D-05.
           MOVE LSD-DATE-CHECK-RECV TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO D-06.

           MOVE LSD-OTHER-EXPENSE TO D-07.
           MOVE LSD-MISC-DESC-EXPENSE TO D-08.
           MOVE LSD-MISC-AMT-EXPENSE TO D-081.
           MOVE LSD-TOT-EXPENSE TO D-09.

           MOVE LSD-INT-REB TO D-10.
           MOVE LSD-CPP-REB TO D-11.
           MOVE LSD-GAP-REB TO D-12.
           MOVE LSD-WARRANTY-REB TO D-13.
           MOVE LSD-INSURANCE-CHECK TO D-14.
           MOVE LSD-MISC-DESC-CREDIT TO D-15.
           MOVE LSD-MISC-AMT-CREDIT TO D-151.
           MOVE LSD-TOT-CREDITS TO D-16.

           MOVE LSD-PAID-TO-OTHER TO D-17.

           MOVE LSD-AMT-OF-SURPLUS       TO D-18.
           MOVE LSD-AMT-OF-DEFICIENCY    TO D-19.
           MOVE LSD-SEND-FMNO            TO D-20.
           MOVE LSD-DEF-LETTER-SENT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-201.
           MOVE LSD-WHO-ADJUSTED         TO D-21.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ***************************************************
       CREATE-CHANGE-RECORD SECTION.
      ***************************************************

           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-LM1-FILE.

           IF  (LSD-OR-AMT-FROM-OWING NOT = LSD-AMT-FROM-OWING)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC AMT FROM OWING" TO CH-DESCRIPT
               MOVE LSD-OR-AMT-FROM-OWING TO CH-ORG-2DEC
               MOVE LSD-AMT-FROM-OWING TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-INS-COMPANY NOT = LSD-INS-COMPANY)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC INS COMPANY" TO CH-DESCRIPT
               MOVE LSD-OR-INS-COMPANY TO CH-ORG-CHAR
               MOVE LSD-INS-COMPANY TO CH-NEW-CHAR
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-STOLEN-DATE NOT = LSD-STOLEN-DATE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC STOLEN DATE" TO CH-DESCRIPT
               MOVE LSD-OR-STOLEN-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY     TO CH-ORG-DATE
               MOVE LSD-STOLEN-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY     TO CH-NEW-DATE
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-INSURANCE-PROCEEDS NOT = LSD-INSURANCE-PROCEEDS)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC INS PROCEEDS" TO CH-DESCRIPT
               MOVE LSD-OR-INSURANCE-PROCEEDS TO CH-ORG-2DEC
               MOVE LSD-INSURANCE-PROCEEDS TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-GAP-INS-PAID NOT = LSD-GAP-INS-PAID)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC GAP PAID" TO CH-DESCRIPT
               MOVE LSD-OR-GAP-INS-PAID TO CH-ORG-2DEC
               MOVE LSD-GAP-INS-PAID TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-DATE-CHECK-RECV NOT = LSD-DATE-CHECK-RECV)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC DATE CHECK RECV" TO CH-DESCRIPT
               MOVE LSD-OR-DATE-CHECK-RECV TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY            TO CH-ORG-DATE
               MOVE LSD-DATE-CHECK-RECV TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY            TO CH-NEW-DATE
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-OTHER-EXPENSE NOT = LSD-OTHER-EXPENSE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC OTHER EXPENSE" TO CH-DESCRIPT
               MOVE LSD-OR-OTHER-EXPENSE TO CH-ORG-2DEC
               MOVE LSD-OTHER-EXPENSE TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-MISC-AMT-EXPENSE NOT = LSD-MISC-AMT-EXPENSE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC MISC AMT EXP" TO CH-DESCRIPT
               MOVE LSD-OR-MISC-AMT-EXPENSE TO CH-ORG-2DEC
               MOVE LSD-MISC-AMT-EXPENSE TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-MISC-DESC-EXPENSE NOT = LSD-MISC-DESC-EXPENSE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC MISC DESC EXP" TO CH-DESCRIPT
               MOVE LSD-OR-MISC-DESC-EXPENSE TO CH-ORG-CHAR
               MOVE LSD-MISC-DESC-EXPENSE TO CH-NEW-CHAR
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-TOT-EXPENSE NOT = LSD-TOT-EXPENSE)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC TOTAL EXPENSE" TO CH-DESCRIPT
               MOVE LSD-OR-TOT-EXPENSE TO CH-ORG-2DEC
               MOVE LSD-TOT-EXPENSE TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-INT-REB NOT = LSD-INT-REB)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC INT REBATE" TO CH-DESCRIPT
               MOVE LSD-OR-INT-REB TO CH-ORG-2DEC
               MOVE LSD-INT-REB TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-CPP-REB NOT = LSD-CPP-REB)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC CPP REBATE" TO CH-DESCRIPT
               MOVE LSD-OR-CPP-REB TO CH-ORG-2DEC
               MOVE LSD-CPP-REB TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-GAP-REB NOT = LSD-GAP-REB)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC GAP REBATE" TO CH-DESCRIPT
               MOVE LSD-OR-GAP-REB TO CH-ORG-2DEC
               MOVE LSD-GAP-REB TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-WARRANTY-REB NOT = LSD-WARRANTY-REB)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC WARRANTY REB" TO CH-DESCRIPT
               MOVE LSD-OR-WARRANTY-REB TO CH-ORG-2DEC
               MOVE LSD-WARRANTY-REB TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-INSURANCE-CHECK NOT = LSD-INSURANCE-CHECK)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC INSUR CHECK" TO CH-DESCRIPT
               MOVE LSD-OR-INSURANCE-CHECK TO CH-ORG-2DEC
               MOVE LSD-INSURANCE-CHECK TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-MISC-AMT-CREDIT NOT = LSD-MISC-AMT-CREDIT)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC MISC AMT CRED" TO CH-DESCRIPT
               MOVE LSD-OR-MISC-AMT-CREDIT TO CH-ORG-2DEC
               MOVE LSD-MISC-AMT-CREDIT TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-MISC-DESC-CREDIT NOT = LSD-MISC-DESC-CREDIT)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC MISC DESC CR" TO CH-DESCRIPT
               MOVE LSD-OR-MISC-DESC-CREDIT TO CH-ORG-CHAR
               MOVE LSD-MISC-DESC-CREDIT TO CH-NEW-CHAR
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-TOT-CREDITS NOT = LSD-TOT-CREDITS)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC TOTAL CREDITS" TO CH-DESCRIPT
               MOVE LSD-OR-TOT-CREDITS TO CH-ORG-2DEC
               MOVE LSD-TOT-CREDITS TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-PAID-TO-OTHER NOT = LSD-PAID-TO-OTHER)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC PAID TO OTHER" TO CH-DESCRIPT
               MOVE LSD-OR-PAID-TO-OTHER TO CH-ORG-2DEC
               MOVE LSD-PAID-TO-OTHER TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-AMT-OF-SURPLUS NOT = LSD-AMT-OF-SURPLUS)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC AMT OF SURPLUS" TO CH-DESCRIPT
               MOVE LSD-OR-AMT-OF-SURPLUS TO CH-ORG-2DEC
               MOVE LSD-AMT-OF-SURPLUS TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-AMT-OF-DEFICIENCY NOT = LSD-AMT-OF-DEFICIENCY)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "AMT OF DEFICIENCY" TO CH-DESCRIPT
               MOVE LSD-OR-AMT-OF-DEFICIENCY TO CH-ORG-2DEC
               MOVE LSD-AMT-OF-DEFICIENCY TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-SEND-FMNO NOT = LSD-SEND-FMNO)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC SEND FMNO" TO CH-DESCRIPT
               MOVE LSD-OR-SEND-FMNO TO CH-ORG-2DEC
               MOVE LSD-SEND-FMNO TO CH-NEW-2DEC
               PERFORM UPDATE-CH-LM-FILE.

           IF  (LSD-OR-WHO-ADJUSTED NOT = LSD-WHO-ADJUSTED)
               PERFORM CREATE-SETUP-CH-REC
               MOVE "DEFC WHO ADJUSTED" TO CH-DESCRIPT
               MOVE LSD-OR-WHO-ADJUSTED TO CH-ORG-CHAR
               MOVE LSD-WHO-ADJUSTED TO CH-NEW-CHAR
               PERFORM UPDATE-CH-LM-FILE.

           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-LM1-FILE.


      ************************************
       UPDATE-CH-LM-FILE SECTION.
      ************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - HOLD-SYS-DATE.
           MOVE 1             TO SAVE-LM-SEQ.
       UPDATE-CH-LM-FILE-AGAIN.
           MOVE SPACES TO LM-REC.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE CH-USERID    TO MEMO-USERID.
           MOVE CH-DESCRIPT  TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR  TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA    TO LM-MEMO.
           MOVE "9"          TO LM-MEMO-TYPE.

           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF SAVE-LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO UPDATE-CH-LM-FILE-AGAIN.

       UPDATE-CH-LM-FILE-EXIT.
           EXIT.

       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPLSDCL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CHDEFC" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHDEFC-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHDEFC-SCREEN UPON CHDEFC-WINDOW-HANDLE.
           MOVE CHDEFC-FORM-FIELDS TO WR-BUF.
           MOVE CHDEFC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CHDEFC-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHDEFC-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHDEFC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC-PARAGRAPHS SECTION.
      ****************************
       COPY "LIBGB/ACCESS.CPY".
       CREATE-SETUP-CH-REC.

           PERFORM CLEAR-CH-FILE.

           MOVE BRANCH             TO CH-BRNO.
           MOVE HOLD-SYS-DATE      TO CH-DATE.
           MOVE "Z"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE 1                  TO CH-SEQ.

           MOVE LN-ACCTNO          TO CH-ACCTNO.
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LSD1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLSDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLSD1IN.CPY".
       COPY "LIBLP/LPLSX1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHDEFC-SCREEN.
           DESTROY CHDEFC-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
