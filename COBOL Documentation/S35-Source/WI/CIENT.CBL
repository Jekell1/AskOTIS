       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CIENT.
       DATE-WRITTEN. 02/07/2011.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   ENTER CUSTOMER INFORMATION WINDOW
      *                 CALLED XONPC0, ENTER INFO, PASS BACK CI-REC
      *
      *         WRITTEN FOR WORLD
      *
      *  REV:
      *  BLM 110207 WORLD PR# 739
      *  BLM 110224 CHANGES TO CI FIELDS, DELETED CI-LESS-RENT,
      *             CI-FREE-INCOME.  REMOVED CENTS FROM ALL AMTS.
      *             DON'T ENTER FIELDS 15-18 IF SSNO2 = 0.  FIX
      *             CALCULATIONS.  WORLD PR# 739
      *  BLM 110713 FIX UP ARROW FROM AMT LOCKING UP
      *  BLM 111014 FIXED CONVERSION TO 8 DIGIT DATE, IF YEAR ENTERED
      *             IS LESS THAN OR EQUAL TO YEAR OF LN-ENTDATE,
      *             CONVERT TO 2000, OTHERWISE CONVERT TO 1900.
      *             WORLD PR# 739
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2016.1003 FIXED CI-DATE FIELDS TO BE IN CCYYMMDD FORMAT.
      * BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 190128 SPLIT CDFILE
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)WI/CIENT.CBL S35 02/23/24-11:36:37 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
      *-----------------------------------------------------------------
       01  ERRCD           PIC X       VALUE SPACES.
       01  LEGEND          PIC X(70).
       01  LEGEND-01.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F7-UPDATE, ENTER FIELD #:".

      *-----------------------------------------------------------------
       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

       01  NEW-FLAG          PIC 9.

       01  WORK-AMT        PIC S9(7) VALUE 0.

      *-----------------------------------------------------------------

       01  DISPLAY-BUF1      VALUE SPACES.
           03  D-ACCTNO      PIC 9(6) BLANK ZERO.
           03  D-SSNO-1      PIC 999/99/9999 BLANK ZERO.
           03  D-NAME-1      PIC X(32).
           03  D-SSNO-2      PIC 999/99/9999 BLANK ZERO.
           03  D-NAME-2      PIC X(32).
           03  D-DATE        PIC 99/99/99.

       01  DISPLAY-LIST1.
           03  FILLER      PIC X(31) VALUE
           "ACCTNO 010 NAME1 020 NAME2 030.".

       01  DISPLAY-BUF      VALUE SPACES.
      * SSNO
           03  D-01             PIC 999/99/9999.
           03  D-011            PIC X(32).
      * SSNO 2
           03  D-02             PIC 999/99/9999 BLANK ZERO.
           03  D-021            PIC X(32).
      * DATE OF INFORMATION
           03  D-03             PIC 99/99/99.
      * AMOUNT REQUESTED
           03  D-04             PIC Z(7)-.
      * MARITAL STATUS
           03  D-05             PIC X.
      * YEARS IN AREA
           03  D-06             PIC Z9.
      * DATE AT PRESENT ADR
           03  D-07             PIC 99/99/99 BLANK ZERO.
      * MONTHLY RENT/MTG
           03  D-08             PIC Z(7)-.
      * FAIR MARKET VALUE
           03  D-09             PIC Z(7)-.
      * MORTGAGE BALANCE
           03  D-10             PIC Z(7)-.
      * CHECKING Y/N
           03  D-11             PIC X.
      * SAVINGS Y/N
           03  D-12             PIC X.
      * DATE OF HIRE (1)
           03  D-13             PIC 99/99/99 BLANK ZERO.
      * POSITION CODE (1)
           03  D-14             PIC X(5).
      * GROSS MO. INCOME (1)
           03  D-15             PIC Z(7)-.
      * DATE OF HIRE (2)
           03  D-16             PIC 99/99/99 BLANK ZERO.
      * POSITION CODE (2)
           03  D-17             PIC X(5).
      * GROSS MO. INCOME (2)
           03  D-18             PIC Z(7)-.
      * TOT MONTHLY INCOME
           03  D-19             PIC Z(7)-.
      * LESS MONTHLY PAY
           03  D-20             PIC Z(7)-.
      * LESS RENT
           03  D-21             PIC Z(7)-.
      * FREE INCOME
           03  D-22             PIC Z(7)-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "010 NAME1 020 NAME2 030 040 050 060 070 080 090 ".
           03  FILLER      PIC X(52) VALUE
           "100 110 120 130 140 150 160 170 180 190 200 210 220.".

       78  MAX            VALUE 20.
      ******************************************************************
      *      ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      * 1 SSNO
           03  FILLER      PIC X(8) VALUE "ERNX090N".
      * 2 SSNO #2
           03  FILLER      PIC X(8) VALUE "ERNX090N".
      * 3 DATE OF INFORMATION
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 4 AMT REQUESTED
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 5 MARITAL STATUS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6 YEARS IN AREA
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 7 DATE AT PRESENT ADR
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 8 MONTHLY RENT/MTG
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 9 FAIR MARKET VALUE
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 10 MORTGAGE BALANCE
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 11  CHECKING Y/N
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 12 SAVINGS Y/N
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 13 DATE OF HIRE (1)
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 14  POSITION CODE (1)
           03  FILLER      PIC X(8) VALUE "E  A050 ".
      * 15 GROSS MONTHLY INCOME (1)
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 16 DATE OF HIRE (2)
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 17  POSITION CODE (2)
           03  FILLER      PIC X(8) VALUE "E  A050 ".
      * 18 GROSS MONTHLY INCOME (2)
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 19 TOTAL MONTHLY INCOME
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 20 LESS MONTHLY PAY
           03  FILLER      PIC X(8) VALUE "ENNN070 ".

       01  SPEC-TAB      REDEFINES SPEC-TABLE.
           03  SPEC-REC  OCCURS MAX TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       01 CIENT-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CIENT_DEF.CPY".
       COPY "LIBWI/CIENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  CIENT-WORKERS.
           03  CIENT-TRANS-DATE     PIC 9(8)    COMP-6.
           03  CIENT-SSNO-1         PIC 9(9)    COMP-3.
           03  CIENT-SSNO-2         PIC 9(9)    COMP-3.
           03  CIENT-ENTRY-REQUIRED PIC X.
           03  CIENT-ENTRY-COMPLETE PIC X.
       COPY "LIBLP/LP01CI.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CIENT_SCN.CPY".

      ************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME CIENT-WORKERS CI-REC EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CIENT" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CIENT-WINDOW-HANDLE.

           MOVE "S  A200TITLE:FR." TO VDU.
           MOVE "CUSTOMER INFORMATION" TO VDUBUF.
           PERFORM SCREENIO.

      D    STOP MESS.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-CI-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              MOVE "N" TO CIENT-ENTRY-COMPLETE
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              MOVE "Y" TO CIENT-ENTRY-COMPLETE
              GO TO END-ROUTINE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      **************************
       REC-CI-MODULE SECTION.
      **************************
           MOVE SPACES TO IO-TYPE.

           MOVE CIENT-SSNO-2     TO CI-SSNO-2.
           IF CI-SSNO-2 = 0
              MOVE "N" TO CHFG(16)
                          CHFG(17)
                          CHFG(18).

           IF CIENT-ENTRY-COMPLETE = "Y"
              MOVE 0 TO NEW-FLAG
              PERFORM DISPLAY-FIELDS
              GO TO END-REC-CI-MODULE.

           MOVE 0 TO D-ACCTNO.

           MOVE CIENT-SSNO-1 TO D-SSNO-1.
           INSPECT D-SSNO-1 REPLACING ALL "/" BY "-".

           PERFORM OPEN-BW1-FILE.
           MOVE CIENT-SSNO-1 TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "INVALID" TO BW-LNAME
              MOVE "BORROWER" TO BW-FNAME.
           MOVE SPACES TO D-NAME-1.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO D-NAME-1.

           IF CIENT-SSNO-2 NOT = 0
              MOVE CIENT-SSNO-2 TO D-SSNO-2
              INSPECT D-SSNO-2 REPLACING ALL "/" BY "-"
              MOVE CIENT-SSNO-2 TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-BAD
                 MOVE "INVALID" TO BW-LNAME
                 MOVE "BORROWER" TO BW-FNAME
              END-IF
              MOVE SPACES TO D-NAME-2
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                            INTO D-NAME-2.

           PERFORM CLOSE-BW1-FILE.

           MOVE CIENT-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-DATE.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM NEW-RECORD-SETUP.

       END-REC-CI-MODULE.
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
           MOVE 3 TO ELE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.
           IF ELE < 4
              GO TO CREATE-ROUTINE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           IF NEW-FLAG = 0
              MOVE "F7-UPDATE, ENTER FIELD #:" TO LEGEND
           ELSE
              MOVE "F1-RESET, F7-UPDATE, ENTER FIELD #:" TO LEGEND.
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

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           IF CHNO < 4
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
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE " F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.

           IF ELE = 4
              MOVE "ENTER AMOUNT REQUESTED:" TO LEGEND.
           IF ELE = 5
              MOVE "ENTER MARITAL STATUS:" TO LEGEND.
           IF ELE = 6
              MOVE "ENTER NUMBER OF YEAR IN AREA:" TO LEGEND.
           IF ELE = 7
              MOVE "ENTER DATE AT CURRENT ADDRESS (MMDDYY):" TO LEGEND.
           IF ELE = 8
              MOVE "ENTER AMOUNT OF MONTHLY RENT OR MORTGAGE PAYMENT:"
                                                             TO LEGEND.
           IF ELE = 9
              MOVE "ENTER FAIR MARKET VALUE OF HOME:" TO LEGEND.
           IF ELE = 10
              MOVE "ENTER MORTGAGE BALANCE:" TO LEGEND.
           IF ELE = 11
              MOVE "ENTER 'Y' OR 'N' FOR CHECKING ACCOUNT:" TO LEGEND.
           IF ELE = 12
              MOVE "ENTER 'Y' OR 'N' FOR SAVINGS ACCOUNT:" TO LEGEND.
           IF ELE = 13
              MOVE "ENTER DATE OF HIRE FOR SSNO #1 (MMDDYY):" TO LEGEND.
           IF ELE = 14
              MOVE "ENTER POSTION CODE FOR SSNO#1, F6-SCAN CODES:"
                                                             TO LEGEND.
           IF ELE = 15
              MOVE "ENTER GROSS MONTHLY INCOME FOR SSNO #1:" TO LEGEND.
           IF ELE = 16
              MOVE "ENTER DATE OF HIRE FOR SSNO #2 (MMDDYY):" TO LEGEND.
           IF ELE = 17
              MOVE "ENTER POSTION CODE FOR SSNO#2, F6-SCAN CODES:"
                                                              TO LEGEND.
           IF ELE = 18
              MOVE "ENTER GROSS MONTHLY INCOME FOR SSNO #2:" TO LEGEND.
           IF ELE = 19
              MOVE "ENTER TOTAL MONTHLY INCOME:" TO LEGEND.
           IF ELE = 20
              MOVE "ENTER MONTHLY PAYMENT AMOUNT:" TO LEGEND.

           PERFORM SEND-LEGEND.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 IF ELE > 4
                    MOVE -1 TO ONE
                    GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F7.
           MOVE 1 TO ONE.
           IF ELE = 14 OR 17
              PERFORM CODE-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
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
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *          SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           GO TO CREATE-ROUTINE.
       F-02.
           GO TO CREATE-ROUTINE.
       F-03.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM0 TO CI-AMT-REQUESTED.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO CI-MARITAL-STAT.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM0 TO CI-YEARS-IN-AREA.
           GO TO CREATE-ROUTINE.
       F-07.
      * LEAVE THESE DATE TESTS ALONE IN A30, TESTING DIFFERENT YEAR RANGE
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-TO-8-DIGIT-DATE.
           MOVE DATE-YYYYMMDD TO CI-DATE-AT-PRESENT-ADR.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM0 TO CI-MONTHLY-RENT-MTG.
           MOVE "SNNN070210." TO VDU.
           MOVE CI-MONTHLY-RENT-MTG TO VDUNUM0.
           PERFORM SCREENIO.
           IF NEW-FLAG NOT = 1
              COMPUTE WORK-AMT = CI-TOTAL-MO-INCOME
                               - CI-LESS-MO-PAY
                               - CI-MONTHLY-RENT-MTG
                 MOVE "SNNS070220." TO VDU
                 MOVE WORK-AMT TO VDUNUM0
                 PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM0 TO CI-FAIR-MARKET-VALUE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM0 TO CI-MTG-BALANCE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO CI-CHECKING-ACCOUNT.
           IF CI-CHECKING-ACCOUNT NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUBUF TO CI-SAVINGS-ACCOUNT.
           IF CI-SAVINGS-ACCOUNT NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-13.
      * LEAVE THESE DATE TESTS ALONE IN A30, TESTING DIFFERENT YEAR RANGE
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-TO-8-DIGIT-DATE.
           MOVE DATE-YYYYMMDD TO CI-DATE-HIRE-1.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO CI-POSITION-CODE-1 CD-CODE.
           MOVE "JT"                      TO CD-TYPE.
           PERFORM GET-CD.
           IF IO-FG = 9
              MOVE "INVALID CODE; F6-SCAN CODES" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO CI-GROSS-MO-INCOME-1.
           GO TO CREATE-ROUTINE.
       F-16.
      * LEAVE THESE DATE TESTS ALONE IN A30, TESTING DIFFERENT YEAR RANGE
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-TO-8-DIGIT-DATE.
           MOVE DATE-YYYYMMDD TO CI-DATE-HIRE-2.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO CI-POSITION-CODE-2 CD-CODE.
           IF CI-SSNO-2 NOT = 0
              MOVE "JT" TO CD-TYPE
              PERFORM GET-CD
              IF IO-FG = 9
                 MOVE "INVALID CODE; F6-SCAN CODES" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM0 TO CI-GROSS-MO-INCOME-2.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM0 TO CI-TOTAL-MO-INCOME.
           IF NEW-FLAG NOT = 1
              COMPUTE WORK-AMT = CI-TOTAL-MO-INCOME
                               - CI-LESS-MO-PAY
                               - CI-MONTHLY-RENT-MTG
                 MOVE "SNNS070220." TO VDU
                 MOVE WORK-AMT TO VDUNUM0
                 PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM0 TO CI-LESS-MO-PAY.
           COMPUTE WORK-AMT = CI-TOTAL-MO-INCOME
                            - CI-LESS-MO-PAY
                            - CI-MONTHLY-RENT-MTG.
              MOVE "SNNS070220." TO VDU.
              MOVE WORK-AMT TO VDUNUM0.
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************
       NEW-RECORD-SETUP SECTION.
      ****************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CI-FILE.
           MOVE 0                TO CI-ACCTNO.
           MOVE CIENT-SSNO-1     TO CI-SSNO.
           MOVE CIENT-SSNO-2     TO CI-SSNO-2.
           MOVE CIENT-TRANS-DATE TO CI-DATE-UPDATED.

       NEW-RECORD-EXIT.
           EXIT.

      **********************************
       CONVERT-TO-8-DIGIT-DATE SECTION.
      **********************************
           MOVE DATE-MMDDYY-MM TO DATE-YYYYMMDD-MM.
           MOVE DATE-MMDDYY-DD TO DATE-YYYYMMDD-DD.
           MOVE DATE-MMDDYY-YY TO DATE-YYYYMMDD-YYYY.
      *------------------------------------------------------
      * IF YEAR ENTERED < OR = TRANS YEAR, IT'S 2000
      * SO, ENTRY OF 7/21/11, 12/15/08 CONVERTS TO 12/15/2008
      *                       12/15/46 CONVERTS TO 12/15/1946
      *                       12/15/12 CONVERTS TO 12/15/1912
      *------------------------------------------------------
           MOVE CIENT-TRANS-DATE TO NUM-DATE.

           IF ( DATE-YYYYMMDD NOT = ZEROES )
              IF ( DATE-YYYYMMDD-YY <= NUM-YR )
                 ADD 2000 TO DATE-YYYYMMDD-YYYY
              ELSE
                 ADD 1900 TO DATE-YYYYMMDD-YYYY.

       EXIT-CONVERT-DATE.
           EXIT.

      ***************************
       DISPLAY-FIELDS SECTION.
      ***************************
           MOVE SPACES                 TO DISPLAY-BUF.

           PERFORM OPEN-BW1-FILE.
           MOVE CI-SSNO                TO D-01 BW-SSNO.
           INSPECT D-01 REPLACING ALL "/" BY "-".
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "INVALID" TO BW-LNAME
              MOVE "BORROWER" TO BW-FNAME.
           MOVE SPACES TO D-011.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO  D-011.

           IF CI-SSNO-2 NOT = 0
              MOVE CI-SSNO-2              TO D-02 BW-SSNO
              INSPECT D-02 REPLACING ALL "/" BY "-"
              PERFORM READ-BW1-FILE
              IF IO-BAD
                 MOVE "INVALID" TO BW-LNAME
                 MOVE "BORROWER" TO BW-FNAME
              END-IF
              MOVE SPACES TO D-021
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                  INTO D-021.
           PERFORM CLOSE-BW1-FILE.

           MOVE CI-DATE-UPDATED        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-03.

           MOVE CI-AMT-REQUESTED       TO D-04.
           MOVE CI-MARITAL-STAT        TO D-05.
           MOVE CI-YEARS-IN-AREA       TO D-06.

           MOVE CI-DATE-AT-PRESENT-ADR TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-07.

           MOVE CI-MONTHLY-RENT-MTG    TO D-08.
           MOVE CI-FAIR-MARKET-VALUE   TO D-09.
           MOVE CI-MTG-BALANCE         TO D-10.
           MOVE CI-CHECKING-ACCOUNT    TO D-11.
           MOVE CI-SAVINGS-ACCOUNT     TO D-12.

           MOVE CI-DATE-HIRE-1         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-13.

           MOVE CI-POSITION-CODE-1     TO D-14.
           MOVE CI-GROSS-MO-INCOME-1   TO D-15.

           MOVE CI-DATE-HIRE-2         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-16.

           MOVE CI-POSITION-CODE-2     TO D-17.
           MOVE CI-GROSS-MO-INCOME-2   TO D-18.

           MOVE CI-TOTAL-MO-INCOME     TO D-19.
           MOVE CI-LESS-MO-PAY         TO D-20.

           MOVE CI-MONTHLY-RENT-MTG    TO D-21.
           COMPUTE WORK-AMT = CI-TOTAL-MO-INCOME
                            - CI-LESS-MO-PAY
                            - CI-MONTHLY-RENT-MTG.
           MOVE WORK-AMT               TO D-22.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      **************************
       CODE-SCAN SECTION.
      **************************
           MOVE "JT"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A050  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDSCAN-CODE TO VDUBUF
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPCICL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           DISPLAY SPACES UPON CIENT-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CIENT-SCREEN UPON CIENT-WINDOW-HANDLE.
           MOVE CIENT-FORM-FIELDS TO WR-BUF.
           MOVE CIENT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE CIENT-HELP--FILE TO HELP-LINK-FILE.
           MOVE CIENT-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CIENT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC-PARAGRAPHS SECTION.
      ****************************
       COPY "LIBGB/ACCESS.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CIENT-SCREEN.
           DESTROY CIENT-WINDOW-HANDLE.
           EXIT PROGRAM.
