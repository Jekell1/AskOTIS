       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CIMAIN.
       DATE-WRITTEN. 02/17/2011.
      *****************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   CUSTOMER INFORMATION FILE MAINTENANCE
      *
      *         WRITTEN FOR WORLD
      *
      * REV:
      *  BLM 110217 WORLD PR# 739
      *  BLM 111014 FIXED CONVERSION TO 8 DIGIT DATE, IF YEAR ENTERED
      *             IS LESS THAN OR EQUAL TO YEAR OF LN-ENTDATE,
      *             CONVERT TO 2000, OTHERWISE CONVERT TO 1900.
      *             WORLD PR# 739
      *  MJD 131210 A30 CONVERSION COMPLETED
      *  KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *                LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *                CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *                CDSCAN-??-KEY (NEW).
      *  KEC 2016.1003 FIXED CI-DATE FIELDS TO BE IN CCYYMMDD FORMAT.
      *  CS  2017.0418 CHANGED DISPLAY OF DATES TO MMDDYY
      *  BAH 2017.0619 ERRLOG
      *  BAH 2018.1009 GLOBAL CIFILE
      *  BAH 2019.0129 SPLIT CDFILE
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
           "@(#)MN/CIMAIN.CBL S35 02/21/24-12:32:20 >".
       COPY "LIBLP/LP01CI.CPY".
       COPY "LIBLP/LP01CI_SQL.CPY".
       COPY "LIBLP/LPWSCI.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      *=================================================================
      *   CICH-REC  [CHANGED-CI-REC]
      *   CIOR-REC  [ORIGINAL-CI-REC]
      *=================================================================
       COPY "LIBLP/LP01CI.CPY" REPLACING LEADING "CI" BY "CIOR".
       COPY "LIBLP/LP01CI.CPY" REPLACING LEADING "CI" BY "CICH".

      ******************************************************************
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(02).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(12).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(15).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(15).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.
       01  HOLD-CI1-KEY                PIC 9(12).               
       01  FILLER                      REDEFINES HOLD-CI1-KEY.
           03  HOLD-CI-BRNO            PIC 9(04).
           03  HOLD-CI-ACCTNO          PIC 9(08).

       01  ERRCD           PIC X       VALUE SPACES.
       01  LEGEND          PIC X(70).

       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

       01  NEW-FLAG          PIC 9.
       01  FIRST-SCAN        PIC 9     VALUE 0.
       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

       01  WORK-AMT           PIC S9(7) VALUE 0.

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
           03  D-07             PIC 99/99/99.
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
           03  D-13             PIC 99/99/99.
      * POSITION CODE (1)
           03  D-14             PIC X(5).
      * GROSS MO. INCOME (1)
           03  D-15             PIC Z(7)-.
      * DATE OF HIRE (2)
           03  D-16             PIC 99/99/99.
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
           03  FILLER      PIC X(8) VALUE "ERNX110N".
      * 2 SSNO #2
           03  FILLER      PIC X(8) VALUE "ERNX110N".
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
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/BWSCANW.CPY".

       01 CIMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CIMAIN_DEF.CPY".
       COPY "LIBMN/CIMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CIMAIN_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "CIMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CIMAIN-WINDOW-HANDLE.


           MOVE FORM-PATHNAME TO FORM-PATH.

      D    STOP MESS.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-CI-MODULE.
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

      **************************
       REC-CI-MODULE SECTION.
      **************************
       REC-CI-MODULE-NO.
           MOVE 0 TO CI-ACCTNO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.
       REC-CI-NUMBER.
           MOVE "EN N060ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-CI-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CI-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-CI-NUMBER.

           MOVE CI-PATH-OWNBR     TO HOLD-CI-BRNO.
           MOVE VDUNUM0 TO CI-ACCTNO HOLD-CI-ACCTNO.

       READ-CIFILE.
           MOVE CI-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* LOAN NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-CI-MODULE-NO.

           PERFORM OPEN-CI1-FILE.
           PERFORM READ-CI1-FILE.
           PERFORM CLOSE-CI1-FILE.
           IF IO-FG = 0
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
           MOVE CI-REC TO CIOR-REC.

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
           MOVE "F1-RESET, 'DELETE', RTN-UPDATE, ENTER FLD #:"
             TO LEGEND.
           PERFORM SEND-LEGEND.
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
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
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
           IF ELE = 21
              MOVE "ENTER AMOUNT OF MONTHLY RENT OR MORTGAGE PAYMENT:"
                                                             TO LEGEND.
           IF ELE = 22
              MOVE "ENTER AMOUNT REQUESTED:" TO LEGEND.

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
           IF ELE < 3
              PERFORM BORROWER-SCAN
              IF IO-BAD
                 GO TO ENTER-ELE
              ELSE
                 MOVE "00" TO VDUSTATUS.

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
           MOVE VDUNUM0 TO CI-SSNO BW-SSNO.
           PERFORM GET-BW.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE "S  A330NAME1." TO VDU.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO VDUBUF.
           PERFORM SCREENIO.
           IF BW-SSNO NOT = LN-SSNO(1)
              MOVE "WARNING: NOT BORROWER #1 ON LOAN!" TO MESS
              PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO CI-SSNO-2 BW-SSNO.
           PERFORM GET-BW.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE "S  A330NAME2." TO VDU.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO VDUBUF.
           PERFORM SCREENIO.
           IF BW-SSNO NOT = LN-SSNO(2)
              MOVE "WARNING: NOT BORROWER #2 ON LOAN!" TO MESS
              PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUNUM0 NOT = LN-ENTDATE
              MOVE "WARNING: DATE NOTE = LOAN ENTRY DATE!" TO MESS
              PERFORM SEND-MESS.
           MOVE VDU-DATE-YYYYMMDD TO CI-DATE-UPDATED.
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
           MOVE VDU-DATE-YYYYMMDD TO CI-DATE-AT-PRESENT-ADR.
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
           MOVE VDU-DATE-YYYYMMDD TO CI-DATE-HIRE-1.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO CI-POSITION-CODE-1 CD-CODE.
           MOVE "JT"   TO CD-TYPE.
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
           MOVE VDU-DATE-YYYYMMDD TO CI-DATE-HIRE-2.
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
           IF NEW-FLAG = 1
              MOVE "SNNN070190." TO VDU
              COMPUTE WORK-AMT = CI-GROSS-MO-INCOME-1
                               + CI-GROSS-MO-INCOME-2.
              MOVE WORK-AMT TO VDUNUM0
              PERFORM SCREENIO.
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

      **********************************
       WRITE-MODULE SECTION.
      ********************************
           PERFORM OPEN-CI1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CI-REC TO CICH-REC.

           IF CIOR-REC = CICH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-CI1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF CI-REC NOT = CIOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CICH-REC TO CI-REC.

           PERFORM REWRITE-CI1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-CI1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-CI1-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE CI-BRNO                    TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF CIOR-SSNO NOT = CICH-SSNO
             MOVE "01 "                  TO LMC-FIELD
             MOVE "SSNO 1"               TO LMC-DESC
             MOVE CIOR-SSNO              TO LMC-ORG-NUM
             MOVE CICH-SSNO              TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-SSNO-2 NOT = CICH-SSNO-2
             MOVE "02 "                  TO LMC-FIELD
             MOVE "SSNO 2"               TO LMC-DESC
             MOVE CIOR-SSNO-2              TO LMC-ORG-NUM
             MOVE CICH-SSNO-2              TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-DATE-UPDATED NOT = CICH-DATE-UPDATED
             MOVE "03 "                  TO LMC-FIELD
             MOVE "INFODATE"             TO LMC-DESC
             MOVE CIOR-DATE-UPDATED       TO LMC-ORG-NUM
             MOVE CICH-DATE-UPDATED       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-AMT-REQUESTED NOT = CICH-AMT-REQUESTED
             MOVE "04 "                    TO LMC-FIELD
             MOVE "AMT REQ"                TO LMC-DESC
             MOVE CIOR-AMT-REQUESTED       TO LMC-ORG-AMT
             MOVE CICH-AMT-REQUESTED       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-MARITAL-STAT NOT = CICH-MARITAL-STAT
             MOVE "05 "                    TO LMC-FIELD
             MOVE "MARITAL"                TO LMC-DESC
             MOVE CIOR-MARITAL-STAT       TO LMC-ORG-TEXT
             MOVE CICH-MARITAL-STAT       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-YEARS-IN-AREA NOT = CICH-YEARS-IN-AREA
             MOVE "06 "                    TO LMC-FIELD
             MOVE "YEARS"                  TO LMC-DESC
             MOVE CIOR-YEARS-IN-AREA       TO LMC-ORG-NUM
             MOVE CICH-YEARS-IN-AREA       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-DATE-AT-PRESENT-ADR NOT = CICH-DATE-AT-PRESENT-ADR
             MOVE "07 "                    TO LMC-FIELD
             MOVE "DATE AT"                  TO LMC-DESC
             MOVE CIOR-DATE-AT-PRESENT-ADR       TO LMC-ORG-NUM
             MOVE CICH-DATE-AT-PRESENT-ADR       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-MONTHLY-RENT-MTG NOT = CICH-MONTHLY-RENT-MTG
             MOVE "08 "                    TO LMC-FIELD
             MOVE "RENT"                   TO LMC-DESC
             MOVE CIOR-MONTHLY-RENT-MTG       TO LMC-ORG-AMT
             MOVE CICH-MONTHLY-RENT-MTG       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-FAIR-MARKET-VALUE NOT = CICH-FAIR-MARKET-VALUE
             MOVE "09 "                    TO LMC-FIELD
             MOVE "FAIR"                   TO LMC-DESC
             MOVE CIOR-FAIR-MARKET-VALUE       TO LMC-ORG-AMT
             MOVE CICH-FAIR-MARKET-VALUE       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-MTG-BALANCE NOT = CICH-MTG-BALANCE
             MOVE "10 "                  TO LMC-FIELD
             MOVE "MTG BAL"              TO LMC-DESC
             MOVE CIOR-MTG-BALANCE       TO LMC-ORG-AMT
             MOVE CICH-MTG-BALANCE       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-CHECKING-ACCOUNT NOT = CICH-CHECKING-ACCOUNT
             MOVE "11 "                       TO LMC-FIELD
             MOVE "CHECKING"                  TO LMC-DESC
             MOVE CIOR-CHECKING-ACCOUNT       TO LMC-ORG-TEXT
             MOVE CICH-CHECKING-ACCOUNT       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-SAVINGS-ACCOUNT NOT = CICH-SAVINGS-ACCOUNT
             MOVE "12 "                       TO LMC-FIELD
             MOVE "SAVINGS"                  TO LMC-DESC
             MOVE CIOR-SAVINGS-ACCOUNT       TO LMC-ORG-TEXT
             MOVE CICH-SAVINGS-ACCOUNT       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-DATE-HIRE-1 NOT = CICH-DATE-HIRE-1
             MOVE "13 "                  TO LMC-FIELD
             MOVE "HIRE 1"               TO LMC-DESC
             MOVE CIOR-DATE-HIRE-1       TO LMC-ORG-NUM
             MOVE CICH-DATE-HIRE-1       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-POSITION-CODE-1 NOT = CICH-POSITION-CODE-1
             MOVE "14 "                      TO LMC-FIELD
             MOVE "POS CODE 1"               TO LMC-DESC
             MOVE CIOR-POSITION-CODE-1       TO LMC-ORG-TEXT
             MOVE CICH-POSITION-CODE-1       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-GROSS-MO-INCOME-1 NOT = CICH-GROSS-MO-INCOME-1
             MOVE "15 "                      TO LMC-FIELD
             MOVE "NETINCOME 1"              TO LMC-DESC
             MOVE CIOR-GROSS-MO-INCOME-1       TO LMC-ORG-AMT
             MOVE CICH-GROSS-MO-INCOME-1       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-DATE-HIRE-2 NOT = CICH-DATE-HIRE-2
             MOVE "16 "                  TO LMC-FIELD
             MOVE "HIRE 2"               TO LMC-DESC
             MOVE CIOR-DATE-HIRE-2       TO LMC-ORG-NUM
             MOVE CICH-DATE-HIRE-2       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-POSITION-CODE-2 NOT = CICH-POSITION-CODE-2
             MOVE "17 "                      TO LMC-FIELD
             MOVE "POS CODE 2"               TO LMC-DESC
             MOVE CIOR-POSITION-CODE-2       TO LMC-ORG-TEXT
             MOVE CICH-POSITION-CODE-2       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-GROSS-MO-INCOME-2 NOT = CICH-GROSS-MO-INCOME-2
             MOVE "18 "                      TO LMC-FIELD
             MOVE "NETINCOME 2"              TO LMC-DESC
             MOVE CIOR-GROSS-MO-INCOME-2       TO LMC-ORG-AMT
             MOVE CICH-GROSS-MO-INCOME-2       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-TOTAL-MO-INCOME NOT = CICH-TOTAL-MO-INCOME
             MOVE "19 "                      TO LMC-FIELD
             MOVE "TOT INCOME"              TO LMC-DESC
             MOVE CIOR-TOTAL-MO-INCOME       TO LMC-ORG-AMT
             MOVE CICH-TOTAL-MO-INCOME       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-LESS-MO-PAY NOT = CICH-LESS-MO-PAY
             MOVE "20 "                      TO LMC-FIELD
             MOVE "LESS PAY"              TO LMC-DESC
             MOVE CIOR-LESS-MO-PAY       TO LMC-ORG-AMT
             MOVE CICH-LESS-MO-PAY       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CIOR-MONTHLY-RENT-MTG NOT = CICH-MONTHLY-RENT-MTG
             MOVE "20 "                      TO LMC-FIELD
             MOVE "LESS RENT"              TO LMC-DESC
             MOVE CIOR-MONTHLY-RENT-MTG       TO LMC-ORG-AMT
             MOVE CICH-MONTHLY-RENT-MTG       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      *********************************
       DELETE-MODULE SECTION.
      *********************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-CI1-FILE.
           PERFORM READ-CI1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF CI-REC NOT = CIOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CI1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING CI1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-CI1-FILE.

      ****************************
       NEW-RECORD-SETUP SECTION.
      ****************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CI-FILE.

           MOVE HOLD-CI1-KEY TO CI1-KEY.
           MOVE LN-OWNBR   TO CI-BRNO.
           MOVE LN-SSNO(1) TO CI-SSNO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110010." TO VDU.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM GET-BW.
           IF IO-BAD
              MOVE "INVALID" TO BW-LNAME
              MOVE "BORROWER" TO BW-FNAME.
           MOVE "S  A330NAME1." TO VDU.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO VDUBUF.
           PERFORM SCREENIO.
           MOVE LN-SSNO(2) TO CI-SSNO-2 BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110020." TO VDU.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           IF LN-SSNO(2) NOT = 0
              PERFORM GET-BW
              IF IO-BAD
                 MOVE "INVALID" TO BW-LNAME
                 MOVE "BORROWER" TO BW-FNAME
              END-IF
              MOVE "S  A330NAME2." TO VDU
              STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                           INTO VDUBUF
              PERFORM SCREENIO.
           MOVE "S  8080030." TO VDU.
           MOVE LN-ENTDATE TO VDU-DATE-YYYYMMDD CI-DATE-UPDATED.
           PERFORM SCREENIO.

       NEW-RECORD-EXIT.
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
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO D-011.

           IF CI-SSNO-2 NOT = 0
              MOVE CI-SSNO-2              TO D-02 BW-SSNO
              INSPECT D-02 REPLACING ALL "/" BY "-"
              PERFORM READ-BW1-FILE
              IF IO-BAD
                 MOVE "INVALID" TO BW-LNAME
                 MOVE "BORROWER" TO BW-FNAME
              END-IF
              STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
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

      ***********************************************
       ENTER-SCANKEY SECTION.
      ***********************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-CI1-FILE.

           MOVE CI-PATH-OWNBR TO CI-BRNO.
           MOVE 0             TO CI-ACCTNO.

           PERFORM START-CI1-FILE.
       ENTER-KEY.
           MOVE "EN N060SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE CI1-KEY TO HOLD-CI1-KEY
                 MOVE CI-REC TO CIOR-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUNUM0 TO CI-ACCTNO.

           PERFORM START-CI1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-CI1-FILE-NEXT.
           IF IO-FG = 9 OR (CI-BRNO NOT = CI-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK CI-FILE.
           MOVE "SN N060ACCTNO." TO VDU.
           MOVE CI-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-CI1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ***********************************************
       BORROWER-SCAN SECTION.
      ***********************************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-SCAN.
           MOVE BWSCAN-NO TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           MOVE ALP-BW TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
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

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY SPACES UPON CIMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CIMAIN-SCREEN UPON CIMAIN-WINDOW-HANDLE.
           MOVE CIMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CIMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE CIMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CIMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CIMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPCICL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************
       MISC-PARAGRAPHS SECTION.
      ****************************
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
       GET-BW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
      *-----------------------------------------------------------------
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CI1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPCI1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM3MU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY CIMAIN-WINDOW-HANDLE
           EXIT PROGRAM.
