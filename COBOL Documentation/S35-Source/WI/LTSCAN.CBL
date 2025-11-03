       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LTSCAN.
      ******************************************************************
      *
      *   DIR :  WI
      *   DESC: LOAN TRAILER RECORD INQUIRY/CHANGE
      *
      *   CALLED: ONLY CALLED FROM CL/CLMENU.C F9
      *
      *=================================================================
      * REV:
      * 042095 BAH ADDED NEW FIELDS
      * 060195 BAH CHANGED LT-MILEAGE TO LT-MILEAGE
      *                    LT-VALUE TO LT-VALUE
      *                    LT-SELLPR TO LT-SELLPR
      *                    LT-MISC-AMT TO LT-MISC-AMT
      * 042696 JTG CHANGED TO INSURE LN-SEQNO = 1, IN ALL CASES
      *            WHERE MORELN'S WITH ADDITIONAL SEQNO'S EXIST
      * 050797 BAH ADDED PASSWORD TO MODIFY ANY FIELD, FLAUTO
      * BAH 000131 VERIFY THE VIN YOU ENTERED DOES NOT ALREADY EXIST
      *            IN THE IXFILE, JUST DISPLAYS A MESSAGE, TENFIN PR#1064
      * BAH 020617 LOG ANY FIELDS THAT GOT CHANGED IN A LOAN MEMO IF
      *            THE BRANCH FLAG IS ON, AND CREATE A SEPARATE CH RECORD
      *            FOR EACH FIELD CHANGED, PL# 362 REGACC
      * BAH 021024 ADD F2-MEMO FOR "T" COLLATERAL MEMOS, PCCREDIT PR# 1940
      * BAH 021113 MESSED UP LOGGING THE 020617 CHANGE INTO MEMO, DIDNT
      *            SPACE OUT A FIELD WHEN CHANGING A NUMERIC FIELD,
      *            LIKE SELLPR, HAD GARBAGE AFTER THE NUMERIC VALUE....
      * BAH 030528 ADDED P2 FOR LIENHOLDER INFO (WI/LTLIEN), REGACC PR# 20
      * BAH 030711 GUNK REMAINED IN THE CH-NEW-CHAR CAUSE IT DID NOT
      *            GET CLEARED OUT BEFORE MOVING IN CH-NEW-NUM'S..
      *            CAUSED AUDIT-CHANGES REGISTER TO NOT LINE UP CORRECTLY
      * BAH 030722 CHANGE OF TITLE WAS NOT CREATING MEMO OR CH, KEVIN
      * BAH 040803 ADDED LIBWI/TITLEW, REGACC PR# 58
      * BAH 060119 PASSWORD TITLE, SOAUTO SAFCO PR# 2688
      * BLF 070815 IF ARCHIVED LOAN, SET UP ARCHIVE PATH FOR LT & DON'T
      *            ALLOW ANY CHANGES OR UPDATES, WORLD PR# 547
      * BAH 100128 ADDED LT-SEQ 5 FOR 3RD HHGS ONLY FOR UNITED ACCEPT
      *                                            UNITED PR# 3524
      * BAH 120905 ADDED LT-VIN-CHG-DATE FOR REGACC PR# 319
      * BAH 130923 ALLOW GP-I-AM-WDT TO USE SEQ 5 ALSO, WDT PR# 4095
      * BAH 140505 RENAMED WI/TITLE TO WI/TITLEW, RESERVED WORD VERYANT
      * BAH 140814 LM-DATE WAS USING SYS-DATE, NOT SET IN A30. FIXED
      * BAH 150929 PASSWORD HHGS2, FIELD 26, REGMGM PR# 4856
      * BAH 151013 PASSWORD HHGS2 EVEN ON NEW ENTRY, REGMGM PR#4856
      * BAH 160114 SPLIT OUT LTFILE, LTC-FILE, PD0003
      * BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0602 RENAMED INTERNAL LN-ACCTNO TO BE WSS-LN-ACCTNO TO
      *         REMOVE CONFUSION THAT IT IS IN LN-REC.
      *         RENAMED WS-ACCTNO TO BE WSS-IX-ACCTNO.
      * BAH 181130 GLOBAL LMFILE
      *  CS 181206 GLOBAL LTCFILE
      *  CS 181220 GLOBAL IXFILE
      * BAH 2024.0416 USE EXT-WI-CL-ACCTNO, DONT NEED TO PASS ANYTHING
      *               REMOVED WSS-IX-ACCTNO, USE IX-ACCTNO
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
           "@(#)WI/LTSCAN.CBL S35 02/22/24-12:53:24 >".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       01  ERRCD             PIC X VALUE " ".

       01  LTSCAN-LINES          PIC 99  VALUE 20.
       01  LTSCAN-COLUMNS        PIC 99  VALUE 78.
       01  LTSCAN-BEGIN-Y        PIC 99  VALUE 2.
       01  LTSCAN-BEGIN-X        PIC 99  VALUE 3.

      *********************************************************
      *   LTFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTC-ARCHIVE-PATH.
      *    03  FILLER                  PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                  PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                  PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                  PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                  PIC X(11) VALUE "/LP/LTCFILE".


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

       01  LEGEND-01.
           03  FILLER       PIC X(53)  VALUE
           "F1-RESET, F2-MEMO, F7-EXIT AND UPDATE, P2, P3, ENTER ".
           03  FILLER       PIC X(8)  VALUE
           "FIELD #:".

       01  HOLD-SYS-DATE   PIC 9(8).

       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  CHAN              PIC X(4).
       01  OUT-SUB           PIC 99     COMP-3 VALUE 0.
       01  OUT-VIN           PIC X(6) VALUE SPACES.
       01  OUT-VINX REDEFINES OUT-VIN.
           03  OUT-VIN-BYTE  PIC X OCCURS 6.

       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1 REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LTC.CPY" REPLACING
                  LEADING "LTC" BY "LTC-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LTC.CPY" REPLACING
                  LEADING "LTC" BY "LTC-CH".

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC X(10).
           03  F02           PIC X(16).
           03  F03           PIC X(16).
           03  F04           PIC X(4).
           03  F05           PIC X(8).
           03  F06           PIC X(8).
           03  F07           PIC X(20).
           03  F08           PIC X(8).
           03  F09           PIC Z(7)-     BLANK ZERO.
           03  F10           PIC Z(5).99   BLANK ZERO.
           03  F11           PIC Z(5).99   BLANK ZERO.
           03  F12           PIC Z(5).99-  BLANK ZERO.

           03  F13           PIC X(10).
           03  F14           PIC X(16).
           03  F15           PIC X(16).
           03  F16           PIC X(4).
           03  F17           PIC X(8).
           03  F18           PIC X(8).
           03  F19           PIC X(20).

           03  F20           PIC X(8).
           03  F21           PIC Z(7)-     BLANK ZERO.
           03  F22           PIC Z(5).99   BLANK ZERO.
           03  F23           PIC Z(5).99   BLANK ZERO.
           03  F24           PIC Z(5).99-  BLANK ZERO.

           03  F25           PIC X(78).

           03  F26           PIC X(78).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(24) VALUE
           "210 220 230 240 250 260.".

       01  DISPLAY-BUF2   VALUE SPACES.
           03  F261         PIC X(78).
       01  DISPLAY-LIST2.
           03  FILLER       PIC X(4) VALUE "261.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX             PIC 99   VALUE 26.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "E  A100 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A040 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
      * MILEAGE
           03  FILLER      PIC X(8) VALUE "ENNS070 ".
      * VALUE
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * SELLPR
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * MISC-AMT
           03  FILLER      PIC X(8) VALUE "ENNS052 ".

           03  FILLER      PIC X(8) VALUE "E  A100 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A160 ".
           03  FILLER      PIC X(8) VALUE "E  A040 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A080 ".
      * MILEAGE
           03  FILLER      PIC X(8) VALUE "ENNS070 ".
      * VALUE
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * SELLPR
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * MISC-AMT
           03  FILLER      PIC X(8) VALUE "ENNS052 ".

           03  FILLER      PIC X(8) VALUE "E  A780 ".
           03  FILLER      PIC X(8) VALUE "E  A780 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 26 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      * KEEP SAME PASSOWRDS AS LTCREA
       01  MODIFY-SEQ              PIC 99 VALUE 01.
       01  TITLE-SEQ               PIC 99 VALUE 02.
       01  HHGS2-SEQ               PIC 99 VALUE 03.

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBWI/TITLEW.CPY".

       01  LTSCAN-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LTSCAN_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/LTSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LTSCAN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LTSCAN" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO
              MOVE "N" TO BR-COLL-ID-SCAN.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT  SCREEN LINE LTSCAN-BEGIN-X * VDU-WIN-L-OFFSET
               AT  SCREEN COL  LTSCAN-BEGIN-Y * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
               SIZE    LTSCAN-COLUMNS + VDU-WIN-W-MOD
               LINES   LTSCAN-LINES + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS LTSCAN-WINDOW-HANDLE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           MOVE SPACE TO IO-TYPE.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
           MOVE " " TO IO-TYPE.
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
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              MOVE "L" TO MEMOSC-ONLY
              MOVE "T" TO MEMOSC-MEMO-TYPE
              MOVE HOLD-SYS-DATE TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              PERFORM UPDATE-LT
              PERFORM CREATE-CHANGE-REC
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

      * IF VIEWING COLLATERAL FOR AN ARCHIVED LOAN, DON'T ALLOW
      * ANY CHANGE OR UPDATE; ALSO, DON'T ALLOW TO P2 OR P3
      * SINCE I DON'T HAVE AN EASY WAY TO PASS THE ARCHIVE
      * FLAG

      *    IF EXT-WI-CL-ARCHIVE-FG = "Y"
      *       GO TO MODIFY-ROUTINE.

           IF FORM-KEY1 = " "
              IF FORM-P1 = "P"
                 IF FORM-SEL1 = 2
                    CALL "LTLIEN" USING FORM-PATH EXT-WI-CL-OWNBR
                                                  EXT-WI-CL-ACCTNO
                    CANCEL "LTLIEN"
                    GO TO MODIFY-ROUTINE
                 ELSE
                 IF FORM-SEL1 = 3
                    MOVE EXT-WI-CL-OWNBR  TO TITLE-LN-OWNBR
                    MOVE EXT-WI-CL-ACCTNO TO TITLE-LN-ACCTNO
                    MOVE "N" TO TITLE-FROM-LONPFA
                    CALL "TITLEW" USING FORM-PATH TITLE-PROCESSING-BUF
                    CANCEL "TITLEW"
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO MODIFY-ROUTINE.
           IF ACTION = SPACES
              GO TO MODIFY-ROUTINE.

           INSPECT ACTION REPLACING ALL " " BY ZEROS.
           IF CHNO-X NOT NUMERIC
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

           IF ELE = 8 OR 20
              MOVE TITLE-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE TITLE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF ELE = 26
              MOVE HHGS2-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE HHGS 2:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

           IF ELE = 1 OR 13
              MOVE " F1-RESET, ENTER DESCRIPTION:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 2 OR 14
              MOVE " F1-RESET, ENTER MAKE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 3 OR 15
              MOVE " F1-RESET, ENTER MODEL:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 4 OR 16
              MOVE " F1-RESET, ENTER YEAR:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 5 OR 17
              MOVE " F1-RESET, ENTER STYLE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 6 OR 18
              MOVE " F1-RESET, ENTER CLASS:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 7 OR 19
              MOVE " F1-RESET, ENTER ID #:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 25
              MOVE " F1-RESET, ENTER GOODS AND APPLIANCES 1:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 26
              MOVE " F1-RESET, ENTER GOODS AND APPLIANCES 2:" TO MESS
              PERFORM SEND-LEGEND.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
      *    IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = CHAN
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
              GO TO EXIT-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08
                 A-09 A-10 A-11 A-12 A-13 A-14 A-15 A-16
                 A-17 A-18 A-19 A-20 A-21 A-22 A-23 A-24
                 A-25 A-26
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       A-01.
           MOVE VDUBUF TO LTC-DESC(1).
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO LTC-MAKE(1).
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO LTC-MODEL(1).
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO LTC-YEAR(1).
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO LTC-STYLE(1).
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO LTC-CLASS(1).
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO LTC-VIN(1).
           IF VDUBUF NOT = SPACES
              IF BR-COLL-ID-SCAN = "Y" OR "S"
                 MOVE LTC-VIN(1) TO IX-VIN
                 PERFORM VERIFY-VIN
                 IF IO-FG = 0
                    STRING "VIN ALREADY EXIST, ACCOUNT: ",
                       IX-ACCTNO DELIMITED BY "  " INTO MESS
                    PERFORM SEND-MESS.

           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO LTC-TITLE-NO(1).
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM0 TO LTC-MILEAGE(1).
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO LTC-VALUE(1).
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM2 TO LTC-SELLPR(1).
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO LTC-MISC-AMT(1).
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO LTC-DESC(2).
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUBUF TO LTC-MAKE(2).
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUBUF TO LTC-MODEL(2).
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUBUF TO LTC-YEAR(2).
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUBUF TO LTC-STYLE(2).
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUBUF TO LTC-CLASS(2).
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUBUF TO LTC-VIN(2).
           IF VDUBUF NOT = SPACES
              IF BR-COLL-ID-SCAN = "Y" OR "S"
                 MOVE LTC-VIN(2) TO IX-VIN
                 PERFORM VERIFY-VIN
                 IF IO-FG = 0
                    STRING "VIN ALREADY EXIST, ACCOUNT: ",
                       IX-ACCTNO DELIMITED BY "  " INTO MESS
                    PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO LTC-TITLE-NO(2).
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM0 TO LTC-MILEAGE(2).
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO LTC-VALUE(2).
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO LTC-SELLPR(2).
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM2 TO LTC-MISC-AMT(2).
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUBUF TO LTC-HHGS(1).
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUBUF TO LTC-HHGS(2).
           GO TO CREATE-ROUTINE.
       A-261.
           MOVE "E  A780261." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-261.
           MOVE VDUBUF TO LTC-HHGS(3).
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.


      *****************************************
       VERIFY-VIN SECTION.
      *****************************************
           IF BR-COLL-ID-SCAN NOT = "S"
              GO TO VERIFY-START-VIN.

           MOVE SPACES TO OUT-VIN.
           MOVE 20 TO SUB.
           MOVE 6 TO OUT-SUB.
       VERIFY-NEXT-VIN.
           IF VDUTAB(SUB) NOT = SPACES
              MOVE VDUTAB(SUB) TO OUT-VIN-BYTE(OUT-SUB)
              SUBTRACT 1 FROM OUT-SUB.
           IF OUT-SUB > 0
              IF SUB > 1
                 SUBTRACT 1 FROM SUB
                 GO TO VERIFY-NEXT-VIN.

           MOVE OUT-VIN TO IX-VIN.

       VERIFY-START-VIN.
           PERFORM OPEN-IX2-FILE.
             
           MOVE IX-PATH-OWNBR TO IX-BRNO.


           PERFORM START-IX2-FILE.
           PERFORM READ-IX2-FILE-NEXT.
           IF IO-FG = 0
              IF BR-COLL-ID-SCAN  = "S"
                 IF IX-VIN NOT = OUT-VIN OR
                     (IX-BRNO NOT = IX-PATH-OWNBR)
                    MOVE 9 TO IO-FG
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF IX-VIN NOT = VDUBUF OR 
                     (IX-BRNO NOT = IX-PATH-OWNBR)
                    MOVE 9 TO IO-FG.

           PERFORM CLOSE-IX2-FILE.
       EXIT-VERIFY-VIN.
           EXIT.

      *******************************
       DISPLAY-FIELDS SECTION.
      *******************************

           MOVE SPACES TO DISPLAY-BUF
                          LTC-OR-REC
                          LTC-CH-REC.

           MOVE LTC-PATH-OWNBR   TO LTC-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LTC-ACCTNO.
           MOVE 1                TO LTC-SEQNO.
           PERFORM OPEN-LTC1-FILE.
           PERFORM READ-LTC1-FILE.
      *    UNLOCK LTC-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-LTC-FILE
              MOVE LTC-PATH-OWNBR   TO LTC-BRNO
              MOVE EXT-WI-CL-ACCTNO TO LTC-ACCTNO
              MOVE 1                TO LTC-SEQNO.

           MOVE LTC-REC TO LTC-OR-REC LTC-CH-REC.

           MOVE LTC-DESC(1) TO F01.
           MOVE LTC-MAKE(1) TO F02.
           MOVE LTC-MODEL(1) TO F03.
           MOVE LTC-YEAR(1) TO F04.
           MOVE LTC-STYLE(1) TO F05.
           MOVE LTC-CLASS(1) TO F06.
           MOVE LTC-VIN(1) TO F07.
           MOVE LTC-TITLE-NO(1) TO F08.
           MOVE LTC-MILEAGE(1) TO F09.
           MOVE LTC-VALUE(1) TO F10.
           MOVE LTC-SELLPR(1) TO F11.
           MOVE LTC-MISC-AMT(1) TO F12.

           MOVE LTC-DESC(2) TO F13.
           MOVE LTC-MAKE(2) TO F14.
           MOVE LTC-MODEL(2) TO F15.
           MOVE LTC-YEAR(2) TO F16.
           MOVE LTC-STYLE(2) TO F17.
           MOVE LTC-CLASS(2) TO F18.
           MOVE LTC-VIN(2) TO F19.
           MOVE LTC-TITLE-NO(2) TO F20.
           MOVE LTC-MILEAGE(2) TO F21.
           MOVE LTC-VALUE(2) TO F22.
           MOVE LTC-SELLPR(2) TO F23.
           MOVE LTC-MISC-AMT(2) TO F24.

           MOVE LTC-HHGS(1) TO F25.
           MOVE LTC-HHGS(2) TO F26.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * THIRD HHGS
      *    IF GP-I-AM-UNITED OR GP-I-AM-WDT
      *       MOVE LTC-HHGS(3) TO F261
      *       MOVE DISPLAY-BUF2 TO WR-BUF
      *       MOVE DISPLAY-LIST2 TO WR-LIST
      *       PERFORM CALL-WRFORM.


       DISPLAY-FIELDS-EXIT.
           PERFORM CLOSE-LTC1-FILE.

      **************************
       UPDATE-LT SECTION.
      **************************
           PERFORM OPEN-LTC1-FILE.

      * IF ORIGINAL NOT = WHAT YOU JUST BUILT
           IF LTC-OR-REC NOT = LTC-REC
              IF LTC-OR-VIN(1) NOT = LTC-VIN(1)
                 MOVE HOLD-SYS-DATE TO LTC-VIN-CHG-DATE(1)
              ELSE
                 MOVE 0 TO LTC-VIN-CHG-DATE(1)
              END-IF
              IF LTC-OR-VIN(2) NOT = LTC-VIN(2)
                 MOVE HOLD-SYS-DATE TO LTC-VIN-CHG-DATE(2)
              ELSE
                 MOVE 0 TO LTC-VIN-CHG-DATE(2)
              END-IF
              PERFORM WRITE-LTC1-FILE
              IF IO-FG NOT = 0
                 PERFORM REWRITE-LTC1-FILE.

           PERFORM CLOSE-LTC1-FILE.

      *************************************
       CREATE-CHANGE-REC SECTION.
      *************************************
           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-LM1-FILE.

           IF LTC-OR-DESC(1) NOT = LTC-DESC(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO DESCRIPT 1" TO CH-DESCRIPT
              MOVE LTC-OR-DESC(1)    TO CH-ORG-CHAR
              MOVE LTC-DESC(1)       TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MAKE(1) NOT = LTC-MAKE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MAKE 1"  TO CH-DESCRIPT
              MOVE LTC-OR-MAKE(1) TO CH-ORG-CHAR
              MOVE LTC-MAKE(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MODEL(1) NOT = LTC-MODEL(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MODEL 1"  TO CH-DESCRIPT
              MOVE LTC-OR-MODEL(1) TO CH-ORG-CHAR
              MOVE LTC-MODEL(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-YEAR(1) NOT = LTC-YEAR(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO YEAR 1"  TO CH-DESCRIPT
              MOVE LTC-OR-YEAR(1) TO CH-ORG-CHAR
              MOVE LTC-YEAR(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-STYLE(1) NOT = LTC-STYLE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO STYLE 1"  TO CH-DESCRIPT
              MOVE LTC-OR-STYLE(1) TO CH-ORG-CHAR
              MOVE LTC-STYLE(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-CLASS(1) NOT = LTC-CLASS(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO CLASS 1"  TO CH-DESCRIPT
              MOVE LTC-OR-CLASS(1) TO CH-ORG-CHAR
              MOVE LTC-CLASS(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-VIN(1) NOT = LTC-VIN(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO VIN 1"  TO CH-DESCRIPT
              MOVE LTC-OR-VIN(1) TO CH-ORG-CHAR
              MOVE LTC-VIN(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-TITLE-NO(1) NOT = LTC-TITLE-NO(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO TITLE 1"     TO CH-DESCRIPT
              MOVE LTC-OR-TITLE-NO(1) TO CH-ORG-CHAR
              MOVE LTC-TITLE-NO(1)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MILEAGE(1) NOT = LTC-MILEAGE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MILEAGE 1" TO CH-DESCRIPT
              MOVE LTC-OR-MILEAGE(1) TO CH-ORG-NUM
              MOVE LTC-MILEAGE(1) TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-SELLPR(1) NOT = LTC-SELLPR(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO SELLPR 1" TO CH-DESCRIPT
              MOVE LTC-OR-SELLPR(1) TO CH-ORG-2DEC
              MOVE LTC-SELLPR(1) TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-VALUE(1) NOT = LTC-VALUE(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO VALUE 1" TO CH-DESCRIPT
              MOVE LTC-OR-VALUE(1) TO CH-ORG-2DEC
              MOVE LTC-VALUE(1) TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MISC-AMT(1) NOT = LTC-MISC-AMT(1)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MISC AMT 1" TO CH-DESCRIPT
              MOVE LTC-OR-MISC-AMT(1) TO CH-ORG-2DEC
              MOVE LTC-MISC-AMT(1) TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-DESC(2) NOT = LTC-DESC(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO DESCRIPT 2" TO CH-DESCRIPT
              MOVE LTC-OR-DESC(2) TO CH-ORG-CHAR
              MOVE LTC-DESC(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MAKE(2) NOT = LTC-MAKE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MAKE 2" TO CH-DESCRIPT
              MOVE LTC-OR-MAKE(2) TO CH-ORG-CHAR
              MOVE LTC-MAKE(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MODEL(2) NOT = LTC-MODEL(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MODEL 2" TO CH-DESCRIPT
              MOVE LTC-OR-MODEL(2) TO CH-ORG-CHAR
              MOVE LTC-MODEL(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-YEAR(2) NOT = LTC-YEAR(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO YEAR 2" TO CH-DESCRIPT
              MOVE LTC-OR-YEAR(2) TO CH-ORG-CHAR
              MOVE LTC-YEAR(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-STYLE(2) NOT = LTC-STYLE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO STYLE 2" TO CH-DESCRIPT
              MOVE LTC-OR-STYLE(2) TO CH-ORG-CHAR
              MOVE LTC-STYLE(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-CLASS(2) NOT = LTC-CLASS(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO CLASS 2" TO CH-DESCRIPT
              MOVE LTC-OR-CLASS(2) TO CH-ORG-CHAR
              MOVE LTC-CLASS(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-VIN(2) NOT = LTC-VIN(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO VIN 2"  TO CH-DESCRIPT
              MOVE LTC-OR-VIN(2) TO CH-ORG-CHAR
              MOVE LTC-VIN(2)    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-TITLE-NO(2) NOT = LTC-TITLE-NO(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO TITLE 2" TO CH-DESCRIPT
              MOVE LTC-OR-TITLE-NO(2) TO CH-ORG-CHAR
              MOVE LTC-TITLE-NO(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MILEAGE(2) NOT = LTC-MILEAGE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MILEAGE 2" TO CH-DESCRIPT
              MOVE SPACES TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE LTC-OR-MILEAGE(2) TO CH-ORG-NUM
              MOVE LTC-MILEAGE(2) TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-SELLPR(2) NOT = LTC-SELLPR(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO SELLPR 2" TO CH-DESCRIPT
              MOVE SPACES TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE LTC-OR-SELLPR(2) TO CH-ORG-2DEC
              MOVE LTC-SELLPR(2) TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-VALUE(2) NOT = LTC-VALUE(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO VALUE 2" TO CH-DESCRIPT
              MOVE SPACES TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE LTC-OR-VALUE(2) TO CH-ORG-2DEC
              MOVE LTC-VALUE(2) TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-MISC-AMT(2) NOT = LTC-MISC-AMT(2)
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO MISC AMT 2" TO CH-DESCRIPT
              MOVE SPACES TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE LTC-OR-MISC-AMT(2) TO CH-ORG-2DEC
              MOVE LTC-MISC-AMT(2) TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTC-OR-COLL-INFO1(1) NOT = LTC-COLL-INFO1(1)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 1" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO1(1) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO1(1) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTC-OR-COLL-INFO2(1) NOT = LTC-COLL-INFO2(1)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 1" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO2(1) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO2(1) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTC-OR-COLL-INFO3(1) NOT = LTC-COLL-INFO3(1)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 1" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO3(1) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO3(1) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTC-OR-COLL-INFO4(1) NOT = LTC-COLL-INFO4(1)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 1" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO4(1) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO4(1) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.


           IF LTC-OR-COLL-INFO1(2) NOT = LTC-COLL-INFO1(2)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 2" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO1(2) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO1(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTC-OR-COLL-INFO2(2) NOT = LTC-COLL-INFO2(2)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 2" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO2(2) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO2(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTC-OR-COLL-INFO3(2) NOT = LTC-COLL-INFO3(2)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 2" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO3(2) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO3(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTC-OR-COLL-INFO4(2) NOT = LTC-COLL-INFO4(2)
              PERFORM CREATE-SETUP-REC
              MOVE "HHGS 2" TO CH-DESCRIPT
              MOVE LTC-OR-COLL-INFO4(2) TO CH-ORG-CHAR
              MOVE LTC-COLL-INFO4(2) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

      * THIRD HHGS
      *    IF GP-I-AM-UNITED OR GP-I-AM-WDT
      *       IF LTC-OR-COLL-INFO1(3) NOT = LTC-COLL-INFO1(3)
      *          PERFORM CREATE-SETUP-REC
      *          MOVE "HHGS 3" TO CH-DESCRIPT
      *          MOVE LTC-OR-COLL-INFO1(3) TO CH-ORG-CHAR
      *          MOVE LTC-COLL-INFO1(3)    TO CH-NEW-CHAR
      *          PERFORM UPDATE-CH-LM-FILE
      *       END-IF
      *       IF LTC-OR-COLL-INFO2(3) NOT = LTC-COLL-INFO2(3)
      *          PERFORM CREATE-SETUP-REC
      *          MOVE "HHGS 3" TO CH-DESCRIPT
      *          MOVE LTC-OR-COLL-INFO2(3) TO CH-ORG-CHAR
      *          MOVE LTC-COLL-INFO2(3) TO CH-NEW-CHAR
      *          PERFORM UPDATE-CH-LM-FILE
      *       END-IF
      *       IF LTC-OR-COLL-INFO3(3) NOT = LTC-COLL-INFO3(3)
      *          PERFORM CREATE-SETUP-REC
      *          MOVE "HHGS 3" TO CH-DESCRIPT
      *          MOVE LTC-OR-COLL-INFO3(3) TO CH-ORG-CHAR
      *          MOVE LTC-COLL-INFO3(3) TO CH-NEW-CHAR
      *          PERFORM UPDATE-CH-LM-FILE
      *       END-IF
      *       IF LTC-OR-COLL-INFO4(3) NOT = LTC-COLL-INFO4(3)
      *          PERFORM CREATE-SETUP-REC
      *          MOVE "HHGS 3" TO CH-DESCRIPT
      *          MOVE LTC-OR-COLL-INFO4(3) TO CH-ORG-CHAR
      *          MOVE LTC-COLL-INFO4(3) TO CH-NEW-CHAR
      *          PERFORM UPDATE-CH-LM-FILE.

           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CH-FILE.

           GO TO EXIT-CREATE-CHANGE-REC.

       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE EXT-CONAME-USER-LOC TO CH-BRNO.
           MOVE HOLD-SYS-DATE       TO CH-DATE.
           MOVE "T"                 TO CH-CODE.
           MOVE EXT-CONAME-USER-ID  TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS            TO CH-TIME.
           MOVE 1                   TO CH-SEQ.

           MOVE EXT-WI-CL-ACCTNO    TO CH-ACCTNO.
       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ************************************
       UPDATE-CH-LM-FILE SECTION.
      ************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
             IF CH-SEQ < 999
                ADD 1 TO CH-SEQ
                GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO NOT = "Y"
              GO TO UPDATE-CH-LM-FILE-EXIT.

           MOVE LM-PATH-OWNBR    TO SAVE-LM-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - HOLD-SYS-DATE.
           MOVE 1             TO SAVE-LM-SEQ.
       UPDATE-CH-LM-FILE-AGAIN.
           MOVE SPACES TO LM-REC.

           MOVE SAVE-LM1-KEY      TO LM1-KEY.
           MOVE CH-USERID         TO MEMO-USERID.
           MOVE CH-DESCRIPT       TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR       TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA         TO LM-MEMO.
           MOVE "9"               TO LM-MEMO-TYPE.

           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF SAVE-LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO UPDATE-CH-LM-FILE-AGAIN.

       UPDATE-CH-LM-FILE-EXIT.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LTSCAN" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLTCCL.CPY".


      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON LTSCAN-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY LTSCAN-SCREEN UPON LTSCAN-WINDOW-HANDLE.
           MOVE LTSCAN-FORM-FIELDS TO WR-BUF.
           MOVE LTSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE LTSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTSCAN-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LTSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ********************************
       IO-ROUTINES SECTION.
      ********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-IX2-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPIX2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LTSCAN-SCREEN.
           DESTROY LTSCAN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
