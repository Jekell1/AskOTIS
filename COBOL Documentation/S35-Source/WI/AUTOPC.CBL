       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AUTOPC.
       AUTHOR.          KEC.
       DATE-WRITTEN.    07-21-2006.
      ******************************************************************
      *
      * IDENTIFICATION: WI/AUTOPC
      *
      * DESCRIPTION   : SCRATCH PAD TRAILER (PC) RECORD INQUIRY/CHANGE
      *                 AUTO INSURANCE INFORMATION
      *             CALLED FROM LONIJ0, LONIN0, XONPC0, LONPC0, LONPB0,
      *                         VONPB0, ZONPC0, VONPC0, XONIN0
      *
      *=================================================================
      * REV:
      * KEC 2006.0721 PR#00127 REGIONAL ACCEPT. NEW
      * BAH 170406 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20200617 CHANGED PC-LT-RECORD TO PC-LTA-RECORD, ADDED LPPCCL CLEAR
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)WI/AUTOPC A30 09/11/14-16:13:29 09/18/14-13:58:17 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".

       01  ERRCD                       PIC X(01)     VALUE SPACES.

       01  AUTOPC-LINES               PIC 9(02)   VALUE 18.
       01  AUTOPC-COLUMNS             PIC 9(02)   VALUE 78.
       01  AUTOPC-BEGIN-Y             PIC 9(02)   VALUE 02.
       01  AUTOPC-BEGIN-X             PIC 9(02)   VALUE 04.

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  CHAN              PIC X(4).

       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.

       COPY "LIBLP/LTAUTOW.CPY".
       COPY "LIBLP/LTAUTOW.CPY" REPLACING LEADING "WK" BY "ORIG-WK".

       01  DISPLAY-BUF.
           03  D-CAR         PIC X(36).
           03  D-VIN         PIC X(20).
           03  F01           PIC X(25).
           03  F02           PIC X(25).
           03  F03           PIC X(25).
           03  F04           PIC X(25).
           03  F05           PIC X(25).
           03  F06           PIC X(15).
           03  F07           PIC X(2).
           03  F08           PIC X(10).
           03  F09           PIC X(12).
           03  F10           PIC X(25).
           03  F11           PIC X.
           03  F12           PIC X(25).
           03  F13           PIC X.
           03  F14           PIC 99/99/99 BLANK ZERO.
           03  F15           PIC 99/99/99 BLANK ZERO.
           03  F16           PIC 99/99/99 BLANK ZERO.
           03  F17           PIC 99/99/99 BLANK ZERO.
           03  F18           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F19           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F20           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F21           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F22           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F23           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F24           PIC ZZ9 BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "CAR VIN 010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(16) VALUE
           "210 220 230 240.".

       01  MAX             PIC 99   VALUE 24.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A150 ".
           03  FILLER      PIC X(8) VALUE "E  A020 ".
           03  FILLER      PIC X(8) VALUE "E  A100 ".
           03  FILLER      PIC X(8) VALUE "E  A120 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A250 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 24 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
       01  MODIFY-SEQ               PIC 99 VALUE 01.
      *-----------------------------------------------------------------

       01  AUTOPC-WINDOW-HANDLE        PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/AUTOPC_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/AUTOPC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/AUTOPCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/AUTOPC_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME AUTOPC-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "AUTOPC" TO VDU-FORM-NAME.

           MOVE SPACES TO VDU-WIMENU-SELECTION.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE AUTOPC-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  AUTOPC-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    AUTOPC-COLUMNS + VDU-WIN-W-MOD
                   LINES   AUTOPC-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS AUTOPC-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE SPACES TO BR-CHANGE-MEMO.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE SPACES TO IO-TYPE.
           MOVE 01     TO NEW-FLAG.
           PERFORM DISPLAY-FIELDS.

           PERFORM ENTRY-MODULE.
           IF ( IO-TYPE = "CLR" ) GO TO MAIN-MODULE.
           IF ( IO-TYPE = "END" ) GO TO MAIN-PROGRAM-EXIT.

           GO TO MAIN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-ROUTINE.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
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
           MOVE
           "F1-RESET, F7-EXIT WITH UPDATE, ENTER FIELD#:"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              PERFORM SAVE-MODULE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF ACTION = " "
              GO TO MODIFY-ROUTINE.

       CHECK-CHNO.
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

       ENTER-ELE.
           IF ELE = 1
              MOVE " F1-RESET, ENTER INSURANCE CO. NAME:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 2
              MOVE " F1-RESET, ENTER INS. AGENCY NAME:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 3
              MOVE " F1-RESET, ENTER INS. AGENT NAME:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 4
              MOVE " F1-RESET, ENTER ADDRESS LINE 1:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 5
              MOVE " F1-RESET, ENTER ADDRESS LINE 2:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 6
              MOVE " F1-RESET, ENTER CITY:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 7
              MOVE " F1-RESET, ENTER STATE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 8
              MOVE " F1-RESET, ENTER ZIP CODE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 9
              MOVE " F1-RESET, ENTER PHONE NUMBER:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 10
              MOVE " F1-RESET, ENTER POLICY NUMBER:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 11
              MOVE " F1-RESET, ENTER COVERAGE TYPE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 12
              MOVE " F1-RESET, ENTER LOSS PAYEE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 13
              MOVE " F1-RESET, ENTER DEALER REQUIREMENT CODE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 14
              MOVE " F1-RESET, ENTER EFFECTIVE DATE OF POLICY:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 15
              MOVE " F1-RESET, ENTER EXPIRATION DATE OF POLICY:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 16
              MOVE " F1-RESET, ENTER CANCELLATION DATE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 17
              MOVE " F1-RESET, ENTER CONFIRMATION DATE:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 18
              MOVE " F1-RESET, ENTER BODILY INJURY LIMIT:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 19
              MOVE " F1-RESET, ENTER PROPERTY DAMAGE LIMIT:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 20
              MOVE " F1-RESET, ENTER PIP LIMIT:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 21
              MOVE " F1-RESET, ENTER UMBRELLA LIMIT:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 22
              MOVE " F1-RESET, ENTER COLLISION LIMIT:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 23
              MOVE " F1-RESET, ENTER COMPREHENSIVE LIMIT:" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 24
              MOVE " F1-RESET, ENTER LAST FORM NO:" TO MESS
              PERFORM SEND-LEGEND.

           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F7.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              PERFORM SAVE-MODULE
              GO TO EXIT-ENTRY-MODULE.
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
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       A-01.
           MOVE VDUBUF TO WK-A-INS-CO.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO WK-A-AGENCY.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO WK-A-AGENT.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO WK-A-ADR1.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO WK-A-ADR2.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO WK-A-CITY.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO WK-A-STATE.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO WK-A-ZIP.
           GO TO CREATE-ROUTINE.
       A-09.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO WK-A-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120090." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUBUF TO WK-A-POLICY-NO.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO WK-A-COV-TYPE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO WK-A-LOSS-PAYEE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO WK-A-DLR-REQ.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDU-DATE-YYYYMMDD TO WK-A-EFF-DATE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDU-DATE-YYYYMMDD TO WK-A-EXP-DATE.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDU-DATE-YYYYMMDD TO WK-A-CAN-DATE.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDU-DATE-YYYYMMDD TO WK-A-CONF-DATE.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM2 TO WK-A-BIL-COV.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO WK-A-PDL-COV.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM2 TO WK-A-PIP-COV.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM2 TO WK-A-UMB-COV.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO WK-A-COL-COV.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO WK-A-COM-COV.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM0 TO WK-A-LAST-FORM.
           GO TO CREATE-ROUTINE.
       EXIT-ENTRY-MODULE.
           EXIT.

      ********************************
       DISPLAY-FIELDS SECTION.
      ********************************
           MOVE " " TO DISPLAY-BUF
                       ORIG-WK-AUTO-INS    WK-AUTO-INS.

           MOVE 0 TO   ORIG-WK-A-PHONE      WK-A-PHONE
                       ORIG-WK-A-EFF-DATE   WK-A-EFF-DATE
                       ORIG-WK-A-EXP-DATE   WK-A-EXP-DATE
                       ORIG-WK-A-CAN-DATE   WK-A-CAN-DATE
                       ORIG-WK-A-CONF-DATE  WK-A-CONF-DATE
                       ORIG-WK-A-LAST-FORM  WK-A-LAST-FORM
                       ORIG-WK-A-BIL-COV    WK-A-BIL-COV
                       ORIG-WK-A-PDL-COV    WK-A-PDL-COV
                       ORIG-WK-A-PIP-COV    WK-A-PIP-COV
                       ORIG-WK-A-UMB-COV    WK-A-UMB-COV
                       ORIG-WK-A-COL-COV    WK-A-COL-COV
                       ORIG-WK-A-COM-COV    WK-A-COM-COV.


           PERFORM LOAD-MODULE.

           PERFORM SET-AUTO1-FIELDS.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           PERFORM CLOSE-PC1-FILE.

      ********************************
       SET-AUTO1-FIELDS SECTION.
      ********************************
           MOVE WK-A-INS-CO TO F01.
           MOVE WK-A-AGENCY TO F02.
           MOVE WK-A-AGENT TO F03.
           MOVE WK-A-ADR1 TO F04.
           MOVE WK-A-ADR2 TO F05.
           MOVE WK-A-CITY TO F06.
           MOVE WK-A-STATE TO F07.
           MOVE WK-A-ZIP TO F08.
           IF WK-A-PHONE NOT = 0
              MOVE WK-A-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F09.
           MOVE WK-A-POLICY-NO TO F10.
           MOVE WK-A-COV-TYPE TO F11.
           MOVE WK-A-LOSS-PAYEE TO F12.
           MOVE WK-A-DLR-REQ TO F13.
           MOVE WK-A-EFF-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F14.
           MOVE WK-A-EXP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F15.
           MOVE WK-A-CAN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F16.
           MOVE WK-A-CONF-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F17.
           MOVE WK-A-BIL-COV TO F18.
           MOVE WK-A-PDL-COV TO F19.
           MOVE WK-A-PIP-COV TO F20.
           MOVE WK-A-UMB-COV TO F21.
           MOVE WK-A-COL-COV TO F22.
           MOVE WK-A-COM-COV TO F23.
           MOVE WK-A-LAST-FORM TO F24.

      ******************************************************************
       LOAD-MODULE SECTION.
      ******************************************************************

           MOVE ZEROES TO NEW-FLAG.

           PERFORM OPEN-PC1-FILE.

           MOVE AUTOPC-PC1-KEY             TO PC1-KEY.
           PERFORM READ-PC1-FILE.
           IF ( IO-GOOD )
      *       UNLOCK PC-FILE
              GO TO LOAD-SET-FIELDS.

           MOVE 1                      TO NEW-FLAG.

           PERFORM CLEAR-PC-FILE.

           MOVE AUTOPC-PC1-KEY         TO PC1-KEY.

           MOVE SPACES                 TO WK-AUTO-INS.
           MOVE ZEROES                 TO WK-A-PHONE
                                          WK-A-EFF-DATE
                                          WK-A-EXP-DATE
                                          WK-A-CAN-DATE
                                          WK-A-CONF-DATE
                                          WK-A-LAST-FORM
                                          WK-A-BIL-COV
                                          WK-A-PDL-COV
                                          WK-A-PIP-COV
                                          WK-A-UMB-COV
                                          WK-A-COL-COV
                                          WK-A-COM-COV.

           MOVE WK-AUTO-INS            TO PC-LTA-RECORD.
           MOVE PC-ID                  TO PC2-ID.
           MOVE PC-BRANCH              TO PC2-BRANCH.
           MOVE PC-SOC-SEC             TO PC2-BWNAME.

       LOAD-SET-FIELDS.

           MOVE PC-LTA-RECORD          TO WK-AUTO-INS
                                          ORIG-WK-AUTO-INS.

       LOAD-MODULE-END.
           PERFORM CLOSE-PC1-FILE.
       LOAD-MODULE-EXIT.
           EXIT.

      ******************************************************************
       SAVE-MODULE SECTION.
      ******************************************************************

           IF ( ORIG-WK-AUTO-INS = WK-AUTO-INS )
              GO TO SAVE-MODULE-EXIT.

           PERFORM OPEN-PC1-FILE.

           MOVE AUTOPC-PC1-KEY             TO PC1-KEY.
           PERFORM READ-PC1-FILE.
           IF ( IO-GOOD )
              GO TO SAVE-MODULE-UPDATE.

           PERFORM CLEAR-PC-FILE.

           MOVE AUTOPC-PC1-KEY             TO PC1-KEY.
           MOVE PC-ID                      TO PC2-ID.
           MOVE PC-BRANCH                  TO PC2-BRANCH.
           MOVE PC-SOC-SEC                 TO PC2-BWNAME.

           PERFORM WRITE-PC1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       SAVE-MODULE-UPDATE.

           MOVE WK-AUTO-INS                TO PC-LTA-RECORD.
           PERFORM REWRITE-PC1-FILE.

       SAVE-MODULE-END.
           PERFORM CLOSE-PC1-FILE.
       SAVE-MODULE-EXIT.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "AUTOPC" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPPCCL.CPY".

      *****************************************
       FORM-RESET SECTION.
      *****************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY AUTOPC-SCREEN UPON AUTOPC-WINDOW-HANDLE.
           MOVE AUTOPC-FORM-FIELDS TO WR-BUF.
           MOVE AUTOPC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE AUTOPC-HELP--FILE TO HELP-LINK-FILE.
           MOVE AUTOPC-HELP--DIR TO HELP-LINK-DIR.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/AUTOPC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARAGRAPHS SECTION.
      ******************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.


      ******************************
       IO-ROUTINES SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPPC1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY AUTOPC-SCREEN.
           DESTROY AUTOPC-WINDOW-HANDLE.
           EXIT PROGRAM.
