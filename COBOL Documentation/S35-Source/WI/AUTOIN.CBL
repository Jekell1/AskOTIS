       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AUTOIN.
      **********************************************************
      *   DIR :  WI
      *   DESC: LOAN TRAILER RECORD INQUIRY/CHANGE
      *         AUTO INSURANCE INFORMATION
      *            CALLED FROM F9 CLMENU
      *
      *  2025.0409 WORLD DOES NOT USE. THEY ARE ONLY ALLOWING 'A' LOGINS
      *            INTO IT
      *
      * REV:
      *  BAH 101797 ADDED PASSWORD TO MODIFY ANY FIELD, INSTANT PR# 565
      *  MJD 112597 EXPANDED LT-REC TO 144 BYTES (ADDED 30)
      *  BAH 030311 CREATE LOAN MEMOS TYPE "A" FOR EVERY INDIVIDUAL FIELD
      *             THAT GETS CHANGED, AND CREATE A CHANGES REC,
      *             TRITON PR# 2029
      *  BAH 030711 GUNK REMAINED IN THE CH-NEW-CHAR CAUSE IT DID NOT
      *             GET CLEARED OUT BEFORE MOVING IN CH-NEW-NUM'S..
      *             CAUSED AUDIT-CHANGES REGISTER TO NOT LINE UP CORRECTLY
      *  BAH 070502 ADDED INSURANCE TRACKING FIELDS IN LT-A-RECORD3
      *             REGACC PR# 173
      *  BAH 070713 CHANGE LOG DATE WAS USING SYS-DATE WHICH WAS GETTING
      *             CHANGED SOMEWHERE ELSE IN THE PROGRAM, SOME MEMO DATES
      *             WERE COMING OUT IN THE FUTURE ! KEVIN/REGACC
      *  BAH 080308 CHANGE PROMPT 13 FROM "DEALER REQ. :" TO "INSUR REQ.  :"
      *             FOR GP-I-AM-REGACC ONLY PR# 201
      *  BAH 081230 ALLOW RETURN THRU FIELDS 25,26 AND 27 UNLESS YOU ARE
      *             GP-I-AM-REGACC...JIMR
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *  BAH 170406 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181130 GLOBAL LMFILE
      *  BAH 190513 NEW CHFILE
      **********************************************************
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
           "@(#)WI/AUTOIN.CBL S35 02/22/24-12:51:18 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  AUTOIN-LINES          PIC 99  VALUE 21.
       01  AUTOIN-COLUMNS        PIC 99  VALUE 78.
       01  AUTOIN-BEGIN-COL      PIC 99  VALUE 2.
       01  AUTOIN-BEGIN-LINE     PIC 99  VALUE 3.

       01  ERRCD             PIC X     VALUE SPACES.
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

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

       01  SYSTEM-DATE-YYYYMMDD   PIC 9(8).

       01  LEGEND-TEXT.
           03  FILLER      PIC X(38) VALUE
           "F1-RESET, F3-MEMOS, F7-EXIT & UPDATE, ".
           03  FILLER      PIC X(13) VALUE
           "ENTER FIELD#:".
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

       01  CAR-WORK          PIC X(45) VALUE SPACES.


      * ORIGINAL RECORD
       COPY "LIBLP/LP01LTA.CPY" REPLACING LEADING "LTA" BY "LTA-OR".
      * CHANGED RECORD
       COPY "LIBLP/LP01LTA.CPY" REPLACING LEADING "LTA" BY "LTA-CH".

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
           03  F25           PIC X.
           03  F251          PIC X(12).
           03  F26           PIC X.
           03  F261          PIC X(12).
           03  F27           PIC X.
           03  F271          PIC X(12).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "CAR VIN 010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(40) VALUE
           "210 220 230 240 250 251 260 261 270 271.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX             PIC 99   VALUE 27.
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
      * 24 LAST FORM #
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      * 25 TRACK ONLY
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 27
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 27 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".

       COPY "LIBGB/PASSWDW.CPY".
       01  MODIFY-SEQ               PIC 99 VALUE 01.
       01  TRACK-SEQ                PIC 99 VALUE 02.
       01  INS-SEQ                  PIC 99 VALUE 03.
       01  CPP-SEQ                  PIC 99 VALUE 04.

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".

       01  AUTOIN-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/AUTOIN_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/AUTOIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/AUTOIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AUTOIN" TO VDU-FORM-NAME.

      D    STOP MESS.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE AUTOIN-BEGIN-LINE * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  AUTOIN-BEGIN-COL * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    AUTOIN-COLUMNS + VDU-WIN-W-MOD
                   LINES   AUTOIN-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS AUTOIN-WINDOW-HANDLE.


           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSTEM-DATE-YYYYMMDD.

           PERFORM OPEN-LN1-FILE.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           MOVE SPACE TO IO-TYPE.
           MOVE 1 TO NEW-FLAG.
           PERFORM DISPLAY-FIELDS.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

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
           MOVE LEGEND-TEXT TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              PERFORM UPDATE-LT
              PERFORM CREATE-CHANGE-REC
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "16"
              MOVE "L" TO MEMOSC-ONLY
              MOVE "A" TO MEMOSC-MEMO-TYPE
              MOVE SYSTEM-DATE-YYYYMMDD TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.

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

           IF ELE = 25
              MOVE TRACK-SEQ TO PASSWDW-SEQ
              MOVE "TO MODIFY TRACK:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.
           IF ELE = 26
              MOVE INS-SEQ TO PASSWDW-SEQ
              MOVE "TO MODIFY INSURANCE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.
           IF ELE = 27
              MOVE CPP-SEQ TO PASSWDW-SEQ
              MOVE "TO MODIFY CPP PROFILE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
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
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 25
              MOVE " F1-RESET, ENTER TRACK ONLY (Y/N):" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 26
              MOVE " F1-RESET, ENTER INSURANCE (Y/N):" TO MESS
              PERFORM SEND-LEGEND
           ELSE
           IF ELE = 27
              MOVE " F1-RESET, ENTER CPP (Y/N) :" TO MESS
              PERFORM SEND-LEGEND
           ELSE
              MOVE SPACES TO MESS
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
       TEST-F7.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              PERFORM UPDATE-LT
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
                 A-25 A-26 A-27
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       A-01.
           MOVE VDUBUF TO LTA-INS-CO.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO LTA-AGENCY.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO LTA-AGENT.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO LTA-ADR1.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO LTA-ADR2.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO LTA-CITY.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO LTA-STATE.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO LTA-ZIP.
           GO TO CREATE-ROUTINE.
       A-09.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LTA-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120090." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUBUF TO LTA-POLICY-NO.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO LTA-COV-TYPE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO LTA-LOSS-PAYEE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO LTA-DLR-REQ.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDU-DATE-YYYYMMDD TO LTA-EFF-DATE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDU-DATE-YYYYMMDD TO LTA-EXP-DATE.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDU-DATE-YYYYMMDD TO LTA-CAN-DATE.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDU-DATE-YYYYMMDD TO LTA-CONF-DATE.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM2 TO LTA-BIL-COV.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO LTA-PDL-COV.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM2 TO LTA-PIP-COV.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM2 TO LTA-UMB-COV.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO LTA-COL-COV.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO LTA-COM-COV.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM0 TO LTA-LAST-FORM.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUBUF TO LTA-TRACK-ONLY.
       A-251.
           MOVE "ENTER TRACK MONTHS :" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A120251." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-251.
           MOVE VDUBUF TO LTA-TRACK-MONTHS.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUBUF TO LTA-INSURANCE.
       A-261.
           MOVE "ENTER INSURANCE MONTHS :" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A120261." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-261.
           MOVE VDUBUF TO LTA-INS-MONTHS.
           GO TO CREATE-ROUTINE.
       A-27.
      * DISPLAY ONLY

       A-271.
           MOVE "ENTER CPP MONTHS :" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A120271." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-271.
           MOVE VDUBUF TO LTA-CPP-MONTHS.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      *********************************
       DISPLAY-FIELDS SECTION.
      *********************************
           MOVE SPACES TO DISPLAY-BUF
                          LTA-OR-REC
                          LTA-CH-REC.

           PERFORM GET-COLLATERAL-INFO.

           PERFORM OPEN-LTA1-FILE.
           MOVE LN-OWNBR  TO LTA-BRNO.
           MOVE LN-ACCTNO TO LTA-ACCTNO.
           MOVE 1         TO LTA-SEQNO.
           PERFORM READ-LTA1-FILE.
      *    UNLOCK LTA-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-LTA-FILE
              MOVE LN-OWNBR  TO LTA-BRNO
              MOVE LN-ACCTNO TO LTA-ACCTNO
              MOVE 1         TO LTA-SEQNO
           ELSE
              MOVE 0 TO NEW-FLAG.

           MOVE LTA-REC TO LTA-OR-REC LTA-CH-REC.

           PERFORM SET-AUTO1-FIELDS.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           PERFORM CLOSE-LTA1-FILE.

      *********************************
       SET-AUTO1-FIELDS SECTION.
      *********************************
           MOVE LTA-INS-CO TO F01.
           MOVE LTA-AGENCY TO F02.
           MOVE LTA-AGENT TO F03.
           MOVE LTA-ADR1 TO F04.
           MOVE LTA-ADR2 TO F05.
           MOVE LTA-CITY TO F06.
           MOVE LTA-STATE TO F07.
           MOVE LTA-ZIP TO F08.
           IF LTA-PHONE NOT = 0
              MOVE LTA-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F09.
           MOVE LTA-POLICY-NO TO F10.
           MOVE LTA-COV-TYPE TO F11.
           MOVE LTA-LOSS-PAYEE TO F12.
           MOVE LTA-DLR-REQ TO F13.
           MOVE LTA-EFF-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F14.
           MOVE LTA-EXP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F15.
           MOVE LTA-CAN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F16.
           MOVE LTA-CONF-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F17.
           MOVE LTA-BIL-COV TO F18.
           MOVE LTA-PDL-COV TO F19.
           MOVE LTA-PIP-COV TO F20.
           MOVE LTA-UMB-COV TO F21.
           MOVE LTA-COL-COV TO F22.
           MOVE LTA-COM-COV TO F23.
           MOVE LTA-LAST-FORM TO F24.
           MOVE LTA-TRACK-ONLY TO F25.
           MOVE LTA-TRACK-MONTHS TO F251.
           MOVE LTA-INSURANCE TO F26.
           MOVE LTA-INS-MONTHS TO F261.
           MOVE SPACES         TO F27.
           MOVE LTA-CPP-MONTHS TO F271.

      ***********************
       UPDATE-LT SECTION.
      ***********************

           PERFORM OPEN-LTA1-FILE.

      * IF ORIGINAL IS NOT = WHAT YOU JUST BUILT
           IF LTA-OR-REC NOT = LTA-REC
              PERFORM WRITE-LTA1-FILE
              IF IO-FG NOT = 0
                 PERFORM REWRITE-LTA1-FILE.

           PERFORM CLOSE-LTA1-FILE.

      *****************************
       CREATE-CHANGE-REC SECTION.
      *****************************

           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-LM1-FILE.

           IF LTA-OR-DLR-REQ NOT = LTA-DLR-REQ
              PERFORM CREATE-SETUP-REC
              MOVE "DEALER REQUIRE." TO CH-DESCRIPT
              MOVE LTA-OR-DLR-REQ TO CH-ORG-CHAR
              MOVE LTA-DLR-REQ TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-INS-TYPE NOT = LTA-INS-TYPE
              PERFORM CREATE-SETUP-REC
              MOVE "INSURANCE TYPE" TO CH-DESCRIPT
              MOVE LTA-OR-INS-TYPE TO CH-ORG-CHAR
              MOVE LTA-INS-TYPE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-INS-CO NOT = LTA-INS-CO
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO INS COMPANY" TO CH-DESCRIPT
              MOVE LTA-OR-INS-CO TO CH-ORG-CHAR
              MOVE LTA-INS-CO TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-POLICY-NO NOT = LTA-POLICY-NO
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO POLICY NO." TO CH-DESCRIPT
              MOVE LTA-OR-POLICY-NO TO CH-ORG-CHAR
              MOVE LTA-POLICY-NO TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-AGENCY NOT = LTA-AGENCY
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO AGENCY NAME" TO CH-DESCRIPT
              MOVE LTA-OR-AGENCY TO CH-ORG-CHAR
              MOVE LTA-AGENCY TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-AGENT NOT = LTA-AGENT
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO AGENT NAME" TO CH-DESCRIPT
              MOVE LTA-OR-AGENT TO CH-ORG-CHAR
              MOVE LTA-AGENT TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-ADR1 NOT = LTA-ADR1
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO ADR 1" TO CH-DESCRIPT
              MOVE LTA-OR-ADR1 TO CH-ORG-CHAR
              MOVE LTA-ADR1 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-ADR2 NOT = LTA-ADR2
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO ADR 2" TO CH-DESCRIPT
              MOVE LTA-OR-ADR2 TO CH-ORG-CHAR
              MOVE LTA-ADR2 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-CITY NOT = LTA-CITY
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO CITY" TO CH-DESCRIPT
              MOVE LTA-OR-CITY TO CH-ORG-CHAR
              MOVE LTA-CITY TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-STATE NOT = LTA-STATE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO STATE" TO CH-DESCRIPT
              MOVE LTA-OR-STATE TO CH-ORG-CHAR
              MOVE LTA-STATE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-ZIP NOT = LTA-ZIP
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO ZIP" TO CH-DESCRIPT
              MOVE LTA-OR-ZIP TO CH-ORG-CHAR
              MOVE LTA-ZIP TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-PHONE NOT = LTA-PHONE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO PHONE" TO CH-DESCRIPT
              MOVE LTA-OR-PHONE TO CH-ORG-PHONE
              MOVE LTA-PHONE TO CH-NEW-PHONE
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-EFF-DATE NOT = LTA-EFF-DATE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO EFF DATE" TO CH-DESCRIPT
              MOVE LTA-OR-EFF-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-DATE
              MOVE LTA-EFF-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-EXP-DATE NOT = LTA-EXP-DATE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO EXP DATE" TO CH-DESCRIPT
              MOVE LTA-OR-EXP-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-DATE
              MOVE LTA-EXP-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-CAN-DATE NOT = LTA-CAN-DATE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO CAN DATE" TO CH-DESCRIPT
              MOVE LTA-OR-CAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-DATE
              MOVE LTA-CAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-CONF-DATE NOT = LTA-CONF-DATE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO CONF DATE" TO CH-DESCRIPT
              MOVE LTA-OR-CONF-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-DATE
              MOVE LTA-CONF-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.
           IF LTA-OR-LAST-FORM NOT = LTA-LAST-FORM
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO LAST FORM" TO CH-DESCRIPT
              MOVE LTA-OR-LAST-FORM TO CH-ORG-NUM
              MOVE LTA-LAST-FORM TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-COV-TYPE NOT = LTA-COV-TYPE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO COVR TYPE" TO CH-DESCRIPT
              MOVE LTA-OR-COV-TYPE TO CH-ORG-CHAR
              MOVE LTA-COV-TYPE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-BIL-COV NOT = LTA-BIL-COV
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO BODILY INJ LIMT" TO CH-DESCRIPT
              MOVE LTA-OR-BIL-COV TO CH-ORG-2DEC
              MOVE LTA-BIL-COV TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-PDL-COV NOT = LTA-PDL-COV
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO PROP DAMG LIMIT" TO CH-DESCRIPT
              MOVE LTA-OR-PDL-COV TO CH-ORG-2DEC
              MOVE LTA-PDL-COV TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-PIP-COV NOT = LTA-PIP-COV
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO PIP LIMIT" TO CH-DESCRIPT
              MOVE LTA-OR-PIP-COV TO CH-ORG-2DEC
              MOVE LTA-PIP-COV TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-UMB-COV NOT = LTA-UMB-COV
              PERFORM CREATE-SETUP-REC
              MOVE " F1-RESET, ENTER PROPERTY DAMAGE LIMIT:" TO MESS
              MOVE "AUTO UMBRLLA. LIMIT" TO CH-DESCRIPT
              MOVE LTA-OR-UMB-COV TO CH-ORG-2DEC
              MOVE LTA-UMB-COV TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-COL-COV NOT = LTA-COL-COV
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO COLLISN LIMIT" TO CH-DESCRIPT
              MOVE LTA-OR-COL-COV TO CH-ORG-2DEC
              MOVE LTA-COL-COV TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-LOSS-PAYEE NOT = LTA-LOSS-PAYEE
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO LOSS PAYEE" TO CH-DESCRIPT
              MOVE LTA-OR-LOSS-PAYEE TO CH-ORG-CHAR
              MOVE LTA-LOSS-PAYEE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTA-OR-COM-COV NOT = LTA-COM-COV
              PERFORM CREATE-SETUP-REC
              MOVE "AUTO COMPREHN. LIMIT" TO CH-DESCRIPT
              MOVE LTA-OR-COM-COV TO CH-ORG-2DEC
              MOVE LTA-COM-COV TO CH-NEW-2DEC
              PERFORM UPDATE-CH-LM-FILE.

           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-LM1-FILE.
           GO TO EXIT-CREATE-CHANGE-REC.

       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE EXT-CONAME-USER-LOC  TO CH-BRNO.
           MOVE SYSTEM-DATE-YYYYMMDD TO CH-DATE.
           MOVE "T"                  TO CH-CODE.
           MOVE EXT-CONAME-USER-ID   TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS             TO CH-TIME.
           MOVE 1                    TO CH-SEQ.

           MOVE LN-ACCTNO            TO CH-ACCTNO.
       EXIT-CREATE-CHANGE-REC.
           EXIT.


      *******************************
       UPDATE-CH-LM-FILE SECTION.
      *******************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO NOT = "Y"
              GO TO UPDATE-CH-LM-FILE-EXIT.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - SYSTEM-DATE-YYYYMMDD.
           MOVE 1             TO SAVE-LM-SEQ.
       UPDATE-CH-LM-FILE-AGAIN.
           MOVE SPACES TO LM-REC.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE CH-USERID TO MEMO-USERID.
           MOVE CH-DESCRIPT TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA TO LM-MEMO.

           MOVE "A" TO LM-MEMO-TYPE.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF SAVE-LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO UPDATE-CH-LM-FILE-AGAIN.

       UPDATE-CH-LM-FILE-EXIT.
           EXIT.



      *******************************
       GET-COLLATERAL-INFO SECTION.
      *******************************
           MOVE SPACES TO D-CAR D-VIN.

           PERFORM OPEN-LTC1-FILE.

           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-GET-COLLATERAL.

           MOVE SPACES TO CAR-WORK.
           MOVE LTC-YEAR(1) TO CAR-WORK.
           INSPECT CAR-WORK REPLACING FIRST "   " BY " % ".
           INSPECT CAR-WORK REPLACING FIRST "%              "
                     BY LTC-MAKE(1).
           INSPECT CAR-WORK REPLACING FIRST "   " BY " % ".
           INSPECT CAR-WORK REPLACING FIRST "%              "
                     BY LTC-MODEL(1).
           MOVE CAR-WORK TO D-CAR.
           MOVE LTC-VIN(1) TO D-VIN.

       EXIT-GET-COLLATERAL.
           PERFORM CLOSE-LTC1-FILE.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "AUTOIN" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLTACL.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY SPACES UPON AUTOIN-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY AUTOIN-SCREEN UPON AUTOIN-WINDOW-HANDLE.
           MOVE AUTOIN-FORM-FIELDS TO WR-BUF.
           MOVE AUTOIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE AUTOIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE AUTOIN-HELP--DIR  TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/AUTOIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
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
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTA1IN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY AUTOIN-SCREEN.
           DESTROY AUTOIN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
