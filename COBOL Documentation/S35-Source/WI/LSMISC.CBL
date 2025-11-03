       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LSMISC.
      **********************************************************
      *   DIR :  WI
      *   DESC: MISCELLANEIOUS FIELDS, LS TRAILER "M"
      *
      *         ONLY CALLED FROM CL/CLMENU.C F9
      *
      * REV:
      *  BAH 100617 ADDED LS "M" FOR 32 MISC FIELDS, UNITED PR# 3615
      *   CS 140618 CHANGED THE PROMPTS MISC 01 THRU MISC 06 TO FIELD.
      *             IF GP-I-AM-NORTHSTATE DISPLAY DIFFERENT PROMPTS IN
      *             FIRST 6 FIELDS   NORTHSTATE PR#4545 
      *  BAH 150219 CHANGE PROMPT 1 TO 1. TCPA FOR UNITED, PR# 4699
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *  BAH 170419 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181130 GLOBAL LMFILE
      *  BAH 20200318 DO NOT ALLOW CHANGES TO FIELD 15, USING THIS FOR
      *               STATE OF "IL" VERITEC ID, #1426
      *  BAH 20220321 ONLY USE FIRST 16 OCCURS OF LSM-MISC-FIELDS AND
      *               ADDED 3 NEW FIELDS TAKEN FROM THE FILLER
      *               LSM-TRANS-FROM-BRNO, LSM-TRANS-FROM-ACCTNO,
      *               AND LSM-TRANS-FROM-DATE FOR METRO2 "L1" SEGMENT #1547
      *  BAH 20220921 ADDED LSM-MISPAY-90-DATE FOR VERITEC UPDATE #1570
      *  BAH 20240109 ADDED SECOND BENEFICIARY FIELDS #1637
      *  BAH 20240410 ALLOW CHANGES TO SECOND BENEFICIARY FIELDS #1653
      *********************************************************************
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
           "@(#)WI/LSMISC.CBL S35 04/10/24-10:51:28 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  LSMISC-LINES          PIC 99  VALUE 20.
       01  LSMISC-COLUMNS        PIC 99  VALUE 78.
       01  LSMISC-BEGIN-Y        PIC 99  VALUE 2.
       01  LSMISC-BEGIN-X        PIC 99  VALUE 2.

       01  ERRCD             PIC X VALUE " ".
       01  LN-ACCTNO         PIC 9(8).

       01  SAVE-LM1-KEY                 VALUE ZEROES.
           03  SAVE-LM-BRNO            PIC 9(04).
           03  SAVE-LM-ACCTNO          PIC 9(08).
           03  SAVE-LM-DATE            PIC 9(08).
           03  SAVE-LM-SEQ             PIC 9(03).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-CHAR   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).

       01  LEGEND              PIC X(70) VALUE SPACES.
       01  LEGEND-01.
           03  FILLER       PIC X(53)  VALUE
           "F1-RESET, F2-MEMO, F7-EXIT AND UPDATE, ENTER FIELD #:".

       01  HOLD-SYS-DATE   PIC 9(8).

       01  ONE               PIC S9.
       01  SUB               PIC 99     VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  CHAN              PIC X(4).

       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LSM.CPY" REPLACING LEADING "LSM" BY "LSM-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LSM.CPY" REPLACING LEADING "LSM" BY "LSM-CH".


      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  FIELD-BUF     PIC X(20) OCCURS 16.
           03  D-T-BRNO      PIC 9(4).
           03  D-T-ACCT      PIC 9(8).
           03  D-T-DATE      PIC 99/99/99.
           03  D-MISPAY-DATE PIC 99/99/99.
           03  D-SECBEN-NAME     PIC X(32).
           03  D-SECBEN-ADR      PIC X(32).
           03  D-SECBEN-CITY     PIC X(16).
           03  D-SECBEN-STATE    PIC X(2).
           03  D-SECBEN-ZIP      PIC X(10).
           03  D-SECBEN-PHONE    PIC X(12).
           03  D-SECBEN-RELATION PIC X(15).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(20) VALUE
           "010 020 030 040 050 ".
           03  FILLER      PIC X(20) VALUE
           "060 070 080 090 100 ".
           03  FILLER      PIC X(44) VALUE
           "110 120 130 140 150 160 170 180 190 200 210 ".
           03  FILLER      PIC X(24) VALUE
           "220 230 231 232 240 250.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       78  MAX             VALUE 25.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 10. MISC 10:
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 17 LSM-TRANS-FROM-BRNO
           03  FILLER      PIC X(8) VALUE "EN N040N".
      * 18 LSM-TRANS-FROM-ACCTNO
           03  FILLER      PIC X(8) VALUE "EN N080N".
      * 19 LSM-TRANS-FROM-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 20 LSM-MISPAY-90-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 21 LSM-SECOND-BEN-NAME
           03  FILLER      PIC X(8) VALUE "E  A320 ".
      * 22 LSM-SECOND-BEN-ADR
           03  FILLER      PIC X(8) VALUE "E  A320 ".
      * 23 LSM-SECOND-BEN-CITY
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 24 LSM-SECOND-BEN-PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 25 LSM-SECOND-BEN-RELATION
           03  FILLER      PIC X(8) VALUE "E  A150 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  FIELD-15-SEQ               VALUE 01.
       78  FIELD-17-19-SEQ            VALUE 02.
       78  FIELD-20-SEQ                   VALUE 03.

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".

       01  LSMISC-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LSMISC_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/LSMISC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LSMISC_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LSMISC" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT  SCREEN LINE LSMISC-BEGIN-X * VDU-WIN-L-OFFSET
               AT  SCREEN COL  LSMISC-BEGIN-Y * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
            COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE    LSMISC-COLUMNS + VDU-WIN-W-MOD
               LINES   LSMISC-LINES + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS LSMISC-WINDOW-HANDLE.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM GET-LS-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *************************
       GET-LS-MODULE SECTION.
      *************************
           MOVE SPACES TO LSM-OR-REC.
           MOVE SPACES TO IO-TYPE.

      * WILL ALWAYS KEEP CURSER AT BOTTOM, NO NEW RECORD WALK THRU
           MOVE 0 TO NEW-FLAG.

           PERFORM OPEN-LM1-FILE.

           MOVE EXT-WI-CL-OWNBR  TO LSM-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSM-ACCTNO.
           MOVE 1                TO LSM-SEQNO.
           PERFORM OPEN-LSM1-FILE.
           PERFORM READ-LSM1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           IF IO-FG = 9
              PERFORM NEW-RECORD-SETUP.

           MOVE LSM-REC TO LSM-OR-REC.

           PERFORM DISPLAY-FIELDS.

       END-GET-LS-MODULE.
           EXIT.

      **************************
       NEW-RECORD-SETUP SECTION.
      **************************

           PERFORM CLEAR-LSM-FILE.

           MOVE EXT-WI-CL-OWNBR  TO LSM-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSM-ACCTNO.
           MOVE 1                TO LSM-SEQNO.

           MOVE 0 TO ELE.
           MOVE 1 TO NEW-FLAG.


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
              MOVE HOLD-SYS-DATE    TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1>"
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              GO TO MODIFY-ROUTINE.

           INSPECT ACTION REPLACING ALL " " BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

           IF ELE = 15
              MOVE "TO CHANGE FIELD 15" TO PASSWDW-PROMPT2
              MOVE FIELD-15-SEQ         TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

           IF ELE = 17 OR 18 OR 19
              MOVE "TO CHANGE THIS FIELD" TO PASSWDW-PROMPT2
              MOVE FIELD-17-19-SEQ        TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

           IF ELE = 20
              MOVE "TO CHANGE THIS FIELD" TO PASSWDW-PROMPT2
              MOVE FIELD-20-SEQ           TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE " F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F7.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
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

      * FIELD 15 IS LS-M-MISC-FIELD(15) VERITEC ID SET IN SP/ILMISC
      * FIELDS 17, 18 AND 19 ARE NEW HARD CODED FIELDS FOR METRO2
      * TRANSFERRED ACCOUNTS
      * FIELD 20 IS NEW FIELD FOR VERITEC MISPAY 90 DAYS DATE
      * FIELD 21 IS NEW FIELD FOR SECOND BENEFICIARY

           IF ELE > MAX
              GO TO EXIT-ENTRY-MODULE
           ELSE
           IF ELE > 16
              SUBTRACT 16 FROM ELE GIVING SUB
              GO TO F-17 F-18 F-19 F-20 F-21 F-22 F-23
                    F-24 F-25 DEPENDING ON SUB
           ELSE
              MOVE VDUBUF TO LSM-MISC-FIELD(ELE)
              GO TO CREATE-ROUTINE.

           GO TO EXIT-ENTRY-MODULE.

       F-17.
           MOVE VDUNUM0 TO LSM-TRANS-FROM-BRNO.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM0 TO LSM-TRANS-FROM-ACCTNO.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDU-DATE-YYYYMMDD TO LSM-TRANS-FROM-DATE.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDU-DATE-YYYYMMDD TO LSM-MISPAY-90-DATE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO LSM-SECOND-BEN-NAME.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUBUF TO LSM-SECOND-BEN-ADR.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUBUF TO LSM-SECOND-BEN-CITY.

       F-231.
           MOVE "R  A020231." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-231.
           MOVE VDUBUF TO LSM-SECOND-BEN-STATE.

       F-232.
           MOVE "R  A100232." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-231.
           IF NOT ENTER-KEY
              GO TO F-232.
           MOVE VDUBUF TO LSM-SECOND-BEN-ZIP.

           GO TO CREATE-ROUTINE.

       F-24.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.

           MOVE TEL-BUF TO LSM-SECOND-BEN-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120240." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-25.
           MOVE VDUBUF TO LSM-SECOND-BEN-RELATION.

           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.


      **********************************
       DISPLAY-FIELDS SECTION.
      **********************************

           MOVE " " TO DISPLAY-BUF.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
              MOVE LSM-MISC-FIELD(SUB) TO FIELD-BUF(SUB)
           END-PERFORM.

           MOVE LSM-TRANS-FROM-BRNO   TO D-T-BRNO.
           MOVE LSM-TRANS-FROM-ACCTNO TO D-T-ACCT.
           MOVE LSM-TRANS-FROM-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO D-T-DATE.
           MOVE LSM-MISPAY-90-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO D-MISPAY-DATE.
           MOVE LSM-SECOND-BEN-NAME   TO D-SECBEN-NAME.
           MOVE LSM-SECOND-BEN-ADR    TO D-SECBEN-ADR.
           MOVE LSM-SECOND-BEN-CITY   TO D-SECBEN-CITY.
           MOVE LSM-SECOND-BEN-STATE  TO D-SECBEN-STATE.
           MOVE LSM-SECOND-BEN-ZIP    TO D-SECBEN-ZIP.
           MOVE LSM-SECOND-BEN-PHONE  TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO D-SECBEN-PHONE.
           MOVE LSM-SECOND-BEN-RELATION TO D-SECBEN-RELATION.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      **************************
       WRITE-MODULE SECTION.
      **************************
       REWRITE-RECORD.

           PERFORM OPEN-LSM1-FILE.

           MOVE LSM-REC TO LSM-CH-REC.
           IF LSM-OR-REC = LSM-CH-REC
              GO TO END-WRITE-MODULE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           IF LSM-OR-REC = SPACES
              GO TO WRITE-RECORD.

           PERFORM READ-LSM1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LSM-REC NOT = LSM-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE LSM-CH-REC TO LSM-REC.
           PERFORM REWRITE-LSM1-FILE.

           PERFORM CREATE-CHANGE-RECORD.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LSM1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-LSM1-FILE.

      *****************************************
       CREATE-CHANGE-RECORD SECTION.
      *****************************************
           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-LM1-FILE.

           MOVE 1 TO SUB.
       CREATE-NEXT-CHANGE.
           IF LSM-OR-MISC-FIELD(SUB) NOT = LSM-MISC-FIELD(SUB)
              PERFORM CREATE-SETUP-CH-REC
              STRING "MISC FIELD ",SUB DELIMITED BY "  " INTO
                  CH-DESCRIPT
              MOVE LSM-OR-MISC-FIELD(SUB) TO CH-ORG-CHAR
              MOVE LSM-MISC-FIELD(SUB) TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF SUB < 16
              ADD 1 TO SUB
              GO TO CREATE-NEXT-CHANGE.

           IF LSM-OR-TRANS-FROM-BRNO NOT = LSM-TRANS-FROM-BRNO
              PERFORM CREATE-SETUP-CH-REC
              MOVE "TRANSFER BRNO" TO CH-DESCRIPT
              MOVE LSM-OR-TRANS-FROM-BRNO TO CH-ORG-CHAR
              MOVE LSM-TRANS-FROM-BRNO   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-TRANS-FROM-ACCTNO NOT = LSM-TRANS-FROM-ACCTNO
              PERFORM CREATE-SETUP-CH-REC
              MOVE "TRANSFER ACCTNO" TO CH-DESCRIPT
              MOVE LSM-OR-TRANS-FROM-ACCTNO TO CH-ORG-CHAR
              MOVE LSM-TRANS-FROM-ACCTNO   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-TRANS-FROM-DATE NOT = LSM-TRANS-FROM-DATE
              PERFORM CREATE-SETUP-CH-REC
              MOVE "TRANSFER DATE" TO CH-DESCRIPT
              MOVE LSM-OR-TRANS-FROM-DATE TO CH-ORG-CHAR
              MOVE LSM-TRANS-FROM-DATE    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-SECOND-BEN-NAME NOT = LSM-SECOND-BEN-NAME
              PERFORM CREATE-SETUP-CH-REC
              MOVE "SECBEN NAME" TO CH-DESCRIPT
              MOVE LSM-OR-SECOND-BEN-NAME TO CH-ORG-CHAR
              MOVE LSM-SECOND-BEN-NAME   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-SECOND-BEN-ADR NOT = LSM-SECOND-BEN-ADR
              PERFORM CREATE-SETUP-CH-REC
              MOVE "SECBEN ADR" TO CH-DESCRIPT
              MOVE LSM-OR-SECOND-BEN-ADR TO CH-ORG-CHAR
              MOVE LSM-SECOND-BEN-ADR   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-SECOND-BEN-CITY NOT = LSM-SECOND-BEN-CITY
              PERFORM CREATE-SETUP-CH-REC
              MOVE "SECBEN CITY" TO CH-DESCRIPT
              MOVE LSM-OR-SECOND-BEN-CITY TO CH-ORG-CHAR
              MOVE LSM-SECOND-BEN-CITY   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-SECOND-BEN-STATE NOT = LSM-SECOND-BEN-STATE
              PERFORM CREATE-SETUP-CH-REC
              MOVE "SECBEN STATE" TO CH-DESCRIPT
              MOVE LSM-OR-SECOND-BEN-STATE TO CH-ORG-CHAR
              MOVE LSM-SECOND-BEN-STATE   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-SECOND-BEN-ZIP NOT = LSM-SECOND-BEN-ZIP
              PERFORM CREATE-SETUP-CH-REC
              MOVE "SECBEN ZIP" TO CH-DESCRIPT
              MOVE LSM-OR-SECOND-BEN-ZIP TO CH-ORG-CHAR
              MOVE LSM-SECOND-BEN-ZIP   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-SECOND-BEN-PHONE NOT = LSM-SECOND-BEN-PHONE
              PERFORM CREATE-SETUP-CH-REC
              MOVE "SECBEN PHONE" TO CH-DESCRIPT
              MOVE LSM-OR-SECOND-BEN-PHONE TO CH-ORG-CHAR
              MOVE LSM-SECOND-BEN-PHONE   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LSM-OR-SECOND-BEN-RELATION NOT = LSM-SECOND-BEN-RELATION
              PERFORM CREATE-SETUP-CH-REC
              MOVE "SECBEN RELATION" TO CH-DESCRIPT
              MOVE LSM-OR-SECOND-BEN-RELATION TO CH-ORG-CHAR
              MOVE LSM-SECOND-BEN-RELATION   TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.


           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CH-FILE.


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

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - HOLD-SYS-DATE.
           MOVE 1             TO SAVE-LM-SEQ.
       UPDATE-CH-LM-FILE-AGAIN.
           MOVE SPACES TO LM-REC.

           MOVE SAVE-LM1-KEY     TO LM1-KEY.
           MOVE CH-USERID        TO MEMO-USERID.
           MOVE CH-DESCRIPT      TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR      TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA        TO LM-MEMO.
           MOVE "9"              TO LM-MEMO-TYPE.

           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF SAVE-LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO UPDATE-CH-LM-FILE-AGAIN.

       UPDATE-CH-LM-FILE-EXIT.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "LSMISC"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON LSMISC-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY LSMISC-SCREEN UPON LSMISC-WINDOW-HANDLE.
           MOVE LSMISC-FORM-FIELDS TO WR-BUF.
           MOVE LSMISC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE LSMISC-HELP--FILE TO HELP-LINK-FILE.
           MOVE LSMISC-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LSMISC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       CREATE-SETUP-CH-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE EXT-CONAME-USER-LOC TO CH-BRNO.
           MOVE HOLD-SYS-DATE       TO CH-DATE.
           MOVE "Z"                 TO CH-CODE.
           MOVE EXT-CONAME-USER-ID  TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS            TO CH-TIME.
           MOVE 1                   TO CH-SEQ.

           MOVE LN-ACCTNO           TO CH-ACCTNO.
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
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


       COPY "LIBLP/LPLSMCL.CPY".
       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **********************************
       IO-ROUTINES SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LSMISC-SCREEN.
           DESTROY LSMISC-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
