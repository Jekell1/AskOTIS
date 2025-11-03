       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       BBINQ2.
       AUTHOR.              MJD.
       DATE-WRITTEN. 11/15/1999.
      *********************************************************************
      *   DIR :  WI
      *   DESC: BULLETIN BOARD MEMO ENTRY
      *
      *   NOTE: THIS PROGRAM USES FORM BBINQ3
      *
      * REV:
      *   MJD 000310 CHANGED TO NOT DISPLAY FIELDS UNTIL AFTER ALL PASSWORDS,
      *              ETC. HAVE BEEN ENTERRED.
      *   BLV 010417 CHANGED TO ALLOW ENTRY OF GROUP ID INSTEAD OF BR #,
      *              IF SUPERVISOR IS IN HOME BASE, THEN USE GROUP NAME &
      *              DISPLAY GROUP PHONE#, WORLD PR# 259
      *              NOTE: WHEN I MADE THIS CHANGE, I USED BB-GROUP WHICH
      *              APPEARED TO BE USED ABSOLUTELY NO WHERE, EXCEPT THIS
      *              PROGRAM MOVE LOGIN-GROUP TO BB-GROUP & I STARRED OFF.
      *   BLV 010417 ADDED WRITE OF BHFILE TO TRACK BULLETIN BOARD HISTORY
      *   BLV 010719 FIXED WRITE OF BHFILE TO BE MOST RECENT HISTORY FIRST,
      *              WORLD PR# 272
      *   BLF 050818 CHANGED TO CREATE HISTORY (BHFILE) EACH TIME USER
      *              COMES IN THIS PROGRAM TO EDIT, EVEN IF NOTHING
      *              CHANGED, WORLD PR# 427
      *   BAH 170406 ACTIVATE 8 DIGIT DATES, PD0006
      *   BAH 190612 PASSWORD ENTRY WAS NOT WORKING DUE TO RRFORM BEING
      *              CHANGED TO SCREENIO BUT NOT MOVING VDUBUF TO
      *              BBPASSWD-BUF, QA0035
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/BBINQ2 S35 04/22/24-10:05:44 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBSP/SP01BB.CPY".
       COPY "LIBSP/SP01BB_SQL.CPY".
       COPY "LIBSP/SPWSBB.CPY".
       COPY "LIBSP/SP01BH.CPY".
       COPY "LIBSP/SP01BH_SQL.CPY".
       COPY "LIBSP/SPWSBH.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  ERRCD             PIC X     VALUE SPACES.
       01  ONE               PIC S9.
       01  SUB               PIC 99.
       01  MEMO-SUB          PIC 99.
       01  ELE               PIC 99     VALUE 0.

       01  BBINQ2-LINES          PIC 99    VALUE 22.
       01  BBINQ2-COLUMNS        PIC 99    VALUE 78.
       01  BBINQ2-BEGIN-Y        PIC 99    VALUE 1.
       01  BBINQ2-BEGIN-X        PIC 99    VALUE 2.

       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).

       01  CASE-BUF          PIC X(20).
       01  LOGIN-PASSWORD    PIC X(10).
       01  LOGIN-NAME        PIC X(20).
       01  LOGIN-LOGIN.
           03  LOGIN-CLASS   PIC X(01).
               88 LOGIN-CLASS-VALID        VALUE "A" THRU "Z".
           03  LOGIN-BRANCH  PIC 9(04).
           03  LOGIN-GROUP   PIC X(04).

       01  ENV-LOGNAME       PIC X(10).
       01  ENV-POSTNAME      PIC X(10).
       01  ENT-GROUP         PIC X(4).
       01  GROUP-FLAG        PIC X      VALUE "N".

       01  HOLD-BB-FIELDS.
           03  HOLD-BB-ACTION  PIC X(40).
           03  HOLD-BB-GROUP   PIC X(4).
           03  HOLD-BB-BRNO REDEFINES HOLD-BB-GROUP
                               PIC 9(4).

      *-----------------------------------------------------------------
      * 78 LEVEL CONSTANTS
      *-----------------------------------------------------------------
       78  NEW-ERR-MESG VALUE "MUST USE PERSONAL LOGIN TO CREATE MEMOS".

       78  TITLE-LIST            VALUE "TITLE.".
       78  TITLE-VIEW            VALUE "      MORE INFORMATION     ".
       78  TITLE-EDIT            VALUE "         EDIT MODE         ".
       78  TITLE-ADMIN           VALUE "    ADMINISTRATION MODE    ".
       78  AM-CMD                VALUE X"616D".
       78  PM-CMD                VALUE X"706D".
       78  BBPASSWD-MLIST        VALUE "NEW.".
       78  MODE-LIST             VALUE "MODE.".
       78  MAX                   VALUE 12.
       78  MAX3                  VALUE 12.

      *-----------------------------------------------------------------
      *PASSWORD WORKERS
      *-----------------------------------------------------------------
       01  BBPASSWD-MBUF      PIC X(24).

      *-----------------------------------------------------------------
      * TITLE AND FIELD LEGEND WORKERS
      *-----------------------------------------------------------------
       01  TITLE-BUF             PIC X(30)    VALUE SPACES.
       01  LEGEND                PIC X(63)    VALUE SPACES.
       01  LEGEND-ADMIN.
           03  FILLER            PIC X(12)    VALUE
               "F1-EXIT,F3-P".
           03  FILLER            PIC X(51)    VALUE
               "ASSWD,F4/F5-NAME,F6-HIST, F7-UPD/UPD, DELETE, FLD#:".
       01  LEGEND-EDIT.
           03  FILLER            PIC X(12)    VALUE "F1-EXIT, F3-".
           03  FILLER            PIC X(51)    VALUE
               "PASSWD, F6-HISTORY, F7-EXIT W/UPDATE, ENTER FIELD#:".
       01  LEGEND-VIEW.
           03  FILLER            PIC X(12)    VALUE SPACES.
           03  FILLER            PIC X(51)    VALUE
               "                                F6-HISTORY,F7-EXIT:".
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  SAVED-REC         PIC X(BB-SIZE).
       01  SET-CHFG          PIC X(01).

      *-----------------------------------------------------------------
      * SCREEN ACTION. THIS PROGRAM CAN BE OPERATED IN ONE OF 3 MODES:
      *
      *    ADMINISTRATION:  ("A" AND "N")
      *            - ALLOW FULL USE OF CREATE RECORD
      *                                EDIT RECORD
      *                                DELETE RECORD
      *                                CHANGE PASSWORD
      *                                UPDATE NAME
      *    EDIT MODE     : ("O")
      *            - ALLOW USER USE OF EDIT RECORD
      *                                CHANGE PASSWORD
      *    VIEW MODE     : ("V")
      *            - USER HAS NO OPTIONS EXCEPT TO EXIT SCREEN
      *-----------------------------------------------------------------
       01  WINDOW-ACTION     PIC X(01).
           88 ACTION-VIEW                 VALUE "V".
           88 ACTION-EDIT                 VALUE "O".
           88 ACTION-ADMIN                VALUE "A" "N".
           88 ACTION-ADMIN-NEW            VALUE "N".
           88 ACTION-ADMIN-EDIT           VALUE "A".

       01  WORK-TIME.
           03  WORK-POSTTIME     PIC 9(6).

      *-----------------------------------------------------------------
      * SCREEN BUFFERS
      *-----------------------------------------------------------------
       01  NO-NUMBER-BUF     PIC X(36) VALUE SPACES.
       01  NUMBER-BUF.
           03  FILLER        PIC X(36)  VALUE
               " 1: 2: 3: 4: 5: 6: 7: 8: 9:10:11:12:".
       01  NUMBER-LIST.
           03  FILLER        PIC X(36)    VALUE
               "N01:P N02:P N03:P N04:P N05:P N06:P ".
           03  FILLER        PIC X(36)    VALUE
               "N07:P N08:P N09:P N10:P N11:P N12:P.".


      *-----------------------------------------------------------------
      * DISPLAY PROPERTIES FOR WORLDS FORM
      *-----------------------------------------------------------------
       01  DISPLAY-BUF3       VALUE SPACES.
           03  D3-USER        PIC X(10).
           03  D3-NAME        PIC X(35).
           03  D3-POSTPROMPT  PIC X(13).
           03  D3-POSTDATEX.
               05 D3-POSTDATE PIC 99/99/99 BLANK ZERO.
           03  D3-POSTTIME    PIC X(08).
           03  G01           PIC Z(4).
           03  G01-ALP REDEFINES G01
                             PIC X(4).
           03  G011          PIC X(30).
           03  G02           PIC X(40).
           03  G03-10        PIC X(50) OCCURS 8.
           03  G11-12        PIC X(60) OCCURS 2.

       01  DISPLAY-LIST3.
           03  FILLER      PIC X(42) VALUE
           "USER NAME UPDPROMPT:P UPDDATE:R UPDTIME:R ".
           03  FILLER      PIC X(44) VALUE
           "010 011 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(08) VALUE
           "110 120.".

      *-----------------------------------------------------------------
      * ELEMENT SPECIFICATIONS
      *-----------------------------------------------------------------
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "E  A040 ".
           03  FILLER      PIC X(8) VALUE "E  A030 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  FILLER  PIC X(7).
               05  CHFG    PIC X.

      *-----------------------------------------------------------------
      * ELEMENT SPECIFICATIONS - WORLD
      *-----------------------------------------------------------------
       01  SPEC-TABLE3.
           03  FILLER      PIC X(8) VALUE "E  A040 ".
           03  FILLER      PIC X(8) VALUE "E  A030 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
           03  FILLER      PIC X(8) VALUE "E  A060 ".
       01  SPEC-TAB3 REDEFINES SPEC-TABLE3.
           03  SPEC-REC3 OCCURS MAX3 TIMES.
               05  SPEC3 PIC X(7).
               05  CHFG3 PIC X.

      ***********************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/BBPASSWDW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  DELETE-SEQ                    VALUE 0.
       01  PASSWORD-STAT      PIC X(01).
           88  PASSWORD-INVALID           VALUE "X".

       COPY "LIBWI/BHSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".

       01  BBINQ2-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BBINQ3_DEF.CPY".
       COPY "LIBWI/BBPASW_DEF.CPY".
       COPY "LIBWI/BBINQ3_WKS.CPY".
       COPY "LIBWI/BBPASW_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/BBINQ2W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BBINQ3_SCN.CPY".
       COPY "LIBWI/BBPASW_SCN.CPY".

      ******************************************************************
      * PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME BBINQ2-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "BBINQ3"  TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE BBINQ2-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  BBINQ2-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    BBINQ2-COLUMNS + VDU-WIN-W-MOD
                   LINES   BBINQ2-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS BBINQ2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE BBINQ2-ACTION TO WINDOW-ACTION.
           MOVE BBINQ2-LOGIN  TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 20.
           MOVE CASE-BUF TO LOGIN-LOGIN.

      * WHEN IN VIEW MODE DO NOT DISPLAY NUMBERS NEXT TO FIELDS

           IF ACTION-VIEW
              MOVE NO-NUMBER-BUF TO WR-BUF
           ELSE
              MOVE NUMBER-BUF TO WR-BUF.
           MOVE NUMBER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * GET USER INFORMATION FROM ENVIRONMENT
      
           ACCEPT ENV-LOGNAME  FROM ENVIRONMENT "LOGNAME".
           ACCEPT ENV-POSTNAME FROM ENVIRONMENT "POSTNAME".

       MAIN-MODULE.

      * DISPLAY APPROPRIATE TITLE FOR MODE OF OPERATION
      
           IF ACTION-VIEW
              MOVE TITLE-VIEW  TO TITLE-BUF
           ELSE
              IF ACTION-EDIT
                 MOVE TITLE-EDIT  TO TITLE-BUF
              ELSE
                 MOVE TITLE-ADMIN TO TITLE-BUF.
           MOVE TITLE-BUF TO WR-BUF.
           MOVE TITLE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACE TO IO-TYPE.
           PERFORM OPEN-GR1-FILE.

      * IF NOT CREATING A NEW RECORD THEN VERIFY GROUP
      
           IF NOT ACTION-ADMIN-NEW
              MOVE LOGIN-GROUP TO CASE-BUF
              CALL "C$TOUPPER" USING CASE-BUF, VALUE 20
              MOVE CASE-BUF TO GR-ID
              PERFORM GET-GR
              IF IO-BAD
                 MOVE " INVALID GROUP ID" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

      * CONFIRM WHETHER OR NOT RECORD EXISTS
      
           PERFORM CLEAR-BB-REC.

           MOVE BBINQ2-BB-KEY TO BB1-KEY.
           PERFORM OPEN-BB1-FILE.
           PERFORM READ-BB1-FILE.
           PERFORM CLOSE-BB1-FILE.

      * NO RECORD EXISTS - CREATE NEW ONE
      
           IF IO-BAD
              IF NOT ACTION-ADMIN-NEW
                 MOVE " CANNOT FIND RECORD" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE
              END-IF
      * PERFORM PASSWORD ENTRY FOR LOGIN ID ONLY
              MOVE "N" TO BBPASSWD-STATUS
              MOVE "Y" TO BBPASSWD-LOGIN-ONLY
              PERFORM PASSWORD-ENTRY
              IF PASSWORD-INVALID
                 GO TO END-ROUTINE
              END-IF
              PERFORM CLEAR-BB-REC
              MOVE "MGR"          TO BB-CATAGORY
              MOVE "GRP"          TO BB-SUBCATAGORY
              MOVE LOGIN-LOGIN    TO CASE-BUF
              CALL "C$TOLOWER" USING CASE-BUF, VALUE 20
              MOVE CASE-BUF       TO BB-USERID BBINQ2-LOGIN
              MOVE 1              TO BB-SEQNO
      * TEST WHETHER ENTERRED LOGIN EXISTS
              PERFORM OPEN-BB1-FILE
              PERFORM READ-BB1-FILE
              PERFORM CLOSE-BB1-FILE
      * IF RECORDS EXISTS FOR LOGIN THEN SKIP PASSWORD ENTRY
      *  AND DISPLAY EXISTING RECORD
              IF IO-GOOD
                 MOVE "O" TO BBPASSWD-STATUS
                 MOVE "A" TO WINDOW-ACTION
                 GO TO CONTINUE-SKIP-PW
              END-IF
      * IF NO RECORD EXISTS FOR LOGIN PROMPT FOR NEW PASSWORD
      *    AND SETUP NEW RECORD
              PERFORM PASSWORD-ENTRY
              MOVE "MGR"          TO BB-CATAGORY
              MOVE "GRP"          TO BB-SUBCATAGORY
              MOVE BBINQ2-LOGIN   TO BB-USERID
              MOVE 1              TO BB-SEQNO
              MOVE LOGIN-PASSWORD TO BB-PASSWORD
              MOVE LOGIN-NAME     TO BB-NAME
              MOVE 1 TO NEW-FLAG
              MOVE 0 TO ELE
           ELSE
              MOVE "O" TO BBPASSWD-STATUS.

      * IF EDITING AN EXISTING RECORD THEN FORCE PASSWORD ANTRY

           IF ACTION-EDIT
              IF ENV-LOGNAME NOT = BB-USERID
                 PERFORM PASSWORD-ENTRY
                 IF PASSWORD-INVALID
                    GO TO END-ROUTINE.

       CONTINUE-SKIP-PW.

      * SET BYTE IN FORM SO THAT I CAN DISPLAY DIFFERENT HELP
      *  OPTIONS FOR ADMINISTRATOR/EDIT/VIEW MODES
      
           PERFORM DISPLAY-FIELDS.
           IF ACTION-EDIT
              MOVE BB-ACTION TO HOLD-BB-ACTION
              IF BB-BRNO = 0
                 MOVE BB-GROUP TO HOLD-BB-GROUP
              ELSE
                 MOVE BB-BRNO TO HOLD-BB-BRNO.

           IF ACTION-ADMIN
              MOVE "A" TO WR-BUF
           ELSE
              IF ACTION-EDIT
                 MOVE "E" TO WR-BUF
           ELSE
              MOVE " " TO WR-BUF.
           MOVE MODE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO END-ROUTINE.

      * IF I AM NOT IN VIEW MODE, SAVE RECORD UPON EXIT
      
           IF IO-TYPE = "END"
              IF NOT ACTION-VIEW
                 PERFORM SAVE-MEMO
              END-IF
              GO TO END-ROUTINE.

      * TEST PASSWORD ON DELETE OF RECORD
      
           IF IO-TYPE = "DEL"
              IF ACTION-ADMIN
                 MOVE DELETE-SEQ TO PASSWDW-SEQ
                 MOVE "TO DELETE BULETIN BOARD ENTRY" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT = "OK"
                    PERFORM DELETE-MODULE
                 END-IF
              END-IF
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
      * ENTRY-MODULE
      *  NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *             1 - INITIAL NEW RECORD ENTRY
      *             2 - MODIFY NEW RECORD
      ******************************************************************
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
           IF ACTION-VIEW
              MOVE "N" TO SET-CHFG
              PERFORM PROTECT-FIELDS
              MOVE LEGEND-VIEW TO LEGEND
           ELSE
              MOVE " " TO SET-CHFG
              PERFORM PROTECT-FIELDS
              IF ACTION-ADMIN
                 MOVE LEGEND-ADMIN TO LEGEND
              ELSE
                 MOVE LEGEND-EDIT  TO LEGEND
              END-IF.

           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

      *    -------------------------------------------------------------
      *    FUNCTION KEYS SERVE DIFFERENT FUNCTIONS FOR
      *    DIFFERENT MODES OF OPERATION
      *       ADMINISTRATION MODE:  <F1> = EXIT WITHOUT UPDATE
      *                             <F3> = CHANGE PASSWORD
      *                             <F4> = UPDATE NAME
      *                             <F7> = UPDATE AND EXIT
      *                             "DELETE" = DELETE RECORD
      *       EDIT MODE          :  <F1> = EXIT WITHOUT UPDATE
      *                             <F3> = CHANGE PASSWORD
      *                             <F7> = UPDATE AND EXIT
      *       VIEW MODE          :  <F7> = EXIT WITHOUT UPDATE
      *    -------------------------------------------------------------
           IF NOT ACTION-VIEW
              IF F1-KEY
                 MOVE "CLR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
           IF ACTION-ADMIN OR ACTION-EDIT
              IF F3-KEY
                 PERFORM CHANGE-PASSWORD
                 GO TO MODIFY-ROUTINE.
           IF ACTION-ADMIN
              IF F4-KEY
                 PERFORM CHANGE-NAME-GROUP
                 GO TO MODIFY-ROUTINE.
           IF ACTION-ADMIN
              IF F5-KEY
                 PERFORM CHANGE-NAME-FREE
                 GO TO MODIFY-ROUTINE.
           IF F7-KEY
              IF ACTION-VIEW
                 MOVE "CLR" TO IO-TYPE
              ELSE
                 MOVE "END" TO IO-TYPE
              END-IF
              GO TO EXIT-ENTRY-MODULE.
           IF F6-KEY
      *       IF ACTION-VIEW
                 MOVE BB-USERID        TO BHSCAN-USERID
                 MOVE "WI/BHSCAN" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH BHSCAN-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = " "
              GO TO MODIFY-ROUTINE.
           IF ACTION-ADMIN
              IF ACTION = "DELETE"
                 MOVE "DEL" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL " " BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
      *    -------------------------------------------------------------
      *    DISPLAY LEGEND FOR EACH FIELD
      *    -------------------------------------------------------------
           EVALUATE ELE
            WHEN 1 MOVE "F1-EXIT, F7-UPDATE, ENTER BRANCH NUMBER"
                                                           TO  LEGEND
            WHEN 2 MOVE "F1-EXIT, F7-UPDATE, ACTION             "
                                                           TO  LEGEND
            WHEN OTHER MOVE "F1-EXIT, F7-UPDATE, ENTER MEMO LINE"
                                                             TO  LEGEND
           END-EVALUATE.
           PERFORM SEND-LEGEND.

           IF CHKEY NOT = ZEROS
              IF CHFG3(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG3(ELE) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC3(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF UP-KEY
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF F7-KEY
              IF NEW-FLAG = 1
                 MOVE 2 TO NEW-FLAG
                 GO TO CREATE-ROUTINE.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-03 A-03 A-03
                 A-03 A-03 A-03 A-03 A-03 A-03
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      ******************************************************************
      * SET RECORD ELEMENTS
      ******************************************************************
       A-01.
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF WGR-TYPE = "A"
              MOVE "Y" TO GROUP-FLAG
              MOVE "S  A030011." TO VDU
              MOVE GR-NAME TO VDUBUF
              PERFORM SCREENIO
              MOVE ENT-GROUP TO BB-GROUP
              MOVE 0 TO BB-BRNO
              GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BB-BRNO BR-NO.
           MOVE SPACES TO BB-GROUP.
           MOVE "SN N040010." TO VDU.
           MOVE BB-BRNO       TO VDUNUM0.
           PERFORM SCREENIO.

           IF BR-NO = 0
              MOVE SPEC3(ELE) TO VDU
              MOVE "S  A" TO VDU(1:4)
              MOVE ELE TO VDUROW
              MOVE 0 TO VDUCOL
              MOVE "." TO VDUDECIMAL
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE SPACES TO BR-NAME
           ELSE
              PERFORM GET-BR
              IF IO-BAD
                 MOVE "INVALID BRANCH" TO MESS
                       PERFORM SEND-MESS
                       GO TO ENTER-ELE.

           MOVE "S  A030011." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO BB-ACTION.
           GO TO CREATE-ROUTINE.
       A-03.
           SUBTRACT 2 FROM ELE GIVING MEMO-SUB.
           MOVE VDUBUF TO BB-MEMO-LINE(MEMO-SUB).
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF3.

           MOVE BBINQ2-LOGIN TO D3-USER.
           MOVE BB-NAME TO D3-NAME.

           IF BB-BRNO = 0
              MOVE BB-GROUP TO GR-ID G01-ALP
              PERFORM GET-GR
              IF IO-BAD
                 MOVE SPACES TO GR-NAME
              END-IF
              MOVE GR-NAME   TO G011
           ELSE
              MOVE BB-BRNO   TO G01 BR-NO
              PERFORM GET-BR
              MOVE BR-NAME   TO G011.
           MOVE BB-ACTION TO G02.
           PERFORM VARYING MEMO-SUB FROM 1 BY 1 UNTIL MEMO-SUB > 8
              MOVE BB-MEMO-LINE(MEMO-SUB) TO G03-10(MEMO-SUB)
           END-PERFORM.
           MOVE BB-MEMO-LINE(9)  TO G11-12(1).
           MOVE BB-MEMO-LINE(10) TO G11-12(2).

           MOVE "LAST UPDATED:" TO D3-POSTPROMPT.
           IF BB-POSTDATE = 0 AND BB-POSTTIME = 0
              MOVE "  NEW" TO D3-POSTDATEX
              MOVE " RECORD" TO D3-POSTTIME
           ELSE
              MOVE BB-POSTDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D3-POSTDATE
              MOVE BB-POSTTIME TO WORK-POSTTIME
              PERFORM CONVERT-TIME
              MOVE TIME-EDIT TO D3-POSTTIME.
           MOVE DISPLAY-BUF3 TO WR-BUF.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       SAVE-MEMO SECTION.
      ******************************************************************
           PERFORM GET-USER-INFO.
           MOVE BB-REC TO SAVED-REC.
           PERFORM OPEN-BB1-FILE.
           PERFORM READ-BB1-FILE.
           IF IO-BAD
              MOVE SAVED-REC TO BB-REC
              PERFORM WRITE-BB1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              END-IF
              GO TO SAVE-MEMO-EXIT.

           MOVE SAVED-REC TO BB-REC.
           PERFORM REWRITE-BB1-FILE.
           PERFORM CLOSE-BB1-FILE.
           IF NOT ACTION-EDIT
              GO TO SAVE-MEMO-EXIT.

           PERFORM CREATE-BB-HISTORY.

       SAVE-MEMO-EXIT.
           EXIT.

      ******************************************************************
       CREATE-BB-HISTORY SECTION.
      ******************************************************************
           PERFORM OPEN-BH1-FILE.
           MOVE BB-USERID TO BH-USERID.
           COMPUTE BH-DATE = 99999999 - BB-POSTDATE.
           MOVE BB-POSTTIME TO HH-MM-SS.
           MOVE H-AND-M TO BH-TIME.
           IF BB-BRNO = 0
              MOVE BB-GROUP TO BH-GROUP
           ELSE
              MOVE BB-BRNO TO BH-BRNO.
           MOVE BB-ACTION TO BH-ACTION.

           PERFORM READ-BH1-FILE.
           IF IO-BAD
              PERFORM WRITE-BH1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              END-IF
              GO TO CREATE-HISTORY-EXIT.

           MOVE BB-ACTION TO BH-ACTION.
           PERFORM REWRITE-BH1-FILE.
       CREATE-HISTORY-EXIT.
           PERFORM CLOSE-BH1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-BB1-FILE.
           PERFORM READ-BB1-FILE.
           IF IO-FG NOT = 0
              MOVE "RECORD NOT FOUND : DELETE" TO MESS
              PERFORM SEND-MESS
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-BB1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-BB1-FILE.

      ******************************************************************
       GET-USER-INFO SECTION.
      ******************************************************************
           PERFORM SET-POSTDATE.
           MOVE POST-TIME   TO BB-POSTTIME.
           MOVE POST-DATE   TO BB-POSTDATE.
           ACCEPT BB-LOGNAME  FROM ENVIRONMENT "LOGNAME".
           ACCEPT BB-POSTNAME FROM ENVIRONMENT "POSTNAME".
           ACCEPT BB-OTHERBR  FROM ENVIRONMENT "OTHERBR".
       GET-USER-INFO-EXIT.
           EXIT.

      ******************************************************************
      * VERIFY USERS PASSWORD SECTION
      *  BBPASSWD-STATUS:  "N" NEW RECORD ENTRY
      *                    "O" EXISTING RECORD
      *                    "M" CHANGE PASSWORD (WORK???)
      *                    "X" UNCONDITIONAL CHANGE (DO NOT TEST OLD)
      ******************************************************************
       PASSWORD-ENTRY SECTION.
       PASSWORD-ENTRY-LOOP.

      * DISPLAY LOGIN WINDOW
           MOVE 9 TO WINDOW-LINES.
           MOVE 40 TO WINDOW-COLUMNS.
           MOVE 10 TO WINDOW-BEGIN-Y.
           MOVE 10 TO WINDOW-BEGIN-X.
           MOVE "BBPASW" TO VDU-FORM-NAME.
           MOVE "WI/BBPASW" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY BBPASW-SCREEN.

           MOVE BBPASW-FORM-FIELDS TO WR-BUF.
           MOVE BBPASW-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       PW-ENTER-LOGIN.
           IF BBINQ2-LOGIN NOT = SPACES
              MOVE "S  A010LOGIN." TO VDU
              MOVE BBINQ2-LOGIN TO VDUBUF
              PERFORM SCREENIO
              GO TO PW-ENTER-PASSWORD.

           MOVE "ENTER LOGIN ID" TO BBPASSWD-MBUF.
           MOVE BBPASSWD-MBUF TO WR-BUF.
           MOVE BBPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010LOGIN." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO PW-ERROR-OUT.

           MOVE VDUBUF TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 20.
           MOVE CASE-BUF TO LOGIN-LOGIN.

      * TEST VALID USER CLASS  A________ THRU Z_________

           IF NOT LOGIN-CLASS-VALID
              MOVE " INVALID BRANCH LOGIN" TO MESS
              PERFORM SEND-MESS
              GO TO PW-ENTER-LOGIN.

      * TEST VALID USER BRANCH  _????____

           MOVE LOGIN-BRANCH TO BR-NO.
           PERFORM GET-BR.
           IF IO-BAD
              MOVE "INVALID BRANCH LOGIN" TO MESS
              PERFORM SEND-MESS
              GO TO PW-ENTER-LOGIN.

      * TEST VALID USER GROUP  _____????

           MOVE LOGIN-GROUP TO GR-ID.
           PERFORM GET-GR.
           IF IO-BAD
              MOVE "INVALID GROUP ID" TO MESS
              PERFORM SEND-MESS
              GO TO PW-ENTER-LOGIN.
           MOVE GR-CONTACT TO LOGIN-NAME.

           IF BBPASSWD-LOGIN-ONLY = "Y"
              MOVE " " TO BBPASSWD-LOGIN-ONLY
              GO TO PASSWORD-ENTRY-EXIT.

      * 1ST PASS AT PASSWORD ENTRY

       PW-ENTER-PASSWORD.
           IF BBPASSWD-STATUS = "X"
              MOVE "ENTER NEW PASSWORD" TO BBPASSWD-MBUF
           ELSE
           IF BBPASSWD-STATUS = "N" OR "O"
              MOVE "ENTER PASSWORD" TO BBPASSWD-MBUF
           ELSE
              MOVE "ENTER OLD PASSWORD" TO BBPASSWD-MBUF.
           MOVE BBPASSWD-MBUF TO WR-BUF.
           MOVE BBPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO BBPASSWD-BUF.
           MOVE BBPASSWD-MBUF TO WR-BUF.
           MOVE BBPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A080BBPASSWD:R." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO PW-ERROR-OUT.
           IF VDUSTATUS NOT = "00"
              GO TO PW-ENTER-PASSWORD.

           MOVE VDUBUF TO BBPASSWD-BUF.

           IF BBPASSWD-STATUS = "N" OR "X"
              IF BBPASSWD-BUF = SPACES
                 MOVE "PASSWORD REQUIRED, PLEASE TRY AGAIN" TO MESS
                 PERFORM SEND-MESS
                 GO TO PW-ENTER-PASSWORD
              END-IF
              GO TO PW-VERIFY-UNIQUE-PASS
           ELSE
              GO TO PW-TEST-BB-PASS.

      * TEST PASSWORD IS VALID

       PW-VERIFY-UNIQUE-PASS.
           MOVE BBINQ2-BB-KEY TO BB1-KEY.
           PERFORM OPEN-BB1-FILE.
           PERFORM READ-BB1-FILE.
           PERFORM CLOSE-BB1-FILE.

      * IF "X" THEN UNCONDITIONALLY FORCE NEW PASSWORD ENTRY
           IF IO-GOOD AND BBPASSWD-STATUS = "X"
              MOVE BBPASSWD-BUF TO BB-PASSWORD.

           IF IO-BAD OR (IO-GOOD AND BB-PASSWORD = BBPASSWD-BUF)
              GO TO PW-ENTER-PASSWORD-PASS2.

           MOVE "N" TO BBPASSWD-OKAY.

           GO TO PW-ERROR-OUT.

      * REENTER PASSWORD FOR VALIDATION

       PW-ENTER-PASSWORD-PASS2.
           MOVE BBPASSWD-BUF TO BB-PASSWORD LOGIN-PASSWORD.
           IF BBPASSWD-STATUS = "M"
              MOVE "RE-ENTER OLD PASSWORD" TO  BBPASSWD-MBUF
           ELSE
              MOVE "RE-ENTER NEW PASSWORD" TO  BBPASSWD-MBUF.
           MOVE BBPASSWD-MBUF TO WR-BUF.
           MOVE BBPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO BBPASSWD-BUF.
           MOVE BBPASSWD-BUF TO WR-BUF.
           MOVE BBPASSWD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A080BBPASSWD:R." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO PW-ERROR-OUT.
           IF VDUSTATUS NOT = "00"
              GO TO PW-ENTER-PASSWORD-PASS2.

           MOVE VDUBUF TO BBPASSWD-BUF.

      * VERIFY 2ND ENTRY OF PASSWORD (NEW ENTRY/EXISTING PW)
       PW-TEST-BB-PASS.
           IF BBPASSWD-UNIVERSAL NOT = SPACES
              IF BBPASSWD-BUF = BBPASSWD-UNIVERSAL
                 MOVE BB-PASSWORD TO BBPASSWD-BUF.

           IF BBPASSWD-BUF = BB-PASSWORD
              IF BBPASSWD-STATUS = "M"
                 MOVE "N" TO BBPASSWD-STATUS
                 GO TO PASSWORD-ENTRY-LOOP
              END-IF
              MOVE "Y" TO BBPASSWD-OKAY
              GO TO PASSWORD-ENTRY-EXIT.

           IF BBPASSWD-STATUS = "N"
              MOVE "NEW PASSWORD VERIFICATION MISMATCH, TRY AGAIN!"
                                                            TO MESS
           ELSE
              MOVE "PASSWORD MISMATCH, MODIFICATIONS DENIED !!"
                                                            TO MESS.
           PERFORM SEND-MESS.
           MOVE "N" TO BBPASSWD-OKAY.
           GO TO PW-ERROR-OUT.

           GO TO PASSWORD-ENTRY-EXIT.

       PW-ERROR-OUT.
           MOVE "X" TO PASSWORD-STAT.

       PASSWORD-ENTRY-EXIT.
           MOVE SPACES TO BBPASSWD-BUF BBPASSWD-MBUF.
           MOVE SPACES        TO WR-BUF.
           MOVE BBPASSWD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES         TO WR-BUF.
           MOVE BBPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BBPASW-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "BBINQ3"  TO VDU-FORM-NAME.


      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "BBINQ2" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
       CONVERT-TIME SECTION.
      ******************************************************************

      * CONVERT TIME FROM MILITARY TO AM/PM FORMAT

           MOVE WORK-TIME TO TIME-NOW.
           MOVE " " TO TIME-MILITARY-FG.
           IF TIME-MILITARY-FG = "Y"
              MOVE " " TO T-AMPM
           ELSE
              IF H-AND-M NOT GREATER THAN 1159
                 MOVE AM-CMD TO T-AMPM
              ELSE
                 MOVE PM-CMD TO T-AMPM.
           IF TIME-MILITARY-FG = " "
              IF H-AND-M IS NOT LESS THAN 1300
                 SUBTRACT 1200 FROM H-AND-M.
           IF TIME-MILITARY-FG = " "
              IF CUR-HH = 0
                 MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.

       CONVERT-TIME-EXIT.
           MOVE " " TO TIME-MILITARY-FG.


      ******************************************************************
       CHANGE-NAME-FREE SECTION.
      ******************************************************************

      * CHANGE NAME - ALLOW FREE TYPE
           MOVE "E  A035NAME." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "00" AND VDUBUF NOT = SPACES
              MOVE VDUBUF TO CASE-BUF
              CALL "C$TOUPPER" USING CASE-BUF, VALUE 20
              MOVE CASE-BUF TO BB-NAME.

           PERFORM DISPLAY-FIELDS.

       CHANGE-NAME-FREE-EXIT.
           EXIT.

      ******************************************************************
       CHANGE-PASSWORD SECTION.
      ******************************************************************

      * PASSWORD ENTRY ROUTINE MAY MAKE CHAGES TO BB-REC SO I NEED
      * TO SAVE IT HERE

           MOVE BB-REC TO SAVED-REC.
           MOVE BB-USERID TO BBINQ2-LOGIN.
           MOVE "X" TO  BBPASSWD-STATUS.
           PERFORM PASSWORD-ENTRY.
           MOVE SAVED-REC TO BB-REC.
           IF NOT PASSWORD-INVALID
              MOVE LOGIN-PASSWORD TO BB-PASSWORD.

       CHANGE-PASSWORD-EXIT.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           DISPLAY SPACES UPON BBINQ2-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.

           DISPLAY BBINQ3-SCREEN UPON BBINQ2-WINDOW-HANDLE
           MOVE BBINQ3-FORM-FIELDS TO WR-BUF
           MOVE BBINQ3-FORM-LIST TO WR-LIST
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE BBINQ3-HELP--FILE TO HELP-LINK-FILE.
           MOVE BBINQ3-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BBINQ3_EVA.CPY".
              COPY "LIBWI/BBPASW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBSP/SPBBCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/POSTDATE.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A630WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE SPACES TO BR-NAME.
       GET-GR.
           PERFORM READ-GR1-FILE.

       PROTECT-FIELDS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > MAX
              MOVE SET-CHFG TO CHFG(SUB)
           END-PERFORM.

       CHANGE-NAME-GROUP.
      * CHANGE NAME - GET FROM GROUP CONTACT
           MOVE GR-CONTACT TO BB-NAME.
           PERFORM DISPLAY-FIELDS.


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BH1-FILE.
           PERFORM CLOSE-BB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPBHGS_SQL.CPY".
       COPY "LIBSP/SPBBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPBB1IN.CPY".
       COPY "LIBSP/SPBH1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BBINQ3-SCREEN.
           DESTROY BBINQ2-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           EXIT PROGRAM.
