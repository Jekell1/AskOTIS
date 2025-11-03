       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWCRE5.
       DATE-WRITTEN.    061120.
      **************************************************************************
      *   BORROWER CREATE   F5-CREATE BORROWER FROM LOAN INPUT
      *                          PAGE 5 ("KNOW YOUR CUSTOMER" FIELDS)
      *                        (WINDOW)
      * REV:
      * MJD 070110 NEW PROGRAM FOR NEW BX FIELDS. REGENCY #2856
      * MJD 070823 ADDED CODE SCAN FOR IDTYPE.    REGENCY #3032
      * MJD 070905 FIXED BUG. CHANGE RECORD WAS INDICATINGS
      *            ZIP WAS CHANGED WHEN THE IDTYPE CHANGED. CHANGED
      *            CHANGE DESC TO INDICATE IDTYPE.
      * MJD 070906 CHANGED TO ALLOW SPACES AS THE IDTYPE        REGENCY PR#3032
      * BAH 081027 MEMO FOR CHANGING FIELD 8 WAS MESSED UP, REGENCY PR# 642
      * BLM 110217 IF I-AM-WORLD & GP-CUST-INFO-FG = Y, DON'T ALLOW CHANGING
      *            FIELD 11 (BX-KYC-LAST-VERDATE), WORLD PR# 739
      * BLM 110217 NOTICED CDSCANW WASN'T UNDER WINDOWSW, SO HITTING F6 AT
      *            FIELD #6 CAUSED "OVERFLOW IN CALLING SCAN"
      * BAH 130123 A30 CONVERSION
      * BAH 150928 MADE PROMPTS 1,2,3,5 AND 7 FIELDS FOR SPANISH, WORLD PR#1103
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *  BAH 170119 ADDED LEGEND-01-SPANISH FOR WORLD PR#1218
      *  BAH 170407 ACTIVATE 8 DIGIT DATES, PD0006
      *  BLM 20210920 MOVE BX-PATH-OWNBR TO BX-BRNO BEFORE WRITING BX
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./WI/BWCRE5.CBL S34 09/20/21-15:47:04 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  ERRCD               PIC X     VALUE SPACES.

       01  SAVE-BM1-KEY.
           03  SAVE-BM-BRNO    PIC 9(4).
           03  SAVE-BM-SSNO    PIC 9(9).
           03  SAVE-BM-DATE    PIC 9(8).
           03  SAVE-BM-SEQ     PIC 9(3).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-CHAR   PIC X(24).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).

       01  LEGEND          PIC X(35).
       01  LEGEND-01.
           03  FILLER      PIC X(21)  VALUE
           "F1, F7-EXIT, ENTER #:".
       01  LEGEND-01-SPANISH.
           03  FILLER      PIC X(24)  VALUE
           "F1, F7-SALIR, # DE FILA:".
       01  LEGEND-02.
           03  FILLER      PIC X(8)  VALUE
           " F7-EXIT".

       01  BWCRE5-LINES          PIC 99  VALUE 22.
       01  BWCRE5-COLUMNS        PIC 99  VALUE 78.
       01  BWCRE5-BEGIN-Y        PIC 99  VALUE 2.
       01  BWCRE5-BEGIN-X        PIC 99  VALUE 2.

       01  IDTYPE-CODE     PIC X(2).
       01  IDTYPE-DESC     PIC X(35).
       01  IDTYPE-VALID-YN PIC X.
           88  IDTYPE-VALID VALUE 'Y'.

       01  ONE             PIC S9     COMP.
       01  ELE             PIC 9(2)   COMP.
       01  NEW-FLAG        PIC 9.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  IO-TYPE         PIC X(3).

       01  HOLD-TIN            PIC 9(9).
       01  HOLD-TINX REDEFINES HOLD-TIN.
           03  HOLD-TIN123     PIC 9(3).
           03  HOLD-TIN45      PIC 9(2).
           03  HOLD-TIN6789    PIC 9(4).
      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-NAME              PIC X(29).
           03  D-TIN               PIC X(11).
           03  D-TINX REDEFINES    D-TIN.
               05  D-TIN123        PIC 9(3).
               05  D-DASH1         PIC X.
               05  D-TIN45         PIC 9(2).
               05  D-DASH2         PIC X.
               05  D-TIN6789       PIC 9(4).
           03  D-BDATE             PIC 99/99/99.
           03  D-EMPLPOS           PIC X(24).
           03  F01                 PIC X(24).
           03  F02                 PIC X(24).
           03  F03                 PIC X(15).
           03  F04                 PIC X(2).
           03  F05                 PIC X(10).
           03  F06                 PIC X(2).
           03  F061                PIC X(35).
           03  F07                 PIC X(2).
           03  F08                 PIC X(25).
           03  F09                 PIC 99/99/99.
           03  F10                 PIC 99/99/99.
           03  F11                 PIC 99/99/99.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(26) VALUE
           "DNAME DSSNO DAGE DEMPLPOS ".
           03  FILLER      PIC X(20) VALUE
           "010 020 030 040 050 ".
           03  FILLER      PIC X(28) VALUE
           "060 061 070 080 090 100 110.".

       01  MAX             PIC 99   VALUE 11.

       01  SPEC-TABLE.
      * 1. KYC ADDRESS # 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 2. KYC ADDRESS # 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 3. KYC CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 4. KYC STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 5. KYC ZIP CODE
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 6. KYC ID TYPE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 7. KYC ISSUING AUTHORITY
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 8. KYC NUMBER
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      * 9. KYC DATE OF ISSUE
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *10. KYC DATE OF EXPIRATION
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
      *11. KYC DATE LAST VERIFIED
           03  FILLER      PIC X(8) VALUE "ERNM080 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 11 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  LEGEND-WALK.
           03  FILLER                   PIC X(10)  VALUE "F1, ^-UP, ".
           03  FILLER                   PIC X      VALUE X'76'.
           03  FILLER                   PIC X(24)  VALUE
           "-DOWN, ENTER FIELD INFO:".

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BWCRE5_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/BWCRE5_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  BWCRE5-WINDOW-HANDLE       PIC X(10).
       01  CHAN                       PIC X(4).

      *=================================================================
      * ORIG-BW-REC FROM 'REPLACING LEADING' ON BW-REC - ORIGINAL-BW-REC
      * ORIG-BX-REC FROM 'REPLACING LEADING' ON BX-REC - ORIGINAL-BX-REC
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "ORIG-BW".
       COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "ORIG-BX".

       01  CHANGED-BX-REC      PIC X(BX-SIZE).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01BW.CPY".
       01  ALLOW-CHANGES       PIC X.

       SCREEN SECTION.
       COPY "LIBWI/BWCRE5_SCN.CPY".

       PROCEDURE DIVISION USING FORM-PATHNAME
                  BW-REC
                  ALLOW-CHANGES 
                  EXIT-PATHNAME.

      *************************************
       MAIN-PROGRAM SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "BWCRE5" TO VDU-FORM-NAME.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE BWCRE5-BEGIN-Y * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  BWCRE5-BEGIN-X * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    BWCRE5-COLUMNS + VDU-WIN-W-MOD
                      LINES   BWCRE5-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS BWCRE5-WINDOW-HANDLE.

           MOVE " " TO VDU-WIMENU-SELECTION.

      * STORE ORIGINAL BW-REC FROM PAGE 1:
           MOVE BW-REC TO ORIG-BW-REC.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

      * RESTORE ORIGINAL BW-REC FROM PAGE 1:
           IF IO-TYPE = "CLR"
              MOVE ORIG-BW-REC TO BW-REC.

           GO TO MAIN-MODULE.


      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF GP-CUST-INFO-FG = "Y"
              MOVE "N" TO CHFG(11).
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
      * ALLOW MODIFY OF BRANCH
           IF ALLOW-CHANGES = "Y"
              PERFORM TEST-LEGEND-01
           ELSE
              MOVE LEGEND-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              PERFORM UPDATE-BX-FILE
              PERFORM CREATE-CHANGE-REC
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           IF ALLOW-CHANGES = "N"
              PERFORM ALARM
              MOVE "PLEASE NOTE..." TO ERRMSG-HEAD
              MOVE "CHANGES ARE NOT ALLOWED FROM THIS SCREEN!!"
                                            TO ERRMSG-MSG1
              MOVE SPACES TO ERRMSG-MSG2 ERRMSG-MSG3
              PERFORM CALL-ERRMSG
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 1
              MOVE LEGEND-WALK TO LEGEND
           ELSE
              IF ELE = 6
                 MOVE "F1-CANCEL,F6-SCAN,ENTER FIELD INFO:" TO LEGEND
              ELSE
                 MOVE "F1-CANCEL, ENTER FIELD INFO:" TO LEGEND.
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
           IF VDUSTATUS = "1<"
              IF ELE = 6
                 PERFORM CODE-SCAN
                 IF IO-FG NOT = 0
                    GO TO ENTER-ELE
                 ELSE
                    GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 F-07 F-08 F-09 F-10 F-11
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO BX-KYC-ADR1.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BX-KYC-ADR2.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BX-KYC-CITY.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BX-KYC-STATE.
           IF VDUBUF = " " AND BX-KYC-ADR1 = " " AND
                               BX-KYC-ADR2 = " "
              GO TO CREATE-ROUTINE.

           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO BX-KYC-ZIP.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO BX-KYC-IDTYPE IDTYPE-CODE.
           PERFORM GET-IDTYPE-DESC.
           IF NOT IDTYPE-VALID
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO BX-KYC-ISSAUTH.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO BX-KYC-NUMBER.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDU-DATE-YYYYMMDD TO BX-KYC-ISSDATE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDU-DATE-YYYYMMDD TO BX-KYC-EXPDATE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDU-DATE-YYYYMMDD TO BX-KYC-LAST-VERDATE.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************
       UPDATE-BX-FILE SECTION.
      ************************************
           MOVE BX-REC TO CHANGED-BX-REC.
           IF ORIG-BX-REC = CHANGED-BX-REC
              GO TO EXIT-UPDATE-BX-FILE.

           PERFORM OPEN-BX1-FILE.

           MOVE BW-SSNO  TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           MOVE CHANGED-BX-REC TO BX-REC.
           IF IO-FG = 0
              PERFORM REWRITE-BX1-FILE
           ELSE
              PERFORM WRITE-BX1-FILE.

           PERFORM CLOSE-BX1-FILE.

       EXIT-UPDATE-BX-FILE.
           EXIT.

      *****************************
       CREATE-CHANGE-REC SECTION.
      *****************************
           PERFORM GETDAT.

           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-BM1-FILE.

           IF ORIG-BX-KYC-ADR1 NOT = BX-KYC-ADR1
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC ADR1" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-ADR1 TO CH-ORG-CHAR
              MOVE BX-KYC-ADR1 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-ADR2 NOT = BX-KYC-ADR2
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC ADR2" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-ADR2 TO CH-ORG-CHAR
              MOVE BX-KYC-ADR2 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-CITY NOT = BX-KYC-CITY
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC CITY" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-CITY TO CH-ORG-CHAR
              MOVE BX-KYC-CITY TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-STATE NOT = BX-KYC-STATE
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC STATE" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-STATE TO CH-ORG-CHAR
              MOVE BX-KYC-STATE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-ZIP NOT = BX-KYC-ZIP
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC ZIP" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-ZIP TO CH-ORG-CHAR
              MOVE BX-KYC-ZIP TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-IDTYPE NOT = BX-KYC-IDTYPE
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC IDTYPE" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-IDTYPE TO CH-ORG-CHAR
              MOVE BX-KYC-IDTYPE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-ISSAUTH NOT = BX-KYC-ISSAUTH
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC ISSAUTH" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-ISSAUTH TO CH-ORG-CHAR
              MOVE BX-KYC-ISSAUTH TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-NUMBER NOT = BX-KYC-NUMBER
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC NUMBER" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-NUMBER TO CH-ORG-CHAR
              MOVE BX-KYC-NUMBER TO CH-NEW-CHAR
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-ISSDATE NOT = BX-KYC-ISSDATE
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC ISSDATE" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-ISSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-DATE
              MOVE BX-KYC-ISSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-DATE
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-EXPDATE NOT = BX-KYC-EXPDATE
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC EXPDATE" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-EXPDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-DATE
              MOVE BX-KYC-EXPDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-DATE
              PERFORM UPDATE-CH-BM-FILE.
           IF ORIG-BX-KYC-LAST-VERDATE NOT = BX-KYC-LAST-VERDATE
              PERFORM CREATE-SETUP-REC
              MOVE SPACE TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE "BX KYC LAST-VERDATE" TO CH-DESCRIPT
              MOVE ORIG-BX-KYC-LAST-VERDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-ORG-DATE
              MOVE BX-KYC-LAST-VERDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO CH-NEW-DATE
              PERFORM UPDATE-CH-BM-FILE.

           PERFORM CLOSE-CH-FILE.
           GO TO EXIT-CREATE-CHANGE-REC.

       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE BW-OWNBR           TO CH-BRNO.
           MOVE SYS-DATE           TO CH-DATE.
           MOVE "M"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE 1                  TO CH-SEQ.

           MOVE BW-SSNO            TO CH-SSNO.
       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ****************************************
       UPDATE-CH-BM-FILE SECTION.
      ****************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO NOT = "Y"
              GO TO UPDATE-CH-BM-FILE-EXIT.

           MOVE BM-PATH-OWNBR TO SAVE-BM-BRNO.
           MOVE CH-SSNO       TO SAVE-BM-SSNO.
           COMPUTE SAVE-BM-DATE = 99999999 - SYS-DATE.
           MOVE 1             TO SAVE-BM-SEQ.
       UPDATE-CH-BM-FILE-AGAIN.
           MOVE SPACES TO BM-REC.

           MOVE SAVE-BM1-KEY TO BM1-KEY.

           MOVE CH-USERID TO MEMO-USERID.
           MOVE CH-DESCRIPT TO MEMO-DESCRIPT.
           MOVE CH-ORG-CHAR TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA TO BM-MEMO.

           MOVE "9" TO BM-MEMO-TYPE.
           PERFORM WRITE-BM1-FILE.
           IF IO-BAD
              IF SAVE-BM-SEQ < 999
                 ADD 1 TO SAVE-BM-SEQ
                 GO TO UPDATE-CH-BM-FILE-AGAIN.

       UPDATE-CH-BM-FILE-EXIT.
           EXIT.

      **************************************
       DISPLAY-FIELDS SECTION.
      **************************************
           MOVE SPACES TO DISPLAY-BUF.

           PERFORM OPEN-BX1-FILE.
           MOVE BW-SSNO TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           PERFORM CLOSE-BX1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-BX-FILE
              MOVE BX-PATH-OWNBR TO BX-BRNO
              MOVE BW-SSNO TO BX-SSNO.
           MOVE BX-REC TO ORIG-BX-REC.

      * DISPLAY ONLY FIELDS
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO D-NAME.
           MOVE BW-SSNO TO HOLD-TIN.
           MOVE HOLD-TIN123 TO D-TIN123.
           MOVE HOLD-TIN45 TO D-TIN45.
           MOVE HOLD-TIN6789 TO D-TIN6789.
           MOVE "-" TO D-DASH1 D-DASH2.
           MOVE BW-EMPLPOS TO D-EMPLPOS.
           MOVE BW-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-BDATE.

      * INTERACTIVE FIELDS

      * IF NO ADDRESS HAS BEEN ENTERRED, PULL INFO FROM BORROWER RECORD.
           IF BX-KYC-ADR1 = SPACES AND
              BX-KYC-ADR2 = SPACES AND
              BX-KYC-CITY = SPACES AND
              BX-KYC-STATE = SPACES AND
              BX-KYC-ZIP = SPACES
                 MOVE BW-ADR1 TO BX-KYC-ADR1
                 MOVE BW-ADR2 TO BX-KYC-ADR2
                 MOVE BW-CITY TO BX-KYC-CITY
                 MOVE BW-STATE TO BX-KYC-STATE
                 MOVE BW-ZIP TO BX-KYC-ZIP.

           MOVE BX-KYC-ADR1 TO F01.
           MOVE BX-KYC-ADR2 TO F02.
           MOVE BX-KYC-CITY TO F03.
           MOVE BX-KYC-STATE TO F04.
           MOVE BX-KYC-ZIP TO F05.
           MOVE BX-KYC-IDTYPE TO F06 IDTYPE-CODE.
           PERFORM GET-IDTYPE-DESC.
           MOVE IDTYPE-DESC TO F061.
           MOVE BX-KYC-ISSAUTH TO F07.
           MOVE BX-KYC-NUMBER TO F08.
           IF BX-KYC-ISSDATE NOT = 0
              MOVE BX-KYC-ISSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F09.
           IF BX-KYC-EXPDATE NOT = 0
              MOVE BX-KYC-EXPDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F10.
           IF BX-KYC-LAST-VERDATE NOT = 0
              MOVE BX-KYC-LAST-VERDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO F11.

           IF BR-STATE = "MX"
              MOVE "S  A160P01." TO VDU
              MOVE "CLAVE ACT ECON :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A160P02." TO VDU
              MOVE "CLAVE OCUPACION:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A160P03." TO VDU
              MOVE "CLAVE PAIS     :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A160P05." TO VDU
              MOVE "CLAVE MUNICIPIO:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200P07." TO VDU
              MOVE "PERSONA PEP        :" TO VDUBUF
              PERFORM SCREENIO.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************
       CODE-SCAN SECTION.
      **************************************
           MOVE "ID"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020060." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF IDTYPE-CODE.
           PERFORM SCREENIO.

           PERFORM GET-IDTYPE-DESC.
           MOVE 0 TO IO-FG.
           MOVE CDSCAN-CODE TO VDUBUF IDTYPE-CODE.

       EXIT-CODE-SCAN.
           EXIT.

      **************************************
       GET-IDTYPE-DESC SECTION.
      **************************************
           IF IDTYPE-CODE = "  "
              MOVE "Y" TO IDTYPE-VALID-YN
              MOVE SPACES TO IDTYPE-DESC
              GO TO GET-IDTYPE-DESC-DISPLAY.

           MOVE "** CODE NOT FOUND **" TO IDTYPE-DESC.
           MOVE "N" TO IDTYPE-VALID-YN.

           MOVE "ID"        TO CD-TYPE.
           MOVE IDTYPE-CODE TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO IDTYPE-VALID-YN
              MOVE CD-DESC TO IDTYPE-DESC.
           PERFORM CLOSE-CD1-FILE.

       GET-IDTYPE-DESC-DISPLAY.
           MOVE "S  A035061." TO VDU.
           MOVE IDTYPE-DESC TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-GET-IDTYPE-DESC.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBXCL.CPY".
       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************
       FORM-RESET SECTION.
      ******************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE5-SCREEN.
           DISPLAY BWCRE5-SCREEN UPON BWCRE5-WINDOW-HANDLE.
           MOVE BWCRE5-FORM-FIELDS TO WR-BUF.
           MOVE BWCRE5-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           MOVE BWCRE5-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWCRE5-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BWCRE5_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARAGRAPHS SECTION.
      **************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       TEST-LEGEND-01.
           IF BR-STATE = "MX"
              MOVE LEGEND-01-SPANISH TO LEGEND
           ELSE
              MOVE LEGEND-01 TO LEGEND.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      **************************************
       IO-ROUTINE SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCHIN.CPY".

      **************************************
       END-ROUTINE SECTION.
      **************************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE5-SCREEN.
           DESTROY BWCRE5-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
