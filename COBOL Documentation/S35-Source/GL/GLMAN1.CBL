       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLMAN1.
      ********************************************************************
      *                 (GL)
      *
      *  DESCRIPTION:   GENERAL LEDGER MASTER
      *                 FILE MAINTENANCE
      *  REV:
      *  060795 BLL ADD F6 OPTION TO ZERO AMT FIELDS ON NEW RECORD
      *  000204 MG  FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  001218 GLB ADD F5 GL SCAN WINDOW
      *  030605 GLB ADD LEGEND INFO; REMOVE ALL "TMMSG" REPLACE WITH
      *             SEND-LEGEND
      *  030605 GLB FIX BUG IF NO BRANCH ENTERED AND F5 GLSCAN
      *  040726 BLV WHEN CALLING UP EXISTING RECORD, DON'T ALLOW CHANGES
      *             TO OPENING BALANCES, CURRENT BALANCES, OR LAST YEAR
      *             BALANCES WITHOUT DAILY PASSWORD, ONLY BUDGETS AMTS
      *             CAN BE CHANGED WITH NOT PASSWORD
      *  BLF 070110 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *             /USR/DATA/R1/LG/CHG.YYMMDD, ALSO LOG DELETE,
      *             SUPPORT PR# 2780
      *  BAH 120514 REMOVED SPECIAL-FG
      *******************************************************************
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
           "@(#)GL/GLMAN1 S34 12/03/21-10:31:32 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".

      *=================================================================
      *   GLCH-REC FROM 'REPLACING LEADING' ON GL-REC.        [CHANGED ]
      *   GLOR-REC FROM 'REPLACING LEADING' ON GL-REC.        [ORIGINAL]
      *=================================================================
       COPY "LIBGL/GL01GL.CPY" REPLACING LEADING "GL" BY "GLOR".
       COPY "LIBGL/GL01GL.CPY" REPLACING LEADING "GL" BY "GLCH".

      ******************************************************************
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-ACCOUNT      PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  WORK-ERRLOG-FIELDN       REDEFINES WORK-ERRLOG-FIELD
                                        PIC 9(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  SUB             PIC 99      VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X        REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1         REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9            VALUE 0.
       01  HOLD-GL1-KEY.
           03  HOLD-GL-BRANCH   PIC 9(4)       VALUE ZEROES.
           03  HOLD-GL-ACCOUNT  PIC 9(7)       VALUE ZEROES.

       01  CUR-BAL         PIC S9(11)V99    COMP-3 VALUE 1.
       01  LYCUR-BAL       PIC S9(11)V99    COMP-3 VALUE 1.

       01  ORIGINAL-GL-REC    PIC X(GL-SIZE).
       01  CHANGED-GL-REC     PIC X(GL-SIZE).

       01  GLMAIN-LINK                 VALUE SPACES.
           03  FORM-NAME               PIC X(20).
           03  SAVEKEY                 PIC X(11).
           03  NEW-REC                 PIC X(01).
           03  ERRCD                   PIC X(01).
           03  FORMSEL                 PIC 9(02).
           03  GLMAIN-WINDOW-HANDLE    PIC X(10).

       01  CHAN          PIC X(4).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  A00         PIC X(20).
           03  A01         PIC X(36).
           03  A02         PIC X(4).
           03  A03         PIC X.
           03  A04         PIC ZZZ,ZZZ,ZZ9.99-.
           03  A06         PIC 9(7).
           03  A07         PIC 9(7).
           03  A08         PIC X.
           03  ACURDATA    OCCURS 12 TIMES.
               05  ADATA   PIC ZZZ,ZZZ,ZZ9.99-.
           03  ACURBUDGET  OCCURS 12 TIMES.
               05  ABUDGET PIC ZZZ,ZZZ,ZZ9-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(39) VALUE
           "BRNAME 010 020 030 040 060 070 080 090 ".
           03  FILLER      PIC X(32) VALUE
           "100 110 120 130 140 150 160 170 ".
           03  FILLER      PIC X(36) VALUE
           "180 190 200 210 220 230 240 250 260 ".
           03  FILLER      PIC X(24) VALUE
           "270 280 290 300 310 320.".

       01  MAX          PIC 99   VALUE 32.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      * 1. DESCRIPTION:
           03  FILLER      PIC X(8) VALUE "E  A360 ".
      * 2. CONSOLIDATION NO:
           03  FILLER      PIC X(8) VALUE "ENNN040N".
      * 3. DEBIT / CREDIT CODE:
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. OPENING BALANCE:
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 5. CURRENT BALANCE:
           03  FILLER      PIC X(8) VALUE "E NS092N".
      * 6. GL NO 1 TOTALS:
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 7. GL NO 1 TOTALS:
           03  FILLER      PIC X(8) VALUE "ENNN070 ".
      * 8. EOY CLEAR FLAG:
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *  9 THRU 20 CURRENT YEAR DATA:
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 21 THRU 32 CURRENT YEAR BUDGET:
           03  FILLER      PIC X(8) VALUE "E NS090 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 10 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       01  HOLD-SYS-DATE         PIC 9(8)   COMP-3.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGL/GLSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLMAN1_DEF.CPY".
       COPY "LIBGL/GLMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA               PIC X(2000).

       SCREEN SECTION.
       COPY "LIBGL/GLMAN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************
      *       MAIN PROGRAM MODULE      *
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLMAN1" TO VDU-FORM-NAME.

           PERFORM SET-HOLD-SYS-DATE.

      D    STOP MESS.
           MOVE FORM-LINK-AREA TO GLMAIN-LINK.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE 0 TO HOLD-GL-BRANCH HOLD-GL-ACCOUNT

           PERFORM REC-GL-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 2 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE GL1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      *******************************
       REC-GL-MODULE SECTION.
      *******************************
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO GL1-KEY
              MOVE SPACES TO SAVEKEY
              MOVE "SNNN040GLBRANCH." TO VDU
              MOVE GL-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN070GLACCTNO." TO VDU
              MOVE GL-ACCOUNT TO VDUNUM0
              PERFORM SCREENIO
              GO TO READ-GLFILE.
           MOVE 0 TO NEW-FLAG.
       REC-GL-BRANCH.
           MOVE "F5-SCAN WINDOW, F6-SCAN NEXT, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040GLBRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GL-MODULE.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GL-MODULE.
           IF F5-KEY
              PERFORM SCAN-GL-ACCOUNTS
              IF GLSCAN-ACCOUNT = 0
                 GO TO REC-GL-BRANCH
              ELSE
                 GO TO READ-GLFILE.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO REC-GL-BRANCH.

           MOVE VDUNUM0 TO GL-BRANCH HOLD-GL-BRANCH.
           IF GL-BRANCH = 0
              GO TO REC-GL-BRANCH.

      * CONFIRM VALID BRANCH:
           MOVE GL-BRANCH TO BR-NO.
           PERFORM VERIFY-BRANCH.
           IF IO-BAD
              GO TO REC-GL-BRANCH.
           MOVE "S  A200BRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       REC-GL-ACCTNO.
           MOVE "ENNN070GLACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GL-MODULE.
           IF UP-KEY OR F1-KEY
              GO TO REC-GL-BRANCH.
           IF F5-KEY
              PERFORM SCAN-GL-ACCOUNTS
              IF GLSCAN-ACCOUNT = 0
                 GO TO REC-GL-ACCTNO
              ELSE
                 GO TO READ-GLFILE.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GL-MODULE.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO REC-GL-ACCTNO.
           MOVE VDUNUM0 TO GL-ACCOUNT HOLD-GL-ACCOUNT.
           IF GL-ACCOUNT = 0
              GO TO REC-GL-ACCTNO.
           MOVE GL1-KEY TO HOLD-GL1-KEY.
       READ-GLFILE.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-GOOD
              MOVE GL-REC TO ORIGINAL-GL-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              MOVE "N" TO CHFG(4)  CHFG(9)
              GO TO END-REC-GL-MODULE.
           PERFORM NEW-RECORD-SETUP.
       END-REC-GL-MODULE.
           EXIT.

      ***********************************
       ENTER-SCANKEY SECTION.
      ***********************************
           MOVE "ENTER GL NUMBER, F6 TO SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-GL1-FILE.

           MOVE HOLD-GL-BRANCH    TO GL-BRANCH QGL1-WBEG-BRANCH.
           IF HOLD-GL-BRANCH NOT = 0
              MOVE HOLD-GL-BRANCH TO QGL1-WEND-BRANCH
           ELSE
              MOVE ALL "9"        TO QGL1-WBEG-BRANCH.
           MOVE 0                 TO GL-ACCOUNT
                                     QGL1-WBEG-ACCOUNT.
           MOVE ALL "9"           TO QGL1-WEND-ACCOUNT.
                   
           PERFORM START-GL1-FILE.
       ENTER-KEY.
           MOVE "E  A110SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF F6-KEY
              GO TO READ-FILES-NEXT.
           IF NOT ENTER-KEY
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE GL1-KEY TO HOLD-GL1-KEY
                 MOVE GL-REC TO ORIGINAL-GL-REC
                 MOVE "N" TO CHFG(4)  CHFG(9)
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF TO GL1-KEY.
           INSPECT GL1-KEY REPLACING ALL " " BY "0".

           PERFORM START-GL1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK GL-FILE.
           MOVE "SNNN040GLBRANCH." TO VDU.
           MOVE GL-BRANCH TO VDUNUM0 HOLD-GL-BRANCH.
           PERFORM SCREENIO.
           MOVE "SNNN070GLACCTNO." TO VDU.
           MOVE GL-ACCOUNT TO VDUNUM0 HOLD-GL-ACCOUNT.
           PERFORM SCREENIO.
           MOVE GL-BRANCH TO BR-NO.
           PERFORM VERIFY-BRANCH.
           MOVE "S  A200BRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-GL1-FILE.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.

      ***********************************
       SCAN-GL-ACCOUNTS SECTION.
      ***********************************
           MOVE HOLD-GL-BRANCH TO GLSCAN-BRANCH.
           MOVE 0              TO GLSCAN-ACCOUNT.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-ACCOUNT NOT = 0
              MOVE "SNNN040GLBRANCH." TO VDU
              MOVE GLSCAN-BRANCH TO VDUNUM0 HOLD-GL-BRANCH
              PERFORM SCREENIO
              MOVE "SNNN070GLACCTNO." TO VDU
              MOVE GLSCAN-ACCOUNT TO VDUNUM0 HOLD-GL-ACCOUNT
              PERFORM SCREENIO
              MOVE HOLD-GL1-KEY TO GL1-KEY.


      ************************************************
      *   ENTRY-MODULE                               *
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD      *
      *              1 - INITIAL NEW RECORD ENTRY    *
      *              2 - MODIFY NEW RECORD           *
      ************************************************
       ENTRY-MODULE SECTION.
      ************************************************
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
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 2
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF ELE = 5
              MOVE SPACES TO CHKEY.
           IF ELE = 2
              IF NEW-FLAG NOT = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CREATE-ROUTINE.

           MOVE SPACES TO MESS.
           IF NEW-FLAG = 1
              IF ELE > 8
                 MOVE "F1-CANCEL, F6 ZERO FIELDS" TO MESS.
           PERFORM SEND-LEGEND.

           IF ELE NOT > 8
              IF CHKEY NOT = ZEROS
                 IF CHFG(ELE) = " "
                    IF NEW-FLAG NOT = 1
                       GO TO MODIFY-ROUTINE.
           IF ELE NOT > 8
              IF CHFG(ELE) = "N"
                 IF NEW-FLAG = 1
                    GO TO CREATE-ROUTINE
                 ELSE
                    IF CHKEY NOT = CHAN
                       GO TO MODIFY-ROUTINE.

           IF (ELE > 8 AND ELE < 21)
              IF NEW-FLAG = 0
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.

           IF ELE NOT < 21
              MOVE SPEC(10) TO VDU
           ELSE
              IF ELE NOT < 9
                 MOVE SPEC(9) TO VDU
              ELSE
                 MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
                MOVE -1 TO ONE
                GO TO CREATE-ROUTINE.
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.
       TEST-F6.
           IF F6-KEY
              IF NEW-FLAG = 1
                 IF ELE > 8
                    PERFORM ZERO-FIELDS
                    GO TO CREATE-ROUTINE.
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD1.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08
                 DEPENDING ON ELE.
           IF ELE NOT < 9
               AND NOT > 20
                   GO TO A-CUR-DATA.
           IF ELE NOT < 21
               AND NOT > 32
                   GO TO A-CUR-BUDGET.
           GO TO END-ENTRY-MODULE.
      *********************************
      *     SET RECORD ELEMENTS       *
      *********************************
       A-01.
           MOVE VDUBUF TO GL-DESCRIPTION.
           GO TO CREATE-ROUTINE.
       A-02.
           GO TO CREATE-ROUTINE.
      *    MOVE VDUNUM0 TO GL-CONSOL.
       A-03.
           MOVE VDUBUF TO GL-TYPE.
           IF NOT (GL-TYPE = "D" OR "C")
              MOVE "ENTER `D` - DEBIT OR `C` - CREDIT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO GL-OPENBAL.
           PERFORM SEND-CURRBAL.
           GO TO CREATE-ROUTINE.
       A-05.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM0 TO GL-NUMBER1.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM0 TO GL-NUMBER2.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO GL-CLEAR.
           IF NOT (GL-CLEAR = "Y" OR "N")
              MOVE "ENTER `Y` OR `N`" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-CUR-DATA.
           SUBTRACT 8 FROM ELE GIVING SUB.
           MOVE VDUNUM2 TO GL-CYBAL(SUB).
           PERFORM SEND-CURRBAL.
           GO TO CREATE-ROUTINE.
       A-CUR-BUDGET.
           SUBTRACT 20 FROM ELE GIVING SUB.
           MOVE VDUNUM0 TO GL-CYBUD(SUB).
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
           PERFORM OPEN-GL1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE GL-REC TO CHANGED-GL-REC.
           IF ORIGINAL-GL-REC = CHANGED-GL-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF GL-REC NOT = ORIGINAL-GL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-GL-REC TO GL-REC.
           PERFORM REWRITE-GL1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-GL1-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.
       END-WRITE-MODULE.
           PERFORM CLOSE-GL1-FILE.


      ****************************
       LOG-MAINTENANCE-CHGS SECTION.
      ****************************
           MOVE ORIGINAL-GL-REC TO GLOR-REC.
           MOVE CHANGED-GL-REC  TO GLCH-REC.

           MOVE SPACES     TO WORK-ERRLOG-DETAIL.
           MOVE GL-BRANCH  TO WORK-ERRLOG-BR.
           MOVE GL-ACCOUNT TO WORK-ERRLOG-ACCOUNT.
           MOVE "TO "      TO WORK-ERRLOG-TO-X.

           IF GLOR-DESCRIPTION NOT = GLCH-DESCRIPTION
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "OLD DESC"              TO WORK-ERRLOG-DESC
             MOVE GLOR-DESCRIPTION        TO WORK-ERRLOG-LONG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF GLOR-DESCRIPTION NOT = GLCH-DESCRIPTION
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "NEW DESC"              TO WORK-ERRLOG-DESC
             MOVE GLCH-DESCRIPTION        TO WORK-ERRLOG-LONG-ALP
             PERFORM LOG-FIELD-CHANGE
             MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF GLOR-TYPE NOT = GLCH-TYPE
             MOVE "030"                   TO WORK-ERRLOG-FIELD
             MOVE "TYPE-D/C"              TO WORK-ERRLOG-DESC
             MOVE GLOR-TYPE               TO WORK-ERRLOG-ORG-ALP
             MOVE GLCH-TYPE               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF GLOR-OPENBAL NOT = GLCH-OPENBAL
             MOVE "040"                   TO WORK-ERRLOG-FIELD
             MOVE "OPEN BAL"              TO WORK-ERRLOG-DESC
             MOVE GLOR-OPENBAL            TO WORK-ERRLOG-ORG-N2
             MOVE GLCH-OPENBAL            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF GLOR-NUMBER1 NOT = GLCH-NUMBER1
             MOVE "060"                   TO WORK-ERRLOG-FIELD
             MOVE "GL #1   "              TO WORK-ERRLOG-DESC
             MOVE GLOR-NUMBER1            TO WORK-ERRLOG-ORG-N0
             MOVE GLCH-NUMBER1            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF GLOR-NUMBER2 NOT = GLCH-NUMBER2
             MOVE "070"                   TO WORK-ERRLOG-FIELD
             MOVE "GL #2   "              TO WORK-ERRLOG-DESC
             MOVE GLOR-NUMBER2            TO WORK-ERRLOG-ORG-N0
             MOVE GLCH-NUMBER2            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF GLOR-CLEAR NOT = GLCH-CLEAR
             MOVE "030"                   TO WORK-ERRLOG-FIELD
             MOVE "CLEAR FG"              TO WORK-ERRLOG-DESC
             MOVE GLOR-CLEAR              TO WORK-ERRLOG-ORG-ALP
             MOVE GLCH-CLEAR              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF GLOR-CYBAL(SUB) NOT = GLCH-CYBAL(SUB)
                COMPUTE WORK-ERRLOG-FIELDN = SUB + 8
                MOVE "CYBAL &&"           TO WORK-ERRLOG-DESC
                INSPECT WORK-ERRLOG-DESC REPLACING FIRST "&&" BY SUB
                MOVE GLOR-CYBAL(SUB)      TO WORK-ERRLOG-ORG-N2
                MOVE GLCH-CYBAL(SUB)      TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF GLOR-CYBUD(SUB) NOT = GLCH-CYBUD(SUB)
                COMPUTE WORK-ERRLOG-FIELDN = SUB + 20
                MOVE "CYBUD &&"           TO WORK-ERRLOG-DESC
                INSPECT WORK-ERRLOG-DESC REPLACING FIRST "&&" BY SUB
                MOVE GLOR-CYBUD(SUB)      TO WORK-ERRLOG-ORG-N2
                MOVE GLCH-CYBUD(SUB)      TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

      *****************************
       LOG-FIELD-CHANGE SECTION.
      *****************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *****************************
       DELETE-MODULE SECTION.
      *****************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM SET-LYCURRBAL.
           IF CUR-BAL NOT = 0
            OR LYCUR-BAL NOT = 0
               PERFORM ACTIVE-ACCOUNT
               GO TO END-DELETE-MODULE.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF GL-REC NOT = ORIGINAL-GL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-GL1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING GL1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-GL1-FILE.

      ********************************
       NEW-RECORD-SETUP SECTION.
      ********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           MOVE SPACES TO GL-REC.
           MOVE HOLD-GL1-KEY TO GL1-KEY.
           MOVE 0 TO GL-CONSOL GL-OPENBAL GL-NUMBER1 GL-NUMBER2
                     GL-LYOPENBAL.
           MOVE 0 TO ELE SUB.
           MOVE " " TO CHFG(4) CHFG(9).
           PERFORM CLEAR-GL-TABLE 11 TIMES.
       CLEAR-GL-TABLE.
           ADD 1 TO SUB.
           MOVE 0 TO GL-CYBAL(SUB)
                     GL-LYBAL(SUB)
                     GL-CYBUD(SUB)
                     GL-LYBUD(SUB)
                     GL-NYBUD(SUB).
       NEW-RECORD-EXIT.
           EXIT.

      ******************************
       DISPLAY-FIELDS SECTION.
      ******************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE GL-BRANCH TO BR-NO.
           PERFORM VERIFY-BRANCH.
           MOVE "S  A200BRNAME." TO VDU.
           MOVE BR-NAME TO A00.
           MOVE GL-DESCRIPTION TO A01.
      *    MOVE GL-CONSOL TO A02.
           MOVE SPACES TO A02.
           MOVE GL-TYPE TO A03.
           MOVE GL-OPENBAL TO A04.
           MOVE GL-NUMBER1 TO A06.
           MOVE GL-NUMBER2 TO A07.
           MOVE GL-CLEAR TO A08.

           MOVE 0 TO SUB.
           PERFORM DISPLAY-FIELDS-TABLES 11 TIMES.
       DISPLAY-FIELDS-TABLES.
           ADD 1 TO SUB.
           MOVE GL-CYBAL(SUB) TO ADATA(SUB).
           MOVE GL-CYBUD(SUB) TO ABUDGET(SUB).

       DISPLAY-FIELDS-BUF.
           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM SEND-CURRBAL.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *******************************
       ZERO-FIELDS SECTION.
      *******************************
       ZERO-FIELDS-LOOP.
      * ZERO CURRENT TABLE OR BUDGET
           IF ELE < 21
               MOVE "S  S092000." TO VDU
               SUBTRACT 8 FROM ELE GIVING SUB
               MOVE 0 TO GL-CYBAL(SUB) VDUNUM2
           ELSE
               MOVE "S  S090000." TO VDU
               SUBTRACT 20 FROM ELE GIVING SUB
               MOVE 0 TO GL-CYBUD(SUB) VDUNUM0.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           ADD 1 TO ELE.
           IF ELE NOT > MAX
              GO TO ZERO-FIELDS-LOOP.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME          TO GLMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO GLMAN1-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD        TO CHAN.
           DISPLAY GLMAN1-SCREEN UPON GLMAIN-WINDOW-HANDLE.
           MOVE GLMAN1-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE GLMAN1-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001


       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE GLMAN1-HELP--FILE          TO HELP-LINK-FILE.
           MOVE GLMAN1-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "00"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBGL/GLMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT

           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-CURRBAL.
           MOVE "S NS092050." TO VDU.
           COMPUTE VDUNUM2 CUR-BAL = GL-OPENBAL
                     + GL-CYBAL(1)  + GL-CYBAL(2)  + GL-CYBAL(3)
                     + GL-CYBAL(4)  + GL-CYBAL(5)  + GL-CYBAL(6)
                     + GL-CYBAL(7)  + GL-CYBAL(8)  + GL-CYBAL(9)
                     + GL-CYBAL(10) + GL-CYBAL(11) + GL-CYBAL(12).
           PERFORM SCREENIO.
       SET-LYCURRBAL.
           COMPUTE LYCUR-BAL = GL-LYOPENBAL
                     + GL-LYBAL(1)  + GL-LYBAL(2)  + GL-LYBAL(3)
                     + GL-LYBAL(4)  + GL-LYBAL(5)  + GL-LYBAL(6)
                     + GL-LYBAL(7)  + GL-LYBAL(8)  + GL-LYBAL(9)
                     + GL-LYBAL(10) + GL-LYBAL(11) + GL-LYBAL(12).
       VERIFY-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE "NOT ON FILE" TO BR-NAME.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-ACCOUNT.
           MOVE "ACTIVE ACCOUNT CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A000MESS." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SET-HOLD-SYS-DATE.
           PERFORM GETDAT.
           MOVE SYS-DATE TO HOLD-SYS-DATE.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *******************************
       END-ROUTINE SECTION.
      *******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLMAN1-SCREEN.
           MOVE FORM-PATH                  TO FORM-PATHNAME.
           MOVE GLMAIN-LINK                TO FORM-LINK-AREA.
           EXIT PROGRAM.
