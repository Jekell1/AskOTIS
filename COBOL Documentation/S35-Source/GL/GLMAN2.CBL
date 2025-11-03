       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLMAN2.
      ******************************************************************
      *                 (GL)
      *
      *  DESCRIPTION:   GENERAL LEDGER MASTER
      *                 FILE MAINTENANCE
      *                 PAGE 2
      *  REV:
      *  BLL 060795 ADDED OPTION FOR F6 ZERO AMOUNT FIELDS ON NEW RECORD
      *  MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  KEC 000204 CHANGED THE PARAGRAPH NAME FROM 'ENTER-KEY' TO
      *              'ENTER-KEY-ENTRY'.
      *  030605 GLB ADD LEGEND INFO; REMOVE ALL "TMMSG" REPLACE WITH
      *              SEND-LEGEND
      *  040726 BLV WHEN CALLING UP EXISTING RECORD, DON'T ALLOW CHANGES
      *             TO OPENING BALANCES, CURRENT BALANCES, OR LAST YEAR
      *             BALANCES WITHOUT DAILY PASSWORD, ONLY BUDGETS AMTS
      *             CAN BE CHANGED WITH NOT PASSWORD
      *  BLF 070110 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *             /USR/DATA/R1/LG/CHG.YYMMDD, SUPPORT PR# 2780
      *  BLM 090120 ADDED FIELD 39, GL-SPANISH-DESC, ONLY ENTERED IF
      *             I-AM-WORLD, WORLD PR# 616
      *  BAH 120514 REMOVED SPECIAL-FG
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./GL/GLMAN2.CBL S34 06/09/21-11:37:58 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

      *=================================================================
      *   GLCH-REC FROM 'REPLACING LEADING' ON GL-REC.
      *   GLOR-REC FROM 'REPLACING LEADING' ON GL-REC.
      *=================================================================
       COPY "LIBGL/GL01GL.CPY" REPLACING LEADING "GL" BY "GLCH".
       COPY "LIBGL/GL01GL.CPY" REPLACING LEADING "GL" BY "GLOR".

      *-----------------------------------------------------------------
       01  CHANGED-GL-REC              PIC X(GL-SIZE).
       01  ORIGINAL-GL-REC             PIC X(GL-SIZE).

      *-----------------------------------------------------------------
       01  HOLD-GL1-KEY.
           03  HOLD-GL-BRANCH          PIC 9(4)      VALUE ZEROES.
           03  HOLD-GL-ACCOUNT         PIC 9(7)      VALUE ZEROES.

      *=================================================================
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

       01  LYCUR-BAL       PIC S9(11)V99    VALUE 1.

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
       01  DISPLAY-BUF          VALUE SPACES.
           03  ADESC            PIC X(36).
           03  A01              PIC ZZZ,ZZZ,ZZ9.99-.
           03  A-LYDATA         OCCURS 12 TIMES.
               05  ALYDATA      PIC ZZZ,ZZZ,ZZ9.99-.
           03  A-LYBUDGET       OCCURS 12 TIMES.
               05  ALYBUDGET    PIC ZZZ,ZZZ,ZZ9-.
           03  A-NYBUDGET       OCCURS 12 TIMES.
               05  ANYBUDGET    PIC ZZZ,ZZZ,ZZ9-.
           03  SPANISH-DESC     PIC X(36).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(39) VALUE
           "GLDESC 010 030 040 050 060 070 080 090 ".
           03  FILLER      PIC X(32) VALUE
           "100 110 120 130 140 150 160 170 ".
           03  FILLER      PIC X(32) VALUE
           "180 190 200 210 220 230 240 250 ".
           03  FILLER      PIC X(32) VALUE
           "260 270 280 290 300 310 320 330 ".
           03  FILLER      PIC X(24) VALUE
           "340 350 360 370 380 390.".

       01  MAX          PIC 99   VALUE 38.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      * 1. LAST YEAR OPENING BALANCE:
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 2. LAST YEAR ENDING BALANCE:
           03  FILLER      PIC X(8) VALUE "E NS092N".
      *  3 THRU 14 LAST YEAR DATA:
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 15 THRU 26 LAST YEAR BUDGET:
           03  FILLER      PIC X(8) VALUE "E NS090 ".
      * 27 THRU 38 LAST YEAR BUDGET:
           03  FILLER      PIC X(8) VALUE "E NS090 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 5 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLMAN2_DEF.CPY".
       COPY "LIBGL/GLMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       01 FORM-LINK-AREA               PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGL/GLMAN2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************
       MAIN-PROGRAM SECTION.
      **********************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLMAN2" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO GLMAIN-LINK.

           MOVE "S  A170PROMPT39." TO VDU.
           MOVE "39. SPANISH DESC:" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 39 TO MAX.

       MAIN-MODULE.
           PERFORM FORM-RESET.
           PERFORM REC-GL-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P1
              MOVE 1 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE GL1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      *******************************
       REC-GL-MODULE SECTION.
      *******************************
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO GL1-KEY HOLD-GL1-KEY
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
      * GLB  06/05/03 FIX LEGENDS
      *    MOVE "F5-SCAN WINDOW, F6-SCAN NEXT, F7-EXIT" TO MESS.
           MOVE "F6-SCAN NEXT, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040GLBRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GL-MODULE.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GL-MODULE.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO REC-GL-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH HOLD-GL-BRANCH.
           IF GL-BRANCH = 0 OR GL-BRANCH > 9998
              GO TO REC-GL-BRANCH.
       REC-GL-ACCTNO.
           MOVE "ENNN070GLACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GL-MODULE.
           IF UP-KEY OR F1-KEY
              GO TO REC-GL-BRANCH.
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
           IF IO-BAD
              GO TO REC-GL-MODULE.
           MOVE GL-REC TO ORIGINAL-GL-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG
           ELSE
              MOVE "N" TO CHFG(1) CHFG(3).
       END-REC-GL-MODULE.
           EXIT.

      ***********************************
       ENTER-SCANKEY SECTION.
      ***********************************
           MOVE "ENTER GL NUMBER, F6 TO SCAN" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-GL1-FILE.
           MOVE HOLD-GL-BRANCH TO GL-BRANCH.
           MOVE 0           TO GL-ACCOUNT.
           PERFORM START-GL1-FILE.

       ENTER-KEY-ENTRY.
           MOVE "E  A110SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF F6-KEY
              GO TO READ-FILES-NEXT.
           IF NOT ENTER-KEY
              GO TO ENTER-KEY-ENTRY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE GL1-KEY TO HOLD-GL1-KEY
                 MOVE GL-REC TO ORIGINAL-GL-REC
                 MOVE "N" TO CHFG(1) CHFG(3)
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY-ENTRY.
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

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY-ENTRY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-GL1-FILE.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE                               *
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD      *
      *              1 - INITIAL NEW RECORD ENTRY    *
      *              2 - MODIFY NEW RECORD           *
      ************************************************
       ENTRY-MODULE SECTION.
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
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 2
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 NOT = 1
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
           IF ELE = 2
              MOVE LOW-VALUES TO CHKEY.

           IF ELE NOT > 2
              IF CHKEY NOT = ZEROS
                 IF CHFG(ELE) = " "
                    IF NEW-FLAG NOT = 1
                       GO TO MODIFY-ROUTINE.
           IF ELE NOT > 2
              IF CHFG(ELE) = "N"
                 IF NEW-FLAG = 1
                    GO TO CREATE-ROUTINE
                 ELSE
                    IF CHKEY NOT = CHAN
                       GO TO MODIFY-ROUTINE.

           IF (ELE > 2 AND ELE < 15)
              IF NEW-FLAG = 0
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.

           MOVE SPACES TO MESS.
           IF NEW-FLAG = 1
              MOVE "F1-CANCEL, F6 ZERO FIELDS" TO MESS.
           PERFORM SEND-LEGEND.

           IF ELE NOT < 27
              MOVE SPEC(5) TO VDU
           ELSE
              IF ELE NOT < 15
                 MOVE SPEC(4) TO VDU
              ELSE
                 IF ELE NOT < 3
                    MOVE SPEC(3) TO VDU
                 ELSE
                    MOVE SPEC(ELE) TO VDU.
           IF ELE = 39
              MOVE "E  A000" TO VDU.
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
           GO TO A-01 A-02
                 DEPENDING ON ELE.
           IF ELE NOT < 3
               AND NOT > 14
                   GO TO A-LY-DATA.
           IF ELE NOT < 15
               AND NOT > 26
                   GO TO A-LY-BUDGET.
           IF ELE NOT < 27
               AND NOT > 38
                   GO TO A-NY-BUDGET.
           IF ELE = 39
              GO TO A-39.

           GO TO END-ENTRY-MODULE.
      *********************************
      *     SET RECORD ELEMENTS       *
      *********************************
       A-01.
           MOVE VDUNUM2 TO GL-LYOPENBAL.
           PERFORM SEND-LYENDBAL.
           GO TO CREATE-ROUTINE.
       A-02.
           GO TO CREATE-ROUTINE.
       A-LY-DATA.
           SUBTRACT 2 FROM ELE GIVING SUB.
           MOVE VDUNUM2 TO GL-LYBAL(SUB).
           PERFORM SEND-LYENDBAL.
           GO TO CREATE-ROUTINE.
       A-LY-BUDGET.
           SUBTRACT 14 FROM ELE GIVING SUB.
           MOVE VDUNUM0 TO GL-LYBUD(SUB).
           GO TO CREATE-ROUTINE.
       A-NY-BUDGET.
           SUBTRACT 26 FROM ELE GIVING SUB.
           MOVE VDUNUM0 TO GL-NYBUD(SUB).
           GO TO CREATE-ROUTINE.
       A-39.
           MOVE VDUBUF TO GL-SPANISH-DESC.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.


      ****************************
       WRITE-MODULE SECTION.
      ****************************
           PERFORM OPEN-GL1-FILE.
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
           MOVE "TO "      TO  WORK-ERRLOG-TO-X.

           IF GLOR-LYOPENBAL NOT = GLCH-LYOPENBAL
             MOVE "010"                   TO WORK-ERRLOG-FIELD
             MOVE "LY OPEN "              TO WORK-ERRLOG-DESC
             MOVE GLOR-LYOPENBAL          TO WORK-ERRLOG-ORG-N2
             MOVE GLCH-LYOPENBAL          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF GLOR-LYBAL(SUB) NOT = GLCH-LYBAL(SUB)
                COMPUTE WORK-ERRLOG-FIELDN = SUB + 2
                MOVE "LYBAL &&"           TO WORK-ERRLOG-DESC
                INSPECT WORK-ERRLOG-DESC REPLACING FIRST "&&" BY SUB
                MOVE GLOR-LYBAL(SUB)      TO WORK-ERRLOG-ORG-N2
                MOVE GLCH-LYBAL(SUB)      TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF GLOR-LYBUD(SUB) NOT = GLCH-LYBUD(SUB)
                COMPUTE WORK-ERRLOG-FIELDN = SUB + 14
                MOVE "LYBUD &&"           TO WORK-ERRLOG-DESC
                INSPECT WORK-ERRLOG-DESC REPLACING FIRST "&&" BY SUB
                MOVE GLOR-LYBUD(SUB)      TO WORK-ERRLOG-ORG-N2
                MOVE GLCH-LYBUD(SUB)      TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF GLOR-NYBUD(SUB) NOT = GLCH-NYBUD(SUB)
                COMPUTE WORK-ERRLOG-FIELDN = SUB + 26
                MOVE "NYBUD &&"           TO WORK-ERRLOG-DESC
                INSPECT WORK-ERRLOG-DESC REPLACING FIRST "&&" BY SUB
                MOVE GLOR-NYBUD(SUB)      TO WORK-ERRLOG-ORG-N2
                MOVE GLCH-NYBUD(SUB)      TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

      *****************************
       LOG-FIELD-CHANGE SECTION.
      *****************************

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ******************************
       DISPLAY-FIELDS SECTION.
      ******************************
           MOVE "S  A170PROMPT39." TO VDU.
           MOVE "39. SPANISH DESC:" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE GL-DESCRIPTION  TO ADESC.
           MOVE GL-LYOPENBAL    TO A01.

           MOVE 0 TO SUB.
           PERFORM DISPLAY-FIELDS-TABLES 11 TIMES.

       DISPLAY-FIELDS-TABLES.
           ADD 1 TO SUB.
           MOVE GL-LYBAL(SUB) TO ALYDATA(SUB).
           MOVE GL-LYBUD(SUB) TO ALYBUDGET(SUB).
           MOVE GL-NYBUD(SUB) TO ANYBUDGET(SUB).
           MOVE GL-SPANISH-DESC TO SPANISH-DESC.

       DISPLAY-FIELDS-BUF.
           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-LYENDBAL.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *******************************
       ZERO-FIELDS SECTION.
      *******************************
       ZERO-FIELDS-LOOP.

           IF ELE < 15
               MOVE "S  S092000." TO VDU
           ELSE
               MOVE "S  S090000." TO VDU.

           IF ELE = 1
              MOVE 0 TO GL-LYOPENBAL VDUNUM2
           ELSE
           IF ELE NOT < 3
               AND NOT > 14
                   SUBTRACT 2 FROM ELE GIVING SUB
                   MOVE 0 TO GL-LYBAL(SUB) VDUNUM2
           IF ELE NOT < 15
               AND NOT > 26
                   SUBTRACT 14 FROM ELE GIVING SUB
                   MOVE 0 TO GL-LYBUD(SUB) VDUNUM0
                   GO TO A-LY-BUDGET.
           IF ELE NOT < 27
               AND NOT > 38
                   SUBTRACT 26 FROM ELE GIVING SUB.
                   MOVE 0 TO GL-NYBUD(SUB) VDUNUM0.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           ADD 1 TO ELE.
           IF ELE NOT > MAX
              GO TO ZERO-FIELDS-LOOP.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME          TO GLMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO GLMAN2-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD        TO CHAN.
           DISPLAY GLMAN2-SCREEN         UPON GLMAIN-WINDOW-HANDLE.
           MOVE GLMAN2-FORM-FIELDS TO WR-BUF.
           MOVE GLMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE GLMAN2-HELP--FILE          TO HELP-LINK-FILE.
           MOVE GLMAN2-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "00"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBGL/GLMAN2_EVA.CPY".
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
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LYENDBAL.
           MOVE "S NS092020." TO VDU.
           COMPUTE VDUNUM2 LYCUR-BAL = GL-LYOPENBAL
                     + GL-LYBAL(1)  + GL-LYBAL(2)  + GL-LYBAL(3)
                     + GL-LYBAL(4)  + GL-LYBAL(5)  + GL-LYBAL(6)
                     + GL-LYBAL(7)  + GL-LYBAL(8)  + GL-LYBAL(9)
                     + GL-LYBAL(10) + GL-LYBAL(11) + GL-LYBAL(12).
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A000MESS." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *******************************
       END-ROUTINE SECTION.
      *******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLMAN2-SCREEN.
           MOVE FORM-PATH                  TO FORM-PATHNAME.
           MOVE GLMAIN-LINK                TO FORM-LINK-AREA.
           EXIT PROGRAM.
