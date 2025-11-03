       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ALLENT.
       DATE-WRITTEN.
      ***********************************************************************
      *   GL ALLOCATION ENTRY   (WINDOW)
      * REV:
      *   030896 BLL DON'T ALLOW ALLOCATION WITH 0 GL ACCOUNTS
      *   102997 BLV ADD GL NUMBER ENTRY OPTION
      *   062998 BAH THE PASSWD PGM'S WERE BACKWARDS FOR ALPHA AND NUMERIC
      *              SCANS
      *   990921 BLV MOVE 0 TO ALLENT-ACCOUNT IF ALLOCATION CONTAINS AN
      *              INVALID GL #, WINDOW WAS ALLOWING F7, THEN ENTER 1
      *              & PUT IN AMT IN THIS SITUATION
      *   991101 BLV WASN'T TESTING CORRECTLY FOR BR-OTHER-BRANCH-GL WHEN
      *              SELECTING ACCOUNT # FROM GLSCAN
      *   000124 MG  FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *   000705 GLB TEST ALLEN-NO-ACCTS IF NUMERIC GL; ALLOCATION O.K.
      *   000906 BLV MOVE ALLENT-NO-ACCTS TO MAX; ENTRY WAS STILL ALLOWING
      *              ENTRY OF MORE THAN MAX & PUTTING GL OUT OF BALANCE
      *   051212 BAH DO NOT ALLOW ZERO AMOUNT, PR# 2662 CENTRIX
      *   100526 BLM ADD AUDIT-ENTRY ROUTINE.  BEFORE EXITING WINDOW,
      *              AUDIT THAT IF ANY ENTRY IN THE TABLE OF 5 HAS A
      *              NON-ZERO AMT WITH NO GL BRANCH OR GL ACCOUNT,
      *              GIVE A MESSAGE & CLEAR THE TABLE.  PRIOR TO THIS
      *              FIX, IT WAS POSSIBLE TO ENTER FIELD NUMBERS
      *              AND CHANGE ALLOCATIONS & AMOUNTS WHICH COULD
      *              CAUSE OUT OF BALANCE ENTRIES. FOR EXAMPLE, YOU
      *              COULD ENTER ALLOCATION & AMT ON LINE 1, THEN
      *              SELECT FIELD 1 & CHANGE AMT TO 0.  SELECT FIELD 3, 4,
      *              OR 5, ENTER ALLOCATION & AMT, GL # WILL DISPLAY ON
      *              LINE 2 BUT AMT WILL BE ON 3, 4, OR 5.  IN THE CHECK
      *              & TR, THE AMT WILL BE ON A LINE WITH NO GL #, AND GL
      *              WILL BE OUT OF BALANCE.  WORLD.
      ***********************************************************************
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
           "@(#)./GL/ALLENT.CBL S35 09/20/22-12:16:20 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGL/GL01AL.CPY".
       COPY "LIBGL/GL01AL_SQL.CPY".
       COPY "LIBGL/GLWSAL.CPY".
       01  ALLENT-LINES            PIC 9(02)   VALUE 10.
       01  ALLENT-COLUMNS          PIC 9(02)   VALUE 78.
       01  ALLENT-BEGIN-Y          PIC 9(02)   VALUE 10.
       01  ALLENT-BEGIN-X          PIC 9(02)   VALUE 02.
       01  ERRCD           PIC X        VALUE SPACES.

       01  LEGEND          PIC X(60).
       01  SUB             PIC 999      VALUE 0.
       01  AL-SUB          PIC 999      VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  BEGIN-ELE       PIC 9(2).
       01  MAX-ELE         PIC 9(2).
       01  ACTION.
           03  FILLER      PIC X(4).
           03  CHNO        PIC 9(2).
       01  NEW-FLAG        PIC 9.

       01  IO-TYPE         PIC X(3).
       01  FIRST-TIME      PIC 9        VALUE 0.
       01  BAD-ACCT-FG     PIC X        VALUE "N".
       01  OUT-OF-BAL-MSG-SENT PIC X    VALUE "N".
       01  AUDIT-OK        PIC X        VALUE "Y".
       01  NO-OF-ACCTS     PIC 99       VALUE 0.
       01  ENTER-AMT-FG    PIC X        VALUE "N".
       01  GL-OR-AL-FG     PIC X        VALUE "A".
       01  WORK-PROOF      PIC S9(7)V99 VALUE 0.
       01  ALLOCATION-TABLE.
           03  HOLD-ID     PIC X(12)    OCCURS 5.

       01  LEGEND-WORKERS.
           03  LEGEND-ALP-TOGGLE.
               05  FILLER        PIC X(30) VALUE
                   "F1-RESET, F3-NUMERIC ENTRY, F6".
               05  FILLER        PIC X(30) VALUE
                   "-SCAN, F7-EXIT, ENTER GL ID:".
           03  LEGEND-ALP-NO-TOGGLE.
               05  FILLER        PIC X(30) VALUE
                   "F1-RESET, F6-SCAN, F7-EXIT, EN".
               05  FILLER        PIC X(30) VALUE
                   "TER GL ID:".
           03  LEGEND-BRANCH-TOGGLE.
               05  FILLER        PIC X(30) VALUE
                   "F1-RESET, F3-ALPHA ENTRY, F6-S".
               05  FILLER        PIC X(30) VALUE
                   "CAN, F7-EXIT, ENTER BRANCH:".
           03  LEGEND-BRANCH-NO-TOGGLE.
               05  FILLER        PIC X(30) VALUE
                   "F1-RESET, F6-SCAN, F7-EXIT, EN".
               05  FILLER        PIC X(30) VALUE
                   "TER BRANCH:".
           03  LEGEND-ACCT-TOGGLE.
               05  FILLER        PIC X(30) VALUE
                   "F1-RESET, F3-ALPHA ENTRY, F6-S".
               05  FILLER        PIC X(30) VALUE
                   "CAN, F7-EXIT, ENTER ACCOUNT:".
           03  LEGEND-ACCT-NO-TOGGLE.
               05  FILLER        PIC X(30) VALUE
                   "F1-RESET, F6-SCAN, F7-EXIT, EN".
               05  FILLER        PIC X(30) VALUE
                   "TER ACCOUNT:".
           03  LEGEND-AMT.
               05  FILLER        PIC X(30) VALUE
                   "F1-RESET, ENTER GL AMOUNT".
               05  FILLER        PIC X(30) VALUE
                   SPACES.
      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF           VALUE SPACES.
           03  D-DISBURSE        PIC ZZZZZZ9.99-.
           03  D-PROOF           PIC ZZZZZZ9.99-.
           03  D-HEAD1           PIC X(12).
           03  DISPLAY-TABLE     OCCURS 5.
               05  D-LINE-NO     PIC Z.
               05  D-ID          PIC X(12).
               05  DISPLAY-ACCT.
                   07  D-BRANCH  PIC ZZZ9 BLANK ZERO.
                   07  D-ACCOUNT PIC 9(7) BLANK ZERO.
               05  D-DESC        PIC X(36).
               05  D-AMT         PIC ZZZZZZ9.99- BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(15)  VALUE
           "DISBURSE PROOF ".
           03  FILLER      PIC X(8) VALUE
           "HEAD1:S ".
           03  FILLER      PIC X(48) VALUE
           "LN1 010 011 012 013 014 LN2 020 021 022 023 024 ".
           03  FILLER      PIC X(48) VALUE
           "LN3 030 031 032 033 034 LN4 040 041 042 043 044 ".
           03  FILLER      PIC X(24) VALUE
           "LN5 050 051 052 053 054.".

       01  MAX             PIC 99   VALUE 5.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      * 1. ALLOCATION #1
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 2. ALLOCATION #2
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 3. ALLOCATION #3
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 4. ALLOCATION #4
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 5. ALLOCATION #5
           03  FILLER      PIC X(8) VALUE "E  A120 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 5 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  ALLENT-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGL/GLSCANW.CPY".
       COPY "LIBGL/ALSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/ALLENT_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGL/ALLENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBGL/ALLENTW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/ALLENT_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME ALLENT-BUF EXIT-PATHNAME.

      ***************************
      *      MAIN PROGRAM MODULE
      ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE "ALLENT" TO VDU-FORM-NAME.

      *    MOVE 10 TO WINDOW-LINES.
      *    MOVE 78 TO WINDOW-COLUMNS.
      *    MOVE 10 TO WINDOW-BEGIN-Y.
      *    MOVE 2 TO WINDOW-BEGIN-X.

      *    PERFORM OPEN-WINDOW.
      *    DISPLAY ALLENT-SCREEN.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE ALLENT-BEGIN-Y
                   AT COL  ALLENT-BEGIN-X
                   SIZE    ALLENT-COLUMNS + VDU-WIN-W-MOD
                   LINES   ALLENT-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS ALLENT-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF ALLENT-GL-ENTRY-DEFAULT = "A" OR "N"
              MOVE ALLENT-GL-ENTRY-DEFAULT TO GL-OR-AL-FG.
           IF ALLENT-GL-TYPE-ALLOWED NOT = "B"
              IF ALLENT-GL-ENTRY-DEFAULT NOT = ALLENT-GL-TYPE-ALLOWED
                 MOVE "A" TO GL-OR-AL-FG.
           MOVE ALLENT-NO-OF-ACCTS TO MAX.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-AL1-FILE.
       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
            ELSE
              PERFORM FORM-RESET.

           PERFORM ENTRY-MODULE.
           PERFORM AUDIT-ENTRY.
           IF AUDIT-OK = "N"
              MOVE "GL ENTRY PROBLEM; PLEASE RE-ENTER!"
                TO MESS
              PERFORM SEND-MESS
              GO TO MAIN-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           PERFORM CHECK-PROOF.

           GO TO END-ROUTINE.

       MAIN-MODULE-EXIT.
           EXIT.

      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
       BEGIN-ENTRY.
           MOVE 1 TO NEW-FLAG.
           MOVE 0 TO NO-OF-ACCTS.
           MOVE 0 TO SUB.
           PERFORM CLEAR-GL-TABLE.
           MOVE ALLENT-DISBURSE TO WORK-PROOF.

           PERFORM DISPLAY-FIELDS.

           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           MOVE "N" TO ENTER-AMT-FG.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "F1-RESET, F7-EXIT, ENTER-ACCEPT, ENTER FIELD #: "
                          TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZERO.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
           IF GL-OR-AL-FG = "A"
              IF ALLENT-ACCOUNT(ELE) NOT = 0
                 MOVE "Y" TO ENTER-AMT-FG.
       ENTER-ELE.
           IF GL-OR-AL-FG = "N"
              IF ALLENT-OTHER-BR-FG = "N"
                 IF ALLENT-GL-TYPE-ALLOWED = "B"
                    MOVE LEGEND-ACCT-TOGGLE TO LEGEND
                 ELSE
                    MOVE LEGEND-ACCT-NO-TOGGLE TO LEGEND
              ELSE
                 IF ALLENT-GL-TYPE-ALLOWED = "B"
                    MOVE LEGEND-BRANCH-TOGGLE TO LEGEND
                 ELSE
                    MOVE LEGEND-BRANCH-NO-TOGGLE TO LEGEND
           ELSE
              IF ENTER-AMT-FG = "N"
                 IF ALLENT-GL-TYPE-ALLOWED = "B"
                    MOVE LEGEND-ALP-TOGGLE TO LEGEND
                 ELSE
                    MOVE LEGEND-ALP-NO-TOGGLE TO LEGEND
              ELSE
                 MOVE LEGEND-AMT TO LEGEND.

           PERFORM SEND-LEGEND.

           IF GL-OR-AL-FG = "N"
              IF ALLENT-OTHER-BR-FG = "N"
                 MOVE "SN N040011." TO VDU
                 MOVE ELE TO VDUROW
                 MOVE 1 TO VDUCOL
                 MOVE "." TO VDUDECIMAL
                 MOVE ALLENT-BRNO TO VDUNUM0 GL-BRANCH
                                     ALLENT-BRANCH(ELE)
                                     D-BRANCH(ELE)
              ELSE
                 MOVE "ENNN040001." TO VDU
                 MOVE ELE TO VDUROW
                 MOVE 1 TO VDUCOL
                 MOVE "." TO VDUDECIMAL
           ELSE
              IF ENTER-AMT-FG = "N"
                 MOVE SPEC(ELE) TO VDU
                 MOVE ELE TO VDUROW
                 MOVE 0 TO VDUCOL
                 MOVE "." TO VDUDECIMAL
              ELSE
                 MOVE "ENNS072000." TO VDU
                 MOVE ELE TO VDUROW
                 MOVE 4 TO VDUCOL
                 MOVE "." TO VDUDECIMAL.

           PERFORM SCREENIO.
       TEST-F1.
           IF F1-KEY
              GO TO BEGIN-ENTRY.
       TEST-UP.
      *    IF UP-KEY
      *       MOVE -1 TO ONE
      *       GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F3.
           IF NOT F3-KEY
              GO TO TEST-F6.
           IF ALLENT-GL-TYPE-ALLOWED NOT = "B"
              GO TO ENTER-ELE.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO ENTER-ELE
           ELSE
              GO TO BEGIN-ENTRY.
       TEST-F6.
           IF F6-KEY
              GO TO GOTO-FIELD.
       TEST-F7.
           IF F7-KEY
              IF WORK-PROOF = 0
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "WARNING: PROOF IS NOT ZERO!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO OUT-OF-BAL-MSG-SENT
                 MOVE 2 TO NEW-FLAG
                 GO TO MODIFY-ROUTINE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.
       GOTO-FIELD.
           MOVE ELE TO SUB.
           IF ENTER-AMT-FG = "Y"
              ADD ALLENT-AMT(ELE) TO WORK-PROOF
              MOVE VDUNUM2 TO ALLENT-AMT(ELE) D-AMT(ELE)
              SUBTRACT ALLENT-AMT(ELE) FROM WORK-PROOF
              MOVE "SN S072PROOF." TO VDU
              MOVE WORK-PROOF TO VDUNUM2
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           IF GL-OR-AL-FG = "N"
              GO TO F-GL F-GL F-GL F-GL F-GL
                 DEPENDING ON ELE
           ELSE
              GO TO F-AL F-AL F-AL F-AL F-AL
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       F-GL.
           IF ALLENT-OTHER-BR-FG = "N"
              GO TO ENTER-ACCOUNT.
           IF F6-KEY
              PERFORM GL-NUMBER-SCAN
              IF GLSCAN-STATUS = "00"
                 GO TO READ-GL
              ELSE
                 GO TO ENTER-ELE.

      * GLB 07/05/00
           IF NO-OF-ACCTS > ALLENT-NO-OF-ACCTS
              MOVE "TOO MANY ACCOUNTS; RE-ENTER!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUNUM0 TO GL-BRANCH ALLENT-BRANCH(SUB)
                           D-BRANCH(SUB).
           IF VDUNUM0 = 0
              IF WORK-PROOF = 0
                 MOVE 2 TO NEW-FLAG
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO ENTER-ELE.
           IF ALLENT-OTHER-BR-FG = "N"
              IF GL-BRANCH NOT = ALLENT-BRNO
                 MOVE "CANNOT DISBURSE TO OTHER BRANCH" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       ENTER-ACCOUNT.
           MOVE "ENNN070 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 2 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF F1-KEY
              GO TO BEGIN-ENTRY.
           IF F3-KEY
              IF ALLENT-GL-TYPE-ALLOWED NOT = "B"
                 GO TO ENTER-ACCOUNT
              ELSE
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO ENTER-ACCOUNT
                 ELSE
                    GO TO BEGIN-ENTRY.
           IF F6-KEY
              PERFORM GL-NUMBER-SCAN
              IF GLSCAN-STATUS = "00"
                 GO TO READ-GL
              ELSE
                 GO TO ENTER-ELE.

           IF F7-KEY
              IF WORK-PROOF = 0
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "WARNING: PROOF IS NOT ZERO!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO OUT-OF-BAL-MSG-SENT
                 MOVE 2 TO NEW-FLAG
                 GO TO MODIFY-ROUTINE.
           IF NOT ENTER-KEY
              IF NOT DOWN-KEY
                 GO TO ENTER-ACCOUNT.
           MOVE VDUNUM0 TO GL-ACCOUNT ALLENT-ACCOUNT(SUB)
                           D-ACCOUNT(SUB).

       READ-GL.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID GL ACCOUNT; PLEASE RE-ENTER!"
                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE "S  A000013." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 3 TO VDUCOL.
           MOVE GL-DESCRIPTION TO VDUBUF D-DESC(SUB).
           PERFORM SCREENIO.

       ENTER-AMOUNT.
           IF ALLENT-OTHER-BR-FG = "N"
              MOVE "^-ACCOUNT #, F1-RESET, ENTER GL AMOUNT:"
                          TO LEGEND
           ELSE
              MOVE "^-BRANCH #, F1-RESET, ENTER GL AMOUNT:"
                          TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNS072004." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 4 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO BEGIN-ENTRY.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO ENTER-AMOUNT.
      * DO NOT ALLOW ZERO OR RETURN, PR# 2662 CENTRIX
           IF VDUNUM2 = 0
              GO TO ENTER-AMOUNT.

           ADD ALLENT-AMT(ELE) TO WORK-PROOF.
           MOVE VDUNUM2 TO ALLENT-AMT(ELE) D-AMT(ELE).
           SUBTRACT ALLENT-AMT(ELE) FROM WORK-PROOF.
           MOVE "SN S072PROOF." TO VDU.
           MOVE WORK-PROOF TO VDUNUM2.
           PERFORM SCREENIO.

      * GLB 07/05/00
           ADD 1 TO NO-OF-ACCTS.

           IF NO-OF-ACCTS NOT < ALLENT-NO-OF-ACCTS
              OR WORK-PROOF = 0
              MOVE 2 TO NEW-FLAG.
           GO TO CREATE-ROUTINE.

       F-AL.
           IF F6-KEY
              PERFORM VERIFY-PASSWORD-FOR-SCAN
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE
              ELSE
                 MOVE SPACES             TO ALSCAN-KEY
                 MOVE ALLENT-NO-OF-ACCTS TO ALSCAN-MAX-ACCTS
                 MOVE "WI/ALSCAN" TO FORM-NAM.
                 CALL FORM-PROGX USING FORM-PATH ALSCAN-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 IF ALSCAN-STATUS NOT = "00"
                    GO TO ENTER-ELE
                 ELSE
                    MOVE "S  A000000." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE ALSCAN-KEY TO VDUBUF
                    PERFORM SCREENIO.
           MOVE VDUBUF TO AL-ID HOLD-ID(SUB) D-ID(SUB).
           IF VDUBUF = SPACES
              IF WORK-PROOF = 0
                 MOVE 2 TO NEW-FLAG
                 GO TO CREATE-ROUTINE.
           PERFORM READ-AL1-FILE.
           IF IO-BAD
              MOVE "INVALID GL ID; PLEASE RE-ENTER!"
                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF AL-NO-ACCTS = 0
              MOVE "ALLOCATION HAS NO ACCOUNTS; RE-ENTER GL ID!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF (AL-NO-ACCTS + NO-OF-ACCTS) > ALLENT-NO-OF-ACCTS
              MOVE "TOO MANY ACCOUNTS; RE-ENTER GL ID!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE 0 TO SUB.
           IF AL-TYPE = "P"
              PERFORM ALLOCATE-PERCENTS
           ELSE
              PERFORM ALLOCATE-AMTS.
           IF IO-TYPE = "CLR"
              GO TO BEGIN-ENTRY.
           IF BAD-ACCT-FG = "Y"
              MOVE "GL ID CONTAINS INVALID ACCOUNTS; PLEASE RE-ENTER!"
                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF BAD-ACCT-FG = "O"
              MOVE "CANNOT DISBURSE TO OTHER BRANCH; PLEASE RE-ENTER!"
                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NO-OF-ACCTS > ALLENT-NO-OF-ACCTS
              OR WORK-PROOF = 0
                 MOVE 2 TO NEW-FLAG.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
       EXIT-ENTRY-MODULE.
           EXIT.

      *************************************************
       GL-NUMBER-SCAN SECTION.
      *************************************************
           PERFORM VERIFY-PASSWORD-FOR-SCAN.
           IF PASSWDW-STAT NOT = "OK"
               MOVE "99" TO GLSCAN-STATUS
               GO TO EXIT-GL-NUMBER-SCAN.

      *    MOVE "010." TO WINDOW-FIT-TO.

           MOVE ALLENT-BRNO TO GLSCAN-BRANCH.
           MOVE 0           TO GLSCAN-ACCOUNT.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-ACCOUNT = 0
              GO TO EXIT-GL-NUMBER-SCAN.

           IF ALLENT-OTHER-BR-FG = "N"
              IF GLSCAN-BRANCH NOT = ALLENT-BRNO
                 MOVE "CANNOT DISBURSE TO OTHER BRANCH" TO MESS
                 PERFORM SEND-MESS
                 MOVE "99" TO GLSCAN-STATUS
                 GO TO EXIT-GL-NUMBER-SCAN.

           MOVE "SN N040011." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-BRANCH TO VDUNUM0 GL-BRANCH
                                 ALLENT-BRANCH(SUB)
                                 D-BRANCH(SUB).
           PERFORM SCREENIO.
           MOVE "SN N070012." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-ACCOUNT TO VDUNUM0 GL-ACCOUNT
                                  ALLENT-ACCOUNT(SUB)
                                  D-ACCOUNT(SUB).
           PERFORM SCREENIO.
       EXIT-GL-NUMBER-SCAN.
           EXIT.

      *************************************************
       ALLOCATE-PERCENTS SECTION.
      *************************************************
           MOVE "N" TO BAD-ACCT-FG.
           MOVE 0 TO AL-SUB.
       ALLOCATE-PCT.
           COMPUTE SUB = NO-OF-ACCTS + 1.
           ADD 1 TO AL-SUB.
           IF AL-SUB > AL-NO-ACCTS
              GO TO ALLOC-END.
           IF SUB > ALLENT-NO-OF-ACCTS
              GO TO ALLOC-END.
           IF AL-ACCT(AL-SUB) = 0
              OR AL-PCT(AL-SUB) = 0
                 GO TO ALLOCATE-PCT.
           MOVE AL-ACCT(AL-SUB) TO ALLENT-BR-ACCOUNT(SUB) GL-BR-ACCOUNT.
           IF ALLENT-BRANCH(SUB) = 0
              MOVE ALLENT-BRNO TO ALLENT-BRANCH(SUB) GL-BRANCH.
           IF ALLENT-BRNO NOT = GL-BRANCH
              IF ALLENT-OTHER-BR-FG = "N"
                 MOVE "O" TO BAD-ACCT-FG
                 COMPUTE NO-OF-ACCTS = NO-OF-ACCTS - AL-SUB + 1
                 MOVE 0 TO ALLENT-BR-ACCOUNT(SUB)
                 GO TO ALLOC-PERCENT-END.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO BAD-ACCT-FG
              COMPUTE NO-OF-ACCTS = NO-OF-ACCTS - AL-SUB + 1
              MOVE 0 TO ALLENT-BR-ACCOUNT(SUB)
              GO TO ALLOC-PERCENT-END.
           MOVE GL-BRANCH TO D-BRANCH(SUB).
           MOVE GL-ACCOUNT TO D-ACCOUNT(SUB).
           MOVE GL-DESCRIPTION TO D-DESC(SUB).
           COMPUTE ALLENT-AMT(SUB) ROUNDED =
              AL-PCT(AL-SUB) * ALLENT-DISBURSE / 100.
           MOVE ALLENT-AMT(SUB) TO D-AMT(SUB).
           SUBTRACT ALLENT-AMT(SUB) FROM WORK-PROOF.
           ADD 1 TO NO-OF-ACCTS.
           GO TO ALLOCATE-PCT.
       ALLOC-END.
           IF WORK-PROOF NOT = 0
              ADD WORK-PROOF TO ALLENT-AMT(1)
              MOVE ALLENT-AMT(1) TO D-AMT(1).

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       ALLOC-PERCENT-END.
           EXIT.

      ********************************************
       ALLOCATE-AMTS SECTION.
      ********************************************
           MOVE "N" TO BAD-ACCT-FG.
           MOVE 0 TO AL-SUB.
       GET-AL-GLNO.
           COMPUTE SUB = NO-OF-ACCTS + 1.
           ADD 1 TO AL-SUB.
           IF AL-SUB > AL-NO-ACCTS
              GO TO GET-AL-GLNO-END.
           IF SUB > ALLENT-NO-OF-ACCTS
              GO TO GET-AL-GLNO-END.
           IF AL-ACCT(AL-SUB) = 0
              GO TO GET-AL-GLNO.
           MOVE AL-ACCT(AL-SUB) TO ALLENT-BR-ACCOUNT(SUB) GL-BR-ACCOUNT.
           IF ALLENT-BRANCH(SUB) = 0
              MOVE ALLENT-BRNO TO ALLENT-BRANCH(SUB) GL-BRANCH.
           IF ALLENT-BRNO NOT = GL-BRANCH
              IF ALLENT-OTHER-BR-FG = "N"
                 MOVE "O" TO BAD-ACCT-FG
                 COMPUTE NO-OF-ACCTS = NO-OF-ACCTS - AL-SUB + 1
                 MOVE 0 TO ALLENT-BR-ACCOUNT(SUB)
                 GO TO ALLOCATE-AMTS-END.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO BAD-ACCT-FG
              COMPUTE NO-OF-ACCTS = NO-OF-ACCTS - AL-SUB + 1
              MOVE 0 TO ALLENT-BR-ACCOUNT(SUB)
              GO TO ALLOCATE-AMTS-END.
           MOVE GL-BRANCH TO D-BRANCH(SUB).
           MOVE GL-ACCOUNT TO D-ACCOUNT(SUB).
           MOVE GL-DESCRIPTION TO D-DESC(SUB).
           MOVE 0 TO ALLENT-AMT(SUB) D-AMT(SUB).
           ADD 1 TO NO-OF-ACCTS.
           GO TO GET-AL-GLNO.
       GET-AL-GLNO-END.
           MOVE WORK-PROOF TO D-PROOF.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM ENTER-AMTS.

       ALLOCATE-AMTS-END.
           EXIT.

      *****************************
       ENTER-AMTS SECTION.
      *****************************
           MOVE ELE TO BEGIN-ELE.
           COMPUTE MAX-ELE = BEGIN-ELE + AL-NO-ACCTS - 1.
           MOVE "F1-RESET, ENTER GL AMOUNT:" TO LEGEND.
           PERFORM SEND-LEGEND.
       ENTER-AMTS-LOOP.
           MOVE "ENNS072002." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 4 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-AMTS.
           IF UP-KEY
              IF ELE > BEGIN-ELE
                 SUBTRACT 1 FROM ELE
                 GO TO ENTER-AMTS-LOOP.
           IF DOWN-KEY
              IF ELE < MAX-ELE
                 ADD 1 TO ELE
                 GO TO ENTER-AMTS-LOOP.
           IF NOT ENTER-KEY
              GO TO ENTER-AMTS-LOOP.
      * DO NOT ALLOW 0 AMOUNT, CENTRIX PR# 2662
           IF VDUNUM2 = 0
              GO TO ENTER-AMTS-LOOP.
           ADD ALLENT-AMT(ELE) TO WORK-PROOF.
           MOVE VDUNUM2 TO ALLENT-AMT(ELE) D-AMT(ELE).
           SUBTRACT ALLENT-AMT(ELE) FROM WORK-PROOF.
           MOVE "SN S072PROOF." TO VDU.
           MOVE WORK-PROOF TO VDUNUM2.
           PERFORM SCREENIO.
           IF ELE < MAX-ELE
              ADD 1 TO ELE
              GO TO ENTER-AMTS-LOOP.
       END-ENTER-AMTS.
           MOVE MAX-ELE TO ELE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE ALLENT-DISBURSE TO D-DISBURSE.
           MOVE WORK-PROOF TO D-PROOF.
           IF GL-OR-AL-FG = "A"
              MOVE "GL ALPHA ID" TO D-HEAD1
           ELSE
              MOVE SPACES TO D-HEAD1.
           MOVE 1 TO SUB.
       DISPLAY-TABLE.
           IF SUB NOT > ALLENT-NO-OF-ACCTS
              MOVE SUB TO D-LINE-NO(SUB).
           MOVE ALLENT-BRANCH(SUB) TO D-BRANCH(SUB).
           MOVE ALLENT-ACCOUNT(SUB) TO D-ACCOUNT(SUB).
           MOVE ALLENT-BR-ACCOUNT(SUB) TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-GOOD
              MOVE GL-DESCRIPTION TO D-DESC(SUB).
           MOVE ALLENT-AMT(SUB) TO D-AMT(SUB).
           ADD 1 TO SUB.
           IF SUB < 6 GO TO DISPLAY-TABLE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************
       AUDIT-ENTRY SECTION.
      *****************************

      * AUDIT GL TABLE OF ACCTS & AMTS
           MOVE "Y" TO AUDIT-OK.
           MOVE 0 TO SUB.
       AUDIT-TABLE.
           ADD 1 TO SUB.
           IF SUB > 5
              GO TO EXIT-AUDIT-TABLE.

           IF ALLENT-ACCOUNT(SUB) = 0
              IF ALLENT-AMT(SUB) NOT = 0
                 MOVE 0 TO ALLENT-BR-ACCOUNT(SUB)
                 MOVE 0 TO ALLENT-AMT(SUB)
                 MOVE "N" TO AUDIT-OK
                 GO TO EXIT-AUDIT-TABLE.
           IF ALLENT-BRANCH(SUB) = 0
              IF ALLENT-AMT(SUB) NOT = 0
                 MOVE 0 TO ALLENT-BR-ACCOUNT(SUB)
                 MOVE 0 TO ALLENT-AMT(SUB)
                 MOVE "N" TO AUDIT-OK
                 GO TO EXIT-AUDIT-TABLE.

           GO TO AUDIT-TABLE.
       EXIT-AUDIT-TABLE.
           EXIT.

      *****************************
       CLEAR-GL-TABLE SECTION.
      *****************************
           MOVE SPACES TO ALLOCATION-TABLE.
           MOVE 1 TO SUB.
       CLEAR-TABLE.
           MOVE 0 TO ALLENT-BR-ACCOUNT(SUB) ALLENT-AMT(SUB).
           ADD 1 TO SUB.
           IF SUB < 6 GO TO CLEAR-TABLE.

      *****************************
       CHECK-PROOF SECTION.
      *****************************
           MOVE 1 TO SUB.
           MOVE 0 TO WORK-PROOF.
       CHECK-PROOF-LOOP.
           ADD ALLENT-AMT(SUB) TO WORK-PROOF.
           ADD 1 TO SUB.
           IF SUB < 6 GO TO CHECK-PROOF-LOOP.
           IF WORK-PROOF = ALLENT-DISBURSE
              MOVE "Y" TO ALLENT-STATUS
           ELSE
              MOVE "N" TO ALLENT-STATUS
              IF OUT-OF-BAL-MSG-SENT = "N"
                 MOVE "WARNING: PROOF IS NOT ZERO!" TO MESS
                 PERFORM SEND-MESS.

      *****************************
       VERIFY-PASSWORD SECTION.
      *****************************
           IF GL-OR-AL-FG = "N"
              MOVE "GLALP" TO PASSWDW-PGM
           ELSE
              MOVE "GLNUM" TO PASSWDW-PGM.
           MOVE " " TO PASSWDW-STAT.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           IF GL-OR-AL-FG = "N"
              MOVE "FOR GL ALPHA ID ENTRY:" TO PASSWDW-PROMPT2
           ELSE
              MOVE "FOR GL NUMBER ENTRY:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
           IF PASSWDW-STAT = "OK"
              IF GL-OR-AL-FG = "N"
                 MOVE "A" TO GL-OR-AL-FG
              ELSE
                 MOVE "N" TO GL-OR-AL-FG.

      *********************************************
       VERIFY-PASSWORD-FOR-SCAN SECTION.
      *********************************************
           IF GL-OR-AL-FG = "N"
              MOVE "GLNUM-SCAN" TO PASSWDW-PGM
           ELSE
              MOVE "GLALP-SCAN" TO PASSWDW-PGM.
           MOVE " " TO PASSWDW-STAT.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           IF GL-OR-AL-FG = "N"
              MOVE "FOR GL ACCOUNT NUMBER SCAN:" TO PASSWDW-PROMPT2
           ELSE
              MOVE "FOR GL ALPHA ID SCAN:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
      *    DISPLAY OMITTED ERASE SCREEN.
      *    DISPLAY ALLENT-SCREEN.
      *    MOVE ALLENT-FORM-FIELDS TO WR-BUF.
      *    MOVE ALLENT-FORM-LIST   TO WR-LIST.
      *    PERFORM CALL-WRFORM.

           DISPLAY ALLENT-SCREEN UPON ALLENT-WINDOW-HANDLE.
           MOVE ALLENT-FORM-FIELDS TO WR-BUF.
           MOVE ALLENT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE ALLENT-HELP--FILE TO HELP-LINK-FILE.
           MOVE ALLENT-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/ALLENT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************
       IO-ROUTINE SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-AL1-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLALGS_SQL.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGL/GLAL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************
       END-ROUTINE SECTION.
      ***************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALLENT-SCREEN.
           DESTROY ALLENT-WINDOW-HANDLE.
           EXIT PROGRAM.
