       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BROPM2.
      ******************************************************************
      *
      *  DIR :   GB
      *  DESC:   COMENU HIDDEN BRANCH FILE MAINT PAGE 2
      *
      * REV :
      *  BAH 020405 NEW PAGE 2
      *  JTG 040219 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      *              ADDED 3. ALLOW NON MONTHLY CAN LOANS
      *  BAH 040726 ADDED BR-FILE FOR BR-ALT-INTO-THIS-BRANCH, SOAUTO PR#2327
      *  BAH 060720 ADDED BR-POFF-TITLE-WINDOW, REGACC PR# 137
      *  AMM 070308 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 120514 REMOVED MAXPRU NONDISPLAY
      *  BAH 130930 ADDED BR-WIMENU-CHECK-WRITING, BAY COUNTRY PR# 4379
      *  BAH 151021 REMOVED B2FILE, PD0003
      *  BLM 160914 ADDED BR-ALLOW-SCANNED-CKS, WORLD PR# 1188
      *  BLM 160914 UNSTAR LINE THAT WENT TO END-WRITE-MODULE, THIS
      *             PAGE WOULD ALWAYS SAY "ALREADY CREATED"
      *  BAH 180214 ADDED 8. BR-USE-DEBIT-CARDS AND
      *             9. BR-GL-DEBIT-CARD-PROCEEDS, NETSPEND WORLD PR#1306
      * KEC 181009 PL#989 PARADATA A15 JAY CISZEWSKI
      *            FIXED AN ISSUE WITH ENTER-SCANKEY SECTION WHEN USING THE
      *            F1-KEY WHEN FIRST COMING INTO IT (BEFORE DOING A READ-NEXT).
      *            IT WOULD FLOW INTO ENTRY-MODULE WITHOUT HAVE READ A GP-REC.
      *            IN MAIN-MODULE, ADDED IF CONDITION ON IO-TYPE FOR "CLR"
      *            AFTER 'PERFORM REC-BR-MODULE' WHICH IF TRUE THEN RESET
      * BAH 2023.0419 ADDED 10. BR-TEST-PP-AUTO-INSCOVR #1594A
      * BAH 2023.0727 ADDED 11. BR-BRANCH-CLOSED #1618
      * BAH 2023.0727 ADDED 11. BR-BRANCH-CLOSED #1618
      * BAH 2023.1110 ADDED 12. BR-ALLOW-F3-ALL-DOCS #1630
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./GB/BROPM2.CBL S35 09/20/22-13:20:28 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BROR".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BRCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.

      ******************************************************************
      *-----------------------------------------------------------------

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.
       01  IO-TYPE         PIC X(3)    VALUE SPACES.
       01  NEW-FLAG        PIC 9       VALUE 0.

       01  ORIGINAL-BR-REC    PIC X(BR-SIZE).
       01  CHANGED-BR-REC     PIC X(BR-SIZE).

       01  UPDATE-BR-STAT            PIC X(02) VALUE SPACES.
           88  UPDATE-BR-STAT-GOOD             VALUE "00".
           88  UPDATE-BR-STAT-BAD              VALUE "  " THRU "//"
                                                     "01" THRU "~~".

       01  BROPMN-LINK     VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BROPMN-WINDOW-HANDLE PIC X(10).

       01  WORK-GL         PIC 9(11).
       01  WORK-NUMBER REDEFINES WORK-GL.
           03  WORK-BR     PIC 9(4).
           03  WORK-NO     PIC 9(7).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  BRNAME      PIC X(20).
           03  BRSTATE     PIC X(2).
           03  F01         PIC X.
           03  F02         PIC X.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC X.
           03  F08         PIC X.
           03  F09         PIC 9(4).
           03  F091        PIC 9(7).
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC X.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(43) VALUE
           "BRNAME BRSTATE 010 020 030 040 050 060 070 ".
           03  FILLER      PIC X(24) VALUE
           "080 090 091 100 110 120.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S - # 1
      *
      ******************************************************************
       78  MAX                      VALUE 12.
      *-----------------------------------------------------------------
       01  SPEC-TABLE.
      * 1. BR-GLOBAL-QUEUE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. BR-FORCE-VALID-CITY-COUTY
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. BR-NON-MONTHLY-CAN-LOANS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. BR-ALT-INTO-THIS-BRANCH
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5. BR-POFF-TITLE-WINDOW
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. BR-WIMENU-CHECK-WRITING
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 7. BR-ALLOW-SCANNED-CKS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 8. BR-USE-DEBIT-CARDS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 9. BR-GL-DEBIT-CARD-PROCEEDS
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *10. BR-TEST-PP-AUTO-INSCOVR
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *11. BR-BRANCH-CLOSED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *12. BR-ALLOW-F3-ALL-DOCS
           03  FILLER      PIC X(8) VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BROPM2_DEF.CPY".
       COPY "LIBGB/BROPM2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BROPM2_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BROPM2" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO BROPMN-LINK.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-BR-MODULE.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-MODULE.

           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE BR-NO TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BR-MODULE SECTION.
      ******************************************************************
       REC-BR-MODULE-NO.

           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BR-NO
              MOVE " " TO SAVEKEY
              MOVE "S  A040BRANCH." TO VDU
              MOVE BR-NO TO VDUBUF
              PERFORM SCREENIO
              GO TO READ-BRANCH.

           MOVE 0 TO NEW-FLAG.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BR-MODULE.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BR-MODULE.

           IF NOT ENTER-KEY
              GO TO REC-BR-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-BR-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO.

       READ-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-GOOD
              MOVE BR-REC TO ORIGINAL-BR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
            ELSE
              PERFORM BRANCH-NOT-FOUND
              GO TO REC-BR-MODULE-NO.

      *-----------------------------------------------------------------
       END-REC-BR-MODULE.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.

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

           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 1 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
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

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.

      *-----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12
           DEPENDING ON ELE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
      * 1. BR-GLOBAL-QUEUE
           MOVE VDUBUF TO BR-GLOBAL-QUEUE.
           IF NOT (BR-GLOBAL-QUEUE = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BR-FORCE-VALID-CITY-COUNTY.
           IF NOT ( BR-FORCE-VALID-CITY-COUNTY = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BR-NON-MONTHLY-CAN-LOANS.
           IF NOT (BR-NON-MONTHLY-CAN-LOANS = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BR-ALT-INTO-THIS-BRANCH.
           IF NOT (BR-ALT-INTO-THIS-BRANCH = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO BR-POFF-TITLE-WINDOW.
           IF NOT (BR-POFF-TITLE-WINDOW = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO BR-WIMENU-CHECK-WRITING.
           IF NOT (BR-WIMENU-CHECK-WRITING = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO BR-ALLOW-SCANNED-CKS.
           IF NOT (BR-ALLOW-SCANNED-CKS = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO BR-USE-DEBIT-CARDS.
           IF NOT (BR-USE-DEBIT-CARDS = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM0 TO GL-BRANCH WORK-BR.
       F-091.
           MOVE "ENNN070091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-091.
           MOVE VDUNUM0 TO GL-ACCOUNT WORK-NO.
           MOVE WORK-GL TO BR-GL-DEBIT-CARD-PROCEEDS.
       F-09-CHECK-GL.
           IF BR-GL-DEBIT-CARD-PROCEEDS NOT = 0
              MOVE BR-GL-DEBIT-CARD-PROCEEDS TO GL-BR-ACCOUNT
              PERFORM OPEN-GL1-FILE
              PERFORM READ-GL1-FILE
              PERFORM CLOSE-GL1-FILE
              IF IO-BAD
                 MOVE "NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO BR-TEST-PP-AUTO-INSCOVR.
           IF NOT (BR-TEST-PP-AUTO-INSCOVR = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUBUF TO BR-BRANCH-CLOSED.
           IF NOT (BR-BRANCH-CLOSED = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDUBUF TO BR-ALLOW-F3-ALL-DOCS.
           IF NOT (BR-ALLOW-F3-ALL-DOCS = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           MOVE STAT---BAD TO UPDATE-BR-STAT.

       REWRITE-RECORD.

           PERFORM OPEN-BR-FILE.

           MOVE BR-REC TO CHANGED-BR-REC.

           IF ORIGINAL-BR-REC = CHANGED-BR-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BR-FILE.

           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BR-REC NOT = ORIGINAL-BR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-BR-REC TO BR-REC.
           MOVE "N" TO BR-UNITPER-Y-D-FG.
      *    MOVE "N" TO BR-UNITPER-FG
      *                BR-UNITPER-Y-D-FG.
           PERFORM REWRITE-BR-FILE.
           MOVE STAT---GOOD TO UPDATE-BR-STAT.

      * CODE TO WRITE CHANGE FILE.
      *    MOVE "C" TO ERRLOGW-ACTION.
      *    MOVE BR-NO TO ERRLOGW-KEY.
      *    MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
      *    CANCEL FORM-PROGX.
           GO TO END-WRITE-MODULE.

      *-----------------------------------------------------------------
       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

           IF ( UPDATE-BR-STAT-GOOD )
              PERFORM LOG-MAINTENANCE-CHGS.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE ORIGINAL-BR-REC               TO BROR-REC.
           MOVE CHANGED-BR-REC                TO BRCH-REC.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-GLOBAL-QUEUE NOT = BRCH-GLOBAL-QUEUE
             MOVE "01 "                   TO LMC-FIELD
             MOVE "GLB QUEUE"             TO LMC-DESC
             MOVE BROR-GLOBAL-QUEUE       TO LMC-ORG-TEXT
             MOVE BRCH-GLOBAL-QUEUE       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-FORCE-VALID-CITY-COUNTY NOT =
              BRCH-FORCE-VALID-CITY-COUNTY
             MOVE "02 "                        TO LMC-FIELD
             MOVE "INS TAX  "                  TO LMC-DESC
             MOVE BROR-FORCE-VALID-CITY-COUNTY TO LMC-ORG-TEXT
             MOVE BRCH-FORCE-VALID-CITY-COUNTY TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NON-MONTHLY-CAN-LOANS NOT =
              BRCH-NON-MONTHLY-CAN-LOANS
             MOVE "03 "                        TO LMC-FIELD
             MOVE "NON MONTH"                  TO LMC-DESC
             MOVE BROR-NON-MONTHLY-CAN-LOANS   TO LMC-ORG-TEXT
             MOVE BRCH-NON-MONTHLY-CAN-LOANS   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALT-INTO-THIS-BRANCH NOT = BRCH-ALT-INTO-THIS-BRANCH
             MOVE "04 "                       TO LMC-FIELD
             MOVE "ALT IN BR"                 TO LMC-DESC
             MOVE BROR-ALT-INTO-THIS-BRANCH   TO LMC-ORG-TEXT
             MOVE BRCH-ALT-INTO-THIS-BRANCH   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-POFF-TITLE-WINDOW NOT = BRCH-POFF-TITLE-WINDOW
             MOVE "05 "                   TO LMC-FIELD
             MOVE "PAYOFF TW"             TO LMC-DESC
             MOVE BROR-POFF-TITLE-WINDOW  TO LMC-ORG-TEXT
             MOVE BRCH-POFF-TITLE-WINDOW  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-WIMENU-CHECK-WRITING NOT = BRCH-WIMENU-CHECK-WRITING
             MOVE "06 "                   TO LMC-FIELD
             MOVE "WIMENU CHK"            TO LMC-DESC
             MOVE BROR-WIMENU-CHECK-WRITING  TO LMC-ORG-TEXT
             MOVE BRCH-WIMENU-CHECK-WRITING  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALLOW-SCANNED-CKS NOT = BRCH-ALLOW-SCANNED-CKS
             MOVE "07 "                   TO LMC-FIELD
             MOVE "SCANED CKS"            TO LMC-DESC
             MOVE BROR-ALLOW-SCANNED-CKS  TO LMC-ORG-TEXT
             MOVE BRCH-ALLOW-SCANNED-CKS  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-USE-DEBIT-CARDS NOT = BRCH-USE-DEBIT-CARDS
             MOVE "08 "                   TO LMC-FIELD
             MOVE "DEBITCARD"             TO LMC-DESC
             MOVE BROR-USE-DEBIT-CARDS  TO LMC-ORG-TEXT
             MOVE BRCH-USE-DEBIT-CARDS  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GL-DEBIT-CARD-PROCEEDS NOT =
               BRCH-GL-DEBIT-CARD-PROCEEDS
             MOVE "09 "                   TO LMC-FIELD
             MOVE "DEBITGLNO"             TO LMC-DESC
             MOVE BROR-GL-DEBIT-CARD-PROCEEDS  TO LMC-ORG-TEXT
             MOVE BRCH-GL-DEBIT-CARD-PROCEEDS  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-TEST-PP-AUTO-INSCOVR NOT = BRCH-TEST-PP-AUTO-INSCOVR
             MOVE "10 "                   TO LMC-FIELD
             MOVE "TEST-PP-CPI"           TO LMC-DESC
             MOVE BROR-TEST-PP-AUTO-INSCOVR  TO LMC-ORG-TEXT
             MOVE BRCH-TEST-PP-AUTO-INSCOVR  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-BRANCH-CLOSED NOT = BRCH-BRANCH-CLOSED
             MOVE "11 "                   TO LMC-FIELD
             MOVE "BRANCHCLOSED"          TO LMC-DESC
             MOVE BROR-BRANCH-CLOSED  TO LMC-ORG-TEXT
             MOVE BRCH-BRANCH-CLOSED  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALLOW-F3-ALL-DOCS NOT = BRCH-ALLOW-F3-ALL-DOCS
             MOVE "12 "                   TO LMC-FIELD
             MOVE "F3-ALL DOCS "          TO LMC-DESC
             MOVE BROR-ALLOW-F3-ALL-DOCS  TO LMC-ORG-TEXT
             MOVE BRCH-ALLOW-F3-ALL-DOCS  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

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

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************
       ENTER-SCN.

           MOVE " ENTER BRANCH # OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM OPEN-BR-FILE.

           MOVE 1 TO BR-NO.
           PERFORM START-BR-FILE.

      *-----------------------------------------------------------------
       ENTER-KEY.

           MOVE "EN N040SCAN." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           IF F6-KEY
              GO TO READ-FILES-NEXT.

           IF NOT ENTER-KEY
              GO TO ENTER-KEY.

           IF VDUNUM0 = 0
              MOVE BR-REC TO ORIGINAL-BR-REC
              GO TO CLOSE-SCAN.

           MOVE VDUNUM0 TO BR-NO.
           PERFORM START-BR-FILE.

      *-----------------------------------------------------------------
       READ-FILES-NEXT.

           PERFORM READ-BR-FILE-NEXT.

           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N040BRANCH." TO VDU.
           MOVE BR-NO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-KEY.

      *-----------------------------------------------------------------
       CLOSE-SCAN.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM CLOSE-BR-FILE.

       END-SCAN.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.
           MOVE BR-NAME                    TO BRNAME.
           MOVE BR-STATE                   TO BRSTATE.
           MOVE BR-GLOBAL-QUEUE            TO F01.
           MOVE BR-FORCE-VALID-CITY-COUNTY TO F02.
           MOVE BR-NON-MONTHLY-CAN-LOANS   TO F03.

           MOVE BR-ALT-INTO-THIS-BRANCH TO F04.
           MOVE BR-POFF-TITLE-WINDOW    TO F05.
           MOVE BR-WIMENU-CHECK-WRITING TO F06.
           MOVE BR-ALLOW-SCANNED-CKS    TO F07.
           MOVE BR-USE-DEBIT-CARDS      TO F08.
           MOVE BR-GL-DEBIT-CARD-PROCEEDS TO WORK-GL.
           MOVE WORK-BR TO F09.
           MOVE WORK-NO TO F091.
           MOVE BR-TEST-PP-AUTO-INSCOVR TO F10.
           MOVE BR-BRANCH-CLOSED        TO F11.
           MOVE BR-ALLOW-F3-ALL-DOCS    TO F12.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BROPM2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BROPM2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BROPM2-SCREEN.
           DISPLAY BROPM2-SCREEN UPON BROPMN-WINDOW-HANDLE.
           MOVE BROPM2-FORM-FIELDS TO WR-BUF.
           MOVE BROPM2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BROPM2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BROPM2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BROPM2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-Y-OR-N.
           MOVE "MUST BE 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       SEND-Y-OR-N-OR-O.
           MOVE "MUST BE 'Y' OR 'N' OR 'O'-ONLY" TO MESS.
           PERFORM SEND-MESS.
       BRANCH-NOT-FOUND.
           MOVE "BRANCH NOT FOUND!" TO MESS.
           PERFORM SEND-MESS.
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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                 TO MESS.
           PERFORM SEND-MESS.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************
       END-ROUTINE SECTION.
      *******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BROPM2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BROPMN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
