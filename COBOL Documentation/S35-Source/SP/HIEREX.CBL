       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HIEREX.
       AUTHOR.        KEC.
       DATE-WRITTEN.  02-05-2018.
      ******************************************************************
      *
      * IDENTIFICATION:  SP/HIEREX.C
      *
      * DESCRIPTION   :  HIERARCHY (BRANCH/GROUP) EXTRACTION
      *
      * CUSTOMER NAMES:  WORLD ACCEPTANCE
      *
      * OUTPUT FILE   :  /USR/DATA/R1/FTP/BRANCH_HIE.TXT
      *                   ... .... .  ...            ...  = LOWERCASE
      *
      *                  /USR/DATA/R1/FTP/BRANCH_LOC.TXT
      *                   ... .... .  ...            ...  = LOWERCASE
      *
      * REV:
      * KEC 2018.0205 PR#1302 ***NEW***
      * KEC 2018.0308 PR#1302 PER KEVIN MULLEN, CHANGED THE EXTRACTION OF THE
      *         HIERARCHY FILE TO BE MORE LIKE THE OUTPUT OF LS/GRLST2.
      *         CHANGED TO USE 15 LEVELS FOUND IN LIBGB/GRSERCH
      *         (WGR-SCURR-TABLE) WITH THE CONTACT NAME IN THE FILE.
      * KEC 2018.0309 PR#1302 PER KEVIN MULLEN, ADDED GR-LEVEL TO EXTRACTION
      *         ADDED ANOTHER LEVEL TO EXTRACTION (FROM 8 TO 9).
      * KEC 2018.0313 PR#1302 REVERSING THE CHANGES MADE ON 2018.0308 & 0309.
      *         IT WAS NOT MADE CLEAR EXACTLY WHAT WAS BEING ASKED FOR.
      *         PUT BACK IN THE ORIGINAL 2018.0302 THEN CREATING THE
      *         HIERARCHY EXTRACTION FILE LAYOUT AS:
      *            BRANCH NUMBER
      *            BRANCH MANAGER
      *            GROUP ID (1)
      *            GROUP LEVEL (1)
      *            GROUP CONTACT (1)
      *            ...
      *            GROUP ID (9)
      *            GROUP LEVEL (9)
      *            GROUP CONTACT (9)
      *         IF A GIVEN GROUP LEVEL NOT USED IS STILL PUT IN THE
      *         EXTRACTION WITH SPACES.
      * KEC 2018.0314 PR#1302 "AND TAKE 4".  WITH BRIAN HOFF IN THE OFFICE,
      *          SAT DOWN WITH HIM ON EXACTLY HE IS LOOKING FOR IN THIS
      *         EXTRACTION.  HE SHOWED ME A OVERVIEW OF THE REASON
      *         WHY THEY NEED THIS EXTRACTION WHICH IS USED TO
      *         POPULATE THE ROSTER DATABASE.
      *         SO WE ARE NOW GOING TO USE THE LOGIC FROM LS/GRLST3
      *         WHICH CREATES A BREAKDOWN ONE ONE LINE (ALL GROUPS
      *         LEVELS) FOR GROUPS THEN ONE OR MORE LINES FOR ALL
      *         BRANCHES FOR THAT GROUP.  THE LAYOUT IS:
      *            GROUP ID (1)
      *            GROUP LEVEL (1)
      *            GROUP CONTACT (1)
      *            GROUP ADR1&2  (1)
      *            GROUP CITY    (1)
      *            GROUP STATE   (1)
      *            GROUP ZIP     (1)
      *            GROUP PHONE   (1)
      *            ...
      *            GROUP ***(8)
      *            BRANCH NUMBER
      *            BRANCH MACHINE
      *            BRANCH NAME
      *            GLOBAL COMPANY NAME
      *            BRANCH MANAGER
      *            BRANCH ADR1&2
      *            BRANCH CITY
      *            BRANCH STATE
      *            BRANCH ZIP
      *            BRANCH PHONE
      *            BRANCH FAX
      * BAH 181101 GLOBAL GBFILE
      * BAH 2019.0319 ADD BR-WORLD-BRANCH-TYPE AND BR-ADP-PAY-GROUP TO THE
      *               END OF THE HIEREX_HIE FILE, #1363
      *  BAH 20191004 REMOVED ACCESS-CALL GBFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * BRANCH HIERARCHY EXTRACTION FILE
           SELECT HIEREX-HIE-FILE ASSIGN OUTPUT HIEREX-HIE-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      *    BRANCH LOCATION EXTRACTION FILE
           SELECT HIEREX-LOC-FILE ASSIGN OUTPUT HIEREX-LOC-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      *    BRANCH HIERARCHY EXTRACTION FILE
       FD  HIEREX-HIE-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 1250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  HIEREX-HIE-REC.
           03  HIEREX-HIE-BYTE         PIC X(01)     OCCURS 1250 TIMES.

      *    BRANCH LOCATION EXTRACTION FILE
       FD  HIEREX-LOC-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 190 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  HIEREX-LOC-REC.
           03  HIEREX-LOC-BYTE         PIC X(01)     OCCURS 190 TIMES.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      *    W O R K I N G - S T O R A G E - S E C T I O N
      ******************************************************************
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/HIEREX.CBL S35 02/21/24-14:11:19 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      *    EXAMPLE: /USR/DATA/R1/FTP/BRANCH_HIE.TXT
      *              ... .... .  ...             ...  = LOWERCASE
      *             /USR/DATA/R1/FTP/BRANCH_LOC.TXT
      *              ... .... .  ...            ...   = LOWERCASE
       01  HIEREX-HIE-PATH             PIC X(40)     VALUE SPACES.
       01  HIEREX-LOC-PATH             PIC X(40)     VALUE SPACES.

      * /USR/DATA/R1/FTP/BRANCH_HIER.TXT
       01  HIEREX-PATH                 PIC X(40)     VALUE SPACES.
       01  FILLER                      REDEFINES HIEREX-PATH.
           03  FILLER                  PIC X(01).    
           03  HIEREX-PATH-FIL         PIC X(39).   
       01  FILLER                      REDEFINES HIEREX-PATH.
           03  HIEREX-PATH-SLASH-1     PIC X(01).   
           03  HIEREX-PATH-DIR-1       PIC X(03).    
           03  HIEREX-PATH-SLASH-2     PIC X(01).    
           03  HIEREX-PATH-DIR-2       PIC X(04).    
           03  HIEREX-PATH-SLASH-3     PIC X(01).    
           03  HIEREX-PATH-DIR-3       PIC X(02).    
           03  HIEREX-PATH-SLASH-4     PIC X(01).    
           03  HIEREX-PATH-DIR-4       PIC X(03).    
           03  HIEREX-PATH-SLASH-5     PIC X(01).    
           03  HIEREX-PATH-FILENAME    PIC X(22).    

       01  HIEREX-DIR-FTP              PIC X(03)     VALUE "FTP".
       01  HIEREX-EXT-TXT              PIC X(04)     VALUE ".TXT".

      *    WORLD GROUP LEVELS:
      *      1  WORLD
      *      2  GL DIVISIONS
      *      3  DIVISION
      *      4  STATE
      *      5  STATE OPERATIOS
      *      6  REGION
      *      7  DISTRICT
      *      8  EMPTY/NOT USED
      *      9  BRANCH
       78  EDF-GR---MAX                VALUE 9.
       01  EDF-WORKERS.
           03  EDF-BR-ADDRESS          PIC X(48).
           03  EDF-BR-PHONE            PIC 9(10).
           03  EDF-BR-FAX-NUMBER       PIC 9(10).
           03  EDF-BR-BRANCH-TYPE      PIC X.
           03  EDF-BR-ADP-PAY-GROUP    PIC X(3).
           03  EDF-GR-TABLE            OCCURS EDF-GR---MAX.
               05  EDF-GR-ID           PIC X(4).
               05  EDF-GR-LEVEL        PIC 9.
               05  EDF-GR-CONTACT      PIC X(20).
               05  EDF-GR-ADDRESS      PIC X(40).
               05  EDF-GR-CITY         PIC X(15).
               05  EDF-GR-STATE        PIC X(4).
               05  EDF-GR-ZIP          PIC X(10).
               05  EDF-GR-PHONE        PIC 9(10).

       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  EXITPROG                    PIC X(09)     VALUE "SP/EXTRMU".

       01  LN-FEED                 PIC 9(02)     VALUE 1.
       01  LN-COUNT                PIC 9(03)     VALUE 61.

       01  PAGE-NUMBER                 PIC 9(06)     VALUE ZEROES.

       01  SUB                     PIC 9(03)     VALUE ZEROES.
       01  SUB-GR-PREV             PIC 9(04)     VALUE ZEROES.
       01  SUB-TITLE               PIC 9(02)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING BRANCH  ".
           03  FILLER PIC X(20) VALUE "                    ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G BRANCH                      ".
           03  FILLER PIC X(20) VALUE "                    ".

      *-----------------------------------------------------------------
       01  LEGEND-ENT-GROUP.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BRANCH GROUP ID           ".
           03  FILLER PIC X(20) VALUE "                    ".

      *-----------------------------------------------------------------
       01  LEGEND-REPORT-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR RE".
           03  FILLER PIC X(20) VALUE "PORT                ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO ACCEPT SETTINGS   ".
           03  FILLER PIC X(20) VALUE "                    ".

      ******************************************************************
      *
      *    R A N G E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(4).
           03  REPORT-ISO              PIC X(01).
               88  REPORT-ISO-INCLUDE      VALUE "I".
               88  REPORT-ISO-SKIP         VALUE "S".
               88  REPORT-ISO-ONLY         VALUE "O".

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SV-BEG-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-END-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H01-HR                  PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H01-MIN                 PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(31)     VALUE SPACES.
           03  H01-NAME                PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(37)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZ9.
 
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H02-USERID              PIC X(10)     VALUE SPACES.
           03  H02-TITLE               PIC X(92)     VALUE
                         "HIERARCHY (BRANCH/GROUP) EXTRACTION REPORT".
           03  FILLER                  PIC X(20)     VALUE SPACES.
 
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "-------[ BRANCH ]------- ---[ ".
           03  FILLER PIC X(30) VALUE "1 ]--- ---[ 2 ]--- ---[ 3 ]---".
           03  FILLER PIC X(30) VALUE " ---[ 4 ]--- ---[ 5 ]--- ---[ ".
           03  FILLER PIC X(30) VALUE "6 ]--- ---[ 7 ]--- ---[ 8 ]---".
           03  FILLER PIC X(12) VALUE " ---[ 9 ]---".

       01  HEADER-LINE-04.
           03  FILLER PIC X(30) VALUE "NO.  MANAGER             ID   ".
           03  FILLER PIC X(30) VALUE "CONT   ID   CONT   ID   CONT  ".
           03  FILLER PIC X(30) VALUE " ID   CONT   ID   CONT   ID   ".
           03  FILLER PIC X(30) VALUE "CONT   ID   CONT   ID   CONT  ".
           03  FILLER PIC X(12) VALUE " ID   CONT  ".

       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.
 
       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  D01-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  D01-MANAGER             PIC X(19).
           03  D01-GR-TABLE            OCCURS 09 TIMES.
               05  FILLER              PIC X(01).
               05  D01-GR-ID           PIC X(04).
               05  FILLER              PIC X(01).
               05  D01-GR-CONTACT      PIC X(06).

       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-DESC                PIC X(22).
           03  R01-TEXT                PIC X(110).
           03  R01-BRNO                REDEFINES R01-TEXT
                                       PIC 9(04).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
      *COPY "LIBGB/CALLID.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/HIEREX_DEF.CPY".
       COPY "LIBSP/HIEREX_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/HIEREX_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE HIEREX-HIE-FILE HIEREX-LOC-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE HIEREX-HIE-FILE HIEREX-LOC-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP FORM-PATHNAME.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           MOVE ENT-GROUP                  TO GR-ID.
           PERFORM SEARCH-GR-INIT.

           MOVE 1 TO WGR-TCURR WGR-TDEX1.
           PERFORM PROCESS-GROUP-MODULE.

       MAIN-PROGRAM-NEXT-GR.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SSTABLE TO SUB-TITLE.

       MAIN-PROGRAM-TITLE-LOOP.

           ADD 01 TO SUB-TITLE.

           IF ( SUB-TITLE > WGR-SDEPTH )
              GO TO MAIN-PROGRAM-SKIP-TITLES.

           MOVE WGR-SCURR(SUB-TITLE)       TO WGR-TCURR.
           MOVE SUB-TITLE                  TO WGR-TDEX1.
           PERFORM PROCESS-GROUP-MODULE.

           GO TO MAIN-PROGRAM-TITLE-LOOP.

       MAIN-PROGRAM-SKIP-TITLES.

           MOVE WGR-SCURR(WGR-TPLUS)       TO WGR-TCURR.
           MOVE WGR-TPLUS                  TO WGR-TDEX1.
           PERFORM PROCESS-GROUP-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GR.

       MAIN-PROGRAM-END.

           PERFORM REPORT-RANGES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    P R O C E S S - G R O U P - M O D U L E
      *
      * IN  : EDF FIELDS
      *       EDF-GR-SUB
      *       SUB-GR-PREV
      *       WGR FIELDS
      *
      * OUT : EDF FIELDS
      *       EDF-GR-SUB
      *       SUB-GR-PREV
      *
      * DESC: PROCESS CURRENT WGR (LIBGB/GRWORKW) LEVEL TO LOAD EDF.
      ******************************************************************
       PROCESS-GROUP-MODULE SECTION.

           IF WGR-SID(WGR-TCURR) NOT NUMERIC
              GO TO PROCESS-GROUP-ALPHA.

           MOVE 9                          TO SUB-GR-PREV.
           MOVE WGR-SID(WGR-TCURR)         TO WGR-WORD.
           MOVE WGR-NUMB                   TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO BR-REC
              MOVE WGR-NUMB                TO BR-NO
              MOVE ZEROES                  TO BR-PHONE
                                              BR-FAX-NUMBER
              MOVE "XX"                    TO BR-MACHINE
              MOVE SPACES                  TO GB-COMPANY-NAME
              GO TO PROCESS-GROUP-NUMERIC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

      * LOAD EDF-WORKERS
           MOVE BR-PHONE                   TO EDF-BR-PHONE.
           MOVE BR-FAX-NUMBER              TO EDF-BR-FAX-NUMBER.
           MOVE SPACES                     TO EDF-BR-ADDRESS.

           STRING BR-ADR1, " ", BR-ADR2
                  DELIMITED BY "  "      INTO EDF-BR-ADDRESS.

           MOVE BR-WORLD-BRANCH-TYPE TO EDF-BR-BRANCH-TYPE.
           MOVE BR-ADP-PAY-GROUP     TO EDF-BR-ADP-PAY-GROUP.

           MOVE "B"                        TO GB-PATH-OVERRIDE.

           MOVE BR-NO                      TO GB-BRNO.
           MOVE 01                         TO GB-NO.
           PERFORM OPEN-GB-FILE.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO GB-COMPANY-NAME.

       PROCESS-GROUP-NUMERIC.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO PROCESS-GROUP-MODULE-EXIT.

       PROCESS-GROUP-ALPHA.

           MOVE WGR-SID(WGR-TCURR)         TO GR-ID.
           PERFORM READ-GR1-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO GR-REC
              MOVE WGR-SID(WGR-TCURR)      TO GR-ID
              MOVE 1                       TO GR-LEVEL   
              MOVE "UNKNOWN"               TO GR-CONTACT
                                              GR-ADDR-1
                                              GR-CITY
                                              GR-ZIP
              MOVE ZEROES                  TO GR-PHONE.

      * LOAD EDF-WORKERS 

           PERFORM MISC-RESET-EDF-GR-LEVELS.
           MOVE GR-ID                      TO EDF-GR-ID     (GR-LEVEL).
           MOVE GR-LEVEL                   TO EDF-GR-LEVEL  (GR-LEVEL).
           MOVE GR-CONTACT                 TO EDF-GR-CONTACT(GR-LEVEL).

           STRING GR-ADDR-1, " ", GR-ADDR-2
                  DELIMITED BY "  "      INTO EDF-GR-ADDRESS(GR-LEVEL).

           MOVE GR-CITY                    TO EDF-GR-CITY   (GR-LEVEL).
           MOVE GR-STATE                   TO EDF-GR-STATE  (GR-LEVEL).
           MOVE GR-ZIP                     TO EDF-GR-ZIP    (GR-LEVEL).
           MOVE GR-PHONE                   TO EDF-GR-PHONE  (GR-LEVEL).

       PROCESS-GROUP-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      *    IN : BR-REC
      *         GB-REC
      *
      *    OUT: HIEREX-HIE-FILE
      *         HIEREX-LOC-FILE
      *
      *-----------------------------------------------------------------
      *    FOR CURRENT BRANCH NUMBER (BR-NO), NEED TO CREATE RECORDS FOR
      *    BRANCH LOCATION EXTRACTION FILE (HIEREX-LOC-FILE) AND
      *    BRANCH HIERARCHY EXTRACTION FILE (HIEREX-HIE-FILE).
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           IF ( REPORT-ISO-INCLUDE )
              PERFORM EXTRACT-ROUTINE
              PERFORM REPORT-DETAIL
           ELSE
           IF ( REPORT-ISO-SKIP )
              PERFORM EXTRACT-ROUTINE
           ELSE
              PERFORM REPORT-DETAIL.

       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E X T R A C T - R O U T I N E
      *
      *    CREATING EXTRACTION RECORDS FOR:
      *    - BRANCH LOCATION EXTRACTION FILE (HIEREX-LOC-FILE)
      *    - BRANCH HIERARCHY EXTRACTION FILE (HIEREX-HIE-FILE)
      ******************************************************************
       EXTRACT-ROUTINE SECTION.

           MOVE SPACES                     TO HIEREX-HIE-REC.

      * WORLD
           STRING EDF-GR-ID     (1),          HEX-TAB,
                  EDF-GR-LEVEL  (1),          HEX-TAB,
                  EDF-GR-CONTACT(1),          HEX-TAB,
                  EDF-GR-ADDRESS(1),          HEX-TAB,
                  EDF-GR-CITY   (1),          HEX-TAB,
                  EDF-GR-STATE  (1),          HEX-TAB,
                  EDF-GR-ZIP    (1),          HEX-TAB,
                  EDF-GR-PHONE  (1),          HEX-TAB,
      * GL DIVISION
                  EDF-GR-ID     (2),          HEX-TAB,
                  EDF-GR-LEVEL  (2),          HEX-TAB,
                  EDF-GR-CONTACT(2),          HEX-TAB,
                  EDF-GR-ADDRESS(2),          HEX-TAB,
                  EDF-GR-CITY   (2),          HEX-TAB,
                  EDF-GR-STATE  (2),          HEX-TAB,
                  EDF-GR-ZIP    (2),          HEX-TAB,
                  EDF-GR-PHONE  (2),          HEX-TAB,
      * DIVISION
                  EDF-GR-ID     (3),          HEX-TAB,
                  EDF-GR-LEVEL  (3),          HEX-TAB,
                  EDF-GR-CONTACT(3),          HEX-TAB,
                  EDF-GR-ADDRESS(3),          HEX-TAB,
                  EDF-GR-CITY   (3),          HEX-TAB,
                  EDF-GR-STATE  (3),          HEX-TAB,
                  EDF-GR-ZIP    (3),          HEX-TAB,
                  EDF-GR-PHONE  (3),          HEX-TAB,
      * STATE
                  EDF-GR-ID     (4),          HEX-TAB,
                  EDF-GR-LEVEL  (4),          HEX-TAB,
                  EDF-GR-CONTACT(4),          HEX-TAB,
                  EDF-GR-ADDRESS(4),          HEX-TAB,
                  EDF-GR-CITY   (4),          HEX-TAB,
                  EDF-GR-STATE  (4),          HEX-TAB,
                  EDF-GR-ZIP    (4),          HEX-TAB,
                  EDF-GR-PHONE  (4),          HEX-TAB,
      * STATE OPS
                  EDF-GR-ID     (5),          HEX-TAB, 
                  EDF-GR-LEVEL  (5),          HEX-TAB,
                  EDF-GR-CONTACT(5),          HEX-TAB,
                  EDF-GR-ADDRESS(5),          HEX-TAB,
                  EDF-GR-CITY   (5),          HEX-TAB,
                  EDF-GR-STATE  (5),          HEX-TAB,
                  EDF-GR-ZIP    (5),          HEX-TAB,
                  EDF-GR-PHONE  (5),          HEX-TAB,
      * REGION
                  EDF-GR-ID     (6),          HEX-TAB,
                  EDF-GR-LEVEL  (6),          HEX-TAB,
                  EDF-GR-CONTACT(6),          HEX-TAB,
                  EDF-GR-ADDRESS(6),          HEX-TAB,
                  EDF-GR-CITY   (6),          HEX-TAB,
                  EDF-GR-STATE  (6),          HEX-TAB,
                  EDF-GR-ZIP    (6),          HEX-TAB,
                  EDF-GR-PHONE  (6),          HEX-TAB,
      * DISTRICT
                  EDF-GR-ID     (7),          HEX-TAB,
                  EDF-GR-LEVEL  (7),          HEX-TAB,
                  EDF-GR-CONTACT(7),          HEX-TAB,
                  EDF-GR-ADDRESS(7),          HEX-TAB,
                  EDF-GR-CITY   (7),          HEX-TAB,
                  EDF-GR-STATE  (7),          HEX-TAB,
                  EDF-GR-ZIP    (7),          HEX-TAB,
                  EDF-GR-PHONE  (7),          HEX-TAB,
      * EMPTY, NOT USED
                  EDF-GR-ID     (8),          HEX-TAB,
                  EDF-GR-LEVEL  (8),          HEX-TAB,
                  EDF-GR-CONTACT(8),          HEX-TAB,
                  EDF-GR-ADDRESS(8),          HEX-TAB,
                  EDF-GR-CITY   (8),          HEX-TAB,
                  EDF-GR-STATE  (8),          HEX-TAB,
                  EDF-GR-ZIP    (8),          HEX-TAB,
                  EDF-GR-PHONE  (8),          HEX-TAB,
                  BR-NO,                      HEX-TAB,
                  BR-MACHINE,                 HEX-TAB,
                  BR-NAME,                    HEX-TAB,
                  GB-COMPANY-NAME,            HEX-TAB,
                  BR-MANAGER,                 HEX-TAB,
                  EDF-BR-ADDRESS,             HEX-TAB,
                  BR-CITY,                    HEX-TAB,
                  BR-STATE,                   HEX-TAB,
                  BR-ZIP,                     HEX-TAB,
                  EDF-BR-PHONE,               HEX-TAB,
                  EDF-BR-FAX-NUMBER,          HEX-TAB,
                  EDF-BR-BRANCH-TYPE,         HEX-TAB,
                  EDF-BR-ADP-PAY-GROUP        HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE      INTO HIEREX-HIE-REC.

           PERFORM WRITE-HIEREX-HIE-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

      * CREATE HIEREX-LOC-REC 

           MOVE SPACES                     TO HIEREX-LOC-REC.

           STRING BR-NO,                      HEX-TAB,
                  BR-NAME,                    HEX-TAB,
                  GB-COMPANY-NAME,            HEX-TAB,
                  EDF-BR-ADDRESS,             HEX-TAB,
                  BR-CITY,                    HEX-TAB,
                  BR-STATE,                   HEX-TAB,
                  BR-ZIP,                     HEX-TAB,
                  EDF-BR-PHONE,               HEX-TAB,
                  EDF-BR-FAX-NUMBER,          HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE      INTO HIEREX-LOC-REC.

           PERFORM WRITE-HIEREX-LOC-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       EXTRACT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "HIEREX"                   TO VDU-FORM-NAME.
           MOVE HIEREX-QUEUE-POS           TO Q-POS.
           MOVE HIEREX-COPIES-POS          TO C-POS.
           MOVE HIEREX-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           MOVE EXT-CONAME-USER-ID         TO H02-USERID.

           IF ( EXT-ROUTE-BUF = "70" )
              MOVE EXT-EOBUF-MESS          TO H02-TITLE.

           CALL "C$JUSTIFY"             USING H02-TITLE "C".

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO          TO BEG-BRANCH   END-BRANCH
              MOVE "N"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH   END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

      * SET HIEREX-PATH BY GETTING CURRENT USERS MACHINE VIA FIL
           CALL "C$TOLOWER"             USING HIEREX-DIR-FTP, VALUE 3.
           CALL "C$TOLOWER"             USING HIEREX-EXT-TXT, VALUE 4.

           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-BAD )
              GO TO IO-ERROR.

           MOVE "/"                        TO HIEREX-PATH-SLASH-1.
           MOVE GETENV-BUF                 TO HIEREX-PATH-FIL.
           MOVE HIEREX-DIR-FTP             TO HIEREX-PATH-DIR-4.
           MOVE SPACES                     TO HIEREX-PATH-SLASH-5.
           MOVE SPACES                     TO HIEREX-PATH-FILENAME.
           MOVE HIEREX-PATH                TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE SPACES                  TO MESS
              STRING HIEREX-PATH, " DOES NOT EXIST"
                     DELIMITED BY "  "   INTO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZATION-END.

           MOVE "/"                        TO HIEREX-PATH-SLASH-5.

      * SET HIEREX-HIE-PATH BY USING PRESET HIEREX-PATH

           MOVE SPACES                     TO HIEREX-PATH-FILENAME.

           STRING "BRANCH_HIE", HIEREX-EXT-TXT
                  DELIMITED BY "  "      INTO HIEREX-PATH-FILENAME.

           MOVE HIEREX-PATH                TO HIEREX-HIE-PATH.

           IF ( REPORT-ISO-INCLUDE ) OR
              ( REPORT-ISO-SKIP    )
              PERFORM OPEN-HIEREX-HIE-FILE-OUTPUT.

      * SET HIEREX-LOC-PATH BY USING PRESET HIEREX-PATH

           MOVE SPACES                     TO HIEREX-PATH-FILENAME.

           STRING "BRANCH_LOC", HIEREX-EXT-TXT
                  DELIMITED BY "  "      INTO HIEREX-PATH-FILENAME.
           MOVE HIEREX-PATH                TO HIEREX-LOC-PATH.

           IF ( REPORT-ISO-INCLUDE ) OR
              ( REPORT-ISO-SKIP    )
              PERFORM OPEN-HIEREX-LOC-FILE-OUTPUT.

           MOVE "HIERARCHY EXTRACTION IN PROGRESS....." TO LEGEND.

           IF ( EXT-EOBUF-ERRCD NOT = "X" )
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           IF ( UP-KEY )
              GO TO ENTER-REPORT-ISO.

       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F6-KEY ) PERFORM SCAN-GR-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              GO TO ENTER-BEG-BRANCH.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
       
           PERFORM BR-RANGE-SECURITY.
       
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.
       
       ENTER-REPORT-ISO.

           MOVE LEGEND-REPORT-ISO          TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010REPORT-ISO."       TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-REPORT-ISO.

           IF ( VDUBUF = "I" OR "S" OR "O" )
              MOVE VDUBUF TO REPORT-ISO
           ELSE
              MOVE "ERROR: (I)NCLUDE, (S)KIP OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REPORT-ISO.

       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           MOVE "S  A000BEG-BRANCH."       TO VDU.
           MOVE ENT-GROUP                  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A001REPORT-ISO."       TO VDU.
           MOVE REPORT-ISO                 TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       SCAN-GR-FILE SECTION.
      ******************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE "XX"                    TO VDUSTATUS
              GO TO SCAN-GR-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH."       TO VDU.
           MOVE GRSCAN-KEY                 TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY                 TO VDUBUF.
           MOVE "00"                       TO VDUSTATUS.

       SCAN-GR-FILE-EXIT.
           EXIT.

      ******************************************************************
       REPORT-DETAIL SECTION.
      ******************************************************************

           MOVE BR-NO                      TO D01-BRNO.
           MOVE BR-MANAGER                 TO D01-MANAGER.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9
                MOVE EDF-GR-ID     (SUB)   TO D01-GR-ID     (SUB)
                MOVE EDF-GR-CONTACT(SUB)   TO D01-GR-CONTACT(SUB)
           END-PERFORM.

           PERFORM WRITE-DETAIL-LINE.

       REPORT-DETAIL-EXIT.
           EXIT.

      ******************************************************************
       REPORT-HEADER SECTION.
      ******************************************************************

           ADD 01                          TO PAGE-NUMBER.
           MOVE PAGE-NUMBER                TO H01-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH                       TO H01-HR.
           MOVE T-MM                       TO H01-MIN.
           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-02             TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-03             TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-04             TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "="                    TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 6 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

       REPORT-HEADER-EXIT.
           EXIT.

      ******************************************************************
       REPORT-RANGES SECTION.
      ******************************************************************

           MOVE 99 TO LN-COUNT.

           IF ( GROUP-FLAG = "Y" )
              MOVE "SELECTED GROUP      : " TO R01-DESC
              MOVE ENT-GROUP                TO R01-TEXT
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH    : " TO R01-DESC
              MOVE BEG-BRANCH               TO R01-BRNO
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH    : " TO R01-DESC
              MOVE END-BRANCH               TO R01-BRNO
              PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT (I/S/O)      : "   TO R01-DESC.
           MOVE REPORT-ISO                 TO R01-TEXT.
           MOVE 01                         TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.

       REPORT-RANGES-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED                     TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM REPORT-HEADER.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME          TO HIEREX-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO HIEREX-SYSDATE.
           DISPLAY HIEREX-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE HIEREX-FORM-FIELDS         TO WR-BUF.
           MOVE HIEREX-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE HIEREX-HELP--FILE          TO HELP-LINK-FILE.
           MOVE HIEREX-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "00"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBSP/HIEREX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      * IN  : EDF-GR-TABLE
      *       GR-LEVEL   [SUBSCRIPT]
      * OUT : EDF-GR-TABLE
      * DESC: RESET EDF-GR-TABLE LEVELS FROM GR-LEVEL TO END OF TABLE.
      *       NEEDED TO CLEAR WHEN A GROUP LEVEL CHANGES.
      * NOTE: MOVING SPACES TO EDF-GR-TABLE LEVEL BUT NOT MOVING ZEROES
      *       TO THE NUMERIC FIELDS.  NEED TO HAVE SPACES IN ALL
      *       EXTRACTION GROUP LEVELS IF NOT USED.
       MISC-RESET-EDF-GR-LEVELS.

           PERFORM VARYING SUB FROM GR-LEVEL BY 1
                     UNTIL SUB > EDF-GR---MAX

                   MOVE SPACES TO EDF-GR-TABLE(SUB)

           END-PERFORM.

       SEND-LEGEND.
           MOVE LEGEND-LIST                TO WR-LIST.
           MOVE LEGEND                     TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       OPEN-HIEREX-HIE-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE HIEREX-HIE-PATH TO E-FILE.
           OPEN OUTPUT HIEREX-HIE-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-HIEREX-HIE-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE HIEREX-HIE-FILE
              GO TO OPEN-HIEREX-HIE-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-HIEREX-HIE-FILE.

           PERFORM READ-IT.
           MOVE HIEREX-HIE-PATH TO E-FILE.
           READ HIEREX-HIE-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-HIEREX-HIE-FILE.

      *-----------------------------------------------------------------
       WRITE-HIEREX-HIE-FILE.

           PERFORM WRITE-IT.
           MOVE HIEREX-HIE-PATH TO E-FILE.
           WRITE HIEREX-HIE-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-HIEREX-HIE-FILE.

      *-----------------------------------------------------------------
       CLOSE-HIEREX-HIE-FILE.

           PERFORM CLOSE-IT.
           CLOSE HIEREX-HIE-FILE.

       OPEN-HIEREX-LOC-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE HIEREX-LOC-PATH TO E-FILE.
           OPEN OUTPUT HIEREX-LOC-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-HIEREX-LOC-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE HIEREX-LOC-FILE
              GO TO OPEN-HIEREX-LOC-FILE-OUTPUT.

      *-----------------------------------------------------------------
       READ-HIEREX-LOC-FILE.

           PERFORM READ-IT.
           MOVE HIEREX-LOC-PATH TO E-FILE.
           READ HIEREX-LOC-FILE.

           IF ( IO-FG = 8 )
              GO TO READ-HIEREX-LOC-FILE.

      *-----------------------------------------------------------------
       WRITE-HIEREX-LOC-FILE.

           PERFORM WRITE-IT.
           MOVE HIEREX-LOC-PATH TO E-FILE.
           WRITE HIEREX-LOC-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-HIEREX-LOC-FILE.

      *-----------------------------------------------------------------
       CLOSE-HIEREX-LOC-FILE.

           PERFORM CLOSE-IT.
           CLOSE HIEREX-LOC-FILE.

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXITPROG                TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY HIEREX-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
